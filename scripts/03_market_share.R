################################################################################
# 03_market_share.R
# Calculates biosimilar market share and builds ITS-ready panel dataset
#
# Inputs:
#   - data/raw/pbs_prescribing/dos_data_raw.rds
#   - data/raw/pbs_prescribing/medicare_stats_raw.rds (optional)
#   - data/reference/pbs_item_mapping.csv
#   - data/reference/policy_timeline.csv
#   - data/reference/atc_molecule_lookup.csv
#
# Outputs:
#   - data/processed/market_share_national.csv
#   - data/processed/market_share_state.csv
#   - data/processed/market_share_panel.rds
################################################################################

library(tidyverse)
library(lubridate)
library(here)
library(glue)
library(cli)

# ─── Load reference data ─────────────────────────────────────────────────────

cli_h1("Loading reference data")

item_mapping <- read_csv(here("data", "reference", "pbs_item_mapping.csv"),
                         show_col_types = FALSE)

policy_timeline <- read_csv(here("data", "reference", "policy_timeline.csv"),
                            show_col_types = FALSE) %>%
  mutate(date = ymd(date))

atc_lookup <- read_csv(here("data", "reference", "atc_molecule_lookup.csv"),
                       show_col_types = FALSE)

# ─── Classification summary ──────────────────────────────────────────────────

cli_h1("Item classification summary")

# item_mapping should already have type = biosimilar/reference from 01_item_mapping.R
classified <- item_mapping %>% filter(type %in% c("biosimilar", "reference"))
unclassified <- item_mapping %>% filter(!type %in% c("biosimilar", "reference"))

cli_alert_info("Classified: {nrow(classified)} items ({sum(classified$type == 'biosimilar')} biosimilar, {sum(classified$type == 'reference')} reference)")

if (nrow(unclassified) > 0) {
  cli_alert_warning("Unclassified: {nrow(unclassified)} items (will be excluded)")
  unclassified %>%
    count(molecule, name = "n_unclassified") %>%
    print(n = Inf)
}

# ─── Load prescribing data ───────────────────────────────────────────────────

cli_h1("Loading prescribing data")

dos_path <- here("data", "raw", "pbs_prescribing", "dos_data_raw.rds")
medicare_path <- here("data", "raw", "pbs_prescribing", "medicare_stats_raw.rds")

has_dos <- file.exists(dos_path)
has_medicare <- file.exists(medicare_path)

if (!has_dos && !has_medicare) {
  cli_alert_danger("No prescribing data found. Run 02_data_acquisition.R first.")
  stop("No prescribing data available", call. = FALSE)
}

prescribing_data <- bind_rows(
  if (has_dos) {
    dos <- readRDS(dos_path)
    cli_alert_success("Loaded DoS data: {format(nrow(dos), big.mark = ',')} records")
    dos
  },
  if (has_medicare) {
    med <- readRDS(medicare_path)
    cli_alert_success("Loaded Medicare Statistics: {format(nrow(med), big.mark = ',')} records")
    med
  }
)

# ─── 1. Harmonise and classify ──────────────────────────────────────────────

cli_h1("Step 1: Harmonise time series and classify items")

# Re-join with item mapping to get type classification
prescribing_data <- prescribing_data %>%
  select(-any_of(c("molecule", "brand_from_desc", "brand_name", "type"))) %>%
  left_join(
    item_mapping %>% select(item_number, molecule, brand_name, type),
    by = "item_number"
  ) %>%
  filter(!is.na(molecule))

# Floor all periods to month start
prescribing_data <- prescribing_data %>%
  mutate(period = floor_date(period, "month"))

# Report classification status
type_summary <- prescribing_data %>%
  group_by(type) %>%
  summarise(
    n_records = n(),
    total_rx = sum(prescriptions, na.rm = TRUE),
    .groups = "drop"
  )
cli_alert_info("Records by type:")
print(type_summary)

# Filter to only classified records for market share calculation
classified_data <- prescribing_data %>%
  filter(type %in% c("biosimilar", "reference"))

unclassified_rx <- prescribing_data %>%
  filter(!type %in% c("biosimilar", "reference")) %>%
  summarise(n = n(), rx = sum(prescriptions, na.rm = TRUE))

if (unclassified_rx$n > 0) {
  cli_alert_warning("Excluding {format(unclassified_rx$n, big.mark = ',')} unclassified records ({format(unclassified_rx$rx, big.mark = ',')} prescriptions)")
  cli_alert_info("These are items where we cannot determine biosimilar vs reference status")
}

# Where we have overlapping DoS and Medicare data, prefer DoS
if (has_dos && has_medicare && "source" %in% names(classified_data)) {
  cli_alert_info("Resolving overlapping DoS and Medicare data...")

  overlap <- classified_data %>%
    group_by(item_number, period, state) %>%
    filter(n_distinct(source) > 1) %>%
    ungroup()

  if (nrow(overlap) > 0) {
    cli_alert_info("  Found {n_distinct(overlap$period)} overlapping periods — preferring DoS data")

    classified_data <- classified_data %>%
      group_by(item_number, period, state) %>%
      filter(n() == 1 | source == "dos") %>%
      ungroup()
  }
}

# ── Fix item reassignment: before a molecule's first biosimilar listing,
#    ALL prescriptions must be reference (no biosimilar existed yet).
#    PBS item numbers get reassigned between brands over time, so current
#    Schedule brand names don't reflect historical dispensing.

first_bio_dates <- policy_timeline %>%
  filter(intervention_type == "biosimilar_listing") %>%
  group_by(molecule) %>%
  summarise(first_bio_date = min(date), .groups = "drop")

classified_data <- classified_data %>%
  left_join(first_bio_dates, by = "molecule") %>%
  mutate(
    was_reclassified = (period < first_bio_date & type == "biosimilar"),
    type = if_else(was_reclassified, "reference", type)
  )

n_reclassified <- sum(classified_data$was_reclassified, na.rm = TRUE)
if (n_reclassified > 0) {
  cli_alert_info("Reclassified {format(n_reclassified, big.mark = ',')} pre-listing records from biosimilar to reference")
  cli_alert_info("  (Item numbers reassigned between brands over time)")
}

classified_data <- classified_data %>%
  select(-first_bio_date, -was_reclassified)

cli_alert_info("Total classified records: {format(nrow(classified_data), big.mark = ',')}")
cli_alert_info("Date range: {min(classified_data$period)} to {max(classified_data$period)}")

# ─── 2. Calculate national market share ──────────────────────────────────────

cli_h1("Step 2: National market share")

# Aggregate prescriptions by molecule x type x period (national)
#
# MUST filter to state == "National" before aggregating. The two sources are
# shaped differently: DoS carries only National rows, whereas Medicare
# Statistics carries a National row AND eight jurisdiction rows that sum to the
# same total. Aggregating without this filter counted every Medicare-era month
# (Jan 2009 - Jun 2022) exactly twice, while DoS-era months (Jul 2022 onward)
# were counted once — producing an apparent halving of volumes at Jul 2022 that
# is an artefact, not a market change. Verified on adalimumab, Jun 2022:
# National = 27,091; the eight jurisdictions also sum to 27,091; the unfiltered
# aggregation returned 54,182. The state-level block below already filters
# correctly (state != "National"); this block did not.
national_rows <- classified_data %>% filter(state == "National")

if (nrow(national_rows) == 0) {
  cli_alert_danger("No rows with state == 'National' — cannot build the national series.")
  stop("Missing National-level rows", call. = FALSE)
}

dropped <- nrow(classified_data) - nrow(national_rows)
cli_alert_info("National aggregation: kept {format(nrow(national_rows), big.mark = ',')} National rows, excluded {format(dropped, big.mark = ',')} jurisdiction rows")

national_agg <- national_rows %>%
  group_by(molecule, type, period) %>%
  summarise(
    prescriptions = sum(prescriptions, na.rm = TRUE),
    .groups = "drop"
  )

# Pivot to get biosimilar and reference in same row
market_share_national <- national_agg %>%
  pivot_wider(
    names_from = type,
    values_from = prescriptions,
    values_fill = 0
  ) %>%
  # Ensure both columns exist even if one type has no data
  mutate(
    biosimilar = if ("biosimilar" %in% names(.)) biosimilar else 0L,
    reference = if ("reference" %in% names(.)) reference else 0L
  ) %>%
  mutate(
    total_rx = biosimilar + reference,
    biosimilar_share = if_else(total_rx > 0, biosimilar / total_rx, 0)
  ) %>%
  rename(
    biosimilar_rx = biosimilar,
    reference_rx = reference
  ) %>%
  arrange(molecule, period)

# Add molecule metadata
# Use distinct molecule-level metadata (avoid duplicates from multiple ATC codes)
market_share_national <- market_share_national %>%
  left_join(atc_lookup %>% select(molecule, molecule_class, therapeutic_area) %>% distinct(molecule, .keep_all = TRUE),
            by = "molecule")

write_csv(market_share_national, here("data", "processed", "market_share_national.csv"))
cli_alert_success("Wrote market_share_national.csv ({format(nrow(market_share_national), big.mark = ',')} rows)")

# Print summary
cli_h2("National market share summary (latest period)")
latest <- market_share_national %>%
  group_by(molecule) %>%
  filter(period == max(period)) %>%
  ungroup() %>%
  select(molecule, period, biosimilar_rx, reference_rx, total_rx, biosimilar_share) %>%
  arrange(desc(biosimilar_share))

print(latest)

# ─── 3. Calculate state-level market share ───────────────────────────────────

cli_h1("Step 3: State-level market share")

# Check if state-level data exists
has_state_data <- "state" %in% names(classified_data) &&
  any(classified_data$state != "National", na.rm = TRUE)

if (has_state_data) {
  state_agg <- classified_data %>%
    filter(state != "National") %>%
    group_by(molecule, type, period, state) %>%
    summarise(
      prescriptions = sum(prescriptions, na.rm = TRUE),
      .groups = "drop"
    )

  market_share_state <- state_agg %>%
    pivot_wider(
      names_from = type,
      values_from = prescriptions,
      values_fill = 0
    ) %>%
    mutate(
      biosimilar = if ("biosimilar" %in% names(.)) biosimilar else 0L,
      reference = if ("reference" %in% names(.)) reference else 0L
    ) %>%
    mutate(
      total_rx = biosimilar + reference,
      biosimilar_share = if_else(total_rx > 0, biosimilar / total_rx, 0)
    ) %>%
    rename(
      biosimilar_rx = biosimilar,
      reference_rx = reference
    ) %>%
    arrange(molecule, state, period)

  # Standardise state names
  state_map <- c(
    "NSW" = "NSW", "VIC" = "VIC", "QLD" = "QLD", "WA" = "WA",
    "SA" = "SA", "TAS" = "TAS", "ACT" = "ACT", "NT" = "NT",
    "NEW SOUTH WALES" = "NSW", "VICTORIA" = "VIC", "QUEENSLAND" = "QLD",
    "WESTERN AUSTRALIA" = "WA", "SOUTH AUSTRALIA" = "SA",
    "TASMANIA" = "TAS", "AUSTRALIAN CAPITAL TERRITORY" = "ACT",
    "NORTHERN TERRITORY" = "NT"
  )

  market_share_state <- market_share_state %>%
    mutate(state = if_else(state %in% names(state_map),
                           state_map[state], state))

  write_csv(market_share_state, here("data", "processed", "market_share_state.csv"))
  cli_alert_success("Wrote market_share_state.csv ({format(nrow(market_share_state), big.mark = ',')} rows)")
  cli_alert_info("States: {paste(sort(unique(market_share_state$state)), collapse = ', ')}")
} else {
  cli_alert_warning("No state-level data available — skipping state market share")
  market_share_state <- NULL
}

# ─── 4. Build ITS-ready panel ────────────────────────────────────────────────

cli_h1("Step 4: Build ITS-ready panel dataset")

# For each molecule, create a complete monthly time series with
# intervention indicators from the policy timeline

build_its_panel <- function(mol, ms_data, timeline) {

  mol_data <- ms_data %>% filter(molecule == mol)
  if (nrow(mol_data) == 0) return(NULL)

  # Create complete monthly sequence
  all_months <- tibble(
    period = seq.Date(min(mol_data$period), max(mol_data$period), by = "month")
  )

  # Join with actual data (fill gaps with 0)
  panel <- all_months %>%
    left_join(mol_data, by = "period") %>%
    mutate(
      molecule = mol,
      biosimilar_rx = replace_na(biosimilar_rx, 0),
      reference_rx = replace_na(reference_rx, 0),
      total_rx = replace_na(total_rx, 0),
      biosimilar_share = if_else(total_rx > 0, biosimilar_rx / total_rx, 0)
    ) %>%
    select(molecule, period, biosimilar_rx, reference_rx, total_rx, biosimilar_share)

  # Sequential time index
  panel <- panel %>%
    mutate(time = row_number())

  # Get molecule-specific interventions
  mol_interventions <- timeline %>%
    filter(molecule == mol | molecule == "all") %>%
    arrange(date)

  # Pre-compute unique column names: add _1, _2, etc. for duplicate types
  mol_interventions <- mol_interventions %>%
    group_by(intervention_type) %>%
    mutate(
      type_n = n(),
      type_seq = row_number(),
      col_name = if_else(type_n > 1,
                         paste0(intervention_type, "_", type_seq),
                         intervention_type)
    ) %>%
    ungroup()

  # Create intervention indicators
  for (i in seq_len(nrow(mol_interventions))) {
    int_date <- mol_interventions$date[i]
    col_name <- mol_interventions$col_name[i]

    # Post-intervention indicator (0/1)
    panel[[paste0("post_", col_name)]] <- as.integer(panel$period >= int_date)

    # Time since intervention (0 before, sequential after)
    panel[[paste0("time_after_", col_name)]] <- pmax(0,
      as.numeric(difftime(panel$period, int_date, units = "days")) / 30.44)
    panel[[paste0("time_after_", col_name)]] <-
      round(panel[[paste0("time_after_", col_name)]])
  }

  panel
}

# Build panel for each molecule
its_panels <- map(
  unique(market_share_national$molecule),
  ~ build_its_panel(.x, market_share_national, policy_timeline)
) %>%
  compact()

its_panel <- bind_rows(its_panels)

cli_alert_info("ITS panel: {format(nrow(its_panel), big.mark = ',')} rows, {ncol(its_panel)} columns")
cli_alert_info("Molecules: {n_distinct(its_panel$molecule)}")

# Save panel
saveRDS(its_panel, here("data", "processed", "market_share_panel.rds"))
cli_alert_success("Saved market_share_panel.rds")

write_csv(its_panel, here("data", "processed", "market_share_panel.csv"))
cli_alert_success("Saved market_share_panel.csv")

# ─── 5. Validation ───────────────────────────────────────────────────────────

cli_h1("Step 5: Validation")

# Check 1: biosimilar_share should be 0 before listing date
cli_h2("Validation 1: Pre-listing biosimilar share should be 0")

first_listings <- policy_timeline %>%
  filter(intervention_type == "biosimilar_listing") %>%
  group_by(molecule) %>%
  summarise(first_listing = min(date), .groups = "drop")

pre_listing_check <- its_panel %>%
  left_join(first_listings, by = "molecule") %>%
  filter(period < first_listing) %>%
  group_by(molecule) %>%
  summarise(
    n_pre_periods = n(),
    max_biosimilar_share = max(biosimilar_share, na.rm = TRUE),
    any_nonzero = any(biosimilar_share > 0),
    .groups = "drop"
  )

if (any(pre_listing_check$any_nonzero)) {
  cli_alert_warning("Some molecules have non-zero biosimilar share before listing:")
  print(pre_listing_check %>% filter(any_nonzero))
} else if (nrow(pre_listing_check) > 0) {
  cli_alert_success("All pre-listing periods have 0% biosimilar share")
} else {
  cli_alert_info("No pre-listing data available for validation")
}

# Check 2: biosimilar_share should be >0 within 3 months of listing
cli_h2("Validation 2: Post-listing uptake within 3 months")

post_listing_check <- its_panel %>%
  left_join(first_listings, by = "molecule") %>%
  filter(
    period >= first_listing,
    period <= first_listing + months(3)
  ) %>%
  group_by(molecule) %>%
  summarise(
    n_post_periods = n(),
    max_share_3mo = max(biosimilar_share, na.rm = TRUE),
    uptake_detected = any(biosimilar_share > 0),
    .groups = "drop"
  )

if (nrow(post_listing_check) > 0) {
  print(post_listing_check)
  if (all(post_listing_check$uptake_detected)) {
    cli_alert_success("All molecules show uptake within 3 months of listing")
  } else {
    cli_alert_warning("Some molecules show no uptake in first 3 months (may be data gap or classification issue)")
  }
}

# Check 3: Time series completeness
cli_h2("Validation 3: Time series completeness")

completeness <- its_panel %>%
  group_by(molecule) %>%
  summarise(
    first_period = min(period),
    last_period = max(period),
    n_months = n(),
    expected_months = as.numeric(difftime(max(period), min(period), units = "days")) / 30.44 + 1,
    pct_complete = round(n_months / expected_months * 100, 1),
    .groups = "drop"
  )

print(completeness)

# Intervention column summary
cli_h2("Intervention indicators")
int_cols <- names(its_panel)[str_detect(names(its_panel), "^post_|^time_after_")]
cli_alert_info("Intervention columns: {length(int_cols)}")
for (col in int_cols[str_detect(int_cols, "^post_")]) {
  cli_alert_info("  {col}: {sum(its_panel[[col]] == 1, na.rm = TRUE)} post-intervention obs across all molecules")
}

cli_alert_success("Market share calculation complete")
cli_alert_info("Next: Run 04_descriptive_analysis.R for adoption curves and velocity metrics")
