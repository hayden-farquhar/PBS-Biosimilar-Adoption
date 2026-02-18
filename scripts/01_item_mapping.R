################################################################################
# 01_item_mapping.R
# Maps PBS item numbers to biosimilar/reference biologic products
# Writes three reference files:
#   - data/reference/atc_molecule_lookup.csv
#   - data/reference/policy_timeline.csv
#   - data/reference/pbs_item_mapping.csv
#
# PRIMARY SOURCE: pbs-item-drug-map.csv from PBS DoS download page
################################################################################

library(tidyverse)
library(lubridate)
library(here)
library(glue)
library(cli)

# ─── 1. ATC molecule lookup ─────────────────────────────────────────────────

cli_h1("Step 1: ATC Molecule Lookup")

# Some molecules have had ATC code reclassifications over time.
# We include all historical and current codes.

atc_molecule_lookup <- tribble(
  ~atc_code,   ~molecule,          ~molecule_class,     ~therapeutic_area,              ~dispensing_setting,
  "L04AB04",   "adalimumab",       "anti-TNF",          "rheumatology/gastro/derm",     "community",
  "L04AB02",   "infliximab",       "anti-TNF",          "rheumatology/gastro",          "hospital",
  "L04AA12",   "infliximab",       "anti-TNF",          "rheumatology/gastro",          "hospital",
  "L04AB01",   "etanercept",       "anti-TNF",          "rheumatology",                 "community",
  "L01FA01",   "rituximab",        "anti-CD20",         "oncology/rheumatology",        "hospital",
  "L01XC02",   "rituximab",        "anti-CD20",         "oncology/rheumatology",        "hospital",
  "L01FD01",   "trastuzumab",      "anti-HER2",         "oncology",                     "hospital",
  "L01XC03",   "trastuzumab",      "anti-HER2",         "oncology",                     "hospital",
  "L03AA02",   "filgrastim",       "G-CSF",             "oncology supportive",          "community",
  "L03AA13",   "pegfilgrastim",    "pegylated G-CSF",   "oncology supportive",          "community",
  "L01FG01",   "bevacizumab",      "anti-VEGF",         "oncology",                     "hospital",
  "L01XC07",   "bevacizumab",      "anti-VEGF",         "oncology",                     "hospital",
  "A10AE04",   "insulin glargine", "long-acting insulin","diabetes",                     "community",
  "S01LA05",   "aflibercept",      "anti-VEGF",         "ophthalmology",                "hospital"
)

write_csv(atc_molecule_lookup, here("data", "reference", "atc_molecule_lookup.csv"))
cli_alert_success("Wrote atc_molecule_lookup.csv ({nrow(atc_molecule_lookup)} rows, {n_distinct(atc_molecule_lookup$molecule)} molecules)")

# ─── 2. Policy timeline ─────────────────────────────────────────────────────

cli_h1("Step 2: Policy Timeline")

policy_timeline <- tribble(
  ~molecule,          ~intervention_type,      ~date,          ~description,                                              ~source,

  # ── Cross-cutting interventions ──
  "all",              "aip_regulation",        "2019-10-31",   "Active Ingredient Prescribing mandatory for biosimilar groups", "TGA/PBS legislative instrument",
  "all",              "streamlined_authority",  "2023-11-01",   "Streamlined authority for anti-TNF biosimilars (adalimumab, etanercept, infliximab)", "PBS Schedule Nov 2023",

  # ── Filgrastim ──
  "filgrastim",       "biosimilar_listing",    "2011-04-01",   "Nivestim (Hospira) listed — first Australian biosimilar",  "PBS Schedule Apr 2011",
  "filgrastim",       "a_flag",                "2011-04-01",   "'a' flag enabled for filgrastim biosimilars",              "PBS Schedule Apr 2011",
  "filgrastim",       "additional_biosimilar",  "2013-09-01",   "Zarzio (Sandoz) listed",                                  "PBS Schedule Sep 2013",
  "filgrastim",       "additional_biosimilar",  "2014-04-01",   "Tevagrastim listed",                                      "PBS Schedule Apr 2014",
  "filgrastim",       "additional_biosimilar",  "2019-12-01",   "Releuko (Sandoz) listed",                                 "PBS Schedule Dec 2019",
  "filgrastim",       "reference_delisting",   "2023-12-01",   "Neupogen (Amgen) delisted from PBS at sponsor request",    "PBS Schedule Dec 2023",

  # ── Infliximab ──
  "infliximab",       "biosimilar_listing",    "2015-12-01",   "Inflectra (Pfizer/Celltrion) listed",                     "PBS Schedule Dec 2015",
  "infliximab",       "a_flag",                "2015-12-01",   "'a' flag enabled for infliximab biosimilars",              "PBS Schedule Dec 2015",
  "infliximab",       "additional_biosimilar",  "2017-08-01",   "Renflexis (Samsung Bioepis) listed",                      "PBS Schedule Aug 2017",
  "infliximab",       "additional_biosimilar",  "2021-07-01",   "Remsima SC listed — subcutaneous infliximab biosimilar",   "PBS Schedule Jul 2021",
  "infliximab",       "additional_biosimilar",  "2025-11-01",   "Ixifi listed",                                            "PBS Schedule Nov 2025",
  "infliximab",       "price_disclosure",      "2018-04-01",   "Price disclosure cycle — infliximab price reduction",      "PBS price disclosure",
  "infliximab",       "streamlined_authority",  "2023-11-01",   "Streamlined authority for RA indications",                 "PBS Schedule Nov 2023",
  "infliximab",       "streamlined_authority",  "2023-12-01",   "Streamlined authority extended to AS indications",          "PBS Schedule Dec 2023",

  # ── Etanercept ──
  "etanercept",       "biosimilar_listing",    "2017-04-01",   "Brenzys (Samsung Bioepis) listed",                        "PBS Schedule Apr 2017",
  "etanercept",       "a_flag",                "2017-04-01",   "'a' flag enabled for etanercept biosimilars",              "PBS Schedule Apr 2017",
  "etanercept",       "streamlined_authority",  "2023-11-01",   "Streamlined authority for RA/psoriasis indications",       "PBS Schedule Nov 2023",
  "etanercept",       "streamlined_authority",  "2023-12-01",   "Streamlined authority extended to AS indications",          "PBS Schedule Dec 2023",
  "etanercept",       "additional_biosimilar",  "2025-07-01",   "Nepexto (Sandoz) listed — second etanercept biosimilar",   "PBS Schedule Jul 2025",
  "etanercept",       "additional_biosimilar",  "2025-10-01",   "Erelzi listed — third etanercept biosimilar",              "PBS Schedule Oct 2025",

  # ── Rituximab ──
  "rituximab",        "biosimilar_listing",    "2019-10-01",   "Riximyo (Sandoz) listed",                                 "PBS Schedule Oct 2019",
  "rituximab",        "a_flag",                "2020-01-01",   "'a' flag enabled for rituximab biosimilars",               "PBS Schedule Jan 2020",
  "rituximab",        "additional_biosimilar",  "2020-01-01",   "Truxima (Celltrion) listed",                              "PBS Schedule Jan 2020",
  "rituximab",        "reference_delisting",   "2021-04-01",   "MabThera IV (reference rituximab) delisted from PBS",      "PBS Schedule Apr 2021",
  "rituximab",        "reference_delisting",   "2021-10-01",   "MabThera SC delisted from PBS",                            "PBS Schedule Oct 2021",
  "rituximab",        "additional_biosimilar",  "2022-09-01",   "Ruxience listed",                                         "PBS Schedule Sep 2022",
  "rituximab",        "streamlined_authority",  "2022-09-01",   "Rituximab changed to Unrestricted Benefit (no authority)", "PBAC Sep 2021 recommendation",

  # ── Trastuzumab ──
  "trastuzumab",      "biosimilar_listing",    "2019-08-01",   "Ogivri (Mylan) listed",                                   "PBS Schedule Aug 2019",
  "trastuzumab",      "a_flag",                "2019-08-01",   "'a' flag enabled for trastuzumab biosimilars",             "PBS Schedule Aug 2019",
  "trastuzumab",      "additional_biosimilar",  "2020-04-01",   "Herzuma (Celltrion) listed",                              "PBS Schedule Apr 2020",
  "trastuzumab",      "additional_biosimilar",  "2020-05-01",   "Trazimera listed",                                        "PBS Schedule May 2020",
  "trastuzumab",      "additional_biosimilar",  "2020-10-01",   "Kanjinti (Amgen) listed",                                 "PBS Schedule Oct 2020",

  # ── Insulin glargine ──
  "insulin glargine", "biosimilar_listing",    "2019-10-01",   "Semglee (Mylan/Viatris) listed",                          "PBS Schedule Oct 2019",
  "insulin glargine", "a_flag",                "2019-10-01",   "'a' flag enabled for insulin glargine biosimilars",        "PBS Schedule Oct 2019",
  "insulin glargine", "reference_delisting",   "2020-07-01",   "Lantus delisted; Sanofi consolidated to Optisulin brand",  "The Limbic / PBS Schedule",

  # ── Pegfilgrastim ──
  "pegfilgrastim",    "biosimilar_listing",    "2020-03-01",   "Ziextenzo (Sandoz) listed",                               "PBS Schedule Mar 2020",
  "pegfilgrastim",    "a_flag",                "2020-03-01",   "'a' flag enabled for pegfilgrastim biosimilars",           "PBS Schedule Mar 2020",
  "pegfilgrastim",    "additional_biosimilar",  "2020-08-01",   "Pelgraz listed — second pegfilgrastim biosimilar",         "PBS Schedule Aug 2020",

  # ── Adalimumab ──
  "adalimumab",       "biosimilar_listing",    "2021-04-01",   "Amgevita (Amgen) and Hadlima (Organon) listed",           "PBS Schedule Apr 2021",
  "adalimumab",       "a_flag",                "2021-04-01",   "'a' flag enabled for adalimumab biosimilars",              "PBS Schedule Apr 2021",
  "adalimumab",       "additional_biosimilar",  "2021-04-01",   "Hyrimoz (Sandoz), Idacio (Fresenius Kabi) listed",        "PBS Schedule Apr 2021",
  "adalimumab",       "additional_biosimilar",  "2023-03-01",   "Yuflyma (Celltrion) listed — fifth adalimumab biosimilar","PBS Schedule Mar 2023",
  "adalimumab",       "price_cut",             "2023-04-01",   "24.39% catch-up statutory price reduction for Humira; 6.91% for biosimilars", "Pearce IP / PBS pricing",
  "adalimumab",       "additional_biosimilar",  "2024-01-01",   "Adalicip (Cipla/Alvotech) listed — sixth adalimumab biosimilar", "PBS Schedule Jan 2024",
  "adalimumab",       "additional_biosimilar",  "2024-08-01",   "Abrilada (Pfizer) listed — seventh adalimumab biosimilar",       "PBS Schedule Aug 2024",
  "adalimumab",       "streamlined_authority",  "2023-11-01",   "Streamlined authority for RA/psoriasis indications",       "PBS Schedule Nov 2023",
  "adalimumab",       "streamlined_authority",  "2023-12-01",   "Streamlined authority extended to AS indications",          "PBS Schedule Dec 2023",

  # ── Bevacizumab ──
  "bevacizumab",      "biosimilar_listing",    "2021-06-01",   "Mvasi (Amgen) listed",                                    "PBS Schedule Jun 2021",
  "bevacizumab",      "a_flag",                "2021-06-01",   "'a' flag enabled for bevacizumab biosimilars",             "PBS Schedule Jun 2021",
  "bevacizumab",      "reference_delisting",   "2021-06-01",   "Avastin (Roche) voluntarily delisted from PBS",            "PBS news Jul 2021",
  "bevacizumab",      "additional_biosimilar",  "2022-12-01",   "Abevmy listed — second bevacizumab biosimilar",            "PBS Schedule Dec 2022",
  "bevacizumab",      "additional_biosimilar",  "2024-10-01",   "Vegzelma listed — third bevacizumab biosimilar",            "PBS Schedule Oct 2024",

  # ── Aflibercept ──
  "aflibercept",      "biosimilar_listing",    "2026-02-01",   "Afqlir (Samsung Bioepis) listed — first aflibercept biosimilar", "PBS Schedule Feb 2026",
  "aflibercept",      "a_flag",                "2026-02-01",   "'a' flag enabled for aflibercept biosimilars",             "PBS Schedule Feb 2026"
)

policy_timeline <- policy_timeline %>%
  mutate(date = ymd(date))

write_csv(policy_timeline, here("data", "reference", "policy_timeline.csv"))
cli_alert_success("Wrote policy_timeline.csv ({nrow(policy_timeline)} intervention records)")

# ─── 3. PBS item mapping from drug map ───────────────────────────────────────

cli_h1("Step 3: PBS Item Number Mapping (from drug map)")

drug_map_path <- here("data", "raw", "pbs_prescribing", "pbs-item-drug-map.csv")

if (!file.exists(drug_map_path)) {
  cli_alert_danger("Drug map not found at: {drug_map_path}")
  cli_alert_info("Download pbs-item-drug-map.csv from:")
  cli_alert_info("  https://www.pbs.gov.au/info/statistics/dos-and-dop/dos-and-dop")
  stop("pbs-item-drug-map.csv required", call. = FALSE)
}

drug_map <- read_csv(drug_map_path, show_col_types = FALSE)
cli_alert_info("Read drug map: {nrow(drug_map)} total items")

# All ATC codes for our molecules (including historical reclassifications)
our_atc_codes <- atc_molecule_lookup$atc_code

# Filter to our molecules
pbs_items <- drug_map %>%
  filter(ATC5_Code %in% our_atc_codes) %>%
  rename(
    item_number = ITEM_CODE,
    drug_name = DRUG_NAME,
    form_strength = `FORM/STRENGTH`,
    atc_code = ATC5_Code
  ) %>%
  mutate(item_number = str_remove_all(item_number, '"'))

# Map ATC codes to molecule names (normalise case)
atc_to_molecule <- atc_molecule_lookup %>%
  select(atc_code, molecule) %>%
  distinct()

pbs_items <- pbs_items %>%
  left_join(atc_to_molecule, by = "atc_code")

cli_alert_info("Filtered to {nrow(pbs_items)} items across {n_distinct(pbs_items$molecule)} molecules")

# ── Extract brand from description where possible ──
# Some items have brand in parentheses: e.g., "(TevaGrastim)", "(Humira)"
# Others have brand-specific formulations

# Known biosimilar brand patterns (case-insensitive)
biosimilar_brands <- c(
  # Filgrastim
  "nivestim", "zarzio", "tevagrastim", "releuko",
  # Infliximab
  "inflectra", "renflexis", "remsima", "ixifi",
  # Etanercept
  "brenzys", "erelzi", "nepexto",
  # Rituximab
  "riximyo", "truxima", "ruxience",
  # Trastuzumab
  "ogivri", "herzuma", "kanjinti", "ontruzant", "trazimera",
  # Pegfilgrastim
  "ziextenzo", "pelgraz", "fulphila",
  # Bevacizumab
  "mvasi", "abevmy", "vegzelma",
  # Insulin glargine
  "semglee",
  # Adalimumab
  "amgevita", "hadlima", "hyrimoz", "idacio", "yuflyma", "adalicip", "abrilada",
  # Aflibercept
  "afqlir"
)

# Known reference brand patterns
reference_brands <- c(
  "neupogen", "remicade", "enbrel", "mabthera", "herceptin",
  "neulasta", "avastin", "lantus", "optisulin", "toujeo",
  "humira", "eylea"
)

# Try to extract brand from form_strength field
pbs_items <- pbs_items %>%
  mutate(
    # Extract text in parentheses
    brand_from_desc = str_extract(form_strength, "\\(([^)]+)\\)") %>%
      str_remove_all("[()]"),
    # Classify type
    type = case_when(
      str_detect(tolower(form_strength), paste(biosimilar_brands, collapse = "|")) ~ "biosimilar",
      str_detect(tolower(form_strength), paste(reference_brands, collapse = "|")) ~ "reference",
      # If no brand in description, cannot classify from drug map alone
      TRUE ~ "unclassified"
    )
  )

# ── Classify using PBS Schedule data (definitive source) ──

cli_h2("Classification from PBS Schedule (items.csv)")

schedule_path <- here("data", "raw", "pbs_schedule", "pbs-api-csv", "tables_as_csv", "items.csv")

if (file.exists(schedule_path)) {
  schedule <- read_csv(schedule_path, show_col_types = FALSE)

  # Extract brand_name per pbs_code — pad to 6 chars to match our item_number format
  schedule_brands <- schedule %>%
    mutate(pbs_code = str_pad(pbs_code, width = 6, side = "left", pad = "0")) %>%
    select(pbs_code, brand_name) %>%
    distinct(pbs_code, .keep_all = TRUE)

  n_before <- sum(pbs_items$type != "unclassified")

  pbs_items <- pbs_items %>%
    left_join(schedule_brands, by = c("item_number" = "pbs_code")) %>%
    mutate(
      # Use PBS Schedule brand_name if available; fall back to drug map extraction
      brand_name = coalesce(brand_name, brand_from_desc),
      # Classify by brand name (originator_brand_indicator is unreliable)
      type = case_when(
        str_detect(tolower(brand_name), paste(biosimilar_brands, collapse = "|")) ~ "biosimilar",
        str_detect(tolower(brand_name), paste(reference_brands, collapse = "|")) ~ "reference",
        # Fall back to drug map description classification
        type %in% c("biosimilar", "reference") ~ type,
        TRUE ~ "unclassified"
      )
    )

  n_matched <- sum(!is.na(pbs_items$brand_name) &
                    pbs_items$brand_name != pbs_items$brand_from_desc, na.rm = TRUE)
  cli_alert_success("Matched {n_matched} items to PBS Schedule brands")
  cli_alert_success("Classified {sum(pbs_items$type != 'unclassified')} items total")
  cli_alert_info("Brands found: {paste(sort(unique(na.omit(pbs_items$brand_name))), collapse = ', ')}")
} else {
  cli_alert_warning("PBS Schedule not found at: {schedule_path}")
  cli_alert_info("Download from: https://www.pbs.gov.au/browse/downloads")
  cli_alert_info("  -> PBS API CSV files (ZIP) -> unzip to data/raw/pbs_schedule/pbs-api-csv/")
  pbs_items <- pbs_items %>% mutate(brand_name = brand_from_desc)
}

# ── Classify remaining items using temporal inference ──
# For items not in current PBS Schedule (delisted products), infer type from
# when the item first appears in prescribing data relative to biosimilar listing dates.

n_unclassified <- sum(pbs_items$type == "unclassified")

if (n_unclassified > 0) {
  cli_h2("Temporal inference for {n_unclassified} unclassified (delisted) items")

  # Load prescribing data to find first prescription dates
  dos_path <- here("data", "raw", "pbs_prescribing", "dos_data_raw.rds")
  med_path <- here("data", "raw", "pbs_prescribing", "medicare_stats_raw.rds")

  rx_data <- bind_rows(
    if (file.exists(dos_path)) readRDS(dos_path) %>% select(item_number, period, prescriptions),
    if (file.exists(med_path)) readRDS(med_path) %>% select(item_number, period, prescriptions)
  )

  if (nrow(rx_data) > 0) {
    item_first_rx <- rx_data %>%
      filter(prescriptions > 0) %>%
      group_by(item_number) %>%
      summarise(first_rx = min(period), .groups = "drop")

    first_bio_dates <- policy_timeline %>%
      filter(intervention_type == "biosimilar_listing") %>%
      group_by(molecule) %>%
      summarise(first_bio_date = min(date), .groups = "drop")

    pbs_items <- pbs_items %>%
      left_join(item_first_rx, by = "item_number") %>%
      left_join(first_bio_dates, by = "molecule") %>%
      mutate(
        type = case_when(
          type != "unclassified" ~ type,
          is.na(first_rx) ~ "no_data",
          first_rx < first_bio_date ~ "reference",
          TRUE ~ "biosimilar"
        )
      ) %>%
      select(-first_rx, -first_bio_date)

    cli_alert_info("After temporal inference:")
  }
}

# Final classification summary
cli_h2("Final classification")
pbs_items %>%
  count(molecule, type) %>%
  pivot_wider(names_from = type, values_from = n, values_fill = 0) %>%
  print(n = Inf)

# Write the full item mapping
pbs_item_mapping <- pbs_items %>%
  select(item_number, molecule, drug_name, form_strength, atc_code,
         brand_name, brand_from_desc, type)

write_csv(pbs_item_mapping, here("data", "reference", "pbs_item_mapping.csv"))
cli_alert_success("Wrote pbs_item_mapping.csv ({nrow(pbs_item_mapping)} items)")

# ─── 4. Validation summary ──────────────────────────────────────────────────

cli_h1("Validation Summary")

cli_h2("Items per molecule")
pbs_item_mapping %>%
  count(molecule, name = "n_items") %>%
  arrange(desc(n_items)) %>%
  print()

# Check all 10 molecules present
molecules_expected <- c("adalimumab", "infliximab", "etanercept", "rituximab",
                        "trastuzumab", "filgrastim", "pegfilgrastim",
                        "bevacizumab", "insulin glargine", "aflibercept")
molecules_found <- unique(pbs_item_mapping$molecule)
missing <- setdiff(molecules_expected, molecules_found)

if (length(missing) > 0) {
  cli_alert_danger("Missing molecules: {paste(missing, collapse = ', ')}")
} else {
  cli_alert_success("All {length(molecules_expected)} molecules have item mappings")
}

# Write item numbers batched by 20 for Medicare Statistics downloads
cli_h2("Item numbers for Medicare Statistics downloads")

batch_file <- here("data", "reference", "medicare_item_batches.txt")
sink(batch_file)

for (mol in sort(molecules_expected)) {
  items <- pbs_item_mapping %>%
    filter(molecule == mol) %>%
    pull(item_number) %>%
    sort()

  n_batches <- ceiling(length(items) / 20)
  cat(sprintf("=== %s (%d items, %d report%s needed) ===\n\n",
              toupper(mol), length(items), n_batches,
              if (n_batches == 1) "" else "s"))

  chunks <- split(items, ceiling(seq_along(items) / 20))
  for (i in seq_along(chunks)) {
    cat(sprintf("--- Batch %d of %d ---\n", i, n_batches))
    cat(paste(chunks[[i]], collapse = ", "), "\n\n")
  }
  cat("\n")
}

sink()

cli_alert_success("Wrote medicare_item_batches.txt — open and copy-paste batches of 20 into Medicare Statistics")
cli_alert_info("  File: {batch_file}")

# Also print summary to console
for (mol in sort(molecules_expected)) {
  n <- sum(pbs_item_mapping$molecule == mol)
  n_batches <- ceiling(n / 20)
  cli_alert_info("{mol}: {n} items ({n_batches} report{?s})")
}

cli_alert_success("Phase 1 Step 1 complete — all reference files written")
