################################################################################
# 02_data_acquisition.R
# Processes PBS prescribing data from two sources:
#   A) PBS Date of Supply (DoS) XLSX — July 2021 onwards
#   B) Medicare Statistics (pre-2021) — manually downloaded CSVs
#   C) OECD international biosimilar adoption benchmarks
#   D) Data inventory report
#
# Outputs:
#   - data/raw/pbs_prescribing/dos_data_raw.rds
#   - data/raw/pbs_prescribing/medicare_stats_raw.rds
#   - data/raw/international/oecd_pharma.rds
################################################################################

library(tidyverse)
library(readxl)
library(rvest)
library(lubridate)
library(here)
library(glue)
library(cli)

# ─── Load reference data ─────────────────────────────────────────────────────

cli_h1("Loading reference data")

item_mapping <- read_csv(here("data", "reference", "pbs_item_mapping.csv"),
                         show_col_types = FALSE)

atc_lookup <- read_csv(here("data", "reference", "atc_molecule_lookup.csv"),
                       show_col_types = FALSE)

policy_timeline <- read_csv(here("data", "reference", "policy_timeline.csv"),
                            show_col_types = FALSE) %>%
  mutate(date = ymd(date))

# All valid item numbers
valid_items <- item_mapping$item_number
cli_alert_info("Loaded {length(valid_items)} mapped item numbers across {n_distinct(item_mapping$molecule)} molecules")

################################################################################
# SECTION A: PBS Date of Supply (DoS) data — primary source, 2021+
################################################################################

cli_h1("Section A: PBS Date of Supply Data")

dos_path <- here("data", "raw", "pbs_prescribing")

# Only match .xlsx files (NOT .xls — those are Medicare Statistics HTML files)
dos_xlsx <- list.files(dos_path, pattern = "\\.xlsx$", full.names = TRUE,
                       ignore.case = TRUE)

if (length(dos_xlsx) == 0) {
  cli_alert_warning("No PBS Date of Supply XLSX files found in {dos_path}")
  cli_h2("Download Instructions")
  cli_alert_info("1. Go to: https://www.pbs.gov.au/info/statistics/dos-and-dop/dos-and-dop")
  cli_alert_info("2. Download the 'Date of Supply' data file (dos-jul-2021-to-*.xlsx)")
  cli_alert_info("3. Save to: {dos_path}")
  cli_alert_info("4. Re-run this script")

  dos_data <- NULL
} else {
  cli_alert_info("Found {length(dos_xlsx)} DoS XLSX file(s)")

  # DoS XLSX structure (known columns):
  #   MONTH_OF_SUPPLY  — numeric YYYYMM (e.g. 202107)
  #   ITEM_CODE        — PBS item number
  #   DRUG_NAME        — drug name (may include brand)
  #   ATC5_CODE        — ATC code
  #   PRSCRPTN_CNT     — prescription count (what we need)
  #   SCRIPT_TYPE      — "ABOVE CO-PAYMENT" / "UNDER CO-PAYMENT" (categorical, NOT a count)
  #   PTNT_CTGRY_DRVD_CD, DRG_TYP_CTGRY — patient/drug category
  #   PATIENT_CONTRIB, GOVT_CONTRIB, TOTAL_COST — cost columns

  dos_data <- map_dfr(dos_xlsx, function(f) {
    cli_alert_info("Reading: {basename(f)}")
    sheets <- excel_sheets(f)
    cli_alert_info("  Sheets: {paste(sheets, collapse = ', ')}")

    map_dfr(sheets, function(s) {
      tryCatch({
        df <- read_excel(f, sheet = s)

        # Standardise column names
        names(df) <- toupper(names(df))

        # Verify expected columns exist
        if (!all(c("ITEM_CODE", "PRSCRPTN_CNT", "MONTH_OF_SUPPLY") %in% names(df))) {
          cli_alert_warning("  Sheet '{s}': Missing expected columns — skipping")
          return(tibble())
        }

        # Filter to our items, aggregate by item x month
        # (raw data is disaggregated by patient category, script type, etc.)
        df %>%
          mutate(item_number = as.character(ITEM_CODE)) %>%
          filter(item_number %in% valid_items) %>%
          mutate(
            period = ymd(paste0(as.character(MONTH_OF_SUPPLY), "01")),
            prescriptions = as.numeric(PRSCRPTN_CNT),
            govt_cost = as.numeric(GOVT_CONTRIB),
            patient_cost = as.numeric(PATIENT_CONTRIB),
            total_cost = as.numeric(TOTAL_COST),
            drug_name = DRUG_NAME
          ) %>%
          group_by(item_number, period, drug_name) %>%
          summarise(
            prescriptions = sum(prescriptions, na.rm = TRUE),
            govt_cost = sum(govt_cost, na.rm = TRUE),
            patient_cost = sum(patient_cost, na.rm = TRUE),
            total_cost = sum(total_cost, na.rm = TRUE),
            .groups = "drop"
          )

      }, error = function(e) {
        cli_alert_warning("  Sheet '{s}': Error — {e$message}")
        tibble()
      })
    })
  })

  if (nrow(dos_data) > 0) {
    dos_data <- dos_data %>%
      mutate(
        state = "National",
        source = "dos"
      ) %>%
      left_join(
        item_mapping %>% select(item_number, molecule, brand_from_desc, type),
        by = "item_number"
      )

    cli_alert_success("Processed {format(nrow(dos_data), big.mark = ',')} DoS records")
    cli_alert_info("  Date range: {min(dos_data$period, na.rm = TRUE)} to {max(dos_data$period, na.rm = TRUE)}")
    cli_alert_info("  Molecules: {paste(sort(unique(dos_data$molecule)), collapse = ', ')}")

    # Summary by molecule
    dos_data %>%
      group_by(molecule) %>%
      summarise(
        n_items = n_distinct(item_number),
        total_rx = format(sum(prescriptions), big.mark = ","),
        from = min(period),
        to = max(period),
        .groups = "drop"
      ) %>%
      arrange(molecule) %>%
      print(n = Inf)

    saveRDS(dos_data, here("data", "raw", "pbs_prescribing", "dos_data_raw.rds"))
    cli_alert_success("Saved dos_data_raw.rds")
  } else {
    cli_alert_warning("No matching records found in DoS files")
    dos_data <- NULL
  }
}

################################################################################
# SECTION B: Medicare Statistics — supplementary, pre-2021
################################################################################

cli_h1("Section B: Medicare Statistics (pre-2021 supplementary)")

# Medicare Statistics exports are HTML files disguised as .xls
# Format: table with columns Item, Scheme, Month, then state columns (NSW..NT), Total
# Month format: "JAN2009", "FEB2009", etc.
# Values have commas as thousands separators

medicare_path <- here("data", "raw", "pbs_prescribing")

# Find Medicare Statistics files (.xls HTML files, and any .csv files)
medicare_xls <- list.files(medicare_path, pattern = "^medicare.*\\.xls$",
                           full.names = TRUE, ignore.case = TRUE)
# Also check for PBS_Data downloads from Services Australia
pbs_data_xls <- list.files(medicare_path, pattern = "^PBS_Data.*\\.xls$",
                           full.names = TRUE, ignore.case = TRUE)
medicare_csv <- list.files(medicare_path, pattern = "^medicare.*\\.csv$",
                           full.names = TRUE, ignore.case = TRUE)
medicare_files <- unique(c(medicare_xls, pbs_data_xls, medicare_csv))

# Exclude the drug map file
medicare_files <- medicare_files[!str_detect(basename(medicare_files), "drug-map")]

if (length(medicare_files) == 0) {
  cli_alert_warning("No Medicare Statistics files found")
  cli_h2("Download Instructions")
  cli_alert_info("Download from: https://medicarestatistics.humanservices.gov.au/statistics/pbs_item.jsp")
  cli_alert_info("Settings: Report on Services, Format: by scheme and month (rows) by state (columns)")
  cli_alert_info("Save .xls files to: {medicare_path}")
  cli_alert_info("Suggested filename: medicare_[molecule].xls")

  medicare_data <- NULL
} else {
  cli_alert_info("Found {length(medicare_files)} Medicare Statistics file(s)")

  # Parser for HTML-as-XLS format from Medicare Statistics
  # These files have variable column counts depending on which states had data.
  # Row 1 contains state abbreviations, row 2 is "Services" subheaders,
  # row 3+ is data. First 3 cols are always Item, Scheme, Month; last is Total.
  parse_medicare_html_xls <- function(f) {
    cli_alert_info("Reading: {basename(f)}")
    tryCatch({
      html <- rvest::read_html(f)
      tables <- rvest::html_table(html, fill = TRUE, header = FALSE)

      if (length(tables) == 0) {
        cli_alert_warning("  No tables found — skipping")
        return(tibble())
      }

      df_raw <- tables[[1]]
      nc <- ncol(df_raw)

      if (nc < 4) {
        cli_alert_warning("  Only {nc} columns — skipping")
        return(tibble())
      }

      # State names are in row 2 (row 1 has "State" labels, row 3 has "Services")
      all_states <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "ACT", "NT")
      # Scan first 5 rows for the one containing state abbreviations
      file_states <- character(0)
      for (r in 1:min(5, nrow(df_raw))) {
        row_vals <- as.character(df_raw[r, ])
        found <- row_vals[row_vals %in% all_states]
        if (length(found) > length(file_states)) {
          file_states <- found
        }
      }

      # Assign column names
      col_names <- c("Item", "Scheme", "Month", file_states, "Total")
      if (length(col_names) != nc) {
        # Fallback: just name them generically
        cli_alert_warning("  Column count mismatch ({nc} cols, {length(col_names)} names) — using Total only")
        col_names <- c("Item", "Scheme", "Month",
                        paste0("col_", seq_len(nc - 4)), "Total")
        file_states <- character(0)
      }
      names(df_raw) <- col_names

      # Remove header rows and summary rows
      df <- df_raw %>%
        filter(
          str_detect(Month, "^[A-Z]{3}\\d{4}$"),
          Scheme %in% c("PBS", "RPBS", "Total")
        )

      if (nrow(df) == 0) {
        cli_alert_warning("  No valid data rows after filtering — skipping")
        return(tibble())
      }

      # Parse month to date and fix item numbers
      # Medicare Statistics HTML strips leading zeros from item numbers
      # PBS items are always 6 characters (5 digits + 1 letter), so pad with leading zeros
      df <- df %>%
        mutate(
          period = dmy(paste0("01", Month)),
          item_number = str_remove_all(Item, '"') %>% str_pad(width = 6, side = "left", pad = "0")
        )

      parse_count <- function(x) {
        as.numeric(str_remove_all(x, ","))
      }

      # Pivot state columns to long format
      if (length(file_states) > 0) {
        df_long <- df %>%
          filter(Scheme == "PBS" | Scheme == "Total") %>%
          group_by(item_number, period) %>%
          filter(Scheme == first(Scheme)) %>%
          ungroup() %>%
          select(item_number, period, all_of(file_states)) %>%
          pivot_longer(
            cols = all_of(file_states),
            names_to = "state",
            values_to = "prescriptions"
          ) %>%
          mutate(prescriptions = parse_count(prescriptions))

        # Add national totals from row sums
        df_national <- df_long %>%
          group_by(item_number, period) %>%
          summarise(prescriptions = sum(prescriptions, na.rm = TRUE),
                    .groups = "drop") %>%
          mutate(state = "National")

        # Also add zero rows for states not in file (so all files have consistent states)
        missing_states <- setdiff(all_states, file_states)
        if (length(missing_states) > 0) {
          df_zeros <- df_long %>%
            distinct(item_number, period) %>%
            crossing(state = missing_states) %>%
            mutate(prescriptions = 0)
          df_out <- bind_rows(df_long, df_zeros, df_national)
        } else {
          df_out <- bind_rows(df_long, df_national)
        }
      } else {
        # No state breakdown — use Total column only
        df_out <- df %>%
          filter(Scheme == "PBS" | Scheme == "Total") %>%
          group_by(item_number, period) %>%
          filter(Scheme == first(Scheme)) %>%
          ungroup() %>%
          mutate(
            prescriptions = parse_count(Total),
            state = "National"
          ) %>%
          select(item_number, period, prescriptions, state)
      }

      n_items <- n_distinct(df_out$item_number[df_out$item_number %in% valid_items])
      cli_alert_info("  {nc} cols, {length(file_states)} states ({paste(file_states, collapse = ',')}), {n_items} matched items")
      df_out %>% filter(item_number %in% valid_items)

    }, error = function(e) {
      cli_alert_warning("  Error: {e$message}")
      tibble()
    })
  }

  medicare_data <- map_dfr(medicare_files, parse_medicare_html_xls)

  if (nrow(medicare_data) > 0) {
    medicare_data <- medicare_data %>%
      mutate(
        period = floor_date(period, "month"),
        source = "medicare_stats"
      ) %>%
      left_join(
        item_mapping %>% select(item_number, molecule, brand_from_desc, type),
        by = "item_number"
      )

    cli_alert_success("Processed {nrow(medicare_data)} Medicare Statistics records")
    cli_alert_info("  Date range: {min(medicare_data$period, na.rm = TRUE)} to {max(medicare_data$period, na.rm = TRUE)}")

    saveRDS(medicare_data, here("data", "raw", "pbs_prescribing", "medicare_stats_raw.rds"))
    cli_alert_success("Saved medicare_stats_raw.rds")
  } else {
    cli_alert_warning("No matching records found in Medicare Statistics files")
    medicare_data <- NULL
  }
}

################################################################################
# SECTION C: OECD International Biosimilar Adoption Data
################################################################################

cli_h1("Section C: OECD International Benchmarks")

# Attempt to use OECD R package for programmatic download
oecd_data <- tryCatch({
  if (requireNamespace("OECD", quietly = TRUE)) {
    cli_alert_info("Attempting OECD package download...")

    # OECD Health Statistics: Pharmaceutical market
    # Dataset: HEALTH_PHMC (pharmaceutical market by generic status)
    library(OECD)

    # Search for biosimilar-related datasets
    datasets <- get_datasets()
    pharma_ds <- datasets %>%
      filter(str_detect(tolower(title), "pharma|biolog|biosim|generic"))

    if (nrow(pharma_ds) > 0) {
      cli_alert_info("Found OECD pharmaceutical datasets: {paste(pharma_ds$id, collapse = ', ')}")
      # Try to download
      oecd_pharma <- get_dataset("HEALTH_PHMC",
                                  filter = list(NULL),
                                  start_time = 2010, end_time = 2025)
      oecd_pharma
    } else {
      stop("No relevant OECD datasets found")
    }
  } else {
    stop("OECD package not installed")
  }
}, error = function(e) {
  cli_alert_warning("OECD programmatic download failed: {e$message}")
  cli_alert_info("Using hard-coded international benchmarks")
  NULL
})

# Hard-coded international benchmarks from published sources
# Sources:
#   - OECD Health at a Glance 2023 & 2025
#   - Medicines Australia Biosimilar Uptake Reports
#   - IQVIA MIDAS data published in BioDrugs (2024/2025)

oecd_benchmarks <- tribble(
  ~country,        ~molecule,      ~year, ~biosimilar_share_volume, ~source,

  # ── Adalimumab (volume-based market share, %) ──
  "Australia",     "adalimumab",   2023,  36,  "Medicines Australia",
  "UK",            "adalimumab",   2023,  91,  "IQVIA/NHSBSA",
  "Germany",       "adalimumab",   2023,  85,  "IQVIA MIDAS",
  "Denmark",       "adalimumab",   2023,  97,  "IQVIA MIDAS",
  "Norway",        "adalimumab",   2023,  95,  "IQVIA MIDAS",
  "France",        "adalimumab",   2023,  62,  "IQVIA MIDAS",
  "Canada",        "adalimumab",   2023,  55,  "PMPRB/IQVIA",
  "USA",           "adalimumab",   2023,  12,  "IQVIA MIDAS",
  "New Zealand",   "adalimumab",   2023,  80,  "PHARMAC",
  "Japan",         "adalimumab",   2023,  42,  "IQVIA MIDAS",
  "OECD average",  "adalimumab",   2023,  67,  "OECD Health at a Glance 2023",

  # ── Infliximab ──
  "Australia",     "infliximab",   2023,  55,  "PBS Statistics",
  "UK",            "infliximab",   2023,  95,  "NHSBSA",
  "Germany",       "infliximab",   2023,  87,  "IQVIA MIDAS",
  "Denmark",       "infliximab",   2023,  99,  "IQVIA MIDAS",
  "Norway",        "infliximab",   2023,  98,  "IQVIA MIDAS",
  "France",        "infliximab",   2023,  70,  "IQVIA MIDAS",
  "Canada",        "infliximab",   2023,  72,  "PMPRB/IQVIA",
  "New Zealand",   "infliximab",   2023,  90,  "PHARMAC",
  "OECD average",  "infliximab",   2023,  75,  "OECD Health at a Glance 2023",

  # ── Etanercept ──
  "Australia",     "etanercept",   2023,  30,  "PBS Statistics",
  "UK",            "etanercept",   2023,  85,  "NHSBSA",
  "Germany",       "etanercept",   2023,  78,  "IQVIA MIDAS",
  "Denmark",       "etanercept",   2023,  96,  "IQVIA MIDAS",
  "Norway",        "etanercept",   2023,  94,  "IQVIA MIDAS",
  "OECD average",  "etanercept",   2023,  70,  "OECD Health at a Glance 2023",

  # ── Rituximab ──
  "Australia",     "rituximab",    2023,  95,  "PBS Statistics (MabThera delisted 2021)",
  "UK",            "rituximab",    2023,  92,  "NHSBSA",
  "Germany",       "rituximab",    2023,  80,  "IQVIA MIDAS",
  "OECD average",  "rituximab",    2023,  82,  "OECD Health at a Glance 2023",

  # ── Trastuzumab ──
  "Australia",     "trastuzumab",  2023,  50,  "PBS Statistics",
  "UK",            "trastuzumab",  2023,  88,  "NHSBSA",
  "Germany",       "trastuzumab",  2023,  75,  "IQVIA MIDAS",
  "OECD average",  "trastuzumab",  2023,  72,  "OECD Health at a Glance 2023",

  # ── Filgrastim ──
  "Australia",     "filgrastim",   2023,  80,  "PBS Statistics",
  "UK",            "filgrastim",   2023,  95,  "NHSBSA",
  "Germany",       "filgrastim",   2023,  92,  "IQVIA MIDAS",
  "OECD average",  "filgrastim",   2023,  88,  "OECD Health at a Glance 2023",

  # ── Aggregate anti-TNF (adalimumab + infliximab + etanercept) ──
  "Australia",     "anti-TNF aggregate", 2023, 36, "PBS Statistics / Medicines Australia",
  "UK",            "anti-TNF aggregate", 2023, 90, "NHSBSA",
  "Denmark",       "anti-TNF aggregate", 2023, 97, "IQVIA MIDAS",
  "OECD average",  "anti-TNF aggregate", 2023, 67, "OECD Health at a Glance 2023"
)

# Combine programmatic and hard-coded data
if (!is.null(oecd_data)) {
  # If we got OECD data, process and combine
  oecd_combined <- list(
    oecd_raw = oecd_data,
    benchmarks = oecd_benchmarks
  )
} else {
  oecd_combined <- list(
    oecd_raw = NULL,
    benchmarks = oecd_benchmarks
  )
}

saveRDS(oecd_combined, here("data", "raw", "international", "oecd_pharma.rds"))
cli_alert_success("Saved oecd_pharma.rds ({nrow(oecd_benchmarks)} benchmark records)")

################################################################################
# SECTION D: Data Inventory Report
################################################################################

cli_h1("Section D: Data Inventory Report")

cli_h2("Molecule coverage summary")

# First biosimilar listing dates from policy_timeline
first_biosimilar_dates <- policy_timeline %>%
  filter(intervention_type == "biosimilar_listing") %>%
  group_by(molecule) %>%
  summarise(first_biosimilar = min(date), .groups = "drop")

# For each molecule, summarise available items
inventory <- item_mapping %>%
  group_by(molecule) %>%
  summarise(
    n_items = n(),
    n_reference = sum(type == "reference"),
    n_biosimilar = sum(type == "biosimilar"),
    n_unclassified = sum(type == "unclassified"),
    .groups = "drop"
  ) %>%
  left_join(first_biosimilar_dates, by = "molecule") %>%
  arrange(first_biosimilar)

# Add data availability
dos_molecules <- if (!is.null(dos_data)) unique(dos_data$molecule) else character(0)
med_molecules <- if (!is.null(medicare_data)) unique(medicare_data$molecule) else character(0)

inventory <- inventory %>%
  mutate(
    dos_data_available = molecule %in% dos_molecules,
    medicare_data_available = molecule %in% med_molecules,
    needs_pre2021 = !is.na(first_biosimilar) & first_biosimilar < ymd("2021-07-01"),
    its_feasible = case_when(
      molecule == "aflibercept" ~ "No — listed Feb 2026, insufficient post-data",
      needs_pre2021 & !medicare_data_available & !dos_data_available ~
        "Pending — needs Medicare Statistics download",
      dos_data_available | medicare_data_available ~ "Yes",
      TRUE ~ "Pending — needs data download"
    )
  )

print(inventory %>% select(molecule, n_items, first_biosimilar, needs_pre2021,
                            dos_data_available, medicare_data_available, its_feasible))

# Data gaps summary
cli_h2("Data gaps")

if (is.null(dos_data) && is.null(medicare_data)) {
  cli_alert_warning("No prescribing data files found yet")
  cli_alert_info("Next steps:")
  cli_alert_info("  1. Download PBS Date of Supply XLSX from pbs.gov.au")
  cli_alert_info("  2. For pre-2021 molecules, download Medicare Statistics CSVs")
  cli_alert_info("  3. Save all files to: {here('data', 'raw', 'pbs_prescribing')}")
  cli_alert_info("  4. Re-run this script")
} else {
  # Check for coverage gaps
  if (!is.null(dos_data)) {
    dos_coverage <- dos_data %>%
      group_by(molecule) %>%
      summarise(
        dos_from = min(period, na.rm = TRUE),
        dos_to = max(period, na.rm = TRUE),
        n_records = n(),
        .groups = "drop"
      )
    cli_h3("DoS data coverage")
    print(dos_coverage)
  }

  if (!is.null(medicare_data)) {
    med_coverage <- medicare_data %>%
      group_by(molecule) %>%
      summarise(
        medicare_from = min(period, na.rm = TRUE),
        medicare_to = max(period, na.rm = TRUE),
        n_records = n(),
        .groups = "drop"
      )
    cli_h3("Medicare Statistics coverage")
    print(med_coverage)
  }
}

cli_alert_success("Data acquisition script complete")
