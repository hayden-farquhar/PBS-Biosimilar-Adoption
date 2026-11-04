################################################################################
# 16_brand_level_benchmark.R
# Benchmarks the item-level bounds against the brand-level prescription counts
# the Department publishes in the annual PBS Expenditure and Prescriptions
# Report
#
# The report's Table 5(g) lists every PBS biosimilar brand and Table 5(e) the
# top 50 brands, with brand taken from the manufacturer code in PBS Online
# Claims data (records with a missing code are excluded). Tables 5(a)/5(b) give
# the molecule total. Counts are subsidised (above co-payment) prescriptions,
# Section 85 and Section 100, excluding Efficient Funding of Chemotherapy, by
# financial year.
#
# Brand-level share for a molecule:
#   - reference brand in the top 50 (adalimumab: Humira): lower = biosimilar /
#     molecule total, upper = 1 - reference / molecule total (the gap is
#     prescriptions with a missing manufacturer code)
#   - reference brand not reported (etanercept, infliximab): lower = biosimilar /
#     molecule total only
#
# Requires pdftotext (poppler) on the PATH.
#
# Inputs:
#   - data/raw/pbs_expenditure_prescriptions/PBS-Expenditure-prescriptions-report-2023-24.pdf
#   - data/raw/pbs_expenditure_prescriptions/Expenditure-prescriptions-report-tables-2024-25.pdf
#   - outputs/tables/tbl_item_class_monthly.csv        (script 11)
#   - data/processed/market_share_national.csv          (conventional series)
#   - outputs/tables/tbl_item_class_monthly_archived.csv (script 13)
# Outputs:
#   - outputs/tables/tbl_brand_level_counts.csv
#   - outputs/tables/table3_brand_level_benchmark.csv
#   - outputs/tables/tbl_rituximab_iqvia_window.csv
################################################################################

library(tidyverse)
library(here)
library(cli)

reports <- tribble(
  ~fy,       ~file,
  "2023-24", "PBS-Expenditure-prescriptions-report-2023-24.pdf",
  "2024-25", "Expenditure-prescriptions-report-tables-2024-25.pdf"
)

molecules <- c("ADALIMUMAB", "ETANERCEPT", "INFLIXIMAB")
reference_brands <- c(ADALIMUMAB = "Humira", ETANERCEPT = "Enbrel", INFLIXIMAB = "Remicade")

num <- function(x) as.numeric(gsub(",", "", x))

read_report <- function(fy, file) {
  txt <- tempfile(fileext = ".txt")
  status <- system2("pdftotext", c("-layout", shQuote(here("data", "raw", "pbs_expenditure_prescriptions", file)),
                                   shQuote(txt)))
  stopifnot(status == 0)
  lines <- readLines(txt, warn = FALSE)

  # Brand rows: "[rank]  Brand®  DRUG  prescriptions  $cost ..."
  brand_re <- "^\\s*(?:\\d+\\s+)?([A-Z][A-Za-z ]*?)®\\s+([A-Z][A-Z ]+?)\\s{2,}([\\d,]+)\\s+\\$"
  brands <- str_match(lines, brand_re) %>% as_tibble(.name_repair = "minimal") %>%
    set_names(c("line", "brand", "drug", "prescriptions")) %>%
    filter(!is.na(line), drug %in% molecules) %>%
    distinct(brand, drug, .keep_all = TRUE) %>%
    transmute(fy, drug, brand, prescriptions = num(prescriptions),
              is_reference = brand == reference_brands[drug])

  # Molecule rows: "rank  DRUG  prescriptions  $cost ..." (same total in each table)
  drug_re <- paste0("^\\s*\\d+\\s+(", paste(molecules, collapse = "|"), ")\\s{2,}([\\d,]+)\\s+\\$")
  totals <- str_match(lines, drug_re) %>% as_tibble(.name_repair = "minimal") %>%
    set_names(c("line", "drug", "prescriptions")) %>%
    filter(!is.na(line)) %>%
    distinct(drug, .keep_all = TRUE) %>%
    transmute(fy, drug, molecule_total = num(prescriptions))

  brands %>% left_join(totals, by = c("fy", "drug"))
}

counts <- pmap_dfr(reports, read_report)
write_csv(counts, here("outputs", "tables", "tbl_brand_level_counts.csv"))
print(counts, n = Inf)

brand_share <- counts %>%
  group_by(fy, molecule = tolower(drug)) %>%
  summarise(
    molecule_total = first(molecule_total),
    biosimilar_rx = sum(prescriptions[!is_reference]),
    reference_rx = if (any(is_reference)) sum(prescriptions[is_reference]) else NA_real_,
    n_biosimilar_brands = sum(!is_reference),
    .groups = "drop"
  ) %>%
  mutate(brand_lower = biosimilar_rx / molecule_total,
         brand_upper = if_else(is.na(reference_rx), NA_real_, 1 - reference_rx / molecule_total))

# ─── Item-level bounds and conventional share over the same financial years ─

fy_of <- function(d) if_else(month(d) >= 7,
                              paste0(year(d), "-", substr(year(d) + 1, 3, 4)),
                              paste0(year(d) - 1, "-", substr(year(d), 3, 4)))

item_fy <- read_csv(here("outputs", "tables", "tbl_item_class_monthly.csv"),
                    show_col_types = FALSE, guess_max = Inf) %>%
  filter(molecule %in% tolower(molecules)) %>%
  mutate(fy = fy_of(period)) %>%
  filter(fy %in% reports$fy) %>%
  group_by(molecule, fy) %>%
  summarise(months = n(), item_total_rx = sum(total_rx),
            item_lower = sum(biosimilar_only) / sum(total_rx),
            item_upper = 1 - sum(reference_only) / sum(total_rx),
            .groups = "drop")

conventional_fy <- read_csv(here("data", "processed", "market_share_national.csv"),
                            show_col_types = FALSE) %>%
  filter(molecule %in% tolower(molecules)) %>%
  mutate(fy = fy_of(period)) %>%
  filter(fy %in% reports$fy) %>%
  group_by(molecule, fy) %>%
  summarise(conventional = sum(biosimilar_rx) / sum(total_rx), .groups = "drop")

benchmark <- brand_share %>%
  left_join(item_fy, by = c("molecule", "fy")) %>%
  left_join(conventional_fy, by = c("molecule", "fy")) %>%
  mutate(
    brand_within_item_bounds = brand_lower >= item_lower - 1e-9 &
      coalesce(brand_upper, brand_lower) <= item_upper + 1e-9,
    conventional_minus_brand_pp = 100 * (conventional - brand_lower)
  ) %>%
  arrange(molecule, fy)

write_csv(benchmark, here("outputs", "tables", "table3_brand_level_benchmark.csv"))
benchmark %>%
  mutate(across(c(brand_lower, brand_upper, item_lower, item_upper, conventional), ~ round(100 * .x, 1)),
         conventional_minus_brand_pp = round(conventional_minus_brand_pp, 1)) %>%
  select(molecule, fy, molecule_total, item_total_rx, brand_lower, brand_upper,
         item_lower, item_upper, conventional, conventional_minus_brand_pp,
         brand_within_item_bounds) %>%
  print(width = Inf)

# ─── Rituximab: IQVIA sales comparison window ───────────────────────────────
# Hillen et al. (Value Health Reg Issues 2025;50:101173, Table 4) report the
# biosimilar share of rituximab standardised units over the 12 months after
# biosimilar entry, from quarterly IQVIA-MIDAS hospital and retail sales. Entry
# is taken as October 2019 (Riximyo PBS listing; their 1.25 years of
# biosimilar market time to the end of 2020 agrees), so the window is
# October 2019 to September 2020. Bounds are volume-weighted over the window.

iqvia_window <- read_csv(here("outputs", "tables", "tbl_item_class_monthly_archived.csv"),
                         show_col_types = FALSE) %>%
  filter(molecule == "rituximab",
         period >= as.Date("2019-10-01"), period <= as.Date("2020-09-01")) %>%
  summarise(months = n(),
            archived_lower = weighted.mean(archived_lower, total_rx),
            archived_upper = weighted.mean(archived_upper, total_rx),
            conventional = weighted.mean(conventional_share, total_rx),
            total_rx = sum(total_rx))
write_csv(iqvia_window, here("outputs", "tables", "tbl_rituximab_iqvia_window.csv"))
print(iqvia_window)

cli_alert_success("Wrote tbl_brand_level_counts.csv, table3_brand_level_benchmark.csv, tbl_rituximab_iqvia_window.csv")
