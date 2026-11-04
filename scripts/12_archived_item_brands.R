################################################################################
# 12_archived_item_brands.R
# Recovers the brands listed under each PBS item number at past dates, from
# Internet Archive (Wayback Machine) snapshots of www.pbs.gov.au item pages
#
# Script 11 classes items by a single February 2026 Schedule snapshot. Items
# are reassigned between brands over time, so that composition may not hold in
# earlier months. This script recovers archived compositions so script 11's
# assumption can be tested, and dates reference-brand withdrawal per item.
#
# Target items: those covering 99% of post-listing national volume for
# rituximab and 95% for adalimumab, etanercept and infliximab (most volume
# first). One snapshot per item per calendar half-year, the earliest in each.
#
# Resumable: every raw page is cached before parsing and a rerun skips pages
# already on disk. Wayback is slow and drops requests; failures back off and
# retry, and anything still failing is logged and picked up by the next run.
#
# Inputs:
#   - data/raw/pbs_prescribing/dos_data_raw.rds
#   - data/raw/pbs_prescribing/medicare_stats_raw.rds
#   - data/reference/policy_timeline.csv
# Outputs:
#   - data/raw/pbs_schedule/wayback_item_pages/<item>/<timestamp>.html (cache)
#   - data/raw/pbs_schedule/wayback_item_pages/_cdx/<item>.txt          (cache)
#   - outputs/tables/tbl_archived_item_brands.csv
#   - outputs/tables/tbl_archived_fetch_log.csv
################################################################################

library(tidyverse)
library(here)
library(httr)
library(rvest)
library(cli)

cache_dir <- here("data", "raw", "pbs_schedule", "wayback_item_pages")
dir.create(file.path(cache_dir, "_cdx"), recursive = TRUE, showWarnings = FALSE)

coverage <- c(rituximab = 0.99, adalimumab = 0.95,
              etanercept = 0.95, infliximab = 0.95)
pause_s <- 4   # between live requests, to stay well inside Wayback's limits

# ─── Target items ────────────────────────────────────────────────────────────

cli_h1("Selecting target items")

first_bio <- read_csv(here("data", "reference", "policy_timeline.csv"),
                      show_col_types = FALSE) %>%
  filter(intervention_type == "biosimilar_listing") %>%
  group_by(molecule) %>%
  summarise(first_bio_date = min(as.Date(date)), .groups = "drop")

dos <- readRDS(here("data", "raw", "pbs_prescribing", "dos_data_raw.rds"))
med <- readRDS(here("data", "raw", "pbs_prescribing", "medicare_stats_raw.rds"))

volume <- bind_rows(
  dos %>% select(item_number, period, prescriptions, state, source, molecule),
  med %>% select(item_number, period, prescriptions, state, source, molecule)
) %>%
  filter(state == "National", molecule %in% names(coverage)) %>%
  group_by(item_number, period) %>%
  filter(n_distinct(source) == 1 | source == "dos") %>%
  ungroup() %>%
  inner_join(first_bio, by = "molecule") %>%
  filter(period >= first_bio_date)

targets <- volume %>%
  group_by(molecule, item_number) %>%
  summarise(rx = sum(prescriptions), first_rx = min(period),
            last_rx = max(period), .groups = "drop") %>%
  group_by(molecule) %>%
  arrange(desc(rx), .by_group = TRUE) %>%
  mutate(cum_share = cumsum(rx) / sum(rx)) %>%
  filter(lag(cum_share, default = 0) < coverage[molecule]) %>%
  ungroup() %>%
  # Molecule priority order, then volume
  mutate(molecule = factor(molecule, levels = names(coverage))) %>%
  arrange(molecule, desc(rx)) %>%
  mutate(molecule = as.character(molecule)) %>%
  left_join(first_bio, by = "molecule")

targets %>% count(molecule) %>% print()

# ─── Helpers ─────────────────────────────────────────────────────────────────

# PBS item URLs drop the leading zero (07257Y -> 7257Y)
item_url <- function(item) paste0("www.pbs.gov.au/medicine/item/", sub("^0+", "", item))

polite_get <- function(url, tries = 5) {
  for (i in seq_len(tries)) {
    resp <- tryCatch(GET(url, timeout(120), user_agent("P20 research; archived PBS item pages")),
                     error = function(e) NULL)
    Sys.sleep(pause_s)
    if (!is.null(resp) && status_code(resp) == 200) return(resp)
    Sys.sleep(pause_s * 2^i)   # back off: 8, 16, 32, 64, 128 s
  }
  NULL
}

get_cdx <- function(item) {
  f <- file.path(cache_dir, "_cdx", paste0(item, ".txt"))
  if (file.exists(f)) return(readLines(f))
  url <- paste0("https://web.archive.org/cdx/search/cdx?url=", item_url(item),
                "&fl=timestamp&filter=statuscode:200&from=2015")
  resp <- polite_get(url)
  if (is.null(resp)) return(NULL)
  txt <- content(resp, "text", encoding = "UTF-8")
  if (grepl("<html", txt, ignore.case = TRUE)) return(NULL)   # Wayback error page
  ts <- str_trim(strsplit(txt, "\n")[[1]])
  ts <- ts[grepl("^[0-9]{14}$", ts)]
  writeLines(ts, f)
  ts
}

get_page <- function(item, ts) {
  d <- file.path(cache_dir, item)
  dir.create(d, showWarnings = FALSE)
  f <- file.path(d, paste0(ts, ".html"))
  if (file.exists(f) && file.size(f) > 1000) return(f)
  url <- paste0("https://web.archive.org/web/", ts, "id_/http://", item_url(item))
  resp <- polite_get(url)
  if (is.null(resp)) return(NULL)
  writeBin(content(resp, "raw"), f)
  f
}

# Brand names are the link text in the "Available brands" table. The class is
# misspelt "avaiablebrands" on the live site; accept either spelling. The older
# layout (still served on some pages into 2022) has no such table: each brand
# is a manufacturer link in the "Available brands" rows of table#medicine-item.
parse_brands <- function(f) {
  doc <- tryCatch(read_html(f), error = function(e) NULL)
  if (is.null(doc)) return(NA_character_)
  tbl <- html_elements(doc, "table.avaiablebrands, table.availablebrands")
  b <- if (length(tbl) > 0) {
    html_text2(html_elements(tbl, "td a"))
  } else {
    html_text2(html_elements(doc, "table#medicine-item a[href*='/browse/manufacturer/']"))
  }
  b <- str_trim(b)
  # "SO" (Supply Only) is a status marker link with tooltip text, not a brand
  b <- unique(b[nchar(b) > 0 & !str_detect(b, "^SO(\\s|$)")])
  if (length(b) == 0) NA_character_ else paste(sort(b), collapse = "; ")
}

# ─── Fetch ───────────────────────────────────────────────────────────────────

cli_h1("Fetching archived item pages")

log_rows <- list()
brand_rows <- list()

for (k in seq_len(nrow(targets))) {
  item <- targets$item_number[k]
  mol <- targets$molecule[k]
  ts <- get_cdx(item)
  if (is.null(ts)) {
    log_rows[[length(log_rows) + 1]] <- tibble(item_number = item, molecule = mol,
                                               timestamp = NA, status = "cdx_failed")
    next
  }
  if (length(ts) == 0) {
    log_rows[[length(log_rows) + 1]] <- tibble(item_number = item, molecule = mol,
                                               timestamp = NA, status = "no_snapshots")
    next
  }
  # Earliest snapshot in each calendar half-year, from a year before first
  # listing (to see the pre-listing composition) to the end of the data
  snap <- tibble(timestamp = ts,
                 date = as.Date(substr(ts, 1, 8), "%Y%m%d")) %>%
    filter(date >= targets$first_bio_date[k] - 365,
           date <= as.Date("2026-02-28")) %>%
    mutate(half = paste0(format(date, "%Y"), if_else(as.integer(format(date, "%m")) <= 6, "H1", "H2"))) %>%
    group_by(half) %>% slice_min(date, n = 1, with_ties = FALSE) %>% ungroup()

  for (j in seq_len(nrow(snap))) {
    f <- get_page(item, snap$timestamp[j])
    status <- if (is.null(f)) "page_failed" else "ok"
    brands <- if (is.null(f)) NA_character_ else parse_brands(f)
    if (status == "ok" && is.na(brands)) status <- "parse_failed"
    log_rows[[length(log_rows) + 1]] <- tibble(item_number = item, molecule = mol,
                                               timestamp = snap$timestamp[j], status = status)
    if (status == "ok") {
      brand_rows[[length(brand_rows) + 1]] <- tibble(
        molecule = mol, item_number = item, snapshot_date = snap$date[j],
        timestamp = snap$timestamp[j], brands = brands)
    }
  }
  cli_alert_info("[{k}/{nrow(targets)}] {mol} {item}: {nrow(snap)} snapshots")
}

fetch_log <- bind_rows(log_rows)
write_csv(fetch_log, here("outputs", "tables", "tbl_archived_fetch_log.csv"))

# ─── Classify archived compositions ──────────────────────────────────────────

reference_brands <- c("neupogen", "remicade", "enbrel", "mabthera", "herceptin",
                      "neulasta", "avastin", "lantus", "optisulin", "toujeo",
                      "humira", "eylea")

archived <- bind_rows(brand_rows) %>%
  mutate(
    has_ref = str_detect(tolower(brands), paste(reference_brands, collapse = "|")),
    has_other = map_lgl(strsplit(tolower(brands), "; "),
                        ~ any(!str_detect(.x, paste(reference_brands, collapse = "|")))),
    archived_class = case_when(has_ref & has_other ~ "mixed",
                               has_ref ~ "reference_only",
                               has_other ~ "biosimilar_only")
  ) %>%
  select(-has_ref, -has_other)

write_csv(archived, here("outputs", "tables", "tbl_archived_item_brands.csv"))

cli_h1("Done")
fetch_log %>% count(molecule, status) %>% print(n = Inf)
cli_alert_success("{nrow(archived)} archived item compositions written to tbl_archived_item_brands.csv")
