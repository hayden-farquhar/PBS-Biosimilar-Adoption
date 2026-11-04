# Can Shared Reimbursement Codes Measure Biosimilar Uptake? Code and data

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22732780.svg)](https://doi.org/10.5281/zenodo.22732780)

Archived at Zenodo. The concept DOI [10.5281/zenodo.22732780](https://doi.org/10.5281/zenodo.22732780) always resolves to the latest version.

Code and data for:

> Farquhar H. Can Shared Reimbursement Codes Measure Biosimilar Uptake? Identification Bounds and Validation Against Brand-Level Counts in Australia's Pharmaceutical Benefits Scheme. Manuscript in preparation.

## Overview

Australia's Pharmaceutical Benefits Scheme (PBS) publishes dispensing by item number, and one item number can cover both the reference brand and its biosimilars. This code decomposes PBS dispensing for ten biologic molecules (746 item numbers, January 2009 to November 2025) by the brands listed under each item, and computes the bounds on biosimilar share that item-level statistics can identify. It checks those bounds against item compositions recovered from archived PBS item pages and against the brand-level counts in the annual PBS Expenditure and Prescriptions Report. It also shows what a structural-break analysis of the conventional item-classified share detects.

## Note on this version (v2.0.0)

This release replaces the analysis in v1.0.1. That analysis classed each item number as reference or biosimilar and treated the resulting share as biosimilar uptake. Because many items list reference and biosimilar brands together, that share is not brand share, and the v1.0.1 results (interrupted time series and regression discontinuity estimates, counterfactual savings, state variation) should not be used. Its scripts and outputs remain in the git history and in Zenodo v1.0.1. `03_market_share.R` is kept because it builds the conventional item-classified series that this analysis evaluates.

## Directory structure

```
├── scripts/
│   ├── 01_item_mapping.R                # Item numbers, molecules, policy timeline
│   ├── 02_data_acquisition.R            # Process PBS Date of Supply and Medicare Statistics data
│   ├── 03_market_share.R                # Conventional item-classified share (the series evaluated)
│   ├── 11_item_class_decomposition.R    # Item brand composition and bounds (February 2026 Schedule)
│   ├── 12_archived_item_brands.R        # Brands under each item from archived PBS item pages
│   ├── 13_archived_bounds_validation.R  # Snapshot vs archived classes; time-varying bounds
│   ├── 14_break_demonstration.R         # Bai-Perron breaks on the conventional series vs item and policy events
│   ├── 15_tables_figures.R              # Main tables and figures
│   └── 16_brand_level_benchmark.R       # Brand-level counts from the Expenditure and Prescriptions Report
├── data/
│   ├── reference/                       # Item mapping, ATC lookup, policy timeline, sources for each date
│   └── processed/                       # Conventional item-classified series
└── outputs/
    ├── figures/                         # Figures 1 and 2 (PDF, PNG)
    └── tables/                          # All tables (CSV)
```

## Data sources

All input data are public:

- PBS and RPBS Date of Supply data, July 2021 to November 2025: Department of Health, Disability and Ageing, https://www.pbs.gov.au/statistics/dos-and-dop/dos-and-dop
- PBS Item Reports, January 2009 to June 2022: Services Australia, Medicare Statistics (https://medicarestatistics.humanservices.gov.au), also listed on data.gov.au (https://data.gov.au/data/dataset/14b536d4-eb6a-485d-bf87-2e6e77ddbac1)
- PBS API CSV files, Schedule of Pharmaceutical Benefits effective 1 February 2026 (schedule code 4604), listed on the PBS Downloads page (archived copy: https://web.archive.org/web/20260227102043/https://www.pbs.gov.au/browse/downloads)
- Archived PBS item pages (`www.pbs.gov.au/medicine/item/<item>`), Internet Archive Wayback Machine
- PBS Expenditure and Prescriptions Reports, 2023-24 (https://pbs.gov.au/statistics/expenditure-prescriptions/2023-2024/PBS-Expenditure-prescriptions-report-2023-24.PDF) and 2024-25 (https://www.pbs.gov.au/statistics/expenditure-prescriptions/2024-2025/Expenditure-prescriptions-report-tables-2024-25.PDF)

Raw files are not redistributed here. The source documents for each policy date are listed in `data/reference/sources/README.md`. The archived item pages are not included either; `outputs/tables/tbl_archived_fetch_log.csv` gives the item and capture timestamp of every page used, and `outputs/tables/tbl_archived_item_brands.csv` the brands parsed from each, so any capture can be retrieved again from `https://web.archive.org/web/<timestamp>id_/https://www.pbs.gov.au/medicine/item/<item>`.

## Reproducing the analysis

Requirements: R (tested on 4.6.1) with tidyverse, here, cli, glue, lubridate, readxl, rvest, httr, scales, patchwork and strucchange; `pdftotext` (poppler) for script 16.

1. Download the raw files above into `data/raw/` (`pbs_prescribing/`, `pbs_schedule/pbs-api-csv/` unzipped, `pbs_expenditure_prescriptions/`).
2. Run from the repository root in this order: 01, 02, 03, 11, 12, 13, 14, 16, 15.

Script 12 needs network access. It caches each archived page before parsing and can be rerun to resume. Scripts 13 to 15 read its outputs, so they can be rerun from the tables in `outputs/tables/` without refetching.

## Licence

Code: MIT. Source data remain subject to the terms of their publishers.
