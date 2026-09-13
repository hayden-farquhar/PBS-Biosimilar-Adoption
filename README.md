# Reference Product Delisting Drives Biosimilar Adoption in Australia

Replication code and data for:

> Farquhar H. Reference Product Delisting Drives Biosimilar Adoption in Australia: Interrupted Time Series Evidence and a Documented Reversal in Adalimumab Uptake. Manuscript under consideration at a peer-reviewed journal.

## Overview

This repository contains all analysis code and processed data to replicate the findings of the study. The study applies interrupted time series (ITS) segmented regression, regression discontinuity in time (RDiT), Bayesian structural time series (CausalImpact), and Bai-Perron structural break detection to PBS prescribing data for 10 biologic molecules.

## Note on this version

This release supersedes the initial commit and corrects two defects found while revising the manuscript. Anyone who cloned the earlier state should re-run from this version.

**A double-counting error in the national series.** `03_market_share.R` aggregated national prescription counts without restricting to national-level records. The two sources are shaped differently: the PBS Date of Supply extract carries only a national row, whereas Medicare Statistics carries a national row *and* eight jurisdiction rows that sum to the same total. Every Medicare-era month (January 2009 to June 2022) was therefore counted twice, while Date of Supply months were counted once, producing an artefactual halving of volumes at July 2022. The fix adds `filter(state == "National")` before the national aggregation; the state-level block was already correct. 1,081 of 1,527 molecule-months changed.

**Unverifiable international benchmarks.** The international comparison in `04_descriptive_analysis.R` carried adalimumab uptake figures attributed to sources that do not contain them, including an "OECD average" that is in fact a tumour-necrosis-factor-inhibitor class figure measured in treatment days. Benchmarks are now restricted to the two exact, adalimumab-specific, retail-setting values published in Tam et al. (BioDrugs 2025;39(3):461-476). Rows that could not be verified are commented out with the reason recorded, not deleted.

Downstream of these fixes, structural break detection showed that neither adalimumab policy date falls within the confidence interval of any detected break, so the adalimumab intervention effects are not identified and are reported descriptively in the manuscript. `09_figure2_observed.R` is new and produces the main-text Figures 1 and 2 as observed series without fitted regressions.

## Directory Structure

```
├── scripts/
│   ├── 01_item_mapping.R           # Map PBS item numbers to biosimilar/reference biologic
│   ├── 02_data_acquisition.R       # Process PBS Date of Supply and Medicare Statistics data
│   ├── 03_market_share.R           # Calculate biosimilar market share by molecule x time
│   ├── 04_descriptive_analysis.R   # Adoption curves, velocity metrics, international benchmarking
│   ├── 05_its_analysis.R           # Interrupted time series: segmented regression + CausalImpact
│   ├── 06_rd_analysis.R            # Regression discontinuity at policy implementation dates
│   ├── 07_geographic_variation.R   # State-level variation, multilevel models, funnel plots
│   └── 08_counterfactual.R         # Counterfactual PBS expenditure under OECD-average uptake
├── data/
│   ├── reference/                  # Policy timeline, item number mappings, ATC codes
│   └── processed/                  # Biosimilar market share time series (analysis-ready)
└── outputs/
    ├── figures/                    # All study figures (PNG)
    └── tables/                     # All results tables (CSV)
```

## Data Sources

All data used in this study are publicly available:

- **PBS Date of Supply data** (July 2021 -- November 2025): Australian Department of Health and Aged Care, [pbs.gov.au/info/statistics/dos-and-dop](https://www.pbs.gov.au/info/statistics/dos-and-dop)
- **Medicare Statistics** (January 2009 -- June 2022): Services Australia, [medicarestatistics.humanservices.gov.au](https://medicarestatistics.humanservices.gov.au)
- **PBS Schedule**: [pbs.gov.au](https://www.pbs.gov.au)
- **OECD Health Statistics**: [stats.oecd.org](https://stats.oecd.org)

Raw data files are not included in this repository due to size and redistribution considerations. Scripts `01_item_mapping.R` and `02_data_acquisition.R` document the acquisition and processing pipeline. The `data/processed/` directory contains the analysis-ready market share time series derived from these sources.

## Reproducing the Analysis

Scripts are numbered sequentially. Scripts 01--03 process raw data into the analysis-ready datasets provided in `data/processed/`. Scripts 04--08 produce all results, figures, and tables reported in the manuscript.

To reproduce from the processed data:

```r
# Install required packages
install.packages(c(
  "tidyverse", "here", "lubridate", "sandwich", "lmtest", "CausalImpact",
  "rdrobust", "lme4", "sf", "ozmaps", "tmap", "scales", "patchwork",
  "knitr", "kableExtra", "cli"
))

# Run analysis scripts (from project root)
source("scripts/04_descriptive_analysis.R")
source("scripts/05_its_analysis.R")
source("scripts/06_rd_analysis.R")
source("scripts/07_geographic_variation.R")
source("scripts/08_counterfactual.R")
```

## Requirements

- R >= 4.3.0
- Key packages: `tidyverse`, `sandwich`, `lmtest`, `CausalImpact`, `rdrobust`, `lme4`, `sf`, `ozmaps`, `tmap`, `patchwork`

See individual script headers for full dependency lists.

## Key Reference Data

- `data/reference/policy_timeline.csv` -- 56 policy intervention records (listing dates, delistings, price cuts, authority changes) across 10 molecules
- `data/reference/pbs_item_mapping.csv` -- 746 PBS item numbers classified as biosimilar or reference biologic
- `data/reference/atc_molecule_lookup.csv` -- ATC code to molecule name mapping

## Author

Hayden Farquhar MBBS MPHTM
Independent Researcher
ORCID: [0009-0002-6226-440X](https://orcid.org/0009-0002-6226-440X)

## License

This code is provided for academic replication purposes. Data derived from publicly available Australian Government sources.
