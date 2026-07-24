# Space Fortress — Psychometrics & EF correlates

Repository containing preprocessing and analysis code for: "The Space Fortress task: Psychometrics Evidence and Relationship with
Executive Functions".

Key points
- **Data**: raw data are located in the `data/` folder.
- **Scripts**: Scripts are located under the `src/` folder. 
- **Results**: Results from the scripts are located under the `results/` folder. 
- **License**: MIT (see LICENSE.md).

Quick start
- Prerequisites: R (>= 4.0) and internet access for installing packages.
- From the project root, install packages and run preprocessing and analyses in R:

  1) Preprocessing (creates combined behavioral data):
     Rscript -e "source('src/preprocessing/behavior/_main.R')"

  2) Full analysis pipeline:
     Rscript -e "source('src/analyses/_main.r')"

Notes on scripts and outputs
- Preprocessing scripts: [src/preprocessing/behavior/_main.R](src/preprocessing/behavior/_main.R)
- Analysis scripts: [src/analyses/_main.r](src/analyses/_main.r) and individual analyses in [src/analyses/](src/analyses/)
- Figures and combined data are written to `results/figures/` and `results/combined_data/` respectively.
- Default random seed used in analyses: `123` (set inside CV scripts) to improve reproducibility of resampling results.

Dependencies & package management
- Most scripts call the helper `load_packages()` in [src/analyses/utils.R](src/analyses/utils.R) which will install missing CRAN packages automatically.
- Otherwise, install the core packages used: `dplyr`, `ggplot2`, `ggpubr`, `broom`, `lmtest`, `car`, `pwr`, `irr`.

Contact
- For data requests or questions, contact Quentin Chenot (quentinchenot@gmail.com).
