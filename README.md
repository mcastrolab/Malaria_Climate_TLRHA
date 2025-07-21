# TLRHA Case-Crossover Analysis (Arisco et al. 2025)

This repository contains RMarkdown scripts for reproducing the analyses and figures in:

> Arisco NJ, et al. *The Long-Run Health Impacts of Amazon Deforestation: A Case-Crossover Study* (2025)

## 📂 Contents

- `casecrossover-allweeks-tlrha-ariscoetal2025.Rmd`  
  RMarkdown for the case-crossover analysis across all weeks.

- `statestratified-casecrossover-allweeks-tlrha-ariscoetal2025.Rmd`  
  RMarkdown for stratified case-crossover analysis by Brazilian states.

- `statestratified-ONI-extrapolation-tlrha-ariscoetal2025.Rmd`  
  RMarkdown for ONI extrapolation and state-stratified analysis.

## 📋 Requirements

The scripts require:

- R >= 4.1
- R packages:
  - `mgcv`
  - `dlnm`
  - `gnm`
  - `data.table`
  - `ggplot2`
  - `splines`
  - `sf`
  - `tidyverse`
  - `forecast`
  - `lubridate`

Install all dependencies using:
```R
install.packages(c("mgcv", "dlnm", "gnm", "data.table", "ggplot2", "splines", "sf", "tidyverse", "forecast", "lubridate"))
```

## Usage

Data are not permitted to be shared. Code that can be used to reference the analytical procedures can be accessed via:

1. Clone the repository:
   ```bash
   git clone https://github.com/YOUR-USERNAME/tlrha-casecrossover.git
   cd tlrha-casecrossover
   ```

2. Open any of the `.Rmd` files in RStudio.

3. Knit the documents to HTML or PDF.

## 📜 License

This project is licensed under the MIT License. See [LICENSE](LICENSE) for details.

## 👨‍💻 Authors

- Nicholas J. Arisco
- Collaborators: Cassio Peterka, Joel Schwartz, Marcia C. Castro