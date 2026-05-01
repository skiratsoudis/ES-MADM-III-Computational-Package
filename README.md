# ES-MADM III Computational Package

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.19945796.svg)](https://doi.org/10.5281/zenodo.19945796)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R](https://img.shields.io/badge/Language-R-blue.svg)](https://www.r-project.org/)

This repository provides the final computational package supporting the ES-MADM III framework, a preference-conditioned fuzzy entropy–synergy model for multi-attribute decision diagnostics and robust ranking under uncertainty.

The repository accompanies the manuscript:

**ES-MADM III: A Preference-Conditioned Fuzzy Entropy–Synergy Framework for Multi-Attribute Decision Diagnostics and Robust Ranking**

The package includes the final R/Shiny implementation, five scenario-specific input workbooks, consolidated computational outputs, and a manuscript figure-generation script. It is designed to support transparent reproduction of the case-study results, benchmark comparison, diagnostic outputs, and graphical results reported in the manuscript.

The archived release is available on Zenodo:

**DOI:** [10.5281/zenodo.19945796](https://doi.org/10.5281/zenodo.19945796)

---

## Repository Structure

```text
ES-MADM-III-Computational-Package/
├── app/
│   ├── ES_MADM_III_Decision_Studio_Final.R
│   └── README.md
├── data/
│   ├── ES_MADM_III_Input_CaseStudy_S1_Baseline.xlsx
│   ├── ES_MADM_III_Input_CaseStudy_S2_HighUncertainty.xlsx
│   ├── ES_MADM_III_Input_CaseStudy_S3_ReducedSeparability.xlsx
│   ├── ES_MADM_III_Input_CaseStudy_S4A_Conflicting_PerformanceFocus.xlsx
│   ├── ES_MADM_III_Input_CaseStudy_S4B_Conflicting_CostResilienceFocus.xlsx
│   └── README.md
├── docs/
│   └── Model_Workflow_Overview.md
├── results/
│   ├── ES_MADM_III_Results_Computational_Outputs.xlsx
│   └── README.md
├── scripts/
│   ├── ES_MADM_III_Figures_Manuscript.R
│   └── README.md
├── CITATION.cff
├── LICENSE
├── .gitignore
└── README.md
```

---

## Overview

ES-MADM III is a computational framework for multi-attribute decision-making problems in which alternatives are evaluated under fuzzy uncertainty, heterogeneous preference structures, and scenario-dependent decision conditions.

The model integrates fuzzy data representation, PROMETHEE-type preference conditioning, entropy-based information processing, operational representative weighting, final alternative scoring, and diagnostic interpretation within a single reproducible workflow.

The repository focuses exclusively on the final computational version used for the manuscript. Earlier development versions, obsolete scripts, draft model documents, superseded outputs, and internal working materials are intentionally excluded.

---

## Computational Workflow

At a high level, the ES-MADM III computational workflow performs the following operations:

1. Defines the alternatives, criteria, and benefit/cost orientation.
2. Represents performance values and subjective criterion weights under fuzzy uncertainty.
3. Normalizes the data while preserving the orientation of benefit and cost criteria.
4. Applies PROMETHEE-type preference functions to pairwise performance gaps.
5. Constructs preference-enhanced conditional probability structures.
6. Evaluates criterion-level entropy and operational diversification.
7. Derives information-sensitive representative objective and integrated weights.
8. Computes final alternative scores and rankings.
9. Computes diagnostic indices, including NMI, CES, ADI, and scenario-relative NMGI.
10. Supports benchmark comparison against fuzzy PROMETHEE II.

This structure allows the analyst to evaluate not only the final ranking of alternatives, but also the informational strength, criterion effectiveness, alternative distinction, and scenario-relative diagnostic behavior of the decision process.

---

## Case-Study Scenarios

The repository contains five scenario-specific input workbooks.

| Scenario | Description |
|---|---|
| S1 | Baseline metropolitan mobility evaluation |
| S2 | High-uncertainty version of the baseline case |
| S3 | Reduced-separability / weak-decision stress-test scenario |
| S4A | Conflicting performance-focused preference regime |
| S4B | Conflicting cost/resilience-focused preference regime |

These scenarios examine the behavior of ES-MADM III under baseline conditions, uncertainty inflation, reduced alternative separability, and conflicting preference postures.

---

## Main Files

### R/Shiny Application

The main computational application is located in:

```text
app/ES_MADM_III_Decision_Studio_Final.R
```

This file implements the ES-MADM III Decision Studio, including data import, scenario processing, entropy-based computations, operational weighting, final scoring, diagnostic outputs, benchmark support, and Excel export functionality.

### Input Workbooks

The scenario-specific Excel input files are located in:

```text
data/
```

Each workbook corresponds to one case-study scenario and contains the structured input data required by the application.

### Computational Outputs

The consolidated output workbook is located in:

```text
results/ES_MADM_III_Results_Computational_Outputs.xlsx
```

This file contains the final computational outputs used to support the manuscript tables, results interpretation, diagnostic analysis, and benchmark comparison.

### Figure-Generation Script

The manuscript figure-generation script is located in:

```text
scripts/ES_MADM_III_Figures_Manuscript.R
```

This script reproduces the manuscript figures and benchmark visualizations from the final computational results.

### Model Workflow Overview

A non-mathematical overview of the ES-MADM III computational process is provided in:

```text
docs/Model_Workflow_Overview.md
```

This document describes the main computational stages and interpretation logic of the model without reproducing the formal mathematical formulation of the manuscript.

---

## Required R Packages

The R/Shiny application and the figure-generation script require the following R packages:

```r
shiny
shinythemes
readxl
writexl
ggplot2
DT
dplyr
tidyr
scales
ggrepel
tibble
grid
```

Missing packages can be installed in R using:

```r
install.packages(c(
  "shiny",
  "shinythemes",
  "readxl",
  "writexl",
  "ggplot2",
  "DT",
  "dplyr",
  "tidyr",
  "scales",
  "ggrepel",
  "tibble"
))
```

---

## How to Run the ES-MADM III Application

1. Open RStudio.
2. Open the file:

```text
app/ES_MADM_III_Decision_Studio_Final.R
```

3. Run the Shiny application.
4. Import the S1 baseline workbook from the `data/` folder.
5. Import the remaining scenario workbooks through the scenario portfolio workflow.
6. Review the scenario-level outputs.
7. Export the consolidated results workbook.
8. Compare the exported workbook with the reference output in the `results/` folder.

The application supports scenario-based evaluation and exports the relevant weights, scores, rankings, entropy measures, diagnostic indices, and benchmark-related outputs.

---

## How to Reproduce the Manuscript Figures

1. Open RStudio.
2. Open the file:

```text
scripts/ES_MADM_III_Figures_Manuscript.R
```

3. Run the script.
4. Review the generated figures in the RStudio Plots pane.
5. Export or copy individual figures into the manuscript as needed.

The script is configured to generate the figures directly in the RStudio Plots pane, allowing the user to adjust the plot size before exporting or copying figures into a document.

---

## Reproducibility Notes

The files in this repository correspond to the final computational version used for the manuscript.

The package allows readers and reviewers to:

- inspect the scenario-specific input data;
- run the final ES-MADM III computational implementation;
- compare exported results with the reference output workbook;
- reproduce the manuscript figures;
- verify the benchmark comparison against fuzzy PROMETHEE II;
- evaluate the consistency between computational results and reported manuscript tables.

Earlier development versions, obsolete scripts, draft model documents, superseded output files, and internal working materials are intentionally excluded to avoid ambiguity.

---

## Citation

If you use this repository, please cite the archived Zenodo record:

**Kiratsoudis, S., Tsiantos, V., & Spyropoulos, A. Z. (2026). ES-MADM III: Final Computational Package (v1.0.0). Zenodo. https://doi.org/10.5281/zenodo.19945796**

The repository also includes a `CITATION.cff` file for software citation metadata.

---

## License

This repository is distributed under the MIT License. See the `LICENSE` file for details.

---

## Contact

**Sideris Kiratsoudis**  
Department of Physics, Faculty of Sciences  
Democritus University of Thrace  
Kavala, Greece
