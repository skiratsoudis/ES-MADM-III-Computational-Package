# ES-MADM III Computational Package

This repository provides the final computational package supporting the ES-MADM III framework, a preference-conditioned fuzzy entropy–synergy model for multi-attribute decision diagnostics and robust ranking under uncertainty.

The repository accompanies the manuscript:

**ES-MADM III: A Preference-Conditioned Fuzzy Entropy–Synergy Framework for Multi-Attribute Decision Diagnostics and Robust Ranking**

The package includes the final R/Shiny implementation, five scenario-specific input workbooks, consolidated computational outputs, and a manuscript figure-generation script. The repository is designed to support transparent reproduction of the case-study results, benchmark comparison, and graphical outputs reported in the manuscript.

---

## Repository Structure

```text
ES-MADM-III-Computational-Package/
│
├── app/
│   └── ES_MADM_III_Decision_Studio_Final.R
│
├── data/
│   ├── ES_MADM_III_Input_CaseStudy_S1_Baseline.xlsx
│   ├── ES_MADM_III_Input_CaseStudy_S2_HighUncertainty.xlsx
│   ├── ES_MADM_III_Input_CaseStudy_S3_ReducedSeparability.xlsx
│   ├── ES_MADM_III_Input_CaseStudy_S4A_Conflicting_PerformanceFocus.xlsx
│   └── ES_MADM_III_Input_CaseStudy_S4B_Conflicting_CostResilienceFocus.xlsx
│
├── results/
│   └── ES_MADM_III_Results_Computational_Outputs.xlsx
│
├── scripts/
│   └── ES_MADM_III_Figures_Manuscript.R
│
├── LICENSE
├── .gitignore
└── README.md
Overview of the Computational Framework

ES-MADM III is designed for decision problems in which alternatives must be evaluated under uncertainty, heterogeneous preferences, and scenario-dependent decision conditions. The computational workflow combines fuzzy performance representation, preference-conditioned comparison, entropy-based information processing, operational weighting, final scoring, and diagnostic interpretation.

At a high level, the model performs the following operations:

Defines the alternatives, criteria, and benefit/cost orientation.
Represents performance values and subjective weights through fuzzy uncertainty intervals.
Normalizes the data while preserving criterion orientation.
Applies PROMETHEE-type preference functions to pairwise performance gaps.
Constructs preference-enhanced conditional probability structures.
Evaluates criterion-level entropy and operational diversification.
Derives information-sensitive representative objective and integrated weights.
Computes final alternative scores and rankings.
Computes diagnostic indices including NMI, CES, ADI, and scenario-relative NMGI.
Benchmarks the results against fuzzy PROMETHEE II.

The repository focuses on the computational implementation and reproducibility of the final manuscript results. It does not include earlier development versions of the model.

Case Study Scenarios

The computational package includes five scenario-specific input files:

Scenario	Description
S1	Baseline metropolitan mobility evaluation
S2	High-uncertainty version of the baseline case
S3	Reduced-separability / weak-decision stress test
S4A	Conflicting performance-focused preference regime
S4B	Conflicting cost/resilience-focused preference regime

These scenarios are used to examine how the model behaves under stable baseline conditions, uncertainty inflation, near-tie structures, and conflicting preference postures.

Main Files
R/Shiny Application

The main computational application is located in:

app/ES_MADM_III_Decision_Studio_Final.R

This file implements the ES-MADM III Decision Studio, including data import, scenario processing, entropy-based computations, operational weighting, final scoring, diagnostic outputs, benchmark support, and Excel export functionality.

Input Workbooks

The scenario-specific Excel input files are located in:

data/

Each workbook corresponds to one case-study scenario and contains the structured input data required by the application.

Computational Outputs

The consolidated output workbook is located in:

results/ES_MADM_III_Results_Computational_Outputs.xlsx

This file contains the final computational outputs used to support the manuscript tables and analysis.

Figure-Generation Script

The manuscript figure-generation script is located in:

scripts/ES_MADM_III_Figures_Manuscript.R

This script reproduces the manuscript figures and benchmark visualizations from embedded final data and computational results.

Required R Packages

The R/Shiny application and figure-generation script require the following R packages:

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

Install missing packages in R using:

install.packages(c(
  "shiny", "shinythemes", "readxl", "writexl", "ggplot2",
  "DT", "dplyr", "tidyr", "scales", "ggrepel", "tibble"
))
How to Run the ES-MADM III Application
Open RStudio.
Open the file:
app/ES_MADM_III_Decision_Studio_Final.R
Run the application.
Import the baseline workbook from the data/ folder.
Import the remaining scenario workbooks through the scenario portfolio workflow.
Export the resulting workbook and compare it with the reference output in results/.

The application is designed to support scenario-based evaluation and export the relevant weights, scores, rankings, entropy measures, diagnostic indices, and benchmark-related outputs.

How to Reproduce the Manuscript Figures
Open RStudio.
Open the file:
scripts/ES_MADM_III_Figures_Manuscript.R
Run the script.
The figures are produced in the RStudio Plots pane.
Individual figures can be reviewed, resized, exported, or copied into the manuscript as needed.

The script is configured to avoid automatic uncontrolled figure export and to support manual adjustment of plot size for manuscript preparation.

Reproducibility Notes

The files in this repository correspond to the final computational version used for the manuscript. Earlier development versions, draft model documents, obsolete outputs, and superseded scripts are intentionally excluded to avoid ambiguity.

The package is intended to allow readers and reviewers to:

inspect the input data used in each scenario;
run the final ES-MADM III computational implementation;
compare exported results with the reference output workbook;
reproduce the manuscript figures;
verify the benchmark comparison against fuzzy PROMETHEE II;
evaluate the consistency between computational results and reported manuscript tables.
Citation

If you use this repository, please cite the associated manuscript and the archived Zenodo record once available.

A formal CITATION.cff file will be included before final Zenodo archiving.

License

This repository is distributed under the MIT License. See the LICENSE file for details.

Contact

For questions related to the computational package, please contact:

Sideris Kiratsoudis
Department of Physics, Faculty of Sciences
Democritus University of Thrace
Kavala, Greece


Commit message:

```text
Update main README
