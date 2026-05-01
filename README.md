ES-MADM III Computational Package

This repository provides the final computational package supporting the ES-MADM III framework, a preference-conditioned fuzzy entropy–synergy model for multi-attribute decision diagnostics and robust ranking under uncertainty.

The repository accompanies the manuscript:

ES-MADM III: A Preference-Conditioned Fuzzy Entropy–Synergy Framework for Multi-Attribute Decision Diagnostics and Robust Ranking

The package includes the final R/Shiny implementation, five scenario-specific input workbooks, consolidated computational outputs, and a manuscript figure-generation script. It is designed to support transparent reproduction of the case-study results, benchmark comparison, and graphical outputs reported in the manuscript.

Repository structure

ES-MADM-III-Computational-Package/

├── app/

│ └── ES_MADM_III_Decision_Studio_Final.R

├── data/

│ ├── ES_MADM_III_Input_CaseStudy_S1_Baseline.xlsx

│ ├── ES_MADM_III_Input_CaseStudy_S2_HighUncertainty.xlsx

│ ├── ES_MADM_III_Input_CaseStudy_S3_ReducedSeparability.xlsx

│ ├── ES_MADM_III_Input_CaseStudy_S4A_Conflicting_PerformanceFocus.xlsx

│ └── ES_MADM_III_Input_CaseStudy_S4B_Conflicting_CostResilienceFocus.xlsx

├── results/

│ └── ES_MADM_III_Results_Computational_Outputs.xlsx

├── scripts/

│ └── ES_MADM_III_Figures_Manuscript.R

├── LICENSE

├── .gitignore

└── README.md

Overview of the computational framework

ES-MADM III is designed for decision problems in which alternatives are evaluated under fuzzy uncertainty, heterogeneous preferences, and scenario-dependent decision conditions. The computational workflow combines fuzzy data representation, preference-conditioned comparison, entropy-based information processing, operational weighting, final scoring, and diagnostic interpretation.

At a high level, the model performs the following operations:

Defines the alternatives, criteria, and benefit/cost orientation.
Represents performance values and subjective weights under fuzzy uncertainty.
Normalizes the data while preserving criterion orientation.
Applies PROMETHEE-type preference functions to pairwise performance gaps.
Constructs preference-enhanced conditional probability structures.
Evaluates criterion-level entropy and operational diversification.
Derives information-sensitive representative objective and integrated weights.
Computes final alternative scores and rankings.
Computes diagnostic indices, including NMI, CES, ADI, and scenario-relative NMGI.
Supports benchmark comparison against fuzzy PROMETHEE II.

The repository focuses on the computational implementation and reproducibility of the final manuscript results. Earlier development versions, obsolete outputs, and draft model files are intentionally excluded to avoid ambiguity.

Case-study scenarios

The computational package includes five scenario-specific input files:

Scenario	Description
S1	Baseline metropolitan mobility evaluation
S2	High-uncertainty version of the baseline case
S3	Reduced-separability / weak-decision stress-test scenario
S4A	Conflicting performance-focused preference regime
S4B	Conflicting cost/resilience-focused preference regime

These scenarios examine how the ES-MADM III framework behaves under baseline conditions, uncertainty inflation, reduced alternative separability, and conflicting preference postures.

Main files
R/Shiny application

The main computational application is located in:

app/ES_MADM_III_Decision_Studio_Final.R

This file implements the ES-MADM III Decision Studio, including data import, scenario processing, entropy-based computations, operational weighting, final scoring, diagnostic outputs, benchmark support, and Excel export functionality.

Input workbooks

The scenario-specific Excel input files are located in:

data/

Each workbook corresponds to one case-study scenario and contains the structured input data required by the application.

Computational outputs

The consolidated output workbook is located in:

results/ES_MADM_III_Results_Computational_Outputs.xlsx

This file contains the final computational outputs used to support the manuscript tables, results interpretation, and benchmark analysis.

Figure-generation script

The manuscript figure-generation script is located in:

scripts/ES_MADM_III_Figures_Manuscript.R

This script reproduces the manuscript figures and benchmark visualizations from the final computational results.

Required R packages

The R/Shiny application and the figure-generation script require the following R packages:

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

Missing packages can be installed in R using:

install.packages(c("shiny", "shinythemes", "readxl", "writexl", "ggplot2", "DT", "dplyr", "tidyr", "scales", "ggrepel", "tibble"))

How to run the ES-MADM III application
Open RStudio.
Open the file app/ES_MADM_III_Decision_Studio_Final.R.
Run the Shiny application.
Import the S1 baseline workbook from the data/ folder.
Import the remaining scenario workbooks through the scenario portfolio workflow.
Review the scenario-level outputs.
Export the consolidated results workbook.
Compare the exported workbook with the reference output in results/.

The application supports scenario-based evaluation and exports the relevant weights, scores, rankings, entropy measures, diagnostic indices, and benchmark-related outputs.

How to reproduce the manuscript figures
Open RStudio.
Open the file scripts/ES_MADM_III_Figures_Manuscript.R.
Run the script.
Review the generated figures in the RStudio Plots pane.
Export or copy individual figures into the manuscript as needed.

The script is configured to generate the figures directly in the RStudio Plots pane, allowing the user to adjust the plot size before exporting or copying the figure into a document.

Reproducibility notes

The files in this repository correspond to the final computational version used for the manuscript. Earlier development versions, obsolete scripts, draft model documents, and superseded output files are intentionally excluded.

The package allows readers and reviewers to:

inspect the scenario-specific input data;
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

Sideris Kiratsoudis
Department of Physics, Faculty of Sciences
Democritus University of Thrace
Kavala, Greece
