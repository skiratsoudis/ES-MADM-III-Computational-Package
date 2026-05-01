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
