# Case-Study Input Data

This folder contains the scenario-specific Excel input files used in the ES-MADM III case study.

## Files

```text
ES_MADM_III_Input_CaseStudy_S1_Baseline.xlsx
ES_MADM_III_Input_CaseStudy_S2_HighUncertainty.xlsx
ES_MADM_III_Input_CaseStudy_S3_ReducedSeparability.xlsx
ES_MADM_III_Input_CaseStudy_S4A_Conflicting_PerformanceFocus.xlsx
ES_MADM_III_Input_CaseStudy_S4B_Conflicting_CostResilienceFocus.xlsx
```

---

## Purpose

These files provide the structured input data required by the ES-MADM III Decision Studio. Each workbook corresponds to one scenario of the manuscript case study and contains the necessary information for executing the ES-MADM III computational workflow.

The input files support the reproduction of the scenario-specific results, multi-scenario comparison, diagnostic interpretation, and benchmark analysis reported in the manuscript.

---

## Scenario Description

| Scenario | File | Description |
|---|---|---|
| S1 | `ES_MADM_III_Input_CaseStudy_S1_Baseline.xlsx` | Baseline metropolitan mobility evaluation |
| S2 | `ES_MADM_III_Input_CaseStudy_S2_HighUncertainty.xlsx` | Baseline central matrix with increased fuzzy uncertainty |
| S3 | `ES_MADM_III_Input_CaseStudy_S3_ReducedSeparability.xlsx` | Reduced-separability stress-test scenario |
| S4A | `ES_MADM_III_Input_CaseStudy_S4A_Conflicting_PerformanceFocus.xlsx` | Conflicting scenario with performance-focused subjective weights |
| S4B | `ES_MADM_III_Input_CaseStudy_S4B_Conflicting_CostResilienceFocus.xlsx` | Conflicting scenario with cost/resilience-focused subjective weights |

---

## Input Data Structure

Each workbook is organized to provide the main computational inputs required by the ES-MADM III application, including:

- alternatives;
- evaluation criteria;
- criterion type, defined as benefit or cost;
- central performance values;
- fuzzy uncertainty spreads;
- subjective criterion weights;
- preference-function parameters;
- scenario-specific configuration data.

These inputs allow the application to construct the fuzzy decision matrix, normalize the performance values, apply PROMETHEE-type preference conditioning, derive entropy-based information measures, and compute the final decision outputs.

---

## Recommended Use

The recommended workflow is:

1. Open the ES-MADM III Decision Studio from the `app/` folder.
2. Import `ES_MADM_III_Input_CaseStudy_S1_Baseline.xlsx` through the main data import interface.
3. Import the remaining scenario files through the scenario portfolio workflow.
4. Execute the ES-MADM III computations.
5. Export the consolidated results workbook.
6. Compare the exported results with the reference output workbook stored in the `results/` folder.

---

## Scenario Design Logic

The five scenarios were designed to test different aspects of the ES-MADM III framework:

- **S1** provides the baseline decision structure.
- **S2** evaluates the effect of uncertainty inflation while preserving the baseline central matrix.
- **S3** examines weak-decision behavior under reduced alternative separability.
- **S4A** evaluates a conflicting performance-focused preference regime.
- **S4B** evaluates a conflicting cost/resilience-focused preference regime.

Together, these scenarios demonstrate how the model responds to uncertainty, near-tie conditions, preference conflict, rank reversal, and scenario-dependent diagnostic behavior.

---

## Notes

The files in this folder correspond to the final input data used for the manuscript. Earlier input versions and development-stage test files are intentionally excluded.

The workbooks should not be modified unless the user intends to create a new computational experiment. Any modification of the input files may change the resulting weights, scores, rankings, entropy measures, diagnostic indices, and benchmark comparison outputs.
