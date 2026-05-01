# ES-MADM III Model Workflow Overview

This document provides a non-mathematical overview of the ES-MADM III computational workflow. It is intended to help users, reviewers, and readers understand how the model operates at an implementation level without reproducing the full mathematical formulation presented in the associated manuscript.

## Purpose of the Model

ES-MADM III is a preference-conditioned fuzzy entropy–synergy framework for multi-attribute decision diagnostics and robust ranking under uncertainty.

The model is designed for decision problems in which alternatives must be evaluated across multiple criteria while accounting for:

- uncertainty in performance data;
- uncertainty in subjective criterion weights;
- benefit and cost criterion orientations;
- preference-sensitive comparison between alternatives;
- information-based criterion importance;
- final ranking behavior;
- diagnostic strength of the decision outcome;
- scenario-dependent changes in preferences or data structure.

Unlike conventional ranking-oriented decision models, ES-MADM III does not only identify the best alternative. It also evaluates how strongly the ranking is supported by the available information, how effectively the criteria discriminate among alternatives, and how clearly the final alternatives are separated.

---

## General Computational Logic

The ES-MADM III workflow transforms uncertain input information into interpretable decision-support outputs through a sequence of structured computational stages.

At a high level, the model performs the following operations:

1. It defines the decision problem.
2. It represents uncertain performance data and subjective weights.
3. It normalizes the data across benefit and cost criteria.
4. It applies preference functions to compare alternatives.
5. It constructs preference-enhanced conditional probability structures.
6. It evaluates the informational content of each criterion.
7. It derives information-sensitive representative weights.
8. It computes final alternative scores and rankings.
9. It evaluates diagnostic indices.
10. It compares scenario behavior through a scenario-level synthesis.

Each stage produces structured intermediate quantities that are used by the next stage. This makes the workflow transparent, auditable, and computationally reproducible.

---

## Step 1: Problem Definition

The model begins by defining the basic decision environment.

This includes:

- the set of alternatives;
- the set of evaluation criteria;
- the orientation of each criterion as benefit or cost;
- the central performance values of each alternative under each criterion;
- the uncertainty spread around each performance value;
- the subjective importance assigned to each criterion;
- the uncertainty associated with subjective weights.

This stage establishes the full input structure of the decision problem.

---

## Step 2: Fuzzy Data Representation

ES-MADM III represents performance values and subjective weights under uncertainty. Instead of treating every input as a single fixed number, the model allows each value to vary within a confidence-level interval.

This allows the model to represent uncertainty in a controlled and transparent way. A lower confidence level corresponds to wider uncertainty intervals, while a higher confidence level corresponds to narrower intervals closer to the central values.

In the manuscript case study, all computations are performed at a selected confidence level, ensuring that the entire workflow is evaluated consistently under the same uncertainty setting.

---

## Step 3: Normalization

The model then normalizes the performance data so that all criteria become comparable.

This is necessary because criteria may have different units, scales, or orientations. Some criteria are benefit-oriented, meaning that larger values are preferred. Others are cost-oriented, meaning that smaller values are preferred.

The normalization stage converts all criteria into a common higher-is-better scale while preserving the uncertainty intervals. This ensures that all subsequent computations are performed on comparable and dimensionless values.

---

## Step 4: Preference Conditioning

After normalization, ES-MADM III applies PROMETHEE-type preference functions to compare alternatives under each criterion.

This stage converts pairwise performance differences into preference intensities. In practical terms, the model evaluates how strongly one alternative is preferred over another under each criterion.

The preference function allows the analyst to distinguish between negligible differences, moderate differences, and strong preference differences. In the manuscript case study, a linear PROMETHEE-type preference function is used consistently across all criteria and scenarios.

---

## Step 5: Preference-Enhanced Conditional Probabilities

The preference information is then combined with the normalized performance data to construct preference-enhanced conditional probability structures.

These structures describe how the alternatives are distributed under each criterion after both performance and preference information have been considered.

This stage is important because it moves the model from raw performance comparison to an information-theoretic representation of the decision system. The alternatives are no longer evaluated only by their normalized scores, but also by their preference-supported position under each criterion.

---

## Step 6: Criterion Informativeness

The model then evaluates how informative each criterion is.

A criterion is considered highly informative when it produces a concentrated and discriminative distribution among the alternatives. A criterion is less informative when it produces a diffuse or nearly uniform distribution, meaning that it does not clearly separate the alternatives.

This stage allows ES-MADM III to distinguish between criteria that truly help the decision process and criteria that carry limited discriminatory information.

---

## Step 7: Feasible Operational Representatives

Because earlier stages may produce interval-valued structures, ES-MADM III constructs feasible operational representatives for final computation.

These representatives are selected so that they remain consistent with the admissible interval bounds and preserve the probabilistic structure required by the model.

This stage is central to the final version of ES-MADM III. It separates interval-level uncertainty auditing from the operational quantities used for scoring, weighting, and diagnostics.

---

## Step 8: Information-Sensitive Representative Weights

The model derives representative objective weights from the operational diversification capacity of each criterion.

These objective weights are then combined with representative subjective weights to form information-sensitive representative integrated weights.

This means that the final criterion weights are not determined only by subjective judgment. They also reflect the realized discriminatory contribution of each criterion. As a result, even when subjective weights are similar or equal, the final integrated weights may differ if the criteria carry different levels of useful information.

---

## Step 9: Final Alternative Scores and Rankings

The representative integrated weights and feasible conditional probability representatives are combined to compute final alternative scores.

These scores form a coherent distribution over the alternatives and are used to derive the final ranking. Alternatives with higher final scores receive higher ranks.

The ranking is therefore based on the interaction between:

- normalized performance;
- preference-conditioned comparison;
- criterion-level information content;
- subjective preferences;
- representative integrated weights.

---

## Step 10: Integrated Criteria Importance

ES-MADM III also computes a criterion-level contribution measure called Integrated Criteria Importance.

This measure reflects both:

- the final representative importance assigned to a criterion;
- the criterion’s operational capacity to discriminate among alternatives.

A criterion receives high importance only when it is both influential in the final weighting structure and genuinely useful for separating alternatives.

This provides a more informative interpretation than weights alone.

---

## Step 11: System-Level Entropy and Information Measures

The model then evaluates the overall uncertainty and information structure of the decision system.

This includes:

- uncertainty in the final alternative-score distribution;
- uncertainty in the final criterion-weight structure;
- residual uncertainty after conditioning on the criteria;
- joint uncertainty of the criterion–alternative system;
- information transferred from criteria to alternatives.

These quantities provide the foundation for the diagnostic layer of ES-MADM III.

---

## Step 12: Diagnostic Indices

ES-MADM III computes a compact non-redundant diagnostic set.

The retained diagnostic indices are:

- **Normalized Mutual Information (NMI)**: evaluates the explanatory strength of the criterion system with respect to the alternatives.
- **Criteria Effectiveness Score (CES)**: evaluates the integrated effectiveness of the criterion structure in reducing uncertainty.
- **Alternatives Distinction Index (ADI)**: evaluates the final separability of the alternative-score distribution.
- **Scenario-relative Net Mutual Growth Index (NMGI)**: synthesizes NMI, CES, and ADI across an explicit scenario family.

These diagnostics allow the analyst to distinguish between a ranking that merely exists and a ranking that is strongly supported by information.

---

## Scenario-Based Interpretation

The model is especially useful in scenario-based decision analysis.

In the manuscript case study, five scenarios are used:

- S1: baseline configuration;
- S2: high-uncertainty configuration;
- S3: reduced-separability configuration;
- S4A: performance-focused conflict regime;
- S4B: cost/resilience-focused conflict regime.

The scenario design allows the analyst to observe how rankings, weights, entropy measures, and diagnostic indices change when uncertainty, performance separability, or subjective preference structures are modified.

This makes ES-MADM III suitable for decision environments where the analyst must evaluate not only the preferred alternative, but also the reliability, clarity, and scenario sensitivity of the decision result.

---

## Benchmark Comparison

The computational package also supports comparison against fuzzy PROMETHEE II.

The benchmark is used to evaluate whether ES-MADM III produces rankings that are consistent with conventional fuzzy outranking logic under baseline conditions, and to identify cases where the entropy–synergy diagnostic structure leads to a richer interpretation.

The benchmark comparison focuses on:

- rank agreement;
- rank divergence;
- rank-transition behavior;
- correlation between the two methods;
- interpretation of complete ties or weak-decision regimes.

The benchmark is not intended to make the two methods numerically identical. ES-MADM III produces probability-consistent alternative scores, whereas fuzzy PROMETHEE II produces representative net flows.

---

## Reproducibility Logic

The repository provides all main computational elements required to reproduce the manuscript results:

- the final R/Shiny implementation;
- the five scenario input workbooks;
- the consolidated output workbook;
- the manuscript figure-generation script.

A user can reproduce the results by running the application, importing the scenario input files, exporting the results, and comparing them with the reference output workbook.

The figure-generation script can then be used to reproduce the manuscript visualizations.

---

## Scope of This Document

This document is intended as a practical computational overview. It does not replace the mathematical formulation of ES-MADM III provided in the manuscript.

For full theoretical definitions, formal notation, and mathematical derivations, readers should consult the associated manuscript.
