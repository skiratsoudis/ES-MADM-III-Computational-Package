# Manuscript Figure-Generation Script

This folder contains the R script used to reproduce the manuscript figures and benchmark visualizations for the ES-MADM III case study.

## File

```text
ES_MADM_III_Figures_Manuscript.R
```

---

## Purpose

The script generates the figures used in the manuscript based on the final ES-MADM III computational results and benchmark comparison structure.

It supports the reproduction of figures related to:

- central performance matrices;
- subjective weighting regimes;
- final ES-MADM III scores and rankings;
- subjective, objective, and integrated weight structures;
- Integrated Criteria Importance;
- system-level entropy and information measures;
- diagnostic indices and scenario-relative NMGI synthesis;
- decision-regime mapping;
- benchmark comparison against fuzzy PROMETHEE II;
- rank-transition and rank-concordance visualizations.

---

## How to Run

1. Open RStudio.
2. Open the file:

```text
ES_MADM_III_Figures_Manuscript.R
```

3. Run the script.
4. Review the generated figures in the RStudio Plots pane.
5. Export or copy individual figures as needed for manuscript preparation.

---

## Output Behavior

The script is configured to generate figures directly in the RStudio Plots pane. This allows the user to adjust the plot window size before exporting, copying, or inserting each figure into a manuscript document.

Automatic uncontrolled image export is avoided by default to preserve flexibility in figure sizing and formatting.

---

## Required R Packages

The script requires the following R packages:

```r
ggplot2
dplyr
tidyr
scales
ggrepel
tibble
grid
```

Missing packages can be installed using:

```r
install.packages(c(
  "ggplot2",
  "dplyr",
  "tidyr",
  "scales",
  "ggrepel",
  "tibble"
))
```

---

## Figure Scope

The script is designed to reproduce the manuscript-ready figures associated with:

- scenario-dependent central performance matrices;
- subjective criterion-weight regimes;
- final ES-MADM III score and ranking profiles;
- representative weight structures;
- criterion-level Integrated Criteria Importance;
- entropy and information measures;
- diagnostic profiles;
- scenario-relative NMGI synthesis;
- benchmark agreement and divergence against fuzzy PROMETHEE II.

The script does not generate obsolete development-stage plots or superseded diagnostic structures.

---

## Notes on Benchmark Figures

The benchmark visualizations compare ES-MADM III with fuzzy PROMETHEE II. The comparison is intended to evaluate ordinal agreement, rank-transition behavior, and methodological divergence under different scenario regimes.

The benchmark figures should not be interpreted as implying identical numerical scales between the two methods. ES-MADM III produces probability-consistent alternative scores, whereas fuzzy PROMETHEE II produces representative net flows. Therefore, the meaningful comparison concerns rank consistency and correlation behavior rather than direct score equality.

---

## Notes

This script corresponds to the final manuscript-ready visualization workflow. It excludes obsolete indices and development-stage figures.

In particular, the retained ES-MADM III diagnostic structure is based on:

- Normalized Mutual Information;
- Criteria Effectiveness Score;
- Alternatives Distinction Index;
- scenario-relative Net Mutual Growth Index.

Earlier visualization scripts and superseded figure-generation routines are intentionally excluded from this repository.
