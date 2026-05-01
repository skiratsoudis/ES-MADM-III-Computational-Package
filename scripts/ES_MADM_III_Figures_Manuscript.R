
###############################################################################
# ES-MADM III - Final Standalone Manuscript Figure Script
#
# Reproduces Figures 2-13 with improved readability:
# - Short scenario labels (S1, S2, S3, S4A, S4B).
# - Page-oriented figures with short titles and controlled labels.
# - Numeric annotations are reduced/removed where they caused overlap.
# - Figure 7 corrected: independent y-axis per entropy/information measure.
# - Benchmark labels corrected: "linear PROMETHEE-type", not "V-shape".
# - S3 benchmark correlation shown as "undefined", not "n/a".
# - Figure 1 is intentionally not generated.
# - CSF is not used.
# - Alternative labels and undefined benchmark labels are spaced to avoid overlap.
# - Page-friendly aspect ratios avoid excessive downscaling inside Word manuscripts.
# - Figures are printed to the RStudio Plots pane and are not saved automatically.
###############################################################################

req <- c("ggplot2", "dplyr", "tidyr", "scales", "ggrepel", "tibble", "grid")
missing <- req[!sapply(req, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) stop(sprintf("Missing required packages: %s", paste(missing, collapse = ", ")))

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(ggrepel)
library(tibble)
library(grid)

###############################################################################
# Plot handling: RStudio Plots pane first, optional export only on demand
###############################################################################

# Default behavior:
#   - Figures are NOT saved automatically.
#   - Figures are printed directly to the RStudio Plots pane.
#   - All plot objects are stored in plot_list and also assigned to the Global
#     Environment using their figure-file names without ".png".
#
# To browse the generated figures:
#   - Use the left/right arrows in the RStudio Plots tab.
#   - Resize the Plots pane and then call show_plot("Figure_4_Final_ES_MADM_III_Scores_and_Rankings")
#     to redraw a specific figure at the new pane size.
#
# To export later only if needed:
#   - Set EXPORT_PLOTS <- TRUE before running the script, or
#   - Use export_plot("Figure_4_Final_ES_MADM_III_Scores_and_Rankings", width = 8, height = 6).
#
# To export benchmark CSV tables later:
#   - Set EXPORT_TABLES <- TRUE before running the script, or
#   - Use write.csv() manually on promethee_results, benchmark_comparison, or rank_summary.

DISPLAY_IN_PLOTS_PANE <- TRUE
EXPORT_PLOTS <- FALSE
EXPORT_TABLES <- FALSE

out_dir <- file.path(getwd(), "ES_MADM_III_Final_Figures")
plot_list <- list()

safe_print_plot <- function(p, plot_name = "plot") {
  # RStudio's plot device can occasionally throw a grid depth(NULL) error
  # after several sequential ggplot prints. This wrapper prevents the script
  # from stopping, resets the graphics device once, and retries the display.
  tryCatch(
    {
      print(p)
      invisible(TRUE)
    },
    error = function(e1) {
      message(sprintf("Automatic display failed for %s: %s", plot_name, conditionMessage(e1)))
      message("Resetting graphics device and retrying once...")
      try(grDevices::graphics.off(), silent = TRUE)
      tryCatch(
        {
          print(p)
          invisible(TRUE)
        },
        error = function(e2) {
          message(sprintf("Retry also failed for %s: %s", plot_name, conditionMessage(e2)))
          message(sprintf("The plot object was still stored. Use show_plot('%s') after resizing/resetting the Plots pane.", plot_name))
          invisible(FALSE)
        }
      )
    }
  )
}

register_plot <- function(p, filename = NULL) {
  plot_name <- if (is.null(filename)) {
    paste0("plot_", length(plot_list) + 1)
  } else {
    sub("\\.png$", "", basename(filename))
  }
  plot_list[[plot_name]] <<- p
  assign(plot_name, p, envir = .GlobalEnv)
  invisible(plot_name)
}

save_plot <- function(p, filename, width = 8.2, height = 6.2, dpi = 600) {
  plot_name <- register_plot(p, filename)

  # Print directly into the RStudio Plots pane, without stopping the script
  # if the plot device temporarily fails.
  if (DISPLAY_IN_PLOTS_PANE) {
    safe_print_plot(p, plot_name)
  }

  # Optional export. Disabled by default.
  if (EXPORT_PLOTS) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    ggsave(file.path(out_dir, filename), p,
           width = width, height = height, dpi = dpi, bg = "white")
  }

  invisible(p)
}

list_plots <- function() {
  names(plot_list)
}

show_plot <- function(name) {
  if (!name %in% names(plot_list)) {
    stop(sprintf("Plot '%s' not found. Use list_plots() to see available plots.", name))
  }
  safe_print_plot(plot_list[[name]], name)
  invisible(plot_list[[name]])
}

show_all_plots <- function() {
  invisible(mapply(function(p, nm) safe_print_plot(p, nm), plot_list, names(plot_list)))
}

export_plot <- function(name, file = NULL, width = 8.2, height = 6.2, dpi = 600) {
  if (!name %in% names(plot_list)) {
    stop(sprintf("Plot '%s' not found. Use list_plots() to see available plots.", name))
  }
  if (is.null(file)) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    file <- file.path(out_dir, paste0(name, ".png"))
  }
  ggsave(file, plot_list[[name]], width = width, height = height, dpi = dpi, bg = "white")
  invisible(file)
}

theme_es <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 4, hjust = 0),
      plot.subtitle = element_text(size = base_size - 1, hjust = 0, margin = margin(b = 8)),
      axis.title = element_text(face = "bold", size = base_size),
      axis.text = element_text(size = base_size - 1),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      strip.text = element_text(face = "bold", size = base_size),
      strip.background = element_rect(fill = "grey92", color = "grey75", linewidth = 0.3),
      panel.spacing = unit(1.05, "lines"),
      panel.grid.minor = element_blank(),
      legend.title = element_text(face = "bold", size = base_size - 1),
      legend.text = element_text(size = base_size - 1),
      legend.position = "bottom",
      plot.margin = margin(14, 20, 14, 14)
    )
}

theme_es_facets <- function(base_size = 15) {
  theme_es(base_size) +
    theme(
      strip.text = element_text(face = "bold", size = base_size),
      strip.background = element_rect(fill = "grey92", color = "grey75", linewidth = 0.3),
      panel.spacing = unit(1.10, "lines")
    )
}


scenario_levels <- c("S1", "S2", "S3", "S4A", "S4B")
criterion_levels <- c("C1", "C2", "C3", "C4", "C5")
alternative_levels <- c("A1", "A2", "A3", "A4")
alt_labels <- c(A1 = "Y1", A2 = "Y2", A3 = "Y3", A4 = "Y4")
alt_legend_labels <- c(A1 = "Y1 = Amsterdam", A2 = "Y2 = Singapore", A3 = "Y3 = Copenhagen", A4 = "Y4 = Toronto")
alt_mapping_text <- "Alternative mapping: Y1 = Amsterdam; Y2 = Singapore; Y3 = Copenhagen; Y4 = Toronto."
scenario_subtitle <- "S1: baseline; S2: high uncertainty; S3: reduced separability; S4A: performance-focused conflict; S4B: cost/resilience-focused conflict"

read_block <- function(txt) read.table(text = txt, header = TRUE, stringsAsFactors = FALSE)

###############################################################################
# 1. Embedded case-study outputs
###############################################################################

central <- read_block("
Scenario Criterion A1 A2 A3 A4
S1 C1 9.0 8.5 7.0 6.5
S1 C2 8.0 9.5 8.5 6.0
S1 C3 9.0 8.5 8.0 7.0
S1 C4 6.5 7.5 5.5 6.0
S1 C5 8.5 9.0 8.0 6.5
S2 C1 9.0 8.5 7.0 6.5
S2 C2 8.0 9.5 8.5 6.0
S2 C3 9.0 8.5 8.0 7.0
S2 C4 6.5 7.5 5.5 6.0
S2 C5 8.5 9.0 8.0 6.5
S3 C1 8.5 8.3 8.2 8.0
S3 C2 8.8 9.0 8.7 8.3
S3 C3 8.6 8.7 8.5 8.2
S3 C4 6.4 6.6 6.2 6.3
S3 C5 8.7 8.9 8.5 8.2
S4A C1 9.0 8.0 7.0 6.0
S4A C2 8.0 9.5 8.5 5.5
S4A C3 9.0 8.0 8.5 7.0
S4A C4 7.0 8.0 5.0 4.5
S4A C5 8.0 9.0 8.0 6.0
S4B C1 9.0 8.0 7.0 6.0
S4B C2 8.0 9.5 8.5 5.5
S4B C3 9.0 8.0 8.5 7.0
S4B C4 7.0 8.0 5.0 4.5
S4B C5 8.0 9.0 8.0 6.0
") %>% mutate(Scenario = factor(Scenario, levels = scenario_levels),
              Criterion = factor(Criterion, levels = criterion_levels))

spread <- read_block("
Scenario Criterion A1 A2 A3 A4
S1 C1 0.5 0.5 0.5 0.5
S1 C2 0.5 0.5 0.5 0.5
S1 C3 0.4 0.4 0.4 0.4
S1 C4 0.5 0.5 0.5 0.5
S1 C5 0.4 0.4 0.4 0.4
S2 C1 1.0 1.0 1.0 1.0
S2 C2 1.0 1.0 1.0 1.0
S2 C3 0.8 0.8 0.8 0.8
S2 C4 1.0 1.0 1.0 1.0
S2 C5 0.8 0.8 0.8 0.8
S3 C1 0.3 0.3 0.3 0.3
S3 C2 0.3 0.3 0.3 0.3
S3 C3 0.3 0.3 0.3 0.3
S3 C4 0.3 0.3 0.3 0.3
S3 C5 0.3 0.3 0.3 0.3
S4A C1 0.5 0.5 0.5 0.5
S4A C2 0.5 0.5 0.5 0.5
S4A C3 0.4 0.4 0.4 0.4
S4A C4 0.5 0.5 0.5 0.5
S4A C5 0.4 0.4 0.4 0.4
S4B C1 0.5 0.5 0.5 0.5
S4B C2 0.5 0.5 0.5 0.5
S4B C3 0.4 0.4 0.4 0.4
S4B C4 0.5 0.5 0.5 0.5
S4B C5 0.4 0.4 0.4 0.4
") %>% mutate(Scenario = factor(Scenario, levels = scenario_levels),
              Criterion = factor(Criterion, levels = criterion_levels))

subjective <- read_block("
Scenario Criterion xSBJ
S1 C1 0.22
S1 C2 0.24
S1 C3 0.20
S1 C4 0.16
S1 C5 0.18
S2 C1 0.22
S2 C2 0.24
S2 C3 0.20
S2 C4 0.16
S2 C5 0.18
S3 C1 0.22
S3 C2 0.24
S3 C3 0.20
S3 C4 0.16
S3 C5 0.18
S4A C1 0.28
S4A C2 0.26
S4A C3 0.18
S4A C4 0.14
S4A C5 0.14
S4B C1 0.18
S4B C2 0.20
S4B C3 0.16
S4B C4 0.24
S4B C5 0.22
") %>% mutate(Scenario = factor(Scenario, levels = scenario_levels),
              Criterion = factor(Criterion, levels = criterion_levels))

weights <- read_block("
Scenario Criterion xSBJ xOBJ xINT h d ICI
S1 C1 0.22 0.2985633239 0.3166958500 0.8943387802 0.1056612198 0.0334624698
S1 C2 0.24 0.2709675231 0.3135535467 0.9041049026 0.0958950974 0.0300682479
S1 C3 0.20 0.1194674484 0.1152027425 0.9577206063 0.0422793937 0.0048707021
S1 C4 0.16 0.1593053173 0.1228947948 0.9436220299 0.0563779701 0.0069285591
S1 C5 0.18 0.1516963873 0.1316530660 0.9463148215 0.0536851785 0.0070678183
S2 C1 0.22 0.3050843036 0.3188251372 0.9089128528 0.0910871472 0.0290408722
S2 C2 0.24 0.3094334232 0.3527674240 0.9076143629 0.0923856371 0.0325906432
S2 C3 0.20 0.1021709531 0.0970662874 0.9694947484 0.0305052516 0.0029609919
S2 C4 0.16 0.1147222635 0.0871918793 0.9657476493 0.0342523507 0.0029864280
S2 C5 0.18 0.1685890567 0.1441492722 0.9496651655 0.0503348345 0.0072557336
S3 C1 0.22 0.0914688313 0.0995355717 0.9993112964 0.0006887036 0.0000685505
S3 C2 0.24 0.2902371513 0.3445456321 0.9978146941 0.0021853059 0.0007529376
S3 C3 0.20 0.0928377693 0.0918405087 0.9993009853 0.0006990147 0.0000641976
S3 C4 0.16 0.0379693206 0.0300491950 0.9997137710 0.0002862290 0.0000086009
S3 C5 0.18 0.4874869275 0.4340290925 0.9963302150 0.0036697850 0.0015930816
S4A C1 0.28 0.2125933508 0.3036772620 0.8812912377 0.1187087623 0.0360491519
S4A C2 0.26 0.1935500032 0.2567267349 0.8919247416 0.1080752584 0.0277458082
S4A C3 0.18 0.0757173928 0.0695300790 0.9577206063 0.0422793937 0.0029396896
S4A C4 0.14 0.3830346978 0.2735714165 0.7861194872 0.2138805128 0.0585115949
S4A C5 0.14 0.1351045553 0.0964945076 0.9245597547 0.0754402453 0.0072795693
S4B C1 0.18 0.2125933508 0.1815804955 0.8812912377 0.1187087623 0.0215551959
S4B C2 0.20 0.1935500032 0.1836835198 0.8919247416 0.1080752584 0.0198516439
S4B C3 0.16 0.0757173928 0.0574860738 0.9577206063 0.0422793937 0.0024304763
S4B C4 0.24 0.3830346978 0.4362107590 0.7861194872 0.2138805128 0.0932969808
S4B C5 0.22 0.1351045553 0.1410391518 0.9245597547 0.0754402453 0.0106400282
") %>% mutate(Scenario = factor(Scenario, levels = scenario_levels),
              Criterion = factor(Criterion, levels = criterion_levels))

alt_scores <- read_block("
Scenario Alternative Score Rank
S1 A2 0.3432563265 1
S1 A1 0.3026929468 2
S1 A3 0.2240963445 3
S1 A4 0.1299543822 4
S2 A2 0.3451500942 1
S2 A1 0.3038630834 2
S2 A3 0.2274778933 3
S2 A4 0.1235089291 4
S3 A2 0.2777236849 1
S3 A1 0.2559545286 2
S3 A3 0.2378346450 3
S3 A4 0.2284871415 4
S4A A1 0.2714207510 1
S4A A2 0.2694221508 2
S4A A3 0.2496405311 3
S4A A4 0.2095165672 4
S4B A4 0.2771218721 1
S4B A3 0.2756562110 2
S4B A2 0.2291013506 3
S4B A1 0.2181205663 4
") %>% mutate(Scenario = factor(Scenario, levels = scenario_levels),
              Alternative = factor(Alternative, levels = alternative_levels))

entropy <- read_block("
Scenario SY SX SY_given_X SXY JXY
S1 1.9175190744 2.1659668774 1.8352044055 4.0011712830 0.0823146689
S2 1.9104976530 2.0923868850 1.8503306622 3.9427175472 0.0601669908
S3 1.9959489584 1.8519066542 1.9950252637 3.8469319178 0.0009236947
S4A 1.9926435892 2.1302836739 1.7349483722 3.8652320461 0.2576952170
S4B 1.9917503696 2.0535141655 1.7044513497 3.7579655152 0.2872990199
") %>% mutate(Scenario = factor(Scenario, levels = scenario_levels))

diagnostics <- read_block("
Scenario NMI CES ADI NMGI
S1 0.0429276923 0.0823977972 0.0412404628 0.0500885788
S2 0.0314928368 0.0748346689 0.0447511735 0.0463936331
S3 0.0004627847 0.0024873681 0.0020255208 0.0015981938
S4A 0.1293232861 0.1325258139 0.0036782054 0.0714530387
S4B 0.1442444918 0.1477743252 0.0041248152 0.0796990797
") %>% mutate(Scenario = factor(Scenario, levels = scenario_levels))

nmgi_weights <- read_block("
Index Weight
NMI 0.3329496275
CES 0.2013335815
ADI 0.4657167910
")

###############################################################################
# 2. Fuzzy PROMETHEE II benchmark computation
###############################################################################

a <- 0.50
criterion_type <- c(C1 = "benefit", C2 = "benefit", C3 = "benefit", C4 = "cost", C5 = "benefit")
q <- c(C1 = 0.20, C2 = 0.20, C3 = 0.20, C4 = 0.20, C5 = 0.20)
p <- c(C1 = 0.80, C2 = 0.80, C3 = 0.80, C4 = 0.80, C5 = 0.80)

to_matrix <- function(df, scen) {
  z <- df %>% filter(Scenario == scen) %>% arrange(Criterion)
  m <- as.matrix(z[, alternative_levels])
  rownames(m) <- as.character(z$Criterion)
  m
}

pref_linear_promethee <- function(d, q, p) ifelse(d <= q, 0, ifelse(d >= p, 1, (d - q) / (p - q)))

compute_promethee <- function(scen) {
  central_m <- to_matrix(central, scen)
  spread_m <- to_matrix(spread, scen)
  w <- subjective %>% filter(Scenario == scen) %>% arrange(Criterion) %>% pull(xSBJ)
  names(w) <- criterion_levels
  w <- w / sum(w)

  lower <- central_m - (1 - a) * spread_m
  upper <- central_m + (1 - a) * spread_m
  R_lower <- lower
  R_upper <- upper

  for (mu in criterion_levels) {
    if (criterion_type[[mu]] == "benefit") {
      denom <- max(upper[mu, ])
      R_lower[mu, ] <- lower[mu, ] / denom
      R_upper[mu, ] <- upper[mu, ] / denom
    } else {
      reference <- min(lower[mu, ])
      R_lower[mu, ] <- reference / upper[mu, ]
      R_upper[mu, ] <- reference / lower[mu, ]
    }
  }

  n <- length(alternative_levels)
  pi_lower <- matrix(0, n, n, dimnames = list(alternative_levels, alternative_levels))
  pi_upper <- matrix(0, n, n, dimnames = list(alternative_levels, alternative_levels))

  for (i in alternative_levels) {
    for (j in alternative_levels) {
      if (i == j) next
      lower_sum <- 0
      upper_sum <- 0
      for (mu in criterion_levels) {
        d_lower <- R_lower[mu, i] - R_upper[mu, j]
        d_upper <- R_upper[mu, i] - R_lower[mu, j]
        lower_sum <- lower_sum + w[[mu]] * pref_linear_promethee(d_lower, q[[mu]], p[[mu]])
        upper_sum <- upper_sum + w[[mu]] * pref_linear_promethee(d_upper, q[[mu]], p[[mu]])
      }
      pi_lower[i, j] <- lower_sum
      pi_upper[i, j] <- upper_sum
    }
  }

  phi_plus_lower <- rowSums(pi_lower) / (n - 1)
  phi_plus_upper <- rowSums(pi_upper) / (n - 1)
  phi_minus_lower <- colSums(pi_lower) / (n - 1)
  phi_minus_upper <- colSums(pi_upper) / (n - 1)

  out <- data.frame(
    Scenario = scen,
    Alternative = alternative_levels,
    PhiPlus_L = as.numeric(phi_plus_lower[alternative_levels]),
    PhiPlus_U = as.numeric(phi_plus_upper[alternative_levels]),
    PhiMinus_L = as.numeric(phi_minus_lower[alternative_levels]),
    PhiMinus_U = as.numeric(phi_minus_upper[alternative_levels]),
    PhiNet_L = as.numeric(phi_plus_lower[alternative_levels] - phi_minus_upper[alternative_levels]),
    PhiNet_U = as.numeric(phi_plus_upper[alternative_levels] - phi_minus_lower[alternative_levels])
  )
  out$PhiNet_star <- (out$PhiNet_L + out$PhiNet_U) / 2
  out$PROMETHEE_Rank <- rank(-out$PhiNet_star, ties.method = "min")
  out
}

promethee_results <- bind_rows(lapply(scenario_levels, compute_promethee)) %>%
  mutate(Scenario = factor(Scenario, levels = scenario_levels),
         Alternative = factor(Alternative, levels = alternative_levels))

benchmark_comparison <- promethee_results %>%
  left_join(
    alt_scores %>% rename(ES_MADM_III_Score = Score, ES_MADM_III_Rank = Rank),
    by = c("Scenario", "Alternative")
  ) %>%
  mutate(Rank_Difference = PROMETHEE_Rank - ES_MADM_III_Rank)

safe_cor <- function(x, y, method) {
  if (length(unique(x)) < 2 || length(unique(y)) < 2) return(NA_real_)
  suppressWarnings(cor(x, y, method = method))
}

rank_summary <- benchmark_comparison %>%
  group_by(Scenario) %>%
  summarise(
    Spearman_rho = safe_cor(PROMETHEE_Rank, ES_MADM_III_Rank, "spearman"),
    Kendall_tau = safe_cor(PROMETHEE_Rank, ES_MADM_III_Rank, "kendall"),
    .groups = "drop"
  ) %>%
  mutate(
    CorrLabel = ifelse(
      is.na(Spearman_rho),
      paste0(as.character(Scenario), "\nrho: undefined; tau: undefined"),
      paste0(as.character(Scenario), "\nrho=", sprintf("%.2f", Spearman_rho),
             "; tau=", sprintf("%.2f", Kendall_tau))
    )
  )

if (EXPORT_TABLES) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  write.csv(promethee_results,
            file.path(out_dir, "All_Scenarios_Fuzzy_PROMETHEE_II_Flows.csv"),
            row.names = FALSE)
  write.csv(benchmark_comparison,
            file.path(out_dir, "All_Scenarios_ES_MADM_III_vs_Fuzzy_PROMETHEE_Comparison.csv"),
            row.names = FALSE)
  write.csv(rank_summary,
            file.path(out_dir, "All_Scenarios_Benchmark_Rank_Correlations.csv"),
            row.names = FALSE)
}

###############################################################################
# 3. Figures
###############################################################################

central_long <- central %>%
  pivot_longer(all_of(alternative_levels), names_to = "Alternative", values_to = "CentralValue")

p2 <- ggplot(central_long, aes(x = Alternative, y = Criterion, fill = CentralValue)) +
  geom_tile(color = "white", linewidth = 0.45) +
  geom_text(aes(label = sprintf("%.1f", CentralValue)), size = 3.8, fontface = "bold") +
  facet_wrap(~Scenario, ncol = 2) +
  scale_x_discrete(labels = alt_labels) +
  scale_fill_gradient(low = "#f4f8ff", high = "#1f5aa6", name = "Value") +
  labs(title = "Fig. 2. Central performance matrices",
       x = "Alternative", y = "Criterion") +
  theme_es(13) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "right")
save_plot(p2, "Figure_2_Central_Performance_Matrices.png", width = 7.8, height = 8.8)

p3 <- ggplot(subjective, aes(x = Criterion, y = xSBJ, fill = Criterion)) +
  geom_col(width = 0.66, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", xSBJ)), vjust = -0.30, size = 3.8, fontface = "bold") +
  facet_wrap(~Scenario, ncol = 2) +
  scale_y_continuous(limits = c(0, 0.34), expand = expansion(mult = c(0, 0.09))) +
  labs(title = "Fig. 3. Subjective weighting regimes",
       x = "Criterion", y = "Subjective weight") +
  theme_es(13)
save_plot(p3, "Figure_3_Subjective_Weight_Regimes.png", width = 7.6, height = 8.4)

p4 <- ggplot(alt_scores, aes(x = Alternative, y = Score, fill = Alternative)) +
  geom_col(width = 0.64) +
  geom_text(aes(label = paste0("r=", Rank, "\n", sprintf("%.3f", Score))),
            vjust = -0.22, size = 3.6, fontface = "bold", lineheight = 0.85) +
  facet_wrap(~Scenario, ncol = 2) +
  scale_x_discrete(labels = alt_labels) +
  scale_fill_discrete(labels = alt_legend_labels, name = "Alternative") +
  scale_y_continuous(limits = c(0, 0.405), expand = expansion(mult = c(0, 0.10))) +
  labs(title = "Fig. 4. Final scores and rankings",
       x = "Alternative", y = "Final score") +
  theme_es(13) +
  theme(axis.text.x = element_text(size = 12),
        legend.position = "bottom",
        legend.box = "horizontal")
save_plot(p4, "Figure_4_Final_ES_MADM_III_Scores_and_Rankings.png", width = 7.8, height = 8.8)

weights_long <- weights %>%
  select(Scenario, Criterion, xSBJ, xOBJ, xINT) %>%
  pivot_longer(c(xSBJ, xOBJ, xINT), names_to = "WeightType", values_to = "Value") %>%
  mutate(WeightType = recode(WeightType, xSBJ = "Subjective", xOBJ = "Objective", xINT = "Integrated"),
         WeightType = factor(WeightType, levels = c("Subjective", "Objective", "Integrated")))

p5 <- ggplot(weights_long, aes(x = Criterion, y = Value, fill = WeightType)) +
  geom_col(position = position_dodge(width = 0.76), width = 0.66) +
  facet_wrap(~Scenario, ncol = 2) +
  scale_y_continuous(limits = c(0, 0.55), expand = expansion(mult = c(0, 0.07))) +
  labs(title = "Fig. 5. Weight structure",
       x = "Criterion", y = "Weight", fill = "Weight type") +
  theme_es(13) +
  theme(legend.position = "bottom")
save_plot(p5, "Figure_5_Weight_Structure.png", width = 7.8, height = 8.8)

p6 <- ggplot(weights, aes(x = Criterion, y = ICI, fill = Criterion)) +
  geom_col(width = 0.64, show.legend = FALSE) +
  geom_text(aes(label = ifelse(ICI >= 0.005, sprintf("%.3f", ICI), "")),
            vjust = -0.28, size = 3.3, fontface = "bold") +
  facet_wrap(~Scenario, ncol = 2) +
  scale_y_continuous(limits = c(0, 0.108), breaks = c(0, 0.03, 0.06, 0.09),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Fig. 6. Integrated Criteria Importance",
       x = "Criterion", y = "Integrated Criteria Importance") +
  theme_es(13) +
  theme(axis.text.x = element_text(size = 12))
save_plot(p6, "Figure_6_Integrated_Criteria_Importance.png", width = 7.8, height = 8.8)

entropy_long <- entropy %>%
  pivot_longer(c(SY, SX, SY_given_X, SXY, JXY), names_to = "Measure", values_to = "Value") %>%
  mutate(Measure = recode(Measure, SY = "S(Y)", SX = "S(X)", SY_given_X = "S(Y|X)", SXY = "S(X,Y)", JXY = "J(X;Y)"),
         Measure = factor(Measure, levels = c("S(Y)", "S(X)", "S(Y|X)", "S(X,Y)", "J(X;Y)")))

p7 <- ggplot(entropy_long, aes(x = Scenario, y = Value, fill = Scenario)) +
  geom_col(width = 0.64, show.legend = FALSE) +
  facet_wrap(~Measure, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Fig. 7. Entropy and information measures",
       x = "Scenario", y = "Value") +
  theme_es(13) +
  theme(axis.text.x = element_text(size = 11))
save_plot(p7, "Figure_7_System_Level_Entropy_and_Information_Measures.png", width = 7.8, height = 8.8)

diag_long <- diagnostics %>%
  pivot_longer(c(NMI, CES, ADI, NMGI), names_to = "Index", values_to = "Value") %>%
  mutate(Index = factor(Index, levels = c("NMI", "CES", "ADI", "NMGI")))

p8 <- ggplot(diag_long, aes(x = Scenario, y = Value, group = Index, color = Index)) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 3.2) +
  geom_text_repel(aes(label = sprintf("%.3f", Value)), size = 3.2, fontface = "bold",
                  min.segment.length = 0, box.padding = 0.25, point.padding = 0.25,
                  max.overlaps = Inf, seed = 120) +
  scale_y_continuous(limits = c(0, 0.17), expand = expansion(mult = c(0, 0.10))) +
  labs(title = "Fig. 8. Diagnostic profile and NMGI_s",
       x = "Scenario", y = "Index value", color = "Index") +
  theme_es(13)
save_plot(p8, "Figure_8_Diagnostic_Profile_and_NMGI_Synthesis.png", width = 7.8, height = 6.6)

map_data <- diagnostics %>%
  mutate(Label = as.character(Scenario),
         nudge_x = case_when(Scenario == "S4A" ~ -0.0015, Scenario == "S4B" ~ 0.0016, Scenario == "S3" ~ 0.0012, TRUE ~ 0),
         nudge_y = case_when(Scenario == "S4A" ~ -0.010, Scenario == "S4B" ~ 0.010, Scenario == "S3" ~ 0.006, TRUE ~ 0))

p9 <- ggplot(map_data, aes(x = ADI, y = NMI)) +
  geom_point(aes(size = NMGI), alpha = 0.72, color = "#1f5aa6") +
  geom_text_repel(aes(label = Label), size = 5.0, fontface = "bold",
                  nudge_x = map_data$nudge_x, nudge_y = map_data$nudge_y,
                  min.segment.length = 0, segment.size = 0.35, box.padding = 0.55,
                  point.padding = 0.50, max.overlaps = Inf, seed = 123) +
  scale_size_continuous(range = c(5, 16), name = "NMGI_s") +
  scale_x_continuous(limits = c(0, 0.05), labels = label_number(accuracy = 0.001)) +
  scale_y_continuous(limits = c(0, 0.16), labels = label_number(accuracy = 0.01)) +
  labs(title = "Fig. 9. Decision-regime map",
       x = "ADI", y = "NMI") +
  theme_es(16)
save_plot(p9, "Figure_9_Decision_Regime_Map.png", width = 7.2, height = 6.2)

p10 <- ggplot(nmgi_weights, aes(x = Index, y = Weight, fill = Index)) +
  geom_col(width = 0.62, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.3f", Weight)), vjust = -0.30, size = 4.2, fontface = "bold") +
  scale_y_continuous(limits = c(0, 0.55), expand = expansion(mult = c(0, 0.11))) +
  labs(title = "Fig. 10. Diagnostic weights for NMGI_s",
       x = "Diagnostic index", y = "Weight") +
  theme_es(13)
save_plot(p10, "Figure_10_Diagnostic_Weights_for_NMGI_Synthesis.png", width = 6.8, height = 5.6)

facet_labels <- rank_summary %>% select(Scenario, CorrLabel)

plot_data <- benchmark_comparison %>%
  left_join(facet_labels, by = "Scenario") %>%
  mutate(CorrLabel = factor(CorrLabel, levels = rank_summary$CorrLabel),
         AlternativeLabel = recode(as.character(Alternative), A1 = "Y1", A2 = "Y2", A3 = "Y3", A4 = "Y4"))

p11 <- ggplot(plot_data, aes(x = PhiNet_star, y = ES_MADM_III_Score)) +
  geom_vline(xintercept = 0, linewidth = 0.35, linetype = "dashed") +
  geom_point(size = 3.4, color = "#1f5aa6") +
  geom_text_repel(aes(label = AlternativeLabel), size = 3.6, fontface = "bold",
                  min.segment.length = 0, box.padding = 0.35, point.padding = 0.30,
                  seed = 123, max.overlaps = Inf) +
  facet_wrap(~CorrLabel, ncol = 2, scales = "free_x") +
  labs(title = "Fig. 11. Benchmark correlation map",
       x = "Fuzzy PROMETHEE II net flow", y = "ES-MADM III score") +
  theme_es(12)
save_plot(p11, "Figure_11_ES_MADM_III_vs_Fuzzy_PROMETHEE_Correlation_Map.png", width = 7.8, height = 8.8)

rank_long <- benchmark_comparison %>%
  transmute(Scenario, Alternative, PROMETHEE_Rank, ES_MADM_III_Rank) %>%
  pivot_longer(c(PROMETHEE_Rank, ES_MADM_III_Rank), names_to = "Method", values_to = "Rank") %>%
  mutate(Method = recode(Method, PROMETHEE_Rank = "Fuzzy PROMETHEE II", ES_MADM_III_Rank = "ES-MADM III"),
         Method = factor(Method, levels = c("Fuzzy PROMETHEE II", "ES-MADM III")),
         AlternativeLabel = recode(as.character(Alternative), A1 = "Y1", A2 = "Y2", A3 = "Y3", A4 = "Y4"))

p12 <- ggplot(rank_long, aes(x = Method, y = Rank, group = AlternativeLabel, color = AlternativeLabel)) +
  geom_line(linewidth = 1.10, alpha = 0.82) +
  geom_point(size = 3.5) +
  facet_wrap(~Scenario, ncol = 2) +
  scale_y_reverse(breaks = 1:4, limits = c(4.25, 0.75)) +
  labs(title = "Fig. 12. Rank transition",
       x = NULL, y = "Rank", color = "Alternative") +
  theme_es(13) +
  theme(axis.text.x = element_text(size = 10),
        legend.position = "bottom",
        legend.box = "horizontal")
save_plot(p12, "Figure_12_Rank_Transition_PROMETHEE_vs_ES_MADM_III.png", width = 7.8, height = 8.8)

concordance_long <- rank_summary %>%
  select(Scenario, Spearman_rho, Kendall_tau) %>%
  pivot_longer(c(Spearman_rho, Kendall_tau), names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = recode(Metric, Spearman_rho = "Spearman rho", Kendall_tau = "Kendall tau"),
         Metric = factor(Metric, levels = c("Spearman rho", "Kendall tau")),
         ValueForPlot = ifelse(is.na(Value), 0, Value),
         Label = ifelse(is.na(Value), "undefined", sprintf("%.2f", Value)),
         LabelY = case_when(
           is.na(Value) & Metric == "Spearman rho" ~ 0.10,
           is.na(Value) & Metric == "Kendall tau" ~ 0.22,
           ValueForPlot >= 0 ~ ValueForPlot + 0.04,
           TRUE ~ ValueForPlot - 0.05
         ))

p13 <- ggplot(concordance_long, aes(x = Scenario, y = ValueForPlot, fill = Metric)) +
  geom_hline(yintercept = 0, linewidth = 0.35) +
  geom_col(position = position_dodge(width = 0.74), width = 0.60) +
  geom_text(aes(y = LabelY, label = Label), position = position_dodge(width = 0.74),
            size = 3.5, fontface = "bold") +
  scale_y_continuous(limits = c(-0.62, 1.18), breaks = seq(-0.5, 1.0, by = 0.25)) +
  labs(title = "Fig. 13. Rank-concordance profile",
       x = "Scenario", y = "Rank-correlation value", fill = "Metric") +
  theme_es(13)
save_plot(p13, "Figure_13_Rank_Concordance_Profile.png", width = 7.4, height = 5.8)

cat("\nES-MADM III figures were generated and sent to the RStudio Plots pane.\n")
cat("No image files were saved automatically because EXPORT_PLOTS = FALSE.\n")
cat("No benchmark CSV files were saved automatically because EXPORT_TABLES = FALSE.\n")
cat("\nIf any automatic display was skipped because of an RStudio plot-device error, the plot object is still stored.\n")
cat("Resize/reset the Plots pane and call show_plot('plot_name') to redraw it.\n")
cat("\nAvailable plot objects:\n")
print(list_plots())
cat("\nExamples:\n")
cat("show_plot('Figure_10_Diagnostic_Weights_for_NMGI_Synthesis')\n")
cat("show_plot('Figure_11_ES_MADM_III_vs_Fuzzy_PROMETHEE_Correlation_Map')\n")
cat("show_all_plots()\n")
cat("export_plot('Figure_4_Final_ES_MADM_III_Scores_and_Rankings', width = 8, height = 6)\n")
