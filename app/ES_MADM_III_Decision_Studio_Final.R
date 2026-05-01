###############################################################################
# ESMADMIII_DECISION_STUDIO.R
#
# ES-MADM III Decision Studio
# Operational Shiny Platform for the ES-MADM III Framework
#
# This application implements the ES-MADM III methodology as a coherent,
# operational, and decision-oriented multicriteria analysis environment under
# fuzzy uncertainty. The platform supports the complete analytical workflow of
# the model, including alpha-cut propagation, normalization, preference-enhanced
# conditional probability construction, entropy-based weighting, alternative
# scoring, diagnostic evaluation, scenario analysis, and structured export of
# results.
#
# The present implementation is aligned with the finalized ES-MADM III model
# specification and is designed to provide a computationally stable,
# professionally structured, and practically usable decision-support interface.
# It supports generalized PROMETHEE-type preference-function families, scenario
# portfolio analysis, preference-function sensitivity analysis, and alpha-based
# robustness exploration within a unified Shiny environment.
#
# Model Creator:
# LT COL (ORD) Dr. Kyratsoudis Sideris
#
# Academic and Computational Scope:
# - Fuzzy multicriteria decision analysis under interval uncertainty
# - Information-sensitive entropy-based objective, subjective, and integrated weighting
# - Preference-conditioned probabilistic evaluation of alternatives
# - Operational diagnostic indices: NMI, CES, ADI, and scenario-level NMGI
# - Multi-scenario analytical support and export-ready decision outputs
#
# This codebase has been structured for professional use, research support,
# reproducible experimentation, and repository publication.
###############################################################################
suppressPackageStartupMessages({
  library(shiny)
  library(shinythemes)
  library(readxl)
  library(writexl)
  library(ggplot2)
  library(DT)
  library(dplyr)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
EPS <- 1e-12

round3 <- function(x) round(x, 3)
fmt3 <- function(x) sprintf("%.3f", x)

restore_shape <- function(ref, y) {
  if (is.matrix(ref) || is.array(ref)) {
    y <- array(as.vector(y), dim = dim(ref), dimnames = dimnames(ref))
  } else if (!is.null(names(ref)) && length(y) == length(ref)) {
    names(y) <- names(ref)
  }
  y
}

clamp01 <- function(x) restore_shape(x, pmin(1, pmax(0, x)))
pmax_preserve <- function(x, y) restore_shape(x, pmax(x, y))
pmin_preserve <- function(x, y) restore_shape(x, pmin(x, y))

as_num_vec <- function(x, allow_empty = FALSE) {
  if (is.null(x)) {
    if (allow_empty) return(numeric(0))
    stop("Encountered NULL where a numeric vector was required.")
  }
  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.matrix(x) || is.array(x)) x <- as.vector(x)
  x <- suppressWarnings(as.numeric(unname(x)))
  if (!allow_empty && length(x) == 0) stop("Encountered an empty numeric vector during computation.")
  x
}

safe_div <- function(num, den, eps = EPS) {
  num <- as_num_vec(num)
  den <- as_num_vec(den)
  if (length(den) == 1L && length(num) > 1L) den <- rep(den, length(num))
  if (length(num) == 1L && length(den) > 1L) num <- rep(num, length(den))
  if (length(num) != length(den)) stop("safe_div received incompatible lengths.")
  num / pmax(den, eps)
}

log2safe <- function(x, eps = EPS) log2(pmax(as_num_vec(x), eps))

renorm_simplex <- function(x, eps = EPS) {
  x <- pmax(as_num_vec(x), 0)
  if (length(x) == 0) stop("Cannot normalize an empty simplex vector.")
  s <- sum(x)
  if (!is.finite(s) || s <= eps) return(rep(1 / length(x), length(x)))
  x / s
}

mid_interval <- function(lower, upper) {
  lower <- as_num_vec(lower)
  upper <- as_num_vec(upper)
  if (length(lower) != length(upper)) stop("mid_interval received incompatible lengths.")
  (lower + upper) / 2
}

entropy_base2 <- function(p, eps = EPS) {
  p <- pmax(as_num_vec(p), 0)
  s <- sum(p)
  if (!is.finite(s) || s <= eps) return(0)
  p <- p / s
  nz <- p > eps
  -sum(p[nz] * log2(p[nz]))
}

repair_interval_bounds <- function(lower, upper, total = 1, eps = EPS) {
  lower <- as_num_vec(lower)
  upper <- as_num_vec(upper)
  if (length(lower) != length(upper)) stop("Interval bounds must have the same length.")
  if (length(lower) == 0) stop("Interval bounds cannot be empty.")

  lower[!is.finite(lower)] <- 0
  upper[!is.finite(upper)] <- 0

  lower <- pmax(lower, 0)
  upper <- pmax(upper, lower)
  lower <- pmin(lower, total)
  upper <- pmin(upper, total)

  sL <- sum(lower)
  if (sL > total + 1e-10) {
    lower <- lower / sL * total
    upper <- pmax(upper, lower)
    upper <- pmin(upper, total)
  }

  sU <- sum(upper)
  if (sU < total - 1e-10) {
    room <- pmax(total - upper, 0)
    if (sum(room) <= eps) {
      upper <- rep(total / length(upper), length(upper))
    } else {
      upper <- upper + (total - sU) * room / sum(room)
      upper <- pmax(upper, lower)
      upper <- pmin(upper, total)
    }
  }

  list(lower = lower, upper = upper)
}

interval_simplex_bounds <- function(lower, upper, eps = EPS) {
  repaired <- repair_interval_bounds(lower, upper, total = 1, eps = eps)
  lower <- repaired$lower
  upper <- repaired$upper
  m <- length(lower)

  outL <- numeric(m)
  outU <- numeric(m)

  for (i in seq_len(m)) {
    denL <- lower[i] + sum(upper[-i])
    denU <- upper[i] + sum(lower[-i])
    outL[i] <- safe_div(lower[i], denL, eps = eps)
    outU[i] <- safe_div(upper[i], denU, eps = eps)
  }

  outL <- pmax(outL, 0)
  outU <- pmax(outU, outL)

  list(
    lower = outL,
    upper = outU,
    crisp = renorm_simplex(mid_interval(outL, outU), eps = eps)
  )
}

project_box_simplex <- function(target, lower, upper, total = 1, eps = EPS) {
  target <- as_num_vec(target)
  repaired <- repair_interval_bounds(lower, upper, total = total, eps = eps)
  lower <- repaired$lower
  upper <- repaired$upper

  if (length(target) != length(lower)) stop("Projection target and bounds must have the same length.")
  if (sum(lower) > total + 1e-10 || sum(upper) < total - 1e-10) {
    stop("Projection bounds do not define a nonempty box-constrained simplex.")
  }

  phi <- function(lambda) sum(pmin(pmax(target - lambda, lower), upper)) - total

  lo <- min(target - upper) - total - 1
  hi <- max(target - lower) + total + 1

  for (iter in seq_len(300)) {
    mid <- (lo + hi) / 2
    val <- phi(mid)
    if (abs(val) <= 1e-13) break
    if (val > 0) lo <- mid else hi <- mid
  }

  x <- pmin(pmax(target - (lo + hi) / 2, lower), upper)
  diff <- total - sum(x)

  if (abs(diff) > 1e-11) {
    if (diff > 0) {
      slack <- upper - x
      ord <- order(slack, decreasing = TRUE)
      for (i in ord) {
        add <- min(diff, slack[i])
        x[i] <- x[i] + add
        diff <- diff - add
        if (diff <= 1e-12) break
      }
    } else {
      diff <- -diff
      slack <- x - lower
      ord <- order(slack, decreasing = TRUE)
      for (i in ord) {
        sub <- min(diff, slack[i])
        x[i] <- x[i] - sub
        diff <- diff - sub
        if (diff <= 1e-12) break
      }
    }
  }

  x <- pmin(pmax(x, lower), upper)
  if (abs(sum(x) - total) > 1e-8) {
    stop("Projection failed to satisfy the simplex equality within tolerance.")
  }
  x
}

entropy_max_vector <- function(lower, upper, total = 1, eps = EPS) {
  repaired <- repair_interval_bounds(lower, upper, total = total, eps = eps)
  lower <- repaired$lower
  upper <- repaired$upper

  f <- function(cval) sum(pmin(pmax(cval, lower), upper)) - total

  lo <- min(lower)
  hi <- max(upper)
  for (iter in seq_len(200)) {
    mid <- (lo + hi) / 2
    val <- f(mid)
    if (abs(val) <= 1e-12) break
    if (val > 0) hi <- mid else lo <- mid
  }

  p <- pmin(pmax((lo + hi) / 2, lower), upper)
  diff <- total - sum(p)

  if (abs(diff) > 1e-10) {
    if (diff > 0) {
      slack <- upper - p
      ord <- order(slack, decreasing = TRUE)
      for (i in ord) {
        add <- min(diff, slack[i])
        p[i] <- p[i] + add
        diff <- diff - add
        if (diff <= 1e-12) break
      }
    } else {
      diff <- -diff
      slack <- p - lower
      ord <- order(slack, decreasing = TRUE)
      for (i in ord) {
        sub <- min(diff, slack[i])
        p[i] <- p[i] - sub
        diff <- diff - sub
        if (diff <= 1e-12) break
      }
    }
  }

  renorm_simplex(p, eps = eps)
}

entropy_min_vector <- function(lower, upper, total = 1, eps = EPS) {
  repaired <- repair_interval_bounds(lower, upper, total = total, eps = eps)
  lower <- repaired$lower
  upper <- repaired$upper

  p <- lower
  residual <- total - sum(p)
  if (residual <= eps) return(renorm_simplex(p, eps = eps))

  ord <- order(upper, decreasing = TRUE)
  for (i in ord) {
    cap <- upper[i] - p[i]
    add <- min(residual, cap)
    p[i] <- p[i] + add
    residual <- residual - add
    if (residual <= eps) break
  }

  renorm_simplex(p, eps = eps)
}

entropy_interval_from_bounds <- function(lower, upper, baseN, eps = EPS) {
  repaired <- repair_interval_bounds(lower, upper, total = 1, eps = eps)
  p_min <- entropy_min_vector(repaired$lower, repaired$upper, total = 1, eps = eps)
  p_max <- entropy_max_vector(repaired$lower, repaired$upper, total = 1, eps = eps)

  H_lower_raw <- entropy_base2(p_min, eps = eps)
  H_upper_raw <- entropy_base2(p_max, eps = eps)

  if (baseN <= 1) {
    h_lower <- 0
    h_upper <- 0
  } else {
    h_lower <- H_lower_raw / log2(baseN)
    h_upper <- H_upper_raw / log2(baseN)
  }

  list(
    p_min = as_num_vec(p_min),
    p_max = as_num_vec(p_max),
    H_lower_raw = H_lower_raw,
    H_upper_raw = H_upper_raw,
    h_lower = h_lower,
    h_upper = h_upper
  )
}

alpha_cut_bounds <- function(central, delta, alpha, eps = EPS) {
  central <- as.numeric(central)
  delta <- pmax(as.numeric(delta), 0)
  spread <- (1 - alpha) * delta
  lower <- central - spread
  upper <- central + spread
  upper <- pmax(upper, lower)
  list(lower = lower, upper = upper)
}

canonical_preference_family <- function(func) {
  f <- tolower(trimws(as.character(func %||% "linear")))
  f <- gsub("_", "-", f)
  f <- gsub("[[:space:]]+", "-", f)
  if (f %in% c("ushape", "u-shape")) return("u-shape")
  if (f %in% c("vshape", "v-shape")) return("v-shape")
  if (f %in% c("gauss", "gaussian")) return("gaussian")
  if (f %in% c("usual", "level", "linear")) return(f)
  "linear"
}

preference_family_choices <- function() {
  c(
    "Usual" = "usual",
    "U-shape" = "u-shape",
    "V-shape" = "v-shape",
    "Level" = "level",
    "Linear" = "linear",
    "Gaussian" = "gaussian"
  )
}

preference_function_vec <- function(d, func = "linear", q = 0, p = 0.1, s = 0.2, eps = EPS) {
  d <- as_num_vec(d, allow_empty = TRUE)
  out <- numeric(length(d))
  if (length(d) == 0) return(out)
  f <- canonical_preference_family(func)
  q <- suppressWarnings(as.numeric(q)[1]); if (!is.finite(q)) q <- 0
  p <- suppressWarnings(as.numeric(p)[1]); if (!is.finite(p)) p <- max(q + 1e-6, 0.1)
  s <- suppressWarnings(as.numeric(s)[1]); if (!is.finite(s)) s <- 0.2
  q <- max(q, 0)
  p <- max(p, q + 1e-6)
  s <- max(s, eps)

  if (f == "usual") {
    out[d <= 0] <- 0
    out[d > 0] <- 1
  } else if (f == "u-shape") {
    out[d <= q] <- 0
    out[d > q] <- 1
  } else if (f == "v-shape") {
    out[d <= 0] <- 0
    idx_mid <- d > 0 & d < p
    out[idx_mid] <- d[idx_mid] / p
    out[d >= p] <- 1
  } else if (f == "level") {
    out[d <= q] <- 0
    out[d > q & d <= p] <- 0.5
    out[d > p] <- 1
  } else if (f == "gaussian") {
    out[d <= 0] <- 0
    idx <- d > 0
    out[idx] <- 1 - exp(-(d[idx]^2) / (2 * s^2))
  } else {
    out[d <= q] <- 0
    idx_mid <- d > q & d < p
    out[idx_mid] <- (d[idx_mid] - q) / (p - q)
    out[d >= p] <- 1
  }
  clamp01(out)
}

read_sheet_safe <- function(path, sheet_name) {
  out <- NULL
  tryCatch({
    out <- as.data.frame(read_excel(path, sheet = sheet_name), stringsAsFactors = FALSE)
    names(out) <- trimws(names(out))
  }, error = function(e) {
    out <<- NULL
  })
  out
}

extract_single_alpha <- function(df) {
  if (is.null(df)) return(NULL)
  vals <- c()
  if (!is.null(names(df))) vals <- c(vals, names(df))
  if (nrow(df) > 0 || ncol(df) > 0) vals <- c(vals, unlist(df, use.names = FALSE))
  nums <- suppressWarnings(as.numeric(vals))
  nums <- nums[is.finite(nums)]
  if (length(nums) == 0) return(NULL)
  nums[1]
}

is_blank_cell <- function(x) is.na(x) | trimws(as.character(x)) == ""

drop_empty_rows_cols <- function(df) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(df)
  keep_rows <- !apply(df, 1, function(r) all(is_blank_cell(r)))
  keep_cols <- !vapply(df, function(col) all(is_blank_cell(col)), logical(1))
  df[keep_rows, keep_cols, drop = FALSE]
}


promote_first_data_row_to_header <- function(df) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(df)
  header <- trimws(as.character(unlist(df[1, , drop = TRUE], use.names = FALSE)))
  header[is.na(header)] <- ""
  if (sum(header != "") < 2) return(df)
  names(df) <- make.unique(header, sep = "_")
  if (nrow(df) == 1) {
    return(df[0, , drop = FALSE])
  }
  df[-1, , drop = FALSE]
}

ensure_expected_headers <- function(df, expected, allow_generic_criterion = FALSE) {
  df <- drop_empty_rows_cols(df)
  if (is.null(df) || ncol(df) == 0) return(df)

  current_names <- trimws(names(df))
  if (all(expected %in% current_names)) return(df)

  if (nrow(df) > 0) {
    first_row <- trimws(as.character(unlist(df[1, , drop = TRUE], use.names = FALSE)))
    first_row[is.na(first_row)] <- ""
    cond_expected <- all(expected %in% first_row)
    cond_generic <- isTRUE(allow_generic_criterion) && length(first_row) >= 2 && identical(first_row[1], "Criterion")
    if (cond_expected || cond_generic) {
      df <- promote_first_data_row_to_header(df)
    }
  }

  df
}

clean_matrix_sheet <- function(df, sheet_name) {
  df <- ensure_expected_headers(df, expected = "Criterion", allow_generic_criterion = TRUE)
  df <- drop_empty_rows_cols(df)
  if (is.null(df) || ncol(df) < 3) {
    stop(paste0(sheet_name, " must contain one criterion column and at least two alternative columns."))
  }

  crit <- trimws(as.character(df[[1]]))
  num_df <- as.data.frame(lapply(df[, -1, drop = FALSE], function(col) suppressWarnings(as.numeric(col))),
                          check.names = FALSE, stringsAsFactors = FALSE)

  keep <- !(is.na(crit) | crit == "") & rowSums(!is.na(num_df)) > 0
  df2 <- data.frame(Criterion = crit[keep], num_df[keep, , drop = FALSE],
                    check.names = FALSE, stringsAsFactors = FALSE)

  if (nrow(df2) < 2) stop(paste0(sheet_name, " must contain at least two valid criteria rows."))
  if (anyDuplicated(df2$Criterion)) stop(paste0(sheet_name, " contains duplicate criterion names."))
  if (anyNA(as.matrix(df2[, -1, drop = FALSE]))) {
    stop(paste0(sheet_name, " contains non-numeric or empty values in the alternatives block."))
  }

  df2
}

clean_subjective_weights_sheet <- function(df) {
  df <- ensure_expected_headers(df, expected = c("Criterion", "Central", "Delta"))
  df <- drop_empty_rows_cols(df)
  need <- c("Criterion", "Central", "Delta")
  if (!all(need %in% names(df))) stop("FuzzySubjectiveWeights must contain: Criterion, Central, Delta.")

  crit <- trimws(as.character(df$Criterion))
  central <- suppressWarnings(as.numeric(df$Central))
  delta <- suppressWarnings(as.numeric(df$Delta))
  delta[!is.finite(delta)] <- 0

  keep <- !(is.na(crit) | crit == "") & is.finite(central)
  out <- data.frame(Criterion = crit[keep], Central = central[keep], Delta = pmax(delta[keep], 0),
                    stringsAsFactors = FALSE)

  if (nrow(out) < 2) stop("FuzzySubjectiveWeights must contain at least two valid rows.")
  if (anyDuplicated(out$Criterion)) stop("FuzzySubjectiveWeights contains duplicate criterion names.")
  out
}

clean_preference_sheet <- function(df) {
  df <- ensure_expected_headers(df, expected = c("Criterion", "PreferenceFunction", "Threshold_q", "Threshold_p", "Threshold_s"))
  df <- drop_empty_rows_cols(df)
  need <- c("Criterion", "PreferenceFunction", "Threshold_q", "Threshold_p", "Threshold_s")
  if (!all(need %in% names(df))) {
    stop("PreferenceParams must contain: Criterion, PreferenceFunction, Threshold_q, Threshold_p, Threshold_s.")
  }

  crit <- trimws(as.character(df$Criterion))
  func <- trimws(as.character(df$PreferenceFunction))
  q <- suppressWarnings(as.numeric(df$Threshold_q))
  p <- suppressWarnings(as.numeric(df$Threshold_p))
  s <- suppressWarnings(as.numeric(df$Threshold_s))
  q[!is.finite(q)] <- 0
  p[!is.finite(p)] <- 0
  s[!is.finite(s)] <- 0

  keep <- !(is.na(crit) | crit == "") & !(is.na(func) | func == "")
  out <- data.frame(
    Criterion = crit[keep],
    PreferenceFunction = func[keep],
    Threshold_q = q[keep],
    Threshold_p = p[keep],
    Threshold_s = s[keep],
    stringsAsFactors = FALSE
  )

  if (nrow(out) < 2) stop("PreferenceParams must contain at least two valid rows.")
  if (anyDuplicated(out$Criterion)) stop("PreferenceParams contains duplicate criterion names.")

  out$PreferenceFunction <- vapply(out$PreferenceFunction, canonical_preference_family, character(1))
  allowed_pref <- unname(preference_family_choices())
  bad_pref <- setdiff(unique(out$PreferenceFunction), allowed_pref)
  if (length(bad_pref) > 0) {
    stop("PreferenceParams contains unsupported PreferenceFunction values.")
  }
  bad_qp <- out$PreferenceFunction %in% c("u-shape", "level", "linear") & out$Threshold_q < 0
  if (any(bad_qp, na.rm = TRUE)) stop("Threshold_q must be nonnegative for U-shape, Level, and Linear families.")
  bad_p <- out$PreferenceFunction %in% c("v-shape", "level", "linear") & out$Threshold_p <= 0
  if (any(bad_p, na.rm = TRUE)) stop("Threshold_p must be strictly positive for V-shape, Level, and Linear families.")
  bad_qp_order <- out$PreferenceFunction %in% c("level", "linear") & !(out$Threshold_q < out$Threshold_p)
  if (any(bad_qp_order, na.rm = TRUE)) stop("PreferenceParams require Threshold_q < Threshold_p for Level and Linear families.")
  bad_s <- out$PreferenceFunction %in% c("gaussian") & out$Threshold_s <= 0
  if (any(bad_s, na.rm = TRUE)) stop("Threshold_s must be strictly positive for the Gaussian family.")
  out
}

clean_benefit_cost_sheet <- function(df) {
  df <- ensure_expected_headers(df, expected = c("Criterion", "Type"))
  df <- drop_empty_rows_cols(df)
  need <- c("Criterion", "Type")
  if (!all(need %in% names(df))) stop("BenefitCost must contain: Criterion and Type.")

  crit <- trimws(as.character(df$Criterion))
  type <- tolower(trimws(as.character(df$Type)))
  keep <- !(is.na(crit) | crit == "") & !(is.na(type) | type == "")
  out <- data.frame(Criterion = crit[keep], Type = type[keep], stringsAsFactors = FALSE)

  if (nrow(out) < 2) stop("BenefitCost must contain at least two valid rows.")
  if (anyDuplicated(out$Criterion)) stop("BenefitCost contains duplicate criterion names.")
  bad_types <- setdiff(unique(out$Type), c("benefit", "cost"))
  if (length(bad_types) > 0) stop("BenefitCost Type values must be only 'benefit' or 'cost'.")
  out
}

validate_bundle <- function(dataMat, deltaMat, sbjDF, prefDF, bcDF) {
  if (is.null(dataMat) || is.null(deltaMat)) stop("DataMatrix and FuzzyDeviations are required.")
  if (!identical(dim(dataMat), dim(deltaMat))) stop("DataMatrix and FuzzyDeviations must have identical dimensions.")
  if (!identical(rownames(dataMat), rownames(deltaMat))) stop("Criteria names must match between DataMatrix and FuzzyDeviations.")
  if (!identical(colnames(dataMat), colnames(deltaMat))) stop("Alternative names must match between DataMatrix and FuzzyDeviations.")
  if (nrow(dataMat) < 2) stop("At least two criteria are required.")
  if (ncol(dataMat) < 2) stop("At least two alternatives are required.")
  if (any(is.na(rownames(dataMat)) | trimws(rownames(dataMat)) == "")) stop("Criteria names cannot be blank.")
  if (any(is.na(colnames(dataMat)) | trimws(colnames(dataMat)) == "")) stop("Alternative names cannot be blank.")
  if (any(!is.finite(dataMat))) stop("DataMatrix contains non-numeric or missing values.")
  if (any(!is.finite(deltaMat))) stop("FuzzyDeviations contains non-numeric or missing values.")
  if (any(deltaMat < 0, na.rm = TRUE)) stop("FuzzyDeviations cannot contain negative values.")

  need_sbj <- c("Criterion", "Central", "Delta")
  need_pref <- c("Criterion", "PreferenceFunction", "Threshold_q", "Threshold_p", "Threshold_s")
  need_bc <- c("Criterion", "Type")

  if (!all(need_sbj %in% names(sbjDF))) stop("FuzzySubjectiveWeights must contain: Criterion, Central, Delta.")
  if (!all(need_pref %in% names(prefDF))) stop("PreferenceParams must contain: Criterion, PreferenceFunction, Threshold_q, Threshold_p, Threshold_s.")
  if (!all(need_bc %in% names(bcDF))) stop("BenefitCost must contain: Criterion and Type.")

  critNames <- rownames(dataMat)
  miss_sbj <- setdiff(critNames, sbjDF$Criterion)
  miss_bc <- setdiff(critNames, bcDF$Criterion)
  if (length(miss_sbj) > 0) stop(paste("Missing subjective weights for:", paste(miss_sbj, collapse = ", ")))
  if (length(miss_bc) > 0) stop(paste("Missing Benefit/Cost type for:", paste(miss_bc, collapse = ", ")))
  TRUE
}

build_model_bundle <- function(dfMatrix, dfDelta, dfSbj, dfPref, dfBC) {
  dfMatrix <- clean_matrix_sheet(dfMatrix, "DataMatrix")
  dfDelta <- clean_matrix_sheet(dfDelta, "FuzzyDeviations")
  dfSbj <- clean_subjective_weights_sheet(dfSbj)
  dfPref <- clean_preference_sheet(dfPref)
  dfBC <- clean_benefit_cost_sheet(dfBC)

  if (!identical(names(dfMatrix), names(dfDelta))) stop("DataMatrix and FuzzyDeviations must have identical alternative columns.")

  critNames <- trimws(as.character(dfMatrix[[1]]))
  altNames <- trimws(names(dfMatrix)[-1])

  dataMat <- as.matrix(dfMatrix[, -1, drop = FALSE])
  deltaMat <- as.matrix(dfDelta[, -1, drop = FALSE])

  storage.mode(dataMat) <- "numeric"
  storage.mode(deltaMat) <- "numeric"

  rownames(dataMat) <- critNames
  colnames(dataMat) <- altNames
  rownames(deltaMat) <- trimws(as.character(dfDelta[[1]]))
  colnames(deltaMat) <- trimws(names(dfDelta)[-1])

  list(dataMat = dataMat, deltaMat = deltaMat, sbjDF = dfSbj, prefDF = dfPref, bcDF = dfBC)
}

normalize_fuzzy_matrix <- function(dataMat, deltaMat, critTypes, alpha, eps = EPS) {
  dataMat <- as.matrix(dataMat)
  deltaMat <- as.matrix(deltaMat)
  M <- nrow(dataMat)
  N <- ncol(dataMat)
  if (!is.finite(M) || !is.finite(N) || M < 1 || N < 1) stop("Invalid DataMatrix dimensions after import.")

  xi_lower <- matrix(0, nrow = M, ncol = N, dimnames = dimnames(dataMat))
  xi_upper <- matrix(0, nrow = M, ncol = N, dimnames = dimnames(dataMat))
  rho_lower <- matrix(0, nrow = M, ncol = N, dimnames = dimnames(dataMat))
  rho_upper <- matrix(0, nrow = M, ncol = N, dimnames = dimnames(dataMat))
  shifts <- numeric(M)
  names(shifts) <- rownames(dataMat)

  for (i in seq_len(M)) {
    acut <- alpha_cut_bounds(dataMat[i, ], deltaMat[i, ], alpha, eps = eps)
    rawL <- as.numeric(acut$lower)
    rawU <- as.numeric(acut$upper)

    minLower <- min(rawL, na.rm = TRUE)
    shiftVal <- if (is.finite(minLower) && minLower <= 0) abs(minLower) + eps else 0
    shifts[i] <- shiftVal

    rawL <- pmax(rawL + shiftVal, eps)
    rawU <- pmax(rawU + shiftVal, rawL + eps)

    xi_lower[i, ] <- rawL
    xi_upper[i, ] <- rawU

    type_i <- tolower(trimws(as.character(critTypes[i])))
    if (!(type_i %in% c("benefit", "cost"))) type_i <- "benefit"

    if (type_i == "cost") {
      minLowerCost <- min(rawL, na.rm = TRUE)
      rho_lower[i, ] <- safe_div(minLowerCost, rawU, eps = eps)
      rho_upper[i, ] <- safe_div(minLowerCost, rawL, eps = eps)
    } else {
      maxUpper <- max(rawU, na.rm = TRUE)
      rho_lower[i, ] <- safe_div(rawL, maxUpper, eps = eps)
      rho_upper[i, ] <- safe_div(rawU, maxUpper, eps = eps)
    }
  }

  rho_lower <- clamp01(rho_lower)
  rho_upper <- pmax_preserve(rho_upper, rho_lower)
  rho_upper <- clamp01(rho_upper)

  list(xi_lower = xi_lower, xi_upper = xi_upper, rho_lower = rho_lower, rho_upper = rho_upper, shifts = shifts)
}

get_pref_row <- function(prefDF, criterion) {
  row <- prefDF[prefDF$Criterion == criterion, , drop = FALSE]
  if (nrow(row) == 0) return(list(func = "linear", q = 0, p = 0.1, s = 0.2))
  qv <- suppressWarnings(as.numeric(row$Threshold_q[1]))
  pv <- suppressWarnings(as.numeric(row$Threshold_p[1]))
  sv <- suppressWarnings(as.numeric(row$Threshold_s[1]))
  if (!is.finite(qv)) qv <- 0
  if (!is.finite(pv)) pv <- 0.1
  if (!is.finite(sv)) sv <- 0.2
  func <- canonical_preference_family(row$PreferenceFunction[1])
  if (func %in% c("level", "linear") && !(qv < pv)) pv <- max(qv + 1e-6, 0.1)
  if (func == "u-shape") qv <- max(qv, 0)
  if (func == "v-shape") pv <- max(pv, 1e-6)
  if (func == "gaussian") sv <- max(sv, 1e-6)
  list(func = func, q = qv, p = pv, s = sv)
}

preference_enhanced_probabilities <- function(rho_lower, rho_upper, prefDF, eps = EPS) {
  rho_lower <- as.matrix(rho_lower)
  rho_upper <- as.matrix(rho_upper)

  M <- nrow(rho_lower)
  N <- ncol(rho_lower)

  if (!is.finite(M) || !is.finite(N) || M < 1 || N < 1) {
    stop("Invalid normalized performance matrix dimensions before Step 3. Please re-check the imported workbook.")
  }
  if (any(!is.finite(rho_lower)) || any(!is.finite(rho_upper))) {
    stop("Non-finite normalized values detected before Step 3.")
  }

  F_lower <- matrix(0, nrow = M, ncol = N, dimnames = dimnames(rho_lower))
  F_upper <- matrix(0, nrow = M, ncol = N, dimnames = dimnames(rho_lower))
  S_lower <- matrix(0, nrow = M, ncol = N, dimnames = dimnames(rho_lower))
  S_upper <- matrix(0, nrow = M, ncol = N, dimnames = dimnames(rho_lower))
  Pi_lower <- matrix(0, nrow = M, ncol = N, dimnames = dimnames(rho_lower))
  Pi_upper <- matrix(0, nrow = M, ncol = N, dimnames = dimnames(rho_lower))

  for (i in seq_len(M)) {
    crit <- rownames(rho_lower)[i]
    par <- get_pref_row(prefDF, crit)

    if (N == 1) {
      F_lower[i, 1] <- 1
      F_upper[i, 1] <- 1
      S_lower[i, 1] <- rho_lower[i, 1]
      S_upper[i, 1] <- rho_upper[i, 1]
      Pi_lower[i, 1] <- 1
      Pi_upper[i, 1] <- 1
      next
    }

    aggL <- numeric(N)
    aggU <- numeric(N)

    for (v in seq_len(N)) {
      valsL <- numeric(0)
      valsU <- numeric(0)

      for (vp in seq_len(N)) {
        if (v == vp) next
        dL <- rho_lower[i, v] - rho_upper[i, vp]
        dU <- rho_upper[i, v] - rho_lower[i, vp]
        valsL <- c(valsL, preference_function_vec(dL, par$func, par$q, par$p, par$s, eps = eps))
        valsU <- c(valsU, preference_function_vec(dU, par$func, par$q, par$p, par$s, eps = eps))
      }

      aggL[v] <- mean(valsL)
      aggU[v] <- mean(valsU)
    }

    F_lower[i, ] <- aggL
    F_upper[i, ] <- aggU

    S_lower[i, ] <- rho_lower[i, ] * F_lower[i, ]
    S_upper[i, ] <- rho_upper[i, ] * F_upper[i, ]

    if (all(S_lower[i, ] <= eps) && all(S_upper[i, ] <= eps)) {
      Pi_lower[i, ] <- rep(1 / N, N)
      Pi_upper[i, ] <- rep(1 / N, N)
    } else {
      piInt <- interval_simplex_bounds(S_lower[i, ], S_upper[i, ], eps = eps)
      Pi_lower[i, ] <- clamp01(piInt$lower)
      Pi_upper[i, ] <- clamp01(pmax(piInt$upper, Pi_lower[i, ]))
    }
  }

  list(
    F_lower = F_lower,
    F_upper = F_upper,
    S_lower = S_lower,
    S_upper = S_upper,
    Pi_lower = Pi_lower,
    Pi_upper = Pi_upper
  )
}

compute_esmadmiii <- function(dataMat, deltaMat, sbjDF, prefDF, bcDF, alpha = 0.5, eps = EPS) {
  validate_bundle(dataMat, deltaMat, sbjDF, prefDF, bcDF)

  alpha_num <- suppressWarnings(as.numeric(alpha)[1])
  if (!is.finite(alpha_num)) alpha_num <- 0.5
  alpha <- min(max(alpha_num, 0), 1)

  critNames <- rownames(dataMat)
  altNames <- colnames(dataMat)
  M <- nrow(dataMat)
  N <- ncol(dataMat)
  logN <- if (N > 1) log2(N) else 0

  bc_map <- setNames(tolower(trimws(as.character(bcDF$Type))), as.character(bcDF$Criterion))
  critTypes <- bc_map[critNames]
  critTypes[is.na(critTypes)] <- "benefit"

  normRes <- normalize_fuzzy_matrix(dataMat, deltaMat, critTypes, alpha, eps = eps)
  prefRes <- preference_enhanced_probabilities(normRes$rho_lower, normRes$rho_upper, prefDF, eps = eps)

  # Interval-level criterion entropy and conservative weight enclosures.
  # These quantities remain available for uncertainty auditing, but they are
  # not used as the final operational representative weights.
  h_lower <- h_upper <- d_lower <- d_upper <- numeric(M)
  p_entropy_min <- matrix(0, nrow = M, ncol = N, dimnames = dimnames(dataMat))
  p_entropy_max <- matrix(0, nrow = M, ncol = N, dimnames = dimnames(dataMat))

  for (i in seq_len(M)) {
    eInt <- entropy_interval_from_bounds(prefRes$Pi_lower[i, ], prefRes$Pi_upper[i, ], baseN = N, eps = eps)
    h_lower[i] <- max(0, min(1, eInt$h_lower))
    h_upper[i] <- max(h_lower[i], min(1, eInt$h_upper))
    d_lower[i] <- max(0, 1 - h_upper[i])
    d_upper[i] <- max(d_lower[i], 1 - h_lower[i])
    p_entropy_min[i, ] <- eInt$p_min
    p_entropy_max[i, ] <- eInt$p_max
  }
  names(h_lower) <- names(h_upper) <- names(d_lower) <- names(d_upper) <- critNames

  objInt <- interval_simplex_bounds(d_lower, d_upper, eps = eps)
  xOBJ_lower <- as_num_vec(objInt$lower)
  xOBJ_upper <- as_num_vec(objInt$upper)
  names(xOBJ_lower) <- names(xOBJ_upper) <- critNames

  sbjRow <- sbjDF[match(critNames, sbjDF$Criterion), , drop = FALSE]
  central <- suppressWarnings(as.numeric(sbjRow$Central))
  delta <- suppressWarnings(as.numeric(sbjRow$Delta))
  central[!is.finite(central)] <- 1 / M
  delta[!is.finite(delta)] <- 0

  sbjAcut <- alpha_cut_bounds(central, delta, alpha, eps = eps)
  xSBJ_lower <- pmax(as_num_vec(sbjAcut$lower), 0)
  xSBJ_upper <- pmax(as_num_vec(sbjAcut$upper), xSBJ_lower)
  if (sum(xSBJ_upper) <= eps) xSBJ_upper <- rep(1, M)
  xSBJ <- renorm_simplex(mid_interval(xSBJ_lower, xSBJ_upper), eps = eps)
  names(xSBJ_lower) <- names(xSBJ_upper) <- names(xSBJ) <- critNames

  # Conservative integrated-weight interval enclosure retained for audit.
  g_lower <- pmax(xSBJ_lower * xOBJ_lower, 0)
  g_upper <- pmax(xSBJ_upper * xOBJ_upper, g_lower)
  intInt <- interval_simplex_bounds(g_lower, g_upper, eps = eps)
  xINT_lower <- as_num_vec(intInt$lower)
  xINT_upper <- as_num_vec(intInt$upper)
  names(xINT_lower) <- names(xINT_upper) <- critNames

  # Feasible representative preference-enhanced conditional probabilities.
  Pi_mid <- (prefRes$Pi_lower + prefRes$Pi_upper) / 2
  P_star <- matrix(0, nrow = M, ncol = N, dimnames = dimnames(dataMat))
  for (i in seq_len(M)) {
    P_star[i, ] <- project_box_simplex(
      Pi_mid[i, ],
      prefRes$Pi_lower[i, ],
      prefRes$Pi_upper[i, ],
      total = 1,
      eps = eps
    )
  }

  # V7 correction: information-sensitive representative weights.
  # Objective weights are derived from the operational diversification degrees
  # induced by P_star, not from the midpoint of conservative intervals.
  h_star <- numeric(M)
  for (i in seq_len(M)) {
    h_star[i] <- if (N <= 1 || logN <= eps) 0 else entropy_base2(P_star[i, ], eps = eps) / logN
  }
  h_star <- pmin(1, pmax(0, h_star))
  d_star <- pmax(0, 1 - h_star)
  names(h_star) <- names(d_star) <- critNames

  if (sum(d_star) <= eps) {
    xOBJ <- rep(1 / M, M)
    warning("Total operational diversification is zero; representative objective weights were set uniformly.")
  } else {
    xOBJ <- renorm_simplex(d_star, eps = eps)
  }
  names(xOBJ) <- critNames

  g_star <- pmax(xSBJ * xOBJ, 0)
  if (sum(g_star) <= eps) {
    xINT_star <- rep(1 / M, M)
    warning("Integrated representative weighting mass is zero; integrated weights were set uniformly.")
  } else {
    xINT_star <- renorm_simplex(g_star, eps = eps)
  }
  names(xINT_star) <- critNames

  scores <- as.numeric(xINT_star %*% P_star)
  names(scores) <- altNames
  scores <- renorm_simplex(scores, eps = eps)

  ICI <- xINT_star * d_star
  names(ICI) <- critNames

  S_Y <- entropy_base2(scores, eps = eps)
  S_X <- entropy_base2(xINT_star, eps = eps)
  S_Y_given_X <- sum(xINT_star * h_star * logN)
  S_XY <- S_X + S_Y_given_X
  J_XY <- max(0, S_Y - S_Y_given_X)

  NMI <- if (S_Y <= eps) 1 else as.numeric(clamp01(J_XY / S_Y))
  CES_from_ici <- as.numeric(clamp01(sum(ICI)))
  CES_from_entropy <- if (logN <= eps) 0 else as.numeric(clamp01(1 - S_Y_given_X / logN))
  CES <- CES_from_ici
  if (abs(CES_from_ici - CES_from_entropy) > 1e-8) {
    warning("Crisp CES from ICI and from the entropy identity differ by more than tolerance.")
  }
  ADI <- if (logN <= eps) 0 else as.numeric(clamp01(1 - S_Y / logN))

  altRank <- rank(-scores, ties.method = "min")
  names(altRank) <- altNames

  criteria_table <- data.frame(
    Criterion = critNames,
    Type = critTypes,
    xSBJ_lower = xSBJ_lower,
    xSBJ_upper = xSBJ_upper,
    xSBJ_star = xSBJ,
    xOBJ_lower = xOBJ_lower,
    xOBJ_upper = xOBJ_upper,
    xOBJ_star = xOBJ,
    xINT_lower = xINT_lower,
    xINT_upper = xINT_upper,
    xINT_star = xINT_star,
    h_lower = h_lower,
    h_upper = h_upper,
    h = h_star,
    d_lower = d_lower,
    d_upper = d_upper,
    d = d_star,
    ICI = ICI,
    stringsAsFactors = FALSE
  )

  alternatives_table <- data.frame(
    Alternative = altNames,
    Score = scores,
    Rank = altRank,
    stringsAsFactors = FALSE
  )
  alternatives_table <- alternatives_table[order(alternatives_table$Rank, alternatives_table$Alternative), ]

  diagnostics_table <- data.frame(
    Measure = c(
      "S(Y) - Entropy of alternatives",
      "S(X) - Entropy of criteria",
      "S(Y|X) - Conditional entropy",
      "S(X,Y) - Joint entropy",
      "J(X;Y) - Mutual information",
      "NMI",
      "CES",
      "ADI"
    ),
    Value = c(S_Y, S_X, S_Y_given_X, S_XY, J_XY, NMI, CES, ADI),
    stringsAsFactors = FALSE
  )

  representative_prob_table <- data.frame(Criterion = critNames, stringsAsFactors = FALSE, check.names = FALSE)
  for (j in seq_len(N)) representative_prob_table[[altNames[j]]] <- P_star[, j]

  representative_weight_table <- data.frame(
    Criterion = critNames,
    xSBJ_star = xSBJ,
    xOBJ_star = xOBJ,
    xINT_star = xINT_star,
    h = h_star,
    d = d_star,
    ICI = ICI,
    stringsAsFactors = FALSE
  )

  representative_within_interval <-
    all(P_star >= prefRes$Pi_lower - 1e-10 & P_star <= prefRes$Pi_upper + 1e-10) &&
    all(xOBJ >= xOBJ_lower - 1e-10 & xOBJ <= xOBJ_upper + 1e-10) &&
    all(xINT_star >= xINT_lower - 1e-10 & xINT_star <= xINT_upper + 1e-10)

  validation_flags <- data.frame(
    Check = c(
      "sum(xSBJ_star)=1",
      "sum(xOBJ_star)=1",
      "sum(xINT_star)=1",
      "each row of representative probabilities sums to 1",
      "sum(scores)=1",
      "CES identity holds",
      "representatives respect conservative interval enclosures"
    ),
    Result = c(
      abs(sum(xSBJ) - 1) <= 1e-8,
      abs(sum(xOBJ) - 1) <= 1e-8,
      abs(sum(xINT_star) - 1) <= 1e-8,
      all(abs(rowSums(P_star) - 1) <= 1e-8),
      abs(sum(scores) - 1) <= 1e-8,
      abs(CES_from_ici - CES_from_entropy) <= 1e-8,
      representative_within_interval
    ),
    stringsAsFactors = FALSE
  )

  summary_table <- data.frame(
    Item = c("Alpha", "Top Alternative", "Top Score", "NMI", "CES", "ADI"),
    Value = c(
      sprintf("%.6f", alpha),
      as.character(alternatives_table$Alternative[1]),
      sprintf("%.12f", alternatives_table$Score[1]),
      sprintf("%.12f", NMI),
      sprintf("%.12f", CES),
      sprintf("%.12f", ADI)
    ),
    stringsAsFactors = FALSE
  )

  list(
    alpha = alpha, M = M, N = N, logN = logN,
    critNames = critNames, altNames = altNames, critTypes = critTypes,
    shifts = normRes$shifts,
    xi_lower = normRes$xi_lower, xi_upper = normRes$xi_upper,
    rho_lower = normRes$rho_lower, rho_upper = normRes$rho_upper,
    F_lower = prefRes$F_lower, F_upper = prefRes$F_upper,
    S_lower = prefRes$S_lower, S_upper = prefRes$S_upper,
    Pi_lower = prefRes$Pi_lower, Pi_upper = prefRes$Pi_upper,
    p_entropy_min = p_entropy_min, p_entropy_max = p_entropy_max,
    xSBJ_lower = xSBJ_lower, xSBJ_upper = xSBJ_upper, xSBJ = xSBJ,
    xOBJ_lower = xOBJ_lower, xOBJ_upper = xOBJ_upper, xOBJ = xOBJ,
    xINT_lower = xINT_lower, xINT_upper = xINT_upper, xINT = xINT_star,
    P_star = P_star,
    h_lower = h_lower, h_upper = h_upper, h = h_star,
    d_lower = d_lower, d_upper = d_upper, d = d_star,
    ICI = ICI,
    scores = scores, P = scores, rank = altRank,
    S_Y = S_Y, S_X = S_X, S_Y_given_X = S_Y_given_X, S_XY = S_XY, J_XY = J_XY,
    NMI = NMI, CES = CES, ADI = ADI,
    criteria_table = criteria_table, alternatives_table = alternatives_table, diagnostics_table = diagnostics_table,
    representative_prob_table = representative_prob_table, representative_weight_table = representative_weight_table,
    validation_flags = validation_flags, summary_table = summary_table
  )
}

scenario_relative_nmgi <- function(scenarios_df, eps = EPS) {
  req_cols <- c("Scenario", "NMI", "CES", "ADI")
  if (!all(req_cols %in% names(scenarios_df))) stop("Scenario table must contain Scenario, NMI, CES, and ADI.")
  df <- as.data.frame(scenarios_df[, req_cols, drop = FALSE], stringsAsFactors = FALSE)
  S <- nrow(df)
  if (S < 2) return(NULL)

  mat <- as.matrix(df[, c("NMI", "CES", "ADI"), drop = FALSE])
  storage.mode(mat) <- "numeric"
  mat[!is.finite(mat)] <- 0
  mat <- pmax(mat, 0)

  K <- ncol(mat)
  weights <- numeric(K)
  names(weights) <- colnames(mat)
  norm_denom <- if (S == 2) log2(2) else log2(S)

  for (k in seq_len(K)) {
    colk <- mat[, k]
    s <- sum(colk)
    if (s <= eps) {
      # V7 safeguard: a diagnostic index with zero total scenario mass carries
      # no scenario-discriminating information and receives zero diversification.
      weights[k] <- 0
      next
    }
    pk <- colk / s
    Ek <- -sum(ifelse(pk > eps, pk * log2(pk), 0)) / max(norm_denom, eps)
    weights[k] <- max(0, 1 - Ek)
  }

  if (sum(weights) <= eps) weights[] <- 1 / K else weights <- weights / sum(weights)
  df$NMGI <- as.numeric(mat %*% weights)
  list(weights = weights, scenario_table = df)
}

matrix_to_df <- function(mat, row_label = "Criterion") {
  mat <- as.matrix(mat)
  rn <- rownames(mat)
  if (is.null(rn)) rn <- paste0("Row_", seq_len(nrow(mat)))
  df <- data.frame(tmp = rn, stringsAsFactors = FALSE, check.names = FALSE)
  names(df)[1] <- row_label
  for (j in seq_len(ncol(mat))) {
    nm <- colnames(mat)[j] %||% paste0("V", j)
    df[[nm]] <- mat[, j]
  }
  df
}

interval_matrix_to_df <- function(lower, upper, row_label = "Criterion") {
  lower <- as.matrix(lower)
  upper <- as.matrix(upper)
  if (!identical(dim(lower), dim(upper))) stop("lower and upper interval matrices must have identical dimensions.")
  rn <- rownames(lower)
  if (is.null(rn)) rn <- paste0("Row_", seq_len(nrow(lower)))
  cn <- colnames(lower)
  if (is.null(cn)) cn <- paste0("V", seq_len(ncol(lower)))
  out <- data.frame(tmp = rn, stringsAsFactors = FALSE, check.names = FALSE)
  names(out)[1] <- row_label
  for (j in seq_len(ncol(lower))) {
    out[[paste0(cn[j], "_L")]] <- lower[, j]
    out[[paste0(cn[j], "_U")]] <- upper[, j]
  }
  out
}

scenario_summary_row <- function(name, res, source = "Manual", origin = "") {
  data.frame(
    Scenario = name,
    Source = source,
    Origin = origin,
    Alpha = res$alpha,
    Winner = res$alternatives_table$Alternative[1],
    WinnerScore = res$alternatives_table$Score[1],
    NMI = res$NMI,
    CES = res$CES,
    ADI = res$ADI,
    stringsAsFactors = FALSE
  )
}

make_unique_scenario_name <- function(name, existing_names) {
  nm <- trimws(name)
  if (!nzchar(nm)) nm <- "Imported_Scenario"
  if (!(nm %in% existing_names)) return(nm)
  base <- nm
  k <- 1
  repeat {
    candidate <- paste0(base, "_", k)
    if (!(candidate %in% existing_names)) return(candidate)
    k <- k + 1
  }
}

validate_scenario_compatibility <- function(baseline_bundle, scenario_bundle) {
  if (!identical(rownames(baseline_bundle$dataMat), rownames(scenario_bundle$dataMat))) {
    stop("Imported scenario criteria do not match the baseline case study.")
  }
  if (!identical(colnames(baseline_bundle$dataMat), colnames(scenario_bundle$dataMat))) {
    stop("Imported scenario alternatives do not match the baseline case study.")
  }
  TRUE
}

bundles_equivalent <- function(a, b, alpha_a = NULL, alpha_b = NULL, tol = 1e-10) {
  if (is.null(a) || is.null(b)) return(FALSE)
  same_alpha <- TRUE
  if (!is.null(alpha_a) && !is.null(alpha_b)) {
    same_alpha <- isTRUE(all.equal(as.numeric(alpha_a), as.numeric(alpha_b), tolerance = tol, check.attributes = FALSE))
  }
  same_matrix <- isTRUE(all.equal(a$dataMat, b$dataMat, tolerance = tol, check.attributes = TRUE))
  same_delta <- isTRUE(all.equal(a$deltaMat, b$deltaMat, tolerance = tol, check.attributes = TRUE))
  same_sbj <- isTRUE(all.equal(a$sbjDF, b$sbjDF, tolerance = tol, check.attributes = TRUE))
  same_pref <- isTRUE(all.equal(a$prefDF, b$prefDF, tolerance = tol, check.attributes = TRUE))
  same_bc <- isTRUE(all.equal(a$bcDF, b$bcDF, tolerance = tol, check.attributes = TRUE))
  same_alpha && same_matrix && same_delta && same_sbj && same_pref && same_bc
}

portfolio_summary_with_baseline <- function(base_res, portfolio_summary, portfolio_nmgi_obj = NULL) {
  base_row <- data.frame(
    Scenario = "Baseline",
    Source = "Baseline workbook",
    Origin = "",
    Alpha = base_res$alpha,
    Winner = base_res$alternatives_table$Alternative[1],
    WinnerScore = base_res$alternatives_table$Score[1],
    NMI = base_res$NMI,
    CES = base_res$CES,
    ADI = base_res$ADI,
    stringsAsFactors = FALSE
  )
  df <- if (!is.null(portfolio_summary) && nrow(portfolio_summary) > 0) {
    dplyr::bind_rows(base_row, portfolio_summary)
  } else {
    base_row
  }
  if (!is.null(portfolio_nmgi_obj) && !is.null(portfolio_nmgi_obj$scenario_table)) {
    df <- merge(df, portfolio_nmgi_obj$scenario_table[, c("Scenario", "NMGI"), drop = FALSE],
                by = "Scenario", all.x = TRUE, sort = FALSE)
    desired_order <- c("Baseline", if (!is.null(portfolio_summary) && nrow(portfolio_summary) > 0) portfolio_summary$Scenario else character(0))
    idx <- match(desired_order, df$Scenario, nomatch = 0)
    df <- df[idx[idx > 0], , drop = FALSE]
  }
  df
}

load_bundle_from_workbook <- function(path,
                                      sheet_matrix = "DataMatrix",
                                      sheet_delta = "FuzzyDeviations",
                                      sheet_weights = "FuzzySubjectiveWeights",
                                      sheet_pref = "PreferenceParams",
                                      sheet_bc = "BenefitCost",
                                      sheet_alpha = "GlobalAlpha",
                                      fallback_alpha = 0.50) {
  sheets <- readxl::excel_sheets(path)
  required <- c(sheet_matrix, sheet_delta, sheet_weights, sheet_pref, sheet_bc)
  missing <- required[!required %in% sheets]
  if (length(missing) > 0) stop(paste("Missing required sheets:", paste(missing, collapse = ", ")))

  dfMatrix <- read_sheet_safe(path, sheet_matrix)
  dfDelta <- read_sheet_safe(path, sheet_delta)
  dfSbj <- read_sheet_safe(path, sheet_weights)
  dfPref <- read_sheet_safe(path, sheet_pref)
  dfBC <- read_sheet_safe(path, sheet_bc)
  alpha_df <- if (sheet_alpha %in% sheets) read_sheet_safe(path, sheet_alpha) else NULL

  alpha_global <- extract_single_alpha(alpha_df)
  alpha_final <- if (is.null(alpha_global)) fallback_alpha else alpha_global
  alpha_final <- min(max(alpha_final, 0), 1)

  bundle <- build_model_bundle(dfMatrix, dfDelta, dfSbj, dfPref, dfBC)
  validate_bundle(bundle$dataMat, bundle$deltaMat, bundle$sbjDF, bundle$prefDF, bundle$bcDF)
  list(bundle = bundle, alpha = alpha_final)
}

build_export_list <- function(res, prefix = "Baseline") {
  out <- list()
  out[[paste0(prefix, "_Summary")]] <- res$summary_table
  out[[paste0(prefix, "_AlphaCuts")]] <- interval_matrix_to_df(res$xi_lower, res$xi_upper, row_label = "Criterion")
  out[[paste0(prefix, "_NormalizedR")]] <- interval_matrix_to_df(res$rho_lower, res$rho_upper, row_label = "Criterion")
  out[[paste0(prefix, "_PreferenceIntervals")]] <- interval_matrix_to_df(res$Pi_lower, res$Pi_upper, row_label = "Criterion")
  out[[paste0(prefix, "_Criteria")]] <- res$criteria_table
  out[[paste0(prefix, "_RepresentativeP")]] <- res$representative_prob_table
  out[[paste0(prefix, "_RepresentativeW")]] <- res$representative_weight_table
  out[[paste0(prefix, "_Alternatives")]] <- res$alternatives_table
  out[[paste0(prefix, "_Diagnostics")]] <- res$diagnostics_table
  out[[paste0(prefix, "_ValidationFlags")]] <- res$validation_flags
  out
}




es_madm_emblem_ui <- function() {
  HTML("<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 240 240' role='img' aria-label='ES-MADM III minimalist emblem'>
  <defs>
    <linearGradient id='g1m' x1='0' y1='0' x2='1' y2='1'>
      <stop offset='0%' stop-color='rgba(255,255,255,0.98)'/>
      <stop offset='100%' stop-color='rgba(197,222,245,0.92)'/>
    </linearGradient>
    <linearGradient id='g2m' x1='0' y1='0' x2='1' y2='1'>
      <stop offset='0%' stop-color='#16324C'/>
      <stop offset='100%' stop-color='#2E7DB7'/>
    </linearGradient>
  </defs>
  <circle cx='120' cy='120' r='88' fill='none' stroke='rgba(255,255,255,0.20)' stroke-width='2'/>
  <circle cx='120' cy='120' r='64' fill='none' stroke='rgba(255,255,255,0.26)' stroke-width='2'/>
  <circle cx='120' cy='120' r='48' fill='url(#g2m)' fill-opacity='0.12' stroke='url(#g1m)' stroke-width='2.6'/>
  <path d='M80 136C92 100 108 84 120 84C132 84 148 100 160 136' fill='none' stroke='rgba(255,255,255,0.86)' stroke-width='4' stroke-linecap='round'/>
  <path d='M86 122H154' fill='none' stroke='rgba(255,255,255,0.72)' stroke-width='2.6' stroke-linecap='round'/>
  <circle cx='86' cy='122' r='5.5' fill='#FFFFFF'/>
  <circle cx='120' cy='84' r='5.5' fill='#FFFFFF'/>
  <circle cx='154' cy='122' r='5.5' fill='#FFFFFF'/>
  <text x='120' y='174' text-anchor='middle' font-family='Arial, Helvetica, sans-serif' font-size='20' letter-spacing='4' font-weight='700' fill='rgba(255,255,255,0.92)'>III</text>
</svg>")
}

apply_preference_family_change <- function(prefDF, criterion_target = "__ALL__", family_value = "linear", q = NULL, p = NULL, s = NULL) {
  out <- prefDF
  idx <- if (identical(criterion_target, "__ALL__")) seq_len(nrow(out)) else which(out$Criterion == criterion_target)
  if (length(idx) == 0) stop("Preference sensitivity target criterion not found.")
  fam <- canonical_preference_family(family_value)
  out$PreferenceFunction[idx] <- fam
  if (!is.null(q)) out$Threshold_q[idx] <- q
  if (!is.null(p)) out$Threshold_p[idx] <- p
  if (!is.null(s)) out$Threshold_s[idx] <- s
  if (fam == "usual") {
    out$Threshold_q[idx] <- 0
    out$Threshold_p[idx] <- 0
    out$Threshold_s[idx] <- 0
  } else if (fam == "u-shape") {
    out$Threshold_q[idx] <- pmax(as.numeric(out$Threshold_q[idx]), 0)
    out$Threshold_p[idx] <- 0
    out$Threshold_s[idx] <- 0
  } else if (fam == "v-shape") {
    out$Threshold_q[idx] <- 0
    out$Threshold_p[idx] <- pmax(as.numeric(out$Threshold_p[idx]), 0.1)
    out$Threshold_s[idx] <- 0
  } else if (fam == "level") {
    q_now <- pmax(as.numeric(out$Threshold_q[idx]), 0)
    p_now <- pmax(as.numeric(out$Threshold_p[idx]), q_now + 0.1)
    out$Threshold_q[idx] <- q_now
    out$Threshold_p[idx] <- p_now
    out$Threshold_s[idx] <- 0
  } else if (fam == "linear") {
    q_now <- pmax(as.numeric(out$Threshold_q[idx]), 0)
    p_now <- pmax(as.numeric(out$Threshold_p[idx]), q_now + 0.1)
    out$Threshold_q[idx] <- q_now
    out$Threshold_p[idx] <- p_now
    out$Threshold_s[idx] <- 0
  } else if (fam == "gaussian") {
    out$Threshold_q[idx] <- 0
    out$Threshold_p[idx] <- 0
    out$Threshold_s[idx] <- pmax(as.numeric(out$Threshold_s[idx]), 0.2)
  }
  out$PreferenceFunction <- vapply(out$PreferenceFunction, canonical_preference_family, character(1))
  out
}

run_preference_sensitivity <- function(bundle, alpha, criterion_target = "__ALL__",
                                       mode = "family",
                                       family_choices = c("usual", "u-shape", "v-shape", "level", "linear", "gaussian"),
                                       family_single = "linear",
                                       q_from = 0.50, q_to = 1.50, q_by = 0.25,
                                       p_from = 0.50, p_to = 1.50, p_by = 0.25,
                                       s_from = 0.50, s_to = 1.50, s_by = 0.25,
                                       eps = EPS) {
  base_pref <- bundle$prefDF
  target_label <- if (identical(criterion_target, "__ALL__")) "All criteria" else criterion_target

  if (identical(mode, "family")) {
    fams <- vapply(family_choices, canonical_preference_family, character(1))
    fams <- unique(fams)
    rows <- list(); k <- 1L
    for (fam in fams) {
      pref_mod <- apply_preference_family_change(base_pref, criterion_target, fam)
      res <- compute_esmadmiii(bundle$dataMat, bundle$deltaMat, bundle$sbjDF, pref_mod, bundle$bcDF, alpha = alpha, eps = eps)
      rows[[k]] <- data.frame(
        Mode = "Family sweep",
        Target = target_label,
        Family = fam,
        Q_Value = if (identical(criterion_target, "__ALL__")) NA_real_ else as.numeric(pref_mod[pref_mod$Criterion == criterion_target, "Threshold_q"][1]),
        P_Value = if (identical(criterion_target, "__ALL__")) NA_real_ else as.numeric(pref_mod[pref_mod$Criterion == criterion_target, "Threshold_p"][1]),
        S_Value = if (identical(criterion_target, "__ALL__")) NA_real_ else as.numeric(pref_mod[pref_mod$Criterion == criterion_target, "Threshold_s"][1]),
        Winner = res$alternatives_table$Alternative[1],
        WinnerScore = res$alternatives_table$Score[1],
        NMI = res$NMI,
        CES = res$CES,
        ADI = res$ADI,
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
    df <- dplyr::bind_rows(rows)
    best_idx <- which.max(df$WinnerScore)[1]
    return(list(table = df, best = df[best_idx, , drop = FALSE], alpha = alpha, criterion_target = criterion_target, mode = "family"))
  }

  fam <- canonical_preference_family(family_single)
  q_vals <- seq(q_from, q_to, by = q_by)
  p_vals <- seq(p_from, p_to, by = p_by)
  s_vals <- seq(s_from, s_to, by = s_by)
  rows <- list(); k <- 1L

  if (fam %in% c("usual")) {
    pref_mod <- apply_preference_family_change(base_pref, criterion_target, fam)
    res <- compute_esmadmiii(bundle$dataMat, bundle$deltaMat, bundle$sbjDF, pref_mod, bundle$bcDF, alpha = alpha, eps = eps)
    rows[[k]] <- data.frame(Mode = "Parameter sweep", Target = target_label, Family = fam, Q_Multiplier = NA_real_, P_Multiplier = NA_real_, S_Multiplier = NA_real_, Q_Value = NA_real_, P_Value = NA_real_, S_Value = NA_real_, Winner = res$alternatives_table$Alternative[1], WinnerScore = res$alternatives_table$Score[1], NMI = res$NMI, CES = res$CES, ADI = res$ADI, stringsAsFactors = FALSE)
  } else if (fam %in% c("u-shape")) {
    if (length(q_vals) > 225) stop("Preference sensitivity grid is too large. Please reduce the q grid size.")
    for (qm in q_vals) {
      pref_mod <- base_pref
      idx <- if (identical(criterion_target, "__ALL__")) seq_len(nrow(pref_mod)) else which(pref_mod$Criterion == criterion_target)
      pref_mod$Threshold_q[idx] <- pmax(as.numeric(pref_mod$Threshold_q[idx]) * qm, 0)
      pref_mod <- apply_preference_family_change(pref_mod, criterion_target, fam)
      res <- compute_esmadmiii(bundle$dataMat, bundle$deltaMat, bundle$sbjDF, pref_mod, bundle$bcDF, alpha = alpha, eps = eps)
      rows[[k]] <- data.frame(Mode = "Parameter sweep", Target = target_label, Family = fam, Q_Multiplier = qm, P_Multiplier = NA_real_, S_Multiplier = NA_real_, Q_Value = if (identical(criterion_target, "__ALL__")) NA_real_ else as.numeric(pref_mod[pref_mod$Criterion == criterion_target, "Threshold_q"][1]), P_Value = NA_real_, S_Value = NA_real_, Winner = res$alternatives_table$Alternative[1], WinnerScore = res$alternatives_table$Score[1], NMI = res$NMI, CES = res$CES, ADI = res$ADI, stringsAsFactors = FALSE)
      k <- k + 1L
    }
  } else if (fam %in% c("v-shape")) {
    if (length(p_vals) > 225) stop("Preference sensitivity grid is too large. Please reduce the p grid size.")
    for (pm in p_vals) {
      pref_mod <- base_pref
      idx <- if (identical(criterion_target, "__ALL__")) seq_len(nrow(pref_mod)) else which(pref_mod$Criterion == criterion_target)
      pref_mod$Threshold_p[idx] <- pmax(as.numeric(pref_mod$Threshold_p[idx]) * pm, 1e-6)
      pref_mod <- apply_preference_family_change(pref_mod, criterion_target, fam)
      res <- compute_esmadmiii(bundle$dataMat, bundle$deltaMat, bundle$sbjDF, pref_mod, bundle$bcDF, alpha = alpha, eps = eps)
      rows[[k]] <- data.frame(Mode = "Parameter sweep", Target = target_label, Family = fam, Q_Multiplier = NA_real_, P_Multiplier = pm, S_Multiplier = NA_real_, Q_Value = NA_real_, P_Value = if (identical(criterion_target, "__ALL__")) NA_real_ else as.numeric(pref_mod[pref_mod$Criterion == criterion_target, "Threshold_p"][1]), S_Value = NA_real_, Winner = res$alternatives_table$Alternative[1], WinnerScore = res$alternatives_table$Score[1], NMI = res$NMI, CES = res$CES, ADI = res$ADI, stringsAsFactors = FALSE)
      k <- k + 1L
    }
  } else if (fam %in% c("gaussian")) {
    if (length(s_vals) > 225) stop("Preference sensitivity grid is too large. Please reduce the s grid size.")
    for (sm in s_vals) {
      pref_mod <- base_pref
      idx <- if (identical(criterion_target, "__ALL__")) seq_len(nrow(pref_mod)) else which(pref_mod$Criterion == criterion_target)
      pref_mod$Threshold_s[idx] <- pmax(as.numeric(pref_mod$Threshold_s[idx]) * sm, 1e-6)
      pref_mod <- apply_preference_family_change(pref_mod, criterion_target, fam)
      res <- compute_esmadmiii(bundle$dataMat, bundle$deltaMat, bundle$sbjDF, pref_mod, bundle$bcDF, alpha = alpha, eps = eps)
      rows[[k]] <- data.frame(Mode = "Parameter sweep", Target = target_label, Family = fam, Q_Multiplier = NA_real_, P_Multiplier = NA_real_, S_Multiplier = sm, Q_Value = NA_real_, P_Value = NA_real_, S_Value = if (identical(criterion_target, "__ALL__")) NA_real_ else as.numeric(pref_mod[pref_mod$Criterion == criterion_target, "Threshold_s"][1]), Winner = res$alternatives_table$Alternative[1], WinnerScore = res$alternatives_table$Score[1], NMI = res$NMI, CES = res$CES, ADI = res$ADI, stringsAsFactors = FALSE)
      k <- k + 1L
    }
  } else {
    if (length(q_vals) * length(p_vals) > 225) stop("Preference sensitivity grid is too large. Please reduce the q/p grid size.")
    for (qm in q_vals) {
      for (pm in p_vals) {
        pref_mod <- base_pref
        idx <- if (identical(criterion_target, "__ALL__")) seq_len(nrow(pref_mod)) else which(pref_mod$Criterion == criterion_target)
        new_q <- pmax(as.numeric(pref_mod$Threshold_q[idx]) * qm, 0)
        new_p <- pmax(as.numeric(pref_mod$Threshold_p[idx]) * pm, new_q + 1e-6)
        pref_mod$Threshold_q[idx] <- new_q
        pref_mod$Threshold_p[idx] <- new_p
        pref_mod <- apply_preference_family_change(pref_mod, criterion_target, fam)
        res <- compute_esmadmiii(bundle$dataMat, bundle$deltaMat, bundle$sbjDF, pref_mod, bundle$bcDF, alpha = alpha, eps = eps)
        rows[[k]] <- data.frame(Mode = "Parameter sweep", Target = target_label, Family = fam, Q_Multiplier = qm, P_Multiplier = pm, S_Multiplier = NA_real_, Q_Value = if (identical(criterion_target, "__ALL__")) NA_real_ else as.numeric(pref_mod[pref_mod$Criterion == criterion_target, "Threshold_q"][1]), P_Value = if (identical(criterion_target, "__ALL__")) NA_real_ else as.numeric(pref_mod[pref_mod$Criterion == criterion_target, "Threshold_p"][1]), S_Value = NA_real_, Winner = res$alternatives_table$Alternative[1], WinnerScore = res$alternatives_table$Score[1], NMI = res$NMI, CES = res$CES, ADI = res$ADI, stringsAsFactors = FALSE)
        k <- k + 1L
      }
    }
  }

  df <- dplyr::bind_rows(rows)
  best_idx <- which.max(df$WinnerScore)[1]
  list(table = df, best = df[best_idx, , drop = FALSE], alpha = alpha, criterion_target = criterion_target, mode = "parameter")
}


build_baseline_summary <- function(res) {
  top_alt <- res$alternatives_table$Alternative[1]
  top_score <- res$alternatives_table$Score[1]
  paste0(
    "<div class='summary-card overview-card'>",
    "<div class='overview-headline'><span class='mini-chip'><i class='fa fa-check-circle'></i> Baseline ready</span></div>",
    "<h3 class='overview-title'>Decision snapshot</h3>",
    "<p class='overview-text'>The aligned ES-MADM III V7 engine has completed a coherent baseline execution. ",
    "The current leader is <b>", top_alt, "</b> with crisp score <b>", fmt3(top_score), "</b>.</p>",
    "<div class='summary-grid'>",
    "<div class='summary-pill'><span class='summary-label'>Global alpha</span><span class='summary-value'>", fmt3(res$alpha), "</span></div>",
    "<div class='summary-pill'><span class='summary-label'>NMI</span><span class='summary-value'>", fmt3(res$NMI), "</span></div>",
    "<div class='summary-pill'><span class='summary-label'>CES</span><span class='summary-value'>", fmt3(res$CES), "</span></div>",
    "<div class='summary-pill'><span class='summary-label'>ADI</span><span class='summary-value'>", fmt3(res$ADI), "</span></div>",
    "</div>",
    "<p class='soft-note' style='margin-top:10px;'>The diagnostic layer is non-redundant: NMI, CES, and ADI are reported, while NMGI is computed only for explicit multi-scenario sets.</p>",
    "</div>"
  )
}

metric_card <- function(icon_name, title, value, subtitle = NULL, accent = "blue") {
  div(
    class = paste("kpi-card", accent),
    div(class = "kpi-icon", icon(icon_name)),
    div(
      class = "kpi-body",
      div(class = "kpi-title", title),
      div(class = "kpi-value", value),
      if (!is.null(subtitle)) div(class = "kpi-subtitle", subtitle)
    )
  )
}

feature_card <- function(icon_name, title, text, accent = "blue") {
  div(
    class = paste("feature-card", accent),
    div(class = "feature-icon", icon(icon_name)),
    div(class = "feature-title", title),
    div(class = "feature-text", text)
  )
}

es_plot_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15, colour = "#17324D"),
      plot.subtitle = element_text(size = 11, colour = "#5D6D7E"),
      axis.title = element_text(face = "bold", colour = "#17324D"),
      axis.text = element_text(colour = "#36495E"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#E6ECF2", linewidth = 0.4),
      panel.grid.major.y = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

es_dt <- function(df, pageLength = 10, scrollX = FALSE, dom = "tip", rownames = FALSE) {
  datatable(
    df,
    rownames = rownames,
    class = "compact stripe hover row-border order-column nowrap",
    options = list(
      pageLength = pageLength,
      autoWidth = TRUE,
      scrollX = scrollX,
      dom = dom
    )
  )
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('theme-mode', function(mode) {
        document.body.classList.toggle('dark-mode', mode === 'dark');
      });
    ")),
    tags$style(HTML("
      body {
        background: linear-gradient(180deg, #F5F8FC 0%, #EDF3F8 100%);
        transition: background 0.2s ease, color 0.2s ease;
      }
      .container-fluid { max-width: 1500px; }
      .app-shell { margin-top: 10px; }
      .theme-toggle-wrap {
        position: fixed;
        top: 18px;
        right: 22px;
        z-index: 9999;
      }
      .theme-toggle-btn {
        border-radius: 999px !important;
        width: 54px;
        height: 54px;
        padding: 0 !important;
        font-size: 22px !important;
        box-shadow: 0 10px 26px rgba(20, 40, 80, 0.18);
        background: linear-gradient(135deg, #17324D, #2780B9) !important;
        border: none !important;
        color: #FFFFFF !important;
      }
      .hero-banner {
        position: relative;
        overflow: hidden;
        background: linear-gradient(120deg, #17324D 0%, #1F4E79 58%, #2780B9 100%);
        color: #FFFFFF;
        border-radius: 24px;
        padding: 30px 32px 26px 32px;
        margin-bottom: 18px;
        box-shadow: 0 14px 34px rgba(18, 50, 77, 0.18);
      }
      .hero-badge, .hero-pill, .mini-chip {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        border-radius: 999px;
      }
      .hero-badge {
        background: rgba(255,255,255,0.16);
        border: 1px solid rgba(255,255,255,0.18);
        padding: 6px 12px;
        font-size: 12px;
        letter-spacing: 0.03em;
        margin-bottom: 10px;
      }
      .hero-title {
        font-size: 38px;
        line-height: 1.08;
        font-weight: 800;
        margin: 6px 0 10px 0;
      }
      .hero-subtitle {
        font-size: 15px;
        line-height: 1.7;
        max-width: 900px;
        color: rgba(255,255,255,0.92);
        margin-bottom: 14px;
      }
      .hero-pills { display: flex; flex-wrap: wrap; gap: 10px; }
      .hero-pill {
        background: rgba(255,255,255,0.12);
        border: 1px solid rgba(255,255,255,0.14);
        padding: 9px 14px;
        font-size: 13px;
      }
      .hero-orb {
        position: absolute;
        display: flex;
        align-items: center;
        justify-content: center;
        border-radius: 50%;
        color: #FFFFFF;
        background: rgba(255,255,255,0.12);
        border: 1px solid rgba(255,255,255,0.18);
        backdrop-filter: blur(6px);
      }
      .orb-1 { width: 92px; height: 92px; right: 34px; top: 24px; font-size: 30px; }
      .orb-2 { width: 66px; height: 66px; right: 130px; top: 118px; font-size: 22px; }
      .orb-3 { width: 54px; height: 54px; right: 76px; bottom: 26px; font-size: 18px; }
      .orb-4 { width: 44px; height: 44px; right: 182px; bottom: 40px; font-size: 16px; }
      .hero-creator {
        margin-top: 12px;
        font-size: 13px;
        line-height: 1.6;
        color: rgba(255,255,255,0.90);
      }
      .hero-creator b { color: #FFFFFF; }
      .hero-emblem-wrap {
        position: absolute;
        right: 30px;
        top: 46px;
        width: 122px;
        height: 122px;
        opacity: 0.92;
        pointer-events: none;
      }
      .hero-emblem-wrap svg {
        width: 100%;
        height: 100%;
        filter: drop-shadow(0 10px 18px rgba(10, 24, 36, 0.22));
      }

      .soft-note { color: #64748B; font-size: 13px; line-height: 1.6; }
      .nav-tabs {
        border-bottom: none;
        margin-bottom: 12px;
      }
      .nav-tabs > li > a {
        border: none !important;
        border-radius: 12px 12px 0 0 !important;
        margin-right: 8px;
        color: #27506D !important;
        font-weight: 700;
        background: rgba(255,255,255,0.78);
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        background: #FFFFFF !important;
        color: #17324D !important;
        box-shadow: 0 -1px 0 #FFFFFF, 0 6px 16px rgba(23, 50, 77, 0.08);
      }
      .tab-content > .tab-pane {
        background: #FFFFFF;
        border-radius: 0 18px 18px 18px;
        padding: 18px;
        box-shadow: 0 8px 22px rgba(20, 40, 80, 0.08);
      }
      .summary-card, .control-block, .viz-card, .feature-card {
        background: #FFFFFF;
        border: 1px solid #E5EDF5;
        border-radius: 18px;
        box-shadow: 0 8px 24px rgba(20, 40, 80, 0.06);
      }
      .summary-card { padding: 18px; margin-bottom: 16px; }
      .overview-card { background: linear-gradient(180deg, #FFFFFF 0%, #F8FBFF 100%); }
      .overview-title {
        margin-top: 4px; margin-bottom: 8px; font-weight: 800; color: #17324D;
      }
      .overview-text { color: #40566B; line-height: 1.7; }
      .mini-chip {
        background: #EAF4FF; color: #0F4C81; padding: 6px 12px; font-size: 12px; font-weight: 700;
      }
      .summary-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
        gap: 10px;
        margin-top: 12px;
      }
      .summary-pill {
        padding: 12px 14px;
        border-radius: 14px;
        background: #F5F9FD;
        border: 1px solid #E2EAF2;
      }
      .summary-label {
        display: block; font-size: 12px; color: #6B7C8E; text-transform: uppercase; letter-spacing: 0.04em; margin-bottom: 3px;
      }
      .summary-value {
        display: block; font-weight: 800; color: #17324D; font-size: 20px;
      }
      .feature-card { padding: 18px; margin-bottom: 16px; min-height: 182px; }
      .feature-icon {
        width: 52px; height: 52px; border-radius: 15px; display: flex; align-items: center; justify-content: center;
        font-size: 20px; margin-bottom: 12px; color: #FFFFFF;
        background: linear-gradient(135deg, #1F4E79, #2F80C2);
      }
      .feature-card.teal .feature-icon { background: linear-gradient(135deg, #0E7490, #14B8A6); }
      .feature-card.amber .feature-icon { background: linear-gradient(135deg, #B45309, #F59E0B); }
      .feature-title { font-weight: 800; color: #17324D; margin-bottom: 6px; font-size: 18px; }
      .feature-text { color: #536579; line-height: 1.65; font-size: 13px; }

      .control-block {
        padding: 15px; margin-bottom: 14px; background: linear-gradient(180deg, #FFFFFF 0%, #F9FBFD 100%);
      }
      .control-block h4 {
        margin-top: 0; margin-bottom: 10px; color: #17324D; font-weight: 800; font-size: 16px;
      }
      .viz-card { padding: 16px; margin-bottom: 16px; }
      .section-title { font-weight: 800; color: #17324D; margin: 0 0 12px 0; }

      .kpi-card {
        display: flex; gap: 14px; align-items: center;
        background: linear-gradient(180deg, #FFFFFF 0%, #F8FBFE 100%);
        border: 1px solid #E5EDF5; border-radius: 18px; padding: 16px;
        box-shadow: 0 10px 24px rgba(20, 40, 80, 0.06); margin-bottom: 14px; min-height: 108px;
      }
      .kpi-icon {
        width: 54px; height: 54px; border-radius: 16px; display: flex; align-items: center; justify-content: center;
        color: #FFFFFF; font-size: 22px; flex: 0 0 54px;
      }
      .kpi-card.blue .kpi-icon { background: linear-gradient(135deg, #1F4E79, #2780B9); }
      .kpi-card.teal .kpi-icon { background: linear-gradient(135deg, #0F766E, #14B8A6); }
      .kpi-card.gold .kpi-icon { background: linear-gradient(135deg, #A16207, #F59E0B); }
      .kpi-card.purple .kpi-icon { background: linear-gradient(135deg, #6D28D9, #8B5CF6); }
      .kpi-title { color: #64748B; font-size: 12px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.05em; margin-bottom: 5px; }
      .kpi-value { color: #17324D; font-size: 24px; font-weight: 800; line-height: 1.1; }
      .kpi-subtitle { color: #5C6D7E; font-size: 12px; margin-top: 6px; line-height: 1.45; }

      .app-footer-note { color: #6B7C8E; font-size: 12px; margin-top: 8px; }
      .btn { border-radius: 12px; font-weight: 700; }
      .well { background: transparent; border: none; box-shadow: none; padding: 0; }

      /* Dark mode */
      body.dark-mode {
        background: linear-gradient(180deg, #0B1420 0%, #0F1B2B 100%);
        color: #E5EEF8;
      }
      body.dark-mode .hero-banner {
        background: linear-gradient(120deg, #0E2238 0%, #12304F 58%, #184A77 100%);
        box-shadow: 0 14px 34px rgba(0, 0, 0, 0.35);
      }
      body.dark-mode .nav-tabs > li > a {
        background: rgba(17, 28, 42, 0.92);
        color: #C7D7E8 !important;
      }
      body.dark-mode .nav-tabs > li.active > a,
      body.dark-mode .nav-tabs > li.active > a:focus,
      body.dark-mode .nav-tabs > li.active > a:hover {
        background: #132131 !important;
        color: #FFFFFF !important;
        box-shadow: 0 -1px 0 #132131, 0 6px 16px rgba(0, 0, 0, 0.28);
      }
      body.dark-mode .tab-content > .tab-pane,
      body.dark-mode .summary-card,
      body.dark-mode .control-block,
      body.dark-mode .viz-card,
      body.dark-mode .feature-card,
      body.dark-mode .kpi-card {
        background: #132131;
        border-color: #22364B;
        box-shadow: 0 10px 24px rgba(0, 0, 0, 0.25);
      }
      body.dark-mode .overview-card { background: linear-gradient(180deg, #132131 0%, #15283C 100%); }
      body.dark-mode .overview-title,
      body.dark-mode .feature-title,
      body.dark-mode .section-title,
      body.dark-mode .kpi-value,
      body.dark-mode .summary-value,
      body.dark-mode .control-block h4 {
        color: #EAF3FB;
      }
      body.dark-mode .overview-text,
      body.dark-mode .feature-text,
      body.dark-mode .kpi-subtitle,
      body.dark-mode .soft-note,
      body.dark-mode .app-footer-note,
      body.dark-mode .summary-label,
      body.dark-mode .kpi-title {
        color: #AFC3D8;
      }
      body.dark-mode .summary-pill {
        background: #172A3F;
        border-color: #27415C;
      }
      body.dark-mode .mini-chip {
        background: #1F4060;
        color: #EAF3FB;
      }
      body.dark-mode .form-control,
      body.dark-mode .selectize-input,
      body.dark-mode .selectize-dropdown,
      body.dark-mode .irs,
      body.dark-mode .datatable,
      body.dark-mode .dataTables_wrapper .dataTables_filter input {
        background-color: #17283B !important;
        color: #EAF3FB !important;
        border-color: #2A425C !important;
      }
      body.dark-mode table.dataTable tbody tr,
      body.dark-mode table.dataTable thead th {
        background-color: #132131 !important;
        color: #EAF3FB !important;
      }
      body.dark-mode .btn-default {
        background: #20354B;
        color: #EAF3FB;
        border-color: #2D4C69;
      }
    "))
  ),
  div(
    class = "theme-toggle-wrap",
    actionButton("toggleTheme", label = NULL, icon = icon("moon-o"), class = "theme-toggle-btn", title = "Toggle dark mode")
  ),
  div(
    class = "app-shell",
    div(
      class = "hero-banner",
      div(class = "hero-badge", icon("cogs"), "Mathematically aligned fuzzy decision analytics"),
      div(class = "hero-title", "ES-MADM III Decision Studio"),
      div(class = "hero-subtitle", "A polished Shiny workspace for entropy-synergy multicriteria analysis, scenario experimentation, interval-coherent diagnostics, scenario portfolios, and exportable decision outputs. The computational core is fully aligned with the final operational ES-MADM III V7 manuscript, including generalized preference-function families."),
      div(
        class = "hero-pills",
        div(class = "hero-pill", icon("sliders"), "Global alpha workflow"),
        div(class = "hero-pill", icon("balance-scale"), "Coherent interval probabilities"),
        div(class = "hero-pill", icon("line-chart"), "Scenario, alpha, and preference-threshold sensitivity")
      ),
      div(class = "hero-creator", HTML("Model creator: <b>LT COL (ORD) Dr. Kyratsoudis Sideris</b>")),
      div(class = "hero-emblem-wrap", es_madm_emblem_ui()),
      div(class = "hero-orb orb-3", icon("sitemap")),
      div(class = "hero-orb orb-4", icon("table"))
    ),

    tabsetPanel(
      id = "mainTabs",
      tabPanel(
        title = tagList(icon("dashboard"), "1. Overview"),
        fluidRow(
          column(4, feature_card("cogs", "Aligned computational core", "Implements the corrected ES-MADM III workflow with symmetric alpha-cuts, ratio-based normalization, criterion-wise coherent entropy envelopes, direct score propagation, and scenario-relative NMGI.", "blue")),
          column(4, feature_card("sliders", "Operational scenario lab", "Modify performances, subjective weights, criterion types, and preference thresholds without touching the baseline bundle. The app keeps the decision workflow coherent under scenario stress tests.", "teal")),
          column(4, feature_card("moon-o", "Light / dark interface", "A clear dark skin mode is built into the interface so the studio remains comfortable during long review sessions, presentations, and late-night manuscript work.", "amber"))
        ),
        fluidRow(
          column(8, uiOutput("baselineSummaryUI")),
          column(
            4,
            div(
              class = "summary-card",
              h4(class = "section-title", HTML(paste(icon("upload"), "Expected workbook structure"))),
              tags$ol(
                tags$li("DataMatrix"),
                tags$li("FuzzyDeviations"),
                tags$li("FuzzySubjectiveWeights"),
                tags$li("PreferenceParams"),
                tags$li("BenefitCost")
              ),
              p(class = "soft-note", "Optional sheets: GlobalAlpha and the legacy FuzzyAlpha_Criteria sheet for compatibility only."),
              p(class = "app-footer-note", "This final implementation aligns the code with the operational manuscript: information-sensitive representative weighting, manuscript-consistent crisp ICI, exact mutual information identity at the crisp level, and scenario-relative NMGI.")
            )
          )
        )
      ),

      tabPanel(
        title = tagList(icon("upload"), "2. Data Import"),
        sidebarLayout(
          sidebarPanel(
            div(
              class = "control-block",
              h4(HTML(paste(icon("file"), "Workbook upload"))),
              fileInput("fileExcel", "Upload Excel workbook", accept = c(".xls", ".xlsx"))
            ),
            div(
              class = "control-block",
              h4(HTML(paste(icon("table"), "Required sheets"))),
              textInput("sheetMatrix", "DataMatrix sheet", "DataMatrix"),
              textInput("sheetDelta", "FuzzyDeviations sheet", "FuzzyDeviations"),
              textInput("sheetWeights", "FuzzySubjectiveWeights sheet", "FuzzySubjectiveWeights"),
              textInput("sheetPref", "PreferenceParams sheet", "PreferenceParams"),
              textInput("sheetBC", "BenefitCost sheet", "BenefitCost")
            ),
            div(
              class = "control-block",
              h4(HTML(paste(icon("sliders"), "Alpha options"))),
              textInput("sheetAlphaGlobal", "Optional GlobalAlpha sheet", "GlobalAlpha"),
              textInput("sheetAlphaLegacy", "Optional legacy FuzzyAlpha_Criteria sheet", "FuzzyAlpha_Criteria"),
              numericInput("fallbackAlpha", "Fallback global alpha", value = 0.50, min = 0, max = 1, step = 0.05),
              actionButton("loadData", "Load workbook", class = "btn-primary")
            )
          ),
          mainPanel(
            div(class = "viz-card",
                h4(class = "section-title", HTML(paste(icon("search"), "Workbook previews"))),
                tabsetPanel(
                  tabPanel("DataMatrix", br(), DTOutput("tableMatrixPreview")),
                  tabPanel("FuzzyDeviations", br(), DTOutput("tableDeltaPreview")),
                  tabPanel("FuzzySubjectiveWeights", br(), DTOutput("tableWeightsPreview")),
                  tabPanel("PreferenceParams", br(), DTOutput("tablePrefPreview")),
                  tabPanel("BenefitCost", br(), DTOutput("tableBCPreview")),
                  tabPanel("Alpha sheets", br(), DTOutput("tableAlphaInfo"))
                )
            )
          )
        )
      ),

      tabPanel(
        title = tagList(icon("bar-chart"), "3. Baseline Results"),
        uiOutput("baselineMetricsUI"),
        tabsetPanel(
          tabPanel(
            title = tagList(icon("trophy"), "Alternatives"),
            div(class = "viz-card", DTOutput("tableAltBaseline")),
            div(class = "viz-card", plotOutput("plotAltBaseline", height = "430px"))
          ),
          tabPanel(
            title = tagList(icon("balance-scale"), "Criteria"),
            div(class = "viz-card", DTOutput("tableCritBaseline")),
            div(class = "viz-card", plotOutput("plotWeightsBaseline", height = "430px")),
            div(class = "viz-card", plotOutput("plotICIBaseline", height = "430px"))
          ),
          tabPanel(
            title = tagList(icon("line-chart"), "Diagnostics"),
            div(class = "viz-card", DTOutput("tableDiagBaseline")),
            div(class = "viz-card", plotOutput("plotDiagBaseline", height = "430px"))
          )
        )
      ),

      tabPanel(
        title = tagList(icon("exchange"), "4. Scenario Lab"),
        uiOutput("scenarioMetricsUI"),
        sidebarLayout(
          sidebarPanel(
            div(
              class = "control-block",
              h4(HTML(paste(icon("sliders"), "Scenario alpha"))),
              numericInput("scenarioAlpha", "Scenario global alpha", value = 0.50, min = 0, max = 1, step = 0.05)
            ),
            div(
              class = "control-block",
              h4(HTML(paste(icon("balance-scale"), "Modify one subjective weight"))),
              selectInput("modWeightCrit", "Criterion", choices = NULL),
              numericInput("modWeightCentral", "New central weight", value = 1, step = 0.05),
              numericInput("modWeightDelta", "New delta", value = 0.05, step = 0.01),
              actionButton("applyWeightChange", "Apply weight change")
            ),
            div(
              class = "control-block",
              h4(HTML(paste(icon("table"), "Modify one data entry"))),
              selectInput("modDataCrit", "Criterion", choices = NULL),
              selectInput("modDataAlt", "Alternative", choices = NULL),
              numericInput("modDataValue", "New central performance", value = 1, step = 0.1),
              actionButton("applyDataChange", "Apply data change")
            ),
            div(
              class = "control-block",
              h4(HTML(paste(icon("exchange"), "Modify criterion type"))),
              selectInput("modTypeCrit", "Criterion", choices = NULL),
              radioButtons("modTypeValue", "Type", choices = c("benefit", "cost"), inline = TRUE),
              actionButton("applyTypeChange", "Apply type change")
            ),
            div(
              class = "control-block",
              h4(HTML(paste(icon("cogs"), "Modify preference settings"))),
              selectInput("modPrefCrit", "Criterion", choices = NULL),
              selectInput("modPrefFunc", "Preference function", choices = preference_family_choices()),
              numericInput("modPrefQ", "q", value = 0, step = 0.01),
              numericInput("modPrefP", "p", value = 0.1, step = 0.01),
              numericInput("modPrefS", "s", value = 0.2, step = 0.01),
              p(class = "soft-note", "Unused parameters are ignored automatically according to the selected family. Supported families: usual, U-shape, V-shape, level, linear, gaussian."),
              actionButton("applyPrefChange", "Apply preference change")
            ),
            div(
              class = "control-block",
              h4(HTML(paste(icon("sliders"), "Preference-function sensitivity"))),
              selectInput("prefSensSource", "Sensitivity source", choices = c("Baseline", "Current scenario"), selected = "Baseline"),
              selectInput("prefSensCriterion", "Criterion target", choices = c("All criteria" = "__ALL__")),
              radioButtons("prefSensMode", "Sensitivity mode", choices = c("Family sweep" = "family", "Parameter sweep" = "parameter"), selected = "family", inline = TRUE),
              checkboxGroupInput("prefSensFamilies", "Families to test (family sweep)", choices = preference_family_choices(), selected = c("usual", "u-shape", "v-shape", "level", "linear", "gaussian")),
              selectInput("prefSensFamilySingle", "Family to parameter-sweep", choices = preference_family_choices(), selected = "linear"),
              fluidRow(
                column(4, numericInput("prefSensQFrom", "q from", value = 0.50, min = 0.05, max = 3, step = 0.05)),
                column(4, numericInput("prefSensQTo", "q to", value = 1.50, min = 0.05, max = 3, step = 0.05)),
                column(4, numericInput("prefSensQBy", "q by", value = 0.25, min = 0.05, max = 1, step = 0.05))
              ),
              fluidRow(
                column(4, numericInput("prefSensPFrom", "p from", value = 0.50, min = 0.05, max = 3, step = 0.05)),
                column(4, numericInput("prefSensPTo", "p to", value = 1.50, min = 0.05, max = 3, step = 0.05)),
                column(4, numericInput("prefSensPBy", "p by", value = 0.25, min = 0.05, max = 1, step = 0.05))
              ),
              fluidRow(
                column(4, numericInput("prefSensSFrom", "s from", value = 0.50, min = 0.05, max = 3, step = 0.05)),
                column(4, numericInput("prefSensSTo", "s to", value = 1.50, min = 0.05, max = 3, step = 0.05)),
                column(4, numericInput("prefSensSBy", "s by", value = 0.25, min = 0.05, max = 1, step = 0.05))
              ),
              actionButton("runPrefSensitivity", "Run preference sensitivity", class = "btn-primary"),
              p(class = "soft-note", "V7 supports generalized PROMETHEE-type families. Family sweep compares admissible function classes; parameter sweep varies only the parameters relevant to the selected family while all downstream V7 computations remain unchanged.")
            ),
            div(
              class = "control-block",
              h4(HTML(paste(icon("save"), "Save this scenario"))),
              textInput("scenarioNameLab", "Scenario name", "Scenario_1"),
              actionButton("saveScenarioLab", "Save current scenario to portfolio", class = "btn-success")
            ),
            actionButton("resetScenario", "Reset scenario to baseline", class = "btn-warning")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel(
                title = tagList(icon("area-chart"), "Scenario outputs"),
                div(class = "viz-card", DTOutput("tableAltScenario")),
                div(class = "viz-card", DTOutput("tableDiagScenario")),
                div(class = "viz-card", plotOutput("plotAltScenarioCompare", height = "430px"))
              ),
              tabPanel(
                title = tagList(icon("exchange"), "Baseline vs scenario"),
                div(class = "viz-card", DTOutput("tableCompScenario")),
                div(class = "viz-card", plotOutput("plotCompScenario", height = "430px"))
              ),
              tabPanel(
                title = tagList(icon("crosshairs"), "Scenario-set NMGI"),
                div(class = "viz-card", DTOutput("tableNMGI2Scenarios")),
                div(class = "viz-card", DTOutput("tableNMGIWeights2Scenarios")),
                div(class = "viz-card", plotOutput("plotNMGI2Scenarios", height = "430px"))
              ),
              tabPanel(
                title = tagList(icon("sliders"), "Preference sensitivity"),
                div(class = "viz-card", uiOutput("prefSensitivitySummaryUI")),
                div(class = "viz-card", DTOutput("tablePrefSensitivity")),
                div(class = "viz-card", plotOutput("plotPrefSensitivityNMI", height = "430px")),
                div(class = "viz-card", plotOutput("plotPrefSensitivityScore", height = "430px"))
              )
            )
          )
        )
      ),

tabPanel(
        title = tagList(icon("sitemap"), "5. Scenario Portfolio"),
        sidebarLayout(
          sidebarPanel(
            div(
              class = "control-block",
              h4(HTML(paste(icon("folder-open"), "Import multiple scenario workbooks"))),
              fileInput("scenarioImportFiles", "Upload scenario Excel files", accept = c(".xls", ".xlsx"), multiple = TRUE),
              selectInput(
                "scenarioImportPolicy",
                "If a scenario name already exists",
                choices = c("Overwrite existing scenario" = "overwrite", "Keep both (auto-rename)" = "rename"),
                selected = "rename"
              ),
              actionButton("importScenarios", "Import scenarios to portfolio", class = "btn-primary"),
              p(class = "soft-note", "Each imported workbook is treated as an independent scenario of the currently loaded baseline case study. The app validates criteria and alternatives against the baseline, computes the full ES-MADM III workflow using the scenario workbook's own alpha, and stores the result in the shared portfolio.")
            ),
            div(
              class = "control-block",
              h4(HTML(paste(icon("save"), "Save the current scenario state"))),
              textInput("scenarioName", "Scenario name", "Scenario_1"),
              actionButton("saveScenario", "Save current scenario to portfolio", class = "btn-success")
            ),
            div(
              class = "control-block",
              h4(HTML(paste(icon("trash"), "Portfolio maintenance"))),
              selectInput("portfolioRemoveName", "Saved scenario", choices = NULL),
              actionButton("removeScenario", "Remove selected scenario", class = "btn-danger"),
              actionButton("clearPortfolio", "Clear all scenarios")
            )
          ),
          mainPanel(
            tabsetPanel(
              tabPanel(
                title = tagList(icon("table"), "Portfolio summary"),
                div(class = "viz-card", DTOutput("tablePortfolioSummary")),
                div(class = "viz-card", plotOutput("plotPortfolioWinners", height = "430px"))
              ),
              tabPanel(
                title = tagList(icon("crosshairs"), "Scenario-set NMGI"),
                div(class = "viz-card", DTOutput("tablePortfolioNMGI")),
                div(class = "viz-card", DTOutput("tablePortfolioNMGIWeights")),
                div(class = "viz-card", plotOutput("plotPortfolioNMGI", height = "430px"))
              ),
              tabPanel(
                title = tagList(icon("file-text-o"), "Import log"),
                div(class = "viz-card", DTOutput("tableScenarioImportLog"))
              )
            )
          )
        )
      ),

      tabPanel(
        title = tagList(icon("line-chart"), "6. Alpha Sweep"),
        uiOutput("alphaSweepMetricsUI"),
        sidebarLayout(
          sidebarPanel(
            div(
              class = "control-block",
              h4(HTML(paste(icon("line-chart"), "Sweep settings"))),
              numericInput("alphaFrom", "From alpha", value = 0.10, min = 0, max = 1, step = 0.05),
              numericInput("alphaTo", "To alpha", value = 1.00, min = 0, max = 1, step = 0.05),
              numericInput("alphaBy", "By", value = 0.10, min = 0.01, max = 1, step = 0.01),
              actionButton("runAlphaSweep", "Run alpha sweep", class = "btn-primary"),
              p(class = "soft-note", "NMGI is computed across the generated alpha scenarios only.")
            )
          ),
          mainPanel(
            tabsetPanel(
              tabPanel(title = tagList(icon("table"), "Scenario table"), div(class = "viz-card", DTOutput("tableAlphaSweep"))),
              tabPanel(title = tagList(icon("crosshairs"), "NMGI"), div(class = "viz-card", DTOutput("tableAlphaSweepNMGI")), div(class = "viz-card", DTOutput("tableAlphaSweepWeights"))),
              tabPanel(title = tagList(icon("area-chart"), "Plots"), div(class = "viz-card", plotOutput("plotAlphaSweepIndices", height = "430px")), div(class = "viz-card", plotOutput("plotAlphaSweepNMGI", height = "430px")))
            )
          )
        )
      ),

      tabPanel(
        title = tagList(icon("download"), "7. Export"),
        fluidRow(
          column(
            6,
            div(
              class = "summary-card",
              h4(class = "section-title", HTML(paste(icon("download"), "Download results"))),
              p("Export baseline outputs, imported scenario-portfolio results, optional Scenario Lab outputs, and alpha-sweep analytics into a structured Excel workbook."),
              downloadButton("downloadResults", "Download Excel results", class = "btn-success"),
              p(class = "soft-note", style = "margin-top:12px;", "Imported scenarios are exported from the Scenario Portfolio. The current Scenario Lab state is exported only when it differs from the loaded baseline, avoiding duplicate baseline-vs-baseline results.")
            )
          ),
          column(
            6,
            div(
              class = "summary-card",
              h4(class = "section-title", HTML(paste(icon("info-circle"), "Alignment note"))),
              p("This final version keeps the model mathematically aligned with the current manuscript while remaining computationally tractable in practice."),
              p(class = "soft-note", "Information-sensitive representative weighting, manuscript-consistent crisp ICI, exact crisp mutual information, and scenario-relative NMGI are all implemented explicitly.")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    baseline_bundle = NULL,
    baseline_alpha = NULL,
    baseline_alpha_info = NULL,
    modified_bundle = NULL,
    alpha_sweep = NULL,
    portfolio = list(),
    portfolio_summary = data.frame(),
    scenario_import_log = data.frame(),
    pref_sensitivity = NULL,
    dark_mode = FALSE
  )

  session$onFlushed(function() {
    session$sendCustomMessage("theme-mode", "light")
  }, once = TRUE)

  observeEvent(input$toggleTheme, {
    rv$dark_mode <- !isTRUE(rv$dark_mode)
    session$sendCustomMessage("theme-mode", if (isTRUE(rv$dark_mode)) "dark" else "light")
  })

  baseline_results <- reactive({
    req(rv$baseline_bundle, rv$baseline_alpha)
    compute_esmadmiii(
      rv$baseline_bundle$dataMat,
      rv$baseline_bundle$deltaMat,
      rv$baseline_bundle$sbjDF,
      rv$baseline_bundle$prefDF,
      rv$baseline_bundle$bcDF,
      rv$baseline_alpha
    )
  })

  modified_results <- reactive({
    req(rv$modified_bundle)
    compute_esmadmiii(
      rv$modified_bundle$dataMat,
      rv$modified_bundle$deltaMat,
      rv$modified_bundle$sbjDF,
      rv$modified_bundle$prefDF,
      rv$modified_bundle$bcDF,
      input$scenarioAlpha
    )
  })

  current_scenario_is_distinct <- reactive({
    req(rv$baseline_bundle, rv$modified_bundle, rv$baseline_alpha)
    !bundles_equivalent(
      rv$baseline_bundle,
      rv$modified_bundle,
      alpha_a = rv$baseline_alpha,
      alpha_b = input$scenarioAlpha
    )
  })

  observeEvent(input$loadData, {
    req(input$fileExcel)
    path <- input$fileExcel$datapath
    sheets <- readxl::excel_sheets(path)

    needed <- c(input$sheetMatrix, input$sheetDelta, input$sheetWeights, input$sheetPref, input$sheetBC)
    miss <- needed[!needed %in% sheets]
    if (length(miss) > 0) {
      showNotification(paste("Missing required sheets:", paste(miss, collapse = ", ")), type = "error")
      return()
    }

    dfMatrix <- read_sheet_safe(path, input$sheetMatrix)
    dfDelta <- read_sheet_safe(path, input$sheetDelta)
    dfSbj <- read_sheet_safe(path, input$sheetWeights)
    dfPref <- read_sheet_safe(path, input$sheetPref)
    dfBC <- read_sheet_safe(path, input$sheetBC)
    if (any(vapply(list(dfMatrix, dfDelta, dfSbj, dfPref, dfBC), is.null, logical(1)))) {
      showNotification("Failed to read one or more required sheets.", type = "error")
      return()
    }

    alpha_global_df <- if (input$sheetAlphaGlobal %in% sheets) read_sheet_safe(path, input$sheetAlphaGlobal) else NULL
    alpha_legacy_df <- if (input$sheetAlphaLegacy %in% sheets) read_sheet_safe(path, input$sheetAlphaLegacy) else NULL

    alpha_global <- extract_single_alpha(alpha_global_df)
    legacy_note <- "No legacy alpha sheet used."

    if (!is.null(alpha_global)) {
      alpha_final <- min(max(alpha_global, 0), 1)
      alpha_source <- "GlobalAlpha sheet"
    } else if (!is.null(alpha_legacy_df) && all(c("Criterion", "Alpha") %in% names(alpha_legacy_df))) {
      legacy_vals <- suppressWarnings(as.numeric(alpha_legacy_df$Alpha))
      legacy_vals <- legacy_vals[is.finite(legacy_vals)]
      if (length(legacy_vals) > 0 && length(unique(round(legacy_vals, 10))) == 1) {
        alpha_final <- min(max(legacy_vals[1], 0), 1)
        alpha_source <- "Legacy FuzzyAlpha_Criteria sheet (constant values)"
        legacy_note <- "Legacy alpha sheet accepted because all criterion-level alpha values were identical."
      } else {
        alpha_final <- min(max(input$fallbackAlpha, 0), 1)
        alpha_source <- "Fallback UI alpha"
        legacy_note <- "Legacy FuzzyAlpha_Criteria sheet was ignored because criterion-specific alpha variation is not coherent in the corrected ES-MADM III implementation."
      }
    } else {
      alpha_final <- min(max(input$fallbackAlpha, 0), 1)
      alpha_source <- "Fallback UI alpha"
    }

    bundle <- build_model_bundle(dfMatrix, dfDelta, dfSbj, dfPref, dfBC)
    validate_bundle(bundle$dataMat, bundle$deltaMat, bundle$sbjDF, bundle$prefDF, bundle$bcDF)

    rv$baseline_bundle <- bundle
    rv$modified_bundle <- list(
      dataMat = bundle$dataMat,
      deltaMat = bundle$deltaMat,
      sbjDF = bundle$sbjDF,
      prefDF = bundle$prefDF,
      bcDF = bundle$bcDF
    )
    rv$baseline_alpha <- alpha_final
    rv$baseline_alpha_info <- data.frame(
      Source = c(alpha_source, "Legacy alpha note"),
      Value = c(fmt3(alpha_final), legacy_note),
      stringsAsFactors = FALSE
    )
    rv$alpha_sweep <- NULL
    rv$pref_sensitivity <- NULL
    rv$portfolio <- list()
    rv$portfolio_summary <- data.frame()
    rv$scenario_import_log <- data.frame()
    updateSelectInput(session, "portfolioRemoveName", choices = character(0))
    updateTextInput(session, "scenarioName", value = "Scenario_1")
    updateTextInput(session, "scenarioNameLab", value = "Scenario_1")

    critChoices <- rownames(bundle$dataMat)
    altChoices <- colnames(bundle$dataMat)
    updateSelectInput(session, "modWeightCrit", choices = critChoices, selected = critChoices[1])
    updateSelectInput(session, "modDataCrit", choices = critChoices, selected = critChoices[1])
    updateSelectInput(session, "modTypeCrit", choices = critChoices, selected = critChoices[1])
    updateSelectInput(session, "modPrefCrit", choices = critChoices, selected = critChoices[1])
    updateSelectInput(session, "prefSensCriterion", choices = c("All criteria" = "__ALL__", stats::setNames(critChoices, critChoices)), selected = "__ALL__")
    updateSelectInput(session, "modDataAlt", choices = altChoices, selected = altChoices[1])
    updateNumericInput(session, "scenarioAlpha", value = alpha_final)

    showNotification(paste("Workbook loaded successfully. Baseline alpha =", fmt3(alpha_final)), type = "message")
  })

  observeEvent(input$modWeightCrit, {
    req(rv$modified_bundle)
    row <- rv$modified_bundle$sbjDF[rv$modified_bundle$sbjDF$Criterion == input$modWeightCrit, , drop = FALSE]
    if (nrow(row) > 0) {
      updateNumericInput(session, "modWeightCentral", value = as.numeric(row$Central[1]))
      updateNumericInput(session, "modWeightDelta", value = as.numeric(row$Delta[1]))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$modPrefCrit, {
    req(rv$modified_bundle)
    row <- rv$modified_bundle$prefDF[rv$modified_bundle$prefDF$Criterion == input$modPrefCrit, , drop = FALSE]
    if (nrow(row) > 0) {
      updateSelectInput(session, "modPrefFunc", selected = canonical_preference_family(as.character(row$PreferenceFunction[1])))
      updateNumericInput(session, "modPrefQ", value = as.numeric(row$Threshold_q[1]))
      updateNumericInput(session, "modPrefP", value = as.numeric(row$Threshold_p[1]))
      updateNumericInput(session, "modPrefS", value = as.numeric(row$Threshold_s[1]))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$modTypeCrit, {
    req(rv$modified_bundle)
    row <- rv$modified_bundle$bcDF[rv$modified_bundle$bcDF$Criterion == input$modTypeCrit, , drop = FALSE]
    if (nrow(row) > 0) {
      updateRadioButtons(session, "modTypeValue", selected = tolower(as.character(row$Type[1])))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$applyWeightChange, {
    req(rv$modified_bundle)
    idx <- which(rv$modified_bundle$sbjDF$Criterion == input$modWeightCrit)
    if (length(idx) == 1) {
      rv$modified_bundle$sbjDF$Central[idx] <- input$modWeightCentral
      rv$modified_bundle$sbjDF$Delta[idx] <- input$modWeightDelta
      showNotification("Scenario subjective weight updated.", type = "message")
    }
  })

  observeEvent(input$applyDataChange, {
    req(rv$modified_bundle)
    rv$modified_bundle$dataMat[input$modDataCrit, input$modDataAlt] <- input$modDataValue
    showNotification("Scenario data entry updated.", type = "message")
  })

  observeEvent(input$applyTypeChange, {
    req(rv$modified_bundle)
    idx <- which(rv$modified_bundle$bcDF$Criterion == input$modTypeCrit)
    if (length(idx) == 1) {
      rv$modified_bundle$bcDF$Type[idx] <- input$modTypeValue
      showNotification("Scenario criterion type updated.", type = "message")
    }
  })

  observeEvent(input$applyPrefChange, {
    req(rv$modified_bundle)
    idx <- which(rv$modified_bundle$prefDF$Criterion == input$modPrefCrit)
    if (length(idx) == 1) {
      rv$modified_bundle$prefDF$PreferenceFunction[idx] <- canonical_preference_family(input$modPrefFunc)
      rv$modified_bundle$prefDF$Threshold_q[idx] <- input$modPrefQ
      rv$modified_bundle$prefDF$Threshold_p[idx] <- input$modPrefP
      rv$modified_bundle$prefDF$Threshold_s[idx] <- input$modPrefS
      showNotification("Scenario preference parameters updated.", type = "message")
    }
  })

  observeEvent(input$resetScenario, {
    req(rv$baseline_bundle)
    rv$modified_bundle <- list(
      dataMat = rv$baseline_bundle$dataMat,
      deltaMat = rv$baseline_bundle$deltaMat,
      sbjDF = rv$baseline_bundle$sbjDF,
      prefDF = rv$baseline_bundle$prefDF,
      bcDF = rv$baseline_bundle$bcDF
    )
    updateNumericInput(session, "scenarioAlpha", value = rv$baseline_alpha)
    showNotification("Scenario reset to baseline data.", type = "warning")
  })



  observe({
    choices <- if (nrow(rv$portfolio_summary) > 0) rv$portfolio_summary$Scenario else character(0)
    updateSelectInput(session, "portfolioRemoveName", choices = choices, selected = if (length(choices) > 0) choices[1] else character(0))
  })

  observeEvent(input$saveScenarioLab, {
    req(rv$baseline_bundle, rv$modified_bundle)
    name <- trimws(input$scenarioNameLab)
    if (!nzchar(name)) {
      showNotification("Please provide a scenario name.", type = "error")
      return()
    }
    res <- modified_results()
    rv$portfolio[[name]] <- list(
      bundle = rv$modified_bundle,
      alpha = input$scenarioAlpha,
      result = res,
      source = "Manual",
      origin = "Scenario Lab"
    )
    row <- scenario_summary_row(name, res, source = "Manual", origin = "Scenario Lab")
    if (is.null(rv$portfolio_summary) || nrow(rv$portfolio_summary) == 0) {
      rv$portfolio_summary <- row
    } else if (name %in% rv$portfolio_summary$Scenario) {
      idx <- which(rv$portfolio_summary$Scenario == name)[1]
      rv$portfolio_summary[idx, ] <- row
    } else {
      rv$portfolio_summary <- dplyr::bind_rows(rv$portfolio_summary, row)
    }
    showNotification(paste("Scenario saved to portfolio:", name), type = "message")
  })

  observeEvent(input$saveScenario, {
    req(rv$baseline_bundle, rv$modified_bundle)
    name <- trimws(input$scenarioName)
    if (!nzchar(name)) {
      showNotification("Please provide a scenario name.", type = "error")
      return()
    }
    res <- modified_results()
    rv$portfolio[[name]] <- list(
      bundle = rv$modified_bundle,
      alpha = input$scenarioAlpha,
      result = res,
      source = "Manual",
      origin = "Scenario Lab"
    )
    row <- scenario_summary_row(name, res, source = "Manual", origin = "Scenario Lab")
    if (is.null(rv$portfolio_summary) || nrow(rv$portfolio_summary) == 0) {
      rv$portfolio_summary <- row
    } else if (name %in% rv$portfolio_summary$Scenario) {
      idx <- which(rv$portfolio_summary$Scenario == name)[1]
      rv$portfolio_summary[idx, ] <- row
    } else {
      rv$portfolio_summary <- dplyr::bind_rows(rv$portfolio_summary, row)
    }
    showNotification(paste("Scenario saved to portfolio:", name), type = "message")
  })

  observeEvent(input$importScenarios, {
    req(rv$baseline_bundle)
    req(input$scenarioImportFiles)

    files <- input$scenarioImportFiles
    if (nrow(files) == 0) {
      showNotification("Please choose at least one scenario workbook.", type = "error")
      return()
    }

    logs <- list()
    n_ok <- 0
    n_fail <- 0

    for (i in seq_len(nrow(files))) {
      fname <- as.character(files$name[i])
      fpath <- as.character(files$datapath[i])
      target_name <- tools::file_path_sans_ext(basename(fname))
      status <- "Imported"
      detail <- ""

      tryCatch({
        loaded <- load_bundle_from_workbook(
          fpath,
          sheet_matrix = input$sheetMatrix,
          sheet_delta = input$sheetDelta,
          sheet_weights = input$sheetWeights,
          sheet_pref = input$sheetPref,
          sheet_bc = input$sheetBC,
          sheet_alpha = input$sheetAlphaGlobal,
          fallback_alpha = rv$baseline_alpha
        )

        validate_scenario_compatibility(rv$baseline_bundle, loaded$bundle)

        existing_names <- names(rv$portfolio)
        if (target_name %in% existing_names) {
          if (identical(input$scenarioImportPolicy, "rename")) {
            target_name <- make_unique_scenario_name(target_name, existing_names)
            detail <- paste0("Imported with auto-renamed scenario name: ", target_name)
          } else {
            detail <- paste0("Existing scenario overwritten: ", target_name)
          }
        }

        res <- compute_esmadmiii(
          loaded$bundle$dataMat,
          loaded$bundle$deltaMat,
          loaded$bundle$sbjDF,
          loaded$bundle$prefDF,
          loaded$bundle$bcDF,
          loaded$alpha
        )

        rv$portfolio[[target_name]] <- list(
          bundle = loaded$bundle,
          alpha = loaded$alpha,
          result = res,
          source = "Imported Excel",
          origin = fname
        )

        row <- scenario_summary_row(target_name, res, source = "Imported Excel", origin = fname)
        if (is.null(rv$portfolio_summary) || nrow(rv$portfolio_summary) == 0) {
          rv$portfolio_summary <- row
        } else if (target_name %in% rv$portfolio_summary$Scenario) {
          idx <- which(rv$portfolio_summary$Scenario == target_name)[1]
          rv$portfolio_summary[idx, ] <- row
        } else {
          rv$portfolio_summary <- dplyr::bind_rows(rv$portfolio_summary, row)
        }

        n_ok <- n_ok + 1
      }, error = function(e) {
        status <<- "Failed"
        detail <<- e$message
        n_fail <<- n_fail + 1
      })

      logs[[length(logs) + 1]] <- data.frame(
        File = fname,
        Scenario = target_name,
        Status = status,
        Detail = ifelse(nzchar(detail), detail, ifelse(status == "Imported", "Scenario imported successfully.", "")),
        stringsAsFactors = FALSE
      )
    }

    rv$scenario_import_log <- if (length(logs) > 0) dplyr::bind_rows(logs) else data.frame()
    showNotification(sprintf("Scenario import completed: %d imported, %d failed.", n_ok, n_fail), type = if (n_fail > 0) "warning" else "message")
  })

  observeEvent(input$removeScenario, {
    nm <- input$portfolioRemoveName
    if (!nzchar(nm)) return()
    rv$portfolio[[nm]] <- NULL
    if (!is.null(rv$portfolio_summary) && nrow(rv$portfolio_summary) > 0) {
      rv$portfolio_summary <- rv$portfolio_summary[rv$portfolio_summary$Scenario != nm, , drop = FALSE]
    }
    showNotification(paste("Scenario removed:", nm), type = "warning")
  })

  observeEvent(input$clearPortfolio, {
    rv$portfolio <- list()
    rv$portfolio_summary <- data.frame()
    rv$scenario_import_log <- data.frame()
    showNotification("Scenario portfolio cleared.", type = "warning")
  })

  observeEvent(input$runPrefSensitivity, {
    req(rv$baseline_bundle)
    source_bundle <- if (identical(input$prefSensSource, "Current scenario")) rv$modified_bundle else rv$baseline_bundle
    validate(
      need(!is.null(source_bundle), "No source bundle is available for preference sensitivity."),
      need(!is.null(input$prefSensCriterion), "Select a criterion target for sensitivity analysis."),
      need(isTRUE(input$prefSensQBy > 0) && isTRUE(input$prefSensPBy > 0) && isTRUE(input$prefSensSBy > 0), "All preference sensitivity steps must be positive."),
      need(length(input$prefSensFamilies) > 0 || identical(input$prefSensMode, "parameter"), "Select at least one family for the family sweep.")
    )
    tryCatch({
      rv$pref_sensitivity <- run_preference_sensitivity(
        bundle = source_bundle,
        alpha = input$scenarioAlpha %||% rv$baseline_alpha,
        criterion_target = input$prefSensCriterion,
        mode = input$prefSensMode,
        family_choices = input$prefSensFamilies,
        family_single = input$prefSensFamilySingle,
        q_from = input$prefSensQFrom,
        q_to = input$prefSensQTo,
        q_by = input$prefSensQBy,
        p_from = input$prefSensPFrom,
        p_to = input$prefSensPTo,
        p_by = input$prefSensPBy,
        s_from = input$prefSensSFrom,
        s_to = input$prefSensSTo,
        s_by = input$prefSensSBy,
        eps = EPS
      )
      showNotification("Preference sensitivity completed.", type = "message")
    }, error = function(e) {
      showNotification(e$message, type = "error")
    })
  })

  observeEvent(input$runAlphaSweep, {
    req(rv$baseline_bundle)
    if (input$alphaBy <= 0 || input$alphaTo < input$alphaFrom) {
      showNotification("Invalid alpha sweep range.", type = "error")
      return()
    }

    alphas <- round(seq(input$alphaFrom, input$alphaTo, by = input$alphaBy), 10)
    alphas <- pmin(pmax(alphas, 0), 1)

    out <- lapply(alphas, function(a) {
      res <- compute_esmadmiii(
        rv$baseline_bundle$dataMat,
        rv$baseline_bundle$deltaMat,
        rv$baseline_bundle$sbjDF,
        rv$baseline_bundle$prefDF,
        rv$baseline_bundle$bcDF,
        a
      )
      data.frame(
        Scenario = paste0("a=", fmt3(a)),
        Alpha = a,
        Winner = res$alternatives_table$Alternative[1],
        WinnerScore = res$alternatives_table$Score[1],
        NMI = res$NMI,
        CES = res$CES,
        ADI = res$ADI,
        stringsAsFactors = FALSE
      )
    })

    sweep_df <- bind_rows(out)
    nmgi_obj <- scenario_relative_nmgi(sweep_df[, c("Scenario", "NMI", "CES", "ADI")])
    rv$alpha_sweep <- list(scenario_table = sweep_df, nmgi = nmgi_obj)
    showNotification("Alpha sweep completed.", type = "message")
  })

  two_scenario_nmgi <- reactive({
    req(rv$baseline_bundle, rv$modified_bundle)
    if (!isTRUE(current_scenario_is_distinct())) return(NULL)
    base <- baseline_results()
    mod <- modified_results()
    df <- data.frame(
      Scenario = c("Baseline", "CurrentScenario"),
      NMI = c(base$NMI, mod$NMI),
      CES = c(base$CES, mod$CES),
      ADI = c(base$ADI, mod$ADI),
      stringsAsFactors = FALSE
    )
    scenario_relative_nmgi(df)
  })



  portfolio_nmgi <- reactive({
    req(rv$baseline_bundle)
    base <- baseline_results()
    rows <- list(data.frame(
      Scenario = "Baseline",
      NMI = base$NMI,
      CES = base$CES,
      ADI = base$ADI,
      stringsAsFactors = FALSE
    ))
    if (!is.null(rv$portfolio_summary) && nrow(rv$portfolio_summary) > 0) {
      rows[[2]] <- rv$portfolio_summary[, c("Scenario", "NMI", "CES", "ADI"), drop = FALSE]
    }
    df <- dplyr::bind_rows(rows)
    if (nrow(df) < 2) return(NULL)
    scenario_relative_nmgi(df)
  })

  output$baselineSummaryUI <- renderUI({
    if (is.null(rv$baseline_bundle)) {
      div(
        class = "summary-card overview-card",
        h3(class = "overview-title", "Decision snapshot"),
        p(class = "overview-text", "Load a workbook to populate the baseline dashboard and generate the decision analytics overview."),
        p(class = "soft-note", "Once the workbook is loaded, this panel will surface the current leading alternative and the principal entropy-based indicators.")
      )
    } else {
      HTML(build_baseline_summary(baseline_results()))
    }
  })

  output$baselineMetricsUI <- renderUI({
    if (is.null(rv$baseline_bundle)) {
      fluidRow(
        column(3, metric_card("upload", "Status", "Awaiting workbook", "Load the Excel bundle to activate the baseline dashboard.", "blue")),
        column(3, metric_card("sliders", "Global alpha", "–", "Will be resolved from GlobalAlpha or fallback settings.", "teal")),
        column(3, metric_card("trophy", "Top alternative", "–", "Ranking appears after the first coherent run.", "gold")),
        column(3, metric_card("area-chart", "Indices", "–", "NMI, CES, and ADI will be shown here.", "purple"))
      )
    } else {
      res <- baseline_results()
      fluidRow(
        column(3, metric_card("sliders", "Global alpha", fmt3(res$alpha), paste(res$M, "criteria ×", res$N, "alternatives"), "blue")),
        column(3, metric_card("trophy", "Top alternative", res$alternatives_table$Alternative[1], paste("Score", fmt3(res$alternatives_table$Score[1])), "gold")),
        column(3, metric_card("exchange", "NMI / CES", paste0(fmt3(res$NMI), " / ", fmt3(res$CES)), "Mutual information and criteria effectiveness.", "teal")),
        column(3, metric_card("crosshairs", "ADI", fmt3(res$ADI), "Alternatives distinction intensity.", "purple"))
      )
    }
  })

  output$scenarioMetricsUI <- renderUI({
    if (is.null(rv$modified_bundle) || is.null(rv$baseline_bundle)) {
      fluidRow(
        column(3, metric_card("exchange", "Scenario status", "Inactive", "Load baseline data first.", "blue")),
        column(3, metric_card("sliders", "Scenario alpha", "–", "No scenario run yet.", "teal")),
        column(3, metric_card("trophy", "Scenario leader", "–", "Will update after scenario computation.", "gold")),
        column(3, metric_card("line-chart", "Impact", "–", "Changes vs baseline will appear here.", "purple"))
      )
    } else {
      base <- baseline_results()
      mod <- modified_results()
      delta_top <- mod$alternatives_table$Score[1] - base$alternatives_table$Score[1]
      fluidRow(
        column(3, metric_card("sliders", "Scenario alpha", fmt3(mod$alpha), paste("Baseline", fmt3(base$alpha)), "blue")),
        column(3, metric_card("trophy", "Scenario leader", mod$alternatives_table$Alternative[1], paste("Score", fmt3(mod$alternatives_table$Score[1])), "gold")),
        column(3, metric_card("exchange", "Top-score shift", sprintf("%+.3f", delta_top), "Scenario minus baseline top score.", "teal")),
        column(3, metric_card("crosshairs", "Scenario NMGI set", "2 scenarios", "Baseline and current scenario are ready for NMGI.", "purple"))
      )
    }
  })

  output$alphaSweepMetricsUI <- renderUI({
    if (is.null(rv$alpha_sweep)) {
      fluidRow(
        column(3, metric_card("line-chart", "Sweep status", "Not run", "Launch the alpha sweep to populate this panel.", "blue")),
        column(3, metric_card("sliders", "Alpha range", "–", "Configured in the left control panel.", "teal")),
        column(3, metric_card("crosshairs", "Best NMGI", "–", "Available after at least two alpha scenarios.", "gold")),
        column(3, metric_card("table", "Scenario count", "0", "No sweep records have been generated yet.", "purple"))
      )
    } else {
      sweep_df <- rv$alpha_sweep$scenario_table
      nmgi_df <- rv$alpha_sweep$nmgi$scenario_table
      best_idx <- which.max(nmgi_df$NMGI)
      fluidRow(
        column(3, metric_card("table", "Scenario count", as.character(nrow(sweep_df)), "Alpha-grid scenarios in the current sweep.", "blue")),
        column(3, metric_card("sliders", "Alpha range", paste0(fmt3(min(sweep_df$Alpha)), " to ", fmt3(max(sweep_df$Alpha))), paste("Step", fmt3(ifelse(nrow(sweep_df) > 1, diff(sweep_df$Alpha)[1], 0))), "teal")),
        column(3, metric_card("trophy", "Best NMGI scenario", nmgi_df$Scenario[best_idx], paste("NMGI", fmt3(nmgi_df$NMGI[best_idx])), "gold")),
        column(3, metric_card("crosshairs", "Winning alpha", fmt3(sweep_df$Alpha[best_idx]), paste("Winner", sweep_df$Winner[best_idx]), "purple"))
      )
    }
  })

  output$prefSensitivitySummaryUI <- renderUI({
    if (is.null(rv$pref_sensitivity)) {
      HTML("<p class='overview-text'>Run a generalized preference-function sensitivity analysis from the Scenario Lab sidebar. Family sweep compares admissible PROMETHEE-type families, whereas parameter sweep varies the relevant thresholds or the Gaussian scale while the downstream V7 operational engine remains fixed.</p>")
    } else {
      df <- rv$pref_sensitivity$table
      best <- rv$pref_sensitivity$best
      if (identical(rv$pref_sensitivity$mode, "family")) {
        HTML(paste0(
          "<div class='overview-text'><b>Family sensitivity completed.</b> ", nrow(df),
          " family configurations were evaluated at alpha = <b>", fmt3(rv$pref_sensitivity$alpha),
          "</b>. Best winner-score configuration: <b>", best$Winner[1],
          "</b> with score <b>", fmt3(best$WinnerScore[1]), "</b> under family <b>", best$Family[1], "</b>.</div>"
        ))
      } else {
        HTML(paste0(
          "<div class='overview-text'><b>Parameter sensitivity completed.</b> ", nrow(df),
          " admissible configurations were evaluated at alpha = <b>", fmt3(rv$pref_sensitivity$alpha),
          "</b> for family <b>", best$Family[1],
          "</b>. Best winner-score configuration: <b>", best$Winner[1],
          "</b> with score <b>", fmt3(best$WinnerScore[1]), "</b>.</div>"
        ))
      }
    }
  })
  output$tablePrefSensitivity <- renderDT({
    req(rv$pref_sensitivity)
    df <- rv$pref_sensitivity$table
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    es_dt(df, pageLength = 12, scrollX = TRUE) %>% formatRound(columns = num_cols, digits = 4)
  })
  output$plotPrefSensitivityNMI <- renderPlot({
    req(rv$pref_sensitivity)
    df <- rv$pref_sensitivity$table
    if (identical(rv$pref_sensitivity$mode, "family")) {
      ggplot(df, aes(x = reorder(Family, NMI), y = NMI, fill = Family)) +
        geom_col(width = 0.68) +
        geom_text(aes(label = Winner), hjust = -0.08, size = 3.8, fontface = "bold", colour = "#17324D") +
        coord_flip(clip = "off") +
        scale_y_continuous(expand = expansion(mult = c(0.02, 0.18))) +
        labs(title = "Preference-family sensitivity: NMI profile", subtitle = "Bar labels show the winning alternative for each tested family", x = NULL, y = "NMI") +
        es_plot_theme() + theme(legend.position = "none")
    } else if (unique(df$Family)[1] %in% c("linear", "level") && any(!is.na(df$Q_Multiplier)) && any(!is.na(df$P_Multiplier))) {
      ggplot(df, aes(x = factor(Q_Multiplier), y = factor(P_Multiplier), fill = NMI)) +
        geom_tile(color = "white", linewidth = 0.6) +
        geom_text(aes(label = Winner), size = 3.6, fontface = "bold") +
        labs(title = "Preference-parameter sensitivity: NMI landscape", subtitle = "Tile labels show the winner for each q/p combination", x = "q multiplier", y = "p multiplier") +
        es_plot_theme()
    } else {
      xvar <- if (all(is.na(df$Q_Multiplier))) if (all(is.na(df$P_Multiplier))) "S_Multiplier" else "P_Multiplier" else "Q_Multiplier"
      ggplot(df, aes_string(x = xvar, y = "NMI")) +
        geom_line(linewidth = 1.15, colour = "#1F4E79") +
        geom_point(size = 3, colour = "#1F4E79") +
        geom_text(aes(label = Winner), nudge_y = 0.015, size = 3.5, fontface = "bold", colour = "#17324D") +
        labs(title = "Preference-parameter sensitivity: NMI response", subtitle = "Winner labels shown at each admissible parameter setting", x = gsub("_", " ", xvar), y = "NMI") +
        es_plot_theme()
    }
  })
  output$plotPrefSensitivityScore <- renderPlot({
    req(rv$pref_sensitivity)
    df <- rv$pref_sensitivity$table
    if (identical(rv$pref_sensitivity$mode, "family")) {
      ggplot(df, aes(x = reorder(Family, WinnerScore), y = WinnerScore, fill = Family)) +
        geom_col(width = 0.68) +
        geom_text(aes(label = fmt3(WinnerScore)), hjust = -0.08, size = 3.8, fontface = "bold", colour = "#17324D") +
        coord_flip(clip = "off") +
        scale_y_continuous(expand = expansion(mult = c(0.02, 0.18))) +
        labs(title = "Preference-family sensitivity: top-score profile", subtitle = "Winning score under each admissible family", x = NULL, y = "Winner score") +
        es_plot_theme() + theme(legend.position = "none")
    } else if (unique(df$Family)[1] %in% c("linear", "level") && any(!is.na(df$Q_Multiplier)) && any(!is.na(df$P_Multiplier))) {
      ggplot(df, aes(x = factor(Q_Multiplier), y = factor(P_Multiplier), fill = WinnerScore)) +
        geom_tile(color = "white", linewidth = 0.6) +
        geom_text(aes(label = fmt3(WinnerScore)), size = 3.4, fontface = "bold") +
        labs(title = "Preference-parameter sensitivity: winning-score landscape", subtitle = "Winner-score response across the q/p grid", x = "q multiplier", y = "p multiplier") +
        es_plot_theme()
    } else {
      xvar <- if (all(is.na(df$Q_Multiplier))) if (all(is.na(df$P_Multiplier))) "S_Multiplier" else "P_Multiplier" else "Q_Multiplier"
      ggplot(df, aes_string(x = xvar, y = "WinnerScore")) +
        geom_line(linewidth = 1.15, colour = "#0E7490") +
        geom_point(size = 3, colour = "#0E7490") +
        geom_text(aes(label = fmt3(WinnerScore)), nudge_y = 0.015, size = 3.5, fontface = "bold", colour = "#17324D") +
        labs(title = "Preference-parameter sensitivity: winning-score response", subtitle = "Operational winner score under admissible parameter variation", x = gsub("_", " ", xvar), y = "Winner score") +
        es_plot_theme()
    }
  })

  output$tableMatrixPreview <- renderDT({
    req(rv$baseline_bundle)
    es_dt(as.data.frame(rv$baseline_bundle$dataMat, stringsAsFactors = FALSE), pageLength = 8, rownames = TRUE) %>%
      formatRound(columns = seq_len(ncol(rv$baseline_bundle$dataMat)), digits = 3)
  })
  output$tableDeltaPreview <- renderDT({
    req(rv$baseline_bundle)
    es_dt(as.data.frame(rv$baseline_bundle$deltaMat, stringsAsFactors = FALSE), pageLength = 8, rownames = TRUE) %>%
      formatRound(columns = seq_len(ncol(rv$baseline_bundle$deltaMat)), digits = 3)
  })
  output$tableWeightsPreview <- renderDT({
    req(rv$baseline_bundle)
    es_dt(rv$baseline_bundle$sbjDF, pageLength = 8) %>% formatRound(columns = c("Central", "Delta"), digits = 3)
  })
  output$tablePrefPreview <- renderDT({
    req(rv$baseline_bundle)
    es_dt(rv$baseline_bundle$prefDF, pageLength = 8) %>% formatRound(columns = c("Threshold_q", "Threshold_p", "Threshold_s"), digits = 3)
  })
  output$tableBCPreview <- renderDT({
    req(rv$baseline_bundle)
    es_dt(rv$baseline_bundle$bcDF, pageLength = 8)
  })
  output$tableAlphaInfo <- renderDT({
    req(rv$baseline_alpha_info)
    es_dt(rv$baseline_alpha_info, pageLength = 5, dom = "t")
  })

  output$tableAltBaseline <- renderDT({
    res <- baseline_results()
    es_dt(res$alternatives_table, pageLength = 10) %>% formatRound(columns = "Score", digits = 4)
  })
  output$tableCritBaseline <- renderDT({
    res <- baseline_results()
    es_dt(res$criteria_table, pageLength = 10, scrollX = TRUE) %>%
      formatRound(columns = names(res$criteria_table)[vapply(res$criteria_table, is.numeric, logical(1))], digits = 4)
  })
  output$tableDiagBaseline <- renderDT({
    res <- baseline_results()
    es_dt(res$diagnostics_table, pageLength = 10, dom = "t") %>% formatRound(columns = "Value", digits = 4)
  })

  output$plotAltBaseline <- renderPlot({
    res <- baseline_results()
    df <- res$alternatives_table
    df$Highlight <- ifelse(df$Rank == 1, "Winner", "Other alternatives")
    ggplot(df, aes(x = reorder(Alternative, Score), y = Score, fill = Highlight)) +
      geom_col(width = 0.72) +
      geom_text(aes(label = fmt3(Score)), hjust = -0.08, size = 4.1, fontface = "bold", colour = "#17324D") +
      coord_flip(clip = "off") +
      scale_fill_manual(values = c("Winner" = "#0F4C81", "Other alternatives" = "#BFD7EA")) +
      scale_y_continuous(expand = expansion(mult = c(0.02, 0.18))) +
      labs(title = "Baseline alternative ranking", subtitle = "Winner highlighted in dark blue", x = NULL, y = "Crisp score") +
      es_plot_theme()
  })

  output$plotWeightsBaseline <- renderPlot({
    res <- baseline_results()
    df <- data.frame(
      Criterion = rep(res$critNames, 3),
      WeightType = factor(rep(c("Subjective", "Objective", "Integrated"), each = res$M), levels = c("Subjective", "Objective", "Integrated")),
      Weight = c(res$xSBJ, res$xOBJ, res$xINT),
      stringsAsFactors = FALSE
    )
    ggplot(df, aes(x = Criterion, y = Weight, fill = WeightType)) +
      geom_col(position = position_dodge(width = 0.72), width = 0.64) +
      scale_fill_manual(values = c("Subjective" = "#94A3B8", "Objective" = "#38B2AC", "Integrated" = "#1F4E79")) +
      labs(title = "Criteria weight architecture", subtitle = "Subjective, objective, and integrated weights side by side", x = NULL, y = "Weight") +
      es_plot_theme()
  })

  output$plotICIBaseline <- renderPlot({
    res <- baseline_results()
    df <- res$criteria_table[order(res$criteria_table$ICI), c("Criterion", "ICI")]
    ggplot(df, aes(x = ICI, y = reorder(Criterion, ICI))) +
      geom_segment(aes(x = 0, xend = ICI, y = Criterion, yend = Criterion), colour = "#C8D6E5", linewidth = 1.2) +
      geom_point(size = 4.2, colour = "#7C3AED") +
      geom_text(aes(label = fmt3(ICI)), nudge_x = 0.015, hjust = 0, size = 4, colour = "#17324D", fontface = "bold") +
      scale_x_continuous(expand = expansion(mult = c(0.02, 0.18))) +
      labs(title = "Integrated criteria importance", subtitle = "Lollipop view of ICI magnitudes", x = "ICI", y = NULL) +
      es_plot_theme()
  })

  output$plotDiagBaseline <- renderPlot({
    res <- baseline_results()
    df <- data.frame(
      Index = factor(c("NMI", "CES", "ADI"), levels = c("NMI", "CES", "ADI")),
      Value = c(res$NMI, res$CES, res$ADI),
      stringsAsFactors = FALSE
    )
    ggplot(df, aes(x = Value, y = Index, fill = Index)) +
      geom_col(width = 0.64) +
      geom_text(aes(label = fmt3(Value)), hjust = -0.08, size = 4.1, fontface = "bold", colour = "#17324D") +
      scale_fill_manual(values = c("NMI" = "#1F4E79", "CES" = "#0F766E", "ADI" = "#A16207")) +
      scale_x_continuous(expand = expansion(mult = c(0.02, 0.18))) +
      labs(title = "Baseline entropy-based indices", subtitle = "Compact diagnostic overview", x = "Value", y = NULL) +
      es_plot_theme() +
      theme(legend.position = "none")
  })

  output$tableAltScenario <- renderDT({
    res <- modified_results()
    es_dt(res$alternatives_table, pageLength = 10) %>% formatRound(columns = "Score", digits = 4)
  })
  output$tableDiagScenario <- renderDT({
    res <- modified_results()
    es_dt(res$diagnostics_table, pageLength = 10, dom = "t") %>% formatRound(columns = "Value", digits = 4)
  })
  output$tableCompScenario <- renderDT({
    base <- baseline_results()
    mod <- modified_results()
    df <- data.frame(
      Metric = c("NMI", "CES", "ADI", "Top score"),
      Baseline = c(base$NMI, base$CES, base$ADI, base$alternatives_table$Score[1]),
      Scenario = c(mod$NMI, mod$CES, mod$ADI, mod$alternatives_table$Score[1]),
      Delta = c(mod$NMI - base$NMI, mod$CES - base$CES, mod$ADI - base$ADI, mod$alternatives_table$Score[1] - base$alternatives_table$Score[1]),
      stringsAsFactors = FALSE
    )
    es_dt(df, pageLength = 10, dom = "t") %>% formatRound(columns = c("Baseline", "Scenario", "Delta"), digits = 4)
  })

  output$plotAltScenarioCompare <- renderPlot({
    base <- baseline_results()
    mod <- modified_results()
    df <- merge(
      data.frame(Alternative = base$altNames, Baseline = base$P, stringsAsFactors = FALSE),
      data.frame(Alternative = mod$altNames, Scenario = mod$P, stringsAsFactors = FALSE),
      by = "Alternative",
      all = TRUE
    )
    df$Alternative <- factor(df$Alternative, levels = df$Alternative[order(df$Scenario)])
    ggplot(df, aes(y = Alternative)) +
      geom_segment(aes(x = Baseline, xend = Scenario, yend = Alternative), colour = "#CBD5E1", linewidth = 1.6) +
      geom_point(aes(x = Baseline), colour = "#94A3B8", size = 3.8) +
      geom_point(aes(x = Scenario), colour = "#0F4C81", size = 4.4) +
      labs(title = "Baseline vs scenario alternative scores", subtitle = "Grey points = baseline, blue points = scenario", x = "Score", y = NULL) +
      es_plot_theme()
  })

  output$plotCompScenario <- renderPlot({
    base <- baseline_results()
    mod <- modified_results()
    df <- merge(
      data.frame(Metric = c("NMI", "CES", "ADI"), Baseline = c(base$NMI, base$CES, base$ADI)),
      data.frame(Metric = c("NMI", "CES", "ADI"), Scenario = c(mod$NMI, mod$CES, mod$ADI)),
      by = "Metric"
    )
    df$Metric <- factor(df$Metric, levels = c("NMI", "CES", "ADI"))
    ggplot(df, aes(y = Metric)) +
      geom_segment(aes(x = Baseline, xend = Scenario, yend = Metric), colour = "#D7E3EF", linewidth = 1.6) +
      geom_point(aes(x = Baseline), colour = "#94A3B8", size = 3.8) +
      geom_point(aes(x = Scenario), colour = "#0E7490", size = 4.4) +
      labs(title = "Index movement under the scenario", subtitle = "Each segment links the baseline and scenario values", x = "Value", y = NULL) +
      es_plot_theme()
  })

  output$tableNMGI2Scenarios <- renderDT({
    nmgi_obj <- two_scenario_nmgi()
    req(nmgi_obj)
    es_dt(nmgi_obj$scenario_table, pageLength = 10, dom = "t") %>% formatRound(columns = c("NMI", "CES", "ADI", "NMGI"), digits = 4)
  })
  output$tableNMGIWeights2Scenarios <- renderDT({
    nmgi_obj <- two_scenario_nmgi()
    req(nmgi_obj)
    df <- data.frame(Index = names(nmgi_obj$weights), Weight = as.numeric(nmgi_obj$weights), stringsAsFactors = FALSE)
    es_dt(df, pageLength = 10, dom = "t") %>% formatRound(columns = "Weight", digits = 4)
  })
  output$plotNMGI2Scenarios <- renderPlot({
    nmgi_obj <- two_scenario_nmgi()
    req(nmgi_obj)
    df <- nmgi_obj$scenario_table
    df$Highlight <- ifelse(df$NMGI == max(df$NMGI), "Best", "Other")
    ggplot(df, aes(x = Scenario, y = NMGI, fill = Highlight)) +
      geom_col(width = 0.64) +
      geom_text(aes(label = fmt3(NMGI)), vjust = -0.35, size = 4.1, fontface = "bold", colour = "#17324D") +
      scale_fill_manual(values = c("Best" = "#A16207", "Other" = "#BFD7EA")) +
      scale_y_continuous(expand = expansion(mult = c(0.02, 0.16))) +
      labs(title = "Scenario-relative NMGI", subtitle = "Two-scenario set: baseline and current scenario", x = NULL, y = "NMGI") +
      es_plot_theme() +
      theme(legend.position = "none")
  })



  output$tablePortfolioSummary <- renderDT({
    req(rv$baseline_bundle)
    base <- baseline_results()
    base_row <- data.frame(
      Scenario = "Baseline",
      Source = "Baseline workbook",
      Origin = "",
      Alpha = base$alpha,
      Winner = base$alternatives_table$Alternative[1],
      WinnerScore = base$alternatives_table$Score[1],
      NMI = base$NMI,
      CES = base$CES,
      ADI = base$ADI,
      stringsAsFactors = FALSE
    )
    df <- if (!is.null(rv$portfolio_summary) && nrow(rv$portfolio_summary) > 0) dplyr::bind_rows(base_row, rv$portfolio_summary) else base_row
    nmgi_obj <- portfolio_nmgi()
    if (!is.null(nmgi_obj)) {
      df <- merge(df, nmgi_obj$scenario_table[, c("Scenario", "NMGI"), drop = FALSE], by = "Scenario", all.x = TRUE, sort = FALSE)
      df <- df[match(c("Baseline", rv$portfolio_summary$Scenario %||% character(0)), df$Scenario, nomatch = 0), , drop = FALSE]
    }
    es_dt(df, pageLength = 10, scrollX = TRUE) %>%
      formatRound(columns = intersect(c("Alpha", "WinnerScore", "NMI", "CES", "ADI", "NMGI"), names(df)), digits = 4)
  })

  output$tablePortfolioNMGI <- renderDT({
    nmgi_obj <- portfolio_nmgi()
    req(nmgi_obj)
    es_dt(nmgi_obj$scenario_table, pageLength = 10, dom = "t") %>%
      formatRound(columns = c("NMI", "CES", "ADI", "NMGI"), digits = 4)
  })

  output$tablePortfolioNMGIWeights <- renderDT({
    nmgi_obj <- portfolio_nmgi()
    req(nmgi_obj)
    df <- data.frame(Index = names(nmgi_obj$weights), Weight = as.numeric(nmgi_obj$weights), stringsAsFactors = FALSE)
    es_dt(df, pageLength = 10, dom = "t") %>% formatRound(columns = "Weight", digits = 4)
  })

  output$plotPortfolioNMGI <- renderPlot({
    nmgi_obj <- portfolio_nmgi()
    req(nmgi_obj)
    df <- nmgi_obj$scenario_table
    df$Highlight <- ifelse(df$NMGI == max(df$NMGI), "Best", "Other")
    ggplot(df, aes(x = reorder(Scenario, NMGI), y = NMGI, fill = Highlight)) +
      geom_col(width = 0.64) +
      geom_text(aes(label = fmt3(NMGI)), hjust = -0.08, size = 4.1, fontface = "bold", colour = "#17324D") +
      coord_flip(clip = "off") +
      scale_fill_manual(values = c("Best" = "#A16207", "Other" = "#BFD7EA")) +
      scale_y_continuous(expand = expansion(mult = c(0.02, 0.18))) +
      labs(title = "Portfolio scenario-set NMGI", subtitle = "Baseline and all saved scenarios compared together", x = NULL, y = "NMGI") +
      es_plot_theme() +
      theme(legend.position = "none")
  })

  output$plotPortfolioWinners <- renderPlot({
    req(rv$baseline_bundle)
    base <- baseline_results()
    rows <- list(data.frame(
      Scenario = "Baseline",
      Winner = base$alternatives_table$Alternative[1],
      WinnerScore = base$alternatives_table$Score[1],
      stringsAsFactors = FALSE
    ))
    if (!is.null(rv$portfolio_summary) && nrow(rv$portfolio_summary) > 0) {
      rows[[2]] <- rv$portfolio_summary[, c("Scenario", "Winner", "WinnerScore"), drop = FALSE]
    }
    df <- dplyr::bind_rows(rows)
    ggplot(df, aes(x = reorder(Scenario, WinnerScore), y = WinnerScore)) +
      geom_col(width = 0.64, fill = "#1F4E79") +
      geom_text(aes(label = paste0(Winner, " | ", fmt3(WinnerScore))), hjust = -0.08, size = 3.9, fontface = "bold", colour = "#17324D") +
      coord_flip(clip = "off") +
      scale_y_continuous(expand = expansion(mult = c(0.02, 0.18))) +
      labs(title = "Top alternative by scenario", subtitle = "Winner and winning score across the current portfolio", x = NULL, y = "Winning score") +
      es_plot_theme()
  })

  output$tableScenarioImportLog <- renderDT({
    if (is.null(rv$scenario_import_log) || nrow(rv$scenario_import_log) == 0) {
      es_dt(data.frame(Status = "No imports yet.", stringsAsFactors = FALSE), pageLength = 5, dom = "t")
    } else {
      es_dt(rv$scenario_import_log, pageLength = 10, scrollX = TRUE)
    }
  })

  output$tableAlphaSweep <- renderDT({
    req(rv$alpha_sweep)
    es_dt(rv$alpha_sweep$scenario_table, pageLength = 10) %>% formatRound(columns = c("Alpha", "WinnerScore", "NMI", "CES", "ADI"), digits = 4)
  })
  output$tableAlphaSweepNMGI <- renderDT({
    req(rv$alpha_sweep, rv$alpha_sweep$nmgi)
    es_dt(rv$alpha_sweep$nmgi$scenario_table, pageLength = 10) %>% formatRound(columns = c("NMI", "CES", "ADI", "NMGI"), digits = 4)
  })
  output$tableAlphaSweepWeights <- renderDT({
    req(rv$alpha_sweep, rv$alpha_sweep$nmgi)
    df <- data.frame(Index = names(rv$alpha_sweep$nmgi$weights), Weight = as.numeric(rv$alpha_sweep$nmgi$weights), stringsAsFactors = FALSE)
    es_dt(df, pageLength = 10, dom = "t") %>% formatRound(columns = "Weight", digits = 4)
  })
  output$plotAlphaSweepIndices <- renderPlot({
    req(rv$alpha_sweep)
    df <- rv$alpha_sweep$scenario_table
    plot_df <- rbind(
      data.frame(Alpha = df$Alpha, Index = "NMI", Value = df$NMI),
      data.frame(Alpha = df$Alpha, Index = "CES", Value = df$CES),
      data.frame(Alpha = df$Alpha, Index = "ADI", Value = df$ADI)
    )
    ggplot(plot_df, aes(x = Alpha, y = Value, color = Index)) +
      geom_line(linewidth = 1.15) +
      geom_point(size = 2.8) +
      scale_color_manual(values = c("NMI" = "#1F4E79", "CES" = "#0E7490", "ADI" = "#A16207")) +
      labs(title = "Alpha sweep diagnostics", subtitle = "Evolution of the principal crisp indices across the alpha grid", x = "Alpha", y = "Value") +
      es_plot_theme()
  })
  output$plotAlphaSweepNMGI <- renderPlot({
    req(rv$alpha_sweep, rv$alpha_sweep$nmgi)
    df <- rv$alpha_sweep$nmgi$scenario_table
    df$Alpha <- rv$alpha_sweep$scenario_table$Alpha
    best_idx <- which.max(df$NMGI)
    ggplot(df, aes(x = Alpha, y = NMGI)) +
      geom_line(linewidth = 1.15, colour = "#7C3AED") +
      geom_point(size = 3, colour = "#7C3AED") +
      geom_point(data = df[best_idx, , drop = FALSE], colour = "#F59E0B", size = 4.8) +
      geom_text(data = df[best_idx, , drop = FALSE], aes(label = paste0("Peak ", fmt3(NMGI))), nudge_y = 0.03, colour = "#17324D", fontface = "bold", size = 4) +
      labs(title = "Alpha sweep NMGI profile", subtitle = "Peak NMGI scenario highlighted in amber", x = "Alpha", y = "NMGI") +
      es_plot_theme()
  })

  output$downloadResults <- downloadHandler(
    filename = function() "ESMADMIII_V7_DECISION_STUDIO_RESULTS.xlsx",
    content = function(file) {
      base <- baseline_results()

      export_list <- build_export_list(base, prefix = "Baseline")

      # Export the Scenario Lab state only when it has actually been modified relative
      # to the loaded baseline. This prevents confusing duplicate Baseline-vs-Baseline
      # sheets when the user relies exclusively on Scenario Portfolio imports.
      if (isTRUE(current_scenario_is_distinct())) {
        mod <- modified_results()
        export_list <- c(export_list, build_export_list(mod, prefix = "CurrentScenario"))

        nmgi2 <- two_scenario_nmgi()
        if (!is.null(nmgi2)) {
          export_list[["Current_vs_Baseline_NMGI"]] <- nmgi2$scenario_table
          export_list[["Current_vs_Baseline_NMGI_Weights"]] <- data.frame(
            Index = names(nmgi2$weights),
            Weight = as.numeric(nmgi2$weights),
            stringsAsFactors = FALSE
          )
        }
      }

      portfolio_nmgi_obj <- portfolio_nmgi()
      if (!is.null(rv$portfolio_summary) && nrow(rv$portfolio_summary) > 0) {
        export_list[["ScenarioPortfolio_Summary"]] <- rv$portfolio_summary
        export_list[["ScenarioPortfolio_Summary_Full"]] <- portfolio_summary_with_baseline(
          base_res = base,
          portfolio_summary = rv$portfolio_summary,
          portfolio_nmgi_obj = portfolio_nmgi_obj
        )
        for (nm in names(rv$portfolio)) {
          scen <- rv$portfolio[[nm]]$result
          pref <- paste0("PF_", gsub("[^A-Za-z0-9_]+", "_", nm))
          tmp <- build_export_list(scen, prefix = pref)
          export_list <- c(export_list, tmp)
        }
      }
      if (!is.null(portfolio_nmgi_obj)) {
        export_list[["ScenarioPortfolio_NMGI"]] <- portfolio_nmgi_obj$scenario_table
        export_list[["ScenarioPortfolio_NMGI_Weights"]] <- data.frame(
          Index = names(portfolio_nmgi_obj$weights),
          Weight = as.numeric(portfolio_nmgi_obj$weights),
          stringsAsFactors = FALSE
        )
      }

      if (!is.null(rv$scenario_import_log) && nrow(rv$scenario_import_log) > 0) {
        export_list[["ScenarioImport_Log"]] <- rv$scenario_import_log
      }

      if (!is.null(rv$pref_sensitivity)) {
        export_list[["PreferenceSensitivity_Results"]] <- rv$pref_sensitivity$table
        export_list[["PreferenceSensitivity_Best"]] <- rv$pref_sensitivity$best
      }

      if (!is.null(rv$alpha_sweep)) {
        export_list[["Alpha_Sweep"]] <- rv$alpha_sweep$scenario_table
        if (!is.null(rv$alpha_sweep$nmgi)) {
          export_list[["Alpha_Sweep_NMGI"]] <- rv$alpha_sweep$nmgi$scenario_table
          export_list[["Alpha_Sweep_NMGI_Weights"]] <- data.frame(
            Index = names(rv$alpha_sweep$nmgi$weights),
            Weight = as.numeric(rv$alpha_sweep$nmgi$weights),
            stringsAsFactors = FALSE
          )
        }
      }

      writexl::write_xlsx(export_list, path = file)
    }
  )
}

shinyApp(ui = ui, server = server)
