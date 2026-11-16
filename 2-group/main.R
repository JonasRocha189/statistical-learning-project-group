# Group version of the fundraising assignment: who should receive a thank-you kit in 2026?
#
# Same question as part 1, with three changes a data scientist would make:
#   1. Better features: social accounts matched by name as well as email, more
#      donation-history and newsletter features.
#   2. Uplift learners suited to a 2% treatment group (X-learner, causal forest),
#      compared with part 1's T-learner on identical cross-validation folds.
#   3. Honest comparison: the net gain of each learner's decision is measured on
#      the pilot, with bootstrap intervals so we know which gaps are just noise.
#
# Run from the project folder:  Rscript 2-group/main.R

suppressPackageStartupMessages({
  library(data.table)
  library(lightgbm)
  library(grf)
})

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
HERE <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else "2-group"
DATA <- file.path(dirname(HERE), "Data-Fundraising-3.3")
OUT <- file.path(HERE, "outputs")

CHEAP_KITS <- 60000
CHEAP_COST <- 5
FULL_COST <- 25
PILOT_YEAR <- 2025
CAMPAIGN_YEAR <- 2026
SEED <- 60603
THREADS <- max(1L, parallel::detectCores() - 1L)
CF_CONTROLS <- 100000
CF_TREES <- 1000
BOOTSTRAPS <- 200

source(file.path(HERE, "features.R"))
source(file.path(HERE, "learners.R"))

dollars <- function(x, digits = 0) paste0("$", formatC(x, format = "f", digits = digits, big.mark = ","))
count <- function(x) formatC(x, format = "d", big.mark = ",")


kit_cost <- function(n) CHEAP_COST * pmin(n, CHEAP_KITS) + FULL_COST * pmax(n - CHEAP_KITS, 0)

# Rank by uplift and send while the next kit earns more than it costs.
choose_recipients <- function(uplift) {
  ranked <- order(-uplift)
  marginal_cost <- ifelse(seq_along(ranked) <= CHEAP_KITS, CHEAP_COST, FULL_COST)
  ranked[seq_len(sum(uplift[ranked] > marginal_cost))]
}

# Net gain of sending kits to `chosen`, measured on the pilot: the kit/no-kit
# difference in 2025 donations among the chosen members, times their number,
# minus the cost. `w` are bootstrap weights (1 = the data as observed).
pilot_gain <- function(chosen, y, kit, w = 1) {
  sel <- logical(length(y)); sel[chosen] <- TRUE
  w <- rep_len(w, length(y))
  t <- sel & kit; c <- sel & !kit
  n <- sum(w[sel])
  n * (sum(w[t] * y[t]) / sum(w[t]) - sum(w[c] * y[c]) / sum(w[c])) - kit_cost(n)
}

# Net gain of sending kits to the top-n members, for every n.
profit_curve <- function(uplift, y, kit) {
  ranked <- order(-uplift)
  y <- y[ranked]; kit <- kit[ranked]
  n <- seq_along(y)
  n_kit <- cumsum(kit)
  extra <- n * (cumsum(y * kit) / pmax(n_kit, 1) - cumsum(y * !kit) / pmax(n - n_kit, 1))
  curve <- data.table(kits_sent = n, extra_donations = extra, net_gain = extra - kit_cost(n))
  curve[n_kit >= 200]  # the first rows hold too few pilot members to be reliable
}


parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- list(folds = 5L, learner = NA_character_)
  usage <- paste("Usage: Rscript 2-group/main.R [--folds N] [--learner",
                 paste(c(names(LEARNERS), "ensemble"), collapse = "|"), "]")
  i <- 1
  while (i <= length(args)) {
    value <- args[i + 1]
    if (args[i] == "--folds" && !is.na(as.integer(value)) && as.integer(value) >= 2) {
      opts$folds <- as.integer(value)
    } else if (args[i] == "--learner" && value %in% c(names(LEARNERS), "ensemble")) {
      opts$learner <- value
    } else {
      stop(usage, call. = FALSE)
    }
    i <- i + 2
  }
  opts
}


main <- function() {
  args <- parse_args()
  dir.create(OUT, showWarnings = FALSE)
  set.seed(SEED)

  cat("Loading data...\n")
  d <- load_data(DATA)
  social_link <- match_social(d$members, d$social)
  cat(sprintf("  social accounts linked to a member: %s (%s by email, %s by unique name)\n",
              count(nrow(social_link)), count(sum(!social_link$by_name)),
              count(sum(social_link$by_name))))

  train <- build_features(d, PILOT_YEAR, social_link)
  ids <- train$ID
  X <- as.matrix(train[, ..FEATURES])
  given <- d$donations[Yr == PILOT_YEAR]
  y <- fcoalesce(as.numeric(given$Amount[match(ids, given$ID)]), 0)
  kit <- ids %in% d$contacted
  cat(sprintf("  average %d donation: %s with kit, %s without (uplift %s)\n", PILOT_YEAR,
              dollars(mean(y[kit]), 2), dollars(mean(y[!kit]), 2),
              dollars(mean(y[kit]) - mean(y[!kit]), 2)))

  # Cross-fitting: every member is scored by models that never saw them, and
  # all learners use the same folds so their results are directly comparable.
  fold <- integer(length(ids))
  for (group in list(which(kit), which(!kit))) {
    fold[group] <- sample(rep_len(seq_len(args$folds), length(group)))
  }
  oof <- list()
  for (name in names(LEARNERS)) {
    started <- Sys.time()
    pred <- numeric(length(ids))
    for (k in seq_len(args$folds)) {
      test <- fold == k
      pred[test] <- LEARNERS[[name]](X[!test, ], y[!test], kit[!test])(X[test, ])
    }
    oof[[name]] <- pred
    cat(sprintf("  cross-validated %-14s (%.1f min)\n", name,
                as.numeric(difftime(Sys.time(), started, units = "mins"))))
  }
  oof$ensemble <- Reduce(`+`, oof) / length(oof)

  # Calibration by tenth of predicted uplift, and the decision each learner makes.
  calibration <- rbindlist(lapply(names(oof), function(name) {
    decile <- ceiling(10 * frank(-oof[[name]], ties.method = "first") / length(y))
    data.table(learner = name, decile, pred = oof[[name]], y, kit)[, .(
      predicted_uplift = round(mean(pred), 2),
      actual_uplift = round(mean(y[kit]) - mean(y[!kit]), 2)
    ), keyby = .(learner, decile)]
  }))
  fwrite(calibration, file.path(OUT, "uplift_by_decile.csv"))

  curves <- rbindlist(lapply(names(oof), function(name) {
    profit_curve(oof[[name]], y, kit)[seq(1, .N, by = 1000)][, learner := name]
  }))
  fwrite(round_dt(curves), file.path(OUT, "profit_curve.csv"))

  chosen <- lapply(oof, choose_recipients)
  gains <- sapply(chosen, pilot_gain, y = y, kit = kit)

  # Paired bootstrap: resample members, recompute every learner's gain on the
  # same resample, and compare each learner with part 1's T-learner.
  boot <- t(replicate(BOOTSTRAPS, {
    w <- rpois(length(y), 1)
    sapply(chosen, pilot_gain, y = y, kit = kit, w = w)
  }))
  summary <- data.table(
    learner = names(oof),
    kits = sapply(chosen, length),
    net_gain = gains,
    se = apply(boot, 2, sd),
    vs_t_learner = gains - gains["t_learner"],
    vs_t_learner_lo = apply(boot - boot[, "t_learner"], 2, quantile, 0.025),
    vs_t_learner_hi = apply(boot - boot[, "t_learner"], 2, quantile, 0.975)
  )
  fwrite(round_dt(summary), file.path(OUT, "learner_comparison.csv"))

  cat("\nNet gain on the 2025 pilot (cross-validated), with 95% bootstrap interval vs T-learner:\n")
  for (i in seq_len(nrow(summary))) {
    s <- summary[i]
    cat(sprintf("  %-14s %9s kits -> %12s (se %s)   vs T: %+11s [%s, %s]\n",
                s$learner, count(s$kits), dollars(s$net_gain), dollars(s$se),
                dollars(s$vs_t_learner), dollars(s$vs_t_learner_lo), dollars(s$vs_t_learner_hi)))
  }

  pick <- if (is.na(args$learner)) summary[which.max(net_gain), learner] else args$learner
  cat(sprintf("\nFitting %s on all members and scoring for %d...\n", pick, CAMPAIGN_YEAR))
  # Features for 2026 include 2025 donations. For the 20,000 pilot members those
  # are inflated by the 2025 kit, so they keep their pre-pilot (2025) features.
  score <- build_features(d, CAMPAIGN_YEAR, social_link)
  stopifnot(identical(score$ID, ids))
  X_score <- as.matrix(score[, ..FEATURES])
  X_score[kit, ] <- X[kit, ]

  fits <- if (pick == "ensemble") names(LEARNERS) else pick
  uplift <- Reduce(`+`, lapply(fits, function(name) LEARNERS[[name]](X, y, kit)(X_score))) /
    length(fits)
  sent <- choose_recipients(uplift)

  scores <- data.table(ID = ids, predicted_uplift = round(uplift, 2), send_kit = 0L)
  scores[sent, send_kit := 1L]
  fwrite(scores, file.path(OUT, "scores.csv"))
  # Same layout as ListContacted2025.csv: one member ID per line, no header.
  fwrite(scores[sent, .(ID)], file.path(OUT, "submission.csv"), col.names = FALSE)

  cat(sprintf("  send %s kits (cost %s), predicted net gain %s\n", count(length(sent)),
              dollars(kit_cost(length(sent))), dollars(sum(uplift[sent]) - kit_cost(length(sent)))))
  cat(sprintf("  wrote %s\n", file.path(basename(HERE), "outputs", "submission.csv")))
}

round_dt <- function(dt) dt[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)]

main()
