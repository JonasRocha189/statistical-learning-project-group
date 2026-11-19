# Step 3: strengthen the kit-group model, the weak spot of the hurdle model.
#
# The hurdle model (step 2) predicts, for each group, P(donate) x E[amount | donate].
# The no-kit side learns from 980,000 members, the kit side from only 20,000
# (about 6,000 donors), so the kit side is where the noise is. Three fixes are
# compared on the same cross-validation, all keeping the same no-kit model:
#
#   anchoring  the kit model starts from the no-kit model's prediction and only
#              learns the correction (LightGBM init_score), so the 980,000
#              no-kit members inform the kit side too;
#   bagging    the kit model is averaged over 10 fits on 80% subsamples;
#   size       31 leaves and 50 members per leaf instead of 8 and 100.
#
# Run from the Team folder, after 01_social_match.R:
#   Rscript 2-group/code/03_kit_model.R [--folds 5] [--kits N] [--model auto]
#
# --model auto (default) writes the submission of the best model in CV.

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
CODE <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else "2-group/code"
source(file.path(CODE, "common.R"))
source(file.path(CODE, "uplift_lib.R"))

VARIANTS <- list(
  hurdle         = list(anchor = FALSE, leaves = 8,  min_leaf = 100, bags = 1),
  hurdle_bag     = list(anchor = FALSE, leaves = 8,  min_leaf = 100, bags = 10),
  anchor         = list(anchor = TRUE,  leaves = 8,  min_leaf = 100, bags = 1),
  anchor_bag     = list(anchor = TRUE,  leaves = 8,  min_leaf = 100, bags = 10),
  anchor_big     = list(anchor = TRUE,  leaves = 31, min_leaf = 50,  bags = 1),
  anchor_big_bag = list(anchor = TRUE,  leaves = 31, min_leaf = 50,  bags = 10)
)
REFERENCE <- "hurdle"


# No-kit model: P(donate) on the logit scale and E[amount | donate].
fit_control <- function(X, y) {
  gives <- y > 0
  p <- fit_lgb(X, as.numeric(gives), "binary")
  a <- fit_lgb(X[gives, , drop = FALSE], y[gives], small = TRUE)
  function(Z) list(logit = predict(p, Z, type = "raw"), amount = pmax(predict(a, Z), 0))
}

# Kit model for one variant. `base` holds the no-kit predictions for the same
# rows; with anchoring they are the starting point the trees correct.
fit_kit <- function(X, y, base, v) {
  gives <- y > 0
  init_p <- if (v$anchor) base$logit
  init_a <- if (v$anchor) base$amount
  fits <- lapply(seq_len(v$bags), function(b) {
    rows <- if (v$bags == 1) seq_len(nrow(X)) else sample.int(nrow(X), round(0.8 * nrow(X)))
    donors <- rows[gives[rows]]
    list(
      p = fit_lgb(X[rows, , drop = FALSE], as.numeric(gives[rows]), "binary",
                  leaves = v$leaves, min_leaf = v$min_leaf, init = init_p[rows], seed = SEED + b),
      a = fit_lgb(X[donors, , drop = FALSE], y[donors],
                  leaves = v$leaves, min_leaf = v$min_leaf, init = init_a[donors], seed = SEED + b)
    )
  })
  function(Z, base) {
    off_p <- if (v$anchor) base$logit else 0
    off_a <- if (v$anchor) base$amount else 0
    p <- rowMeans(sapply(fits, function(f) plogis(predict(f$p, Z, type = "raw") + off_p)))
    a <- rowMeans(sapply(fits, function(f) pmax(predict(f$a, Z, type = "raw") + off_a, 0)))
    p * a
  }
}

# Fits the no-kit model once and every kit-model variant; returns a function
# giving m0 and each variant's m1 and uplift.
fit_all <- function(X, y, kit, variants = VARIANTS) {
  y <- pmin(y, CAP)
  control <- fit_control(X[!kit, ], y[!kit])
  base_kit <- control(X[kit, ])
  kit_models <- lapply(variants, function(v) fit_kit(X[kit, ], y[kit], base_kit, v))
  function(Z) {
    base <- control(Z)
    m0 <- plogis(base$logit) * base$amount
    m1 <- lapply(kit_models, function(f) f(Z, base))
    list(m0 = m0, m1 = m1, uplift = lapply(m1, function(m) m - m0))
  }
}


parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- list(folds = 5L, kits = NA_integer_, model = "auto")
  i <- 1
  while (i <= length(args)) {
    value <- suppressWarnings(as.integer(args[i + 1]))
    if (args[i] == "--folds" && !is.na(value) && value >= 2) opts$folds <- value
    else if (args[i] == "--kits" && !is.na(value) && value > 0) opts$kits <- value
    else if (args[i] == "--model" && isTRUE(args[i + 1] %in% c("auto", names(VARIANTS)))) opts$model <- args[i + 1]
    else stop(sprintf("Usage: Rscript 2-group/code/03_kit_model.R [--folds N] [--kits N] [--model auto|%s]",
                      paste(names(VARIANTS), collapse = "|")), call. = FALSE)
    i <- i + 2
  }
  opts
}

main <- function() {
  args <- parse_args()
  set.seed(SEED)
  cat("Loading data...\n")
  d <- prepare_data()
  X <- d$X
  y <- d$y
  kit <- d$kit

  cat(sprintf("Cross-validating %d kit-model variants (%d folds)...\n", length(VARIANTS), args$folds))
  fold <- make_folds(kit, args$folds)
  oof <- setNames(lapply(VARIANTS, function(v) numeric(length(y))), names(VARIANTS))
  m0 <- m1 <- numeric(length(y))
  for (k in seq_len(args$folds)) {
    test <- fold == k
    pred <- fit_all(X[!test, ], y[!test], kit[!test])(X[test, ])
    for (m in names(VARIANTS)) oof[[m]][test] <- pred$uplift[[m]]
    m0[test] <- pred$m0
    m1[test] <- pred$m1[[REFERENCE]]
    cat(sprintf("  fold %d done\n", k))
  }

  yc <- pmin(y, CAP)
  # Nuisance from the reference hurdle model, as in step 2.
  G <- dr_outcome(yc, kit, m0, m1)
  results <- compare_models(oof, G, yc, kit, ref = REFERENCE)
  fwrite(results[, lapply(.SD, function(v) if (is.numeric(v)) round(v) else v)],
         file.path(OUT, "model_comparison_kit_model.csv"))
  print_comparison(results, REFERENCE)

  model <- if (args$model == "auto") results[which.max(dr_gain_at_eval), model] else args$model
  calib <- write_diagnostics(oof[[model]], G, "_kit_model")
  cat(sprintf("\n%s, predicted vs actual (DR) uplift by tenth:\n", model))
  print(as.data.frame(calib), row.names = FALSE)

  cat(sprintf("\nFitting %s on all members and scoring 2026...\n", model))
  final <- fit_all(X, y, kit, VARIANTS[model])
  up <- final(d$X26)$uplift[[model]]
  rule <- rule_kits(up)
  n <- if (is.na(args$kits)) rule else args$kits
  chosen <- order(-up)[seq_len(n)]
  fwrite(data.table(ID = d$members$ID, uplift = round(up, 2),
                    send_kit = as.integer(seq_along(up) %in% chosen)),
         file.path(OUT, sprintf("scores_%s_2026.csv", model)))
  path <- file.path(OUT, sprintf("submission_%s_2026_%d.csv", model, n))
  fwrite(data.table(ID = d$members$ID[chosen]), path, col.names = FALSE)
  cat(sprintf("  rule says %s kits; wrote %s kits (cost %s, predicted net gain %s) -> %s\n",
              count(rule), count(n), dollars(kit_cost(n)), dollars(sum(up[chosen]) - kit_cost(n)),
              basename(path)))
}


main()
