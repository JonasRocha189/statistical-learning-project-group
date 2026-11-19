# Step 2: compare uplift models on the same cross-validation, then score 2026.
#
# Models (all LightGBM, all predicting the 2025 donation with and without a kit):
#   T        one regression per group on the amount; uplift = difference (part 1 method).
#   hurdle   per group, P(donate) x E[amount | donate]; the kit raises both
#            (12% -> 31% donate, $52 -> $116 average gift), so modelling them
#            separately is closer to how the kit works.
#   hurdle_X X-learner on top of the hurdle: the kit group's effect is imputed with
#            the precise no-kit model, which suits a 2% treated group.
#   blend    average of hurdle and hurdle_X.
#
# Models are compared with the doubly robust net gain described in uplift_lib.R.
#
# Run from the Team folder, after 01_social_match.R:
#   Rscript 2-group/code/02_uplift.R [--folds 5] [--kits N] [--model hurdle]
#
# --model picks which model writes the submission (default hurdle, the best in CV).

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
CODE <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else "2-group/code"
source(file.path(CODE, "common.R"))
source(file.path(CODE, "uplift_lib.R"))

MODELS <- c("T", "hurdle", "hurdle_X", "blend")


# Expected donation for one group: P(donate) x E[amount | donate].
fit_hurdle <- function(X, y, small) {
  gives <- y > 0
  p <- fit_lgb(X, as.numeric(gives), "binary", small)
  a <- fit_lgb(X[gives, , drop = FALSE], y[gives], "regression", small = TRUE)
  function(Z) predict(p, Z) * pmax(predict(a, Z), 0)
}

# Fits every model on (X, y, kit); returns a function giving each model's uplift,
# plus m0/m1 (hurdle expected donations) used as DR nuisance.
fit_all <- function(X, y, kit) {
  y <- pmin(y, CAP)
  t1 <- fit_lgb(X[kit, ], y[kit], small = TRUE)
  t0 <- fit_lgb(X[!kit, ], y[!kit])
  h1 <- fit_hurdle(X[kit, ], y[kit], small = TRUE)
  h0 <- fit_hurdle(X[!kit, ], y[!kit], small = FALSE)
  # X-learner: impute each member's effect with the other group's model.
  d1 <- y[kit] - h0(X[kit, ])
  d0 <- h1(X[!kit, ]) - y[!kit]
  x1 <- fit_lgb(X[kit, ], d1, small = TRUE)
  x0 <- fit_lgb(X[!kit, ], d0)
  function(Z) {
    m1 <- h1(Z)
    m0 <- h0(Z)
    hurdle <- m1 - m0
    hurdle_X <- P_KIT * predict(x0, Z) + (1 - P_KIT) * predict(x1, Z)
    list(
      m0 = m0, m1 = m1,
      uplift = list(
        T = predict(t1, Z) - predict(t0, Z),
        hurdle = hurdle,
        hurdle_X = hurdle_X,
        blend = (hurdle + hurdle_X) / 2
      )
    )
  }
}


parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- list(folds = 5L, kits = NA_integer_, model = "hurdle")
  i <- 1
  while (i <= length(args)) {
    value <- suppressWarnings(as.integer(args[i + 1]))
    if (args[i] == "--folds" && !is.na(value) && value >= 2) opts$folds <- value
    else if (args[i] == "--kits" && !is.na(value) && value > 0) opts$kits <- value
    else if (args[i] == "--model" && isTRUE(args[i + 1] %in% MODELS)) opts$model <- args[i + 1]
    else stop("Usage: Rscript 2-group/code/02_uplift.R [--folds N] [--kits N] [--model T|hurdle|hurdle_X|blend]",
              call. = FALSE)
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
  cat(sprintf("  %s members, %d features\n", count(nrow(X)), ncol(X)))

  cat(sprintf("Cross-validating (%d folds)...\n", args$folds))
  fold <- make_folds(kit, args$folds)
  oof <- setNames(lapply(MODELS, function(m) numeric(length(y))), MODELS)
  m0 <- m1 <- numeric(length(y))
  for (k in seq_len(args$folds)) {
    test <- fold == k
    pred <- fit_all(X[!test, ], y[!test], kit[!test])(X[test, ])
    for (m in MODELS) oof[[m]][test] <- pred$uplift[[m]]
    m0[test] <- pred$m0
    m1[test] <- pred$m1
    cat(sprintf("  fold %d done\n", k))
  }

  yc <- pmin(y, CAP)
  G <- dr_outcome(yc, kit, m0, m1)
  cat(sprintf("  check: mean DR uplift %s vs difference in means %s (capped outcome)\n",
              dollars(mean(G), 2), dollars(mean(yc[kit]) - mean(yc[!kit]), 2)))
  results <- compare_models(oof, G, yc, kit, ref = "T")
  fwrite(results[, lapply(.SD, function(v) if (is.numeric(v)) round(v) else v)],
         file.path(OUT, "model_comparison.csv"))
  print_comparison(results, "T")
  calib <- write_diagnostics(oof[[args$model]], G, "")
  cat(sprintf("\n%s model, predicted vs actual (DR) uplift by tenth:\n", args$model))
  print(as.data.frame(calib), row.names = FALSE)

  cat("\nFitting final models on all members and scoring 2026...\n")
  final <- fit_all(X, y, kit)
  variants <- list(`2026` = d$X26, ref2025 = X)
  for (v in names(variants)) {
    up <- final(variants[[v]])$uplift[[args$model]]
    rule <- rule_kits(up)
    n <- if (is.na(args$kits)) rule else args$kits
    chosen <- order(-up)[seq_len(n)]
    fwrite(data.table(ID = d$members$ID, uplift = round(up, 2),
                      send_kit = as.integer(seq_along(up) %in% chosen)),
           file.path(OUT, sprintf("scores_%s.csv", v)))
    path <- file.path(OUT, sprintf("submission_%s_%s_%d.csv", args$model, v, n))
    fwrite(data.table(ID = d$members$ID[chosen]), path, col.names = FALSE)
    cat(sprintf("  features %-7s: rule says %s kits; wrote %s kits (cost %s, predicted net gain %s) -> %s\n",
                v, count(rule), count(n), dollars(kit_cost(n)), dollars(sum(up[chosen]) - kit_cost(n)),
                basename(path)))
  }
}


main()
