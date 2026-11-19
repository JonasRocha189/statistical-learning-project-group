# Features, model fitting and evaluation shared by the uplift scripts (02, 03).
# Source after common.R.

suppressPackageStartupMessages(library(lightgbm))

P_KIT <- 0.02
# One-off gifts up to $10,000 exist only in the no-kit group (kit gifts stop at
# $750) and repeat rarely; capping them keeps the squared error from chasing them.
CAP <- 1000
EVAL_KITS <- 452000
BOOTSTRAP <- 200


# ------------------------------------------------------------------ features

# One row per member, using only donations before `ref_year`.
history_features <- function(members, donations, ref_year) {
  past <- donations[Yr < ref_year]
  setorder(past, ID, Yr)
  h <- past[, .(
    n_don = .N,
    sum_don = sum(Amount),
    avg_don = mean(Amount),
    max_don = max(Amount),
    last_amount = Amount[.N],
    lag1 = sum(Amount[Yr == ref_year - 1L]),
    lag2 = sum(Amount[Yr == ref_year - 2L]),
    lag3 = sum(Amount[Yr == ref_year - 3L]),
    n_last3 = sum(Yr >= ref_year - 3L),
    n_last5 = sum(Yr >= ref_year - 5L),
    years_since_last = ref_year - max(Yr),
    years_since_first = ref_year - min(Yr)
  ), by = ID]
  X <- h[members[, .(ID, Joined)], on = "ID"]
  for (col in setdiff(names(h), c("ID", "years_since_last", "years_since_first"))) {
    set(X, which(is.na(X[[col]])), col, 0)
  }
  X[is.na(years_since_last), `:=`(years_since_last = 99, years_since_first = 99)]
  X[, tenure := ref_year - Joined]
  X[, don_rate := n_don / pmax(tenure, 1)]
  X[, Joined := NULL]
  X[]
}

static_features <- function(members, news, social) {
  months <- setdiff(names(news), "email")
  reads <- as.matrix(news[, ..months])
  row <- match(members$email, news$email)
  S <- members[, .(
    ID,
    woman = Woman,
    age = Age,
    salary = Salary,
    education = match(Education, c("Elementary", "High School", "University / College")) - 1L,
    city = match(City, c("Rural", "Suburban", "City", "Downtown")) - 1L
  )]
  S[, news_reads := rowSums(reads)[row]]
  S[, news_h2 := rowSums(reads[, 7:12])[row]]
  S[, news_q4 := rowSums(reads[, 10:12])[row]]
  soc <- social[, .(ID, soc_prob, soc_likes, soc_shares, soc_supporter,
                    soc_certain = as.integer(soc_tier %in% c("email", "name")))]
  soc[S, on = "ID"]
}

build_X <- function(static, history) {
  X <- static[history, on = "ID"]
  X[, ID := NULL]
  as.matrix(X)
}

# Everything the uplift scripts need: training features (as of 2025), 2026
# scoring features, the 2025 outcome and the pilot flag, all in member order.
prepare_data <- function() {
  members <- load_members()
  donations <- load_donations()
  contacted <- load_contacted()
  social_path <- file.path(OUT, "social_features.csv")
  if (!file.exists(social_path)) stop("Run 2-group/code/01_social_match.R first.", call. = FALSE)
  static <- static_features(members, load_news(), fread(social_path))
  static <- static[match(members$ID, ID)]
  X <- build_X(static, history_features(members, donations, PILOT_YEAR))
  out <- pilot_outcome(members$ID, donations, contacted)
  # Pilot members keep their pre-pilot (2025) features: their 2025 gifts were
  # partly caused by the kit and would overstate them.
  X26 <- build_X(static, history_features(members, donations, CAMPAIGN_YEAR))
  X26[out$kit, ] <- X[out$kit, ]
  list(members = members, X = X, X26 = X26, y = out$y, kit = out$kit)
}

# Fold numbers, drawn separately within kit and no-kit members.
make_folds <- function(kit, k) {
  fold <- integer(length(kit))
  for (group in list(which(kit), which(!kit))) {
    fold[group] <- sample(rep_len(seq_len(k), length(group)))
  }
  fold
}


# ------------------------------------------------------------------ models

# LightGBM with early stopping on a random tenth of the rows. `init` is an
# optional starting prediction per row (raw scale): the trees then only learn
# the correction to it.
fit_lgb <- function(X, y, objective = "regression", small = FALSE,
                    leaves = if (small) 8 else 31, min_leaf = if (small) 100 else 200,
                    init = NULL, seed = SEED) {
  valid <- sample.int(nrow(X)) <= 0.1 * nrow(X)
  dtrain <- lgb.Dataset(X[!valid, , drop = FALSE], label = y[!valid], init_score = init[!valid],
                        categorical_feature = "city")
  dvalid <- lgb.Dataset.create.valid(dtrain, X[valid, , drop = FALSE], label = y[valid],
                                     init_score = init[valid])
  lgb.train(
    params = list(
      objective = objective,
      learning_rate = 0.05,
      num_leaves = leaves,
      min_data_in_leaf = min_leaf,
      feature_fraction = 0.9,
      lambda_l2 = 1.0,
      seed = seed,
      deterministic = TRUE,
      force_row_wise = TRUE,
      num_threads = 4,
      verbose = -1
    ),
    data = dtrain, nrounds = 1000, valids = list(valid = dvalid),
    early_stopping_rounds = 20, verbose = -1
  )
}


# ------------------------------------------------------------------ evaluation
#
# Each member gets a doubly robust (DR) estimate of their own uplift,
#     G = m1 - m0 + kit (y - m1) / 0.02 - (1 - kit) (y - m0) / 0.98,
# with m0, m1 from models that never saw that member. Kits were random with a
# known 2% rate, so the sum of G over any set of members is an unbiased estimate
# of what kits to that set would add, whatever m0 and m1 are; good m0, m1 only
# reduce the noise.

dr_outcome <- function(y, kit, m0, m1) {
  m1 - m0 + kit * (y - m1) / P_KIT - (!kit) * (y - m0) / (1 - P_KIT)
}

# Net gain of sending kits to the top n by `score`, for every n.
dr_curve <- function(score, G) {
  cumsum(G[order(-score)]) - kit_cost(seq_along(G))
}

# Part 1's measure, for comparison: kit minus no-kit mean inside the top n.
dim_gain <- function(score, y, kit, n) {
  top <- order(-score)[seq_len(n)]
  n * (mean(y[top][kit[top]]) - mean(y[top][!kit[top]])) - kit_cost(n)
}

# Paired bootstrap of (model - reference) DR net gain at n kits.
bootstrap_diff <- function(score, ref_score, G, n) {
  a <- order(-score)[seq_len(n)]
  b <- order(-ref_score)[seq_len(n)]
  only_a <- setdiff(a, b)
  only_b <- setdiff(b, a)
  # Members chosen by both cancel out; only the disagreements matter.
  replicate(BOOTSTRAP, {
    sum(G[only_a] * rpois(length(only_a), 1)) - sum(G[only_b] * rpois(length(only_b), 1))
  })
}

# One row per model: DR net gain at EVAL_KITS, best n, part 1 measure, and the
# bootstrap difference to the reference model.
compare_models <- function(oof, G, y, kit, ref) {
  rbindlist(lapply(names(oof), function(m) {
    curve <- dr_curve(oof[[m]], G)
    diff <- if (m == ref) NA else bootstrap_diff(oof[[m]], oof[[ref]], G, EVAL_KITS)
    data.table(
      model = m,
      dr_gain_at_eval = curve[EVAL_KITS],
      best_kits = which.max(curve),
      dr_gain_best = max(curve),
      part1_measure_at_eval = dim_gain(oof[[m]], y, kit, EVAL_KITS),
      vs_ref = if (m == ref) NA else mean(diff),
      vs_ref_low = if (m == ref) NA else unname(quantile(diff, 0.025)),
      vs_ref_high = if (m == ref) NA else unname(quantile(diff, 0.975))
    )
  }))
}

print_comparison <- function(results, ref) {
  cat(sprintf("\nCross-validated net gain vs. sending no kits (DR estimate), top %s members:\n",
              count(EVAL_KITS)))
  for (i in seq_len(nrow(results))) {
    r <- results[i]
    extra <- if (is.na(r$vs_ref)) "" else sprintf("  vs %s: %s [%s, %s]", ref,
      dollars(r$vs_ref), dollars(r$vs_ref_low), dollars(r$vs_ref_high))
    cat(sprintf("  %-15s %s  (best %s at %s kits)%s\n", r$model, dollars(r$dr_gain_at_eval),
                dollars(r$dr_gain_best), count(r$best_kits), extra))
  }
}

# Writes the decile calibration and profit curve of one model's OOF scores.
write_diagnostics <- function(score, G, suffix) {
  decile <- ceiling(10 * frank(-score, ties.method = "first") / length(score))
  calib <- data.table(decile, pred = score, G)[, .(
    predicted = round(mean(pred), 2), actual_dr = round(mean(G), 2)), keyby = decile]
  fwrite(calib, file.path(OUT, sprintf("uplift_by_decile%s.csv", suffix)))
  curve <- dr_curve(score, G)
  steps <- seq(1000, length(curve), by = 1000)
  fwrite(data.table(kits_sent = steps, net_gain = round(curve[steps])),
         file.path(OUT, sprintf("profit_curve%s.csv", suffix)))
  calib
}

# Kits go to every member whose uplift beats the marginal cost ($5, then $25).
rule_kits <- function(uplift) {
  ranked <- sort(uplift, decreasing = TRUE)
  sum(ranked > ifelse(seq_along(ranked) <= CHEAP_KITS, CHEAP_COST, FULL_COST))
}
