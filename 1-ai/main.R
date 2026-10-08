# AI version of the fundraising assignment: who should receive a thank-you kit in 2026?
#
# The 2025 pilot sent kits to 20,000 randomly chosen members, so the difference in
# 2025 donations between kit and no-kit members is the causal effect of the kit.
# We model that effect per member (uplift) and send a kit wherever the expected
# extra donation is larger than what the kit costs.
#
# Run from the Team folder:  Rscript 1-ai/main.R

suppressPackageStartupMessages({
  library(data.table)
  library(lightgbm)
})

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
HERE <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else "1-ai"
DATA <- file.path(dirname(HERE), "Data-Fundraising-3.3")
OUT <- file.path(HERE, "outputs")

CHEAP_KITS <- 60000
CHEAP_COST <- 5
FULL_COST <- 25
PILOT_YEAR <- 2025
CAMPAIGN_YEAR <- 2026
SEED <- 60603

FEATURES <- c(
  "woman", "age", "salary", "education", "city", "tenure",
  "n_don", "sum_don", "avg_don", "max_don", "amount_lag1", "amount_lag2",
  "years_since_last", "don_per_year",
  "news_reads", "on_social", "likes", "shares", "supporter"
)

dollars <- function(x, digits = 0) {
  paste0("$", formatC(x, format = "f", digits = digits, big.mark = ","))
}
count <- function(x) formatC(x, format = "d", big.mark = ",")


load_data <- function() {
  members <- fread(file.path(DATA, "MembersList.csv"))
  donations <- fread(file.path(DATA, "DonationHistory.csv"))
  # Member 9000000 is stored as 9e+06 in both files, so fread reads the IDs as doubles.
  members[, ID := as.integer(ID)]
  donations[, ID := as.integer(ID)]
  news <- fread(file.path(DATA, "NewsletterRead.csv"))
  social <- fread(file.path(DATA, "SocialNetworkUsage.csv"))
  contacted <- fread(file.path(DATA, "ListContacted2025.csv"), header = FALSE)[[1]]
  list(members = members, donations = donations, news = news, social = social,
       contacted = contacted)
}


# One row per member, describing what was known before the `ref_year` campaign.
#
# Donation features only use years before `ref_year` and are expressed relative
# to it, so the same columns can be built for 2025 (training) and 2026 (scoring).
build_features <- function(d, ref_year) {
  members <- d$members
  X <- members[, .(
    ID,
    woman = Woman,
    age = Age,
    salary = Salary,
    education = match(Education, c("Elementary", "High School", "University / College")) - 1L,
    city = as.integer(factor(City)) - 1L,
    tenure = ref_year - Joined
  )]

  past <- d$donations[Yr < ref_year]
  hist <- past[, .(
    n_don = .N,
    sum_don = sum(Amount),
    avg_don = mean(Amount),
    max_don = max(Amount),
    amount_lag1 = sum(Amount[Yr == ref_year - 1L]),
    amount_lag2 = sum(Amount[Yr == ref_year - 2L]),
    years_since_last = ref_year - max(Yr)
  ), by = ID]
  X <- hist[X, on = "ID"]
  donation_cols <- c("n_don", "sum_don", "avg_don", "max_don", "amount_lag1", "amount_lag2")
  for (col in donation_cols) set(X, which(is.na(X[[col]])), col, 0)
  X[is.na(years_since_last), years_since_last := 99]
  X[, don_per_year := n_don / tenure]

  # Newsletter and social data only exist for 2025. The pilot shows the kit did
  # not change them, so they are used as a stable description of engagement.
  reads <- rowSums(d$news[, !"email"])
  X[, news_reads := reads[match(members$email, d$news$email)]]

  # Only social accounts with an email that matches a member are used; names
  # are not unique enough to match the rest safely.
  soc <- unique(d$social[!is.na(email) & email != ""], by = "email")
  row <- match(members$email, soc$email)
  X[, on_social := as.integer(!is.na(row))]
  X[, likes := fcoalesce(soc$Likes[row], 0L)]
  X[, shares := fcoalesce(soc$Shares[row], 0L)]
  X[, supporter := fcoalesce(as.integer(soc$Supporter[row]), 0L)]
  X
}


fit_model <- function(X, y) {
  # The kit group is 50x smaller than the no-kit group, so its model is kept simpler.
  small <- nrow(X) < 100000
  # A tenth of the rows is held out to decide when to stop adding trees.
  valid <- sample.int(nrow(X)) <= 0.1 * nrow(X)
  train_set <- lgb.Dataset(X[!valid, ], label = y[!valid], categorical_feature = "city")
  valid_set <- lgb.Dataset.create.valid(train_set, X[valid, ], label = y[valid])
  lgb.train(
    params = list(
      objective = "regression",
      learning_rate = 0.05,
      num_leaves = if (small) 8 else 31,
      min_data_in_leaf = if (small) 100 else 200,
      lambda_l2 = 1.0,
      seed = SEED,
      deterministic = TRUE,
      force_row_wise = TRUE,
      verbose = -1
    ),
    data = train_set,
    nrounds = 400,
    valids = list(valid = valid_set),
    early_stopping_rounds = 10,
    verbose = -1
  )
}


# Two models: expected donation with a kit and without. Uplift is the difference.
fit_uplift <- function(X, y, kit) {
  with_kit <- fit_model(X[kit, ], y[kit])
  without_kit <- fit_model(X[!kit, ], y[!kit])
  function(Z) predict(with_kit, Z) - predict(without_kit, Z)
}


kit_cost <- function(n) {
  CHEAP_COST * pmin(n, CHEAP_KITS) + FULL_COST * pmax(n - CHEAP_KITS, 0)
}


# Rank by uplift and send while the next kit earns more than it costs.
choose_recipients <- function(uplift) {
  ranked <- order(-uplift)
  marginal_cost <- ifelse(seq_along(ranked) <= CHEAP_KITS, CHEAP_COST, FULL_COST)
  n_send <- sum(uplift[ranked] > marginal_cost)
  ranked[seq_len(n_send)]
}


# Net gain of sending kits to the top-n members, measured on the pilot.
#
# For the top-n members by predicted uplift, the real effect of the kit is the
# average 2025 donation of those who got a kit minus those who did not.
profit_curve <- function(uplift, y, kit) {
  ranked <- order(-uplift)
  y <- y[ranked]
  kit <- kit[ranked]
  n <- seq_along(y)
  n_kit <- cumsum(kit)
  mean_kit <- cumsum(y * kit) / pmax(n_kit, 1)
  mean_no_kit <- cumsum(y * !kit) / pmax(n - n_kit, 1)
  extra_donations <- n * (mean_kit - mean_no_kit)
  curve <- data.table(
    kits_sent = n,
    extra_donations = extra_donations,
    kit_cost = kit_cost(n),
    net_gain = extra_donations - kit_cost(n)
  )
  # The first rows contain too few pilot members to be reliable.
  curve[n_kit >= 200]
}


parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- list(score_year = CAMPAIGN_YEAR, folds = 5L)
  usage <- "Usage: Rscript 1-ai/main.R [--score-year 2025|2026] [--folds N]"
  i <- 1
  while (i <= length(args)) {
    value <- suppressWarnings(as.integer(args[i + 1]))
    if (args[i] == "--score-year" && value %in% c(PILOT_YEAR, CAMPAIGN_YEAR)) {
      opts$score_year <- value
    } else if (args[i] == "--folds" && !is.na(value) && value >= 2) {
      opts$folds <- value
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
  d <- load_data()
  features <- build_features(d, PILOT_YEAR)
  ids <- features$ID
  X <- as.matrix(features[, ..FEATURES])
  given <- d$donations[Yr == PILOT_YEAR]
  y <- fcoalesce(as.numeric(given$Amount[match(ids, given$ID)]), 0)
  kit <- ids %in% d$contacted
  average_uplift <- mean(y[kit]) - mean(y[!kit])
  cat(sprintf("  %s members, %s received a kit in %d\n", count(length(ids)), count(sum(kit)),
              PILOT_YEAR))
  cat(sprintf("  average %d donation: %s with kit, %s without (uplift %s)\n", PILOT_YEAR,
              dollars(mean(y[kit]), 2), dollars(mean(y[!kit]), 2), dollars(average_uplift, 2)))

  # Honest check: every member is scored by models that never saw them.
  cat(sprintf("Cross-validating (%d folds)...\n", args$folds))
  fold <- integer(length(ids))
  for (group in list(which(kit), which(!kit))) {
    fold[group] <- sample(rep_len(seq_len(args$folds), length(group)))
  }
  oof <- numeric(length(ids))
  for (k in seq_len(args$folds)) {
    test <- fold == k
    predict_uplift <- fit_uplift(X[!test, ], y[!test], kit[!test])
    oof[test] <- predict_uplift(X[test, ])
  }

  curve <- profit_curve(oof, y, kit)
  fwrite(round(curve[seq(1, .N, by = 1000)]), file.path(OUT, "profit_curve.csv"))

  decile <- ceiling(10 * frank(-oof, ties.method = "first") / length(oof))
  calibration <- data.table(decile, pred = oof, y, kit)[, .(
    predicted_uplift = round(mean(pred), 2),
    actual_uplift = round(mean(y[kit]) - mean(y[!kit]), 2)
  ), keyby = decile]
  fwrite(calibration, file.path(OUT, "uplift_by_decile.csv"))
  cat("\nPredicted vs actual uplift, from best (1) to worst (10) tenth of members:\n")
  print(as.data.frame(calibration), row.names = FALSE)

  n_policy <- length(choose_recipients(oof))
  cat(sprintf("\nNet gain measured on the %d pilot (extra donations minus kit cost):\n",
              PILOT_YEAR))
  report <- function(label, n, gain) {
    cat(sprintf("  %-18s %9s kits -> %12s\n", label, count(n), dollars(gain)))
  }
  checks <- list(n_policy, CHEAP_KITS, length(ids))
  names(checks) <- c("model's choice", sprintf("top %s only", count(CHEAP_KITS)), "everyone")
  for (label in names(checks)) {
    row <- curve[kits_sent == checks[[label]]]
    if (nrow(row)) report(label, row$kits_sent, row$net_gain)
  }
  report("random members", n_policy, n_policy * average_uplift - kit_cost(n_policy))

  cat(sprintf("\nFitting final models and scoring members for %d...\n", args$score_year))
  predict_uplift <- fit_uplift(X, y, kit)
  X_score <- if (args$score_year == PILOT_YEAR) {
    X
  } else {
    as.matrix(build_features(d, args$score_year)[, ..FEATURES])
  }
  uplift <- predict_uplift(X_score)
  chosen <- choose_recipients(uplift)
  expected <- sum(uplift[chosen]) - kit_cost(length(chosen))

  suffix <- if (args$score_year == CAMPAIGN_YEAR) "" else paste0("_features", args$score_year)
  submission_path <- file.path(OUT, paste0("submission", suffix, ".csv"))
  scores <- data.table(ID = ids, predicted_uplift = round(uplift, 2), send_kit = 0L)
  scores[chosen, send_kit := 1L]
  fwrite(scores, file.path(OUT, paste0("scores", suffix, ".csv")))
  # Same layout as ListContacted2025.csv: one member ID per line, no header.
  fwrite(scores[chosen, .(ID)], submission_path, col.names = FALSE)

  cat(sprintf("  send %s kits (cost %s)\n", count(length(chosen)),
              dollars(kit_cost(length(chosen)))))
  cat(sprintf("  predicted net gain: %s\n", dollars(expected)))
  cat(sprintf("  wrote %s\n", file.path(basename(HERE), "outputs", basename(submission_path))))
}


main()
