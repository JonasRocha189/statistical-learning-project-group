# Uplift learners. Each one takes the training data (features X, 2025 donation y,
# pilot indicator kit) and returns a function that predicts the uplift
# E[donation | kit, x] - E[donation | no kit, x] for new members.

fit_lgb <- function(X, y, small = nrow(X) < 100000) {
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
      num_threads = THREADS,
      verbose = -1
    ),
    data = train_set,
    nrounds = 1000,
    valids = list(valid = valid_set),
    early_stopping_rounds = 20,
    verbose = -1
  )
}


# T-learner (part 1): one outcome model per group, uplift = difference.
# Weakness: the kit model only sees 20,000 members, so its errors dominate.
fit_t_learner <- function(X, y, kit) {
  mu1 <- fit_lgb(X[kit, ], y[kit])
  mu0 <- fit_lgb(X[!kit, ], y[!kit])
  function(Z) predict(mu1, Z) - predict(mu0, Z)
}


# X-learner (Kunzel et al., 2019), built for unbalanced groups like ours (2% kit).
# 1. Fit the outcome models mu0 (980k members) and mu1 (20k members).
# 2. Impute each member's individual effect using the *other* group's model:
#    kit members:    D1 = y - mu0(x)   (mu0 is very precise)
#    no-kit members: D0 = mu1(x) - y
# 3. Regress D1 and D0 on x and blend: tau = e * tau0 + (1 - e) * tau1, where
#    e = share of kit members. Almost all weight goes on tau1, which only
#    depends on the precise no-kit model.
fit_x_learner <- function(X, y, kit) {
  mu1 <- fit_lgb(X[kit, ], y[kit])
  mu0 <- fit_lgb(X[!kit, ], y[!kit])
  tau1 <- fit_lgb(X[kit, ], y[kit] - predict(mu0, X[kit, ]))
  tau0 <- fit_lgb(X[!kit, ], predict(mu1, X[!kit, ]) - y[!kit])
  e <- mean(kit)
  function(Z) e * predict(tau0, Z) + (1 - e) * predict(tau1, Z)
}


# Causal forest (Athey, Tibshirani & Wager, 2019), from the grf package.
# Trees split to maximise differences in the kit effect itself, with honest
# estimation. To keep it fast, all kit members are kept and the no-kit members
# are subsampled; the kit was random, so the propensity is known exactly.
fit_causal_forest <- function(X, y, kit, n_control = CF_CONTROLS) {
  control <- which(!kit)
  keep <- c(which(kit), control[sample.int(length(control), min(n_control, length(control)))])
  w <- as.numeric(kit[keep])
  cf <- causal_forest(
    X[keep, ], y[keep], w,
    W.hat = rep(mean(w), length(keep)),
    num.trees = CF_TREES,
    seed = SEED,
    num.threads = THREADS
  )
  function(Z) predict(cf, Z, num.threads = THREADS)$predictions
}


LEARNERS <- list(
  t_learner = fit_t_learner,
  x_learner = fit_x_learner,
  causal_forest = fit_causal_forest
)
