# Data loading and feature engineering for the group solution.
#
# Every feature describes a member as known *before* the campaign of `ref_year`:
# ref_year = 2025 builds the training table (outcome = 2025 donation, pilot kit),
# ref_year = 2026 builds the table we score for the real decision.

load_data <- function(data_dir) {
  members <- fread(file.path(data_dir, "MembersList.csv"))
  donations <- fread(file.path(data_dir, "DonationHistory.csv"))
  # Member 9000000 is stored as 9e+06, so fread reads the ID column as doubles.
  members[, ID := as.integer(ID)]
  donations[, ID := as.integer(ID)]
  list(
    members = members,
    donations = donations,
    news = fread(file.path(data_dir, "NewsletterRead.csv")),
    social = fread(file.path(data_dir, "SocialNetworkUsage.csv")),
    contacted = fread(file.path(data_dir, "ListContacted2025.csv"), header = FALSE)[[1]]
  )
}


# Link social accounts to members: first by email, then by full name for the
# accounts left over, but only when the name belongs to exactly one member and
# exactly one leftover account (otherwise we cannot tell who is who).
match_social <- function(members, social) {
  social <- copy(social)
  social[, row := .I]
  social[, member := match(email, members$email)]
  social[email == "", member := NA_integer_]

  full_name <- toupper(trimws(paste(trimws(members$FirstName), trimws(members$LastName))))
  unique_names <- full_name[!full_name %in% full_name[duplicated(full_name)]]
  taken <- social[!is.na(member), member]
  left <- social[is.na(member)]
  left[, name := toupper(trimws(Name))]
  left <- left[!name %in% name[duplicated(name)] & name %in% unique_names]
  left[, member := match(name, full_name)]
  left <- left[!member %in% taken]
  social[left, on = "row", `:=`(member = i.member, by_name = TRUE)]
  social[is.na(by_name), by_name := FALSE]
  social[!is.na(member)]
}


build_features <- function(d, ref_year, social_link) {
  members <- d$members
  X <- members[, .(
    ID,
    woman = Woman,
    age = Age,
    salary = Salary,
    education = match(Education, c("Elementary", "High School", "University / College")) - 1L,
    city = match(City, c("Rural", "Suburban", "City", "Downtown")) - 1L,
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
    n_don_last3 = sum(Yr >= ref_year - 3L),
    years_since_last = ref_year - max(Yr),
    years_since_first = ref_year - min(Yr)
  ), by = ID]
  X <- hist[X, on = "ID"]
  zero_cols <- c("n_don", "sum_don", "avg_don", "max_don", "amount_lag1", "amount_lag2",
                 "n_don_last3", "years_since_first")
  for (col in zero_cols) set(X, which(is.na(X[[col]])), col, 0)
  X[is.na(years_since_last), years_since_last := 99]
  X[, don_per_year := n_don / pmax(tenure, 1)]

  # Newsletter reads are 2025 data. The pilot shows the kit did not change them
  # (1.76 vs 1.77 issues), so we treat them as a stable trait of the member.
  news <- as.matrix(d$news[match(members$email, d$news$email), !"email"])
  X[, news_reads := rowSums(news)]
  X[, news_reads_h2 := rowSums(news[, 7:12])]

  row <- match(seq_len(nrow(members)), social_link$member)
  X[, on_social := as.integer(!is.na(row))]
  X[, likes := fcoalesce(social_link$Likes[row], 0L)]
  X[, shares := fcoalesce(social_link$Shares[row], 0L)]
  X[, supporter := fcoalesce(as.integer(social_link$Supporter[row]), 0L)]
  X
}


FEATURES <- c(
  "woman", "age", "salary", "education", "city", "tenure",
  "n_don", "sum_don", "avg_don", "max_don", "amount_lag1", "amount_lag2",
  "n_don_last3", "years_since_last", "years_since_first", "don_per_year",
  "news_reads", "news_reads_h2", "on_social", "likes", "shares", "supporter"
)
