# Step 1: link social media accounts to members, as many and as safely as possible.
#
# What the data looks like (checked on the 72,069 accounts whose email is a member's):
#   - Member emails are personal: firstname.N@personal-domain.
#   - A social account's email is either that personal email (exact match) or a
#     work email first.last[.N]@company, which spells out the full name.
#   - The Name field is always faithful to the member's name, in one of four
#     formats: "FIRST LAST", "F LAST", "FIRST L" or "The LASTs".
#
# Matching, from most to least certain:
#   1. email      the account email is a member's email.
#   2. name       the full name (from the work email, or from Name) or the partial
#                 name points to a single member. Members already linked are removed
#                 and the step is repeated, since each member has at most one account.
#   3. ambiguous  several members fit; each gets a share 1/k of the account.
#
# A validation hides the email of half of the email-matched accounts, runs the same
# matching, and checks how often it finds the right member.
#
# Run from the Team folder:  Rscript 2-group/code/01_social_match.R

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
CODE <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else "2-group/code"
source(file.path(CODE, "common.R"))

# Accounts that fit more members than this are left unmatched (each share would be < 1%).
MAX_CANDIDATES <- 100

# Columns used to find candidate members for each account format.
KEYS <- list(
  work_email = c("first", "last"),
  full_name = c("first", "last"),
  initial_last = c("fi", "last"),
  first_initial = c("first", "li"),
  the_family = "last"
)


# Adds `sid` (row number), `format` and the name parts used as matching keys.
parse_social <- function(social) {
  s <- copy(social)
  s[, sid := .I]
  s[, Name := trimws(Name)]
  s[, local := sub("@.*$", "", email)]
  # Member emails are firstname[.N]; only work emails have letters after the dot.
  s[, format := fcase(
    grepl("^[a-z'-]+[.][a-z'-]+([.][0-9]+)?$", local), "work_email",
    grepl("^The [A-Z'-]+s$", Name), "the_family",
    grepl("^[A-Z] [A-Z'-]+$", Name), "initial_last",
    grepl("^[A-Z'-]+ [A-Z]$", Name), "first_initial",
    grepl("^[A-Z'-]+ [A-Z'-]+$", Name), "full_name",
    default = "unparsed"
  )]
  s[, c("first", "last", "fi", "li") := NA_character_]
  s[format == "work_email",
    c("first", "last") := tstrsplit(toupper(sub("[.][0-9]+$", "", local)), ".", fixed = TRUE)]
  s[format == "full_name", c("first", "last") := tstrsplit(Name, " ", fixed = TRUE)]
  s[format == "initial_last", `:=`(fi = substr(Name, 1, 1), last = sub("^. ", "", Name))]
  s[format == "first_initial", `:=`(first = sub(" .$", "", Name), li = substring(Name, nchar(Name)))]
  s[format == "the_family", last := sub("^The (.*)s$", "\\1", Name)]
  s[]
}


# All (account, member) pairs where the member fits the account's name.
candidate_pairs <- function(accounts, pool) {
  pool <- pool[, .(ID, first, last, fi = substr(first, 1, 1), li = substr(last, 1, 1))]
  rbindlist(lapply(names(KEYS), function(f) {
    k <- KEYS[[f]]
    q <- accounts[format == f, c("sid", k), with = FALSE]
    n_fit <- pool[, .(n = .N), by = k]
    q <- n_fit[q, on = k][!is.na(n) & n <= MAX_CANDIDATES]
    pool[q, on = k, nomatch = NULL, allow.cartesian = TRUE, .(sid, ID)]
  }))
}


# Repeatedly links accounts that fit a single member, removing linked members from
# the other accounts. Returns the unique links and the remaining ambiguous pairs.
resolve <- function(pairs) {
  unique_links <- pairs[0]
  conflicts <- 0L
  repeat {
    pairs[, k := .N, by = sid]
    new <- pairs[k == 1L, .(sid, ID)]
    if (!nrow(new)) break
    # Two accounts pointing only to the same member: keep one, count the clash.
    conflicts <- conflicts + sum(duplicated(new$ID))
    new <- new[!duplicated(ID)]
    unique_links <- rbind(unique_links, new)
    pairs <- pairs[!(sid %in% new$sid) & !(ID %in% new$ID)]
  }
  pairs[, k := .N, by = sid]
  list(unique = unique_links, ambiguous = pairs[, .(sid, ID, w = 1 / k)], conflicts = conflicts)
}


# Links social accounts to members. Returns the links (sid, ID, w, tier) and the
# parsed accounts with the tier each one ended in.
match_social <- function(members, social) {
  s <- parse_social(social)
  s[, email_ID := members$ID[match(email, members$email)]]
  s[email == "", email_ID := NA_integer_]
  direct <- s[!is.na(email_ID), .(sid, ID = email_ID)]

  pool <- members[!(ID %in% direct$ID)]
  res <- resolve(candidate_pairs(s[is.na(email_ID)], pool))

  links <- rbind(
    direct[, .(sid, ID, w = 1, tier = "email")],
    res$unique[, .(sid, ID, w = 1, tier = "name")],
    res$ambiguous[, .(sid, ID, w, tier = "ambiguous")]
  )
  s[, tier := "unmatched"]
  s[links, on = "sid", tier := i.tier]
  list(links = links, accounts = s, conflicts = res$conflicts)
}


# One row per member. Ambiguous accounts contribute in proportion to their share,
# so soc_likes is the expected number of likes and soc_prob the chance of having an account.
social_features <- function(members, social, links) {
  l <- cbind(links, social[links$sid, .(Likes, Shares, Supporter = as.numeric(Supporter))])
  f <- l[, .(
    soc_tier = tier[1],
    soc_prob = 1 - prod(1 - w),
    soc_likes = sum(w * Likes),
    soc_shares = sum(w * Shares),
    soc_supporter = pmin(sum(w * Supporter), 1),
    soc_candidates = round(1 / max(w))
  ), by = ID]
  f <- f[members[, .(ID)], on = "ID"]
  f[is.na(soc_tier), `:=`(soc_tier = "none", soc_candidates = 0)]
  for (col in c("soc_prob", "soc_likes", "soc_shares", "soc_supporter")) {
    set(f, which(is.na(f[[col]])), col, 0)
  }
  f[]
}


# Hides the email of a share of the email-matched accounts and checks the matching
# finds the right member for them by name alone.
validate <- function(members, social, share = 0.5) {
  known <- which(social$email %in% members$email)
  hidden <- sample(known, round(share * length(known)))
  truth <- data.table(sid = hidden, true_ID = members$ID[match(social$email[hidden], members$email)])
  blind <- copy(social)
  blind[hidden, email := ""]
  res <- match_social(members, blind)

  acc <- res$accounts[truth, on = "sid", .(sid, format, tier, true_ID)]
  found <- res$links[acc, on = "sid", nomatch = NULL]
  per_account <- found[, .(right_w = sum(w[ID == true_ID]), in_candidates = any(ID == true_ID)),
                       by = sid]
  acc <- per_account[acc, on = "sid"]
  acc[is.na(right_w), `:=`(right_w = 0, in_candidates = FALSE)]
  acc[, .(
    accounts = .N,
    linked_unique = mean(tier == "name"),
    correct_if_unique = mean(right_w[tier == "name"] == 1),
    ambiguous = mean(tier == "ambiguous"),
    truth_among_candidates = mean(in_candidates[tier == "ambiguous"]),
    avg_share_on_truth = mean(right_w[tier == "ambiguous"]),
    unmatched = mean(tier == "unmatched")
  ), keyby = format]
}


main <- function() {
  set.seed(SEED)
  cat("Loading data...\n")
  members <- load_members()
  social <- load_social()
  donations <- load_donations()
  contacted <- load_contacted()

  cat("Matching social accounts to members...\n")
  res <- match_social(members, social)
  feats <- social_features(members, social, res$links)

  accounts <- dcast(res$accounts[, .N, by = .(format, tier)], format ~ tier, value.var = "N", fill = 0)
  setcolorder(accounts, intersect(c("format", "email", "name", "ambiguous", "unmatched"), names(accounts)))
  fwrite(accounts, file.path(OUT, "social_match_accounts.csv"))
  cat("\nSocial accounts by name format and how they were matched:\n")
  print(as.data.frame(accounts), row.names = FALSE)
  cat(sprintf("  conflicts (two accounts pointing only to the same member): %s\n", count(res$conflicts)))

  coverage <- feats[, .(members = .N, expected_accounts = round(sum(soc_prob))), keyby = soc_tier]
  fwrite(coverage, file.path(OUT, "social_match_coverage.csv"))
  cat("\nMembers by best link:\n")
  print(as.data.frame(coverage), row.names = FALSE)
  cat(sprintf("  members with a certain link: %s (part 1 used %s)\n",
              count(feats[soc_tier %in% c("email", "name"), .N]), count(feats[soc_tier == "email", .N])))

  cat("\nValidating on email-matched accounts with the email hidden...\n")
  validation <- validate(members, social)
  fwrite(validation, file.path(OUT, "social_match_validation.csv"))
  print(as.data.frame(validation[, lapply(.SD, function(x) if (is.double(x)) round(x, 3) else x)]),
        row.names = FALSE)

  # If name links are right, likes should predict the kit's effect as strongly as
  # they do for email links.
  out <- pilot_outcome(members$ID, donations, contacted)
  sig <- data.table(tier = feats$soc_tier, likes = feats$soc_likes, y = out$y, kit = out$kit)
  sig <- sig[tier %in% c("email", "name")]
  sig[, likes_band := cut(likes, c(-Inf, 0, 2, 5, 10, Inf), labels = c("0", "1-2", "3-5", "6-10", ">10"))]
  signal <- sig[, uplift_summary(y, kit), keyby = .(tier, likes_band)]
  signal[, `:=`(uplift = round(uplift, 2), se = round(se, 2))]
  fwrite(signal, file.path(OUT, "social_likes_uplift.csv"))
  cat("\nUplift per kit by likes, for email links vs name links (2025 pilot):\n")
  print(as.data.frame(signal), row.names = FALSE)

  fwrite(feats, file.path(OUT, "social_features.csv"))
  cat(sprintf("\nWrote %s\n", file.path(basename(GROUP), "outputs", "social_features.csv")))
}


main()
