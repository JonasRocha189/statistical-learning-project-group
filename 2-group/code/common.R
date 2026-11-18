# Shared paths, constants and loaders for the group solution (part 2).
#
# Each script defines CODE (its own folder) and then sources this file:
#   file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
#   CODE <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else "2-group/code"
#   source(file.path(CODE, "common.R"))

suppressPackageStartupMessages(library(data.table))

GROUP <- dirname(CODE)
DATA <- file.path(dirname(GROUP), "Data-Fundraising-3.3")
OUT <- file.path(GROUP, "outputs")
dir.create(OUT, showWarnings = FALSE)

CHEAP_KITS <- 60000
CHEAP_COST <- 5
FULL_COST <- 25
PILOT_YEAR <- 2025
CAMPAIGN_YEAR <- 2026
SEED <- 60603


load_members <- function() {
  m <- fread(file.path(DATA, "MembersList.csv"))
  # Member 9000000 is stored as 9e+06, so fread reads the IDs as doubles.
  m[, ID := as.integer(ID)]
  m[, `:=`(first = toupper(trimws(FirstName)), last = toupper(trimws(LastName)))]
  m[]
}

load_donations <- function() {
  d <- fread(file.path(DATA, "DonationHistory.csv"))
  d[, ID := as.integer(ID)]
  d[]
}

load_social <- function() fread(file.path(DATA, "SocialNetworkUsage.csv"))

load_news <- function() fread(file.path(DATA, "NewsletterRead.csv"))

load_contacted <- function() {
  fread(file.path(DATA, "ListContacted2025.csv"), header = FALSE)[[1]]
}


# 2025 donation (0 if none) and pilot kit flag, aligned with `ids`.
pilot_outcome <- function(ids, donations, contacted) {
  given <- donations[Yr == PILOT_YEAR]
  list(
    y = fcoalesce(as.numeric(given$Amount[match(ids, given$ID)]), 0),
    kit = ids %in% contacted
  )
}

# Difference in mean outcome between kit and no-kit members, with its standard error.
uplift_summary <- function(y, kit) {
  list(
    n = length(y),
    n_kit = sum(kit),
    uplift = mean(y[kit]) - mean(y[!kit]),
    se = sqrt(var(y[kit]) / sum(kit) + var(y[!kit]) / sum(!kit))
  )
}

kit_cost <- function(n) {
  CHEAP_COST * pmin(n, CHEAP_KITS) + FULL_COST * pmax(n - CHEAP_KITS, 0)
}

dollars <- function(x, digits = 0) {
  paste0("$", formatC(x, format = "f", digits = digits, big.mark = ","))
}
count <- function(x) formatC(x, format = "d", big.mark = ",")
pct <- function(x, digits = 1) paste0(formatC(100 * x, format = "f", digits = digits), "%")
