# Part 2: the group solution

Code is in [code/](code/), results go to `outputs/` (not committed; rerun the scripts to rebuild them). Run every script from the Team folder:

```
Rscript 2-group/code/01_social_match.R
```

| Script | What it does | Main output |
|---|---|---|
| `common.R` | Paths, constants and data loaders shared by all scripts. | |
| `01_social_match.R` | Links social media accounts to members by email, then by name. | `social_features.csv` |

## Step 1: social matching

Part 1 linked a social account to a member only when the emails matched exactly: 72,069 members. The first-place team in round 1 also matched by name.

What the data shows (checked on the accounts whose email matches a member):

- Member emails are personal (`firstname.N@personal-domain`). A social account's email is either that same personal email or a work email `first.last[.N]@company`, which spells out the full name.
- The social `Name` is always the member's real name, written in one of four formats: `FIRST LAST`, `F LAST`, `FIRST L` or `The LASTs`.

Matching runs from most to least certain:

1. **email**: the account email is a member's email.
2. **name**: the full name (taken from the work email, or from `Name`) or the partial name fits a single member. Members already linked are removed and the step repeats, since a member has at most one account.
3. **ambiguous**: several members fit. Each gets a share of the account (1/k), so their social features are expected values.

Results:

| Link | Members |
|---|---|
| email | 72,069 |
| name (unique) | 118,040 |
| ambiguous (shared) | 528,384, worth 81,237 accounts in total |
| none | 281,507 |

Members with a certain link go from 72,069 to **190,109**.

**Validation.** Half of the email-matched accounts had their email hidden, and the matching was rerun on them. Every account it linked to a single member was linked correctly (100% for all four name formats). For the ambiguous ones, the true member was always among the candidates.

**The new links carry the same signal.** Likes predict the kit's effect as strongly for name links as for email links:

| Likes | Uplift, email links | Uplift, name links |
|---|---|---|
| 1–2 | $13 | $12 |
| 3–5 | $23 | $30 |
| 6–10 | $44 | $58 |
| more than 10 | $76 | $67 |
