# Fundraising: who should receive a thank-you kit?

Team assignment #1 for MATH 60603 Statistical Learning (Autumn 2026), worth 30% of the grade. The full brief is in [Group_Assignment.pdf](Group_Assignment.pdf).

## The problem

A charitable foundation with 1,000,000 members is preparing its 2026 campaign. It can mail members a thank-you kit that encourages them to donate. We decide how many kits to send and to whom, to maximize **donations minus kit cost**.

- The first 60,000 kits cost $5 each; every kit after that costs $25.
- Members can donate whether or not they receive a kit, so a kit is only worth sending if it brings in more *extra* money than it costs.
- In 2025 the foundation sent a similar kit to 20,000 randomly chosen members. That pilot is what lets us measure the kit's effect.

The assignment has two parts, and the point is to compare them:

| Part | Folder | What it is |
|---|---|---|
| 1 | [1-ai/](1-ai/) | The solution an AI produces when used by someone with no data science training. |
| 2 | [2-group/](2-group/) | The team's own solution, using course knowledge (AI still allowed). In progress; see its README. |

The code is in R, which the brief says is rewarded. An earlier Python version of part 1 is kept in [python/](python/) for reference; it uses the same approach and is no longer maintained.

Decisions are uploaded as a CSV of member IDs to <https://edge.hec.ca/play> (maximum 99 uploads per team). Each upload shows the net money raised on an interim leaderboard; the final ranking uses a hidden copy of the data.

Deliverables: progress discussion in session 7 (AI solutions already submitted), oral presentation in sessions 11–12 with questions to each member, and the slides.

## Data

The files are in [Data-Fundraising-3.3/](Data-Fundraising-3.3/).

| File | Rows | Contents |
|---|---|---|
| `MembersList.csv` | 1,000,000 | ID, name, email, gender, age, salary, education, city type, year joined |
| `DonationHistory.csv` | 650,096 | One row per member per year donated, 2014–2025, with the amount |
| `NewsletterRead.csv` | 1,000,000 | Which of the 12 monthly newsletters each member read in 2025, keyed by email |
| `SocialNetworkUsage.csv` | 300,000 | Shares, likes and a supporter flag for 2025; only 72,069 rows have an email matching a member |
| `ListContacted2025.csv` | 20,000 | IDs of the members who received the 2025 pilot kit (no header) |

Things to know before using them:

- Member 9000000 is written as `9e+06` in `MembersList.csv` and `DonationHistory.csv`, so tools read the ID column as decimal numbers unless it is converted back to integers.
- 181,313 members joined in 2024, about twice as many as any other year.
- The newsletter and social data are from 2025, the same year as the pilot. Kit and no-kit members read the newsletter equally often (1.76 vs 1.77 issues), so the kit does not seem to have changed it.

## How to run

The project needs R (tested with 4.4.2) and two packages, installed once from an R console:

```
install.packages(c("data.table", "lightgbm"))
```

Then, from this folder:

```
Rscript 1-ai/main.R
```

The run takes about a minute and a half and writes its results to `1-ai/outputs/`. Results are identical from one run to the next (fixed seed).

Options:

| Option | Effect |
|---|---|
| `--score-year 2026` | Default. Members are scored using their donations up to 2025. Writes `submission.csv`. |
| `--score-year 2025` | Members are scored using their donations up to 2024, exactly as in training. Writes `submission_features2025.csv`. |
| `--folds N` | Number of cross-validation folds (default 5). |

Output files:

| File | Contents |
|---|---|
| `submission.csv` | The decision to upload: one member ID per line, no header (same layout as `ListContacted2025.csv`). |
| `scores.csv` | Every member with their predicted uplift and whether they get a kit. |
| `uplift_by_decile.csv` | Predicted vs actual uplift for each tenth of members, from the cross-validation. |
| `profit_curve.csv` | Net gain measured on the pilot for sending kits to the top-n members, in steps of 1,000. |

## Part 1: the AI version

Produced by Claude Opus 5.5 (in Claude Code), given only the assignment PDF and the data folder and asked to build the project. No modelling guidance came from the team.

### Approach

1. **Measure the kit's effect from the pilot.** Kits were assigned at random in 2025, so the difference in 2025 donations between kit and no-kit members is the effect of the kit: $36.48 vs $6.35 per member, an average uplift of $30.13.
2. **Describe each member** using only what was known before the campaign: demographics, years of membership, donation history (number, total, average, largest, last two years, years since last gift), newsletter reads, and social activity where the email matches.
3. **Predict uplift per member** with two gradient-boosting models (LightGBM): one predicts the 2025 donation of members who got a kit, the other of members who did not. Uplift is the difference between the two predictions.
4. **Decide.** Rank members by predicted uplift. Send a kit to the first 60,000 if their uplift is above $5, and to everyone after that whose uplift is above $25.
5. **Score for 2026.** Training describes members as of the end of 2024; for the 2026 decision the same features are rebuilt one year later, using donations up to 2025.

### Results

The check below is cross-validated: every member is scored by models that never saw their data, then the real 2025 pilot donations are compared within each group.

| Tenth of members (1 = highest predicted uplift) | Predicted uplift | Actual uplift |
|---|---|---|
| 1 | $81.22 | $85.39 |
| 2 | $58.32 | $58.27 |
| 3 | $43.67 | $44.77 |
| 4 | $33.48 | $33.09 |
| 5 | $25.46 | $27.04 |
| 6 | $19.67 | $17.41 |
| 7 | $15.65 | $17.08 |
| 8 | $12.43 | $10.04 |
| 9 | $9.10 | $5.51 |
| 10 | $4.34 | $3.46 |

Net gain (extra donations minus kit cost) of different decisions, measured on the 2025 pilot:

| Decision | Kits | Net gain |
|---|---|---|
| Model's choice | 454,540 | $13,756,904 |
| Same number of kits, random members | 454,540 | $3,531,470 |
| Only the 60,000 cheap kits, best members | 60,000 | $5,072,656 |
| Everyone | 1,000,000 | $6,329,296 |

The final decision for 2026 sends **461,805 kits** (cost $10,345,125) for a predicted net gain of $13,627,259. Stopping at the 60,000 discounted kits would leave most of the gain on the table: the kit pays for itself even at $25 for nearly half of the members.

These figures are *extra* money compared with sending no kits. The leaderboard shows total donations minus kit cost, so its numbers will not match them directly.

### Open points

- **Not yet uploaded.** Nothing has been submitted to the leaderboard. The expected CSV layout (header or no header) should be confirmed on the platform.
- **Two candidate submissions.** `submission.csv` and `submission_features2025.csv` agree on 446,048 members and differ on about 10,000–16,000 each. The leaderboard can tell which assumption fits better.
- **2025 donations of pilot members are inflated by the kit they received**, and these feed the 2026 scoring for those 20,000 members. Not corrected.
- **Assumes 2026 behaves like 2025**: same kit, same response. The pilot is the only evidence available.
- **The R and Python versions do not pick exactly the same members.** Same method, different gradient-boosting library: the R list has 461,805 members and the Python one 459,988, with 441,202 in common. Their predicted uplifts are 0.98 correlated; 85% of the members they disagree on have a predicted uplift within $5 of the $25 cut-off.
- **Social data is under-used.** Only the 72,069 accounts with a matching email are used. About 180,000 accounts share a name with a member, but names are not unique (780,527 distinct names for 1,000,000 members), so they were left out.

Suggested leaderboard comment: `AI only - Claude Opus 5.5 - R, two-model uplift (LightGBM), send if uplift > marginal cost, 461,805 kits`.
