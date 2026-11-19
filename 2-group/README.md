# Part 2: the group solution

Code is in [code/](code/), results go to `outputs/` (not committed; rerun the scripts to rebuild them). Run every script from the Team folder:

```
Rscript 2-group/code/01_social_match.R
Rscript 2-group/code/02_uplift.R            # about 5 minutes
Rscript 2-group/code/03_kit_model.R --kits 446384   # about 10 minutes; writes the final submission
```

| Script | What it does | Main output |
|---|---|---|
| `common.R` | Paths, constants and data loaders shared by all scripts. | |
| `01_social_match.R` | Links social media accounts to members by email, then by name. | `social_features.csv` |
| `uplift_lib.R` | Features, LightGBM fitting and the doubly robust evaluation shared by steps 2 and 3. | |
| `02_uplift.R` | Compares four uplift models on the same cross-validation, then scores 2026. | `model_comparison.csv`, `submission_<model>_<features>_<kits>.csv` |
| `03_kit_model.R` | Compares six versions of the kit-group model, then scores 2026 with the best. | `model_comparison_kit_model.csv`, `submission_anchor_bag_2026_<kits>.csv` |

**Final submission:** the anchored, bagged hurdle model cut at 446,384 kits. The script's cost rule picks 437,761 kits; we cut at 446,384 to match the step 2 count, after test uploads showed shorter lists lose money. It scored $21,259,370, first on the interim leaderboard.

## Step 2: uplift models

Every model predicts each member's 2025 donation with and without a kit (LightGBM). The features are part 1's plus a longer donation history, newsletter timing and the social features from step 1, 27 in all. Gifts are capped at $1,000 for training: larger one-off gifts only appear in the no-kit group and rarely repeat.

| Model | How it predicts each member's donation |
|---|---|
| T | One regression on the amount per group (part 1's method). |
| hurdle | Per group: probability of giving × expected gift if they give. The kit raises both (12% → 31% give, $52 → $116 average gift). |
| hurdle_X | X-learner on top of the hurdle: the kit group's effect is imputed with the more precise no-kit model. |
| blend | Average of hurdle and hurdle_X. |

**Evaluation.** Part 1 measured the net gain inside the top n by comparing the kit and no-kit members there, which is noisy. Here each member gets a doubly robust estimate of their own uplift, built from models that never saw them. Because kits were random at a known 2% rate, summing it over any group of members gives an unbiased estimate of what kits would add there. Differences between models come from a paired bootstrap (200 resamples).

Cross-validated net gain against sending no kits, top 452,000 members:

| Model | Net gain | vs T (95% interval) |
|---|---|---|
| T | $14.18M | |
| **hurdle** | **$14.58M** | **+$406k [+$101k, +$719k]** |
| hurdle_X | $14.33M | +$157k [−$42k, +$369k] |
| blend | $14.56M | +$383k [+$121k, +$665k] |

The hurdle model is the only one clearly better than T. Its predicted uplift matches the actual uplift in every tenth of members (`uplift_by_decile.csv`).

On the leaderboard, the hurdle model with 2026 features (446,384 kits) scored $21,114,080: +$440k over part 1, in line with the cross-validation.

## Step 3: a stronger kit-group model

`03_kit_model.R` keeps the same no-kit model and tests three fixes for the kit side, which learns from only 20,000 members: **anchoring** (the kit model starts from the no-kit prediction and learns only the correction), **bagging** (10 fits on 80% subsamples, averaged) and a **larger model** (31 leaves instead of 8).

Cross-validated net gain at 452,000 kits:

| Kit model | Net gain | vs hurdle (95% interval) |
|---|---|---|
| hurdle (step 2) | $14.61M | |
| hurdle, bagged | $14.48M | −$121k [−$364k, +$171k] |
| anchored | $14.62M | +$12k [−$221k, +$237k] |
| **anchored, bagged** | **$14.69M** | **+$78k [−$165k, +$287k]** |
| anchored, larger | $14.51M | −$80k [−$387k, +$211k] |
| anchored, larger, bagged | $14.62M | +$12k [−$269k, +$296k] |

No variant is clearly better than the step 2 hurdle model; every interval includes zero. The pilot can resolve differences of about ±$250k, not smaller ones, so from here the leaderboard is the sharper test. Anchored and bagged is the best point estimate.

**2026 scoring.** Step 2 writes two versions. `2026` rebuilds the donation features with 2025 gifts included; the 20,000 pilot members keep their pre-pilot features, because their 2025 gifts were partly caused by the kit. `ref2025` uses the training features unchanged. In each, kits go to every member whose predicted uplift beats the kit's marginal cost ($5, then $25).

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
