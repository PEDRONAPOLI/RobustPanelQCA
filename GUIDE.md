# A practical guide to panel fsQCA with RobustPanelQCA

This guide assumes no prior knowledge of QCA or of R. It explains what the
method does, what every number in the output means, how to choose the
thresholds, and what to do when something goes wrong.

If you just want to run the analysis, follow the Quick start in
[README.md](README.md) and come back here to interpret the results.

---

## 1. What QCA actually does

Regression asks: *if this variable goes up by one unit, how does the outcome
change on average, holding everything else constant?*

QCA asks a different question: *which combinations of conditions are enough to
produce the outcome, and are any of them indispensable?*

Three ideas follow from that, and they are the reason to use QCA at all:

**Conjunctural causation.** A condition may only matter in the presence of
others. Money without talent may achieve nothing; money with talent may achieve
a lot. QCA works with combinations, not with isolated variables.

**Equifinality.** There can be several different routes to the same outcome. A
region can build a thriving ecosystem through infrastructure and knowledge, or
through finance and talent. Regression looks for one best-fitting model; QCA
expects several paths and reports all of them.

**Asymmetry.** The explanation for a high outcome is not the mirror image of the
explanation for a low outcome. If you want to explain the absence of the
outcome, that is a separate analysis (calibrate a negated outcome and run it
again).

Every case has a **membership score** between 0 and 1 in each condition: 0 means
fully out of the set, 1 means fully in, 0.5 is the point of maximum ambiguity.
That is what "fuzzy set" means here.

---

## 2. Calibration: from your data to fuzzy sets

Your raw data is in R$, or points, or percentages. QCA needs membership scores
between 0 and 1. Converting one to the other is **calibration**, and it is the
step that most affects your results.

This package uses percentile anchors, which is the pragmatic default when you
have no external benchmark:

| Anchor | Default | Meaning |
|---|---|---|
| Full exclusion | 10th percentile | below this, membership ≈ 0 |
| Crossover | 50th percentile | maximum ambiguity, membership = 0.5 |
| Full inclusion | 90th percentile | above this, membership ≈ 1 |

```r
data_cal <- calibrate_panel(data, vars = c(conditions, outcome))
# or with different anchors:
data_cal <- calibrate_panel(data, vars = c(conditions, outcome),
                            probs = c(0.20, 0.50, 0.80))
```

**Three things to know.**

*Calibration is relative to your sample.* The 90th percentile of your data may
not be "high" in any absolute sense. If theory or an external standard gives you
substantive anchors (a poverty line, a regulatory threshold, a known industry
benchmark), those are better than percentiles — and you should say in your paper
which you used and why.

*It is applied to the pooled panel.* All periods share the same anchors, so a
score of 0.8 means the same thing in period 1 and period 3. This is what makes
comparison over time meaningful. The alternative — recalibrating within each
period — would measure something different (position relative to that period's
peers) and would make between-period metrics uninterpretable.

*A variable with almost no variation cannot be calibrated.* If the 10th and
90th percentiles are the same value, there is nothing to calibrate, and the
package warns you and returns `NA`. Drop that condition.

---

## 3. Necessity: what has to be there

A condition is **necessary** when the outcome essentially never occurs without
it. In set terms, the outcome is a subset of the condition.

```r
necessity_test(data_cal, outcome, conditions, params)
```

| Column | Meaning |
|---|---|
| `condition` | The condition. A `~` prefix means its **absence** |
| `inclN` | Consistency of the necessity claim. **≥ 0.90** is the usual bar |
| `covN` | Coverage — how non-trivial the claim is |
| `nec_flag` | Whether `inclN` passed your threshold |

**Always read `covN` alongside `inclN`.** A condition present at high levels in
literally every case will score a perfect `inclN` while explaining nothing: it
cannot discriminate between cases with and without the outcome. Low `covN` (say
below 0.5) is the signal for that. Reporting a trivially necessary condition as
a finding is the most common beginner mistake in QCA.

**Finding no necessary condition is a result, not a failure.** It says there is
more than one way to reach the outcome — which is precisely what the sufficiency
analysis will then map out.

### Does necessity hold in every period and every case?

```r
diag <- necessity_panel_diagnostics(data_cal, outcome, conditions,
                                    id_var = "case_id", time_var = "period")
diag$between   # BECONS_N: necessity computed within each period
diag$within    # WICONS_N: necessity computed within each case
```

A condition whose `BECONS_N` is 0.95 in one period and 0.60 in another is not
"necessary" in any useful sense — it is necessary *sometimes*, and that is what
you should report.

---

## 4. Sufficiency: what is enough

```r
suf <- sufficiency_analysis(data_cal, outcome, conditions, params)
suf$terms
#> "infrastructure*knowledge"  "finance*talent"
```

Read this as: **(infrastructure AND knowledge) OR (finance AND talent)** is
sufficient for the outcome. `*` is AND, `~` is absence, and each term is an
alternative path.

Two steps happen under the hood. First a **truth table** is built: one row per
logically possible combination of conditions (4 conditions → 16 rows), each
scored for how consistently the cases in it show the outcome. Then rows that
clear the consistency cutoff are **minimized** with Boolean algebra: if
`A*B*C` and `A*B*~C` both lead to the outcome, then C is irrelevant there and
both collapse into `A*B`.

### Quality of each path

```r
unique_coverage(data_cal, outcome, suf$terms)
```

| Column | Meaning | Rule of thumb |
|---|---|---|
| `cons` | Consistency: how reliably this path produces the outcome | ≥ 0.75 minimum, ≥ 0.80 comfortable |
| `raw_cov` | Share of the outcome this path accounts for | — |
| `unique_cov` | Share **only** this path accounts for | near zero = redundant with other paths |

A path with high raw coverage but near-zero unique coverage is not really a
separate finding: the cases it covers are already covered elsewhere.

### Parsimonious vs. intermediate solutions

The problem: with 4 conditions there are 16 possible configurations, and your
data will not contain cases for all of them. Those empty configurations are
**logical remainders**, and what you assume about them changes the answer.

- **Parsimonious solution** (the default here): uses whatever remainder
  assumptions produce the simplest expression. Simplest, but some of those
  assumptions may be implausible.
- **Intermediate solution**: only uses remainders consistent with your stated
  theory. Requires you to declare directional expectations.

```r
params <- pfsqca_params(
  incl_cut = 0.80,
  dir_exp = c(infrastructure = 1, knowledge = 1, finance = 1, talent = 1)
)
# 1 = you expect presence to contribute, 0 = absence, -1 = no expectation
```

With expectations set, you also get **core** conditions (present in both the
parsimonious and the intermediate solution — the strongest evidence) and
**contributing** conditions (only in the intermediate one). `create_ball_table()`
formats that in the circle notation journals expect.

---

## 5. Panel metrics: the part that is specific to this package

A solution can fit the pooled data well and still be an artefact — driven by one
period, or by a handful of cases. These three metrics take it apart.

```r
pm <- panel_metrics(data_cal, outcome, suf$terms,
                    id_var = "case_id", time_var = "period")
```

**POCONS / POCOV** (`pm$pooled`) — consistency and coverage over all
observations at once. This is what standard fsQCA reports.

**BECONS / BECOV** (`pm$between`) — the same metrics computed *within each
period*. Compare them across periods: a solution with BECONS of 0.95, 0.93 and
0.91 is temporally stable. One with 0.95, 0.94 and 0.62 is not — something
changed, and that is worth investigating rather than averaging away.

**WICONS / WICOV** (`pm$within`) — computed *within each case*, across its own
periods. Report the mean and the share of cases above 0.70. Then sort ascending
and look at the bottom: those are the cases your solution does not explain, and
they are the natural candidates for qualitative follow-up.

The value of these is diagnostic. A pooled POCONS of 0.90 built on periods
scoring 0.95 / 0.94 / 0.81 is a different empirical claim from one built on
0.90 / 0.90 / 0.90, and only the panel decomposition tells them apart.

---

## 6. Robustness: does it survive dropping cases?

QCA works with relatively few cases, so a single unusual case can create or
destroy a path. The random deletion test (Emmenegger, Schraff & Walter) makes
that visible: delete a random 10% of observations, re-run the entire analysis,
repeat several hundred times, and count how often each finding reappears.

```r
params <- pfsqca_params(robustness_B = 999, robustness_drop = 0.10)
rob <- sufficiency_robustness(data_cal, outcome, conditions, params)
rob$freq_tbl
```

| Column | Meaning |
|---|---|
| `term` | The path |
| `n`, `freq` | How many / what share of iterations produced it |
| `in_baseline` | Whether it is in the solution from the full data |

**Interpretation.** `freq ≥ 0.80` is the conventional bar for calling a path
robust. A baseline path with `freq = 0.35` depends on which cases happen to be
in your sample — report it with that caveat, or not as a finding at all. A path
with high `freq` that is *not* in the baseline means your thresholds are sitting
on a knife edge; try nudging `incl_cut` and see how much moves.

`robustness_B = 999` with 999 re-runs of the minimization is the slow part of
the analysis — minutes, not seconds. Use 100 while you are still setting up, and
999 for the results you will publish. `robustness_seed` keeps it reproducible:
same seed, same numbers, every time.

---

## 7. Choosing thresholds

```r
params <- pfsqca_params(
  incl_cut      = 0.80,   # consistency cutoff for the truth table
  pri_cut       = 0.50,   # PRI cutoff
  n_cut         = 1,      # minimum cases per configuration
  nec_threshold = 0.90    # necessity cutoff
)
```

**`incl_cut`** — the most consequential choice. Below 0.75 you are calling paths
sufficient when they frequently are not. Above 0.85 you may find nothing. Start
at 0.80. Then look at the truth table (`suf$tt`) for a natural gap in the
consistency column and set the cutoff there; a gap is a better justification
than a round number.

**`n_cut`** — how many cases a configuration needs before it counts as observed
rather than a remainder. With small-N panel data, 1 is normal. With large-N data
raise it, so that single-case configurations do not drive the solution.

**`pri_cut`** — PRI guards against a configuration being consistent for *both*
the outcome and its negation. 0.50 is the standard floor; a row with high
consistency but PRI below 0.50 should not be treated as sufficient.

**`nec_threshold`** — 0.90 is the convention. Lowering it to 0.80 to make
something come out "necessary" is not a defensible move.

Whatever you choose, **report it**, and report what happens when you change it.
Threshold sensitivity is a legitimate finding.

---

## 8. When something goes wrong

**"Variable not found in the data"** — a column name is misspelled. R is
case-sensitive: `Period` and `period` are different. The error message lists the
column names you actually have; copy from there.

**"These variables are not fuzzy-set scores in [0, 1]"** — you passed raw data
to a function that expects calibrated data. Run `calibrate_panel()` first, or
set `CALIBRATE <- TRUE` in the script.

**"No truth table row reached the consistency cutoff"** — nothing in your data
is sufficient at that threshold. Lower `incl_cut`, lower `n_cut`, or use fewer
conditions.

**"Calibration returned only NA"** — that variable has (almost) no variation, so
the percentile anchors collapse into one value. Drop the condition.

**The solution has one long path with every condition in it** — usually too many
conditions for too few cases. With 6 conditions there are 64 possible
configurations; if you have 90 observations, most configurations are empty and
minimization has nothing to work with. Four or five conditions is the practical
ceiling for most panels.

**Nothing is robust** — either your thresholds are borderline, or the finding
genuinely rests on a few cases. Both are worth reporting honestly. A QCA paper
that reports which of its paths failed the robustness test is more credible than
one that does not mention robustness at all.

---

## 9. Reporting checklist

- The calibration anchors, and why (percentiles or substantive thresholds).
- `incl_cut`, `pri_cut`, `n_cut`, and the reasoning behind them.
- The full necessity table including `covN` — not only what passed.
- The truth table, or at least the rows above the cutoff.
- Every sufficient path with consistency, raw coverage and unique coverage.
- Solution-level POCONS and POCOV.
- Between-period and within-case spread, not just the pooled figure.
- Which solution you report (parsimonious or intermediate) and, if
  intermediate, your directional expectations.
- The robustness procedure and which findings survived it.

`export_pfsqca()` writes all of these to CSV, and `00_parameters.txt` records
the exact settings used, so the analysis can be reproduced.

---

## Further reading

- Ragin, C. C. (2008). *Redesigning Social Inquiry: Fuzzy Sets and Beyond*. University of Chicago Press.
- Schneider, C. Q., & Wagemann, C. (2012). *Set-Theoretic Methods for the Social Sciences*. Cambridge University Press.
- Emmenegger, P., Schraff, D., & Walter, A. (2014). QCA, the Truth Table Analysis and Large-N Survey Data. *COMPASSS Working Paper* 2014-79.
- Dusa, A. (2019). *QCA with R: A Comprehensive Resource*. Springer.
- [compasss.org](https://compasss.org) — the QCA research community, with bibliography and software list.
