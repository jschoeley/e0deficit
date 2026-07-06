# Formalized Clustering of Life Expectancy Deficit Trajectories

## Goal

The visual grouping in `cfg/config.yaml` was formalized without using the prior
cluster assignments in the classifier. The implemented method uses simulation
draws of total-sex life expectancy deficits for 2020-2024, derives
country-specific evidence for peak/range/trend structure, and clusters countries
unsupervised.

Deficits are actual minus expected life expectancy, so more negative values
indicate larger deficits.

## Implementation

The implementation is in `src/95-cluster_deficits.R`. It defines:

```r
AssignE0DeficitClusters(deficit_sim_qs, method = ...)
```

The function takes the path to `50-deficits_and_excesses_sim.qs` and returns one
row per country with the assigned `formal_group` plus diagnostic features.

When `src/95-cluster_deficits.R` is run as a script, it writes the default
`method = "kmeans_pvalues"` output to `out/95-cluster_deficits.csv` and exports
a scatterplot matrix of the k-means features to
`out/95-kmeans_scatterplot_matrix.pdf`. It also exports a country-by-method
assignment comparison to `out/95-cluster_method_comparison.pdf`.

Input draws are read from `tmp/50-deficits_and_excesses_sim.qs`:

```r
x["0", 2020:2024, "Total", region, 2:251, "ex_actual_minus_expected"]
```

`sim_id == 1` is the mean slot; stochastic draws start at `sim_id == 2`.

## Simulation-Based Features

For each country, the script computes the median 2020-2024 deficit trajectory
from the simulation draws. It then constructs a country-specific no-shape null
distribution by centering each year of the draw matrix to a common country mean.
This preserves simulation uncertainty and covariance while removing the observed
trajectory shape.

Three trajectory statistics are evaluated against that null:

- `peak_prominence`: gap between the worst and second-worst year.
- `trajectory_range`: maximum minus minimum deficit.
- `linear_slope`: linear trend over 2020-2024.

The resulting posterior-predictive p-values are:

- `p_peak`
- `p_range`
- `p_trend`

Large values mean weak evidence against a no-shape/prolonged-depression
trajectory. Small values mean the simulations support a structured peak, range,
or trend.

## Available Methods

`AssignE0DeficitClusters()` supports three methods:

1. `method = "fixed_thresholds"`

   Assigns D when both the peak prominence and trajectory range are below fixed
   cutoffs. Defaults reproduce the first formalization:

   ```r
   peak_prominence < 0.22 && trajectory_range < 0.75
   ```

2. `method = "kmeans_pvalues"`

   Clusters countries with k-means on the standardized vector:

   ```r
   c(p_peak, p_range, p_trend)
   ```

   The number of clusters defaults to four because the target typology has four
   classes, but the visual labels are not used in fitting. The D cluster is
   identified after fitting as the cluster with the largest average
   `mean(p_peak, p_range, p_trend)`, i.e. the cluster with weakest
   simulation-based evidence for a structured trajectory.

3. `method = "alpha_rule"`

   Assigns D when both peak and trend features are not significant:

   ```r
   p_trend > alpha && p_peak > alpha
   ```

## A/B/C Classification

For countries not assigned to D, the cluster follows the timing of the median
peak deficit:

- peak in 2020: `A First wave peak`
- peak in 2021: `B Second wave peak`
- peak in 2022 or later: `C Late peak`

Near-tied first-wave cases are handled with posterior peak probabilities:
if the median peak is 2021 but 2020 has posterior support as the only relevant
competing peak year, the country is assigned to `A First wave peak`. This uses
the simulation draws and avoids a numeric tie threshold.

## Result

Current default `method = "kmeans_pvalues"` concordance with the visual clusters:

```text
91.2% (31/34 countries)
```

Discordant assignments:

| Region | Formal group | Visual group | Peak year | D evidence score |
|---|---|---|---:|---:|
| LU | D Prolonged depression | A First wave peak | 2020 | 0.623 |
| IL | C Late peak | D Prolonged depression | 2024 | 0.033 |
| GB-NIR | C Late peak | D Prolonged depression | 2024 | 0.449 |

Interpretation of mismatches:

- Luxembourg has a 2020 median peak, but the simulation-based p-values place it
  in the weak-shape cluster.
- Israel has strong simulation evidence for a late 2024 peak.
- Northern Ireland is borderline: trend evidence is weak, but peak/range
  evidence pulls it outside the D cluster.

The method exceeds the requested 90% concordance while avoiding use of prior
cluster assignments in the classifier.
