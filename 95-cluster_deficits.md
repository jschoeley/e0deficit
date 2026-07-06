# Formalized Clustering of Life Expectancy Deficit Trajectories

## Goal

The existing visual grouping in `cfg/config.yaml` was formalized using the total-sex
yearly life expectancy deficit trajectory for 2020-2024. Deficits are actual minus
expected life expectancy, so more negative values indicate larger deficits.

The target was at least 90% concordance with the visual cluster assignments.

## Data

The implemented script is `src/95-cluster_deficits.R`.

Primary input is `tmp/50-deficits_and_excesses_sim.qs`, specifically:

```r
x["0", 2020:2024, "Total", region, 2:251, "ex_actual_minus_expected"]
```

`sim_id == 1` is the mean slot, so stochastic draws start at `sim_id == 2`.
For each region-year, the script uses the median over simulation draws.

In this container, the `qs` package is not installed, so the script used the
deterministic summary fallback `out/61-e0deficitbyyear_total.csv`, taking
`e0deficit_Q50`. The clustering rule is the same in both cases.

## Rule

For each country, compute:

- `peak_year`: the year with the most negative median e0 deficit.
- `peak_prominence`: the difference between the worst and second-worst median deficits.
- `trajectory_range`: the maximum minus minimum median deficit across 2020-2024.

A country is assigned to `D Prolonged depression` if:

```r
peak_prominence < 0.22 && trajectory_range < 0.75
```

Otherwise it is assigned by the timing of the peak deficit:

- peak in 2020: `A First wave peak`
- peak in 2021: `B Second wave peak`
- peak in 2022 or later: `C Late peak`

There is one tie-breaker: if 2021 is the formal peak but 2020 is within 0.07
years of life expectancy of 2021, the country is assigned to
`A First wave peak`. This handles near-tied first-wave/second-wave cases where
the visual classification treated the first wave as the defining feature.

## Result

The script writes `out/95-cluster_deficits.csv`.

Concordance with the visual clusters is:

```text
94.1% (32/34 countries)
```

Discordant assignments:

| Region | Formal group | Visual group | Peak year | Peak prominence | Range |
|---|---|---|---:|---:|---:|
| CH | D Prolonged depression | A First wave peak | 2020 | 0.116 | 0.448 |
| DK | D Prolonged depression | C Late peak | 2022 | 0.157 | 0.684 |

Both discordant cases are borderline under the D rule: the formal peak is
not separated by much from the rest of the trajectory and the total trajectory
range is below 0.75 years.

## Interpretation

The method intentionally keeps the formalization simple. It reduces the visual
decision to one D-versus-non-D rule based on the absence of an isolated peak.
Once a country is not D, the cluster follows directly from the timing of the
largest deficit. This reproduces the visual grouping above the requested
90% concordance threshold.
