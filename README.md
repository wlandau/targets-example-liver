

This pipeline demonstrates how to use `targets` to simulate a clinical
trial. The goal of the illustrative simulation is to determine when the
trial should conduct an interim analysis to assess futility.

## Illustrative clinical trial

Suppose we are designing a randomized controlled double-blind phase 2
clinical trial to test a novel therapy in patients with an autoimmune
disease of the liver. The trial will enroll up to 100 patients, and
because the disease is extremely rare, the accrual period is expected to
last 10 years. If the treatment does not meaningfully improve survival,
then the trial should discontinue enrollment.

## Interim analysis

An interim analysis will assess futility when a certain number of
hospitalizations have occurred. This event threshold is crucial. If it
is too low, then there is not enough data to determine if the therapy
improves survival. If too high, then the trial may expose patients to
unnecessary risk. The goal of the simulation is to empirically determine
when the interim should occur. With appropriate timing, the interim will
have power to detect futility while minimizing patient exposures.

## Joint model

The primary endpoint is the number of years until hospitalization. The
futility analysis uses a Bayesian joint model to account for the
association between survival and bilirubin levels (Lawrence et al.
(2016), Brilleman et al. (2018), Goodrich et al. (2024)). The
longitudinal submodel is defined as follows:

$$
\begin{aligned}
\eta_i(t) = x_i(t) \cdot \beta + b_i
\end{aligned}
$$

- $\eta_i(t)$ is the expected value $E(y_i(t))$.
- $y_i(t)$ is the log bilirubin measurement of patient $i$ at time $t$
  (in years).
- $x_i(t)$ is a matrix of covariates (treatment arm, time-varying
  albumin measurement, and visit time in years).
- $\beta$ is a vector of fixed effects parameters.
- $b_i$ is the random intercept of patient $i$.

And the event submodel uses so-called “current value association” to
relate log bilirubin measurements to survival outcomes:

$$
\begin{aligned}
h_i(t) = h_0(t, \lambda) \cdot \exp \left (\eta_i(t) \cdot \alpha + \gamma + w_i \cdot \theta \right )
\end{aligned}
$$

- $h_i(t)$ is the hazard rate of a hepatic event for patient $i$ at time
  $t$ (in years).
- $h_0(t, \lambda)$ is the baseline hazard function (modeled with
  B-splines).
- $\alpha$ is the association parameter that links the longitudinal and
  event submodels. It is the log fold change in the hazard ratio of
  hepatic events per unit increase in log bilirubin.
- $w_i$ equals 1 of patient $i$ is in the active treatment arm (0
  otherwise).
- $\gamma$ is an intercept term.
- $\theta$ is the log hazard ratio of treatment vs control.

## Decision rule

The treatment is considered effective if the hazard ratio is less than
0.75.

$$
\begin{aligned}
\exp(\theta) < 0.75
\end{aligned}
$$

The interim declares futility if the posterior probability of efficacy
from the joint model is less than 40%.

$$
\begin{aligned}
P \left ( \exp(\theta) < 0.75 \ \mid \ \text{data} \right ) < 0.4
\end{aligned}
$$

## Simulation

The pipeline simulates thousands of independent replications of the
trial. For each replication, we analyze the dataset with the joint
model, and we determine if trial declares futility. The estimated
probability of futility is fraction of simulated trials that declare
futility. To help determine when the interim should happen, we simulate
different interim timing scenarios and compare futility probabilities
and average enrollments across scenarios.

## Historical data

To help make this exercise realistic, the simulated datasets use hazard
ratios drawn from the posterior distribution of a joint model fitted to
historical data. The historical data consists of datasets `pbc` and
`pbcseq` from the `survival` R package (Therneau and Grambsch (2000)).
The underlying clinical trial compared the efficacy of D-penicillamine
vs placebo in patients with primary biliary cholangitis (Dickson et al.
(1985)). A joint model fit to this data returns a posterior mean hazard
ratio near 1. By drawing hazard ratios from the posterior distribution
of this fitted model, the simulations assume an overall lack of efficacy
and a historically-motivated level of uncertainty around the hazard
ratio.

## Implementation

This simulation exercise is a `targets` pipeline (Landau (2021)). The
pipeline uses the SGE `crew.cluster` backend to run simulations in
parallel. Configure the `crew` backend in `_targets.R` to fit your
computational resources and then call `tar_make()` to run the pipeline.
The Quarto report in `results.html` summarizes the results.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-Brilleman2018" class="csl-entry">

Brilleman, S. L., M. J. Crowther, M. Moreno-Betancur, J. Buros Novik,
and R. Wolfe. 2018. “Joint Longitudinal and Time-to-Event Models via
Stan.” In *StanCon 2018*. <https://github.com/stan-dev/stancon_talks/>.

</div>

<div id="ref-Dickson1985" class="csl-entry">

Dickson, E. R., T. R. Fleming, R. H. Wiesner, W. P. Baldus, C. R.
Fleming, J. Ludwig, and J. T. McCall. 1985. “Trial of Penicillamine in
Advanced Primary Biliary Cirrhosis.” *New England Journal of Medicine*
312 (16): 1011–15. <https://doi.org/10.1056/NEJM198504183121602>.

</div>

<div id="ref-Goodrich2024" class="csl-entry">

Goodrich, B., J. Gabry, I. Ali, and S. L. Brilleman. 2024.
“<span class="nocase">rstanarm: Bayesian applied regression modeling via
Stan</span>.” <https://mc-stan.org/rstanarm>.

</div>

<div id="ref-Landau2021" class="csl-entry">

Landau, W. M. 2021. “<span class="nocase">The targets R package: a
dynamic Make-like function-oriented pipeline toolkit for reproducibility
and high-performance computing</span>.” *Journal of Open Source
Software* 6 (57): 2959. <https://doi.org/10.21105/joss.02959>.

</div>

<div id="ref-Lawrence2016" class="csl-entry">

Lawrence, G. A., M. E. Boye, M. J. Crowther, J. G. Ibrahim, G. Quartey,
S. Micallef, and F. Y. Bois. 2016. “<span class="nocase">Joint modeling
of survival and longitudinal non-survival data: current methods and
issues. Report of the DIA Bayesian joint modeling working group</span>.”
*Statistics in Medicine* 34 (14): 2181–95.
<https://doi.org/10.1002/sim.6141>.

</div>

<div id="ref-Therneau2000" class="csl-entry">

Therneau, T. M., and P. M. Grambsch. 2000. *Modeling Survival Data:
Extending the Cox Model*. New York: Springer.

</div>

</div>
