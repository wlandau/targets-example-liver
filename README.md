This pipeline demonstrates how to use `targets` to simulate a clinical trial.
The goal of the illustrative simulation is to determine when the trial should conduct an interim analysis to assess futility.

## Illustrative clinical trial

Suppose we are designing a randomized controlled double-blind phase 2 clinical trial to test a novel therapy in patients with a hepatic autoimmune disease.
The trial will enroll up to 100 patients, and because the disease is extremely rare, the accrual period is expected to last 10 years.
The therapy showed promise in preclinical and phase 1 studies, but patients undergoing treatment may be more susceptible to infections.
If the treatment does not meaningfully improve survival, then the trial should discontinue enrollment.

## Interim analysis

An interim analysis will assess futility when a certain number of hepatic adverse events have occurred.
This event threshold is crucial.
If it is too low, then there is not enough data to determine if the therapy improves survival.
If too high, then the trial may expose patients to unnecessary risk.
The goal of the simulation is to empirically determine when the interim should occur.
With appropriate timing, the interim will have power to detect futility while minimizing patient exposures.

## Joint model

The primary endpoint is the number of years until a hepatic adverse event occurs (death or liver transplant).
The futility analysis uses a Bayesian joint model to account for the association between survival and bilirubin levels.
The longitudinal submodel is defined as follows:

$$
\begin{aligned}
E(y_i(t)) &:= \eta_i(t) \\
\eta_i(t) &= x_i(t) \cdot \beta + b_i
\end{aligned}
$$

* $y_i(t)$ is the log bilirubin measurement of patient $i$ at time $t$ (in years).
* $x_i(t)$ is a matrix of covariates (treatment arm, time-varying albumin measurement, and visit time in years).
* $\beta$ is a vector of fixed effects parameters.
* $b_i$ is the random intercept of patient $i$.

And the event submodel uses so-called "current value association" to relate log bilirubin measurements to survival outcomes:

$$
\begin{aligned}
h_i(t) = h_0(t, \lambda) \cdot \exp \left (\eta_i(t) \cdot \alpha + \gamma + w_i \cdot \theta \right )
\end{aligned}
$$

* $h_i(t)$ is the hazard rate of a hepatic event for patient $i$ at time $t$ (in years).
* $h_0(t, \lambda)$ is the baseline hazard function (modeled with B-splines).
* $\alpha$ is the association parameter that links the longitudinal and event submodels. It is the log fold change in the hazard ratio of hepatic events per unit increase in log bilirubin.
* $w_i$ equals 1 of patient $i$ is in the active treatment arm (0 otherwise).
* $\gamma$ is an intercept term.
* $\theta$ is the log hazard ratio of treatment vs control.

## Decision rule

The treatment is considered effective if the hazard ratio is less than 0.75.

$$
\begin{aligned}
\exp(\theta) < 0.75
\end{aligned}
$$

The interim declares futility if the posterior probability of efficacy is less than 40%.

$$
\begin{aligned}
P \left ( \exp(\theta) < 0.75 \ \mid \ \text{data} \right ) < 0.4
\end{aligned}
$$

## Simulation

The pipeline simulates thousands of independent replications of the trial.
For each replication, we analyze the dataset with the joint model, and we
determine if the estimated posterior probability of efficacy is less thatn 40%.
The estimated probability of futility is fraction of simulated trials that declare futility.
To help determine when the interim should happen, we simulate different interim timing scenarios and compare futility probability and average enrollment of each.
To make the simulation realistic, the simulated datasets use hazard ratios drawn from the posterior distribution of a joint model fitted to historical data.

## Historical data



## Simulation

## Implementation
