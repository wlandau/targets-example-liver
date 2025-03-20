This pipeline demonstrates how to use `targets` to simulate a clinical trial.
The goal of the illustrative simulation is to determine when the trial should conduct an interim analysis to assess futility.

## Illustrative clinical trial

Suppose we are designing a randomized controlled double-blind phase 2 clinical trial to test a novel therapy in patients with an extremely rare hepatic autoimmune disease.
The therapy showed promise in preclinical and phase 1 studies, but patients may be more susceptible to infections while undergoing treatment.
If the treatment does not meaningfully improve survival, then the trial should discontinue enrollment.

## Interim analysis

An interim analysis will assess futility when a certain number of hepatic adverse events have occurred.
This event threshold is crucial.
If it is too low, then there is not enough data to determine if the therapy improves survival.
If too high, then the trial may expose patients to unnecessary risk.
The goal of the simulation is to empirically determine when the interim should occur.
With appropriate timing, the interim will have power to detect futility while minimizing patient exposures.

## Futility analysis

The primary endpoint is the number of years until a hepatic adverse event occurs (death or liver transplant).
The futility analysis uses a Bayesian joint model

## Historical data

## Simulation

## Implementation
