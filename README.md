# Phase II Thall, Nguyen & Estey clinical trial design for co-primary efficacy and toxicity outcomes adjusted for covariates

## Introduction
P2TNE, for _Phase 2 Thall, Nguyen and Estey_, is a clinical trial design for 
assessing efficacy and toxicity outcomes where predictive baseline coavariates 
are available.
It is a simplification of Thall, Nguyen and Estey's design for dose-finding.
The core probability model for co-primary binary variables mimics that in _EffTox_, by Thall & Cook (2004).

Development of the design was inspired by the PePS2 trial, investigating 
pembrolizumab in performance status 2 lung cancer patients. 

This repo contains research article(s) written on the topic.
It also contains full R-code to reproduce examples and statistics in the 
manuscript(s).


## Prerequisites

Models are implemented in Stan.
To run the enclosed simulations, you will need to install the R-package [_rstan_](http://mc-stan.org/users/interfaces/rstan).


## Model names
I investigate P2TNE designs that deploy parameters in the:

1) efficacy model
2) toxicity model
3) and associative model

To distinguish these different models, I refer to them by the number of params 
used in those aspects of the design, respectively. For instance, the _411-params_
model uses 4 parameters in the efficacy model, 1 in the toxicity model, and 1
in the associative model. 
When 1 paramater is used in the toxicity model, it is the intercept; i.e. Pr(Tox) 
is assumed to be homogeneous across cohorts.
When 4 params are used in efficacy or toxicity models, they are for intercept,
pre-treatedness, low PD-L1, and medium PD-L1 respectively.
When 6 are used, the fifth and sixth parameters handle interaction between 
pre-treatedness, and low and medium PD-L1, respectively.
The associative model uses a single parameter to correlate the two events, or it
uses none (i.e. independence is assumed).


## Layout of this package

Clone this repo using:

```
cd /path/to/your/dev/place
git clone https://github.com/brockk/bebop
```


The package is provided as an RStudio project. Handily, that sets the working 
directory.

Scripts to run the simulations are in subdirectories according to model and 
priors used, like:

- `411-params/scept_priors/`
- `411-params/uninf_priors/`
- `411-params/inf_priors/`

etc.

Simulation results are saved as JSON objects in the applicable `/sims` directory.

Other noteworthy scripts:

- `Results.R` presents the simulated results;
- `CohortSizes.R` simulates the distribution of cohort sizes using prevalences justified in the paper and supplement;
- `CommonObjects.R` provides functions and data used in many places.
- `Batch-Model-xyz.R` runs scenarios for a given model. 
It would take many days to run serially.
To avoid that, the simulation results are stored in the repo as JSON objects.


## Acknowledgements
Thanks to the peer reviewers who have invested the time to point out valuable improvements.
Thanks to everyone at Stan for revolutionising Bayesian computation, building on
the great work of everyone involved in the BUGS projects.
Thanks also to GitHub for making it easy to conduct and disseminate reproducible research.


## What is BEBOP?
The repo is called BEBOP - what is that?
I developed this trial design as an extension of EffTox and named the design 
BEBOP, for Bivariate Evaluation of Bivariate Binary Outcomes and Predictive Variables. 
A journal reviewer pointed out that the design was actually a special case of
Thall, Nguyen and Estey, published some 10 years previosuly.
Forlornly, I abandoned the name BEBOP and referred to it instead as P2TNE for 
_Phase 2 Thall, Nguyen and Estey_.
This repo still bears the name BEBOP for posterity (and because changing repo 
names seems dangerous to me).


Kristian Brock, 2018



## References
Thall, P. F., Nguyen, H. Q., & Estey, E. H. (2008). Patient-specific dose finding based on bivariate outcomes and covariates. Biometrics, 64(4), 1126–1136. https://doi.org/10.1111/j.1541-0420.2008.01009.x

Thall, P., & Cook, J. (2004). Dose-Finding Based on Efficacy-Toxicity Trade-Offs. Biometrics, 60(3), 684–693.



