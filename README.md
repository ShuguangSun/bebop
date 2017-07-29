# BEBOP

## Introduction
BEBOP stands for Bivariate Evaluation of Bivariate Binary Outcomes and Predictive Variables. It is a design for phase II clinical trials in stratified medicine with efficacy and toxicity outcomes where predictive variables are available. It was inspired by the PePS2 trial, investigating pembrolizumab in lung cancer patients. BEBOP uses the same probability model for co-primary binary variables as _EffTox_, by Thall & Cook (2004).

This git repo contains a research article titled "A Design for Phase II Clinical Trials in Stratified Medicine with Efficacy and Toxicity Outcomes and Predictive Variables".

It also contains full R-code to reproduce all of the examples and statistics in the manuscript.



## Prerequisites

To run the enclosed simulations, you will need to install my [_trialr_](https://github.com/brockk/trialr) R-package. _trialr_ itself depends on rstan, the R-implementation of the powerful Bayesian statistical environment, Stan. To install rstan, a C++ compiler must be installed on your system: 

- On Windows, this can be satisfied by installing [Rtools](https://cran.r-project.org/bin/windows/Rtools/). 
- On Mac, you may need to install command line Xcode tools; I think Apple stopped shipping it with Xcode by default. You may (will?) need to reinstall command line tools when you upgrade macOS version too because I think the update blitzes them. 
- On Linux, you are probably good to go (but if you chose Linux, you already knew that).

Once you have a C++ installer set-up, you should be able to install all the prerequisites by proceeding in R:

```
install.packages('rstan')
install.packages('devtools')
devtools::install_github('brockk/trialr')
# or maybe even
devtools::install_github('https://github.com/brockk/trialr')
```



## Layout of this package

The package is provided as an RStudio project. Handily, that sets the working directory.

Scripts to run the simulations are in directories like:

- `6params/paper_priors/`
- `6params/uninformative_priors/`

etc.

Simulation results are saved as JSON objects in the applicable `/sims` directory.

Other noteworthy scripts:

- `priors.R` presents the priors used;
- `cohort_sizes.R` simulates the distribution of cohort sizes using prevalences justified in the paper and supplement;
- `to_latex.R` summarises the simulation results and presents in LaTeX format. I used this script to get results into the paper;
- `batch.R` runs all examples. It would take hours, if not days, to run serially. I used different machines.

Clone this repo using:

```
cd /path/to/your/dev/place
git clone https://github.com/brockk/bebop
```



I hope you appreciate all this effort to conduct and disseminate reproducible research!

Kristian Brock, 2017