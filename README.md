## Replication code for paper "Linking Absolute and Relative Mobility: A Tractable Framework, a Decomposition Method, and an Application to Black–White Mobility Differences"

This repository contains all code necessary to reproduce the estimations, tables, and figures presented in the project. The workflow is fully scripted. Executing the master file runs the complete pipeline and reproduces all results without manual intervention.

1. SOFTWARE REQUIREMENTS

Required:

* R (version 4.2 or newer recommended)
* CmdStan properly installed and configured
* A working C++ toolchain (required for rstan / cmdstanr)

If `pacman` is not installed:

install.packages("pacman")

To install CmdStan (required only once):

cmdstanr::install_cmdstan()


2. REQUIRED FOLDER STRUCTURE


All folders must be located in the project root directory. Before running the code, ensure the following structure exists:

project_root/
│
├── code/
├── data/
├── figures/
├── tables/

Important:

* All input datasets must be stored inside the `data/` folder.
* The user must create empty folders named `figures/` and `tables/` in the root directory before running the scripts.
* The `code/` folder must contain the following core scripts:

  * 0_masterfile.R
  * 1_montecarlo.R
  * 2_empiricalcase.R
  * 3_plotcurves.R


3. SETUP


Inside `0_masterfile.R`, define the root directory path:

folder <- "YOUR/PATH/TO/PROJECT"

This path must point to the `project_root` directory containing `code/`, `data/`, `figures/`, and `tables/`.

All paths are handled using the `here()` package to ensure portability across systems.


4. EXECUTION


Open the `code/` folder and run `0_masterfile.R`. This master script executes the full workflow sequentially:

A) MONTE CARLO EXPERIMENT

source(here("code","1_montecarlo.R"))

This script:

* Runs the data generating process (DGP)
* Performs Bayesian estimation using rstan / cmdstanr
* Stores simulation outputs

B) EMPIRICAL APPLICATION

source(here("code","2_empiricalcase.R"))

This script:

* Loads empirical data from `data/`
* Estimates the structural model
* Produces tables saved in `tables/`

C) PLOT CURVES

source(here("code","3_plotcurves.R"))

This script:

* Generates the article's figure 2 panel A and figure 4, panels A and B.
* Saves outputs in `figures/`
* Applies the custom graphical theme

No additional steps are required if the directory structure and dependencies are correctly configured.


5. MCMC CONFIGURATION


Default configuration:

n_chains = 4
n_iter   = 2000
target_ratio = 0.5
n_warmup = max(75, floor(n_iter * target_ratio))

These parameters can be adjusted depending on computational capacity and convergence diagnostics.


6. COMPUTATIONAL CONSIDERATIONS


* Bayesian estimation may be computationally intensive.
* Initial Stan model compilation may take several minutes.
* Parallel chains require sufficient RAM.

Recommended:

* At least 16GB RAM
* Multi-core processor

If the directory structure is correctly defined and all dependencies are installed, the replication should run without further modification.
