# Linking Absolute and Relative Mobility

> Replication code for "Linking Absolute and Relative Mobility: A Tractable Framework, a Decomposition Method, and an Application to Black–White Mobility Differences"

This repository provides complete replication materials for the paper, including all data processing, Bayesian estimation, and visualization code. The workflow is fully automated—running the master script reproduces all tables and figures without manual intervention.

---

## 📋 Table of Contents

- [Features](#features)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Project Structure](#project-structure)
- [Quick Start](#quick-start)
- [Workflow](#workflow)
- [Configuration](#configuration)
- [Computational Requirements](#computational-requirements)
- [Troubleshooting](#troubleshooting)
- [Citation](#citation)
- [License](#license)

---

## ✨ Features

- **Fully automated pipeline**: One-click replication of all results
- **Bayesian estimation**: Stan-based MCMC for structural model estimation
- **Monte Carlo experiments**: Comprehensive simulation studies
- **Publication-ready outputs**: High-quality figures and formatted tables
- **Portable code**: Cross-platform compatibility using `here()` package

---

## 📦 Prerequisites

### Required Software

| Software | Minimum Version | Purpose |
|----------|----------------|---------|
| **R** | 4.2+ | Statistical computing |
| **CmdStan** | Latest | Bayesian inference engine |
| **C++ toolchain** | - | Required for Stan compilation |

### R Package Installation

All required packages will be automatically installed via `pacman`. If you don't have `pacman`, install it first:

```r
install.packages("pacman")
```

### Stan Installation

CmdStan must be installed before running the code (required only once):

```r
# Install cmdstanr if not already installed
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# Install CmdStan
cmdstanr::install_cmdstan()
```

For detailed installation instructions, see the [CmdStanR documentation](https://mc-stan.org/cmdstanr/).

---

## 📁 Project Structure

Ensure your project directory follows this structure:

```
project_root/
│
├── code/
│   ├── 0_masterfile.R          # Master execution script
│   ├── 1_montecarlo.R          # Monte Carlo simulations
│   ├── 2_empiricalcase.R       # Empirical estimation
│   └── 3_plotcurves.R          # Figure generation
│
├── data/                        # Input datasets (required)
│
├── figures/                     # Output figures (create empty)
│
└── tables/                      # Output tables (create empty)
```

### Setup Instructions

1. **Create output directories** (if they don't exist):
   ```bash
   mkdir figures tables
   ```

2. **Place input data** in the `data/` folder

3. **Verify** that all scripts are in the `code/` folder

---

## 🚀 Quick Start

### 1. Configure Project Path

Open `code/0_masterfile.R` and set the root directory:

```r
folder <- "YOUR/PATH/TO/PROJECT"
```

> **Note**: This path should point to `project_root/`, the directory containing `code/`, `data/`, `figures/`, and `tables/`.

### 2. Run the Master File

```r
source("code/0_masterfile.R")
```

That's it! The script will execute the entire workflow and generate all outputs.

---

## 🔄 Workflow

The master file orchestrates three main stages:

### Stage 1: Monte Carlo Experiment
```r
source(here("code", "1_montecarlo.R"))
```

- Implements the data generating process (DGP)
- Performs Bayesian estimation using Stan
- Validates model recovery properties
- Stores simulation results

### Stage 2: Empirical Application
```r
source(here("code", "2_empiricalcase.R"))
```

- Loads empirical datasets from `data/`
- Estimates structural parameters
- Generates tables for the paper
- Saves outputs to `tables/`

### Stage 3: Visualization
```r
source(here("code", "3_plotcurves.R"))
```

- Creates publication-quality figures:
  - Figure 2, Panel A
  - Figure 4, Panels A and B
- Applies custom graphical theme
- Saves outputs to `figures/`

---

## ⚙️ Configuration

### MCMC Parameters

Default settings in the estimation scripts:

```r
n_chains     = 4      # Number of MCMC chains
n_iter       = 2000   # Iterations per chain
target_ratio = 0.5    # Warmup proportion
n_warmup     = max(75, floor(n_iter * target_ratio))
```

**Adjusting parameters:**
- Increase `n_iter` for better convergence (at the cost of runtime)
- Modify `n_chains` based on available CPU cores
- Check convergence diagnostics (R̂, ESS) in Stan output

---

## 💻 Computational Requirements

### Recommended Specifications

- **RAM**: 16 GB minimum (32 GB recommended for parallel chains)
- **CPU**: Multi-core processor (4+ cores recommended)
- **Storage**: 2 GB free space for outputs
- **OS**: Windows 10/11, macOS 10.13+, or Linux

### Performance Notes

- **Initial compilation**: Stan models compile on first run (5-10 minutes)
- **Estimation time**: Varies by dataset size and MCMC settings
  - Monte Carlo: ~30 minutes
  - Empirical case: ~1-2 hours
- **Parallel processing**: Utilizes multiple cores for chain parallelization


**[⬆ back to top](#linking-absolute-and-relative-mobility)**

Made with ❤️ for reproducible research

</div>
