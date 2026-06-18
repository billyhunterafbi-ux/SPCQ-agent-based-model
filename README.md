# SPC-Q Agent-Based Model with Monte Carlo Simulations

This repository contains the implementation of the **Sensory–Predictability–Context (SPC-Q)** agent-based model described in the accompanying manuscript:

> *Distinct behavioural pathways converge on similar microbiome states*

The model is designed as a **minimal mechanistic framework** linking cognition, behaviour, and ecology. It demonstrates how dietary behaviour can emerge from latent sensory traits and environmental modulation, and how distinct behavioural pathways can converge on similar gut microbiome outcomes.

---

## Overview

The SPC-Q model represents food choice as a **stochastic, state-dependent decision process** with reinforcement learning. Agents repeatedly select from a structured food environment under fluctuating contextual conditions (e.g. stress, sensory load). Over time, these dietary choices shape an associated microbial community.

### Key features

- Continuous latent traits governing behaviour (sensory drive, predictability preference, context sensitivity)
- Probabilistic choice via a softmax decision rule
- Experience-dependent learning using a reinforcement learning update
- A simplified, dynamic microbiome module driven by dietary inputs
- Multiple Monte Carlo simulations to characterise variability and degeneracy

The model is **illustrative rather than predictive**: it is intended to isolate core mechanisms and demonstrate general principles, not to provide fully converged quantitative estimates.

---

## Files

- `SPCQ_agent_based_model_MC.R`  
  Main R script implementing the SPC-Q model, Monte Carlo simulations, and analysis pipeline.

- `SPCQ_generate_figures_MC.R`  
  R script to reproduce Figures 1–4 shown in the manuscript from model output files. This script does **not** run simulations, except for the parameterised robustness analysis (Figure 4).

- `Microbiome_alpha_beta_summary_MC.csv`  
  Output summary file containing mean and variability metrics for microbiome structure across emergent behavioural strategies.

---

## Requirements

The model is implemented in **R** and uses the following packages:

- `dplyr` – data manipulation  
- `tidyr` – tidy data utilities  
- `ggplot2` – plotting  
- `ggtern` – ternary (compositional) visualisation  
- `proxy` – distance and similarity calculations  
- `knitr` – summary tables  

All packages are available from CRAN.

---

## Usage

1. Open R or RStudio.  
2. Set the working directory to the location of the scripts.  
3. Run the model:

```r
source("SPCQ_agent_based_model_MC.R")
```

Running the model script will:

- Simulate food choice behaviour for a population of agents across multiple Monte Carlo runs
- Compute agent-level summaries of dietary behaviour and microbiome structure
- Write a summary CSV file to disk

The number of Monte Carlo simulations can be adjusted via the `n_sims` parameter near the top of the script.

### Reproducing figures only

To regenerate manuscript figures **without rerunning simulations**:

```r
source("SPCQ_generate_figures_MC.R")
```

---

## Outputs

The scripts produce:

- Agent-level summaries of dietary entropy, repertoire size, and microbiome metrics
- Publication-ready figures illustrating relationships between dietary diversity, behavioural strategy, and microbiome dominance
- Publication-ready figures illustrating emergent behaviour, strategy structure, degeneracy (Figure 3), and robustness to parameter variation (Figure 4)
- A CSV file summarising mean and variability of microbiome structure across emergent strategies

Figures are saved at publication resolution (300 dpi).

---

## Figure descriptions

The figure generation script reproduces four main figures:

- **Figure 1 — Emergent behaviour**  
  Illustrates the conceptual structure of the SPC-Q model, latent trait space, and the emergence of dietary diversity across agents.

- **Figure 2 — Behavioural strategies**  
  Maps latent SPC traits onto emergent behavioural strategies (Explorer, Opportunist, Specialist) and links dietary behaviour to microbiome dominance.

- **Figure 3 — Degeneracy analysis**  
  Demonstrates that large differences in latent trait space can produce similar dietary behaviour, and that similar behaviours can converge on similar microbiome states. Degenerate regions are identified using distance-based thresholds.

- **Figure 4 — Robustness / sensitivity analysis**  
  Evaluates whether the degeneracy phenomenon persists under variation in key model parameters:
  - learning rate (`alpha`)
  - choice stochasticity (`beta`)
  - microbiome persistence (`decay`)
  
  The analysis recomputes pairwise latent–behavioural relationships across parameter settings and shows that weak coupling (degeneracy) is preserved across a broad parameter space.

---

## Reproducibility

Each Monte Carlo run is initialised with a fixed random seed to ensure reproducibility. A `sessionInfo()` call can be added at the end of the scripts if exact package versions are required.

---

## Scope and limitations

This model intentionally simplifies both behaviour and microbiome dynamics:

- Physiological gut–brain feedbacks are not included  
- Behaviour is restricted to dietary choice  
- Microbial taxa are represented as abstract functional groups  

Despite these simplifications, the model captures the emergence of structured behavioural regimes and **degeneracy**, where distinct latent and behavioural pathways converge on similar microbiome states.

---

## License

This code is released under the MIT License.

If you use or adapt this model, please cite the accompanying manuscript.

---

## Citation

If you use this code, please cite the accompanying manuscript and the Zenodo archive:

DOI: 10.5281/zenodo.xxxxxxx](https://doi.org/10.5281/zenodo.20665865

---

## Contact

**William Ross Hunter**  
Agri-Food and Bioscience Institute (AFBI), Belfast  
Email: Billy.Hunter@afbini.gov.uk
