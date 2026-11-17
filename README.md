# Disturbance of plant groups in the Serengeti food web

Bayesian network model of how disturbance of groups of plants (habitat, growth
form, trophic group) propagates to consumers in the Serengeti food web
(Baskerville et al. 2011).

## Run

All scripts are run with `Code/` as working directory, in this order:

1. `Rscript 01_prepare_network.R` – builds the acyclic, topologically sorted
   network and lists which plants belong to which disturbance scenario.
   Writes `Code/Data/processed/`.
2. `julia -t auto 02_simulate.jl` – Monte Carlo evaluation of the Bayesian
   network for the control, each targeted disturbance and the random
   disturbances. Writes `Results/simulations/targeted.csv` and `random.csv`.
   Settings (α, β, π, number of iterations and replicates) are at the top of
   the script. The full run (21 scenarios × 5000 random replicates) takes
   about 1.5 h on two cores; with more cores it is correspondingly faster.
3. `Rscript 03_figures.R` – manuscript figures 1–4 and S1–S3, written to
   `Results/figures/`.

Requirements: R with tidyverse, igraph and cowplot; Julia with Distributions
(`julia -e 'using Pkg; Pkg.add("Distributions")'`).

## Data

- `Code/Data/serengeti_links.csv` – feeding links (predator, prey).
- `Code/Data/serengeti_info.csv` – Baskerville group for all species and
  habitat for plants. Habitat codes: G grassland, W woodland, K kopje,
  R riparian, S shrubland, T thicket, D disturbed, missing = no habitat
  given. Plants in two habitats are written e.g. `G_K` and are counted in
  both habitats.
- `Code/Data/All_traits_r.txt` – growth form of plants (from TRY and the IUCN
  growth form scheme).

## Model

Each species goes extinct with a baseline probability π (0.1); disturbed
plants get π = 0.5. A consumer that has lost a fraction f of its prey goes
extinct with probability π + (1 − π) · F(f; α, β), where F is the cumulative
distribution function of a beta distribution (α = β = 3). In the random
disturbances, the same number of plants as in a scenario is drawn among all
129 plants.
