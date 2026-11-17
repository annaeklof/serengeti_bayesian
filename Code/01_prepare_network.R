## 01 Prepare the network and the disturbance scenarios
## Reads the raw Serengeti data and writes the files that 02_simulate.jl needs.
## Run with Code/ as working directory.

## Settings
links_file  <- "Data/serengeti_links.csv"
info_file   <- "Data/serengeti_info.csv"
traits_file <- "Data/All_traits_r.txt"
out_path    <- "Data/processed/"

source("functions.R")


## Network: acyclic and topologically sorted (basal species first)
links <- read_csv(links_file, col_types = "cc")
M <- build_network(links)

# the lion cannibalism link is the only link removed
n_removed <- nrow(links) - sum(M)
print(paste("Links removed to make the network acyclic:", n_removed))


## Species information, in the same order as the network
species <- read_species(info_file, traits_file) %>%
    mutate(sp_no = match(code, rownames(M))) %>%
    arrange(sp_no)

# all species in the network must be in the info file, and vice versa
stopifnot(all(rownames(M) == species$code))


## Disturbance scenarios: which plants belong to which scenario
memberships <- plant_memberships(species)

scenario_sizes <- memberships %>%
    count(grouping, scenario, name = "n_plants")
print(scenario_sizes, n = Inf)


## Save
dir.create(out_path, showWarnings = FALSE, recursive = TRUE)

M %>%
    as.data.frame() %>%
    write_csv(paste0(out_path, "network.csv"))

species %>%
    select(sp_no, code, species, type, group, habitat, growth_form) %>%
    write_csv(paste0(out_path, "species.csv"), na = "")

memberships %>%
    write_csv(paste0(out_path, "memberships.csv"))
