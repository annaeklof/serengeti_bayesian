## Functions used by 01_prepare_network.R and 03_figures.R
## Run all scripts with Code/ as working directory.

require(tidyverse)
require(igraph)


## ---------------------------------------------------------------
## Food web
## ---------------------------------------------------------------

# Removes cycles by depth-first search, so that the network becomes acyclic.
# Input: A, adjacency matrix (A[i,j] = 1 if j eats i), producers in the first rows
# Output: A without the back edges found by the search
remove_cycles <- function(A) {
    S <- nrow(A)
    color <- rep(0, S)            # 0 = not visited, 1 = visiting, 2 = done
    back_edges <- matrix(0, S, S)

    visit <- function(i) {
        color[i] <<- 1
        for (j in which(A[i, ] != 0)) {
            if (color[j] == 0) {
                visit(j)
            } else if (color[j] == 1) {
                back_edges[i, j] <<- 1
            }
        }
        color[i] <<- 2
    }

    for (i in 1:S) {
        if (color[i] == 0) visit(i)
    }
    return(A - back_edges)
}

# Builds the acyclic, topologically sorted adjacency matrix used by the
# Bayesian network.
# Input: links, data frame with columns pred_code and prey_code
# Output: matrix M where M[i,j] = 1 if species i eats species j. Rows are
#         in topological order (basal species first), so M is lower triangular.
build_network <- function(links) {
    A <- links %>%
        select(prey_code, pred_code) %>%
        graph_from_data_frame() %>%
        as_adjacency_matrix() %>%
        as.matrix()
    # producers (no prey) first
    A <- A[order(colSums(A)), order(colSums(A))]
    A <- remove_cycles(A)
    o <- A %>%
        graph_from_adjacency_matrix() %>%
        topo_sort(mode = "out")
    A <- A[o, o]
    M <- t(A)
    return(M)
}


## ---------------------------------------------------------------
## Species and plant groupings
## ---------------------------------------------------------------

# Collects consumer type, Baskerville group, habitat and growth form per species.
# Input: paths to the data files
# Output: tibble with one row per species (code, group, type, habitat, growth_form)
read_species <- function(info_file, traits_file) {
    info <- read_csv(info_file, col_types = "cic") %>%
        mutate(habitat = str_trim(habitat))
    traits <- read_tsv(traits_file, col_types = cols(.default = "c")) %>%
        select(code, species = Species, growth_form)
    species <- info %>%
        left_join(traits, by = "code") %>%
        mutate(type = case_when(
            group %in% 1:2 ~ "carnivore",
            group %in% 3:6 ~ "herbivore",
            TRUE ~ "plant"
        )) %>%
        mutate(habitat = ifelse(type == "plant", habitat, NA)) %>%
        mutate(growth_form = ifelse(type == "plant", growth_form, NA))
    return(species)
}

# Lists which plants belong to which disturbance scenario. A plant that
# occurs in two habitats (e.g. "G_K") belongs to both habitat scenarios.
# Input: species, tibble from read_species()
# Output: tibble (scenario, grouping, code), one row per plant and scenario
plant_memberships <- function(species) {
    plants <- species %>%
        filter(type == "plant")
    habitat <- plants %>%
        select(code, habitat) %>%
        separate_rows(habitat, sep = "_") %>%
        filter(habitat != "termite") %>%
        mutate(grouping = "habitat", scenario = habitat)
    growth_form <- plants %>%
        mutate(grouping = "growth form", scenario = growth_form)
    group <- plants %>%
        mutate(grouping = "group", scenario = as.character(group))
    memberships <- bind_rows(habitat, growth_form, group) %>%
        select(scenario, grouping, code)
    return(memberships)
}


## ---------------------------------------------------------------
## Labels for figures
## ---------------------------------------------------------------

scenario_labels <- c(
    "G" = "grassland",
    "W" = "woodland",
    "K" = "kopje",
    "R" = "riparian",
    "S" = "shrubland",
    "T" = "thicket",
    "D" = "disturbed",
    "missing" = "missing",
    "tree" = "trees",
    "shrub" = "shrubs",
    "herb" = "herbs",
    "graminoid" = "grasses",
    "annual" = "annuals",
    "7" = "group 7",
    "8" = "group 8",
    "9" = "group 9",
    "10" = "group 10",
    "11" = "group 11",
    "12" = "group 12",
    "13" = "group 13",
    "14" = "group 14"
)

# Gives a scenario its readable name, e.g. "W" -> "woodland".
# Input: scenario, character vector of scenario codes
# Output: character vector of labels
label_scenario <- function(scenario) {
    return(unname(scenario_labels[scenario]))
}
