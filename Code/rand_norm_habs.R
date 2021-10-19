require(tidyverse)
library(ggplot2)
library(rcartocolor)


# Load distributions from the sim. with random disturbances
# Choose functional response
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"

# Disturbed plant sp based on:
charac <- c("S","D","T","R","missing","G","W","K")

# RANDOM SIMULATIONS
# Collect all values from all scenarios and reps into single df tab
for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,"random/",funcresp,"/rand_size_", ch, ".RData"))
    if (ch=="S"){
        tab <- per
    } else{ 
        tab <- bind_rows(tab,per)
    }
}

# Calculate the mean probability to survive for each species over
# all replicates, result is one value per species
tab_rand <- tab %>%
    filter(group %in% c(1:6)) %>%
    rename(dist_rand=rand_size) %>%
    select(code, group, per, dist_rand, rep) %>%
    group_by(code, dist_rand, group) %>%
    summarise(per_rand = mean(per)) %>%
    ungroup() %>%
    mutate(dist_rand=as.character(dist_rand))


# DISTURBANCE SIMULATIONS (no randomness)
# Collect all values from all scenarios into a single df tab_d

for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,funcresp,"/Higher_", ch, ".RData"))
    if (ch=="S"){
        tab_d <- persistence
    } else{ 
        tab_d <- bind_rows(tab_d,persistence)
    }
}
# modify tab_d (df of disturbance sims) and add results from
# tab_rand.
tab_tot <- tab_d %>%
    filter(group %in% c(1:6)) %>%
    select(code, habitat_higher_pi, group, per) %>%
    arrange(code) %>%
    rename(per_dist = per) %>%
    rename(dist_scen = habitat_higher_pi) %>%
    mutate(tab_rand) %>%
    mutate(per_diff = (per_rand - per_dist)) # mean rand (all reps) - val
# from dist scen. If positive - higher persistence in random sims
# than in dist scen (ex 0.9 random - 0.8 dist). If negative - 
# lower persistence in random sims than in dist scen.
# For ex., graminoids mostly positive values. This means that
# consumers get a lower probability to persist if graminoids are
# specifically targeted, in comparison to if the same number of 
# plant species are targeted.
labs_habs <- c(
    "D" = "size disturbed, n=2, frac=1.2 %",
    "T" = "size thicket, n=2, frac=1.4 %",
    "S" = "size shrubland, n=5, frac=5.7 %",
    "R" = "size riverland, n=7, frac=1.6 %",
    "missing" = "size missing hab, n=18, frac=10 %",
    "G" = "size grassland, n=46, frac=41 %",
    "W" = "size wetland, n=28, frac=31 %",
    "K" = "size kopje, n=17, frac=15.4 %")
ggplot(tab_tot) +
    aes(x=factor(code), y=per_diff, color=factor(dist_scen)) +
    geom_point() +
    scale_color_manual(guide=FALSE,values=carto_pal(name="Safe")) +
    facet_wrap(~factor(dist_scen), labeller=as_labeller(labs_habs)) +
    xlab("Consumer species") + ylab("Difference in persistence") +
    theme(axis.text.x = element_text(angle = 90, size=5)) 
ggsave(paste0("../Results/Figures/", funcresp, "/random/",
              "rand_vs_habs", ".pdf"), width = 8, height = 6)


#####################################################################

# to find fraction of links going from consumers to plant groups
links <- tibble(read.table(paste("Data/serengeti_links.csv",sep=''), sep=",", header=TRUE)) %>%
    mutate_if(is.factor, as.character)
habs <- tibble(read.table(paste("Data/serengeti_info_K.csv",sep=''), sep=",", header=TRUE)) %>%
    select(code, habitat)


# Connect predators to their respective habitat (non-existing)
pred_links <- links %>% 
    select(code=pred_code) %>%
    left_join(., habs, by="code") %>%
    select(pred_code=code, pred_trait=habitat)
# Connect preys to their resp. habitat, and combine the resulting 
# data frame with the one for predators
links_groups <- links %>% 
    select(code=prey_code) %>%
    left_join(., habs, by="code") %>%
    select(prey_code=code, prey_trait=habitat) %>%
    bind_cols(pred_links,.)
# below summarizes the fraction of links going into each plant group
# (here plant groups based on traits) from herbivores
# Problematic habitats: G, W, K (plants can belong to two habitats)
frac_links <- links_groups %>%
    filter(pred_trait %in% c("small_herbi", "large_grazer", "mega_grazer", "hyrax")) %>%
    mutate(pred_code = 1, prey_code=1) %>%
    group_by(prey_trait) %>%
    mutate(counts = sum(prey_code)) %>%
    ungroup() %>%
    distinct(.) %>%
    select(prey_trait, counts) %>%
    distinct(.) %>%
    mutate(all_links = sum(counts)) %>%
    mutate(frac=round((counts/all_links),3))





