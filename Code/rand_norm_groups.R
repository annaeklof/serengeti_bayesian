require(tidyverse)
library(ggplot2)
library(rcartocolor)


# Load distributions from the sim. with random disturbances
# Choose functional response
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"

# Disturbed plant sp based on:
charac <- c(7:14) # plant groups 7-14

# RANDOM SIMULATIONS
# Collect all values from all scenarios and reps into single df tab
for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,"random/",funcresp,"/rand_size_", ch, ".RData"))
    if (ch==7){
        tab <- per
    } else{ 
        tab <- bind_rows(tab,per)
    }
}

# Calculate the mean probability to survive for each species over
# all replicates, result is one value per species
tab_rand <- tab %>%
    filter(group %in% c(1:6)) %>%
    rename(dist_rand=group_size) %>%
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
    if (ch==7){
        tab_d <- persistence
    } else{ 
        tab_d <- bind_rows(tab_d,persistence)
    }
}
# modify tab_d (df of disturbance sims) and add results from
# tab_rand
tab_tot <- tab_d %>%
    filter(group %in% c(1:6)) %>%
    select(code, habitat_higher_pi, group, per) %>%
    arrange(code) %>%
    rename(per_dist = per) %>%
    rename(dist_scen = habitat_higher_pi) %>%
    mutate(tab_rand) %>%
    mutate(per_diff = (per_rand - per_dist))

labs_groups <- c(
    "7" = "group 7, n=3, frac=3.2 %",
    "8" = "group 8, n=9, frac=4.9 %",
    "9" = "group 9, n=25, frac=49.5 %",
    "10" = "group 10, n=1, frac=0.6 %",
    "11" = "group 11, n=24, frac=6.7 %",
    "12" = "group 12, n=1, frac=0.8 %",
    "13" = "group 13, n=, frac=33.5 %",
    "14" = "group 14, n=2, frac=0.8 %"
)

ggplot(tab_tot) +
    aes(x=factor(code), y=per_diff, color=factor(dist_scen)) +
    geom_point() +
    scale_color_manual(guide=FALSE,values=carto_pal(name="Safe")) +
    facet_wrap(~factor(dist_scen), labeller=as_labeller(labs_groups)) +
    ggtitle("Plants randomly disturbed following functional groups") +
    xlab("Consumer group") + ylab("Persistence") +
    theme_bw() +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90, size=5)) 

ggsave(paste0("../Results/Figures/", funcresp, "/random/",
              "rand_vs_groups", ".pdf"), width = 8, height = 6)


# to find fraction of links going from consumers to plant groups
links <- tibble(read.table(paste("Data/serengeti_links.csv",sep=''), sep=",", header=TRUE)) %>%
    mutate_if(is.factor, as.character)
groups <- read_csv(paste("Data/serengeti_groups.csv",sep=''), col_types="ci") 


# Connect predators to their respective group
pred_links <- links %>% 
    select(code=pred_code) %>%
    left_join(., groups, by="code") %>%
    select(pred_code=code, pred_group=group)
# Connect preys to their resp. group, and combine the resulting 
# data frame with the one for predators
links_groups <- links %>% 
    select(code=prey_code) %>%
    left_join(., groups, by="code") %>%
    select(prey_code=code, prey_group=group) %>%
    bind_cols(pred_links,.)
# below summarizes the fraction of links going into each plant group
# (here plant groups based on traits) from herbivores (cons.gr 3:6)
frac_links <- links_groups %>%
    filter(pred_group %in% c(3:6)) %>%
    mutate(pred_code = 1, prey_code=1) %>%
    group_by(prey_group) %>%
    mutate(counts = sum(prey_code)) %>%
    ungroup() %>%
    distinct(.) %>%
    select(prey_group, counts) %>%
    distinct(.) %>%
    mutate(all_links = sum(counts)) %>%
    mutate(frac=round((counts/all_links),3))

