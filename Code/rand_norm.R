require(tidyverse)
library(ggplot2)
theme_set(theme_bw())
library(rcartocolor)


# Load distributions from the sim. with random disturbances
# Choose functional response
#funcresp = "Linear"
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"

labs_groups <- c(
    "control" = "control, n=129",
    "7" = "group 7, n=3",
    "8" = "group 8, n=9",
    "9" = "group 9, n=25",
    "10" = "group 10, n=1",
    "11" = "group 11, n=24",
    "12" = "group 12, n=1",
    "13" = "group 13, n=64",
    "14" = "group 14, n=2"
)

labs_habs <- c(
    "control" = "control, n=129",
    "D" = "disturbed, n=2",
    "T" = "thicket, n=2",
    "S" = "shrubland, n=5",
    "R" = "riverland, n=7",
    "missing" = "missing hab, n=18",
    "G" = "grassland, n=46",
    "W" = "wetland, n=28",
    "K" = "kopje, n=17")

labs_traits <- c(
    "control" = "control, n=129",
    "tree" = "large trees, n=5",
    "shrub" = "shrubs/small trees, n=38",
    "herb" = "herbs and forbs, n=25",
    "graminoid" = "grasses, n=55",
    "annual" = "annual plants, n=6")

# Disturbed plant sp based on original functional groups 7-14
charac <- c(7:14)
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
tab <- rename(tab, dist_rand = group_size)

# Calculate the mean probability to survive for each species over
# all replicates, result is one value per species
tab_rand <- tab %>%
    filter(group %in% c(1:6)) %>%
    select(code, group, per, dist_rand, rep) %>%
    group_by(code, dist_rand, group) %>%
    summarise(per_rand = mean(per)) %>%
    ungroup() 

# Load control data set
load(paste0(path,funcresp,"/HigherHab_control", ".RData"))
control <- persistence %>%
    mutate(rand_size=habitat_higher_pi, Per=per) %>%
    select(code, rand_size, group, Per) %>%
    filter(group %in% c(1:6)) %>%
    arrange(group)


# Connect data sets and create figure comparing mean persistence
# over groups of consumer sp. in randomized scen vs control scen
bind_rows(control, all) %>%
    spread(rand_size, Per) %>%
    gather(dist_type, persistence, "7","8",
           "9", "10", "11", "12", "13", "14") %>%
    ggplot() +
    geom_point(aes(x=control, y=persistence, color=dist_type), alpha=0.7) +
    xlab("Persistence undisturbed") + ylab("Persistence disturbed") +
    facet_wrap(~dist_type, scales="free_x", labeller=as_labeller(labs_groups)) +
    geom_abline(slope=1,intercept=0) +
    scale_color_manual(guide=FALSE,values=carto_pal(name="Safe")) +
    theme_bw()
ggsave(paste0("../Results/Figures/", funcresp, "/random/contr_vs_random.jpg"), width = 8, height = 7)

# y-axis: difference in persistence between random simulation 
# (mean persistence each consumer species) AND real disturbance
# scenarios (mean persistence each consumer species).
# x-axis: consumer species

# load persistences from disturbance sims (no randomness)

