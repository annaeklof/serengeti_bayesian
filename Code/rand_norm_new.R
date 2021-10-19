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

ggplot(tab_tot) +
    aes(x=factor(code), y=per_diff, color=factor(dist_scen)) +
    geom_point() +
    scale_color_manual(guide=FALSE,values=carto_pal(name="Safe")) +
    facet_wrap(~factor(dist_scen, levels=c(7:14))) +
    ggtitle("Plants randomly disturbed following functional groups") +
    xlab("Consumer group") + ylab("Persistence") +
    theme_bw() +
    theme(legend.position = "none") 
    


