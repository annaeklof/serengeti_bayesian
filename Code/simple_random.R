########### COMPARISON RANDOM SIMS VS DISTURBANCE SCEN ###############
## HABITATS
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
    group_by(code, group, dist_rand) %>%
    summarise(per_rand = mean(per)) %>%
    ungroup() %>%
    arrange(dist_rand) %>%
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
    arrange(habitat_higher_pi) %>%
    rename(per_dist = per) %>%
    rename(dist_scen = habitat_higher_pi) %>%
    mutate(tab_rand) %>%
    mutate(per_diff = (per_rand - per_dist)) %>%
    group_by(dist_scen) %>%
    mutate(mean_diff = mean(per_diff)) %>%
    ungroup() %>%
    select(dist_scen, mean_diff) %>%
    distinct()

# from dist scen. If positive - higher persistence in random sims
# than in dist scen (ex 0.9 random - 0.8 dist). If negative - 
# lower persistence in random sims than in dist scen.
# For ex., graminoids mostly positive values. This means that
# consumers get a lower probability to persist if graminoids are
# specifically targeted, in comparison to if the same number of 
# plant species are targeted.

tab_tot %>%
    mutate(label = case_when(
        dist_scen=="D" ~ "size disturbed, n=2, frac=1.2 %",
        dist_scen=="T" ~ "size thicket, n=2, frac=1.4 %",
        dist_scen=="S" ~ "size shrubland, n=5, frac=5.7 %",
        dist_scen=="R" ~ "size riverland, n=7, frac=1.6 %",
        dist_scen=="missing" ~ "size missing hab, n=18, frac=10 %",
        dist_scen=="G" ~ "size grassland, n=46, frac=41 %",
        dist_scen=="W" ~ "size wetland, n=28, frac=31 %",
        dist_scen=="K" ~ "size kopje, n=17, frac=15.4 %",
        TRUE ~ dist_scen
    )) %>%
    ggplot() +
    aes(x=factor(dist_scen), y=mean_diff, color=factor(label)) +
    geom_point(size=2) +
    geom_hline(yintercept = 0, lty= "dashed", color="black", size=0.5) +
    scale_color_manual(name="Random disturbance",values=carto_pal(name="Safe")) +
    xlab("Disturbance scenario") + ylab("Difference in persistence") +
    theme_bw()+
    theme(axis.text.x = element_text( size=10)) 
ggsave(paste0("../Results/Figures/", funcresp, "/random/",
              "rand_habs", ".pdf"), width = 8, height = 6)
    

## TRAITS
# Load distributions from the sim. with random disturbances
# Choose functional response
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"

# Disturbed plant sp based on:
charac <- c("tree", "shrub", "herb", "graminoid", "annual")

# RANDOM SIMULATIONS
# Collect all values from all scenarios and reps into single df tab
for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,"random/",funcresp,"/rand_size_", ch, ".RData"))
    if (ch=="tree"){
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
    arrange(dist_rand) %>%
    mutate(dist_rand=as.character(dist_rand))


# DISTURBANCE SIMULATIONS (no randomness)
# Collect all values from all scenarios into a single df tab_d

for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,funcresp,"/Higher_", ch, ".RData"))
    if (ch=="tree"){
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
    arrange(habitat_higher_pi) %>%
    rename(per_dist = per) %>%
    rename(dist_scen = habitat_higher_pi) %>%
    mutate(tab_rand) %>%
    mutate(per_diff = (per_rand - per_dist)) %>%  # mean rand (all reps) - val
    group_by(dist_scen) %>%
    mutate(mean_diff = mean(per_diff)) %>%
    ungroup() %>%
    select(dist_scen, mean_diff) %>%
    distinct()
# from dist scen. If positive - higher persistence in random sims
# than in dist scen (ex 0.9 random - 0.8 dist). If negative - 
# lower persistence in random sims than in dist scen.
# For ex., graminoids mostly positive values. This means that
# consumers get a lower probability to persist if graminoids are
# specifically targeted, in comparison to if the same number of 
# plant species are targeted.
trait_labels <- c(
    "tree" = "size large trees, n=5, frac=2.8 %",
    "shrub" = "size shrubs/small trees, n=38, frac=24.5 %",
    "herb" = "size herbs and forbs, n=25, frac=17 %",
    "graminoid" = "size grasses, n=55, frac=52.5 %",
    "annual" = "size annual plants, n=6, frac=3.4 %")
tab_tot %>%
    mutate(label = case_when(
        dist_scen=="tree" ~ "size large trees, n=5, frac=2.8 %",
        dist_scen=="shrub" ~ "size shrubs/small trees, n=38, frac=24.5 %",
        dist_scen=="herb" ~ "size herbs and forbs, n=25, frac=17 %",
        dist_scen=="graminoid" ~ "size grasses, n=55, frac=52.5 %",
        dist_scen=="annual" ~ "size annual plants, n=6, frac=3.4 %",
        TRUE ~ dist_scen
    )) %>%
    ggplot() +
    aes(x=factor(dist_scen), y=mean_diff, color=factor(label)) +
    geom_point(size=2) +
    geom_hline(yintercept = 0, lty= "dashed", color="black", size=0.5) +
    scale_color_manual(name="Random disturbance",values=carto_pal(name="Safe")) +
    xlab("Disturbance scenario") + ylab("Difference in persistence") +
    theme_bw()+
    theme(axis.text.x = element_text(angle=90,size=10)) 
ggsave(paste0("../Results/Figures/", funcresp, "/random/",
              "rand_traits", ".pdf"), width = 8, height = 6)

trait_labels <- c(
    "tree" = "size large trees, n=5, frac=2.8 %",
    "shrub" = "size shrubs/small trees, n=38, frac=24.5 %",
    "herb" = "size herbs and forbs, n=25, frac=17 %",
    "graminoid" = "size grasses, n=55, frac=52.5 %",
    "annual" = "size annual plants, n=6, frac=3.4 %")
tab_tot %>%
    mutate(label = case_when(
        dist_scen=="tree" ~ "size large trees, n=5, frac=2.8 %",
        dist_scen=="shrub" ~ "size shrubs/small trees, n=38, frac=24.5 %",
        dist_scen=="herb" ~ "size herbs and forbs, n=25, frac=17 %",
        dist_scen=="graminoid" ~ "size grasses, n=55, frac=52.5 %",
        dist_scen=="annual" ~ "size annual plants, n=6, frac=3.4 %",
        TRUE ~ dist_scen
    )) %>%
    ggplot() +
    aes(x=factor(dist_scen), y=per_diff, color=factor(label)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, lty= "dashed", color="black", size=0.5) +
    scale_color_manual(name="Random disturbance",values=carto_pal(name="Safe")) +
    xlab("Disturbance scenario") + ylab("Difference in persistence") +
    theme_bw()+
    theme(axis.text.x = element_text(angle=90,size=10)) 
    
     
ggsave(paste0("../Results/Figures/", funcresp, "/random/",
              "rand_vs_traits", ".pdf"), width = 8, height = 6)

## GROUPS
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
    mutate(per_diff = (per_rand - per_dist)) %>%
    group_by(dist_scen) %>%
    mutate(mean_diff = mean(per_diff)) %>%
    ungroup() %>%
    select(dist_scen, mean_diff) %>%
    distinct()


tab_tot %>%
    mutate(label = case_when(
        dist_scen=="7" ~ "group 7, n=3, frac=3.2 %",
        dist_scen=="8" ~ "group 8, n=9, frac=4.9 %",
        dist_scen=="9" ~ "group 9, n=25, frac=49.5 %",
        dist_scen=="10" ~ "group 10, n=1, frac=0.6 %",
        dist_scen=="11" ~ "group 11, n=24, frac=6.7 %",
        dist_scen=="12" ~ "group 12, n=1, frac=0.8 %",
        dist_scen=="13" ~ "group 13, n=, frac=33.5 %",
        dist_scen=="14" ~ "group 14, n=2, frac=0.8 %",
        TRUE ~ dist_scen
    )) %>%
    mutate(dist_scen = fct_reorder(dist_scen, as.integer(as_factor(dist_scen)))) %>%
    mutate(label = fct_reorder(label, as.integer(as_factor(label)))) %>%
    ggplot() +
    aes(x=factor(dist_scen), y=mean_diff, color=factor(label)) +
    geom_point(size=2) +
    geom_hline(yintercept = 0, lty= "dashed", color="black", size=0.5) +
    scale_color_manual(name="Random disturbance",values=carto_pal(name="Safe")) +
    xlab("Disturbance scenario") + ylab("Difference in persistence") +
    theme_bw()+
    theme(axis.text.x = element_text(angle=90,size=10)) 

ggsave(paste0("../Results/Figures/", funcresp, "/random/",
              "rand_vs_groups", ".pdf"), width = 8, height = 6)
