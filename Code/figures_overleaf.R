require(tidyverse)
library(rcartocolor)
#funcresp = "Linear"
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"
#charac <- c("control","S","D","T","R","missing","G","W","K", 7:14, "Woody", 
#            "NWoody", "P", "A", "tree", "shrub", "herb", "graminoid", "annual")
charac <- c("control","S","D","T","R","missing","G","W","K", 7:14,
            "tree", "shrub", "herb", "graminoid", "annual")

for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,funcresp,"/Higher_", ch, ".RData"))
    if (ch=="control"){
        tab <- persistence
    } else{ 
        tab <- bind_rows(tab,persistence)
    }
}



cbp2 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#000000", "#000099",  "#996633", "#CC79A7", "#660066" )
# HABITATS
p1 <- tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control","D","T","S","R","missing",
                                                       "G","W","K")) %>%
    mutate(type=if_else(group%in%c(1,2),"Carnivores","Herbivores")) %>%
    #mutate(control=if_else(habitat_higher_pi=="control","control","disturbed")) %>%
    mutate(habitat_higher_pi = case_when(
        habitat_higher_pi=="control" ~ "control, n=129",
        habitat_higher_pi=="D" ~ "disturbed, n=2",
        habitat_higher_pi=="T" ~ "thicket, n=2",
        habitat_higher_pi=="S" ~ "shrubland, n=5",
        habitat_higher_pi=="R" ~ "riverland, n=7",
        habitat_higher_pi=="missing" ~ "missing, n=18",
        habitat_higher_pi=="W" ~ "wetland, n=28",
        habitat_higher_pi=="K" ~ "kopje, n=17",
        habitat_higher_pi=="G" ~ "grassland, n=46",
        TRUE ~ habitat_higher_pi
    )) %>%
    mutate(habitat_higher_pi = fct_relevel(habitat_higher_pi, "control, n=129", "disturbed, n=2",
                                           "grassland, n=46", "kopje, n=17", "missing, n=18",
                                           "riverland, n=7", "shrubland, n=5", "thicket, n=2", "wetland, n=28")) %>%
    ggplot() +
    aes(x=code, y=per, color=factor(habitat_higher_pi), shape=factor(habitat_higher_pi), size=factor(habitat_higher_pi)) +
    geom_point(alpha=0.7) +
    #facet_wrap(~factor(group), scales="free_x", labeller=as_labeller(dist_labels)) +
    facet_wrap(~factor(type), scales="free_x") +
    xlab("Consumer") + ylab("Persistence") +
    theme_bw() +
    #theme(axis.text.x = element_text(angle = 90, size=7)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    scale_color_manual(name="Disturbed group", values=cbp2) +
    scale_shape_manual(name="Disturbed group", values=c(17,19, 19, 19, 19, 19, 19, 19, 19)) +
    scale_size_manual(name="Disturbed group", values=c(3.5,2,2,2,2,2,2,2,2))  +
    ylim(0.4,0.9) 
#ggsave(paste0("../Results/Figures/", funcresp, "/dist_hab.jpg"), width = 8, height = 5)

# TRAITS
p2 <- tab %>% 
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control", "tree","shrub",
                                                       "herb", "graminoid", "annual")) %>%
    #mutate(control=if_else(habitat_higher_pi=="control","control","disturbed")) %>%
    mutate(type=if_else(group%in%c(1,2),"Top consumers","Primary consumers")) %>%
    mutate(habitat_higher_pi = case_when(
        habitat_higher_pi=="control" ~ "control, n=129",
        habitat_higher_pi=="shrub" ~ "shrubs, n=38",
        habitat_higher_pi== "tree" ~ "trees, n=5",
        habitat_higher_pi=="herb" ~ "herbs, n=25",
        habitat_higher_pi=="graminoid" ~ "grasses, n=55",
        habitat_higher_pi=="annual" ~ "annuals, n=6",
        TRUE ~ habitat_higher_pi
    )) %>% 
    mutate(habitat_higher_pi = fct_relevel(habitat_higher_pi, "control, n=129", "shrubs, n=38",
                                           "trees, n=5", "grasses, n=55","herbs, n=25","annuals, n=6")) %>%
    ggplot() +
    aes(x=code, y=per, color=factor(habitat_higher_pi), shape=factor(habitat_higher_pi), size=factor(habitat_higher_pi)) +
    geom_point(alpha=0.7) +
    facet_wrap(~factor(type), scales="free_x") +
    xlab("Consumer") + ylab("Persistence") +
    theme_bw() +
    #theme(axis.text.x = element_text(angle = 90, size=7)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    scale_color_manual(name="Disturbed group", values=cbp2) +
    scale_shape_manual(name="Disturbed group", values=c(17,19, 19, 19, 19, 19, 19, 19, 19)) +
    scale_size_manual(name="Disturbed group", values=c(3.5,2,2,2,2,2,2,2,2))  +
    ylim(0.4,0.9) 
#ggsave(paste0("../Results/Figures/", funcresp, "/dist_traits.jpg"), width = 8, height = 5)


# GROUPS

p3 <- tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control", 7:14)) %>%
    mutate(type=if_else(group%in%c(1,2),"Top consumers","Primary consumers")) %>%
    #mutate(control=if_else(habitat_higher_pi=="control","control","disturbed")) %>%
    mutate(habitat_higher_pi = case_when(
        habitat_higher_pi=="control" ~ "control, n=129",
        habitat_higher_pi=="7" ~ "group 7, n=3",
        habitat_higher_pi=="8" ~ "group 8, n=9",
        habitat_higher_pi=="9" ~ "group 9, n=25",
        habitat_higher_pi=="10" ~ "group 10, n=1",
        habitat_higher_pi=="11" ~ "group 11, n=24",
        habitat_higher_pi=="12" ~ "group 12, n=1",
        habitat_higher_pi=="13" ~ "group 13, n=64",
        habitat_higher_pi=="14" ~ "group 14, n=2",
        TRUE ~ habitat_higher_pi
    )) %>%
    mutate(habitat_higher_pi = fct_reorder(habitat_higher_pi, as.integer(as_factor(habitat_higher_pi)))) %>%
    ggplot() +
    aes(x=code, y=per, color=factor(habitat_higher_pi), shape=factor(habitat_higher_pi), size=factor(habitat_higher_pi)) +
    geom_point(alpha=0.7) +
    facet_wrap(~factor(type), scales="free_x") +
    xlab("Consumer species") + ylab("Persistence") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size=7)) +
    scale_color_manual(name="Disturbed group", values=cbp2) +
    scale_shape_manual(name="Disturbed group", values=c(17,19, 19, 19, 19, 19, 19, 19, 19)) +
    scale_size_manual(name="Disturbed group", values=c(3.5,2,2,2,2,2,2,2,2))  +
    ylim(0.4,0.9) 

#ggsave(paste0("../Results/Figures/", funcresp, "/dist_groups75.jpg"), width = 8, height = 5)

 plot_grid(p1, p2, p3,
          nrow = 3,
          labels = "AUTO",
          label_size = 12,
          align = "v",
          rel_heights = c(3,3,4))
ggsave(paste0("../Results/Figures/", funcresp, "/all_dist.jpg"), width = 8, height = 9.5)

########### COMPARISON RANDOM SIMS VS DISTURBANCE SCEN ###############
## HABITATS
# Load distributions from the sim. with random disturbances
# Choose functional response
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"

# Disturbed plant sp based on:
charac <- c("S","D","T","R","missing","G","W","K")
#charac <- c("G","W","K")

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
    if (ch=="G"){
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
    mutate(per_diff = (per_rand - per_dist))
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
p1 <- ggplot(tab_tot) +
    aes(x=factor(code), y=per_diff) +
    geom_point(color="darkblue") +
    geom_hline(yintercept = 0, lty= "dashed", color="black", size=0.5) +
    #scale_color_manual(guide=FALSE,values=carto_pal(name="Safe")) +
    facet_wrap(~factor(dist_scen), labeller=as_labeller(labs_habs)) +
    xlab("Consumer species") + ylab("Persistence difference") +
    #theme(axis.text.x = element_text(angle = 90, size=5)) 
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    ylim(-0.2,0.4) 

#ggsave(paste0("../Results/Figures/", funcresp, "/random/",
#              "rand_vs_habs", ".pdf"), width = 8, height = 6)

## TRAITS
# Load distributions from the sim. with random disturbances
# Choose functional response
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"

# Disturbed plant sp based on:
#charac <- c("tree", "shrub", "herb", "graminoid", "annual")
charac <- c("shrub", "herb", "graminoid")

# RANDOM SIMULATIONS
# Collect all values from all scenarios and reps into single df tab
for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,"random/",funcresp,"/rand_size_", ch, ".RData"))
    if (ch=="shrub"){
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
    if (ch=="shrub"){
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
    mutate(per_diff = (per_rand - per_dist))  # mean rand (all reps) - val
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
p2 <- ggplot(tab_tot) +
    aes(x=factor(code), y=per_diff) +
    geom_point(color="darkblue") +
    geom_hline(yintercept = 0, lty= "dashed", color="black", size=0.5) +
    #scale_color_manual(guide=FALSE,values=carto_pal(name="Safe")) +
    facet_wrap(~factor(dist_scen), labeller=as_labeller(trait_labels)) +
    xlab("Consumer species") + ylab("Persistence difference") +
    #theme(axis.text.x = element_text(angle = 90, size=5)) 
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    ylim(-0.2,0.4) 

ggsave(paste0("../Results/Figures/", funcresp, "/random/",
              "rand_vs_traits", ".pdf"), width = 8, height = 6)


## GROUPS
# Load distributions from the sim. with random disturbances
# Choose functional response
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"

# Disturbed plant sp based on:
#charac <- c(7:14) # plant groups 7-14
charac <- c(9, 11, 13)
# RANDOM SIMULATIONS
# Collect all values from all scenarios and reps into single df tab
for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,"random/",funcresp,"/rand_size_", ch, ".RData"))
    if (ch==9){
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
    if (ch==9){
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
    "7" = "size group 7, n=3, frac=3.2 %",
    "8" = "size group 8, n=9, frac=4.9 %",
    "9" = "size group 9, n=25, frac=49.5 %",
    "10" = "size group 10, n=1, frac=0.6 %",
    "11" = "size group 11, n=24, frac=6.7 %",
    "12" = "size group 12, n=1, frac=0.8 %",
    "13" = "size group 13, n=, frac=33.5 %",
    "14" = "size group 14, n=2, frac=0.8 %"
)

p3 <- tab_tot %>%
    mutate(dist_scen = fct_reorder(dist_scen, as.integer(as_factor(dist_scen)))) %>%
    ggplot() +
    aes(x=factor(code), y=per_diff) +
    geom_point(color="darkblue") +
    geom_hline(yintercept = 0, lty= "dashed", color="black", size=0.5) +
    #scale_color_manual(guide=FALSE,values=carto_pal(name="Safe")) +
    facet_wrap(~factor(dist_scen), labeller=as_labeller(labs_groups)) +
    #ggtitle("Plants randomly disturbed following functional groups") +
    xlab("Consumer species") + ylab("Persistence difference") +
    theme_bw() +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90, size=6)) +
    ylim(-0.2,0.4) 


#ggsave(paste0("../Results/Figures/", funcresp, "/random/",
#              "rand_vs_groups", ".pdf"), width = 8, height = 6)
plot_grid(p1, p2, p3,
          nrow = 3,
          labels = "AUTO",
          label_size = 12,
          align = "v",
          rel_heights = c(3,3,3))
ggsave(paste0("../Results/Figures/", funcresp, "/selected_random.jpg"), width = 8.5, height = 8.5)
