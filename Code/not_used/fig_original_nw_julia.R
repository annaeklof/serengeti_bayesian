require(tidyverse)
library(rcartocolor)
#funcresp = "Linear"
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"
charac <- c("control","S","D","T","R","missing","G","W","K", 7:14, "Woody", 
            "NWoody", "P", "A", "tree", "shrub", "herb", "graminoid", "annual")

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


# BOXPLOTS GROUPS
tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control", 7:14)) %>% # only interested in group 1-6 --> consumer sp.
    ggplot() +
    aes(x=factor(group), y=per, color=factor(group)) +
    geom_boxplot(outlier.size=0.75) +
    facet_wrap(~factor(habitat_higher_pi, levels=c("control", 7:14))) +
    ggtitle("Plants disturbed by functional groups") +
    xlab("Consumer group") + ylab("Persistence") +
    theme_bw() +
    theme(legend.position = "none") +
    ggsave(paste0("../Results/Figures/", funcresp, "/box_orig_groups.jpg"), width = 6, height = 6)

# BOXPLOTS GENERAL CHARACTERS
tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control", "Woody", "NWoody", 
                                                       "P", "A")) %>% 
    ggplot() +
    aes(x=factor(group), y=per, color=factor(group)) +
    geom_boxplot(outlier.size=0.75) +
    facet_wrap(~factor(habitat_higher_pi, levels=c("control", "Woody", "NWoody", 
                                                   "P", "A"))) +
    ggtitle("Plants disturbed by general characters") +
    xlab("Consumer group") + ylab("Persistence") +
    theme_bw() +
    theme(legend.position = "none") +
    ggsave(paste0("../Results/Figures/", funcresp, "/box_orig_gen_charac.jpg"), width = 6, height = 6)

# BOXPLOTS TRAITS
tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control", "tree", "shrub", 
                                                       "herb", "graminoid", "annual")) %>% 
    ggplot() +
    aes(x=factor(group), y=per, color=factor(group)) +
    geom_boxplot(outlier.size=0.75) +
    facet_wrap(~factor(habitat_higher_pi, levels=c("control", "tree", "shrub", 
                                                   "herb", "graminoid", "annual"))) +
    ggtitle("Plants disturbed by traits") +
    xlab("Consumer group") + ylab("Persistence") +
    theme_bw() +
    theme(legend.position = "none") +
    ggsave(paste0("../Results/Figures/", funcresp,"/box_orig_traits.jpg"), width = 6, height = 6)

# HABITATS
tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control", "S","D","T","R","G","W","K")) %>%  # only interested in group 1-6 --> consumer sp.
    ggplot() +
    aes(x=factor(group), y=per, color=factor(group)) +
    geom_boxplot(outlier.size=0.75) +
    facet_wrap(~factor(habitat_higher_pi, levels=c("control", "S","D","T","R","G","W","K"))) +
    ggtitle("Plants disturbed by habitats") +
    xlab("Consumer group") + ylab("Persistence") +
    theme_bw() +
    theme(legend.position = "none") 

ggsave(paste0("../Results/Figures/", funcresp, "/box_orig_habitats.jpg"), width = 6, height = 6)


# TRAITS NO BOXPLOT (all consumers on x-axis, panels disturbance scen)
trait_labels <- c(
    "control" = "control, n=129",
    "tree" = "large trees, n=5",
    "shrub" = "shrubs/small trees, n=38",
    "herb" = "herbs and forbs, n=25",
    "graminoid" = "grasses, n=55",
    "annual" = "annual plants, n=6")

tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control", "tree", "shrub", 
                                                       "herb", "graminoid", "annual")) %>%
    # only interested in group 1-6 --> consumer sp.
    ggplot() +
    aes(x=reorder(code,group), y=per, color=factor(group)) +
    geom_point() +
    facet_wrap(~factor(habitat_higher_pi), labeller=as_labeller(trait_labels)) +
    ggtitle("Trait disturbance, nonlinear") +
    xlab("Consumer") + ylab("Persistence") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size=7)) 

    ggsave(paste0("../Results/Figures/", funcresp, "/orig_trait.jpg"), width = 10, height = 6)


# HABITATS no boxplots (all consumers on x-axis, panels disturbance scen)
hab_labels <- c(
    "control" = "control, n=129",
    "S" = "shrubland, n=5",
    "D" = "disturbed, n=2",
    "R" = "riverland, n=7",
    "G" = "grassland, n=46",
    "W" = "wetland, n=28",
    "K" = "kopje, n=17")

tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control", "S","D","R","G","W","K")) %>%
    # only interested in group 1-6 --> consumer sp.
    ggplot() +
    aes(x=reorder(code,group), y=per, color=factor(group)) +
    geom_point() +
    facet_wrap(~factor(habitat_higher_pi), labeller=as_labeller(hab_labels)) +
    ggtitle("Habitat disturbance, nonlinear") +
    xlab("Consumer") + ylab("Persistence") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size=7)) 

    ggsave(paste0("../Results/Figures/", funcresp, "/orig_habitats.jpg"), width = 10, height = 6)


## TRIAL WITH DIST of apex --> only linear?
for (i in 1:8){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,funcresp,"/HigherHab_", ch, "_apex.RData"))
    if (ch=="control"){
        tab_c <- persistence
    } else{ 
        tab_c <- bind_rows(tab_c,persistence)
    }
}

tab_hab <- tab %>%
    filter(group==1, habitat_higher_pi %in% c("control", "S","R","G","W","K")) %>% # only interested in group 1-6 --> consumer sp.
    mutate(type="hab_disturbed") %>%
    select(code, group, per, habitat_higher_pi, type)

# Figure trial with points
tab_c %>%
    filter(group==1, habitat_higher_pi %in% c("control", "S","R","G","W","K")) %>% # only interested in group 1-6 --> consumer sp.
    mutate(type="hab_cons_disturbed") %>%
    select(code, group, per, habitat_higher_pi, type) %>%
    bind_rows(.,tab_hab) %>%
    #spread(type, per) %>%
    ggplot() +
    geom_point(aes(x=code, y=per, 
                   color=type, shape=factor(habitat_higher_pi))) +
    #ggtitle("Plants disturbed by habitat") +
    xlab("Apex predators") + ylab("Persistence") +
    theme_bw() 
    #theme(legend.position = "none")
    
    #Figure trial with boxplot
    tab_c %>%
    filter(group==1, habitat_higher_pi %in% c("control", "S","D","T","R","G","W","K")) %>% # only interested in group 1-6 --> consumer sp.
    mutate(type="hab_cons_disturbed") %>%
    select(code, group, per, habitat_higher_pi, type) %>%
    bind_rows(.,tab_hab) %>%
    #spread(type, per) %>%
    ggplot() +
    aes(x=type, y=per, color=type) +
    geom_boxplot(outlier.size=0.75) +
    facet_wrap(~factor(habitat_higher_pi, levels=c("control", "S","D","T","R","G","W","K"))) 

#ggtitle("Plants disturbed by habitat") +
xlab("Apex predators") + ylab("Persistence") +
    theme_bw() +
    theme(legend.position = "none")

all %>%
    filter(habitat_higher_pi=="control")


# VARIOUS DISTURBANCES, OVER CONSUMER GROUPS
labs <- c(
    "control" = "control, n=129",
    "S" = "shrubland, n=5",
    "R" = "riverland, n=7",
    "missing" = "missing hab, n=18",
    "G" = "grassland, n=46",
    "W" = "wetland, n=28",
    "K" = "kopje, n=17",
    "tree" = "large trees, n=5",
    "shrub" = "shrubs/small trees, n=38",
    "herb" = "herbs and forbs, n=25",
    "graminoid" = "grasses, n=55",
    "annual" = "annual plants, n=6")

dist_labels = c(
    "1" = "cons. group 1 (large carni)",
    "2" = "cons. group 2 (small carni)",
    "3" = "cons. group 3 (small herbi)",
    "4" = "cons. group 4 (large grazers)",
    "5" = "cons. group 5 (hyraxes)",
    "6" = "cons. group 6")

tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control","S","R","missing","G","W","K",
                                                       "tree","shrub","herb", "graminoid", "annual")) %>%
    mutate(control=if_else(habitat_higher_pi=="control","control","disturbed")) %>%
    ggplot() +
    aes(x=code, y=per, color=factor(habitat_higher_pi), shape=control, size=control) +
    geom_point(alpha=0.7) +
    facet_grid(habitat_higher_pi~factor(group), scales="free_x") +
    ggtitle("Various disturbances, nonlinear") +
    xlab("Consumer") + ylab("Persistence") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size=7)) +
    scale_color_manual(name = "Disturbed feature", labels = labs, 
                       values=carto_pal(name="Safe")) +
    scale_shape_manual(name="", values=c(4,19)) +
    scale_size_manual(guide=FALSE, name="", values=c(4,2))
ggsave(paste0("../Results/Figures/", funcresp, "/disturb.jpg"), width = 10, height = 6)

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
tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control","D","T","S","R","missing",
                                                       "G","W","K")) %>%
    mutate(control=if_else(habitat_higher_pi=="control","control","disturbed")) %>%
    ggplot() +
    aes(x=code, y=per, color=factor(habitat_higher_pi), shape=control, size=control) +
    geom_point(alpha=0.7) +
    facet_wrap(~factor(group), scales="free_x", labeller=as_labeller(dist_labels)) +
    #ggtitle("Various disturbances, nonlinear") +
    xlab("Consumer") + ylab("Persistence") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size=7)) +
    scale_color_manual(name = "Disturbed feature", labels = labs_habs, 
                       values=carto_pal(name="Safe")) +
    scale_shape_manual(name="", values=c(4,19)) +
    scale_size_manual(guide=FALSE, name="", values=c(4,2))
ggsave(paste0("../Results/Figures/", funcresp, "/cons_sep_hab.jpg"), width = 8, height = 5)

tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control", "tree","shrub",
                                                       "herb", "graminoid", "annual")) %>%
    mutate(control=if_else(habitat_higher_pi=="control","control","disturbed")) %>%
    ggplot() +
    aes(x=code, y=per, color=factor(habitat_higher_pi), shape=control, size=control) +
    geom_point(alpha=0.7) +
    facet_wrap(~factor(group), scales="free_x", labeller=as_labeller(dist_labels)) +
    #ggtitle("Various disturbances, nonlinear") +
    xlab("Consumer") + ylab("Persistence") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size=7)) +
    scale_color_manual(name = "Disturbed feature", labels = labs_traits, 
                       values=carto_pal(name="Safe")) +
    scale_shape_manual(name="", values=c(4,19)) +
    scale_size_manual(guide=FALSE, name="", values=c(4,2))
ggsave(paste0("../Results/Figures/", funcresp, "/cons_sep_traits.jpg"), width = 8, height = 5)

tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control", 7:14)) %>%
    mutate(control=if_else(habitat_higher_pi=="control","control","disturbed")) %>%
    ggplot() +
    aes(x=code, y=per, color=factor(habitat_higher_pi), shape=control, size=control) +
    geom_point(alpha=0.7) +
    facet_wrap(~factor(group), scales="free_x", labeller=as_labeller(dist_labels)) +
    #ggtitle("Various disturbances, nonlinear") +
    xlab("Consumer") + ylab("Persistence") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size=7)) +
    scale_color_manual(name = "Disturbed feature", labels = labs_groups, 
                       values=carto_pal(name="Safe")) +
    scale_shape_manual(name="", values=c(4,19)) +
    scale_size_manual(guide=FALSE, name="", values=c(4,2)) 
ggsave(paste0("../Results/Figures/", funcresp, "/cons_sep_groups.jpg"), width = 8, height = 5)


# COMPARISON CONTROL-DISTURBANCES

labs_traits <- c(
    "control" = "control, n=129",
    "tree" = "large trees, n=5",
    "shrub" = "shrubs/small trees, n=38",
    "herb" = "herbs and forbs, n=25",
    "graminoid" = "grasses, n=55",
    "annual" = "annual plants, n=6")

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

# HABITATS
tab %>%
    filter(group %in% c(1:6), 
           habitat_higher_pi %in% c("control","S","R", "D", "T",
                                    "missing","G","W","K")) %>%
    spread(habitat_higher_pi,per) %>%
    gather(dist_type, persistence, "S", "R", "missing", "D", "T", "G", "W", "K") %>%
    ggplot() +
    geom_point(aes(x=control, y=persistence, color=dist_type), alpha=0.7) +
    #facet_wrap(~dist_type)
    xlab("Persistence undisturbed") + ylab("Persistence disturbed") +
    geom_abline(slope=1,intercept=0) +
    scale_color_manual(name = "Disturbed feature", labels = labs_habs, 
                       values=carto_pal(name="Safe")) +
    theme_bw()
ggsave(paste0("../Results/Figures/", funcresp, "/comp_habs.jpg"), width = 8, height = 5)

# TRAITS
tab %>%
    filter(group %in% c(1:6), 
           habitat_higher_pi %in% c("control", "tree","shrub",
                                    "herb", "graminoid", "annual")) %>%
    spread(habitat_higher_pi,per) %>%
    gather(dist_type, persistence, "tree","shrub",
           "herb", "graminoid", "annual") %>%
    ggplot() +
    geom_point(aes(x=control, y=persistence, color=dist_type), alpha=0.7) +
    xlab("Persistence undisturbed") + ylab("Persistence disturbed") +
    #facet_wrap(~dist_type) +
    geom_abline(slope=1,intercept=0) +
    scale_color_manual(name = "Disturbed feature", labels = labs_traits, 
                       values=carto_pal(name="Safe")) +
    theme_bw()
ggsave(paste0("../Results/Figures/", funcresp, "/comp_traits.jpg"), width = 8, height = 5)

# GROUPS SEP
tab %>%
    filter(group %in% c(1:6), 
           habitat_higher_pi %in% c("control",7:14)) %>%
    spread(habitat_higher_pi,per) %>%
    gather(dist_type, persistence, "7","8",
           "9", "10", "11", "12", "13", "14") %>%
    ggplot() +
    geom_point(aes(x=control, y=persistence, color=dist_type), alpha=0.7) +
    xlab("Persistence undisturbed") + ylab("Persistence disturbed") +
    facet_wrap(~dist_type, scales="free_x", labeller=as_labeller(labs_groups)) +
    geom_abline(slope=1,intercept=0) +
    scale_color_manual(guide=FALSE,values=carto_pal(name="Safe")) +
    theme_bw()
ggsave(paste0("../Results/Figures/", funcresp, "/comp_groups_sep.jpg"), width = 8, height = 7)

# GROUPS 
tab %>%
    filter(group %in% c(1:6), 
           habitat_higher_pi %in% c("control",7:14)) %>%
    spread(habitat_higher_pi,per) %>%
    gather(dist_type, persistence, "7","8",
           "9", "10", "11", "12", "13", "14") %>%
    ggplot() +
    geom_point(aes(x=control, y=persistence, color=dist_type), alpha=0.7) +
    xlab("Persistence undisturbed") + ylab("Persistence disturbed") +
    #facet_wrap(~dist_type) +
    geom_abline(slope=1,intercept=0) +
    scale_color_manual(name = "Disturbed feature", labels = labs_groups, 
                       values=carto_pal(name="Safe")) +
    theme_bw()
ggsave(paste0("../Results/Figures/", funcresp, "/comp_groups.jpg"), width = 8, height = 5)

