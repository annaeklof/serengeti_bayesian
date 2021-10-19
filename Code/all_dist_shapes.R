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


dist_labels = c(
    "1" = "cons. group 1 (large carni)",
    "2" = "cons. group 2 (small carni)",
    "3" = "cons. group 3 (small herbi)",
    "4" = "cons. group 4 (large grazers)",
    "5" = "cons. group 5 (hyraxes)",
    "6" = "cons. group 6")

col_groups <- c( "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7", "#999999", "#E69F00", "#56B4E9",
                 "#000000")
col_traits <- c( "#009E73", "#000000", "#0072B2", "#D55E00",
                 "#CC79A7", "#999999")

# HABITATS
p1 <- tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control","D","T","S","R","missing",
                                                       "G","W","K")) %>%
    mutate(type=if_else(group%in%c(1,2),"carnivores","herbivores")) %>%
    #mutate(control=if_else(habitat_higher_pi=="control","control","disturbed")) %>%
    mutate(habitat_higher_pi = case_when(
        habitat_higher_pi=="control" ~ "control, n=129",
        habitat_higher_pi=="D" ~ "disturbed, n=2",
        habitat_higher_pi=="T" ~ "thicket, n=2",
        habitat_higher_pi=="S" ~ "shrubland, n=5",
        habitat_higher_pi=="R" ~ "riverland, n=7",
        habitat_higher_pi=="missing" ~ "missing hab, n=18",
        habitat_higher_pi=="W" ~ "wetland, n=28",
        habitat_higher_pi=="K" ~ "kopje, n=17",
        habitat_higher_pi=="G" ~ "grassland, n=46",
        TRUE ~ habitat_higher_pi
    )) %>%
    mutate(habitat_higher_pi = fct_reorder(habitat_higher_pi, as.integer(as_factor(habitat_higher_pi)))) %>%
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
    scale_color_manual(name="Disturbed group", values=c("red", "darkblue", "darkblue",
                                                        "darkblue", "darkblue", "darkblue",
                                                        "darkblue", "darkblue", "darkblue")) +
    scale_shape_manual(name="Disturbed group", values=c(17,3,4,15,16,5,8,18,10)) +
    scale_size_manual(name="Disturbed group", values=c(4.5,3,3,3,3,3,3,3,3))  +
    ylim(0.4,0.9) 
ggsave(paste0("../Results/Figures/", funcresp, "/dist_hab75.jpg"), width = 8, height = 5)

# TRAITS
p2 <- tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control", "tree","shrub",
                                                       "herb", "graminoid", "annual")) %>%
    #mutate(control=if_else(habitat_higher_pi=="control","control","disturbed")) %>%
    mutate(type=if_else(group%in%c(1,2),"carnivores","herbivores")) %>%
    mutate(habitat_higher_pi = case_when(
        habitat_higher_pi=="control" ~ "control, n=129",
        habitat_higher_pi== "tree" ~ "trees, n=5",
        habitat_higher_pi=="shrub" ~ "shrubs, n=38",
        habitat_higher_pi=="herb" ~ "herbs, n=25",
        habitat_higher_pi=="graminoid" ~ "grasses, n=55",
        habitat_higher_pi=="annual" ~ "annual plants, n=6",
        TRUE ~ habitat_higher_pi
    )) %>%
    mutate(habitat_higher_pi = fct_reorder(habitat_higher_pi, as.integer(as_factor(habitat_higher_pi)))) %>%
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
          axis.ticks.x=element_blank()) +
    #scale_color_brewer(name = "Disturbed feature", palette="RdBu") +
    scale_color_manual(name="Disturbed group", values=c("red", "darkblue", "darkblue",
                                                          "darkblue", "darkblue", "darkblue")) +
    scale_shape_manual(name="Disturbed group", values=c(17,3,4,15,16,5)) +
    scale_size_manual(name="Disturbed group", values=c(4.5,3,3,3,3,3))  +
    ylim(0.4,0.9) 
ggsave(paste0("../Results/Figures/", funcresp, "/dist_traits75.jpg"), width = 8, height = 5)


# GROUPS

p3 <- tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control", 7:14)) %>%
    mutate(type=if_else(group%in%c(1,2),"carnivores","herbivores")) %>%
    mutate(control=if_else(habitat_higher_pi=="control","control","disturbed")) %>%
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
    xlab("Consumer") + ylab("Persistence") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size=7)) +
    scale_color_manual(name="Disturbed group", values=c("red", "darkblue", "darkblue",
                                                          "darkblue", "darkblue", "darkblue",
                                                          "darkblue", "darkblue", "darkblue")) +
    scale_shape_manual(name="Disturbed group", values=c(17,3,4,15,16,5,8,18,10)) +
    scale_size_manual(name="Disturbed group", values=c(4.5,3,3,3,3,3,3,3,3))  +
    ylim(0.4,0.9) 

ggsave(paste0("../Results/Figures/", funcresp, "/dist_groups75.jpg"), width = 8, height = 5)

plot_grid(p1, p2, p3,
          nrow = 3,
          labels = "AUTO",
          label_size = 12,
          align = "v",
          rel_heights = c(3,3,4))
ggsave(paste0("../Results/Figures/", funcresp, "/all_dist_shapes.jpg"), width = 8, height = 9.5)



