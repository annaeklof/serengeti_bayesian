require(tidyverse)
library(rcartocolor)
library(cowplot)
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
        tab_m <- persistence
    } else{ 
        tab_m <- bind_rows(tab_m,persistence) %>%
            mutate(level="medium")
    }
}

for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,funcresp,"/lowerPi25/Higher_", ch, ".RData"))
    if (ch=="control"){
        tab_l <- persistence
    } else{ 
        tab_l <- bind_rows(tab_l,persistence) %>%
            mutate(level="low")
    }
}

for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,funcresp,"/higherPi75/Higher_", ch, ".RData"))
    if (ch=="control"){
        tab_h <- persistence
    } else{ 
        tab_h <- bind_rows(tab_h,persistence) %>%
            mutate(level="high")
    }
}

tab <- bind_rows(tab_l, tab_m, tab_h) %>%
    select(code, group, per, habitat_higher_pi, level)


col_groups <- c( "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7", "#999999", "#E69F00", "#56B4E9",
                 "#000000")
col_traits <- c( "#009E73", "#000000", "#0072B2", "#D55E00",
                 "#CC79A7", "#999999")


get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

# HABITATS
p2 <- tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control","D","T","S","R","missing",
                                                       "G","W","K")) %>%
    mutate(type=if_else(group%in%c(1,2),"carnivores","herbivores")) %>%
    mutate(control=if_else(habitat_higher_pi=="control","control","disturbed")) %>%
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
    mutate(level = fct_reorder(level, as.integer(as_factor(level)))) %>%
    #filter(type=="herbivores") %>%
    filter(type=="carnivores") %>%
    ggplot() +
    aes(x=code, y=per, color=factor(habitat_higher_pi), shape=control, size=control) +
    geom_point(alpha=0.7) +
    facet_grid(type~factor(level)) +
    xlab("Consumer") + ylab("Persistence") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size=5)) +
    # scale_color_manual(name = "Disturbed feature", labels = labs_habs, 
    #                    values=cbp1) +
    scale_color_manual(name="Disturbed feature", values=rev(carto_pal(name="Safe"))) +
    scale_shape_manual(name="", values=c(17,19)) +
    scale_size_manual(guide=FALSE, name="", values=c(4,2))
#plot_grid(p1, p2, ncol=1)

legend <- get_legend(p2) # from the fig with legend, other has guide=FALSE in all scale_
p2 <- p2 + theme(legend.position="none")
p3<-grid.arrange(p1, p2, legend, ncol=3, widths=c(2.5, 1.8, 0.7))
#g <- arrangeGrob(p1, p2, legend, ncol=3, widths=c(2.5, 1.8, 0.7))
ggsave(paste0("../Results/Figures/", funcresp, "/habs_all_dist.jpg"), p3, width = 12, height = 5)

# TRAITS
p2 <- tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control", "tree","shrub",
                                                       "herb", "graminoid", "annual")) %>%
    mutate(control=if_else(habitat_higher_pi=="control","control","disturbed")) %>%
    mutate(type=if_else(group%in%c(1,2),"carnivores","herbivores")) %>%
    mutate(habitat_higher_pi = case_when(
        habitat_higher_pi=="control" ~ "control, n=129",
        habitat_higher_pi== "tree" ~ "large trees, n=5",
        habitat_higher_pi=="shrub" ~ "shrubs/small trees, n=38",
        habitat_higher_pi=="herb" ~ "herbs and forbs, n=25",
        habitat_higher_pi=="graminoid" ~ "grasses, n=55",
        habitat_higher_pi=="annual" ~ "annual plants, n=6",
        TRUE ~ habitat_higher_pi
    )) %>%
    mutate(habitat_higher_pi = fct_reorder(habitat_higher_pi, as.integer(as_factor(habitat_higher_pi)))) %>%
    mutate(level = fct_reorder(level, as.integer(as_factor(level)))) %>%
    #filter(type=="herbivores") %>%
    filter(type=="carnivores") %>%
    ggplot() +
    aes(x=code, y=per, color=factor(habitat_higher_pi), shape=control, size=control) +
    geom_point(alpha=0.7) +
    facet_grid(type~factor(level)) +
    xlab("Consumer") + ylab("Persistence") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size=5)) +
    #scale_color_brewer(name = "Disturbed feature", palette="RdBu") +
    scale_color_manual( name="Disturbed feature", values=rev(carto_pal(name="Safe"))) +
    scale_shape_manual( name="", values=c(17,19)) +
    scale_size_manual(guide=FALSE, name="", values=c(4,2))

legend <- get_legend(p2) # from the fig with legend, other has guide=FALSE in all scale_
p2 <- p2 + theme(legend.position="none")
p3<-grid.arrange(p1, p2, legend, ncol=3, widths=c(2.5, 1.8, 0.7))
ggsave(paste0("../Results/Figures/", funcresp, "/trait_all_dist.jpg"), p3, width = 12, height = 5)

# GROUPS

p2 <- tab %>%
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
    mutate(level = fct_reorder(level, as.integer(as_factor(level)))) %>%
    #filter(type=="herbivores") %>%
    filter(type=="carnivores") %>%
    ggplot() +
    aes(x=code, y=per, color=factor(habitat_higher_pi), shape=control, size=control) +
    geom_point(alpha=0.7) +
    facet_grid(type~factor(level)) +
    xlab("Consumer") + ylab("Persistence") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size=5)) +
    #scale_color_brewer(name = "Disturbed feature", palette="RdBu") +
    scale_color_manual( name="Disturbed feature", values=rev(carto_pal(name="Safe"))) +
    scale_shape_manual( name="", values=c(17,19)) +
    scale_size_manual(guide=FALSE, name="", values=c(4,2))

legend <- get_legend(p2) # from the fig with legend, other has guide=FALSE in all scale_
p2 <- p2 + theme(legend.position="none")
p3<-grid.arrange(p1, p2, legend, ncol=3, widths=c(2.5, 1.8, 0.7))
ggsave(paste0("../Results/Figures/", funcresp, "/group_all_dist.jpg"), p3, width = 12, height = 5)
