require(tidyverse)
funcresp = "A3B3"
#funcresp = "Linear"
weight = "/w"
path_node <- "/Users/annak52/Documents/Serengeti/Results/collapsed/trait_node/"
path_orig <- "/Users/annak52/Documents/Serengeti/Results/original/"

col_traits <- c( "#009E73", "#000000", "#0072B2", "#D55E00",
                 "#CC79A7", "#999999")

charac <- c("control", "tree", "shrub", "herb", "annual", "graminoid")
for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path_orig, funcresp, "/HigherHab_", ch, ".RData"))
    if (ch=="control"){
        tab_161 <- persistence
    } else{ 
        tab_161 <- bind_rows(tab_161,persistence)
    }
}

for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path_node,funcresp, weight, "/HigherTrait_", ch, ".RData"))
    if (ch=="control"){
        tab_11 <- persistence
    } else{ 
        tab_11 <- bind_rows(tab_11,persistence)
    }
}
names <- c("group_1", "group_2", "group_3", "group_4", "group_5", "group_6",
           "gr_7", "gr_8", "gr_9", "gr_10", "gr_11", "gr_12", "gr_13", "gr_14")

tab_orig <- tab_161 %>%
    group_by(group, habitat_higher_pi) %>%
    summarise(per=mean(per)) %>%
    ungroup() %>%
    mutate(group_higher_pi=habitat_higher_pi) %>%
    select(-habitat_higher_pi) %>%
    mutate(web="original") %>%
    mutate(group=names[group]) %>%
    filter(group %in% c(names[1:6]))

tab_11 %>%
    mutate(web="node") %>%
    filter(group %in% c(names[1:6])) %>%
    bind_rows(.,tab_orig) %>%
    spread(web, per) %>%
    mutate(group_higher_pi = case_when(
        group_higher_pi=="control" ~ "control",
        group_higher_pi== "tree" ~ "large trees",
        group_higher_pi=="shrub" ~ "shrubs/small trees",
        group_higher_pi=="herb" ~ "herbs and forbs",
        group_higher_pi=="graminoid" ~ "grasses",
        group_higher_pi=="annual" ~ "annual plants",
        TRUE ~ group_higher_pi
    )) %>%
    mutate(group = case_when(
        group=="group_1" ~ "group 1",
        group== "group_2" ~ "group 2",
        group=="group_3" ~ "group 3",
        group=="group_4" ~ "group 4",
        group=="group_5" ~ "group 5",
        group=="group_6" ~ "group 6",
        TRUE ~ group_higher_pi
    )) %>%
    mutate(group_higher_pi = fct_reorder(group_higher_pi, as.integer(as_factor(group_higher_pi)))) %>%
    
    ggplot() +
    geom_point(aes(x=original, y=node, color=factor(group_higher_pi),
                   shape=factor(group), size=factor(group)), alpha=0.8) +
    geom_abline(slope=1,intercept=0) +
    xlab("Original network") + ylab("Collapsed network") +
    scale_color_manual(name = "Plant trait", values=col_traits) +
    scale_shape_manual(name = "Consumer group", values=c(0, 2, 1, 15, 17, 19)) +
    scale_size_manual(guide=FALSE, values=rep(3,6))
ggsave(paste0("../Results/Figures/", funcresp,"/collapsed_traits_nl_w.jpg"), width = 8, height = 6)












