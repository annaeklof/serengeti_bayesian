require(tidyverse)
funcresp = "A3B3"
funcresp = "Linear"
path_node <- "/Users/annak52/Documents/Serengeti/Results/collapsed/trait_node/"
path_orig <- "/Users/annak52/Documents/Serengeti/Results/"

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
    load(paste0(path_node,funcresp, "/HigherTrait_", ch, ".RData"))
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
    ggplot() +
    geom_point(aes(x=original, y=node, color=factor(group_higher_pi),
                   shape=factor(group))) +
    ggtitle("Disturbance by traits, linear response") +
    geom_abline(slope=1,intercept=0)
ggsave(paste0("../Results/Figures/", funcresp,"/comp_node_orig_traits.jpg"), width = 8, height = 6)












