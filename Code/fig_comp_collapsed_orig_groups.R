require(tidyverse)
funcresp = "A3B3"
funcresp = "Linear"
path_node <- "/Users/annak52/Documents/Serengeti/Results/collapsed/"
path_orig <- "/Users/annak52/Documents/Serengeti/Results/"

charac <- c("control", 7:14)
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
    load(paste0(path_node, funcresp, "/HigherNode_", ch, ".RData"))
    if (ch=="control"){
        tab_14 <- persistence
    } else{ 
        tab_14 <- bind_rows(tab_14,persistence)
    }
}

tab_orig <- tab_161 %>%
    group_by(group, habitat_higher_pi) %>%
    summarise(per=mean(per)) %>%
    ungroup() %>%
    mutate(group_higher_pi=habitat_higher_pi) %>%
    select(-habitat_higher_pi) %>%
    mutate(web="original")
tab_14 %>%
    select(-group) %>%
    mutate(group=node) %>%
    select(-node) %>%
    mutate(web="node") %>%
    bind_rows(.,tab_orig) %>%
    filter(group %in% c(1:6)) %>%
    spread(web, per) %>%
    ggplot() +
    geom_point(aes(x=original, y=node, color=factor(group_higher_pi),
                   shape=factor(group))) +
    ggtitle("Disturbance by functional groups, nonlinear response") +
    geom_abline(slope=1,intercept=0)
ggsave(paste0("../Results/Figures/", funcresp, "/comp_node_orig_groups.jpg"), width = 8, height = 6)



