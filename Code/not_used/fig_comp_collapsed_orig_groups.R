require(tidyverse)
funcresp = "A3B3"
#funcresp = "Linear"
weight = "/w"
path_node <- "/Users/annak52/Documents/Serengeti/Results/collapsed/groups/"
path_orig <- "/Users/annak52/Documents/Serengeti/Results/original/"

col_groups <- c( "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7", "#999999", "#E69F00", "#56B4E9",
                 "#000000")

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
    load(paste0(path_node, funcresp, weight, "/HigherNode_", ch, ".RData"))
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
    mutate(group_higher_pi = case_when(
        group_higher_pi=="control" ~ "control",
        group_higher_pi== "7" ~ "group 7",
        group_higher_pi=="8" ~ "group 8",
        group_higher_pi=="9" ~ "group 9",
        group_higher_pi=="10" ~ "group 10",
        group_higher_pi=="11" ~ "group 11",
        group_higher_pi=="12" ~ "group 12",
        group_higher_pi=="13" ~ "group 13",
        group_higher_pi=="14" ~ "group 14",
        TRUE ~ group_higher_pi
    )) %>%
    mutate(group=as.character(group)) %>%
    mutate(group = case_when(
        group=="1" ~ "group 1",
        group== "2" ~ "group 2",
        group=="3" ~ "group 3",
        group=="4" ~ "group 4",
        group=="5" ~ "group 5",
        group=="6" ~ "group 6",
        TRUE ~ group
    )) %>%
    mutate(group_higher_pi = fct_reorder(group_higher_pi, as.integer(as_factor(group_higher_pi)))) %>%
    
    ggplot() +
    geom_point(aes(x=original, y=node, color=factor(group_higher_pi),
                   shape=factor(group), size=factor(group)), alpha=0.85) +
    geom_abline(slope=1,intercept=0) +
    xlab("Original network") + ylab("Collapsed network") +
    scale_color_manual(name = "Plant group/node", values=col_groups) +
    scale_shape_manual(name = "Consumer group", values=c(0, 2, 1, 15, 17, 19)) +
    scale_size_manual(guide=FALSE, values=rep(3,6))+
    theme_bw()
ggsave(paste0("../Results/Figures/", funcresp, "/collapsed_groups_nl_uw.jpg"), width = 8, height = 6)



