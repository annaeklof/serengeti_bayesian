require(tidyverse)
funcresp = "Linear"

# GROUPS
path <- paste0("/Users/annak52/Documents/Serengeti/Results/collapsed/groups/", funcresp)
charac <- c("control", 7:14)
for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,"/HigherNode_", ch, ".RData"))
    if (ch=="control"){
        tab <- persistence
    } else{ 
        tab <- bind_rows(tab,persistence)
    }
}

tab %>%
    filter(node %in% c(1:6), group_higher_pi %in% c("control", 7:14)) %>% # only interested in group 1-6 --> consumer sp.
    ggplot() +
    aes(x=factor(node), y=per, color=factor(node)) +
    geom_boxplot(outlier.size=0.75) +
    facet_wrap(~factor(group_higher_pi, levels=c("control", 7:14))) +
    ggtitle("Disturbance of groups, collapsed nw") +
    xlab("Consumer group") + ylab("Persistence") +
    theme_bw() +
    theme(legend.position = "none")
ggsave(paste0("../Results/Figures/", funcresp, "/box_collapsed_groups.jpg"), width = 6, height = 6)

# TRAITS IN GROUPS
path <- paste0("/Users/annak52/Documents/Serengeti/Results/collapsed/trait_in_group/", funcresp)
charac <- c("control", "tree", "shrub", "herb", "annual", "graminoid")
for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,"/HigherNode_", ch, ".RData"))
    if (ch=="control"){
        tab <- persistence
    } else{ 
        tab <- bind_rows(tab,persistence)
    }
}

tab %>%
    filter(group %in% c("sm_herbi", "large_graz", "hyrax", "large_herbi", "sm_carni", "large_carni")) %>% # only interested in group 1-6 --> consumer sp.
    ggplot() +
    aes(x=factor(group), y=per, color=factor(group)) +
    geom_boxplot(outlier.size=0.75) +
    facet_wrap(~factor(group_higher_pi, levels=c("control", "tree", "shrub", 
                                                 "herb", "annual", "graminoid"))) +
    ggtitle("Disturbance of traits in groups, collapsed nw") +
    xlab("Consumer group") + ylab("Persistence") +
    scale_x_discrete(labels=c("1","2","3", "4","5","6")) +
    theme_bw() +
    theme(legend.position = "none")
ggsave(paste0("../Results/Figures/", funcresp, "/box_collapsed_trait_in_groups.jpg"), width = 6, height = 6)

# TRAITS
path <-  paste0("/Users/annak52/Documents/Serengeti/Results/collapsed/trait_node/", funcresp)
charac <- c("control", "tree", "shrub", "herb", "annual", "graminoid")

for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,"/HigherTrait_", ch, ".RData"))
    if (ch=="control"){
        tab <- persistence
    } else{ 
        tab <- bind_rows(tab,persistence)
    }
}

names <- c("group_1", "group_2", "group_3", "group_4", "group_5", "group_6")
vec <- c(1:6)
tab %>%
    filter(group %in% c(names[1:6])) %>%
    ggplot() +
    aes(x=factor(group), y=per, color=factor(group)) +
    geom_boxplot(outlier.size=0.75) +
    facet_wrap(~factor(group_higher_pi, levels=c("control", "tree", "shrub", 
                                                 "herb", "annual", "graminoid"))) +
    ggtitle("Distrubance of traits, collapsed nw") +
    xlab("Consumer group") + ylab("Persistence") +
    scale_x_discrete(labels=c("1","2","3", "4","5","6")) +
    theme_bw() +
    theme(legend.position = "none") 
ggsave(paste0("../Results/Figures/", funcresp, "/box_collapsed_traits.jpg"), width = 6, height = 6)

