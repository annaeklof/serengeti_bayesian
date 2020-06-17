require(tidyverse)
funcresp = "Linear"
#funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/"
charac <- c("control","S","D","T","R","G","W","K", 7:14, "Woody", "NWoody", "P", 
            "A", "tree", "shrub", "herb", "graminoid", "annual")

for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,funcresp,"/HigherHab_", ch, ".RData"))
    if (ch=="control"){
        tab <- persistence
    } else{ 
        tab <- bind_rows(tab,persistence)
    }
}


# GROUPS
tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control", 7:14)) %>% # only interested in group 1-6 --> consumer sp.
    ggplot() +
    aes(x=factor(group), y=per, color=factor(group)) +
    geom_boxplot(outlier.size=0.75) +
    facet_wrap(~factor(habitat_higher_pi, levels=c("control", 7:14))) +
    ggtitle("Plants disturbed by groups, original web") +
    xlab("Consumer group") + ylab("Persistence") +
    theme_bw() +
    theme(legend.position = "none")
ggsave(paste0("../Results/Figures/", funcresp, "/box_orig_groups.jpg"), width = 6, height = 6)

# GENERAL CHARACTERS
tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control", "Woody", "NWoody", 
                                                       "P", "A")) %>% 
    ggplot() +
    aes(x=factor(group), y=per, color=factor(group)) +
    geom_boxplot(outlier.size=0.75) +
    facet_wrap(~factor(habitat_higher_pi, levels=c("control", "Woody", "NWoody", 
                                                   "P", "A"))) +
    ggtitle("Plants disturbed by gen. characters, original web") +
    xlab("Consumer group") + ylab("Persistence") +
    theme_bw() +
    theme(legend.position = "none")
ggsave(paste0("../Results/Figures/", funcresp, "/box_orig_gen_charac.jpg"), width = 6, height = 6)

# TRAITS
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
    theme(legend.position = "none")
ggsave(paste0("../Results/Figures/", funcresp,"/box_orig_traits.jpg"), width = 6, height = 6)

# HABITATS
tab %>%
    filter(group %in% c(1:6), habitat_higher_pi %in% c("control", "S","D","T","R","G","W","K")) %>% # only interested in group 1-6 --> consumer sp.
    ggplot() +
    aes(x=factor(group), y=per, color=factor(group)) +
    geom_boxplot(outlier.size=0.75) +
    facet_wrap(~factor(habitat_higher_pi, levels=c("control", "S","D","T","R","G","W","K"))) +
    ggtitle("Plants disturbed by habitat") +
    xlab("Consumer group") + ylab("Persistence") +
    theme_bw() +
    theme(legend.position = "none")
ggsave(paste0("../Results/Figures/", funcresp, "/box_orig_habitats.jpg"), width = 6, height = 6)


