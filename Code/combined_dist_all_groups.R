library(dplyr)
library(tidyverse)
library(rcartocolor)
library(ggplot2)
library(ggrepel)
library("RColorBrewer")
#funcresp = "Linear"
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"
charac <- c("control","S","D","T","R","missing","G","W","K", 7:14,
            "tree", "shrub", "herb", "graminoid", "annual")
dist <- c("S","D","T","R","missing","G","W","K", 7:14,
          "tree", "shrub", "herb", "graminoid", "annual")

habs <- tibble(read.table(paste("Data/serengeti_info_habs.csv",sep=''), sep=",", header=TRUE)) %>%
    select(code, habitat)
habitat <- habs%>%
    mutate(count=1) %>%
    group_by(habitat)%>%
    mutate(tot_group = sum(count)) %>%
    ungroup %>%
    mutate(habitat_higher_pi = habitat) %>%
    select(habitat_higher_pi, tot_group) %>%
    distinct()

traits <- as_tibble(read.table("Data/All_traits_r.txt", header=TRUE, sep="\t")) %>%
    select(code, growth_form) 


pg <- traits %>%
    mutate(count=1) %>%
    group_by(growth_form)%>%
    mutate(tot_group = sum(count)) %>%
    ungroup %>%
    mutate(habitat_higher_pi = growth_form) %>%
    select(habitat_higher_pi, tot_group) %>%
    distinct()

groups <- read_csv(paste("Data/serengeti_groups.csv",sep=''), col_types="ci") 
grupper <-  groups %>%
    mutate(count=1) %>%
    group_by(group)%>%
    mutate(tot_group = sum(count)) %>%
    ungroup %>%
    mutate(habitat_higher_pi = as.character(group)) %>%
    select(habitat_higher_pi, tot_group) %>%
    distinct() %>%
    add_row(., pg) %>%
    add_row(., habitat) %>%
    filter(habitat_higher_pi %in% dist)



for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,funcresp,"/Higher_", ch, ".RData"))
    if (ch=="control"){
        tab <- persistence
    } else{ 
        tab <- bind_rows(tab,persistence) %>%
            filter(group %in% c(3:6)) %>%
            select(code, per, habitat_higher_pi, group)
    }
}


control <- tab %>%
    filter(habitat_higher_pi=="control") %>%
    rename(control=habitat_higher_pi, per_c = per)


for (i in 1:length(dist)) {
    d <- dist[i]
    df_sep <- tab %>%
        filter(habitat_higher_pi==d) %>%
        left_join(.,control, by=c("code", "group")) %>%
        mutate(per_diff = per - per_c)
    
    if (d=="S") {
        df <- df_sep
    } else {
        df <- bind_rows(df, df_sep)
    }
}


final <- df %>% 
    left_join(., grupper) %>%
    
    mutate(habitat_higher_pi = case_when(
        habitat_higher_pi=="shrub" ~ "shrubs",
        habitat_higher_pi== "tree" ~ "trees",
        habitat_higher_pi=="herb" ~ "herbs",
        habitat_higher_pi=="graminoid" ~ "grasses",
        habitat_higher_pi=="annual" ~ "annuals",
        habitat_higher_pi=="7" ~ "group 7",
        habitat_higher_pi=="8" ~ "group 8",
        habitat_higher_pi=="9" ~ "group 9",
        habitat_higher_pi=="10" ~ "group 10",
        habitat_higher_pi=="11" ~ "group 11",
        habitat_higher_pi=="12" ~ "group 12",
        habitat_higher_pi=="13" ~ "group 13",
        habitat_higher_pi=="14" ~ "group 14",
        habitat_higher_pi=="D" ~ "disturbed",
        habitat_higher_pi=="T" ~ "thicket",
        habitat_higher_pi=="S" ~ "shrubland",
        habitat_higher_pi=="R" ~ "riverland",
        habitat_higher_pi=="missing" ~ "missing",
        habitat_higher_pi=="W" ~ "wetland",
        habitat_higher_pi=="K" ~ "kopje",
        habitat_higher_pi=="G" ~ "grassland",
        TRUE ~ habitat_higher_pi
    )) %>%
    mutate(habitat_higher_pi = fct_relevel(habitat_higher_pi, "disturbed",
                                           "grassland", "kopje", "missing",
                                           "riverland", "shrubland", "thicket", "wetland",
                                           "group 7", "group 8", "group 9","group 10",
                                           "group 11","group 12","group 13","group 14",
                                           "trees", "grasses","herbs","annuals", "shrubs"
    )) 

myPalette <- colorRampPalette(brewer.pal(10, "Spectral"))
sc <- scale_colour_gradientn(colours = rev(myPalette(10)), limits=c(0,64), name="Number of\nplant species \n")

final %>% 
    ggplot(aes(x=habitat_higher_pi, y=per_diff, color=tot_group)) +
    geom_boxplot(width=0.5,lwd=0.6) +
    xlab("Disturbed plant group") + ylab("Persistence impact") +
    sc +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13)) 
ggsave(paste0("../Results/Figures/", funcresp, "/compl_dist_herbi.jpg"), width = 8, height = 5)

animals <- c("PEDCAP", "DAMKOR", "PANPAR", "TRASCR", "ACIJUB")
spec <- final %>%
    filter(code %in% animals) %>%
    mutate(pred_code=code) %>%
    select(-code) %>%
    left_join(., ranking_sub, by="pred_code")

spec %>% 
    ggplot() +
    aes(x=pred_code, y=per) +
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


spec %>% 
    ggplot() +
    aes(x=pred_code, y=per, color=factor(habitat_higher_pi), shape=factor(habitat_higher_pi), size=factor(habitat_higher_pi)) +
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
