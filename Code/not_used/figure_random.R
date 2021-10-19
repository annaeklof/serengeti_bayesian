require(tidyverse)
library(ggplot2)
theme_set(theme_bw())

# Load distributions from the sim. with random disturbances
# Choose functional response
funcresp = "Linear"
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"
# Disturbed plant sp based on original functional groups 7-14
charac <- c(7:14)
# Collect all values from all scenarios and reps into single df tab
for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,"random/",funcresp,"/rand_size_", ch, ".RData"))
    if (ch==7){
        tab <- per
    } else{ 
        tab <- bind_rows(tab,per)
    }
}

## IF boxplots
boxplots_random <- function() {
    # Calculate the mean probability to survive for each species over
    # all replicates, result is one value per species
    all <- tab %>%
        filter(group %in% c(1:6)) %>%
        select(code, group, per, group_size, rep) %>%
        group_by(code, group_size, group) %>%
        summarise(Per = mean(per)) %>%
        ungroup() %>%
        mutate(group_size=as.character(group_size))
    # Load control data set
    load(paste0(path,funcresp,"/HigherHab_control", ".RData"))
    control <- persistence %>%
        mutate(group_size=habitat_higher_pi, Per=per) %>%
        select(code, group_size, group, Per) %>%
        filter(group %in% c(1:6))
    # Connect data sets
    tot <- bind_rows(control, all)
    # Do boxplots of the distribution within each group of consumer species
    tot %>%
        ggplot() +
        aes(x=factor(group), y=Per, color=factor(group)) +
        geom_boxplot(outlier.size=0.75) +
        facet_wrap(~factor(group_size, levels=c("control", 7:14))) +
        ggtitle("Plants randomly disturbed following functional groups") +
        xlab("Consumer group") + ylab("Persistence") +
        theme_bw() +
        theme(legend.position = "none") +
        ggsave(paste0("../Results/Figures/", funcresp, "/box_orig_random_groups.jpg"), width = 6, height = 6)
}
## IF histograms
histogram_random <- function() {
    # histogram of sp in each focus group (focus_gr) - comparing random disturbance 
    # with true value from disturbance of corresponding functional group (focus_fg)
    for (i in 1:6){
        focus_fg <- 14
        focus_gr <- i
        # create df random with persistence values for all species in the focus group
        # of consumer species, when a group of plant species with size corresponding
        # to size of original functional group was disturbed. E.g., functional group
        # 13 consists of around 35 plant sp --> 35 randomly chosen plant sp. were
        # given a higher pi, repeated 500 times. Each consumer sp get a distribution of
        # 500 persistence values.
        random <- tab %>%
            filter(group_size==focus_fg, group==focus_gr) %>%
            select(code, per)
        
        # get true value of persistence from when the corresponding functional 
        # group was disturbed
        path_orig <- "/Users/annak52/Documents/Serengeti/Results/original/"
        load(paste0(path_orig,funcresp,"/HigherHab_", focus_fg, ".RData"))
        original <- persistence %>%
            filter(group == focus_gr) %>%
            select(code, per)
        
        ggplot() +
            geom_histogram(data=random, aes(x=per), binwidth = 0.01, alpha=0.7) +
            geom_point(data=original, aes(x=per, y=0), col="red", size=2) +
            labs(x = "persistence",
                 y = "counts") +
            facet_wrap(~code)
        ggsave(paste0("../Results/Figures/", funcresp, "/random/gr_size", focus_fg, "/rnd_vs_true_pr_gr", i, ".pdf"), width = 6, height = 6)
    }
}
p_random <- function(){
    for (i in 1:length(charac)){
        focus_fg <- charac[i]
        for (j in 1:6) {
            focus_gr <- j
            # get true value of persistence from when the corresponding functional 
            # group was disturbed
            path_orig <- "/Users/annak52/Documents/Serengeti/Results/original/"
            load(paste0(path_orig,funcresp,"/HigherHab_", focus_fg, ".RData"))
            original <- persistence %>%
                filter(group == focus_gr) %>%
                select(code, per)
            # create p-value: comparing distribution of persistence from
            # the random sim. and the observed value from original sim.
            p_t <- tab %>%
                filter(group_size==focus_fg, group==focus_gr) %>%
                select(code, per) %>%
                left_join(original, by="code") %>%
                rename(per_rand=per.x, per_obs=per.y) %>%
                mutate(count = ifelse(per_rand<per_obs,1,0)) %>%
                group_by(code) %>%
                summarise(p=mean(count)) %>%
                mutate(group_size=focus_fg, cons_group=focus_gr)
            
            if (j==1){
                p_tbl <- p_t
            } else {
                p_tbl <- bind_rows(p_tbl,p_t)
            }
        }
        if (i==1){
            p_table <- p_tbl
        } else{
            p_table <- bind_rows(p_table,p_tbl)    
        }
    }
    return(p_table)
}

p_table <- p_random()
cons_per_group <- p_table %>%
    count(cons_group) %>% 
    mutate(cons_per_gr = n/8) %>% # div by 8 since cons_group is counted 8 times in p_table (8 charac)
    select(-n)


    fg <- read_csv(paste("Data/serengeti_groups.csv",sep=''), col_types="ci") %>%
        group_by(group) %>%
        count(group) %>%
        ungroup() %>%
        rename(size_fg=n, group_size=group) %>%
        mutate(size_fg=as.factor(size_fg)) %>%
        arrange(size_fg)
    
    p_table %>%
        mutate(point_five = ifelse(p>0.95|p<0.05,1,0)) %>% 
        group_by(cons_group, group_size) %>%
        summarise(prop=sum(point_five==1)) %>%
        ungroup() %>%
        left_join(cons_per_group, by="cons_group") %>% 
        mutate(frac_p=(prop/cons_per_gr)) %>% 
        mutate(group_size=factor(group_size, ordered=TRUE, levels=fg$group_size)) %>%
        ggplot() +
        aes(x=group_size, y=frac_p) +
        geom_point() +
        facet_wrap(~cons_group) +
        ggtitle("Effect of functional groups greater than chance") +
        xlab("Corresponding group label (ordered by no of disturbed sp)") + ylab("Frac of extreme p_vals, mean over cons groups")
    ggsave(paste0("../Results/Figures/", funcresp, "/random/", "p_val", ".pdf"), width = 6, height = 6)
    
        
# boxplot
p_table %>%
    #filter(cons_group %in% c(3:6)) %>%
    mutate(point_five = ifelse(p>0.95|p<0.05,1,0)) %>% 
    mutate(group_size=factor(group_size, ordered=TRUE, levels=fg$group_size)) %>%
    ggplot() +
    aes(x=factor(group_size), y=point_five) +
    geom_boxplot(aes(middle=mean(point_five))) +
    facet_wrap(~factor(cons_group)) +
    xlab("Corresponding group label (ordered by no of disturbed sp)") + ylab("Frac of extreme p_vals")

ggsave(paste0("../Results/Figures/", funcresp, "/random/", "p_val_box", ".pdf"), width = 6, height = 6)


# to find fraction of links going from consumer groups to plant groups
as_data_frame <- dplyr::as_data_frame # overwritten above, necessary to read in file below as data_frame
links <- tibble(read.table(paste("Data/serengeti_links.csv",sep=''), sep=",", header=TRUE)) %>%
    mutate_if(is.factor, as.character)
groups <- read_csv(paste("Data/serengeti_groups.csv",sep=''), col_types="ci") 
# Connect predators to their respective group
pred_links <- links %>% 
    select(code=pred_code) %>%
    left_join(., groups, by="code") %>%
    select(pred_code=code, pred_group=group)
# Connect preys to their resp. group, and combine the resulting 
# data frame with the one for predators
links_groups <- links %>% 
    select(code=prey_code) %>%
    left_join(., groups, by="code") %>%
    select(prey_code=code, prey_group=group) %>%
    bind_cols(pred_links,.)
# below summarizes how many links a group of pred got to each group of prey,
# and also the fraction of no of links going to each prey group. Make fig
compl_list <- links_groups %>%
    mutate(pred_code = 1, prey_code=1) %>%
    mutate_if(is.double, as.integer) %>%
    group_by(pred_group, prey_group) %>%
    mutate(counts = sum(prey_code)) %>%
    ungroup() %>%
    distinct(.) %>%
    select(pred_group, prey_group, counts) %>%
    slice(., -8) %>% # manually remove cannibalistic lion...
    group_by(pred_group) %>%
    mutate(tot_counts = sum(counts)) %>%
    ungroup() %>%
    arrange(., desc(pred_group)) %>%
    mutate(frac = counts/tot_counts) %>%
    select(pred_group, prey_group, frac) %>%
    rename(cons_group=pred_group, group_size=prey_group) %>% 
    filter(cons_group %in% c(3:6)) %>%
    ggplot() +
    aes(x=group_size, y=frac) +
    geom_point() +
    facet_wrap(~cons_group)
   
