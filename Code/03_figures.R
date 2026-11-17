## 03 Figures for the manuscript
## Reads the simulation results from 02_simulate.jl and the network files
## from 01_prepare_network.R. Run with Code/ as working directory.

## Settings
sim_path    <- "../Results/simulations/"
data_path   <- "Data/processed/"
fig_path    <- "../Results/figures/"
p_limit     <- 0.0005   # extreme p-values: p < p_limit or p > 1 - p_limit (Fig 2)
p_limit_si  <- 0.025    # more relaxed limit (Fig S3)

grouping_order <- c("habitat", "growth form", "group")
grouping_titles <- c("habitat" = "Habitat", "growth form" = "Growth form", "group" = "Group membership")

source("functions.R")
require(cowplot)


## ---------------------------------------------------------------
## Read data
## ---------------------------------------------------------------

species <- read_csv(paste0(data_path, "species.csv"), col_types = "icccicc")
memberships <- read_csv(paste0(data_path, "memberships.csv"), col_types = "ccc")
links <- read_csv("Data/serengeti_links.csv", col_types = "cc")

scenario_info <- memberships %>%
    count(scenario, grouping, name = "n_plants") %>%
    mutate(label = label_scenario(scenario)) %>%
    mutate(grouping = factor(grouping, levels = grouping_order)) %>%
    arrange(grouping, n_plants) %>%
    mutate(label = fct_inorder(label))

consumer_type <- species %>%
    filter(type != "plant") %>%
    select(code, type)

targeted <- read_csv(paste0(sim_path, "targeted.csv"), col_types = "cccd")

control <- targeted %>%
    filter(scenario == "control") %>%
    select(code, per_control = persistence)

targeted <- targeted %>%
    filter(scenario != "control") %>%
    inner_join(consumer_type, by = "code") %>%
    select(scenario, code, type, per_target = persistence)

random <- read_csv(paste0(sim_path, "random.csv"), col_types = cols(.default = "d", scenario = "c", grouping = "c")) %>%
    select(-grouping) %>%
    pivot_longer(-c(scenario, rep), names_to = "code", values_to = "per_random")


## ---------------------------------------------------------------
## Functions
## ---------------------------------------------------------------

# Compares targeted and random disturbance for each consumer and scenario.
# p is the fraction of random replicates where the consumer's persistence is
# lower than after the targeted disturbance. p close to 0: the targeted
# disturbance is more harmful than a random one; p close to 1: less harmful.
# Input: targeted, random (tibbles above)
# Output: tibble (scenario, code, type, p)
compare_random <- function(targeted, random) {
    p_values <- random %>%
        inner_join(targeted, by = c("scenario", "code")) %>%
        group_by(scenario, code, type) %>%
        summarise(p = mean(per_random < per_target), .groups = "drop")
    return(p_values)
}

# Figure 1: change in consumer persistence after targeted and random disturbance.
# Input: consumer types to include ("herbivore", "carnivore")
# Output: ggplot
plot_impact <- function(types) {
    random_mean <- random %>%
        group_by(scenario, code) %>%
        summarise(per_random = mean(per_random), .groups = "drop")
    impact <- targeted %>%
        filter(type %in% types) %>%
        left_join(random_mean, by = c("scenario", "code")) %>%
        left_join(control, by = "code") %>%
        mutate(targeted = per_target - per_control) %>%
        mutate(random = per_random - per_control) %>%
        select(scenario, code, targeted, random) %>%
        pivot_longer(c(targeted, random), names_to = "disturbance", values_to = "impact") %>%
        mutate(disturbance = factor(disturbance, levels = c("targeted", "random"))) %>%
        left_join(scenario_info, by = "scenario")
    p <- ggplot(impact, aes(x = label, y = impact, colour = n_plants)) +
        geom_hline(yintercept = 0, lty = "dashed") +
        geom_boxplot(aes(fill = disturbance), position = position_dodge(width = 0.8), width = 0.7, outlier.size = 0.8) +
        scale_fill_manual(name = "Disturbance", values = c("targeted" = "grey60", "random" = "white")) +
        scale_colour_viridis_c(name = "Number of\nplant species", end = 0.9) +
        xlab("Disturbed plant group") +
        ylab("Change in consumer persistence") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 11))
    return(p)
}

# Figure 2: fraction of consumers whose response to targeted disturbance
# differs from random disturbance.
# Input: consumer types to include, limit for extreme p-values,
#        show_all = FALSE drops scenarios without any extreme p-values
# Output: ggplot
plot_extremes <- function(types, limit, show_all = FALSE) {
    extremes <- compare_random(targeted, random) %>%
        filter(type %in% types) %>%
        group_by(scenario) %>%
        summarise(more = mean(p < limit), less = mean(p > 1 - limit), .groups = "drop") %>%
        pivot_longer(c(more, less), names_to = "direction", values_to = "fraction") %>%
        left_join(scenario_info, by = "scenario") %>%
        group_by(scenario) %>%
        filter(show_all | sum(fraction) > 0) %>%
        ungroup() %>%
        mutate(direction = factor(direction, levels = c("less", "more"))) %>%
        mutate(label = paste0(label, " (", n_plants, ")")) %>%
        arrange(n_plants) %>%
        mutate(label = fct_inorder(label))
    p <- ggplot(extremes, aes(x = label, y = fraction, fill = direction)) +
        geom_col(colour = "black") +
        scale_fill_manual(name = "Targeted disturbance",
                          values = c("more" = "#545b94", "less" = "#33a39b"),
                          labels = c("more" = paste0("more harmful than random (p < ", 100 * limit, "%)"),
                                     "less" = paste0("less harmful than random (p > ", 100 * (1 - limit), "%)"))) +
        xlab("Disturbed plant group (number of plant species), by increasing size") +
        ylab("Fraction of consumer species") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 11),
              legend.position = "bottom", legend.direction = "vertical")
    return(p)
}

# Herbivore-plant links with each plant's scenarios. A plant in two habitats
# counts in both.
# Output: tibble (herbivore, plant, scenario, grouping)
herbivore_links <- function() {
    herbivores <- species %>%
        filter(type == "herbivore") %>%
        pull(code)
    hl <- links %>%
        filter(pred_code %in% herbivores) %>%
        select(herbivore = pred_code, plant = prey_code) %>%
        inner_join(memberships, by = c("plant" = "code"), relationship = "many-to-many")
    return(hl)
}

# Figure 3: number of herbivores feeding on each plant, per plant group.
# Output: ggplot with one panel per grouping
plot_links_per_plant <- function() {
    plants <- species %>%
        filter(type == "plant")
    herbivores <- species %>%
        filter(type == "herbivore") %>%
        pull(code)
    n_links <- links %>%
        filter(pred_code %in% herbivores) %>%
        count(prey_code, name = "n_links") %>%
        right_join(select(plants, code), by = c("prey_code" = "code")) %>%
        mutate(n_links = replace_na(n_links, 0))
    network_mean <- mean(n_links$n_links)
    per_plant <- memberships %>%
        left_join(n_links, by = c("code" = "prey_code")) %>%
        left_join(scenario_info, by = c("scenario", "grouping"))
    panels <- map(grouping_order, function(g) {
        per_plant %>%
            filter(grouping == g) %>%
            ggplot(aes(x = label, y = n_links)) +
            geom_boxplot() +
            stat_summary(fun = mean, geom = "point", shape = 5, size = 3, colour = "darkblue") +
            geom_hline(yintercept = network_mean, lty = "dashed") +
            xlab("") +
            ylab("Herbivores per plant species") +
            ggtitle(grouping_titles[g]) +
            theme_bw()
    })
    p <- plot_grid(plotlist = panels, ncol = 1, labels = "AUTO")
    return(p)
}

# Figure 4: fraction of each herbivore's feeding links going to each plant group.
# Output: ggplot with one panel per grouping
plot_diet_fractions <- function() {
    fractions <- herbivore_links() %>%
        count(grouping, herbivore, scenario, name = "n_links") %>%
        group_by(grouping, herbivore) %>%
        mutate(fraction = n_links / sum(n_links)) %>%
        ungroup() %>%
        left_join(scenario_info, by = c("scenario", "grouping")) %>%
        mutate(legend = paste0(label, ", n=", n_plants))
    panels <- map(grouping_order, function(g) {
        d <- fractions %>%
            filter(grouping == g) %>%
            arrange(n_plants) %>%
            mutate(legend = fct_inorder(legend))
        ggplot(d, aes(x = herbivore, y = fraction, colour = legend)) +
            geom_point(size = 2, alpha = 0.8) +
            scale_colour_manual(name = grouping_titles[g],
                                values = c("#E69F00", "#56B4E9", "#009E73", "#000000",
                                           "#000099", "#996633", "#CC79A7", "#660066")) +
            xlab("") +
            ylab("Fraction of feeding links") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8))
    })
    p <- plot_grid(plotlist = panels, ncol = 1, labels = "AUTO", align = "v")
    return(p)
}


## ---------------------------------------------------------------
## Make figures
## ---------------------------------------------------------------

dir.create(fig_path, showWarnings = FALSE, recursive = TRUE)
all_consumers <- c("herbivore", "carnivore")

fig1 <- plot_impact(all_consumers)
ggsave(paste0(fig_path, "fig1_impact.pdf"), fig1, width = 10, height = 5)

fig2 <- plot_extremes(all_consumers, p_limit)
ggsave(paste0(fig_path, "fig2_extremes.pdf"), fig2, width = 7, height = 6)

fig3 <- plot_links_per_plant()
ggsave(paste0(fig_path, "fig3_links_per_plant.pdf"), fig3, width = 8, height = 10)

fig4 <- plot_diet_fractions()
ggsave(paste0(fig_path, "fig4_diet_fractions.pdf"), fig4, width = 8, height = 10)

figS1 <- plot_impact("herbivore")
ggsave(paste0(fig_path, "figS1_impact_herbivores.pdf"), figS1, width = 10, height = 5)

figS2 <- plot_extremes("herbivore", p_limit)
ggsave(paste0(fig_path, "figS2_extremes_herbivores.pdf"), figS2, width = 7, height = 6)

figS3 <- plot_extremes(all_consumers, p_limit_si)
ggsave(paste0(fig_path, "figS3_extremes_relaxed.pdf"), figS3, width = 7, height = 6)
