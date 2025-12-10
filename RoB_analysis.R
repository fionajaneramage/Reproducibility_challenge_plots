# Load required R packages
library(tidyverse)
library(stringr)
library(forcats)
library(tidyr)
library(patchwork)
library(scales)

# Set RoB domain order
RoB_order <- c("Sequence generation", "Baseline characteristics", "Allocation concealment", "Random housing", "Blinded caregivers", "Random outcome assessment", "Blinding of outcome assessment", "Incomplete outcome data", "Selective outcome reporting", "Other")

# Domain order (for abbreviated names)
RoB_domain_list <- c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10")

# Create lookup table to match domains to nicknames
RoB_code <- tibble(RoB_domain = RoB_order, RoB_nickname = RoB_domain_list)

#-------------------------------------------------

# Part 1: Calculate concordance from Vojvodic data

#-------------------------------------------------    

# Load data frame
Vojvodic_RoB <- read_csv("Data/Entered/Vojvodic_results_per_study.csv")

# Pivot wider
Vojvodic_RoB <- Vojvodic_RoB %>%
  mutate(RoB_domain = fct_relevel(RoB_domain, !!!RoB_order)) %>% # reorder levels
  pivot_wider(names_from = "Project", values_from = "Result") %>%
  arrange(Study, RoB_domain)

# Add match column and compute concordance
Vojvodic_RoB_summary <- Vojvodic_RoB %>%
  mutate(Match = ifelse(Replication == Original, "Yes", "No")) %>%
  group_by(RoB_domain) %>%
  mutate(Match_percent = 100 * mean(Match == "Yes", na.rm = TRUE)) %>%
  ungroup() %>%
  select(RoB_domain, Match_percent) %>%
  distinct()


#-------------------------------------------------    
  
# Part 2: Calculate concordance for groups where individual study results are presented  
  
#-------------------------------------------------   

# Import data frame  
RoB_individual <- read_csv("Data/Entered/RoB_individual_concordance.csv")
  
# Fix data frame for plotting
RoB_individual <- RoB_individual %>%
  mutate(Percent_concordance = as.numeric(Percent_concordance), 
         N_studies = as.numeric(N_studies), 
         RoB_domain = factor(RoB_domain, levels = RoB_order)) %>%
  group_by(Author) %>%
  mutate(
    N_lab = first(na.omit(N_studies)),
    N_lab = ifelse(is.na(N_lab), "NA", as.character(N_lab)),
    Author_lab = ifelse(N_lab == "NA", Author, paste0(Author, " (N=", N_lab, ")"))
  ) %>%
  ungroup() %>%
  left_join(RoB_code, by = "RoB_domain")

# Define empty authors
Empty_authors <- c("Economou", "Lalu", "Ramage")
Non_empty <- c("Gallas-Lopes", "Vojvodic")

# Author labels
Labels <- unique(RoB_individual$Author_lab, sort = TRUE)

base_labels <- sub(" \\(N=.*\\)$", "", Labels)

base_cols <- c(
  "Economou" = "grey70",
  "Lalu"     = "grey70",
  "Ramage"   = "grey70",
  # replace the two below with your actual author names from base_labels
  "Gallas-Lopes"  = "#1b9e77",
  "Vojvodic"  = "#d95f02"
)

cols <- setNames(unname(base_cols[base_labels]), Labels)

# Use manual fill scale with your colors
plot_individual <- ggplot(
  RoB_individual,
  aes(x = RoB_nickname, y = Percent_concordance, fill = Author_lab)
) +
  geom_col(position = position_dodge2(width = 0.8, preserve = "single"),
           width = 0.7, colour = "grey20", na.rm = TRUE) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                     expand = expansion(c(0, 0.05))) +
  scale_fill_manual(values = cols, limits = Labels, drop = FALSE, name = "Author") +
  labs(title = "SYRCLE RoB evaluation concordance between original and replication study",
         subtitle = "(Note: evaluation of individual studies, where data was available)",
         x = "Risk of bias domain",
         y = "Percent concordance (%)",
         fill = "Author") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption  = element_text(hjust = 0.5))

plot_individual

#-------------------------------------------------    
  
# Part 3: Calculate concordance for groups where summary data are provided
  
#-------------------------------------------------     

# Import data frame
RoB_combined <- read_csv("Data/Entered/RoB_combined_concordance.csv")

# Set RoB domain order
RoB_order <- c("Sequence generation", "Baseline characteristics", "Allocation concealment", "Random housing", "Blinded caregivers", "Random outcome assessment", "Blinding of outcome assessment", "Incomplete outcome data", "Selective outcome reporting", "Other")

# Domain order (for abbreviated names)
RoB_domain_list <- c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10")

# Create lookup table to match domains to nicknames
RoB_code <- tibble(RoB_domain = RoB_order, RoB_nickname = RoB_domain_list)

# Join domain numbers to data frame
RoB_combined <- RoB_combined %>%
  left_join(RoB_code, by = "RoB_domain")

# Prep data for plots
RoB_combined_plot <- RoB_combined %>%
  mutate(
    RoB_score = factor(RoB_score, levels = c("Low","Unclear","High")),
    RoB_nickname = factor(RoB_nickname, levels = paste0("D", 1:10)),
    Study_type = factor(Study_type, levels = c("Original","Replication")))

RoB_combined_plot$Percent_studies <- signif(RoB_combined_plot$Percent_studies, 3)


# Plots

# Version 1: faceted by author and by study type (original VS replication)

RoB1 <- ggplot(RoB_combined_plot, aes(x = RoB_nickname, y = Percent_studies, fill = RoB_score)) +
  geom_col(width = 0.8, color = "grey20") +
  facet_grid(Author ~ Study_type) +
  scale_y_continuous(limits = c(0, 100.1), expand = c(0, 0),
                     breaks = seq(0, 100, 20),
                     labels = label_number(accuracy = 1, suffix = "%")) +
  scale_fill_manual(values = c(Low = "#009E73", Unclear = "#E69F00", High = "#D55E00")) +
  labs(
    x = "SYRCLE RoB domain",
    y = "Percent of judgements",
    fill = "RoB score",
    title = "SYRCLE Risk of Bias distributions by domain",
    subtitle = "Original vs Replication per author"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "grey95", color = NA),
        legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

RoB1


# Version 2: faceted just by author (original and replication side by side)

gap    <- 2.0   # distance between domain groups (bigger = more space)
offset <- 0.30  # half-distance between Original and Replication within a domain
bar_w  <- 0.55  # bar width

domain_levels <- levels(RoB_combined_plot$RoB_nickname)

RoB2 <- ggplot(
  RoB_combined_plot,
  aes(
    x = (as.integer(RoB_nickname) - 1) * gap + ifelse(Study_type == "Original", -offset, offset),
    y = Percent_studies,
    fill = RoB_score
  )
) +
  geom_col(width = bar_w, color = "grey20") +     # stacked by RoB_score
  facet_wrap(~ Author, ncol = 2) +
  scale_x_continuous(
    breaks = (seq_along(domain_levels) - 1) * gap,
    labels = domain_levels,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 100.1),
    breaks = seq(0, 100, 20),
    labels = label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(values = c(Low = "#009E73", Unclear = "#E69F00", High = "#D55E00")) +
  labs(
    x = "RoB domain (nickname)",
    y = "Percent of judgements",
    fill = "RoB score",
    title = "Original vs Replication per domain (stacked) by author",
    subtitle = "Original (left) and Replication (right) with extra spacing between domains"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "bottom", 
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

RoB2


#-------------------------------------------------

# plots together
RoB1
RoB2


