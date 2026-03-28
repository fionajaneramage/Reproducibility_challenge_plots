# Load required R packages
library(tidyverse)
library(stringr)
library(forcats)
library(tidyr)
library(patchwork)
library(scales)

Risk_of_bias_per_study <- read_csv("Data/Entered/Risk_of_bias_per_study.csv") %>%
  filter(!if_all(everything(), is.na))

# Domain order (for abbreviated names)
RoB_domain_list <- c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10")

RoB_individual <- Risk_of_bias_per_study %>%
  tidyr::pivot_wider(names_from = Type, values_from = Risk_Type) %>% 
  filter(!is.na(Original)) %>%
  filter(!is.na(Replication)) %>%
  mutate(match = ifelse(Original == Replication, "yes", "no")) %>%
  group_by(Author, Domain_short) %>%
  add_count(name = "total") %>%
  group_by(Author, Domain_short, match) %>%
  add_count(name = "count") %>%
  ungroup() %>%
  mutate(perc = count / total * 100) 

# Custom legend order
legend_order <- c("Gallas-Lopes", "Vojvodic", "Ramage")

Concordance <- RoB_individual %>%
  filter(match == "yes") %>%
  select(Author, Domain_short, perc) %>%
  unique() %>%
  mutate(Author = factor(Author, levels = legend_order)) %>%
  complete(Domain_short, Author) %>%
  rename(Percent_concordance = perc) %>%
  mutate(Domain_short = factor(Domain_short, levels = RoB_domain_list)) 
   
cols <- c(
  # replace the two below with your actual author names from base_labels
  "Gallas-Lopes"  =  viridis::viridis(4)[1],
  "Vojvodic"  =   viridis::viridis(4)[2],
  "Ramage" =  viridis::viridis(4)[3]
)

plot_individual <- ggplot(
  Concordance,
  aes(x = Domain_short, y = Percent_concordance, fill = Author)
) +
  geom_col(
    position = position_dodge2(width = 0.95, preserve = "single"),
    width = 0.95,
    colour = "white",
    linewidth = 0.1,
    na.rm = TRUE
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_fill_manual(
    values = cols,
    limits = legend_order,
    drop = FALSE,
    name = "Author"
  ) +
  labs(
    title = "SYRCLE RoB evaluation concordance",
    subtitle = "Comparison between original systematic reviews and individual replications",
    x = "Risk of bias domain",
    y = "Percent concordance (%)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),  # cleaner look
    
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    
    plot.margin = margin(10, 15, 10, 10)
  )

plot_individual

ggsave(
  filename = "plots/Figure3A.png",
  plot = plot_individual,
  width = 12,       # square
  height = 12,      # square
  units = "in",
  dpi = 600
)

## Plot combined concordance data --------------------------------------------
Overall_summary <- RoB_individual %>%
  filter(match == "yes") %>%
  select(Author, Domain_short, count, total) %>%
  unique() %>%
  group_by(Domain_short) %>%
  mutate(overall_total = 21 + 20 + 19) %>%
  mutate(overall_agree = sum(count)) %>%
  mutate(overall_concordance = overall_agree/overall_total*100) %>%
  select(-c(Author, count, total)) %>%
  unique() %>%
  mutate(Domain_short = factor(Domain_short, levels = RoB_domain_list)) 

# Use manual fill scale with your colors
plot_avgs <- ggplot(
  Overall_summary,
  aes(x = Domain_short, y = overall_concordance, fill = Domain_short)
) +
  geom_col(
    width = 0.7,
    colour = "white",
    linewidth = 0.3,
    alpha = 0.9,
    na.rm = TRUE
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_discrete(
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_fill_manual(values = viridis::viridis(10), name = "Domain") +
  labs(
    title = "SYRCLE RoB evaluation concordance",
    subtitle = "Overall agreement between original systematic reviews and replications, per domain",
    x = "Risk of bias domain",
    y = "Overall concordance (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    legend.position = "none",  
    
    plot.margin = margin(10, 15, 10, 10)
  )

plot_avgs

ggsave(
  filename = "plots/Figure3B.png",
  plot = plot_avgs,
  width = 12,       # square
  height = 12,      # square
  units = "in",
  dpi = 600
)

## Plot risk of bias classification distributions  --------------------------------------------
RoB_percentages <- Risk_of_bias_per_study %>%
  group_by(Author, Type, Domain_short) %>%
  filter(Study %in% RoB_individual$Study) %>%
  add_count(name = "total") %>%
  group_by(Author, Type, Domain_short, Risk_Type) %>%
  add_count(name = "count") %>%
  mutate(perc = count/total*100) %>%
  ungroup() %>%
  select(Author, Domain_short, Type, perc, Risk_Type) %>%
  distinct() %>%
  mutate(Domain_short = factor(Domain_short, levels = RoB_domain_list)) %>%
  mutate(Risk_Type = factor(Risk_Type, levels = c("Low", "Unclear", "High")))

RoB1 <- ggplot(RoB_percentages, aes(x = Domain_short, y = perc, fill = Risk_Type)) +
  geom_col(width = 0.8, color = "white", linewidth = 0.3) +
  facet_grid(Type ~ Author) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.02)),
    breaks = seq(0, 100, 20),
    labels = label_number(accuracy = 1, suffix = "%")
  ) +
  scale_fill_manual(
    values = c(Low = "#009E73", Unclear = "#E69F00", High = "#D55E00"),
    name = "RoB score"
  ) +
  labs(
    x = "SYRCLE RoB domain",
    y = "Percent of judgements",
    title = "SYRCLE Risk of Bias distributions by domain"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(10, 15, 10, 10)
  )

RoB1

ggsave(
  filename = "plots/Figure2.png",
  plot = RoB1,
  width = 12,       # square
  height = 12,      # square
  units = "in",
  dpi = 600
)

