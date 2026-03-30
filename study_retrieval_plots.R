## Load packages ------
library(tidyverse)

## Create table of studies included 
included <- tribble(
  ~Author,        ~N_replication, ~N_original, ~pct_orig_in_rep, ~pct_rep_in_orig,
  "Gallas-Lopes", 28,             22,          95.4,             75.0,
  "Lalu",         11,             11,          90.9,             90.9,
  "Ramage",       25,             23,          91.3,             84.0,
)

## Calculate percentages included and format for plot
included <- included %>%
  mutate(
    Overlap       = round((N_original * pct_orig_in_rep/100 + N_replication * pct_rep_in_orig/100)/2),
    Original_only = N_original - Overlap,
    Replication_only = N_replication - Overlap
  )

print(included)

# Prepare long format for stacked bar
included_plot <- included %>%
  select(Author, Original_only, Overlap, Replication_only) %>%
  pivot_longer(cols = c(Original_only, Overlap, Replication_only),
               names_to = "Component",
               values_to = "Count") %>%
  mutate(
    Component = factor(Component, levels = c("Replication_only", "Original_only", "Overlap"))
  ) %>% # compute position for text labels
  mutate(
    Component2 = factor(Component, levels = c("Overlap", "Original_only", "Replication_only"))
  ) %>%
  group_by(Author) %>%
  arrange(Component2) %>%  # ensures correct stacking order
  mutate(ypos = cumsum(Count) - Count/2) %>%
  ungroup()


library(viridis)
stack_cols <- c(
  "Original_only"    = viridis::viridis(4)[3],  # soft greenish
  "Overlap"          = viridis::viridis(4)[2],  # strong blue — pops
  "Replication_only" = viridis::viridis(4)[1]   # muted purple/orange-ish
)

# Plot
p <- ggplot(included_plot, aes(x = Author, y = Count, fill = Component)) +
  geom_col(
    width = 0.7,
    color = "white",
    linewidth = 0.3,
    alpha = 0.9
  ) +
  geom_text(
    aes(y = ypos, label = Count),
    color = "white",
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = stack_cols,
    breaks = c("Overlap", "Original_only", "Replication_only"),
    labels = c("Overlap", "Original only", "Replication only"),
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = "Number of studies in Original SRs and Replications",
    x = "",
    y = "Number of studies",
    fill = ""
  ) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    
    legend.position = "top",
    legend.text = element_text(size = 12),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    plot.margin = margin(10, 15, 10, 10)
  )



ggsave(
  filename = "plots/Figure1.png",
  plot = p,
  width = 12,       # square
  height = 12,      # square
  units = "in",
  dpi = 600
)
