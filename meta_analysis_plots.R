
# Load required R packages
library(tidyverse)
library(stringr)
library(forcats)
library(tidyr)
library(patchwork)

# Import data
meta_analysis <- read_csv("Data/Entered/Meta-analysis.csv")

# Subset required columns
meta_analysis <- meta_analysis[,1:9]

meta_analysis <- meta_analysis %>%
  filter(!is.na(Outcome)) %>%
  rename(
    Orig_ES  = `Original effect size`,
    Orig_LCI = `Original LCI`,
    Orig_UCI = `Original UCI`,
    Rep_ES   = `Replication effect size`,
    Rep_LCI  = `Replication LCI`,
    Rep_UCI  = `Replication UCI`,
    Method = `Method details`
  )

df_long <- meta_analysis %>%
  pivot_longer(
    cols = c(Orig_ES, Orig_LCI, Orig_UCI, Rep_ES, Rep_LCI, Rep_UCI),
    names_to = c("Type", ".value"),
    names_pattern = "(Orig|Rep)_(ES|LCI|UCI)"
  ) %>%
  mutate(Type = ifelse(Type == "Orig", "Original", "Replication")) %>%
  group_by(Author, Type, Outcome) %>%
  mutate(Method_label = paste0("Method ", row_number())) %>%
  mutate(Method_label = ifelse(Type == "Original", "Original", Method_label)) %>%
  ungroup() %>%
  mutate(Method_label = factor(Method_label,
                               levels = c("Original",
                                          sort(unique(Method_label[Method_label != "Original"])))))


p_infarct2  <- df_long %>%
  filter(Outcome == "Infarct volume") %>%
  ggplot(aes(x = ES, y = Method_label, color = Type)) +
  geom_rect(data = . %>% filter(Type == "Original"),
            aes(xmin = LCI, xmax = UCI, ymin = -Inf, ymax = Inf),
            fill="gainsboro", alpha=0.3, color = NA) +
  geom_errorbarh(aes(xmin = LCI, xmax = UCI), height=.3) +
  geom_point(size=3) +
  labs(x = "SMD (95% CIs)", y = "") +
  theme_bw() +
  coord_flip()+
  theme(legend.position = "none") +
  scale_color_manual(
    values = c("Original" = viridis::viridis(4)[3],
               "Replication" = viridis::viridis(4)[2])) +
  expand_limits(x = 0) +
  ggtitle("Economou\nInfarct volume")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  labs(tag = "C") +
  theme(text = element_text(size = 15), axis.text = element_text(size = 17), axis.title = element_text(size = 17))


p_dep  <- df_long %>%
  filter(Outcome == "Depressive-like behaviours") %>%
  ggplot(aes(x = ES, y = Method_label, color = Type)) +
  geom_rect(data = . %>% filter(Type == "Original"),
            aes(xmin = LCI, xmax = UCI, ymin = -Inf, ymax = Inf),
            fill="gainsboro", alpha=0.3, color = NA) +
  geom_errorbarh(aes(xmin = LCI, xmax = UCI), height=.3) +
  geom_point(size=3) +
  labs(x = "SMD (95% CIs)", y = "") +
  theme_bw() +
  coord_flip()+
  theme(legend.position = "none") +
  scale_color_manual(
    values = c("Original" = viridis::viridis(4)[3],
               "Replication" = viridis::viridis(4)[2])) +
  expand_limits(x = 0) +
  ggtitle("Gallas-Lopes\nDepressive-like behaviour") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  labs(tag = "A") + 
  theme(text = element_text(size = 15), axis.text = element_text(size = 17), axis.title = element_text(size = 17))


p_infarct1 <- df_long %>%
  filter(Outcome == "% reduction in infarct size") %>%
  ggplot(aes(x = ES, y = Method_label, color = Type)) +
  geom_rect(data = . %>% filter(Type == "Original"),
            aes(xmin = LCI, xmax = UCI, ymin = -Inf, ymax = Inf),
            fill="gainsboro", alpha=0.3, color = NA) +
  geom_errorbarh(aes(xmin = LCI, xmax = UCI), height=.3) +
  geom_point(size=3) +
  labs(x = "NMD (95% CIs)", y = "") +
  theme_bw() +
  coord_flip()+
  theme(legend.position = "none") +
  scale_color_manual(
    values = c("Original" = viridis::viridis(4)[3],
               "Replication" = viridis::viridis(4)[2])) +
  expand_limits(x = 0) +
  ggtitle("Tangamornsuksan\nInfarct volume")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  labs(tag = "B") +
  theme(text = element_text(size = 15), axis.text = element_text(size = 17), axis.title = element_text(size = 17))

p_mwm <- df_long %>%
  filter(Outcome == "MWM") %>%
  ggplot(aes(x = ES, y = Method_label, color = Type)) +
  geom_rect(data = . %>% filter(Type == "Original"),
            aes(xmin = LCI, xmax = UCI, ymin = -Inf, ymax = Inf),
            fill="gainsboro", alpha=0.3, color = NA) +
  geom_errorbarh(aes(xmin = LCI, xmax = UCI), height=.3) +
  geom_point(size=3) +
  labs(x = "SMD (95% CIs)", y = "") +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "none") +
  scale_color_manual(
    values = c("Original" = viridis::viridis(4)[3],
               "Replication" = viridis::viridis(4)[2])) +
  ggtitle("Ramage\nMorris water maze") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  labs(tag = "E") +
  theme(text = element_text(size = 15), axis.text = element_text(size = 17), axis.title = element_text(size = 17)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1))


p_nor <- df_long %>%
  filter(Outcome == "NOR") %>%
  ggplot(aes(x = ES, y = Method_label, color = Type)) +
  geom_rect(data = . %>% filter(Type == "Original"),
            aes(xmin = LCI, xmax = UCI, ymin = -Inf, ymax = Inf),
            fill="gainsboro", alpha=0.3, color = NA) +
  geom_errorbarh(aes(xmin = LCI, xmax = UCI), height=.3) +
  geom_point(size=3) +
  labs(x = "SMD (95% CIs)", y = "") +
  theme_bw() +
  expand_limits(x = 0) +
  coord_flip() +
  theme(legend.position = "none") +
  scale_color_manual(
    values = c("Original" = viridis::viridis(4)[3],
               "Replication" = viridis::viridis(4)[2])) +
  ggtitle("Ramage\nNovel object recognition") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  labs(tag = "D") +
  theme(text = element_text(size = 15), axis.text = element_text(size = 17), axis.title = element_text(size = 17))+
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1))

combined_plot <- 
  (p_dep + p_infarct1) /
  (p_infarct2 + p_nor) /
  p_mwm +
  plot_layout(heights = c(1, 1, 1.2)) +
  plot_annotation(
    title = "Effect sizes in Original SRs and Replications",
    theme = theme(plot.title = element_text(size = 20, face = "bold"))
  )

combined_plot

ggsave(
  filename = "plots/Figure4.png",
  plot = combined_plot,
  width = 12,       # square
  height = 12,      # square
  units = "in",
  dpi = 600
)

