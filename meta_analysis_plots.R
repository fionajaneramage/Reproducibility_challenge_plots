
# Load required R packages
library(tidyverse)
library(stringr)
library(forcats)
library(tidyr)
library(patchwork)

# Import data
meta_analysis <- read_csv("Data/Entered_summary_data/Meta-analysis.csv")

# Subset required columns
meta_analysis <- meta_analysis[,1:9] 

# Fix outcome value for Economou
meta_analysis$Outcome <- as.character(meta_analysis$Outcome) 
meta_analysis$Outcome[meta_analysis$Author == "Economou"] <- "Not specified"

# Add measure column (due to Lalu et al. being different)
meta_analysis$Measure <- ""
meta_analysis$Measure[meta_analysis$Author == "Lalu"] <- "Difference in infarct size (%)"
meta_analysis$Measure[meta_analysis$Author %in% c("Economou", "Ramage", "Gallas-Lopes")] <- "SMD"

## Effect size difference calculation

# We're opting to use an effect size difference rather than an effect size ratio as reported by Lalu et al. due to not all values being strictly positive or of the same sign
# We will calculate an estimate of the difference in the format Difference = Effect size A - Effect size B
# We will calculate standard errors from the 95% CI (formula: (upper CI - lower CI)/(2 * 1.96))
# To calculate the standard error of the difference, we use formula: Difference_SE = sqrt(A_SE^2 + B_SE^2) based on calculating variances from linear combinations
# We then re-calculate the 95% CIs for this value, i.e., upper CI = D + D_SE * 1.96, lower CI = D - D_SE * 1.96
# We are assuming statistical independence (i.e., rho = 0), but this is not technically accurate as they are MAs of the same datasets (by separate teams). This is for illustration purposes only.

meta_analysis_difference <- meta_analysis %>%
  mutate(ES_difference = `Replication effect size` - `Original effect size`, # replication minus original for change relative to original
         Original_ES_SE = (`Original UCI` - `Original LCI`)/(2*1.96),
         Replication_ES_SE = (`Replication UCI` - `Replication LCI`)/(2*1.96),
         ES_difference_SE = sqrt(Original_ES_SE^2 + Replication_ES_SE^2),
         ES_difference_LCI = ES_difference - ES_difference_SE*1.96,
         ES_difference_UCI = ES_difference + ES_difference_SE*1.96) %>%
  select(-Original_ES_SE, -Replication_ES_SE, -ES_difference_SE) %>%
  select(Author, Outcome, `Method details`, ES_difference, ES_difference_LCI, ES_difference_UCI)

# Lalu et al. is technically not an effect size like SMD etc. so will have to keep separate

# Pivot data frame to long format
meta_analysis_long <- meta_analysis %>%
  pivot_longer(
    cols = c(
      `Original effect size`, `Original LCI`, `Original UCI`,
      `Replication effect size`, `Replication LCI`, `Replication UCI`
    ),
    names_to = c("study_type", ".value"),
    names_pattern = "^(Original|Replication) (effect size|LCI|UCI)$"
  ) %>%
  # optional: clean up names and order
  rename(effect_size = `effect size`) %>%
  mutate(study_type = factor(study_type, levels = c("Original", "Replication")))

#----------------------------------------------

## Plot for raw effect sizes (coded using ELM (Edinburgh Language Model, GPT-5))
  
#----------------------------------------------  

multi_author <- "Ramage" # only author with multiple outcomes
lalu_name    <- "Lalu"
extra_author <- "Vojvodic" # show an NA panel for this author

# 1) Clean
df0 <- meta_analysis_long %>%
  rename(Method = `Method details`) %>%
  mutate(
    Method     = str_replace(Method, "(?i)specifi+ed", "specified"),
    study_type = fct_relevel(study_type, "Original", "Replication"),
    Measure    = as.character(Measure)
  )

# 2) Ramage outcome tags; everyone else blank
df0 <- df0 %>%
  mutate(
    outcome_tag = case_when(
      Author == multi_author & str_detect(Outcome, regex("morris|water maze|MWM", TRUE)) ~ "MWM",
      Author == multi_author & str_detect(Outcome, regex("novel|object|recognition|NOR", TRUE)) ~ "NOR",
      TRUE ~ ""  # single-outcome authors: no tag in x label
    ),
    outcome_order = case_when(
      Author == multi_author & outcome_tag == "MWM" ~ 1L,
      Author == multi_author & outcome_tag == "NOR" ~ 2L,
      TRUE ~ 1L
    )
  )

# 3) Number replication methods within (Author, Outcome): M1, M2, ...
rep_index <- df0 %>%
  filter(study_type == "Replication") %>%
  distinct(Author, Outcome, Method) %>%
  arrange(Author, Outcome, Method) %>%
  group_by(Author, Outcome) %>%
  mutate(MethodID = row_number()) %>%
  ungroup()

df <- df0 %>%
  left_join(rep_index, by = c("Author", "Outcome", "Method"))

# 4) Build x labels
orig_once <- df %>%
  filter(study_type == "Original") %>%
  group_by(Author, Outcome, outcome_tag, outcome_order) %>%
  slice_head(n = 1) %>%  # pick the representative Original per outcome
  ungroup() %>%
  mutate(
    x_lab = if_else(outcome_tag != "", paste0("O ", outcome_tag), "Original"),
    ord_within_outcome = 0L
  )

rep_only <- df %>%
  filter(study_type == "Replication", !is.na(MethodID)) %>%
  mutate(
    x_lab = if_else(outcome_tag != "", paste0("M", MethodID, " ", outcome_tag),
                    paste0("M", MethodID)),
    ord_within_outcome = MethodID
  )

plot_df <- bind_rows(orig_once, rep_only) %>%
  group_by(Author) %>%
  arrange(outcome_order, ord_within_outcome, .by_group = TRUE) %>%
  mutate(x_lab = factor(x_lab, levels = unique(x_lab))) %>%
  ungroup()

# 5) Split SMD (non-Lalu) vs Lalu; compute SMD y-limits
smd_df  <- plot_df %>% filter(Author != lalu_name, Measure == "SMD")
lalu_df <- plot_df %>% filter(Author == lalu_name)

smd_limits <- smd_df %>%
  summarise(ymin = min(LCI, na.rm = TRUE), ymax = max(UCI, na.rm = TRUE)) %>%
  as.list()

lalu_limits <- lalu_df %>%
  summarise(ymin = min(LCI, na.rm = TRUE), ymax = max(UCI, na.rm = TRUE)) %>%
  as.list()

# 6) Ensure Vodjovic panel appears with NA
if (!(extra_author %in% smd_df$Author)) {
  smd_df <- bind_rows(
    smd_df,
    tibble(
      Author     = extra_author,
      x_lab      = factor("(none)"),
      study_type = factor("Original", levels = levels(df$study_type)),
      effect_size= NA_real_, LCI = NA_real_, UCI = NA_real_
    )
  )
}

# NA label positions
smd_mid <- smd_df %>%
  filter(!is.na(effect_size) & !is.na(LCI) & !is.na(UCI)) %>%
  group_by(Author) %>%
  summarise(y_mid = median(c(effect_size, LCI, UCI), na.rm = TRUE), .groups = "drop")

if (!(extra_author %in% smd_mid$Author) && length(smd_limits) > 0) {
  smd_mid <- bind_rows(smd_mid, tibble(Author = extra_author, y_mid = mean(unlist(smd_limits))))
}

smd_na <- smd_df %>%
  filter(is.na(effect_size) | is.na(LCI) | is.na(UCI)) %>%
  distinct(Author, x_lab) %>%
  left_join(smd_mid, by = "Author")

lalu_mid <- lalu_df %>%
  filter(!is.na(effect_size) & !is.na(LCI) & !is.na(UCI)) %>%
  summarise(y_mid = median(c(effect_size, LCI, UCI), na.rm = TRUE), .groups = "drop")

lalu_na <- lalu_df %>%
  filter(is.na(effect_size) | is.na(LCI) | is.na(UCI)) %>%
  mutate(y_mid = if (nrow(lalu_mid) == 0) 0 else lalu_mid$y_mid)

# 7) Base aesthetics
cols <- c(Original = "#f7837b", Replication = "#2596be")
base_theme <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.spacing.x = unit(2, "mm"),
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1), 
    strip.text.x = element_text(face = "bold")
  )


# 8) Build plots (one row each, but we’ll place them side-by-side so the final is a single line)
p_smd <- ggplot(smd_df, aes(x = x_lab, y = effect_size, colour = study_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.15, alpha = 0.9, na.rm = TRUE) +
  geom_point(size = 2.6, na.rm = TRUE) +
  geom_text(
    data = smd_na,
    aes(x = x_lab, y = y_mid, label = "NA"),
    inherit.aes = FALSE, colour = "black", size = 3, fontface = "bold"
  ) +
  facet_grid(cols = vars(Author), scales = "free_x", space = "free_x", switch = "x") +
  scale_y_continuous(
    limits = c(smd_limits$ymin, smd_limits$ymax),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_color_manual(values = cols) +
  labs(x = NULL, y = "Effect size (95% CI)", colour = "Study type") +
  base_theme

p_lalu <- ggplot(lalu_df, aes(x = x_lab, y = effect_size, colour = study_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.15, alpha = 0.9, na.rm = TRUE) +
  geom_point(size = 2.6, na.rm = TRUE) +
  geom_text(
    data = lalu_na,
    aes(x = x_lab, y = y_mid, label = "NA"),
    inherit.aes = FALSE, colour = "black", size = 3, fontface = "bold"
  ) +
  facet_grid(cols = vars(Author), scales = "free_x", space = "free_x", switch = "x") +
  scale_y_continuous(
    limits = c(lalu_limits$ymin, lalu_limits$ymax),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_color_manual(values = cols) +
  labs(x = NULL, y = "Effect size (95% CI)", colour = "Study type") +
  base_theme

# 9) Put both on one line, giving the Lalu panel only as much width as its category count warrants
smd_cats_total <- smd_df %>%
  group_by(Author) %>% summarise(n = n_distinct(x_lab), .groups = "drop") %>%
  summarise(total = sum(n), .groups = "drop") %>% pull(total)
lalu_cats <- max(1L, lalu_df %>% summarise(n = n_distinct(x_lab)) %>% pull(n))

p_smd  <- p_smd  + theme(strip.text.x = element_text(face = "bold"))
p_lalu <- p_lalu + theme(strip.text.x = element_text(face = "bold"))

p_smd  <- p_smd  + theme(panel.spacing.x = unit(20, "mm"),
                         strip.text.x = element_text(face = "bold"))
p_lalu <- p_lalu + theme(panel.spacing.x = unit(20, "mm"),
                         strip.text.x = element_text(face = "bold"))

final_plot <-
  (p_smd | p_lalu) +
  plot_layout(widths = c(smd_cats_total, lalu_cats), guides = "collect") +
  plot_annotation(
    title    = "Effect sizes in original and replication studies",
    subtitle = "Per author (and per outcome, for each method)"
  ) &
  theme(
    legend.position = "bottom",
    # optional centering/bold for title/subtitle
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

final_plot

#--------------------------------------------------------------------

## Plot for effect size difference (coded using ELM (Edinburgh Language Model, GPT-5))
  
#--------------------------------------------------------------------

multi_author <- "Ramage" # only author with multiple outcomes (MWM/NOR)
lalu_name    <- "Lalu"
extra_author <- "Vojvodic" # force an NA panel if absent

# 1) Clean
df0 <- meta_analysis_difference %>%
  rename(Method = `Method details`) %>%
  mutate(
    Method = str_replace(Method, "(?i)specifi+ed", "specified")
  )

# 2) Ramage outcome tags; everyone else blank (no outcome text in x labels)
df0 <- df0 %>%
  mutate(
    outcome_tag = case_when(
      Author == multi_author & str_detect(Outcome, regex("morris|water maze|MWM", TRUE)) ~ "MWM",
      Author == multi_author & str_detect(Outcome, regex("novel|object|recognition|NOR", TRUE)) ~ "NOR",
      TRUE ~ ""  # single-outcome authors: no tag in x label
    ),
    outcome_order = case_when(
      Author == multi_author & outcome_tag == "MWM" ~ 1L,
      Author == multi_author & outcome_tag == "NOR" ~ 2L,
      TRUE ~ 1L
    )
  )

# 3) Number methods within (Author, Outcome): M1, M2, ...
rep_index <- df0 %>%
  distinct(Author, Outcome, Method) %>%
  arrange(Author, Outcome, Method) %>%
  group_by(Author, Outcome) %>%
  mutate(MethodID = row_number()) %>%
  ungroup()

df <- df0 %>%
  left_join(rep_index, by = c("Author", "Outcome", "Method"))

# 4) Build x labels (differences only -> no "Original" point)
plot_df <- df %>%
  mutate(
    x_lab = if_else(outcome_tag != "", paste0("M", MethodID, " ", outcome_tag),
                    paste0("M", MethodID)),
    ord_within_outcome = MethodID
  ) %>%
  group_by(Author) %>%
  arrange(outcome_order, ord_within_outcome, .by_group = TRUE) %>%
  mutate(x_lab = factor(x_lab, levels = unique(x_lab))) %>%
  ungroup()

# 5) Compute y-limits: shared across non-Lalu authors; Lalu separate
smd_df  <- plot_df %>% filter(Author != lalu_name)
lalu_df <- plot_df %>% filter(Author == lalu_name)

smd_limits <- smd_df %>%
  summarise(ymin = min(ES_difference_LCI, na.rm = TRUE),
            ymax = max(ES_difference_UCI, na.rm = TRUE)) %>%
  as.list()

lalu_limits <- lalu_df %>%
  summarise(ymin = min(ES_difference_LCI, na.rm = TRUE),
            ymax = max(ES_difference_UCI, na.rm = TRUE)) %>%
  as.list()

# 6) Author panel builder (uses ES_difference columns)
cols_point <- "#6719bf"
zero_line  <- ggplot2::geom_hline(yintercept = 0, colour = "red3", linewidth = 1)

author_panel_diff <- function(a, df, smd_limits, lalu_limits) {
  adf <- df %>% filter(Author == a)
  
  # If this author has no rows: show an empty panel with NA
  if (nrow(adf) == 0) {
    ylims <- c(smd_limits$ymin, smd_limits$ymax)
    return(
      ggplot() +
        zero_line +
        annotate("text", x = 1, y = mean(ylims), label = "NA", fontface = "bold") +
        scale_y_continuous(limits = ylims, expand = expansion(mult = c(0.05, 0.05))) +
        scale_x_discrete(limits = "(none)", expand = expansion(mult = c(0, 0.02))) +
        labs(x = a, y = NULL) +
        theme_minimal(base_size = 11) +
        theme(
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_text(face = "bold", margin = margin(t = 6)),
          panel.grid.minor = element_blank(),
          legend.position = "none"
        )
    )
  }
  
  # Per-author y limits (Lalu gets his own)
  ylims <- if (a == lalu_name) c(lalu_limits$ymin, lalu_limits$ymax)
  else                 c(smd_limits$ymin,  smd_limits$ymax)
  
  # NA label y position
  mid_y <- adf %>%
    filter(!is.na(ES_difference) & !is.na(ES_difference_LCI) & !is.na(ES_difference_UCI)) %>%
    summarise(y_mid = median(c(ES_difference, ES_difference_LCI, ES_difference_UCI), na.rm = TRUE)) %>%
    pull(y_mid)
  if (length(mid_y) == 0 || is.na(mid_y)) mid_y <- mean(ylims)
  
  na_x <- adf %>%
    filter(is.na(ES_difference) | is.na(ES_difference_LCI) | is.na(ES_difference_UCI)) %>%
    distinct(x_lab)
  
  ggplot(adf, aes(x = x_lab, y = ES_difference)) +
    zero_line +
    geom_errorbar(aes(ymin = ES_difference_LCI, ymax = ES_difference_UCI),
                  width = 0.15, alpha = 0.9, colour = cols_point, na.rm = TRUE) +
    geom_point(size = 2.6, colour = cols_point, na.rm = TRUE) +
    geom_text(
      data = na_x,
      aes(x = x_lab, y = mid_y, label = "NA"),
      inherit.aes = FALSE, colour = "black", size = 3, fontface = "bold"
    ) +
    scale_x_discrete(expand = expansion(mult = c(0, 0.02))) +
    scale_y_continuous(limits = ylims, expand = expansion(mult = c(0.05, 0.05))) +
    labs(x = a, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x  = element_text(angle = 55, hjust = 1, vjust = 1),
      axis.title.x = element_text(face = "bold", margin = margin(t = 6)),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
}

# 7) Authors to plot and widths
smd_authors <- smd_df %>% distinct(Author) %>% pull(Author)
if (!(extra_author %in% smd_authors)) smd_authors <- c(smd_authors, extra_author)
authors_order <- c(smd_authors, lalu_name)

panels <- list()
widths <- numeric()
gap_w  <- 0.6  # spacer width between authors

for (i in seq_along(authors_order)) {
  a <- authors_order[i]
  panels <- append(panels, list(author_panel_diff(a, plot_df, smd_limits, lalu_limits)))
  n_x <- plot_df %>% filter(Author == a) %>% summarise(n = n_distinct(x_lab)) %>% pull(n)
  if (length(n_x) == 0 || is.na(n_x) || n_x < 1) n_x <- 1
  widths <- c(widths, n_x)
  if (i < length(authors_order)) {
    panels <- append(panels, list(patchwork::plot_spacer()))
    widths <- c(widths, gap_w)
  }
}

final_plot_diff <-
  patchwork::wrap_plots(panels, nrow = 1, widths = widths) +
  plot_annotation(
    title    = "Difference in effect sizes (replication − original)",
    subtitle = "Per author; Ramage shown by outcome (MWM/NOR)"
  ) &
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

final_plot_diff

#--------------------------------------------------------------------

# Plots together
  
#--------------------------------------------------------------------  

# Raw effect size comparison
final_plot  
  
# Effect size difference 
final_plot_diff
