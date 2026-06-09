# Load required R packages
library(dplyr)
library(readr)

# Import data
meta_analysis <- read_csv("Data/Entered/Meta-analysis.csv")

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

replication_analysis <- meta_analysis %>% 
    mutate(
      # 1. CI widths
      width_orig = Orig_UCI - Orig_LCI,
      width_rep  = Rep_UCI - Rep_LCI,
      
      # 2. Percentage change (from original)
      est_diff   = (Rep_ES - Orig_ES) / Orig_ES,
      width_diff = (width_rep - width_orig) / width_orig,
      
      # ---- CRITERION 1: within 10% ----
      est_within_10pct   = abs(est_diff) <= 0.10,
      width_within_10pct = abs(width_diff) <= 0.10,
      
      # ---- CRITERION 2: same direction ----
      same_direction = sign(Orig_ES) == sign(Rep_ES),
      
      # ---- CRITERION 3: statistical significance ----
      sig_orig = !(Orig_LCI <= 0 & Orig_UCI >= 0),
      sig_rep  = !(Rep_LCI  <= 0 & Rep_UCI  >= 0),
      
      diff_significance = sig_orig != sig_rep
    )

  