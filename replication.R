# Load required R packages
library(tidyverse)
library(stringr)
library(forcats)
library(tidyr)
library(patchwork)

# Import data
meta_analysis <- read_csv("Data/Entered/Meta-analysis.csv")

replication_analysis <- df %>% 
    mutate(
      # 1. CI widths
      width_orig = Orig_UCI - Orig_LCI,
      width_rep  = Rep_UCI - Rep_LCI,
      
      # 2. Relative differences
      est_diff   = abs(Rep_ES - Orig_ES) / ((Rep_ES + Orig_ES)/2),
      width_diff = abs(width_rep - width_orig) / ((width_rep = width_orig)/2),
      
      # ---- CRITERION 1: within 10% ----
      est_within_10pct   = est_diff <= 0.10,
      width_within_10pct = width_diff <= 0.10,
      
      # ---- CRITERION 2: same direction ----
      same_direction = sign(Orig_ES) == sign(Rep_ES),
      
      # ---- CRITERION 3: statistical significance ----
      sig_orig = !(Orig_LCI <= 0 & Orig_UCI >= 0),
      sig_rep  = !(Rep_LCI  <= 0 & Rep_UCI  >= 0),
      
      diff_significance = sig_orig != sig_rep
    )

  