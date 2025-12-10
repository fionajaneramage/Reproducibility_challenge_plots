# load required R packages

library(tidyverse)
library(stringr)
library(forcats)
library(tidyr)
library(patchwork)


# Read in data frame and fix variables   

screening <- read_csv("Data/Entered/Screening.csv")
screening <- screening[,1:5]

screening <- screening %>% 
  rename(N_original = `N studies in original`, 
         N_replication = `N studies in replication`, 
         Percent_in_original = `% replication studies in original`, 
         Percent_in_replication = `% original studies in replication`)

#----------------------------------------------------

### Part 1: Number of studies
  
#----------------------------------------------------  

# Fix dataframe
screening_long <- screening %>%
  pivot_longer(
    c(N_original, N_replication), 
    names_to = "Set", 
    values_to = "N"
  ) %>%
  mutate(Set = recode(Set,
                     N_original   = "Original review",
                     N_replication = "Replication study"), 
         Set = factor(Set, levels = c("Original review", "Replication study"))
  ) %>%
  mutate(na_y = 1)

na_rows <- screening_long %>% filter(is.na(N)) %>%   mutate(na_y = 1)

xlabels <- screening_long %>%
  distinct(Author, Set) %>%
  mutate(y = -3) 

#---

# Plot: first option (with facet wrap)
  
#---
  
plot <- ggplot(screening_long, aes(x = Set, y = N)) +
  geom_line(data = screening_long %>% filter(!is.na(N)),
            aes(group = Author),
            color = "black",
            linetype = "dotted") +
  geom_point(aes(color = Set), size = 3, na.rm = TRUE) +
  geom_text(aes(label = paste0("N=", N)), vjust = 3, size = 3, na.rm = TRUE) +
  geom_text(data = na_rows, aes(x = Set, y = na_y, label = "NA"),
            color = "black", size = 3, vjust = -0.3) +
  geom_text(data = xlabels, aes(x = Set, y = y, label = Set),
            inherit.aes = FALSE, size = 3) +
  facet_wrap(~ Author, ncol = 3) +
  scale_y_continuous(limits = c(-5, 28), expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "Number of studies") +
  theme_minimal(base_size = 13) +
  ggtitle("Number of studies included in each review") +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_blank(),      # hide default x-axis labels
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

plot

#---

# Plot second option (no facet wrap)
  
#---  

# Ensure Author is a factor
screening_long <- screening_long %>%
  mutate(Author = factor(Author),
         Set = factor(Set, levels = c("Original review", "Replication study")),
         x_num = as.numeric(Author) + ifelse(Set == "Original review", -0.15, 0.15))

# NA rows with x_num
na_rows <- screening_long %>%
  filter(is.na(N)) %>%
  mutate(x_num = as.numeric(Author) + ifelse(Set == "Original review", -0.15, 0.15),
         na_y = 1)  # y position for NA label

# Manual x-axis labels
xlabels <- screening_long %>%
  distinct(Author) %>%
  mutate(x_num = as.numeric(Author),
         y = -2)  # y position for axis label

plot2 <- ggplot(screening_long, aes(x = x_num, y = N, color = Set)) +
  geom_line(aes(group = Author), color = "black", linetype = "dotted") +
  geom_point(size = 3) +
  geom_text(aes(label = paste0("N=", N)),
            vjust = 3, size = 3, color = "black") +
  geom_text(data = na_rows %>% mutate(x_num = as.numeric(Author)),
            aes(x = x_num, y = 3, label = "NA"),
            color = "black", size = 3, vjust = -0.5, fontface = "plain") +
  geom_text(data = xlabels %>% mutate(x_num = as.numeric(Author)),
            aes(x = x_num, y = -2, label = Author),
            inherit.aes = FALSE, size = 3, fontface = "bold") +
  scale_y_continuous(limits = c(-5, 28), expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "Number of studies", color = "Study type") +
  ggtitle("Number of studies included in each review") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

plot2

#----------------------------------------------------

### Part 2: Overlap of studies

#----------------------------------------------------    

# Fix dataframe
screening_pct_long <- screening %>%
  select(Author, Percent_in_replication, Percent_in_original) %>% 
  pivot_longer(
    cols = c(Percent_in_replication, Percent_in_original),
    names_to = "type",
    values_to = "percent"
  ) %>%
  mutate(
    type = recode(type,
                  Percent_in_replication = "% original in replication",
                  Percent_in_original = "% replication in original"),
    type = factor(type, levels = c("% replication in original", "% original in replication"))
  )

xlabels_pct <- screening_pct_long %>%
  distinct(Author, type) %>%
  mutate(y = -3)

na_rows_pct <- screening_pct_long %>% filter(is.na(percent)) %>%   mutate(na_y = 1)

---
  
# Plot option 1 (facet wrap)
  
---
  
plot_pct <- ggplot(screening_pct_long, aes(x = type, y = percent)) +
  geom_point(aes(color = type), size = 3, na.rm = TRUE) +
  geom_text(aes(label = paste0(percent, "%")), vjust = 3, size = 3, na.rm = TRUE) +
  geom_text(data = na_rows_pct, aes(x = type, y = 1, label = "NA"),
            color = "black", size = 3, vjust = -1) +
  geom_text(data = xlabels_pct, aes(x = type, y = y, label = type),
            inherit.aes = FALSE, size = 3) +
  facet_wrap(~ Author, ncol = 3) +
  scale_y_continuous(limits = c(-5, 100)) +
  theme_minimal(base_size = 13) +
  labs(x = NULL, y = "% Studies") +
  ggtitle("Overlap in included studies between both reviews") +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_blank(),  # hide original x-axis labels
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

plot_pct

#---

# Plot option 2 (no facet wrap): 
  
#---  

screening_pct_long <- screening %>%
  select(Author, Percent_in_replication, Percent_in_original) %>% 
  pivot_longer(
    cols = c(Percent_in_replication, Percent_in_original),
    names_to = "type",
    values_to = "percent"
  ) %>%
  mutate(
    Author = factor(Author),  # convert to factor first
    type = recode(type,
                  Percent_in_replication = "% original in replication",
                  Percent_in_original = "% replication in original"),
    type = factor(type, levels = c("% replication in original","% original in replication")),
    x_num = as.numeric(Author) + ifelse(type == "% replication in original", -0.15, 0.15)
  )

# NA rows
na_rows_pct <- screening_pct_long %>%
  filter(is.na(percent)) %>%
  group_by(Author) %>%
  summarise(
    x_num = mean(as.numeric(Author) + c(-0.15, 0.15)),  # midpoint between Original & Replication
    na_y = 1
  )

# Manual x-axis labels
xlabels_pct <- screening_pct_long %>%
  distinct(Author) %>%
  mutate(x_num = as.numeric(Author),
         y = 5)

# Plot
plot_pct2 <- ggplot(screening_pct_long, aes(x = x_num, y = percent, color = type)) +
  geom_segment(aes(x = x_num, xend = x_num, y = 0, yend = percent), color = "gray70") +
  geom_point(aes(x = x_num, y = percent, color = type), size = 3) +
  geom_text(aes(label = paste0(percent, "%")),
            vjust = 3,
            size = 3,
            na.rm = TRUE,
            color = "black") +
  geom_text(data = na_rows_pct,
            aes(x = x_num, y = na_y, label = "NA"),
            color = "black",
            size = 3,
            vjust = -8) +
  geom_text(data = xlabels_pct,
            aes(x = x_num, y = -3, label = Author),
            inherit.aes = FALSE,
            size = 3,
            fontface = "bold") +
  scale_y_continuous(limits = c(-5, 100), expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "% Studies", color = "Analysis type") +
  ggtitle("Overlap in included studies between both reviews") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

plot_pct2

#----------------------------------------------------
  
## All plots together

#----------------------------------------------------    

# Number of studies
plot # facet wrap
plot_pct # no facet wrap

# Overlap of studies
plot2 # facet wrap
plot_pct2 # no facet wrap
