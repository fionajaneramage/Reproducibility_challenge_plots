library(dplyr)
library(tidyr)
library(robvis)
library(ggplot2)
library(stringr)

# import files 
recon <- read.csv("data/tidy/reconciled_data_24-09-26.csv")
agree <- read.csv("data/tidy/SVMAA_agreements.csv")

# Clean -------------------------------------------------------------------
clean_rob_cols <- function(x){
  colnames(x) <- colnames(x) %>% 
    {gsub("RoB", "", .)} %>% 
    {gsub("\\(s\\)","", .)}
  x
} 

# reconciled data RoB ----
data <- recon %>% 
  # fix rob col naming
  clean_rob_cols() %>% 
  # consistent column names
  janitor::clean_names() %>% 
  # trim trailing whitespace
  mutate(across(where(is.character), ~ trimws(., "both"))) %>%
  # convert blank to NA
  mutate(across(where(is.character), ~na_if(., ""))) 

rob_cols <- data %>% 
  select(starts_with("item_")) %>% 
  colnames() %>% 
  sort()

rob_data <- data %>% 
  select(
    c(study_id, authors, year,
      outcome_label,
      all_of(rob_cols),
    )) %>% 
  relocate(item_10_other_other_sources_of_bias_answer, .after = last_col()) %>% 
  distinct()

rob_data <- rob_data %>%
  select(authors, year, ends_with("answer")) %>%
  #mutate(across(where(is.character), ~ str_to_sentence(.))) %>%
  #mutate(across(where(is.character), ~ str_replace_all(., "Unclear", "Some concerns"))) %>%
  filter(if_all(where(is.character), ~ !str_detect(., regex("Not applicable", ignore_case = TRUE)))) 
#%>%
#mutate(Overall = "Not Applicable")  

# clean author names
clean_authors <- function(author_str, year) {
  # Replace various newline combinations with a semicolon
  author_str <- str_replace_all(author_str, "\\r\\n|\\n\\r|\\r|\\n", ";")
  # Split authors based on ";" delimiter
  authors <- str_split(author_str, ";")[[1]]
  # Trim whitespace from author names
  authors <- str_trim(authors)
  # Split the first author's name by spaces
  first_author_parts <- str_split(authors[1], " ")[[1]]
  # Get the last part of the first author's name, which is usually the last name
  last_name <- first_author_parts[length(first_author_parts)]
  # Return formatted author string with year
  return(paste(last_name, " et al. ", year, "", sep = ""))
}

rob_data$authors <- apply(rob_data, 1, function(row) {
  clean_authors(row["authors"], row["year"])
})

# Function to add index for duplicate author-year combinations
add_index <- function(df) {
  df %>%
    group_by(authors) %>%
    mutate(index = row_number(),
           authors = ifelse(index == 1, authors, paste0(authors, letters[index]))) %>%
    ungroup() %>%
    select(-index)
}

rob_data <- add_index(rob_data)

rob_data <- rob_data %>%
  select (-year)

# manually change names where author function didn't work properly
rob_data <- rob_data %>%
  mutate(authors = case_when(
    authors == "Zeng et al. 2013" ~ "Ma et al. 2013",
    authors == "Redington et al. 2011" ~ "Hahn et al. 2011", 
    authors == "Hu et al. 2016" ~ "Qi et al. 2016", 
    authors == "Long et al. 2014" ~ "Liu et al. 2014", 
    TRUE ~ authors # Keep all other names unchanged
  ))

#clean column names
rob_data <- rob_data %>%
  rename_with(~ sub("_answer$", "", .), .cols = everything()) %>%
  rename_with(~ gsub("_", " ", .), .cols = everything())

write.csv(rob_data, file = "data/tidy/robvis_clean_reconciled_data.csv", row.names = F)

png (filename = "data/plots/rob_traffic_reconciled.png", height = 500, width = 700)
rob_traffic_light(
  rob_data,
  tool = "Generic",
  overall = FALSE,
  colour = "cochrane") 
# judgement_labels = c("Low risk of bias",
#                      "Unclear risk of bias",
#                      "High risk of bias",
#                      "Critical risk of bias",
#                      "No information")) +
# theme(text = element_text(size = 25),
#       legend.text = element_text(size = 20),
#       axis.text.y = element_text(size = 20))
dev.off()

# two reviewer agreements RoB ----
data <- agree %>% 
  # fix rob col naming
  clean_rob_cols() %>% 
  # consistent column names
  janitor::clean_names() %>% 
  # trim trailing whitespace
  mutate(across(where(is.character), ~ trimws(., "both"))) %>%
  # convert blank to NA
  mutate(across(where(is.character), ~na_if(., ""))) 

rob_cols <- data %>% 
  select(starts_with("item_")) %>% 
  colnames() %>% 
  sort()

rob_data <- data %>% 
  select(
    c(study_id, authors, year,
      outcome_label,
      all_of(rob_cols),
    )) %>% 
  relocate(item_10_other_other_sources_of_bias_answer, .after = last_col()) %>% 
  distinct()

rob_data <- rob_data %>%
  select(authors, year, ends_with("answer")) %>%
  #mutate(across(where(is.character), ~ str_to_sentence(.))) %>%
  #mutate(across(where(is.character), ~ str_replace_all(., "Unclear", "Some concerns"))) %>%
  filter(if_all(where(is.character), ~ !str_detect(., regex("Not applicable", ignore_case = TRUE)))) 
#%>%
#mutate(Overall = "Not Applicable")  

# clean author names
clean_authors <- function(author_str, year) {
  # Replace various newline combinations with a semicolon
  author_str <- str_replace_all(author_str, "\\r\\n|\\n\\r|\\r|\\n", ";")
  # Split authors based on ";" delimiter
  authors <- str_split(author_str, ";")[[1]]
  # Trim whitespace from author names
  authors <- str_trim(authors)
  # Split the first author's name by spaces
  first_author_parts <- str_split(authors[1], " ")[[1]]
  # Get the last part of the first author's name, which is usually the last name
  last_name <- first_author_parts[length(first_author_parts)]
  # Return formatted author string with year
  return(paste(last_name, " et al. ", year, "", sep = ""))
}

rob_data$authors <- apply(rob_data, 1, function(row) {
  clean_authors(row["authors"], row["year"])
})

# Function to add index for duplicate author-year combinations
add_index <- function(df) {
  df %>%
    group_by(authors) %>%
    mutate(index = row_number(),
           authors = ifelse(index == 1, authors, paste0(authors, letters[index]))) %>%
    ungroup() %>%
    select(-index)
}

rob_data <- add_index(rob_data)

rob_data <- rob_data %>%
  select (-year)

# manually change names where author function didn't work properly
rob_data <- rob_data %>%
  mutate(authors = case_when(
    authors == "Zeng et al. 2013" ~ "Ma et al. 2013",
    authors == "Redington et al. 2011" ~ "Hahn et al. 2011", 
    authors == "Hu et al. 2016" ~ "Qi et al. 2016", 
    authors == "Long et al. 2014" ~ "Liu et al. 2014", 
    TRUE ~ authors # Keep all other names unchanged
  ))

#clean column names
rob_data <- rob_data %>%
  rename_with(~ sub("_answer$", "", .), .cols = everything()) %>%
  rename_with(~ gsub("_", " ", .), .cols = everything())

write.csv(rob_data, file = "data/tidy/robvis_clean_2revagreements_data.csv", row.names = F)

png (filename = "data/plots/rob_traffic_2revagreements.png", height = 500, width = 700)
rob_traffic_light(
  rob_data,
  tool = "Generic",
  overall = FALSE,
  colour = "cochrane") 
# judgement_labels = c("Low risk of bias",
#                      "Unclear risk of bias",
#                      "High risk of bias",
#                      "Critical risk of bias",
#                      "No information")) +
# theme(text = element_text(size = 25),
#       legend.text = element_text(size = 20),
#       axis.text.y = element_text(size = 20))
dev.off()

