library(dplyr)
library(readxl)


# pre reconciliation ----

# study level
df_stud <- read.csv("Annotation_data_-_2024_09_26_-_Wide_format_-_f668c4cf-449f-411b-a0fd-7c7c0db010b1_-_Investigators_Unblinded.csv")

# outcome level
df_out <- read.csv("Annotation_data_-_2024_09_26_-_Wide_format_-_f668c4cf-449f-411b-a0fd-7c7c0db010b1_-_Investigators_outcome.csv")

# identify answers with disagreements (rows with FALSE)
df_stud <- df %>%
  group_by(StudyId) %>%
  summarize(across(ends_with("_Answer"), ~ all(. == first(.)), .names = "consistent_{col}")) %>%
  ungroup()

df_out <- df_out %>%
  group_by(StudyId) %>%
  summarize(across(ends_with("_Answer"), ~all (. == first(.)), .names = "consistent_{col}")) %>%
  ungroup()

df <- left_join(df, df_out, by = "StudyId")

# export studies with disagreements 
df_disagree <- df %>%
  filter(if_any(starts_with("consistent_"), ~ . == FALSE))

df_dis_out <- df_out %>%
  filter(if_any(starts_with("consistent"), ~ . ==FALSE))

write.csv(df_dis_out, 
file = "data/tidy/outcome_disagreements_24-09-19.csv", row.names=F)
       
write.csv(df_disagree, file = "data/tidy/disagreements_all_24-09-18.csv", row.names = F)  

# post reconciliation ----

# study level
df_stud <- read.csv("Annotation_data_-_2024_09_26_-_Wide_format_-_f668c4cf-449f-411b-a0fd-7c7c0db010b1_-_Investigators_Unblinded.csv")

# outcome level
df_out <- read.csv("Annotation_data_-_2024_09_26_-_Wide_format_-_f668c4cf-449f-411b-a0fd-7c7c0db010b1_-_Investigators_outcome.csv")

# non overlaping studies - 2 don't have inf vols (should have been excluded from SR?), 1 Torsten didn't reconcile outcome lvl
diff <- anti_join(df_stud, df_out)

# combine outcome and study level annotations
# select only the outcome lvl annotations from df_out
colnames(df_out)

df_out_subset <- df_out %>%
  select(StudyId, 19:34)

df <- df_stud %>%
  merge(df_out_subset, by = c("StudyId", "InvestigatorId", "InvestigatorName"))

# filter to reconciled dataset - only Torsten's annotations (InvestigatorId e599294d-3218-4f40-b4b9-3389481cdde2)
df_rec <- df %>%
  filter(InvestigatorId == "e599294d-3218-4f40-b4b9-3389481cdde2")

write.csv(df_rec, file = "data/tidy/reconciled_data_24-09-26.csv", row.names = F)

# filter studies where SV (7fe9ced0-1c03-4baa-87a7-4714cb64513f) and MAA (8cb37f13-ff72-4f55-8b40-6a5ac023eed6) disagree
df_nonrec <- df %>%
  filter(InvestigatorId %in% c("1b96b68f-9627-4436-b421-db699510f2ae", 
                               "96b6bdc6-13fd-4440-84f4-1342ebd6b178"))

df_sv <- df_nonrec %>%
  filter (InvestigatorId == "1b96b68f-9627-4436-b421-db699510f2ae")

df_maa <- df_nonrec %>%
  filter (InvestigatorId == "96b6bdc6-13fd-4440-84f4-1342ebd6b178")

# identify answers with disagreements (rows with FALSE)
df_disagree <- df_nonrec %>%
  group_by(StudyId) %>%
  summarize(across(ends_with("_Answer"), ~ all(. == first(.)), .names = "consistent_{col}")) %>%
  ungroup()

df_disagree <- df_disagree %>%
  filter(if_any(starts_with("consistent_"), ~ . == FALSE))

# import file with info which study was done with which method
method <- read_excel("Bibliographic_data_-_2024_08_22_-_Wide_format_-_f668c4cf-449f-411b-a0fd-7c7c0db010b1_-_Investigators_Unblinded.xlsx")

# add method column to dataset - 1 = rob assessment as in original study, 2 = rob assessment with flowchart
df_disagree <- df_disagree %>%
  left_join(method %>% select(StudyId, rand2), by = "StudyId")

# see how many disagreements were in each assessment method
table(df_disagree$rand2)

# take out studies that TR reconciled
df_disagree <- anti_join(df_disagree, df_rec, by = "StudyId")

# create unique datasets with assessments from SV and MAA separately
df_sv <- df_sv %>%
  semi_join(df_disagree, by = "StudyId")

df_maa <- df_maa %>%
  semi_join(df_disagree, by = "StudyId")

write.csv(df_sv, file = "data/tidy/SV_post_discussion.csv", row.names = F)
write.csv(df_maa, file = "data/tidy/MAA_post_discussion.csv", row.names = F)

# filter studies where SV and MAA agree
df_agree <- df_nonrec %>%
  semi_join(df_disagree, by = "StudyId")

df_agree <- df_agree %>%
  left_join(method %>% select(StudyId, rand2), by = "StudyId")

table(df_agree$rand2)

write.csv(df_agree, file = "data/tidy/SVMAA_agreements.csv", row.names = F)


# inter rater consistency -------------------------------------------------

rand = data.table::fread("Bibliographic_data_-_2024_08_22_-_Wide_format_-_f668c4cf-449f-411b-a0fd-7c7c0db010b1_-_Investigators_Unblinded.csv") %>% 
  select(rand2, StudyId)

data <- df %>% 
  filter(!InvestigatorName %in% "Torsten Rackoll") %>% 
  left_join(rand, by = "StudyId") %>% 
  relocate(rand2)

singles <- data %>% 
  group_by(StudyId) %>% 
  tally() %>% 
  filter(n < 2) %>% 
  pull(StudyId)

agreements <- data %>% 
  filter(!StudyId %in% singles) %>% 
  select(rand2, StudyId, InvestigatorName, contains("Answer")) %>% 
  tidyr::pivot_longer(cols = contains("Answer"), names_to = "rob", values_to = "rating") %>% 
  tidyr::pivot_wider(names_from = InvestigatorName, values_from = rating) %>% 
  mutate(agreement = ifelse(`sofija vojvodic` == `María Arroyo Araujo`, TRUE, FALSE))

agreements %>% 
  group_by(rand2) %>% 
  summarize(n=length(unique(StudyId)))

result <- agreements %>% 
  group_by(rand2, rob, agreement) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(percentage = case_when(rand2 == 1 ~ n/36*100,
                                rand2 == 2 ~ n/33*100))


