# SILAC Analysis
# Protein profiles during myocardial cell differentiation

# Load packages ----
library(tidyverse)
library(glue)

# Part 0: Import data ----
protein_df <- read_tsv("data/Protein.txt")
# protein_df <- read.delim("data/Protein.txt")

# Examine the data:
head(protein_df)
glimpse(protein_df)
# str(protein_df)
summary(protein_df)
dim(protein_df)
protein_df # useful if you have a tibble

# good, but not interesting
typeof(protein_df)
class(protein_df)
# Avoid:
# View(protein_df)

# Quantify the contaminants ----
sum(protein_df$Contaminant == "+", na.rm = TRUE)
protein_df %>% 
  count(Contaminant)
table(protein_df$Contaminant)

# Proportion of contaminants
# the total number of contaminants divided by the total number of observations
sum(protein_df$Contaminant == "+")/nrow(protein_df)
# length(protein_df$Contaminant)


# Percentage of contaminants (just multiply proportion by 100)
sum(protein_df$Contaminant == "+")/nrow(protein_df) * 100


# Transformations & cleaning data ----

# Remove contaminants ====

# protein_df <- 
# This will work if you have a "+" and ""
protein_df %>% 
  filter(Contaminant != "+")

# But if you have NAs it won't
# Because filter() automatically removes NAs
protein_df <- protein_df %>% 
  filter(is.na(Contaminant))

# Asking
is.na(protein_df$Contaminant)


# log 10 transformations of the intensities ====
protein_df$Intensity.L <- log10(protein_df$Intensity.L)
protein_df$Intensity.M <- log10(protein_df$Intensity.M)
protein_df$Intensity.H <- log10(protein_df$Intensity.H)


# Add the intensities ====
protein_df$Intensity.H.M <- protein_df$Intensity.H + protein_df$Intensity.M
protein_df$Intensity.M.L <- protein_df$Intensity.M + protein_df$Intensity.L

# log2 transformations of the ratios ====
protein_df$Ratio.H.M <- log2(protein_df$Ratio.H.M)
protein_df$Ratio.M.L <- log2(protein_df$Ratio.M.L)

# Part 2: Query data using filter() ----
# Exercise 9.2 (Find protein values) ====

query <- c("GOGA7", "PSA6", "S10AB")

# Add _MOUSE to query
# paste0(query, "_MOUSE")
query <- glue("{query}_MOUSE")

protein_df %>% 
  filter(Uniprot %in% query) %>%
  select(Intensity.H.M)

# protein_df %>% 
#   filter(Uniprot=="GOGA7") %>% 
#   select(protein)

protein_df[protein_df$Uniprot %in% query, "Intensity.H.M"]

protein_df %>% 
  filter(str_detect(Uniprot, "Q3UF01"))

protein_df %>% 
  filter(str_detect(Description, "Ubi"))

protein_df_modify <- protein_df %>% 
  mutate(Uniprot = strsplit(Uniprot,";")) %>% 
  unnest(Uniprot) %>% 
  mutate(Uniprot = str_remove(Uniprot, "_MOUSE")) %>% 
  filter(Uniprot %in% c("GOGA7", "PSA6", "S10AB"))

glimpse(protein_df_modify)

protein_df_modify$Uniprot[1]

glimpse(starwars)


# Exercise 9.3 (Find significant hits) and 10.2 ====
# For the H/M ratio column, create a new data 
# frame containing only proteins that have 
# a p-value less than 0.05


# Exercise 10.4 (Find top 20 values) ==== 


# Exercise 10.5 (Find intersections) ====

