library("tidyverse")

df_per <- readRDS("../stores/train_personas.Rds")
df_hog <- readRDS("../stores/train_hogares.Rds")

data <- df_hog %>% left_join(df_per, by = "id")

# Select those variables whose number of na's does not exceed 33% of the total number of observations
data <- data %>%
    select(
        where(
            ~ sum(is.na(.)) <= (nrow(data) * 0.33)
        )
    )
