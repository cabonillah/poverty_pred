# TODO: delete unnecessary variables before line 33

library("tidyverse")

df_per <- readRDS("../stores/train_personas.Rds")
df_hog <- readRDS("../stores/train_hogares.Rds")

# Omit redundant variables in df_per
vars_rep <- intersect(colnames(df_per), colnames(df_hog))[-1]
df_per <- df_per %>% select(-all_of(vars_rep))

# Join information from households and individuals
data <- df_hog %>% left_join(df_per, by = "id")

# Select those variables whose number of na's does not exceed 33%
# of the total number of observations
data <- data %>%
    select(
        where(
            ~ sum(is.na(.)) <= (nrow(data) * 0.33)
        )
    )

# Sort remaining variables
data <- data %>%
    select(
        id, # id column
        matches(c("^Ing", "^Io")), # Variables starting with "Ing" or "Io"
        order(tidyselect::peek_vars()) # The rest of available variables, ordered alphabetically
    )
