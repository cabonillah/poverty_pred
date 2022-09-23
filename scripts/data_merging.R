library("tidyverse")

df_per <- readRDS("../stores/train_personas.Rds")
df_hog <- readRDS("../stores/train_hogares.Rds")

# Omit redundant variables in df_per
vars_rep <- intersect(colnames(df_per), colnames(df_hog))[-1]
df_per <- df_per %>% select(-all_of(vars_rep))

df_per <- df_per %>% mutate(
    P6020 = ifelse(P6020 == 1, 0, 1),
    P6090 = ifelse(P6090 == 1, 1, 0)
)

# Join information from households and individuals
data <- df_hog %>% left_join(df_per, by = "id")

# TODO: verify aggregation... takes too long, explore parallelizing https://blog.aicry.com/multidplyr-dplyr-meets-parallel-processing/index.html
data <- data %>%
    group_by(id) %>%
    mutate(
        P6020 = sum(P6020, na.rm = TRUE) / n(),
        P6090 = sum(P6090, na.rm = TRUE) / n(),
        P6040 = mean(P6040, na.rm = TRUE),
        P6210 = max(P6210, na.rm = TRUE),
        P6760 = mean(P6760, na.rm = TRUE)
    ) %>%
    mutate_at(
        vars(
            c(
                "P6510",
                "P6585s1",
                "P6585s2",
                "P6585s3",
                "P6585s4",
                "P6920",
                "P7040",
                "P7090",
                "P7110",
                "P7500s1",
                "P7500s2",
                "P7500s3",
                "P7505",
                "P7510s1",
                "P7510s2",
                "P7510s3",
                "P7510s5",
                "P7510s6",
                "P7510s7"
            )
        ), ~ min(na.rm = TRUE)
    )


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
