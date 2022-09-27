library("tidyverse")

df_per <- readRDS("../stores/train_personas.Rds")
df_hog <- readRDS("../stores/train_hogares.Rds")

# Omit redundant variables in df_per
vars_rep <- intersect(colnames(df_per), colnames(df_hog))[-1]
df_per <- df_per %>% select(-all_of(vars_rep))

# Preliminary re-coding

df_hog[which(df_hog$Indigente == 1), "Pobre"] <- 1
df_per[which(df_per$Des == 1), "Oc"] <- 0

df_per <- df_per %>% mutate(
    P6020 = ifelse(P6020 == 1, 0, 1),
    P6090 = ifelse(P6090 == 1, 1, 0)
)
df_per[which(df_per$P6210 == 9), "P6210"] <- NA

# Join information from households and individuals
data <- df_hog %>% left_join(df_per, by = "id")

fmax <- function(x, na.rm = TRUE) {
    if (all(is.na(x))) {
        return(x[1])
    }
    max(x, na.rm = na.rm)
}

fmin <- function(x, na.rm = TRUE) {
    if (all(is.na(x))) {
        return(x[1])
    }
    x <- abs(x - 2)
    min(x, na.rm = na.rm)
}
# Agregate selected variables by household
data <- data %>%
    mutate(
        across(
            c(
                P6210,
                P6510,
                P6585s1,
                P6585s2,
                P6585s3,
                P6585s4,
                P6920,
                P7040,
                P7090,
                P7110,
                P7500s1,
                P7500s2,
                P7500s3,
                P7505,
                P7510s1,
                P7510s2,
                P7510s3,
                P7510s5,
                P7510s6,
                P7510s7
            ), as.numeric
        )
    ) %>%
    group_by(id) %>%
    mutate(
        Oc = sum(Oc, na.rm = TRUE) / dplyr::n(),
        P6020 = sum(P6020, na.rm = TRUE) / dplyr::n(),
        P6090 = sum(P6090, na.rm = TRUE) / dplyr::n(),
        P6040 = mean(P6040, na.rm = TRUE),
        P6210 = fmax(P6210),
        P6760 = mean(P6760, na.rm = TRUE)
    ) %>%
    mutate(
        across(
            c(
                P6510,
                P6585s1,
                P6585s2,
                P6585s3,
                P6585s4,
                P6920,
                P7040,
                P7090,
                P7110,
                P7500s1,
                P7500s2,
                P7500s3,
                P7505,
                P7510s1,
                P7510s2,
                P7510s3,
                P7510s5,
                P7510s6,
                P7510s7
            ), ~ fmin(.)
        )
    ) %>%
    ungroup()

# Collapse data back to household level
data <- data %>% filter(Orden == 1)

# Select those variables whose number of na's does not exceed 33%
# of the total number of observations
data <- data %>%
    select(
        where(
            ~ sum(is.na(.)) <= (nrow(data) * 0.30)
        )
    )


data <- data %>% select(
    Depto,
    Nper,
    Oc,
    Ingpcug,
    Pobre,
    Lp,
    P5000,
    P5010,
    P5090,
    P6920,
    P7040,
    P7090,
    P7505,
    P6020,
    P6090,
    P6040,
    P6210
)

data <- data %>%
    mutate(
        across(
            c(
                Depto,
                P5090,
                P6920,
                P7040,
                P7090,
                P7505,
                P6210
            ), as.factor
        )
    )

# Store data file
if (file.exists("../stores/data.rds")) {
    file.remove("../stores/data.rds")
}

write_rds(data, file = "../stores/data.rds")
