library(dplyr)
library(ggplot2)
setwd("/exports/csce/datastore/geos/users/s1318698/FORVAC_matching")
L <- readRDS("0_data/1_format/2_province_clip/Lindi/FR_L_matched.Rds") # Matched data
df_raw <- readRDS("0_data/1_format/2_province_clip/Lindi/df2.Rds")

## Format
# Add FR name for matched pixels
# Using lapply to perform the operation on each dataframe in the list L
L <- parallel::mclapply(L, function(df) merge(df, df_raw[, c("px_id", "FR_name")], by = "px_id", all.x = TRUE), mc.cores = 2)

# Step 1: Find non-NA values of FR_name in each dataframe
non_na_values <- parallel::mclapply(L, function(df) {
  na_index <- is.na(df$FR_name)
  unique_values <- na.omit(df$FR_name)
  if (length(unique_values) == 0) {
    return(NA)
  } else {
    return(unique_values[1])  # Take the first non-NA value
  }
}, mc.cores = 2)  # Specify the number of cores

# Step 2: Replace NA values in FR_name column with the non-NA value
L <- parallel::mclapply(seq_along(L), function(i) {
  non_na_value <- non_na_values[[i]]
  L[[i]]$FR_name[is.na(L[[i]]$FR_name)] <- non_na_value
  return(L[[i]])
}, mc.cores = 2)  # Specify the number of cores

# List to dataframe
df <- bind_rows(L)

df$FR_name <- as.factor(df$FR_name) # VLFR_name to factor

# Attach deforestation data to matched pixels
#df <- merge(df, df_raw[, c("px_id", "liwale_def_fac")], by = "px_id", all.x = TRUE)
df <- merge(df, df_raw[, c("px_id", "hansen_def")], by = "px_id", all.x = TRUE)

# Hansen def to fac
df$hansen_def_fac <- !is.na(df$hansen_def) & df$hansen_def > 1

rm(df_raw); rm(L); rm(non_na_values); gc()

## Calculate deforestation rates
def_sum <- df %>%
  group_by(FR_name, FR_fac) %>%
  summarise(def_r = mean(hansen_def_fac)*100,
            def_sd = sd(hansen_def_fac),
            def_se = def_sd / sqrt(length(hansen_def_fac)))
# Avg T vs F
def_sum2 <- def_sum %>%
  group_by(FR_fac) %>%
  summarise(def = mean(def_r),
            sd = sd(def_r),
            se = sd/sqrt(length(def_r)))

# Calculate effectiveness score: E = 1 - (def rate where FR_fac == TRUE/def rate where FR_fac == FALSE)*100
E <- def_sum %>%
  group_by(FR_name) %>%
  summarise(E = (1 - (def_r[FR_fac == TRUE] / def_r[FR_fac == FALSE])) * 100)

## Plot--------------------------------------------------------------------------------
## Avg VLFR vs Outside
def_sum2$FR_fac <- factor(def_sum2$FR_fac, levels = c(TRUE, FALSE))

E_TFS <- (1-(1.26/6.33))*100 # Add to plot

# Dot plot
dot <- ggplot(def_sum2, aes(x = FR_fac, y = def, colour = FR_fac)) +
  geom_errorbar(aes(ymin = def - se, ymax = def + se), width = .1, alpha = 0.5) +
  geom_point(stat = "identity", size= 5) +
  scale_x_discrete(labels=c("TFS Forest Reserve", "Matched outside forest")) +
  scale_colour_manual("Management",
                      values = c("FALSE" = rgb(0.7,0.2,0.1, 0.5)),
                      labels = c("Matched outside forest", "TFS Forest Reserve"),
                      guide = "none") +
  theme_classic() +
  theme(
    axis.text=element_text(size=12),
    axis.title=element_text(size=14)) +
  labs(y= "Mean Deforestation Rate 2018-2022 (%)", x = "Management Type")

ggsave("/exports/csce/datastore/geos/users/s1318698/FORVAC_matching/FR_outside_dot.png", dot, width = 15, height = 15, units = "cm", dpi = 300)
