library(dplyr)
library(ggplot2)
setwd("/exports/csce/datastore/geos/users/s1318698/FORVAC_matching")
L <- readRDS("0_data/1_format/2_province_clip/Lindi/VLFR_L_matched.Rds") # Matched data
df_raw <- readRDS("0_data/1_format/2_province_clip/Lindi/df2.Rds")

## Format
# Add VLFR name for matched pixels
# Using lapply to perform the operation on each dataframe in the list L
L <- parallel::mclapply(L, function(df) merge(df, df_raw[, c("px_id", "VLFR_name")], by = "px_id", all.x = TRUE), mc.cores = length(L))

# Step 1: Find non-NA values of VLFR_name in each dataframe
non_na_values <- parallel::mclapply(L, function(df) {
  na_index <- is.na(df$VLFR_name)
  unique_values <- na.omit(df$VLFR_name)
  if (length(unique_values) == 0) {
    return(NA)
  } else {
    return(unique_values[1])  # Take the first non-NA value
  }
}, mc.cores = length(L))  # Specify the number of cores

# Step 2: Replace NA values in VLFR_name column with the non-NA value
L <- parallel::mclapply(seq_along(L), function(i) {
  non_na_value <- non_na_values[[i]]
  L[[i]]$VLFR_name[is.na(L[[i]]$VLFR_name)] <- non_na_value
  return(L[[i]])
}, mc.cores = length(L))  # Specify the number of cores

# List to dataframe
df <- bind_rows(L)

df$VLFR_name <- as.factor(df$VLFR_name) # VLFR_name to factor

# Attach deforestation data to matched pixels
#df <- merge(df, df_raw[, c("px_id", "liwale_def_fac")], by = "px_id", all.x = TRUE)
df <- merge(df, df_raw[, c("px_id", "hansen_def")], by = "px_id", all.x = TRUE)

rm(df_raw); rm(L); rm(non_na_values); gc()

# Hansen def to fac
df$hansen_def_fac <- !is.na(df$hansen_def) & df$hansen_def > 1

## Calculate deforestation rates
def_sum <- df %>%
  group_by(VLFR_name, VLFR_fac) %>%
  summarise(def_r = mean(hansen_def_fac)*100,
            def_sd = sd(hansen_def_fac),
            def_se = def_sd / sqrt(length(hansen_def_fac)))
# Avg T vs F
def_sum2 <- def_sum %>%
  group_by(VLFR_fac) %>%
  summarise(def = mean(def_r),
            sd = sd(def_r),
            se = sd/sqrt(length(def_r)))

# Calculate effectiveness score: E = 1 - (def rate where VLFR_fac == TRUE/def rate where VLFR_fac == FALSE)*100
E_VLFR <- (1-(1.46/9.55))*100 # Add to plot

E <- def_sum %>% # Individual VLFRs
  group_by(VLFR_name) %>%
  summarise(E = (1 - (def_r[VLFR_fac == TRUE] / def_r[VLFR_fac == FALSE])) * 100)

# Add categorical income
rev<- read.csv("0_data/0_downloaded/timber_revenue.csv", strip.white = TRUE)
def_sum <- merge(def_sum, rev[, c("VLFR_name","rev")], by = "VLFR_name", all.x = TRUE)
E <- merge(E, rev[, c("VLFR_name","rev")], by = "VLFR_name", all.x = TRUE)
def_sum$rev <- as.factor(def_sum$rev)
E$rev <- as.factor(E$rev)

# Calculate avg def rates by income
def_sum_i <- def_sum %>%
  group_by(rev, VLFR_fac) %>%
  summarise(mean  = mean(def_r))


## Plot--------------------------------------------------------------------------------
## Avg VLFR vs Outside
def_sum2$VLFR_fac <- factor(def_sum2$VLFR_fac, levels = c(TRUE, FALSE))

# Dot plot
dot <- ggplot(def_sum2, aes(x = VLFR_fac, y = def, colour = VLFR_fac)) +
  geom_errorbar(aes(ymin = def - se, ymax = def + se), width = .1, alpha = 0.5) +
  geom_point(stat = "identity", size= 5) +
  scale_x_discrete(labels=c("VLFR", "Matched outside forest")) +
  scale_colour_manual("Management",
                      values = c("TRUE" = rgb(0.2,0.7,0.1, 0.5), "FALSE" = rgb(0.7,0.2,0.1, 0.5)),
                      labels = c("Matched outside forest", "VLFR"),
                      guide = "none") +
  theme_classic() +
  theme(
    axis.text=element_text(size=12),
    axis.title=element_text(size=14)) +
  labs(y= "Mean Deforestation Rate 2018-2022 (%)", x = "Management Type")

ggsave("/exports/csce/datastore/geos/users/s1318698/FORVAC_matching/VLFR_outside_dot.png", dot, width = 15, height = 15, units = "cm", dpi = 300)

## VLFR T/F def rates
# Widen df for plotting
def_sum_wide <- def_sum %>%
  tidyr::pivot_wider(
    id_cols = VLFR_name,
    names_from = VLFR_fac,
    values_from = c(def_r,def_sd,def_se, rev)
  )
def_sum_wide <- merge(def_sum_wide, E[, c("VLFR_name","E")], by = "VLFR_name", all.x = TRUE) # add E


# Plot using different dataframes and reordered
ggplot() +
  geom_segment(data = def_sum_wide %>% mutate(VLFR_name = forcats::fct_reorder(VLFR_name, E)),
               aes(x=VLFR_name, xend=VLFR_name, y=def_r_TRUE, yend=def_r_FALSE), color="grey") +
  geom_point(data = def_sum, aes(x=VLFR_name, y=def_r, colour = VLFR_fac), size=4 ) +
  coord_flip()+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_colour_manual("Management",
                      values = c("TRUE" = rgb(0.2,0.7,0.1,0.5), "FALSE" = rgb(0.7,0.2,0.1,0.5)),
                      labels = c("Other forests", "VLFR")) +
  xlab("VLFR") +
  ylab("Mean Deforestation Rate 2018-2022 (%)")

# Plot avg def rate by income
# Widen df for plotting segments in ggplot
def_sum_wide_i <- def_sum_i %>%
  tidyr::pivot_wider(
    id_cols = rev,
    names_from = VLFR_fac,
    values_from = c(mean)
  )
colnames(def_sum_wide_i)[2] <- "vlfr_false" # rename columns so not TRUE/FALSE
colnames(def_sum_wide_i)[3] <- "vlfr_true"

def_sum_wide_i <- def_sum_wide_i %>%
  mutate(E = (1-(vlfr_true/vlfr_false))*100)

# Reorder factor levels (not working with forcats in plot)
def_sum_wide_i$rev <- factor(def_sum_wide_i$rev, levels = rev(c("High", "Medium", "Low", "None", "NA")))
def_sum_i$rev <- factor(def_sum_i$rev, levels = rev(c("High", "Medium", "Low", "None", "NA")))
def_sum_i$VLFR_fac <- factor(def_sum_i$VLFR_fac, levels = c("TRUE", "FALSE"))

inc <- ggplot() +
  geom_segment(data = def_sum_wide_i,
               aes(x=rev, xend=rev, y=vlfr_true, yend=vlfr_false), color="grey") +
  geom_point(data = def_sum_i, aes(x=rev, y=mean, colour = VLFR_fac), size=6 ) +
  coord_flip()+
  theme_minimal() +
  theme(
    axis.text=element_text(size=12),
    axis.title=element_text(size=14),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(color="white"),
  ) +
  scale_colour_manual("Management",
                      values = c("TRUE" = rgb(0.2,0.7,0.1,0.5), "FALSE" = rgb(0.7,0.2,0.1,0.5)),
                      labels = c("VLFR", "Matched outside forest")) +
  xlab("VLFR Timber Revenue") +
  ylab("Mean Deforestation Rate 2018-2022 (%)") +
  annotate("text", x = 4.5, y = 4.5, label = "E", size = 4) +
  annotate("text", x = 4, y = 4.5, label = "87 %", size = 4) +
  annotate("text", x = 3, y = 4.5, label = "90 %", size = 4) +
  annotate("text", x = 2, y = 4.5, label = "85 %", size = 4) +
  annotate("text", x = 1, y = 4.5, label = "82 %", size = 4)

ggsave("/exports/csce/datastore/geos/users/s1318698/FORVAC_matching/VLFR_outside_inc.jpeg", inc, width = 25, height = 15, units = "cm", dpi = 300)
