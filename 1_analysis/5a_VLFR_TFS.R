## Run 5_VLFR post matching
def_sum_VLFR <- def_sum2
def_sum_VLFR$type <- "VLFR"
colnames(def_sum_VLFR)[1] <- "fac" # rename column

## Run 5 FR_post matching
def_sum_FR <- def_sum2
def_sum_FR$type <- "TFS"
colnames(def_sum_FR)[1] <- "fac" # rename column

## Merge dataframes
df <- rbind(def_sum_FR, def_sum_VLFR)
df$fac <- as.factor(df$fac)
df$fac <- factor(df$fac, levels = c("TRUE", "FALSE"))

# Widen df for plotting segments in ggplot
def_sum_wide <- df %>%
  tidyr::pivot_wider(
    id_cols = type,
    names_from = fac,
    values_from = c(def)
  )
colnames(def_sum_wide)[2] <- "m_false" # rename columns so not TRUE/FALSE
colnames(def_sum_wide)[3] <- "m_true"

a <- ggplot() +
  geom_segment(data = def_sum_wide,
               aes(x=type, xend=type, y=m_true, yend=m_false), color="grey") +
  geom_point(data = df, aes(x=type, y=def, colour = fac), size=7 ) +
  coord_flip()+
  theme_minimal() +
  theme(
    axis.text=element_text(size=15),
    axis.title=element_text(size=17),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(color="white"),
    legend.title=element_blank(),
    legend.text=element_text(size=12)
  ) +
  scale_colour_manual(
                      values = c("TRUE" = "#67a9cf", "FALSE" = "#ef8a62"),
                      labels = c("Treated", "Non-treated")) +
  xlab("Management") +
  ylab("Mean Deforestation Rate 2018-2022 (%)") +
  annotate("text", x = 2.3, y = 11, label = "E", size = 6) +
  annotate("text", x = 2, y = 11, label = "85 %", size = 6) +
  annotate("text", x = 1, y = 11, label = "80 %", size = 6) 
ggsave("/exports/csce/datastore/geos/users/s1318698/FORVAC_matching/VLFR_TFS.png", a, width = 25, height = 15, units = "cm", dpi = 300)
