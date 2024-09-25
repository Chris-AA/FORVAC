library(dplyr)
library(corrplot)
library(MatchIt)

setwd("/exports/csce/datastore/geos/users/s1318698/FORVAC_matching")
df <- readRDS("0_data/1_format/2_province_clip/Lindi/df2.Rds")

# Remove unnecessary variables
df <- df %>%
  select(px_id, liwale_def_fac, convrt, roads, setl, wedge, maize, agc, tt, PA, pop, FR_fac, VLFR, VLFR_fac, district)

# Filter to Liwale and Nachingwea
df <- df %>%
  filter(district == 4 | district == 5)

# Remove forest reserves and protected areas
df2 <- df %>%
  filter(FR_fac == FALSE & is.na(PA))

rm(df);gc()

# Create a list of dataframes of individual VLFRs + outside forests for matching
L <- list()

for (i in unique(df2$VLFR)) {
  # Filter dataframe based on current value of "VLFR"
  filtered_df <- df2 %>%
    filter(VLFR == i | is.na(VLFR)) %>%
    select(px_id, VLFR_fac, VLFR,convrt, roads, setl, wedge, maize, agc, tt) %>%
    as.tibble()
  
  # Assign filtered dataframe to the list with corresponding name
  L[[paste0("vlfr_", i)]] <- filtered_df
}

L <- L[-1] # Remove first dataframe that has only NAs


rm(df2);gc()

# Assess correlations among co-variates
for (i in L) {
  df <- i %>%
    select(convrt, roads, setl, wedge, maize, agc, tt)
  M <- cor(df)
  corrplot(M, method = "number")
}

rm(df); rm(i); rm(M); gc()

# Assess initial imbalance pre matching
m.out0 <- matchit(VLFR_fac ~ convrt + roads + setl + wedge + maize + agc + tt,
                  data = L[[1]],
                  method = NULL,
                  distance = "glm")

## Output unmatched VLFR list
saveRDS(L, "0_data/1_format/2_province_clip/Lindi/VLFR_L_unmatched.Rds")
