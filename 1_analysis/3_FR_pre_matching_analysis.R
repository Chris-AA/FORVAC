library(dplyr)
library(corrplot)
library(MatchIt)
library(tibble)
setwd("/exports/csce/datastore/geos/users/s1318698/FORVAC_matching")
df <- readRDS("0_data/1_format/2_province_clip/Lindi/df2.Rds")

# Remove unnecessary variables
df <- df %>%
  select(px_id, liwale_def_fac, convrt, roads, setl, wedge, maize, agc, tt, PA, pop, FR_fac, FR, VLFR_fac, district)
df$FR <- as.factor(df$FR)

# Filter to Liwale and Nachingwea
df <- df %>%
  filter(district == 4 | district == 5)

# Remove VLFRs and protected areas
df2 <- df %>%
  filter(VLFR_fac == FALSE & is.na(PA))

rm(df);gc()

# Create a list of dataframes of individual FRs + outside forests for matching
L <- list()

L$fr_5 <- df2 %>%
  filter(FR == 5 | is.na(FR)) %>%
  select(px_id, FR_fac, convrt, roads, setl, wedge, maize, agc, tt) %>%
  as.tibble()
L$fr_11<- df2 %>%
  filter(FR == 11 | is.na(FR)) %>%
  select(px_id, FR_fac, convrt, roads, setl, wedge, maize, agc, tt) %>%
  as.tibble()


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
m.out0 <- matchit(FR_fac ~ convrt + roads + setl + wedge + maize + agc + tt,
                  data = L[[1]],
                  method = NULL,
                  distance = "glm")

## Output unmatched FR list
saveRDS(L, "0_data/1_format/2_province_clip/Lindi/FR_L_unmatched.Rds")
