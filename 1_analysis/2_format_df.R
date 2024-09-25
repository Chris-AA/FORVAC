library(dplyr)

setwd("/exports/csce/datastore/geos/users/s1318698/FORVAC_matching")
df <- readRDS("0_data/1_format/2_province_clip/Lindi/df.Rds")

## Format data for analysis
# Pop data
# df$pop2 <- df$pop
# df$pop2[df$pop2==0] <- min(df$pop2[df$pop2!=0])
# df$pop_log <- log10(df$pop2)

# Def vars
df$liwale_def[df$liwale_def > 1] <- NA # all values != 1 to na
df$hansen_def[df$hansen_def < 18] <- NA # 0s and before 2016 to NA

# Logical def
df$liwale_def_fac <- !is.na(df$liwale_def) & df$liwale_def == 1
df$mc_def_fac <- !is.na(df$mc_def) & df$mc_def == 1
df$hansen_def_fac <- !is.na(df$hansen_def) & df$hansen_def > 1

# FR to logical
df$FR_fac <- !is.na(df$FR) & df$FR > 0

# PA to logical
df$PA_fac <- !is.na(df$PA) & df$PA > 0

# VLFR to logical
df$VLFR_fac <- !is.na(df$VLFR) & df$VLFR > 0

# Exclude PA buffer
df <- df %>%
  filter(is.na(PA_buff) | PA_buff == 1 & PA == 1)

# Exclude VLFR buffer
df <- df %>%
  filter(is.na(VLFR_buff) | VLFR_buff == 1 & VLFR_fac == 1)

# Exclude FR buffer
df <- df %>%
  filter(is.na(FR_buff) | FR_buff == 1 & FR_fac == 1)

# FR names vector
df <- df %>%
  mutate(
    FR_name = case_when(
      FR == 1 ~ "Mangroves", FR == 2 ~ "Mitauure", FR == 3 ~ "Mangroves", FR == 4 ~ "Mbinga Kimaji", FR == 5 ~ "Lugonya",
      FR == 6 ~ "Mangroves", FR == 7 ~ "Matapwa", FR == 8 ~ "Ruawa", FR == 9 ~ "Ndimba", FR == 10 ~ "Malehi",
      FR == 11 ~ "Nyera", FR == 12 ~ "Rungo", FR == 13 ~ "Mangroves", FR == 14 ~ "Ntama", FR == 15 ~ "Mkangala",
      FR == 16 ~ "Tongomba", FR == 17 ~ "Chitoa", FR == 18 ~ "Nandimba", FR == 19 ~ "Mitundumbea", FR == 20 ~ "Mangroves",
      FR == 21 ~ "Mangroves", FR == 22 ~ "Mangroves", FR == 23 ~ "Ngarama North", FR == 24 ~ "Nampekeso", FR == 25 ~ "Tambulu",
      FR == 26 ~ "Mangroves", FR == 27 ~ "Mangroves", FR == 28 ~ "Mangroves", FR == 29 ~ "Mangroves", FR == 30 ~ "Mangroves",
      FR == 31 ~ "Mangroves", FR == 32 ~ "Mangroves", FR == 33 ~ "Mangroves", FR == 34 ~ "Mangroves", FR == 35 ~ "Mangroves",
      FR == 36 ~ "Mangroves", FR == 37 ~ "Mangroves", FR == 38 ~ "Mangroves", FR == 39 ~ "Mangroves", FR == 40 ~ "Mangroves",
      FR == 41 ~ "Mangroves", FR == 42 ~ "Mangroves", FR == 43 ~ "Mangroves", FR == 44 ~ "Mangroves", FR == 45 ~ "Mangroves",
      FR == 46 ~ "Mangroves", FR == 47 ~ "Mangroves", FR == 48 ~ "Mangroves", FR == 49 ~ "Mangroves", FR == 50 ~ "Mangroves",
      FR == 51 ~ "Mangroves", FR == 52 ~ "Mangroves", FR == 53 ~ "Mangroves", FR == 54 ~ "Mangroves", FR == 55 ~ "Mangroves",
      FR == 56 ~ "Mangroves", FR == 57 ~ "Mangroves", FR == 58 ~ "Mangroves", FR == 59 ~ "Mangroves", FR == 60 ~ "Mangroves",
      FR == 61 ~ "Mangroves", FR == 62 ~ "Mangroves", FR == 63 ~ "Mangroves", FR == 64 ~ "Mangroves", FR == 65 ~ "Mangroves",
      FR == 66 ~ "Kitope Hill", FR == 67 ~ "Litipo"
    )
  )
# Exclude TFS Mangroves
df <- df %>%
  filter(is.na(FR_name) | FR_name != "Mangroves")

# VLFR income
df <- df %>%
  mutate(
    rev = case_when(
      VLFR == 1 ~ 536927837 , VLFR == 2 ~ 222470000, VLFR == 3 ~ 222470000, VLFR == 4 ~ 379085000, VLFR == 5 ~ 161605000,
      #VLFR == 6 ~ "NA", 
      VLFR == 7 ~ 43210000, VLFR == 8 ~ 29900000, VLFR == 9 ~ 192650000, VLFR == 10 ~ 5800000,
      VLFR == 11 ~ 118216935, VLFR == 12 ~ 306176566, VLFR == 13 ~ 27455700, VLFR == 14 ~ 53487600, VLFR == 15 ~ 430890000,
      VLFR == 16 ~ 430890000, VLFR == 17 ~ 78700000,
      #VLFR == 18 ~ "NA",
      VLFR == 19 ~ 79685000, VLFR == 20 ~ 163919000,
      VLFR == 21 ~ 361489900, VLFR == 22 ~ 106780000, VLFR == 23 ~ 678883790, VLFR == 24 ~ 729533100, VLFR == 25 ~ 229750000,
      VLFR == 26 ~ 164397695, VLFR == 27 ~ 420046600, VLFR == 28 ~ 24360000, VLFR == 29 ~ 215895580, VLFR == 30 ~ 310960000,
      VLFR == 31 ~ 153695000, VLFR == 32 ~ 58580000, VLFR == 33 ~ 420739880, VLFR == 34 ~ 420739880,
      #VLFR == 35 ~ "NA", VLFR == 36 ~ "NA",
      VLFR == 37 ~ 218219500
    )
  )

# VLFR names vector
df <- df %>%
  mutate(
    VLFR_name = case_when(
      VLFR == 1 ~ "Barikiwa", VLFR == 2 ~ "Chigugu A", VLFR == 3 ~ "Chigugu B", VLFR == 4 ~ "Chimbuko", VLFR == 5 ~ "Kibutuka",
      VLFR == 6 ~ "Kiegei A", VLFR == 7 ~ "Kiegei B", VLFR == 8 ~ "Kilimarondo", VLFR == 9 ~ "Kitogoro", VLFR == 10 ~ "Legezamwendo",
      VLFR == 11 ~ "Lichwachwa", VLFR == 12 ~ "Likombora", VLFR == 13 ~ "Lipuyu", VLFR == 14 ~ "Litou", VLFR == 15 ~ "Luwele A",
      VLFR == 16 ~ "Luwele B", VLFR == 17 ~ "Mahonga", VLFR == 18 ~ "Matekwe", VLFR == 19 ~ "Mbondo", VLFR == 20 ~ "Mchichili",
      VLFR == 21 ~ "Mihumo", VLFR == 22 ~ "Mikunya", VLFR == 23 ~ "Mikuyu", VLFR == 24 ~ "Mtawatawa", VLFR == 25 ~ "Mtungunyu",
      VLFR == 26 ~ "Nahanga", VLFR == 27 ~ "Nahoro", VLFR == 28 ~ "Namatunu", VLFR == 29 ~ "Nandenje", VLFR == 30 ~ "Nangano",
      VLFR == 31 ~ "Nanjegeja", VLFR == 32 ~ "Naujombo", VLFR == 33 ~ "Ngongowele A", VLFR == 34 ~ "Ngongowele B", VLFR == 35 ~ "Ngumbu",
      VLFR == 36 ~ "Ngunichile", VLFR == 37 ~ "Turuki"
    )
  )

# District names
df <- df %>%
  mutate(
    district_name = case_when(
      district == 1 ~ "Kilwa",
      district == 2 ~ "Lindi",
      district == 3 ~ "Lindi",
      district == 4 ~ "Liwale",
      district == 5 ~ "Nachingwea",
      district == 6 ~ "Ruangwa"
    )
  )

# Distance vars to km and other vars to factor
df <- df %>%
  mutate(
    px_id = 1:nrow(df), # add a pixel id column
    convrt = convrt/1000,
    roads = roads/1000,
    setl = setl/1000,
    wedge = wedge/1000,
    VLFR = as.factor(VLFR),
    district = as.factor(district)
  )

## Check
summary(df)

## Save
saveRDS(df, "0_data/1_format/2_province_clip/Lindi/df2.Rds")
