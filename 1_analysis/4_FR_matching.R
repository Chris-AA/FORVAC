library(dplyr)
library(MatchIt)
setwd("/exports/csce/datastore/geos/users/s1318698/FORVAC_matching")
L <- readRDS("0_data/1_format/2_province_clip/Lindi/FR_L_unmatched.Rds")

## Define CEM cutpoints
# Human access and proximity -----------------------------------------------------------------
tt_cut <- c(0,60,120,240,360,480,600,max(L[[1]]$tt))               
setl_cut <- c(0,10,25,50,100,200,max(L[[1]]$setl))      
roads_cut <- c(0,5,10,20,50,100,max(L[[1]]$roads))            
convrt_cut <- c(0,2.5,5,7.5,100,max(L[[1]]$convrt))          
# Forest edge and structure ------------------------------------------------------------------
wedge_cut <- c(0,2.5,5,10,20,max(L[[1]]$wedge))                 
agc_cut <- c(10,25,50,max(L[[1]]$agc))                             # 0/10/50 (due to previous steps = woodland(10-50)/forest(50+))
# Agricultural Suitability -------------------------------------------------------------------
maize_cut <- c(seq(0,max(L[[1]]$maize),length= 4))

# Make a full list (which is included is determined in the matchit call)
cutpoints <- list(tt = tt_cut,
                  setl = setl_cut,
                  roads =roads_cut,
                  convrt =convrt_cut,
                  wedge = wedge_cut,
                  agc = agc_cut,
                  maize = maize_cut)

## Matching----------------------------------------------------------------------------------
#m.out1 <- matchit(VLFR_fac ~ tt + setl + roads + convrt + wedge + agc + maize,
                  #data = L[[1]],
                  #method = "cem",
                  #cutpoints = cutpoints)

#plot(m.out1, type = "density", interactive = FALSE,
     #which.xs = c("tt", "setl" , "roads", "convrt" ,"wedge", "agc", "maize"))

#match <- match.data(m.out1)

match_fun <- function(L){
  
  # Run matchit
  match_obj <- matchit(FR_fac ~ tt + setl + roads + convrt + wedge + agc + maize,
                       data = L,
                       method = "cem",
                       cutpoints = cutpoints)
  
  # Extract matched data
  matched_data <- match.data(match_obj)
  
  return(matched_data)
}

L2 <- parallel::mclapply(L, match_fun, mc.cores = 2)

## Output unmatched VLFR list
saveRDS(L2, "0_data/1_format/2_province_clip/Lindi/FR_L_matched.Rds")
