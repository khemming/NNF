
# library -------------------------------------
  library(tidyverse)

# data ----------------------------------------
# families
  fams <-  data.frame(read.csv("Results/csv/families and predictors/study families.csv", stringsAsFactors = F)[1:22, 1])
  colnames(fams) <- "family"
  glimpse(fams)

# worldwide famileis from  CHRISTENHUSZ & BYNG, 2016
# note: Poaceae numbers are Chris...'s & Sage 2016's C4 ones 
# Chenopodicaea from: https://bioone.org/journals/willdenowia/volume-45/issue-3/wi.45.45301/A-taxonomic-backbone-for-the-global-synthesis-of-species-diversity/10.3372/wi.45.45301.full
  fams$ww_gen <- c(165, 442, 366, 114, 1632,
                  135, 328,  100,  53,   90, 
                  124, 209, 751,   8,  241,
                  244,  94, 459,  321,  590,
                   62, 100)
  # note: Poaceae genera are 780 and spp. 12,000
  
  fams$ww_spp <- c(2040, 3575,  5100, 2900, 24700,
                  2535, 3628,    NA, 1660,  5500,  
                  4250, 6252, 19500,  464,  7530,
                  4225, 1900,  6956, 5044, 13620,
                  1830, 2600)  
# Sage, 2016
  fams$C4_spp <- c(257,   0,   0,    0, 0, 
                  130,   0, 558,    0, 0, 
                    0, 350,   0,    0, 0, 
                    0,   0,  1600, 5044, 0, 
                    4,   0)
# save
  write.csv(fams, "Results/csv/correlates/worldwide spp gen pp.csv", row.names = F)

# -------------------------------------------------  