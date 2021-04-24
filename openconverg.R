###################################################################################
# R implementation of the New Jersey School Funding Formula for Equalization Aid  #
# Gavin Rozzi                                                                     #
###################################################################################

setwd("~/GitHub/nj-school-funding")

library(tidyverse)
library(sas7bdat)

###########################
# Define needed functions #
###########################

# Local Share
# LSHARE = (EQVAL x PVR x 50%) + (INC x INR x 50%)
lshare <- function(WLT_EQVL,prm_evm,WLT_INCM,prm_im) {
  share <- (WLT_EQVL * prm_evm * 0.5) + (WLT_INCM * prm_im * 0.5)
  return(share)
}

# Read in EQA_FORM data with inputs to the algorithm from NJDOE
eqa_form <- read.sas7bdat("data/168719 eqa form.sas7bdat")


# Target is the amount determined pursuant to N.J.S.A. 18A:7F-48

# Step 1. Calculate Local Share, and difference between adequacy budget and
# # [(Property Value Property Rate Multiplier) + (Income Income Rate Multiplier)]/2. 
# prm_im = Income multiplier aka f18ap
# prm_evm = Property value multiplier aka f18bp
# WLT_EQVL = Equalized valuation
# WLT_INCM = Aggregate income

eqa_form <- eqa_form %>% 
  mutate(local_share = lshare(WLT_EQVL,prm_evm,WLT_INCM,prm_im))
         
# Calculate total of adequacy budgets and local share by county
# COAB is sum of all county adequacy budgets
# COLSHARE is sum of local shares for all school districts

county_eqa <- eqa_form %>% 
  group_by(CO) %>% 
  summarise(COAB = sum(adq_bud),
            COLSHARE = sum(na.omit(local_share)))

# Join votech schools with county data and calculate local share
votech <- eqa_form %>% 
  filter(local_share == 'NaN') %>% 
  left_join(county_eqa, by = 'CO') %>%
  mutate(local_share = (COLSHARE / COAB) * adq_bud)

# Drop the original votech rows from the eqa_form
eqa_form <- eqa_form %>%
  anti_join(votech,by = 'DIST')

# Bind the votech dataframe with calculations back to eqa_form
eqa_form <- rbind(eqa_form,votech %>% select(-COAB,-COLSHARE)) %>%
  group_by(CO)

# Calculate difference between adequacy budget and local share. Set equalization aid to 0 when difference between adequacy budget and local share is negative
eqa_form <- eqa_form %>%
  mutate(difference = adq_bud - local_share,
         eqaid = case_when(difference < 0 ~ 0,
                           difference > 0 ~ difference))


eqa_form$prm_evm * 7911180840

# Iterate until convergence is reached
# Initialize flag and cycle counter at 0
flag <- 0
cyc <- 0

while (flag == 0)
{
  for (cyc in 1:3) {
    print(cyc)
  }
  
  
}

if (cyc == 1) {
  eqa_form$prm_evm <- round(eqa_form$prm_evm[1],9)
  eqa_form$prm_im <- 0
} else if (cyc == 2) {
  eqa_form$prm_evm <- 0
  eqa_form$prm_im <- round(eqa_form$prm_evm[1],9)
} else if (cyc == 3) {
  
}

for (i in 1:nrow(eqa_form)) {
  if (eqa_form$WENR_ENR[i] > 0) {
    eqa_form$EQA_DLSR <- 1
    eqa_form$EQA_DSLS <- round((eqa_form$EQA_ABPY[i] * eqa_form$EQA_DLSR[i]),1)
  } else {
    eqa_form$EQA_DLSR <- 0
    eqa_form$EQA_DSLS <- 0
  }
}

