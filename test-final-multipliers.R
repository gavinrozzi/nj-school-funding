library(tidyverse)
library(sas7bdat)

setwd("~/GitHub/nj-school-funding")

# Read in EQA_FORM data with inputs to the algorithm from NJDOE
eqa_form_final <- read.sas7bdat("data/168719 eqa form.sas7bdat")


# Local Share
# LSHARE = (EQVAL x PVR x 50%) + (INC x INR x 50%)
lshare <- function(WLT_EQVL,prm_evm,WLT_INCM,prm_im) {
  share <- (WLT_EQVL * prm_evm * 0.5) + (WLT_INCM * prm_im * 0.5)
  return(share)
}

# Final multipliers for 2020-2021
pvr <- as.double(0.014483784, length = 9)

inr <- as.double(0.052773887, length = 9)

# Remove Tavistock Borough from consideration
eqa_form_final <- eqa_form_final %>% filter(DIST != '5140') 

eqa_form_final <- eqa_form_final %>% 
  mutate(local_share = lshare(WLT_EQVL,pvr,WLT_INCM,inr))

# Calculate difference between adequacy budget and local share
eqa_form_final <- eqa_form_final %>%
  mutate(difference = adq_bud - local_share,
         eqaid = case_when(difference <= 0 ~ 0,
                           difference > 0 ~ difference))

# Calculate total of adequacy budgets and local share by county
# COAB is sum of all county adequacy budgets
# COLSHARE is sum of local shares for all school districts

county_eqa_final <- eqa_form_final %>% 
  group_by(CO) %>% 
  summarise(COAB = sum(adq_bud),
            COLSHARE = sum(na.omit(local_share)))

# Join votech schools with county data and calculate local share
votech_final <- eqa_form_final %>% 
  filter(local_share == 'NaN') %>% 
  left_join(county_eqa_final, by = 'CO') %>%
  mutate(local_share = (COLSHARE / COAB) * adq_bud,
         eqaid = adq_bud - local_share)

# Drop the original votech rows from the eqa_form
eqa_form_final <- eqa_form_final %>%
  anti_join(votech_final,by = 'DIST')

# Bind the votech dataframe with calculations back to eqa_form
eqa_form_final <- rbind(eqa_form_final,votech_final %>% select(-COAB,-COLSHARE)) %>%
  group_by(CO)

# Calculate difference between adequacy budget and local share. Set equalization aid to 0 when difference between adequacy budget and local share is negative
eqa_form_final <- eqa_form_final %>%
  mutate(difference = adq_bud - local_share,
         eqaid = case_when(difference < 0 ~ 0,
                           difference > 0 ~ difference))

difference_from_tg <- eqa_form_final$target[1] - sum(eqa_form_final$eqaid)

cat(paste('Total Aid:',sum(eqa_form_final$eqaid)))
cat(paste('Difference from target:',difference_from_tg))

