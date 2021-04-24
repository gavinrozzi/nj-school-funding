##############################################################
# R implementation of the New Jersey School Funding Formula  #
# Gavin Rozzi                                                #
##############################################################

setwd("~/GitHub/nj-school-funding")

# Load required libraries
library(tidyverse)
library(readxl)
library(sas7bdat)



###############################################
# Part 1. Reconstructing the EQA_FORM dataset #
###############################################

# Get District Info from NJ SMART and read the Excel sheet to a dataframe
tmp <- tempfile()
download.file("https://www.njsmart.org/njr/ks/Key%20Documents/NJ%20SMART%20County%20District%20School%20Code%20List.xlsx", destfile = tmp, mode="wb")
district_info <- read_xlsx(tmp, sheet = "District Info")


# Read in ASSA data from NJDOE OPRA request
# Source: https://opramachine.com/alaveteli_pro/info_requests/data_collected_pursuant_to_njsa

assa_2018 <- read_xlsx("data/OCTOBER 2018 ASSA.xlsx", sheet = "October 2018 ASSA")
assa_2019 <- read_xlsx("data/OCTOBER 2019 ASSA.xlsx", sheet = "MERGED3")


# Use the 2021 data file just for the current equalized valuations from 2020
taxdata <- read_csv('data/2021/esttax21_minus2.csv') %>%
  select(county_id,county_name,district_id,district_name,estrwo_eev) %>%
  rename('equalized_valuation' = estrwo_eev) %>% 
  mutate(county_name = paste(county_name,'County'))

# Revenue by district from NJ DOE. Filtered just to line items for aid payments.
revdata <- read_csv('data/2021/rev21_minus2.csv') %>% 
  filter(grepl('Aid',line_desc))

# Enrollment data by grade for NJ school districts
enrollment <- read_xlsx('data/enrollment_1920.xlsx', sheet = "District")

# Geographic Cost Adjustment by County multipliers. Latest revision is 2014 updates. Based on ACS 5-year estimates.
# Source: https://www.state.nj.us/education/sff/gca2014.pdf
gca <- read_csv('data/gca2014.csv') %>% 
  select(county,revised_gca_14)

# Data from the Governor's Educational Adequacy Report

# Join equalized values with gca multipliers
eqa_form <- taxdata %>% 
  left_join(gca, by = c('county_name' = 'county'))

# Calculate the Adequacy Budget and its components using a custom function

# Base Amount Per Pupil pursuant to N.J.S.A. 18A:7F-46(a)
base_per_pupil <- 11775

# Define grade weights
ms_weight <- 1.04
hs_weight <- 1.16

# Adequacy Budget = (Base Cost + AR Cost + LEP Cost + Comb.Cost + Spec Ed Census) x GCA
# Being tested with jc18dataframe

adequacy_budget <- function(base_per_pupil, elem_enrollment, ms_enrollment, hs_enrollment,gca) {
  # Apply the weights to total enrollment numbers by grade level
  elem <- jc18[6,]$ONROLL_FT + jc18[7,]$ONROLL_FT + jc18[8,]$ONROLL_FT + jc18[9,]$ONROLL_FT + jc18[10,]$ONROLL_FT + jc18[11,]$ONROLL_FT
  
  ms_enrollment <- jc18[12,]$ONROLL_FT + jc18[13,]$ONROLL_FT + jc18[14,]$ONROLL_FT
  
  ms <- ms_enrollment * ms_weight
  
  hs_enrollment <- jc18[15,]$ONROLL_FT + jc18[16,]$ONROLL_FT + jc18[17,]$ONROLL_FT + jc18[18,]$ONROLL_FT
  
  
  hs <- hs_enrollment * hs_weight
  
  # Calculate base cost 
  # Base Cost = 11,775 x [Elem Enr + (MS Enr x 1.04) + (HS Enr x 1.16)]
  total_enrollment <- sum(elem,ms,hs)
  base_cost <- base_per_pupil * total_enrollment
  
  # Calculate At-Risk cost
  at_risk_cost <- 
    
  # Calculate the Limited English Proficient (LEP) cost
  lep_cost <- 
    
  # COMB cost
  comb_cost <-
    
  # Special Ed census
  spec_ed <- 
  
  # Use the base cost and other values to calculate the final adequacy budget
  budget <- (base_cost + at_risk_cost + lep_cost + comb_cost + spec_ed) * gca
  return(budget)
}

#########################
# Part 2. The algorithm #
#########################

# Read in EQA_FORM data with inputs to the algorithm from NJDOE
eqa_form <- read.sas7bdat("data/168719 eqa form.sas7bdat")

# Initialize flag and cycle counter at 0
flag <- 0
cyc <- 0

# Iterate until convergence is reached
while (flag == 0)
{
  
  cyc <- cyc + 1

}

if (cyc == 1) {
  results$PRM_EVM <- round(results$m2,.000000001)
  
} else if (cyc = 2) {
  
} else if (cyc = 3) {
  
}




output <- tibble('id', 'cycl','nit', 'targ', 'tot', 'datd2', 'f18amult', 'f18bmult')
