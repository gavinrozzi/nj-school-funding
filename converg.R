##############################################################
# R implementation of the New Jersey School Funding Formula  #
# Gavin Rozzi                                                #
##############################################################


# Load required libraries
library(tidyverse)


# Read in some data

# Use the 2021 data file just for the current equalized valuation
taxdata <- read_csv('data/2021/esttax21_minus2.csv') %>%
  select(district_id,district_name,estrwo_eev) %>%
  rename('equalized_valuation' = estrwo_eev)

# Revenue by district from NJ DOE. Filtered just to line items for aid payments.
revdata <- read_csv('data/rev21_minus2.csv') %>% 
  filter(grepl('Aid',line_desc))


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
  
} else if (cyc = 2) {
  
}




output <- tibble('id', 'cycl','nit', 'targ', 'tot', 'datd2', 'f18amult', 'f18bmult')
