# Creating an estimated internet use by datazone variable from
# Output area estimated internet use

library(tidyverse)
library(here)
library(haven)
library(stringr)


# notes -------------------------------------------------------------------

# for internet use data see
# http://journals.sagepub.com/doi/full/10.1177/0894439317693332
# and http://reshare.ukdataservice.ac.uk/851760/

# Output Area population comes from the census
# at http://www.scotlandscensus.gov.uk/ods-web/data-warehouse.html#standarddatatab
# this .zip file is 500MB though so be careful when downloading of this!
# We want the file KS101SC.csv within this

internet_use <- haven::read_dta("http://reshare.ukdataservice.ac.uk/851760/1/internet_use_esrc.dta")

dz_lookup <- readr::read_csv("https://www2.gov.scot/Resource/0046/00462936.csv")

output_area_pop <- read_csv(here("data", "KS101SC.csv"), skip = 4, na = "-")


# smell test --------------------------------------------------------------

tail(internet_use)

# looks like the cases of interest will be "SXXXXXXXX"

scot_oas <- 
  internet_use %>% 
  filter(str_detect(oa11, "S")) %>% 
  mutate(OutputArea = as.character(oa11))

output_area_pop <- 
  output_area_pop %>% 
  rename(OutputArea = X1,
         usual_resident_pop = `All people`) %>% 
  select(OutputArea, usual_resident_pop) %>% 
  filter(OutputArea != "Scotland")


# sync with datazones to aggregate ----------------------------------------


oa_internet <- left_join(dz_lookup, scot_oas, by = "OutputArea")

oa_internet <- 
  oa_internet %>% 
  rename(Data_Zone = DataZone) %>% 
  select(1:4, 6:9)



# aggregating to datazone -------------------------------------------------

oa_internet <- left_join(oa_internet, output_area_pop, by = "OutputArea")


dz_internet_est <- 
  oa_internet %>% 
  select(OutputArea, Data_Zone, pusenet, usual_resident_pop) %>% 
  arrange(Data_Zone) %>% 
  group_by(Data_Zone) %>% 
  summarise(est_internet_use = weighted.mean(pusenet, usual_resident_pop),
            usual_pop = sum(usual_resident_pop))




# dir.create(here("data", "derived"))

write_csv(dz_internet_est,
          path = here("data", "derived", "dz_internet_est.csv"))
