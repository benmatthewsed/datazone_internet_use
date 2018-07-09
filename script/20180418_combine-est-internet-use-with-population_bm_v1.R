# 


library(tidyverse)
library(here)
library(haven)
library(stringr)
library(digest)


# notes -------------------------------------------------------------------

# for internet use data see
# http://journals.sagepub.com/doi/full/10.1177/0894439317693332
# and http://reshare.ukdataservice.ac.uk/851760/



internet_use <- haven::read_dta("http://reshare.ukdataservice.ac.uk/851760/1/internet_use_esrc.dta")


# settings ----------------------------------------------------------------

ggplot2::theme_set(theme_bw())

# load data ---------------------------------------------------------------

# internet_use <- haven::read_dta(here("data", "internet_use_esrc.dta"))
# or just read in straight from the url

internet_use <- haven::read_dta("http://reshare.ukdataservice.ac.uk/851760/1/internet_use_esrc.dta")


digest(internet_use, "md5")

dz_lookup <- read_csv(here("data", "00462936.csv"))

simd <- readxl::read_xlsx(here("data", "00510566.xlsx"), sheet = 3, na = "*")

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

# I probably need to include SIMD here to do a weighted average

# wrong! I need Output Area population for this. No worries though I can get
# this from census http://www.scotlandscensus.gov.uk/ods-web/data-warehouse.html#standarddatatab


oa_internet <- left_join(oa_internet, output_area_pop, by = "OutputArea")


dz_internet_est <- 
oa_internet %>% 
  select(OutputArea, Data_Zone, pusenet, usual_resident_pop) %>% 
  arrange(Data_Zone) %>% 
  group_by(Data_Zone) %>% 
  summarise(est_internet_use = weighted.mean(pusenet, usual_resident_pop),
            usual_pop = sum(usual_resident_pop))


#dz_internet_est is basically the dataset we'd want to import into the Safe Haven
# I think

# dir.create(here("data", "derived"))

write_csv(dz_internet_est,
          path = here("data", "derived", "dz_internet_est.csv"))

# analysing dz internet use -----------------------------------------------

dz_simd <- left_join(simd, dz_internet_est, by = "Data_Zone")

# correlation with total population and usual_pop

dz_simd %>% 
  ggplot(aes(x = Total_population, y = usual_pop)) +
  geom_point() +
  geom_smooth(method = "lm")

summary(lm(Total_population ~ usual_pop, data = dz_simd))

sqrt(0.8659)

# correlation is sqrt(0.8659) -> 0.9305375
# discrepancies could be to do with the difference in measurement occasion
# 2011 vs ...2016?


dz_simd %>% 
  filter(usual_pop < 1000 & Total_population > 1500) %>% 
  select(1:4, usual_pop)

#internet use by council

dz_simd %>% 
  group_by(Council_area) %>% 
  summarise(internet = mean(est_internet_use)) %>% 
  arrange(desc(internet))

# when sending to .fun you don't write mean(x) you do mean, x

dz_simd %>% 
  ggplot(aes(x = est_internet_use, fill = fct_reorder(Council_area, .fun = mean, est_internet_use))) +
  geom_density(alpha = 0.2) +
  facet_wrap(~ fct_reorder(Council_area, .fun = mean, est_internet_use))



# relationship between internet use and SIMD domains ----------------------

# read in simd indicator data

simd_ranks <- readxl::read_xlsx(here("data", "00510565.xlsx"), sheet = 2, na = "*")

simd_ranks <- 
simd_ranks %>% 
  select(1, 6:13)

dz_simd_ranks <- left_join(dz_simd, simd_ranks, by = "Data_Zone")

dz_simd_ranks %>% 
  select(Data_Zone, est_internet_use, Overall_SIMD16_rank:Crime_domain_2016_rank) %>% 
  gather("simd_domain", "rank", Overall_SIMD16_rank:Crime_domain_2016_rank) %>% 
  ggplot(aes(x = est_internet_use, y = rank, colour = simd_domain)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm") +
  facet_wrap(~ simd_domain)


# internet use is -vely correlated with access (weakly), wekly +vley correlated with
# housing and crime and pretty strongly +vely correlated with educatino, 
# employment, health, income and overall simd
