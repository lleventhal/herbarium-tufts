# Generate climate averages

library(tidyverse)

locs = read_csv("data/geos_for_tufts.csv")

dat = read_csv("data/flintbcm_climate_tall.csv") %>% 
  left_join(., locs) %>% 
  filter(!is.na(decimalLatitude)) %>% 
  # comment comment
  mutate(tave = (tmin +tmax)/2) %>% 
  filter((year == clim_year & clim_month <= 4) | (clim_year == year-1 & clim_month >= 9)) %>% 
  group_by(id) %>% 
  summarize(cwd_sep_apr = sum(cwd), temp_sep_apr =mean(tave), ppt_sep_apr = sum(ppt_mm))

# adding stuff making changes jigfojreajgoeanrafkmewiakom
write.csv(dat, "data/climate_sep_apr.csv", row.names = FALSE)

m1 = glm(filled_proportion ~ log_maintuft_area*species, data = all_tufts, family = binomial)
summary(m1)
plot(ggpredict(m1, terms = c("log_maintuft_area", "species")))
m1 = glm(filled_proportion ~ log_maintuft_area*species, data = all_tufts, family = binomial)
summary(m1)
plot(ggpredict(m1, terms = c("log_maintuft_area", "species")))
m1 = glm(filled_proportion ~ log_maintuft_area*species, data = all_tufts, family = binomial)
summary(m1)
plot(ggpredict(m1, terms = c("log_maintuft_area", "species")))

