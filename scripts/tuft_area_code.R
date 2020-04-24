library(dplyr)#imports data wrangling package 
library(tidyr)#imports data wrangling package
library(ggplot2)#imports plot making package
library(tidyverse)#imports data wrangling package
library(lme4)
library(lmerTest)
library(ggeffects)
library(readxl)

<<<<<<< HEAD
inflatus = readxl::read_excel("data/tuft_area_inflatus.xlsx")
insignis = readxl::read_excel("data/tuft_area_insignis.xlsx")
coulteri = readxl::read_excel("data/tuft_area_coulteri.xlsx")
=======
inflatus = read_excel("data/tuft_area_inflatus.xlsx", na = c("NA", "")) %>% 
  mutate(plant_id = as.character(plant_id)) # uploading data from R and putting it in the global environment 
insignis = read_excel("data/tuft_area_insignis.xlsx", na = c("NA", "")) %>%
  mutate(plant_id = as.character(plant_id))
coulteri = read_excel("data/tuft_area_coulteri.xlsx", na = c("NA", "")) %>% 
  mutate(plant_id = as.character(plant_id)) 
>>>>>>> 1fcc4711b7f947308acc98574ed4866a55d0ddd2

all_areas = bind_rows(inflatus, insignis, coulteri) %>% 
  unite(unique_id, c(plant_id, plant_number), sep = "_", remove = FALSE)

inflatus_herb_data = readxl::read_excel("data/c_inflatus_image_data.xlsx", na = c("NA", ""))
insignis_herb_data = readxl::read_excel("data/s_insignis_image_data.xlsx", na = c("NA", ""))
coulteri_herb_data = readxl::read_excel("data/c_coulteri_image_data.xlsx", na = c("NA", ""))

all_herb = bind_rows(inflatus_herb_data, insignis_herb_data, coulteri_herb_data) %>% 
  unite(unique_id, c(image, plant), sep = "_") %>%
  rename(herb_notes = notes)

# the other data we need to add is info from the xml files and maybe climate data?
herb_counts = read.csv("data/compiled_counts_for_tufts10Apr2020.csv") #imports herbarium counts

herb_counts$unique_id <- paste(herb_counts$specimen, herb_counts$individual, sep = "_")
herb_counts$reproductive_attempts<-rowSums(herb_counts[,c("bud","flwr","immFrt","fillFrt","infFrt","unfFrt","infFlwr","infImmFrt","infBud","unkRepStr")])
herb_counts$filled_proportion<-herb_counts$fillFrt/(herb_counts$unfFrt + herb_counts$fillFrt)

# Also pull in climate
clim = read_csv("data/climate_sep_apr.csv") %>% 
  rename(specimen = id)

all_tufts <- left_join(all_areas, all_herb) %>% 
  left_join(., herb_counts) %>%
  left_join(., clim) %>% 
  filter(!is.na(maintuft_area),
         !is.na(stem_diam_cm)) %>% 
  # Make a phenological stage variable
  mutate(pheno_stage = (fillFrt+immFrt+infFrt+unfFrt+infImmFrt)/reproductive_attempts) # 1 = late (all fruits), 0 = early (no fruits)

summary(all_tufts)
#make all tuft area variable
all_tufts<-all_tufts%>%
  mutate(all_tufts_area = select(., maintuft_area:tuft_7_area,tuft_8_area:tuft_14_area) %>% rowSums(na.rm = TRUE))

# Some specimens are missing their herbarium counts, Megan can troubleshoot this
setdiff(all_areas$plant_id, herb_counts$specimen) %>% sort()
# Some are missing climate data, 5 weren't georeferenced and others are outside the time range of the flint database.
setdiff(all_areas$plant_id, clim$specimen) %>% sort()


# First, visualize all your variables just to get a sense of how they're distributed:

hist(all_tufts$maintuft_area) # maybe need to log
hist(all_tufts$stem_height)
hist(all_tufts$stem_diam_cm) # probably ok
hist(all_tufts$reproductive_attempts) # this is a count, so when if it's a response ned to use a GLM with family = poisson
hist(all_tufts$pheno_stage)

all_tufts$log_maintuft_area <- log(all_tufts$maintuft_area + 1)
hist(all_tufts$log_maintuft_area)

# Q1: Does the proportion of fruits that are filled depend on tuft size?
# response: proportion filled fruits (bounded by 0, 1)
# predictor: tuft area

m1 = glm(filled_proportion ~ log_maintuft_area*species, data = all_tufts, family = binomial)
summary(m1)
plot(ggpredict(m1, terms = c("log_maintuft_area", "species")))


# Q2: Does the tuft size depend on how many fruits have been filled?
# response: tuft area
# predictor: proportion filled fruits (bounded by 0, 1)

m2 = lm(log_maintuft_area ~ filled_proportion*species, data = all_tufts)
summary(m2)
plot(ggpredict(m2, terms = c("maintuft_area", "species")))



#Q3 Do tufts get larger or smaller as phenology progresses?
m3 = lm(log_maintuft_area ~ pheno_stage*species + stem_diam_cm, data = all_tufts)
summary(m3)
plot(ggeffect(m3, terms = c("pheno_stage", "species")))
# Yes, for insignis only, interesting. As plant gets more advanced, tuft gets larger



#Q4 How does plant size affect tufts size?
#repsonse: main tuft area, explanatory= stem height
m4=lm(log_maintuft_area ~ stem_height, data = all_tufts)
summary(m4)
plot(ggpredict(m4, terms = c("stem_height", "species")))
#bigger plants have bigger tufts, but very small positive slope



#Q5 how does climate affect plant size?
#response: plant size, predictor: climate
m5=lm(stem_height ~ cwd_sep_apr, data = all_tufts)
summary(m5)
plot(ggpredict(m5, terms = c("cwd_sep_apr", "species")))
#small negative value. As CWD increases, plant size decreases


# Q6 Does climate's effect on plant size depend on species?
m6=lm(stem_height ~ species*cwd_sep_apr, data = all_tufts)
summary(m6)
#DIFFERENCEs between size and climate are insignificant, more significant when you look at it by species

#so just make size a main effect on the model
m7=lm(all_tufts_area ~ cwd_sep_apr*species + stem_height, data = all_tufts)
summary(m7)









# If climate's effect on plant size is not dependent on species, then plant size can be a main effect in the model
m8=lm(all_tufts_area ~ cwd_sep_apr + stem_height, data = all_tufts)
summary(m8)


#climate's affect on tuft area without stem height
m9=lm(all_tufts_area ~ cwd_sep_apr, data = all_tufts)
summary(m9)



#does tuft area trade off with reproductive attempts?
m10=lm(reproductive_attempts ~ log_maintuft_area + pheno_stage, data = all_tufts)
summary(m10)
#strong positive relationship between tuft area and number of reprodutive structures... so tuft area does not trade off with reproductive attempts

#does having a tuft increase fruit filling?
m11=lm(filled_proportion ~ log_maintuft_area, data = all_tufts)
summary(m11)
#no?
