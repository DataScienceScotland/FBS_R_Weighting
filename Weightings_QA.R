library(tidyverse)
#Weightings QA
datayear=2022
prevyear=datayear-1
#Read in weight files
Z_drive_path <- Sys.getenv("Z_drive_path")
setwd(paste0(Z_drive_path,"prod",datayear,"/FBS_R_Weighting"))
new_curr_weights <- read.csv(paste0('Outputs/FBSWT',datayear,'.csv')) %>% 
  mutate(f_number = floor(fa_id/10000))
new_prev_weights <- read.csv(paste0('Outputs/FBSWT',prevyear,'.csv')) %>% 
mutate(f_number = floor(fa_id/10000))
old_prev_weights <- read.csv(paste0('../../prod',prevyear,'/FBS_R_Weighting/Outputs/FBSWT',prevyear,'.csv')) %>% 
mutate(f_number = floor(fa_id/10000))

#Compare new_curr_weights with new_prev_weights
compare_old_new <- new_curr_weights %>% 
  inner_join(new_prev_weights, 
             by = 'f_number',
             suffix = c("_datayear", "_prevyear")) %>% 
  mutate(ratio = fbswt_datayear/fbswt_prevyear)
         

compare_old_new$abs_ratio <- ifelse(compare_old_new$ratio <1, 1/compare_old_new$ratio, compare_old_new$ratio)


##Add markers for type/tenure changes
compare_old_new$type_change <- ifelse(compare_old_new$type_datayear==compare_old_new$type_prevyear,0,1)
compare_old_new$tenure_change <- ifelse(compare_old_new$tenure_datayear==compare_old_new$tenure_prevyear,0,1)
compare_old_new$typetenure_change <- ifelse(compare_old_new$type_change==0 & compare_old_new$tenure_change==0, 0, 1)

##Remove typetenure changes and take top 20 changes, for closer inspection
compare_old_new_static <- compare_old_new %>% 
  filter(typetenure_change==0) 
compare_old_new_top <- compare_old_new_static[order(compare_old_new_static$abs_ratio, decreasing = TRUE), ] %>%
  slice(1:20)

#Summary by farmtype
Summary_old <- new_prev_weights %>% 
  group_by(type) %>% 
  summarise(count_old = n(),
            avg_weight_old = mean(fbswt))
Summary_new <- new_curr_weights %>% 
  group_by(type) %>% 
  summarise(count_new = n(),
            avg_weight_new = mean(fbswt))
Old_new_farmtype_summary <- Summary_new %>% 
  left_join(Summary_old, by='type') %>% 
  mutate(count_ratio = count_new/count_old,
         weight_ratio = avg_weight_old/avg_weight_new)

#Summary by tenure
Summary_ten_old <- new_prev_weights %>% 
  group_by(tenure) %>% 
  summarise(count_old = n(),
            avg_weight_old = mean(fbswt))
Summary_ten_new <- new_curr_weights %>% 
  group_by(tenure) %>% 
  summarise(count_new = n(),
            avg_weight_new = mean(fbswt))
Old_new_tenure_summary <- Summary_ten_new %>% 
  left_join(Summary_ten_old, by='tenure') %>% 
  mutate(count_ratio = count_new/count_old,
         weight_ratio = avg_weight_old/avg_weight_new)

##graphs
ggplot(compare_old_new, aes(x=fbswt_datayear, y=fbswt_prevyear, colour=as.factor(typetenure_change))) +
  geom_point()
ggplot(compare_old_new[compare_old_new$typetenure_change==0, ], aes(x=fbswt_datayear, y=fbswt_prevyear, colour=type_datayear)) +
  geom_point()
ggplot(compare_old_new[compare_old_new$typetenure_change==1, ], aes(x=fbswt_datayear, y=fbswt_prevyear, colour=type_datayear)) +
  geom_point()
ggplot(compare_old_new, aes(x=fbswt_datayear, y=abs_ratio, colour = as.factor(typetenure_change)))+
  geom_point()

##Output tables for QA document
Biggest_changes <- compare_old_new[order(compare_old_new$abs_ratio, decreasing = TRUE), ]  %>% 
  filter(abs_ratio > 1.5) %>% 
  slice(1:20) %>% 
  mutate(fbswt_datayear = round(fbswt_datayear, 1),
         fbswt_prevyear = round(fbswt_prevyear, 1),
         ratio = round(ratio, 2),
         abs_ratio = round(abs_ratio, 2),
         typetenure_change = as.logical(typetenure_change)) %>%
  select(Farm = f_number,  
         'Previous weight' = fbswt_prevyear, 'Current weight' = fbswt_datayear, 
         Ratio = ratio, Factor = abs_ratio, 'Type/tenure change?' = typetenure_change) 
Biggest_changes_static <- compare_old_new_static[order(compare_old_new_static$abs_ratio, decreasing = TRUE), ]  %>% 
  filter(abs_ratio > 1.2) %>% 
  slice(1:20) %>% 
  mutate(fbswt_datayear = round(fbswt_datayear, 1),
         fbswt_prevyear = round(fbswt_prevyear, 1),
         ratio = round(ratio, 2),
         abs_ratio = round(abs_ratio, 2),
         typetenure_change = as.logical(typetenure_change)) %>%
  select(Farm = f_number,  
         'Previous weight' = fbswt_prevyear, 'Current weight' = fbswt_datayear,
         'Type' = type_datayear, 'tenure' = tenure_datayear,
         Ratio = ratio, Factor = abs_ratio) 

#Compare new_prev_weights with old_prev_weights
compare_prev_prev <- new_prev_weights %>% 
  inner_join(old_prev_weights, 
             by = 'f_number',
             suffix = c("_new", "_old")) %>% 
  mutate(ratio = fbswt_new/fbswt_old)

compare_prev_prev$abs_ratio <- ifelse(compare_prev_prev$ratio <1, 1/compare_prev_prev$ratio, compare_prev_prev$ratio)


# types <- Summary_new %>% 
#   select(type)
# tenures <- Summary_ten_new %>% 
#   select(tenure)
# categories <- data.frame(category = colnames(select(new_curr_weights, c(barley_hct:Ewes))))
#   
# types_tenures_cats <- types %>% 
#   merge(tenures) %>% 
#   merge(categories)

cf_census <- new_curr_weights %>% 
  group_by(Type = type, Tenure = tenure) %>% 
  summarise(across(c(barley_hct:Ewes), ~sum(.*fbswt)),
            Holdings = sum(fbswt)) 
cf_census <- cf_census[order(cf_census$Type, cf_census$Tenure), ]

cf_census_raw <- new_curr_weights %>% 
  group_by(Type = type, Tenure = tenure) %>% 
  summarise(across(c(barley_hct:Ewes), ~sum(.)),
            Holdings = n()) 
cf_census_raw <- cf_census_raw[order(cf_census_raw$Type, cf_census_raw$Tenure), ]

cf_census_totals <- cf_census %>% 
  group_by(Type) %>% 
  summarise(across(c(barley_hct:Holdings), ~sum(.))) %>% 
  mutate(tenure = 'All tenures')
cf_census_raw <- cf_census_raw[order(cf_census_raw$Type, cf_census_raw$Tenure), ]
  

# cf_census <- cf_census %>% 
#   bind_rows(cf_census_totals)

farmcensus <- read.csv('Inputs/farmcensus2021.csv') %>% 
  select(colnames(cf_census))
farmcensus <- farmcensus[order(farmcensus$Type, farmcensus$Tenure), ]

Ratio <- cf_census[-c(1:2)]/farmcensus[-c(1:2)]
Ratio[Ratio < 5 & Ratio > 0.2] <- 0
Ratio <- bind_cols(cf_census[c(1:2)], Ratio)


check <- new_curr_weights %>% 
  group_by(type, tenure) %>% 
  summarise(count = n())
  
  
##Check for instances where sample counts exceed census counts
Exceeds <- (cf_census_raw[-c(1:2)] - farmcensus[-c(1:2)])
Exceeds[Exceeds <= 0]=0
Exceeds <- bind_cols(cf_census_raw[c(1:2)], Exceeds)


check <- new_curr_weights %>% 
 filter(type=="Specialist Sheep (LFA) farms", tenure=="Part Occupier/Tenanted",
        pota_hct>0) 


compare_old_new_normalised <- compare_old_new_static %>% 
  left_join(Old_new_farmtype_summary, by=c("type_datayear"="type")) %>% 
  mutate(normalised_relative_weight_change = fbswt_datayear*count_ratio/fbswt_prevyear,
         fbswt_datayear_norm = fbswt_datayear * weight_ratio)

ggplot(compare_old_new_normalised, aes(x=fbswt_datayear_norm , y=fbswt_prevyear, colour=normalised_relative_weight_change)) +
  geom_point()


mean(compare_old_new_normalised$normalised_relative_weight_change)


new_sheep <- new_curr_weights %>% 
  filter(type=="Specialist Sheep (LFA) farms")

old_sheep <- old_prev_weights %>% 
  filter(type=="Specialist Sheep (LFA) farms")