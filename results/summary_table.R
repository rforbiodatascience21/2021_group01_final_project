
Data <- read_tsv(file = "data/03_data_aug.tsv.gz", col_types = cols(
  Age = col_double(),
  Resting_blood_pressure = col_double(),
  Serum_cholestoral = col_double(),
  Maximum_heart_rate_achieved = col_double(),
  ST_depression_induced_by_exercise = col_double()))

# summary overview of attributes
Data %>% 
  select(Age, 
         Resting_blood_pressure, 
         Serum_cholestoral,
         Maximum_heart_rate_achieved,
         ST_depression_induced_by_exercise) %>% 
  summarise_all(funs(mean, sd, min,max),na.rm = TRUE) %>% 
  gather(key = key, value = value) %>% 
  separate(key, into = c("type", "stat"), sep = "_(?=[^_]+$)") %>% 
  spread(key = stat, value = value) %>% 
  mutate("mean w. sd" = paste0(round(mean, 2), " (", intToUtf8("177"), round(sd, 2), ")")) %>% 
  select(type, 'mean w. sd', min, max) 
