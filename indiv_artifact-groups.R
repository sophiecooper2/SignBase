library(tidyverse)
library(magrittr)
library(rcarbon)
signbase_full <- read_csv("data/signBase_Version1.0.csv")

signbase_full_clean <- signbase_full %>% 
  filter(site_name != "Willendorf",
         site_name != "Riparo di Fontana Nuova",
         site_name != "Muralovka",
         site_name != "Shanidar Cave",
         site_name != "Hayonim Cave",
         site_name != "El Salitre",
         site_name != "Grotte De La Princesse Pauline",
         site_name != "Šandalja II") %>% 
  dplyr::select(-other, -rectangle, - circumnotch, - zigzag, -hashtag, -maccaroni, -circumspiral, -anthropomorph, -concenline, -zigzagrow, -pinleft, -pinright)


signbase_years <- signbase_full_clean %>% 
  drop_na(date_bp_max_min) %>% 
  mutate(date_bp_max_min = str_replace_all(date_bp_max_min, "\\+\\/\\-", "±")) %>% 
  mutate(date_bp_max_min = str_replace_all(date_bp_max_min, "\\+", "±")) %>% 
  separate(date_bp_max_min,
           sep = " - ",
           c("date_bp_max", "date_bp_min"),
           remove = FALSE) %>% 
  mutate(date_bp_max = ifelse(str_detect(date_bp_max, "\\/"),
                              str_extract(date_bp_max, ".*?(?=\\/)"),
                              date_bp_max)) %>% 
  mutate(date_bp_min = ifelse(str_detect(date_bp_min, "\\/"),
                              str_extract(date_bp_min, ".*?(?=\\/)"),
                              date_bp_min)) %>% 
  separate(date_bp_max,
           sep = "±",
           c("date_bp_max_age",
             "date_bp_max_error")) %>% 
  separate(date_bp_min,
           sep = "±",
           c("date_bp_min_age",
             "date_bp_min_error")) %>% 
  drop_na(date_bp_max_age,
          date_bp_max_error) %>% 
  mutate(date_bp_max_age = parse_number(date_bp_max_age),
         date_bp_max_error = parse_number(date_bp_max_error))
signbase_years_cal <- 
  rcarbon::calibrate(signbase_years$date_bp_max_age,
                     signbase_years$date_bp_max_error,
                     verbose = FALSE) %>% 
  summary() %>% 
  data_frame()

signbase_years_cal$object_id <- signbase_years$object_id

signbase_full_clean <- inner_join(signbase_full_clean, signbase_years_cal)

signbase_full_clean <- signbase_full_clean %>% 
  filter(MedianBP > 36000) %>% 
  filter(MedianBP < 39000)


artifact_data <- signbase_full_clean %>% 
  column_to_rownames("site_name") %>% 
  dplyr::select(line:star)

artifact_data <- artifact_data %>% 
  mutate(row_sums = rowSums(.)) %>% 
  filter(row_sums >= 2) %>% 
  dplyr::select(-row_sums)




##37500 - 42500
dplyr::select(-other, -rectangle, -circumline, -pinleft, -pinright, -star, -circumspiral, -circumnotch, -maccaroni, -obnotch, -hashtag, -anthropomorph)
filter(object_id != "laf0022",
       object_id != "gkl0025",
       object_id != "vhc0135",
       object_id != "sol0007",
       object_id != "gff0001",
       object_id != "cel0001")



