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
  dplyr::select(-other, -rectangle, -circumline, -pinleft, -pinright, -star, -circumspiral, -circumnotch, -maccaroni, -obnotch, -hashtag, -anthropomorph) %>%
  filter(object_id != "laf0022",
       object_id != "gkl0025",
       object_id != "vhc0135",
       object_id != "sol0007",
       object_id != "gff0001",
       object_id != "cel0001")

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
  filter(MedianBP > 37500) %>% 
  filter(MedianBP < 42500)

artifact_data <- signbase_full_clean %>% 
  arrange(object_id) %>% 
  column_to_rownames("object_id") %>% 
  dplyr::select(line:concenline)

artifact_data <- artifact_data %>% 
  mutate(row_sums = rowSums(.)) %>% 
  filter(row_sums >= 2) %>% 
  dplyr::select(-row_sums)

x_df <- 
  artifact_data %>% 
  rownames_to_column("object_id") %>% 
  left_join(signbase_full_clean %>% 
              distinct(object_id, .keep_all = TRUE) %>% 
              select(object_id,
                     country),
            by = "object_id")

countries <- x_df$country

types <- list("1" = c("bla0005", "vhc0002", "vhc0022"),
               "2" = c("hss0002",  "hfc0046",  "hfc0044", "hfc0028","laf0005",
                       "laf0026",  "mla0003", "cas0009", "cas0014", "cas0001",
                       "cas0020", "cas0008", "cas0003", "cel0009", "vhc0060",
                       "vhc0084", "gzr0001", "gzr0002", "vhc0029", "vhc0026",
                       "vhc0157", "vhc0132", "vhc0046", "vhc0102", "mla0004"),
              "3" = c("vhc0107", "vhc0019", "hfc0025"),
              "4" = c("vhc0096", "vhc0094", "vhc0081", "vhc0112", "vhc0146",
                      "vhc0150", "vhc0049"),
              "5" = c("bla0001", "gzr0003", "bla0007"),
              "6" = c("spy0010", "cas0019", "spy0004", "vdj0001", "vhc0152"),
              "7" = c("cel0027", "cel0025", "laf0010", "cel0003", "cel0029",
                      "cel0020", "cel0021", "cel0014", "cel0019"),
              "8" = c( "spy0022","vhc0059", "vhc0015"),
              "9" = c("spy0006", "hfc0018", "spy0012"),
              "10" = c("bla0014", "hfc0003", "vhc0147", "vhc0087", "vhc0008"),
              "11" = c("spy0027", "vhc0065", "vhc0155", "vhc0052", "spy0011",
                       "spy0025"),
              "12" = c("hfc0005", "hfc0009", "gkl0028", "sol0006", "gkl0012",
                       "vhc0051",  "gkl0001"),
              "13" = c("spy0009","hfc0042", "vhc0043"),
              "14" = c("vhc0131", "hfc0043", "vhc0090"),
              "15" = c("spy0018", "vhc0005",  "hfc0004","vhc0145", "vhc0003" ),
              "16" = c("bla0019", "bla0015", "bla0016"),
              "17" = c("bla0008",  "laf0011", "laf0018"),
              "18" = c("gkl0011", "spy0026","spy0023")) 
type_df <- 
  enframe(types, 
          name = "type", 
          value = "object_id") %>%
  unnest(cols = object_id)


lat_long_df <- signbase_full_clean %>% 
  dplyr::select(object_id, longitude, latitude) %>% 
  distinct(object_id, .keep_all = TRUE)

artifact_type_df <- artifact_data %>% 
  rownames_to_column("object_id") %>% 
  left_join(type_df) %>% 
  left_join(lat_long_df) %>% 
  left_join((signbase_full_clean %>% 
              select(object_id, site_name, MedianBP)))

groups <- list("1" = c("Abri Pataud", "Riparo Bombrini", "Grottes de Fonds-de-Forêt"), 
               "2" = c("Gatzarria", "Grotte de la Verpillière I", "Hohlenstein-Stadel", "Mladeč"),
               "3" = c("Labeko Koba", "Vindija Cave"),
               "4" = c( "Geissenklösterle", "Hohle Fels", "Vogelherd", "Spy", "Solutré","Grotte du Renne"),
               "5" = c("Castanet",  "Blanchard", "La Ferrassie", "Cellier"))

group_df <- 
  enframe(groups, 
          name = "group", 
          value = "site_name") %>%
  unnest(cols = site_name)

artifact_type_df <- artifact_data %>% 
  rownames_to_column("object_id") %>% 
  left_join(type_df) %>% 
  left_join(lat_long_df) %>% 
  left_join((signbase_full_clean %>% 
               select(object_id, site_name, MedianBP))) %>% 
  left_join(group_df)

##plot site by artifact type

ggplot(artifact_type_df) +
  aes(x = group, fill = type) +
  geom_bar()



##37500 - 42500
dplyr::select(-other, -rectangle, -circumline, -pinleft, -pinright, -star, -circumspiral, -circumnotch, -maccaroni, -obnotch, -hashtag, -anthropomorph) %>% 
  filter(object_id != "laf0022",
       object_id != "gkl0025",
       object_id != "vhc0135",
       object_id != "sol0007",
       object_id != "gff0001",
       object_id != "cel0001")


## 36000 - 39000
dplyr::select(-other, -rectangle, - circumnotch, - zigzag, -hashtag, -maccaroni, -circumspiral, -anthropomorph, -concenline, -zigzagrow, -pinleft, -pinright)

