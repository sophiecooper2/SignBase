library(tidyverse)
signbase_full <- read_csv("signBase_Version1.0.csv") 
signbase_full <- signbase_full %>% 
  filter(site_name != "Willendorf",
         site_name != "Riparo di Fontana Nuova",
         site_name != "Muralovka",
         site_name != "Shanidar Cave",
         site_name != "Hayonim Cave")


groups <- list("1" = c("Abri Pataud", "El Rascaño", 
                       "Fumane", "La Viña",
                       "Lhotka", "Pod Hradem",
                       "Riparo Bombrini", "Sirgenstein Cave",
                       "Šandalja II"),
               "2" = c("Aurignac", "Gargas",
                       "Grotte de la Verpillière I", "Hohlenstein-Stadel",
                       "Istállóskő Cave", "Mladeč",
                       "d'Engihoul"), 
               "3" = c("Grottes de Fonds-de-Forêt", "Kvasice",
                       "Labeko Koba", "Les Rois",
                       "Breitenbach", "Peskő Cave",
                       "Vindija Cave", "Wildscheuer"),
               "4" = c("Le Trou Du Renard", "Trou Magrite", 
                       "Gatzarria", "Bockstein-Törle",
                       "Grotte De La Princesse Pauline", "Abri Lartet/Gorge d'Enfer",
                       "Tuto de Camalhot", "Slatinice"),
               "5" = c("Spy", "Solutré",
                       "Nová Dědina", "Grotte de Goyet",
                       "Brassempouy",  "El Castillo",
                       "Menton/Grottes du Grimaldi", "Geissenklösterle",
                       "El Salitre", "Göpfelsteinhöhle",
                       "Vogelherd"),
               "6" = c("Castanet", "Maisières-Canal",
                       "Grotte du Renne", "Hohle Fels",
                       "Žlutava", "Cellier",
                       "Blanchard", "La Ferrassie"),
               "7" = c("La Souquette", "Shelter Birów IV",
                       "Hornos de la Pena", "Trou al'Wesse",
                       "Les Cottés", "Le Terme Pialat",
                       "Abri de Laussel", "Abri du Poisson",
                       "Otaslavice"))
  
df <- 
  enframe(groups, 
          name = "group", 
          value = "site_name") %>%
  unnest(cols = site_name)

# assign groups to signbase data
signbase_unique_groups <- 
  signbase_full %>% group_by(site_name) %>% 
  summarize(across(where(is.numeric), sum)) %>% 
  left_join(df)


           
      
