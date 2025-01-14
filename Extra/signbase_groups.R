jac_df <- as_tibble(dm)
rownames(jac_df) <- rownames(artifact_data)
jac_df <- jac_df %>% 
  rownames_to_column("site_name") %>% 
  pivot_longer(cols = `Abri Lartet/Gorge d'Enfer`: Žlutava) %>% 
  filter(value < 0.6)


pia <- jac_df %>% 
  filter(site_name =="Nová Dědina")

deng <- 

groups <- list("1" = c("Abri Pataud", "El Rascaño", 
                         "Fumane", "La Viña",
                         "Lhotka", "Pod Hradem",
                         "Riparo Bombrini", "Sirgenstein Cave",
                         "Šandalja II"),
                "2" = c("Kvasice", "Labeko Koba", 
                        "Les Rois", "Slatinice",
                        "Göpfelsteinhöhle"),
                "3" = c("Aurignac", "d'Engihoul",
                         "Istállóskő Cave", "Hohlenstein-Stadel",
                         "Gargas", "Grotte de la Verpillière I", 
                         "Mladeč", "Brassempouy",
                         "Gatzarria"),
                "4" = c("Wildscheuer", "Breitenbach",
                        "Peskő Cave", "Vindija Cave",
                        "El Castillo"),
                "5" = c("Tuto de Camalhot","Abri Lartet/Gorge d'Enfer",
                        "Castanet", "Le Trou Du Renard", 
                        "Grottes de Fonds-de-Forêt"),
                "6" = c("Nová Dědina", "Menton/Grottes du Grimaldi",
                        "Grotte du Renne", "Solutré", 
                        "Žlutava", "Spy"),
                "7" = c("Trou Magrite", "Grotte De La Princesse Pauline",
                        "Grotte de Goyet", "Bockstein-Törle"),
                "8" = c("Trou al'Wesse", "Maisières-Canal",
                        "Les Cottés", "Le Terme Pialat"),
                "9" = c("Vogelherd","Hohle Fels",
                        "Geissenklösterle"),
                "10" = c("Cellier", "Blanchard", 
                         "La Ferrassie", "Hornos de la Pena",
                         "Otaslavice"),
                "11" = c("La Souquette", "Shelter Birów IV",
                         "Abri de Laussel", "Abri du Poisson"))
  
