library(tidyverse)
library(magrittr)
library(ggpubr)

abundance_data_full <-   signbase_full_clean %>% 
  pivot_longer(cols= line:star,
               values_to = "sign_total") %>% 
  filter(sign_total != 0,
        object_type != "undetermined") %>% 
  left_join(group_df, join_by(site_name)) %>% 
  group_by(site_name) %>% 
  mutate(object_abundance = n()) %>% 
  mutate(object_count = 1)

##plot object abundance by sign count

ggplot(abundance_data_full) +
  aes(x = object_type, y = sign_total, fill = name, group) +
  geom_col() +
  rotate_x_text(angle = 90, hjust = NULL, vjust = NULL)

##plot object type by sign type

ggplot(abundance_data_full) +
  aes(x = object_type) +
  geom_bar() +
  facet_wrap(~name) +
  rotate_x_text(angle = 90, hjust = NULL, vjust = NULL)

##plot sign type by object type

ggplot(abundance_data_full) +
  aes(x = name) +
  geom_bar() +
  facet_wrap(~object_type) +
  rotate_x_text(angle = 90, hjust = NULL, vjust = NULL)

##plot object type by site
ggplot(abundance_data_full) +
  aes(x = object_type, fill = name) +
  geom_bar() +
  facet_wrap(~site_name, scales = "free_y") +
  rotate_x_text(angle = 90, hjust = NULL, vjust = NULL)


##plot object type by group
ggplot(abundance_data_full) +
  aes(x = object_type, fill = name) +
  geom_bar() +
  facet_wrap(~group) +
  rotate_x_text(angle = 90, hjust = NULL, vjust = NULL)


