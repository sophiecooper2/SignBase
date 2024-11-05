library(tidyverse)
library(magrittr)


signbase_data <- read_csv("signBase_Version1.0.csv")
signbase_data_clean <- signbase_data %>% 
  filter(site_name != "Willendorf",
         site_name != "Riparo di Fontana Nuova",
         site_name != "Muralovka",
         site_name != "Shanidar Cave",
         site_name != "Hayonim Cave") %>% 
  select(-other)

signbase_years <- signbase_data_clean %>% 
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
         date_bp_max_error = parse_number(date_bp_max_error)) %>% 
  rownames_to_column("DateID")

library(rcarbon)
# summed probability distribution
spd_output <- 
  spd(calibrate(signbase_years$date_bp_max_age,
                signbase_years$date_bp_max_error),
      timeRange = c(50000,
                    10000))
plot(spd_output)

# calibrate the ages
signbase_years_cal <- 
  calibrate(signbase_years$date_bp_max_age,
            signbase_years$date_bp_max_error)

# investigate ranges of dates
summary(signbase_years_cal) %>% 
  filter(between(MedianBP, 41000, 44000)) %>% 
  nrow()

# histogram to visualise 
summary(signbase_years_cal) %>% 
  ggplot() +
  aes(MedianBP) +
  geom_histogram(binwidth = 100)

# extract time range, see what we get

signbase_years_cal_clean <- summary(signbase_years_cal) %>% 
  data_frame() %>%
  filter(MedianBP > 32500) %>% 
  filter(MedianBP < 42500)

signbase_years_clean <- signbase_years_cal_clean %>% 
  mutate(MedianBP = as.character(MedianBP)) %>% 
  select(DateID, MedianBP)

signbase_years_clean <- signbase_years_clean %>% 
  right_join(signbase_years) %>% 
  filter(!is.na(MedianBP))
signbase_years_clean <- signbase_years_clean %>% 
  distinct(site_name, .keep_all = TRUE)


##run a seriation on timerange

artifact_data <- signbase_years_clean %>% 
  column_to_rownames(var = "site_name") %>% 
  select(line:star)

library(ggpubr)
artifact_matrix <- as.matrix(artifact_data %>% 
                               mutate(across(everything(), ~replace(., . > 1, 1))))
rownames(artifact_matrix) <- rownames(artifact_data)


mplot <- function(x,...){
  d <- data.frame(x = rep(1:ncol(x), each = nrow(x)),
                  y = rep(nrow(x):1, ncol(x)),
                  z = as.integer(x))
  ggplot(d, aes(x, y, fill = factor(z))) +
    geom_tile(col = "grey50", linewidth = 0.5) +
    scale_fill_manual(values = c("0" = "white", "1" = "black")) +
    coord_equal(expand = FALSE) +
    scale_x_continuous(breaks = 1:ncol(x), 
                       labels = colnames(x),
                       position = "top") +
    scale_y_continuous(breaks = 1:nrow(x), 
                       labels = rev(rownames(x))) +
    theme(axis.text = element_text(size = rel(0.5)),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0.25)) +
    labs(x = "Sign Type",
         y = "Site Name",
         fill = "Count") +
    rotate_x_text(angle = 90, hjust = NULL, vjust = NULL)}


increase_focus <- function(x){
  mcp <- apply(
    x, 
    MARGIN = 1, 
    FUN = \(z){ mean(which(z == 1)) }, 
    simplify = FALSE
  ) |> unlist()
  x <- x[order(mcp), ]
  mrp <- apply(
    x, 
    MARGIN = 2, 
    FUN = \(z){ mean(which(z == 1)) }, 
    simplify = FALSE) |> unlist()
  x[, order(mrp)]}

concentrate <- function(x){
  old <- x
  not_identical <- TRUE
  while(not_identical){
    new <- increase_focus(old)
    not_identical <- !identical(old, new)
    old <- new}
  new}

artifact_matrix |> concentrate() |> mplot()
