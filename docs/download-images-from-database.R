
library(tidyverse)
library(rvest)
library(httr)

signbase_full <- read_csv("data/signBase_Version1.0.csv")

# find objects with hatching or grid
sb_grid_hatch <- 
signbase_full %>% 
  filter(notch == 1)

# urls to each object data webpage
urls_object_webpage <- 
  paste0("https://www.signbase.org/artifacts/",
         sb_grid_hatch$access_id)

# get images of objects with hatching or grid
img_urls <- 
  map(
  urls_object_webpage,
  ~.x %>%
  read_html() %>% 
  html_nodes("img.img-fluid.img-thumbnail") %>%  
  html_attr("src")
  )

# add to data frame so we have urls together with other data
# about the objects
sb_grid_hatch$img_urls  <- unlist(img_urls)

# drop objects with no image
sb_grid_hatch_img <- 
sb_grid_hatch %>% 
  filter(img_urls != "../../static/images/signbase_logo.png" )

for(i in 1:length(sb_grid_hatch_img$img_urls)){
  
  # work on this image
  img_url <- sb_grid_hatch_img$img_urls[i]
  
  # download the image with an informative 
  # filename
if (!is.na(img_url)) {
  img <- GET(img_url)
  writeBin(content(img, "raw"), 
           paste0("Extra/images/signbase-", 
                  sb_grid_hatch_img$object_id[i], 
           ".jpg"))  
           }
}
  
