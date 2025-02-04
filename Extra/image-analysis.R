# Do object recognition on the  images

library(keras) # install_keras()

# Check where Python's looked for
reticulate::py_config() 
# https://stackoverflow.com/a/46820701/1036500
model <- application_xception(weights = "imagenet")
# pip install pip-system-certs
# for certificate error: https://stackoverflow.com/a/58525755/1036500
# I got an error the first time, then downloaded the model file an 
# put this in ~/.keras/models : "inception_v3_weights_tf_dim_ordering_tf_kernels.h5"
# get a list of the cropped images
imgs_cropped <- list.files("Extra/images", full.names = TRUE)
# loop over all  images and get a list of the ten highest 
# probability objects recognised in each image
output_list <- vector("list", length = length(imgs_cropped))
for(i in seq_along(imgs_cropped)){
  f <- paste0(getwd(), "/", imgs_cropped[i])
  # load the image
  img_path <- f
  img <- image_load(img_path, target_size = c(299,299))
  x <- image_to_array(img)
  # ensure we have a 4d tensor with single element in the batch dimension,
  # the preprocess the input for prediction using resnet50
  x <- array_reshape(x, c(1, dim(x)))
  x <- xception_preprocess_input(x)
  # make predictions then decode and print them
  preds <- model %>% predict(x)
  output_list[[i]] <- imagenet_decode_predictions(preds, top = 20)[[1]]
}
# tidy output into data frame
library(tidyverse)
wide_out <-
  bind_rows(output_list,
            .id = "image")  %>%
  select(-class_name) %>%
  pivot_wider(image,
              names_from = "class_description",
              values_from = "score",
              values_fill = 0)


# Now we have attributes for each image, in the form of the objects recognised
# in them, we can see how images with similar objects cluster together. We use
# t-SNE for reducing the dimensionality, and then plot the images to see how 
# groupings appear. 


library(Rtsne)
set.seed(42)
tsne_out <-
  Rtsne(wide_out,
        pca=TRUE,
        perplexity=30,
        theta=0.0) # Run TSNE
tsne_out_plot <-
  tibble(x = tsne_out$Y[,1],
         y = tsne_out$Y[,2],
         img = paste0(getwd(), "/",  imgs_cropped)
  )

library(ggimage)
ggplot(tsne_out_plot %>% 
         slice_sample(n = 30)) +
  aes(x, y) +
  geom_image(aes(image=img),
             size=.025) +
  theme_void()
