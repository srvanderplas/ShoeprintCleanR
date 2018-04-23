library(imager)
suppressPackageStartupMessages(library(tidyverse))
library(ShoeprintCleanR)


pic_paths <- list.files("/storage/CSAFE/Test Shoes/Prints/", 
                        full.names = T, 
                        pattern = "(\\d{1,}[LR]_\\d{8}_\\d_\\d_\\d_csafe_[a-z]{1,}).tif$")

pic_paths <- pic_paths[str_detect(pic_paths, "6_[12]_.")]

process_shoe <- function(i) {
  print(i)
  shoe <- load.image(i) %>%
    quantize_colors(8) %>%
    remove_print_label(thr = "10%") %>%
    remove_local_background() %>%
    quantize_colors(4) %>%
    remove_border_lines(maxiter = 10) %>%
    crop_border(axis = "xy", tol = .1)
    pad(10, "xy", c(-1, 1), val = 255) %>%
    align_shoe_print()  %>%
    crop_border(axis = "xy")
  
  if (width(shoe) < 10) {
    print("Shoe process not wide enough")
    return()
  } 
  if (height(shoe) < 200) {
    print("Shoe process not tall enough")
    return()
  }
  
  filename <- str_replace(i, ".tif", "-tif-shoeonly.jpg") %>%
    str_replace("Prints", "Prints/Edited")
  save.image(shoe, file = filename, quality = 1)
  
  rm(shoe)
  gc()
}


