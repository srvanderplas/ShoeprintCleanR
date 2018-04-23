library(imager)
suppressPackageStartupMessages(library(tidyverse))
library(ShoeprintCleanR)


pic_paths <- list.files("inst/localdata", 
                        full.names = T, 
                        pattern = "(\\d{1,}[LR]_\\d{8}_\\d_\\d_\\d_csafe_[a-z]{1,}).tif$")

pic_paths <- pic_paths[str_detect(pic_paths, "6_[12]_.")]

save_path <- "inst/cleandata/"

process_shoe <- function(i) {
  print(i)
  shoe <- load.image(i) %>%
    quantize_colors(8) %>%
    remove_print_label(thr = "10%") %>%
    remove_local_background() %>%
    quantize_colors(4) %>%
    remove_border_lines(maxiter = 10) %>%
    crop_border(axis = "xy", tol = .1) %>%
    pad(nPix = 10, axes = "xy", pos = -1, val = 0) %>%
    pad(nPix = 10, axes = "xy", pos = 1, val = 0) %>%
    bucketfill(1, 1, 1, color = 255) %>%
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
    str_replace("inst/localdata/", save_path)
  
  save.image(shoe, file = filename, quality = 1)
  
  rm(shoe)
  gc()
}


