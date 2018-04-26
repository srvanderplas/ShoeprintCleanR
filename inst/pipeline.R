library(imager)
suppressPackageStartupMessages(library(tidyverse))
library(ShoeprintCleanR)
library(future.apply)
library(parallel)
library(tools)

pic_paths <- list.files("inst/localdata", 
                        full.names = T, 
                        pattern = "(\\d{1,}[LR]_\\d{8}_\\d_\\d_\\d_csafe_[a-z]{1,}).tif$")

pic_paths <- pic_paths[str_detect(pic_paths, "5_[12]_.")]

save_path <- "inst/cleandata/"
# # For paper prints (e.g. 6_[12]_.)
# process_shoe <- function(i) {
#   print(i)
#   shoe <- load.image(i) %>%
#     quantize_colors(8) %>%
#     remove_print_label(thr = "10%") %>%
#     crop_border(axis = "xy", tol = .001, sigma = 30) %>%
#     remove_local_background(n = 5) %>%
#     quantize_colors(4) %>%
#     remove_border_lines(maxiter = 10) %>%
#     crop_border(axis = "xy", tol = .1) %>%
#     align_shoe_print()  %>%
#     crop_border(axis = "xy")
#   
#   if (width(shoe) < 10) {
#     print("Shoe process not wide enough")
#     return()
#   } 
#   if (height(shoe) < 200) {
#     print("Shoe process not tall enough")
#     return()
#   }
#   
#   filename <- str_replace(i, ".tif", "-tif-shoeonly-cropfirst.jpg") %>%
#     str_replace("inst/localdata/", save_path)
#   
#   save.image(shoe, file = filename, quality = 1)
#   
#   tmp <- dim(shoe)
#   # return(shoe)
#   rm(shoe)
#   gc()
#   tmp
# }

# Process film prints
process_shoe <- function(i) {
  shoe <- load.image(i) %>%
    renorm() 
  
  shoedens <- density(shoe)
  mode <- shoedens$x[which.max(shoedens$y)] # Most common value
  
  shoe[shoe >= (mode*.95)] <- 255
  
  shoe <- shoe %>%
    quantize_colors(4) %>%
    crop_border(tol = .01, sigma = 80) %>%
    align_shoe_print()
  
  plot(shoe)
  
  filename <- str_replace(i, ".tif", "-tif-shoeonly2.jpg") %>%
    str_replace("inst/localdata/", save_path)
  
  save.image(shoe, file = filename, quality = 1)
  
  tmp <- dim(shoe)
  # return(shoe)
  rm(shoe)
  gc()
  tmp
}


# process_shoe(pic_paths[1])


setup_cluster <- function(){
  # Hack to kill future workers
  # https://github.com/HenrikBengtsson/future/issues/93
  cl <- makeClusterPSOCK(availableCores())
  
  # Get PIDs
  for (kk in seq_along(cl)) {
    parallel:::sendCall(cl[[kk]], fun = Sys.getpid, args = list())
    pid <- parallel:::recvResult(cl[[kk]])
    attr(cl[[kk]]$host, "pid") <- pid
  }
  
  plan(cluster, workers = cl)
  
  return(cl)
}

kill_cluster <- function(cl){
  # Kill child processes
  lapply(cl, function(x) pskill(x$session_info$process$pid))
}

cl <- setup_cluster()

# plan(multicore)
system.time(cleanshoes <- pic_paths %>%
  future_lapply(process_shoe)
)

# kill_cluster(cl)
# rm(cl)
