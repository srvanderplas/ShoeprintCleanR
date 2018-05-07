library(tidyverse)
library(magrittr)
library(ShoeprintCleanR)
library(imager)

shoe_paths <- list.files(path = "inst/test_shoes/original", pattern = ".*.tif", full.names = T)
shoes <- lapply(shoe_paths, load.image) %>% as.imlist()


process_shoe <- function(i) {
  shoe <- load.image(i) %>%
    renorm() 
  
  shoedens <- density(shoe)
  mode <- shoedens$x[which.max(shoedens$y)] # Most common value
  
  shoe[shoe >= (mode*.95)] <- 255
  
  shoe <- shoe %>%
    remove_border_lines(n = 5, maxiter = 2) %>%
    quantize_colors(8) %>%
    crop_border() %>%
    clip_corners() %>%
    align_shoe_print()
  
  # plot(shoe)
  
  return(shoe)
}

cleanedshoes <- lapply(shoe_paths, process_shoe) %>% as.imlist()

initial_boundaries <- function(img) {

  xhist <- apply(img, 1, mean) %>% zoo::rollapply(width = 75, fill = max(.), partial = T, FUN = mean, na.rm = T)
  yhist <- apply(img, 2, mean) %>% zoo::rollapply(width = 75, fill = max(.), partial = T, FUN = mean, na.rm = T)
  
  gmean <- outer(xhist, yhist, function(x, y) sqrt(x*y)) %>%
    as_data_frame() %>%
    mutate(row = 1:nrow(.)) %>%
    gather(key = col, value = value, -row) %>%
    mutate(col = str_replace(col, "\\D", "")) %>%
    mutate_all(as.numeric)
  
  qplot(1:length(xhist), xhist, geom = "line")
  qplot(1:length(yhist), yhist, geom = "line")
  
  start.x <- range(which(xhist != 255))
  start.y <- range(which(yhist != 255))
  
  plot(img)
  abline(h = start.y, col = "red")
  abline(v = start.x, col = "blue")
  
  return(c(x1 = start.x[1], x2 = start.x[2], y1 = start.y[1], y2 = start.y[2]))
}

evaluate_bounds <- function(img, bounds = NULL) {
  if (is.null(bounds)) {
    bounds <- c(2, ncol(img) - 1, 2, nrow(img) - 1)
  }
  # print(length(bounds))
  
  stopifnot(length(bounds) == 4)
  if(sum(bounds > 1) < 4) return(-Inf)
  if(sum(bounds[1:2] >= ncol(img)) > 0) return(-Inf)
  if(sum(bounds[3:4] >= nrow(img)) > 0) return(-Inf)
  
  x1 <- min(bounds[1:2])
  x2 <- max(bounds[1:2])
  y1 <- min(bounds[3:4])
  y2 <- max(bounds[3:4])
  
  npix <- length(img)
  # Get inside image
  inside_img <- imsub(img, x %inr% c(x1, x2), y %inr% c(y1, y2))
  area <- length(inside_img)/npix
  
  # print(c(x1, x2, y1, y2))
  
  # Set inside image to NA
  coords <- (1 - (Xc(img) > x1)*(Xc(img) < x2)*(Yc(img) > y1)*(Yc(img) < y2))
  outside_img <- img
  outside_img[coords == 1] <- NA
  # try(outside_img[x1:x2, y1:y2] <- NA)
  
  # Calculate area difference
  denom <- 1 - (area - .5)^2
  
  (mean(outside_img, na.rm = T) - mean(inside_img))/denom
}

res <- expand.grid(x1 = seq.int(2, (ncol(img) - 1)/2, length.out = 15) %>% floor(),
                   y1 = seq.int(2, (nrow(img) - 1)/2, length.out = 30) %>% floor()) %>%
  as_data_frame() %>%
  mutate(x2 = ncol(img) - x1, y2 = nrow(img) - y1) %>%
  select(x1, x2, y1, y2) %>%
  as.matrix() %>%
  apply(., 1, function(x) evaluate_bounds(img = img, bounds = as.numeric(x)))

tmp <- expand.grid(x1 = seq.int(2, (ncol(img) - 1)/2, length.out = 15) %>% floor(),
                   y1 = seq.int(2, (nrow(img) - 1)/2, length.out = 30) %>% floor()) %>%
  as_data_frame() %>%
  mutate(res = res)

qplot(tmp$y1, tmp$x1, z = res, geom = "contour")
qplot(tmp$x1, tmp$res, color = factor(tmp$y1), geom = "line")
optim(par = initial_boundaries(img), fn = evaluate_bounds, img = img, lower = 2, upper = max(dim(img)))
