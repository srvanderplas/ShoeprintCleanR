library(tidyverse)
library(EBImage)
library(furrr)
plan(multicore)

# ---- Convenience functions ----
prune <- function(imgx, prop_limit = 0.02) {
  counts <- table(imgx)
  categories <- sort(unique(as.numeric(imgx)))
  prop <- counts/sum(counts)
  cat_replace <- categories[prop < prop_limit]
  imgx[imgx %in% cat_replace] <- 0
  imgx
} # Get rid of really small areas - assumes large areas have merged...


initial_mask <- function(img, blur_rad = 15, w = 10, h = 10, offset = 0.025) {
  blur_brush <- makeBrush(blur_rad, shape = "disc", step = T)^2
  blur_brush <- blur_brush/sum(blur_brush)
  
  img %>% filter2(filter = blur_brush) %>% (function(x) (1 - x)) %>% thresh(w = w, h = h, offset = offset)
}

clean_mask <- function(img, rad1 = 5, rad2 = 91, prop = 1.5*pi*rad2^2/length(img)) {
  f1 <- makeBrush(rad1, shape = "disc")
  f2 <- makeBrush(rad2, shape = "disc")
  img %>%
    erode(f1) %>%
    dilate(f2) %>%
    bwlabel() %>%
    prune(prop_limit = prop)
}

get_expanded_mask <- function(mask_init, rad1 = 5, rad2 = 91, 
                              prop = 1.5*pi*rad2^2/length(img), 
                              expand_rad = 50) {
  
  background <- table(mask_init[0:10,0:10]) %>% sort() %>% magrittr::extract(1) %>%
    names() %>% parse_number()
  useful <- mask_init != background
  
  if (sum(useful) > 0) {
    useful_df <- as_tibble(useful, rownames = NA) %>%
      mutate(row = 1:n()) %>%
      tidyr::gather(key = col, value = value, -row) %>%
      mutate(col = col %>% str_remove("V") %>% parse_integer()) %>%
      filter(value > background)
    
    test_mat <- 0*useful
    
    useful_cols <- useful_df %>%
      group_by(row) %>%
      summarize(mincol = min(col), maxcol = max(col)) %>%
      mutate(col = purrr::map2(
        mincol, maxcol, 
        ~tibble(col = pmax(0, .x - expand_rad):pmin(ncol(mask_init), .y + expand_rad)))) %>%
      select(-mincol, -maxcol) %>%
      unnest(col) %>%
      select(row, col)
    
    useful_rows <- useful_df %>%
      group_by(col) %>%
      summarize(minrow = min(row), maxrow = max(row)) %>%
      mutate(row = purrr::map2(
        minrow, maxrow, 
        ~tibble(row = pmax(0, .x - expand_rad):pmin(nrow(mask_init), .y + expand_rad)))) %>%
      select(-minrow, -maxrow) %>%
      unnest(row) %>%
      select(row, col)
    
    bind_rows(useful_rows, useful_cols) %>%
      unique() %>%
      mutate(value = 1) %>%
      complete(row = 1:nrow(mask_init), col = 1:ncol(mask_init), fill = list(value = 0)) %>%
      spread(key = col, value = value, fill = NA) %>%
      arrange(row) %>%
      select(-row) %>%
      as.matrix() %>%
      as.Image()
  }
  
  
} # Shorthand for erode, dilate, bwlabel, then "convex hull" calculation

# ------------------------------------------------------------------------------

imglist <- list.files("/lss/research/csafe-shoeprints/ShoeImagingPermanent/", 
                      pattern = "00\\d{4}[RL]_\\d{8}_5_1_1", full.names = T)

d5 <- makeBrush(15, shape = 'disc', step = T)^2
d5 <- d5/sum(d5)

dir <- tempdir()
file.copy(imglist, file.path(dir, basename(imglist)))
imglist <- list.files(dir, "\\..*", full.names = T)

out_img_dir <- "~/Projects/CSAFE/2019-this_is_us/images/shoes/longitudinal/"


# --- Step by step -------------------------------------------------------------
imgs <- tibble(
  file = imglist,
  shoe_id = str_extract(file, "\\d{6}[RL]"),
  date = str_extract(file, "\\d{8}") %>% lubridate::ymd()
) %>%
  group_by(shoe_id, date) %>%
  filter(row_number() == 1)


img_steps <- imgs %>%
  filter(str_detect(shoe_id, "L")) %>%
  group_by(shoe_id) %>%
  arrange(date) %>%
  filter(row_number() == 1) %>%
  mutate(orig = purrr::map(file, readImage, all = F)) 

# png(file.path(out_img_dir, "Film_Demo_Orig.png"), width = 300*9, height = 300*2, units = "px")
# par(mfrow = c(1, 9)) 
# purrr::map(img_steps$orig, plot)
# dev.off()

img_steps <- img_steps %>%
  mutate(
    filter_init = map(orig, filter2, filter = d5),
    inverse = map(filter_init, ~1 - .),
    thresh_init = map(inverse, thresh, w = 10, h = 10, offset = 0.025)
  )

# png(file.path(out_img_dir, "Film_Demo_Inv.png"), width = 300*9, height = 300*2, units = "px")
# par(mfrow = c(1, 9)) 
# purrr::map(img_steps$inverse, plot)
# dev.off()
# 
# png(file.path(out_img_dir, "Film_Demo_Filter_Init.png"), width = 300*9, height = 300*2, units = "px")
# par(mfrow = c(1, 9)) 
# purrr::map(img_steps$filter_init, plot)
# dev.off()
# 
# png(file.path(out_img_dir, "Film_Demo_Thresh_Init.png"), width = 300*9, height = 300*2, units = "px")
# par(mfrow = c(1, 9)) 
# purrr::map(img_steps$thresh_init, plot)
# dev.off()

img_steps <- img_steps %>%
  mutate(
    mask = furrr::future_map(thresh_init, clean_mask, rad2 = 151),
    hull = furrr::future_map(mask, fillHull)
  )
img_steps <- img_steps %>%
  mutate(
    expand_mask = furrr::future_map(hull, get_expanded_mask)
  )

# png(file.path(img_dir, "Film_Demo_Mask.png"), width = 300*9, height = 300*2, units = "px")
# par(mfrow = c(1, 9))
# purrr::map(img_steps$mask, plot)
# dev.off()
# 
# png(file.path(img_dir, "Film_Demo_Hull.png"), width = 300*9, height = 300*2, units = "px")
# par(mfrow = c(1, 9))
# purrr::map(img_steps$hull, plot)
# dev.off()
# 
# png(file.path(img_dir, "Film_Demo_Mask_Expand.png"), width = 300*9, height = 300*2, units = "px")
# par(mfrow = c(1, 9))
# purrr::map(img_steps$expand_mask, plot)
# dev.off()

img_steps <- img_steps %>%
  mutate(cleaned = purrr::map2(orig, expand_mask, function(x, y) as.Image(x*y + (1 - y)*median(x))))%>%
  mutate(cleaned_thresh = purrr::map(cleaned, ~1 - thresh(1 - ., w = 150, h = 150, offset = .05)))

# png(file.path(img_dir, "Film_Demo_Cleaned.png"), width = 300*9, height = 300*2, units = "px")
# par(mfrow = c(1, 9))
# purrr::map(img_steps$cleaned, plot)
# dev.off()
# 
# png(file.path(img_dir, "Film_Demo_Cleaned_Thresh.png"), width = 300*9, height = 300*2, units = "px")
# par(mfrow = c(1, 9))
# purrr::map(img_steps$cleaned_thresh, plot)
# dev.off()

img_steps <- img_steps %>%
  mutate(clean_balance = purrr::map2(cleaned_thresh, cleaned, function(x, y) normalize(1 - (1 - x)*y)))

# png(file.path(out_img_dir, "Film_Demo_Cleaned_Balance_Compare.png"), width = 300*9, height = 300*2, units = "px")
# par(mfrow = c(1, 9)) 
# purrr::map2(img_steps$clean_balance, img_steps$cleaned_thresh, function(x, y) {
#   print(sum(normalize(x) != normalize(y)))
#   plot(rgbImage(x-y, 1 + 0*x, x+y))
# })
# dev.off()

# --- Parameter value selection ------------------------------------------------

img_subset <- img_steps[c(2:4, 8),]
par_explore <- crossing(rad1 = c(3, 5, 9),
                        rad2 = c(51, 91, 151))
img_pars <- crossing(img_subset, par_explore)
img_pars_subset <- img_pars %>%
  mutate(all = furrr::future_pmap(list(img = thresh_init, rad1 = rad1, rad2 = rad2), clean_mask))

png(file.path(out_img_dir, "Film_Demo_Parameter_Selection.png"), width = 300*9, height = 300*2*4, units = "px")
layout(matrix(1:45, nrow = 5, byrow = T), heights = c(1, 3, 3, 3, 3))
img_pars_subset %>% ungroup() %>%
  select(rad1, rad2) %>% unique() %>%
  mutate(tmp = purrr::map2(rad1, rad2, function(x, y) {
    par(mar = c(0,0,0,0))
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(.5, .75, sprintf("rad1 = %d", x), adj = .5)
    text(.5, .25, sprintf("rad2 = %d", y), adj = .5)
    TRUE
  }))
purrr::map(img_pars_subset$all, plot)
dev.off()

# --- All Left Shoes -----------------------------------------------------------

imgs <- tibble(
  file = imglist,
  shoe_id = str_extract(file, "\\d{6}[RL]"),
  date = str_extract(file, "\\d{8}") %>% lubridate::ymd()
) %>%
  group_by(shoe_id, date) %>%
  filter(row_number() == 1)


img_steps <- imgs %>%
  filter(str_detect(shoe_id, "L")) %>%
  group_by(shoe_id) %>%
  arrange(date) %>%
  mutate(orig = purrr::map(file, readImage, all = F))

img_steps <- img_steps %>%
  mutate(thresh_init = map(orig, initial_mask)) %>%
  mutate(
    mask = furrr::future_map(thresh_init, clean_mask, rad2 = 151),
    hull = furrr::future_map(mask, fillHull)
  )

img_steps <- img_steps %>%
  mutate(
    expand_mask = map(thresh_init, get_expanded_mask)
  )

img_steps <- img_steps %>%
  mutate(cleaned = purrr::map2(orig, expand_mask, function(x, y) as.Image(x*y + (1 - y)*median(x)))) %>%
  mutate(cleaned_thresh = purrr::map(cleaned, ~1 - thresh(1 - ., w = 150, h = 150, offset = .05)))
