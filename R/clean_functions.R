#' Removes a label from the shoeprint scan
#' 
#' This algorithm assumes the label will be larger than 50 px and will be dark
#' 
#' @param shoe cimg image type
#' @param ... arguments to pass to imager::threshold
#' @return cimg with any label which may be present removed
#' @import imager
remove_print_label <- function(shoe, ...) {
  
  if (max(shoe) > 255) {
    shoe <- renorm(shoe)
  }
  
  replace_color <- 255 
  
  # This identifies the label if it is bigger than 50 px and dark
  z <- threshold(shoe, ...) %>%
    grow(100) %>%
    shrink(125)
  
  # Only remove label if it has a reasonable area
  islabeled <- mean(z) < .98
  
  if (islabeled) {
    px <- z < median(z)
    shoe[px] <- replace_color
  }
  
  return(shoe)
}

#' Limit the number of colors in the image
#' 
#' @param shoe cimage
#' @param n number of colors in returned image
#' @param return cimg
#' @import imager
#' @import dplyr
quantize_colors <- function(shoe, n = 16) {
  if (max(shoe) > 255) {
    shoe <- renorm(shoe)
  }
  
  shoe_df <- shoe %>%
    as.data.frame(wide = 'c') %>%
    tbl_df()
  
  shoe_cluster <- shoe_df %>% select(-x, -y) %>% kmeans(centers = n)
  
  shoe_df %>%
    mutate(label = as.character(shoe_cluster$cluster)) %>%
    select(x, y, label) %>%
    left_join(shoe_cluster$centers %>% tbl_df %>% mutate(label = as.character(row_number()))) %>%
    select(-label) %>%
    gather(key = 'cc', value = 'value', starts_with('c.')) %>%
    mutate(cc = as.integer(gsub('c\\.', '', cc))) %>%
    as.cimg(dim = dim(shoe))
}
