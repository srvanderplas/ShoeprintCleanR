#' Removes a label from the shoeprint scan
#' 
#' This algorithm assumes the label will be larger than 50 px and will be dark
#' 
#' @param shoe cimg image type
#' @param ... arguments to pass to imager::threshold
#' @return cimg with any label which may be present removed
#' @import imager
#' @export
remove_print_label <- function(shoe, ...) {
  
  if (max(shoe) > 255) {
    shoe <- renorm(shoe)
  }
  
  if (!exists("thr")) {
    thr <- "10%"
  }
  replace_color <- max(shoe) 
  
  # This identifies the label if it is bigger than 50 px and dark
  z <- threshold(shoe, thr = thr) %>%
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
#' @importFrom tidyr gather
#' @export
quantize_colors <- function(shoe, n = 16) {
  if (max(shoe) > 255) {
    shoe <- renorm(shoe)
  }
  # https://lumiamitie.github.io/r/imager-color-quantization/
  
  shoe_df <- shoe %>%
    as.data.frame(wide = 'c') %>%
    tbl_df()
  
  shoe_cluster <- suppressMessages(
    shoe_df %>% select(-x, -y) %>% kmeans(centers = n)
  )
  
  shoe_df %>%
    mutate(label = as.character(shoe_cluster$cluster)) %>%
    select(x, y, label) %>%
    left_join(
      shoe_cluster$centers %>% 
        tbl_df %>% 
        mutate(label = as.character(row_number())), 
      by = "label") %>%
    select(-label) %>%
    tidyr::gather(key = 'cc', value = 'value', starts_with('c.')) %>%
    mutate(cc = as.integer(gsub('c\\.', '', cc))) %>%
    as.cimg(dim = dim(shoe))
}

#' Remove local background 
#' 
#' @param shoe cimage
#' @param n number of sub-images to use. shoe is divided into an n x n grid for local background computation
#' @param threshold if a pixel is within threshold of the median pixel value, it should be set to white as well
#' @return a cimg
#' @export
#' @import imager
#' @importFrom purrr map_df
#' @importFrom dplyr data_frame
#' @importFrom purrr map
remove_local_background <- function(shoe, n = 10, threshold = 10, borderarea = 100, borderonly = F) {
  grid_shoe <- data_frame(
    x = 1:n,
    xstr = imsplit(renorm(shoe), axis = "x", nb = n)
  ) %>%
    split(.$x) %>%
    map_df(.x = ., .f = function(zz) {
      tmp <- imsplit(zz$xstr[[1]], axis = "y", nb = n)
      data_frame(
        x = zz$x,
        y = 1:n,
        xyshoe = tmp,
        # Get local background values
        medvalue = sapply(tmp, median),
        # Replace local background with white
        normshoe = lapply(tmp, function(x) {y <- x; y[(x >= median(x))] <- 255; y[abs(y - median(x)) < threshold] <- 255; y})
      )
    })
  
  center_shoe <- !px.borders(shoe, n = borderarea)
  
  shoe_reassemble <- select(grid_shoe, x, y, normshoe) %>%
    split(.$x) %>%
    map(~imappend(imlist = .$normshoe, axis = "y")) %>%
    imappend(imlist = ., axis = "x") %>%
    renorm()
  
  if (borderonly) {
    shoe_fixpieces <- renorm(shoe)
    shoe_fixpieces[!center_shoe] <- shoe_reassemble[!center_shoe]
  } else {
    shoe_fixpieces <- shoe_reassemble
  }

  shoe_fixpieces %>% renorm()
}


#' Remove border shading
#' 
#' @param shoe cimg
#' @param nk number of knots to fit in the earth/MARS algorithm
#' @param degree degree of interaction allowed between knots
#' @return image with border shading removed
#' @export
#' @import earth
#' @import imager
remove_border_shading <- function(shoe, nk = 25, degree = 2) {

  shoedf <- shoe %>% imager::renorm() %>% as.data.frame()
  
  marstmp <- earth::earth(value ~ x + y, data = shoedf, nk = nk, degree = degree)
  shoedf$resid <- resid(marstmp)
  
  rm(marstmp)
  gc()
  
  shoeresid <- imager::as.cimg(shoedf %>% select(-value) %>% rename(value = resid))
  
  shoe_max <- shoe
  shoe_max[shoeresid >= 0] <- 255 # Anything with a positive residual is white
  
  shoe_max
}

#' Remove line segments in border region
#' 
#' @param shoe cimg
#' @param n vector of length 4, or single numeric value indicating the width of the border. 
#' @param maxiter number of times to iterate
#' @param angletol lines with slope more than angletol off of vertical or horizontal will be ignored
#' @export
#' @return image with line segments removed
#' @import imager
#' @import dplyr
#' @importFrom pixmap pixmapGrey
#' @importFrom image.LineSegmentDetector image_line_segment_detector
remove_border_lines <- function(shoe, n = c(100, 100, 100, 100), maxiter = 2, angletol = 5) {
  
  if (length(n) < 4) {
    n <- rep(n, 4)[1:4]
  }
  
  fill_color <- max(shoe)
  
  border <- shoe %>% (function(x) {
    Yc(x) < n[2] | Yc(x) > (height(x) - n[4]) | 
      Xc(x) < n[1] | Xc(x) > (width(x) - n[3])
  })
  
  fillcells <- sum(border)
  m <- 0
  while (fillcells > 0  & m < maxiter) {
    m <- m + 1
    
    shoe_segments <-  image.LineSegmentDetector::image_line_segment_detector(renorm(shoe)[, , 1, 1], scale = 1)
    shoe_seg_lines <- shoe_segments$lines %>% as.data.frame %>%
      mutate(n = 1:n(),
             len = sqrt((x1 - x2)^2 + (y1 - y2)^2),
             angle = asin((y2 - y1)/len)*180/pi) %>%
      filter(len > floor(width(shoe)/50)) %>%
      filter(abs(abs(angle) - 90) < angletol | abs(angle) < angletol)
    
    shoe_segments$pixels_filtered <- shoe_segments$pixels %>%
      multiply_by(shoe_segments$pixels %in% shoe_seg_lines$n)
    
    shoe_segment_mask <- border * 0
    shoe_segment_mask[ , , 1, 1] <- shoe_segments$pixels_filtered != 0
    
    combined_mask <- (shoe_segment_mask & border)
    fillcells <- sum(combined_mask)
    if ( fillcells > 0 ) {
      shoe[combined_mask] <- fill_color
    }
  }
  
  shoe 
}

#' Crop shoe border
#' 
#' Remove most of the white space around the shoe. This function splits the image
#' into two pieces at the middle, then uses the mean intensity (0-255) in the chosen dimension
#' to determine where the image should be cropped. The region to crop is the value closest to the 
#' center of the image which is within tol of the maximum mean intensity found in the
#' image. 
#' @param shoe cimg
#' @param axis either "x", "y", or "xy"
#' @param sigma radius to use for blurring the image
#' @param tol change the tolerance of the cropping function
#' @export
#' @import imager
crop_border <- function(shoe, axis = "xy", sigma = 10, tol = 0.01) {
  stopifnot(axis %in% c("x", "y", "xy"))
  
  if (axis == "xy") {
    axis = c("x", "y")
  }
  
  modimg <- shoe
  
  # Deal with x first
  if ("x" %in% axis) {
    tshoe_x <- imsplit(modimg, "x", 2)
    tshoe_x[[2]] <- mirror(tshoe_x[[2]], "x")
    
    tshoe_xfix <- lapply(tshoe_x, function(xx) {
      xxmod <- isoblur(xx, sigma)
      yy <- apply(xxmod, 1, mean) 
      zz <- which(abs(yy - max(yy)) < tol) %>% max()
      imsub(xx, x > (zz + 1)) 
    }) 
    
    tshoe_xfix[[2]] <- mirror(tshoe_xfix[[2]], "x")
    modimg <- imappend(tshoe_xfix, axis = "x")
    rm(tshoe_x, tshoe_xfix)
  }
  
  if ("y" %in% axis) {
    tshoe_y <- imsplit(modimg, "y", 2)
    tshoe_y[[2]] <- mirror(tshoe_y[[2]], "y")
    
    tshoe_yfix <- lapply(tshoe_y, function(xx) {
      xxmod <- isoblur(xx, sigma)
      yy <- apply(xxmod, 2, mean) 
      zz <- which(abs(yy - max(yy)) < tol) %>% max()
      imsub(xx, y > (zz + 1)) 
    }) 
    
    tshoe_yfix[[2]] <- mirror(tshoe_yfix[[2]], "y")
    modimg <- imappend(tshoe_yfix, axis = "y")
    rm(tshoe_y, tshoe_yfix)
  }
  
  return(modimg)
}

#' Rotate the shoe print
#' 
#' @param shoe cimg
#' @export
#' @import magrittr
#' @import imager
align_shoe_print <- function(shoe) {
  
  fill_color <- 255
  
  # Get img dimensions
  img_width <- width(shoe)
  img_height <- height(shoe)
  
  # Create small version for linear regression
  smshoe <- shoe %>%
    threshold() %>%
    imsub(x %inr% (img_width * c(.1, .9)), y %inr% (img_height * c(.1, .9))) 
  
  rot_ds <- (smshoe == 0) %>%
    as.data.frame() %>%
    select(-z, -cc)
  
  rot_matrix <- prcomp(rot_ds) %>%
    magrittr::extract2("rotation") %>%
    as.data.frame()
  
  rot_angle_a <- rot_matrix$PC1[1]/rot_matrix$PC1[2] %>%
    atan()
  
  rot_sign <- ifelse (diff(sign(rot_matrix$PC1)) == 0, -1, 1)
  
  rot_angle <- rot_angle_a %>%
    magrittr::multiply_by(180/pi) %>%
    magrittr::multiply_by(rot_sign) %>%
    unlist()
  
  shoe_whiteborder <-  shoe %>%
    pad(nPix = 10, axes = "xy", pos = -1, val = 0) %>%
    pad(nPix = 10, axes = "xy", pos = 1, val = 0) %>%
    bucketfill(1, 1, 1, color = fill_color)
  
  rot_shoe <- imrotate(shoe_whiteborder, rot_angle, boundary = 1)
  
  rot_shoe
}
