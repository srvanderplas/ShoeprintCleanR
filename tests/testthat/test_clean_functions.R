context("clean_functions")


test_that("clean_functions works as expected", {


  line_img <- imfill(100, 100, val = 255)
  line_img[3,] <- 0
  
  remove_border_lines(line_img, n = 10) %>% plot()
  
})
