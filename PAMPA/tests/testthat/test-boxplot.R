library("ggplot2")
library(testthat)
library(vdiffr)

testthat::test_that("plots have known output", {
  disp_hist_base <- function() hist(mtcars$disp)
  vdiffr::expect_doppelganger("disp-histogram-base", disp_hist_base)

  disp_hist_ggplot <- ggplot(mtcars, aes(disp)) + geom_histogram()
  vdiffr::expect_doppelganger("disp-histogram-ggplot", disp_hist_ggplot)
})

