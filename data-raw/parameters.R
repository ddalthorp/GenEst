#### SE and CP plot parameters
## general
.cols_SE = c(spc = colors()[123], ref = colors()[238])
.cols_div = colors()[395]
.cols_CP = c(
  exponential = rgb(0.80, 0.38, 0.56),
  weibull = rgb(1.00, 0.76, 0.15),
  loglogistic = rgb(0.00, 1.00, 1.00),
  lognormal = rgb(0.00, 0.41, 0.55))
# inches in the GUI, simply relative in command-line:
.panel_H = 2.5 
.panel_W = 3
.box_H = 4
.header = 1.7
.buffer = 1.5
.footer = 1.4
## GUI
.pointsize = 14
.res = 100
usethis::use_data(
  .cols_CP,
  .cols_SE,
  .cols_div,
  .panel_H,
  .panel_W,
  .box_H,
  .header,
  .buffer,
  .footer,
  .pointsize,
  .res,
  internal = TRUE, overwrite = TRUE)
