# MAKE_Y_AXIS
# Formats the y axis ticks and labels so that they are easier to read.
# Also returns a multiplicative factor for the plot so that the plot is on the right scale.
make_y_axis <- function(yrange) {
  y_max <- yrange[2]
  if (y_max < 1000) {
    # Do almost nothing
    factor <- 1
    ticks <- pretty(yrange)
    labels <- format(ticks, big.mark=",", scientific=FALSE)    
  } else if (y_max < 10000) {
    # Label with ab,cde
    factor <- 1
    ticks <- pretty(yrange)
    labels <- format(ticks, big.mark=",", scientific=FALSE)
  } else if (y_max < 1000000) {
    # Label with K
    factor <- 1/1000
    ticks <- pretty(yrange*factor)
    labels <- paste(ticks,"K",sep="")
  } else if (y_max < 1000000000) {
    # Label with M
    factor <- 1/1000000
    ticks <- pretty(yrange*factor)
    labels <- paste(ticks,"M",sep="")
  } else {
    # Label with B
    factor <- 1/1000000000
    ticks <- pretty(yrange*factor)
    labels <- paste(ticks,"B",sep="")
  }
  # Remove 0unit, just have 0
  if ("0K" %in% labels) {
    labels[which(labels=="0K")]="0"
  }
  if ("0M" %in% labels) {
    labels[which(labels=="0M")]="0"
  }
  if ("0B" %in% labels) {
    labels[which(labels=="0B")]="0"
  }
  y_axis <- list(factor=factor,ticks=ticks,labels=labels)
  return(y_axis)
}

# PLOT_HR_YAXIS
#
# Plot data using a human readable y-axis
plot_hr_yaxis <- function(x, y, ...) {
  y_range = range(y, na.rm = TRUE)
  y_axis <- make_y_axis(y_range)
  plot(x,y*y_axis$factor,
       yaxt = "n", ...)
  axis(2, at = y_axis$ticks,
       labels = y_axis$labels,
       las = 1, cex.axis=0.8)
}

# PLOT_HR_XAXIS_YAXIS
#
# Plot data using a human readable y-axis
# x must be sim_constants$time$dateFull
plot_hr_xaxis_yaxis <- function(x, y, ...) {
  
  y_range = range(y, na.rm = TRUE)
  y_axis <- make_y_axis(y_range)
  
  x_axis = make_x_axis_week(x)
  # print(x_axis$labels)
  # print(x_axis$ticks)
  
  plot(x,y*y_axis$factor,
       yaxt = "n", ...)
  axis(1, at = x_axis$ticks,
       labels = x_axis$labels,
       las=2, cex.axis=0.8)
  axis(2, at = y_axis$ticks,
       labels = y_axis$labels,
       las = 1, cex.axis=0.8)
}

# PLOT_HR_XAXIS
#
# Plot data using a human readable y-axis
# x must be sim_constants$time$dateFull
plot_hr_xaxis <- function(x, y, ...) {
  
  x_axis = make_x_axis_week(x)
  # y_range = range(y)
  # y_ticks = pretty(ylim)
  
  plot(x,y,yaxt = "n", ...)
  axis(1, at = x_axis$ticks,
       labels = x_axis$labels,
       las=2, cex.axis=0.8)
  # axis(2, at = y_ticks,
       # labels = as.numeric(y_ticks),
       # las = 1, cex.axis=0.8)
}

# Assumes you have Imagemagick installed and usable from the command line (so in the PATH)
crop_figure = function(file) {
  fileName = tools::file_path_sans_ext(file)
  fileExt = tools::file_ext(file)
  if (fileExt == "pdf") {
    command_str = sprintf("pdfcrop %s",file)
    system(command_str)
    command_str = sprintf("mv %s-crop.pdf %s.pdf",fileName,fileName)
    system(command_str)
  }
  if (fileExt == "png") {
    command_str = sprintf("convert %s -trim %s-trim.png",file,fileName)
    system(command_str)
    command_str = sprintf("mv %s-trim.png %s.png",fileName,fileName)
    system(command_str)
  }
  if (fileExt == "tif") {
    command_str = sprintf("convert %s -trim %s-trim.tif",file,fileName)
    system(command_str)
    command_str = sprintf("mv %s-trim.tif %s.tif",fileName,fileName)
    system(command_str)
  }
}
