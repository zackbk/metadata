#' @param rvalue reactive value which contains the following
#'   'xCol'
#'   'yNumCols' (optional)
#'   'groupBy'
#'   'SDfun': can be 'nothing' if we want a boxplot.
#' @param DataTab Data table to be plotted.
#' @param chartType controller string which chooses what to plot.
#' @return p Plot object.

chartView <- function(rvalue = NULL, DataTab = rvalue$summaryView, chartType = "auto", xCol = NULL, yCol = NULL, groupBy = NULL, SDfun = "nothing",
                      date_setting = "year",
                      width_svg = 14, height_svg = 6, ppi = 96, chartPkg = c("ggplot2","ggirafe","plotly"), theme = "grey") {
  print("chart")
  p <- ggplot2::ggplot(data = DataTab)
  
  if(!is.null(rvalue$xCol))  xCol <- rvalue$xCol
  if(!is.null(rvalue$yCol))  yCol <- rvalue$yCol
  if(!is.null(rvalue$groupBy))  groupBy <- rvalue$groupBy # ensure standalone functionality
  if(!is.null(rvalue$SDfun))  SDfun <- rvalue$SDfun
  if(!is.null(rvalue$width_svg))  width_svg <- rvalue$width_svg
  if(!is.null(rvalue$height_svg))  height_svg <- rvalue$height_svg
  #if(!is.null(rvalue$chartPkg))  chartPkg <- rvalue$chartPkg # comment b/c we will do multiple calls of chartview. 
  
  
  if(is.null(chartType)) chartType <- "auto"
  if(chartType == "auto") { # logic for auto selecting plot type
    if(SDfun == "nothing" & is.null(xCol) ){ chartType <- "boxplot"
    } else if(SDfun != "nothing" & is.null(xCol) ) { chartType <- "col"
    } else if(SDfun == "nothing" & (!is.numeric(DataTab[[xCol[1]]])) & is.numeric(DataTab[[yCol[1]]])) { chartType <- "boxplot"
    } else if(SDfun == "nothing" & is.numeric(DataTab[[xCol[1]]]) & is.numeric(DataTab[[yCol[1]]])) { chartType <- "point"  
    } else if(SDfun == "sum" & is.numeric(DataTab[[xCol[1]]]) & is.numeric(DataTab[[yCol[1]]])) { chartType <- "area" #2num
    } else if(SDfun != "sum" & is.numeric(DataTab[[xCol[1]]]) & is.numeric(DataTab[[yCol[1]]])) { 
      chartType <- ifelse(dim(DataTab)[1]>1E4,"point","density")  # 2num
    } else if(SDfun != "nothing" & !is.numeric(DataTab[[xCol[1]]]) & is.numeric(DataTab[[yCol[1]]])) { chartType <- "col" # num+char
    } else if(SDfun != "nothing" & is.numeric(DataTab[[xCol[1]]]) & !is.numeric(DataTab[[yCol[1]]])) { chartType <- "bar" # num+char
    } else if(SDfun == "sum" & !is.numeric(DataTab[[xCol[1]]]) & !is.numeric(DataTab[[yCol[1]]])) { chartType <- "raster" # 2char
    }
    if(length(unique(DataTab[[xCol[1]]])) == 1 & chartType %in% "area") chartType <- "col" 
  }
  
  print(paste("chartType:",chartType))
  if(chartPkg == "ggiraph") {
    if (chartType == "area") { PlotFun <- ggiraph::geom_area_interactive
    } else if (chartType %in% c("bar","pie") ) { PlotFun <- ggiraph::geom_bar_interactive # UPDATED
    } else if (chartType == "col") { PlotFun <- ggiraph::geom_col_interactive 
    } else if (chartType == "boxplot") { PlotFun <- ggiraph::geom_boxplot_interactive 
    } else if (chartType == "point") { PlotFun <- ggiraph::geom_point_interactive
    } else if (chartType == "bin2d") { PlotFun <- ggplot2::geom_bin2d # only ggplot2 option avialable
    } else if (chartType == "raster") { PlotFun <- ggiraph::geom_raster_interactive 
    } else if (chartType == "histogram") { PlotFun <- ggiraph::geom_histogram_interactive 
    } else if (chartType == "line") { PlotFun <- ggiraph::geom_line_interactive
    }
  } else if (chartPkg %in% c("ggplot2","plotly") ) {
    # base::get(paste0(chartPkg,"::"))
    if (chartType == "area") { PlotFun <- ggplot2::geom_area 
    } else if (chartType %in% c("bar","pie") ) { PlotFun <- ggplot2::geom_bar # UPDATED
    } else if (chartType == "col") { PlotFun <- ggplot2::geom_col 
    } else if (chartType == "boxplot") { PlotFun <- ggplot2::geom_boxplot 
    } else if (chartType == "point") { PlotFun <- ggplot2::geom_point
    } else if (chartType == "bin2d") { PlotFun <- ggplot2::geom_bin2d 
    } else if (chartType == "raster") { PlotFun <- ggplot2::geom_raster
    } else if (chartType == "histogram") { PlotFun <- ggplot2::geom_histogram 
    } else if (chartType == "line") { PlotFun <- ggplot2::geom_line
    }
  }
  
  if( chartType %in% c("bar")) {
    p <- p + PlotFun(ggplot2::aes_string(y = yCol[1], fill = groupBy[1]))
  } else if ( chartType %in% c("pie") ) {  # NEW
    p <- p + ggplot2::coord_polar(yCol[1], start=0) # NEW 
  } else if (is.null(groupBy) | length(groupBy)==0) { # change colors
    p <- p + PlotFun(ggplot2::aes_string(x = xCol[1], y = yCol[1]
    ))
  } else {
    if(chartPkg %in% "ggiraph") {
      p <- p + PlotFun(ggplot2::aes_string(x= xCol[1], y = yCol[1], 
                                           fill = groupBy[1], 
                                           tooltip = groupBy[1],
                                           data_id = groupBy[ifelse(length(groupBy)>1, 2, 1)],
                                           color = groupBy[ifelse(length(groupBy)>1, 2, 1)]))
    } else {
      p <- p + PlotFun(ggplot2::aes_string(x= xCol[1], y = yCol[1], 
                                           fill = groupBy[1], 
                                           color = groupBy[ifelse(length(groupBy)>1, 2, 1)]))      
    }
  }
  # add final values
  if(chartPkg %in% c("ggplot2","ggiraph") ) {
    
    if(theme %in% "void" | chartType == "pie" ) {  # NEW
      themeFun <- ggplot2::theme_void
    } else if(theme %in% "grey") {
      themeFun <- ggplot2::theme_grey
      p <- p + themeFun(base_size = 8) # + scale_colour_distiller() # (max allowed: 9)
      p <- p + ggplot2::theme(legend.position = "bottom",
                              axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))  
      p <- p + ggplot2::guides(fill = ggiraph::guide_legend_interactive(nrow=5, byrow=TRUE,
                                                                        legend.spacing = ggplot2::unit(0, 'mm'),
                                                                        legend.key.size = ggplot2::unit(15, "mm"),
                                                                        label.theme = ggplot2::element_text(size = ggplot2::unit(9, "mm"),
                                                                                                            face = "bold") ))
    }
    if(chartPkg %in% "ggplot2") { 
      return(p)
    }
    
    gir <- ggiraph::ggiraph(ggobj = p,width_svg = width_svg, height_svg = height_svg)
    gir <- ggiraph::girafe_options(x = gir, ggiraph::opts_hover(css = "fill:wheat;stroke:orange;r:3pt;"))
    return(gir)
  } else if(chartPkg %in% "plotly") {
    # convert to interactive chart with hovering option (in progress)
    
    if(!is.null(ppi)) fig <- plotly::ggplotly(p,width = width_svg*ppi,height = height_svg*ppi)
    if(is.null(ppi)) fig <- plotly::ggplotly(p,width = width_svg,height = height_svg)
    fig <- plotly::layout(fig, legend = list(orientation = "v", x = 100, y = 0.5, xanchor = "left", yanchor = "top"),
                          margin = list(r=0, l=0, t=0, b=0)) # LEGEND LOCATION (right)
    return(fig) # must change renderPlot and plotOutput to plotly::renderPlotly and plotly::plotlyOutput (server and UI, respectively).
  }
  
}
