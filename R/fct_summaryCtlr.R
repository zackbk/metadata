#' @param ctlr list of controller values that have columns to summarise by (and to summarise).
#' @param rvalue optional reactive value
#' @param DataTab data table that will be summarised.
#' @return rvalue Now containing additional value called "summaryView".

summaryCtlr <- function(ctlr = input, rvalue = NULL, DataTab = rvalue$modelData, 
                        xCol = NULL, yCol = NULL, yNumCols = NULL, groupBy = NULL, SDfun = "sum", SDunit = "actual") {
  print("start summary")
  print("DataTab")
  print(class(DataTab))
  print(names(DataTab))
  print("yNumCols")
  # print(ctlr$yNumCols)
  if(!is.null(ctlr$xCol)) xCol <- ctlr$xCol # ensure standalone functionality
  if(!is.null(ctlr$yCol)) yCol <- ctlr$yCol
  if(!is.null(ctlr$yNumCols)) yNumCols <- ctlr$yNumCols
  if(!is.null(ctlr$groupBy)) groupBy <- ctlr$groupBy
  if(!is.null(ctlr$SDfun)) SDfun <- ctlr$SDfun
  if(!is.null(ctlr$SDunit)) SDunit <- ctlr$SDunit
  
  if(is.null(rvalue)) rvalue <- list()  # incase we want the 
  rvalue$yNumCols <- names(DataTab)[data.table::chmatch(yNumCols, names(DataTab), nomatch = 0)] # transfer valid columns to reactive list
  # print(rvalue$yNumCols)
  print("yCol")
  rvalue$yCol <- names(DataTab)[data.table::chmatch(yCol, names(DataTab), nomatch = 0)]
  # print(rvalue$yCol)
  print("xCol")
  rvalue$xCol <- names(DataTab)[data.table::chmatch(xCol, names(DataTab), nomatch = 0)]
  # print(rvalue$xCol)
  print("groupBy")
  rvalue$groupBy <- names(DataTab)[data.table::chmatch(groupBy, names(DataTab), nomatch = 0)]
  # print(rvalue$groupBy)
  # print("groupBy")
  rvalue$by <- unique(c(rvalue$xCol,rvalue$groupBy))
  # print(rvalue$groupBy)
  # print("SDCols")
  rvalue$SDcols <- unique(c(rvalue$yCol,rvalue$yNumCols))
  # print(rvalue$SDcols)
  rvalue$SDfun <- SDfun
  print("interim summary")
  if((length(SDfun) > 0) & SDfun %in% c("length")) {
    summaryView <- DataTab[, lapply(.SD, function(x, f = SDfun) round( do.call(what = f,args = list(x),quote = T) , 3) ), .SDcols = c(rvalue$SDcols), by = c(rvalue$by)]
    if(SDunit %in% c("percent") ){ # calculate %
      summaryView <- summaryView[, lapply(.SD, function(x) {
        if(is.numeric(x)) {
          round(x/sum(x,na.rm=T)*100,3)  
        } else{
          x
        }
      }) , .SDcols = c(rvalue$SDcols,rvalue$groupBy), by = c(rvalue$xCol)]
    }
  } else if (!is.null(SDfun)) {
    summaryView <- DataTab[, lapply(.SD, function(x, f = SDfun) round( do.call(what = f,args = list(x, na.rm=T), quote = T) , 3) ), .SDcols = c(rvalue$SDcols), by = c(rvalue$by)]
    if(SDunit %in% c("percent") ){ # calculate %
      summaryView <- summaryView[, lapply(.SD, function(x) {
        if(is.numeric(x)) {
          round(x/sum(x,na.rm=T)*100,3)  
        } else{
          x
        }
        }) , .SDcols = c(rvalue$SDcols,rvalue$groupBy), by = c(rvalue$xCol)]
    }
    if(SDfun %in% "sum" & ("EI_MtCO2eperPJ" %in% rvalue$SDcols)) { # calculate EI
      tempDT <- DataTab[, lapply(.SD, mean, na.rm=T), .SDcols = c("GHG_MtCO2e","Energy_PJ"), by = c(rvalue$by)] 
      summaryView[, EI_MtCO2eperPJ := round(tempDT$GHG_MtCO2e/tempDT$Energy_PJ, 3)]
    }
  } else {
    summaryView <- DataTab[, lapply(.SD, function(x, f = "nothing") round( do.call(what = f,args = list(x, na.rm=T), quote = T) , 3) ), .SDcols = c(rvalue$SDcols), by = c(rvalue$by)]
  }
  print("end summary")
  return(summaryView)
}
