#'@param x data table.
#'@param input controller with 'arrangeBy' value that is non-zero and 'order' value to describe descending or ascending order.
#'@return sorted data table of x
SortView <- function(x,input){
  if(input$arrangeBy != "") {
    data.table::setorderv(x,cols = names(x)[names(x) %in% input$arrangeBy],order = ifelse(1 %in% input$order,1,-1) )
  }
  return(x)
}