##arrange df vars by position
##'vars' must be a named vector, e.g. c("var.name"=1)
arrange_vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}

### FROM https://stackoverflow.com/questions/5620885/how-does-one-reorder-columns-in-a-data-frame
# table <- data.frame(Time=c(1,2), In=c(2,3), Out=c(3,4), Files=c(4,5))
# table
# ##  Time In Out Files
# ##1    1  2   3     4
# ##2    2  3   4     5
# 
# arrange.vars(table, c("Out"=2))
# ##  Time Out In Files
# ##1    1   3  2     4
# ##2    2   4  3     5