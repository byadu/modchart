#' @title df2g
#' @description This is a utility function to create a 'graph' data structure to pass to chart module
#' @export
#' @param title is the title for the chart
#' @param dxy is the dataframe to draw the chart from
#' @param ndim is the number of dimensions in the xl file; it is assumed these are in the first ndim columns of the xl
#' @param nseries is the number of series in the xl file; it is assumed these are in the last nseries columns of the xl
#
df2g<- function(title, dxy, ndim=1, nseries=1) {
	gp<- list()
	gp$gtype<- 'dt'
	gp$title<- title
	gp$gfdim<- ndim
	gp$nseries<- nseries

	g<- list()
	g$gp<- gp
	g$dxy<- dxy
	g$dxy<- g$dxy[order(g$dxy[,1]),]
	return(g)
	}

#' @title xl2g
#' @description This is a utility function to create a 'graph' data structure to pass to chart module from an 'Excel' sheet
#' @export
#' @param xl has the title and data of the 'Excel' file
#' @param ndim is the number of dimensions in the 'Excel' file; it is assumed these are in the first ndim columns of the xl
#' @param nseries is the number of series in the 'Excel' file; it is assumed these are in the last nseries columns of the xl
#
xl2g<- function(xl, ndim=1, nseries=1) {
	title<- xl$sheets[1]
	dxy<- xl$sheetdata[[1]]
	return(df2g(title, dxy, ndim, nseries))
	}
