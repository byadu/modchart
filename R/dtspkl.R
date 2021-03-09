grpby<- function(tbls, cols) {
	dots <- lapply(cols, as.symbol)
	group_by_(tbls, .dots=dots, add=TRUE)
	}
#' @import sparkline
sparklines<- function(gfdim, nseries, dxy, xynam) {
	dxy<- dxy[,1:3]
	sdxy<- list()
	for(i in 1:nseries) {
		sdxy[[i]]<- dxy %>% grpby(xynam[1:(gfdim-1)]) %>% summarise_(interp(~sparkline::spk_chr(var, type=dtopts$sl), var=as.name(xynam[gfdim+i])))
		colnames(sdxy[[i]])<- c(xynam[1:(gfdim-1)], paste(xynam[gfdim+i], "by", xynam[gfdim]))
		}
	gfdim<- gfdim-1
	dxy<- sdxy[[1]]
	if(nseries > 1)
		for(i in 2:nseries)
			dxy<- merge(dxy, sdxy[[i]], all=TRUE)
	DT::datatable(dxy, escape=FALSE, class='compact', filter='top', extensions='Buttons', options=list(fnDrawCallback = JS( 'function(){ HTMLWidgets.staticRender(); } '), autowidth=FALSE, dom='<"bottom"itBp><"clear">',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), pageLength=10)) %>% sparkline::spk_add_deps()
	}
