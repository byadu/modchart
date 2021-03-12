#' @importFrom sparkline spk_chr spk_add_deps
#' @importFrom lazyeval interp
#' @importFrom dplyr summarise group_by
#' @importFrom htmlwidgets JS

sparklines<- function(gfdim, nseries, dxy, xynam) {
	dxy<- dxy[,1:3]
	sdxy<- list()
	for(i in 1:nseries) {
		sdxy[[i]]<- dxy %>% group_by(!!as.name(xynam[1:(gfdim-1)])) %>% summarise(spk_chr(!!as.name(xynam[gfdim+i]), type=dtopts$sl))
		colnames(sdxy[[i]])<- c(xynam[1:(gfdim-1)], paste(xynam[gfdim+i], "by", xynam[gfdim]))
		}
	gfdim<- gfdim-1
	dxy<- sdxy[[1]]
	if(nseries > 1)
		for(i in 2:nseries)
			dxy<- merge(dxy, sdxy[[i]], all=TRUE)
	DT::datatable(dxy, escape=FALSE, class='compact', filter='top', extensions='Buttons', options=list(fnDrawCallback = JS( 'function(){ HTMLWidgets.staticRender(); } '), autowidth=FALSE, dom='<"bottom"itBp><"clear">',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), pageLength=10)) %>% sparkline::spk_add_deps()
	}
