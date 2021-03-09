sunbobservers<- 0
sunbopts<- reactiveValues(sbcolors='Dark2')
#' @title sunb
#' @description A 'shiny' module to display 'sunburst' chart with options
#' @details Option for 'sunburst' is color palette
#' 
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @import sunburstR
#' @export 
sunb<- function(input, output, session, g, noopt=0) {
	ns<- session$ns

	output$sunchart<- renderSunburst({ 
		dxy<- g$dxy
		if(!is.null(f$rows_selected))
			dxy<-g$dxy[f$rows_selected,]
		gfdim<- g$gp$gfdim
		d1<- gsub('-','_',dxy[,1])
		d2<- gsub('-','_',dxy[,2])
		donut<- paste(d1, d2, sep='-')
		if(gfdim > 2)
		for(i in 3:gfdim) {
			d2<- gsub('-','_',dxy[,i])
			donut<- paste(donut, d2, sep='-')
			}
		donut <- data.frame(donut, values= dxy[,gfdim+1])
		d<- sunburst(donut,colors=sunbopts$sbcolors,legend=list())
		d
		})
	observeEvent(input$sbcolors, ignoreInit=TRUE,{
		sunbopts$sbcolors<- input$sbcolors
		})

	output$sunoptions<- renderUI({
		fluidRow(
			selectInput(ns('sbcolors'), 'Color Palette', c('Set2', 'Blues', 'Dark2', 'Greens', 'Paired', 'PiYG', 'Purples', 'Reds', 'Spectral'), selected=sunbopts$sbcolors)
			)
		})
	}

#' @title sunbUI
#' @description User interface to display 'sunburst' chart type
#' @param id is the caller's id
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @export 
sunbUI<- function(id, g, noopt=0) {
	ns<- NS(id)

	sui<- box(title=g$gp$title,width=12,closable=FALSE,solidHeader=FALSE,status="info",collapsible=TRUE,collapsed=ifelse(noopt,T,F),
			sidebar=boxSidebar(id='sunside', width=25, fluidPage(uiOutput(ns('sunoptions')))), 
			fluidPage(sunburstOutput(ns('sunchart')))
			)
	sui
	}
