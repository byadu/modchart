#' @title dyg
#' @description A 'shiny' module to display 'dygraph' chart with options
#' @details Options for 'dygraph' are range selector and line fill
#' 
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @import dygraphs
#' @export
dyg<- function(input, output, session, g, noopt=0) {
	ns<- session$ns

	output$dchart<- renderDygraph({ 
		dxy<- g$dxy
		if(!is.null(f$rows_selected))
			dxy<-g$dxy[f$rows_selected,]
		rownames(dxy)<- dxy[,1]
		dxy[,1]<-NULL
		d<- dygraph(dxy)%>% dyOptions(fillGraph=TRUE, fillAlpha=input$fill)
		if(!is.null(input$range))
			if(input$range == 'Yes')
				d<- dyRangeSelector(d) 
		d
		})

	output$dygoptions<- renderUI({
		fluidRow(
				radioButtons(ns('range'), 'Range Selector', choices=c('Yes', 'No'), inline=TRUE),
				sliderInput(ns('fill'), 'Line Fill', min=0, max=1, step=0.1, value=0)
				)
		})
	}

#' @title dygUI
#' @description User interface to display 'dygraph' chart type
#' @param id is the caller's id
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @export
dygUI<- function(id, g, noopt=0) {
	ns<- NS(id)

	dui<- box(title=g$gp$title,width=12,closable=FALSE,solidHeader=FALSE,status="info",collapsible=TRUE,collapsed=ifelse(noopt,T,F),
			sidebar=boxSidebar(id='dygside', width=25, fluidPage(uiOutput(ns('dygoptions')))),
			fluidPage(dygraphOutput(ns('dchart')))
			)
	dui
	}
