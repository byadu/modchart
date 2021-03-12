ctobservers<- 0
ctopts<- reactiveValues(color=TRUE, size=TRUE)
#' @title ctree
#' @description A 'shiny' module to display 'collapsibleTree' chart with options
#' @details Options for 'collapsibleTree' are color and size
#' 
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @importFrom collapsibleTree collapsibleTreeOutput renderCollapsibleTree collapsibleTreeSummary collapsibleTree
#' @export
ctree<- function(input, output, session, g, noopt=0) {
	ns<- session$ns

	output$cchart<- renderCollapsibleTree({ 
		dxy<- g$dxy
		gfdim<- g$gp$gfdim
		if(!is.null(f$rows_selected))
			dxy<-g$dxy[f$rows_selected,]
		dxy<-dxy[complete.cases(dxy),]
		xynam<- colnames(dxy)

		if(is.na(xynam[gfdim+2]))
			attr<-"leafCount"
		else
			attr<- xynam[gfdim+2]

		if(ctopts$size)
			nsize<- xynam[gfdim+1]
		else
			nsize=NULL

	#	dxy$ctooltip<- paste(xynam[gfdim+1], dxy[,gfdim+1], xynam[gfdim+2], dxy[,gfdim+2])
		if(ctopts$color)
			ct<- collapsibleTree::collapsibleTreeSummary(dxy, xynam[1:gfdim], root=xynam[gfdim+1], nodeSize=nsize, attribute=attr)
		else
			ct<- collapsibleTree::collapsibleTree(dxy, xynam[1:gfdim], root=xynam[gfdim+1], nodeSize=nsize, tooltip=TRUE, attribute=xynam[gfdim+1])
		ct
		})

	if(!ctobservers) {
		observe({
			if(!is.null(input$color))
				ctopts$color<- input$color
			if(!is.null(input$size))
				ctopts$size<- input$size
			})
		ctobservers<- 1
		}

	output$ctoptions<- renderUI({
		fluidRow(
			checkboxInput(ns('color'), 'Color', value=TRUE),
			checkboxInput(ns('size'), 'Size', value=TRUE)
			)
		})
	}

#' @title ctreeUI
#' @description User interface to display 'collapsibleTree' chart type
#' @param id is the caller's id
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @export
ctreeUI<- function(id, g, noopt=0) {
	ns<- NS(id)

	ctui<- box(title=g$gp$title,width=12,closable=FALSE,solidHeader=FALSE,status="info",collapsible=TRUE,collapsed=ifelse(noopt,T,F),
			sidebar=boxSidebar(id='ctside', width=25, fluidPage(uiOutput(ns('ctoptions')))), 
			fluidPage(collapsibleTree::collapsibleTreeOutput(ns('cchart')))
			)
	ctui
	}
