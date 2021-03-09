skyobservers<- 0
#' @title sky
#' @description A 'shiny' module to display 'sankey' chart with options
#' @details Options for 'sankey' chart are font size and node width
#' 
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @importFrom networkD3 renderSankeyNetwork sankeyNetwork sankeyNetworkOutput 
#' @export
sky<- function(input, output, session, g, noopt=0) {
	sopts<- reactiveValues(fontsz=12, nodew=30)

	if(!skyobservers) {
	observe({
		if(!is.null(input$fontsz))
			sopts$fontsz<- input$fontsz
		if(!is.null(input$nodew))
			sopts$nodew<- input$nodew
		})
	skyobservers<- 1
	}

	output$schart<- renderSankeyNetwork({ 
		if(g$gp$gfdim < 2)
			return(NULL)
		dxy<- g$dxy
		if(!is.null(f$rows_selected))
			dxy<-g$dxy[f$rows_selected,]
		xynam<-colnames(dxy)
		san<-sanky(g$gp$gfdim,dxy)
		sankeyNetwork(Links=san$links, Nodes=san$nodes, Source="source", Target="target", Value="value", NodeID="name",
		fontSize=sopts$fontsz, nodeWidth=sopts$nodew)
		})

	output$skyopts<- renderUI({
		ns<- session$ns

		fluidRow(
			sliderInput(ns('fontsz'), 'Font Size', min=10, max=20, step=1, value=sopts$fontsz),
			sliderInput(ns('nodew'), 'Node Width', min=10, max=50, step=5, value=sopts$nodew)
			)
		})
	}

#' @title skyUI
#' @description User interface to display 'sankey' chart type
#' @param id is the caller's id
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @export
skyUI<- function(id, g, noopt=0) {
	ns<- NS(id)

	sui<- box(title=g$gp$title,width=12,closable=FALSE,solidHeader=FALSE,status="info",collapsible=TRUE,collapsed=ifelse(noopt,T,F),
			sidebar=boxSidebar(id='skyside', width=25, fluidPage(uiOutput(ns('skyopts')))), 
			fluidPage(sankeyNetworkOutput(ns('schart')))
			)
	sui
	}
