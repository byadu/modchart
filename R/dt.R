dtobservers<- 0
dtopts<- reactiveValues(heat='none', heatclr="Red", sl='none')
#' @title dtbl
#' @description A 'shiny' module to display 'DT' chart with options
#' @details Options for 'DT' are column and table heatmaps, and 'sparklines' on the last dimension
#' 
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param g is the graph/chart to be charted
#' @param setdrill is the function to chart will call upstream to set a drill value on a chart
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @importFrom DT dataTableOutput renderDataTable datatable 
#' @export
dtbl<- function(input, output, session, g, setdrill=NULL, noopt=0) {
	ns<- session$ns

	output$dt<- DT::renderDataTable(server=TRUE,{

		dxy<- g$dxy
		gfdim<- g$gp$gfdim
		xynam<- names(dxy)
		nseries<- g$gp$nseries

		dxy[,1]<- as.factor(dxy[,1])
		if(dtopts$sl != 'none') {
			dtab<- sparklines(gfdim, nseries, dxy, xynam)
			}
		else {
			if(noopt)
				dtab<- DT::datatable(dxy, escape=FALSE, class='compact', options=list(pageLength=5))
			else
				dtab<- DT::datatable(dxy, escape=FALSE, class='compact', filter='top', options=list(autowidth=FALSE, dom='<"top" Bi>t<"bottom" lp><"clear">', pageLength=10))

			if(dtopts$heat != 'none')
				dtab<- dtheat(dtopts$heat, dtab, dxy, gfdim, xynam)
			}
		dtab
  		})

	if(!dtobservers) {
	observeEvent(input$dt_rows_all, {
		if(cr$dualmode)
			f$rows_selected<- input$dt_rows_all
		})
	observeEvent(input$dt_rows_selected, ignoreNULL=TRUE, ignoreInit=TRUE,{
		if(!is.null(setdrill))
			setdrill(g, input$dt_rows_selected)
		})

	observe({
		if(!is.null(input$heat))
			dtopts$heat<- input$heat
		if(!is.null(input$heatclr))
			dtopts$heatclr<- input$heatclr
		if(!is.null(input$sl))
			dtopts$sl<- input$sl
		})
	dtobservers<- 1
	}

	output$dopts<- renderUI({
		ns<- session$ns

			fluidRow(
			radioButtons(ns("heat"), "Heatmap", choices= c("None", "Column", "Table"), inline=TRUE, selected=dtopts$heat),
			radioButtons(ns("heatclr"), "Heatmap Color", choices= c("Red", "Green", "Blue"), inline=TRUE, selected=dtopts$heatclr),
			selectInput(ns('sl'), 'Sparkline', choices= c("none", "bar", "line", "box"), selected=dtopts$sl)
			)
		})
	}

#' @title dtblUI
#' @description User interface to display 'DT' chart type
#' @param id is the caller's id
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @export
dtblUI<- function(id, g, noopt=0) {
	ns<- NS(id)
	if(noopt) {
		dui<- ''
		}
	else {
		dui<- uiOutput(ns('dopts'))
		}

	box(title=g$gp$title, width=12, closable=FALSE, solidHeader=FALSE, status="info", collapsible=TRUE, collapsed=ifelse(noopt,T,F),
		sidebar=boxSidebar(id='dtside', width=25, fluidPage(dui)),
		fluidPage(DT::dataTableOutput(ns('dt')))
		)
	}

