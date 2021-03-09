#' @export
#' @title cr 
#' @description cr is chart reactive that tracks user click on chart types
cr<- reactiveValues(gtype=NULL, dualmode=FALSE)
f<- reactiveValues(rows_selected=NULL)
chartinit<- 1

#' @rawNamespace import(shiny, except = c(renderDataTable, dataTableOutput))
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import shinyWidgets
#' @import shinyBS
#' @import rpivotTable
#' @import RColorBrewer
# @import lubridate
#' @import dplyr
#' @import lazyeval
#' @import reshape2
#' @import tidyr
#' @importFrom stats aggregate complete.cases quantile reshape
#' @importFrom utils str
#' @export

#' @title chart
#' @description A 'shiny' module to display many types of charts available as 'htmlwidgets' with a dataframe as input
#' @details The graph structure containing chart type and chart data is passed as input. 
#' @details The graph/chart data is displayed in appropriate chart type with options to change to other chart types
#' @seealso See chartex for an example
#' 
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param g is the graph/chart to be charted
#' @param setdrill is the function chart will call upstream to set a drill value on a chart
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults

# Server function chart
#' @export
chart<- function(input, output, session, g, setdrill=NULL, noopt=0) {
	ns<- session$ns

	req(g)
	cr$gtype<- g$gp$gtype
	gtype<- isolate(cr$gtype)

	if(chartinit) {
		observeEvent(input$text, ignoreInit=TRUE, { cr$dualmode<- FALSE; cr$gtype='dt' })
		observeEvent(input$plotly, ignoreInit=TRUE, { cr$gtype='bar' })
		observeEvent(input$calendar, ignoreInit=TRUE, { cr$gtype='dyg' })
		observeEvent(input$sanky, ignoreInit=TRUE, { cr$gtype='sanky' })
		observeEvent(input$tree, ignoreInit=TRUE, { cr$gtype='tree' })
		observeEvent(input$map, ignoreInit=TRUE, { cr$gtype='map' })
		observeEvent(input$sunb, ignoreInit=TRUE, { cr$gtype='sunb' })
		observeEvent(input$gvis, ignoreInit=TRUE, { cr$gtype='gvis' })
		observeEvent(input$ctree, ignoreInit=TRUE, { cr$gtype='ctree' })
		observeEvent(input$vbox, ignoreInit=TRUE, { cr$gtype='vbox' })
		observeEvent(input$dual, ignoreInit=TRUE, {
			cr$dualmode<- input$dual
			if(!cr$dualmode)
				f$rows_selected<- NULL
			})
		chartinit<- 0
		}

	output$chartout<- renderUI ({
		if(!noopt) {
			gtype<- cr$gtype
			g$gp$gtype<- gtype
			}
		else
			gtype<- g$gp$gtype
		ca<- NULL
		if(gtype == 'dt') { ca<- dtblUI(ns('chart'), g, noopt); callModule(dtbl, 'chart', g, setdrill=setdrill) }
		else if(gtype == 'bar' | gtype == 'pie' | gtype == 'line') { ca<- plotlyUI(ns('chart'), g, noopt); callModule(plotly, 'chart', g, setdrill=setdrill) }
		else if(gtype == 'sanky') { ca<- skyUI(ns('chart'), g, noopt); callModule(sky, 'chart', g, noopt) }
		else if(gtype == 'tree') { ca<- treeUI(ns('chart'), g, noopt); callModule(tree, 'chart', g, noopt) }
		else if(gtype == 'dyg') { ca<- dygUI(ns('chart'), g, noopt); callModule(dyg, 'chart', g, noopt) }
		else if(gtype == 'map') { ca<- mapUI(ns('chart'), g, noopt); callModule(map, 'chart', g, noopt) }
		else if(gtype == 'sunb') { ca<- sunbUI(ns('chrt'), g, noopt); callModule(sunb, 'chrt', g, noopt) }
		else if(gtype == 'ctree') { ca<- ctreeUI(ns('chart'), g, noopt); callModule(ctree, 'chart', g, noopt) }
		else if(gtype == 'vbox') { ca<- vboxUI(ns('chart'), g, noopt); callModule(vbox, 'chart', g, noopt) }
		ca
		})

	output$reprt<- renderUI({
		ca<- dtblUI(ns('chart'), g, noopt=0)
		callModule(dtbl, 'chart', g, noopt=0)
		ca
		})

	output$chartarea<- renderUI({
		gtype<- isolate(cr$gtype)
		if(gtype != 'dt' & cr$dualmode)
			fluidRow(column(4, uiOutput(ns('reprt'))), column(8, uiOutput(ns('chartout'))))
		else
			uiOutput(ns('chartout'))
		})
	}

#' @title chartUI
#' @description User interface to display a chart
#' @param id is the caller's id
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @export
chartUI<- function(id, g, noopt=0) {
	ns<- NS(id)

	req(g)
	nseries<- g$gp$nseries
	gfdim<- g$gp$gfdim

	if(noopt)
		chartbuts<- ''
	else {
		if(gfdim >= 2 & nseries == 1) {
			disablesbut<- FALSE
			disabletbut<- FALSE
			disablesunbut<- FALSE
			}
		else {
			disablesbut<- TRUE
			disabletbut<- FALSE
			disablesunbut<- TRUE
			}
		sbut<- bsButton(ns('sanky'),'',icon=icon('random', lib='glyphicon'), disabled=disablesbut)
		tbut<- bsButton(ns('tree'),'',icon=icon('sitemap'), disabled=disabletbut)
		sunbut<- bsButton(ns('sunb'),'',icon=icon('star'), disabled=disablesunbut)
	
		if(gfdim >= 2 & nseries <= 2)
			disablectbut<- FALSE
		else
			disablectbut<- TRUE
		ctbut<- bsButton(ns('ctree'),'',icon=icon('pagelines'), disabled=disablectbut)
	
		if(gfdim <= 2)
			disablepbut<- FALSE
		else
			disablepbut<- TRUE
		pbut<- bsButton(ns('plotly'), '', icon=icon('signal'), disabled=disablepbut)
	
		disablecbut<- FALSE
		cbut<- bsButton(ns('calendar'),'',icon=icon('calendar'), disabled=disablecbut)
	
		if(gfdim == 1) 
			disablembut<- FALSE
		else
			disablembut<- TRUE
		mbut<- bsButton(ns('map'),'',icon=icon('map'), disabled=disablembut)
	
		chartbuts<-fluidRow(
				column(12, align='right', bsButton(ns('text'),'',icon=icon('table')), bsButton(ns('dual'),'',icon=icon('object-ungroup'),type='toggle',size='default', value=FALSE), bsButton(ns('vbox'), '', icon=icon('square-o')), pbut, cbut, mbut, sbut, sunbut, tbut, ctbut),
				bsTooltip(ns('text'), 'Tabular report'),
				bsTooltip(ns('vbox'), 'Single Value Box'),
				bsTooltip(ns('plotly'), 'Bar, Line, Scatter and Pie Charts <BR> Up to 2 Metrics, 2 Attributes'),
				bsTooltip(ns('dual'), 'Table and chart side by side'),
				bsTooltip(ns('calendar'), 'Time trend chart <BR> Only 1 <i>Time</i> Attribute, N Metrics'),
				bsTooltip(ns('map'), 'Bubble chart on Map <BR> Only 1 <i>Geo</i> Attribute, 1 Metric'),
				bsTooltip(ns('sanky'), 'Data Flow Chart using Sanky Diagram <BR> 1 Metric N Attributes'),
				bsTooltip(ns('sunb'), 'Sunburst Chart <BR> 1 Metric N Attributes'),
				bsTooltip(ns('tree'), 'Treemap with rectangles <BR> 1 Metric N Attributes'),
				bsTooltip(ns('ctree'), 'Collapsible Tree Chart <BR> up to 2 Metrics N Attributes')
				)
		}
	fluidRow(
		column(12,fluidPage(chartbuts)),
		column(12, uiOutput(ns('chartarea')))
		)
	}
