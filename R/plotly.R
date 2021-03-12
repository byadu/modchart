plotlyobservers<- 0
pevents<- reactiveValues(click=NULL, src=NULL, clickreg=NULL)
popts<- reactiveValues(ptype=NULL, color='RoyalBlue', orientation='v', tickangle=0, 
						linetype='solid', lineshape='linear', fill='Line', 
						direction='clockwise', donut=0,
						pal='Set2',
						barmode='group',
						lmargin=50,
						bmargin=50)

#' @title plotly
#' @description A 'shiny' module to display 'plot_ly' chart with options
#' @details Options for 'plotly' are provided for bar, line, scatter and pie charts
#' @details Common options are tick angle for x axis, margin width/height, vertical or horizontal orientation, color/palette
#' @details  Additional option for bar chart option is stacked bar chart  
#' @details  Additional options for line chart option are line type, line shape, and area fill
#' @details  Additional options for pie chart option are donut size and clockwise/counter-clockwise drawing
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param g is the graph/chart to be charted
#' @param setdrill is the function to chart will call upstream to set a drill value on a chart
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @importFrom plotly plot_ly config layout event_register event_data renderPlotly plotlyOutput add_trace add_markers add_text
#' @export
plotly<- function(input, output, session, g, setdrill=NULL, noopt=0) {

	doplotly<- function(dxy, gp, gfdim, gtype) {
		if(gfdim > 3)
			return(NULL)
	
		gftitle<-gp$title
		nseries<-gp$nseries
	
		dxy<-dxy[dxy[,gfdim+1]>0,] # plot only positive values
		xynam<-colnames(dxy)
		if(gfdim==3) {
			p<- threedplot(gftitle, nseries, dxy, xynam, gtype)
			}
		else if(gfdim==2) {
			p<- twodplot(gftitle, nseries, dxy, xynam, gtype)
			}
		else if(gfdim == 1) {
			p<- onedplot(gftitle, nseries, dxy, xynam, gtype)
			}
		else if(gfdim == 0) {
			x<- as.vector(rep(dxy[,1], dxy[,2]))
			p<-plot_ly(x=x, type="histogram")
			}
		p<- p %>% config(displayModeBar=FALSE)
		p<- p %>% layout(margin=list(l=popts$lmargin, b=popts$bmargin))
		p<- p %>% event_register('plotly_click')
		pevents$clickreg<- 1
		p
		}

	observeEvent(event_data(event="plotly_click", source=g$gp$title), {
		pevents$src<- g$gp$title
		pevents$click<- event_data(event="plotly_click", source=g$gp$title)
		})

	output$pchart<- renderPlotly({
		gtype= g$gp$gtype
		if(!is.null(popts$ptype))
			gtype= popts$ptype
		dxy<- g$dxy
		if(!is.null(f$rows_selected))
			dxy<-g$dxy[f$rows_selected,]
		p<-doplotly(dxy, g$gp, g$gp$gfdim, gtype)
		p$elementId<-NULL
		p
		})

	if(!plotlyobservers) {
		pobserv(input, popts, g, setdrill=setdrill)
		plotlyobservers<- 1
		}

	output$plotoptions<- renderUI({
		ns<- session$ns

		barmode= checkboxInput(ns('barmode'), 'Stack Bar', value= (popts$barmode=='stack'))
		tickangle= selectInput(ns('tickangle'), 'Tick Angle', c('Normal', 'Slanted'), selected=ifelse(popts$tickangle==0, 'Normal', 'Slanted'))
		orientation= selectInput(ns('orientation'), 'Orientation', c('Vertical', 'Horizontal'), selected=ifelse(popts$orientation=='h', 'Horizontal', 'Vertical'))
		color= selectInput(ns('color'), 'Color', c('Royalblue', 'Black', 'Brown', 'Gold', 'Gray', 'Hotpink', 'Indianred', 'Khaki', 'Orchid', 'Purple', 'Red', 'Tomato', 'Yellowgreen'), selected=popts$color)

		lineshape= selectInput(ns('lineshape'), 'Line Shape', c('linear', 'spline', 'vhv', 'hvh', 'vh', 'hv'), selected=popts$lineshape)
		linetype= selectInput(ns('linetype'), 'Line Type', c('solid', 'dash', 'dot'), selected=popts$linetype)
		area= selectInput(ns('area'), 'Area Chart', c('Line', 'Filled'), selected=ifelse(popts$fill=='tozeroy','Filled','Line'))

		direction= selectInput(ns('direction'), 'Direction', c('clockwise', 'counter-clockwise'), selected=popts$direction)
		donut= sliderInput(ns('donut'), 'Donut', min=0, max=1, step=0.1, value=popts$donut)

		pal= selectInput(ns('pal'), 'Color Palette', c('Set2', 'Blues', 'Dark2', 'Greens', 'Paired', 'PiYG', 'Purples', 'Reds', 'Spectral'), selected=popts$pal)

		lmargin= sliderInput(ns('lmargin'), label='Left Margin', min=10, max=200, value=popts$lmargin, step=10)
		bmargin= sliderInput(ns('bmargin'), label='Bottom Margin', min=10, max=200, value=popts$bmargin, step=10)

		gtype= g$gp$gtype
		if(!is.null(popts$ptype))
			gtype= popts$ptype

		if(length(g$gp$series)>1 | g$gp$gfdim > 1)
			colorpal<- pal
		else
			colorpal<- color

		if(gtype == 'line')
			fluidRow(colorpal, orientation, tickangle, lineshape,linetype,area, bmargin, lmargin)
		else if(gtype == 'bar')
			fluidRow(barmode, colorpal, orientation, tickangle, bmargin, lmargin)
		else if(gtype == 'scatter')
			fluidRow(colorpal,orientation,tickangle,bmargin,lmargin)
		else if(gtype == 'pie')
			fluidRow(direction,donut)
		})
	}

#' @title plotlyUI
#' @description User interface to display 'plot_ly' chart type
#' @param id is the caller's id
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @export
plotlyUI<- function(id, g, noopt=0) {
	ns<- NS(id)

	if(g$gp$gfdim > 1)
		disablepie<- TRUE
	else
		disablepie<- FALSE

		if(noopt) ptypes<- ''
		else
		ptypes<- fluidRow(
			column(11, align='right',
				bsButton(ns('bar'), '', icon=icon('bar-chart')),
				bsButton(ns('line'), '', icon=icon('line-chart')),
				bsButton(ns('scatter'), '', icon=icon('equalizer', lib="glyphicon")),
				bsButton(ns('pie'), '', icon=icon('pie-chart'), disabled=disablepie)
				),
				bsTooltip(ns('bar'), 'Bar, Stacked Bar'),
				bsTooltip(ns('line'), 'Line, Area'),
				bsTooltip(ns('scatter'), 'Scatter, Bubble'),
				bsTooltip(ns('pie'), 'Pie, Donut')
			)
	pchart<- box(title=g$gp$title, width=12, closable=FALSE, solidHeader=FALSE, status="info", collapsible=TRUE, collapsed=ifelse(noopt,T,F), sidebar=boxSidebar(id='plside', width=25, fluidPage(uiOutput(ns('plotoptions')))), fluidPage(fluidRow(ptypes, plotlyOutput(ns('pchart')))))
	pchart
	}
