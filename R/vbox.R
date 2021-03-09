#' @title vbox
#' @description A 'shiny' module to display 'valueBox' chart with options
#' @details This is drawn as one standard value box, with no further options
#' 
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @export
vbox<- function(input, output, session, g, noopt=0) {
	output$vbox<- renderValueBox({ 
		dxy<- g$dxy
		if(!is.null(f$rows_selected))
			dxy<-g$dxy[f$rows_selected,]
		d<- valueBox(subtitle=dxy[1,1], value=dxy[1,2])
		d
		})
	}

#' @title vboxUI
#' @description User interface to display 'valueBox' chart type
#' @param id is the caller's id
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @export
vboxUI<- function(id, g, noopt=0) {
	ns<- NS(id)

	if(noopt)
		gui<- column(12, valueBoxOutput(ns('vbox'), width=NULL))
	else {
		gui<- fluidRow(
			column(10, valueBoxOutput(ns('vbox'), width=NULL)), 
			column(2, 
				hr(),
				)
			)
		}
	gui
	}
