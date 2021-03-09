# plotly observers; set plotly options
pobserv<- function(input, popts, g, setdrill=NULL) {
	observeEvent(input$donut, ignoreInit=TRUE, { popts$donut<- input$donut })
	observeEvent(input$pal, ignoreInit=TRUE, { popts$pal<- input$pal })
	observeEvent(input$color, ignoreInit=TRUE, { popts$color<- input$color })
	observeEvent(input$lineshape, ignoreInit=TRUE, { popts$lineshape<- input$lineshape })
	observeEvent(input$orientation, ignoreInit=TRUE, {
		if(!is.null(input$orientation) && input$orientation == 'Horizontal')
			popts$orientation<- 'h'
		else
			popts$orientation<- 'v'
		})
	observeEvent(input$tickangle, ignoreInit=TRUE, {
		if(!is.null(input$tickangle) && input$tickangle == 'Slanted')
			popts$tickangle<- -45
		else
			popts$tickangle<- 0
		})
	observeEvent(input$linetype, ignoreInit=TRUE, {
		if(!is.null(input$linetype) && input$linetype != 'solid')
			popts$linetype<- input$linetype
		else
			popts$linetype<- 'solid'
		})
	observeEvent(input$area, ignoreInit=TRUE, {
		if(!is.null(input$area) && input$area == 'Filled')
			popts$fill<- 'tozeroy'
		else
			popts$fill<- ''
		})
	observeEvent(input$barmode, ignoreInit=TRUE, {
		if(!is.null(input$barmode)) {
			popts$barmode<- ifelse(input$barmode==1,'stack','group')
			}
		})
	observeEvent(input$direction, ignoreInit=TRUE, {
		popts$direction<- input$direction
		})
	observeEvent(input$bar, ignoreInit=TRUE, {
		popts$ptype<- 'bar'
		})
	observeEvent(input$pie, ignoreInit=TRUE, {
		popts$ptype<- 'pie'
		})
	observeEvent(input$line, ignoreInit=TRUE, {
		popts$ptype<- 'line'
		})
	observeEvent(input$scatter, ignoreInit=TRUE, {
		popts$ptype<- 'scatter'
		})
	observeEvent(input$lmargin, ignoreInit=TRUE, {
		popts$lmargin<- input$lmargin
		})
	observeEvent(input$bmargin, ignoreInit=TRUE, {
		popts$bmargin<- input$bmargin
		})
	observeEvent(pevents$click, ignoreNULL=T, ignoreInit=T, {
		if(isolate(pevents$src) == g$gp$title)
			if(!is.null(setdrill)) {
				setdrill(g, isolate(pevents$click$pointNumber+1))
				}
		})
	}
