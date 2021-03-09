treeobservers<- 0
topts<- reactiveValues(hctree=TRUE, pal='Set2')
#' @title tree
#' @description A 'shiny' module to display 'treemap' chart with options
#' @details Options for treemap are: interactive or static tree, and choice of color palette
#' 
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @import treemap
#' @importFrom highcharter hctreemap renderHighchart highchartOutput hc_title
#' @export
tree<- function(input, output, session, g, noopt=0) {
	ns<- session$ns

	gettree<- function(dxy, gp) {
		gfdim<- gp$gfdim
		indx<-c()
		xynam<-colnames(dxy)
		for(ix in 1:gfdim) {
			indx<-c(indx,xynam[ix])
			}
		y<-gfdim+1
		neg<-dxy[,y]<0
		dxy[neg,y]<-0
		if(topts$hctree == FALSE) {
			dxy[,gfdim]<- paste(dxy[,gfdim], dxy[,gfdim+1])
			dxy[,1]<-as.factor(dxy[,1])
			tm<-treemap(dxy,indx,xynam[gfdim+1],palette=topts$pal,type='categorical',vColor=xynam[1],fontsize.labels=10)
			}
		else
			tm<-treemap(dxy,indx,xynam[gfdim+1],palette=topts$pal, draw=FALSE)
		return(tm)
		}
	plothctree<- function(dxy, gp, gfdim) {
		if(nrow(dxy) == 0) 
			return(NULL)
		tm<-gettree(dxy, gp)
		xynam<- colnames(dxy)
	#	p<- highchart()%>%hc_title(text=gp$gftitle) %>%hc_chart(type="treemap")%>%hc_add_series(data=dxy,y=xynam[gfdim+1]) #name=xynam[gfdim+1])
		p<- hctreemap(tm, allowDrillToNode = TRUE, layoutAlgorithm = "squarified") %>% hc_title(text = gp$gftitle)
		return(p)
		}
	output$tmap<- renderPlot({
		dxy<- g$dxy
		if(!is.null(f$rows_selected))
			dxy<-g$dxy[f$rows_selected,]
		gettree(dxy, g$gp)
		})

	output$tchart<- renderHighchart({ 
		dxy<- g$dxy
		if(!is.null(f$rows_selected))
			dxy<-g$dxy[f$rows_selected,]
		g<- plothctree(dxy, g$gp, g$gp$gfdim)
		g
		})
		
	output$tree<- renderUI({	
		ns<- session$ns

		if(topts$hctree)
			highchartOutput(ns('tchart')) 
		else
			plotOutput(ns('tmap'))
		})

	if(!treeobservers) {
	observe({
		if(!is.null(input$pal))
			topts$pal<- input$pal
		if(!is.null(input$hctree))
			topts$hctree<- input$hctree
		})
	treeobservers<- 1
	}
	output$topts<- renderUI({
		fluidRow(
			checkboxInput(ns('hctree'), 'Interactive', value=topts$hctree),
			selectInput(ns('pal'), 'Palette', c('Set2', 'Blues', 'Dark2', 'Greens', 'Paired', 'PiYG', 'Purples', 'Reds', 'Spectral'), selected=topts$pal)
			)
		})

		}

#' @title treeUI
#' @description User interface to display 'treemap' chart type
#' @param id is the caller's id
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @export
treeUI<- function(id, g, noopt=0) {
	ns<- NS(id)

	tui<- box(title=g$gp$title,width=12,closable=FALSE,solidHeader=FALSE,status="info",collapsible=TRUE,collapsed=ifelse(noopt,T,F),
			sidebar=boxSidebar(id='tside', width=25, fluidPage(uiOutput(ns('topts')))), 
			fluidPage(uiOutput(ns('tree')))
			)
	tui
	}
