mapobservers<- 0
#' @title map
#' @description A 'shiny' module to display 'leaflet' chart with options
#' @details Options for 'leaflet' are shapes or circles on map, basemap, function to apply, color palette, fill opacity, and circle scale
#' 
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @import rgdal
#' @import sp
#' @importFrom leaflet leaflet addProviderTiles addCircles colorBin colorNumeric addPolygons highlightOptions renderLeaflet leafletOutput
#' @export
map<- function(input, output, session, g, noopt=0) {
	mopts<- reactiveValues(maptype='Circles', basemap="CartoDB.DarkMatter", fun="sum", colors="Set1", reverse="Y", fillopc=0.8, scale=300000, smooth="Yes")

	circout<- function(dxy, gfdim, func, basemap, colorpal) {
		mapdata <- function(dxy, gfdim, func) {
			circ<- dxy
  			circ$sum <- round(tapply(circ[,ncol(circ)], circ[,1], func))
  			attr(circ$sum,"dimnames") <- NULL
  			return(circ)
			}
 
		circ <- mapdata(dxy, gfdim, func)
		if(mopts$smooth=='Yes')
			circ$radius<- sqrt(circ$sum)
		else
			circ$radius<- circ$sum
		maxr<- max(circ$radius)
		circ$radius<- circ$radius/maxr
   		m <- leaflet() %>%
		addProviderTiles(basemap) %>%
		addCircles(data= circ, lat=~Lat, lng=~Lon, layerId=~State ,popup=~paste(State,":", "Total:", sum), radius=~mopts$scale*radius, weight=1,
		fillColor= ~colorBin(colorpal,sum,bins=3,reverse=ifelse(mopts$reverse=="Yes",T,F))(sum), fillOpacity=mopts$fillopc,stroke=FALSE)
		m
		}

	shapeout<- function(dxy, gfdim, func, basemap, colorpal) {
		shapes<- readOGR('www/shapefile.shp')
		shapes<- sp::merge(shapes, dxy, by.x='NAME', by.y=colnames(dxy)[1])
  		shapes@data$sum <- shapes@data[,ncol(shapes@data)]
  		attr(shapes@data$sum,"dimnames") <- NULL
		col <- colorNumeric(colorpal, shapes@data$sum)
		leaflet()%>%
			addProviderTiles(basemap) %>%
			addPolygons(data= shapes, stroke=NULL, color= ~col(sum), fillOpacity=0.7, popup= ~paste(NAME, ":", round(sum)),
 				highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))
		}

	output$leaf <- renderLeaflet({
		dxy<- g$dxy
		if(!is.null(f$rows_selected))
			dxy<-g$dxy[f$rows_selected,]
		if(mopts$maptype=='Circles')
			circout(dxy, g$gp$gfdim, mopts$fun, mopts$basemap, mopts$colors)
		else
			shapeout(dxy, g$gp$gfdim, mopts$fun, mopts$basemap, mopts$colors)
		})

	if(!mapobservers) {
	observe({
		if(!is.null(input$maptype))
			mopts$maptype<- input$maptype
		if(!is.null(input$basemap))
			mopts$basemap<- input$basemap
		if(!is.null(input$fun))
			mopts$fun<- input$fun
		if(!is.null(input$colors))
			mopts$colors<- input$colors
		if(!is.null(input$reverse))
			mopts$reverse<- input$reverse
		if(!is.null(input$fillopc))
			mopts$fillopc<- input$fillopc
		if(!is.null(input$scale))
			mopts$scale<- input$scale
		if(!is.null(input$smooth))
			mopts$smooth<- input$smooth
		})
	mapobservers<- 1
	}
	output$mapoptions<- renderUI({
		basemaps <- c("Stamen.Toner", "CartoDB.Positron","CartoDB.DarkMatter", "OpenStreetMap","Thunderforest.Transport","OpenStreetMap.HOT","Esri.WorldStreetMap", "Hydda.RoadsAndLabels")
		ns<- session$ns

		fluidRow(
			column(11,
    			selectInput(ns("maptype"), "Select Maptype", choices= c('Circles', 'Shapes'), selected = mopts$maptype),
    			selectInput(ns("basemap"), "Basemap Options", choices= basemaps, selected = mopts$basemap),
    			selectInput(ns("fun"), "Apply a Function", choices= c("sum", "mean", "median", "min", "max"), selected = mopts$fun),
    			selectInput(ns("colors"), "Colour Palettes", choices= c(rownames(brewer.pal.info)), selected = mopts$colors),
    			radioButtons(ns("reverse"), "Reverse Colour", c("Yes", "No"), inline=TRUE),
				sliderInput(ns("fillopc"), "Fill Opacity", min=0.1, max=0.9, value=mopts$fillopc),
				numericInput(ns("scale"), "Radius Scale", value=mopts$scale),
				radioButtons(ns("smooth"), "Use Sqrt to Smooth?", c("Yes", "No"), inline=TRUE)
				)
			)
		})
	}
		
#' @title mapUI
#' @description User interface to display 'leaflet' chart type
#' @param id is the caller's id
#' @param g is the graph/chart to be charted
#' @param noopt is a toggle that tells chart module not to display options to change chart defaults
#' @export
mapUI<- function(id, g, noopt=0) {
	ns<- NS(id)

	mui<- box(title=g$gp$title,width=12,closable=FALSE,solidHeader=FALSE,status="info",collapsible=TRUE,collapsed=ifelse(noopt,T,F),
			sidebar=boxSidebar(id='mapside', width=25, fluidPage(uiOutput(ns('mapoptions')))), 
			fluidPage(leafletOutput(ns('leaf'), height=550))
			)
		mui
	}
