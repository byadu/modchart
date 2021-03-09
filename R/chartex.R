#' @title modchart Examples
#' @name chartex
#' @description Use 'shiny' module 'modchart' for generating various types of charts
#' @details The data for these examples are provided in the "extdata" directory of package
#' @details The location of these can be obtained via the call:  system.file("extdata", "abcd.xlsx", "modchart")
#' @details Please select this location for accessing the files to run the example below
#' @details There are three data files supplied with this package to try out the charts. 
#' @details They are mtcars.xlsx, airpass2.xlsx, and uspop.xlsx
#' @details  mtcars.xlsx helps demonstrate multiple series in plotly
#' @details  airpass2.xlsx helps to demonstrate dygraph time series chart as well as stack bar in plotly
#' @details  uspop.xlsx helps to demonstrate map/leaflet chart 
#' @details In addition, to demonstrate choropleth, associated shape files are provided as shapefile.xxx;
#' @details   please copy these shape files into your www directory for example to work correctly
#' @examples
#' library(shiny)
#' library(modchart)
#' library(shinydashboard)
#' library(shinydashboardPlus)
#' app<- shinyApp(
#' 	ui= shinyUI(
#'	 dashboardPage(skin='purple',
#' 		header=dashboardHeader(title = 'Charts Demo'),
#' 		sidebar=dashboardSidebar(sidebarMenuOutput('sidemenu')),
#' 		body=dashboardBody(uiOutput('mainbody'))
#' 		)
#' 	 ),
#' 	server=shinyServer(function(input, output, session) {
#' 		sink(file=stderr())
#' 	
#' 		options(shiny.maxRequestSize=1*1024^2) # 1MB
#' 	
#' 		output$xl<- renderUI({
#' 			getxlUI('server')
#' 			})
#' 		xl<- callModule(getxl, 'server')
#' 	
#' 		output$charts<- renderUI({	
#' 			if(length(xl$sheets) > 0) {
#' 				title<- xl$sheets[1]
#' 				if(title == 'mtcars' | title == 'airpass2')
#' 					ndim<- 2
#' 				else
#' 					ndim<- 1
#' 				nseries<- 1
#' 				g<- xl2g(xl, ndim=ndim, nseries=nseries)
#' 				callModule(chart, 'server', g)
#' 				chartUI('server', g)
#' 				}
#' 			})
#' 		output$sidemenu<- renderMenu({
#' 			m1<- menuItem( "Upload Excel", menuSubItem("Excel", tabName="xltab"))
#' 			m2<- menuItem( "Create Chart", menuSubItem("Chart", tabName="charttab"))
#' 			sidebarMenu(m1,m2)
#' 			})
#' 
#' 		output$mainbody<- renderUI({
#' 			t1<- list(); t1[[1]]<- tabItem(tabName="xltab", uiOutput("xl"))
#' 			t2<- list(); t2[[1]]<- tabItem(tabName="charttab", uiOutput("charts"))
#' 			do.call(tabItems, c(t1,t2))
#' 			})
#' 		})
#' 	)
#' if(interactive()) {
#'	runApp(app)
#' }
#'
NULL
