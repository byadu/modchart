# plotly chart with 1 dimension with 1 or 2 series
#
onedplot<- function(gftitle, nseries, dxy, xynam, gtype) {

	if(nseries > 3) {
		dxy<- reshape(dxy, direction='long', varying=list(2:ncol(dxy)), timevar='Series', times=colnames(dxy[,2:ncol(dxy)]))
		rownames(dxy)<- NULL
		dxy<- dxy[,1:3]

		return(twodplot(gftitle, nseries, dxy, xynam, gtype))
		}

	# nseries <= 3
	p<-plot_ly(data=dxy, source=gftitle)
	if(gtype == 'pie') {
		p<-plot_ly(source=gftitle, type='pie', hole=popts$donut, direction=popts$direction, labels=as.vector(dxy[,1]), values=as.vector(dxy[,2]))
		p<- layout(p, dragmode='select', xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
			yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
			)
		return(p)
		}

	# gtype is 'bar', 'line', or 'scatter'
	x=dxy[,1]
	y=dxy[,2]
	xtitle=xynam[1]
	ytitle=xynam[2]
	fill=popts$fill
	if(popts$orientation == 'h') {
		x=dxy[,2]
		y=dxy[,1]
		xtitle=xynam[2]
		ytitle=xynam[1]
		if(popts$fill == 'tozeroy')
			fill='tozerox'
		}

	if(gtype == 'bar') {
		p<- layout(p, xaxis=list(title=xtitle,tickangle=popts$tickangle), yaxis=list(title=ytitle))
		p<-add_trace(p, type='bar', x=x, y=y, color=I(popts$color), orientation=popts$orientation, name=ytitle)
		if(nseries==2) { # combo chart with bar and line
			p<- add_trace(p, type="scatter", mode='lines', x=x, y=dxy[,3], name=xynam[3], yaxis="y2") #color=I(popts$color), name=xynam[3], yaxis="y2")
			p<- layout(p, yaxis2=list(side='right', title=xynam[3], overlaying='y'))
			}
		}
	else if(gtype == 'line') {
		p<-add_trace(p, type='scatter', mode='lines', x=x, y=y, fill=fill, color=I(popts$color), line=list(shape=popts$lineshape, dash=popts$linetype), name=xynam[2])
		}
	else if(gtype == 'scatter') {
		if(nseries==2) { 
			p<- plot_ly(data=dxy, x=dxy[,2], y=dxy[,3], text=dxy[,1]) %>% add_markers() %>% add_text(textposition='top right')
			p<- layout(p, yaxis=list(title=xynam[3]), xaxis=list(title=xynam[2]), showlegend=FALSE)
			}
		if(nseries==3) { # bubble 
			p<- add_trace(p, x=dxy[,2], y=dxy[,3], text=dxy[,1], color=dxy[,1], marker=list(size=dxy[,4], opacity=0.5))
			p<- layout(p, yaxis=list(title=xynam[3]), xaxis=list(title=xynam[2]))
			}
		else
			p<-add_trace(p, type='scatter', mode='markers',  x=x, y=y, color=I(popts$color), name=xynam[2])
		}
	p
	}

