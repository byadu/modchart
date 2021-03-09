# plotly chart with 2 dimensions and 1 series
#
twodplot<- function(gftitle, nseries, dxy, xynam, gtype) {
	x=dxy[,1]
	y=dxy[,3]
	grpby=as.factor(dxy[,2])
	xtitle=xynam[1]
	ytitle=xynam[3]

	fill= popts$fill
	if(popts$orientation == 'h') {
		x=dxy[,3]
		y=dxy[,1]
		xtitle=xynam[3]
		ytitle=xynam[1]
		if(popts$fill == 'tozeroy')
			fill='tozerox'
		}

	p<-plot_ly(data=dxy)
	p<- layout(p, xaxis=list(title=xtitle, tickangle=popts$tickangle), yaxis=list(title=ytitle), legend=list(traceorder="grouped+reversed", tracegroupgap=20), barmode=popts$barmode)
	if(gtype == 'bar')
		p<- add_trace(p,  type='bar', x=x,y=y, color=grpby, colors=popts$pal, orientation=popts$orientation)
	else if(gtype == 'line')
		p<- add_trace(p, type='scatter', mode='lines', x=x, y=y, color=grpby, fill=fill, colors=popts$pal, line=list(shape=popts$lineshape, dash=popts$linetype), name=xynam[2])
	else if(gtype == 'scatter')
			p<- add_trace(p, type='scatter', mode='markers', x=x, y=y, symbol=grpby, size=y, colors=popts$pal)
	p
	}

