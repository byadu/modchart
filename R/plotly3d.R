# plotly chart with 3 dimensions and 1 series
#
threedplot<- function(gftitle, nseries, dxy, xynam, gtype) {
	dxy1<-aggregate(dxy[,4], list(dxy[,1],dxy[,2]), sum)
	dxy1<- as.data.frame(dxy1)
	colnames(dxy1)<-list(xynam[1], xynam[2], xynam[4])
	p<-plot_ly(type="bar", x=dxy1[,1], y=dxy1[,3], color=dxy1[,2], xaxis='x1', yaxis='y1', legendgroup="g1")

	dxy2<-aggregate(dxy[,4], list(dxy[,1],dxy[,3]), sum)
	colnames(dxy2)<-list(xynam[1], xynam[3], xynam[4])
	p<-add_trace(p, type="bar", x=dxy2[,1], y=dxy2[,3], color=dxy2[,2], xaxis='x1', yaxis='y2', legendgroup="g2")

	p <- layout(p, barmode=popts$barmode, xaxis=list(title=xynam[1]), yaxis2 = list(anchor = 'x', domain = c(0, 0.45), title=xynam[4]),
            yaxis = list(anchor = 'x', domain = c(0.55, 1), title=xynam[4]),
		legend=list(traceorder="grouped", tracegroupgap=120))
	p
	}

