heatclrs<- function(brks) {
	.<- NULL
	if(dtopts$heatclr == 'Red')
		clrs<- round(seq(255, 40, length.out = length(brks) + 1), 0) %>% {paste0("rgb(255,", ., ",", ., ")")}
	else if(dtopts$heatclr == 'Green')
		clrs<- round(seq(255, 40, length.out = length(brks) + 1), 0) %>% paste0("rgb(", ., ",255,", ., ")")
	else
		clrs<- round(seq(255, 40, length.out = length(brks) + 1), 0) %>% paste0("rgb(", ., ",", ., ",255)")
	clrs
	}

dtheat<- function(heat, dtab, dxy, gfdim, xynam) {
	if(heat == 'Table') {
		brks<- quantile(dxy[,(gfdim+1):ncol(dxy)], probs = seq(.1, .9, .1), na.rm = TRUE)
		clrs<- heatclrs(brks)
		heatab<- dtab %>% DT::formatStyle(xynam[(gfdim+1):length(xynam)], backgroundColor = DT::styleInterval(brks, clrs))
		}
	else if(heat == 'Column') {
		heatab<- dtab
		for(i in (gfdim+1):ncol(dxy)) {
			brks<- quantile(dxy[,i], probs = seq(.1, .9, .1), na.rm = TRUE)
			clrs<- heatclrs(brks)
			heatab<- heatab %>% DT::formatStyle(xynam[i], backgroundColor = DT::styleInterval(brks, clrs))
			}
		}
	heatab
	}
