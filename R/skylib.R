#' @importFrom jsonlite fromJSON
sanky<-function(gfdim, dxy) {
	if(gfdim < 2) return(NULL)
	
	names<- c()
	for(i in 1:gfdim) {
		namesi<-unique(dxy[,i])
		names<-unique(c(names,namesi))
		}

	nodes<-createnodes(names)

	links<-list()
	for(i in 1:(gfdim-1)) {
		l<-tapply(dxy[,gfdim+1], list(dxy[,i],dxy[,i+1]), sum)
		l<-createlinks(l,names)
		links[[i]]<-l
		}

	allinks<-"["
	for(i in 1:(gfdim-1)) {
		allinks<-paste(allinks, links[[i]])
		if(i!=(gfdim-1))
			allinks<-paste(allinks, ",")
		}
	allinks<-paste(allinks,"]")

	sankey<-paste('{"nodes":', nodes, ',"links":', allinks, '}') 
	sankey<-fromJSON(sankey)
	l<-sankey$links
	sankey$links<-l[l[,(3)]!=0,]
	return(sankey)
	}

createnodes<- function(names) {
	nodes<-'['
	for(n in 1:length(names)) {
		noden<-paste('{"name":"', names[n], '"}', sep='')
		if(n < length(names))
			noden<-paste(noden, ',')
		nodes<-paste(nodes, noden)
		}
	nodes<-paste(nodes, "]")
	return(nodes)
	}

createnodes<- function(names) {
	nodes<-'['
	for(n in 1:length(names)) {
		noden<-paste('{"name":"', names[n], '","group":"', ifelse(grepl("mst",names[n]),"Mst",names[n]),'"}', sep='')
		if(n < length(names))
			noden<-paste(noden, ',')
		nodes<-paste(nodes, noden)
		}
	nodes<-paste(nodes, "]")
	return(nodes)
	}
createlinks<- function(dxy, names) {
	nc<-length(colnames(dxy))
	nr<-length(rownames(dxy))
	links<-''
	for(r in 1:nr) {
		for(c in 1:nc) {
			rn<-rownames(dxy)[r]
			cn<-colnames(dxy)[c]
			src<-match(rn,names)-1
			dest<-match(cn,names)-1
			val<-dxy[r,c]
			if(is.na(val)) 
				val<-0
			linkrc<-paste('{"source":', src, ', "target":', dest, ', "value":', val, '}', sep="")
			if(r < nr | c < nc)
				linkrc<-paste(linkrc, ',')
			links<-paste(links, linkrc)
			}
		}
	return(links)
	}
