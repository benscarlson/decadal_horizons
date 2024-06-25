
.mkdir=function(x){if(!file.exists(x)) dir.create(x)}

size=function(x){paste0(round(object.size(x)/1e6,1),' Mb')}

chopTasks=function(x,n) split(x, cut(seq_along(x), n, labels  = FALSE))


fdZoomPlot=function(rr,plot.f=NULL,shp=NULL,open=F,mc.cores=6,regions=1:6,zlims=NULL,plot.labs=NULL,
	legend.args=list(text=NULL,line=3,side=3,cex=4,adj=c(0, 
        0.5)),
	axis.args=list(cex.axis=6),lab.cex=12,quantileBreaks=FALSE,col.bias=1,
	form.args=list(digits=NULL,scientific=NA,round=16),...){
	
	# for testing
	# zlims=NULL
	# regions=1:6; plot.labs=NULL; legend.args=list(text=NULL,line=3,side=3,cex=4,adj=c(0, 0.5)); axis.args=list(cex.axis=6);lab.cex=12;quantileBreaks=FALSE; col.bias=1; form.args=list(digits=NULL,scientific=NA,round=16)

	regionBounds=data.frame(na=c(-168,-50,-1,74), eur=c(-15,70,33,73),asia=c(40,169,-14,72),sa=c(-130,-21,-60,20), afr=c(-30,70,-40,40), aus=c(112,180,-52,-6))
	regionBreaks=data.frame(na=c(-168,-50,-1,74), eur=c(-15,70,33,73),asia=c(40,169,-14,72),sa=c(-130,-21,-60,20), afr=c(-30,70,-40,40), aus=c(112,180,-52,-6))
	#regionBreaks=data.frame(na=c(-180e5,-20e5,-5e5,90e5), eur=c(-10e5,80e5,40e5,80e5),asia=c(1e5,170e5,-25e5,90e5),sa=c(-105e5,-30e5,-65e5,170e5), afr=c(-35e5,85e5,-45e5,95e5), aus=c(90e5,190e5,-75e5,-3e5))

	if(!is.null(plot.f)) {
		png(plot.f,h=700*2*2,w=1000*3*2)
		#par(mfrow=c(2,3),mar=c(0,3,0,19),oma=c(2,2,15,6))
	}
	
	this.rich=mclapply(regions,function(i){
		aa=raster::crop(rr,extent(regionBreaks[,i]))
		bproj='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
		#ex=extent(aa)
		#bproj=paste0("+proj=laea +lat_0=",mean(regionBounds[3:4,i])," +lon_0=",mean(regionBounds[1:2,i])," +x_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs=0,0,0")
		projectRaster(aa, crs=CRS(bproj),method='ngb')
	},mc.cores=mc.cores)
	
	for(i in seq_along(this.rich)){
		if(is.null(zlims)) { zs=c(minValue(this.rich[[i]]),maxValue(this.rich[[i]])) 
		} else{zs=zlims} #else { zs=zlims[i,]}
		if(quantileBreaks){
			myBreaks=unique(quantile(values(this.rich[[regions[i]]]), probs=seq(0,1,length=100),na.rm=T))
		} else {
			myBreaks=unique(seq(zs[1],zs[2],length=99))
		}
		ax.labs=round(myBreaks[seq(1,length(myBreaks),length=8)], form.args$round)
		ax.labs= format(ax.labs,digits=form.args$digits,scientific=form.args$scientific)
		cols=c('white',cm.cols1(length(myBreaks)-2,bias=col.bias))
		
		# from fields
		old.par <- par(no.readonly = TRUE)
		graphics.reset=FALSE; add=FALSE
		#== map 
		bigplot=c(0,.81,0,1) # manual change
		par(plt = bigplot)
		image(this.rich[[regions[i]]],zlim=zs, axes=T,xlab="",ylab="",xaxt='n',yaxt='n',col= cols, xlim=regionBounds[1:2,regions[i]],ylim=regionBounds[3:4,regions[i]],breaks=myBreaks,main='')#,...) #legend.args=legend.args, axis.args=axis.args,smallplot= c(.9,.93,.1,.9),
    big.par <- par(no.readonly = TRUE)
		
		plot(shp,add=T,lwd=5)
		
		if(!is.null(plot.labs)){
		 pu=par('usr')
		 xx=pu[1]+.1*(pu[2]-pu[1])
		 yy=pu[4]-.1*(pu[4]-pu[3])
		 text(xx,yy,plot.labs[i],cex=lab.cex,font=2)
		}
				
		#== legend
		smallplot=c(.82,.87,.05,.9) # my manual change
    par(new = TRUE, pty = "m", plt = smallplot, err = -1)
		barplot(rep(2, length(cols)),axes = F, space = 0, col= cols, horiz=T,border=NA)#,legend.text=legend.args$text,args.legend=list(x=5.5q,y=125,cex=legend.args$cex,bty='n',col=grey(1,0),border=grey(1,0),adj=legend.args$adj))
		axis(4,at=seq(0,length(cols),length=length(ax.labs)),label=ax.labs, line=0,cex.axis=axis.args$cex.axis,las=1)
	 	par(plt = c(0,1,0,1))
	 	pu=par('usr')
	 	t.x=1.2 * diff(pu[1:2]) + pu[1]
 		t.y=1.1* diff(pu[3:4]) + pu[3]
		text(t.x,t.y,label=legend.args$text,cex=legend.args$cex,xpd=NA, adj=legend.args$adj)

		#== from fields
		mfg.save <- par()$mfg
    if (graphics.reset | add) {
        par(old.par)
        par(mfg = mfg.save, new = FALSE)
        invisible()
    }
    else {
        par(big.par)
        par(plt = big.par$plt, xpd = FALSE)
        par(mfg = mfg.save, new = FALSE)
        invisible()
    }
     
	}
	
	if(!is.null(plot.f)){ 
		dev.off()
		if(open) system(paste0('open ',plot.f))
	}
}




# from http://rfunctions.blogspot.com/2015/03/bivariate-maps-bivariatemap-function.html
# rasterx: It is the raster to be represented as the x axis in the bivariate map.
# rastery: It is the raster to be represented as the y axis in the bivariate map.
# colormatrix: Color matrix to be used to generate the bivariate map.
# nquantiles: The same number of quantiles you specified when generating the color matrix.
# # bivariate.map<-function(rasterx, rastery, colormatrix=col.matrix, nquantiles=10){
# # quanmean<-getValues(rasterx)
# # temp<-data.frame(quanmean, quantile=rep(NA, length(quanmean)))
# # brks<-with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
# # r1<-within(temp, quantile <- cut(quanmean, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
# # quantr<-data.frame(r1[,2]) 
# # quanvar<-getValues(rastery)
# # temp<-data.frame(quanvar, quantile=rep(NA, length(quanvar)))
# # brks<-with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
# # r2<-within(temp, quantile <- cut(quanvar, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
# # quantr2<-data.frame(r2[,2])
# # as.numeric.factor<-function(x) {as.numeric(levels(x))[x]}
# # col.matrix2<-colormatrix
# # cn<-unique(colormatrix)
# # for(i in 1:length(col.matrix2)){
# # ifelse(is.na(col.matrix2[i]),col.matrix2[i]<-1,col.matrix2[i]<-which(col.matrix2[i]==cn)[1])}
# # cols<-numeric(length(quantr[,1]))
# # for(i in 1:length(quantr[,1])){
# # a<-as.numeric.factor(quantr[i,1])
# # b<-as.numeric.factor(quantr2[i,1])
# # cols[i]<-as.numeric(col.matrix2[b,a])}
# # r<-rasterx
# # r[1:length(r)]<-cols
# # return(r)}

bivariate.map<-function(rasterx,
                        rastery,
                        colormatrix=col.matrix,
                        nquantiles=NULL,
                        revXAxis=FALSE,revYAxis=FALSE,
                        myXBreaks=NULL,myYBreaks=NULL,remove0s=FALSE){
	# for testing:  colormatrix=col.matrix; nquantiles=10; revXAxis=FALSE; revYAxis=FALSE; myXBreaks=NULL; myYBreaks=NULL; remove0s=T
	# rasterx=expos; rastery=toplot
	# rasterx=expos.eco; rastery=toplot.eco; colormatrix=col.matrix2; nquantiles=4;  myXBreaks=myXBreaks2; myYBreaks=myYBreaks2; revXAxis=FALSE; revYAxis=FALSE; remove0s=FALSE
  if(is.null(nquantiles) & is.null(myXBreaks) & is.null(myYBreaks)){
    stop('hey dummy, specify some breaks')
  }

  quanmean<-getValues(rasterx)
  if(remove0s) quanmean=quanmean[!quanmean==0]
  temp<-data.frame(quanmean, quantile=rep(NA, length(quanmean)))
  if(is.null(myXBreaks)){
    brks<-unique(with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles)))))
    if(revXAxis) brks=rev(brks)
  } else { brks=myXBreaks}
  ##r1<-within(temp, quantile <- cut(quanmean, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
  r1<-within(temp, quantile <- cut(quanmean, breaks = brks, labels = 1:(length(brks)-1),include.lowest = TRUE))

  ##quantr<-data.frame(r1[,2])
  quantr<-data.frame(as.numeric(r1[,2]))

  quanvar<-getValues(rastery)
  temp<-data.frame(quanvar, quantile=rep(NA, length(quanvar)))
  if(is.null(myYBreaks)){
    brks<-unique(with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles)))))
    if(revYAxis) brks=rev(brks)
  } else { brks=myYBreaks}

  ##r2<-within(temp, quantile <- cut(quanvar, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
  r2<-within(temp, quantile <- cut(quanvar, breaks = brks, labels = 1:(length(brks)-1),include.lowest = TRUE))

  ##quantr2<-data.frame(r2[,2])
  quantr2<-data.frame(as.numeric(r2[,2]))

  ##as.numeric.factor<-function(x) {as.numeric(levels(x))[x]}

  col.matrix2<-colormatrix
  cn<-unique(colormatrix)
  for(i in 1:length(col.matrix2)){
    ifelse(is.na(col.matrix2[i]),col.matrix2[i]<-1,col.matrix2[i]<-which(col.matrix2[i]==cn)[1])
  }

  cols<-numeric(length(quantr[,1]))
  col.matrix2=apply(col.matrix2,2,as.numeric)

  # failed attemp to speed up; never finishes.
  # cols=rep(NA,length(quantr2[,1]))
  # not.na=which(complete.cases(data.frame(quantr2[,1],quantr[,1])))
  # cols1<-col.matrix2[quantr2[not.na,1],quantr[not.na,1]]

  for(i in 1:length(quantr[,1])){ cols[i]<-col.matrix2[quantr2[i,1],quantr[i,1]] }

  # for(i in 1:length(quantr[,1])){
  #   a<-as.numeric.factor(quantr[i,1])
  #   b<-as.numeric.factor(quantr2[i,1])
  #   cols[i]<-as.numeric(col.matrix2[b,a])
  # }
  r<-rasterx
  r[1:length(r)]<-cols
  return(r)
}

#=====================================================================
# Function to make the colour matrix for the figure legend, with custom x and y labels - Chris T
#' @export
colmatxy<-function(nquantiles=NULL,
                   upperleft=rgb(0,150,235, maxColorValue=255),
                   upperright=rgb(130,0,80, maxColorValue=255),
                   bottomleft="grey",
                   bottomright=rgb(255,230,15, maxColorValue=255),
                   xlab="x label", ylab="y label",
                   brksx=NULL, brksy=NULL,
                   tckspotsx=seq(0,1,length=length(brksx)), tckspotsy=seq(0,1,length=length(brksy)),cex.axis=1.2,...){

	#  nquantiles=nCats; upperleft='red1'; upperright='steelblue3'; bottomleft="gold1"; bottomright='grey';  xlab=''; ylab=''; brksx=NULL; brksy=NULL
  my.data<-seq(0,1,.01)
  my.class<-classIntervals(my.data,n=nquantiles,style="quantile")

  my.pal.1<-findColours(my.class,c(upperleft,bottomleft))
  my.pal.2<-findColours(my.class,c(upperright, bottomright))
  col.matrix<-matrix(nrow = 101, ncol = 101, NA)
  for(i in 1:101){
    my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[102-i,]<-findColours(my.class,my.col)
  }

  par(mai=c(1.2,1.2,0.5,0.5))
  plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F,axes=F, xlab=xlab, ylab=ylab,cex.lab=1.7,...) # change axis label size
  axis(side=1, at=tckspotsx, labels=round(brksx,2), cex.axis=cex.axis)
  axis(side=2, at=tckspotsy, labels=round(brksy,2), cex.axis=cex.axis)
  for(i in 1:101){
    col.temp<-col.matrix[i-1,]
    points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex=1)
  }

  # seqs<-seq(0,100,(100/nquantiles))
  # seqs[1]<-1
  # col.matrix <- col.matrix[c(seqs), c(seqs)]
  seqs=seq(1,101,length=nquantiles)
  col.matrix[c(seqs), c(seqs)]
}
#=====================================================================


#' Inset the bivariate chloropleth in a map
#' @export
	insetBCScale=function(bounds,col.matrix,
                      xlab,ylab,
                      brksx, brksy,
                      tckspotsx=seq(0,1,length=length(brksx)),
                      tckspotsy=seq(0,1,length=length(brksy)),
                      scaling=1,
                      cell.cex=2){
  # bounds=c(0.25,0.33, .25, .44); xlab='# Species exposed';ylab='% Species Recorded'; brksx=myXBreaks; brksy=myYBreaks;scaling=.6; cell.cex=3;  tckspotsx=seq(0,1,length=length(brksx)); tckspotsy=seq(0,1,length=length(brksy))
  #grid::roundrectGrob(x=mean(bounds[1:2]), y=mean(bounds[3:4]), width=diff(bounds[1:2])+.02,
  # height=diff(bounds[3:4])+.02)
  ncx=grconvertX(bounds[1:2], "ndc","user")
  ncy=grconvertY(bounds[3:4], "ndc","user")
  rect(ncx[1]-19,ncy[1]-7,ncx[2]+2,ncy[2]+2,col = grey(.97,1))
  par(fig = bounds,mar=c(1,1,0,0),new = T)
  plot(c(1,1),pch=19,col='white',xlim=c(-.05,1.05),ylim=c(-.05,1.05), frame.plot=F,xaxt='n',yaxt='n',xlab='',ylab='')
  axis(1,at=tckspotsx, labels=brksx,line=-1,cex.axis=1*scaling,mgp=c(2,.9*scaling,0),tck=-.03*scaling) # these might be qunatiles!
  axis(2,at=tckspotsy, labels=brksy,line=.2,cex.axis=1*scaling,mgp=c(2,.9*scaling,0),tck=-.03*scaling,las=1)
  mtext(xlab,1,line=1.2*scaling,cex=1*scaling)
  mtext(ylab,2,line=3*scaling,cex=1*scaling)
  res=nrow(col.matrix)
  for(i in 1:res){
    col.temp<-col.matrix[i-1,]
    points(seq(0,1,length=res),rep((i-1)/(res-1),res),pch=15,col=col.temp, cex=cell.cex*scaling)
  }
}
	

# # 
# # colmat<-function(nquantiles=10, upperleft=rgb(0,150,235, maxColorValue=255), upperright=rgb(130,0,80, maxColorValue=255), bottomleft="grey", bottomright=rgb(255,230,15, maxColorValue=255), xlab="x label", ylab="y label"){
# # 	 my.data<-seq(0,1,.01)
# # 	 my.class<-classIntervals(my.data,n=nquantiles,style="quantile")
# # 	 my.pal.1<-findColours(my.class,c(upperleft,bottomleft))
# # 	 my.pal.2<-findColours(my.class,c(upperright, bottomright))
# # 	 col.matrix<-matrix(nrow = 101, ncol = 101, NA)
# # 	 for(i in 1:101){
# # 	 my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
# # 	 col.matrix[102-i,]<-findColours(my.class,my.col)}
# # 	 plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=1.3)
# # 	 for(i in 1:101){
# # 	 col.temp<-col.matrix[i-1,]
# # 	 points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex=1)}
# # 	 seqs<-seq(0,100,(100/nquantiles))
# # 	 seqs[1]<-1
# # 	 col.matrix<-col.matrix[c(seqs), c(seqs)]
# # }