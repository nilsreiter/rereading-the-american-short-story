
library(data.table)

ttr <- function(x,w=1000) {
  ds <- split(x, ceiling(seq_along(x)/w))
  
  ttr <- 0
  for (d in ds) {
    ttr <- ttr + length(levels(factor(d))) / length(d)
  }
  
  ttr/length(ds)
}

pdfbegin <- function(filename,width=7,height=4) {
  switch(doPlot,
         PDF={
            pdf(file = paste0("Plots/",filename,".pdf"),width=width,height=height)
            par(mar=c(4,4,1,0))
         },
         TIFF={
           png(file=paste0("Plots/",filename,".tiff"), width=width*300, height=height*300, res=300)
           par(mar=c(4,4,1,0))
         },
         {})
}
pdfend <- function() {
  switch(doPlot,
         NONE={},
         { dev.off() })
}


scplot <- function(formula,xlim=xlim.default,ylim=c(0,1),xlab="",ylab="",col=dblue,
                   trend=TRUE,yAt=NULL,yLabels=NULL,pch=20,axes=TRUE,log="",textpos=3,main="",symbol=TRUE,cex.lab=1,smooth=TRUE,data=NULL,sub="") {
  if (smooth) {
    #trend <- FALSE
    plot(formula,
         col=col,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,
        frame=FALSE,
        axes=FALSE,pch=pch,log=log,type=(ifelse(symbol,"p","n")),
        cex.lab = cex.lab)
    title(main=main,sub=sub,line=2.5)
  } else {
    plot(formula,
         col=col,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,
        frame=FALSE,
        axes=FALSE,pch=pch,log=log,main=main,type=(ifelse(symbol,"p","n")),
        cex.lab = cex.lab)
  }
  if (showText) {
    if (symbol) {
      text(formula, labels=data$Id,cex=0.5,pos=textpos,log=log)
    } else if (FALSE) {
      text(formula, labels=data$Id,cex=0.5,log=log,col=ifelse(data$Author=="Poe","red",ifelse(data$Author=="Hawthorne","blue","black")))
    } else {
       text(formula, labels=data$Id,cex=0.5,log=log,col="black")
    }
  }
  if (axes) {
    axis(1, at=xAt, col=anthr, col.ticks = anthr)
    axis(2, at=yAt, labels=yLabels, col=anthr, las=1)
  }
    if (trend && !is.null(data)) {
      plx<-predict(loess(formula), se=T)
      lines(data$Year,plx$fit)
      lines(data$Year,plx$fit-qt(0.995,plx$df)*plx$se,lty=2)
      lines(data$Year,plx$fit+qt(0.995,plx$df)*plx$se,lty=2)
    } else if (trend) {
      clip(xlim[1],xlim[2],ylim[1],ylim[2])
      lm  <- lm(formula)
      abline(lm,col=col)
      if (FALSE) {
        xpos <- 1910
        coef <- coefficients(lm)
        text(x=xpos,y=coef[1]+coef[2]*xpos,labels=coef[2])
      }
    
  }
}

scboxplot <- function(formula,xlim=xlim.default,ylim=c(0,1),xlab="",ylab="",
                   yAt=NULL,yLabels=NULL,pch=20,axes=TRUE,range=0,col=anthr) {
  b <- boxplot(formula,
       xlab=xlab,ylab=ylab,
       frame=FALSE, border=col,
       axes=FALSE,range=range,yaxt="n",xaxt="n",ylim=ylim)
  if (axes) {
    axis(1, at=c(1:length(b$names)), col=anthr, col.ticks = anthr,labels=b$names)
    axis(2, at=yAt, col=anthr, las=1,col.ticks = anthr)
  }
}

doPlot <- "TIFF"
doPDF <- TRUE
showText <- TRUE
par(family="Arial")

anthr <- rgb(62,68,76,alpha=255,maxColorValue = 255)
dblue <- rgb(0,81,158,alpha=255,maxColorValue = 255)
hblue <- rgb(0,0,0,maxColorValue=255)

xlim <- c(1820,1920)
xlim.default <- xlim
xAt <- c(1820,1840,1860,1880,1900,1920)


load("text.RData")

text.char <- text[text$QuoteWithId!="-",]
text.narr <- text[text$QuoteWithId=="-",]


text.basics <- text[,
                    .(sentences=max(Sentence),
                      tokens=.N,
                      slength=.N/max(Sentence),
                      PER=sum(PER=="PER"),
                      LOC=sum(LOC=="LOC"),
                      UniquePER=length(unique(FullNE)),
                      Past=sum(Tense=="B-Past"),
                      Present=sum(Tense=="B-Present"),
                      Quotes=sum(Quote=="B-Quote"),
                      InQuotes=sum(Quote=="B-Quote"|Quote=="I-Quote"),
                      Perfective=sum(Aspect=="B-Perfective"),
                      Progressive=sum(Aspect=="B-Progressive"),
                      ttr_lemma=ttr(Lemma),
                      ttr_surface=ttr(Surface),
                      Anomaly=sum(Spelling=="Anomaly")),
                    .(Year,Text=Title, Author,Era,Id)]

text.char.stat <- text.char[,
                    .(sentences=max(Sentence),
                      tokens=.N,
                      slength=.N/max(Sentence),
                      PER=sum(PER=="PER"),
                      LOC=sum(LOC=="LOC"),
                      UniquePER=length(unique(FullNE)),
                      Past=sum(Tense=="B-Past"),
                      Present=sum(Tense=="B-Present"),
                      Quotes=sum(Quote=="B-Quote"),
                      InQuotes=sum(Quote=="B-Quote"|Quote=="I-Quote"),
                      Perfective=sum(Aspect=="B-Perfective"),
                      Progressive=sum(Aspect=="B-Progressive"),
                      ttr_lemma=ttr(Lemma),
                      ttr_surface=ttr(Surface),
                      Anomaly=sum(Spelling=="Anomaly")),
                    .(Year,Text=Title, Author,Era,Id)]

text.narr.stat <- text.narr[,
                    .(sentences=max(Sentence),
                      tokens=.N,
                      slength=.N/max(Sentence),
                      PER=sum(PER=="PER"),
                      LOC=sum(LOC=="LOC"),
                      UniquePER=length(unique(FullNE)),
                      Past=sum(Tense=="B-Past"),
                      Present=sum(Tense=="B-Present"),
                      Quotes=sum(Quote=="B-Quote"),
                      InQuotes=sum(Quote=="B-Quote"|Quote=="I-Quote"),
                      Perfective=sum(Aspect=="B-Perfective"),
                      Progressive=sum(Aspect=="B-Progressive"),
                      ttr_lemma=ttr(Lemma),
                      ttr_surface=ttr(Surface),
                      Anomaly=sum(Spelling=="Anomaly")),
                    .(Year,Text=Title, Author,Era,Id)]



## Figure 1: Overview of the corpus


corpus <- text[,.N,.(Year,Title,Decade)][,.N,.(Decade)]

pdfbegin("Fig1_Corpus")
barplot(corpus$N,names.arg = corpus$Decade,col="black",border=NA,las=1)
pdfend()

## Figure 2: Story length in tokens and sentences


pdfbegin("Fig2_Length")
par(mfrow=c(2,2),mar=c(4,4,1,0),fg=anthr)
layout(matrix(c(1,2), 2, 1, byrow = TRUE))
ymax <- 60000
scplot(text.basics$tokens ~ text.basics$Year,ylim=c(0,ymax),trend=TRUE,sub="(a) Tokens",col=hblue,symbol = FALSE, data=text.basics)

ymax <- 3000
scplot(text.basics$sentences ~ text.basics$Year,axes=TRUE,
       col=hblue,
       ylim=c(0,ymax),textpos=4,trend=TRUE,sub="(b) Sentences",symbol=FALSE, data=text.basics)
pdfend()

## Sentence length


text.slen <- text[,
                  .(Length=.N),
                  .(Year,Text=Title,Sentence,Id,Author)][,
                  .(max=max(Length),mean=mean(Length),median=median(Length),sd=sd(Length)),
                  .(Year,Text,Id,Author)]

ylim=c(0,50)

pdfbegin("Fig3_Sentence_Length")
par(mar=c(4.5,2,0.5,1))
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

scplot(text.slen$mean ~ text.slen$Year,ylim=c(0,50), sub="(a) Mean",trend=TRUE,col="black",pch=4,symbol=FALSE, data=text.slen)

scplot(text.slen$max~ text.slen$Year,ylim=c(0,450),col="black",axes=TRUE, sub="(b) Maximum",pch=4,symbol=FALSE, data=text.slen)

scplot(text.slen$sd~text.slen$Year,ylim=c(0,40),col="black",sub="(c) Standard deviation",pch=4,symbol=FALSE, data=text.slen)

pdfend()





## Figure 4: Proper names


topy <- 0.055

t <- text.basics

pdfbegin("Fig4_Proper_Names")

par(col.axis=anthr,col=anthr,col.lab=anthr,mar=c(4.5,3,0,1),mfrow=c(1,2))

scplot(t$PER/t$tokens ~ t$Year,
       ylim=c(0,topy),col="black",
       main="",pch=4,symbol=FALSE,cex=0.7,data=t,
       yAt=seq(0,0.05,0.01),yLabel=paste(0:5),sub="(a) Person names (per 100 tokens)")

scplot(t$UniquePER/t$tokens ~ t$Year,ylim=c(0,0.011),sub="(b) Unique Names (per 100 tokens)",col="black",pch=4,symbol=FALSE,data=t,yAt=seq(0,0.01,0.002),yLabels=(0:5)/10)

pdfend()



## Figure 5: Type-Token-Ratio


pdfbegin("Fig5_Type_Token_Ratio")
par(mar=c(4,3,1,1))
layout(matrix(c(1,2), 1, 2, byrow = TRUE))

scplot(text.basics$ttr_surface ~ text.basics$Year, ylim=c(0.3,0.55),pch=4,col="black",symbol=FALSE, main="Standardized Type-Token-Ratio",data=text.basics,sub="(a) Overall")

scplot(text.char.stat[text.char.stat$tokens>500]$ttr_surface ~ text.char.stat[text.char.stat$tokens>500]$Year, ylim=c(0.3,0.76),symbol = FALSE, data=text.char.stat[text.char.stat$tokens>500], sub="(b) Speech (if > 500 words)")

pdfend()



## Figure 6: Direct Speech 


agg <- text[,.N,.(QuoteWithId,Year,Title,Author,Decade,Id)]

# remove non-utterances
agg <- agg[agg$QuoteWithId!="-",]


agg.stat <- agg[,.(mean=mean(N),sd=sd(N),min=min(N),max=max(N),median=median(N)),.(Year,Title,Author,Id)]


pdfbegin("Fig6_Direct-Speech")
par(mar=c(4,5,1,1))
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
scplot(text.basics$Quote/text.basics$tokens ~ text.basics$Year,ylab="% tokens",
       ylim=c(0,0.05),trend=TRUE,symbol=FALSE,sub="(a) Instances", 
       data=text.basics,yLabels=paste(0:5,"%"),yAt=seq(0,0.05,0.01))

scplot(text.basics$InQuotes / text.basics$tokens ~ text.basics$Year,symbol=FALSE,sub="(b)  Spoken tokens",data=text.basics,yAt=seq(0,1,0.2),yLabel=paste(seq(0,100,20),"%"),ylab="tokens")

scplot(agg.stat$mean ~ agg.stat$Year,ylim=c(0,130),symbol = FALSE,sub="(c) Average utterance length",data=agg.stat,ylab="tokens")

agg.stat <- agg.stat[!is.na(sd)]

scplot(agg.stat$sd ~ agg.stat$Year,ylim=c(0,225),symbol = FALSE,sub="(d) Standard deviation in utterance length",data=agg.stat,ylab="tokens")
pdfend()




## Figure 7: Irregular spelling



pdfbegin("Fig7_Spelling")
par(mar=c(4,3,1,1))
layout(matrix(c(1,2), 1, 2, byrow = TRUE))

scplot(text.basics$Anomaly/text.basics$tokens ~ text.basics$Year, ylim=c(0,0.16),symbol = FALSE, data=text.basics, sub="(a) Overall (per 100 tokens)",yAt=seq(0,0.15,0.05),yLabels=seq(0,15,5))


scplot(text.char.stat$Anomaly/text.char.stat$tokens ~ text.char.stat$Year, ylim=c(0,0.25), data=text.char.stat, symbol = FALSE, sub="(b) Speech (per 100 tokens)",yAt=seq(0,0.25,0.05),yLabels=seq(0,25,5))

pdfend()


