setwd("/home/dale/Documents/bombris/")

## set the base url for the particular station
sturl <- "http://www.bom.gov.au/climate/dwo/"
enurl <- "/text/IDCJDW4019."

## function for reading each csv
## bloody degree symbol breaks the import if encoding not set
rf <- function(sturl, enurl, mon) {
    read.csv(paste0(sturl, mon, enurl, mon, ".csv"), skip=8, header=TRUE, fileEncoding="Latin1")
}

## loop over each month for 2020 and import everything into a list
wthr <- lapply(sprintf("%s%02d", "2020", 1:10), rf, sturl=sturl, enurl=enurl)

## drop first column, which is always empty for some reason
## and bind all into one big dataset
wthr <- do.call(rbind, wthr)[-1]

## rename columns to nicer names
names(wthr) <- c(
    "date","mintemp","maxtemp","rain","evap","sunhrs","dirmaxwind","spdmaxwind","timemaxwind",
    "temp0900","relhum0900","cldamt0900","dirwind0900","spdwind0900","pres0900",
    "temp1500","relhum1500","cldamt1500","dirwind1500","spdwind1500","pres1500"
)

## convert date/time stamps
wthr$date <- as.Date(wthr$date)
wthr$timemaxwind <- as.POSIXct(paste(wthr$date, wthr$timemaxwind), format="%Y-%m-%d %H:%M")

## fill in missing values with 0 - assume okay
wthr$rain[is.na(wthr$rain)] <- 0

## reshape the chunk of variables that are for 2 times to long format
##wthr <- reshape(wthr[-(2:9)], idvar="date", varying=-1, sep="", direction="long")


## basic plotting colours etc
litecol <- "grey80"  ## lite grey
medcol  <- "grey40"  ## medium grey

palette(c("#1f7691","#d8482b","#1c8918","#b51778")) ## blue/red/line etc colours

## rain history plot for 2020
png(filename="rain.png", width=450, height=300, type="cairo", family="Mono")
{
    marg <- c(5.1, 4.1, 2.1, 2.1)
   
    op <- par(mar = marg)
    
    plot(wthr$date, wthr$rain, type="n", axes=FALSE, ann=FALSE)
    xat <- seq(as.Date("2020-01-01"), as.Date("2020-10-31"), by="month")
    abline(v=xat, h=axTicks(2), col=litecol, lty=2)
    
    axis.Date(1, wthr$date, at=xat, lwd=0, lwd.ticks=1, cex.axis=0.9, col.ticks=litecol, col.axis=medcol)
    axis(2, las=1, cex.axis=0.9, lwd=0, lwd.ticks=1, col.ticks=litecol, col.axis=medcol)

    lines(wthr$date, replace(wthr$rain, wthr$rain==0, NA), col=1, lwd=2, type="h")
    
    box(col=litecol)
    title(xlab="Date", ylab="Rain (mm)", cex.lab=0.9, font.lab=2, col.lab=medcol)
    par(op)
}
dev.off()


## temperature history plot for 2020

## min/max temps for each month for highlighting:
minix <- by(
    wthr[c("date","mintemp")],
    format(wthr$date, "%Y-%m"),
    FUN=function(x) rownames(x)[which.min(x$mintemp)]
)
wthr[minix, "minflag"] <- 1

maxix <- by(
    wthr[c("date","maxtemp")],
    format(wthr$date, "%Y-%m"),
    FUN=function(x) rownames(x)[which.max(x$maxtemp)]
)
wthr[maxix, "maxflag"] <- 1

## actual plot
png(filename="temp.png", width=450, height=300, type="cairo", family="Mono")
{
    marg <- c(5.1, 4.1, 2.1, 2.1)
   
    op <- par(mar = marg)

    ymin <- min(wthr$mintemp, na.rm=TRUE)
    ymax <- max(wthr$maxtemp, na.rm=TRUE)
    
    plot(wthr$date, wthr$maxtemp, type="n", axes=FALSE, ann=FALSE, ylim=c(ymin,ymax))
    xat <- seq(as.Date("2020-01-01"), as.Date("2020-10-31"), by="month")
    abline(v=xat, h=axTicks(2), col=litecol, lty=2)
    
    axis.Date(1, wthr$date, at=xat, lwd=0, lwd.ticks=1, cex.axis=0.9, col.ticks=litecol, col.axis=medcol)
    axis(2, las=1, cex.axis=0.9, lwd=0, lwd.ticks=1, col.ticks=litecol, col.axis=medcol)

    ## background polygon of min/max range
    ##nas <- is.na(wthr$mintemp) | is.na(wthr$maxtemp)
    ##polygon(x=c(wthr$date[!nas],rev(wthr$date[!nas])),
    ##        y=c(wthr$mintemp[!nas], rev(wthr$maxtemp[!nas])), col=litecol, border=NA)

    ## smoothed spline of min and max temperatures 
    smline <- function(x, y, spar=0.5, ...) {
        nas <- is.na(y)
        smsp <- smooth.spline(x[!nas], y[!nas], spar=spar)
        lines(smsp, ...)
    }
    smline(wthr$date, wthr$mintemp, spar=0.5, col=1, lwd=2)
    smline(wthr$date, wthr$maxtemp, spar=0.5, col=2, lwd=2)
    
    ## highlight cold/hot spots for each month
    points(wthr$date[wthr$minflag==1], wthr$mintemp[wthr$minflag==1], pch=20, col=1, cex=0.7)
    points(wthr$date[wthr$maxflag==1], wthr$maxtemp[wthr$maxflag==1], pch=20, col=2, cex=0.7)
    
    box(col=litecol)
    title(xlab="Date", ylab="Temp (C)", cex.lab=0.9, font.lab=2, col.lab=medcol)
    par(op)
}
dev.off()


## wind direction plot
compass = data.frame(
    name = c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW"),
    angle = seq(0, by=360/16, length.out=16)
)

tab  <- table(as.numeric(format(wthr$date, "%m")), wthr$dirmaxwind)
sumtab <- xtabs(spdmaxwind ~ as.numeric(format(date, "%m")) + dirmaxwind, data=wthr)

stars(stars(sumtab/tab)[,compass$name],
      scale=FALSE, len=0.6, nrow=1, ncol=10, draw.segments=TRUE, col.segments=1)
