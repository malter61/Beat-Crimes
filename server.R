library(shiny)
library(data.table)
#install.packages("googleVis")

require(ggplot2)
require(plyr)
#install.packages("lubridate")
require(lubridate)
#devtools::install_github('rstudio/rscrypt')
#addAuthorizedUser("lighthouse")
beat.crimes <- fread("http://data.cityofchicago.org/views/x2n5-8w5q/rows.csv")
names(beat.crimes) <- gsub(' ','.',names(beat.crimes))
### FUNCTIONS TO CONVERT GEO CODES INTO MILES.
lat.dist.miles <- function(lat1,lat2){return(69.172*(lat2-lat1))}
long.dist.miles <- function(long1,long2,lat){return(69.172*(long2-long1)*cos(pi*lat/180))}
total.dist.miles <- function(lat1,lat2,long1,long2){return(sqrt(lat.dist.miles(lat1,lat2)^2 + 
                                                                  long.dist.miles(long1,long2,((lat1+lat2)/2))^2))}
hour.bucket <- seq(hour(Sys.time()),hour(Sys.time())+3)
weekday <- weekdays(Sys.Date())

#beat.crimes <- paste("https://www.dropbox.com/s/rwiyrjkd43r3b6j/beat.crimes.csv?dl=0")
#beat.crimes <- repmis::source_data(beat.crimes, sep = ",", header = TRUE)
#beat.crimes <- beat.crimes[complete.cases(beat.crimes),]
#beat.crimes <- beat.crimes[,c(3,6,9,11,16,17)]
beat.crimes$DATE..OF.OCCURRENCE <- as.character(beat.crimes$DATE..OF.OCCURRENCE)
beat.crimes$PRIMARY.DESCRIPTION <- as.character(beat.crimes$PRIMARY.DESCRIPTION)
beat.crimes$ARREST <- as.character(beat.crimes$ARREST)

#beat.crimes <- read.csv("http://data.cityofchicago.org/views/x2n5-8w5q/rows.csv",header=TRUE,as.is=TRUE)
#beat.crimes <- beat.crimes[,c(2,5,8,10,15,16)]
beat.crimes <- beat.crimes[complete.cases(beat.crimes),]

### FOR THE FUNCTION TO GRAB THE TOP CRIMES OVER THE NEXT FOUR HOUR BUCKET, WE MIGHT NEED TO GO INTO THE NEXT DAY
### (IF WE'RE ALREADY PAST 9:00 PM).  SO I CREATED A nextday VARIABLE WHICH IS USED IN THE FUNCTION LOGIC TO GRAB 
### THE FIRST 1, 2, OR 3 HOURS IN THE NEXT DAY IF NECESSARY.

if(weekday=="Sunday"){nextday="Monday"
}else if(weekday=="Monday"){nextday="Tuesday"
}else if(weekday=="Tuesday"){nextday="Wednesday"
}else if(weekday=="Wednesday"){nextday="Thursday"
}else if(weekday=="Thursday"){nextday="Friday"
}else if(weekday=="Friday"){nextday="Saturday"
}else {nextday="Sunday"}

longitude <- (max(beat.crimes$LONGITUDE,na.rm=T)+min(beat.crimes$LONGITUDE,na.rm=T))/2
latitude <- (max(beat.crimes$LATITUDE,na.rm=T)+min(beat.crimes$LATITUDE,na.rm=T))/2
beat.crimes$x.miles <- long.dist.miles(longitude,beat.crimes$LONGITUDE,latitude)
beat.crimes$x.miles <- round(beat.crimes$x.miles*100)/100
beat.crimes$y.miles <- lat.dist.miles(latitude,beat.crimes$LATITUDE)
beat.crimes$y.miles <- round(beat.crimes$y.miles*100)/100
beat.crimes$distance.from.officer <- total.dist.miles(beat.crimes$LATITUDE,latitude,beat.crimes$LONGITUDE,longitude)
beat.crimes$angle.from.officer <- (180/pi)*(atan(beat.crimes$y.miles/beat.crimes$x.miles))
beat.crimes$Date <- as.character(beat.crimes$DATE..OF.OCCURRENCE)
beat.crimes$time <- substr(beat.crimes$DATE..OF.OCCURRENCE,12,16)
beat.crimes$time.bucket <- 2*as.numeric(substr(beat.crimes$time,1,2))+as.numeric(substr(beat.crimes$time,4,5))%/%30+1
beat.crimes$hour <- as.numeric(substr(beat.crimes$time,1,2)) 
beat.crimes$hour <- ifelse(substr(beat.crimes$Date,21,22)=="PM",beat.crimes$hour+12,beat.crimes$hour)
beat.crimes$hour <- ifelse(beat.crimes$hour %in% c(12,24),beat.crimes$hour-12,beat.crimes$hour)
beat.crimes$hour.text <- paste(beat.crimes$hour,":00",sep='')
beat.crimes$date <- mdy(substr(beat.crimes$DATE..OF.OCCURRENCE,1,10))
beat.crimes$day <- weekdays(mdy(substr(beat.crimes$DATE..OF.OCCURRENCE,1,10)))
beat.crimes$day <- factor(beat.crimes$day,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
beat.crimes$month <- month(mdy(substr(beat.crimes$DATE..OF.OCCURRENCE,1,10)))
beat.crimes$date <- as.Date(beat.crimes$date)
beat.crimes$DATE..OF.OCCURRENCE <- as.character(beat.crimes$DATE..OF.OCCURRENCE)
beat.crimes$date.diff <- as.numeric(ymd(Sys.Date())-mdy(substr(beat.crimes$DATE..OF.OCCURRENCE,1,10)))
beat.crimes$recency.weight <- exp(-beat.crimes$date.diff/365)
beat.crimes$ampm.flag <- ifelse(substr(beat.crimes$DATE..OF.OCCURRENCE,21,22)=="AM",0,1)
beat.crimes$seconds <- 3600*as.numeric(substr(beat.crimes$time,1,2)) + 60*as.numeric(substr(beat.crimes$time,4,5))+43200*beat.crimes$ampm.flag
beat.crimes$daynum <- ifelse(beat.crimes$day=="Sunday",1,ifelse(beat.crimes$day=="Monday",2,
                                                          ifelse(beat.crimes$day=="Tuesday",3,
                                                          ifelse(beat.crimes$day=="Wednesday",4,
                                                          ifelse(beat.crimes$day=="Thursday",5,
                                                          ifelse(beat.crimes$day=="Friday",6,7))))))

#library(shiny)

shinyServer(
  function(input,output){
    
    beatnum <- reactive({
      as.numeric(input$beat)
    })
    
    num.clusters <- reactive({
      as.numeric(input$num.clusters)
    })
    
    
    output$ggplot1 <- renderPlot({
      crimes <- beat.crimes
      #crimes <- crimes[complete.cases(crimes),]
      if (!is.na(beatnum())) crimes <- crimes[which(crimes$BEAT==beatnum()),]
      if(! "All Week" %in% input$day) crimes <- crimes[which(crimes$day %in% input$day),]
      if(! "All Day" %in% input$hour) crimes <- crimes[which(crimes$hour.text %in% input$hour),]
      if(!"ALL CRIMES" %in% input$crimeType) crimes <- crimes[which(crimes$PRIMARY.DESCRIPTION %in% input$crimeType),]
      if(input$arrest=="yes") {crimes <- crimes[which(crimes$ARREST=="Y"),]
      }else if (input$arrest != "all cases"){crimes <- crimes[which(crimes$ARREST=="N"),]}
      crimes <- subset(crimes,month>=input$range[1] & month<=input$range[2])
      longitude <- (max(crimes$LONGITUDE,na.rm=T)+min(crimes$LONGITUDE,na.rm=T))/2
      latitude <- (max(crimes$LATITUDE,na.rm=T)+min(crimes$LATITUDE,na.rm=T))/2
      crimes$x.miles <- long.dist.miles(longitude,crimes$LONGITUDE,latitude)
      crimes$x.miles <- round(crimes$x.miles*100)/100
      crimes$y.miles <- lat.dist.miles(latitude,crimes$LATITUDE)
      crimes$y.miles <- round(crimes$y.miles*100)/100
      
      if(input$axes=="lng-lat"){
        scatterplot <- ggplot(crimes,aes(x=LONGITUDE,y=LATITUDE, color=PRIMARY.DESCRIPTION))+
          xlab("longitude")+
          ylab("latitude")
        if(input$beat != ''){
        title <- paste("Selected crimes by geocode for beat number ",input$beat,sep='')
        }else{title <- "Selected crimes by geocode for entire city "}
      }else {scatterplot <- ggplot(crimes,aes(x=x.miles,y=y.miles, color=PRIMARY.DESCRIPTION))+
               xlab("miles from center of beat")+
               ylab("miles from center of beat")
             if(input$beat != ''){
             title <- paste("Selected crimes by mileage for beat number ",input$beat,sep='')
             }else{title <-'Selected crimes by mileage for entire city'}
             }
      
      if(input$recency=="crime recency") {scatterplot <- scatterplot + geom_point(aes(size=recency.weight/10))
                                          title <- paste(title,"\n dots are sized according to crime recency",sep='')}
      scatterplot <- scatterplot+geom_point(shape=16) +  
        scale_size_continuous(guide="none")+
        ggtitle(title)
      #xlab("longitude")+
      #ylab("latitude")
      
      scatterplot
      
      
      #scatterplot <- ggplot(crimes,aes(x=LONGITUDE,y=LATITUDE, color=PRIMARY.DESCRIPTION))+
      #geom_point(shape=16) + #geom_point(aes(size=recency.weight/10)) + 
      #scale_size_continuous(guide="none")+
      #ggtitle("Selected crimes by geocode")+
      #xlab("longitude")+
      #ylab("latitude")
      
      
      #if(input$recency=="crime total"){
      #scatterplot
      #ggplot(crimes,aes(x=LONGITUDE,y=LATITUDE, color=PRIMARY.DESCRIPTION))+
      #geom_point(shape=16) + #geom_point(aes(size=recency.weight/10)) + 
      #scale_size_continuous(guide="none")+
      #ggtitle("Selected crimes by geocode")+
      #xlab("longitude")+
      #ylab("latitude") 
      #}else{
      #scatterplot + geom_point(aes(size=recency.weight/10))
      #ggplot(crimes,aes(x=x.miles,y=y.miles, color=PRIMARY.DESCRIPTION))+
      #geom_point(shape=16) + geom_point(aes(size=recency.weight/10)) + 
      #scale_size_continuous(guide="none")+
      #ggtitle("Selected crimes by mileage: Dots are sized according to crime recency")+
      #xlab("miles from center of beat")+
      #ylab("miles from center of beat")
      #}
      
    })
    
    output$ggplot2 <- renderPlot({
      crimes <- beat.crimes
      crimes <- subset(crimes,month>=input$range[1] & month<=input$range[2])
      if (!is.na(beatnum())) {
        crimes <- crimes[which(crimes$BEAT==input$beat),]
        crimes$dot.size <- crimes$recency.weight/10
      }else crimes$dot.size <- .000001
      if(! "All Week" %in% input$day) crimes <- crimes[which(crimes$day %in% input$day),]
      if(! "All Day" %in% input$hour) crimes <- crimes[which(crimes$hour.text %in% input$hour),]
      if(!"ALL CRIMES" %in% input$crimeType) crimes <- crimes[which(crimes$PRIMARY.DESCRIPTION %in% input$crimeType),]
      if(input$arrest=="yes") {crimes <- crimes[which(crimes$ARREST=="Y"),]
      }else if (input$arrest != "all cases"){crimes <- crimes[which(crimes$ARREST=="N"),]}
      crimes <- subset(crimes,month>=input$range[1] & month<=input$range[2])
      longitude <- (max(crimes$LONGITUDE,na.rm=T)+min(crimes$LONGITUDE,na.rm=T))/2
      latitude <- (max(crimes$LATITUDE,na.rm=T)+min(crimes$LATITUDE,na.rm=T))/2
      crimes$x.miles <- long.dist.miles(longitude,crimes$LONGITUDE,latitude)
      crimes$x.miles <- round(crimes$x.miles*100)/100
      crimes$y.miles <- lat.dist.miles(latitude,crimes$LATITUDE)
      crimes$y.miles <- round(crimes$y.miles*100)/100
      crimes$distance.from.officer <- total.dist.miles(crimes$LATITUDE,latitude,crimes$LONGITUDE,longitude)
      crimes$angle.from.officer <- (180/pi)*(atan(crimes$y.miles/crimes$x.miles))    
      ggplot(crimes,aes(x=x.miles,y=y.miles, color=PRIMARY.DESCRIPTION))+
        geom_point(shape=16) + geom_point(aes(size=recency.weight/10)) + 
        scale_size_continuous(guide="none")+
        ggtitle("Selected crimes by mileage: Dots are sized according to crime recency")+
        xlab("miles from center of beat")+
        ylab("miles from center of beat")
      
    })
    
    output$ggplot3 <- renderPlot({
      crimes <- beat.crimes
      if(input$arrest=="yes") {crimes <- crimes[which(crimes$ARREST=="Y"),]
      }else if (input$arrest != "all cases"){crimes <- crimes[which(crimes$ARREST=="N"),]}
      if(! "All Week" %in% input$day) count <- crimes[which(crimes$day %in% input$day),]
      if(!"ALL CRIMES" %in% input$crimeType) crimes <- crimes[which(crimes$PRIMARY.DESCRIPTION %in% input$crimeType),]
      if(! "All Day" %in% input$hour) crimes <- crimes[which(crimes$hour.text %in% input$hour),]
      crimes <- subset(crimes,month>=input$range[1] & month<=input$range[2])
      if (!is.na(beatnum())) crimes <- crimes[which(crimes$BEAT==input$beat),]
      
      if(input$recency=='crime total'){
        count <- as.data.frame(table(crimes$PRIMARY.DESCRIPTION,crimes$day))
        if(input$levels=="crimes-day"){colnames(count) <- c("PRIMARY.DESCRIPTION","day","frequency")
                                       gg <- ggplot(data=count, aes(x=day,y=frequency)) + 
                                         facet_grid(. ~ PRIMARY.DESCRIPTION)+
                                         geom_bar(aes(fill=day),stat="identity") +  xlab("day")+ylab("crime count")
                                       if(input$beat != '')
                                       {title <- paste("Crime counts by crime type and day within beat ",input$beat,sep='')
                                       }else {title <- "Crime counts by crime type and day within entire city "}
        }else {colnames(count) <- c("PRIMARY.DESCRIPTION","day","frequency")
               gg <- ggplot(data=count, aes(x=PRIMARY.DESCRIPTION,y=frequency))+
                 geom_bar(aes(fill=PRIMARY.DESCRIPTION),stat="identity")+
                 facet_grid(. ~ day) + xlab("crime")+ylab("crime count")
               if(input$beat != '')
               {title <-paste("Crime counts by day and crime type within beat ",input$beat,sep='')
               }else {title <- 'Crime counts by day and crime type within entire city'}}
        #ylab("Number of Crimes")
      }else {count <- ddply(crimes[which(!is.na(crimes$recency.weight)),],.(PRIMARY.DESCRIPTION,day),
                            summarise,Total=sum(recency.weight))
             if(input$levels=="crimes-day"){colnames(count) <- c("PRIMARY.DESCRIPTION","day","recency")
                                            gg <- ggplot(data=count, aes(x=day,y=recency)) + 
                                              facet_grid(. ~ PRIMARY.DESCRIPTION)+
                                              geom_bar(aes(fill=day),stat="identity") + xlab('day') + ylab('crime count weighted by recency')
                                            if(input$beat != '')
                                            {title <- paste("Crime recency by crime type and day within beat ",input$beat,sep='')
                                            }else{title <- "Crime recency by crime type and day within entire city "}
                                            
             }else {colnames(count) <- c("PRIMARY.DESCRIPTION","day","recency")
                    gg <- ggplot(data=count, aes(x=PRIMARY.DESCRIPTION,y=recency))+
                      geom_bar(aes(fill=PRIMARY.DESCRIPTION),stat="identity")+
                      facet_grid(. ~ day) + xlab('crimes') + ylab('crime count weighted by recency')
                    if(input$beat != '')
                    {title <- paste("Crime recency by day and crime type within beat ",input$beat,sep='')
                    }else {title <- "Crime recency by day and crime type within entire city"}
                    }
             
      }
      
      gg <- gg+ 
        theme(axis.text.x=element_text(angle=90))+
        ggtitle(title) +
        xlab("day")+ 
        theme(legend.position="none")
      gg
      
      
      #ggplot(data=count, aes(x=day,y=Freq))+
      #geom_bar(aes(fill=day),stat="identity")+
      #facet_grid(. ~ PRIMARY.DESCRIPTION)+ 
      #theme(axis.text.x=element_text(angle=90))+
      #ggtitle("Crime Counts by Crime Type and Day Within Beat")+
      #xlab("day")+
      #ylab("Number of Crimes") + 
      #theme(legend.position="none")
    })
    
    output$ggplot4 <- renderPlot({
      crimes <- beat.crimes
      if(input$arrest=="yes") {crimes <- crimes[which(crimes$ARREST=="Y"),]
      }else if (input$arrest != "all cases"){crimes <- crimes[which(crimes$ARREST=="N"),]}
      if(! "All Day" %in% input$hour) crimes <- crimes[which(crimes$hour.text %in% input$hour),]
      crimes <- subset(crimes,month>=input$range[1] & month<=input$range[2])
      if (!is.na(beatnum())) crimes <- crimes[which(crimes$BEAT==input$beat),]
      crimes <- ddply(crimes[which(!is.na(crimes$recency.weight)),],.(PRIMARY.DESCRIPTION,day),
                      summarise,Total=sum(recency.weight))
      colnames(crimes) <- c("PRIMARY.DESCRIPTION","day","recency.count")
      if(! "All Week" %in% input$day) crimes<- crimes[which(crimes$day %in% input$day),]
      if(!"ALL CRIMES" %in% input$crimeType) crimes <- crimes[which(crimes$PRIMARY.DESCRIPTION %in% input$crimeType),]
      ggplot(data=crimes, aes(x=day,y=recency.count))+
        geom_bar(aes(fill=day),stat="identity")+
        facet_grid(. ~ PRIMARY.DESCRIPTION)+ 
        theme(axis.text.x=element_text(angle=90))+
        ggtitle("Crime Recency by Crime Type and Day Within Beat")+
        xlab("day")+
        ylab("Recency Weighted Count of Crimes") + 
        theme(legend.position="none")
    })
    
    output$ggplot5 <- renderPlot({
      crimes <- beat.crimes
      if(input$arrest=="yes") {crimes <- crimes[which(crimes$ARREST=="Y"),]
      }else if (input$arrest != "all cases"){crimes <- crimes[which(crimes$ARREST=="N"),]}
      if(! "All Day" %in% input$hour) crimes <- crimes[which(crimes$hour.text %in% input$hour),]
      crimes <- subset(crimes,month>=input$range[1] & month<=input$range[2])
      if (!is.na(beatnum())) crimes <- crimes[which(crimes$BEAT==input$beat),]
      count.by.day <- ddply(crimes[which(!is.na(crimes$recency.weight)),],.(PRIMARY.DESCRIPTION,day),
                            summarise,Total=sum(recency.weight))
      colnames(count.by.day) <- c("PRIMARY.DESCRIPTION","day","recency.count")
      if(! "All Week" %in% input$day) count.by.day<- count.by.day[which(count.by.day$day %in% input$day),]
      if(!"ALL CRIMES" %in% input$crimeType) count.by.day <- count.by.day[which(count.by.day$PRIMARY.DESCRIPTION %in% input$crimeType),]
      ggplot(data=count.by.day, aes(x=PRIMARY.DESCRIPTION,y=recency.count))+
        geom_bar(aes(fill=PRIMARY.DESCRIPTION),stat="identity")+
        facet_grid(. ~ day)+ 
        theme(axis.text.x=element_text(angle=90))+
        ggtitle("Crime Recency by Day and Crime Type Within Beat")+
        xlab("Crime Type")+
        ylab("Recency Weighted Count of Crimes") + 
        theme(legend.position="none")
    })
    
    output$ggplot6 <- renderPlot({
      crimes <- beat.crimes
      print(names(crimes))
      if (!is.na(beatnum())) crimes <- crimes[which(crimes$BEAT==beatnum()),]
      if(! "All Week" %in% input$day) crimes <- crimes[which(crimes$day %in% input$day),]
      if(! "All Day" %in% input$hour) crimes <- crimes[which(crimes$hour.text %in% input$hour),]
      if(!"ALL CRIMES" %in% input$crimeType) crimes <- crimes[which(crimes$PRIMARY.DESCRIPTION %in% input$crimeType),]
      if(input$arrest=="yes") {crimes <- crimes[which(crimes$ARREST=="Y"),]
      }else if (input$arrest != "all cases"){crimes <- crimes[which(crimes$ARREST=="N"),]}
      crimes <- subset(crimes,month>=input$range[1] & month<=input$range[2])
      crimes <- na.omit(crimes)
      temp <- crimes$PRIMARY.DESCRIPTION
      #if(input$clusters=="spatial"){
        crimes <- crimes[,c(7,6)]
        kc <- kmeans(crimes,num.clusters())
        crimes$cluster <- kc$cluster
        crimes$PRIMARY.DESCRIPTION <- temp
        crimes$cluster <- as.factor(crimes$cluster)
        crimes$PRIMARY.DESCRIPTION <- as.factor(crimes$PRIMARY.DESCRIPTION)
        gg <- ggplot(crimes,aes(x=LONGITUDE,y=LATITUDE,color=cluster,shape=PRIMARY.DESCRIPTION))+
          ggtitle("Crime Clusters by Location")+
          xlab("longitude")+
          ylab("latitude")
      #}else{crimes <- crimes[,c(24,15)]
            #kc <- kmeans(crimes,num.clusters())
            #crimes$cluster <- kc$cluster
            #crimes$PRIMARY.DESCRIPTION <- temp
            #crimes$cluster <- as.factor(crimes$cluster)
            #crimes$PRIMARY.DESCRIPTION <- as.factor(crimes$PRIMARY.DESCRIPTION)
            #gg <- ggplot(crimes,aes(x=daynum,y=hour,color=cluster,shape=PRIMARY.DESCRIPTION))+
              #geom_point(size=2) + 
              #ggtitle("Crime Clusters by Time")+
              #xlab("day of week (Sunday=1)")+
              #ylab("hour of day") 
            
      #}
      #kc <- kmeans(crimes,num.clusters())
      #crimes$cluster <- kc$cluster
      #crimes$PRIMARY.DESCRIPTION <- temp
      #crimes$cluster <- as.factor(crimes$cluster)
      #crimes$PRIMARY.DESCRIPTION <- as.factor(crimes$PRIMARY.DESCRIPTION)
      #ggplot(crimes,aes(x=LONGITUDE,y=LATITUDE,color=cluster,shape=PRIMARY.DESCRIPTION))+
      #geom_point(size=2) + 
      #scale_size_continuous(guide="none")+
      #ggtitle("Crime Clusters by Location")+
      #xlab("longitude")+
      #ylab("latitude")
      gg <- gg+geom_point(size=2) + 
        scale_size_continuous(guide="none")
      gg
      
    })
    
    output$ggplot7 <- renderPlot({
      crimes <- beat.crimes
      if (!is.na(beatnum())) crimes <- crimes[which(crimes$BEAT==beatnum()),]
      if(!"ALL CRIMES" %in% input$crimeType) crimes <- crimes[which(crimes$PRIMARY.DESCRIPTION %in% input$crimeType),]
      if(input$arrest=="yes") {crimes <- crimes[which(crimes$ARREST=="Y"),]
      }else if (input$arrest != "all cases"){crimes <- crimes[which(crimes$ARREST=="N"),]}
      crimes <- subset(crimes,month>=input$range[1] & month<=input$range[2])
      crimes <- na.omit(crimes)
      temp <- crimes$PRIMARY.DESCRIPTION
      crimes <- crimes[,c(24,15)]
      kc <- kmeans(crimes,num.clusters())
      crimes$cluster <- kc$cluster
      crimes$PRIMARY.DESCRIPTION <- temp
      crimes$cluster <- as.factor(crimes$cluster)
      crimes$PRIMARY.DESCRIPTION <- as.factor(crimes$PRIMARY.DESCRIPTION)
      ggplot(crimes,aes(x=daynum,y=hour,color=cluster,shape=PRIMARY.DESCRIPTION))+
        geom_point(size=2) + 
        scale_size_continuous(guide="none")+
        ggtitle("Crime Clusters by Time")+
        xlab("day of week (Sunday=1)")+
        ylab("hour of day")     
    })
    
    output$ggplot8 <- renderPlot({
      crimes <- beat.crimes
      if (!is.na(beatnum())) crimes <- crimes[which(crimes$BEAT==beatnum()),]
      if(!"ALL CRIMES" %in% input$crimeType) crimes <- crimes[which(crimes$PRIMARY.DESCRIPTION %in% input$crimeType),]
      if(input$arrest=="yes") {crimes <- crimes[which(crimes$ARREST=="Y"),]
      }else if (input$arrest != "all cases"){crimes <- crimes[which(crimes$ARREST=="N"),]}
      crimes <- subset(crimes,month>=input$range[1] & month<=input$range[2])
      
      #a <-count(crimes,'hour')
      #b <- merge(crimes,a,by='hour')
      m <- count(crimes,vars=c('day','hour','PRIMARY.DESCRIPTION'))
      #print(table(m$day,m$hour))
      #ggplot(b,aes(x=day,y=hour, color=PRIMARY.DESCRIPTION))+
      #geom_point(shape=16) + geom_point(aes(size=freq/1000)) + 
      #scale_size_continuous(guide="none")+
      #theme(legend.position="none") +
      #ggtitle("Selected crimes by frequency, time and day")+
      #xlab("")+
      #ylab("hour of day")
      
      ggplot(m,aes(x=day,y=hour, color=PRIMARY.DESCRIPTION))+
        geom_point(shape=16) + geom_point(aes(size=freq/1000)) + 
        scale_size_continuous(guide="none")+
        #theme(legend.position="none") +
        ggtitle(paste("Selected crimes by frequency, time and day for beat ",input$beat,sep=''))+
        xlab("")+
        ylab("hour of day")
      
      
      
      #ggplot(crimes,aes(x=day,y=hour, color=PRIMARY.DESCRIPTION))+
      #geom_point(shape=16) + #geom_point(aes(size=recency.weight/1000)) + 
      #scale_size_continuous(guide="none")+
      #ggtitle("Selected crimes by time and day")+
      #xlab("")+
      #ylab("hour of day")           
    })
    
    output$freq.data <- renderTable({
      #current.hour <- hour(Sys.time())
      #current.hour <- as.numeric(substr(as.POSIXlt(Sys.time(),"America/Chicago"),12,13))
      current.hour <- hour(as.POSIXlt(Sys.time(),"America/Chicago"))
      #print(current.hour)
      #if(input$hour != "All Day"){
      #if(nchar(input$hour)==4) {current.hour <- as.numeric(substr(input$hour,1,1))
      #                          }else{current.hour <- as.numeric(substr(input$hour,1,2))}
      #}
      #print(current.hour)}
      #if(input$day=="Sunday"){nextday="Monday"
      #}else if(input$day=="Monday"){nextday="Tuesday"
      #}else if(input$day=="Tuesday"){nextday="Wednesday"
      #}else if(input$day=="Wednesday"){nextday="Thursday"
      #}else if(input$day=="Thursday"){nextday="Friday"
      #}else if(input$day=="Friday"){nextday="Saturday"
      #}else {nextday="Sunday"}
      crimes <- beat.crimes
      if(input$arrest=="yes") {crimes <- crimes[which(crimes$ARREST=="Y"),]
      }else if (input$arrest != "all cases"){crimes <- crimes[which(crimes$ARREST=="N"),]}
      crimes <- subset(crimes,month>=input$range[1] & month<=input$range[2])
      #current.hour <- hour(Sys.time())
      if(current.hour<=20){hour.bucket <- seq(current.hour,(current.hour+3))
                           crimes <- crimes[which(crimes$day==weekday & crimes$hour %in% hour.bucket),]
      }else if (current.hour == 21) {hour.bucket <- seq(21,23)
                                     crimes <- crimes[which((crimes$day==weekday & crimes$hour %in% hour.bucket) |
                                                              (crimes$day==nextday & crimes$hour==0)) ,]                            
      }else if (current.hour==22) {hour.bucket <- seq(22,23)
                                   crimes <- crimes[which((crimes$day==weekday & crimes$hour %in% hour.bucket) |
                                                            (crimes$day==nextday & crimes$hour %in% c(0,1))) ,]                         
      }else {hour.bucket=23
             crimes <- crimes[which((crimes$day==weekday & crimes$hour %in% hour.bucket) |
                                      (crimes$day==nextday & crimes$hour %in% c(0,1,2))) ,]
      }
      print(hour.bucket)
      if (!is.na(beatnum())) crimes <- crimes[which(crimes$BEAT==input$beat),]
      #if(! "All Week" %in% input$day) crimes <- crimes[which(crimes$day %in% input$day),]
      #if(! "All Day" %in% input$hour) crimes <- crimes[which(crimes$hour.text %in% input$hour),]
      count <- as.data.frame(table(crimes$PRIMARY.DESCRIPTION))
      colnames(count) <- c("Crime","Freq")
      count <- count[order(-count$Freq),]
      #print(head(count))
      #head(count,10)
      crime.count <- data.frame(cbind(as.character(count$Crime),count$Freq))
      colnames(crime.count) <- c("Crime","Freq")
      crime.count$Freq <- as.integer(as.character(crime.count$Freq))
      print(str(crime.count))
      crime.count <- head(crime.count,11)
      crime.count$Crime <- as.character(crime.count$Crime)
      crime.count[11,1] <- paste('beat number ',input$beat,sep='')
      crime.count[11,2] <- ''
      crime.count
    })
    
    output$instructions <- renderUI({
      #str1 <- "Please click on any of the above tabs and allow about 90 seconds for the initial"
      #str2 <- "data download to visualize your data.  Once the data has been downloaded once,"
      #str3 <- "user can change any input value and/or select a new output tab and see the changes"
      #str4 <- "almost instantaneously."
      #str5 <- ""
      #str6 <- ""
      str7 <- h5("Sidebar Layout:")
      str8 <- "Beat Number: enter beat number of interest, or leave blank to view crimes across entire city."
      str9 <- "Days- leave 'All Week', or select the day(s) of your choice."
      str10 <- "Hours- leave 'All Day', or select the hours(s) of your choice."
      str11 <- "Months- slide the switch to view month range of interest."
      str12 <- "Crimes- By default, five crimes are selected. Click the crimes of interest."
      str13 <- "Arrests made- Default is all cases, but user can click 'yes' or 'no to view only a subset of all cases."
      str14 <- "Recency radio button: Allows view of either crime counts or crime recency counts, which gives more weight
                to recent crimes than to older crimes."
      str15 <- "Axes radio button: Allows the choice between viewing the spatial scatter plots with axes in either geo 
                coordinates or in mileage from beat center."
      str16 <- "Levels: For the bar charts, the user has the option of viewing crimes by day, or days by crime."
      str17 <- "Cluster Type: Choose between seeing crime clusters by area or by time."
      str18 <- "Number of clusters: Enter the number of clusters to view."
      str19 <- h5("Main Panel:")
      str20 <- "Spatial Scatterplot: Plots all selected crimes within territory and timeframe, with dots sized either 
              according to recency or by crime count; and axes either geocoded or mileage."
      str21 <- "Crime count- Bar chart of weekdays per selected crimes. Can be viewed as either crime counts or crime recency
              counts, and with days/crimes bars or crimes/days bars."
      str22 <- "Cluster analysis.  Select up to six crime types and enter the desired number of clusters.  Can view
              either crime spatial clusters or crime time clusters."
      str23 <- "Table of top crimes in chosen area over next four hour time bucket as of the current hour."
      str24 <- ""
      str25 <- "Feel free to provide feedback to malter61@gmail.com"
      str26 <- 'Mark'
      
      HTML(paste(str7,str8,str9,str10,str11,str12,str13,str14,
                 str15,str16,str17,str18,str19,str20,str21,str22,str23,str24,str25,str26,sep='<br/>'))
    })
    
    #output$instructions2 <- renderUI({
    # str8 <- h5("Main Panel:")
    # str9 <- "Geocode Scatterplot -plots all selected crimes within territory and timefrime, with dots sized according to recency."
    # str10 <- "Distance Scatterplot -same as above, except that points are scaled as mileage from center of location."
    #str11 <- "Crime count- Bar chart of wekdays per selected crimes."
    #str12 <- "Same as above, except crimes are scaled for recency."
    # str13 <- "Same as above, except this view shows selected crimes per weekday."
    # str14 <- "Chart of top crimes in chosen area over next four hour time bucket as of the current hour."
    # HTML(paste(str1,str2,str3,str4,str5,str6,str7,sep='<br/>'))
    #})
    
  })

