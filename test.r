#getwd()
#head(x,2)
#install.packages("DT")
library('DT')
#install.packages("forecast")
#install.packages("forecast",
#                 repos = c("http://rstudio.org/_packages",
# 
#"http://cran.rstudio.com"))
remove.packages("ggplot2")
install.packages("ggplot2",repos = "http://cran.rstudio.com" )
library("forecast")
install.packages("gmodels")
#remove.packages("forecast")
library("gmodels")
#install.packages("dplyr")
library("dplyr")
#remove.packages("dplyr")
#install.packages("dplyr")
#install.packages("DT")
install.packages('rpart.plot')
library('rpart.plot')
#install.packages("ff")
library("ff")
install.packages("plotly")
library("plotly")
install.packages("maps")
library("maps")
remove.packages("plotly")
install.packages("ggplot2")
library("ggplot2")
remove.packages("ggplot2")

#install.packages("MASS")
library("MASS")
#install.packages("tidyverse")
#library("tidyverse")
#install.packages("TTR")
library("TTR")
install.packages("scales")
#remove.packages("scales")
library("scales")
#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("shinydashboard")
#install.packages("shiny")
#library(shiny)
#library(shinydashboard)
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("SnowballC")
#install.packages("rpart")
install.packages("lubridate")
library("tm")
library("wordcloud")
library("RColorBrewer")
library("SnowballC")
library("lubridate")
#library("rpart")
#setwd("F:/Coursera/course_Stats_with_R/shiny tutorials")
options(fftempdir = "C:/Users/shyam/Documents")
x<- read.csv.ffdf(file="Traffic_Violations_Test.csv", header=TRUE, VERBOSE=TRUE, first.rows=10000, next.rows=50000, colClasses=NA)
x <- as.data.frame(x)
x<-x %>% rowwise() %>% mutate(Year.Of.Stop = substring(Date.Of.Stop,7,10))
x <- x %>% filter(Year.Of.Stop==2018)
x<-x %>% rowwise() %>% mutate(Month.Of.Stop = substring(Date.Of.Stop,1,2))
#unique(x$Month.Of.Stop)

#head(x$Month.Of.Stop,20)
#head(x$Belts,10)
#class(x$Month.Of.Stop)
#unique(x$Year.Of.Stop)
x.Jan<-x%>%filter(Month.Of.Stop=="01")
x.Feb<-x%>%filter(Month.Of.Stop=="02")
x.Mar<-x%>%filter(Month.Of.Stop=="03")
x.Apr<-x%>%filter(Month.Of.Stop=="04")
x.May<-x%>%filter(Month.Of.Stop=="05")
x.Jun<-x%>%filter(Month.Of.Stop=="06")
x.July<-x%>%filter(Month.Of.Stop=="07")
x.Aug<-x%>%filter(Month.Of.Stop=="08")
x.Sep<-x%>%filter(Month.Of.Stop=="09")
x.Oct<-x%>%filter(Month.Of.Stop=="10")
x.Nov<-x%>%filter(Month.Of.Stop=="11")
x.Dec<-x%>%filter(Month.Of.Stop=="12")

m = map_data('state', region = 'Maryland')
plotForLocation<-x%>%filter((Longitude!="NA")&(Latitude!="NA"))
plotForLocation<-head((x%>%select(Location,Latitude,Longitude)%>%group_by(Location,Latitude,Longitude)%>%summarise(Cases=n())%>%arrange(desc(Cases))),200)
plotForLocation1<-plotForLocation%>%mutate(mytext=paste("Location: ", Location, "\n", "Cases: ", Cases, sep=""))
p1=ggplot() +
  geom_polygon(data = m, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data=plotForLocation1,aes(x=Longitude, y=Latitude, size=Cases, color=Cases, text=mytext, alpha=Cases) ) +
  labs(x = "Longitude", y= "Latitude") +
  scale_color_gradient(low="#FA8072",high="#DC143C", space ="Lab" )
p1=ggplotly(p1, tooltip="text")
p1
#write.table(x$Description,"Case_Description.txt",sep="\t",row.names=FALSE)
Violations = "C:\\Users\\shyam\\Documents\\Case_Description.txt"
Cases = readLines(Violations)
docs<-Corpus(VectorSource(Cases))
#inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("posted", "use", "person", "hwy", "upon")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
#head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          scale = c(3,0.5),
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#plot for driver demographics (male and female driver)
carAndDriver<-x%>%select(VehicleType,Year,Make,Model,Color,Race,Gender,Driver.State)%>%group_by(Year,Make,Model,Color,Race,Gender,Driver.State)%>%summarise(Cases=n())
driverGender1<-carAndDriver%>%dplyr::select(Gender)%>%dplyr::group_by(Gender)%>%dplyr::summarise(Cases=n())
driverGender1<-driverGender1 %>%dplyr::mutate(percentCases=paste0(round(100*Cases/sum(Cases),0),'%'))
#data to be used for this plot
driverGender1<-driverGender1%>%dplyr::filter(Gender!="U")

driverGender1$Gender <- as.character(driverGender1$Gender)
driverGender1$Gender[driverGender1$Gender == 'M'] <- 'Male'
driverGender1$Gender[driverGender1$Gender == 'F'] <- 'Female'
# optional
driverGender1$Gender <- as.factor(driverGender1$Gender)

colors <- c("#FF0000", "#808080")
p3<-plot_ly(driverGender1, labels = ~Gender, values = ~Cases, type = 'pie',
            textposition = 'inside',
            textinfo = 'percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste('Traffic Rule Violation Cases:', Cases),
            marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1))) %>%
  layout(title = 'Driver Demographics',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p3

#plot for driver demographics (inside and outside Maryland)

driverState<-carAndDriver%>%dplyr::select(Driver.State)%>%dplyr::group_by(Driver.State)%>%dplyr::summarise(Cases=n())
driverState$Driver.State <- as.character(driverState$Driver.State)
driverState$Driver.State[driverState$Driver.State == 'MD'] <- 'InState'
driverState$Driver.State[driverState$Driver.State != 'InState'] <- 'Out of State'
# optional
driverState$Driver.State <- as.factor(driverState$Driver.State)

driverState<-driverState%>%dplyr::group_by(Driver.State)%>%dplyr::summarise(TotalCases=sum(Cases))
driverState<-driverState %>%dplyr::mutate(percentCases=paste0(round(100*TotalCases/sum(TotalCases),0),'%'))
#labels = ~Driver.State
colors <- c("#FF0000", "#808080")
p4<-plot_ly(driverState, labels = ~Driver.State,values = ~TotalCases, type = 'pie',
            textposition = 'inside',
            textinfo = 'percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste('Traffic Rule Violation Cases:', TotalCases),
            marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1))) %>%
  layout(title = 'Driver Demographics',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p4

#plot for traffic violations across time
#4
monthViolations <- x%>%dplyr::select(Month.Of.Stop)%>%dplyr::group_by(Month.Of.Stop)%>%dplyr::summarise(TotalViolations=n())
#p3<- plot_ly(x = ~monthViolations$Month.Of.Stop, y = ~monthViolations$TotalViolations, mode = 'lines', text = paste("Total Traffic Violations:",monthViolations$TotalViolations))
#p3

#plot for car types violations
#3
carTypes<-x%>%dplyr::select(VehicleType)%>%dplyr::mutate(realVehicleType=substring(VehicleType,5,50))
carTypes<-carTypes%>%dplyr::group_by(realVehicleType)%>%dplyr::summarise(TotalCasesBooked=n())%>%dplyr::arrange(desc(TotalCasesBooked))
carTypeTop<-head(carTypes,2)
other2= (sum(carTypes$TotalCasesBooked)-sum(carTypeTop$TotalCasesBooked))
carTypes<-carTypes%>%dplyr::filter(realVehicleType!="Other")
carTypes<-carTypes%>%dplyr::filter(realVehicleType!="Unknown")
newRow2<-data.frame("Other Car Types",other2)
names(newRow2)<-c("realVehicleType","TotalCasesBooked")
carTypeTop <- rbind(carTypeTop, newRow2)
p6 <- ggplot(carTypeTop, aes(x=realVehicleType,y=(TotalCasesBooked/sum(TotalCasesBooked)),fill=realVehicleType))+scale_y_continuous(labels=scales::percent)
p6<-p6+geom_bar(position="dodge",stat="identity",width=0.5) + ylab("Traffic Violations")
# Number of cars in each class:
p6<-p6 + xlab("Car Type")
p6<-p6+geom_text(aes(label=paste0(round(100*TotalCasesBooked/sum(TotalCasesBooked),0),'%')), position=position_dodge(width=0.9), vjust=-0.25) + coord_cartesian(ylim=c(0,1))
p6<-p6+theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),
             legend.position = "right")
p6

#p4<-plot_ly(x = carTypes$realVehicleType,y = carTypes$TotalCasesBooked,type = "bar")
#p4
#2
arrestType<-x%>%dplyr::select(Arrest.Type)%>%dplyr::group_by(Arrest.Type)%>%dplyr::summarise(TotalCasesBooked=n())%>%dplyr::arrange(desc(TotalCasesBooked))
#p5<-plot_ly(x = arrestType$Arrest.Type,y = arrestType$TotalCasesBooked,type = "bar")
#p5
#arrestType["A-Marked Patrol",]
arrestTypeTop<-head(arrestType,3)
other= (sum(arrestType$TotalCasesBooked)-sum(arrestTypeTop$TotalCasesBooked))
newRow<-data.frame("Other Arrest Types",other)
names(newRow)<-c("Arrest.Type","TotalCasesBooked")
arrestTypeTop <- rbind(arrestTypeTop, newRow)

p7 <- ggplot(arrestTypeTop, aes(x=Arrest.Type,y=(TotalCasesBooked/sum(TotalCasesBooked)),fill=Arrest.Type))+scale_y_continuous(labels=scales::percent)
p7<-p7+geom_bar(position="dodge",stat="identity",width=0.5) + ylab("Traffic Violations")
# Number of cars in each class:
p7<-p7 + xlab("Arrest Type")
p7<-p7+geom_text(aes(label=paste0(round(100*TotalCasesBooked/sum(TotalCasesBooked),0),'%')), position=position_dodge(width=0.9), vjust=-0.25) + coord_cartesian(ylim=c(0,1))
p7<-p7+theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),
             legend.position = "right")
p7



#1
violationType<-x%>%dplyr::select(Violation.Type)%>%dplyr::group_by(Violation.Type)%>%dplyr::summarise(TotalCasesBooked=n())%>%arrange(TotalCasesBooked)
violationType<-violationType%>%dplyr::mutate(percentCases=paste0(round(100*TotalCasesBooked/sum(TotalCasesBooked),0),'%'))

#violationType<-violationType%>%dplyr::arrange(Tot)
#p6<-ggplot(data=violationType,aes(x=Violation.Type))+geom_bar(aes(y=percentCases),stat="identity",fill="red")
#+geom_text(aes(ylabel=percentCases), vjust=3.1,color="white", size=3.5)
#p6<-plot_ly(x = violationType$Violation.Type,y = violationType$percentCases,type = "bar",color="red")
#violationType$Violation.Type <- with(percentCases, reorder(Violation.Type, -percentCases))
g <- ggplot(violationType, aes(x=Violation.Type,y=(TotalCasesBooked/sum(TotalCasesBooked))))+scale_y_continuous(labels=scales::percent)
g<-g+geom_bar(fill="red",position="dodge",stat="identity",width=0.5) + ylab("Traffic Violations")
# Number of cars in each class:
g<-g + xlab("Violation Type")
p8<-g+geom_text(aes(label=paste0(round(100*TotalCasesBooked/sum(TotalCasesBooked),0),'%')), position=position_dodge(width=0.9), vjust=-0.25) + coord_cartesian(ylim=c(0,1))
p8
#+ylab("Traffic Violations")
#p6

#line chart to be placed
p5 <- ggplot(data=monthViolations,aes(x = Month.Of.Stop,y=TotalViolations,group=1))+geom_point(color="red")+geom_line(color="red")
p5 <- p5+labs(x = "Month", y = "Total Traffic Violation Cases")
p5
#title = "Motor vehicle emissions in Baltimore")


x<-x%>%dplyr::mutate(Hour=hour(hms(as.character(factor(Time.Of.Stop)))))
x<-x %>% dplyr::mutate(TimeOfDay = ifelse(Hour >=0 & Hour <=6, "Nights", ifelse(Hour >=7 & Hour<= 12, "Midmornings", ifelse(Hour >=13 & Hour<= 18, "Afternoons","Evenings"))))
x.Nights<-x%>%dplyr::filter(TimeOfDay=="Nights")
x.Midmornings<-x%>%dplyr::filter(TimeOfDay=="Midmornings")
x.Afternoons<-x%>%dplyr::filter(TimeOfDay=="Afternoons")
x.Evenings<-x%>%dplyr::filter(TimeOfDay=="Evenings")

x.Nights.Metrics<-c("Alcohol Cases","Hazardous Materiels","Fatal Accidents","Personal Injuries","Property Damage","Belt Violations")
x.Nights.Values<-c(nrow(x.Nights%>%dplyr::filter(Alcohol=="Yes")),nrow(x.Nights%>%dplyr::filter(HAZMAT=="Yes")),nrow(x.Nights%>%dplyr::filter(Fatal=="Yes")),nrow(x.Nights%>%dplyr::filter(Personal.Injury=="Yes")),nrow(x.Nights%>%dplyr::filter(Property.Damage=="Yes")),nrow(x.Nights%>%dplyr::filter(Belts=="Yes")))
x.Nights.df<-data.frame(x.Nights.Metrics,x.Nights.Values)

p9 <- ggplot(x.Nights.df, aes(x=x.Nights.Metrics,y=(x.Nights.Values/sum(x.Nights.Values)),fill=x.Nights.Metrics))+scale_y_continuous(labels=scales::percent)
p9<-p9+geom_bar(position="dodge",stat="identity",width=0.5) + ylab("Traffic Violations")
# Number of cars in each class:
p9<-p9 + xlab("Offences and Accidents")
p9<-p9+geom_text(aes(label=paste0(round(100*x.Nights.Values/sum(x.Nights.Values),0),'%')), position=position_dodge(width=0.9), vjust=-0.25) + coord_cartesian(ylim=c(0,1))
p9<-p9+theme(
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank(),
  legend.position = "right",
  legend.title=element_blank(),
  legend.text=element_text(size=14),
  text = element_text(size=14),
  legend.key.size = unit(3.0, 'lines'),
  axis.text = element_text(size = 14))
p9
#axis.title.x=element_blank(),
x.Midmornings.Metrics<-c("Alcohol Cases","Hazardous Materiels","Fatal Accidents","Personal Injuries","Property Damage","Belt Violations")
x.Midmornings.Values<-c(nrow(x.Midmornings%>%dplyr::filter(Alcohol=="Yes")),nrow(x.Midmornings%>%dplyr::filter(HAZMAT=="Yes")),nrow(x.Midmornings%>%dplyr::filter(Fatal=="Yes")),nrow(x.Midmornings%>%dplyr::filter(Personal.Injury=="Yes")),nrow(x.Midmornings%>%dplyr::filter(Property.Damage=="Yes")),nrow(x.Midmornings%>%dplyr::filter(Belts=="Yes")))
x.Midmornings.df<-data.frame(x.Midmornings.Metrics,x.Midmornings.Values)

p10 <- ggplot(x.Midmornings.df, aes(x=x.Midmornings.Metrics,y=(x.Midmornings.Values/sum(x.Midmornings.Values)),fill=x.Midmornings.Metrics))+scale_y_continuous(labels=scales::percent)
p10<-p10+geom_bar(position="dodge",stat="identity",width=0.5) + ylab("Traffic Violations")
# Number of cars in each class:
p10<-p10 + xlab("Offences and Accidents")
p10<-p10+geom_text(aes(label=paste0(round(100*x.Midmornings.Values/sum(x.Midmornings.Values),0),'%')), position=position_dodge(width=0.9), vjust=-0.25) + coord_cartesian(ylim=c(0,1))
p10<-p10+theme(
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank(),
  legend.position = "right",
  legend.title=element_blank(),
  legend.text=element_text(size=14),
  text = element_text(size=14),
  legend.key.size = unit(3.0, 'lines'),
  axis.text = element_text(size = 14))
p10

x.Afternoons.Metrics<-c("Alcohol Cases","Hazardous Materiels","Fatal Accidents","Personal Injuries","Property Damage","Belt Violations")
x.Afternoons.Values<-c(nrow(x.Afternoons%>%dplyr::filter(Alcohol=="Yes")),nrow(x.Afternoons%>%dplyr::filter(HAZMAT=="Yes")),nrow(x.Afternoons%>%dplyr::filter(Fatal=="Yes")),nrow(x.Afternoons%>%dplyr::filter(Personal.Injury=="Yes")),nrow(x.Afternoons%>%dplyr::filter(Property.Damage=="Yes")),nrow(x.Afternoons%>%dplyr::filter(Belts=="Yes")))
x.Afternoons.df<-data.frame(x.Afternoons.Metrics,x.Afternoons.Values)

p11 <- ggplot(x.Afternoons.df, aes(x=x.Afternoons.Metrics,y=(x.Afternoons.Values/sum(x.Afternoons.Values)),fill=x.Afternoons.Metrics))+scale_y_continuous(labels=scales::percent)
p11<-p11+geom_bar(position="dodge",stat="identity",width=0.5) + ylab("Traffic Violations")
# Number of cars in each class:
p11<-p11 + xlab("Offences and Accidents")
p11<-p11+geom_text(aes(label=paste0(round(100*x.Afternoons.Values/sum(x.Afternoons.Values),0),'%')), position=position_dodge(width=0.9), vjust=-0.25) + coord_cartesian(ylim=c(0,1))
p11<-p11+theme(
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank(),
  legend.position = "right",
  legend.title=element_blank(),
  legend.text=element_text(size=14),
  text = element_text(size=14),
  legend.key.size = unit(3.0, 'lines'),
  axis.text = element_text(size = 14))
p11

x.Evenings.Metrics<-c("Alcohol Cases","Hazardous Materiels","Fatal Accidents","Personal Injuries","Property Damage","Belt Violations")
x.Evenings.Values<-c(nrow(x.Evenings%>%dplyr::filter(Alcohol=="Yes")),nrow(x.Evenings%>%dplyr::filter(HAZMAT=="Yes")),nrow(x.Evenings%>%dplyr::filter(Fatal=="Yes")),nrow(x.Evenings%>%dplyr::filter(Personal.Injury=="Yes")),nrow(x.Evenings%>%dplyr::filter(Property.Damage=="Yes")),nrow(x.Evenings%>%dplyr::filter(Belts=="Yes")))
x.Evenings.df<-data.frame(x.Evenings.Metrics,x.Evenings.Values)

p12 <- ggplot(x.Evenings.df, aes(x=x.Evenings.Metrics,y=(x.Evenings.Values/sum(x.Evenings.Values)),fill=x.Evenings.Metrics))+scale_y_continuous(labels=scales::percent)
p12<-p12+geom_bar(position="dodge",stat="identity",width=0.5) + ylab("Traffic Violations")
# Number of cars in each class:
p12<-p12 + xlab("Offences and Accidents")
p12<-p12+geom_text(aes(label=paste0(round(100*x.Evenings.Values/sum(x.Evenings.Values),0),'%')), position=position_dodge(width=0.9), vjust=-0.25) + coord_cartesian(ylim=c(0,1))
p12<-p12+theme(
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank(),
  legend.position = "right",
  legend.title=element_blank(),
  legend.text=element_text(size=14),
  text = element_text(size=14),
  legend.key.size = unit(3.0, 'lines'),
  axis.text = element_text(size = 14))
p12
colors1<-c("BLACK","GRAY","RED","SILVER","WHITE")
carColors2<-x%>%dplyr::select(Color,Contributed.To.Accident)%>%filter(Color %in% colors1)
carColors1<-na.omit(x%>%dplyr::select(Color,Contributed.To.Accident))
#carColors2<-x%>%dplyr::select(Color,Contributed.To.Accident)
carColors<-head(carColors1%>%group_by(Color)%>%summarise(CountOfCars=n())%>%arrange(desc(CountOfCars)),5)
p13 <- ggplot(carColors, aes(x=Color,y=(CountOfCars/sum(CountOfCars))))+scale_y_continuous(labels=scales::percent)
p13<-p13+geom_bar(fill="red",position="dodge",stat="identity",width=0.5) + ylab("Traffic Violations")
# Number of cars in each class:
p13<-p13 + xlab("Vehicle Color")
p13<-p13+geom_text(aes(label=paste0(round(100*CountOfCars/sum(CountOfCars),0),'%')), position=position_dodge(width=0.9), vjust=-0.25) + coord_cartesian(ylim=c(0,1))
p13

#class(x$Contributed.To.Accident)
chi_sq_test1<-chisq.test(table(droplevels(carColors2$Color),carColors2$Contributed.To.Accident))
#table(carColors2$Color,carColors2$Contributed.To.Accident)

#View(test_df<-table(carColors1$Color,carColors1$Contributed.To.Accident))
#View(unique(carColors1$Color))
#View(CrossTable(carColors1$Color, carColors1$Contributed.To.Accident))
models<-c("4S","ACCORD","CAMRY","CIVIC","TK")
models2<-x%>%dplyr::select(Model,Contributed.To.Accident)%>%dplyr::filter(Model %in% models)
models1<-x%>%dplyr::select(Model,Contributed.To.Accident)
models1<-head(models1%>%group_by(Model)%>%summarise(CountOfCases=n())%>%arrange(desc(CountOfCases)),5)
p14 <- ggplot(models1, aes(x=Model,y=(CountOfCases/sum(CountOfCases))))+scale_y_continuous(labels=scales::percent)
p14<-p14+geom_bar(fill="red",position="dodge",stat="identity",width=0.5) + ylab("Traffic Violations")
# Number of cars in each class:
p14<-p14 + xlab("Vehicle Model")
p14<-p14+geom_text(aes(label=paste0(round(100*CountOfCases/sum(CountOfCases),0),'%')), position=position_dodge(width=0.9), vjust=-0.25) + coord_cartesian(ylim=c(0,1))
p14

chi_sq_test2<-chisq.test(table(droplevels(models2$Model),models2$Contributed.To.Accident))
chi_sq_test2

test<-x
test$Date.Of.Stop<-as.Date(test$Date.Of.Stop,"%m/%d/%Y")
#class(test$Date.Of.Stop)
test<-test%>%dplyr::arrange(Date.Of.Stop)
test<-test%>%dplyr::group_by(Date.Of.Stop)%>%summarise(Traffic_Violations=n())
#test<-test%>%dplyr::filter(Date.Of.Stop)
new_DF<-test
new_DF<-new_DF %>% filter(!is.na(Date.Of.Stop))
#new_DF

p15<-ggplot(data=new_DF,aes(x=Date.Of.Stop,y=Traffic_Violations))+geom_line(color = "red", size = 0.7)
p15<-p15+xlab("Month")+ylab("Traffic Violations")
p15

tsdata<-ts(new_DF$Traffic_Violations,frequency=30)
ddata<-decompose(tsdata,"multiplicative")
p16<-plot(ddata)
p16

myModel<-forecast::auto.arima(new_DF$Traffic_Violations)
myModel

myforecast<-forecast(myModel,level=c(95),h=20)
p17<-plot(myforecast)
p17

Box.test(myModel$residuals,lag = 15,type="Ljung-Box")