#setwd("F:\\Coursera\\course_Stats_with_R\\shiny tutorials")
#install.packages("shinydashboard")
#install.packages("shiny")
#library(shiny)
#library(shinydashboard)
#library('DT')
#library("dplyr")
#choices <- c("Select All", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
#trafficData <- read.csv('Traffic_Violations.csv')
shinyServer(function(input, output, session){
  monthChoice <- reactive({
    input$Month
  })
  TimeOfDay <- reactive({
    input$TimeOfDay
  })
  #traffic <- reactive({
  #   trafficData <- read.csv('Traffic_Violations.csv')
  #  head(trafficData,100)
  
  output$mydatatable <- DT::renderDataTable(
    x, options = list(scrollX = TRUE)
    #options = list(scrollX = TRUE)
    
  )
  #input$Month<-as.numeric(input$Month)
  output$belt_ <- renderValueBox({
    valueBox( 
      #input$Month<-as.numeric(input$Month)
      if(monthChoice()=="01"){
        value = nrow(x.Jan%>%filter(Belts=="Yes"))
      }else if(monthChoice()=="02"){
        value = nrow(x.Feb%>%filter(Belts=="Yes"))
      }else if(monthChoice()=="03"){
        value = nrow(x.Mar%>%filter(Belts=="Yes"))
      }else if(monthChoice()=="04"){
        value = nrow(x.Apr%>%filter(Belts=="Yes"))
      }else if(monthChoice()=="05"){
        value = nrow(x.May%>%filter(Belts=="Yes"))
      }else if(monthChoice()=="06"){
        value = nrow(x.Jun%>%filter(Belts=="Yes"))
      }else if(monthChoice()=="07"){
        value = nrow(x.July%>%filter(Belts=="Yes"))
      }else if(monthChoice()=="08"){
        value = nrow(x.Aug%>%filter(Belts=="Yes"))
      }else if(monthChoice()=="09"){
        value = nrow(x.Sep%>%filter(Belts=="Yes"))
      }else if(monthChoice()=="10"){
        value = nrow(x.Oct%>%filter(Belts=="Yes"))
      }else if(monthChoice()=="11"){
        value = nrow(x.Nov%>%filter(Belts=="Yes"))
      }else{
        value = nrow(x.Dec%>%filter(Belts=="Yes"))
      },
      subtitle = "Seat-Belt Violation Cases", icon("angle-double-left"),
      color = "red") 
    
  })
  output$alcohol_ <- renderValueBox({
    valueBox( 
      if(monthChoice()=="01"){
        value = nrow(x.Jan%>%filter(Alcohol=="Yes"))
      }else if(monthChoice()=="02"){
        value = nrow(x.Feb%>%filter(Alcohol=="Yes"))
      }else if(monthChoice()=="03"){
        value = nrow(x.Mar%>%filter(Alcohol=="Yes"))
      }else if(monthChoice()=="04"){
        value = nrow(x.Apr%>%filter(Alcohol=="Yes"))
      }else if(monthChoice()=="05"){
        value = nrow(x.May%>%filter(Alcohol=="Yes"))
      }else if(monthChoice()=="06"){
        value = nrow(x.Jun%>%filter(Alcohol=="Yes"))
      }else if(monthChoice()=="07"){
        value = nrow(x.July%>%filter(Alcohol=="Yes"))
      }else if(monthChoice()=="08"){
        value = nrow(x.Aug%>%filter(Alcohol=="Yes"))
      }else if(monthChoice()=="09"){
        value = nrow(x.Sep%>%filter(Alcohol=="Yes"))
      }else if(monthChoice()=="10"){
        value = nrow(x.Oct%>%filter(Alcohol=="Yes"))
      }else if(monthChoice()=="11"){
        value = nrow(x.Nov%>%filter(Alcohol=="Yes"))
      }else{
        value = nrow(x.Dec%>%filter(Alcohol=="Yes"))
      },
      subtitle = "Drunk Driving Cases", icon("angle-double-left"),
      color = "red") 
    
  })
  output$hazmat_ <- renderValueBox({
    valueBox( 
      if(monthChoice()=="01"){
        value = nrow(x.Jan%>%filter(HAZMAT=="Yes"))
      }else if(monthChoice()=="02"){
        value = nrow(x.Feb%>%filter(HAZMAT=="Yes"))
      }else if(monthChoice()=="03"){
        value = nrow(x.Mar%>%filter(HAZMAT=="Yes"))
      }else if(monthChoice()=="04"){
        value = nrow(x.Apr%>%filter(HAZMAT=="Yes"))
      }else if(monthChoice()=="05"){
        value = nrow(x.May%>%filter(HAZMAT=="Yes"))
      }else if(monthChoice()=="06"){
        value = nrow(x.Jun%>%filter(HAZMAT=="Yes"))
      }else if(monthChoice()=="07"){
        value = nrow(x.July%>%filter(HAZMAT=="Yes"))
      }else if(monthChoice()=="08"){
        value = nrow(x.Aug%>%filter(HAZMAT=="Yes"))
      }else if(monthChoice()=="09"){
        value = nrow(x.Sep%>%filter(HAZMAT=="Yes"))
      }else if(monthChoice()=="10"){
        value = nrow(x.Oct%>%filter(HAZMAT=="Yes"))
      }else if(monthChoice()=="11"){
        value = nrow(x.Nov%>%filter(HAZMAT=="Yes"))
      }else{
        value = nrow(x.Dec%>%filter(HAZMAT=="Yes"))
      },
      subtitle = "Hazardous Materials Loaded Cases", icon("angle-double-left"),
      color = "red") 
    
  })
  output$fatal_ <- renderValueBox({
    valueBox( 
      if(monthChoice()=="01"){
        value = nrow(x.Jan%>%filter(Fatal=="Yes"))
      }else if(monthChoice()=="02"){
        value = nrow(x.Feb%>%filter(Fatal=="Yes"))
      }else if(monthChoice()=="03"){
        value = nrow(x.Mar%>%filter(Fatal=="Yes"))
      }else if(monthChoice()=="04"){
        value = nrow(x.Apr%>%filter(Fatal=="Yes"))
      }else if(monthChoice()=="05"){
        value = nrow(x.May%>%filter(Fatal=="Yes"))
      }else if(monthChoice()=="06"){
        value = nrow(x.Jun%>%filter(Fatal=="Yes"))
      }else if(monthChoice()=="07"){
        value = nrow(x.July%>%filter(Fatal=="Yes"))
      }else if(monthChoice()=="08"){
        value = nrow(x.Aug%>%filter(Fatal=="Yes"))
      }else if(monthChoice()=="09"){
        value = nrow(x.Sep%>%filter(Fatal=="Yes"))
      }else if(monthChoice()=="10"){
        value = nrow(x.Oct%>%filter(Fatal=="Yes"))
      }else if(monthChoice()=="11"){
        value = nrow(x.Nov%>%filter(Fatal=="Yes"))
      }else{
        value = nrow(x.Dec%>%filter(Fatal=="Yes"))
      },
      subtitle = "Fatal Accidents Encountered Cases", icon("angle-double-left"),
      color = "yellow") 
    
  })
  output$propDamage_ <- renderValueBox({
    valueBox( 
      if(monthChoice()=="01"){
        value = nrow(x.Jan%>%filter(Property.Damage=="Yes"))
      }else if(monthChoice()=="02"){
        value = nrow(x.Feb%>%filter(Property.Damage=="Yes"))
      }else if(monthChoice()=="03"){
        value = nrow(x.Mar%>%filter(Property.Damage=="Yes"))
      }else if(monthChoice()=="04"){
        value = nrow(x.Apr%>%filter(Property.Damage=="Yes"))
      }else if(monthChoice()=="05"){
        value = nrow(x.May%>%filter(Property.Damage=="Yes"))
      }else if(monthChoice()=="06"){
        value = nrow(x.Jun%>%filter(Property.Damage=="Yes"))
      }else if(monthChoice()=="07"){
        value = nrow(x.July%>%filter(Property.Damage=="Yes"))
      }else if(monthChoice()=="08"){
        value = nrow(x.Aug%>%filter(Property.Damage=="Yes"))
      }else if(monthChoice()=="09"){
        value = nrow(x.Sep%>%filter(Property.Damage=="Yes"))
      }else if(monthChoice()=="10"){
        value = nrow(x.Oct%>%filter(Property.Damage=="Yes"))
      }else if(monthChoice()=="11"){
        value = nrow(x.Nov%>%filter(Property.Damage=="Yes"))
      }else{
        value = nrow(x.Dec%>%filter(Property.Damage=="Yes"))
      },
      subtitle = "Property Damage Caused Cases", icon("angle-double-left"),
      color = "yellow") 
    
  })
  output$personal_ <- renderValueBox({
    valueBox( 
      if(monthChoice()=="01"){
        value = nrow(x.Jan%>%filter(Personal.Injury=="Yes"))
      }else if(monthChoice()=="02"){
        value = nrow(x.Feb%>%filter(Personal.Injury=="Yes"))
      }else if(monthChoice()=="03"){
        value = nrow(x.Mar%>%filter(Personal.Injury=="Yes"))
      }else if(monthChoice()=="04"){
        value = nrow(x.Apr%>%filter(Personal.Injury=="Yes"))
      }else if(monthChoice()=="05"){
        value = nrow(x.May%>%filter(Personal.Injury=="Yes"))
      }else if(monthChoice()=="06"){
        value = nrow(x.Jun%>%filter(Personal.Injury=="Yes"))
      }else if(monthChoice()=="07"){
        value = nrow(x.July%>%filter(Personal.Injury=="Yes"))
      }else if(monthChoice()=="08"){
        value = nrow(x.Aug%>%filter(Personal.Injury=="Yes"))
      }else if(monthChoice()=="09"){
        value = nrow(x.Sep%>%filter(Personal.Injury=="Yes"))
      }else if(monthChoice()=="10"){
        value = nrow(x.Oct%>%filter(Personal.Injury=="Yes"))
      }else if(monthChoice()=="11"){
        value = nrow(x.Nov%>%filter(Personal.Injury=="Yes"))
      }else{
        value = nrow(x.Dec%>%filter(Personal.Injury=="Yes"))
      },
      subtitle = "Personal Damage Faced Cases", icon("angle-double-left"),
      color = "yellow") 
    
  })
  output$license_ <- renderValueBox({
    valueBox( 
      if(monthChoice()=="01"){
        value = nrow(x.Jan%>%filter(Commercial.License=="Yes"))
      }else if(monthChoice()=="02"){
        value = nrow(x.Feb%>%filter(Commercial.License=="Yes"))
      }else if(monthChoice()=="03"){
        value = nrow(x.Mar%>%filter(Commercial.License=="Yes"))
      }else if(monthChoice()=="04"){
        value = nrow(x.Apr%>%filter(Commercial.License=="Yes"))
      }else if(monthChoice()=="05"){
        value = nrow(x.May%>%filter(Commercial.License=="Yes"))
      }else if(monthChoice()=="06"){
        value = nrow(x.Jun%>%filter(Commercial.License=="Yes"))
      }else if(monthChoice()=="07"){
        value = nrow(x.July%>%filter(Commercial.License=="Yes"))
      }else if(monthChoice()=="08"){
        value = nrow(x.Aug%>%filter(Commercial.License=="Yes"))
      }else if(monthChoice()=="09"){
        value = nrow(x.Sep%>%filter(Commercial.License=="Yes"))
      }else if(monthChoice()=="10"){
        value = nrow(x.Oct%>%filter(Commercial.License=="Yes"))
      }else if(monthChoice()=="11"){
        value = nrow(x.Nov%>%filter(Commercial.License=="Yes"))
      }else{
        value = nrow(x.Dec%>%filter(Commercial.License=="Yes"))
      },
      subtitle = "Commercial Licenses Violation Cases", icon("angle-double-left"),
      color = "green") 
    
  })
  output$contribAccident_ <- renderValueBox({
    valueBox( 
      if(monthChoice()=="01"){
        value = nrow(x.Jan%>%filter(Contributed.To.Accident=="Yes"))
      }else if(monthChoice()=="02"){
        value = nrow(x.Feb%>%filter(Contributed.To.Accident=="Yes"))
      }else if(monthChoice()=="03"){
        value = nrow(x.Mar%>%filter(Contributed.To.Accident=="Yes"))
      }else if(monthChoice()=="04"){
        value = nrow(x.Apr%>%filter(Contributed.To.Accident=="Yes"))
      }else if(monthChoice()=="05"){
        value = nrow(x.May%>%filter(Contributed.To.Accident=="Yes"))
      }else if(monthChoice()=="06"){
        value = nrow(x.Jun%>%filter(Contributed.To.Accident=="Yes"))
      }else if(monthChoice()=="07"){
        value = nrow(x.July%>%filter(Contributed.To.Accident=="Yes"))
      }else if(monthChoice()=="08"){
        value = nrow(x.Aug%>%filter(Contributed.To.Accident=="Yes"))
      }else if(monthChoice()=="09"){
        value = nrow(x.Sep%>%filter(Contributed.To.Accident=="Yes"))
      }else if(monthChoice()=="10"){
        value = nrow(x.Oct%>%filter(Contributed.To.Accident=="Yes"))
      }else if(monthChoice()=="11"){
        value = nrow(x.Nov%>%filter(Contributed.To.Accident=="Yes"))
      }else{
        value = nrow(x.Dec%>%filter(Contributed.To.Accident=="Yes"))
      },
      subtitle = "Contributed To Accidents Cases", icon("angle-double-left"),
      color = "green") 
    
  })
  output$rule_ <- renderValueBox({
    valueBox( 
      if(monthChoice()=="01"){
        value = nrow(x.Jan%>%filter(Driver.State!="MD"))
      }else if(monthChoice()=="02"){
        value = nrow(x.Feb%>%filter(Driver.State!="MD"))
      }else if(monthChoice()=="03"){
        value = nrow(x.Mar%>%filter(Driver.State!="MD"))
      }else if(monthChoice()=="04"){
        value = nrow(x.Apr%>%filter(Driver.State!="MD"))
      }else if(monthChoice()=="05"){
        value = nrow(x.May%>%filter(Driver.State!="MD"))
      }else if(monthChoice()=="06"){
        value = nrow(x.Jun%>%filter(Driver.State!="MD"))
      }else if(monthChoice()=="07"){
        value = nrow(x.July%>%filter(Driver.State!="MD"))
      }else if(monthChoice()=="08"){
        value = nrow(x.Aug%>%filter(Driver.State!="MD"))
      }else if(monthChoice()=="09"){
        value = nrow(x.Sep%>%filter(Driver.State!="MD"))
      }else if(monthChoice()=="10"){
        value = nrow(x.Oct%>%filter(Driver.State!="MD"))
      }else if(monthChoice()=="11"){
        value = nrow(x.Nov%>%filter(Driver.State!="MD"))
      }else{
        value = nrow(x.Dec%>%filter(Driver.State!="MD"))
      },
      subtitle = "Road Rules Violation Cases", icon("angle-double-left"),
      color = "green") 
    
  })
  output$plot1 <- renderPlotly({
    p1
    
  })
  output$plot2 <- renderPlot({
    set.seed(1234)
    wordcloud(words = d$word, freq = d$freq, min.freq = 5,
              scale=c(5,.5),
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  output$plot3 <- renderPlotly({
    p3
    
  })
  
  output$plot4 <- renderPlotly({
    p4
    
  })
  output$plot5 <- renderPlot({
    p5
    
  })
  output$plot6 <- renderPlot({
    p6
    
  })
  output$plot7 <- renderPlot({
    p7
    
  })
  output$plot8 <- renderPlot({
    p8
    
  })
  output$plot9 <- renderPlot({
    if(TimeOfDay()=="Nights"){
      p9
    }else if(TimeOfDay()=="Midmornings"){
      p10
    }else if(TimeOfDay()=="Afternoons"){
      p11
    }else{
      p12
    }
    
    
  })
  output$plot10 <- renderPlot({
    p13
    
  })
  
  output$plot11 <- renderPlot({
    p14
    
  })
  output$chi_sq1 <- renderPrint({ 
    chi_sq_test1
  })
  output$chi_sq2 <- renderPrint({ 
    chi_sq_test2
  })
  output$plot13 <- renderPlot({
    #tsdata<-ts(test$Traffic_Violations,frequency=30)
    #ddata<-decompose(tsdata,"multiplicative")
    plot(ddata)
    
  })
  output$plot14 <- renderPlot({
    
    #myforecast<-forecast(myModel,level=c(95),h=365)
    plot(myforecast)
    
    
  })
  
  
}
)





