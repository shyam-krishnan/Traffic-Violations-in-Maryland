library(shiny)
library(shinydashboard)
library('DT')

#setwd("F:\\Coursera\\course_Stats_with_R\\shiny tutorials")
#choices <- c("Select All", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
title <- tags$b(tags$a(href='https://en.wikipedia.org/wiki/Maryland',tags$img(src="maryland.png", height = '40', width = '40'))," Traffic Violations in Montgomery County (MD) in 2018")
#               ' Traffic Violations in Maryland', target="_blank"
shinyUI(
  dashboardPage(skin ="red",
                
                dashboardHeader(title = title ,titleWidth=600,
                                tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/shyam-krishnan-862b13136/", icon("linkedin"), "Shyam Krishnan - Profile", target="_blank")),
                                tags$li(class="dropdown",tags$a(href="https://github.com/shyam-krishnan",icon("github"), "Source Code", target="_blank")),
                                tags$li(class="dropdown",tags$a(href="https://shyamkrishnan3.wixsite.com/myresume",icon("globe"), "My Website", target="_blank"))),
                
                dashboardSidebar(width=250,collapsed = TRUE,
                                 sidebarMenu(id = 'sidebarmenu',
                                             menuItem(text = "About Data", tabName = "About", icon=icon("clipboard"),
                                                      menuSubItem('Sample Data',
                                                                  tabName = 'Data'
                                                      ),
                                                      menuSubItem('Summary',
                                                                  tabName = 'Summary'
                                                      )),
                                             menuItem("Descriptive Analytics", tabName = "descriptiveAnalytics", icon=icon("database"),
                                                      menuSubItem('Traffic Offences',
                                                                  tabName = 'trafficOffences'
                                                      ),
                                                      menuSubItem('Driver Demographics',
                                                                  tabName = 'driverDemographics'
                                                      ),
                                                      menuSubItem('Patrol Monitoring',
                                                                  tabName = 'patrolAndMonitoring'
                                                      )),
                                             menuItem("Diagnostic Analytics", tabName = "diagnosticAnalytics", icon=icon("briefcase"),
                                                      menuSubItem('Hypothesis - 1',
                                                                  tabName = 'Hypothesis1'
                                                      ),
                                                      menuSubItem('Hypothesis - 2',
                                                                  tabName = 'Hypothesis2'
                                                      )
                                             ),
                                             menuItem("Predictive Analytics", tabName = "predictiveAnalytics", icon=icon("clock")),
                                             menuItem("Prescriptive Analytics", tabName = "prescriptiveAnalytics", icon=icon("prescription-bottle-alt"))
                                             #menuItem("Link to code files",  href = "https://www.google.com", icon=icon("code"))
                                             # https://fontawesome.com/icons?d=gallery
                                 )),
                
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "Data", DT::dataTableOutput("mydatatable")),
                    tabItem(tabName = "Summary",selectInput("Month", "Select the Desired Month", c("January"="01", "February"="02", "March"="03","April"="04", "May"="05","June"="06", "July"="07", "August"="08", "September"="09", "October"="10", "November"="11", "December"="12"), selected = "January"),
                            fluidRow(valueBoxOutput("belt_", width = 4), valueBoxOutput("alcohol_", width = 4), valueBoxOutput("hazmat_", width = 4)),
                            fluidRow(valueBoxOutput("fatal_", width = 4), valueBoxOutput("propDamage_", width = 4), valueBoxOutput("personal_", width = 4)),
                            fluidRow(valueBoxOutput("license_", width = 4), valueBoxOutput("contribAccident_", width = 4), valueBoxOutput("rule_", width = 4)),
                            tags$b(p("Note:")),
                            p("Belt Violations include not wearing seatbelts in the drivers and co-passengers seat."),
                            p("Driving a commercial motor vehicle requires a higher level of knowledge and needs a commercial license."),
                            p("Road rule violations include traffic violations committed by drivers who are not based out of Maryland.")
                    ),
                    tabItem(tabName = "trafficOffences",
                            fluidRow(box(title = "Map of Traffic Offences in the State Of Maryland", plotlyOutput("plot1", height = 450)),
                                     box(title = "Most Common Traffic Violation Cases in the State of Maryland", plotOutput("plot2", height = 450))
                                     #box(title = "Box with a plot", plotlyOutput("plot2", height = 250))
                                     #fluidRow(box(title = "Box with datatable", tableOutput("data"), width = 8),
                                     #box(title = "Box with input widget", uiOutput("inputwidget"), width = 4))
                            )
                    ),
                    tabItem(tabName = "driverDemographics",
                            fluidRow(box(title = "Traffic Violation Cases - Male vs Female", plotlyOutput("plot3", height = 450)),
                                     box(title = "Traffic Violation Cases - InState vs Out Of State", plotlyOutput("plot4", height = 450))
                                     #box(title = "Box with a plot", plotlyOutput("plot2", height = 250))
                                     #fluidRow(box(title = "Box with datatable", tableOutput("data"), width = 8),
                                     #box(title = "Box with input widget", uiOutput("inputwidget"), width = 4))
                            )),
                    tabItem(tabName = "patrolAndMonitoring",
                            fluidRow(box(title = "Traffic Violation Cases - Month on Month", plotOutput("plot5", height = 218)),
                                     box(title = "Traffic Violation Cases - Vehicle types", plotOutput("plot6", height = 218))
                                     #box(title = "Box with a plot", plotlyOutput("plot2", height = 250))
                                     #fluidRow(box(title = "Box with datatable", tableOutput("data"), width = 8),
                                     #box(title = "Box with input widget", uiOutput("inputwidget"), width = 4))
                            ),
                            fluidRow(box(title = "Traffic Violation Cases - Arrest Types", plotOutput("plot7", height = 218)),
                                     box(title = "Traffic Violation Cases - Violation Types", plotOutput("plot8", height = 218)))),
                    tabItem(tabName = "Hypothesis1",
                            selectInput("TimeOfDay", "Select the Desired Time Frame", c("Nights", "Midmornings", "Afternoons","Evenings"), selected = "January"),
                            fluidRow(box(title = "Traffic Violation Cases - Based on Time of Day", plotOutput("plot9", height = 280, width = 850), width = 10)
                                     
                                     #fluidRow(box(title = "Traffic Violation Cases - Month on Month", plotOutput("plot5", height = 218)),
                                     #box(title = "Traffic Violation Cases - Vehicle types", plotOutput("plot6", height = 218))
                                     #box(title = "Box with a plot", plotlyOutput("plot2", height = 250))
                                     #fluidRow(box(title = "Box with datatable", tableOutput("data"), width = 8),
                                     #box(title = "Box with input widget", uiOutput("inputwidget"), width = 4))
                                     #),
                                     #fluidRow(box(title = "Traffic Violation Cases - Arrest Types", plotOutput("plot7", height = 218)),
                                     #box(title = "Traffic Violation Cases - Violation Types", plotOutput("plot8", height = 218)))
                            ),
                            tags$b(p("Note:")),
                            p("Nights: From 12:00 AM till 06:00 AM"),
                            p("Midmornings: From 07:00 AM till 12:00 PM"),
                            p("Afternoons: From 01:00 PM till 06:00 PM"),
                            p("Evenings: From 07:00 PM till 11:00 PM")
                    ),
                    tabItem(tabName = "Hypothesis2",
                            fluidRow(box(title = "Association between color of vehicle and traffic violations", plotOutput("plot10", height = 380),verbatimTextOutput("chi_sq1")),
                                     box(title = "Association between automobile model and traffic violations", plotOutput("plot11", height = 380),verbatimTextOutput("chi_sq2"))
                                     #box(title = "Box with a plot", plotlyOutput("plot2", height = 250))
                                     #fluidRow(box(title = "Box with datatable", tableOutput("data"), width = 8),
                                     #box(title = "Box with input widget", uiOutput("inputwidget"), width = 4))
                            )
                    ),
                    tabItem(tabName = "predictiveAnalytics",
                            
                            fluidRow(box(title="Yearly Data Analysis Month on Month",plotOutput("plot13", height = 395)),
                                     box(title="Traffic Violation Cases prediction in 2019",plotOutput("plot14", height = 395))
                                     #box(title = "Box with a plot", plotlyOutput("plot2", height = 250))
                                     #fluidRow(box(title = "Box with datatable", tableOutput("data"), width = 8),
                                     #box(title = "Box with input widget", uiOutput("inputwidget"), width = 4))
                            ),
                            tags$b(p("Note:")),
                            p("Time Series Data has been made stationary by removing Trend, Seasonality, Cyclicity and Random Noise"),
                            p("ARIMA Parameters: p(order of time-lags = 3), d(The degree of differencing = 1), q(order of moving average model = 2)"),
                            p("Box-Ljung test of this time-series data yields a p-value of 0.2144. Hence, we can see that this ARIMA model works fine without significant autocorrelation of this time-series")
                    ),
                    tabItem(tabName = "prescriptiveAnalytics",
                            
                            fluidRow(box(title = tags$b("Key Takeaway-1"), background = "blue", solidHeader = TRUE, width = 6, height = 180,tags$b(p("The main contributors for fatal accidents are seat-belt violations and drunk-driving cases"),
                                                                                                                                                   p("Most of the fatal accidents happen in the month of February, March and April"))),
                                     box(title = tags$b("Recommendation-1"), background = "maroon", solidHeader = TRUE, width = 6, height = 180,tags$b(p("Traffic patrolling can be jacked up in the form of Motorcycle,Marked VASCAR and Mounted Patrol to curb these violations"),
                                                                                                                                                       p("Traffic Awareness campaigns can be conducted and extra precautions can be taken to avoid fatal accidents in the future")))),
                            fluidRow(box(title = tags$b("Key Takeaway-2"), background = "blue", solidHeader = TRUE, width = 6, height = 180,tags$b(p("The three top locations for traffic violations in Mongomery County are Montogomery Village Avenue (70 Cases), 270 @ Old Georgetown Road (60 Cases) and Nebel Street @ Merinelli Road (53 Cases"),
                                                                                                                                                   p("Majority of the traffic violation cases in those regions are due to speeding Vehicles, suspended licenses and driving without registration"))),
                                     box(title = tags$b("Recommendation-2"), background = "maroon", solidHeader = TRUE, width = 6, height = 180,tags$b(p("Much more awareness and Driver improvement programs can be conducted in those places to improve the safety of commuters"),
                                                                                                                                                       p("Comparatively the number of traffic violations has fallen due to the great awareness campaigns conducted about the Driver improvment programs in the second half of 2018")))),
                            fluidRow(box(title = tags$b("Key Takeaway-3"), background = "blue", solidHeader = TRUE, width = 6, height = 180,tags$b(p("Most number of property damage cases are reported in the night and most number of seatbelt violation cases are reported in the morning"),
                                                                                                                                                   p("Black and Red colored vehicles have a higher propesity to be involved in accidents"))),
                                     box(title = tags$b("Recommendation-3"), background = "maroon", solidHeader = TRUE, width = 6, height = 180,tags$b(p("Proper street illuminations and patrolling could be provided in high-risk areas at night, since the color of vehicles has a clear association with property damage and contribution to accidents"),
                                                                                                                                                       p("Marked patrol can be jacked up at high congestion areas in the mornings"))))
                            
                            
                            
                    )
                   
                    
                    
               
                    
                    
                    
                  )
                )
  )
)