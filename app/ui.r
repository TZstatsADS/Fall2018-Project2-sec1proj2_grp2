
source("../app/global.R")

##ui part#########################################################################

navbarPage("NYC TAXI", id="nav", 
           ## Interactive Map ####################################################
           tabPanel("Interactive Map",
                    div(class="outer",
                        tags$head(
                          ## Include our custom CSS ##
                          includeCSS("styles.css"),
                          includeScript("gomap.js")),
                        leafletOutput("map", width="100%", height="100%"),
                        
                        ## Layers ##
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,draggable = F, top = 60, left = "auto", right = 0, bottom = "auto",width = 160, height = 220,
                                      radioButtons("CF", label = strong("Layers"),choices = c("Probability" = "Prob", "Trip Direction" = "Heatmap","Count Number" = "count", "Fare By Distance" = "FPD","Cluster" = "cluster1" ,"Percent Paying Cash" = "cash"),selected = "Prob")
                        ),
                        
                        ## Probability&Recommand Position ##############################
                        conditionalPanel(condition = "input.CF.indexOf('Prob')>-1",
                                         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,draggable = TRUE, top = 60, left = 0, right = "auto", bottom = "auto",width = 330, height = "auto",
                                                      h2("Recommended Taxi Pick-up Locations"),
                                                      radioButtons("type",strong("Result Type"),choices=c("Heatmap","Points"),selected="Heatmap"),
                                                      selectInput("pd2", strong("Ride Event"), c("Pick-up", "Drop-off"),selected = "Pick up"),
                                                      sliderInput("radius_pos", strong("Radius of 'Near' Locations"), min=0, max=500, value = 250, step=10),
                                                      conditionalPanel(condition="input.type=='Points'",sliderInput("time", strong("Set Time"),min = 0, max = 23, value = 9, step = 1,animate = FALSE)),
                                                      strong(textOutput("pos")),
                                                      textOutput("lng"),
                                                      textOutput("lat"))
                        ),
                        
                        ## Direction Heatmap ###########################################
                        conditionalPanel(condition = "input.CF.indexOf('Heatmap')>-1",
                                         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,draggable = TRUE, top = 60, left = 0, right = "auto", bottom = "auto",width = 330, height = "auto",
                                                       h2("Trip Direction"),
                                                       selectInput("direction", strong("Direction"),choices=c('Northbound','Southbound')),
                                                       sliderInput("Amount2", strong("Number of Pick-up Locations"), 1, 10000, 1000, step =1000),
                                                       sliderInput("pickuphour", strong("Pick-up Time"), 0, 23, 0, step =1)
                                                       )
                                         ),
  
                        ## Others ######################################################
                        conditionalPanel(condition = "input.CF.indexOf('count')>-1|input.CF.indexOf('FPD')>-1|input.CF.indexOf('cluster1')>-1|input.CF.indexOf('cash')>-1",
                                         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,draggable = TRUE, top = 60, left = 0, right = "auto", bottom = "auto",width = 330, height = "auto",
                                                       h2("Panal"),
                                                       selectInput("days", "Days", c("All Day", "Business Day", "Not Business Day"),selected = "All Day"),
                                                       selectInput("subway","Subway Station",choices =list("Do not appear" = 1, "Show all stations" = 2, "Show unique station" = 3), selected = 1),
                                                       selectInput("boroSelect", "Borough for 5 Largest Fares by Distance",c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island", "All"), selected = "All"),
                                                       #checkboxInput("opt","Optional",value=FALSE),
                                                       #conditionalPanel("input.opt=='false'" ),
                                                       #conditionalPanel("input.opt=='true'",
                                                       checkboxInput("showhr","Show hour",value=FALSE),
                                                       conditionalPanel(condition = "input.showhr==false"),
                                                       conditionalPanel(condition = "input.showhr==true",sliderInput("hr_adjust","Time of Day",min = 0,max = 23, value = NULL, step = 1)),
                                                       checkboxInput("top15count", "Top 5 Count", FALSE),
                                                       checkboxInput("top15FPD", "Top 5 FPD", FALSE)
                                                       #checkboxInput(inputId = "showbr",label = strong("Show Borough for Top 5 counts/FPD"),value = FALSE)
                                                       #conditionalPanel(condition = "input.showbr==false"),
                                                       #conditionalPanel(condition = "input.showbr==true",selectInput("boroSelect", "Borough for Top 5 counts/FPD",c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island", "All"), selected = "All")
                                                       )
                        )
                    )
           ),
                     
           ## Dynamic Map #######################################################
           tabPanel("Dynamic Map",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        leafletOutput("map2", width="100%", height="100%"),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",width = 330, height = "auto",
                                      h2("Hourly Taxi Flow"),
                                      selectInput("pd", "Ride Event", c("Pick-up", "Drop-off", "All"), selected = "Pick up"),
                                      textInput("choose date", "Date", "1/1/2015"),
                                      sliderInput("hours", "Hour of Day:", 
                                                  min = 0, max = 23, value = 0, step = 1,
                                                  animate=animationOptions(interval = 500)),
                                      helpText("Click play button to see dynamic flow data")
                        )
                    )
           ),
           ######
           tabPanel('Statistical Analysis',
                    
                    sidebarLayout(      
                      
                      sidebarPanel(
                        selectInput("method", "Payment Method:", 
                                    choices=c('cash','noncash')),
                        selectInput("type2", "Pickup or Dropoff:", 
                                    choices=c('pickup','dropoff')),
                        hr()
                      ),
                      
                      # Create a spot for the barplot
                      mainPanel(
                        plotlyOutput('plot2'),
                        br(),
                        br(),
                        plotlyOutput('payment_type')
                      )
                    )),
           
           ## Data Explorer ###################################################
           tabPanel("Data Explorer",
                    hr(),
                    DT::dataTableOutput("rawtable"),
                    hr(),
                    DT::dataTableOutput('dynamicdata'),
                    hr()
           )
)




