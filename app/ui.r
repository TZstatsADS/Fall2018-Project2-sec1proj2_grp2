
source("global.r")


navbarPage("NYC TAXI", id="nav", 
           ## Interactive Map ####################################################
           tabPanel("Interactive Map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = F, top = 60, left = "auto", right = 0, bottom = "auto",
                                      width = 160, height = 180,
                                      
                                      radioButtons("CF", label = "Layers",
                                                   choices = list("Count Number" = "count", "Fare Per Distance" = "FPD","Cluster" = "cluster1" ,"Cash Paying Percentage" = "cash"), 
                                                   selected = "count")
                                      
                                      
                        ),
                        
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 30, right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h3("Control Panel"),
                                      
                                      
                                      selectInput("days", "Days", c("All Day", "Business Day", "Not Business Day"),selected = "All Day"),
                                      
                                      
                                      checkboxInput(inputId = "showhr",
                                                    label = strong("Show hours"),
                                                    value = FALSE),
                                      
                                      conditionalPanel(condition = "input.showhr == false"
                                                       
                                      ),
                                      
                                      
                                      conditionalPanel(condition = "input.showhr == true",
                                                       sliderInput(inputId = "hr_adjust",
                                                                   label = "Choose the time of the day:",
                                                                   min = 0, max = 23, value = NULL, step = 1)
                                      ),
                                      
                                      
                                      
                                      
                                      checkboxInput("top15count", "Top 5 Count", FALSE),
                                      checkboxInput("top15FPD", "Top 5 FPD", FALSE),
                                      
                                      
                                      checkboxInput(inputId = "showbr",
                                                    label = strong("Show Borough for Top 5 counts/FPD"),
                                                    value = FALSE),
                                      
                                      conditionalPanel(condition = "input.showbr == true",
                                                       selectInput("boroSelect", "Borough for Top 5 counts/FPD", 
                                                                   c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island", "All"), 
                                                                   selected = "All")
                                      ),
                                      
                                      
                                      
                                      
                                      radioButtons("subway", label = h4("Subway Station : "),
                                                   choices = list("Do not appear" = 1, "Show all stations" = 2, "Show unique station" = 3), 
                                                   selected = 1),
                                      
                                      plotOutput("districttimeplot", height = 280),
                                      helpText(   a("Analysis",
                                                    href="https://github.com/TZstatsADS/Spr2017-proj2-grp2/blob/master/doc/analysis.html")
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
                                      draggable = TRUE, top = 60, left = 30, right = "auto", bottom = "auto",width = 330, height = "auto",
                                      h3("Hourly Taxi Flow"),
                                      selectInput("pd", "Ride Event", c("Pick up", "Drop off", "All"), selected = "Pick up"),
                                      textInput("choose date", "Date", "1/1/2015"),
                                      sliderInput("hours", "Hour of Day:", 
                                                  min = 0, max = 23, value = 0, step = 1,
                                                  animate=animationOptions(interval = 500)),
                                      helpText("Click play button to see dynamic flow data")
                        )
                    )
           ),
           
           ## Localized Map #######################################################
           tabPanel("Localized Map",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        leafletOutput("map3", width="100%", height="100%"),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 30, right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      h3("Local Situation to Call Taxi"),
                                      helpText("Use 'Locate Me' button at left to locate yourself."),
                                      helpText("Use 'Measure' button at bottom left to measure distance."),
                                      selectInput("pod", "Pick-up VS Drop-off", c("Pick up", "Drop off"), 
                                                  selected = "Pick up"),
                                      helpText("Note:"),
                                      helpText("Select Pick-up to find locations with more people picking up."),
                                      helpText("Select Drop-off to find locations with more people dropping off."),
                                      hr(),
                                      sliderInput("time", "Select Your Time",
                                                  min = 0, max = 23, value = 9, step = 1,
                                                  animate = FALSE)
                        )
                    )
           ),
           
           ##Statistical Analysis##################################################
           tabPanel('Statistical Analysis',
                    
                    sidebarLayout(      
                      
                      sidebarPanel(

                        radioButtons("DorN", "Day or Night", 
                                     choices=c('Day','Night'),
                                     selected = "Day"),
                        helpText("Note: 7am to 7pm is day"),
                        radioButtons("NSdirec", "Driving Direction", 
                                    choices=c('North','South'),
                                    selected = "North"),
                        helpText("Note: North or South"),
                        radioButtons("EWdirec", "Driving Direction", 
                                     choices=c('West','East'),
                                     selected = "West"),
                        helpText("Note: East or West"),
                        
                        checkboxInput(inputId = "gettime",
                                      label = strong("Add Time data in Hour for Heatmap"),
                                      value = FALSE),
                        helpText("Note: If you want to deploy heatmap, you have to add time data."),
                        conditionalPanel(condition = "input.gettime == true",
                                         sliderInput(inputId = "hour_hp",
                                                     label = "Choose the hour time:",
                                                     min = 0, max = 23, value = NULL, step = 1,
                                                     animate=animationOptions(interval = 500))
                        ),
                        
                        width = 2
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Scatter: Fare VS distance", plotOutput('plot1', height = "600px")),
                                    tabPanel("Heatmanp: Taxi picking-up density VS Time", leafletOutput('plot2', height = "600px"))
                                    ),
                        width = 10
                      )
                    )
           ),
           
           ## Data Explorer ###################################################
           tabPanel("Data Explorer",
                    hr(),
                    DT::dataTableOutput("rawtable"),
                    hr(),
                    DT::dataTableOutput('dynamicdata'),
                    hr()
           )
)




