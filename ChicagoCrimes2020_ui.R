js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #d73925;
}"'

#UI
ui <- dashboardPage (skin = "black",
                     dashboardHeader(title = ("2020 Chicago Crimes")),
                     
                     #---------------------------------------        
                     # Defining the Sidebar
                     #---------------------------------------
                     dashboardSidebar(sidebarMenu(
                       
                       menuItem(tabName = "Overview", text = "Overview", icon = icon("clipboard-check")),
                       
                       menuItem(tabName = "Dashboard", text = "Dashboard", icon = icon("dashboard"),startExpanded = TRUE,
                                menuSubItem(tabName = "freq", text = "Crime Frequency", icon = icon("chart-bar")),
                                menuSubItem(tabName = "location", text = "Crime Locations", icon = icon("map-marker-alt")),
                                menuSubItem(tabName = "heatmap",text = "Crimes by Time of Day",icon = icon("clock")),
                                menuSubItem(tabName = "alert",text = "Location Caution",icon = icon("exclamation"))),
                       
                       menuItem(tabName = "Data", text = "Data", icon = icon("database"), startExpanded = TRUE,
                                menuSubItem(tabName = "Columns", text = "Column Details", icon = icon("info-circle")),
                                menuSubItem(tabName = "RawData", text = "Raw Data", icon = icon("table")))
                       
                       
                     )#end of sidebarMenu
                     ), #end of dashboardSidebar
                     
                     
                     #---------------------------------------        
                     # Defining the body
                     #---------------------------------------
                     dashboardBody(
                       tags$style(js),
                       tabItems(  
                         #-----------------------------------------------
                         # Overview Tab
                         #-----------------------------------------------
                         tabItem("Overview",
                                 h3("Project Overview"),
                                 
                                 br(),
                                 
                                 fluidRow(
                                   column(width = 10, offset = 1,
                                          box( width = 12,
                                               
                                               h5("2020 Chicago Crimes is a",
                                                  a(href = 'https://shiny.rstudio.com/articles/dashboards.html', 'Shiny dashboard'),
                                                  "created as a part of BUAN6357 - Advanced Business Analytics with R; Spring 2021 course at UT Dallas by",
                                                  a(href = 'https://www.linkedin.com/in/akshatabhandiwad/', 'Akshata Bhandiwad'),
                                                  ",under the guidance of",
                                                  a(href = 'https://www.linkedin.com/in/sourav-chatterjee-unt/', 'Prof.Sourav Chatterjee'),
                                                  "."),
                                               
                                               
                                               h5("Chicago has been named one of the most dangerous cities, with extremely high crime rates. The data set contains the reported incidents of crime in the City of Chicago from January 2020 to end of September 2020. It is part of data privided by Chicago Police Department via",
                                                  a(href = 'https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-Present/ijzp-q8t2', 'Chicago Data Portal'),
                                                  ". The data used for this project can be accessed through the 'Data' tab of the dashboard.")
                                               
                                          ),#end box
                                          
                                          box(title = "Crime Frequency", width = 6, status = "warning", 
                                              solidHeader = TRUE, 
                                              
                                              h5("Monthly variation of crime frequency in 2020 of Chicago can be visualized by selecting the month(s)."),
                                              h5("January, July and August months have maximum number of crimes. Of all the crimes committed, battery and theft are the most common, with nearly 40% offenses recorded every month in 2020.")
                                              
                                          ),#end box
                                          
                                          
                                          box(title = "Crime Locations", width = 6, status = "warning", 
                                              solidHeader = TRUE, 
                                              
                                              h5("The crime location map visualizes density of crime in different areas filtering by date range and crime category."),
                                              h5("Majority of Crimes are near Millennium Park, Downtown and around major expressways. These crime pockets can also be",
                                                 a(href = 'https://bestneighborhood.org/household-income-chicago-il/', 'identified'),
                                                 "as average and low income key households")
                                              
                                          ),#end box
                                          
                                          
                                          box(title = "Crime by Time of Day", width = 6, status = "warning", 
                                              solidHeader = TRUE,
                                              
                                              h5("Heatmap and cluster map are used to identify relationship between crime type and hour at which the crime is committed."),
                                              h5("Theft, Deceptive practice and other offence crime types are more frequent during afternoons. More number of Battery, Assault and Criminal Damages are recorded in evenings and night times. Locations where frequent crimes being committed at a particular time of day and day of week can be identified. The relationship between crime type and hour of the day varies during weekdays and weekends")
                                              
                                          ),#end box
                                          
                                          
                                          box(title = "Location Caution", width = 6, status = "danger", 
                                              solidHeader = TRUE,
                                              
                                              h5("City of Chicago has divided the city into 77 communities. The geographic boundaries of the communites is downloaded from",
                                                 a(href = 'https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6', 'here.')), 
                                              h5("Each community area in Chicago is assigned 'Caution Level' of High, Medium and Low based on the filtering options available in this tab"),
                                              h5("This can be utilized by the Chicago Police Department to assign patrol duties. The general public can also take necessary precautions while travelling or finding a new home in these areas.")
                                              
                                          )#end box
                                          
                                   )  #end column 
                                 )#end fluid row
                         ), #end of overview tab
                         
                         
                         
                         
                         #-----------------------------------------------
                         # Dashboard Tab
                         #-----------------------------------------------
                         
                         
                         # Crime Frequency Tab
                         #-----------------------------------------------
                         
                         
                         tabItem("freq",
                                 h3 ("Frequency of Crimes"),
                                 fluidRow(
                                   tabBox(id = "tabset1", width = 12, 
                                          tabPanel("By Month and Crime type",
                                                   
                                                   br(),
                                                   
                                                   #---- Drop down for Months -----
                                                   pickerInput( inputId = "Months",
                                                                label = "Select desired Month(s):",
                                                                choices = c("January", "February", "March", "April",
                                                                            "May", "June", "July", "August",
                                                                            "September"),
                                                                selected = c("April","June"),
                                                                options = list(`actions-box` = TRUE),
                                                                multiple = TRUE),
                                                   
                                                   #---- Value boxes ----- 
                                                   absolutePanel(id = "controls", class = "panel panel-default", 
                                                                 fixed = TRUE, draggable = FALSE, width = "500px",
                                                                 top = 180, left = "auto", right = 20, bottom = "auto",
                                                                 
                                                                 valueBoxOutput("totalcrimes", width = 6),
                                                                 valueBoxOutput("montharrest", width = 6)),
                                                   
                                                   
                                                   br(),
                                                   
                                                   #---- Bar Plot -----                                      
                                                   plotOutput(outputId = "BG_month.crimetype",height = "500px"),
                                                   
                                                   br()
                                                   
                                          ) #end tab panel 1
                                   ) #end tab box
                                 ) #end fluid row of tab item
                         ),#end freq
                         
                         
                         
                         # Locations Tab
                         #-----------------------------------------------
                         tabItem("location",
                                 h3 ("Location of Crimes"),
                                 
                                 leafletOutput("crimemap", height = "700px") %>% withSpinner(color="red"),
                                 
                                 absolutePanel(id = "controls", class = "panel panel-default", 
                                               fixed = TRUE, draggable = FALSE, width = "250px",
                                               top = 150, left = "auto", right = 20, bottom = "auto",
                                               
                                               valueBoxOutput("totalcrimesbydate",width = "250px"),
                                               
                                               
                                               dateRangeInput(inputId = "date",
                                                              label = "Select the Date:", 
                                                              min = min(crime$Date),
                                                              max = max(crime$Date),
                                                              start = "2020-03-25",
                                                              end = "2020-03-27"),
                                               
                                               
                                               prettyCheckboxGroup(inputId = "crime.type",
                                                                   label = "Select Crime Category:",
                                                                   choices = c("Arson","Assault","Battery","Damage","Exploitation","Homicide",
                                                                               "Narcotics","Non Violence","Others","Sexual","Theft","Weapons"),
                                                                   inline = FALSE,
                                                                   outline = TRUE,
                                                                   selected = c("Arson","Assault","Battery","Damage","Exploitation","Homicide",
                                                                                "Narcotics","Non Violence","Others","Sexual","Theft","Weapons"))
                                               
                                 )#end absolute panel 
                         ),#end location                   
                         
                         
                         
                         
                         
                         # Heat map  Tab
                         #-----------------------------------------------
                         tabItem("heatmap",
                                 h3 ("Crimes Types and Time of Day"),
                                 fluidRow(
                                   tabBox(id = "tabset2", width = 12, 
                                          tabPanel("Relationship Summary",
                                                   
                                                   #---- Heat map by crime type -----                                      
                                                   plotlyOutput(outputId = "heatmap", height = "650px")     
                                                   
                                          ),#end of tab panel1 
                                          
                                          tabPanel("By Location",
                                                   
                                                   #--- Map output ---
                                                   leafletOutput("heatmap.time", height = "680px")%>% withSpinner(color="red"),
                                                   
                                                   absolutePanel(id = "controls", class = "panel panel-default", 
                                                                 fixed = TRUE, draggable = FALSE, width = "280px",
                                                                 top = 180, left = "auto", right = 30, bottom = "auto",
                                                                 
                                                                 
                                                                 valueBoxOutput("percent.crime",width = "280px"),
                                                                 
                                                                 #---- Drop down for Crime Category -----
                                                                 pickerInput( inputId = "crime.type.time",
                                                                              label = "Select Crime Type(s):",
                                                                              choices = sort(unique(crime$`Primary Type`)),
                                                                              selected = c("THEFT","BATTERY"),
                                                                              options = list(`actions-box` = TRUE),
                                                                              multiple = TRUE),
                                                                 
                                                                 #---- Slider for Time of Day -----
                                                                 sliderInput(inputId = "TOD", 
                                                                             label = "Select Time of Day (OR) Click Play:",
                                                                             min = min(crime$Hour),
                                                                             max = max(crime$Hour),
                                                                             value = c(5,5),
                                                                             step = 1,
                                                                             ticks = FALSE,
                                                                             animate = animationOptions(interval = 10000, loop = TRUE)),
                                                                 
                                                                 
                                                                 #---- Check box for Weekday -----
                                                                 prettyCheckboxGroup(inputId = "week",
                                                                                     label = "Select Weekday:",
                                                                                     choices = c("Sunday", "Monday", "Tuesday", "Wednesday",
                                                                                                 "Thursday", "Friday", "Saturday"),
                                                                                     inline = FALSE,
                                                                                     outline = TRUE,
                                                                                     selected = c("Sunday", "Monday", "Tuesday", "Wednesday",
                                                                                                  "Thursday", "Friday", "Saturday"))
                                                                 
                                                   )#end absolute panel
                                          )#end of tab Panel2
                                   ) #end tab box
                                 ) #end fluid row of tab item
                         ),#end heat map        
                         
                         
                         
                         
                         # Crime Caution Tab
                         #-----------------------------------------------
                         tabItem("alert",
                                 h3 ("Crime Caution by Location"),
                                 
                                 leafletOutput("cautionmap", height = "700px") %>% withSpinner(color='red'),
                                 
                                 absolutePanel(id = "controls", class = "panel panel-default", 
                                               fixed = TRUE, draggable = FALSE, width = "280px",
                                               top = 180, left = "auto", right = 20, bottom = "auto",
                                               
                                               valueBoxOutput("high.caution.com",width = "280px"),
                                               
                                               #---- Check box for crime type -----
                                               radioGroupButtons( inputId = "crime.caution.type",
                                                                  label = "Select Crime Type(s):",
                                                                  choices = c("BATTERY", "THEFT"),
                                                                  selected = c("BATTERY")),
                                               
                                               
                                               
                                               #---- Slider for Time of Day -----
                                               sliderInput(inputId = "crime.caution.TOD", 
                                                           label = "Select Time of Day (OR) Click Play:",
                                                           min = min(crime$Hour),
                                                           max = max(crime$Hour),
                                                           value = c(6,15),
                                                           step = 1,
                                                           ticks = FALSE,
                                                           animate = animationOptions(interval = 5000, loop = TRUE)),
                                               
                                               
                                               #---- Check box for Weekday -----
                                               prettyCheckboxGroup(inputId = "crime.caution.week",
                                                                   label = "Select Weekday:",
                                                                   choices = unique(crime$Weekday.end),
                                                                   inline = FALSE,
                                                                   outline = TRUE,
                                                                   selected = unique(crime$Weekday.end))
                                               
                                 )#end absolute panel
                         ),#end alert tab                               
                         
                         
                         
                         
                         #-----------------------------------------------
                         # Data Tab
                         #-----------------------------------------------
                         tabItem("Columns",
                                 h3 ("Column Details"),
                                 
                                 fluidRow( 
                                   column(10,
                                          dataTableOutput("col.details")
                                   ) #end Column 
                                 )#end Fluid row
                         ),#end tab Item - Columns
                         
                         
                         
                         tabItem("RawData",
                                 h3 ("Data Preview and Download"),
                                 
                                 fluidRow(
                                   div(dataTableOutput("rawtable"), style = "font-size:70%"),
                                   downloadButton("downloadCsv", "Download as CSV")
                                 ) 
                         )# end Raw data
                       ) # end tab Items
                       
                     ) # End Dashboard Body
) # End Dashboard Page 

