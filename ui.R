# renderPlot() function in shiny
# for this example, we will use the iris data set
str1 <- " Please click on any of the below tabs and allow about one minute for the initial"
str2 <- "data download to visualize your data.  Once the data has been downloaded once,"
str3 <- "user can change any input value and/or select a new output tab and see the changes"
str4 <- "almost instantaneously."
str5 <- 'Designed and built by Mark Malter'

library(shiny)

shinyUI(fluidPage(
  titlePanel(HTML(paste(h6("Twelve Month Rolling Chicago Crimes: ",str1,str2,str3,str4,str5),sep='<br/>'))),
  sidebarLayout(
    sidebarPanel(
      
      textInput(inputId="beat",label="Beat Number (entire city if blank)",value="2222"),
      br(),
      
      
      p("hold control/command key to add multiple inputs"),
      selectInput("crimeType","Select the crime type(s) of interest",
                  c("ALL CRIMES","BATTERY","CRIMINAL DAMAGE","ROBBERY","MOTOR VEHICLE THEFT","THEFT","NARCOTICS",                        
                    "DECEPTIVE PRACTICE","BURGLARY","PUBLIC PEACE VIOLATION","ASSAULT","SEX OFFENSE","CRIMINAL TRESPASS",                
                    "INTERFERENCE WITH PUBLIC OFFICER","OFFENSE INVOLVING CHILDREN","WEAPONS VIOLATION","HOMICIDE",                         
                    "CRIM SEXUAL ASSAULT","KIDNAPPING","STALKING","ARSON","GAMBLING","OTHER NARCOTIC VIOLATION",
                    "PROSTITUTION","LIQUOR LAW VIOLATION","INTIMIDATION","NON-CRIMINAL",
                    "PUBLIC INDECENCY","OBSCENITY","CONCEALED CARRY LICENSE VIOLATION","OTHER OFFENSE"),
                  selected=c("BATTERY","MOTOR VEHICLE THEFT","CRIMINAL DAMAGE","ROBBERY",
                             "NARCOTICS"),selectize=F,multiple=T),
      
      
      selectInput(inputId="day",label="Select the day of interest",
                  choices=c("All Week","Sunday", "Monday","Tuesday",
                            "Wednesday","Thursday","Friday","Saturday"),
                  selected="All Week",selectize=F,multiple=T),
      br(),
      
      selectInput(inputId="hour", label="Select the hour of the day",
                  choices = c("All Day","0:00","1:00","2:00","3:00","4:00","5:00","6:00","7:00","8:00",
                              "9:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00",
                              "17:00","18:00","19:00","20:00","21:00","22:00","23:00"),
                  selected="All Day", selectize=F,multiple=T),
      br(),
      sliderInput("range","Month:",
                  min=1,max=12,format="###",
                  value=c(1,12),step=1),
      
      br(),
      
      radioButtons("recency","Plot by crime total or recency:",c("crime total","crime recency"),inline=TRUE),
      radioButtons("axes","Scatter plot axes:",c("lng-lat","miles"),inline=TRUE),
      radioButtons("levels","Order of bar levels",c("crimes-day","day-crimes"),inline=TRUE),
      radioButtons("arrest","Arrest Made:",c("all cases","yes","no"),inline=TRUE),
      #radioButtons("clusters","Cluster type",c("time","spatial"),inline=TRUE),
      
      textInput(inputId="num.clusters",label="Number of crime clusters",value="3")
      
      
      
    ),
    
    mainPanel(
      tabsetPanel(type="tab",
                  tabPanel("Introduction",
                    HTML("<div><b>Sidebar Layout:</b><br></br>
      <p>Beat Number: enter beat number of interest, or leave blank to view crimes across entire city.</p>
      <p>Days- leave 'All Week', or select the day(s) of your choice.</p>
      <p>Hours- leave 'All Day', or select the hours(s) of your choice.</p>
      <p>Months- slide the switch to view month range of interest.</p>
      <p>Crimes- By default, five crimes are selected. Click the crimes of interest.</p>
      <p>Arrests made- Default is all cases, but user can click 'yes' or 'no to view only a subset of all cases.</p>
      <p>Recency button: Allows view of either crime counts or crime recency counts, which gives more weight
      to recent crimes than to older crimes.</p>
      <p>Axes button: Allows the choice between viewing the spatial scatter plots with axes in either geo 
      coordinates or in mileage from beat center.</p>
      <p>Levels: For the bar charts, the user has the option of viewing crimes by day, or days by crime.</p>
      
      <b>Main Panel:</b>
      <p>Selected Crimes Scatterplot: Plots all selected crimes within territory and timeframe, with dots sized either 
      according to recency or by crime count; and axes either geocoded or mileage.</p>
      <p>Crime count- Bar chart of weekdays per selected crimes. Can be viewed as either crime counts or crime recency
         counts, and with days/crimes bars or crimes/days bars.</p>
      <p>Cluster analysis.  Select up to six crime types and enter the desired number of clusters.  Can view
       either crime spatial clusters or crime time clusters.</p>  
      <p>Table of top crimes in chosen area over next four hour time bucket as of the current hour.</p>      
            <p>Click to download the Chicago crimes data used in this analysis,
            <a href= 'http://data.cityofchicago.org/views/x2n5-8w5q/rows.csv'>Chicago crimes over the last 12 months</a>,</p>
            <b>Warning:</b>The crimes file is about 23 MB in size.</p>
            <p>Or click to view the Chicago police department beat map, 
            <a href= 'http://gis.chicagopolice.org/pdfs/district_beat.pdf'>Chicago police beat map</a></p>
            Please play around with the app and enjoy.  Feel free to provide feedback to malter61@gmail.com <br></br>
              Mark
                          </div>")),
                                   
                                                         
                  #tabPanel("Introduction"),
                  tabPanel("Selected Crimes Scatter Plot",plotOutput("ggplot1"),
                    HTML("<div><p><br></br>
                            <p><b>Choose your beat number in the box at the left</b> (see the link below to the Chicago police beat map)</p>
                            <p>Select the crimes of interest to view.</p>
                           <p>Select 'lat-lng' on the left panel to view by geo codes, or select 'miles' 
                           to view by miles from center of beat.</p>
                           Select 'crime total' to view all crimes by count, or select 'crimes recency' to give greater weight
                            to more recent crimes.</p><br></br>
                           Click to view the Chicago police department beat map, 
                         <a href= 'http://gis.chicagopolice.org/pdfs/district_beat.pdf'>Chicago police beat map</a>
                         </div>")),
                  
                  #tabPanel("Distance Scatter Plot",plotOutput("ggplot2")),
                  tabPanel("Crime Count",plotOutput("ggplot3"),
                    HTML("<div><p><br></br><p>Select the crimes of interest to view.</p>
                            Select 'crimes-day' to view frequency of days by selected crime, or select 'day-crimes' to view
                            crime frequency by day.</p>
                            <p>Select 'crime total' to view crimes by count, or 'crime recency' to give higher weight to more recent crimes.</p>
                           <p>Select hour of day and/or 'Arrest Made', if interested.</p></div>")),
                  #tabPanel("Crime Recency 1",plotOutput("ggplot4")),
                  #tabPanel("Crime Recency 2",plotOutput("ggplot5")),
                  tabPanel("Spatial Clusters",plotOutput("ggplot6")),
                  tabPanel("Time Clusters",plotOutput("ggplot7")),
                  tabPanel("Time Scatter Plot",plotOutput("ggplot8"),
                           HTML("<div><p>Here we show crimes by day of week and on a 24 hour clock.  Bubble sizes are proportional to 
                                the particular crime count on the selected day/hour.</p></div>")),
                  tabPanel("Top Crimes in Next Four Hours",tableOutput("freq.data"),
                           HTML("<div><p>As of the current hour and day of week, this table shows the top crimes that have occurred over the past
                                twelve month period.  For example, if it is currently 9:30 PM on Friday, we see the top crimes in the last three hours
                                on Friday nights, and on the first hour Saturday morning.</p></div>"))
                  #tabPanel("About the Sidebar Layout",htmlOutput("instructions1")),
                  #tabPanel("About the Sidebar Layout",includeMarkdown("SidebarLayout.md.docx"))
                  
                  
                  #plotOutput("ggplot1"),
                  #downloadButton(outputId="down",label="download plot" )
      )
    )         
    
  )))


