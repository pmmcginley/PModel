# WEAL Lake Data Plotting

shinyUI(fluidPage(

#this sets the tab colors
 tags$style(HTML("
    .tabbable > .nav > li > a  {background-color: lightgrey;  color:black}")),

  # Application title
  titlePanel(
   fluidRow(
        column(4,tags$p(style = "font-size: 40px;","Exploring Your Lake Data ")),
        column(4,tags$p(style = "font-size: 30px;", "Water & Environmental Analysis Laboratory 715 346-3209")),
        column(3, img(src='CenterLogo.jpg',align="center",width=300))
)  
),

 selectInput('lake', 'Lake', 
            c("Golden","Long Lake (Sax) West","Long Lake (Sax) East","Can Add Other Lakes")),
    dateRangeInput('dateRange',
      label = 'Plot Date Range: yyyy-mm-dd',
      start = "2000-01-01", end = Sys.Date(),
       startview="year"),
 
   selectInput('tograph', 'What do you want to plot?', list("Chloride"=940,"Total P"=665,"Total Kjeldahl N"=625,"Secchi"=49701,"Temperature"=10,"Dissolved Oxygen"=300,"Conductivity"=95),
                selected="Chloride"),
 checkboxInput(inputId = "toponly",
      label = strong("Only show surface (upper 2 meter) samples?"),
      value = TRUE),


  tabsetPanel(type="tabs",
     tabPanel(span("Trends Over Time",style="color:black"),         

pageWithSidebar(
  headerPanel('Explore Trends Across Many Years'),
  sidebarPanel(

 ( "We suggest exploring trends in chloride, total phosphorus and conductivity" ),
 ( "to show year-to-year variations" )
  ), #for sidebarPanel
 
  mainPanel(list(tags$head(tags$style("body {background-color: #9966CC; }"))),
    plotOutput('plot1')


  ) # one for mainPanel
) # one for page with sidebar

), # one for tab panel
 tabPanel(span("Patterns within the Year",style="color:black"),
       pageWithSidebar(
           headerPanel('Explore Trends Within the Year'),
           sidebarPanel( 
 ( "We suggest you explore Secchi Depth and Total P" ),
 ( "to explore how lake biology influences them during the summer" )
  ), #for sidebarPanel
  mainPanel(
    plotOutput('plot2')
    
  ) # for MainPanel
) # for page with sidebar

), # one for tab panel
 tabPanel(span("Profiles in the Lake",style="color:black"),
       pageWithSidebar(
           headerPanel('Explore Trends Within the Lake Profiles'),
           sidebarPanel( 
 ( "We suggest plotting profiles of temperature and dissolved oxygen" ),
 ( "if you collect this information during the year" ),

 sliderInput("proyear", "Select the Year to Profile:",
                  min = 1975, max = 2020,
                  value = 2017, sep=""),
 ( "if the plot doesn't extend to the bottomm, uncheck the"),
 ( "surface samples box above")

  ), #for sidebarPanel

  mainPanel(
    plotOutput('plot3')
    
  ) # for MainPanel
) # for page with sidebar

) # one for tab panel

) # one for tabset panel
))  # two for fluid page