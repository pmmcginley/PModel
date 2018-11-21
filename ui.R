library(shiny)

# Define UI for simple steady-state lake model application

shinyUI(fluidPage(

  # Application title
  titlePanel("Simple WiLMS... The Lake Phosphorus Model"),

  tabsetPanel(type="tabs",
     tabPanel("Lake Input",
         column(4,
               numericInput("area", "Lake Area (acres):", 200),
               numericInput("vol", "Lake Volume (acre-ft):", 2000),
               numericInput("Q", "Watershed Runoff Depth (inches/yr):", 10),
               ("Based on the Wisconsin Lake Modeling Suite (WiLMS)"),
               ( "developed by John Panuska and Jeff Kreider with the Wisconsin DNR")

)),
      tabPanel("Watershed Input",
         column(2,
            numericInput("A.ag1", "Ag Row Crop (acres):", 0),
            numericInput("A.ag2", "Ag Mixed Ag (acres):", 0),
            numericInput("A.pst", "Pasture     (acres):", 0),
            numericInput("A.urb", "Urban       (acres):", 0),
            numericInput("A.wet", "Wetland     (acres):", 0),
            numericInput("A.frs", "Forest      (acres):", 0),
            textInput("caption","Area of Lake",value="Lake Area from Input Tab")

),
       column(8,
            numericInput("ag1", "Likely (lb/acre-yr):", 1.0,step=0.05,width='50%'),
            numericInput("ag2", "Likely (lb/acre-yr):", 0.8,step=0.05,width='50%'),
            numericInput("pst", "Likely (lb/acre-yr):", 0.3,step=0.05,width='50%'),
            numericInput("urb", "Likely (lb/acre-yr):", 0.5,step=0.05,width='50%'),
            numericInput("wet", "Likely (lb/acre-yr):", 0.1,step=0.05,width='50%'),
            numericInput("frs", "Likely (lb/acre-yr):", 0.09,step=0.05,width='50%'),
            numericInput("lke", "Likely (lb/acre-yr):", 0.30,step=0.05,width='50%')


)),
      tabPanel("Output",
         column(4,
            (textOutput("M.out")),
            (textOutput("pload.out")),
            (textOutput("z.out")),
            (textOutput("tau.out")),
            (textOutput("qs.out")),
            (textOutput("Pin.out"))


 
),
         column(10, 
           (tableOutput("table"))


))  #two at the end of each tabPanel, commas except last

)  #one for tabsetpanel start


))  #two for fluid page 
