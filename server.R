library(shiny)

#initialize a data.frame

Lake_Phosphorus_Model<-c("Walker, 1987 Reservoir", 
  "Canfield-Bachmann, 1981 Natural Lake",
  "Canfield-Bachmann, 1981 Artificial Lake",
   "Reckhow, 1979 General",
   "Reckhow, 1977 Anoxic",
   "Reckhow, 1977 water load < 50 m/yr",
   "Reckhow, 1977 water load > 50 m/yr",
   "Walker, 1977 General",
   "Vollenwider, 1982 Combined OECD",
   "Dillon-Rigler-Kirchner",
   "Vollenwider, 1982 Shallow Lake/Res",
   "Larsen-Mercier, 1976",
   "Nurnberg, 1984 Oxic")
Low<-rep(5,13)
Likely_Total_P<-rep(6,13)
High<-rep(10,13)

#Walker Res Model Functions
NR<-function(QS,PIN,TW) (0.17*QS/(QS+13.3))*PIN*TW
Rwalker<-function(NR) 1+((1-((1+4*NR)^0.5))/(2*NR))
Pwalker<-function(PIN,R) PIN*(1-R)
#Canfield Bachman
P_CB_Nat<-function(L,z,p) L/(z*(0.162*(L/z)^0.458+p))
P_CB_Art<-function(L,z,p) L/(z*(0.114*(L/z)^0.589+p))
#Reckhow
Preckhow_gen<-function(L,QS) L/(11.6+1.2*QS)
Preckhow_anox<-function(L,Z,TW) L/(0.17*Z + (1.13*Z/TW))
Preckhow_oxless50<-function(L,Z,TW) L/((18*Z/(10+Z))+1.05*(Z/TW)*exp(0.012*Z/TW))
Preckhow_oxmore50<-function(L,Z,TW) L/( (2.77*Z)+1.05*(Z/TW)*exp(0.0011*Z/TW) )
Pwalker_gen<-function(PIN,TW) PIN*(1/(1+0.824*(TW^0.454)))
PvollenOECD<-function(PIN,TW) 1.55*((PIN/(1+((TW)^0.5)))^0.82)
Rkirchdill<-function(QS) 0.426*exp(-0.271*QS)+0.574*exp(-0.00949*QS)
Pkirchdill<-function(L,Z,p,R) L*(1-R)/(Z*p)
Pvollenshallow<-function(PIN,TW) 1.02*( ( PIN/(1+((TW)^0.5)) )^0.88 )
Rlarsonmercier<-function(p) 1/(1+p^0.5)
Plarsonmercier<-function(PIN,R) PIN*(1-R)
Rnurnberg<-function(QS) 15/(18+QS)
#note this does not include the internal load! for P nurnberg
Pnurnberg<-function(L,QS,R) L*(1-R)/QS

# Define server calcs

shinyServer(function(input, output, session) {

#Calculate watershed area
Wshed<-reactive({    
 input$A.ag1 + input$A.ag2 + input$A.pst + input$A.urb + input$A.wet + input$A.frs
})

#Calculate P Load in pounds per year
M<-reactive({ 
 (input$A.ag1*input$ag1 + input$A.ag2*input$ag2 + input$A.pst*input$pst + input$A.urb*input$urb + input$A.wet*input$wet + input$A.frs*input$frs + input$area*input$lke)*0.89
}) 

#Areal P Load
L<-reactive({ M()*453600*3.28*3.28/(input$area*43560) })
#Mean Depth in feet
z<-reactive({   input$vol/input$area  })
#Lake Hydraulic Retention Time in years
tau<-reactive({ input$vol/(input$Q*Wshed()/12)  })
#Lake Flushing Rate
littlep<-reactive({ 1/tau() })
#Water load on lake in feet
qs<-reactive({ (input$Q/12)*Wshed()/(input$area) })
#Average Inflow P Conc
Pin<-reactive({ L()*tau()/(z()/3.28)   })



#make the storage data.frame reactive
B<-reactiveValues()
B$df<-data.frame(Lake_Phosphorus_Model,Likely_Total_P)


#now populate the storage dateframe with P solutions
observe({
    B$df[1,2]<- Pwalker(Pin(),Rwalker(NR(qs()/3.28,Pin(),tau() )))
})
observe({
    B$df[2,2]<- P_CB_Nat( L(),z()/3.28,littlep() )
})
observe({
    B$df[3,2]<- P_CB_Art( L(),z()/3.28,littlep() )
})
observe({
    B$df[4,2]<- Preckhow_gen( L(),qs()/3.28 )
})
observe({
    B$df[5,2]<- Preckhow_anox( L(),z()/3.28, tau() )
})
observe({
    B$df[6,2]<- Preckhow_oxless50( L(),z()/3.28, tau() )
})
observe({
    B$df[7,2]<- ifelse(qs()>50*3.28, Preckhow_oxmore50( L(),z()/3.28, tau() ), NA)
})
observe({
    B$df[8,2]<- Pwalker_gen(Pin(),tau())
})
observe({
    B$df[9,2]<- PvollenOECD(Pin(),tau())
})
observe({
    B$df[10,2]<- Pkirchdill( L(),z()/3.28,littlep(), Rkirchdill(qs()/3.28) )
})
observe({
    B$df[11,2]<- Pvollenshallow( Pin(),tau() )
})
observe({
    B$df[12,2]<- Plarsonmercier( Pin(), Rlarsonmercier(littlep()) )
})
observe({
    B$df[13,2]<- Pnurnberg( L(),qs()/3.28, Rnurnberg(qs()/3.28) )
})
 
 


Rnurnberg<-function(QS) 15/(18+QS)
#note this does not include the internal load! for P nurnberg
Pnurnberg<-function(L,QS,R) L*(1-R)/QS

# 
PloadText <- reactive({
  paste(" External P / Lake Area =", round( L(), digits=1), "mg/m2-yr")
})

#PconcText <- reactive({
#    paste( "P Concentration =", round(Pconc1(),digits=1), " ug/l" )
#})

MText <- reactive({
  paste(" Total External P     =", round( M() , digits=5), "lb/yr")
})
zText <- reactive({
  paste(" Mean Depth        =", round( z() , digits=5), "feet")
})
tauText <- reactive({
  paste(" Hydraulic Res Time   =", round( tau() , digits=1), "years")
})
qsText <- reactive({
  paste(" Water Load on Lake =", round( qs() , digits=2), "feet/year")
})
PinText <-reactive({
  paste(" Average Inflow P Conc =", round( Pin() , digits=1), "mg/m3 or ug/l")
})


# Return the formula text for printing as a caption
  output$pload.out <- renderText({ PloadText() })
   output$M.out <- renderText({ MText() })
output$z.out <- renderText({ zText() })
output$tau.out <- renderText({ tauText() })
output$qs.out <- renderText({ qsText() })
output$Pin.out <- renderText({ PinText() })


# Return a table
  output$table<-renderTable({B$df}, digits=0)



})
