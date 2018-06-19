
library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(xts)
library(scales)
library(stringr)
library(lubridate)
#library(grid)
library(gridExtra)


#####  FUNCTION TO READ SWIMS CSV FILE AND CLEAN UP
#

SWIMS.df.param<-function(df,param) 
{
#######SWIMS Download read as csv file (BE SURE TO FORMAT DATE COLUMN AS 'DATE' (mm/dd/yyyy) AND NOT 'CUSTOM'
#for this demo pull lines with TP
df.param<-subset(df,df$DNR.Parameter==param) 
#convert the Start.Date.Time to Date format
df.param$Date<-as.POSIXct(df.param$Start.Date.Time, format='%m/%d/%Y')
#cleanup by removing all but a few columns
df.param<-subset(df.param,select=c("DNR.Parameter","Date","Result","Units","Result.Depth","Header.Labslip.Depth"))
#rename the columns
colnames(df.param)<-c("Param","Date","Result","Units","Depth1","Depth2")
#removes all asterisk (note asterisk is a special character so need brackets)
df.param$Result.clean<-gsub("[*]","",df.param$Result)
# remove less than sign but add a column to indicate less than for that entry (just in case you want that later)
df.param$LessThan<-grepl("<",df.param$Result.clean)
# then remove the less than sign and replace with nothing (note you could also divide by 2 etc if thats how you want to handle less than)
df.param$Result.clean<-gsub("<","",df.param$Result.clean)
# remove NDs and indicate so in another vector ND and replace with NA
df.param$ND<-grepl("ND",df.param$Result.clean)
#convert results to numeric vector this should convert NDs to NAs (that's good)
df.param$Result.clean<-as.numeric(df.param$Result.clean)

# on to the depth
#THIS IS FOR CHEMISTRY NOT SECCHI but OK to do for all just that secchi will not have many of these filled
# first need to figure out which column to make depth so check Depth1 first if blank then use Depth2
df.param$Depth<-ifelse(df.param$Depth1=="",df.param$Depth2,df.param$Depth1)
# find units by looking for "Feet" or "Meters" and create a new "DepthUnits" col
# here using mutate from dplyr with ifelse and grepl to find text
df.param<-dplyr::mutate(df.param,DepthUnits=ifelse(grepl("Feet",Depth),"feet",
                                      ifelse(grepl("Meter",Depth),"meter",
                                      "?")))
# now pull out some depth numbers for each sample
# using str_extract from package stringr here
regexp <- "[[:digit:]]+"    # double bracket digit is any number 0 to 9 and + repeats it
df.param<-dplyr::mutate(df.param,SampleDepths=as.numeric(stringr::str_extract(Depth, regexp)))
#multiply by 3.28 if units are in meters
df.param$SampleDepthsFt<-ifelse(df.param$DepthUnits=="meter",df.param$SampleDepths*3.28,df.param$SampleDepths)
#if no depth assume it is zero feet
df.param$SampleDepthsFt<-ifelse(is.na(df.param$SampleDepthsFt),0.0,df.param$SampleDepthsFt)


#FOR SECCHI CONVERT ALL TO FEET
# DIFFERENT FROM DEPTH COLUMN AS THIS IS RESULT COLUMN
#multiply by 3.28 if units are in meters
if (param==49701) {
df.param$Result.clean<-ifelse(df.param$Units=="METERS",as.numeric(df.param$Result)*3.28,as.numeric(df.param$Result))
}
#
if (param==10) {
#for temp convert all to F
df.param$Result.clean<-ifelse(df.param$Units=="DEGREES C" | df.param$Units=="C",as.numeric(df.param$Result)*(5/9)+32,as.numeric(df.param$Result))
}
#
if (param==665) {
#for TP convert all to ug/l
df.param$Result.clean<-df.param$Result.clean*1000
}
return(df.param)
}


FindMaxValue<-function(df.param)
#finds the largest Result.clean
{
themax<-max(df.param$Result.clean)
return(themax)
}
FindMinValue<-function(df.param)
#finds the smallest Result.clean
{
themin<-min(df.param$Result.clean)
return(themin)
}



##### Surface Samples Function  ######################
# here's a function that clips out only values above a certain depth 
surface.only<-function(df,depth) {
surface.df<-subset(df,SampleDepthsFt<depth | is.na(SampleDepthsFt) )
return(surface.df)
}


################## FUNCTION TO CLEANUP WEAL DATA  ################
WEAL.df.param<-function(df.param,parameter) {
#convert the Start.Date.Time to Date format
df.param$Date<-as.POSIXct(df.param$Date, format='%m/%d/%Y')
#cleanup by removing all but a few columns
df.param<-subset(df.param,select=c("Date",parameter))
#create a new column with the parameter name
df.param$Param<-parameter
#duplicate parameter column as Result.clean to be consistent with SWIMS function
df.param$Result.clean<-df.param[[parameter]]
# remove less than sign but add a column to indicate less than for that entry (just in case you want that later)
df.param$LessThan<-grepl("<",df.param$Result.clean)
# then remove the less than sign and replace with nothing (note you could also divide by 2 etc if thats how you want to handle less than)
df.param$Result.clean<-gsub("<","",df.param$Result.clean)
# remove NDs and indicate so in another vector ND and replace with NA
df.param$ND<-grepl("ND",df.param$Result.clean)
#convert results to numeric vector this should convert NDs to NAs (that's good)
df.param$Result.clean<-as.numeric(df.param$Result.clean)
#set depth of these as 1
df.param$SampleDepthsFt<-1.0
return(df.param)
}

################


### FUNCTION TO ROWCOMBINE WEAL AND SWIMS
## YOU NEED Result.clean, Date and Depth in both
##
rowcombine<-function(df1,df2) {
# this is an rbind.fill so you need plyr
combo<-rbind.fill(df1,df2)
return(combo)
}




#####LONG TERM TREND GRAPH ######
# GRAPHING FUNCTIONS
PlotManyYears <-function(df.param,lims,yaxislabel,ymin,ymax) 
#simple param over time plot
{  
  p<-ggplot() +
  geom_point(data=df.param, ggplot2::aes(x=Date, y=Result.clean), cex=3, na.rm=TRUE) +
#  geom_smooth(ggplot2::aes(x=Date, y=Result.clean), data=df.param, method=lm, se=TRUE, fill="green",col="purple", linetype="dashed") +
  scale_x_datetime(limits=lims, date_breaks="5 year",date_minor_breaks="6 months", date_labels="%Y",name="Date") +
  ylab(yaxislabel) + 
  ylim(ymin,ymax) +
  theme_classic(base_size=18)
  return(p)
}


##############################################################
##################  DAY OF YEAR GRAPH FUNCTION  ###############
##############################################################
dayofyearplot<-function(df.param,yaxislabel,multi,ymin,ymax,reversed,yledge) 
#simple param over single year of days plot
#can adjust with multi as a mulitplier
{  
  p<-ggplot(data=df.param,aes(x=yday(Date), y=Result.clean*multi,colour=factor(year(Date)))) +
  geom_point(cex=3, na.rm=TRUE) +
  geom_line() +
#  geom_smooth(aes(x=yday(Date), y=Result.clean*multi), data=df.param, method=loess, se=FALSE, fill="lightgreen", col="black", linetype="dashed") +
  geom_smooth(aes(x=yday(Date), y=Result.clean*multi), data=df.param, method=loess, se=FALSE, col="black", linetype="dashed") +

  labs(colour="Year") +
  ylab(yaxislabel) + 
 # scale_y_reverse() +
  coord_cartesian(ylim = c(ymin,ymax),xlim=c(0,365)) +

  xlab("Day of Year") +
  theme_classic(base_size=18) 
  if(reversed) {
    p<-p+scale_y_reverse()
   }
   p<-p+guides(colour=guide_legend(nrow=3))
 #  p<-p+theme(legend.position=c(0.25,yledge),legend.direction="horizontal")
    p<-p+theme(legend.position="top",legend.direction="horizontal")


  return(p)

}


#########################################
# SINGLE YEAR PROFILE PLOT  ############################
#########################################

paramprofile<-function(df.param,yaxislabel,ymin,ymax,xaxislabel,xmin,xmax,leglabel) 
#simple param over single year of days plot
{  
  p<-ggplot(data=df.param,aes(y=Result.clean, x=SampleDepthsFt, colour=factor(format(df.param$Date,"%m/%d/%y")))) +
     geom_point(cex=5,na.rm=TRUE) +
     geom_line() +
     labs(colour=leglabel) +
  xlab(yaxislabel) + 
  xlim(ymin,ymax) +
  ylab(xaxislabel) +
  ylim(xmin,xmax) +
  coord_flip() +
  theme_classic(base_size=18) 
 p<-p+theme(legend.position="top")
  return(p)
}




#################

#establish the WEAL and SWIMS equivalency not reactive    
WEALorder<-c("Cl","TP","TKN","None","None","None","Cond_l")
SWIMSorder<-c(940,665,625,49701,10,300,95)
labelorder<-c("Chloride mg/l","Total P ug/l","Total Kjeldahl N mg/l","Secchi feet","Temperature","Dissolved Oxygen","Conductivity")

#create the dropdown lake name correlation with the SWIMS and WEAL file names
#note that these need to be in the same order
dropdownorder<-c("Golden","Long Lake (Sax) West","Long Lake (Sax) East")
swimscsvorder<-c("Goldenlake.csv","Long Lake West.csv","Long Lake East.csv")
wealcsvorder<-c("Golden 1700583.csv","Long Lk W 1600152.csv","Long Lk E 1600152.csv")




function(input, output, session) {


# Read the SWIMS and WEAL csv files
thelake<-reactive({input$lake})
#SWIMS  
df<-reactive({read.csv(swimscsvorder[dropdownorder==thelake()],header=TRUE,stringsAsFactors=FALSE)})
#WEAL 
#make sure you are using the WEAL file with the "less than signs"
WEAL <- reactive({read.csv(wealcsvorder[dropdownorder==thelake()], header=TRUE,stringsAsFactors=FALSE)})

#set graph time limits
limits<-reactive({as.POSIXct(input$dateRange)})

#get the SWIMS and WEAL data for just the parameter of interest
 SWIMS.param<-reactive({SWIMS.df.param(df(),input$tograph)}) 
#Note some nice shiny work here using logic inside a reactive assignment
 WEAL.param<-reactive({ 
   newparam<-input$tograph  #assign here to make reactive
   if (WEALorder[SWIMSorder==newparam] != "None") {
   WEALpiece<-WEAL.df.param(WEAL(),WEALorder[SWIMSorder==newparam]) 
    } else {
   WEALpiece<-data.frame() #is just empty
    }
   WEALpiece
})
#now combine SWIMS and WEAL then clip to date range
both<- reactive({
   combo<-rowcombine(SWIMS.param(),WEAL.param())
   combo<-combo[ymd(combo$Date) >= limits()[1], ]
   combo<-combo[ymd(combo$Date) <= limits()[2], ]
   if (input$toponly) combo<-{surface.only(combo,6)}
   combo
})




graphlabel<-reactive({ labelorder[SWIMSorder==input$tograph] })


maxvalue<-reactive({FindMaxValue(both())})
minvalue<-reactive({FindMinValue(both())})

#make the plots
  output$plot1 <- renderPlot({
   PlotManyYears(both(),limits(),graphlabel(),0,maxvalue())
  })
  output$plot2 <- renderPlot({
   dayofyearplot(both(),graphlabel(),1,0,maxvalue(),FALSE,0.2) 
  })
 output$plot3 <- renderPlot({
   paramprofile(subset(both(),year(Date)==input$proyear),"Depth (ft)",50,0,graphlabel(),minvalue(),maxvalue(),"Profile Date") 
  })

}
 
