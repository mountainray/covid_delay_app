library(shiny)
library(tidyverse)
library(usmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(maps)
library(lwgeom)

dfx<<-data.frame(read_csv("https://opendata.arcgis.com/datasets/1456d8d43486449292e5784dcd9ce4a7_0.csv"),
		stringsAsFactors = F)%>%
	filter(!(is.na(Value)==T & is.na(Rate)==T))%>% # n=128, missing data...
	filter(!(LABEL %in% c("Unknown Or Pending County", "Out Of State County", "International")))%>% # not in Colorado
	filter(Desc_ %in% c("Case Rates Per 100,000 People in Colorado by County","Cases of COVID-19 in Colorado by County"))%>%
	mutate(newdate=as.Date(Date, format = "%m/%d/%Y"),
	       newvalue=ifelse(grepl("Rate",Desc_)==T,Rate,Value),
	       fips=str_c("08",FIPS), newcolor="blue")%>% # combine Rate and Value columns
	arrange(newdate, fips, Desc_)%>%rename(County=LABEL)
dfx2<<-dfx%>%
#	filter(LABEL %in% c("Montezuma", "La Plata", "Dolores", "San Juan", "San Miguel",
#			    "Mineral", "Hinsdale", "Montrose", "Archuleta"))%>% # SW CO Counties
	select(-COUNTY, -FULL_, -Value, -Rate, -ObjectId)
head(dfx2)

# hospitality_2020<-c(7.5,7.6,7.9,4.8,6.0,7.2,7.4,8.0,7.9,7.9)*100
# dates<-c("2020-01-20","2020-02-20","2020-03-20","2020-04-20","2020-05-20","2020-06-20",
# 	 "2020-07-20","2020-08-20","2020-09-20", "2020-10-20")
#
# (all<-data.frame(
# 	#construction_2020,
# 	hospitality_2020,
# 	#trade_2020,
# 	datex=as.Date(dates))%>%filter(datex>="2020-04-01"))

world <<- ne_countries(scale = "medium", returnclass = "sf", continent = "north america", country = "united states of america")

counties <<- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <<- subset(counties, grepl("colorado", counties$ID))
counties$area <<- as.numeric(st_area(counties))
(county_names<<-st_as_sf(st_centroid(counties))$data)
(county_names<<-st_centroid(counties))
str(st_centroid(counties))
length(county_names$ID)
county_names$geom[1]
(county_names$geom[1])


shinyUI(navbarPage("COVID-19 Delay Application", theme = "bootstrap.css",
#shinyUI(fluidPage(
#    titlePanel(HTML("Colorado County-level COVID data<br></br><h4>Detecting delayed effects of changes in Hospitality Employment levels</h4>")),
tabPanel("Documentation", fluidPage(HTML("
<h2>Documentation...</h3>
<h3>Concept</h3>
<p>With the pandemic raging, it was of interest to see how well one could assess when the ramifications of <q>opening up</q> would
be observed as increased case counts.  Obviously monthly employment level data (in this case for Hospitality) over 2020 has seen some
appreciable increases as bars, restaurants, clubs and resorts, etc. were opened more and more.</p>
<p>With the situation (at least) somewhat politicized, and with the economic effects acute, this tool may serve as a basis for a more
robust approach.  This would allow one to estimate the degree to which a county or area (multiple counties) perpetuates COVID-19 spread
given public health administration guidelines.</p>

<h3>Data</h3>
<p>The Colorado Case data on COVID-19 positivity is updated daily, and can be found
<a href='https://opendata.arcgis.com/datasets/1456d8d43486449292e5784dcd9ce4a7_0.csv'>here</a>.</p>
<p>The Hospitality employment level data was obtained from the Bureau of Labor Statistics, local and county data.  As my location is in
southwest Colorado, the <a href='https://www.bls.gov/regions/mountain-plains/co_grandjunction_msa.htm#eag'>Grand Junction</a> data were more applicable.</p>
<h3>Processing</h3>
<p>The main plot indicates the Cases per County, across dates.  In the plot, the yellow line is a plot of the
Hospitality employment.  In this case, only the months capturing the aggressive increases in <q>opening up</q>.</p>
<p>Conceptually, we would expect as things open up, the delay in the increase in Cases is unknown.  In other words, while
it is known that the virus can take 10 to 14 days to take hold, this is affected by the level of interaction.  This level of interaction
is proxied by the opening of restaurants, bars, resorts, etc.</p>
<p>A simple GLM is fit (<mark>Cases ~ Hospitality</mark>), but with no delay (0 weeks) it is observing an increase in Hospitality
with no immediate increase in Cases (as it takes time for COVID-19 to spread).  The smaller plot is a histogram of the
model residuals (SSE), with the lowest/best fit signaled in green.</p>
<p>As you investigate/increase the delay, the model has a better fit in the future, and at some point drops off in accuracy.
 Therefore this application can give one a feel for the length of time it takes for an observed/new increase in employment to
manifest as an increase in Cases.</p>"))),
tabPanel("Main",
    titlePanel(HTML("Colorado County-level COVID data\n<h4>Detecting delayed effects of changes in Hospitality Employment levels</h4><br></br>")),
    # fluidRow("a try"),
    sidebarLayout(
        sidebarPanel(
    fluidRow(h3("Instructions..."),HTML("Pick at least 1 <b>County</b>, the map below will indicate its location.  Choose a <b>measure</b>, then <b>delay in weeks</b>.<br></br>")),
            checkboxGroupInput("county_selector",
                        "Choose county...",
            		   selected = "Montezuma",
            		   inline = T,
                        choiceNames = as.list(sort(unique(dfx2$County))), choiceValues = as.list(sort(unique(dfx2$County)))),
            plotOutput("plotmapx"),
            radioButtons("measure_selector",
                        "Choose measure...",
                        choiceNames = as.list(unique(dfx2$Desc_)), choiceValues = as.list(unique(dfx2$Desc_))),
            sliderInput("delay_selector",
                        "Choose delay...",value = 0,
            	    min = 0, max = 10, animate = T, post = " Weeks"
),
helpText("The slider above determines the delay (in weeks) from observed changes in Hospitality industry employment levels.
	 Press the Play button to cycle through them all, and observe the fit of the model lining up (or not) with Cases.")

        ),

        mainPanel(
            plotOutput("plotx2", width = "800px", height = "600px"),
            plotOutput("plotx", width = "400px", height = "200px"),
	    helpText("Sum of Squared Errors, Cases ~ Hospitality"),
            tableOutput("tablex")
        )
    )
),
# tabPanel("Documentation", fluidPage(HTML("
# <h2>Documentation...</h3><br></br>
# <h3>Data</h3><br></br>
# <p>The Colorado Case data on COVID-19 positivity is updated daily, and can be found here:<br></br>
# https://opendata.arcgis.com/datasets/1456d8d43486449292e5784dcd9ce4a7_0.csv
# <br></br>
# The Hospitality employment level data was obtained from the Bureau of Labor Statistics, local and county data.  As my location is in
# southwest Colorado, the Grand Junction data were more applicable.</p>
# <br></br>
# <h3>Processing</h3><br></br>
# <p>The main plot indicates the Cases per County, across dates.  In the plot, the yellow line is a plot of the
# Hospitality employment.  In this case, only the months capturing the aggressive increases in <q>opening up</q>.
# <br></br>
# Conceptually, we would expect as things open up, the delay in the increase in Cases is unknown.  In other words, while
# it is known that the virus can take 10 to 14 days to take hold, this is affected by the level of interaction.  This level of interaction
# is proxied by the opening of restaurants, bars, resorts, etc.
# <br></br>
# A simple GLM is fit (Cases ~ Hospitality), but with no delay (0 weeks) it is observing an increase in Hospitality
# with no immediate increase in Cases (as it takes time for COVID-19 to spread).  The smaller plot is a histogram of the
# model residuals (SSE), with the lowest/best fit signaled in green.
# <br></br>
# As you investigate/increase the delay, the model has a better fit in the future, and at some point drops off in accuracy.
#  Therefore this application can give one a feel for the length of time it takes for an observed/new increase in employment to
# manifest as an increase in Cases.</p>"))),
tabPanel("Github", fluidPage(HTML("
<h2>Links to Github code...</h2>
<p><a href='https://github.com/mountainray/covid_delay_app/blob/main/ui.R'>ui.R</a></p>

<p><a href='https://github.com/mountainray/covid_delay_app/blob/main/server.R'>server.R</a><p>")))
))

