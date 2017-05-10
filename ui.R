library(shiny)
library(leaflet)
library(scales)
library(ggplot2)
library(ggvis)
library(ggmap)
library(magrittr)
library(reshape2)
library(data.table)
library(tidyr)
library(dplyr)
library(plotly)
source('server.R',local=TRUE)

ui <- fluidPage(
	 
	titlePanel(strong('Zillow Housing Prices')),
	
	br(),	
	br(),	

	tabsetPanel(	
		tabPanel('Compare Prices by Location',
			sidebarLayout(
				sidebarPanel(
					tags$style(" .well { background-color: white;
							   			 border: 0px;
							   			 webkit-box-shadow: none;
							   			 box-shadow: none; } ")
					
					, uiOutput(outputId = 'mapMetricSel')
					, uiOutput(outputId = 'mapTypeSel')
					, uiOutput(outputId = 'mapRegionSel')
					, uiOutput(outputId = 'mapCitySel')
					, uiOutput(outputId = 'mapMaxPriceSel')
					, br()
					, uiOutput(outputId = 'mapSubmit')
					
					, width = 2
				),
				mainPanel(
					br(),
					leafletOutput('propertyMap',
								  height = '768px'),
					width = 10
				)
			)
		),
		
		tabPanel('Visualize Pricing History',
			sidebarLayout(
				sidebarPanel(
					tags$style(" .well { background-color: white;
							   			 border: 0px;
							   			 webkit-box-shadow: none;
							   			 box-shadow: none; } ")
				
					, uiOutput(outputId = 'metricSel')
					, uiOutput(outputId = 'typeSel')
					, uiOutput(outputId = 'citySel')
					, uiOutput(outputId = 'regionSel')
					, uiOutput(outputId = 'placeSel')
					, uiOutput(outputId = 'dateSel')
					, br()
					, uiOutput(outputId = 'submit')
					, width = 2
				),
				
				mainPanel(
					br(),
					br(),
					ggvisOutput('tsViz'),
					width = 10
				)
			)
		),
		
		tabPanel('See Change in Value Over Time',
			sidebarLayout(
				sidebarPanel(
					tags$style(" .well { background-color: white;
							   			 border: 0px;
							   			 webkit-box-shadow: none;
							   			 box-shadow: none; } ")
				
					, uiOutput(outputId = 'tsHeatmapMetricSel')
					, uiOutput(outputId = 'tsHeatmapCitySel')
					, uiOutput(outputId = 'tsHeatmapRegionSel')
					, uiOutput(outputId = 'tsHeatmapMaxPriceSel')
					, br()
					, uiOutput(outputId = 'tsHeatmapSubmit')
					, width = 2
				),
				
				mainPanel(
					br(),
					leafletOutput('tsHeatmapViz',
								  height = '728px'),
					hr(),
					uiOutput(outputId = 'tsHeatmapTimeSel'),
					width = 10
				)
			)
		),
		
		tabPanel('Find the Hottest Markets',
		         headerPanel('Listing price change over time by city'),
		         selectInput("State", "State", 
		                     choices = list("CA","WA","NY"), 
		                     selected = 1),
		         
		         conditionalPanel(condition = "input.State == 'CA'",
		                          selectInput(inputId="metroca",
		                                      "Select a metro area in state California",choices = ca_metros, multiple=FALSE)),
		         conditionalPanel(condition = "input.State == 'WA'",
		                          selectInput(inputId="metrowa", 
		                                      "Select a metro area in state Washington",choices = wa_metros, multiple=FALSE)),
		         conditionalPanel(condition = "input.State == 'NY'", 
		                          selectInput(inputId="metrony", 
		                                      "Select a metro area in state New York",choices = ny_metros, multiple=FALSE)),
		         
		         mainPanel(
		           plotlyOutput("heatmap"), 
		           width = 10
		           )
		         ),
		type = 'pills'
)
)
