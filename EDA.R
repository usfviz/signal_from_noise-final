library(magrittr)
library(reshape2)
library(data.table)
library(dtplyr)
library(dplyr)
library(ggplot2)
library(ggvis)
library(tidyr)

setwd("/home/ttzhou/documents/study/MSAN/coursework/spring/module-2/MSAN 622/signal_from_noise-/")

state_1BR <- read.csv('data/State/State_Zhvi_1bedroom.csv', stringsAsFactors = F)
colnames(state_1BR) <- gsub('X([0-9]+)\\.([0-9]+)', '\\1-\\2', colnames(state_1BR))

state_1BR <- melt(state_1BR,
				  id.vars = c('RegionID', 'RegionName', 'SizeRank'),
				  variable.name = 'Time',
				  variable.factor = F,
				  value.name = 'Median House Value (ZHVI)')

state_1BR$Time <- lubridate::ymd(paste0(state_1BR$Time, '-01'))
state_1BR$Time

THEME <- theme(
	panel.background = element_rect(fill = 'white'),
	panel.grid.major = element_line(color = 'grey80', size = 0.25),
	panel.grid.minor = element_blank(),
	
	plot.title = element_text(size = 14, margin = margin(t = -10, b = 10),
							  hjust = 0.5,
							  face = 'bold'),

	axis.title.x = element_text(size = 14, vjust = -5),
	axis.title.y = element_text(size = 12, vjust = 5),

	axis.text.x = element_text(size = 12, margin = margin(t = 10)),
	axis.text.y = element_text(size = 12, margin = margin(r = 10)),

	axis.line = element_line(size = 1, color = 'grey40'),

	legend.background = element_rect(fill = "white", color = "grey"),
	legend.margin = margin(0.5, 1, 0.5, 1, 'cm'),
	legend.position = c(0.91, 0.8),
	legend.title = element_blank(),
	legend.text = element_text(size = 14),
	legend.key = element_rect(fill = "white", colour = NULL),
	legend.key.width = unit(1, 'line'),

	plot.margin = unit(c(1, 1, 1, 1), 'cm')
)

## GGPLOT
p <- ggplot(data = state_1BR,
			aes(x = Time, y = `Median House Value (ZHVI)`)) +
	
	geom_line(aes(color = RegionName),
			  size = 1) +
	
	scale_x_date(breaks = date_breaks('4 years'),
				 labels = date_format('%b %Y'),
				 expand = c(0.01, 0.01)) +
	
	scale_y_continuous(labels = scales::comma) +
	
	ggtitle('Median Housing Values for 1 Bedrooms, by State') +
	
	guides(color = F) +
	
	THEME

print(p)

## GGVIS
# state_1BR %>% 
# 	subset(!is.na(`Median House Value (ZHVI)`)) %>% 
# 	ggvis(x = ~Time, y = ~`Median House Value (ZHVI)`) %>% 
# 	group_by(RegionName) %>% 
# 	layer_lines(stroke = ~RegionName, strokeWidth := 1) %>% 
# 	
# 	add_axis("x", properties = axis_props(
# 		axis = list(stroke = "black", strokeWidth = 2),
# 		grid = list(stroke = "#F8F8F8"),
# 		ticks = list(stroke = "grey90", strokeWidth = 1),
# 		labels = list(angle = 0, align = "center", fontSize = 10)
# 	)) %>% 
# 
# 	add_axis("y", properties = axis_props(
# 		axis = list(stroke = "black", strokeWidth = 2),
# 		grid = list(stroke = "#F8F8F8"),
# 		ticks = list(stroke = "grey90", strokeWidth = 1),
# 		labels = list(angle = 0, align = "right", fontSize = 10)
# 	)) %>% 
# 	
# 	hide_legend('stroke')
