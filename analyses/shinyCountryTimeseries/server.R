# server.R

library(RCurl)
library(stringr)
library(reshape2)
library(magrittr)
library(dplyr)
library(scales)
library(shiny)
library(foreign)
library(ggvis)
library(RColorBrewer)

url <- "https://raw.githubusercontent.com/cmrivers/ebola/master/country_timeseries.csv"

data <- getURL(url, ssl.verifypeer = FALSE)
df <- read.csv(textConnection(data))

#Drop the Date col

df <- df[, !names(df) %in% c("Date")]

#Convert to long table (day, type_place, count)
long <- na.omit(melt(df, id.vars=c("Day")))
#Split by _
long[,c("type","place")] <- colsplit(long$variable, "_", c("type","place"))

long <- long[,-2] #Drop old _-delimited col

long$type[long$type == "Case"] <- "Cases"
names(long)[1] <- "absolute.days"
names(long)[2] <- "count"

long <- long %>% group_by(place) %>% mutate(relative.days = absolute.days - min(absolute.days)) %>% mutate(count=as.numeric(count)) %>% mutate(log.count = log10(1+count))

all <- unique(long$place)
c_colors <- brewer.pal(length(all), 'Set1')
names(c_colors) <- all

#Won't work all the time, but works here.
depluralise <- function(word){
  gsub("s$", "", word)
}

all <- unique(long$place)
c_colors <- brewer.pal(length(all), 'Set1')
names(c_colors) <- all
shinyServer(function(input, output) {



  data_plot <- reactive({
	df_plot <- long

    if(input$absolute){
      df_plot$days <- df_plot$absolute.days
    }else{
      df_plot$days <- df_plot$relative.days
    }
	if(input$all_countries || length(input$countries) == 0){
		selection <- all
    }else{
        selection <- input$countries
    }
    if(input$log_scale){
      df_plot <- df_plot %>% mutate(count.value = log10(1+ count))
    }else{
      df_plot <- df_plot  %>% mutate(count.value = count)
    }
   df_plot %>% 
	 filter(place %in% selection) 
  })
  
  output$countriesList <- renderUI({
    checkboxGroupInput("countries",
                       label = h3("Countries"),
                       choices = all)
  })

    plots <- c("Cases", "Deaths")
    sapply(plots, function(plotType){
    data_plot %>% filter(type == plotType) %>% 
      ggvis(~days,~count.value) %>% 
      group_by(place,type) %>% 
      layer_points(fill=~place, stroke=~place) %>% 
      layer_lines(stroke=~place) %>%
      add_tooltip(function(data){ paste0(data$place," : ",ifelse(input$log_scale,ceiling(10**(data$count.value)-1),data$count.value), " ", data$type)}) %>%
      add_legend(c("stroke","fill")) %>%
      add_axis("x", title=paste("Days since first reported", depluralise(tolower(plotType)))) %>%
      add_axis("y", title=plotType) %>%
      bind_shiny(paste0("plot_",tolower(plotType)),paste0("plot_",tolower(plotType),"_controls"))
      })


})
