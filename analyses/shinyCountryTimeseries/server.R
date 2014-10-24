# server.R

library(RCurl)
library(ggplot2)
library(stringr)
library(reshape2)
library(magrittr)
library(dplyr)
library(scales)
library(shiny)
library(foreign)
library(RColorBrewer)

url <- "https://raw.githubusercontent.com/cmrivers/ebola/master/country_timeseries.csv"

data <- getURL(url, ssl.verifypeer = FALSE)
df <- read.csv(textConnection(data))

#Drop the Date col
df1_noDate <- df[, !names(df) %in% c("Date")]
#Build a series from 0...latest day in data set
day <- c(0:max(df1_noDate$Day))
#We'll add updates on each day we have data for each country here
df3_merge <- data.frame(day)

#For each country:
for(country in 2:ncol(df1_noDate)){
  df_temp <- df1_noDate[, c(1, country)] #Day,(Cases|Deaths)_Country
  #Data set is snapshots at day of reporting, with NAs representing "no change"/"no new data"
  #so ignore those with NAs.
  df_temp <- na.omit(df_temp)

  #Rescale all series so day 0 == first reported case/death
  df_temp$day.adj <- df_temp$Day - min(df_temp$Day)

  df3_merge <- merge(x = df3_merge, y = df_temp[, names(df_temp) != "Day"],
                     by.x = "day", by.y = "day.adj", all.x = TRUE)
}


row.names(df3_merge) <- df3_merge$day
df3_merge <- df3_merge[, names(df3_merge) != "day"]
#df3 is day, country, country, country, country, ...


df4 <- as.data.frame(t(as.matrix(df3_merge)))

#split into country, first get names:
vars <- colsplit(row.names(df4), "_", c("type", "place"))
df4 <- cbind(vars, df4)
row.names(df4) <- NULL

df5_melt <- melt(df4)
names(df5_melt) <- c("type", "place", "day", "count")
df5_melt$type[df5_melt$type == "Case"] <- "Cases"


all <- unique(df5_melt$place)
c_colors <- brewer.pal(length(all), 'Set1')
names(c_colors) <- all

shinyServer(function(input, output) {



  data_plot <- reactive({
    df_plot <- df5_melt[!is.na(df5_melt$count), ]

    if("All" %in% input$countries){selection <- all}
    else{selection <- input$countries}
    df_plot %>% 
      filter(place %in% selection) %>% 
      mutate(count = as.numeric(count), day=as.numeric(day)) %>%
      transform(log.count = log10(count))
  })



    plots <- c("Cases", "Deaths")
    sapply(plots, function(plotType){
    data_plot %>% filter(type == plotType) %>% ggvis(~day, ~count) %>% 
      group_by(place,type) %>% 
      layer_points(fill=~place, stroke=~place) %>% 
      layer_lines(stroke=~place) %>%
      add_tooltip(function(data){ paste0(data$place," : ",data$count, " ", data$type)}) %>%
      add_legend(c("stroke","fill")) %>%
      bind_shiny(paste0("plot_",tolower(plotType)),paste0("plot_",tolower(plotType),"controls"))
      })
    
    # g <- ggplot(data = data_plot(),
    #             aes(x = as.numeric(day), y = as.numeric(count),
    #                 group = place, color = place)) +
    #                     geom_point() + geom_line()+
    #                         facet_grid(~ type) +
    #                             scale_x_continuous(name="Days after first report") +
    #                                 scale_y_continuous(name="Counts") +
    #                                     scale_colour_manual(name="Country", values=c_colors) +
    #                                       ggtitle("Number of observations for days after first report")




})
