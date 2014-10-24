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

df1_noDate <- df[, !names(df) %in% c("Date")]
day <- c(0:df1_noDate[1, 1])
df3_merge <- data.frame(day)

theme_set(theme_minimal())
#Calculate death rates:
for(i in 2:ncol(df1_noDate)){
  df_temp <- df1_noDate[, c(1, i)]
  df_temp <- na.omit(df_temp)
  last <- nrow(df_temp)
  last
  df_temp[last,1]
  day.offset <- df_temp[last,1]
  df_temp$day.adj <- df_temp$Day - day.offset
  df_temp
  df3_merge <- merge(x = df3_merge, y = df_temp[, names(df_temp) != "Day"],
                     by.x = "day", by.y = "day.adj", all.x = TRUE)
}



row.names(df3_merge) <- df3_merge$day
df3_merge <- df3_merge[, names(df3_merge) != "day"]

#df4 <- df3_merge %>%
#    as.matrix() %>%
#        t() %>%
#            as.data.frame()

df4 <- as.data.frame(t(as.matrix(df3_merge)))

vars <- colsplit(row.names(df4), "_", c("type", "place"))
df4 <- cbind(vars, df4)
row.names(df4) <- NULL

df5_melt <- melt(df4)
names(df5_melt) <- c("type", "place", "day", "count")
df5_melt$type[df5_melt$type == "Case"] <- "Cases"


all <- c("Guinea", "Liberia", "SierraLeone", "Nigeria", "Senegal")
c_colors <- brewer.pal(length(all), 'Set1')
names(c_colors) <- all

shinyServer(function(input, output) {



  data_plot <- reactive({
    df_plot <- df5_melt[!is.na(df5_melt$count), ]

    if("All" %in% input$countries){selection <- all}
    else{selection <- input$countries}
    df_plot <- df_plot[df_plot$place %in% selection, ]
    df_plot %>% dplyr::mutate(count = as.numeric(count), day=as.numeric(day))
  })



    
    data_plot %>% ggvis(~day, ~count) %>% 
      group_by(place,type) %>% 
      layer_points(fill=~place) %>% 
      layer_lines(stroke=~place) %>%
      add_tooltip(function(data){ paste0(data$place," : ",as.numeric(data$count), " ", data$type)}) %>%
      bind_shiny("plot","plot_controls")
    
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
