library(tidyverse)
library(janitor)
library(fmsb)

data <- read_csv("/Users/pujenshrestha/Downloads/prem.csv")


data <- data %>% 
  clean_names()

data <- data %>% 
  mutate(w_rank = row_number((w)),
         gf_rank = row_number((gf)),
         ga_rank = row_number(desc(ga)),
         attendance_rank = row_number((attendance)),
         x_g_rank = row_number((x_g)),
         x_ga_rank = row_number(desc(x_ga))) 

dataPlot <- data %>% 
  select(squad, w_rank, gf_rank, ga_rank,
         attendance_rank, x_g_rank, x_ga_rank)

max_min <- tibble(
  squad = c("Max", "Min"), w_rank = c(20, 0), gf_rank = c(20, 0), ga_rank = c(20, 0),
  attendance_rank = c(20, 0), x_g_rank = c(20, 0), x_ga_rank = c(20, 0))


dfPlot <- rbind(max_min, dataPlot)

dfPlot_2 <- dfPlot %>% 
  filter(squad == "Manchester City" |
         squad == "Max" |
         squad == "Min") %>% 
  select(-squad)

radarchart(dfPlot_2, axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

set.seed(99)
data <- as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data) <- paste("mister" , letters[1:3] , sep="-")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(20,5) , rep(0,5) , data)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

# plot with default options:
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

# Add a legend
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
