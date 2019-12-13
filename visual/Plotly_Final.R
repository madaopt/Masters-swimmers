#################################################################################################################

#plotly for HOME page

#Dear TA, for the purpose of grading, just let you know that....
######You may ran into error  because I removed my api key!!!!!!!!#######
######You may ran into error because I removed my api key!!!!!!!!#######
######You may ran into error because I removed my api key!!!!!!!!#######

#################################################################################################################


library(ggplot2)
library(plotly)
library(dplyr)


#On the first page, create a timeline as introduction 

dat2 <- readRDS("FinalProjCleanedv2.Rda")
pplperyr <-  dat2 %>%  
  group_by(Year, Gender) %>%
  summarise(num.swimmer = n()) %>% 
  ungroup()
pplperyr$Gender <- factor(pplperyr$Gender)
pplperyr$text <- paste("In", pplperyr$Year, pplperyr$num.swimmer, pplperyr$Gender, "swimmers participated")


f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "Year",
  titlefont = f
)
y <- list(
  title = "Number of swimmers attended",
  titlefont = f
)


p.opening <- pplperyr %>%
  plot_ly(
    x = ~Year,
    y = ~num.swimmer,
    frame = ~Year,
    type = 'scatter',
    mode = 'markers',
    color = ~Gender,
    hoverinfo = "text",
    text = ~text,
    marker = list(
      size = 25
      ),
    showlegend = T
  ) %>%
  config(displayModeBar = F) %>%
  layout(
    xaxis = list(
      ticktext = list(c(2009:2019)), 
      tickmode = "array"
    ), yaxis = y
    )

p.opening

Sys.setenv("plotly_username"="madaopt")
Sys.setenv("plotly_api_key"="")#removed my key
api_create(p.opening, filename = "msproj.open")

#prepare club data

club <- read.csv("Team score.csv")
str(club$Team.cat)
club$Team_Category <- factor(club$Team.cat, levels = c("USMS Club", "Large Team", "Medium Team", "Small Team", "Squad"))

x2 <- list(
  title = "Year",
  titlefont = f
)
y2 <- list(
  title = "Team place",
  titlefont = f
)
p <- club %>%
 # group_by(Nameabb) %>%
  plot_ly(
    x = ~Year, 
    y = ~place, 
   frame = ~Team_Category, 
 # color  = ~Nameabb,
    text = ~paste(Name, "placed", place, "in", Year), 
    hoverinfo = "text",
   type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      ticktext = list(c(2009:2019)), 
      tickmode = "array"
    ),
    yaxis = y2
  ) %>%
#  add_lines()  %>%
animation_opts(
    1000, easing = "elastic", redraw = FALSE
  ) %>%
config(displayModeBar = F)

p

Sys.setenv("plotly_username"="madaopt")
Sys.setenv("plotly_api_key"="")
api_create(p, filename = "club_perform_easy")






