#Shiny App for final proj
#Futu Chen
library(shiny)
library(RColorBrewer)
library(tidyverse)
library(tidyr)
library(tidygraph)
library(stringr)
library(data.table)
library(ggnetwork)
library(intergraph)
library(plyr)
library(dplyr)
library(plotly)
library(igraph)
library(ggraph)
library(shinythemes)
library(scales)


# dat2 <- readRDS("FinalProjCleanedv2.Rda")
# dat2$eventname_full <- paste(dat2$distance,dat2$stroke.f)
# #get some summary stats
# dat2.sum <- dat2 %>%  
#   group_by(Year, eventname_full, Gender, agegroupv2) %>%
#   summarise(num.swimmer = n()) %>% 
#   ungroup()
# saveRDS(dat2.sum, file="dat2.sum.Rda")

setwd("/Users/madaopt/Desktop/Masters-swimmers/Masters-swimmers/visual")
#read in data and some cleaning
dat2.sum <- readRDS("dat2.sum.Rda")
ag.list <- unique(dat2.sum$agegroupv2)
event.list <- unique(dat2.sum$eventname_full)
dat2.sum$agegroupv2 <- as.character(dat2.sum$agegroupv2)
dat2.sum$Year <- factor(dat2.sum$Year)
df.edges <- readRDS("justswimmer.Rda")
uniquename <- sort(as.character(unique(df.edges$swimmer1)))
dat <- readRDS("forinference.Rda")
dat.cs <- readRDS("forinference_crosssectional.Rda")
withmedals <- readRDS("withmedals.Rda")

dat$tab2text <- paste(dat$FullName, "win a", dat$cate, "medal!", "I guess s/he should be a", dat$axis.final2, "swimmer.")
dat.cs$p <- ifelse(dat.cs$Gender == "Male", "He", "She")
dat.cs$text <- paste(dat.cs$p, "entered", dat.cs$par_byevent, "event(s) prior to 2019, and pariticipated for", dat.cs$par_byyear, "year(s) from 2009 to 2018.")

netweight <- df.edges %>% select("swimmer1", "V1") %>%
  dplyr::group_by(swimmer1) %>%
  dplyr::summarise(num.connection = dplyr::n()) %>%  #get total number of connections, regardless of who they connected to
  ungroup()

#ref: https://stackoverflow.com/questions/24900903/how-to-draw-gauge-chart-in-r
gg.gauge <- function(pos,breaks=c(0,21,61,274)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/274)
    th.end   <- pi*(1-b/274)
    th       <- seq(th.start,th.end,length=274)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="cyan3")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="deepskyblue")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="dodgerblue2")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
             aes(x=1.1*cos(pi*(1-breaks/274)),y=1.1*sin(pi*(1-breaks/274)),label=paste0(breaks)))+
    annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank())
  
  
  }


ui <- fluidPage(
  theme = shinytheme("flatly"),
  fluidRow(
    column(width=12,
          sidebarLayout(
            sidebarPanel(
              width=5,
              p("From the visualization above, we can see that the total number of swimmers that entered the individual events decreases over time. However, the gap between the total number of male and female participants was also reduced. So, just as first glance, the bad news: it looks as though people are less and less likely to join the meet; the good news: we have smaller and smaller gender gaps (mainly due to the decrease in male participants). 
              Now, what if we break those numbers down into events and age groups?  
"),
              br(),
              #dropdown
              selectInput("event", "Select an event", choices = c( "50 Freestyle","100 Freestyle" , "200 Freestyle" , "500 Freestyle","1000 Freestyle"  ,"1650 Freestyle","50 Backstroke" ,  "100 Backstroke" , "200 Backstroke",  "50 Breaststroke", "100 Breaststroke","200 Breaststroke", "50 Butterfly", "100 Butterfly" , "200 Butterfly"   , "100 IM","200 IM","400 IM"), selected= "50 Freestyle", multiple=FALSE),
             # selectInput("gender",  "Choose a gender", choices = c("Male","Female")),
              selectInput("agegroup",  "Choose an age group", choices = c("18-24","25–29","30–34", "35–39", "40–44", "45–49", "50–54", "55–59", "60–64", "65–69", "70–74", "80–84", "75–79", "90–94", "85–89"))
            ),
            mainPanel(
              width=7,
              plotOutput("multi.plot")
            ) #main
          ) #sidebar
    )#col
  ),#frow
  
  p("The following four tabs allow you to customize your visualizations. The first two tabs showcase the individual data, while the 3rd and 4th tabs are views of different swimmer groups. For more information about how I categorized “fast fish” and “others”, please refer to the second tab."),
  
  tabsetPanel(
    tabPanel( "Network", 
              sidebarLayout(
                sidebarPanel(
                  width = 6,
                p("This was supposed to be a dedicated network graph, but due to the storage I have, it failed to upload to my free Plotly account. Sigh…"), p("Basically, I collapsed all the individual event data from 2009 to 2019, and made them into three columns:"),p("swimmer1: where it will filter by the name you input"),p ("swimmer2: people who swam with you in the *same event* of the *same age group* from 2009 to 2019. I just want to remind you that you actually swam with people by the 'heat' instead of your age group. However, there is no way for me to figure this out in the data, so I have to use the age group. My justification is that your place/point is eventually calculated within an age group."),p("number of time swam together: is the total number of times that you both entered the same event in the same age group."),p("The more lines you have, the more people you connected with."),
              selectInput("nametab1", "Select your name", choices = uniquename)),
              mainPanel(
                width=6,
                span(textOutput("tab1text"), style="color: #581845"),
                fluidRow(
                  column(width=12,
                         plotlyOutput("tab1plot")
                  )#col
                ),#f row
                
                fluidRow(
                  column(width=12,
                         plotOutput("tab1plot2", width = "100%", height = "200px"),
                         p("Network strength: N=3582, minimum = 1, maximum = 274, median =35, top 25% (dark blue) = 61, bottom 25% (cyan)= 21.")
                        
                  )#col
                )#f row
                
              
              )#main panel
              ),#sidebar
              
              fluidRow(
                column(width=12,
                       h4("Explanations"),
                       verbatimTextOutput("click_info_tab1"),
                       DT::dataTableOutput("click_table_tab1")
                )#col
              )#f row
      
    ), #tab1
    
    tabPanel("Individual data", 
             sidebarLayout(
               sidebarPanel(
                 width = 4,
             p("You may also be curious about how you did over time. Just like the first tab, you can input your name to view your own factors (or your network from the first tab!) "), p("The plot on the right-hand side provides you an overview of how you did over time. Speed is calculated by dividing distance (in yards) by your time. Scroll down for more fun facts!"),
             selectInput("nametab2", "Select a name (try multiple inputs)", choices = uniquename, multiple = TRUE), selected = uniquename[1]
               ),
             mainPanel(
               width=8,
               plotOutput("tab2plot")
             )#main panel
             ),
             p("
Medal: There are three medals – gold, silver, and bronze that are 'awarded' to each swimmer (and yes, everyone gets a medal!). It is measured by adding up the total place over time, divided by the total number of races they did. DQ and NS are considered missing. Therefore, I had an average place per race per person. The top 33% (from the smallest to largest, the smaller, the better) win a gold, the middle wins a sliver, and the rest wins a bronze. "),p("Fast fish: fast fishes are those people who, on average, placed first 3. It is a binary variable with 1 = fast fish and 0 = others."),p("Swimmer type: there are three types - long-axis swimmer, short-axis swimmer and IMer. It is calculated by comparing the total number of freestyle, and backstroke events participated vs. the total number of breaststroke and butterfly events. DQ and NS also count for 'participation'. If the person swam more long-axis events than short-axis events, then that person is a long-axis swimmer; If the person swam more short-axis events than long-axis events, then that person is a short-axis swimmer; If there is a draw, then I determine it by whether the person swam butterfly or not. Butterfly lover? Short-axis swimmers! Hate butterfly? Long-axis swimmers! Additionally, if that person participated in all five events in the past (free, back, breast, fly, and IM), I call that person an 'IMer'. Well-deserved!"),
             
             fluidRow(
               column(width=12,
                      span(textOutput("tab2text0"), style="color: #581845", align="center"),
                   h4(span(textOutput("tab2text1"), style="color:red", align="center")),
                    h4(span(textOutput("tab2text2"), style="color: rgb(0, 40, 158)", align="center"))
               ),#col
               column(width=12,
                      DT::dataTableOutput("tab2table")
               )#col
             )#f row
             
    ), #tab2
    
    tabPanel("Fast fishes",
             sidebarLayout(
               sidebarPanel(
                 p("This tab tells a story about those who on average, placed the first 3 per race. They can be viewed as “elite” master swimmers. Let’s see how they performed over time."), p("Small dots represent individual data points. Big dots are the mean speed with its standard deviations (as bars). Blue lines represent a regression line with confidence intervals."),
                 selectInput("eventtab3", "Select an event", choices = c( "50 Freestyle","100 Freestyle" , "200 Freestyle" , "500 Freestyle","1000 Freestyle"  ,"1650 Freestyle","50 Backstroke" ,  "100 Backstroke" , "200 Backstroke",  "50 Breaststroke", "100 Breaststroke","200 Breaststroke", "50 Butterfly", "100 Butterfly" , "200 Butterfly"   , "100 IM","200 IM","400 IM"), selected= "50 Freestyle", multiple=FALSE),
                 selectInput("gendertab3",  "Choose a gender", choices = c("Male","Female")),
                 selectInput("agegrouptab3",  "Choose an age group", choices = c("18-24","25–29","30–34", "35–39", "40–44", "45–49", "50–54", "55–59", "60–64", "65–69", "70–74", "80–84", "75–79", "90–94", "85–89"))
                 ),
               mainPanel(
                 plotOutput("tab3plot", click = "plot3_click")
               )#main panel
             ),#sidebar
             
             fluidRow(
               column(width=12,
                      h4("A Peek at Data"),
                      verbatimTextOutput("click_info_tab3"),
                      DT::dataTableOutput("click_table_tab3")
               )#col
             )#f row
             
          
    ),#tab3
    
    tabPanel("Others",
             sidebarLayout(
               sidebarPanel(
                 p("This tab tells a story about those non-fast fishes. Let’s see how they performed over time."), p("Small dota represents individual data points. Big dots are the mean speed with its standard deviations (as bars). Blue lines represent a regression line with confidence intervals."),
                 selectInput("eventtab4", "Select an event", choices = c( "50 Freestyle","100 Freestyle" , "200 Freestyle" , "500 Freestyle","1000 Freestyle"  ,"1650 Freestyle","50 Backstroke" ,  "100 Backstroke" , "200 Backstroke",  "50 Breaststroke", "100 Breaststroke","200 Breaststroke", "50 Butterfly", "100 Butterfly" , "200 Butterfly"   , "100 IM","200 IM","400 IM"), selected= "50 Freestyle", multiple=FALSE),
                 selectInput("gendertab4",  "Choose a gender", choices = c("Male","Female")),
                 selectInput("agegrouptab4",  "Choose an age group", choices = c("18-24","25–29","30–34", "35–39", "40–44", "45–49", "50–54", "55–59", "60–64", "65–69", "70–74", "80–84", "75–79", "90–94", "85–89"))
               ),
               mainPanel(
                 plotOutput("tab4plot")
               )#main panel
             ),#sidebar
             
             fluidRow(
               column(width=12,
                      h4("A Peek at Data"),
                      verbatimTextOutput("click_info_tab4"),
                      DT::dataTableOutput("click_table_tab4")
               )#col
             )#f row
    ),#tab4
    tabPanel("Some reflections",
             h4("Some take aways I found:"),
             p("Here I briefly summarized some points that I found from the visualizations, as well as highlighting a big caveat of my “naïve” regression. However, you may have a different interpretation as you explore the data."),p("
It seems that the performance over time (in terms of yards per second stratified by gender, event, and age group) is more stable among fast fishes, although it can totally be attributed to the number of data points I have for fast fishes (admit it or not, they are indeed rare-findings). Among younger age groups, it seems that we are getting slower over time, while among middle age groups, it appears that we are getting faster over time. "), p("However, this does not infer that we are getting slower! I see it as a good indicator that more and more people at different levels are joining the meet."),p("Tab 3 and tab 4 compare performance between fast fish and other types of fish. I have to say that after categorizing fast v. others, the data per age group per gender per event is sparse, that is why you can see the big error bars and sometimes the model even became saturated."),p(" A HUGE caveat I need to flag is that those data are longitudinal points, which means that even though we assume individuals are independent of each other, the measurement within each individual at different time points are correlated. Here I presented the linear model using “naïve” OLS estimator for the purpose of viewing the mean trajectories over time under the assumption of linearity. However, this violates the zero conditional mean assumption (because our error is now correlated with observations). Thus, the error bar and the confidence intervals around my slope are biased (estimates are biased too)! Well, more to cover in the “inference” session).") 

             
    )#tab5
    
  )#tabset panel
  
)#ui


server <- function(input, output) {
 
  
  output$click_table_tab1 <- DT::renderDataTable({
    result <- df.edges %>% filter(swimmer1 %in% input$nametab1)
    colnames(result) <- c("Swimmer1","Compete with swimmer2", "for this much times from 2009 to 2019")
    DT::datatable(result, rownames = FALSE)
  })
  
  output$tab2text0 <- renderText({
   
    paste("To view individualized output, make sure your name of interest is selected first")
    
  })
  
  output$tab1text <- renderText({
    tab1text <- df.edges %>% filter(swimmer1 %in% input$nametab1)  %>%  arrange(desc(V1))  
    tab1text2 <- netweight %>% filter(swimmer1 %in% input$nametab1)
   paste("Your total number of connections are", tab1text2$num.connection, ", and your maximum connection strength is", max(tab1text$V1), ". You and", tab1text[1,2], "swam", tab1text[1,3], "time(s) together. Check your network out!")
  })
  
  output$tab2text1 <- renderText({
    newdat <- dat %>% filter(FullName %in% input$nametab2) 
    newdat[1,"tab2text"]

  })
  
  output$tab2text2 <- renderText({
    newdat2 <- dat.cs %>% filter(FullName %in% input$nametab2) 
    newdat2[1,"text"]
  })
  
  output$tab2table <- DT::renderDataTable({
    result <- dat %>% filter(FullName %in% input$nametab2)
    result <- result[c("FullName" , "Gender" ,"Year", "agegroupv2" , "eventname_full", "finaltimev2","place","ydpersec", "fastfish" )]
    colnames(result) <- c("Name", "Gender", "Year", "Age group", "Event", "Final time", "Place","Speed", "Fast fish")
    DT::datatable(result, rownames = FALSE)
  })
  
  
  output$click_table_tab3<- DT::renderDataTable({
    datv2 <-  dat %>% filter( (fastfish == 1) & (eventname_full %in% input$eventtab3) & (Gender %in% input$gendertab3) & (agegroupv2 %in% input$agegrouptab3))
    result <- datv2[c("FullName", "Year" ,  "eventname_full" ,"entrytime", "final_time", "place"         ,"ydpersec")]
    DT::datatable(result, rownames = FALSE)
  })
  
  output$click_table_tab4<- DT::renderDataTable({
    datv2 <-  dat %>% filter( (fastfish == 0) & (eventname_full %in% input$eventtab4) & (Gender %in% input$gendertab4) & (agegroupv2 %in% input$agegrouptab4))
    result <- datv2[c("FullName", "Year" ,  "eventname_full" ,"entrytime", "final_time", "place"         ,"ydpersec")]
    DT::datatable(result, rownames = FALSE)
  })
  
  
  output$multi.plot <- renderPlot({  
    dat2.sum %>%
      filter((eventname_full %in% input$event) &  (agegroupv2 %in% input$agegroup)) %>%
      ggplot(aes(x=Year,y=num.swimmer, fill = Gender))+
      geom_bar(stat="identity", color="black", position=position_dodge())+
      ggtitle(paste("Number of total participants:", input$agegroup,input$event)) + 
      #scale_y_discrete(breaks = c(2009:2019)) +
      scale_fill_brewer(palette="Set3")+
      ylab("Number of total swimmers")+
      geom_text(aes(label=num.swimmer), vjust=1.6, color="black",
                position = position_dodge(0.9), size=3.5)+
      theme_bw()
  })#render
  
  output$tab1plot <- renderPlotly({
    p <- df.edges %>%
      dplyr::filter((swimmer1 %in% input$nametab1) | (swimmer2 %in% input$nametab1)) %>%
      graph.data.frame(directed = F) %>%
      as_tbl_graph() %>% activate(nodes)  %>% dplyr::mutate(text = paste(.N()$name) ) %>% 
      as.igraph()  %>%
      ggnetwork(niter=50, arrow.gap=0)  %>%
      ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edges(aes(), size=0.4, alpha=0.25) +
      geom_nodes(aes(text=text)) +
      ggtitle("") + #title need to be changed
      guides(size=FALSE) +
      theme_blank() 
   p %>% ggplotly(tooltip="text")

  })#render
  

  output$tab1plot2 <- renderPlot({
    nw2 <- netweight %>% filter(swimmer1 %in% input$nametab1) 
    gg.gauge(nw2$num.connection,breaks=c(0,21,61,274))
  })
  
  
 
  output$tab2plot <- renderPlot({ 
    
    if(length(input$nametab2) <= 1) {
      dat %>%
        filter(FullName %in% input$nametab2) %>%
        ggplot(aes(x = Year, y = ydpersec, color = eventname_full)) +
        geom_line() +
        geom_point() +
        ylab("Speed: yard per second")+
        xlab("Year")+
        scale_x_continuous(breaks=c(2009:2019)) +
        scale_y_continuous(breaks=pretty_breaks()) +
       # facet_wrap(FullName ~ ., ncol=1) +
        scale_color_discrete(name = "Event Name") +
        ggtitle("") + #title need to be changed 
        theme_bw() 
      
    }else{
      
      dat %>%
        filter(FullName %in% input$nametab2) %>%
        ggplot(aes(x = Year, y = ydpersec, color = eventname_full)) +
        geom_line() +
        geom_point() +
        ylab("Speed: yard per second")+
        xlab("Year")+
        scale_x_continuous(breaks=c(2009:2019)) +
        scale_y_continuous(breaks=pretty_breaks()) +
        facet_wrap(FullName ~ ., ncol=1) +
        scale_color_discrete(name = "Event Name") +
        ggtitle("") + #title need to be changed 
        theme_bw() 
      
    }#ifelse
    
    
  })#render
  
  
  output$tab3plot <- renderPlot({ 
    dat %>%
      filter( (fastfish == 1) & (eventname_full %in% input$eventtab3) & (Gender %in% input$gendertab3) & (agegroupv2 %in% input$agegrouptab3)) %>%
      ggplot(aes(Year,ydpersec)) +
      stat_summary(fun.data= mean_cl_normal) + 
      geom_smooth(method='lm')+
      ylab("Speed: yard per second")+
      xlab("Year")+
      geom_point(alpha=0.4, size=1) +
      scale_x_continuous(breaks=c(2009:2019)) +
      ylim(0.65,2.5)+
      #scale_y_continuous(breaks=pretty_breaks()) +
      ggtitle("") + #title need to be changed 
      theme_bw() 
  })#render
  
  
  output$tab4plot <- renderPlot({ 
    dat %>%
      filter( (fastfish == 0) & (eventname_full %in% input$eventtab4) & (Gender %in% input$gendertab4) & (agegroupv2 %in% input$agegrouptab4)) %>%
      ggplot(aes(Year,ydpersec)) +
      stat_summary(fun.data= mean_cl_normal) + 
      geom_smooth(method='lm')+
      ylab("Speed: yard per second")+
      xlab("Year")+
      ylim(0.65,2.5)+
      geom_point(alpha=0.4,size=1) +
      scale_x_continuous(breaks=c(2009:2019)) +
      #scale_y_continuous(breaks=pretty_breaks()) +
      ggtitle("") + #title need to be changed 
      theme_bw() 
  })#render
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
