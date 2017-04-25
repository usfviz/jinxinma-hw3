facebook <- read.csv("dataset_Facebook.csv", sep = ";")
colnames(facebook) <- c("total likes", "Type", "category", "month", "Weekday", "hour",
                        "paid", "Total.Reach", "Total.Impressions", "Engaged.Users",
                        "Consumers", "Consumptions", "Impressions.by.ppl.who.liked.your.page",
                        "Lifetime.Post.reach.by.people.who.like.your.Page",                   
                        "Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post",
                        "comment",                                                            
                        "like",                                                               
                        "share",                                                              
                        "Total.Interactions")
library(shiny)
library(ggplot2)
library(plotly)
library(GGally)

ui <- fluidPage(
  headerPanel("Jinxin - Homework 3"),
  sidebarPanel(
    sliderInput(inputId = 'month', label = 'Select Month', value = 1, min = 1, 
                max = 12, step = 1)
  ),
  mainPanel(tabsetPanel(
    tabPanel("BubblePlot", plotlyOutput("BubblePlot")),
    tabPanel("ScatterPlot Matrix", plotOutput("ScatterPlot_Matrix")),
    tabPanel("Parallel", plotlyOutput("Parallel"))
  ))
)

server <- function(input, output) {
  
  output$BubblePlot <- renderPlotly(
    
    { sliderMonth <- reactive({input$month})
    ggplot() + geom_point(data = subset(facebook, month == sliderMonth()), 
                          aes(x = Total.Reach, y = Total.Impressions, colour = Type, size = like)) + 
      theme_bw() + theme(axis.title = element_blank()) +
      xlim(1000, 50000) + ylim(2000, 100000) + ggtitle("Life Time Post Total Reach VS Total Impressions") + scale_size(guide = 'none')}
  )
  
  output$ScatterPlot_Matrix <- renderPlot(
    { sliderMonth <- reactive({input$month})
    my_fn <- function(data, mapping, ...){
      p <- ggplot(data = data, mapping = mapping) + 
        geom_point() +
        geom_smooth(method=lm, fill="blue", color="red", ...)
      p
    }
    ggpairs(subset(facebook, month == sliderMonth()), columns = c(16:18), lower = list(continuous = my_fn))}
  )
  
  output$Parallel <- renderPlotly(
    { sliderMonth <- reactive({input$month})
    ggparcoord(data = subset(facebook, month == sliderMonth()), columns = 8:11, groupColumn = 'Type', scale = 'center') +  
      theme_bw() + theme(axis.title = element_blank())}
  )
}

shinyApp(ui = ui, server = server)

