library(shiny)
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard For Moonquake Classification"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Moonquake Dataset"),
      menuItem("Dashboard"),
       menuSubItem("Moonquake Types"),
       menuSubItem("Max Velocity"),
       menuSubItem("Min Velocity"),
      menuItem("Dataset Summary")
    )
  ),
  dashboardBody
  (
    box(title = "Full Dataset",status = "danger",solidHeader = TRUE,width = 15,DT::dataTableOutput("fulldata")),
    fluidRow
    (
      box(title="Barplot-Moonquake Types",status="primary",solidheader=TRUE,width = 6, plotOutput("Barplot")),
      box(title="Piechart-Moonquake Types",status="warning",solidheader=TRUE,width=6,plotOutput("Peichart"))
    ),
    fluidRow(
      box(title="Histogram-MaxVelocity",plotOutput("histogram1")),
      box(sliderInput("bins","Number Of Breaks",1,100,50,animate=TRUE))
    ),
    fluidRow(
      box(title="Histogram-MinVelocity",plotOutput("histogram2")),
      box(sliderInput("bins","Number Of Breaks",1,100,50,animate=TRUE))
    ),
      box(title = "Dataset Summary",status = "info",solidHeader = TRUE,width=16,verbatimTextOutput("summary"))
  )
)
server<-(function(input,output){
  output$fulldata<-DT::renderDataTable({
    data
  })
  output$Barplot<-renderPlot({
    moonquake_type<-c(data$mq_type)
    typecount<-table(moonquake_type)
    barplot(typecount,main="Barplot For counting Moonquakes Types",xlab = "moonquake types",ylab = "Frequency",col = "skyblue")
  })
  output$Peichart<-renderPlot({
    moonquake_type<-c(data$mq_type)
    typecount<-table(moonquake_type)
    pie(typecount,main="Moonquakes Type Distribution",col = rainbow(length(typecount)))
  })
  output$histogram1<-renderPlot({
    hist(data$Max.Velocity,breaks = input$bins)
  })
  output$histogram2<-renderPlot({
    hist(data$Min.Velocity,breaks = input$bins)
  })
  output$summary<-renderPrint({
    summary(data)
  })
})
shinyApp(ui, server)
