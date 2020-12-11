#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(openxlsx)
library(stringr)
library(shinydashboardPlus)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library("reshape2")

df<-read.xlsx("/Users/cherynaouaj/Desktop/bst260_final_project/Shiny/temperature_and_coral_cover.xlsx") 
climate<-read.csv("/Users/cherynaouaj/Desktop/bst260_final_project/Shiny/global.csv")
climate<-climate[, c("Year", "Average")]

df$date<-convertToDate(df$date)

colnames(df)[2]<-"Water"
colnames(df)[3]<-"Coral"

df_long <- melt(df, id="date")


ui <- dashboardPage(
    dashboardHeader(title = "Moore Reef Temperature and Hard Coral Cover Relationship Dashboard"),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            box(
                column(12,align="center",plotOutput("plot1", height = 350)),
                column(12,align="center",sliderInput("sliderA","",
                                                     min=min(as.Date(df$date)), max=max(as.Date(df$date)), step=30, value=as.Date("1997-11-21"),
                                                     animate = animationOptions(interval = 300, loop = TRUE)))),
            box(plotOutput("plot2"))
            
        )
    )
)


server <- function(input, output) {

    df_2<- reactive({
        df_long %>% dplyr::filter(date >= min(date) & date <= input$sliderA) })
    
    output$plot1 <- renderPlot({
        
        ggplot(df_2(),aes(x=date, y=value, colour=variable)) +
            geom_line(size=1.5)+
            scale_color_manual(values=c("#a0cf8d", "#89cff0"))+
            ylim(14,31) +
            ggtitle("Average Water Temperature and Mean Live Coral Cover Percentage")+
            theme(panel.background = element_blank(),
                  plot.title = element_text(size=12, face="bold",hjust = 0.5),
                  legend.position="top",
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  axis.text.x = element_text(face = "bold",size = 11 ,angle = 45),
                  axis.text.y = element_text(face = "bold",size = 12),
                  legend.background = element_rect(fill = "white"),legend.key = element_rect(fill = "white"),
                  legend.title = element_blank())
        
    })
    
    output$plot2 <- renderPlot({
        
        ggplot(climate,aes(x=Year, y=Average, group=1)) +
            ggtitle("Global Temperature Yearly Increase")+
            geom_line(size=1,color="#2565AE")+
            theme(panel.background = element_blank(),
                  plot.title = element_text(size=14, face="bold",hjust = 0.5),
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  axis.text.x = element_text(face = "bold",size = 11 ,angle = 45),
                  axis.text.y = element_text(face = "bold",size = 12),
                  legend.background = element_rect(fill = "white"),legend.key = element_rect(fill = "white"),
                  legend.title = element_blank())
        
    })
    
}

# Run the application 
shinyApp(ui, server, options = list(height = 1080))

