library(shiny)
library(ggplot2)
library(dplyr)
library(ggvis)
library(plotly)
library(shinydashboard)
library(quantmod)
library(scales)
asxList0 <-read.csv("http://www.asx.com.au/asx/research/ASXListedCompanies.csv", skip=2,stringsAsFactors=FALSE)

# Global variables can go here
# Immediately enter the browser when an error occurs
options(shiny.error = browser)
# Define the UI
ui <- navbarPage("ASX list",
          tabPanel("Monthly Returns",
             sidebarLayout(
               sidebarPanel(
                 selectInput('ASX', 'ASX code', choices=asxList0$ASX.code,selected="ANZ"),
                 helpText("This application is used to show the distribution
                          of historical monthly returns from the Australian Stock Exchange (ASX).
                          visit http://github.com/wil337/")
               ),
               mainPanel(
                 textOutput('returnMedian'),
                 plotlyOutput('timeSeries'),
                 plotOutput('mthlyReturns'),
                 tableOutput('table')
               )
             )
          )
      )



# Define the server code
server <- function(input, output) {
  mthlyRet <- reactive({
    data <- getSymbols(paste0(input$ASX,".AX"), src = "yahoo", 
                       from = as.Date("2010-01-01"),
                       to = as.Date("2016-01-01"),
                       auto.assign = FALSE)
    adjusted <- paste(input$ASX,".AX",".Adjusted",sep="")
    data <- data[, adjusted, drop=F]
    d<-data.frame(x=monthlyReturn(data))
    d$mth <-row.names(d)
    d2<- d %>% mutate(mthly.ret.pa = (1+monthly.returns)^12-1) %>%
      mutate(yr = substr(mth,1,4))
    d2
  })
  
  
  output$mthlyReturns <- renderPlot({
    .e <- environment()
    g<-ggplot(data=mthlyRet() ,aes(x=monthly.returns))+
      geom_density(kernel='gaussian')+
      geom_dotplot(aes(fill=as.factor(yr)),method='histodot',stackgroups=TRUE)+
      scale_fill_brewer(palette="Blues")+
      scale_y_continuous(NULL, breaks = NULL)
    g
  })
  output$timeSeries <- renderPlotly({
    .e <- environment()
    ts<-ggplot(mthlyRet(),aes(x=mth,y=monthly.returns))+
      geom_bar(stat='identity')+
      scale_y_continuous(labels=scales::percent)#+
    ts
    ggplotly(ts)
  })
  
  output$table <- renderTable({
    summary(mthlyRet())
  })
  output$returnMedian <- renderText({
    paste0("The median annualised (mthly) return is ",percent((lapply(mthlyRet(),median)[[1]]+1)^12-1))
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)

