# Check and install additional packages
packages.used <- c("shiny", "dplyr","leaflet","shinyBS","shinyjs","ggplot2","plotly","readr","fmsb","shinythemes")
packages.needed=setdiff(packages.used,intersect(installed.packages()[,1],packages.used))

if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}
options(warn = -1)


library(shiny)
library(shinyBS)
library(shinyjs)
library(shinythemes)
library(dplyr)
library(leaflet)
library(ggplot2)
library(fmsb)
library(readr)
library(plotly)



# Define UI for application that draws a histogram
ui <- fluidPage(
  # begin navbarPage
  theme = shinytheme("darkly"),
  includeCSS("navbar.css"),
  navbarPage(
    
    tags$strong("H1B Viz"),
    
    # map
    tabPanel(
      strong("Map"),
      div(
        class = "outer",
        includeCSS("map.css"),
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(NULL, id = "controls", class = "panel panel-default", fixed = TRUE,draggable = TRUE, left = 10, right = "auto",
                      top =140, bottom = "auto", width = 200, height = "auto", cursor = "move",
                      uiOutput("reset"),
                      fluidRow(
                        column(12, align = "center", offset = 0,
                               actionButton("reset_input","Reset"),
                               tags$style(type = "text/css", "#reset_input {width:100%}"))
                      )
        ),
        absolutePanel(NULL, id = "controls", class = "panel panel-default", fixed = TRUE,draggable = TRUE, left = "auto", right = 10,
                      top = 70, bottom = "auto", width = 200, height = "auto", cursor = "move",
                      uiOutput("uni"),
                      tags$style(type = "text/css", "#uni_num {font-size:18px; color: white; background-color: #343332; text-align: center}"),
                      htmlOutput("uni_num")
                      
        ),
        useShinyjs(),
        hidden(
          div(id = "conditionalPanel",
              #fluidRow(
              absolutePanel(id = "information", class = "panel panel-default", style="margin-top: 20px; margin-bottom: 0px;", fixed = TRUE,
                            draggable = FALSE, top =230, right=0, bottom = 0,
                            width = 400, height = "auto", cursor = "move",
                            # h2("Detailed Information"),
                            uiOutput("info1"),
                            plotlyOutput("bar", width = "350px")
                            
              )
              #)
          )
        )
      ) # end div
    ), # end map
    
    # comparison
    tabPanel(
      strong("Company"),
      fluidPage(
        sidebarLayout(
          absolutePanel(NULL, id = "controls", class = "panel panel-default", fixed = FALSE,draggable = FALSE, left = "auto", right = "5",
                        top = 130, bottom = "auto", width = 200, height = "auto", cursor = "move",
                        uiOutput("uni_reset", inline = TRUE),
                        fluidRow(
                          column(12, align = "center", offset = 0,
                                 actionButton("reset_input2", "Reset"),
                                 tags$style(type = "text/css", "#reset_input2 {width:100%}")
                          )
                        )
          ),
          mainPanel(
            width = 10,
            tabsetPanel(type = "tabs",
                        tabPanel(strong("Top 10 Company"),
                                 plotlyOutput("basicPlot")
                                )),
            tabsetPanel(type="tabs",
                        tabPanel(strong("H1B Application by Industry"),
                        plotlyOutput("basicPlot2"))
                        ,
            tabPanel(strong("H1B Application Percentage"),
             plotlyOutput("PiePlot"))
          ),
          position = "right"
        )
      )
    ) 
  )
))
 # end ui


