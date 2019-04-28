# Define server
finaldata=read.csv("./data/finaldata.csv")
finaldata=finaldata[c(3,7,9,10,12)]
data1=read.csv("./data/data1.csv")
data2=read.csv("./data/data2.csv")

server <- function(input, output){
  
  # basic map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "https://api.mapbox.com/styles/v1/zhengfei0908/cjsmaov8e0rvq1gqgl10ytul5/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiemhlbmdmZWkwOTA4IiwiYSI6ImNqc204YTNpMzF6bG00M3A2NGRmYmx6ZXcifQ.SyAyO42-9ko6-NxGFdpdTQ",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      addCircles(lng = data1$Long, lat = data1$Lat, popup = data1$Zipcode,radius = 5, color = "#FFFF00") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  # data filter
  zip.select <-  eventReactive(c(input$State,input$Application), {
    zip.s <- data1 %>% filter(
      Application >= input$Application[1] 
      & Application <= input$Application[2]
    )
    
    if (!is.null(input$State)){
      zip.s <- data1 %>% 
        filter(State %in% input$State
               &Application >= input$Application[1] 
               & Application <= input$Application[2])
    }
    zip.s
  },
  ignoreInit = TRUE)
  
  output$uni_num <- renderText({
    paste("<b>Search Result: ", nrow(zip.select()), "</b>")
  })
  
  
  # change the filter conditions
  observeEvent(c(input$State, input$Application), {
    tmp <- zip.select()
    if (nrow(tmp) != 0){
      leafletProxy("map") %>%
        clearShapes() %>%
        addCircles(lng = tmp$Long, lat = tmp$Lat, 
                   popup = tmp$Zipcode, radius = 5, color = "#FFFF00")
    }
  })
  
  output$reset <- renderUI({
    count1 <- input$reset_input
    bsCollapse(id=NULL, multiple = TRUE,
               bsCollapsePanel("Application",
                               sliderInput("Application", NULL, min = 0, max = 30000, value = c(0,30000))),
               bsCollapsePanel("State",
                               selectInput("State", NULL, choices = c(Choice = "", unique(as.character(data1$State))),
                                           selectize = TRUE, multiple = TRUE))
    
  )})
  
  # information, adaptable by filtering conditions
  output$uni <- renderUI({
    count2 <- input$reset_input
    bsCollapse(id="uni_choice2",
               bsCollapsePanel("Zipcode",
                               selectInput("Zipcode", NULL,
                                           choice = c(Choice = "", zip.select()$Zipcode), selected = character(0))
               )
    )
    
  })
  
  observeEvent(input$Zipcode,{
    if (is.null(input$Zipcode)){
      return()
    }
    else if(input$Zipcode == ""){
      return()
    }
    
    ZIP <- zip.select() %>% filter(Zipcode == input$Zipcode)
    
    # add barplot
    output$bar <- renderPlotly({
      
      data <- c(ZIP$Application,ZIP$Approval)
      
      flag <- c("Total Applications","Total Approvals")
      plot_ly(
        type = "bar",
        x=flag,
        y=data
      ) %>%
        layout(
          font = list(
            family = 'Arial',
            size = 13,
            color = '#ffffff'
          ),
          showlegend = F,
          # bg color
          paper_bgcolor = "#303030",
          plot_bgcolor='rgba(0,0,0,0)'
        )
    })
    
    output$info1 <- renderUI({
      head <- paste("<h3 style='font-family:Palatino' align=center>",
                    as.character(ZIP$Zipcode), "</h3>")
      application <- paste("<p style='font-size:15px' align=center>",
                    as.character(ZIP$Application), "</p>")
      # college
      approval<- paste("<p style='font-size:15px' align=center>",
                    as.character(ZIP$Approval), "</p>")
      
      state <- paste("<p style='font-size:15px' align=center>",
                      as.character(ZIP$State), "</p>")
      
      city <- paste("<p style='font-size:15px' align=center>",
                      as.character(ZIP$City), "</p>")
      
      table <- paste(
        '<table id = "customers">', "<tr>", 
        "<th>", "<strong>  Applcation </strong>", "</th>",
        "<th>", "<strong>  Approval </strong>", "</th>",
        "</tr>",
        "<tr>",
        "<td>", application,"</td>",
        "<td>", approval, "</td>",
        "</tr>",
        "<tr>",
        "<th>", "<strong>  State </strong>", "</th>",
        "<th>", "<strong>  City </strong>", "</th>",
        "</tr>",
        "<tr>",
        "<td>", state, "</td>",
        "<td>", city, "</td>",
        "</tr>",
        "</table>"
      )
      
      HTML(paste(head, table))
    })
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(ZIP$Long, ZIP$Lat, popup = 
                   paste0(
                     "<br/>",
                     paste(
                       as.character(ZIP$City), 
                       as.character(ZIP$State)
                     ),
                     '<br/>',
                     as.character(ZIP$Zipcode)
                   ),
                 options = popupOptions(closeButton = TRUE)
      ) %>% 
      clearPopups() %>%
      addPopups(
        ZIP$Long, ZIP$Lat, 
        popup = paste0(
          "<br/>",
          paste(
            as.character(ZIP$City), 
            as.character(ZIP$State)
          ),
          '<br/>',
          as.character(ZIP$Zipcode)
        ),
        options = popupOptions(closeButton = TRUE)
      ) %>%
      flyTo(ZIP$Long, ZIP$Lat, zoom = 8)
    
    shinyjs::show(id = "conditionalPanel")
  })
  
  observeEvent(input$map_marker_click, {
    
    shinyjs::show(id = "conditionalPanel")
  })
  
  observeEvent(input$map_click, {
    
    shinyjs::hide(id = "conditionalPanel")
  })
  
  observeEvent(input$reset_input, {
    shinyjs::hide(id = "conditionalPanel")
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearPopups() %>%
      addCircles(lng = data1$Long, lat = data1$Lat, 
                 popup = data1$Zipcode, radius = 5, color = "#FFFF00") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  
  ### top 15 company
  output$uni_choice <- renderUI({
    bsCollapse(id = NULL, open = "Industry",
               bsCollapsePanel("Industry",
                              
                               div(
                                
                                 selectInput("uni_choice", label = NULL, 
                                                    choices = unique(finaldata$Industry.Title), 
                                                    selected = unique(finaldata$Industry.Title)[9])
                               )
               ))
    
  })
  
  
  zip.select.comp <- eventReactive(input$uni_choice, {
    finaldata %>%
      filter(Industry.Title %in% input$uni_choice)
  })
  
  output$uni_reset <- renderUI({
    uiOutput("uni_choice")
  })
  
  observeEvent(input$reset_input2,{
    output$uni_reset <- renderUI({
      uiOutput("uni_choice")
    })
  })
  
  title.font <- list(
    family = "Helvetica",
    size = 18,
    color = "F8F86E"
  )
  
  y.title.font <- list(
    family = "Helvetica",
    size = 12,
    color = "FFFFFF"
  )
  
  output$basicPlot <- renderPlotly({
      tmp <- zip.select.comp()%>%select(Employer,Total.Applicants)%>%arrange(desc(Total.Applicants))%>%head(15)
      tmp$Employer <- factor(tmp$Employer, levels = tmp$Employer[order(tmp$Total.Applicants, decreasing = F)])
      plot_ly(tmp,
              y = ~Employer, x = ~Total.Applicants, type = "scatter", marker = list(color = "#D54B84"),mode = "markers")%>%
        layout(title = "Top 15 Companies by H1B applications",
               titlefont = title.font,
               xaxis = list(title = "H1B Applications",tickfont = y.title.font),
               yaxis = list(title = "",
                            tickfont = y.title.font),
               paper_bgcolor="#222222",
               plot_bgcolor="#222222",
               margin = list(l = 50))
    })
  output$basicPlot2 <- renderPlotly({
    tmp <- data2%>%arrange(desc(Application))
    tmp$Industry.Title <- factor(tmp$Industry.Title, levels = tmp$Industry.Title[order(tmp$Application, decreasing = TRUE)])
    plot_ly(tmp,
            x = ~Industry.Title, y = ~Application, type = "bar", color = I("#D54B84"))%>%
      layout(title = "H1B applications by Industry",
             titlefont = title.font,
             xaxis = list(title = "",
                          showticklabels = F,tickfont = y.title.font),
             yaxis = list(title = "",
                          tickfont = y.title.font),
             paper_bgcolor="#222222",
             plot_bgcolor="#222222",
             margin = list(t = 50))
  })
  output$PiePlot <- renderPlotly({
    tmp <- data2%>%arrange(desc(Application))
    tmp$Industry.Title <- factor(tmp$Industry.Title, levels = tmp$Industry.Title[order(tmp$Application, decreasing = TRUE)])
    plot_ly(tmp,
            labels = ~Industry.Title, values = ~Application, type = "pie",hole=0.6)%>%
      layout(title = "H1B applications percentage by Industry",
             titlefont = title.font,
             showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE,
                          showticklabels = F,tickfont = y.title.font),
             yaxis = list(showgrid = FALSE, zeroline = FALSE,
                          tickfont = y.title.font),
             paper_bgcolor="#222222",
             plot_bgcolor="#222222",
             margin = list(t = 50))
  })
} 
