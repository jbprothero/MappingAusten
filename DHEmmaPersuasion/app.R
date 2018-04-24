
library(shiny)
library(ggmap)
library(mapproj)

lang = read.csv("data/Language.csv", header=T)
set = read.csv("data/Settlement.csv", header=T)
langlatlon = read.csv("data/langlatlon.csv", header=T)
setlatlon = read.csv("data/setlatlon.csv", header=T)
map = get_map(location="Europe", zoom=3)

ui <- fluidPage(
   
   # Application title
   titlePanel(div(HTML("Location Name Origins in Austen's <em>Emma</em> & <em>Persuasion</em>"))),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
     sidebarPanel(
     
     selectInput("selectInfo", label = h3("Origin Information"), 
                 choices = c("Etymology", "Settlement History"), 
                 selected = "Etymology"),
     
     selectInput("novelGroup", label = h3("Novels to Include"), 
                        choices = c("Emma", "Persuasion", "Both"),
                        selected = "Emma")
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("distPlot"),
       tableOutput("freq")
     )
      
   )
)

server <- function(input, output) {
  
  
  coords <- reactive({switch(input$selectInfo, 
                  "Etymology" = langlatlon, 
                  "Settlement History" = setlatlon)})
  typestr <- reactive({switch(input$selectInfo, 
                   "Etymology" = 'Location Etymology', 
                   "Settlement History" = 'Settlement History')})
  
  colr <- reactive({switch(input$novelGroup,
                "Emma" = 'red',
                "Persuasion" = 'blue',
                "Both" = 'purple')})
  novstr <- reactive({switch(input$novelGroup,
                  "Emma" = 'Emma',
                  "Persuasion" = 'Persuasion',
                  "Both" = 'Emma & Persuasion')})
  
  
  output$freq <- renderTable({
    
    cts
    
  })
  
  output$distPlot <- renderPlot({
    
    dat <- switch(input$selectInfo, 
                            "Etymology" = lang, 
                            "Settlement History" = set)
    
    rows <- switch(input$novelGroup,
                             "Both" = rep(1,length(dat[,1])), 
                             "Emma" =  dat$Emma,
                             "Persuasion" = dat$Persuasion)
    
    cts <- apply(dat[rows,5:length(dat[1,])], 2, sum)
    
    ggmap(map) + 
      scale_x_continuous(limits = c(-15, 55), expand = c(0,0))+
      scale_y_continuous(limits = c(30, 65), expand = c(0,0))+
      scale_size_continuous(range = c(min(cts)*2, max(cts)), breaks=unique(sort(cts)), name="Frequency")+
      geom_point(aes(x=lon, y=lat, size=cts), color=colr, data=coords, alpha=0.7)+
      ggtitle(paste(typestr, "in", novstr))
    
  })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

