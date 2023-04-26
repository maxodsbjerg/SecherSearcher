#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

secher <- read.csv("data/secher_words.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Secher Søgemaskinen"),
    fluidRow(column(width = 7, 
                    p("Secher Søgemaskinen arbejder med lovteksterne fra Sechers Forordninger og er udtrukket fra pdf. Logikken er her at kommentarerne og fodnoter til lovteksterne står i kursiv, mens den faktisk lovtekst står med almindelige skrift og i størrelse 12.5. Det data som Secher Søgemaskinen arbejder på er altså ikke-kursiveret tekst i størrelse 12.5. Data er på et enkelt ords niveau. Derfor at det ikke muligt at lave frasesøgninger f.eks. 'frycht for gud'  "),
                    p("Want to examine the underlying dataprocessing steps of this app? See the source code on Github:"),
                  a("https://github.com/maxodsbjerg/SecherSearcher", href="https://github.com/maxodsbjerg/SecherSearcher")
                    ), 
             column(width = 4, offset = 1, img(src= "secher_page.png", alt = "Et eksempel på en side fra Sechers Forordninger", width = "100%"))
),

hr(),
    # Sidebar with a text input
    h2("Søg i alle bindene for antal forekomster af ord"),
    p("Herunder kan du søge på ord i alle bindene af Sechers forordninger. Læg mærke til at søgningen er   meget bred, derfor finder den også 'rugudførsel', fordi der står 'gud' inde i ordet."),
    p("Vil du søge på flere ord på samme tid, så skal de være adskilt af '|', feks 'gudfrycht|gudsfrycht|gudfrøcht'"),
    sidebarLayout(
        sidebarPanel(
          textInput("text", 
                    p("Søgning:"), 
                    value = "gud", 
                    placeholder = "Skriv et ord og tryk på søg"),
          actionButton("goButtonAll", "Søg!", class = "btn-success"),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Plot")
        )
    ),
hr(),
    h2("Search word occurences dispersed on volumes"),
p("Herunder kan du søge på ord i og få deres forekomster fordelt ud på bindene af Sechers forordninger. Læg mærke til at søgningen er   meget bred, derfor finder den også 'rugudførsel', fordi der står 'gud' inde i ordet."),
p("Vil du søge på flere ord på samme tid, så skal de være adskilt af '|', feks 'gudfrycht|gudsfrycht|gudfrøcht'"),
    sidebarLayout(
      sidebarPanel(
        textInput("volumes",
                  p("Query:"),
                  value = "gud"),
        actionButton("goButtonVolumes", "Search!", class = "btn-success"),
          
        ),
        mainPanel(
          plotOutput("volumesPlot")
        )
      )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$Plot <- renderPlot({
    input$goButtonAll
    
    word <- isolate(input$text)
  secher %>% 
    filter(font_size == 12.50) %>% 
    filter(font_name == "Times-Roman") %>%  
    mutate(text = str_to_lower(text)) %>% 
    mutate(text = str_remove_all(text, "\U00ad*")) %>% 
    mutate(text = str_remove_all(text, "[[:punct:] ]+")) %>% 
    filter(str_detect(text, str_to_lower(word))) %>% 
    count(text) %>% 
    mutate(text = reorder(text, n)) %>% 
    ggplot(aes(x = text, y = n))+
    geom_col(fill = "#0B775E") +
    coord_flip() +
    labs(title = paste0('Count of words containing the word ', word), 
          subtitle = "in all volumes of Sechers Forordninger",
          x = "word")
  
  })
  
  output$volumesPlot <- renderPlot({
    
    input$goButtonVolumes
    
    word_V <- isolate(input$volumes)
    
    secher %>% 
      filter(font_size == 12.50) %>% 
      filter(font_name == "Times-Roman") %>%  
      mutate(text = str_to_lower(text)) %>% 
      mutate(text = str_remove_all(text, "\U00ad*")) %>% 
      mutate(text = str_remove_all(text, "[[:punct:] ]+")) %>% 
      filter(str_detect(text, str_to_lower(word_V))) %>% 
      count(aarrække, text) %>% 
      mutate(text = reorder(text, n)) %>% 
      ggplot(aes(x = text, y = n))+
      geom_col(fill = "#0B775E") +
      facet_wrap(~aarrække, scales = "free_y") +
      coord_flip() +
      labs(title = paste0('Count of words containing', word_V), 
           subtitle = "dispersed on volumes of Sechers Forordninger",
           x = "word")
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
