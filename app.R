
# required packages
library(shiny)
library(tidyverse)
library(sentimentr)
library(readr)
library(DT)
library(ggplot2)

### TO DO

#### maak grafieken en sentiment analysis ------------------

#### maak tab voor wordcloud
#### vergeet niet om de bronnen te vermelden
#### maak selectInput voor methode sentiment analyse


df_comm <- read_csv("df_comments.csv")
df_comm <- df_comm %>%
  select(textOriginal, model, model_year, video_by) %>%
  mutate(model = as.factor(model))



# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Tabsets"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
     
      # radioButtons("dist", "Distribution type:",
      #              c("Normal" = "norm",
      #                "Uniform" = "unif",
      #                "Log-normal" = "lnorm",
      #                "Exponential" = "exp")),
      
      
      selectInput(inputId = "modelInput", 
                  label = "Kies een model", 
                  choices = sort(df_comm$model),
                  selected = 1,
                  multiple = FALSE),
      
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      textInput("keyword", label = "Filter comments op", value = NULL, placeholder = "audi, brake, exhaust, ..."),
      
      br(),
      
      # Input: Slider for the number of observations to generate ----
      sliderInput("n",
                  "Number of observations:",
                  value = 500,
                  min = 1,
                  max = 1000)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("emotions")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table of comments", dataTableOutput("table"))
                           
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output, session) {
  # df1 <- reactive({
  #   emo <- df_comm %>%
  #     filter(model == input$modelInput)
  #   
  #   emo <- str_subset(emo$textOriginal, pattern = c(input$keyword))
  #   emo
  # })
  
  # df <- reactive({
  #   
  #   # emo <- df_comm %>%
  #   #   filter(model == input$modelInput)
  #   # 
  #   # emo <- str_subset(emo$textOriginal, pattern = c(input$keyword))
  #   
  #   # sentences <- get_sentences(emo)
  #   
  #   sentences <- get_sentences(df1())
  #   
  #   emotions <- emotion(sentences)
  #   
  #   emotions
  #   
  # })
  
  # render DT
  output$table <- DT::renderDataTable({
    
    emo <- df_comm %>%
      filter(model == input$modelInput) %>%
      filter(textOriginal == str_subset(textOriginal, pattern = c(input$keyword)))
    emo
      
    #emo <- str_subset(emo$textOriginal, pattern = c(input$keyword))
    
    #df1
    
    # df_comm %>%
    #   filter(model == input$modelInput)
  })
  
 output$emotions <- renderPlot({
   # emo <- df_comm %>%
   #   filter(model == input$modelInput)
   # emo <- str_subset(emo$textOriginal, pattern = c(input$keyword))
   # 
   # sentences <- get_sentences(emo)
   # 
   # emotions <- emotion(sentences)
   
   
   
   emo <- df_comm %>%
     filter(model == input$modelInput) %>%
     filter(textOriginal == str_subset(textOriginal, pattern = c(input$keyword)))
   
   sentences <- get_sentences(emo)
     
   emotions <- emotion(sentences)
   

   #df() %>%
   emotions %>%
     ggplot(aes(reorder(emotion_type, desc(emotion_count)), emotion_count)) + 
     geom_col() +
     coord_flip()

  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    
    
    emo <- df_comm %>%
      filter(model == input$modelInput) %>%
      filter(textOriginal == str_subset(textOriginal, pattern = c(input$keyword)))
    
    sentences <- get_sentences(emo)
    sentiment(sentences)
    
     # df_comm_sentiment <- df_comm %>%
     #   filter(model == input$modelInput)
     # sentiment(df_comm_sentiment$textOriginal)
    
  })
  
  # # Generate an HTML table view of the data 
  # output$table <- renderTable({
  #   df_comm()
  # })
  

  
}

# Create Shiny app 
shinyApp(ui, server)