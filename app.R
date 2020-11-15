library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(data.table)

# Section 0: Global

# Read data for primary table
primary_table <- read_csv("https://raw.githubusercontent.com/PWPiranha/US_Elections_Draft/main/Primary_Table_Draft_1.csv") %>% 
  select(state, EV, Biden_poll, Biden_vote, Poll_error) 
head(primary_table)

# Section 1: UI
ui <- dashboardPage(
  dashboardHeader(title = "US Election 2020", titleWidth = 250),
  dashboardSidebar(width = 250,
                   sidebarMenu(
                     menuItem("Interactive Table", icon = icon("table"))
                   )
                   
  ), # closes dashboardSidebar
  dashboardBody(
    
    fluidRow(
      box(
        title = "Select State as Democrat/Biden",
        selectizeInput(inputId = "dem_selection",
                       label   = "Select State as Democrat/Biden",
                       choices = primary_table$state,
                       multiple = TRUE,
                       selected = c("New York", "California"))
      ), # closes box (Dem selection)
      
      box(
        title = "Select State as Republican/Trump",
        selectizeInput(inputId = "rep_selection",
                       label   = "Select State as Republican/Trump",
                       choices = NULL,
                       multiple = TRUE
                       
        )
      ), # closes box (Rep selection)
      
      box(splitLayout(cellWidths = c("75%", "25%"),
                      DT::dataTableOutput("primary_table"),
                      plotOutput("EV_chart")
      ), width = 12 # closes splitlayout
      ) # closes box
      
    )
  )
)

# Section 2: Server
server <- function(input, output, session) {
  
  output$primary_table <- DT::renderDataTable({
    datatable(primary_table,
              options = list(
                autoWidth = FALSE, scrollX = TRUE))
  })
  
  observeEvent(input$dem_selection,{
    updateSelectizeInput(session,'rep_selection',
                         choices = primary_table %>% filter(!state %in% input$dem_selection) %>% pull(state)
    )
  })
  
  
  win <- 270L
  
  output$EV_chart <- renderPlot({
    EV_chart <- primary_table %>%
      select(state, EV) %>%
      mutate(outcome = case_when(state %in% input$dem_selection ~ "Dem",
                                 state %in% input$rep_selection ~ "Rep",
                                 TRUE                  ~ NA_character_)) %>%
      na.omit() %>%
      group_by(outcome) %>%
      summarize(EV_total = sum(EV), .groups="drop") %>%
      ggplot(aes(x=outcome, y=EV_total, fill = outcome)) +
      geom_col() +
      scale_fill_manual(values = c("Dem" = "blue", "Rep" = "red")) +
      labs(title = "Electoral Votes",
           x     = "Electoral Votes by Party",
           y     = "Electoral Votes") +
      geom_hline(yintercept = win, size=1) +
      theme_bw() +
      theme(legend.position = "none") +
      geom_text(aes(y=win, label = win, vjust = 0))
    EV_chart
    
  })
  
  
}

shinyApp(ui, server)