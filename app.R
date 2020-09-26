library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(reticulate)
library(cowplot)
reticulate::source_python("clean_raw_zoom.py")
#use_python("/usr/local/bin/python3")

change_answer_name <- function(st){ gsub("a(\\d+)", "Answer \\1", st) }

ui <- fluidPage(theme = shinytheme("cerulean"),
  
  # Application title
  titlePanel("zoompollviewR: View your Zoom poll results"),
  p("Written by", a("Stephanie J. Spielman, Ph.D.",href="https://spielmanlab.github.io"), "and released under GPL-3 License. Source code:", 
    a("https://github.com/sjspielman/zoompollviewR",href="https://github.com/sjspielman/zoompollviewR")),
  p("Work in progress! Please report bugs and/or request features on the", a("Github Issues Page.",href="https://github.com/sjspielman/zoompollviewR/issues")),
  
  
  sidebarLayout(
    sidebarPanel(width = 3,
           fileInput("zoom_results_csv", "Upload your Zoom Poll CSV file.",
                 accept = c(".csv") # only the zoom poll output, it's a .csv
           ),
           helpText("This file should be exactly what you exported from the Zoom website."),
           numericInput("num_questions", "How many questions are in your poll?", min = 1, max = 50, value = 1),
           actionButton("go", "Process Zoom Poll!")
           
    ),
    
    
    mainPanel(width = 9,
          tabsetPanel(
            tabPanel(id = "full", "Full results", 
                 
                 h3("Poll questions"),
                 tableOutput("questions"),
                 br(),br(),
                 h3("Poll answers"),
                 DT::dataTableOutput("full_answer_table")
            ),
            
            tabPanel(id = "student", "Single student view",
                br(),
                uiOutput("select_student"),
                h3("Student responses:"),
                DT::dataTableOutput("single_student_view"),
                br(),br(),
                h3("Poll questions:"),
                tableOutput("questions1")
            ),
            tabPanel(id = "question", "Single question view",
                br(),
                h3("Question tallies:"),
                DT::dataTableOutput("question_view"),
                br(),br(),
                h3("Poll questions:"),
                tableOutput("questions2")
            )
          )
    )
  )
  
  
)

server <- function(input, output) {

  zoom_csv <- reactive({
    req(input$go >= 1)
    input_file <- input$zoom_results_csv$datapath
    clean_zoom_string <- clean_raw_zoom_csv(input_file, as.integer(input$num_questions))
    
    return( tibble::as_tibble(read.csv(text = clean_zoom_string)) )
    
    
  })
    
  processed_df <- reactive({
    zoom_csv() %>%
      # can re-instate `email` column if requested, remove for now
      dplyr::select(name, starts_with("a")) %>%
      dplyr::rename_with(change_answer_name, starts_with("a")) %>%
      dplyr::mutate(name = stringr::str_replace(name, 
                            "(.+)\\# (.+)", 
                            "\\2 \\1")) %>%
      dplyr::arrange(name) %>%
      dplyr::rename("Student name" = name) -> answers_df
    
    
    answers_df %>% 
      dplyr::select(-`Student name`) %>% 
      tidyr::pivot_longer(everything(), 
                names_to = "name", 
                values_to = "answer") %>% 
      dplyr::group_by(name) %>% 
      dplyr::count(answer) %>% 
      dplyr::arrange(desc(n), .by_group=TRUE) %>% 
      dplyr::group_by(name) %>%
      dplyr::mutate(percent = round(n/sum(n),3) * 100,
                    percent = paste0(percent, "%")) -> tallied_answers_df
    
    zoom_csv() %>%
      dplyr::select(starts_with("q")) %>%
      dplyr::distinct() %>%
      tidyr::pivot_longer(everything(), 
                names_to = "name", 
                values_to = "question") %>%
      dplyr::select(question) %>%
      dplyr::mutate("Question number" = 1:n()) %>%
      dplyr::select(`Question number`, question) -> questions_df
    
    return(list("student_names" = sort(unique(answers_df$`Student name`)), 
          "answers"     = answers_df, 
          "tallied_answers" = tallied_answers_df,
          "questions"   = questions_df ))
  })
  
  
  # honestly i need to make this a module. this is embarrassing.
  output$questions <-  renderTable(  processed_df()$questions, colnames=FALSE  )
  output$questions1 <-  renderTable(  processed_df()$questions, colnames=FALSE  )
  output$questions2 <-  renderTable(  processed_df()$questions, colnames=FALSE  )
  output$full_answer_table <- DT::renderDataTable(  processed_df()$answers  )
  
  
  output$select_student <- renderUI({
    #req(!(is.null(zoom_csv())))
    selectInput("selected_student", 
          "Select student to view:",
          processed_df()$student_names, 
          selected=processed_df()$student_names[1])
  })
  
  output$single_student_view <- DT::renderDataTable({
    
    processed_df()$answers %>%
      dplyr::filter(`Student name` == input$selected_student) %>%
      tidyr::pivot_longer(-c(`Student name`),
                names_to = "q", 
                values_to = "Answer") %>%
      dplyr::mutate(q = stringr::str_replace(q, "Answer ", "")) %>%
      dplyr::rename("Question number" = q) 
  })
  
  
  output$question_view <- DT::renderDataTable(rownames = FALSE, {
    processed_df()$tallied_answers %>%
        dplyr::rename("Number of answers"= n,
                      "Percent of answers" = percent,
                      "Question" = name, 
                      "Answer" = answer) %>%
        dplyr::mutate(Question = stringr::str_replace(Question, "Answer ", "")) 
  })  
  
  # output$question_view_plot <- renderPlot({
  #     
  #     all_questions <- list()
  #     for (i in 1:nrow(processed_df()$questions)){
  #         processed_df()$tallied_answers %>%
  #             dplyr::filter(name == paste0("Answer ", i)) %>%
  #             ggplot(aes(x = answer, y = n)) + 
  #             geom_col(fill = "navy") + 
  #             theme_light() + 
  #             theme(axis.text.x = element_text(angle = 90)) +
  #             labs(y = "Number of answers", 
  #                  title = paste0("Question ", i)) -> p
  #         all_questions[[i]] <- p
  #     }
  #   plot_grid(plotlist = all_questions, nrow = 5)
  # })
}




# Run the application 
shinyApp(ui = ui, server = server)
