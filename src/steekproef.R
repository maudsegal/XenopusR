library(shiny)
library(shinyjs)
library(shinyFiles)
library(tuneR)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Audio Sample Validation Tool"),
  sidebarLayout(
    sidebarPanel(
      shinyDirButton("folder", "Select Output Folder", "Select Folder"),
      verbatimTextOutput("selected_folder"),
      numericInput("sample_num", "Sample Number:", value = 50, min = 1),
      actionButton("run", "Run", class = "btn-primary"),
      hr(),
      hidden(
        div(id = "player_panel",
            uiOutput("audio_ui"),
            actionButton("play", "Play", icon = icon("play")),
            hr(),
            actionButton("score1", "Klauwkikker", style = "background-color: #ff4444; color: white;"),
            actionButton("score2", "Achtergrond", style = "background-color: #44ff44; color: black;"),
            actionButton("score3", "Onzeker", style = "background-color: #888888; color: white;"),
            hr(),
            actionButton("next_btn", "Next File", class = "btn-warning")
        )
      )
    ),
    mainPanel(
      dataTableOutput("progress_table")
    )
  )
)

server <- function(input, output, session) {
  volumes <- c(Home = normalizePath("~"))
  shinyDirChoose(input, "folder", roots = volumes, session = session)
  
  rv <- reactiveValues(
    files = NULL,
    current_index = 1,
    df = data.frame(file_name = character(),
                    model_result = character(),
                    final_result = character(),
                    stringsAsFactors = FALSE),
    selected_score = NULL,
    active_folder = NULL
  )
  
  # Display selected folder ####
  output$selected_folder <- renderText({
    req(input$folder)
    parseDirPath(volumes, input$folder)
  })
  
  # Run button handler ####
  observeEvent(input$run, {
    req(input$folder)
    folder_path <- parseDirPath(volumes, input$folder)
    
    categories <- c("likely", "unlikely", "highly_unlikely")
    all_files <- list()
    
    # Collect files with verification
    for(cat in categories) {
      cat_path <- file.path(folder_path, cat)
      if(!dir.exists(cat_path)) {
        showNotification(paste("Missing directory:", cat_path), type = "error")
        return()
      }
      
      cat_files <- list.files(cat_path, pattern = "\\.wav$", full.names = TRUE)
      
      if(length(cat_files) < input$sample_num) {
        showModal(modalDialog(
          title = "Warning",
          paste("Only", length(cat_files), "files found in", cat, "!"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm", "Continue Anyway")
          )
        ))
        return()
      }
      all_files[[cat]] <- sample(cat_files, size = input$sample_num)
    }
    
    removeModal()
    rv$files <- unlist(all_files)
    rv$current_index <- 1
    rv$df <- rv$df[0,]
    rv$active_folder <- folder_path
    shinyjs::show("player_panel")
    disable("run")
  })
  
  # Confirm proceed with insufficient files
  observeEvent(input$confirm, {
    folder_path <- file.path("./output", input$folder)
    categories <- c("likely", "unlikely", "highly_unlikely")
    all_files <- list()
    
    for(cat in categories) {
      cat_path <- file.path(folder_path, cat)
      cat_files <- list.files(cat_path, pattern = "\\.wav$", full.names = TRUE)
      all_files[[cat]] <- if(length(cat_files) > 0) cat_files else character(0)
    }
    
    rv$files <- unlist(all_files)
    rv$current_index <- 1
    rv$df <- rv$df[0,]
    rv$active_folder <- folder_path
    shinyjs::show("player_panel")
    disable("run")
    removeModal()
  })
  
  # Audio player
  output$audio_ui <- renderUI({
    req(rv$files, rv$current_index <= length(rv$files))
    tags$audio(id = "audio_player", controls = TRUE,
               tags$source(src = rv$files[rv$current_index], type = "audio/wav"))
  })
  
  # Play button handler
  observeEvent(input$play, {
    runjs("document.getElementById('audio_player').play();")
  })
  
  # Score buttons handler
  observeEvent(list(input$score1, input$score2, input$score3), {
    rv$selected_score <- c("Klauwkikker", "Achtergrond", "Onzeker")[which(c(input$score1, input$score2, input$score3) > 0)]
  })
  
  # Next button handler
  observeEvent(input$next_btn, {
    req(rv$selected_score, rv$current_index <= length(rv$files))
    
    current_file <- rv$files[rv$current_index]
    model_result <- basename(dirname(current_file))
    
    new_row <- data.frame(
      file_name = basename(current_file),
      model_result = model_result,
      final_result = rv$selected_score,
      stringsAsFactors = FALSE
    )
    
    rv$df <- rbind(rv$df, new_row)
    write.csv(rv$df, file.path(rv$active_folder, "steekproef_results.csv"), row.names = FALSE)
    
    if(rv$current_index < length(rv$files)) {
      rv$current_index <- rv$current_index + 1
      rv$selected_score <- NULL
    } else {
      showNotification("All files processed!", type = "message")
      shinyjs::hide("player_panel")
      enable("run")
    }
  })
  
  # Save on exit - modified version
  session$onSessionEnded(function() {
    isolate({
      if(!is.null(rv$selected_score) && !is.null(rv$active_folder) && rv$current_index <= length(rv$files)) {
        current_file <- rv$files[rv$current_index]
        model_result <- basename(dirname(current_file))
        
        new_row <- data.frame(
          file_name = basename(current_file),
          model_result = model_result,
          final_result = rv$selected_score,
          stringsAsFactors = FALSE
        )
        
        updated_df <- rbind(rv$df, new_row)
        write.csv(updated_df, file.path(rv$active_folder, "steekproef_results.csv"), row.names = FALSE)
      }
    })
  })
  
  # Progress table
  output$progress_table <- renderDataTable({
    rv$df
  }, options = list(pageLength = 10))
}

shinyApp(ui, server)
