# Setup ####
library(shiny)
library(shinyjs)
library(shinyFiles)
library(DT)
library(howler)

# UI ####
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
      uiOutput("audio"),  # This will now be populated with howler player
      howlerPlayPauseButton("audio"),  # Add play/pause button
      hr(),
      actionButton("score1", "Klauwkikker", style = "background-color: #ff4444; color: white;"),
      actionButton("score2", "Achtergrond", style = "background-color: #44ff44; color: black;"),
      actionButton("score3", "Onzeker", style = "background-color: #888888; color: white;")
    ),
    mainPanel(
      DTOutput("results_table"),
      hr(),
      textOutput("selected_file_path")  # Display full file path at the bottom
    )
  )
)

# Server ####
server <- function(input, output, session) {
  volumes <- c(Home = normalizePath("~/Github/XenopusR/Output/"))
  shinyDirChoose(input, "folder", roots = volumes, session = session)
  
  rv <- reactiveValues(
    df = NULL,
    selected_row = NULL,
    folder_path = NULL,
    current_audio = NULL  # To store the current audio file path
  )
  
  
  # Display selected folder ####
  output$selected_folder <- renderText({
    req(input$folder)
    rv$folder_path <- parseDirPath(volumes, input$folder)
    rv$folder_path
  })
  
  # Run button handler ####
  observeEvent(input$run, {
    req(rv$folder_path)
    categories <- c("likely", "unlikely", "highly_unlikely")
    all_files <- list()
    
    for(cat in categories) {
      cat_path <- file.path(rv$folder_path, cat)
      cat_files <- list.files(cat_path, pattern = "\\.wav$", full.names = TRUE)
      
      if(length(cat_files) < input$sample_num) {
        showModal(modalDialog(
          title = "Warning",
          paste("Less than", input$sample_num, "files found in", cat, "! Continue?"),
          footer = tagList(
            modalButton("No"),
            actionButton("yes", "Yes")
          )
        ))
        return()
      }
      
      all_files[[cat]] <- sample(cat_files, size = input$sample_num)
    }
    
    files <- unlist(all_files)
    rv$df <- data.frame(
      file_name = basename(files),
      full_path = files,
      model_result = rep(categories, each = input$sample_num),
      final_result = "",
      stringsAsFactors = FALSE
    )
    
    write.csv(rv$df, file.path(rv$folder_path, "steekproef_results.csv"), row.names = FALSE)
  })
  
  # Confirm proceed with insufficient files ####
  observeEvent(input$yes, {
    removeModal()
    categories <- c("likely", "unlikely", "highly_unlikely")
    all_files <- list()
    
    for(cat in categories) {
      cat_path <- file.path(rv$folder_path, cat)
      cat_files <- list.files(cat_path, pattern = "\\.wav$", full.names = TRUE)
      all_files[[cat]] <- cat_files
    }
    
    files <- unlist(all_files)
    rv$df <- data.frame(
      file_name = basename(files),
      full_path = files,
      model_result = rep(categories, sapply(all_files, length)),
      final_result = "",
      stringsAsFactors = FALSE
    )
    
    write.csv(rv$df, file.path(rv$folder_path, "steekproef_results.csv"), row.names = FALSE)
  })
  
  # Results table ####
  output$results_table <- renderDT({
    req(rv$df)
    datatable(
      rv$df[, c("file_name", "model_result", "final_result")],
      selection = 'single',
      options = list(
        columnDefs = list(list(
          targets = 0,
          render = JS("
          function(data, type, row, meta) {
            return '<span title=\"' + row[1] + '\">' + data + '</span>';
          }
        ")
        ))
      )
    )
  })
  
  # Update selected row ####
  rv$audio_update <- reactiveVal(0)
  observeEvent(input$results_table_rows_selected, {
    rv$selected_row <- input$results_table_rows_selected
    if (!is.null(rv$selected_row) && !is.null(rv$df)) {
      rv$current_audio <- rv$df$full_path[rv$selected_row]
      print(paste("Selected audio file:", rv$current_audio))
      
      # check file usability
      if (file.exists(rv$current_audio)) {
        print(paste("File exists:", rv$current_audio))
        if (file.access(rv$current_audio, mode = 4) == 0) {
          print("File is readable")
        } else {
          print("File is not readable")
        }
      } else {
        print(paste("File does not exist:", rv$current_audio))
      }
    }
    changeTrack("audio", rv$current_audio)
    rv$audio_update(rv$audio_update() + 1)
  })
  
  observeEvent(rv$current_audio, {
    if (!is.null(rv$current_audio)) {
      changeTrack("audio", rv$current_audio)
    }
  })
  
  output$selected_file_path <- renderText({
    req(rv$selected_row, rv$df)
    paste("Selected File Path:", rv$df$full_path[rv$selected_row])
  })
  
  # Audio player ####
  output$audio_player <- renderUI({
    req(rv$current_audio, rv$audio_update())
    howler(
      elementId = "audio",
      tracks = list(rv$current_audio),
      seek_ping_rate = 1000
    )
  })
  
  # Score buttons handler ####
  observeEvent(list(input$score1, input$score2, input$score3), {
    req(rv$selected_row)
    score <- c("Klauwkikker", "Achtergrond", "Onzeker")[which(c(input$score1, input$score2, input$score3) > 0)]
    rv$df$final_result[rv$selected_row] <- score
    write.csv(rv$df, file.path(rv$folder_path, "steekproef_results.csv"), row.names = FALSE)
    # Refresh the table
    dataTableProxy("results_table") %>% 
      replaceData(rv$df[, c("file_name", "model_result", "final_result")])
  })
  
  # Save on exit ####
  session$onSessionEnded(function() {
    if(!is.null(rv$df) && !is.null(rv$folder_path)) {
      write.csv(rv$df, file.path(rv$folder_path, "steekproef_results.csv"), row.names = FALSE)
    }
  })
}

shinyApp(ui, server)
