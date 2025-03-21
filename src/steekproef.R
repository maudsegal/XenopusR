# Setup ####
library(shiny)
library(shinyjs)
library(shinyFiles)
library(DT)
library(base64enc)

# UI ####
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .scrolling-sidebar {
        position: fixed;
        top: 1;
        bottom: 1;
        left: 0;
        overflow-y: auto;
        width: 25%; /* Adjust as needed */
        padding: 25px;
      }
    "))
  ),
  tags$script("
    Shiny.addCustomMessageHandler('jsCode', function(message) {
      eval(message.code);
    });
  "),
  useShinyjs(),
  titlePanel("Audio Sample Validation Tool"),
  sidebarLayout(
    sidebarPanel(
      class = "scrolling-sidebar",
      shinyDirButton("folder", "Select Output Folder", "Select Folder"),
      verbatimTextOutput("selected_folder"),
      numericInput("sample_num", "Sample Number:", value = 50, min = 1),
      actionButton("run", "Nieuwe steekproef", class = "btn-primary"),
      hr(),
      uiOutput("audio"),  # This will be populated with HTML5 audio player
      hr(),
      actionButton("score1", "Klauwkikker", style = "background-color: #ff4444; color: white;"),
      actionButton("score2", "Achtergrond", style = "background-color: #44ff44; color: black;"),
      actionButton("score3", "Onzeker", style = "background-color: #888888; color: white;"),
      textOutput("accuracy_display")  # Add this line to display accuracy
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
  
  runjs <- function(js) {
    session$sendCustomMessage(type = 'jsCode', list(code = js))
  }
  
  # Reactive expression for accuracy calculation ####
  accuracy <- reactive({
    req(rv$df)
    
    # Filter for assessed files
    assessed_files <- rv$df[rv$df$final_result!="", ]
    
    # Calculate correct predictions
    correct_predictions <- sum(
      (assessed_files$model_result == "likely" & assessed_files$final_result == "Klauwkikker") |
        (assessed_files$model_result %in% c("unlikely", "highly_unlikely") & 
           assessed_files$final_result %in% c("Achtergrond", "Onzeker"))
    )
    
    print(correct_predictions)
    print(nrow(assessed_files))
    
    # Calculate accuracy
    accuracy <- correct_predictions / nrow(assessed_files)
    
    # Return formatted accuracy
    paste0("Model resultaat correct: ", correct_predictions, " van ", nrow(assessed_files), " gecontroleerde fragmenten => Accuracy:", sprintf("%.2f%%", accuracy * 100))
  })
  
  # Display selected folder ####
  output$selected_folder <- renderText({
    req(input$folder)
    
    # Parse the selected folder path
    rv$folder_path <- parseDirPath(volumes, input$folder)
    
    # Ensure folder_path is valid before proceeding
    if (!is.null(rv$folder_path) && nzchar(rv$folder_path)) {
      # Check if steekproef_results.csv exists and load it
      results_file <- file.path(rv$folder_path, "steekproef_results.csv")
      if (file.exists(results_file)) {
        rv$df <- read.csv(results_file, stringsAsFactors = FALSE)
        print("Loaded existing steekproef_results.csv")
      } else {
        rv$df <- NULL
        print("No existing steekproef_results.csv found")
      }
    } else {
      rv$df <- NULL
      print("Invalid folder path")
    }
    
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
        stateSave = TRUE,
        stateDuration = -1,  # Save state in sessionStorage
        pageLength = input$sample_num * 3,  # Set a consistent page length
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
  })
  
  output$selected_file_path <- renderText({
    req(rv$selected_row, rv$df)
    paste("Selected File Path:", rv$df$full_path[rv$selected_row])
  })
  
  # Audio player ####
  output$audio <- renderUI({
    req(rv$current_audio)
    base64 <- dataURI(file = rv$current_audio, mime = "audio/wav")
    tags$audio(src = base64, type = "audio/wav", controls = TRUE, style = "width: 100%;", autoplay = TRUE)
  })
  
  # Render accuracy ####
  output$accuracy_display <- renderText({
    accuracy()
  })
  
  # Score buttons handler ####
  observeEvent(list(input$score1, input$score2, input$score3), {
    req(rv$selected_row, rv$df)
    
    # Determine which button was clicked
    clicked_button <- which(c(input$score1, input$score2, input$score3) > 0)
    
    if (length(clicked_button) > 0) {
      # Highlight the clicked button
      runjs(sprintf("$('#score%s').css('background-color', 'yellow');", clicked_button))
      
      score <- c("Klauwkikker", "Achtergrond", "Onzeker")[clicked_button]
      
      # Update the final_result column for the selected row
      rv$df$final_result[rv$selected_row] <- score
      
      # Write updated data to CSV
      write.csv(rv$df, file.path(rv$folder_path, "steekproef_results.csv"), row.names = FALSE)
      
      # Get current table state
      current_state <- input$results_table_state
      
      # Refresh the table while maintaining the current state
      dataTableProxy("results_table") %>%
        replaceData(rv$df[, c("file_name", "model_result", "final_result")], 
                    resetPaging = FALSE, 
                    rownames = FALSE) %>%
        selectRows(rv$selected_row) %>%
        selectPage(as.integer(current_state$start / current_state$length) + 1)
      
      # Reset all button colors
      runjs("$('#score1').css('background-color', '#ff4444');")
      runjs("$('#score2').css('background-color', '#44ff44');")
      runjs("$('#score3').css('background-color', '#888888');")
      
      # Reset score & clicked_button
      score <- NULL
      clicked_button <- NULL
      
      # Trigger accuracy recalculation
      accuracy()
    }
  })
  
  
  # Save on exit ####
  session$onSessionEnded(function() {
    observe({
      if(!is.null(rv$df) && !is.null(rv$folder_path)) {
        write.csv(rv$df, file.path(rv$folder_path, "steekproef_results.csv"), row.names = FALSE)
      }
    })
  })
}

shinyApp(ui, server)
