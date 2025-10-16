cut_into_fragments <- function(input_dir = NULL,
                               fragment_length = 60){
  
  browser()
  # Set input_dir
  if(is.null(input_dir) || is.na(input_dir)){
    input_dir <- rstudioapi::selectDirectory()
  }
  
  if(!dir.exists(input_dir)){
    stop(paste(input_dir, "doesn't exist => choose another folder"))
  }
  cat("reading from:", input_dir,"\n")
  
  # Set Cores
  num_cores <- parallel::detectCores()
  cat("Number of cores advailable:", num_cores, "\n")
  
  if(num_cores - 4 > 1){
    if(askYesNo(msg = "Use parallel computing? \n")){
      cores_to_use <- num_cores - 4
    }else{
      cores_to_use <- 1
    }
  }else{
    cores_to_use <- 1
  }
  
  cat("Using", cores_to_use, "CPU cores")
  
  # Install warbleR
  if(!"warbleR" %in% installed.packages()){
    cat("warbleR is not installed => installing \n")
    remotes::install_github("maRce10/warbleR")
  }
  
  # Split into fragments
  warbleR::split_sound_files(path = input_dir, 
                             sgmt.dur = fragment_length,
                             parallel = cores_to_use,
                             pb = TRUE)
}