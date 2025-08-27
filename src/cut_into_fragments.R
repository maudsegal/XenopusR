cut_into_fragments <- function(fragment_length = 60){
  
  # Set input_dir
  input_dir <- choose.dir(caption = "Set input_dir")
  
  if(!dir.exists(input_dir)){
    stop(paste(input_dir, "doesn't exist => choose another folder"))
  }
  
  # Set Cores
  num_cores <- parallel::detectCores()
  print(num_cores)
  
  if(num_cores - 4 > 1){
    if(askYesNo(msg = "Use parallel computing?")){
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
    cat("warbleR is not installed => installing")
    remotes::install_github("maRce10/warbleR")
  }
  
  # Split into fragments
  warbleR::split_sound_files(path = input_dir, 
                             sgmt.dur = fragment_length,
                             parallel = cores_to_use,
                             pb = TRUE)
}