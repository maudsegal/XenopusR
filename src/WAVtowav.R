WAVtowav <- function(input_dir, 
                     output_dir = NULL){
  # This function will convert all .WAV files in a directory to .wav files
  # input_dir is the directory where the .WAV files are stored
  # output_dir is the directory where the .wav files will be stored. If NULL, the .wav files will be stored in the input directory
  # The function will return a list of the .wav files that were created
  
  # If the output directory is NULL, set it to the input directory
  if(is.null(output_dir)){
    output_dir <- input_dir
  } else {
    # If the output directory does not exist, create it
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }
  }
  
  # Get a list of all the .WAV files in the input directory
  list.of.wav.files <- list.files(input_dir, pattern = ".WAV", recursive = TRUE)
  
  # Create a list to store the .wav files that are created
  list.of.wav.files.created <- list()
  
  # Loop through the .WAV files
  # Skip loop if no .WAV files are found
  if(length(list.of.wav.files) == 0){
    cat("No .WAV files found in the input directory")
  }else{
    # initiate progress bar
    pb <- utils::txtProgressBar(min = 0, max = length(list.of.wav.files), style = 3)
    
    for(x in 1:length(list.of.wav.files)){
      # update progress bar
      utils::setTxtProgressBar(pb, x)
      
      # Get the name of the .WAV file
      temp.wav.file <- list.of.wav.files[x]
      temp.wav.file <- paste(input_dir, temp.wav.file, sep = "/")
      
      # Get the name of the .wav file
      temp.wav.file.new <- gsub(".WAV", ".wav", temp.wav.file)
      
      # Check if the .wav file already exists
      if(file.exists(temp.wav.file.new)){
        # if file exists copy it to a temporary file
        temp.file <- tempfile()
        
        file.copy(from = temp.wav.file.new, 
                  to = temp.file)
        file.remove(temp.wav.file.new)
        file.copy(from = temp.file, 
                  to = temp.wav.file.new)
      }else{
        # if file does not exist, copy it to the new file
        file.copy(from = temp.wav.file, 
                  to = temp.wav.file.new)
      }
      
      # Add the new .wav file to the list
      list.of.wav.files.created[[x]] <- temp.wav.file.new
    }}
  # Return the list of .wav files that were created
  return(list.of.wav.files.created)
}