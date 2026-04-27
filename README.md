# XenopusR

detection of calls of Xenopus laevis underwater

## Structure of the repository

-   `input/` contains the input files. These files mainly originate from the hydromoths deployed in the field.
    -   `input/trainingdata/` contains the data used for training the model. These are the data that have an id (klauwkikker_x, achtergrond_x, ...\_x), are determined to be a target species or noise (achtergrond) and are used for training.
    -   `input/testdata/` contains the data used for testing the model these are that have an id (yyyymmdd_hhmmsss), are determined to be the target species or noise and are not used for training. This folder also contains a .csv with the ids and the corresponding detections.
    -   `input/onbekend/` contains the data that is not determined to be the target species or noise and is not used for training.
    -   `input/%classification%/` This is a set of folders that contain the data that is classified as a certain class. This is used for the traiing of the model. Upon training the files are renamed to the id of the class and moved to the trainingdata folder.
-   `output/` contains the output files. The most important file is the .rda file that contains the trained model.
-   `src/` contains the scripts
