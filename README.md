# Installation for ISDS 1102 Instructors
1. Download this repository onto your computer.
2. Install R.
3. Install RStudio.
4. Open RStudio, navigate to the "packages" tab in the bottom-right window.
5. In the packages tab, click "install."
6. Install the following packages:

   - RMySQL
   - tidyverse
   - shiny
   - shinydashboard
   - RColorBrewer
   - scales

## Using the Dashboard
1. Change your working directory to the location of this repository on your computer.

   - Select the "files" tab from the same window where the "packages" tab is located.
   - This should default to your Documents folder. If your copy of the repository is in this folder, navigate to it from here.
   - If the repository is not in your Documents folder, type this into the RStudio console:
   `setwd("path")` (where path is the path to the proper folder)
   - If you navigate to the proper folder by clicking, you can set the working directory by clicking "More" in the files tab and 
   select "Set as Working Directory"
   
      - You can also get the working directory path from "More" or by typing `getwd()` into the console.

2. Create your own "connection.R" file.

   - Just under the "File" tab at the top left of RStudio, there is an icon for creating a new file. Click this and select "R Script"
   
      - Or, alternatively, use Ctrl+Shift+N
   - In type this code into the script:  
   ```
   library(tidyverse)
   
   conDatasource <- src_mysql(host = "host name", 
                              port = port#, 
                              user = "username", 
                              password = "password", 
                              dbname = "database name"
   )
   ```  
   Where the host name, port#, username, password, and database name are your credentials. If you don't have this information, ask someone who does.
   - Save the script as "connection.R" since that is the file name that is used in the backend.
3. Run the dashboard
   - Open either twoInputs.R or oneInput.R (twoInputs allows you to choose a section to narrow down the list of names, while oneInput 
   allows you to search all students enrolled in ISDS 1102 for the current semester).
   - In the top-right area of the top-left window (which shows the code and currently open files) select "Run App."
   - The dashboard might take a few seconds, but it should come up in another window.
