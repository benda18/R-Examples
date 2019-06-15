## Current STIP parse from NCDOT webiste

##LIBRARIES----
library("tidyverse")
library("pdftools")
library("lubridate")

##FUNCTIONS----


##VARS----
url_current_stip <- "https://connect.ncdot.gov/projects/planning/STIPDocuments1/NCDOT%20Current%20STIP.pdf"
url_amendments_webpage <- "https://connect.ncdot.gov/projects/planning/Pages/STIP-Amendments.aspx"

##TIDY----
#---1a Download current stip
if(!file.exists("CurrentSTIP.pdf")){
  download.file(url_current_stip, "CurrentSTIP.pdf")
  print("Downloading CurrentSTIP.pdf...")
}
cur_stip_asPDF <- pdf_text("CurrentSTIP.pdf")
#---1b identify number of pages 
n_pages <- sum(length(cur_stip_asPDF)) #gets last page number so that you can iterate through 1:len
df_stip <- enframe(cur_stip_asPDF) 

#---2a Note date of file
cur_stip_date <- df_stip[[1]][1] %>% str_replace_all("\\s+", " ") %>% trimws()
for (i in 1:12){
  if(grepl(tolower(month.name[i]), tolower(cur_stip_date)) == TRUE){
    cs_date     <- gsub(" ", "", paste(month.name[i], str_extract_all(cur_stip_date,"\\d+")[[1]]))
    cs_date_pdf <- gsub(" ", "", paste(cs_date,"STIP.pdf"))
  }
}

#---2b Archive current stip as pdf with version date as filename
if(!file.exists(paste(cs_date_pdf))){
  #download.file(url_current_stip, paste(cs_date_pdf))
  file.copy("CurrentSTIP.pdf", cs_date_pdf)
  #print(paste("Downloading", cs_date_pdf, "...\ndone...."))
}

#---3 Keep only pages that include "non-highway projects" header and other key words
#head(df_stip[[1]][493])

keep_pgs <- NULL
for (i in 1:as.numeric(tally(df_stip))){
  if(grepl("non highway program", df_stip[[1]][i], ignore.case = TRUE) == TRUE &
     grepl("capital area", df_stip[[1]][i], ignore.case = TRUE) == TRUE &
     grepl("transit", df_stip[[1]][i], ignore.case = TRUE) == TRUE &
     grepl("wake", df_stip[[1]][i], ignore.case = TRUE) == TRUE){
    keep_pgs <- union(keep_pgs, i)
  }
}



#---4 Keep only pages that have wake county projects.
#strsplit()
gorCS <- df_stip[[1]][keep_pgs]
View(gor_CS)
t_gorCS <- strsplit(gorCS,"\n")
t_gorCS

#---5 Parse and save header row


#---6 index every line with respect to top level project line


#---7 Keep only GoRaleigh or CAT projects. 


#---8 Output a tidied data frame of GoR stip projects

