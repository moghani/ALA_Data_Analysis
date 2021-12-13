#How to download the data from https://www.ala.org.au/
#-------------------------------------------------------------------------------
#
# preparation: necessary package. With the next comand lines (line 8 to 11)
# it checks if the package exist and if they do not exist in the system, they 
# are installed. The first time that run this part, it may be slow until the 
# installation is finished if you do not have the packages previously installed.
if (!require("galah")) install.packages("galah", dependencies = TRUE)

#Now we load the packages:
library(galah) # A package created by ALA
#(https://www.ala.org.au/blogs-news/galah-takes-flight-alas-new-r-package-now-available/) 
#to download the data directly using on R.

#Download the data from ALA
#-----------------------------
#First we need to create an account in the 
#ALA website (https://auth.ala.org.au/userdetails/registration/createAccount)

#----------------------------------------------------------------
#Remember:
#Only after the account has been created, go to the next command.
#----------------------------------------------------------------
galah_config(email="john@example.com") # run this command to set email of 
# login account in ALA to download

#Download the data with the function ala_occurrences(). This function 
#have the argument to select the records "taxa = select_taxa("Reptilia")", 
#the argument to filter "filters =select_filters()" the records and the 
#columns or variables you want to see for the records "columns = select_columns()"
occ <- ala_occurrences(taxa = select_taxa("Reptilia"),
                       filters = select_filters(stateProvince = "Australian Capital Territory", 
                                                assertions!="TAXON_MATCH_NONE",
                                                assertions!="INVALID_SCIENTIFIC_NAME",
                                                assertions!="TAXON_HOMONYM",
                                                assertions!="UNKNOWN_KINGDOM",
                                                assertions!="TAXON_SCOPE_MISMATCH",
                                                duplicateType!="DIFFERENT_DATASET",
                                                coordinateUncertaintyInMeters<=10000,
                                                occurrenceStatus!="ABSENT"), 
                       columns = select_columns("decimalLatitude","decimalLongitude",
                                                "coordinateUncertaintyInMeters",
                                                "stateProvince", 
                                                "habitat", "cl10902",
                                                "scientificName","basisOfRecord",
                                                "kingdom",	"phylum",	"class",	
                                                "order",	"family",
                                                "taxonRank","dataResourceName"))

#Next we save the data as ".csv" format,
write.csv(occ, "code/downloaded_data.csv")
