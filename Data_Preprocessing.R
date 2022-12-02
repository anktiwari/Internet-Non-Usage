#1 Working Directory   
getwd()   
setwd("/Users/syedmurtazahassan7/Desktop/R/R Assignment")  

#2 Installing & Calling Library 
install.packages("ggplot2") 
library("ggplot2") 
install.packages("tidyverse") 
library("tidyverse")
install.packages("dpylr")
library("dplyr")
install.packages("janitor")
library("janitor")

#3 Importing the dataset - Reading CSV File
nonUse <- read.csv('nonUse.csv', header = TRUE)

#4 Cleaning data
nonUse <- clean_names(nonUse)
colnames(nonUse)

#5 Removing empty Columns and Rows - If any
nonUse<-nonUse %>% remove_empty(whic=c("rows"))
nonUse<-nonUse %>% remove_empty(whic=c("cols"))

#6 Check sum of Null & NA values
sum(is.na(nonUse))  
sum(is.null(nonUse))


# Change Column & Row names
nonUse <- read.csv("C:/Users/Ank/Downloads/nonUse.csv", header = TRUE)


nonUse$PROVINCE1 = ifelse(nonUse$PROVINCE == 10, "Newfoundland and Labrador", 
                       ifelse(nonUse$PROVINCE == 11,"Prince Edward Island",
                              ifelse(nonUse$PROVINCE == 12,"Nova Scotia",
                                     ifelse(nonUse$PROVINCE == 13,"New Brunswick",
											ifelse(nonUse$PROVINCE == 24,"Quebec",
													ifelse(nonUse$PROVINCE == 35,"Ontario",
															ifelse(nonUse$PROVINCE == 46,"Manitoba",
																	ifelse(nonUse$PROVINCE == 47,"Saskatchewan",
																			ifelse(nonUse$PROVINCE == 48,"Alberta","British Columbia")))))))))



nonUse$REGION1 = ifelse(nonUse$REGION == 1, "Atlantic Region", 
                       ifelse(nonUse$REGION == 2,"Quebec",
                              ifelse(nonUse$REGION == 3,"Ontario",
                                     ifelse(nonUse$REGION == 4,"Manitoba/Saskatchewan",
											                      ifelse(nonUse$REGION == 5,"Alberta","British Columbia")))))
										

nonUse$COMMUNITY = ifelse(nonUse$G_URBRUR == 01, "Montreal", 
                       ifelse(nonUse$G_URBRUR == 02,"Toronto",
                              ifelse(nonUse$G_URBRUR == 03,"Vancouver",
                                     ifelse(nonUse$G_URBRUR == 04,"Other Urban excluding Prince Edward Island",
											                      ifelse(nonUse$G_URBRUR == 05,"Rural excluding Prince Edward Island","Prince Edward Island")))))



nonUse$AGEGRP = ifelse(nonUse$GCAGEGR6 == 01, "16 to 24", 
                       ifelse(nonUse$GCAGEGR6 == 02,"25 to 34",
                              ifelse(nonUse$GCAGEGR6 == 03,"35 to 44",
                                     ifelse(nonUse$GCAGEGR6 == 04,"45 to 54",
											                      ifelse(nonUse$GCAGEGR6 == 05,"55 to 64","65 and older")))))

									
nonUse$GENDER = ifelse(nonUse$CSEX == 1, "MALE", "FEMALE")

nonUse$EDUCATIONLEVEL = ifelse(nonUse$G_CEDUC == 1, "High school or less",
								                ifelse(nonUse$G_CEDUC == 2, "College or some post-secondary", "University certificate or degree"))


nonUse$ISSTUDENT = ifelse(nonUse$G_CSTUD == 1, "Yes", "No")


nonUse$EMPLOYMENTSTATUS = ifelse(nonUse$G_CLFSST == 1, "Employed",
							                  	ifelse(nonUse$G_CLFSST == 2, "Unemployed", "Not in the labour force"))


nonUse$HOUSEHOLDTYPE = ifelse(nonUse$GFAMTYPE == 1, "Single family household with unmarried children under 16",
              								ifelse(nonUse$GFAMTYPE == 2, "Single family household without unmarried children under 16",
              										ifelse(nonUse$GFAMTYPE == 3, "One person households", "Multi family households")))



nonUse$HOUSEHOLDSIZE = ifelse(nonUse$G_HHSIZE == 1, "1 person",
              								ifelse(nonUse$G_HHSIZE == 2, "2 persons",
              										ifelse(nonUse$G_HHSIZE == 3, "3 persons", "4 or more persons")))


nonUse$HOUSEHOLDEDUCATION = ifelse(nonUse$G_HEDUC == 1, "High school or less",
								                  ifelse(nonUse$G_HEDUC == 2, "College or some post-secondary", "University certificate or degree"))
							
nonUse$STUDENTINHOUSEHOLD = ifelse(nonUse$G_HSTUD == 1, "Yes", "No")

nonUse$ISINTERNETUSER = ifelse(nonUse$EV_Q01 == 1, "Yes", "No")

nonUse$INTERNETUSEDURATION = ifelse(nonUse$EV_Q02 == 1, "Less than 1 year", 
                  								   ifelse(nonUse$EV_Q02 == 2,"1 to 2 years",
                  										  ifelse(nonUse$EV_Q02 == 3,"2 to 5 years",
                  												 ifelse(nonUse$EV_Q02 == 4,"5 or more years",
                  														ifelse(nonUse$EV_Q02 == 6,"Valid skip",
                  																ifelse(nonUse$EV_Q02 == 7,"Don't know", "Refusal"))))))
                  																
nonUse$nonUse = ifelse(nonUse$NU_Q01 == 1, "Yes",
      								ifelse(nonUse$NU_Q01 == 2, "No", 
      										ifelse(nonUse$NU_Q01 == 6, "Valid skip","Don't know")))
								
nonUse$NU_COST = ifelse(nonUse$NU_Q02A == 1, "Yes", 
        								ifelse(nonUse$NU_Q02A == 2,"No",
        										ifelse(nonUse$NU_Q02A == 6,"Valid skip",
        												ifelse(nonUse$NU_Q02A == 7,"Don't know",
        														ifelse(nonUse$NU_Q02A == 8,"Refusal","Not stated")))))
														
nonUse$NU_LIMITEDACCESS = ifelse(nonUse$NU_Q02B == 1, "Yes", 
                								ifelse(nonUse$NU_Q02B == 2,"No",
                										ifelse(nonUse$NU_Q02B == 6,"Valid skip",
                												ifelse(nonUse$NU_Q02B == 7,"Don't know",
                														ifelse(nonUse$NU_Q02B == 8,"Refusal","Not stated")))))
														
nonUse$NU_DIFFICULT = ifelse(nonUse$NU_Q02C == 1, "Yes", 
              								ifelse(nonUse$NU_Q02C == 2,"No",
              										ifelse(nonUse$NU_Q02C == 6,"Valid skip",
              												ifelse(nonUse$NU_Q02C == 7,"Don't know",
              														ifelse(nonUse$NU_Q02C == 8,"Refusal","Not stated")))))

nonUse$NU_NONEED = ifelse(nonUse$NU_Q02D == 1, "Yes", 
          								ifelse(nonUse$NU_Q02D == 2,"No",
          										ifelse(nonUse$NU_Q02D == 6,"Valid skip",
          												ifelse(nonUse$NU_Q02D == 7,"Don't know",
          														ifelse(nonUse$NU_Q02D == 8,"Refusal","Not stated")))))
														
nonUse$NU_NOTIME = ifelse(nonUse$NU_Q02E == 1, "Yes", 
          								ifelse(nonUse$NU_Q02E == 2,"No",
          										ifelse(nonUse$NU_Q02E == 6,"Valid skip",
          												ifelse(nonUse$NU_Q02E == 7,"Don't know",
          														ifelse(nonUse$NU_Q02E == 8,"Refusal","Not stated")))))
														
nonUse$NU_LACKOFSKILLS = ifelse(nonUse$NU_Q02F == 1, "Yes", 
                								ifelse(nonUse$NU_Q02F == 2,"No",
                										ifelse(nonUse$NU_Q02F == 6,"Valid skip",
                												ifelse(nonUse$NU_Q02F == 7,"Don't know",
                														ifelse(nonUse$NU_Q02F == 8,"Refusal","Not stated")))))
														
nonUse$NU_NOINTEREST = ifelse(nonUse$NU_Q02I == 1, "Yes", 
								              ifelse(nonUse$NU_Q02I == 2,"No",
										                ifelse(nonUse$NU_Q02I == 6,"Valid skip",
												                  ifelse(nonUse$NU_Q02I == 7,"Don't know",
														                      ifelse(nonUse$NU_Q02I == 8,"Refusal","Not stated")))))
nonUse$NU_AGEREASONS = ifelse(nonUse$NU_G02K == 1, "Yes", 
              								ifelse(nonUse$NU_G02K == 2,"No",
              										ifelse(nonUse$NU_G02K == 6,"Valid skip",
              												ifelse(nonUse$NU_G02K == 7,"Don't know",
              														ifelse(nonUse$NU_G02K == 8,"Refusal","Not stated")))))
nonUse$NU_OTHERS = ifelse(nonUse$NU_G02 == 1, "Yes", 
          								ifelse(nonUse$NU_G02 == 2,"No",
          										ifelse(nonUse$NU_G02 == 6,"Valid skip","Not stated")))


#Split new data set in separate file
nonUse_new <- nonUse[,25:48]
