library(tidyverse)
library(lubridate)

################## WAVE DATA ##################################################

prep_wave <- function(wave_file, institution_file = NULL){
  
  # Read in data
  WAVE_data <- read.csv(wave_file)
  
  # Read times as date times
  WAVE_data$Tap.Time <- str_replace_all(WAVE_data$Tap.Time, "pm", "PM")
  WAVE_data$Tap.Time <- str_replace_all(WAVE_data$Tap.Time, "am", "AM")
  WAVE_data$Time <- as.POSIXct(paste(WAVE_data$Tap.Date, 
                                     WAVE_data$Tap.Time),
                               format = "%m/%d/%Y %I:%M:%S %p",
                               tz = "EST")
  
  # Correct route number
  WAVE_data$Route.Number <- str_sub(WAVE_data$Route.Number, 2)
  
  # Filter out errors followed by another entry to avoid duplicates
  num_mins <- 1
  remove_ids <- WAVE_data %>%
    group_by(Card.Number) %>%
    arrange(Time, .by_group = TRUE) %>%
    filter(Transaction.Status != "Ok" & 
             # followed by another transaction within 1 minute
             abs(as.numeric(difftime(Time, lead(Time), 
                                     units = "mins") ) < num_mins)) %>%
    pull(Ride.Transaction.ID)
  
  # Drop those IDs - all others represent true demand in our assumption
  WAVE_data <- WAVE_data[!(WAVE_data$Ride.Transaction.ID %in% remove_ids), ]
  
  # Creating uniform types across categories
  
  # Rename to Type
  WAVE_data <- WAVE_data %>%
    rename(Type = Fare.Category)
  
  # Merging "Off-Peak" Data
  WAVE_data$Type[WAVE_data$Type == "Disabled Off-Peak"] <- "Disabled"
  WAVE_data$Type[WAVE_data$Type == "Senior Off-Peak"] <- "Senior"
  
  # Adding Low Income Type to Adult Type
  WAVE_data$Type[WAVE_data$Type == "Low Income"] <- "Adult"
  
  #Switching Spouse/Retiree to Employee/Spouse/Retiree
  WAVE_data$Type[WAVE_data$Product.Name == "Employee/Spouse/Retiree"] <- "Employee/Spouse/Retiree"
  WAVE_data$Type[WAVE_data$Type == "Spouse/Retiree"] <- "Employee/Spouse/Retiree"
  
  # Create new tag columns from the product name
  WAVE_data <- WAVE_data %>%
    rename(Tag = Product.Name)
  
  # Adding Tags
  WAVE_data$Monthly.Pass <- ifelse(
    WAVE_data$Tag %in% c("Monthly Pass (Institutional)", "Monthly Pass (Student)",
                         "Monthly Pass", "Monthly Pass (Goodwill)", 
                         "Monthly Pass QR Code"), 1, 0
  )
  WAVE_data$Eco.Pass <- ifelse(WAVE_data$Tag %in% c("EcoPass"), 1, 0)
  WAVE_data$Day.Pass <- ifelse(
    WAVE_data$Tag %in% c("Day Pass", "Day Pass (Institutional)", 
                         "Day Pass (Disposable)", "Day Pass (Student)"), 1, 0)
  WAVE_data$Student <- ifelse(
    WAVE_data$Tag %in% c("Monthly Pass (Institutional)", "Monthly Pass (Student)",
                         "Day Pass (Institutional)", "Day Pass (Student)",
                         "UPASS"), 1, 0)
  WAVE_data$Off.Peak <- ifelse(
    WAVE_data$Tag %in% c("Senior Off-Peak", "Disability Off-Peak"), 1, 0)
  WAVE_data$Low.Income <- ifelse(
    WAVE_data$Tag %in% c("Disability", "Low Income", "Senior"), 1, 0)
  WAVE_data$One.Hour.Pass <- ifelse(
    WAVE_data$Tag %in% c("1-HOUR Pass (Disposable)", "Single Ride QR Code"), 
    1, 0)
  WAVE_data$Transfer <- ifelse(
    WAVE_data$Transaction.Type %in% c("Transfer"), 1, 0)
  
  # Add in institution data
  if (!is.null(institution_file)){
    
    # select columns
    Institution_data <- read.csv(institution_file, skip = 6) 
    
    # update time and select cols
    Institution_data$Time <- as.POSIXct(paste(Institution_data$Tap.Date, 
                                              Institution_data$Tap.Time),
                                        format = "%m/%d/%Y %I:%M:%S %p",
                                        tz = "EST")
    Institution_data <- Institution_data %>% select(Institution.Name, 
                                                    Card.Number, Time) %>%
      mutate(Card.Number = as.character(Card.Number))
    
    # join in institution name to WAVE data
    # Adds about 1000 entries because of multiple matches
    WAVE_data <- left_join(WAVE_data, Institution_data, 
                                      by = c("Time","Card.Number"),
                                      relationship = "many-to-many")
  } else{
    WAVE_data$Institution.Name = "None"
  }
  
  
  # Select final columns
  WAVE_data$Source <- "WAVE"
  WAVE_data$Route.Number <- as.numeric(WAVE_data$Route.Number)
  WAVE_data <- WAVE_data %>%
    select(Source, Route.Number, Trip.Number, Stop.Number, Time, Ride.Count, 
           Type, Student, Institution.Name, Low.Income, Off.Peak, 
           Eco.Pass, Transfer, Monthly.Pass, Day.Pass, One.Hour.Pass) 
  
  return(WAVE_data)
}

################## TTP DATA ################################################### 

prep_ttp <- function(ttp_filename){

  # Pivot to separate by type
  TTP_data <- read.csv(ttp_filename, skip=5) %>% 
    pivot_longer(cols = c("TTP1.FF.XFER":"TTP48.CHANGE"), 
                 names_to = "Product.Type", values_to = "Ride.Count") %>% 
    filter(Ride.Count !=0, !(Location %in% c("Route Total", "Location Total",
                                             "Grand Total"))) %>%
    mutate(Product.Type =  word(Product.Type, 1, sep="\\."),
           Route = as.numeric(Route)) %>%
    rename(Route.Number = Route, Trip.Number = Trip)
  
  # Update time
  TTP_data$Time <- as.POSIXct(TTP_data$Date.Time, 
                              format = "%m/%d/%y %H:%M",
                              tz = "EST")
  
  # Delete non-ridership categories
  delete_cats <- c("TTP4", "TTP18", "TTP47", "TTP43", "TTP44")
  TTP_data <- TTP_data[!(TTP_data$Product.Type %in% delete_cats | 
                         is.na(TTP_data$Product.Type)), ]
  
  # Categorize
  adult_cats <- c("TTP6", "TTP7", "TTP8", "TTP9", "TTP10", "TTP12", "TTP14", 
                  "TTP15", "TTP17", "TTP19", "TTP20", "TTP21", "TTP23", 
                  "TTP24", "TTP25", "TTP26", "TTP28", "TTP30", "TTP34",
                  "TTP35", "TTP37", "TTP38", "TTP39", 
                  "TTP40", "TTP41", "TTP42", "TTP45", "TTP46", "TTP48")
  senior_cats <- c("TTP16", "TTP29", "TTP36")
  disabled_cats <- c("TTP27", "TTP31", "TTP32")
  employee_cats <- c("TTP5", "TTP13", "TTP33")
  transfer_cats <- c("TTP1", "TTP11", "TTP2", "TTP3", "TTP22")
  
  # Adding "Type" Column (Adult, Senior, Employee/Spouse/Retiree, Disabled)
  TTP_data$Type <- case_when(TTP_data$Product.Type %in% adult_cats ~ "Adult",
                            TTP_data$Product.Type %in% senior_cats ~ "Senior",
                            TTP_data$Product.Type %in% disabled_cats ~ 
                              "Disabled",
                            TTP_data$Product.Type %in% employee_cats ~ 
                              "Employee/Spouse/Retiree",
                            TTP_data$Product.Type %in% transfer_cats ~ 
                              "Transfer")
  
  # Adding Tags
  TTP_data$Student <- ifelse(
    TTP_data$Product.Type %in% c("TTP8", "TTP10", "TTP12", "TTP23",
                                "TTP26", "TTP28", "TTP30", "TTP38","TTP40"), 1, 0
  )
  TTP_data$Low.Income <- ifelse(
    TTP_data$Product.Type %in% c("TTP16", "TTP27", "TTP36"), 1, 0)
  TTP_data$Off.Peak <- ifelse(TTP_data$Product.Type %in% c("TTP41"), 1, 0)
  TTP_data$Eco.Pass <- ifelse(TTP_data$Product.Type %in% c("TTP42"), 1, 0)
  TTP_data$Day.Pass <- ifelse(
    TTP_data$Product.Type %in% c("TTP14", "TTP24"), 1, 0)
  TTP_data$Monthly.Pass <- ifelse(TTP_data$Product.Type %in% c("TTP9"), 1, 0)
  TTP_data$Week.Pass <- ifelse(TTP_data$Product.Type %in% c("TTP34", "TTP37"), 
                               1, 0)
  TTP_data$Two.Hour.Pass <- ifelse(TTP_data$Product.Type %in% c("TTP35"), 1, 0)
  TTP_data$Ten.Ride.Pass <- ifelse(
    TTP_data$Product.Type %in% c("TTP7", "TTP25", "TTP39"), 1, 0)
  
  # Select columns
  TTP_data$Source <- "TTP"
  TTP_data <- TTP_data %>%
    select(Source, Route.Number, Trip.Number, Time, Ride.Count, 
           Type, Student, Low.Income, Off.Peak, Eco.Pass, Monthly.Pass, 
           Ten.Ride.Pass, Week.Pass, Day.Pass, Two.Hour.Pass)
  
  return(TTP_data)
}

###################### KEY ####################################################

prep_key <- function(key_filename){
  
  # Read in data and remove columns
  KEY_data <- read.csv(key_filename, skip=5) %>% 
    select(-c("Key.6.WHEELCHR", "Key.8.EMG.XFER", "Key..", "Key.B.BIKE",
                                          "Key.C.CHANGE")) %>%
    mutate(Route = as.numeric(Route)) %>%
    rename(Route.Number = Route, Trip.Number = Trip) %>%
    filter(!is.na(Route.Number))
    
  # Convert to numeric
  num_cols <- colnames(KEY_data)[grepl("Key", colnames(KEY_data))]
  KEY_data[, num_cols] <- lapply(num_cols, function(x) as.numeric(KEY_data[,x]))
  
  # Pivot to separate by type 
  KEY_data <- KEY_data %>% 
    pivot_longer(cols = c("Key.1.SP.EVENT":"Key.D.DAY.PASS"), 
                 names_to = "Product.Type", values_to = "Ride.Count") %>%
    filter(Ride.Count != 0)
  
  # Update time
  KEY_data$Time <- as.POSIXct(KEY_data$Date.and.Time, 
                              format = "%m/%d/%Y %H:%M:%S",
                              tz = "EST")
  
  
  # Add type
  adult_keys <- c("Preset", "Key.1.SP.EVENT", "Monthly.Pass", "Student", 
                  "Ten.Ride.Pass","Week.Pass", "Key.9.NO.READ", 
                  "Low.Income", "Day.Pass")
  child_keys <- c("Key.4.KIDS")
  KEY_data$Type <- case_when(KEY_data$Product.Type %in% adult_keys ~ "Adult",
                            KEY_data$Product.Type %in% child_keys ~ "Child")
  
  # Adding tags
  KEY_data$Student <- ifelse(KEY_data$Product.Type %in% c("Key.3.UPASS"), 1, 0)
  KEY_data$Low.Income <- ifelse(KEY_data$Product.Type %in% c("Key.A.PCA"), 1, 0)
  KEY_data$Day.Pass <- ifelse(KEY_data$Product.Type %in% c("Key.D.DAY.PASS"), 
                              1, 0)
  KEY_data$Monthly.Pass <- ifelse(KEY_data$Product.Type %in% c("Key.2.MBTA"), 
                                  1, 0)
  KEY_data$Week.Pass <- ifelse(KEY_data$Product.Type %in% c("Key.7.ISS.7DAY"), 
                               1, 0)
  KEY_data$Ten.Ride.Pass <- ifelse(
    KEY_data$Product.Type %in% c("Key.5.10.RIDE"), 1, 0)
  
  # Select cols
  KEY_data$Source <- "KEY"
  KEY_data$Trip.Number <- as.numeric(KEY_data$Trip.Number)
  KEY_data <- KEY_data %>%
    select(Source, Route.Number, Trip.Number, Time, Ride.Count, 
           Type, Student, Low.Income, Monthly.Pass, Ten.Ride.Pass)
  return(KEY_data)
}

###################### MERGE ##################################################

prep_full <- function(wave_filename, inst_filename, ttp_filename, key_filename){
  
  # read in data
  WAVE_data <- prep_wave(wave_filename, inst_filename)
  TTP_data <- prep_ttp(ttp_filename)
  KEY_data <- prep_key(key_filename)
  
  # combine
  full_df <- bind_rows(WAVE_data, TTP_data, KEY_data)
  
  # Make NA's be 0 except Location
  tag_cols <- c("Student", "Low.Income", "Off.Peak", "Eco.Pass",
                "Transfer", "Monthly.Pass", "Day.Pass", "One.Hour.Pass", 
                "Ten.Ride.Pass", "Week.Pass", "Two.Hour.Pass")
  for (i in tag_cols){
    full_df[is.na(full_df[,i]),i] <- 0
  }
  
  # Make NA's in Institution.Name be "None"
  full_df$Institution.Name <- ifelse(is.na(full_df$Institution.Name), 
                                     "None", full_df$Institution.Name)
  
  # Add Trip ID
  full_df <- full_df %>%
    arrange(Time) %>%
    mutate(Trip.Id = 1:nrow(full_df))
  
  # Fill in stop number by next stop
  full_df <- full_df %>%
    group_by(Route.Number) %>%
    arrange(Time) %>%
    fill(names(.),.direction = "up") %>%
    na.omit()
  
  # Add days of the week column
  full_df$Day.of.Week <- wday(full_df$Time, label = TRUE)
  
  # Change order of columns in the data set
  full_df <- full_df %>% select("Trip.Id", "Time", "Day.of.Week", 
                                "Route.Number", "Stop.Number", "Trip.Number", 
                                "Source", "Ride.Count",
                                "Type", "Student", "Low.Income", 
                                "Off.Peak", "Eco.Pass", "Transfer", 
                                "One.Hour.Pass","Two.Hour.Pass", "Day.Pass",
                                "Ten.Ride.Pass", "Week.Pass", "Monthly.Pass",
                                "Institution.Name")
  
  return(full_df)
}


# DEMO
#setwd("/Users/alice/Dropbox/RIPTA/")
#wave_filename <- "data/Wave_Aug23.csv"
#inst_filename <- "data/Institutional Rides Invoice Report_2023-11-27_12-02-18_20611.csv"
#ttp_filename <- "data/CORRECTED_CashFareBox_TTPs_Aug23.csv"
#key_filename <- "data/CashFareBox_August23.csv"
#full_df <- prep_full(wave_filename, inst_filename, ttp_filename, key_filename)
#write.csv(full_df, "data/MergedData.csv", row.names = FALSE)


