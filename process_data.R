################## WAVE DATA ##################################################
prep_wave <- function(wave_path, institution_path = NULL) {
  #' Pre-processes Wave data.
  #'
  #' @param wave_path string - File path to the Wave
  #' data
  #' @param institution_path string - File path to
  #' the institution data
  #' @returns data frame - Pre-processed Wave data
  
  # Read in data
  WAVE_data <- read.csv(wave_path)
  
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
             # Followed by another transaction within 1 minute
             abs(as.numeric(difftime(Time,
                                     lead(Time), 
                                     units = "mins") ) < num_mins)) %>%
    pull(Ride.Transaction.ID)
  
  # Drop those IDs - all others represent true demand in our assumption
  WAVE_data <- WAVE_data[!(WAVE_data$Ride.Transaction.ID %in% remove_ids), ]
  
  # Create uniform types across categories
  
  # Rename to Type
  WAVE_data <- WAVE_data %>%
    rename(Type = Fare.Category)
  
  # Merge "Off-Peak" Data
  WAVE_data$Type[WAVE_data$Type == "Disabled Off-Peak"] <- "Disabled"
  WAVE_data$Type[WAVE_data$Type == "Senior Off-Peak"] <- "Senior"
  
  # Add Low Income Type to Adult Type
  WAVE_data$Type[WAVE_data$Type == "Low Income"] <- "Adult"
  
  # Switch Spouse/Retiree to Employee/Spouse/Retiree
  WAVE_data$Type[WAVE_data$Product.Name == "Employee/Spouse/Retiree"] <-
    "Employee/Spouse/Retiree"
  WAVE_data$Type[WAVE_data$Type == "Spouse/Retiree"] <-
    "Employee/Spouse/Retiree"
  
  # Create new tag columns from the product name
  WAVE_data <- WAVE_data %>%
    rename(Tag = Product.Name)
  
  # Add Tags
  WAVE_data$Monthly.Pass <- ifelse(
    WAVE_data$Tag %in% c("Monthly Pass (Institutional)",
                         "Monthly Pass (Student)",
                         "Monthly Pass", "Monthly Pass (Goodwill)",
                         "Monthly Pass QR Code"), 1, 0)
  WAVE_data$Eco.Pass <- ifelse(WAVE_data$Tag %in% c("EcoPass"), 1, 0)
  WAVE_data$Day.Pass <- ifelse(
    WAVE_data$Tag %in% c("Day Pass",
                         "Day Pass (Institutional)", 
                         "Day Pass (Disposable)",
                         "Day Pass (Student)"), 1, 0)
  WAVE_data$Off.Peak <- ifelse(
    WAVE_data$Tag %in% c("Senior Off-Peak", "Disability Off-Peak"), 1, 0)
  WAVE_data$Low.Income <- ifelse(
    WAVE_data$Tag %in% c("Disability", "Low Income", "Senior"), 1, 0)
  WAVE_data$One.Hour.Pass <- ifelse(
    WAVE_data$Tag %in% c("1-HOUR Pass (Disposable)", "Single Ride QR Code"), 
    1, 0)
  WAVE_data$Transfer <- ifelse(
    WAVE_data$Transaction.Type %in% c("Transfer"), 1, 0)
  
  # Add in institutional data
  if (!is.null(institution_path)) {
    institution_data <- read.csv(institution_path)
    # Update time and select columns
    institution_data$Time <- as.POSIXct(paste(institution_data$Tap.Date, 
                                              institution_data$Tap.Time),
                                        format = "%m/%d/%Y %I:%M:%S %p",
                                        tz = "EST")
    institution_data <- institution_data %>%
      select(Institution.Name, Card.Number, Time) %>%
      mutate(Card.Number = as.character(Card.Number))
    
    # Join in institution name to WAVE data
    # Add about 1000 entries because of multiple matches
    WAVE_data <- left_join(WAVE_data,
                           institution_data,
                           by = c("Time","Card.Number"),
                           relationship = "many-to-many")
    
    # Create column for college
    WAVE_data$College <- case_when(
      WAVE_data$Institution.Name == "CCRI"
      ~ "Community College of Rhode Island",
      WAVE_data$Institution.Name == "URI Campus Store"
      ~ "University of Rhode Island",
      WAVE_data$Institution.Name == "Johnson & Wales University"
      ~ "Johnson & Wales",
      WAVE_data$Institution.Name == "Roger Williams University"
      ~ "Roger Williams",
      TRUE ~ "None")
    
    # Create column for high school
    WAVE_data$High.School <- case_when(
      WAVE_data$Institution.Name == "Providence Public School Department"
      ~ "Providence Public School Department",
      WAVE_data$Institution.Name == "The Met School"
      ~ "The Met School",
      WAVE_data$Institution.Name == "RI Nurses - Institutional Middle College"
      ~ "RI Nurses - Institutional Middle College",
      WAVE_data$Institution.Name == "Village Green Charter School"
      ~ "Village Green Charter School",
      WAVE_data$Institution.Name == "Achievement First RI"
      ~ "Achievement First RI",
      WAVE_data$Institution.Name == "Paul Cuffee School"
      ~ "Paul Cuffee School",
      WAVE_data$Institution.Name == "Charette Charter School"
      ~ "Charette Charter School",
      WAVE_data$Institution.Name == "Nowell Leadership Academy"
      ~ "Nowell Leadership Academy",
      WAVE_data$Institution.Name == "Trinity Academy for the Performing Arts"
      ~ "Trinity Academy for the Performing Arts",
      WAVE_data$Institution.Name == "Youth Build Prep Academy"
      ~ "Youth Build Prep Academy",
      WAVE_data$Institution.Name == "Times2 Academy"
      ~ "Times2 Academy",
      WAVE_data$Institution.Name == "Highlander Charter School"
      ~ "Highlander Charter School",
      WAVE_data$Institution.Name == "NEL/CPS Construction and Career Academy"
      ~ "NEL/CPS Construction and Career Academy",
      WAVE_data$Institution.Name == "Urban Collaborative Program"
      ~ "Urban Collaborative Program",
      WAVE_data$Institution.Name == "Blackstone Academy"
      ~ "Blackstone Academy",
      WAVE_data$Institution.Name == "360 High School"
      ~ "360 High School",
      WAVE_data$Institution.Name == "Central High School"
      ~ "Central High School",
      WAVE_data$Institution.Name == "Davies Career & Tech High School"
      ~ "Davies Career & Tech High School",
      WAVE_data$Institution.Name == "Bishop Hendricken HS"
      ~ "Bishop Hendricken HS",
      WAVE_data$Institution.Name == "Pawtucket School Department"
      ~ "Pawtucket School Department",
      WAVE_data$Institution.Name == "E-Cubed Academy"
      ~ "E-Cubed Academy",
      WAVE_data$Institution.Name == "Providence Career Technical HS"
      ~ "Providence Career Technical HS",
      WAVE_data$Institution.Name == "Warwick Schools"
      ~ "Warwick Schools",
      TRUE ~ "None")
    
    # Create column for institution type
    name_to_type <- readRDS("./data/institution_name_to_type.rds") %>%
      # Convert string None to NA to merge with NA values
      # in Institution.Name
      mutate(Institution.Name = replace(Institution.Name, 
                                        Institution.Name == "None", 
                                        NA))
    WAVE_data <- WAVE_data %>%
      left_join(name_to_type) %>%
      rename(Institution = Institution.Type)
  } else {
    WAVE_data$Institution.Name = "None"
    WAVE_data$College = "None"
    WAVE_data$High.School = "None"
    WAVE_data$Institution = "None"
  }
  
  # Add transfer columns
  WAVE_data <- WAVE_data %>%
    group_by(Card.Number) %>%
    arrange(Time, .by_group = TRUE)
  
  transfers <- sapply(1:nrow(WAVE_data),
                      function(i) is_transfer(i, WAVE_data))
  WAVE_data$Transfer <- transfers[1, ]
  WAVE_data$Transfer.2 = transfers[2, ]
  
  # Select final columns
  WAVE_data <- WAVE_data %>%
    mutate(Source = "WAVE",
           Route.Number = as.numeric(Route.Number)) %>%
    select(Source, Route.Number, Trip.Number, Stop.Number, Time,
           Ride.Count, Type, College, High.School, Institution.Name,
           Institution, Low.Income, Off.Peak, Eco.Pass, Transfer,
           Transfer.2, Monthly.Pass, Day.Pass, One.Hour.Pass, Card.Number)
  
  return(WAVE_data)
}

################## TTP DATA ################################################### 

prep_ttp <- function(path) {
  #' Pre-processes TTP data.
  #'
  #' @param path string - File
  #' path to the TTP data
  #'
  #' @returns data frame - Pre-processed
  #' TTP data
  
  # Pivot to separate by type
  TTP_data <- read.csv(path) %>%
    pivot_longer(cols = c("TTP1.FF.XFER":"TTP48.CHANGE"),
                 names_to = "Product.Type",
                 values_to = "Ride.Count") %>%
    filter(Ride.Count !=0,
           !(Location %in% c("Route Total",
                             "Location Total",
                             "Grand Total"))) %>%
    mutate(Product.Type =  word(Product.Type, 1, sep="\\."),
           Route = as.numeric(Route)) %>%
    rename(Route.Number = Route, Trip.Number = Trip)
  
  # Update time
  TTP_data$Time <- as.POSIXct(TTP_data$Date.Time, 
                              format = "%m/%d/%Y %H:%M",
                              tz = "EST")
  
  # Delete non-ridership categories
  delete_cats <- c("TTP4", "TTP18", "TTP47", "TTP43", "TTP44")
  TTP_data <- TTP_data[!(TTP_data$Product.Type %in% delete_cats | 
                           is.na(TTP_data$Product.Type)), ]
  
  # Categorize
  adult_cats <- c("TTP6", "TTP7", "TTP8", "TTP9", "TTP10", "TTP12",
                  "TTP14", "TTP15", "TTP17", "TTP19", "TTP20", "TTP21",
                  "TTP23", "TTP24", "TTP25", "TTP26", "TTP28", "TTP30",
                  "TTP32", "TTP34", "TTP35", "TTP37", "TTP38", "TTP39", 
                  "TTP40", "TTP41", "TTP42", "TTP45", "TTP46", "TTP48")
  senior_cats <- c("TTP16", "TTP29", "TTP36")
  disabled_cats <- c("TTP27", "TTP31")
  employee_cats <- c("TTP5", "TTP13", "TTP33")
  transfer_cats <- c("TTP1", "TTP11", "TTP2", "TTP3", "TTP22")
  
  # Add "Type" column (Adult, Senior, Employee/Spouse/Retiree, Disabled)
  TTP_data$Type <- case_when(TTP_data$Product.Type %in% adult_cats ~ "Adult",
                             TTP_data$Product.Type %in% senior_cats ~ "Senior",
                             TTP_data$Product.Type %in% disabled_cats
                             ~ "Disabled",
                             TTP_data$Product.Type %in% employee_cats
                             ~ "Employee/Spouse/Retiree",
                             TTP_data$Product.Type %in% transfer_cats
                             ~ "Transfer")
  
  # Add tags
  TTP_data$College <- case_when(
    TTP_data$Product.Type == "TTP8" ~ "Bryant",
    TTP_data$Product.Type == "TTP10" ~ "Salve Regina",
    TTP_data$Product.Type == "TTP12" ~ "Brown",
    TTP_data$Product.Type == "TTP23" ~ "RISD",
    TTP_data$Product.Type == "TTP26" ~ "Roger Williams",
    TTP_data$Product.Type %in% c("TTP28", "TTP38") ~ "Johnson & Wales",
    TTP_data$Product.Type == "TTP30" ~ "University of Rhode Island",
    TTP_data$Product.Type == "TTP32" ~ "Rhode Island College",
    TTP_data$Product.Type == "TTP40" ~ "Providence College",
    TRUE ~ "None", 
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
  TTP_data <- TTP_data %>%
    mutate(Source = "TTP") %>%
    select(Source, Route.Number, Trip.Number, Time, Ride.Count, 
           Type, College, Low.Income, Off.Peak, Eco.Pass,
           Monthly.Pass, Ten.Ride.Pass, Week.Pass, Day.Pass, Two.Hour.Pass)
  return(TTP_data)
}

###################### KEY ####################################################
prep_key <- function(path) {
  #' Pre-processes Key data.
  #'
  #' @param path string - File
  #' path to the Key data
  #'
  #' @returns data frame - Pre-processed
  #' Key data
  
  # Read in data and remove columns
  KEY_data <- read.csv(path) %>%
    select(-c("Key.6.WHEELCHR", "Key.8.EMG.XFER", "Key..",
              "Key.B.BIKE", "Key.C.CHANGE")) %>%
    mutate(Route = as.numeric(Route)) %>%
    rename(Route.Number = Route, Trip.Number = Trip) %>%
    filter(!is.na(Route.Number))
  
  # Convert to numeric
  numeric_cols <- c("Preset",
                    colnames(KEY_data)[grepl("Key", colnames(KEY_data))])
  KEY_data[, numeric_cols] <- lapply(numeric_cols,
                                     function(x) as.numeric(KEY_data[, x]))
  
  # Pivot to separate by type
  KEY_data <- KEY_data %>%
    pivot_longer(cols = all_of(numeric_cols),
                 names_to = "Product.Type",
                 values_to = "Ride.Count") %>%
    filter(Ride.Count != 0)
  
  # Update time
  # Determine if the column for date
  # and time is formatted with seconds
  if (str_count(KEY_data$Date.and.Time[1], ":") == 1) {
    KEY_data$Time <- as.POSIXct(KEY_data$Date.and.Time,
                                format = "%m/%d/%Y %H:%M",
                                tz = "EST")
  } else {
    KEY_data$Time <- as.POSIXct(KEY_data$Date.and.Time,
                                format = "%m/%d/%Y %H:%M:%S",
                                tz = "EST")
  }
  
  # Add type
  KEY_data$Type <- ifelse(KEY_data$Product.Type == "Key.4.KIDS",
                          "Child",
                          "Adult")
  
  # Add tags
  KEY_data$College <- ifelse(KEY_data$Product.Type == "Key.3.UPASS",
                             "Unknown",
                             "None")
  KEY_data$Low.Income <- ifelse(KEY_data$Product.Type == "Key.A.PCA", 1, 0)
  KEY_data$Day.Pass <- ifelse(KEY_data$Product.Type == "Key.D.DAY.PASS", 1, 0)
  KEY_data$Monthly.Pass <- ifelse(KEY_data$Product.Type == "Key.2.MBTA", 1, 0)
  KEY_data$Week.Pass <- ifelse(KEY_data$Product.Type == "Key.7.ISS.7DAY", 1, 0)
  KEY_data$Ten.Ride.Pass <- ifelse(KEY_data$Product.Type == "Key.5.10.RIDE",
                                   1, 0)
  
  # Select columns
  KEY_data <- KEY_data %>%
    mutate(Source = "KEY",
           Trip.Number = as.numeric(Trip.Number)) %>%
    select(Source, Route.Number, Trip.Number, Time, Ride.Count,
           Type, College, Low.Income, Monthly.Pass, Ten.Ride.Pass)
  return(KEY_data)
}

###################### MERGE ##################################################

prep_full <- function(wave, institution, ttp, key) {
  #' Pre-processes Wave, TTP, and Key data for a
  #' given month, then combines the resulting
  #' data frames.
  #'
  #' @param wave string - File path to the Wave
  #' data
  #' @param institution string - File path to
  #' the institution data
  #' @param ttp string - File path to the TTP
  #' data
  #' @param key string - File path to the Key
  #' data
  #'
  #' @returns data frame - Merged data across
  #' all data sources
  
  # Read in data
  WAVE_data <- prep_wave(wave, institution)
  TTP_data <- prep_ttp(ttp)
  KEY_data <- prep_key(key)
  
  # Combine
  full_data <- bind_rows(WAVE_data, TTP_data, KEY_data)
  
  binary_vars <- c("Low.Income", "Off.Peak", "Eco.Pass",
                   "Transfer", "Monthly.Pass", "Day.Pass",
                   "One.Hour.Pass", "Ten.Ride.Pass", "Week.Pass",
                   "Two.Hour.Pass")
  
  full_data <- full_data %>%
    # Update NA values
    mutate(
      # Replace NA values in Institution.Name with values in College
      Institution.Name = ifelse(is.na(Institution.Name),
                                College,
                                Institution.Name),
      # NA values in Institution either Misc. or College
      Institution = case_when(
        !is.na(Institution) ~ Institution,
        Institution.Name == "None" ~ "Misc. - Other Riders",
        TRUE ~ "College"),
      High.School = ifelse(is.na(High.School), "None", High.School),
      Card.Number = ifelse(is.na(Card.Number), "None", Card.Number)) %>%
    # Set NA values in remaining columns to 0
    mutate_at(binary_vars, ~replace_na(., 0)) %>%
    
    # Add Trip ID
    arrange(Time) %>%
    mutate(Trip.Id = 1:nrow(full_data)) %>%

    # Fill in stop number by next stop
    group_by(Route.Number) %>%
    arrange(Time) %>%
    fill(names(.),.direction = "up") %>%
    na.omit()
  
  # Add days of the week column
  full_data$Day.of.Week <- wday(full_data$Time, label = TRUE)
  
  # Change order of columns in the data set
  full_data <- full_data %>%
    select("Trip.Id", "Time", "Day.of.Week", "Route.Number",
           "Stop.Number", "Trip.Number", "Source", "Ride.Count",
           "Type", "College", "High.School", "Low.Income",
           "Off.Peak", "Eco.Pass", "Transfer", "One.Hour.Pass",
           "Two.Hour.Pass", "Day.Pass", "Ten.Ride.Pass", "Week.Pass",
           "Monthly.Pass", "Institution.Name", "Institution", "Card.Number")
  return(full_data)
}


# DEMO
#setwd("/Users/alice/Dropbox/RIPTA/")
#wave_filename <- "data/Wave_Aug23.csv"
#inst_filename <- "data/Institutional Rides Invoice Report_2023-11-27_12-02-18_20611.csv"
#ttp_filename <- "data/CORRECTED_CashFareBox_TTPs_Aug23.csv"
#key_filename <- "data/CashFareBox_August23.csv"
#full_df <- prep_full(wave_filename, inst_filename, ttp_filename, key_filename)
#write.csv(full_df, "data/MergedData.csv", row.names = FALSE)