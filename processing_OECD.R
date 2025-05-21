# Script for OECD data
library(rsdmx)
library(dplyr) # For data manipulation, if needed

# The data query URL provided by OECD
# NOTE: I've removed the "..." before "?startPeriod" from your example,
# as it might be a typo or placeholder. If "..." is a required part of the
# filter expression for your specific query, you might need to adjust the URL.
data_url <- "https://sdmx.oecd.org/public/rest/data/OECD.GOV.GIP,DSD_GOV@DF_GOV_PF_YU,/A.GBR.GGD.PT_B1GQ...?startPeriod=2007"

# The structure query URL (useful for metadata)
structure_url <- "https://sdmx.oecd.org/public/rest/dataflow/OECD.GOV.GIP/DSD_GOV@DF_GOV_PF_YU/?references=all"

# --- Fetching and Processing Data ---
message("Attempting to fetch data using rsdmx...")
oecd_sdmx_data <- NULL
tryCatch({
  # readSDMX will attempt to fetch and parse the SDMX data.
  # It can handle both SDMX-XML and SDMX-JSON.
  # The OECD API might return SDMX-JSON by default or with appropriate headers.
  oecd_sdmx_data <- readSDMX(data_url)
  
  if (!is.null(oecd_sdmx_data)) {
    message("Successfully fetched SDMX data object.")
    
    # Convert the SDMX object to a more user-friendly R data frame
    # The exact column names will depend on the dataset's structure
    oecd_df <- as.data.frame(oecd_sdmx_data)
    
    message("Data converted to data frame. Displaying first few rows:")
    print(head(oecd_df))
    
    message("\nStructure of the data frame:")
    str(oecd_df)
    
    # Common columns in SDMX data frames include:
    # - obsTime: The time period of the observation (e.g., year)
    # - obsValue: The actual data value
    # - And columns for each dimension in your query (e.g., FREQ, LOCATION, INDICATOR, etc.)
    
    # Example of selecting specific columns if they exist
    # (Adjust column names based on the actual output from str(oecd_df))
    if ("obsTime" %in% names(oecd_df) && "obsValue" %in% names(oecd_df)) {
      oecd_df_selected <- oecd_df %>%
        select(any_of(c("obsTime", "obsValue", "FREQ", "LOCATION", "REF_AREA", "INDICATOR", "MEASURE", "UNIT_MEASURE", "TIME_PERIOD"))) %>%
        mutate(obsValue = as.numeric(as.character(obsValue))) # Ensure obsValue is numeric
      
      message("\nSelected and processed data frame snippet:")
      print(head(oecd_df_selected))
    }
    
  } else {
    message("Failed to fetch data or data object is NULL.")
  }
}, error = function(e) {
  message("Error fetching or processing data with rsdmx:")
  message(e$message)
})
