setwd("C:\\Users") # set working dir 
data <- read.csv("OECD_IO2021_country.csv") # add country name to load the IOT
conversion <- read.csv("Conversion_rate.csv") # load the conversion rates

################################################################
## Required libraries 
library(tidyverse)
################################################################

## Function computing import-adjusted FD components from the OECD csv. IOT
calculate_F_imp_adj <- function(year, country) 
{
  data_filtered <- data %>%
    filter(Country == country) %>% 
    filter(Time == year) %>% # Filter the data for the specified year
    unite(From_Industry, c("ROW", "From.industry...sector"), sep = " ") %>% # Merging columns 
    unite(To_Industry, c("COL", "To.industry...sector"), sep = " ") %>% # Merging columns 
    select(From_Industry, To_Industry, Value) # Removing redundant columns  
  
  data_pivoted <- data_filtered %>% 
    pivot_wider(names_from = To_Industry, values_from = Value) # Pivoting raw data set 
  
  country_raw <- data_pivoted[,-1] # Removing non-numerical first column 
  country_raw <- as.matrix(sapply(country_raw, as.numeric)) # Converting data frame into numeric matrix
  dimnames(country_raw)[1] <- data_pivoted[, 1] # Adding the first columns as row names
  
  ## Converting the basic US prices into local currencies 
  multiplier_year <- conversion %>% # Conversion rates from the 2021 OECD Inter-Country Input-Output Tables (ICIOs)
    filter(Country %in% country) %>% 
    select(paste0("X", year)) %>% 
    as.numeric()
  
  country_converted <- country_raw * multiplier_year
  
  ## Reducing the number of final demand expenditure
  # Consumption 
  Consumption1 <- c("HFCE Final consumption expenditure of households", "NPISH Final consumption expenditure of non-profit institutions serving households")
  Consumption <- rowSums(country_converted[, Consumption1])
  IO_country <- cbind(country_converted, Consumption)
  col_indices <- match(Consumption1, colnames(IO_country))
  IO_country <- IO_country[, -col_indices]
  # Investment  
  Investment1 <- c("GFCF Gross Fixed Capital Formation", "INVNT Changes in inventories")
  Investment <- rowSums(IO_country[, Investment1])
  IO_country <- cbind(IO_country , Investment)
  col_indices <- match(Investment1, colnames(IO_country))
  IO_country <- IO_country[, -col_indices]
  #Exports 
  Exports1 <- c("EXPO Exports (cross border)", "CONS_NONRES Direct purchases by non-residents (exports)", "CONS_ABR Direct purchases abroad by residents (imports)")
  Exports <- rowSums(IO_country[, Exports1])
  IO_country <- cbind(IO_country , Exports)
  col_indices <- match(Exports1, colnames(IO_country))
  IO_country <- IO_country[, -col_indices]
  #Public expenditure
  column_index <- which(colnames(IO_country) == "GGFC Final consumption expenditure of general government")
  colnames(IO_country)[column_index] <- "Government spending"
  
  ##Subsetting I-O Table
  # OECD IOT order
  ISIC <- 
    c("Agriculture, hunting, forestry",
      "Fishing and aquaculture",
      "Mining and quarrying, energy producing products",
      "Mining and quarrying, non-energy producing products",
      "Mining support service activities",
      "Food products, beverages and tobacco",
      "Textiles, textile products, leather and footwear",
      "Wood and products of wood and cork",
      "Paper products and printing",
      "Coke and refined petroleum products",
      "Chemical and chemical products",
      "Pharmaceuticals, medicinal chemical and botanical products",
      "Rubber and plastics products",
      "Other non-metallic mineral products",
      "Basic metals",
      "Fabricated metal products",
      "Computer, electronic and optical equipment",
      "Electrical equipment",
      "Machinery and equipment, nec ",
      "Motor vehicles, trailers and semi-trailers",
      "Other transport equipment",
      "Manufacturing nec; repair and installation of machinery and equipment",
      "Electricity, gas, steam and air conditioning supply",
      "Water supply; sewerage, waste management and remediation activities",
      "Construction",
      "Wholesale and retail trade; repair of motor vehicles",
      "Land transport and transport via pipelines",
      "Water transport",
      "Air transport",
      "Warehousing and support activities for transportation",
      "Postal and courier activities",
      "Accommodation and food service activities",
      "Publishing, audiovisual and broadcasting activities",
      "Telecommunications",
      "IT and other information services",
      "Financial and insurance activities",
      "Real estate activities",
      "Professional, scientific and technical activities",
      "Administrative and support services",
      "Public administration and defence; compulsory social security",
      "Education",
      "Human health and social work activities",
      "Arts, entertainment and recreation",
      "Other service activities",
      "Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use")
  
  # Domestic_inter
  pattern_inter <- "^D[0-9]"
  col_indices <- grep(pattern_inter, colnames(IO_country))
  pattern_dom <- "^DOM"
  row_indices_dom <- grep(pattern_dom, rownames(IO_country))
  Zd_raw <- IO_country[row_indices_dom , col_indices]
  new_rownames_Zd <- sub("^\\S+\\s+", "", rownames(Zd_raw)) # removing "DOM" abbreviation
  rownames(Zd_raw) <- new_rownames_Zd
  new_colnames_Zd <- sub("^\\S+\\s+", "", colnames(Zd_raw)) # removing "D[0-9]" abbreviation
  colnames(Zd_raw) <- new_colnames_Zd
  
  ## Import_inter
  pattern_inter <- "^D[0-9]"
  col_indices <- grep(pattern_inter, colnames(IO_country))
  pattern_imp <- "^IMP"
  row_indices_import <- grep(pattern_imp, rownames(IO_country))
  Zm_raw <- IO_country[row_indices_import , col_indices]
  new_rownames_Zm <- sub("^\\S+\\s+", "", rownames(Zm_raw)) # removing "IMP" abbreviation
  rownames(Zm_raw) <- new_rownames_Zm
  new_colnames_Zm <- sub("^\\S+\\s+", "", colnames(Zm_raw)) # removing "D[0-9]" abbreviation
  colnames(Zm_raw) <- new_colnames_Zm
  
  
  ## Aligning the ZM and ZD rows 
  # Zd
  Zd_rownames <- rownames(Zd_raw)
  Zd <- matrix(, nrow = nrow(Zd_raw), ncol = ncol(Zd_raw))
  for (i in 1:length(ISIC)) {
    row_index <- match(ISIC[i], Zd_rownames)
    Zd[i, ] <- Zd_raw[row_index, ]
  }
  rownames(Zd) <- ISIC
  colnames(Zd) <- colnames(Zd_raw)
  match_columns_Zd <- match(colnames(Zd), ISIC)
  Zd <- Zd[, match_columns_Zd]
  
  #Zm 
  Zm_rownames <- rownames(Zm_raw)
  Zm <- matrix(, nrow = nrow(Zm_raw), ncol = ncol(Zm_raw))
  for (i in 1:length(ISIC)) {
    row_index <- match(ISIC[i], Zm_rownames)
    Zm[i, ] <- Zm_raw[row_index, ]
  }
  rownames(Zm) <- ISIC
  colnames(Zm) <- colnames(Zm_raw)
  match_columns_Zm <- match(colnames(Zm), ISIC)
  Zm <- Zm[, match_columns_Zm]
  
  ## Output 
  pattern_inter <- "^D[0-9]"
  col_indices <- grep(pattern_inter, colnames(IO_country))
  row_indices_output <- grep(pattern = "^VAL|^OUTPUT", rownames(IO_country))
  output <- IO_country[row_indices_output, col_indices]
  new_rownames_output <- sub("^\\S+\\s+", "", rownames(output))
  rownames(output) <- new_rownames_output
  new_colnames_output <- sub("^\\S+\\s+", "", colnames(output))
  colnames(output) <- new_colnames_output
  match_columns_output <- match(colnames(output), ISIC)
  output <- output[, match_columns_output]
  
  ## Domestic_FD (Final Demand)
  col_indices_FD <- grep(pattern = "Government spending|Consumption|Investment|Export", colnames(IO_country))
  pattern_dom <- "^DOM"
  row_indices_dom <- grep(pattern_dom, rownames(IO_country))
  Fd_raw <- IO_country[row_indices_dom , col_indices_FD]
  new_rownames_Fd <- sub("^\\S+\\s+", "", rownames(Fd_raw)) # removing "DOM" abbreviation
  rownames(Fd_raw) <- new_rownames_Fd
  
  Fd_rownames <- rownames(Fd_raw)
  Fd <- matrix(, nrow = nrow(Fd_raw), ncol = ncol(Fd_raw))
  for (i in 1:length(ISIC)) {
    row_index <- match(ISIC[i], Fd_rownames)
    Fd[i, ] <- Fd_raw[row_index, ]
  }
  rownames(Fd) <- ISIC
  colnames(Fd) <- colnames(Fd_raw)
  order <- c("Consumption", "Investment", "Government spending", "Exports")
  Fd <- Fd[, order]
  
  ## Import_FD (Final Demand)
  col_indices_FD <- grep(pattern = "Government spending|Consumption|Investment|Export", colnames(IO_country))
  pattern_imp <- "^IMP"
  row_indices_import <- grep(pattern_imp, rownames(IO_country))
  Fm_raw <- IO_country[row_indices_import , col_indices_FD]
  new_rownames_Fm <- sub("^\\S+\\s+", "", rownames(Fm_raw)) # removing "IMP" abbreviation
  rownames(Fm_raw) <- new_rownames_Fm
  
  Fm_rownames <- rownames(Fm_raw)
  Fm <- matrix(, nrow = nrow(Fm_raw), ncol = ncol(Fm_raw))
  for (i in 1:length(ISIC)) {
    row_index <- match(ISIC[i], Fm_rownames)
    Fm[i, ] <- Fm_raw[row_index, ]
  }
  rownames(Fm) <- ISIC
  colnames(Fm) <- colnames(Fm_raw)
  order <- c("Consumption", "Investment", "Government spending", "Exports")
  Fm <- Fm[, order]
  
  
  ##Transforming the matrices
  # Value produced per one unit of output
  row_indice_Y <- grep(pattern = "^Output", rownames(output))
  Y <- as.vector(output[row_indice_Y, ])
  Am <- matrix(0, nrow = nrow(Zm), ncol = ncol(Zm))
  for (j in 1:ncol(Zm)) {
    Am[, j] <- Zm[, j] / Y[j]
  }
  row.names(Am) <- row.names(Zm)
  colnames(Am) <- colnames(Zm)
  
  # Ad 
  Ad <- matrix(0, nrow = nrow(Zd), ncol = ncol(Zd))
  for (j in 1:ncol(Zd)) {
    Ad[, j] <- Zd[, j] / Y[j]
  }
  row.names(Ad) <- row.names(Zd)
  colnames(Ad) <- colnames(Zd)
  
  ##Leontief Inverse
  # Compute the total output in each sector
  total_output <- rowSums(Zd) + rowSums(Zm)
  
  # Compute the Leontief inverse matrix
  I <- diag(length(total_output)) # Identity matrix
  L <- solve(I - Ad)
  X <- L %*% Fd # Leontief inverse matrix
  
  # Imports of intermediate inputs from sector i induced by domestically produced products and services satisfying the demand component k
  M_ind <- Am %*% X 
  
  ## Total imports
  M_tot <- M_ind + Fm
  
  ## Total Final expenditures linked to each demand component
  F_tot <- Fd + Fm
  
  # Calculate the import-adjusted FD components
  F_imp <- round(Fd - M_ind, 4) 
  
  # Add columns sums 
  sumtotal <- colSums(F_imp)
  #F_imp_adj <- rbind(F_imp, sumtotal) ## If all 45 industries are required
  F_imp_adj <- rbind(sumtotal)
  Output <- rowSums(F_imp_adj)
  F_imp_adj <- cbind(F_imp_adj, Output)
  return(F_imp_adj)
}

###################################################################################
## Creating data frame with import-adjusted components and output from 1995 to 2018
calculate_F_imp_adj_loop <- function() {
result_df <- data.frame(Year = numeric(), 
                        Consumption = numeric(), 
                        Investment = numeric(),
                        Government_spending = numeric(), 
                        Exports = numeric(),
                        Output = numeric(), 
                        stringsAsFactors = FALSE)

# Loop over the years from 1995 to 2018
for (year in 1995:2018) {
  # Call the original function to calculate F_imp_adj for the current year
  F_imp_adj <- calculate_F_imp_adj(year, "") # add country 
  
  # Create a data frame row for the current year
  row <- data.frame(Year = year, 
                    Consumption = F_imp_adj[1], 
                    Investment = F_imp_adj[2], 
                    Government_spending = F_imp_adj[3],
                    Exports = F_imp_adj[4],
                    Output = F_imp_adj[5],
                    stringsAsFactors = FALSE)
  
  # Append the row to the result data frame
  result_df <- rbind(result_df, row)
}

return(result_df)
}

IM_adj_results <- calculate_F_imp_adj_loop()

################################################################################
## Calculating growth contributions 
IM_adj_country <- IM_adj_results %>% 
  mutate(GrowthRate_C = ((Consumption - lag(Consumption)) / lag(Output))*100,
         GrowthRate_I = ((Investment - lag(Investment)) / lag(Output))*100,
         GrowthRate_G = ((Government_spending - lag(Government_spending)) / lag(Output))*100,
         GrowthRate_EX = ((Exports - lag(Exports)) / lag(Output))*100,
         GrowthRate_Y = ((Output - lag(Output)) / lag(Output))*100) %>%
  mutate(Relative_share_C = (GrowthRate_C / GrowthRate_Y)*100,
         Relative_share_I = (GrowthRate_I / GrowthRate_Y)*100,
         Relative_share_G = (GrowthRate_G / GrowthRate_Y)*100,
         Relative_share_EX = (GrowthRate_EX / GrowthRate_Y)*100) %>% 
  round(2)

# Save the final_df dataframe as a CSV file in the working directory
write.csv(IM_adj_country, file = "IM_adj_Spain.csv", row.names = TRUE)

