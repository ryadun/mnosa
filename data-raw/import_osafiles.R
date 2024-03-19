library(tidyverse)
library(readxl)

# source: https://www.osa.state.mn.us/reports-data-analysis/local-government/cities/

OVERWRITE_DATA_DICTIONARIES <- FALSE

files <- paste0("data-raw/ciRed_", sprintf("%0.2d", 8:11), "_data.xls") %>%
  append(paste0("data-raw/cired_", 12:21, "_data.xlsx"))

# List of bad column names to replace for a consistent table structure
REPLACEMENT_NAMES <- tribble(
  ~orig_name, ~replacement_name,
  "gov_entity_id", "entity_id",
  "city_name", "entity_name",
  "gov_entity_name", "entity_name",
  "county", "parent_entity_name",
  "ecenomic_development_capital_outlay", "economic_development_capital_outlay",
  "total_capital_outaly1", "total_capital_outlay",
  "total_capital_outalys", "total_capital_outlay",
  "current_expend_1", "alt_current_expend",
  "tax_cap", "tax_capacity",
  "sa_levy", "special_assessment_levy",
  "ambulance_capital_outaly", "ambulance_capital_outlay"
  ) %>%
  deframe()

# custom function to clean names on import
osa_clean_names <- function(vector_names){
  clean_names <- janitor::make_clean_names(vector_names, case = "snake")
  ifelse(clean_names %in% names(REPLACEMENT_NAMES), REPLACEMENT_NAMES[clean_names], clean_names)
}

osa_safe_import <- function(filename, sheetname = "Governmental Funds") {
  if(sheetname %in% excel_sheets(filename)) {
  read_excel(filename, sheet = sheetname, .name_repair = osa_clean_names) %>%
    mutate(across(any_of(c("gaap_ind", "entity_id")), as.character))
  }
}

import_files <- function(filelist, sheetname = "Governmental Funds") {
  map(filelist, osa_safe_import, sheetname = sheetname) %>%
    bind_rows(.id = "filename")
}

# run single-use unit tests for files
# source("data-raw/test_importosafiles.R")

OSAGovFunds <- import_files(filelist = set_names(files), sheetname = "Governmental Funds")
OSAFundBal <- import_files(filelist = set_names(files), sheetname = "Fund Balance")

# Debt | Indebtedness

debt <- import_files(filelist = set_names(files), sheetname = "Debt")
indebtedness <- import_files(filelist = set_names(files), sheetname = "Indebtedness")

OSADebt <- debt %>%
  bind_rows(indebtedness) %>%
  arrange(financial_year, entity_name)

# Enterprise Funds | Enterprises

entfunds <- import_files(filelist = set_names(files), sheetname = "Enterprise Funds")
enterprises <- import_files(filelist = set_names(files), sheetname = "Enterprises")

OSAEntFunds <- entfunds %>%
  bind_rows(enterprises) %>%
  arrange(financial_year, entity_name)

# Employee Data | Employees

empdata <- import_files(filelist = set_names(files), sheetname = "Employee Data")
employees <- import_files(filelist = set_names(files), sheetname = "Employees")

OSAEmpData <- empdata %>%
  bind_rows(employees) %>%
  arrange(financial_year, entity_name)

if(OVERWRITE_DATA_DICTIONARIES) {
  rdtheme::generate_data_dictionary(OSAGovFunds, "OSAGovFunds.csv")
  rdtheme::generate_data_dictionary(OSADebt, "OSADebt.csv")
  rdtheme::generate_data_dictionary(OSAEmpData, "OSAEmpData.csv")
  rdtheme::generate_data_dictionary(OSAFundBal, "OSAFundBal.csv")
  rdtheme::generate_data_dictionary(OSAEntFunds, "OSAEntFunds.csv")
  cat("Data Dictionaries generated. Use utils::edit(file=\"filename\") to modify\n")
}


usethis::use_data(OSAGovFunds, overwrite = TRUE)
usethis::use_data(OSADebt, overwrite = TRUE)
usethis::use_data(OSAEmpData, overwrite = TRUE)
usethis::use_data(OSAFundBal, overwrite = TRUE)
usethis::use_data(OSAEntFunds, overwrite = TRUE)
