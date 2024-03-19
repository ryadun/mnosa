# Governmental Funds
# Debt | Indebtedness
# Enterprise Funds | Enterprises
# Fund Balance
# Employee Data | Employees

# unit tests for file consistency

# sheet locations are not consistent: can't use index
# names are also inconsistent
get_sheet_names <- function(filename) {
  excel_sheets(filename) %>%
    paste(collapse = " | ")
}

for (f in files) {
  cat(f, ":", get_sheet_names(f), fill = TRUE)
}

# Number of columns are not consistent
get_file_width <- function(filename) {
  read_excel(filename) %>%
    ncol()
}

for (f in files) {
  cat(f, ":", get_file_width(f), fill = TRUE)
}

# confirm importing inconsistent columns uses names and not column location

# --- iterate this section

num_cleared <- 14
clearedfiles <- files[1:num_cleared]

test_govfunds <- import_files(clearedfiles, sheetname = "Governmental Funds")

compare_file <- import_files(files[num_cleared+1])
cat("Testing against file", files[num_cleared+1])

# Mismatched variables
setdiff(colnames(test_govfunds), colnames(compare_file)) # columns not in use
setdiff(colnames(compare_file), colnames(test_govfunds)) # new columns

# attempt merge
ncol(test_govfunds)

test_govfunds %>%
  bind_rows(compare_file) %>%
  ncol()

# --- Repeat section above


# best way to do this, continue to create an aggregate set one at a time and find differences to the next file in line

# find column names
colnames(test_govfunds)[grepl("expend", colnames(test_govfunds))]

