library(data.table)
library(stringr)

# convert all to long to be combined (as many have different number of columns)
dt_lng <- data.table(Source = character(),
                     Commodity = character(),
                     Country = character(),
                     Type = character(),
                     Meas_Unit = character(),
                     Meas_Name = character(),
                     Value = character())

files <- fread("data/USGS/mineral_file_names_before_2025.csv")

for (i in 1:nrow(files)) {
  fname <- files[i]$filename
  print(fname)
  
  # fix file naming errors
  fname <- str_replace(fname, "-world", "_world")
  fname <- str_replace(fname, "mgcom_", "mgcomp_")
  fname <- str_replace(fname, "mcs2023-zirco_", "mcs2023-zirco-hafni_")
  fname <- str_replace(fname, "mcs2024-bismu_world", "MCS2024-bismu_world")
  if (substr(fname, 8, 8) == "_") {
    fname <- str_replace(fname, "_", "-")
  } 
  
  # find and read file
  fpath <- list.files("data/USGS", pattern = fname, full.names = T, recursive = T)
  d <- fread(fpath, header = T, colClasses = c("character"))
  
  # fix - change column Form to Type if exists
  if ("Form" %in% colnames(d)) setnames(d, "Form", "Type")
  # change column 'Mine production, fluorspar' to Type
  if ("Mine production, fluorspar" %in% colnames(d)) setnames(d, "Mine production, fluorspar", "Type")
  
  # add Commodity column
  d[, Commodity := files[i]$mineral]
  
  # add Meas_Unit column
  d[, Meas_Unit := files[i]$unit]
  
  # set source column (which sometimes doesn't exist)
  d$Source <- toupper(substr(basename(fpath), 1, 7))
  
  m <- melt(d, 
            id.vars = c("Source", "Commodity", "Country", "Type", "Meas_Unit"), 
            variable.name = "Meas_Name",
            value.name = "Value",
            variable.factor = F)
  
  # remove NAs
  m <- m[!is.na(Value),]
  # remove missing countries
  m <- m[Country != "",]
  
  dt_lng <- rbindlist(list(dt_lng, m))
}

# finalize 2022-2024
dt2224 <- dt_lng[, .(Source, Commodity, Country, Type, Meas_Unit, Meas_Name, Value)]
colnames(dt2224) <- toupper(colnames(dt2224))


# melt 2025 data
# load 2025 file
d25 <- fread("data/USGS/usgs_mineral_2025/MCS2025_World_Data.csv")
# fix Sierra Leone Titanium
d25[toupper(COMMODITY) %like% "TITAN" & toupper(COUNTRY) %like% "SIERRA", UNIT_MEAS := "metric tons"]

# melt
d25m <- melt(d25, 
             id.vars = c("SOURCE", "COMMODITY", "COUNTRY", "TYPE", "UNIT_MEAS"), 
             variable.name = "MEAS_NAME",
             value.name = "VALUE")
setnames(d25m, "UNIT_MEAS", "MEAS_UNIT")
d25m[, MEAS_UNIT := toupper(MEAS_UNIT)]

# combine 2022-2025
dt2225 <- rbindlist(list(dt2224, d25m))

# convert factor to character
dt2225$MEAS_NAME <- as.character(dt2225$MEAS_NAME)

# Add MEAS_TYPE, MEAS_YR, ESTIMATED, and SRC_YR columns
dt2225[, SRC_YR := substr(SOURCE, 4, 7)]
dt2225[toupper(MEAS_NAME) %like% "^RESERV", MEAS_TYPE := "Reserve"]
dt2225[toupper(MEAS_NAME) %like% "^CAP", MEAS_TYPE := "Capacity"]
dt2225[toupper(MEAS_NAME) %like% "^PROD", MEAS_TYPE := "Production"]
dt2225[!is.na(MEAS_UNIT), MEAS_YR :=  substr(MEAS_NAME, nchar(MEAS_NAME)-3, nchar(MEAS_NAME))]
dt2225[!is.na(MEAS_UNIT) & toupper(MEAS_NAME) %like% "RESERV", MEAS_YR := as.integer(SRC_YR) - 1]
dt2225[toupper(MEAS_NAME) %like% "EST", ESTIMATED := "Y"]


# remove leading and following whitespace
dt2225[, VALUE := trimws(VALUE)]

# remove leading "e"
dt2225[VALUE %like% "^e[0-9]", VALUE := substr(VALUE, 2, nchar(VALUE))]

# remove empty notes
dt2225 <- dt2225[!(is.na(MEAS_UNIT) & VALUE == ""),]

# remove meas_name = "V9" - from Barite file
dt2225 <- dt2225[!MEAS_NAME %like% "^V"]

# notes don't have measurements
dt2225[toupper(MEAS_NAME) %like% "NOTE", MEAS_UNIT := NA]
dt2225[toupper(MEAS_NAME) %like% "NOTE", MEAS_YR := NA]

# remove gt and lt symbols in VALUE
dt2225[VALUE %like% "^>", VALUE := substr(VALUE, 2, nchar(VALUE))]
dt2225[VALUE %like% "^<", VALUE := substr(VALUE, 2, nchar(VALUE))]

# substitute and for & in COMMODITY
dt2225[COMMODITY %like% "&", COMMODITY := str_replace_all(COMMODITY, "&", "and")]

# remove where VALUE equals "XX" (meaning invalid)
dt2225 <- dt2225[!VALUE=="XX"]

# remove if NA or blank in VALUE
dt2225 <- dt2225[!is.na(VALUE) & VALUE != ""]

# capitalize COMMODITY, COUNTRY, and MEAS_TYPE
dt2225[, COMMODITY := toupper(COMMODITY)]
dt2225[, COUNTRY := toupper(COUNTRY)]
dt2225[, MEAS_TYPE := toupper(MEAS_TYPE)]

# order columns
dt2225 <- dt2225[, .(SOURCE, COMMODITY, COUNTRY, TYPE, MEAS_UNIT, MEAS_NAME, MEAS_TYPE, SRC_YR, MEAS_YR, ESTIMATED, VALUE)]

# add space before open parentheses
dt2225[COMMODITY %like% "[A-Z]\\(", COMMODITY := str_replace(COMMODITY, "([A-Z])\\(", "\\1 (")]

# double dash to single and add space before and after
dt2225[COMMODITY %like% "\\)--[A-Z]", COMMODITY := str_replace(COMMODITY, "--", " - ")]

# harmonize commodity names to 2025 names
dt2225[COMMODITY == "BAUXITE AND ALUMINA", COMMODITY := "BAUXITE"]
dt2225[COMMODITY == "NIOBIUM (COLUMBIUM)", COMMODITY := "NIOBIUM"]
dt2225[COMMODITY == "MICA", COMMODITY := "MICA (NATURAL)"]
dt2225[COMMODITY == "GRAPHITE (NATURAL)", COMMODITY := "GRAPHITE"]
dt2225[COMMODITY == "FELDSPAR AND NEPHELINE SYENITE", COMMODITY := "FELDSPAR"]
dt2225[COMMODITY == "TALC AND PYROPHYLLITE", COMMODITY := "TALC, CRUDE"]
dt2225[COMMODITY == "ABRASIVES (MANUFACTURED)", COMMODITY := "ABRASIVES"]


# write final combined file
fwrite(dt2225, "data/USGS/world_mineral_commodity_reports_2022-2025.csv")

