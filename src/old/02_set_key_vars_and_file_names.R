library(XML)
library(xml2)
library(data.table)
library(stringr)

xml_files <- list.files("data/USGS", pattern = "*.xml", full.names = T, recursive = T)

mineral_files <- data.table(year = character(), mineral = character(), unit = character(), filename = character())

for (f in 1:length(xml_files)) {
  yr <- str_extract(xml_files[f], "[1-9][0-9][0-9][0-9]")
  if (yr > 2024) break
  
  xml <- read_xml(xml_files[f])
  
  attrs <- xml_find_all(xml, ".//attr")
  ents <- xml_find_all(xml, ".//enttyp")
  attrunit <- xml_find_all(xml, ".//attrunit")
  title <- as_list(xml_find_all(xml, ".//title")[1])[[1]][[1]]
  
  attrs_list <- as_list(attrs)
  ents_list <- as_list(ents)
  attrunit_list <- as_list(attrunit)
  
  for (i in 1:length(attrs_list)) {
    a <- attrs_list[[i]]
    if(a$attrlabl[[1]] == "Commodity") {
      print(a$attrdomv$edom$edomv[[1]])
      mineral <- toupper(a$attrdomv$edom$edomv[[1]])
      break
    } else if(str_detect(title, "Data Release")) {
      x <- as_list(xml_find_all(xml, ".//title")[1])[[1]][[1]]
      mineral <- substr(x, str_locate(x, "[A-Z][A-Z]+")[1], str_locate(x, " Data Release")[1]-1)
    } else {
      mineral <- "mineral not found"
    }
  }
  
  for (j in 1:length(ents_list)) {
    e <- ents_list[[j]]$enttypl[[1]]
    if(grepl("world", e, fixed = TRUE)) {
      print(e)
      filename <- e
      break
    } else {
      filename <- "filename not found"
    }
  }
  
  for (k in 1:length(attrunit_list)) {
    u <- trimws(attrunit_list[[k]][[1]][[1]][[1]])
    if(u != "" & !u %like% ",") {
      unit <- toupper(u)
      break
    } else {
      unit <- "unit not found"
    }
  }
  
  if (mineral != "not found" && filename != "not found") {
    dt <- data.table(year = yr, mineral = mineral, unit = unit, filename = filename)
    mineral_files <- rbindlist(list(mineral_files, dt))
  }
}

mineral_file_final <- mineral_files[filename != "filename not found"]
fwrite(mineral_file_final, "data/USGS/mineral_file_names_before_2025.csv")








