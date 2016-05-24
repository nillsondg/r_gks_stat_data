library(XML)
# init
host_name <- 'http://www.gks.ru'

# all stat years and path to it db
years_v <-    c(2003, 2004, 2005, 2006,
              2007, 2008, 2009, 2010,
              2011, 2012, 2013, 2014,
              2015)
db_names <- c('B03_14', 'B04_14', 'B05_14p', 'B06_14p',
              'B07_14p', 'B08_14p', 'B09_14p', 'B10_14p',
              'B11_14p', 'B12_14p', 'B13_14p', 'B14_14p',
              'B15_14p')
years <- data.frame(years_v, db_names)

#load doc with stat data and convert containing table into dataframe
loadGKSData <- function(ref){
  url <- paste(host_name, ref, sep = "")
  doc <- htmlParse(url, encoding = "Windows-1251")
  tables <- readHTMLTable(doc, trim = TRUE, as.data.frame = TRUE, which = 1)
}

#dialog with user and return reference to needed stat doc
getGKSDataRef <- function(){
  path <- '/bgd/regl/'
  params <- '/?List&Id='
  id <- -1
  year <- readline(prompt = paste("Введите год от", years$years_v[1],
                                "до", tail(years$years_v, n = 1), " "))
  db_name <- years$db_names[years_v == year]
  # go through tree until we got a link to doc instead of id in stat db
  while(TRUE){
    url <- paste(host_name, path, db_name, params, id, sep = '')
    xml <- xmlTreeParse(url, useInternalNodes = T)
    names <- xpathSApply(xml, "//name", xmlValue)
    refs <- xpathSApply(xml, "//ref", xmlValue)
    for(i in 1:length(names))
      print(paste(i, names[i]))
    num <- readline(prompt = "Введите номер ")
    ref <- refs[as.numeric(num)]
    if(substr(ref, 1, 1) != "?")
      return(ref)
    id <- substr(ref, 2, nchar(ref))
  }
}
