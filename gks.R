library(XML)
library(xml2)
library(tools)
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
  ext <- file_ext(url)
  if(ext == "doc"){
    print("Неподдерживаемый тип источника")
    #getTableFromDoc(url)
  }
  if(ext == "docx"){
    getTableFromDoc(url)
  }else if(ext == "htm"){
    getTableFromHtm(url)
  }else
    print("Неподдерживаемый тип источника")
  
}

toLocalEncoding <- function(x, sep="\t", quote=FALSE, encoding="utf-8"){
  rawtsv <- tempfile()
  write.table(x, file=rawtsv, sep=sep, quote=quote)
  result <- read.table(file(rawtsv, encoding=encoding), sep=sep, quote=quote)
  unlink(rawtsv)
  result
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
    Encoding(names) <- "UTF-8" 
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

getTableFromHtm <- function(doc) {
  doc <- htmlParse(url)
  if(length(xpathSApply(doc,"//table", xmlValue)) == 0){
    print("Нет таблицы в источнике")
    return()
  }
  
  data <- readHTMLTable(doc, trim = TRUE, which = 1, stringsAsFactors = FALSE, as.data.frame = TRUE, encoding="Windows-1251")
  #res <- gsub("&nbsp;"," ", tables)
  #res <- toLocalEncoding(res)
}

##get tables from doc
getTableFromDoc <- function(word_doc) {
  
  tmpd <- tempdir()
  tmpf <- tempfile(tmpdir=tmpd, fileext=".zip")
  
  file.copy(word_doc, tmpf)
  unzip(tmpf, exdir=sprintf("%s/docdata", tmpd))
  
  doc <- read_xml(sprintf("%s/docdata/word/document.xml", tmpd))
  
  unlink(tmpf)
  unlink(sprintf("%s/docdata", tmpd), recursive=TRUE)
  
  ns <- xml_ns(doc)
  
  tbls <- xml_find_all(doc, ".//w:tbl", ns=ns)
  
  lapply(tbls, function(tbl) {
    
    cells <- xml_find_all(tbl, "./w:tr/w:tc", ns=ns)
    rows <- xml_find_all(tbl, "./w:tr", ns=ns)
    dat <- data.frame(matrix(xml_text(cells), 
                             ncol=(length(cells)/length(rows)), 
                             byrow=TRUE), 
                      stringsAsFactors=FALSE)
    colnames(dat) <- dat[1,]
    dat <- dat[-1,]
    rownames(dat) <- NULL
    dat
    
  })
  data <- dat
}
