library(rvest)
library(magrittr)
library(stringr)
library(futile.logger)


# CONSTANTS DEFINED ============================================================================== #
# wikipedia page we look at
URL <- "https://ja.wikipedia.org/wiki/\u305d\u308c\u3044\u3051\u21\u30a2\u30f3\u30d1\u30f3\u30de\u30f3\u306e\u30a8\u30d4\u30bd\u30fc\u30c9\u4e00\u89a7"

# columns we need
COLUMNS <- c(
  "\u8a71\u6570",                          # episode number
  "\u653e\u9001\u65e5",                    # broadcast date
  "\u30b5\u30d6\u30bf\u30a4\u30c8\u30eb"   # episode title
)

NUMBERCOL <- grep("\u8a71\u6570", COLUMNS)
DATECOL   <- grep("\u653e\u9001\u65e5", COLUMNS)

TVJAPANMATCH <- data.frame(
  japan=c(
    "2018-04-27"     # dokudami
    ,"2018-05-18"    # book
  ) %>% as.Date(), 
  us=c(
    "2019-03-20"
    ,"2019-04-17"
  ) %>% as.Date(),
  stringsAsFactors=FALSE
)
TVJAPANMATCH <- TVJAPANMATCH[order(TVJAPANMATCH[["japan"]]),]
# ================================================================================================ #



latestData <- function() {
  htm <- scan(URL, what="character", sep="\n") %>%
    paste0(collapse="\n") %>%
    removeFootnotes() %>%
    read_html()
  tbls <- html_nodes(htm, "table.wikitable")
  
  x <- NULL
  i <- 0
  for (tbl in tbls) {
    i <- i + 1
    flog.info("Table %d / %d", i, length(tbls))
    d <- getData(tbl)
    if (is.null(d)) next
    
    x <- if (is.null(x)) d else rbind(x, d)
  }
  x <- cleanData(x)
  
  x[["\u653e\u9001\u65e5 (TV Japan)"]] <- guessTVJapan(x)
  
  x <- x[order(x[[DATECOL]], decreasing=TRUE),]
  x
}


guessTVJapan <- function(x) {
   daydiffs <- difftime(TVJAPANMATCH[["us"]], TVJAPANMATCH[["japan"]], units="days")
   pred <- x[[DATECOL]] + daydiffs[1]  # initial guess
   for (j in 1:length(daydiffs)) {
     flg <- (x[[DATECOL]] >= TVJAPANMATCH[["japan"]][j])
     pred[flg] <- x[[DATECOL]][flg] + daydiffs[j]
   }
   pred
}


removeFootnotes <- function(htm) {
  htm <- str_replace_all(htm, "<sup[^<>]*>.*?</sup>", "")
  htm
}

cleanData <- function(x) {
  x[[NUMBERCOL]] <- str_extract(x[[NUMBERCOL]], "[0-9]+") %>% as.integer()
  
  years  <- str_match(x[[DATECOL]], "([0-9]+)\u5e74")[,2] %>% as.integer() %>% fillNA()
  months <- str_match(x[[DATECOL]], "([0-9]+)\u6708")[,2] %>% as.integer() %>% fillNA()
  days   <- str_match(x[[DATECOL]], "([0-9]+)\u65e5")[,2] %>% as.integer() %>% fillNA()
  x[[DATECOL]] <- as.Date(sprintf("%d-%d-%d", years, months, days))
  x
}


fillNA <- function(x) {
  if (length(x) <= 1) return(x)
  for (i in 2:length(x)) if (is.na(x[i])) x[i] <- x[i-1]
  x
}

getData <- function(tbl) {
  check <- checkColumns(tbl)
  if (!check) return(NULL)  
  
  #d <- html_table(tbl, fill=TRUE)   # use rvest if rowspan bug is fixed
  d <- parseTable(tbl)
  d <- d[COLUMNS] 
  d
}

checkColumns <- function(tbl) {

  row1 <- tbl %>% html_node("tr") %>% as.character()
  check <- sapply(COLUMNS, grepl, row1)
  if (!all(check)) {
    not_exist <- names(check)[!check]
    flog.info("Column not exists: %s", paste0(not_exist, collapse=","))  
    return(FALSE)
  }
  TRUE
}


parseTable <- function(tbl) {
  # write a quick table parser since rvest has rowspan bug
  header <- html_node(tbl, "tr") %>% html_nodes("th") %>% html_text() %>% trimws()
  rows <- html_nodes(tbl, "tr")[-1] 
  nc <- length(header)
  spans <- rep(list(character(0)), nc)
  nr <- length(rows)
  o <- matrix(NA_character_, nrow=nr, ncol=nc)
  
  for (i in seq_along(rows)) {
    row <- rows[[i]]
    cells <- html_nodes(row, "td")
    vals <- html_text(cells) %>% trimws()
    rowspans <- html_attr(cells, "rowspan", "1") %>% as.integer()
    colspans <- html_attr(cells, "colspan", "1") %>% as.integer()
    k <- 1
    for (j in 1:nc) {
      if (length(spans[[j]]) > 0) {
        o[i,j] <- spans[[j]][1]
        spans[[j]] <- spans[[j]][-1]
      } else {
        if (k <= length(cells)) {
          v <- vals[k]
          rs <- rowspans[k]
          cs <- colspans[k]
        } else {
          v <- ""
          rs <- 1
          cs <- 1
        }
        o[i,j] <- v
        
        for (l in j:(j+cs-1)) {
          if (l > nc) break  # handles too many colspans error
          if (l==j) {
            spans[[l]] <- c(spans[[l]], rep(v, rs-1))
          } else {
            spans[[l]] <- c(spans[[l]], rep(v, rs))
          }
        }
        
        k <- k + 1
      }
    }
  }
  
  o <- as.data.frame(o) %>% setNames(header)
  o
}

