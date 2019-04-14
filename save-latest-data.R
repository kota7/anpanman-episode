source("utils.R")

if (!dir.exists("www/data")) dir.create("www/data", recursive=TRUE)

today <- Sys.Date()
csvpath <- sprintf("www/data/%s.csv", strftime(today, "%Y%m%d"))
flog.info("Today is %s", today)
flog.info("Save path is `%s`", csvpath)

flog.info("Obtaining latest data")
x <- latestData()
flog.info("Number of records: %d", nrow(x))

write.csv(x, csvpath, row.names=FALSE)


