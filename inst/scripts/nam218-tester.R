#library(nam218)

nam218="/mnt/ecocast/corecode/R/nam218"
thredds="/mnt/ecocast/corecode/R/thredds"
devtools::load_all(nam218)
devtools::load_all(thredds)


what = 'analysis-historical'
day = '20180704'
ftime = '1200'
ahead = '000'
cat("******** ", what, " ********", "\n")
print(query_nam218(what = what, day = day, ftime = ftime, ahead = ahead))


what = "analysis"
day = '20180704'
ftime = '1200'
ahead = '000'
cat("******** ", what, " ********", "\n")
print(query_nam218(what = what, day = day, ftime = ftime, ahead = ahead))


what = "analysis"
day = format(Sys.Date() - 4, format = "%Y%m%d")
ftime = '1200'
ahead = '000'
cat("******** ", what, " ********", "\n")
print(query_nam218(what = what, day = day, ftime = ftime, ahead = ahead))

what = "forecast"
day = format(Sys.Date() - 4, format = "%Y%m%d")
ftime = '1200'
ahead = '000'
cat("******** ", what, " ********", "\n")
print(query_nam218(what = what, day = day, ftime = ftime, ahead = ahead))

