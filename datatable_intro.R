library(data.table) #https://cran.r-project.org/web/packages/data.table/vignettes
library(ggplot2)
library(testit)
rm(list=ls())

# load in data
flights <- fread("data/flights14.csv")

# display data structure
head(flights, 5)

# display the distance of all routes from origin airport to dest served by all
origin.routes <- flights[, .N, .(origin, dest)][, .N, .(origin)][order(-N)]
common.routes <- flights[,.N,.(origin, dest)][, .N, .(dest)][N == 3]
route.dists <- flights[common.routes, .(distance = mean(distance)), .(origin, dest), on = "dest"][order(-distance)]

assert("Same number of common destinations", {
  length(unique(route.dists[common.routes, on = "dest"]$dest)) == dim(common.routes)[1]
})

route.order <- unique(route.dists$dest)

(ggplot(route.dists, aes(x = dest, y = distance, fill = distance)) 
  + geom_col() 
  + facet_wrap(~ origin)
  + scale_x_discrete(limits = route.order, labels = list())
  )

# look at distance verses delay time in air by airline
flights$tot_delay <- flights$arr_delay - flights$dep_delay
(ggplot(flights, aes(x = distance, y = tot_delay))
  + geom_point(mapping = aes(color = carrier))
  + geom_smooth()
 )

# look at share of flights by origin airport by carrier
tot.flights.origin <- (flights[, .N, .(origin, carrier)]
                       [, .(tot_flights = sum(N)), .(origin)])
shares.flights.origin <- (flights
                          [tot.flights.origin, on = "origin"]
                          [,.(share = .N / mean(tot_flights)), by = .(origin, carrier)]
                          )

(ggplot(shares.flights.origin, aes(x = carrier, y = share, fill = share))
  + geom_col()
  + facet_wrap(~ origin)
  + scale_y_continuous(labels = function(x) paste0(x*100, "%"))
  + coord_flip()
  + theme_bw()
  + labs(title = "Share of flights by airline",
         y = "Flight Share (%)",
         x = "Airline")
)

# delay by carrier
(ggplot(flights, aes(x = carrier, y = dep_delay))
  + geom_boxplot()
  + theme_bw()
  + facet_wrap(~ origin)
  + labs(title = "Delay by carrier",
         x = "Airline",
         y = "Average departure delay (min)")
  )

# airtime by distance
(ggplot(flights, aes(x = distance, y = air_time))
  + geom_point(mapping = aes(color = carrier))
  + geom_smooth()
  + theme_bw(bas)
  + labs(title = "On average flights take about 100 min per 1000 miles",
         y = "Flight Time (min)",
         x = "Distance (miles)")
)

lm.fit.1 <- lm(air_time ~ distance, data = flights)
lm.fit.2 <- lm(air_time ~ distance + dep_delay, data = flights)
lm.fit.3 <- lm(air_time ~ distance + dep_delay + carrier, data = flights)
all.lm <- list(lm.fit.1, lm.fit.2, lm.fit.3)
do.call(anova, all.lm)
  
# airtime by route by airline
threshold <- 4
competitive.routes <- flights[, .(n_carriers = length(unique(carrier))), .(origin, dest)][n_carriers > threshold]
routes <- (cbind(
  competitive.routes,
  route_id = seq_len(nrow(competitive.routes)))
)

flights[, speed := distance/air_time*60]
speeds <- flights[routes, on = c("origin", "dest"), nomatch = NULL][, .(speed = mean(speed)), .(route_id, carrier)]

(ggplot(speeds, aes(x = carrier, y = speed))
  + geom_col()
  + facet_wrap(~ route_id)
  + theme_bw()
)

# number of airlines servicing each route
interim <- flights[, .(n_carriers = length(unique(carrier))), .(origin, dest)]
competition <- cbind(interim, id = seq_len(nrow(interim)))
competition.count <- flights[, 1, .(origin, dest, carrier)][competition, .(x.origin, x.dest, id = i.id), on = c("origin", "dest")]

(ggplot(interim[,.N, n_carriers][,.(n_carriers, N = N/sum(N))], aes(x = n_carriers, y = N, fill = N))
  + geom_col()
  + theme_bw()
  + labs(title = "~60% of routes are only serviced by a single airline",
         subtitle = "Competition distribution",
         x = "Number of competitors",
         y = "% of routes")
  + scale_y_continuous(labels = function(x) paste0(x*100, "%"))
  + geom_text(aes(label=paste0(round(100*N,0),"%")), position=position_dodge(width=0.9), vjust=-0.25, size=3)
  + theme(legend.position = "none")
)

# segment out airlines so you see big/med/small and how they are distributed in terms of routes
flights[,.N,.(carrier, origin, dest)][, .(n_flights = sum(N), n_routes = .N, weekly_flights_per_route = sum(N)/.N/10/4), .(carrier)][order(-n_routes)]

# time series view
flights$date <- as.Date(paste(flights$year, flights$month, flights$day, sep="-"), format = "%Y-%m-%d")
flights$weekday <- weekdays(flights$date)

# relative share of daily flights doesn't vary much throughout the year
(ggplot(flights[,.N, .(carrier, date)], aes(x = date, y = N))
  + geom_area(aes(group = carrier, fill = carrier), position = "fill")
  + theme_bw()
  + labs(title = "Daily flights by airline")
  )

# flight volume drops industry wide on weekends
(ggplot(flights[,.N, .(weekday, carrier)], aes(x = weekday, y = N))
  + geom_col(aes(group = carrier, fill = carrier))
  + geom_hline(yintercept = 35000)
  + geom_hline(yintercept = 30000)
  + theme_bw()
  + labs(title = "Flight volume remain stable during the work\nweek but drops noticeably during the weekends",
         subtitle = "Weekly flight profile by airline", 
         x = NULL)
  + scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
)

# smaller airlines also tend to operate on regional monopolies
flight.airports <- c(unique(flights$dest), unique(flights$origin))

airports <- read.delim("data/GlobalAirportDatabase.txt", header = FALSE, sep = ":")
airports <- data.table(airports)

setkey(airports, V2)
us.airports <- airports[.(flight.airports)]
lims <- us.airports[!is.na(V15) & !is.na(V16), .(min(V15), max(V15), min(V16), max(V16))]
lims <- as.matrix(lims)

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")

(ggplot(data = world)
  + geom_sf(color = "black", fill = "lightgrey")
  + geom_point(data = us.airports, aes(x = V16, y = V15))
  + coord_sf(xlim = c(-125.0, -65.0), ylim = c(24, 50), expand = FALSE)
  + theme_bw()
)

# using keys for fast subset selection
is.null(key(flights))
setkey(flights, carrier, origin)

# allows for "in" comparisons with provided vector
flights[.(c("AA", "B6"), "JFK")]

# just look at second key by allowing first to be anything
flights[.(unique(carrier), "JFK")]

# get all 'hours' in flights
flights[, sort(unique(hour))]
# replace some column values based on subset
setkey(flights, hour)
flights[.(24), c("hour"):=.(0)]
# flights[.(24L), hour := 0L, on = "hour"] alternative way using secondary indicies
key(flights) # note this is NULL now since the prev key column was mutated

# look at flight volume by hour for the various carriers by origin
hourly.flights <- flights[, .N, by = .(carrier, origin, hour)]
periods <- c(0, 6, 12, 18, 24)
assert("Sequence is same as auto gen", all(periods == seq(0, 24, 6)))
hourly.flights[, period := cut(hour, breaks = c(0, 6, 12, 18, 24), labels = 1:4, right = FALSE)]
periodly.flights <- hourly.flights[, j = .(N = sum(N)), .(carrier, origin, period)]

(ggplot(hourly.flights, aes(x = hour, y = N))
  + geom_col(aes(fill = carrier))
  + facet_wrap(~ origin)
  )

(ggplot(periodly.flights, aes(x = period, y = N))
  + geom_col(aes(fill = carrier))
  + facet_wrap(~ origin)
  + scale_x_discrete(labels = c("Dawn", "Morning", "Afternoon", "Night"))
)

(ggplot(hourly.flights, aes(x = hour, y = N))
  + geom_col(aes(fill = carrier))
  + facet_wrap(~ carrier)
)

# normalize flights in period by total daily flights
flight.time.shares <- periodly.flights[
  periodly.flights[,.(N = sum(N)), .(carrier, origin)], 
  .(carrier, origin, period, share = x.N / i.N), 
  on = .(carrier, origin)]

(ggplot(flight.time.shares[origin=="JFK"], aes(x = period, y = share))
  + geom_col(aes(fill = share))
  + facet_wrap(~ carrier)
  + scale_x_discrete(labels = c("Dawn", "Morning", "Afternoon", "Night"))
  + theme_bw()
  + labs(title = "United Airlines are the only airline originating at\nJFK running flights between midnight and 6am",
         subtitle = "Share of daily flights by time of day",
         x = NULL,
         y = "Share of daily flights")
)

# adding in multiple columns using .SD
in_cols  = c("dep_delay", "arr_delay")
out_cols = c("max_dep_delay", "max_arr_delay")
flights[, c(out_cols) := lapply(.SD, max), by = month, .SDcols = in_cols]

# grouped regression i.e delays ~ hour by carrier and origin
flights[, c("carrier", "origin", "dest") := lapply(.SD, factor), .SDcols = c("carrier", "origin", "dest")]
betas <- flights[, .(h_coef = coef(lm(arr_delay ~ hour))['hour']),
                 by = .(carrier, origin)][ , hist(h_coef, 10L, las = 1L,
                                          xlab = 'Fitted coefficient on hours')]

flights[, (lapply(lapply(.SD, unique), length)), .SDcols = c("hour", "origin"),
        by = .(carrier)]

# long to short and back methods
s1 <- "family_id age_mother dob_child1 dob_child2 dob_child3
1         30 1998-11-26 2000-01-29         NA
2         27 1996-06-22         NA         NA
3         26 2002-07-11 2004-04-05 2007-09-02
4         32 2004-10-10 2009-08-27 2012-07-21
5         29 2000-12-05 2005-02-28         NA"
DT <- fread(s1)

DT.m1 <- melt(DT, 
     id.vars = c("family_id", "age_mother"), 
     measure.vars = c("dob_child1", "dob_child2", "dob_child3"),
     variable.name = "child", 
     value.name = "dob")

DT.back <- dcast(DT.m1, family_id + age_mother ~ child, value.var = "dob")

identical(DT.m1[, .(family_id, child, V2 = !is.na(dob))][order(family_id)], DT.m1[, .(child, V2 = !is.na(dob)), .(family_id)])
?which


# baseball example

Teams <- fread('data/Teams.csv', header = TRUE)
Pitching <- fread('data/Pitching.csv', header = TRUE)

start <- 2000
Pitching[.(start:max(yearID)), on = "yearID"][,.(avg_hr = mean(HR)), .(yearID)]
Pitching[yearID %in% start:max(yearID), mean(HR), .(yearID)]

yearly.pitching.avg <- Pitching[, lapply(.SD, function(x){mean(x[!is.na(x)])}), by = .(yearID), .SDcols = c("HR", "ERA")]

# look at best performance year by team
a <- Teams[, .SD[which.max(-Rank)], 
      by = .(teamID)][order(teamID)]

b <- Teams[order(Rank)][, head(.SD,1), .(teamID)][order(teamID)]
identical(a, b)
Teams[.("ANA"), .(yearID, Rank), on = "teamID"]

team_idx = grep('team', names(Teams), value = TRUE)
Teams[, (team_idx) := lapply(.SD, factor), .SDcols = team_idx]

# combinations of controls for reg
extra_var = c('yearID', 'teamID', 'G', 'L')
models = unlist(
  lapply(0L:length(extra_var), combn, x = extra_var, simplify = FALSE),
  recursive = FALSE
)

coef <- sapply(models, function(rhs){
  lm.fit <- Pitching[, lm(ERA ~ ., data = .SD), .SDcols = c("W", rhs)] # response doesn't need to be in .SDcols
  est <- coef(lm.fit)['W']
  se <- sqrt(diag(vcov(lm.fit)))[2]
  c("est" = est, "se" = se)
})

model.results <- transpose(data.frame(coef))
names(model.results) <- c("est", "SE")
model.results$spec = sapply(models, paste0, collapse = "/")

(ggplot(model.results, aes(x = spec, y = est))
  + geom_pointrange(aes(ymax = est+1.96*SE, ymin = est-1.96*SE))
)

# Overall coefficient for comparison
overall_coef = Pitching[ , coef(lm(ERA ~ W))['W']]
# use the .N > 20 filter to exclude teams with few observations
Pitching[ , if (.N > 20L) .(w_coef = coef(lm(ERA ~ W))['W']), by = teamID
          ][ , hist(w_coef, 20L, las = 1L,
                    xlab = 'Fitted Coefficient on W',
                    ylab = 'Number of Teams', col = 'darkgreen',
                    main = 'Team-Level Distribution\nWin Coefficients on ERA')]
abline(v = overall_coef, lty = 2L, col = 'red')


# to exclude pitchers with exceptional performance in a few games,
#   subset first; then define rank of pitchers within their team each year
#   conditional join
Pitching[G > 5, rank_in_team := frank(ERA), by = .(teamID, yearID)] # NA for those rows G <= 5
Pitching[rank_in_team == 1, team_performance :=
           Teams[.SD, Rank, on = c('teamID', 'yearID')]] # join is conditional on rank_in_team == 1

?data.table
