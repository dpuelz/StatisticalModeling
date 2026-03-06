# diff in diff example with minimum wage data
minwage <- read.csv("../data/minwage.csv") # load the data
minwageNJ <- subset(minwage, subset = (location != "PA"))
minwagePA <- subset(minwage, subset = (location == "PA"))

# defining outcome of interest
minwageNJ$fullPropAfter <- minwageNJ$fullAfter /
  (minwageNJ$fullAfter + minwageNJ$partAfter)
minwagePA$fullPropAfter <- minwagePA$fullAfter /
  (minwagePA$fullAfter + minwagePA$partAfter)

minwageNJ$fullPropBefore <- minwageNJ$fullBefore /
  (minwageNJ$fullBefore + minwageNJ$partBefore)
minwagePA$fullPropBefore <- minwagePA$fullBefore /
  (minwagePA$fullBefore + minwagePA$partBefore)

# diffindiff estimate
NJdiff <- mean(minwageNJ$fullPropAfter) - mean(minwageNJ$fullPropBefore)
PAdiff <- mean(minwagePA$fullPropAfter) - mean(minwagePA$fullPropBefore)
NJdiff - PAdiff

# adding a binary variable to our data set
minwage_big = rbind(minwage,minwage)
minwage_big$time = rep(NA,nrow(minwage_big))
minwage_big$time[1:nrow(minwage)] = "Before"
minwage_big$time[(nrow(minwage)+1):nrow(minwage_big)] = "After"

minwage_big$wageBefore[which(minwage_big$time=="After")] <- NA
minwage_big$fullBefore[which(minwage_big$time=="After")] <- NA
minwage_big$partBefore[which(minwage_big$time=="After")] <- NA

minwage_big$wageAfter[which(minwage_big$time=="Before")] <- NA
minwage_big$fullAfter[which(minwage_big$time=="Before")] <- NA
minwage_big$partAfter[which(minwage_big$time=="Before")] <- NA

head(minwage_big)
tail(minwage_big)

# fit a special linear model
minwage_big$fullPropBefore = minwage_big$fullBefore /
  (minwage_big$fullBefore + minwage_big$partBefore)

minwage_big$fullPropAfter = minwage_big$fullAfter /
  (minwage_big$fullAfter + minwage_big$partAfter)

fullProp = c(minwage_big$fullPropBefore[!is.na(minwage_big$fullPropBefore)],minwage_big$fullPropAfter[!is.na(minwage_big$fullPropAfter)])
minwage_big$fullProp = fullProp

# fix location variable
minwage_big$location[which(minwage_big$location!="PA")] <- "NJ"

head(minwage_big)

fit = lm(fullProp ~ location + time + location*time, data=minwage_big)
summary(fit)
