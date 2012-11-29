outcome = read.csv("outcome-of-care-measures.csv", colClass="character")
heart.attack = as.numeric(outcome[, 11])
heart.failure = as.numeric(outcome[, 17])
pneumonia = as.numeric(outcome[, 23])
par(mfrow=c(3, 1))
hist(pneumonia, main="Pneumonia 30 day Death Rate", xlim=c(10,20))
hist(heart.failure, main="Heart Failure 30 day Death Rate", xlim=c(10, 20))
hist(heart.attack, main="Heart Attack 30 day Death Rate", xlim=c(10, 20))
lines(density(heart.attack, na.rm=T))
abline(v = median(heart.attack, na.rm=T), lwd=3, col="salmon")
par(las = 2, cex = 0.6, cex.lab = 1.2)

#THE LATTICE PLOTS
hospital = read.csv("hospital-data.csv", colClasses="character")
outcome.hospital = merge(outcome, hospital, by="Provider.Number")
death = as.numeric(outcome.hospital[, 11])
npatient = as.numeric(outcome.hospital[, 15])
owner = factor(outcome.hospital$Hospital.Ownership)
xyplot(death ~ npatient | owner, main="Heart Attack 30 day Death Rate by Ownership", xlab="Number of Patients seen", ylab="30-day Death Rate",
       panel = function(x, y, ...){
           panel.xyplot(x, y, ...)
           panel.lmline(x, y, lwd=3, col="salmon4")
       }, pch=20, col="salmon")

