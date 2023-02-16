test.RSVcorr <- function() {

library("RUnit")
library("RSVcorr")
suppressWarnings(RNGversion("3.5.0"))
RNGkind("Mersenne-Twister", "Inversion")    
tolerance=1e-6
verbose=0


checkEqualsNumeric(mean(dat.wide.v$wt), 5.742015, tolerance=tolerance)    

checkEqualsNumeric(colMeans(dat.wide.v[,c(t(outer(assays,"."%.%times[1:3], paste0)))]), 
    c(1050.02703, 1910.68550, 1793.44717, 1105.65602, 3107.75430, 1731.85012,  814.01966, 9312.30713, 6927.82310,   17.23096, 123.30713,  100.87224), tolerance=tolerance)    

checkEqualsNumeric(nrow(dat.wide.v), 407, tolerance=tolerance)    

checkEqualsNumeric(summary(dat.wide$EIA.log10d14overd0), c(-1.29801,0.03725,0.98059,0.86763,1.45303,2.54516,133), tolerance=3e-6)    


}
