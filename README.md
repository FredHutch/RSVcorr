# RSVcorr
RSV correlates data package (for manuscript). This contains the Clinical and Immunogenicity Data from subjects in South Africa required for correlates analyses.

# Description
In both dat.wide and dat.wide.v datasets, each row corresponds to one mother-infant dyad. 

dat.wide (2337 rows) contains all study participants in South Africa. Columns specific to mothers are prefixed with m.; columns specific to infants are prefixed with p. There are 16 immune biomarkers for 4 assays: RSVA, RSVB, PCA, and EIA and 4 time points: d0, d14, log10d14overd0, and cord. The first three time points are maternal and the last is infant cord blood. The biomarker values at d0, d14, and cord are on the original scale, while the biomarker values at log10d14overd0 are log10 transformed. 

dat.wide.v (407 rows) is a subset of dat.wide and contains participants only in the VISC sampling plan (v for VISC). It further excludes 10 participants with a certain kind of missingness pattern in their RSV A/B measurements (see SAP for more detail). For the remaining participants, missing RSV A/B, EIA and PCA measurements are imputed. Ten copies of imputed RSV A/B, EIA and PCA measurements are saved, e.g. RSVA.d0.imp1 ~ RSVA.d0.imp10. The default copy RSVA.d0 corresponds to the first imputed copy RSVA.d0.imp1. dat.wide.v also contains 4 extra columns: wt, wt.ppimmfl, wt.ppimifl and wt.ppintersectfl.


# To install the package
library(devtools)
install_github("FredHutch/RSVcorr")  

# Checking the datasets within the package
RSVcorr:: <hit tab>

# For data dictionary
?RSVcorr::dat.wide
