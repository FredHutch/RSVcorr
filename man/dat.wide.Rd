\name{dat.wide}
\alias{dat.wide}
\alias{dat.wide.v}
\docType{data}
\title{Clinical and Immunogenicity Data for South Africa}
\description{
In both dat.wide and dat.wide.v, each row corresponds to one mother-infant dyad. \cr 
\cr
dat.wide (2337 rows) contains all study participants in South Africa. Columns specific to mothers are prefixed with m.; columns specific to infants are prefixed with p. There are 16 immune biomarkers for 4 assays: RSVA, RSVB, PCA, and EIA and 4 time points: d0, d14, log10d14overd0, and cord. The first three time points are maternal and the last is infant cord blood. The biomarker values at d0, d14, and cord are on the original scale, while the biomarker values at log10d14overd0 are log10 transformed. \cr
\cr
dat.wide.v (407 rows) is a subset of dat.wide and contains participants only in the VISC sampling plan (v for VISC). It further excludes 10 participants with a certain kind of missingness pattern in their RSV A/B measurements (see SAP for more detail). For the remaining participants, missing RSV A/B, EIA and PCA measurements are imputed. Ten copies of imputed RSV A/B, EIA and PCA measurements are saved, e.g. RSVA.d0.imp1 ~ RSVA.d0.imp10. The default copy RSVA.d0 corresponds to the first imputed copy RSVA.d0.imp1. dat.wide.v also contains 4 extra columns: wt, wt.ppimmfl, wt.ppimifl and wt.ppintersectfl. \cr
\cr
}
\format{
  \describe{
    \item{\code{pair.id}}{character. Identifier for each mother-infant pair}
    \item{\code{y1}}{numeric. Case endpoint 1. Defined based on ADEFEPIP as follows: IF endpt3_1 == "Y" | endpt3_2 == "Y" | endpt3_3 == "Y") & epi.st.age <= 90, then 1, else 0}
    \item{\code{y2}}{numeric. Case endpoint 2. Defined based on ADEFEPIP as follows: IF endpt3_2 == "Y" & epi.st.age <= 90, then 1, else 0}
    \item{\code{y3}}{numeric. Case endpoint 3. Defined based on ADEFEPIP as follows: IF  endpt3_2_noco == "Y"  & epi.st.age <= 90, then 1, else 0}
    \item{\code{SCRMSITEID}}{character. Study site identifier}
    \item{\code{sampled}}{character. Y if part of VISC stratified sampling plan, N otherwise}
    \item{\code{sitegrp}}{character. Grouped site identifier: NN, PP, QQ, RR, SS}
    \item{\code{season}}{numeric. 1, 2, or 3}
    \item{\code{trt}}{factor. 0 for placebo and 1 for RSV F vaccine}
    \item{\code{trtsdt}}{Date. Date of first exposure to treatment}
    \item{\code{agegr}}{factor. 0: 18 to < 29 YEARS; 1: 29 to <= 40 YEARS}
    \item{\code{m.race}}{factor: 0: AMERICAN INDIAN OR ALASKA NATIVE; 1: ASIAN; 2: BLACK OR AFRICAN AMERICAN; 3: OTHER; 4: WHITE}
    \item{\code{m.raceoth}}{character. Explains what other in m.race means}
    \item{\code{m.ethnic}}{character. HISPANIC OR LATINO; NOT HISPANIC OR LATINO }
    \item{\code{p.1st.epi.no}}{numeric. Infant first (primary) endpoint criteria-qualifying positive RSV episode number. This may not be the first RSV episode}
    \item{\code{p.1st.epi.st.age}}{numeric. Infant first (primary) endpoint criteria-qualifying positive RSV episode start age in days. 0 to 90}
    \item{\code{p.1st.epi.en.age}}{numeric. Infant first (primary) endpoint criteria-qualifying positive RSV episode end age in days. 17, ., 168, 312}
    \item{\code{m.1st.epi.no}}{numeric. Mother first RSV positive episode number. This is the first episode at which swab collected turns out positive for either RSVA or RSVB}
    \item{\code{m.1st.epi.st.age}}{numeric. Mother first RSV positive episode (as defined by m.1st.epi.no) start age in days}
    \item{\code{m.1st.epi.en.age}}{numeric. Mother first RSV positive episode (as defined by m.1st.epi.no) end age in days}
    \item{\code{m.1st.epi.st.dy}}{numeric. Mother first RSV positive episode (as defined by m.1st.epi.no) start study day}
    \item{\code{m.1st.epi.en.dy}}{numeric. Mother first RSV positive episode (as defined by m.1st.epi.no) end study day}
    \item{\code{ittimfl}}{character. ITT immunogenicity population flag for mothers. All 'Y'}
    \item{\code{itteffl}}{character. ITT efficacy population flag for mothers. All 'Y'}
    \item{\code{ppimmfl}}{character. PP immunogenicity population flag for mothers. Some 'N', most 'Y'}
    \item{\code{ppefmfl}}{character. PP efficacy population flag for mothers. Few 'N', most 'Y'}
    \item{\code{m.eosstt}}{character. Maternal end of study status, Some 'DISCONTINUED', most 'COMPLETED'}
    \item{\code{m.eosdt}}{Date. End of study date}
    \item{\code{exage}}{numeric. Gestational age at time of vaccination (weeks)}
    \item{\code{mhsmopr}}{character. Is subject a current smoker or tobacco. Some 'Y', most 'N'}
    \item{\code{mhalc}}{character. Is subject currently consuming alcohol. Some 'Y', most 'N'}
    \item{\code{mhdri}}{character. If Yes, how many drinks per day}
    \item{\code{mhrdr}}{character. Does subject use recreational drug(s). Few 'Y', most 'N'}
    \item{\code{mhfre}}{character. If Yes, how frequently}
    \item{\code{mhalc2yr}}{character. Alcohol on regular basis last 2 years. Some 'Y', most 'N', some missing}
    \item{\code{prebrth}}{numeric. Number of previous children. 1, 2, 3, 4, or 5. About half are missing}
    \item{\code{bmi}}{numeric. Weight / ((Height/100)**2). Two missing}
    \item{\code{hiv}}{character. HIV status. Most 'NEGATIVE', some empty}
    \item{\code{m.ast}}{character. Maternal asthma. Most 'N', some 'Y'}
    \item{\code{ga}}{numeric. Gestational age at birth (weeks)}
    \item{\code{vacc2birth}}{numeric. Days from vaccination to birth}
    \item{\code{vacc2birthLESSTHAN30}}{numeric. 0 or 1}
    \item{\code{m.age.at.trt.binned}}{numeric. Mother's age in years at vaccination binned into 5 year intervals, derived from birth date and vaccination date. One missing}
    \item{\code{m.ageCAT}}{character. '<= 28 yrs' or '> 28 yrs', one missing}
    \item{\code{m.tt1rsv}}{numeric. Duration (in days) between episode start date and the first swab taken (with RSV positive) date. 0 to 34, mostly NA}
    \item{\code{m.rsvA.disease}}{numeric. If any positive in episode then 1; if all negative then 0; missing otherwise. Most NA, some 0, some 2}
    \item{\code{m.rsvB.disease}}{numeric. Same as above}
    \item{\code{EIA.d0}}{numeric}
    \item{\code{EIA.d14overd0}}{numeric}
    \item{\code{EIA.d14}}{numeric}
    \item{\code{PCA.d0}}{numeric}
    \item{\code{PCA.d14overd0}}{numeric}
    \item{\code{PCA.d14}}{numeric}
    \item{\code{RSVA.d0}}{numeric}
    \item{\code{RSVB.d0}}{numeric}
    \item{\code{RSVA.d14overd0}}{numeric}
    \item{\code{RSVA.d14}}{numeric}
    \item{\code{RSVB.d14overd0}}{numeric}
    \item{\code{RSVB.d14}}{numeric}
    \item{\code{EIA.log10d0}}{numeric}
    \item{\code{EIA.log10d14overd0}}{numeric}
    \item{\code{EIA.log10d14}}{numeric}
    \item{\code{PCA.log10d0}}{numeric}
    \item{\code{PCA.log10d14overd0}}{numeric}
    \item{\code{PCA.log10d14}}{numeric}
    \item{\code{RSVA.log10d0}}{numeric}
    \item{\code{RSVB.log10d0}}{numeric}
    \item{\code{RSVA.log10d14overd0}}{numeric}
    \item{\code{RSVA.log10d14}}{numeric}
    \item{\code{RSVB.log10d14overd0}}{numeric}
    \item{\code{RSVB.log10d14}}{numeric}
    \item{\code{sampdy.d0}}{numeric. Sample collected date - vaccinated date + 1. Most around -7, but many are way back, some NA}
    \item{\code{sampdy.d14}}{numeric. Sample collection date relative to vaccination date for D14 visit. 13 to 17, many NA}
    \item{\code{EIA.2fold.d14overd0}}{numeric. Indicator variable: 1 if more than 2-fold rise in EIA titer at day 14 in comparison to day 0, else 0}
    \item{\code{EIA.4fold.d14overd0}}{numeric. Indicator variable: 1 if more than 4-fold rise in EIA titer at day 14 in comparison to day 0, else 0}
    \item{\code{PCA.2fold.d14overd0}}{numeric. Indicator variable: 1 if more than 2-fold rise in PCA titer at day 14 in comparison to day 0, else 0}
    \item{\code{PCA.4fold.d14overd0}}{numeric. Indicator variable: 1 if more than 4-fold rise in PCA titer at day 14 in comparison to day 0, else 0}
    \item{\code{RSVA.2fold.d14overd0}}{numeric. Indicator variable: 1 if more than 2-fold rise in RSVA titer at day 14 in comparison to day 0, else 0}
    \item{\code{RSVA.4fold.d14overd0}}{numeric. Indicator variable: 1 if more than 4-fold rise in RSVA titer at day 14 in comparison to day 0, else 0}
    \item{\code{RSVB.2fold.d14overd0}}{numeric. Indicator variable: 1 if more than 2-fold rise in RSVB titer at day 14 in comparison to day 0, else 0}
    \item{\code{RSVB.4fold.d14overd0}}{numeric. Indicator variable: 1 if more than 4-fold rise in RSVB titer at day 14 in comparison to day 0, else 0}
    \item{\code{p.age}}{numeric. Infant age when the informed consent was signed for infants (weeks)}
    \item{\code{p.sex}}{character. Infant sex: F or M}
    \item{\code{p.race}}{character. AMERICAN INDIAN OR ALASKA NATIVE; ASIAN; BLACK OR AFRICAN AMERICAN; OTHER; WHITE}
    \item{\code{p.raceoth}}{character.}
    \item{\code{p.ethnic}}{character. HISPANIC OR LATINO; NOT HISPANIC OR LATINO}
    \item{\code{ppimifl}}{character. PP immunogenicity population flag for infants. Some 'N', most 'Y'}
    \item{\code{ppefifl}}{character. PP efficacy population flag for infants. Some 'N', most 'Y'}
    \item{\code{ppintersectfl}}{character. PP immunogenicity population flag for mothers and infants (Intersection of ppimmfl and ppimifl). Some 'N', most 'Y'}
    \item{\code{p.eosstt}}{character. Infants end of study status. Most 'COMPLETED', some 'DISCONTINUED'}
    \item{\code{p.eosdt}}{Date. Infants end of study date}
    \item{\code{smoker}}{character. Does infant live with a smoker. Most 'N', some 'Y', a few empty}
    \item{\code{child5}}{character. Indicator other children < 5 years of age in home. Most 'N', some 'Y', some empty}
    \item{\code{daycare}}{character. Most 'N', some 'Y', a few empty}
    \item{\code{iwt}}{numeric. Birthweight. Some missing}
    \item{\code{iwtlen}}{numeric. Ratio of length to birthweight (a marker of being a 'physiologically young infant' at birth). }
    \item{\code{hdcirc}}{numeric. Frontal Occipital Head Circumference (FOC)}
    \item{\code{year}}{numeric. RSV peak year determined by Lou Fries. 2016, 2017, 2018, some NA}
    \item{\code{midpt.week}}{numeric. RSV peak week determined by Lou Fries. 15, 17, 19, 20, 21, 22, 24, some NA}
    \item{\code{br.year}}{numeric. Infant birth year. 2016, 2017, 2018, some NA}
    \item{\code{br.week}}{numeric. Infant birth week. 0 to 52, some NA}
    \item{\code{dur.week}}{numeric. Number of weeks between birth week and RSV peak week. -31 to 8, some NA}
    \item{\code{p.total.rsv.swabs}}{numeric. Number of total RSV positive swabs for each infant. Comes from ADSL:RSV_events (no info for mother, thus no m.total.rsv.swabs). It is used for RSV peak determination. 2 to 331, some NA}
    \item{\code{p.small}}{character. Small for gestational age. Most 'N', some 'Y'}
    \item{\code{p.lbw}}{character. Low birth weight. Most 'N', some 'Y'}
    \item{\code{p.igr}}{character. Infant intrauterine growth retardation. Most 'N', some 'Y'}
    \item{\code{EIA.cord}}{numeric}
    \item{\code{PCA.cord}}{numeric}
    \item{\code{RSVA.cord}}{numeric}
    \item{\code{RSVB.cord}}{numeric}
    \item{\code{EIA.log10cord}}{numeric}
    \item{\code{PCA.log10cord}}{numeric}
    \item{\code{RSVA.log10cord}}{numeric}
    \item{\code{RSVB.log10cord}}{numeric}
    \item{\code{sampdy.cord}}{numeric. Sample collection date relative to vaccination date for cord blood. 4 to 105, some NA}
    \item{\code{RSVA.d0.imp1}}{numeric. The first imputed copy of RSVA.d0}
    \item{\code{RSVA.d0.imp2}}{numeric. The second imputed copy of RSVA.d0}
    \item{\code{RSVA.d0.imp10}}{numeric. The tenth imputed copy of RSVA.d0}
    \item{\code{stratum}}{character. Sampling stratum}
    \item{\code{wt}}{numeric. Inverse sampling probability weights for ittimfl population}
    \item{\code{wt.ppimmfl}}{numeric. Inverse sampling probability weights for pp-imm-M population}
    \item{\code{wt.ppimifl}}{numeric. Inverse sampling probability weights for pp-imm-I population}
    \item{\code{wt.ppintersectfl}}{numeric. Inverse sampling probability weights for intersection of pp-imm-M and pp-imm-I populations}
    \item{\code{riskScore.mat.endpoint1}}{numeric. Risk scores (fitted probability of outcome) predicted by selected superlearner (k=1) using variables in the maternal enrollment set and endpoint 1. These have been scaled to have mean 0 and sd 1.}
    \item{\code{riskScore.mat.endpoint2}}{numeric. Risk scores (fitted probability of outcome) predicted by selected superlearner (k=2) using variables in the maternal enrollment set and endpoint 2. These have been scaled to have mean 0 and sd 1.}
  }
}
\details{

This data frame combines data from ADSL (subject level data), ADEFEPIP (infant episode database), ADEFEPIM (mother episode database), and ADIS (immunogenicity database). Only the first RSV positive episode info is contained in this data frame.  \cr

dat.wide does not contain mothers/infants in adsl who are not paired. \cr

LLOQ: 400 for EIA, 12 for PCA, 13 for RSVA, and 8 for RSVB. \cr

}
\examples{
    plot(PCA.cord~PCA.d14, dat.wide, log="xy")
}
