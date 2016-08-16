library(dplyr)
library(tibble)
library(sbtools)
library(powstreams)
login_sb()

# GET DATA
ss = list_sites(c('gpp_estBest','dischdaily_calcDMean')) # sites with gpp estimates
# variables to grab
#get_ts_metadata()$var_src
v = c('dischdaily_calcDMean','gpp_estBest','er_estBest','K600_estBest')
# get random sites
length(si)
si = ss[sample.int(length(ss),50)]
# timeseries with those data
dd = data_frame()
for(s in si){
  ts = unitted::v(get_ts(v, s))
  ts = ts[apply(ts[,c("gpp","er","K600")],1,function(x) all(!is.na(x))),]
  if(nrow(ts) > 100) dd = rbind(dd, data.frame(site = s, ts))
}

dim(dd)
head(dd)

save(dd, file="powstreamseg.Rda")

# RELOAD DATA
load("powstreamseg.Rda")
# discharge from each site -- check distribution
dis = dd %>% group_by(site) %>% summarize(charge = median(dischdaily))
hist(log(dis$charge))
head(dd)

#plot(ts$gpp~ts$DateTime)
#plot(ts$er~ts$DateTime)
#plot(ts$K600~ts$DateTime)
#hist(ts$er)
#pairs(ts %>% select(gpp, er, K600))
