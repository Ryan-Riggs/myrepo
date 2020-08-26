library(sf)
library(geosphere)
library(tidyverse)
library(sf)
library(sp)
library(ggplot2)
library(rgeos)
library(dplyr)
library(gmt)

if (!"foreign" %in% rownames(installed.packages())){
  install.packages("foreign")}; require(foreign)
if (!"rgdal" %in% rownames(installed.packages())){
  install.packages("rgdal")}; require(rgdal)
if (!"shapefiles" %in% rownames(installed.packages())){
  install.packages("shapefiles")}; require(shapefiles)
if (!"RColorBrewer" %in% rownames(installed.packages())){
  install.packages("RColorBrewer")}; require(RColorBrewer)
if (!"zyp" %in% rownames(installed.packages())){
  install.packages("zyp")}; require(zyp)


####before running in arc. 
data = read.dbf("E:\\research\\GRWL\\GRWL_2015_present\\W_validation\\1_spc\\merge_1.dbf")
data$ID_2 = seq.int(nrow(data))
foreign::write.dbf(data, "E:\\research\\GRWL\\GRWL_2015_present\\W_validation\\1_spc\\merge_1.dbf")

setwd("E:\\research\\MERIT_GRWL")

###grwl cross sections w/ grades discharge data & order by ID.  

data_sj = read.dbf("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\1spc_3x\\spatial_join\\East_val_spatial_join.dbf")
data_sj = as.data.frame(data_sj)

quant_files = list.files("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\1spc_3x\\quantiles", full.names = TRUE)

vars = LETTERS[seq(from = 1, to = length(quant_files))]


for (i in 1:length(vars)){
  assign(vars[i], as.data.frame(read.csv(quant_files[i])))
}

#data = inner_join(a, b)

multi_inner <- Reduce(
  function(x, y, ...) merge(x, y, ...), 
  list(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T)
)
data = multi_inner
#write.csv(data, "E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\e_widths_1percentiles\\quantile_width_export_combined_updated83.csv")
data = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\e_widths_1percentiles\\quantile_width_export_combined_updated83.csv")

#data = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\e_widths_1percentiles\\quantile_width_export_combined.csv")

###west. 
data = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\W_validation\\xsections\\western_gauges_quants_comb.csv")
data_change = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\W_validation\\xsections\\western_gauges_change.csv")
data_flags = grep("flag", names(data_data))
data = data%>%select(-data_flags)
data_val = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\W_validation\\2010_2020_811\\val_comb.csv")
####


names(data_sj) = substring(names(data_sj), 5)
#data = data[order(data$ID),]
data$ID=data$ID_2


data$COMID = data_sj$COMID[match(data$ID_2, data_sj$ID_2)]
data$strmOrder = data_sj$strmOrder[match(data$ID_2, data_sj$ID_2)]

data_change = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\e_widths_1percentiles\\Spatialjoin\\east_val_3x_3spc_change.csv")
data$change = data_change$median[match(data$ID_2, data_change$ID_2)]


###Landsat estimated widths from GEE 2015-present. 
data_val_15 = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\1spc_3x\\2015_2020\\vals\\east_val_15_3x_1spc.csv")
data_val_161 = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\1spc_3x\\2015_2020\\vals\\East_val_16_1_3x_1spc.csv")
data_val_162 = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\1spc_3x\\2015_2020\\vals\\East_val_16_2_3x_1spc.csv")
data_val_17 = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\1spc_3x\\2015_2020\\vals\\East_val_17_3x_1spc.csv")
data_val_18 = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\1spc_3x\\2015_2020\\vals\\East_val_18_3x_1spc.csv")
data_val_19 = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\1spc_3x\\2015_2020\\vals\\East_val_19_3x_1spc.csv")
data_val_20 = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\1spc_3x\\2015_2020\\vals\\East_val_20_3x_1spc.csv")

data_val = rbind(data_val_15, data_val_161, data_val_162, data_val_17, data_val_18, data_val_19, data_val_20)

data_val$system.index = gsub("^[[:digit:]]_", "", data_val$system.index)
data_val$system.index = substr(data_val$system.index, 0, 20)
d_dates = str_sub(data_val$system.index, start = -8)
d_date = as.Date(d_dates, format = "%Y%m%d")
data_val$Date = d_date

##read in validation dates to assign to validation cross section measurements. 
validation_dates = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\1spc_3x\\2013\\East_val_13_3x_1spc_dates.csv")

# data_val_dt_files = list.files("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\1spc_3x\\2015_2020\\date", full.names = TRUE)
# data_val_dt_list = lapply(data_val_dt_files, read.csv)
# data_val_dt = lapply(data_val_dt_list, as.list)
# data_val_dt = combine(data_val_dt)
# validation_dates = data_val_dt


#validation_dates = read.csv("C:\\Users\\rriggs\\Downloads\\Validation_w_3spc_dates.csv")

dts = as.character(validation_dates$list)
sep_val = unlist(strsplit(dts, "\\,"))
sep_val = as.vector(sep_val)
sep_val = noquote(sep_val)
sep_val = as.character(sep_val)
sep_val = trimws(sep_val)
sep_val = gsub("[", "", sep_val, fixed = TRUE)
sep_val = gsub("]", "", sep_val, fixed = TRUE)

##read in validation ids to help assign dates for each cross section measurement. 
#validation_id = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\Landsat_id_nh15.csv")

#validation_id=read.csv("C:\\Users\\rriggs\\Downloads\\Validation_w_3spc_id.csv")

validation_id=read.csv("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\1spc_3x\\2013\\East_val_13_3x_1spc_id.csv")


id = as.character(validation_id$list)
sep_id = unlist(strsplit(id, "\\,"))
sep_id = as.vector(sep_id)
sep_id = noquote(sep_id)
sep_id = as.character(sep_id)
sep_id = trimws(sep_id)
sep_id = gsub("[", "", sep_id, fixed = TRUE)
sep_id = gsub("]", "", sep_id, fixed = TRUE)
val_comb = cbind(sep_id, sep_val)

data_val$ID=data_val$ID_2



##GRADES 2012-2014
grades_ind = which(filt_df$COMID %in% data$COMID)
grades = filt_df[grades_ind,]





# 
# usgs_w = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\width_val\\input\\gaugeData\\USGS\\dailyW\\07374000.csv")
# usgs_q = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\width_val\\input\\gaugeData\\USGS\\dailyQ\\07374000.csv")
gageinfo = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\width_val\\input\\gaugeData\\USGS\\gaugeTable.csv")
gageinfo = gageinfo[gageinfo$GRWL_width>99,]

##grwl x section length
grwl_l = 3
##perform math on data
# data$w0 = grwl_l*data$q000*data$width_m
# data$w10 = grwl_l*data$q010*data$width_m
# data$w20= grwl_l*data$q020*data$width_m
# data$w30= grwl_l*data$q030*data$width_m
# data$w40= grwl_l*data$q040*data$width_m
# data$w50= grwl_l*data$q050*data$width_m
# data$w60= grwl_l*data$q060*data$width_m
# data$w70= grwl_l*data$q070*data$width_m
# data$w80= grwl_l*data$q080*data$width_m
# data$w90= grwl_l*data$q090*data$width_m
# data$w100= grwl_l*data$q100*data$width_m




####Filter out gage info to only NH15 area
gageinfo_lat = gageinfo[gageinfo$LAT >28.5451 & gageinfo$LAT <32.0911,]
gageinfo_lon = gageinfo_lat[gageinfo_lat$LONG > -96.0782 & gageinfo_lat$LONG < -89.7776,]
gageinfo = gageinfo_lon



##Don't read these e&n conversions together-messes up the grwl 'data'##
e = n = rep(NA, nrow(gageinfo))
zone = ceiling(data$lon_dd/6) + 30
data_1 = cbind(gageinfo, e, n)
projString = paste("+proj=utm +zone=", zone, " +datum=WGS84 +ellps=WGS84", sep='')
eCol = which('e'==names(gageinfo))
nCol = which('n'==names(gageinfo))
for (i in 1:nrow(gageinfo)){
  gageinfo[i, c(eCol,nCol)] = project(cbind(gageinfo$LONG[i], gageinfo$LAT[i]), projString[i])
}

# convert grwl xsections from Lat Long to UTM coordinates: 
e = n = rep(NA, nrow(data))
zone = ceiling(data$lon_dd/6) + 30
data = cbind(data, e, n)
projString = paste("+proj=utm +zone=", zone, " +datum=WGS84 +ellps=WGS84", sep='')
eCol = which('e'==names(data))
nCol = which('n'==names(data))
for (i in 1:nrow(data)){
  data[i, c(eCol,nCol)] = project(cbind(data$lon_dd[i], data$lat_dd[i]), projString[i])
}

#How many xsections to consider
nGRWL = as.numeric(5)

#Calculate distance function
distance = function(ge, gn, xe, xn){
  calculate=sqrt(((ge-xe)^2)+((gn-xn)^2))
  return(calculate)
}

w = distGeo(as.data.frame(gageinfo_coords[2, 1:2]), as.data.frame(data_coords[,1:2]))


gageinfo_coords = cbind(gageinfo$LONG, gageinfo$LAT)
data_coords = cbind(data$lon_dd, data$lat_dd)

#plot and determine nearest xsections
plot(gageinfo$e, gageinfo$n, type="p")
closestDF=as.data.frame(array(NA, c(nrow(as.vector(gageinfo)), nGRWL)))
distanceDF=as.data.frame(array(NA, c(nrow(as.vector(gageinfo)), nGRWL)))
for(i in 1:nrow(gageinfo_coords)){
  ##dist=distance(gageinfo$e[i], gageinfo$n[i], data$e, data$n)
  dist = distGeo(gageinfo_coords[i, 1:2], data_coords[,1:2])##### seems to be working. 
  #dist = geodist(gageinfo_coords[i, 1],gageinfo_coords[i,2], data_coords[,1], data_coords[,2])
  
  close=order(dist, decreasing=FALSE)[1:nGRWL]
  closeID=data$ID[close]
  
  distance_to = sort(dist)[1:nGRWL]
  ##nearest[gageinfo$SITE_NUM[i]]=order(dist)[1:nGRWL]
  
  #points(data$e[close], data$n[close], pch=1, col=i)
  
  closestDF[i,]=closeID
  distanceDF[i,]=distance_to #was close
}

##determine closest xsections to each gage
Site_number_xsections=cbind(gageinfo$SITE_NUM, closestDF)
Site_number_distances=cbind(gageinfo$SITE_NUM, distanceDF)


##filter out gages with no xsections within 500m. 
gage_filter = Site_number_distances[,(nGRWL+1)] < 500

Site_number_xsections = Site_number_xsections[gage_filter,]

# sort out rating curve headers:
tab = data

# comid_df = cbind(data_sj$ID_2, data_sj$COMID)
# 
# mt = unique(comid_df[,1])
# m = comid_df[mt,]
# blah = m[order(m[,1]),]
# COMID = blah[,2]
# 
# blah_ind = blah[blah[,1]> 0]
# blah_1 = blah[blah_ind]
# 
# data = data[order(data$ID),]
# 
# data = cbind(COMID, data)
# 



# sort out rating curve headers:
tab = data
tab = tab[, order(names(tab))]
width_cols = grep("q" ,names(tab))
width_cols_ordered = grep("q[[:digit:]]", names(tab))
width_cols_new = paste("w", seq(0, 100, 1), sep = "")

test = as.data.frame(matrix(numeric(), nrow = nrow(tab), ncol = 101))
colnames(test)= width_cols_new

for (i in 1:ncol(test)){
  test[,i] = tab[,width_cols_ordered[i]] * grwl_l * tab$width_m
}

#output = as.data.frame(matrix(numeric(), nrow =length(1), ncol = 101))

tabcols = grep("\\<[^w]", names(tab))
tab = cbind(test, tab[,tabcols])


wd_GRADES =setwd("E:\\research\\GRADES\\percentiles_1_w\\")
COMID_files = list.files(wd_GRADES)
COMID_vals = gsub(".csv", "", COMID_files)


#####################################################filter tab to only comids available - not needed once expanded. 
ryan = which(tab$COMID %in% COMID_vals)
ryan_df = tab[ryan,]
tab = ryan_df
######################################################





###how to rearrange GRADES data to easily add into tab. 
ry_d= as.data.frame(matrix(numeric(), nrow = 1, ncol = 101))
ry  = read.csv(COMID_files[1])
ry_d[1,] = ry[,1]
Q_cols_new = paste("Q", seq(0, 100, 1), sep = "")
colnames(ry_d) = Q_cols_new
ry_d[1,] = ry[,2]
tab_1 = as.data.frame(matrix(numeric(), nrow = 52, ncol = 101))
colnames(tab_1) = Q_cols_new
for(i in 1:length(COMID_vals)){
  ry  = read.csv(COMID_files[i])
  ry_d[1,] = ry[,1]
  Q_cols_new = paste("Q", seq(0, 100, 1), sep = "")
  colnames(ry_d) = Q_cols_new
  ry_d[1,] = ry[,2]
  tab_1[i,] = ry_d 
}
#as.data.frame(tab_1)
#colnames(tab_1[,1:101]) = colnames(ry_d)
#tab_1 = cbind(COMID_vals, tab_1)
comid = as.vector(COMID_vals)
tab_1$COMID = comid
tab_1 = sapply(tab_1, as.numeric)
tab_1 = as.data.frame(tab_1)


#tab_1ind = match(tab$COMID, tab_1$COMID)

# tab_2 =as.data.frame(matrix(numeric(), nrow = nrow(tab), ncol = 102))
# 
# for(i in 1:nrow(tab)){
#   tab_2[i,] = tab_1[tab_1ind[i],, drop = FALSE]
# }
# 
# 
# colnames(tab_2) = colnames(tab_1)
# ryan_df = cbind(tab_2, tab, drop = FALSE)
# 
# tab = ryan_df
# 
# 
# tab_07777 = tab


ry = left_join(tab, tab_1)

tab = ry


























#tab = tab[, order(names(tab))]
QrecCols = grep("Q[[:digit:]]", names(tab))

wCols_rev = grep("w[[:digit:]]", names(tab))
wCols = rev(wCols_rev)
wOccCols = wCols
##try to sort properly
#wCols = c("w100", "w90", "w80", "w70", "w60", "w50", "w40", "w30", "w20", "w10", "w0")
#wOccCols = wCols[grep("flag", names(tab)[wCols], invert=T)]
wFlagCols = wCols[grep("flag", names(tab)[wCols])]


# filter:
f = tab$strmOrder >= 3 #& tab$width_m > 100 ##############typically 3
# f = 1:nrow(tab)

pTab = tab[,]
qTab = pTab[, QrecCols]
wTab = pTab[, wOccCols]
fTab = pTab[, wFlagCols]



# set to NA measurements with values of zero or less and measuremets taken 
# where river water is located at the end of their cross section segments:
rmBoo = wTab<=0 | qTab<=0
qTab[rmBoo] = NA
wTab[rmBoo] = NA




##############################
# DHG:

# plot DHG colored by recurrence interval:
qCols = rainbow(length(QrecCols))
qCols_trans = rainbow(length(QrecCols), alpha=0.1)

aVec = bVec = r2Vec = rep(NA, length(QrecCols))

plot(range(qTab, na.rm=T), range(wTab, na.rm=T),
     main = "DHG",
     xlab = "Q (cms)",
     ylab = "w (m)",
     type="n", log="xy"
)

for (i in 1:length(QrecCols)){
  # set up vars:
  Q = qTab[,i]
  w = wTab[,i]
  
  # remove non positive values:
  boo = Q>0 & w>0
  if (!(T %in% boo)){next}
  
  #rmBoo = is.na(Q) | is.na(w) | Q<=0 | w<=0
  
  
  
  # take least squares linear regression:
  reg = lm(log(w) ~ log(Q))
  aVec[i] = reg$coefficients[[1]]
  bVec[i] = reg$coefficients[[2]]
  r2Vec[i] = summary(reg)$r.squared
  xSeq = seq(min(Q, na.rm=T), max(Q, na.rm=T), length.out=100)
  ySeq = exp(aVec[i])*xSeq^bVec[i]
  
  # plot:
  points(Q, w, pch=16, col=qCols_trans[i], cex=0.8)
  lines(xSeq, ySeq, lwd=1.8, col=1)
  lines(xSeq, ySeq, lwd=1, lty=3, col=qCols[i])
}



legend("bottomright", 
       paste0(names(tab[QrecCols]),
              "  b=", round(bVec, 2),
              "  r2=", round(r2Vec, 2)), 
       pch=16, col=qCols
)




##############################
#### AHG:

qTabLog = log(qTab)
wTabLog = log(wTab)

# qTabLog = qTabLog[, -match(c("Q0", "Q10", "Q90", "Q100"), names(qTabLog))]
# wTabLog = wTabLog[, -match(c("w100", "w090", "w010", "w000"), names(wTabLog))]

# generate exponent and coefficient tables:
regTabLog = as.data.frame(array(NA, c(nrow(qTabLog), 3)))
names(regTabLog) = c("a", "b", "r2")

for (i in seq(nrow(qTabLog))){
  # skip cross sections without at least 3 valid measurements:
  if (length(which((!is.na(as.numeric(wTabLog[i,])))))<=3){next}
  reg = lm(as.numeric(wTabLog[i,]) ~ as.numeric(qTabLog[i,]));
  regTabLog[i,] = c(reg$coefficients, suppressWarnings(summary(reg)$r.squared))
}

# might be a faster way to do it:
# regTabLog = data.frame(t(sapply(seq(nrow(qTabLog)),
#                                 function(x){
#                                   reg = lm(as.numeric(wTabLog[x,])~as.numeric(qTabLog[x,]));
#                                   coef = c(reg$coefficients, summary(reg)$r.squared)
#                                 })))

regTab = regTabLog
regTab$a = exp(regTabLog$a)

summary(regTab)


# plot AHG exponent and coefficient by R2:
pal = colorRampPalette(c("red", "blue"))
rCol = pal(10)[as.numeric(cut(regTabLog$r2, breaks=10))]
plot(regTab$b, regTab$a, log="y", col=rCol)
legend('topright', levels(cut(regTabLog$r2, breaks = 10)), col= pal(10), pch=16)


tab$AHG_a = tab$AHG_b = tab$AHG_r2 = NA

tab$AHG_a[f] = regTab$a
tab$AHG_b[f] = regTab$b
tab$AHG_r2[f] = regTab$r2


ryan_p = paste("/0", ryan, sep="")
for(i in 1:length(ryan_p)){
if(nchar(ryan_p[i]) > 4){
  ryan_p[i] = sub("^/0", "", ryan_p[i])
  ryan_p[i] = paste("/", ryan_p[i], sep = "")
}}


wd =setwd("E:\\research\\GRWL\\GRWL_2015_present\\width_val\\input\\gaugeData\\USGS\\dailyW\\")

##filter site numbers and add variables to be the same as file name. 
Gages_char = as.character(Site_number_xsections$`gageinfo$SITE_NUM`)
Gages_char_p = paste("/0", Gages_char, sep="")

for(i in 1:length(Gages_char_p)){
  if(nchar(Gages_char_p[i]) > 9){
    Gages_char_p[i] = sub("^/0", "", Gages_char_p[i])
    Gages_char_p[i] = paste("/", Gages_char_p[i], sep = "")
  }}



p = paste(Gages_char_p, ".csv", sep="")
trial = paste(wd, p, sep="")

#create function for processing each USGS dataset. ##########################################################
usgs_processing = function(usgs_w){
  usgs_w$measurement_dt <- substr(usgs_w$measurement_dt, 0, 10)
  usgs_w_filter = subset(usgs_w, usgs_w$measured_rating_diff!= "Poor")# & usgs_w$chan_loc_dist <= 500)
  ##Just using the dishcharge values found in the width dataset - slightly different from discharge values.
  w = as.vector(usgs_w_filter$chan_width)
  q = as.vector(usgs_w_filter$discharge_va)
  ##convert to proper units for plotting. 
  w_m = as.numeric(w)*0.3048
  q_cms = as.numeric(q)*0.02832
  q_w = cbind(q_cms, w_m)
  return(q_w)
}

##is error function. 
is.error <- function(
  expr,
  tell=FALSE,
  force=FALSE
)
{
  expr_name <- deparse(substitute(expr))
  test <- try(expr, silent=TRUE)
  iserror <- inherits(test, "try-error")
  if(tell) if(iserror) message("Note in is.error: ", test)
  if(force) if(!iserror) stop(expr_name, " is not returning an error.", call.=FALSE)
  # output:
  iserror
}

##set wd for USGS discharge measurements ####### sometimes you have to run this wd_q twice to get the correct data. 
wd_q =setwd("E:\\research\\GRWL\\GRWL_2015_present\\width_val\\input\\gaugeData\\USGS\\dailyQ\\")
usgs_q_list = paste(wd_q, p, sep="")

#create a function to process q data for simple output. working. 
usgs_q_processing = function(usgs_q){
  q_v = as.vector(usgs_q[,4])
  q_c = as.character(usgs_q[4])
  q_n = as.numeric(q_v)
  q= q_n *0.02832
  usgs_q = cbind(usgs_q, q)
  as.character(usgs_q$datetime)
  return(usgs_q)
}

###id/date info. 

## work on splitting data_val system so landsat ids will match each other. 
syst = data_val$system.index
syst_c = as.character(syst)
syst_1 = substr(syst_c, 0, 22)
data_val$system.index=syst_1

##attach dates to data_val based on corresponding values in val_comb table. ##############################################this data_val for loop won't stop running but if you stop it, it will produce the correct data. 
val_dates= as.vector(nrow(data_val))

ind = match(data_val$system.index, sep_id)
val_dates = as.character(sep_val[ind])

data_val$Date = val_dates
data_val$calc_mean = data_val$mean * data_val$width_m * grwl_l
data_val$ID = data_val$ID_2

auto.legend.pos <- function(x,y,xlim=NULL,ylim=NULL) {
  if (dev.cur() > 1) {
    p <- par('usr')
    if (is.null(xlim)) xlim <- p[1:2]
    if (is.null(ylim)) ylim <- p[3:4]
  } else {
    if (is.null(xlim)) xlim <- range(x, na.rm = TRUE)
    if (is.null(ylim)) ylim <- range(y, na.rm = TRUE)
  }
  countIt <- function(a) {
    tl <- sum(x <= xlim[1]*(1-a)+xlim[2]*a & y >= ylim[1]*a+ylim[2]*(1-a))
    tr <- sum(x >= xlim[1]*a+xlim[2]*(1-a) & y >= ylim[1]*a+ylim[2]*(1-a))
    bl <- sum(x <= xlim[1]*(1-a)+xlim[2]*a & y <= ylim[1]*(1-a)+ylim[2]*a)
    br <- sum(x >= xlim[1]*a+xlim[2]*(1-a) & y <= ylim[1]*(1-a)+ylim[2]*a)
    c(topleft=tl,topright=tr,bottomleft=bl,bottomright=br)
  }
  for (k in seq(0.5,0.1,by=-0.05)) {
    a <- countIt(k)
    if (sum(a==0)>0) break
    #if (!is.na(sum(a))) break
  
    }
  names(a)[which(a==0)][1]   # may delete "[1]"
}





## loop to create plots with rating curves, usgs data, and landsat estimated widths. ### still producing multiple plots of gages w/ diff xsections.


##create NA columns in data so that we can bypass them in the following loop. 
for (i in 1:nrow(tab)){
  notNA = !is.na(qTab[i,])
  Q = as.numeric(qTab[i,notNA])
  w = as.numeric(wTab[i,notNA])
}

tab$change= tab$median

tab$change[mapply(is.na, tab$change)] <- 0


data_val$Date = as.character(data_val$Date)

xSecq=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecw=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecIDcol=grep("V", names(Site_number_xsections))
mInd = array(5, dimnames = NULL)
rangedf_1 = as.data.frame(matrix(numeric(), nrow = 1, ncol = 4))

gage_stats = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 14))
gage_stats_GRADES = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 14))
colnames(gage_stats_GRADES)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "p_val","Bias", "RRMSE", "avg_std", "change", 'RRMSE_median', "std_Q", "STDE")
as.data.frame(gage_stats_GRADES)
l_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
u_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
sd_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))

colnames(gage_stats)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "p_val","Bias", "RRMSE","avg_std", "change", "RRMSE_median", "std_Q","STDE")
as.data.frame(gage_stats)
gage_stats_col1 = as.vector(1)
gage_stats_col2 = as.vector(1)
gage_stats_GRADES_col1 = as.vector(1)
gage_stats_GRADES_col2 = as.vector(1)
pdfOut = "E:/research/temp_plots/Western_alaska.pdf"
pdf(pdfOut)

Site_number_xsections_1 = Site_number_xsections
for (i in 1:nrow(Site_number_xsections)){
  print(i)
  #for (j in 1:(ncol(Site_number_xsections))){
  xSecIDcol = Site_number_xsections[i,]
  ##if you uncomment the next two lines it will only produce the gages with 5 lines. Only 4 plots produced. 
  #if(xSecIDcol[1]==Site_number_xsections[i,1] & xSecIDcol[2]==Site_number_xsections[i,2] & xSecIDcol[3]==Site_number_xsections[i,3] ##extra step to plot only proper curves. 
  #&xSecIDcol[4]==Site_number_xsections[i,4]&xSecIDcol[5]==Site_number_xsections[i,5]&xSecIDcol[6]==Site_number_xsections[i,6]){
  xSecID=xSecIDcol[2:ncol(xSecIDcol)]
  
  
  #print(xSecID)
  #[xSecIDcol==Site_number_xsections[i,2:6]]
  mInd = match(xSecID, tab$ID)
  
  xSecw=wTab[mInd,] ##notNA
  xSecq=qTab[mInd,] ##notNA
  #xSecw = xSecw[,11:91]
  #xSecq = xSecq[,11:91]
  
  
  #xSecw = xSecw[,25:75]
  #xSecq = xSecq[,25:75]
  
  
  w_max=max(range(xSecw, na.rm = TRUE))
  w_min=min(range(xSecw, na.rm = TRUE))
  q_max=max(range(xSecq, na.rm = TRUE))
  q_min=min(range(xSecq, na.rm = TRUE))
  is.na(w_max) = sapply(w_max, is.infinite)
  is.na(w_min) = sapply(w_min, is.infinite)
  is.na(q_max) = sapply(q_max, is.infinite)
  is.na(q_min) = sapply(q_min, is.infinite)
  rangedf_1 = cbind(q_max, q_min, w_max, w_min)
  
  data_val_subset = try(subset(data_val, xSecID[,1]==data_val$ID | xSecID[,2]==data_val$ID| 
                                 xSecID[,3]==data_val$ID| xSecID[,4]==data_val$ID| xSecID[,5]==data_val$ID))
  
  # data_val_subset = try(subset(data_val, xSecID[,1]==data_val$ID | xSecID[,2]==data_val$ID| 
  #                                xSecID[,3]==data_val$ID| xSecID[,4]==data_val$ID| xSecID[,5]==data_val$ID
  #                              | xSecID[,6]==data_val$ID| xSecID[,7]==data_val$ID| xSecID[,8]==data_val$ID
  #                              | xSecID[,9]==data_val$ID| xSecID[,10]==data_val$ID))
  #
  # if(!is.error(data_val_subset)){
  #  data_val_subset$calc_mean = data_val_subset$calc_mean + 21}
  data_val_sub_agg = try(aggregate(data_val_subset$calc_mean, by=list(data_val_subset$Date), FUN=mean))
  if(!is.na(rangedf_1)){
    #par(mfrow = c(1, 2))
    # par(mfrow =c(2, 2), mar = c(4, 4, 5, 4))
    # layout(matrix(c(1,1,1,1,1,0,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3), 2, 11, byrow = TRUE))
    
    # plot(c(rangedf_1[,1], rangedf_1[,2]), c(rangedf_1[,3], rangedf_1[,4]), col = "white", 
    #      main=paste("USGS Gage:", Site_number_xsections$`gageinfo$SITE_NUM`[i]),
    #      log = "", type="p", xlab="Q (cms)",
    #      ylab="W (m)",)
    
    
    #average rating curve
    q_rc = apply(xSecq, 2, FUN = mean)#,na.rm = TRUE)
    w_rc = apply(xSecw, 2, FUN = mean)#,na.rm = TRUE)
    
    #mx = which.max(xSecw$w0)
    mx = xSecw[order(xSecw$w50, decreasing = TRUE),]
    
    # 
    # wm = function(tbl){
    #   wm = weighted.mean(tbl, c(.8, .1, .05, .025, .025))
    #   return(wm)
    # }
    
     if(mean(tab$width_m[mInd], na.rm = TRUE)>600){
        w_rc = w_rc
      } else{w_rc = w_rc * 1.33}
    
    #w_rc = apply(mx, 2, FUN = wm)
    
    #w_rc = w_rc * 1.33
    
    q_sd = apply(xSecq, 2, FUN = sd)#, na.rm = TRUE)
    w_sd = apply(xSecw, 2, FUN = sd)#, na.rm = TRUE)
    print(w_sd)
    
    w_sd_avg = mean(w_sd, na.rm = TRUE)
    print(w_sd_avg)
    avg_w = mean(tab$width_m[mInd], na.rm = TRUE)
    
    ##background grid that shows the percentiles across the figure. ###probably not useful. 
    #abline(v = q_rc, col = "lightgray", lty = "dotted", lwd = 0.25)
    #abline(h = w_rc, col = "lightgray", lty = "dotted", lwd = 0.25)
    
    
    #lines(q_rc, w_rc, type = "l", lwd = 3) ##average xsection line before interpolation. 
    ##add in lines of rating curves to each plot. 
    for (k in 1:nrow(xSecq)){     
      #   #print(xSecq)
      #lines(as.numeric(xSecq[k,]), as.numeric(xSecw[k,]), type="l", lwd = 0.5, col = "blue")
      
      
      ##creates continuous dataset for each individual cross section. 
     ###spl_c = try(approxfun(as.numeric(xSecw[k,]), as.numeric(xSecq[k,]), ties = "ordered"))
      #try(lines(spl_c(as.numeric(xSecw[k,])), as.numeric(xSecw[k,]), col = k))
      
      ####create a new spline function for each rating curve. 
       spl_1=try(approxfun(as.numeric(xSecw[1,]), as.numeric(xSecq[1,])))#, method = "hyman", ties = "ordered"))
       spl_2=try(approxfun(as.numeric(xSecw[2,]), as.numeric(xSecq[2,])))#, method = "hyman", ties = "ordered"))
       spl_3=try(approxfun(as.numeric(xSecw[3,]), as.numeric(xSecq[3,])))#, method = "hyman", ties = "ordered"))
       spl_4=try(approxfun(as.numeric(xSecw[4,]), as.numeric(xSecq[4,])))#, method = "hyman", ties = "ordered"))
       spl_5=try(approxfun(as.numeric(xSecw[5,]), as.numeric(xSecq[5,])))#, method = "hyman", ties = "ordered"))
      
      ## Set up values for approx/splinefun. 
      y = q_rc
      x = w_rc
      #x = a3
      spl = try(approxfun(x, y)) #, method = "hyman")) #####either usse 'approxfun' or use splinefun with method = "hyman"
      # a=try(lines(spl(x), x , col = "black", lwd = 2)) ##average xsection line after interpolation. 
      #a = splinefun(x, y)
      #print(a)
      
      ####standard deviation boundaries around average rating curve. 
      # try(lines(spl(x), x+w_sd, col = "red"))
      # try(lines(spl(x), x - w_sd, col = "red"))
      
      ## create spline for estimating error bars below. 
      spl_sd = try(splinefun(x, w_sd))
    }
    #seqx = seq(1, 10, 0.1)
    #lines(seqx, spl(seqx))
    
    #logestimate = try(lm(lines_maybe[,1] ~ log(lines_maybe[,2])))
    #try(curve(logestimate))
    
    #logestimate = lm(xSecw[k] ~ log(xSecq[k]), na.exclude)
    #print(logestimate)
    #text(1, 2, xSecID[1:5])
    #text(as.numeric(xSecq[k,10]), as.numeric(xSecw[k,10]), xSecID[k])
    # legend("bottomright", 
    #        legend = c("Rating curve","Landsat widths", "USGS widths"), 
    #        col = c("black", "blue", 
    #                "lightgray"),
    #        lty=c(1, NA, NA),
    #        lwd = c(2, NA, NA),
    #        pch = c(NA, 17,01), 
    #        bty = "n", 
    #        text.col = "black", 
    #        horiz = F)
    #inset = c(0.9, 0.25)
    
    usgs = try(usgs_processing(read.csv(trial[i])))
    
    ##if above line works then add in the usgs width/discharge information. if the usgs data is missing then skip this step. 
    if(!is.error(usgs)){
      
      
      ###get percentile USGS discharge and width values and plot them. 
      u=as.data.frame(usgs)
      u_w_q = try(quantile(u$w_m, probs = seq(0, 1, .01), na.rm = TRUE))
      u_q_q = try(quantile(u$q_cms, probs = seq(0, 1, .01), na.rm = TRUE))
      #try(lines(u_q_q,u_w_q,type = "l", lty = 2, col = "red"))
      usgs_q = try(usgs_q_processing(read.csv(usgs_q_list[i], stringsAsFactors = FALSE)))
      
      if(!is.error(data_val_sub_agg) & !is.error(usgs_q)){
        usgs_q_ind=which(usgs_q$datetime %in% data_val_sub_agg$Group.1)
        usgs_q_subset= usgs_q[usgs_q_ind,]
        usgs_q_subset[order(usgs_q_subset$datetime),]
        data_val_sub_agg_ind=which(data_val_sub_agg$Group.1 %in% usgs_q_subset$datetime)
        data_val_sub_agg_1 = data_val_sub_agg[data_val_sub_agg_ind,]
        data_val_sub_agg_1[order(data_val_sub_agg_1$Group.1),]
        
        
        ds_df = data_val
        ds_df$spl1 = try(spl_1(ds_df$calc_mean))
        ds_df$spl2 = try(spl_2(ds_df$calc_mean))
        ds_df$spl3 = try(spl_3(ds_df$calc_mean))
        ds_df$spl4 = try(spl_4(ds_df$calc_mean))
        ds_df$spl5 = try(spl_5(ds_df$calc_mean))
        
        ds_df_agg = ds_df[,12 & 14:18] %>% group_by(Date) %>%
          mutate(grp = 1:n())%>%
          gather(var, val, -Date, -grp) %>%
          unite("var_grp", var, grp, sep ='') %>%
          spread(var_grp, val, fill = '')
        
        ds_df_spl_names = grep("^spl", colnames(ds_df_agg))
        ds_df_sd = apply(ds_df_agg[,ds_df_spl_names], 1, FUN = sd, na.rm = TRUE)
        ds_df_sd1 = cbind(as.vector(ds_df_agg$Date), as.vector(ds_df_sd))
        ds_df_sd1 = as.data.frame(ds_df_sd1)
        ds_df_sd1$date = as.Date(ds_df_sd1$V1)
        
        
        data_val_sub_agg_sd = try(aggregate(data_val_subset$calc_mean, by=list(data_val_subset$Date), FUN=sd))
        data_val_sub_agg_2 = data_val_sub_agg_1
        data_val_sub_agg_2$sd = data_val_sub_agg_sd$x[match(data_val_sub_agg_1$Group.1, data_val_sub_agg_sd$Group.1)]
        
        ## usgs width vs usgs discharge points. 
        # points(usgs_q_subset$q,data_val_sub_agg_1$x, pch=17, col="green")
        u_1=cbind(usgs_q_subset$q, data_val_sub_agg_1$x)
        u = as.data.frame(u_1)
        
        
        #data_Val_spl = spl(data_val_sub_agg_1$x)
        #try(print(spl(data_val_sub_agg_1$x)))
        dv = as.vector(nrow(data_val_sub_agg_1))
        sd_dv = try(spl_sd(data_val_sub_agg_1$x)) ##added
        dv = try(spl(data_val_sub_agg_1$x))
        
           # if(!is.error(dv)){
           #   sd_dv_df = cbind(dv, sd_dv) ##added
           #   sd_dv_out = sd_dv_df ##added
           #   sd_dv_out[,1][sd_dv_df[,2]> (avg_w * .2)] = NA ##added
           #   dv = try(sd_dv_out[,1]) ##added
           # }
           # 
        l = try(as.data.frame(cbind(dv, data_val_sub_agg_1$x)))
        
        #total = try(cbind(u$V1, l$dv, Site_number_xsections$`gageinfo$SITE_NUM`[i]))
        
        
        
        ## landsat estimated widths vs discharge from rating curve. 
        # try(points(dv, data_val_sub_agg_1$x, pch = 17, col = "blue"))
        
        ##add in all 2015-present landsat widths onto curve. Pretty much the same as above. 
        # dv_1 = as.vector(nrow(data_val_sub_agg))
        # dv_1 =try(spl(data_val_sub_agg$x))
        #try(points(dv_1, data_val_sub_agg$x, pch = 19, col = "green"))
        u_1 = try(which(u$w_m %in% l$V2))
        u_2 = u[u_1, ]
        
        
        ###add in error bars for each landsat estimated plot based on variation in nearest cross sections at that point. 
        # try(arrows(x0 = dv, y0= data_val_sub_agg_1$x - spl_sd(data_val_sub_agg_1$x), 
        #            x1 = dv, y1 = data_val_sub_agg_1$x + spl_sd(data_val_sub_agg_1$x), col = "black" , 
        #            code=3, angle = 90, length = 0.1))
        
        ###try to create a new plot for Qinsitu vs Qlandsat
        #if(!is.error(l)){
        # par(new = TRUE)
        #try(plot(u$V1, l$dv))} else{plot(1:10)}
        ###calculate and add R2 value to plot. 
        linearmodel = try(lm(u$V1 ~ l$dv))
        r_2 = try(summary(linearmodel)$r.squared)
        bb = try(bquote(R^2 ==.(format(round(r_2, 2)))))
        if(!is.error(r_2)){
          
        } else{next} 
        
        
        ###calculate and add pearsons correlation (R) to plot. 
        pearson = try(cor.test(u$V1, l$dv, method = "pearson"))
        r = try(pearson$estimate)
        rlabel = try(bquote(italic(R) == .(format(r, digits = 3))))
        if(!is.error(r)){
          par(mfrow =c(2, 2), mar = c(4, 4, 5, 4))
          #par(mfrow = c(1,3), mar = c(4,4,5,4))
          #layout(matrix(c(1,1,0,2,2,2,2,2,2,0,3,3), 1, 12, byrow = TRUE))
          #layout(matrix(c(1,1,0,2,2,2,2,2,2,0,3,3), 2, 12, byrow = TRUE))
        layout(matrix(c(1,1,1,1,1,0,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3), 2, 11, byrow = TRUE))
          try(plot(c(min(x-w_sd, na.rm = TRUE), max(x+w_sd, na.rm = TRUE)),c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)), col = "white", 
                   main=paste("USGS Gage:", Site_number_xsections$`gageinfo$SITE_NUM`[i]),
                   log = "", type="p", ylab="Discharge (cms)",
                   xlab="W (m)",))
          # 
          # plot(spl(x), x, col = "black", 
          #      main=paste("USGS Gage:", Site_number_xsections$`gageinfo$SITE_NUM`[i]),
          #      log = "", type="l", lwd = 2,xlab="Q (cms)",
          #      ylab="W (m)",)
          
          # try(lines(as.numeric(xSecq[1,]), as.numeric(xSecw[1,]), type="l", lwd = 0.5, col = "blue"))
          # try(lines(as.numeric(xSecq[2,]), as.numeric(xSecw[2,]), type="l", lwd = 0.5, col = "blue"))
          # try(lines(as.numeric(xSecq[3,]), as.numeric(xSecw[3,]), type="l", lwd = 0.5, col = "blue"))
          # try(lines(as.numeric(xSecq[4,]), as.numeric(xSecw[4,]), type="l", lwd = 0.5, col = "blue"))
          # try(lines(as.numeric(xSecq[5,]), as.numeric(xSecw[5,]), type="l", lwd = 0.5, col = "blue"))
          #try(lines(u_q_q,u_w_q,type = "l", lty = 2, col = "red"))
          
          #  
          # try(text(as.numeric(xSecq[1,100]), as.numeric(xSecw[1,100]), xSecID[1]))
          # try(text(as.numeric(xSecq[2,100]), as.numeric(xSecw[2,100]), xSecID[2]))
          # try(text(as.numeric(xSecq[3,100]), as.numeric(xSecw[3,100]), xSecID[3]))
          # try(text(as.numeric(xSecq[4,100]), as.numeric(xSecw[4,100]), xSecID[4]))
          # try(text(as.numeric(xSecq[5,100]), as.numeric(xSecw[5,100]), xSecID[5]))
          #  
          
          poly_bound_mx = cbind(x+w_sd, spl(x))
          poly_bound_mn = cbind(x-w_sd, spl(x))
          
          ##represents error margins from rating curves throughout plot. 
          polygon(c(poly_bound_mn[,1],  max(poly_bound_mx[,1], na.rm = TRUE), rev(poly_bound_mx[,1]), min(poly_bound_mn[,1], na.rm = TRUE))
                  ,c(poly_bound_mn[,2],  max(poly_bound_mx[,2], na.rm = TRUE), rev(poly_bound_mx[,2]), min(poly_bound_mn[,2], na.rm = TRUE)),
                   col = yarrr::transparent("red", trans.val = .95), border = NA)
          
          
          a=try(lines(x ,spl(x), col = "black", lwd = 2))
          #try(lines(x+w_sd, spl(x), col = "red"))
          #try(lines(x - w_sd, spl(x), col = "red"))
          #points(data_val_sub_agg_1$x,usgs_q_subset$q, pch=17, col="green")
          points(usgs[,2], usgs[,1], col = "lightgray", lwd = 0.5)
          try(points(data_val_sub_agg_1$x,dv,pch = 1, col = "blue"))
          #try(points(dv, data_val_sub_agg_1$x+ spl_sd(data_val_sub_agg_1$x), pch = 17, col = "blue"))
          
          ##new error bars: represent each day sd from landsat withds. 
          try(arrows(y0 = dv, x0= data_val_sub_agg_1$x - data_val_sub_agg_2$sd, 
                     y1 = dv, x1 = data_val_sub_agg_1$x + data_val_sub_agg_2$sd, col = "indianred" , 
                     code=3, angle = 180, length = 0, lwd = 0.5))
          
          ##old error bars: represent sd of rating curves
          # try(arrows(y0 = dv, x0= data_val_sub_agg_1$x - spl_sd(data_val_sub_agg_1$x), 
          #            y1 = dv, x1 = data_val_sub_agg_1$x + spl_sd(data_val_sub_agg_1$x), col = "indianred" , 
          #            code=3, angle = 180, length = 0, lwd = 0.5))
          # 
          
          location = auto.legend.pos(na.omit(x), na.omit(y))
          #location = plotrix::emptyspace(x,y)
   
          
          legend(location,
                 legend = c("Rating curve (1984-2014)","Landsat widths (2015-2020)",
                            "USGS widths"), 
                 col = c("black", "blue",
                         "lightgray"),
                 lty=c(1, NA, NA),
                 lwd = c(2, NA, NA),
                 pch = c(NA, 01, 01), 
                 bty = "n", 
                 text.col = "black", 
                 horiz = FALSE, cex = 0.6)
          
          
          
          
          u_l_mn = min(c(u$V1, l$dv), na.rm = TRUE)
          u_l_mx = max(c(u$V1, l$dv), na.rm = TRUE)
          
          try(plot(c(u_l_mn, u_l_mx), c(u_l_mn, u_l_mx), type = "n", xlab = "In situ Discharge (cms)", ylab = "Landsat Discharge (cms)", main = "In situ vs Landsat"))
          points(u$V1, l$dv, col = "blue")
          abline(1,1)
          #try(plot(u$V1, l$dv, xlab = "In situ Q (cms)", ylab = "Landsat Q (cms)", main = "In situ vs Landsat", asp = 1))
          r_2_label = paste("RMSE = ", round(rmse), "cms")

          try(mtext(side = 1, line = 6, adj = 1, text = bb, cex = 0.75))
          gage_stats_col2 = r_2
          gage_stats$R_2[i] = gage_stats_col2
          gage_stats_col1=Site_number_xsections[i,1]
          gage_stats$Site_number[i] = gage_stats_col1
          gage_stats_col7=length(na.omit(dv))
          gage_stats$n_Landsat_obs[i] = gage_stats_col7
          #try(mtext(side = 1, line = 6, adj = 1, text = rlabel))
          gage_stats_col3 = r
          gage_stats$R[i] = gage_stats_col3
          gage_stats_col9 = w_sd_avg
          gage_stats$avg_std[i] = gage_stats_col9
          gage_stats_col10 = mean(abs(tab$change[mInd]))
          gage_stats$change[i] = gage_stats_col10
        } else{next}
        
        ###calculate and add spearmans coefficient (p) to plot. 
        spearman = try(cor.test(u$V1, l$dv, method = "spearman"))
        p_val = try(spearman$p.value)
        plabel = try(bquote(italic(p) == .(format(p_val, digits = 3))))
        if(!is.error(p_val)){
          #try(mtext(side = 1, line = 6, adj = 0, text = plabel))
          gage_stats_col4 = p_val
          gage_stats$p_val[i] = gage_stats_col4} else{next}
        
        ###calculate and add rmse to plot. 
        error = try(u[,1] - l[,1])
        rmse = try(sqrt(mean(error^2, na.rm = TRUE)))
        #rmse_label = try(bquote(italic(RMSE) == .(format(rmse, digits = 5))))
        rmse_label = try(paste("RMSE = ", round(rmse), "cms"))
        # rrmse = mean(error/u$V1, na.rm = TRUE) * 100
        # #rrmse = rmse/(mean(u[,1], na.rm = TRUE)) *100
        # rrmse = sqrt((rrmse^2))
        # rrmse_median = median(error/u$V1, na.rm = TRUE) * 100
        # rrmse_median = sqrt((rrmse_median^2))
        
        rrmse = error/u$V1
        rrmse = sqrt((rrmse^2))
        rrmse = rrmse * 100
        rrmse = mean(rrmse, na.rm = TRUE)
        
        
        rrmse_median = error/u$V1
        rrmse_median = sqrt((rrmse_median^2))
        rrmse_median = rrmse_median * 100
        rrmse_median = median(rrmse_median, na.rm = TRUE)
        
        
        RRMSE_value = RRMSE(l$dv, u$V1)
        
        #rrmse_label = try(bquote(italic(RRMSE) == .(format(RRMSE_value, digits = 5))))
        rrmse_label = paste("RRMSE = ", round(RRMSE_value), "%", sep = "")
        l_vals[i,1:nrow(l)] = l[,1]
        u_vals[i,1:nrow(u)] = u[,1]
        if(!is.error(rmse)){
          try(mtext(side = 1, line = 6, adj = 0, text = rmse_label, cex = 0.75))
          #try(mtext(side = 2, line = 7.5, adj = 0, text = rrmse_label))
          try(mtext(side = 1, line = 4.5, adj = 1, text = rrmse_label, cex = 0.75))
          gage_stats_col5 = rmse
          gage_stats$RMSE[i] = gage_stats_col5
          gage_stats_col9 = rrmse
          gage_stats$RRMSE[i] = RRMSE_value
          gage_stats_col12 = rrmse_median
          gage_stats$RRMSE_median[i] = gage_stats_col12
        } else{next}
        
        ###calculate and add bias to plot
        error = try(l[,1] - u[,1])
        bias = mean(error, na.rm = TRUE)
        #bias_label = try(bquote(italic(Bias) == .(format(bias, digits = 5))))
        bias_label = paste("Bias =", round(bias), "cms")
        if(!is.error(bias)){
          try(mtext(side = 1, line = 4.5, adj = 0, text = bias_label, cex = 0.75))
          gage_stats_col6 = bias
          gage_stats$Bias[i] = gage_stats_col6
          gage_stats_col8 = tab$width_m[mInd[1]]
          gage_stats$GRWL_width_m[i] = gage_stats_col8} else{next}
        #l_df = data.frame(l$dv)
        # comb = cbind(l$dv, u$V1)
        # cumulative_df_l = append(cumulative_df_l, comb)
        # 
        # 
        # #u_df = data.frame(u$V1)
        # cumulative_df_u = append(cumulative_df_u, comb[,2])
        # #if(!is.error(r_2)){
        #try(plot(u$V1, l$dv, xlab = "In situ Q (cms)", ylab = "Landsat Q (cms)", main = "In situ vs Landsat"))} else{next}
        
        ##add in hydrographs: first is 2015-present. 
        if(nrow(usgs_q_subset) >0){
          #par(new = TRUE)
          
          
          ##add in date field to usgs data and filter to 2015- present.  
          usgs_q$date = as.Date(usgs_q$datetime, "%Y-%m-%d")
          usgs_q_2015 = usgs_q %>%
            filter(usgs_q$date > as.Date('2015-01-01') & usgs_q$date < as.Date('2020-12-31'))
          
          
          ##add in date field to validation data and assign to corresponding usgs days.
          data_val_sub_agg_1$date = as.Date(data_val_sub_agg_1$Group.1, "%Y-%m-%d")
          
          
          #### Calculate Standard Dev for Q based on SD from using each individual rating curve. 
          ry = try(cbind(spl_1(data_val_sub_agg_1$x), spl_2(data_val_sub_agg_1$x), spl_3(data_val_sub_agg_1$x), spl_4(data_val_sub_agg_1$x), spl_5(data_val_sub_agg_1$x)))
          
          
          ry1 = data_val_subset %>% group_by(Date) %>%
            mutate(grp = 1:n())%>%
            gather(var, val, -Date, -grp) %>%
            unite("var_grp", var, grp, sep ='') %>%
            spread(var_grp, val, fill = '')
          
          ry1_cols = colnames(ry1) 
          width_cols_landsat = grep("calc" ,colnames(ry1))
          
          ry1_widths = ry1[,width_cols_landsat]
          
          ry2 = cbind(ry1$Date, ry1[,width_cols_landsat])
          ry2_ind = ry2$`ry1$Date` %in% usgs_q_subset$datetime
          ry2 = ry2[ry2_ind,]
          ry2_ncols = ncol(ry2)
          #ry3 = cbind(ry2, data_val_sub_agg_1)
          est = as.data.frame(ry2)
          
          for (r in 2:ncol(ry2)){
            est[r] = spl(ry2[,r])
          }
          
          mx = apply(est[2:ry2_ncols], 1, max, na.rm = TRUE)
          mn = apply(est[2:ry2_ncols], 1, min, na.rm = TRUE)
          sd = apply(est[2:ry2_ncols], 1, sd, na.rm = TRUE)
          mx_mn = cbind(mx, mn, sd)
          mx_mn[mapply(is.infinite, mx_mn)] <- NA
          
          
          
          data_val_sub_agg_1 = cbind(data_val_sub_agg_1, mx_mn)
          
          
          
          
          
          # ry_c = cbind(dv, ry)
          # 
          # 
          # ry_min = apply(ry, 1, FUN = min, na.rm = TRUE)
          # ry_max = apply(ry, 1, FUN = max, na.rm = TRUE)
          # ry_c1 = cbind(ry_c, ry_min, ry_max)
          # 
          # #ry_max = apply(ry, 1, function(x) which (x==max(x)))
          # #ry_min = apply(ry, 1, function(x) which (x==min(x)))
          # 
          # data_val_sub_agg_1$max = ry_max
          # data_val_sub_agg_1$min = ry_min
          usgs_q_2015_wval = left_join(usgs_q_2015, data_val_sub_agg_1, copy = FALSE)
          usgs_q_2015_wval$landat = spl(usgs_q_2015_wval$x)
          
 
          usgs_q_2015_wval$mx[mapply(is.na, usgs_q_2015_wval$landat)] = NA
          usgs_q_2015_wval$mn[mapply(is.na, usgs_q_2015_wval$landat)] = NA
          usgs_q_2015_wval$sd[mapply(is.na, usgs_q_2015_wval$landat)] = NA
          usgs_q_2015_wval$sd_mx = usgs_q_2015_wval$landat + usgs_q_2015_wval$sd
          usgs_q_2015_wval$sd_mn = usgs_q_2015_wval$landat - usgs_q_2015_wval$sd
          
          ############### new sd. 
          ds_join = left_join(usgs_q_2015_wval, ds_df_sd1)
          usgs_q_2015_wval = ds_join
          usgs_q_2015_wval$V2[mapply(is.na, usgs_q_2015_wval$landat)] = NA
          usgs_q_2015_wval$V2 = as.numeric(as.character(usgs_q_2015_wval$V2))
          usgs_q_2015_wval$sd_mx1 = usgs_q_2015_wval$landat + usgs_q_2015_wval$V2
          usgs_q_2015_wval$sd_mn1 = usgs_q_2015_wval$landat - usgs_q_2015_wval$V2
          
          
          ###### insitu vs landsat error updated to new error bars. 
          in_situ_sd = usgs_q_2015_wval[!is.na(usgs_q_2015_wval$landat),]
          in_situ_sd_mx = in_situ_sd$sd_mx1[order(in_situ_sd$q)]
          in_situ_sd_mn = in_situ_sd$sd_mn1[order(in_situ_sd$q)]
          
          
          ####changed the gage_stats sd
          gage_stats_col13 = mean(usgs_q_2015_wval$V2)
          #gage_stats_col13 = mean(usgs_q_2015_wval$sd, na.rm = TRUE)
          gage_stats$std_Q[i] = gage_stats_col13
          gage_stats_col14 = sqrt(var(error, na.rm = TRUE))
          gage_stats$STDE[i] = gage_stats_col14
          mean_q = mean(usgs_q_2015_wval$landat, na.rm = TRUE)
          
 
          

          
          try(arrows(x0 = in_situ_sd$q[order(in_situ_sd$q)], y0=  in_situ_sd_mn - mean_q, 
                     x1 = in_situ_sd$q[order(in_situ_sd$q)], y1 = in_situ_sd_mx + mean_q, col = "indianred",
                     code=3, angle = 180, length = 0, lwd = 0.5))
          
          try(plot(usgs_q_2015_wval$date, usgs_q_2015_wval$q, xlab = "", ylab = "Discharge (cms)", type = "l", col = "lightgray"))
          
          try(points(usgs_q_2015_wval$date,usgs_q_2015_wval$landat, col = "blue"))
          
          
          ##represent errors from determing each Q across all rating curves. Maybe too much? 
          try(arrows(x0 = usgs_q_2015_wval$date, y0=  usgs_q_2015_wval$sd_mn1 - mean_q, 
                     x1 = usgs_q_2015_wval$date, y1 = usgs_q_2015_wval$sd_mx1 + mean_q, col = "indianred",
                     code=3, angle = 180, length = 0, lwd = 0.5))
          
          ##old error bars. represent sd from using 5 ind rating curves to determine Q. 
          # try(arrows(x0 = usgs_q_2015_wval$date, y0= usgs_q_2015_wval$sd_mn - mean_q, 
          #             x1 = usgs_q_2015_wval$date, y1 = usgs_q_2015_wval$sd_mx + mean_q, col = "indianred",
          #             code=3, angle = 180, length = 0, lwd = 0.5))
          
          
          ####change sd vals to new sd. 
          sd_vals[i, 1:length(usgs_q_2015_wval$V2[!is.na(usgs_q_2015_wval$landat)])] = usgs_q_2015_wval$V2[!is.na(usgs_q_2015_wval$landat)]
          
          #sd_vals[i, 1:length(usgs_q_2015_wval$sd[!is.na(usgs_q_2015_wval$landat)])] = usgs_q_2015_wval$sd[!is.na(usgs_q_2015_wval$landat)]
          
        
          #try(plot(smooth.spline(na.omit(dv), spar = 0.3), ylim = c(q_min_plot, q_max_plot), type = "l", xlab = "2015-Present", ylab = "Q (cms)", xaxt = 'n', main = "Hydrograph 2015-Present",col= "red"))
          title("Hydrograph 2015-2020", line = 0.5)
          
          
          #par(new = TRUE)
          #plot(smooth.spline(usgs_q_subset$q, spar = 0.25), type = "l", xlab = "", ylab = "", axes = F)
          #axis(4)
          #points(dv, col = "blue", pch = 16)
          legend("bottom", inset = c(0, -.2), legend = c("Rating curve", "In situ"),
                 col = c("blue", "lightgray"),lty = c(NA, 1), pch = c(1, NA), bty = "n", xpd = TRUE, horiz = TRUE)
          
          # try(plot(c(u_l_mn, u_l_mx), c(u_l_mn, u_l_mx), type = "n", xlab = "In situ Q (cms)", ylab = "Landsat Discharge (cms)", main = "In situ vs Landsat"))
          # points(u$V1, l$dv, col = "blue")
          # abline(1,1)
          
          #try(plot(usgs[,1], type = "l", xlab = "", ylab = ""))
          
          ###add in hydrographs for entire time period of gage. 
          ##smooth discharge measurements and plot them. 
          
          
          #try(points(spl(usgs[,2]), col = "blue"))
          #try(lines(spl(usgs[,2]), col = "green"))
          
          
        }
        else{
          next}
        
      } else{
        next}
      
      
    }else{
      #title(main = NULL, sub = "No USGS Data")
      #mtext(side =3, line = 0.5, "No USGS Data")
      #text(y[5],x[5], pos = 4, offset = 9, "No USGS Data") 
      next}
    ##### for individual rating curves. 
  }else{next}
}
dev.off()
cmd = paste('open', pdfOut)
system(cmd)


#write.csv(u_vals, "E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\e_widths_1percentiles\\Values_u_l\\u_vals_occ.csv")
#write.csv(l_vals, "E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\e_widths_1percentiles\\Values_u_l\\l_vals_occ.csv")
gage_stats$RRMSE[mapply(is.infinite, gage_stats$RRMSE)] <- NA


plot.new()
par(mfrow =c(1,1))
bias_mn = min(c(abs(gage_stats$Bias), gage_stats$std_Q), na.rm = TRUE)
bias_mx = max(c(abs(gage_stats$Bias), gage_stats$std_Q), na.rm = TRUE)
plot(c(bias_mn, bias_mx), c(bias_mn, bias_mx), type = "n", log = "xy")
points(abs(gage_stats$Bias), gage_stats$std_Q)
abline(0,1)
#abline(1, 1)
abline(lm(log(abs(gage_stats$Bias))~ log(gage_stats$std_Q)), col = "red")

##RMSE = sqrt STDE ^2 + abs(BIAS^2)
plot((gage_stats$RMSE), sqrt((gage_stats$STDE^2) + abs(gage_stats$Bias^2)))

plot(((gage_stats_83$RRMSE/100)^2), ((MRR_df[,1]^2) + (SDRR_df[,1]^2)))
abline(1, 1)



e_df = u_vals - l_vals
error_df = apply(e_df, 1, FUN = mean, na.rm = TRUE)

relative_residuals = e_df/u_vals
######################################################################################### RRMSE vs SDRR/MRR
plot(ecdf(abs(SDRR_df$V1)), col = "red", xlim = c(0, 10), main = "")
lines(ecdf(abs(MRR_df$V1)), col = "blue")
lines(ecdf(abs(gage_stats_83$RRMSE/100)), col = "forestgreen")




########################################################################################## Avg. Model Q for each Gauge.

Q_df = apply(l_vals, 1, FUN = mean, na.rm = TRUE)


########################################################################################## Actual cumulative error statistics across all gauges for entire timeperiod. 
norm_rmse = gage_stats$RMSE/Q_df
norm_bias = abs(gage_stats$Bias)/Q_df
norm_stde = gage_stats$STDE/Q_df

norm_stde = norm_stde[is.na(Total_gage_stats_dams$dam) & !is.na(Total_gage_stats_dams$Site_number & Total_gage_stats_dams$change<10)]


plot.new()
par(mfrow = c(1, 1))
plot(ecdf(norm_rmse), xlim = c(0, 5), main = "Actual cumulative error statistics", col = "forestgreen", cex = 0.15)
#abline(h = 0.5)
#abline(v = 1.1)
lines(ecdf(norm_bias), col = "gold", cex = 0.15)
lines(ecdf(norm_stde), col = "blue", cex = 0.15)
points(median(norm_stde, na.rm = TRUE), 0.5, pch = 4, col = "blue", cex = 1.5, lwd = 2)
text(median(norm_stde, na.rm = TRUE), 0.5, labels = paste("(", paste(signif(median(norm_stde, na.rm = TRUE), 2), "0.5", sep = ","), ")", sep = ""), pos = 3,
     cex = 0.75)
points(median(norm_bias, na.rm = TRUE), 0.5, pch = 4, col = "gold", cex = 1.5, lwd = 2)
text(median(norm_bias, na.rm = TRUE), 0.5, labels = paste("(", paste(signif(median(norm_bias, na.rm = TRUE), 2), "0.5", sep = ","), ")", sep = ""), pos = 1,
     cex = 0.75)
points(median(norm_rmse, na.rm = TRUE), 0.5, pch = 4, col = "forestgreen", cex = 1.5, lwd = 2)
text(median(norm_rmse, na.rm = TRUE), 0.5, labels = paste("(", paste(signif(median(norm_rmse, na.rm = TRUE), 2), "0.5", sep = ","), ")", sep = ""), pos = 4,
     cex = 0.75)
legend("bottomright", legend = c("Normalized Bias", "Normalized STDE", "Normalized RMSE"),
       col = c("gold", "blue", "forestgreen"), lty = c(1, 1, 1))

################################################################################################ Estimated Error Metrics. 
GRADES_b = Q_df * 0.5 #0.2
Total_estimated_error = GRADES_b + sd_vals

error_estimate = l_vals[!is.na(l_vals)] - sd_vals
mean_error_estimate = apply(Total_estimated_error, 1, FUN = mean, na.rm = TRUE)
STDE_estimate = sqrt(apply(Total_estimated_error, 1, FUN = var, na.rm = TRUE))
STDE_estimate[STDE_estimate==0] = NA

rmse_estimate = Total_estimated_error^2
rmse_estimate = sqrt(apply(rmse_estimate, 1, FUN = mean, na.rm = TRUE))

plot((rmse_estimate), sqrt((STDE_estimate^2) + abs(mean_error_estimate^2))) ### shows proper relationship. They are all calculated consistently. 

####Normalized estimated values. 

norm_rmse_est = rmse_estimate/Q_df
norm_bias_est = abs(mean_error_estimate)/Q_df
norm_stde_est = STDE_estimate/Q_df

norm_stde_est = norm_stde_est[is.na(Total_gage_stats_dams$dam) & !is.na(Total_gage_stats_dams$Site_number) & Total_gage_stats_dams$change<10]


############################################################################################### Cedric's plots. Accounting for more Bias. 

plot.new()
par(mfrow =c(2,2), oma = c(0, 0, 2, 0))
plot(ecdf(norm_rmse_est), xlim = c(0, 5), col = "forestgreen", main = "Cumulated estimated error metrics", cex.main = 0.8, cex = 0.15)
mtext("Estimated error metrics", outer = TRUE, cex = 1.5)
lines(ecdf(norm_bias_est), col = "gold", cex = 0.15)
lines(ecdf(norm_stde_est), col = "blue", cex = 0.15)
points(median(norm_stde_est, na.rm = TRUE), 0.5, pch = 4, col = "blue", cex = 1.5, lwd = 2)
text(median(norm_stde_est, na.rm = TRUE), 0.5, labels = paste("(", paste(signif(median(norm_stde_est, na.rm = TRUE), 2), "0.5", sep = ","), ")", sep = ""), pos = 3,
     cex = 0.75)
points(median(norm_bias_est, na.rm = TRUE), 0.5, pch = 4, col = "gold", cex = 1.5, lwd = 2)
text(median(norm_bias_est, na.rm = TRUE), 0.5, labels = paste("(", paste(signif(median(norm_bias_est, na.rm = TRUE), 2), "0.5", sep = ","), ")", sep = ""), pos = 1,
     cex = 0.75)
points(median(norm_rmse_est, na.rm = TRUE), 0.5, pch = 4, col = "forestgreen", cex = 1.5, lwd = 2)
text(median(norm_rmse_est, na.rm = TRUE), 0.5, labels = paste("(", paste(signif(median(norm_rmse_est, na.rm = TRUE), 2), "0.5", sep = ","), ")", sep = ""), pos = 4,
     cex = 0.75)
legend("bottomright", legend = c("Normalized Bias", "Normalized STDE", "Normalized RMSE"),
       col = c("gold", "blue", "forestgreen"), lty = c(1, 1, 1), cex = 0.75)

bias_mn = min(c(abs(gage_stats$Bias), abs(mean_error_estimate)), na.rm = TRUE)
bias_mx = max(c(abs(gage_stats$Bias), abs(mean_error_estimate)), na.rm = TRUE)
plot(c(bias_mn, bias_mx), c(bias_mn, bias_mx), type = "n", log = "xy", xlab = "Q error", ylab = "Q error",
     main = "Estimated errors as a function of actual errors", cex.main = 0.8)
points(abs(gage_stats$Bias), abs(mean_error_estimate), col = "gold")
bias_lm = lm(log(abs(mean_error_estimate))~log(abs(gage_stats$Bias)))
bias_seq = seq(bias_mn, bias_mx)
bias_y_seq = exp(bias_lm$coefficients[1]) * bias_seq^bias_lm$coefficients[2]
lines(bias_seq, bias_y_seq, col = "gold")
bias_legend = paste("y=", signif(bias_lm$coefficients[2], 2), sep = "")
bias_legend = paste0(bias_legend, "x")
bias_legend = paste(bias_legend,"R^2",sep = ",")
bias_legend = paste0(bias_legend, "=", signif(summary(bias_lm)$r.squared, 2))
legend("topleft", legend = c("Bias", bias_legend),
       col = c("gold", "gold"), pch = c(1, NA), lty = c(NA, 1), bty = "n")


stde_mn = min(c(gage_stats$STDE, STDE_estimate), na.rm = TRUE)
stde_mx = max(c(gage_stats$STDE, STDE_estimate), na.rm = TRUE)
plot(c(stde_mn, stde_mx), c(stde_mn, stde_mx), type = "n", log = "xy", xlab = "Q error", ylab = "Q error",
     main = "Estimated errors as a function of actual errors", cex.main = 0.8)
points(gage_stats$STDE, STDE_estimate, col = "blue")
#abline(1, 1)
stde_lm = lm(log(STDE_estimate)~log(gage_stats$STDE))
stde_seq = seq(stde_mn, stde_mx)
stde_y_seq = exp(stde_lm$coefficients[1]) * stde_seq^stde_lm$coefficients[2]
stde_legend = paste("y=", signif(stde_lm$coefficients[2], 2), sep = "")
lines(stde_seq, stde_y_seq, col = "blue")
stde_legend = paste0(stde_legend, "x")
stde_legend = paste(stde_legend,"R^2",sep = ",")

stde_legend = paste0(stde_legend, "=", signif(summary(stde_lm)$r.squared, 2))
legend("topleft", legend = c("STDE", stde_legend),
       col = c("blue", "blue"), pch = c(1, NA), lty = c(NA, 1), bty = "n")

rmse_mn = min(c(gage_stats$RMSE, rmse_estimate), na.rm = TRUE)
rmse_mx = max(c(gage_stats$RMSE, rmse_estimate), na.rm = TRUE)
plot(c(rmse_mn, rmse_mx), c(rmse_mn, rmse_mx), type = "n", log = "xy", xlab = "Q error", ylab = "Q error",
     main = "Estimated errors as a function of actual errors", cex.main = 0.8)
points(gage_stats$RMSE, rmse_estimate, col = "forestgreen")
rmse_lm = lm(log(rmse_estimate)~log(gage_stats$RMSE))
rmse_seq = seq(rmse_mn, rmse_mx)
rmse_y_seq = exp(rmse_lm$coefficients[1]) * rmse_seq^rmse_lm$coefficients[2]
lines(rmse_seq, rmse_y_seq, col = "forestgreen")
rmse_legend = paste("y=", signif(rmse_lm$coefficients[2], 2), sep = "")
rmse_legend = paste0(rmse_legend, "x")
rmse_legend = paste(rmse_legend,"R^2",sep = ",")

rmse_legend = paste0(rmse_legend, "=", signif(summary(rmse_lm)$r.squared, 2))
legend("topleft", legend = c("RMSE", rmse_legend),
       col = c("forestgreen", "forestgreen"), pch = c(1, NA), lty = c(NA, 1), bty = "n")
##################################################################################################################




l_vals_filt = l_vals[!is.na(l_vals),]
l_vals_filt = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))




norm_rmse_e = gage_stats_83$std_Q/Q_df
norm_bias_e = abs(gage_stats$Bias)/Q_df
norm_stde_e = gage_stats$STDE/Q_df

n_rmse_cum = ecdf(norm_rmse)

plot(ecdf(norm_rmse_e), xlim = c(0, 2), main = NA, col = "forestgreen")
abline(h = 0.5)
abline(v = 1.1)
lines(ecdf(norm_bias), col = "gold")
lines(ecdf(norm_stde), col = "blue")






plot(norm_rmse[order(norm_rmse)], 1:length(norm_rmse), type = "l", log = "x", col = "forestgreen")
lines(norm_bias[order(norm_bias)],1:length(norm_bias), col = "gold")
lines(norm_stde[order(norm_stde)],1:length(norm_stde), col = "blue")



gage_stats$usgs_mean = apply(u_vals, 1, FUN = mean, na.rm = TRUE)
gage_stats$Landsat_mean = apply(l_vals, 1, FUN = mean, na.rm = TRUE)


MRR_df =  as.data.frame(matrix(numeric(), nrow = nrow(gage_stats), ncol = 1))
SDRR_df = as.data.frame(matrix(numeric(), nrow = nrow(gage_stats), ncol = 1))
RMSE_df = as.data.frame(matrix(numeric(), nrow = nrow(gage_stats), ncol = 1))
NRRMSE_df = as.data.frame(matrix(numeric(), nrow = nrow(gage_stats), ncol = 1))
rBias_df = as.data.frame(matrix(numeric(), nrow = nrow(gage_stats), ncol = 1))
error_df = as.data.frame(matrix(numeric(), nrow = nrow(gage_stats), ncol = 1))
Q_df = as.data.frame(matrix(numeric(), nrow = nrow(gage_stats), ncol = 1))


##updated RRMSE
for (i in 1:nrow(u_vals)){
  #MRR_df[i,] = mean(as.numeric((l_vals[i,] - u_vals[i,]/as.numeric(u_vals[i,]))), na.rm = TRUE)
  #SDRR_df[i,] = sd(as.numeric((l_vals[i,] - u_vals[i,]/as.numeric(u_vals[i,]))), na.rm = TRUE)
  rrmse = as.numeric((l_vals[i,] - u_vals[i,]))
  nrmse = rrmse/as.numeric(u_vals[i,])
  #rrmse = sqrt((nrmse^2))
  #rrmse = rrmse * 100
  #rrmse = mean(rrmse, na.rm = TRUE)
  rrmse = (nrmse^2)
  rrmse = mean(rrmse, na.rm = TRUE)
  rrmse = sqrt(rrmse)
  NRRMSE_df[i,] = rrmse
}  

gage_stats$MRR = MRR_df$V1
gage_stats$SDRR = SDRR_df$V1
gage_stats$RRMSE_real = NRRMSE_df$V1

rrmse = as.numeric((l_vals[i,] - u_vals[i,]))
nrmse = rrmse/as.numeric(u_vals[i,])
#rrmse = sqrt((nrmse^2))
#rrmse = rrmse * 100
#rrmse = mean(rrmse, na.rm = TRUE)
rrmse = (nrmse^2)
rrmse = mean(rrmse, na.rm = TRUE)
rrmse = sqrt(rrmse)
NRRMSE_df[i,] = rrmse

for (i in 1:nrow(u_vals)){
  # Mr = as.numeric((l_vals[i,] - u_vals[i,]))
  # Mr = Mr/as.numeric(u_vals[i,])
  # MRR_df[i,] = mean(Mr, na.rm = TRUE)
  # SDRR_df[i,] = sd(Mr, na.rm = TRUE)
  # rrmse = as.numeric((l_vals[i,] - u_vals[i,]))
  # nrmse = rrmse/(mean(as.numeric(u_vals[i,]),na.rm = TRUE))
  #rrmse = sqrt((nrmse^2))
  #rrmse = rrmse * 100
  #rrmse = mean(rrmse, na.rm = TRUE)
  #rrmse = (nrmse^2)
  #rrmse = mean(rrmse, na.rm = TRUE)
  #rrmse = sqrt(rrmse)
  #NRRMSE_df[i,] = mean(nrmse, na.rm = TRUE)
  rBias_df[i,] = rBias(l_vals[i,], u_vals[i,])
}  

gage_stats$rel_Bias = NRRMSE_df$V1

gage_stats$rBias = rBias_df$V1




gage_stats1 = gage_stats[,13:24]

gage_stats = gage_stats1




a = read.csv(trial[i])
a1 = as.vector(a$chan_width)
a2 = as.numeric(a1) * 0.3048
a3 = quantile(a2, probs = seq(0, 1, .01), na.rm = TRUE)
print(a3)



##see what percentage off they are. 
ryan = usgs_q_2015_wval
ryan$above = ((ryan$landat * .2) - ryan$landat)*-1


try(points(ryan$date,ryan$above, col = "red"))



gage_flags = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\2015_2020\\Stats\\2015_2020stats_1perc_adjusted_dam_flags.csv")


u_vals$Site = Site_number_xsections$`gageinfo$SITE_NUM`
u_vals_ind = u_vals[!is.na(u_vals[,1]),]
l_vals_ind = l_vals[row.names(u_vals_ind),]
#u_vals_widths = u_vals

#u_vals_widths$Site = Site_number_xsections$`gageinfo$SITE_NUM`
#u_vals_widths$widths = gage_stats$GRWL_width_m[match(u_vals_widths$Site, gage_stats$Site_number)]

u_vals_ind1 = u_vals_ind$Site %in% gage_stats_change_filter$Site_number
u_vals_ind = u_vals_ind[u_vals_ind1,]
l_vals_ind = l_vals[row.names(u_vals_ind),]


u_cols = ncol(u_vals_ind) - 1

u_mx = max(u_vals_ind[,1:u_cols], na.rm = TRUE)
u_mn = min(u_vals_ind[,1:u_cols], na.rm = TRUE)

l_mx = max(l_vals_ind, na.rm = TRUE)
l_mn = min(l_vals_ind, na.rm = TRUE)



plot.new()
par(mfrow = c(1,1))
plot(c(u_mn, u_mx), c(l_mn, l_mx), type = "n", asp = 1)
abline(1,1)
for (i in 1:nrow(u_vals_ind)){
  for(j in 1:u_cols){
    points(u_vals_ind[i,j], l_vals_ind[i,j])
  }
}

e_df = (u_vals_ind[,1:u_cols] - l_vals_ind)
e_df = e_df / u_vals_ind[,1:u_cols]
e_df = sqrt((e_df^2))
e_df = e_df * 100
e_v = unlist(e_df)






####only keep plots that produce statistics. 
#gage_stats = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol =12))
#gage_stats_GRADES = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol =12))
colnames(gage_stats_GRADES)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "p_val","Bias")
as.data.frame(gage_stats_GRADES)



colnames(gage_stats)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "p_val","Bias")
as.data.frame(gage_stats)
gage_stats_col1 = as.vector(1)
gage_stats_col2 = as.vector(1)
gage_stats_GRADES_col1 = as.vector(1)
gage_stats_GRADES_col2 = as.vector(1)
pdfOut = "E:/research/temp_plots/filtered_eastern_2013_GR&Val.pdf"
pdf(pdfOut)




Site_number_xsections_real = Site_number_xsections


ry = Site_number_xsections_real[Site_number_xsections_real$`gageinfo$SITE_NUM`==2469761,]
Site_number_xsections= ry

Site_number_xsections = Site_number_xsections_real














gage_stats_output= na.omit(gage_stats)
gage_stats_output$RRMSE[mapply(is.infinite, gage_stats_output$RRMSE)] <- NA
gage_stats_output = na.omit(gage_stats_output)
gage_stats_output$R = as.numeric(gage_stats_output$R)
gage_stats_output$dam_flags = gage_flags$Dam_flag[match(gage_stats_output$Site_number, gage_flags$Site_number)]
gage_stats_output$dam_flags[mapply(is.na, gage_stats_output$dam_flags)] <- 0

gage_stats_GRADES_output = na.omit(gage_stats_GRADES[,1:9])
gage_stats_GRADES_output$RRMSE_median = na.omit(gage_stats_GRADES[,12])

ryan = gage_stats_output %>%
          filter(gage_stats_output$RRMSE < 40)
a = getJenksBreaks(gage_stats_change_filter$GRWL_width_m, 5)





##plot stats. 
stat_names = colnames(gage_stats_output)
for( i in 1:ncol(gage_stats_output)){
  plot(gage_stats_output$GRWL_width_m, gage_stats_output[,i], main = paste(stat_names[i]))
}

##filter for gages wider than 99 m and plot. 
gage_stats_filter = gage_stats_output %>%
  filter(gage_stats_output$GRWL_width_m > 99 & gage_stats_output$n_Landsat_obs > 9) ###### change back to 9.
for( i in 1:ncol(gage_stats_filter)){
  plot(gage_stats_filter$GRWL_width_m, gage_stats_filter[,i], main = paste(stat_names[i]))
  abline(lm(gage_stats_filter[,i]~gage_stats_filter$GRWL_width_m))
}

#####plot GRADES stats
gage_stats_GRADES_output = na.omit(gage_stats_GRADES)
rownames(gage_stats_GRADES_output) = NULL


for( i in 1:ncol(gage_stats_GRADES_output)){
  plot(gage_stats_GRADES_output$GRWL_width_m, gage_stats_GRADES_output[,i], main = paste(stat_names[i]))
  points(gage_stats_output$GRWL_width_m, gage_stats_output[,i], col = "red")
}

####plot GRADES stats vs Landsat stats
for( i in 1:ncol(gage_stats_GRADES_output)){
  plot(gage_stats_GRADES[,i], gage_stats[,i], main = paste(stat_names[i]))
}





##filter for data >99 m. 
gage_stats_GRADES_filter = gage_stats_GRADES_output %>%
  filter(gage_stats_GRADES_output$GRWL_width_m > 99)
for( i in 1:ncol(gage_stats_GRADES_filter)){
  plot(gage_stats_GRADES_filter$GRWL_width_m, gage_stats_GRADES_filter[,i], main = paste(stat_names[i]))
  points(gage_stats_filter$GRWL_width_m, gage_stats_filter[,i], col = "red")
}

###averages on all statistics of all gages. 
GRADES_stats_avg = apply(gage_stats_GRADES_output, 2, FUN = mean) #,na.rm = TRUE)
Gage_stats_avg = apply(gage_stats_output, 2, FUN = mean) #,na.rm = TRUE)

###averages on all statistics of gages >99 m. 
GRADES_stats_filt_avg = apply(gage_stats_GRADES_filter, 2, FUN = mean) #,na.rm = TRUE)
Gage_stats_filt_avg = apply(gage_stats_filter, 2, FUN = mean) #,na.rm = TRUE)




Miss_gages_ind = gage_stats_output[gage_stats_output$Site_number==7374525 | gage_stats_output$Site_number==7374000 |
                                     gage_stats_output$Site_number==7289000 |gage_stats_output$Site_number==7265450 |
                                     gage_stats_output$Site_number==70320000 |gage_stats_output$Site_number==70722000 |
                                     gage_stats_output$Site_number==7020500 |gage_stats_output$Site_number==7010000 |
                                     gage_stats_output$Site_number==5587500 |gage_stats_output$Site_number==5587455 |
                                     gage_stats_output$Site_number==5587450 |gage_stats_output$Site_number==5474500
                                   |gage_stats_output$Site_number==5420500|gage_stats_output$Site_number==5420460
                                   |gage_stats_output$Site_number==5411500|gage_stats_output$Site_number==5383500
                                   |gage_stats_output$Site_number==5355250|gage_stats_output$Site_number==5344500
                                   |gage_stats_output$Site_number==5344490,] 


write.csv(gage_stats_output, "E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\2015_2020\\Stats\\2015_2020stats_1perc.csv")

###with GRADES plotted. 



gage_stats_output_1 = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\2015_2020\\Stats\\2015_2020stats_4000comids_1perc.csv")

Gage_stats_1_avg = apply(gage_stats_output_1, 2, FUN = mean)

gage_stats_dam_filter = gage_stats_filter %>%
  filter(gage_stats_filter$dam_flags==0)
print(apply(gage_stats_dam_filter, 2, FUN= mean))

gage_stats_change_filter = gage_stats_dam_filter %>%
  filter(gage_stats_dam_filter$change < 10)
print(apply(gage_stats_change_filter, 2, FUN= median))

gage_stats_std_filter = gage_stats_change_filter %>%
  filter(gage_stats_change_filter$avg_std < gage_stats_change_filter$GRWL_width_m * .1)
print(apply(gage_stats_std_filter, 2, FUN= mean))

write.csv(ryan, "E:\\research\\GRWL\\GRWL_2015_present\\E_validation\\1spc_3x\\stats\\Filtered_change_eastern_stats_1perc.csv")




for( i in 1:ncol(gage_stats_change_filter)){
  plot(gage_stats_change_filter$GRWL_width_m, gage_stats_change_filter[,i], main = paste(stat_names[i]))
}


plot(gage_stats_change_filter$n_Landsat_obs, gage_stats_change_filter$RRMSE)

gage_stats_output_corrected_filtered = gage_stats_change_filter






station_ind = gageinfo$SITE_NUM %in% gage_stats_output$Site_number
stations = gageinfo[station_ind,]
ryan = cbind(gage_stats_output, stations)

ryan1 = st_as_sf(ryan, coords = c("LONG", "LAT"))

##filter grades stats to only where filtered rating curve data available. 
GRADES_corr = gage_stats_GRADES_filter$Site_number %in% gage_stats_change_filter$Site_number
GRADES_correlation = gage_stats_GRADES_filter[GRADES_corr,]
apply(GRADES_correlation,2, FUN = median, na.rm = TRUE)


gage_corr = gage_stats_change_filter$Site_number %in% GRADES_correlation$Site_number
gage_correlation = gage_stats_change_filter[gage_corr,]


plot.new()
par(mfrow = c(1,1))
plot(GRADES_correlation$GRWL_width_m, GRADES_correlation$RRMSE)
points(gage_stats_change_filter$GRWL_width_m, gage_stats_change_filter$RRMSE, col = "red")

sub = gage_correlation$RRMSE < GRADES_correlation$RRMSE
sum(sub)
##37/81 the RC has lower RRMSE than GRADES. 
subtract = gage_correlation$RRMSE - GRADES_correlation$RRMSE
sub_sites = cbind(gage_stats_change_filter$Site_number, subtract)


station_ind = gageinfo$SITE_NUM %in% gage_stats_output$Site_number
stations = gageinfo[station_ind,]
ryan = cbind(gage_stats_output, stations)

ryan1 = st_as_sf(ryan, coords = c("LONG", "LAT"))


gage_stats$lat = gageinfo$LAT[match(gage_stats$Site_number, gageinfo$SITE_NUM)]
gage_stats$long = gageinfo$LONG[match(gage_stats$Site_number, gageinfo$SITE_NUM)]
ryan_1 = st_as_sf(gage_stats[!is.na(gage_stats$Site_number),],coords = c("long", "lat"))


library(tmap)
data("World")
tmap_mode("view")

tm_shape(World[World$continent == "North America",]) +
  tm_polygons() +
  tm_shape(ryan1)+
  tm_symbols(col = "subtract", scale = .5)

tm_shape(World[World$name == "United States",]) +
  tm_polygons() +
  tm_shape(ryan1)+
  tm_symbols(col = "R", size = "GRWL_width_m", scale = .5, midpoint = NA, breaks = c(-1, -.8, -.5, 0, .25, .5, .8, 1))

tm_shape(World[World$name == "United States",]) +
  tm_polygons() +
  tm_shape(ryan1)+
  tm_symbols(col = "Bias", size = "GRWL_width_m", scale = .5, midpoint = NA,breaks = c(-12000, -2000, -500, -250, 0, 250,500, 2000))


tm_shape(World[World$name == "United States",]) +
  tm_polygons() +
  tm_shape(ryan_1[is.na(ryan_1$dam),])+
  tm_symbols(col = "RRMSE", size = "GRWL_width_m", scale = .5, midpoint = NA,breaks = c(0, 50, 70, 100, 3000)
)






ryan_623 = gage_stats_output
ryan_623_grades = gage_stats_GRADES_output

gage_stats_output = ryan_623
gage_stats_GRADES_output = ryan_623_grades




############################with grades
pdfOut = "E:/research/temp_plots/filtered_eastern_2013_GRADES_20std_relativeRRMSE_1spc_percentbreaks_1.pdf"
pdf(pdfOut)
for (i in 1:nrow(Site_number_xsections)){
  #for (j in 1:(ncol(Site_number_xsections))){
  xSecIDcol = Site_number_xsections[i,]
  ##if you uncomment the next two lines it will only produce the gages with 5 lines. Only 4 plots produced. 
  #if(xSecIDcol[1]==Site_number_xsections[i,1] & xSecIDcol[2]==Site_number_xsections[i,2] & xSecIDcol[3]==Site_number_xsections[i,3] ##extra step to plot only proper curves. 
  #&xSecIDcol[4]==Site_number_xsections[i,4]&xSecIDcol[5]==Site_number_xsections[i,5]&xSecIDcol[6]==Site_number_xsections[i,6]){
  xSecID=xSecIDcol[2:ncol(xSecIDcol)]
  
  
  #print(xSecID)
  #[xSecIDcol==Site_number_xsections[i,2:6]]
  mInd = match(xSecID, tab$ID)
  
  xSecw=wTab[mInd,] ##notNA
  xSecq=qTab[mInd,] ##notNA
  #xSecw = xSecw[,11:91]
  #xSecq = xSecq[,11:91]
  
  
  w_max=max(range(xSecw, na.rm = TRUE))
  w_min=min(range(xSecw, na.rm = TRUE))
  q_max=max(range(xSecq, na.rm = TRUE))
  q_min=min(range(xSecq, na.rm = TRUE))
  is.na(w_max) = sapply(w_max, is.infinite)
  is.na(w_min) = sapply(w_min, is.infinite)
  is.na(q_max) = sapply(q_max, is.infinite)
  is.na(q_min) = sapply(q_min, is.infinite)
  rangedf_1 = cbind(q_max, q_min, w_max, w_min)
  
  data_val_subset = try(subset(data_val, xSecID[,1]==data_val$ID | xSecID[,2]==data_val$ID| 
                                 xSecID[,3]==data_val$ID| xSecID[,4]==data_val$ID| xSecID[,5]==data_val$ID))
  
  # data_val_subset = try(subset(data_val, xSecID[,1]==data_val$ID | xSecID[,2]==data_val$ID| 
  #                                xSecID[,3]==data_val$ID| xSecID[,4]==data_val$ID| xSecID[,5]==data_val$ID
  #                              | xSecID[,6]==data_val$ID| xSecID[,7]==data_val$ID| xSecID[,8]==data_val$ID
  #                              | xSecID[,9]==data_val$ID| xSecID[,10]==data_val$ID))
  # 
  data_val_sub_agg = try(aggregate(data_val_subset$calc_mean, by=list(data_val_subset$Date), FUN=mean))
  if(!is.na(rangedf_1)){
    #par(mfrow = c(1, 2))
    # par(mfrow =c(2, 2), mar = c(4, 4, 5, 4))
    # layout(matrix(c(1,1,1,1,1,0,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3), 2, 11, byrow = TRUE))
    
    # plot(c(rangedf_1[,1], rangedf_1[,2]), c(rangedf_1[,3], rangedf_1[,4]), col = "white", 
    #      main=paste("USGS Gage:", Site_number_xsections$`gageinfo$SITE_NUM`[i]),
    #      log = "", type="p", xlab="Q (cms)",
    #      ylab="W (m)",)
    
    
    #average rating curve
    q_rc = apply(xSecq, 2, FUN = mean)#,na.rm = TRUE)
    w_rc = apply(xSecw, 2, FUN = mean)#,na.rm = TRUE)
    
    q_sd = apply(xSecq, 2, FUN = sd)#, na.rm = TRUE)
    w_sd = apply(xSecw, 2, FUN = sd)#, na.rm = TRUE)
    print(w_sd)
    
    #w_rc = w_rc * 1.33
    if(mean(tab$width_m[mInd], na.rm = TRUE)>600){
       w_rc = w_rc
     } else{w_rc = w_rc * 1.33}
    avg_w = mean(tab$width_m[mInd], na.rm = TRUE)
    # 
    # if(avg_w >99 & avg_w <200){
    #       w_rc = w_rc * corr_1_2
    #       }
    # if(avg_w >200 & avg_w <300){
    #   w_rc = w_rc * corr_2_3
    # }
    # if(avg_w >300 & avg_w <400){
    #   w_rc = w_rc * corr_3_4
    # }
    # if(avg_w >400 & avg_w <500){
    #   w_rc = w_rc * corr_4_5
    # }
    # if(avg_w >500){
    #   w_rc = w_rc * corr_5
    # }
    # 
    
    w_sd_avg = mean(w_sd, na.rm = TRUE)
    print(w_sd_avg)
    
    ##background grid that shows the percentiles across the figure. ###probably not useful. 
    #abline(v = q_rc, col = "lightgray", lty = "dotted", lwd = 0.25)
    #abline(h = w_rc, col = "lightgray", lty = "dotted", lwd = 0.25)
    
    
    #lines(q_rc, w_rc, type = "l", lwd = 3) ##average xsection line before interpolation. 
    ##add in lines of rating curves to each plot. 
    for (k in 1:nrow(xSecq)){     
      #   #print(xSecq)
      #lines(as.numeric(xSecq[k,]), as.numeric(xSecw[k,]), type="l", lwd = 0.5, col = "blue")
      
      
      ##creates continuous dataset for each individual cross section. 
      spl_c = try(approxfun(as.numeric(xSecw[k,]), as.numeric(xSecq[k,]), ties = "ordered"))
      #try(lines(spl_c(as.numeric(xSecw[k,])), as.numeric(xSecw[k,]), col = k))
      
      ####create a new spline function for each rating curve. 
      # spl_1=try(splinefun(as.numeric(xSecw[1,]), as.numeric(xSecq[1,]), method = "hyman", ties = "ordered"))
      # spl_2=try(splinefun(as.numeric(xSecw[2,]), as.numeric(xSecq[2,]), method = "hyman", ties = "ordered"))
      # spl_3=try(splinefun(as.numeric(xSecw[3,]), as.numeric(xSecq[3,]), method = "hyman", ties = "ordered"))
      # spl_4=try(splinefun(as.numeric(xSecw[4,]), as.numeric(xSecq[4,]), method = "hyman", ties = "ordered"))
      # spl_5=try(splinefun(as.numeric(xSecw[5,]), as.numeric(xSecq[5,]), method = "hyman", ties = "ordered"))
      
      ## Set up values for approx/splinefun. 
      y = q_rc
      x = w_rc
      spl = try(approxfun(x, y)) #, method = "hyman")) #####either usse 'approxfun' or use splinefun with method = "hyman"
      # a=try(lines(spl(x), x , col = "black", lwd = 2)) ##average xsection line after interpolation. 
      #a = splinefun(x, y)
      #print(a)
      
      ####standard deviation boundaries around average rating curve. 
      # try(lines(spl(x), x+w_sd, col = "red"))
      # try(lines(spl(x), x - w_sd, col = "red"))
      
      ## create spline for estimating error bars below. 
      spl_sd = try(splinefun(x, w_sd))
    }
    #seqx = seq(1, 10, 0.1)
    #lines(seqx, spl(seqx))
    
    #logestimate = try(lm(lines_maybe[,1] ~ log(lines_maybe[,2])))
    #try(curve(logestimate))
    
    #logestimate = lm(xSecw[k] ~ log(xSecq[k]), na.exclude)
    #print(logestimate)
    #text(1, 2, xSecID[1:5])
    #text(as.numeric(xSecq[k,10]), as.numeric(xSecw[k,10]), xSecID[k])
    # legend("bottomright", 
    #        legend = c("Rating curve","Landsat widths", "USGS widths"), 
    #        col = c("black", "blue", 
    #                "lightgray"),
    #        lty=c(1, NA, NA),
    #        lwd = c(2, NA, NA),
    #        pch = c(NA, 17,01), 
    #        bty = "n", 
    #        text.col = "black", 
    #        horiz = F)
    #inset = c(0.9, 0.25))
    
    usgs = try(usgs_processing(read.csv(trial[i])))
    
    ##if above line works then add in the usgs width/discharge information. if the usgs data is missing then skip this step. 
    if(!is.error(usgs)){
      
      
      ###get percentile USGS discharge and width values and plot them. 
      u=as.data.frame(usgs)
      u_w_q = try(quantile(u$w_m, probs = seq(.2, 1, .1), na.rm = TRUE))
      u_q_q = try(quantile(u$q_cms, probs = seq(.2, 1, .1), na.rm = TRUE))
      #try(lines(u_q_q,u_w_q,type = "l", lty = 2, col = "red"))
      usgs_q = try(usgs_q_processing(read.csv(usgs_q_list[i], stringsAsFactors = FALSE)))
      
      if(!is.error(data_val_sub_agg) & !is.error(usgs_q)){
        usgs_q_ind=which(usgs_q$datetime %in% data_val_sub_agg$Group.1)
        usgs_q_subset= usgs_q[usgs_q_ind,]
        usgs_q_subset[order(usgs_q_subset$datetime),]
        data_val_sub_agg_ind=which(data_val_sub_agg$Group.1 %in% usgs_q_subset$datetime)
        data_val_sub_agg_1 = data_val_sub_agg[data_val_sub_agg_ind,]
        data_val_sub_agg_1[order(data_val_sub_agg_1$Group.1),]
        

        ## usgs width vs usgs discharge points. 
        # points(usgs_q_subset$q,data_val_sub_agg_1$x, pch=17, col="green")
        u_1=cbind(usgs_q_subset$q, data_val_sub_agg_1$x)
        u = as.data.frame(u_1)
        
        
        #data_Val_spl = spl(data_val_sub_agg_1$x)
        #try(print(spl(data_val_sub_agg_1$x)))
        dv = as.vector(nrow(data_val_sub_agg_1))
        sd_dv = try(spl_sd(data_val_sub_agg_1$x)) ##added
        dv = try(spl(data_val_sub_agg_1$x))
        
        if(!is.error(dv)){
        sd_dv_df = cbind(dv, sd_dv) ##added
        sd_dv_out = sd_dv_df ##added
        sd_dv_out[,1][sd_dv_df[,2]> (avg_w * .2)] = NA ##added
        dv = try(sd_dv_out[,1]) ##added
        }

        l = try(as.data.frame(cbind(dv, data_val_sub_agg_1$x)))
        
         
        
        #total = try(cbind(u$V1, l$dv, Site_number_xsections$`gageinfo$SITE_NUM`[i]))
        
        
        
        ## landsat estimated widths vs discharge from rating curve. 
        # try(points(dv, data_val_sub_agg_1$x, pch = 17, col = "blue"))
        
        ##add in all 2015-present landsat widths onto curve. Pretty much the same as above. 
        # dv_1 = as.vector(nrow(data_val_sub_agg))
        # dv_1 =try(spl(data_val_sub_agg$x))
        #try(points(dv_1, data_val_sub_agg$x, pch = 19, col = "green"))
        u_1 = try(which(u$w_m %in% l$V2))
        u_2 = u[u_1, ]
        
        
        ###add in error bars for each landsat estimated plot based on variation in nearest cross sections at that point. 
        # try(arrows(x0 = dv, y0= data_val_sub_agg_1$x - spl_sd(data_val_sub_agg_1$x), 
        #            x1 = dv, y1 = data_val_sub_agg_1$x + spl_sd(data_val_sub_agg_1$x), col = "black" , 
        #            code=3, angle = 90, length = 0.1))
        
        ###try to create a new plot for Qinsitu vs Qlandsat
        #if(!is.error(l)){
        # par(new = TRUE)
        #try(plot(u$V1, l$dv))} else{plot(1:10)}
        ###calculate and add R2 value to plot. 
        linearmodel = try(lm(u$V1 ~ l$dv))
        r_2 = try(summary(linearmodel)$r.squared)
        bb = try(bquote(italic(R)^2 ==.(format(r_2, digits = 3))))
        if(!is.error(r_2)){
          
        } else{next} 
        
        
        ###calculate and add pearsons correlation (R) to plot. 
        pearson = try(cor.test(u$V1, l$dv, method = "pearson"))
        r = try(pearson$estimate)
        rlabel = try(bquote(italic(R) == .(format(r, digits = 3))))
        if(!is.error(r)){
          par(mfrow =c(2, 2), mar = c(4, 4, 5, 4))
          layout(matrix(c(1,1,1,1,1,0,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3), 2, 11, byrow = TRUE))
          try(plot(c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)), c(min(x-w_sd, na.rm = TRUE), max(x+w_sd, na.rm = TRUE)), col = "white", 
                   main=paste("USGS Gage:", Site_number_xsections$`gageinfo$SITE_NUM`[i]),
                   log = "", type="p", xlab="Q (cms)",
                   ylab="W (m)",))
          # 
          # plot(spl(x), x, col = "black", 
          #      main=paste("USGS Gage:", Site_number_xsections$`gageinfo$SITE_NUM`[i]),
          #      log = "", type="l", lwd = 2,xlab="Q (cms)",
          #      ylab="W (m)",)
          
          # try(lines(as.numeric(xSecq[1,]), as.numeric(xSecw[1,]), type="l", lwd = 0.5, col = "blue"))
          #  try(lines(as.numeric(xSecq[2,]), as.numeric(xSecw[2,]), type="l", lwd = 0.5, col = "blue"))
          #  try(lines(as.numeric(xSecq[3,]), as.numeric(xSecw[3,]), type="l", lwd = 0.5, col = "blue"))
          #  try(lines(as.numeric(xSecq[4,]), as.numeric(xSecw[4,]), type="l", lwd = 0.5, col = "blue"))
          #  try(lines(as.numeric(xSecq[5,]), as.numeric(xSecw[5,]), type="l", lwd = 0.5, col = "blue"))
          #  
          # try(text(as.numeric(xSecq[1,100]), as.numeric(xSecw[1,100]), xSecID[1]))
          # try(text(as.numeric(xSecq[2,100]), as.numeric(xSecw[2,100]), xSecID[2]))
          # try(text(as.numeric(xSecq[3,100]), as.numeric(xSecw[3,100]), xSecID[3]))
          # try(text(as.numeric(xSecq[4,100]), as.numeric(xSecw[4,100]), xSecID[4]))
          # try(text(as.numeric(xSecq[5,100]), as.numeric(xSecw[5,100]), xSecID[5]))
          #  
          
          
          
          a=try(lines(spl(x), x , col = "black", lwd = 2))
          try(lines(spl(x), x+w_sd, col = "red"))
          try(lines(spl(x), x - w_sd, col = "red"))
          points(usgs_q_subset$q,data_val_sub_agg_1$x, pch=17, col="green")
          try(points(dv, data_val_sub_agg_1$x, pch = 17, col = "blue"))
          try(arrows(x0 = dv, y0= data_val_sub_agg_1$x - spl_sd(data_val_sub_agg_1$x), 
                     x1 = dv, y1 = data_val_sub_agg_1$x + spl_sd(data_val_sub_agg_1$x), col = "black" , 
                     code=3, angle = 90, length = 0.1))
          
          
          
          legend("bottomright", 
                 legend = c("Rating curve","Landsat widths", "USGS widths"), 
                 col = c("black", "blue", 
                         "lightgray"),
                 lty=c(1, NA, NA),
                 lwd = c(2, NA, NA),
                 pch = c(NA, 17,01), 
                 bty = "n", 
                 text.col = "black", 
                 horiz = FALSE)
          
          points(usgs[,1], usgs[,2], col = "lightgray", lwd = 0.5)
          
          
          
          
          
          try(plot(u$V1, l$dv, xlab = "In situ Q (cms)", ylab = "Landsat Q (cms)", main = "In situ vs Landsat", asp = 1))
          try(mtext(side = 1, line = 4.5, adj = 1, text = bb))
          gage_stats_col2 = r_2
          gage_stats$R_2[i] = gage_stats_col2
          gage_stats_col1=Site_number_xsections[i,1]
          gage_stats$Site_number[i] = gage_stats_col1
          gage_stats_col7=length(na.omit(dv))
          gage_stats$n_Landsat_obs[i] = gage_stats_col7
          try(mtext(side = 1, line = 6, adj = 1, text = rlabel))
          gage_stats_col3 = r
          gage_stats$R[i] = gage_stats_col3} else{next}
        
        ###calculate and add spearmans coefficient (p) to plot. 
        spearman = try(cor.test(u$V1, l$dv, method = "spearman"))
        p_val = try(spearman$p.value)
        plabel = try(bquote(italic(p) == .(format(p_val, digits = 3))))
        if(!is.error(p_val)){
          #try(mtext(side = 1, line = 6, adj = 0, text = plabel))
          gage_stats_col4 = p_val
          gage_stats$p_val[i] = gage_stats_col4} else{next}
        
        ###calculate and add rmse to plot. 
        error = try(u[,1] - l[,1])
        rmse = sqrt(mean(error^2, na.rm = TRUE))
        rmse_label = try(bquote(italic(RMSE) == .(format(rmse, digits = 5))))
        rrmse = mean(error/u$V1, na.rm = TRUE) * 100
        #rrmse = rmse/(mean(u[,1], na.rm = TRUE)) *100
        rrmse = sqrt((rrmse^2))
        rrmse_median = median(error/u$V1, na.rm = TRUE) * 100
        rrmse_median = sqrt((rrmse_median^2))
        rrmse_label = try(bquote(italic(RRMSE) == .(format(rrmse, digits = 5))))
        if(!is.error(rmse)){
          try(mtext(side = 1, line = 7.5, adj = 1, text = rmse_label))
          #original####try(mtext(side = 2, line = 7.5, adj = 1, text = rrmse_label))
          try(mtext(side = 1, line = 6, adj = 0, text = rrmse_label))
          gage_stats_col5 = rmse
          gage_stats$RMSE[i] = gage_stats_col5
          gage_stats_col9 = rrmse
          gage_stats$RRMSE[i] = rrmse
          gage_stats_col9 = w_sd_avg
          gage_stats$avg_std[i] = gage_stats_col9
          gage_stats_col10 = mean(abs(tab$change[mInd]))
          gage_stats$change[i] = gage_stats_col10
          gage_stats_col12 = rrmse_median
          gage_stats$RRMSE_median[i] = rrmse_median
          } else{next}
        
        ###calculate and add bias to plot
        error = try(l[,1] - u[,1])
        bias = mean(error, na.rm = TRUE)
        bias_label = try(bquote(italic(Bias) == .(format(bias, digits = 5))))
        if(!is.error(bias)){
          try(mtext(side = 1, line = 4.5, adj = 0, text = bias_label))
          gage_stats_col6 = bias
          gage_stats$Bias[i] = gage_stats_col6
          gage_stats_col8 = tab$width_m[mInd[1]]
          gage_stats$GRWL_width_m[i] = gage_stats_col8} else{next}
        #l_df = data.frame(l$dv)
        #comb = cbind(l$dv, u$V1)
        #cumulative_df_l = append(cumulative_df_l, comb)
        
        
        #u_df = data.frame(u$V1)
        #cumulative_df_u = append(cumulative_df_u, comb[,2])
        #if(!is.error(r_2)){
        #try(plot(u$V1, l$dv, xlab = "In situ Q (cms)", ylab = "Landsat Q (cms)", main = "In situ vs Landsat"))} else{next}
        
        ##add in hydrographs: first is 2015-present. 
        if(nrow(usgs_q_subset) >0){
          #par(new = TRUE)
          
          
          ##add in date field to usgs data and filter to 2015- present.  
          usgs_q$date = as.Date(usgs_q$datetime, "%Y-%m-%d")
          usgs_q_2015 = usgs_q %>%
            filter(usgs_q$date > as.Date('2013-01-01') & usgs_q$date < as.Date('2013-12-31'))
          
          
          ##add in date field to validation data and assign to corresponding usgs days.
          data_val_sub_agg_1$date = as.Date(data_val_sub_agg_1$Group.1, "%Y-%m-%d")
          usgs_q_2015_wval = left_join(usgs_q_2015, data_val_sub_agg_1, copy = FALSE)
          usgs_q_2015_wval$landat = spl(usgs_q_2015_wval$x)
          
          
          ry = try(cbind(spl_1(data_val_sub_agg_1$x), spl_2(data_val_sub_agg_1$x), spl_3(data_val_sub_agg_1$x), spl_4(data_val_sub_agg_1$x), spl_5(data_val_sub_agg_1$x)))
          
          ry1 = data_val_subset %>% group_by(Date) %>%
            mutate(grp = 1:n())%>%
            gather(var, val, -Date, -grp) %>%
            unite("var_grp", var, grp, sep ='') %>%
            spread(var_grp, val, fill = '')
          
          ry1_cols = colnames(ry1) 
          width_cols_landsat = grep("calc" ,colnames(ry1))
          
          ry1_widths = ry1[,width_cols_landsat]
          
          ry2 = cbind(ry1$Date, ry1[,width_cols_landsat])
          ry2_ind = ry2$`ry1$Date` %in% usgs_q_subset$datetime
          ry2 = ry2[ry2_ind,]
          ry2_ncols = ncol(ry2)
          #ry3 = cbind(ry2, data_val_sub_agg_1)
          est = as.data.frame(ry2)
          
          for (r in 2:ncol(ry2)){
            est[r] = spl(ry2[,r])
          }
          
          mx = apply(est[2:ry2_ncols], 1, max, na.rm = TRUE)
          mn = apply(est[2:ry2_ncols], 1, min, na.rm = TRUE)
          sd = apply(est[2:ry2_ncols], 1, sd, na.rm = TRUE)
          mx_mn = cbind(mx, mn, sd)
          mx_mn[mapply(is.infinite, mx_mn)] <- NA
          
          data_val_sub_agg_1 = cbind(data_val_sub_agg_1, mx_mn)
          
          # ry_c = cbind(dv, ry)
          # 
          # 
          # ry_min = apply(ry, 1, FUN = min, na.rm = TRUE)
          # ry_max = apply(ry, 1, FUN = max, na.rm = TRUE)
          # ry_c1 = cbind(ry_c, ry_min, ry_max)
          # 
          # #ry_max = apply(ry, 1, function(x) which (x==max(x)))
          # #ry_min = apply(ry, 1, function(x) which (x==min(x)))
          # 
          # data_val_sub_agg_1$max = ry_max
          # data_val_sub_agg_1$min = ry_min
          usgs_q_2015_wval = left_join(usgs_q_2015, data_val_sub_agg_1, copy = FALSE)
          usgs_q_2015_wval$landat = spl(usgs_q_2015_wval$x)
          
          usgs_q_2015_wval$mx[mapply(is.na, usgs_q_2015_wval$landat)] = NA
          usgs_q_2015_wval$mn[mapply(is.na, usgs_q_2015_wval$landat)] = NA
          usgs_q_2015_wval$sd[mapply(is.na, usgs_q_2015_wval$landat)] = NA
          usgs_q_2015_wval$sd_mx = usgs_q_2015_wval$landat + usgs_q_2015_wval$sd
          usgs_q_2015_wval$sd_mn = usgs_q_2015_wval$landat - usgs_q_2015_wval$sd
          
          
          try(plot(usgs_q_2015_wval$date, usgs_q_2015_wval$q, xlab = "", ylab = "Q (cms)", type = "l", col = "lightgray"))
          
          try(points(usgs_q_2015_wval$date,usgs_q_2015_wval$landat, col = "blue"))
          
          try(arrows(x0 = usgs_q_2015_wval$date, y0= usgs_q_2015_wval$sd_mn, 
                     x1 = usgs_q_2015_wval$date, y1 = usgs_q_2015_wval$sd_mx, col = "black" , 
                     code=3, angle = 90, length = 0.1))
          
          #try(plot(smooth.spline(na.omit(dv), spar = 0.3), ylim = c(q_min_plot, q_max_plot), type = "l", xlab = "2013", ylab = "Q (cms)", xaxt = 'n', main = "Hydrograph 2015-Present",col= "red"))
          title("2013 Hydrograph", line = 0.5)
          
          
          #par(new = TRUE)
          #plot(smooth.spline(usgs_q_subset$q, spar = 0.25), type = "l", xlab = "", ylab = "", axes = F)
          #axis(4)
          #points(dv, col = "blue", pch = 16)
          legend("bottom", inset = c(0, -.2), legend = c("Rating curve", "In situ"),
                 col = c("blue", "lightgray"),lty = c(NA, 1), pch = c(1, NA), bty = "n", xpd = TRUE, horiz = TRUE)
          
          
          #try(plot(smooth.spline(na.omit(dv), spar = 0.3), ylim = c(q_min_plot, q_max_plot), type = "l", xlab = "2015-Present", ylab = "Q (cms)", xaxt = 'n', main = "Hydrograph 2015-Present",col= "red"))
          ##title("Hydrograph 2015-2020", line = 0.5)
          
          #try(lines(1:length(dv), dv_spl(1:length(dv)), col = "blue"))
          ##add in GRADES data
          data_ind = tab[mInd,]
          data_comid = data_ind$COMID[1]
          GRADES_ind = which(filt_df$COMID %in% data_comid)
          GRADES_df = filt_df[GRADES_ind,]
          usgs_q_2015_GRADES = left_join(usgs_q_2015, GRADES_df, copy = FALSE)
          try(lines(usgs_q_2015_GRADES$date, usgs_q_2015_GRADES$Q, col = "red"))
          print(try(quantile(usgs_q_2015_GRADES$Q, probs = seq(.2, 1, .05))))
          
          
          usgs_q_2015_wval_filt = usgs_q_2015_wval[!is.na(usgs_q_2015_wval$landat),]
          
          GRADES_stats_ind = GRADES_df$date%in%usgs_q_2015_wval_filt$date
          GRADES_stats_df = GRADES_df[GRADES_stats_ind,]
          usgs_q_2015_wval_GRADES = left_join(GRADES_stats_df, usgs_q_2015_wval, copy = FALSE)
          
          ##calculate GRADES_stats
          linearmodel_G = try(lm(usgs_q_2015_wval_GRADES$q ~ usgs_q_2015_wval_GRADES$Q))
          r_2_G = try(summary(linearmodel_G)$r.squared)
          gage_stats_GRADES_col2=r_2_G
          gage_stats_GRADES$R_2[i] = gage_stats_GRADES_col2
          gage_stats_GRADES_col1=Site_number_xsections[i,1]
          gage_stats_GRADES$Site_number[i] = gage_stats_GRADES_col1
          gage_stats_GRADES_col7=nrow(na.omit(usgs_q_2015_wval_GRADES))
          gage_stats_GRADES$n_Landsat_obs[i] = gage_stats_GRADES_col7
          
          pearson_G = try(cor.test(usgs_q_2015_wval_GRADES$q, usgs_q_2015_wval_GRADES$Q, method = "pearson"))
          r_G = try(pearson_G$estimate)
          gage_stats_GRADES_col3 = r_G
          gage_stats_GRADES$R[i] = gage_stats_GRADES_col3
          
          
          spearman_G = try(cor.test(usgs_q_2015_wval_GRADES$q, usgs_q_2015_wval_GRADES$Q, method = "spearman"))
          p_val_G = try(spearman_G$p.value)
          gage_stats_GRADES_col4 = p_val_G
          gage_stats_GRADES$p_val[i] = gage_stats_GRADES_col4
          
          error_G = try(usgs_q_2015_wval_GRADES$q - usgs_q_2015_wval_GRADES$Q)
          rmse_G = sqrt(mean(error_G^2, na.rm = TRUE))
          gage_stats_GRADES_col5 = rmse_G
          gage_stats_GRADES$RMSE[i] = gage_stats_GRADES_col5
          rrmse_G = mean(error_G/usgs_q_2015_wval_GRADES$q, na.rm = TRUE) * 100
          #rrmse_G = sqrt(rrmse_G^2)
          #rrmse_G = rmse_G/(mean(usgs_q_2015_wval_GRADES$q)) *100
          rrmse_G = sqrt((rrmse_G^2))
          rrmse_G_median = median(error_G/usgs_q_2015_wval_GRADES$q, na.rm = TRUE) * 100
          rrmse_G_median = sqrt((rrmse_G_median^2))
          
          gage_stats_GRADES_col9 = rrmse_G
          gage_stats_GRADES$RRMSE[i] = gage_stats_GRADES_col9
          gage_stats_GRADES_col12 = rrmse_G_median
          gage_stats_GRADES$RRMSE_median[i] = gage_stats_GRADES_col12
          
          bias_G = mean(error_G, na.rm = TRUE)
          gage_stats_GRADES_col6 = bias_G
          gage_stats_GRADES$Bias[i] = gage_stats_GRADES_col6
          gage_stats_GRADES_col8 = tab$width_m[mInd[1]]
          gage_stats_GRADES$GRWL_width_m[i] = gage_stats_GRADES_col8
          #par(new = TRUE)
          #plot(smooth.spline(usgs_q_subset$q, spar = 0.25), type = "l", xlab = "", ylab = "", axes = F)
          #axis(4)
          #points(dv, col = "blue", pch = 16)
          legend("bottom", inset = c(0, -.2), legend = c("Rating curve", "In situ"),
                 col = c("blue", "lightgray"),lty = c(NA, 1), pch = c(1, NA), bty = "n", xpd = TRUE, horiz = TRUE)
          
          
          
          #try(plot(usgs[,1], type = "l", xlab = "", ylab = ""))
          
          ###add in hydrographs for entire time period of gage. 
          ##smooth discharge measurements and plot them. 
          
          
          #try(points(spl(usgs[,2]), col = "blue"))
          #try(lines(spl(usgs[,2]), col = "green"))
          
          
        }
        else{
          next}
        
      } else{
        next}
      
      
    }else{
      #title(main = NULL, sub = "No USGS Data")
      #mtext(side =3, line = 0.5, "No USGS Data")
      #text(y[5],x[5], pos = 4, offset = 9, "No USGS Data") 
      next}
    ##### for individual rating curves. 
  }else{next}
}
dev.off()
cmd = paste('open', pdfOut)
system(cmd)





filt_100_200 = gage_stats_change_filter %>%
                  filter(gage_stats_change_filter$GRWL_width_m>100 & gage_stats_change_filter$GRWL_width_m<200)
apply(filt_100_200, 2, FUN=mean)

filt_200_300 = gage_stats_change_filter %>%
  filter(gage_stats_change_filter$GRWL_width_m>200 & gage_stats_change_filter$GRWL_width_m<300)
apply(filt_200_300, 2, FUN=mean)

filt_300_400 = gage_stats_change_filter %>%
  filter(gage_stats_change_filter$GRWL_width_m>300 & gage_stats_change_filter$GRWL_width_m<400)
apply(filt_300_400, 2, FUN=mean)

filt_400_500 = gage_stats_change_filter %>%
  filter(gage_stats_change_filter$GRWL_width_m>400 & gage_stats_change_filter$GRWL_width_m<500)
apply(filt_400_500, 2, FUN=mean)

filt_500_600 = gage_stats_change_filter %>%
  filter(gage_stats_change_filter$GRWL_width_m>500 & gage_stats_change_filter$GRWL_width_m<600)
apply(filt_500_600, 2, FUN=mean)

filt_600 = gage_stats_change_filter %>%
  filter(gage_stats_change_filter$GRWL_width_m>600)
apply(filt_600, 2, FUN=mean)


###GRADES
GRADES_100_200 = GRADES_correlation %>%
  filter(GRADES_correlation$GRWL_width_m>100 & GRADES_correlation$GRWL_width_m<200)
apply(GRADES_100_200, 2, FUN=mean)

GRADES_200_300 = GRADES_correlation %>%
  filter(GRADES_correlation$GRWL_width_m>200 & GRADES_correlation$GRWL_width_m<300)
apply(GRADES_200_300, 2, FUN=mean)

GRADES_300_400 = GRADES_correlation %>%
  filter(GRADES_correlation$GRWL_width_m>300 & GRADES_correlation$GRWL_width_m<400)
apply(GRADES_300_400, 2, FUN=mean)

GRADES_400_500 = GRADES_correlation %>%
  filter(GRADES_correlation$GRWL_width_m>400 & GRADES_correlation$GRWL_width_m<500)
apply(GRADES_400_500, 2, FUN=mean)

GRADES_500_600 = GRADES_correlation %>%
  filter(GRADES_correlation$GRWL_width_m>500 & GRADES_correlation$GRWL_width_m<600)
apply(GRADES_500_600, 2, FUN=mean)

GRADES_600 = GRADES_correlation %>%
  filter(GRADES_correlation$GRWL_width_m>600)
apply(GRADES_600, 2, FUN=mean)




###############Determine correction factors. 

gage_quants = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 102))
rc_quants = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 103))
for (i in 1:nrow(Site_number_xsections)){
  #for (j in 1:(ncol(Site_number_xsections))){
  xSecIDcol = Site_number_xsections[i,]
  ##if you uncomment the next two lines it will only produce the gages with 5 lines. Only 4 plots produced. 
  #if(xSecIDcol[1]==Site_number_xsections[i,1] & xSecIDcol[2]==Site_number_xsections[i,2] & xSecIDcol[3]==Site_number_xsections[i,3] ##extra step to plot only proper curves. 
  #&xSecIDcol[4]==Site_number_xsections[i,4]&xSecIDcol[5]==Site_number_xsections[i,5]&xSecIDcol[6]==Site_number_xsections[i,6]){
  xSecID=xSecIDcol[2:ncol(xSecIDcol)]
  
  
  #print(xSecID)
  #[xSecIDcol==Site_number_xsections[i,2:6]]
  mInd = match(xSecID, tab$ID)
  
  xSecw=wTab[mInd,] ##notNA
  xSecq=qTab[mInd,] ##notNA
  #xSecw = xSecw[,11:91]
  #xSecq = xSecq[,11:91]
  
  
  w_max=max(range(xSecw, na.rm = TRUE))
  w_min=min(range(xSecw, na.rm = TRUE))
  q_max=max(range(xSecq, na.rm = TRUE))
  q_min=min(range(xSecq, na.rm = TRUE))
  is.na(w_max) = sapply(w_max, is.infinite)
  is.na(w_min) = sapply(w_min, is.infinite)
  is.na(q_max) = sapply(q_max, is.infinite)
  is.na(q_min) = sapply(q_min, is.infinite)
  rangedf_1 = cbind(q_max, q_min, w_max, w_min)
  
  data_val_subset = try(subset(data_val, xSecID[,1]==data_val$ID | xSecID[,2]==data_val$ID| 
                                 xSecID[,3]==data_val$ID| xSecID[,4]==data_val$ID| xSecID[,5]==data_val$ID))
  
  # data_val_subset = try(subset(data_val, xSecID[,1]==data_val$ID | xSecID[,2]==data_val$ID| 
  #                                xSecID[,3]==data_val$ID| xSecID[,4]==data_val$ID| xSecID[,5]==data_val$ID
  #                              | xSecID[,6]==data_val$ID| xSecID[,7]==data_val$ID| xSecID[,8]==data_val$ID
  #                              | xSecID[,9]==data_val$ID| xSecID[,10]==data_val$ID))
  # 
  data_val_sub_agg = try(aggregate(data_val_subset$calc_mean, by=list(data_val_subset$Date), FUN=mean))
  if(!is.na(rangedf_1)){
    #par(mfrow = c(1, 2))
    # par(mfrow =c(2, 2), mar = c(4, 4, 5, 4))
    # layout(matrix(c(1,1,1,1,1,0,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3), 2, 11, byrow = TRUE))
    
    # plot(c(rangedf_1[,1], rangedf_1[,2]), c(rangedf_1[,3], rangedf_1[,4]), col = "white", 
    #      main=paste("USGS Gage:", Site_number_xsections$`gageinfo$SITE_NUM`[i]),
    #      log = "", type="p", xlab="Q (cms)",
    #      ylab="W (m)",)
    
    
    #average rating curve
    q_rc = apply(xSecq, 2, FUN = mean)#,na.rm = TRUE)
    w_rc = apply(xSecw, 2, FUN = mean)#,na.rm = TRUE)
    
    q_sd = apply(xSecq, 2, FUN = sd)#, na.rm = TRUE)
    w_sd = apply(xSecw, 2, FUN = sd)#, na.rm = TRUE)
    print(w_sd)
    
    #w_rc = w_rc * 1.33
    
    
    
    w_sd_avg = mean(w_sd, na.rm = TRUE)
    print(w_sd_avg)
    
    ##background grid that shows the percentiles across the figure. ###probably not useful. 
    #abline(v = q_rc, col = "lightgray", lty = "dotted", lwd = 0.25)
    #abline(h = w_rc, col = "lightgray", lty = "dotted", lwd = 0.25)
    
    
    #lines(q_rc, w_rc, type = "l", lwd = 3) ##average xsection line before interpolation. 
    ##add in lines of rating curves to each plot. 
    for (k in 1:nrow(xSecq)){     
      #   #print(xSecq)
      #lines(as.numeric(xSecq[k,]), as.numeric(xSecw[k,]), type="l", lwd = 0.5, col = "blue")
      
      
      ##creates continuous dataset for each individual cross section. 
      spl_c = try(approxfun(as.numeric(xSecw[k,]), as.numeric(xSecq[k,]), ties = "ordered"))
      #try(lines(spl_c(as.numeric(xSecw[k,])), as.numeric(xSecw[k,]), col = k))
      
      ####create a new spline function for each rating curve. 
      # spl_1=try(splinefun(as.numeric(xSecw[1,]), as.numeric(xSecq[1,]), method = "hyman", ties = "ordered"))
      # spl_2=try(splinefun(as.numeric(xSecw[2,]), as.numeric(xSecq[2,]), method = "hyman", ties = "ordered"))
      # spl_3=try(splinefun(as.numeric(xSecw[3,]), as.numeric(xSecq[3,]), method = "hyman", ties = "ordered"))
      # spl_4=try(splinefun(as.numeric(xSecw[4,]), as.numeric(xSecq[4,]), method = "hyman", ties = "ordered"))
      # spl_5=try(splinefun(as.numeric(xSecw[5,]), as.numeric(xSecq[5,]), method = "hyman", ties = "ordered"))
      
      ## Set up values for approx/splinefun. 
      y = q_rc
      x = w_rc
      spl = try(approxfun(x, y)) #, method = "hyman")) #####either usse 'approxfun' or use splinefun with method = "hyman"
      # a=try(lines(spl(x), x , col = "black", lwd = 2)) ##average xsection line after interpolation. 
      #a = splinefun(x, y)
      #print(a)
      
      ####standard deviation boundaries around average rating curve. 
      # try(lines(spl(x), x+w_sd, col = "red"))
      # try(lines(spl(x), x - w_sd, col = "red"))
      
      ## create spline for estimating error bars below. 
      spl_sd = try(splinefun(x, w_sd))
    }
    #seqx = seq(1, 10, 0.1)
    #lines(seqx, spl(seqx))
    
    #logestimate = try(lm(lines_maybe[,1] ~ log(lines_maybe[,2])))
    #try(curve(logestimate))
    
    #logestimate = lm(xSecw[k] ~ log(xSecq[k]), na.exclude)
    #print(logestimate)
    #text(1, 2, xSecID[1:5])
    #text(as.numeric(xSecq[k,10]), as.numeric(xSecw[k,10]), xSecID[k])
    # legend("bottomright", 
    #        legend = c("Rating curve","Landsat widths", "USGS widths"), 
    #        col = c("black", "blue", 
    #                "lightgray"),
    #        lty=c(1, NA, NA),
    #        lwd = c(2, NA, NA),
    #        pch = c(NA, 17,01), 
    #        bty = "n", 
    #        text.col = "black", 
    #        horiz = F)
    #inset = c(0.9, 0.25))
    
    usgs = try(usgs_processing(read.csv(trial[i])))
    
    ##if above line works then add in the usgs width/discharge information. if the usgs data is missing then skip this step. 
    if(!is.error(usgs)){
      
      
      ###get percentile USGS discharge and width values and plot them. 
      u=as.data.frame(usgs)
      u_w_q = try(quantile(u$w_m, probs = seq(0, 1, .01), na.rm = TRUE))
      u_q_q = try(quantile(u$q_cms, probs = seq(0, 1, .01), na.rm = TRUE))
      rc_quants[i,1] = Site_number_xsections$`gageinfo$SITE_NUM`[i]
      gage_quants[i,1] = Site_number_xsections$`gageinfo$SITE_NUM`[i]
      
      
      rc_quants[i,2:102] = w_rc
      gage_quants[i,2:102] = u_w_q
      rc_quants[i,103] = tab$width_m[mInd[1]]
      
    }else{next}
  }else{next}
}


rc_quants_output = rc_quants[!is.na(rc_quants$V1),]


rc_quants_vals = rc_quants_output[,2:102]
colnames(rc_quants_vals)= width_cols_new
gage_quants_output = gage_quants[!is.na(gage_quants$V1),]

rc_gage_comb = cbind(rc_quants_vals, gage_quants_output[,2:102])
rc_gage_comb = cbind(rc_quants_output[,103],rc_gage_comb)
rc_gage_comb = cbind(rc_quants_output[,1],rc_gage_comb)

##create random 20% sets of various river width groupings. 

q_random_rows_100_200 = rc_gage_comb %>%
                  filter(rc_gage_comb[,2]>99 & rc_gage_comb[,2] <200) %>%
                  .[sample(nrow(.), (nrow(.) * .2)),]

q_random_rows_200_300 = rc_gage_comb %>%
  filter(rc_gage_comb[,2]>200 & rc_gage_comb[,2] <300) %>%
  .[sample(nrow(.), (nrow(.) * .2)),]

q_random_rows_300_400 = rc_gage_comb %>%
  filter(rc_gage_comb[,2]>300 & rc_gage_comb[,2] <400) %>%
  .[sample(nrow(.), (nrow(.) * .2)),]

q_random_rows_400_500 = rc_gage_comb %>%
  filter(rc_gage_comb[,2]>400 & rc_gage_comb[,2] <500) %>%
  .[sample(nrow(.), (nrow(.) * .2)),]

q_random_rows_500 = rc_gage_comb %>%
  filter(rc_gage_comb[,2]>500) %>%
  .[sample(nrow(.), (nrow(.) * .2)),]

### determine difference for each group
q_fact_1_2 = (q_random_rows_100_200[,104:204] / q_random_rows_100_200[,3:103])
q_fact_1_2[mapply(is.na, q_fact_1_2)] <- 1
corr_1_2 = apply(q_fact_1_2, 2, FUN = mean, na.rm = TRUE)
corr_1_2 = mean(corr_1_2, na.rm = TRUE)

q_fact_2_3 = (q_random_rows_200_300[,104:204] / q_random_rows_200_300[,3:103])
q_fact_2_3[mapply(is.na, q_fact_2_3)] <- 1
corr_2_3 = apply(q_fact_2_3, 2, FUN = mean, na.rm = TRUE)
corr_2_3 = mean(corr_2_3, na.rm = TRUE)

q_fact_3_4 = (q_random_rows_300_400[,104:204] / q_random_rows_300_400[,3:103])
q_fact_3_4[mapply(is.na, q_fact_3_4)] <- 1
corr_3_4 = apply(q_fact_3_4, 2, FUN = mean, na.rm = TRUE)
corr_3_4 = mean(corr_3_4, na.rm = TRUE)

q_fact_4_5 = (q_random_rows_400_500[,104:204] / q_random_rows_400_500[,3:103])
q_fact_4_5[mapply(is.na, q_fact_4_5)] <- 1
corr_4_5 = apply(q_fact_4_5, 2, FUN = mean, na.rm = TRUE)
corr_4_5 = mean(corr_4_5, na.rm = TRUE)

q_fact_5 = (q_random_rows_500[,104:204] / q_random_rows_500[,3:103])
q_fact_5[mapply(is.na, q_fact_5)] <- 1
corr_5 = apply(q_fact_5, 2, FUN = mean, na.rm = TRUE)
corr_5 = mean(corr_5, na.rm = TRUE)
