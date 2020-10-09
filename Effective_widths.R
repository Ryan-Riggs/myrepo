library(sf)
library(geosphere)
library(tidyverse)
library(sf)
library(sp)
library(ggplot2)
library(rgeos)
library(dplyr)
library(gmt)
library(vroom)
library(BAMMtools)
library(hydroGOF)

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

###########################################################################################################
##Trials of different versions. 

##Unchanged code version. Random 30 gauges. 
Eff_widths = vroom(list.files("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\Subset\\Trials\\Unchanged_code\\Effective_widths_unchanged", full.names = TRUE))
##Updated with new polygon/flags. 
Eff_widths = vroom(list.files("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\Subset\\Trials\\flags_new_polygon_1km", full.names = TRUE))

##More in depth_polygon on 30 gauges. 1km
Eff_widths = vroom(list.files("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\Subset\\Trials\\in_depth_polygon\\1km", full.names = TRUE))

##More in depth_polygon on 30 gauges. 2 km. 
Eff_widths = vroom(list.files("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\Subset\\Trials\\in_depth_polygon\\2km", full.names = TRUE))



Eff_widths = as.data.frame(Eff_widths)
Eff_widths$system.index = Eff_widths$`system:index` 
Eff_widths$system.index = gsub("^[[:digit:]]_", "", Eff_widths$system.index)
Eff_widths$system.index = substr(Eff_widths$system.index, 0, 20)
d_dates = str_sub(Eff_widths$system.index, start = -8)
d_date = as.Date(d_dates, format = "%Y%m%d")
Eff_widths$Date = d_date

data = distinct(Eff_widths, ID, .keep_all = TRUE)
tab = data
Lat_lon_df = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\NA_quantiles_combined.csv")
data$lon_dd = Lat_lon_df$lon_dd[match(data$ID, Lat_lon_df$ID_2)]
data$lat_dd= Lat_lon_df$lat_dd[match(data$ID, Lat_lon_df$ID_2)]
################################################################################################################
area_files = list.files("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\Quantiles\\Area\\Area")
length_files = list.files("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\Quantiles\\Lengths\\Lengths")

area_files = gsub("Area__", "", area_files)
area_files = gsub(".csv", "", area_files)

length_files = gsub("length__", "", length_files)
length_files = gsub(".csv", "", length_files)

length_files_filter = length_files[length_files %in% area_files]
length_files = paste0("length__", length_files_filter, ".csv")
length_files = paste0("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\Quantiles\\Lengths\\Lengths\\", length_files)

Lat_lon_df = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\NA_quantiles_combined.csv")

Quantile_area = vroom(list.files("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\Quantiles\\Area\\Area", full.names = TRUE))
Quantile_area = as.data.frame(Quantile_area)
Quantile_area = Quantile_area[!duplicated(Quantile_area$id),]

##Quantile_lengths = vroom(list.files("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\Quantiles\\Lengths\\Lengths", full.names = TRUE))
Quantile_lengths = vroom(length_files)
Quantile_lengths = as.data.frame(Quantile_lengths)
Quantile_lengths = Quantile_lengths[!duplicated(Quantile_lengths$id),]


Quantile_area = Quantile_area[which(Quantile_lengths$id %in% Quantile_area$id),]
Quantile_lengths = Quantile_lengths[which(Quantile_area$id %in% Quantile_lengths$id),]


Eff_widths = vroom(list.files("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\widths\\Effective_widths", full.names = TRUE))
Eff_widths = as.data.frame(Eff_widths)
Eff_widths$system.index = Eff_widths$`system:index` 
Eff_widths$system.index = gsub("^[[:digit:]]_", "", Eff_widths$system.index)
Eff_widths$system.index = substr(Eff_widths$system.index, 0, 20)
d_dates = str_sub(Eff_widths$system.index, start = -8)
d_date = as.Date(d_dates, format = "%Y%m%d")
Eff_widths$Date = d_date

Quantile_area$lon_dd = Lat_lon_df$lon_dd[match(Quantile_area$id, Lat_lon_df$ID_2)]
Quantile_area$lat_dd= Lat_lon_df$lat_dd[match(Quantile_area$id, Lat_lon_df$ID_2)]

Quantile_area$ID = Quantile_area$id
Quantile_area$ID_2 = Quantile_area$id

Quantile_area = Quantile_area[, order(names(Quantile_area))]
width_cols = grep("q" ,names(Quantile_area))
width_cols_ordered = grep("q[[:digit:]]", names(Quantile_area))
width_cols_new = paste("w", seq(0, 100, 1), sep = "")

Quantile_lengths = Quantile_lengths[, order(names(Quantile_lengths))]
width_cols_l = grep("q" ,names(Quantile_lengths))
width_cols_ordered_l = grep("q[[:digit:]]", names(Quantile_lengths))
width_cols_new_l = paste("length", seq(0, 100, 1), sep = "")


Quantile_area = Quantile_area[order(Quantile_area$id),]
Quantile_lengths = Quantile_lengths[order(Quantile_lengths$id),]


if(all(Quantile_area$id == Quantile_lengths$id)){
test = as.data.frame(matrix(numeric(), nrow = nrow(Quantile_area), ncol = 101))
colnames(test)= width_cols_new
for (i in 1:ncol(test)){
  test[,i] = (Quantile_area[,width_cols_ordered[i]] * 900) / (Quantile_lengths[,width_cols_ordered_l[i]] * 30)
}
} 
#output = as.data.frame(matrix(numeric(), nrow =length(1), ncol = 101))

tabcols = grep("\\<[^w]", names(Quantile_area))
tab = cbind(test, Quantile_area[,tabcols])

tab$COMID = Eff_widths$COMID[match(tab$id, Eff_widths$ID)]

data = Quantile_area
#########################################################################################################################
gageinfo = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\width_val\\input\\gaugeData\\USGS\\gaugeTable.csv")
gageinfo = gageinfo[gageinfo$GRWL_width>99,]

##grwl x section length
grwl_l = 3

#How many xsections to consider
nGRWL = as.numeric(1)

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

wd_GRADES =setwd("E:\\research\\GRADES\\percentiles_1_gauges\\")
COMID_files = list.files(wd_GRADES)
COMID_vals = gsub(".csv", "", COMID_files)


#####################################################filter tab to only comids available - not needed once expanded. 
ryan = which(tab$COMID %in% COMID_vals)
ryan_df = tab[ryan,]
tab = ryan_df

COMID_vals_1 = which(COMID_vals%in%tab$COMID)
######################################################
COMID_vals = COMID_files[COMID_vals_1]


###how to rearrange GRADES data to easily add into tab. 
ry_d= as.data.frame(matrix(numeric(), nrow = 1, ncol = 101))
ry  = read.csv(COMID_files[1])
ry_d[1,] = ry[,1]
Q_cols_new = paste("Q", seq(0, 100, 1), sep = "")
colnames(ry_d) = Q_cols_new
ry_d[1,] = ry[,2]
tab_1 = as.data.frame(matrix(numeric(), nrow = length(COMID_vals), ncol = 102))
colnames(tab_1) = c(Q_cols_new, "COMID")
for(i in 1:length(COMID_vals)){
  ry  = read.csv(COMID_vals[i])
  ry_d[1,] = ry[,1]
  Q_cols_new = paste("Q", seq(0, 100, 1), sep = "")
  colnames(ry_d) = Q_cols_new
  ry_d[1,] = ry[,2]
  tab_1[i,1:101] = ry_d
  tab_1[i,102] = gsub(".csv", "", COMID_vals[i])
  print(COMID_vals[i])
}
#as.data.frame(tab_1)
#colnames(tab_1[,1:101]) = colnames(ry_d)
#tab_1 = cbind(COMID_vals, tab_1)

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
  q_w = cbind(q_cms, w_m, usgs_w_filter$measurement_dt)
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
start = as.Date("1979-01-01")

data_val = Eff_widths

data_val$ID = data_val$ID
data_val$ID_2 = data_val$ID
data_val$calc_mean = data_val$Effective_width
data_val_8415 = data_val[data_val$Date<as.Date("2015-01-01"),]
data_val_1520 = data_val[data_val$Date>as.Date("2014-12-31"),]
data_val = data_val_1520
data_val$Date = as.character(data_val$Date)



tab$ID = tab$id
tab$ID_2 = tab$id
tab$width_m = data_val$width_m[match(tab$ID, data_val$ID)]


tab$change= tab$median
tab$change[mapply(is.na, tab$change)] <- 0
tab$width_m = data$width_m[match(tab$ID_2, data$ID_2)]
Gauge_comid = vroom(list.files("E:\\research\\GRADES\\Gauge_rawdata_19842014\\", full.names = TRUE))
Gauge_comid = as.data.frame(Gauge_comid)
Gauge_comid$date = start+Gauge_comid$time
data_val$Date = as.character(data_val$Date)
data_val_8415$Date = as.Date(data_val_8415$Date)
xSecq=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecw=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecIDcol=grep("V", names(Site_number_xsections))
mInd = array(5, dimnames = NULL)
rangedf_1 = as.data.frame(matrix(numeric(), nrow = 1, ncol = 4))

gage_stats = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
gage_stats_GRADES = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 14))
colnames(gage_stats_GRADES)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "p_val","Bias", "RRMSE", "avg_std", "change", 'RRMSE_median', "std_Q", "STDE")
as.data.frame(gage_stats_GRADES)
l_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
u_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
sd_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
width_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))

colnames(gage_stats)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "p_val","Bias", "RRMSE","avg_std", "change", "RRMSE_median", "std_Q","STDE", "KGE", "NSE", "rBias",
                        "SDRR", "MRR", "NRMSE")
as.data.frame(gage_stats)
gage_stats_col1 = as.vector(1)
gage_stats_col2 = as.vector(1)
gage_stats_GRADES_col1 = as.vector(1)
gage_stats_GRADES_col2 = as.vector(1)
paired_df_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
rmse = 1
pdfOut = "E:/research/temp_plots/USA_Effective_Widths_Paired_unchanged.pdf"
pdf(pdfOut)

width_grouping = 60

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
  
  #xSecw=wTab[mInd,] ##Commented out to quickly do paired method. 
  #xSecq=qTab[mInd,] ##Commented out to quickly do paired method. 
  xSecw = as.data.frame(seq(1:100))
  xSecq = as.data.frame(seq(1:100))
  
  #xSecw = xSecw[,11:91]
  #xSecq = xSecq[,11:91]
  mInd_paired = which(data_val_8415$ID %in% xSecID)
  paired_df = data_val_8415[mInd_paired,]
  # try(if(regmatches(unique(paired_df$COMID), regexpr("\\d", unique(paired_df$COMID))) == 7){
  # Q_df = nc_function(unique(paired_df$COMID))}
  # else{Q_df = nc_function_8(unique(paired_df$COMID)[1])})
  # paired_df = try(left_join(paired_df, Q_df, by = c("COMID" = "COMID", "Date" = "date")))
  
  
  if(nrow(paired_df)>0){
    paired_df = try(left_join(paired_df, Gauge_comid, by = c("COMID" = "COMID", "Date" = "date")))
    if(all(is.na(paired_df$Q))){next}
    #paired_df = try(aggregate(cbind(calc_mean, Q)~Date+COMID, paired_df,mean))
    #t.spl = approxfun(paired_df$calc_mean, paired_df$Q)
    p_q = try(quantile(paired_df$Q, probs = seq(0,1,.01), na.rm = TRUE))
    p_w = try(quantile(paired_df$calc_mean, probs = seq(0,1,.01), na.rm = TRUE))
    
    #pd_l = t.apr(min(paired_df$calc_mean):max(paired_df$calc_mean))
    # t.apr_mx = try(approxfun(paired_df$calc_mean, paired_df$Q, ties = max))
    # t.apr_mn = try(approxfun(paired_df$calc_mean, paired_df$Q, ties = min))
    Natural_breaks = getJenksBreaks(paired_df$calc_mean, .1 * nrow(paired_df))
    
    pxl_breaks = seq(0,max(paired_df$calc_mean, na.rm = TRUE)+width_grouping, by = width_grouping)
    
    Natural_breaks = pxl_breaks
    
    
    
    
    min_df = try(as.vector(length(Natural_breaks)))
    max_df = try(as.vector(length(Natural_breaks)))
    
    if(min_df>2){
      
      # for(r in 1:length(Natural_breaks)){
      #   min_df[r] = min(paired_df$Q[paired_df$calc_mean>Natural_breaks[r-1] & paired_df$calc_mean<Natural_breaks[r]], na.rm = TRUE)
      #   max_df[r] = max(paired_df$Q[paired_df$calc_mean>Natural_breaks[r-1] & paired_df$calc_mean<Natural_breaks[r]], na.rm = TRUE)
      # }
      
      for(r in 1:length(Natural_breaks)){
        min_df[r] = sd(paired_df$Q[paired_df$calc_mean>Natural_breaks[r] & paired_df$calc_mean<Natural_breaks[r+1]], na.rm = TRUE)
        max_df[r] = sd(paired_df$Q[paired_df$calc_mean>Natural_breaks[r] & paired_df$calc_mean<Natural_breaks[r+1]], na.rm = TRUE)
      }
      print(min_df)
      
      
      natural_breaks_df = as.data.frame(Natural_breaks)
      natural_breaks_df$min = min_df
      natural_breaks_df$max = max_df
      
      # t.apr_mx = try(approxfun(paired_df$calc_mean, paired_df$Q, method = "constant", rule = 1, f = 1,ties = max))
      # t.apr_mn = try(approxfun(paired_df$calc_mean, paired_df$Q, method = "constant", rule = 1, f = 0,ties = min))
      
      t.apr_mx = try(approxfun(natural_breaks_df$Natural_breaks, natural_breaks_df$max, method = "constant", rule = 2, f = 1,ties = max))
      t.apr_mn = try(approxfun(natural_breaks_df$Natural_breaks, natural_breaks_df$min, method = "constant", rule = 2, f = 1,ties = max))
      # plot(paired_df$calc_mean, paired_df$Q)
      # for(i in 1:nrow(paired_df[order(paired_df$Q),])){
      #   points(paired_df$calc_mean[i], min(t.apr_mn(paired_df$calc_mean[i-2:i+2]), na.rm = TRUE), col = "blue")
      #   points(paired_df$calc_mean[i], max(t.apr_mx(paired_df$calc_mean[i-2:i+2]), na.rm = TRUE), col = "red")
      #   
      #   }
      
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
      
      # data_val_subset = try(subset(data_val, xSecID[,1]==data_val$ID | xSecID[,2]==data_val$ID| 
      #                                xSecID[,3]==data_val$ID| xSecID[,4]==data_val$ID| xSecID[,5]==data_val$ID))
      data_val_subset = subset(data_val, xSecID[,1] ==data_val$ID)
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
        #mx = xSecw[order(xSecw$w50, decreasing = TRUE),] ##Commented out to do quick paired approach.
        
        # 
        # wm = function(tbl){
        #   wm = weighted.mean(tbl, c(.8, .1, .05, .025, .025))
        #   return(wm)
        # }
        
        # if(mean(tab$width_m[mInd], na.rm = TRUE)>600){
        #   w_rc = w_rc
        # } else{w_rc = w_rc * 1.33}
        # 
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
        
        ### change to use paired method for rating curves. 
        w_rc= p_w
        q_rc = p_q
        
        #lines(q_rc, w_rc, type = "l", lwd = 3) ##average xsection line before interpolation. 
        ##add in lines of rating curves to each plot. 
        #for (k in 1:nrow(xSecq)){     
          #   #print(xSecq)
          #lines(as.numeric(xSecq[k,]), as.numeric(xSecw[k,]), type="l", lwd = 0.5, col = "blue")
          
          
          ##creates continuous dataset for each individual cross section. 
          ###spl_c = try(approxfun(as.numeric(xSecw[k,]), as.numeric(xSecq[k,]), ties = "ordered"))
          #try(lines(spl_c(as.numeric(xSecw[k,])), as.numeric(xSecw[k,]), col = k))
          
          # ####create a new spline function for each rating curve. 
          # spl_1=try(approxfun(as.numeric(xSecw[1,]), as.numeric(xSecq[1,])))#, method = "hyman", ties = "ordered"))
          # spl_2=try(approxfun(as.numeric(xSecw[2,]), as.numeric(xSecq[2,])))#, method = "hyman", ties = "ordered"))
          # spl_3=try(approxfun(as.numeric(xSecw[3,]), as.numeric(xSecq[3,])))#, method = "hyman", ties = "ordered"))
          # spl_4=try(approxfun(as.numeric(xSecw[4,]), as.numeric(xSecq[4,])))#, method = "hyman", ties = "ordered"))
          # spl_5=try(approxfun(as.numeric(xSecw[5,]), as.numeric(xSecq[5,])))#, method = "hyman", ties = "ordered"))
          # 
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
          #spl_sd = try(splinefun(x, w_sd))
       # }
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
          u$Group.1 = as.character(u$V3)
          usgs_widths_landsat = right_join(u, data_val_sub_agg)
        }
          #try(plot(usgs_widths_landsat$V3, usgs_widths_landsat$x))
          
          #u_w_q = try(quantile(u$w_m, probs = seq(0, 1, .01), na.rm = TRUE))
          #u_q_q = try(quantile(u$q_cms, probs = seq(0, 1, .01), na.rm = TRUE))
          #try(lines(u_q_q,u_w_q,type = "l", lty = 2, col = "red"))
          usgs_q = try(usgs_q_processing(read.csv(usgs_q_list[i], stringsAsFactors = FALSE)))
          
          if(!is.error(data_val_sub_agg) & !is.error(usgs_q)){
            usgs_q_ind=which(usgs_q$datetime %in% data_val_sub_agg$Group.1)
            usgs_q_subset= usgs_q[usgs_q_ind,]
            usgs_q_subset[order(usgs_q_subset$datetime),]
            data_val_sub_agg_ind=which(data_val_sub_agg$Group.1 %in% usgs_q_subset$datetime)
            data_val_sub_agg_1 = data_val_sub_agg[data_val_sub_agg_ind,]
            data_val_sub_agg_1[order(data_val_sub_agg_1$Group.1),]
            
            ##alternate SD method. 
            # ds_df = data_val
            # ds_df$spl1 = try(spl_1(ds_df$calc_mean))
            # ds_df$spl2 = try(spl_2(ds_df$calc_mean))
            # ds_df$spl3 = try(spl_3(ds_df$calc_mean))
            # ds_df$spl4 = try(spl_4(ds_df$calc_mean))
            # ds_df$spl5 = try(spl_5(ds_df$calc_mean))
            # 
            # ds_df_agg = ds_df[,12 & 14:18] %>% group_by(Date) %>%
            #   mutate(grp = 1:n())%>%
            #   gather(var, val, -Date, -grp) %>%
            #   unite("var_grp", var, grp, sep ='') %>%
            #   spread(var_grp, val, fill = '')
            # 
            # ds_df_spl_names = grep("^spl", colnames(ds_df_agg))
            # ds_df_sd = apply(ds_df_agg[,ds_df_spl_names], 1, FUN = sd, na.rm = TRUE)
            # ds_df_sd1 = cbind(as.vector(ds_df_agg$Date), as.vector(ds_df_sd))
            # ds_df_sd1 = as.data.frame(ds_df_sd1)
            # ds_df_sd1$date = as.Date(ds_df_sd1$V1)
            # 
            # 
            # data_val_sub_agg_sd = try(aggregate(data_val_subset$calc_mean, by=list(data_val_subset$Date), FUN=sd))
            # data_val_sub_agg_2 = data_val_sub_agg_1
            # data_val_sub_agg_2$sd = data_val_sub_agg_sd$x[match(data_val_sub_agg_1$Group.1, data_val_sub_agg_sd$Group.1)]
            
            ## usgs width vs usgs discharge points. 
            # points(usgs_q_subset$q,data_val_sub_agg_1$x, pch=17, col="green")
            u_1=cbind(usgs_q_subset$q, data_val_sub_agg_1$x)
            u = as.data.frame(u_1)
            
            
            #data_Val_spl = spl(data_val_sub_agg_1$x)
            #try(print(spl(data_val_sub_agg_1$x)))
            dv = as.vector(nrow(data_val_sub_agg_1))
            #sd_dv = try(spl_sd(data_val_sub_agg_1$x)) ##added
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
              try(plot(c(min(x, na.rm = TRUE), max(x, na.rm = TRUE)),c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)), col = "white", 
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
              points(paired_df$calc_mean, paired_df$Q, col = "red")
              abline(v = Natural_breaks, lwd = 0.25, col = "lightgray")
              poly_bound_mx = cbind(x+w_sd, spl(x))
              poly_bound_mn = cbind(x-w_sd, spl(x))
              
              ##represents error margins from rating curves throughout plot. 
              # polygon(c(poly_bound_mn[,1],  max(poly_bound_mx[,1], na.rm = TRUE), rev(poly_bound_mx[,1]), min(poly_bound_mn[,1], na.rm = TRUE))
              #         ,c(poly_bound_mn[,2],  max(poly_bound_mx[,2], na.rm = TRUE), rev(poly_bound_mx[,2]), min(poly_bound_mn[,2], na.rm = TRUE)),
              #          col = yarrr::transparent("red", trans.val = .95), border = NA)
              
              
              a=try(lines(x ,spl(x), col = "black", lwd = 2))
              #try(lines(x+w_sd, spl(x), col = "red"))
              #try(lines(x - w_sd, spl(x), col = "red"))
              #points(data_val_sub_agg_1$x,usgs_q_subset$q, pch=17, col="green")
              try(points(usgs[,2], usgs[,1], col = "lightgray", lwd = 0.5))
              try(points(data_val_sub_agg_1$x,dv,pch = 1, col = "blue"))
              #try(points(dv, data_val_sub_agg_1$x+ spl_sd(data_val_sub_agg_1$x), pch = 17, col = "blue"))
              
              ##new error bars: represent each day sd from landsat withds. 
              # try(arrows(y0 = dv, x0= data_val_sub_agg_1$x - data_val_sub_agg_2$sd, 
              #            y1 = dv, x1 = data_val_sub_agg_1$x + data_val_sub_agg_2$sd, col = "indianred" , 
              #            code=3, angle = 180, length = 0, lwd = 0.5))
              
              ##min and max values from paired data. 
              # try(arrows(y0 = dv - t.apr_mn(data_val_sub_agg_1$x), x0= data_val_sub_agg_1$x, 
              #            y1 = dv + t.apr_mx(data_val_sub_agg_1$x), x1 = data_val_sub_agg_1$x, col = "indianred" , 
              #            code=3, angle = 180, length = 0, lwd = 0.5))
              
              
              landsat_df = as.data.frame(data_val_sub_agg_1$x)
              landsat_df$landsat_mn = t.apr_mn(landsat_df[,1])
              landsat_df$landsat_mx = t.apr_mx(landsat_df[,1])
              landsat_df[,1] = dv
              
              landsat_df$landsat_mn = dv - landsat_df$landsat_mn
              landsat_df$landsat_mx = dv + landsat_df$landsat_mx
              
              landsat_df$minimum = apply(landsat_df,1, FUN = min, na.rm = TRUE)
              
              for(m in 1:nrow(landsat_df)){
                if(sign(landsat_df$minimum[m]) == -1){
                  landsat_df$minimum[m] = 0
                }
              }
              
              
              landsat_df$maximum = apply(landsat_df,1, FUN = max, na.rm = TRUE)
              
              try(arrows(y0 = landsat_df$minimum, x0= data_val_sub_agg_1$x, 
                         y1 = landsat_df$maximum, x1 = data_val_sub_agg_1$x, col = "indianred" , 
                         code=3, angle = 180, length = 0, lwd = 0.5))
              

              #######Natural breaks arrows. 
              # try(arrows(y0 = landsat_df$minimum, x0= data_val_sub_agg_1$x, 
              #            y1 = landsat_df$maximum, x1 = data_val_sub_agg_1$x, col = "indianred" , 
              #            code=3, angle = 180, length = 0, lwd = 0.5))
              # 
              
              
              
              
              
              
              
              
              
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
              
              # try(plot(c(u_l_mn, u_l_mx), c(u_l_mn, u_l_mx), type = "n", xlab = "In situ Discharge (cms)", ylab = "Landsat Discharge (cms)", main = "In situ vs Landsat"))
              try(plot(c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)),c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)), type = "n", xlab = "In situ Discharge (cms)", ylab = "Landsat Discharge (cms)", main = "In situ vs Landsat"))
              points(u$V1, l$dv, col = "blue")
              abline(0,1)
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
              # gage_stats_col10 = mean(abs(tab$change[mInd]))
              # gage_stats$change[i] = gage_stats_col10
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
            gage_stats$KGE[i] = KGE(l$dv, u$V1)
            gage_stats$NSE[i] = NSE(l$dv, u$V1)
            gage_stats$rBias[i] = rBias(l$dv, u$V1)
            
            gage_stats$SDRR[i] = SDRR(l$dv, u$V1)
            gage_stats$MRR[i] = MRR(l$dv, u$V1)
            gage_stats$NRMSE[i] = NRMSE(l$dv, u$V1)
            
            
            
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
              #gage_stats_col8 = tab$width_m[mInd]
              #gage_stats$GRWL_width_m[i] = gage_stats_col8
              } else{next}
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
              #ry = try(cbind(spl_1(data_val_sub_agg_1$x), spl_2(data_val_sub_agg_1$x), spl_3(data_val_sub_agg_1$x), spl_4(data_val_sub_agg_1$x), spl_5(data_val_sub_agg_1$x)))
              
              
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
              # ds_join = left_join(usgs_q_2015_wval, ds_df_sd1)
              # usgs_q_2015_wval = ds_join
              # usgs_q_2015_wval$V2[mapply(is.na, usgs_q_2015_wval$landat)] = NA
              # usgs_q_2015_wval$V2 = as.numeric(as.character(usgs_q_2015_wval$V2))
              # usgs_q_2015_wval$sd_mx1 = usgs_q_2015_wval$landat + usgs_q_2015_wval$V2
              # usgs_q_2015_wval$sd_mn1 = usgs_q_2015_wval$landat - usgs_q_2015_wval$V2
              # 
              # usgs_q_2015_wval$sd_mx1 = usgs_q_2015_wval$landat + t.apr_mx(usgs_q_2015_wval$landat)
              # usgs_q_2015_wval$sd_mn1 = usgs_q_2015_wval$landat - t.apr_mn(usgs_q_2015_wval$landat)
              
              
              
              ###### insitu vs landsat error updated to new error bars. 
              in_situ_sd = usgs_q_2015_wval[!is.na(usgs_q_2015_wval$landat),]
              in_situ_sd_mx = in_situ_sd$sd_mx1[order(in_situ_sd$q)]
              in_situ_sd_mn = in_situ_sd$sd_mn1[order(in_situ_sd$q)]
              
              
              # ####changed the gage_stats sd
              # gage_stats_col13 = mean(usgs_q_2015_wval$V2)
              # #gage_stats_col13 = mean(usgs_q_2015_wval$sd, na.rm = TRUE)
              # gage_stats$std_Q[i] = gage_stats_col13
              # gage_stats_col14 = sqrt(var(error, na.rm = TRUE))
              # gage_stats$STDE[i] = gage_stats_col14
              # mean_q = mean(usgs_q_2015_wval$landat, na.rm = TRUE)
              # 
              
              
              try(arrows(x0 = u$V1, y0=  landsat_df$minimum, 
                         x1 = u$V1, y1 = landsat_df$maximum, col = "indianred",
                         code=3, angle = 180, length = 0, lwd = 0.5))
              
              
              # try(arrows(x0 = in_situ_sd$q[order(in_situ_sd$q)], y0=  in_situ_sd_mn - mean_q, 
              #            x1 = in_situ_sd$q[order(in_situ_sd$q)], y1 = in_situ_sd_mx + mean_q, col = "indianred",
              #            code=3, angle = 180, length = 0, lwd = 0.5))
              
              try(plot(usgs_q_2015_wval$date, usgs_q_2015_wval$q,ylim = c(min(spl(x), na.rm = TRUE),
                                                                          max(spl(x), na.rm = TRUE)), xlab = "", ylab = "Discharge (cms)", type = "l", col = "lightgray"))
              
              try(points(usgs_q_2015_wval$date,usgs_q_2015_wval$landat, col = "blue"))
              
              data_val_sub_agg_1$widths = data_val_sub_agg_1$x
              usgs_q_2015_wval$landsat = usgs_q_2015_wval$landat
              usgs_joined = left_join(usgs_q_2015_wval, data_val_sub_agg_1, by = "date")
              
              ###fix error bars going into negative Q.
              
              # landsat_mn = usgs_joined$landsat[!is.na(usgs_joined$landsat)] - t.apr_mn(usgs_joined$widths[!is.na(usgs_joined$landsat)])
              # landsat_mx = usgs_joined$landsat[!is.na(usgs_joined$landsat)] + t.apr_mx(usgs_joined$widths[!is.na(usgs_joined$landsat)])
              # 
              # for(p in 1:length(landsat_mn)){
              # if(sign(landsat_mn[p])==-1){
              #   landsat_mn[p] = 0
              # }}
              
              ##########################Natural Breaks. 
              landsat_df = usgs_joined$widths[!is.na(usgs_joined$landsat)]
              landsat_df = as.data.frame(landsat_df)
              landsat_df$landsat_mn = t.apr_mn(landsat_df[,1])
              landsat_df$landsat_mx = t.apr_mx(landsat_df$landsat_df)
              landsat_df[,1] = usgs_joined$landsat[!is.na(usgs_joined$landsat)]
              
              landsat_df$minimum = apply(landsat_df,1, FUN = min, na.rm = TRUE)
              landsat_df$maximum = apply(landsat_df,1, FUN = max, na.rm = TRUE)
              
              
              try(arrows(x0 = usgs_joined$date[!is.na(usgs_joined$landsat)], y0=  landsat_df$minimum, 
                         x1 = usgs_joined$date[!is.na(usgs_joined$landsat)], y1 = landsat_df$maximum, col = "indianred",
                         code=3, angle = 180, length = 0, lwd = 0.5))
              ########################################################################
              
              
              ###Using sd of Q for each grouping of width pixels. 
              landsat_df = usgs_joined$widths[!is.na(usgs_joined$landsat)]
              landsat_df = as.data.frame(landsat_df)
              landsat_df$landsat_mn = t.apr_mn(landsat_df[,1])
              landsat_df$landsat_mx = t.apr_mx(landsat_df$landsat_df)
              landsat_df[,1] = usgs_joined$landsat[!is.na(usgs_joined$landsat)]
              
              landsat_df$minimum = landsat_df$landsat_df - landsat_df$landsat_mn
              landsat_df$maximum = landsat_df$landsat_df + landsat_df$landsat_mx
              
              
              
              for(m in 1:nrow(landsat_df)){
                if(!is.na(landsat_df$minimum[m])){
                if(sign(landsat_df$minimum[m]) == -1){
                  landsat_df$minimum[m] = 0
                } }else{next}
              }
              
              
              landsat_df$minimum = apply(landsat_df,1, FUN = min, na.rm = TRUE)
              landsat_df$maximum = apply(landsat_df,1, FUN = max, na.rm = TRUE)
              
              
              try(arrows(x0 = usgs_joined$date[!is.na(usgs_joined$landsat)], y0=  landsat_df$minimum, 
                         x1 = usgs_joined$date[!is.na(usgs_joined$landsat)], y1 = landsat_df$maximum, col = "indianred",
                         code=3, angle = 180, length = 0, lwd = 0.5))
              
              
              
              
              
              landsat_diff = landsat_df$maximum - landsat_df$minimum
              ####changed the gage_stats sd
              gage_stats_col13 = mean(landsat_diff)
              #gage_stats_col13 = mean(usgs_q_2015_wval$sd, na.rm = TRUE)
              gage_stats$std_Q[i] = gage_stats_col13
              gage_stats_col14 = sqrt(var(error, na.rm = TRUE))
              gage_stats$STDE[i] = gage_stats_col14
              mean_q = mean(usgs_q_2015_wval$landat, na.rm = TRUE)
              
              sd_vals[i, 1:length(usgs_q_2015_wval$landsat[!is.na(usgs_q_2015_wval$landat)])] = landsat_diff
              width_vals[i, 1:length(usgs_q_2015_wval$landsat[!is.na(usgs_q_2015_wval$landat)])] = usgs_q_2015_wval$x[!is.na(usgs_q_2015_wval$landat)] 
              

              ##represent errors from determing each Q across all rating curves. Maybe too much? 
              # try(arrows(x0 = usgs_q_2015_wval$date, y0=  usgs_q_2015_wval$sd_mn1 - mean_q, 
              #            x1 = usgs_q_2015_wval$date, y1 = usgs_q_2015_wval$sd_mx1 + mean_q, col = "indianred",
              #            code=3, angle = 180, length = 0, lwd = 0.5))
              
              ##old error bars. represent sd from using 5 ind rating curves to determine Q. 
              # try(arrows(x0 = usgs_q_2015_wval$date, y0= usgs_q_2015_wval$sd_mn - mean_q, 
              #             x1 = usgs_q_2015_wval$date, y1 = usgs_q_2015_wval$sd_mx + mean_q, col = "indianred",
              #             code=3, angle = 180, length = 0, lwd = 0.5))
              
              
              ####change sd vals to new sd. 
              # sd_vals[i, 1:length(usgs_q_2015_wval$V2[!is.na(usgs_q_2015_wval$landat)])] = usgs_q_2015_wval$V2[!is.na(usgs_q_2015_wval$landat)]
              
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
              
           # }else{next} ## for paired_df. 
            
          } else{next} ### this one for min df. 
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

gage_stats$dams = gage_regulation_usgs$distance[match(gage_stats$Site_number, gage_regulation_usgs$stationid)]


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
plot(ecdf(abs(gage_stats$SDRR)), col = "red", xlim = c(0, 100), main = "")
lines(ecdf(abs(gage_stats$MRR)), col = "blue")
lines(ecdf(abs(gage_stats$RRMSE)), col = "forestgreen")

gage_stats = gage_stats[!is.na(gage_stats$Site_number),]

n = nrow(gage_stats)
plot(abs(gage_stats$RRMSE[order(abs(gage_stats$RRMSE))]),(1:n)/n, type = "l", col = "green")
lines(abs(gage_stats$SDRR[order(abs(gage_stats$SDRR))]),(1:n)/n, type= "l", col = "yellow")
lines(abs(gage_stats$MRR[order(abs(gage_stats$MRR))]),(1:n)/n, type = "l", col = "blue")







########################################################################################## Avg. Model Q for each Gauge.

Q_df = apply(l_vals, 1, FUN = mean, na.rm = TRUE)


########################################################################################## Actual cumulative error statistics across all gauges for entire timeperiod. 
norm_rmse = gage_stats$RMSE/Q_df
norm_bias = abs(gage_stats$Bias)/Q_df
norm_stde = gage_stats$STDE/Q_df

norm_stde = norm_stde[is.na(gage_stats$dam) & gage_stats$change<10]
norm_bias = norm_bias[is.na(gage_stats$dam) & gage_stats$change<10]
norm_rmse = norm_rmse[is.na(gage_stats$dam) & gage_stats$change<10]

plot.new()
par(mfrow = c(1, 1))
plot(ecdf(norm_rmse), xlim = c(0, 5), main = "Effective Widths Occurrence: Actual cumulative error statistics", col = "forestgreen", cex = 0.15)
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
GRADES_b = Q_df * 0.2 #0.5
Total_estimated_error = GRADES_b + sd_vals
Total_estimated_error = sd_vals

error_estimate = l_vals[!is.na(l_vals)] - sd_vals
mean_error_estimate = apply(Total_estimated_error, 1, FUN = mean, na.rm = TRUE)
STDE_estimate = sqrt(apply(Total_estimated_error, 1, FUN = var, na.rm = TRUE))
STDE_estimate[STDE_estimate==0] = NA


rmse_estimate = Total_estimated_error^2
rmse_estimate = sqrt(apply(rmse_estimate, 1, FUN = mean, na.rm = TRUE))


###Converts infinite values to NA. 
mean_error_estimate[is.infinite(mean_error_estimate)] = NA
STDE_estimate[is.infinite(STDE_estimate)] = NA
rmse_estimate[is.infinite(rmse_estimate)] = NA

plot((rmse_estimate), sqrt((STDE_estimate^2) + abs(mean_error_estimate^2))) ### shows proper relationship. They are all calculated consistently. 

####Normalized estimated values. 

norm_rmse_est = rmse_estimate/Q_df
norm_bias_est = abs(mean_error_estimate)/Q_df
norm_stde_est = STDE_estimate/Q_df

norm_stde_est = norm_stde_est[is.na(gage_stats$dam) & gage_stats$change<10]
norm_rmse_est = norm_rmse_est[is.na(gage_stats$dam) & gage_stats$change<10]
norm_bias_est = norm_bias_est[is.na(gage_stats$dam) & gage_stats$change<10]



############################################################################################### Cedric's plots. Accounting for more Bias. 
plot.new()
par(mfrow =c(2,2), oma = c(0, 0, 2, 0))
plot(ecdf(norm_rmse_est), xlim = c(0, quantile(ecdf(norm_rmse_est), .95)), col = "forestgreen", main = "Cumulated estimated error metrics", cex.main = 0.8, cex = 0.15)
mtext("Paired: Estimated error metrics", outer = TRUE, cex = 1.5)
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
plot(c(bias_mn, bias_mx), c(bias_mn, bias_mx), type = "n", log = "xy", xlab = "actual Q error", ylab = " estimated Q error",
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
plot(c(stde_mn, stde_mx), c(stde_mn, stde_mx), type = "n", log = "xy", xlab = "actual Q error", ylab = "estimated Q error",
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
plot(c(rmse_mn, rmse_mx), c(rmse_mn, rmse_mx), type = "n", log = "xy", xlab = "actual Q error", ylab = "estimated Q error",
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

9#####################

paired_l_vals = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\Stats\\Paired\\Paired.l_vals.csv")
paired_u_vals = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\Stats\\Paired\\Paired.u_vals.csv")
paired_gage_stats = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\Stats\\Paired\\Paired.gage_stats.csv")

rBias_df = as.data.frame(matrix(numeric(), nrow = nrow(paired_gage_stats), ncol = 2))

for(i in 1:nrow(paired_l_vals)){
  a = rBias(as.vector(as.numeric(paired_l_vals[i,])), as.vector(as.numeric(paired_u_vals[i,])))
  rBias_df[i,1] = a
}

paired_gage_stats$rBias = rBias_df$V1
paired_gage_stats$rBias[paired_gage_stats$rBias==0] = NA


nRMSE_df = as.data.frame(matrix(numeric(), nrow = nrow(paired_gage_stats), ncol = 2))

for(i in 1:nrow(paired_l_vals)){
  a = NRMSE(as.vector(as.numeric(paired_l_vals[i,])), as.vector(as.numeric(paired_u_vals[i,])))
  nRMSE_df[i,1] = a
}

paired_gage_stats$NRMSE = nRMSE_df$V1
paired_gage_stats$NRMSE[paired_gage_stats$NRMSE==0] = NA



KGE_df = as.data.frame(matrix(numeric(), nrow = nrow(paired_gage_stats), ncol = 2))

for(i in 1:nrow(paired_l_vals)){
  a = KGE(as.vector(as.numeric(paired_l_vals[i,])), as.vector(as.numeric(paired_u_vals[i,])))
  KGE_df[i,1] = a
}

paired_gage_stats$KGE = KGE_df$V1


NSE_df = as.data.frame(matrix(numeric(), nrow = nrow(paired_gage_stats), ncol = 2))

for(i in 1:nrow(paired_l_vals)){
  a = NSE(as.vector(as.numeric(paired_l_vals[i,])), as.vector(as.numeric(paired_u_vals[i,])))
  NSE_df[i,1] = a
}

paired_gage_stats$NSE = NSE_df$V1

norm_bias = abs(paired_gage_stats$Bias)/Q_df















occ_l_vals = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\Stats\\Occurrence\\Occ.l_vals.csv")
occ_u_vals = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\Stats\\Occurrence\\Occ.u_vals.csv")
occ_gage_stats = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\Stats\\Occurrence\\Occ.gage_stats.csv")
occ_sd_vals = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\Stats\\Occurrence\\Occ.sd_vals.csv")

rBias_df = as.data.frame(matrix(numeric(), nrow = nrow(occ_gage_stats), ncol = 2))

for(i in 1:nrow(occ_l_vals)){
  a = rBias(as.vector(as.numeric(occ_l_vals[i,])), as.vector(as.numeric(occ_u_vals[i,])))
  rBias_df[i,1] = a
}


occ_gage_stats$rBias = rBias_df$V1
occ_gage_stats$rBias[occ_gage_stats$rBias==0] = NA


nRMSE_df = as.data.frame(matrix(numeric(), nrow = nrow(occ_gage_stats), ncol = 2))

for(i in 1:nrow(occ_l_vals)){
  a = NRMSE(as.vector(as.numeric(occ_l_vals[i,])), as.vector(as.numeric(occ_u_vals[i,])))
  nRMSE_df[i,1] = a
}

occ_gage_stats$NRMSE = nRMSE_df$V1
occ_gage_stats$NRMSE[occ_gage_stats$NRMSE==0] = NA







KGE_df = as.data.frame(matrix(numeric(), nrow = nrow(occ_gage_stats), ncol = 2))

for(i in 1:nrow(occ_l_vals)){
  a = KGE(as.vector(as.numeric(occ_l_vals[i,])), as.vector(as.numeric(occ_u_vals[i,])))
  KGE_df[i,1] = a
}

occ_gage_stats$KGE = KGE_df$V1


NSE_df = as.data.frame(matrix(numeric(), nrow = nrow(occ_gage_stats), ncol = 2))

for(i in 1:nrow(occ_l_vals)){
  a = NSE(as.vector(as.numeric(occ_l_vals[i,])), as.vector(as.numeric(occ_u_vals[i,])))
  NSE_df[i,1] = a
}

occ_gage_stats$NSE = NSE_df$V1






























gage_stats$change = xsection_method$change[match(gage_stats$Site_number, xsection_method$Site_number)]
gage_stats$GRWL_width_m = xsection_method$GRWL_width_m[match(gage_stats$Site_number, xsection_method$Site_number)]


xsection_method = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Surface_reflectance\\Stats\\Gage_stats.csv")

xsection_method_filt = xsection_method[which(xsection_method$Site_number %in% gage_stats$Site_number),]
xsection_method_filt = xsection_method_filt[!is.na(xsection_method_filt$Site_number),]

gage_stats_filtered = gage_stats[which(gage_stats$Site_number %in% xsection_method_filt$Site_number),]


comp_df = as.data.frame(matrix(numeric(), nrow = nrow(gage_stats_filtered), ncol = 2))

for(i in 1:nrow(gage_stats_filtered)){
  a = gage_stats_filtered$RRMSE[i] - xsection_method_filt$RRMSE[i]
  comp_df[i,1] = sign(a)
  comp_df[i,2] = a
  }


gage_stats_filtered$comparison = comp_df$V1
gage_stats = gage_stats_filtered

gage_stats_dams_change = gage_stats[is.na(gage_stats$dams) & gage_stats$change<10,]













plot(gage_stats_filtered$RRMSE, xsection_method_filt$RRMSE)

Sites = paste0(gageinfo$SITE_NUM[gageinfo$GRWL_width>99], ",")
Sites = as.data.frame(Sites)


gage_stats$lat = gageinfo$LAT[match(gage_stats$Site_number, gageinfo$SITE_NUM)]
gage_stats$long = gageinfo$LONG[match(gage_stats$Site_number, gageinfo$SITE_NUM)]

ryan1 = st_as_sf(gage_stats[!is.na(gage_stats$Site_number),], coords = c("long", "lat"))
library(tmap)
data("World")
tmap_mode("view")


tm_shape(ryan1)+
tm_symbols(col = "RRMSE", size = "GRWL_width_m",scale = .5, midpoint = NA,breaks = c(0, 50, 75, 100), palette = "Set1")


tm_shape(ryan1)+
  tm_symbols(col = "change", size = "GRWL_width_m",scale = .5, midpoint = NA, breaks = c(0, 3, 5, 7, 10, 50))

tm_shape(ryan1[ryan1$change<10 & is.na(ryan1$dams),])+
  tm_symbols(col = "comparison", "GRWL_width_m", scale = .5)




##################################################################
##Calculate NSE

NSE_df = as.data.frame(matrix(numeric(), nrow = nrow(gage_stats), ncol = 1))

for(i in 1:nrow(l_vals)){
  a = NSE(as.vector(as.numeric(l_vals[i,])), as.vector(as.numeric(u_vals[i,])), na.rm = TRUE)
  NSE_df[i,1] = a
}

median(NSE_df$V1[gage_stats$change<10 & is.na(gage_stats$dams)], na.rm = TRUE)




####################################################################
##Calculate KGE

KGE_df = as.data.frame(matrix(numeric(), nrow = nrow(gage_stats), ncol = 1))

for(i in 1:nrow(l_vals)){
  a = KGE(as.vector(as.numeric(l_vals[i,])), as.vector(as.numeric(u_vals[i,])), na.rm = TRUE)
  KGE_df[i,1] = a
}

median(KGE_df$V1[gage_stats$change<10 & is.na(gage_stats$dams)], na.rm = TRUE)

gage_stats$NSE = NSE_df$V1
gage_stats$KGE = KGE_df$V1






RRMSE_df = as.data.frame(matrix(numeric(), nrow = nrow(gage_stats_filtered), ncol = 1))

for(i in 1:nrow(l_vals)){
  a = RRMSE(as.vector(as.numeric(l_vals[i,])), as.vector(as.numeric(u_vals[i,])))
  RRMSE_df[i,1] = a
}

median(RRMSE_df$V1[gage_stats$change<10 & is.na(gage_stats$dams)], na.rm = TRUE)
