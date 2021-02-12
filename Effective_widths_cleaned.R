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
library(data.table)
library(parallel)
library(foreach)

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

##Error functions. 
source("E:/research/2019_08_30_rivObs/git/src/Error_stats_functions.R")

###Either "USGS" or "Canada"
gauging = "USGS"

###Read in effective widths csv files. 
if(gauging == "USGS"){
Eff_widths = map_df(list.files("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\AGU\\USGS_validation\\USGS_AGU", full.names = TRUE), ~vroom(.x))
gageinfo = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\width_val\\input\\gaugeData\\USGS\\gaugeTable.csv")
} else{
  Eff_widths = map_df(list.files("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Effective_widths\\AGU\\USGS_validation\\CAN_AGU\\Canadian_AGU", full.names = TRUE), ~vroom(.x))
  gageinfo = read.csv("E:\\research\\GRDC\\Canada_stationid_filtered_lakes_widths.csv")
  gageinfo$SITE_NUM = gageinfo$Station_Num
}

##############################################################################################
###Process width values. 
Eff_widths = as.data.frame(Eff_widths)
Eff_widths$system.index = Eff_widths$`system:index`
Eff_widths$system.index = gsub("^[[:digit:]]_", "", Eff_widths$system.index)
Eff_widths$system.index = substr(Eff_widths$system.index, 0, 22)
d_dates = str_sub(Eff_widths$system.index, start = -8) ##was -8
d_date = as.Date(d_dates, format = "%Y%m%d")
Eff_widths$Date = d_date
#Filter out flags
Eff_widths = Eff_widths[Eff_widths$Difference==0,]
#Filter out NA DATES to figure out what the error is. 
Dates_error = Eff_widths[is.na(Eff_widths$Date),]
Eff_widths = Eff_widths[!is.na(Eff_widths$Date),]
Dates_error$system.index = Dates_error$`system:index`
Dates_error$system.index = gsub("^[[:digit:]]_", "", Dates_error$system.index)
Dates_error$system.index = substr(Dates_error$system.index, 0, 25)
d_dates = str_sub(Dates_error$system.index, start = -8) ##was -8
d_date = as.Date(d_dates, format = "%Y%m%d")
Dates_error$Date = d_date
binded = rbind(Eff_widths, Dates_error)
bind_na = binded[is.na(binded$Date),]
for(i in 1:nrow(bind_na)){
  if(stringr::str_detect(bind_na$`system:index`[i], "^[[:digit:]]_[[:digit:]]_", negate = TRUE)){
    bind_na$`system:index`[i] = paste("1_", bind_na$`system:index`)
  }
}
d_dates = str_sub(bind_na$id, start = -10) ##was -8
d_dates = str_sub(d_dates, end = 8)
d_date = as.Date(d_dates, format = "%Y%m%d")
bind_na$Date = d_date
Eff_total = rbind(Eff_widths, bind_na)
Eff_widths = Eff_total
all(!is.na(Eff_widths$Date))
test = Eff_widths[Eff_widths$Date> as.Date("2014-12-31"),]
data = distinct(test, ID, .keep_all = TRUE)
tab = data
Lat_lon_df = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\NA_quantiles_combined.csv")
Lat_lon_df = read.dbf("E:\\research\\GRWL\\Subset_GRWL_1spc\\NA_100m_min\\na_sj_using_R_min_100.dbf")
Lat_lon_df = as.data.frame(Lat_lon_df)
data$GRWL_width_m = Lat_lon_df$dbf.width_m[match(data$ID, Lat_lon_df$dbf.ID_2)]
data$lon_dd = Lat_lon_df$dbf.lon_dd[match(data$ID, Lat_lon_df$dbf.ID_2)]
data$lat_dd= Lat_lon_df$dbf.lat_dd[match(data$ID, Lat_lon_df$dbf.ID_2)]
tab = data
rm(Lat_lon_df)
####################################################################################################################
###Read in gage locations. 
gageinfo = gageinfo[gageinfo$GRWL_width>99,]
gageinfo = gageinfo[gageinfo$lakeFlag==0,]
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
#determine closest xsections to each gage
Site_number_xsections=cbind(gageinfo$SITE_NUM, closestDF)
Site_number_distances=cbind(gageinfo$SITE_NUM, distanceDF)
#filter out gages with no xsections within 500m. 
gage_filter = Site_number_distances[,(nGRWL+1)] < 500
Site_number_xsections = Site_number_xsections[gage_filter,]
######################################################################################################################
###Set up validation data. 


#
Gages_char = as.character(Site_number_xsections$`gageinfo$SITE_NUM`)
Gages_char_p = paste("/0", Gages_char, sep="")

for(i in 1:length(Gages_char_p)){
  if(nchar(Gages_char_p[i]) > 9){
    Gages_char_p[i] = sub("^/0", "", Gages_char_p[i])
    Gages_char_p[i] = paste("/", Gages_char_p[i], sep = "")
  }}
p = paste(Gages_char_p, ".csv", sep="")

if(gauging == "USGS"){
wd_q ="E:/research/GRWL/GRWL_2015_present/width_val/input/gaugeData/USGS/dailyQ/"
usgs_q_list = paste(wd_q, p, sep="")
Gages_char = paste("0", Gages_char, sep = "")
files = list.files(wd_q)
gauges = gsub(".csv", "",files)
#usgs_q_list = usgs_q_list[which(gauges%in%Gages_char)]
#Site_number_xsections = Site_number_xsections[which(gauges%in%Gages_char),]
} else{
wd_q = "E:/research/GRDC/Canada_2016_20/"
Gages_char = as.character(Site_number_xsections$`gageinfo$SITE_NUM`)
usgs_q_list = paste(wd_q, Gages_char, "_Q_Day.Cmd", ".txt", sep="")
files = list.files(wd_q)
gauges = gsub("_Q_Day.Cmd.txt", "",files)
#usgs_q_list = usgs_q_list[which(gauges%in%Gages_char)]
#Site_number_xsections = Site_number_xsections[which(gauges%in%Gages_char),]
}

filtering = Gages_char%in%gauges

usgs_q_processing = function(usgs_q){
  q_v = as.vector(usgs_q[,4])
  q_c = as.character(usgs_q[4])
  q_n = as.numeric(q_v)
  q= q_n *0.02832
  usgs_q = cbind(usgs_q, q)
  as.character(usgs_q$datetime)
  return(usgs_q)
}
##################################################################################################
###Functions. 
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

#################################################################################################
start = as.Date("1979-01-01")
data_val = Eff_widths
RC_End = as.Date("2014-12-31") ###WAs 2014
RC_year = format(RC_End, "%Y")
RC_year_1 = format(RC_End+1, "%Y")
data_val$ID = data_val$ID
data_val$ID_2 = data_val$ID
data_val$calc_mean = data_val$Effective_width
data_val_8415 = data_val[data_val$Date< (RC_End +1),] ##Changed from 2015-01-01
data_val_1520 = data_val[data_val$Date> RC_End,] ##Changed from 2014-12-31
data_val = data_val_1520
data_val$Date = as.character(data_val$Date)
tab$ID = tab$id
tab$ID_2 = tab$id
tab$width_m = data_val$width_m[match(tab$ID, data_val$ID)]
#tab$change= tab$median
#tab$change[mapply(is.na, tab$change)] <- 0
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
gage_stats = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 22))
gage_stats_GRADES = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 14))
colnames(gage_stats_GRADES)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "p_val","Bias", "RRMSE", "avg_std", "change", 'RRMSE_median', "std_Q", "STDE")
as.data.frame(gage_stats_GRADES)
l_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
Mean_grades = as.vector(nrow(Site_number_xsections))
u_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
sd_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
sd_vals_1 = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
width_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
gage_quants_q = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 100))
gage_quants_w = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 100))
colnames(gage_stats)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "p_val","Bias", "RRMSE","avg_std", "change", "RRMSE_median", "std_Q","STDE", "KGE", "NSE", "rBias",
                        "SDRR", "MRR", "NRMSE", "Q_50", "W_50")
as.data.frame(gage_stats)
gage_stats_col1 = as.vector(1)
gage_stats_col2 = as.vector(1)
gage_stats_GRADES_col1 = as.vector(1)
gage_stats_GRADES_col2 = as.vector(1)
paired_df_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
rmse = 1
Gauge_comid = as.data.table(Gauge_comid)
setkey(Gauge_comid, COMID)
width_grouping = 30
percentiles = c(0.05, 0.95)

################################################################################################################
###processing each gauge location. 


cl <- makePSOCKcluster(5)
registerDoParallel(cl)
for(i in 1:nrow(Site_number_xsections)){
  ###Filter to widths near gauge location.   
  print(i)
  xSecIDcol = Site_number_xsections[i,]
  xSecID=xSecIDcol[2:ncol(xSecIDcol)]
  mInd = match(xSecID, data$ID) #changed from tab
  xSecw = as.data.frame(seq(1:100))
  xSecq = as.data.frame(seq(1:100))
  mInd_paired = which(data_val_8415$ID %in% xSecID)
  paired_df = data_val_8415[mInd_paired,]
  
  ###Pair largest GRADES flowline with same day Landsat widths. 
  ################################################################################################    
  if(nrow(paired_df)>1){
    pdf_comid = grep("COMID", names(paired_df))
    pdf_filtering = unique(paired_df[,pdf_comid])
    list = na.omit(unlist(pdf_filtering))
    testing_df = as.vector(as.numeric(length(list)))
    for(l in 1:length(list)){
      paired_df$COMID = list[l]
      Gauge_comid_filt = Gauge_comid[.(paired_df$COMID)]
      joining = left_join(paired_df, Gauge_comid_filt, by = c("COMID" = "COMID", "Date" = "date"))
      testing_df[l] = mean(joining$Q, na.rm = TRUE)
    }
    place = as.data.frame(cbind(testing_df, list))
    if(all(is.na(place$testing_df))){next} else{
      placement = place$list[place$testing_df==max(place$testing_df, na.rm = TRUE)]
      paired_df$COMID = placement[1]
    }
    Gauge_comid_filter1  = Gauge_comid[.(paired_df$COMID)]
    paired_df = left_join(paired_df, Gauge_comid_filter1, by = c("COMID" = "COMID", "Date" = "date"))
    paired_df = distinct(paired_df, Date, .keep_all = TRUE)
    if(all(is.na(paired_df$Q))){next}
    #########################################################################################################    
    p_q = quantile(paired_df$Q, probs = seq(percentiles[1],percentiles[2],.01), na.rm = TRUE)
    p_w = quantile(paired_df$calc_mean, probs = seq(percentiles[1],percentiles[2],.01), na.rm = TRUE)
    
    l_q = length(p_q)
    l_w = length(p_w)
    
    gage_quants_q[i,1:l_q] = p_q
    gage_quants_w[i,1:l_w] = p_w
    
    
    ###Make quantile breaks at 0.05 percentiles. 
    Natural_breaks = quantile(paired_df$calc_mean, probs = seq(percentiles[1], percentiles[2], .05, na.rm = TRUE))
    min_df = as.vector(length(Natural_breaks))
    max_df = as.vector(length(Natural_breaks))
    
    if(min_df>2){
      
      
      ###Calculate standard deviation of GRADES Q within each width bin and use this to estimate error. I divide by half so that the entire error bar is 1SD. 
      for(r in 1:length(Natural_breaks)){
        min_df[r] = sd(paired_df$Q[paired_df$calc_mean>Natural_breaks[r] & paired_df$calc_mean<Natural_breaks[r+1]], na.rm = TRUE)/2
        max_df[r] = sd(paired_df$Q[paired_df$calc_mean>Natural_breaks[r] & paired_df$calc_mean<Natural_breaks[r+1]], na.rm = TRUE)/2
        
        
      }
      natural_breaks_df = as.data.frame(Natural_breaks)
      natural_breaks_df$min = min_df
      natural_breaks_df$max = max_df
      
      if(all(is.na(natural_breaks_df$min))){next}else{
      ###Create function to assign standard deviation value for Landsat widths >2014. 
      t.apr_mx = approxfun(natural_breaks_df$Natural_breaks, natural_breaks_df$max, method = "constant", rule = 2, f = 1,ties = max)
      t.apr_mn = approxfun(natural_breaks_df$Natural_breaks, natural_breaks_df$min, method = "constant", rule = 2, f = 1,ties = max)
      }
      
      
      
      ###Determine widths post 2014 and aggregate to same day in case of overlapping observations. 
      data_val_subset = subset(data_val, xSecID[,1] ==data_val$ID)
      data_val_sub_agg = aggregate(data_val_subset$calc_mean, by=list(data_val_subset$Date), FUN=max)
      #if(!is.na(rangedf_1) & !is.error(data_val_sub_agg)){
        ## Set up values for approx/splinefun. 
        w_rc= p_w
        q_rc = p_q
        y = q_rc
        x = w_rc
        
        ###Create function to estimate Q from a Landsat width based on quantile pair up. 
        spl = approxfun(x, y) #, method = "hyman")) #####either usse 'approxfun' or use splinefun with method = "hyman"
        
        ###Read in either USGS or GRDC gauges and process them. 
        if(gauging == "USGS"){
          usgs_q = try(usgs_q_processing(read.csv(usgs_q_list[i], stringsAsFactors = FALSE))) ##Usgs Make a switch for USGS vs Canadian. 
        }else{
          
          ################################################################################GRDC
          usgs_q = try(read.table(usgs_q_list[i], stringsAsFactors = FALSE)) ##GRDC
          usgs_q$V1 = substr(usgs_q$V1, 0, 10)
          usgs_q$datetime = usgs_q$V1
          usgs_q$q = as.numeric(usgs_q$V2)}
        ####################################################################################  
        if(is.error(usgs_q)){next}else{
        ###Pair same day gauge Q with our estimated Q.         
        #if(!is.error(data_val_sub_agg) & !is.error(usgs_q)){
          usgs_q_ind=which(usgs_q$datetime %in% data_val_sub_agg$Group.1)}
          usgs_q_subset= usgs_q[usgs_q_ind,]
          usgs_q_subset[order(usgs_q_subset$datetime),]
          data_val_sub_agg_ind=which(data_val_sub_agg$Group.1 %in% usgs_q_subset$datetime)
          data_val_sub_agg_1 = data_val_sub_agg[data_val_sub_agg_ind,]
          data_val_sub_agg_1[order(data_val_sub_agg_1$Group.1),]
          u_1=cbind(usgs_q_subset$q, data_val_sub_agg_1$x)
          u = as.data.frame(u_1)
          dv = as.vector(nrow(data_val_sub_agg_1))
          dv = spl(data_val_sub_agg_1$x)
          l = as.data.frame(cbind(dv, data_val_sub_agg_1$x))
          u_1 = which(u$w_m %in% l$V2)
          u_2 = u[u_1, ]
          
          if(length(na.omit(l$dv))>2){
          
          
          ###Calculate statistics and prepare labels.       
          linearmodel = lm(u$V1 ~ l$dv)
          r_2 = summary(linearmodel)$r.squared
          bb = bquote(R^2 ==.(format(round(r_2, 2))))
          
          ###calculate and add pearsons correlation (R) to plot. 
          pearson = cor.test(u$V1, l$dv, method = "pearson")
          } else{next}
          r = pearson$estimate
          rlabel = bquote(italic(R) == .(format(r, digits = 3)))
          #if(!is.error(r)){
            ###Plot the rating curve (black line) and GRADES Q (red circles), width bins (gray vertical lines), and estimated Landsat Q (blue circles).             
            par(mfrow =c(2, 2), mar = c(4, 4, 5, 4))
            layout(matrix(c(1,1,1,1,1,0,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3), 2, 11, byrow = TRUE))
            plot(c(min(x, na.rm = TRUE), max(x, na.rm = TRUE)),c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)), col = "white", 
                 main=paste("USGS Gage:", Site_number_xsections$`gageinfo$SITE_NUM`[i]),
                 log = "", type="p", ylab="Discharge (cms)",
                 xlab="W (m)",)
            
            points(paired_df$calc_mean, paired_df$Q, col = "red")
            abline(v = Natural_breaks, lwd = 0.25, col = "lightgray")
            lines(x ,spl(x), col = "black", lwd = 2)
            points(data_val_sub_agg_1$x,dv,pch = 1, col = "blue")
            
            modelQ = spl(paired_df$calc_mean)
            difference = paired_df$Q - modelQ
            sd_vals_1[i,1:length(difference)] = difference
            
            Mean_grades[i] = mean(paired_df$Q, na.rm = TRUE)
            
            
            
            ###Create a dataframe of Landsat width and the SD from GRADES within that width            
            landsat_df = as.data.frame(data_val_sub_agg_1$x)
            landsat_df$landsat_mn = t.apr_mn(landsat_df[,1])
            landsat_df$landsat_mx = t.apr_mx(landsat_df[,1])
            ###Replace the width value with the estimated Q from Landsat.             
            landsat_df[,1] = dv
            ###Subtract the SD from GRADES within a width from the estimated Q to estimate uncertainty.             
            landsat_df$landsat_mn = dv - landsat_df$landsat_mn
            landsat_df$landsat_mx = dv + landsat_df$landsat_mx
            
            
            ###Determine minimum values for each observation to convert estimated errors that extend to negative numbers into 0.             
            landsat_df$minimum = apply(landsat_df,1, FUN = min, na.rm = TRUE)
            
            
            landsat_df$minimum[landsat_df$minimum<0]=0
            
            ###Determine the Maximum value which is the estimated Q + GRADES SD and plot error bars.             
            landsat_df$maximum = apply(landsat_df,1, FUN = max, na.rm = TRUE)
            
            arrows(y0 = landsat_df$minimum, x0= data_val_sub_agg_1$x, 
                   y1 = landsat_df$maximum, x1 = data_val_sub_agg_1$x, col = "indianred" , 
                   code=3, angle = 180, length = 0, lwd = 0.5)
            ###Create a dataframe for error estimates on future plots based on date.             
            landsat_df = cbind(data_val_sub_agg_1$Group.1, landsat_df)
            
            ###Add in legend.             
            location = auto.legend.pos(na.omit(x), na.omit(y))
            legend(location,
                   legend = c(paste("Rating curve (1984-", RC_year, ")", sep = ""),paste("Landsat widths (", RC_year_1, "-2020)", sep = ""),
                              "GRADES"), 
                   col = c("black", "blue",
                           "red"),
                   lty=c(1, NA, NA),
                   lwd = c(2, NA, NA),
                   pch = c(NA, 01, 01), 
                   bty = "n", 
                   text.col = "black", 
                   horiz = FALSE, cex = 0.6)
            
            ###Plot in situ Q vs Landsat Q.             
            u_l_mn = min(c(u$V1, l$dv), na.rm = TRUE)
            u_l_mx = max(c(u$V1, l$dv), na.rm = TRUE)
            plot(c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)),c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)), type = "n", xlab = "In situ Discharge (cms)", ylab = "Landsat Discharge (cms)", main = "In situ vs Landsat")
            points(u$V1, l$dv, col = "blue")
            abline(0,1)
            ###Calculate various statistics and add them to the gage_stats dataframe.             
            r_2_label = paste("RMSE = ", round(rmse), "cms")
            mtext(side = 1, line = 6, adj = 1, text = bb, cex = 0.75)
            gage_stats_col2 = r_2
            gage_stats$R_2[i] = gage_stats_col2
            gage_stats_col1=Site_number_xsections[i,1]
            gage_stats$Site_number[i] = gage_stats_col1
            gage_stats_col7=length(na.omit(dv))
            gage_stats$n_Landsat_obs[i] = gage_stats_col7
            gage_stats_col3 = r
            gage_stats$R[i] = gage_stats_col3
            #gage_stats_col9 = w_sd_avg
            #gage_stats$avg_std[i] = gage_stats_col9
            gage_stats$change[i] = paired_df$change[1]
            gage_stats$Q_50[i] = p_q[50]
            gage_stats$W_50[i] = p_w[50]
          #} else{next}
          
          ###calculate and add spearmans coefficient (p) to plot. 
          spearman = cor.test(u$V1, l$dv, method = "spearman")
          p_val = spearman$p.value
          plabel = bquote(italic(p) == .(format(p_val, digits = 3)))
          #if(!is.error(p_val)){
            gage_stats_col4 = p_val
            gage_stats$p_val[i] = gage_stats_col4#} else{next}
          
          ###calculate and add rmse to plot. 
          error = u[,1] - l[,1]
          rmse = sqrt(mean(error^2, na.rm = TRUE))
          rmse_label = paste("RMSE = ", round(rmse), "cms")
          
          
          RRMSE_value = RRMSE(l$dv, u$V1)
          gage_stats$KGE[i] = KGE(l$dv, u$V1)
          gage_stats$NSE[i] = NSE(l$dv, u$V1)
          gage_stats$rBias[i] = rBias(l$dv, u$V1)
          
          gage_stats$SDRR[i] = SDRR(l$dv, u$V1)
          gage_stats$MRR[i] = MRR(l$dv, u$V1)
          gage_stats$NRMSE[i] = NRMSE(l$dv, u$V1)
          
          
          
          rrmse_label = paste("RRMSE = ", round(RRMSE_value), "%", sep = "")
          l_vals[i,1:nrow(l)] = l[,1]
          u_vals[i,1:nrow(u)] = u[,1]
          #if(!is.error(rmse)){
            mtext(side = 1, line = 6, adj = 0, text = rmse_label, cex = 0.75)
            mtext(side = 1, line = 4.5, adj = 1, text = rrmse_label, cex = 0.75)
            gage_stats_col5 = rmse
            gage_stats$RMSE[i] = gage_stats_col5
            gage_stats$RRMSE[i] = RRMSE_value
          #} else{next}
          
          ###calculate and add bias to plot
          error = l[,1] - u[,1]
          bias = mean(error, na.rm = TRUE)
          bias_label = paste("Bias =", round(bias), "cms")
          #if(!is.error(bias)){
            mtext(side = 1, line = 4.5, adj = 0, text = bias_label, cex = 0.75)
            gage_stats_col6 = bias
            gage_stats$Bias[i] = gage_stats_col6
            gage_stats$GRWL_width_m[i] = data$GRWL_width_m[mInd]
          #} else{next}
          
          ###Add in error bars for the in situ vs Landsat Q comparison plot.           
          arrows(x0 = u$V1, y0=  landsat_df$minimum, 
                 x1 = u$V1, y1 = landsat_df$maximum, col = "indianred",
                 code=3, angle = 180, length = 0, lwd = 0.5)
          
          ###add in hydrograph
          #if(nrow(usgs_q_subset) >0){
          ##add in date field to usgs data and filter to 2015- present.  
          usgs_q$date = as.Date(usgs_q$datetime, "%Y-%m-%d")
          usgs_q_2015 = usgs_q %>%
            filter(usgs_q$date > (RC_End+1) & usgs_q$date < as.Date('2020-12-31'))
          ##add in date field to validation data and assign to corresponding usgs days.
          data_val_sub_agg_1$date = as.Date(data_val_sub_agg_1$Group.1, "%Y-%m-%d")
          usgs_q_2015_wval = left_join(usgs_q_2015, data_val_sub_agg_1, copy = FALSE)
          
          
          
          ###Plot the hydrograph and add Landsat estimate Q. 
          plot(usgs_q_2015_wval$date, usgs_q_2015_wval$q,ylim = c(min(spl(x), na.rm = TRUE),
                                                                  max(spl(x), na.rm = TRUE)), xlab = "", ylab = "Discharge (cms)", type = "l", col = "lightgray")
          landsat_df$Date = as.Date(landsat_df$`data_val_sub_agg_1$Group.1`)
          points(landsat_df$Date, landsat_df$`data_val_sub_agg_1$x`)
          
          
          
          
          ###Add in error bars for the hydrograph Q estimates.             
          arrows(x0 = landsat_df$Date, y0 =  landsat_df$minimum, 
                 x1 = landsat_df$Date, y1 = landsat_df$maximum, col = "indianred",
                 code=3, angle = 180, length = 0, lwd = 0.5)
          ########################################################################
          
          ###Calculate various statistics. The first is the length of the error bars which will be used for exporting. 
          landsat_diff = landsat_df$maximum - landsat_df$minimum
          gage_stats_col13 = mean(landsat_diff)
          gage_stats_col13 = mean(usgs_q_2015_wval$sd, na.rm = TRUE)
          gage_stats$std_Q[i] = gage_stats_col13
          gage_stats_col14 = sqrt(var(error, na.rm = TRUE))
          gage_stats$STDE[i] = gage_stats_col14
          ###Determine error bar lengths and add them to the sd_vals dataframe for further analysis. 
          sd_vals[i,1:length(landsat_diff)] = landsat_diff
          ###Add title and legend. 
          title(paste("Hydrograph ", RC_year_1, "-2020", sep = ""), line = 0.5)
          legend("bottom", inset = c(0, -.2), legend = c("Rating curve", "In situ"),
                 col = c("blue", "lightgray"),lty = c(NA, 1), pch = c(1, NA), bty = "n", xpd = TRUE, horiz = TRUE)
          } else{next} ### this one for min df. 

  } else{next} ### this one for min df. 
  
}
gage_stats_can = gage_stats
u_vals_can = u_vals
l_vals_can = l_vals
sd_vals = sd_vals_1
sd_vals_can = sd_vals
grades_can = Mean_grades

grades_usa = Mean_grades
sd_vals = sd_vals_1
sd_vals = bind_rows(sd_vals, sd_vals_can)
u_vals = bind_rows(u_vals, u_vals_can)
l_vals = bind_rows(l_vals, l_vals_can)
gage_stats = bind_rows(gage_stats, gage_stats_can)

grades = c(grades_usa, grades_Can)

#########################################################################################################################################################
###Determine dam locations from USGS. 
gage_regulation = read.csv("C:\\Users\\rriggs\\Downloads\\gage_regulation_new.csv")
gage_usgs = grep("^USGS", as.character(gage_regulation$stationid))
gage_regulation_usgs = gage_regulation[gage_usgs,]
##modify the station id so that it can match with gageinfo file. 
gage_reg_usgs_updated = gsub("^USGS", "", gage_regulation_usgs$stationid)
gage_reg_usgs_updated_1 = gsub("^_", "", gage_reg_usgs_updated)
gage_reg_usgs_updated_0 = gsub("^0", "", gage_reg_usgs_updated_1)
gage_regulation_usgs$stationid = gage_reg_usgs_updated_0
gage_stats$dams = gage_regulation_usgs$distance[match(gage_stats$Site_number, gage_regulation_usgs$stationid)]

visual = read.csv("E:\\research\\GRWL\\GRWL_2015_present\\East_west_together\\Surface_reflectance\\Stats\\Gage_stats_visual_dams.csv")
gage_stats$visual = visual$visual_flags[match(gage_stats$Site_number, visual$Site_number)]

###Determine dam locations from GRDC. 
gage_regulation = read.csv("C:\\Users\\rriggs\\Downloads\\gage_regulation_new.csv")
gage_GRDC = grep("^GRDC", as.character(gage_regulation$stationid))
gage_regulation_GRDC = gage_regulation[gage_GRDC,]

##modify the station id so that it can match with gageinfo file. 
gage_reg_grdc_updated = gsub("^GRDC", "", gage_regulation_GRDC$stationid)
gage_reg_grdc_updated_1 = gsub("^_", "", gage_reg_grdc_updated)

#gage_reg_usgs_updated_0 = gsub("^0", "", gage_reg_usgs_updated_1)
gage_regulation_GRDC$stationid = gage_reg_grdc_updated_1
gage_stats$Candams = gage_regulation_GRDC$distance[match(gage_stats$Site_number, gage_regulation_GRDC$stationid)]
##################################################################################################################################################################################################################################
###Apply various filters to determine median and filter for future error plots. 
filtering = !is.na(gage_stats$Site_number)# & gage_stats$n_Landsat_obs>9# & is.na(gage_stats$dams) & is.na(gage_stats$visual) & is.na(gage_stats$Candams)# gage_stats$n_Landsat_obs >9# & is.na(gage_stats$dams)
nrow(gage_stats[filtering,])
apply(gage_stats[filtering,], 2, median, na.rm = TRUE)

test = gage_stats[filtering,]

gage_stats_all = gage_stats
gage_stats_index = gage_stats_all==filtering

gage_stats = gage_stats[filtering, ]

l_vals = l_vals[as.numeric(rownames(gage_stats)),]
u_vals = u_vals[as.numeric(rownames(gage_stats)),]
sd_vals = sd_vals[as.numeric(rownames(gage_stats)),]
grades = grades[as.numeric(rownames(gage_stats))]



