import ee
#ee.Authenticate()
ee.Initialize()

x = 'users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_waterClassification_Jones2019.py'
fls = 'users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.py'
flsh = 'users/eeProject/RivWidthCloudPaper:rwc_landsat.py'
fnsLandsat = 'users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.py'
lsFun = 'users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.py'
riverFun = 'users/eeProject/RivWidthCloudPaper:functions_river.py'
grwl_cline = ee.FeatureCollection('users/eeProject/GRWL_summaryStats')
#fc_3spc = ee.FeatureCollection("users/rriggs/east_validation_1spc_3x/eastern_merged")
fc_3spc = ee.FeatureCollection("users/rriggs/SA_sj_min_100") ##South America. 
fc_3spc = ee.FeatureCollection("users/rriggs/na_sj_using_R_min_100")
#gauges = ee.FeatureCollection("users/rriggs/Gauge_points") ##USGS Gauges 
#gauges = ee.FeatureCollection("users/rriggs/Canadian_gauges_100m")
#gauges = ee.FeatureCollection("users/rriggs/South_AM_100m_stationid")
#fc_3spc = ee.FeatureCollection("users/rriggs/Africa_min_100min_3x_1spc")
#gauges = ee.FeatureCollection("users/rriggs/LwrMiss_points")
#gauges = ee.FeatureCollection("users/rriggs/MissBasinPoints")
gauges = ee.FeatureCollection("users/rriggs/NApoints")



jrcSummary = ee.Image("JRC/GSW1_0/GlobalSurfaceWater")
occ = jrcSummary.select('occurrence')
jrcMeta = ee.Image("JRC/GSW1_0/Metadata")
nobs = jrcMeta.select('valid_obs')
change = jrcSummary.select('change_abs')
change = change.abs()
waterMax = occ.gte(0)
grwl = ee.FeatureCollection("users/eeProject/grwl")
# create image with bands ranging from quantile 0-100
quantileBreaks = ee.List.sequence(0, 100, 1)
#import folium
def creatQuantileImage(l, prev):
    return(ee.Image(prev).addBands(occ.gte(ee.Image.constant(l)).rename([ee.String('q').cat(ee.Number(l).format('%03d'))])))


# rename the flags so that their value won't be replaced in the next step
# when calculating the mean river mask values
def appendStringFlag(l):
    return(ee.String(l).cat('_flag'))

def Mndwi(image):
    return(image.normalizedDifference(['Green', 'Swir1']).rename('mndwi'))

def Mbsrv(image):
    return(image.select(['Green']).add(image.select(['Red'])).rename('mbsrv'))

def Mbsrn(image):
    return(image.select(['Nir']).add(image.select(['Swir1'])).rename('mbsrn'))

def Ndvi(image):
    return(image.normalizedDifference(['Nir', 'Red']).rename('ndvi'))

def Awesh(image):
    return(image.expression('Blue + 2.5 * Green + (-1.5) * mbsrn + (-0.25) * Swir2', {
    'Blue': image.select(['Blue']),
    'Green': image.select(['Green']),
    'mbsrn': Mbsrn(image).select(['mbsrn']),
    'Swir2': image.select(['Swir2'])}))

def Dswe(i):
    mndwi = Mndwi(i)
    mbsrv = Mbsrv(i)
    mbsrn = Mbsrn(i)
    awesh = Awesh(i)
    swir1 = i.select(['Swir1'])
    nir = i.select(['Nir'])
    ndvi = Ndvi(i)
    blue = i.select(['Blue'])
    swir2 = i.select(['Swir2'])

    t1 = mndwi.gt(0.124)
    t2 = mbsrv.gt(mbsrn)
    t3 = awesh.gt(0)
    t4 = (mndwi.gt(-0.44)
    .And(swir1.lt(900))
    .And(nir.lt(1500))
    .And(ndvi.lt(0.7)))
    t5 = (mndwi.gt(-0.5)
    .And(blue.lt(1000))
    .And(swir1.lt(3000))
    .And(swir2.lt(1000))
    .And(nir.lt(2500)))

    t = t1.add(t2.multiply(10)).add(t3.multiply(100)).add(t4.multiply(1000)).add(t5.multiply(10000))

    noWater = (t.eq(0)
    .Or(t.eq(1))
    .Or(t.eq(10))
    .Or(t.eq(100))
    .Or(t.eq(1000)))
    hWater = (t.eq(1111)
    .Or(t.eq(10111))
    .Or(t.eq(11011))
    .Or(t.eq(11101))
    .Or(t.eq(11110))
    .Or(t.eq(11111)))
    mWater = (t.eq(111)
    .Or(t.eq(1011))
    .Or(t.eq(1101))
    .Or(t.eq(1110))
    .Or(t.eq(10011))
    .Or(t.eq(10101))
    .Or(t.eq(10110))
    .Or(t.eq(11001))
    .Or(t.eq(11010))
    .Or(t.eq(11100)))
    pWetland = t.eq(11000)
    lWater = (t.eq(11)
    .Or(t.eq(101))
    .Or(t.eq(110))
    .Or(t.eq(1001))
    .Or(t.eq(1010))
    .Or(t.eq(1100))
    .Or(t.eq(10000))
    .Or(t.eq(10001))
    .Or(t.eq(10010))
    .Or(t.eq(10100)))

    iDswe = (noWater.multiply(0)
    .add(hWater.multiply(1))
    .add(mWater.multiply(2))
    .add(pWetland.multiply(3))
    .add(lWater.multiply(4)))

    return(iDswe.rename(['dswe']))

def ClassifyWaterJones2019(img):
    dswe = Dswe(img)
    waterMask = dswe.eq(1).Or(dswe.eq(2)).rename(['waterMask'])
    out = waterMask.copyProperties(img)
    return(out.set({'system:time_start':img.get('system:time_start')}))

def switchGeometryLine2Endpoints(f):
        f = f.set({'lineGeometry': f.geometry()})
        l = f.geometry().coordinates()
        g = ee.Geometry.MultiPoint(l, 'EPSG:4326')
        return(f.setGeometry(g))

def switchGeometryEndpoints2Line(f):
        return(f.setGeometry(f.get('lineGeometry')).set('lineGeometry', None))

def GetCenterline(clDataset, bound):
    # // filter the GRWL centerline based on area of interest
    cl = clDataset.filterBounds(bound)
    return(cl)

def ExtractChannel(image):
    # // extract the channel water bodies from the water mask, based on connectivity to the reference centerline.
    connectedToCl = (image.Not().cumulativeCost(
        source = ee.Image().toByte().paint(grwl_cline, 1).And(image), #// only use the centerline that overlaps with the water mask
        maxDistance = 4000,
        geodeticDistance = False).eq(0))

    channel = image.updateMask(connectedToCl).unmask(0).updateMask(image.gte(0)).rename(['channelMask'])
    return(channel)


def RemoveIsland(channel):
    # /* fill in island as water if the size (number of pixels) of the island is smaller than FILL_SIZE */
    fill = channel.Not().selfMask().connectedPixelCount(333).lt(333)
    river = channel.where(fill, ee.Image(1)).rename(['riverMask'])
    return(river)

def ExtractRiver(imgIn, clData, maxDist, minIslandRemoval):
    waterMask = imgIn.select(['waterMask'])
    bound = waterMask.geometry()
    cl = GetCenterline(clData, bound)
    channelMask = ExtractChannel(waterMask, cl, maxDist)
    riverMask = RemoveIsland(channelMask, minIslandRemoval)
    return(imgIn.addBands(channelMask).addBands(riverMask))

def widths(image):
  width = (image.eq(1).reduceRegions(
  collection = filt, 
  reducer= ee.Reducer.mean(),
  ))
  flags = (image.reduceRegions(
  collection= width.map(switchGeometryLine2Endpoints), 
  reducer= ee.Reducer.max(),
))
  return(flags)


def filt_lines (f):
  return f.set('geo_type', f.geometry().type())

def maximum_no_of_tasks(MaxNActive, waitingPeriod):
	"""maintain a maximum number of active tasks
	"""
	import time
	import ee
	ee.Initialize()

	time.sleep(10)
	## initialize submitting jobs
	ts = list(ee.batch.Task.list())

	NActive = 0
	for task in ts:
		if ('RUNNING' in str(task) or 'READY' in str(task)):
			NActive += 1
	## wait if the number of current active tasks reach the maximum number
	## defined in MaxNActive
	while (NActive >= MaxNActive):
		time.sleep(waitingPeriod) # if reach or over maximum no. of active tasks, wait for 2min and check again
		ts = list(ee.batch.Task.list())
		NActive = 0
		for task in ts:
			if ('RUNNING' in str(task) or 'READY' in str(task)):
				NActive += 1
	return()
 
 ##changed to 2000 from 5000
def buffer_zone (f):
  return f.buffer(2000)
def  ftr_coll (f):
  return ee.FeatureCollection(f)

def distance_fun (f):
  l = f.geometry().distance(filter_gauge.geometry())
  d = f.set('distance', l)
  return(d)

def distance_fun_poly (f):
  l = f.geometry().distance(far.geometry())
  d = f.set('distance_1', l)
  return(d)

def Unpack(bitBand, startingBit, bitWidth):
    # unpacking bit bands
    # see: https://groups.google.com/forum/#!starred/google-earth-engine-developers/iSV4LwzIW7A
    return (ee.Image(bitBand)
            .rightShift(startingBit)
            .bitwiseAnd(ee.Number(2).pow(ee.Number(bitWidth)).subtract(ee.Number(1)).int()))
def UnpackAllSR(bitBand):
    # apply Unpack function for multiple pixel qualities
    bitInfoSR = {
    'Cloud': [5, 1],
    'CloudShadow': [3, 1],
    'SnowIce': [4, 1],
    'Water': [2, 1]
    }
    unpackedImage = ee.Image.cat([Unpack(bitBand, bitInfoSR[key][0], bitInfoSR[key][1]).rename([key]) for key in bitInfoSR])
    return unpackedImage
def AddFmaskSR(image):
    # // add fmask as a separate band to the input image
    temp = UnpackAllSR(image.select(['BQA']))

    fmask = (temp.select(['Water']).rename(['fmask'])
    .where(temp.select(['SnowIce']), ee.Image(3))
    .where(temp.select(['CloudShadow']), ee.Image(2))
    .where(temp.select(['Cloud']), ee.Image(4))
    .mask(temp.select(['Cloud']).gte(0)))

    return image.addBands(fmask)

riverMask = ExtractChannel(waterMax)
quantileImage = ee.Image(quantileBreaks.iterate(creatQuantileImage, ee.Image())).select('^q.*').updateMask(riverMask).unmask(0)
bn = ee.List(quantileImage.bandNames()).map(appendStringFlag)
reducer = ee.Reducer.anyNonZero().forEach(bn)

collection = fc_3spc.map(filt_lines)
fc_3spc = collection.filter(ee.Filter.eq('geo_type', 'LineString'))

#fc_3spc = fc_3spc.filterBounds(rc_roi)
LT5_BANDS = ['B1',   'B2',    'B3',  'B4',  'B5',    'B7',    'B6', 'pixel_qa'];
LE7_BANDS = ['B1',   'B2',    'B3',  'B4',  'B5',    'B7',    'B6', 'pixel_qa'];
LC8_BANDS = ['B2',   'B3',    'B4',  'B5',  'B6',    'B7',    'B10', 'pixel_qa'];
STD_NAMES = ['Blue', 'Green', 'Red', 'Nir', 'Swir1', 'Swir2', 'Temp', 'BQA'];

#// load Landsat 5,7,8 collections:
#// TODO(GHA): combine 5, 7, and 8 collections:
LT5 = ee.ImageCollection('LANDSAT/LT5_L1T_SR').select(LT5_BANDS, STD_NAMES);
LT5 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR').select(LT5_BANDS, STD_NAMES); 
LE7 = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR').select(LE7_BANDS, STD_NAMES).filterDate('1999-01-01', '2003-05-30')
##.select(LE7_BANDS, STD_NAMES);
LC8 = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR').select(LC8_BANDS, STD_NAMES);

collection = LC8.merge(LT5).merge(LE7)


p = list(range(3266, 20499,1))

distFilter = ee.Filter.contains(
  leftField ='.geo',
  rightField = '.geo'
  )

ee.Filter.contains()

distSaveAll = ee.Join.saveAll(
    matchesKey ='points',
    measureKey = 'distance');


  
def ij (state):
  nPowerPlants = ee.List(state.get('points'))
  mean = ee.FeatureCollection(nPowerPlants).aggregate_mean('width_m')
  return ee.FeatureCollection(state.set({'width_m': mean}))


def connected (f):
  con = f.clip(buff1) ####changed this from f.clipToBoundsAndScale(filt) &&& changed from polygon to buff1
  return con

def grwl_filter_fun (f):
  con = f.clip(polygon)
  return con


##Changed f.eq(1) to f.
def effective_width_1 (f):                     
  lng = (f.reduceRegions(
  collection= grwl_cline.filterBounds(filt), 
  reducer= ee.Reducer.count(),
  ))
  return lng 


##Changed collection = fi1.flatten().geometry()  & con.select('channelMask').eq(1) This function is causing the issues. It will not allow for the properties to be set. 
def effective_width_2 (con):
  sum = (con.select('channelMask').eq(1).reduceRegions(
  collection= buff1, ##changed from polygon 
  reducer= ee.Reducer.sum().unweighted(),  ##Unweighted added for flagging technique. 
  ))
  time = con.get('system:time_start')
  return sum.copyProperties(con).set({'system:time_start':time}) #do sum.flatte() if needed. 

##changed from 22
def Area_fun (feature):
  d = ee.Number(feature.get('sum'))
  id = feature.getString('system:index').slice(0,24)
  return feature.set({'Area': ee.Number(d).multiply(900), 'id': id, 'system:time_start':feature.get('system:time_start'), 'length':cline_updated.length()})


##changed from 22
def len_fun (feature):
  d = ee.Number(feature.get('count'))
  id = feature.getString('system:index').slice(0,24)
  return feature.set({'length': cline_updated.length(), 'id':id})

toyFilter = ee.Filter.equals(leftField = 'system:time_start',
                             rightField ='system:time_start');


innerJoin = ee.Join.inner()


def fc_function (f):
  p = ee.Feature(f.get('primary'))
  s = ee.Feature(f.get('secondary'))
  properties = p.copyProperties(s)
  return properties


def effW_fun (feature):
  Area = ee.Number(feature.get('Area'))
  Length = ee.Number(feature.get('length'))
  return feature.set({'Effective_width': Area.divide(Length)})


##'COMID': distinct_comid.first().get('COMID')
def fields (f):
  return f.set({'width_m':intersectJoined.flatten().first().get('width_m'), 'ID': fc_id.first().get('ID_2')})


def set_id (f):
  return f.set({'id': fc_id})


#Changed collection from pts
def flagging (f):
  a = (f.reduceRegions(
      collection = pts.geometry(),
      reducer = ee.Reducer.max(),
  ))
  return a
##changed from 22
def flagged_collection_fun (f):
  id = f.getString('system:index').slice(0,24)
  return f.set({'id': id})



def feature_fun (f):
  a = ee.Geometry.Point(f)
  b = ee.Feature(a, {'id': 1, 'e/o':2})
  return(b)

def id_fun (f):
  a = f.get('system:index')
  b = f.set({'id': a})
  return(b)
def getOddNumbers (f):
  number = ee.Number.parse(f.get('id'))
  remainder = number.mod(2)
  val = number.multiply(remainder)
  c = f.set({'e/o':val})
  return(c)

def farthest_distance_function (f):
  a = ee.Feature(f).distance(ee.Feature(farthest_pt))
  b = f.set('distance', a)
  return(b)

def farthest_distance_function_even (f):
  a = ee.Feature(f).distance(ee.Feature(farthest_even))
  b = f.set('distance', a)
  return(b)

def comid_fun (f):
  #a = 'COMID{}'.format(f)##.slice(0,6)
  a = str(f)
  return a

def difference_fun (f):
  p = ee.Feature(f.get('primary'))
  a = ee.Number(p.get('Area'))
  s = ee.Feature(f.get('secondary'))
  b = ee.Number(s.get('Area'))
  return s.set({'Difference': a.subtract(b)})

def change_fun (f):
  a = median_change.get('change_abs')
  b = f.set({'change':a})
  b = b.set(comid_dict)
  return b



def MaskFunction (f):
  a = f.eq(1)
  b = f.mask(a)
  return b

def cloudFunction (f):
  min = f.select('fmask').reduceRegion(ee.Reducer.max(), cline_updated)
  add = f.set({'cloud':min.get('fmask')})
  return(add)

def eff_width2_function (f):
  ft = ee.FeatureCollection(f).first()
  return ee.Feature(ft).copyProperties(f).set({'system:time_start': f.get('system:time_start')})

##skipping any errors. 
##GRWL_buffer
for i in range(len(p)):
  filter_gauge = gauges.filter(ee.Filter.eq('Sttn_Nm', p[i])) ##Changed from SITE_NUM
  buff1 = filter_gauge.map(buffer_zone)
  fi1 = buff1.map(ftr_coll)
  filt1 = fc_3spc.filterBounds(fi1.flatten().geometry())
  
  fff = filt1.map(distance_fun)
  filt = fff.filter(ee.Filter.lte('distance', 1000))
  fc_id = filt.limit(1, 'distance')
  
  grwl_filt = grwl_cline.filterBounds(filt)
  grwl_filt_geom = grwl_filt.geometry().buffer(grwl_filt.first().get('width_mean'))

  intersect = grwl_filt_geom.intersection(buff1)
  polygon = intersect
  spatialJoined = distSaveAll.apply(fi1.flatten(), filt, distFilter);
  distinct_comid = filt1.filterBounds(polygon).sort('distance').distinct('COMID')

  
  distinct_comid = distinct_comid.reduceColumns(ee.Reducer.toList(), ['COMID'])
  distinct_length = ee.List(distinct_comid.get('list'))
  numb_comid = distinct_length.size()

  try:
    n = numb_comid.getInfo()
  except:
    pass
  else:
    seq = list(range(0, n))
    seq = ['COMID'+str(i) for i in seq]

  #print(seq)
  #comid_seq = seq.map(comid_fun)
    comid_dict = ee.Dictionary.fromLists(seq, distinct_length)


    intersectJoined = spatialJoined.map(ij)
    fi1 = intersectJoined
  
  #filt = fc_3spc.filterBounds(x1)
    filtered = collection.filterDate('1984-01-01', '2021-12-31').sort('system:time_start').filterBounds(filt)#.filterMetadata('CLOUD_COVER', 'less_than', 10)
    #print(filtered.getInfo())
    filt_con = filtered.map(connected)
    cline_updated = grwl_filt.geometry().intersection(polygon)

    flagged = filt_con.map(AddFmaskSR)
    cloud_filter = flagged.map(cloudFunction).filterMetadata('cloud', 'less_than', 2)

    filt_con = cloud_filter
    filtered = filt_con
    waterMask = filtered.map(ClassifyWaterJones2019)
    GetCenterline(grwl_cline, filt)
    riverMask = waterMask.map(ExtractChannel)

    connected_mask = riverMask#.map(connected)
    connected_test = connected_mask.map(grwl_filter_fun)
    

    eff_width1 = connected_test.map(effective_width_1)

    connected_test = connected_test.map(MaskFunction)
    connected_mask = connected_mask.map(MaskFunction)

    eff_width2 = connected_test.map(effective_width_2)
    

    eff_width2_filt = connected_mask.map(effective_width_2)
    #circle = eff_width2_filt.flatten().map(Area_fun)
    
    eff_width2 = eff_width2.map(eff_width2_function)
    eff_width2_filt = eff_width2_filt.map(eff_width2_function)

    area_map = eff_width2.map(Area_fun)
    
    circle = eff_width2_filt.map(Area_fun)

    fc_test = area_map
    difference = innerJoin.apply(circle, fc_test, toyFilter)
    



    diff_t = difference.map(difference_fun)

    fc_test = diff_t


    testing = fc_test.map(effW_fun)
    fields_vals = testing.map(fields)
    selection = fields_vals.select(['Effective_width', 'ID', 'id', 'COMID', 'width_m', 'Difference','system:time_start'])
    sel = selection.filter(ee.Filter.gt('Effective_width', 0))
    sel = sel.filter(ee.Filter.eq('Difference',0))
    selection = sel
  
    median_change = change.reduceRegion(ee.Reducer.median(), polygon)
  
    selection = selection.map(change_fun)

  #selection = selection.distinct('id')

    task = (ee.batch.Export.table.toDrive(
    collection = selection,
    description = 'widths_' + '_' + str(p[i]),
    folder = 'Pub_test',
    fileNamePrefix = 'Gauge_' + '_' + str(p[i]),
    fileFormat = 'csv'
    ))

    task.start()
#print(output.first())
    print('task', i, 'has started')
    maximum_no_of_tasks(3, 60)