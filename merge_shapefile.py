import sys, os, ogr, osr


outputMergefn = 'merged.shp'
try:
    directory = sys.argv[1]
except:
    print "Please provide directory to the shapefiles"
    sys.exit(1)

fileStartsWith = 'test'
fileEndsWith = '.shp'
driverName = 'ESRI Shapefile'
geometryType = ogr.wkbPolygon

out_driver = ogr.GetDriverByName( driverName )
if os.path.exists(outputMergefn):
    out_driver.DeleteDataSource(outputMergefn)
out_ds = out_driver.CreateDataSource(outputMergefn)
out_layer = out_ds.CreateLayer(outputMergefn, geom_type=geometryType)

fileList = os.listdir(directory)

for file in fileList:
    if file.endswith(fileEndsWith):
        print file
        ds = ogr.Open(directory+file)
        lyr = ds.GetLayer()
        for feat in lyr:
            out_feat = ogr.Feature(out_layer.GetLayerDefn())
            out_feat.SetGeometry(feat.GetGeometryRef().Clone())
            out_layer.CreateFeature(out_feat)
            out_layer.SyncToDisk()

