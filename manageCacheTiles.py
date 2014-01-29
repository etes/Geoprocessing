'''
Created on 29. jan. 2014

@author: ermias
'''

import arcpy, time

serviceConn = "C:/Users/ermias/AppData/Roaming/ESRI/Desktop10.2/ArcCatalog/arcgis on willem3 (admin)"
serviceFolder = "Basisdata_Intern"
serviceName = "NP_Nordomraadene_WMTS_25833"
serviceType = "MapServer"
service = serviceConn + "/" + serviceFolder + "/" + serviceName + "." + serviceType
scales = "10240000;5120000;2560000;1280000;640000;320000;160000;80000;40000;20000;10000;5000;2500;1250;625"
arcpy.ManageMapServerCacheScales_server(service, scales)
result = arcpy.ManageMapServerCacheTiles_server(service,scales,"RECREATE_EMPTY_TILES","3","","","DO_NOT_WAIT")
while result.status < 4:
    print result.status, result.getMessages()
    time.sleep (0.2)
