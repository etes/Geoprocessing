'''
Created on 5. sep. 2012

@author: ermias
'''

import arcpy

arcpy.env.workspace = 'C:/Data/Projects/IsAnalyse/Isdata/barents_shp'

fcs = arcpy.ListFeatureClasses()

# Add ice concentration field
# Create Ice type Dictionary
# Process Ice type in dictionary
for fc in fcs:
    arcpy.AddField_management(fc,'IceConc',"SHORT")
    arcpy.CalculateField_management(fc, "IceConc", "iceConc(!ICE_TYPE!)", "PYTHON",
                                    "def iceConc(iceType):\
                                    \n    iceDict = {\
                                    'Fast Ice':100,\
                                    'Very Close Drift Ice':95,\
                                    'Close Drift Ice':80,\
                                    'Open Drift Ice':55,\
                                    'Very Open Drift Ice':25,\
                                    'Open Water':5}\
                                    \n    return iceDict.get(iceType)")

print "Ice Concentration added successfully."
