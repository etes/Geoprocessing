'''
Created on 17. des. 2012

@author: ermias.TESSINFJELLET
'''


import arcpy
from arcpy import env

env.workspace = r"C:\Data\Projects\u37"
env.overwriteOutput = "True"

# Create a pollution file geodatabase if it doesn't exist
try:
    arcpy.CreateFileGDB_management("C:/Data/Projects/u37", "pollution.gdb")
except:
    pass

outWorkspace = "C:/Data/Projects/u37/pollution.gdb/"

fcs = arcpy.ListFeatureClasses()

arcpy.AddMessage(" Executing: subset layer by attribute ...")
# Put in error trapping in case an error occurs when running tool
try:

   for fc in fcs:
    rows = arcpy.SearchCursor(fc)
    fileN = str.split(str(fc), '.')[0]
    for row in rows:
        species = row.Species
        tissue = row.Tissue
        year = row.Year
        expressionS = "\"Species\" = '%s'" %species
        expressionY = '"Year" = %d' %year
        expressionT = "\"Tissue\" = '%s'" %tissue
        arcpy.MakeFeatureLayer_management(fc, "fc_Lyr", expressionS)
        arcpy.SelectLayerByAttribute_management("fc_Lyr", "NEW_SELECTION", expressionY)
        arcpy.SelectLayerByAttribute_management("fc_Lyr", "NEW_SELECTION", expressionT)
        speciesN = ''.join(e for e in species if e.isalnum())
        yearN = str (int(year))
        tissueN = str(tissue)
        arcpy.CopyFeatures_management("fc_Lyr", outWorkspace + fileN + "_" + speciesN + "_" + tissueN + "_" + yearN)

except:
   print arcpy.GetMessages()