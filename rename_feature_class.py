# -*- coding: utf-8 -*-

import arcpy
from arcpy import env

arcpy.env.workspace = r"C:/3Dbymodell/fredrikstad.gdb"
soner = r"C:/3Dbymodell/ToolData/ToolData.gdb/Levekarssoner"
navner = {}
cursor = arcpy.SearchCursor(soner, fields="NAVN, NR", sort_fields="NR A")
for row in cursor:
	navner[row.getValue("NR")] = row.getValue("NAVN").replace(" ", "_")

fcs = arcpy.ListFeatureClasses()
for fc in fcs:
	arcpy.Rename_management(fc, navner[int(fc.strip("B_"))])

