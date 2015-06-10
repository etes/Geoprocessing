#-------------------------------------------------------------------------------
# Name:        Ovlese_Oslofjord
# Purpose:
#
# Author:      ermtes.GEOWKS3-38240 2015
#
# Created:     08.06.2015
# Copyright:   (c) ermtes.GEOWKS3-38240 2015
# Licence:     MIT
#-------------------------------------------------------------------------------

import arcpy
import os
from datetime import datetime, timedelta

class Toolbox(object):
    def __init__(self):
        """Ovelse_oslofjord ArcGIS Python Toolbox
        for bruk i Ovelse Oslofjørd 2015."""
        self.label = "Toolbox"
        self.alias = ""

        # List of tool classes associated with this toolbox
        self.tools = [HendelserTool]


class HendelserTool(object):
    def __init__(self):
        """HendelserTool: Converts an asset movement (bevelgelser) line to a set of points track
        calculated from average speed (from departure time and arrival time) of the asset."""
        self.label = "HendelserTool"
        self.description = """HendelserTool: Converts an asset movement (bevelgelser) line to a set of points track
        calculated from average speed (from departure time and arrival time) of the asset.
        Parameters: Input line features, Start Time, End Time, Output path, Output feature class name, Time interval"""
        self.canRunInBackground = False

    def getParameterInfo(self):
        """Define parameter definitions"""
        # First parameter
        param0 = arcpy.Parameter(
          displayName="Settin Linje Lag",
          name="in_features",
          datatype="GPFeatureLayer",
          parameterType="Required",
          direction="Input")

        # Second parameter
        param1 = arcpy.Parameter(
          displayName="Velg Start Tid",
          name="start_field",
          datatype="Field",
          parameterType="Required",
          direction="Input")

        param1.parameterDependencies = [param0.name]

        # Third parameter
        param2 = arcpy.Parameter(
          displayName="Velg Slutt Tid",
          name="end_field",
          datatype="Field",
          parameterType="Required",
          direction="Input")

        param2.parameterDependencies = [param0.name]

        # Fourth parameter
        param3 = arcpy.Parameter(
          displayName="Ut path (FileGeodatabase)",
          name="out_path",
          datatype="Workspace",
          parameterType="Required",
          direction="Input")

        # Fifth parameter
        param4 = arcpy.Parameter(
          displayName="Ny Feature Klassenavn",
          name="out_features",
          datatype="GPString",
          parameterType="Required",
          direction="Input")

        # Sixth parameter
        param5 = arcpy.Parameter(
          displayName="Tid resolusjon (i sekunder: anbefales mer enn 5 sekunder)",
          name="time_interval",
          datatype="GPLong",
          parameterType="Required",
          direction="Input")

        params = [param0, param1, param2, param3, param4, param5]
        return params

    def isLicensed(self):
        """Set whether tool is licensed to execute."""
        return True

    def updateParameters(self, parameters):
        """Modify the values and properties of parameters before internal
        validation is performed.  This method is called whenever a parameter
        has been changed."""
        return

    def updateMessages(self, parameters):
        """Modify the messages created by internal validation for each tool
        parameter.  This method is called after internal validation."""
        return

    def execute(self, parameters, messages):
        """The source code of the tool."""
        in_features = parameters[0].value
        start_time_field = str(parameters[1].value)
        end_time_field = str(parameters[2].value)
        ovelse_gdb = parameters[3].value
        track_features_name = str(parameters[4].value)
        time_interval = int(parameters[5].value)
        fc_name = "temp_" + datetime.now().strftime('%Y%m%d%H%M%S')
        arcpy.AddMessage("Opprettet midlertidig feature klasse {0}\\{1}.".format(ovelse_gdb,fc_name))
        arcpy.FeatureClassToFeatureClass_conversion(in_features=in_features, out_path=ovelse_gdb, out_name=fc_name)
        temp_features = os.path.join(str(ovelse_gdb),  fc_name)
        has_m = "DISABLED"
        has_z = "DISABLED"
        spatial_ref = arcpy.Describe(temp_features).spatialReference
        arcpy.CreateFeatureclass_management(ovelse_gdb, track_features_name, "POINT", "", has_m, has_z, spatial_ref)
        track_features = os.path.join(str(ovelse_gdb), track_features_name)
        arcpy.AddMessage("Opprettet feature klasse {0}".format(track_features))
        arcpy.AddField_management(track_features, "TRACKID", "LONG")
        arcpy.AddField_management(track_features, "DATETIME", "DATE")
        arcpy.AddField_management(track_features, "JNR", "TEXT")
        dsc = arcpy.Describe(temp_features)
        cursor = arcpy.SearchCursor(temp_features)
        track_rows = arcpy.InsertCursor(track_features)
        arcpy.AddMessage("Begining populating feature klasse {0}".format(track_features))
        for row in cursor:
            shape = row.getValue(dsc.shapeFieldName)
            start = row.getValue(start_time_field)
            end = row.getValue(end_time_field)
            try:
                jnr = row.getValue("JNR")
            except:
                jnr = ""
            objectid = row.getValue("OBJECTID")
            total_dist = shape.length
            if end and start:
                total_time = end - start
                duration = 0
                while duration < total_time.total_seconds():
                    values = track_rows.newRow()
                    values.TRACKID = objectid
                    values.JNR = jnr
                    values.DATETIME = start + timedelta(seconds=duration)
                    distance = (total_dist / total_time.total_seconds()) * duration
                    point = shape.positionAlongLine(distance)
                    values.Shape = point
                    track_rows.insertRow(values)
                    duration = duration + time_interval
                    del values
            else:
                arcpy.AddMessage("Feature {0} har ikke TID_START eller TID_SLUTT.".format(jnr))

        del cursor, row, track_rows
        arcpy.AddMessage("Finalizing processing features")
        arcpy.JoinField_management(in_data=track_features, in_field="TRACKID", join_table=temp_features, join_field="OBJECTID")
        if arcpy.Exists(temp_features):
            arcpy.AddMessage("Slette midlertidige feature klasse {0}.".format(temp_features))
            arcpy.DeleteFeatures_management(temp_features)
	    arcpy.DeleteFeatures_management("in_memory")
        try:
            mxd = arcpy.mapping.MapDocument("CURRENT")
            dataFrame = arcpy.mapping.ListDataFrames(mxd, "*")[0]
            addLayer = arcpy.mapping.Layer(track_features)
            arcpy.mapping.AddLayer(dataFrame, addLayer)
        except:
            pass
        return
