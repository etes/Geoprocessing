'''
Created on 3. des. 2014

@author: ermias
'''
import arcpy
import glob, os, sys
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")
arcpy.env.overwriteOutput = True
from multiprocessing import Pool

def reclassRaster(inraster):
        #Get Path and Name of Inputfile
    (rasterpath, filename) = os.path.split(inraster)             #get path and filename seperately
    (rastername, extension) = os.path.splitext(filename)           #get file name without extension
    outfilepath = rasterpath + '\\reclassed\\'
    if not os.path.exists(outfilepath):
        os.makedirs(outfilepath)
    outraster = outfilepath + rastername + '_reclass' + '.tif'
    arcpy.AddMessage("Reclassifying {0}".format(inraster))
    remap = RemapRange([[0,0.01, 0],[0.01,10,5],[10,20,15], [20,30,25],[30,40,35],[40,50,45],[50,60,55],[60,70,65],[70,80,75],[80,90,85],[90,99,95],[99,600,100]])
    #arcpy.gp.Reclassify_sa(inraster,"Value","0 0;10 5;2 25;3 55;4 80;5 95;6 100;NODATA 0",outraster,"DATA")
    outReclassify = Reclassify(inraster, "Value", remap, "DATA")
    outReclassify.save(outraster)
    arcpy.AddMessage("Saving {0}".format(outraster))


if __name__ == '__main__':
    pool = Pool(processes=10)              # start 10 worker processes
    filelist = glob.glob('filelist = glob.glob("E:\\data\\Barentsportal\\Ocean\\seaice\\maxkonig\\EPSG32637\\*.tif")')
    pool.map(reclassRaster,filelist)

