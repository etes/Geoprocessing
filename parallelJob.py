#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      ermias.TESSINFJELLET
#
# Created:     19.12.2012
# Copyright:   (c) ermias 2012
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import arcpy
import multiprocessing

def simpleFunction(arg):
    return arg*3

if __name__ == "__main__":
    arcpy.AddMessage(" Multiprocessing test...")
jobs = []
pool = multiprocessing.Pool()
for i in range(10):
    job = pool.apply_async(simpleFunction, (i,))
    jobs.append(job)

for job in jobs: # collect results from the job server
    print job.get()

del job, jobs
arcpy.AddMessage(" Complete")
print "Complete"