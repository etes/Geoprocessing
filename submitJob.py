import urllib, json, time
    
requestURL = "http://ARCGISSERVER:6080/arcgis/rest/services/System/PublishingTools/GPServer/Get%20Cache%20Info/submitJob?in_serverCacheFolder=%5C%5CWILLEM3%5Carcgisserver%5Cdirectories%5Carcgiscache&in_serviceName=NP_Basiskart_JanMayen_WMTS_25833_UtenStedsnavn&in_serviceType=MapServer&in_serviceFolder=Basisdata_Intern&in_resultFormat=&env%3AoutSR=&env%3AprocessSR=&returnZ=false&returnM=false&token=TOKEN&f=json"

taskUrl = "http://ARCGISSERVER:6080/arcgis/rest/services/System/PublishingTools/GPServer/Get%20Cache%20Info"
submitUrl = taskUrl + "/submitJob"

data = {'in_serverCacheFolder' : "\\SERVER\arcgisserver\directories\arcgiscache",
        'in_serviceName' : "NP_Basiskart_JanMayen_WMTS_25833_UtenStedsnavn",
        'in_serviceType' : "MapServer",
        'in_serviceFolder' : "Basisdata_Intern",
        'in_resultFormat': "&env%3A",
        'outSR' : '&env%3A',
        'processSR': '&env%3A',
        'returnZ' : 'false',
        'returnM' : 'false',
        'token' : '',
        'f' : 'json'
        }
taskUrl = "http://willem3:6080/arcgis/rest/services/System/PublishingTools/GPServer/Get%20Cache%20Info"
submitUrl = taskUrl + "/submitJob"
submitResponse = urllib.urlopen(submitUrl, urllib.urlencode(data))   
submitJson = json.loads(submitResponse.read())
if 'jobId' in submitJson:  
    jobID = submitJson['jobId']        
    status = submitJson['jobStatus']        
    jobUrl = taskUrl + "/jobs/" + jobID
    jobData = {'token' : 'gRSvTxB_Pg1a6C7VoPIeFNz4r4yvWPHCTldfIbqSGEgbypfw3mv3uMpAsmvw1Llq', 'f' : 'json'}
    jobResponse = urllib.urlopen(jobUrl, urllib.urlencode(jobData))     
    jobJson = json.loads(jobResponse.read())
    
    if 'jobStatus' in jobJson:  
        status = jobJson['jobStatus']            
     
        if status == "esriJobSucceeded":                                        
                if 'results' in jobJson:
                    resultsUrl = jobUrl + "/results/"
                    resultsJson = jobJson['results']
                    for paramName in resultsJson.keys():
                        resultUrl = resultsUrl + paramName                                        
                        resultResponse = urllib.urlopen(resultUrl, "f=json")   
                        resultJson = json.loads(resultResponse.read())                            
                        print resultJson     
            
        if status == "esriJobFailed":                                        
                if 'messages' in jobJson:                        
                    print jobJson['messages']
                                       
else:
    print "no jobId found in the response"
    

