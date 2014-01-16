from urllib import urlencode
from urllib2 import urlopen
 
pg = urlopen("http://willem.npolar.no/ArcGIS/services/inspire1/TopoSvalbard/MapServer/WMSServer?request=GetCapabilities&service=WMS")

text = pg.read().decode("utf8") 
title = text.find('<Title>') 
start_of_title = title + 16 
titleend = text.find('</Title>')
end_of_title = titleend - 3 
wmstitle = text[start_of_title:end_of_title]

print "The title of the WMS services is:", wmstitle

