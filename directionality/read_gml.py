import gzip
import xml.etree.ElementTree
from pprint import pprint
import time
try:
    import simplejson as json
except ImportError, e:
    import json
    print e


x = time.time()
print 'loading gml file'

gml_file = gzip.open("/Users/gerardcasey/itn_repository/London/gml/mastermap-itn_1445595_0_RRI.gml.gz")

# Read RRI data
e = xml.etree.ElementTree.parse(gml_file).getroot()

namespaces = {'osgb': 'http://www.ordnancesurvey.co.uk/xml/namespaces/osgb'}
namesspace = {'xlink':'http://www.w3.org/1999/xlink'}
y = time.time()

print 'loaded in : ' + str(y-x) + ' seconds'

results = []

print 'parsing gml file for restrictions'

for elem in e.findall("osgb:roadInformationMember", namespaces):
	for ele in elem:
		toid = ele.get('fid')
		toids = []
		for i in ele.findall("osgb:directedLink",namespaces):
			orientation = i.get('orientation')
			toid_raw = i.get('{http://www.w3.org/1999/xlink}href')
			# Remove hashtag
			toid = toid_raw[1:]
			toid_data = {'toid': toid,
			"orientation": orientation}
			toids.append(toid_data)
		for el in ele.findall("osgb:environmentQualifier",namespaces):
			for e in el.findall("osgb:instruction",namespaces):
				restriction = e.text
				data = {"restriction": restriction,
				"toid_data": toids}
				results.append(data)
				# print data
				
print 'exporting to ./tmp/restricted_links_list.json.gz file'

with gzip.open('./tmp/restricted_links_list.json.gz', 'w') as outfile:
	json.dump(results, outfile, indent=3)