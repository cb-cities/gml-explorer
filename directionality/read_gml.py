import gzip
import json
import xml.etree.ElementTree
from pprint import pprint
import time

x = time.time()
print 'loading gml file'

gml_file = gzip.open("/Users/gerardcasey/itn_repository/London/gml/mastermap-itn_1445496_0.gml.gz")

# Read RRI data
# Isolated RRI data `gml`
e = xml.etree.ElementTree.parse(gml_file).getroot()

namespaces = {'osgb': 'http://www.ordnancesurvey.co.uk/xml/namespaces/osgb'}
namesspace = {'xlink':'http://www.w3.org/1999/xlink'}
y = time.time()

print 'loaded in : ' + str(y-x) + ' seconds'

results = []

for elem in e.findall("osgb:roadInformationMember", namespaces):
	for ele in elem:
		toid = ele.get('fid')
		for i in ele.findall("osgb:directedLink",namespaces):
			orientation = i.get('orientation')
			toid_raw = i.get('{http://www.w3.org/1999/xlink}href')
			# Remove hashtag
			toid = toid_raw[1:]
		for el in ele.findall("osgb:environmentQualifier",namespaces):
			for e in el.findall("osgb:instruction",namespaces):
				restriction = e.text
				toid = toid
				data = {"restriction": restriction,
				"toid": toid}
				results.append(data)

with gzip.open('./out/directedlinks.json.gz', 'w') as outfile:
	json.dump(results, outfile, indent=3)