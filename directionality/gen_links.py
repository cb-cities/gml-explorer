import gzip
from pprint import pprint
import glob
try:
    import simplejson as json
except ImportError, e:
    import json
    print e

directed_files = gzip.open("./tmp/directed.json.gz")
directed_links = json.load(directed_files)

undirected_files = gzip.open("./tmp/undirected.json.gz")
undirected_links = json.load(undirected_files)

generated_links = []

for directed_link in directed_links:
	if directed_link['link_restrictions']['restriction']['toid_data'][0]['orientation'] == "-":
		polyline = directed_link['link_restrictions']['link']['polyline']
		polyline_reverse = []
		# Reverse the list (i.e. road operates in negative direction (positiveNode == negativeNode) and thus polyline must be inversed)
		length = len(polyline)/2
		for n in range(len(polyline)/2):
			n = length-n
			y = polyline[n*2-1]
			x = polyline[n*2-2]
			polyline_reverse.append(x)
			polyline_reverse.append(y)
		link = {'negativeNode': directed_link['link_restrictions']['link']['positiveNode'],
		"toid": directed_link['link_restrictions']['link']['toid'],
		"term": directed_link['link_restrictions']['link']['term'],
		"polyline": polyline_reverse,
		"positiveNode" : directed_link['link_restrictions']['link']['negativeNode'],
		"nature": directed_link['link_restrictions']['link']['nature'] }
		# Need to fix indexing
		link[u'index'] = 1
		generated_links.append(link)
	else:
		# In the correct orientation, requiring no manipulation
		directed_link = link
		generated_links.append(link)

for undirected_link in undirected_links:
	positiveLink = undirected_link
	polyline = undirected_link['polyline']
	polyline_reverse = []
	# Reverse the list (i.e. road operates in negative direction (positiveNode == negativeNode) and thus polyline must be inversed)
	length = len(polyline)/2
	for n in range(len(polyline)/2):
		n = length-n
		y = polyline[n*2-1]
		x = polyline[n*2-2]
		polyline_reverse.append(x)
		polyline_reverse.append(y)
	negativeLink = {'negativeNode': undirected_link['positiveNode'],
	"toid": undirected_link['toid'],
	"term": undirected_link['term'],
	"polyline": polyline_reverse,
	"positiveNode" : undirected_link['negativeNode'],
	"nature": undirected_link['nature'] }
	# Need to fix indexing
	negativeLink[u'index'] = 1
	generated_links.append(negativeLink)
	generated_links.append(positiveLink)

print "# checking stuff ..."

print "total number of directed links :", len(directed_links)
print "total number of undirected links :", len(undirected_links) * 2

print "total number of links generated :", len(generated_links)

total_undirected_links = len(undirected_links) * 2

if len(generated_links) == total_undirected_links + len(directed_links):
	print "No issues detected"
else:
	print "Unkown issue detected"

print "# Reindexing"

for count, generated_link in enumerate(generated_links):
	generated_link['index'] = count

print "# Splitting into smaller json.gz files"

chunkSize = 100000
for i in xrange(0, len(generated_links), chunkSize):
	with gzip.open('./out/roadlinks' + str((i//chunkSize)+1) + '.json.gz', 'w') as outfile:
		json.dump(generated_links[i:i+chunkSize], outfile)

print "# Complete"
