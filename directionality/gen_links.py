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
	link = directed_link['link_restrictions']['link']
	link['restriction'] = directed_link['link_restrictions']['restriction']['restriction']
	link['orientation'] = directed_link['link_restrictions']['restriction']['toid_data'][0]['orientation']
	generated_links.append(link)


for undirected_link in undirected_links:
	undirected_link['restriction'] = False
	undirected_link['orientation'] = None
	generated_links.append(undirected_link)

print "# Reindexing"

for count, generated_link in enumerate(generated_links):
	generated_link['index'] = count

print "# Splitting into smaller json.gz files"

chunkSize = 100000
for i in xrange(0, len(generated_links), chunkSize):
	with gzip.open('./out/roadlinks' + str((i//chunkSize)+1) + '.json.gz', 'w') as outfile:
		json.dump(generated_links[i:i+chunkSize], outfile, indent=0)

print "# Calculating max/min coords"

print "# Calculating max/min coords"

nodes_files = gzip.open("../out/roadnodes1.json.gz")
nodes = json.load(nodes_files)

lats = []
lngs = []

pprint(nodes[0])

for node in nodes:
	lat = node['point'][0]
	lats.append(lat)
	lng = node['point'][1]
	lngs.append(lng)

max_lat = max(lats)
max_lng = max(lngs)
min_lat = min(lats)
min_lng = min(lngs)

print max_lat, max_lng
print min_lat, min_lng

stats = {
	'max_lat': max_lat,
	'max_lng': max_lng,
	'min_lat': min_lat,
	'min_lng': min_lng
}

with gzip.open('./out/stats.json.gz', 'w') as outfile:
	json.dump(stats, outfile)

print "# Complete"