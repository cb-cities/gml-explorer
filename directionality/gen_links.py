import gzip
from pprint import pprint
import glob
import simplejson as json
import math
from pyproj import Proj, transform

directed_files = gzip.open("./tmp/directed.json.gz")
directed_links = json.load(directed_files)

undirected_files = gzip.open("./tmp/undirected.json.gz")
undirected_links = json.load(undirected_files)

generated_links = []

for directed_link in directed_links:
	link = {
	"index": directed_link['index'],
	"term": directed_link['term'],
	"negativeNode": directed_link['negativeNode'],
	"nature": directed_link['nature'],
	"toid": directed_link['toid'],
	"polyline": directed_link['polyline'],
	"positiveNode": directed_link['positiveNode'],
	"restriction": directed_link['restriction']['restriction'],
	"orientation": directed_link['restriction']['toid_data'][0]['orientation']
	}
	generated_links.append(link)

for undirected_link in undirected_links:
	undirected_link['restriction'] = "No Restriction"
	undirected_link['orientation'] = None
	generated_links.append(undirected_link)

print "# Reindexing"

for count, generated_link in enumerate(generated_links):
	generated_link['index'] = count

inProj = Proj(init='epsg:27700')
outProj = Proj(init='epsg:4326')

def distance_calc(O_lat,O_lng,D_lat,D_lng):
	return math.acos(math.cos(math.radians(90-D_lat)) * math.cos(math.radians(90-O_lat)) + math.sin(math.radians(90-D_lat)) *math.sin(math.radians(90-O_lat)) *math.cos(math.radians(D_lng-O_lng))) *6371000

print "# Computing link length and adding to record"

for generated_link in generated_links:
	pairs = []
	for i in range(len(generated_link['polyline'])/2):
		o = (generated_link['polyline'][i*2],generated_link['polyline'][i*2+1])
		# Convert to lat/lng for metric distance output
		o_c = transform(inProj,outProj,o[0],o[1])
		# Re-order in form (lat,lng)
		o_c = o_c[1],o_c[0]
		pairs.append(o_c)
	distance_total = 0
	for i in range(len(pairs)-1):
		try:
			distance = distance_calc(pairs[i][0],pairs[i][1],pairs[i+1][0],pairs[i+1][1])
		except ValueError,e:
			# Debugging
			# print(e, "for these coordinates")
			# print (pairs[i][0],pairs[i][1],pairs[i+1][0],pairs[i+1][1])
			# Round up to 1m
			distance = 1
		distance_total = distance_total + distance
	generated_link['length'] = distance_total

print "# Link length added to record"

print "# Splitting into smaller json.gz files"

chunkSize = 100000
for i in xrange(0, len(generated_links), chunkSize):
	with gzip.open('../out/roadlinks' + str((i//chunkSize)+1) + '.json.gz', 'w') as outfile:
		json.dump(generated_links[i:i+chunkSize], outfile, indent=2)

print "# Calculating max/min coords"

print "# Calculating max/min coords for sierra-charlie defs.js"

nodes_files = gzip.open("../out/roadnodes1.json.gz")
nodes = json.load(nodes_files)

lats = []
lngs = []

for node in nodes:
	lat = node['point'][0]
	lats.append(lat)
	lng = node['point'][1]
	lngs.append(lng)

max_lat = max(lats)
max_lng = max(lngs)
min_lat = min(lats)
min_lng = min(lngs)

stats = {
	'max_lat': max_lat,
	'max_lng': max_lng,
	'min_lat': min_lat,
	'min_lng': min_lng
}

with gzip.open('../out/stats.json.gz', 'w') as outfile:
	json.dump(stats, outfile)

print "# Complete"