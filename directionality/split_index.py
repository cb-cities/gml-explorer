import gzip
from pprint import pprint
try:
    import simplejson as json
except ImportError, e:
    import json
    print e

results_fi = gzip.open("./out/roadlinks.json.gz")
results = json.load(results_fi)

print " # Reindexing"

for count, result in enumerate(results):
	result['index'] = count

print " # Splitting into smaller json.gz files"

chunkSize = 100000
for i in xrange(0, len(results), chunkSize):
	with gzip.open('./out/roadlinks' + str((i//chunkSize)+1) + '.json.gz', 'w') as outfile:
		json.dump(results[i:i+chunkSize], outfile)
