import gzip
import simplejson as json
import glob

directed_files = gzip.open("directed.json.gz")
directed = json.load(directed_files)

undirected_files = gzip.open("undirected.json.gz")
undirected = json.load(undirected_files)

print 'previous output'

print(len(directed))

print(len(undirected))
sums = len(directed) + len(undirected)
print(sums)

tot_length = []
for file in glob.glob("../../out/roadlinks*.json.gz"):
	links_file = gzip.open(file)
	links = json.load(links_file)
	length = len(links)
	tot_length.append(length)

print (sum(tot_length))