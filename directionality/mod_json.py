import gzip
from pprint import pprint
from functools import partial
import glob
try:
    import simplejson as json
except ImportError, e:
    import json
    print e
import multiprocessing

print " # Reading restricted_links_list file"

dif_files = gzip.open("./tmp/restricted_links_list.json.gz")
dif = json.load(dif_files)

print "Number of restrictions (all types): ", len(dif)
print " # Isolating One Way restrictions"

restrictions = []
for di in dif:
	if di['restriction'] == "One Way":
		restrictions.append(di)

print "Number of One Way restrictions : ", len(restrictions)

print "Commencing matching restrictions to links files"

# Create a list of file inputs
list_files=[]
for file in glob.glob("../out/roadlinks*.json.gz"):
	list_files.append(file)

# Function to iterate and match restrictions to links
def link_restriction_match(directed_matches, undirected_matches, file):
	local_directed = []
	local_undirected = []
	links_file = gzip.open(file)
	links = json.load(links_file)
	print "working on file : ", file
	for link in links:
		found = False
		try:
			for restriction in restrictions:
				if restriction['toid_data'][0]['toid'] == link['toid']:
					link['restriction'] = restriction
					directed_matches.append(link)
					found = True
					break
		finally:
			if found == False:
				undirected_matches.append(link)
				pass

# Do in parallel
if __name__ == '__main__':
	manager = multiprocessing.Manager()
	directed_matches = manager.list()
	undirected_matches = manager.list()
	matches = partial(link_restriction_match, directed_matches, undirected_matches)
	print "Distributing work across CPU cores"
	pool = multiprocessing.Pool(processes=8)
	pool.map(matches, list_files)
	pool.close()
	pool.join()
	print "Map processes complete"

directed = list(directed_matches)
undirected = list(undirected_matches)

print "Directed links collated"

print "Number of links: ", str(len(directed) + len(undirected)), "links accross ", len(list_files), " individual file(s)"

print " # writing to files in ./tmp/ "

with gzip.open('./tmp/directed.json.gz','w') as outfile:
	json.dump(directed,outfile,indent=2)

with gzip.open('./tmp/undirected.json.gz','w') as outfile:
	json.dump(undirected,outfile,indent=2)

print "Number of matches found : ", len(directed_matches)

print "unaffected links : ", len(undirected_matches)