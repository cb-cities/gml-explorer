import gzip
from pprint import pprint
import glob
try:
    import simplejson as json
except ImportError, e:
    import json
    print e

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

directed = []
undirected = []
total_length =[]

for file in glob.glob("../out/roadlinks*.json.gz"):
	links_file = gzip.open(file)
	links = json.load(links_file)
	print "working on file : ", file
	length = len(links)
	total_length.append(length)
	for link in links:
		found = False
		try:
			for restriction in restrictions:
				if restriction['toid_data'][0]['toid'] == link['toid']:
					data = {"link_restrictions":{"link": link,
					"restriction": restriction}}
					directed.append(data)
					found = True
					break
		finally:
			if found == False:
				undirected.append(link)

print "Directed links collated"

print "Number of links: ", sum(total_length), "links accross ", len(total_length), " individual file(s)"

print " # writing to files in ./tmp/ "

with gzip.open('./tmp/directed.json.gz','w') as outfile:
	json.dump(directed,outfile,indent=2)

with gzip.open('./tmp/undirected.json.gz','w') as outfile:
	json.dump(undirected,outfile,indent=2)

print "Number of matches found : ", len(directed)

print "unaffected links : ", len(undirected)