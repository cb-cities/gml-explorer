import simplejson as json
import gzip
import glob
from pprint import pprint

links_list = []
for file in glob.glob("./out/roadlinks*.json.gz"):
	json_files = gzip.open(file)
	links = json.load(json_files)
	links_list.extend(links)

print(len(links_list))

links_list_gen = []
for file in glob.glob("./results/roadlinks*.json.gz"):
	json_files_gen = gzip.open(file)
	links_gen = json.load(json_files_gen)
	links_list_gen.extend(links_gen)

print(len(links_list_gen))