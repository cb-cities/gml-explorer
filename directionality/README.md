Directed & Undirected Road Links
=================================

The ITN `gml` file from the Ordnance Survey also contains Road Routing Information (`RRI`).

The documentation on the RRI is found on page 30 of the [technical document](http://digimap.edina.ac.uk/webhelp/os/data_files/os_manuals/osmm_itn_userguide_v1.0.pdf)

The build process contains the following parts:

1. `read_gml.py`

`read_gml.py` reads and parses the `RRI` containing `gml` file. It extracts the `toid`, `orientation` and corresponding `restriction` in the form:

```
{
      "restriction": "One Way",
      "toid_data": [
         {
            "toid": "osgb4000000023429420",
            "orientation": "+"
         }
      ]
   },
```

This is then exported to `./tmp/restricted_links_list.json.gz`

2. `mod_json.py`

`mod_json.py` reads the `restricted_links_list.json.gz` file and iterates through all `roadlinks` files matching `restrictions ` with their corresponding `roadlinks`.

It then extracts those which are `directed`, i.e. `One Way` and exports these to `./tmp/directed.json.gz`. Those whcih are `undirected` are exported to `./tmp/undirected.json.gz`.

3. `gen_links.py`

`gen_links.py` iterates through the `directed` and `undirected` files generated previously and creates individual links. In the case of an unidrected link, an inverted original link is created.

The new `links` are reindexed and outputted in manageable file sizes.

A final summary is taken off the coordinates to act as input for `defs.js` in `sierra-charlie`.


