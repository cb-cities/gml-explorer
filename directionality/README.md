Directed & Undirected Road Links
=================================

The ITN `gml` file from the Ordnance Survey also contains Road Routing Information `RRI`.

The documentation on the RRI is found on page 30 of the [technical document](http://digimap.edina.ac.uk/webhelp/os/data_files/os_manuals/osmm_itn_userguide_v1.0.pdf)

`read_gml.py` reads and parses the `RRI` containing `gml` file. It extracts the `toid` and corresponding `restriction`, exporting this to a `json` file of the form:

```
  {
      "toid": "osgb4000000023055804", 
      "restriction": "One Way"
   },
```

This `json` file can then be combined with the `roadlink` files in order to create two seperate edges where the link is bidirectional and maintain one edge when the link is