_gml-explorer_
==============


Features
--------

- Road nodes
- Road links
	- Direction
	- Length
- Roads

Automated Usage
-----

You can now use a bash script to automate the generation of node, links and road files from a given `gml` input. This also features the `python` directionality implementation which adds road restriction information. The directionality implementation makes use of multiple cores.

`cd gml-explorer/bin`

`./builder`

### Change input
Presently, this builds from the `itn_repository` repo. To change the `gml` file input, edit the `builder` bash script. Change the given directory relative to the `gml-explorer` directory:

`make run ARGS="/path/to/directory/mastermap-itn_1445496_0.gml roadnodes"`

NOTE: To reduce bandwith demands, the `gml` files are compressed using `gzip`. To compress a `.gml` using the following command: `gzip *.gml`


Manual Usage
-----

First, download the project source and data files:

The following instructions assume the project is in the current working directory.

```
cd gml-explorer
```

### Installing dependencies

The project is written in [Haskell](https://www.haskell.org/).  To build the project, the GHC compiler and the [`cabal-install`](https://www.haskell.org/cabal/) tool must be installed on the local machine.

The project is developed on OS X, but may support other UNIX platforms.  On OS X, system-level dependencies should be installed with the [`brew`](http://brew.sh/) tool.

```
brew install ghc cabal-install
cabal update
```


### Building the project

[GNU Make](https://www.gnu.org/software/make/) is used to simplify managing the Cabal sandbox, installing project-level dependencies, and building the project.  The `make` tool is included with OS X.

To build the project, give the following command:

```
make
```

If the build is successful, the project is ready to run in-place — either via `make`, or directly.  Both methods are equivalent.

For example, let us extract OS RoadNode geometry from a GML file located at `/Users/fnord/mastermap-itn_123456/mastermap-itn_123456_0.gml`.  Results will be located in the default output directory, `out`.

To extract via `make`:

```
make run ARGS="/Users/fnord/mastermap-itn_123456/mastermap-itn_123456_0.gml roadnodes"
```

To extract directly:

```
./dist/build/gml-explorer/gml-explorer /Users/fnord/mastermap-itn_123456/mastermap-itn_123456_0.gml roadnodes
```


### Options and commands

The project accepts command-line arguments according to the following grammar:

```
gml-explorer INPUT_FILE [-o OUTPUT_DIR] [-s MAX_FILE_SIZE] COMMAND
```

Arguments given in square brackets are optional.  Other arguments are mandatory.

Option             | Description
------------------ | -----------
`-h`,`--help`      | Show help text
`INPUT_FILE`       | File containing OS GML input
`-o OUTPUT_DIR`    | Output directory (default: `out`)
`-s MAX_FILE_SIZE` | Maximum size of file to output (default: `31457280`)

Command        | Description
------------   | -----------
`roads`        | Output OS Road geometry
`roadlinks`    | Output OS RoadLink geometry
`roadnodes`    | Output OS RoadNode geometry
`ferrylinks`   | Output OS FerryLink geometry
`ferrynodes`   | Output OS FerryNode geometry
`roadlinkrci` | Output OS RoadLinkRCI geometry


About
-----

Made by [Miëtek Bak](https://mietek.io/).  Published under the [MIT X11 license](LICENSE.md).
