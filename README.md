# graphite [![Build Status](https://travis-ci.org/stchang/graph.svg?branch=master)](https://travis-ci.org/stchang/graph)

A variation graph tool in racket.

![rsv image](docs/images/rsv.png)

## Compile
```
$ make
```

## Running
### Graph construction
#### Generate a gfa
Without a compiled binary
```
$ ./bin/graphite construct \
 -o z.dot \
 -f dot \
  data/1mb1kgp/z.fa  data/1mb1kgp/z.vcf 
```

#### Generate a dot
```
$ ./bin/graphite construct \
 -o z.gfa \
 -f gfa \
  data/1mb1kgp/z.fa  data/1mb1kgp/z.vcf
```

### Graph update

## Visualization
### GFA
Load the GFA file into [bandage](https://rrwick.github.io/Bandage)

### Graphviz
Generate svg out of .dot
```
$ dot -Tsvg -o z.svg z.dot
```

## Documentation
To be found at [official graphite docs](https://urbanslug.github.io/graphite/).

## Test
Run all tests
```
$ raco test -p graphite
```

Run tests on a specific module
```
$ raco test -m tests/run-all-tests.rkt
```
