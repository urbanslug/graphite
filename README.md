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
```
./bin/graphite construct \
 -o z.dot \
 -f dot \
  data/1mb1kgp/z.fa  data/1mb1kgp/z.vcf 
```

#### Generate a dot
```
./bin/graphite construct \
 -o z.gfa \
 -f gfa \
  data/1mb1kgp/z.fa  data/1mb1kgp/z.vcf
```

### Graph update
Output a .gra file (a format specific to graphite/racket)
```
./bin/graphite construct \
 -o rsv1.gra \
 -f gra \
data/RSV/refererence_and_vcf_file/9465113.fa data/RSV/refererence_and_vcf_file/H_3801_22_04.freebayes.vcf
```

Update the graph and output a dot format or whatever other format you'd like:
```
./bin/graphite update \
 -o rsv2.dot \
 -f dot \
 rsv1.gra data/RSV/refererence_and_vcf_file/fake_H_3801_22_04.freebayes.vcf
```

### Align reads
Generate a partial order alignment
```
./bin/graphite align \
 tests/poa/example4.fa
```

## Visualization

Generate a visualization out of a gra(serialized graph)
```
./bin/graphite view \
 -o rsv1.dot \
 -f dot \
 rsv1.gra

```

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
