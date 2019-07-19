# graphite [![Build Status](https://travis-ci.org/stchang/graph.svg?branch=master)](https://travis-ci.org/stchang/graph)

A variation graph tool in racket.

![rsv image](docs/images/rsv.png)

## Dependencies
Install dependencies

```
$ raco pkg install --deps search-auto
```

## Running

### Compile a binary

```
$ raco exe graphite/graphite.rkt
```

#### Generate a gfa
Without a compiled binary
```
racket graphite/graphite.rkt \
-o output.gfa \
data/RSV/refererence_and_vcf_file/9465113.fa \
data/RSV/refererence_and_vcf_file/H_3801_22_04.freebayes.vcf
```

If compiled, you'd just call the graphite binary with the arguments needed.

## Help

```
$ graphite -h
```

or if not compiled
```
$ racket graphite.rkt -h
```

## Visualization
Load the gfa file into [bandage](https://rrwick.github.io/Bandage)


## Documentation

To be found in the [docs directory](/docs).


## Test
Run all tests
```
$ raco test -p graphite
```

Run tests on a specific module
```
$ raco test -m tests/run-all-tests.rkt
```
