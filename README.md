# graphite [![Build Status](https://travis-ci.org/stchang/graph.svg?branch=master)](https://travis-ci.org/stchang/graph)

A tool that uses implements a variant graph and other necessary functionality for [BioD](https://github.com/biod/biod)



## Dependencies
```
raco pkg install graph
```


## Running

### Compile a binary

```
raco exe graphite.rkt 
```

### Run graphite from source

```
racket graphite/graphite.rkt data/RSV/refererence_and_vcf_file/9465113.fa data/RSV/refererence_and_vcf_file/H_3801_22_04.freebayes.vcf 
```

By default graphite outputs graph in dot format to `data/output/graph.gv`.

## Help

```
graphite -h
```

or if not compiled
```
racket graphite.rkt -h
```

## Visualization

Use graphviz to view the graph as an SVG.  
Graphite exports the graph in dot format into `data/output/graph.gv` but you can override this with the `-o` flag
```
cd data/output

dot -Tsvg -o graph.svg graph.gv
```

## Documentation

To be found in the [docs directory](/docs).
