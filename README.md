# GSoC experiments
Some experiments for GSoC 2019

# Deps
```
raco pkg install graph
```

# Running
```
# Output a graph.gv in dot format
racket vg.rkt
By default outputs to data/output/graph.gv.


# Visualize the graph - output an SVG
cd data/output
dot -Tsvg -o graph.svg graph.gv
```
