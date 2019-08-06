#lang scribble/manual

@title{Graphite - a varitation graph tool.}

Variation graphs are an alternative to the conventional linear reference used to
represent graphs.

@section[#:tag "compile"]{Compiling}

Clone the repo from https://github.com/urbanslug/graphite `cd` into the directory and run `make`
It will compile a graphite binary into `bin/`


@include-section["alignment.scrbl"]
@include-section["construction.scrbl"]
@include-section["visualization.scrbl"]