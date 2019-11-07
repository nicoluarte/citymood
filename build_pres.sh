#!/bin/bash
while :
do
  sleep 1
  pandoc -t beamer -o presentation.pdf presentation.md --filter pandoc-citeproc --pdf-engine xelatex --bibliography=references.bib --slide-level 2
done

