#!/bin/bash
mv $1 infile
./renum.exe
mv outfile $1
cobc $1

