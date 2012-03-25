#!/bin/bash

pane_dir=$1
out_dir=$2

if [ "$out_dir" == "" ] || [ ! -d $pane_dir ] || [ -f $out_dir ] || [ -d $out_dir ]; then
	echo "Required: <pane_dir> <out_dir>"
	exit
fi

rm pane
ln -s $pane_dir pane

mkdir $out_dir

gnuplot pane.plot

mv *.ps $out_dir

cd $out_dir
for i in `ls -1 *.ps`; do ps2pdf $i; done
cd ..
