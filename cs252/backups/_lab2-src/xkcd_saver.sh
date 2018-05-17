#!/bin/bash

rm -f -r imgs/
mkdir imgs

start_range=$1
end_range=$2
comic=$1

numComics=$((end_range - start_range + 1))

if [ $# -lt 2 ] || [ $numComics -gt 21 ] || [ $numComics -lt 0 ]; then
	echo "Usage: $0 start_range end_range"
	echo "range, i.e. end_range-start_range, cannot be greater than 20"
	echo "start_range cannot be after end_range"
	rm -f index.html*
	rm -f -r imgs/
	exit
fi

while [ $numComics -gt 0 ]; do
	wget_output=$(wget -q "www.xkcd.com/$comic" -O index.html)
	if [ $? -eq 0 ]; then
		imgLink=$(egrep "hotlinking" index.html | cut -c39-)
		wget -q $imgLink -O ./imgs/$(echo $imgLink | cut -c29-)
	else 
		echo The comic $comic does not seem to exist.
	fi
	let numComics=numComics-1
	let comic=comic+1
done

cd imgs/

if egrep -q ".png" <<< $(ls); then
	tar -zcf ../xkcd_comics.tar . 
	echo Tarball created
else
	echo No comics were downloaded, so there will be no tarball created
fi

cd ..

rm -f index.html*
rm -f -r imgs/

exit
