#!/bin/bash
a=$(cat <<SETA
<?xml version="1.0" encoding="UTF-8"?>
<!-- Created with ppm2rdf.sh (https://github.com/millercl/xslt-rdf-mr) -->
<!DOCTYPE rdf:RDF [
 <!ENTITY rdf 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'>
 <!ENTITY tiff 'http://ns.adobe.com/tiff/1.0/'>
 <!ENTITY xsd 'http://www.w3.org/2001/XMLSchema#'>
 ]>
<RDF xmlns="&rdf;" xmlns:tiff="&tiff;" xml:lang="${1:-zxx}" >
SETA
)
l=1
m=0
w=0
h=0
r=0
g=0
b=0
x=1
y=1
t=true
z=171
if [ ! -z $2 ]
then
 t=false
 z=$2
fi
while read line
do
 if [[ $l -eq 1 && $line -eq "P3" ]]
 then
  echo "$a"
 fi
 if [[ $l -eq 3 ]]
 then
  w=`echo $line | cut -d ' ' -f 1`
  h=`echo $line | cut -d ' ' -f 2`
  echo " <Description about=\"\" > <tiff:ImageWidth datatype=\"&xsd;int\" >$w</tiff:ImageWidth> </Description>"
  echo " <Description about=\"\" > <tiff:ImageLength datatype=\"&xsd;int\" >$h</tiff:ImageLength> </Description>"
 fi
 if [ $l -eq 4 ]
 then
  [[ $line != "255" ]] && exit
 fi
 if [ $l -gt 4 ]
 then
  case $m in
   0 ) r=`printf '%02x' $line` ;;
   1 ) g=`printf '%02x' $line` ;;
   2 ) b=`printf '%02x' $line` ;;
  esac
  if [ $m -eq 2 ]
  then
   if [ $t = true ] || (( 0x$r == 255 && 0x$g == 0 && 0x$b >= $z ))
   then
    echo " <Description about=\"&rdf;_$x\" > <_$y >#$r$g$b</_$y> </Description>"
   fi
   if [ $(($x+1)) -gt $w ]
   then
    ((y++))
   fi
   x=$(( ( x % w ) + 1 ))
  fi
  m=$(( ( m + 1 ) % 3 ))
 fi
 ((l++))
done
cat <<SETC
</RDF>
SETC
