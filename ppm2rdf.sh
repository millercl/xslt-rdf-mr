#!/bin/bash
a=$(cat <<SETA
<?xml version="1.0" encoding="UTF-8"?>
<!-- Created with ppm2rdf.sh (https://github.com/millercl/xslt-rdf-mr) -->
<!DOCTYPE rdf:RDF [
 <!ENTITY rdf 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'>
 ]>
<RDF xmlns="&rdf;" xml:lang="${1:-zxx}" >
SETA
)
l=1
m=0
w=0
r=0
g=0
b=0
x=1
y=1
while read line
do
 [[ $l -eq 1 && $line != "P3" ]] && exit
 if [[ $l -eq 3 ]]
 then
  w=`echo $line | cut -d ' ' -f 1`
 fi
 if [ $l -eq 4 ]
 then
  [[ $line != "255" ]] && exit
  echo "$a"
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
   echo " <Description about=\"&rdf;_$x\" > <_$y >#$r$g$b</_$y> </Description>"
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
