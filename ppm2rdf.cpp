#include <iostream>
#include <string>
#include <sstream>

std::string a="\
<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
<!-- Created with ppm2rdf.cpp (https://github.com/millercl/xslt-rdf-mr) -->\n\
<!DOCTYPE rdf:RDF [\n\
 <!ENTITY rdf 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'>\n\
 <!ENTITY tiff 'http://ns.adobe.com/tiff/1.0/'>\n\
 <!ENTITY xsd 'http://www.w3.org/2001/XMLSchema#'>\n\
 ]>\n\
<RDF xmlns=\"&rdf;\" xmlns:tiff=\"&tiff;\" xml:lang=\"";
std::string c="zxx";
std::string d="\" >\n";
int l=1;
int m=0;
int w=0;
int h=0;
int r=0;
int g=0;
int b=0;
int x=1;
int y=1;
bool t=true;
int z=255;
std::string line;

int main(int argc, char **argv) {
 if(argc>=2){
  c=argv[1];
 }
 if(argc==3){
  std::istringstream iss = std::istringstream(argv[2]);
  iss >> z;
  t=false;
 }
 while(getline(std::cin, line, '\n')) {
  if(l==1 && line=="P3") {
   std::cout << a << c << d;
  }
  if(l==3) {
   std::istringstream iss = std::istringstream(line);
   iss >> w;
   iss >> h;
   std::cout << " <Description about=\"\" > <tiff:ImageWidth datatype=\"&xsd;int\" >";
   std::cout << w;
   std::cout << "</tiff:ImageWidth> </Description>\n";
   std::cout << " <Description about=\"\" > <tiff:ImageLength datatype=\"&xsd;int\" >";
   std::cout << h;
   std::cout << "</tiff:ImageLength> </Description>\n";
  }
  if(l==4 && line!="255") {
   return 4;
  }
  if(l>4) {
   std::istringstream iss = std::istringstream(line);
   switch(m) {
    case 0:
     iss >> r;
     break;
    case 1:
     iss >> g;
     break;
    case 2:
     iss >> b;
     break;
   }
   if(m==2) {
    if(t||(r==255&&g==0&&b>=z)){
     std::cout << " <Description about=\"&rdf;_" << x << "\" > ";
     std::cout << "<_" << y << " >#" ;
     printf("%02x",r) ;
     printf("%02x",g) ;
     printf("%02x",b) ;
     std::cout << "</_" << y << "> </Description>\n";
    }
    if((x+1)>w) {
     y++;
    }
    x=(x%w)+1;
   }
   m=(m+1)%3;
  }
  l++;
 }
 std::cout << "</RDF>\n";
 return 0;
}
