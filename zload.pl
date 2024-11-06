#!/usr/bin/env swipl
:- use_module( library( dcg/basics ) ) .
:- use_module( library( lists ) ) .
:- use_module( library( semweb/rdf11 ) ) .
:- use_module( library( semweb/rdf11_containers ) ) .
:- use_module( library( semweb/rdf_portray ) ) .
:- use_module( library( semweb/rdf_zlib_plugin ) ) .
:- rdf_portray_as( prefix:id ) .
:- rdf_register_prefix( tiff, 'http://ns.adobe.com/tiff/1.0/' ) .

load( File ) :-
    file_base_name( File, Basename ) ,
    file_name_extension( Name, ".xml", Basename ) ,
    rdf_load( File, [ base_uri( Name ), silent( false ) ] ) .

main( [] ) :-
    load( "o.xml" ) .

:- initialization( main, main ) .

hexrgb( R , G , B ) -->
    { var( R ) } , % type safety determines direction:
    { var( G ) } , % this way is uninstantiated toward parsing.
    { var( B ) } , % get integers out.
    ! ,
    [ '#' ] ,
    xdigit( D1 ) , xdigit( D2 ) , % six hexadecimal digits next.
    xdigit( D3 ) , xdigit( D4 ) ,
    xdigit( D5 ) , xdigit( D6 ) ,
    { R is 16 * D1 + D2 } , % perserve significant digit values .
    { G is 16 * D3 + D4 } , % base-16 math.
    { B is 16 * D5 + D6 } . % goes back to predicate signature.

hexrgb( R , G , B ) -->
    { integer( R ) , R >= 0 , R =< 255 } , % type safety determines direction:
    { integer( G ) , G >= 0 , G =< 255 } , % this way is instantiated toward
    { integer( B ) , B >= 0 , B =< 255 } , % generation; get strings out.
    ! ,
    { string_codes( '#' , W ) } ,
    { format(string( X ) ,'~|~`0t~16r~2|', [ R ] ) } , % leading zeros hexadecimal
    { format(string( Y ) ,'~|~`0t~16r~2|', [ G ] ) } , % comes from signature
    { format(string( Z ) ,'~|~`0t~16r~2|', [ B ] ) } , % does not work w/ xdigit.
    W, X, Y, Z .

h( S , P , R , G , B , T ) :-
    rdf( N , M , L@T ) , % this is a performance quirk, rdf query goes first.
    rdfs_container_membership_property( N , S ) , % constrain subject to _n .
    rdfs_container_membership_property( M , P ) , % constrain predicate to _n .
    string_chars( L , X ) , % this is a workaround for the rdf11 library .
    phrase( hexrgb( R , G , B ) , X ) . % call the dcg parse on the langtag.

qtc( R , G , B ) -->
    { integer( R ) , R >= 0 , R =< 15 } ,
    { integer( G ) , G >= 0 , G =< 15 } ,
    { integer( B ) , B >= 0 , B =< 15 } ,
    { string_codes( '#' , W ) } ,
    W , xinteger( R ) , xinteger( G ) , xinteger( B ) .

wsr( B , N ) :-
    integer( B ) , B >= 0 , B =< 255 , % instantiated
    var( N ) , % uninstantiated
    N is div( B , 16 ) ,
    ! . % improve arithmetic termination, don't try another way.

wsr( B , N ) :-
    var( B ) , % opposite direction, uninstantiated
    integer( N ) , N >=0 , N =< 15 , % instantiated, no double uninstant.
    B is N * 16 + N ,
    ! . % improve arithmetic termination.

update(X,Y,RR,GG,BB,T) :-
    wsr(RR,R) ,
    wsr(GG,G) ,
    wsr(BB,B) ,
    phrase( qtc(R,G,B) , LNSC ) ,
    phrase( hexrgb(RR,GG,BB) , LBSC ) ,
    string_codes( LN , LNSC ) ,
    string_codes( LB , LBSC ) ,
    rdfs_container_membership_property( S , X ) ,
    rdfs_container_membership_property( P , Y ) ,
    rdf_retractall( S , P , LB@T ) ,
    rdf_assert( S , P , LN@qtc ) .

% generate alternatives for rdf coordinates in order .
% left to right, top to bottom .
lrtb( X , Y , T ) :-
    findall( Y , ( rdf_predicate( P ) , rdfs_container_membership_property( P , Y ) ) , BY ) ,
    max_list( BY , MY ) , % list of actual axis coordinates BY is potentially unordered.
    findall( X , ( rdf_subject( S ) , rdfs_container_membership_property( S , X ) ) , BX ) ,
    max_list( BX , MX ) , % likewise with BX, take only the maximum for between/3.
    between( 1, MY, Y ) , % the x/y order is sigificant to sorting.
    between( 1, MX, X ) ,
    rdfs_container_membership_property( P , Y ) ,
    rdfs_container_membership_property( S , X ) ,
    rdf( S , P , _O@T ) .

% collect the ordered alternative from lrtb/3.
coor( L, T ) :- % the term rewrite 'o(x,y)' is for legibity ;
    findall( o(X,Y) , lrtb( X, Y, T ) , L ) . % but it will also pass to re/3 .

attributes_svg -->
    { rdf( _, tiff:'ImageLength', MY^^xsd:int ) ,
      rdf( _, tiff:'ImageWidth', MX^^xsd:int ) ,
      string_concat( "0 0 " , MX , V ) ,
      string_concat( V , " " , B ) ,
      string_concat( B , MY , VB ) } ,
    [ height='100%' ,
      viewBox=VB ,
      width='100%' ,
      style="background-image: linear-gradient(45deg, #ccc 25%, transparent 25%), linear-gradient(135deg, #ccc 25%, transparent 25%), linear-gradient(45deg, transparent 75%, #ccc 75%), linear-gradient(135deg, transparent 75%, #ccc 75%);background-size: 24px 24px;background-position: 0 0, 12px 0, 12px -12px, 0px 12px;" ,
      xmlns='http://www.w3.org/2000/svg' ] .

:- use_module( library( apply ) ) . % for maplist/3

% rewrite a single coordinate term as a svg:rect element .
% coordinate(x,y) |-> element( rect , [x,y,...] , [sub-elements] ) .
re( T, o(Xo,Yo) , element( rect, A , [] ) ) :- % []: there are no sub-elements .
    rdfs_container_membership_property( S , Xo ) , % e.g: ( rdf:'_1' , 1 ) .
    rdfs_container_membership_property( P , Yo ) ,
    rdf( S , P , L@T ) , % bind L to the coordinated object type rdf:langString .
    string_concat( "fill:" , L , Style ) , % inline CSS attribute value .
    Xk is Xo-1 , % convert between cardinal and ordinal coordinates .
    Yk is Yo-1 , % e.g: (0,0) <- (1,1) .
    C=[ style=Style , width=1 , height=1 , x=Xk , y=Yk ] ,
    string_chars( L , X ) , % this is a workaround for the rdf11 library .
    phrase( hexrgb( R , G , B ) , X ) ,
    rect_class(R,G,B,E) ,
    append( C , E , A ) .
% attribute list is a (sub)term of xml_write/3 format .
% result in signature concats the attributes with the tag .

% map all ordered coordinates into svg:rect body .
rect( T ) -->
    { coor( L, T ) , maplist( re( T ) , L , D ) } ,
    D . % this list is xml_write/3 format .

% compose svg tag, attributes, and sub-elements .
element_svg( T ) -->
    { phrase( attributes_svg , A ) ,
      phrase( rect( T ) , E ) } ,
    [ element( svg , A , E ) ] . % this list is xml_write/3 format .

:- use_module(library(sgml)). % for xml_write/3 .
svg( T, Filename ) :-
    open( Filename , write , Stream ) ,
    phrase( element_svg( T ) , D ) ,
    xml_write( Stream, D , [] ) ,
    close( Stream ) .

rect_class( R, G, B, A ) :-
    equilateral( R, G, B ) , A=[class="e"], ! ;
    \+equilateral(R,G,B) , isosceles(R,G,B) , A=[class="i"], ! ;
    A=[] .

equilateral( R, G, B ) :-
    R is G ,
    G is B .

isosceles( R, G, B ) :-
    R is G , ! ;
    R is B , ! ;
    G is B , ! .

retract( X, Y, T ) :-
    rdfs_container_membership_property( S, X ) ,
    rdfs_container_membership_property( P, Y ) ,
    rdf_retractall( S, P, _O@T ) .

% https://dl.acm.org/doi/10.1145/965139.807361 p.14
rn( R, G, B, rgb(RR,GG,BB) ) :-
    var(RR) , var(GG) , var(BB) ,
    integer(R) , integer(G) , integer(B) , ! ,
    RR is R/255 , GG is G/255 , BB is B/255 .

rn( R, G, B, rgb(RR,GG,BB) ) :-
    number(RR) , number(GG) , number(BB) ,
    var(R) , var(G) , var(B) , ! ,
    round( RR*255 , R ) ,
    round( GG*255 , G ) ,
    round( BB*255 , B ) .
% 1) V=0
hsvc( rgb(0,0,0) , hsv(nan,nan,0) ) .
% 3) C=0
hsvc( rgb(R,G,B) , hsv(nan,0,V) ) :-
    number(R) , number(G) , number(B) ,
    var(V) ,
    max_list( [R,G,B] , V ) ,
    min_list( [R,G,B] , X ) ,
    V=X , ! .
% 4) & 6)
hsvc( rgb(R,G,B) , hsv(H,S,V) ) :-
    number(R) , number(G) , number(B) ,
    var(H) , var(S) , var(V) ,
    max_list( [R,G,B] , V ) ,
    min_list( [R,G,B] , X ) ,
    S is (V-X)/V ,
    O is (V-R)/(V-X) ,
    L is (V-G)/(V-X) ,
    Y is (V-B)/(V-X) ,
    hsvd(R,G,B,V,X,O,L,Y,U) ,
    H is U/6.
% 5)
hsvd( R, G,_B, V, X,_O,_L, Y, U ) :- R =V , G =X , ! , U is 5+Y .
hsvd( R, G,_B, V, X,_O, L,_Y, U ) :- R =V , G\=X , ! , U is 1-L .
hsvd(_R, G, B, V, X, O,_L,_Y, U ) :- G =V , B =X , ! , U is 1+O .
hsvd(_R, G, B, V, X,_O,_L, Y, U ) :- G =V , B\=X , ! , U is 3-Y .
hsvd( R, G,_B, V, X,_O, L,_Y, U ) :- G\=V , R =X , ! , U is 3+L .
hsvd( R, G,_B, V, X, O,_L,_Y, U ) :- G\=V , R\=X , ! , U is 5-O .
% p.13
dcltpm( R, G, B, D, C, L, T, P, M ) :-
    min_list( [R,G,B] , X ) ,
    D is X , C is X , L is X ,
    T is R - X ,
    P is G - X ,
    M is B - X .

udcl(X,Y,RR,GG,BB,Tag) :-
    dcltpm(RR,GG,BB,D,C,L,_T,_P,_M) ,
    phrase( hexrgb(D,C,L) , LNSC ) ,
    phrase( hexrgb(RR,GG,BB) , LBSC ) ,
    string_codes( LN , LNSC ) ,
    string_codes( LB , LBSC ) ,
    rdfs_container_membership_property( S , X ) ,
    rdfs_container_membership_property( P , Y ) ,
    rdf_retractall( S , P , LB@Tag ) ,
    rdf_assert( S , P , LN@Tag ) .

q([81]) --> [81]. % Q
s([65]) --> [65]. s([66]) --> [66]. % A B
s([67]) --> [67]. s([68]) --> [68]. % C D
s([69]) --> [69]. s([70]) --> [70]. % E F
s([71]) --> [71]. s([72]) --> [72]. % G H
s([73]) --> [73]. s([74]) --> [74]. % I J
s([75]) --> [75]. s([76]) --> [76]. % K L
s([77]) --> [77]. s([78]) --> [78]. % M N
s([79]) --> [79]. s([80]) --> [80]. % O P
s([81]) --> [81]. s([82]) --> [82]. % Q R
s([83]) --> [83]. s([84]) --> [84]. % S T
s( [ "A" , "B" , "C" , "D" , "E" , "F" ,
     "G" , "H" , "I" , "J" , "K" , "L" ,
     "M" , "N" , "O" , "P" , "Q" , "R" ,
     "S" , "T" ] ) .
a([65]) --> [65]. a([66]) --> [66]. % A B
a([67]) --> [67]. a([68]) --> [68]. % C D
a([69]) --> [69]. a([70]) --> [70]. % E F
a([71]) --> [71]. a([72]) --> [72]. % G H
a([73]) --> [73]. a([74]) --> [74]. % I J
a([75]) --> [75]. a([76]) --> [76]. % K L
a([77]) --> [77]. a([78]) --> [78]. % M N
a([79]) --> [79]. a([80]) --> [80]. % O P
a([81]) --> [81]. a([82]) --> [82]. % Q R
a([83]) --> [83]. a([84]) --> [84]. % S T
a([85]) --> [85]. a([86]) --> [86]. % U V
a([87]) --> [87]. a([88]) --> [88]. % W X
a([89]) --> [89]. a([90]) --> [90]. % Y Z
a( [ "A" , "B" , "C" , "D" , "E" , "F" ,
     "G" , "H" , "I" , "J" , "K" , "L" ,
     "M" , "N" , "O" , "P" , "Q" , "R" ,
     "S" , "T" , "U" , "V" , "W" , "X" ,
     "Y" , "Z" ] ) .
x([88]) --> [88]. % X
xa([88, A]) --> x([88]), a([A]).

n( [ "A" , "B" , "C" , "D" , "E" , "F" , \
     "G" , "H" , "I" , "J" , "K" , "L" ] ) .

m([77]) --> [77]. m([78]) --> [78]. % M N
m([79]) --> [79]. m([80]) --> [80]. % O P
m([81]) --> [81]. m([82]) --> [82]. % Q R
m([83]) --> [83]. m([84]) --> [84]. % S T
m([85]) --> [85]. m([86]) --> [86]. % U V
m([87]) --> [87]. m([88]) --> [88]. % W X
m([89]) --> [89]. m([90]) --> [90]. % Y Z
m( [ "M" , "N" , "O" , "P" , "Q" , "R" ,
     "S" , "T" , "U" , "V" , "W" , "X" ,
     "Y" , "Z" ] ) .
qm([81, M]) --> q([81]), m([M]).

y --> "A" ; "B" ; "C" ; "D" ; "E" ; "F" ;
      "G" ; "H" ; "I" ; "J" ; "K" ; "L" ;
      "M" ; "N" ; "O" ; "P" ; "Q" ; "R" ;
      "S" ; "T" ; "U" ; "V" ; "W" ; "X" ;
      "Y" ; "Z" ; "0" ; "1" ; "2" ; "3" ;
      "4" ; "5" ; "6" ; "7" ; "8" ; "9" .

d([48]) --> [48]. d([49]) --> [49]. % 0 1
d([50]) --> [50]. d([51]) --> [51]. % 2 3
d([52]) --> [52]. d([53]) --> [53]. % 4 5
d([54]) --> [54]. d([55]) --> [55]. % 6 7
d([56]) --> [56]. d([57]) --> [57]. % 8 9
% f([Dt]) --> dcg_basics:digit(Dt). % ; false
e([57]) --> [57] . % 9
ed([57, F, G]) --> e([57]), d([F]), d([G]). % M49 PUA

% https://unicode.org/charts/PDF/UE0000.pdf
ta(   ) --> [917505]. %   &#xE0001;
t([ 32]) --> [917536].%   &#xE0020;
t([ 33]) --> [917537].% ! &#xE0021;
t([ 34]) --> [917538].% " &#xE0022;
t([ 35]) --> [917539].% # &#xE0023;
t([ 36]) --> [917540].% $ &#xE0024;
t([ 37]) --> [917541].% % &#xE0025;
t([ 38]) --> [917542].% & &#xE0026;
t([ 39]) --> [917543].% ' &#xE0027;
t([ 40]) --> [917544].% ( &#xE0028;
t([ 41]) --> [917545].% ) &#xE0029;
t([ 42]) --> [917546].% * &#xE002A;
t([ 43]) --> [917547].% + &#xE002B;
t([ 44]) --> [917548].% , &#xE002C;
t([ 45]) --> [917549].% - &#xE002D;
t([ 46]) --> [917550].% . &#xE002E;
t([ 47]) --> [917551].% / &#xE002F;
t([ 48]) --> [917552].% 0 &#xE0030;
t([ 49]) --> [917553].% 1 &#xE0031;
t([ 50]) --> [917554].% 2 &#xE0032;
t([ 51]) --> [917555].% 3 &#xE0033;
t([ 52]) --> [917556].% 4 &#xE0034;
t([ 53]) --> [917557].% 5 &#xE0035;
t([ 54]) --> [917558].% 6 &#xE0036;
t([ 55]) --> [917559].% 7 &#xE0037;
t([ 56]) --> [917560].% 8 &#xE0038;
t([ 57]) --> [917561].% 9 &#xE0039;
t([ 58]) --> [917562].% : &#xE003A;
t([ 59]) --> [917563].% ; &#xE003B;
t([ 60]) --> [917564].% < &#xE003C;
t([ 61]) --> [917565].% = &#xE003D;
t([ 62]) --> [917566].% > &#xE003E;
t([ 63]) --> [917567].% ? &#xE003F;
t([ 64]) --> [917568].% @ &#xE0040;
t([ 65]) --> [917569].% A &#xE0041;
t([ 66]) --> [917570].% B &#xE0042;
t([ 67]) --> [917571].% C &#xE0043;
t([ 68]) --> [917572].% D &#xE0044;
t([ 69]) --> [917573].% E &#xE0045;
t([ 70]) --> [917574].% F &#xE0046;
t([ 71]) --> [917575].% G &#xE0047;
t([ 72]) --> [917576].% H &#xE0048;
t([ 73]) --> [917577].% I &#xE0049;
t([ 74]) --> [917578].% J &#xE004A;
t([ 75]) --> [917579].% K &#xE004B;
t([ 76]) --> [917580].% L &#xE004C;
t([ 77]) --> [917581].% M &#xE004D;
t([ 78]) --> [917582].% N &#xE004E;
t([ 79]) --> [917583].% O &#xE004F;
t([ 80]) --> [917584].% P &#xE0050;
t([ 81]) --> [917585].% Q &#xE0051;
t([ 82]) --> [917586].% R &#xE0052;
t([ 83]) --> [917587].% S &#xE0053;
t([ 84]) --> [917588].% T &#xE0054;
t([ 85]) --> [917589].% U &#xE0055;
t([ 86]) --> [917590].% V &#xE0056;
t([ 87]) --> [917591].% W &#xE0057;
t([ 88]) --> [917592].% X &#xE0058;
t([ 89]) --> [917593].% Y &#xE0059;
t([ 90]) --> [917594].% Z &#xE005A;
t([ 91]) --> [917595].% [ &#xE005B;
t([ 92]) --> [917596].% \ &#xE005C;
t([ 93]) --> [917597].% ] &#xE005D;
t([ 94]) --> [917598].% ^ &#xE005E;
t([ 95]) --> [917599].% _ &#xE005F;
t([ 96]) --> [917600].% ` &#xE0060;
t([ 97]) --> [917601].% a &#xE0061;
t([ 98]) --> [917602].% b &#xE0062;
t([ 99]) --> [917603].% c &#xE0063;
t([100]) --> [917604].% d &#xE0064;
t([101]) --> [917605].% e &#xE0065;
t([102]) --> [917606].% f &#xE0066;
t([103]) --> [917607].% g &#xE0067;
t([104]) --> [917608].% h &#xE0068;
t([105]) --> [917609].% i &#xE0069;
t([106]) --> [917610].% j &#xE006A;
t([107]) --> [917611].% k &#xE006B;
t([108]) --> [917612].% l &#xE006C;
t([109]) --> [917613].% m &#xE006D;
t([110]) --> [917614].% n &#xE006E;
t([111]) --> [917615].% o &#xE006F;
t([112]) --> [917616].% p &#xE0070;
t([113]) --> [917617].% q &#xE0071;
t([114]) --> [917618].% r &#xE0072;
t([115]) --> [917619].% s &#xE0073;
t([116]) --> [917620].% t &#xE0074;
t([117]) --> [917621].% u &#xE0075;
t([118]) --> [917622].% v &#xE0076;
t([119]) --> [917623].% w &#xE0077;
t([120]) --> [917624].% x &#xE0078;
t([121]) --> [917625].% y &#xE0079;
t([122]) --> [917626].% z &#xE007A;
t([123]) --> [917627].% { &#xE007B;
t([124]) --> [917628].% | &#xE007C;
t([125]) --> [917629].% } &#xE007D;
t([126]) --> [917630].% ~ &#xE007E;
tz() --> [917631].%   &#xE007F;

transpose( o(X,Y) , t(Y,X) ).

metric( t(QY,QX) , m(XY,XX) ) :-
    XY is QY-0.5 ,
    XX is QX-0.5 .

lri( QT , XM ) :-
    coor( XL, QT ) ,
    maplist( transpose, XL, XT ) ,
    maplist( metric, XT, XM ) .

csv( QM , ZZ ) :-
    open( ZZ , write , XZ ) ,
    phrase( csv_(QM), XD ) ,
    string_codes( XS, XD ) ,
    write( XZ, XS ) ,
    close( XZ ) .

csv_([]) --> [] .
csv_([m(QY,QX)|QT]) -->  float(QY), "," , float(QX), "\n" , csv_(QT) .

qaap( QaaZ , QaaP ) :- sum_list( QaaZ , QaaP ) .
qaaz( LRI , QaaZ ) :- maplist( qaak , LRI , QaaZ ) .
qaak( m(XY,XX) , QaaK ) :- QaaK is XY * XX .
qaax( QaaI , QaaX ) :- sum_list( QaaI , QaaX ) .
qaai( LRI , QaaI ) :- maplist( qaat , LRI , QaaI ) .
qaat( m(XY,_X) , QaaT ) :- QaaT is XY .
qaay( QaaJ , QaaY ) :- sum_list( QaaJ , QaaY ) .
qaaj( LRI , QaaJ ) :- maplist( qaao , LRI , QaaJ ) .
qaao( m(_Y,XX) , QaaO ) :- QaaO is XX .
qaas( QaaU , QaaS ) :- sum_list( QaaU , QaaS ) .
qaau( LRI , QaaU ) :- maplist( qaae , LRI , QaaU ) .
qaae( m(XY,_X) , QaaE ) :- QaaE is XY ** 2 .
qaal( LRI , QaaL ) :- length( LRI , QaaL ) .
qaaw( QaaX , QaaY , QaaW ) :- QaaW is QaaX * QaaY .
qaaf( QaaL , QaaP , QaaF ) :- QaaF is QaaL * QaaP .
qaan( QaaF , QaaW , QaaN ) :- QaaN is QaaF - QaaW .
qaad( QaaG , QaaV , QaaD ) :- QaaD is QaaG - QaaV .
qaaa( QaaN , QaaD , QaaA ) :- QaaA is QaaN / QaaD .
qaag( QaaL , QaaS , QaaG ) :- QaaG is QaaL * QaaS .
qaav( QaaX , QaaV ) :- QaaV is QaaX ** 2 .
qaa( LRI , QaaA ) :-
    qaaz( LRI , QaaZ ) ,
    qaai( LRI , QaaI ) ,
    qaaj( LRI , QaaJ ) ,
    qaau( LRI , QaaU ) ,
    qaal( LRI , QaaL ) ,
    qaap( QaaZ , QaaP ) ,
    qaax( QaaI , QaaX ) ,
    qaay( QaaJ , QaaY ) ,
    qaas( QaaU , QaaS ) ,
    qaaf( QaaL , QaaP , QaaF ) ,
    qaaw( QaaX , QaaY , QaaW ) ,
    qaag( QaaL , QaaS , QaaG ) ,
    qaav( QaaX , QaaV ) ,
    qaan( QaaF , QaaW , QaaN ) ,
    qaad( QaaG , QaaV , QaaD ) ,
    qaaa( QaaN , QaaD , QaaA ) .
