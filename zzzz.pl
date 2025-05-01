#!/usr/bin/env swipl
:- use_module( library( dcg/basics ) ) .
:- use_module( library( lists ) ) .
:- use_module( library( semweb/rdf11 ) ) .
:- use_module( library( semweb/rdf11_containers ) ) .
:- use_module( library( semweb/rdf_portray ) ) .
:- use_module( library( semweb/rdf_zlib_plugin ) ) .
:- use_module( library( apply ) ) . % for maplist/3
:- use_module( library( sgml ) ) . % for xml_write/3
:- use_module( library( xpath ) ) . % xpath_chk/3
:- rdf_portray_as( prefix:id ) .
:- rdf_register_prefix( tiff, 'http://ns.adobe.com/tiff/1.0/' ) .
:- rdf_meta rdfs_cmp( r, ? ) .
rdfs_cmp( QR , XN ) :- rdfs_container_membership_property( QR , XN ) .

load( File ) :-
    file_base_name( File, Basename ) ,
    file_name_extension( Name, ".xml", Basename ) ,
    rdf_load( File, [ base_uri( Name ), silent( false ) ] ) .

zz :-
    current_prolog_flag( argv , AAV ) ,
    nth1( 1 , AAV , qof ) ,
    qofz( _AA ) .

:- initialization( zz, main ) .

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

qzba( XX , XY ) :- qzba( XX , XY , zxx ) .
qzba( XX , XY , XT ) :- qzba( XX , XY , XT , _XG ) .
qzba( XX , XY , XT , XG ) :-
    qzam( XT , QXT ) ,
    qzav( XG , QXG ) ,
    findall( XY ,
             ( rdf( _XXS , QP , _XXO , QXG ) ,
               rdfs_container_membership_property( QP , XY ) ) ,
             QY ) ,
    max_list( QY , QYZ ) ,
    findall( XX ,
             ( rdf( QS , _QQP , _QQO , QXG ) ,
               rdfs_container_membership_property( QS , XX ) ) ,
             QX ) ,
    max_list( QX , QXZ ) ,
    between( 1, QYZ, XY ) ,
    between( 1, QXZ, XX ) ,
    rdfs_container_membership_property( QP , XY ) ,
    rdfs_container_membership_property( QS , XX ) ,
    rdf( QS , QP , _QO@QXT , QXG ) .
qzca( QXZ , QYZ , XQ ) :- qzca( QXZ , QYZ , XQ , _XG ) .
qzca( QXZ , QYZ , XQ , XG ) :-
    number( QXZ ) ,
    number( QYZ ) ,
    qzav( XG , QXG ) ,
    between( 1, QYZ, XY ) ,
    between( 1, QXZ, XX ) ,
    rdfs_container_membership_property( QP , XY ) ,
    rdfs_container_membership_property( QS , XX ) ,
    rdf( XQ , rdf:subject , QS , QXG ) ,
    rdf( XQ , rdf:predicate , QP , QXG ) .
qzda( QXZ , QYZ , XG ) :-
    qztf( QXZ , QYZ , XG ) , ! .
qzda( QXZ , QYZ , XG ) :-
    qzav( XG , QXG ) ,
    findall( XY ,
             ( rdf( _XXQ , rdf:predicate , QP , QXG ) ,
               rdfs_container_membership_property( QP , XY ) ) ,
             QY ) ,
    sort( QY , QYA ) ,
    reverse( QYA , [ QYZ | _QTY ] ) ,
    findall( XX ,
             ( rdf( _QXQ , rdf:subject , QS , QXG ) ,
               rdfs_container_membership_property( QS , XX ) ) ,
             QX ) ,
    sort( QX , QXA ) ,
    reverse( QXA , [ QXZ | _QTX ] ) .
qztf( QXZ , QYZ , XG ) :-
    qzav( XG , QXG ) ,
    rdf( _, tiff:'ImageLength', QYZ^^xsd:int , QXG ) ,
    rdf( _, tiff:'ImageWidth', QXZ^^xsd:int , QXG ) .

% collect the ordered alternative from qzba/3.
coor( L, T ) :- % the term rewrite 'o(x,y)' is for legibity ;
    findall( o(X,Y) , qzba( X, Y, T ) , L ) . % but it will also pass to re/3 .

qzbb( XT , XG , XS ) :-
    qzam( XT , QXT ) ,
    qzav( XG , QXG ) ,
    findall(
        element( xo , [ qx=QX , qy=QY ] , [] ) ,
        qzba( QX , QY , QXT , QXG ) ,
        QS ) ,
    XS = [ element( qzbb , [] , QS ) ] .
qzbb( XT , XG , XS , ZZ ) :-
    qzbb( XT , XG , XS ) ,
    open( ZZ , write , ZZZ ) ,
    xml_write( ZZZ , XS , [] ) ,
    close( ZZZ ) .
qzcb( XGS , QTG ) :-
    qzda( X , Y , XGS ) ,
    findall( XQ , qzca( X , Y , XQ , XGS ) , QQ ) ,
    rdf_assert_seq( QTG , QQ , QTG ) .
qzav( AA , ZZ ) :-
    integer( AA ) , ! , format( atom( ZZ ) , '~|~`0t~d~6|' , AA ) .
qzav( AA , ZZ ) :-
    atom( AA ) , ! , ZZ = AA .
qzav( AA , ZZ ) :-
    string( AA ) , ! , atom_string( ZZ , AA ) .
qzav( AA , ZZ ) :-
    var( AA ) , ! , ZZ = AA .
qzam( AA , ZZ ) :-
    integer( AA ) , ! , format( atom( ZZ ) , '~|~`0t~d~3|' , AA ) .
qzam( AA , ZZ ) :-
    atom( AA ) , ! , ZZ = AA .
qzam( AA , ZZ ) :-
    string( AA ) , ! , atom_string( ZZ , AA ) .
qzam( AA , ZZ ) :-
    var( AA ) , ! , ZZ = AA .

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
qabb( QabC , QaaD , QabB ) :- QabB is QabC / QaaD .
qabc( QabH , QabQ , QabC ) :- QabC is QabH - QabQ .
qabh( QaaS , QaaY , QabH ) :- QabH is QaaS * QaaY .
qabq( QaaP , QaaX , QabQ ) :- QabQ is QaaP * QaaX .
qab( LRI , QabB ) :-
    qaai( LRI , QaaI ) ,
    qaaj( LRI , QaaJ ) ,
    qaal( LRI , QaaL ) ,
    qaau( LRI , QaaU ) ,
    qaaz( LRI , QaaZ ) ,
    qaas( QaaU , QaaS ) ,
    qaap( QaaZ , QaaP ) ,
    qaax( QaaI , QaaX ) ,
    qaay( QaaJ , QaaY ) ,
    qaag( QaaL , QaaS , QaaG ) ,
    qaav( QaaX , QaaV ) ,
    qaad( QaaG , QaaV , QaaD ) ,
    qabq( QaaP , QaaX , QabQ ) ,
    qabh( QaaS , QaaY , QabH ) ,
    qabc( QabH , QabQ , QabC ) ,
    qabb( QabC , QaaD , QabB ) .

:- rdf_register_prefix( qka, 'http://id.loc.gov/vocabulary/iso639-1' ) .
:- rdf_register_prefix( qua, 'http://www.loc.gov/mads/rdf/v1#' ) .
:- rdf_register_prefix( quy, 'http://www.w3.org/2004/02/skos/core#' ) .
qka :-
    rdf_retractall( qka:'' , rdf:type , qua:'MADSScheme' ) ,
    rdf_retractall( qka:'' , qua:hasTopMemberOfMADSScheme, _O ) ,
    bagof( XS , rdf_subject( XS ) , XL ) ,
    partition( qkas , XL , _QS , _S ) ,
    true .
qkas( QS ) :- \+ rdf_equal( QS , rdf:subject ) .
qkap( QP ) :- \+ rdf_equal( QP , rdf:predicate ) .
qkar( QS ) :- rdf_assert_bag( rdf:subject , QS ) .
qkao( QP ) :- rdf_assert_bag( rdf:predicate , QP ) .
qkad --> "|" .
qkab( QB ) --> blanks , nonblanks(QB) , blanks .
qkac( [ QH | QT ] ) --> string( XH ) , { phrase(qkab(QH),XH) } , qkad , qkac( QT ) , ! .
qkac( [QC] ) -->  qkab( QC ) , eos , ! .
qkay( QO, XA ) :-
    string_codes( QO , XL ) ,
    phrase( qkac( XC ) , XL ) ,
    maplist( string_codes , XA , XC ) .
qkaw( QO , XX ) :-
    qkay( QO , XS ) ,
    member( XX , XS ) ,
    \+ re_match( "[[:alnum:]]" , XX ) .


qova( QS , QG ) :-
    rdf( QS , rdf:type , rdf:'Statement' , QG ) .
qovb( QG , XL ) :-
    bagof( QS , qova( QS , QG ) , XL ) .
qovt216( QN , QT , QG ) :-
    rdf( QN , rdf:type , rdf:'Statement' , QG ) ,
    rdf( QN , rdf:subject , XS , QG ) ,
    rdf( QN , rdf:predicate , XP , QG ) ,
    rdf( QN , rdf:object , XO@QT , QG ) ,
    string_chars( XO , XK ) ,
    phrase( hexrgb( XR , XG , XB ) , XK ) ,
    XR = XB ,
    XG = XB ,
    XG >= 216 ,
    rdf_assert( XS , XP , "#ffffff"@QT , QN ) .
qovt216( QN , QT , QG ) :-
    rdf( QN , rdf:type , rdf:'Statement' , QG ) ,
    rdf( QN , rdf:subject , XS , QG ) ,
    rdf( QN , rdf:predicate , XP , QG ) ,
    rdf( QN , rdf:object , XO@QT , QG ) ,
    string_chars( XO , XK ) ,
    phrase( hexrgb( XR , XG , XB ) , XK ) ,
    XR = XB ,
    XG = XB ,
    XG < 216 ,
    rdf_assert( XS , XP , "#000000"@QT , QN ) .
qovc( QT , QG ) :-
    once( foreach( qova( QN , QG ) , qovt216( QN , QT , QG ) ) ) .
qovd( XS , XP , QT , QG ) :-
    rdf( XS , XP , _XO@QT , QG ) ,
    rdfs_container_membership_property( XS ) ,
    rdfs_container_membership_property( XP ) .
qove( QS , QP , QT , QG ) :-
    rdf_retractall( QS , QP , _XO@QT , QG ) .
qovf( QT , QG ) :-
    foreach( qovd( XS, XP , QT , QG ) , qove( XS , XP , QT , QG ) ) .
qovg( QS , QT , QG ) :-
    rdf_retractall( QS , rdf:object , _XO@QT , QG ) ,
    rdf_retractall( QS , rdf:predicate , _XP , QG ) ,
    rdf_retractall( QS , rdf:subject , _XS , QG ) ,
    rdf_retractall( QS , rdf:type , rdf:'Statement' ) .
qovh( QT , QG ) :-
    foreach( qova( XS , QG ) , qovg( XS , QT , QG ) ) .
qovi( QT , QN , QD ) :-
    rdf( XS , XP , XO@QT , QN ) ,
    rdf_assert( XS , XP , XO@QT , QD ) .
qovj( QT , QD ) :-
    foreach( qova( XS , QD ) , qovi( QT , XS , QD ) ) .
qovk( QN ) :-
    rdf_retractall( _S , _P , _O , QN ) .
qovl( QD ) :-
    foreach( qova( XS , QD ) , qovk( XS ) ) .
qovm( QT , QG ) :-
    qovc( QT , QG ) ,
    qovf( QT , QG ) ,
    qovj( QT , QG ) ,
    qovl( QG ) ,
    qovh( QT , QG ) .

qisa( ZXX , _939 , QS , QP ) :-
    rdf( QS , QP , _QO@ZXX ) ,
    rdfs_container_membership_property( QS ) ,
    rdfs_container_membership_property( QP ) ,
    \+ rdf( QS , QP , _XO@_939 ) .
qisb( ZXX , _939 ) :-
    foreach(
        qisa( ZXX , _939 , QS , QP ) ,
        rdf_retractall( QS , QP , _QO@ZXX )
    ).
qisc( _939 ) :-
    rdf_retractall( _XS , _XP , _XO@_939 ) .
qisd :-
    load( "j239367.xml" ) ,
    load( "qis-939-x-ff0000.xml" ) ,
    qisb( zxx , '939' ) ,
    qisc( '939' ) .
qise( XR , XG , XB ) :-
    rdf( _XS , _XP , XO@zxx ) ,
    string_chars( XO , XK ) ,
    phrase( hexrgb( XR , XG , XB ) , XK ) .
qisf( QNR , QNG , QNB , QXR, QXG, QXB ) :-
    findall( XR , qise( XR , XG , XB ) , XXR ) ,
    findall( XG , qise( XR , XG , XB ) , XXG ) ,
    findall( XB , qise( XR , XG , XB ) , XXB ) ,
    min_list( XXR , QNR ) ,
    min_list( XXG , QNG ) ,
    min_list( XXB , QNB ) ,
    max_list( XXR , QXR ) ,
    max_list( XXG , QXG ) ,
    max_list( XXB , QXB ) .

qpad( XT , XD ) :-
    once( rdf( QS , QP , QO@XT , XD ) ) ,
    uuid( XB , [] ) ,
    rdf_assert( QS , QP , QO@XT , XB ) ,
    rdf_retractall( QS , QP , QO@XT , XD ) .
qpae( XD , QQ ) :-
    findall(
        XNG ,
        ( rdf( _QS , _QP , _QO , XNG ) ,
          \+ rdf_equal( XNG , XD ) ) ,
        QQ ) .
qpah( XSN, XPN, XT, XD , QS , QP ) :-
    rdf( QS , QP , _QO@XT , XD ) ,
    rdfs_container_membership_property( QS , QSN ) ,
    rdfs_container_membership_property( QP , QPN ) ,
    QSN < XSN + 2 ,
    QSN > XSN - 2 ,
    QPN < XPN + 2 ,
    QPN > XPN - 2 .
qpai( XSN, XPN, XT, XD , QOO) :-
    findall(
        qo( QS , QP ) ,
        qpah( XSN, XPN, XT, XD , QS , QP ) ,
        QOO ) .
qpaj( XT, XD, XQO , XQ ) :-
    foreach(
        member( qo( XXS , XXP ) , XQO ) ,
        qpak( XXS , XXP , XT , XD , XQ ) ) .
qpak( XS , XP , XT , XD , XQ ) :-
    rdf( XS , XP , QO@XT , XD ) ,
    rdf_assert( XS , XP , QO@XT , XQ ) ,
    rdf_retractall( XS , XP , QO@XT , XD ) .
qpal( XT , XQ , QZ ) :-
    findall(
        xo( XXS , XXP ) ,
        ( rdf( XS , XP , _XO@XT , XQ ) ,
          rdfs_container_membership_property( XS , XXS ) ,
          rdfs_container_membership_property( XP , XXP ) ) ,
        QZ ) .
qpam( XT , XD , XQ , XZ , QOA ) :-
    setof(
        qoa( xo(XXS,XXP) , QOO ) ,
        ( member(xo(XXS,XXP),XZ) ,
          qpai( XXS, XXP, XT, XD, QOO ) ,
          qpaj( XT, XD, QOO, XQ ) ) ,
        QOA ) .
qpan( QOA , QOI ) :-
    partition( qpao, QOA, QOI, _QOO ) .
qpao( qoa(xo(_QS,_QP),XN) ) :-
    length(XN,QN) ,
    QN > 0 .

qofa(Qtd) :-
    load_xml("qof/qtd.xml", Qtd, [space(remove)]).
qofb(Qtd, Qt) :-
    xpath_chk(Qtd,
              //tree/directory(1),
              element(directory, _Qn, Qt)).
qofc(element(file, [name=Qn], []), Xb) :-
    file_name_extension(Xb, _Qx, Qn).
qofd(Zinh) :-
    load_xml("qof/zinh-xb.svg", Zinh, [space(remove)]).
qofe(Zz, element(Xe, Xa, Xd), element(Xe, Xa, Qz)) :-
    dict_create(Xo, zsym, Xa),
    get_dict(id, Xo, Qo),
    Qo\=qo,
    !,
    maplist(qofe(Zz), Xd, Qz).
qofe(Zz, element(Xe, Xa, _Xd), element(Xe, Xa, [Zz])) :-
    dict_create(Xo, zsym, Xa),
    get_dict(id, Xo, Qo),
    Qo=qo,
    !.
qofe(_Zz, Qm, Qm) :-
    atom(Qm),
    !.
qofe(Zz, element(Xe, Xa, Xd), element(Xe, Xa, Qz)) :-
    dict_create(Xo, zsym, Xa),
    \+ get_dict(id, Xo, _Qo),
    !,
    maplist(qofe(Zz), Xd, Qz).
qoff(Qs) :-
    qofd([Zinh]),
    qofe(Qs, Zinh, Qt),
    qofh(Qs, Qp),
    open(Qp, write, Xy),
    xml_write(Xy, Qt, []),
    close(Xy).
qofg(Qs) :-
    qofh(Qs, Xae),
    qofi(Qs, Xze),
    atom_concat('inkscape -o ', Xze, Xmz),
    atom_concat(Xmz, ' ', Xoz),
    atom_concat(Xoz, Xae, Qnk),
    shell(Qnk).
qofh(Qs, Qu) :-
    atom_concat('qof/', Qs, Xd),
    atom_concat(Xd, '.svg', Qu).
qofi(Qs, Qu) :-
    atom_concat('qof/', Qs, Xd),
    atom_concat(Xd, '.pdf', Qu).
qofj(Xc) :-
    maplist(qofi, Xc, Xi),
    atomics_to_string(Xi, ' ', Qr),
    string_concat('pdfunite ', Qr, Xy),
    string_concat(Xy, " qof/qpu.pdf", Qpu),
    shell(Qpu).
qofz(Xc) :-
    qofa(Qtd),
    qofb(Qtd, Xb),
    maplist(qofc, Xb, Xc),
    maplist(qoff, Xc),
    maplist(qofg, Xc),
    qofj(Xc).
