#!/usr/bin/env swipl
:- use_module( library( dcg/basics ) ) .
:- use_module( library( lists ) ) .
:- use_module( library( semweb/rdf11 ) ) .
:- use_module( library( semweb/rdf11_containers ) ) .
:- use_module( library( semweb/rdf_portray ) ) .
:- use_module( library( semweb/rdf_zlib_plugin ) ) .
:- rdf_portray_as( prefix:id ) .

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
    { string_codes( '#' , W ) } ,
    { format(string( X ) ,'~|~`0t~16r~2|', [ R ] ) } , % leading zeros hexadecimal
    { format(string( Y ) ,'~|~`0t~16r~2|', [ G ] ) } , % comes from signature
    { format(string( Z ) ,'~|~`0t~16r~2|', [ B ] ) } , % does not work w/ xdigit.
    W, X, Y, Z .

h( S , P , R , G , B ) :-
    rdf( N , M , L@hexrgb ) , % this is a performance quirk, rdf query goes first.
    rdfs_container_membership_property( N , S ) , % constrain subject to _n .
    rdfs_container_membership_property( M , P ) , % constrain predicate to _n .
    string_chars( L , X ) , % this is a workaround for the rdf11 library .
    phrase( hexrgb( R , G , B ) , X ) . % call the dcg parse on the langtag.

t( R , G , B ) -->
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

update(X,Y,RR,GG,BB) :-
    wsr(RR,R) ,
    wsr(GG,G) ,
    wsr(BB,B) ,
    phrase( t(R,G,B) , LNSC ) ,
    phrase( hexrgb(RR,GG,BB) , LBSC ) ,
    string_codes( LN , LNSC ) ,
    string_codes( LB , LBSC ) ,
    rdfs_container_membership_property( S , X ) ,
    rdfs_container_membership_property( P , Y ) ,
    rdf_retractall( S , P , LB@hexrgb ) ,
    rdf_assert( S , P , LN@t ) .

% generate alternatives for rdf coordinates in order .
scan( X , Y ) :-
    findall( Y , ( rdf_predicate( P ) , rdfs_container_membership_property( P , Y ) ) , BY ) ,
    max_list( BY , MY ) , % list of actual axis coordinates BY is potentially unordered.
    findall( X , ( rdf_subject( S ) , rdfs_container_membership_property( S , X ) ) , BX ) ,
    max_list( BX , MX ) , % likewise with BX, take only the maximum for between/3.
    between( 1, MY, Y ) , % the x/y order is sigificant to sorting.
    between( 1, MX, X ) ,
    rdfs_container_membership_property( Pc , Y ) ,
    rdfs_container_membership_property( Sc , X ) ,
    rdf( Sc , Pc , _O@hexrgb ) .

% collect the ordered alternative from scan/2.
coor( L ) :- % the term rewrite 'c(x,y)' is for legibity ;
    findall( c(X,Y) , scan( X, Y ) , L ) . % but it will also pass to re/2 .

attributes_svg -->
    { findall( Y , ( rdf_predicate( P ) , rdfs_container_membership_property( P , Y ) ) , BY ) ,
      max_list( BY , MY )  ,
      findall( X , ( rdf_subject( S ) , rdfs_container_membership_property( S , X ) ) , BX ) ,
      max_list( BX , MX ) ,
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
re( c(Xo,Yo) , element( rect, A , [] ) ) :- % []: there are no sub-elements .
    rdfs_container_membership_property( S , Xo ) , % e.g: ( rdf:'_1' , 1 ) .
    rdfs_container_membership_property( P , Yo ) ,
    rdf( S , P , L@hexrgb ) , % bind L to the coordinated object type rdf:langString .
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
rect -->
    { coor( L ) , maplist( re , L , D ) } ,
    D . % this list is xml_write/3 format .

% compose svg tag, attributes, and sub-elements .
element_svg -->
    { phrase( attributes_svg , A ) ,
      phrase( rect , E ) } ,
    [ element( svg , A , E ) ] . % this list is xml_write/3 format .

:- use_module(library(sgml)). % for xml_write/3 .
svg( Filename ) :-
    open( Filename , write , Stream ) ,
    phrase( element_svg , D ) ,
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

retract( X, Y ) :-
    rdfs_container_membership_property( S, X ) ,
    rdfs_container_membership_property( P, Y ) ,
    rdf_retractall( S, P, _O@hexrgb ) .

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
