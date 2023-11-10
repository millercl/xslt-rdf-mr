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

update(S,P,RB,GB,BB) :-
    wsr(RB,RN) ,
    wsr(GB,GN) ,
    wsr(BB,BN) ,
    phrase( t(RN,GN,BN) , LNSC ) ,
    phrase( hexrgb(RB,GB,BB) , LBSC ) ,
    string_codes( LN , LNSC ) ,
    string_codes( LB , LBSC ) ,
    rdfs_container_membership_property( N , S ) ,
    rdfs_container_membership_property( M , P ) ,
    rdf_assert( N , M , LN@t ) ,
    rdf_retractall( N , M , LB@hexrgb ) .

scan( X , Y ) :-
    findall( Y , ( rdf_predicate( P ) , rdfs_container_membership_property( P , Y ) ) , BY ) ,
    max_list( BY , MY ) ,
    findall( X , ( rdf_subject( S ) , rdfs_container_membership_property( S , X ) ) , BX ) ,
    max_list( BX , MX ) ,
    between( 1, MY, Y ) ,
    between( 1, MX, X ) .

coor( L ) :-
    findall( c(X,Y) , scan( X, Y ) , L ) .

:- use_module(library(sgml)).

save( D ) :-
    open( "jb.svg" , write , Stream ) ,
    xml_write( Stream, D , [] ) ,
    close( Stream ) .

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
      xmlns='http://www.w3.org/2000/svg' ] .

:- use_module( library( apply ) ) .

re( c(Xo,Yo) , element( rect, A , [] ) ) :-
    rdfs_container_membership_property( S , Xo ) ,
    rdfs_container_membership_property( P , Yo ) ,
    rdf( S , P , L@hexrgb ) ,
    string_concat( "fill:" , L , Style ) ,
    Xk is Xo-1 ,
    Yk is Yo-1 ,
    A=[ style=Style , width=1 , height=1 , x=Xk , y=Yk ] .

rect -->
    { coor( L ) , maplist( re , L , D ) } ,
    D .

element_svg -->
    { phrase( attributes_svg , A ) ,
      phrase( rect , E ) } ,
    [ element( svg , A , E ) ] .
