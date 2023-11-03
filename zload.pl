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
    { var( R ) } ,
    { var( G ) } ,
    { var( B ) } ,
    [ '#' ] ,
    xdigit( D1 ) ,
    xdigit( D2 ) ,
    xdigit( D3 ) ,
    xdigit( D4 ) ,
    xdigit( D5 ) ,
    xdigit( D6 ) ,
    { R is 16 * D1 + D2 } ,
    { G is 16 * D3 + D4 } ,
    { B is 16 * D5 + D6 } .

hexrgb( R , G , B ) -->
    { integer( R ) , R >= 0 , R =< 255 } ,
    { integer( G ) , G >= 0 , G =< 255 } ,
    { integer( B ) , B >= 0 , B =< 255 } ,
    { string_codes( '#' , W ) } ,
    W ,
    { format(string( X ) ,'~|~`0t~16r~2|', [ R ] ) } ,
    X ,
    { format(string( Y ) ,'~|~`0t~16r~2|', [ G ] ) } ,
    Y ,
    { format(string( Z ) ,'~|~`0t~16r~2|', [ B ] ) } ,
    Z .

h( S , P , R , G , B ) :-
    rdf( N , M , L@hexrgb ) ,
    rdfs_container_membership_property( N , S ) ,
    rdfs_container_membership_property( M , P ) ,
    string_chars( L , X ) ,
    phrase( hexrgb( R , G , B ) , X ) .

t( R , G , B ) -->
    { integer( R ) , R >= 0 , R =< 15 } ,
    { integer( G ) , G >= 0 , G =< 15 } ,
    { integer( B ) , B >= 0 , B =< 15 } ,
    { string_codes( '#' , W ) } ,
    W ,
    xinteger( R ) ,
    xinteger( G ) ,
    xinteger( B ) .

wsr( B , N ) :-
    integer( B ) , B >= 0 , B =< 255 ,
    var( N ) ,
    N is div( B , 16 ) ,
    ! .

wsr( B , N ) :-
    var( B ) ,
    integer( N ) , N >=0 , N =< 15 ,
    B is N * 16 + N ,
    ! .

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
