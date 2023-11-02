#!/usr/bin/env swipl
:- use_module( library( dcg/basics ) ) .
:- use_module( library( lists ) ) .
:- use_module( library( semweb/rdf11 ) ) .
:- use_module( library( semweb/rdf11_containers ) ) .
:- use_module( library( semweb/rdf_portray ) ) .
:- use_module( library( semweb/rdf_zlib_plugin ) ) .
:- rdf_portray_as( prefix:id ) .
:- set_prolog_flag(double_quotes, chars).

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
