-module(polyhedron_test).

-include_lib("eunit/include/eunit.hrl").
-export([go/0]).
-import(polyhedron_aux, [compare_min_max/2,find_min_max/2,normalize/1]).

go() -> start().

start() ->
    [
        compare_min_max_test(),
        find_min_max_test()%,
        %normalize_test()
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compare_min_max_test() ->
    [
        "compare_min_max_tests",
        compare_min_max_one_test(),
        compare_min_max_two_test(),
        compare_min_max_three_test()    
    ].

find_min_max_test() ->
    [
        "find_min_max_tests",
        find_min_max_one_test(),
        find_min_max_two_test(),
        find_min_max_three_test(), 
        find_min_max_four_test()   
    ].

normalize_test() ->
    [
        "normalize_tests",
        normalize_one_test()
    ].

%%%%%%%%%%%%%%%%%%%compare_min_max tests%%%%%%%%%%%%%%%%%%%
compare_min_max_one_test() -> 
    ?assertEqual(
        [{0,0,0},{1,2,3}],
        polyhedron_aux:compare_min_max({1,2,3},[{0,0,0},{0,0,0}]),
        "Test: should replace the maximum"
    ).

compare_min_max_two_test() -> 
    ?assertEqual(
        [{1,2,3},{4,4,4}],
        polyhedron_aux:compare_min_max({1,2,3},[{4,4,4},{4,4,4}]),
        "Test: should replace the minimum"
    ).

compare_min_max_three_test() -> 
    ?assertEqual(
        [{0,0,0},{1,1,1}],
        polyhedron_aux:compare_min_max({0.5,0.5,0.5},[{0,0,0},{1,1,1}]),
        "Test: should change nothing"
    ).

%%%%%%%%%%%%%%%%%%%find_min_max tests%%%%%%%%%%%%%%%%%%%
find_min_max_one_test() -> 
    L = [{-1,-2,-3},{0.5,0.5,0.5},{1,2,3}],
    ?assertEqual(
        [{-1,-2,-3},{1,2,3}],
        polyhedron_aux:find_min_max(L,[{0,0,0},{0,0,0}]),
        "Test: should find the two extremum"
    ).

find_min_max_two_test() -> 
    L = [{0.5,0.5,0.5},{1,2,3}],
    ?assertEqual(
        [{0,0,0},{1,2,3}],
        polyhedron_aux:find_min_max(L,[{0,0,0},{0,0,0}]),
        "Test: should find the maximum"
    ).

find_min_max_three_test() -> 
    L = [{-1,-2,-3},{-0.5,-0.5,-0.5}],
    ?assertEqual(
        [{-1,-2,-3},{0,0,0}],
        polyhedron_aux:find_min_max(L,[{0,0,0},{0,0,0}]),
        "Test: should find the minimum"
    ).

find_min_max_four_test() -> 
    L = [{-0.5,-0.5,-0.5},{0.5,0.5,0.5}],
    ?assertEqual(
        [{-1,-2,-3},{1,2,3}],
        polyhedron_aux:find_min_max(L,[{-1,-2,-3},{1,2,3}]),
        "Test: should change nothing"
    ).

%%%%%%%%%%%%%%%%%%%normalize tests%%%%%%%%%%%%%%%%%%%
normalize_one_test() -> 
    Vrs = [{-3,-3,-3},{-3,-3,3},{-3,3,-3},{-3,3,3},{3,-3,-3},{3,-3,3},{3,3,-3},{3,3,3}],
    ?assertEqual(
        [{-1,-1,-1},{-1,-1,1},{-1,1,-1},{-1,1,1},{1,-1,-1},{1,-1,1},{1,1,-1},{1,1,1}],
        polyhedron_aux:normalize(Vrs),
        "Test: should normalize the cube"
    ).