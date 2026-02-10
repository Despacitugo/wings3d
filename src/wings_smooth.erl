
-module(wings_smooth).
-export([smooth/1]).

-include("wings.hrl").
-import(lists, [foldl/3,reverse/1]).


%print_list(List, String) ->
%    io:format("~s: ",[String]),
%    print_list(List).
%
%print_list([]) ->
%    io:format("~n"),
%    ok;
%print_list([Head | Tail]) ->
%    io:format("~w-", [Head]),
%    print_list(Tail).

%% Return a sorted list of vertex
sorted_vertex_list(Edges,We) ->
    EdgesList = gb_sets:to_list(Edges),
    add_next_vertex(EdgesList, We,[]).

%% Adds the next vertex AT THE END of the Acc
add_next_vertex([], _We, Acc) -> Acc;
add_next_vertex([H|T], We, []) ->
    Vs = wings_vertex:from_edges(gb_sets:from_list([H]),We),
    add_next_vertex(T, We, Vs);
add_next_vertex(Edges, We, Acc) -> 
    Prev = lists:last(Acc),
    Result = find_next_vertex(Edges,Prev,We),
    case Result of
        {Edge, V} -> 
            L = lists:delete(Edge, Edges),
            add_next_vertex(L, We, Acc++[V]);
        _ -> add_vertex_reverse(Edges, We, Acc)
    end.

%% like add_next_vertex but it adds vertices IN FRONT of Acc
add_vertex_reverse([], _We, Acc) -> Acc;
add_vertex_reverse(Edges, We, [Next|_]=Acc) ->
    Result = find_next_vertex(Edges,Next,We),
    case Result of
        {Edge, V} -> 
            L = lists:delete(Edge, Edges),
            add_vertex_reverse(L, We, [V]++Acc);
        _ -> Acc
    end.

%% Find an Edge from Edges which has vertex V, returns the Edge and the other vertex
find_next_vertex([]=_Egdes, _V, _We) -> nil;
find_next_vertex([Edge|T]=_Egdes, V, We) -> 
    Vs = wings_vertex:from_edges(gb_sets:from_list([Edge]),We),
    case Vs of 
        [V0, V1] when V0 =:= V -> 
            {Edge,V1};
        [V0, V1] when V1 =:= V -> 
            {Edge,V0};
        _ -> find_next_vertex(T,V,We)
    end.

%% Function to smooth edges
smooth(St0) ->
    wings_sel:map(fun(Edges, We0) ->
        Vs = sorted_vertex_list(Edges, We0),

        case lists:last(Vs) =:= lists:nth(1,Vs) of
            true -> smooth_loop(Vs, We0); % It's a loop
            _ -> smooth_segment(Vs, We0) % It's not a loop
        end
    end, St0).

%% Calculates the new position of the Current vertex based on the Previous and Next vertices
new_position(Previous,Current,Next, Vtab) ->
    PosP = array:get(Previous, Vtab),
    PosC = array:get(Current, Vtab),
    PosN = array:get(Next, Vtab),
    e3d_vec:add(e3d_vec:add(e3d_vec:divide(PosP,4), e3d_vec:divide(PosN,4) ), e3d_vec:divide(PosC,2)).

%% Smooth function for a loop
smooth_loop([], We) -> We;
smooth_loop([H|T]=Vs, #we{vp=Vtab0}=We) ->
    %print_list(Vs, "Vs")
    Fist = lists:nth(2,Vs),
    Vtab = smooth_loop_1(T, Vtab0, H, Fist, Vtab0),
    We#we{vp=Vtab}.

smooth_loop_1([],_,_,_,Acc) -> Acc; % never happens
smooth_loop_1([Curr], Vtab, Prev, Next, Acc) ->
    Pos = new_position(Prev, Curr, Next, Vtab),
    array:set(Curr, Pos, Acc);
smooth_loop_1([Curr,Next|T], Vtab, Prev, First, Acc) ->
    Pos = new_position(Prev, Curr, Next, Vtab),
    NewAcc = array:set(Curr, Pos, Acc),
    smooth_loop_1([Next]++T, Vtab, Curr, First, NewAcc).
    
smooth_segment([],We) -> We;
smooth_segment([H|T], #we{vp=Vtab0}=We) ->
    %print_list(Vs, "Vs")
    Vtab = smooth_segment_1(T, Vtab0, H, Vtab0),
    We#we{vp=Vtab}.

%% Smooth function for a segment
smooth_segment_1([],_,_,Acc) -> Acc; % never happens
smooth_segment_1([_],_,_,Acc) -> Acc; % never happens
smooth_segment_1([Curr,Next], Vtab, Prev, Acc) ->
    Pos = new_position(Prev, Curr, Next, Vtab),
    array:set(Curr, Pos, Acc);
smooth_segment_1([Curr,Next|T], Vtab, Prev, Acc) ->
    Pos = new_position(Prev, Curr, Next, Vtab),
    NewAcc = array:set(Curr, Pos, Acc),
    smooth_segment_1([Next]++T, Vtab, Curr, NewAcc).