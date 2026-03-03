%%
%%  wpc_radialgrid --
%%
%%     Radial Grid Plugin
%%
%%  Copyright (c) 2026 Hugo Tressous
%%  
%%  Adaptation of wpc_cylinder.erl for radial grid.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wpc_radialgrid).
-export([init/0,menu/2,command/2]).
-include_lib("wings/src/wings.hrl"). 
-import(math, [cos/1,sin/1,pi/0]).

init() -> true.

menu(_, Menu) -> Menu.

command({shape,{radialgrid, Ask}}, St) -> make_cylinder(Ask, St);
command(_, _) -> next.

%%% The rest are local functions.

%%%
%%% Cylinder
%%%

cylinder_dialog() ->
    [{label_column, [
	{?__(1,"Sections"), {text,16,[{key,sections},{range,{3,infinity}}]}},
    {?__(2,"Slices"), {text,8,[{key,slices},{range,{1,infinity}}]}},
	{" ", separator},
	{?__(3,"Radius"), {text,5.0,[{key,radius},{range,{0.0,infinity}}]}},
    {?__(4,"Thickness"), {text,0.2,[{key,thickness},{range,{0.0,infinity}}]}}]
     },
     wings_shapes:transform_obj_dlg()].

make_cylinder(Arg, St) when is_atom(Arg) ->
    Qs = cylinder_dialog(),
    Label = ?__(1,"RadialGrid Options"),
    wings_dialog:dialog_preview({shape,radialgrid}, Arg, Label, Qs, St);
make_cylinder(Arg, St) ->
    ArgDict = dict:from_list(Arg),
    Sections = dict:fetch(sections, ArgDict),
    Slices = dict:fetch(slices, ArgDict),
    Radius = dict:fetch(radius, ArgDict),
    Thickness = dict:fetch(thickness, ArgDict),
    Modify = [{dict:fetch(rot_x, ArgDict), dict:fetch(rot_y, ArgDict), dict:fetch(rot_z, ArgDict)},
	      {dict:fetch(mov_x, ArgDict), dict:fetch(mov_y, ArgDict), dict:fetch(mov_z, ArgDict)},
	      dict:fetch(ground, ArgDict)],
    make_cylinder(Sections, Slices, Radius, Radius, Radius, Radius, Thickness, Modify, St).

%%%
%%% Cylinder
%%%

make_cylinder(Sections, Slices, TopX, TopZ, BotX, BotZ, Height, [Rot, Mov, Ground], St) ->
    Vs0 = cylinder_verts(Sections, TopX, TopZ, BotX, BotZ, Height),
    Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
    Fs = cylinder_faces(Sections),
    % Pole
    We0 = wings_we:build(Fs, Vs),
    {We1, _NewFaces} = wings_face_cmd:pole(gb_sets:from_list([0, 1]), We0),
    % Cut
    Edges = gb_sets:from_list(get_edges(Sections)),
    {We2, NewVs} = cut_edges(Edges, Slices, We1),
    % Connect
    We3 = wings_vertex_cmd:connect(gb_sets:from_list(NewVs), We2),
    wings_obj:new(?__(2,"Radial Grid"), We3, St).

cylinder_verts(Sections, TopX, TopZ, BotX, BotZ, Height) ->
    YAxis = Height/2,
    Delta = pi()*2/Sections,
    Rings = lists:seq(0, Sections-1),
    Top = ring_of_verts(Rings, Delta, YAxis, TopX, TopZ, 0.0),
    Bottom = ring_of_verts(Rings, Delta, -YAxis, BotX, BotZ, 0.0),
    Top ++ Bottom.

cylinder_faces(N) ->
    Ns =lists:reverse(lists:seq(0, N-1)),
    Upper= Ns,
    Lower= lists:seq(N, N+N-1),
    Sides= [[I, (I+1) rem N, N + (I+1) rem N, N + I] || I <- Ns],
    [Upper, Lower | Sides].


ring_of_verts(Rings, Delta, YAxis, XAxis, ZAxis, Offset) ->
    [{XAxis*cos(Offset+I*Delta), YAxis, ZAxis*sin(Offset+I*Delta)} || I <- Rings].

% gets the edges newly created by the pole command
get_edges(Sections) -> 
    From = 3 * (Sections + 1),
    Count = Sections * 2 - 1,
    To = From + 2 * Count,
    lists:seq(From, To, 2).

% function copied from wings_edge_cmd.erl and edited to get the new vertices
cut_edges(Edges, N, We0) ->
    gb_sets:fold(fun(Edge, {W0, AccVs}) ->
			 {We, _} = wings_edge:cut(Edge, N, W0),
			 NewVs = wings_we:new_items_as_ordset(vertex, W0, We),
			 {We, AccVs ++ NewVs}
		 end, {We0, []}, Edges).