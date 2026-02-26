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

menu({shape}, []) ->
    menu();
menu({shape}, Menu) ->
    menu()++Menu;
menu(_, Menu) -> Menu.

menu() ->
    [{radialgrid(),radialgrid,?__(2,"Create a radial grid"),[option]}].

radialgrid() ->
    ?__(1,"Radial Grid").

command({shape,{radialgrid, Ask}}, St) -> make_cylinder(Ask, St);
command(_, _) -> next.

%%% The rest are local functions.

%%%
%%% Cylinder
%%%

cylinder_dialog() ->
    [{label_column, [
	{?__(1,"Sections"), {text,16,[{key,sections},{range,{3,infinity}}]}},
    %{?__(2,"Slices"), {text,8,[{key,slices},{range,{1,infinity}}]}},
	{" ", separator},
	{?__(3,"Radius"), {text,5.0,[{key,radius},{range,{0.0,infinity}}]}},
    {?__(4,"Thickness"), {text,0.2,[{key,thickness},{range,{0.0,infinity}}]}}]
     },
     wings_shapes:transform_obj_dlg()].

make_cylinder(Arg, St) when is_atom(Arg) ->
    Qs = cylinder_dialog(),
    Label = ?__(1,"RadialGrid Options"),
    wings_dialog:dialog_preview({shape,radialgrid}, Arg, Label, Qs, St);
make_cylinder(Arg, _St) ->
    ArgDict = dict:from_list(Arg),
    Sections = dict:fetch(sections, ArgDict),
    Radius = dict:fetch(radius, ArgDict),
    Thickness = dict:fetch(thickness, ArgDict),
    Modify = [{dict:fetch(rot_x, ArgDict), dict:fetch(rot_y, ArgDict), dict:fetch(rot_z, ArgDict)},
	      {dict:fetch(mov_x, ArgDict), dict:fetch(mov_y, ArgDict), dict:fetch(mov_z, ArgDict)},
	      dict:fetch(ground, ArgDict)],
    make_cylinder(Sections, Radius, Radius, Radius, Radius, Thickness, Modify).


%make_cylinder(Arg, St) when is_atom(Arg) ->
%    Qs = cylinder_dialog(),
%    Label = ?__(1,"Radial Grid Options"),
%    wings_dialog:dialog_preview({shape,cylinder}, Arg, Label, Qs, St);
%make_cylinder(Arg, _St) ->
%    ArgDict = dict:from_list(Arg),
%    Sections = dict:fetch(sections, ArgDict),
%    %Slices = dict:fetch(slices, ArgDict),
%    Radius = dict:fetch(radius, ArgDict),
%    Thickness = dict:fetch(thickness, ArgDict),
%    Modify = [{dict:fetch(rot_x, ArgDict), dict:fetch(rot_y, ArgDict), dict:fetch(rot_z, ArgDict)},
%	      {dict:fetch(mov_x, ArgDict), dict:fetch(mov_y, ArgDict), dict:fetch(mov_z, ArgDict)},
%	      dict:fetch(ground, ArgDict)],
%    make_cylinder(Sections, Radius, Radius, Radius, Radius, Thickness, Modify).



%%%
%%% Cylinder
%%%

make_cylinder(Sections, TopX, TopZ, BotX, BotZ, Height, [Rot, Mov, Ground]) ->
    Vs0 = cylinder_verts(Sections, TopX, TopZ, BotX, BotZ, Height),
    Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
    Fs = cylinder_faces(Sections),
    {new_shape,?__(2,"Radial Grid"),Fs,Vs}.

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




    