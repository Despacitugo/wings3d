%%
%%  wpc_cylinder --
%%
%%     Cylinder, Gear and Tube Plugin
%%
%%  Copyright (c) 2003-2012 Anthony D'Agostino
%%  
%%  Former wpc_gear.erl adapted to single preview dialog.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wpc_cylinder).
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
    [{cylinder(),cylinder,?__(2,"Create a cylinder"),[option]}].

cylinder() ->
    cylinder_type(cylinder).

cylinder_type(cylinder) -> ?__(1,"Cylinder");
cylinder_type(tube) -> ?__(2,"Tube");
cylinder_type(gear) -> ?__(3,"Gear");
cylinder_type(pie) -> ?__(4,"Pie").

command({shape,{cylinder, Ask}}, St) -> make_cylinder(Ask, St);
command(_, _) -> next.

%%% The rest are local functions.

%%%
%%% Cylinder
%%%

cylinder_dialog() ->
    Hook = fun(Var, Val, Sto) ->
	case Var of
	    cylinder_type ->
		wings_dialog:enable(thickness, (Val=/=cylinder) and (Val=/=pie) , Sto),
		wings_dialog:enable(angle_offset, Val==pie, Sto),
		wings_dialog:enable(degrees, Val==pie, Sto);
	    _ -> ok
	end
    end,

    [{label_column, [
	{?__(1,"Sections"), {text,16,[{key,sections},{range,{3,infinity}}]}},
	{?__(2,"Height"), {text,2.0,[{key,height},{range,{0.0,infinity}}]}},
	{" ", separator},
	{?__(3,"Top X Radius"), {text,1.0,[{key,top_x},{range,{0.0,infinity}}]}},
	{?__(4,"Top Z Radius"), {text,1.0,[{key,top_z},{range,{0.0,infinity}}]}},
	{" ", separator},
	{?__(5,"Bottom X Radius"), {text,1.0,[{key,bottom_x},{range,{0.0,infinity}}]}},
	{?__(6,"Bottom Z Radius"), {text,1.0,[{key,bottom_z},{range,{0.0,infinity}}]}},
    {" ", separator},
    {?__(7,"Slice"), {text,1.0,[{key,slice},{range,{1.0,infinity}}]}}]   
     },
     {hradio, [
	{cylinder_type(cylinder),cylinder},
	{cylinder_type(tube),tube},
	{cylinder_type(gear),gear},
	{cylinder_type(pie),pie}],
		cylinder, [{key,cylinder_type},{hook, Hook},{title,?__(10,"Cylinder Type")}]},
     {label_column,[
	 {?__(11,"Thickness"), {text,0.25,[{key,thickness},{range,{0.0,infinity}}]}},
	 {?__(12,"Degrees"), {text,360.0,[{key,degrees},{range,{0.0,360.0}}]}},
	 {?__(13,"Angle Offset"), {text,0.0,[{key,angle_offset},{range,{-360.0,360.0}}]}}]},
     wings_shapes:transform_obj_dlg()].

make_cylinder(Arg, St) when is_atom(Arg) ->
    Qs = cylinder_dialog(),
    Label = ?__(1,"Cylinder Options"),
    wings_dialog:dialog_preview({shape,cylinder}, Arg, Label, Qs, St);
make_cylinder(Arg, _St) ->
    ArgDict = dict:from_list(Arg),
    Sections = dict:fetch(sections, ArgDict),
    Height = dict:fetch(height, ArgDict),
    TopX = dict:fetch(top_x, ArgDict),
    TopZ = dict:fetch(top_z, ArgDict),
    BotX = dict:fetch(bottom_x, ArgDict),
    BotZ = dict:fetch(bottom_z, ArgDict),
    Slice = dict:fetch(slice, ArgDict),
    Thickness = dict:fetch(thickness, ArgDict),
    Degrees = dict:fetch(degrees, ArgDict),
    AngleOffset = dict:fetch(angle_offset, ArgDict),
    Modify = [{dict:fetch(rot_x, ArgDict), dict:fetch(rot_y, ArgDict), dict:fetch(rot_z, ArgDict)},
	      {dict:fetch(mov_x, ArgDict), dict:fetch(mov_y, ArgDict), dict:fetch(mov_z, ArgDict)},
	      dict:fetch(ground, ArgDict)],

    Type = dict:fetch(cylinder_type, ArgDict),
    case Type of
        cylinder ->
            make_cylinder(Sections, TopX, TopZ, BotX, BotZ, Height, Slice, Modify);
        tube ->
            make_tube(Sections, TopX, TopZ, BotX, BotZ, Height, Thickness, Slice, Modify);
        gear ->
            [Min|_] = lists:sort([TopX, TopZ, BotX, BotZ]),
            Thickness1 = min(Min, Thickness),
            make_gear(Sections, TopX, TopZ, BotX, BotZ, Height, Thickness1, Slice, Modify);
        pie ->
            make_pie(Sections, TopX, TopZ, BotX, BotZ, Height, Degrees, AngleOffset, Slice, Modify)
    end.

%%%
%%% Cylinder
%%%

make_cylinder(Sections, TopX, TopZ, BotX, BotZ, Height, Slice, [Rot, Mov, Ground]) ->
    case Slice of
        % No slices
        1.0 -> 
            Vs0 = cylinder_verts(Sections, TopX, TopZ, BotX, BotZ, Height),
            Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
            Fs = cylinder_faces(Sections);
        % Needs slicing
        _ -> 
            Vs0 = cylinder_verts_slice(Sections, TopX, TopZ, BotX, BotZ, Height, Slice),
            Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
            Fs = cylinder_faces_slice(Sections, Slice)
        end,
    {new_shape,cylinder_type(cylinder),Fs,Vs}.

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

cylinder_verts_slice(Sections, TopX, TopZ, BotX, BotZ, Height, Slice) ->
    Slice0 = round(Slice),
    YAxis = Height/2,
    Delta = pi()*2/Sections,
    Rings = lists:seq(0, Sections-1),
    lists:flatten([
        begin
            T = I / Slice0,
            Y = YAxis - (T * Height),
            Rx = TopX + T * (BotX - TopX),
            Rz = TopZ + T * (BotZ - TopZ),
            ring_of_verts(Rings, Delta, Y, Rx, Rz, 0.0)
        end || I <- lists:seq(0, Slice0)
    ]).

cylinder_faces_slice(Sections, NumSlice) ->
    NumSlice0 = round(NumSlice),
    Ns = lists:reverse(lists:seq(0, Sections-1)),
    Upper = Ns,
    Lower = lists:seq(Sections * NumSlice0, Sections * (NumSlice0 + 1) - 1),
    Sides = [[I + Sections * S, (I + 1) rem Sections + Sections * S, (I + 1) rem Sections + Sections * (S + 1), I + Sections * (S + 1)]
             || S <- lists:seq(0, NumSlice0 - 1), I <- Ns],
    [Upper, Lower | Sides].

%%%
%%% Gear
%%%

make_gear(Sections0, TopX, TopZ, BotX, BotZ, Height, ToothHeight, Slice, [Rot, Mov, Ground]) ->
    case Slice of
        % No slices
        1.0 -> 
            Vs0 = gear_verts(Sections0, TopX, TopZ, BotX, BotZ, Height, ToothHeight),
            Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
            Fs = gear_faces(Sections0);
        % Needs slicing
        _ -> 
            Vs0 = gear_verts_slice(Sections0, TopX, TopZ, BotX, BotZ, Height, ToothHeight, Slice),
            Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
            Fs = gear_faces_slice(Sections0, Slice)
        end,
    {new_shape,cylinder_type(gear),Fs,Vs}.

gear_verts(Sections, TopX, TopZ, BotX, BotZ, Height, ToothHeight) ->
    YAxis = Height/2,
    Delta = pi()*2/Sections,
    Rings = lists:seq(0, Sections-1),
    TopOuter = ring_of_verts(Rings, Delta, YAxis, TopX, TopZ, 0.0),
    BotOuter = ring_of_verts(Rings, Delta, -YAxis, BotX, BotZ, 0.0),
    TopInner = ring_of_verts(Rings, Delta, YAxis, TopX-ToothHeight, TopZ-ToothHeight, 0.0),
    BotInner = ring_of_verts(Rings, Delta, -YAxis, BotX-ToothHeight, BotZ-ToothHeight, 0.0),
    OuterVerts = TopOuter ++ TopInner,
    InnerVerts = BotOuter ++ BotInner,
    OuterVerts ++ InnerVerts.

gear_faces(Nres) ->
    Offset = Nres*2,
    A = lists:seq(Nres-1, 0, -1),
    B = lists:seq(2*Nres-2, Nres, -1) ++ [2*Nres-1],
    TopFace = zip_lists_2e(A,B),
    BotFace = [Index+Offset || Index <- lists:reverse(TopFace)],
    InnerFaces = [[I, I+1, I+Offset+1, I+Offset]
		  || I <- lists:seq(0, Nres-2, 2)],
    OuterFaces = [[I, I+1, I+Offset+1, I+Offset]
		  || I <- lists:seq(Nres+1, 2*Nres-3, 2)]
		  ++ [[2*Nres-1, Nres, 3*Nres, 4*Nres-1]], % the last face
    SideFacesO = [[I, I+Nres, 3*Nres+I, 2*Nres+I]
		  || I <- lists:seq(1, Nres-1, 2)],
    SideFacesE = [[I+Nres, I, 2*Nres+I, 3*Nres+I]
		  || I <- lists:seq(2, Nres-2, 2)]
		  ++ [[Nres, 0, 2*Nres, 3*Nres]], % the last face
    [TopFace]++[BotFace] ++ InnerFaces++OuterFaces ++ SideFacesO++SideFacesE.

gear_verts_slice(Sections, TopX, TopZ, BotX, BotZ, Height, ToothHeight, Slice) ->
    Slice0 = round(Slice),
    YAxis = Height/2,
    Delta = pi()*2/Sections,
    Rings = lists:seq(0, Sections-1),
    lists:flatten([
        begin
            T = I / Slice0,
            Y = YAxis - (T * Height),
            Rx = TopX + T * (BotX - TopX),
            Rz = TopZ + T * (BotZ - TopZ),
            Outer = ring_of_verts(Rings, Delta, Y, Rx, Rz, 0.0),
            Inner = ring_of_verts(Rings, Delta, Y, Rx-ToothHeight, Rz-ToothHeight, 0.0),
            Outer ++ Inner
        end || I <- lists:seq(0, Slice0)
    ]).

gear_faces_slice(Sections, NumSlice) ->
    NumSlice0 = round(NumSlice),
    N = Sections,
    Offset = 2 * N,

    A = lists:seq(N-1, 0, -1),
    B = lists:seq(2*N-2, N, -1) ++ [2*N-1],
    TopFace = zip_lists_2e(A, B),

    BotBase = NumSlice0 * Offset,
    BotFace = [Idx + BotBase || Idx <- lists:reverse(TopFace)],

    OuterSides = [[I, I+1, I+Offset+1, I+Offset]
                  || S <- lists:seq(0, NumSlice0-1),
                     I0 <- lists:seq(0, N-2, 2),
                     I <- [S*Offset + I0]],
    InnerSides = [[I, I+1, I+Offset+1, I+Offset]
                  || S <- lists:seq(0, NumSlice0-1),
                     I0 <- lists:seq(N+1, 2*N-3, 2),
                     I <- [S*Offset + I0]]
                  ++ [[S*Offset+2*N-1, S*Offset+N, (S+1)*Offset+N, (S+1)*Offset+2*N-1]
                      || S <- lists:seq(0, NumSlice0-1)],

    SideFacesO = [[S*Offset+I, S*Offset+I+N, (S+1)*Offset+I+N, (S+1)*Offset+I]
                  || S <- lists:seq(0, NumSlice0-1),
                     I <- lists:seq(1, N-1, 2)],
    SideFacesE = [[S*Offset+I+N, S*Offset+I, (S+1)*Offset+I, (S+1)*Offset+I+N]
                  || S <- lists:seq(0, NumSlice0-1),
                     I <- lists:seq(2, N-2, 2)]
                  ++ [[S*Offset+N, S*Offset+0, (S+1)*Offset+0, (S+1)*Offset+N]
                      || S <- lists:seq(0, NumSlice0-1)],

    [TopFace] ++ [BotFace] ++ OuterSides ++ InnerSides ++ SideFacesO ++ SideFacesE.

%%%
%%% Tube
%%%

make_tube(Sections, TopX, TopZ, BotX, BotZ, Height, Thickness, Slice, [Rot, Mov, Ground]) ->
    case Slice of
        % No slices
        1.0 -> 
            Vs0 = tube_verts(Sections, TopX, TopZ, BotX, BotZ, Height, Thickness),
            Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
            Fs = tube_faces(Sections);
        % Needs slicing
        _ -> 
            Vs0 = tube_verts_slice(Sections, TopX, TopZ, BotX, BotZ, Height, Thickness, Slice),
            Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
            Fs = tube_faces_slice(Sections, Slice)
        end,
    {new_shape,cylinder_type(tube),Fs,Vs}.

tube_verts(Sections, TopX, TopZ, BotX, BotZ, Height, Thickness) ->
    YAxis = Height/2,
    Delta = pi()*2/Sections,
    Rings = lists:seq(0, Sections-1),
    TopOuter = ring_of_verts(Rings, Delta, YAxis, TopX, TopZ, 0.0),
    BotOuter = ring_of_verts(Rings, Delta, -YAxis, BotX, BotZ, 0.0),
    TopInner = ring_of_verts(Rings, Delta, YAxis, TopX-Thickness, TopZ-Thickness, 0.0),
    BotInner = ring_of_verts(Rings, Delta, -YAxis, BotX-Thickness, BotZ-Thickness, 0.0),
    OuterVerts = TopOuter ++ BotOuter,
    InnerVerts = TopInner ++ BotInner,
    OuterVerts ++ InnerVerts.

tube_faces(Nres) ->
    Offset = 2*Nres,
    TopFaces =
	[[I, I+1, I+Nres+1, I+Nres] || I <- lists:seq(0, Nres-2)] ++
	[[Nres-1, 0, Nres, 2*Nres-1] ],        % the last face
    BotFaces = [[D+Offset,C+Offset,B+Offset,A+Offset] || [A,B,C,D] <- TopFaces],
    InnerFaces =
	[[I, I-1, I+Offset-1, I+Offset] || I <- lists:seq(1, Nres-1)] ++
	[[0, Nres-1, 3*Nres-1, 2*Nres] ],      % the last face
    OuterFaces =
	[[I, I+1, I+Offset+1, I+Offset] || I <- lists:seq(Nres, 2*Nres-2)] ++
	[[2*Nres-1, Nres, 3*Nres, 4*Nres-1] ], % the last face
    TopFaces ++ BotFaces ++ InnerFaces ++ OuterFaces.

tube_verts_slice(Sections, TopX, TopZ, BotX, BotZ, Height, Thickness, Slice) ->
    Slice0 = round(Slice),
    YAxis = Height/2,
    Delta = pi()*2/Sections,
    Rings = lists:seq(0, Sections-1),
    lists:flatten([
        begin
            T = I / Slice0,
            Y = YAxis - (T * Height),
            Rx = TopX + T * (BotX - TopX),
            Rz = TopZ + T * (BotZ - TopZ),
            Outer = ring_of_verts(Rings, Delta, Y, Rx, Rz, 0.0),
            Inner = ring_of_verts(Rings, Delta, Y, Rx-Thickness, Rz-Thickness, 0.0),
            Outer ++ Inner
        end || I <- lists:seq(0, Slice0)
    ]).

tube_faces_slice(Nres, NumSlice) ->
    NumSlice0 = round(NumSlice),
    Offset = Nres * 2,

    TopFaces = [[I+Nres, I+Nres+1, I+1, I] || I <- lists:seq(0, Nres-2)] ++
        [[2*Nres-1, Nres, 0, Nres-1]],

    BotBase = NumSlice0 * Offset,
    BotFaces = [[D+BotBase, C+BotBase, B+BotBase, A+BotBase] || [A,B,C,D] <- TopFaces],

    OuterFaces = [[I, I+1, I+Offset+1, I+Offset]
                  || S <- lists:seq(0, NumSlice0-1),
                     I0 <- lists:seq(0, Nres-2),
                     I <- [S*Offset + I0]]
                  ++ [[S*Offset+Nres-1, S*Offset+0, (S+1)*Offset+0, (S+1)*Offset+Nres-1]
                      || S <- lists:seq(0, NumSlice0-1)],

    InnerFaces = [[I+1, I, I+Offset, I+Offset+1]
                  || S <- lists:seq(0, NumSlice0-1),
                     I0 <- lists:seq(Nres, 2*Nres-2),
                     I <- [S*Offset + I0]]
                  ++ [[S*Offset+Nres, S*Offset+2*Nres-1, (S+1)*Offset+2*Nres-1, (S+1)*Offset+Nres]
                      || S <- lists:seq(0, NumSlice0-1)],
    TopFaces ++ BotFaces ++ OuterFaces ++ InnerFaces.

ring_of_verts(Rings, Delta, YAxis, XAxis, ZAxis, Offset) ->
    [{XAxis*cos(Offset+I*Delta), YAxis, ZAxis*sin(Offset+I*Delta)} || I <- Rings].

zip_lists_2e([], []) -> [];   % Zip two lists together, two elements at a time.
zip_lists_2e(A, B) ->	      % Both lists must be equal in length
    [HA1,HA2 | TA] = A,       % and must have an even number of elements
    [HB1,HB2 | TB] = B,
    lists:flatten([[HA1,HA2,HB1,HB2] | zip_lists_2e(TA, TB)]).

%%%
%%% Pie
%%%

make_pie(Sections, TopX, TopZ, BotX, BotZ, Height, Degrees, AngleOffset, Slice, [Rot, Mov, Ground]) ->
    case Slice of
        % No slices
        1.0 -> 
            Vs0 = pie_verts(Sections, TopX, TopZ, BotX, BotZ, Height, Degrees, AngleOffset),
            Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
            Fs = cylinder_faces(trunc(length(Vs0)/2));
        % Needs slicing
        _ -> 
            Vs0 = pie_verts_slice(Sections, TopX, TopZ, BotX, BotZ, Height, Degrees, AngleOffset, Slice),
            Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
            Fs = pie_faces_slice(Sections, Slice)
        end,
    {new_shape,cylinder_type(pie),Fs,Vs}.

pie_verts(Sections, TopX, TopZ, BotX, BotZ, Height, Degrees, AngleOffset) ->
    YAxis = Height/2,
    DtoRad = math:pi()/180.0,
    Offset = AngleOffset*DtoRad,
    Delta = (Degrees*DtoRad)/Sections,
    Rings = lists:seq(0, Sections),
    [Top0|_] = Top = ring_of_verts(Rings, Delta, YAxis, TopX, TopZ, Offset),
    [Bottom0|_] = Bottom = ring_of_verts(Rings, Delta, -YAxis, BotX, BotZ, Offset),
    {TopExt,BottomExt} =
        case Degrees of
            360.0 -> {[Top0,{0.0,YAxis,0.0}],[Bottom0,{0.0,-YAxis,0.0}]};
            _ -> {[{0.0,YAxis,0.0}],[{0.0,-YAxis,0.0}]}
        end,
    Top ++ TopExt ++ Bottom ++ BottomExt.

pie_verts_slice(Sections, TopX, TopZ, BotX, BotZ, Height, Degrees, AngleOffset, Slice) ->
    Slice0 = round(Slice),
    YAxis = Height/2,
    DtoRad = math:pi()/180.0,
    Offset = AngleOffset * DtoRad,
    Delta = (Degrees * DtoRad) / Sections,
    Rings = lists:seq(0, Sections),
    lists:flatten([
        begin
            T = I / Slice0,
            Y = YAxis - (T * Height),
            Rx = TopX + T * (BotX - TopX),
            Rz = TopZ + T * (BotZ - TopZ),
            Ring = ring_of_verts(Rings, Delta, Y, Rx, Rz, Offset),
            Ring ++ [{0.0, Y, 0.0}]
        end || I <- lists:seq(0, Slice0)
    ]).

pie_faces_slice(Sections, NumSlice) ->
    NumSlice0 = round(NumSlice),
    N = Sections + 1,
    Offset = N + 1,
    Center = N,

    TopFace = lists:reverse(lists:seq(0, N-1)) ++ [Center],

    BotBase = NumSlice0 * Offset,
    BotFace = [Center+BotBase | lists:seq(BotBase, N-1+BotBase)],

    ArcFaces = [[I, I+1, I+Offset+1, I+Offset]
                || S <- lists:seq(0, NumSlice0-1),
                   I0 <- lists:seq(0, N-2),
                   I <- [S*Offset + I0]],

    LeftWall = [[S*Offset, (S+1)*Offset, (S+1)*Offset+Center, S*Offset+Center]
                || S <- lists:seq(0, NumSlice0-1)],

    Last = N - 1,
    RightWall = [[S*Offset+Center, (S+1)*Offset+Center, (S+1)*Offset+Last, S*Offset+Last]
                 || S <- lists:seq(0, NumSlice0-1)],

    [TopFace, BotFace] ++ ArcFaces ++ LeftWall ++ RightWall.