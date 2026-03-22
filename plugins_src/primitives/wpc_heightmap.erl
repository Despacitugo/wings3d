%%
%%  wpc_heightmap.erl --
%%
%%     Height Map primitive plug-in.
%%     Generates a 3D heightmap mesh from an imported image's gray levels.
%%
%%  Copyright (c) 2026
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_heightmap).

-export([init/0, menu/2, command/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

init() ->
    true.

menu({shape}, Menu) ->
    lists:reverse(parse(Menu, [], false));
menu(_, Menu) ->
    Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [heightmap_menu() | NewMenu];
parse([A = {_, grid, _, _} | Rest], NewMenu, false) ->
    parse(Rest, [A, heightmap_menu() | NewMenu], true);
parse([Elem | Rest], NewMenu, Found) ->
    parse(Rest, [Elem | NewMenu], Found).

heightmap_menu() ->
    {"Heightmap Grid...",
     heightmap,
     "Create a height map from an image in grey levels",
     [option]}.

command({shape, {heightmap, Ask}}, _St) when is_atom(Ask) ->
    make_heightmap(Ask);
command({shape, {heightmap, Params}}, _St) ->
    make_heightmap_1(Params);
command(_, _) ->
    next.

%%% ============================================================
%%% Image selection dialog
%%% ============================================================

make_heightmap(Ask) ->
    case wings_image:images() of
        [] ->
            wings_u:message("No images imported. Please import an image first.");
        Images ->
            MenuItems = [{Name, Id} || {Id, #e3d_image{name = Name}} <- Images],
            {_DefName, DefId} = hd(MenuItems),
            Qs = [{vframe,
                   [{label_column, [{"Image", {menu, MenuItems, DefId, [{key, image_id}]}}]}]}],
            wings_dialog:dialog(Ask,
                                "Heightmap Grid",
                                Qs,
                                fun(Res) -> {shape, {heightmap, Res}} end)
    end.

%%% ============================================================
%%% HeightMap mesh generation
%%% ============================================================

make_heightmap_1(Params) ->
    ImageId = proplists:get_value(image_id, Params),
    case ImageId of
        undefined ->
            wings_u:message("No image selected."),
            keep;
        _ ->
            make_heightmap_from_image(ImageId)
    end.

make_heightmap_from_image(ImageId) ->
    Img0 = wings_image:info(ImageId),
    Img = e3d_image:convert(Img0, g8, 1, upper_left),
    #e3d_image{width = Width,
               height = Height,
               image = Pixels} =
        Img,
    case Width >= 2 andalso Height >= 2 of
        false ->
            wings_u:message("Heightmap image must be at least 2x2 pixels."),
            keep;
        true ->
            Vs0 = heightmap_vertices(Width, Height, Pixels),
            Fs0 = heightmap_faces(Width, Height),
            Perim = heightmap_perimeter(Width, Height),
            BaseVs = heightmap_base_vertices(Vs0, Perim, Width * Height),
            SideFaces = heightmap_side_faces(Perim, Width * Height, length(Perim)),
            BottomFace = heightmap_bottom_face(Width * Height, length(Perim)),
            Vs = Vs0 ++ BaseVs,
            Fs = Fs0 ++ SideFaces ++ [BottomFace],
            {new_shape, "Heightmap Grid", Fs, Vs}
    end.

heightmap_vertices(Width, Height, Pixels) ->
    HalfW = (Width - 1) / 2.0,
    HalfH = (Height - 1) / 2.0,
    [{X - HalfW, float(binary:at(Pixels, Y * Width + X)) * 0.2, Y - HalfH}
     || Y <- lists:seq(0, Height - 1), X <- lists:seq(0, Width - 1)].

heightmap_faces(Width, Height) ->
    [{default,
      [idx(X, Y, Width), idx(X, Y + 1, Width), idx(X + 1, Y + 1, Width), idx(X + 1, Y, Width)]}
     || Y <- lists:seq(0, Height - 2), X <- lists:seq(0, Width - 2)].

%% Ordered perimeter indices (clockwise when viewed from above):
%%   top edge L->R, right edge T->B, bottom edge R->L, left edge B->T
heightmap_perimeter(Width, Height) ->
    Top = [idx(X, 0, Width) || X <- lists:seq(0, Width - 1)],
    Right = [idx(Width - 1, Y, Width) || Y <- lists:seq(1, Height - 1)],
    Bottom = [idx(X, Height - 1, Width) || X <- lists:seq(Width - 2, 0, -1)],
    Left = [idx(0, Y, Width) || Y <- lists:seq(Height - 2, 1, -1)],
    Top ++ Right ++ Bottom ++ Left.

%% For each perimeter vertex, create a copy at Y=0
heightmap_base_vertices(Vs, Perim, _BaseOffset) ->
    VsArr = list_to_tuple(Vs),
    [{X, 0.0, Z} || I <- Perim, {X, _Y, Z} <- [element(I + 1, VsArr)]].

%% Creates the side wall quads, for each consecutive pair of perimeter vertices,
%% connect (top_i, top_next, base_next, base_i) so the shared edge
%% with the heightmap top face is traversed in the opposite direction.
heightmap_side_faces(Perim, BaseOffset, NPerim) ->
    PerimArr = list_to_tuple(Perim),
    [{default,
      [element(I, PerimArr), % top_i
       element(I rem NPerim + 1, PerimArr), % top_next
       BaseOffset + I rem NPerim, % base_next
       BaseOffset + I - 1]} % base_i
     || I <- lists:seq(1, NPerim)].

%% Creates the bottom face with the base perimeter vertices (clockwise order from the top view)
heightmap_bottom_face(BaseOffset, NPerim) ->
    {default, [BaseOffset + I || I <- lists:seq(0, NPerim - 1)]}. % base_i, base_i+1, ...

idx(X, Y, Width) ->
    Y * Width + X.
