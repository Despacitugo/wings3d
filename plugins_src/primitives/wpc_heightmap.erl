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
-export([init/0,menu/2,command/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

init() -> true.

menu({shape}, Menu) ->
    lists:reverse(parse(Menu, [], false));
menu(_, Menu) -> Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [heightmap_menu()|NewMenu];
parse([A = {_,grid,_,_}|Rest], NewMenu, false) ->
    parse(Rest, [A,heightmap_menu()|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

heightmap_menu() ->
    {"Heightmap Grid...",heightmap,"Create a height map from an image in grey levels",[option]}.

command({shape,{heightmap,Ask}}, _St) when is_atom(Ask) ->
    make_heightmap(Ask);
command({shape,{heightmap,Params}}, _St) ->
    make_heightmap_1(Params);
command(_, _) -> next.

%%% ============================================================
%%% Image selection dialog
%%% ============================================================

make_heightmap(Ask) ->
    case wings_image:images() of
        [] ->
            wings_u:message("No images imported. Please import an image first.");
        Images ->
            MenuItems = [{Name, Id} || {Id, #e3d_image{name=Name}} <- Images],
            {_DefName, DefId} = hd(MenuItems),
            Qs = [{vframe,
                   [{label_column,
                     [{"Image",
                       {menu, MenuItems, DefId, [{key,image_id}]}}
                     ]}
                   ]}],
            wings_dialog:dialog(Ask, "Heightmap Grid", Qs,
                fun(Res) ->
                    {shape,{heightmap,Res}}
                end)
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
    #e3d_image{width=Width,height=Height,image=Pixels} = Img,
    case Width >= 2 andalso Height >= 2 of
        false ->
            wings_u:message("Heightmap image must be at least 2x2 pixels."),
            keep;
        true ->
            Vs0 = heightmap_vertices(Width, Height, Pixels),
            Fs0 = heightmap_faces(Width, Height),
            {BaseVs, BaseFace} = heightmap_base(Width, Height),
            Vs = Vs0 ++ BaseVs,
            Fs = Fs0 ++ [BaseFace],
            {new_shape, "Heightmap Grid", Fs, Vs}
    end.

heightmap_vertices(Width, Height, Pixels) ->
    HalfW = (Width - 1) / 2.0,
    HalfH = (Height - 1) / 2.0,
    [{X - HalfW,
      (float(binary:at(Pixels, Y*Width + X))) * 0.2,
      Y - HalfH}
     || Y <- lists:seq(0, Height-1), X <- lists:seq(0, Width-1)].

heightmap_faces(Width, Height) ->
        [{default,
            [idx(X, Y, Width),
             idx(X, Y+1, Width),
             idx(X+1, Y+1, Width),
             idx(X+1, Y, Width)]}
     || Y <- lists:seq(0, Height-2), X <- lists:seq(0, Width-2)].

heightmap_base(Width, Height) ->
    HalfW = (Width - 1) / 2.0,
    HalfH = (Height - 1) / 2.0,
    BaseVs = [{-HalfW, 0.0, -HalfH},
              {-HalfW, 0.0,  HalfH},
              { HalfW, 0.0,  HalfH},
              { HalfW, 0.0, -HalfH}],
    I0 = Width*Height,
    BaseFace = {default, [I0, I0+1, I0+2, I0+3]},
    {BaseVs, BaseFace}.

idx(X, Y, Width) ->
    Y*Width + X.
