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
                       {menu, MenuItems, DefId, [{key,image_id}]}},
                       "Size",
                       {text, 2.0, [{key,size},{range,{0.01,infinity}}]},
                       {"Max Height",
                        {text, 1.0, [{key,max_height},{range,{0.001,infinity}}]}}
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
    io:format("HeightMap: selected image id = ~p~n", [ImageId]),
    %% TODO: generate heightmap
    keep.
