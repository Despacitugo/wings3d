-module(polyhedron_aux).
-export([compare_min_max/2,find_min_max/2,transform/2,normalize/1]).

compare_min_max({X,Y,Z},[{MinX,MinY,MinZ},{MaxX,MaxY,MaxZ}]) ->  
    if
        X < MinX -> MinX1 = X;
        true -> MinX1 = MinX
    end,
    if
        Y < MinY -> MinY1 = Y;
        true -> MinY1 = MinY
    end,
    if
        Z < MinZ -> MinZ1 = Z;
        true -> MinZ1 = MinZ
    end,
    if
        X > MaxX -> MaxX1 = X;
        true -> MaxX1 = MaxX
    end,
    if
        Y > MaxY -> MaxY1 = Y;
        true -> MaxY1 = MaxY
    end,
    if
        Z > MaxZ -> MaxZ1 = Z;
        true -> MaxZ1 = MaxZ
    end,

    [{MinX1,MinY1,MinZ1},{MaxX1,MaxY1,MaxZ1}].

find_min_max(List,MinMax0) ->
    case List of
        [] -> MinMax0;
        [H | T] -> MinMax = compare_min_max(H,MinMax0),
                    find_min_max(T,MinMax)
    end.

transform(List, Mat) ->
    case List of
        [] -> [];
        [H|T] -> [e3d_mat:mul_point(Mat,H)] ++ transform(T,Mat)
    end.

normalize(Vrs) ->
    [Min, Max] = find_min_max(Vrs,[{0.0,0.0,0.0}, {0.0,0.0,0.0}]),

    % Size -> size of the cube enclosing We
    {X,Y,Z} = e3d_vec:sub(Max, Min),
    if 
        Y < Z, X < Z -> Size = {Z,Z,Z};
        X < Y -> Size = {Y,Y,Y};
        true -> Size = {X,X,X}
    end,

    Scale =  e3d_vec:len({2.0,2.0,2.0}) / e3d_vec:len(Size),
    Matrix = e3d_mat:scale(Scale),
    transform(Vrs, Matrix).

