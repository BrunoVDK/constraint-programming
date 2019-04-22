%
% Utility functions.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

% Convert the given 2-dimensional list to an array.
%
% @param    The list to convert to an array.
% @param    The array representing the same collection as the given list.
list_2d_to_array(List, Array) :-
    % First, the rows are transformed to arrays.
    % The resulting list of arrays is transformed to an array.
    (foreach(RowList, List), foreach(Row, Rows) do
        array_list(Row, RowList)
    ),
    array_list(Array, Rows).
