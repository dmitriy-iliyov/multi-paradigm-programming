:- use_module(library(clpfd)).
:- use_module(library(lists)).

read_and_print_numbers(FileName, NumericSeries) :-
    read_file(FileName, NumericSeries),
    % print_numbers(NumericSeries),
    % writeln(' '),
    list_length(NumericSeries, Length),
    format('list size: ~w~n', [Length]).

read_file(FileName, Numbers) :-
    open(FileName, read, Stream),
    read_lines(Stream, Numbers),
    close(Stream).

read_lines(Stream, []) :-
    at_end_of_stream(Stream), !.
read_lines(Stream, [Number|Numbers]) :-
    \+ at_end_of_stream(Stream),
    read_line(Stream, Number),
    read_lines(Stream, Numbers).

read_line(Stream, Number) :-
    read_line_to_string(Stream, Line),
    atom_number(Line, Number).

print_numbers([]).
print_numbers([Number|Numbers]) :-
    format('~2f ', [Number]),
    print_numbers(Numbers).

list_length([], 0).
list_length([_|Tail], Length) :-
    list_length(Tail, TailLength),
    Length is TailLength + 1.
    
merge_sort([], []).
merge_sort([X], [X]).
merge_sort(List, Sorted) :-
    List = [_, _|_],
    split(List, L1, L2),
    merge_sort(L1, SortedL1),
    merge_sort(L2, SortedL2),
    merge(SortedL1, SortedL2, Sorted).

split([], [], []).
split([X], [X], []).
split([X,Y|T], [X|L1], [Y|L2]) :-
    split(T, L1, L2).

merge([], L, L).
merge(L, [], L).
merge([X|T1], [Y|T2], [X|T]) :-
    X =< Y,
    merge(T1, [Y|T2], T).
merge([X|T1], [Y|T2], [Y|T]) :-
    X > Y,
    merge([X|T1], T2, T).

truncate_decimal(Number, Precision, Truncated) :-
    (   var(Number), var(Precision)
    ->  throw(error(instantiation_error, truncate_decimal/3))
    ;   nonvar(Number), nonvar(Precision)
    ->  Truncated is round(Number * 10^Precision) / 10^Precision
    ;   throw(error(instantiation_error, truncate_decimal/3))
    ).

reley_distribution(X, Sigma, Result) :-
    (   var(X), var(Sigma)
    ->  throw(error(instantiation_error, reley_distribution/3))
    ;   nonvar(X), nonvar(Sigma)
    ->  (   X < 0
        ->  Result = 0
        ;   Intermediate is 1 - exp(-0.5 * (X / Sigma) ** 2),
            truncate_decimal(Intermediate, 6, Truncated),
            Result = Truncated
        )
    ;   throw(error(instantiation_error, reley_distribution/3))
    ).

inverse_reley_distribution(P, Sigma, Result) :-
    (   var(P), var(Sigma)
    ->  throw(error(instantiation_error, inverse_reley_distribution/3))
    ;   nonvar(P), nonvar(Sigma)
    ->  (   P < 0
        ->  Result = -1
        ;   P > 1
        ->  Result = -1
        ;   P == 1
        ->  Result = -1
        ;   Intermediate is Sigma * sqrt(-2 * log(1 - P)),
            truncate_decimal(Intermediate, 6, Truncated),
            Result = Truncated
        )
    ;   throw(error(instantiation_error, inverse_reley_distribution/3))
    ).


compute_intervals(_, _, _, AlphabetPower, []) :-
    AlphabetPower =< 0.

compute_intervals(SortedArray, Sigma, Buffer, AlphabetPower, [[Interval0, Interval1]|Intervals]) :-
    AlphabetPower > 0,
    Interval0 = Buffer,
    reley_distribution(Interval0, Sigma, Pa),
    IntermediateAlphabetPower is 1.0 / 26,
    Intermediate is IntermediateAlphabetPower + Pa,
    truncate_decimal(Intermediate, 6, Pb),
    (
        Pb > 1 ->
        last(SortedArray, LastElement),
        Interval1 = LastElement
        ;
        inverse_reley_distribution(Pb, Sigma, Interval1)
    ),
    % write('Interval0: '), write(Interval0), nl,
    % write('Pa: '), write(Pa), nl,
    % write('Pb: '), write(Pb), nl,
    % write('Interval1: '), write(Interval1), nl,
    NextPower is AlphabetPower - 1,
    compute_intervals(SortedArray, Sigma, Interval1, NextPower, Intervals).

compute_intervals_main(SortedArray, Sigma, AlphabetPower, MatrixInterval) :-
    (   SortedArray = [First|_]
    ->  compute_intervals(SortedArray, Sigma, First, AlphabetPower, MatrixInterval)
    ;   MatrixInterval = []
    ).

sum_of_squares([], 0).
sum_of_squares([H|T], Sum) :-
    sum_of_squares(T, RestSum),
    Sum is H * H + RestSum.

compute_sigma(SortedArray, Sigma) :-
    length(SortedArray, Size),
    sum_of_squares(SortedArray, Sum),
    Sigma is sqrt(Sum / (2 * Size)).

to_char_array(Array, _CharArray, MatrixInterval, Alphabet) :-
    length(Array, Size),
    to_char_array(0, Size, Array, _CharArray, MatrixInterval, Alphabet).

to_char_array(Size, Size, _, [], _, _).
to_char_array(Index, Size, Array, [Char|Rest], MatrixInterval, Alphabet) :-
    Index < Size,
    nth0(Index, Array, Element),
    map_interval(Element, Char, MatrixInterval, Alphabet),
    NextIndex is Index + 1,
    to_char_array(NextIndex, Size, Array, Rest, MatrixInterval, Alphabet).

map_interval(Element, Char, [[Low, High]|_], [LowChar|_]) :-
    Element >= Low,
    Element =< High,
    Char = LowChar.
map_interval(Element, Char, [_|Rest], [_|RestAlpha]) :-
    map_interval(Element, Char, Rest, RestAlpha).
    
print_intervals([], _).
print_intervals([[Interval0, Interval1]|Intervals], [Letter|Letters]) :-
    format('~w : [~f, ~f]~n', [Letter, Interval0, Interval1]),
    print_intervals(Intervals, Letters).

generate_alphabet(A) :-
    generate_alphabet(97, A).

generate_alphabet(123, []) :- !.
generate_alphabet(Curr, [Letter|Rest]) :-
    char_code(Letter, Curr),
    Next is Curr + 1,
    generate_alphabet(Next, Rest).

find_index(Element, List, Index) :-
    nth0(Index, List, Element).

make_result_matrix(CharArray, Alphabet, ResultMatrix, FinalResultMatrix) :-
    length(CharArray, Size),
    make_result_matrix_helper(CharArray, Alphabet, ResultMatrix, FinalResultMatrix, Size, 0).

make_result_matrix_helper(_, _, ResultMatrix, ResultMatrix, Size, Index) :-
    Index >= Size - 1, !.

make_result_matrix_helper(CharArray, Alphabet, ResultMatrix, FinalResultMatrix, Size, _Index) :-
    _NextIndex is _Index + 1,
    nth0(_Index, CharArray, CurrentChar),
    nth0(_NextIndex, CharArray, NextChar),
    find_index(CurrentChar, Alphabet, CurrentIndex),
    find_index(NextChar, Alphabet, NextIndex),
    increment_matrix(ResultMatrix, CurrentIndex, NextIndex, TempResultMatrix),
    make_result_matrix_helper(CharArray, Alphabet, TempResultMatrix, FinalResultMatrix, Size, _NextIndex).

increment_matrix(ResultMatrix, CurrentIndex, NextIndex, NewResultMatrix) :-
    nth0(CurrentIndex, ResultMatrix, Row),
    nth0(NextIndex, Row, Value),
    NewValue is Value + 1,
    replace(Row, NextIndex, NewValue, NewRow),
    replace(ResultMatrix, CurrentIndex, NewRow, NewResultMatrix).
    
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).

initialize_result_matrix(Alphabet, ResultMatrix) :-
    length(Alphabet, Len),
    length(Row, Len),
    maplist(=(0), Row),
    length(ResultMatrix, Len),
    maplist(=(Row), ResultMatrix).

print_matrix([]).
print_matrix([Row|Rest]) :-
    writeln(Row),
    print_matrix(Rest).


main() :-
    % writeln('array:'),
    read_and_print_numbers('data.txt', NumericSeries),
    merge_sort(NumericSeries, SortedArray),
    compute_sigma(SortedArray, Sigma),
    generate_alphabet(A),
    write('alphabet: '), nl,
    writeln(A),
    write('sigma: '), write(Sigma), nl,
    % writeln('sorted array:'),
    % print_numbers(SortedArray),
    % writeln(' '),
    compute_intervals_main(SortedArray, Sigma, 26, Intervals),
    print_intervals(Intervals, A),
    to_char_array(NumericSeries, CharArray, Intervals, A),
    writeln('char array:'),
    writeln(CharArray),
    initialize_result_matrix(A, ResultMatrix),
    make_result_matrix(CharArray, A, ResultMatrix, FinalResultMatrix),
    writeln('Final matrix:'),
    print_matrix(FinalResultMatrix).


:- initialization(main()).
