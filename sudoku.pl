/* 
   There are variations of the suduko puzzle
   with different main grid sizes and mini-block sizes.

   For example, junior-sudoku is based on
       4 x 4 grid with 2 x 2 mini-blocks

   Another example is mini-sudoku that is based on
       6 x 6 grid with 3 x 2 mini-blocks

   Task
   ====
   Generalize your sudoku solution generator using
   a new predicate gen_suduko below which supports
   different variations of sudoku puzzles, based on
   grid and mini-block sizes.

   gen_sudoku(Rows,N,B_R,B_C)
      N - size of entire block of N x N
      B_R - mini-block row size
      B_C - mini-block column size

   We can add the following constraints:
         N #>3, B_R >1, B_C>1, N #= B_R * B_C
   To restrict ourselves to regular-shaped sudokus that
   that are easier for humans to follow.
   
   The output for gen_sudoku will be made using maplist(portray_clause, Rows)
   in the query predicate.
*/

:- use_module(library(clpfd)).

sudoku(Rows) :-
        length(Rows, 9), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        blocks(As, Bs, Cs),
        blocks(Ds, Es, Fs),
        blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
        all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
        blocks(Ns1, Ns2, Ns3).

problem(1, [[_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,3,_,8,5],
            [_,_,1,_,2,_,_,_,_],
            [_,_,_,5,_,7,_,_,_],
            [_,_,4,_,_,_,1,_,_],
            [_,9,_,_,_,_,_,_,_],
            [5,_,_,_,_,_,_,7,3],
            [_,_,2,_,1,_,_,_,_],
            [_,_,_,_,4,_,_,_,9]]).

problem(2, [[3,_,_,8,_,1,_,_,2],
            [2,_,1,_,3,_,6,_,4],
            [_,_,_,2,_,4,_,_,_],
            [8,_,9,_,_,_,1,_,6],
            [_,6,_,_,_,_,_,5,_],
            [7,_,2,_,_,_,4,_,9],
            [_,_,_,5,_,9,_,_,_],
            [9,_,4,_,8,_,7,_,5],
            [6,_,_,1,_,7,_,_,3]]).

mini_suduko(1,[[_,_,6,_,4,_],
               [_,_,_,_,6,_],
               [_,_,_,5,_,3],
               [3,_,_,_,_,_],
               [_,1,_,_,_,_],
               [_,5,_,_,4,_]]).

junior_suduko(1,[[_,4,_,1],
                 [3,_,4,_],
                 [1,_,_,4],
                 [_,2,1,_]]).

rectangle_suduko(1, [[_,5,_,_,2,_],
                     [_,_,2,5,_,_],
                     [_,_,_,4,3,_],
                     [4,_,_,_,5,6],
                     [2,_,5,_,_,1],
                     [_,6,1,2,4,_]]).

rectangle_suduko(2, [[_,4,_,_,5,2],
                     [_,_,1,_,4,_],
                     [_,_,_,_,1,4],
                     [_,1,2,5,_,3],
                     [1,_,6,4,3,_],
                     [5,_,_,_,2,_]]).

% query(Prob,Rows) :- problem(Prob, Rows), sudoku(Rows), maplist(portray_clause, Rows).


gen_sudoku(Rows,N,B_R,B_C) :- N #>1, B_R >1, B_C>1, N #= B_R * B_C,
        length(Rows, N), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..N,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        check_chunks(Rows, B_R, B_C).

check_chunks([],_,_).
check_chunks(Rows, B_R, B_C) :-
   length(BlockRows, B_R), append(BlockRows, Rest, Rows),
   block_columns(BlockRows,B_R, B_C, [], []),
   check_chunks(Rest, B_R, B_C).

block_columns([Row|Rows],B_R,B_C,Bs,Rs) :-   
   length(Cols,B_C), append(Cols, Rest, Row),
   block_columns(Rows,B_R, B_C,[Cols|Bs],[Rest|Rs]).
block_columns([],B_R,B_C,Bs,Rs) :-
   flatten(Bs,Ns), all_distinct(Ns),
   flatten(Rs,Xs),
   (Xs = [] ->
      true
   ; block_columns(Rs,B_R, B_C, [], [])).

% example query: replace rectangle_suduko(Prob, Rows) with any suduko
query(Prob,Rows,N,B_R,B_C) :- rectangle_suduko(Prob, Rows), gen_sudoku(Rows,N,B_R,B_C), maplist(portray_clause, Rows).

/*
  N - size of entire block of N x N
  B_R - mini-block row size
  B_C - mini-block column size
*/

mini_sudoku(Rows) :- gen_sudoku(Rows,6,2,3).
junior_sudoku(Rows) :- gen_sudoku(Rows,4,2,2).
new_sudoku(Rows) :- gen_sudoku(Rows,9,3,3).
