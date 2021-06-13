
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbitr <img src="man/figures/rbitr_logo.png" align="right" width="150">

<!-- badges: start -->
<!-- badges: end -->

Arbiter: One who ensures adherence to the rules and laws of chess

## Installation

You can install the beta version of rbitr from github with:

``` r
devtools::install_github("dryguy/rbitr")
```

### Introduction

rbitr is a an R package for analyzing chess games, with emphasis on
generating game statistics that may be useful for the detection of
illicit computer assistance.

For game analysis, rbitr relies on an external chess engine.
Communication between rbitr and the chess engine is done via the
[Universal Chess Interface
Protocol](http://wbec-ridderkerk.nl/html/UCIProtocol.html), so a chess
engine that is compatible with the UCI protocol is required in order to
use many of rbitr’s functions. rbitr was developed using the [Stockfish
13](https://stockfishchess.org/) chess engine, and currently has only
been tested with this engine. Other UCI-compatible engines should also
work, but your mileage may vary.

A big thanks is due to Wojciech Rosa for creating the package
[bigchess](https://github.com/rosawojciech/bigchess). rbitr depends on
bigchess to handle the interface between R and the chess engine. It is
unlikely that rbitr would exist without it.

Once you have installed rbitr and are ready to begin, simply load the
package.

``` r
library(rbitr)
```

### Importing Games from PGN Files

Chess games stored in the Portable Game Notation (PGN) format can easily
be imported into R using the function `get_pgn()`.

A chess game stored in a PGN file looks something like this:

    [Event "Casual Game"]
    [Site "Arctic Ocean"]
    [Date "17-"]
    [Round "?"]
    [White "Victor Frankenstein"]
    [Black "'Adam'"]
    [Result "0-1"]

    1. f3 e6 2. g4 Qh4#

The data in brackets are known as tag pairs, and the first word in each
bracket is the tag name. Below the tags is the “movetext” section.

rbitr stores chess games as
[tibbles](https://github.com/tidyverse/tibble). The main reason for this
is that the moves of longer chess games can make the onscreen display of
a data frame rather unwieldy, whereas tibbles are designed to provide a
nice orderly display. With `get_pgn()`, each game in a PGN file will
become a row in a tibble, each tag will become a column, and each tag
name will become a column name. The moves become an additional column
named “Movetext”.

A few example PGN files have been included with the rbitr package.
Here’s an example showing how to import a PGN file containing two short
games.

``` r
two_games_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'two_games.pgn'
)
get_pgn(two_games_path)
#> # A tibble: 2 x 8
#>   Event   Site      Date    Round White   Black              Result Movetext    
#>   <chr>   <chr>     <chr>   <chr> <chr>   <chr>              <chr>  <chr>       
#> 1 Casual~ 221B Bak~ 1887.0~ ?     Sherlo~ John H. Watson     1-0    1. e4 g5 2.~
#> 2 Casual~ Not in K~ 1900.0~ ?     Scarec~ Oscar Zoroaster P~ 1-0    1. e4 f5 2.~
```

PGN files often contain more data than the simple examples just shown.
PGN files of online games will often store data from the clocks for
timed games, or game analyses by chess engines. Here’s an example from a
PGN file containing both of these types of data.

``` r
pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'short_game.pgn'
)
pgn <- get_pgn(pgn_path)
strwrap(pgn$Movetext, 70)
#> [1] "1. e4 { [%eval 0.05] [%clk 0:05:00] } 1... e5 { [%eval 0.21] [%clk"   
#> [2] "0:05:00] } 2. d4 { [%eval 0.0] [%clk 0:05:07] } 2... f6 { [%eval"     
#> [3] "1.94] [%clk 0:05:02] } 3. dxe5 { [%eval 1.43] [%clk 0:05:13] } 3..."  
#> [4] "fxe5 { [%eval 8.27] [%clk 0:05:07] } 4. Qh5+ { [%eval 8.55] [%clk"    
#> [5] "0:05:21] } 4... Ke7 { [%eval 10.78] [%clk 0:05:05] } 5. Qxe5+ {"      
#> [6] "[%eval 10.82] [%clk 0:05:22] } 5... Kf7 { [%eval 10.94] [%clk"        
#> [7] "0:05:12] } 6. Bc4+ { [%eval 12.62] [%clk 0:05:27] } 6... Kg6 { [%eval"
#> [8] "#1] [%clk 0:05:12] } 7. Qf5# { [%clk 0:05:23] } 1-0"
```

We’ll examine how to extract this data in the next section.

To learn more about the PGN format, see the [PGN
Specification](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm)
and the [PGN Specification
Supplement](http://www.enpassant.dk/chess/palview/enhancedpgn.htm).

(Note that `get_pgn()` is a fairly bare bones function. It has no
options to limit which tags are read, and has no support for large PGN
files. For a PGN reader in R with more features, check out the function
`bigchess::read.pgn()`.)

### Extracting Data from Imported PGN Files

As we saw in the last section, PGN files may store commentary,
annotations, clock data, engine analysis, or other content interspersed
among the moves of the game. In this section, we’ll examine ways to
extract some of this data.

rbitr has two ways to extract the moves of the game from an imported PGN
file. The function `clean_movetext()` removes any comments, annotations,
embedded data, or extra white space, leaving just the moves and the move
numbers. PGN files store moves in standard algebraic notation (SAN), so
the end result after using `clean_movetext()` is in a format easily read
by most human chess players.

``` r
movetext <- clean_movetext(pgn$Movetext)
movetext
#> [1] "1. e4 e5 2. d4 f6 3. dxe5 fxe5 4. Qh5+ Ke7 5. Qxe5+ Kf7 6. Bc4+ Kg6 7. Qf5#"
```

Communicating chess moves to a chess engine through the UCI interface
requires a different format know as long algebraic notation (LAN). This
format is somewhat less readable for humans, since it leaves out move
numbers and does not use letters or symbols to identify the pieces. The
function `get_moves()` removes everything that `clean_movetext()`
removes, but also removes move numbers, splits the moves, and converts
them to LAN format. (The conversion to LAN is done behind the scenes
using the function bigchess::san2lan().)

``` r
moves <- get_moves(pgn$Movetext)
moves
#> [[1]]
#>  [1] "e2e4" "e7e5" "d2d4" "f7f6" "d4e5" "f6e5" "d1h5" "e8e7" "h5e5" "e7f7"
#> [11] "f1c4" "f7g6" "e5f5"
```

rbitr can also extract the times or the evaluations using `get_clocks()`
or `get_evals()`.

``` r
clocks <- get_clocks(pgn$Movetext)
clocks
#> [[1]]
#>  [1] 300 300 307 302 313 307 321 305 322 312 327 312 323
evals <- get_evals(pgn$Movetext)
evals
#> [[1]]
#>  [1]     5    21     0   194   143   827   855  1078  1082  1094  1262 50000
```

Note that the clock data has been converted from minutes to seconds, and
the evaluations have been converted from pawns to centipawns.

Having the clock data is nice, but what we usually care about is the
time taken for each move. To calculate this, we need to know if the time
control for the game included an increment, so we’ll look at the
TimeControl tag from the PGN file.

``` r
pgn$TimeControl
#> [1] "300+8"
```

We can see that in this case, the time control was 300 seconds plus an
additional 8 seconds per move. For PGN files with a lot of games, it is
convenient to be able to extract the increments using the function
`get_incrememnts()`. Here there’s just one game, but we’ll demonstrate
the use of `get_increments()` anyway.

``` r
increments <- get_increments(pgn$TimeControl)
increments
#> [1] 8
```

Once we have that in hand, it is now possible to calculate the move
times using `get_move_times()`.

``` r
white_move_times <- get_move_times(clocks[[1]], increments[[1]], 'white')
white_move_times
#> [1]  0  1  2  0  7  3 12
black_move_times <- get_move_times(clocks[[1]], increments[[1]], 'black')
black_move_times
#> [1]  0  6  3 10  1  8
```

Note that since the PGN file can contain more than one game, `clocks`
and `increments` are returned as lists. Since we are dealing with the
first (and only) game in this PGN file, we had to specify the first
elements of `clocks` and `increments` when calling `get_move_times()`.

There is some variation in how clocks are treated when it comes to each
player’s first move. According to FIDE’s [laws of
chess](https://www.fide.com/FIDE/handbook/LawsOfChess.pdf), white’s
clock is started before white plays their first move. On the website
lichess.org, neither player’s clock starts until after they play their
first move (i.e., each player’s first move is counted as 0 seconds).
Since PGN files containing over-the-board games rarely include data for
the clocks, rbitr currently follows the convention that each player’s
first move counts as 0 seconds.

### Plotting Move Times

To visualize move times over the course of the game, rbitr borrows a
scaling function from the website [lichess.org](lichess.org). The
logarithmic scaling function for [move
times](https://lichess.org/blog/WOEVrjAAALNI-fWS/a-better-game-clock-history)
ensures that rapid moves made in long games don’t disappear into the
baseline of the plot. This same scaling function is used in rbitr’s
`scaled_time_plot()`.

``` r
scaled_time_plot(white_move_times, black_move_times)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

In chess, each time a player moves a piece is considered a “half-move”.
So, in the strictest sense, anywhere we’ve said “move time”, we’ve
actually been referring to “half-move times”, and the x-axis on the
scaled time plot is given in half-moves. The plot begins with white’s
first half-move, and black’s half-move times are plotted with the
opposite sign of white’s.

### Plotting Evaluations

Scaling is also useful when plotting engine evaluations, because the
players may place less value on material in proportion to how imbalanced
the position is. In other words, losing a pawn when you are up a queen
has much less impact on your game than it does when material is even. To
reflect this, rbitr again borrows a scaling function from
[lichess](https://lichess.org/blog/WFvLpiQAACMA8e9D/learn-from-your-mistakes).
The function `scaled_advantage_plot()` plots an engine’s evaluations of
the game’s positions after applying this scaling function.

Before making an advantage plot, there is one wrinkle to address. A
chess engine will provide an evaluation for each position in a chess
game, including the first position, and also for mated positions (where
the evaluation is ‘mate 0’). PGN files do not generally store an
engine’s evaluation of the first position. If desired, this initial
evaluation can be supplied to the `get_evals()` function so that the
output is in sync with what an engine would produce. Also, `get_evals()`
will only provide a centipawn value for the final mated position if the
`mate0` parameter is set to TRUE.

``` r
evals <- get_evals(pgn$Movetext, first_eval = 15, mate0 = TRUE)
scaled_advantage_plot(evals[[1]])
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

The sign convention used here is that positive evaluations mean that
white has the advantage, while negative evaluations mean that black has
the advantage. The x-axis is numbered starting with 1 for the first
evaluation that was supplied (in this case, for the initial position).
There is one more position than there were half-moves, as expected,
since half-moves can only occur in between adjacent positions.

Notice that we had to specify that we want the evaluations for game 1,
because like `get_clocks()` and `get_increments()`, the function
`get_evals()` returns a list of results, with each list entry
corresponding to a different game in the original PGN file.

### Analyzing Games Using an External Chess Engine

Next, we’ll look at how to analyze games using an external chess engine.
To analyze games, we’ll use the function `evaluate_game()`. In order to
use the function, you will need to provide the path to the engine you’ll
be using for the game analysis. In the next line of code, you’ll need to
change the path as required by your engine location and operating
system.

``` r
engine_path <- '//stockfish_13_win_x64_bmi2.exe'
```

Depending on your processor speed, this step may take some time. If you
have more than one CPU, you can speed up the analysis by changing the
`n_cpus` parameter. In this example, the default setting of `n_cpus` = 1
is used. We will set the `mute` parameter to `FALSE` in order to monitor
game progress. This will cause the function to print the ply number
before starting to analyze each position. The search termination can be
controlled by limiting the depth, by limiting the total number of
positions or ‘nodes’ evaluated, or by limiting the time. Here we will
set the `limiter` parameter to ‘nodes’, and set the `limit` parameter to
2250000.

``` r
gamelog <- evaluate_game(pgn$Movetext[[1]], engine_path, limiter = 'nodes',
                         limit = 2250000, mute = FALSE)
#> [1] "Analyzing position 1 of 14"
#> [1] "Analyzing position 2 of 14"
#> [1] "Analyzing position 3 of 14"
#> [1] "Analyzing position 4 of 14"
#> [1] "Analyzing position 5 of 14"
#> [1] "Analyzing position 6 of 14"
#> [1] "Analyzing position 7 of 14"
#> [1] "Analyzing position 8 of 14"
#> [1] "Analyzing position 9 of 14"
#> [1] "Analyzing position 10 of 14"
#> [1] "Analyzing position 11 of 14"
#> [1] "Analyzing position 12 of 14"
#> [1] "Analyzing position 13 of 14"
#> [1] "Analyzing position 14 of 14"
gamelog[[1]]
#>  [1] "Stockfish 13 by the Stockfish developers (see AUTHORS file)"                                                                                                                                                                         
#>  [2] "readyok"                                                                                                                                                                                                                             
#>  [3] "readyok"                                                                                                                                                                                                                             
#>  [4] "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled"                                                                                                                                                                      
#>  [5] "info depth 1 seldepth 1 multipv 1 score cp 29 nodes 20 nps 20000 tbhits 0 time 1 pv d2d4"                                                                                                                                            
#>  [6] "info depth 2 seldepth 2 multipv 1 score cp 89 nodes 42 nps 42000 tbhits 0 time 1 pv d2d4 a7a6"                                                                                                                                       
#>  [7] "info depth 3 seldepth 3 multipv 1 score cp 65 nodes 84 nps 84000 tbhits 0 time 1 pv e2e4 e7e6 d2d4"                                                                                                                                  
#>  [8] "info depth 4 seldepth 4 multipv 1 score cp 114 nodes 241 nps 120500 tbhits 0 time 2 pv g1f3 d7d5"                                                                                                                                    
#>  [9] "info depth 5 seldepth 5 multipv 1 score cp -2 nodes 572 nps 286000 tbhits 0 time 2 pv g1f3 d7d5 d2d4 g8f6 b1d2"                                                                                                                      
#> [10] "info depth 6 seldepth 6 multipv 1 score cp 9 nodes 741 nps 370500 tbhits 0 time 2 pv g1f3 d7d5 d2d4 g8f6 b1d2"                                                                                                                       
#> [11] "info depth 7 seldepth 7 multipv 1 score cp 4 nodes 1169 nps 389666 tbhits 0 time 3 pv g1f3 d7d5 d2d4 g8f6 c1e3 b8d7"                                                                                                                 
#> [12] "info depth 8 seldepth 10 multipv 1 score cp 21 nodes 1849 nps 616333 tbhits 0 time 3 pv e2e4 c7c5 g1f3 b8c6 d2d4 c5d4"                                                                                                               
#> [13] "info depth 9 seldepth 10 multipv 1 score cp 40 nodes 3894 nps 973500 tbhits 0 time 4 pv d2d4 d7d5 c2c4 d5c4 g1f3 g8f6 b1c3"                                                                                                          
#> [14] "info depth 10 seldepth 12 multipv 1 score cp 38 nodes 10144 nps 1268000 tbhits 0 time 8 pv e2e4 c7c5 g1f3 e7e6 d2d4 c5d4 f3d4 g8f6"                                                                                                  
#> [15] "info depth 11 seldepth 16 multipv 1 score cp 31 nodes 29662 nps 1348272 tbhits 0 time 22 pv e2e4 e7e5 g1f3 b8c6 f1c4 f8c5 e1g1 d7d6 h2h3 c6d4 f3d4 c5d4"                                                                             
#> [16] "info depth 12 seldepth 20 multipv 1 score cp 17 nodes 57232 nps 1430800 tbhits 0 time 40 pv e2e4 e7e5 b1c3 g8f6 f1c4 f8c5 g1f3 d7d6 e1g1 c8e6 c4e6 f7e6 d2d4 e5d4 f3d4"                                                              
#> [17] "info depth 13 seldepth 19 multipv 1 score cp 34 nodes 89986 nps 1406031 tbhits 0 time 64 pv e2e4 c7c5 b1c3 b8c6 f1b5 g8f6 b5c6 b7c6 e4e5 f6d5"                                                                                       
#> [18] "info depth 14 seldepth 15 multipv 1 score cp 43 nodes 114602 nps 1397585 tbhits 0 time 82 pv e2e4 e7e5 g1f3 b8c6 f1b5 f8c5 e1g1 g8f6 b5c6 d7c6 d2d3 d8e7 a2a4"                                                                       
#> [19] "info depth 15 seldepth 21 multipv 1 score cp 27 nodes 263818 nps 1410791 tbhits 0 time 187 pv d2d4 g8f6 c2c4 e7e6 g1f3 d7d5 b1c3 c7c5 c1g5 d5c4 e2e3 b8c6 f1c4 c5d4 e3d4"                                                            
#> [20] "info depth 16 seldepth 21 multipv 1 score cp 42 nodes 319442 nps 1426080 tbhits 0 time 224 pv d2d4 d7d5 c2c4 e7e6 b1c3 h7h6 g1f3 g8f6 e2e3 c7c5 c4d5 e6d5 a2a3 c5d4 e3d4 b8c6"                                                       
#> [21] "info depth 17 seldepth 21 multipv 1 score cp 27 nodes 539694 nps 1409122 tbhits 0 time 383 pv d2d4 d7d5 c2c4 e7e6 g1f3 g8f6 b1c3 f8b4 c1g5 h7h6 g5f6 d8f6 d1b3 c7c5 c4d5 e6d5 d4c5 b4c3 b2c3"                                        
#> [22] "info depth 18 seldepth 23 multipv 1 score cp 32 nodes 605175 nps 1404118 tbhits 0 time 431 pv d2d4 d7d5 c2c4 e7e6 g1f3 g8f6 b1c3 c7c5 c1g5 c5d4 f3d4 d5c4 e2e3 b8c6 d4c6 d8d1 a1d1 b7c6 f1c4 f8e7 e3e4 a8b8"                         
#> [23] "info depth 19 seldepth 31 multipv 1 score cp 35 nodes 849949 nps 1397942 tbhits 0 time 608 pv d2d4 g8f6 c2c4 e7e6 g1f3 d7d5 b1c3 f8e7 c1f4 e8g8 e2e3 b8d7 a2a3 c7c5 c4d5 f6d5 c3d5 e6d5 d4c5 d7c5"                                   
#> [24] "info depth 20 seldepth 26 multipv 1 score cp 21 nodes 1504284 nps 1378812 hashfull 631 tbhits 0 time 1091 pv d2d4 g8f6 c2c4 e7e6 b1c3 f8b4 e2e3 e8g8 c1d2 d7d5 g1f3 c7c5 a2a3 b4c3 d2c3 f6e4 c4d5 e4c3 b2c3 e6d5 d4c5 d8a5 a1c1 a5a3"
#> [25] "info depth 21 seldepth 29 multipv 1 score cp 12 nodes 2250910 nps 1372506 hashfull 814 tbhits 0 time 1640 pv d2d4 g8f6"                                                                                                              
#> [26] "bestmove d2d4 ponder g8f6"
```

The result of the analysis is a list of raw chess engine output for each
position in the game. The output can get to be quite verbose; we’ve only
displayed the output for the first position, and it’s already quite
long. We see the name of the engine that was used, some messages that
the engine was ready, a line telling us that the engine-specific NNUE
option was turned on, and a number of lines showing evaluations at
various depths. The last line tells us the best move that was found, and
the move that the engine would “ponder” on the opponent’s time, if it
was in a game. We’ll show how to extract the data we want from the
engine output in the next section.

(Note that the `evaluate_game()` and `parse_gamelog()` functions
essentially duplicate features that are available in the bigchess
package. I overlooked the existence of those features, and wound up
writing my own code, thus proving that “weeks of coding will save you
hours of planning.” The silver lining is that, for whatever reason,
`evaluate_game()` runs about 3-4 times faster than
`bigchess::analyze_game()` so I’ve kept it in. There’s a number of other
places where rbitr’s features overlap with bigchess, but those are more
deliberate decisions having to do with creating the types of work flow
that I wanted for rbitr.)

Batch analysis of games is also possible using the function
`evaluate_pgn()`, which is essentially a fancy wrapper for
`evaluate_game()`. The results of `evaluate_pgn()` are not saved to disk
by default. However, deep analysis of large number of games can take a
*very* long time. It is highly recommended to set the `save_logs`
parameter to `TRUE`. That way, if the analysis has to be interrupted,
you will not lose what could be many hours of analysis, and can resume
the analysis later, if desired. The analyses will be saved to a
directory with the same base name as the PGN file.

``` r
pgnlog <- evaluate_pgn(two_games_path, engine_path,
                       limiter = 'nodes', limit = 2250000)
#> [1] "game 1 of 2"
#> [1] "estimated time remaining 7.4 secs"
#> [1] "game 2 of 2"
#> [1] "estimated time remaining 0 secs"
```

### Extracting Data from Chess Engine Logs

The `parse_gamelog()` function can extract evaluations (‘score’), the
best moves the engine found (‘bestmove’), or the principal variations
(‘pv’).

``` r
bestmoves <- unlist(parse_gamelog(gamelog, 'bestmove'))
bestmoves
#>  [1] "d2d4" "e7e5" "g1f3" "e5d4" "f1c4" "b8c6" "d1h5" "g7g6" "h5e5" "e7f7"
#> [11] "f1c4" "d7d5" "e5f5" ""
scores <- unlist(parse_gamelog(gamelog, 'score'))
scores
#>  [1] "12"     "-18"    "29"     "8"      "240"    "-160"   "834"    "-809"  
#>  [9] "1078"   "-1035"  "1082"   "-1048"  "mate 1" "mate 0"
pvs <- unlist(parse_gamelog(gamelog, 'pv'))
pvs
#>  [1] "d2d4 g8f6"                                                                                                                                                           
#>  [2] "e7e5 g1f3 b8c6 f1c4 g8f6 d2d3 f8c5 b1c3 h7h6 e1g1 a7a6 c3e2 d7d6 c2c3 c5b6 c4b3 e8g8 e2g3 c8e6 d3d4 e5d4 c3d4 e6b3 a2b3 f8e8 f1e1"                                   
#>  [3] "g1f3 b8c6"                                                                                                                                                           
#>  [4] "e5d4 d1d4"                                                                                                                                                           
#>  [5] "f1c4 b8c6 g1f3 d7d6 c2c3 h7h5 b2b4 g7g5 d1b3 g8h6 a2a4 d8d7 b1a3 d7h7 d4e5 h7e4 c1e3 c6e5 f3e5 d6e5"                                                                 
#>  [6] "b8c6 g1f3"                                                                                                                                                           
#>  [7] "d1h5 g7g6 h5e5 d8e7 e5h8 e7f7 h8e5 f7e7 e5e7 g8e7 b1c3 f8g7 f1c4 g7c3 b2c3 b7b6 g1f3 c8a6 c4d3 a6b7 e1g1 b8a6 c1e3 a6c5 e3c5 b6c5 a1e1 d7d6 f3g5 e8c8"               
#>  [8] "g7g6 h5e5 d8e7 e5h8 e7f7 h8e5 f7e7 e5e7 g8e7 b1c3 b8c6 c3b5 e8d8 c1e3 f8g7 e1c1 a7a6 b5c3 d7d6 g1f3 g7c3 b2c3 c8e6 c1b2 c6a5 e4e5 d8e8 e5d6 c7d6 f3g5 a5c4 f1c4 e6c4"
#>  [9] "h5e5 e7f7"                                                                                                                                                           
#> [10] "e7f7 f1c4 d7d5 c4d5 d8d5 e5d5 f7e8 d5e5 g8e7 e5c7 b8a6 c7c4 c8d7 g1f3 a8c8 c4e2 a6c5 e1g1 d7g4 h2h3 g4f3 e2f3 c5d7 b1c3 e7g6 e4e5 c8c6 e5e6 d7e5 f3f5 f8b4 c3d5 c6c4"
#> [11] "f1c4 d7d5"                                                                                                                                                           
#> [12] "d7d5 c4d5 d8d5 e5d5 f7e8 d5e5 g8e7 e5c7 b8a6 c7c4 c8d7 b1c3 a8c8 c4e2 a6c5 c1e3 b7b6 e1c1 e7c6 h2h3 g7g6 g1f3 f8g7 f3g5 c5e6 f2f3 e6g5 e3g5 c6d4"                    
#> [13] "e5f5"                                                                                                                                                                
#> [14] NA
```

Note that the output of parse\_gamelog is a list. That’s because games
may be analyzed in multi-PV mode, resulting in multiple results for
scores, bestmoves, and pvs at each position in the game. Here we
analyzed in single PV mode, so we don’t have to get fancy trying to
extract just the data for the PVs we are interested in. Instead, we can
just unlist the results.

The parsed scores are a character vector, rather than numeric. There are
also a couple of scores that are in the form ‘mate x’. We can use the
function `convert_scores()` to assign a centipawn value for ‘mate x’,
and convert the rest to numeric values.

``` r
evals <- convert_scores(scores)
evals
#>  [1]   12   18   29   -8  240  160  834  809 1078 1035 1082 1048 5000 5000
```

The output from `evaluate_pgn()` will be a list of game logs from the
`evaluate_game()` function. The logs can be parsed individually with
`parse_gamelog()`, or in a batch using `parse_pgnlog()`.

``` r
pgn_scores <- parse_pgnlog(pgnlog, 'score')
lapply(pgn_scores, unlist)
#> [[1]]
#> [1] "12"     "-18"    "226"    "-228"   "mate 1" "mate 0"
#> 
#> [[2]]
#> [1] "12"     "-18"    "215"    "-55"    "mate 1" "mate 0"
pgn_bestmoves <- parse_pgnlog(pgnlog, 'bestmove')
lapply(pgn_bestmoves, unlist)
#> [[1]]
#> [1] "d2d4" "e7e5" "b1c3" "e7e6" "d1h5" ""    
#> 
#> [[2]]
#> [1] "d2d4" "e7e5" "e4f5" "f5e4" "d1h5" ""
pgn_pvs <- parse_pgnlog(pgnlog, 'pv')
lapply(pgn_pvs, unlist)
#> [[1]]
#> [1] "d2d4 g8f6"                                                                                                                        
#> [2] "e7e5 g1f3 b8c6 f1c4 g8f6 d2d3 f8c5 b1c3 h7h6 e1g1 a7a6 c3e2 d7d6 c2c3 c5b6 c4b3 e8g8 e2g3 c8e6 d3d4 e5d4 c3d4 e6b3 a2b3 f8e8 f1e1"
#> [3] "b1c3 e7e6"                                                                                                                        
#> [4] "e7e6 d2d4"                                                                                                                        
#> [5] "d1h5"                                                                                                                             
#> [6] NA                                                                                                                                 
#> 
#> [[2]]
#> [1] "d2d4 g8f6"                                                                                                                                  
#> [2] "e7e5 g1f3 b8c6 f1c4 g8f6 d2d3 f8c5 b1c3 h7h6 e1g1 a7a6 c3e2 d7d6 c2c3 c5b6 c4b3 e8g8 e2g3 c8e6 d3d4 e5d4 c3d4 e6b3 a2b3 f8e8 f1e1"          
#> [3] "e4f5 g8f6"                                                                                                                                  
#> [4] "f5e4 d2d3 e4d3 f1d3 g8f6 g1f3 d7d5 f3g5 e7e6 d1e2 b8c6 g5e6 c8e6 e2e6 d8e7 e6e7 f8e7 c1g5 e8c8 e1c1 h8f8 h1e1 f6g8 f2f4 g8f6 g5f6 e7f6 d3h7"
#> [5] "d1h5"                                                                                                                                       
#> [6] NA
```

### Determining Inaccuracies, Mistakes, and Blunders

Next, we’ll take a look at finding inaccuracies, mistakes, and blunders
using `get_imb()`. These are identified by looking at how the evaluation
changes after a move is made. A big loss would count as a blunder, a
smaller loss as a mistake, and so on. However, a move doesn’t count as
an error if it was the best available move, even if the evaluation drops
heavily after the move. There fore, `get_imb()` needs to compare the
moves of the game to the best moves found by the engine.

``` r
white_imb <- get_imb(evals, moves[[1]], bestmoves, 'white')
white_imb
#> $inaccuracies
#> [1] 3
#> 
#> $mistakes
#> integer(0)
#> 
#> $blunders
#> integer(0)
black_imb <- get_imb(evals, moves[[1]], bestmoves, 'black')
black_imb
#> $inaccuracies
#> integer(0)
#> 
#> $mistakes
#> integer(0)
#> 
#> $blunders
#> [1] 2 3
```

Now we can see that according to the engine, white played an inaccuracy
on move 3, and black blundered moves 2 and 3. Also, note that we still
had to specify the game number for `moves[[1]]` because it is a list
taken from a PGN file, and these files often contain multiple games.
However, `bestmoves` and `evals` came from `parse_gamelog()` which only
ever operates on a single game at a time.

### Average Centipawn Loss

Average centipawn loss is a measure of how bad a player’s moves were
compared to the “best” moves in the same positions. The evaluation of a
strong chess engine is used to determine which moves are the “best”.
When a player chooses a move that is not the engine’s favorite, the
difference in the evaluations of the two moves is the “loss” for that
move. The loss is averaged over all moves to get the average centipawn
loss (ACPL). In practice, rather than having the engine evaluate the
position after both the engine’s move and the played move, the loss is
just taken to be the drop in evaluation after the played move. This is
really only an approximation, because the evaluation after the move has
been played is now one ply deeper, but this approximation halves the
analysis time.

Not every source calculates average centipawn loss using the same rules.
Therefore, `get_acpl()` has options to control the method of
calculation. The `cap` parameter sets a threshold for evaluations to
avoid having one bad move dominate the final value. Some sources will
discard values outside the range \[-cap, cap\], while others will
replace values outside the range with ±cap. This behavior is controlled
using the `cap_action` parameter.

``` r
white_acpl <- get_acpl(evals, 'white', cap = 1000, cap_action = 'replace')
white_acpl
#> [1] 20
black_acpl <- get_acpl(evals, 'black', cap = 1000, cap_action = 'replace')
black_acpl
#> [1] 187
```

### Summarizing Game Data

The function `game_summary_plot()` combines many of the features that
were introduced above into a single graphic. To specify a game to
summarize, we need only provide the path to a PGN file containing the
game, and indicate which game in the file to summarize. In this case,
there is only one game, so we set the game\_number parameter to 1. We
can also choose whether or not to use any evaluations that are already
present in the file.

``` r
game_summary_plot(pgn_path, game_number = 1, use_pgn_evals = FALSE)
```

<img src="man/figures/README-unnamed-chunk-21-1.png" width="100%" />

If this plot looks familiar, that’s because it was inspired by the game
analysis functions available on lichess.org. On the left, we see a
scaled move time plot and a scaled advantage plot, both of which we have
already covered.

To the right of the plots are tables summarizing some statistics for
each player. At the top right are statistics for white, beginning with
the white player’s name. Next are the total number of inaccuracies,
mistakes, and blunders. Lastly, the average centipawn loss is shown. The
same stats for black are shown in the bottom right.
