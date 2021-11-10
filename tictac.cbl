000010************************************************************
000020*                                                          *
000030*              (C) Micro Focus Ltd. 1989                   *
000040*                                                          *
000050*                     TICTAC.CBL                           *
000060*                                                          *
000070*    This program demonstrates how to use a CRT.           *
000080*                                                          *
000090************************************************************
000100* 2017-06 - Converting to GNU COBOL 2.0                    *
000110*           Jerry D. Farrell                               *
000120*           The University of Tennessee                    *
000130************************************************************
000140 identification division.
000150     program-id. tictac.
000160 environment division.
000170 configuration section.
000180     source-computer. ibm-pc.
000190     object-computer. ibm-pc.
000200 special-names.
000210     console is crt.
000220 data division.
000230 working-storage section.
000240 01 tictac-00.
000250  02 tictac-q.
000260     03 game           pic x(10) value spaces.
000270     03 filler-0       pic x(70) value spaces.
000280     03 question       pic x(20) value spaces.
000290  02 filler.
000300     03 filler-1       pic x(414) value all spaces.
000310     03 tictac-00-0735 pic x(17) value "7บ      8บ      9".
000320     03 filler-2       pic x(64) value all spaces.
000330     03 tictac-00-0836 pic x(09) value "บ       บ".
000340     03 filler-3       pic x(71) value all spaces.
000350     03 tictac-00-0936 pic x(09) value "บ       บ".
000360     03 filler-4       pic x(64) value all spaces.
000370     03 tictac-00-1029 pic x(23) value "อออออออฮอออออออฮอออออออ".
000380     03 filler-5       pic x(63) value all spaces.
000390     03 tictac-00-1135 pic x(17) value "4บ      5บ      6".
000400     03 filler-6       pic x(64) value all spaces.
000410     03 tictac-00-1236 pic x(09) value "บ       บ".
000420     03 filler-7       pic x(71) value all spaces.
000430     03 tictac-00-1336 pic x(09) value "บ       บ".
000440     03 filler-8       pic x(64) value all spaces.
000450     03 tictac-00-1429 pic x(23) value "อออออออฮอออออออฮอออออออ".
000460     03 filler-9       pic x(63) value all spaces.
000470     03 tictac-00-1535 pic x(17) value "1บ      2บ      3".
000480     03 filler-10      pic x(64) value all spaces.
000490     03 tictac-00-1636 pic x(09) value "บ       บ".
000500     03 filler-11      pic x(71) value all spaces.
000510     03 tictac-00-1736 pic x(09) value "บ       บ".
000520     03 filler-12      pic x(595) value all spaces.
000530 01 entry-array.
000540     03 entry-char     pic x       occurs 9 times.
000550 01 check-array.
000560     03 check          pic s99     comp  occurs 9 times.
000570 01 xcount             pic 9(2)    comp.
000580 01 ocount             pic 9(2)    comp.
000590 01 factor             pic s9(2)   comp.
000600 01 char               pic x.
000610 01 char9 redefines char pic 9.
000620 01 idx                pic 9(2)    comp.
000630 01 result             pic 9(2)    comp.
000640 01 cursor-pos.
000650     03 row              pic 9(2)    comp  value 99.
000660     03 filler           pic 9(2)    comp  value 99.
000670 01 address-init.
000680     03 filler           pic 9(4)    value   1735.
000690     03 filler           pic 9(4)    value   1743.
000700     03 filler           pic 9(4)    value   1751.
000710     03 filler           pic 9(4)    value   1335.
000720     03 filler           pic 9(4)    value   1343.
000730     03 filler           pic 9(4)    value   1351.
000740     03 filler           pic 9(4)    value   0935.
000750     03 filler           pic 9(4)    value   0943.
000760     03 filler           pic 9(4)    value   0951.
000770 01 address-array        redefines   address-init.
000780     03 addr             pic 9(4)    occurs 9 times.
000790 01 location             pic 9(4).
000800 01 game-lines value     "147123311113332436978979".
000810     03 a                pic 9       occurs 8 times.
000820     03 b                pic 9       occurs 8 times.
000830     03 c                pic 9       occurs 8 times.
000840 01 i                    pic 9(2)    comp.
000850 01 j                    pic 9(2)    comp.
000860 01 moves                pic 9(2)    comp.
000870
000880 78 clear-screen        value x"e4".
000890 78 sound-bell          value x"e5".
000900
000910 procedure division.
000920 play-game section.
000930 play-1.
000940     perform with test after
000950         until char not = "Y" and char not = "y"
000960         call clear-screen
000970         display
000980             "To select a square type a number between 1 and 9"
000990             upon crt
001000         perform init
001010         move "Shall I start ? " to question
001020         perform get-reply
001030         if char = "Y" or char = "y"
001040             move 10 to check(5)
001050             perform put-move
001060         end-if
001070         perform new-move until game not = spaces
001080         move "Play again ?    " to question
001090         perform get-reply
001100     end-perform.
001110
001120 play-stop.
001130     display space
001140     stop run.
001150
001160 get-reply section.
001170     display tictac-q at 0201
001180     accept char at 0317 with no-echo auto-skip
001190     move spaces to question
001200     display tictac-00 at 0201.
001210
001220 init section.
001230     move "y" to char
001240     move spaces to entry-array
001250     move low-values to check-array
001260     move spaces to game
001270     move zero to moves.
001280
001290 new-move section.
001300     perform get-move with test after until char9 not = 0
001310     perform move-check
001320     if game not = "stalemate"
001330         move low-values to check-array
001340         perform check-line varying i from 1 by 1
001350                         until i > 8 or game not = spaces
001360         if game not = "You win"
001370             perform put-move
001380         end-if
001390         if game = "I win" or game = "You win"
001400               perform varying idx from a(j) by b(j)
001410                                          until idx > c(j)
001420                   move addr(idx) to location
001430                   move entry-char(idx) to char
001440                   display char at location with blink highlight
001450               end-perform
001460         end-if
001470     end-if.
001480
001490 check-line section.
001500     move zero to xcount,ocount,factor
001510     perform count-up varying idx from a(i) by b(i)
001520                                      until idx > c(i)
001530     if ocount = 0 or xcount = 0
001540         evaluate true
001550         when ocount = 2
001560             if i = 4
001570                 move 6 to j
001580                 move zero to xcount,ocount
001590                 perform count-up varying idx from a(j) by b(j)
001600                                          until idx > c(j)
001610                 if xcount = 3
001620                     move 6 to i
001630                 end-if
001640             end-if
001650             if xcount not = 3
001660                 move 50 to factor
001670                 move "I win" to game
001680                 move i to j
001690             end-if
001700         when xcount = 2
001710             move 20 to factor
001720         when ocount = 1
001730             move  4 to factor
001740         when xcount = 1
001750             if entry-char(5) = "x"
001760                 move  1 to factor
001770             else
001780                 move -1 to factor
001790             end-if
001800         when ocount = 0
001810             if xcount = 0
001820                 move  2 to factor
001830             end-if
001840         end-evaluate
001850     end-if
001860     if xcount = 3
001870         move "You win" to game
001880         move i to j
001890     else
001900         perform varying idx from a(i) by b(i) until idx > c(i)
001910             if entry-char(idx) = space
001920                 add factor to check(idx)
001930             end-if
001940         end-perform
001950     end-if.
001960
001970 count-up section.
001980     if entry-char(idx) = "X"        add 1 to xcount
001990     else if entry-char(idx) = "O"   add 1 to ocount.
002000
002010 put-move section.
002020     move zero to idx
002030     move -99 to factor
002040     perform find-pos varying i from 1 by 1 until i > 9
002050     move "O" to entry-char(idx)
002060     perform move-check.
002070
002080 move-check section.
002090     move addr(idx) to location
002100     move entry-char(idx) to char
002110     display char at location
002120     add 1 to moves
002130     if moves > 8 and game = spaces
002140         move "stalemate" to game
002150     end-if.
002160
002170 find-pos section.
002180     if entry-char(5) = space
002190         move check(5) to factor
002200         move 5 to idx
002210     else
002220         if check(i) not < factor and entry-char(i) = space
002230             move check(i) to factor
002240             move i to idx
002250         end-if
002260     end-if.
002270
002280 get-move section.
002290     display "Please select an empty square" at 0201
002300     move 0 to char9
002310     accept char9 at 0231 with auto-skip
002320     if char9 = 0
002330         call sound-bell
002340     else
002350         move char9 to idx
002360         if entry-char(idx) = space
002370             move "X" to entry-char(idx)
002380         else
002390             move 0 to char9
002400             call sound-bell
002410         end-if
002420     end-if.
