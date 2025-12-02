       IDENTIFICATION DIVISION.
       PROGRAM-ID. TIC-TAC-TOE-3D.
       AUTHOR. Jerry D Farrell.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *> **************************************************
      *> Compile USING cobc -x -free <name>.cbl -o <name>
      *> **************************************************
       01  GAME-BOARD.
           05  LAYER OCCURS 3 TIMES.
               10  ROW OCCURS 3 TIMES.
                   15  CELL OCCURS 3 TIMES PIC X VALUE SPACE.
       
       01  PLAYER-SYMBOL PIC X VALUE "X".
       01  COMPUTER-SYMBOL PIC X VALUE "O".
       01  CURRENT-PLAYER PIC X VALUE "X".
       01  GAME-OVER PIC X VALUE "N".
       01  WINNER PIC X VALUE SPACE.
       
       01  INPUT-COORDS.
           05  LAYER-INPUT PIC 9.
           05  ROW-INPUT PIC 9.
           05  COL-INPUT PIC 9.
       
       01  MOVE-COUNT PIC 99 VALUE 0.
       01  I PIC 9.
       01  J PIC 9.
       01  K PIC 9.
       01  VALID-MOVE PIC X.
       
       01  CHOICE PIC X.
       01  DIFFICULTY PIC 9 VALUE 1.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM DISPLAY-INSTRUCTIONS
           PERFORM INITIALIZE-GAME
           PERFORM GAME-LOOP UNTIL GAME-OVER = "Y"
           PERFORM DISPLAY-FINAL-RESULTS
           STOP RUN.
       
       DISPLAY-INSTRUCTIONS.
           DISPLAY " "
           DISPLAY "=================================="
           DISPLAY "   3D TIC TAC TOE - COBOL EDITION"
           DISPLAY "=================================="
           DISPLAY " "
           DISPLAY "GAME RULES:"
           DISPLAY "- The game is played on a 3x3x3 cube"
           DISPLAY "- You are X, Computer is O"
           DISPLAY "- Get 3 in a row to win (any direction)"
           DISPLAY "- Valid directions include:"
           DISPLAY "  * Rows, columns, pillars"
           DISPLAY "  * Face diagonals (on each layer)"
           DISPLAY "  * Space diagonals (through the cube)"
           DISPLAY " "
           DISPLAY "COORDINATES:"
           DISPLAY "- Layer: 1-3 (1=top, 3=bottom)"
           DISPLAY "- Row: 1-3 (top to bottom on layer)"
           DISPLAY "- Col: 1-3 (left to right)"
           DISPLAY " "
           DISPLAY "Press ENTER to start..."
           ACCEPT CHOICE
           .
       
       INITIALIZE-GAME.
           MOVE "N" TO GAME-OVER
           MOVE SPACE TO WINNER
           MOVE 0 TO MOVE-COUNT
           MOVE "X" TO CURRENT-PLAYER
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
                   PERFORM VARYING K FROM 1 BY 1 UNTIL K > 3
                       MOVE SPACE TO CELL(I, J, K)
                   END-PERFORM
               END-PERFORM
           END-PERFORM
           .
       
       GAME-LOOP.
           PERFORM DISPLAY-BOARD
           
           IF CURRENT-PLAYER = "X"
               PERFORM GET-PLAYER-MOVE
           ELSE
               PERFORM COMPUTER-MOVE
           END-IF
           
           ADD 1 TO MOVE-COUNT
           PERFORM CHECK-WINNER
           
           IF GAME-OVER = "N"
               IF MOVE-COUNT >= 27
                   MOVE "Y" TO GAME-OVER
                   DISPLAY " "
                   DISPLAY "GAME OVER - IT'S A DRAW!"
               ELSE
                   IF CURRENT-PLAYER = "X"
                       MOVE "O" TO CURRENT-PLAYER
                   ELSE
                       MOVE "X" TO CURRENT-PLAYER
                   END-IF
               END-IF
           END-IF
           .
       
       DISPLAY-BOARD.
           DISPLAY " "
           DISPLAY "=================================="
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               DISPLAY " "
               DISPLAY "LAYER " I ":"
               DISPLAY "     1   2   3"
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
                   DISPLAY "  " J " " 
                       CELL(I, J, 1) " | " 
                       CELL(I, J, 2) " | " 
                       CELL(I, J, 3) WITH NO ADVANCING
                   DISPLAY " "
                   IF J < 3
                       DISPLAY "    -----------"
                   END-IF
               END-PERFORM
           END-PERFORM
           DISPLAY "=================================="
           .
       
       GET-PLAYER-MOVE.
           MOVE "N" TO VALID-MOVE
           PERFORM UNTIL VALID-MOVE = "Y"
               DISPLAY " "
               DISPLAY "Your turn (X)"
               DISPLAY "Enter Layer (1-3): " WITH NO ADVANCING
               ACCEPT LAYER-INPUT
               DISPLAY "Enter Row (1-3): " WITH NO ADVANCING
               ACCEPT ROW-INPUT
               DISPLAY "Enter Column (1-3): " WITH NO ADVANCING
               ACCEPT COL-INPUT
               
               IF LAYER-INPUT >= 1 AND LAYER-INPUT <= 3
                   IF ROW-INPUT >= 1 AND ROW-INPUT <= 3
                       IF COL-INPUT >= 1 AND COL-INPUT <= 3
                           IF CELL(LAYER-INPUT, ROW-INPUT, COL-INPUT) 
                               = SPACE
                               MOVE "X" TO CELL(LAYER-INPUT, 
                                   ROW-INPUT, COL-INPUT)
                               MOVE "Y" TO VALID-MOVE
                           ELSE
                               DISPLAY "That spot is taken! Try again."
                           END-IF
                       ELSE
                           DISPLAY "Invalid column! Use 1-3."
                       END-IF
                   ELSE
                       DISPLAY "Invalid row! Use 1-3."
                   END-IF
               ELSE
                   DISPLAY "Invalid layer! Use 1-3."
               END-IF
           END-PERFORM
           .
       
       COMPUTER-MOVE.
           DISPLAY " "
           DISPLAY "Computer's turn (O)..."
           
           MOVE "N" TO VALID-MOVE
           PERFORM UNTIL VALID-MOVE = "Y"
               COMPUTE I = FUNCTION RANDOM * 3 + 1
               COMPUTE J = FUNCTION RANDOM * 3 + 1
               COMPUTE K = FUNCTION RANDOM * 3 + 1
               
               IF CELL(I, J, K) = SPACE
                   MOVE "O" TO CELL(I, J, K)
                   MOVE "Y" TO VALID-MOVE
                   DISPLAY "Computer chose: Layer " I 
                       ", Row " J ", Col " K
               END-IF
           END-PERFORM
           .
       
       CHECK-WINNER.
           PERFORM CHECK-ROWS
           IF GAME-OVER = "N"
               PERFORM CHECK-COLUMNS
           END-IF
           IF GAME-OVER = "N"
               PERFORM CHECK-PILLARS
           END-IF
           IF GAME-OVER = "N"
               PERFORM CHECK-DIAGONALS
           END-IF
           .
       
       CHECK-ROWS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
                   IF CELL(I, J, 1) NOT = SPACE
                       IF CELL(I, J, 1) = CELL(I, J, 2)
                           IF CELL(I, J, 1) = CELL(I, J, 3)
                               MOVE "Y" TO GAME-OVER
                               MOVE CELL(I, J, 1) TO WINNER
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       
       CHECK-COLUMNS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               PERFORM VARYING K FROM 1 BY 1 UNTIL K > 3
                   IF CELL(I, 1, K) NOT = SPACE
                       IF CELL(I, 1, K) = CELL(I, 2, K)
                           IF CELL(I, 1, K) = CELL(I, 3, K)
                               MOVE "Y" TO GAME-OVER
                               MOVE CELL(I, 1, K) TO WINNER
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       
       CHECK-PILLARS.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
               PERFORM VARYING K FROM 1 BY 1 UNTIL K > 3
                   IF CELL(1, J, K) NOT = SPACE
                       IF CELL(1, J, K) = CELL(2, J, K)
                           IF CELL(1, J, K) = CELL(3, J, K)
                               MOVE "Y" TO GAME-OVER
                               MOVE CELL(1, J, K) TO WINNER
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       
       CHECK-DIAGONALS.
      *> Face diagonals on each layer
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               IF CELL(I, 1, 1) NOT = SPACE
                   IF CELL(I, 1, 1) = CELL(I, 2, 2)
                       IF CELL(I, 1, 1) = CELL(I, 3, 3)
                           MOVE "Y" TO GAME-OVER
                           MOVE CELL(I, 1, 1) TO WINNER
                       END-IF
                   END-IF
               END-IF
               IF CELL(I, 1, 3) NOT = SPACE
                   IF CELL(I, 1, 3) = CELL(I, 2, 2)
                       IF CELL(I, 1, 3) = CELL(I, 3, 1)
                           MOVE "Y" TO GAME-OVER
                           MOVE CELL(I, 1, 3) TO WINNER
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           
      *> Space diagonals through cube
           IF CELL(1, 1, 1) NOT = SPACE
               IF CELL(1, 1, 1) = CELL(2, 2, 2)
                   IF CELL(1, 1, 1) = CELL(3, 3, 3)
                       MOVE "Y" TO GAME-OVER
                       MOVE CELL(1, 1, 1) TO WINNER
                   END-IF
               END-IF
           END-IF
           
           IF CELL(1, 1, 3) NOT = SPACE
               IF CELL(1, 1, 3) = CELL(2, 2, 2)
                   IF CELL(1, 1, 3) = CELL(3, 3, 1)
                       MOVE "Y" TO GAME-OVER
                       MOVE CELL(1, 1, 3) TO WINNER
                   END-IF
               END-IF
           END-IF
           
           IF CELL(1, 3, 1) NOT = SPACE
               IF CELL(1, 3, 1) = CELL(2, 2, 2)
                   IF CELL(1, 3, 1) = CELL(3, 1, 3)
                       MOVE "Y" TO GAME-OVER
                       MOVE CELL(1, 3, 1) TO WINNER
                   END-IF
               END-IF
           END-IF
           
           IF CELL(1, 3, 3) NOT = SPACE
               IF CELL(1, 3, 3) = CELL(2, 2, 2)
                   IF CELL(1, 3, 3) = CELL(3, 1, 1)
                       MOVE "Y" TO GAME-OVER
                       MOVE CELL(1, 3, 3) TO WINNER
                   END-IF
               END-IF
           END-IF
           .
       
       DISPLAY-FINAL-RESULTS.
           PERFORM DISPLAY-BOARD
           DISPLAY " "
           IF WINNER NOT = SPACE
               IF WINNER = "X"
                   DISPLAY "*** CONGRATULATIONS! YOU WIN! ***"
               ELSE
                   DISPLAY "*** COMPUTER WINS! ***"
               END-IF
           END-IF
           DISPLAY " "
           DISPLAY "Thanks for playing!".


