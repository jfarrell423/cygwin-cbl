       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONNECT4.
       AUTHOR. Jerry D Farrell.
*> ****************************************************************
*> Compile USING cobc -x -free <name>.cbl -o <name>
*> **************************************************************** 
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  BOARD.
           05  BOARD-ROW OCCURS 6 TIMES.
               10  BOARD-CELL OCCURS 7 TIMES PIC X VALUE SPACE.
       
       01  GAME-VARS.
           05  CURRENT-PLAYER        PIC X VALUE "1".
           05  COLUMN-CHOICE         PIC 9 VALUE 0.
           05  ROW-POSITION          PIC 9 VALUE 0.
           05  GAME-OVER             PIC X VALUE "N".
           05  WINNER                PIC X VALUE SPACE.
           05  MOVE-COUNT            PIC 99 VALUE 0.
           05  INPUT-STRING          PIC X(10).
       
       01  LOOP-COUNTERS.
           05  ROW-IDX               PIC 9 VALUE 0.
           05  COL-IDX               PIC 9 VALUE 0.
           05  CHECK-IDX             PIC 9 VALUE 0.
       
       01  CHECK-VARS.
           05  CHECK-ROW             PIC 9 VALUE 0.
           05  CHECK-COL             PIC 9 VALUE 0.
           05  COUNT-IN-ROW          PIC 9 VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-GAME
           PERFORM GAME-LOOP UNTIL GAME-OVER = "Y"
           PERFORM DISPLAY-FINAL-RESULT
           STOP RUN.
       
       INITIALIZE-GAME.
           MOVE "N" TO GAME-OVER
           MOVE "1" TO CURRENT-PLAYER
           MOVE 0 TO MOVE-COUNT
           PERFORM VARYING ROW-IDX FROM 1 BY 1 UNTIL ROW-IDX > 6
               PERFORM VARYING COL-IDX FROM 1 BY 1 UNTIL COL-IDX > 7
                   MOVE SPACE TO BOARD-CELL(ROW-IDX, COL-IDX)
               END-PERFORM
           END-PERFORM.
       
       GAME-LOOP.
           PERFORM DISPLAY-BOARD
           PERFORM GET-PLAYER-MOVE
           PERFORM PLACE-PIECE
           PERFORM CHECK-WIN
           IF GAME-OVER = "N"
               PERFORM SWITCH-PLAYER
           END-IF.
       
       DISPLAY-BOARD.
           DISPLAY " "
           DISPLAY "   1   2   3   4   5   6   7"
           DISPLAY "+---+---+---+---+---+---+---+"
           PERFORM VARYING ROW-IDX FROM 1 BY 1 UNTIL ROW-IDX > 6
               DISPLAY "| " WITH NO ADVANCING
               PERFORM VARYING COL-IDX FROM 1 BY 1 UNTIL COL-IDX > 7
                   IF BOARD-CELL(ROW-IDX, COL-IDX) = SPACE
                       DISPLAY "  | " WITH NO ADVANCING
                   ELSE
                       DISPLAY BOARD-CELL(ROW-IDX, COL-IDX) 
                           " | " WITH NO ADVANCING
                   END-IF
               END-PERFORM
               DISPLAY " "
               DISPLAY "+---+---+---+---+---+---+---+"
           END-PERFORM
           DISPLAY " ".
       
       GET-PLAYER-MOVE.
           DISPLAY "Player " CURRENT-PLAYER 
               ", choose column (1-7): " WITH NO ADVANCING
           ACCEPT INPUT-STRING
           MOVE FUNCTION NUMVAL(INPUT-STRING) TO COLUMN-CHOICE
           
           IF COLUMN-CHOICE < 1 OR COLUMN-CHOICE > 7
               DISPLAY "Invalid column! Choose 1-7."
               PERFORM GET-PLAYER-MOVE
           ELSE
               IF BOARD-CELL(1, COLUMN-CHOICE) NOT = SPACE
                   DISPLAY "Column full! Choose another."
                   PERFORM GET-PLAYER-MOVE
               END-IF
           END-IF.
       
       PLACE-PIECE.
           MOVE 6 TO ROW-POSITION
           PERFORM UNTIL ROW-POSITION < 1 OR 
                        BOARD-CELL(ROW-POSITION, COLUMN-CHOICE) = SPACE
               SUBTRACT 1 FROM ROW-POSITION
           END-PERFORM
           
           IF ROW-POSITION < 1
               MOVE 1 TO ROW-POSITION
           END-IF
           
           MOVE CURRENT-PLAYER TO BOARD-CELL(ROW-POSITION, COLUMN-CHOICE)
           ADD 1 TO MOVE-COUNT.
       
       CHECK-WIN.
           PERFORM CHECK-HORIZONTAL
           IF GAME-OVER = "N"
               PERFORM CHECK-VERTICAL
           END-IF
           IF GAME-OVER = "N"
               PERFORM CHECK-DIAGONAL-DOWN
           END-IF
           IF GAME-OVER = "N"
               PERFORM CHECK-DIAGONAL-UP
           END-IF
           IF GAME-OVER = "N" AND MOVE-COUNT = 42
               MOVE "Y" TO GAME-OVER
               MOVE "D" TO WINNER
           END-IF.
       
       CHECK-HORIZONTAL.
           PERFORM VARYING ROW-IDX FROM 1 BY 1 UNTIL ROW-IDX > 6
               PERFORM VARYING COL-IDX FROM 1 BY 1 UNTIL COL-IDX > 4
                   IF BOARD-CELL(ROW-IDX, COL-IDX) = CURRENT-PLAYER AND
                      BOARD-CELL(ROW-IDX, COL-IDX + 1) = CURRENT-PLAYER AND
                      BOARD-CELL(ROW-IDX, COL-IDX + 2) = CURRENT-PLAYER AND
                      BOARD-CELL(ROW-IDX, COL-IDX + 3) = CURRENT-PLAYER
                       MOVE "Y" TO GAME-OVER
                       MOVE CURRENT-PLAYER TO WINNER
                       EXIT PARAGRAPH
                   END-IF
               END-PERFORM
           END-PERFORM.
       
       CHECK-VERTICAL.
           PERFORM VARYING COL-IDX FROM 1 BY 1 UNTIL COL-IDX > 7
               PERFORM VARYING ROW-IDX FROM 1 BY 1 UNTIL ROW-IDX > 3
                   IF BOARD-CELL(ROW-IDX, COL-IDX) = CURRENT-PLAYER AND
                      BOARD-CELL(ROW-IDX + 1, COL-IDX) = CURRENT-PLAYER AND
                      BOARD-CELL(ROW-IDX + 2, COL-IDX) = CURRENT-PLAYER AND
                      BOARD-CELL(ROW-IDX + 3, COL-IDX) = CURRENT-PLAYER
                       MOVE "Y" TO GAME-OVER
                       MOVE CURRENT-PLAYER TO WINNER
                       EXIT PARAGRAPH
                   END-IF
               END-PERFORM
           END-PERFORM.
       
       CHECK-DIAGONAL-DOWN.
           PERFORM VARYING ROW-IDX FROM 1 BY 1 UNTIL ROW-IDX > 3
               PERFORM VARYING COL-IDX FROM 1 BY 1 UNTIL COL-IDX > 4
                   IF BOARD-CELL(ROW-IDX, COL-IDX) = CURRENT-PLAYER AND
                      BOARD-CELL(ROW-IDX + 1, COL-IDX + 1) = 
                          CURRENT-PLAYER AND
                      BOARD-CELL(ROW-IDX + 2, COL-IDX + 2) = 
                          CURRENT-PLAYER AND
                      BOARD-CELL(ROW-IDX + 3, COL-IDX + 3) = 
                          CURRENT-PLAYER
                       MOVE "Y" TO GAME-OVER
                       MOVE CURRENT-PLAYER TO WINNER
                       EXIT PARAGRAPH
                   END-IF
               END-PERFORM
           END-PERFORM.
       
       CHECK-DIAGONAL-UP.
           PERFORM VARYING ROW-IDX FROM 4 BY 1 UNTIL ROW-IDX > 6
               PERFORM VARYING COL-IDX FROM 1 BY 1 UNTIL COL-IDX > 4
                   IF BOARD-CELL(ROW-IDX, COL-IDX) = CURRENT-PLAYER AND
                      BOARD-CELL(ROW-IDX - 1, COL-IDX + 1) = 
                          CURRENT-PLAYER AND
                      BOARD-CELL(ROW-IDX - 2, COL-IDX + 2) = 
                          CURRENT-PLAYER AND
                      BOARD-CELL(ROW-IDX - 3, COL-IDX + 3) = 
                          CURRENT-PLAYER
                       MOVE "Y" TO GAME-OVER
                       MOVE CURRENT-PLAYER TO WINNER
                       EXIT PARAGRAPH
                   END-IF
               END-PERFORM
           END-PERFORM.
       
       SWITCH-PLAYER.
           IF CURRENT-PLAYER = "1"
               MOVE "2" TO CURRENT-PLAYER
           ELSE
               MOVE "1" TO CURRENT-PLAYER
           END-IF.
       
       DISPLAY-FINAL-RESULT.
           PERFORM DISPLAY-BOARD
           IF WINNER = "D"
               DISPLAY "Game Over - It's a draw!"
           ELSE
               DISPLAY "Player " WINNER " wins! Congratulations!"
           END-IF.
		   
		   