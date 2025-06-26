000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. COMMENT.
000300 ENVIRONMENT DIVISION.
       DATA DIVISION.                                        
000500 PROCEDURE DIVISION.
000600* cobc -x -v comment.cbl
000700* This is a comment.                                    
000800* This paragraph displays information about the program.
000900 PROGRAM-BEGIN.
003700     DISPLAY "This program contains four DIVISIONS,".
003800     DISPLAY "three PARAGRAPHS".                    
001000     DISPLAY "and four SENTENCES".
001100 PROGRAM-DONE.
001200     STOP RUN.
