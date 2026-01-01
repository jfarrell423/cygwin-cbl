       IDENTIFICATION DIVISION.
       PROGRAM-ID. MONTHLY-INCOME-ALL-RATES.
      *> $cobc -x -free retire-amounts-01.cbl
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-AMOUNT                 PIC 9(7)V99 VALUE 0.
       01  WS-AMOUNT-DISPLAY         PIC Z,ZZZ,ZZZ,ZZ9.99.

       01  WS-RATE-3                 PIC 9V9999 VALUE 0.0300.
       01  WS-RATE-4                 PIC 9V9999 VALUE 0.0400.
       01  WS-RATE-8                 PIC 9V9999 VALUE 0.0800.

       01  WS-ANNUAL-3               PIC 9(7)V99 VALUE 0.
       01  WS-ANNUAL-4               PIC 9(7)V99 VALUE 0.
       01  WS-ANNUAL-8               PIC 9(7)V99 VALUE 0.

       01  WS-MONTHLY-3              PIC 9(7)V99 VALUE 0.
       01  WS-MONTHLY-4              PIC 9(7)V99 VALUE 0.
       01  WS-MONTHLY-8              PIC 9(7)V99 VALUE 0.

       01  WS-ANNUAL-DISP-3          PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-ANNUAL-DISP-4          PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-ANNUAL-DISP-8          PIC Z,ZZZ,ZZZ,ZZ9.99.

       01  WS-MONTHLY-DISP-3         PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-MONTHLY-DISP-4         PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-MONTHLY-DISP-8         PIC Z,ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       MAIN-PARA.

           DISPLAY "Enter amount (0.01 to 9,999,999.99): "
           ACCEPT WS-AMOUNT

           IF WS-AMOUNT < 0.01 OR WS-AMOUNT > 9999999.99
               DISPLAY "Invalid amount entered."
               STOP RUN
           END-IF

      *> ---- 3% ---- Very conservative Suze Orman
           COMPUTE WS-ANNUAL-3  ROUNDED = WS-AMOUNT * WS-RATE-3
           COMPUTE WS-MONTHLY-3 ROUNDED = WS-ANNUAL-3 / 12

      *> ---- 4% ---- Originally suggested by William Bengen
      *> ---- Then Adjust for Inflation
           COMPUTE WS-ANNUAL-4  ROUNDED = WS-AMOUNT * WS-RATE-4
           COMPUTE WS-MONTHLY-4 ROUNDED = WS-ANNUAL-4 / 12

      *> ---- 8% ---- Less Conservative by Dave Ramsey
           COMPUTE WS-ANNUAL-8  ROUNDED = WS-AMOUNT * WS-RATE-8
           COMPUTE WS-MONTHLY-8 ROUNDED = WS-ANNUAL-8 / 12

           MOVE WS-AMOUNT      TO WS-AMOUNT-DISPLAY
           MOVE WS-ANNUAL-3    TO WS-ANNUAL-DISP-3
           MOVE WS-ANNUAL-4    TO WS-ANNUAL-DISP-4
           MOVE WS-ANNUAL-8    TO WS-ANNUAL-DISP-8
           MOVE WS-MONTHLY-3   TO WS-MONTHLY-DISP-3
           MOVE WS-MONTHLY-4   TO WS-MONTHLY-DISP-4
           MOVE WS-MONTHLY-8   TO WS-MONTHLY-DISP-8

           DISPLAY "========================================"
           DISPLAY "Principal Amount : $" WS-AMOUNT-DISPLAY
           DISPLAY "----------------------------------------"
           DISPLAY "Rate   Annual Income     Monthly Income"
           DISPLAY "----------------------------------------"
           DISPLAY "3%   $" WS-ANNUAL-DISP-3 "   $" WS-MONTHLY-DISP-3
           DISPLAY "4%   $" WS-ANNUAL-DISP-4 "   $" WS-MONTHLY-DISP-4
           DISPLAY "8%   $" WS-ANNUAL-DISP-8 "   $" WS-MONTHLY-DISP-8
           DISPLAY "========================================"

           STOP RUN.


