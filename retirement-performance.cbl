       IDENTIFICATION DIVISION.
       PROGRAM-ID. RETIREMENT-PERFORMANCE.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  BEGIN-BALANCE      PIC 9(9)V99.
       01  CURRENT-BALANCE    PIC 9(9)V99.
       01  DOLLAR-GAIN        PIC S9(9)V99.
       01  PERCENT-GAIN      PIC S9(3)V99.

       01  DISPLAY-BEGIN      PIC $$$,$$$,$$$.99.
       01  DISPLAY-CURRENT    PIC $$$,$$$,$$$.99.
       01  DISPLAY-GAIN       PIC $$$,$$$,$$$.99.
       01  DISPLAY-PERCENT    PIC ZZ9.99.

       PROCEDURE DIVISION.

       MAIN-PARA.
           DISPLAY "Enter Beginning Balance: ".
           ACCEPT BEGIN-BALANCE.

           DISPLAY "Enter Current Balance: ".
           ACCEPT CURRENT-BALANCE.

           COMPUTE DOLLAR-GAIN = CURRENT-BALANCE - BEGIN-BALANCE.

           IF BEGIN-BALANCE NOT = 0
               COMPUTE PERCENT-GAIN =
                   (DOLLAR-GAIN / BEGIN-BALANCE) * 100
           ELSE
               MOVE 0 TO PERCENT-GAIN
           END-IF.

           MOVE BEGIN-BALANCE   TO DISPLAY-BEGIN.
           MOVE CURRENT-BALANCE TO DISPLAY-CURRENT.
           MOVE DOLLAR-GAIN     TO DISPLAY-GAIN.
           MOVE PERCENT-GAIN    TO DISPLAY-PERCENT.

           DISPLAY " ".
           DISPLAY "===== Retirement Account Performance =====".
           DISPLAY "Beginning Balance : " DISPLAY-BEGIN.
           DISPLAY "Current Balance   : " DISPLAY-CURRENT.
           DISPLAY "Dollar Gain/Loss  : " DISPLAY-GAIN.
           DISPLAY "Percentage Gain   : " DISPLAY-PERCENT "%".
           DISPLAY "==========================================".

           STOP RUN.
