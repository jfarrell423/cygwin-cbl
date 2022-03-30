000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. utphwin.
000030*--------------------------------
000040* Creates the database for use by the .
000050* Phone Number Data Entry Screen.
000070*--------------------------------
000080 ENVIRONMENT DIVISION.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.
000110 SELECT PHONEFILE ASSIGN TO "documents\PHONENUMBERDB.DAT"
000120 FILE STATUS IS FILE-CHECK-KEY
000130 ORGANIZATION IS INDEXED
000140 ACCESS MODE IS DYNAMIC
000150 RECORD KEY IS FULLNAME.
000160
000170 DATA DIVISION.
000180 FILE SECTION.
000190 FD PHONEFILE.
000200 01  DBS-REC-1.
000210     05  fullname.
000220        10    first-name              pic x(10).
000230        10    last-name               pic x(10).
000240     05  home-number                  pic x(10).
000250     05  work-number                  pic x(10).
000260     05  cell-number                  pic x(10).
000270     05  pager-number                 pic x(10).
000280
000290
000300
000310 WORKING-STORAGE SECTION.
000320 01  WS-WORK-AREAS.
000330      05  FILE-CHECK-KEY              PIC X(2).
000340      88  RECORDFOUND                 VALUE "00".
000350      88  ENDOFFILE                   VALUE HIGH-VALUES.
000360
000370 PROCEDURE DIVISION.
000380 PROGRAM-BEGIN.
000390     PERFORM MAIN-PROCESS.
000400
000410 MAIN-PROCESS.
000420*****************************************
000430* This is where we will open the table!
000440* and the database is created.
000450*****************************************
000460     OPEN  OUTPUT PHONEFILE.
000470     CLOSE PHONEFILE.
000480     STOP RUN.
000490     EXIT PROGRAM.
000500
000510
