000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. utphwin.
000030*--------------------------------
000040* Menu for the phone lookup system.
000050*
000060* Using full screen IO.
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
000370 01 ws-hold-rec.
000380      05 ws-first-name                pic x(10).
000390      05 ws-last-name                 pic x(10).
000400      05 ws-home-number               pic x(10).
000410      05 ws-work-number               pic x(10).
000420      05 ws-cell-number               pic x(10).
000430      05 ws-pager-number              pic x(10).
000440
000450 01 ws-phone-screen.
000460   03 ws-phone-rec occurs 20 times.
000470      05 ws-first-name-id             pic x(10).
000480      05 ws-last-name-id              pic x(10).
000490      05 ws-home-number-id            pic x(10).
000500      05 ws-work-number-id            pic x(10).
000510      05 ws-cell-number-id            pic x(10).
000520      05 ws-pager-number-id           pic x(10).
000530
000540 01 key-status.
000550      05  key-type                    pic x.
000560      05  key-code-1                  pic 9(2) comp-x.
000570      05  filler                      pic x.
000580
000590 01 any-data                          pic x.
000600 01 key-code-1-display                pic z9.
000610 01 eof                               pic x value space.
000620 01 dup-stat-mess                     pic x(25)
000630        value "Record already on file.".
000640 01 chg-stat-mess                     pic x(25)
000650        value "Do you want to change?".
000660 01 not-stat-mess                     pic x(25)
000670        value "Record not on file.".
000680 01 ent-stat-mess                     pic x(25)
000690        value "<<Enter Login ID>>".
000700 01 rem-stat-mess                     pic x(25)
000710        value "Do you want to delete?".
000720
000730 01 clr-stat-mess                     pic x(25)
000740        value spaces.
000750 01 screen-mess                       pic x(25) value spaces.
000760 01 bye                               pic x value space.
000770 01 rec-ok                            pic x value space.
000780 01 yorn                              pic x value space.
000790 01 x                                 pic 99 value zeroes.
000800 01 xcnt                              pic 99.
000810
000820 01  unseen-dat                       pic x(4).
000830
000840 01 ws-user-ok                        pic x.
000850 01 done                              pic x value space.
000860
000870
000880 77  MENU-PICK                        PIC 9 VALUE 0.
000890     88  MENU-PICK-IS-VALID           VALUES 0 THRU 6.
000900
000910 77  ERROR-MESSAGE                    PIC X(79).
000920
000930 SCREEN SECTION.
000940 01  MENU-SCREEN.
000950     05  BLANK SCREEN.
000960     05  foreground-color 6
000970         LINE  6 COL 20 VALUE "PLEASE SELECT:".
000980     05  foreground-color 6
000990         LINE  8 COL 25 VALUE "1. Enter a new number".
001000     05  foreground-color 6
001010         LINE  9 COL 25 VALUE "2. Change a number".
001020     05  foreground-color 6
001030         LINE 10 COL 25 VALUE "3. Remove a number".
001040     05  foreground-color 6
001050         LINE 11 COL 25 VALUE "4. Lookup a number".
001060     05  foreground-color 6
001070         LINE 12 COL 25 VALUE "5. List Current Numbers".
001080     05  foreground-color 6
001090         LINE 13 COL 25 VALUE "6. Exit this program".
001100     05  foreground-color 6
001110         LINE 20 COL  1 VALUE "YOUR SELECTION".
001120     05  foreground-color 6
001130         LINE 20 COL 16 PIC Z USING MENU-PICK.
001140     05  foreground-color 6
001150         LINE 23 COL  1 PIC X(70) FROM FILE-CHECK-KEY.
001160     05  foreground-color 6
001170         LINE 24 COL  1 PIC X(79) FROM ERROR-MESSAGE.
001180
001190 01  DATA-ENTER-SCREEN.
001200     03  blank screen.
001210     03  foreground-color 6
001220         line 2 column 11 value "Phone Number Access".
001230     03  foreground-color 6
001240         line 6 column 11 value "FIRST NAME".
001250     03  foreground-color 6
001260         line 6 column 30 pic x(10)
001270          using ws-first-name
001280          prompt character is "*"
001290          justified right.
001300     03  foreground-color 6
001310         line 8 column 11 value "LAST NAME".
001320     03  foreground-color 6
001330         line 8 column 30 pic x(10)
001340           using ws-last-name
001350           prompt character is "*"
001360           justified right.
001370     03  foreground-color 6
001380         line 10 column 11 value "HOME NUMBER".
001390     03  foreground-color 6
001400         line 10 column 30 pic x(10)
001410           using ws-home-number
001420           prompt character is "*"
001430           justified right.
001440     03  foreground-color 6
001450         line 12 column 11 value "WORK NUMBER".
001460     03  foreground-color 6
001470         line 12 column 30 pic x(10)
001480           using ws-work-number
001490           prompt character is "*"
001500           justified right.
001510     03  foreground-color 6
001520         line 14 column 11 value "CELL NUMBER".
001530     03  foreground-color 6
001540         line 14 column 30 pic x(10)
001550           using ws-cell-number
001560           prompt character is "*"
001570           justified right.
001580     03  foreground-color 6
001590         line 16 column 11 value "PAGER NUMBER".
001600     03  foreground-color 6
001610         line 16 column 30 pic x(10)
001620           using ws-pager-number
001630           prompt character is "*"
001640           justified right.
001650     03  foreground-color 6
001660         LINE 22 COL  1 pic x(79) USING screen-mess.
001670     03  foreground-color 6
001680         LINE 23 COL  1 PIC X(79) USING FILE-CHECK-KEY.
001690     03  foreground-color 6
001700         LINE 24 COL  1 PIC X(79) USING ERROR-MESSAGE.
001710
001720  01  CLEAR-SCREEN.
001730     03  blank screen.
001740
001750 01  lst-screen-one.
001760 03  blank screen.
001770 03  foreground-color 6
001780     line 1 column 1 value "   First   ".
001790 03  foreground-color 6
001800     line 1 column 14 value "  Last    ".
001810 03  foreground-color 6
001820     line 1 column 24 value "  Home    ".
001830 03  foreground-color 6
001840     line 1 column 34 value "  Work    ".
001850 03  foreground-color 6
001860     line 1 column 45 value "  Cell    ".
001870 03  foreground-color 6
001880     line 1 column 56 Value "  Pager   ".
001890 03  foreground-color 6
001900     line 2 column 1     pic x(10) using ws-first-name-id(01).
001910 03  foreground-color 6
001920     line 2 column 12    pic x(10) using ws-last-name-id(01).
001930 03  foreground-color 6
001940     line 2 column 23    pic x(10) using ws-home-number-id(01).
001950 03  foreground-color 6
001960     line 2 column 34    pic x(10) using ws-work-number-id(01).
001970 03  foreground-color 6
001980     line 2 column 45    pic x(10) using ws-cell-number-id(01).
001990 03  foreground-color 6
002000     line 2 column 56    pic x(10) using ws-pager-number-id(01).
002010 03  foreground-color 6
002020     line 3 column 1     pic x(10) using ws-first-name-id(02).
002030 03  foreground-color 6
002040     line 3 column 12    pic x(10) using ws-last-name-id(02).
002050 03  foreground-color 6
002060     line 3 column 23    pic x(10) using ws-home-number-id(02).
002070 03  foreground-color 6
002080     line 3 column 34    pic x(10) using ws-work-number-id(02).
002090 03  foreground-color 6
002100     line 3 column 45    pic x(10) using ws-cell-number-id(02).
002110 03  foreground-color 6
002120     line 3 column 56    pic x(10) using ws-pager-number-id(02).
002130 03  foreground-color 6
002140     line 4 column 1     pic x(10) using ws-first-name-id(03).
002150 03  foreground-color 6
002160     line 4 column 12    pic x(10) using ws-last-name-id(03).
002170 03  foreground-color 6
002180     line 4 column 23    pic x(10) using ws-home-number-id(03).
002190 03  foreground-color 6
002200     line 4 column 34    pic x(10) using ws-work-number-id(03).
002210 03  foreground-color 6
002220     line 4 column 45    pic x(10) using ws-cell-number-id(03).
002230 03  foreground-color 6
002240     line 4 column 56    pic x(10) using ws-pager-number-id(03).
002250 03  foreground-color 6
002260     line 5 column 1     pic x(10) using ws-first-name-id(04).
002270 03  foreground-color 6
002280     line 5 column 12    pic x(10) using ws-last-name-id(04).
002290 03  foreground-color 6
002300     line 5 column 23    pic x(10) using ws-home-number-id(04).
002310 03  foreground-color 6
002320     line 5 column 34    pic x(10) using ws-work-number-id(04).
002330 03  foreground-color 6
002340     line 5 column 45    pic x(10) using ws-cell-number-id(04).
002350 03  foreground-color 6
002360     line 5 column 56    pic x(10) using ws-pager-number-id(04).
002370 03  foreground-color 6
002380     line 6 column 1     pic x(10) using ws-first-name-id(05).
002390 03  foreground-color 6
002400     line 6 column 12    pic x(10) using ws-last-name-id(05).
002410 03  foreground-color 6
002420     line 6 column 23    pic x(10) using ws-home-number-id(05).
002430 03  foreground-color 6
002440     line 6 column 34    pic x(10) using ws-work-number-id(05).
002450 03  foreground-color 6
002460     line 6 column 45    pic x(10) using ws-cell-number-id(05).
002470 03  foreground-color 6
002480     line 6 column 56    pic x(10) using ws-pager-number-id(05).
002490 03  foreground-color 6
002500     line 7 column 1     pic x(10) using ws-first-name-id(06).
002510 03  foreground-color 6
002520     line 7 column 12    pic x(10) using ws-last-name-id(06).
002530 03  foreground-color 6
002540     line 7 column 23    pic x(10) using ws-home-number-id(06).
002550 03  foreground-color 6
002560     line 7 column 34    pic x(10) using ws-work-number-id(06).
002570 03  foreground-color 6
002580     line 7 column 45    pic x(10) using ws-cell-number-id(06).
002590 03  foreground-color 6
002600     line 7 column 56    pic x(10) using ws-pager-number-id(06).
002610 03  foreground-color 6
002620     line 8 column 1     pic x(10) using ws-first-name-id(07).
002630 03  foreground-color 6
002640     line 8 column 12    pic x(10) using ws-last-name-id(07).
002650 03  foreground-color 6
002660     line 8 column 23    pic x(10) using ws-home-number-id(07).
002670 03  foreground-color 6
002680     line 8 column 34    pic x(10) using ws-work-number-id(07).
002690 03  foreground-color 6
002700     line 8 column 45    pic x(10) using ws-cell-number-id(07).
002710 03  foreground-color 6
002720     line 8 column 56    pic x(10) using ws-pager-number-id(07).
002730 03  foreground-color 6
002740     line 9 column 1     pic x(10) using ws-first-name-id(08).
002750 03  foreground-color 6
002760     line 9 column 12    pic x(10) using ws-last-name-id(08).
002770 03  foreground-color 6
002780     line 9 column 23    pic x(10) using ws-home-number-id(08).
002790 03  foreground-color 6
002800     line 9 column 34    pic x(10) using ws-work-number-id(08).
002810 03  foreground-color 6
002820     line 9 column 45    pic x(10) using ws-cell-number-id(08).
002830 03  foreground-color 6
002840     line 9 column 56    pic x(10) using ws-pager-number-id(08).
002850 03  foreground-color 6
002860     line 10 column 1    pic x(10) using ws-first-name-id(09).
002870 03  foreground-color 6
002880     line 10 column 12   pic x(10) using ws-last-name-id(09).
002890 03  foreground-color 6
002900     line 10 column 23   pic x(10) using ws-home-number-id(09).
002910 03  foreground-color 6
002920     line 10 column 34   pic x(10) using ws-work-number-id(09).
002930 03  foreground-color 6
002940     line 10 column 45   pic x(10) using ws-cell-number-id(09).
002950 03  foreground-color 6
002960     line 10 column 56   pic x(10) using ws-pager-number-id(09).
002970 03  foreground-color 6
002980     line 11 column 1    pic x(10) using ws-first-name-id(10).
002990 03  foreground-color 6
003000     line 11 column 12   pic x(10) using ws-last-name-id(10).
003010 03  foreground-color 6
003020     line 11 column 23   pic x(10) using ws-home-number-id(10).
003030 03  foreground-color 6
003040     line 11 column 34   pic x(10) using ws-work-number-id(10).
003050 03  foreground-color 6
003060     line 11 column 45   pic x(10) using ws-cell-number-id(10).
003070 03  foreground-color 6
003080     line 11 column 56   pic x(10) using ws-pager-number-id(10).
003090 03  foreground-color 6
003100     line 12 column 1    pic x(10) using ws-first-name-id(11).
003110 03  foreground-color 6
003120     line 12 column 12   pic x(10) using ws-last-name-id(11).
003130 03  foreground-color 6
003140     line 12 column 23   pic x(10) using ws-home-number-id(11).
003150 03  foreground-color 6
003160     line 12 column 34   pic x(10) using ws-work-number-id(11).
003170 03  foreground-color 6
003180     line 12 column 45   pic x(10) using ws-cell-number-id(11).
003190 03  foreground-color 6
003200     line 12 column 56   pic x(10) using ws-pager-number-id(11).
003210 03  foreground-color 6
003220     line 13 column 1    pic x(10) using ws-first-name-id(12).
003230 03  foreground-color 6
003240     line 13 column 12   pic x(10) using ws-last-name-id(12).
003250 03  foreground-color 6
003260     line 13 column 23   pic x(10) using ws-home-number-id(12).
003270 03  foreground-color 6
003280     line 13 column 34   pic x(10) using ws-work-number-id(12).
003290 03  foreground-color 6
003300     line 13 column 45   pic x(10) using ws-cell-number-id(12).
003310 03  foreground-color 6
003320     line 13 column 56   pic x(10) using ws-pager-number-id(12).
003330 03  foreground-color 6
003340     line 14 column 1    pic x(10) using ws-first-name-id(13).
003350 03  foreground-color 6
003360     line 14 column 12   pic x(10) using ws-last-name-id(13).
003370 03  foreground-color 6
003380     line 14 column 23   pic x(10) using ws-home-number-id(13).
003390 03  foreground-color 6
003400     line 14 column 34   pic x(10) using ws-work-number-id(13).
003410 03  foreground-color 6
003420     line 14 column 45   pic x(10) using ws-cell-number-id(13).
003430 03  foreground-color 6
003440     line 14 column 56   pic x(10) using ws-pager-number-id(13).
003450 03  foreground-color 6
003460     line 15 column 1    pic x(10) using ws-first-name-id(14).
003470 03  foreground-color 6
003480     line 15 column 12   pic x(10) using ws-last-name-id(14).
003490 03  foreground-color 6
003500     line 15 column 23   pic x(10) using ws-home-number-id(14).
003510 03  foreground-color 6
003520     line 15 column 34   pic x(10) using ws-work-number-id(14).
003530 03  foreground-color 6
003540     line 15 column 45   pic x(10) using ws-cell-number-id(14).
003550 03  foreground-color 6
003560     line 15 column 56   pic x(10) using ws-pager-number-id(14).
003570 03  foreground-color 6
003580     line 16 column 1    pic x(10) using ws-first-name-id(15).
003590 03  foreground-color 6
003600     line 16 column 12   pic x(10) using ws-last-name-id(15).
003610 03  foreground-color 6
003620     line 16 column 23   pic x(10) using ws-home-number-id(15).
003630 03  foreground-color 6
003640     line 16 column 34   pic x(10) using ws-work-number-id(15).
003650 03  foreground-color 6
003660     line 16 column 45   pic x(10) using ws-cell-number-id(15).
003670 03  foreground-color 6
003680     line 16 column 56   pic x(10) using ws-pager-number-id(15).
003690 03  foreground-color 6
003700     line 17 column 1    pic x(10) using ws-first-name-id(16).
003710 03  foreground-color 6
003720     line 17 column 12   pic x(10) using ws-last-name-id(16).
003730 03  foreground-color 6
003740     line 17 column 23   pic x(10) using ws-home-number-id(16).
003750 03  foreground-color 6
003760     line 17 column 34   pic x(10) using ws-work-number-id(16).
003770 03  foreground-color 6
003780     line 17 column 45   pic x(10) using ws-cell-number-id(16).
003790 03  foreground-color 6
003800     line 17 column 56   pic x(10) using ws-pager-number-id(16).
003810 03  foreground-color 6
003820     line 18 column 1    pic x(10) using ws-first-name-id(17).
003830 03  foreground-color 6
003840     line 18 column 12   pic x(10) using ws-last-name-id(17).
003850 03  foreground-color 6
003860     line 18 column 23   pic x(10) using ws-home-number-id(17).
003870 03  foreground-color 6
003880     line 18 column 34   pic x(10) using ws-work-number-id(17).
003890 03  foreground-color 6
003900     line 18 column 45   pic x(10) using ws-cell-number-id(17).
003910 03  foreground-color 6
003920     line 18 column 56   pic x(10) using ws-pager-number-id(17).
003930 03  foreground-color 6
003940     line 19 column 1    pic x(10) using ws-first-name-id(18).
003950 03  foreground-color 6
003960     line 19 column 12   pic x(10) using ws-last-name-id(18).
003970 03  foreground-color 6
003980     line 19 column 23   pic x(10) using ws-home-number-id(18).
003990 03  foreground-color 6
004000     line 19 column 34   pic x(10) using ws-work-number-id(18).
004010 03  foreground-color 6
004020     line 19 column 45   pic x(10) using ws-cell-number-id(18).
004030 03  foreground-color 6
004040     line 19 column 56   pic x(10) using ws-pager-number-id(18).
004050 03  foreground-color 6
004060     line 20 column 1    pic x(10) using ws-first-name-id(19).
004070 03  foreground-color 6
004080     line 20 column 12   pic x(10) using ws-last-name-id(19).
004090 03  foreground-color 6
004100     line 20 column 23   pic x(10) using ws-home-number-id(19).
004110 03  foreground-color 6
004120     line 20 column 34   pic x(10) using ws-work-number-id(19).
004130 03  foreground-color 6
004140     line 20 column 45   pic x(10) using ws-cell-number-id(19).
004150 03  foreground-color 6
004160     line 20 column 56   pic x(10) using ws-pager-number-id(19).
004170 03  foreground-color 6
004180     line 21 column 2    value "Enter to continue...".
004190 03  foreground-color 6
004200     line 21 column 38   pic x using any-data
004210                                    prompt character is "_"
004220                                    justified right.
004230
004240 PROCEDURE DIVISION.
004250 PROGRAM-BEGIN.
004260     PERFORM MAIN-PROCESS.
004270
004280
004290 MAIN-PROCESS.
004300     PERFORM GET-MENU-PICK THRU DO-THE-PICK
004310         UNTIL MENU-PICK = 6.
004320*--------------------------------
004330* MENU
004340*--------------------------------
004350 GET-MENU-PICK.
004360     MOVE SPACE TO ERROR-MESSAGE.
004370     CLOSE PHONEFILE.
004380     DISPLAY MENU-SCREEN.
004390     ACCEPT MENU-SCREEN.
004400
004410 DO-THE-PICK.
004420     IF MENU-PICK = 1
004430         PERFORM enter-num
004440     ELSE
004450     IF MENU-PICK = 2
004460         PERFORM change-num
004470     ELSE
004480     IF MENU-PICK = 3
004490         PERFORM rem-rec
004500     ELSE
004510     IF MENU-PICK = 4
004520         PERFORM look-up
004530     ELSE
004540     IF MENU-PICK = 5
004550         MOVE SPACES TO eof
004560         MOVE SPACES TO done
004570         MOVE SPACES TO any-data
004580         PERFORM LISTRECS until eof = "y"
004590     ELSE
004600     IF MENU-PICK = 6
004610         PERFORM PROGRAM-DONE.
004620
004630 PROGRAM-EXIT.
004640     EXIT PROGRAM.
004650
004660 PROGRAM-DONE.
004670     STOP RUN.
004680
004690 enter-num.
004700     perform init-screen.
004710     move 1 to x.
004720     display data-enter-screen.
004730     accept  data-enter-screen.
004740     move ws-first-name   to first-name.
004750     move ws-last-name    to last-name.
004760     move ws-home-number  to home-number.
004770     move ws-work-number  to work-number.
004780     move ws-pager-number to pager-number.
004790     open I-O PHONEFILE.
004800     write dbs-rec-1 from ws-hold-rec
004810           invalid key perform dup-key
004820     end-write.
004830     close PHONEFILE.
004840     perform init-screen
004850     move FILE-CHECK-KEY to screen-mess.
004860     move "y" to rec-ok.
004870
004880 dup-key.
004890     move dup-stat-mess to screen-mess.
004900     display data-enter-screen.
004910     accept ws-user-ok.
004920
004930 init-screen.
004940     move clr-stat-mess to screen-mess.
004950     move spaces to ws-first-name.
004960     move spaces to ws-last-name.
004970     move spaces to ws-home-number.
004980     move spaces to ws-cell-number.
004990     move spaces to ws-work-number.
005000     move spaces to ws-pager-number.
005010     move zero   to MENU-PICK.
005020
005030 init-lst.
005040     display "initializing the list".
005050     move spaces to ws-phone-rec(x).
005060     add 1 to x.
005070
005080 change-num.
005090     move ent-stat-mess to screen-mess.
005100     perform init-screen.
005110     display data-enter-screen.
005120     accept data-enter-screen.
005130     move ws-first-name to first-name.
005140     move ws-last-name  to last-name.
005150     open I-O PHONEFILE.
005160     read PHONEFILE into ws-hold-rec
005170         invalid key perform not-there
005180     end-read.
005190     move chg-stat-mess to screen-mess.
005200     display data-enter-screen.
005210     accept ws-user-ok.
005220     if ws-user-ok = "N" or "n" perform init-screen.
005230     if ws-user-ok = "Y" or "y" perform change-ok.
005240     close PHONEFILE.
005250     perform init-screen.
005260
005270 Change-ok.
005280     move clr-stat-mess to screen-mess.
005290     display data-enter-screen.
005300     accept data-enter-screen.
005310     move ws-first-name   to first-name.
005320     move ws-last-name    to last-name.
005330     move ws-home-number  to home-number.
005340     move ws-work-number  to work-number.
005350     move ws-pager-number to pager-number.
005360     rewrite dbs-rec-1    from ws-hold-rec
005370     invalid key display "Problem with index!"
005380     accept ws-user-ok
005390     end-rewrite.
005400     perform init-screen.
005410
005420 not-there.
005430     move not-stat-mess to screen-mess.
005440     display data-enter-screen.
005450     accept ws-user-ok.
005460     perform init-screen.
005470
005480 look-up.
005490     move ent-stat-mess to screen-mess.
005500     open I-O PHONEFILE.
005510     perform init-screen.
005520     display data-enter-screen.
005530     accept  data-enter-screen.
005540     move clr-stat-mess to screen-mess.
005550     move ws-first-name to first-name.
005560     move ws-last-name to last-name.
005570     read PHONEFILE into ws-hold-rec
005580         invalid key perform not-there
005590     end-read.
005600     move dbs-rec-1 to ws-hold-rec.
005610     close PHONEFILE.
005620     display data-enter-screen.
005630     accept ws-user-ok.
005640     move clr-stat-mess to screen-mess.
005650     perform init-screen.
005660
005670 rem-rec.
005680     move ent-stat-mess to screen-mess.
005690     display data-enter-screen.
005700     accept  data-enter-screen.
005710     move ws-first-name to first-name.
005720     move ws-last-name  to last-name.
005730     read PHONEFILE into ws-hold-rec
005740         invalid key perform not-there
005750     end-read.
005760     move rem-stat-mess to screen-mess.
005770     display data-enter-screen.
005780     accept ws-user-ok  at 2238.
005790     display ws-user-ok at 2238.
005800     if ws-user-ok = "y" or "Y" perform goodbye-rec.
005810     perform init-screen.
005820
005830 goodbye-rec.
005840     open I-O PHONEFILE.
005850     delete  PHONEFILE.
005860     close   PHONEFILE.
005870
005880 OPEN-PHONEDATA.
005890*****************************************
005900* This is where we will open the table!
005910*****************************************
005920     open I-O PHONEFILE.
005930
005940 CLOSE-PHONEDATA.
005950*****************************************
005960* This is where we will close the table.
005970*****************************************
005980     close PHONEFILE.
005990
006000 READ-NEXT-RECORD.
006010*****************************************
006020* This is where we will read the table.
006030*****************************************
006040     read PHONEFILE next record into ws-hold-rec
006050          at end move "y" to eof.
006060
006070 LISTRECS.
006080     PERFORM A-100-INITIALIZE.
006090     PERFORM B-100-PROCESS.
006100     PERFORM C-100-TERMINATE.
006110
006120 A-100-INITIALIZE.
006130     MOVE SPACES TO DBS-REC-1.
006140     MOVE SPACES TO eof.
006150     MOVE SPACES TO done.
006160     MOVE SPACES TO any-data.
006170     MOVE ZEROES TO MENU-PICK.
006180     initialize ws-phone-screen.
006190     OPEN INPUT PHONEFILE.
006200
006210 B-100-PROCESS.
006220      READ PHONEFILE
006230          AT END
006240             MOVE "y" TO eof
006250             close PHONEFILE.
006260     PERFORM B-200-LOOP
006270         UNTIL eof = "y" OR any-data = "q".
006280
006290 B-200-LOOP.
006300*    MOVE ZEROES TO x.
006310     PERFORM B-300-SETUP
006320             varying x from 1 by 1
006330             until x > 19 OR eof = "y"
006340             OR any-data ="q".
006350     DISPLAY lst-screen-one.
006360     ACCEPT any-data.
006370     IF x > 19 MOVE ZEROES TO X
006380         initialize ws-phone-screen.
006390     IF eof = "y"
006400        or any-data = "q"
006410        move spaces to any-data
006420        CLOSE PHONEFILE.
006430
006440 B-300-SETUP.
006450     move dbs-rec-1 to ws-phone-rec(x).
006460     IF eof <> "y"
006470     READ PHONEFILE
006480        AT END MOVE "y" to eof.
006490
006500 C-100-TERMINATE.
006510     CLOSE-PHONEFILE.
006520
