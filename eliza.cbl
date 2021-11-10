000010 IDENTIFICATION DIVISION.
000020
000030 PROGRAM-ID.             ELIZA.
000040*AUTHOR.                 ARNOLD J. TREMBLEY.
000050*DATE-WRITTEN.           2017-10-01.
000060*SECURITY.               THIS PROGRAM IS PUBLIC DOMAIN FREEWARE.
000070
000080****************************************************************
000090*                                                              *
000100*    https://en.wikipedia.org/wiki/ELIZA                       *
000110*    ELIZA is an early natural language processing program     *
000120*    created around 1964 by Joseph Wiezenbaum at MIT.  This    *
000130*    version is adapted from ELIZA.BAS which appeared in       *
000140*    Creative Computing magazine in 1977, written by Jeff      *
000150*    Shrager and adapted for IBM PC in the early 1980's by     *
000160*    Patricia Danielson and Paul Hashfield.                    *
000170*                                                              *
000180*    COBOL translation by Arnold Trembley, 2017-10-01.         *
000190*    arnold.trembley@att.net                                   *
000200*    Using MinGW GnuCOBOL 2.2 for Windows 7.                   *
000210*    This version is public domain freeware.                   *
000220*                                                              *
000230*    ELIZA simulates a psychotherapist interacting with a      *
000240*    human patient. Enter "shut up" to stop the dialog.        *
000250*                                                              *
000260****************************************************************
000270
000280 ENVIRONMENT DIVISION.
000290
000300 CONFIGURATION SECTION.
000310
000320 REPOSITORY.
000330     FUNCTION ALL INTRINSIC.
000340
000350 INPUT-OUTPUT SECTION.
000360
000370 FILE-CONTROL.
000380
000390 DATA DIVISION.
000400
000410 FILE SECTION.
000420
000430 WORKING-STORAGE SECTION.
000440
000450 01  100-PROGRAM-FLAGS.
000460     05  100-EOF-FLAG                PIC X(01)   VALUE SPACE.
000470         88  88-100-ALL-DONE                     VALUE "Y".
000480     05  100-KEYWORD-FLAG            PIC X(01)   VALUE SPACE.
000490         88  88-100-KEYWORD-FOUND                VALUE "Y".
000500         88  88-100-KEYWORD-NOT-FOUND            VALUE "N".
000510
000520 01  200-USER-INPUT                  PIC X(80)   VALUE SPACES.
000530
000540 01  210-USER-INPUT-LC               PIC X(80)   VALUE SPACES.
000550
000560 01  220-LAST-USER-INPUT             PIC X(80)   VALUE SPACES.
000570
000580 01  230-TRANSLATED-INPUT            PIC X(80)   VALUE SPACES.
000590
000600 01  240-REPLY                       PIC X(79)   VALUE SPACES.
000610
000620 01  250-SUBSTITUTE-WORK             PIC X(100)  VALUE SPACES.
000630
000640 01  300-PROGRAM-CONSTANTS.
000650     05  300-MAX-KEYWORD-ENTRIES     PIC S9(4)   COMP VALUE +36.
000660     05  300-MAX-SCAN-LEN            PIC S9(4)   COMP VALUE +30.
000670     05  300-SHUT                    PIC X(04)   VALUE "shut".
000680     05  300-ASTERISK                PIC X(01)   VALUE "*".
000690
000700 01  400-PROGRAM-COUNTERS.
000710     05  400-HOLD-KW-LEN             PIC S9(4)   COMP VALUE ZERO.
000720     05  400-SCAN-LEN                PIC S9(4)   COMP VALUE ZERO.
000730     05  400-HOLD-500-K              PIC S9(4)   COMP VALUE +0.
000740     05  400-HOLD-OFFSET             PIC S9(4)   COMP VALUE +0.
000750     05  400-OFFSET                  PIC S9(4)   COMP VALUE +0.
000760     05  400-SUB                     PIC S9(4)   COMP VALUE ZERO.
000770     05  400-SPACES-COUNT            PIC S9(4)   COMP VALUE ZERO.
000780
000790 01  500-KEYWORD-TABLE-DATA.
000800     05  FILLER   PIC X(16)  VALUE "07can you ".
000810     05  FILLER   PIC X(16)  VALUE "05can i ".
000820     05  FILLER   PIC X(16)  VALUE "07you are ".
000830     05  FILLER   PIC X(16)  VALUE "06you're ".
000840     05  FILLER   PIC X(16)  VALUE "07i don't ".
000850     05  FILLER   PIC X(16)  VALUE "06i feel  ".
000860     05  FILLER   PIC X(16)  VALUE "13why don't you ".
000870     05  FILLER   PIC X(16)  VALUE "11why can't i ".
000880     05  FILLER   PIC X(16)  VALUE "07are you ".
000890     05  FILLER   PIC X(16)  VALUE "07i can't ".
000900     05  FILLER   PIC X(16)  VALUE "04i am ".
000910     05  FILLER   PIC X(16)  VALUE "03i'm  ".
000920     05  FILLER   PIC X(16)  VALUE "03you ".
000930     05  FILLER   PIC X(16)  VALUE "06i want ".
000940     05  FILLER   PIC X(16)  VALUE "04what ".
000950     05  FILLER   PIC X(16)  VALUE "03how ".
000960     05  FILLER   PIC X(16)  VALUE "03who ".
000970     05  FILLER   PIC X(16)  VALUE "05where ".
000980     05  FILLER   PIC X(16)  VALUE "04when ".
000990     05  FILLER   PIC X(16)  VALUE "03why ".
001000     05  FILLER   PIC X(16)  VALUE "04name ".
001010     05  FILLER   PIC X(16)  VALUE "05cause ".
001020     05  FILLER   PIC X(16)  VALUE "05sorry ".
001030     05  FILLER   PIC X(16)  VALUE "05dream ".
001040     05  FILLER   PIC X(16)  VALUE "05hello ".
001050     05  FILLER   PIC X(16)  VALUE "02hi ".
001060     05  FILLER   PIC X(16)  VALUE "05maybe ".
001070     05  FILLER   PIC X(16)  VALUE "02no ".
001080     05  FILLER   PIC X(16)  VALUE "04your ".
001090     05  FILLER   PIC X(16)  VALUE "06always ".
001100     05  FILLER   PIC X(16)  VALUE "05think ".
001110     05  FILLER   PIC X(16)  VALUE "05alike ".
001120     05  FILLER   PIC X(16)  VALUE "03yes ".
001130     05  FILLER   PIC X(16)  VALUE "06friend ".
001140     05  FILLER   PIC X(16)  VALUE "08computer ".
001150     05  FILLER   PIC X(16)  VALUE "10NOKEYFOUND".
001160
001170 01  500-KEYWORD-TABLE       REDEFINES 500-KEYWORD-TABLE-DATA.
001180     05  500-KEYWORD-ENTRY       OCCURS 36 TIMES
001190                                 INDEXED BY 500-K.
001200         10  500-KW-LEN              PIC 9(02).
001210         10  500-KEYWORD             PIC X(14).
001220
001230 01  520-TRANSLATION-CONSTANTS.
001240     05 520-THING-IN                 PIC X(05)   VALUE "thing".
001250     05 520-HIGH-IN                  PIC X(04)   VALUE "high".
001260     05 520-SHI-IN                   PIC X(03)   VALUE "shi".
001270     05 520-CHI-IN                   PIC X(03)   VALUE "chi".
001280     05 520-HIT-IN                   PIC X(03)   VALUE "hit".
001290     05 520-OUR-IN                   PIC X(03)   VALUE "our".
001300     05 520-QMARK-IN                 PIC X(02)   VALUE "? ".
001310     05 520-XMARK-IN                 PIC X(02)   VALUE "! ".
001320     05 520-FSTOP-IN                 PIC X(02)   VALUE ". ".
001330
001340     05 520-THING-OUT                PIC X(05)   VALUE "th!ng".
001350     05 520-HIGH-OUT                 PIC X(04)   VALUE "h!gh".
001360     05 520-SHI-OUT                  PIC X(03)   VALUE "sh!".
001370     05 520-CHI-OUT                  PIC X(03)   VALUE "ch!".
001380     05 520-HIT-OUT                  PIC X(03)   VALUE "h!t".
001390     05 520-OUR-OUT                  PIC X(03)   VALUE "0ur".
001400     05 520-QMARK-OUT                PIC X(02)   VALUE "  ".
001410     05 520-FSTOP-OUT                PIC X(02)   VALUE "  ".
001420
001430     05 520-ARE-IN                   PIC X(05)   VALUE " are ".
001440     05 520-WERE-IN                  PIC X(06)   VALUE " were ".
001450     05 520-YOU-IN                   PIC X(05)   VALUE " you ".
001460     05 520-YOUR-IN                  PIC X(06)   VALUE " your ".
001470     05 520-MY-IN                    PIC X(04)   VALUE " my ".
001480     05 520-IVE-IN                   PIC X(06)   VALUE " i've ".
001490     05 520-IM-IN                    PIC X(05)   VALUE " i'm ".
001500     05 520-I-AM-IN                  PIC X(06)   VALUE " i am ".
001510     05 520-ME-IN                    PIC X(04)   VALUE " me ".
001520     05 520-I-IN                     PIC X(03)   VALUE " i ".
001530     05 520-YOURE-IN              PIC X(08)   VALUE " you're ".
001540     05 520-YOU-ARE-IN            PIC X(09)   VALUE " you are ".
001550     05 520-YOURSELF-IN           PIC X(10)   VALUE " yourself ".
001560
001570     05 520-AM-OUT                   PIC X(04)   VALUE " am ".
001580     05 520-WAS-OUT                  PIC X(05)   VALUE " was ".
001590     05 520-I-FIX                    PIC X(04)   VALUE " i# ".
001600     05 520-IM-FIX                   PIC X(06)   VALUE " i'm# ".
001610     05 520-I-AM-FIX                 PIC X(07)   VALUE " i am# ".
001620     05 520-MY-FIX                   PIC X(05)   VALUE " my# ".
001630     05 520-YOUR-FIX                 PIC X(07)   VALUE " your# ".
001640     05 520-YOUVE-OUT             PIC X(08)   VALUE " you've ".
001650     05 520-YOURE-OUT             PIC X(08)   VALUE " you're ".
001660     05 520-YOU-FIX                  PIC X(06)   VALUE " you# ".
001670     05 520-MYSELF-OUT            PIC X(08)   VALUE " myself ".
001680
001690     05 520-I-OUT                    PIC X(03)   VALUE " I ".
001700     05 520-IM-OUT                   PIC X(05)   VALUE " I'm ".
001710     05 520-I-AM-OUT                 PIC X(06)   VALUE " I am ".
001720     05 520-MY-OUT                   PIC X(04)   VALUE " my ".
001730     05 520-YOUR-OUT                 PIC X(06)   VALUE " your ".
001740     05 520-YOU-OUT                  PIC X(05)   VALUE " you ".
001750
001760
001770 01  540-REPLY-TABLE-DATA.
001780     05  PIC x(60)   VALUE "29Don't you believe that I can*".
001790     05  PIC X(60)   VALUE "29Perhaps you would like me to*".
001800     05  PIC x(60)   VALUE "29Do you want me to be able to*".
001810     05  PIC x(60)   VALUE "26Perhaps you don't want to*".
001820     05  PIC x(60)   VALUE "26Do you want to be able to*".
001830     05  PIC x(60)   VALUE "26What makes you think I am*".
001840
001850     05  PIC X(30)   VALUE "35Does it please you to believ".
001860     05  PIC X(30)   VALUE "e I am*".
001870
001880     05  PIC x(60)   VALUE "29Perhaps you would like to be*".
001890
001900     05  PIC X(30)   VALUE "31Do you sometimes wish you we".
001910     05  PIC X(30)   VALUE "re*".
001920
001930     05  PIC x(60)   VALUE "17Don't you really*".
001940     05  PIC x(60)   VALUE "14Why don't you*".
001950     05  PIC x(60)   VALUE "26Do you wish to be able to*".
001960     05  PIC x(60)   VALUE "22Does that trouble you?".
001970     05  PIC x(60)   VALUE "18Do you often feel*".
001980     05  PIC x(60)   VALUE "18Do you often feel*".
001990     05  PIC x(60)   VALUE "21Do you enjoy feeling*".
002000     05  PIC x(60)   VALUE "30Do you really believe I don't*".
002010     05  PIC x(60)   VALUE "28Perhaps in good time I will*".
002020     05  PIC x(60)   VALUE "18Do you want me to*".
002030
002040     05  PIC X(30)   VALUE "35Do you think you should be a".
002050     05  PIC X(30)   VALUE "ble to*".
002060
002070     05  PIC x(60)   VALUE "14Why can't you*".
002080
002090     05  PIC X(30)   VALUE "46Why are you interested in wh".
002100     05  PIC X(30)   VALUE "ether or not I am*".
002110
002120     05  PIC x(60)   VALUE "31Would you prefer if I were not*".
002130     05  PIC x(60)   VALUE "31Perhaps in your fantasies I am*".
002140     05  PIC x(60)   VALUE "26How do you know you can't*".
002150     05  PIC x(60)   VALUE "15Have you tried?".
002160     05  PIC x(60)   VALUE "20Perhaps you can now*".
002170
002180     05  PIC X(30)   VALUE "35Did you come to me because y".
002190     05  PIC X(30)   VALUE "ou are*".
002200
002210     05  PIC x(60)   VALUE "23How long have you been*".
002220
002230     05  PIC X(30)   VALUE "34Do you believe it is normal ".
002240     05  PIC X(30)   VALUE "to be*".
002250
002260     05  PIC x(60)   VALUE "19Do you enjoy being*".
002270     05  PIC x(60)   VALUE "31We were discussing you--not me.".
002280     05  PIC x(60)   VALUE "06Oh, I*".
002290
002300     05  PIC X(30)   VALUE "44You're not really talking ab".
002310     05  PIC X(30)   VALUE "out me, are you?".
002320
002330     05  PIC X(30)   VALUE "37What would it mean to you if".
002340     05  PIC X(30)   VALUE " you got*".
002350
002360     05  PIC x(60)   VALUE "16Why do you want*".
002370     05  PIC x(60)   VALUE "21Suppose you soon got*".
002380     05  PIC x(60)   VALUE "22What if you never got*".
002390     05  PIC x(60)   VALUE "22I sometimes also want*".
002400     05  PIC x(60)   VALUE "15Why do you ask?".
002410     05  PIC x(60)   VALUE "32Does that question interest you?".
002420
002430     05  PIC X(30)   VALUE "38What answer would please you".
002440     05  PIC X(30)   VALUE " the most?".
002450
002460     05  PIC x(60)   VALUE "18What do you think?".
002470
002480     05  PIC X(30)   VALUE "38Are such questions on your m".
002490     05  PIC X(30)   VALUE "ind often?".
002500
002510     05  PIC X(30)   VALUE "40What is it that you really w".
002520     05  PIC X(30)   VALUE "ant to know?".
002530
002540     05  PIC x(60)   VALUE "27Have you asked anyone else?".
002550
002560     05  PIC X(30)   VALUE "37Have you asked such question".
002570     05  PIC X(30)   VALUE "s before?".
002580
002590     05  PIC X(30)   VALUE "42What else comes to mind when".
002600     05  PIC X(30)   VALUE " you ask that?".
002610
002620     05  PIC x(60)   VALUE "24Names don't interest me.".
002630
002640     05  PIC X(30)   VALUE "41I don't care about names -- ".
002650     05  PIC X(30)   VALUE "Please go on.".
002660
002670     05  PIC x(60)   VALUE "24Is that the real reason?".
002680
002690     05  PIC X(30)   VALUE "37Don't any other reasons come".
002700     05  PIC X(30)   VALUE " to mind?".
002710
002720     05  PIC X(30)   VALUE "39Does that reason explain any".
002730     05  PIC X(30)   VALUE "thing else?".
002740
002750     05  PIC X(30)   VALUE "34What other reasons might the".
002760     05  PIC X(30)   VALUE "re be?".
002770
002780     05  PIC x(60)   VALUE "23Please don't apologize!".
002790     05  PIC x(60)   VALUE "28Apologies are not necessary.".
002800
002810     05  PIC X(30)   VALUE "45What feelings do you have wh".
002820     05  PIC X(30)   VALUE "en you apologize?".
002830
002840     05  PIC x(60)   VALUE "22Don't be so defensive!".
002850
002860     05  PIC X(30)   VALUE "36What does that dream suggest".
002870     05  PIC X(30)   VALUE " to you?".
002880
002890     05  PIC x(60)   VALUE "19Do you dream often?".
002900
002910     05  PIC X(30)   VALUE "35What persons appear in your ".
002920     05  PIC X(30)   VALUE "dreams?".
002930
002940     05  PIC X(30)   VALUE "33Are you disturbed by your dr".
002950     05  PIC X(30)   VALUE "eams?".
002960
002970     05  PIC X(30)   VALUE "43How do you do ...Please stat".
002980     05  PIC X(30)   VALUE "e your problem.".
002990
003000     05  PIC x(60)   VALUE "29You don't seem quite certain.".
003010     05  PIC x(60)   VALUE "23Why the uncertain tone?".
003020     05  PIC x(60)   VALUE "27Can't you be more positive?".
003030     05  PIC x(60)   VALUE "16You aren't sure?".
003040     05  PIC x(60)   VALUE "15Don't you know?".
003050
003060     05  PIC X(30)   VALUE "38Are you saying no just to be".
003070     05  PIC X(30)   VALUE " negative?".
003080
003090     05  PIC x(60)   VALUE "29You are being a bit negative.".
003100     05  PIC x(60)   VALUE "08Why not?".
003110     05  PIC x(60)   VALUE "13Are you sure?".
003120     05  PIC x(60)   VALUE "07Why no?".
003130     05  PIC x(60)   VALUE "31Why are you concerned about my*".
003140     05  PIC x(60)   VALUE "20What about your own*".
003150
003160     05  PIC X(30)   VALUE "36Can you think of a specific ".
003170     05  PIC X(30)   VALUE "example?".
003180
003190     05  PIC x(60)   VALUE "05When?".
003200     05  PIC x(60)   VALUE "25What are you thinking of?".
003210     05  PIC x(60)   VALUE "15Really, always?".
003220     05  PIC x(60)   VALUE "23Do you really think so?".
003230     05  PIC x(60)   VALUE "21But you are not sure*".
003240     05  PIC x(60)   VALUE "13Do you doubt*".
003250     05  PIC x(60)   VALUE "12In what way?".
003260     05  PIC x(60)   VALUE "28What resemblance do you see?".
003270
003280     05  PIC X(30)   VALUE "40What does the similarity sug".
003290     05  PIC X(30)   VALUE "gest to you?".
003300
003310     05  PIC X(30)   VALUE "34What other connections do yo".
003320     05  PIC X(30)   VALUE "u see?".
003330
003340     05  PIC X(30)   VALUE "38Could there really be some c".
003350     05  PIC X(30)   VALUE "onnection?".
003360
003370     05  PIC x(60)   VALUE "04How?".
003380     05  PIC x(60)   VALUE "24You seem quite positive.".
003390     05  PIC x(60)   VALUE "13Are you sure?".
003400     05  PIC x(60)   VALUE "06I see.".
003410     05  PIC x(60)   VALUE "13I understand.".
003420
003430     05  PIC X(30)   VALUE "41Why do you bring up the topi".
003440     05  PIC X(30)   VALUE "c of friends?".
003450
003460     05  PIC x(60)   VALUE "26Do your friends worry you?".
003470     05  PIC x(60)   VALUE "28Do your friends pick on you?".
003480
003490     05  PIC X(30)   VALUE "34Are you sure you have any fr".
003500     05  PIC X(30)   VALUE "iends?".
003510
003520     05  PIC x(60)   VALUE "30Do you impose on your friends?".
003530
003540     05  PIC X(30)   VALUE "42Perhaps your love for friend".
003550     05  PIC X(30)   VALUE "s worries you.".
003560
003570     05  PIC x(60)   VALUE "23Do computers worry you?".
003580
003590     05  PIC X(30)   VALUE "39Are you talking about me in ".
003600     05  PIC X(30)   VALUE "particular?".
003610
003620     05  PIC X(30)   VALUE "31Are you frightened by machin".
003630     05  PIC X(30)   VALUE "es?".
003640
003650     05  PIC x(60)   VALUE "29Why do you mention computers?".
003660
003670     05  PIC X(30)   VALUE "56What do you think machines h".
003680     05  PIC X(30)   VALUE "ave to do with your problem?".
003690
003700     05  PIC X(30)   VALUE "42Don't you think computers ca".
003710     05  PIC X(30)   VALUE "n help people?".
003720
003730     05  PIC X(30)   VALUE "43What is it about machines th".
003740     05  PIC X(30)   VALUE "at worries you?".
003750
003760     05  PIC X(30)   VALUE "44Say, do you have any psychol".
003770     05  PIC X(30)   VALUE "ogical problems?".
003780
003790     05  PIC x(60)   VALUE "30What does that suggest to you?".
003800     05  PIC x(60)   VALUE "06I see.".
003810
003820     05  PIC X(30)   VALUE "36I'm not sure I understand yo".
003830     05  PIC X(30)   VALUE "u fully.".
003840
003850     05  PIC X(30)   VALUE "36Come, Come, elucidate your t".
003860     05  PIC X(30)   VALUE "houghts.".
003870
003880     05  PIC x(60)   VALUE "26Can you elaborate on that?".
003890     05  PIC x(60)   VALUE "26That is quite interesting.".
003900
003910 01  540-REPLY-TABLE         REDEFINES 540-REPLY-TABLE-DATA.
003920     05  540-REPLY-ENTRY         OCCURS 112 TIMES
003930                                 INDEXED BY 540-R.
003940         10  540-REPLY-LENGTH        PIC 9(02).
003950         10  540-REPLY               PIC X(58).
003960
003970
003980 01  560-REPLY-LOCATER-DATA.
003990     05  FILLER      PIC X(12)   VALUE "000100030004".
004000     05  FILLER      PIC X(12)   VALUE "000400050005".
004010     05  FILLER      PIC X(12)   VALUE "000600090009".
004020     05  FILLER      PIC X(12)   VALUE "000600090009".
004030     05  FILLER      PIC X(12)   VALUE "001000130013".
004040     05  FILLER      PIC X(12)   VALUE "001400160016".
004050     05  FILLER      PIC X(12)   VALUE "001700190019".
004060     05  FILLER      PIC X(12)   VALUE "002000210021".
004070     05  FILLER      PIC X(12)   VALUE "002200240024".
004080     05  FILLER      PIC X(12)   VALUE "002500270027".
004090     05  FILLER      PIC X(12)   VALUE "002800310031".
004100     05  FILLER      PIC X(12)   VALUE "002800310031".
004110     05  FILLER      PIC X(12)   VALUE "003200340034".
004120     05  FILLER      PIC X(12)   VALUE "003500390039".
004130     05  FILLER      PIC X(12)   VALUE "004000480048".
004140     05  FILLER      PIC X(12)   VALUE "004000480048".
004150     05  FILLER      PIC X(12)   VALUE "004000480048".
004160     05  FILLER      PIC X(12)   VALUE "004000480048".
004170     05  FILLER      PIC X(12)   VALUE "004000480048".
004180     05  FILLER      PIC X(12)   VALUE "004000480048".
004190     05  FILLER      PIC X(12)   VALUE "004900500050".
004200     05  FILLER      PIC X(12)   VALUE "005100540054".
004210     05  FILLER      PIC X(12)   VALUE "005500580058".
004220     05  FILLER      PIC X(12)   VALUE "005900620062".
004230     05  FILLER      PIC X(12)   VALUE "006300630063".
004240     05  FILLER      PIC X(12)   VALUE "006300630063".
004250     05  FILLER      PIC X(12)   VALUE "006400680068".
004260     05  FILLER      PIC X(12)   VALUE "006900730073".
004270     05  FILLER      PIC X(12)   VALUE "007400750075".
004280     05  FILLER      PIC X(12)   VALUE "007600790079".
004290     05  FILLER      PIC X(12)   VALUE "008000820082".
004300     05  FILLER      PIC X(12)   VALUE "008300890089".
004310     05  FILLER      PIC X(12)   VALUE "009000920092".
004320     05  FILLER      PIC X(12)   VALUE "009300980098".
004330     05  FILLER      PIC X(12)   VALUE "009901050105".
004340     05  FILLER      PIC X(12)   VALUE "010601120112".
004350
004360 01  560-REPLY-LOCATER-TABLE REDEFINES 560-REPLY-LOCATER-DATA.
004370   05  560-REPLY-LOCATER-ENTRY OCCURS 36 TIMES INDEXED BY 560-L.
004380         10  560-REPLY-LO            PIC 9(04).
004390         10  560-REPLY-HI            PIC 9(04).
004400         10  560-REPLY-LAST-USED     PIC 9(04).
004410
004420 01  600-PROGRAM-MESSAGES.
004430     05  600-REPLY-LIST.
004440         10  FILLER                  PIC X(07)   VALUE 'Reply: '.
004450         10  600-REPLY-DATA          PIC X(70)   VALUE SPACES.
004460
004470     05  600-INITIAL-MESSAGE         PIC X(40)   VALUE
004480         "Hi!  I'm ELIZA.  What's your problem?".
004490
004500     05  600-GOODBYE-MESSAGE         PIC X(40)   VALUE
004510         "If that's how you feel--goodbye...".
004520
004530     05  600-NO-REPEAT-MSG           PIC X(32)   VALUE
004540         "Please don't repeat yourself!".
004550
004560 PROCEDURE DIVISION.
004570
004580****************************************************************
004590*    0 0 0 0 - M A I N L I N E .                               *
004600****************************************************************
004610*    START THE PSYCHOTHERAPIST DIALOG WITH THE USER, ANALYZE   *
004620*    THE USER INPUT AND GENERATE THE REPLIES.  THE USER CAN    *
004630*    TYPE "SHUT UP" OR SIMPLY "SHUT" TO TERMINATE THE SESSION. *
004640****************************************************************
004650
004660 0000-MAINLINE.
004670
004680     DISPLAY SPACE
004690     MOVE SPACE                  TO 100-EOF-FLAG
004700     DISPLAY 600-INITIAL-MESSAGE
004710     PERFORM UNTIL 88-100-ALL-DONE
004720         ACCEPT 200-USER-INPUT
004730         MOVE FUNCTION LOWER-CASE (200-USER-INPUT)
004740                                 TO 210-USER-INPUT-LC
004750         IF 210-USER-INPUT-LC (1:4) = 300-SHUT
004760             SET 88-100-ALL-DONE TO TRUE
004770             DISPLAY 600-GOODBYE-MESSAGE
004780         ELSE
004790             IF 210-USER-INPUT-LC = 220-LAST-USER-INPUT
004800                 DISPLAY 600-NO-REPEAT-MSG
004810             ELSE
004820                 MOVE 210-USER-INPUT-LC
004830                                 TO 220-LAST-USER-INPUT
004840                 PERFORM 1000-SCAN-FOR-KEYWORD
004850                 IF 400-HOLD-OFFSET > ZERO
004860                     PERFORM 2000-TRANSLATE-USER-INPUT
004870                 END-IF
004880                 PERFORM 3000-BUILD-KEYWORD-REPLY
004890             END-IF
004900         END-IF
004910     END-PERFORM
004920
004930     STOP RUN.
004940
004950****************************************************************
004960*    1 0 0 0 - S C A N - F O R - K E Y W O R D .               *
004970****************************************************************
004980*    SEARCH THE USER INPUT FOR KEYWORDS THAT WILL TRIGGER      *
004990*    THE RESPONSES FROM THE REPLY TABLE.                       *
005000****************************************************************
005010
005020 1000-SCAN-FOR-KEYWORD.
005030
005040     PERFORM 1100-MASK-STRING-HI
005050
005060     SET 88-100-KEYWORD-NOT-FOUND TO TRUE
005070     MOVE ZERO                   TO 400-HOLD-OFFSET
005080     PERFORM VARYING 400-SUB FROM +1 BY +1
005090             UNTIL   400-SUB > 300-MAX-SCAN-LEN
005100             OR      88-100-KEYWORD-FOUND
005110         PERFORM VARYING 500-K FROM +1 BY +1
005120                 UNTIL   500-K > 300-MAX-KEYWORD-ENTRIES
005130                 OR      88-100-KEYWORD-FOUND
005140             MOVE 500-KW-LEN (500-K)
005150                                 TO 400-HOLD-KW-LEN
005160             IF 210-USER-INPUT-LC (400-SUB:400-HOLD-KW-LEN) =
005170                     500-KEYWORD (500-K)
005180                 SET 400-HOLD-500-K TO 500-K
005190                 SET 88-100-KEYWORD-FOUND TO TRUE
005200                 COMPUTE 400-HOLD-OFFSET =
005210                     400-SUB + 400-HOLD-KW-LEN
005220                 COMPUTE 400-SUB = 400-SCAN-LEN + 1
005230             END-IF
005240         END-PERFORM
005250     END-PERFORM
005260
005270     IF 88-100-KEYWORD-NOT-FOUND
005280         MOVE 300-MAX-KEYWORD-ENTRIES
005290                                 TO 400-HOLD-500-K
005300         SET 88-100-KEYWORD-FOUND TO TRUE
005310     END-IF
005320
005330     PERFORM 1200-RESTORE-STRING-HI
005340     .
005350
005360****************************************************************
005370*    1 1 0 0 - M A S K - S T R I N G - H I .                   *
005380****************************************************************
005390*    WORDS LIKE "THING" AND "HIGH" WERE CAUSING A KEYWORD      *
005400*    "HI" MATCH THAT TRIGGERED THE HELLO/HI KEYWORD RESPONSES, *
005410*    SO THEY ARE MASKED HERE TO PREVENT THAT.                  *
005420*    ALSO REMOVE TRAILING "?", "!", AND "." CHARACTERS.        *
005430****************************************************************
005440
005450 1100-MASK-STRING-HI.
005460
005470     MOVE FUNCTION SUBSTITUTE
005480         (210-USER-INPUT-LC, 520-THING-IN, 520-THING-OUT,
005490                             520-HIGH-IN,  520-HIGH-OUT,
005500                             520-SHI-IN,   520-SHI-OUT,
005510                             520-CHI-IN,   520-CHI-OUT,
005520                             520-HIT-IN,   520-HIT-OUT,
005530                             520-OUR-IN,   520-OUR-OUT,
005540                             520-QMARK-IN, 520-QMARK-OUT,
005550                             520-XMARK-IN, 520-QMARK-OUT,
005560                             520-FSTOP-IN, 520-FSTOP-OUT)
005570                                 TO 250-SUBSTITUTE-WORK
005580     MOVE 250-SUBSTITUTE-WORK    TO 210-USER-INPUT-LC
005590****************************************************************
005600*    REMOVE MULTIPLE TRAILING QUESTION MARKS, EXCLAMATION      *
005610*    POINTS, AND PERIODS (FULL STOPS).                         *
005620****************************************************************
005630     MOVE FUNCTION SUBSTITUTE
005640         (210-USER-INPUT-LC, 520-QMARK-IN, 520-QMARK-OUT,
005650                             520-XMARK-IN, 520-QMARK-OUT,
005660                             520-FSTOP-IN, 520-FSTOP-OUT)
005670                                 TO 250-SUBSTITUTE-WORK
005680     MOVE 250-SUBSTITUTE-WORK    TO 210-USER-INPUT-LC
005690     MOVE FUNCTION SUBSTITUTE
005700         (210-USER-INPUT-LC, 520-QMARK-IN, 520-QMARK-OUT,
005710                             520-XMARK-IN, 520-QMARK-OUT,
005720                             520-FSTOP-IN, 520-FSTOP-OUT)
005730                                 TO 250-SUBSTITUTE-WORK
005740     MOVE 250-SUBSTITUTE-WORK    TO 210-USER-INPUT-LC
005750     .
005760
005770****************************************************************
005780*    1 2 0 0 - R E S T O R E - S T R I N G - H I .             *
005790****************************************************************
005800*    AFTER COMPLETING THE KEYWORD SEARCH, RESTORE THE "HI"     *
005810*    STRING IN THE USER INPUT.                                 *
005820****************************************************************
005830
005840 1200-RESTORE-STRING-HI.
005850
005860     MOVE FUNCTION SUBSTITUTE
005870         (210-USER-INPUT-LC, 520-THING-OUT, 520-THING-IN,
005880                             520-HIGH-OUT,  520-HIGH-IN,
005890                             520-SHI-OUT,   520-SHI-IN,
005900                             520-CHI-OUT,   520-CHI-IN,
005910                             520-HIT-OUT,   520-HIT-IN,
005920                             520-OUR-OUT,   520-OUR-IN)
005930                                 TO 250-SUBSTITUTE-WORK
005940     MOVE 250-SUBSTITUTE-WORK    TO 210-USER-INPUT-LC
005950     .
005960
005970****************************************************************
005980*    2 0 0 0 - T R A N S L A T E - U S E R - I N P U T .       *
005990****************************************************************
006000*    PERFORM PRONOUN REPLACEMENT AND CONJUGATION ON THE USER   *
006010*    INPUT SO IT WILL SOUND FAIRLY NORMAL WHEN APPENDED TO     *
006020*    THE DOCTOR'S REPLY.                                       *
006030****************************************************************
006040
006050 2000-TRANSLATE-USER-INPUT.
006060
006070     MOVE 210-USER-INPUT-LC (400-HOLD-OFFSET:)
006080                                 TO 230-TRANSLATED-INPUT.
006090
006100     MOVE FUNCTION SUBSTITUTE
006110         (230-TRANSLATED-INPUT, 520-ARE-IN,  520-AM-OUT,
006120                                520-WERE-IN, 520-WAS-OUT
006130                                520-YOU-IN,  520-I-FIX,
006140                                520-YOUR-IN, 520-MY-FIX,
006150                                520-MY-IN,   520-YOUR-FIX,
006160                                520-IVE-IN,  520-YOUVE-OUT,
006170                                520-IM-IN,   520-YOURE-OUT,
006180                                520-I-AM-IN, 520-YOURE-OUT,
006190                                520-ME-IN,   520-YOU-FIX,
006200                                520-I-IN,    520-YOU-FIX,
006210                                520-YOURE-IN 520-IM-FIX,
006220                            520-YOU-ARE-IN   520-I-AM-FIX,
006230                            520-YOURSELF-IN, 520-MYSELF-OUT)
006240                                 TO 250-SUBSTITUTE-WORK.
006250
006260     MOVE 250-SUBSTITUTE-WORK TO 230-TRANSLATED-INPUT.
006270
006280     MOVE FUNCTION SUBSTITUTE
006290         (230-TRANSLATED-INPUT, 520-I-FIX,     520-I-OUT,
006300                                520-IM-FIX,    520-IM-OUT,
006310                                520-I-AM-FIX,  520-I-AM-OUT,
006320                                520-MY-FIX,    520-MY-OUT,
006330                                520-YOUR-FIX,  520-YOUR-OUT,
006340                                520-YOU-FIX,   520-YOU-OUT)
006350                                 TO 250-SUBSTITUTE-WORK.
006360
006370     MOVE 250-SUBSTITUTE-WORK    TO 230-TRANSLATED-INPUT
006380     .
006390
006400****************************************************************
006410*    3 0 0 0 - B U I L D - K E Y W O R D - R E P L Y .         *
006420****************************************************************
006430*    BUILD THE REPLY BASED ON THE KEYWORD FOUND IN THE USER    *
006440*    INPUT.  NOTE THERE ARE A VARIABLE NUMBER OF POSSIBLE      *
006450*    REPLIES FOR EACH KEYWORD, AND SOME REPLIES INCLUDE TEXT   *
006460*    ECHOED FROM THE USER INPUT.                               *
006470****************************************************************
006480
006490 3000-BUILD-KEYWORD-REPLY.
006500
006510     SET 560-L                   TO 400-HOLD-500-K
006520     ADD +1                      TO 560-REPLY-LAST-USED (560-L)
006530     IF 560-REPLY-LAST-USED (560-L) > 560-REPLY-HI (560-L)
006540         MOVE 560-REPLY-LO (560-L) TO 560-REPLY-LAST-USED (560-L)
006550     END-IF
006560
006570     SET 540-R                    TO 560-REPLY-LAST-USED (560-L)
006580     MOVE 540-REPLY (540-R)       TO 240-REPLY
006590     MOVE 540-REPLY-LENGTH (540-R)    TO 400-SUB
006600     IF 240-REPLY (400-SUB:1) = 300-ASTERISK
006610         MOVE SPACE               TO 240-REPLY (400-SUB:1)
006620         MOVE 230-TRANSLATED-INPUT
006630                                  TO 240-REPLY (400-SUB:)
006640         PERFORM 3100-FIX-MORE-BAD-GRAMMAR
006650         MOVE ZERO                TO 400-SPACES-COUNT
006660         INSPECT 240-REPLY TALLYING 400-SPACES-COUNT
006670             FOR TRAILING SPACES
006680****************************************************************
006690*        MERGE USER INPUT INTO THE REPLY AND THEN CORRECT      *
006700*        ENDING PUNCTUATION FOR "?" OR "." (FULL-STOP).        *
006710****************************************************************
006720         IF  400-SPACES-COUNT > ZERO
006730         AND 400-SPACES-COUNT < (LENGTH OF 240-REPLY) - 1
006740             COMPUTE 400-OFFSET =
006750                 (LENGTH OF 240-REPLY) - 400-SPACES-COUNT + 1
006760             END-COMPUTE
006770             IF 560-REPLY-LAST-USED (560-L) = 02 OR 04 OR 05
006780             OR 08 OR 18 OR 24 OR 33 OR 39 OR 81
006790                 MOVE "."         TO 240-REPLY (400-OFFSET:1)
006800             ELSE
006810                 MOVE "?"         TO 240-REPLY (400-OFFSET:1)
006820             END-IF
006830         END-IF
006840     END-IF
006850
006860     DISPLAY 240-REPLY
006870     .
006880
006890****************************************************************
006900*    3 1 0 0 - F I X - M O R E - B A D - G R A M M A R .       *
006910****************************************************************
006920*    HERE ARE SOME MORE FIXUPS FOR GRAMMAR PROBLEMS.  BUT IT   *
006930*    DOESN'T SOLVE ALL OF THEM.                                *
006940****************************************************************
006950
006960 3100-FIX-MORE-BAD-GRAMMAR.
006970
006980     MOVE FUNCTION SUBSTITUTE (240-REPLY,
006990         " you want I ",            " you want me ",
007000         " you got I ",             " you got me ",
007010         " to make I ",             " to make me ",
007020         " you been I ",            " you been me ",
007030         " you be I ",              " you be me ",
007040         " to be I ",               " to be me ",
007050         " soon got I ",            " soon got me ",
007060         " never got I ",           " never got me ",
007070         " sometimes also want I ", " sometimes also want me ",
007080         " normal to be I ",        " normal to be me ",
007090         " enjoy being I ",         " enjoy being me ",
007100         " can't make I ",          " can't make me ",
007110         " can now make I ",        " can now make me ",
007120         " I are ",                 " I am ",
007130         " you am ",                " you are ",
007140         " with I ",                " with me")
007150                                 TO 250-SUBSTITUTE-WORK.
007160
007170     MOVE 250-SUBSTITUTE-WORK TO 240-REPLY.
007180
007190 END PROGRAM ELIZA.
