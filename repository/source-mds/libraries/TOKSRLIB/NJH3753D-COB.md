# NJH3753D

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NJH3753D.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹　　　　　　　　　　　　　*
*    業務名　　　　　　　：　受注　　　　　　　　　　　　　　　*
*    モジュール名　　　　：　ナフコ発注データ累積制御Ｂ編集    *
*    作成日　　　　　　　：　2019/12/03 INOUE                  *
*    処理概要　　　　　　：　ＷＥＢ－ＥＤＩ受信データを　　　　*
*                            累積する。以降処理はこれを　　　　*
*                            ＩＮＰＵＴとする。                *
*    更新日　　　　　　　：　    /  /                          *
*    更新内容　　　　　　：　　　　　　　　　　　　　　　　　　*
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
*
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NJH3753D.
 AUTHOR.               NAV-ASSIST.
 DATE-WRITTEN.         2019/12/03.
*
 ENVIRONMENT           DIVISION.
 CONFIGURATION         SECTION.
 SOURCE-COMPUTER.      FUJITSU.
 OBJECT-COMPUTER.      FUJITSU.
 SPECIAL-NAMES.
     CONSOLE IS        CONS.
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*ナフコ発注データ
     SELECT   NFHACWK   ASSIGN    TO    DA-01-S-NFHACWK
                        ACCESS MODE    IS    SEQUENTIAL
                        ORGANIZATION   IS    SEQUENTIAL
                        FILE   STATUS  IS    IN-ST.
*ナフコ発注累積データ
     SELECT   NFHACL1   ASSIGN    TO   DA-01-VI-NFHACL1
                        ACCESS MODE    IS    DYNAMIC
                        ORGANIZATION   IS    INDEXED
                        RECORD KEY     IS    OUT-A83
                                             OUT-A88
                                             OUT-A25
                                             OUT-A26
                                             OUT-A22
                                             OUT-A23
                                             OUT-A24
                        FILE   STATUS  IS    OUT-ST.
*
******************************************************************
*                                                                *
*    DATA              DIVISION                                  *
*                                                                *
******************************************************************
*
 DATA                  DIVISION.
 FILE                  SECTION.
 FD  NFHACWK           BLOCK CONTAINS  1   RECORDS
                       LABEL RECORD   IS   STANDARD.
 01  IN-REC.
   03  IN-RECORD       PIC X(722).

 FD  NFHACL1
                       LABEL RECORD   IS   STANDARD.
     COPY              NFHACL1        OF   XFDLIB
     JOINING           OUT            AS   PREFIX.

 WORKING-STORAGE       SECTION.
*
******************************************************************
*    WORKING-STORAGE   SECTION                                   *
******************************************************************
*
 77  IN-ST                 PIC XX    VALUE     "00".
 77  OUT-ST                PIC XX    VALUE     "00".
 77  END-FLG               PIC X     VALUE     " ".
 01  RD-CNT                PIC  9(08)     VALUE  ZERO.
 01  WT-CNT                PIC  9(07)     VALUE  ZERO.
*
 01  WK-HEAD.
   03  WK-HEAD-01          PIC X(722) VALUE SPACE.
*
     COPY                  NFHACWK        OF   XFDLIB
     JOINING               WK             AS   PREFIX.
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH3753D".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH3753D".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH3753D".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 LINKAGE SECTION.
   01  LINK-01             PIC 9(07).
******************************************************************
*                                                                *
*    PROCEDURE         DIVISION                                  *
*                                                                *
******************************************************************
*
 PROCEDURE             DIVISION       USING LINK-01.
*PROCEDURE             DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFHACWK.
     MOVE      "NFHACWK"    TO   AB-FILE.
     MOVE      IN-ST        TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFHACL1.
     MOVE      "NFHACL1"    TO   AB-FILE.
     MOVE      OUT-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
 PRG-CONTROL  SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC.
     PERFORM      END-SEC.
     STOP RUN.
*
*----------------------------------------------------------------*
*       LEVEL     1    ｲﾆｼｬﾙ ｼｮﾘ                                 *
*----------------------------------------------------------------*
*
 INIT-SEC  SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     DISPLAY  MSG-START UPON CONS.
*
     OPEN    INPUT     NFHACWK.
 INIT-1.
     OPEN    I-O       NFHACL1.
*
 INIT-2.
     READ    NFHACWK
       AT END
             DISPLAY   NC"＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
             DISPLAY   NC"＃＃　データ０件です　＃＃" UPON CONS
             DISPLAY   NC"＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
*            MOVE      4010       TO      PROGRAM-STATUS
             STOP      RUN
     END-READ.
*
 INIT-3.
     IF      IN-REC(1:3) NOT = "AH1"
             DISPLAY   NC"＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
             DISPLAY   NC"＃＃　発注データが　　＃＃" UPON CONS
             DISPLAY   NC"＃＃　異常です！！　　＃＃" UPON CONS
             DISPLAY   NC"＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
             MOVE      4010       TO      PROGRAM-STATUS
             STOP      RUN
     END-IF.
*
 INIT-4.
     MOVE    IN-REC               TO      WK-HEAD.
     ADD     1                    TO      RD-CNT.
*
 INIT-EXT.
     EXIT.
*
*----------------------------------------------------------------*
*       LEVEL     1    ﾒｲﾝ   ｼｮﾘ                                 *
*----------------------------------------------------------------*
*
 MAIN-SEC   SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
 MAIN-01.
     READ         NFHACWK    AT   END
                  GO         TO   MAIN-EXT.
     ADD          1          TO   RD-CNT.
*
 MAIN-02.
     MOVE         IN-REC     TO   WK-REC.
     MOVE         WK-A83     TO   OUT-A83.
     MOVE         WK-A88     TO   OUT-A88.
     MOVE         WK-A25     TO   OUT-A25.
     MOVE         WK-A26     TO   OUT-A26.
     MOVE         WK-A22     TO   OUT-A22.
     MOVE         WK-A23     TO   OUT-A23.
     MOVE         WK-A24     TO   OUT-A24.
     READ         NFHACL1
          INVALID
                  GO         TO   MAIN-03
          NOT INVALID
                  GO         TO   MAIN-04
     END-READ.
*
 MAIN-03.
     MOVE         SPACE      TO   OUT-REC.
     INITIALIZE                   OUT-REC.
     MOVE X"28" TO OUT-A07  OUT-A12  OUT-A17  OUT-A30  OUT-A36
                   OUT-A48  OUT-A52  OUT-A58  OUT-A84  OUT-A89
                   OUT-A96.
     MOVE X"29" TO OUT-A09  OUT-A14  OUT-A19  OUT-A32  OUT-A38
                   OUT-A50  OUT-A54  OUT-A60  OUT-A86  OUT-A91
                   OUT-A98.
     IF  IN-REC(1:3) =  "A01"
         MOVE  WK-HEAD(1:56) TO   OUT-REC(1:56)
         MOVE  ZERO          TO   OUT-AH09
         MOVE  WK-A01        TO   OUT-A01
         MOVE  WK-A02        TO   OUT-A02
         MOVE  WK-A03        TO   OUT-A03
         MOVE  WK-A04        TO   OUT-A04
         MOVE  WK-A05        TO   OUT-A05
         MOVE  WK-A06        TO   OUT-A06
         MOVE  WK-A08        TO   OUT-A08
         MOVE  WK-A10        TO   OUT-A10
         MOVE  WK-A11        TO   OUT-A11
         MOVE  WK-A13        TO   OUT-A13
         MOVE  WK-A15        TO   OUT-A15
         MOVE  WK-A16        TO   OUT-A16
         MOVE  WK-A18        TO   OUT-A18
         MOVE  WK-A20        TO   OUT-A20
         MOVE  WK-A21        TO   OUT-A21
         MOVE  WK-A22        TO   OUT-A22
         MOVE  WK-A23        TO   OUT-A23
         MOVE  WK-A24        TO   OUT-A24
         MOVE  WK-A25        TO   OUT-A25
         MOVE  WK-A26        TO   OUT-A26
         MOVE  WK-A27        TO   OUT-A27
         MOVE  WK-A28        TO   OUT-A28
         MOVE  WK-A29        TO   OUT-A29
         MOVE  WK-A31        TO   OUT-A31
         MOVE  WK-A33        TO   OUT-A33
         MOVE  WK-A34        TO   OUT-A34
         MOVE  WK-A35        TO   OUT-A35
         MOVE  WK-A37        TO   OUT-A37
         MOVE  WK-A39        TO   OUT-A39
         MOVE  WK-A40        TO   OUT-A40
         MOVE  WK-A41        TO   OUT-A41
         MOVE  WK-A42        TO   OUT-A42
         MOVE  WK-A43        TO   OUT-A43
         MOVE  WK-A44        TO   OUT-A44
         MOVE  WK-A45        TO   OUT-A45
         MOVE  WK-A46        TO   OUT-A46
         MOVE  WK-A47        TO   OUT-A47
         MOVE  WK-A49        TO   OUT-A49
         MOVE  WK-A51        TO   OUT-A51
         MOVE  WK-A53        TO   OUT-A53
         MOVE  WK-A55        TO   OUT-A55
         MOVE  WK-A56        TO   OUT-A56
         MOVE  WK-A57        TO   OUT-A57
         MOVE  WK-A59        TO   OUT-A59
         MOVE  WK-A61        TO   OUT-A61
         MOVE  WK-A62        TO   OUT-A62
         MOVE  WK-A63        TO   OUT-A63
         MOVE  WK-A64        TO   OUT-A64
         MOVE  WK-A65        TO   OUT-A65
         MOVE  WK-A66        TO   OUT-A66
         MOVE  WK-A67        TO   OUT-A67
         MOVE  WK-A68        TO   OUT-A68
         MOVE  WK-A69        TO   OUT-A69
         MOVE  WK-A70        TO   OUT-A70
         MOVE  WK-A71        TO   OUT-A71
         MOVE  WK-A72        TO   OUT-A72
         MOVE  WK-A73        TO   OUT-A73
         MOVE  WK-A74        TO   OUT-A74
         MOVE  WK-A75        TO   OUT-A75
         MOVE  WK-A76        TO   OUT-A76
         MOVE  WK-A77        TO   OUT-A77
         MOVE  WK-A78        TO   OUT-A78
         MOVE  WK-A79        TO   OUT-A79
         MOVE  WK-A80        TO   OUT-A80
         MOVE  WK-A81        TO   OUT-A81
         MOVE  WK-A82        TO   OUT-A82
         MOVE  WK-A83        TO   OUT-A83
         MOVE  WK-A85        TO   OUT-A85
         MOVE  WK-A87        TO   OUT-A87
         MOVE  WK-A88        TO   OUT-A88
         MOVE  WK-A90        TO   OUT-A90
         MOVE  WK-A92        TO   OUT-A92
         MOVE  WK-A93        TO   OUT-A93
         MOVE  WK-A94        TO   OUT-A94
         MOVE  WK-A95        TO   OUT-A95
         MOVE  WK-A97        TO   OUT-A97
         MOVE  WK-A99        TO   OUT-A99
     END-IF.
     WRITE     OUT-REC.
     ADD       1             TO   WT-CNT.
     GO                      TO   MAIN-05.
*
 MAIN-04.
     MOVE         SPACE      TO   OUT-REC.
     INITIALIZE                   OUT-REC.
     MOVE X"28" TO OUT-A07  OUT-A12  OUT-A17  OUT-A30  OUT-A36
                   OUT-A48  OUT-A52  OUT-A58  OUT-A84  OUT-A89
                   OUT-A96.
     MOVE X"29" TO OUT-A09  OUT-A14  OUT-A19  OUT-A32  OUT-A38
                   OUT-A50  OUT-A54  OUT-A60  OUT-A86  OUT-A91
                   OUT-A98.
     IF  IN-REC(1:3) =  "A01"
         MOVE  WK-HEAD(1:56) TO   OUT-REC(1:56)
         MOVE  ZERO          TO   OUT-AH09
         MOVE  ZERO          TO   OUT-AH09
         MOVE  WK-A01        TO   OUT-A01
         MOVE  WK-A02        TO   OUT-A02
         MOVE  WK-A03        TO   OUT-A03
         MOVE  WK-A04        TO   OUT-A04
         MOVE  WK-A05        TO   OUT-A05
         MOVE  WK-A06        TO   OUT-A06
         MOVE  WK-A08        TO   OUT-A08
         MOVE  WK-A10        TO   OUT-A10
         MOVE  WK-A11        TO   OUT-A11
         MOVE  WK-A13        TO   OUT-A13
         MOVE  WK-A15        TO   OUT-A15
         MOVE  WK-A16        TO   OUT-A16
         MOVE  WK-A18        TO   OUT-A18
         MOVE  WK-A20        TO   OUT-A20
         MOVE  WK-A21        TO   OUT-A21
         MOVE  WK-A22        TO   OUT-A22
         MOVE  WK-A23        TO   OUT-A23
         MOVE  WK-A24        TO   OUT-A24
         MOVE  WK-A25        TO   OUT-A25
         MOVE  WK-A26        TO   OUT-A26
         MOVE  WK-A27        TO   OUT-A27
         MOVE  WK-A28        TO   OUT-A28
         MOVE  WK-A29        TO   OUT-A29
         MOVE  WK-A31        TO   OUT-A31
         MOVE  WK-A33        TO   OUT-A33
         MOVE  WK-A34        TO   OUT-A34
         MOVE  WK-A35        TO   OUT-A35
         MOVE  WK-A37        TO   OUT-A37
         MOVE  WK-A39        TO   OUT-A39
         MOVE  WK-A40        TO   OUT-A40
         MOVE  WK-A41        TO   OUT-A41
         MOVE  WK-A42        TO   OUT-A42
         MOVE  WK-A43        TO   OUT-A43
         MOVE  WK-A44        TO   OUT-A44
         MOVE  WK-A45        TO   OUT-A45
         MOVE  WK-A46        TO   OUT-A46
         MOVE  WK-A47        TO   OUT-A47
         MOVE  WK-A49        TO   OUT-A49
         MOVE  WK-A51        TO   OUT-A51
         MOVE  WK-A53        TO   OUT-A53
         MOVE  WK-A55        TO   OUT-A55
         MOVE  WK-A56        TO   OUT-A56
         MOVE  WK-A57        TO   OUT-A57
         MOVE  WK-A59        TO   OUT-A59
         MOVE  WK-A61        TO   OUT-A61
         MOVE  WK-A62        TO   OUT-A62
         MOVE  WK-A63        TO   OUT-A63
         MOVE  WK-A64        TO   OUT-A64
         MOVE  WK-A65        TO   OUT-A65
         MOVE  WK-A66        TO   OUT-A66
         MOVE  WK-A67        TO   OUT-A67
         MOVE  WK-A68        TO   OUT-A68
         MOVE  WK-A69        TO   OUT-A69
         MOVE  WK-A70        TO   OUT-A70
         MOVE  WK-A71        TO   OUT-A71
         MOVE  WK-A72        TO   OUT-A72
         MOVE  WK-A73        TO   OUT-A73
         MOVE  WK-A74        TO   OUT-A74
         MOVE  WK-A75        TO   OUT-A75
         MOVE  WK-A76        TO   OUT-A76
         MOVE  WK-A77        TO   OUT-A77
         MOVE  WK-A78        TO   OUT-A78
         MOVE  WK-A79        TO   OUT-A79
         MOVE  WK-A80        TO   OUT-A80
         MOVE  WK-A81        TO   OUT-A81
         MOVE  WK-A82        TO   OUT-A82
         MOVE  WK-A83        TO   OUT-A83
         MOVE  WK-A85        TO   OUT-A85
         MOVE  WK-A87        TO   OUT-A87
         MOVE  WK-A88        TO   OUT-A88
         MOVE  WK-A90        TO   OUT-A90
         MOVE  WK-A92        TO   OUT-A92
         MOVE  WK-A93        TO   OUT-A93
         MOVE  WK-A94        TO   OUT-A94
         MOVE  WK-A95        TO   OUT-A95
         MOVE  WK-A97        TO   OUT-A97
         MOVE  WK-A99        TO   OUT-A99
     END-IF.
     REWRITE     OUT-REC.
     ADD       1             TO   WT-CNT.
*
 MAIN-05.
     GO TO   MAIN-SEC.
*
 MAIN-EXT.
     EXIT.
*
*----------------------------------------------------------------*
*       LEVEL     1    ｴﾝﾄﾞ  ｼｮﾘ                                 *
*----------------------------------------------------------------*
*
 END-SEC  SECTION.
     MOVE    "END-SEC"          TO   S-NAME.
     CLOSE   NFHACWK.
     CLOSE   NFHACL1.
     MOVE    RD-CNT    TO      IN-CNT.
     MOVE    WT-CNT    TO      OUT-CNT
                               LINK-01.
     DISPLAY MSG-IN    UPON CONS.
     DISPLAY MSG-OUT   UPON CONS.
     DISPLAY MSG-END   UPON CONS.
**
 END-EXT.
     EXIT.
 END PROGRAM NJH3753D.

```
