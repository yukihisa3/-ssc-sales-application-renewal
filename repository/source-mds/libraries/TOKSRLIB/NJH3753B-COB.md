# NJH3753B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NJH3753B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹　　　　　　　　　　　　　*
*    業務名　　　　　　　：　受注　　　　　　　　　　　　　　　*
*    モジュール名　　　　：　ナフコ発注データ累積              *
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
 PROGRAM-ID.           NJH3753B.
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
     SELECT   NFHACSF   ASSIGN    TO    DA-01-S-NFHACSF
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
 FD  NFHACSF           BLOCK CONTAINS  1   RECORDS
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
     COPY                  NFHACSF2       OF   XFDLIB
     JOINING               WK             AS   PREFIX.
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH3753B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH3753B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH3753B".
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
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFHACSF.
     MOVE      "NFHACSF"    TO   AB-FILE.
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
     OPEN    INPUT     NFHACSF.
 INIT-1.
     OPEN    I-O       NFHACL1.
*
 INIT-2.
     READ    NFHACSF
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
     READ         NFHACSF    AT   END
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
     IF  IN-REC(1:3) =  "A01"
         MOVE  WK-HEAD(1:56) TO   OUT-REC(1:56)
         MOVE  ZERO          TO   OUT-AH09
         MOVE  IN-REC(1:646) TO   OUT-REC(75:646)
     END-IF.
     WRITE     OUT-REC.
     ADD       1             TO   WT-CNT.
     GO                      TO   MAIN-05.
*
 MAIN-04.
     MOVE         SPACE      TO   OUT-REC.
     INITIALIZE                   OUT-REC.
     IF  IN-REC(1:3) =  "A01"
         MOVE  WK-HEAD(1:56) TO   OUT-REC(1:56)
         MOVE  ZERO          TO   OUT-AH09
         MOVE  IN-REC(1:646) TO   OUT-REC(75:646)
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
     CLOSE   NFHACSF.
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
 END PROGRAM NJH3753B.

```
