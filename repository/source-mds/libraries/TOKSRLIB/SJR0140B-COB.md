# SJR0140B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJR0140B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹　　　　　　　　　　　　　*
*    業務名　　　　　　　：　受領返品　　　　　　　　　　　　　*
*    モジュール名　　　　：　受領ＣＳＶデータ結合              *
*                            　ジュンテンドー(受領・返品）
*    作成日　　　　　　　：　2017/08/30 INOUE                  *
*    処理概要　　　　　　：　共通データ変換のため、ＩＮＰＵＴ　*
*                            とするファイルを結合する。　　　　*
*                                                              *
*    更新日　　　　　　　：　    /  /                          *
*    更新内容　　　　　　：　　　　　　　　　　　　　　　　　　*
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
*
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJR0140B.
 AUTHOR.               NAV-ASSIST.
 DATE-WRITTEN.         2017/08/30.
*
 ENVIRONMENT           DIVISION.
 CONFIGURATION         SECTION.
 SOURCE-COMPUTER.      FUJITSU.
 OBJECT-COMPUTER.      FUJITSU.
 SPECIAL-NAMES.
     CONSOLE IS        CONS.
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受領データＣＳＶ
     SELECT   JUNJYRW1  ASSIGN    TO    DA-01-S-JUNJYRW1
                        ACCESS MODE    IS    SEQUENTIAL
                        ORGANIZATION   IS    SEQUENTIAL
                        FILE   STATUS  IS    IN-ST.
*受領結合ワーク
     SELECT   JUNJYRW2  ASSIGN    TO   DA-01-S-JUNJYRW2
                        ACCESS MODE    IS    SEQUENTIAL
                        ORGANIZATION   IS    SEQUENTIAL
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
 FD  JUNJYRW1          BLOCK CONTAINS 31   RECORDS
                       LABEL RECORD   IS   STANDARD.
 01  IN-REC.
   03  IN-RECORD       PIC X(128).

 FD  JUNJYRW2          BLOCK CONTAINS  1   RECORDS
                       LABEL RECORD   IS   STANDARD.
 01  OUT-REC.
   03  OUT-RECORD      PIC X(256).

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
 01  WT-CNT                PIC  9(08)     VALUE  ZERO.
*
 01  WK-HEAD.
   03  WK-HEAD-01          PIC X(128) VALUE SPACE.
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR0140B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0140B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0140B".
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
   01  LINK-01             PIC X(20).
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
                        PROCEDURE   JUNJYRW1.
     MOVE      "JUNJYRW1"   TO   AB-FILE.
     MOVE      IN-ST        TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JUNJYRW2.
     MOVE      "JUNJYRW2"   TO   AB-FILE.
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
     OPEN    INPUT     JUNJYRW1.
*    IF IN-ST   NOT  =    ZERO
*       DISPLAY "JUNJYRW1 INPUT ERR"  UPON CONS
*       STOP     RUN.
 INIT-1.
     OPEN    OUTPUT    JUNJYRW2.
*    IF OUT-ST   NOT  =    ZERO
*       DISPLAY "JUNJYRW1 OUTPUT ERR"  UPON CONS
*       STOP     RUN.
*
 INIT-EXT.
     EXIT.
*
*----------------------------------------------------------------*
*       LEVEL     1    ﾒｲﾝ   ｼｮﾘ                                 *
*----------------------------------------------------------------*
*
 MAIN-SEC   SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
     IF  END-FLG = "E"
                  GO             TO   MAIN-EXT
     END-IF.
     READ         JUNJYRW1       AT   END
                  GO             TO   MAIN-EXT.
     ADD          1              TO   RD-CNT.
*
     IF  IN-REC(7:2) =  00
         MOVE  IN-REC   TO   WK-HEAD
*        MOVE  ","      TO   WK-HEAD(108:1)
*        MOVE  """      TO   WK-HEAD(109:1)
*        MOVE  """      TO   WK-HEAD(127:1)
*        MOVE  ","      TO   WK-HEAD(128:1)
         MOVE  LINK-01  TO   WK-HEAD(109:20)
         GO             TO   MAIN-SEC
     END-IF.
*
     MOVE         WK-HEAD  TO   OUT-REC.
     MOVE         IN-REC   TO   OUT-REC(129:128).
*
     WRITE        OUT-REC.
     ADD          1        TO   WT-CNT.
*    IF           OUT-ST   NOT =    "00"
*                 DISPLAY "JUNJYRW2  OVER   "  OUT-ST
*                 UPON  CONS
*                 STOP RUN
*    END-IF.
     GO TO   MAIN-SEC.
 MAIN-EXT.
     EXIT.
*
*----------------------------------------------------------------*
*       LEVEL     1    ｴﾝﾄﾞ  ｼｮﾘ                                 *
*----------------------------------------------------------------*
*
 END-SEC  SECTION.
     MOVE    "END-SEC"          TO   S-NAME.
     CLOSE   JUNJYRW1.
     CLOSE   JUNJYRW2.
     MOVE    RD-CNT    TO      IN-CNT.
     MOVE    WT-CNT    TO      OUT-CNT.
     DISPLAY MSG-IN    UPON CONS.
     DISPLAY MSG-OUT   UPON CONS.
     DISPLAY MSG-END   UPON CONS.
**
 END-EXT.
     EXIT.
 END PROGRAM SJR0140B.

```
