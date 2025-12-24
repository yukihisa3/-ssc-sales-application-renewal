# SKYSBCKB

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SKYSBCKB.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　：　_サカタのタネ　特販部　殿向け　　　　*
*    業務名　　　　　：　システム共通サブシステム　　　　　　　*
*    モジュール名　　：　伝票番号自動採番                     *
*    作成日／更新日　：　2005/09/15                            *
*    作成者／更新者　：　NAV-ASSIST                            *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKYSBCKB.
 AUTHOR.                NAV.
 DATE-WRITTEN.          05/09/15.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO   DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS  MODE   IS RANDOM
                        RECORD  KEY    IS TOK-F01
                        FILE    STATUS IS TOK-ST.
***************************************************************
*INPUT-OUTPUT           SECTION.
******************************************************************
 DATA                      DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*--------------------------------------------------------------*
 FD    TOKMS2      LABEL      RECORD    IS    STANDARD.
       COPY        HTOKMS     OF    XFDLIB
                   JOINING    TOK   PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  WK-DENNO                PIC  9(08).
 01  TOK-ST                  PIC  X(02).
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  ST-PG           PIC  X(08) VALUE "SKYSBCKB".
         05  FILLER          PIC  X(11)
                                  VALUE " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  END-PG          PIC  X(08) VALUE "SKYSBCKB".
         05  FILLER          PIC  X(11)
                                  VALUE "  END  *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  END-PG          PIC  X(08) VALUE "SKYSBCKB".
         05  FILLER          PIC  X(11)
                                  VALUE " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  AB-FILE         PIC  X(08).
         05  FILLER          PIC  X(06) VALUE " ST = ".
         05  AB-STS          PIC  X(02).
         05  FILLER          PIC  X(05) VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  FILLER          PIC  X(07) VALUE " SEC = ".
         05  S-NAME          PIC  X(30).
*--------------------------------------------------------------*
 LINKAGE                SECTION.
*--------------------------------------------------------------*
 01  IN-TORCD                PIC  9(08).
 01  IN-DENNO                PIC  9(09).
 01  OUT-DENNO               PIC  9(09).
*
******************************************************************
 PROCEDURE    DIVISION       USING     IN-TORCD IN-DENNO
                                       OUT-DENNO.
******************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
    USE      AFTER        EXCEPTION
                          PROCEDURE     TOKMS2.
    MOVE     "TOKMS2"     TO     AB-FILE.
    MOVE     TOK-ST       TO     AB-STS.
    DISPLAY  MSG-ABEND           UPON CONS.
    DISPLAY  SEC-NAME            UPON CONS.
    DISPLAY  ABEND-FILE          UPON CONS.
    MOVE     4000         TO     PROGRAM-STATUS.
    STOP     RUN.
 END      DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE     "PROG-CNTL" TO   SEC-NAME.
*伝票番号(OUT)初期化
     OPEN     I-O       TOKMS2.
     MOVE     ZERO      TO   IN-DENNO.
     MOVE     ZERO      TO   OUT-DENNO.
*取引先マスタ読込
     PERFORM  TOK-READ.
*採番
     PERFORM  SAIBAN.
     CLOSE              TOKMS2.
     DISPLAY "伝票番号(IN) " IN-DENNO  UPON CONS.
     DISPLAY "伝票番号(OUT)" OUT-DENNO UPON CONS.
 000-PROG-CNTL-EXIT.
     EXIT     PROGRAM.
*--------------------------------------------------------------*
*    LEVEL   1.1   取引先マスタ読込　　　                      *
*--------------------------------------------------------------*
 TOK-READ         SECTION.
     MOVE     "TOK-READ"     TO     SEC-NAME.
     MOVE     SPACE          TO     TOK-REC.
     INITIALIZE                     TOK-REC.
     MOVE     IN-TORCD       TO     TOK-F01.
     READ     TOKMS2
         INVALID
              MOVE   SPACE   TO     TOK-REC
              INITIALIZE            TOK-REC
         NOT INVALID
              MOVE   TOK-F54 TO     IN-DENNO
     END-READ.
  TOK-READ-EXIT.
 EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   採番　　　                      *
*--------------------------------------------------------------*
 SAIBAN           SECTION.
     MOVE     "SAIBAN"       TO     SEC-NAME.
     MOVE     IN-DENNO       TO     WK-DENNO.
     ADD      1              TO     WK-DENNO.
     MOVE     WK-DENNO       TO     OUT-DENNO.
     MOVE     OUT-DENNO      TO     TOK-F54.
     REWRITE                        TOK-REC.
  SAIBAN-EXIT.
 EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
