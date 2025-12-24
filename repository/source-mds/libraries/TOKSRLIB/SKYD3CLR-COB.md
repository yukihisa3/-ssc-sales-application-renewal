# SKYD3CLR

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SKYD3CLR.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　：　_サカタのタネ　営業第２部向け　　　　*
*    業務名　　　　　：　システム共通サブシステム　　　　　　　*
*    モジュール名　　：　Ｄ３６５伝票番号初期化                *
*    作成日／更新日　：　2020/04/15                            *
*    作成者／更新者　：　NAV-ASSIST                            *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKYD3CLR.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2020/04/15.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO   DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS  MODE   IS RANDOM
                        RECORD  KEY    IS JYO-F01  JYO-F02
                        FILE    STATUS IS JYO-ST.
***************************************************************
*INPUT-OUTPUT           SECTION.
******************************************************************
 DATA                      DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*--------------------------------------------------------------*
 FD    HJYOKEN     LABEL      RECORD    IS    STANDARD.
       COPY        HJYOKEN    OF    XFDLIB
                   JOINING    JYO   PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  WK-RENBAN               PIC  9(08)  VALUE  ZERO.
 01  WK-CRTCNT               PIC  9(05)  VALUE  ZERO.
 01  WK-REWCNT               PIC  9(05)  VALUE  ZERO.
 01  WK-SKPCNT               PIC  9(05)  VALUE  ZERO.
 01  JYO-ST                  PIC  X(02).
 01  HJYOKEN-INV-FLG         PIC  X(03)  VALUE  SPACE.
*D365伝票番号索引キー
 01  WK-D365-KEY.
     03  WK-D365-KEY1        PIC  X(06).
     03  WK-D365-KEY2        PIC  X(01).
*D365伝票番号編集
 01  WK-D365-DEN.
     03  WK-D365-TYPE        PIC  X(01).
     03  WK-D365-TOKCD       PIC  9(08).
     03  WK-D365-DATE        PIC  9(06).
     03  WK-D365-RENBAN      PIC  9(05).
*システム日付編集
 01  WK-D365-SYSDT           PIC  9(09)V9(02).
 01  FILLER                  REDEFINES  WK-D365-SYSDT.
     03  WK-D365-SYSDT1      PIC  9(01).
     03  WK-D365-SYSDT2      PIC  9(08).
     03  WK-D365-SYSDT3      PIC  9(02).
*ＭＳＧエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  ST-PG           PIC  X(08) VALUE "SKYD3CLR".
         05  FILLER          PIC  X(11)
                                  VALUE " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  END-PG          PIC  X(08) VALUE "SKYD3CLR".
         05  FILLER          PIC  X(11)
                                  VALUE "  END  *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  END-PG          PIC  X(08) VALUE "SKYD3CLR".
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
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC  9(06)  VALUE  ZERO.
     03  SYS-DATEW           PIC  9(08)  VALUE  ZERO.
     03  SYS-DATE-R          REDEFINES SYS-DATEW.
         05  SYS-YY          PIC  9(04).
         05  SYS-MM          PIC  9(02).
         05  SYS-DD          PIC  9(02).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                   SECTION.
 01  IN-DTTYPE              PIC   X(01).
*
******************************************************************
 PROCEDURE    DIVISION   USING   IN-DTTYPE.
******************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE      AFTER        EXCEPTION
                           PROCEDURE     HJYOKEN.
     MOVE     "JYOKEN1"    TO     AB-FILE.
     MOVE     JYO-ST       TO     AB-STS.
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
*
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
*
*伝票番号(OUT)初期化
     OPEN     I-O       HJYOKEN.
*条件ファイルスタート
     MOVE     "D365NO"    TO   WK-D365-KEY1.
     MOVE     IN-DTTYPE   TO   WK-D365-KEY2.
*条件ファイル読込
     PERFORM  JYO-READ.
     IF  HJYOKEN-INV-FLG  =  "INV"
         MOVE   SPACE        TO     JYO-REC
         INITIALIZE                 JYO-REC
         MOVE   60           TO     JYO-F01
         MOVE   WK-D365-KEY  TO     JYO-F02
         EVALUATE  IN-DTTYPE
             WHEN  "E"
             MOVE  NC"Ｄ３６５伝票番号（ＯＮＬ）" TO JYO-F03
             WHEN  "T"
             MOVE  NC"Ｄ３６５伝票番号（手書き）" TO JYO-F03
             WHEN  "H"
             MOVE  NC"Ｄ３６５伝票番号（返品）　" TO JYO-F03
             WHEN  "N"
             MOVE  NC"Ｄ３６５伝票番号（値引き）" TO JYO-F03
             WHEN  "K"
             MOVE  NC"Ｄ３６５伝票番号（廃棄　）" TO JYO-F03
             WHEN  "W"
             MOVE  NC"Ｄ３６５伝票番号（その他）" TO JYO-F03
             WHEN  OTHER
             DISPLAY NC"＃Ｄ３６５伝票番号初期化異常！！＃＃"
                     UPON  CONS
             MOVE  4000      TO     PROGRAM-STATUS
             STOP  RUN
         END-EVALUATE
         MOVE   1            TO     JYO-F04  JYO-F05
         MOVE   99999        TO     JYO-F06
         MOVE   SYS-DATEW    TO     JYO-F07
         WRITE  JYO-REC
         ADD    1            TO     WK-CRTCNT
     ELSE
         MOVE   JYO-F07      TO     WK-D365-SYSDT
         IF     SYS-DATEW  NOT =  WK-D365-SYSDT2
                MOVE   1     TO     JYO-F04
                MOVE SYS-DATEW TO   JYO-F07
                REWRITE  JYO-REC
                ADD    1     TO     WK-REWCNT
         ELSE
                ADD    1     TO     WK-SKPCNT
         END-IF
     END-IF.
*
     CLOSE              HJYOKEN.
*表示
     DISPLAY "## CRT-CNT = " WK-CRTCNT " ##" UPON CONS.
     DISPLAY "## REW-CNT = " WK-REWCNT " ##" UPON CONS.
     DISPLAY "## SKP-CNT = " WK-SKPCNT " ##" UPON CONS.
 000-PROG-CNTL-EXIT.
     EXIT     PROGRAM.
*--------------------------------------------------------------*
*    LEVEL   1.1   条件ファイル読込　　　                      *
*--------------------------------------------------------------*
 JYO-READ         SECTION.
*
     MOVE     "JYO-READ"     TO     SEC-NAME.
*
     MOVE     60             TO     JYO-F01.
     MOVE     WK-D365-KEY    TO     JYO-F02.
     DISPLAY "KEY = " JYO-F01 " : " JYO-F02  UPON CONS.
     READ     HJYOKEN
         INVALID
              MOVE   "INV"   TO     HJYOKEN-INV-FLG
         NOT INVALID
              MOVE   SPACE   TO     HJYOKEN-INV-FLG
     END-READ.
  TOK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
