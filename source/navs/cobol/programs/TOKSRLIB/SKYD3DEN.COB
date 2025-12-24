****************************************************************
*    顧客名　　　　　：　_サカタのタネ　営業第２部向け　　　　*
*    業務名　　　　　：　システム共通サブシステム　　　　　　　*
*    モジュール名　　：　Ｄ３６５伝票番号採番                 *
*    作成日／更新日　：　2020/04/15                            *
*    作成者／更新者　：　NAV-ASSIST                            *
*    モジュール名　　：　Ｄ３６５伝票番号採番                 *
*    作成日／更新日　：　2020/06/30                            *
*    作成者／更新者　：　NAV-ASSIST T.TAKAHASHI                *
*    　　　　　　　　　　連携課題対応の為（Ｄ３６５側要求）    *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKYD3DEN.
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
 01  WK-RENBAN               PIC  9(08).
 01  JYO-ST                  PIC  X(02).
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
*#2020/06/30 NAV ST 連携課題対応
*D365伝票番号編集（連携課題対応後）
 01  WK-D3652-DEN.
     03  WK-D3652-TYPE       PIC  X(01).
     03  WK-D3652-DATE       PIC  9(08).
     03  WK-D3652-RENBAN     PIC  9(06).
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
         05  ST-PG           PIC  X(08) VALUE "SKYD3DEN".
         05  FILLER          PIC  X(11)
                                  VALUE " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  END-PG          PIC  X(08) VALUE "SKYD3DEN".
         05  FILLER          PIC  X(11)
                                  VALUE "  END  *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  END-PG          PIC  X(08) VALUE "SKYD3DEN".
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
 01  IN-DTTYPE               PIC  X(01).
 01  IN-TOKCD                PIC  9(08).
 01  OUT-D365-DEN            PIC  X(20).
 01  OUT-D365-ERR            PIC  X(01).
*
******************************************************************
 PROCEDURE    DIVISION       USING     IN-DTTYPE   IN-TOKCD
                                       OUT-D365-DEN OUT-D365-ERR.
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
*伝票番号(OUT)初期化
     OPEN     I-O       HJYOKEN.
     INITIALIZE         WK-D365-KEY  WK-D365-DEN.
*#2020/06/30 NAV ST 連携課題対応
     INITIALIZE         WK-D3652-DEN.
*#2020/06/30 NAV ED 連携課題対応
*ＫＥＹ２編集
     MOVE     "D365NO"    TO   WK-D365-KEY1.
     MOVE     IN-DTTYPE   TO   WK-D365-KEY2.
*条件ファイル読込
     PERFORM  JYO-READ.
     IF       OUT-D365-ERR  NOT =  SPACE
              DISPLAY "# D365" NC"伝票" "NO" NC"採番異常！"
                      " #" UPON CONS
              GO          TO   000-PROG-CNTL-EXIT
     END-IF.
*採番
     PERFORM  SAIBAN.
     CLOSE              HJYOKEN.
*表示
*****DISPLAY "D365" NC"伝票" "NO = " OUT-D365-DEN UPON CONS.
 000-PROG-CNTL-EXIT.
     EXIT     PROGRAM.
*--------------------------------------------------------------*
*    LEVEL   1.1   取引先マスタ読込　　　                      *
*--------------------------------------------------------------*
 JYO-READ         SECTION.
*
     MOVE     "JYO-READ"     TO     SEC-NAME.
*
     MOVE     60             TO     JYO-F01.
     MOVE     WK-D365-KEY    TO     JYO-F02.
*****DISPLAY "KEY = " JYO-F01 " : " JYO-F02  UPON CONS.
     READ     HJYOKEN
         INVALID
              MOVE   "1"     TO     OUT-D365-ERR
         NOT INVALID
              MOVE   SPACE   TO     OUT-D365-ERR
              MOVE   JYO-F04 TO     WK-RENBAN
     END-READ.
  TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   採番　　　                      *
*--------------------------------------------------------------*
 SAIBAN           SECTION.
     MOVE     "SAIBAN"       TO     SEC-NAME.
*Ｄ３６５伝票編集
     ADD      1              TO     WK-RENBAN.
*最大値以上の場合、最小値をセット
     IF       WK-RENBAN  >  JYO-F06
              MOVE  JYO-F05  TO     WK-RENBAN
     END-IF.
*#2020/06/30 NAV ST 連携課題対応
*Ｄ３６５伝票番号作成
*****MOVE     IN-DTTYPE      TO     WK-D365-TYPE.
*    MOVE     IN-TOKCD       TO     WK-D365-TOKCD.
*    MOVE     JYO-F07        TO     WK-D365-SYSDT.
*    MOVE     WK-D365-SYSDT2 TO     WK-D365-DATE.
*    MOVE     WK-RENBAN      TO     WK-D365-RENBAN.
**** MOVE     WK-D365-DEN    TO     OUT-D365-DEN.
     MOVE     JYO-F07        TO     WK-D365-SYSDT.
     MOVE     IN-DTTYPE      TO     WK-D3652-TYPE.
     MOVE     WK-D365-SYSDT2 TO     WK-D3652-DATE.
     MOVE     WK-RENBAN      TO     WK-D3652-RENBAN.
     MOVE     WK-D3652-DEN   TO     OUT-D365-DEN.
*#2020/06/30 NAV ED 連携課題対応
*採番値を条件Ｆにセット
     MOVE     WK-RENBAN      TO     JYO-F04.
*条件Ｆ更新　
     REWRITE                        JYO-REC.
  SAIBAN-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
