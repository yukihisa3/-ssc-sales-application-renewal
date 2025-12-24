# SER0010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SER0010B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　：　_サカタのタネ　営業第２部向け　　　　*
*    業務名　　　　　：　システム共通サブシステム　　　　　　　*
*    モジュール名　　：　バッチ_採番更新（Ｄ３６５）          *
*    作成日／作成者　：　2021/11/16 INOUE                      *
*    処理概要　　　　：　管理番号・バッチ_を採番　　　　　　　*
*    更新日／更新者　：　                                      *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SER0010B.
*                  流用:SKYD3DEN.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/11/16.
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
*Ｄ３６５エラー状況ファイル
     SELECT   ERRJYOL1  ASSIGN    TO   DA-01-VI-ERRJYOL1
                        ORGANIZATION   INDEXED
                        ACCESS  MODE   IS DYNAMIC
                        RECORD  KEY    IS ERJ-F01
                        FILE    STATUS IS ERJ-ST.
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
 FD    ERRJYOL1    LABEL      RECORD    IS    STANDARD.
       COPY        ERRJYOL1   OF    XFDLIB
                   JOINING    ERJ   PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  WK-RENBAN               PIC  9(08).
 01  JYO-ST                  PIC  X(02).
 01  ERJ-ST                  PIC  X(02).
 01  JYO-INV-FLG             PIC  X(01).
 01  ERJ-INV-FLG             PIC  X(01).
*条件ファイル索引キー
 01  WK-JYO-KEY.
     03  WK-JYO-KEY1         PIC  X(06).
*D365伝票番号編集
 01  WK-D365-DEN.
     03  WK-D365-TYPE        PIC  X(01).
     03  WK-D365-TOKCD       PIC  9(08).
     03  WK-D365-DATE        PIC  9(06).
     03  WK-D365-RENBAN      PIC  9(05).
*D365伝票番号編集（連携課題対応後）
 01  WK-D3652-DEN.
     03  WK-D3652-TYPE       PIC  X(01).
     03  WK-D3652-DATE       PIC  9(08).
     03  WK-D3652-RENBAN     PIC  9(06).
*システム日付編集
*01  WK-D365-SYSDT           PIC  9(09)V9(02).
*01  FILLER                  REDEFINES  WK-D365-SYSDT.
*    03  WK-D365-SYSDT1      PIC  9(01).
*    03  WK-D365-SYSDT2      PIC  9(08).
*    03  WK-D365-SYSDT3      PIC  9(02).
     03  SYS-DATE            PIC  9(06).
     03  SYS-DATEW           PIC  9(08).
*時刻
 01  TIME-AREA.
     03  WK-TIME             PIC  9(08)  VALUE  ZERO.
*
*ＭＳＧエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  ST-PG           PIC  X(08) VALUE "SER0010B".
         05  FILLER          PIC  X(11)
                                  VALUE " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  END-PG          PIC  X(08) VALUE "SER0010B".
         05  FILLER          PIC  X(11)
                                  VALUE "  END  *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  END-PG          PIC  X(08) VALUE "SER0010B".
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
*
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC  X(01).
     03  LINK-IN-YMD6        PIC  9(06).
     03  LINK-IN-YMD8        PIC  9(08).
     03  LINK-OUT-RET        PIC  X(01).
     03  LINK-OUT-YMD8       PIC  9(08).
*
*--------------------------------------------------------------*
 LINKAGE                SECTION.
*--------------------------------------------------------------*
*01  IN-DTTYPE               PIC  X(01).
*01  IN-TOKCD                PIC  9(08).
*01  OUT-D365-DEN            PIC  X(20).
*01  OUT-D365-ERR            PIC  X(01).
 01  LINK-IN-BUMCD           PIC  X(04).
 01  LINK-IN-TANCD           PIC  X(02).
 01  LINK-OUT-KANRINO        PIC  9(08).
 01  LINK-OUT-BDATE          PIC  9(08).
 01  LINK-OUT-BTIME          PIC  9(06).
*
******************************************************************
*PROCEDURE    DIVISION       USING     IN-DTTYPE   IN-TOKCD
*                                      OUT-D365-DEN OUT-D365-ERR.
 PROCEDURE    DIVISION       USING
                                       LINK-IN-BUMCD
                                       LINK-IN-TANCD
                                       LINK-OUT-KANRINO
                                       LINK-OUT-BDATE
                                       LINK-OUT-BTIME.
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
 FILEERR-SEC2              SECTION.
     USE      AFTER        EXCEPTION
                           PROCEDURE     ERRJYOL1.
     MOVE     "ERRJYOL1"   TO     AB-FILE.
     MOVE     ERJ-ST       TO     AB-STS.
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
     OPEN     I-O       HJYOKEN  ERRJYOL1.
     INITIALIZE         WK-JYO-KEY  WK-D365-DEN.
     INITIALIZE         WK-D3652-DEN.
*
*システム日付編集*
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*システム時間取得
     ACCEPT    WK-TIME          FROM TIME.
*ＫＥＹ編集
     MOVE     "ERRNO "          TO   WK-JYO-KEY1.
*条件ファイル読込
     PERFORM  JYO-READ.
     IF       JYO-INV-FLG NOT =  SPACE
              DISPLAY "# 条件ファイルなし #" UPON CONS
              DISPLAY "#  KEY1=27         #" UPON CONS
              DISPLAY "#  KEY2=ERRNO      #" UPON CONS
              DISPLAY "# 作成します       #" UPON CONS
     END-IF.
*採番
     PERFORM  SAIBAN.
     CLOSE              HJYOKEN  ERRJYOL1.
*表示
     IF       PROGRAM-STATUS   =   4001
              GO               TO      000-PROG-CNTL-EXIT
     END-IF.
     MOVE     WK-RENBAN        TO      LINK-OUT-KANRINO.
     MOVE     SYS-DATEW        TO      LINK-OUT-BDATE.
     MOVE     WK-TIME(1:6)     TO      LINK-OUT-BTIME.
     DISPLAY "管理番号" " = " LINK-OUT-KANRINO UPON CONS.
     DISPLAY "バッチ日" " = " LINK-OUT-BDATE   UPON CONS.
     DISPLAY "バッチ時" " = " LINK-OUT-BTIME   UPON CONS.
*
 000-PROG-CNTL-EXIT.
     EXIT     PROGRAM.
*--------------------------------------------------------------*
*    LEVEL   1.1   条件ファイル読込　　　                      *
*--------------------------------------------------------------*
 JYO-READ         SECTION.
*
     MOVE     "JYO-READ"     TO     SEC-NAME.
*
     MOVE     27             TO     JYO-F01.
     MOVE     WK-JYO-KEY     TO     JYO-F02.
*****DISPLAY "KEY = " JYO-F01 " : " JYO-F02  UPON CONS.
     READ     HJYOKEN
         INVALID
              MOVE   "1"     TO     JYO-INV-FLG
         NOT INVALID
              MOVE   SPACE   TO     JYO-INV-FLG
              MOVE   JYO-F04 TO     WK-RENBAN
     END-READ.
 JYO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   採番　　　                      *
*--------------------------------------------------------------*
 SAIBAN           SECTION.
     MOVE     "SAIBAN"       TO     SEC-NAME.
*
 SAIBAN-01.
*管理番号編集
     IF       JYO-INV-FLG = SPACE
              IF   WK-RENBAN  <  JYO-F06
                   ADD    1         TO     WK-RENBAN
              ELSE
                   MOVE   JYO-F05   TO     WK-RENBAN
              END-IF
     ELSE
              MOVE     SPACE        TO     JYO-REC
              INITIALIZE                   JYO-REC
              MOVE     27           TO     JYO-F01
              MOVE     WK-JYO-KEY1  TO     JYO-F02
              MOVE     NC"Ｄ３６５連携エラー管理番号" TO JYO-F03
              MOVE     1            TO     WK-RENBAN
              MOVE     1            TO     JYO-F05
              MOVE     99999999     TO     JYO-F06
     END-IF.
*
 SAIBAN-02.
*最大値以上の場合、最小値をセット
     IF       WK-RENBAN  >  JYO-F06
              MOVE  JYO-F05  TO     WK-RENBAN
     END-IF.
*
 SAIBAN-03.
*エラー状況ファイル作成
*ファイル検索
     PERFORM  ERJ-READ.
     IF       ERJ-INV-FLG NOT =  SPACE
              DISPLAY "# 状況ファイル　　 #" UPON CONS
              DISPLAY "#  KEY=" WK-RENBAN    "#" UPON CONS
              DISPLAY "# 作成します       #" UPON CONS
              MOVE     SPACE          TO     ERJ-REC
              INITIALIZE                     ERJ-REC
              MOVE     WK-RENBAN      TO     ERJ-F01
              MOVE     SYS-DATEW      TO     ERJ-F02
              MOVE     WK-TIME(1:6)   TO     ERJ-F03
              MOVE     LINK-IN-BUMCD  TO     ERJ-F04
              MOVE     LINK-IN-TANCD  TO     ERJ-F05
              WRITE                          ERJ-REC
     ELSE
              IF       WK-RENBAN      =      JYO-F06
                       DISPLAY
                        "# 連番が上限に達しています！ #"
                       UPON CONS
                       MOVE   4001    TO     PROGRAM-STATUS
                       GO             TO     SAIBAN-EXIT
              END-IF
              ADD      1              TO     WK-RENBAN
              GO                      TO     SAIBAN-03
     END-IF.
*
 SAIBAN-04.
*条件ファイル　更新OR作成
     MOVE     WK-RENBAN      TO     JYO-F04.
     IF       JYO-INV-FLG = SPACE
              REWRITE               JYO-REC
     ELSE
              WRITE                 JYO-REC
     END-IF.
*
  SAIBAN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   エラー状況ファイル検索                      *
*--------------------------------------------------------------*
 ERJ-READ         SECTION.
*
     MOVE     "ERJ-READ"     TO     SEC-NAME.
*
     MOVE     WK-RENBAN      TO     ERJ-F01.
*****DISPLAY "KEY = " ERJ-F01       UPON CONS.
     READ     ERRJYOL1
         INVALID
              MOVE   "1"     TO     ERJ-INV-FLG
         NOT INVALID
              MOVE   SPACE   TO     ERJ-INV-FLG
     END-READ.
*
 ERJ-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
