# SER0030B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SER0030B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　：　_サカタのタネ　営業第２部向け　　　　*
*    業務名　　　　　：　システム共通サブシステム　　　　　　　*
*    モジュール名　　：　エラーデータ累積　　　　　　　        *
*    作成日／作成者　：　2021/11/27 INOUE                      *
*    処理概要　　　　：　Ｄ３６５エラー取込ファイルを　　　　　*
*    　　　　　　　　　　エラー累積ファイルに追加出力　　　　　*
*    更新日／更新者　：　                                      *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SER0030B.
*                  流用:SER0010B.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/11/27.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*Ｄ３６５エラー取込ファイル
     SELECT  ERR365F    ASSIGN    TO   DA-01-S-ERR365F
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE   IS SEQUENTIAL
                        FILE      STATUS IS ERT-STATUS.
*Ｄ３６５累積ファイル
     SELECT  ERRRUIL1   ASSIGN    TO   DA-01-VI-ERRRUIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE   IS DYNAMIC
                        RECORD    KEY    IS ERR-F01
                        FILE      STATUS IS ERR-STATUS.
*Ｄ３６５エラー状況ファイル
     SELECT   ERRJYOL1  ASSIGN    TO   DA-01-VI-ERRJYOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE   IS RANDOM
                        RECORD    KEY    IS ERJ-F01
                        FILE      STATUS IS ERJ-STATUS.
***************************************************************
*INPUT-OUTPUT           SECTION.
******************************************************************
 DATA                      DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*--------------------------------------------------------------*
 FD    ERR365F     LABEL      RECORD    IS    STANDARD
                   BLOCK      CONTAINS  40    RECORDS.
       COPY        ERR365F    OF    XFDLIB
                   JOINING    ERT   PREFIX.
 FD    ERRRUIL1    LABEL      RECORD    IS    STANDARD.
       COPY        ERRRUIL1   OF    XFDLIB
                   JOINING    ERR   PREFIX.
 FD    ERRJYOL1    LABEL      RECORD    IS    STANDARD.
       COPY        ERRJYOL1   OF    XFDLIB
                   JOINING    ERJ   PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  WK-RENBAN               PIC  9(08).
 01  ERT-STATUS              PIC  X(02).
 01  ERR-STATUS              PIC  X(02).
 01  ERJ-STATUS              PIC  X(02).
 01  ERT-END-FLG             PIC  X(01).
 01  ERJ-INV-FLG             PIC  X(01).
*件数
 01  RD-CNT                  PIC  9(07).
 01  WT-CNT                  PIC  9(07).
 01  ER01-CNT                PIC  9(07).
 01  ER02-CNT                PIC  9(07).
 01  ER03-CNT                PIC  9(07).
 01  ER04-CNT                PIC  9(07).
 01  ER05-CNT                PIC  9(07).
*条件ファイル索引キー
*01  WK-ERT-KEY.
*    03  WK-ERT-KEY1         PIC  X(06).
*D365伝票番号編集
*01  WK-D365-DEN.
*    03  WK-D365-TYPE        PIC  X(01).
*    03  WK-D365-TOKCD       PIC  9(08).
*    03  WK-D365-DATE        PIC  9(06).
*    03  WK-D365-RENBAN      PIC  9(05).
*D365伝票番号編集（連携課題対応後）
*01  WK-D3652-DEN.
*    03  WK-D3652-TYPE       PIC  X(01).
*    03  WK-D3652-DATE       PIC  9(08).
*    03  WK-D3652-RENBAN     PIC  9(06).
*システム日付編集
*01  WK-D365-SYSDT           PIC  9(09)V9(02).
*01  FILLER                  REDEFINES  WK-D365-SYSDT.
*    03  WK-D365-SYSDT1      PIC  9(01).
*    03  WK-D365-SYSDT2      PIC  9(08).
*    03  WK-D365-SYSDT3      PIC  9(02).
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
 01  SYS-DATEW               PIC  9(08)  VALUE  ZERO.
*時刻
 01  TIME-AREA.
     03  WK-TIME             PIC  9(08)  VALUE  ZERO.
*
*ＭＳＧエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  ST-PG           PIC  X(08) VALUE "SER0030B".
         05  FILLER          PIC  X(11)
                                  VALUE " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  END-PG          PIC  X(08) VALUE "SER0030B".
         05  FILLER          PIC  X(11)
                                  VALUE "  END  *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  END-PG          PIC  X(08) VALUE "SER0030B".
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
 01  LINK-IN-BUMCD           PIC  X(04).
 01  LINK-IN-TANCD           PIC  X(02).
 01  LINK-IN-KANRINO         PIC  9(08).
 01  LINK-IN-BDATE           PIC  9(08).
 01  LINK-IN-BTIME           PIC  9(06).
*
******************************************************************
 PROCEDURE    DIVISION       USING
                                       LINK-IN-BUMCD
                                       LINK-IN-TANCD
                                       LINK-IN-KANRINO
                                       LINK-IN-BDATE
                                       LINK-IN-BTIME.
******************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE      AFTER        EXCEPTION
                           PROCEDURE     ERR365F.
     MOVE     "ERR365F"    TO     AB-FILE.
     MOVE     ERT-STATUS   TO     AB-STS.
     DISPLAY  MSG-ABEND           UPON CONS.
     DISPLAY  SEC-NAME            UPON CONS.
     DISPLAY  ABEND-FILE          UPON CONS.
     MOVE     4000         TO     PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2              SECTION.
     USE      AFTER        EXCEPTION
                           PROCEDURE     ERRRUIL1.
     MOVE     "ERRRUIL1"   TO     AB-FILE.
     MOVE     ERR-STATUS   TO     AB-STS.
     DISPLAY  MSG-ABEND           UPON CONS.
     DISPLAY  SEC-NAME            UPON CONS.
     DISPLAY  ABEND-FILE          UPON CONS.
     MOVE     4000         TO     PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC3              SECTION.
     USE      AFTER        EXCEPTION
                           PROCEDURE     ERRJYOL1.
     MOVE     "ERRJYOL1"   TO     AB-FILE.
     MOVE     ERJ-STATUS   TO     AB-STS.
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
 000-PROG-01.
     OPEN     INPUT     ERR365F.
     OPEN     I-O       ERRRUIL1  ERRJYOL1.
     MOVE     ZERO      TO        ER01-CNT
                                  ER02-CNT
                                  ER03-CNT
                                  ER04-CNT
                                  ER05-CNT
                                  RD-CNT
                                  WT-CNT.
*    INITIALIZE         WK-ERT-KEY  WK-D365-DEN.
*    INITIALIZE         WK-D3652-DEN.
*
*システム日付編集*
 000-PROG-02.
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
 000-PROG-03.
     ACCEPT    WK-TIME          FROM TIME.
*ＫＥＹ編集
*    MOVE     "ERRNO "          TO   WK-ERT-KEY1.
 000-PROG-04.
*エラー状況ファイル検索
     PERFORM  ERJ-READ.
     IF       ERJ-INV-FLG NOT =  SPACE
              DISPLAY "# エラー状況ファイルなし #" UPON CONS
              MOVE   4001    TO     PROGRAM-STATUS
              GO             TO     000-PROG-08
     END-IF.
*
*取込ファイル読込
 000-PROG-05.
     PERFORM  ERT-READ.
     IF       ERT-END-FLG NOT =  SPACE
              DISPLAY "# 取込ファイルなし #" UPON CONS
              MOVE    4001      TO   PROGRAM-STATUS
              GO                TO   000-PROG-08
     END-IF.
*累積
 000-PROG-06.
     PERFORM  RUISEKI.
*エラー状況ファイル更新
     MOVE     ER01-CNT      TO     ERJ-F061.
     MOVE     ER02-CNT      TO     ERJ-F071.
     MOVE     ER03-CNT      TO     ERJ-F081.
     MOVE     ER04-CNT      TO     ERJ-F091.
     MOVE     ER05-CNT      TO     ERJ-F101.
     REWRITE  ERJ-REC.
*
 000-PROG-07.
     DISPLAY "管理番号" " = " LINK-IN-KANRINO UPON CONS.
     DISPLAY "バッチ日" " = " LINK-IN-BDATE   UPON CONS.
     DISPLAY "バッチ時" " = " LINK-IN-BTIME   UPON CONS.
     DISPLAY "取込件数" " = " RD-CNT          UPON CONS.
     DISPLAY "累積件数" " = " WT-CNT          UPON CONS.
*表示
 000-PROG-08.
     CLOSE    ERR365F ERRRUIL1 ERRJYOL1.
*
 000-PROG-09.
     STOP RUN.
*
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   取込ファイル読込　　　                      *
*--------------------------------------------------------------*
 ERT-READ         SECTION.
*
     MOVE     "ERT-READ"     TO     SEC-NAME.
*
     READ     ERR365F
         AT END
              MOVE   "1"     TO     ERT-END-FLG
         NOT AT END
              MOVE   SPACE   TO     ERT-END-FLG
              ADD    1       TO     RD-CNT
     END-READ.
 ERT-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   累積（メイン）処理                          *
*--------------------------------------------------------------*
 RUISEKI           SECTION.
     MOVE     "RUISEKI"       TO     SEC-NAME.
*
 RUISEKI-01.
*エラー区分カウント
     EVALUATE ERT-F03
        WHEN  "01"
              ADD    1         TO     ER01-CNT
        WHEN  "02"
              ADD    1         TO     ER02-CNT
        WHEN  "03"
              ADD    1         TO     ER03-CNT
        WHEN  "04"
              ADD    1         TO     ER04-CNT
        WHEN  "05"
              ADD    1         TO     ER05-CNT
        WHEN  OTHER
              DISPLAY "# 想定外エラー区分 #"    UPON CONS
              DISPLAY "# スキップします　 #"    UPON CONS
              DISPLAY "  エラー区分= " ERT-F03  UPON CONS
              DISPLAY "  管理番号　= " LINK-IN-KANRINO UPON CONS
              GO               TO     RUISEKI-04
     END-EVALUATE.
*
 RUISEKI-02.
*D365エラー累積ファイル出力
     MOVE     SPACE            TO     ERR-REC.
     INITIALIZE                       ERR-REC.
     MOVE     ERT-REC          TO     ERR-F04.
     MOVE     LINK-IN-KANRINO  TO     ERR-F01.
     MOVE     LINK-IN-BDATE    TO     ERR-F02.
     MOVE     LINK-IN-BTIME    TO     ERR-F03.
     WRITE    ERR-REC.
     ADD      1             TO     WT-CNT.
*
*RUISEKI-03.
*エラー状況ファイル更新
*    MOVE     ER01-CNT      TO     ERJ-F061.
*    MOVE     ER02-CNT      TO     ERJ-F071.
*    MOVE     ER03-CNT      TO     ERJ-F081.
*    MOVE     ER04-CNT      TO     ERJ-F091.
*    MOVE     ER05-CNT      TO     ERJ-F101.
*    REWRITE  ERJ-REC.
*
 RUISEKI-04.
*取込ファイル読込
     PERFORM  ERT-READ.
     IF       ERT-END-FLG     =  SPACE
              GO                TO   RUISEKI-01
     ELSE
              GO                TO   RUISEKI-EXIT
     END-IF.
*
  RUISEKI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   エラー状況ファイル検索                      *
*--------------------------------------------------------------*
 ERJ-READ         SECTION.
*
     MOVE     "ERJ-READ"      TO     SEC-NAME.
*
     MOVE     LINK-IN-KANRINO TO     ERJ-F01.
*****DISPLAY "KEY = " ERJ-F01        UPON CONS.
     READ     ERRJYOL1
         INVALID
              MOVE   "1"      TO     ERJ-INV-FLG
         NOT INVALID
              MOVE   SPACE    TO     ERJ-INV-FLG
     END-READ.
*
 ERJ-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
