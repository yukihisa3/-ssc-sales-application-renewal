****************************************************************
*    顧客名　　　　　：　_サカタのタネ　営業第２部向け　　　　*
*    業務名　　　　　：　システム共通サブシステム　　　　　　　*
*    モジュール名　　：　入荷実績リカバリ処理　　　　　        *
*    作成日／作成者　：　2021/12/02 INOUE                      *
*    処理概要　　　　：　Ｄ３６５エラー累積ファイルを元に　　　*
*    　　　　　　　　　　入荷実績を未計上に戻す　　　　　　　　*
*    更新日／更新者　：　                                      *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SER0080B.
*                  流用:SER0030B.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/12/02.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*Ｄ３６５累積ファイル
     SELECT  ERRRUIL1   ASSIGN    TO   DA-01-VI-ERRRUIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE   IS SEQUENTIAL
                        RECORD    KEY    IS ERR-F01
                        FILE      STATUS IS ERR-STATUS.
*Ｄ３６５入庫ファイル
     SELECT  NYKFILL1   ASSIGN    TO   DA-01-VI-NYKFILL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE   IS RANDOM
                        RECORD    KEY    IS NYK-F02
                                            NYK-F03
                                            NYK-F04
                                            NYK-F05
                        FILE      STATUS IS NYK-STATUS.
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
 FD    ERRRUIL1    LABEL      RECORD    IS    STANDARD.
       COPY        ERRRUIL1   OF    XFDLIB
                   JOINING    ERR   PREFIX.
 FD    NYKFILL1    LABEL      RECORD    IS    STANDARD.
       COPY        NYKFILL1   OF    XFDLIB
                   JOINING    NYK   PREFIX.
 FD    ERRJYOL1    LABEL      RECORD    IS    STANDARD.
       COPY        ERRJYOL1   OF    XFDLIB
                   JOINING    ERJ   PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  WK-RENBAN               PIC  9(08).
 01  NYK-STATUS              PIC  X(02).
 01  ERR-STATUS              PIC  X(02).
 01  ERJ-STATUS              PIC  X(02).
 01  RUI-END-FLG             PIC  X(01).
 01  ERJ-INV-FLG             PIC  X(01).
 01  NYK-INV-FLG             PIC  X(01).
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
*件数
 01  RD-CNT                  PIC  9(07).
 01  UP-CNT                  PIC  9(07).
 01  ER01-CNT                PIC  9(07).
 01  ER02-CNT                PIC  9(07).
 01  ER03-CNT                PIC  9(07).
 01  ER04-CNT                PIC  9(07).
 01  ER05-CNT                PIC  9(07).
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
         05  ST-PG           PIC  X(08) VALUE "SER0080B".
         05  FILLER          PIC  X(11)
                                  VALUE " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  END-PG          PIC  X(08) VALUE "SER0080B".
         05  FILLER          PIC  X(11)
                                  VALUE "  END  *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  END-PG          PIC  X(08) VALUE "SER0080B".
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
                           PROCEDURE     NYKFILL1.
     MOVE     "NYKFILL1"   TO     AB-FILE.
     MOVE     NYK-STATUS   TO     AB-STS.
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
     OPEN     I-O       ERRRUIL1  NYKFILL1  ERRJYOL1.
     MOVE     ZERO      TO        ER01-CNT
                                  ER02-CNT
                                  ER03-CNT
                                  ER04-CNT
                                  ER05-CNT
                                  RD-CNT
                                  UP-CNT.
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
 000-PROG-04.
*エラー状況ファイル検索
     PERFORM  ERJ-READ.
     IF       ERJ-INV-FLG NOT =  SPACE
              DISPLAY "# エラー状況ファイルなし #" UPON CONS
              MOVE   4001    TO     PROGRAM-STATUS
              GO             TO     000-PROG-09
     END-IF.
*
*累積ファイルＳＴＡＲＴ
     MOVE   LINK-IN-KANRINO  TO ERR-F01.
     START  ERRRUIL1  KEY  >=   ERR-F01
            INVALID   KEY
               DISPLAY NC"＃累積ファイルなし１＃" UPON CONS
               MOVE    4001      TO   PROGRAM-STATUS
               GO                TO   000-PROG-09
     END-START.
*累積ファイル読込
 000-PROG-05.
     PERFORM  RUI-READ.
     IF       RUI-END-FLG NOT =  SPACE
              DISPLAY NC"＃累積ファイルなし２＃" UPON CONS
              MOVE    4001      TO   PROGRAM-STATUS
              GO                TO   000-PROG-09
     END-IF.
*メイン
 000-PROG-06.
     PERFORM  MAIN-SEC  UNTIL  END-FLG = "END".
*エラー状況ファイル更新
 000-PROG-07.
     MOVE     UP-CNT        TO     ERJ-F082.
     MOVE     LINK-IN-BUMCD TO     ERJ-F083.
     MOVE     LINK-IN-TANCD TO     ERJ-F084.
     MOVE     SYS-DATEW     TO     ERJ-F085.
     MOVE     WK-TIME(1:6)  TO     ERJ-F086.
     REWRITE  ERJ-REC.
*表示
 000-PROG-08.
     DISPLAY "管理番号" " = " LINK-IN-KANRINO UPON CONS.
     DISPLAY "バッチ日" " = " LINK-IN-BDATE   UPON CONS.
     DISPLAY "バッチ時" " = " LINK-IN-BTIME   UPON CONS.
     DISPLAY "読込件数" " = " RD-CNT          UPON CONS.
     DISPLAY "更新件数" " = " UP-CNT          UPON CONS.
*クローズ
 000-PROG-09.
     CLOSE    ERRRUIL1 NYKFILL1 ERRJYOL1.
*
 000-PROG-10.
     STOP RUN.
*
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   累積ファイル読込　　　                      *
*--------------------------------------------------------------*
 RUI-READ         SECTION.
*
     MOVE     "RUI-READ"     TO     SEC-NAME.
*
     READ     ERRRUIL1
         AT END
              MOVE   "1"     TO     RUI-END-FLG
              GO             TO     RUI-READ-EXIT
         NOT AT END
              MOVE   SPACE   TO     RUI-END-FLG
              ADD    1       TO     RD-CNT
     END-READ.
*
     IF  ERR-F01  NOT = LINK-IN-KANRINO
         MOVE    "1"         TO     RUI-END-FLG
         GO                  TO     RUI-READ-EXIT
     END-IF.
*
     IF  ERR-F043 NOT = "03"
         GO                  TO     RUI-READ
     END-IF.
*
 RUI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   メイン処理                                  *
*--------------------------------------------------------------*
 MAIN-SEC           SECTION.
     MOVE     "MAIN-SEC"       TO     SEC-NAME.
*
 MAIN-SEC-01.
*Ｄ３６５入庫ファイル検索
     PERFORM  NYK-READ.
     IF       NYK-INV-FLG NOT =  SPACE
              DISPLAY "# Ｄ３６５入庫ファイルなし #" UPON CONS
              DISPLAY "# 　伝票番号＝ " ERR-F044 "#" UPON CONS
              DISPLAY "# 　枝番　　＝ " ERR-F046 " #" UPON CONS
              DISPLAY "# 　相殺区分＝ " "0"      "  #" UPON CONS
              DISPLAY "# 　行番号　＝ " ERR-F045 " #" UPON CONS
              GO             TO     MAIN-SEC-99
     END-IF.
*
 MAIN-SEC-02.
*D365入庫ファイル更新
     MOVE     0                TO     NYK-F31.
     MOVE     SPACE            TO     NYK-F35.
     MOVE     SPACE            TO     NYK-F92.
     MOVE     SPACE            TO     NYK-F93.
     REWRITE  NYK-REC.
*
 MAIN-SEC-03.
*D365エラー累積ファイル更新
     MOVE     LINK-IN-TANCD    TO     ERR-F97.
     MOVE     SYS-DATEW        TO     ERR-F98.
     MOVE     WK-TIME(1:6)     TO     ERR-F99.
     REWRITE  ERR-REC.
*
     ADD      1             TO     UP-CNT.
*
 MAIN-SEC-99.
*D365エラー累積ファイル読込
     PERFORM  RUI-READ.
     IF       RUI-END-FLG  NOT=  SPACE
              MOVE   "END"      TO   END-FLG
              GO                TO   MAIN-SEC-EXIT
     END-IF.
*
  MAIN-SEC-EXIT.
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
*--------------------------------------------------------------*
*    LEVEL   1.1   Ｄ３６５入庫ファイル検索                    *
*--------------------------------------------------------------*
 NYK-READ         SECTION.
*
     MOVE     "NYK-READ"      TO     SEC-NAME.
*
     MOVE     ERR-F044(14:7)  TO     NYK-F02.
     MOVE     ERR-F046        TO     NYK-F03.
     MOVE     0               TO     NYK-F04.
     MOVE     ERR-F045        TO     NYK-F05.
*****DISPLAY "KEY2= " NYK-F02        UPON CONS.
*****DISPLAY "KEY3= " NYK-F03        UPON CONS.
*****DISPLAY "KEY4= " NYK-F04        UPON CONS.
*****DISPLAY "KEY5= " NYK-F05        UPON CONS.
     READ     NYKFILL1
         INVALID
              MOVE   "1"      TO     NYK-INV-FLG
         NOT INVALID
              MOVE   SPACE    TO     NYK-INV-FLG
     END-READ.
*
 NYK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
