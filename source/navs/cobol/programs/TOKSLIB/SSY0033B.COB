****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　出荷明細表                        *
*    モジュール名　　　　：　出荷明細データ_番集計            *
*    作成日／更新日　　　：　99/09/14                          *
*    作成者／更新者　　　：　ＮＡＶ吉田　　　　　　　　　　　　*
*    処理概要　　　　　　：　出荷明細ワークを読み、倉庫、納品  *
*                            日、部門、_番、相手商品コードで  *
*                            数量を集計し、出荷明細ワーク_へ  *
*                            出力する。                        *
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*　　更新日／更新者　　　：　04/06/24  /  YOSHIDA              *
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY0033B.
 AUTHOR.                NAV Y.YOSHIDA.
 DATE-WRITTEN.          99/09/14.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*出荷明細ワークファイル
     SELECT   SHWSYUKF  ASSIGN    TO        DA-01-S-SHWSYUKF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SYUK-STATUS.
*出荷明細ワーク（_）
     SELECT   SHWSYUTF  ASSIGN    TO        DA-01-S-SHWSYUTF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SYUT-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    出荷明細ワークファイル
******************************************************************
 FD  SHWSYUKF           BLOCK     CONTAINS  5    RECORDS.
     COPY     SHWSYUKF  OF        XFDLIB
              JOINING   SYUK  AS  PREFIX.
*
******************************************************************
*    出荷明細ワーク（_）
******************************************************************
 FD  SHWSYUTF           BLOCK     CONTAINS  5    RECORDS.
     COPY     SHWSYUKF  OF        XFDLIB
              JOINING   SYUT  AS  PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
     03  WK-HACYUSU          PIC S9(09)V99  VALUE  ZERO.
     03  WK-TEISUU           PIC S9(09)V99  VALUE  ZERO.
     03  WK-SYUK-REC         PIC  X(200)    VALUE  SPACE.
*    キーエリア
 01  KEY-AREA.
     03  BRK-KEY.
       05  BRK-SOKO          PIC  X(02)     VALUE  ZERO.
       05  BRK-NOUHIN        PIC  9(08)     VALUE  ZERO.
       05  BRK-BUMON         PIC  X(04)     VALUE  SPACE.
       05  BRK-TANA          PIC  X(06)     VALUE  SPACE.
       05  BRK-SHOCD         PIC  X(13)     VALUE  SPACE.
     03  CUR-KEY.
       05  CUR-SOKO          PIC  X(02)     VALUE  ZERO.
       05  CUR-NOUHIN        PIC  9(08)     VALUE  ZERO.
       05  CUR-BUMON         PIC  X(04)     VALUE  SPACE.
       05  CUR-TANA          PIC  X(06)     VALUE  SPACE.
       05  CUR-SHOCD         PIC  X(13)     VALUE  SPACE.
*
 01  WK-ST.
     03  SYUK-STATUS       PIC  X(02).
     03  SYUT-STATUS       PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY0033B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY0033B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY0033B".
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
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHWSYUKF.
     MOVE      "SHWSYUKF"   TO   AB-FILE.
     MOVE      SYUK-STATUS  TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHWSYUTF.
     MOVE      "SHWSYUTF"   TO   AB-FILE.
     MOVE      SYUT-STATUS  TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SHWSYUKF.
     OPEN     OUTPUT    SHWSYUTF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
     READ     SHWSYUKF
         AT END    MOVE      9         TO   END-FG
         NOT AT END
                   ADD       1         TO   RD-CNT
                   MOVE      SYUK-F02  TO   CUR-SOKO
                   MOVE      SYUK-F15  TO   CUR-NOUHIN
                   MOVE      SYUK-F04  TO   CUR-BUMON
                   MOVE      SYUK-F05  TO   CUR-TANA
                   MOVE      SYUK-F07  TO   CUR-SHOCD
                   MOVE      CUR-KEY   TO   BRK-KEY
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     IF       CUR-KEY        =    BRK-KEY
              MOVE      SYUK-REC       TO   WK-SYUK-REC
              ADD       SYUK-F09       TO   WK-HACYUSU
              ADD       SYUK-F10       TO   WK-TEISUU
     ELSE
              MOVE      SPACE          TO   SYUT-REC
              INITIALIZE                    SYUT-REC
              MOVE      WK-SYUK-REC    TO   SYUT-REC
              MOVE      WK-HACYUSU     TO   SYUT-F09
              MOVE      WK-TEISUU      TO   SYUT-F10
              WRITE     SYUT-REC
              ADD       1              TO   WRT-CNT
              MOVE      ZERO           TO   WK-HACYUSU
              MOVE      ZERO           TO   WK-TEISUU
*
              MOVE      CUR-KEY        TO   BRK-KEY
              MOVE      SYUK-REC       TO   WK-SYUK-REC
              ADD       SYUK-F09       TO   WK-HACYUSU
              ADD       SYUK-F10       TO   WK-TEISUU
     END-IF.
*
     READ     SHWSYUKF
         AT END    MOVE      9         TO   END-FG
         NOT AT END
                   ADD       1         TO   RD-CNT
                   MOVE      SYUK-F02  TO   CUR-SOKO
                   MOVE      SYUK-F15  TO   CUR-NOUHIN
                   MOVE      SYUK-F04  TO   CUR-BUMON
                   MOVE      SYUK-F05  TO   CUR-TANA
                   MOVE      SYUK-F07  TO   CUR-SHOCD
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC      SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     IF       RD-CNT    >         ZERO
              MOVE      SPACE          TO   SYUT-REC
              INITIALIZE                    SYUT-REC
              MOVE      WK-SYUK-REC    TO   SYUT-REC
              MOVE      WK-HACYUSU     TO   SYUT-F09
              MOVE      WK-TEISUU      TO   SYUT-F10
              WRITE     SYUT-REC
              ADD       1              TO   WRT-CNT
     END-IF.
     MOVE     RD-CNT    TO        IN-CNT.
     MOVE     WRT-CNT   TO        OUT-CNT.
     DISPLAY  MSG-IN    UPON CONS.
     DISPLAY  MSG-OUT   UPON CONS.
     DISPLAY  MSG-END   UPON CONS.
*
     CLOSE    SHWSYUKF  SHWSYUTF.
*
     STOP     RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
