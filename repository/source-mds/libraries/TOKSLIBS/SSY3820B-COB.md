# SSY3820B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3820B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　発注業務　　　　　　　            *
*    モジュール名　　　　：　発注書印刷ワーク作成　　　　　　　*
*    作成日／更新日　　　：　2015/05/15                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注書商品明細ワークより、　　　  *
*                            発注書印刷ワークデータを作成する。*
*    作成日／更新日　　　：　2015/08/12                        *
*                            発注書店舗ワークＬＦキー変更      *
*                            出荷日→店着日　　　　　　　      *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3820B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/05/15.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*発注書明細ワークＦ
     SELECT   HCSMXXX1  ASSIGN    TO        DA-01-VI-HCSMXXX1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SME-F01
                                            SME-F05
                                            SME-F07
                                            SME-F09
                                            WITH DUPLICATES
                        FILE STATUS    IS   SME-STATUS.
*発注書店舗ワーク
     SELECT   HCTPXXX1  ASSIGN    TO        DA-01-VI-HCTPXXX1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       TEN-F01
                                            TEN-F05
                                            TEN-F09
                                            TEN-F06
                                            TEN-F07
                        FILE  STATUS   IS   TEN-STATUS.
*発注書印刷ワークＦ
     SELECT   HCPRXXX1  ASSIGN    TO        DA-01-VI-HCPRXXX1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       PRW-F01
                                            PRW-F05
                                            PRW-F07
                                            PRW-F12
                                            PRW-F13
                                            WITH DUPLICATES
                        FILE STATUS    IS   PRW-STATUS.
******************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    発注書明細ワークＦ
******************************************************************
 FD  HCSMXXX1           LABEL RECORD   IS   STANDARD.
     COPY     HCSMXXX1  OF        XFDLIB
              JOINING   SME  AS   PREFIX.
******************************************************************
*    発注書店舗ワーク
******************************************************************
 FD  HCTPXXX1           LABEL RECORD   IS   STANDARD.
     COPY     HCTPXXX1  OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
*    発注書印刷ワークＦ
******************************************************************
 FD  HCPRXXX1           LABEL RECORD   IS   STANDARD.
     COPY     HCPRXXX1  OF        XFDLIB
              JOINING   PRW  AS   PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  SUTE-FLG                PIC  X(03)     VALUE  ZERO.
 01  DEL-FLG                 PIC  X(03)     VALUE  ZERO.
 01  KEP-FLG                 PIC  X(01)     VALUE  ZERO.
 01  SKIP-CNT                PIC  9(07)     VALUE  ZERO.
 01  HCTPXXX1-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  HCSMXXX1-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  HCSMXXX1-SEQ-CNT        PIC  9(07)     VALUE  ZERO.
 01  TRKAKUF-READ-CNT        PIC  9(07)     VALUE  ZERO.
 01  HCSMXXX1-SKIP-CNT       PIC  9(07)     VALUE  ZERO.
 01  PRW-ADD-CNT             PIC  9(07)     VALUE  ZERO.
 01  PRW-UPD-CNT             PIC  9(07)     VALUE  ZERO.
 01  PRW-HACU-ADD-CNT        PIC  9(07)     VALUE  ZERO.
 01  PRW-HACU-UPD-CNT        PIC  9(07)     VALUE  ZERO.
 01  PRW-TRAY-ADD-CNT        PIC  9(07)     VALUE  ZERO.
 01  PRW-TRAY-UPD-CNT        PIC  9(07)     VALUE  ZERO.
 01  PRW-HAKO-ADD-CNT        PIC  9(07)     VALUE  ZERO.
 01  PRW-HAKO-UPD-CNT        PIC  9(07)     VALUE  ZERO.
 01  TEN-DATA-CNT            PIC  9(07)     VALUE  ZERO.
 01  TEN-GYO-CNT             PIC  9(07)     VALUE  ZERO.
 01  TEN-WT-CNT              PIC  9(07)     VALUE  ZERO.
 01  TEN-REWT-CNT            PIC  9(07)     VALUE  ZERO.
 01  KIHON-ADD-CNT           PIC  9(07)     VALUE  ZERO.
 01  KIHON-UPD-CNT           PIC  9(07)     VALUE  ZERO.
 01  DEN-UPD-CNT             PIC  9(07)     VALUE  ZERO.
 01  TPTBL-CNT               PIC  9(07)     VALUE  ZERO.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  ZAMZAIF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HCSMXXX1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  HCSMXXX1-END-FLG        PIC  X(03)     VALUE  SPACE.
 01  HCSMXXX1-DEL-CNT        PIC  9(07)     VALUE  ZERO.
 01  HCTPXXX1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  HCTPXXX1-END-FLG        PIC  X(03)     VALUE  SPACE.
 01  HCTPXXX1-DEL-CNT        PIC  9(07)     VALUE  ZERO.
 01  NFKOSUL1-DEL-CNT        PIC  9(07)     VALUE  ZERO.
 01  NFSHOMS1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  NFJOHOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  SHTDENF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  TPTBL-IX                PIC  9(03)     VALUE  ZERO.
*
*ワーク店舗ＴＢＬ
 01  WK-TPTBL.
   03  WK-TPTBLAREA          OCCURS         500.
     05  TPTBL-TENCD         PIC  9(05)     VALUE  ZERO.
     05  TPTBL-HAKO          PIC  9(07)     VALUE  ZERO.
     05  TPTBL-PAGE          PIC  9(05)     VALUE  ZERO.
 01  WRK-AREA.
     03  WRK-TEISUU          PIC S9(09)V99  VALUE  ZERO.
     03  WRK-MAETEISUU       PIC S9(09)V99  VALUE  ZERO.
*ブレイク判定
 01  WK-BRK.
*    発注商品明細ワーク
     03  OLD-SME-F01         PIC  9(08)     VALUE  ZERO.
     03  OLD-SME-F05         PIC  X(02)     VALUE  SPACE.
     03  OLD-SME-F07         PIC  9(08)     VALUE  ZERO.
     03  HCSMXXX1-BRK-FLG    PIC  X(03)     VALUE  SPACE.
*    発注店舗ワーク
     03  OLD-TEN-F01         PIC  9(08)     VALUE  ZERO.
     03  OLD-TEN-F05         PIC  X(02)     VALUE  SPACE.
     03  OLD-TEN-F09         PIC  9(08)     VALUE  ZERO.
     03  HCTPXXX1-BRK-FLG    PIC  X(03)     VALUE  SPACE.
*発注数合計レコード用
*01  PRW-HACU-REC.
     COPY     HCPRXXX1  OF        XFDLIB
              JOINING   PRW-HACU  AS   PREFIX.
*トレー数合計レコード用
*01  PRW-TRAY-REC.
     COPY     HCPRXXX1  OF        XFDLIB
              JOINING   PRW-TRAY  AS   PREFIX.
*箱数合計レコード用
*01  PRW-HAKO-REC.
     COPY     HCPRXXX1  OF        XFDLIB
              JOINING   PRW-HAKO  AS   PREFIX.
*
*計算領域
 01  WRK-AREA2.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*計算領域
 01  WRK-AREA3.
     03  WK-TORAY0.
       05  WK-TORAY          PIC 9(06)V9(02)   VALUE ZERO.
     03  WK-TORAYR           REDEFINES WK-TORAY0.
       05  WK-TORAY1         PIC 9(08).
     03  WK-KONPO0.
       05  WK-KONPO          PIC 9(06)V9(01)    VALUE ZERO.
     03  WK-KONPOR           REDEFINES WK-KONPO0.
       05  WK-KONPO1         PIC 9(07).
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  KAK-STATUS        PIC  X(02).
     03  STE-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  ZAI-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  SME-STATUS        PIC  X(02).
     03  SHO-STATUS        PIC  X(02).
     03  PRW-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3820B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3820B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3820B".
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
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*LINKAGE                SECTION.
*01  PARA-IN-KANRINO    PIC   9(08).
*01  PARA-IN-SAKUBACD   PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HCSMXXX1.
     MOVE      "HCSMXXX1"   TO   AB-FILE.
     MOVE      SME-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HCTPXXX1.
     MOVE      "HCTPXXX1"   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HCPRXXX1.
     MOVE      "HCPRXXX1"   TO   AB-FILE.
     MOVE      PRW-STATUS   TO   AB-STS.
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
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     INITIALIZE                   WRK-AREA.
*
     OPEN     INPUT     HCTPXXX1 HCSMXXX1.
     OPEN     I-O       HCPRXXX1.
*
     DISPLAY  MSG-START UPON CONS.
*
******************
*システム日付編集*
******************
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
*
*INIT-01.
*発注書商品明細ワーク初期読込
     PERFORM HCSMXXX1-READ-SEC.
     IF   HCSMXXX1-END-FLG = "END"
          DISPLAY NC"＃＃発注書商品明細ワーク　データなし！＃＃"
                                                     UPON CONS
          MOVE "END"            TO   END-FLG
          MOVE 4010             TO   PROGRAM-STATUS
          GO                    TO   INIT-EXIT
     END-IF.
*
 INIT-02.
*発注書店舗ワーク初期読込
     PERFORM HCTPXXX1-READ-SEC.
     IF   HCTPXXX1-END-FLG = "END"
          DISPLAY NC"＃＃発注書店舗ワーク　データなし！＃＃"
                                                     UPON CONS
          MOVE "END"            TO   END-FLG
          MOVE 4010             TO   PROGRAM-STATUS
          GO                    TO   INIT-EXIT
     ELSE
          MOVE   TEN-F01        TO   OLD-TEN-F01
          MOVE   TEN-F05        TO   OLD-TEN-F05
          MOVE   TEN-F09        TO   OLD-TEN-F09
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    発注書商品明細ワーク読込
****************************************************************
 HCSMXXX1-READ-SEC           SECTION.
*
     MOVE    "HCSMXXX1-READ-SEC"  TO   S-NAME.
*
     READ     HCSMXXX1  AT  END
              MOVE     "END"      TO   HCSMXXX1-END-FLG
              GO                  TO   HCSMXXX1-READ-EXIT
     END-READ.
*
     ADD      1                   TO   HCSMXXX1-READ-CNT
                                       HCSMXXX1-SEQ-CNT.
*
 HCSMXXX1-READ-EXIT.
     EXIT.
*
****************************************************************
*    発注書店舗ワーク読込
****************************************************************
 HCTPXXX1-READ-SEC           SECTION.
*
     MOVE    "HCTPXXX1-READ-SEC"  TO   S-NAME.
*
     READ     HCTPXXX1  AT  END
              MOVE     "END"      TO   HCTPXXX1-END-FLG
              GO                  TO   HCTPXXX1-READ-EXIT
     END-READ.
*
     ADD      1                   TO   HCTPXXX1-READ-CNT.
*
 HCTPXXX1-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO      S-NAME.
*
 MAIN-000.
*発注商品明細ワーク　ブレイク判定
*TEST↓
*    DISPLAY "MAIN-000 TEN-F01=" TEN-F01 UPON CONS.
*    DISPLAY "MAIN-000 OLD-TEN-F01=" OLD-TEN-F01 UPON CONS.
*    DISPLAY "MAIN-000 TEN-F05=" TEN-F05 UPON CONS.
*    DISPLAY "MAIN-000 OLD-TEN-F05=" OLD-TEN-F05 UPON CONS.
*    DISPLAY "MAIN-000 TEN-F09=" TEN-F09 UPON CONS.
*    DISPLAY "MAIN-000 OLD-TEN-F09=" OLD-TEN-F09 UPON CONS.
*TEST↑
     PERFORM  HCSMXXX1-BRK-SEC.
     IF       HCSMXXX1-BRK-FLG     =    "BRK"
*             MOVE    "   "        TO    HCSMXXX1-BRK-FLG
              MOVE     ZERO        TO    TPTBL-CNT
              MOVE     1           TO    HCSMXXX1-SEQ-CNT
              GO                   TO    MAIN-001
     ELSE
              GO                   TO    MAIN-010
     END-IF.
*
 MAIN-001.
*発注書店舗ワーク→ワーク店舗ＴＢＬセット
     INITIALIZE                          WK-TPTBL.
     PERFORM WKTPTBL-SET-SEC.
*TEST↓
     DISPLAY NC"店舗ＴＢＬセット件数＝" TPTBL-CNT  UPON  CONS.
*TEST↑
*
*発注書印刷ワーク　レコード項目セット～出力
*　　・発注書印刷ワーク
*　　・発注数合計
*　　・トレー数合計
*　　・箱数合計
 MAIN-010.
*発注商品明細１レコード毎に１から（ＭＡＸ５００）
     MOVE     1                  TO      TPTBL-CNT.
 MAIN-011.
*発注商品明細１レコード内１２店舗毎
     MOVE     SPACE              TO      PRW-REC
                                         PRW-HACU-REC
                                         PRW-TRAY-REC
                                         PRW-HAKO-REC.
     INITIALIZE                          PRW-REC
                                         PRW-HACU-REC
                                         PRW-TRAY-REC
                                         PRW-HAKO-REC.
     PERFORM  VARYING  TPTBL-IX  FROM  1 BY 1
              UNTIL  ( TPTBL-IX  > 12  )
                  OR ( TPTBL-CNT > 500 )
                  OR ( TPTBL-TENCD(TPTBL-CNT) = 0 )
*    発注書印刷ワーク　セット
              MOVE     SME-F01     TO    PRW-F01
              MOVE     SME-F02     TO    PRW-F02
              MOVE     SME-F03     TO    PRW-F03
              MOVE     SME-F04     TO    PRW-F04
              MOVE     SME-F05     TO    PRW-F05
              MOVE     SME-F06     TO    PRW-F06
              MOVE     SME-F07     TO    PRW-F07
              MOVE     SME-F09     TO    PRW-F08
              MOVE     SME-F10     TO    PRW-F09
              MOVE     SME-F11     TO    PRW-F10
              MOVE     SME-F12     TO    PRW-F11
              MOVE     TPTBL-PAGE (TPTBL-CNT)
                                   TO    PRW-F12
              MOVE     HCSMXXX1-SEQ-CNT
                                   TO    PRW-F13
              MOVE     TPTBL-TENCD(TPTBL-CNT)
                                   TO    PRW-F201(TPTBL-IX)
              MOVE     SME-F201(TPTBL-CNT)
                                   TO    PRW-F202(TPTBL-IX)
              MOVE     SME-F21     TO    PRW-F21
              MOVE     SME-F22     TO    PRW-F22
*
*    発注数合計・トレー数合計・箱数合計　セット
              MOVE     SME-F01     TO    PRW-HACU-F01
                                         PRW-TRAY-F01
                                         PRW-HAKO-F01
              MOVE     SME-F02     TO    PRW-HACU-F02
                                         PRW-TRAY-F02
                                         PRW-HAKO-F02
              MOVE     SME-F03     TO    PRW-HACU-F03
                                         PRW-TRAY-F03
                                         PRW-HAKO-F03
              MOVE     SME-F04     TO    PRW-HACU-F04
                                         PRW-TRAY-F04
                                         PRW-HAKO-F04
              MOVE     SME-F05     TO    PRW-HACU-F05
                                         PRW-TRAY-F05
                                         PRW-HAKO-F05
              MOVE     SME-F06     TO    PRW-HACU-F06
                                         PRW-TRAY-F06
                                         PRW-HAKO-F06
              MOVE     SME-F07     TO    PRW-HACU-F07
                                         PRW-TRAY-F07
                                         PRW-HAKO-F07
              MOVE     SPACE       TO    PRW-HACU-F08
                                         PRW-TRAY-F08
                                         PRW-HAKO-F08
              MOVE     SPACE       TO    PRW-HACU-F09
                                         PRW-TRAY-F09
                                         PRW-HAKO-F09
              MOVE     SPACE       TO    PRW-HACU-F10
                                         PRW-TRAY-F10
                                         PRW-HAKO-F10
              MOVE     ZERO        TO    PRW-HACU-F11
                                         PRW-TRAY-F11
                                         PRW-HAKO-F11
              MOVE     TPTBL-PAGE (TPTBL-CNT)
                                   TO    PRW-HACU-F12
                                         PRW-TRAY-F12
                                         PRW-HAKO-F12
              MOVE     99997       TO    PRW-HACU-F13
              MOVE     99998       TO    PRW-TRAY-F13
              MOVE     99999       TO    PRW-HAKO-F13
              MOVE     ZERO        TO    PRW-HACU-F201(TPTBL-IX)
                                         PRW-TRAY-F201(TPTBL-IX)
                                         PRW-HAKO-F201(TPTBL-IX)
              MOVE     SME-F201(TPTBL-CNT)
                                   TO    PRW-HACU-F202(TPTBL-IX)
              MOVE     SME-F202(TPTBL-CNT)
                                   TO    PRW-TRAY-F202(TPTBL-IX)
              MOVE     TPTBL-HAKO(TPTBL-CNT)
                                   TO    PRW-HAKO-F202(TPTBL-IX)
              MOVE     ZERO        TO    PRW-HACU-F21
                                         PRW-TRAY-F21
                                         PRW-HAKO-F21
              MOVE     ZERO        TO    PRW-HACU-F22
                                         PRW-TRAY-F22
                                         PRW-HAKO-F22
*
              ADD      1           TO    TPTBL-CNT
     END-PERFORM.
*
     IF     ( TPTBL-CNT > 493 )
         OR ( TPTBL-TENCD(TPTBL-CNT) = 0 )
*TEST↓
*             DISPLAY "TPTBL-CNT=" TPTBL-CNT UPON CONS
*             DISPLAY "TPTBL-TENCD=" TPTBL-TENCD(TPTBL-CNT)
*                                            UPON CONS
*TEST↑
              MOVE    "1"          TO    PRW-F23
     END-IF.
*
 MAIN-020.
*    発注書印刷ワーク　出力
*TEST↓
*    DISPLAY  "MAIN-020 WRITE F01=" PRW-F01 UPON CONS.
*TEST↑
     WRITE    PRW-REC.
     ADD      1        TO          PRW-ADD-CNT.
*
 MAIN-021.
*    発注数合計レコード　出力OR更新
     MOVE     PRW-HACU-F01         TO    PRW-F01.
     MOVE     PRW-HACU-F05         TO    PRW-F05.
     MOVE     PRW-HACU-F07         TO    PRW-F07.
     MOVE     PRW-HACU-F12         TO    PRW-F12.
     MOVE     PRW-HACU-F13         TO    PRW-F13.
     READ     HCPRXXX1
        INVALID
*TEST↓
*    DISPLAY  "MAIN-021 WRITE F01=" PRW-HACU-F01 UPON CONS
*TEST↑
              WRITE    PRW-REC     FROM  PRW-HACU-REC
              ADD      1           TO    PRW-HACU-ADD-CNT
        NOT INVALID
              PERFORM  VARYING  TPTBL-IX FROM  1 BY 1
                       UNTIL  ( TPTBL-IX > 12  )
                       ADD      PRW-HACU-F202(TPTBL-IX)
                                TO       PRW-F202(TPTBL-IX)
              END-PERFORM
*TEST↓
*    DISPLAY  "MAIN-021 REWRITE F01=" PRW-HACU-F01 UPON CONS
*TEST↑
              REWRITE  PRW-REC
              ADD      1           TO    PRW-HACU-UPD-CNT
     END-READ.
*
 MAIN-022.
*    トレー数合計レコード　出力OR更新
     MOVE     PRW-TRAY-F01         TO    PRW-F01.
     MOVE     PRW-TRAY-F05         TO    PRW-F05.
     MOVE     PRW-TRAY-F07         TO    PRW-F07.
     MOVE     PRW-TRAY-F12         TO    PRW-F12.
     MOVE     PRW-TRAY-F13         TO    PRW-F13.
     READ     HCPRXXX1
        INVALID
*TEST↓
*    DISPLAY  "MAIN-022 WRITE F01=" PRW-TRAY-F01 UPON CONS
*TEST↑
              WRITE    PRW-REC     FROM  PRW-TRAY-REC
              ADD      1           TO    PRW-TRAY-ADD-CNT
        NOT INVALID
              PERFORM  VARYING  TPTBL-IX FROM  1 BY 1
                       UNTIL  ( TPTBL-IX > 12  )
                       ADD      PRW-TRAY-F202(TPTBL-IX)
                                TO       PRW-F202(TPTBL-IX)
              END-PERFORM
*TEST↓
*    DISPLAY  "MAIN-022 REWRITE F01=" PRW-TRAY-F01 UPON CONS
*TEST↑
              REWRITE  PRW-REC
              ADD      1           TO    PRW-TRAY-UPD-CNT
     END-READ.
*
 MAIN-023.
*    箱数合計レコード　出力OR更新
     MOVE     PRW-HAKO-F01         TO    PRW-F01.
     MOVE     PRW-HAKO-F05         TO    PRW-F05.
     MOVE     PRW-HAKO-F07         TO    PRW-F07.
     MOVE     PRW-HAKO-F12         TO    PRW-F12.
     MOVE     PRW-HAKO-F13         TO    PRW-F13.
     READ     HCPRXXX1
        INVALID
*TEST↓
*    DISPLAY  "MAIN-023 WRITE F01=" PRW-HAKO-F01 UPON CONS
*TEST↑
              WRITE    PRW-REC     FROM  PRW-HAKO-REC
              ADD      1           TO    PRW-HAKO-ADD-CNT
        NOT INVALID
              PERFORM  VARYING  TPTBL-IX FROM  1 BY 1
                       UNTIL  ( TPTBL-IX > 12  )
                       MOVE     PRW-HAKO-F202(TPTBL-IX)
                                TO       PRW-F202(TPTBL-IX)
              END-PERFORM
*TEST↓
*    DISPLAY  "MAIN-023 REWRITE F01=" PRW-HAKO-F01 UPON CONS
*TEST↑
              REWRITE  PRW-REC
              ADD      1           TO    PRW-HAKO-UPD-CNT
     END-READ.
*
 MAIN-030.
*発注店舗ＴＢＬ分（ＭＡＸ５００）の店舗のセット・出力が
*　店舗のセット・出力が完了したかどうかを判定。
*＝発注商品明細１レコード分のセット・出力が完了したかどうか、
*　と同義。
     IF     ( TPTBL-CNT <= 500  )
        IF  ( TPTBL-TENCD(TPTBL-CNT)     NOT   =   0 )
*TEST↓
*    DISPLAY "MAIN-030 TPTBL-CNT=" TPTBL-CNT UPON CONS
*    DISPLAY "MAIN-030 TPTBL-TENCD=" TPTBL-TENCD(TPTBL-CNT)
*                                            UPON CONS
*TEST↑
              GO                   TO    MAIN-011
     END-IF.
*
 MAIN-040.
*発注商品明細ワーク　次レコード読込
*
     PERFORM  HCSMXXX1-READ-SEC.
     IF       HCSMXXX1-END-FLG     =    "END"
              MOVE "END"           TO    END-FLG
              GO                   TO    MAIN-EXIT
     END-IF.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*    発注書店舗ワーク→ワーク店舗ＴＢＬセット
****************************************************************
 WKTPTBL-SET-SEC             SECTION.
*
     MOVE    "WKTPTBL-SET-SEC  "  TO   S-NAME.
*
     MOVE    "   "                TO   HCTPXXX1-BRK-FLG.
*
     PERFORM  VARYING  TPTBL-IX FROM 1 BY 1
              UNTIL  ( TPTBL-IX > 500 )
                  OR ( HCTPXXX1-END-FLG = "END" )
                  OR ( HCTPXXX1-BRK-FLG = "BRK" )
              MOVE     TEN-F06     TO   TPTBL-TENCD(TPTBL-IX)
              MOVE     TEN-F11     TO   TPTBL-HAKO (TPTBL-IX)
              MOVE     TEN-F14     TO   TPTBL-PAGE (TPTBL-IX)
              ADD      1           TO   TPTBL-CNT
              PERFORM  HCTPXXX1-READ-SEC
*TEST↓
*    DISPLAY "WKTPTBL-SET-SEC TEN-F01=" TEN-F01 UPON CONS
*    DISPLAY "WKTPTBL-SET-SEC OLD-TEN-F01=" OLD-TEN-F01 UPON CONS
*    DISPLAY "WKTPTBL-SET-SEC TEN-F05=" TEN-F05 UPON CONS
*    DISPLAY "WKTPTBL-SET-SEC OLD-TEN-F05=" OLD-TEN-F05 UPON CONS
*    DISPLAY "WKTPTBL-SET-SEC TEN-F09=" TEN-F09 UPON CONS
*    DISPLAY "WKTPTBL-SET-SEC OLD-TEN-F09=" OLD-TEN-F09 UPON CONS
*TEST↑
              PERFORM  HCTPXXX1-BRK-SEC
     END-PERFORM.
*
 WKTPTBL-SET-EXIT.
     EXIT.
*
****************************************************************
*    発注書店舗ワーク　ブレイクチェック
****************************************************************
 HCTPXXX1-BRK-SEC           SECTION.
*
     MOVE    "HCTPXXX1-BRK-SEC "  TO   S-NAME.
*
     MOVE    "   "                TO   HCTPXXX1-BRK-FLG
     IF     ( TEN-F01   NOT =     OLD-TEN-F01 )
        OR  ( TEN-F05   NOT =     OLD-TEN-F05 )
        OR  ( TEN-F09   NOT =     OLD-TEN-F09 )
              MOVE     "BRK"      TO   HCTPXXX1-BRK-FLG
              MOVE      TEN-F01   TO   OLD-TEN-F01
              MOVE      TEN-F05   TO   OLD-TEN-F05
              MOVE      TEN-F09   TO   OLD-TEN-F09
     END-IF.
*
 HCTPXXX1-BRK-EXIT.
     EXIT.
*
****************************************************************
*    発注書商品明細ワーク　ブレイクチェック
****************************************************************
 HCSMXXX1-BRK-SEC           SECTION.
*
     MOVE    "HCSMXXX1-BRK-SEC "  TO   S-NAME.
*
     MOVE    "   "                TO   HCSMXXX1-BRK-FLG.
     IF     ( SME-F01   NOT =     OLD-SME-F01 )
        OR  ( SME-F05   NOT =     OLD-SME-F05 )
        OR  ( SME-F07   NOT =     OLD-SME-F07 )
              MOVE     "BRK"      TO   HCSMXXX1-BRK-FLG
              MOVE      SME-F01   TO   OLD-SME-F01
              MOVE      SME-F05   TO   OLD-SME-F05
              MOVE      SME-F07   TO   OLD-SME-F07
     END-IF.
*
 HCSMXXX1-BRK-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数印字
*発注書商品明細ワーク読込
     DISPLAY "HCSMXXX1  READ CNT = " HCSMXXX1-READ-CNT UPON CONS.
*発注書店舗ワーク読込
     DISPLAY "HCTPXXX1  READ CNT = " HCTPXXX1-READ-CNT UPON CONS.
*ＳＫＩＰ
*****DISPLAY "HCSMXXX1  SKIP CNT = " HCSMXXX1-SKIP-CNT UPON CONS.
*発注書印刷ワーク（明細）追加
     DISPLAY "HCPRXXX1  WRT  CNT = " PRW-ADD-CNT       UPON CONS.
*発注書印刷ワーク（発注数合計）追加
     DISPLAY "HCPR(HACU)WRT  CNT = " PRW-HACU-ADD-CNT  UPON CONS.
*発注書印刷ワーク（発注数合計）加算
     DISPLAY "HCPR(HACU)UPD  CNT = " PRW-HACU-UPD-CNT  UPON CONS.
*発注書印刷ワーク（トレー数合計）追加
     DISPLAY "HCPR(TRAY)WRT  CNT = " PRW-TRAY-ADD-CNT  UPON CONS.
*発注書印刷ワーク（トレー数合計）加算
     DISPLAY "HCPR(TRAY)UPD  CNT = " PRW-TRAY-UPD-CNT  UPON CONS.
*発注書印刷ワーク（箱数合計）追加
     DISPLAY "HCPR(HAKO)WRT  CNT = " PRW-HAKO-ADD-CNT  UPON CONS.
*発注書印刷ワーク（箱数合計）加算
     DISPLAY "HCPR(HAKO)UPD  CNT = " PRW-HAKO-UPD-CNT  UPON CONS.
*
     CLOSE     HCSMXXX1  HCTPXXX1  HCPRXXX1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
