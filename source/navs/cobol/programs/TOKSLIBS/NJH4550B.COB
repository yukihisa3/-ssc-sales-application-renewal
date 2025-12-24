****************************************************************
*                                                              *
*    顧客名　　　　　　　：（株）サカタのタネ殿　　　　　　　　*
*    業務名　　　　　　　：　ジャスコオンラインシステム　　　　*
*    モジュール名　　　　：　受信データタイプ別振り分け　　　　*
*    作成日／更新日　　　：　2003/04/30                        *
*    作成者／更新者　　　：　NAV                              *
*    処理概要　　　　　　：　オンライン受信データ区分ごとに　　*
*　　　　　　　　　　　　　　振り分ける　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SJH4550B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          03/04/30.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*****<<受信データ　>>******************************************
     SELECT   CVCS256   ASSIGN    TO        CVCS256
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   CV256-ST.
*
*****<<運用情報ファイル>>**************************************
     SELECT   DATAF1    ASSIGN    TO        DATAF1
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   DT1-ST.
*****<<発伝情報ファイルジャスコ>>******************************
     SELECT   DATAF2    ASSIGN    TO        DATAF2
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   DT2-ST.
*****<<汎用情報ファイル>>**************************************
     SELECT   DATAF3    ASSIGN    TO        DATAF3
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   DT3-ST.
*****<<支払明細ファイル>>**************************************
     SELECT   DATAF4    ASSIGN    TO        DATAF4
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   DT4-ST.
*****<<支払案内ファイル>>**************************************
     SELECT   DATAF5    ASSIGN    TO        DATAF5
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   DT5-ST.
*****<<発伝情報ファイルイオン>>********************************
     SELECT   DATAF6    ASSIGN    TO        DATAF6
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   DT6-ST.
*
 DATA                   DIVISION.
 FILE                   SECTION.
****************************************************************
*　　　　受信データ　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 FD  CVCS256.
 01  CV256-REC.
     03  CV256-REC1.
         05  CV1-F01              PIC  X(03).
         05  CV1-F02              PIC  X(13).
         05  CV1-F03              PIC  X(02).
         05  FILLER               PIC  X(110).
     03  CV256-REC2.
         05  CV2-F01              PIC  X(03).
         05  CV2-F02              PIC  X(13).
         05  CV2-F03              PIC  X(02).
         05  FILLER               PIC  X(110).
*
****************************************************************
*　　　　運用情報ファイル　　　　　　　　　　　　　　　　　　　*
****************************************************************
 FD  DATAF1.
 01  DATAF1-REC.
     03  FILLER                   PIC  X(128).
*
****************************************************************
*　　　　発伝情報ファイル（ジャスコ）　　　　　　　　　　　　　*
****************************************************************
 FD  DATAF2.
 01  DATAF2-REC.
     03  FILLER                   PIC  X(256).
*
****************************************************************
*　　　　汎用情報ファイル　　　　　　　　　　　　　　　　　　　*
****************************************************************
 FD  DATAF3.
 01  DATAF3-REC.
     03  FILLER                   PIC  X(128).
*
****************************************************************
*　　　　支払明細ファイル　　　　　　　　　　　　　　　　　　　*
****************************************************************
 FD  DATAF4.
 01  DATAF4-REC.
     03  FILLER                   PIC  X(128).
*
****************************************************************
*　　　　支払案内ファイル　　　　　　　　　　　　　　　　　　　*
****************************************************************
 FD  DATAF5.
 01  DATAF5-REC.
     03  FILLER                   PIC  X(128).
*
****************************************************************
*　　　　発伝情報ファイル（イオン）　　　　　　　　　　　　　　*
****************************************************************
 FD  DATAF6.
 01  DATAF6-REC.
     03  FILLER                   PIC  X(256).
*
 WORKING-STORAGE        SECTION.
*    エンドフラグ
 01  END-FLG                      PIC  9(01)  VALUE  ZERO.
*    伝票カウンタ
 01  DEN-CNT                      PIC  9(05)  VALUE  ZERO.
*
 01  HIKAKU-AREA.
     03  OLD-KUBUN                PIC  X(03)  VALUE  SPACE.
     03  OLD-SYUBETU              PIC  X(02)  VALUE  SPACE.
 01  SEL-FILE                     PIC  9(01)  VALUE  ZERO.
 01  SEL-CNT                      PIC  9(01)  VALUE  ZERO.
*    発注レコード編集
 01  WK-HENSYU-REC.
     03  WK-HENSYU-1              PIC  X(128).
     03  WK-HENSYU-2              PIC  X(128).
*    出力件数カウント
 01  WK-CNT-AREA.
     03  WK-CNT1                  PIC  9(06)  VALUE  ZERO.
     03  WK-CNT2                  PIC  9(06)  VALUE  ZERO.
     03  WK-CNT3                  PIC  9(06)  VALUE  ZERO.
     03  WK-CNT4                  PIC  9(06)  VALUE  ZERO.
     03  WK-CNT5                  PIC  9(06)  VALUE  ZERO.
     03  WK-CNT6                  PIC  9(06)  VALUE  ZERO.
*    ステイタス　エリア
 01  STATUS-AREA.
     03  CV256-ST                 PIC  X(02).
     03  DT1-ST                   PIC  X(02).
     03  DT2-ST                   PIC  X(02).
     03  DT3-ST                   PIC  X(02).
     03  DT4-ST                   PIC  X(02).
     03  DT5-ST                   PIC  X(02).
     03  DT6-ST                   PIC  X(02).
*    メッセージ　エリア


 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC X(04) VALUE "*** ".
         05  FILLER               PIC X(08) VALUE "SJH4550B".
         05  FILLER               PIC X(10) VALUE " START ***".
     03  MSG-END.
         05  FILLER               PIC X(04) VALUE "*** ".
         05  FILLER               PIC X(08) VALUE "SJH4550B".
         05  FILLER               PIC X(10) VALUE " END   ***".
     03  MSG-ABEND1.
         05  FILLER               PIC X(04) VALUE "### ".
         05  ERR-PG-ID            PIC X(08) VALUE "SJH4550B".
         05  FILLER               PIC X(10) VALUE " ABEND ###".
     03  MSG-ABEND2.
         05  FILLER               PIC X(04) VALUE "### ".
         05  ERR-FL-ID            PIC X(08) VALUE SPACE.
         05  FILLER               PIC X(04) VALUE " ST-".
         05  ERR-STCD             PIC X(02) VALUE SPACE.
         05  FILLER               PIC X(04) VALUE " ###".
*
     03  MSG-READ.
         05  FILLER               PIC X(04) VALUE "*** ".
         05  FILLER               PIC X(08) VALUE "SJH4550B".
         05  FILLER               PIC X(09) VALUE " READ = ".
         05  MSG-READ-CNT         PIC 9(06) VALUE ZERO.
         05  FILLER               PIC X(04) VALUE " ***".
     03  MSG-OUT.
         05  FILLER               PIC X(04) VALUE "*** ".
         05  FILLER               PIC X(08) VALUE "SJH4550B".
         05  FILLER               PIC X(09) VALUE " WRITE= ".
         05  MSG-OUT-CNT          PIC 9(06) VALUE ZERO.
         05  FILLER               PIC X(04) VALUE " ***".
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE CVCS256.
     MOVE     "CVCS256 "          TO        ERR-FL-ID.
     MOVE     CV256-ST            TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE DATAF1.
     MOVE     "DATAF1  "          TO        ERR-FL-ID.
     MOVE     DT1-ST              TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC4         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE DATAF2.
     MOVE     "DATAF2  "          TO        ERR-FL-ID.
     MOVE     DT2-ST              TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC5         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE DATAF3.
     MOVE     "DATAF3  "          TO        ERR-FL-ID.
     MOVE     DT3-ST              TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC6         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE DATAF4.
     MOVE     "DATAF4  "          TO        ERR-FL-ID.
     MOVE     DT4-ST              TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC7         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE DATAF5.
     MOVE     "DATAF5  "          TO        ERR-FL-ID.
     MOVE     DT5-ST              TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC6         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE DATAF6.
     MOVE     "DATAF6  "          TO        ERR-FL-ID.
     MOVE     DT6-ST              TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
*
****************************************************************
*　　　メインモジュール　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 GENERAL-PROCESS     SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC       UNTIL  END-FLG = 1.
     PERFORM  END-SEC.
     STOP     RUN.
 GENERAL-END.
     EXIT.
****************************************************************
*　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC     SECTION.
*
     DISPLAY  MSG-START UPON CONS.
     OPEN     INPUT     CVCS256.
     OPEN     OUTPUT    DATAF1    DATAF2    DATAF3
                        DATAF4    DATAF5    DATAF6.
*編集レコード初期化
     MOVE     SPACE          TO   WK-HENSYU-REC.
*編集カウント初期化
     MOVE     ZERO           TO   SEL-CNT.
*件数カウント初期化
     MOVE     ZERO           TO   WK-CNT-AREA.
*入力ファイル初期ＲＥＡＤ
     READ     CVCS256
              AT END
                 MOVE   1    TO   END-FLG
              NOT AT END
                 ADD    1    TO   MSG-READ-CNT
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     IF       MSG-READ-CNT   =   1
        IF    CV1-F01        NOT =     "JET"
              MOVE     1     TO  END-FLG
              GO             TO  MAIN-EXIT
        END-IF
     END-IF.
*
*データ振り分け
     IF       CV1-F01   NOT  =   "JET"     AND   "JSA"
                                           AND   "JSB"
              CONTINUE
     ELSE
              EVALUATE  CV1-F01  ALSO      CV1-F03
                   WHEN "JET"    ALSO      "00"
*                       運用情報ファイル
                        MOVE     1    TO   SEL-FILE
*****************************************
                   WHEN "JET"    ALSO      "01"
*                       発注情報ファイル（ジャスコ）
                        MOVE     2    TO   SEL-FILE
                   WHEN "JSA"    ALSO      "01"
*                       発注情報ファイル（ジャスコ）
                        MOVE     2    TO   SEL-FILE
                   WHEN "JSB"    ALSO      "01"
*                       発注情報ファイル（ジャスコ）
                        MOVE     2    TO   SEL-FILE
*****************************************
                   WHEN "JET"    ALSO      "02"
*                       汎用情報ファイル
                        MOVE     3    TO   SEL-FILE
                   WHEN "JET"    ALSO      "04"
*                       支払明細ファイル
                        MOVE     4    TO   SEL-FILE
                   WHEN "JET"    ALSO      "12"
*                       支払案内ファイル
                        MOVE     5    TO   SEL-FILE
*****************************************
                   WHEN "JET"    ALSO      "31"
*                       発注情報ファイル（イオン）
                        MOVE     6    TO   SEL-FILE
                   WHEN "JSA"    ALSO      "31"
*                       発注情報ファイル（イオン）
                        MOVE     6    TO   SEL-FILE
                   WHEN "JSB"    ALSO      "31"
*                       発注情報ファイル（イオン）
                        MOVE     6    TO   SEL-FILE
*****************************************
              END-EVALUATE
     END-IF.
*
*ファイル出力
     EVALUATE SEL-FILE
              WHEN      1
                        MOVE     CV256-REC1 TO  DATAF1-REC
                        WRITE    DATAF1-REC
                        IF       CV2-F01 NOT = SPACE
                                MOVE     CV256-REC2 TO  DATAF1-REC
                                WRITE    DATAF1-REC
                                ADD      1    TO   WK-CNT1
                        END-IF
                        ADD      1    TO   WK-CNT1
              WHEN      2
                        MOVE CV256-REC TO  DATAF2-REC
                        WRITE DATAF2-REC
                        ADD      1    TO   WK-CNT2
              WHEN      3
                        MOVE     CV256-REC1 TO  DATAF3-REC
                        WRITE    DATAF3-REC
                        IF       CV2-F01 NOT = SPACE
                                MOVE     CV256-REC2 TO  DATAF3-REC
                                WRITE    DATAF3-REC
                                ADD      1    TO   WK-CNT3
                        END-IF
                        ADD      1    TO   WK-CNT3
              WHEN      4
                        MOVE     CV256-REC1 TO  DATAF4-REC
                        WRITE    DATAF4-REC
                        IF       CV2-F01 NOT = SPACE
                                MOVE     CV256-REC2 TO  DATAF4-REC
                                WRITE    DATAF4-REC
                                ADD      1    TO   WK-CNT4
                        END-IF
                        ADD      1    TO   WK-CNT4
              WHEN      5
                        MOVE     CV256-REC1 TO  DATAF5-REC
                        WRITE    DATAF5-REC
                        IF       CV2-F01 NOT = SPACE
                                MOVE     CV256-REC2 TO  DATAF5-REC
                                WRITE    DATAF5-REC
                                ADD      1    TO   WK-CNT5
                        END-IF
                        ADD      1    TO   WK-CNT5
              WHEN      6
                        MOVE CV256-REC TO  DATAF6-REC
                        WRITE DATAF6-REC
                        ADD      1    TO   WK-CNT6
     END-EVALUATE.
*ファイル出力番号初期化
     MOVE     ZERO           TO   SEL-FILE.
*
     READ     CVCS256
              AT END
                 MOVE   1    TO   END-FLG
              NOT AT END
                 ADD    1    TO   MSG-READ-CNT
     END-READ.
*
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　終了処理　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC      SECTION.
*ファイルのクローズ
     CLOSE    CVCS256   DATAF1    DATAF2    DATAF3
              DATAF4    DATAF5    DATAF6.
*
     DISPLAY NC"運用件数　＝　"  WK-CNT1  UPON  CONS.
     DISPLAY NC"発伝件数Ｊ＝　"  WK-CNT2  UPON  CONS.
     DISPLAY NC"汎用件数　＝　"  WK-CNT3  UPON  CONS.
     DISPLAY NC"支明件数　＝　"  WK-CNT4  UPON  CONS.
     DISPLAY NC"支案件数　＝　"  WK-CNT5  UPON  CONS.
     DISPLAY NC"発伝件数Ｉ＝　"  WK-CNT6  UPON  CONS.
*
     DISPLAY  MSG-END   UPON CONS.
*
 END-EXIT.
     EXIT.
********************<<  PROGRAM  END  >>**************************
