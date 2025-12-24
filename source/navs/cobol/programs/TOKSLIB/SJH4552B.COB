****************************************************************
*                                                              *
*    顧客名　　　　　　　：（株）サカタのタネ殿　　　　　　　　*
*    業務名　　　　　　　：　イオンオンラインシステム　　　　*
*    モジュール名　　　　：　支店／営業所別　　振り分け　　　　*
*    作成日／更新日　　　：　2003/04/30                        *
*    作成者／更新者　　　：　NAV                              *
*    処理概要　　　　　　：　店舗ＣＤにより、支店／営業所毎に　*
*　　　　　　　　　　　　　　データ振分処理を行う。　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SJH4552B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          03/04/30.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*****<<受信データイオン>>************************************
     SELECT   CVCS256   ASSIGN    TO        CVCS256
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   CV256-ST.
*
*****<<出力ファイル１>>****************************************
     SELECT   DATAF1    ASSIGN    TO        DATAF1
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   DT1-ST.
*****<<出力ファイル２>>****************************************
     SELECT   DATAF2    ASSIGN    TO        DATAF2
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   DT2-ST.
*****<<出力ファイル３>>****************************************
     SELECT   DATAF3    ASSIGN    TO        DATAF3
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   DT3-ST.
*****<<出力ファイル４>>****************************************
     SELECT   DATAF4    ASSIGN    TO        DATAF4
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   DT4-ST.
*****<<出力ファイル５>>****************************************
     SELECT   DATAF5    ASSIGN    TO        DATAF5
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   DT5-ST.
*****<<出力ファイル６>>****************************************
     SELECT   DATAF6    ASSIGN    TO        DATAF6
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   DT6-ST.
*****<<出力ファイル７>>****************************************
     SELECT   DATAF7    ASSIGN    TO        DATAF7
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   DT7-ST.
*****<<出力ファイル８>>****************************************
     SELECT   DATAF8    ASSIGN    TO        DATAF8
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   DT8-ST.
*****<<出力ファイル９>>****************************************
     SELECT   DATAF9    ASSIGN    TO        DATAF9
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   DT9-ST.
****<<店舗マスタ>>*********************************************
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F52 TEN-F011
                        FILE      STATUS    IS   TEN-ST.
*
 DATA                   DIVISION.
 FILE                   SECTION.
****************************************************************
*　　　　受信データ　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 FD  CVCS256.
 01  CV256-REC.
     03  CV128-REC                OCCURS 2.
         05  CV1-F01              PIC  X(03).
         05  CV1-F02              PIC  X(13).
         05  CV1-F03              PIC  X(02).
         05  CV1-F04              PIC  X(10).
         05  CV1-F05              PIC  X(01).
         05  CV1-F06              PIC  X(99).
*
****************************************************************
*　　出力ファイル１
****************************************************************
 FD  DATAF1.
 01  DATAF1-REC.
     03  FILLER                   PIC  X(128).
*
****************************************************************
*　　出力ファイル２
****************************************************************
 FD  DATAF2.
 01  DATAF2-REC.
     03  FILLER                   PIC  X(128).
*
****************************************************************
*　　出力ファイル３
****************************************************************
 FD  DATAF3.
 01  DATAF3-REC.
     03  FILLER                   PIC  X(128).
*
****************************************************************
*　　出力ファイル４
****************************************************************
 FD  DATAF4.
 01  DATAF4-REC.
     03  FILLER                   PIC  X(128).
*
****************************************************************
*　　出力ファイル５
****************************************************************
 FD  DATAF5.
 01  DATAF5-REC.
     03  FILLER                   PIC  X(128).
*
****************************************************************
*　　出力ファイル６
****************************************************************
 FD  DATAF6.
 01  DATAF6-REC.
     03  FILLER                   PIC  X(128).
*
****************************************************************
*　　出力ファイル７
****************************************************************
 FD  DATAF7.
 01  DATAF7-REC.
     03  FILLER                   PIC  X(128).
*
****************************************************************
*　　出力ファイル８
****************************************************************
 FD  DATAF8.
 01  DATAF8-REC.
     03  FILLER                   PIC  X(128).
*
****************************************************************
*　　出力ファイル９
****************************************************************
 FD  DATAF9.
 01  DATAF9-REC.
     03  FILLER                   PIC  X(128).
*
****************************************************************
*　　店舗マスタ
****************************************************************
 FD  HTENMS.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*
 WORKING-STORAGE        SECTION.
*    エンドフラグ
 01  END-FLG                      PIC  9(01)  VALUE  ZERO.
*    添字
 01  IDX                          PIC  9(01)  VALUE  ZERO.
*    取引先ＣＤ設定
 01  WK-TOKCD                     PIC  9(08)  VALUE  8737.
*    伝票カウンタ
 01  DEN-CNT                      PIC  9(05)  VALUE  ZERO.
*    ＩＮＶＡＬＩＤフラグ
 01  HTENMS-INV-FLG               PIC  X(03)  VALUE  SPACE.
*    支店／営業所区分
 01  WK-TEN-F77                   PIC  9(01)  VALUE  ZERO.
*    ＪＳＡフラグ
 01  FIL-FLG1                     PIC  X(02)  VALUE  SPACE.
 01  FIL-FLG2                     PIC  X(02)  VALUE  SPACE.
 01  FIL-FLG3                     PIC  X(02)  VALUE  SPACE.
 01  FIL-FLG4                     PIC  X(02)  VALUE  SPACE.
 01  FIL-FLG5                     PIC  X(02)  VALUE  SPACE.
 01  FIL-FLG6                     PIC  X(02)  VALUE  SPACE.
 01  FIL-FLG7                     PIC  X(02)  VALUE  SPACE.
 01  FIL-FLG8                     PIC  X(02)  VALUE  SPACE.
 01  FIL-FLG9                     PIC  X(02)  VALUE  SPACE.
*    ＪＳＡレコ－ド退避
 01  WK-JSA-REC                   PIC  X(128) VALUE  SPACE.
*
*    出力件数カウント
 01  WK-CNT-AREA.
     03  WK-CNT1                  PIC  9(06)  VALUE  ZERO.
     03  WK-CNT2                  PIC  9(06)  VALUE  ZERO.
     03  WK-CNT3                  PIC  9(06)  VALUE  ZERO.
     03  WK-CNT4                  PIC  9(06)  VALUE  ZERO.
     03  WK-CNT5                  PIC  9(06)  VALUE  ZERO.
     03  WK-CNT6                  PIC  9(06)  VALUE  ZERO.
     03  WK-CNT7                  PIC  9(06)  VALUE  ZERO.
     03  WK-CNT8                  PIC  9(06)  VALUE  ZERO.
     03  WK-CNT9                  PIC  9(06)  VALUE  ZERO.
*    ステイタス　エリア
 01  STATUS-AREA.
     03  CV256-ST                 PIC  X(02).
     03  DT1-ST                   PIC  X(02).
     03  DT2-ST                   PIC  X(02).
     03  DT3-ST                   PIC  X(02).
     03  DT4-ST                   PIC  X(02).
     03  DT5-ST                   PIC  X(02).
     03  DT6-ST                   PIC  X(02).
     03  DT7-ST                   PIC  X(02).
     03  DT8-ST                   PIC  X(02).
     03  DT9-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
*取引先フォーマット
 01  WK-JSBH-REC.
     03  WK-JSBH.
         05  JSBH-F01             PIC  X(03).
         05  JSBH-F02.
             07  JSBH-F021        PIC  X(06).
             07  JSBH-F022        PIC  X(02).
         05  JSBH-F03             PIC  9(05).
         05  JSBH-F04             PIC  X(02).
         05  JSBH-F05             PIC  9(07).
         05  JSBH-F06             PIC  9(02).
         05  JSBH-F07             PIC  X(01).
         05  JSBH-F08             PIC  X(01).
         05  JSBH-F09             PIC  9(05).
         05  JSBH-F10             PIC  X(06).
         05  JSBH-F11             PIC  X(12).
         05  JSBH-F12             PIC  X(04).
         05  JSBH-F13             PIC  X(01).
         05  JSBH-F14             PIC  X(02).
         05  JSBH-F15.
             07  JSBH-F151        PIC  X(01).
             07  JSBH-F152        PIC  X(02).
         05  JSBH-F16             PIC  X(01).
         05  JSBH-F17             PIC  X(06).
         05  JSBH-F18             PIC  9(06).
         05  JSBH-F19             PIC  X(04).
         05  JSBH-F20             PIC  9(06).
         05  JSBH-F21             PIC  X(12).
         05  JSBH-F22.
             07  JSBH-F221        PIC  X(13).
             07  JSBH-F222        PIC  X(01).
             07  JSBH-F223        PIC  X(01).
         05  JSBH-F23.
             07  JSBH-F231        PIC  X(08).
             07  JSBH-F232        PIC  X(07).
             07  JSBH-F233        PIC  X(01).
*店舗コード変換
 01  WK-TENPO              PIC X(04).
 01  WK-TENPO-R            REDEFINES   WK-TENPO.
     03  WK-TENPO-H        PIC 9(04).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC X(04) VALUE "*** ".
         05  FILLER               PIC X(08) VALUE "SJH4552B".
         05  FILLER               PIC X(10) VALUE " START ***".
     03  MSG-END.
         05  FILLER               PIC X(04) VALUE "*** ".
         05  FILLER               PIC X(08) VALUE "SJH4552B".
         05  FILLER               PIC X(10) VALUE " END   ***".
     03  MSG-ABEND1.
         05  FILLER               PIC X(04) VALUE "### ".
         05  ERR-PG-ID            PIC X(08) VALUE "SJH4552B".
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
         05  FILLER               PIC X(08) VALUE "SJH4552B".
         05  FILLER               PIC X(09) VALUE " READ = ".
         05  MSG-READ-CNT         PIC 9(06) VALUE ZERO.
         05  FILLER               PIC X(04) VALUE " ***".
     03  MSG-OUT.
         05  FILLER               PIC X(04) VALUE "*** ".
         05  FILLER               PIC X(08) VALUE "SJH4552B".
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
 FILEERROR-SEC8         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE DATAF6.
     MOVE     "DATAF6  "          TO        ERR-FL-ID.
     MOVE     DT6-ST              TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC9         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE HTENMS.
     MOVE     "HTENMS  "          TO        ERR-FL-ID.
     MOVE     TEN-ST              TO        ERR-STCD.
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
     OPEN     INPUT     CVCS256   HTENMS.
     OPEN     OUTPUT    DATAF1  DATAF2  DATAF3  DATAF4  DATAF5
                        DATAF6  DATAF7  DATAF8  DATAF9.
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
     PERFORM  VARYING   IDX      FROM      1  BY  1
              UNTIL     IDX      >         2
*          データセット
           MOVE  CV128-REC(IDX)      TO  WK-JSBH-REC
*          ﾍｯﾀﾞｰの場合ﾃﾞｰﾀ退避を行う
           IF    JSBH-F01 = "JSA"
                 MOVE  CV128-REC(IDX) TO WK-JSA-REC
                 MOVE  SPACE         TO  FIL-FLG1 FIL-FLG2
                                         FIL-FLG3 FIL-FLG4
                                         FIL-FLG5 FIL-FLG6
                                         FIL-FLG7 FIL-FLG8
                                         FIL-FLG8
           ELSE
*             ヘッダ行判定
              IF    JSBH-F08  =  "H"
*                   出力支店／営業所番号取得
                    MOVE WK-TOKCD       TO  TEN-F52
                    MOVE JSBH-F10(1:4)  TO  WK-TENPO
                    DISPLAY "WK-TENPO = " WK-TENPO UPON CONS
                    MOVE WK-TENPO-H     TO  TEN-F011
                    PERFORM HTENMS-READ-SEC
                    IF   HTENMS-INV-FLG = SPACE
                         MOVE   TEN-F77 TO  WK-TEN-F77
                    ELSE
                         MOVE   9       TO  WK-TEN-F77
                    END-IF
              END-IF
*             各支店／営業所別ファイルへ出力
              PERFORM DATA-SET-SEC
           END-IF
*
     END-PERFORM.
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
     CLOSE    CVCS256  DATAF1  DATAF2  DATAF3  DATAF4  DATAF5
              DATAF6  DATAF7  DATAF8  DATAF9  HTENMS.
*
     DISPLAY NC"総読込件数　＝　"  MSG-READ-CNT  UPON  CONS.
     DISPLAY NC"東日本第二　＝　"  WK-CNT1  UPON  CONS.
     DISPLAY NC"九州支店　　＝　"  WK-CNT2  UPON  CONS.
     DISPLAY NC"東日本仙台　＝　"  WK-CNT3  UPON  CONS.
     DISPLAY NC"西日本岡山　＝　"  WK-CNT4  UPON  CONS.
     DISPLAY NC"北海道支店　＝　"  WK-CNT5  UPON  CONS.
     DISPLAY NC"西日本支店　＝　"  WK-CNT6  UPON  CONS.
     DISPLAY NC"予備１　　　＝　"  WK-CNT7  UPON  CONS.
     DISPLAY NC"予備２　　　＝　"  WK-CNT8  UPON  CONS.
     DISPLAY NC"指定無し　　＝　"  WK-CNT9  UPON  CONS.
*
     DISPLAY  MSG-END   UPON CONS.
*
 END-EXIT.
     EXIT.
****************************************************************
*                店舗マスタ読込み                              *
****************************************************************
 HTENMS-READ-SEC           SECTION.
*
     READ      HTENMS
               INVALID
               MOVE      "INV"    TO    HTENMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*    支店／営業所別出力処理　　　                              *
****************************************************************
 DATA-SET-SEC              SECTION.
*
     EVALUATE  WK-TEN-F77
         WHEN  1
               IF   FIL-FLG1 = SPACE
                    MOVE   SPACE   TO    DATAF1-REC
                    INITIALIZE           DATAF1-REC
                    MOVE WK-JSA-REC TO   DATAF1-REC
                    WRITE DATAF1-REC
                    ADD  1              TO    WK-CNT1
                    MOVE "OK"       TO   FIL-FLG1
               END-IF
               MOVE SPACE          TO    DATAF1-REC
               INITIALIZE                DATAF1-REC
               MOVE CV128-REC(IDX) TO    DATAF1-REC
               WRITE DATAF1-REC
               ADD  1              TO    WK-CNT1
         WHEN  2
               IF   FIL-FLG2 = SPACE
                    MOVE   SPACE   TO    DATAF2-REC
                    INITIALIZE           DATAF2-REC
                    MOVE WK-JSA-REC TO   DATAF2-REC
                    WRITE DATAF2-REC
                    ADD  1              TO    WK-CNT2
                    MOVE "OK"       TO   FIL-FLG2
               END-IF
               MOVE SPACE          TO    DATAF2-REC
               INITIALIZE                DATAF2-REC
               MOVE CV128-REC(IDX) TO    DATAF2-REC
               WRITE DATAF2-REC
               ADD  1              TO    WK-CNT2
         WHEN  3
               IF   FIL-FLG3 = SPACE
                    MOVE   SPACE   TO    DATAF3-REC
                    INITIALIZE           DATAF3-REC
                    MOVE WK-JSA-REC TO   DATAF3-REC
                    WRITE DATAF3-REC
                    ADD  1              TO    WK-CNT3
                    MOVE "OK"       TO   FIL-FLG3
               END-IF
               MOVE SPACE          TO    DATAF3-REC
               INITIALIZE                DATAF3-REC
               MOVE CV128-REC(IDX) TO    DATAF3-REC
               WRITE DATAF3-REC
               ADD  1              TO    WK-CNT3
         WHEN  4
               IF   FIL-FLG4 = SPACE
                    MOVE   SPACE   TO    DATAF4-REC
                    INITIALIZE           DATAF4-REC
                    MOVE WK-JSA-REC TO   DATAF4-REC
                    WRITE DATAF4-REC
                    ADD  1              TO    WK-CNT4
                    MOVE "OK"       TO   FIL-FLG4
               END-IF
               MOVE SPACE          TO    DATAF4-REC
               INITIALIZE                DATAF4-REC
               MOVE CV128-REC(IDX) TO    DATAF4-REC
               WRITE DATAF4-REC
               ADD  1              TO    WK-CNT4
         WHEN  6
               IF   FIL-FLG6 = SPACE
                    MOVE   SPACE   TO    DATAF6-REC
                    INITIALIZE           DATAF6-REC
                    MOVE WK-JSA-REC TO   DATAF6-REC
                    WRITE DATAF6-REC
                    ADD  1              TO    WK-CNT6
                    MOVE "OK"       TO   FIL-FLG6
               END-IF
               MOVE SPACE          TO    DATAF6-REC
               INITIALIZE                DATAF6-REC
               MOVE CV128-REC(IDX) TO    DATAF6-REC
               WRITE DATAF6-REC
               ADD  1              TO    WK-CNT6
         WHEN  7
               IF   FIL-FLG7 = SPACE
                    MOVE   SPACE   TO    DATAF7-REC
                    INITIALIZE           DATAF7-REC
                    MOVE WK-JSA-REC TO   DATAF7-REC
                    WRITE DATAF7-REC
                    ADD  1              TO    WK-CNT7
                    MOVE "OK"       TO   FIL-FLG7
               END-IF
               MOVE SPACE          TO    DATAF7-REC
               INITIALIZE                DATAF7-REC
               MOVE CV128-REC(IDX) TO    DATAF7-REC
               WRITE DATAF7-REC
               ADD  1              TO    WK-CNT7
         WHEN  8
               IF   FIL-FLG8 = SPACE
                    MOVE   SPACE   TO    DATAF8-REC
                    INITIALIZE           DATAF8-REC
                    MOVE WK-JSA-REC TO   DATAF8-REC
                    WRITE DATAF8-REC
                    ADD  1              TO    WK-CNT8
                    MOVE "OK"       TO   FIL-FLG8
               END-IF
               MOVE SPACE          TO    DATAF8-REC
               INITIALIZE                DATAF8-REC
               MOVE CV128-REC(IDX) TO    DATAF8-REC
               WRITE DATAF8-REC
               ADD  1              TO    WK-CNT8
         WHEN  9  WHEN  0
               IF   FIL-FLG9 = SPACE
                    MOVE   SPACE   TO    DATAF9-REC
                    INITIALIZE           DATAF9-REC
                    MOVE WK-JSA-REC TO   DATAF9-REC
                    WRITE DATAF9-REC
                    ADD  1              TO    WK-CNT9
                    MOVE "OK"       TO   FIL-FLG9
               END-IF
               MOVE SPACE          TO    DATAF9-REC
               INITIALIZE                DATAF9-REC
               MOVE CV128-REC(IDX) TO    DATAF9-REC
               WRITE DATAF9-REC
               ADD  1              TO    WK-CNT9
     END-EVALUATE.
*
 DATA-SET-EXIT.
     EXIT.
********************<<  PROGRAM  END  >>**************************
