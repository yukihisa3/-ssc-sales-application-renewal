****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　_卸処理　　　　　　　　　　　　　*
*    モジュール名　　　　：　_卸時チェック処理　　            *
*    作成日／更新日　　　：　2019/05/16                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　在庫マスタを読み、商品＋品単で商　*
*　　　　　　　　　　　　　　品変換ＴＢＬを索引し、存在しない　*
*　　　　　　　　　　　　　　場合、エラーデータへの出力を行な　*
*　　　　　　　　　　　　　　う。　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STA0210B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 商品在庫マスタファイル >>****************************
     SELECT   ZAMZAIF    ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-STATUS.
****<< _卸時チェックエラーデータ >>************************
     SELECT   TANERRF    ASSIGN    TO        DA-01-VI-TANERRL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ERR-F01
                                                 ERR-F031
                                                 ERR-F032
                                                 ERR-F05
                        FILE      STATUS    IS   ERR-STATUS.
****<< 倉庫マスタファイル >>********************************
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
*
****<< 商品名称マスタファイル >>****************************
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
*
****<< 商品変換ＴＢＬ >>************************************
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL16
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TBL-F031
                                                 TBL-F0321
                                                 TBL-F0322
                                                 TBL-F0323
                        FILE      STATUS    IS   TBL-STATUS.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< 商品在庫マスタファイル >>****************************
 FD  ZAMZAIF.
     COPY     ZAMZAIF    OF       XFDLIB
              JOINING   ZAI       PREFIX.
****<< _卸時チェックエラーデータ >>************************
 FD  TANERRF.
     COPY     TANERRF    OF       XFDLIB
              JOINING   ERR       PREFIX.
****<< 倉庫マスタファイル >>********************************
 FD  ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
****<< 商品名称マスタファイル >>****************************
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
****<< 商品変換ＴＢＬ >>************************************
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 ZAI-STATUS           PIC  X(2).
     02 ERR-STATUS           PIC  X(2).
     02 SOK-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
     02 TBL-STATUS           PIC  X(2).
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  ZSOKMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  RD-CNT                  PIC  9(07)  VALUE  ZERO.
 01  WT-CNT                  PIC  9(07)  VALUE  ZERO.
 01  SKIP-CNT                PIC  9(07)  VALUE  ZERO.
****  日付保存                ****
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "STA0210B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
************************************************************
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZAMZAIF.
     MOVE   "ZAMZAIF "        TO    ERR-FL-ID.
     MOVE    ZAI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  TANERRF.
     MOVE   "TANERRL1"        TO    ERR-FL-ID.
     MOVE    ERR-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS  "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HMEIMS.
     MOVE   "HMEIMS  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HSHOTBL.
     MOVE   "SHOTBL16"        TO    ERR-FL-ID.
     MOVE    TBL-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
*      0.0       全体処理                                  *
************************************************************
 STA0210B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC        UNTIL  END-FLG = "END".
     PERFORM       END-SEC.
     STOP     RUN.
 STA0210B-END.
     EXIT.
************************************************************
*      1.0      初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       TANERRF.
     OPEN     INPUT     ZAMZAIF  ZSOKMS  HMEIMS  HSHOTBL.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*在庫マスタ読込
     PERFORM  ZAMZAIF-READ-SEC.
*
 INIT-END.
     EXIT.
************************************************************
*      3.0       メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
*商品変換ＴＢＬを索引
     MOVE   ZAI-F021          TO       TBL-F031.
     MOVE   ZAI-F022(1:5)     TO       TBL-F0321.
     MOVE   ZAI-F022(6:2)     TO       TBL-F0322.
     MOVE   ZAI-F022(8:1)     TO       TBL-F0323.
     PERFORM  TBL-READ-SEC.
     IF   HSHOTBL-INV-FLG  =  SPACE
          ADD     1           TO       SKIP-CNT
          GO                  TO       MAIN-010
     END-IF.
*_卸時チェックエラーデータ初期化
     MOVE  SPACE              TO       ERR-REC.
     INITIALIZE                        ERR-REC.
*項目セット
**倉庫ＣＤ
     MOVE   ZAI-F01           TO       ERR-F01.
**倉庫名
     MOVE   ZAI-F01           TO       SOK-F01.
     PERFORM  SOK-READ-SEC.
     IF   ZSOKMS-INV-FLG  =  "INV"
            MOVE  ALL NC"＊"  TO       ERR-F02
     ELSE
            MOVE  SOK-F02     TO       ERR-F02
     END-IF.
**自社商品ＣＤ
     MOVE   ZAI-F021          TO       ERR-F031   MEI-F011.
     MOVE   ZAI-F022(1:5)     TO       ERR-F0321  MEI-F0121.
     MOVE   ZAI-F022(6:2)     TO       ERR-F0322  MEI-F0122.
     MOVE   ZAI-F022(8:1)     TO       ERR-F0323  MEI-F0123.
**商品名
     PERFORM  SHO-READ-SEC.
     IF   HMEIMS-INV-FLG  =  "INV"
            MOVE  ALL NC"＊"  TO       ERR-F041   ERR-F042
     ELSE
            MOVE  MEI-F021    TO       ERR-F041
            MOVE  MEI-F022    TO       ERR-F042
     END-IF.
**_番
     MOVE   ZAI-F03           TO       ERR-F05.
**現在庫数
     MOVE   ZAI-F04           TO       ERR-F06.
*_卸時チェックエラーデータ更新
     WRITE  ERR-REC.
     ADD    1                 TO       WT-CNT.
 MAIN-010.
     PERFORM  ZAMZAIF-READ-SEC.
*
 MAIN-END.
     EXIT.
************************************************************
*          在庫マスタ・読み込み
************************************************************
 ZAMZAIF-READ-SEC       SECTION.
     READ     ZAMZAIF
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   ZAMZAIF-READ-END
     END-READ.
*
     ADD      1              TO   RD-CNT.
     IF  RD-CNT(5:3)  =  "000" OR "500"
         DISPLAY "#RD-CNT = " RD-CNT  UPON CONS
     END-IF.
*
*現在庫数＝０は対象外
     IF       ZAI-F04   =    ZERO
              GO             TO   ZAMZAIF-READ-SEC
     END-IF.
 ZAMZAIF-READ-END.
     EXIT.
************************************************************
*      2.1       倉庫名の取得                              *
************************************************************
 SOK-READ-SEC           SECTION.
*
     READ    ZSOKMS
       INVALID         MOVE  "INV"    TO   ZSOKMS-INV-FLG
       NOT INVALID     MOVE  SPACE    TO   ZSOKMS-INV-FLG
     END-READ.
*
 SOK-READ-END.
     EXIT.
************************************************************
*      2.2       商品名の取得                              *
************************************************************
 SHO-READ-SEC           SECTION.
*
     READ    HMEIMS
       INVALID         MOVE  "INV"    TO   HMEIMS-INV-FLG
       NOT INVALID     MOVE  SPACE    TO   HMEIMS-INV-FLG
     END-READ.
*
 SHO-READ-END.
     EXIT.
************************************************************
*      2.3       商品変換ＴＢＬの存在チェック              *
************************************************************
 TBL-READ-SEC           SECTION.
*
     READ    HSHOTBL
       INVALID         MOVE  "INV"    TO   HSHOTBL-INV-FLG
       NOT INVALID     MOVE  SPACE    TO   HSHOTBL-INV-FLG
     END-READ.
*
 TBL-READ-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
*
     CLOSE    ZAMZAIF  TANERRF  ZSOKMS  HMEIMS  HSHOTBL.
*
     DISPLAY  "# READ-CNT  = " RD-CNT    UPON  CONS.
     DISPLAY  "# WRITE-CNT = " WT-CNT    UPON  CONS.
     DISPLAY  "# SKIP-CNT  = " SKIP-CNT  UPON  CONS.
*
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
