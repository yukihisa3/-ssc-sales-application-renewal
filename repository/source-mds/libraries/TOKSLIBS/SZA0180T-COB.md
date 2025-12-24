# SZA0180T

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SZA0180T.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　在庫マスタ月次繰越                *
*    作成日／更新日　　　：　00/05/15                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　商品在庫マスタの月次繰越処理を　　*
*                          行う                                *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0180T.
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
****<< 在庫マスタ >>************************************
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F021
                                                 ZAI-F022
                                                 ZAI-F031
                                                 ZAI-F032
                                                 ZAI-F033
                        FILE      STATUS    IS   ZAI-STATUS.
****<< 在庫マスタ（実績） >>****************************
     SELECT   ZAMJISF   ASSIGN    TO        DA-01-VI-ZAMJISL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   ZAJ-F01
                                                 ZAJ-F021
                                                 ZAJ-F022
                                                 ZAJ-F031
                                                 ZAJ-F032
                                                 ZAJ-F033
                        FILE      STATUS    IS   ZAJ-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 在庫マスタ >>***************************************
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
****<< 在庫マスタ（実績） >>*******************************
 FD  ZAMJISF.
     COPY     ZAMJISF   OF        XFDLIB
              JOINING   ZAJ       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 ZAI-STATUS           PIC  X(02).
     02 ZAJ-STATUS           PIC  X(02).
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  SHORI-F                 PIC  9(02).
 01  ZAMJISF-INV-FLG         PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  READ-CNT                PIC  9(07)  VALUE  0.
 01  ZAJ-U-CNT               PIC  9(07)  VALUE  0.
 01  ZAI-A-CNT               PIC  9(07)  VALUE  0.
 01  ZAJ-A-CNT               PIC  9(07)  VALUE  0.
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYS-YMD             PIC  9(8).
*
 01  WORK-AREA.
     03  WK-SIMEMM           PIC  9(09)V99.
     03  WK-SIMEMM-R         REDEFINES      WK-SIMEMM.
         05  SIME-YY         PIC  9(07).
         05  SIME-MM         PIC  9(02).
         05  SIME-SS         PIC  9(02).
     03  IX                  PIC  9(02).
*
 01  WK-ZAI-KEY.
     03  WK-ZAI-F01          PIC  X(02).
     03  WK-ZAI-F02          PIC  X(16).
     03  WK-ZAI-F03          PIC  X(06).
 01  WK-ZAJ-KEY.
     03  WK-ZAJ-F01          PIC  X(02).
     03  WK-ZAJ-F02          PIC  X(16).
     03  WK-ZAJ-F03          PIC  X(06).
*
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SZA0180T".
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
 PROCEDURE              DIVISION.
************************************************************
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
                   PROCEDURE  ZAMJISF.
     MOVE   "ZAMJISF "        TO    ERR-FL-ID.
     MOVE    ZAJ-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           INIT-SEC.
     PERFORM           MAIN-SEC
                       UNTIL     END-FLG  =  "END".
     PERFORM           END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      1.0      初期処理                                   *
************************************************************
 INIT-SEC                        SECTION.
     OPEN         INPUT     ZAMZAIF    ZAMJISF.
*システム日付の取得
     MOVE     "3"                 TO   LINK-IN-KBN.
     ACCEPT   LINK-IN-YMD6      FROM   DATE.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-YMD.
*
     PERFORM      ZAI-READ-SEC.
*
 INIT-SEC-EXIT.
     EXIT.
************************************************************
*      2.0      主処理　                                   *
************************************************************
 MAIN-SEC                        SECTION.
*
     MOVE      ZAI-F01    TO   ZAJ-F01.
     MOVE      ZAI-F021   TO   ZAJ-F021.
     MOVE      ZAI-F022   TO   ZAJ-F022.
     MOVE      ZAI-F031   TO   ZAJ-F031.
     MOVE      ZAI-F032   TO   ZAJ-F032.
     MOVE      ZAI-F033   TO   ZAJ-F033.
     PERFORM   ZAJ-READ-SEC.
     IF        ZAMJISF-INV-FLG  =  "INV"
           IF   ZAI-F98  <  20070910
               DISPLAY "KEY1 = " ZAI-F01 ZAI-F021 ZAI-F022
                        ZAI-F031 ZAI-F032  ZAI-F033 UPON CONS
               MOVE    "END"      TO       END-FLG
               GO                 TO       MAIN-SEC-EXIT
           END-IF
     END-IF.
*
     PERFORM   ZAI-READ-SEC.
*
 MAIN-SEC-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                         SECTION.
     CLOSE             ZAMZAIF    ZAMJISF.
     DISPLAY "* READ-CNT = " READ-CNT       UPON CONS.
 END-SEC-EXT.
     EXIT.
*==========================================================*
*            在庫マスタの読込処理
*==========================================================*
 ZAI-READ-SEC                    SECTION.
*
     READ    ZAMZAIF
        AT   END
             MOVE      "END"          TO  END-FLG
             GO                       TO  ZAI-READ-EXT
     END-READ.
*
     ADD          1         TO   READ-CNT.
     IF  READ-CNT(5:3)  =  "000" OR "500"
         DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*
 ZAI-READ-EXT.
     EXIT.
*==========================================================*
*            在庫マスタの読込処理
*==========================================================*
 ZAJ-READ-SEC                    SECTION.
*
     READ    ZAMJISF   INVALID
                       MOVE  "INV"    TO  ZAMJISF-INV-FLG
             NOT INVALID
                       MOVE  SPACE    TO  ZAMJISF-INV-FLG
     END-READ.
*
 ZAJ-READ-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
