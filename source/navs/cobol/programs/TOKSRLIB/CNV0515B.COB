****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　マスタデータコンバート　　　　　　*
*    モジュール名　　　　：　商品変換Ｗ⇒商品変換ＴＢＬにセット*
*    作成日／更新日　　　：　03/07/03                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　商品変換Ｗ⇒商品変換ＴＢＬにセット*
*                          する。                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CNV0515B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 商品変換テーブル >>************************************
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TBL-F01
                                                 TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
****<< 変換済商品変換テーブル >>******************************
     SELECT   TBLWK     ASSIGN    TO        DA-01-S-TBLWK
                        FILE      STATUS    IS   TWK-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 商品変換テーブル >>*************************************
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
****<< 変換後商品変換TBL>>*********************************
 FD  TBLWK
              BLOCK CONTAINS  48  RECORDS.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TWK       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TBL-STATUS           PIC  X(02).
     02 TWK-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "CNV0515B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  HSHOTBL-END             PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  OUT-CNT1            PIC  9(07)   VALUE  0.
     03  OUT-CNT2            PIC  9(07)   VALUE  0.
     03  OUT-CNT3            PIC  9(07)   VALUE  0.
*
************************************************************
 PROCEDURE              DIVISION.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HSHOTBL.
     MOVE   "HSHOTBL "        TO    ERR-FL-ID.
     MOVE    TBL-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  TBLWK.
     MOVE   "TBLWK   "        TO    ERR-FL-ID.
     MOVE    TWK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           100-INIT-SEC.
     PERFORM           200-MAIN-SEC
             UNTIL     END-FLG   =    "END".
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      １００   初期処理                                   *
************************************************************
 100-INIT-SEC           SECTION.
*
     OPEN         INPUT     TBLWK.
     OPEN         I-O       HSHOTBL.
*
     PERFORM  TBLWK-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*
     MOVE  TWK-F01        TO   TBL-F01.
     MOVE  TWK-F02        TO   TBL-F02.
     READ  HSHOTBL
           INVALID      MOVE  "INV"   TO  HSHOTBL-INV-FLG
           NOT INVALID  MOVE  SPACE   TO  HSHOTBL-INV-FLG
     END-READ.
*
     IF    HSHOTBL-INV-FLG = "INV"
           MOVE   SPACE   TO   TBL-REC
           INITIALIZE          TBL-REC
           MOVE   TWK-REC TO   TBL-REC
           WRITE  TBL-REC
           ADD    1       TO   OUT-CNT1
     ELSE
           ADD    1       TO   OUT-CNT3
     END-IF.
*
     PERFORM TBLWK-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            商品変換テーブルの読込処理
************************************************************
 TBLWK-READ-SEC                   SECTION.
     READ   TBLWK
       AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        TBLWK-READ-EXIT
     END-READ.
     ADD     1         TO        IN-CNT.
*
 TBLWK-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             HSHOTBL  TBLWK.
*
     DISPLAY "* TBLWK   (INPUT)  = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* HSHOTBL (OUTPUT) = " OUT-CNT1 " *"  UPON CONS.
     DISPLAY "* HSHOTBL (REWRITE)= " OUT-CNT3 " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
