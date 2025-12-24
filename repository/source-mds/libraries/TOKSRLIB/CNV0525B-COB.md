# CNV0525B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/CNV0525B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　ＳＵＢＷＫ⇒ＳＵＢ変換Ｆにセット　*
*    作成日／更新日　　　：　03/07/03                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＳＵＢＷＫ⇒ＳＵＢ変換ＴＢＬへセッ*
*                          とする。                            *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CNV0525B.
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
     SELECT   SUBTBLF   ASSIGN    TO        DA-01-VI-SUBTBLL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TBL-F01
                                                 TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
****<< 変換済商品変換テーブル >>******************************
     SELECT   SUBWK     ASSIGN    TO        DA-01-S-SUBWK
                        FILE      STATUS    IS   TWK-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 商品変換テーブル >>*************************************
 FD  SUBTBLF.
     COPY     SUBTBLF   OF        XFDLIB
              JOINING   TBL       PREFIX.
****<< 変換後商品変換TBL>>*********************************
 FD  SUBWK
              BLOCK CONTAINS  8   RECORDS.
     COPY     SUBTBLF   OF        XFDLIB
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "CNV0525B".
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
 01  SUBTBLF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  SUBTBLF-END             PIC  X(03)  VALUE  SPACE.
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
                   PROCEDURE  SUBTBLF.
     MOVE   "SUBTBLF "        TO    ERR-FL-ID.
     MOVE    TBL-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBWK.
     MOVE   "SUBWK   "        TO    ERR-FL-ID.
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
     OPEN         INPUT     SUBWK.
     OPEN         I-O       SUBTBLF.
*
     PERFORM  SUBWK-READ-SEC.
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
     READ  SUBTBLF
           INVALID      MOVE  "INV"   TO  SUBTBLF-INV-FLG
           NOT INVALID  MOVE  SPACE   TO  SUBTBLF-INV-FLG
     END-READ.
*
     IF    SUBTBLF-INV-FLG = "INV"
           MOVE   SPACE   TO   TBL-REC
           INITIALIZE          TBL-REC
           MOVE   TWK-REC TO   TBL-REC
           WRITE  TBL-REC
           ADD    1       TO   OUT-CNT1
     ELSE
           ADD    1       TO   OUT-CNT3
     END-IF.
*
     PERFORM SUBWK-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            商品変換テーブルの読込処理
************************************************************
 SUBWK-READ-SEC                   SECTION.
     READ   SUBWK
       AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        SUBWK-READ-EXIT
     END-READ.
     ADD     1         TO        IN-CNT.
*
 SUBWK-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             SUBTBLF  SUBWK.
*
     DISPLAY "* SUBWK   (INPUT)  = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* SUBTBLF (OUTPUT) = " OUT-CNT1 " *"  UPON CONS.
     DISPLAY "* SUBTBLF (REWRITE)= " OUT-CNT3 " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
