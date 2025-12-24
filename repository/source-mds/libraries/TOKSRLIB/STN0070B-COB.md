# STN0070B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/STN0070B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＨＨＴ_卸業務　　　　　　　　　　*
*    モジュール名　　　　：　_卸用担当者データ作成　　　　　　*
*    作成日／更新日　　　：　2021/03/16                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　ＳＵＢ商品名称マスタを順読みし、　*
*                          _卸用商品データを作成する。　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STN0070B.
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
****<< 担当者マスタ　　　　　 >>******************************
     SELECT   HTANMS    ASSIGN    TO        DA-01-VI-TANMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   TAN-F01
                                                 TAN-F02
                        FILE      STATUS    IS   TAN-STATUS.
****<< _卸用担当者データ　　 >>******************************
     SELECT   HHTTANF  ASSIGN    TO         DA-01-VI-HHTTANL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   HTA-F01
                        FILE      STATUS    IS   HTA-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 担当者マスタ　　　　　>>*******************************
 FD  HTANMS.
     COPY     HTANMS    OF        XFDLIB
              JOINING   TAN       PREFIX.
****<< _卸用担当者データ　　 >>*******************************
 FD  HHTTANF.
     COPY     HHTTANF   OF        XFDLIB
              JOINING   HTA       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TAN-STATUS           PIC  X(02).
     02 HTA-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "STN0070B".
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
 01  HHTTANF-INV-FLG         PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  WT-CNT              PIC  9(07)   VALUE  0.
     03  RW-CNT              PIC  9(07)   VALUE  0.
     03  SKIP-CNT            PIC  9(07)   VALUE  0.
*
************************************************************
 PROCEDURE              DIVISION.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HTANMS.
     MOVE   "TANMS1  "        TO    ERR-FL-ID.
     MOVE    TAN-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HHTTANF.
     MOVE   "HHTTANL1"        TO    ERR-FL-ID.
     MOVE    HTA-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
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
     OPEN         INPUT     HTANMS.
     OPEN         I-O       HHTTANF.
*
     PERFORM  HTANMS-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃対象データ無２！！＃＃" UPON CONS
           MOVE   4000    TO    PROGRAM-STATUS
     END-IF.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*レコード初期化　　　　
     MOVE    SPACE                TO   HTA-REC.
     INITIALIZE                        HTA-REC.
*項目セット
*    ＨＨＴ用担当者ＣＤ
     MOVE    TAN-F11              TO   HTA-F01.
*    ＨＨＴ用担当者名　
     MOVE    TAN-F03              TO   HTA-F02.
*    ＮＡＶＳ担当者ＣＤ
     MOVE    TAN-F02              TO   HTA-F03.
*    削除区分
     MOVE    "0"                  TO   HTA-F04.
*更新チェック
     PERFORM  HHTTANF-READ-SEC.
     IF  HHTTANF-INV-FLG = "INV"
         WRITE   HTA-REC
         ADD     1                TO   WT-CNT
     ELSE
         REWRITE   HTA-REC
         ADD     1                TO   RW-CNT
     END-IF.
*
 MAIN-010.
     PERFORM  HTANMS-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    ＳＵＢ商品名称マスタ読込　　　　　　　　　　　　
************************************************************
 HTANMS-READ-SEC                   SECTION.
*
     READ    HTANMS
        AT   END
             MOVE      "END"     TO   END-FLG
             GO                  TO   HTANMS-READ-EXT
     END-READ.
*
     ADD     1         TO        IN-CNT.
*
     IF      IN-CNT(5:3)  =  "000" OR "500"
             DISPLAY "ｼｮﾘｹﾝｽｳ = " IN-CNT  UPON CONS
     END-IF.
*
     IF   TAN-F11  NOT =  SPACE
          CONTINUE
     ELSE
          GO                   TO   HTANMS-READ-SEC
     END-IF.
*
 HTANMS-READ-EXT.
     EXIT.
************************************************************
*    _卸用担当者データ存在チェック
************************************************************
 HHTTANF-READ-SEC                   SECTION.
*
     READ    HHTTANF
        INVALID
             MOVE      "INV"     TO   HHTTANF-INV-FLG
        NOT  INVALID
             MOVE      SPACE     TO   HHTTANF-INV-FLG
     END-READ.
*
 HHTTANF-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             HTANMS  HHTTANF.
*
     DISPLAY "* HTANMS (INPUT)   = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* HHTTANF (WT-CNT) = " WT-CNT   " *"  UPON CONS.
     DISPLAY "* HHTTANF (RW-CNT) = " RW-CNT   " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
