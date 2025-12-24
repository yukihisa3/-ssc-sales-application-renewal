# STN0120B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/STN0120B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＨＨＴ_卸業務　　　　　　　　　　*
*    モジュール名　　　　：　_卸予定→_卸確定ＤＴ作成　　　　*
*    作成日／更新日　　　：　2021/03/17                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　_卸予定データより、_卸確定データ*
*                          を作成する。　　　　　　　。　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STN0120B.
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
****<< _卸予定データ　　　　>>*******************************
     SELECT   YTTANAF   ASSIGN    TO        DA-01-VI-YTTANAL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   YTT-F01
                        FILE      STATUS    IS   YTT-STATUS.
****<< _卸確定データ　　　　 >>******************************
     SELECT   KKTANAF  ASSIGN    TO         DA-01-VI-KKTANAL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   KKT-F01
                        FILE      STATUS    IS   KKT-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< _卸予定データ　　　　>>*******************************
 FD  YTTANAF.
     COPY     YTTANAF   OF        XFDLIB
              JOINING   YTT       PREFIX.
****<< _卸確定データ　　　　 >>******************************
 FD  KKTANAF.
     COPY     KKTANAF   OF        XFDLIB
              JOINING   KKT       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 YTT-STATUS           PIC  X(02).
     02 KKT-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "STN0120B".
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
 01  KKTANAF-INV-FLG         PIC  X(03)  VALUE  SPACE.
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
                   PROCEDURE  YTTANAF.
     MOVE   "YTTANAL1"        TO    ERR-FL-ID.
     MOVE    YTT-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  KKTANAF.
     MOVE   "KTTANAL1"        TO    ERR-FL-ID.
     MOVE    KKT-STATUS       TO    ERR-STCD.
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
     OPEN         INPUT     YTTANAF.
     OPEN         I-O       KKTANAF.
*
     PERFORM  YTTANAF-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃対象データ無！！＃＃" UPON CONS
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
     MOVE    SPACE                TO   KKT-REC.
     INITIALIZE                        KKT-REC.
*項目セット
*    レコード店舗
     MOVE    YTT-REC              TO   KKT-REC.
*    数量初期化
     MOVE    ZERO                 TO   KKT-F11
*更新チェック
     PERFORM  KKTANAF-READ-SEC.
     IF  KKTANAF-INV-FLG = "INV"
         WRITE   KKT-REC
         ADD     1                TO   WT-CNT
     ELSE
         REWRITE   KKT-REC
         ADD     1                TO   RW-CNT
     END-IF.
*
 MAIN-010.
     PERFORM  YTTANAF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    ＳＵＢ商品名称マスタ読込　　　　　　　　　　　　
************************************************************
 YTTANAF-READ-SEC                   SECTION.
*
     READ    YTTANAF
        AT   END
             MOVE      "END"     TO   END-FLG
             GO                  TO   YTTANAF-READ-EXT
     END-READ.
*
     ADD     1         TO        IN-CNT.
*
     IF      IN-CNT(5:3)  =  "000" OR "500"
             DISPLAY "ｼｮﾘｹﾝｽｳ = " IN-CNT  UPON CONS
     END-IF.
*
 YTTANAF-READ-EXT.
     EXIT.
************************************************************
*    _卸確定データ（存在チェック）　　　　　　
************************************************************
 KKTANAF-READ-SEC                   SECTION.
*
     READ    KKTANAF
        INVALID
             MOVE      "INV"     TO   KKTANAF-INV-FLG
        NOT  INVALID
             MOVE      SPACE     TO   KKTANAF-INV-FLG
     END-READ.
*
 KKTANAF-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             YTTANAF  KKTANAF.
*
     DISPLAY "* YTTANAF (INPUT)  = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* KKTANAF (WT-CNT) = " WT-CNT   " *"  UPON CONS.
     DISPLAY "* KKTANAF (RW-CNT) = " RW-CNT   " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
