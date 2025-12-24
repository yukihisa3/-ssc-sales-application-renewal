****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＨＨＴ_卸業務　　　　　　　　　　*
*    モジュール名　　　　：　_卸用商品ＣＳＶデータ作成　　　　*
*    作成日／更新日　　　：　2021/03/16                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　_卸用商品データを順読みし、ＨＨＴ*
*                          用の_卸用ＣＳＶデータを出力擦る。　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STN0061B.
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
****<< _卸予定データ   >>************************************
     SELECT   HHTSYOF   ASSIGN    TO        DA-01-VI-HHTSYOL1
                        ORGANIZATION        IS   INDEXED
                        RECORD    KEY       IS   HS1-F01
                        FILE      STATUS    IS   HS1-STATUS.
****<< _卸予定ＣＳＶデータ　 >>******************************
     SELECT   HHTSYOCS ASSIGN    TO         DA-01-S-HHTSYOCS
                        FILE      STATUS    IS   HS2-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< _卸予定データ   >>*************************************
 FD  HHTSYOF.
     COPY     HHTSYOF   OF        XFDLIB
              JOINING   HS1       PREFIX.
****<<　_卸予定ＣＳＶデータ　　>>*****************************
 FD  HHTSYOCS          BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HHTSYOCS  OF   XFDLIB
                       JOINING   HS2       AS   PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 HS1-STATUS           PIC  X(02).
     02 HS2-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "STN0061B".
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
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  OK-CNT              PIC  9(07)   VALUE  0.
     03  SKIP-CNT            PIC  9(07)   VALUE  0.
*
************************************************************
 PROCEDURE              DIVISION.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HHTSYOF.
     MOVE   "HHTSYOL1"        TO    ERR-FL-ID.
     MOVE    HS1-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HHTSYOCS.
     MOVE   "HHTSYOCS"        TO    ERR-FL-ID.
     MOVE    HS2-STATUS       TO    ERR-STCD.
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
     OPEN         INPUT     HHTSYOF.
     OPEN         OUTPUT    HHTSYOCS.
*
     PERFORM  HHTSYOF-READ-SEC.
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
     MOVE    SPACE                TO   HS2-REC.
     INITIALIZE                        HS2-REC.
*カンマセット
     MOVE    ","                  TO   HS2-F001K  HS2-F002K
                                       HS2-F003K  HS2-F004K.
*制御ＣＤセット
     MOVE   X"28"                 TO   HS2-F002S.
     MOVE   X"29"                 TO   HS2-F002E.
*項目セット
*    ＪＡＮＣＤ
     MOVE    HS1-F01              TO   HS2-F001.
*    商品名
     MOVE    HS1-F02              TO   HS2-F002.
*    サカタ商品ＣＤ
     MOVE    HS1-F03              TO   HS2-F003.
*    削除区分
     MOVE    "0"                  TO   HS2-F004.
     WRITE   HS2-REC.
     ADD     1                    TO   OK-CNT.
*
 MAIN-010.
     PERFORM  HHTSYOF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    ＨＨＴ用商品データ読込　　　　　　　　　　　　　
************************************************************
 HHTSYOF-READ-SEC                   SECTION.
     READ    HHTSYOF
        AT   END
             MOVE      "END"     TO   END-FLG
             GO                  TO   HHTSYOF-READ-EXT
     END-READ.
*
     ADD     1         TO        IN-CNT.
*
     IF      IN-CNT(5:3)  =  "000" OR "500"
             DISPLAY "ｼｮﾘｹﾝｽｳ = " IN-CNT  UPON CONS
     END-IF.
*
 HHTSYOF-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             HHTSYOF  HHTSYOCS.
*
     DISPLAY "* HHTSYOF (INPUT)  = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* HHTSYOCS(OUTPUT) = " OK-CNT   " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
