****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＨＨＴ_卸業務　　　　　　　　　　*
*    モジュール名　　　　：　_卸用商品データ作成　　　　　　　*
*    作成日／更新日　　　：　2021/03/16                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　ＳＵＢ商品名称マスタを順読みし、　*
*                          _卸用商品データを作成する。　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STN0060B.
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
****<< ＳＵＢ商品名称マスタ　>>*******************************
     SELECT   SUBMEIF   ASSIGN    TO        DA-01-VI-SUBMEIL7
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   MEI-D01
                        FILE      STATUS    IS   MEI-STATUS.
****<< _卸用商品データ　　　 >>******************************
     SELECT   HHTSYOF  ASSIGN    TO         DA-01-VI-HHTSYOL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SYO-F01
                        FILE      STATUS    IS   SYO-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< ＳＵＢ商品名称マスタ　>>*******************************
 FD  SUBMEIF.
     COPY     SUBMEIF   OF        XFDLIB
              JOINING   MEI       PREFIX.
****<< _卸用商品データ　　　 >>*******************************
 FD  HHTSYOF.
     COPY     HHTSYOF   OF        XFDLIB
              JOINING   SYO       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 MEI-STATUS           PIC  X(02).
     02 SYO-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "STN0060B".
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
 01  HHTSYOF-INV-FLG         PIC  X(03)  VALUE  SPACE.
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
                   PROCEDURE  SUBMEIF.
     MOVE   "SUBMEIL1"        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HHTSYOF.
     MOVE   "HHTSYOL1"        TO    ERR-FL-ID.
     MOVE    SYO-STATUS       TO    ERR-STCD.
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
     OPEN         INPUT     SUBMEIF.
     OPEN         I-O       HHTSYOF.
*
     PERFORM  SUBMEIF-READ-SEC.
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
     MOVE    SPACE                TO   SYO-REC.
     INITIALIZE                        SYO-REC.
*項目セット
*    ＪＡＮＣＤ
     MOVE    MEI-D01              TO   SYO-F01.
*    商品名　　
     MOVE    MEI-F021             TO   SYO-F02(1:15).
     MOVE    MEI-F022             TO   SYO-F02(16:15).
*    サカタ商品ＣＤ
     MOVE    MEI-F011             TO   SYO-F03(1:8).
     MOVE    MEI-F012             TO   SYO-F03(9:8).
*    削除区分
     MOVE    "0"                  TO   SYO-F04.
*更新チェック
     PERFORM  HHTSYOF-READ-SEC.
     IF  HHTSYOF-INV-FLG = "INV"
         WRITE   SYO-REC
         ADD     1                TO   WT-CNT
     ELSE
         REWRITE   SYO-REC
         ADD     1                TO   RW-CNT
     END-IF.
*
 MAIN-010.
     PERFORM  SUBMEIF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    ＳＵＢ商品名称マスタ読込　　　　　　　　　　　　
************************************************************
 SUBMEIF-READ-SEC                   SECTION.
*
     READ    SUBMEIF
        AT   END
             MOVE      "END"     TO   END-FLG
             GO                  TO   SUBMEIF-READ-EXT
     END-READ.
*
     ADD     1         TO        IN-CNT.
*
     IF      IN-CNT(5:3)  =  "000" OR "500"
             DISPLAY "ｼｮﾘｹﾝｽｳ = " IN-CNT  UPON CONS
     END-IF.
*
     IF   MEI-D01  NOT =  SPACE
          CONTINUE
     ELSE
          GO                   TO   SUBMEIF-READ-SEC
     END-IF.
*
 SUBMEIF-READ-EXT.
     EXIT.
************************************************************
*    _卸用商品データ読込（存在チェック）　　　　　　
************************************************************
 HHTSYOF-READ-SEC                   SECTION.
*
     READ    HHTSYOF
        INVALID
             MOVE      "INV"     TO   HHTSYOF-INV-FLG
        NOT  INVALID
             MOVE      SPACE     TO   HHTSYOF-INV-FLG
     END-READ.
*
 HHTSYOF-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             SUBMEIF  HHTSYOF.
*
     DISPLAY "* SUBMEIF (INPUT)  = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* HHTSYOF (WT-CNT) = " WT-CNT   " *"  UPON CONS.
     DISPLAY "* HHTSYOF (RW-CNT) = " RW-CNT   " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
