# NVD0456B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVD0456B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫移動処理　　　　　　　　　　　*
*    モジュール名　　　　：　在庫移動商品情報出力　　　        *
*    作成日／作成者　　　：　2024/02/29 TAKAHASHI              *
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　移動用の商品情報をＣＳＶ形式で出力*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NVD0456B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PRIMERGY6000.
 OBJECT-COMPUTER.       PRIMERGY6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         YA        IS   NIHONGO
         YB        IS   YB
         YB-21     IS   YB-21
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 商品名称マスタ　　   >>******************************
     SELECT   SUBMEIF   ASSIGN  TO   DA-01-VI-SUBMEIL1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  SEQUENTIAL
                        RECORD  KEY          IS  MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE STATUS          IS  MEI-STATUS.
****<< 移動用商品情報　　　　 >>******************************
     SELECT   IDOMEISF  ASSIGN  TO   DA-01-S-IDOMEISF
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   CSV-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< 商品名称マスタ　　　　 >>******************************
 FD    SUBMEIF.
       COPY     SUBMEIF    OF        XFDLIB
                JOINING   MEI  AS   PREFIX.
****<< 移動用商品情報　　　　 >>*****************************
 FD    IDOMEISF.
       COPY     IDOMEISF   OF        XFDLIB
                JOINING  CSV   AS   PREFIX.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 CSV-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
****  ワークエリア　          ****
****  カウンタ                ****
 01  SUBMEIF-CNT              PIC  9(8)   VALUE  ZERO.
 01  IDOMEISF-CNT             PIC  9(8)   VALUE  ZERO.
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
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
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NVD0456B".
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
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBMEIF.
     MOVE   "SUBMEIF  "       TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  IDOMEISF.
     MOVE   "IDOMEISF  "        TO    ERR-FL-ID.
     MOVE    CSV-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
 NVD0456B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 NVD0456B-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
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
*
     OPEN     INPUT     SUBMEIF.
     OPEN     OUTPUT    IDOMEISF.
 010-INIT.
     PERFORM  SUBMEIF-READ-SEC.
*
 INIT-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 SUBMEIF-READ-SEC       SECTION.
*
     READ     SUBMEIF
              AT END        MOVE     "END" TO  END-FLG
                            GO             TO  SUBMEIF-READ-EXIT
              NOT  AT  END  ADD      1     TO  SUBMEIF-CNT
     END-READ.
*
     IF  SUBMEIF-CNT(6:3) =  "000" OR  "500"
         DISPLAY "READ-CNT   = "  SUBMEIF-CNT    UPON CONS
     END-IF.
*
 SUBMEIF-READ-EXIT.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
*初期化
     MOVE     SPACE         TO      CSV-REC.
     INITIALIZE                     CSV-REC.
*カンマ、制御バイトセット
     MOVE     ","           TO      CSV-K01  CSV-K02.
     MOVE    X"28"          TO      CSV-S021 CSV-S031.
     MOVE    X"29"          TO      CSV-S022 CSV-S032.
*
     MOVE    MEI-D01        TO      CSV-F01.
     MOVE    MEI-F021       TO      CSV-F02.
     MOVE    MEI-F022       TO      CSV-F03.
*レコード出力
     WRITE     CSV-REC.
     ADD     1              TO      IDOMEISF-CNT.
*
 010-MAIN.
     PERFORM  SUBMEIF-READ-SEC.
*
 MAIN-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
*
     CLOSE    IDOMEISF  SUBMEIF.

     DISPLAY "*********************"         UPON CONS.
     DISPLAY "READ-CNT   = "  SUBMEIF-CNT    UPON CONS.
     DISPLAY "WRITE-CNT  = "  IDOMEISF-CNT   UPON CONS.
     DISPLAY "*********************"         UPON CONS.
*
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
