****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　全社在庫管理システム　　　　　　　*
*    モジュール名　　　　：　【本社】在庫マスタ出力　　　　　　*
*    作成日／更新日　　　：　03/07/08                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　在庫マスタを読み、全社在庫マスタへ*
*                          出力を行います。　　　　　　　      *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZE0010B.
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
****<< 在庫マスタ       >>************************************
     SELECT   ZAMZAIF   ASSIGN    TO        ZAMZAIF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   ZAI-STATUS.
****<< 条件ファイル     >>************************************
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-HJYOKEN
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
****<< 全社在庫マスタ >>**************************************
     SELECT   ZENSYASF  ASSIGN    TO        DA-01-S-ZENSYASF
                        FILE      STATUS    IS   ZEN-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 在庫マスタ >>*******************************************
 FD  ZAMZAIF.
 01  ZAI-REC.
     03  ZAI-F01        PIC       X(250).
****<< 条件マスタ >>*******************************************
 FD  HJYOKEN            BLOCK     CONTAINS   6   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****<< 全社在庫マスタ   >>*************************************
 FD  ZENSYASF           BLOCK     CONTAINS  1    RECORDS.
 01  ZEN-REC.
   03  ZEN-F01          PIC       X(004).
   03  ZEN-F02          PIC       X(250).
   03  FILLER           PIC       X(038).
   03  ZEN-F99          PIC       9(008).
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 ZAI-STATUS           PIC  X(02).
     02 JYO-STATUS           PIC  X(02).
     02 ZEN-STATUS           PIC  X(02).
****  部門                    ****
 01  WK-BUMON                PIC  9(04)  VALUE  ZERO.
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(8).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SZE0010B".
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
 01  ZAMZAIF-END             PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  OUT-CNT1            PIC  9(07)   VALUE  0.
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
                   PROCEDURE  HJYOKEN.
     MOVE   "HJYOKEN "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZENSYASF.
     MOVE   "ZENSYASF"        TO    ERR-FL-ID.
     MOVE    ZEN-STATUS       TO    ERR-STCD.
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
     OPEN         INPUT     ZAMZAIF  HJYOKEN.
     OPEN         OUTPUT    ZENSYASF.
*
*    条件ファイル索引（部門コード取得）
     MOVE    99              TO   JYO-F01.
     MOVE    "BUMON"         TO   JYO-F02.
     READ    HJYOKEN
             INVALID
             DISPLAY "HJYOKEN BUMON INVALID" JYO-F01 " - " JYO-F02
                      UPON CONS
                      STOP RUN
             NOT  INVALID
             MOVE     JYO-F04     TO   WK-BUMON
     END-READ.
*    システム日付の取得
     ACCEPT       SYSYMD    FROM     DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYSYMD              TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYSYMD.
*    在庫マスタの読込み
     PERFORM  ZAMZAIF-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*
     MOVE  SPACE          TO   ZEN-REC.
     INITIALIZE                ZEN-REC.
     MOVE  WK-BUMON       TO   ZEN-F01.
     MOVE  ZAI-REC        TO   ZEN-F02.
     MOVE  SYSYMD         TO   ZEN-F99.
     WRITE ZEN-REC.
     ADD   1              TO   OUT-CNT1.
*
     PERFORM ZAMZAIF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            在庫マスタの読込処理
************************************************************
 ZAMZAIF-READ-SEC                   SECTION.
     READ    ZAMZAIF
        NEXT   AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        ZAMZAIF-READ-EXT
     END-READ.
     ADD     1         TO        IN-CNT.
*
 ZAMZAIF-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             ZAMZAIF   HJYOKEN  ZENSYASF.
*
     DISPLAY "* ZAMZAIF (INPUT) = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* ZENSYASF(OUTPUT)= " OUT-CNT1 " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
