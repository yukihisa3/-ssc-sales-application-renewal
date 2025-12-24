# STN0021B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/STN0021B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＨＨＴ_卸業務　　　　　　　　　　*
*    モジュール名　　　　：　_卸予定ＣＳＶデータ作成　　　　　*
*    作成日／更新日　　　：　2021/03/16                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　パラメタを受取り、_卸予定ＤＴにス*
*                          タートを掛け、_卸予定ＣＳＶを作成　*
*    更新日／更新者　　　：　2021/05/17   INOUE                *
*    変更概要　　　　　　：　ＯＵＴＰＵＴ項目追加　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STN0021B.
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
     SELECT   YTTANAF   ASSIGN    TO        DA-01-VI-YTTANAL3
                        ORGANIZATION        IS   INDEXED
                        RECORD    KEY       IS   YTT-F04
                                                 YTT-F01
                        FILE      STATUS    IS   YTT-STATUS.
****<< _卸予定ＣＳＶデータ　 >>******************************
     SELECT   YTTANACS ASSIGN    TO         DA-01-S-YTTANACS
                        FILE      STATUS    IS   YCS-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< _卸予定データ   >>*************************************
 FD  YTTANAF.
     COPY     YTTANAF   OF        XFDLIB
              JOINING   YTT       PREFIX.
****<<　_卸予定ＣＳＶデータ　　>>*****************************
 FD  YTTANACS          BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      YTTANACS  OF   XFDLIB
                       JOINING   YCS       AS   PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 YTT-STATUS           PIC  X(02).
     02 YCS-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "STN0021B".
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
 LINKAGE                SECTION.
 01  PARA-SOKCD              PIC  X(02).
*
************************************************************
 PROCEDURE              DIVISION USING  PARA-SOKCD.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  YTTANAF.
     MOVE   "YTTANAL3"        TO    ERR-FL-ID.
     MOVE    YTT-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  YTTANACS.
     MOVE   "YTTANACS"        TO    ERR-FL-ID.
     MOVE    YCS-STATUS       TO    ERR-STCD.
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
     OPEN         OUTPUT    YTTANACS.
*
     MOVE    SPACE          TO    YTT-REC.
     INITIALIZE                   YTT-REC.
     MOVE    PARA-SOKCD     TO    YTT-F04.
     START   YTTANAF  KEY  IS  >=  YTT-F04 YTT-F01
             INVALID
             DISPLAY NC"＃＃対象データ無１！！＃＃" UPON CONS
             MOVE   4000    TO    PROGRAM-STATUS
             MOVE   "END"   TO    END-FLG
             GO             TO    100-INIT-END
     END-START.
*
     PERFORM  YTTANAF-READ-SEC.
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
     MOVE    SPACE                TO   YCS-REC.
     INITIALIZE                        YCS-REC.
*カンマセット
     MOVE    ","                  TO   YCS-F001K  YCS-F002K
***** YCS-F003K  YCS-F004K  YCS-F005K  YCS-F006K  YCS-F007K.
*2021/05/17↓
******YCS-F003K  YCS-F004K  YCS-F005K  YCS-F006K.
      YCS-F003K  YCS-F004K  YCS-F005K  YCS-F006K
      YCS-F007K  YCS-F008K.
*2021/05/17↑
*
*項目セット
*    原票ＮＯ
     MOVE    YTT-F01              TO   YCS-F001.
*    場所ＣＤ
     MOVE    YTT-F04              TO   YCS-F002.
*    _番　
     MOVE    YTT-F15              TO   YCS-F003.
*    ＪＡＮＣＤ
     MOVE    YTT-F88              TO   YCS-F004.
*    サカタ商品ＣＤ
     MOVE    YTT-F05              TO   YCS-F005(1:8).
     MOVE    YTT-F06X             TO   YCS-F005(9:8).
*    数量　　　
     IF      YTT-F16  <  ZERO
             MOVE   "-"           TO   YCS-F006F
     ELSE
             MOVE   "0"           TO   YCS-F006F
     END-IF.
     MOVE    YTT-F16              TO   YCS-F006.
*    在庫数＜０の時
     IF      YTT-F16  <  ZERO
             MOVE   "0"           TO   YCS-F006F
             MOVE    ZERO         TO   YCS-F006
     END-IF.
*    削除区分
     MOVE    "0"                  TO   YCS-F007.
*2021/05/17↓
*    日付
     MOVE    YTT-F81              TO   YCS-F008.
*    時刻
     MOVE    YTT-F82              TO   YCS-F009.
*2021/05/17↑
*
     WRITE   YCS-REC.
     ADD     1                    TO   OK-CNT.
*
 MAIN-010.
     PERFORM  YTTANAF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    _卸予定データ読込　　　　　　　　　　　　　　　
************************************************************
 YTTANAF-READ-SEC                   SECTION.
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
     IF   YTT-F04  =  PARA-SOKCD
          CONTINUE
     ELSE
          MOVE  "END"          TO   END-FLG
     END-IF.
*
 YTTANAF-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             YTTANAF  YTTANACS.
*
     DISPLAY "* YTTANAF (INPUT)  = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* YTTANACS(OUTPUT) = " OK-CNT   " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
