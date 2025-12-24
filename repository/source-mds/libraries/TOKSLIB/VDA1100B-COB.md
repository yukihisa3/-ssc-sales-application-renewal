# VDA1100B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/VDA1100B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　ユニディーオンライン　　　　　　　*
*    モジュール名　　　　：　レコード分割　　　　　　　　　　　*
*    作成日／更新日　　　：　95/12/13                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＣＶＣＳ２５６をＣＶＣＳ１２８に　*
*                        ：　分解する。　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            VDA1100B.
 AUTHOR.                TOMIZAWA.N.
 DATE-WRITTEN.          95/12/13.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*****<< オンラインデータ>>*****
     SELECT   CVCSF     ASSIGN    TO        CVCSF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   CVCS-STATUS.
*
*****<< 変換ファイル　  >>*****
     SELECT   WORK01    ASSIGN    TO        WORK01
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   WK-STATUS.
*----------------------------------------------------------------*
 DATA                   DIVISION.
*----------------------------------------------------------------*
 FILE                   SECTION.
*    FILE = ｼﾕｳｼﾝ ﾃﾞ-ﾀ ﾌｱｲﾙ                                      *
 FD  CVCSF              BLOCK CONTAINS   1  RECORDS.
 01  CVCS-REC.
     03  CVCS01                   PIC       X(128).
     03  CVCS02                   PIC       X(128).
*    FILE = ｼｲﾚ ﾃﾞ-ﾀ ﾌｱｲﾙ                                        *
 FD  WORK01             BLOCK CONTAINS   1  RECORDS.
 01  WORK-REC.
     03  FILLER                   PIC       X(128).
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*----------------------------------------------------------------*
*    エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE SPACE.
*    伝票カウンタ
 01  DEN-CNT                      PIC       9(05)  VALUE ZERO.
*    ステイタス　エリア
 01  STATUS-AREA.
     03  CVCS-STATUS              PIC       X(02).
     03  WK-STATUS                PIC       X(02).
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                      "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                      "VDA1000B".
         05  FILLER               PIC       X(10)  VALUE
                      " ABEND ###".
*
     03  MSG-ABEND2.
         05  FILLER               PIC       X(04)  VALUE
                      "### ".
         05  ERR-FL-ID            PIC       X(08).
         05  FILLER               PIC       X(04)  VALUE
                      " ST-".
         05  ERR-STCD             PIC       X(02).
         05  FILLER               PIC       X(04)  VALUE
                      " ###".
*
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE CVCSF.
     MOVE     "CVCSF"             TO        ERR-FL-ID.
     MOVE     CVCS-STATUS         TO        ERR-STCD.
     MOVE     4000                TO        PROGRAM-STATUS.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE WORK01.
     MOVE     "WORK01 "           TO        ERR-FL-ID.
     MOVE     WK-STATUS           TO        ERR-STCD.
     MOVE     4000                TO        PROGRAM-STATUS.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
 END          DECLARATIVES.
*
******************************************************************
*                       SHORI                         0.0        *
******************************************************************
 CONTROL-START       SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC       UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
     STOP     RUN.
 CONTROL-END.
     EXIT.
******************************************************************
*             INIT      SHORI                         1.0        *
******************************************************************
 INIT-SEC     SECTION.
*
     OPEN     INPUT     CVCSF.
     OPEN     OUTPUT    WORK01.
*オンラインデータファイル初期ＲＥＡＤ
     PERFORM  CVCSRD-SEC.
*
 INIT-EXIT.
     EXIT.
******************************************************************
*             CVCSRD    SHORI                         1.1        *
******************************************************************
 CVCSRD-SEC   SECTION.
*
     READ     CVCSF  AT        END
              MOVE     "END"      TO        END-FLG
     END-READ.
*
 CVCSRD-EXIT.
     EXIT.
******************************************************************
*             MAIN      SHORI                         2.0        *
******************************************************************
 MAIN-SEC     SECTION.
*
     MOVE     CVCS01    TO   WORK-REC
     WRITE    WORK-REC.
*
     IF       CVCS02    =    SPACE
              GO        TO   MAIN010
     END-IF.
*
     MOVE     CVCS02    TO   WORK-REC
     WRITE    WORK-REC.
*
 MAIN010.
*
     PERFORM  CVCSRD-SEC.
*
 MAIN-EXIT.
     EXIT.
******************************************************************
*             END       SHORI                         3.0        *
******************************************************************
 END-SEC      SECTION.
*
     CLOSE    CVCSF  WORK01.
*
 END-EXIT.
     EXIT.
********************<<  PROGRAM  END  >>**************************

```
