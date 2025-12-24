# VDA1191B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/VDA1191B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　ユニディーオンライン　　　　　　　*
*    モジュール名　　　　：　受信ファイル_検収データファイル　*
*    作成日／更新日　　　：　98/04/16                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　受信ファイル_検収データファイルへ*
*                        ：　コピー                            *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            VDA1191B.
 AUTHOR.                TAKAHASHI.
 DATE-WRITTEN.          98/04/16.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*****<< 検収データファイル >>*****
     SELECT   YNKENSYU  ASSIGN    TO        YNKENSYU
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   KEN-STATUS.
*
*****<< 変換ファイル　  >>*****
     SELECT   WORKF     ASSIGN    TO        WORKF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   WK-STATUS.
*----------------------------------------------------------------*
 DATA                   DIVISION.
*----------------------------------------------------------------*
 FILE                   SECTION.
*    FILE = ｼﾕｳｼﾝ ﾃﾞ-ﾀ ﾌｱｲﾙ                                      *
 FD  YNKENSYU           BLOCK CONTAINS   5  RECORDS.
 01  KEN-REC.
     03  KEN01                    PIC       X(128).
     03  KEN02                    PIC       X(072).
*    FILE = ｼｲﾚ ﾃﾞ-ﾀ ﾌｱｲﾙ                                        *
 FD  WORKF              BLOCK CONTAINS   1  RECORDS.
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
     03  KEN-STATUS               PIC       X(02).
     03  WK-STATUS                PIC       X(02).
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                      "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                      "VDA1191B".
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
                        PROCEDURE YNKENSYU.
     MOVE     "YNKENSYU"          TO        ERR-FL-ID.
     MOVE     KEN-STATUS          TO        ERR-STCD.
     MOVE     4000                TO        PROGRAM-STATUS.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE WORKF.
     MOVE     "WORKF "            TO        ERR-FL-ID.
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
     OPEN     INPUT     WORKF.
     OPEN     OUTPUT    YNKENSYU.
*オンラインデータファイル初期ＲＥＡＤ
     PERFORM  WORKRD-SEC.
*
 INIT-EXIT.
     EXIT.
******************************************************************
*             WORKRD    SHORI                         1.1        *
******************************************************************
 WORKRD-SEC   SECTION.
*
     READ     WORKF  AT        END
              MOVE     "END"      TO        END-FLG
     END-READ.
*
 WORKRD-EXIT.
     EXIT.
******************************************************************
*             MAIN      SHORI                         2.0        *
******************************************************************
 MAIN-SEC     SECTION.
*
     MOVE     WORK-REC  TO   KEN01.
     MOVE     SPACE     TO   KEN02.
     WRITE    KEN-REC.
*
     PERFORM  WORKRD-SEC.
*
 MAIN-EXIT.
     EXIT.
******************************************************************
*             END       SHORI                         3.0        *
******************************************************************
 END-SEC      SECTION.
*
     CLOSE    YNKENSYU WORKF.
*
 END-EXIT.
     EXIT.
********************<<  PROGRAM  END  >>**************************

```
