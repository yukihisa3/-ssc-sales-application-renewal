# DTH64256

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/DTH64256.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ営業第２部　　　*
*    業務名　　　　　　　：　オンライン運用処理　　　　　　　　*
*    モジュール名　　　　：　オンラインデータ結合　　　　　　　*
*    作成日／更新日　　　：　2021/10/12                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　送信ＤＴより送信する形式に結合する*
*                        ：   64BYTE ==> 256BYTE               *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            DTH64256.
 AUTHOR.                NAV.
 DATE-WRITTEN.          02/01/08.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*****<< オンラインデータ>>*****
     SELECT   DTH64     ASSIGN    TO        DTH64
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   C64-ST.
*
*****<< 変換ファイル　  >>*****
     SELECT   DTH256    ASSIGN    TO        DTH256
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   C256-ST.
*----------------------------------------------------------------*
 DATA                   DIVISION.
*----------------------------------------------------------------*
 FILE                   SECTION.
*
 FD  DTH64             BLOCK CONTAINS   1  RECORDS.
 01  C64-REC.
     03  FILLER                   PIC       X(064).
*
 FD  DTH256           BLOCK CONTAINS   1  RECORDS.
 01  C256-REC.
     03  C256-REC1     OCCURS    4.
         05  C256-01             PIC       X(064).
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*----------------------------------------------------------------*
*    エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE SPACE.
*    インデックス
 01  IDX                          PIC       9(02)  VALUE ZERO.
*    カウンタ
 01  IN-CNT                       PIC       9(06)  VALUE ZERO.
 01  OUT-CNT                      PIC       9(06)  VALUE ZERO.
*    ステイタス　エリア
 01  STATUS-AREA.
     03  C64-ST                   PIC       X(02).
     03  C256-ST                  PIC       X(02).
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                      "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                      "DTH64256".
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
 LINKAGE                          SECTION.
 01  LINK-KENSU                   PIC       9(07).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING  LINK-KENSU.
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE DTH64.
     MOVE     "DHT64"             TO        ERR-FL-ID.
     MOVE     C64-ST              TO        ERR-STCD.
     MOVE     4000                TO        PROGRAM-STATUS.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE DTH256.
     MOVE     "DTH256 "           TO        ERR-FL-ID.
     MOVE     C256-ST             TO        ERR-STCD.
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
     OPEN     INPUT     DTH64.
     OPEN     OUTPUT    DTH256.
*
     MOVE    SPACE      TO       C256-REC.
     INITIALIZE                  C256-REC.
*オンラインデータファイル初期ＲＥＡＤ
     PERFORM  DHT64-READ-SEC.
*
 INIT-EXIT.
     EXIT.
******************************************************************
*             CVCSRD    SHORI                         1.1        *
******************************************************************
 DHT64-READ-SEC   SECTION.
*
     READ     DTH64
         AT        END
              MOVE     "END"      TO        END-FLG
         NOT AT    END
              ADD      1          TO        IN-CNT
     END-READ.
*
 DHT64-READ-EXIT.
     EXIT.
******************************************************************
*             MAIN      SHORI                         2.0        *
******************************************************************
 MAIN-SEC     SECTION.
*
     IF   IDX  =  4
          WRITE   C256-REC
          ADD     1          TO       OUT-CNT
          MOVE    ZERO       TO       IDX
          MOVE    SPACE      TO       C256-REC
          INITIALIZE                  C256-REC
     END-IF.
*
     ADD          1          TO       IDX
     MOVE         C64-REC    TO       C256-01(IDX)
*
     PERFORM  DHT64-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
******************************************************************
*             END       SHORI                         3.0        *
******************************************************************
 END-SEC      SECTION.
*
     IF   IDX  >  ZERO
          WRITE   C256-REC
          ADD     1          TO       OUT-CNT
     END-IF.
*
     DISPLAY  "*** DTH64  IN-REC    =" IN-CNT  " ***" UPON CONS.
     DISPLAY  "*** DTH256 OUT-REC   =" OUT-CNT " ***" UPON CONS.
     MOVE     OUT-CNT      TO    LINK-KENSU.
     CLOSE    DTH64  DTH256.
*
 END-EXIT.
     EXIT.
********************<<  PROGRAM  END  >>**************************

```
