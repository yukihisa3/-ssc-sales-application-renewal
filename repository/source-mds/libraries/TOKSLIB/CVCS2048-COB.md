# CVCS2048

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/CVCS2048.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　オンライン運用処理　　　　　　　　*
*    モジュール名　　　　：　オンラインデータ分割　　　　　　　*
*    作成日／更新日　　　：　95/12/15                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＣＶＣＳ受信ファイルのデータを分割*
*                        ：　する。　　　　　　　　　　　　　　*
*                        ：　２０４８_＞２５６ｂｙｔｅ　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            CVCS2048.
 AUTHOR.                TOMIZAWA.
 DATE-WRITTEN.          96/01/09.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*****<< オンラインデータ>>*****
     SELECT   CVCSG001  ASSIGN    TO        CVCSG001
*****SELECT   CVCSG001  ASSIGN    TO        CVCSG001
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   C2048-ST.
*
*****<< 変換ファイル　  >>*****
     SELECT   CVCS256   ASSIGN    TO        CVCS256
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   C256-ST.
*----------------------------------------------------------------*
 DATA                   DIVISION.
*----------------------------------------------------------------*
 FILE                   SECTION.
*
 FD  CVCSG001           BLOCK CONTAINS   1  RECORDS.
 01  C2048-REC.
     03  C2048-REC1     OCCURS    8.
         05  C2048-01             PIC       X(01).
         05  C2048-02             PIC       X(255).
*
 FD  CVCS256            BLOCK CONTAINS   1  RECORDS.
 01  C256-REC.
     03  FILLER                   PIC       X(256).
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
     03  C2048-ST                 PIC       X(02).
     03  C256-ST                  PIC       X(02).
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                      "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                      "CVCS2048".
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
                        PROCEDURE CVCSG001.
     MOVE     "CVCSG001"          TO        ERR-FL-ID.
     MOVE     C2048-ST            TO        ERR-STCD.
     MOVE     4000                TO        PROGRAM-STATUS.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE CVCS256.
     MOVE     "CVCS256 "          TO        ERR-FL-ID.
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
     OPEN     INPUT     CVCSG001.
     OPEN     OUTPUT    CVCS256.
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
     READ     CVCSG001
         AT        END
              MOVE     "END"      TO        END-FLG
         NOT AT    END
              ADD      1          TO        IN-CNT
     END-READ.
*
 CVCSRD-EXIT.
     EXIT.
******************************************************************
*             MAIN      SHORI                         2.0        *
******************************************************************
 MAIN-SEC     SECTION.
*
     PERFORM  VARYING        IDX      FROM      1    BY   1
              UNTIL          IDX      >         8    OR
            ( C2048-01(IDX)  NOT =    "L" AND "D" AND "A" AND
                                      "B" AND "C" )

              MOVE           C2048-REC1(IDX)    TO   C256-REC
              WRITE          C256-REC
              END-WRITE
              ADD            1            TO    OUT-CNT
     END-PERFORM.
*
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
     DISPLAY  "*** CVCS2048 IN-REC    =" IN-CNT  " ***" UPON CONS.
     DISPLAY  "*** CVCS2048 OUT-REC   =" OUT-CNT " ***" UPON CONS.
     CLOSE    CVCSG001  CVCS256.
*
 END-EXIT.
     EXIT.
********************<<  PROGRAM  END  >>**************************

```
