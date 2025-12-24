****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　オンライン運用処理　　　　　　　　*
*    モジュール名　　　　：　オンラインデータ分割　　　　　　　*
*    作成日／更新日　　　：　02/01/09                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＣＶＣＳ受信ファイルのデータを分割*
*                        ：　する。　　　　　　　　　　　　　　*
*                        ：  128BYTE ==> 256BYTE               *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            CVCS2561.
 AUTHOR.                NAV.
 DATE-WRITTEN.          02/01/08.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*****<< オンラインデータ>>*****
     SELECT   CVCS256   ASSIGN    TO        CVCS256
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   C256-ST.
*
*****<< 変換ファイル　  >>*****
     SELECT   CVCS128   ASSIGN    TO        CVCS128
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   C128-ST.
*----------------------------------------------------------------*
 DATA                   DIVISION.
*----------------------------------------------------------------*
 FILE                   SECTION.
*
 FD  CVCS256            BLOCK CONTAINS   1  RECORDS.
 01  C256-REC.
     03  C256-REC1     OCCURS    2.
         05  C256-01              PIC       X(128).
*
 FD  CVCS128            BLOCK CONTAINS   1  RECORDS.
 01  C128-REC.
     03  FILLER                   PIC       X(128).
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
     03  C256-ST                  PIC       X(02).
     03  C128-ST                  PIC       X(02).
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                      "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                      "CVCS2561".
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
                        PROCEDURE CVCS256.
     MOVE     "CVCS256"           TO        ERR-FL-ID.
     MOVE     C256-ST             TO        ERR-STCD.
     MOVE     4000                TO        PROGRAM-STATUS.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE CVCS128.
     MOVE     "CVCS128 "          TO        ERR-FL-ID.
     MOVE     C128-ST             TO        ERR-STCD.
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
     OPEN     OUTPUT    CVCS256.
     OPEN     INPUT     CVCS128.
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
     READ     CVCS128
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
     IF   IDX  =  2
          WRITE   C256-REC
          ADD     1          TO       OUT-CNT
          MOVE    ZERO       TO       IDX
          MOVE    SPACE      TO       C256-REC
          INITIALIZE                  C256-REC
     END-IF.
*
     ADD          1          TO       IDX
     MOVE         C128-REC   TO       C256-01(IDX)
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
     IF   IDX  >  ZERO
          WRITE   C256-REC
          ADD     1          TO       OUT-CNT
     END-IF.
*
     DISPLAY  "*** CVCS2561 IN-REC    =" IN-CNT  " ***" UPON CONS.
     DISPLAY  "*** CVCS128 OUT-REC   =" OUT-CNT " ***" UPON CONS.
     MOVE     OUT-CNT      TO    LINK-KENSU.
     CLOSE    CVCS256  CVCS128.
*
 END-EXIT.
     EXIT.
********************<<  PROGRAM  END  >>**************************
