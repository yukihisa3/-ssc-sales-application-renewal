# SBT0630B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBT0630B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　カインズ出荷梱包データ取込　　　　*
*    モジュール名　　　　：　出荷梱包データレコード長編集　　　*
*    作成日／更新日　　　：　14/09/05                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　取込んだカインズ出荷梱包データを　*
*                        ：　指定レコード長に変換する。　　　　*
*                        ：  RL=1250 ==> RL=1200               *
*    作成日／更新日　　　：　14/10/08                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　出力時、１１９８バイト目に１を　　*
*                        ：　セットするように変更する。　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SBT0630B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/09/05.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*****<< 出荷梱包データ（取込）>>*****
     SELECT   CNZLNKWK ASSIGN    TO        CNZLNKWK
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   WRK-ST.
*
*****<< 出荷梱包データ（編集）>>*****
     SELECT   CNZLNKF   ASSIGN    TO        CNZLNKF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   LNK-ST.
*----------------------------------------------------------------*
 DATA                   DIVISION.
*----------------------------------------------------------------*
 FILE                   SECTION.
*
 FD  CNZLNKWK          BLOCK CONTAINS   3  RECORDS.
 01  WRK-REC.
     03  WRK01                    PIC       X(1200).
     03  WRK02                    PIC       X(50).
*
 FD  CNZLNKF            BLOCK CONTAINS   3  RECORDS.
 01  LNK-REC.
     03  LNK01                    PIC       X(1200).
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
     03  WRK-ST                   PIC       X(02).
     03  LNK-ST                   PIC       X(02).
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                      "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                      "SBT0630B".
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
                        PROCEDURE CNZLNKWK.
     MOVE     "CNZLNKWK"          TO        ERR-FL-ID.
     MOVE     WRK-ST              TO        ERR-STCD.
     MOVE     4000                TO        PROGRAM-STATUS.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE CNZLNKF.
     MOVE     "CNZLNKF"           TO        ERR-FL-ID.
     MOVE     LNK-ST              TO        ERR-STCD.
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
     OPEN     INPUT     CNZLNKWK.
     OPEN     OUTPUT    CNZLNKF.
*オンラインデータファイル初期ＲＥＡＤ
     PERFORM  CNZLNKWK-READ-SEC.
*
 INIT-EXIT.
     EXIT.
******************************************************************
*             CVCSRD    SHORI                         1.1        *
******************************************************************
 CNZLNKWK-READ-SEC   SECTION.
*
     READ     CNZLNKWK
         AT        END
              MOVE     "END"      TO        END-FLG
         NOT AT    END
              ADD      1          TO        IN-CNT
     END-READ.
*
 CNZLNKWK-READ-EXIT.
     EXIT.
******************************************************************
*             MAIN      SHORI                         2.0        *
******************************************************************
 MAIN-SEC     SECTION.
*
     MOVE     SPACE          TO       LNK-REC.
     INITIALIZE                       LNK-REC.
*
     MOVE     WRK01          TO       LNK-REC.
     MOVE     "1"            TO       LNK01(1198:1).
     WRITE    LNK-REC.
     ADD      1              TO       OUT-CNT.
*
     PERFORM  CNZLNKWK-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
******************************************************************
*             END       SHORI                         3.0        *
******************************************************************
 END-SEC      SECTION.
*
     DISPLAY  "*** INFILE   =" IN-CNT  " ***" UPON CONS.
     DISPLAY  "*** OUTFILE  =" OUT-CNT " ***" UPON CONS.
     CLOSE    CNZLNKWK  CNZLNKF.
*
 END-EXIT.
     EXIT.
********************<<  PROGRAM  END  >>**************************

```
