# VDA2400B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/VDA2400B.COB`

## ソースコード

```cobol
**************************************************************
* システム名  :カーマオンラインシステム                      *
* プログラム名:受信データ分割（２５６→１２８）              *
* ＰＧ－ＩＤ  :VDA2400B                                      *
* 作成日      :1999/02/04                                    *
* 作成者      :T.TAKAHASHI                                   *
**************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            VDA2400B.
 AUTHOR.                T.TAKAHASHI.
 DATE-WRITTEN.          99/02/04.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*****<< 受信データファイル >>*****
     SELECT   INFILE    ASSIGN    TO        INFILE
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   IN-STATUS.
*
*****<< 分割ファイル       >>*****
     SELECT   OUTFILE   ASSIGN    TO        OUTFILE
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   OUT-STATUS.
*----------------------------------------------------------------*
 DATA                   DIVISION.
*----------------------------------------------------------------*
 FILE                   SECTION.
*****<< 受信データファイル >>*****
 FD  INFILE             BLOCK CONTAINS  15  RECORDS.
 01  IN-REC.
     03  IN-F01                   PIC       X(128).
     03  IN-F02                   PIC       X(128).
*****<< 分割ファイル       >>*****
 FD  OUTFILE            BLOCK CONTAINS   1  RECORDS.
 01  OUT-REC.
     03  FILLER                   PIC       X(128).
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*----------------------------------------------------------------*
*    エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE SPACE.
*    伝票カウンタ
 01  IN-CNT                       PIC       9(06)  VALUE ZERO.
 01  OUT-CNT                      PIC       9(06)  VALUE ZERO.
*    ステイタス　エリア
 01  STATUS-AREA.
     03  IN-STATUS                PIC       X(02).
     03  OUT-STATUS               PIC       X(02).
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                      "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                      "VDA2400B".
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
     USE AFTER          EXCEPTION           PROCEDURE INFILE.
     MOVE     "INFILE"            TO        ERR-FL-ID.
     MOVE     IN-STATUS           TO        ERR-STCD.
     MOVE     4000                TO        PROGRAM-STATUS.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION           PROCEDURE OUTFILE.
     MOVE     "OUTFILE"           TO        ERR-FL-ID.
     MOVE     OUT-STATUS          TO        ERR-STCD.
     MOVE     4000                TO        PROGRAM-STATUS.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
 END          DECLARATIVES.
*
******************************************************************
*    0.0      コントロール
******************************************************************
 CONTROL-START       SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC       UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
     STOP     RUN.
 CONTROL-END.
     EXIT.
******************************************************************
*    1.0      初期処理
******************************************************************
 INIT-SEC               SECTION.
*ファイルＯＰＥＮ
     OPEN     INPUT     INFILE.
     OPEN     OUTPUT    OUTFILE.
*ワーククリア
     MOVE     ZERO      TO     IN-CNT OUT-CNT.
*受信データ初期ＲＥＡＤ
     PERFORM  INFILE-READ-SEC.
*
 INIT-EXIT.
     EXIT.
******************************************************************
*    1.1      受信データ初期ＲＥＡＤ
******************************************************************
 INFILE-READ-SEC        SECTION.
*
     READ     INFILE    AT    END
              MOVE     "END"      TO        END-FLG
              NOT AT END
              ADD       1         TO        IN-CNT
     END-READ.
*
 INFILE-READ-EXIT.
     EXIT.
******************************************************************
*    2.0      メイン処理
******************************************************************
 MAIN-SEC                SECTION.
*受信データ（１～１２８目レコード作成）
     MOVE     IN-F01    TO   OUT-REC.
     WRITE    OUT-REC.
*作成件数アウントアップ
     ADD      1         TO   OUT-CNT.
*受信データ（１２９－２５６バイト目がスペースか判断）
     IF       IN-F02    =    SPACE
              GO        TO   MAIN010
     END-IF.
*受信データ（１２９～２５６目レコード作成）
     MOVE     IN-F02    TO   OUT-REC.
     WRITE    OUT-REC.
*作成件数アウントアップ
     ADD      1         TO   OUT-CNT.
*
 MAIN010.
     PERFORM  INFILE-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
******************************************************************
*    3.0      終了処理
******************************************************************
 END-SEC                SECTION.
*ファイルクローズ
     CLOSE    INFILE    OUTFILE.
*件数コンソール表示
     DISPLAY "INFILE READ-CNT = " IN-CNT  UPON  CONS.
     DISPLAY "OUTFILE-WT-CNT  = " OUT-CNT UPON  CONS.
     IF       IN-CNT  =  ZERO
              DISPLAY NC"受信データなし"  UPON CONS
     END-IF.
*
 END-EXIT.
     EXIT.
********************<<  PROGRAM  END  >>**************************

```
