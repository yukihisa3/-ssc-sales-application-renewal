# STSHOTBL

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/STSHOTBL.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　　　　　　　　　　　　　　　　　　*
*    業務名　　　　　　　：　指定取引先変換テーブルコピー　　　*
*    モジュール名　　　　：　指定取引先変換テーブルセット      *
*    作成日／更新日　　　：　01/02/28                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　    *
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            STSHOTBL.
 AUTHOR.                NAV.
 DATE-WRITTEN.          01/02/28.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ユ＆サ商品コード
     SELECT   SHOTBLWK  ASSIGN         DA-01-S-SHOTBLWK
                        ORGANIZATION   SEQUENTIAL
                        ACCESS    MODE SEQUENTIAL
                        STATUS         TBL-ST.
*商品コード変換テーブル
     SELECT   HSHOTBL   ASSIGN         DA-01-VI-SHOTBL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SHO-F01   SHO-F02
                        STATUS         SHO-ST.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*　　ユ＆サ商品コード
******************************************************************
 FD  SHOTBLWK           LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS  48        RECORDS.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*　　商品コード変換テーブル
******************************************************************
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  WORK-FLD.
     03  END-FG              PIC  9(01)     VALUE  ZERO.
     03  TBL-CNT             PIC  9(08)     VALUE  ZERO.
     03  SHO-CNT             PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  HSHOTBL-INV-FLG     PIC  X(08)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE            PIC 9(08).
     03  SYS-DATEP           PIC 9(08) PACKED-DECIMAL.
 01  WK-ST.
     03  TBL-ST            PIC  X(02).
     03  SHO-ST            PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "STSHOTBL".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STSHOTBL".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STSHOTBL".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " WRITE = ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT1.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE "REWRITE= ".
         05  OUT-CNT-R      PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBLWK.
     MOVE      "SHOTBLWK"   TO   AB-FILE.
     MOVE      TBL-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBL.
     MOVE      "HSHOTBL "   TO   AB-FILE.
     MOVE      SHO-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SHOTBLWK.
     OPEN     I-O       HSHOTBL.
*
     MOVE     ZERO      TO        TBL-CNT  SHO-CNT  SKIP-CNT.
*
     PERFORM  TBL-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    商品変換テーブル索引
     MOVE     TBL-F01             TO   SHO-F01.
     MOVE     TBL-F02             TO   SHO-F02.
     READ     HSHOTBL
              INVALID
              MOVE    "INV"       TO   HSHOTBL-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   HSHOTBL-INV-FLG
     END-READ.
*
     IF       HSHOTBL-INV-FLG = "INV"
              MOVE    SPACE     TO    SHO-REC
              INITIALIZE              SHO-REC
              MOVE    TBL-REC   TO    SHO-REC
              WRITE   SHO-REC
              ADD     1         TO    SHO-CNT
     ELSE
              ADD     1         TO    SKIP-CNT
     END-IF.
*
     PERFORM  TBL-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "IN-CNT   = " TBL-CNT  UPON CONS.
     DISPLAY "OUT-CNT  = " SHO-CNT  UPON CONS.
     DISPLAY "SKIP-CNT = " SKIP-CNT UPON CONS.
*
     CLOSE     SHOTBLWK HSHOTBL.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　商品変換ワーク読込み　　　　　　　　　　　　　　*
****************************************************************
 TBL-READ-SEC           SECTION.
*
     MOVE    "TBL-READ-SEC"       TO   S-NAME.
     READ     SHOTBLWK
              AT  END
              MOVE       9      TO    END-FG
              NOT  AT  END
              ADD        1      TO    TBL-CNT
     END-READ.
*
 TBL-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
