# SOKCNV06

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SOKCNV06.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　　　　　　　　　　　　　　　　　　*
*    業務名　　　　　　　：　入庫ファイル　　　　　　　　　　　*
*    モジュール名　　　　：　倉庫コード変換処理　　            *
*    作成日／更新日　　　：　00/11/29                          *
*    作成者／更新者　　　：　ＮＡＶ金子　　　　　　　　　　　　*
*    処理概要　　　　　　：　倉庫コード　６２→６Ａ　　　　    *
*                            　　　　　　８７→６０　　　　    *
*                            に変換する　　　　                *
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SOKCNV06.
 AUTHOR.                NAV N.KANEKO.
 DATE-WRITTEN.          00/11/29.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*入庫ファイル　
     SELECT   NYKFILF   ASSIGN    TO        DA-01-VI-NYKFILL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY  NYK-F02   NYK-F03
                                       NYK-F04   NYK-F05
                        FILE      STATUS    IS   NYK-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*　　入庫ファイル　
******************************************************************
 FD  NYKFILF            LABEL RECORD   IS   STANDARD.
     COPY     NYKFILF   OF        XFDLIB
              JOINING   NYK       PREFIX.
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  CHK-FLG                 PIC  X(01)     VALUE  SPACE.
 01  CHG-FLG                 PIC  X(01)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE            PIC 9(06).
     03  SYS-DATEW           PIC 9(08).
 01  WK-ST.
     03  NYK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SOKCNV06".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SOKCNV06".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SOKCNV06".
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
         05  FILLER         PIC   X(09)  VALUE " REWRT = ".
         05  OUT-CNT        PIC   9(06).
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
                        PROCEDURE   NYKFILF.
     MOVE      "NYKFILF"    TO   AB-FILE.
     MOVE      NYK-STATUS   TO   AB-STS.
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
     OPEN     I-O       NYKFILF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
     PERFORM  NYK-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
     MOVE     SPACE               TO   CHG-FLG.
*
     EVALUATE NYK-F27
         WHEN "62"
              MOVE "6A"           TO   NYK-F27
              MOVE "1"            TO   CHG-FLG
         WHEN "87"
              MOVE "60"           TO   NYK-F27
              MOVE "1"            TO   CHG-FLG
     END-EVALUATE.
*
     EVALUATE NYK-F32
         WHEN "62"
              MOVE "6A"           TO   NYK-F32
              MOVE "1"            TO   CHG-FLG
         WHEN "87"
              MOVE "60"           TO   NYK-F32
              MOVE "1"            TO   CHG-FLG
     END-EVALUATE.
*
     IF  CHG-FLG   NOT  =    SPACE
         REWRITE  NYK-REC
         ADD      1          TO   WRT-CNT
     END-IF.
*
     PERFORM  NYK-READ-SEC.
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
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     NYKFILF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　入庫ファイル　　　読む　　　　　　　　　　　　　*
****************************************************************
 NYK-READ-SEC           SECTION.
*
     READ     NYKFILF
              AT   END
              MOVE      9         TO   END-FG
         NOT  AT   END
              ADD       1         TO   RD-CNT
     END-READ.
     IF  RD-CNT(6:3)    =    "000"     OR   "500"
         DISPLAY   "ｹﾞﾝｻﾞｲｼｮﾘｹﾝｽｳ = " RD-CNT UPON CONS
     END-IF.
*
 NYK-READ-SEC-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
