# ZTANASET

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/ZTANASET.COB`

## ソースコード

```cobol
*************************************************************
*                                                           *
*    顧客名　　　　　：　（株）サカタのタネ殿　　　　　　 　*
*    業務名　　　　　：　在庫管理システム　　　　　　　     *
*    モジュール名　　：　_卸ファイルセット　               *
*    作成日／更新日　：　93/06/02                           *
*    作成者／更新者　：　ＮＡＶ　　　　　　　　　　　　　   *
*    処理概要　　　　：　外注パンチされた_卸ファイルを     *
*                        読み実際の_卸ファイルにセットする *
*    変更履歴　　　　：  メインファイルレイアウト変更による *
*                        変更(95/05/01 NAV･ASSIST)          *
*************************************************************
 IDENTIFICATION         DIVISION.
*************************************************************
 PROGRAM-ID.            ZTANASET.
 AUTHOR.                NAV.
 DATE-WRITTEN.          93/06/02.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K6500.
 OBJECT-COMPUTER.       K6500.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  ＩＮファイル  >>---*
     SELECT   INFILE    ASSIGN    TO        DA-01-S-INFILE
                        ACCESS    MODE      SEQUENTIAL.
*---<<  _卸ファイル  >>---*
     SELECT   ZTANADT1  ASSIGN    TO        DA-01-VI-ZTANADT1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       IS   TAN-F01
                        FILE      STATUS    IS   TAN-ST.
*
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＩＮファイル  >>---*
 FD  INFILE  BLOCK CONTAINS  3   RECORDS.
****変更****  95/05/01
 01  IN-REC.
     03  IN-DATA1            PIC  X(29).
     03  IN-DATA2            PIC  X(06).
     03  IN-DATA3            PIC  X(06).
     03  IN-DATA4            PIC  9(08)V999.
     03  IN-DATA5            PIC  9(07)V99.
     03  IN-DATA6            PIC  9(02).
     03  IN-DATA7            PIC  X(10).
     03  IN-DATA8            PIC  X(07).
*---<<  _卸ファイル  >>---*
 FD  ZTANADT1.
 01  TAN-REC.
     03  TAN-DATA1.
         05  TAN-F01         PIC  9(07).
         05  TAN-DATA1X      PIC  X(22).
     03  TAN-DATA2           PIC  X(05).
     03  TAN-DATA3.
         05  TAN-F10         PIC  X(03).
         05  TAN-F11         PIC S9(07)V99.
         05  TAN-F12         PIC  9(07)V99.
         05  TAN-F13         PIC  9(02).
         05  TAN-F14         PIC  X(10).
         05  TAN-F15         PIC  X(06).
         05  TAN-F16         PIC  9(08)V99  PACKED-DECIMAL.
         05  TAN-F99         PIC  X(01).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  STATUS-AREA.
     03  TAN-ST              PIC  X(02).
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     03  IN-CNT              PIC  9(06)  VALUE ZERO.
     03  DUP-CNT             PIC  9(06)  VALUE ZERO.
     03  OUT-CNT             PIC  9(06)  VALUE ZERO.
 01  MSG-AREA1-1.
     03  MSG-ABEND1.
       05  FILLER            PIC  X(04)  VALUE "### ".
       05  ERR-PG-ID         PIC  X(08)  VALUE "ZTANASET".
       05  FILLER            PIC  X(10)  VALUE " ABEND ###".
     03  MSG-ABEND2.
       05  FILLER            PIC  X(04)  VALUE "### ".
       05  ERR-FL-ID         PIC  X(08).
       05  FILLER            PIC  X(04)  VALUE " ST-".
       05  ERR-STCD          PIC  X(02).
       05  FILLER            PIC  X(04)  VALUE " ###".
*--------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                     *
*--------------------------------------------------------------*
 PROCEDURE               DIVISION.
**
*DECLARATIVES.
*FILEERR-SEC1        SECTION.
*    USE AFTER       EXCEPTION
*                    PROCEDURE    ZTANADT1.
*    MOVE     "ZTANADT1"     TO   ERR-FL-ID.
*    MOVE      TAN-ST        TO   ERR-STCD.
*    DISPLAY   MSG-ABEND1    UPON   CONS.
*    DISPLAY   MSG-ABEND2    UPON   CONS.
*    MOVE      4000          TO   PROGRAM-STATUS.
*    STOP      RUN.
*END     DECLARATIVES.
****************************************************************
 ZTANASET-START              SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 ZTANASET-END.
     EXIT.
****************************************************************
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     INFILE.
     OPEN     I-O       ZTANADT1.
     MOVE     SPACE          TO   END-FLG.
     MOVE     ZERO           TO   IN-CNT.
     MOVE     ZERO           TO   OUT-CNT.
     READ     INFILE
          AT END
             MOVE  "END"       TO   END-FLG
          NOT AT END
             ADD    1          TO   IN-CNT
     END-READ.
 INIT-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*
***** 変更 ***** 95/05/01     転送方法変更
     MOVE     IN-DATA1       TO   TAN-DATA1.
     MOVE     IN-DATA2       TO   TAN-DATA2.
     MOVE     IN-DATA3       TO   TAN-F15.
     MOVE     IN-DATA4       TO   TAN-F11.
     MOVE     IN-DATA5       TO   TAN-F12.
     MOVE     IN-DATA6       TO   TAN-F13.
     MOVE     IN-DATA7       TO   TAN-F14.
     MOVE     ZERO           TO   TAN-F16.
     MOVE     SPACE          TO   TAN-F99.
     MOVE     SPACE          TO   TAN-F10.
*
     WRITE    TAN-REC.
**** DISPLAY "ST= " TAN-ST UPON CONS.
     IF  TAN-ST   =   "22"
        ADD   1         TO   DUP-CNT
        DISPLAY "** ｼﾞｭｳﾌｸ KEY= " IN-DATA1(1:6) "-" IN-DATA1(7:1)
     ELSE
         ADD   1         TO   OUT-CNT
     END-IF.
***
* ＩＮファイルＲＥＡＤ
     READ     INFILE
          AT END
             MOVE  "END"       TO   END-FLG
          NOT AT END
             ADD    1          TO   IN-CNT
     END-READ.
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     DISPLAY "***   INPUT   DATA = "  IN-CNT    UPON  CONS.
     DISPLAY "***   INVALID DATA = "  DUP-CNT   UPON  CONS.
     DISPLAY "***   OUTPUT1 DATA = "  OUT-CNT   UPON  CONS.
     CLOSE    INFILE     ZTANADT1.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
