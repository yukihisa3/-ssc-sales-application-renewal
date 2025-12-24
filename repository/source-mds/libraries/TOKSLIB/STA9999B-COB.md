# STA9999B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/STA9999B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　_卸原票データ部門セット　　　　　*
*    作成日／更新日　　　：　00/11/27                          *
*    作成者／更新者　　　：　NAV                              *
*    処理概要　　　　　　：　指定部門ＣＤをセットする。       *
*                                                             *
*    変更履歴　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STA9999B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K6500.
 OBJECT-COMPUTER.       FACOM-K6500.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS
         YA        IS   YA
         YB        IS   YB.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  _卸ファイル  >>---*
     SELECT   ZTANADT   ASSIGN    TO        DA-01-VI-ZTANADT1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   TAN-F01
                        FILE      STATUS    IS   TAN-STATUS.
 DATA                   DIVISION.
 FILE                   SECTION.
*---((  _卸ファイル  ))---*
 FD  ZTANADT.
     COPY     ZTANADT   OF        XFDLIB
              JOINING   TAN       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  TAN-STATUS          PIC  X(02).
****  フラグ      ****
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
****　カウンタ    ****
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)  VALUE ZERO.
     02  WRITE-CNT           PIC  9(07)  VALUE ZERO.
     02  PAGE-CNT            PIC  9(07)  VALUE ZERO.
     02  REWT-CNT            PIC  9(07)  VALUE ZERO.
**** 日付／時刻   ****
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "STA9999B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージ ＦＯＲ　ファイル  ***
 01  FILE-ERROR.
     02  FILE-ERR1           PIC  N(10)  VALUE
            NC"_卸ファイル　異常！".
**** 日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
****  ＬＩＮＫ領域  ***
 LINKAGE                     SECTION.
 01  PARA-BUMON              PIC  9(03).
 01  PARA-KAISI              PIC  9(07).
 01  PARA-END                PIC  9(07).
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION   USING   PARA-BUMON
                                                PARA-KAISI
                                                PARA-END.
**
 DECLARATIVES.
**_卸ファイル
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    ZTANADT.
     MOVE     "ZTANADT"          TO   ERR-FL-ID.
     MOVE     TAN-STATUS         TO   ERR-STCD.
     DISPLAY  MSG-ABEND1       UPON   CONS.
     DISPLAY  MSG-ABEND2       UPON   CONS.
     MOVE     4000               TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 FTA00201-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 FTA00201-END.
     EXIT.
****************************************************************
*      1.0　　初期処理
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    I-O             ZTANADT.
*
     PERFORM   ZTANADT-READ-SEC.
*
 INIT-END.
     EXIT.
****************************************************************
*      2.0 　　メイン処理
****************************************************************
 MAIN-SEC                    SECTION.
*部門ＣＤセット
     MOVE    PARA-BUMON   TO    TAN-F02.
*
     REWRITE  TAN-REC.
     ADD     1            TO    REWT-CNT.
*_卸データ読込み
     PERFORM ZTANADT-READ-SEC.
*
 MAIN-END.
     EXIT.
****************************************************************
*      2.1 　　_卸データ読込み
****************************************************************
 ZTANADT-READ-SEC            SECTION.
*
     READ   ZTANADT   AT  END
            MOVE   "END"     TO    END-FLG
            GO               TO    ZTANADT-READ-EXIT
            NOT  AT  END
            ADD     1        TO    READ-CNT
     END-READ.
*
     IF      PARA-KAISI   <=  TAN-F01
     AND     PARA-END     >=  TAN-F01
             CONTINUE
     ELSE
             GO              TO    ZTANADT-READ-SEC
     END-IF.
*
 ZTANADT-READ-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE    ZTANADT.
     DISPLAY   "ﾀﾅｵﾛｼﾌｱｲﾙ     (READ) = "  READ-CNT   UPON   CONS.
     DISPLAY   "ﾀﾅｵﾛｼﾌｱｲﾙ      (OUT) = "  REWT-CNT   UPON   CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
