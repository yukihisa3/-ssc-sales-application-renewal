# NKE0410B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE0410B.COB`

## ソースコード

```cobol
****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　棚卸
*    サブシステム　　　　：　検品システム　　　　　　　　　
*    モジュール名　　　　：　棚卸予定データ抽出
*    作成日／作成者　　　：　2018/12/20 INOUE
*    処理概要　　　　　　：　受け取ったパラメタ（倉庫コード）
*                            に該当するデータを抽出する。
*    更新日／更新者      ：  2019/02/01 INOUE
*                            倉庫棚卸データレイアウト変更(2版)
*                            にともなう転送項目変更
*　
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE0410B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2018/12/20.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YA        IS   YA
         YB-21     IS   YB-21
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*棚卸原票データ
     SELECT   ZTANADT3  ASSIGN    TO        DA-01-VI-ZTANADT3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       ZTA-F04  ZTA-F01
                        FILE      STATUS    IS   ZTA-STATUS.
*倉庫棚卸データ
     SELECT   SNDTANXX  ASSIGN    TO        SNDTANXX
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    IS   SND-STATUS.
*倉庫棚卸データ（件数）
     SELECT   SNDTAKXX  ASSIGN    TO        SNDTAKXX
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    IS   TAK-STATUS.
*条件ファイル
     SELECT   JYOKEN1   ASSIGN    TO       DA-01-VI-JYOKEN1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      JYO-F01  JYO-F02
                        FILE      STATUS   JYO-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    棚卸原票データ
******************************************************************
 FD  ZTANADT3
                        LABEL RECORD   IS   STANDARD.
     COPY     ZTANADT3  OF        XFDLIB
              JOINING   ZTA  AS   PREFIX.
*
******************************************************************
*    倉庫棚卸データ
******************************************************************
 FD  SNDTANXX           LABEL RECORD   IS   STANDARD.
     COPY     SNDTANXX  OF        XFDLIB
              JOINING   SND       PREFIX.
******************************************************************
*    倉庫棚卸データ（件数）
******************************************************************
 FD  SNDTAKXX           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K01-REC.
     03  K01-F01             PIC  9(08).
     03  K01-F02             PIC  X(02).
******************************************************************
*    条件ファイル　
******************************************************************
 FD  JYOKEN1            LABEL RECORD   IS   STANDARD.
     COPY     JYOKEN1   OF        XFDLIB
              JOINING   JYO       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WT-CNT                  PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE.
         05  SYS-YY        PIC 9(02).
         05  SYS-MM        PIC 9(02).
         05  SYS-DD        PIC 9(02).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  ZTA-STATUS        PIC  X(02).
     03  SND-STATUS        PIC  X(02).
     03  TAK-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NKE0410B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0410B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0410B".
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
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*    数量編集
 01  WK-HEN                 PIC   9(09)V9(02).
 01  WK-HEN-R               REDEFINES   WK-HEN.
     03  WK-HEN-1           PIC   9(09).
     03  WK-HEN-2           PIC   9(02).
*    数量編集
 01  WK-HEN1.
     03  WK-HEN1-1          PIC   X(01).
     03  WK-HEN1-2          PIC   X(09).
     03  WK-HEN1-3          PIC   X(01).
     03  WK-HEN1-4          PIC   X(02).
*    日付編集
 01  WK-HEN-DATE.
     03  WK-HEN-DATE1       PIC   X(04).
     03  FILLER             PIC   X(01)  VALUE  "/".
     03  WK-HEN-DATE2       PIC   X(02).
     03  FILLER             PIC   X(01)  VALUE  "/".
     03  WK-HEN-DATE3       PIC   X(02).
*    時間編集
 01  WK-HEN-TIME.
     03  WK-HEN-TIME1       PIC   X(02).
     03  FILLER             PIC   X(01)  VALUE  ":".
     03  WK-HEN-TIME2       PIC   X(02).
*    日付変換１
 01  WK-HENKAN              PIC   9(08)  VALUE  ZERO.
 01  WK-HIDUKE.
     03  WK-HIDUKE1         PIC   9(04).
     03  WK-HIDUKE2         PIC   9(02).
     03  WK-HIDUKE3         PIC   9(02).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 01  WK-ZTA-F112                  PIC       9(08)  VALUE  ZERO.
*
 LINKAGE                SECTION.
 01  PARA-IN-SIJISOKO       PIC   X(02).
 01  PARA-OUT-KENSU         PIC   9(07).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-SIJISOKO
                                       PARA-OUT-KENSU.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZTANADT3.
     MOVE      "ZTANADT3"   TO   AB-FILE.
     MOVE      ZTA-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDTANXX.
     MOVE      "SNDTANXX"   TO   AB-FILE.
     MOVE      SND-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDTAKXX.
     MOVE      "SNDTAKXX"   TO   AB-FILE.
     MOVE      TAK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JYOKEN1.
     MOVE      "JYOKEN1 "   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
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
     OPEN     INPUT     ZTANADT3  JYOKEN1.
     OPEN     OUTPUT    SNDTANXX  SNDTAKXX.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     SPACE            TO   ZTA-REC.
     INITIALIZE                     ZTA-REC.
     MOVE     PARA-IN-SIJISOKO TO   ZTA-F04.
     MOVE     ZERO             TO   ZTA-F01.
     START    ZTANADT3   KEY   >=   ZTA-F04   ZTA-F01
         INVALID   KEY
              MOVE      9      TO   END-FG
              DISPLAY NC"抽出対象データなし．" UPON CONS
              MOVE    4010     TO   PROGRAM-STATUS
              STOP    RUN
     END-START.
*
 INIT-010.
*
     PERFORM  ZTANADT-READ-SEC.
     IF       END-FG   =   9
              DISPLAY NC"抽出対象データなし．．" UPON CONS
              MOVE    4010     TO   PROGRAM-STATUS
              STOP    RUN
     END-IF.
     IF       PARA-IN-SIJISOKO NOT =  ZTA-F04
              DISPLAY NC"抽出対象データなし．．．" UPON CONS
              MOVE    4010     TO   PROGRAM-STATUS
              STOP    RUN
     END-IF.
*
 INIT-020.
*
     MOVE     99                  TO   JYO-F01.
     MOVE     "TANA    "          TO   JYO-F02.
     READ     JYOKEN1
         INVALID   KEY
              MOVE      9      TO   END-FG
              DISPLAY NC"条件ファイルなし"  UPON CONS
              DISPLAY   "KEY1=99 KEY2=TANA" UPON CONS
              MOVE    4010     TO   PROGRAM-STATUS
              STOP    RUN
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     IF       PARA-IN-SIJISOKO =  ZTA-F04
              CONTINUE
     ELSE
              MOVE      9    TO   END-FG
              GO   TO   MAIN-EXIT
     END-IF.
*
*倉庫棚卸データ出力
     MOVE     SPACE          TO   SND-REC.
     INITIALIZE                   SND-REC.
*棚卸_
     MOVE     ZTA-F01        TO   SND-F01.
*部門ＣＤ
     MOVE     ZTA-F02X       TO   SND-F02.
*場所ＣＤ
     MOVE     ZTA-F04        TO   SND-F03.
*棚番
     MOVE     ZTA-F15        TO   SND-F04.
*JANCD
     MOVE     ZTA-F88        TO   SND-F05.
*サカタ商品ＣＤ＋品単
     MOVE     ZTA-F05        TO   SND-F06(1:8).
     MOVE     ZTA-F06X       TO   SND-F06(9:8).
*棚卸数量   項目カット 2019/02/01
*帳簿在庫数 項目名変更 2019/02/01
*    MOVE     ZTA-F16        TO   SND-F08.
     MOVE     ZTA-F16        TO   SND-F07.
*担当者CD１ 項目カット 2019/02/01
*担当者CD２ 項目カット 2019/02/01
*担当者CD３ 項目カット 2019/02/01
*予備領域   項目カット 2019/02/01
*棚卸日　　 項目追加   2019/02/01
     MOVE     JYO-F04        TO   SND-F08.
*
     WRITE    SND-REC.
     ADD      1              TO   WT-CNT.
*
 MAIN-010.
*
     PERFORM  ZTANADT-READ-SEC.
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
     MOVE      WT-CNT       TO   PARA-OUT-KENSU.
     DISPLAY   NC"抽出件数＝"    PARA-OUT-KENSU   UPON CONS.
*件数ファイル出力
     MOVE  SPACE            TO   K01-REC.
     INITIALIZE                  K01-REC.
     MOVE  WT-CNT           TO   K01-F01.
     MOVE  X"0D0A"          TO   K01-F02.
     WRITE K01-REC.
*
     CLOSE     ZTANADT3  SNDTANXX  JYOKEN1  SNDTAKXX.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　棚卸原票ファイル　ＲＥＡＤ　　　　　　　　　　　*
****************************************************************
 ZTANADT-READ-SEC      SECTION.
*
     MOVE "ZTANADT-READ-SEC"   TO   S-NAME.
*
     READ     ZTANADT3
              AT END
                 MOVE    9     TO   END-FG
                 GO            TO   ZTANADT-READ-EXIT
              NOT AT END
                 ADD     1     TO   RD-CNT
     END-READ.
*
 ZTANADT-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
