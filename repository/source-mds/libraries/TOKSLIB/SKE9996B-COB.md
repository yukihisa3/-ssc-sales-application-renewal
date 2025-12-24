# SKE9996B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKE9996B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷検品システム　　　　　　　　　*
*    業務名　　　　　　　：　リカバリ処理                      *
*    モジュール名　　　　：　検品抽出データ→結果コピー　　　　*
*    作成日／更新日　　　：　2002/06/05                        *
*    作成者／更新者　　　：　T.TAKAHASHI                       *
*    処理概要　　　　　　：　検品抽出データを検品結果Ｆへコピー*
*                            する。                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE9996B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/10/10.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*送信用伝票データ
     SELECT   SNDDENF   ASSIGN    TO   DA-01-S-SNDDENF
                        FILE      STATUS    IS   SND-STATUS.
*送信用伝票結果ファイル
     SELECT   RCVSYUF   ASSIGN    TO   DA-01-S-RCVSYUF
                        FILE      STATUS    IS   RCV-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    送信用売上伝票データ
******************************************************************
 FD  SNDDENF            LABEL RECORD   IS   STANDARD.
     COPY     SNDDENF   OF        XFDLIB
              JOINING   SND       PREFIX.
******************************************************************
*    受信用検品結果データ
******************************************************************
 FD  RCVSYUF            BLOCK CONTAINS 42   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     RCVSYUF   OF        XFDLIB
              JOINING   RCV       PREFIX.
*
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
     03  SND-STATUS        PIC  X(02).
     03  RCV-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SKE9996B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKE9996B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKE9996B".
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
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDDENF.
     MOVE      "SNDDENF "   TO   AB-FILE.
     MOVE      SND-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RCVSYUF.
     MOVE      "RCVSYUF "   TO   AB-FILE.
     MOVE      RCV-STATUS   TO   AB-STS.
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
     OPEN     INPUT     SNDDENF.
     OPEN     OUTPUT    RCVSYUF.
     DISPLAY  MSG-START UPON CONS.
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
         MOVE    SYS-DATEW(1:4) TO   WK-HEN-DATE1
         MOVE    SYS-DATEW(5:2) TO   WK-HEN-DATE2
         MOVE    SYS-DATEW(7:2) TO   WK-HEN-DATE3
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
     PERFORM  SNDDENF-READ-SEC.
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
     MOVE     SPACE               TO   RCV-REC.
     INITIALIZE                        RCV-REC.
*項目セット
*取引先ＣＤ
     MOVE     SND-F01             TO   RCV-F01.
*店舗ＣＤ
     MOVE     SND-F04             TO   RCV-F02.
*発送完了日
     MOVE     WK-HEN-DATE         TO   RCV-F03.
*相手取引先ＣＤ
     MOVE     SND-F22(42:4)       TO   RCV-F04.
*納品日
     MOVE     SND-F06             TO   RCV-F05.
*伝票番号
     MOVE     SND-F02             TO   RCV-F06.
*行番号
     MOVE     SND-F03             TO   RCV-F07.
*ＪＡＮＣＤ
     MOVE     SND-F14             TO   RCV-F08.
*発注数量
     MOVE     SND-F21             TO   RCV-F10.
*出荷数量
     MOVE     SND-F11             TO   RCV-F12.
*符号セット
     MOVE     0                   TO   RCV-F09  RCV-F11.
*レコード出力
     WRITE    RCV-REC.
     ADD      1                   TO   WT-CNT.
*
     PERFORM  SNDDENF-READ-SEC.
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
     DISPLAY "READ-CNT  = " RD-CNT  UPON  CONS.
     DISPLAY "WRITE-CNT = " WT-CNT  UPON  CONS.
*
     CLOSE     SNDDENF  RCVSYUF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 SNDDENF-READ-SEC      SECTION.
*
     MOVE "SNDDENF-READ-SEC"   TO   S-NAME.
*
     READ     SNDDENF
              AT END
                 MOVE    9     TO   END-FG
              NOT AT END
                 ADD     1     TO   RD-CNT
     END-READ.
*
 SHTDENF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
