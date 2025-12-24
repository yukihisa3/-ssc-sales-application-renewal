# SAG0030B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SAG0030B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　グッデイ計上処理　　　　　　　　　
*    業務名　　　　　　　：　グッデイ計上処理　　　　　　　　　
*    モジュール名　　　　：　グッデイの計上データを作成する。
*    作成日／更新日　　　：　07/06/23
*    作成者／更新者　　　：　NAV
*    処理概要　　　　　　：　グッデイのウタネ分計上データを作成
*                            する。　　　　
*    更新履歴            ：
*      2011/10/07 飯田/NAV 基幹サーバ統合
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SAG0030B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/06/23.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENL1  ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                       *> 2011/10/07,S  S.I/NAV
                                            DEN-F07   DEN-F112
                                       *> 2011/10/07,E  S.I/NAV
                                            DEN-F03
                        FILE      STATUS    IS   DEN-STATUS.
*赤黒データ
     SELECT   AKADENF   ASSIGN    TO        AKADENF
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    IS   AKA-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENL1
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    送信用売上伝票データ
******************************************************************
 FD  AKADENF            LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   AKA       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  READ-CNT                PIC  9(08)     VALUE  ZERO.
 01  CRT-CNT1                PIC  9(08)     VALUE  ZERO.
 01  CRT-CNT2                PIC  9(08)     VALUE  ZERO.
 01  AKA-CNT                 PIC  9(08)     VALUE  ZERO.
 01  KUR-CNT                 PIC  9(08)     VALUE  ZERO.
 01  SHTDENL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  AKA-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SAG0030B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SAG0030B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SAG0030B".
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
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                    SECTION.
 01  PARA-OUT-CNT1          PIC   9(07).
 01  PARA-OUT-CNT2          PIC   9(07).
 01  PARA-OUT-CNT3          PIC   9(07).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING  PARA-OUT-CNT1
                                         PARA-OUT-CNT2
                                         PARA-OUT-CNT3.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENL1.
     MOVE      "SHTDENL1"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AKADENF.
     MOVE      "AKADENF "   TO   AB-FILE.
     MOVE      AKA-STATUS   TO   AB-STS.
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
     OPEN     I-O       SHTDENL1.
     OPEN     INPUT     AKADENF.
     DISPLAY  MSG-START UPON CONS.
 INIT-010.
*
     PERFORM  AKADENF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*売上ファイル検索
     MOVE     AKA-F01             TO   DEN-F01.
     MOVE     AKA-F02             TO   DEN-F02.
     MOVE     AKA-F04             TO   DEN-F04.
     MOVE     AKA-F051            TO   DEN-F051.
     MOVE     AKA-F03             TO   DEN-F03.
* 2011/10/06,S  S.I/NAV
     MOVE  AKA-F07          TO  DEN-F07.  *> 店舗コード
     MOVE  AKA-F112         TO  DEN-F112. *> 納品日
* 2011/10/06,E  S.I/NAV
     READ     SHTDENL1
              INVALID
              MOVE  "INV"         TO   SHTDENL1-INV-FLG
              NOT  INVALID
              MOVE  SPACE         TO   SHTDENL1-INV-FLG
     END-READ.
*
     IF       SHTDENL1-INV-FLG = "INV"
              MOVE    SPACE       TO   DEN-REC
              INITIALIZE               DEN-REC
              MOVE    AKA-REC     TO   DEN-REC
**************登録更新承認担当者初期値セット
              MOVE    DEN-F06     TO   DEN-F59 DEN-F60 DEN-F61
**************登録更新承認日初期値セット
              MOVE    DEN-F112    TO   DEN-F62 DEN-F63 DEN-F64
**************代表権限・権限初期値セット
              MOVE    "1"         TO   DEN-F65 DEN-F66
**************最終更新時間
              MOVE    ZERO        TO   DEN-F67
              WRITE   DEN-REC
              ADD     1           TO   CRT-CNT1
     ELSE
              ADD     1           TO   CRT-CNT2
     END-IF.
*
     PERFORM  AKADENF-READ-SEC.
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
     DISPLAY "READ-CNT    =  "  READ-CNT  UPON CONS.
     DISPLAY "KEIJYOU     =  "  CRT-CNT1  UPON CONS.
     DISPLAY "TAISYOUGAI  =  "  CRT-CNT2  UPON CONS.
*##内部統制対応 2008/09/02
     MOVE     READ-CNT    TO    PARA-OUT-CNT1.
     MOVE     CRT-CNT1    TO    PARA-OUT-CNT2.
     MOVE     CRT-CNT2    TO    PARA-OUT-CNT3.
*
     CLOSE    SHTDENL1  AKADENF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　売上伝票ファイル読込処理　　　　　　　　　　　　　　　　　*
****************************************************************
 AKADENF-READ-SEC     SECTION.
*
     MOVE "AKADENF-READ-SEC" TO   S-NAME.
*
     READ AKADENF  AT END
          MOVE     9          TO   END-FG
          GO                  TO   AKADENF-READ-EXIT
          NOT  AT  END
          ADD      1          TO   READ-CNT
     END-READ.
*
     IF        READ-CNT(6:3)  =  "000" OR "500"
               DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*
 AKADENF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
