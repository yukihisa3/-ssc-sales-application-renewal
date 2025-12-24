# SOS2190B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SOS2190B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＳＯＳ処理　　　　　　　　　　　　*
*    業務名　　　　　　　：　ＳＯＳ処理　　　　　　　　　　　　*
*    モジュール名　　　　：　ダイキデータ変換処理              *
*    作成日／更新日　　　：　04/10/21                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　ダイキリカバリデータ作成。　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SOS2190B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          04/10/21.
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
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
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
 01  CRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  AKA-CNT                 PIC  9(08)     VALUE  ZERO.
 01  KUR-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WK-DEN-F112             PIC  9(08)     VALUE  ZERO.
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
         05  ST-PG          PIC   X(08)  VALUE "SOS2190B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SOS2190B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SOS2190B".
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
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
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
     OPEN     INPUT     SHTDENL1.
     OPEN     OUTPUT    AKADENF.
     DISPLAY  MSG-START UPON CONS.
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
     MOVE    SPACE          TO    DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE    100403         TO    DEN-F01.
     START  SHTDENL1  KEY  IS  >=  DEN-F01  DEN-F02  DEN-F04
                                   DEN-F051 DEN-F03
            INVALID
            MOVE      9     TO    END-FG
            GO              TO    INIT-EXIT
     END-START.
 INIT-010.
*
     PERFORM  SHTDENL1-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*黒データ
     MOVE     SPACE               TO   AKA-REC.
     INITIALIZE                        AKA-REC.
     MOVE     DEN-REC             TO   AKA-REC.
     MOVE     1                   TO   AKA-F04.
     WRITE    AKA-REC.
     ADD      1                   TO   AKA-CNT.
     MOVE     SPACE               TO   AKA-REC.
     INITIALIZE                        AKA-REC.
     MOVE     DEN-REC             TO   AKA-REC.
     MOVE     2                   TO   AKA-F04.
     WRITE    AKA-REC.
     ADD      1                   TO   KUR-CNT.
*
     PERFORM  SHTDENL1-READ-SEC.
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
     DISPLAY "READ-CNT  =  "  READ-CNT  UPON CONS.
     DISPLAY "DATA-CNT  =  "  CRT-CNT   UPON CONS.
     DISPLAY "AKA-CNT   =  "  AKA-CNT   UPON CONS.
     DISPLAY "KUR-CNT   =  "  KUR-CNT   UPON CONS.
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
 SHTDENL1-READ-SEC     SECTION.
*
     MOVE "SHTDENL1-READ-SEC" TO   S-NAME.
*
     READ SHTDENL1 AT END
          MOVE     9          TO   END-FG
          GO                  TO   SHTDENL1-READ-EXIT
          NOT  AT  END
          ADD      1          TO   READ-CNT
     END-READ.
*処理件数表示
     IF   READ-CNT(6:3) =  "000"  OR "500"
          DISPLAY "READ-CNT = " READ-CNT  UPON CONS
     END-IF.
*取引先CDチェック
     IF   DEN-F01  >  100403
          MOVE     9          TO   END-FG
          GO                  TO   SHTDENL1-READ-EXIT
     END-IF.
*計上区分チェック
     IF   DEN-F277  NOT =  9
          GO                  TO   SHTDENL1-READ-SEC
     END-IF.
*
     ADD        1             TO   CRT-CNT.
*
 SHTDENL1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
