****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　ＪＥＤＩＣＯＳデータ変換　　　　　*
*    作成日／更新日　　　：　2008/04/21                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　リックデータを変換する。　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJH8900B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/04/21.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   JEDICOS   ASSIGN    TO        DA-01-S-JEDICOS
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    EDI-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（資材）
     SELECT   LICFILE1   ASSIGN    TO        DA-01-S-LICFILE1
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    LIC1-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（植物）
     SELECT   LICFILE2   ASSIGN    TO        DA-01-S-LICFILE2
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    LIC2-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（以外）
     SELECT   LICFILE3   ASSIGN    TO        DA-01-S-LICFILE3
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    LIC3-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２５６　  ＢＦ＝　１
******************************************************************
 FD  JEDICOS
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  EDI-REC.
     03  EDI-01                  PIC  X(02).
     03  EDI-02                  PIC  X(1861).
     03  EDI-03                  PIC  9(06).
     03  EDI-04                  PIC  X(1272).
******************************************************************
*    ＪＣＡフォーマットファイル（資材）
******************************************************************
 FD  LICFILE1            LABEL RECORD   IS   STANDARD.
*
 01  LIC1-REC.
     03  FILLER                   PIC X(3141).
******************************************************************
*    ＪＣＡフォーマットファイル（植物）
******************************************************************
 FD  LICFILE2            LABEL RECORD   IS   STANDARD.
*
 01  LIC2-REC.
     03  FILLER                   PIC X(3141).
******************************************************************
*    ＪＣＡフォーマットファイル（以外）
******************************************************************
 FD  LICFILE3            LABEL RECORD   IS   STANDARD.
*
 01  LIC3-REC.
     03  FILLER                   PIC X(3141).
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  FURI-FLG                PIC  X(01)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  LIC-CNT1                PIC  9(08)     VALUE  ZERO.
 01  LIC-CNT2                PIC  9(08)     VALUE  ZERO.
 01  LIC-CNT3                PIC  9(08)     VALUE  ZERO.
 01  WK-ST.
     03  EDI-STATUS        PIC  X(02).
     03  LIC1-STATUS       PIC  X(02).
     03  LIC2-STATUS       PIC  X(02).
     03  LIC3-STATUS       PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJH8900B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH8900B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH8900B".
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

         05  FILLER         PIC   X(05)  VALUE " *** ".
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
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
                        PROCEDURE   JEDICOS.
     MOVE      "JEDICOS "   TO   AB-FILE.
     MOVE      EDI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   LICFILE1.
     MOVE      "LICFILE1"    TO   AB-FILE.
     MOVE      LIC1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   LICFILE2.
     MOVE      "LICFILE2"    TO   AB-FILE.
     MOVE      LIC2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   LICFILE3.
     MOVE      "LICFILE3"    TO   AB-FILE.
     MOVE      LIC3-STATUS   TO   AB-STS.
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
     OPEN     INPUT     JEDICOS.
     OPEN     OUTPUT    LICFILE1  LICFILE2  LICFILE3.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG       RD-CNT.
     MOVE     ZERO      TO        LIC-CNT1     LIC-CNT2.
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
*
     PERFORM  JEDICOS-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*    伝票ヘッダレコード
     IF    EDI-01  =   "HD"
           EVALUATE  EDI-03
               WHEN  1994
                     MOVE "1"    TO   FURI-FLG
               WHEN  1995
                     MOVE "2"    TO   FURI-FLG
               WHEN  OTHER
                     MOVE "3"    TO   FURI-FLG
           END-EVALUATE
     END-IF.
*レコード出力
     EVALUATE  FURI-FLG
         WHEN  "1"
               MOVE  SPACE       TO   LIC1-REC
               MOVE  EDI-REC     TO   LIC1-REC
               WRITE LIC1-REC
               ADD   1           TO   LIC-CNT1
         WHEN  "2"
               MOVE  SPACE       TO   LIC2-REC
               MOVE  EDI-REC     TO   LIC2-REC
               WRITE LIC2-REC
               ADD   1           TO   LIC-CNT2
         WHEN  "3"
               MOVE  SPACE       TO   LIC3-REC
               MOVE  EDI-REC     TO   LIC3-REC
               WRITE LIC3-REC
               ADD   1           TO   LIC-CNT3
     END-EVALUATE.
*
     PERFORM JEDICOS-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル読込　　　　　　　　　　　　　　　　　　*
****************************************************************
 JEDICOS-READ-SEC      SECTION.
*
     MOVE "JEDICOS-READ-SEC" TO       S-NAME.
*
     READ     JEDICOS
              AT END
              MOVE     9      TO    END-FG
              NOT AT END
              ADD      1      TO    RD-CNT
     END-READ.
*
     IF   RD-CNT(6:3) = "000" OR "500"
          DISPLAY "READ-CNT = " RD-CNT   UPON CONS
     END-IF.
*
 JEDICOS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     CLOSE     JEDICOS  LICFILE1 LICFILE2 LICFILE3.
*
     DISPLAY NC"＃受信データ　　　ＣＮＴ＝"  RD-CNT
                                             UPON CONS.
     DISPLAY NC"＃リック（資材）　ＣＮＴ＝"  LIC-CNT1
                                             UPON CONS.
     DISPLAY NC"＃リック（植物）　ＣＮＴ＝"  LIC-CNT2
                                             UPON CONS.
     DISPLAY NC"＃リック（以外）　ＣＮＴ＝"  LIC-CNT3
                                             UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
