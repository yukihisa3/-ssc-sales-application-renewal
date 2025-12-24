****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　出荷検品システム　　　　　　　　　
*    業務名　　　　　　　：　出荷
*    モジュール名　　　　：　売上伝票データ集計（アレンザ）
*    作成日／更新日　　　：　2023/04/26
*    作成者／更新者　　　：　NAV
*    処理概要　　　　　　：　アレンザ出荷指示データ作成用に
*                            センター納品発注のみ
*                            同一ＫＥＹの数量を集計する
*    更新履歴            ：
*　
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE5014B.
*                  流用:SKE5013B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2023/04/26.
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
*売上伝票抽出Ｆ
     SELECT   ARZDENW1  ASSIGN    TO        DA-01-VI-ARZDENW1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-XI0001
                                            DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F112  DEN-F07
                                            DEN-F25
                        FILE      STATUS    IS   DEN-STATUS.
*売上伝票集計Ｆ
     SELECT   ARZSYUWK   ASSIGN    TO       ARZSYUWK
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    IS   SDE-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票抽出Ｆ　ＲＬ＝１０２０
******************************************************************
 FD  ARZDENW1
                        LABEL RECORD   IS   STANDARD.
     COPY     ARZDENW1  OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    売上伝票集計Ｆ
******************************************************************
 FD  ARZSYUWK            LABEL RECORD   IS   STANDARD.
     COPY     ARZSYUWK   OF        XFDLIB
              JOINING   SDE       PREFIX.
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
     03  DEN-STATUS        PIC  X(02).
     03  SDE-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SKE5014B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKE5014B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKE5014B".
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
*    トステムビバ伝票番号編集(店舗ＣＤを付加）
 01  WK-TOS-DEN.
     03  WK-TOS-DEN1        PIC   9(03)  VALUE  ZERO.
     03  WK-TOS-DEN2        PIC   9(06)  VALUE  ZERO.
*    日付変換１
 01  WK-HENKAN              PIC   9(08)  VALUE  ZERO.
 01  WK-HIDUKE.
     03  WK-HIDUKE1         PIC   9(04).
     03  WK-HIDUKE2         PIC   9(02).
     03  WK-HIDUKE3         PIC   9(02).
*
*ブレイクキー
 01  BRK-AREA.
     03  BRK-DEN-F46        PIC   9(08).
     03  BRK-DEN-F47        PIC   9(04).
     03  BRK-DEN-F01        PIC   9(08).
     03  BRK-DEN-F48        PIC   X(02).
     03  BRK-DEN-F112       PIC   9(08).
     03  BRK-DEN-F07        PIC   9(05).
     03  BRK-DEN-F25        PIC   X(13).
     03  BRK-DEN-REC        PIC   X(1020).
*数量加算
 01  SUM-AREA.
     03  SUM-DEN-F15        PIC  S9(09).
     03  SUM-DEN-F50        PIC  S9(09).
     03  SUM-CNT            PIC   9(09).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 01  WK-DEN-F112                  PIC       9(08)  VALUE  ZERO.
*
*LINKAGE                SECTION.
*01  PARA-JDATE             PIC   9(08).
*01  PARA-JTIME             PIC   9(04).
*01  PARA-TORICD            PIC   9(08).
*01  PARA-SOKO              PIC   X(02).
*01  PARA-NOHBI             PIC   9(08).
*01  PARA-BUMON             PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
*PROCEDURE              DIVISION USING PARA-JDATE
*                                      PARA-JTIME
*                                      PARA-TORICD
*                                      PARA-SOKO
*                                      PARA-NOHBI.
*                                      PARA-BUMON.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ARZDENW1.
     MOVE      "ARZDENW1"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ARZSYUWK.
     MOVE      "ARZSYUWK "   TO   AB-FILE.
     MOVE      SDE-STATUS   TO   AB-STS.
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
     PERFORM  MAIN1-SEC
              UNTIL     END-FG    =    9.
     MOVE     0                   TO   END-FG.
     CLOSE    ARZDENW1.
     OPEN     INPUT ARZDENW1.
     PERFORM  MAIN2-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     ARZDENW1.
     OPEN     OUTPUT    ARZSYUWK.
     DISPLAY  MSG-START UPON CONS.
*
     INITIALIZE         BRK-AREA  SUM-AREA.
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
 INIT-010.
*
     PERFORM  ARZDENW1-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理１　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN1-SEC     SECTION.
*
     MOVE    "MAIN1-SEC"           TO   S-NAME.
*
 MAIN1-01.
     IF  DEN-XI0001  NOT = "02"
         ADD      1          TO   RD-CNT
         MOVE     SPACE      TO   SDE-REC
         INITIALIZE               SDE-REC
         MOVE     DEN-REC    TO   SDE-REC
         GO                  TO   MAIN1-02
     ELSE
         GO                  TO   MAIN1-010
     END-IF.
*
 MAIN1-02.
     WRITE    SDE-REC.
     ADD      1              TO   WT-CNT.
*
 MAIN1-010.
*
     PERFORM  ARZDENW1-READ-SEC.
*
 MAIN1-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理２　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN2-SEC     SECTION.
*
     MOVE    "MAIN2-SEC"           TO   S-NAME.
*
 MAIN2-01.
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     "02"           TO   DEN-XI0001.
     START    ARZDENW1  KEY  >=   DEN-XI0001
                                  DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F112  DEN-F07
                                  DEN-F25
         INVALID   KEY
              MOVE      9    TO   END-FG
              GO   TO   MAIN2-EXIT
     END-START.
 MAIN2-02.
*    初期READ
     PERFORM  ARZDENW1-READ-SEC.
     IF       END-FG         =  9
          OR  DEN-XI0001 NOT = "02"
              GO   TO   MAIN2-EXIT
     END-IF.
     ADD      1          TO   RD-CNT.
 MAIN2-03.
*    ブレイクキー保管
     MOVE     DEN-F46        TO   BRK-DEN-F46.
     MOVE     DEN-F47        TO   BRK-DEN-F47.
     MOVE     DEN-F01        TO   BRK-DEN-F01.
     MOVE     DEN-F48        TO   BRK-DEN-F48.
     MOVE     DEN-F112       TO   BRK-DEN-F112.
     MOVE     DEN-F07        TO   BRK-DEN-F07.
     MOVE     DEN-F25        TO   BRK-DEN-F25.
     MOVE     DEN-REC        TO   BRK-DEN-REC.
*
 MAIN2-04.
*    非ブレイク
     IF     ( DEN-F46        =    BRK-DEN-F46  ) AND
            ( DEN-F47        =    BRK-DEN-F47  ) AND
            ( DEN-F01        =    BRK-DEN-F01  ) AND
            ( DEN-F48        =    BRK-DEN-F48  ) AND
            ( DEN-F112       =    BRK-DEN-F112 ) AND
            ( DEN-F07        =    BRK-DEN-F07  ) AND
            ( DEN-F25        =    BRK-DEN-F25  )
              ADD   DEN-F15  TO   SUM-DEN-F15
              ADD   DEN-F50  TO   SUM-DEN-F50
              ADD   1        TO   SUM-CNT
              GO             TO   MAIN2-010
         END-IF.
*    ブレイク
         MOVE     BRK-DEN-REC    TO   SDE-REC.
         MOVE     SUM-DEN-F15    TO   SDE-F15.
         MOVE     SUM-DEN-F50    TO   SDE-F50.
*T↓
*        DISPLAY "DEN-F02=" DEN-F02 UPON CONS.
*T↑
         WRITE    SDE-REC.
*T↓
*        DISPLAY "SDE-WRT="         UPON CONS.
*T↑
         ADD      1              TO   WT-CNT.
         MOVE     ZERO           TO   SUM-DEN-F15 SUM-DEN-F50.
*        ブレイクキー保管
         MOVE     DEN-F46        TO   BRK-DEN-F46.
         MOVE     DEN-F47        TO   BRK-DEN-F47.
         MOVE     DEN-F01        TO   BRK-DEN-F01.
         MOVE     DEN-F48        TO   BRK-DEN-F48.
         MOVE     DEN-F112       TO   BRK-DEN-F112.
         MOVE     DEN-F07        TO   BRK-DEN-F07.
         MOVE     DEN-F25        TO   BRK-DEN-F25.
         MOVE     DEN-REC        TO   BRK-DEN-REC.
         ADD      DEN-F15        TO   SUM-DEN-F15
         ADD      DEN-F50        TO   SUM-DEN-F50
         MOVE     1              TO   SUM-CNT.
         GO                      TO   MAIN2-010.
*
 MAIN2-010.
*
     PERFORM  ARZDENW1-READ-SEC.
     IF       DEN-XI0001 NOT =  "02"
              MOVE  9    TO      END-FG
     END-IF.
     IF       END-FG         =    9
              GO       TO    MAIN2-EXIT
     ELSE
              ADD  1   TO    RD-CNT
              GO       TO    MAIN2-04
     END-IF.
*
 MAIN2-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
*    最終レコード
     IF        SUM-CNT > 0
               MOVE     BRK-DEN-REC    TO   SDE-REC
               MOVE     SUM-DEN-F15    TO   SDE-F15
               MOVE     SUM-DEN-F50    TO   SDE-F50
               WRITE    SDE-REC
               ADD      1              TO   WT-CNT
     END-IF.
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WT-CNT    TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON    CONS.
     DISPLAY   MSG-OUT   UPON    CONS.
     CLOSE     ARZDENW1  ARZSYUWK.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上伝票ファイル　ＲＥＡＤ　　　　　　　　　　　*
****************************************************************
 ARZDENW1-READ-SEC      SECTION.
*
     MOVE "ARZDENW1-READ-SEC"   TO   S-NAME.
*
     READ     ARZDENW1
              AT END
                 MOVE    9     TO   END-FG
                 GO            TO   ARZDENW1-READ-EXIT
*             NOT AT END
*                ADD     1     TO   RD-CNT
     END-READ.
*
 ARZDENW1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
