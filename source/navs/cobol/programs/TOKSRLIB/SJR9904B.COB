****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受領返品取込機能（リカバリ）　　　*
*    業務名　　　　　　　：　計上済売上伝票ＤＴ商品リカバリ　　*
*    モジュール名　　　　：　計上済売上伝票ＤＴ商品リカバリ　　*
*    作成日／更新日　　　：　2021/01/18                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　売上伝票Ｆのリカバリ　　　　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJR9904B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          16/12/15.
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
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*受領返品データ　　　　
     SELECT   COMRHEL1  ASSIGN    TO        DA-01-VI-COMRHEL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RHE-F01   RHE-F03
                                            RHE-F02   RHE-F04
                                            RHE-F05
                        FILE      STATUS    RHE-STATUS.
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
 FD  COMRHEL1
                        LABEL RECORD   IS   STANDARD.
*
     COPY     COMRHEL1  OF        XFDLIB
              JOINING   RHE       PREFIX.
*
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  X(03)     VALUE  SPACE.
 01  READ-CNT                PIC  9(08)     VALUE  ZERO.
 01  UP-CNT                  PIC  9(08)     VALUE  ZERO.
 01  COMRHEL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  RHE-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR9904B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR9904B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR9904B".
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
 LINKAGE                SECTION.
 01  PARA-DENNO             PIC   9(09).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING  PARA-DENNO.
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
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRHEL1.
     MOVE      "COMRHEL1"   TO   AB-FILE.
     MOVE      RHE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       SHTDENL1.
     OPEN     INPUT     COMRHEL1.
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
     MOVE     SPACE             TO   DEN-REC.
     INITIALIZE                      DEN-REC.
     MOVE     275               TO   DEN-F01.
     MOVE     PARA-DENNO        TO   DEN-F02.
     MOVE     ZERO              TO   DEN-F04.
     MOVE     42                TO   DEN-F051.
     START  SHTDENL1  KEY  IS  >=  DEN-F01  DEN-F02  DEN-F04
                                   DEN-F051 DEN-F07  DEN-F112
                                   DEN-F03
            INVALID
            MOVE      9     TO     END-FG
            GO              TO     INIT-EXIT
     END-START.
*
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
*受領返品データ索引
     MOVE     DEN-F01             TO   RHE-F01.
     MOVE     DEN-F112            TO   RHE-F03.
     MOVE     DEN-F07             TO   RHE-F02.
     MOVE     DEN-F02             TO   RHE-F04.
     MOVE     DEN-F03             TO   RHE-F05.
     READ     COMRHEL1
              INVALID
              MOVE "INV"          TO   COMRHEL1-INV-FLG
              NOT  INVALID
              MOVE SPACE          TO   COMRHEL1-INV-FLG
     END-READ.
*
     DISPLAY "INV-FLG = " COMRHEL1-INV-FLG UPON CONS.
     IF       COMRHEL1-INV-FLG  =  SPACE
              MOVE     RHE-F09    TO   DEN-F25
              REWRITE  DEN-REC
              ADD      1          TO   UP-CNT
     END-IF.
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
     DISPLAY "UP-CNT    =  "  UP-CNT    UPON CONS.
*
     CLOSE    SHTDENL1  COMRHEL1.
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
          MOVE     "END"      TO   END-FG
          GO                  TO   SHTDENL1-READ-EXIT
          NOT  AT  END
          ADD      1          TO   READ-CNT
     END-READ.
*処理件数表示
     IF   READ-CNT(6:3) =  "000"  OR "500"
          DISPLAY "READ-CNT = " READ-CNT  UPON CONS
     END-IF.
 DEN010.
*取引先ＣＤ
     IF   DEN-F01   >   275
          MOVE     "END"      TO   END-FG
          GO                  TO   SHTDENL1-READ-EXIT
     END-IF.
 DEN020.
*伝票番号チェック
*****DISPLAY "DEN-F02 = " DEN-F02  " - " PARA-DENNO UPON CONS.
     IF   DEN-F02   NOT =  PARA-DENNO
          MOVE     "END"      TO   END-FG
          GO                  TO   SHTDENL1-READ-EXIT
     END-IF.
 DEN030.
*相殺区分チェック
     IF   DEN-F04   NOT =  ZERO
          MOVE     "END"      TO   END-FG
          GO                  TO   SHTDENL1-READ-EXIT
     END-IF.
 DEN040.
*伝票区分チェック
     IF   DEN-F051  NOT =  42
          MOVE     "END"      TO   END-FG
          GO                  TO   SHTDENL1-READ-EXIT
     END-IF.
*
 SHTDENL1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*


