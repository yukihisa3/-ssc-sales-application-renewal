****************************************************************
*                                                              *
*    顧客名　　　　　：　（株）サカタのタネ殿　　　　　　　　　*
*    サブシステム　　：　Ｄ３６５連携リカバリ対応　　　　　　  *
*    モジュール名　　：　日次計上ＤＴの連携日付を変更する。　　*
*    作成日　　　　　：　2024/04/04                            *
*    作成者　　　　　：　TAKAHASHI                             *
*    処理概要　　　　：　ＣＡＮＳＥＮＫＦを読み、計上Ｆを索引　*
*                        し、データ作成時刻を変更擦る。　　　　*
*    更新日　　　　　：　                                      *
*    更新者　　　　　：　                                      *
*    更新概要　　　　：　                                      *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NVS9010B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/06/10.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*取消データ　　　　　
     SELECT   CANSENK1  ASSIGN    TO        DA-01-VI-CANSENK1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       CAN-F01
                                            CAN-F02
                                            CAN-F03
                                            CAN-F04
                                            CAN-F05
                                            CAN-F06
                                            CAN-F07
                        FILE  STATUS   IS   CAN-STATUS.
*日次計上データ
     SELECT   URIKEJL1  ASSIGN    TO        DA-01-VI-URIKEJL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       URI-F01
                                            URI-F02
                                            URI-F03
                                            URI-F04
                                            URI-F05
                                            URI-F06
                                            URI-F07
                                            WITH DUPLICATES
                        FILE  STATUS   IS   URI-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    取消データ
******************************************************************
 FD  CANSENK1
                        LABEL RECORD   IS   STANDARD.
     COPY     CANSENK1  OF        XFDLIB
              JOINING   CAN  AS   PREFIX.
*
******************************************************************
*    日次計上データ
******************************************************************
 FD  URIKEJL1
                        LABEL RECORD   IS   STANDARD.
     COPY     URIKEJF   OF        XFDLIB
              JOINING   URI  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  SKP1-CNT                PIC  9(08)     VALUE  ZERO.
 01  SKP2-CNT                PIC  9(08)     VALUE  ZERO.
 01  URI-CNT                 PIC  9(08)     VALUE  ZERO.
 01  HEN-CNT                 PIC  9(08)     VALUE  ZERO.
 01  DEL-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WK-URI-F112             PIC  9(09)     PACKED-DECIMAL.
*
 01  WK-PARA-IN-AREA.
     03  WK-PARA             OCCURS  12.
       05  WK-SDENNO         PIC  9(09).
       05  WK-EDENNO         PIC  9(09).
 01  FLG-AREA.
     03  URIKEJL1-END        PIC  X(03)     VALUE SPACE.
     03  URIKEJL1-INV-FLG    PIC  X(03)     VALUE SPACE.
     03  SUBTBLL1-INV-FLG    PIC  X(03)     VALUE SPACE.
*
 01  WK-ST.
     03  CAN-STATUS        PIC  X(02).
     03  URI-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NVS9010B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVS9010B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVS9010B".
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
         05  FILLER         PIC   X(09)  VALUE " UPDATE= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*2020/05/25 NAV ST Ｄ３６５伝票番号取得
 01  WK-D365-DEN-PARA.
     03  WK-D365-PARA-IN1   PIC   X(01).
     03  WK-D365-PARA-IN2   PIC   9(08).
     03  WK-D365-PARA-OUT1  PIC   X(20).
     03  WK-D365-PARA-OUT2  PIC   X(01).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE             DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CANSENK1.
     MOVE      "CANSENK1"   TO   AB-FILE.
     MOVE      CAN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   URIKEJL1.
     MOVE      "URIKEJL1"   TO   AB-FILE.
     MOVE      URI-STATUS   TO   AB-STS.
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
              UNTIL     END-FLG    =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     CANSENK1.
     OPEN     I-O       URIKEJL1.
     DISPLAY  MSG-START UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     SYS-TIME       TO   SYS-TIME2.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD   TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     PERFORM  CANSENK1-READ-SEC.
     IF       END-FLG  =  "END"
              DISPLAY NC"＃対象データがありません＃" UPON CONS
     END-IF.
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
     PERFORM  HENKOU-SEC.
*
 MAIN-99.
*
     PERFORM  CANSENK1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　取消データ　　　　　　　　　　　　　　　　　　　*
****************************************************************
 CANSENK1-READ-SEC       SECTION.
*
     MOVE     "CANSENK1-READ-SEC"  TO      S-NAME.
*
*取消データ　　　　　　　　
     READ     CANSENK1
         AT END
              MOVE   "END"      TO    END-FLG
              GO                TO    CANSENK1-READ-EXIT
         NOT AT END
              ADD      1        TO    RD-CNT
     END-READ.
*
     IF   RD-CNT(6:3)  =  "000"  OR  "500"
          DISPLAY "#RD-CNT = " RD-CNT " #"  UPON  CONS
     END-IF.
*
 CANSENK1-READ-EXIT.
     EXIT.
****************************************************************
*　　変更処理　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 HENKOU-SEC              SECTION.
*
     MOVE     "HENKOU-SEC"         TO      S-NAME.
*
*売上データ索引　　　　　　
     MOVE     CAN-F01           TO    URI-F01.
     MOVE     CAN-F02           TO    URI-F02.
     MOVE     CAN-F03           TO    URI-F03.
     MOVE     CAN-F04           TO    URI-F04.
     MOVE     CAN-F05           TO    URI-F05.
     MOVE     CAN-F06           TO    URI-F06.
     MOVE     CAN-F07           TO    URI-F07.
     READ     URIKEJL1
              INVALID
              MOVE   "INV"      TO    URIKEJL1-INV-FLG
              NOT  INVALID
              MOVE   SPACE      TO    URIKEJL1-INV-FLG
     END-READ.
*    DISPLAY "URIKEJL1-INV-FLG = " URIKEJL1-INV-FLG UPON CONS.
*
     IF   URIKEJL1-INV-FLG  =  "INV"
          ADD      1            TO    SKP1-CNT
          GO                    TO    HENKOU-EXIT
     END-IF.
*
     IF    URI-F48  =  20240704
     AND   URI-F49  =  214339
           MOVE  214340         TO    URI-F49
     ELSE
          ADD      1            TO    SKP2-CNT
          GO                    TO    HENKOU-EXIT
     END-IF.
*
     REWRITE                          URI-REC.
     ADD       1                 TO   URI-CNT.
*
 HENKOU-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "RD-CNT   = " RD-CNT   UPON CONS.
     DISPLAY "URI-CNT  = " URI-CNT  UPON CONS.
     DISPLAY "SKP1-CNT = " SKP1-CNT UPON CONS.
     DISPLAY "SKP2-CNT = " SKP2-CNT UPON CONS.
*
     CLOSE     CANSENK1  URIKEJL1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
