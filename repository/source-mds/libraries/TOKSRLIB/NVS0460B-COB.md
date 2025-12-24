# NVS0460B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVS0460B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　：　（株）サカタのタネ殿　　　　　　　　　*
*    サブシステム　　：　Ｄ３６５連携　　　　　　　　　　　　　*
*    モジュール名　　：　売上伝票Ｆ納品一括変更　　　　　　　　*
*    作成日　　　　　：　2022/09/05                            *
*    作成者　　　　　：　TAKAHASHI                             *
*    処理概要　　　　：　エラー復旧Ｆを読み、取得したキー情報で*
*                        売上伝票Ｆを読み、納期を一括変更する。*
*    更新日　　　　　：　                                      *
*    更新者　　　　　：　                                      *
*    更新概要　　　　：　                                      *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NVS0460B.
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
     SELECT   CANNOKH1  ASSIGN    TO        DA-01-VI-CANNOKH1
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
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    取消データ
******************************************************************
 FD  CANNOKH1
                        LABEL RECORD   IS   STANDARD.
     COPY     CANNOKH1  OF        XFDLIB
              JOINING   CAN  AS   PREFIX.
*
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  SKP1-CNT                PIC  9(08)     VALUE  ZERO.
 01  UPD-CNT                 PIC  9(08)     VALUE  ZERO.
 01  SKP2-CNT                PIC  9(08)     VALUE  ZERO.
 01  HEN-CNT                 PIC  9(08)     VALUE  ZERO.
 01  DEL-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WK-DEN-F112             PIC  9(09)     PACKED-DECIMAL.
*
 01  WK-PARA-IN-AREA.
     03  WK-PARA             OCCURS  12.
       05  WK-SDENNO         PIC  9(09).
       05  WK-EDENNO         PIC  9(09).
 01  FLG-AREA.
     03  SHTDENL1-END        PIC  X(03)     VALUE SPACE.
     03  SHTDENL1-INV-FLG    PIC  X(03)     VALUE SPACE.
     03  SUBTBLL1-INV-FLG    PIC  X(03)     VALUE SPACE.
*
 01  WK-ST.
     03  CAN-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NVS0460B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVS0460B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVS0460B".
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
 01  LINK-AREA.
     03  LINK-IN-DATE           PIC 9(08)  VALUE  ZERO.
     03  LINK-IN-TIME           PIC 9(04)  VALUE  ZERO.
     03  LINK-IN-TORICD         PIC 9(08)  VALUE  ZERO.
     03  LINK-IN-SOKCD          PIC X(02)  VALUE  SPACE.
     03  LINK-IN-FDATE          PIC 9(08)  VALUE  ZERO.
     03  LINK-IN-TDATE          PIC 9(08)  VALUE  ZERO.
     03  LINK-IN-DENNO          PIC 9(09)  VALUE  ZERO.
     03  LINK-IN-GYO            PIC 9(09)  VALUE  ZERO.
     03  LINK-IN-TENCD          PIC 9(09)  VALUE  ZERO.
     03  LINK-IN-TANCD          PIC X(02)  VALUE  SPACE.
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE             DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CANNOKH1.
     MOVE      "CANNOKH1"   TO   AB-FILE.
     MOVE      CAN-STATUS   TO   AB-STS.
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
     OPEN     INPUT     CANNOKH1.
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
     PERFORM  CANNOKH1-READ-SEC.
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
     MOVE     CAN-F09             TO   LINK-IN-DATE.
     MOVE     CAN-F10             TO   LINK-IN-TIME.
     MOVE     CAN-F01             TO   LINK-IN-TORICD.
     MOVE     CAN-F11             TO   LINK-IN-SOKCD.
     MOVE     CAN-F06             TO   LINK-IN-FDATE.
     MOVE     CAN-F08             TO   LINK-IN-TDATE.
     MOVE     CAN-F02             TO   LINK-IN-DENNO.
     MOVE     CAN-F07             TO   LINK-IN-GYO.
     MOVE     CAN-F05             TO   LINK-IN-TENCD.
     MOVE     "48"                TO   LINK-IN-TANCD.
     CALL    "SSY9420R"      USING     LINK-IN-DATE
                                       LINK-IN-TIME
                                       LINK-IN-TORICD
                                       LINK-IN-SOKCD
                                       LINK-IN-FDATE
                                       LINK-IN-TDATE
                                       LINK-IN-DENNO
                                       LINK-IN-GYO
                                       LINK-IN-TENCD
                                       LINK-IN-TANCD.
     ADD      1                   TO   UPD-CNT.
 MAIN-99.
*
     PERFORM  CANNOKH1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　取消データ　　　　　　　　　　　　　　　　　　　*
****************************************************************
 CANNOKH1-READ-SEC       SECTION.
*
     MOVE     "CANNOKH1-READ-SEC"  TO      S-NAME.
*
*取消データ　　　　　　　　
     READ     CANNOKH1
         AT END
              MOVE   "END"      TO    END-FLG
              GO                TO    CANNOKH1-READ-EXIT
         NOT AT END
              ADD      1        TO    RD-CNT
     END-READ.
*
     IF   RD-CNT(6:3)  =  "000"  OR  "500"
          DISPLAY "#RD-CNT = " RD-CNT " #"  UPON  CONS
     END-IF.
*
 CANNOKH1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "RD-CNT   = " RD-CNT   UPON CONS.
     DISPLAY "UPD-CNT  = " UPD-CNT  UPON CONS.
*
     CLOSE     CANNOKH1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
