# NVS0420B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVS0420B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　：　（株）サカタのタネ殿　　　　　　　　　*
*    サブシステム　　：　Ｄ３６５連携　　　　　　　　　　　　　*
*    モジュール名　　：　ナフコ先行受注分　在庫調整ＤＴセット　*
*    作成日　　　　　：　2022/03/29                            *
*    作成者　　　　　：　TAKAHASHI                             *
*    処理概要　　　　：　エラー復旧Ｆを読み、取得したキー情報で*
*                        売上伝票Ｆを索引し、在庫調整ＤＴ連携済*
*                        に更新する。　　　　　　　　　　　　　*
*    更新日　　　　　：　                                      *
*    更新者　　　　　：　                                      *
*    更新概要　　　　：　                                      *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NVS0420B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2022/03/22.
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
*売上伝票データ
     SELECT   SHTDENL1  ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01
                                            DEN-F02
                                            DEN-F04
                                            DEN-F051
                                            DEN-F07
                                            DEN-F112
                                            DEN-F03
                                            WITH DUPLICATES
                        FILE  STATUS   IS   DEN-STATUS.
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
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENL1
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  SKP1-CNT                PIC  9(08)     VALUE  ZERO.
 01  DEN-CNT                 PIC  9(08)     VALUE  ZERO.
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
         05  ST-PG          PIC   X(08)  VALUE "NVS0420B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVS0420B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVS0420B".
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
                        PROCEDURE   SHTDENL1.
     MOVE      "SHTDENL1"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
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
     OPEN     I-O       SHTDENL1.
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
     MOVE     CAN-F01           TO    DEN-F01.
     MOVE     CAN-F02           TO    DEN-F02.
     MOVE     CAN-F03           TO    DEN-F04.
     MOVE     CAN-F04           TO    DEN-F051.
     MOVE     CAN-F05           TO    DEN-F07.
     MOVE     ZERO              TO    WK-DEN-F112.
     MOVE     CAN-F06           TO    WK-DEN-F112.
     MOVE     WK-DEN-F112       TO    DEN-F112.
     MOVE     CAN-F07           TO    DEN-F03.
*    DISPLAY "KEJ-F01 = " KEJ-F01  "-" DEN-F01  UPON CONS.
*    DISPLAY "KEJ-F02 = " KEJ-F02  "-" DEN-F02  UPON CONS.
*    DISPLAY "KEJ-F03 = " KEJ-F03  "-" DEN-F04  UPON CONS.
*    DISPLAY "KEJ-F04 = " KEJ-F04  "-" DEN-F051 UPON CONS.
*    DISPLAY "KEJ-F05 = " KEJ-F05  "-" DEN-F07  UPON CONS.
*    DISPLAY "KEJ-F06 = " WK-DEN-F112  "-" DEN-F112 UPON CONS.
*    DISPLAY "KEJ-F07 = " KEJ-F07  "-" DEN-F03  UPON CONS.
     READ     SHTDENL1
              INVALID
              MOVE   "INV"      TO    SHTDENL1-INV-FLG
              NOT  INVALID
              MOVE   SPACE      TO    SHTDENL1-INV-FLG
     END-READ.
*    DISPLAY "SHTDENL1-INV-FLG = " SHTDENL1-INV-FLG UPON CONS.
*
     IF   SHTDENL1-INV-FLG  =  "INV"
          ADD      1            TO    SKP1-CNT
          GO                    TO    HENKOU-EXIT
     END-IF.
*在庫調整ＦＬＧ
     MOVE  SPACE                TO    DEN-D93.
*在庫調整日付　　
     MOVE  SYS-DATEW            TO    DEN-D94.
*
     REWRITE                          DEN-REC.
     ADD       1                 TO   DEN-CNT.
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
     DISPLAY "DEN-CNT  = " DEN-CNT  UPON CONS.
     DISPLAY "SKP1-CNT = " SKP1-CNT UPON CONS.
*
     CLOSE     CANSENK1  SHTDENL1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
