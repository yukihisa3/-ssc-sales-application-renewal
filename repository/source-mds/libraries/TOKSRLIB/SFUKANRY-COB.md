# SFUKANRY

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SFUKANRY.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　社内振替　　　                    *
*    モジュール名　　　　：　完納区分セット　　　　　　　　　　*
*    作成日／作成者　　　：　2016/01/15 INOUE                  *
*    処理概要　　　　　　：　帳票・ＣＳＶ作成用に、            *
*      　　　　　　　　　　　振替情報ファイルからレコード　　　*
*                            を抽出する。　                    *
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　　　　　　　　　　　　　　        *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SFUKANRY.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2016/01/15.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*振替情報ファイル
     SELECT   SFRHEDL1  ASSIGN    TO        DA-01-VI-SFRHEDL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HED-F01
                                            HED-F02
                                            HED-F03
                                            HED-F04
                                            HED-F05
                                            HED-F06
                                            HED-F07
                                            HED-F08
                        FILE  STATUS   IS   HED-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    振替情報ファイル
******************************************************************
 FD  SFRHEDL1
                        LABEL RECORD   IS   STANDARD.
     COPY     SFRHEDL1  OF        XFDLIB
              JOINING   HED  AS   PREFIX.
*
*****************************************************************
 WORKING-STORAGE        SECTION.
*
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  RW-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WK-HED-F112             PIC  9(08)     VALUE  ZERO.
*
 01  WK-ST.
     03  HED-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SFUKANRY".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SFUKANRY".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SFUKANRY".
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
*------------------------------------------------------------
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
 01  G-TIME.
     03  G-TIME-HH                PIC  Z9.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  G-TIME-MM                PIC  Z9.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  G-TIME-SS                PIC  Z9.
 LINKAGE                SECTION.
 01  PARA-IN-BUMON          PIC   X(04).
 01  PARA-IN-TANCD          PIC   X(02).
 01  PARA-IN-DENKU          PIC   X(02).
 01  PARA-IN-NENDO          PIC   9(04).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-BUMON
                                       PARA-IN-TANCD
                                       PARA-IN-DENKU
                                       PARA-IN-NENDO.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SFRHEDL1.
     MOVE      "SFRHEDL1"   TO   AB-FILE.
     MOVE      HED-STATUS   TO   AB-STS.
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
     OPEN     I-O       SFRHEDL1.
     DISPLAY  MSG-START UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
*      DISPLAY "LINK-YMD8 = " LINK-OUT-YMD8 UPON CONS
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     SYS-MMW        TO   WK-SYSMM.
     MOVE     SYS-DDW        TO   WK-SYSDD.
*
     ACCEPT   SYS-TIME       FROM TIME.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    RW-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
     MOVE     SPACE           TO   HED-REC.
     INITIALIZE                    HED-REC.
*
     MOVE     PARA-IN-DENKU   TO   HED-F01.
     MOVE     PARA-IN-NENDO   TO   HED-F02.
*
     START    SFRHEDL1  KEY   >=   HED-F01
                                   HED-F02
                                   HED-F03
                                   HED-F04
                                   HED-F05
                                   HED-F06
                                   HED-F07
                                   HED-F08
         INVALID   KEY
              MOVE      9    TO   END-FG
              DISPLAY NC"＃対象データ無１＃" UPON CONS
              GO   TO   INIT-EXIT
     END-START.
*
 INIT-010.
*
     PERFORM  SFRHEDL1-READ-SEC.
     IF       END-FG  =  9
              DISPLAY NC"＃対象データ無２＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 SFRHEDL1-READ-SEC    SECTION.
*
     READ  SFRHEDL1
           NEXT  AT  END
           MOVE       9          TO        END-FG
           GO                    TO        SFRHEDL1-READ-EXIT
     END-READ.
*
     ADD              1          TO        RD-CNT.
*
     IF  RD-CNT(6:3)  =  "000" OR "500"
         DISPLAY "READ-CNT = " RD-CNT  UPON  CONS
     END-IF.
*
**** DISPLAY "MSG = " HED-F01 " - " HED-F02  UPON CONS.
     IF  PARA-IN-DENKU  =  HED-F01
     AND PARA-IN-NENDO  =  HED-F02
         CONTINUE
     ELSE
         MOVE         9          TO        END-FG
     END-IF.
*
     IF  HED-F24  =  "1"
         GO                      TO        SFRHEDL1-READ-SEC
     END-IF.
*
 SFRHEDL1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*完納区分セット
     MOVE    "1"                  TO   HED-F24.
*完納日セット
     MOVE    SYS-DATEW            TO   HED-F25.
*完納者部門
     MOVE    PARA-IN-BUMON        TO   HED-F26.
*完納者担当者ＣＤ
     MOVE    PARA-IN-TANCD        TO   HED-F27.
*
     ADD     1                    TO   RW-CNT.
*
     REWRITE  HED-REC.
*
     PERFORM  SFRHEDL1-READ-SEC.
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
     DISPLAY "# READ-CNT = " RD-CNT  " #" UPON CONS.
     DISPLAY "# REWT-CNT = " RW-CNT  " #" UPON CONS.
*
     CLOSE     SFRHEDL1.
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
