****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　棚卸
*    サブシステム　　　　：　検品システム　　　　　　　　　
*    モジュール名　　　　：　棚卸確定データ取込
*    作成日／作成者　　　：　2018/12/26 INOUE
*    処理概要　　　　　　：　検品システムから受け取った棚卸確定
*                            データを取り込む（累積する）。
*    更新日／更新者      ：  2019/02/01 INOUE
*                            棚卸確定データレイアウト変更
*
*　
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE0430B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2018/12/26.
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
*棚卸確定データ
     SELECT   RCVTANXX  ASSIGN    TO        DA-01-S-RCVTANXX
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    IS   RCV-STATUS.
*棚卸累積データ
     SELECT   RUITANL1  ASSIGN    TO        DA-01-VI-RUITANL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       RUI-F03  RUI-F01
                        FILE      STATUS    IS   RUI-STATUS.
*棚卸原票データ
     SELECT   ZTANADT1  ASSIGN    TO        DA-01-VI-ZTANADT1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       ZTA-F01
                        FILE      STATUS    IS   ZTA-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    棚卸確定データ
******************************************************************
*↓2019/02/01
*FD  RCVTANXX           BLOCK CONTAINS 50   RECORDS
 FD  RCVTANXX           BLOCK CONTAINS 30   RECORDS
*↑2019/02/01
                        LABEL RECORD   IS   STANDARD.
     COPY     RCVTANXX  OF        XFDLIB
              JOINING   RCV  AS   PREFIX.
*
******************************************************************
*    棚卸累積データ
******************************************************************
 FD  RUITANL1           LABEL RECORD   IS   STANDARD.
     COPY     RUITANL1  OF        XFDLIB
              JOINING   RUI       PREFIX.
******************************************************************
*    棚卸原票データ
******************************************************************
 FD  ZTANADT1
                        LABEL RECORD   IS   STANDARD.
     COPY     ZTANADT1  OF        XFDLIB
              JOINING   ZTA  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  RD-CNT                  PIC  9(07)     VALUE  ZERO.
 01  WT-CNT                  PIC  9(07)     VALUE  ZERO.
 01  RW-CNT                  PIC  9(07)     VALUE  ZERO.
*
*    ﾌﾗｸﾞ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  INV-FG                  PIC  9(01)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE.
         05  SYS-YY        PIC 9(02).
         05  SYS-MM        PIC 9(02).
         05  SYS-DD        PIC 9(02).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  RCV-STATUS        PIC  X(02).
     03  RUI-STATUS        PIC  X(02).
     03  ZTA-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NKE0430B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0430B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0430B".
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
 01  WK-RUI-F112                  PIC       9(08)  VALUE  ZERO.
*
 LINKAGE                SECTION.
 01  PARA-IN-BUMON          PIC   X(04).
 01  PARA-IN-TANTOU         PIC   X(02).
 01  PARA-IN-TDATE          PIC   9(08).
 01  PARA-IN-TTIME          PIC   9(06).
 01  PARA-OUT-KENSU         PIC   9(07).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-BUMON
                                       PARA-IN-TANTOU
                                       PARA-IN-TDATE
                                       PARA-IN-TTIME
                                       PARA-OUT-KENSU.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RCVTANXX.
     MOVE      "RCVTANXX"   TO   AB-FILE.
     MOVE      RCV-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RUITANL1.
     MOVE      "RUITANL1"   TO   AB-FILE.
     MOVE      RUI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZTANADT1.
     MOVE      "ZTANADT1"   TO   AB-FILE.
     MOVE      ZTA-STATUS   TO   AB-STS.
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
     OPEN     INPUT     RCVTANXX ZTANADT1.
     OPEN     I-O       RUITANL1.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG  RD-CNT  WT-CNT  RW-CNT.
     MOVE     ZERO      TO        IN-CNT  OUT-CNT.
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
     PERFORM  RCVTANXX-READ-SEC.
     IF       END-FG   =   9
              DISPLAY NC"取込対象データなし．．" UPON CONS
              MOVE    4010     TO   PROGRAM-STATUS
              STOP    RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"      TO   S-NAME.
*
 MAIN-01.
*存在チェック
*    場所ＣＤ
     MOVE     RCV-F03        TO   RUI-F03.
*    棚卸_
     MOVE     RCV-F01        TO   RUI-F01.
     PERFORM  RUITANL1-READ-SEC.
     IF       INV-FG   =     9
              GO             TO   MAIN-02
     ELSE
              GO             TO   MAIN-03
     END-IF.
*
 MAIN-02.
     MOVE     SPACE          TO   RUI-REC.
     INITIALIZE                   RUI-REC.
*場所ＣＤ
     MOVE     RCV-F03        TO   RUI-F03.
*棚卸_
     MOVE     RCV-F01        TO   RUI-F01.
*
 MAIN-03.
*部門ＣＤ
     MOVE     RCV-F02        TO   RUI-F02.
*棚番
     MOVE     RCV-F04        TO   RUI-F04.
*JANCD
     MOVE     RCV-F05        TO   RUI-F05.
*サカタ商品ＣＤ＋品単
     MOVE     RCV-F06        TO   RUI-F06.
*帳簿在庫数
*↓2019/02/01
*    COMPUTE  RUI-F07   =    RCV-F07  /  100.
     MOVE     RCV-F07        TO   RUI-F07.
*↑2019/02/01
*棚卸数量
*↓2019/02/01
*    COMPUTE  RUI-F08   =    RCV-F08  /  100.
     MOVE     RCV-F08        TO   RUI-F08.
*↑2019/02/01
*差数
*↓2019/01/07
*    COMPUTE  RUI-F09   =    RCV-F09  /  100.
     COMPUTE  RUI-F09   =    RUI-F08  -  RUI-F07.
*↑2019/01/07
*↓2019/02/01
*担当者CD１(実施者)
*    MOVE     RCV-F10        TO   RUI-F10.
     MOVE     RCV-F17        TO   RUI-F10.
*担当者CD２(立会者)
*    MOVE     RCV-F11        TO   RUI-F11.
     MOVE     RCV-F18        TO   RUI-F11.
*担当者CD３(承認者)
*    MOVE     RCV-F12        TO   RUI-F12.
     MOVE     RCV-F19        TO   RUI-F12.
*取込者部門CD
     MOVE     PARA-IN-BUMON  TO   RUI-F13.
*取込者担当者CD
     MOVE     PARA-IN-TANTOU TO   RUI-F14.
*取込日
     MOVE     PARA-IN-TDATE  TO   RUI-F15.
*取込時刻
     MOVE     PARA-IN-TTIME  TO   RUI-F16.
*↓2019/02/01
*年度・ストック_
     MOVE     RCV-F01        TO   ZTA-F01.
*T↓
*    DISPLAY  "ZTA-F01=" ZTA-F01 UPON CONS.
*T↑
     READ     ZTANADT1
       INVALID
              MOVE   ZERO    TO   RUI-F17
              MOVE   SPACE   TO   RUI-F18
*T↓
*             MOVE   99      TO   RUI-F17
*             MOVE  "?????"  TO   RUI-F18
*T↑
       NOT INVALID
              MOVE   ZTA-F13 TO   RUI-F17
              MOVE   ZTA-F09 TO   RUI-F18
     END-READ.
*↑2019/02/01
*予備領域FIL:INIT(SPACE)
*
*棚卸累積データ出力・更新
     IF       INV-FG   =     9
              WRITE    RUI-REC
     ELSE
              REWRITE  RUI-REC
              ADD      1     TO   RW-CNT
     END-IF.
     ADD      1              TO   WT-CNT.
*
 MAIN-010.
*
     PERFORM  RCVTANXX-READ-SEC.
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
     DISPLAY   NC"読込件数　　　＝"  RD-CNT          UPON CONS.
     MOVE      WT-CNT       TO   PARA-OUT-KENSU.
     DISPLAY   NC"取込件数　　　＝"  PARA-OUT-KENSU  UPON CONS.
     DISPLAY   NC"　内．更新件数＝"  RW-CNT          UPON CONS.
*
     CLOSE     RCVTANXX  RUITANL1  ZTANADT1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　棚卸確定ファイル　ＲＥＡＤ　　　　　　　　　　　*
****************************************************************
 RCVTANXX-READ-SEC      SECTION.
*
     MOVE "RCVTANXX-READ-SEC"  TO   S-NAME.
*
     READ     RCVTANXX
              AT END
                 MOVE    9     TO   END-FG
                 GO            TO   RCVTANXX-READ-EXIT
              NOT AT END
                 ADD     1     TO   RD-CNT
     END-READ.
*
 RCVTANXX-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　棚卸累積データ　存在チェック　　　　　　　　　　*
****************************************************************
 RUITANL1-READ-SEC      SECTION.
*
     MOVE "RUITANL1-READ-SEC"  TO   S-NAME.
*
     READ     RUITANL1
              INVALID
                 MOVE    9     TO   INV-FG
              NOT INVALID
                 MOVE    0     TO   INV-FG
     END-READ.
*
 TANRUIL1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
