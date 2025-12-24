# SSY9490B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY9490B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受注　　　　　　　　　　　        *
*    サブシステム　　　　：　ヨドバシカメラ　　　　　　　　　　*
*    モジュール名　　　　：　ＥＸＣＥＬ納期数量一括変更Ｆ取込　*
*    作成日／作成者　　　：　2023/04/12 NAV INOUE              *
*    処理概要　　　　　　：　納期数量一括変更ＷＫを読み、各ファ*
*                          　イルを参照しチェックする　　　　　*
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2023/04/12 TAKAHASHI新規作成  流用：SSY9420B.TOKSRLIB
* 2023/06/13 TAKAHASHI仕様変更  最大月チェックを２→４月に変更
*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY9490B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2023/04/12.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<<納期数量一括変更取込ファイル>>--*
     SELECT   RCVKKTEI  ASSIGN    TO   DA-01-S-RCVKKTEI
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   RCV-ST.
*----<<伝票データ>>--*
     SELECT   SHTDENLA  ASSIGN         DA-01-VI-SHTDENLA
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  DEN-F46   DEN-F47
                                       DEN-F01   DEN-F48
                                       DEN-F02   DEN-F04
                                       DEN-F051  DEN-F07
                                       DEN-F112  DEN-F03
                        WITH  DUPLICATES
                        FILE STATUS    DEN-ST.
*---<<基本情報ファイル>>---*
     SELECT   YODJOHL4  ASSIGN    TO   DA-01-VI-YODJOHL4
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JOH-F15  JOH-F16
                                       JOH-F17  JOH-F28
                                       JOH-F14  JOH-F18
                                       JOH-F19
                        WITH  DUPLICATES
                        FILE STATUS    JOH-ST.
*---<<納期数量一括変更累積データ>>---*
     SELECT   YODHENL1  ASSIGN    TO   DA-01-VI-YODHENL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  HEN-F01  HEN-F02
                                       HEN-F03  HEN-F04
                                       HEN-F05
                        FILE STATUS    HEN-ST.
*----<<条件ファイル>>---*
     SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-ST.
*----<<納期数量一括変更チェックデータ>>---*
     SELECT   YODHECKF  ASSIGN    TO   DA-01-S-YODHECKF
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   HEC-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<納期数量一括変更取込ファイル>>--*
 FD  RCVKKTEI             BLOCK     CONTAINS 56   RECORDS
                          LABEL     RECORD   IS   STANDARD.
 01  RCVWK-REC.
     03  FILLER                   PIC   X(72).
*----<< 伝票データ >>--*
 FD  SHTDENLA           LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 基本情報ファイル >>--*
 FD  YODJOHL4           LABEL RECORD   IS   STANDARD.
     COPY     YODJOHL4  OF        XFDLIB
              JOINING   JOH       PREFIX.
*---<<納期数量一括変更累積データ>>---*
 FD  YODHENL1           LABEL RECORD   IS   STANDARD.
     COPY     YODHENL1  OF        XFDLIB
              JOINING   HEN       PREFIX.
*---<<条件ファイル>>---*
 FD  JYOKEN1
     LABEL     RECORD    IS   STANDARD.
     COPY      JYOKEN1   OF   XFDLIB
     JOINING   JYO       AS   PREFIX.
*----<<納期数量一括変更チェックデータ>>-*
 FD  YODHECKF             BLOCK     CONTAINS  1   RECORDS
                          LABEL     RECORD   IS   STANDARD.
 01  HECWK-REC.
     03  FILLER                   PIC   X(600).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*納期数量一括変更取込Ｆレコード
     COPY     RCVKKTEI  OF  XFDLIB
     JOINING  RCV           PREFIX.
*納期数量一括変更取込Ｆレコード
     COPY     YODHECKF  OF  XFDLIB
     JOINING  HEC           PREFIX.
 01  FLAGS.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG             PIC  X(01)  VALUE  SPACE.
     03  CHK-ERR-FLG         PIC  X(01)  VALUE  SPACE.
     03  SHTDENLA-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  YODJOHL4-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  JYOKEN1-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  HIDUKE1-CHK-FLG     PIC  X(01)  VALUE  SPACE.
     03  HIDUKE2-CHK-FLG     PIC  X(01)  VALUE  SPACE.
     03  HIDUKE3-CHK-FLG     PIC  X(01)  VALUE  SPACE.
     03  SURYO-CHK-FLG       PIC  X(01)  VALUE  SPACE.
     03  KAKUTEI-CHK-FLG     PIC  X(01)  VALUE  SPACE.
 01  WORK-KBN.
     03  ERR-KBN             PIC  X(01)  VALUE  SPACE.
     03  CHK-ERR-KBN         PIC  X(01)  VALUE  SPACE.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  RCV-ST            PIC  X(02)        VALUE  SPACE.
 01  DEN-ST            PIC  X(02)        VALUE  SPACE.
 01  JOH-ST            PIC  X(02)        VALUE  SPACE.
 01  HEN-ST            PIC  X(02)        VALUE  SPACE.
 01  JYO-ST            PIC  X(02)        VALUE  SPACE.
 01  HEC-ST            PIC  X(02)        VALUE  SPACE.
*
*----<< CNT        >>--*
 01  RCV-CNT           PIC  9(07)        VALUE  ZERO.
 01  DEN-CNT           PIC  9(07)        VALUE  ZERO.
 01  JOH-CNT           PIC  9(07)        VALUE  ZERO.
 01  HEN-CNT           PIC  9(07)        VALUE  ZERO.
 01  HEC-CNT           PIC  9(07)        VALUE  ZERO.
*----<< WORK       >>--*
 01  WK-WORK.
     03 WK-KAKUT       PIC  X(01)        VALUE  ZERO.
     03 WK-NOUDT       PIC  9(08)        VALUE  ZERO.
     03 WK-SURYO       PIC  9(09)        VALUE  ZERO.
     03 WK-SURYO1      PIC  9(09)        VALUE  ZERO.
     03 WK-SEQ         PIC  9(06)        VALUE  ZERO.
 01  IX                PIC  9(02)        VALUE  ZERO.
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
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
 01  WK-SYS-DATE             PIC  9(08).
 01  FILLER                  REDEFINES   WK-SYS-DATE.
     03  WK-SYS-YY           PIC  9(04).
     03  WK-SYS-MM           PIC  9(02).
     03  WK-SYS-DD           PIC  9(02).
*
 01  WRK-NAME.
     03  WRK-KANA            PIC  X(15)        VALUE SPACE.
*
 01  GETUDO                  PIC  9(02).
 01  GETUDO2                 PIC S9(02).
 01  GETUDO2-YMD.
     03  GETUDO2-YY          PIC  9(04).
     03  GETUDO2-MM          PIC  9(02).
     03  GETUDO2-DD          PIC  9(02).
 01  GETUDO2-YMDR      REDEFINES  GETUDO2-YMD.
     03  GETUDO2-YYMMR       PIC  9(06).
     03  FILLER              PIC  X(02).
 01  ACOS-DATE               PIC  9(08).
 01  FILLER                  REDEFINES      ACOS-DATE.
     03  ACOS-YYMM           PIC  9(06).
     03  ACOS-DD             PIC  9(02).
 01  START-YYMM              PIC  9(06).
 01  FILLER                  REDEFINES      START-YYMM.
     03  START-YY            PIC  9(04).
     03  START-MM            PIC  9(02).
 01  END-YYMM                PIC  9(06).
 01  FILLER                  REDEFINES      END-YYMM.
     03  END-YY              PIC  9(04).
     03  END-MM              PIC  9(02).
 01  CHK-DATE                PIC  9(08).
 01  FILLER                  REDEFINES      CHK-DATE.
     03  CHK-YYMM            PIC  9(06).
     03  FILLER              REDEFINES      CHK-YYMM.
         05  CHK-YY          PIC  9(04).
         05  CHK-MM          PIC  9(02).
     03  CHK-DD              PIC  9(02).
 01  SAV-NOU-DATE2           PIC  9(08)    VALUE ZERO.
 01  SAV-NOU-DATE.
     03  SAV-NOU-YYMM        PIC  9(06).
     03  SAV-NOU-DD          PIC  9(02).
 01  SAV-HNOU-DATE2          PIC  9(08)    VALUE ZERO.
 01  SAV-HNOU-DATE.
     03  SAV-HNOU-YYMM       PIC  9(06).
     03  SAV-HNOU-DD         PIC  9(02).
 01  SAV-HTENCD              PIC  9(05)    VALUE ZERO.
 01  SAV-CYU-DATE            PIC  9(08)    VALUE ZERO.
 01  SAV-SYU-DATE            PIC  9(08)    VALUE ZERO.
 01  SAV-ZENGETU             PIC  9(08).
 01  SAV-ACOS-YYMM.
     03  SAV-ACOS-YY         PIC  9(04).
     03  SAV-ACOS-MM         PIC  9(02).
 01  SAV-ACOS-MMS            PIC  9(02).
***
 01  CHK-DATE-WORK.
     03  CHK-01              PIC  9(04).
     03  CHK-02              PIC  9(02).
     03  MATUBI              PIC  X(24)  VALUE
         "312831303130313130313031".
     03  FILLER              REDEFINES   MATUBI.
         05  WK-MATUBI       PIC  9(02)  OCCURS  12.
***検収日開始日
 01  KAISI-YMD               PIC  9(08).
 01  FILLER                  REDEFINES   KAISI-YMD.
     03  KAISI-YY            PIC  9(04).
     03  KAISI-MM            PIC  9(02).
     03  KAISI-DD            PIC  9(02).
***検収日終了日
 01  SYURYO-YMD              PIC  9(08).
 01  FILLER                  REDEFINES   SYURYO-YMD.
     03  SYURYO-YY           PIC  9(04).
     03  SYURYO-MM           PIC  9(02).
     03  SYURYO-DD           PIC  9(02).
*計算領域
 01  WRK-AREA2.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*
 01  SEC-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                    SECTION.
 01  LINK-IN-SOKCD          PIC 9(08).
 01  LINK-IN-DSOKCD         PIC 9(04).
 01  LINK-IN-BUMONCD        PIC 9(08).
 01  LINK-IN-TANCD          PIC X(02).
 01  LINK-OUT-ERRKBN        PIC X(01).
 01  LINK-OUT-TDATE         PIC 9(08).
 01  LINK-OUT-TTIME         PIC 9(06).
*
****************************************************************
 PROCEDURE              DIVISION  USING
                                  LINK-IN-SOKCD
                                  LINK-IN-DSOKCD
                                  LINK-IN-BUMONCD
                                  LINK-IN-TANCD
                                  LINK-OUT-ERRKBN
                                  LINK-OUT-TDATE
                                  LINK-OUT-TTIME.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<<納期数量一括変更取込ファイル>>--*
 RCVKKTEI-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCVKKTEI.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9490B RCVKKTEI ERROR " RCV-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 伝票データ >>--*
 SHTDENLA-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENLA.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9490B SHTDENLA ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 基本情報ファイル　　　 >>--*
 YODJOHL4-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      YODJOHL4.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9490B YODJOHL4 ERROR " JOH-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*---<<納期数量一括変更累積データ>>---*
 YODHENL1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      YODHENL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9490B YODHENL1 ERROR " HEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*---<<納期数量一括変更累積データ>>---*
 JYOKEN1-ERR               SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JYOKEN1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9490B JYOKEN1  ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<<納期数量一括変更チェックデータ>>---*
 YODHECKF-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      YODHECKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9490B YODHECKF ERROR " HEC-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL  END-FLG = "END".
     PERFORM  300-END-RTN.
 000-PROG-CNTL-EXIT.
     EXIT     PROGRAM.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
*
 100-INIT-01.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW WK-SYS-DATE
     ELSE
         MOVE ZERO           TO   SYS-DATEW WK-SYS-DATE
     END-IF.
*
     ACCEPT   SYS-TIME       FROM TIME.
*
 100-INIT-02.
     OPEN     INPUT     RCVKKTEI  JYOKEN1.
     OPEN     I-O       SHTDENLA  YODJOHL4  YODHENL1.
     OPEN     OUTPUT    YODHECKF.
*
*条件ファイルより日付取得
*　（経理月）
     MOVE     "58"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG  =  "INV"
              DISPLAY   "HJYOKEN INV KEY=58"  UPON CONS
              MOVE      "END"     TO   END-FLG
              GO   TO   100-INIT-RTN-EXIT
     END-IF.
     MOVE     JYO-F04        TO   GETUDO.
     MOVE     JYO-F04        TO   GETUDO2.
*TEST↓
     DISPLAY "-----------------------------" UPON CONS.
     DISPLAY NC"取得：条件Ｆ（経理月）＝"  GETUDO   UPON CONS.
     DISPLAY "-----------------------------" UPON CONS.
*TEST↑
*　（ＡＣＯＳ用締日）
     MOVE     "99"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG  =  "INV"
              DISPLAY   "HJYOKEN INV KEY=99"  UPON CONS
              MOVE      "END"     TO   END-FLG
              GO   TO   100-INIT-RTN-EXIT
     END-IF.
     EVALUATE GETUDO
         WHEN 1
              MOVE      JYO-F04        TO   ACOS-DATE
         WHEN 2
              MOVE      JYO-F05        TO   ACOS-DATE
         WHEN 3
              MOVE      JYO-F06        TO   ACOS-DATE
         WHEN 4
              MOVE      JYO-F07        TO   ACOS-DATE
         WHEN 5
              MOVE      JYO-F08        TO   ACOS-DATE
         WHEN 6
              MOVE      JYO-F09        TO   ACOS-DATE
         WHEN 7
              MOVE      JYO-F10        TO   ACOS-DATE
         WHEN 8
              MOVE      JYO-F11        TO   ACOS-DATE
         WHEN 9
              MOVE      JYO-F12        TO   ACOS-DATE
         WHEN 10
              MOVE      JYO-F12A       TO   ACOS-DATE
         WHEN 11
              MOVE      JYO-F12B       TO   ACOS-DATE
         WHEN 12
              MOVE      JYO-F12C       TO   ACOS-DATE
         WHEN OTHER
              DISPLAY  "### ｹﾞﾂﾄﾞ ｲｼﾞｮｳ  ｹﾞﾂﾄﾞ= " GETUDO " ###"
                                       UPON CONS
              MOVE      "END"     TO   END-FLG
              GO   TO   100-INIT-RTN-EXIT
     END-EVALUATE.
*TEST↓
     DISPLAY "-------------------------------------" UPON CONS.
     DISPLAY NC"条件Ｆ（ＡＣＯＳ締日）＝"  ACOS-DATE UPON CONS.
     DISPLAY "-------------------------------------" UPON CONS.
*TEST↑
*
     MOVE     ACOS-DATE      TO   GETUDO2-YMD.
     MOVE     99             TO   GETUDO2-DD.
     IF       GETUDO2        =    12
              COMPUTE        GETUDO2-YY     =
                             GETUDO2-YY     -        1
              MOVE     12    TO             GETUDO2-MM
     ELSE
              MOVE      GETUDO2   TO        GETUDO2-MM
     END-IF.
*
     MOVE     SYS-YY         TO   START-YY
                                  END-YY.
     MOVE     SYS-MM         TO   START-MM
                                  END-MM.
**   DISPLAY "START-YY   = " START-YY " - " END-YY UPON CONS.
**   DISPLAY "START-MM   = " START-MM " - " END-MM UPON CONS.
**   DISPLAY "WK-SYS-YY  = " WK-SYS-YY             UPON CONS.
*#2023/06/13 NAV ST
*****ADD      2              TO   END-MM.
     ADD      4              TO   END-MM.
*#2023/06/13 NAV ED
     IF       END-MM  >  12
              MOVE      1         TO   END-MM
              COMPUTE   END-YY   =  WK-SYS-YY  +  1
     ELSE
              MOVE      WK-SYS-YY   TO      END-YY
     END-IF.
     SUBTRACT 1              FROM START-MM.
**   DISPLAY "START-MM1  = " START-MM " - " END-MM UPON CONS.
     IF       START-MM  <  1
              MOVE      12        TO   START-MM
              COMPUTE   START-YY  =  WK-SYS-YY  -  1
     ELSE
              MOVE      WK-SYS-YY      TO   START-YY
     END-IF.
*
     MOVE     GETUDO2-YYMMR  TO      START-YYMM.
*
*TEST↓
     DISPLAY "------------------------------" UPON CONS.
     DISPLAY NC"算出：開始日付＝"  START-YYMM UPON CONS.
     DISPLAY NC"算出：終了日付＝"  END-YYMM   UPON CONS.
     DISPLAY "------------------------------" UPON CONS.
*
     MOVE    SPACE           TO      CHK-ERR-KBN.
 100-INIT-03.
*　　納期数量一括変更取込ファイルＲＥＡＤ
     PERFORM RCVKKTEI-READ-SEC.
     IF      END-FLG    =     "END"
             DISPLAY NC"納期数量一括変更取込Ｆ無"  UPON CONS
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      メイン処理　　　　　　　　　                *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*各エラーＦＬＧ解除
     MOVE    SPACE                TO   ERR-FLG.
     MOVE    SPACE                TO   HIDUKE1-CHK-FLG.
     MOVE    SPACE                TO   HIDUKE2-CHK-FLG.
     MOVE    SPACE                TO   HIDUKE3-CHK-FLG.
     MOVE    SPACE                TO   SURYO-CHK-FLG.
     ADD     1                    TO   WK-SEQ.
*基本情報ファイル検索
     MOVE    RCV-F01              TO   JOH-F15.
     MOVE    RCV-F02              TO   JOH-F16.
     MOVE    RCV-F03              TO   JOH-F17.
     MOVE    RCV-F09              TO   JOH-F28.
     MOVE    RCV-F06              TO   JOH-F14.
     MOVE    RCV-F04              TO   JOH-F18.
     MOVE    RCV-F05              TO   JOH-F19.
     PERFORM YODJOHL4-READ-SEC.
*売上伝票ファイル検索
     MOVE    RCV-F01              TO   DEN-F46.
     MOVE    RCV-F02              TO   DEN-F47.
     MOVE    RCV-F03              TO   DEN-F01.
     MOVE    RCV-F09              TO   DEN-F48.
     MOVE    RCV-F04              TO   DEN-F02.
     MOVE    ZERO                 TO   DEN-F04.
     MOVE    40                   TO   DEN-F051.
     MOVE    RCV-F07              TO   DEN-F07.
     MOVE    RCV-F06              TO   DEN-F112.
     MOVE    RCV-F05              TO   DEN-F03.
*****DISPLAY "DEN-F46  = " DEN-F46   UPON CONS.
*    DISPLAY "DEN-F47  = " DEN-F47   UPON CONS.
*    DISPLAY "DEN-F01  = " DEN-F01   UPON CONS.
*    DISPLAY "DEN-F48  = " DEN-F48   UPON CONS.
*    DISPLAY "DEN-F02  = " DEN-F02   UPON CONS.
*    DISPLAY "DEN-F04  = " DEN-F04   UPON CONS.
*    DISPLAY "DEN-F051 = " DEN-F051  UPON CONS.
*    DISPLAY "DEN-F07  = " DEN-F07   UPON CONS.
*    DISPLAY "DEN-F112 = " DEN-F112  UPON CONS.
**** DISPLAY "DEN-F03  = " DEN-F03   UPON CONS.
     PERFORM SHTDENLA-READ-SEC.
*納品日論理チェック
     IF   RCV-F11  NOT =  SPACE
          MOVE     "2"            TO   LINK-IN-KBN
          MOVE     ZERO           TO   LINK-IN-YMD6
          MOVE     RCV-F11        TO   LINK-IN-YMD8
          MOVE     ZERO           TO   LINK-OUT-RET
          MOVE     ZERO           TO   LINK-OUT-YMD8
          CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                          LINK-IN-YMD6
                                          LINK-IN-YMD8
                                          LINK-OUT-RET
                                          LINK-OUT-YMD8
          IF   LINK-OUT-RET   = 9
               MOVE  "1"      TO   HIDUKE1-CHK-FLG  ERR-FLG
          END-IF
     END-IF.
*当月以前の納品日の場合／２ヶ月後チェック
     IF   RCV-F11  NOT =  SPACE
          MOVE   RCV-F11              TO   WK-NOUDT
          IF   START-YYMM  >  WK-NOUDT(1:6)
               MOVE   "1"             TO   HIDUKE2-CHK-FLG
               MOVE   "1"             TO   ERR-FLG
          END-IF
          IF   END-YYMM  <  WK-NOUDT(1:6)
               MOVE   "1"             TO   HIDUKE3-CHK-FLG
               MOVE   "1"             TO   ERR-FLG
          END-IF
     END-IF.
*確定区分チェック（確定済の場合はエラー）
     IF   RCV-F10  NOT =  SPACE
          IF  YODJOHL4-INV-FLG  NOT =  "INV"
              IF  JOH-F36  =  "1"
                  MOVE  "1"           TO   KAKUTEI-CHK-FLG
                  MOVE  "1"           TO   ERR-FLG
              END-IF
          END-IF
     END-IF.
*数量チェック
     IF   RCV-F12  NOT =  SPACE
          MOVE   RCV-F12          TO   WK-SURYO
          MOVE   DEN-F50          TO   WK-SURYO1
          IF SHTDENLA-INV-FLG  NOT =  "INV"
        DISPLAY "DEN-F50 = " WK-SURYO " - " WK-SURYO1 UPON CONS
*************IF  DEN-F50  <  WK-SURYO
             IF  WK-SURYO1  <  WK-SURYO
                 MOVE   "1"           TO   SURYO-CHK-FLG
                 MOVE   "1"           TO   ERR-FLG
             END-IF
          END-IF
     END-IF.
     IF  SHTDENLA-INV-FLG  =  "INV"
            MOVE  "1"                 TO  ERR-FLG
     END-IF.
     IF  YODJOHL4-INV-FLG  =  "INV"
            MOVE  "1"                 TO  ERR-FLG
     END-IF.
*
     PERFORM  YODHENL1-WRITE-SEC.
*
***  DISPLAY "## ERR-FLG = " ERR-FLG UPON CONS.
     IF  ERR-FLG  NOT =  SPACE
         MOVE  "1"                TO   CHK-ERR-FLG
         PERFORM YODHECKF-WRITE-SEC
     END-IF.
*売上伝票ファイルＲＥＡＤ
 200-MAIN-06.
     PERFORM  RCVKKTEI-READ-SEC.
*
 200-MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     CLOSE    RCVKKTEI  JYOKEN1  SHTDENLA  YODJOHL4  YODHENL1
              YODHECKF.
*
     DISPLAY  NC"納期数量一括ＤＴ件数＝" RCV-CNT NC"件"
                                                     UPON CONS.
     DISPLAY  NC"納期数量一括作成件数＝" HEN-CNT NC"件"
                                                     UPON CONS.
     DISPLAY  NC"納期数量一括ＣＶ件数＝" HEC-CNT NC"件"
                                                     UPON CONS.
*
     MOVE   CHK-ERR-FLG           TO     LINK-OUT-ERRKBN.
     MOVE   SYS-DATEW             TO     LINK-OUT-TDATE.
     MOVE   SYS-TIMEW             TO     LINK-OUT-TTIME.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    納期数量一括変更取込ファイル
*--------------------------------------------------------------*
 RCVKKTEI-READ-SEC       SECTION.
     MOVE  "RCVKKTEI-READ1-SEC"    TO  S-NAME.
*
     READ   RCVKKTEI
               AT  END
                        MOVE "END" TO  END-FLG
                        GO         TO  RCVKKTEI-READ-EXIT
     END-READ.
*
     ADD       1                   TO  RCV-CNT.
     IF  RCV-CNT(5:3)  =  "000" OR  "500"
         DISPLAY "## RCV-CNT = " RCV-CNT  UPON  CONS
     END-IF.
*
     MOVE      RCVWK-REC           TO  RCV-REC.
*
 RCVKKTEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    売上伝票ファイルＲＥＡＤ
*--------------------------------------------------------------*
 SHTDENLA-READ-SEC       SECTION.
     MOVE  "SHTDENLA-READ-SEC"      TO  S-NAME.
*
 SHTDENLA-READ-01.
     READ   SHTDENLA
            INVALID
            MOVE   "INV"           TO  SHTDENLA-INV-FLG
            NOT  INVALID
            MOVE   SPACE           TO  SHTDENLA-INV-FLG
     END-READ.
*
 SHTDENLA-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    基本情報ファイルＲＥＡＤ
*--------------------------------------------------------------*
 YODJOHL4-READ-SEC      SECTION.
     MOVE  "YODJOHL4-READ-SEC"     TO  S-NAME.
*
 YODJOHL4-READ-01.
     READ   YODJOHL4
               INVALID
                        MOVE "INV" TO  YODJOHL4-INV-FLG
               NOT INVALID
                        MOVE SPACE TO  YODJOHL4-INV-FLG
     END-READ.
*
 YODJOHL4-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    条件ファイルＲＥＡＤ
*--------------------------------------------------------------*
 JYOKEN1-READ-SEC       SECTION.
     MOVE  "JYOKEN1-READ-SEC"      TO  S-NAME.
*
 JYOKEN1-READ-01.
     READ   JYOKEN1
               INVALID
                        MOVE "INV" TO  JYOKEN1-INV-FLG
               NOT INVALID
                        MOVE SPACE TO  JYOKEN1-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    納期数量一括変更累積データ　ＷＲＩＴＥ
*--------------------------------------------------------------*
 YODHENL1-WRITE-SEC    SECTION.
     MOVE  "YODHENL1-WRITE-SEC"    TO  S-NAME.
*
**   DISPLAY "WK-SEQ = " WK-SEQ UPON CONS.
 YODHENL1-WRITE-SEC-01.
     MOVE   SPACE                  TO  HEN-REC.
     INITIALIZE                        HEN-REC.
     MOVE   SYS-DATEW              TO  HEN-F01.
     MOVE   SYS-TIMEW              TO  HEN-F02.
     MOVE   LINK-IN-TANCD          TO  HEN-F03.
     MOVE   LINK-IN-BUMONCD        TO  HEN-F04.
     MOVE   WK-SEQ                 TO  HEN-F05.
     MOVE   RCV-REC                TO  HEN-A00.
     MOVE   ERR-FLG                TO  HEN-F80.
     IF  SHTDENLA-INV-FLG  =  "INV"
            MOVE  "1"              TO  HEN-F81
     END-IF.
     IF  YODJOHL4-INV-FLG  =  "INV"
            MOVE  "1"              TO  HEN-F82
     END-IF.
     MOVE   HIDUKE1-CHK-FLG        TO  HEN-F83.
     MOVE   HIDUKE2-CHK-FLG        TO  HEN-F84.
     MOVE   HIDUKE3-CHK-FLG        TO  HEN-F85.
     MOVE   SURYO-CHK-FLG          TO  HEN-F86.
     MOVE   KAKUTEI-CHK-FLG        TO  HEN-F87.
     ADD    1                      TO  HEN-CNT.
     WRITE  HEN-REC.
*
 YODHENL1-WRITE-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    納期数量一括変更累積チェックデータＷＲＩＴＥ
*--------------------------------------------------------------*
 YODHECKF-WRITE-SEC    SECTION.
     MOVE  "YODHECKF-WRITE-SEC"    TO  S-NAME.
*
 YODHECKF-WRITE-SEC-01.
     MOVE   SPACE                  TO  HEC-REC.
     INITIALIZE                        HEC-REC.
*
     MOVE   ","                    TO  HEC-K01  HEC-K02  HEC-K03
                                       HEC-K04  HEC-K05  HEC-K06
                                       HEC-K07  HEC-K08  HEC-K09
                                       HEC-K10  HEC-K11  HEC-K12
                                       HEC-K13  HEC-K14  HEC-K15
                                       HEC-K16  HEC-K17  HEC-K18
                                       HEC-K19  HEC-K20  HEC-K21
                                       HEC-K22  HEC-K23  HEC-K24
                                       HEC-K25  HEC-K26  HEC-K27
                                       HEC-K28  HEC-K29  HEC-K30
                                       HEC-K31  HEC-K32  HEC-K33
                                       HEC-K34  HEC-K35  HEC-K36
                                       HEC-K37  HEC-K38  HEC-K081.
     MOVE  X"28"                   TO  HEC-S08.
     MOVE  X"29"                   TO  HEC-E08.
*メッセージタイプ
     MOVE  JOH-F01                 TO  HEC-F01.
*購買発注番号
     MOVE  JOH-F02                 TO  HEC-F02.
*発注伝票日付
     MOVE  JOH-F03                 TO  HEC-F03.
*仕入先コード
     MOVE  JOH-F04                 TO  HEC-F04.
*発注先コード
     MOVE  JOH-F05                 TO  HEC-F05.
*出荷先コード
     MOVE  JOH-F06                 TO  HEC-F06.
*明細行番号
     MOVE  JOH-F07                 TO  HEC-F07.
*品目番号
     MOVE  JOH-F08                 TO  HEC-F08.
*商品名
     MOVE  JOH-F38                 TO  HEC-F081.
*明細フリーテキスト
     MOVE  JOH-F09                 TO  HEC-F09.
*明細単価
     MOVE  JOH-F10                 TO  HEC-F10.
*お客様伝票番号
     MOVE  JOH-F11                 TO  HEC-F11.
*配送場所
     MOVE  JOH-F12                 TO  HEC-F12.
*発注数量
     MOVE  JOH-F13                 TO  HEC-F13.
*納入期日
     MOVE  JOH-F14                 TO  HEC-F14.
*バッチ日付
     MOVE  JOH-F15                 TO  HEC-F15.
*バッチ時刻
     MOVE  JOH-F16                 TO  HEC-F16.
*バッチ取引先
     MOVE  JOH-F17                 TO  HEC-F17.
*基幹伝票番号
     MOVE  JOH-F18                 TO  HEC-F18.
*基幹行番号
     MOVE  JOH-F19                 TO  HEC-F19.
*取込担当部門
     MOVE  LINK-IN-BUMONCD         TO  HEC-F20.
*取込担当者
     MOVE  LINK-IN-TANCD           TO  HEC-F21.
*エラー区分１
     IF  SHTDENLA-INV-FLG  =  "INV"
            MOVE  "1"              TO  HEC-F22
     END-IF.
*エラー区分２
     IF  YODJOHL4-INV-FLG  =  "INV"
            MOVE  "1"              TO  HEC-F23
     END-IF.
*エラー区分３
     MOVE  HIDUKE1-CHK-FLG         TO  HEC-F24.
*エラー区分４
     MOVE  HIDUKE2-CHK-FLG         TO  HEC-F25.
*エラー区分５
     MOVE  HIDUKE3-CHK-FLG         TO  HEC-F26.
*エラー区分６
     MOVE  SURYO-CHK-FLG           TO  HEC-F27.
*エラー区分７
     MOVE  KAKUTEI-CHK-FLG         TO  HEC-F28.
*エラー区分８
     MOVE  SPACE                   TO  HEC-F29.
*エラー区分９
     MOVE  SPACE                   TO  HEC-F30.
*エラー区分１０
     MOVE  SPACE                   TO  HEC-F31.
*エラー区分１１
     MOVE  SPACE                   TO  HEC-F32.
*エラー区分１２
     MOVE  SPACE                   TO  HEC-F33.
*エラー区分１３
     MOVE  SPACE                   TO  HEC-F34.
*エラー区分１４　
     MOVE  SPACE                   TO  HEC-F35.
*エラー区分１５
     MOVE  SPACE                   TO  HEC-F36.
*出荷確定区分
     MOVE  RCV-F10                 TO  HEC-F37
*変更納期
     IF    RCV-F11  NOT =  SPACE
           MOVE   RCV-F11          TO  WK-NOUDT
           MOVE   WK-NOUDT         TO  HEC-F38
     END-IF.
*変更後数量
     IF    RCV-F12  NOT =  SPACE
           MOVE   RCV-F12          TO  WK-SURYO
           MOVE   WK-SURYO         TO  HEC-F39
     END-IF.
     ADD    1                      TO  HEC-CNT.
     MOVE   HEC-REC                TO  HECWK-REC.
     WRITE  HECWK-REC.
*
 YODHECKF-WRITE-SEC-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
