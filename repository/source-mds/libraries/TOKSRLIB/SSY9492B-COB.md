# SSY9492B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY9492B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受注　　　　　　　　　　　        *
*    サブシステム　　　　：　ヨドバシカメラ　　　　　　　　　　*
*    モジュール名　　　　：　ＥＸＣＥＬ納期数量一括変更Ｆ更新　*
*    作成日／作成者　　　：　2023/04/12 NAV TAKAHASHI          *
*    処理概要　　　　　　：　納期数量一括変更Ｆを読み、基本情報*
*                          　売上伝票Ｆの更新を行う。在庫更新有*
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2023/04/12 TAKAHASHI新規作成  流用：SSY9420B.TOKSRLIB
*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY9492B.
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
*---<<納期数量一括変更累積データ>>---*
     SELECT   YODHENL1  ASSIGN    TO   DA-01-VI-YODHENL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  HEN-F01  HEN-F02
                                       HEN-F03  HEN-F04
                                       HEN-F05
                        FILE STATUS    HEN-ST.
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
     SELECT   YODJOHL2  ASSIGN    TO   DA-01-VI-YODJOHL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JOH-F15  JOH-F16
                                       JOH-F17  JOH-F03
                                       JOH-F18  JOH-F19
                        WITH  DUPLICATES
                        FILE STATUS    JOH-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*---<<納期数量一括変更累積データ>>---*
 FD  YODHENL1           LABEL RECORD   IS   STANDARD.
     COPY     YODHENL1  OF        XFDLIB
              JOINING   HEN       PREFIX.
*----<< 伝票データ >>--*
 FD  SHTDENLA           LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 基本情報ファイル >>--*
 FD  YODJOHL2           LABEL RECORD   IS   STANDARD.
     COPY     YODJOHL2  OF        XFDLIB
              JOINING   JOH       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  SHTDENLA-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  YODJOHL2-INV-FLG    PIC  X(03)  VALUE  SPACE.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  DEN-ST            PIC  X(02)        VALUE  SPACE.
 01  JOH-ST            PIC  X(02)        VALUE  SPACE.
 01  HEN-ST            PIC  X(02)        VALUE  SPACE.
*
*----<< CNT        >>--*
 01  HEN-CNT           PIC  9(07)        VALUE  ZERO.
 01  DEN-CNT           PIC  9(07)        VALUE  ZERO.
 01  JOH-CNT           PIC  9(07)        VALUE  ZERO.
 01  SKIP-CNT          PIC  9(07)        VALUE  ZERO.
 01  HEN-CNT1          PIC  9(07)        VALUE  ZERO.
 01  HEN-CNT2          PIC  9(07)        VALUE  ZERO.
 01  HEN-CNT3          PIC  9(07)        VALUE  ZERO.
*----<< WORK       >>--*
 01  WK-WORK.
     03 WK-KAKUT       PIC  X(01)        VALUE  ZERO.
     03 WK-NOUDT       PIC  9(08)        VALUE  ZERO.
     03 WK-SURYO       PIC  9(09)        VALUE  ZERO.
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
*
 01  NOUHIN-DATE             PIC  X(08).
 01  FILLER                  REDEFINES      NOUHIN-DATE.
     03  NOUHIN-DATE-HEN     PIC  9(08).
*
 01  HENKO-SURYO             PIC  X(09).
 01  FILLER                  REDEFINES      HENKO-SURYO.
     03  HENKO-SURYO-HEN     PIC  9(09).
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
*----<< 在庫更新サブルーチン用 >>--*
 01  ZAIKO-UPDATE-AREA.
     03  ZAIKO-MODE          PIC  X(01)     VALUE  "1".
     03  ZAIKO-TORICD        PIC  9(08).
     03  ZAIKO-AITESHOHINCD  PIC  X(13).
     03  ZAIKO-SOKOCD        PIC  X(02).
     03  ZAIKO-SHOHINCD      PIC  X(08).
     03  ZAIKO-HINTANCD      PIC  X(08).
     03  ZAIKO-NOHINBI       PIC  9(08).
     03  HENKOMAE-SURYO      PIC  9(09).
     03  HENKOGO-SURYO       PIC  9(09)     VALUE  ZERO.
     03  ZAIKO-TANABAN       PIC  X(06).
     03  ZAIKO-HIKIATE-FLG   PIC  X(01).
*
 LINKAGE                    SECTION.
 01  LINK-IN-TDATE          PIC 9(08).
 01  LINK-IN-TTIME          PIC 9(06).
 01  LINK-IN-BUMONCD        PIC X(04).
 01  LINK-IN-TANCD          PIC X(02).
*
****************************************************************
 PROCEDURE              DIVISION  USING
                                  LINK-IN-TDATE
                                  LINK-IN-TTIME
                                  LINK-IN-BUMONCD
                                  LINK-IN-TANCD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*---<<納期数量一括変更累積データ>>---*
 YODHENL1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      YODHENL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9492B YODHENL1 ERROR " HEN-ST " "
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
     DISPLAY  "### SSY9492B SHTDENLA ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 基本情報ファイル　　　 >>--*
 YODJOHL2-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      YODJOHL2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9492B YODJOHL2 ERROR " JOH-ST " "
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
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     ACCEPT   SYS-TIME       FROM TIME.
*
 100-INIT-02.
     OPEN     INPUT     YODHENL1.
     OPEN     I-O       SHTDENLA  YODJOHL2.
*
 100-INIT-03.      *>納期数量一括変更ファイルスタート
     MOVE     SPACE               TO   HEN-REC.
     INITIALIZE                        HEN-REC.
     MOVE     LINK-IN-TDATE       TO   HEN-F01.
     MOVE     LINK-IN-TTIME       TO   HEN-F02.
     MOVE     LINK-IN-TANCD       TO   HEN-F03.
     MOVE     LINK-IN-BUMONCD     TO   HEN-F04.
     START  YODHENL1  KEY  IS  >=  HEN-F01  HEN-F02  HEN-F03
                                   HEN-F04  HEN-F05
            INVALID
            MOVE  "END"           TO   END-FLG
            DISPLAY NC"納期変更一括累積Ｆ無１！" UPON CONS
            GO          TO   100-INIT-RTN-EXIT
     END-START.
 100-INIT-04.      *>納期数量一括変更ファイル読込
     PERFORM YODHENL1-READ-SEC.
     IF      END-FLG    =     "END"
             DISPLAY NC"納期変更一括累積Ｆ無２！" UPON CONS
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      メイン処理　　　　　　　　　                *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
     ADD     1                    TO   WK-SEQ.
*基本情報ファイル検索
     MOVE    HEN-A01              TO   JOH-F15.
     MOVE    HEN-A02              TO   JOH-F16.
     MOVE    HEN-A03              TO   JOH-F17.
     MOVE    HEN-A08              TO   JOH-F03.
     MOVE    HEN-A04              TO   JOH-F18.
     MOVE    HEN-A05              TO   JOH-F19.
     PERFORM YODJOHL2-READ-SEC.
*存在しない場合は次レコードへスキップ
     IF  YODJOHL2-INV-FLG  =  "INV"
         ADD 1                    TO   SKIP-CNT
         GO                       TO   200-MAIN-06
     END-IF.
*売上伝票ファイル検索
     MOVE    HEN-A01              TO   DEN-F46.
     MOVE    HEN-A02              TO   DEN-F47.
     MOVE    HEN-A03              TO   DEN-F01.
     MOVE    HEN-A09              TO   DEN-F48.
     MOVE    HEN-A04              TO   DEN-F02.
     MOVE    ZERO                 TO   DEN-F04.
     MOVE    40                   TO   DEN-F051.
     MOVE    HEN-A07              TO   DEN-F07.
     MOVE    HEN-A06              TO   DEN-F112.
     MOVE    HEN-A05              TO   DEN-F03.
     PERFORM SHTDENLA-READ-SEC.
*存在しない場合は次レコードへスキップ
     IF  SHTDENLA-INV-FLG  =  "INV"
         ADD 1                    TO   SKIP-CNT
         GO                       TO   200-MAIN-06
     END-IF.
*更新判定
     IF   HEN-A10  NOT =  SPACE  *>倉庫確定区分更新
          MOVE     SYS-DATEW      TO   JOH-F34   *>倉庫確定区分
          MOVE     SYS-TIMEW      TO   JOH-F35   *>倉庫確定日付
          ADD      1              TO   HEN-CNT1
     END-IF.
*
     IF   HEN-A11  NOT =  SPACE  *>納期更新
          MOVE     HEN-A11        TO   NOUHIN-DATE
          MOVE   NOUHIN-DATE-HEN  TO   JOH-F14  DEN-F112
          MOVE     SYS-DATEW      TO   JOH-F32   *>納品日変更日
          ADD      1              TO   HEN-CNT2
     END-IF.
*
     IF   HEN-A12  NOT =  SPACE  *>数量変更
          MOVE     HEN-A12        TO   HENKO-SURYO
**********在庫更新
          MOVE     "2"            TO   ZAIKO-MODE
          MOVE     DEN-F01        TO   ZAIKO-TORICD
          MOVE     DEN-F25        TO   ZAIKO-AITESHOHINCD
          MOVE     DEN-F08        TO   ZAIKO-SOKOCD
          MOVE     DEN-F1411      TO   ZAIKO-SHOHINCD
          MOVE     DEN-F1412      TO   ZAIKO-HINTANCD
          MOVE     DEN-F112       TO   ZAIKO-NOHINBI
          MOVE     DEN-F15        TO   HENKOMAE-SURYO
          MOVE   HENKO-SURYO-HEN  TO   HENKOGO-SURYO
          MOVE     DEN-F49        TO   ZAIKO-TANABAN
          MOVE     DEN-F27D       TO   ZAIKO-HIKIATE-FLG
*----<< サブルーチンコール >>--*
          CALL    "SJK9010B" USING     ZAIKO-MODE
                                       ZAIKO-TORICD
                                       ZAIKO-AITESHOHINCD
                                       ZAIKO-SOKOCD
                                       ZAIKO-SHOHINCD
                                       ZAIKO-HINTANCD
                                       ZAIKO-NOHINBI
                                       HENKOMAE-SURYO
                                       HENKOGO-SURYO
                                       ZAIKO-TANABAN
                                       ZAIKO-HIKIATE-FLG
          MOVE   HENKO-SURYO-HEN  TO   DEN-F15   JOH-F13 *>数量
          MOVE  ZAIKO-HIKIATE-FLG TO   DEN-F27D *>引当済区分
          MOVE     SYS-DATEW      TO   JOH-F33  *>数量変更日
     END-IF.
*各ファイル更新
*    納期数量一括変更ファイル
     REWRITE  JOH-REC.
     ADD         1                TO   JOH-CNT.
*    売上伝票ファイル
     REWRITE  DEN-REC.
     ADD         1                TO   DEN-CNT.
*納期数量一括変更ファイルＲＥＡＤ
 200-MAIN-06.
     PERFORM  YODHENL1-READ-SEC.
*
 200-MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     CLOSE    YODHENL1  SHTDENLA  YODJOHL2.
*
     DISPLAY  "## READ-CNT    = " HEN-CNT    " ##"    UPON CONS.
     DISPLAY  "## SKIP-CNT    = " SKIP-CNT   " ##"    UPON CONS.
     DISPLAY  "## DEN-CNT     = " DEN-CNT    " ##"    UPON CONS.
     DISPLAY  "## JOH-CNT     = " JOH-CNT    " ##"    UPON CONS.
     DISPLAY  "## ｶｸﾃｲｸﾌﾞﾝ    = " HEN-CNT1   " ##"    UPON CONS.
     DISPLAY  "## ﾉｳｷﾍﾝｺｳ     = " HEN-CNT2   " ##"    UPON CONS.
     DISPLAY  "## ｽｳﾘｮｳﾍﾝｺｳ   = " HEN-CNT3   " ##"    UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    納期数量一括変更取込ファイル
*--------------------------------------------------------------*
 YODHENL1-READ-SEC       SECTION.
     MOVE  "YODHENL1-READ-SEC"     TO  S-NAME.
*
     READ   YODHENL1
            NEXT  AT  END
            MOVE "END"             TO  END-FLG
            GO                     TO  YODHENL1-READ-EXIT
     END-READ.
*
     ADD    1                      TO  HEN-CNT.
     IF   HEN-CNT(5:3)  =  "000"  OR  "500"
          DISPLAY "RD-CNT = " HEN-CNT  UPON CONS
     END-IF.
*
     IF   LINK-IN-TDATE    =  HEN-F01
     AND  LINK-IN-TTIME    =  HEN-F02
     AND  LINK-IN-TANCD    =  HEN-F03
     AND  LINK-IN-BUMONCD  =  HEN-F04
          CONTINUE
     ELSE
          MOVE  "END"             TO  END-FLG
     END-IF.
*
 YODHENL1-READ-EXIT.
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
 YODJOHL2-READ-SEC      SECTION.
     MOVE  "YODJOHL2-READ-SEC"     TO  S-NAME.
*
 YODJOHL2-READ-01.
     READ   YODJOHL2
               INVALID
                        MOVE "INV" TO  YODJOHL2-INV-FLG
               NOT INVALID
                        MOVE SPACE TO  YODJOHL2-INV-FLG
     END-READ.
*
 YODJOHL2-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
