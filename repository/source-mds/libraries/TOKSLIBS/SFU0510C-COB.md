# SFU0510C

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SFU0510C.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　在庫管理システム　　　　          *
*　　モジュール名　　　　：　実績連携Ｆ（オフコン累積用）削除  *
*　　作成日／更新日　　　：　2009/10/09                        *
*　　作成者／更新者　　　：　大野                              *
*　　処理概要　　　　　　：　条件Ｆより削除基準月数と在庫締月を*
*                  取得して、削除基準月を算出し、ＰＣデータ作成*
*                  日付順で実績連携Ｆ（オフコン連携用）を読み、*
*                  削除基準日以下のデータを削除する            *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SFU0510C.
 AUTHOR.                NAV.
 DATE-WRITTEN.          09/10/09.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  出荷情報データ  >>---*
     SELECT   PCJISSF   ASSIGN         DA-01-VI-PCJISSL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  PCJ-F98
                        STATUS         PCJ-ST.
*----<< 条件ファイル >>--*
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01   JYO-F02
                        STATUS         JYO-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 実績連携Ｆ（オフコン累積用）>>--*
 FD  PCJISSF            LABEL RECORD   IS   STANDARD.
     COPY        PCJISSL2    OF      XFDLIB
                 JOINING     PCJ     PREFIX.
*----<< 条件ファイル >>--*
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)     VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  DEL-CNT        PIC  9(06).
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-SAKUJO          PIC  9(02)     VALUE  ZERO.
 01  WK-HENKAN          PIC  9(06)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  PCJ-ST             PIC  X(02).
 01  JYO-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  WK-SYS-DATE        PIC  9(08).
 01  WK-SYS-TIME        PIC  9(06).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
*
*
 01  WK-SAK-DATE.
     03  SAK-YYYY       PIC  9(04).
     03  SAK-MM         PIC  9(02).
     03  SAK-DD         PIC  9(02).
*
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  WK-ZAIKO-DATE.
     03  WK-ZAI-1.
         05  WK-ZAI-11  PIC  9(04)     VALUE  ZERO.
         05  WK-ZAI-12  PIC  9(02)     VALUE  ZERO.
         05  WK-ZAI-12S PIC S9(02)     VALUE  ZERO.
         05  WK-ZAI-13  PIC  9(02)     VALUE  ZERO.
     03  WK-ZAI-2       PIC  9(02)     VALUE  ZERO.
 01  LINK-AREA2.
     03  LINK-IN-KBN    PIC  X(01).
     03  LINK-IN-YMD6   PIC  9(06).
     03  LINK-IN-YMD8   PIC  9(08).
     03  LINK-OUT-RET   PIC  X(01).
     03  LINK-OUT-YMD8  PIC  9(08).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 出荷情報データ >>--*
 PCJ-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PCJISSF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SFU0510B PCJISSF    ERROR " PCJ-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 条件ファイル >>--*
 JYO-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SSY8797B HJYOKEN    ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PROG-CNTL              SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     END-FLG   =   "END".
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
*    システム日付８桁変換
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     MOVE     LINK-OUT-YMD8  TO   WK-SYS-DATE.
     MOVE     SYS-TIME(1:6)  TO   WK-SYS-TIME.
*ファイルＯＰＥＮ
     OPEN     I-O       PCJISSF.
     OPEN     INPUT     HJYOKEN.
*----<< 削除基準月数取得 >>-*
     MOVE    99              TO   JYO-F01.
     MOVE    "JIS"           TO   JYO-F02.
     READ    HJYOKEN
             INVALID
             DISPLAY "HJYOKEN ZAI INVALID" JYO-F01 " - " JYO-F02
                      UPON CONS
                      STOP RUN
             NOT  INVALID
             MOVE     JYO-F04     TO   WK-SAKUJO
     END-READ.
*----<< 在庫締月数取得 >>-*
     MOVE    99              TO   JYO-F01.
     MOVE    "ZAI"           TO   JYO-F02.
     READ    HJYOKEN
             INVALID
             DISPLAY "HJYOKEN DEL INVALID" JYO-F01 " - " JYO-F02
                      UPON CONS
                      STOP RUN
             NOT  INVALID
             MOVE     JYO-F05     TO   WK-HENKAN
             MOVE     WK-HENKAN   TO   WK-ZAI-1
     END-READ.
*
*----<< 削除基準月算出 >>----*
     MOVE     99                  TO   WK-ZAI-2.
     COMPUTE  WK-ZAI-12S =  WK-ZAI-12  -  WK-SAKUJO.
     COMPUTE  WK-ZAI-12  =  WK-ZAI-12  -  WK-SAKUJO.
*
*****IF       WK-ZAI-12  >= 1
     IF       WK-ZAI-12S >= 1
              MOVE       WK-ZAI-11   TO        SAK-YYYY
              MOVE       WK-ZAI-12   TO        SAK-MM
              MOVE       99          TO        SAK-DD
     ELSE
*             WK-ZAI-12  <=  ZERO
              COMPUTE    SAK-YYYY    =  WK-ZAI-11 - 1
              COMPUTE    SAK-MM      =  12        - WK-ZAI-12
              MOVE       99          TO        SAK-DD
*
     END-IF.
*----<< 削除基準日出力 >>-*
     DISPLAY "DELETE DATE = "  WK-SAK-DATE  UPON CONS.
*ワークエリア　クリア
     INITIALIZE                COUNTERS.
     INITIALIZE                FLAGS.
*    MOVE     WK-SAK-DATE      TO           PCJ-F98.
     PERFORM  PCJISSF-READ-SEC.
     IF       END-FLG  =  "END"
              DISPLAY NC"＃＃対象データ無し＃＃" UPON CONS
     END-IF.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*    出荷情報データ削除
     DELETE   PCJISSF.
     ADD      1          TO       DEL-CNT.
*    出荷情報読込み
     PERFORM  PCJISSF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    PCJISSF   HJYOKEN.
*
     DISPLAY  "## READ   CNT => " IN-CNT    UPON CONS.
     DISPLAY  "## DELETE CNT => " DEL-CNT   UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SFU0510B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　出荷情報データ読込み　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PCJISSF-READ-SEC       SECTION.
*    売上伝票ファイル参照
     READ     PCJISSF   AT  END
              MOVE  "END"    TO   END-FLG
              GO             TO   PCJISSF-READ-EXIT
     END-READ.
*    読込みカウント
     ADD      1              TO   IN-CNT.
*    読込み件数表示
     IF       IN-CNT(3:4)  =  "0000"  OR  "5000"
            DISPLAY "READ-CNT = " IN-CNT " : " DEL-CNT UPON CONS
     END-IF.
*    日付チェック
     IF       PCJ-F98  >  WK-SAK-DATE
              MOVE  "END"    TO   END-FLG
              GO             TO   PCJISSF-READ-EXIT
     END-IF.
*    送信ＦＬＧチェック
*****IF       PCJ-F95  NOT =  "1"
*             GO             TO   PCJISSF-READ-SEC
*****END-IF.
*
 PCJISSF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
