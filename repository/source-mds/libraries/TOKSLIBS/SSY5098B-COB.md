# SSY5098B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY5098B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷管理（トステムビバ新EDI）
*　　モジュール名　　　　：　基本情報データ削除（月次）        *
*　　作成日／更新日　　　：　2010/03/26                        *
*　　作成者／更新者　　　：　NAV                               *
*　　処理概要　　　　　　：　基本情報データの削除を行なう      *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY5098B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/03/26.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  基本情報ファイル  >>---*
     SELECT   KHJOHOF   ASSIGN         DA-01-VI-KHJOHOL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  KHJ-F01  KHJ-F02
                                       KHJ-F03  KHJ-F04
                                       KHJ-F05  KHJ-F06
                                       KHJ-F07  KHJ-F08
                        STATUS         KHJ-ST.
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
*----<< 基本情報ファイル >>--*
 FD  KHJOHOF            LABEL RECORD   IS   STANDARD.
     COPY        KHJOHOF     OF      XFDLIB
                 JOINING     KHJ     PREFIX.
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
 01  KHJ-ST             PIC  X(02).
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
     03  WK-ZAI-2       PIC  9(02)     VALUE  ZERO.
 01  CHK-MONTH          PIC S9(02)     VALUE  ZERO.
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
*----<< 基本情報ファイル >>--*
 KMS-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KHJOHOF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SSY5098B DJJOHOF    ERROR " KHJ-ST " "
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
     DISPLAY  "### SSY5098B HJYOKEN    ERROR " JYO-ST " "
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
     OPEN     I-O       KHJOHOF.
     OPEN     INPUT     HJYOKEN.
*----<< 最終月次締日取得 >>-*
     MOVE    99              TO   JYO-F01.
     MOVE    "ZAI"           TO   JYO-F02.
     READ    HJYOKEN
             INVALID
             DISPLAY "HJYOKEN ZAI INVALID" JYO-F01 " - " JYO-F02
                      UPON CONS
                      STOP RUN
             NOT  INVALID
             MOVE     JYO-F05     TO   WK-HENKAN
             MOVE     WK-HENKAN   TO   WK-ZAI-1
     END-READ.
*----<< 削除月数取得 >>-*
     MOVE    83              TO   JYO-F01.
     MOVE    "DATE"          TO   JYO-F02.
     READ    HJYOKEN
             INVALID
             DISPLAY "HJYOKEN DEL INVALID" JYO-F01 " - " JYO-F02
                      UPON CONS
                      STOP RUN
             NOT  INVALID
             MOVE     JYO-F04     TO   WK-SAKUJO
     END-READ.
*----<< 基準日付算出 >>-*
     MOVE     99                  TO   WK-ZAI-2.
     MOVE     WK-ZAI-12           TO   CHK-MONTH.
     COMPUTE  CHK-MONTH  =  CHK-MONTH  -  WK-SAKUJO.
     COMPUTE  WK-ZAI-12  =  WK-ZAI-12  -  WK-SAKUJO.
     IF  CHK-MONTH      <=  ZERO
*        年を一つ前にする
         ADD    -1      TO        WK-ZAI-11
*   　　 月が負の時は、１２からその分を引いて月を合わせる。
         COMPUTE  WK-ZAI-12  = 12 -  WK-ZAI-12
     END-IF.
*----<< 削除基準日出力 >>-*
     DISPLAY "DELETE DATE = "  WK-ZAIKO-DATE  UPON CONS.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*基本情報ファイル読込み
     PERFORM  KHJOHOF-READ-SEC.
     IF   END-FLG  =  "END"
          DISPLAY NC"＃＃対象データ無し＃＃" UPON CONS
     END-IF.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*    基本情報データ削除
     DELETE   KHJOHOF.
     ADD      1          TO       DEL-CNT.
*    基本情報ファイル読込み
     PERFORM  KHJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    KHJOHOF   HJYOKEN.
*
     DISPLAY  "## READ   CNT => " IN-CNT    UPON CONS.
     DISPLAY  "## DELETE CNT => " DEL-CNT   UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY5098B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　基本情報ファイル読込み　　　　　　　　　　　　　　　　　　*
****************************************************************
 KHJOHOF-READ-SEC       SECTION.
*    基本情報ファイル参照
     READ     KHJOHOF   AT  END
              MOVE  "END"    TO   END-FLG
              GO             TO   KHJOHOF-READ-EXIT
     END-READ.
*    読込みカウント
     ADD      1              TO   IN-CNT.
*    読込み件数表示
     IF       IN-CNT(4:3)  =  "000"  OR  "500"
            DISPLAY "READ-CNT = " IN-CNT " : " DEL-CNT UPON CONS
     END-IF.
*    日付チェック
     IF       KHJ-F01  >  WK-ZAIKO-DATE
              MOVE  "END"    TO   END-FLG
              GO             TO   KHJOHOF-READ-EXIT
     END-IF.
*    送信ＦＬＧチェック
     IF       KHJ-F11  NOT =  "1"
              GO             TO   KHJOHOF-READ-SEC
     END-IF.
*
 KHJOHOF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
