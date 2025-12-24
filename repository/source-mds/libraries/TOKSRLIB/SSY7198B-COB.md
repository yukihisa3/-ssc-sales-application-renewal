# SSY7198B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY7198B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　Ｊ本田出荷検品システム            *
*　　モジュール名　　　　：　Ｊ本田出荷情報データ削除          *
*　　作成日／更新日　　　：　2014/11/19                        *
*　　作成者／更新者　　　：　NAV                               *
*　　処理概要　　　　　　：　Ｊ本田出荷情報データの削除を行なう*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY7198B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/11/19.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  出荷情報データ  >>---*
     SELECT   HDSYUKF   ASSIGN         DA-01-VI-HDSYUKL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DJH-F01  DJH-F02
                                       DJH-F04  DJH-F06
                        STATUS         KMS-ST.
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
*----<< 出荷情報データ >>--*
 FD  HDSYUKF            LABEL RECORD   IS   STANDARD.
     COPY        HDSYUKF     OF      XFDLIB
                 JOINING     DJH     PREFIX.
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
 01  KMS-ST             PIC  X(02).
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
 01  WK-ZAI-12-WK       PIC S9(02)     VALUE  ZERO.
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
 KMS-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HDSYUKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SSY7198B HDSYUKF    ERROR " KMS-ST " "
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
     DISPLAY  "### SSY7198B HJYOKEN    ERROR " JYO-ST " "
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
     OPEN     I-O       HDSYUKF.
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
             DISPLAY NC"＃当月度" " = " WK-HENKAN " " NC"＃"
                     UPON CONS
     END-READ.
*----<< 削除月数取得 >>-*
     MOVE    83              TO   JYO-F01.
     MOVE    "DATE3"         TO   JYO-F02.
     READ    HJYOKEN
             INVALID
             DISPLAY "HJYOKEN DEL INVALID" JYO-F01 " - " JYO-F02
                      UPON CONS
                      STOP RUN
             NOT  INVALID
             MOVE     JYO-F04     TO   WK-SAKUJO
             DISPLAY NC"＃削除月" " = " WK-SAKUJO " " NC"＃"
                     UPON CONS
     END-READ.
*----<< 基準日付算出 >>-*
     MOVE     99                  TO   WK-ZAI-2.
     COMPUTE  WK-ZAI-12-WK  =  WK-ZAI-12  -  WK-SAKUJO.
     IF  WK-ZAI-12-WK  <=  ZERO
         ADD    -1      TO        WK-ZAI-11
         MOVE   12      TO        WK-ZAI-12
         COMPUTE WK-ZAI-12  =  12  +  WK-ZAI-12-WK
     ELSE
         MOVE WK-ZAI-12-WK TO     WK-ZAI-12
     END-IF.
*----<< 削除基準日出力 >>-*
     DISPLAY "DELETE DATE = "  WK-ZAIKO-DATE  UPON CONS.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*出荷情報データ初期読込み
     PERFORM  HDSYUKF-READ-SEC.
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
*    出荷情報データ削除
     DELETE   HDSYUKF.
     ADD      1          TO       DEL-CNT.
*    出荷情報読込み
     PERFORM  HDSYUKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    HDSYUKF   HJYOKEN.
*
     DISPLAY  "## READ   CNT => " IN-CNT    UPON CONS.
     DISPLAY  "## DELETE CNT => " DEL-CNT   UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY7198B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　出荷情報データ読込み　　　　　　　　　　　　　　　　　　　*
****************************************************************
 HDSYUKF-READ-SEC       SECTION.
*    売上伝票ファイル参照
     READ     HDSYUKF   AT  END
              MOVE  "END"    TO   END-FLG
              GO             TO   HDSYUKF-READ-EXIT
     END-READ.
*    読込みカウント
     ADD      1              TO   IN-CNT.
*    読込み件数表示
     IF       IN-CNT(4:3)  =  "000"  OR  "500"
            DISPLAY "READ-CNT = " IN-CNT " : " DEL-CNT UPON CONS
     END-IF.
*    日付チェック
     IF       DJH-F011  >  WK-ZAIKO-DATE
              MOVE  "END"    TO   END-FLG
              GO             TO   HDSYUKF-READ-EXIT
     END-IF.
*    ＤＴ作成ＦＬＧチェック
     IF       DJH-F081  =  "1"
              CONTINUE
     ELSE
              GO             TO   HDSYUKF-READ-SEC
     END-IF.
*    ＤＴ送信ＦＬＧチェック
     IF       DJH-F091  =  "1"
              CONTINUE
     ELSE
              GO             TO   HDSYUKF-READ-SEC
     END-IF.
*
 HDSYUKF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
