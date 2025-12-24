# SKE0930B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SKE0930B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品システム                  *
*　　モジュール名　　　　：　検品ＤＴ削除                      *
*　　作成日／更新日　　　：　2009/06/15                        *
*　　作成者／更新者　　　：　NAV                               *
*　　処理概要　　　　　　：　梱包ＤＴの削除処理を行う。       *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE0930B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          09/06/15.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  出荷情報データ  >>---*
     SELECT   RUIKONF   ASSIGN         DA-01-VI-RUIKONL3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  RUI-F05
                                       RUI-F06
                        STATUS         RUI-ST.
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
 FD  RUIKONF            LABEL RECORD   IS   STANDARD.
     COPY        RUIKONF     OF      XFDLIB
                 JOINING     RUI     PREFIX.
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
     03  IN-CNT         PIC  9(08).
     03  DEL-CNT        PIC  9(08).
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-SAKUJO          PIC  9(02)     VALUE  ZERO.
 01  WK-HENKAN          PIC  9(06)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  RUI-ST             PIC  X(02).
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
 01  LINK-AREA2.
     03  LINK-IN-KBN    PIC  X(01).
     03  LINK-IN-YMD6   PIC  9(06).
     03  LINK-IN-YMD8   PIC  9(08).
     03  LINK-OUT-RET   PIC  X(01).
     03  LINK-OUT-YMD8  PIC  9(08).
*
 LINKAGE                SECTION.
 01  PARA-TOKCD         PIC  9(08).
****************************************************************
 PROCEDURE              DIVISION    USING    PARA-TOKCD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 出荷情報データ >>--*
 RUI-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RUIKONF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE0930B RUIKONF    ERROR " RUI-ST " "
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
     DISPLAY  "### SKE0930B HJYOKEN    ERROR " JYO-ST " "
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
     OPEN     I-O       RUIKONF.
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
     MOVE    "DATE1"         TO   JYO-F02.
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
     COMPUTE  WK-ZAI-12  =  WK-ZAI-12  -  WK-SAKUJO.
     IF  WK-ZAI-12  <=  ZERO
         ADD    -1      TO        WK-ZAI-11
         MOVE   12      TO        WK-ZAI-12
     END-IF.
*----<< 削除基準日出力 >>-*
     DISPLAY "DELETE DATE = "  WK-ZAIKO-DATE  UPON CONS.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*出荷情報データスタート
     PERFORM  RUIKONF-START-SEC.
     IF   END-FLG  =  "END"
          DISPLAY NC"＃＃対象データ無し＃＃" UPON CONS
          GO            TO     INIT-RTN-EXIT
     END-IF.
*出荷情報データ初期読込み
     PERFORM  RUIKONF-READ-SEC.
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
     DELETE   RUIKONF.
     ADD      1          TO       DEL-CNT.
*    出荷情報読込み
     PERFORM  RUIKONF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    RUIKONF   HJYOKEN.
*
     DISPLAY  "## READ   CNT => " IN-CNT    UPON CONS.
     DISPLAY  "## DELETE CNT => " DEL-CNT   UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE0930B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　出荷情報データスタート　　　　　　　　　　　　　　　　　　*
****************************************************************
 RUIKONF-START-SEC      SECTION.
*スタートキーセット
     MOVE     1              TO   RUI-F05.
     START    RUIKONF   KEY  IS  >=  RUI-F05 RUI-F06
              INVALID
              MOVE    "END"  TO   END-FLG
     END-START.
*
 RUIKONF-START-EXIT.
     EXIT.
****************************************************************
*　　出荷情報データ読込み　　　　　　　　　　　　　　　　　　　*
****************************************************************
 RUIKONF-READ-SEC       SECTION.
*    売上伝票ファイル参照
     READ     RUIKONF   AT  END
              MOVE  "END"    TO   END-FLG
              GO             TO   RUIKONF-READ-EXIT
     END-READ.
 READ010.
*    読込みカウント
     ADD      1              TO   IN-CNT.
 READ020.
*    読込み件数表示
     IF       IN-CNT(6:3)  =  "000"  OR  "500"
            DISPLAY "READ-CNT = " IN-CNT " : " DEL-CNT UPON CONS
     END-IF.
 READ030.
*    送信ＦＬＧが１以外は対象外
     IF       RUI-F05  NOT =  1
              MOVE  "END"    TO   END-FLG
              GO             TO   RUIKONF-READ-EXIT
     END-IF.
 READ040.
*    送信ＦＬＧ＝１で対象日付以下の場合
     IF   RUI-F05  =  1
     AND  RUI-F06  <  WK-ZAIKO-DATE
          CONTINUE
     ELSE
          GO        TO   RUIKONF-READ-SEC
     END-IF
*
 RUIKONF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
