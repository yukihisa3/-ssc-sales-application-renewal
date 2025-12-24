# NDL0010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NDL0010B.COB`

## ソースコード

```cobol
****************************************************************
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　Ｄ３６５連携　　　　　　　        *
*　　モジュール名　　　　：　Ｄ３６５連携累積データ削除        *
*　　作成日／更新日　　　：　2021/12/13                        *
*　　作成者／更新者　　　：　NAV                               *
*　　処理概要　　　　　　：　Ｄ３６５連携累積データの削除を　　*
*                            行なう。                          *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NDL0010B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          21/02/118
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  D365連携データ  >>---*
     SELECT   URIKEJF   ASSIGN         DA-01-VI-URIKEJL4
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  RUI-F48   RUI-F49
                                       RUI-F01   RUI-F02
                                       RUI-F04   RUI-F03
                                       RUI-F05   RUI-F06
                                       RUI-F07
                        STATUS         RUI-ST.
*----<< 条件ファイル >>--*
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01   JYO-F02
                        STATUS         JYO-ST.
*---<<  バックアップ  >>---*
     SELECT   URIKEJBB  ASSIGN    TO        DA-01-S-URIKEJBB
                        FILE      STATUS    IS   UBK-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*---<<  D365連携データ  >>---*
 FD  URIKEJF            LABEL RECORD   IS   STANDARD.
     COPY        URIKEJF     OF      XFDLIB
                 JOINING     RUI     PREFIX.
*----<< 条件ファイル >>--*
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY        HJYOKEN     OF      XFDLIB
                 JOINING     JYO     PREFIX.
*----<< 条件ファイル >>--*
 FD  URIKEJBB.
     COPY     URIKEJF   OF        XFDLIB
              JOINING   UBK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)     VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(07).
     03  DEL-CNT        PIC  9(07).
     03  BAK-CNT        PIC  9(07).
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-SAKUJO          PIC  9(02)     VALUE  ZERO.
 01  WK-HENKAN          PIC  9(06)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  RUI-ST             PIC  X(02).
 01  JYO-ST             PIC  X(02).
 01  UBK-ST             PIC  X(02).
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
 01  WK-KEISAN          PIC S9(05).
 01  WK-KEISAN1         PIC  9(05).
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
*---<<  D365連携データ  >>---*
 RUI-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      URIKEJF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NDL0010B URIKEJF    ERROR " RUI-ST " "
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
     DISPLAY  "### NDL0010B HJYOKEN    ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< バックアップ >>--*
 UBK-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      URIKEJBB.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NDL0010B URIKEJBB   ERROR " UBK-ST " "
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
     OPEN     I-O       URIKEJF.
     OPEN     INPUT     HJYOKEN.
     OPEN     OUTPUT    URIKEJBB.
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
             DISPLAY NC"＃月次締日" " = " WK-HENKAN UPON CONS
     END-READ.
*----<< 削除月数取得 >>-*
     MOVE    83              TO   JYO-F01.
     MOVE    "D365DEL"       TO   JYO-F02.
     READ    HJYOKEN
             INVALID
             DISPLAY "HJYOKEN DEL INVALID" JYO-F01 " - " JYO-F02
                      UPON CONS
                      STOP RUN
             NOT  INVALID
             MOVE     JYO-F04     TO   WK-SAKUJO
     END-READ.
     DISPLAY NC"削除基準月" " = " WK-SAKUJO  UPON CONS.
*----<< 基準日付算出 >>-*
     MOVE     99                  TO   WK-ZAI-2.
     COMPUTE  WK-KEISAN  =  WK-ZAI-12  -  WK-SAKUJO.
     DISPLAY "WK-KEISAN = " WK-KEISAN  UPON CONS.
     IF  WK-KEISAN  <=  ZERO
         ADD    -1      TO        WK-ZAI-11
         MOVE   12      TO        WK-ZAI-12
         MOVE WK-KEISAN TO        WK-KEISAN1
         COMPUTE  WK-ZAI-12 = WK-ZAI-12 -  WK-KEISAN1
     ELSE
         MOVE WK-KEISAN TO        WK-KEISAN1
         COMPUTE  WK-ZAI-12 = WK-ZAI-12 -  WK-SAKUJO
     END-IF.
*----<< 削除基準日出力 >>-*
     DISPLAY "DELETE DATE = "  WK-ZAIKO-DATE  UPON CONS.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*出荷情報データ初期読込み
     PERFORM  URIKEJF-READ-SEC.
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
*
     MOVE     SPACE      TO       UBK-REC.
     INITIALIZE                   UBK-REC.
     MOVE     RUI-REC    TO       UBK-REC.
     WRITE    UBK-REC.
*    Ｄ３６５連携データ削除
     DELETE   URIKEJF.
     ADD      1          TO       DEL-CNT.
*    出荷情報読込み
     PERFORM  URIKEJF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    URIKEJF   HJYOKEN.
*
     DISPLAY  "## READ   CNT => " IN-CNT    UPON CONS.
     DISPLAY  "## DELETE CNT => " DEL-CNT   UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NDL0010B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　出荷情報データ読込み　　　　　　　　　　　　　　　　　　　*
****************************************************************
 URIKEJF-READ-SEC       SECTION.
*    売上伝票ファイル参照
     READ     URIKEJF   AT  END
              MOVE  "END"    TO   END-FLG
              GO             TO   URIKEJF-READ-EXIT
     END-READ.
*    読込みカウント
     ADD      1              TO   IN-CNT.
*    読込み件数表示
     IF       IN-CNT(5:3)  =  "000"  OR  "500"
            DISPLAY "READ-CNT = " IN-CNT " : " DEL-CNT UPON CONS
     END-IF.
*    日付チェック
     IF       RUI-F48  >  WK-ZAIKO-DATE
              MOVE  "END"    TO   END-FLG
              GO             TO   URIKEJF-READ-EXIT
     END-IF.
*
 URIKEJF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
