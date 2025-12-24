****************************************************************
*　　顧客名　　　　　　　：　（株）サカタのタネ営業第２部様　　*
*　　業務名　　　　　　　：　営利種子ストックＮＯ管理          *
*　　モジュール名　　　　：　ストックＮＯ管理Ｆ削除　　　　　　*
*　　作成日／更新日　　　：　2020/07/10                        *
*　　作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*　　処理概要　　　　　　：　条件Ｆより削除基準日を算出し、　　*
*                            基準日以前のレコードを削除する。  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SST0030B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          20/07/10.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  ストックＮＯ管理ファイル  >>---*
     SELECT   STNOKRF   ASSIGN         DA-01-VI-STNOKRL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  STN-F01  STN-F02
                                       STN-F03  STN-F04
                                       STN-F05  STN-F06
                                       STN-F07  STN-F08
                        STATUS         STN-ST.
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
*---<<  ストックＮＯ管理ファイル  >>---*
 FD  STNOKRF            LABEL RECORD   IS   STANDARD.
     COPY        STNOKRF     OF      XFDLIB
                 JOINING     STN     PREFIX.
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
 01  WK-DATE-A          PIC  9(03)     VALUE  ZERO.
 01  WK-DATE-B          PIC  9(02)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  STN-ST             PIC  X(02).
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
 01  WK-ZAI-12-R        PIC S9(02)     VALUE  ZERO.
 01  WK-ZAI-12-Z        PIC  9(02)     VALUE  ZERO.
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
 STN-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      STNOKRF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SST0030B STNOKRF    ERROR " STN-ST " "
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
     DISPLAY  "### SST0030B HJYOKEN    ERROR " JYO-ST " "
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
     OPEN     I-O       STNOKRF.
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
     DISPLAY "WK-ZAI-1(1) = "  WK-ZAI-1       UPON CONS.
*----<< 削除月数取得 >>-*
     MOVE    83              TO   JYO-F01.
     MOVE    "STNO"          TO   JYO-F02.
     READ    HJYOKEN
             INVALID
             DISPLAY "HJYOKEN DEL INVALID" JYO-F01 " - " JYO-F02
                      UPON CONS
                      STOP RUN
             NOT  INVALID
             MOVE     JYO-F04     TO   WK-SAKUJO
*************削除月数が９９以上の場合、異常終了させる ST
             IF   JYO-F04 >  99
                  DISPLAY NC"＃削除月設定エラー＃" UPON CONS
                  STOP  RUN
             END-IF
*************削除月数が９９以上の場合、異常終了させる ED
     END-READ.
     DISPLAY "WK-SAKUJO   = "  WK-SAKUJO      UPON CONS.
*----<< 基準日付算出 >>-*
*****１．年削除数の算出
     MOVE     99                  TO   WK-ZAI-2.
     DIVIDE   WK-SAKUJO    BY   12   GIVING    WK-DATE-A
                                     REMAINDER WK-DATE-B.
     IF  WK-DATE-A  >  0
         COMPUTE  WK-ZAI-11 = WK-ZAI-11  -  WK-DATE-A
     END-IF.
     DISPLAY "WK-ZAI-1(2) = "  WK-ZAI-1       UPON CONS.
****２．月削除数の算出
     COMPUTE  WK-ZAI-12-R  =  WK-ZAI-12  -  WK-DATE-B.
     DISPLAY "WK-ZAI-12-R = "  WK-ZAI-12-R    UPON CONS.
     IF  WK-ZAI-12-R  <=  ZERO
         ADD    -1      TO        WK-ZAI-11
         MOVE   12      TO        WK-ZAI-12
         MOVE   WK-ZAI-12-R   TO  WK-ZAI-12-Z
         COMPUTE  WK-ZAI-12 = WK-ZAI-12 - WK-ZAI-12-Z
     ELSE
         MOVE   WK-ZAI-12-R   TO  WK-ZAI-12-Z
         MOVE   WK-ZAI-12-Z   TO  WK-ZAI-12
     END-IF.
*----<< 削除基準日出力 >>-*
     DISPLAY "DELETE DATE = "  WK-ZAIKO-DATE  UPON CONS.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*出荷情報データ初期読込み
     PERFORM  STNOKRF-READ-SEC.
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
     DELETE   STNOKRF.
     ADD      1          TO       DEL-CNT.
*    出荷情報読込み
     PERFORM  STNOKRF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    STNOKRF   HJYOKEN.
*
     DISPLAY  "## READ   CNT => " IN-CNT    UPON CONS.
     DISPLAY  "## DELETE CNT => " DEL-CNT   UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SST0030B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　出荷情報データ読込み　　　　　　　　　　　　　　　　　　　*
****************************************************************
 STNOKRF-READ-SEC       SECTION.
*    売上伝票ファイル参照
     READ     STNOKRF   AT  END
              MOVE  "END"    TO   END-FLG
              GO             TO   STNOKRF-READ-EXIT
     END-READ.
*    読込みカウント
     ADD      1              TO   IN-CNT.
*    読込み件数表示
     IF       IN-CNT(4:3)  =  "000"  OR  "500"
            DISPLAY "READ-CNT = " IN-CNT " : " DEL-CNT UPON CONS
     END-IF.
*    日付チェック
     IF       STN-F01  >  WK-ZAIKO-DATE
              MOVE  "END"    TO   END-FLG
              GO             TO   STNOKRF-READ-EXIT
     END-IF.
*
 STNOKRF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
