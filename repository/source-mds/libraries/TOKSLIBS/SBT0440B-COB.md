# SBT0440B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBT0440B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹システム　　　　　　　　　*
*    業務名　　　　　　　：　ＬＩＮＫＳ連携　　　　　　　　　　*
*    モジュール名　　　　：　出荷梱包データチェック処理　　　　*
*    作成日　　　　　　　：　2014/08/14                        *
*    作成者　　　　　　　：　NAV                               *
*    処理概要　　　　　　：　出荷梱包チェックワークを順読みし  *
*                            出荷梱包累積Ｆを索引し、Ｋａ側　　*
*                            出荷梱包データのチェックを行なう。*
*　　更新日　　　　　　　：　2014/09/08                        *
*    修正概要　　　　　　：　最終納品先ＣＤを　　　　　　　　　*
*      　　　　　　　　　　　出荷梱包累積ファイルよりセット　　*
*      　　　　　　　　　　　するよう変更。　　　　　　　　　　*
*      　　　　　　　　　　　これにより累積ファイルのキー　　　*
*      　　　　　　　　　　　より最終納品先ＣＤを除外。　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBT0440B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/10/05.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*出荷梱包チェックワーク
     SELECT   SYKCKL1   ASSIGN    TO        DA-01-VI-SYKCKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SYK-F011  SYK-F012
                                            SYK-F013  SYK-F014
                        FILE  STATUS   IS   SYK-ST.
*出荷梱包累積ファイル（Ｌ４）
     SELECT   CNZSYRL4  ASSIGN    TO        DA-01-VI-CNZSYRL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SYR-F92   SYR-F013
                                            SYR-F302
                                            SYR-F346  SYR-F402
                        FILE  STATUS   IS   SYR-ST.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    出荷梱包チェックワーク
******************************************************************
 FD  SYKCKL1
                        LABEL RECORD   IS   STANDARD.
     COPY     SYKCKF     OF        XFDLIB
              JOINING   SYK  AS   PREFIX.
*
******************************************************************
*    出荷梱包累積ファイル
******************************************************************
 FD  CNZSYRL4
                        LABEL RECORD   IS   STANDARD.
     COPY     CNZSYRL4   OF        XFDLIB
              JOINING   SYR  AS   PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  RW-CNT                  PIC  9(08)     VALUE  ZERO.
 01  CNZSYRL4-INV-FLG        PIC  X(03)     VALUE  SPACE.
*プログラムＳＴＡＴＵＳ.
 01  WK-ST.
     03  SYK-ST        PIC  X(02).
     03  SYR-ST        PIC  X(02).
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC   9(06)  VALUE  ZERO.
     03  SYS-DATEW           PIC   9(08)  VALUE  ZERO.
     03  SYS-DATE-R          REDEFINES SYS-DATEW.
         05  SYS-YY          PIC   9(04).
         05  SYS-MM          PIC   9(02).
         05  SYS-DD          PIC   9(02).
***** システム時刻ワーク
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HHMMSS     PIC  9(06).
     03  SYS-MS         PIC  9(02).
*
 01  WK-SYK-F0224       PIC  X(07).
 01  FILLER             REDEFINES      WK-SYK-F0224.
     03  WK-SYK-F0224-1 PIC  9(07).
 01  WK-SYK-F0224-2     PIC  9(06)V9(01).
 01  WK-SYK-F0224-3     PIC  9(06)V9(01).
*
***  セクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)     VALUE " *** ".
     03  S-NAME             PIC  X(30).
*メッセージ出力
 01  FILE-ERR.
     03  SYK-ERR           PIC  N(20)  VALUE
         NC"出荷梱包チェックワークエラー".
     03  SYR-ERR           PIC  N(20)  VALUE
         NC"出荷梱包累積ファイルエラー".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBT0440B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBT0440B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " INPUT  = ".
         05  IN-CNT         PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " OUTPUT = ".
         05  OUT-CNT        PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION.
 DECLARATIVES.
 SYK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SYKCKL1.
     DISPLAY     SYK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SYK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SYR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE CNZSYRL4.
     DISPLAY     SYR-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SYR-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
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
              UNTIL     END-FLG    =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN     I-O       SYKCKL1.
     OPEN     INPUT     CNZSYRL4.
     DISPLAY  MSG-START UPON CONS.
*システム時刻取得
     ACCEPT   SYS-TIME  FROM      TIME.
*システム日付取得
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
*
     MOVE     SPACE     TO        END-FLG.
     MOVE     ZERO      TO        RD-CNT    RW-CNT.
*出荷梱包チェックワーク
     PERFORM  SYKCKL1-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*出荷梱包累積ファイル検索
     MOVE     SPACE               TO   SYR-REC.
     INITIALIZE                        SYR-REC.
*オ／手区分
     MOVE     "0"                 TO   SYR-F92.
*取引先ＣＤ
     MOVE     SYK-F013            TO   SYR-F013.
*****MOVE     921084              TO   SYR-F013.
*最終納品先ＣＤ
*--- MOVE     SYK-F0230           TO   SYR-F308.
*最終納品先納品日
     MOVE     SYK-F0205           TO   SYR-F346.
*取引番号
     MOVE     SYK-F0202           TO   SYR-F302.
*取引明細番号
     MOVE     SYK-F0203           TO   SYR-F402.
*****DISPLAY "SYR-F92   = "  SYR-F92    UPON CONS.
*    DISPLAY "SYR-F013  = "  SYR-F013   UPON CONS.
*    DISPLAY "SYR-F308  = "  SYR-F308   UPON CONS.
*    DISPLAY "SYR-F346  = "  SYR-F346   UPON CONS.
*    DISPLAY "SYR-F302  = "  SYR-F302   UPON CONS.
*****DISPLAY "SYR-F402  = "  SYR-F402   UPON CONS.
     PERFORM CNZSYRL4-READ-SEC.
*エラー区分を一度、初期化する
     MOVE     SPACE               TO   SYK-F015.
*出荷梱包累積Ｆに存在しない場合
     IF  CNZSYRL4-INV-FLG  =  "INV"
         MOVE   "1"               TO   SYK-F0151
         GO                       TO   MAIN-010
*2014/09/08↓
     ELSE
         MOVE    SYR-F308         TO   SYK-F0230
         MOVE    SYR-F604         TO   SYK-F0231
*2014/09/08↑
     END-IF.
*出荷指示数とＬＩＮＫＳ出荷確定数が異なる場合
     MOVE SYK-F0226               TO   WK-SYK-F0224.
     COMPUTE WK-SYK-F0224-2 = WK-SYK-F0224-1 / 10.
     IF  WK-SYK-F0224-2  NOT =  SYR-F604
         DISPLAY NC"＃出荷指示数" " = " WK-SYK-F0224-2 UPON CONS
         DISPLAY NC"＃出荷確定数" " = " SYR-F604       UPON CONS
         MOVE   "1"               TO   SYK-F0152
     END-IF.
*発注数＜ＬＩＮＫＳ出荷確定数の場合
     MOVE SYK-F0224               TO   WK-SYK-F0224.
     COMPUTE WK-SYK-F0224-2 = WK-SYK-F0224-1 / 10.
     MOVE SYK-F0226               TO   WK-SYK-F0224.
     COMPUTE WK-SYK-F0224-3 = WK-SYK-F0224-1 / 10.
     IF  WK-SYK-F0224-2  <      WK-SYK-F0224-3
         DISPLAY NC"＃発注数　　" " = " WK-SYK-F0224-2 UPON CONS
         DISPLAY NC"＃出荷確定数" " = " WK-SYK-F0224-3 UPON CONS
         MOVE   "1"               TO   SYK-F0153
     END-IF.
*発注数＞ＬＩＮＫＳ出荷確定数の場合で欠品情報がない場合
     IF  WK-SYK-F0224-2  >      WK-SYK-F0224-3
     AND SYK-F0301  NOT =  "G"
         DISPLAY NC"＃発注数　　" " = " WK-SYK-F0224-2 UPON CONS
         DISPLAY NC"＃出荷確定数" " = " WK-SYK-F0224-3 UPON CONS
         DISPLAY NC"＃欠品ＲＥＣ" " = " NC"無異常！" UPON CONS
         MOVE   "1"               TO   SYK-F0154
     END-IF.
*
 MAIN-010.
*出荷梱包チェックワーク更新
     REWRITE  SYK-REC.
     ADD         1                TO   RW-CNT.
*
     PERFORM  SYKCKL1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*プログラム終了メッセージ表示
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      RW-CNT    TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*ファイルクローズ
     CLOSE     SYKCKL1   CNZSYRL4.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　出荷梱包チェックワーク読込
****************************************************************
 SYKCKL1-READ-SEC          SECTION.
*
     READ     SYKCKL1
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO SYKCKL1-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT " #"  UPON  CONS
     END-IF.
*
 SYKCKL1-READ-EXIT.
     EXIT.
****************************************************************
*　　出荷梱包累積ファイル読込
****************************************************************
 CNZSYRL4-READ-SEC          SECTION.
*
     READ     CNZSYRL4
              INVALID       MOVE  "INV"   TO  CNZSYRL4-INV-FLG
              NOT  INVALID  MOVE  SPACE   TO  CNZSYRL4-INV-FLG
     END-READ.
*
 CNZSYRL4-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
