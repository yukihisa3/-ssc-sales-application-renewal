****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩシステム　　　　　　*
*    業務名　　　　　　　：　出荷実績抽出　　　　              *
*    モジュール名　　　　：　出荷実績抽出　　　　　　          *
*    作成日／更新日　　　：　12/03/30                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　ナフコ基本情報Ｆより出荷実績を抽出*
*    　　　　　　　　　　　　する。（指定範囲データ）　　　　  *
*            更新日　　　：　12/05/30                          *
*                        ：　INPUTを売上累積ファイルに変更
*                        ：　OUTPUTにサカタ伝票区分を追加
*            更新日　　　：　16/10/05                          *
*                        ：　行_＝８０が抽出しない
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY3710B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/03/30.
*
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
*
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<売上累積ファイル >>********************************
     SELECT   SHTDENLK  ASSIGN    TO   DA-01-VI-SHTDENLK
                       ORGANIZATION   INDEXED
                       ACCESS    MODE SEQUENTIAL
                       RECORD    KEY  SHT-F01 SHT-F112 SHT-F07
                                      SHT-F02 SHT-F04  SHT-F051
                                      SHT-F03
                       STATUS         SHT-STATUS.
****<<ナフコ出荷実績Ｆ >>********************************
     SELECT   NFJISKF  ASSIGN    TO   DA-01-VI-NFJISKL1
                       ORGANIZATION   INDEXED
                       ACCESS    MODE RANDOM
                       RECORD    KEY  NSK-F01  NSK-F04  NSK-F05
                                      NSK-F06
                       STATUS         NSK-STATUS.
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = 売上累積ファイル                                   *
*--------------------------------------------------------------*
 FD  SHTDENLK            LABEL RECORD   IS   STANDARD.
     COPY     SHTDENLK   OF        XFDLIB
              JOINING   SHT       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ出荷実績Ｆ                                   *
*--------------------------------------------------------------*
 FD  NFJISKF            LABEL RECORD   IS   STANDARD.
     COPY     NFJISKF   OF        XFDLIB
              JOINING   NSK       PREFIX.
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  SHT-STATUS                   PIC       X(02).
 01  NSK-STATUS                   PIC       X(02).
*
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC       9(06)  VALUE  ZERO.
     03  SYS-DATEW                PIC       9(08)  VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES SYS-DATEW.
         05  SYS-YY               PIC       9(04).
         05  SYS-MM               PIC       9(02).
         05  SYS-DD               PIC       9(02).
*
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
 01  WK-SHT-F02                   PIC       9(07)  VALUE  ZERO.
 01  NFJISKF-INV-FLG              PIC       X(03)  VALUE  SPACE.
***** 店舗テーブル
 01  BRK-KEY.
     03  BRK-F06                  PIC       9(05).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3710B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3710B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY3710B".
         05  FILLER               PIC       X(10)  VALUE
                       " ABEND ###".
*
     03  MSG-ABEND2.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-FL-ID            PIC       X(08).
         05  FILLER               PIC       X(04)  VALUE
                       " ST-".
         05  ERR-STCD             PIC       X(02).
         05  FILLER               PIC       X(04)  VALUE
                       " ###".
*
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
         05  FILLER         PIC   X(09)  VALUE " OUTPG= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
 LINKAGE                SECTION.
 01  PARA-ST-DATE           PIC   9(08).
 01  PARA-ED-DATE           PIC   9(08).
****************************************************************
 PROCEDURE              DIVISION   USING  PARA-ST-DATE
                                          PARA-ED-DATE.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE SHTDENLK.
     MOVE     "SHTDENLK"          TO        ERR-FL-ID.
     MOVE     SHT-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO        PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFJISKF.
     MOVE     "NFJISKL1"          TO        ERR-FL-ID.
     MOVE     NSK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO        PROGRAM-STATUS.
     DISPLAY "SHT-F09 = " SHT-F09 UPON CONS.
     DISPLAY "SHT-F06 = " SHT-F06 UPON CONS.
     DISPLAY "SHT-F07 = " SHT-F07 UPON CONS.
     DISPLAY "SHT-F08 = " SHT-F08 UPON CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY3710B-START         SECTION.
*
     MOVE   "SSY3710B-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSY3710B-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SHTDENLK.
     OPEN     I-O       NFJISKF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     SPACE              TO        SHT-REC.
     INITIALIZE                            SHT-REC.
     MOVE     137607             TO        SHT-F01.
     MOVE     PARA-ST-DATE       TO        SHT-F112.
     START  SHTDENLK KEY IS >= SHT-F01 SHT-F112 SHT-F07
                               SHT-F02 SHT-F04  SHT-F051 SHT-F03
            INVALID
            DISPLAY NC"＃対象データなし＃" UPON CONS
            MOVE  "END"          TO        END-FLG
            GO                   TO        INIT-EXIT
     END-START.
*
     PERFORM  SHTDENLK-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
              GO   TO    INIT-EXIT.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    売上伝票ファイル読込　　　　　　
****************************************************************
 SHTDENLK-RD-SEC             SECTION.
*
     MOVE    "SHTDENLK-RD-SEC"    TO   S-NAME.
*
     READ     SHTDENLK
          NEXT  AT END
              MOVE     "END"      TO        END-FLG
              GO                  TO        SHTDENLK-RD-EXIT
          NOT  AT  END
              ADD       1         TO        READ-CNT
     END-READ.
*
     IF   READ-CNT(5:3)  =  "000" OR "500"
          DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*##2016/10/05 NAV ST
*行番号＝８０の場合は対象外とする
     IF   SHT-F03  =  80
          GO                  TO   SHTDENLK-RD-SEC
     END-IF.
*##2016/10/05 NAV ED
*
     IF   ( SHT-F01 NOT = 137607 ) OR ( SHT-F112 >  PARA-ED-DATE )
          MOVE     "END"      TO        END-FLG
     END-IF.
*
 SHTDENLK-RD-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     MOVE     SHT-F112            TO   NSK-F01.
     MOVE     SHT-F07             TO   NSK-F04.
     MOVE     SHT-F02             TO   WK-SHT-F02.
     MOVE     WK-SHT-F02          TO   NSK-F05.
     MOVE     SHT-F03             TO   NSK-F06.
*
     PERFORM  NFJISKF-READ-SEC.
     IF   NFJISKF-INV-FLG = SPACE
          GO                      TO   MAIN-010
     END-IF
*初期化
     MOVE     SPACE               TO   NSK-REC.
     INITIALIZE                        NSK-REC.
*項目転送
* 納品日
     MOVE     SHT-F112            TO   NSK-F01.
* 出荷日
     MOVE     SHT-F113            TO   NSK-F02.
* 入荷予定日
     MOVE     ZERO                TO   NSK-F03.
* 店舗ＣＤ
     MOVE     SHT-F07             TO   NSK-F04.
* 伝票番号
     MOVE     SHT-F02             TO   WK-SHT-F02.
     MOVE     WK-SHT-F02          TO   NSK-F05.
* 行番号
     MOVE     SHT-F03             TO   NSK-F06.
* 出荷倉庫
     MOVE     SHT-F08             TO   NSK-F07.
* 相手商品ＣＤ
     MOVE     SHT-F25             TO   NSK-F08.
* サカタ商品ＣＤ
     MOVE     SHT-F1411           TO   NSK-F09.
* サカタ品単ＣＤ
     MOVE     SHT-F1412           TO   NSK-F10.
* 発注数
     MOVE     SHT-F50             TO   NSK-F11.
* 出荷数
     MOVE     SHT-F15             TO   NSK-F12.
* 原価単価
     MOVE     SHT-F172            TO   NSK-F13.
* 売価単価　
     MOVE     SHT-F173            TO   NSK-F14.
* サカタ伝票区分
     MOVE     SHT-F051            TO   NSK-F15.
*ナフコ出荷実績出力
     WRITE    NSK-REC.
     ADD      1                   TO   OUTPUT-CNT.
 MAIN-010.
*    次レコード読込み
     PERFORM  SHTDENLK-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
***************************************************************
*             ナフコ出荷実績Ｆ重複チェック  3.0               *
***************************************************************
 NFJISKF-READ-SEC       SECTION.
*
     READ  NFJISKF
           INVALID      MOVE  "INV"  TO  NFJISKF-INV-FLG
           NOT  INVALID MOVE  SPACE  TO  NFJISKF-INV-FLG
     END-READ.
*
 NFJISKF-READ-EXIT.
     EXIT.
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
*
     CLOSE    SHTDENLK   NFJISKF.
*
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.
