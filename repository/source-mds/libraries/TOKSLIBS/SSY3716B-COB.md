# SSY3716B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3716B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩシステム　　　　　　*
*    業務名　　　　　　　：　受領実績抽出　　　　              *
*    モジュール名　　　　：　受領実績抽出　　　　　　          *
*    作成日／更新日　　　：　12/03/30                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　基本情報ファイルより出荷実績を抽出*
*    　　　　　　　　　　　　する。（指定範囲データ）　　　　  *
*            更新日　　　：　12/05/30                          *
*                  　　　：　OUTPUTに項目追加(元伝票番号)
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY3716B.
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
****<<ナフコ受領情報Ｆ >>********************************
     SELECT   NFJYURF  ASSIGN    TO   DA-01-VI-NFJYURL6
                       ORGANIZATION   INDEXED
                       ACCESS    MODE SEQUENTIAL
                       RECORD    KEY  NFJ-F02  NFJ-F05  NFJ-F07
                       STATUS         NFJ-STATUS.
****<<ナフコ受領実績Ｆ >>********************************
     SELECT   NFJUJKF  ASSIGN    TO   DA-01-VI-NFJUJKL1
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
*    FILE = ナフコ受領情報Ｆ                                   *
*--------------------------------------------------------------*
 FD  NFJYURF            LABEL RECORD   IS   STANDARD.
     COPY     NFJYURF   OF        XFDLIB
              JOINING   NFJ       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ受領実績Ｆ                                   *
*--------------------------------------------------------------*
 FD  NFJUJKF            LABEL RECORD   IS   STANDARD.
     COPY     NFJUJKF   OF        XFDLIB
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
 01  NFJ-STATUS                   PIC       X(02).
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
 01  WK-NFJ-F07                   PIC       9(07)  VALUE  ZERO.
 01  NFJUJKF-INV-FLG              PIC       X(03)  VALUE  SPACE.
***** 店舗テーブル
 01  BRK-KEY.
     03  BRK-F06                  PIC       9(05).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3716B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3716B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY3716B".
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
                        PROCEDURE NFJYURF.
     MOVE     "NFJYURLF"          TO        ERR-FL-ID.
     MOVE     NFJ-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO        PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFJUJKF.
     MOVE     "NFJUJKL1"          TO        ERR-FL-ID.
     MOVE     NSK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO        PROGRAM-STATUS.
     DISPLAY "NFJ-F09 = " NFJ-F09 UPON CONS.
     DISPLAY "NFJ-F06 = " NFJ-F06 UPON CONS.
     DISPLAY "NFJ-F07 = " NFJ-F07 UPON CONS.
     DISPLAY "NFJ-F08 = " NFJ-F08 UPON CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY3716B-START         SECTION.
*
     MOVE   "SSY3716B-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSY3716B-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     NFJYURF.
     OPEN     I-O       NFJUJKF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     SPACE              TO        NFJ-REC.
     INITIALIZE                            NFJ-REC.
     MOVE     PARA-ST-DATE       TO        NFJ-F02.
     MOVE     ZERO               TO        NFJ-F05.
     MOVE     ZERO               TO        NFJ-F07.
     START  NFJYURF KEY IS >= NFJ-F02  NFJ-F05 NFJ-F07
            INVALID
            DISPLAY NC"＃対象データなし＃" UPON CONS
            MOVE  "END"          TO        END-FLG
            GO                   TO        INIT-EXIT
     END-START.
*
     PERFORM  NFJYURF-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
              GO   TO    INIT-EXIT.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ出荷指示Ｆ読込　　　　　　
****************************************************************
 NFJYURF-RD-SEC             SECTION.
*
     MOVE    "NFJYURF-RD-SEC"    TO   S-NAME.
*
     READ     NFJYURF
          NEXT  AT END
              MOVE     "END"      TO        END-FLG
              GO                  TO        NFJYURF-RD-EXIT
          NOT  AT  END
              ADD       1         TO        READ-CNT
     END-READ.
*
     IF   READ-CNT(5:3)  =  "000" OR "500"
          DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*
     IF   NFJ-F02  >  PARA-ED-DATE
          MOVE     "END"      TO        END-FLG
     END-IF.
*
 NFJYURF-RD-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     MOVE     NFJ-F02             TO   NSK-F01.
     MOVE     NFJ-F05             TO   NSK-F04.
     MOVE     NFJ-F07             TO   NSK-F05.
     MOVE     NFJ-F08             TO   NSK-F06.
*
     PERFORM  NFJUJKF-READ-SEC.
     IF   NFJUJKF-INV-FLG = SPACE
          GO                      TO   MAIN-010
     END-IF
*初期化
     MOVE     SPACE               TO   NSK-REC.
     INITIALIZE                        NSK-REC.
*項目転送
* 納品日
     MOVE     NFJ-F02             TO   NSK-F01.
* 出荷日
     MOVE     ZERO                TO   NSK-F02.
* 入荷予定日
     MOVE     ZERO                TO   NSK-F03.
* 店舗ＣＤ
     MOVE     NFJ-F05             TO   NSK-F04.
* 伝票番号
     MOVE     NFJ-F07             TO   NSK-F05.
* 行番号
     MOVE     NFJ-F08             TO   NSK-F06.
* 出荷倉庫
     MOVE     SPACE               TO   NSK-F07.
* 相手商品ＣＤ
     MOVE     NFJ-F10             TO   NSK-F08.
* サカタ商品ＣＤ
     MOVE     SPACE               TO   NSK-F09.
* サカタ品単ＣＤ
     MOVE     SPACE               TO   NSK-F10.
* 発注数
     MOVE     ZERO                TO   NSK-F11.
* 出荷数
     MOVE     NFJ-F12             TO   NSK-F12.
* 原価単価
     MOVE     NFJ-F13             TO   NSK-F13.
* 売価単価　
     MOVE     NFJ-F14             TO   NSK-F14.
* 伝票区分　
     MOVE     NFJ-F03             TO   NSK-F15.
* 赤黒区分　
     MOVE     NFJ-F04             TO   NSK-F16.
* 受領区分
     MOVE     NFJ-F15             TO   NSK-F17.
* 理由区分　
     MOVE     NFJ-F16             TO   NSK-F18.
* 理由ＣＤ
     MOVE     NFJ-F17             TO   NSK-F19.
* 理由名　
     MOVE     NFJ-F18             TO   NSK-F20.
* 連絡事項　
     MOVE     NFJ-F19             TO   NSK-F21.
*↓20120530
* 元伝票番号
     MOVE     NFJ-F09             TO   NSK-F22.
*↑20120530
*ナフコ受領実績出力
     WRITE    NSK-REC.
     ADD      1                   TO   OUTPUT-CNT.
 MAIN-010.
*    次レコード読込み
     PERFORM  NFJYURF-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
***************************************************************
*             ナフコ出荷実績Ｆ重複チェック  3.0               *
***************************************************************
 NFJUJKF-READ-SEC       SECTION.
*
     READ  NFJUJKF
           INVALID      MOVE  "INV"  TO  NFJUJKF-INV-FLG
           NOT  INVALID MOVE  SPACE  TO  NFJUJKF-INV-FLG
     END-READ.
*
 NFJUJKF-READ-EXIT.
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
     CLOSE    NFJYURF   NFJUJKF.
*
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.

```
