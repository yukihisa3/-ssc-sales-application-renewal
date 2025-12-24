# SSI5002V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSI5002V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　トステムビバ　支払明細書データ　　*
*    業務名　　　　　　　：　支払明細書データＣＳＶ            *
*    モジュール名　　　　：　支払明細書データＣＳＶ            *
*    作成日／更新日　　　：　08/04/16                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　支払明細データをＣＳＶファイルに  *
*    　　　　　　　　　　　　出力する。　　　　　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSI5002V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/29.
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
****<<DCMJAPAN物品受領データ >>*********************************
     SELECT   TOSSHI             ASSIGN    TO   TOSSHI
                                 STATUS         TOS-STATUS.
*
*****<<  受領ﾃﾞｰﾀCSV   >>**************************************
     SELECT   TOSCSV             ASSIGN    TO   TOSCSV
                                 STATUS         CSV-STATUS.
*****<< 店舗マスタ >>******************************************
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-STATUS.
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ＤＣＭＪＡＰＡＮ　物品受領書                       *
*--------------------------------------------------------------*
 FD  TOSSHI             BLOCK CONTAINS 1    RECORDS.
 01  TOS-REC.
     03  TOS-F01        PIC       X(01).
     03  TOS-F02        PIC       X(127).
*
*--------------------------------------------------------------*
*    FILE = ＤＣＭＪＡＰＡＮ　物品ＣＳＶデータ                 *
*--------------------------------------------------------------*
 FD  TOSCSV             BLOCK CONTAINS 1    RECORDS.
 01  MEISAI-REC.
     03  FILLER         PIC       X(500).
*
*--------------------------------------------------------------*
*    FILE = ＤＣＭＪＡＰＡＮ　物品ＣＳＶデータ                 *
*--------------------------------------------------------------*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*明細情報
 01  MEI-REC.
     02  MEI-F01
               PIC X(1).
     02  MEI-F02
                  PIC 9(4).
     02  MEI-F03
                  PIC 9(6).
     02  MEI-F04
                  PIC 9(5).
     02  MEI-F05
                  PIC 9(6).
     02  MEI-F06
                  PIC 9(5).
     02  MEI-F07
                  PIC 9(2).
     02  MEI-F08
                  PIC 9(11).
     02  MEI-F09
                  PIC 9(10).
     02  MEI-F10
                  PIC 9(2).
     02  MEI-F11
                  PIC 9(2).
     02  MEI-F12
                  PIC 9(4).
     02  MEI-F13
                 PIC 9(7).
     02  MEI-F14
                  PIC 9(6).
     02  MEI-F15
                  PIC S9(10).
     02  MEI-F16
                  PIC 9(1).
     02  MEI-F17
                  PIC 9(1).
     02  MEI-F18
                  PIC S9(10).
     02  MEI-F19
                  PIC 9(1).
     02  MEI-F20
                  PIC 9(6).
     02  MEI-F21
                  PIC 9(6).
     02  MEI-F22
                  PIC X(22).
*テイル情報
 01  TIL-REC.
     02  TIL-F01
                  PIC X(1).
     02  TIL-F02
                  PIC 9(4).
     02  TIL-F03
                  PIC 9(6).
     02  TIL-F04
                  PIC 9(5).
     02  TIL-F05
                  PIC 9(5).
     02  TIL-F06
                  PIC 9(5).
     02  TIL-F07
                  PIC S9(11).
     02  TIL-F08
                  PIC S9(11).
     02  TIL-F09
                  PIC 9(6).
     02  TIL-F10
                  PIC 9(6).
     02  TIL-F11
                  PIC X(68).
*ＣＳＶ情報
 01  CSV-REC.
     02  CSV-F01
                  PIC 9(2).
     02  CSV-A01
                  PIC X(1).
     02  CSV-F02
                  PIC 9(5).
     02  CSV-A02
                  PIC X(1).
     02  CSV-F031
                  PIC X(1).
     02  CSV-F03
                  PIC N(15).
     02  CSV-F032
                  PIC X(1).
     02  CSV-A03
                  PIC X(1).
     02  CSV-F04
                  PIC X(2).
     02  CSV-A04
                  PIC X(1).
     02  CSV-F051
                  PIC X(1).
     02  CSV-F05
                  PIC N(15).
     02  CSV-F052
                  PIC X(1).
     02  CSV-A05
                  PIC X(1).
     02  CSV-F06
                  PIC 9(6).
     02  CSV-A06
                  PIC X(1).
     02  CSV-F07
                  PIC 9(11).
     02  CSV-A07
                  PIC X(1).
     02  CSV-F08
                  PIC 9(10).
     02  CSV-A08
                  PIC X(1).
     02  CSV-F091
                  PIC X(1).
     02  CSV-F09
                  PIC 9(9).
     02  CSV-A09
                  PIC X(1).
     02  CSV-F101
                  PIC X(1).
     02  CSV-F10
                  PIC 9(9).
     02  CSV-A10
                  PIC X(1).
     02  CSV-F11
                  PIC 9(1).
     02  CSV-A11
                  PIC X(1).
     02  CSV-F12
                  PIC 9(1).
     02  CSV-A12
                  PIC X(1).
     02  CSV-F13
                  PIC 9(1).
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  CHK-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  TOS-STATUS                   PIC       X(02).
 01  CSV-STATUS                   PIC       X(02).
 01  TEN-STATUS                   PIC       X(02).
*
***** ワーク　エリア
 01  WK-AREA.
     03  WK-KUBUN                 PIC   X(01)   VALUE SPACE.
     03  WK-SEQ                   PIC   9(03)   VALUE ZERO.
     03  WK-TENCD                 PIC   9(05)   VALUE ZERO.
     03  WK-SEIKIN                PIC  S9(11)   VALUE ZERO.
     03  WK-SIHKIN                PIC  S9(11)   VALUE ZERO.
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
*タイトルエリア
 01  WK-TAITL.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"ＳＥＱ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"店ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"店名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"伝種".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"伝種名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"納品日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"伝票番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"請求金額".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"支払金額".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(09)  VALUE  NC"アンマッチ計上金額".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"計上".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"マッチ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"ダブリ".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
*テイルエリア
 01  WK-TAIL1.
     03  FILLER        PIC X(06)  VALUE  ",,,,,,".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"＜合計＞".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  TAIL11        PIC X(01)  VALUE  SPACE.
     03  TAIL01        PIC 9(11).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  TAIL22        PIC X(01)  VALUE  SPACE.
     03  TAIL02        PIC 9(11).
*
*合計エリア
 01  WK-TAIL-GK.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"＜総合計＞".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"伝票枚数".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  TAILGK01      PIC 9(05).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"請求合計".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  TAILGK02X     PIC X(01)  VALUE  SPACE.
     03  TAILGK02      PIC 9(09).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"支払合計".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  TAILGK03X     PIC X(01)  VALUE  SPACE.
     03  TAILGK03      PIC 9(09).
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSI5002V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSI5002V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSI5002V".
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
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TOSSHI.
     MOVE     "TOSSHI  "          TO        ERR-FL-ID.
     MOVE     TOS-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TOSCSV.
     MOVE     "TOSCSV"          TO        ERR-FL-ID.
     MOVE     CSV-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE HTENMS.
     MOVE     "HTENMS"          TO        ERR-FL-ID.
     MOVE     TEN-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSI5002V-START         SECTION.
*
     MOVE   "SSI5002V-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSI5002V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     TOSSHI.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    TOSCSV.
     DISPLAY  MSG-START UPON CONS.
*
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
     PERFORM  TOSSHI-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     ELSE
              MOVE   TOS-F01       TO        WK-KUBUN
              MOVE   SPACE         TO        MEISAI-REC
              MOVE   WK-TAITL      TO        MEISAI-REC
              WRITE  MEISAI-REC
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ホーマック物品受領書データ読込み
****************************************************************
 TOSSHI-RD-SEC             SECTION.
*
     MOVE    "TOSSHI-RD-SEC"    TO   S-NAME.
*
     READ     TOSSHI
          AT END
              MOVE     "END"      TO        END-FLG
          NOT  AT  END
              MOVE      TOS-REC   TO        MEI-REC
              ADD       1         TO        READ-CNT
     END-READ.
*
 TOSSHI-RD-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     IF       WK-KUBUN  NOT =  TOS-F01
              MOVE     TOS-F01    TO   WK-KUBUN
              IF       TOS-F01 = "B"
                       MOVE     MEI-F06    TO   WK-TENCD
              END-IF
     END-IF.
*
     IF       WK-KUBUN  =  "B"
*    店ＣＤブレイク
              IF       WK-TENCD   NOT =   MEI-F06
                       MOVE     MEI-F06     TO   WK-TENCD
                       PERFORM    MEI-GOKEI-SEC
              END-IF
*    明細データ転送処理
              PERFORM  MEI-TENSO-SEC
     END-IF.
*
     IF       WK-KUBUN  =  "T"
              MOVE      TOS-REC   TO        TIL-REC
     END-IF.
*    次レコード読込み
     PERFORM  TOSSHI-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*             明細データ転送                2.0                *
****************************************************************
 MEI-TENSO-SEC          SECTION.
*
     MOVE    "MEI-TENSO-SEC"           TO   S-NAME.
*初期化
     MOVE     SPACE               TO   MEISAI-REC.
*    INITIALIZE                        MEISAI-REC.
*項目転送
     MOVE     ","                 TO   CSV-A01 CSV-A02 CSV-A03.
     MOVE     ","                 TO   CSV-A04 CSV-A05 CSV-A06.
     MOVE     ","                 TO   CSV-A07 CSV-A08 CSV-A09.
     MOVE     ","                 TO   CSV-A10 CSV-A11 CSV-A12.
*
     MOVE    X"28"                TO   CSV-F031 CSV-F051.
     MOVE    X"29"                TO   CSV-F032 CSV-F052.
*
     ADD      1         TO        WK-SEQ.
     MOVE     WK-SEQ              TO   CSV-F01.
*****DISPLAY "MEI-F06 = " MEI-F06 ":MEI-F03 = " MEI-F03 UPON CONS.
     MOVE     MEI-F06             TO   CSV-F02  TEN-F011.
     MOVE     MEI-F03             TO   TEN-F52.
     READ     HTENMS    INVALID
                  MOVE      ALL NC"＊"     TO   CSV-F03
              NOT INVALID
                  MOVE      TEN-F03        TO   CSV-F03
     END-READ.
     MOVE     MEI-F07             TO   CSV-F04.
     EVALUATE  MEI-F07
             WHEN  "21"
                   MOVE  NC"仕入"                TO   CSV-F05
             WHEN  "22"
                   MOVE  NC"返品"                TO   CSV-F05
             WHEN  "23"
                   MOVE  NC"値引"                TO   CSV-F05
             WHEN  "31"
                   MOVE  NC"仕入取消"            TO   CSV-F05
             WHEN  "32"
                   MOVE  NC"返品取消"            TO   CSV-F05
             WHEN  "33"
                   MOVE  NC"値引取消"            TO   CSV-F05
             WHEN  "71"
                   MOVE  NC"支払配信費用"        TO   CSV-F05
             WHEN  "72"
                   MOVE  NC"発注伝票代"         TO   CSV-F05
             WHEN  "73"
                   MOVE  NC"月間管理費"         TO   CSV-F05
             WHEN  "74"
                   MOVE  NC"新店歩引き"         TO   CSV-F05
             WHEN  "75"
                   MOVE  NC"仕入割戻"           TO   CSV-F05
             WHEN  "76"
                   MOVE  NC"受取配送料"         TO   CSV-F05
             WHEN  "77"
                   MOVE  NC"オンライン費用"     TO   CSV-F05
             WHEN  "78"
                   MOVE  NC"売掛相殺"           TO   CSV-F05
             WHEN  "79"
                   MOVE  NC"その他相殺"         TO   CSV-F05
             WHEN  "89"
                   MOVE  NC"前月支払予定額（税込）" TO  CSV-F05
             WHEN  "91"
                  MOVE  NC"当月支払額（税込）"  TO   CSV-F05
             WHEN  "92"
                  MOVE  NC"相殺済金額合計（税込）" TO   CSV-F05
             WHEN  "93"
                  MOVE  NC"仕入金額（税抜）"    TO   CSV-F05
             WHEN  "94"
                  MOVE  NC"差異金額（税抜）"    TO   CSV-F05
             WHEN  "95"
                  MOVE  NC"相殺予定金額（税込）" TO   CSV-F05
             WHEN  "96"
                  MOVE  NC"仕入金額消費税"      TO   CSV-F05
             WHEN  "97"
                  MOVE  NC"当月保留（税込）"    TO   CSV-F05
             WHEN  "99"
                  MOVE  NC"支払予定額（税込）"  TO   CSV-F05
     END-EVALUATE.
     MOVE     MEI-F05             TO   CSV-F06.
     MOVE     MEI-F08             TO   CSV-F07.
     MOVE     MEI-F09             TO   CSV-F08.
     ADD      MEI-F09             TO   WK-SEIKIN.
     MOVE     MEI-F15             TO   CSV-F09.
     ADD      MEI-F15             TO   WK-SIHKIN.
     IF       MEI-F15 < ZERO
              MOVE  "-"           TO   CSV-F091
     ELSE
              MOVE  "0"           TO   CSV-F091
     END-IF.
     MOVE     MEI-F18             TO   CSV-F10.
     IF       MEI-F18 < ZERO
              MOVE  "-"           TO   CSV-F101
     ELSE
              MOVE  "0"           TO   CSV-F101
     END-IF.
     MOVE     MEI-F16             TO   CSV-F11.
     MOVE     MEI-F17             TO   CSV-F12.
     MOVE     MEI-F19             TO   CSV-F13.
     MOVE     CSV-REC             TO   MEISAI-REC
     WRITE    MEISAI-REC.
     ADD      1                   TO   OUTPUT-CNT.
 MEI-TENSO-EXIT.
     EXIT.
****************************************************************
*             合計データ転送                2.0                *
****************************************************************
 TIL-TENSO-SEC          SECTION.
*
     MOVE    "TIL-TENSO-SEC"           TO   S-NAME.
*初期化
     MOVE     SPACE               TO   MEISAI-REC.
*伝票枚数
     MOVE     TIL-F06             TO   TAILGK01.
*請求合計
     MOVE     TIL-F07             TO   TAILGK02.
     IF       TIL-F07 < ZERO
              MOVE  "-"           TO   TAILGK02X
     ELSE
              MOVE  "0"           TO   TAILGK02X
     END-IF.
*支払合計
     MOVE     TIL-F08             TO   TAILGK03.
     IF       TIL-F08 < ZERO
              MOVE  "-"           TO   TAILGK03X
     ELSE
              MOVE  "0"           TO   TAILGK03X
     END-IF.
*
     MOVE     WK-TAIL-GK  TO   MEISAI-REC.
     WRITE    MEISAI-REC.
*
 TIL-TENSO-EXIT.
     EXIT.
****************************************************************
*             明細データ転送                2.0                *
****************************************************************
 MEI-GOKEI-SEC          SECTION.

     MOVE    "MEI-SOKEI-SEC"           TO   S-NAME.

     MOVE     SPACE       TO   MEISAI-REC.
*    INITIALIZE                MEISAI-REC.
*請求金額合計出力
     MOVE     WK-SEIKIN   TO   TAIL01.
     IF       WK-SEIKIN  <  ZERO
              MOVE   "-"  TO   TAIL11
     ELSE
              MOVE   "0"  TO   TAIL11
     END-IF.
*
     MOVE     WK-TAIL1    TO   MEISAI-REC.
*****WRITE    MEISAI-REC.
*支払金額合計出力
     MOVE     WK-SIHKIN   TO   TAIL02.
     IF       WK-SIHKIN  <  ZERO
              MOVE   "-"  TO   TAIL22
     ELSE
              MOVE   "0"  TO   TAIL22
     END-IF.
*
     MOVE     WK-TAIL1    TO   MEISAI-REC.
     WRITE    MEISAI-REC.
     MOVE     ZERO        TO   WK-SEIKIN.
     MOVE     ZERO        TO   WK-SIHKIN.
*
*MEI-GOKEI-EXIT.
*    EXIT.
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     PERFORM  MEI-GOKEI-SEC.
     PERFORM  TIL-TENSO-SEC.
*
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
*
     CLOSE    TOSSHI   TOSCSV.
*
 END-EXIT.
     EXIT.
********************<<  PUROGRAM  END  >>*************************

```
