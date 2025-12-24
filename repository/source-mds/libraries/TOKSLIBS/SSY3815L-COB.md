# SSY3815L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3815L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援                    *
*    モジュール名　　　　：　オンライン作場振分情報リスト      *
*    　　　　　　　　　　：　（作表）　　　　　　　　　　      *
*    作成日／作成者　　　：　2015/04/28 INOUE                  *
*    処理概要　　　　　　：　該当バッチ_のデータについて、    *
*                            振分情報リストを発行する。        *
*    更新日／更新者　　　：　2016/08/23 INOUE                  *
*    処理概要　　　　　　：　本発ＥＤＩ対応                    *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY3815L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/04/28.
*
*
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
*
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS        CONS
                        YA        IS        YA
                        YB-21     IS        YB-21.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*****<<振分リスト出力ワーク >>**********************************
     SELECT   HURXXXW   ASSIGN    TO        DA-01-S-HURXXXW
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HUR-STATUS.
*
*****<<作場マスタ >>********************************************
     SELECT   SAKUBAL1  ASSIGN    TO        DA-01-VI-SAKUBAL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SAK-F01
                        FILE      STATUS    IS   SAK-STATUS.
*****<<ナフコ商品マスタ>>**************************************
     SELECT   NFSHOMS   ASSIGN    TO        DA-01-VI-NFSHOMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   NFM-F01
                        FILE  STATUS        IS   NFM-STATUS.
*****<<商品変換テーブル>>*************************************
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TBL-F01
                                                 TBL-F02
                        FILE  STATUS        IS   TBL-STATUS.
*
*↓2016/08/23
*****<<ナフコ商品分類パターンマスタ>>***************************
     SELECT   NFSHBPL1  ASSIGN    TO        DA-01-VI-NFSHBPL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F03
                        FILE  STATUS        IS   SHO-STATUS.
*↑2016/08/23
*
*****<<  プリント　Ｆ   >>**************************************
     SELECT   PRINTF    ASSIGN    TO        LP-04-PRTF.
*                                                                *
*↓TEST
*****<<内部テーブルワーク >>**********************************
*    SELECT   WKTENPO   ASSIGN    TO        DA-01-S-WKTENPO
*                       ACCESS    MODE      IS   SEQUENTIAL
*                       FILE      STATUS    IS   TEN-STATUS.
*↑TEST
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*--------------------------------------------------------------*
*    FILE = 振分リスト出力ワーク  RL=256    BF=15     ORG=SF   *
*--------------------------------------------------------------*
 FD  HURXXXW            BLOCK     CONTAINS  15   RECORDS.
     COPY     HURXXXW   OF        XFDLIB
              JOINING    HUR  AS   PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 作場マスタ　　　　　　　　　　　　　　　　　　　   *
*--------------------------------------------------------------*
 FD  SAKUBAL1 LABEL RECORD   IS   STANDARD.
     COPY     SAKUBAL1   OF        XFDLIB
              JOINING    SAK  AS   PREFIX.
*--------------------------------------------------------------*
*    ナフコ商品マスタ                                          *
*--------------------------------------------------------------*
 FD  NFSHOMS            LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS   OF        XFDLIB
              JOINING   NFM       PREFIX.
*--------------------------------------------------------------*
*    商品変換テーブル                                          *
*--------------------------------------------------------------*
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
*
*↓2016/08/23
*--------------------------------------------------------------*
*    ナフコ商品分類パターンマスタ
*--------------------------------------------------------------*
 FD  NFSHBPL1           LABEL RECORD   IS   STANDARD.
     COPY     NFSHBPL1  OF        XFDLIB
              JOINING   SHO       PREFIX.
*↑2016/08/23
*--------------------------------------------------------------*
*    FILE = プリントファイル                                   *
*--------------------------------------------------------------*
 FD  PRINTF.
 01  P-REC                        PIC       X(200).
*
*↓TEST
*--------------------------------------------------------------*
*    FILE = 内部テーブルワーク　  RL=3651   BF=1      ORG=SF   *
*--------------------------------------------------------------*
*FD  WKTENPO            BLOCK     CONTAINS  1    RECORDS.
*    COPY     WKTENPO   OF        XFDLIB
*             JOINING   TEN   AS   PREFIX.
*
*↑TEST
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** フラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  SAK-INV-FLG                  PIC       X(03)  VALUE  SPACE.
 01  NFSHOMS-INV-FLG              PIC       X(03)  VALUE  SPACE.
 01  HSHOTBL-INV-FLG              PIC       X(03)  VALUE  SPACE.
*↓2016/08/23
 01  SHO-INV-FLG                  PIC       X(03)  VALUE  SPACE.
*↑2016/08/23
*
**** ステイタス　エリア
 01  HUR-STATUS                   PIC       X(02).
 01  SAK-STATUS                   PIC       X(02).
 01  NFM-STATUS                   PIC       X(02).
 01  TBL-STATUS                   PIC       X(02).
*↓2016/08/23
 01  SHO-STATUS                   PIC       X(02).
*↑2016/08/23
*
*↓TEST
*01  TEN-STATUS                   PIC       X(02).
*↑TEST
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
***** 発注日ワーク
 01  HACHU-HIZUKE.
     03  HACYMD                   PIC       9(08)  VALUE  ZERO.
     03  HACYMD-R                 REDEFINES HACYMD.
         05  HUR-YY               PIC       9(04).
         05  HUR-MM               PIC       9(02).
         05  HUR-DD               PIC       9(02).
*
***** 納品日ワーク
 01  NOUHIN-HIZUKE.
     03  NOUYMD                   PIC       9(08)  VALUE  ZERO.
     03  NOUYMD-R                 REDEFINES NOUYMD.
         05  NOU-YY               PIC       9(04).
         05  NOU-MM               PIC       9(02).
         05  NOU-DD               PIC       9(02).
*
***** カウンタ
 01  P-CNT                        PIC       9(04)  VALUE  ZERO.
 01  L-CNT                        PIC       9(03)  VALUE  ZERO.
 01  CNT-READ                     PIC       9(06)  VALUE  ZERO.
 01  MISE-CNT                     PIC       9(03)  VALUE  ZERO.
*
***** ラインカウンタ計算領域
 01  W-L-CNT                      PIC       9(03)  VALUE  ZERO.
*
***** 店インデックス
 01  IDX-CNT                      PIC       9(03)  VALUE  ZERO.
*
*↓2016/08/23
***** パターンＣＤインデックス
 01  PTN-IDX-CNT                  PIC       9(02)  VALUE  ZERO.
 01  PTN-SET-CNT                  PIC       9(02)  VALUE  ZERO.
*↑2016/08/23
*
***** 店テーブルワーク
 01  WK-MISE.
     03  FILLER                   PIC       X(01)  VALUE  "(".
     03  MISE                     PIC       9(05)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  ")".
***** テーブルワーク
*↓2016/08/23
*01  TBL-MISE-AREA.
*--- 03  TBL-MISE                 OCCURS    400.
*---     05  T-MISE               PIC       9(05).
*---     05  T-SURYO              PIC       9(05).
 01  TBL-MISE-AREA.
   02  TBL-PTN                    OCCURS    50.
     03  TBL-PTNCD                PIC       X(02).
     03  TBL-PTNSURYO             PIC       9(06).
     03  TBL-MISE                 OCCURS    500.
         05  T-MISE               PIC       9(05).
         05  T-SURYO              PIC       9(05).
*↑2016/08/23
*
*↓TEST
*01  TEST-AREA.
*    03  TEST-IX                  PIC       9(03)  VALUE  ZERO.
*↑TEST
 01  WK-BACHI-AREA.
     03  WK-JDATE                 PIC       9(08)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  WK-JTIME                 PIC       9(04)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  WK-TORICD                PIC       9(08)  VALUE  ZERO.
*
 01  WL-IDX-AREA.
     03  WK-IXM                   PIC       9(03)  VALUE  ZERO.
     03  WK-IXD                   PIC       9(03)  VALUE  ZERO.
     03  WK-IX                    PIC       9(03)  VALUE  ZERO.
*
 01  WK-NEW-KEY.
     03  NEW-NOHINBI              PIC       9(06)  VALUE  ZERO.
     03  NEW-BUMONCD              PIC       X(04)  VALUE  SPACE.
     03  NEW-SYOHINCD             PIC       X(16)  VALUE  SPACE.
*↓2016/08/23
*--- 03  NEW-RTCD                 PIC       X(10)  VALUE  SPACE.
     03  NEW-SYOBUNCD             PIC       X(08)  VALUE  SPACE.
     03  NEW-PATNCD               PIC       X(02)  VALUE  SPACE.
*↑2016/08/23
     03  NEW-SAKUBACD             PIC       X(02)  VALUE  SPACE.
*
 01  WK-OLD-KEY.
     03  OLD-NOHINBI              PIC       9(06)  VALUE  ZERO.
     03  OLD-BUMONCD              PIC       X(04)  VALUE  SPACE.
     03  OLD-SYOHINCD             PIC       X(16)  VALUE  SPACE.
*↓2016/08/23
*    03  OLD-RTCD                 PIC       X(10)  VALUE  SPACE.
     03  OLD-SYOBUNCD             PIC       X(08)  VALUE  SPACE.
     03  OLD-PATNCD               PIC       X(02)  VALUE  SPACE.
*↑2016/08/23
     03  OLD-SAKUBACD             PIC       X(02)  VALUE  SPACE.
*
***** 原価単価保存ワーク
 01  WGENTAN                      PIC       9(07)V99 VALUE ZERO.
*
***** 売価単価保存ワーク
 01  WBAITAN                      PIC       9(07)  VALUE  ZERO.
*
***** 自社商品コード保存ワーク
 01  WJISHOCD                     PIC       X(16)  VALUE  SPACE.
*
***** 商品名保存ワーク
 01  WSYOHINNM                    PIC       X(30)  VALUE  SPACE.
*
***** 規格保存ワーク
 01  WKIKAKU                      PIC       X(15)  VALUE  SPACE.
*
***** 相手商品保存ワーク
 01  WAITECD                      PIC       X(13)  VALUE  SPACE.
*
***** 企業名保存ワーク
 01  WKIGYONM                     PIC       X(20)  VALUE  SPACE.
***** 作場名保存ワーク??
 01  WSAKUBAMEI                   PIC       N(10)  VALUE  SPACE.
***** 棚番保存ワーク
 01  WTANABAN                     PIC       X(06)  VALUE  SPACE.
***** 管理番号ワーク
 01  WKANRINO                     PIC       9(08)  VALUE  ZERO.
*↓2016/08/23
***** 商品分類保存ワーク
 01  WSYOBUNCD                    PIC       X(08)  VALUE  SPACE.
 01  WSYOBUNNM                    PIC       N(15)  VALUE  SPACE.
***** パターン保存ワーク
 01  WPATNCD                      PIC       X(02)  VALUE  SPACE.
*↑2016/08/23
*
***  2012/08/15 START
***** 小売区分ワーク
 01  WK-KOURI-KBN                 PIC       X(01)  VALUE  SPACE.
***  2012/08/15 END
***** 合計計算ワーク
 01  WSURYO                       PIC       9(06)  VALUE  ZERO.
 01  WGENHEI                      PIC       9(07)  VALUE  ZERO.
 01  WBAIHEI                      PIC       9(07)  VALUE  ZERO.
*マスタ存在チェック結果退避
 01  WNFSHOMS                     PIC       X(03)  VALUE  SPACE.
 01  WHSHOTBL                     PIC       X(03)  VALUE  SPACE.
*
***** 合計見出し領域
 01  G-MIDASI      CHARACTER      TYPE      IS     YA.
     03  SYO-MI                   PIC       N(05)  VALUE
       NC"【小　計】".
     03  CYU-MI                   PIC       N(05)  VALUE
       NC"【中　計】".
     03  GOU-MI                   PIC       N(05)  VALUE
       NC"【合　計】".
     03  SAK-MI                   PIC       N(05)  VALUE
       NC"【作場計】".
     03  SOU-MI                   PIC       N(05)  VALUE
       NC"【総合計】".
*
***** 合計集計ワーク
 01  G-WORK.
     03  SYO-WORK.
         05  SYO-SU               PIC       9(06)  VALUE  ZERO.
         05  SYO-GENKIN           PIC      S9(09)  VALUE  ZERO.
         05  SYO-GENHEI           PIC       9(07)  VALUE  ZERO.
         05  SYO-BAIKIN           PIC      S9(09)  VALUE  ZERO.
         05  SYO-BAIHEI           PIC       9(07)  VALUE  ZERO.
     03  CYU-WORK.
         05  CYU-SU               PIC       9(06)  VALUE  ZERO.
         05  CYU-GENKIN           PIC      S9(09)  VALUE  ZERO.
         05  CYU-GENHEI           PIC       9(07)  VALUE  ZERO.
         05  CYU-BAIKIN           PIC      S9(09)  VALUE  ZERO.
         05  CYU-BAIHEI           PIC       9(07)  VALUE  ZERO.
     03  GOU-WORK.
         05  GOU-SU               PIC       9(06)  VALUE  ZERO.
         05  GOU-GENKIN           PIC      S9(09)  VALUE  ZERO.
         05  GOU-GENHEI           PIC       9(07)  VALUE  ZERO.
         05  GOU-BAIKIN           PIC      S9(09)  VALUE  ZERO.
         05  GOU-BAIHEI           PIC       9(07)  VALUE  ZERO.
     03  SAK-WORK.
         05  SAK-SU               PIC       9(06)  VALUE  ZERO.
         05  SAK-GENKIN           PIC      S9(09)  VALUE  ZERO.
         05  SAK-GENHEI           PIC       9(07)  VALUE  ZERO.
         05  SAK-BAIKIN           PIC      S9(09)  VALUE  ZERO.
         05  SAK-BAIHEI           PIC       9(07)  VALUE  ZERO.
     03  SOU-WORK.
         05  SOU-SU               PIC       9(06)  VALUE  ZERO.
         05  SOU-GENKIN           PIC      S9(09)  VALUE  ZERO.
         05  SOU-GENHEI           PIC       9(07)  VALUE  ZERO.
         05  SOU-BAIKIN           PIC      S9(09)  VALUE  ZERO.
         05  SOU-BAIHEI           PIC       9(07)  VALUE  ZERO.
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3815L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3815L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY3815L".
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
***** 見出し行１
 01  MIDASHI1.
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  X(08)  VALUE "SSY3815L".
     03  FILLER                   PIC  X(26)  VALUE  SPACE.
     03  FILLER                   PIC  N(15)  VALUE
       NC"＜オンライン作場別振分リスト＞"
                   CHARACTER      TYPE IS     YB-21.
     03  FILLER                   PIC  X(02)  VALUE  SPACE.
     03  HD1-TEISEI               PIC  N(06)
                   CHARACTER      TYPE IS     YA.
     03  FILLER                   PIC  X(19)  VALUE  SPACE.
     03  HD1-YY                   PIC  9999.
     03  FILLER                   PIC  N(01)  VALUE
       NC"年"
                   CHARACTER      TYPE IS     YA.
     03  HD1-MM                   PIC  Z9.
     03  FILLER                   PIC  N(01)  VALUE
       NC"月"
                   CHARACTER      TYPE IS     YA.
     03  HD1-DD                   PIC  Z9.
     03  FILLER                   PIC  N(01)  VALUE
       NC"日"
                   CHARACTER      TYPE IS     YA.
     03  FILLER                   PIC  X(02)  VALUE  SPACE.
     03  HD1-PAGE                 PIC  ZZ9.
     03  FILLER                   PIC  N(01)  VALUE
       NC"頁"
                   CHARACTER      TYPE IS     YA.
***** 見出し行２
 01  MIDASHI2.
*    03  FILLER                   PIC  X(43)  VALUE  SPACE.
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  N(06)  VALUE
       NC"作場コード："
                   CHARACTER      TYPE      IS     YA.
     03  HD2-SAKUBACD             PIC  X(02).
     03  FILLER                   PIC  X(04)  VALUE  SPACE.
     03  HD2-SAKUBAMEI            PIC  N(10)  VALUE  SPACE
                   CHARACTER      TYPE IS     YA.
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE
       NC"［"
                   CHARACTER      TYPE IS     YA.
     03  FILLER                   PIC  N(05)  VALUE
       NC"管理番号："  CHARACTER  TYPE IS     YA.
     03  HD2-KANRINO              PIC  9(08).
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  X(07)  VALUE "ﾊﾞｯﾁNO:".
     03  HD2-BACHI                PIC  X(22).
     03  FILLER                   PIC  N(01)  VALUE
       NC"］"
                   CHARACTER      TYPE IS     YA.
*
***** 見出し行３
 01  MIDASHI3.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(06)  VALUE
       NC"発注企業名："
                   CHARACTER      TYPE      IS     YA.
     03  KIGYONM                  PIC       X(20).
     03  FILLER                   PIC       X(18)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE
       NC"（"
                   CHARACTER      TYPE IS     YA.
     03  OUTNONM                  PIC  N(07)
                   CHARACTER      TYPE IS     YA.
     03  FILLER                   PIC  N(01)  VALUE
       NC"）"
                   CHARACTER      TYPE IS     YA.
     03  FILLER                   PIC       X(36)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"発注日："
                   CHARACTER      TYPE      IS     YA.
     03  H-YY                     PIC       9999.
     03  FILLER                   PIC       N(01)  VALUE
       NC"年"
                   CHARACTER      TYPE      IS     YA.
     03  H-MM                     PIC       Z9.
     03  FILLER                   PIC       N(01)  VALUE
       NC"月"
                   CHARACTER      TYPE      IS     YA.
     03  H-DD                     PIC       Z9.
     03  FILLER                   PIC       N(01)  VALUE
       NC"日"
                   CHARACTER      TYPE      IS     YA.
     03  FILLER                   PIC       X(09)  VALUE  SPACE.
*
***** 見出し行４
 01  MIDASHI4      CHARACTER      TYPE      IS     YA.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(06)  VALUE
       NC"部門コード：".
     03  BUMONCD                  PIC       X(04).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"ルート：".
     03  RTCD                     PIC       X(10).
*↓2016/08/23
     03  FILLER                   PIC       X(12)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"商品分類".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  SYOBUNCD                 PIC       X(08).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  SYOBUNNM                 PIC       N(15).
*    03  FILLER                   PIC       X(68)  VALUE  SPACE.
     03  FILLER                   PIC       X(08)  VALUE  SPACE.
*↑2016/08/23
     03  FILLER                   PIC       N(04)  VALUE
       NC"納品日：".
     03  N-YY                     PIC       9999.
     03  FILLER                   PIC       N(01)  VALUE
       NC"年".
     03  N-MM                     PIC       Z9.
     03  FILLER                   PIC       N(01)  VALUE
       NC"月".
     03  N-DD                     PIC       Z9.
     03  FILLER                   PIC       N(01)  VALUE
       NC"日".
     03  FILLER                   PIC       X(09)  VALUE  SPACE.
*
***** 見出し行5
 01  MIDASHI5      CHARACTER      TYPE      IS     YA.
     03  FILLER                   PIC       N(02)  VALUE
       NC"棚番".
*
***** 見出し行6
 01  MIDASHI6      CHARACTER      TYPE      IS     YA.
*****03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(07)  VALUE
       NC"自社商品コード".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(05)  VALUE
       NC"商　品　名".
*↓2016/08/23
*    03  FILLER                   PIC       X(22)  VALUE  SPACE.
     03  FILLER                   PIC       X(10)  VALUE  SPACE.
*↑2016/08/23
     03  FILLER                   PIC       N(03)  VALUE
       NC"原単価".
     03  FILLER                   PIC       X(05)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"数量合計".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
*
***** 見出し行7
 01  MIDASHI7      CHARACTER      TYPE      IS     YA.
*****03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(07)  VALUE
       NC"相手商品コード".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
*↓2016/08/23
*    03  FILLER                   PIC       N(03)  VALUE
*      NC"規　格".
     03  FILLER                   PIC       X(06)  VALUE  SPACE.
*    03  FILLER                   PIC       X(26)  VALUE  SPACE.
     03  FILLER                   PIC       X(14)  VALUE  SPACE.
*↑2016/08/23
     03  FILLER                   PIC       N(03)  VALUE
       NC"売単価".
*↓2016/08/23
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       X(06)  VALUE "(ﾊﾟﾀｰﾝ".
     03  FILLER                   PIC       N(02)  VALUE
       NC"合計".
     03  FILLER                   PIC       X(01)  VALUE ")".
*    03  FILLER                   PIC       X(12)  VALUE  SPACE.
*↑2016/08/23
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(10)  VALUE  SPACE.
*
***** 見出し行8
 01  MIDASHI8.
     03  PRT-SEN                  OCCURS    136.
         05  FILLER               PIC       X(01)  VALUE  "=".
***** 見出し行9
*01  MIDASHI9.
*    03  PRT-TENSEN               OCCURS    136.
*        05  FILLER               PIC       X(01)  VALUE  "-".
*
* 流用元、この行を使用していない！
***** 明細行３
*01  MIDASHI7.
*    03  MID-TEN                  PIC       N(68)  VALUE  SPACE.
*
***** 明細行0
 01  MEISAI0                      CHARACTER  TYPE  IS  YA.
     03  TANABAN                  PIC       X(06).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MASTERCHK1               PIC       N(10).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MASTERCHK2               PIC       N(10).
*
***** 明細行１
 01  MEISAI1.
     03  SYOHINCD                 PIC       X(16).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
*↓2016/08/23
*    03  SYOHINNM                 PIC       X(30).
     03  SYOHINNM1                PIC       X(15).
*    03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
*↑2016/08/23
     03  GENTAN                   PIC       ZZZZZZ9.99.
     03  GENTANR   REDEFINES      GENTAN    PIC    X(10).
*↓2016/08/23
*    03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       X(05)  VALUE  SPACE.
*↑2016/08/23
     03  SURYOKEI                 PIC       ZZZZZZ.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MISE-TBL                 OCCURS    10.
         05  MISEBAN              PIC       X(07)  VALUE  SPACE.
*
***** 明細行２
 01  MEISAI2.
     03  MEI2-SHOCD               PIC       X(13).
     03  FILLER                   PIC       X(04)  VALUE  SPACE.
*↓2016/08/23
*    03  KIKAKU                   PIC       X(15).
     03  SYOHINNM2                PIC       X(15).
*    03  FILLER                   PIC       X(03)  VALUE  SPACE.
*↑2016/08/23
*↓2016/08/23
*    03  MEI2-KOURI-KBN           PIC       N(06)
*                  CHARACTER      TYPE      IS     YA.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
*↑2016/08/23
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  BAITAN                   PIC       ZZZZZZZ.
*↓2016/08/23
     03  FILLER                   PIC       X(04)  VALUE  SPACE.
     03  PATNCD                   PIC       X(02)  VALUE  SPACE.
     03  KAKKO1                   PIC       X(01)  VALUE "(".
     03  PATNGK                   PIC       ZZZZZZZ.
     03  KAKKO2                   PIC       X(02)  VALUE ") ".
*    03  FILLER                   PIC       X(10)  VALUE  SPACE.
*    03  FILLER                   PIC       X(01)  VALUE  SPACE.
*↑2016/08/23
     03  SURYO-TBL                OCCURS    10.
         05  SURYO                PIC       ZZZZZ  VALUE  ZERO.
         05  FILLER               PIC       X(02)  VALUE  SPACE.
*
***** 明細行３
 01  MEISAI3.
     03  PRT-TEN                  OCCURS    136.
         05  FILLER               PIC       X(01)  VALUE  "-".
*
***** 合計行
 01  GOUKEI        CHARACTER      TYPE      IS     YA.
     03  FILLER                   PIC       X(16)  VALUE  SPACE.
     03  GKMIDASI                 PIC       N(05).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"数量合計".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  SUKEI                    PIC       ZZZ,ZZ9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"原価金額".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  GENKEI                   PIC       ----,---,--9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"平均原価".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  GENHEI                   PIC       Z,ZZZ,ZZ9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"売価金額".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  BAIKEI                   PIC       ----,---,--9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"平均売価".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  BAIHEI                   PIC       Z,ZZZ,ZZ9.
     03  FILLER                   PIC       X(06)  VALUE  SPACE.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
*01  PARA-TKBN              PIC   9(01).
 01  PARA-OUTNO             PIC   9(01).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
*PROCEDURE              DIVISION  USING    PARA-TKBN
*                                          PARA-OUTNO.
 PROCEDURE              DIVISION  USING    PARA-OUTNO.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE HURXXXW.
     MOVE     "HURXXXW "         TO        ERR-FL-ID.
     MOVE     HUR-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE SAKUBAL1.
     MOVE     "SAKUBAL1"          TO        ERR-FL-ID.
     MOVE     SAK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFSHOMS.
     MOVE     "NFSHOMS1"          TO        ERR-FL-ID.
     MOVE     NFM-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
 FILEERROR-SEC4         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE HSHOTBL.
     MOVE     "SHOTBL1 "          TO        ERR-FL-ID.
     MOVE     TBL-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
*↓2016/08/23
 FILEERROR-SEC5         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFSHBPL1.
     MOVE     "NFSHBPL1"          TO        ERR-FL-ID.
     MOVE     SHO-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*↑2016/08/23
*
*↓TEST
*FILEERROR-SEC6         SECTION.
*    USE AFTER          EXCEPTION
*                       PROCEDURE WKTENPO.
*    MOVE     "WKTENPO "          TO        ERR-FL-ID.
*    MOVE     TEN-STATUS          TO        ERR-STCD.
*    DISPLAY  MSG-ABEND1          UPON      CONS.
*    DISPLAY  MSG-ABEND2          UPON      CONS.
*    DISPLAY  SEC-NAME            UPON      CONS.
*    STOP     RUN.
*↑TEST
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY3815L-START          SECTION.
*
     MOVE     "SSY3815L-START"     TO   S-NAME.
     PERFORM            INIT-SEC.
*
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SSY3815L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     HURXXXW.
     OPEN     INPUT     SAKUBAL1   NFSHOMS  HSHOTBL.
*↓2016/08/23
     OPEN     INPUT     NFSHBPL1.
*↑2016/08/23
     OPEN     OUTPUT    PRINTF.
*↓TEST
*    OPEN     OUTPUT    WKTENPO.
*↑TEST
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
     PERFORM  HUR-RD-SEC.
     IF       END-FLG   =   "END"
              GO        TO        INIT-EXIT
     END-IF.
*
     MOVE     1         TO        IDX-CNT.
     PERFORM  CLR-SEC   UNTIL     IDX-CNT   >   10.
*
     MOVE     1         TO        IDX-CNT   MISE-CNT.
*↓2016/08/23
     MOVE     1         TO        PTN-IDX-CNT.
*↑2016/08/23
     MOVE     ZERO      TO        WK-IXM.
*↓2016/08/23
*--- MOVE     ZERO      TO        TBL-MISE-AREA.
     INITIALIZE                   TBL-MISE-AREA.
*↑2016/08/23
     MOVE     HUR-F05   TO        HACYMD.
     MOVE     HUR-F06   TO        NOUYMD.
     MOVE     HUR-F04   TO        OLD-BUMONCD.
*****MOVE     HUR-F03   TO        OLD-RTCD.
     MOVE     HUR-F03   TO        OLD-SAKUBACD.
*↓2016/08/23
*--- MOVE     HUR-F20   TO        OLD-RTCD.
     MOVE     HUR-F24   TO        OLD-SYOBUNCD.
     MOVE     HUR-F25   TO        OLD-PATNCD.
*
     IF       PARA-OUTNO     =    2
              MOVE      SPACE     TO   OLD-SYOHINCD
              MOVE      HUR-F08   TO   OLD-SYOHINCD
     ELSE
              MOVE      SPACE     TO   OLD-SYOHINCD
              MOVE      HUR-F07   TO   OLD-SYOHINCD
     END-IF.
*
     MOVE     HUR-F15   TO        WGENTAN.
     MOVE     HUR-F16   TO        WBAITAN.
     MOVE     HUR-F11   TO        WSYOHINNM.
     MOVE     HUR-F12   TO        WKIKAKU.
     MOVE     HUR-F17   TO        WKIGYONM.
     MOVE     HUR-F07   TO        WAITECD.
*↓2016/08/23
     MOVE     HUR-F25   TO        WPATNCD.
*↑2016/08/23
*ナフコ商品マスタ存在チェック
     MOVE     HUR-F07   TO        NFM-F01.
     PERFORM  NFSHOMS-READ-SEC.
     IF  NFSHOMS-INV-FLG = "INV"
         MOVE "CHK"     TO        WNFSHOMS
     ELSE
         MOVE SPACE     TO        WNFSHOMS
     END-IF.
*商品変換ＴＢＬ存在チェック
     MOVE     HUR-F013  TO        TBL-F01.
     MOVE     HUR-F07   TO        TBL-F02.
     PERFORM  HSHOTBL-READ-SEC.
     IF  HSHOTBL-INV-FLG = "INV"
         MOVE "CHK"     TO        WHSHOTBL
     ELSE
         MOVE SPACE     TO        WHSHOTBL
     END-IF.
     MOVE     HUR-F08   TO        WJISHOCD.
     MOVE     HUR-F22   TO        WK-KOURI-KBN.
     MOVE     HUR-F23   TO        WKANRINO.
     MOVE     HUR-F03   TO        SAK-F01.
     PERFORM  SAK-RD-SEC.
     IF       SAK-INV-FLG  NOT = "INV"
              MOVE     SAK-F02    TO        WSAKUBAMEI
     ELSE
              MOVE     ALL NC"？" TO        WSAKUBAMEI
     END-IF.
     MOVE     HUR-F09   TO        WTANABAN.
*↓2016/08/23
*　商品分類名取得
     MOVE     HUR-F24   TO        SHO-F01.
     MOVE     SPACE     TO        SHO-F03.
     PERFORM  NFSHBPL1-READ-SEC.
     IF       SHO-INV-FLG  NOT = "INV"
              MOVE     SHO-F02    TO        WSYOBUNNM
     ELSE
              MOVE     ALL NC"？" TO        WSYOBUNNM
     END-IF.
*↑2016/08/23
*
     MOVE     HUR-F011  TO        WK-JDATE.
     MOVE     HUR-F012  TO        WK-JTIME.
     MOVE     HUR-F013  TO        WK-TORICD.
*
     PERFORM  MIDASI-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*             振分リスト出力ワークＲＥＡＤ処理      1.1
****************************************************************
 HUR-RD-SEC             SECTION.
*
     MOVE    "HUR-RD-SEC"    TO   S-NAME.
     MOVE     NEW-NOHINBI    TO   OLD-NOHINBI.
     MOVE     NEW-BUMONCD    TO   OLD-BUMONCD.
     MOVE     NEW-SYOHINCD   TO   OLD-SYOHINCD.
*↓2016/08/23
*    MOVE     NEW-RTCD       TO   OLD-RTCD.
     MOVE     NEW-SYOBUNCD   TO   OLD-SYOBUNCD.
     MOVE     NEW-PATNCD     TO   OLD-PATNCD.
*↑2016/08/23
     MOVE     NEW-SAKUBACD   TO   OLD-SAKUBACD.
*
     READ     HURXXXW
          AT END
              MOVE     "END"      TO        END-FLG
              MOVE     HIGH-VALUE TO        WK-NEW-KEY
          NOT AT END
              MOVE     HUR-F06    TO        NEW-NOHINBI
*             IF   PARA-OUTNO     =    1
              IF   PARA-OUTNO     =    2
                   MOVE      HUR-F08   TO   NEW-SYOHINCD
              ELSE
                   MOVE      HUR-F07   TO   NEW-SYOHINCD
              END-IF
              MOVE     HUR-F04    TO        NEW-BUMONCD
*******       MOVE     HUR-F03    TO        NEW-RTCD
*↓2016/08/23
*             MOVE     HUR-F20    TO        NEW-RTCD
              MOVE     HUR-F24    TO        NEW-SYOBUNCD
              MOVE     HUR-F25    TO        NEW-PATNCD
*↑2016/08/23
              MOVE     HUR-F03    TO        NEW-SAKUBACD
              ADD      1          TO        CNT-READ
     END-READ.
*
 HUR-RD-EXIT.
     EXIT.
*
****************************************************************
*             作場マスタＲＥＡＤ処理      1.1
****************************************************************
 SAK-RD-SEC             SECTION.
*
     MOVE    "SAK-RD-SEC"    TO   S-NAME.
*
     MOVE    "   "                TO        SAK-INV-FLG.
     READ     SAKUBAL1
          INVALID
              MOVE   "INV"        TO        SAK-INV-FLG
     END-READ.
*
 SAK-RD-EXIT.
     EXIT.
*
*↓2016/08/23
****************************************************************
*             商品分類パターンマスタＲＥＡＤ処理      1.1
****************************************************************
 NFSHBPL1-READ-SEC             SECTION.
*
     MOVE    "NFSHBPL1-READ-SEC"  TO   S-NAME.
*
     MOVE    "   "                TO        SHO-INV-FLG.
     READ     NFSHBPL1
          INVALID
              MOVE   "INV"        TO        SHO-INV-FLG
     END-READ.
*
 NFSHBPL1-READ-EXIT.
     EXIT.
*
****************************************************************
*             見出し出力処理                1.2                *
****************************************************************
 MIDASI-SEC             SECTION.
*
     MOVE    "MIDASI-SEC"              TO    S-NAME.
*    IF       PARA-TKBN      =    1
*             MOVE NC"（発注数量）"    TO    HD1-TEISEI
*    ELSE
*             MOVE NC"（訂正数量）"    TO    HD1-TEISEI
*    END-IF.
     MOVE     NC"（発注数量）"    TO    HD1-TEISEI.
     MOVE     SYS-YY         TO   HD1-YY.
     MOVE     SYS-MM         TO   HD1-MM.
     MOVE     SYS-DD         TO   HD1-DD.
     ADD      1              TO   P-CNT.
     MOVE     P-CNT          TO   HD1-PAGE.
*
     MOVE     OLD-SAKUBACD   TO   HD2-SAKUBACD
                                  SAK-F01.
     PERFORM  SAK-RD-SEC.
     IF       SAK-INV-FLG  NOT = "INV"
              MOVE     SAK-F02    TO        HD2-SAKUBAMEI
     ELSE
              MOVE     ALL NC"？" TO        HD2-SAKUBAMEI
     END-IF.
     MOVE     WKANRINO       TO   HD2-KANRINO.
*↓2016/08/23
     MOVE     OLD-SYOBUNCD   TO   SYOBUNCD
                                  SHO-F01.
     MOVE     SPACE          TO   SHO-F03.
     PERFORM  NFSHBPL1-READ-SEC.
     IF       SHO-INV-FLG  NOT = "INV"
              MOVE     SHO-F02    TO        SYOBUNNM
     ELSE
              MOVE     ALL NC"？" TO        SYOBUNNM
     END-IF.
*↑2016/08/23
*
     MOVE     WK-BACHI-AREA  TO   HD2-BACHI.
*
     MOVE     WKIGYONM       TO   KIGYONM.
     IF       PARA-OUTNO     =    2
              MOVE      NC"サカタコード順"  TO   OUTNONM
     ELSE
              MOVE      NC"　相手商品順　"  TO   OUTNONM
     END-IF.
     MOVE     HUR-YY         TO   H-YY.
     MOVE     HUR-MM         TO   H-MM.
     MOVE     HUR-DD         TO   H-DD.
     MOVE     OLD-BUMONCD    TO   BUMONCD.
*↓2016/08/23
*    MOVE     OLD-RTCD       TO   RTCD.
     MOVE     SPACE          TO   RTCD.
*↑2016/08/23
     MOVE     NOU-YY         TO   N-YY.
     MOVE     NOU-MM         TO   N-MM.
     MOVE     NOU-DD         TO   N-DD.
*
     IF       P-CNT     NOT =     1
              MOVE      SPACE     TO        P-REC
              WRITE     P-REC     AFTER     PAGE
     END-IF.
*
     WRITE    P-REC     FROM      MIDASHI1  AFTER     1.
     WRITE    P-REC     FROM      MIDASHI2  AFTER     1.
     WRITE    P-REC     FROM      MIDASHI3  AFTER     1.
     WRITE    P-REC     FROM      MIDASHI4  AFTER     1.
     WRITE    P-REC     FROM      MIDASHI8  AFTER     1.
     WRITE    P-REC     FROM      MIDASHI5  AFTER     1.
*    WRITE    P-REC     FROM      MIDASHI6  AFTER     2.
     WRITE    P-REC     FROM      MIDASHI6  AFTER     1.
     WRITE    P-REC     FROM      MIDASHI7  AFTER     1.
     WRITE    P-REC     FROM      MIDASHI8  AFTER     1.
*    MOVE     SPACE     TO        P-REC.
*    WRITE    P-REC     AFTER     1.
*
*    MOVE     9         TO        L-CNT.
     MOVE     11        TO        L-CNT.
*
 MIDASI-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
*    作場チェック  作場計
     IF       HUR-F03    NOT =     OLD-SAKUBACD
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              PERFORM   CYUKEI-SEC
              PERFORM   GOUKEI-SEC
              PERFORM   SAKKEI-SEC
              MOVE      HUR-F04    TO        OLD-BUMONCD
*↓2016/08/23
*             MOVE      HUR-F20    TO        OLD-RTCD
              MOVE      HUR-F24    TO        OLD-SYOBUNCD
*↑2016/08/23
              MOVE      HUR-F06    TO        NOUYMD
              MOVE      HUR-F03    TO        OLD-SAKUBACD
*↓2016/08/23
              IF  PARA-OUTNO     =    2
                  MOVE  SPACE      TO        OLD-SYOHINCD
                  MOVE  HUR-F08    TO        OLD-SYOHINCD
              ELSE
                  MOVE  SPACE      TO        OLD-SYOHINCD
                  MOVE  HUR-F07    TO        OLD-SYOHINCD
              END-IF
              MOVE      HUR-F25    TO        OLD-PATNCD
*↑2016/08/23
              PERFORM   MIDASI-SEC
     END-IF.
*
*
*    合計チェック  部門計
     IF       HUR-F04    NOT =     OLD-BUMONCD
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              PERFORM   CYUKEI-SEC
              PERFORM   GOUKEI-SEC
              MOVE      HUR-F04    TO        OLD-BUMONCD
*******       MOVE      HUR-F03    TO        OLD-RTCD
*↓2016/08/23
*             MOVE      HUR-F20    TO        OLD-RTCD
              MOVE      HUR-F24    TO        OLD-SYOBUNCD
*↑2016/08/23
              MOVE      HUR-F06    TO        NOUYMD
              MOVE      HUR-F03    TO        OLD-SAKUBACD
*↓2016/08/23
              IF  PARA-OUTNO     =    2
                  MOVE  SPACE      TO        OLD-SYOHINCD
                  MOVE  HUR-F08    TO        OLD-SYOHINCD
              ELSE
                  MOVE  SPACE      TO        OLD-SYOHINCD
                  MOVE  HUR-F07    TO        OLD-SYOHINCD
              END-IF
              MOVE      HUR-F25    TO        OLD-PATNCD
*↑2016/08/23
              PERFORM   MIDASI-SEC
     END-IF.
*
*    中計チェック　ルート計 →　商品分類計に変更
*****IF       HUR-F03    NOT =     OLD-RTCD
*↓2016/08/23
*    IF       HUR-F20    NOT =     OLD-RTCD
     IF       HUR-F24    NOT =     OLD-SYOBUNCD
*↑2016/08/23
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              PERFORM   CYUKEI-SEC
              MOVE      HUR-F04    TO        OLD-BUMONCD
******        MOVE      HUR-F03    TO        OLD-RTCD
*↓2016/08/23
*             MOVE      HUR-F20    TO        OLD-RTCD
              MOVE      HUR-F24    TO        OLD-SYOBUNCD
*↑2016/08/23
              MOVE      HUR-F06    TO        NOUYMD
              MOVE      HUR-F03    TO        OLD-SAKUBACD
*↓2016/08/23
              IF  PARA-OUTNO     =    2
                  MOVE  SPACE      TO        OLD-SYOHINCD
                  MOVE  HUR-F08    TO        OLD-SYOHINCD
              ELSE
                  MOVE  SPACE      TO        OLD-SYOHINCD
                  MOVE  HUR-F07    TO        OLD-SYOHINCD
              END-IF
              MOVE      HUR-F25    TO        OLD-PATNCD
*↑2016/08/23
              PERFORM   MIDASI-SEC
     END-IF.
*
*    小計チェック　納品日計
     IF       HUR-F06    NOT =     NOUYMD
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              MOVE      HUR-F06    TO        NOUYMD
*↓2016/08/23
              IF  PARA-OUTNO     =    2
                  MOVE  SPACE      TO        OLD-SYOHINCD
                  MOVE  HUR-F08    TO        OLD-SYOHINCD
              ELSE
                  MOVE  SPACE      TO        OLD-SYOHINCD
                  MOVE  HUR-F07    TO        OLD-SYOHINCD
              END-IF
              MOVE      HUR-F25    TO        OLD-PATNCD
*↑2016/08/23
              PERFORM   MIDASI-SEC
     END-IF.
*
*    商品ＣＤブレイク判定・制御
*    1：相手商品順　2：自社商品順
     IF  PARA-OUTNO     =    2
         IF   HUR-F08   NOT =     OLD-SYOHINCD
*             明細出力制御
              PERFORM   S-CHECK-SEC
              MOVE      SPACE      TO        OLD-SYOHINCD
              MOVE      HUR-F08    TO        OLD-SYOHINCD
              MOVE      HUR-F25    TO        OLD-PATNCD
         END-IF
     ELSE
         IF   HUR-F07    NOT =     OLD-SYOHINCD(1:13)
*             明細出力制御
              PERFORM   S-CHECK-SEC
              MOVE      SPACE      TO        OLD-SYOHINCD
              MOVE      HUR-F07    TO        OLD-SYOHINCD
              MOVE      HUR-F25    TO        OLD-PATNCD
         END-IF
     END-IF.
*
*    明細行(内部テーブル)セット
     ADD      1         TO        WK-IXM.
*
*↓2016/08/23
**** MOVE     HUR-F02   TO        T-MISE(WK-IXM).
**** MOVE     HUR-F13   TO        T-SURYO(WK-IXM).
     IF       HUR-F25   =    OLD-PATNCD
              MOVE      HUR-F25   TO   TBL-PTNCD(PTN-IDX-CNT)
              MOVE      HUR-F02   TO   T-MISE(PTN-IDX-CNT WK-IXM)
              MOVE      HUR-F13   TO   T-SURYO(PTN-IDX-CNT WK-IXM)
              COMPUTE   TBL-PTNSURYO(PTN-IDX-CNT) =
                        TBL-PTNSURYO(PTN-IDX-CNT) + HUR-F13
     ELSE
              ADD       1         TO   PTN-IDX-CNT
              MOVE      1         TO   WK-IXM
              MOVE      HUR-F25   TO   TBL-PTNCD(PTN-IDX-CNT)
              MOVE      HUR-F02   TO   T-MISE(PTN-IDX-CNT WK-IXM)
              MOVE      HUR-F13   TO   T-SURYO(PTN-IDX-CNT WK-IXM)
              COMPUTE   TBL-PTNSURYO(PTN-IDX-CNT) =
                        TBL-PTNSURYO(PTN-IDX-CNT) + HUR-F13
              MOVE      HUR-F25   TO   OLD-PATNCD
     END-IF.
*↑2016/08/23
*
     COMPUTE  WSURYO    =    HUR-F13   +    WSURYO.
*
     MOVE     IDX-CNT   TO        WK-IXD.
     ADD      1         TO        IDX-CNT.
     ADD      1         TO        MISE-CNT.
*
     PERFORM  HUR-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*             商品コードチェック            2.1                *
****************************************************************
 S-CHECK-SEC            SECTION.
*
     MOVE     "S-CHECK-SEC"            TO        S-NAME.
     IF        END-FLG  =             "END"
               MOVE     SPACE          TO        HUR-F07
               MOVE     SPACE          TO        HUR-F08
     END-IF.
*
     COMPUTE   SYO-SU    =
                                  WSURYO    +         SYO-SU.
     COMPUTE   SYO-GENKIN  =
                                  WSURYO *  WGENTAN + SYO-GENKIN.
     COMPUTE   SYO-BAIKIN  =
                                  WSURYO *  WBAITAN + SYO-BAIKIN.
*
*****MOVE      OLD-SYOHINCD   TO        SYOHINCD.
     MOVE      WTANABAN       TO        TANABAN.
*
     IF  WNFSHOMS = "CHK"
         MOVE NC"【ナフコ商品Ｍ無！】"  TO  MASTERCHK1
     ELSE
         MOVE SPACE                     TO  MASTERCHK1
     END-IF.
*
     IF  WHSHOTBL = "CHK"
         MOVE NC"【商品変換ＴＭ無！】"  TO  MASTERCHK2
     ELSE
         MOVE SPACE                     TO  MASTERCHK2
     END-IF.
*
*↓2016/08/23
*    MOVE      WSYOHINNM          TO        SYOHINNM.
     MOVE      WSYOHINNM(1:15)    TO        SYOHINNM1.
*    MOVE      WKIKAKU            TO        KIKAKU.
     MOVE      WSYOHINNM(16:15)   TO        SYOHINNM2.
*↑2016/08/23
     MOVE      WJISHOCD           TO        SYOHINCD.
     MOVE      WAITECD            TO        MEI2-SHOCD.
*↓2016/08/23 カット
* ** 2012/08/16 START
*    IF        WK-KOURI-KBN   =     "1"
*              MOVE  NC"☆小売出荷☆"   TO   MEI2-KOURI-KBN
*    ELSE
*              MOVE  SPACE              TO   MEI2-KOURI-KBN
*    END-IF.
* ** 2012/08/16 END
*↑2016/08/23
     MOVE      WGENTAN        TO        GENTAN.
     MOVE      WBAITAN        TO        BAITAN.
*↓2016/08/23
*****MOVE      WPATNCD        TO        PATNCD.
*↑2016/08/23
     MOVE      WSURYO         TO        SURYOKEI.
*
*↓2016/08/23
*    MOVE   ZERO              TO        WK-IXM.
*↑2016/08/23
*
*↓2016/08/23
     PERFORM   VARYING   PTN-SET-CNT  FROM  1 BY 1
               UNTIL     PTN-SET-CNT  > PTN-IDX-CNT
*
               MOVE      ZERO         TO        WK-IXM
*
*↓TEST
*       IF (  WJISHOCD(1:8) =  "00018050" ) AND
*          (  NOUYMD        =  20160730 )
*             DISPLAY "SHOCD="       WJISHOCD(1:8) UPON CONS
*             DISPLAY "PTN-IDX-CNT=" PTN-IDX-CNT   UPON CONS
*             DISPLAY "PTN-SET-CNT=" PTN-SET-CNT   UPON CONS
*       END-IF
*↑TEST
*****2011/02/25 テーブル拡張 ST
*****PERFORM   UNTIL     WK-IXM  >  300   OR
*↓2016/08/23
*      PERFORM   UNTIL     WK-IXM  >  400   OR
       PERFORM   UNTIL     WK-IXM  >  500   OR
*↑2016/08/23
*****2011/02/25 テーブル拡張 ED
                         WK-IXM  >  WK-IXD
*↓2016/08/23
***                      OR  PTN-SET-CNT  >    PTN-IDX-CNT
*↑2016/08/23
*
               MOVE      ZERO             TO   WK-IX
*↓2016/08/23
***            MOVE      1                TO   PTN-SET-CNT
*↑2016/08/23
               PERFORM   MEISAI-SEC
                         UNTIL  WK-IX  >  10
*
*                 IF     L-CNT     >=        60
                  IF     L-CNT     >=        53
                         PERFORM   MIDASI-SEC
                  END-IF
*
                  IF     MEISAI1   NOT =     SPACE
                         WRITE  P-REC  FROM  MEISAI0  AFTER  1
                         WRITE  P-REC  FROM  MEISAI1  AFTER  1
                         WRITE  P-REC  FROM  MEISAI2  AFTER  1
*                        ADD    2      TO    L-CNT
                         ADD    3      TO    L-CNT
                  END-IF
*
        END-PERFORM
*↓2016/08/23
*↓TEST
*       IF (  WJISHOCD(1:8) =  "00738604" OR "00738614" ) AND
*          (  NOUYMD        =  20160727 )
*             DISPLAY "SHOCD="       WJISHOCD(1:8) UPON CONS
*             DISPLAY "PTN-SET-CNT=" PTN-SET-CNT   UPON CONS
*       END-IF
*       MOVE      NOUYMD                     TO  TEN-F01
*       MOVE      ","                        TO  TEN-K01
*       MOVE      WAITECD                    TO  TEN-F02
*       MOVE      ","                        TO  TEN-K02
*       MOVE      WJISHOCD(1:8)              TO  TEN-F03
*       MOVE      ","                        TO  TEN-K03
*       MOVE      OLD-SYOBUNCD               TO  TEN-F04
*       MOVE      ","                        TO  TEN-K04
*       MOVE      TBL-PTNCD(PTN-SET-CNT)     TO  TEN-F05
*       MOVE      ","                        TO  TEN-K05
*       MOVE      TBL-PTNSURYO(PTN-SET-CNT)  TO  TEN-F06
*       MOVE      ","                        TO  TEN-K06
*       PERFORM   VARYING TEST-IX FROM  1 BY 1
*        UNTIL TEST-IX > 300
*           MOVE T-MISE(PTN-SET-CNT TEST-IX)  TO TEN-F071(TEST-IX)
*           MOVE ","                          TO TEN-K071(TEST-IX)
*           MOVE T-SURYO(PTN-SET-CNT TEST-IX) TO TEN-F072(TEST-IX)
*           MOVE ","                          TO TEN-K072(TEST-IX)
*       END-PERFORM
*       WRITE     TEN-REC
*↑TEST
********ADD    1              TO       PTN-SET-CNT
     END-PERFORM.
*↑2016/08/23
*
*
     WRITE     P-REC  FROM  MEISAI3  AFTER  1.
     ADD       1              TO        L-CNT.
*
*↓2016/08/23
*    MOVE      ZERO           TO        TBL-MISE-AREA.
     INITIALIZE                         TBL-MISE-AREA.
*↑2016/08/23
     MOVE      ZERO           TO        WK-IXM.
*
     IF  PARA-OUTNO     =    2
         MOVE      HUR-F08    TO        OLD-SYOHINCD
     ELSE
         MOVE      HUR-F07    TO        OLD-SYOHINCD
     END-IF.
*ナフコ商品マスタ存在チェック
     MOVE     HUR-F07   TO        NFM-F01.
     PERFORM  NFSHOMS-READ-SEC.
     IF  NFSHOMS-INV-FLG = "INV"
         MOVE "CHK"     TO        WNFSHOMS
     ELSE
         MOVE SPACE     TO        WNFSHOMS
     END-IF.
*商品変換ＴＢＬ存在チェック
     MOVE     HUR-F013  TO        TBL-F01.
     MOVE     HUR-F07   TO        TBL-F02.
     PERFORM  HSHOTBL-READ-SEC.
     IF  HSHOTBL-INV-FLG = "INV"
         MOVE "CHK"     TO        WHSHOTBL
     ELSE
         MOVE SPACE     TO        WHSHOTBL
     END-IF.
     MOVE      HUR-F15        TO        WGENTAN.
     MOVE      HUR-F16        TO        WBAITAN.
     MOVE      HUR-F11        TO        WSYOHINNM.
     MOVE      HUR-F12        TO        WKIKAKU.
     MOVE      HUR-F07        TO        WAITECD.
     MOVE      HUR-F08        TO        WJISHOCD.
     MOVE      HUR-F22        TO        WK-KOURI-KBN.
*↓2016/08/23
     MOVE      HUR-F25        TO        WPATNCD.
*↑2016/08/23
     MOVE      HUR-F09        TO        WTANABAN.
     MOVE      ZERO           TO        WSURYO.
*↓2016/08/23
     MOVE      ZERO           TO        WSURYO.
*
     MOVE      1              TO        IDX-CNT.
     PERFORM   CLR-SEC   UNTIL          IDX-CNT   >   10.
*
     MOVE      1              TO        IDX-CNT.
     MOVE      1              TO        MISE-CNT.
*↓2016/08/23
     MOVE      1              TO        PTN-IDX-CNT.
*↑2016/08/23
*
 S-CHECK-EXIT.
     EXIT.
*
****************************************************************
*             明細行出力処理                2.1.1              *
****************************************************************
 MEISAI-SEC             SECTION.
*
     MOVE    "MEISAI-SEC"    TO   S-NAME.
     ADD      1         TO   WK-IX.
     IF       WK-IX     >    10
              GO   TO   MEISAI-EXIT
     END-IF.
*
     ADD      1         TO   WK-IXM.
*****2011/02/25 テーブル拡張 ST
*****IF       WK-IXM    >    300
*↓2016/08/23
*    IF       WK-IXM    >    400
     IF       WK-IXM    >    500
*↑2016/08/23
*****2011/02/25 テーブル拡張 ED
              GO   TO   MEISAI-EXIT
     END-IF.
*
*↓2016/08/23
     MOVE     TBL-PTNCD(PTN-SET-CNT)    TO    PATNCD.
     MOVE     "("                       TO    KAKKO1.
     MOVE     ") "                      TO    KAKKO2.
*↑2016/08/23
*
     IF       WK-IXM      >   10
              MOVE   SPACE              TO    SYOHINCD
*↓2016/08/23
*-                                            SYOHINNM
                                              SYOHINNM1
                                              SYOHINNM2
*-                                            KIKAKU
*↑2016/08/23
                                              MEI2-SHOCD
*↓2016/08/23
*-                                            MEI2-KOURI-KBN
*↑2016/08/23
                                              TANABAN
                                              MASTERCHK1
                                              MASTERCHK2
              MOVE   SPACE          TO        GENTANR
              MOVE   ZERO           TO        BAITAN
              MOVE   ZERO           TO        SURYOKEI
*↓2016/08/23
              MOVE   SPACE          TO        PATNCD
              MOVE   SPACE          TO        KAKKO1 KAKKO2
              MOVE   ZERO           TO        PATNGK
*↑2016/08/23
     END-IF.
*↓2016/08/23
     IF     ( PTN-SET-CNT  >  1 )
              MOVE   SPACE              TO    SYOHINCD
                                              SYOHINNM1
                                              SYOHINNM2
                                              MEI2-SHOCD
                                              TANABAN
                                              MASTERCHK1
                                              MASTERCHK2
              MOVE   SPACE          TO        GENTANR
              MOVE   ZERO           TO        BAITAN
              MOVE   ZERO           TO        SURYOKEI
     END-IF.
     IF     ( WK-IXM     >   10 ) AND ( PTN-SET-CNT  >  1 )
              MOVE   SPACE              TO    SYOHINCD
                                              SYOHINNM1
                                              SYOHINNM2
                                              MEI2-SHOCD
                                              TANABAN
                                              MASTERCHK1
                                              MASTERCHK2
              MOVE   SPACE          TO        GENTANR
              MOVE   ZERO           TO        BAITAN
              MOVE   ZERO           TO        SURYOKEI
              MOVE   SPACE          TO        PATNCD
              MOVE   SPACE          TO        KAKKO1 KAKKO2
              MOVE   ZERO           TO        PATNGK
     END-IF.
*↑2016/08/23
*
*↓2016/08/23
*    IF       T-MISE(WK-IXM)      =    ZERO
     IF       T-MISE(PTN-SET-CNT WK-IXM)      =    ZERO
*↑2016/08/23
              MOVE      SPACE          TO   MISEBAN(WK-IX)
     ELSE
*↓2016/08/23
*             MOVE      T-MISE(WK-IXM)      TO   MISE
              MOVE      T-MISE(PTN-SET-CNT WK-IXM)      TO   MISE
*↑2016/08/23
              MOVE      WK-MISE             TO   MISEBAN(WK-IX)
     END-IF.
*↓2016/08/23
*    MOVE     T-SURYO(WK-IXM)     TO   SURYO(WK-IX).
     MOVE     T-SURYO(PTN-SET-CNT WK-IXM)     TO   SURYO(WK-IX).
     IF       WK-IXM   <   11
              MOVE     TBL-PTNSURYO(PTN-SET-CNT)   TO   PATNGK
     END-IF.
*↑2016/08/23
*
 MEISAI-EXIT.
     EXIT.
*
***************************************************************
*             テーブルクリア処理            2.2               *
***************************************************************
 CLR-SEC                SECTION.
*
     MOVE    "CLR-SEC"  TO        S-NAME.
     MOVE     SPACE     TO        MISEBAN(IDX-CNT).
     MOVE     ZERO      TO        SURYO(IDX-CNT).
     ADD      1         TO        IDX-CNT.
*
 CLR-EXIT.
     EXIT.
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*////IF       WSURYO    NOT =     ZERO
              PERFORM   S-CHECK-SEC.
              PERFORM   SYOKEI-SEC.
              PERFORM   CYUKEI-SEC.
              PERFORM   GOUKEI-SEC.
              PERFORM   SAKKEI-SEC.
              PERFORM   SOUKEI-SEC.
*////END-IF.
*
     MOVE      CNT-READ  TO      IN-CNT.
     MOVE      P-CNT     TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE    HURXXXW   PRINTF  NFSHOMS  HSHOTBL.
*↓2016/08/23
     CLOSE    NFSHBPL1.
*↑2016/08/23
*
*↓TEST
*    CLOSE    WKTENPO.
*↑TEST
*
 END-EXIT.
     EXIT.
*
****************************************************************
*             小計行出力処理                3.1                *
****************************************************************
 SYOKEI-SEC             SECTION.
*
     MOVE    "SYOKEI-SEC"    TO        S-NAME.
     MOVE     SYO-MI         TO        GKMIDASI.
     MOVE     SYO-SU         TO        SUKEI
     MOVE     SYO-GENKIN     TO        GENKEI.
     MOVE     SYO-BAIKIN     TO        BAIKEI.
*
     IF       SYO-SU      =  ZERO
              MOVE           ZERO      TO        GENHEI
                                                 BAIHEI
     ELSE
              COMPUTE        WGENHEI   =     SYO-GENKIN / SYO-SU
              MOVE           WGENHEI   TO        GENHEI
              COMPUTE        WBAIHEI   =     SYO-BAIKIN / SYO-SU
              MOVE           WBAIHEI   TO        BAIHEI
     END-IF.
*
*    IF       L-CNT       >= 60
     IF       L-CNT       >= 53
              PERFORM        MIDASI-SEC
     END-IF.
*
*    COMPUTE  W-L-CNT     =  61             -    L-CNT.
     COMPUTE  W-L-CNT     =  54             -    L-CNT.
     WRITE    P-REC          FROM      GOUKEI    AFTER    W-L-CNT.
*
     COMPUTE  CYU-SU      =  SYO-SU         +    CYU-SU.
     COMPUTE  CYU-GENKIN  =  SYO-GENKIN     +    CYU-GENKIN.
     COMPUTE  CYU-BAIKIN  =  SYO-BAIKIN     +    CYU-BAIKIN.
*
     MOVE     ZERO           TO        SYO-SU
                                       SYO-GENKIN
                                       SYO-BAIKIN
                                       WGENHEI
                                       WBAIHEI.
*
 SYOKEI-EXIT.
     EXIT.
*
****************************************************************
*             中計行出力処理                3.2                *
****************************************************************
 CYUKEI-SEC             SECTION.
*
     MOVE    "CYUKEI-SEC"    TO        S-NAME.
     MOVE     CYU-MI         TO        GKMIDASI.
     MOVE     CYU-SU         TO        SUKEI
     MOVE     CYU-GENKIN     TO        GENKEI.
     MOVE     CYU-BAIKIN     TO        BAIKEI.
*
     IF       CYU-SU      =  ZERO
              MOVE           ZERO      TO        GENHEI
                                                 BAIHEI
     ELSE
              COMPUTE        WGENHEI   =     CYU-GENKIN / CYU-SU
              MOVE           WGENHEI   TO        GENHEI
              COMPUTE        WBAIHEI   =     CYU-BAIKIN / CYU-SU
              MOVE           WBAIHEI   TO        BAIHEI
     END-IF.
*
     WRITE    P-REC          FROM      GOUKEI    AFTER     1.
*
     COMPUTE  GOU-SU      =  CYU-SU         +    GOU-SU.
     COMPUTE  GOU-GENKIN  =  CYU-GENKIN     +    GOU-GENKIN.
     COMPUTE  GOU-BAIKIN  =  CYU-BAIKIN     +    GOU-BAIKIN.
*
     MOVE     ZERO           TO        CYU-SU
                                       CYU-GENKIN
                                       CYU-BAIKIN
                                       WGENHEI
                                       WBAIHEI.
*
 CYUKEI-EXIT.
     EXIT.
*
****************************************************************
*             合計行出力処理                3.2                *
****************************************************************
 GOUKEI-SEC             SECTION.
*
     MOVE    "GOUKEI-SEC"    TO        S-NAME.
     MOVE     GOU-MI         TO        GKMIDASI.
     MOVE     GOU-SU         TO        SUKEI
     MOVE     GOU-GENKIN     TO        GENKEI.
     MOVE     GOU-BAIKIN     TO        BAIKEI.
*
     IF       GOU-SU      =  ZERO
              MOVE           ZERO      TO        GENHEI
                                                 BAIHEI
     ELSE
              COMPUTE        WGENHEI   =     GOU-GENKIN / GOU-SU
              MOVE           WGENHEI   TO        GENHEI
              COMPUTE        WBAIHEI   =     GOU-BAIKIN / GOU-SU
              MOVE           WBAIHEI   TO        BAIHEI
     END-IF.
*
     WRITE    P-REC          FROM      GOUKEI    AFTER     1.
*
*    COMPUTE  SOU-SU      =  GOU-SU         +    SOU-SU.
*    COMPUTE  SOU-GENKIN  =  GOU-GENKIN     +    SOU-GENKIN.
*    COMPUTE  SOU-BAIKIN  =  GOU-BAIKIN     +    SOU-BAIKIN.
     COMPUTE  SAK-SU      =  GOU-SU         +    SAK-SU.
     COMPUTE  SAK-GENKIN  =  GOU-GENKIN     +    SAK-GENKIN.
     COMPUTE  SAK-BAIKIN  =  GOU-BAIKIN     +    SAK-BAIKIN.
*
     MOVE     ZERO           TO        GOU-SU
                                       GOU-GENKIN
                                       GOU-BAIKIN
                                       WGENHEI
                                       WBAIHEI.
*
 GOUKEI-EXIT.
     EXIT.
*
****************************************************************
*           作場計行出力処理                3.2                *
****************************************************************
 SAKKEI-SEC             SECTION.
*
     MOVE    "SAKKEI-SEC"    TO        S-NAME.
     MOVE     SAK-MI         TO        GKMIDASI.
     MOVE     SAK-SU         TO        SUKEI
     MOVE     SAK-GENKIN     TO        GENKEI.
     MOVE     SAK-BAIKIN     TO        BAIKEI.
*
     IF       SAK-SU      =  ZERO
              MOVE           ZERO      TO        GENHEI
                                                 BAIHEI
     ELSE
              COMPUTE        WGENHEI   =     SAK-GENKIN / SAK-SU
              MOVE           WGENHEI   TO        GENHEI
              COMPUTE        WBAIHEI   =     SAK-BAIKIN / SAK-SU
              MOVE           WBAIHEI   TO        BAIHEI
     END-IF.
*
     WRITE    P-REC          FROM      GOUKEI    AFTER     1.
*
     COMPUTE  SOU-SU      =  SAK-SU         +    SOU-SU.
     COMPUTE  SOU-GENKIN  =  SAK-GENKIN     +    SOU-GENKIN.
     COMPUTE  SOU-BAIKIN  =  SAK-BAIKIN     +    SOU-BAIKIN.
*
     MOVE     ZERO           TO        SAK-SU
                                       SAK-GENKIN
                                       SAK-BAIKIN
                                       WGENHEI
                                       WBAIHEI.
*
 SAKKEI-EXIT.
     EXIT.
*
****************************************************************
*             総計行出力処理                3.3                *
****************************************************************
 SOUKEI-SEC             SECTION.
*
     MOVE    "SOUKEI-SEC"    TO        S-NAME.
     MOVE     SOU-MI         TO        GKMIDASI.
     MOVE     SOU-SU         TO        SUKEI
     MOVE     SOU-GENKIN     TO        GENKEI.
     MOVE     SOU-BAIKIN     TO        BAIKEI.
*
     IF       SOU-SU      =  ZERO
              MOVE           ZERO      TO        GENHEI
                                                 BAIHEI
     ELSE
              COMPUTE        WGENHEI   =     SOU-GENKIN / SOU-SU
              MOVE           WGENHEI   TO        GENHEI
              COMPUTE        WBAIHEI   =     SOU-BAIKIN / SOU-SU
              MOVE           WBAIHEI   TO        BAIHEI
     END-IF.
*
     WRITE    P-REC          FROM      GOUKEI    AFTER     1.
*
 SOUKEI-EXIT.
     EXIT.
****************************************************************
*             ナフコ商品マスタ読み込み      ALL                *
****************************************************************
 NFSHOMS-READ-SEC       SECTION.
*
     MOVE    "NFSHOMS-READ-SEC"  TO    S-NAME.
*
     READ  NFSHOMS
           INVALID      MOVE "INV"     TO   NFSHOMS-INV-FLG
           NOT  INVALID MOVE SPACE     TO   NFSHOMS-INV-FLG
     END-READ.
*
 NFSHOMS-READ-EXIT.
     EXIT.
****************************************************************
*             商品変換ＴＢＬ読み込み　      ALL                *
****************************************************************
 HSHOTBL-READ-SEC       SECTION.
*
     MOVE    "HSHOTBL-READ-SEC"  TO    S-NAME.
*
     READ  HSHOTBL
           INVALID      MOVE "INV"     TO   HSHOTBL-INV-FLG
           NOT  INVALID MOVE SPACE     TO   HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
*
********************<<  PUROGRAM  END  >>*************************

```
