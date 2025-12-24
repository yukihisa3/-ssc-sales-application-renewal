# SDE0140L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SDE0140L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　在庫ＥＸＣＥＬ                    *
*    モジュール名　　　　：　伝票変換発注集計表                *
*    作成日／更新日　　　：　2016/09/27                        *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　発注集計表データファイルより、    *
*                            発注集計表を作表する。            *
*    作成日／更新日　　　：　2017/02/09                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　１頁の行数を６０→５０に変更　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SDE0140L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          16/09/37.
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
*****<<発注集計表出力ワーク>>***********************************
     SELECT   HACXXXF   ASSIGN    TO        HACXXXF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HAC-STATUS.
*****<<取引先マスタ>>*****************************************
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE  STATUS        IS   TOK-STATUS.
*                                                                *
*****<<担当者マスタ>>*****************************************
     SELECT   HTANMS    ASSIGN    TO        DA-01-VI-TANMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TAN-F01
                                                 TAN-F02
                        FILE  STATUS        IS   TAN-STATUS.
*                                                                *
*****<<倉庫マスタ>>*******************************************
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE  STATUS        IS   SOK-STATUS.
*
*****<<  プリント　Ｆ   >>**************************************
     SELECT   PRINTF    ASSIGN    TO        LP-04-PRTF
                        FILE  STATUS        IS   PRT-STATUS.
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*--------------------------------------------------------------*
*    FILE = 集計表データファイル  RL=256    BF=15     ORG=SF   *
*--------------------------------------------------------------*
 FD  HACXXXF            BLOCK     CONTAINS  15   RECORDS.
     COPY     HACXXXF   OF        XFDLIB
              JOINING   HAC  AS   PREFIX.
*--------------------------------------------------------------*
*    FILE = 取引先マスタ                                       *
*--------------------------------------------------------------*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 担当者マスタ                                       *
*--------------------------------------------------------------*
 FD  HTANMS             LABEL RECORD   IS   STANDARD.
     COPY     HTANMS    OF        XFDLIB
              JOINING   TAN       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 倉庫マスタ                                         *
*--------------------------------------------------------------*
 FD  ZSOKMS             LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = プリントファイル                                   *
*--------------------------------------------------------------*
 FD  PRINTF.
 01  P-REC                        PIC       X(200).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  HAC-STATUS                   PIC       X(02).
 01  TOK-STATUS                   PIC       X(02).
 01  TAN-STATUS                   PIC       X(02).
 01  SOK-STATUS                   PIC       X(02).
 01  PRT-STATUS                   PIC       X(02).
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
         05  HAC-YY               PIC       9(04).
         05  HAC-MM               PIC       9(02).
         05  HAC-DD               PIC       9(02).
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
 01  HTANMS-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  HTOKMS-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  ZSOKMS-INV-FLG               PIC       X(03)  VALUE  SPACE.
*
***** ラインカウンタ計算領域
 01  W-L-CNT                      PIC       9(03)  VALUE  ZERO.
*
***** 店インデックス
 01  IDX-CNT                      PIC       9(03)  VALUE  ZERO.
*
***** 店テーブルワーク
 01  WK-MISE.
     03  FILLER                   PIC       X(01)  VALUE  "(".
     03  MISE                     PIC       9(05)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  ")".
***** テーブルワーク
 01  TBL-MISE-AREA.
     03  TBL-MISE                 OCCURS    500.
         05  T-MISE               PIC       9(05).
         05  T-SURYO              PIC       9(05).
*
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
     03  NEW-SOKCD                PIC       X(02)  VALUE  SPACE.
     03  NEW-TOKCD                PIC       9(08)  VALUE  ZERO.
     03  NEW-SYOHINCD             PIC       X(16)  VALUE  SPACE.
*
 01  WK-OLD-KEY.
     03  OLD-NOHINBI              PIC       9(06)  VALUE  ZERO.
     03  OLD-BUMONCD              PIC       X(04)  VALUE  SPACE.
     03  OLD-SOKCD                PIC       X(02)  VALUE  SPACE.
     03  OLD-TOKCD                PIC       9(08)  VALUE  ZERO.
     03  OLD-SYOHINCD             PIC       X(16)  VALUE  SPACE.
*
***** 部門ＣＤ保存ワーク
 01  WBUMON                       PIC       X(04)  VALUE  SPACE.
***** 原価単価保存ワーク
 01  WGENTAN                      PIC       9(07)V99 VALUE ZERO.
***** 売価単価保存ワーク
 01  WBAITAN                      PIC       9(07)  VALUE  ZERO.
***** 自社商品コード保存ワーク
 01  WJISHOCD                     PIC       X(16)  VALUE  SPACE.
***** 商品名保存ワーク
 01  WSYOHINNM                    PIC       X(30)  VALUE  SPACE.
***** 規格保存ワーク
 01  WKIKAKU                      PIC       X(15)  VALUE  SPACE.
***** 相手商品保存ワーク
 01  WAITECD                      PIC       X(13)  VALUE  SPACE.
***** 企業名保存ワーク
 01  WKIGYOCD                     PIC       9(08)  VALUE  ZERO.
 01  WKIGYONM                     PIC       N(15)  VALUE  SPACE.
***** 倉庫保存ワーク
 01  WSOKCD                       PIC       X(02)  VALUE  ZERO.
 01  WSOKNM                       PIC       N(18)  VALUE  SPACE.
***** 変換情報ワーク
 01  WK-HENKAN-TANTOU.
     03  WK-HEN-TANCD-R.
         05  WK-HEN-BUMON         PIC       X(04)  VALUE  SPACE.
         05  WK-HEN-KU1           PIC       X(01)  VALUE  "-".
         05  WK-HEN-TANCD         PIC       X(02)  VALUE  SPACE.
     03  WK-HEN-TANNM             PIC       N(05)  VALUE  SPACE.
     03  WK-HEN-DATE.
         05  WK-HEN-YY            PIC       9(04)  VALUE  ZERO.
         05  WK-HEN-MM            PIC       9(02)  VALUE  ZERO.
         05  WK-HEN-DD            PIC       9(02)  VALUE  ZERO.
     03  WK-HEN-TIME.
         05  WK-HEN-HH            PIC       9(02)  VALUE  ZERO.
         05  WK-HEN-KU2           PIC       X(01)  VALUE  ZERO.
         05  WK-HEN-HM            PIC       9(02)  VALUE  ZERO.
*
***** 合計計算ワーク
 01  WSURYO                       PIC       9(06)  VALUE  ZERO.
 01  WGENHEI                      PIC       9(07)  VALUE  ZERO.
 01  WBAIHEI                      PIC       9(07)  VALUE  ZERO.
*
***** 合計見出し領域
 01  G-MIDASI      CHARACTER      TYPE      IS     YA.
     03  SYO-MI                   PIC       N(05)  VALUE
       NC"【小　計】".
     03  GOU-MI                   PIC       N(05)  VALUE
       NC"【合　計】".
     03  SOU-MI                   PIC       N(05)  VALUE
       NC"【総合計】".
     03  CYU-MI                   PIC       N(05)  VALUE
       NC"【中　計】".
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
         05  ST-PG          PIC   X(08)  VALUE "SDE0140L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SDE0140L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SDE0140L".
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
     03  FILLER                   PIC  X(08)  VALUE "SDE0140L".
     03  FILLER                   PIC  X(40)  VALUE  SPACE.
     03  FILLER                   PIC  N(16)  VALUE
       NC"＜（伝票ＥＸＣＥＬ）発注集計表＞"
                   CHARACTER      TYPE IS     YB-21.
     03  FILLER                   PIC  X(18)  VALUE  SPACE.
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
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  N(06)  VALUE
       NC"倉庫ＣＤ　："
                   CHARACTER      TYPE IS     YA.
     03  HD2-SOKCD                PIC  X(02).
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  HD2-SOKNM                PIC  N(10)
                   CHARACTER      TYPE IS     YA.
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  N(09)  VALUE
       NC"（伝票変換担当者："
                   CHARACTER      TYPE IS     YA.
     03  HD2-BUTANCD              PIC  X(07).
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  HD2-BUTANNM              PIC  N(05)
                   CHARACTER      TYPE IS     YA.
     03  FILLER                   PIC  X(02)  VALUE  SPACE.
     03  FILLER                   PIC  N(05)  VALUE
       NC"変換日時："
                   CHARACTER      TYPE IS     YA.
     03  HD2-HEN-YY               PIC  9(04).
     03  FILLER                   PIC  N(01)  VALUE  NC"年"
                   CHARACTER      TYPE IS     YA.
     03  HD2-HEN-MM               PIC  Z9.
     03  FILLER                   PIC  N(01)  VALUE  NC"月"
                   CHARACTER      TYPE IS     YA.
     03  HD2-HEN-DD               PIC  Z9.
     03  FILLER                   PIC  N(01)  VALUE  NC"日"
                   CHARACTER      TYPE IS     YA.
     03  FILLER                   PIC  X(02)  VALUE  SPACE.
     03  HD2-HEN-HHMM             PIC  X(05).
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）"
                   CHARACTER      TYPE IS     YA.
*
***** 見出し行３
 01  MIDASHI3.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(06)  VALUE
       NC"発注企業名："
                   CHARACTER      TYPE      IS     YA.
     03  HD3-TOKCD                PIC       9(08).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  HD3-TOKNM                PIC       N(15)
                   CHARACTER      TYPE      IS     YA.
     03  FILLER                   PIC       X(55)  VALUE  SPACE.
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
     03  FILLER                   PIC       X(90)  VALUE  SPACE.
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
***** 見出し行５
 01  MIDASHI5      CHARACTER      TYPE      IS     YA.
*****03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(07)  VALUE
       NC"自社商品コード".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(05)  VALUE
       NC"商　品　名".
     03  FILLER                   PIC       X(22)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
       NC"原単価".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
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
***** 見出し行６
 01  MIDASHI6      CHARACTER      TYPE      IS     YA.
*****03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(07)  VALUE
       NC"相手商品コード".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
       NC"規　格".
     03  FILLER                   PIC       X(26)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
       NC"売単価".
     03  FILLER                   PIC       X(12)  VALUE  SPACE.
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
***** 明細行３
 01  MIDASHI7.
     03  MID-TEN                  PIC       N(68)  VALUE  SPACE.
*
***** 明細行１
 01  MEISAI1.
*****03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  SYOHINCD                 PIC       X(16).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  SYOHINNM                 PIC       X(30).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  GENTAN                   PIC       ZZZZZZ9.99.
     03  GENTANR   REDEFINES      GENTAN    PIC    X(10).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  SURYOKEI                 PIC       ZZZZZZ.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MISE-TBL                 OCCURS    10.
         05  MISEBAN              PIC       X(07)  VALUE  SPACE.
*
***** 明細行２
 01  MEISAI2.
*****03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MEI2-SHOCD               PIC       X(13).
     03  FILLER                   PIC       X(04)  VALUE  SPACE.
     03  KIKAKU                   PIC       X(15).
     03  FILLER                   PIC       X(16)  VALUE  SPACE.
     03  BAITAN                   PIC       ZZZZZZZ.
     03  FILLER                   PIC       X(10)  VALUE  SPACE.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  SURYO-TBL                OCCURS    10.
         05  SURYO                PIC       ZZZZZ  VALUE  ZERO.
         05  FILLER               PIC       X(02)  VALUE  SPACE.
*
***** 明細行３
 01  MEISAI3.
     03  PRT-TEN                  OCCURS    136.
         05  FILLER               PIC       X(01)  VALUE  "-".
*
***** 明細行４
 01  MEISAI4.
     03  PRT-SEN                  OCCURS    136.
         05  FILLER               PIC       X(01)  VALUE  "=".
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
 01  PARA-BUMON             PIC   X(04).
 01  PARA-TANCD             PIC   X(02).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-BUMON
                                           PARA-TANCD.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE HACXXXF.
     MOVE     "HACXXXF "          TO        ERR-FL-ID.
     MOVE     HAC-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE HTOKMS.
     MOVE     "TOKMS2  "          TO        ERR-FL-ID.
     MOVE     TOK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE HTANMS.
     MOVE     "TANMS1  "          TO        ERR-FL-ID.
     MOVE     TAN-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC4         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE ZSOKMS.
     MOVE     "ZSOKMS1 "          TO        ERR-FL-ID.
     MOVE     SOK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC5         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE PRINTF.
     MOVE     "PRINTF  "          TO        ERR-FL-ID.
     MOVE     PRT-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SDE0140L-START          SECTION.
*
     MOVE     "SDE0140L-START"     TO   S-NAME.
     PERFORM            INIT-SEC.
*
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SDE0140L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     HACXXXF  HTOKMS  HTANMS  ZSOKMS.
     OPEN     OUTPUT    PRINTF.
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
     PERFORM  HAC-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃＃出力対象ＤＴ無し！！＃＃" UPON CONS
              GO        TO        INIT-EXIT
     END-IF.
*
     MOVE     1         TO        IDX-CNT.
     PERFORM  CLR-SEC   UNTIL     IDX-CNT   >   10.
*
     MOVE     1         TO        IDX-CNT   MISE-CNT.
     MOVE     ZERO      TO        WK-IXM.
     MOVE     ZERO      TO        TBL-MISE-AREA.
*ブレイクキー情報
     MOVE     HAC-F04   TO        OLD-BUMONCD.
     MOVE     HAC-F013  TO        OLD-TOKCD.
     MOVE     HAC-F03   TO        OLD-SOKCD.
     MOVE     HAC-F06   TO        OLD-NOHINBI.
     MOVE     HAC-F03   TO        OLD-SOKCD.
     MOVE     HAC-F07   TO        OLD-SYOHINCD.
*ヘッダ情報
     MOVE     HAC-F05   TO        HACYMD.
     MOVE     HAC-F06   TO        NOUYMD.
     MOVE     HAC-F013  TO        TOK-F01  WKIGYOCD.
     PERFORM  HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG =  SPACE
              MOVE TOK-F03  TO    WKIGYONM
     ELSE
              MOVE SPACE    TO    WKIGYONM
     END-IF.
     MOVE     HAC-F03   TO        WSOKCD  SOK-F01.
     PERFORM  ZSOKMS-READ-SEC.
     IF  ZSOKMS-INV-FLG =  SPACE
              MOVE SOK-F02  TO    WSOKNM
     ELSE
              MOVE SPACE    TO    WSOKNM
     END-IF.
     MOVE     PARA-BUMON    TO    WK-HEN-BUMON  TAN-F01.
     MOVE     PARA-TANCD    TO    WK-HEN-TANCD  TAN-F02.
     MOVE     "-"           TO    WK-HEN-KU1.
     PERFORM  HTANMS-READ-SEC.
     IF  HTANMS-INV-FLG =  SPACE
              MOVE TAN-F03  TO    WK-HEN-TANNM
     ELSE
              MOVE SPACE    TO    WK-HEN-TANNM
     END-IF.
     MOVE     HAC-F01       TO   WK-HEN-DATE.
     MOVE     HAC-F012(1:2) TO    WK-HEN-HH.
     MOVE     HAC-F012(3:2) TO    WK-HEN-HM.
     MOVE     ":"           TO    WK-HEN-KU2.
     MOVE     HAC-F04       TO    WBUMON.
*明細情報
     MOVE     HAC-F15       TO    WGENTAN.
     MOVE     HAC-F16       TO    WBAITAN.
     MOVE     HAC-F11       TO    WSYOHINNM.
     MOVE     HAC-F12       TO    WKIKAKU.
     MOVE     HAC-F07       TO    WAITECD.
     MOVE     HAC-F08       TO    WJISHOCD.
*
     PERFORM  MIDASI-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*             集計表データＲＥＡＤ処理      1.1                *
****************************************************************
 HAC-RD-SEC             SECTION.
*
     MOVE    "HAC-RD-SEC"    TO   S-NAME.
     MOVE     NEW-NOHINBI    TO   OLD-NOHINBI.
     MOVE     NEW-BUMONCD    TO   OLD-BUMONCD.
     MOVE     NEW-SYOHINCD   TO   OLD-SYOHINCD.
     MOVE     NEW-TOKCD      TO   OLD-TOKCD.
     MOVE     NEW-SOKCD      TO   OLD-SOKCD.

     READ     HACXXXF
          AT END
              MOVE     "END"      TO        END-FLG
              MOVE     HIGH-VALUE TO        WK-NEW-KEY
          NOT AT END
              MOVE     HAC-F06    TO        NEW-NOHINBI
              MOVE     HAC-F07    TO        NEW-SYOHINCD
              MOVE     HAC-F04    TO        NEW-BUMONCD
              MOVE     HAC-F013   TO        NEW-TOKCD
              MOVE     HAC-F03    TO        NEW-SOKCD
              ADD      1          TO        CNT-READ
     END-READ.
*
 HAC-RD-EXIT.
     EXIT.
*
****************************************************************
*             見出し出力処理                1.2                *
****************************************************************
 MIDASI-SEC             SECTION.
*
     MOVE    "MIDASI-SEC"              TO    S-NAME.
*
     MOVE     SYS-YY         TO   HD1-YY.
     MOVE     SYS-MM         TO   HD1-MM.
     MOVE     SYS-DD         TO   HD1-DD.
     ADD      1              TO   P-CNT.
     MOVE     P-CNT          TO   HD1-PAGE.
*
     MOVE     HAC-YY         TO   H-YY.
     MOVE     HAC-MM         TO   H-MM.
     MOVE     HAC-DD         TO   H-DD.
     MOVE     WSOKCD         TO   HD2-SOKCD.
     MOVE     WSOKNM         TO   HD2-SOKNM.
     MOVE     WBUMON         TO   BUMONCD.
     MOVE     WKIGYOCD       TO   HD3-TOKCD.
     MOVE     WKIGYONM       TO   HD3-TOKNM.
     MOVE     NOU-YY         TO   N-YY.
     MOVE     NOU-MM         TO   N-MM.
     MOVE     NOU-DD         TO   N-DD.
     MOVE     WK-HEN-TANCD-R TO   HD2-BUTANCD.
     MOVE     WK-HEN-TANNM   TO   HD2-BUTANNM.
     MOVE     WK-HEN-YY      TO   HD2-HEN-YY.
     MOVE     WK-HEN-MM      TO   HD2-HEN-MM.
     MOVE     WK-HEN-DD      TO   HD2-HEN-DD.
     MOVE     WK-HEN-TIME    TO   HD2-HEN-HHMM.
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
     WRITE    P-REC     FROM      MEISAI4   AFTER     1.
     WRITE    P-REC     FROM      MIDASHI5  AFTER     1.
     WRITE    P-REC     FROM      MIDASHI6  AFTER     1.
     WRITE    P-REC     FROM      MEISAI4   AFTER     1.
*
     MOVE     9         TO        L-CNT.
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
*    総合計チェック
     IF       HAC-F013   NOT =     OLD-TOKCD
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              PERFORM   CYUKEI-SEC
              PERFORM   GOUKEI-SEC
              PERFORM   SOUKEI-SEC
              MOVE      HAC-F04    TO        OLD-BUMONCD
              MOVE      HAC-F20    TO        OLD-TOKCD
              MOVE      HAC-F06    TO        OLD-NOHINBI NOUYMD
              MOVE      HAC-F03    TO        OLD-SOKCD
              PERFORM   MIDASI-SEC
     END-IF.
*
*    合計チェック
     IF       HAC-F04    NOT =     OLD-BUMONCD
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              PERFORM   CYUKEI-SEC
              PERFORM   GOUKEI-SEC
              MOVE      HAC-F04    TO        OLD-BUMONCD
              MOVE      HAC-F20    TO        OLD-TOKCD
              MOVE      HAC-F06    TO        OLD-NOHINBI NOUYMD
              MOVE      HAC-F03    TO        OLD-SOKCD
              PERFORM   MIDASI-SEC
     END-IF.
*
*    中計チェック
     IF       HAC-F03    NOT =     OLD-SOKCD
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              PERFORM   CYUKEI-SEC
              MOVE      HAC-F04    TO        OLD-BUMONCD
              MOVE      HAC-F20    TO        OLD-TOKCD
              MOVE      HAC-F06    TO        OLD-NOHINBI NOUYMD
              MOVE      HAC-F03    TO        OLD-SOKCD
              PERFORM   MIDASI-SEC
     END-IF.
*
*    小計チェック
     IF       HAC-F06    NOT =     NOUYMD
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              MOVE      HAC-F04    TO        OLD-BUMONCD
              MOVE      HAC-F20    TO        OLD-TOKCD
              MOVE      HAC-F06    TO        OLD-NOHINBI NOUYMD
              MOVE      HAC-F03    TO        OLD-SOKCD
              PERFORM   MIDASI-SEC
     END-IF.
*
*    商品チェック
     IF   HAC-F07    NOT =     OLD-SYOHINCD(1:13)
          PERFORM   S-CHECK-SEC
          MOVE      HAC-F07    TO        OLD-SYOHINCD
     END-IF.
*
*    明細行作成
     ADD      1         TO        WK-IXM.
*
     MOVE     HAC-F02   TO        T-MISE(WK-IXM).
*    発注数量
     MOVE     HAC-F13   TO        T-SURYO(WK-IXM).
     COMPUTE  WSURYO    =         HAC-F13   +    WSURYO.
*
     MOVE     IDX-CNT   TO        WK-IXD.
     ADD      1         TO        IDX-CNT.
     ADD      1         TO        MISE-CNT.
*
     PERFORM  HAC-RD-SEC.
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
               MOVE     SPACE          TO        HAC-F07
               MOVE     SPACE          TO        HAC-F08
     END-IF.
*
     COMPUTE   SYO-SU    =
                                  WSURYO    +         SYO-SU.
     COMPUTE   SYO-GENKIN  =
                                  WSURYO *  WGENTAN + SYO-GENKIN.
     COMPUTE   SYO-BAIKIN  =
                                  WSURYO *  WBAITAN + SYO-BAIKIN.
*
     MOVE      WSYOHINNM      TO        SYOHINNM.
     MOVE      WKIKAKU        TO        KIKAKU.
     MOVE      WJISHOCD       TO        SYOHINCD.
     MOVE      WAITECD        TO        MEI2-SHOCD.
     MOVE      WGENTAN        TO        GENTAN.
     MOVE      WBAITAN        TO        BAITAN.
     MOVE      WSURYO         TO        SURYOKEI.
*
     MOVE   ZERO              TO        WK-IXM.
*
     PERFORM   UNTIL     WK-IXM  >  500   OR
                         WK-IXM  >  WK-IXD
*
               MOVE      ZERO             TO   WK-IX
               PERFORM   MEISAI-SEC
                         UNTIL  WK-IX  >  10
*
*2017/02/09 NAV ST
******************IF     L-CNT     >=        60
                  IF     L-CNT     >=        50
*2017/02/09 NAV ED
                         PERFORM   MIDASI-SEC
                  END-IF
*
                  IF     MEISAI1   NOT =     SPACE
                         WRITE  P-REC  FROM  MEISAI1  AFTER  1
                         WRITE  P-REC  FROM  MEISAI2  AFTER  1
                         ADD    2      TO    L-CNT
                  END-IF
*
     END-PERFORM.
*
     WRITE     P-REC  FROM  MEISAI3  AFTER  1.
     ADD       1              TO        L-CNT.
*
     MOVE      ZERO           TO        TBL-MISE-AREA.
     MOVE      ZERO           TO        WK-IXM.
*
     MOVE      HAC-F07        TO        OLD-SYOHINCD.
*ヘッダ情報
     MOVE      HAC-F05        TO        HACYMD.
     MOVE      HAC-F06        TO        NOUYMD.
     MOVE      HAC-F013       TO        TOK-F01  WKIGYOCD.
     PERFORM  HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG =  SPACE
              MOVE TOK-F03    TO    WKIGYONM
     ELSE
              MOVE SPACE      TO    WKIGYONM
     END-IF.
     MOVE     HAC-F03         TO    WSOKCD  SOK-F01.
     PERFORM  ZSOKMS-READ-SEC.
     IF  ZSOKMS-INV-FLG =  SPACE
              MOVE SOK-F02    TO   WSOKNM
     ELSE
              MOVE SPACE      TO    WSOKNM
     END-IF.
     MOVE     PARA-BUMON      TO    WK-HEN-BUMON  TAN-F01.
     MOVE     PARA-TANCD      TO    WK-HEN-TANCD  TAN-F02.
     MOVE     "-"             TO    WK-HEN-KU1.
     PERFORM  HTANMS-READ-SEC.
     IF  HTANMS-INV-FLG =  SPACE
              MOVE TAN-F03    TO    WK-HEN-TANNM
     ELSE
              MOVE SPACE      TO    WK-HEN-TANNM
     END-IF.
     MOVE     HAC-F01         TO    WK-HEN-DATE.
     MOVE     HAC-F012(1:2)   TO    WK-HEN-HH.
     MOVE     HAC-F012(3:2)   TO    WK-HEN-HM.
     MOVE     "-"             TO    WK-HEN-KU1.
     MOVE     HAC-F04         TO    WBUMON.
*
     MOVE      HAC-F15        TO        WGENTAN.
     MOVE      HAC-F16        TO        WBAITAN.
     MOVE      HAC-F11        TO        WSYOHINNM.
     MOVE      HAC-F12        TO        WKIKAKU.
     MOVE      HAC-F07        TO        WAITECD.
     MOVE      HAC-F08        TO        WJISHOCD.
     MOVE      ZERO           TO        WSURYO.
*
     MOVE      1              TO        IDX-CNT.
     PERFORM   CLR-SEC   UNTIL          IDX-CNT   >   10.
*
     MOVE      1              TO        IDX-CNT.
     MOVE      1              TO        MISE-CNT.
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
     IF       WK-IXM    >    500
              GO   TO   MEISAI-EXIT
     END-IF.
*
     IF       WK-IXM    >     10
              MOVE   SPACE          TO        SYOHINCD
                                              SYOHINNM
                                              KIKAKU
                                              MEI2-SHOCD
              MOVE   SPACE          TO        GENTANR
              MOVE   ZERO           TO        BAITAN
              MOVE   ZERO           TO        SURYOKEI
     END-IF.
*
     IF       T-MISE(WK-IXM)      =    ZERO
              MOVE      SPACE          TO   MISEBAN(WK-IX)
     ELSE
              MOVE      T-MISE(WK-IXM)      TO   MISE
              MOVE      WK-MISE             TO   MISEBAN(WK-IX)
     END-IF.
     MOVE     T-SURYO(WK-IXM)     TO   SURYO(WK-IX).
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
              PERFORM   SOUKEI-SEC.
*////END-IF.
*
     MOVE      CNT-READ  TO      IN-CNT.
     MOVE      P-CNT     TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE    HACXXXF   PRINTF  HTOKMS  HTANMS  ZSOKMS.
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
*2017/02/09 NAV ST
*****IF       L-CNT       >= 60
     IF       L-CNT       >= 50
*2017/02/09 NAV ED
              PERFORM        MIDASI-SEC
     END-IF.
*
*2017/02/09 NAV ST
*****COMPUTE  W-L-CNT     =  61             -    L-CNT.
     COMPUTE  W-L-CNT     =  51             -    L-CNT.
*2017/02/09 NAV ED
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
     COMPUTE  SOU-SU      =  GOU-SU         +    SOU-SU.
     COMPUTE  SOU-GENKIN  =  GOU-GENKIN     +    SOU-GENKIN.
     COMPUTE  SOU-BAIKIN  =  GOU-BAIKIN     +    SOU-BAIKIN.
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
     MOVE     ZERO           TO        SOU-SU
                                       SOU-GENKIN
                                       SOU-BAIKIN
                                       WGENHEI
                                       WBAIHEI.
*
 SOUKEI-EXIT.
     EXIT.
*
****************************************************************
*             担当者マスタ読込
****************************************************************
 HTANMS-READ-SEC        SECTION.
*
     READ  HTANMS
           INVALID      MOVE  "INV"  TO  HTANMS-INV-FLG
           NOT  INVALID MOVE  SPACE  TO  HTANMS-INV-FLG
     END-READ.
*
 HTANMS-READ-EXIT.
     EXIT.
*
****************************************************************
*             取引先マスタ読込
****************************************************************
 HTOKMS-READ-SEC        SECTION.
*
     READ  HTOKMS
           INVALID      MOVE  "INV"  TO  HTOKMS-INV-FLG
           NOT  INVALID MOVE  SPACE  TO  HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
*
****************************************************************
*             倉庫マスタ読込
****************************************************************
 ZSOKMS-READ-SEC        SECTION.
*
     READ  ZSOKMS
           INVALID      MOVE  "INV"  TO  ZSOKMS-INV-FLG
           NOT  INVALID MOVE  SPACE  TO  ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
*
********************<<  PUROGRAM  END  >>*************************

```
