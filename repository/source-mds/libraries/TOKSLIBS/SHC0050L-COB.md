# SHC0050L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SHC0050L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　発注集計表                        *
*    モジュール名　　　　：　発注集計表発行（_番＋商品順）    *
*    作成日／更新日　　　：　99/09/22                          *
*    作成者／更新者　　　：　ＮＡＶ吉田　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注集計表データファイルより、    *
*                            発注集計表を作表する。            *
*　　2004/06/29 TAKAHASHI　　　　　　　　　　　　　　　　*
*　  税込売価出力追加                                    *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SHC0050L.
 AUTHOR.                NAV Y.YOSHIDA.
 DATE-WRITTEN.          99/09/22.
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
*****<<集計表データ　Ｆ >>**************************************
     SELECT   SHWHACF   ASSIGN    TO        SHWHACF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HAC-STATUS.
*
*****<<  プリント　Ｆ   >>**************************************
     SELECT   PRINTF    ASSIGN    TO        LP-04-PRTF.
*
*                                                                *
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*--------------------------------------------------------------*
*    FILE = 集計表データファイル  RL=200    BF=5      ORG=SF   *
*--------------------------------------------------------------*
 FD  SHWHACF            BLOCK     CONTAINS  5    RECORDS.
     COPY     SHWHACF   OF        XFDLIB
              JOINING   HAC  AS   PREFIX.
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
 01  TANA-FLG                     PIC       9(01)  VALUE  ZERO.
*
**** ステイタス　エリア
 01  HAC-STATUS                   PIC       X(02).
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
     03  T-FLG                    PIC       9(01).
*****03  TBL-MISE                 OCCURS    200.
     03  TBL-MISE                 OCCURS    300.
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
     03  NEW-SYOHINCD             PIC       X(13)  VALUE  SPACE.
     03  NEW-RTCD                 PIC       X(10)  VALUE  SPACE.
*
 01  WK-OLD-KEY.
     03  OLD-NOHINBI              PIC       9(06)  VALUE  ZERO.
     03  OLD-BUMONCD              PIC       X(04)  VALUE  SPACE.
     03  OLD-SYOHINCD             PIC       X(13)  VALUE  SPACE.
     03  OLD-RTCD                 PIC       X(10)  VALUE  SPACE.
*
***** 原価単価保存ワーク
 01  WGENTAN                      PIC       9(07)V99 VALUE ZERO.
*
***** 売価単価保存ワーク
 01  WBAITAN                      PIC       9(07)  VALUE  ZERO.
***** 税込単価保存ワーク
 01  WZEITAN                      PIC       9(07)  VALUE  ZERO.
*
*
***** _番保存ワーク
 01  WTANABAN                     PIC       X(06)  VALUE  SPACE.
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
         05  ST-PG          PIC   X(08)  VALUE "SHC0050L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SHC0050L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SHC0050L".
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
     03  FILLER                   PIC  X(08)  VALUE "SHC0050L".
     03  FILLER                   PIC  X(26)  VALUE  SPACE.
     03  FILLER                   PIC  N(15)  VALUE
       NC"※※　発　注　集　計　表　※※"
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
     03  FILLER                   PIC  X(43)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE
       NC"［"
                   CHARACTER      TYPE IS     YA.
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
     03  FILLER                   PIC       X(20)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE
       NC"（"
                   CHARACTER      TYPE IS     YA.
     03  OUTNONM                  PIC  N(05)
                   CHARACTER      TYPE IS     YA.
     03  FILLER                   PIC  N(01)  VALUE
       NC"）"
                   CHARACTER      TYPE IS     YA.
     03  FILLER                   PIC       X(38)  VALUE  SPACE.
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
     03  FILLER                   PIC       X(68)  VALUE  SPACE.
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
*****03  FILLER                   PIC       X(12)  VALUE  SPACE.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"（税込）".
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
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
***** 明細行７
 01  MIDASHI7      CHARACTER      TYPE      IS     YA.
*****03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"_番".
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
 01  MEISAI2        CHARACTER      TYPE      IS     YA.
*****03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MEI2-SHOCD               PIC       X(13).
*****03  FILLER                   PIC       X(04)  VALUE  SPACE.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MEI2-FLG                 PIC       N(02).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  KIKAKU                   PIC       X(15).
     03  FILLER                   PIC       X(16)  VALUE  SPACE.
     03  BAITAN                   PIC       ZZZZZZZ.
*****03  FILLER                   PIC       X(10)  VALUE  SPACE.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  ZEITAN-1                 PIC       X(01).
     03  ZEITAN                   PIC       ZZZZZZZ.
     03  ZEITAN-2                 PIC       X(01).
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
*****03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  PRT-TANA                 PIC       X(06).
     03  FILLER                   PIC       X(129) VALUE  SPACE.
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
 01  PARA-TKBN              PIC   9(01).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-TKBN.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE SHWHACF.
     MOVE     "SHWHACF "          TO        ERR-FL-ID.
     MOVE     HAC-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY0021-START          SECTION.
*
     MOVE     "SSY0021-START"     TO   S-NAME.
     PERFORM            INIT-SEC.
*
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SSY0021-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SHWHACF.
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
              GO        TO        INIT-EXIT
     END-IF.
*
     MOVE     1         TO        IDX-CNT.
     PERFORM  CLR-SEC   UNTIL     IDX-CNT   >   10.
*
     MOVE     1         TO        IDX-CNT   MISE-CNT.
     MOVE     ZERO      TO        WK-IXM.
     MOVE     ZERO      TO        TBL-MISE-AREA.
     MOVE     ZERO      TO        T-FLG.
     MOVE     HAC-F05   TO        HACYMD.
     MOVE     HAC-F06   TO        NOUYMD.
     MOVE     HAC-F04   TO        OLD-BUMONCD.
*****MOVE     HAC-F03   TO        OLD-RTCD.
     MOVE     HAC-F20   TO        OLD-RTCD.
     MOVE     HAC-F07   TO        OLD-SYOHINCD
*
*## 1999/11/29 NAV T.T START ##*
     MOVE     HAC-F15   TO        WGENTAN.
*## 1999/11/29 NAV T.T END   ##*
     MOVE     HAC-F16   TO        WBAITAN.
     MOVE     HAC-F21   TO        WZEITAN.
     MOVE     HAC-F11   TO        WSYOHINNM.
     MOVE     HAC-F12   TO        WKIKAKU.
     MOVE     HAC-F17   TO        WKIGYONM.
     MOVE     HAC-F07   TO        WAITECD.
     MOVE     HAC-F08   TO        WJISHOCD.
     MOVE     HAC-F09   TO        WTANABAN.
*
     MOVE     HAC-F011  TO        WK-JDATE.
     MOVE     HAC-F012  TO        WK-JTIME.
     MOVE     HAC-F013  TO        WK-TORICD.
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
     MOVE     NEW-RTCD       TO   OLD-RTCD.

     READ     SHWHACF
          AT END
              MOVE     "END"      TO        END-FLG
              MOVE     HIGH-VALUE TO        WK-NEW-KEY
          NOT AT END
              MOVE     HAC-F06    TO        NEW-NOHINBI
              MOVE     HAC-F07    TO        NEW-SYOHINCD
              MOVE     HAC-F04    TO        NEW-BUMONCD
******        MOVE     HAC-F03    TO        NEW-RTCD
              MOVE     HAC-F20    TO        NEW-RTCD
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
     IF       PARA-TKBN      =    1
              MOVE NC"（発注数量）"    TO    HD1-TEISEI
     ELSE
              MOVE NC"（訂正数量）"    TO    HD1-TEISEI
     END-IF.
     MOVE     SYS-YY         TO   HD1-YY.
     MOVE     SYS-MM         TO   HD1-MM.
     MOVE     SYS-DD         TO   HD1-DD.
     ADD      1              TO   P-CNT.
     MOVE     P-CNT          TO   HD1-PAGE.
*
     MOVE     WK-BACHI-AREA  TO   HD2-BACHI.
*
     MOVE     WKIGYONM       TO   KIGYONM.
     MOVE     NC"　_　順　" TO   OUTNONM.
     MOVE     HAC-YY         TO   H-YY.
     MOVE     HAC-MM         TO   H-MM.
     MOVE     HAC-DD         TO   H-DD.
     MOVE     OLD-BUMONCD    TO   BUMONCD.
     MOVE     OLD-RTCD       TO   RTCD.
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
     WRITE    P-REC     FROM      MIDASHI7  AFTER     2.
     WRITE    P-REC     FROM      MIDASHI5  AFTER     1.
     WRITE    P-REC     FROM      MIDASHI6  AFTER     1.
     MOVE     SPACE     TO        P-REC.
     WRITE    P-REC     AFTER     1.
*
     MOVE     10        TO        L-CNT.
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
*    合計チェック
     IF       HAC-F04    NOT =     OLD-BUMONCD
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              PERFORM   CYUKEI-SEC
              PERFORM   GOUKEI-SEC
              MOVE      HAC-F04    TO        OLD-BUMONCD
******        MOVE      HAC-F03    TO        OLD-RTCD
              MOVE      HAC-F20    TO        OLD-RTCD
              MOVE      HAC-F06    TO        NOUYMD
              PERFORM   MIDASI-SEC
     END-IF.
*
*    中計チェック
*****IF       HAC-F03    NOT =     OLD-RTCD
     IF       HAC-F20    NOT =     OLD-RTCD
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              PERFORM   CYUKEI-SEC
              MOVE      HAC-F04    TO        OLD-BUMONCD
*******       MOVE      HAC-F03    TO        OLD-RTCD
              MOVE      HAC-F20    TO        OLD-RTCD
              MOVE      HAC-F06    TO        NOUYMD
              PERFORM   MIDASI-SEC
     END-IF.
*
*    小計チェック
     IF       HAC-F06    NOT =     NOUYMD
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              MOVE      HAC-F06    TO        NOUYMD
              PERFORM   MIDASI-SEC
     END-IF.
*
*    商品チェック
     IF       HAC-F07    NOT =     OLD-SYOHINCD
              PERFORM   S-CHECK-SEC
              MOVE      HAC-F07    TO       OLD-SYOHINCD
     END-IF.
*
*    明細行作成
     ADD      1         TO        WK-IXM.
*    引当済があった場合、ＦＬＧセット
     IF       HAC-F22 = ZERO
              MOVE  1             TO   T-FLG
     END-IF.
*
*
     MOVE     HAC-F02   TO        T-MISE(WK-IXM).
*    発注数量又は、訂正数量か判定
     IF       PARA-TKBN      =    1
              MOVE      HAC-F13   TO   T-SURYO(WK-IXM)
              COMPUTE   WSURYO    =    HAC-F13   +    WSURYO
     END-IF.
     IF       PARA-TKBN      =    2
              MOVE      HAC-F14   TO   T-SURYO(WK-IXM)
              COMPUTE   WSURYO    =    HAC-F14   +    WSURYO
     END-IF.
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
*****MOVE      OLD-SYOHINCD   TO        SYOHINCD.
     MOVE      WTANABAN       TO        PRT-TANA.
     MOVE      WSYOHINNM      TO        SYOHINNM.
     MOVE      WKIKAKU        TO        KIKAKU.
     MOVE      WJISHOCD       TO        SYOHINCD.
     MOVE      WAITECD        TO        MEI2-SHOCD.
     MOVE      WGENTAN        TO        GENTAN.
     MOVE      WBAITAN        TO        BAITAN.
     MOVE      WZEITAN        TO        ZEITAN.
     IF        WZEITAN  NOT =  ZERO
               MOVE  "("      TO        ZEITAN-1
               MOVE  ")"      TO        ZEITAN-2
     ELSE
               MOVE  SPACE    TO        ZEITAN-1 ZEITAN-2
     END-IF.
     MOVE      WSURYO         TO        SURYOKEI.
*    在庫不足分記号出力
     IF        T-FLG    =  1
               MOVE  NC"★"   TO        MEI2-FLG
     ELSE
               MOVE  SPACE    TO        MEI2-FLG
     END-IF.
*
     MOVE   ZERO              TO        WK-IXM.
*
     PERFORM   UNTIL     WK-IXM  >  200   OR
                         WK-IXM  >  WK-IXD
*
               MOVE      ZERO             TO   WK-IX
               PERFORM   MEISAI-SEC
                         UNTIL  WK-IX  >  10
*
                  IF     L-CNT     >=        58
                         PERFORM   MIDASI-SEC
                  END-IF
*
                  IF     MEISAI1   NOT =     SPACE
                         IF  TANA-FLG  =     ZERO
                             WRITE P-REC FROM MEISAI4 AFTER  1
                             ADD   1   TO    L-CNT
                         END-IF
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
*## 1999/11/29 NAV T.T START ##*
     MOVE      HAC-F15        TO        WGENTAN.
*## 1999/11/29 NAV T.T END   ##*
     MOVE      HAC-F16        TO        WBAITAN.
     MOVE      HAC-F21        TO        WZEITAN.
     MOVE      HAC-F11        TO        WSYOHINNM.
     MOVE      HAC-F12        TO        WKIKAKU.
     MOVE      HAC-F07        TO        WAITECD.
     MOVE      HAC-F08        TO        WJISHOCD.
     MOVE      HAC-F09        TO        WTANABAN.
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
*
     IF       WK-IX     =    1
              MOVE      ZERO      TO   TANA-FLG
     END-IF.
*
     IF       WK-IX     >    10
              GO   TO   MEISAI-EXIT
     END-IF.
*
     ADD      1         TO   WK-IXM.
     IF       WK-IXM    >    200
              GO   TO   MEISAI-EXIT
     END-IF.
*
     IF       WK-IXM    >     10
              MOVE   SPACE          TO        SYOHINCD
                                              SYOHINNM
                                              KIKAKU
                                              MEI2-SHOCD
                                              PRT-TANA
              MOVE   SPACE          TO        GENTANR
              MOVE   ZERO           TO        BAITAN
              MOVE   ZERO           TO        SURYOKEI
              MOVE   1              TO        TANA-FLG
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
     CLOSE    SHWHACF   PRINTF.
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
     IF       L-CNT       >= 58
              PERFORM        MIDASI-SEC
     END-IF.
*
     COMPUTE  W-L-CNT     =  61             -    L-CNT.
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
 SOUKEI-EXIT.
     EXIT.
*
********************<<  PUROGRAM  END  >>*************************

```
