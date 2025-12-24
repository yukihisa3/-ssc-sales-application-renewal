****************************************************************
*    顧客名　　　　　　　：　_サカタのタネ殿　　　　　　　　　*
*    業務名　　　　　　　：　ナフコ新ＥＤＩ　支払照合          *
*    モジュール名　　　　：　不照合リスト作成　　　　　　　　　*
*    作成日／更新日　　　：　12/05/04                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払合計ファイルと請求合計ファイル*
*                        ：　を比較して不照合リストを作成する。*
*    作成日／更新日　　　：　12/10/19                          *
*                        ：　ＯＵＴファイルに項目追加。　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI3703N.
 AUTHOR.                Y.Y.
 DATE-WRITTEN.          12/05/04.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         YA        IS   YA
         YB        IS   YB
         YB-21     IS   YB-21
         YA-21     IS   YA-21
*        STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払合計ファイル >>--*
     SELECT   SITGKF90  ASSIGN         DA-01-S-SITGKF90
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SSI-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   SETGK904   ASSIGN         DA-01-VI-SETGK904
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01   SEI-F03
                                       SEI-F05
                        STATUS         SEI-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< 不照合データ　>>-*
     SELECT   NFNUNMWK  ASSIGN         DA-01-S-NFNUNMWK
                        STATUS         UWK-ST.
*----<< 不照合データ　>>-*
     SELECT   NFNUNMW1  ASSIGN         DA-01-S-NFNUNMW1
                        STATUS         UW1-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払合計ファイル >>--*
 FD  SITGKF90           LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  20  RECORDS.
     COPY     SITGKF90  OF        XFDLIB
              JOINING   SSI       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  SETGK904            LABEL RECORD   IS   STANDARD.
     COPY     SETGK904  OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 不照合データ　>>-*
 FD  NFNUNMWK
                   BLOCK CONTAINS 1 RECORDS.
 01  UWK-REC.
     03  FILLER        PIC  X(1000).
*----<< 不照合データ　>>-*
 FD  NFNUNMW1
                   BLOCK CONTAINS 1 RECORDS.
 01  UW1-REC.
     03  FILLER        PIC  X(1000).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  SSI-END        PIC  X(03)     VALUE  SPACE.
     03  SEI-END        PIC  X(03)     VALUE  SPACE.
     03  INV-FLG        PIC  9(01)     VALUE  ZERO.
     03  CHK-FLG        PIC  X(01)     VALUE  SPACE.
     03  OUT-FLG        PIC  9(01)     VALUE  ZERO.
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03)     VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)     VALUE  ZERO.
     03  IN-CNT         PIC  9(09)     VALUE  ZERO.
 01  HTOKMS-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  HTENMS-INV-FLG     PIC  X(03)     VALUE  SPACE.
*
 01  WK-CNT1            PIC  9(06)     VALUE  ZERO.
 01  WK-CNT2            PIC  9(06)     VALUE  ZERO.
 01  WK-CNT3            PIC  9(06)     VALUE  ZERO.
 01  WK-CNT4            PIC  9(06)     VALUE  ZERO.
 01  SEI-CNT            PIC  9(07)     VALUE  ZERO.
 01  SII-CNT            PIC  9(07)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SSI-ST             PIC  X(02).
 01  SEI-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
 01  TOK-ST             PIC  X(02).
 01  UWK-ST             PIC  X(02).
 01  UW1-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  PG-ID              PIC  X(08)     VALUE   "SSI3703N".
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
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
 01  WK-DATE            PIC  9(06).
 01  FILLER             REDEFINES      WK-DATE.
     03  WK-YY          PIC  9(02).
     03  WK-MM          PIC  9(02).
     03  WK-DD          PIC  9(02).
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  SSI-KEY.
         05  SSI-TENPO  PIC  9(05)    VALUE  ZERO.
         05  SSI-DENNO  PIC  X(09)    VALUE  SPACE.
     03  SEI-KEY.
         05  SEI-TENPO  PIC  9(05)    VALUE  ZERO.
         05  SEI-DENNO  PIC  X(09)    VALUE  SPACE.
*
 01  WK-TORICD          PIC  9(08)    VALUE  ZERO.
*
*
 01  WK-KINGAKU         PIC S9(09)    VALUE  ZERO.
*----<< ｶｳﾝﾄｴﾘｱ >>--*
 01  CNT-AREA.
     03  CNT-UNMATCH    PIC  9(05)    VALUE  ZERO.
     03  CNT-OK         PIC  9(05)    VALUE  ZERO.
     03  CNT-SEI        PIC  9(05)    VALUE  ZERO.
     03  CNT-SSI        PIC  9(05)    VALUE  ZERO.
*2012/10/19↓
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
*01  HEAD01.
*    03  FILLER         PIC  X(01)     VALUE  X"28".
*    03  FILLER         PIC  N(04)     VALUE  NC"店舗ＣＤ".
*    03  FILLER         PIC  X(01)     VALUE  X"29".
*    03  FILLER         PIC  X(01)     VALUE  ",".
*    03  FILLER         PIC  X(01)     VALUE  X"28".
*    03  FILLER         PIC  N(03)     VALUE  NC"店舗名".
*    03  FILLER         PIC  X(01)     VALUE  X"29".
*    03  FILLER         PIC  X(01)     VALUE  ",".
*    03  FILLER         PIC  X(01)     VALUE  X"28".
*    03  FILLER         PIC  N(04)     VALUE  NC"伝票区分".
*    03  FILLER         PIC  X(01)     VALUE  X"29".
*    03  FILLER         PIC  X(01)     VALUE  ",".
*    03  FILLER         PIC  X(01)     VALUE  X"28".
*    03  FILLER         PIC  N(05)     VALUE  NC"伝票区分名".
*    03  FILLER         PIC  X(01)     VALUE  X"29".
*    03  FILLER         PIC  X(01)     VALUE  ",".
*    03  FILLER         PIC  X(01)     VALUE  X"28".
*    03  FILLER         PIC  N(04)     VALUE  NC"伝票番号".
*    03  FILLER         PIC  X(01)     VALUE  X"29".
*    03  FILLER         PIC  X(01)     VALUE  ",".
*    03  FILLER         PIC  X(01)     VALUE  X"28".
*    03  FILLER         PIC  N(04)     VALUE  NC"請求金額".
*    03  FILLER         PIC  X(01)     VALUE  X"29".
*    03  FILLER         PIC  X(01)     VALUE  ",".
*    03  FILLER         PIC  X(01)     VALUE  X"28".
*    03  FILLER         PIC  N(04)     VALUE  NC"支払金額".
*    03  FILLER         PIC  X(01)     VALUE  X"29".
*    03  FILLER         PIC  X(01)     VALUE  ",".
*    03  FILLER         PIC  X(01)     VALUE  X"28".
*    03  FILLER         PIC  N(08)     VALUE
*                       NC"レコード区分内容".
*    03  FILLER         PIC  X(01)     VALUE  X"29".
*    03  FILLER         PIC  X(01)     VALUE  ",".
*    03  FILLER         PIC  X(01)     VALUE  X"28".
*    03  FILLER         PIC  N(05)     VALUE  NC"データ内容".
*    03  FILLER         PIC  X(01)     VALUE  X"29".
*    03  FILLER         PIC  X(01)     VALUE  ",".
*    03  FILLER         PIC  X(01)     VALUE  X"28".
*    03  FILLER         PIC  N(03)     VALUE  NC"検収日".
*    03  FILLER         PIC  X(01)     VALUE  X"29".
*
*01  MEISAI01.
*    03  MS01           PIC  9(05).
*    03  KU01           PIC  X(01).
*    03  MS02-1         PIC  X(01).
*    03  MS02           PIC  N(15).
*    03  MS02-2         PIC  X(01).
*    03  KU02           PIC  X(01).
*    03  MS03           PIC  X(02).
*    03  KU03           PIC  X(01).
*    03  MS04-1         PIC  X(01).
*    03  MS04           PIC  N(05).
*    03  MS04-2         PIC  X(01).
*    03  KU04           PIC  X(01).
*    03  MS05           PIC  X(09).
*    03  KU05           PIC  X(01).
*    03  MS061          PIC  X(01).
*    03  MS06           PIC  9(10).
*    03  KU06           PIC  X(01).
*    03  MS071          PIC  X(01).
*    03  MS07           PIC  9(10).
*    03  KU07           PIC  X(01).
*    03  MS08-1         PIC  X(01).
*    03  MS08           PIC  N(15).
*    03  MS08-2         PIC  X(01).
*    03  KU08           PIC  X(01).
*    03  MS09-1         PIC  X(01).
*    03  MS09           PIC  N(15).
*    03  MS09-2         PIC  X(01).
*    03  KU09           PIC  X(01).
*    03  MS10           PIC  9(08).
*
*--<< CSV   AREA >>-*
 01  HEAD01.
     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(04)     VALUE  NC"店舗ＣＤ".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(03)     VALUE  NC"店舗名".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(04)     VALUE  NC"伝票区分".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(05)     VALUE  NC"伝票区分名".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(04)     VALUE  NC"伝票番号".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(08)
                             VALUE  NC"請求金額（税抜）".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(07)
                             VALUE  NC"請求金額消費税".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(08)
                             VALUE  NC"請求金額（税込）".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(08)
                             VALUE  NC"支払金額（税抜）".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(07)
                             VALUE  NC"支払金額消費税".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(08)
                             VALUE  NC"支払金額（税込）".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(08)     VALUE
                        NC"レコード区分内容".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(05)     VALUE  NC"データ内容".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(03)     VALUE  NC"検収日".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(04)     VALUE  NC"前回保留".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(04)     VALUE  NC"今回保留".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(05)     VALUE  NC"元伝票番号".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(04)     VALUE  NC"理由区分".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(05)     VALUE  NC"理由コード".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(04)     VALUE  NC"相殺区分".
     03  FILLER         PIC  X(01)     VALUE  X"29".

* 2023/09/27 ADD START
     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(07)    VALUE  NC"ＶＤ値引消費税".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(08) VALUE  NC"標準税率当月相殺".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(07)    VALUE  NC"標準相殺消費税".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(08) VALUE  NC"軽減税率当月相殺".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(07)     VALUE  NC"軽減相殺消費税"
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(04)     VALUE  NC"登録番号".
     03  FILLER         PIC  X(01)     VALUE  X"29".

     03  FILLER         PIC  X(01)     VALUE  ",".

     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(04)     VALUE  NC"元取引日".
     03  FILLER         PIC  X(01)     VALUE  X"29".
* 2023/09/27 ADD END
*
 01  MEISAI01.
*    店舗ＣＤ
     03  MS01           PIC  9(05).

     03  KU01           PIC  X(01).

*    店舗名
     03  MS02-1         PIC  X(01).
     03  MS02           PIC  N(15).
     03  MS02-2         PIC  X(01).

     03  KU02           PIC  X(01).

*伝票区分
     03  MS03           PIC  X(02).

     03  KU03           PIC  X(01).

*伝票区分名
     03  MS04-1         PIC  X(01).
     03  MS04           PIC  N(05).
     03  MS04-2         PIC  X(01).

     03  KU04           PIC  X(01).

*伝票番号
     03  MS05           PIC  X(09).

     03  KU05           PIC  X(01).

*請求金額（税抜）
     03  MS061          PIC  X(01).
     03  MS06           PIC  9(10).

     03  KU06           PIC  X(01).

*請求金額消費税
     03  MS111          PIC  X(01).
     03  MS11           PIC  9(10).

     03  KU07           PIC  X(01).

*請求金額（税込）
     03  MS121          PIC  X(01).
     03  MS12           PIC  9(10).

     03  KU08           PIC  X(01).

*支払金額（税抜）
     03  MS071          PIC  X(01).
     03  MS07           PIC  9(10).

     03  KU09           PIC  X(01).

*支払金額消費税
     03  MS131          PIC  X(01).
     03  MS13           PIC  9(10).

     03  KU10           PIC  X(01).

*支払金額（税込）
     03  MS141          PIC  X(01).
     03  MS14           PIC  9(10).

     03  KU11           PIC  X(01).

*レコード区分名
     03  MS08-1         PIC  X(01).
     03  MS08           PIC  N(15).
     03  MS08-2         PIC  X(01).

     03  KU12           PIC  X(01).

*データ内容
     03  MS09-1         PIC  X(01).
     03  MS09           PIC  N(15).
     03  MS09-2         PIC  X(01).

     03  KU13           PIC  X(01).

*検収日
     03  MS10           PIC  9(08).

     03  KU14           PIC  X(01).

*前回保留ＦＬＧ
     03  MS15           PIC  X(01).

     03  KU15           PIC  X(01).

*今回保留ＦＬＧ
     03  MS16           PIC  X(01).

     03  KU16           PIC  X(01).

*元伝票番号
     03  MS17           PIC  X(09).

     03  KU17           PIC  X(01).

*理由区分
     03  MS18-1         PIC  X(01).
     03  MS18           PIC  N(15).
     03  MS18-2         PIC  X(01).

     03  KU18           PIC  X(01).

*理由コード
     03  MS19-1         PIC  X(01).
     03  MS19           PIC  N(10).
     03  MS19-2         PIC  X(01).

     03  KU19           PIC  X(01).

*相殺区分
     03  MS20-1         PIC  X(01).
     03  MS20           PIC  N(10).
     03  MS20-2         PIC  X(01).

* 2023/09/27 ADD START

     03  KU20           PIC  X(01).
*ＶＤ値引消費税
     03  MS21           PIC  9(10).

     03  KU21           PIC  X(01).

*標準税率当月相殺
     03  MS22           PIC  9(10).

     03  KU22           PIC  X(01).

*標準相殺消費税
     03  MS23           PIC  9(10).

     03  KU23           PIC  X(01).

*軽減税率当月相殺
     03  MS24           PIC  9(10).

     03  KU24           PIC  X(01).

*軽減相殺消費税
     03  MS25           PIC  9(10).

     03  KU25           PIC  X(01).

*登録番号
     03  MS26           PIC  X(14).

     03  KU26           PIC  X(01).

*元取引日
     03  MS27           PIC  9(08).
*
*2012/10/19↑
*
 01  TAIL01.
     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(15)     VALUE
                        NC"＜不照合情報＞　請求データ無：".
     03  FILLER         PIC  X(01)     VALUE  X"29".
     03  FILLER         PIC  X(01)     VALUE  ",".
     03  TL-CNT1        PIC  9(07).
     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(08)     VALUE
                        NC"　支払データ無：".
     03  FILLER         PIC  X(01)     VALUE  X"29".
     03  FILLER         PIC  X(01)     VALUE  ",".
     03  TL-CNT2        PIC  9(07).
     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(06)     VALUE
                        NC"　金額違い：".
     03  FILLER         PIC  X(01)     VALUE  X"29".
     03  FILLER         PIC  X(01)     VALUE  ",".
     03  TL-CNT3        PIC  9(07).
     03  FILLER         PIC  X(01)     VALUE  X"28".
     03  FILLER         PIC  N(06)     VALUE
                        NC"　照合件数：".
     03  FILLER         PIC  X(01)     VALUE  X"29".
     03  FILLER         PIC  X(01)     VALUE  ",".
     03  TL-CNT4        PIC  9(07).
*
*01  LINK-AREA2.
*    03  LINK-IN-KBN        PIC   X(01).
*    03  LINK-IN-YMD6       PIC   9(06).
*    03  LINK-IN-YMD8       PIC   9(08).
*    03  LINK-OUT-RET       PIC   X(01).
*    03  LINK-OUT-YMD8      PIC   9(08).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計ファイル >>--*
 SSI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SITGKF90.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SITGKF90 ERROR " SSI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 SETGK904-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGK904.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SETGK904  ERROR " SEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " HTENMS   ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  HTOKMS   ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< アンマッチＣＳＶ >>--*
 NFNUNMWK-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFNUNMWK.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID " NFNUNMWK ERROR " UWK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< アンマッチＣＳＶ >>--*
 NFNUNMW1-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFNUNMW1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID " NFNUNMWK ERROR " UW1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*プログラム開始メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*プログラムコントロール
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     SSI-END   =    "END"
                                   AND SEI-END   =    "END".
     PERFORM  300-END-RTN.
*プログラム終了メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*ファイルのオープン
     OPEN     INPUT     SITGKF90.
     OPEN     INPUT     SETGK904.
     OPEN     INPUT     HTENMS.
     OPEN     INPUT     HTOKMS.
     OPEN     OUTPUT    NFNUNMWK.
     OPEN     OUTPUT    NFNUNMW1.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     ZERO           TO   CNT-AREA.
     MOVE     SPACE          TO   SSI-END SEI-END.
     MOVE     SPACE          TO   BREAK-KEY.
     MOVE     99             TO   LINE-CNT.
     INITIALIZE              BREAK-KEY.
*
     PERFORM  210-HEAD-PRINT.
*
     PERFORM  900-SSI-READ.
     MOVE     SSI-F01        TO   WK-TORICD.
     PERFORM  900-SEI-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     EVALUATE  TRUE
*請求データなしチェック
         WHEN      SSI-KEY   <    SEI-KEY
***************IF   SSI-F07  NOT =  ZERO
                    ADD      1         TO      WK-CNT1
                    PERFORM  210-HENSYU1-RTN
***************END-IF
               PERFORM  900-SSI-READ
*正常データ（金額アンマッチチェック）
         WHEN      SSI-KEY   =    SEI-KEY
               PERFORM  210-HENSYU2-RTN
               PERFORM  900-SSI-READ
               PERFORM  900-SEI-READ
*支払情報データなしチェック
         WHEN      SSI-KEY   >    SEI-KEY
               ADD      1         TO      WK-CNT2
               PERFORM  210-HENSYU3-RTN
               PERFORM  900-SEI-READ
     END-EVALUATE.
*
     IF  SII-CNT(5:3) = "000" OR "500"
         DISPLAY "READ-CNT = " SII-CNT " - " SEI-CNT UPON CONS
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*    テイル印刷
     PERFORM  TAIL-RTN.
*ファイルのクローズ
     CLOSE    SITGKF90.
     CLOSE    SETGK904.
     CLOSE    HTENMS.
     CLOSE    HTOKMS.
     CLOSE    NFNUNMWK.
     CLOSE    NFNUNMW1.
*データ内容メッセージ
     DISPLAY "ｾｲｷｭｳﾃﾞｰﾀ ﾅｼ = "  WK-CNT1     UPON CONS.
     DISPLAY "ｼﾊﾗｲ ﾃﾞｰﾀ ﾅｼ = "  WK-CNT2     UPON CONS.
     DISPLAY "ｷﾝｶﾞｸﾁｶﾞｲ ﾅｼ = "  WK-CNT3     UPON CONS.
     DISPLAY "OKﾃﾞｰﾀ       = "  WK-CNT4     UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 210-HEAD-PRINT         SECTION.
*ＨＥＡＤ印字
     MOVE     HEAD01         TO   UWK-REC.
     WRITE  UWK-REC.
     MOVE     HEAD01         TO   UW1-REC.
     WRITE  UW1-REC.
*
 210-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      明細行印字（請求データ無）
*--------------------------------------------------------------*
 210-HENSYU1-RTN        SECTION.
*
     MOVE     SPACE          TO   MEISAI01.
     MOVE     ","            TO   KU01 KU02 KU03 KU04 KU05
                                  KU06 KU07 KU08 KU09 KU10
                                  KU11 KU12 KU13 KU14 KU15
                                  KU16 KU17 KU18 KU19.
* 2023/09/27 ADD START
     MOVE     ","            TO   KU20 KU21 KU22 KU23 KU24
                                  KU25 KU26.
* 2023/09/27 ADD END
     MOVE     X"28"                     TO   MS02-1.
     MOVE     X"29"                     TO   MS02-2.
     MOVE     X"28"                     TO   MS04-1.
     MOVE     X"29"                     TO   MS04-2.
     MOVE     X"28"                     TO   MS08-1.
     MOVE     X"29"                     TO   MS08-2.
     MOVE     X"28"                     TO   MS09-1.
     MOVE     X"29"                     TO   MS09-2.
     MOVE     X"28"                     TO   MS18-1.
     MOVE     X"29"                     TO   MS18-2.
     MOVE     X"28"                     TO   MS19-1.
     MOVE     X"29"                     TO   MS19-2.
     MOVE     X"28"                     TO   MS20-1.
     MOVE     X"29"                     TO   MS20-2.
*
     MOVE     SSI-F01                   TO   TEN-F52.
     MOVE     SSI-F03                   TO   TEN-F011  MS01.
     PERFORM  900-TEN-READ.
     IF   HTENMS-INV-FLG = "INV"
          MOVE  ALL NC"＊"              TO   MS02
     ELSE
          MOVE  TEN-F03                 TO   MS02
     END-IF.
*相殺伝票の場合の店舗名
     IF  SSI-F03  =  ZERO
          MOVE  NC"【相殺伝票分】"      TO   MS02
     END-IF.
*伝票区分名称セット
     MOVE  SSI-F04                      TO   MS03.
     EVALUATE SSI-F04
         WHEN 1      MOVE NC"ＴＡ仕入"  TO   MS04
         WHEN 2      MOVE NC"手書仕入"  TO   MS04
         WHEN 11     MOVE NC"返品　　"  TO   MS04
                     IF  SSI-F07 < ZERO
                         MOVE NC"返品赤" TO  MS09(8:3)
                     END-IF
*2012/11/08*ST*******COMPUTE SSI-F07 = SSI-F07 * -1
*                    COMPUTE SSI-F071 = SSI-F071 * -1
************ED*******COMPUTE SSI-F072 = SSI-F072 * -1
         WHEN 12     MOVE NC"仕入値引"  TO   MS04
                     IF  SSI-F07 < ZERO
                         MOVE NC"値引赤" TO  MS09(8:3)
                     END-IF
*2012/11/08*ST*******COMPUTE SSI-F07 = SSI-F07 * -1
*                    COMPUTE SSI-F071 = SSI-F071 * -1
*2012/11/08*ED*******COMPUTE SSI-F072 = SSI-F072 * -1
         WHEN 61     MOVE NC"相殺　　"  TO   MS04
*2013/03/26 NAV ST
******************** COMPUTE SSI-F07 = SSI-F07 * -1
*                    COMPUTE SSI-F071 = SSI-F071 * -1
******************** COMPUTE SSI-F072 = SSI-F072 * -1
                     COMPUTE SSI-F07 = SSI-F07
                     COMPUTE SSI-F071 = SSI-F071
                     COMPUTE SSI-F072 = SSI-F072
*2013/03/26 NAV ED
         WHEN OTHER  MOVE ALL NC"＊"    TO   MS04
     END-EVALUATE.
*伝票番号
     MOVE     SSI-F05                  TO   MS05.
*請求金額（税抜）、消費税、（税込）
     MOVE     ZERO                     TO   MS061.
     MOVE     ZERO                     TO   MS06.
     MOVE     ZERO                     TO   MS111.
     MOVE     ZERO                     TO   MS11.
     MOVE     ZERO                     TO   MS121.
     MOVE     ZERO                     TO   MS12.
*支払金額（税抜）
     MOVE     SSI-F07                  TO   MS07.
     IF  SSI-F07  <  ZERO
              MOVE  "-"                TO   MS071
     ELSE
              MOVE  ZERO               TO   MS071
     END-IF.
*支払金額消費税
     MOVE     SSI-F071                 TO   MS13.
     IF  SSI-F071 <  ZERO
              MOVE  "-"                TO   MS131
     ELSE
              MOVE  ZERO               TO   MS131
     END-IF.
*支払金額（税込）
     MOVE     SSI-F072                 TO   MS14.
     IF  SSI-F072 <  ZERO
              MOVE  "-"                TO   MS141
     ELSE
              MOVE  ZERO               TO   MS141
     END-IF.
*レコード区分内容
     MOVE     NC"【請求データ無】"     TO   MS08.
*データ内容
     MOVE     NC"※支払Ｄより※"       TO   MS09(1:7).
*検収日
     MOVE     SSI-F02                  TO   MS10.
*前回保留ＦＬＧ
     IF       SSI-F16  =  0
              MOVE     SPACE           TO   MS15
     ELSE
              MOVE     SSI-F16         TO   MS15
     END-IF.
*今回保留ＦＬＧ
     IF       SSI-F17  =  0
              MOVE     SPACE           TO   MS16
     ELSE
              MOVE     SSI-F17         TO   MS16
     END-IF.
*元伝票番号
     MOVE     SSI-F13                  TO   MS17.
*理由区分・理由コード
     MOVE     SPACE                    TO   MS18   MS19.
     EVALUATE SSI-F14
         WHEN 01     MOVE NC"Ｔ／Ａ訂正理由（注）"      TO MS18
                     IF  SSI-F15 = 01
                         MOVE NC"仕入先都合"       TO   MS19
                     END-IF
                     IF  SSI-F15 = 02
                         MOVE NC"ナフコ都合"       TO   MS19
                     END-IF
                     IF  SSI-F15 = 03
                         MOVE NC"その他"           TO   MS19
                     END-IF
         WHEN 02     MOVE NC"返品理由"                  TO MS18
                     IF  SSI-F15 = 01
                         MOVE NC"不良品"           TO   MS19
                     END-IF
                     IF  SSI-F15 = 99
                         MOVE NC"本部指示"         TO   MS19
                     END-IF
         WHEN 03     MOVE NC"修正（赤伝）理由"          TO MS18
                     IF  SSI-F15 = 01
                         MOVE NC"店舗コード違い"   TO   MS19
                     END-IF
                     IF  SSI-F15 = 02
                         MOVE NC"仕入先違い"       TO   MS19
                     END-IF
                     IF  SSI-F15 = 03
                         MOVE NC"日付違い"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 04
                         MOVE NC"原価違い"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 05
                         MOVE NC"売価違い"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 06
                         MOVE NC"数量違い"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 07
                         MOVE NC"金額違い"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 08
                         MOVE NC"部門違い"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 09
                         MOVE NC"商品コード違い"   TO   MS19
                     END-IF
         WHEN 07     MOVE NC"仕入値引理由（買掛）"      TO MS18
                     IF  SSI-F15 = 01
                         MOVE NC"ＶＤ仮引"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 02
                         MOVE NC"ＶＤ精算"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 03
                         MOVE NC"ＶＤ前期末精算"   TO   MS19
                     END-IF
         WHEN 08     MOVE NC"仕入値引理由（本部発注）"  TO MS18
                     IF  SSI-F15 = 04
                         MOVE NC"ＶＤ精算"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 05
                         MOVE NC"違算調整"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 99
                         MOVE NC"本部指示"         TO   MS19
                     END-IF
         WHEN 09     MOVE NC"仕入値引理由（店舗発注）"  TO MS18
                     IF  SSI-F15 = 99
                         MOVE NC"本部指示"         TO   MS19
                     END-IF
         WHEN 14     MOVE NC"検品差異理由"              TO MS18
                     IF  SSI-F15 = 01
                         MOVE NC"数量不足"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 02
                         MOVE NC"不良品"           TO   MS19
                     END-IF
         WHEN OTHER  MOVE ALL NC"＊"                    TO MS18
     END-EVALUATE.
*
*相殺区分
     MOVE     SPACE                    TO   MS20.
     EVALUATE SSI-F10
         WHEN 01     MOVE NC"郵送料"                    TO MS20
         WHEN 02     MOVE NC"振込料"                    TO MS20
         WHEN 03     MOVE NC"シール代"                  TO MS20
         WHEN 04     MOVE NC"アドバルーン"              TO MS20
         WHEN 05     MOVE NC"協賛金"                    TO MS20
         WHEN 06     MOVE NC"機会損失料"                TO MS20
         WHEN 07     MOVE NC"ＥＯＳ処理料"              TO MS20
         WHEN 08     MOVE NC"ＥＯＳ伝票代"              TO MS20
         WHEN 09     MOVE NC"値札代"                    TO MS20
         WHEN 10     MOVE NC"センター手数料"            TO MS20
         WHEN 11     MOVE NC"買掛処理料"                TO MS20
         WHEN 12     MOVE NC"現金リベート"              TO MS20
         WHEN 13     MOVE NC"ＥＯＳ基本料"              TO MS20
         WHEN 14     MOVE NC"カラーコピー使用料"        TO MS20
         WHEN 18     MOVE NC"支払修正－"                TO MS20
         WHEN 19     MOVE NC"支払修正＋"                TO MS20
         WHEN 20     MOVE NC"債務免除"                  TO MS20
         WHEN OTHER  MOVE ALL NC"＊"                    TO MS20
     END-EVALUATE.
* 2023/09/27 ADD START
*ＶＤ値引消費税
     MOVE     SSI-F18                  TO   MS21.
*標準税率当月相殺
     MOVE     SSI-F19                  TO   MS22.
*標準相殺消費税
     MOVE     SSI-F20                  TO   MS23.
*軽減税率当月相殺
     MOVE     SSI-F21                  TO   MS24.
*軽減相殺消費税
     MOVE     SSI-F22                  TO   MS25.
*登録番号
     MOVE     SSI-F23                  TO   MS26.
*元取引日
     MOVE     SSI-F24                  TO   MS27.
* 2023/09/27 ADD END
*
*明細行印字
     IF   SSI-F03  <  170
          MOVE     MEISAI01            TO   UWK-REC
          WRITE  UWK-REC
     ELSE
          MOVE     MEISAI01            TO   UW1-REC
          WRITE  UW1-REC
     END-IF.
*
     ADD      1                        TO   LINE-CNT.
*
 210-HENSYU1-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      明細行印字（照合ＯＫ金額不一致）
*--------------------------------------------------------------*
 210-HENSYU2-RTN        SECTION.
*
     MOVE     SPACE          TO   MEISAI01.
     MOVE     ","            TO   KU01 KU02 KU03 KU04 KU05
                                  KU06 KU07 KU08 KU09 KU10
                                  KU11 KU12 KU13 KU14 KU15
                                  KU16 KU17 KU18 KU19.
* 2023/09/27 ADD START
     MOVE     ","            TO   KU20 KU21 KU22 KU23 KU24
                                  KU25 KU26.
* 2023/09/27 ADD END
     MOVE     X"28"                     TO   MS02-1.
     MOVE     X"29"                     TO   MS02-2.
     MOVE     X"28"                     TO   MS04-1.
     MOVE     X"29"                     TO   MS04-2.
     MOVE     X"28"                     TO   MS08-1.
     MOVE     X"29"                     TO   MS08-2.
     MOVE     X"28"                     TO   MS09-1.
     MOVE     X"29"                     TO   MS09-2.
     MOVE     X"28"                     TO   MS18-1.
     MOVE     X"29"                     TO   MS18-2.
     MOVE     X"28"                     TO   MS19-1.
     MOVE     X"29"                     TO   MS19-2.
     MOVE     X"28"                     TO   MS20-1.
     MOVE     X"29"                     TO   MS20-2.
*
     MOVE     SSI-F01                   TO   TEN-F52.
     MOVE     SSI-F03                   TO   TEN-F011  MS01.
     PERFORM  900-TEN-READ.
     IF   HTENMS-INV-FLG = "INV"
          MOVE  ALL NC"＊"              TO   MS02
     ELSE
          MOVE  TEN-F03                 TO   MS02
     END-IF.
*伝票区分名称セット
     MOVE  SSI-F04                      TO   MS03.
     EVALUATE SSI-F04
         WHEN 1      MOVE NC"ＴＡ仕入"  TO   MS04
         WHEN 2      MOVE NC"手書仕入"  TO   MS04
         WHEN 11     MOVE NC"返品　　"  TO   MS04
                     IF  SSI-F07 < ZERO
                         MOVE NC"返品赤" TO  MS09(8:3)
                     END-IF
*2012/11/08*ST*******COMPUTE SSI-F07 = SSI-F07 * -1
*                    COMPUTE SSI-F071 = SSI-F071 * -1
*2012/11/08*ST*******COMPUTE SSI-F072 = SSI-F072 * -1
         WHEN 12     MOVE NC"仕入値引"  TO   MS04
                     IF  SSI-F07 < ZERO
                         MOVE NC"値引赤" TO  MS09(8:3)
                     END-IF
*2012/11/08*ST*******COMPUTE SSI-F07 = SSI-F07 * -1
*                    COMPUTE SSI-F071 = SSI-F071 * -1
*2012/11/08*ST*******COMPUTE SSI-F072 = SSI-F072 * -1
         WHEN 61     MOVE NC"相殺　　"  TO   MS04
*2012/11/08*ST*******COMPUTE SSI-F07 = SSI-F07 * -1
*                    COMPUTE SSI-F07 = SSI-F07 * -1
*                    COMPUTE SSI-F071 = SSI-F071 * -1
*2012/11/08*ST*******COMPUTE SSI-F072 = SSI-F072 * -1
         WHEN OTHER  MOVE ALL NC"＊"    TO   MS04
     END-EVALUATE.
*伝票番号
     MOVE     SSI-F05                  TO   MS05.
*請求金額（税抜）
     MOVE     SEI-F06                  TO   MS06.
     IF  SEI-F06  <  ZERO
              MOVE  "-"                TO   MS061
     ELSE
              MOVE  ZERO               TO   MS061
     END-IF.
*請求金額消費税
     MOVE     SEI-F061                 TO   MS11.
     IF  SEI-F061 <  ZERO
              MOVE  "-"                TO   MS111
     ELSE
              MOVE  ZERO               TO   MS111
     END-IF.
*請求金額（税込）
     MOVE     SEI-F062                 TO   MS12.
     IF  SEI-F062 <  ZERO
              MOVE  "-"                TO   MS121
     ELSE
              MOVE  ZERO               TO   MS121
     END-IF.
*支払金額（税抜）
     MOVE     SSI-F07                  TO   MS07.
     IF  SSI-F07  <  ZERO
              MOVE  "-"                TO   MS071
     ELSE
              MOVE  ZERO               TO   MS071
     END-IF.
*支払金額消費税
     MOVE     SSI-F071                 TO   MS13.
     IF  SSI-F071 <  ZERO
              MOVE  "-"                TO   MS131
     ELSE
              MOVE  ZERO               TO   MS131
     END-IF.
*支払金額（税込）
     MOVE     SSI-F072                 TO   MS14.
     IF  SSI-F072 <  ZERO
              MOVE  "-"                TO   MS141
     ELSE
              MOVE  ZERO               TO   MS141
     END-IF.
*レコード区分内容
     MOVE     NC"【照合データＯＫ】"   TO   MS08.
*データ内容
*****MOVE     NC"☆金額不一致☆"       TO   MS09(1:7).
*検収日
     MOVE     SSI-F02                  TO   MS10.
*前回保留ＦＬＧ
     IF       SSI-F16  =  0
              MOVE     SPACE           TO   MS15
     ELSE
              MOVE     SSI-F16         TO   MS15
     END-IF.
*今回保留ＦＬＧ
     IF       SSI-F17  =  0
              MOVE     SPACE           TO   MS16
     ELSE
              MOVE     SSI-F17         TO   MS16
     END-IF.
*元伝票番号
     MOVE     SSI-F13                  TO   MS17.
*理由区分・理由コード
     MOVE     SPACE                    TO   MS18   MS19.
     EVALUATE SSI-F14
         WHEN 01     MOVE NC"Ｔ／Ａ訂正理由（注）"      TO MS18
                     IF  SSI-F15 = 01
                         MOVE NC"仕入先都合"       TO   MS19
                     END-IF
                     IF  SSI-F15 = 02
                         MOVE NC"ナフコ都合"       TO   MS19
                     END-IF
                     IF  SSI-F15 = 03
                         MOVE NC"その他"           TO   MS19
                     END-IF
         WHEN 02     MOVE NC"返品理由"                  TO MS18
                     IF  SSI-F15 = 01
                         MOVE NC"不良品"           TO   MS19
                     END-IF
                     IF  SSI-F15 = 99
                         MOVE NC"本部指示"         TO   MS19
                     END-IF
         WHEN 03     MOVE NC"修正（赤伝）理由"          TO MS18
                     IF  SSI-F15 = 01
                         MOVE NC"店舗コード違い"   TO   MS19
                     END-IF
                     IF  SSI-F15 = 02
                         MOVE NC"仕入先違い"       TO   MS19
                     END-IF
                     IF  SSI-F15 = 03
                         MOVE NC"日付違い"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 04
                         MOVE NC"原価違い"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 05
                         MOVE NC"売価違い"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 06
                         MOVE NC"数量違い"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 07
                         MOVE NC"金額違い"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 08
                         MOVE NC"部門違い"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 09
                         MOVE NC"商品コード違い"   TO   MS19
                     END-IF
         WHEN 07     MOVE NC"仕入値引理由（買掛）"      TO MS18
                     IF  SSI-F15 = 01
                         MOVE NC"ＶＤ仮引"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 02
                         MOVE NC"ＶＤ精算"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 03
                         MOVE NC"ＶＤ前期末精算"   TO   MS19
                     END-IF
         WHEN 08     MOVE NC"仕入値引理由（本部発注）"  TO MS18
                     IF  SSI-F15 = 04
                         MOVE NC"ＶＤ精算"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 05
                         MOVE NC"違算調整"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 99
                         MOVE NC"本部指示"         TO   MS19
                     END-IF
         WHEN 09     MOVE NC"仕入値引理由（店舗発注）"  TO MS18
                     IF  SSI-F15 = 99
                         MOVE NC"本部指示"         TO   MS19
                     END-IF
         WHEN 14     MOVE NC"検品差異理由"              TO MS18
                     IF  SSI-F15 = 01
                         MOVE NC"数量不足"         TO   MS19
                     END-IF
                     IF  SSI-F15 = 02
                         MOVE NC"不良品"           TO   MS19
                     END-IF
         WHEN OTHER  MOVE ALL NC"＊"                    TO MS18
     END-EVALUATE.
*
*相殺区分
     MOVE     SPACE                    TO   MS20.
     EVALUATE SSI-F10
         WHEN 01     MOVE NC"郵送料"                    TO MS20
         WHEN 02     MOVE NC"振込料"                    TO MS20
         WHEN 03     MOVE NC"シール代"                  TO MS20
         WHEN 04     MOVE NC"アドバルーン"              TO MS20
         WHEN 05     MOVE NC"協賛金"                    TO MS20
         WHEN 06     MOVE NC"機会損失料"                TO MS20
         WHEN 07     MOVE NC"ＥＯＳ処理料"              TO MS20
         WHEN 08     MOVE NC"ＥＯＳ伝票代"              TO MS20
         WHEN 09     MOVE NC"値札代"                    TO MS20
         WHEN 10     MOVE NC"センター手数料"            TO MS20
         WHEN 11     MOVE NC"買掛処理料"                TO MS20
         WHEN 12     MOVE NC"現金リベート"              TO MS20
         WHEN 13     MOVE NC"ＥＯＳ基本料"              TO MS20
         WHEN 14     MOVE NC"カラーコピー使用料"        TO MS20
         WHEN 18     MOVE NC"支払修正－"                TO MS20
         WHEN 19     MOVE NC"支払修正＋"                TO MS20
         WHEN 20     MOVE NC"債務免除"                  TO MS20
         WHEN OTHER  MOVE ALL NC"＊"                    TO MS20
     END-EVALUATE.
* 2023/09/27 ADD START
*ＶＤ値引消費税
     MOVE     SSI-F18                  TO   MS21.
*標準税率当月相殺
     MOVE     SSI-F19                  TO   MS22.
*標準相殺消費税
     MOVE     SSI-F20                  TO   MS23.
*軽減税率当月相殺
     MOVE     SSI-F21                  TO   MS24.
*軽減相殺消費税
     MOVE     SSI-F22                  TO   MS25.
*登録番号
     MOVE     SSI-F23                  TO   MS26.
*元取引日
     MOVE     SSI-F24                  TO   MS27.
* 2023/09/27 ADD END
*
*出力判定
*----IF  SSI-F07  NOT =  SEI-F06
     IF  SSI-F072 NOT =  SEI-F062
         ADD  1         TO      WK-CNT3
         MOVE NC"【金額不一致です】"   TO   MS08
         MOVE NC"☆金額不一致☆"       TO   MS09(1:7)
     ELSE
         ADD  1         TO      WK-CNT4
*********GO             TO      210-HENSYU2-RTN-EXIT
     END-IF.
*
*明細行印字
     IF   SSI-F03  <  170
          MOVE     MEISAI01            TO   UWK-REC
          WRITE  UWK-REC
     ELSE
          MOVE     MEISAI01            TO   UW1-REC
          WRITE  UW1-REC
     END-IF.
*
     ADD      1                        TO   LINE-CNT.
*
 210-HENSYU2-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      明細行印字（支払データ無）
*--------------------------------------------------------------*
 210-HENSYU3-RTN        SECTION.
*
     MOVE     SPACE          TO   MEISAI01.
     MOVE     ","            TO   KU01 KU02 KU03 KU04 KU05
                                  KU06 KU07 KU08 KU09 KU10
                                  KU11 KU12 KU13 KU14 KU15
                                  KU16 KU17 KU18 KU19.
* 2023/09/27 ADD START
     MOVE     ","            TO   KU20 KU21 KU22 KU23 KU24
                                  KU25 KU26.
* 2023/09/27 ADD END
     MOVE     X"28"                     TO   MS02-1.
     MOVE     X"29"                     TO   MS02-2.
     MOVE     X"28"                     TO   MS04-1.
     MOVE     X"29"                     TO   MS04-2.
     MOVE     X"28"                     TO   MS08-1.
     MOVE     X"29"                     TO   MS08-2.
     MOVE     X"28"                     TO   MS09-1.
     MOVE     X"29"                     TO   MS09-2.
     MOVE     X"28"                     TO   MS18-1.
     MOVE     X"29"                     TO   MS18-2.
     MOVE     X"28"                     TO   MS19-1.
     MOVE     X"29"                     TO   MS19-2.
     MOVE     X"28"                     TO   MS20-1.
     MOVE     X"29"                     TO   MS20-2.
*
     MOVE     SEI-F01                   TO   TEN-F52.
     MOVE     SEI-F03                   TO   TEN-F011  MS01.
     PERFORM  900-TEN-READ.
     IF   HTENMS-INV-FLG = "INV"
          MOVE  ALL NC"＊"              TO   MS02
     ELSE
          MOVE  TEN-F03                 TO   MS02
     END-IF.
*伝票区分名称セット
     MOVE  SEI-F07            TO    MS03.
     EVALUATE SEI-F07
         WHEN 40     MOVE NC"ＳＴ売上"  TO   MS04
         WHEN 41     MOVE NC"ＳＴ返品"  TO   MS04
         WHEN 42     MOVE NC"ＳＴ値引"  TO   MS04
         WHEN OTHER  MOVE ALL NC"ＡＣＯＳ" TO   MS04
     END-EVALUATE.
*伝票番号
     MOVE     SEI-F05                  TO   MS05.
*請求金額（税抜）
     MOVE     SEI-F06                  TO   MS06.
     IF  SEI-F06  <  ZERO
              MOVE  "-"                TO   MS061
     ELSE
              MOVE  ZERO               TO   MS061
     END-IF.
*請求金額消費税
     MOVE     SEI-F061                 TO   MS11.
     IF  SEI-F061 <  ZERO
              MOVE  "-"                TO   MS111
     ELSE
              MOVE  ZERO               TO   MS111
     END-IF.
*請求金額（税込）
     MOVE     SEI-F062                 TO   MS12.
     IF  SEI-F062 <  ZERO
              MOVE  "-"                TO   MS121
     ELSE
              MOVE  ZERO               TO   MS121
     END-IF.
*支払金額（税抜）、消費税、（税込）
     MOVE     ZERO                     TO   MS071.
     MOVE     ZERO                     TO   MS07.
     MOVE     ZERO                     TO   MS131.
     MOVE     ZERO                     TO   MS13.
     MOVE     ZERO                     TO   MS141.
     MOVE     ZERO                     TO   MS14.
*レコード区分内容
     MOVE     NC"【支払データ無】"     TO   MS08.
*データ内容
     MOVE     NC"※請求Ｄより※"       TO   MS09.
*検収日
     MOVE     SEI-F14                  TO   MS10.
*前回保留ＦＬＧ
     MOVE     SPACE                    TO   MS15.
*今回保留ＦＬＧ
     MOVE     SPACE                    TO   MS16.
*元伝票番号
     MOVE     SPACE                    TO   MS17.
*理由区分・理由コード
     MOVE     SPACE                    TO   MS18   MS19.
*
*相殺区分
     MOVE     SPACE                    TO   MS20.
*
* 2023/09/27 ADD START
*ＶＤ値引消費税
     MOVE     ZERO                     TO   MS21.
*標準税率当月相殺
     MOVE     ZERO                     TO   MS22.
*標準相殺消費税
     MOVE     ZERO                     TO   MS23.
*軽減税率当月相殺
     MOVE     ZERO                     TO   MS24.
*軽減相殺消費税
     MOVE     ZERO                     TO   MS25.
*登録番号
     MOVE     SPACE                    TO   MS26.
*元取引日
     MOVE     ZERO                     TO   MS27.
* 2023/09/27 ADD END
*明細行印字
     IF  SEI-F03  <  170
         MOVE     MEISAI01             TO   UWK-REC
         WRITE  UWK-REC
     ELSE
         MOVE     MEISAI01             TO   UW1-REC
         WRITE  UW1-REC
     END-IF.
*
     ADD      1                        TO   LINE-CNT.
*
 210-HENSYU3-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       テイル部印刷                                *
*--------------------------------------------------------------*
 TAIL-RTN               SECTION.
*
     MOVE     WK-CNT1        TO   TL-CNT1.
     MOVE     WK-CNT2        TO   TL-CNT2.
     MOVE     WK-CNT3        TO   TL-CNT3.
     MOVE     WK-CNT4        TO   TL-CNT4.
*
     MOVE     TAIL01         TO   UWK-REC.
*明細行印字
     WRITE  UWK-REC.
*
 TAIL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計Ｆ　　 READ                          *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
*
     IF       SEI-END   =   "END"
              GO        TO        900-SEI-READ-EXIT
     END-IF.
     READ     SETGK904   AT  END
              MOVE   HIGH-VALUE   TO   SEI-KEY
              MOVE   "END"        TO   SEI-END
              NOT AT END
              MOVE   SEI-F03      TO   SEI-TENPO
              MOVE   SEI-F05      TO   SEI-DENNO
              ADD    1            TO   SEI-CNT
     END-READ.
*
 900-SEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払合計ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SSI-READ           SECTION.
     IF       SSI-END   =   "END"
              GO        TO        900-SSI-READ-EXIT
     END-IF.
     READ     SITGKF90  AT  END
              MOVE   HIGH-VALUE   TO   SSI-KEY
              MOVE   "END"        TO   SSI-END
              NOT AT END
              MOVE   SSI-F03      TO   SSI-TENPO
              MOVE   SSI-F05      TO   SSI-DENNO
              ADD    1            TO   SII-CNT
     END-READ.
*
 900-SSI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     READ     HTENMS    INVALID
              MOVE      "INV"     TO   HTENMS-INV-FLG
              NOT  INVALID
              MOVE      SPACE     TO   HTENMS-INV-FLG
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     READ     HTOKMS    INVALID
              MOVE      "INV"     TO   HTOKMS-INV-FLG
              NOT  INVALID
              MOVE      SPACE     TO   HTOKMS-INV-FLG
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
