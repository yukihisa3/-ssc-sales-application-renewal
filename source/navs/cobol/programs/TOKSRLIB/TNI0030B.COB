****************************************************************
*新基幹システムへ移行します。 1999/10/16 T.TAKAHASHI
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　
*    モジュール名　　　　：　売上データ抽出　　　　　　　　　　
*    作成日／更新日　　　：　93/05/14   (SSKT160ﾖﾘ)
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　
*    処理概要　　　　　　：　伝票データより，売上データを出力　
*                        ：　する。　　　　　　　　　　　　　　
*    更新履歴            ：
*　　  94/08/02
*　　　　売上計上の対象データを入力日付と
*　　　　出荷日が一致したデータとする　　
*　　  99/05/28
*　　　　６月以降の大阪地方の売上を特販部
*　　　　から大阪営業所扱いにする。　　　
*　　  03/06/04
*　　　　６月以降の指定取引先の売上を各営
*　　　　業所扱いに変更する。
*　　  05/06/10
*　　　　部門変更に伴い，北海道売上の固定
*　　　　部門ＣＤを変更する。
*　　  05/12/23
*　　　　西日本取引先追加　　　　　　　　
*　　  06/09/12
*　　　　ホーマック植物追加　　　　　　　
*　　  06/10/05
*　　　　カーマ植物追加　　　　　　　　　
*　　  06/11/17
*　　　　サンワドー原価消費税抜き対応　　
*　　  07/05/10
*　　　　サンワドー北海道計上部門追加　　
*　　  08/07/01
*　　　　リック追加　　　　　　　　　　　
*　　  08/07/01
*　　　　オージョイフル追加　　　　　　　
*　　  08/08/20
*　　　　内部統制対応
*　　　　１：承認担当者＝空白の場合エラー
*　　　　２：担当者　Ｆ０６→Ｆ６０に変更
*　　  09/04/06
*　　　　ホームエキスポ→エイデンに変更　
*　　  09/08/03
*　　　　ダイキ植物追加　　　　　　　　　　
*　　  09/10/09
*　　　　ＰＣ実績連携関係処理追加　　　　　
*　　  10/02/08
*　　　　リック原価消費税抜き対応　　　　
*      2011/10/06  S.I/NAV  基幹サーバ統合
*      2011/11/17  T.MIURA  基幹サーバ統合
*      2012/04/05  T.T NAV  山新原価消費税抜き対応
*      2012/05/11  T.T NAV  アスタラビスタ原価消費税抜き対応
*      2014/01/06  T.T NAV  消費税増税対応
*      2020/04/23  T.T NAV  Ｄ３６５連携対応
*      2020/06/29  T.T NAV  Ｄ３６５連携対応（連携課題対応）
*      2020/11/27  T.T NAV  Ｄ３６５連携対応（顧客需要家対応）
*      2021/01/12  T.T NAV  Ｄ３６５連携対応（値引伝票対応）　　
*      2021/03/09  T.T NAV  Ｄ３６５連携対応（値引伝票ＮＯ対応）
*      2021/05/06  T.T NAV  Ｄ３６５連携対応（０伝票連携無対応）
*      2021/05/14  T.T NAV  Ｄ３６５連携対応（未計上対応）
*      2021/05/14  T.T NAV  Ｄ３６５連携対応（見本鉢無償品対応）
*      2021/06/01  T.T NAV  Ｄ３６５連携対応（計上需要家ＩＤ）　
*      2021/06/14  T.T NAV  Ｄ３６５連携対応（先行受注ＥＲ対応）
*      2021/07/20  T.T NAV  Ｄ３６５連携対応（数量＝０ＥＲ対応）
*      2021/08/03  T.T NAV  Ｄ３６５連携対応（先行数量＝０チェ）
*      2021/08/18  T.T NAV  Ｄ３６５連携対応（Ｄ４問題対応）　　
*      2022/02/28  T.T NAV  Ｄ３６５連携対応（先行受注実納品日）
*      2022/09/20  M.I NAV  変換エラーファイル出力追加
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            TNI0030B.
 AUTHOR.                T.A.
 DATE-WRITTEN.          93/05/14.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 伝票データ >>--*
     SELECT   HDENJNL   ASSIGN         DA-01-VI-DENJNL4
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                   *> 2011/10/06  S.I/NAV
                   *>     RECORD    KEY  DEN-F01   DEN-F02
                   *>                    DEN-F04   DEN-F03
                        RECORD    KEY  DEN-F01  DEN-F02
                                       DEN-F04  DEN-F07
                                       DEN-F112 DEN-F03
                   *> 2011/10/06  S.I/NAV
                        STATUS         HDENJNL-ST.
*----<< 条件ファイル >>--*
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01   JYO-F02
                        STATUS         HJYOKEN-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         HTENMS-ST.
*----<< 商品名称マスタ >>--*
     SELECT   HMEIMS    ASSIGN         DA-01-VI-MEIMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
*93/02******************RECORD    KEY  MEI-F01
                        RECORD    KEY  MEI-F011
                                       MEI-F0121
                                       MEI-F0122
                                       MEI-F0123
                        STATUS         HMEIMS-ST.
*----<< 商品変換テーブル >>--*
     SELECT   HSHOTBL   ASSIGN         DA-01-VI-SHOTBL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SHO-F01   SHO-F02
                        STATUS    HSHOTBL-ST1.
*----<< 売上データ >>--*
     SELECT   TOKU      ASSIGN         DA-01-S-TOKU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOKU-ST1.
*----<< 計上用売上データ >>--*
     SELECT   TOKUJ     ASSIGN         DA-01-S-TOKUJ
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOKUJ-ST1.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*# 2020/04/23 Ｄ３６５連携追加 ADD ST
*----<< SUB商品名称マスタ >>--*
     SELECT   SUBMEIF   ASSIGN         DA-01-VI-SUBMEIL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SBM-F011  SBM-F0121
                                       SBM-F0122 SBM-F0123
                        STATUS    SUBMEIF-ST1.
*----<< 売上受注調整計上データ >>--*
     SELECT   URIKEJF   ASSIGN         DA-01-VI-URIKEJL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  KEJ-F01   KEJ-F02
                                       KEJ-F03   KEJ-F04
                                       KEJ-F05   KEJ-F06
                                       KEJ-F07
                                       WITH  DUPLICATES
                        STATUS    URIKEJF-ST1.
*# 2020/04/23 Ｄ３６５連携追加 ADD ED
*#2020/12/01 NAV ST
*顧客需要家ＩＤ管理マスタ
     SELECT   KYKJYKF   ASSIGN    TO        DA-01-VI-KYKJYKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KYK-F01
                                            KYK-F02
                        FILE  STATUS   IS   KYKJYKF-ST.
*#2020/12/01 NAV ED
*2022/09/20↓
*---<<  日次更新計上エラーファイル  >>---*
     SELECT   DNIERRF   ASSIGN    TO        DA-01-VI-DNIERRL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   ERR-F01
                                                 ERR-F02
                                                 ERR-F03
                                                 ERR-F04
                                                 ERR-F07
                                                 ERR-F08
                                                 ERR-F11
                                                 ERR-F12
                                                 ERR-F06
                        FILE      STATUS    IS   DNIERRF-ST.
*2022/09/20↑
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 伝票データ >>--*
 FD  HDENJNL            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 条件ファイル >>--*
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 商品名称マスタ >>--*
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*----<< 商品変換テーブル >>--*
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*----<< 売上データ >>--*
 FD  TOKU               LABEL RECORD   IS   STANDARD.
     COPY     TOKUREC   OF        XFDLIB
              JOINING   TOKU      PREFIX.
*----<< 実績用売上データ >>--*
 FD  TOKUJ              LABEL RECORD   IS   STANDARD.
     COPY     TOKUJREC  OF        XFDLIB
              JOINING   TOKUJ     PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*# 2020/04/23 Ｄ３６５連携追加 ADD ST
**----<< SUB商品名称マスタ >>--*
 FD  SUBMEIF            LABEL RECORD   IS   STANDARD.
     COPY     SUBMEIF   OF        XFDLIB
              JOINING   SBM       PREFIX.
*----<< 売上受注調整計上データ >>--*
 FD  URIKEJF            LABEL RECORD   IS   STANDARD.
     COPY     URIKEJF   OF        XFDLIB
              JOINING   KEJ       PREFIX.
*# 2020/04/23 Ｄ３６５連携追加 ADD ED
*#2020/12/01 NAV ST
******************************************************************
*    顧客需要家ＩＤ管理マスタ
******************************************************************
 FD  KYKJYKF            LABEL RECORD   IS   STANDARD.
     COPY     KYKJYKF   OF        XFDLIB
              JOINING   KYK       PREFIX.
*#2020/12/01 NAV ED
*2022/09/20↓
*---<<  オンライン伝票変換エラーファイル  >>---*
 FD  DNIERRF.
     COPY     DNIERRF   OF        XFDLIB
              JOINING   ERR       PREFIX.
*2022/09/20↑
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01).
     03  MEI-INV-FLG    PIC  9(01).
     03  SHO-INV-FLG    PIC  9(01).
     03  TEN-INV-FLG    PIC  9(01).
     03  TOK-INV-FLG    PIC  9(01).
     03  JYO-INV-FLG    PIC  9(01).
     03  ONL-FLG        PIC  9(01).
     03  TAN-ERR-FLG    PIC  9(01).
     03  TEI-FLG        PIC  9(01).
     03  SYONIN-FLG     PIC  9(01).
     03  SBM-INV-FLG    PIC  9(01).
     03  KEJ-INV-FLG    PIC  9(01).
*#2020/06/30 NAV ST 在庫調整ＦＬＧチェック用
     03  ZTS-INV-FLG    PIC  9(01).
*#2020/06/30 NAV ED 在庫調整ＦＬＧチェック用
*#2021/05/06 NAV ST 先行受注分売上時数量＝０対応
     03  SENKOU-CHK-FLG PIC  9(01).
*#2021/05/06 NAV ED 先行受注分売上時数量＝０対応
*#2020/12/01 NAV ST
     03  KYK-INV-FLG    PIC  X(03).
*#2020/12/01 NAV ED
 01  COUNTERS.
     03  IN-CNT         PIC  9(07).
     03  OUT-CNT        PIC  9(07).
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
     03  CNT            PIC  9(06).
 01  INDEXES.
     03  I              PIC  9(03).
 01  TANTO-WORK.
     03  TAN-OLD        PIC  9(02).
     03  TAN-NEW        PIC  9(02).
 01  WK-TANCD           PIC  9(02).
 01  WK-DEN-F45         PIC  9(08).
 01  WK-DEN-F15         PIC  9(08)   VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  HDENJNL-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  HJYOKEN-ST        PIC  X(02).
 01  HTENMS-ST         PIC  X(02).
 01  HMEIMS-ST         PIC  X(02).
 01  HSHOTBL-ST.
     03  HSHOTBL-ST1   PIC  X(02).
     03  HSHOTBL-ST2   PIC  X(04).
 01  TOKU-ST.
     03  TOKU-ST1      PIC  X(02).
     03  TOKU-ST2      PIC  X(04).
 01  TOKUJ-ST.
     03  TOKUJ-ST1      PIC  X(02).
     03  TOKUJ-ST2      PIC  X(04).
 01  SUBMEIF-ST.
     03  SUBMEIF-ST1    PIC  X(02).
     03  SUBMEIF-ST2    PIC  X(04).
 01  URIKEJF-ST.
     03  URIKEJF-ST1    PIC  X(02).
     03  URIKEJF-ST2    PIC  X(04).
 01  KYKJYKF-ST        PIC  X(02).
*2022/09/20↓
 01  DNIERRF-ST         PIC  X(02).
*2022/09/20↑
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  WK-NOUHIN          PIC  9(08).
 01  FILLER             REDEFINES      WK-NOUHIN.
     03  WK-YYYY        PIC  9(02).
     03  WK-YMD         PIC  9(06).
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
*01  SYS-DATE           PIC  9(06).
*01  FILLER             REDEFINES      SYS-DATE.
*    03  SYS-YY         PIC  9(02).
*    03  SYS-MM         PIC  9(02).
*    03  SYS-DD         PIC  9(02).
*01  SYS-TIME           PIC  9(08).
*01  FILLER             REDEFINES      SYS-TIME.
*    03  SYS-HH         PIC  9(02).
*    03  SYS-MN         PIC  9(02).
*    03  SYS-SS         PIC  9(02).
*    03  SYS-MS         PIC  9(02).
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
 01  WK-TOKCD           PIC  9(08).
 01  CYU-DATE           PIC  9(08).
 01  FILLER             REDEFINES      CYU-DATE.
     03  CYU-YY         PIC  9(04).
     03  CYU-MM         PIC  9(02).
     03  CYU-DD         PIC  9(02).
 01  NOU-DATE           PIC  9(08).
 01  FILLER             REDEFINES      NOU-DATE.
     03  NOU-YY         PIC  9(04).
     03  NOU-MM         PIC  9(02).
     03  NOU-DD         PIC  9(02).
 01  SYU-DATE           PIC  9(08).
 01  FILLER             REDEFINES      SYU-DATE.
     03  SYU-YY         PIC  9(04).
     03  SYU-MM         PIC  9(02).
     03  SYU-DD         PIC  9(02).
 01  CNV-YY             PIC  9(04)     VALUE  0.
 01  ACOS-MM            PIC  9(02)     VALUE  0.
*計上部門
 01  WK-BUMON           PIC  9(04)     VALUE  ZERO.
*サンワドー消費税計算
 01  WK-UTIZEI.
     03  WK-SYOHIZEI-RITU   PIC  9(01)V9(02) VALUE  ZERO.
     03  WK-ZEINUKI         PIC S9(09)     VALUE  ZERO.
     03  WK-GENKA1          PIC S9(09)     VALUE  ZERO.
     03  WK-GENKA2          PIC S9(09)     VALUE  ZERO.
*----<< ﾌﾞﾚｲｸ ﾜｰｸ >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-F01    PIC  9(08).
         05  NEW-F02    PIC  9(09).
     03  OLD.
         05  OLD-F01    PIC  9(08).
         05  OLD-F02    PIC  9(09).
*
 01  WK-TOK-F76             PIC  9(08)   VALUE  ZERO.
 01  WK-DEN-F113            PIC  9(08)   VALUE  ZERO.
*#2021/01/12 NAV ST　値引き伝票オーダー区分退避
 01  WK-ORDER-KBN           PIC  X(03)   VALUE  SPACE.
*#2021/01/12 NAV ED　値引き伝票オーダー区分退避
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
*### 99/05/28 NAV START ###*
*日付比較用ワーク
 01  WK-DATE-NONYU-SYUKA.
     03  WK-SYU         PIC  9(08)  VALUE  ZERO.
*### 99/05/28 NAV END   ###*
*2022/09/20↓
     03  ER01                PIC  9(01)  VALUE ZERO.
     03  ER02                PIC  9(01)  VALUE ZERO.
     03  ER03                PIC  9(01)  VALUE ZERO.
     03  ER04                PIC  9(01)  VALUE ZERO.
     03  ER05                PIC  9(01)  VALUE ZERO.
     03  ER06                PIC  9(01)  VALUE ZERO.
     03  ER07                PIC  9(01)  VALUE ZERO.
     03  ER08                PIC  9(01)  VALUE ZERO.
     03  ER09                PIC  9(01)  VALUE ZERO.
     03  ER10                PIC  9(01)  VALUE ZERO.
     03  ER11                PIC  9(01)  VALUE ZERO.
     03  ER12                PIC  9(01)  VALUE ZERO.
     03  ER13                PIC  9(01)  VALUE ZERO.
     03  ER14                PIC  9(01)  VALUE ZERO.
     03  ER15                PIC  9(01)  VALUE ZERO.
*2022/09/20↑
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(40)     VALUE  SPACE.
         05  FILLER     PIC  N(18)     VALUE
              NC"【　売上データ　変換エラーリスト　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"処理日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-011     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-02      PIC  ZZ9.
 01  HEAD02.
     03  FILLER         CHARACTER TYPE MODE-2.
*#2020/06/30 NAV ST 在庫調整ＦＬＧチェック追加
*********05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
*#2020/06/30 NAVEDT 在庫調整ＦＬＧチェック追加
         05  FILLER     PIC  N(03)     VALUE  NC"伝票_".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"店舗".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"店　舗　名".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"取引先商品".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"商品・品単".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"商　品　名".
         05  FILLER     PIC  X(22)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(01)     VALUE  NC"１".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"２".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"３".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"４".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"５".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"６".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"７".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"８".
*#2020/06/30 NAV ST 在庫調整ＦＬＧチェック追加
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"９".
*#2020/06/30 NAV ED 在庫調整ＦＬＧチェック追加
*#2020/12/01 NAV ST 顧客需要家ＩＤチェック追加
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"０".
*#2020/12/01 NAV ED 顧客需要家ＩＤチェック追加
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(111)    VALUE  SPACE.
         05  FILLER     PIC  N(12)     VALUE
             NC"＜＜　エラー情報　　＞＞".
 01  HEAD04.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  X(03)     VALUE  "ｴﾗｰ".
         05  FILLER     PIC  N(04)     VALUE  NC"情報→「".
         05  FILLER     PIC  X(02)     VALUE  "1:".
         05  FILLER     PIC  N(04)     VALUE  NC"商品変換".
         05  FILLER     PIC  X(03)     VALUE  "TBL".
         05  FILLER     PIC  N(03)     VALUE  NC"未登録".
         05  FILLER     PIC  X(03)     VALUE  " 2:".
         05  FILLER     PIC  N(04)     VALUE  NC"商品変換".
         05  FILLER     PIC  X(03)     VALUE  "TBL".
         05  FILLER     PIC  N(03)     VALUE  NC"不一致".
         05  FILLER     PIC  X(03)     VALUE  " 3:".
         05  FILLER     PIC  N(02)     VALUE  NC"商品".
         05  FILLER     PIC  X(01)     VALUE  "M".
         05  FILLER     PIC  N(03)     VALUE  NC"未登録".
         05  FILLER     PIC  X(03)     VALUE  " 4:".
         05  FILLER     PIC  N(02)     VALUE  NC"店舗".
         05  FILLER     PIC  X(01)     VALUE  "M".
         05  FILLER     PIC  N(03)     VALUE  NC"未登録".
         05  FILLER     PIC  X(06)     VALUE  " 5:SUB".
         05  FILLER     PIC  N(02)     VALUE  NC"商品".
         05  FILLER     PIC  N(03)     VALUE  NC"未登録".
         05  FILLER     PIC  X(03)     VALUE  " 6:".
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(01)     VALUE  "M".
         05  FILLER     PIC  N(03)     VALUE  NC"未登録".
*#2021/05/06 NAV ST
*********05  FILLER     PIC  X(03)     VALUE  " 7:".
*********05  FILLER     PIC  N(04)     VALUE  NC"部門取得".
*********05  FILLER     PIC  X(03)     VALUE  "ｴﾗｰ".
         05  FILLER     PIC  X(03)     VALUE  " 7:".
         05  FILLER     PIC  N(05)     VALUE  NC"先行分数量".
         05  FILLER     PIC  X(03)     VALUE  "ｴﾗｰ".
*#2021/05/06 NAV ED
         05  FILLER     PIC  X(03)     VALUE  " 8:".
         05  FILLER     PIC  N(03)     VALUE  NC"税区分".
         05  FILLER     PIC  X(03)     VALUE  "ｴﾗｰ".
         05  FILLER     PIC  X(03)     VALUE  " 9:".
*#2020/06/30 NAV ST 在庫調整ＦＬＧチェック追加
         05  FILLER     PIC  N(03)     VALUE  NC"在調戻".
         05  FILLER     PIC  X(03)     VALUE  "ｴﾗｰ".
*#2020/06/30 NAV ED 在庫調整ＦＬＧチェック追加
*#2020/12/01 NAV ST 顧客需要家ＩＤチェック追加
         05  FILLER     PIC  X(03)     VALUE  " 0:".
         05  FILLER     PIC  N(02)     VALUE  NC"顧需".
         05  FILLER     PIC  X(05)     VALUE  "IDｴﾗｰ".
*#2020/12/01 NAV ED 顧客需要家ＩＤチェック追加
         05  FILLER     PIC  N(01)     VALUE  NC"」".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
*#2020/06/30 NAV ST 在庫調整ＦＬＧチェック追加
*********05  FILLER     PIC  X(05).
*#2020/12/01 NAV ST 顧客需要家ＩＤチェック追加
*********05  FILLER     PIC  X(02).
         05  FILLER     PIC  X(01).
*#2020/12/01 NAV ED 顧客需要家ＩＤチェック追加
*#2020/06/30 NAV ST 在庫調整ＦＬＧチェック追加
         05  ME-03      PIC  X(09).
         05  FILLER     PIC  X(02).
         05  ME-04      PIC  9(08).
         05  FILLER     PIC  X(03).
         05  ME-05      PIC  9(05).
         05  FILLER     PIC  X(02).
         05  ME-06      PIC  N(10).
         05  FILLER     PIC  X(02).
         05  ME-07      PIC  X(13).
         05  FILLER     PIC  X(02).
         05  ME-07A     PIC  X(16).
         05  FILLER     PIC  X(02).
*#2021/06/21 NAV ST
***      05  ME-08      PIC  X(30).
***      05  FILLER     PIC  X(02).
         05  ME-08      PIC  X(20).    *>商品名カナ
         05  FILLER     PIC  X(01).
         05  ME-081     PIC  X(01).    *> (
         05  ME-082     PIC  ZZZZZZZ9. *>数量
         05  ME-083     PIC  X(01).    *> )
         05  FILLER     PIC  X(01).
*#2021/06/21 NAV ED
         05  ME-091     PIC  N(01).
         05  FILLER     PIC  X(01).
         05  ME-092     PIC  N(01).
         05  FILLER     PIC  X(01).
         05  ME-093     PIC  N(01).
         05  FILLER     PIC  X(01).
         05  ME-094     PIC  N(01).
         05  FILLER     PIC  X(01).
         05  ME-095     PIC  N(01).
         05  FILLER     PIC  X(01).
         05  ME-096     PIC  N(01).
         05  FILLER     PIC  X(01).
         05  ME-097     PIC  N(01).
         05  FILLER     PIC  X(01).
         05  ME-098     PIC  N(01).
*#2020/06/30 NAV ST 在庫調整ＦＬＧチェック追加
         05  FILLER     PIC  X(01).
         05  ME-099     PIC  N(01).
*#2020/06/30 NAV ED 在庫調整ＦＬＧチェック追加
*#2020/12/01 NAV ST 顧客需要家ＩＤチェック追加
         05  FILLER     PIC  X(01).
         05  ME-09A     PIC  N(01).
*#2020/12/01 NAV ED 顧客需要家ＩＤチェック追加
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
*--<< ｻﾌﾞﾙｰﾁﾝ ﾊﾟﾗﾒﾀ >>-*
 01  SSKTDTCK-PARA.
     03  SSKTDTCK-YMD        PIC  9(06).
     03  FILLER              REDEFINES SSKTDTCK-YMD.
         05  SSKTDTCK-Y      PIC  9(02).
         05  SSKTDTCK-M      PIC  9(02).
         05  SSKTDTCK-D      PIC  9(02).
     03  SSKTDTCK-RET        PIC  9(01).
*##2014/01/06 NAV ST 消費税増税対応
*取引先マスタ　税抜計算区分項目退避エリア
 01  WK-ZEINUKI-KBN.
     03  WK-ZEINUKI-KBN1     PIC  9(01)  VALUE  ZERO.
     03  WK-ZEINUKI-KBN2     PIC  9(08)  VALUE  ZERO.
     03  WK-ZEINUKI-KBN3     PIC  9(01)  VALUE  ZERO.
 01  WK-ZEINUKI-CHK          PIC  9(01)  VALUE  ZERO.
*##2014/01/06 NAV ST 税区分判定チェック
 01  WK-ZEIKBN               PIC  X(01)  VALUE  SPACE.
*##2014/01/06 NAV ED 税区分判定チェック
*条件ファイル格納領域
*****消費税率レコード退避
     COPY   HJYOKEN  OF XFDLIB  JOINING   JBK  AS   PREFIX.
*****税区分レコード退避
     COPY   HJYOKEN  OF XFDLIB  JOINING   JZK  AS   PREFIX.
*##2014/01/06 NAV ED
*# 2020/04/24 NAV ST
*受注売上調整計上データワーク
     COPY   URIKEJF  OF XFDLIB  JOINING   WKJ  AS   PREFIX.
*# 2020/04/24 NAV ST
 01  WK-KOUSIN-KBN           PIC  X(01)  VALUE  SPACE.
 01  WK-ZEI-KBN              PIC  X(01)  VALUE  SPACE.
 01  KEJ-REWT-CNT            PIC  9(07)  VALUE  ZERO.
 01  KEJ-WT-CNT              PIC  9(07)  VALUE  ZERO.
*2022/09/20↓
 01  DNI-WT-CNT              PIC  9(07)  VALUE  ZERO.
*2022/09/20↑
*# 2020/04/24 NAV ED
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                   SECTION.
 01  PARA-JKDATE           PIC 9(08).
 01  PARA-JKTIME           PIC 9(06).
****************************************************************
 PROCEDURE              DIVISION USING PARA-JKDATE PARA-JKTIME.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 伝票データ >>--*
 HDENJNL-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HDENJNL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TNI0030B HDENJNL ERROR " HDENJNL-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU TOKUJ.
     CLOSE    HDENJNL   HTOKMS    HTENMS    SUBMEIF  URIKEJF.
     CLOSE    DNIERRF.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 条件ファイル >>--*
 HJYOKEN-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TNI0030B HJYOKEN ERROR " HJYOKEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU TOKUJ.
     CLOSE    HDENJNL   HTOKMS    HTENMS    SUBMEIF  URIKEJF.
     CLOSE    DNIERRF.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TNI0030B HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU.
     CLOSE    HDENJNL   HTOKMS    HTENMS    SUBMEIF  URIKEJF.
     CLOSE    DNIERRF.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TNI0030B HTENMS ERROR " HTENMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU TOKUJ.
     CLOSE    HDENJNL   HTOKMS    HTENMS   SUBMEIF  URIKEJF.
     CLOSE    DNIERRF.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 商品名称マスタ >>--*
 HMEIMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HMEIMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TNI0030B HMEIMS ERROR " HMEIMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU TOKUJ.
     CLOSE    HDENJNL   HTOKMS    HTENMS    SUBMEIF  URIKEJF.
     CLOSE    DNIERRF.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 商品変換テーブル >>--*
 HSHOTBL-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHOTBL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TNI0030B HSHOTBL ERROR " HSHOTBL-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU TOKUJ.
     CLOSE    HDENJNL   HTOKMS    HTENMS    SUBMEIF  URIKEJF.
     CLOSE    DNIERRF.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 売上データ >>--*
 TOKU-ERR               SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOKU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TNI0030B TOKU ERROR " TOKU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU.
     CLOSE    HDENJNL   HTOKMS    HTENMS    SUBMEIF  URIKEJF.
     CLOSE    DNIERRF.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 実績用売上データ >>--*
 TOKUJ-ERR               SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOKUJ.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TNI0030B TOKUJ ERROR " TOKUJ-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU TOKUJ.
     CLOSE    HDENJNL   HTOKMS    HTENMS    SUBMEIF  URIKEJF.
     CLOSE    DNIERRF.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*# 2020/04/23 NAV ST
*----<< SUB商品名称マスタ >>--*
 SBM-ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SUBMEIF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TNI0030B SUBMEIF ERROR " SUBMEIF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU TOKUJ.
     CLOSE    HDENJNL   HTOKMS    HTENMS    SUBMEIF  URIKEJF.
     CLOSE    DNIERRF.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 売上受注調整計上データ >>--*
 SBM-ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      URIKEJF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TNI0030B URIKEIJF ERROR " URIKEJF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU TOKUJ.
     CLOSE    HDENJNL   HTOKMS    HTENMS    SUBMEIF  URIKEJF.
     CLOSE    DNIERRF.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*# 2020/04/23 NAV ED
*# 2020/12/01 NAV ST
*----<< 顧客需要家ＩＤ管理マスタ >>--*
 KYK-ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KYKJYKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TNI0030B KUKJYKL1 ERROR " KYKJYKF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU TOKUJ.
     CLOSE    HDENJNL   HTOKMS    HTENMS    SUBMEIF  URIKEJF.
     CLOSE    DNIERRF.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*# 2020/12/01 NAV ED
*2022/09/20↓
*----<< 日次更新計上エラーファイル >>--*
 ERR-ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DNIERRF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TNI0030B DNIERRL1 ERROR " DNIERRF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU TOKUJ.
     CLOSE    HDENJNL   HTOKMS    HTENMS    SUBMEIF  URIKEJF.
     CLOSE    DNIERRF.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*2022/09/20↑
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO     =    99.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     SYS-TIME       TO   SYS-TIME2.
     MOVE     SYS-TIMEW      TO   PARA-JKTIME.
     DISPLAY  "*** TNI0030B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD   TO   SYS-DATEW PARA-JKDATE
     ELSE
         MOVE ZERO           TO   SYS-DATEW PARA-JKDATE
     END-IF.
*
     OPEN     I-O       HDENJNL.
     OPEN     INPUT     HJYOKEN.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     INPUT     HMEIMS.
     OPEN     INPUT     HSHOTBL.
     OPEN     OUTPUT    TOKU.
     OPEN     OUTPUT    TOKUJ.
     OPEN     I-O       URIKEJF.
     OPEN     INPUT     SUBMEIF.
     OPEN     INPUT     KYKJYKF.
*2022/09/20↓
     OPEN     I-O       DNIERRF.
*2022/09/20↑
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     0         TO   GR-NO.
*----<< ｼｽﾃﾑﾋﾂﾞｹﾍﾝｶﾝ >>-*
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-YYMD.
*#2021/01/12 NAV ST
*----<< 値引伝票　オーダー区分取得退避 >>-*
     MOVE     27        TO   JYO-F01.
     MOVE     "NEBIKI"  TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG   =    0
              MOVE      JYO-F14(1:3)   TO  WK-ORDER-KBN
         DISPLAY "## " NC"値引ＯＲＤ区分"  NC"取得" "OK]]"
         UPON CONS
         DISPLAY "## " NC"値引ＯＲＤ区分" " = " WK-ORDER-KBN
         UPON CONS
     ELSE
         DISPLAY "## " NC"値引ＯＲＤ区分"  NC"取得" "NG]] ##"
         UPON CONS
         DISPLAY "## HJYOKEN ORDER INVALID" JYO-F01 " - " JYO-F02
         " ##" UPON CONS
              MOVE     "D99"           TO  WK-ORDER-KBN
     END-IF.
*#2021/01/12 NAV ED
     MOVE     58        TO   JYO-F01.
     MOVE     SPACE     TO   JYO-F02.
     PERFORM  900-JYO-READ.
*----<< ﾌﾞﾓﾝｺｰﾄﾞ ｼｭﾄｸ >>-*
     MOVE    99              TO   JYO-F01.
     MOVE    "BUMON"         TO   JYO-F02.
     READ    HJYOKEN
             INVALID
             DISPLAY "HJYOKEN BUMON INVALID" JYO-F01 " - " JYO-F02
                      UPON CONS
                      STOP RUN
             NOT  INVALID
             MOVE     JYO-F04     TO   WK-BUMON
     END-READ.
*----<< 日次更新条件取得 >>-*
     MOVE     99        TO   JYO-F01.
     MOVE     "DAY"     TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG   =    0
              MOVE      JYO-F04   TO   WK-TOKCD
              MOVE      JYO-F05   TO   CYU-DATE
              MOVE      JYO-F06   TO   NOU-DATE
              MOVE      JYO-F07   TO   SYU-DATE
     END-IF.
*----<< 消費税率取得 >>-*
     MOVE     99        TO   JYO-F01.
     MOVE     "ZEI"     TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG   =    0
              MOVE      JYO-F04   TO   WK-SYOHIZEI-RITU
**************2014/01/06 NAV ST 消費税率レコードをワークに退避
              MOVE      JYO-REC   TO   JBK-REC
**************2014/01/06 NAV ED
     END-IF.
     IF      INV-FLG         =    1
     OR      ERR-FLG    NOT  =    0
             MOVE       99        TO   GR-NO
             DISPLAY NC"＃＃日次異常発生　条件Ｍ＃＃" UPON CONS
     END-IF.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     OPEN     OUTPUT    PRTF.
     MOVE     99             TO   LINE-CNT.
     MOVE     0              TO   PAGE-CNT.
     MOVE     0              TO   ERR-FLG.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
*
     MOVE     ZERO           TO   DEN-F01.
     MOVE     ZERO           TO   DEN-F02.
     MOVE     ZERO           TO   DEN-F04.
     MOVE     ZERO           TO   DEN-F03.
     MOVE     WK-TOKCD       TO   DEN-F01.
* 2011/10/06,S  S.I/NAV
     MOVE  ZERO             TO  DEN-F07  *> 店舗CD
     MOVE  ZERO             TO  DEN-F112 *> 納品日
* 2011/10/06,E  S.I/NAV
     PERFORM  900-DEN-START-READ.
     PERFORM  241-OUT-MAIN
                        UNTIL     OLD  =    HIGH-VALUE.
     CLOSE    PRTF.
*
     MOVE     99             TO   GR-NO.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HDENJNL.
     CLOSE    HJYOKEN.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
     CLOSE    HMEIMS.
     CLOSE    HSHOTBL.
     CLOSE    TOKU.
     CLOSE    TOKUJ.
     CLOSE    SUBMEIF.
     CLOSE    URIKEJF.
     CLOSE    KYKJYKF.
*2022/09/20↓
     CLOSE    DNIERRF.
*2022/09/20↑
*
     DISPLAY  "+++ TNI0030B ｾﾝﾀｸ  ｹﾝｽｳ = " IN-CNT " +++"
                                       UPON CONS.
     DISPLAY  "+++ TNI0030B ｼｭﾂﾘｮｸｹﾝｽｳ = " OUT-CNT " +++"
                                       UPON CONS.
     DISPLAY  "+++ TNI0030B URIKEJF WT = " KEJ-REWT-CNT " +++"
                                       UPON CONS.
     DISPLAY  "+++ TNI0030B URIKEJF RW = " KEJ-WT-CNT " +++"
                                       UPON CONS.
*2022/09/20↓
     DISPLAY  "+++ TNI0030B DNIERRF WT = " DNI-WT-CNT " +++"
                                       UPON CONS.
*2022/09/20↑
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** TNI0030B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｳﾘｱｹﾞﾃﾞｰﾀ ｼｭﾂﾘｮｸ                            *
*--------------------------------------------------------------*
 241-OUT-MAIN           SECTION.
*---<< ﾚｺｰﾄﾞ ｶｲﾎｳ >>---*
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   900-DEN-REWRITE
              ADD       1         TO   IN-CNT
     END-IF.
*---<< ﾃﾞｰﾀ ｼｭﾂﾘｮｸ ｵﾖﾋﾞ ﾌﾗｸﾞｾｯﾄ >>---*
     IF       NEW  NOT  =    OLD
     AND      OLD  NOT  =    LOW-VALUE
     AND      ERR-FLG   =    0
              MOVE OLD-F01   TO   DEN-F01   NEW-F01
              MOVE OLD-F02   TO   DEN-F02   NEW-F02
              MOVE ZERO      TO   DEN-F04
              MOVE ZERO      TO   DEN-F03
         *> 2011/10/06,S  S.I/NAV
              MOVE  ZERO    TO  DEN-F07  *> 店舗CD
              MOVE  ZERO    TO  DEN-F112 *> 納品日
         *> 2011/10/06,E  S.I/NAV
              PERFORM   900-DEN-START-READ
              PERFORM   2411-TOKU-OUTPUT
                             UNTIL     NEW  NOT  =    OLD
              MOVE ZERO      TO   ONL-FLG TAN-ERR-FLG TEI-FLG
     END-IF.
     IF       NEW  NOT  =    OLD
              MOVE 0         TO   ERR-FLG
     END-IF.
*---<< ｴﾗｰ ﾁｪｯｸ >>---*
     IF       NEW  NOT  =    HIGH-VALUE
     AND      DEN-F03   NOT  =    80   AND  90
              MOVE      DEN-F01        TO   TEN-F52
              MOVE      DEN-F07        TO   TEN-F011
*##  2009/10/09 NAV T.T 追加 ST
**************取引先Ｍチェック
              MOVE      DEN-F01        TO   TOK-F01
              PERFORM   900-TOK-READ
              IF   TOK-INV-FLG = 0
                   IF   WK-TOK-F76  >  ZERO
                        IF   TOK-F77  =  ZERO
                             MOVE  2   TO   TOK-INV-FLG
                        END-IF
                   ELSE
                        IF   TOK-F75  =  ZERO
                             MOVE  2   TO   TOK-INV-FLG
                        END-IF
                   END-IF
              END-IF
*##  2009/10/09 NAV T.T 追加 ED
*\\  93.07.10
              MOVE      ZERO           TO   TEN-INV-FLG
**************店舗マスタチェック
              IF        DEN-F07        NOT =    ZERO
                        PERFORM   900-TEN-READ
              END-IF
*\\  93.07.10  END
**************顧客需要家ＩＤ管理マスタチェック追加
**************# 2020/12/01 NAV ST
              MOVE      ZERO           TO   KYK-INV-FLG
              MOVE      DEN-F01        TO   KYK-F01
              MOVE      DEN-F07        TO   KYK-F02
              PERFORM   900-KYK-READ
**************# 2020/12/01 NAV ED
**************商品名称マスタチェック
              MOVE      DEN-F141       TO   MEI-F01
              PERFORM   900-MEI-READ
**************ＳＵＢ商品名称マスタチェック追加
**************# 2020/04/24 NAV ST
              MOVE      ZERO           TO   SBM-INV-FLG
              MOVE      DEN-F1411      TO   SBM-F011
              MOVE      DEN-F1412      TO   SBM-F012
              PERFORM   900-SBM-READ
****2021/07/20 NAV ST 数量＝０の時はＩＮＶでもエラーとしない
              IF  SBM-INV-FLG  =  1
                  IF  DEN-F15  =  ZERO
                      MOVE  ZERO       TO   SBM-INV-FLG
                  END-IF
              END-IF
****2021/07/20 NAV ED 数量＝０の時はＩＮＶでもエラーとしない
**************商品変換ＴＢＬチェック
              MOVE 0         TO   SHO-INV-FLG
              IF   DEN-F25   NOT  =    SPACE
                   MOVE      DEN-F01        TO   SHO-F01
                   MOVE      DEN-F25        TO   SHO-F02
                   PERFORM   900-SHO-READ
              END-IF
**************2009/10/09 NAV T.T ST
**************仕入単価＝０の時、変換ＴＢＬの仕入単価
**************　　　　　　　　　商品名称Ｍの仕入単価とセット
              IF  SHO-INV-FLG  =  0
                  IF  DEN-F171 <=  0
                      IF  SHO-F09  NOT =  ZERO
                          MOVE  SHO-F09  TO  DEN-F171
                      ELSE
                          MOVE  MEI-F041 TO  DEN-F171
                      END-IF
                  END-IF
              END-IF
**************2009/10/09 NAV T.T ED
**************2008/08/20 内部統制対応追加（承認済みチェック）
              MOVE      ZERO           TO   SYONIN-FLG
              IF        DEN-F61  =  SPACE
                        MOVE   1       TO   SYONIN-FLG
              END-IF
*##2013/01/06 NAV ST  税区分存在チェック
              IF        DEN-F34 = SPACE OR "0"
                        MOVE   0       TO   JYO-INV-FLG
              ELSE
                        MOVE   50      TO   JYO-F01
                        MOVE   DEN-F34 TO   JYO-F02
                        PERFORM  900-JYO-READ
                        IF     INV-FLG = 0
                               MOVE  0 TO   JYO-INV-FLG
                        ELSE
                               MOVE  1 TO   JYO-INV-FLG
                        END-IF
              END-IF
*##2013/01/06 NAV ED  税区分存在チェック
*##2020/06/30 NAV ST  在庫調整ＦＬＧチェック　
              IF       DEN-D93  =  "1"
                       MOVE         1       TO   ZTS-INV-FLG
              ELSE
                       MOVE         0       TO   ZTS-INV-FLG
***********************#2021/06/14 NAV ST
                       IF       DEN-F32  NOT =  SPACE
                       AND      DEN-F33  =      SPACE
*###2021/08/03 NAV ST 下記を条件に復活　　　　
***********************AND      DEN-F15  >      ZERO
                       AND      DEN-F15  >      ZERO
*###2021/08/03 NAV ED 下記を条件に復活　　　　
                                MOVE  1     TO   ZTS-INV-FLG
                       END-IF
***********************#2021/06/14 NAV ED
              END-IF
*##2020/06/30 NAV ED  在庫調整ＦＬＧチェック　
*##2021/05/06 NAV ST
              IF   DEN-F02  =  5163121
                   DISPLAY "DEN-F15 = " DEN-F15  UPON CONS
                   DISPLAY "DEN-D86 = " DEN-D86  UPON CONS
                   DISPLAY "DEN-D88 = " DEN-D88  UPON CONS
              END-IF
              IF       DEN-F15  =  ZERO
              AND      DEN-D86     NUMERIC
              AND      DEN-D86  >  ZERO
              AND      DEN-D88  =  "1"
***********************MOVE NC"７"    TO   ME-097
                       MOVE 1         TO   SENKOU-CHK-FLG
              ELSE
                       MOVE ZERO      TO   SENKOU-CHK-FLG
              END-IF
*##2021/05/06 NAV ED
**************
              IF        TEN-INV-FLG    =    1
              OR        MEI-INV-FLG    =    1
              OR        SHO-INV-FLG    =    1
**************2008/08/20 承認担当者のﾁｪｯｸ追加
              OR        SYONIN-FLG     =    1
**************
**************2009/10/09 エラー追加
              OR        SHO-INV-FLG    =    2
              OR        TOK-INV-FLG    =    1
              OR        TOK-INV-FLG    =    2
**************
*##2013/01/06 NAV ST 条件Ｆチェック
              OR        JYO-INV-FLG    =    1
*##2013/01/06 NAV ED 条件Ｆチェック
*##2020/04/24 NAV ST ＳＵＢ商品名称マスタチェック
              OR        SBM-INV-FLG    =    1
*##2020/04/24 NAV ED ＳＵＢ商品名称マスタチェック
*#2020/06/30 NAV ST 在庫調整ＦＬＧチェック追加
              OR        ZTS-INV-FLG    =    1
*#2020/06/30 NAV ST 在庫調整ＦＬＧチェック追加
*#2020/12/01 NAV ST 顧客需要家ＩＤ管理マスタチェック追加
              OR        KYK-INV-FLG    =    1
*#2020/12/01 NAV ST 顧客需要家ＩＤ管理マスタチェック追加
*#2021/05/06 NAV ST 先行受注分売上時数量＝０対応
              OR     SENKOU-CHK-FLG    =    1
*#2021/05/06 NAV ST 先行受注分売上時数量＝０対応
                        MOVE      1         TO   ERR-FLG
                        PERFORM   2412-ERROR-PRINT
              END-IF
*             ｵﾝﾗｲﾝ区分=1の場合
              IF   DEN-F274  =  1
                   IF     ONL-FLG  =  ZERO
                          MOVE DEN-F06  TO  TAN-OLD
                   END-IF
                   MOVE   1       TO    ONL-FLG
*                  伝票内担当者比較
                   IF   TAN-OLD  NOT =  TAN-NEW
                        MOVE   1  TO    TAN-ERR-FLG
                   END-IF
*                  数量訂正ﾁｪｯｸ
                   IF   DEN-F15  NOT =  DEN-F50
                        MOVE   1  TO    TEI-FLG
                   END-IF
              END-IF
     END-IF.
*
     MOVE     NEW       TO   OLD.
     MOVE     TAN-NEW   TO   TAN-OLD.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   900-DEN-READ
     END-IF.
 241-OUT-MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      ｳﾘｱｹﾞﾃﾞｰﾀ ｼｭﾂﾘｮｸ                            *
*--------------------------------------------------------------*
 2411-TOKU-OUTPUT       SECTION.
*売上作成ＦＬＧセット
     MOVE     9         TO  DEN-F277.
*売上計上日セット
     MOVE     DEN-F45   TO  WK-DEN-F45.
     MOVE     SYS-YYMD  TO  DEN-F45.
     IF       DEN-F113  =        ZERO
        IF    SYU-DATE  NOT  =    0
              MOVE      SYU-DATE  TO   DEN-F113
         ELSE
              MOVE      DEN-F112  TO   DEN-F113
         END-IF
     END-IF.
*担当者チェック
     IF       ONL-FLG     = 1
     AND      TAN-ERR-FLG = 1
              IF  TAN-OLD = 90 OR 99
                  IF  TEI-FLG = 1
**********************MOVE  90  TO  WK-TANCD DEN-F06
                      MOVE  99  TO  WK-TANCD DEN-F06
                      MOVE  1   TO  DEN-F53
                  ELSE
                      MOVE  99  TO  WK-TANCD DEN-F06
                      MOVE  0   TO  DEN-F53
                  END-IF
              ELSE
******************2008/08/20 内部統制対応
******************MOVE  DEN-F06 TO  WK-TANCD
                  MOVE  DEN-F60 TO  WK-TANCD
              END-IF
     ELSE
**************2008/08/20 内部統制対応
**************MOVE    DEN-F06   TO  WK-TANCD
              MOVE    DEN-F60   TO  WK-TANCD
     END-IF.
*
*2001/09/10 特別処理
     IF       DEN-F01  =  173
     AND      DEN-F279 =  1
              COMPUTE  DEN-F181  =  DEN-F15  * DEN-F172
              COMPUTE  DEN-F182  =  DEN-F15  * DEN-F173
     END-IF.
*##2014/01/06 NAV ST 原価税_し判定
     IF  WK-ZEINUKI-KBN2  >  ZERO
         IF       DEN-F113  >=  WK-ZEINUKI-KBN2
              MOVE    WK-ZEINUKI-KBN3   TO   WK-ZEINUKI-CHK
         ELSE
              MOVE    WK-ZEINUKI-KBN1   TO   WK-ZEINUKI-CHK
         END-IF
     ELSE
         MOVE    WK-ZEINUKI-KBN1   TO   WK-ZEINUKI-CHK
     END-IF.
*##2014/01/06 NAV ST 税区分セット方法を変更
     IF  DEN-F34  = SPACE  OR  "0"  *>売伝Ｆ：税区分判定
         IF  DEN-F113  >=  JBK-F06  *>消費税改定日判定
             MOVE  JBK-F15(1:1)   TO  WK-ZEIKBN *>後税区分　条件
         ELSE
             MOVE  JBK-F14(1:1)   TO  WK-ZEIKBN *>前税区分　条件
         END-IF
     ELSE
         MOVE      DEN-F34   TO   WK-ZEIKBN     *>指定税区分売上
     END-IF.
*****税区分の消費税率を取得
     MOVE     50        TO   JYO-F01.
     MOVE     WK-ZEIKBN TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG   =    0
              MOVE      JYO-REC   TO   JZK-REC
     END-IF.
*##2014/01/06 NAV ED 税区分セット方法を変更
*##2014/01/06 NAV ED 原価税_し判定
*****サンワドー特別処理
*****IF       DEN-F01  =  3924
*****IF       DEN-F01  =  3924  OR  39241
*****IF     ( DEN-F01  =  3924  OR  39241  OR  1994  OR  1995 )
*****IF     ( DEN-F01  =  3924 OR 39241 OR 1994 OR 1995 OR 1041 )
*##2014/01/06 NAV ST 原価税_し方法変更
*##  IF  ( DEN-F01 = 3924 OR 39241 OR 1994 OR 1995 OR 1041 OR 13 )
*##  AND ( DEN-F27E =  ZERO )
     IF  ( WK-ZEINUKI-CHK = 1     )  *>税_対象取引先
     AND ( DEN-F27E       = ZERO  )  *>税抜ＦＬＧ（旧）チェック
     AND ( DEN-F35        = SPACE )  *>税抜ＦＬＧ（新）チェック
*##2014/01/06 NAV ED 原価税_し方法変更
              MOVE     DEN-F172   TO  WK-GENKA1
**************税抜額算出
**************COMPUTE  WK-ZEINUKI ROUNDED =
**************         WK-GENKA1 / WK-SYOHIZEI-RITU
**************指定日以上の場合、新税率で計算／以下は、旧税率
        IF  JZK-F05  =  0  *>税区分ﾚｺｰﾄﾞの消費税判定
            IF  DEN-F113 >= JBK-F06 *>以下、消費税率ﾚｺｰﾄﾞ参照
                COMPUTE  WK-ZEINUKI ROUNDED =
                         WK-GENKA1 / JBK-F07
            ELSE
                COMPUTE  WK-ZEINUKI ROUNDED =
                         WK-GENKA1 / JBK-F04
            END-IF
        ELSE                        *>以下、税区分ﾚｺｰﾄﾞ参照
                COMPUTE  WK-ZEINUKI ROUNDED =
                         WK-GENKA1 / JZK-F05
        END-IF
        MOVE     WK-ZEINUKI TO  DEN-F172
        COMPUTE  DEN-F181  =  DEN-F15  * DEN-F172
********消費税抜きセット
        MOVE     "1"   TO   DEN-F27E
*##2014/01/06 NAV ST 原価税_し方法変更
        MOVE     "1"   TO   DEN-F35
*##2014/01/06 NAV ED 原価税_し方法変更
     END-IF.
*
     MOVE     SPACE          TO   TOKU-REC.
     MOVE     SPACE          TO   TOKUJ-REC.
     MOVE     SPACE          TO   WKJ-REC.
     INITIALIZE                   TOKU-REC.
     INITIALIZE                   TOKUJ-REC.
     INITIALIZE                   WKJ-REC.
*****MOVE     2900           TO   TOKU-F01.
*****MOVE     WK-BUMON       TO   TOKU-F01.
*## 2009/10/09 部門ＣＤ取得追加
     MOVE     DEN-F01        TO   TOK-F01.
     PERFORM  900-TOK-READ.
*****部門CD変更日
     MOVE     TOK-F76        TO   WK-TOK-F76.
     MOVE     DEN-F113       TO   WK-DEN-F113.
     IF  TOK-F76  >  ZERO
         IF   WK-DEN-F113  >=  WK-TOK-F76
              MOVE  TOK-F77  TO   TOKU-F01
         ELSE
              MOVE  TOK-F75  TO   TOKU-F01
         END-IF
     ELSE
         MOVE TOK-F75        TO   TOKU-F01
     END-IF.
     MOVE     DEN-F051       TO   TOKU-F02.
*    伝票区分＝４７の時、情報ＳＹＳ送信時の伝票区分は”４２”で
     IF       DEN-F051 = 47
              MOVE  42       TO   TOKU-F02
              MOVE  DEN-F051 TO   TOKU-F40
              MOVE  "ﾍﾝﾋﾟﾝﾊｲｷ" TO TOKU-F22
     ELSE
              MOVE  DEN-F22  TO   TOKU-F22
     END-IF.
     MOVE     DEN-F23        TO   TOKU-F03.
     MOVE     DEN-F03        TO   TOKU-F04.
     MOVE     DEN-F04        TO   TOKU-F05.
     MOVE     DEN-F24        TO   TOKU-F06.
*### 03/06/04 NAV START ###*
*    各支店／営業所毎に売上場所変換
     MOVE     DEN-F113       TO   WK-SYU.
*
     MOVE     DEN-F07        TO   TOKU-F07.
     MOVE     DEN-F111       TO   TOKU-F08.
     MOVE     DEN-F112       TO   TOKU-F09.
     MOVE     DEN-F113       TO   TOKU-F10.
     IF       DEN-F113       =    ZERO
         MOVE DEN-F112       TO   TOKU-F10
     END-IF.
*
     MOVE     DEN-F1411      TO   TOKU-F11.
     MOVE     DEN-F1412      TO   TOKU-F12.
*## 1999/12/27 ｽﾄｯｸNO. NAV START ##*
*## 2005/12/07 ｽﾄｯｸNO. NAV START ##* 6ｹﾀに変更
*****MOVE     DEN-F21        TO   TOKU-F13.
     MOVE     DEN-F43        TO   TOKU-F13.
     MOVE     DEN-F16        TO   TOKU-F14.
     MOVE     DEN-F15        TO   TOKU-F15.
     MOVE     DEN-F172       TO   TOKU-F16.
     MOVE     DEN-F181       TO   TOKU-F17.
*2001/09/10 特別処理
     IF       DEN-F01  =  173
     AND      DEN-F279 =  1
              COMPUTE  TOKU-F17  =  DEN-F15  * DEN-F172
              MOVE  TOKU-F17 TO   WKJ-F23
     END-IF.
     MOVE     DEN-F08        TO   TOKU-F18.
     MOVE     "2"            TO   TOKU-F19.
*##2014/01/06 NAV ST 税区分セット方法を変更
*****MOVE     SPACE          TO   TOKU-F20.
     IF  DEN-F34  = SPACE  OR  "0"  *>売伝Ｆ：税区分判定
         IF  DEN-F113  >=  JBK-F06  *>消費税改定日判定
             MOVE  JBK-F15(1:1)   TO  TOKU-F20 *>後税区分　条件
         ELSE
             MOVE  JBK-F14(1:1)   TO  TOKU-F20 *>前税区分　条件
         END-IF
     ELSE
         MOVE      DEN-F34   TO   TOKU-F20     *>指定税区分売上
     END-IF.
*# 2020/04/24 NAV ST
     MOVE     TOKU-F20       TO   WK-ZEI-KBN.
*# 2020/04/24 NAV ED
*##2014/01/06 NAV ED 税区分セット方法を変更
     MOVE     SPACE          TO   TOKU-F21.    *>作業明細区分
*****MOVE     DEN-F22        TO   TOKU-F22.    *>備考
     MOVE     DEN-F10        TO   TOKU-F23.    *>注文番号
     MOVE     DEN-F25        TO   TOKU-F24.
*****MOVE     DEN-F06        TO   TOKU-F25.
     MOVE     WK-TANCD       TO   TOKU-F25.
     MOVE     DEN-F133       TO   TOKU-F26.
     MOVE     SYS-YYMD       TO   TOKU-F27.
*## 2009/10/09 NAV T.T ST
     IF  DEN-F171 <=  0
          MOVE     DEN-F141       TO   MEI-F01
          PERFORM  900-MEI-READ
          MOVE     0              TO   SHO-INV-FLG
          IF   DEN-F25   NOT  =   SPACE
               MOVE     DEN-F01   TO   SHO-F01
               MOVE     DEN-F25   TO   SHO-F02
               PERFORM   900-SHO-READ
          END-IF
          IF  SHO-INV-FLG  =  0
              IF  SHO-F09  NOT =  ZERO
                  MOVE  SHO-F09   TO   DEN-F171
              ELSE
                  MOVE  MEI-F041  TO   DEN-F171
              END-IF
          END-IF
     END-IF.
*
     MOVE     DEN-F171       TO   TOKU-F37.
     MOVE     DEN-F173       TO   TOKU-F38.
     MOVE     DEN-F01        TO   TOKU-F39.
     IF       DEN-F50  NOT =  ZERO
              MOVE  DEN-F50  TO   TOKU-F41
     ELSE
              MOVE  DEN-F15  TO   TOKU-F41
     END-IF.
*## 2009/10/09 NAV T.T ED
*## 2010/01/28 NAV T.T ST
*****MOVE     DEN-F31        TO   TOKU-F42.
     MOVE     SPACE          TO   TOKU-F42.
     MOVE     TOKU-REC(1:248)  TO   TOKUJ-REC(1:248).
     MOVE     DEN-F1421     TO   TOKUJ-F43.
     MOVE     DEN-F1422     TO   TOKUJ-F44.
*# 2020/04/27 NAV ST
     MOVE     DEN-D99       TO   TOKUJ-F45.
*# 2020/04/27 NAV ED
*## 2010/01/28 NAV T.T ED
     IF       DEN-F03   NOT  =    80
              ADD       1         TO   OUT-CNT
**************# 2020/04/27 NAV ST  Ｄ３６５連携データ作成
**************PERFORM  KEJ-WT-SEC
**************# 2021/05/14 NAV ST  未計上対応
              IF  DEN-F15  >  ZERO
**************# 2021/05/06 NAV ST  数量＝０の時は連携しない
                  IF  DEN-D89  =  SPACE
                      PERFORM  KEJ-WT-SEC
                  END-IF
**************# 2021/05/06 NAV ED  数量＝０の時は連携しない
              END-IF
**************# 2021/05/06 NAV ED  数量＝０の時は連携しない
**************# 2020/04/27 NAV ED  Ｄ３６５連携データ作成
              WRITE     TOKU-REC
              WRITE     TOKUJ-REC
     END-IF.
*次レコード読込直前ＲＥＷＲＩＴＥへ変更
     PERFORM  900-DEN-REWRITE.
*
     PERFORM  900-DEN-READ.
 2411-TOKU-OUTPUT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ｴﾗｰ  ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2412-ERROR-PRINT       SECTION.
     IF       LINE-CNT  >    63
              PERFORM   2413-HEAD-PRINT
     END-IF.
*
*2022/09/20↓
     MOVE      0           TO        ER01
                                     ER02
                                     ER03
                                     ER04
                                     ER05
                                     ER06
                                     ER07
                                     ER08
                                     ER09
                                     ER10
                                     ER11
                                     ER12
                                     ER13
                                     ER14
                                     ER15.
     MOVE      SPACE       TO        ERR-REC.
     INITIALIZE                      ERR-REC.
*2022/09/20↑
     MOVE     SPACE          TO   MEIS01.
     MOVE     DEN-F02        TO   ME-03.
     MOVE     DEN-F01        TO   ME-04.
     MOVE     DEN-F07        TO   ME-05.
     MOVE     TEN-F03        TO   ME-06.
     MOVE     DEN-F25        TO   ME-07.
     MOVE     DEN-F141       TO   ME-07A.
     MOVE     DEN-F142       TO   ME-08.
*#2021/06/21 NAV ST
     MOVE     "("            TO   ME-081.
     MOVE     DEN-F15        TO   WK-DEN-F15.
     MOVE     WK-DEN-F15     TO   ME-082.
     MOVE     ")"            TO   ME-083.
*#2021/06/21 NAV ED
*****MOVE     MEI-F03        TO   ME-08.
     IF       SHO-INV-FLG    =    2
*##           MOVE NC"商品変換ＴＢＬ不一致" TO   ME-09
              MOVE NC"２"    TO   ME-092
*2022/09/20↓
              MOVE 1         TO   ER02
*2022/09/20↑
     END-IF.
     IF       SHO-INV-FLG    =    1
*##           MOVE NC"商品変換ＴＢＬ未登録" TO   ME-09
              MOVE NC"１"    TO   ME-091
*2022/09/20↓
              MOVE 1         TO   ER01
*2022/09/20↑
     END-IF.
     IF       MEI-INV-FLG    =    1
*##           MOVE NC"商品Ｍ　未登録"  TO   ME-09
              MOVE NC"３"    TO   ME-093
*2022/09/20↓
              MOVE 1         TO   ER03
*2022/09/20↑
     END-IF.
     IF       TEN-INV-FLG    =    1
*##           MOVE NC"店舗Ｍ　未登録"  TO   ME-09
              MOVE NC"４"    TO   ME-094
*2022/09/20↓
              MOVE 1         TO   ER04
*2022/09/20↑
     END-IF.
     IF       SYONIN-FLG    =    1
*##           MOVE NC"伝票未承認！！"  TO   ME-09
              MOVE NC"５"    TO   ME-095
     END-IF.
     IF       TOK-INV-FLG   =    1
              MOVE NC"６"    TO   ME-096
*2022/09/20↓
              MOVE 1         TO   ER06
*2022/09/20↑
     END-IF.
     IF       TOK-INV-FLG   =    2
              MOVE NC"７"    TO   ME-097
     END-IF.
*##2014/01/06 NAV ST 税区分エラー
     IF       JYO-INV-FLG   =    1
**************MOVE NC"８"    TO   ME-097
              MOVE NC"８"    TO   ME-098
*2022/09/20↓
              MOVE 1         TO   ER08
*2022/09/20↑
     END-IF.
*##2014/01/06 NAV ED 税区分エラー
*##2020/04/24 NAV ST ＳＵＢ商品名称Ｍエラー
     IF       SBM-INV-FLG   =    1
              MOVE NC"５"    TO   ME-095
*2022/09/20↓
              MOVE 1         TO   ER05
*2022/09/20↑
     END-IF.
*##2020/04/24 NAV ED ＳＵＢ商品名称Ｍエラー
*##2020/06/30 NAV ST 在庫調整ＦＬＧエラー
     IF       ZTS-INV-FLG   =    1
              MOVE NC"９"    TO   ME-099
*2022/09/20↓
              MOVE 1         TO   ER09
*2022/09/20↑
     END-IF.
*##2020/06/30 NAV ED 在庫調整ＦＬＧエラー
*##2020/12/01 NAV ST 顧客需要家ＩＤ管理マスタエラー
     IF       KYK-INV-FLG   =    1
              MOVE NC"０"    TO   ME-09A
*2022/09/20↓
              MOVE 1         TO   ER10
*2022/09/20↑
     END-IF.
*##2020/12/01 NAV ED 顧客需要家ＩＤ管理マスタエラー
*##2021/05/06 NAV ST 先行受注数量変更＝０エラー　　
     IF       SENKOU-CHK-FLG  =  1
              MOVE NC"７"    TO   ME-097
*2022/09/20↓
              MOVE 1         TO   ER07
*2022/09/20↑
     END-IF.
*##2021/05/06 NAV ED 先行受注数量変更＝０エラー　　
*
     WRITE    PRT-REC        FROM MEIS01    AFTER     1.
     ADD      1              TO   LINE-CNT.
*2022/09/14↓
     IF  ( ER01 =  0 ) AND
         ( ER02 =  0 ) AND
         ( ER03 =  0 ) AND
         ( ER04 =  0 ) AND
         ( ER05 =  0 ) AND
         ( ER06 =  0 ) AND
         ( ER07 =  0 ) AND
         ( ER08 =  0 ) AND
         ( ER09 =  0 ) AND
         ( ER10 =  0 ) AND
         ( ER11 =  0 ) AND
         ( ER12 =  0 ) AND
         ( ER13 =  0 ) AND
         ( ER14 =  0 ) AND
         ( ER15 =  0 )
           CONTINUE
     ELSE
*日次更新日付
           MOVE    SYS-DATEW      TO   ERR-F01
*日次更新時刻
           MOVE    SYS-TIMEW      TO   ERR-F02
*取引先CD
           MOVE    DEN-F01        TO   ERR-F03
*伝票番号
           MOVE    DEN-F02        TO   ERR-F04
*たねまる伝票番号
           MOVE    DEN-D99        TO   ERR-F05
*行番号
           MOVE    DEN-F03        TO   ERR-F06
*相殺区分
           MOVE    DEN-F04        TO   ERR-F07
*伝票区分
           MOVE    DEN-F051       TO   ERR-F08
*担当者CD
           MOVE    DEN-F06        TO   ERR-F09
*出荷場所
           MOVE    DEN-F08        TO   ERR-F10
*店舗CD
           MOVE    DEN-F07        TO   ERR-F11
*納品日
           MOVE    DEN-F112       TO   ERR-F12
*出荷日
           MOVE    DEN-F113       TO   ERR-F13
*サカタ商品CD
           MOVE    DEN-F1411      TO   ERR-F14
*サカタ品単CD
           MOVE    DEN-F1412      TO   ERR-F15
*相手商品CD
           MOVE    DEN-F25        TO   ERR-F16
*たねまるJANCD
*商品名１
*商品名２
           IF      SBM-INV-FLG NOT = 1
                   MOVE    SBM-D01        TO   ERR-F17
                   MOVE    SBM-F031       TO   ERR-F18
                   MOVE    SBM-F032       TO   ERR-F19
           END-IF
*受注時数量
           MOVE    DEN-F50        TO   ERR-F20
*出荷時数量
           MOVE    DEN-F15        TO   ERR-F21
*原価単価
           MOVE    DEN-F172       TO   ERR-F22
*売価単価
           MOVE    DEN-F173       TO   ERR-F23
*付番区分
           MOVE    DEN-F276       TO   ERR-F24
*売上計上区分
           MOVE    DEN-F277       TO   ERR-F25
*ORDセット区分
           MOVE    DEN-F32        TO   ERR-F26
*連携番号
           MOVE    DEN-F33        TO   ERR-F27
*ナフコ管理番号
           MOVE    DEN-F413       TO   ERR-F28
*受信日
           MOVE    DEN-F46        TO   ERR-F29
*受信時刻
           MOVE    DEN-F47        TO   ERR-F30
*振分倉庫
           MOVE    DEN-F48        TO   ERR-F31
*需要家ID
           MOVE    DEN-D88        TO   ERR-F32
*在庫調整フラグ
           MOVE    DEN-F27C       TO   ERR-F33
*先行受注時数量
           MOVE    DEN-D86        TO   ERR-F34
*エラー区分1
           IF      ER01               =  1
                   MOVE    1              TO   ERR-F35
           END-IF
*エラー区分2
           IF      ER02               =  1
                   MOVE    1              TO   ERR-F36
           END-IF
*エラー区分3
           IF      ER03               =  1
                   MOVE    1              TO   ERR-F37
           END-IF
*エラー区分4
           IF      ER04               =  1
                   MOVE    1              TO   ERR-F38
           END-IF
*エラー区分5
           IF      ER05               =  1
                   MOVE    1              TO   ERR-F39
           END-IF
*エラー区分6
           IF      ER06               =  1
                   MOVE    1              TO   ERR-F40
           END-IF
*エラー区分7
           IF      ER07               =  1
                   MOVE    1              TO   ERR-F41
           END-IF
*エラー区分8
           IF      ER08               =  1
                   MOVE    1              TO   ERR-F42
           END-IF
*エラー区分9
           IF      ER09               =  1
                   MOVE    1              TO   ERR-F43
           END-IF
*エラー区分10
           IF      ER10              =  1
                   MOVE    1              TO   ERR-F44
           END-IF
*エラー区分11
           IF      ER11              =  1
                   MOVE    1              TO   ERR-F45
           END-IF
*エラー区分12
           IF      ER12              =  1
                   MOVE    1              TO   ERR-F46
           END-IF
*エラー区分13
           IF      ER13              =  1
                   MOVE    1              TO   ERR-F47
           END-IF
*エラー区分14
           IF      ER14              =  1
                   MOVE    1              TO   ERR-F48
           END-IF
*エラー区分15
           IF      ER15              =  1
                   MOVE    1              TO   ERR-F49
           END-IF
*
           WRITE   ERR-REC
           ADD     1                      TO   DNI-WT-CNT
     END-IF.
*2022/09/14↑
*
 2412-ERROR-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 2413-HEAD-PRINT        SECTION.
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*
     ADD      1                   TO   PAGE-CNT.
     MOVE     SYS-YY              TO   HD-011.
     MOVE     SYS-MM              TO   HD-012.
     MOVE     SYS-DD              TO   HD-013.
     MOVE     PAGE-CNT            TO   HD-02.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     1.
     WRITE    PRT-REC   FROM      P-SPACE   AFTER     1.
     MOVE     7              TO   LINE-CNT.
 2413-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    条件ファイル　 READ                          *
*--------------------------------------------------------------*
 900-JYO-READ           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HJYOKEN   INVALID
              MOVE      1              TO   INV-FLG
     DISPLAY  "### TNI0030B HJYOKEN INVALID KEY= "
                                     JYO-F01  JYO-F02 " ###"
                                       UPON CONS
     END-READ.
 900-JYO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     MOVE     0         TO   TOK-INV-FLG.
     READ     HTOKMS    INVALID
              MOVE      1         TO   TOK-INV-FLG
*##2014/01/06 NAV ST 取引先Ｍ読込時、税抜区分退避の為
              GO                  TO   900-TOK-READ-EXIT
*##2014/01/06 NAV ED 取引先Ｍ読込時、税抜区分退避の為
*## 2009/10/09 NAV T.T ｺﾒﾝﾄｱｳﾄ
**************MOVE      0         TO   TOK-INV-FLG
     END-READ.
*##2014/01/06 NAV ST 税抜区分関係退避
     MOVE     TOK-F20   TO   WK-ZEINUKI-KBN1.
     MOVE     TOK-F21   TO   WK-ZEINUKI-KBN2.
     MOVE     TOK-F22   TO   WK-ZEINUKI-KBN3.
     IF  NEW-F01  NOT =  OLD-F01
      DISPLAY NC"☆☆☆☆☆☆☆☆☆☆☆☆☆☆☆"  UPON  CONS
      DISPLAY NC"☆取引先ＣＤ" " = " TOK-F01      UPON  CONS
      DISPLAY NC"☆取引先名称" " = " TOK-F03      UPON  CONS
      DISPLAY NC"☆部門ＣＤ　" " = " TOK-F75      UPON  CONS
      DISPLAY NC"☆部門改訂日" " = " TOK-F76      UPON  CONS
      DISPLAY NC"☆改定後部門" " = " TOK-F77      UPON  CONS
      DISPLAY NC"☆税抜計算１" " = " WK-ZEINUKI-KBN1  UPON  CONS
      DISPLAY NC"☆税抜改定日" " = " WK-ZEINUKI-KBN2  UPON  CONS
      DISPLAY NC"☆税抜計算２" " = " WK-ZEINUKI-KBN3  UPON  CONS
     END-IF.
*##2014/01/06 NAV ED 税抜区分関係退避
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     MOVE     0         TO   TEN-INV-FLG.
     READ     HTENMS    INVALID
              MOVE      1         TO   TEN-INV-FLG
              MOVE      SPACE     TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    商品マスタ　　READ                           *
*--------------------------------------------------------------*
 900-MEI-READ           SECTION.
     MOVE     0         TO   MEI-INV-FLG.
     READ     HMEIMS    INVALID
              MOVE      1         TO   MEI-INV-FLG
              MOVE      SPACE     TO   MEI-F03
     END-READ.
 900-MEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    商品変換テーブル　　READ                     *
*--------------------------------------------------------------*
 900-SHO-READ           SECTION.
     MOVE     0         TO   SHO-INV-FLG.
     READ     HSHOTBL    INVALID
              MOVE      1         TO   SHO-INV-FLG
     END-READ.
     IF       SHO-INV-FLG    =    0
     AND      SHO-F03   NOT  =    DEN-F141
              MOVE      2         TO   SHO-INV-FLG
     END-IF.
 900-SHO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-DEN-START-READ     SECTION.
* 2011/10/06,S  S.I/NAV
**     START    HDENJNL   KEY  >=   DEN-F01   DEN-F02
**                                  DEN-F04   DEN-F03
**              INVALID   KEY
**                        MOVE HIGH-VALUE     TO   NEW
**     END-START.
     START    HDENJNL   KEY  >=   DEN-F01  DEN-F02
                                  DEN-F04  DEN-F07
                                  DEN-F112 DEN-F03
              INVALID   KEY
                        MOVE HIGH-VALUE     TO   NEW
     END-START.
* 2011/10/06,E  S.I/NAV
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   900-DEN-READ
     END-IF.
 900-DEN-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 READ                          *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     READ     HDENJNL   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-DEN-READ-EXIT
     END-READ.
 DEN010.
     MOVE     DEN-F112  TO   WK-NOUHIN.
     ADD      1         TO   CNT.
     IF       CNT(4:3) =  "500" OR  "000"
              DISPLAY  "DEN-CNT= " CNT    UPON   CONS
     END-IF.
 DEN020.
     IF       WK-YMD    =    ZERO
              GO   TO   900-DEN-READ
     END-IF.
 DEN030.
     IF       WK-TOKCD  NOT  =    0
     AND      DEN-F01   >    WK-TOKCD
              PERFORM   900-DEN-REWRITE
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-DEN-READ-EXIT
     END-IF.
 DEN040.
*
     IF       DEN-F277  NOT  =    0
     OR       DEN-F276  NOT  =    9
**************PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
 DEN050.
     IF       CYU-DATE  NOT  =    ZERO
     AND      DEN-F111  NOT  =    CYU-DATE
**************PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*
**** AND      DEN-F112  NOT  =    NOU-DATE   92.12.14
**** 納品日ベースの売上計上へ変更する****************
*****AND      DEN-F112  >         NOU-DATE          *
*****************************************************
 DEN060.
     IF       NOU-DATE  NOT  =    ZERO
     AND      DEN-F113  >         NOU-DATE
**************PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*
 DEN070.
     IF      (DEN-F113  =         ZERO)         AND
             (DEN-F051  =  40  OR 41  )
**************PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*
 DEN080.
     IF       DEN-F051  NOT =  40 AND 41
        IF    DEN-F112  >         NOU-DATE
**************PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
        END-IF
     END-IF.
*
*
 DEN090.
     MOVE     DEN-F01        TO   NEW-F01.
     MOVE     DEN-F02        TO   NEW-F02.
*    ﾀﾝﾄｳｼｬｾｯﾄ
     MOVE     DEN-F06        TO   TAN-NEW.
 900-DEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 REWRITE                       *
*--------------------------------------------------------------*
 900-DEN-REWRITE        SECTION.
     REWRITE  DEN-REC.
 900-DEN-REWRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    SUB商品名称マスタ  READ                      *
*--------------------------------------------------------------*
 900-SBM-READ           SECTION.
     MOVE     0         TO   SBM-INV-FLG.
     READ     SUBMEIF   INVALID
              MOVE      1         TO   SBM-INV-FLG
     END-READ.
 900-SBM-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    顧客需要家ＩＤ管理マスタ  READ               *
*--------------------------------------------------------------*
 900-KYK-READ           SECTION.
     MOVE     0         TO   KYK-INV-FLG.
     READ     KYKJYKF   INVALID
              MOVE      1         TO   KYK-INV-FLG
     END-READ.
 900-KYK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    受注売上調整計上データ　存在チェック　　　　 *
*--------------------------------------------------------------*
 900-KEJ-READ           SECTION.
     MOVE     0         TO   KEJ-INV-FLG.
     READ     URIKEJF   INVALID
              MOVE      1         TO   KEJ-INV-FLG
     END-READ.
 900-KEJ-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    受注売上調整計上データ　作成　　　　　　　　 *
*--------------------------------------------------------------*
 KEJ-WT-SEC             SECTION.
*
*初期化
     MOVE     SPACE     TO        KEJ-REC.
     INITIALIZE                   KEJ-REC.
*取引先ＣＤ
     MOVE     DEN-F01   TO        KEJ-F01.
*自社伝票番号
     MOVE     DEN-F02   TO        KEJ-F02.
*相殺区分
     MOVE     DEN-F04   TO        KEJ-F03.
*伝票区分
     MOVE     DEN-F051  TO        KEJ-F04.
*店舗ＣＤ
     MOVE     DEN-F07   TO        KEJ-F05.
*納品日
     MOVE     DEN-F112  TO        KEJ-F06.
*行番号
     MOVE     DEN-F03   TO        KEJ-F07.
*存在チェック
     PERFORM  900-KEJ-READ.
     IF  KEJ-INV-FLG  =  1
         MOVE  SPACE    TO        WK-KOUSIN-KBN *>登録
         MOVE  SPACE    TO        KEJ-REC
         INITIALIZE               KEJ-REC
*********キー再セット
         MOVE DEN-F01   TO        KEJ-F01
         MOVE DEN-F02   TO        KEJ-F02
         MOVE DEN-F04   TO        KEJ-F03
         MOVE DEN-F051  TO        KEJ-F04
         MOVE DEN-F07   TO        KEJ-F05
         MOVE DEN-F112  TO        KEJ-F06
         MOVE DEN-F03   TO        KEJ-F07
     ELSE
         MOVE  "R"      TO        WK-KOUSIN-KBN *>更新上書き
     END-IF.
*オーダー区分
*#2020/12/21 NAV ST  オーダー区分セット方法変更
*****MOVE     SBM-D03   TO        KEJ-F08.
*    EVALUATE DEN-F32
*        WHEN "1"  WHEN  "3"
*        MOVE "D21"     TO        KEJ-F08
*        WHEN "2"
*        MOVE "D23"     TO        KEJ-F08
*        WHEN  OTHER
*        MOVE  SBM-D03  TO        KEJ-F08
*****END-EVALUATE.
*****仕変　先行受注の場合、先行受注時に連携したオーダー区分を
*****セット。以外の場合は、ＳＵＢ商名Ｍのオーダー区分をセット
     IF  DEN-D96  NOT =  SPACE
         MOVE  DEN-D96  TO        KEJ-F08
     ELSE
         MOVE  SBM-D03  TO        KEJ-F08
*********#2021/05/14 NAV ST
         IF    DEN-F16  =  "5"
               IF  KEJ-F08  =  "D04       "
                   MOVE  "P10       "  TO    KEJ-F08
               END-IF
         END-IF
         IF    DEN-F16  =  "6"
               IF  KEJ-F08  =  "D04       "
                   MOVE  "D10       "  TO    KEJ-F08
               END-IF
         END-IF
*********#2021/05/14 NAV ED
     END-IF.
*#2020/12/21 NAV ED  オーダー区分セット方法変更
*#2021/08/18 NAV ST  オーダー区分＝Ｄ４になる障害対応
     IF  KEJ-F08  =  "D4        "
         MOVE  "D04       "       TO    KEJ-F08
         DISPLAY "ORD SET KBN ERR = " SBM-D03 " - " KEJ-F08
                 UPON CONS
     END-IF.
*#2021/08/18 NAV ST  オーダー区分＝Ｄ４になる障害対応
*#2021/01/12 NAV ST  値引き伝票　オーダー区分セット
     IF  KEJ-F04  =  "42"
         MOVE  WK-ORDER-KBN       TO   KEJ-F08
     END-IF.
*#2021/01/12 NAV ED  値引き伝票　オーダー区分セット
*Ｄ３６５伝票番号
*#2020/06/29 NAV ST 連携課題対応
*****MOVE     DEN-D99   TO        KEJ-F09.
*
*#2021/03/09 NAV ST 値引の場合１９：２に行番号をセットしたが
*#                  行番号をセットしない様に変更する。
     IF  DEN-F051  =  42
         MOVE  DEN-D99(1:15)      TO   KEJ-F09(1:15)
         MOVE  KEJ-F08(1:3)       TO   KEJ-F09(16:3)
*#2021/03/09 NAV ST
*********MOVE  DEN-F03            TO   KEJ-F09(19:2)
         MOVE  SPACE              TO   KEJ-F09(19:2)
*#2021/03/09 NAV ED
     ELSE
         MOVE  DEN-D99(1:15)      TO   KEJ-F09(1:15)
         MOVE  KEJ-F08(1:3)       TO   KEJ-F09(16:3)
         MOVE  SPACE              TO   KEJ-F09(19:2)
     END-IF.
*#2020/06/29 NAV ED 連携課題対応
*担当者
     MOVE     DEN-F06   TO        KEJ-F10.
*出荷場所
     MOVE     DEN-F08   TO        KEJ-F11.
*注文日
     MOVE     DEN-F111  TO        KEJ-F12.
*出荷日
     MOVE     DEN-F113  TO        KEJ-F13.
*商品ＣＤ
     MOVE     DEN-F1411 TO        KEJ-F14.
*品単ＣＤ
     MOVE     DEN-F1412 TO        KEJ-F15.
*商品名１
     MOVE     DEN-F1421 TO        KEJ-F16.
*商品名２
     MOVE     DEN-F1422 TO        KEJ-F17.
*数量
     MOVE     DEN-F15   TO        KEJ-F18.
*単価区分
     MOVE     DEN-F16   TO        KEJ-F19.
*仕入単価
     MOVE     DEN-F171  TO        KEJ-F20.
*原価単価
     MOVE     DEN-F172  TO        KEJ-F21.
*売価単価
     MOVE     DEN-F173  TO        KEJ-F22.
*原価金額
     MOVE     DEN-F181  TO        KEJ-F23.
*売価金額
     MOVE     DEN-F182  TO        KEJ-F24.
*備考
     MOVE     DEN-F22   TO        KEJ-F25.
*指定伝票番号
     MOVE     DEN-F23   TO        KEJ-F26.
*自社得意先ＣＤ
     MOVE     DEN-F24   TO        KEJ-F27.
*指定商品ＣＤ
     MOVE     DEN-F25   TO        KEJ-F281.
*Ｄ３６５商品ＣＤ（ＪＡＮＣＤ）
     MOVE      ZERO           TO   SBM-INV-FLG.
     MOVE      DEN-F1411      TO   SBM-F011.
     MOVE      DEN-F1412      TO   SBM-F012.
     PERFORM   900-SBM-READ.
     IF  SBM-INV-FLG  =  1
         MOVE DEN-F25   TO        KEJ-F282
     ELSE
         MOVE SBM-D01   TO        KEJ-F282
     END-IF.
*付番区分
     MOVE     DEN-F276  TO        KEJ-F29.
*売上データ作成
     MOVE     DEN-F277  TO        KEJ-F30.
*在庫引落区分
     MOVE     DEN-F27C  TO        KEJ-F31.
*在庫引当区分
     MOVE     DEN-F27D  TO        KEJ-F32.
*欠品制御区分
     MOVE     DEN-F31   TO        KEJ-F33.
*先行受注送信区分／先行受注連携ＮＯ
     IF  DEN-F33 NOT = SPACE
         MOVE "1"       TO        KEJ-F34
         MOVE DEN-F33   TO        KEJ-F35
     ELSE
         MOVE SPACE     TO        KEJ-F34
         MOVE SPACE     TO        KEJ-F35
     END-IF.
*税区分
     MOVE     WK-ZEI-KBN TO       KEJ-F36.
*税抜計算済ＦＬＧ
     MOVE     DEN-F35   TO        KEJ-F37.
*ナフコ管理番号
     MOVE     DEN-F413  TO        KEJ-F38.
*受注／売上／売上調整作成判定
     EVALUATE DEN-F051  ALSO  DEN-F04  ALSO  DEN-D95
*伝区＝４０、相殺＝０、先行＝空白　（受注、売上、××）
         WHEN    40     ALSO     0     ALSO     SPACE
              MOVE  "1"          TO    KEJ-F39
              MOVE  "1"          TO    KEJ-F42
              MOVE  SPACE        TO    KEJ-F45
*伝区＝４０、相殺＝０、先行＝済　　（××、売上、××）
         WHEN    40     ALSO     0     ALSO     "1"
              MOVE  SPACE        TO    KEJ-F39
              MOVE  "1"          TO    KEJ-F40
              MOVE  "1"          TO    KEJ-F42
              MOVE  SPACE        TO    KEJ-F45
*伝区＝４０、相殺＝１，３，５、先行＝空白　（××、××、調整）
         WHEN    40     ALSO     1     ALSO     SPACE
         WHEN    40     ALSO     1     ALSO     "1"
         WHEN    40     ALSO     3     ALSO     SPACE
         WHEN    40     ALSO     3     ALSO     "1"
         WHEN    40     ALSO     5     ALSO     SPACE
         WHEN    40     ALSO     5     ALSO     "1"
              MOVE  SPACE        TO    KEJ-F39
              MOVE  SPACE        TO    KEJ-F42
*#2020/06/29 NAV ST
**************MOVE  "2"          TO    KEJ-F45
              MOVE  "1"          TO    KEJ-F45
*#2020/06/29 NAV ST
*伝区＝４０、相殺＝２，４，６、先行＝空白　（××、××、調整）
         WHEN    40     ALSO     2     ALSO     SPACE
         WHEN    40     ALSO     2     ALSO     "1"
         WHEN    40     ALSO     4     ALSO     SPACE
         WHEN    40     ALSO     4     ALSO     "1"
         WHEN    40     ALSO     6     ALSO     SPACE
         WHEN    40     ALSO     6     ALSO     "1"
              MOVE  SPACE        TO    KEJ-F39
              MOVE  SPACE        TO    KEJ-F42
              MOVE  "1"          TO    KEJ-F45
*伝区＝４１、相殺＝０、先行＝空白　（受注、売上、××）
         WHEN    41     ALSO     0     ALSO     SPACE
              MOVE  "2"          TO    KEJ-F39
              MOVE  "2"          TO    KEJ-F42
              MOVE  SPACE        TO    KEJ-F45
****************2020/09/01 先行受注返品
*伝区＝４１、相殺＝０、先行＝済　　（××、売上、××）
         WHEN    41     ALSO     0     ALSO     "1"
              MOVE  SPACE        TO    KEJ-F39
              MOVE  "2"          TO    KEJ-F40
              MOVE  "2"          TO    KEJ-F42
              MOVE  SPACE        TO    KEJ-F45
****************2020/09/01 先行受注返品
*伝区＝４１、相殺＝１，３，５、先行＝空白　（××、××、調整）
         WHEN    41     ALSO     1     ALSO     SPACE
         WHEN    41     ALSO     1     ALSO     "1"
         WHEN    41     ALSO     3     ALSO     SPACE
         WHEN    41     ALSO     3     ALSO     "1"
         WHEN    41     ALSO     5     ALSO     SPACE
         WHEN    41     ALSO     5     ALSO     "1"
              MOVE  SPACE        TO    KEJ-F39
              MOVE  SPACE        TO    KEJ-F42
*#2020/06/29 NAV ST
**************MOVE  "1"          TO    KEJ-F45
              MOVE  "2"          TO    KEJ-F45
*#2020/06/29 NAV ED
*伝区＝４１、相殺＝２，４，６、先行＝空白　（××、××、調整）
         WHEN    41     ALSO     2     ALSO     SPACE
         WHEN    41     ALSO     2     ALSO     "1"
         WHEN    41     ALSO     4     ALSO     SPACE
         WHEN    41     ALSO     4     ALSO     "1"
         WHEN    41     ALSO     6     ALSO     SPACE
         WHEN    41     ALSO     6     ALSO     "1"
              MOVE  SPACE        TO    KEJ-F39
              MOVE  SPACE        TO    KEJ-F42
              MOVE  "2"          TO    KEJ-F45
*#2020/06/29 NAV ST 値引きデータの作成方法を変更
*伝区＝４２、相殺＝０、先行＝空白　（××、××、調整）
*        WHEN    42     ALSO     0     ALSO     SPACE
*             MOVE  SPACE        TO    KEJ-F39
*             MOVE  SPACE        TO    KEJ-F42
*             MOVE  "2"          TO    KEJ-F45
*伝区＝４２、相殺＝１，３，５、先行＝空白　（××、××、調整）
*        WHEN    42     ALSO     1     ALSO     SPACE
*        WHEN    42     ALSO     1     ALSO     "1"
*        WHEN    42     ALSO     3     ALSO     SPACE
*        WHEN    42     ALSO     3     ALSO     "1"
*        WHEN    42     ALSO     5     ALSO     SPACE
*        WHEN    42     ALSO     5     ALSO     "1"
*             MOVE  SPACE        TO    KEJ-F39
*             MOVE  SPACE        TO    KEJ-F42
*             MOVE  "1"          TO    KEJ-F45
*伝区＝４２、相殺＝２，４，６、先行＝空白　（××、××、調整）
*        WHEN    42     ALSO     2     ALSO     SPACE
*        WHEN    42     ALSO     2     ALSO     "1"
*        WHEN    42     ALSO     4     ALSO     SPACE
*        WHEN    42     ALSO     4     ALSO     "1"
*        WHEN    42     ALSO     6     ALSO     SPACE
*        WHEN    42     ALSO     6     ALSO     "1"
*             MOVE  SPACE        TO    KEJ-F39
*             MOVE  SPACE        TO    KEJ-F42
*             MOVE  "2"          TO    KEJ-F45
*#2020/06/29 NAV ED 値引きデータの作成方法を変更
*#2020/06/29 NAV ST 値引きデータ作成方法を以下の通り変更します。
*伝区＝４２、相殺＝０、先行＝空白　（受注、売上、××）
         WHEN    42     ALSO     0     ALSO     SPACE
              MOVE  "2"          TO    KEJ-F39
              MOVE  "2"          TO    KEJ-F42
              MOVE  SPACE        TO    KEJ-F45
*伝区＝４２、相殺＝１，３，５、先行＝空白　（××、××、調整）
         WHEN    42     ALSO     1     ALSO     SPACE
         WHEN    42     ALSO     1     ALSO     "1"
         WHEN    42     ALSO     3     ALSO     SPACE
         WHEN    42     ALSO     3     ALSO     "1"
         WHEN    42     ALSO     5     ALSO     SPACE
         WHEN    42     ALSO     5     ALSO     "1"
              MOVE  SPACE        TO    KEJ-F39
              MOVE  SPACE        TO    KEJ-F42
              MOVE  "2"          TO    KEJ-F45
*伝区＝４２、相殺＝２，４，６、先行＝空白　（××、××、調整）
         WHEN    42     ALSO     2     ALSO     SPACE
         WHEN    42     ALSO     2     ALSO     "1"
         WHEN    42     ALSO     4     ALSO     SPACE
         WHEN    42     ALSO     4     ALSO     "1"
         WHEN    42     ALSO     6     ALSO     SPACE
         WHEN    42     ALSO     6     ALSO     "1"
              MOVE  SPACE        TO    KEJ-F39
              MOVE  SPACE        TO    KEJ-F42
              MOVE  "2"          TO    KEJ-F45
*#2020/06/29 NAV ED 値引きデータ作成方法を以下の通り変更します。
     END-EVALUATE.
*実行日付
     MOVE     SYS-DATEW TO        KEJ-F48.
     MOVE     SYS-DATEW TO        PARA-JKDATE.
*実行時刻
     MOVE     SYS-TIMEW TO        KEJ-F49.
     MOVE     SYS-TIMEW TO        PARA-JKTIME.
*#2021/06/01 NAV ST
*計上需要家ＩＤ
     IF       DEN-D85  NOT =  SPACE
              MOVE  DEN-D85   TO  KEJ-F50
     ELSE
              MOVE  SPACE     TO  KEJ-F50
     END-IF.
*#2021/06/01 NAV ED
*#2022/02/28 NAV ST 実納品日をセット
     IF   DEN-D87  NUMERIC
     AND  DEN-D87  >  ZERO
          MOVE   DEN-D87      TO  KEJ-F51
     ELSE
          MOVE   ZERO         TO  KEJ-F51
     END-IF.
*#2022/02/28 NAV ED
*更新判定
     IF  WK-KOUSIN-KBN  =  "R"
         ADD  1         TO        KEJ-REWT-CNT
         REWRITE  KEJ-REC
     ELSE
         ADD  1         TO        KEJ-WT-CNT
         WRITE    KEJ-REC
     END-IF.
*
 KEJ-WT-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
