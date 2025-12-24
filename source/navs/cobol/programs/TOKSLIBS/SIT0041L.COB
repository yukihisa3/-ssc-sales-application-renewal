****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　　　　　　
*    業務名　　　　　　　：　取引先マスタ　　　　　　　
*    モジュール名　　　　：　（連携）取引先マスタリスト
*    作成日／作成者　　　：　09/03/11  /S.I
*    処理概要　　　　　　：　ORIGINAL SLST010→ＩＴ統制対応　
*    更新履歴　　　　　　：
*      09/03/    /
*        新郵便番号追加
*        代表倉庫（倉庫マスタＲＥＡＤ）追加
*        自社指定伝票タイプ追加
*        部門ＣＤ追加　　　　　
*
*      2011/04/14 T.TAKAHASHI 出荷オ、出荷手区分の追加
*      2011/10/13 飯田/NAV    基幹サーバ統合・業務改善
*      2011/12/07 三浦/NAV    種子実績管理改修取引先マスタ変更
*      2012/10/31 武井/NAV    流通ＢＭＳ対応
*      2013/12/11 井上/NAV    消費税増税対応
*      2013/12/24 井上/NAV    消費税　不要項目カット
*      2013/12/25 井上/NAV    消費税　項目追加
*
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SIT0041L.
 AUTHOR.                   S.I.
 DATE-WRITTEN.             09/03/11.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     STATION     IS        STA
     YA          IS        NIHONGO
     YB          IS        YB
     YB-21       IS        YB-21
     CONSOLE     IS        CONS.
***********************************************************
*             INPUT-OUTPUT                                *
***********************************************************
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*マスタ更新履歴ファイル
     SELECT     MSTLOGF    ASSIGN    TO        MSTLOGL3
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       MSL-F01
                                               MSL-F02
                                               MSL-F03
                                               MSL-F05
                                               MSL-F06
                                               MSL-F07
                     FILE      STATUS    MSL-ST.
*担当者マスタ
     SELECT     HTANMS     ASSIGN    TO        TANMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TAN-F01
                                               TAN-F02
                           FILE      STATUS    TAN-ST.
*倉庫マスタ
     SELECT     ZSOKMS     ASSIGN    TO        ZSOKMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SOK-F01
                           FILE      STATUS    SOK-ST.
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-ST.
*%* プリンター *%*
     SELECT     PRTF       ASSIGN    TO        LP-04.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
*マスタ更新履歴ファイル
 FD  MSTLOGF
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        MSTLOGF   OF        XFDLIB
     JOINING     MSL       AS        PREFIX.
*担当者マスタ
 FD  HTANMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTANMS    OF        XFDLIB
     JOINING     TAN       AS        PREFIX.

*倉庫マスタ
 FD  ZSOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSOKMS    OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
*---<<  条件ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*プリンター
 FD    PRTF      LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
 01  FILE-STATUS.
     03  MSL-ST            PIC X(02).
     03  MSL-ST1           PIC X(04).
     03  PRT-ST            PIC X(02).
     03  PRT-ST1           PIC X(04).
     03  SOK-ST            PIC X(02).
     03  TAN-ST            PIC X(02).
     03  JYO-ST            PIC X(02).
*年度
 01  YYYY.
     03  YY1               PIC  9(02).
     03  YY2               PIC  9(02).
 01  WK-AREA.
     03  END-SW            PIC 9(01) VALUE     ZERO.
     03  ERR-SW            PIC 9(01) VALUE     ZERO.
     03  PAGE-CNT          PIC 9(05) VALUE     ZERO.
     03  LINE-CNT          PIC 9(05) VALUE     ZERO.
     03  WK-NYUKIN         PIC 9(02) VALUE     ZERO.
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 01  IN-DATA               PIC X(01).
 01  FILE-ERR.
     03  MSL-ERR           PIC  N(11) VALUE
                        NC"マスタ更新履歴Ｆエラー".
     03  TAN-ERR           PIC N(10) VALUE
                        NC"担当者マスタエラー".
     03  PRT-ERR           PIC N(10) VALUE
                        NC"プリンターエラー".
     03  SOK-ERR           PIC N(10) VALUE
                        NC"倉庫マスタエラー".
     03  JYO-ERR           PIC N(10) VALUE
                        NC"条件ファイルエラー".
*帳票表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*帳票表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*帳票　更新履歴編集
 01  HEN-KUBUN.
     03  FILLER                   PIC  N(01)  VALUE NC"［".
     03  HEN-KUBUNNM              PIC  N(02).
     03  FILLER                   PIC  N(01)  VALUE NC"：".
 01  HEN-TAN.
     03  HEN-TANNM                PIC  N(08).
     03  FILLER                   PIC  N(01)  VALUE NC"：".
*担当者コード
 01  WK-TANCD.
     03  WK-TANCD1         PIC  X(02).
     03  WK-FILLER         PIC  X(06).
*マスタレコードエリア（取引先マスタ）
     COPY    HTOKMS        OF   XFDLIB
     JOINING TOK           AS   PREFIX.
*更新範囲
 01  TRND-DT.
     03  TRND-DATE         PIC  9(08).
     03  TRND-TIME         PIC  9(06).
 01  TO-DT.
     03  TO-DATE           PIC  9(08).
     03  TO-TIME           PIC  9(06).
*
 01  READ-CNT              PIC  9(07) VALUE 0.
 01  IN-CNT                PIC  9(07) VALUE 0.
 01  HJYOKEN-INV-FLG       PIC  X(03) VALUE SPACE.
*2013/12/11↓
 01  JYO-INVALID-FLG       PIC  9(01) VALUE ZERO.
 01  WK-TOK-F19            PIC  9(11) VALUE ZERO.
 01  WK-TOK-F20            PIC  9(11) VALUE ZERO.
 01  WK-TOK-F21            PIC  9(11) VALUE ZERO.
*2013/12/11↑
*
*帳票出力定義エリア
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SIT0041L".
     02  FILLER              PIC  X(36)  VALUE  SPACE.
     02  FILLER              PIC  N(16)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＊＊　取引先マスタ　リスト　＊＊".
     02  FILLER              PIC  X(16)  VALUE  SPACE.
     02  FILLER              PIC  X(05)  VALUE  "DATE:".
     02  SYSYY               PIC  9999.
     02  FILLER              PIC  X(01)  VALUE  ".".
     02  SYSMM               PIC  Z9.
     02  FILLER              PIC  X(01)  VALUE  ".".
     02  SYSDD               PIC  Z9.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  X(05)  VALUE  "PAGE:".
     02  LPAGE               PIC  ZZZ9.
****  見出し行１２　１４　１９***
 01  MIDASI-12.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(22)  VALUE
         "+------+------+------+".
****  見出し行１３           ****
 01  MIDASI-13.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"責任者".
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"担当者".
     02  FILLER              PIC  X(01)  VALUE  "-".
***  見出し行１５ １６ １７ １８
 01  MIDASI-15.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "-".
*
****  見出し行２             ****
 01  MIDASI-2.
     02  FILLER              PIC  N(03)  VALUE
         NC"取引先" CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(03)  VALUE  "CD ".
     02  FILLER              PIC  N(04)  VALUE
         NC"取引先名" CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(16)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE
         NC"〒" CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"住　所" CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(16)  VALUE  SPACE.
     02  FILLER              PIC  N(07)  VALUE
         NC"ＴＥＬ／ＦＡＸ" CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"自得意先" CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(02)  VALUE  "CD".
     02  FILLER              PIC  N(01)  VALUE
         NC"　"       CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  N(04)  VALUE
         NC"伝票番号" CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"締　日" CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"原／売率" CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"伝発" CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"税端" CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"伝パ" CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"伝桁" CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(04)  VALUE  "CHKD".
****  見出し行２１           ****
 01  MIDASI-21.
* 2011/10/13,S  S.I/NAV
**     02  FILLER              PIC  X(88)  VALUE  SPACE.
*    02  FILLER              PIC  X(79)  VALUE  SPACE.
*    02  FILLER              PIC  N(02)  VALUE
*        NC"発集" CHARACTER  TYPE  IS  NIHONGO.
*    02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  X(70)  VALUE  SPACE.
     02  FILLER              PIC  N(06)  VALUE
         NC"シーズン春秋" CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
* 2011/10/13,E  S.I/NAV
     02  FILLER              PIC  N(04)  VALUE
         NC"代表倉庫" CHARACTER  TYPE  IS  NIHONGO.
* 2012/10/31
**** 02  FILLER              PIC  X(24)  VALUE  SPACE.
     02  FILLER              PIC  X(15)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"出場特定" CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
***
     02  FILLER              PIC  N(02)  VALUE
         NC"発集" CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  N(02)  VALUE
         NC"伝票" CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(04)  VALUE  "ﾀｲﾌﾟ".
*
********09/8/28  管理区分追加
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"管理" CHARACTER  TYPE  IS  NIHONGO.
****  見出し行２１           ****
 01  MIDASI-22      CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(07)  VALUE
         NC"［処理：担当者".
     02  FILLER              PIC  X(14)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"：更新日".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"更新時刻］".
*2013/12/11↓
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"税扱".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
*2013/12/24↓
*    02  FILLER              PIC  N(05)  VALUE
*        NC"計上税区分".
*    02  FILLER              PIC  X(01)  VALUE  SPACE.
*    02  FILLER              PIC  N(01)  VALUE
*        NC"→".
*    02  FILLER              PIC  X(02)  VALUE  " (".
*    02  FILLER              PIC  N(06)  VALUE
*        NC"税区分改訂日".
*    02  FILLER              PIC  X(02)  VALUE  SPACE.
*    02  FILLER              PIC  N(06)  VALUE
*        NC"改訂後税区分".
*    02  FILLER              PIC  X(02)  VALUE  ") ".
*2013/12/24↑
     02  FILLER              PIC  N(06)  VALUE
         NC"税抜計算区分".
*2013/12/11↑
*2013/12/25↓
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE
         NC"→".
     02  FILLER              PIC  X(02)  VALUE  " (".
     02  FILLER              PIC  N(07)  VALUE
         NC"税抜区分改定日".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(06)  VALUE
         NC"税抜計算区分".
     02  FILLER              PIC  X(02)  VALUE  ") ".
*2013/12/25↑

 01  SEN1.
     03  FILLER                  PIC  X(136) VALUE ALL "-".
****  明細行１               ****
 01  MEISAI-1       CHARACTER     TYPE   IS   YB.
     02  TORICD              PIC  9(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TNAME               PIC  N(16).
     02  NYBN1               PIC  XXX.
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  NYBN2               PIC  XXXX.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  JYU1                PIC  N(16).
     02  TEL                 PIC  X(12).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TOKUCD              PIC  XXXXXXXX.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  DENNO               PIC  XXXXXXXXX.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SIME1               PIC  Z9.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SIME2               PIC  Z9.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SIME3               PIC  Z9.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  GENKAR              PIC  9.99.
     02  FILLER              PIC  X(01)  VALUE  "/".
     02  BAIKAR              PIC  9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  KUBUN1              PIC  9.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  KUBUN2              PIC  9.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  KUBUN3              PIC  X.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  KUBUN4              PIC  9.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  KUBUN5              PIC  X.
****  明細行２               ****
 01  MEISAI-2       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  TKANA               PIC  X(15).
*    02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  YUBIN               PIC  X(06).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  JYU2                PIC  N(16).
     02  FAX                 PIC  X(12)  VALUE  SPACE.
* 2011/10/13,S  S.I/NAV
**     02  FILLER              PIC  X(10)  VALUE  SPACE.
*    02  FILLER              PIC  X(01)  VALUE  SPACE.
*    02  MEISAI-2-HCSYU      PIC  X(01). *>発注集計表出力区分
*    02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEISAI-2-HARU       PIC  X(04). *>シーズン開始日春　
     02  MEISAI-2-AKI        PIC  X(04). *>シーズン開始日春　
     02  FILLER              PIC  X(01)  VALUE  SPACE.
* 2011/10/13,E  S.I/NAV
     02  SOKOCD              PIC  99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SOKONM              PIC  N(18).
*** 2012/10/31
***  02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  STOKBN              PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
***
     02  MEISAI-2-HCSYU      PIC  X(01). *>発注集計表出力区分
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  DENCD               PIC  9.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  DENCNM              PIC  N(06).
*
********09/8/28  管理区分追加
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  KANRI               PIC  X(01).
*
****  明細行３               ****
 01  MEISAI-3       CHARACTER     TYPE   IS   YB.
*  2013/12/11↓
*--- MEISA-4へ移動
*--- 02  FILLER              PIC  X(01)  VALUE  SPACE.
*--- 02  UDKBN               PIC  N(04).
*--- 02  TANCD               PIC  X(02).
*--- 02  FILLER              PIC  X(01)  VALUE  SPACE.
*--- 02  TANNM               PIC  N(11).
*--- 02  UPDT                PIC  X(10).
*--- 02  FILLER              PIC  X(01)  VALUE  SPACE.
*--- 02  UPTM                PIC  X(08).
*--- 02  KAKO                PIC  N(01).
     02  FILLER              PIC  X(48)  VALUE  SPACE.
*  2013/12/11↑
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  KEIJYO              PIC  N(05).
     02  BUMON               PIC  X(04).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  BUMNM               PIC  N(05).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  KEIJYO1             PIC  N(06).
     02  BUMHEN              PIC  ZZZZZZZZ.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  KEIJYO2             PIC  N(05).
     02  BUMON2              PIC  ZZZZ.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  BUMNM2              PIC  N(05).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  SYUTT1              PIC  N(04).
     02  SYUCD1              PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SYUNM1              PIC  N(01).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  SYUTT2              PIC  N(04).
     02  SYUCD2              PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SYUNM2              PIC  N(01).
*
*2013/12/11↓ 追加
 01  MEISAI-4       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  UDKBN               PIC  N(04).
     02  TANCD               PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TANNM               PIC  N(11).
     02  UPDT                PIC  X(10).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  UPTM                PIC  X(08).
     02  KAKO                PIC  N(01).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  ZEIA                PIC  N(02).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
*2013/12/24↓
*    02  ZEINM1              PIC  N(05).
*    02  FILLER              PIC  X(01)  VALUE  SPACE.
*    02  YAJIRUSI            PIC  N(01)  VALUE  NC"→".
*    02  KAKKO1              PIC  X(02)  VALUE  " (".
*    02  CHGDAY-YYYY         PIC  X(04).
*    02  SLASH1              PIC  X(01)  VALUE  "/".
*    02  CHGDAY-MM           PIC  XX.
*    02  SLASH2              PIC  X(01)  VALUE  "/".
*    02  CHGDAY-DD           PIC  XX.
*    02  FILLER              PIC  X(01)  VALUE  SPACE.
*    02  ZEINM2              PIC  N(05).
*    02  KAKKO2              PIC  X(03)  VALUE  " ) ".
*2013/12/24↑
     02  ZEIN                PIC  N(05).
*2013/12/11↑
*2013/12/25↓
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  YAJIRUSI            PIC  N(01)  VALUE  NC"→".
     02  KAKKO1              PIC  X(02)  VALUE  " (".
     02  CHGDAY-YYYY         PIC  X(04).
     02  SLASH1              PIC  X(01)  VALUE  "/".
     02  CHGDAY-MM           PIC  XX.
     02  SLASH2              PIC  X(01)  VALUE  "/".
     02  CHGDAY-DD           PIC  XX.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  ZEIN2               PIC  N(05).
     02  KAKKO2              PIC  X(02)  VALUE  " )".
*2013/12/25↑
*
 LINKAGE                   SECTION.
 01  PARA-BUMONCD          PIC X(04).
 01  PARA-TANCD            PIC X(08).
 01  PARA-UPDTDATE         PIC 9(08).
 01  PARA-UPDTIME          PIC 9(06).
 01  PARA-UPDTDATE-E       PIC 9(08).
 01  PARA-UPDTIME-E        PIC 9(06).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                 DIVISION USING PARA-BUMONCD
                                          PARA-TANCD
                                          PARA-UPDTDATE
                                          PARA-UPDTIME
                                          PARA-UPDTDATE-E
                                          PARA-UPDTIME-E.
 DECLARATIVES.
 MSL-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE MSTLOGF.
     DISPLAY     MSL-ERR   UPON      STA.
     DISPLAY     MSL-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      STA.
     DISPLAY     PRT-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      STA.
     DISPLAY     SOK-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR   UPON      STA.
     DISPLAY     TAN-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ERR   UPON      STA.
     DISPLAY     JYO-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC                  SECTION.
*
 PROC-010.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-SW = 9
     PERFORM     END-SEC.
*
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
     MOVE        ZERO      TO        PAGE-CNT.
     MOVE        61        TO        LINE-CNT.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*パラメタ：担当者コード取得
     MOVE     PARA-TANCD          TO   WK-TANCD.
*
     OPEN        INPUT     MSTLOGF ZSOKMS HTANMS HJYOKEN
                 OUTPUT    PRTF.
*帳票初期化*
*マスタ更新履歴ファイル初期ＲＥＡＤ
     MOVE     "01"                TO   MSL-F01.
     MOVE     PARA-BUMONCD        TO   MSL-F02
     MOVE     WK-TANCD            TO   MSL-F03.
     MOVE     PARA-UPDTDATE       TO   MSL-F05.
     MOVE     PARA-UPDTIME        TO   MSL-F06.

     START    MSTLOGF            KEY IS >= MSL-F01
                                           MSL-F02
                                           MSL-F03
                                           MSL-F05
                                           MSL-F06
              INVALID  MOVE  9    TO   END-SW
                       GO TO      INIT-EXIT.
     PERFORM  READMSL-SEC.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 MAIN-SEC                  SECTION.
*
     PERFORM     MEIEDT-SEC.
*2013/12/11↓
**** IF  (LINE-CNT  >  60)
     IF  (LINE-CNT  >  53)
*2013/12/11↑
          PERFORM MIDA-SEC
     END-IF.
*
     PERFORM     MEIWRT-SEC.
*
     PERFORM     READMSL-SEC.
*
 MAIN-EXIT.
     EXIT.
**********************
*マスタ更新履歴ファイルＲＥＡＤ*
**********************
 READMSL-SEC               SECTION.
*
     READ   MSTLOGF        AT        END
         MOVE    9         TO        END-SW
         GO                TO        READMSL-EXIT.
*
     MOVE   MSL-F05        TO        TRND-DATE.
     MOVE   MSL-F06        TO        TRND-TIME.
     MOVE   PARA-UPDTDATE-E TO       TO-DATE.
     MOVE   PARA-UPDTIME-E  TO       TO-TIME.

     IF  (MSL-F01  >  "01") OR (MSL-F02  >  PARA-BUMONCD) OR
         (MSL-F03  >  PARA-TANCD) OR (TRND-DT > TO-DT)
         MOVE    9         TO        END-SW
         GO TO             READMSL-EXIT.
     ADD    1              TO        READ-CNT.
*
 READMSL-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
*
     CLOSE       PRTF      MSTLOGF   ZSOKMS  HTANMS HJYOKEN.
     DISPLAY  "SIT0041L READ =" READ-CNT UPON STA.
     DISPLAY  "SIT0041L IN   =" IN-CNT   UPON STA.
 END-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 MIDA-SEC                  SECTION.
*
     IF  PAGE-CNT  >  ZERO
         MOVE       SPACE   TO      P-REC
         WRITE      P-REC   AFTER   PAGE
     END-IF.
*
     MOVE        WK-YS     TO        YY1.
     MOVE        WK-Y      TO        YY2.
     MOVE        YYYY      TO        SYSYY.
     MOVE        WK-M      TO        SYSMM.
     MOVE        WK-D      TO        SYSDD.
     ADD         1         TO        PAGE-CNT.
     MOVE        ZERO      TO        LINE-CNT.
     MOVE        PAGE-CNT  TO        LPAGE.
**************
*帳票書き出し*
**************
     WRITE      P-REC   FROM    MIDASI-1   AFTER  2.
     WRITE      P-REC   FROM    MIDASI-12  AFTER  1.
     WRITE      P-REC   FROM    MIDASI-13  AFTER  1.
     WRITE      P-REC   FROM    MIDASI-12  AFTER  1.
     WRITE      P-REC   FROM    MIDASI-15  AFTER  1.
     WRITE      P-REC   FROM    MIDASI-15  AFTER  1.
     WRITE      P-REC   FROM    MIDASI-15  AFTER  1.
     WRITE      P-REC   FROM    MIDASI-15  AFTER  1.
     WRITE      P-REC   FROM    MIDASI-12  AFTER  1.
     WRITE      P-REC   FROM    MIDASI-2   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-21  AFTER  1.
     WRITE      P-REC   FROM    MIDASI-22  AFTER  1.
     WRITE      P-REC   FROM    SEN1       AFTER  1.
*
     ADD         14        TO        LINE-CNT.
*
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIEDT-SEC                  SECTION.
     MOVE        MSL-F08   TO        TOK-REC.
     MOVE        TOK-F01   TO        TORICD.
     MOVE        TOK-F02   TO        TNAME.
     MOVE        TOK-F04   TO        TKANA.
     MOVE        TOK-F801  TO        NYBN1.
     MOVE        TOK-F802  TO        NYBN2.
     MOVE        TOK-F05   TO        YUBIN.
     MOVE        TOK-F06   TO        JYU1.
     MOVE        TOK-F07   TO        JYU2.
     MOVE        TOK-F08   TO        TEL.
     MOVE        TOK-F09   TO        FAX.
* 2011/10/13,S  S.I/NAV
*  発注集計表出力区分
*    MOVE  TOK-FIL1(2:1)   TO  MEISAI-2-HCSYU.
     MOVE  TOK-FIL1(2:1)   TO  MEISAI-2-HCSYU.
     MOVE  TOK-F70         TO  MEISAI-2-HARU.
     MOVE  TOK-F71         TO  MEISAI-2-AKI.
* 2011/10/13,E  S.I/NAV
     MOVE        TOK-F52   TO        TOKUCD.
     MOVE        TOK-F54   TO        DENNO.
     MOVE        TOK-F12(1) TO       SIME1.
     MOVE        TOK-F12(2) TO       SIME2.
     MOVE        TOK-F12(3) TO       SIME3.
     MOVE        TOK-F55   TO        GENKAR.
     MOVE        TOK-F56   TO        BAIKAR.
     MOVE        TOK-F84   TO        KUBUN1.
     MOVE        TOK-F88   TO        KUBUN2.
     MOVE        TOK-F91   TO        KUBUN3.
     MOVE        TOK-F92   TO        KUBUN4.
     MOVE        TOK-F93   TO        KUBUN5.
     MOVE        TOK-F81   TO        SOKOCD  SOK-F01.
*
********09/8/28  管理区分追加
     MOVE        TOK-F78   TO        KANRI.
*
*** 2012/10/31
     MOVE        TOK-F95   TO        STOKBN.
***
***  倉庫名称取得
     READ        ZSOKMS
       INVALID
                 MOVE   SPACE       TO  SOKONM
       NOT INVALID
                 MOVE  SOK-F02      TO  SOKONM
     END-READ.
*## 1999/11/02 NAV T.T START ##*
     MOVE        TOK-F79   TO        DENCD.
     EVALUATE    DENCD
         WHEN    0
                 MOVE NC"指定無し　　"  TO   DENCNM
         WHEN    1
                 MOVE NC"ＴＡ_型　　"  TO   DENCNM
         WHEN    2
                 MOVE NC"ＴＡ_型　　"  TO   DENCNM
         WHEN    3
                 MOVE NC"タイプ用　　"  TO   DENCNM
         WHEN    4
                 MOVE NC"タイプ用_型"  TO   DENCNM
     END-EVALUATE.
*
*更新履歴
     EVALUATE    MSL-F04
         WHEN    "1"
                 MOVE NC"登録"          TO   HEN-KUBUNNM
         WHEN    "2"
                 MOVE NC"修正"          TO   HEN-KUBUNNM
         WHEN    "3"
                 MOVE NC"削除"          TO   HEN-KUBUNNM
     END-EVALUATE.
     MOVE        HEN-KUBUN              TO   UDKBN.
*
     MOVE        MSL-F03                TO   WK-TANCD.
     MOVE        WK-TANCD1              TO   TANCD.
*担当者名取得
     MOVE        MSL-F02                TO   TAN-F01.
     MOVE        WK-TANCD1              TO   TAN-F02.
     READ        HTANMS
       INVALID
                 MOVE   SPACE           TO  HEN-TANNM
       NOT INVALID
                 MOVE   TAN-F03         TO  HEN-TANNM
     END-READ.
*
     MOVE        HEN-TAN                TO  TANNM.
     MOVE        MSL-F05(1:4)           TO  HEN-DATE-YYYY.
     MOVE        MSL-F05(5:2)           TO  HEN-DATE-MM.
     MOVE        MSL-F05(7:2)           TO  HEN-DATE-DD.
     MOVE        HEN-DATE               TO  UPDT.
*
     MOVE        MSL-F06(1:2)           TO  HEN-TIME-HH.
     MOVE        MSL-F06(3:2)           TO  HEN-TIME-MM.
     MOVE        MSL-F06(5:2)           TO  HEN-TIME-SS.
     MOVE        HEN-TIME               TO  UPTM.
*
     MOVE        NC"］"                 TO  KAKO.
*2009/09/30 部門ＣＤ追加 START
     MOVE        NC"計上部門："         TO  KEIJYO.
     MOVE        TOK-F75                TO  BUMON.
     MOVE        TOK-F75                TO  JYO-F02.
     PERFORM     HJYOKEN-READ-SEC.
     IF          HJYOKEN-INV-FLG = SPACE
                 MOVE  JYO-F03          TO  BUMNM
     ELSE
                 MOVE  ALL NC"＊"       TO  BUMNM
     END-IF.
     MOVE        NC"部門変更日："       TO  KEIJYO1.
     MOVE        TOK-F76                TO  BUMHEN.
     MOVE        NC"変更部門："         TO  KEIJYO2.
     MOVE        TOK-F77                TO  BUMON2.
     MOVE        TOK-F77                TO  JYO-F02.
     PERFORM     HJYOKEN-READ-SEC.
     IF          HJYOKEN-INV-FLG = SPACE
                 MOVE  JYO-F03          TO  BUMNM2
     END-IF.
*2011/04/14 ST T.TAKAHASHI
*<出荷オンライン区分>
     MOVE        NC"出荷オ："           TO  SYUTT1.
     MOVE        TOK-FIL1(3:1)         TO  SYUCD1.
     IF          TOK-FIL1(3:1) = SPACE
                 MOVE NC"無"            TO  SYUNM1
     ELSE
                 MOVE NC"有"            TO  SYUNM1
     END-IF.
*<出荷手書区分>
     MOVE        NC"出荷手："           TO  SYUTT2.
     MOVE        TOK-FIL1(4:1)         TO  SYUCD2.
     IF          TOK-FIL1(4:1) = SPACE
                 MOVE NC"無"            TO  SYUNM2
     ELSE
                 MOVE NC"有"            TO  SYUNM2
     END-IF.
*2011/04/14 ED T.TAKAHASHI
*
*2013/12/11↓
*<税扱い区分>
 ZEIA.
     EVALUATE    TOK-F18
          WHEN   1
                 MOVE NC"内税"          TO  ZEIA
          WHEN   2
                 MOVE NC"外税"          TO  ZEIA
          WHEN   OTHER
                 MOVE NC"？？"          TO  ZEIA
     END-EVALUATE.
*2013/12/24↓
*<計上税区分>
*ZEINM1.
*    MOVE        50                     TO  JYO-F01.
*    MOVE        TOK-F19                TO  WK-TOK-F19.
*    MOVE        WK-TOK-F19(11:1)       TO  JYO-F02.
*    PERFORM     HJYOKEN-READ-SUB.
*    IF          JYO-INVALID-FLG   =    1
*                MOVE  NC"？？？？？"   TO  ZEINM1
*    ELSE
*                MOVE  JYO-F03          TO  ZEINM1
*    END-IF.
*<税区分改訂日>
*CHGDAY.
*    MOVE        NC"→"                 TO  YAJIRUSI.
*    MOVE        " ("                   TO  KAKKO1.
*    MOVE        TOK-F20                TO  WK-TOK-F20.
*    IF          TOK-F20      =         ZERO
*                MOVE         SPACE     TO  CHGDAY-YYYY
*                                           SLASH1
*                                           CHGDAY-MM
*                                           SLASH2
*                                           CHGDAY-DD
*                GO           TO        ZEINM2
*    END-IF.
*    MOVE        WK-TOK-F20(4:4)        TO  CHGDAY-YYYY.
*    MOVE        "/"                    TO  SLASH1.
*    MOVE        WK-TOK-F20(8:2)        TO  CHGDAY-MM.
*    MOVE        "/"                    TO  SLASH2.
*    MOVE        WK-TOK-F20(10:2)       TO  CHGDAY-DD.
*<改定後計上税区分>
*ZEINM2.
*    IF          TOK-F20      =         ZERO
*                MOVE  SPACE  TO        ZEINM2
*                GO           TO        ZEIN
*    END-IF.
*    MOVE        50                     TO  JYO-F01.
*    MOVE        TOK-F21                TO  WK-TOK-F21.
*    MOVE        WK-TOK-F21(11:1)       TO  JYO-F02.
*    PERFORM     HJYOKEN-READ-SUB.
*    IF          JYO-INVALID-FLG   =    1
*                MOVE  NC"？？？？？"   TO  ZEINM2
*    ELSE
*                MOVE  JYO-F03          TO  ZEINM2
*    END-IF.
*2013/12/24↑
*<税抜き計算区分>
 ZEIN.
*2013/12/24↓
*    MOVE        " )"                   TO  KAKKO2.
*2013/12/24↑
*2013/12/25↓
*    EVALUATE    TOK-F22
     EVALUATE    TOK-F20
*2013/12/25↑
          WHEN   0
                 MOVE NC"計算しない"    TO  ZEIN
          WHEN   1
                 MOVE NC"計算する　"    TO  ZEIN
          WHEN   OTHER
                 MOVE NC"？？？？？"    TO  ZEIN
     END-EVALUATE.
*2013/12/11↑
*
*2013/12/25↓
*<税抜き計算区分改訂日>
 CHGDAY.
     MOVE        NC"→"                 TO  YAJIRUSI.
     MOVE        " ("                   TO  KAKKO1.
     MOVE        TOK-F21                TO  WK-TOK-F21.
     IF          TOK-F21 =  ZERO
                 MOVE    SPACE              TO  CHGDAY-YYYY
                                                SLASH1
                                                CHGDAY-MM
                                                SLASH2
                                                CHGDAY-DD
     ELSE
                 MOVE    WK-TOK-F21(4:4)    TO  CHGDAY-YYYY
                 MOVE    "/"                TO  SLASH1
                 MOVE    WK-TOK-F21(8:2)    TO  CHGDAY-MM
                 MOVE    "/"                TO  SLASH2
                 MOVE    WK-TOK-F21(10:2)   TO  CHGDAY-DD
     END-IF.
*<改定後税抜き計算区分>
 ZEIN2.
     EVALUATE    TOK-F22
          WHEN   0
                 MOVE NC"計算しない"    TO  ZEIN2
          WHEN   1
                 MOVE NC"計算する　"    TO  ZEIN2
          WHEN   OTHER
                 MOVE NC"？？？？？"    TO  ZEIN2
     END-EVALUATE.
     IF          TOK-F21      =         ZERO
                 MOVE         SPACE     TO  ZEIN2
     END-IF.
     MOVE        " )"                   TO  KAKKO2.
*2013/12/25↑
*
 MEIEDT-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し                  *
**********************************************************
 MEIWRT-SEC                   SECTION.
*
     ADD        1          TO        IN-CNT.
**************
*帳票書き出し*
**************
     WRITE      P-REC   FROM    MEISAI-1   AFTER  1.
     WRITE      P-REC   FROM    MEISAI-2   AFTER  1.
     WRITE      P-REC   FROM    MEISAI-3   AFTER  1.
*    2013/12/11↓
     WRITE      P-REC   FROM    MEISAI-4   AFTER  1.
*    2013/12/11↑
     WRITE      P-REC   FROM    SEN1       AFTER  1.
*
*    2013/12/11↓
*----ADD         4         TO        LINE-CNT.
     ADD         5         TO        LINE-CNT.
*    2013/12/11↑
*
 MEIWRT-EXIT.
     EXIT.
****************************************************************
*            条件ファイル読込
****************************************************************
 HJYOKEN-READ-SEC            SECTION.
*
     MOVE    "22"      TO    JYO-F01.
*
     READ    HJYOKEN
             INVALID     MOVE "INV"     TO   HJYOKEN-INV-FLG
             NOT INVALID MOVE  SPACE    TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*      3.0        条件ファイル索引                             *
****************************************************************
 HJYOKEN-READ-SUB       SECTION.
     MOVE     0                  TO   JYO-INVALID-FLG.
     READ     HJYOKEN
        INVALID
              MOVE      1        TO   JYO-INVALID-FLG
     END-READ.
 HJYOKEN-READ-END.
     EXIT.
