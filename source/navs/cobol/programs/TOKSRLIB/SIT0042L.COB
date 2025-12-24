****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　基幹
*    サブシステム　　　　：　マスタ保守
*    モジュール名　　　　：　取引先マスタ　メンテリスト
*    作成日／作成者　　　：　2019.03.15 INOUE
*    　　　　　　　　　　：　S11300660 マスタリスト改善　　　
*    更新履歴　　　　　　：
*    更新日／更新者　　　：　
*
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SIT0042L.
*                  流用元：SIT0041L
 AUTHOR.                   NAV.
 DATE-WRITTEN.             2019.03.15.
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
     SELECT     MSTLOGL1   ASSIGN    TO        MSTLOGL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       MSL-F01
                                               MSL-F02
                                               MSL-F03
                                               MSL-F04
                                               MSL-F05
                                               MSL-F06
                                               MSL-F07
                           FILE      STATUS    MSL-ST.
*担当者マスタ
     SELECT     TANMS1     ASSIGN    TO        TANMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TAN-F01
                                               TAN-F02
                           FILE      STATUS    TAN-ST.
*プリンター
     SELECT     PRTF       ASSIGN    TO        LP-04.
*
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
*マスタ更新履歴ファイル
 FD  MSTLOGL1
     LABEL       RECORD    IS        STANDARD.
     COPY        MSTLOGL1  OF        XFDLIB
     JOINING     MSL       AS        PREFIX.
*担当者マスタ
 FD  TANMS1
     LABEL       RECORD    IS        STANDARD.
     COPY        TANMS1    OF        XFDLIB
     JOINING     TAN       AS        PREFIX.
*プリンター
 FD    PRTF      LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
*A
*マスタレコードエリア（取引先マスタ）
     COPY    HTOKMS        OF   XFDLIB
     JOINING TOKL          AS   PREFIX.
     COPY    HTOKMS        OF   XFDLIB
     JOINING TOKR          AS   PREFIX.
*改ページ条件
 01  NEW-KEY.
     03  NEW-MSL-F04       PIC  X(01).
     03  NEW-MSL-F05       PIC  9(08).
     03  NEW-MSL-F06       PIC  9(06).
 01  OLD-KEY.
     03  OLD-MSL-F04       PIC  X(01).
     03  OLD-MSL-F05       PIC  9(08).
     03  OLD-MSL-F06       PIC  9(06).
*A
*01  WK-TOKL-F19            PIC  9(11) VALUE ZERO.
*01  WK-TOKL-F20            PIC  9(11) VALUE ZERO.
 01  WK-TOKL-F21            PIC  9(11) VALUE ZERO.
 01  WK-TOKL-F76            PIC  9(08) VALUE ZERO.
*01  WK-TOKR-F19            PIC  9(11) VALUE ZERO.
*01  WK-TOKR-F20            PIC  9(11) VALUE ZERO.
 01  WK-TOKR-F21            PIC  9(11) VALUE ZERO.
 01  WK-TOKR-F76            PIC  9(08) VALUE ZERO.
*
*ファイルＳＴＡＴＵＳ
 01  FILE-STATUS.
     03  MSL-ST            PIC X(02).
     03  MSL-ST1           PIC X(04).
     03  PRT-ST            PIC X(02).
     03  PRT-ST1           PIC X(04).
     03  TAN-ST            PIC X(02).
*年度
 01  YYYY.
     03  YY1               PIC  9(02).
     03  YY2               PIC  9(02).
 01  WK-AREA.
     03  END-FLG           PIC X(03) VALUE    SPACE.
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
*担当者コード
 01  WK-TANCD.
     03  WK-TANCD1         PIC  X(02).
     03  WK-FILLER         PIC  X(06).
*
*更新範囲
 01  TRND-DT.
     03  TRND-DATE         PIC  9(08).
     03  TRND-TIME         PIC  9(06).
 01  TO-DT.
     03  TO-DATE           PIC  9(08).
     03  TO-TIME           PIC  9(06).
*
 01  READ-CNT              PIC  9(07) VALUE 0.
 01  WT-CNT                PIC  9(07) VALUE 0.
*A
*帳票出力定義エリア
****  見出し行０             ****
 01  MIDASI-0.
     02  FILLER              PIC  X(108) VALUE  SPACE.
     02  FILLER              PIC  X(22)  VALUE
         "+------+------+------+".
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SIT0042L".
     02  FILLER              PIC  X(15)  VALUE  SPACE.
     02  FILLER              PIC  N(14)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＜取引先マスタメンテリスト＞".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  MIDASI-1-1          CHARACTER  TYPE  IS  NIHONGO.
         03  SYSYY           PIC  9999.
         03  FILLER          PIC  N(01)  VALUE  NC"年".
         03  SYSMM           PIC  Z9.
         03  FILLER          PIC  N(01)  VALUE  NC"月".
         03  SYSDD           PIC  Z9.
         03  FILLER          PIC  N(01)  VALUE  NC"日".
         03  FILLER          PIC  X(01)  VALUE  SPACE.
         03  TIMEHH          PIC  9(02).
         03  FILLER          PIC  X(01)  VALUE  ":".
         03  TIMEMM          PIC  9(02).
         03  FILLER          PIC  X(01)  VALUE  ":".
         03  TIMESS          PIC  9(02).
         03  FILLER          PIC  X(01)  VALUE  " ".
         03  LPAGE           PIC  ZZ9.
         03  FILLER          PIC  N(01)  VALUE  NC"頁".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  X(22)  VALUE
         "-      -      -      -".
****  見出し行２　***
 01  MIDASI-2.
     02  FILLER              PIC  X(108) VALUE  SPACE.
     02  FILLER              PIC  X(22)  VALUE
         "+------+------+------+".
****  見出し行３ ****
 01  MIDASI-3.
     02  FILLER              PIC  X(108) VALUE  SPACE.
     02  FILLER              PIC  X(22)  VALUE
         "-      -      -      -".
*
****  見出し行４ ****
 01  MIDASI-4                CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"担当者：".
     02  MIDASI-4-TANCD      PIC  X(07).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MIDASI-4-TANNM      PIC  N(10).
     02  FILLER              PIC  X(71)  VALUE  SPACE.
     02  FILLER              PIC  X(22)  VALUE
         "-      -      -      -".
****  見出し行５ ****
 01  MIDASI-5.
     02  FILLER              PIC  X(108) VALUE  SPACE.
     02  FILLER              PIC  X(22)  VALUE
         "-      -      -      -".
****  見出し行６ ****
 01  MIDASI-6                CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"モード：".
     02  MIDASI-6-MODE       PIC  N(02).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"更新日：".
     02  MIDASI-6-YYYY       PIC  9(04).
     02  FILLER              PIC  N(01)  VALUE  NC"年".
     02  MIDASI-6-MM         PIC  9(02).
     02  FILLER              PIC  N(01)  VALUE  NC"月".
     02  MIDASI-6-DD         PIC  9(02).
     02  FILLER              PIC  N(01)  VALUE  NC"日".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE  NC"更新時刻：".
     02  MIDASI-6-TIME       PIC  X(08).
     02  FILLER              PIC  X(51)  VALUE  SPACE.
     02  FILLER              PIC  X(22)  VALUE
         "-      -      -      -".
****  見出し行７ ****
 01  MIDASI-7.
     02  FILLER              PIC  X(108) VALUE  SPACE.
     02  FILLER              PIC  X(22)  VALUE
         "+------+------+------+".
****  見出し行８ ****
 01  MIDASI-8                CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE  NC"＜項目名＞".
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  MIDASI-8-MODE-L     PIC  N(03).
     02  FILLER              PIC  X(29)  VALUE  SPACE.
     02  MIDASI-8-MODE-R     PIC  N(03).
*
****  明細行1               ****
 01  MEISAI-01                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　相手取引先コード　．．．".
     02  MEISAI-01-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-01-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-01-L         PIC  9(08).
         03  FILLER              PIC  X(24)  VALUE  SPACE.
         03  MEISAI-01-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-01-R         PIC  9(08).
****  明細行2               ****
 01  MEISAI-02                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　自社得意先コード　．．．".
     02  MEISAI-02-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-02-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-02-L         PIC  9(08).
         03  FILLER              PIC  X(24)  VALUE  SPACE.
         03  MEISAI-02-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-02-R         PIC  9(08).
****  明細行3               ****
 01  MEISAI-03                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　取引先名　．．．．．．．".
     02  MEISAI-03-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-03-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-03-L         PIC  N(15).
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-03-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-03-R         PIC  N(15).
****  明細行4               ****
 01  MEISAI-04                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　取引先カナ名．．．．．．".
     02  MEISAI-04-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-04-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-04-L         PIC  X(15).
         03  FILLER              PIC  X(17)  VALUE  SPACE.
         03  MEISAI-04-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-04-R         PIC  X(15).
****  明細行5               ****
 01  MEISAI-05                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　取引先略名　．．．．．．".
     02  MEISAI-05-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-05-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-05-L         PIC  N(10).
         03  FILLER              PIC  X(12)  VALUE  SPACE.
         03  MEISAI-05-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-05-R         PIC  N(10).
****  明細行6               ****
 01  MEISAI-06                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　郵便番号　．．．．．．．".
     02  MEISAI-06-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-06-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-06-L         PIC  X(08).
         03  FILLER              PIC  X(24)  VALUE  SPACE.
         03  MEISAI-06-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-06-R         PIC  X(08).
****  明細行7               ****
 01  MEISAI-07                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　住所　．．．．．．．．．".
     02  MEISAI-07-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-07-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-07-L         PIC  N(15).
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-07-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-07-R         PIC  N(15).
****  明細行8               ****
 01  MEISAI-08                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　　　　　　　　　　　　　".
     02  MEISAI-08-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-08-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-08-L         PIC  N(15).
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-08-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-08-R         PIC  N(15).
****  明細行9               ****
 01  MEISAI-09                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　電話番号　．．．．．．．".
     02  MEISAI-09-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-09-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-09-L         PIC  X(12).
         03  FILLER              PIC  X(20)  VALUE  SPACE.
         03  MEISAI-09-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-09-R         PIC  X(12).
****  明細行10              ****
 01  MEISAI-10                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　ファックス　．．．．．．".
     02  MEISAI-10-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-10-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-10-L         PIC  X(12).
         03  FILLER              PIC  X(20)  VALUE  SPACE.
         03  MEISAI-10-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-10-R         PIC  X(12).
****  明細行11              ****
 01  MEISAI-11                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆請求締日　．．．．．．．".
     02  MEISAI-11-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-11-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-11-L         PIC  X(08).
         03  FILLER              PIC  X(24)  VALUE  SPACE.
         03  MEISAI-11-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-11-R         PIC  X(08).
****  明細行12              ****
 01  MEISAI-12                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　伝票発行区分　．．．．．".
     02  MEISAI-12-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-12-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-12-L         PIC  N(03).
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-12-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-12-R         PIC  N(03).
****  明細行13              ****
 01  MEISAI-13                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　付番区分　．．．．．．．".
     02  MEISAI-13-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-13-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-13-L         PIC  N(03).
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-13-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-13-R         PIC  N(03).
****  明細行14              ****
 01  MEISAI-14                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　消費税端数区分　．．．．".
     02  MEISAI-14-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-14-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-14-L         PIC  N(04).
         03  FILLER              PIC  X(24)  VALUE  SPACE.
         03  MEISAI-14-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-14-R         PIC  N(04).
****  明細行15              ****
 01  MEISAI-15                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆伝票番号ケタ数　．．．．".
     02  MEISAI-15-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-15-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-15-L         PIC  9(01).
         03  FILLER              PIC  X(31)  VALUE  SPACE.
         03  MEISAI-15-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-15-R         PIC  9(01).
****  明細行16              ****
 01  MEISAI-16                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆行区分　．．．．．．．．".
     02  MEISAI-16-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-16-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-16-L         PIC  9(02).
         03  FILLER              PIC  X(30)  VALUE  SPACE.
         03  MEISAI-16-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-16-R         PIC  9(02).
****  明細行17              ****
 01  MEISAI-17                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆チェックデジット区分　．".
     02  MEISAI-17-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-17-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-17-L         PIC  N(07).
         03  FILLER              PIC  X(18)  VALUE  SPACE.
         03  MEISAI-17-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-17-R         PIC  N(07).
****  明細行18              ****
 01  MEISAI-18                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆チェックデジット区分２　".
     02  MEISAI-18-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-18-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-18-L         PIC  N(07).
         03  FILLER              PIC  X(18)  VALUE  SPACE.
         03  MEISAI-18-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-18-R         PIC  N(07).
****  明細行19              ****
 01  MEISAI-19                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆税計算区分　．．．．．．".
     02  MEISAI-19-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-19-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-19-L         PIC  N(03).
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-19-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-19-R         PIC  N(03).
****  明細行20              ****
 01  MEISAI-20                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆売価チェック区分　．．．".
     02  MEISAI-20-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-20-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-20-L         PIC  N(03).
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-20-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-20-R         PIC  N(03).
****  明細行21              ****
 01  MEISAI-21                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆管理区分　．．．．．．．".
     02  MEISAI-21-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-21-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-21-L         PIC  N(03).
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-21-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-21-R         PIC  N(03).
****  明細行22              ****
 01  MEISAI-22                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　出荷送信（オンライン）　".
     02  MEISAI-22-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-22-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-22-L         PIC  N(01).
         03  FILLER              PIC  X(30)  VALUE  SPACE.
         03  MEISAI-22-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-22-R         PIC  N(01).
****  明細行23              ****
 01  MEISAI-23                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　出荷送信（手書）　．．．".
     02  MEISAI-23-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-23-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-23-L         PIC  N(01).
         03  FILLER              PIC  X(30)  VALUE  SPACE.
         03  MEISAI-23-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-23-R         PIC  N(01).
****  明細行24              ****
 01  MEISAI-24                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　売上用　伝票ＮＯ　．．．".
     02  MEISAI-24-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-24-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-24-L         PIC  9(09).
         03  FILLER              PIC  X(23)  VALUE  SPACE.
         03  MEISAI-24-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-24-R         PIC  9(09).
****  明細行25              ****
 01  MEISAI-25                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　売上用　範囲伝票ＮＯ　．".
     02  MEISAI-25-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-25-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-25-L-1       PIC  9(09).
         03  MEISAI-25-L-2       PIC  N(01).
         03  MEISAI-25-L-3       PIC  9(09).
         03  FILLER              PIC  X(12)  VALUE  SPACE.
         03  MEISAI-25-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-25-R-1       PIC  9(09).
         03  MEISAI-25-R-2       PIC  N(01).
         03  MEISAI-25-R-3       PIC  9(09).
****  明細行26              ****
 01  MEISAI-26                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　付番用　伝票ＮＯ　．．．".
     02  MEISAI-26-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-26-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-26-L         PIC  9(09).
         03  FILLER              PIC  X(23)  VALUE  SPACE.
         03  MEISAI-26-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-26-R         PIC  9(09).
****  明細行27              ****
 01  MEISAI-27                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　付番用　範囲伝票ＮＯ　．".
     02  MEISAI-27-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-27-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-27-L-1       PIC  9(09).
         03  MEISAI-27-L-2       PIC  N(01).
         03  MEISAI-27-L-3       PIC  9(09).
         03  FILLER              PIC  X(12)  VALUE  SPACE.
         03  MEISAI-27-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-27-R-1       PIC  9(09).
         03  MEISAI-27-R-2       PIC  N(01).
         03  MEISAI-27-R-3       PIC  9(09).
****  明細行28              ****
 01  MEISAI-28                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆代表倉庫コード　．．．．".
     02  MEISAI-28-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-28-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-28-L         PIC  X(02).
         03  FILLER              PIC  X(30)  VALUE  SPACE.
         03  MEISAI-28-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-28-R         PIC  X(02).
****  明細行29              ****
 01  MEISAI-29                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆出荷場所決定区分　．．．".
     02  MEISAI-29-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-29-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-29-L         PIC  N(06).
         03  FILLER              PIC  X(20)  VALUE  SPACE.
         03  MEISAI-29-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-29-R         PIC  N(06).
****  明細行30              ****
 01  MEISAI-30                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆指定伝票タイプ　．．．．".
     02  MEISAI-30-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-30-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-30-L         PIC  N(06).
         03  FILLER              PIC  X(20)  VALUE  SPACE.
         03  MEISAI-30-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-30-R         PIC  N(06).
****  明細行31              ****
 01  MEISAI-31                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　シーズン開始日（春）　．".
     02  MEISAI-31-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-31-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-31-L         PIC  X(05).
         03  FILLER              PIC  X(27)  VALUE  SPACE.
         03  MEISAI-31-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-31-R         PIC  X(05).
****  明細行32              ****
 01  MEISAI-32                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　シーズン開始日（秋）　．".
     02  MEISAI-32-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-32-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-32-L         PIC  X(05).
         03  FILLER              PIC  X(27)  VALUE  SPACE.
         03  MEISAI-32-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-32-R         PIC  X(05).
****  明細行33              ****
 01  MEISAI-33                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆発注集計表出力区分　．．".
     02  MEISAI-33-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-33-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-33-L         PIC  N(02).
         03  FILLER              PIC  X(28)  VALUE  SPACE.
         03  MEISAI-33-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-33-R         PIC  N(02).
****  明細行34              ****
 01  MEISAI-34                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆税扱区分　．．．．．．．".
     02  MEISAI-34-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-34-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-34-L         PIC  N(02).
         03  FILLER              PIC  X(28)  VALUE  SPACE.
         03  MEISAI-34-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-34-R         PIC  N(02).
****  明細行35              ****
 01  MEISAI-35                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆伝票纏め区分　．．．．．".
     02  MEISAI-35-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-35-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-35-L         PIC  N(05).
         03  FILLER              PIC  X(22)  VALUE  SPACE.
         03  MEISAI-35-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-35-R         PIC  N(05).
****  明細行36              ****
 01  MEISAI-36                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆売上計上時税抜　．．．．".
     02  MEISAI-36-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-36-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-36-L         PIC  N(03).
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-36-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-36-R         PIC  N(03).
****  明細行37              ****
 01  MEISAI-37                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆　　　　　改定後税抜　．".
     02  MEISAI-37-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-37-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-37-L         PIC  N(03).
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-37-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-37-R         PIC  N(03).
****  明細行38              ****
 01  MEISAI-38                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆　　　　　改訂日　．．．".
     02  MEISAI-38-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-38-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-38-L         PIC  X(10).
         03  FILLER              PIC  X(22)  VALUE  SPACE.
         03  MEISAI-38-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-38-R         PIC  X(10).
****  明細行39              ****
 01  MEISAI-39                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆計上部門コード　．．．．".
     02  MEISAI-39-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-39-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-39-L         PIC  X(04).
         03  FILLER              PIC  X(28)  VALUE  SPACE.
         03  MEISAI-39-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-39-R         PIC  X(04).
****  明細行40              ****
 01  MEISAI-40                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆計上部門変更日　．．．．".
     02  MEISAI-40-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-40-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-40-L         PIC  X(10).
         03  FILLER              PIC  X(22)  VALUE  SPACE.
         03  MEISAI-40-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-40-R         PIC  X(10).
****  明細行41              ****
 01  MEISAI-41                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆変更部門コード　．．．．".
     02  MEISAI-41-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-41-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-41-L         PIC  X(04).
         03  FILLER              PIC  X(28)  VALUE  SPACE.
         03  MEISAI-41-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-41-R         PIC  X(04).
*#2019/09/27 NAV ST
****  明細行42              ****
 01  MEISAI-42                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"☆請求区分．．．．．．．．".
     02  MEISAI-42-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-42-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-42-L         PIC  N(05).
         03  FILLER              PIC  X(22)  VALUE  SPACE.
         03  MEISAI-42-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-42-R         PIC  N(05).
*#2019/09/27 NAV ED
*
 LINKAGE                   SECTION.
 01  PARA-BUMONCD          PIC X(04).
 01  PARA-TANCD            PIC X(02).
 01  PARA-UPDTDATE-F       PIC 9(08).
 01  PARA-UPDTDATE-T       PIC 9(08).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                 DIVISION USING PARA-BUMONCD
                                          PARA-TANCD
                                          PARA-UPDTDATE-F
                                          PARA-UPDTDATE-T.
 DECLARATIVES.
 MSL-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE MSTLOGL1.
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
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE TANMS1.
     DISPLAY     TAN-ERR   UPON      STA.
     DISPLAY     TAN-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＧＲＡＮＤ　                        *
***********************************************************
 PROC-SEC                  SECTION.
*
 PROC-010.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-FLG = "END".
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
     MOVE        2         TO        LINE-CNT.
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
     OPEN        INPUT     MSTLOGL1 TANMS1.
     OPEN        OUTPUT    PRTF.
*
*担当者名取得
     MOVE        PARA-BUMONCD           TO   TAN-F01.
     MOVE        PARA-TANCD             TO   TAN-F02.
     READ        TANMS1
       INVALID
                 MOVE   ALL NC"＊"      TO  TAN-F03
     END-READ.
*
*マスタ更新履歴ファイル初期ＲＥＡＤ
     MOVE     "01"                TO   MSL-F01.
     MOVE     PARA-BUMONCD        TO   MSL-F02
     MOVE     WK-TANCD            TO   MSL-F03.
     MOVE     "1"                 TO   MSL-F04.
     MOVE     PARA-UPDTDATE-F     TO   MSL-F05.
     MOVE     ZERO                TO   MSL-F06.
     MOVE     ZERO                TO   MSL-F07.
*
     START    MSTLOGL1     KEY IS >=   MSL-F01
                                       MSL-F02
                                       MSL-F03
                                       MSL-F04
                                       MSL-F05
                                       MSL-F06
                                       MSL-F07
        INVALID  MOVE "END" TO     END-FLG
                 GO         TO     INIT-EXIT
     END-START.
*
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
     IF ( LINE-CNT  >  1 ) OR ( NEW-KEY NOT = OLD-KEY )
          PERFORM MIDA-SEC
          MOVE    MSL-F04  TO   OLD-MSL-F04
          MOVE    MSL-F05  TO   OLD-MSL-F05
          MOVE    MSL-F06  TO   OLD-MSL-F06
     END-IF.
*
     PERFORM     MEIWRT-SEC.
*
     PERFORM     READMSL-SEC.
*
 MAIN-EXIT.
     EXIT.
***********************************************************
*            マスタ更新履歴ファイルＲＥＡＤ
***********************************************************
 READMSL-SEC               SECTION.
*
 READMSL-01.
     READ   MSTLOGL1
        AT  END
            MOVE   "END"    TO        END-FLG
            GO              TO        READMSL-EXIT
     END-READ.
*
     IF   ( MSL-F01  >  "01"            ) OR
          ( MSL-F02  >  PARA-BUMONCD    ) OR
          ( MSL-F03  >  PARA-TANCD      )
*         ( MSL-F03  >  PARA-TANCD      ) OR
*         ( MSL-F05  >  PARA-UPDTDATE-T )
            MOVE   "END"      TO        END-FLG
            GO                TO        READMSL-EXIT
     END-IF.
*
     IF   ( MSL-F05  <  PARA-UPDTDATE-F ) OR
          ( MSL-F05  >  PARA-UPDTDATE-T )
            GO                TO        READMSL-01
     END-IF.
*
     MOVE   MSL-F04           TO        NEW-MSL-F04.
     MOVE   MSL-F05           TO        NEW-MSL-F05.
     MOVE   MSL-F06           TO        NEW-MSL-F06.
*
     ADD    1                 TO        READ-CNT.
*
 READMSL-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
*
     CLOSE     PRTF  MSTLOGL1  TANMS1.
     DISPLAY "SIT0042L READ =" READ-CNT NC"件" UPON CONS.
     DISPLAY "         PRINT=" WT-CNT   NC"件" UPON CONS.
     DISPLAY "         PAGE =" PAGE-CNT NC"頁" UPON CONS.
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
*MIDASI-1
     MOVE       WK-YS             TO       YY1.
     MOVE       WK-Y              TO       YY2.
     MOVE       YYYY              TO       SYSYY.
     MOVE       WK-M              TO       SYSMM.
     MOVE       WK-D              TO       SYSDD.
     MOVE       WK-TIME(1:2)      TO       TIMEHH.
     MOVE       WK-TIME(3:2)      TO       TIMEMM.
     MOVE       WK-TIME(5:2)      TO       TIMESS.
     ADD        1                 TO       PAGE-CNT.
     MOVE       1                 TO       LINE-CNT.
     MOVE       PAGE-CNT          TO       LPAGE.
*MIDASI-4
     MOVE       PARA-BUMONCD      TO       MIDASI-4-TANCD(1:4).
     MOVE       "-"               TO       MIDASI-4-TANCD(5:1).
     MOVE       PARA-TANCD        TO       MIDASI-4-TANCD(6:2).
     MOVE       TAN-F03           TO       MIDASI-4-TANNM.
*MIDASI-6
     EVALUATE   MSL-F04
       WHEN "1"
                MOVE  NC"登録"    TO       MIDASI-6-MODE
       WHEN "2"
                MOVE  NC"修正"    TO       MIDASI-6-MODE
       WHEN "3"
                MOVE  NC"削除"    TO       MIDASI-6-MODE
     END-EVALUATE.
     MOVE       MSL-F05(1:4)      TO       MIDASI-6-YYYY.
     MOVE       MSL-F05(5:2)      TO       MIDASI-6-MM.
     MOVE       MSL-F05(7:2)      TO       MIDASI-6-DD.
     MOVE       MSL-F06(1:2)      TO       MIDASI-6-TIME(1:2).
     MOVE       ":"               TO       MIDASI-6-TIME(3:1).
     MOVE       MSL-F06(3:2)      TO       MIDASI-6-TIME(4:2).
     MOVE       ":"               TO       MIDASI-6-TIME(6:1).
     MOVE       MSL-F06(5:2)      TO       MIDASI-6-TIME(7:2).
*MIDASI-8
     EVALUATE   MSL-F04
       WHEN "1"
                MOVE  NC"登録　"  TO       MIDASI-8-MODE-L
                MOVE  NC"　　　"  TO       MIDASI-8-MODE-R
       WHEN "2"
                MOVE  NC"修正後"  TO       MIDASI-8-MODE-L
                MOVE  NC"修正前"  TO       MIDASI-8-MODE-R
       WHEN "3"
                MOVE  NC"削除　"  TO       MIDASI-8-MODE-L
                MOVE  NC"　　　"  TO       MIDASI-8-MODE-R
     END-EVALUATE.
*
**************
*帳票書き出し*
**************
     WRITE      P-REC   FROM    MIDASI-0   AFTER  2.
     WRITE      P-REC   FROM    MIDASI-1   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-2   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-3   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-4   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-5   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-6   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-7   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-8   AFTER  1.
*
*    ADD         14        TO        LINE-CNT.
*
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIEDT-SEC                  SECTION.
*
*修正前・修正後レコード準備
     IF      MSL-F04    =          "2"
             MOVE       MSL-F08     TO      TOKL-REC
             MOVE       MSL-F09     TO      TOKR-REC
* 項目セット
             PERFORM    MEIEDT2-SEC
             GO                     TO      MEIEDT-EXIT
     END-IF.
*
*登録・削除レコード準備
     MOVE        MSL-F08    TO        TOKL-REC.
* 項目セット
*   相手取引先コード
     MOVE        SPACE      TO        MEISAI-01-1.
     MOVE        TOKL-F01   TO        MEISAI-01-L.
*   自社得意先コード
     MOVE        SPACE      TO        MEISAI-02-1.
     MOVE        TOKL-F52   TO        MEISAI-02-L.
*   取引先名
     MOVE        SPACE      TO        MEISAI-03-1.
     MOVE        TOKL-F02   TO        MEISAI-03-L.
*   取引先カナ名
     MOVE        SPACE      TO        MEISAI-04-1.
     MOVE        TOKL-F04   TO        MEISAI-04-L.
*   取引先名略称
     MOVE        SPACE      TO        MEISAI-05-1.
     MOVE        TOKL-F03   TO        MEISAI-05-L.
*   郵便番号
     MOVE        SPACE      TO        MEISAI-06-1.
     MOVE        TOKL-F801  TO        MEISAI-06-L(1:3).
     MOVE        "-"        TO        MEISAI-06-L(4:1).
     MOVE        TOKL-F802  TO        MEISAI-06-L(5:4).
*   住所（上段）
     MOVE        SPACE      TO        MEISAI-07-1.
     MOVE        TOKL-F06   TO        MEISAI-07-L.
*   住所（下段）
     MOVE        SPACE      TO        MEISAI-08-1.
     MOVE        TOKL-F07   TO        MEISAI-08-L.
*   電話番号
     MOVE        SPACE      TO        MEISAI-09-1.
     MOVE        TOKL-F08   TO        MEISAI-09-L.
*   ファックス
     MOVE        SPACE      TO        MEISAI-10-1.
     MOVE        TOKL-F09   TO        MEISAI-10-L.
*   ☆請求締日　
     MOVE        SPACE      TO        MEISAI-11-1.
     MOVE        NC"☆"     TO        MEISAI-11-L-STAR.
     MOVE        TOKL-F12(1) TO       MEISAI-11-L(1:2).
     MOVE        "/"         TO       MEISAI-11-L(3:1).
     MOVE        TOKL-F12(2) TO       MEISAI-11-L(4:2).
     MOVE        "/"         TO       MEISAI-11-L(6:1).
     MOVE        TOKL-F12(3) TO       MEISAI-11-L(7:2).
*  伝票発行区分
     MOVE        SPACE      TO        MEISAI-12-1.
     EVALUATE    TOKL-F84
      WHEN  0    MOVE   NC"する"      TO        MEISAI-12-L
      WHEN  9    MOVE   NC"しない"    TO        MEISAI-12-L
      WHEN OTHER MOVE   NC"？？？"    TO        MEISAI-12-L
     END-EVALUATE.
*  付番区分
     MOVE        SPACE      TO        MEISAI-13-1.
     EVALUATE    TOKL-F89
      WHEN  0    MOVE   NC"する"      TO        MEISAI-13-L
      WHEN  9    MOVE   NC"しない"    TO        MEISAI-13-L
      WHEN OTHER MOVE   NC"？？？"    TO        MEISAI-13-L
     END-EVALUATE.
*  消費税端数区分
     MOVE        SPACE      TO        MEISAI-14-1.
     EVALUATE    TOKL-F88
      WHEN  0    MOVE   NC"切り捨て"  TO        MEISAI-14-L
      WHEN  4    MOVE   NC"四捨五入"  TO        MEISAI-14-L
      WHEN  9    MOVE   NC"切り上げ"  TO        MEISAI-14-L
      WHEN OTHER MOVE   NC"？？？？"  TO        MEISAI-14-L
     END-EVALUATE.
*  ☆伝票番号ケタ数
     MOVE        SPACE      TO        MEISAI-15-1.
     MOVE        NC"☆"     TO        MEISAI-15-L-STAR.
     MOVE        TOKL-F92   TO        MEISAI-15-L.
*  ☆行区分
     MOVE        SPACE      TO        MEISAI-16-1.
     MOVE        NC"☆"     TO        MEISAI-16-L-STAR.
     MOVE        TOKL-F82   TO        MEISAI-16-L.
*  ☆チェックデジット区分
     MOVE        SPACE      TO        MEISAI-17-1.
     MOVE        NC"☆"     TO        MEISAI-17-L-STAR.
     EVALUATE    TOKL-F93
      WHEN  0    MOVE   NC"なし"           TO    MEISAI-17-L
      WHEN  1    MOVE   NC"モジュラス７"   TO    MEISAI-17-L
      WHEN  2    MOVE   NC"モジュラス１０" TO    MEISAI-17-L
      WHEN  3    MOVE   NC"モジュラス１１" TO    MEISAI-17-L
      WHEN OTHER MOVE   NC"？？？？？？？" TO    MEISAI-17-L
     END-EVALUATE.
*  ☆チェックデジット区分２
     MOVE        SPACE      TO        MEISAI-18-1.
     MOVE        NC"☆"     TO        MEISAI-18-L-STAR.
     EVALUATE    TOKL-F94
      WHEN  0    MOVE   NC"なし"           TO    MEISAI-18-L
      WHEN  1    MOVE   NC"モジュラス７"   TO    MEISAI-18-L
      WHEN  2    MOVE   NC"モジュラス１０" TO    MEISAI-18-L
      WHEN  3    MOVE   NC"モジュラス１１" TO    MEISAI-18-L
      WHEN OTHER MOVE   NC"？？？？？？？" TO    MEISAI-18-L
     END-EVALUATE.
*  ☆税計算区分
     MOVE        SPACE      TO        MEISAI-19-1.
     MOVE        NC"☆"     TO        MEISAI-19-L-STAR.
     EVALUATE    TOKL-F97
      WHEN "0"   MOVE   NC"する"      TO        MEISAI-19-L
      WHEN "9"   MOVE   NC"しない"    TO        MEISAI-19-L
      WHEN OTHER MOVE   NC"？？？"    TO        MEISAI-19-L
     END-EVALUATE.
*  ☆売価チェック区分
     MOVE        SPACE      TO        MEISAI-20-1.
     MOVE        NC"☆"     TO        MEISAI-20-L-STAR.
     EVALUATE    TOKL-F96
      WHEN  0    MOVE   NC"する"      TO        MEISAI-20-L
      WHEN  9    MOVE   NC"しない"    TO        MEISAI-20-L
      WHEN OTHER MOVE   NC"？？？"    TO        MEISAI-20-L
     END-EVALUATE.
*  ☆管理区分
     MOVE        SPACE      TO        MEISAI-21-1.
     MOVE        NC"☆"     TO        MEISAI-21-L-STAR.
     EVALUATE    TOKL-F78
      WHEN "1"   MOVE   NC"する"      TO        MEISAI-21-L
      WHEN " "   MOVE   NC"しない"    TO        MEISAI-21-L
      WHEN OTHER MOVE   NC"？？？"    TO        MEISAI-21-L
     END-EVALUATE.
*  出荷送信（オンライン）
     MOVE        SPACE      TO         MEISAI-22-1.
     EVALUATE    TOKL-FIL1(3:1)
      WHEN " "   MOVE   NC"無"         TO       MEISAI-22-L
      WHEN "1"   MOVE   NC"有"         TO       MEISAI-22-L
      WHEN OTHER MOVE   NC"？"         TO       MEISAI-22-L
     END-EVALUATE.
*  出荷送信（手書）
     MOVE        SPACE      TO         MEISAI-23-1.
     EVALUATE    TOKL-FIL1(4:1)
      WHEN " "   MOVE   NC"無"         TO       MEISAI-23-L
      WHEN "1"   MOVE   NC"有"         TO       MEISAI-23-L
      WHEN OTHER MOVE   NC"？"         TO       MEISAI-23-L
     END-EVALUATE.
*  売上用　伝票ＮＯ
     MOVE        SPACE      TO         MEISAI-24-1.
     MOVE        TOKL-F54   TO         MEISAI-24-L.
*  売上用　範囲伝票ＮＯ
     MOVE        SPACE      TO         MEISAI-25-1.
     MOVE        TOKL-F57   TO         MEISAI-25-L-1.
     MOVE        NC"～"     TO         MEISAI-25-L-2.
     MOVE        TOKL-F58   TO         MEISAI-25-L-3.
*  付番用　伝票ＮＯ
     MOVE        SPACE      TO         MEISAI-26-1.
     MOVE        TOKL-F59   TO         MEISAI-26-L.
*  付番用　範囲伝票ＮＯ
     MOVE        SPACE      TO         MEISAI-27-1.
     MOVE        TOKL-F60   TO         MEISAI-27-L-1.
     MOVE        NC"～"     TO         MEISAI-27-L-2.
     MOVE        TOKL-F61   TO         MEISAI-27-L-3.
*  ☆代表倉庫コード　
     MOVE        SPACE      TO         MEISAI-28-1.
     MOVE        NC"☆"     TO         MEISAI-28-L-STAR.
     MOVE        TOKL-F81   TO         MEISAI-28-L.
*  ☆出荷場所決定区分
     MOVE        SPACE      TO         MEISAI-29-1.
     MOVE        NC"☆"     TO         MEISAI-29-L-STAR.
     EVALUATE    TOKL-F95
      WHEN  1    MOVE   NC"ルートマスタ" TO     MEISAI-29-L
      WHEN  2    MOVE   NC"変換ＴＢＬ"   TO     MEISAI-29-L
      WHEN  3    MOVE   NC"代表倉庫"     TO     MEISAI-29-L
      WHEN OTHER MOVE   NC"？？？？？？" TO     MEISAI-29-L
     END-EVALUATE.
*  ☆指定伝票タイプ
     MOVE        SPACE      TO         MEISAI-30-1.
     MOVE        NC"☆"     TO         MEISAI-30-L-STAR.
     EVALUATE    TOKL-F79
      WHEN  0    MOVE   NC"指定なし"     TO     MEISAI-30-L
      WHEN  1    MOVE   NC"ＴＡ１型"     TO     MEISAI-30-L
      WHEN  2    MOVE   NC"ＴＡ２型"     TO     MEISAI-30-L
      WHEN  3    MOVE   NC"タイプ用"     TO     MEISAI-30-L
      WHEN  4    MOVE   NC"タイプ用１型" TO     MEISAI-30-L
      WHEN OTHER MOVE   NC"？？？？？？" TO     MEISAI-30-L
     END-EVALUATE.
*  シーズン開始日（春）
     MOVE        SPACE      TO         MEISAI-31-1.
     MOVE        TOKL-F70(1:2)  TO     MEISAI-31-L(1:2).
     MOVE        "/"            TO     MEISAI-31-L(3:1).
     MOVE        TOKL-F70(3:2)  TO     MEISAI-31-L(4:2).
*  シーズン開始日（秋）
     MOVE        SPACE      TO         MEISAI-32-1.
     MOVE        TOKL-F71(1:2)  TO     MEISAI-32-L(1:2).
     MOVE        "/"            TO     MEISAI-32-L(3:1).
     MOVE        TOKL-F71(3:2)  TO     MEISAI-32-L(4:2).
*  ☆発注集計表出力区分
     MOVE        SPACE      TO         MEISAI-33-1.
     MOVE        NC"☆"     TO         MEISAI-33-L-STAR.
     EVALUATE    TOKL-FIL1(2:1)
      WHEN " "   MOVE   NC"税抜"         TO     MEISAI-33-L
      WHEN "1"   MOVE   NC"税込"         TO     MEISAI-33-L
      WHEN OTHER MOVE   NC"？？"         TO     MEISAI-33-L
     END-EVALUATE.
*  ☆税扱区分
     MOVE        SPACE      TO         MEISAI-34-1.
     MOVE        NC"☆"     TO         MEISAI-34-L-STAR.
     EVALUATE    TOKL-F18
      WHEN  1    MOVE   NC"内税"         TO     MEISAI-34-L
      WHEN  2    MOVE   NC"外税"         TO     MEISAI-34-L
      WHEN OTHER MOVE   NC"？？"         TO     MEISAI-34-L
     END-EVALUATE.
*  ☆伝票纏め区分
     MOVE        SPACE      TO         MEISAI-35-1.
     MOVE        NC"☆"     TO         MEISAI-35-L-STAR.
     EVALUATE    TOKL-FIL1(1:1)
      WHEN " "   MOVE   NC"纏め対象外"   TO     MEISAI-35-L
      WHEN "1"   MOVE   NC"纏め対象"     TO     MEISAI-35-L
      WHEN OTHER MOVE   NC"？？？？？"   TO     MEISAI-35-L
     END-EVALUATE.
*  ☆売上計上時税抜
     MOVE        SPACE      TO         MEISAI-36-1.
     MOVE        NC"☆"     TO         MEISAI-36-L-STAR.
     EVALUATE    TOKL-F20
      WHEN  0    MOVE   NC"しない"       TO     MEISAI-36-L
      WHEN  1    MOVE   NC"する"         TO     MEISAI-36-L
      WHEN OTHER MOVE   NC"？？"         TO     MEISAI-36-L
     END-EVALUATE.
*  ☆改訂後税抜
     MOVE        SPACE      TO         MEISAI-37-1.
     MOVE        NC"☆"     TO         MEISAI-37-L-STAR.
     EVALUATE    TOKL-F22
      WHEN  0    MOVE   NC"しない"       TO     MEISAI-37-L
      WHEN  1    MOVE   NC"する"         TO     MEISAI-37-L
      WHEN OTHER MOVE   NC"？？"         TO     MEISAI-37-L
     END-EVALUATE.
*  ☆改訂日
     MOVE        SPACE      TO         MEISAI-38-1.
     MOVE        NC"☆"     TO         MEISAI-38-L-STAR.
     MOVE        TOKL-F21   TO         WK-TOKL-F21.
     IF          TOKL-F21   NOT =  ZERO
                 MOVE    WK-TOKL-F21(4:4)  TO  MEISAI-38-L(1:4)
                 MOVE    "/"               TO  MEISAI-38-L(5:1)
                 MOVE    WK-TOKL-F21(8:2)  TO  MEISAI-38-L(6:2)
                 MOVE    "/"               TO  MEISAI-38-L(8:1)
                 MOVE    WK-TOKL-F21(10:2) TO  MEISAI-38-L(9:2)
     END-IF.
*  ☆計上部門コード
     MOVE        SPACE             TO     MEISAI-39-1.
     MOVE        NC"☆"            TO     MEISAI-39-L-STAR.
     MOVE        TOKL-F75          TO     MEISAI-39-L.
*  ☆計上部門変更日
     MOVE        SPACE             TO     MEISAI-40-1.
     MOVE        NC"☆"            TO     MEISAI-40-L-STAR.
     MOVE        TOKL-F76          TO     WK-TOKL-F76.
     MOVE        WK-TOKL-F76(1:4)  TO     MEISAI-40-L(1:4).
     MOVE        "/"               TO     MEISAI-40-L(5:1).
     MOVE        WK-TOKL-F76(5:2)  TO     MEISAI-40-L(6:2).
     MOVE        "/"               TO     MEISAI-40-L(8:1).
     MOVE        WK-TOKL-F76(7:2)  TO     MEISAI-40-L(9:2).
*  ☆変更部門コード
     MOVE        SPACE             TO     MEISAI-41-1.
     MOVE        NC"☆"            TO     MEISAI-41-L-STAR.
     MOVE        TOKL-F77          TO     MEISAI-41-L.
*#2019/09/27 NAV ST
*  ☆請求区分
     MOVE        SPACE      TO         MEISAI-42-1.
     MOVE        NC"☆"     TO         MEISAI-42-L-STAR.
     EVALUATE    TOKL-F90
      WHEN " "   MOVE   NC"請求しない"   TO     MEISAI-42-L
      WHEN "1"   MOVE   NC"請求する"     TO     MEISAI-42-L
      WHEN OTHER MOVE   NC"？？？？？"   TO     MEISAI-42-L
     END-EVALUATE.
*#2019/09/27 NAV ED
*
 MEIEDT-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集（修正モード）           *
**********************************************************
 MEIEDT2-SEC                  SECTION.
*
* 項目セット
*   相手取引先コード
     MOVE        SPACE      TO        MEISAI-01-1.
     MOVE        TOKR-F01   TO        MEISAI-01-R.
     IF          TOKR-F01   NOT =     TOKL-F01
                 MOVE       TOKL-F01  TO        MEISAI-01-L
     END-IF.
*   自社得意先コード
     MOVE        SPACE      TO        MEISAI-02-1.
     MOVE        TOKR-F52   TO        MEISAI-02-R.
     IF          TOKR-F52   NOT =     TOKL-F52
                 MOVE       TOKL-F52  TO        MEISAI-02-L
     END-IF.
*   取引先名
     MOVE        SPACE      TO        MEISAI-03-1.
     MOVE        TOKR-F02   TO        MEISAI-03-R.
     IF          TOKR-F02   NOT =     TOKL-F02
                 MOVE       TOKL-F02  TO        MEISAI-03-L
     END-IF.
*   取引先カナ名
     MOVE        SPACE      TO        MEISAI-04-1.
     MOVE        TOKR-F04   TO        MEISAI-04-R.
     IF          TOKR-F04   NOT =     TOKL-F04
                 MOVE       TOKL-F04  TO        MEISAI-04-L
     END-IF.
*   取引先名略称
     MOVE        SPACE      TO        MEISAI-05-1.
     MOVE        TOKR-F03   TO        MEISAI-05-R.
     IF          TOKR-F03   NOT =     TOKL-F03
                 MOVE       TOKL-F03  TO        MEISAI-05-L
     END-IF.
*   郵便番号
     MOVE        SPACE      TO        MEISAI-06-1.
     MOVE        TOKR-F801  TO        MEISAI-06-R(1:3).
     MOVE        "-"        TO        MEISAI-06-R(4:1).
     MOVE        TOKR-F802  TO        MEISAI-06-R(5:4).
     IF        ( TOKR-F801  NOT =     TOKL-F801 ) OR
               ( TOKR-F802  NOT =     TOKL-F802 )
                 MOVE       TOKL-F801 TO        MEISAI-06-L(1:3)
                 MOVE       "-"       TO        MEISAI-06-L(4:1)
                 MOVE       TOKL-F802 TO        MEISAI-06-L(5:4)
     END-IF.
*   住所（上段）
     MOVE        SPACE      TO        MEISAI-07-1.
     MOVE        TOKR-F06   TO        MEISAI-07-R.
     IF          TOKR-F06   NOT =     TOKL-F06
                 MOVE       TOKL-F06  TO        MEISAI-07-L
     END-IF.
*   住所（下段）
     MOVE        SPACE      TO        MEISAI-08-1.
     MOVE        TOKR-F07   TO        MEISAI-08-R.
     IF          TOKR-F07   NOT =     TOKL-F07
                 MOVE       TOKL-F07  TO        MEISAI-08-L
     END-IF.
*   電話番号
     MOVE        SPACE      TO        MEISAI-09-1.
     MOVE        TOKR-F08   TO        MEISAI-09-R.
     IF          TOKR-F08   NOT =     TOKL-F08
                 MOVE       TOKL-F08  TO        MEISAI-09-L
     END-IF.
*   ファックス
     MOVE        SPACE      TO        MEISAI-10-1.
     MOVE        TOKR-F09   TO        MEISAI-10-R.
     IF          TOKR-F09   NOT =     TOKL-F09
                 MOVE       TOKL-F09  TO        MEISAI-10-L
     END-IF.
*   ☆請求締日　
     MOVE        SPACE         TO        MEISAI-11-1.
     MOVE        NC"☆"        TO        MEISAI-11-R-STAR.
     MOVE        TOKR-F12(1)   TO        MEISAI-11-R(1:2).
     MOVE        "/"           TO        MEISAI-11-R(3:1).
     MOVE        TOKR-F12(2)   TO        MEISAI-11-R(4:2).
     MOVE        "/"           TO        MEISAI-11-R(6:1).
     MOVE        TOKR-F12(3)   TO        MEISAI-11-R(7:2).
     IF        ( TOKR-F12(1)   NOT =     TOKL-F12(1)  )  OR
               ( TOKR-F12(2)   NOT =     TOKL-F12(2)  )  OR
               ( TOKR-F12(3)   NOT =     TOKL-F12(3)  )
                 MOVE  NC"☆"            TO   MEISAI-11-L-STAR
                 MOVE  TOKL-F12(1)       TO   MEISAI-11-L(1:2)
                 MOVE  "-"               TO   MEISAI-11-L(3:1)
                 MOVE  TOKL-F12(2)       TO   MEISAI-11-L(4:2)
                 MOVE  "-"               TO   MEISAI-11-L(6:1)
                 MOVE  TOKL-F12(3)       TO   MEISAI-11-L(7:2)
     END-IF.
*  伝票発行区分
     MOVE        SPACE      TO        MEISAI-12-1.
     EVALUATE    TOKR-F84
      WHEN  0    MOVE   NC"する"      TO        MEISAI-12-R
      WHEN  9    MOVE   NC"しない"    TO        MEISAI-12-R
      WHEN OTHER MOVE   NC"？？？"    TO        MEISAI-12-R
     END-EVALUATE.
     IF  TOKR-F84    NOT =     TOKL-F84
         EVALUATE    TOKL-F84
          WHEN  0    MOVE   NC"する"      TO    MEISAI-12-L
          WHEN  9    MOVE   NC"しない"    TO    MEISAI-12-L
          WHEN OTHER MOVE   NC"？？？"    TO    MEISAI-12-L
         END-EVALUATE
     END-IF.
*  付番区分
     MOVE        SPACE      TO        MEISAI-13-1.
     EVALUATE    TOKR-F89
      WHEN  0    MOVE   NC"する"      TO        MEISAI-13-R
      WHEN  9    MOVE   NC"しない"    TO        MEISAI-13-R
      WHEN OTHER MOVE   NC"？？？"    TO        MEISAI-13-R
     END-EVALUATE.
     IF  TOKR-F89    NOT =     TOKL-F89
         EVALUATE    TOKL-F89
          WHEN  0    MOVE   NC"する"      TO        MEISAI-13-L
          WHEN  9    MOVE   NC"しない"    TO        MEISAI-13-L
          WHEN OTHER MOVE   NC"？？？"    TO        MEISAI-13-L
         END-EVALUATE
     END-IF.
*  消費税端数区分
     MOVE        SPACE      TO        MEISAI-14-1.
     EVALUATE    TOKR-F88
      WHEN  0    MOVE   NC"切り捨て"  TO        MEISAI-14-R
      WHEN  4    MOVE   NC"四捨五入"  TO        MEISAI-14-R
      WHEN  9    MOVE   NC"切り上げ"  TO        MEISAI-14-R
      WHEN OTHER MOVE   NC"？？？？"  TO        MEISAI-14-R
     END-EVALUATE.
     IF  TOKR-F88    NOT =     TOKL-F88
         EVALUATE    TOKL-F88
          WHEN  0    MOVE   NC"切り捨て"  TO        MEISAI-14-L
          WHEN  4    MOVE   NC"四捨五入"  TO        MEISAI-14-L
          WHEN  9    MOVE   NC"切り上げ"  TO        MEISAI-14-L
          WHEN OTHER MOVE   NC"？？？？"  TO        MEISAI-14-L
         END-EVALUATE
     END-IF.
*  ☆伝票番号ケタ数
     MOVE        SPACE      TO        MEISAI-15-1.
     MOVE        NC"☆"     TO        MEISAI-15-R-STAR.
     MOVE        TOKR-F92   TO        MEISAI-15-R.
     IF          TOKR-F92   NOT =     TOKL-F92
                 MOVE       NC"☆"    TO     MEISAI-15-L-STAR
                 MOVE       TOKL-F92  TO     MEISAI-15-L
     END-IF.
*  ☆行区分
     MOVE        SPACE      TO        MEISAI-16-1.
     MOVE        NC"☆"     TO        MEISAI-16-R-STAR.
     MOVE        TOKR-F82   TO        MEISAI-16-R.
     IF          TOKR-F82   NOT =     TOKL-F82
                 MOVE       NC"☆"    TO     MEISAI-16-L-STAR
                 MOVE       TOKL-F82  TO     MEISAI-16-L
     END-IF.
*  ☆チェックデジット区分
     MOVE        SPACE      TO        MEISAI-17-1.
     MOVE        NC"☆"     TO        MEISAI-17-R-STAR.
     EVALUATE    TOKR-F93
      WHEN  0    MOVE   NC"なし"           TO    MEISAI-17-R
      WHEN  1    MOVE   NC"モジュラス７"   TO    MEISAI-17-R
      WHEN  2    MOVE   NC"モジュラス１０" TO    MEISAI-17-R
      WHEN  3    MOVE   NC"モジュラス１１" TO    MEISAI-17-R
      WHEN OTHER MOVE   NC"？？？？？？？" TO    MEISAI-17-R
     END-EVALUATE.
     IF  TOKR-F93    NOT =     TOKL-F93
         MOVE        NC"☆"           TO       MEISAI-17-L-STAR
         EVALUATE    TOKL-F93
          WHEN  0    MOVE   NC"なし"           TO MEISAI-17-L
          WHEN  1    MOVE   NC"モジュラス７"   TO MEISAI-17-L
          WHEN  2    MOVE   NC"モジュラス１０" TO MEISAI-17-L
          WHEN  3    MOVE   NC"モジュラス１１" TO MEISAI-17-L
          WHEN OTHER MOVE   NC"？？？？？？？" TO MEISAI-17-L
         END-EVALUATE
     END-IF.
*  ☆チェックデジット区分２
     MOVE        SPACE      TO        MEISAI-18-1.
     MOVE        NC"☆"     TO        MEISAI-18-R-STAR.
     EVALUATE    TOKR-F94
      WHEN  0    MOVE   NC"なし"           TO    MEISAI-18-R
      WHEN  1    MOVE   NC"モジュラス７"   TO    MEISAI-18-R
      WHEN  2    MOVE   NC"モジュラス１０" TO    MEISAI-18-R
      WHEN  3    MOVE   NC"モジュラス１１" TO    MEISAI-18-R
      WHEN OTHER MOVE   NC"？？？？？？？" TO    MEISAI-18-R
     END-EVALUATE.
     IF  TOKR-F94    NOT =     TOKL-F94
         MOVE        NC"☆"           TO       MEISAI-18-L-STAR
         EVALUATE    TOKL-F94
          WHEN  0    MOVE   NC"なし"           TO MEISAI-18-L
          WHEN  1    MOVE   NC"モジュラス７"   TO MEISAI-18-L
          WHEN  2    MOVE   NC"モジュラス１０" TO MEISAI-18-L
          WHEN  3    MOVE   NC"モジュラス１１" TO MEISAI-18-L
          WHEN OTHER MOVE   NC"？？？？？？？" TO MEISAI-18-L
         END-EVALUATE
     END-IF.
*  ☆税計算区分
     MOVE        SPACE      TO        MEISAI-19-1.
     MOVE        NC"☆"     TO        MEISAI-19-R-STAR.
     EVALUATE    TOKR-F97
      WHEN "0"   MOVE   NC"する"      TO        MEISAI-19-R
      WHEN "9"   MOVE   NC"しない"    TO        MEISAI-19-R
      WHEN OTHER MOVE   NC"？？？"    TO        MEISAI-19-R
     END-EVALUATE.
     IF  TOKR-F97    NOT =     TOKL-F97
         MOVE        NC"☆"           TO       MEISAI-19-L-STAR
         EVALUATE    TOKL-F97
          WHEN "0"   MOVE   NC"する"           TO MEISAI-19-L
          WHEN "9"   MOVE   NC"しない"         TO MEISAI-19-L
          WHEN OTHER MOVE   NC"？？？"         TO MEISAI-19-L
         END-EVALUATE
     END-IF.
*  ☆売価チェック区分
     MOVE        SPACE      TO        MEISAI-20-1.
     MOVE        NC"☆"     TO        MEISAI-20-R-STAR.
     EVALUATE    TOKR-F96
      WHEN  0    MOVE   NC"する"      TO        MEISAI-20-R
      WHEN  9    MOVE   NC"しない"    TO        MEISAI-20-R
      WHEN OTHER MOVE   NC"？？？"    TO        MEISAI-20-R
     END-EVALUATE.
     IF  TOKR-F96    NOT =     TOKL-F96
         MOVE        NC"☆"           TO       MEISAI-20-L-STAR
         EVALUATE    TOKL-F96
          WHEN  0    MOVE   NC"する"           TO MEISAI-20-L
          WHEN  9    MOVE   NC"しない"         TO MEISAI-20-L
          WHEN OTHER MOVE   NC"？？？"         TO MEISAI-20-L
         END-EVALUATE
     END-IF.
*  ☆管理区分
     MOVE        SPACE      TO        MEISAI-21-1.
     MOVE        NC"☆"     TO        MEISAI-21-R-STAR.
     EVALUATE    TOKR-F78
      WHEN "1"   MOVE   NC"する"      TO        MEISAI-21-R
      WHEN " "   MOVE   NC"しない"    TO        MEISAI-21-R
      WHEN OTHER MOVE   NC"？？？"    TO        MEISAI-21-R
     END-EVALUATE.
     IF  TOKR-F78    NOT =     TOKL-F78
         MOVE        NC"☆"           TO       MEISAI-21-L-STAR
         EVALUATE    TOKL-F96
          WHEN "1"   MOVE   NC"する"           TO MEISAI-21-L
          WHEN " "   MOVE   NC"しない"         TO MEISAI-21-L
          WHEN OTHER MOVE   NC"？？？"         TO MEISAI-21-L
         END-EVALUATE
     END-IF.
*  出荷送信（オンライン）
     MOVE        SPACE      TO         MEISAI-22-1.
     EVALUATE    TOKR-FIL1(3:1)
      WHEN " "   MOVE   NC"無"         TO      MEISAI-22-R
      WHEN "1"   MOVE   NC"有"         TO      MEISAI-22-R
      WHEN OTHER MOVE   NC"？"         TO      MEISAI-22-R
     END-EVALUATE.
     IF  TOKR-FIL1(3:1) NOT =  TOKL-FIL1(3:1)
         EVALUATE    TOKL-FIL1(3:1)
          WHEN " "   MOVE   NC"無"             TO MEISAI-22-L
          WHEN "1"   MOVE   NC"有"             TO MEISAI-22-L
          WHEN OTHER MOVE   NC"？"             TO MEISAI-22-L
         END-EVALUATE
     END-IF.
*  出荷送信（手書）
     MOVE        SPACE      TO         MEISAI-23-1.
     EVALUATE    TOKR-FIL1(4:1)
      WHEN " "   MOVE   NC"無"         TO       MEISAI-23-R
      WHEN "1"   MOVE   NC"有"         TO       MEISAI-23-R
      WHEN OTHER MOVE   NC"？"         TO       MEISAI-23-R
     END-EVALUATE.
     IF  TOKR-FIL1(4:1) NOT =  TOKL-FIL1(4:1)
         EVALUATE    TOKL-FIL1(4:1)
          WHEN " "   MOVE   NC"無"             TO MEISAI-23-L
          WHEN "1"   MOVE   NC"有"             TO MEISAI-23-L
          WHEN OTHER MOVE   NC"？"             TO MEISAI-23-L
         END-EVALUATE
     END-IF.
*  売上用　伝票ＮＯ
     MOVE        SPACE      TO         MEISAI-24-1.
     MOVE        TOKR-F54   TO         MEISAI-24-R.
     IF          TOKR-F54   NOT =      TOKL-F54
                 MOVE       TOKL-F54   TO         MEISAI-24-L
     END-IF.
*  売上用　範囲伝票ＮＯ
     MOVE        SPACE      TO         MEISAI-25-1.
     MOVE        TOKR-F57   TO         MEISAI-25-R-1.
     MOVE        NC"～"     TO         MEISAI-25-R-2.
     MOVE        TOKR-F58   TO         MEISAI-25-R-3.
     IF       (  TOKR-F57   NOT =      TOKL-F57  )  OR
              (  TOKR-F58   NOT =      TOKL-F58  )
                 MOVE       TOKL-F57   TO         MEISAI-25-L-1
                 MOVE       NC"～"     TO         MEISAI-25-L-2
                 MOVE       TOKL-F58   TO         MEISAI-25-L-3
     END-IF.
*  付番用　伝票ＮＯ
     MOVE        SPACE      TO         MEISAI-26-1.
     MOVE        TOKR-F59   TO         MEISAI-26-R.
     IF          TOKR-F59   NOT =      TOKL-F59
                 MOVE       TOKL-F59   TO         MEISAI-26-L
     END-IF.
*  付番用　範囲伝票ＮＯ
     MOVE        SPACE      TO         MEISAI-27-1.
     MOVE        TOKR-F60   TO         MEISAI-27-R-1.
     MOVE        NC"～"     TO         MEISAI-27-R-2.
     MOVE        TOKR-F61   TO         MEISAI-27-R-3.
     IF       (  TOKR-F60   NOT =      TOKL-F60  )  OR
              (  TOKR-F61   NOT =      TOKL-F61  )
                 MOVE       TOKL-F60   TO         MEISAI-27-L-1
                 MOVE       NC"～"     TO         MEISAI-27-L-2
                 MOVE       TOKL-F61   TO         MEISAI-27-L-3
     END-IF.
*  ☆代表倉庫コード　
     MOVE        SPACE      TO         MEISAI-28-1.
     MOVE        NC"☆"     TO         MEISAI-28-R-STAR.
     MOVE        TOKR-F81   TO         MEISAI-28-R.
     IF          TOKR-F81   NOT =      TOKL-F81
                 MOVE       NC"☆"     TO      MEISAI-28-L-STAR
                 MOVE       TOKL-F81   TO      MEISAI-28-L
     END-IF.
*  ☆出荷場所決定区分
     MOVE        SPACE      TO         MEISAI-29-1.
     MOVE        NC"☆"     TO         MEISAI-29-R-STAR.
     EVALUATE    TOKR-F95
      WHEN  1    MOVE   NC"ルートマスタ" TO     MEISAI-29-R
      WHEN  2    MOVE   NC"変換ＴＢＬ"   TO     MEISAI-29-R
      WHEN  3    MOVE   NC"代表倉庫"     TO     MEISAI-29-R
      WHEN OTHER MOVE   NC"？？？？？？" TO     MEISAI-29-R
     END-EVALUATE.
     IF  TOKR-F95   NOT =      TOKL-F95
         MOVE       NC"☆"     TO         MEISAI-29-L-STAR
         EVALUATE    TOKL-F95
          WHEN  1    MOVE   NC"ルートマスタ" TO     MEISAI-29-L
          WHEN  2    MOVE   NC"変換ＴＢＬ"   TO     MEISAI-29-L
          WHEN  3    MOVE   NC"代表倉庫"     TO     MEISAI-29-L
          WHEN OTHER MOVE   NC"？？？？？？" TO     MEISAI-29-L
         END-EVALUATE
     END-IF.
*  ☆指定伝票タイプ
     MOVE        SPACE      TO         MEISAI-30-1.
     MOVE        NC"☆"     TO         MEISAI-30-R-STAR.
     EVALUATE    TOKR-F79
      WHEN  0    MOVE   NC"指定なし"     TO     MEISAI-30-R
      WHEN  1    MOVE   NC"ＴＡ１型"     TO     MEISAI-30-R
      WHEN  2    MOVE   NC"ＴＡ２型"     TO     MEISAI-30-R
      WHEN  3    MOVE   NC"タイプ用"     TO     MEISAI-30-R
      WHEN  4    MOVE   NC"タイプ用１型" TO     MEISAI-30-R
      WHEN OTHER MOVE   NC"？？？？？？" TO     MEISAI-30-R
     END-EVALUATE.
     IF  TOKR-F79   NOT =      TOKL-F79
         MOVE        NC"☆"    TO        MEISAI-30-L-STAR
         EVALUATE    TOKL-F79
          WHEN  0    MOVE   NC"指定なし"     TO     MEISAI-30-L
          WHEN  1    MOVE   NC"ＴＡ１型"     TO     MEISAI-30-L
          WHEN  2    MOVE   NC"ＴＡ２型"     TO     MEISAI-30-L
          WHEN  3    MOVE   NC"タイプ用"     TO     MEISAI-30-L
          WHEN  4    MOVE   NC"タイプ用１型" TO     MEISAI-30-L
          WHEN OTHER MOVE   NC"？？？？？？" TO     MEISAI-30-L
         END-EVALUATE
     END-IF.
*  シーズン開始日（春）
     MOVE        SPACE      TO         MEISAI-31-1.
     MOVE        TOKR-F70(1:2)  TO     MEISAI-31-R(1:2).
     MOVE        "/"            TO     MEISAI-31-R(3:1).
     MOVE        TOKR-F70(3:2)  TO     MEISAI-31-R(4:2).
     IF          TOKR-F70    NOT =     TOKL-F70
                 MOVE   TOKL-F70(1:2)  TO     MEISAI-31-L(1:2)
                 MOVE   "/"            TO     MEISAI-31-L(3:1)
                 MOVE   TOKL-F70(3:2)  TO     MEISAI-31-L(4:2)
     END-IF.
*  シーズン開始日（秋）
     MOVE        SPACE      TO         MEISAI-32-1.
     MOVE        TOKR-F71(1:2)  TO     MEISAI-32-R(1:2).
     MOVE        "/"            TO     MEISAI-32-R(3:1).
     MOVE        TOKR-F71(3:2)  TO     MEISAI-32-R(4:2).
     IF          TOKR-F71    NOT =     TOKL-F71
                 MOVE   TOKL-F71(1:2)  TO     MEISAI-32-L(1:2)
                 MOVE   "/"            TO     MEISAI-32-L(3:1)
                 MOVE   TOKL-F71(3:2)  TO     MEISAI-32-L(4:2)
     END-IF.
*  ☆発注集計表出力区分
     MOVE        SPACE      TO         MEISAI-33-1.
     MOVE        NC"☆"     TO         MEISAI-33-R-STAR.
     EVALUATE    TOKR-FIL1(2:1)
      WHEN " "   MOVE   NC"税抜"         TO     MEISAI-33-R
      WHEN "1"   MOVE   NC"税込"         TO     MEISAI-33-R
      WHEN OTHER MOVE   NC"？？"         TO     MEISAI-33-R
     END-EVALUATE.
     IF  TOKR-FIL1(2:1)  NOT =   TOKL-FIL1(2:1)
         MOVE   NC"☆"   TO      MEISAI-33-L-STAR
         EVALUATE    TOKL-FIL1(2:1)
          WHEN " "   MOVE   NC"税抜"         TO     MEISAI-33-L
          WHEN "1"   MOVE   NC"税込"         TO     MEISAI-33-L
          WHEN OTHER MOVE   NC"？？"         TO     MEISAI-33-L
         END-EVALUATE
     END-IF.
*  ☆税扱区分
     MOVE        SPACE      TO         MEISAI-34-1.
     MOVE        NC"☆"     TO         MEISAI-34-R-STAR.
     EVALUATE    TOKR-F18
      WHEN  1    MOVE   NC"内税"         TO     MEISAI-34-R
      WHEN  2    MOVE   NC"外税"         TO     MEISAI-34-R
      WHEN OTHER MOVE   NC"？？"         TO     MEISAI-34-R
     END-EVALUATE.
     IF  TOKR-F18    NOT =   TOKL-F18
         MOVE        NC"☆"     TO       MEISAI-34-L-STAR
         EVALUATE    TOKL-F18
          WHEN  1    MOVE   NC"内税"         TO     MEISAI-34-L
          WHEN  2    MOVE   NC"外税"         TO     MEISAI-34-L
          WHEN OTHER MOVE   NC"？？"         TO     MEISAI-34-L
         END-EVALUATE
     END-IF.
*  ☆伝票纏め区分
     MOVE        SPACE      TO         MEISAI-35-1.
     MOVE        NC"☆"     TO         MEISAI-35-R-STAR.
     EVALUATE    TOKR-FIL1(1:1)
      WHEN " "   MOVE   NC"纏め対象外"   TO     MEISAI-35-R
      WHEN "1"   MOVE   NC"纏め対象"     TO     MEISAI-35-R
      WHEN OTHER MOVE   NC"？？？？？"   TO     MEISAI-35-R
     END-EVALUATE.
     IF  TOKR-FIL1(1:1)     NOT =   TOKL-FIL1(1:1)
         MOVE        NC"☆"     TO       MEISAI-35-L-STAR
         EVALUATE    TOKL-FIL1(1:1)
          WHEN " "   MOVE   NC"纏め対象外"   TO     MEISAI-35-L
          WHEN "1"   MOVE   NC"纏め対象"     TO     MEISAI-35-L
          WHEN OTHER MOVE   NC"？？？？？"   TO     MEISAI-35-L
         END-EVALUATE
     END-IF.
*  ☆売上計上時税抜
     MOVE        SPACE      TO         MEISAI-36-1.
     MOVE        NC"☆"     TO         MEISAI-36-R-STAR.
     EVALUATE    TOKR-F20
      WHEN  0    MOVE   NC"しない"       TO     MEISAI-36-R
      WHEN  1    MOVE   NC"する"         TO     MEISAI-36-R
      WHEN OTHER MOVE   NC"？？"         TO     MEISAI-36-R
     END-EVALUATE.
     IF  TOKR-F20    NOT =   TOKL-F20
         MOVE        NC"☆"     TO       MEISAI-36-L-STAR
         EVALUATE    TOKL-F20
          WHEN  0    MOVE   NC"しない"       TO     MEISAI-36-L
          WHEN  1    MOVE   NC"する"         TO     MEISAI-36-L
          WHEN OTHER MOVE   NC"？？"         TO     MEISAI-36-L
         END-EVALUATE
     END-IF.
*  ☆改訂後税抜
     MOVE        SPACE      TO         MEISAI-37-1.
     MOVE        NC"☆"     TO         MEISAI-37-R-STAR.
     EVALUATE    TOKR-F22
      WHEN  0    MOVE   NC"しない"       TO     MEISAI-37-R
      WHEN  1    MOVE   NC"する"         TO     MEISAI-37-R
      WHEN OTHER MOVE   NC"？？"         TO     MEISAI-37-R
     END-EVALUATE.
     IF  TOKR-F22    NOT =   TOKL-F22
         MOVE        NC"☆"     TO       MEISAI-37-L-STAR
         EVALUATE    TOKL-F22
          WHEN  0    MOVE   NC"しない"       TO     MEISAI-37-L
          WHEN  1    MOVE   NC"する"         TO     MEISAI-37-L
          WHEN OTHER MOVE   NC"？？"         TO     MEISAI-37-L
         END-EVALUATE
     END-IF.
*  ☆改訂日
     MOVE        SPACE      TO         MEISAI-38-1.
     MOVE        NC"☆"     TO         MEISAI-38-R-STAR.
     MOVE        TOKR-F21   TO         WK-TOKR-F21.
     IF          TOKR-F21   NOT =  ZERO
                 MOVE    WK-TOKR-F21(4:4)  TO  MEISAI-38-R(1:4)
                 MOVE    "/"               TO  MEISAI-38-R(5:1)
                 MOVE    WK-TOKR-F21(8:2)  TO  MEISAI-38-R(6:2)
                 MOVE    "/"               TO  MEISAI-38-R(8:1)
                 MOVE    WK-TOKR-F21(10:2) TO  MEISAI-38-R(9:2)
     END-IF.
     IF  TOKR-F21    NOT =   TOKL-F21
         MOVE    NC"☆"     TO         MEISAI-38-L-STAR
         MOVE    TOKL-F21   TO         WK-TOKL-F21
         IF      TOKL-F21   NOT =  ZERO
                 MOVE    WK-TOKL-F21(4:4)  TO  MEISAI-38-L(1:4)
                 MOVE    "/"               TO  MEISAI-38-L(5:1)
                 MOVE    WK-TOKL-F21(8:2)  TO  MEISAI-38-L(6:2)
                 MOVE    "/"               TO  MEISAI-38-L(8:1)
                 MOVE    WK-TOKL-F21(10:2) TO  MEISAI-38-L(9:2)
         END-IF
     END-IF.
*  ☆計上部門コード
     MOVE        SPACE          TO     MEISAI-39-1.
     MOVE        NC"☆"         TO     MEISAI-39-R-STAR.
     MOVE        TOKR-F75       TO     MEISAI-39-R.
     IF          TOKR-F75       NOT =  TOKL-F75
                 MOVE  NC"☆"   TO     MEISAI-39-L-STAR
                 MOVE  TOKL-F75 TO     MEISAI-39-L
     END-IF.
*  ☆計上部門変更日
     MOVE        SPACE             TO     MEISAI-40-1.
     MOVE        NC"☆"            TO     MEISAI-40-R-STAR.
     MOVE        TOKR-F76          TO     WK-TOKR-F76.
     MOVE        WK-TOKR-F76(1:4)  TO     MEISAI-40-R(1:4).
     MOVE        "/"               TO     MEISAI-40-R(5:1).
     MOVE        WK-TOKR-F76(5:2)  TO     MEISAI-40-R(6:2).
     MOVE        "/"               TO     MEISAI-40-R(8:1).
     MOVE        WK-TOKR-F76(7:2)  TO     MEISAI-40-R(9:2).
     IF          TOKR-F76       NOT =     TOKL-F76
                 MOVE  NC"☆"             TO   MEISAI-40-L-STAR
                 MOVE  TOKL-F76           TO   WK-TOKL-F76
                 MOVE  WK-TOKL-F76(1:4)   TO   MEISAI-40-L(1:4)
                 MOVE  "/"                TO   MEISAI-40-L(5:1)
                 MOVE  WK-TOKL-F76(5:2)   TO   MEISAI-40-L(6:2)
                 MOVE  "/"                TO   MEISAI-40-L(8:1)
                 MOVE  WK-TOKL-F76(7:2)   TO   MEISAI-40-L(9:2)
     END-IF.
*  ☆変更部門コード
     MOVE        SPACE          TO     MEISAI-41-1.
     MOVE        NC"☆"         TO     MEISAI-41-R-STAR.
     MOVE        TOKR-F77       TO     MEISAI-41-R.
     IF          TOKR-F77       NOT =  TOKL-F77
*T↓
*         IF ( MSL-F05 = 20190315 ) AND
*            ( MSL-F06 = 112608   )
*              DISPLAY "TOKR-F77=" TOKR-F77 UPON CONS
*              DISPLAY "TOKL-F77=" TOKL-F77 UPON CONS
*         END-IF
*T↑
                 MOVE  NC"☆"   TO     MEISAI-41-L-STAR
                 MOVE  TOKL-F77 TO     MEISAI-41-L
     END-IF.
*#2019/09/27 NAV ST
*  ☆請求区分
     MOVE        SPACE      TO         MEISAI-42-1.
     MOVE        NC"☆"     TO         MEISAI-42-R-STAR.
     EVALUATE    TOKR-F90
      WHEN " "   MOVE   NC"請求しない"   TO     MEISAI-42-R
      WHEN "1"   MOVE   NC"請求する"     TO     MEISAI-42-R
      WHEN OTHER MOVE   NC"？？？？？"   TO     MEISAI-42-R
     END-EVALUATE.
     IF  TOKR-F90           NOT =   TOKL-F90
         MOVE        NC"☆"     TO       MEISAI-42-L-STAR
         EVALUATE    TOKL-F90
          WHEN " "   MOVE   NC"請求しない"   TO     MEISAI-42-L
          WHEN "1"   MOVE   NC"請求する"     TO     MEISAI-42-L
          WHEN OTHER MOVE   NC"？？？？？"   TO     MEISAI-42-L
         END-EVALUATE
     END-IF.
*#2019/09/27 NAV ED
*
 MEIEDT2-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し                  *
**********************************************************
 MEIWRT-SEC                   SECTION.
*
     ADD        1          TO        WT-CNT.
**************
*帳票書き出し*
**************
     WRITE      P-REC   FROM    MEISAI-01  AFTER  2.
     WRITE      P-REC   FROM    MEISAI-02  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-03  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-04  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-05  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-06  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-07  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-08  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-09  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-10  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-11  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-12  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-13  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-14  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-15  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-16  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-17  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-18  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-19  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-20  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-21  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-22  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-23  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-24  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-25  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-26  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-27  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-28  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-29  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-30  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-31  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-32  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-33  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-34  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-35  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-36  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-37  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-38  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-39  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-40  AFTER  1.
     WRITE      P-REC   FROM    MEISAI-41  AFTER  1.
*#2019/09/27 NAV ST
     WRITE      P-REC   FROM    MEISAI-42  AFTER  1.
*#2019/09/27 NAV ED
*
     ADD        1       TO      LINE-CNT.
*
 MEIWRT-EXIT.
     EXIT.
