# SIT0121L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SIT0121L.COB`

## ソースコード

```cobol
****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　基幹
*    サブシステム　　　　：　マスタ保守
*    モジュール名　　　　：　条件ファイル　メンテリスト
*    作成日／作成者　　　：　2019.03.20 INOUE
*    　　　　　　　　　　：　S11300660 マスタリスト改善　　　
*    更新履歴　　　　　　：
*    更新日／更新者　　　：　
*
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SIT0121L.
*                  流用元：SIT0061L
 AUTHOR.                   NAV.
 DATE-WRITTEN.             2019.03.20.
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
*マスタレコードエリア（条件ファイル）
     COPY    HJYOKEN       OF   XFDLIB
     JOINING JYOL          AS   PREFIX.
     COPY    HJYOKEN       OF   XFDLIB
     JOINING JYOR          AS   PREFIX.
*改ページ条件
 01  NEW-KEY.
     03  NEW-MSL-F04       PIC  X(01).
     03  NEW-MSL-F05       PIC  9(08).
*--- 03  NEW-MSL-F06       PIC  9(06).
 01  OLD-KEY.
     03  OLD-MSL-F04       PIC  X(01).
     03  OLD-MSL-F05       PIC  9(08).
*--- 03  OLD-MSL-F06       PIC  9(06).
*A
*01  WK-JYOL-F19            PIC  9(11) VALUE ZERO.
*01  WK-JYOL-F20            PIC  9(11) VALUE ZERO.
*01  WK-JYOL-F21            PIC  9(11) VALUE ZERO.
*01  WK-JYOL-F76            PIC  9(08) VALUE ZERO.
*01  WK-JYOR-F19            PIC  9(11) VALUE ZERO.
*01  WK-JYOR-F20            PIC  9(11) VALUE ZERO.
*01  WK-JYOR-F21            PIC  9(11) VALUE ZERO.
*01  WK-JYOR-F76            PIC  9(08) VALUE ZERO.
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
     02  FILLER              PIC  X(08)  VALUE  "SIT0121L".
     02  FILLER              PIC  X(12)  VALUE  SPACE.
     02  FILLER              PIC  N(14)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＜条件ファイルメンテリスト＞".
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
     02  FILLER              PIC  X(11)  VALUE  SPACE.
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
*--- 02  FILLER              PIC  N(05)  VALUE  NC"更新時刻：".
*--- 02  MIDASI-8-TIME       PIC  X(08).
     02  FILLER              PIC  X(18)  VALUE  SPACE.
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
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE  NC"更新時刻：".
     02  MIDASI-8-TIME       PIC  X(08).
     02  FILLER              PIC  X(16)  VALUE  SPACE.
     02  MIDASI-8-MODE-R     PIC  N(03).
*
****  明細行1               ****
 01  MEISAI-01                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　ＫＥＹ１　．．．．．．．".
     02  MEISAI-01-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-01-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-01-L         PIC  9(02).
         03  FILLER              PIC  X(38)  VALUE  SPACE.
         03  MEISAI-01-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-01-R         PIC  9(02).
****  明細行2               ****
 01  MEISAI-02                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　ＫＥＹ２　．．．．．．．".
     02  MEISAI-02-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-02-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-02-L         PIC  X(08).
         03  FILLER              PIC  X(32)  VALUE  SPACE.
         03  MEISAI-02-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-02-R         PIC  X(08).
****  明細行3               ****
 01  MEISAI-03                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　項目０１（日本語１）　．".
     02  MEISAI-03-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-03-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-03-L         PIC  N(20).
         03  MEISAI-03-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-03-R         PIC  N(20).
****  明細行4               ****
 01  MEISAI-04                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　項目０２（数字１）　．．".
     02  MEISAI-04-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-04-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-04-L         PIC  ----,---,--9.99.
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-04-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-04-R         PIC  ----,---,--9.99.
****  明細行5               ****
 01  MEISAI-05                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　項目０３（数字２）　．．".
     02  MEISAI-05-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-05-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-05-L         PIC  ----,---,--9.99.
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-05-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-05-R         PIC  ----,---,--9.99.
****  明細行6               ****
 01  MEISAI-06                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　項目０４（数字３）　．．".
     02  MEISAI-06-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-06-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-06-L         PIC  ----,---,--9.99.
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-06-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-06-R         PIC  ----,---,--9.99.
****  明細行7               ****
 01  MEISAI-07                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　項目０５（数字４）　．．".
     02  MEISAI-07-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-07-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-07-L         PIC  ----,---,--9.99.
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-07-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-07-R         PIC  ----,---,--9.99.
****  明細行8               ****
 01  MEISAI-08                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　項目０６（数字５）　．．".
     02  MEISAI-08-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-08-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-08-L         PIC  ----,---,--9.99.
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-08-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-08-R         PIC  ----,---,--9.99.
****  明細行9               ****
 01  MEISAI-09                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　項目０７（数字６）　．．".
     02  MEISAI-09-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-09-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-09-L         PIC  ----,---,--9.99.
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-09-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-09-R         PIC  ----,---,--9.99.
****  明細行10              ****
 01  MEISAI-10                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　項目０８（数字７）　．．".
     02  MEISAI-10-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-10-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-10-L         PIC  ----,---,--9.99.
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-10-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-10-R         PIC  ----,---,--9.99.
****  明細行11              ****
 01  MEISAI-11                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　項目０９（数字８）　．．".
     02  MEISAI-11-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-11-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-11-L         PIC  ----,---,--9.99.
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-11-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-11-R         PIC  ----,---,--9.99.
****  明細行12              ****
 01  MEISAI-12                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　項目１０（数字９）　．．".
     02  MEISAI-12-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-12-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-12-L         PIC  ----,---,--9.99.
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-12-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-12-R         PIC  ----,---,--9.99.
****  明細行13              ****
 01  MEISAI-13                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　項目１１（数字Ａ）　．．".
     02  MEISAI-13-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-13-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-13-L         PIC  ----,---,--9.99.
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-13-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-13-R         PIC  ----,---,--9.99.
****  明細行14              ****
 01  MEISAI-14                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　項目１２（数字Ｂ）　．．".
     02  MEISAI-14-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-14-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-14-L         PIC  ----,---,--9.99.
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-14-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-14-R         PIC  ----,---,--9.99.
****  明細行15              ****
 01  MEISAI-15                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　項目１３（数字Ｃ）　．．".
     02  MEISAI-15-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-15-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-15-L         PIC  ----,---,--9.99.
         03  FILLER              PIC  X(26)  VALUE  SPACE.
         03  MEISAI-15-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-15-R         PIC  ----,---,--9.99.
****  明細行16              ****
 01  MEISAI-16                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　項目１４（日本語２）　．".
     02  MEISAI-16-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-16-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-16-L         PIC  N(05).
         03  FILLER              PIC  X(30)  VALUE  SPACE.
         03  MEISAI-16-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-16-R         PIC  N(05).
****  明細行17              ****
 01  MEISAI-17                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　項目１５（英数字１）　．".
     02  MEISAI-17-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-17-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-17-L         PIC  X(15).
         03  FILLER              PIC  X(25)  VALUE  SPACE.
         03  MEISAI-17-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-17-R         PIC  X(15).
****  明細行18              ****
 01  MEISAI-18                   CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER                  PIC  X(01)  VALUE  SPACE.
     02  FILLER                  PIC  N(13)
         VALUE NC"　項目１６（英数字２）　．".
     02  MEISAI-18-1.
         03  FILLER              PIC  X(02)  VALUE  SPACE.
         03  MEISAI-18-L-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-18-L         PIC  X(15).
         03  FILLER              PIC  X(25)  VALUE  SPACE.
         03  MEISAI-18-R-STAR    PIC  N(01).
         03  FILLER              PIC  X(01)  VALUE  SPACE.
         03  MEISAI-18-R         PIC  X(15).
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
     MOVE        3         TO        LINE-CNT.
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
     MOVE     "05"                TO   MSL-F01.
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
     IF ( LINE-CNT  >  2 ) OR ( NEW-KEY NOT = OLD-KEY )
          PERFORM MIDA-SEC
          MOVE    MSL-F04  TO   OLD-MSL-F04
          MOVE    MSL-F05  TO   OLD-MSL-F05
*---      MOVE    MSL-F06  TO   OLD-MSL-F06
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
     IF   ( MSL-F01  >  "05"            ) OR
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
*--- MOVE   MSL-F06           TO        NEW-MSL-F06.
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
     DISPLAY "SIT0121L READ =" READ-CNT NC"件" UPON CONS.
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
     MOVE       MSL-F06(1:2)      TO       MIDASI-8-TIME(1:2).
     MOVE       ":"               TO       MIDASI-8-TIME(3:1).
     MOVE       MSL-F06(3:2)      TO       MIDASI-8-TIME(4:2).
     MOVE       ":"               TO       MIDASI-8-TIME(6:1).
     MOVE       MSL-F06(5:2)      TO       MIDASI-8-TIME(7:2).
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
             MOVE       MSL-F08     TO      JYOL-REC
             MOVE       MSL-F09     TO      JYOR-REC
* 項目セット
             PERFORM    MEIEDT2-SEC
             GO                     TO      MEIEDT-EXIT
     END-IF.
*
*登録・削除レコード準備
     MOVE        MSL-F08    TO        JYOL-REC.
* 項目セット
*   ＫＥＹ１
     MOVE        SPACE      TO        MEISAI-01-1.
     MOVE        JYOL-F01   TO        MEISAI-01-L.
*   ＫＥＹ２
     MOVE        SPACE      TO        MEISAI-02-1.
     MOVE        JYOL-F02   TO        MEISAI-02-L.
*   項目０１（日本語１）
     MOVE        SPACE      TO        MEISAI-03-1.
     MOVE        JYOL-F03   TO        MEISAI-03-L.
*   項目０２（数字１）
     MOVE        SPACE      TO        MEISAI-04-1.
     MOVE        JYOL-F04   TO        MEISAI-04-L.
*   項目０３（数字２）
     MOVE        SPACE      TO        MEISAI-05-1.
     MOVE        JYOL-F05   TO        MEISAI-05-L.
*   項目０４（数字３）
     MOVE        SPACE      TO        MEISAI-06-1.
     MOVE        JYOL-F06   TO        MEISAI-06-L.
*   項目０５（数字４）
     MOVE        SPACE      TO        MEISAI-07-1.
     MOVE        JYOL-F07   TO        MEISAI-07-L.
*   項目０６（数字５）
     MOVE        SPACE      TO        MEISAI-08-1.
     MOVE        JYOL-F08   TO        MEISAI-08-L.
*   項目０７（数字６）
     MOVE        SPACE      TO        MEISAI-09-1.
     MOVE        JYOL-F09   TO        MEISAI-09-L.
*   項目０８（数字７）
     MOVE        SPACE      TO        MEISAI-10-1.
     MOVE        JYOL-F10   TO        MEISAI-10-L.
*   項目０９（数字８）
     MOVE        SPACE      TO        MEISAI-11-1.
     MOVE        JYOL-F11   TO        MEISAI-11-L.
*   項目１０（数字９）
     MOVE        SPACE      TO        MEISAI-12-1.
     MOVE        JYOL-F12   TO        MEISAI-12-L.
*   項目１１（数字Ａ）
     MOVE        SPACE      TO        MEISAI-13-1.
     MOVE        JYOL-F12A  TO        MEISAI-13-L.
*   項目１２（数字Ｂ）
     MOVE        SPACE      TO        MEISAI-14-1.
     MOVE        JYOL-F12B  TO        MEISAI-14-L.
*   項目１３（数字Ｃ）
     MOVE        SPACE      TO        MEISAI-15-1.
     MOVE        JYOL-F12C  TO        MEISAI-15-L.
*   項目１４（日本語２）
     MOVE        SPACE      TO        MEISAI-16-1.
     MOVE        JYOL-F13   TO        MEISAI-16-L.
*   項目１５（英数字１）
     MOVE        SPACE      TO        MEISAI-17-1.
     MOVE        JYOL-F14   TO        MEISAI-17-L.
*   項目１６（英数字２）
     MOVE        SPACE      TO        MEISAI-18-1.
     MOVE        JYOL-F15   TO        MEISAI-18-L.
*
 MEIEDT-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集（修正モード）           *
**********************************************************
 MEIEDT2-SEC                  SECTION.
*
* 項目セット
*   ＫＥＹ１
     MOVE        SPACE      TO        MEISAI-01-1.
     MOVE        JYOR-F01   TO        MEISAI-01-R.
     IF          JYOR-F01   NOT =     JYOL-F01
                 MOVE       JYOL-F01  TO        MEISAI-01-L
     END-IF.
*   ＫＥＹ２
     MOVE        SPACE      TO        MEISAI-02-1.
     MOVE        JYOR-F02   TO        MEISAI-02-R.
     IF          JYOR-F02   NOT =     JYOL-F02
                 MOVE       JYOL-F02  TO        MEISAI-02-L
     END-IF.
*   項目０１（日本語１）
     MOVE        SPACE      TO        MEISAI-03-1.
     MOVE        JYOR-F03   TO        MEISAI-03-R.
     IF          JYOR-F03   NOT =     JYOL-F03
                 MOVE       JYOL-F03  TO        MEISAI-03-L
     END-IF.
*   項目０２（数字１）
     MOVE        SPACE      TO        MEISAI-04-1.
     MOVE        JYOR-F04   TO        MEISAI-04-R.
     IF          JYOR-F04   NOT =     JYOL-F04
                 MOVE       JYOL-F04  TO        MEISAI-04-L
     END-IF.
*   項目０３（数字２）
     MOVE        SPACE      TO        MEISAI-05-1.
     MOVE        JYOR-F05   TO        MEISAI-05-R.
     IF          JYOR-F05   NOT =     JYOL-F05
                 MOVE       JYOL-F05  TO        MEISAI-05-L
     END-IF.
*   項目０４（数字３）
     MOVE        SPACE      TO        MEISAI-06-1.
     MOVE        JYOR-F06   TO        MEISAI-06-R.
     IF          JYOR-F06   NOT =     JYOL-F06
                 MOVE       JYOL-F06  TO        MEISAI-06-L
     END-IF.
*   項目０５（数字４）
     MOVE        SPACE      TO        MEISAI-07-1.
     MOVE        JYOR-F07   TO        MEISAI-07-R.
     IF          JYOR-F07   NOT =     JYOL-F07
                 MOVE       JYOL-F07  TO        MEISAI-07-L
     END-IF.
*   項目０６（数字５）
     MOVE        SPACE      TO        MEISAI-08-1.
     MOVE        JYOR-F08   TO        MEISAI-08-R.
     IF          JYOR-F08   NOT =     JYOL-F08
                 MOVE       JYOL-F08  TO        MEISAI-08-L
     END-IF.
*   項目０７（数字６）
     MOVE        SPACE      TO        MEISAI-09-1.
     MOVE        JYOR-F09   TO        MEISAI-09-R.
     IF          JYOR-F09   NOT =     JYOL-F09
                 MOVE       JYOL-F09  TO        MEISAI-09-L
     END-IF.
*   項目０８（数字７）
     MOVE        SPACE      TO        MEISAI-10-1.
     MOVE        JYOR-F10   TO        MEISAI-10-R.
     IF          JYOR-F10   NOT =     JYOL-F10
                 MOVE       JYOL-F10  TO        MEISAI-10-L
     END-IF.
*   項目０９（数字８）
     MOVE        SPACE      TO        MEISAI-11-1.
     MOVE        JYOR-F11   TO        MEISAI-11-R.
     IF          JYOR-F11   NOT =     JYOL-F11
                 MOVE       JYOL-F11  TO        MEISAI-11-L
     END-IF.
*   項目１０（数字９）
     MOVE        SPACE      TO        MEISAI-12-1.
     MOVE        JYOR-F12   TO        MEISAI-12-R.
     IF          JYOR-F12   NOT =     JYOL-F12
                 MOVE       JYOL-F12  TO        MEISAI-12-L
     END-IF.
*   項目１１（数字Ａ）
     MOVE        SPACE      TO        MEISAI-13-1.
     MOVE        JYOR-F12A  TO        MEISAI-13-R.
     IF          JYOR-F12A  NOT =     JYOL-F12A
                 MOVE       JYOL-F12A TO        MEISAI-13-L
     END-IF.
*   項目１２（数字Ｂ）
     MOVE        SPACE      TO        MEISAI-14-1.
     MOVE        JYOR-F12B  TO        MEISAI-14-R.
     IF          JYOR-F12B  NOT =     JYOL-F12B
                 MOVE       JYOL-F12B TO        MEISAI-14-L
     END-IF.
*   項目１３（数字Ｃ）
     MOVE        SPACE      TO        MEISAI-15-1.
     MOVE        JYOR-F12C  TO        MEISAI-15-R.
     IF          JYOR-F12C  NOT =     JYOL-F12C
                 MOVE       JYOL-F12C TO        MEISAI-15-L
     END-IF.
*   項目１４（日本語２）
     MOVE        SPACE      TO        MEISAI-16-1.
     MOVE        JYOR-F13   TO        MEISAI-16-R.
     IF          JYOR-F13   NOT =     JYOL-F13
                 MOVE       JYOL-F13  TO        MEISAI-16-L
     END-IF.
*   項目１５（英数字１）
     MOVE        SPACE      TO        MEISAI-17-1.
     MOVE        JYOR-F14   TO        MEISAI-17-R.
     IF          JYOR-F14   NOT =     JYOL-F14
                 MOVE       JYOL-F14  TO        MEISAI-17-L
     END-IF.
*   項目１６（英数字２）
     MOVE        SPACE      TO        MEISAI-18-1.
     MOVE        JYOR-F15   TO        MEISAI-18-R.
     IF          JYOR-F15   NOT =     JYOL-F15
                 MOVE       JYOL-F15  TO        MEISAI-18-L
     END-IF.
*
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
     IF         LINE-CNT  >     1
                MOVE    MSL-F06(1:2)   TO    MIDASI-8-TIME(1:2)
                MOVE    ":"            TO    MIDASI-8-TIME(3:1)
                MOVE    MSL-F06(3:2)   TO    MIDASI-8-TIME(4:2)
                MOVE    ":"            TO    MIDASI-8-TIME(6:1)
                MOVE    MSL-F06(5:2)   TO    MIDASI-8-TIME(7:2)
                WRITE   P-REC   FROM   MIDASI-8  AFTER  5
     END-IF.
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
*
     ADD        1       TO      LINE-CNT.
*
 MEIWRT-EXIT.
     EXIT.

```
