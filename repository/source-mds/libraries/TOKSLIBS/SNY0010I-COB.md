# SNY0010I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNY0010I.COB`

## ソースコード

```cobol
**************************************************************
*    顧客名　　　　　：　_サカタのタネ殿　　　　　　　　　　*
*    業務名　　　　　：　入荷管理システム　　　　　　　　　　*
*    サブシステム名　：　入荷管理　                          *
*    モジュール名　　：　発注消込入力                        *
*    プログラムＩＤ　：　SNY0010I                            *
*    作成日　　　　　：　2000/05/13                          *
*    　作成者　　　　：　  T.TAKAHASHI                       *
*    更新日　　　　　：　2006/07/10                          *
*    　更新者　　　　：　  NAV TAKAHASHI                     *
*    　更新内容　　　：　  相殺区分＞０は対象外とする　　　　*
*    更新日　　　　　：　2008/08/08 - 08/11                  *
*    　更新者　　　　：　  NAV TAKEI                         *
*    　更新内容　　　：　  内部統制対応・担当者　　　　　　　*
*    更新日　　　　　：　2013/12/18                          *
*    　更新者　　　　：　  NAV INOUE                         *
*    　更新内容　　　：　  消費税増税対応　　　　　　　　　　*
*    更新日　　　　　：　2013/12/26                          *
*    　更新者　　　　：　  NAV INOUE                         *
*    　更新内容　　　：　  消費税　入力税区分　適用日チェック*
*    更新日　　　　　：　2014/05/09                          *
*    　更新者　　　　：　  NAV TAKAHASHI *$$                 *
*    　更新内容　　　：　  修正時障害対応　　　　　　　　　　*
**************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SNY0010I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/5/13.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
***************************************************************
 INPUT-OUTPUT           SECTION.
***************************************************************
 FILE-CONTROL.
*----<< 取引先マスタ >>-*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-ST1.
*----<< 店舗マスタ >>-*
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE      STATUS    TEN-ST1.
*----<< 仕入先マスタ >>-*
     SELECT   ZSHIMS    ASSIGN    TO        DA-01-VI-ZSHIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SHI-F01
                        FILE      STATUS    SHI-ST1.
*----<< 条件ファイル >>-*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-ST1.
*----<< 商品名称マスタ >>-*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F01
                        FILE      STATUS    MEI-ST1.
*----<< 倉庫マスタ >>-*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE      STATUS    SOK-ST1.
*----<< 発注ファイル（ヘッダ） >>-*
     SELECT   HACHEDF   ASSIGN    TO        DA-01-VI-HACHEDL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       HAH-F02
                        FILE      STATUS    HAH-ST1.
*----<< 発注ファイル（ヘッダ） >>-*
     SELECT   HACMEIF   ASSIGN    TO        DA-01-VI-HACMEIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       HAM-F02 HAM-F03
                        FILE      STATUS    HAM-ST1.
*----<< 入庫ファイル >>-*
     SELECT   NYKFILF   ASSIGN    TO        DA-01-VI-NYKFILL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       NYU-F02  NYU-F03
                                            NYU-F04  NYU-F05
                        FILE      STATUS    NYU-ST1.
*----<< 商品在庫マスタ >>-*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       ZAI-F01  ZAI-F021
                                            ZAI-F022 ZAI-F031
                                            ZAI-F032 ZAI-F033
                        FILE      STATUS    ZAI-ST1.
*---<<  商品コード変換テーブル  >>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SHO-F01
                                            SHO-F031
                                            SHO-F032
                        FILE      STATUS    SHO-ST1.
*----<< 担当者マスタ    >>-*  2008.08.08
     SELECT   HTANMS    ASSIGN    TO        DA-01-VI-TANMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TAN-F01   TAN-F02
                        FILE      STATUS    TAN-ST1.
*----<< 画面ファイル >>-*
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
                        SYMBOLIC  DESTINATION        "DSP"
                        FORMAT              DSP-FMT
                        GROUP               DSP-GRP
                        PROCESSING  MODE    DSP-PRO
                        SELECTED  FUNCTION  DSP-FNC
                        FILE      STATUS    DSP-ST1.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*----<< 取引先マスタ >>-*
 FD  HTOKMS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HTOKMS    OF   XFDLIB    JOINING   TOK  AS   PREFIX.
*----<< 店舗マスタ >>-*
 FD  HTENMS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HTENMS    OF   XFDLIB    JOINING   TEN  AS   PREFIX.
*----<< 仕入先マスタ >>-*
 FD  ZSHIMS
                        LABEL     RECORD     IS  STANDARD.
     COPY     ZSHIMS    OF   XFDLIB    JOINING   SHI  AS   PREFIX.
*----<< 条件ファイル >>-*
 FD  HJYOKEN
                        LABEL     RECORD     IS  STANDARD.
     COPY     HJYOKEN   OF   XFDLIB    JOINING   JYO  AS   PREFIX.
*----<< 商品名称マスタ >>-*
 FD  HMEIMS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HMEIMS    OF   XFDLIB    JOINING   MEI  AS   PREFIX.
*----<< 倉庫マスタ >>-*
 FD  ZSOKMS
                        LABEL     RECORD     IS  STANDARD.
     COPY     ZSOKMS    OF   XFDLIB    JOINING   SOK  AS   PREFIX.
*----<< 発注ファイル（ヘッダ） >>-*
 FD  HACHEDF
                        LABEL     RECORD     IS  STANDARD.
     COPY     HACHEDF   OF   XFDLIB    JOINING   HAH  AS   PREFIX.
*----<< 発注ファイル（明細） >>-*
 FD  HACMEIF
                        LABEL     RECORD     IS  STANDARD.
     COPY     HACMEIF   OF   XFDLIB    JOINING   HAM  AS   PREFIX.
*----<< 入庫ファイル >>-*
 FD  NYKFILF
                        LABEL     RECORD     IS  STANDARD.
     COPY     NYKFILF   OF   XFDLIB    JOINING   NYU  AS   PREFIX.
*----<< 商品在庫マスタ >>-*
 FD  ZAMZAIF
                        LABEL     RECORD     IS  STANDARD.
     COPY     ZAMZAIF   OF   XFDLIB    JOINING   ZAI  AS   PREFIX.
*----<< 商品ＣＤ変換ＴＢＬ>>-*
 FD  HSHOTBL
                        LABEL     RECORD     IS  STANDARD.
     COPY     HSHOTBL   OF   XFDLIB    JOINING   SHO  AS   PREFIX.
*----<< 担当者マスタ>>-* 2008.08.08
 FD  HTANMS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HTANMS    OF   XFDLIB    JOINING   TAN  AS   PREFIX.
*----<< 画面ファイル >>-*
 FD  DSPF.
     COPY     FNY00101   OF   XMDLIB.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  PGM-ID                  PIC  X(08)     VALUE  "SNY0010I".
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  DSP-CNTL.
     03  DSP-FMT             PIC  X(08).
     03  DSP-GRP             PIC  X(08).
     03  DSP-PRO             PIC  X(02).
     03  DSP-FNC             PIC  X(04).
     03  DSP-ST1             PIC  X(02).
     03  DSP-ST2             PIC  X(04).
     03  DSP-CON             PIC  X(06).
     03  WK-GRP.
         05  WK-GRP-BODY     PIC  X(04).
         05  WK-GRP-L-CNT    PIC  9(02).
         05  FILLER          PIC  X(02).
 01  STATUS-AREA.
     03  IN-DATA             PIC  X(01).
     03  TOK-ST1             PIC  X(02).
     03  TEN-ST1             PIC  X(02).
     03  SHI-ST1             PIC  X(02).
     03  JYO-ST1             PIC  X(02).
     03  MEI-ST1             PIC  X(02).
     03  SOK-ST1             PIC  X(02).
     03  HAH-ST1             PIC  X(02).
     03  HAM-ST1             PIC  X(02).
     03  NYU-ST1             PIC  X(02).
     03  ZAI-ST1             PIC  X(02).
     03  SHO-ST1             PIC  X(02).
     03  TAN-ST1             PIC  X(02).
*
 01  WK-TAIHI.
     03  WK-SIIRE            PIC  9(08)V99  VALUE  ZERO.
     03  WK-SIIREG           PIC  9(08)V99  VALUE  ZERO.
     03  WK-GENKA            PIC  9(08)V99  VALUE  ZERO.
     03  WK-GENKAG           PIC  9(08)V99  VALUE  ZERO.
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ﾃ-ﾌﾞﾙ >>-*
 01  FNC-TABLE.
     03  ENT                 PIC  X(04)  VALUE "E000".
     03  PF04                PIC  X(04)  VALUE "F004".
     03  PF05                PIC  X(04)  VALUE "F005".
     03  PF06                PIC  X(04)  VALUE "F006".
     03  PF11                PIC  X(04)  VALUE "F011".
     03  PF12                PIC  X(04)  VALUE "F012".
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｶﾞｲﾀﾞﾝｽ ｴﾘｱ >>-*
 01  GUIDE-AREA.
     03  G001                PIC  N(30)  VALUE
         NC"_取消_終了_項目戻り_前レコード_次レコード".
     03  G002                PIC  N(30)  VALUE
         NC"_取消_終了_項目戻り".
     03  G003                PIC  N(30)  VALUE
         NC"_取消_終了".
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
                             NC"ＰＦキーが違います".
     03  MSG02               PIC  N(20)  VALUE
                             NC"処理区分が違います".
     03  MSG03               PIC  N(20)  VALUE
                             NC"相殺区分が違います".
     03  MSG04               PIC  N(20)  VALUE
                             NC"計上区分が違います".
     03  MSG05               PIC  N(20)  VALUE
                             NC"納入日に誤りがあります".
     03  MSG06               PIC  N(20)  VALUE
                             NC"送料区分が違います".
     03  MSG07               PIC  N(20)  VALUE
                             NC"送料に誤りがあります".
     03  MSG08               PIC  N(20)  VALUE
                             NC"該当するデータがありません".
     03  MSG09               PIC  N(20)  VALUE
                             NC"該当するデータではありません".
     03  MSG10               PIC  N(20)  VALUE
                             NC"完了区分が違います".
     03  MSG11               PIC  N(20)  VALUE
                             NC"単価区分が違います".
     03  MSG12               PIC  N(20)  VALUE
                             NC"数量に誤りがあります".
     03  MSG13               PIC  N(20)  VALUE
                             NC"仕単／金額に誤りがあります".
     03  MSG14               PIC  N(20)  VALUE
                             NC"原単／金額に誤りがあります".
     03  MSG15               PIC  N(20)  VALUE
                             NC"Ｙ，Ｈ，Ｎを入力して下さい".
     03  MSG16               PIC  N(20)  VALUE
                             NC"前レコードはありません".
     03  MSG17               PIC  N(20)  VALUE
                             NC"次レコードはありません".
     03  MSG18               PIC  N(20)  VALUE
                             NC"Ｈは入力できません".
     03  MSG19               PIC  N(20)  VALUE
              NC"登録時、ＰＦ_・_は使用できません".
     03  MSG20               PIC  N(20)  VALUE
                             NC"発注_に誤りがあります".
     03  MSG21               PIC  N(20)  VALUE
                             NC"締日を過ぎています".
     03  MSG22               PIC  N(20)  VALUE
                             NC"仕入先コードに誤りがあります".
     03  MSG23               PIC  N(20)  VALUE
                             NC"商品コードに誤りがあります".
     03  MSG24               PIC  N(20)  VALUE
                             NC"商品コードは変更できません".
     03  MSG25               PIC  N(20)  VALUE
                             NC"該当データはありません。－発明".
     03  MSG26               PIC  N(20)  VALUE
                     NC"納品日が締日以下の為、処理できません。".
     03  MSG27               PIC  N(20)  VALUE
                             NC"既に相殺処理がされています。".
     03  MSG28               PIC  N(20)  VALUE
                           NC"支払締日を正しく入力して下さい。".
     03  MSG29               PIC  N(20)  VALUE
                     NC"既に分納分で商品が変更されたいます。".
     03  MSG30               PIC  N(20)  VALUE
                     NC"入荷された数量をチェックして下さい。".
     03  MSG31               PIC  N(20)  VALUE
                     NC"赤黒処理は出来ません。".
     03  MSG32               PIC  N(20)  VALUE
                     NC"計上済の伝票です。修正出来ません。".
*
     03  MSG33               PIC  N(20)  VALUE
                     NC"担当者コードに誤りがあります".
*2013/12/18↓
     03  MSG34               PIC  N(20)  VALUE
     NC"計上税区分が条件ファイルに未登録です！！".
*2013/12/18↑
*2013/12/26↓
     03  MSG35               PIC  N(20)  VALUE
     NC"入力した税区分は適用日に至っていません！".
*2013/12/26↑
 01  FILLER                  REDEFINES   MSG-AREA.
*2013/12/18↓
***  03  MSG-TBL             PIC  N(20)  OCCURS       33.
*2013/12/26↓
*    03  MSG-TBL             PIC  N(20)  OCCURS       34.
     03  MSG-TBL             PIC  N(20)  OCCURS       35.
*2013/12/26↑
*2013/12/18↑
 01  SYS-DATE                PIC  9(06).
 01  FILLER                  REDEFINES   SYS-DATE.
     03  SYS-YY              PIC  9(02).
     03  SYS-MM              PIC  9(02).
     03  SYS-DD              PIC  9(02).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
 01  WK-DATE.
     03  WK-YY               PIC  9(02).
     03  WK-MM               PIC  9(02).
     03  WK-DD               PIC  9(02).
*
 01  WK-YYYYMMDD.
     03  WK-YYYY             PIC  9(04)  VALUE  ZERO.
     03  WK-YY-MM            PIC  9(02)  VALUE  ZERO.
     03  WK-YY-DD            PIC  9(02)  VALUE  ZERO.
*
 01  WK-BODY.
     03  WK-MEISAI           OCCURS  6.
         05  IN-FLG          PIC  9(01)         VALUE ZERO.
         05  WK-SURYO        PIC  S9(06)V9(02)  VALUE ZERO.
         05  WK-TAN1         PIC  X(01)         VALUE SPACE.
         05  WK-SITAN        PIC  S9(07)V9(02)  VALUE ZERO.
         05  WK-TAN2         PIC  X(01)         VALUE SPACE.
         05  WK-GENTAN       PIC  S9(07)V9(02)  VALUE ZERO.
         05  WK-SURYOX       PIC  S9(06)V9(02)  VALUE ZERO.
         05  WK-HAM-F18      PIC   X(01)        VALUE SPACE.
         05  WK-HAM-F181     PIC   X(01)        VALUE SPACE.
         05  WK-HAM-F06      PIC   X(08)        VALUE SPACE.
         05  WK-HAM-F07.
             07  WK-HAM-F071 PIC   X(05)        VALUE SPACE.
             07  WK-HAM-F072 PIC   X(02)        VALUE SPACE.
             07  WK-HAM-F073 PIC   X(01)        VALUE SPACE.
         05  WK-HAM-F09      PIC  S9(08)V99     VALUE ZERO.
         05  WK-HAM-F10      PIC  S9(08)V99     VALUE ZERO.
         05  WK-HAM-F101     PIC  S9(08)V99     VALUE ZERO.
         05  WK-HAH-F17      PIC   X(02)        VALUE SPACE.
         05  WK-HAM-F08      PIC   X(06)        VALUE SPACE.
         05  WK-HAM-F081     PIC   X(06)        VALUE SPACE.
         05  WK-NYU-F35      PIC   X(01)        VALUE SPACE.
         05  WK-KANRYO       PIC   9(01)        VALUE ZERO.
         05  WK-ZANSURYO     PIC  S9(06)V9(02)  VALUE ZERO.
 01  SHO-CHG                 PIC   9(01)  VALUE ZERO.
*
 01  HIZ-CHK.
     03  WK-AMARI                PIC  9V99.
     03  WK-SHO                  PIC  9(05).
     03  WK-MATUBI               PIC  9(02).
*
 01  WK-ZAN-SURYO            PIC  S9(06)V9(02)  VALUE  ZERO.
 01  WK-WKSTN                PIC  X(02)   VALUE  SPACE.
 01  WK-DSOKO                PIC  X(02)   VALUE  SPACE.
 01  WK-SIME                 PIC  9(09)V99.
 01  WK-SIME-R REDEFINES     WK-SIME.
     03  WK-SIME5            PIC  9(03).
     03  WK-SIME4.
         05  WK-SIME41       PIC  9(04).
         05  WK-SIME42       PIC  9(02).
     03  WK-SIME2            PIC  9(02).
 01  WK-SIMEX                PIC  9(09)V99.
 01  WK-SIME-Z REDEFINES     WK-SIMEX.
     03  WK-SIME5X           PIC  9(03).
     03  WK-SIME6X.
         05  WK-SIME41X      PIC  9(04).
         05  WK-SIME42X      PIC  9(02).
     03  WK-SIME2X           PIC  9(02).
 01  WK-NOUNYU.
     03  WK-NOUNYU4            PIC  9(06).
     03  WK-NOUNYU2            PIC  9(02).
 01  WK-NOUNYUX.
     03  WK-NOUNYU1X           PIC  9(02).
     03  WK-NOUNYU2X           PIC  9(02).
     03  WK-NOUNYU3X           PIC  9(02).
 01  WK-HEN-DATE1            PIC  9(06)   VALUE  ZERO.
 01  WK-HEN-DATE2            PIC  9(08)   VALUE  ZERO.
*2013/12/26↓
 01  WK-CHK-NOUHIN           PIC  9(08)   VALUE  ZERO.
*2013/12/26↑
 01  WK-EDA                  PIC  9(02)   VALUE  ZERO.
 01  WK-TUKI                 PIC  9(02)   VALUE  ZERO.
 01  WK-GYO                  PIC  9(02)   VALUE  ZERO.
 01  WK-SYORI                PIC  9(01)   VALUE  ZERO.
 01  WK-SOUSAI               PIC  9(01)   VALUE  ZERO.
 01  WK-DENKU                PIC  9(02)   VALUE  ZERO.
 01  WK-SA                   PIC  S9(06)V9(02)  VALUE  ZERO.
 01  WK-KEY.
     03  WK-SOSAI             PIC  9(01)   VALUE  ZERO.
     03  WK-HACYU             PIC  9(07)   VALUE  ZERO.
     03  WK-HACYUE             PIC  9(02)   VALUE  ZERO.
 01  WK-KEY2.
     03  WK-HAH31            PIC  X(02)   VALUE  SPACE.
     03  WK-HAH20            PIC  X(08)   VALUE  SPACE.
     03  WK-HAH21            PIC  X(08)   VALUE  SPACE.
     03  WK-HAH30            PIC  X(06)   VALUE  SPACE.
 01  WK-KEY3.
     03  WK-NYU24            PIC  X(02)   VALUE  SPACE.
     03  WK-NYU13            PIC  X(08)   VALUE  SPACE.
     03  WK-NYU14            PIC  X(08)   VALUE  SPACE.
     03  WK-NYU22            PIC  X(06)   VALUE  SPACE.
*
 01  INDEXES.
     03  I                   PIC  9(02)   VALUE  ZERO.
     03  IXA                 PIC  9(02)   VALUE  ZERO.
     03  IXB                 PIC  9(02)   VALUE  ZERO.
     03  IXC                 PIC  9(02)   VALUE  ZERO.
     03  IXD                 PIC  9(02)   VALUE  ZERO.
 01  FLAGS.
     03  SONZAI-F            PIC  9(01)   VALUE  ZERO.
     03  SONZAI-F2           PIC  9(01)   VALUE  ZERO.
     03  SONZAI-F3           PIC  9(01)   VALUE  ZERO.
     03  SONZAI-F4           PIC  9(01)   VALUE  ZERO.
     03  END-FLG             PIC  9(01)   VALUE  ZERO.
     03  ERR-FLG             PIC  9(02)   VALUE  ZERO.
     03  INV-FLG             PIC  9(01)   VALUE  ZERO.
     03  MST-INV             PIC  9(01)   VALUE  ZERO.
     03  HEN-FLG             PIC  9(01)   VALUE  ZERO.
     03  PF-FLG              PIC  9(01)   VALUE  ZERO.
     03  GR-NO               PIC  9(02)   VALUE  ZERO.
     03  HAH-READ-FLG        PIC  X(03)   VALUE  SPACE.
     03  HAM-READ-FLG        PIC  X(03)   VALUE  SPACE.
     03  NYK-READ-FLG        PIC  X(03)   VALUE  SPACE.
*2013/12/18↓
     03  JYO-INV-FLG         PIC  9(01)   VALUE  ZERO.
*2013/12/18↑
 01  WK-HACHU-SA             PIC S9(08)V99  VALUE ZERO.
*支払締日チェック用
 01  WK-CHK1-DT.
     03  WK-CHK1-DT1         PIC  9(02)   VALUE  ZERO.
     03  WK-CHK1-DT2         PIC  9(02)   VALUE  ZERO.
 01  WK-CHK2-DT.
     03  WK-CHK2-DT1         PIC  9(02)   VALUE  ZERO.
     03  WK-CHK2-DT2         PIC  9(02)   VALUE  ZERO.
*支払締日変換用
 01  WK-SSIME.
     03  WK-SSIME-YYMM       PIC  9(04)   VALUE  ZERO.
     03  WK-SSIME-DD         PIC  9(02)   VALUE  ZERO.
*担当者ＣＤ退避
 01  WK-TANTO                PIC  X(02)   VALUE  SPACE.
*画面表示日付編集
 01  DATE-AREA               PIC  9(08)   VALUE  ZERO.
 01  HEN-DATE.
     03  HEN-DATE-YYYY       PIC  9(04)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM         PIC  9(02)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD         PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH         PIC  9(02)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM         PIC  9(02)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS         PIC  9(02)  VALUE  ZERO.
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER              PIC  N(01)   VALUE  NC"（".
     03  HEN-TOKHAN          PIC  N(06)   VALUE  SPACE.
     03  FILLER              PIC  N(01)   VALUE  NC"）".
*日付変換サブルーチンワーク
 01  LINK-AREA2.
     03  LINK-IN-KBN         PIC  X(01).
     03  LINK-IN-YMD6        PIC  9(06).
     03  LINK-IN-YMD8        PIC  9(08).
     03  LINK-OUT-RET        PIC  X(01).
     03  LINK-OUT-YMD8       PIC  9(08).
*---<< ｻﾌﾞﾙｰﾁﾝ LINK AREA >>-*
 LINKAGE                     SECTION.
 01  LINK-WKSTN              PIC  X(08).
 01  LINK-M-TANCD            PIC  X(02).
 01  LINK-M-BUMON            PIC  X(04).
******************************************************************
 PROCEDURE                 DIVISION  USING  LINK-WKSTN
                                            LINK-M-TANCD
                                            LINK-M-BUMON.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   0     エラー処理　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DECLARATIVES.
*----------  取引先マスタ　------------------------------------*
 TOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HTOKMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"取引先マスタ異常！"
              "ST1=" TOK-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  店舗マスタ　--------------------------------------*
 TEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HTENMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"店舗マスタ異常！"
              "ST1=" TEN-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  仕入先マスタ--------------------------------------*
 SHI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   ZSHIMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"仕入先マスタ異常！"
              "ST1=" TEN-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------   条件ファイル　-----------------------------------*
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HJYOKEN.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"条件ファイル異常！"
              "ST1=" JYO-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------   商品名称マスタ　 --------------------------------*
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HMEIMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"商品名称マスタ異常！"
              "ST1=" MEI-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------    倉庫マスタ　 -----------------------------------*
 SOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   ZSOKMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"倉庫マスタ異常！"
              "ST1=" SOK-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  発注ファイル（ヘッダ）   -------------------------*
 HAH-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HACHEDF.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"発注（Ｈ）Ｆ異常！"
              "ST1=" HAH-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  発注ファイル（明細）   ---------------------------*
 HAM-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HACMEIF.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"発注（Ｍ）Ｆ異常！"
              "ST1=" HAM-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  入庫ファイル   -----------------------------------*
 NYU-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   NYKFILF.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"入庫ファイル異常！"
              "ST1=" NYU-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  商品在庫マスタ -----------------------------------*
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   ZAMZAIF.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"商品在庫マスタ異常！"
              "ST1=" ZAI-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  商品ＣＤ変換ＴＢＬ -------------------------------*
 SHO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HSHOTBL.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"商品変換ＴＢＬ異常！"
              "ST1=" SHO-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  担当者マスタ   -----------------------------------*
 TAN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HTANMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"担当者マスタ異常！"
              "ST1=" TAN-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------    表示ファイル -----------------------------------*
 DSP-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   DSPF.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"表示ファイル異常！"
              "ST1=" DSP-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1         プログラムコントコール
*--------------------------------------------------------------*
 PROG-CNTL          SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     GR-NO    =    99.
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   2         初期処理
*--------------------------------------------------------------*
 INIT-RTN           SECTION.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " START "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STAT.
**
     DISPLAY  "LINK-M-TANCD="  LINK-M-TANCD   UPON  STAT.
     DISPLAY  "LINK-M-BUMON="  LINK-M-BUMON   UPON  STAT.
*
*
*
     MOVE        ZERO      TO        FLAGS.
*
     OPEN     INPUT     HTOKMS
                        HTENMS
                        ZSHIMS
                        HJYOKEN
                        HMEIMS
                        ZSOKMS  HTANMS
                        HSHOTBL.
     OPEN     I-O       HACHEDF  HACMEIF
                        NYKFILF
                        ZAMZAIF
                        DSPF.
*条件ファイル読込み（実行倉庫）文字
     DISPLAY "LINK-WKSTN = " LINK-WKSTN UPON STAT.
     MOVE     "65"           TO   JYO-F01.
     MOVE     LINK-WKSTN     TO   JYO-F02.
     READ     HJYOKEN
              INVALID
              DISPLAY   "HJYOKEN INV KEY=65"  UPON STAT
              MOVE      99        TO   GR-NO
              GO   TO   INIT-EXIT
     END-READ.
     MOVE     JYO-F14(1:2)   TO   WK-WKSTN.
     MOVE     JYO-F15(1:2)   TO   WK-DSOKO.
*条件ファイル読込み（在庫締日）
     MOVE     "99"           TO   JYO-F01.
     MOVE     "ZAI"          TO   JYO-F02.
     READ     HJYOKEN
              INVALID
              DISPLAY   "HJYOKEN INV KEY=99"  UPON STAT
              MOVE      99        TO   GR-NO
              GO   TO   INIT-EXIT
     END-READ.
     MOVE     JYO-F05        TO   WK-SIME.
     MOVE     JYO-F05        TO   WK-SIMEX.
     ADD      1              TO   WK-SIME.
     IF       WK-SIME42      >    12
              MOVE     1     TO   WK-SIME42
              ADD      1     TO   WK-SIME41
     END-IF.
*特販部名称編集
     MOVE    SPACE               TO        JYO-REC.
     INITIALIZE                            JYO-REC.
     MOVE    "99"                TO        JYO-F01.
     MOVE    "BUMON"             TO        JYO-F02.
     READ    HJYOKEN
       INVALID KEY
             MOVE NC"＊＊＊＊＊＊"   TO    HEN-TOKHAN
       NOT INVALID KEY
             MOVE JYO-F03            TO    HEN-TOKHAN
     END-READ.
     MOVE    HEN-TOKHAN-AREA      TO   TOKHAN.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD8.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
     MOVE      LINK-OUT-YMD8      TO   DATE-AREA.
*画面表示日付編集
     MOVE      DATE-AREA(1:4)     TO   HEN-DATE-YYYY.
     MOVE      DATE-AREA(5:2)     TO   HEN-DATE-MM.
     MOVE      DATE-AREA(7:2)     TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    SYS-TIME           FROM   TIME.
*画面表示時刻編集
     MOVE      SYS-TIME(1:2)      TO   HEN-TIME-HH.
     MOVE      SYS-TIME(3:2)      TO   HEN-TIME-MM.
     MOVE      SYS-TIME(5:2)      TO   HEN-TIME-SS.
*****REWRITE  JYO-REC.
*
     MOVE     0              TO   GR-NO.
 INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2          メイン処理
*--------------------------------------------------------------*
 MAIN-RTN           SECTION.
* 画面初期表示
     PERFORM     DSP-INIT-RTN     UNTIL     GR-NO  NOT  =  0.
* 処理区分入力
     PERFORM     DSP-KBN-RTN      UNTIL     GR-NO  NOT  =  1.
* 相殺区分・発注_入力
     PERFORM     DSP-SOSAI-RTN    UNTIL     GR-NO  NOT  =  2.
* ＨＥＡＤ入力
     PERFORM     DSP-HEAD1-RTN    UNTIL     GR-NO  NOT  =  3.
* ＢＯＤＹ入力
     PERFORM     DSP-BODY1-RTN    UNTIL     GR-NO  NOT  =  4.
* 確認入力
     PERFORM     DSP-KAKU-RTN     UNTIL     GR-NO  NOT  =  5.
* ＢＯＤＹクリア
     PERFORM     DSP-CLR-RTN      UNTIL     GR-NO  NOT  =  6.
 MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2          終了処理
*--------------------------------------------------------------*
 END-RTN            SECTION.
     CLOSE              HTOKMS
                        HTENMS
                        ZSHIMS
                        HJYOKEN
                        HMEIMS
                        ZSOKMS
                        HACHEDF
                        HACMEIF
                        NYKFILF
                        ZAMZAIF
                        HSHOTBL  HTANMS
                        DSPF.
*
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " END   "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STAT.
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   3         画面初期表示        (GR-NO = 0)         *
*--------------------------------------------------------------*
 DSP-INIT-RTN           SECTION.
*画面初期化
     MOVE     SPACE          TO   FNY00101.
     MOVE     SPACE          TO   DSP-CNTL.
*行番号セット
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL I  >  6
        MOVE  I              TO   GYO(I)
     END-PERFORM.
*画面定義セット
     MOVE     "FNY00101"     TO   DSP-FMT.
     MOVE     "ALLF"         TO   DSP-GRP.
     MOVE      ZERO          TO   WK-TAIHI.
*明細クリア
     PERFORM   WK-BODY-CLR.
*
     MOVE     1              TO   GR-NO.
 DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3          処理区分　入力　    (GR-NO = 1)　　　
*--------------------------------------------------------------*
 DSP-KBN-RTN           SECTION.
     PERFORM  CLR-HEAD-RTN.
*ＰＦガイドセット
     MOVE     G003           TO   PFGID.
*処理区分入力
     MOVE     "SYORI"        TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
     WHEN     PF05
              MOVE      99        TO   GR-NO
     WHEN     ENT
              PERFORM   CHK-KBN-RTN
              IF   ERR-FLG  =  ZERO
                 MOVE     2              TO   GR-NO
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              MOVE      MSG01     TO   MSG
     END-EVALUATE.
*
 DSP-KBN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      処理区分　チェック　       　　　　　
*--------------------------------------------------------------*
 CHK-KBN-RTN           SECTION.
     MOVE     ZERO      TO     ERR-FLG.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
*処理区分が数字以外の場合
     IF       KBN  IS  NOT  NUMERIC
              MOVE      0         TO   KBN
     END-IF.
*処理区分が１から３以外はエラー
     IF       KBN  NOT  =  1  AND  2  AND  3
              MOVE      2         TO   ERR-FLG
              MOVE     "R"        TO   EDIT-OPTION OF KBN
              MOVE      MSG02     TO   MSG
     ELSE
              EVALUATE  KBN
                  WHEN  1     MOVE   NC"登録"  TO  KBNNM
                              MOVE   1         TO  WK-SYORI
                  WHEN  2     MOVE   NC"修正"  TO  KBNNM
                              MOVE   2         TO  WK-SYORI
                  WHEN  3     MOVE   NC"削除"  TO  KBNNM
                              MOVE   3         TO  WK-SYORI
              END-EVALUATE
     END-IF.
 CHK-KBN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3          相殺区分　入力　    (GR-NO = 2)　　　
*--------------------------------------------------------------*
 DSP-SOSAI-RTN       SECTION.
     PERFORM CLR-BODY-RTN.
*ＰＦガイドセット
     MOVE     G001           TO   PFGID.
*相殺区分／発注番号／枝番号設定
     MOVE     "SOUSAI"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      ZERO      TO   PF-FLG
**************MOVE      6         TO   GR-NO
              MOVE      0         TO   GR-NO
     WHEN     PF05
              MOVE      99        TO   GR-NO
     WHEN     PF06
              MOVE      1         TO   GR-NO
     WHEN     PF11
*             登録時
              IF   KBN  =  1
                   MOVE     MSG19      TO   MSG
                   MOVE     1          TO   ERR-FLG
              ELSE
                   PERFORM   MAEREC-DSP
                   IF  ERR-FLG = ZERO
                       PERFORM   TOT-SEC
                   END-IF
              END-IF
     WHEN     PF12
*             登録時
              IF   KBN  =  1
                   MOVE     MSG19      TO   MSG
                   MOVE     1          TO   ERR-FLG
              ELSE
                   PERFORM   BACREC-DSP
                   IF  ERR-FLG = ZERO
                       PERFORM   TOT-SEC
                   END-IF
              END-IF
     WHEN     ENT
*             相殺区分／発注番号／枝番号入力チェック
              PERFORM   CHK-SOSAI-RTN
              IF   ERR-FLG  =  ZERO
                   IF   PF-FLG = ZERO
                        PERFORM   DSP-SEC
                        MOVE   ZERO   TO  WK-TAIHI
                        PERFORM   TOT-SEC
                   END-IF
                   MOVE   ZERO        TO   PF-FLG
                   IF  KBN = 3
                   OR  KBN = 1  AND  SOSAI = 1
                   OR  KBN = 1  AND  SOSAI = 3
                       MOVE  "Y"      TO   KAKUN
                       MOVE   5       TO   GR-NO
                   ELSE
                       MOVE   3       TO   GR-NO
                   END-IF
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              MOVE      MSG01     TO   MSG
     END-EVALUATE.
*
 DSP-SOSAI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4          前レコード表示             　　　　　
*--------------------------------------------------------------*
 MAEREC-DSP             SECTION.
     MOVE   ZERO        TO    ERR-FLG.
     PERFORM   CLR-HEAD-RTN.
*相殺区分チェック（相殺区分／発注番号／発注枝番号ＷＫ退避）
     IF     SOSAI NUMERIC   AND  HACYU NUMERIC AND HACYUE NUMERIC
            MOVE  SOSAI     TO   WK-SOSAI
            MOVE  HACYU     TO   WK-HACYU
            MOVE  HACYUE    TO   WK-HACYUE
     ELSE
*           ＷＫ相殺区分／ＷＫ発注／ＷＫ発注枝が０の場合
            IF     WK-SOSAI  =  ZERO  AND  WK-HACYU  =  ZERO  AND
                   WK-HACYUE  =  ZERO
                   MOVE     1         TO   ERR-FLG
                   MOVE     MSG16     TO   MSG
                   GO                 TO   MAEREC-DSP-EXIT
            END-IF
     END-IF.
*　　入庫ファイル読込み
     MOVE   WK-HACYU     TO     NYU-F02.
     MOVE   WK-HACYUE    TO     NYU-F03.
     MOVE   WK-SOSAI     TO     NYU-F04.
     MOVE   1            TO     NYU-F05.
     START  NYKFILF   KEY  IS  <  NYU-F02  NYU-F03
                                  NYU-F04  NYU-F05
                                  WITH  REVERSED
            INVALID   MOVE     MSG16       TO   MSG
                      MOVE     1           TO   ERR-FLG
                      GO                   TO   MAEREC-DSP-EXIT
     END-START.
 MAEREC-DSP-01.
     READ   NYKFILF   NEXT
            AT  END   MOVE    MSG16        TO   MSG
                      MOVE    1            TO   ERR-FLG
                      GO                   TO   MAEREC-DSP-EXIT
     END-READ.
*    計上済フラグ＝１、相殺区分が１又は２の時
     IF     NYU-F31   =  1    OR   NYU-F04  =  1   OR   2
            GO        TO      MAEREC-DSP-01
     END-IF.
*    実行本社以外で入庫ファイルの倉庫ＣＤと異なる場合
     IF     WK-DSOKO  NOT = "01" AND  WK-WKSTN NOT = NYU-F27
            GO        TO      MAEREC-DSP-01
     END-IF.
*    相殺区分＝０、伝票区分＝５０又は６０
     IF     NYU-F04   =  0    AND  NYU-F01  =  50  OR  60
            CONTINUE
     ELSE
            GO        TO      MAEREC-DSP-01
     END-IF.
*
     MOVE   1         TO      PF-FLG.
     PERFORM     WK-BODY-CLR.
     PERFORM     DSP-BODY-CLR.
     PERFORM     TBL-SET2.
 MAEREC-DSP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4          次レコード表示             　　　　　
*--------------------------------------------------------------*
 BACREC-DSP             SECTION.
     MOVE   ZERO               TO  ERR-FLG.
*ヘッダ項目クリア
     PERFORM   CLR-HEAD-RTN.
*
     IF     WK-SOSAI  =  ZERO  AND  WK-HACYU  =  ZERO  AND
            WK-HACYUE =  ZERO  AND  SOSAI  NOT NUMERIC  AND
            HACYU  NOT NUMERIC  AND  HACYUE  NOT NUMERIC
            MOVE     ZERO      TO   NYU-F02
            MOVE     ZERO      TO   NYU-F03
            MOVE     ZERO      TO   NYU-F04
            MOVE     ZERO      TO   NYU-F05
            START  NYKFILF   KEY  IS  >=  NYU-F02  NYU-F03
                                          NYU-F04  NYU-F05
                   INVALID   MOVE     MSG17       TO   MSG
                             MOVE     1           TO   ERR-FLG
                             GO               TO   BACREC-DSP-EXIT
            END-START
     ELSE
            IF  WK-SOSAI = ZERO AND WK-HACYU = ZERO AND
                WK-HACYUE = ZERO
                MOVE    HACYU   TO     WK-HACYU
                MOVE    HACYUE  TO     WK-HACYUE
                MOVE    SOSAI   TO     WK-SOSAI
            END-IF
            MOVE   WK-HACYU     TO     NYU-F02
            MOVE   WK-HACYUE    TO     NYU-F03
            MOVE   WK-SOSAI     TO     NYU-F04
            MOVE   6            TO     NYU-F05
            START  NYKFILF   KEY  IS  >  NYU-F02  NYU-F03
                                         NYU-F04  NYU-F05
                   INVALID   MOVE     MSG17       TO   MSG
                             MOVE     1           TO   ERR-FLG
                             GO              TO   BACREC-DSP-EXIT
            END-START
     END-IF.
 BACREC-DSP-01.
     READ  NYKFILF    NEXT
           AT  END    MOVE     MSG17       TO   MSG
                      MOVE     1           TO   ERR-FLG
                      GO                   TO   BACREC-DSP-EXIT
     END-READ.
*    計上済フラグ＝１、相殺区分が１又は２の時
     IF    NYU-F31  =  1    OR    NYU-F04  =  1   OR   2
           GO               TO    BACREC-DSP-01
     END-IF.
*    実行本社以外で入庫ファイルの倉庫ＣＤと異なる場合
     IF    WK-DSOKO  NOT = "01" AND  WK-WKSTN NOT = NYU-F27
           GO               TO    BACREC-DSP-01
     END-IF.
*    相殺区分＝０、伝票区分＝５０又は６０
     IF    NYU-F04  =  0    AND   NYU-F01  =  50  OR  60
           CONTINUE
     ELSE
           GO               TO    BACREC-DSP-01
     END-IF.
     MOVE        1          TO    PF-FLG.
     PERFORM     WK-BODY-CLR.
     PERFORM     DSP-BODY-CLR.
     PERFORM     TBL-SET2.
 BACREC-DSP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4          相殺区分　チェック　　　
*--------------------------------------------------------------*
 CHK-SOSAI-RTN       SECTION.
*属性クリア
     MOVE     ZERO                TO   ERR-FLG.
     PERFORM  CLR-HEAD-RTN.
*相殺区分チェック（数字以外）
     IF       SOSAI  IS  NOT  NUMERIC
              MOVE     "R"        TO   EDIT-OPTION OF SOSAI
              MOVE     "C"        TO   EDIT-CURSOR OF SOSAI
              MOVE      26        TO   ERR-FLG
              MOVE      MSG03     TO   MSG
     END-IF.
*発注番号チェック（数字以外）
     IF       HACYU  IS  NOT  NUMERIC
              MOVE     "R"        TO   EDIT-OPTION OF HACYU
              IF   ERR-FLG  = ZERO
                   MOVE  "C"      TO   EDIT-CURSOR OF HACYU
                   MOVE   MSG20   TO   MSG
              END-IF
              MOVE      26        TO   ERR-FLG
     END-IF.
*発注番号（枝）チェック（数字以外）
     IF       HACYUE IS  NOT  NUMERIC
              MOVE     "R"        TO   EDIT-OPTION OF HACYUE
              IF   ERR-FLG  = ZERO
                   MOVE  "C"      TO   EDIT-CURSOR OF HACYUE
                   MOVE   MSG20   TO   MSG
              END-IF
              MOVE      26        TO   ERR-FLG
     END-IF.
*相殺区分が４以上の場合
*****2006/07/10 相殺区分は０のみ入力を可能とする。
*****IF       SOSAI  > 4
     IF       SOSAI  > 0
              MOVE     "R"        TO   EDIT-OPTION OF SOSAI
              IF   ERR-FLG  = ZERO
                   MOVE  "C"      TO   EDIT-CURSOR OF SOSAI
*******************MOVE   MSG03   TO   MSG
                   MOVE   MSG31   TO   MSG
              END-IF
              MOVE      1         TO   ERR-FLG
     END-IF.
     IF    ERR-FLG NOT = ZERO
           GO        TO     CHK-SOSAI-EXIT
     END-IF.
*画面表示チェック
     EVALUATE   KBN
         WHEN   1       PERFORM  TOU-CHK
         WHEN   2       PERFORM  SYU-CHK
         WHEN   3       PERFORM  DEL-CHK
     END-EVALUATE.
*
 CHK-SOSAI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5          登録時チェック
*--------------------------------------------------------------*
 TOU-CHK            SECTION.
     EVALUATE   SOSAI
         WHEN   0
*            発注番号で発注（Ｈ）Ｆ索引
             MOVE  HACYU   TO    HAH-F02
             START HACHEDF  KEY IS >= HAH-F02
                   INVALID
                   MOVE   MSG08    TO   MSG
                   MOVE   "R"      TO   EDIT-OPTION OF SOSAI
                                        EDIT-OPTION OF HACYU
                                        EDIT-OPTION OF HACYUE
                   MOVE    1       TO   ERR-FLG
*************DISPLAY "A1A" UPON STAT
                           GO      TO   TOU-CHK-EXIT
             END-START
*************DISPLAY "B1B" UPON STAT
             PERFORM   HAH-READ
*            発注明細Ｆ読込み
             IF    ERR-FLG  =  ZERO
*発注（Ｍ）Ｆ読込み（明細の存在チェック）
                   MOVE    HAH-F02    TO   HAM-F02
                   MOVE    ZERO       TO   HAM-F03
                   START   HACMEIF  KEY  IS  >=  HAM-F02 HAM-F03
                      INVALID
                      MOVE    MSG25  TO     MSG
                      MOVE    "R"    TO     EDIT-OPTION OF SOSAI
                                            EDIT-OPTION OF HACYU
                                            EDIT-OPTION OF HACYUE
                      MOVE    1      TO     ERR-FLG
*************DISPLAY "A2A" UPON STAT
                      NOT INVALID
                      PERFORM  HAM-READ
                   END-START
             END-IF
*************DISPLAY "B2B" UPON STAT
         WHEN   1   WHEN   2
*            相殺時処理（登録モードで処理）
             MOVE  HACYU   TO    NYU-F02
             MOVE  HACYUE  TO    NYU-F03
             MOVE  ZERO    TO    NYU-F04
             MOVE  1       TO    NYU-F05
             START   NYKFILF   KEY IS >= NYU-F02  NYU-F03
                                         NYU-F04  NYU-F05
                     INVALID KEY
                       MOVE   MSG08  TO   MSG
***********************DISPLAY "AAAAAAA"  UPON CONS
                       MOVE   "R"    TO   EDIT-OPTION OF SOSAI
                                          EDIT-OPTION OF HACYU
                                          EDIT-OPTION OF HACYUE
                       MOVE    1     TO   ERR-FLG
                       GO            TO   TOU-CHK-EXIT
             END-START
*
             MOVE      ZERO          TO   SONZAI-F   SONZAI-F2
             IF  SOSAI  = 1
                 PERFORM   NYU-READ
             ELSE
                 PERFORM   NYU-READ5
             END-IF
         WHEN   3   WHEN   4
*            赤／黒相殺時処理
             MOVE  HACYU   TO    NYU-F02
             MOVE  HACYUE  TO    NYU-F03
             MOVE  2       TO    NYU-F04
             MOVE  1       TO    NYU-F05
             START   NYKFILF   KEY IS >= NYU-F02  NYU-F03
                                         NYU-F04  NYU-F05
                     INVALID KEY
                       MOVE   MSG08  TO   MSG
                       MOVE   "R"    TO   EDIT-OPTION OF SOSAI
                                          EDIT-OPTION OF HACYU
                                          EDIT-OPTION OF HACYUE
                       MOVE    1     TO   ERR-FLG
                       GO            TO   TOU-CHK-EXIT
             END-START
*
             MOVE      ZERO          TO   SONZAI-F3  SONZAI-F4
             IF  SOSAI  = 3
                 PERFORM   NYU-READ8
             ELSE
                 PERFORM   NYU-READ9
             END-IF
     END-EVALUATE.
 TOU-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          発注ファイルＲＥＡＤ
*--------------------------------------------------------------*
 HAH-READ            SECTION.
*発注ファイル読込み
     PERFORM  HACHEDF-READ-SEC.
*****DISPLAY "AAA-01" UPON STAT.
*
     IF     HAH-READ-FLG  =  "END"
            MOVE   MSG08   TO   MSG
********DISPLAY "BBB" UPON CONS
            MOVE   "R"     TO   EDIT-OPTION  OF SOSAI
                                EDIT-OPTION  OF HACYU
                                EDIT-OPTION  OF HACYUE
            MOVE    1      TO   ERR-FLG
            GO             TO   HAH-READ-EXIT
     END-IF.
*****DISPLAY "AAA-02" UPON STAT.
*発注番号が入力番号以上の場合
     IF     HAH-F02  >  HACYU
            MOVE    MSG08  TO     MSG
            MOVE    1      TO     ERR-FLG
            MOVE    "R"    TO     EDIT-OPTION OF SOSAI
                                  EDIT-OPTION OF HACYU
                                  EDIT-OPTION OF HACYUE
            GO             TO     HAH-READ-EXIT
     END-IF.
*****DISPLAY "AAA-03" UPON STAT.
*取消区分＝１の場合
     IF     HAH-F24  = 1
*****DISPLAY "AAA-031" UPON STAT
            GO             TO     HAH-READ
     ELSE
*           発注フラグ＝０、発注書出力フラグ＝０の場合
            IF   HAH-F25  =  ZERO  OR  HAH-F97  =  ZERO
                 GO        TO     HAH-READ
            END-IF
     END-IF.
*****DISPLAY "AAA-04" UPON STAT.
*実行倉庫により処理判定
     IF     WK-DSOKO = "01"
*           発注番号、発注番号枝、相殺区分
************DISPLAY "AAA-05" UPON STAT
            IF  HAH-F02 = HACYU  AND  HAH-F03 = HACYUE
*               完了区分＝０、伝票区分＝５０、６０の時
                IF  (HAH-F04 = ZERO  AND  HAH-F01 = 50)  OR
                    (HAH-F04 = ZERO  AND  HAH-F01 = 60)
                    CONTINUE
                ELSE
                    GO            TO  HAH-READ
                END-IF
            ELSE
************DISPLAY "AAA-06" UPON STAT
                   MOVE    MSG08  TO     MSG
********DISPLAY "DDD" UPON CONS
                   MOVE    1      TO     ERR-FLG
                   MOVE    "R"    TO     EDIT-OPTION OF SOSAI
                                         EDIT-OPTION OF HACYU
                                         EDIT-OPTION OF HACYUE
                   GO             TO     HAH-READ-EXIT
            END-IF
     ELSE
************DISPLAY "AAA-07" UPON STAT
*           発注番号、発注番号枝、相殺区分
            IF     HAH-F02 = HACYU  AND  HAH-F03 = HACYUE AND
                   HAH-F17 = WK-WKSTN
*                  完了区分＝０、伝票区分＝５０、６０の時
                   IF ( HAH-F04 = 0  AND  HAH-F01 = 50 ) OR
                      ( HAH-F04 = 0  AND  HAH-F01 = 60 )
                       CONTINUE
                   ELSE
                       GO         TO     HAH-READ
                   END-IF
            ELSE
************DISPLAY "AAA-08" UPON STAT
                   MOVE    MSG08  TO     MSG
                   MOVE    1      TO     ERR-FLG
                   MOVE    "R"    TO     EDIT-OPTION OF SOSAI
                                         EDIT-OPTION OF HACYU
                                         EDIT-OPTION OF HACYUE
                   GO             TO     HAH-READ-EXIT
            END-IF
     END-IF.
 HAH-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          発注明細ファイル読込み
*--------------------------------------------------------------*
 HAM-READ            SECTION.
*発注ファイル読込み
     PERFORM    HACMEIF-READ-SEC.
************DISPLAY "BBB-01" UPON STAT.
*終了チェック
     IF     HAM-READ-FLG = "END"
            MOVE    MSG25  TO     MSG
            MOVE    1      TO     ERR-FLG
            MOVE    "R"    TO     EDIT-OPTION OF SOSAI
                                  EDIT-OPTION OF HACYU
                                  EDIT-OPTION OF HACYUE
            GO             TO     HAM-READ-EXIT
     END-IF.
************DISPLAY "BBB-02" UPON STAT.
*完了区分＝１の場合は再読込みへ
     IF     HAM-F05  =  1  OR  9
            GO             TO   HAM-READ
     END-IF.
************DISPLAY "BBB-03" UPON STAT.
*発注番号が入力番号以上の場合
     IF     HAM-F02  >  HACYU
************DISPLAY "BBB-04" UPON STAT
            MOVE    MSG25  TO     MSG
            MOVE    1      TO     ERR-FLG
            MOVE    "R"    TO     EDIT-OPTION OF SOSAI
                                  EDIT-OPTION OF HACYU
                                  EDIT-OPTION OF HACYUE
            GO             TO     HAM-READ-EXIT
     ELSE
            MOVE  HAM-F03  TO     WK-GYO
     END-IF.
 HAM-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ALL               発注ヘッダファイル読込み（順読み）
*--------------------------------------------------------------*
 HACHEDF-READ-SEC    SECTION.
*
     MOVE   SPACE          TO   HAH-READ-FLG.
     READ   HACHEDF  NEXT
            AT   END
            MOVE    "END"  TO   HAH-READ-FLG
     END-READ.
*
 HACHEDF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ALL               発注明細ファイル読込み（順読み）
*--------------------------------------------------------------*
 HACMEIF-READ-SEC    SECTION.
*
     MOVE   SPACE          TO   HAM-READ-FLG.
     READ   HACMEIF  NEXT
            AT   END
            MOVE    "END"  TO   HAM-READ-FLG
     END-READ.
*
 HACMEIF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイル読込み（順読み）
*--------------------------------------------------------------*
 NYKFILF-READ-SEC    SECTION.
*
     MOVE   SPACE          TO   NYK-READ-FLG.
     READ   NYKFILF  NEXT
            AT   END
            MOVE    "END"  TO   NYK-READ-FLG
     END-READ.
*
 NYKFILF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイルＲＥＡＤ（登録）相殺　１
*--------------------------------------------------------------*
 NYU-READ            SECTION.
*入庫ファイル読込み
     PERFORM  NYKFILF-READ-SEC.
*ファイル終了時
     IF       NYK-READ-FLG  =  "END"
              IF    SONZAI-F = 1
                    GO     TO  NYU-READ-01
              ELSE
                    MOVE  MSG08  TO  MSG
                    MOVE  "R"    TO  EDIT-OPTION OF SOSAI
                                     EDIT-OPTION OF HACYU
                                     EDIT-OPTION OF HACYUE
                    MOVE   1     TO  ERR-FLG
                    GO           TO  NYU-READ-EXIT
              END-IF
     END-IF.
*本社にて実行時
     IF     WK-DSOKO = "01"
*           発注番号、発注番号枝、相殺、計上済フラグ
            IF     NYU-F02 = HACYU  AND  NYU-F03 = HACYUE AND
                   NYU-F04 = ZERO   AND  NYU-F31 = 1
                   IF  NYU-F26(1:6) NOT > WK-SIME6X
                       MOVE   MSG26 TO   MSG
                       MOVE   1     TO   ERR-FLG
                       MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                         EDIT-OPTION OF HACYU
                                         EDIT-OPTION OF HACYUE
                   ELSE
                       MOVE   1     TO   SONZAI-F
                       MOVE   NYU-F01 TO WK-DENKU
                       GO           TO   NYU-READ
                   END-IF
            ELSE
*                  発注番号、発注番号枝、相殺
                   IF  NYU-F02 = HACYU AND NYU-F03 = HACYUE AND
                       NYU-F04 = 1
                       MOVE  1      TO   ERR-FLG
                       MOVE  MSG27  TO   MSG
                       MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                         EDIT-OPTION OF HACYU
                                         EDIT-OPTION OF HACYUE
                       GO           TO   NYU-READ-EXIT
                   END-IF
                   IF  SONZAI-F = 1
                       CONTINUE
                   ELSE
                       MOVE   1     TO  ERR-FLG
                       MOVE   MSG08 TO   MSG
                       MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                         EDIT-OPTION OF HACYU
                                         EDIT-OPTION OF HACYUE
                   END-IF
            END-IF
     ELSE
*           発注番号、発注番号枝、相殺区分、倉庫、計上済フラグ
            IF     NYU-F02 = HACYU  AND  NYU-F03 = HACYUE AND
                   NYU-F04 = ZERO  AND  NYU-F27 = WK-WKSTN  AND
                   NYU-F31 = 1
                   MOVE      1     TO   SONZAI-F
                   MOVE    NYU-F01 TO   WK-DENKU
                   GO              TO   NYU-READ
            ELSE
*                  発注番号、発注番号枝、相殺
                   IF  NYU-F02 = HACYU AND NYU-F03 = HACYUE AND
                       NYU-F04 = 1
                       MOVE  MSG27  TO   MSG
                       MOVE  1      TO   ERR-FLG
                       MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                         EDIT-OPTION OF HACYU
                                         EDIT-OPTION OF HACYUE
                       GO           TO   NYU-READ-EXIT
                   END-IF
                   IF  SONZAI-F = 1
                       CONTINUE
                   ELSE
                       MOVE   1     TO   ERR-FLG
                       MOVE   MSG08 TO   MSG
                       MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                         EDIT-OPTION OF HACYU
                                         EDIT-OPTION OF HACYUE
                   END-IF
            END-IF
     END-IF.
*チェック後、同一キーにて再度入庫ファイル読込み
 NYU-READ-01.
     MOVE   HACYU      TO   NYU-F02.
     MOVE   HACYUE     TO   NYU-F03.
     MOVE   ZERO       TO   NYU-F04.
     MOVE   1          TO   NYU-F05.
     START   NYKFILF   KEY IS >= NYU-F02  NYU-F03
                                 NYU-F04  NYU-F05
             INVALID  DISPLAY  "*E  NYKFILF  READ INVALID  "
                  "NYU-F01 = " NYU-F01 " NYU-F02 = " NYU-F02
                 " NYU-F03 = " NYU-F03 " NYU-F04 = " NYU-F04
                   UPON  STAT
                   STOP  RUN
     END-START.
     PERFORM  NYKFILF-READ-SEC.
     IF     NYK-READ-FLG  =  "END"
            DISPLAY  "*E  NYKFILF  READ INVALID  "
                     "NYU-F01 = " NYU-F01 " NYU-F02 = " NYU-F02
                     " NYU-F03 = " NYU-F03 " NYU-F04 = " NYU-F04
                       UPON  STAT
                       STOP  RUN
     END-IF.
 NYU-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイルＲＥＡＤ（登録）相殺　１
*--------------------------------------------------------------*
 NYU-READ8           SECTION.
*入庫ファイル読込み
     PERFORM  NYKFILF-READ-SEC.
*ファイル終了時
     IF       NYK-READ-FLG  =  "END"
              IF    SONZAI-F3 = 1
                    GO     TO  NYU-READ8-01
              ELSE
                    MOVE  MSG08  TO  MSG
                    MOVE  "R"    TO  EDIT-OPTION OF SOSAI
                                     EDIT-OPTION OF HACYU
                                     EDIT-OPTION OF HACYUE
                    MOVE   1     TO  ERR-FLG
                    GO           TO  NYU-READ8-EXIT
              END-IF
     END-IF.
*本社にて実行時
     IF     WK-DSOKO = "01"
*           発注番号、発注番号枝、相殺、計上済フラグ
            IF     NYU-F02 = HACYU  AND  NYU-F03 = HACYUE AND
                   NYU-F04 = 2      AND  NYU-F31 = 1
                   IF  NYU-F26(1:6) NOT > WK-SIME6X
                       MOVE   MSG26 TO   MSG
                       MOVE   1     TO   ERR-FLG
                       MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                         EDIT-OPTION OF HACYU
                                         EDIT-OPTION OF HACYUE
                   ELSE
                       MOVE   1     TO   SONZAI-F3
                       MOVE   NYU-F01 TO WK-DENKU
                       GO           TO   NYU-READ8
                   END-IF
            ELSE
*                  発注番号、発注番号枝、相殺
                   IF  NYU-F02 = HACYU AND NYU-F03 = HACYUE AND
                       NYU-F04 = 3
                       MOVE  1      TO   ERR-FLG
                       MOVE  MSG27  TO   MSG
                       MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                         EDIT-OPTION OF HACYU
                                         EDIT-OPTION OF HACYUE
                       GO           TO   NYU-READ8-EXIT
                   END-IF
                   IF  SONZAI-F3 = 1
                       CONTINUE
                   ELSE
                       MOVE   1     TO  ERR-FLG
                       MOVE   MSG08 TO   MSG
                       MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                         EDIT-OPTION OF HACYU
                                         EDIT-OPTION OF HACYUE
                   END-IF
            END-IF
     ELSE
*           発注番号、発注番号枝、相殺区分、倉庫、計上済フラグ
            IF     NYU-F02 = HACYU  AND  NYU-F03 = HACYUE AND
                   NYU-F04 = 2     AND  NYU-F27 = WK-WKSTN  AND
                   NYU-F31 = 1
                   MOVE      1     TO   SONZAI-F3
                   MOVE    NYU-F01 TO   WK-DENKU
                   GO              TO   NYU-READ8
            ELSE
*                  発注番号、発注番号枝、相殺
                   IF  NYU-F02 = HACYU AND NYU-F03 = HACYUE AND
                       NYU-F04 = 3
                       MOVE  MSG27  TO   MSG
                       MOVE  1      TO   ERR-FLG
                       MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                         EDIT-OPTION OF HACYU
                                         EDIT-OPTION OF HACYUE
                       GO           TO   NYU-READ8-EXIT
                   END-IF
                   IF  SONZAI-F3 = 1
                       CONTINUE
                   ELSE
                       MOVE   1     TO   ERR-FLG
                       MOVE   MSG08 TO   MSG
                       MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                         EDIT-OPTION OF HACYU
                                         EDIT-OPTION OF HACYUE
                   END-IF
            END-IF
     END-IF.
*チェック後、同一キーにて再度入庫ファイル読込み
 NYU-READ8-01.
     MOVE   HACYU      TO   NYU-F02.
     MOVE   HACYUE     TO   NYU-F03.
     MOVE   2          TO   NYU-F04.
     MOVE   1          TO   NYU-F05.
     START   NYKFILF   KEY IS >= NYU-F02  NYU-F03
                                 NYU-F04  NYU-F05
             INVALID  DISPLAY  "*E  NYKFILF  READ INVALID  "
                  "NYU-F01 = " NYU-F01 " NYU-F02 = " NYU-F02
                 " NYU-F03 = " NYU-F03 " NYU-F04 = " NYU-F04
                   UPON  STAT
                   STOP  RUN
     END-START.
     PERFORM  NYKFILF-READ-SEC.
     IF     NYK-READ-FLG  =  "END"
            DISPLAY  "*E  NYKFILF  READ INVALID  "
                     "NYU-F01 = " NYU-F01 " NYU-F02 = " NYU-F02
                     " NYU-F03 = " NYU-F03 " NYU-F04 = " NYU-F04
                       UPON  STAT
                       STOP  RUN
     END-IF.
 NYU-READ8-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイルＲＥＡＤ（登録）相殺　２
*--------------------------------------------------------------*
 NYU-READ5           SECTION.
*入庫ファイル読込み
     PERFORM   NYKFILF-READ-SEC.
*終了チェック
     IF     NYK-READ-FLG = "END"
            IF    SONZAI-F = 1 AND  SONZAI-F2 = 1
                  GO     TO  NYU-READ5-01
            ELSE
                  MOVE  MSG08  TO  MSG
                  MOVE  "R"    TO  EDIT-OPTION OF SOSAI
                                   EDIT-OPTION OF HACYU
                                   EDIT-OPTION OF HACYUE
                  MOVE   1     TO  ERR-FLG
                  GO           TO  NYU-READ5-EXIT
            END-IF
     END-IF.
*本社の場合
     IF     WK-DSOKO  =  "01"
*           発注番号、発注番号枝、相殺、発注残、計上済フラグ
            IF     NYU-F02 = HACYU  AND  NYU-F03 = HACYUE AND
                   NYU-F04 = ZERO   AND  NYU-F31 = 1
                   MOVE   1        TO   SONZAI-F
                   MOVE   NYU-F01  TO   WK-DENKU
                   GO              TO   NYU-READ5
            ELSE
*                  発注番号、発注番号枝、相殺
                   IF  NYU-F02 = HACYU AND NYU-F03 = HACYUE AND
                       NYU-F04 = 1
                       MOVE  1      TO   SONZAI-F2
                       GO           TO   NYU-READ5
                   ELSE
*                     発注番号、発注番号枝、相殺
*                     IF  NYU-F02 = HACYU AND NYU-F03 = HACYUE AND
*                         NYU-F04 = 2
*                         MOVE  MSG08  TO   MSG
*                         MOVE  1      TO   ERR-FLG
*                         MOVE  "R"    TO   EDIT-OPTION OF SOSAI
*                                           EDIT-OPTION OF HACYU
*                                           EDIT-OPTION OF HACYUE
*                         GO           TO   NYU-READ5-EXIT
*                     END-IF
                      IF  SONZAI-F = 1 AND  SONZAI-F2 = 1
                          CONTINUE
                      ELSE
                          MOVE   MSG08 TO   MSG
                          MOVE   1     TO   ERR-FLG
                          MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                            EDIT-OPTION OF HACYU
                                           EDIT-OPTION OF HACYUE
                      END-IF
                   END-IF
            END-IF
     ELSE
*         発注番号、発注番号枝、相殺、発注残、倉庫、計上済フラグ
            IF     NYU-F02 = HACYU  AND  NYU-F03 = HACYUE AND
                   NYU-F04 = ZERO   AND  NYU-F31 = 1  AND
                   NYU-F27 = WK-WKSTN
                   MOVE   1        TO   SONZAI-F
                   MOVE   NYU-F01  TO   WK-DENKU
                   GO              TO   NYU-READ5
            ELSE
*                  発注番号、発注番号枝、相殺
                   IF  NYU-F02 = HACYU AND NYU-F03 = HACYUE AND
                       NYU-F04 = 1
                       MOVE  1      TO   SONZAI-F2
                       GO           TO   NYU-READ5
                   ELSE
*                    発注番号、発注番号枝、相殺
                     IF  NYU-F02 = HACYU AND NYU-F03 = HACYUE AND
                           NYU-F04 = 2
                           MOVE  MSG08  TO   MSG
                           MOVE  1      TO   ERR-FLG
                           MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                             EDIT-OPTION OF HACYU
                                            EDIT-OPTION OF HACYUE
                           GO           TO   NYU-READ5-EXIT
                     END-IF
                       IF  SONZAI-F = 1 AND  SONZAI-F2 = 1
                           CONTINUE
                       ELSE
                           MOVE   MSG08 TO   MSG
                           MOVE   1     TO   ERR-FLG
                           MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                             EDIT-OPTION OF HACYU
                                            EDIT-OPTION OF HACYUE
                       END-IF
                   END-IF
            END-IF
     END-IF.
 NYU-READ5-01.
*チェック後、同一キーにて再度入庫ファイル読込み
     MOVE   HACYU      TO   NYU-F02.
     MOVE   HACYUE     TO   NYU-F03.
     MOVE   1          TO   NYU-F04.
     MOVE   1          TO   NYU-F05.
     START   NYKFILF   KEY IS >= NYU-F02  NYU-F03
                                 NYU-F04  NYU-F05
             INVALID  DISPLAY  "*E  NYKFILF  READ INVALID  "
                  "NYU-F01 = " NYU-F01 " NYU-F02 = " NYU-F02
                 " NYU-F03 = " NYU-F03 " NYU-F04 = " NYU-F04
                   UPON  STAT
                   STOP  RUN
     END-START.
*入庫ファイル読込み
     PERFORM  NYKFILF-READ-SEC.
     IF   NYK-READ-FLG = "END"
          DISPLAY  "*E  NYKFILF  READ INVALID  "
                   "NYU-F01 = " NYU-F01 " NYU-F02 = " NYU-F02
                   " NYU-F03 = " NYU-F03 " NYU-F04 = " NYU-F04
                    UPON  STAT
                    STOP  RUN
     END-IF.
 NYU-READ5-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイルＲＥＡＤ（登録）相殺　２
*--------------------------------------------------------------*
 NYU-READ9           SECTION.
*入庫ファイル読込み
     PERFORM   NYKFILF-READ-SEC.
*終了チェック
     IF     NYK-READ-FLG = "END"
            IF    SONZAI-F3 = 1 AND  SONZAI-F4 = 1
                  GO     TO  NYU-READ9-01
            ELSE
                  MOVE  MSG08  TO  MSG
                  MOVE  "R"    TO  EDIT-OPTION OF SOSAI
                                   EDIT-OPTION OF HACYU
                                   EDIT-OPTION OF HACYUE
                  MOVE   1     TO  ERR-FLG
                  GO           TO  NYU-READ9-EXIT
            END-IF
     END-IF.
*本社の場合
     IF     WK-DSOKO  =  "01"
*           発注番号、発注番号枝、相殺、発注残、計上済フラグ
            IF     NYU-F02 = HACYU  AND  NYU-F03 = HACYUE AND
                   NYU-F04 = 2      AND  NYU-F31 = 1
                   MOVE   1        TO   SONZAI-F3
                   MOVE   NYU-F01  TO   WK-DENKU
                   GO              TO   NYU-READ9
            ELSE
*                  発注番号、発注番号枝、相殺
                   IF  NYU-F02 = HACYU AND NYU-F03 = HACYUE AND
                       NYU-F04 = 3
                       MOVE  1      TO   SONZAI-F4
                       GO           TO   NYU-READ9
                   ELSE
*                     発注番号、発注番号枝、相殺
                      IF  NYU-F02 = HACYU AND NYU-F03 = HACYUE AND
                          NYU-F04 = 4
                          MOVE  MSG08  TO   MSG
                          MOVE  1      TO   ERR-FLG
                          MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                            EDIT-OPTION OF HACYU
                                            EDIT-OPTION OF HACYUE
                          GO           TO   NYU-READ9-EXIT
                      END-IF
                      IF  SONZAI-F3 = 1 AND  SONZAI-F4 = 1
                          CONTINUE
                      ELSE
                          MOVE   MSG08 TO   MSG
                          MOVE   1     TO   ERR-FLG
                          MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                            EDIT-OPTION OF HACYU
                                            EDIT-OPTION OF HACYUE
                      END-IF
                   END-IF
            END-IF
     ELSE
*         発注番号、発注番号枝、相殺、発注残、倉庫、計上済フラグ
            IF     NYU-F02 = HACYU  AND  NYU-F03 = HACYUE AND
                   NYU-F04 = 2      AND  NYU-F31 = 1  AND
                   NYU-F27 = WK-WKSTN
                   MOVE   1        TO   SONZAI-F3
                   MOVE   NYU-F01  TO   WK-DENKU
                   GO              TO   NYU-READ9
            ELSE
*                  発注番号、発注番号枝、相殺
                   IF  NYU-F02 = HACYU AND NYU-F03 = HACYUE AND
                       NYU-F04 = 3
                       MOVE  1      TO   SONZAI-F4
                       GO           TO   NYU-READ9
                   ELSE
*                    発注番号、発注番号枝、相殺
                     IF  NYU-F02 = HACYU AND NYU-F03 = HACYUE AND
                           NYU-F04 = 2
                           MOVE  MSG08  TO   MSG
                           MOVE  1      TO   ERR-FLG
                           MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                             EDIT-OPTION OF HACYU
                                             EDIT-OPTION OF HACYUE
                           GO           TO   NYU-READ9-EXIT
                     END-IF
                       IF  SONZAI-F3 = 1 AND  SONZAI-F4 = 1
                           CONTINUE
                       ELSE
                           MOVE   MSG08 TO   MSG
                           MOVE   1     TO   ERR-FLG
                           MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                             EDIT-OPTION OF HACYU
                                             EDIT-OPTION OF HACYUE
                       END-IF
                   END-IF
            END-IF
     END-IF.
 NYU-READ9-01.
*チェック後、同一キーにて再度入庫ファイル読込み
     MOVE   HACYU      TO   NYU-F02.
     MOVE   HACYUE     TO   NYU-F03.
     MOVE   3          TO   NYU-F04.
     MOVE   1          TO   NYU-F05.
     START   NYKFILF   KEY IS >= NYU-F02  NYU-F03
                                 NYU-F04  NYU-F05
             INVALID  DISPLAY  "*E  NYKFILF  READ INVALID  "
                  "NYU-F01 = " NYU-F01 " NYU-F02 = " NYU-F02
                 " NYU-F03 = " NYU-F03 " NYU-F04 = " NYU-F04
                   UPON  STAT
                   STOP  RUN
     END-START.
*入庫ファイル読込み
     PERFORM  NYKFILF-READ-SEC.
     IF   NYK-READ-FLG = "END"
          DISPLAY  "*E  NYKFILF  READ INVALID  "
                   "NYU-F01 = " NYU-F01 " NYU-F02 = " NYU-F02
                   " NYU-F03 = " NYU-F03 " NYU-F04 = " NYU-F04
                    UPON  STAT
                    STOP  RUN
     END-IF.
 NYU-READ9-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5          修正時チェック
*--------------------------------------------------------------*
 SYU-CHK            SECTION.
     EVALUATE   SOSAI
         WHEN   0
             MOVE   HACYU   TO    NYU-F02
             MOVE   HACYUE  TO    NYU-F03
             MOVE   SOSAI   TO    NYU-F04
             MOVE   1       TO    NYU-F05
             START   NYKFILF   KEY IS >= NYU-F02  NYU-F03
                                         NYU-F04  NYU-F05
                     INVALID   MOVE   MSG08  TO   MSG
                               MOVE   "R"    TO
                                             EDIT-OPTION OF SOSAI
                                             EDIT-OPTION OF HACYU
                                            EDIT-OPTION OF HACYUE
                               MOVE   1      TO   ERR-FLG
                               GO            TO   SYU-CHK-EXIT
             END-START
             PERFORM   NYU-READ2
         WHEN   2
             MOVE   HACYU   TO    NYU-F02
             MOVE   HACYUE  TO    NYU-F03
             MOVE   SOSAI   TO    NYU-F04
             MOVE   1       TO    NYU-F05
             START   NYKFILF   KEY IS >= NYU-F02  NYU-F03
                                         NYU-F04  NYU-F05
                     INVALID   MOVE   MSG08  TO   MSG
                               MOVE   "R"    TO
                                             EDIT-OPTION OF SOSAI
                                             EDIT-OPTION OF HACYU
                                             EDIT-OPTION OF HACYUE
                               MOVE   1      TO   ERR-FLG
                               GO            TO   SYU-CHK-EXIT
             END-START
             PERFORM   NYU-READ3
         WHEN   4
             MOVE   HACYU   TO    NYU-F02
             MOVE   HACYUE  TO    NYU-F03
             MOVE   SOSAI   TO    NYU-F04
             MOVE   1       TO    NYU-F05
             START   NYKFILF   KEY IS >= NYU-F02  NYU-F03
                                         NYU-F04  NYU-F05
                     INVALID   MOVE   MSG08  TO   MSG
                               MOVE   "R"    TO
                                             EDIT-OPTION OF SOSAI
                                             EDIT-OPTION OF HACYU
                                             EDIT-OPTION OF HACYUE
                               MOVE   1      TO   ERR-FLG
                               GO            TO   SYU-CHK-EXIT
             END-START
             PERFORM   NYU-READ10
         WHEN   1   WHEN   3
             MOVE    1     TO   ERR-FLG
             MOVE    MSG08 TO   MSG
             MOVE    "R"   TO   EDIT-OPTION OF SOSAI
                                EDIT-OPTION OF HACYU
                                EDIT-OPTION OF HACYUE
     END-EVALUATE.
 SYU-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイルＲＥＡＤ（修正）相殺　０
*--------------------------------------------------------------*
 NYU-READ2           SECTION.
*入庫ファイル読込み
     PERFORM NYKFILF-READ-SEC.
*終了チェック
     IF  NYK-READ-FLG  =  "END"
         MOVE  MSG08  TO  MSG
         MOVE  "R"    TO  EDIT-OPTION OF SOSAI
                          EDIT-OPTION OF HACYU
                          EDIT-OPTION OF HACYUE
         MOVE   1     TO  ERR-FLG
         GO           TO  NYU-READ2-EXIT
     END-IF.
*本社の場合
     IF     WK-DSOKO  =  "01"
*           発注番号、発注番号枝、相殺
            IF   NYU-F02 = HACYU  AND  NYU-F03 = HACYUE  AND
                 NYU-F04 = SOSAI
*                計上済フラグ＝０、伝票区分＝５０、６０
*$$2014/05/09 NAV ST ↓条件変更
*$$              IF  (( NYU-F31 = 0 ) AND ( NYU-F01 = 50 OR 60))
                 IF  (( NYU-F31 = 0 )
                 AND  ( NYU-F01 = 50 OR 60 OR 51 OR 61))
                     CONTINUE
*$$2014/05/09 NAV ED ↑条件変更
                 ELSE
*$$2014/05/09 NAV ST ↓条件変更
*$$                  IF NYU-F31 = 1 AND NYU-F01 = 50 OR 60
                     IF NYU-F31 = 1
*$$2014/05/09 NAV ED ↑条件変更
                        MOVE  1      TO   ERR-FLG
                        MOVE  MSG32  TO   MSG
                        MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                          EDIT-OPTION OF HACYU
                                          EDIT-OPTION OF HACYUE
                        GO           TO   NYU-READ2-EXIT
*$$2014/05/09 NAV ST ↓停止
*$$                  ELSE
*$$                     GO        TO    NYU-READ2
*$$2014/05/09 NAV ED ↑停止
                     END-IF
                 END-IF
            ELSE
                 MOVE  1      TO   ERR-FLG
                 MOVE  MSG08  TO   MSG
                 MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                   EDIT-OPTION OF HACYU
                                   EDIT-OPTION OF HACYUE
                 GO           TO   NYU-READ2-EXIT
            END-IF
     ELSE
*           発注番号、発注番号枝、相殺
            IF   NYU-F02 = HACYU  AND  NYU-F03 = HACYUE  AND
                 NYU-F04 = SOSAI
*                計上済フラグ＝０、伝票区分＝５０、６０、倉庫
*$$2014/05/09 NAV ST ↓条件変更
*$$              IF  NYU-F31 = ZERO  AND NYU-F01 = 50 AND
*$$                  NYU-F27 = WK-WKSTN  OR
*$$                  NYU-F31 = ZERO  AND NYU-F01 = 60 AND
*$$                  NYU-F27 = WK-WKSTN
                 IF  ((NYU-F31 = ZERO)
                 AND  (NYU-F01 = 50  OR  51  OR  60  OR  61)
                 AND  ( NYU-F27 = WK-WKSTN))
*$$2014/05/09 NAV ED ↑条件変更
                     CONTINUE
                 ELSE
*$$2014/05/09 NAV ST ↓条件変更
*$$                  IF NYU-F31 = 1 AND NYU-F01 = 50 OR 60
                     IF NYU-F31 = 1
*$$2014/05/09 NAV ED ↑条件変更
                        MOVE  1      TO   ERR-FLG
                        MOVE  MSG32  TO   MSG
                        MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                          EDIT-OPTION OF HACYU
                                          EDIT-OPTION OF HACYUE
                        GO           TO   NYU-READ2-EXIT
*$$2014/05/09 NAV ST ↓停止
*$$                  ELSE
*$$                     GO        TO    NYU-READ2
*$$2014/05/09 NAV ED ↑停止
                     END-IF
                 END-IF
            ELSE
                 MOVE    1      TO   ERR-FLG
                 MOVE    MSG08  TO   MSG
                 MOVE    "R"    TO   EDIT-OPTION OF SOSAI
                                     EDIT-OPTION OF HACYU
                                     EDIT-OPTION OF HACYUE
             END-IF
     END-IF.
 NYU-READ2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイルＲＥＡＤ（修正）相殺　２
*--------------------------------------------------------------*
 NYU-READ3           SECTION.
*入庫ファイル読込み
     PERFORM  NYKFILF-READ-SEC.
*終了チェック
     IF  NYK-READ-FLG = "END"
         MOVE  MSG08  TO  MSG
         MOVE  "R"    TO  EDIT-OPTION OF SOSAI
                          EDIT-OPTION OF HACYU
                          EDIT-OPTION OF HACYUE
         MOVE   1     TO  ERR-FLG
         GO           TO  NYU-READ3-EXIT
     END-IF.
*本社の場合
     IF     WK-DSOKO  =  "01"
*           発注番号、発注番号枝、相殺
            IF   NYU-F02 = HACYU  AND  NYU-F03 = HACYUE  AND
                 NYU-F04 = SOSAI
*                計上済フラグ
                 IF  NYU-F31 = ZERO
                     CONTINUE
                 ELSE
                     GO       TO   NYU-READ3
                 END-IF
            ELSE
                 MOVE  1      TO   ERR-FLG
                 MOVE  MSG08  TO   MSG
                 MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                   EDIT-OPTION OF HACYU
                                   EDIT-OPTION OF HACYUE
                 GO           TO   NYU-READ3-EXIT
            END-IF
     ELSE
*           発注番号、発注番号枝、相殺
            IF   NYU-F02 = HACYU  AND  NYU-F03 = HACYUE  AND
                 NYU-F04 = SOSAI
*                計上済フラグ＝０、倉庫
                 IF  NYU-F31 = ZERO  AND NYU-F27 = WK-WKSTN
                     CONTINUE
                 ELSE
                     GO         TO    NYU-READ3
                 END-IF
            ELSE
                 MOVE    1      TO   ERR-FLG
                 MOVE    MSG08  TO   MSG
                 MOVE   "R"     TO   EDIT-OPTION OF SOSAI
                                     EDIT-OPTION OF HACYU
                                     EDIT-OPTION OF HACYUE
            END-IF
     END-IF.
 NYU-READ3-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイルＲＥＡＤ（修正）相殺　２
*--------------------------------------------------------------*
 NYU-READ10          SECTION.
*入庫ファイル読込み
     PERFORM  NYKFILF-READ-SEC.
*終了チェック
     IF  NYK-READ-FLG = "END"
         MOVE  MSG08  TO  MSG
         MOVE  "R"    TO  EDIT-OPTION OF SOSAI
                          EDIT-OPTION OF HACYU
                          EDIT-OPTION OF HACYUE
         MOVE   1     TO  ERR-FLG
         GO           TO  NYU-READ10-EXIT
     END-IF.
*本社の場合
     IF     WK-DSOKO  =  "01"
*           発注番号、発注番号枝、相殺
            IF   NYU-F02 = HACYU  AND  NYU-F03 = HACYUE  AND
                 NYU-F04 = SOSAI
*                計上済フラグ
                 IF  NYU-F31 = ZERO
                     CONTINUE
                 ELSE
                     GO       TO   NYU-READ10
                 END-IF
            ELSE
                 MOVE  1      TO   ERR-FLG
                 MOVE  MSG08  TO   MSG
                 MOVE  "R"    TO   EDIT-OPTION OF SOSAI
                                   EDIT-OPTION OF HACYU
                                   EDIT-OPTION OF HACYUE
                 GO           TO   NYU-READ10-EXIT
            END-IF
     ELSE
*           発注番号、発注番号枝、相殺
            IF   NYU-F02 = HACYU  AND  NYU-F03 = HACYUE  AND
                 NYU-F04 = SOSAI
*                計上済フラグ＝０、倉庫
                 IF  NYU-F31 = ZERO  AND NYU-F27 = WK-WKSTN
                     CONTINUE
                 ELSE
                     GO         TO    NYU-READ10
                 END-IF
            ELSE
                 MOVE    1      TO   ERR-FLG
                 MOVE    MSG08  TO   MSG
                 MOVE   "R"     TO   EDIT-OPTION OF SOSAI
                                     EDIT-OPTION OF HACYU
                                     EDIT-OPTION OF HACYUE
            END-IF
     END-IF.
 NYU-READ10-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5          削除時チェック
*--------------------------------------------------------------*
 DEL-CHK           SECTION.
     EVALUATE   SOSAI
         WHEN   0
             MOVE   HACYU  TO    NYU-F02
             MOVE   HACYUE TO    NYU-F03
             MOVE   SOSAI  TO    NYU-F04
             MOVE   1      TO    NYU-F05
             START   NYKFILF   KEY IS >= NYU-F02  NYU-F03
                                         NYU-F04  NYU-F05
                     INVALID   MOVE   MSG08  TO   MSG
                               MOVE   "R"    TO
                                             EDIT-OPTION OF SOSAI
                                             EDIT-OPTION OF HACYU
                                             EDIT-OPTION OF HACYUE
                               MOVE   1      TO   ERR-FLG
                               GO            TO   DEL-CHK-EXIT
             END-START
             PERFORM   NYU-READ6
         WHEN   1   WHEN   2
             MOVE   HACYU  TO    NYU-F02
             MOVE   HACYUE TO    NYU-F03
             MOVE   1      TO    NYU-F04
             MOVE   1      TO    NYU-F05
             START   NYKFILF   KEY IS >= NYU-F02  NYU-F03
                                         NYU-F04  NYU-F05
                     INVALID   MOVE   MSG08  TO   MSG
                               MOVE   "R"    TO
                                             EDIT-OPTION OF SOSAI
                                             EDIT-OPTION OF HACYU
                                             EDIT-OPTION OF HACYUE
                               MOVE   1      TO   ERR-FLG
                               GO            TO   DEL-CHK-EXIT
             END-START
             MOVE      ZERO    TO    SONZAI-F  SONZAI-F2
             PERFORM   NYU-READ7
         WHEN   3   WHEN   4
             MOVE   HACYU  TO    NYU-F02
             MOVE   HACYUE TO    NYU-F03
             MOVE   3      TO    NYU-F04
             MOVE   1      TO    NYU-F05
             START   NYKFILF   KEY IS >= NYU-F02  NYU-F03
                                         NYU-F04  NYU-F05
                     INVALID   MOVE   MSG08  TO   MSG
                               MOVE   "R"    TO
                                             EDIT-OPTION OF SOSAI
                                             EDIT-OPTION OF HACYU
                                             EDIT-OPTION OF HACYUE
                               MOVE   1      TO   ERR-FLG
                               GO            TO   DEL-CHK-EXIT
             END-START
             MOVE      ZERO    TO    SONZAI-F  SONZAI-F2
             PERFORM   NYU-READ11
     END-EVALUATE.
 DEL-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイルＲＥＡＤ（削除）相殺　０    *
*--------------------------------------------------------------*
 NYU-READ6         SECTION.
*入庫ファイル読込み
     PERFORM  NYKFILF-READ-SEC.
*終了チェック
     IF  NYK-READ-FLG = "END"
         MOVE   MSG08  TO   MSG
         MOVE   "R"    TO
                            EDIT-OPTION OF SOSAI
                            EDIT-OPTION OF HACYU
                            EDIT-OPTION OF HACYUE
         MOVE   1      TO   ERR-FLG
         GO            TO   NYU-READ6-EXIT
     END-IF.
*本社の場合
*    発注番号、発注番号枝、相殺、計上済フラグ
     IF     NYU-F02 = HACYU  AND  NYU-F03 = HACYUE  AND
            NYU-F04 = ZERO   AND  NYU-F31 = 1
            MOVE   MSG08  TO   MSG
            MOVE   "R"    TO
                          EDIT-OPTION OF SOSAI
                          EDIT-OPTION OF HACYU
                          EDIT-OPTION OF HACYUE
            MOVE   1      TO   ERR-FLG
            GO            TO   NYU-READ6-EXIT
     ELSE
*   発注番号、発注番号枝、相殺、計上済フラグ、伝区＝５０、６０
            IF  NYU-F02 = HACYU AND NYU-F03 = HACYUE  AND
                NYU-F04 = ZERO AND NYU-F01 = 50  OR  60
                IF  WK-DSOKO = "01"  OR  WK-WKSTN = NYU-F27
                    CONTINUE
                ELSE
                    MOVE   MSG08  TO   MSG
                    MOVE   "R"    TO
                                  EDIT-OPTION OF SOSAI
                                  EDIT-OPTION OF HACYU
                                  EDIT-OPTION OF HACYUE
                    MOVE   1      TO   ERR-FLG
                    GO            TO   NYU-READ6-EXIT
                END-IF
            ELSE
                MOVE   MSG08  TO   MSG
                MOVE   "R"    TO
                              EDIT-OPTION OF SOSAI
                              EDIT-OPTION OF HACYU
                              EDIT-OPTION OF HACYUE
                MOVE   1      TO   ERR-FLG
            END-IF
     END-IF.
 NYU-READ6-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LABEL  6          入庫ファイルＲＥＡＤ（削除）相殺　１，２*
*--------------------------------------------------------------*
*2000/05/13
 NYU-READ7         SECTION.
*入庫ファイル読込み
     PERFORM  NYKFILF-READ-SEC.
*終了チェック
     IF     NYK-READ-FLG = "END"
            IF  SONZAI-F = 1
                GO       TO   NYU-READ7-01
            ELSE
                MOVE   MSG08  TO   MSG
                MOVE   "R"    TO   EDIT-OPTION OF SOSAI
                                   EDIT-OPTION OF HACYU
                                   EDIT-OPTION OF HACYUE
                MOVE   1      TO   ERR-FLG
                GO            TO   NYU-READ7-EXIT
            END-IF
     END-IF.
*    発注番号、発注番号枝、相殺区分
*## 2001/04/06 NAV START ##
*****IF     NYU-F02  =  HACYU  AND  NYU-F03 = HACYUE  AND
*****       NYU-F04  =  1
*****DISPLAY "NYU-F02 = " NYU-F02  UPON CONS.
*****DISPLAY "NYU-F03 = " NYU-F03  UPON CONS.
*****DISPLAY "NYU-F04 = " NYU-F04  UPON CONS.
*****DISPLAY "NYU-F31 = " NYU-F31  UPON CONS.
     IF     NYU-F02  =  HACYU  AND  NYU-F03 = HACYUE  AND
            NYU-F04  =  1      AND  NYU-F31 = ZERO
*## 2001/04/06 NAV END   ##
*           伝票区分＝５０、５１、６０、６１
            IF  NYU-F01 = 50 OR 51 OR 60 OR 61
*               本社、実行倉庫とファイル内倉庫が同一の場合
                IF   WK-DSOKO = "01"  OR  WK-WKSTN = NYU-F27
                     MOVE   1     TO   SONZAI-F
                     GO           TO   NYU-READ7
                ELSE
                     MOVE   MSG08  TO   MSG
                     MOVE   "R"    TO
                                   EDIT-OPTION OF SOSAI
                                   EDIT-OPTION OF HACYU
                                   EDIT-OPTION OF HACYUE
                     MOVE   1      TO   ERR-FLG
                     GO            TO   NYU-READ7-EXIT
                END-IF
            ELSE
                MOVE   MSG08  TO   MSG
                MOVE   "R"    TO
                              EDIT-OPTION OF SOSAI
                              EDIT-OPTION OF HACYU
                              EDIT-OPTION OF HACYUE
                MOVE   1      TO   ERR-FLG
                GO            TO   NYU-READ7-EXIT
     ELSE
*           発注番号、発注番号枝、相殺区分
*## 2001/04/06 NAV START ##
************IF NYU-F02  =  HACYU  AND  NYU-F03 = HACYUE  AND
************   NYU-F04  =  2
            IF NYU-F02  =  HACYU  AND  NYU-F03 = HACYUE  AND
               NYU-F04  =  2      AND  NYU-F31 = ZERO
*## 2001/04/06 NAV END   ##
*              伝票区分＝５０、５１、６０、６１
               IF  NYU-F01 = 50 OR 51 OR 60 OR 61
*                  本社、実行倉庫とファイル倉庫が同一の場合
                   IF   WK-DSOKO = "01"  OR  WK-WKSTN = NYU-F27
                        MOVE   1     TO   SONZAI-F2
                   END-IF
               END-IF
     END-IF.
*****DISPLAY "SONZAI-1 = "  SONZAI-F  UPON CONS.
*****DISPLAY "SONZAI-2 = "  SONZAI-F2 UPON CONS.
     EVALUATE  SOSAI
         WHEN  1
*              存在Ｆ＝１で存在Ｆ＝０の場合
*              相殺区分＝１実行時、既に相殺が存在する場合
               IF   SONZAI-F = 1  AND SONZAI-F2 = ZERO
                    CONTINUE
               ELSE
                    MOVE   MSG08  TO   MSG
                    MOVE   "R"    TO
                                  EDIT-OPTION OF SOSAI
                                  EDIT-OPTION OF HACYU
                                  EDIT-OPTION OF HACYUE
                    MOVE   1      TO   ERR-FLG
                    GO            TO   NYU-READ7-EXIT
               END-IF
         WHEN  2
*              存在Ｆ１＝１で存在Ｆ２＝１の時
*              赤黒時、既に相殺済の黒伝票が存在する場合
               IF   SONZAI-F = 1  AND SONZAI-F2 = 1
                    CONTINUE
               ELSE
                    MOVE   MSG08  TO   MSG
                    MOVE   "R"    TO
                                  EDIT-OPTION OF SOSAI
                                  EDIT-OPTION OF HACYU
                                  EDIT-OPTION OF HACYUE
                    MOVE   1      TO   ERR-FLG
                    GO            TO   NYU-READ7-EXIT
               END-IF
     END-EVALUATE.
 NYU-READ7-01.
*チェック後、同一キーにて再度入庫ファイル読込み
     MOVE   HACYU    TO   NYU-F02.
     MOVE   HACYUE   TO   NYU-F03.
     MOVE   SOSAI    TO   NYU-F04.
     MOVE   1        TO   NYU-F05.
     START   NYKFILF   KEY IS >= NYU-F02  NYU-F03
                                 NYU-F04  NYU-F05
             INVALID   DISPLAY  "*E  NYKFILF  READ  INVALID "
                       "NYU-F02 = " NYU-F02 " NYU-F03 = " NYU-F03
                       " NYU-F04 = " NYU-F04  UPON  STAT
                       STOP  RUN
     END-START.
     READ   NYKFILF    NEXT
            AT   END   DISPLAY  "*E  NYKFILF  READ  INVALID "
                       "NYU-F02 = " NYU-F02 " NYU-F03 = " NYU-F03
                       " NYU-F04 = " NYU-F04  UPON  STAT
                       STOP  RUN
     END-READ.
 NYU-READ7-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LABEL  6          入庫ファイルＲＥＡＤ（削除）相殺　１，２*
*--------------------------------------------------------------*
*2000/05/13
 NYU-READ11        SECTION.
*入庫ファイル読込み
     PERFORM  NYKFILF-READ-SEC.
*終了チェック
     IF     NYK-READ-FLG = "END"
            IF  SONZAI-F3 = 1
                GO       TO   NYU-READ11-01
            ELSE
                MOVE   MSG08  TO   MSG
                MOVE   "R"    TO   EDIT-OPTION OF SOSAI
                                   EDIT-OPTION OF HACYU
                                   EDIT-OPTION OF HACYUE
                MOVE   1      TO   ERR-FLG
                GO            TO   NYU-READ11-EXIT
            END-IF
     END-IF.
*    発注番号、発注番号枝、相殺区分
*## 2001/04/09 NAV START ##
*****IF     NYU-F02  =  HACYU  AND  NYU-F03 = HACYUE  AND
************NYU-F04  =  3
     IF     NYU-F02  =  HACYU  AND  NYU-F03 = HACYUE  AND
            NYU-F04  =  3      AND  NYU-F31 = ZERO
*## 2001/04/09 NAV END   ##
*           伝票区分＝５０、５１、６０、６１
            IF  NYU-F01 = 50 OR 51 OR 60 OR 61
*               本社、実行倉庫とファイル内倉庫が同一の場合
                IF   WK-DSOKO = "01"  OR  WK-WKSTN = NYU-F27
                     MOVE   1     TO   SONZAI-F
                     GO           TO   NYU-READ11
                ELSE
                     MOVE   MSG08  TO   MSG
                     MOVE   "R"    TO
                                   EDIT-OPTION OF SOSAI
                                   EDIT-OPTION OF HACYU
                                   EDIT-OPTION OF HACYUE
                     MOVE   1      TO   ERR-FLG
                     GO            TO   NYU-READ11-EXIT
                END-IF
            ELSE
                MOVE   MSG08  TO   MSG
                MOVE   "R"    TO
                              EDIT-OPTION OF SOSAI
                              EDIT-OPTION OF HACYU
                              EDIT-OPTION OF HACYUE
                MOVE   1      TO   ERR-FLG
                GO            TO   NYU-READ11-EXIT
     ELSE
*## 2001/04/09 NAV START ##
*           発注番号、発注番号枝、相殺区分
************IF NYU-F02  =  HACYU  AND  NYU-F03 = HACYUE  AND
***************NYU-F04  =  4
            IF NYU-F02  =  HACYU  AND  NYU-F03 = HACYUE  AND
               NYU-F04  =  4      AND  NYU-F31 = ZERO
*              伝票区分＝５０、５１、６０、６１
               IF  NYU-F01 = 50 OR 51 OR 60 OR 61
*                  本社、実行倉庫とファイル倉庫が同一の場合
                   IF   WK-DSOKO = "01"  OR  WK-WKSTN = NYU-F27
                        MOVE   1     TO   SONZAI-F4
                   END-IF
               END-IF
     END-IF.
     EVALUATE  SOSAI
         WHEN  3
*              存在Ｆ＝１で存在Ｆ＝０の場合
*              相殺区分＝１実行時、既に相殺が存在する場合
               IF   SONZAI-F3 = 1  AND SONZAI-F4 = ZERO
                    CONTINUE
               ELSE
                    MOVE   MSG08  TO   MSG
                    MOVE   "R"    TO
                                  EDIT-OPTION OF SOSAI
                                  EDIT-OPTION OF HACYU
                                  EDIT-OPTION OF HACYUE
                    MOVE   1      TO   ERR-FLG
                    GO            TO   NYU-READ11-EXIT
               END-IF
         WHEN  4
*              存在Ｆ１＝１で存在Ｆ２＝１の時
*              赤黒時、既に相殺済の黒伝票が存在する場合
               IF   SONZAI-F3 = 1  AND SONZAI-F4 = 1
                    CONTINUE
               ELSE
                    MOVE   MSG08  TO   MSG
                    MOVE   "R"    TO
                                  EDIT-OPTION OF SOSAI
                                  EDIT-OPTION OF HACYU
                                  EDIT-OPTION OF HACYUE
                    MOVE   1      TO   ERR-FLG
                    GO            TO   NYU-READ11-EXIT
               END-IF
     END-EVALUATE.
 NYU-READ11-01.
*チェック後、同一キーにて再度入庫ファイル読込み
     MOVE   HACYU    TO   NYU-F02.
     MOVE   HACYUE   TO   NYU-F03.
     MOVE   SOSAI    TO   NYU-F04.
     MOVE   1        TO   NYU-F05.
     START   NYKFILF   KEY IS >= NYU-F02  NYU-F03
                                 NYU-F04  NYU-F05
             INVALID   DISPLAY  "*E  NYKFILF  READ  INVALID "
                       "NYU-F02 = " NYU-F02 " NYU-F03 = " NYU-F03
                       " NYU-F04 = " NYU-F04  UPON  STAT
                       STOP  RUN
     END-START.
     READ   NYKFILF    NEXT
            AT   END   DISPLAY  "*E  NYKFILF  READ  INVALID "
                       "NYU-F02 = " NYU-F02 " NYU-F03 = " NYU-F03
                       " NYU-F04 = " NYU-F04  UPON  STAT
                       STOP  RUN
     END-READ.
 NYU-READ11-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4          画面ＷＲＩＴＥ
*--------------------------------------------------------------*
 DSP-SEC           SECTION.
     IF   KBN = 1  AND  SOSAI = 0
          PERFORM   TBL-SET
     ELSE
          PERFORM   TBL-SET2
     END-IF.
     MOVE     "ALLF"   TO   DSP-GRP.
     PERFORM   900-DSP-WRITE.
 DSP-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5          画面セット（発注ファイルより）
*--------------------------------------------------------------*
 TBL-SET            SECTION.
*計上区分
     IF   WK-DSOKO = "01"
          MOVE   HAH-F22    TO   KEIJYO
     END-IF.
*伝票区分
     MOVE   HAH-F01      TO   DENKU.
*納入日
     MOVE   SYS-DATE     TO   NOUNYU.
*支払締日
     MOVE   SYS-DATE(1:4) TO  SSIME.
*納期
     MOVE   HAH-F11      TO   NOUKI.
*倉庫ＣＤ
     MOVE   HAH-F17      TO   SOKCD.
*発注日
     MOVE   HAH-F10(3:6) TO   HACYUD.
*量販番号
     MOVE   HAH-F09      TO   RYOHAN.
*請求区分
     MOVE   HAH-F20      TO   SEIKYU.
*仕入先ＣＤ
     MOVE   HAH-F06      TO   SIRCD.
*送料ＣＤ
     MOVE   HAH-F141     TO   SORYCD.
*送料
     MOVE   HAH-F142     TO   SORYO.
*税区分
     MOVE   HAH-F13      TO   ZEIKU.
*2013/12/18↓ 計上税区分セット
*2013/12/26↓
*    IF       ZEIKU   NOT  =  0
     IF       ZEIKU   NOT  =  " "
*2013/12/26↑
              MOVE    "50"     TO   JYO-F01
              MOVE    ZEIKU    TO   JYO-F02
              PERFORM JYO-RD-RTN
              IF      JYO-INV-FLG  =  1
                      MOVE     NC"？？？？？"  TO   ZEINM
              ELSE
                      MOVE     JYO-F03   TO   ZEINM
              END-IF
     END-IF.
*2013/12/18↑
*メモ
     MOVE   HAH-F15      TO   MEMO.
*取引先ＣＤ
     IF     HAH-F15  NOT =    ZERO
            MOVE   HAH-F07      TO   TOKCD
     END-IF.
*納入先ＣＤ
     IF     HAH-F16  NOT =    ZERO
            MOVE   HAH-F16      TO   NONCD
     END-IF.
*伝票区分名称
     EVALUATE   HAH-F01
         WHEN   50     MOVE   NC"仕入"  TO  DENKUN
         WHEN   60     MOVE   NC"直送"  TO  DENKUN
     END-EVALUATE.
*倉庫名称取得
     MOVE   HAH-F17    TO   SOK-F01.
     PERFORM  SOK-READ.
*仕入先名称取得
     MOVE   HAH-F06    TO   SHI-F01.
     PERFORM  SHI-READ.
*取引先名称取得
     MOVE   HAH-F07    TO   TOK-F01.
     IF     HAH-F07    NOT = ZERO
            PERFORM    TOK-READ
     END-IF.
*店舗名称取得
     MOVE   HAH-F07    TO   TEN-F52.
     MOVE   HAH-F16    TO   TEN-F011.
     IF     HAH-F16    NOT = ZERO
            PERFORM  TEN-READ
     END-IF.
*送先名称取得
     MOVE    HAH-F26    TO   OKRICD.
     MOVE    HAH-F27    TO   OKRI1.
     MOVE    HAH-F28    TO   OKRI2.
     MOVE    HAH-F29    TO   OKRI3.
*摘要表示
     MOVE    HAH-F321   TO   TEKCD.
     MOVE    HAH-F322   TO   TEKNM1.
     MOVE    HAH-F323   TO   TEKNM2.
*担当者退避
     MOVE    HAH-F05    TO   WK-TANTO.
*明細行表示
     MOVE    HACYU      TO   HAM-F02.
     MOVE    WK-GYO     TO   HAM-F03.
     READ    HACMEIF
             INVALID  DISPLAY  "*E  HACMEIL1 READ INVALID  "
                       "HAM-F02 = " HACYU " HAM-F03 = " HAM-F03
                       UPON  STAT
                       STOP  RUN
     END-READ.
     MOVE   ZERO         TO   END-FLG.
*    明細情報セット
     PERFORM   VARYING   I  FROM  1  BY  1  UNTIL  I > 6  OR
                                     END-FLG  =  9
        PERFORM  TBL-MEI-SET
        PERFORM  HAC-READ2
     END-PERFORM.
*    本社の場合、仕入金額合計、本社以外の場合、原価金額
     IF  WK-DSOKO = "01"
         MOVE   WK-SIIREG        TO  SIG
     END-IF.
     MOVE       WK-GENKAG        TO  GEG.
 TBL-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6.1        明細行のセット
*--------------------------------------------------------------*
 TBL-MEI-SET        SECTION.
*    発注明細Ｆ－完了区分＝０の時、画面表示
     IF HAM-F05  =  ZERO
*       明細存在フラグ
        MOVE    1             TO    IN-FLG(HAM-F03)
*       完了区分
        MOVE   "1"            TO    KANRYO(HAM-F03)
*       商品ＣＤ
        MOVE    HAM-F06       TO    SYOCD(HAM-F03)
*       品単ＣＤ
        MOVE    HAM-F07(1:5)  TO    HIN1(HAM-F03)
        MOVE    HAM-F07(6:2)  TO    HIN2(HAM-F03)
        MOVE    HAM-F07(8:1)  TO    HIN3(HAM-F03)
*       商品変更区分
        MOVE    HAM-F18          TO  WK-HAM-F18(HAM-F03)
*       商品ＣＤ
        MOVE    HAM-F06          TO  WK-HAM-F06(HAM-F03)
*       品単ＣＤ
        MOVE    HAM-F07          TO  WK-HAM-F07(HAM-F03)
*       発注数量
        MOVE    HAM-F09          TO  WK-HAM-F09(HAM-F03)
*       入庫数量
        MOVE    HAM-F10          TO  WK-HAM-F10(HAM-F03)
*       _番
        MOVE    HAM-F08          TO  WK-HAM-F08(HAM-F03)
*       倉庫
        MOVE    HAH-F17          TO  WK-HAH-F17(HAM-F03)
*       発注数－入庫数
        COMPUTE WK-SURYO(HAM-F03)  = HAM-F09 -  HAM-F10
*       発注数－入庫数
        COMPUTE WK-SURYOX(HAM-F03) = HAM-F09 -  HAM-F10
*       発注数－入庫数が０以上の場合
        IF    WK-SURYO(HAM-F03)  >=  0
              COMPUTE   SURYO(HAM-F03)  =     HAM-F09 -  HAM-F10
        ELSE
              MOVE      ZERO           TO    SURYO(HAM-F03)
        END-IF
*       本社の場合
        IF    WK-DSOKO = "01"
*             仕入単価区分
              MOVE   HAM-F11   TO  TAN1(HAM-F03)
                                   WK-TAN1(HAM-F03)
*             仕入単価
              MOVE   HAM-F12   TO  SITAN(HAM-F03)
                                   WK-SITAN(HAM-F03)
*             仕入単価区分判定
              EVALUATE  HAM-F11
                  WHEN  "1"
                        COMPUTE  WK-SIIRE  =
                                 SURYO(HAM-F03) * SITAN(HAM-F03)
                        COMPUTE  WK-SIIREG =
                                 WK-SIIREG + WK-SIIRE
                  WHEN  SPACE
                        COMPUTE  WK-SIIRE  =
                                 SURYO(HAM-F03) * SITAN(HAM-F03)
                        COMPUTE  WK-SIIREG =
                                 WK-SIIREG + WK-SIIRE
                  WHEN  "3"
                        COMPUTE  WK-SIIREG =
                                 WK-SIIREG + SITAN(HAM-F03)
              END-EVALUATE
        END-IF
*       原価単価区分
        MOVE    HAM-F13          TO  TAN2(HAM-F03)
                                     WK-TAN2(HAM-F03)
*       原価単価
        MOVE    HAM-F14          TO  GENTAN(HAM-F03)
                                     WK-GENTAN(HAM-F03)
*       原価単価区分判定
        EVALUATE  HAM-F13
            WHEN  "1"
                  COMPUTE  WK-GENKA  =
                           SURYO(HAM-F03) * GENTAN(HAM-F03)
                  COMPUTE  WK-GENKAG =
                           WK-GENKAG + WK-GENKA
            WHEN  SPACE
                  COMPUTE  WK-GENKA  =
                           SURYO(HAM-F03) * GENTAN(HAM-F03)
                  COMPUTE  WK-GENKAG =
                           WK-GENKAG + WK-GENKA
            WHEN  "3"
                  COMPUTE  WK-GENKAG =
                           WK-GENKAG + GENTAN(HAM-F03)
        END-EVALUATE
*       販売単価
        MOVE    HAM-F15          TO  HANTAN(HAM-F03)
*       明細備考
        MOVE    HAM-F16          TO  MBIKOU(HAM-F03)
*       商品名称取得
        MOVE    HAM-F06          TO  MEI-F011
        MOVE    HAM-F07          TO  MEI-F012
        READ    HMEIMS
                INVALID      MOVE ALL NC"＊" TO  SYOMEI(HAM-F03)
                NOT INVALID  MOVE MEI-F02    TO  SYOMEI(HAM-F03)
        END-READ
     END-IF.
*
 TBL-MEI-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          発注ファイルＲＥＡＤ（表示時）
*--------------------------------------------------------------*
 HAC-READ2          SECTION.
     READ   HACMEIF   NEXT
            AT  END   MOVE   9     TO  END-FLG
                      GO           TO  HAC-READ2-EXIT
     END-READ.
*    発注番号、発注番号枝、相殺区分が同一の場合
     IF     HAM-F02  =  HACYU
            CONTINUE
     ELSE
            MOVE    9       TO    END-FLG
     END-IF.
  HAC-READ2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5          画面セット（入庫ファイルより）
*--------------------------------------------------------------*
 TBL-SET2           SECTION.
*計上区分
     IF   WK-DSOKO = "01"
          MOVE   NYU-F30    TO   KEIJYO
     END-IF.
*画面へ、発注番号・枝番号・相殺区分セット
     IF   PF-FLG = 1
          MOVE   NYU-F02    TO   HACYU
          MOVE   NYU-F03    TO   HACYUE
          MOVE   NYU-F04    TO   SOSAI
     END-IF.
*相殺区分⇒ワークセット
     MOVE   NYU-F04      TO   WK-SOUSAI.
*伝票区分
     MOVE   NYU-F01      TO   DENKU.
*納入日
     MOVE   NYU-F26      TO   NOUNYU.
*支払締日
     MOVE   NYU-F34(3:4) TO   SSIME.
*倉庫ＣＤ
     MOVE   NYU-F27      TO   SOKCD.
*発注日
     MOVE   NYU-F13(3:6) TO   HACYUD.
*納入日
     MOVE   NYU-F14(3:6) TO   NOUKI.
*量販番号
     MOVE   NYU-F07      TO   RYOHAN.
*請求区分
     MOVE   NYU-F28      TO   SEIKYU.
*仕入先ＣＤ
     MOVE   NYU-F08      TO   SIRCD.
*送料ＣＤ
     MOVE   NYU-F111     TO   SORYCD.
*送料
     MOVE   NYU-F112     TO   SORYO.
*税区分
     MOVE   NYU-F10      TO   ZEIKU.
*2013/12/18↓ 計上税区分セット
*2013/12/26↓
*    IF       ZEIKU   NOT  =  0
     IF       ZEIKU   NOT  =  " "
*2013/12/26↑
              MOVE    "50"     TO   JYO-F01
              MOVE    ZEIKU    TO   JYO-F02
              PERFORM JYO-RD-RTN
              IF      JYO-INV-FLG  =  1
                      MOVE     NC"？？？？？"  TO   ZEINM
              ELSE
                      MOVE     JYO-F03   TO   ZEINM
              END-IF
     END-IF.
*2013/12/18↑
*メモ
*****MOVE   SPACE        TO   MEMO.
*取引先ＣＤ
     IF     NYU-F09  NOT =    ZERO
            MOVE   NYU-F09      TO   TOKCD
     END-IF.
*納入先ＣＤ
     IF     NYU-F12  NOT =    ZERO
            MOVE   NYU-F12      TO   NONCD
     END-IF.
*伝票区分
     EVALUATE   NYU-F01
         WHEN   50     MOVE   NC"仕入"    TO  DENKUN
         WHEN   51     MOVE   NC"返品"    TO  DENKUN
         WHEN   60     MOVE   NC"直送"    TO  DENKUN
         WHEN   61     MOVE   NC"直返品"  TO  DENKUN
         WHEN  OTHER   MOVE   ALL NC"＊"  TO  DENKUN
     END-EVALUATE.
*倉庫名称取得
     MOVE   NYU-F27    TO   SOK-F01.
     PERFORM  SOK-READ.
*仕入先名称取得
     MOVE   NYU-F08    TO   SHI-F01.
     PERFORM  SHI-READ.
*取引先名称取得
     MOVE   NYU-F09    TO   TOK-F01.
     PERFORM  TOK-READ.
*店舗名称取得
     MOVE   NYU-F09    TO   TEN-F52.
     MOVE   NYU-F12    TO   TEN-F011.
     IF     HAH-F16    NUMERIC
     AND    HAH-F16    NOT = ZERO
            PERFORM  TEN-READ
     END-IF.
*送先名称取得／摘要ＣＤセット
     MOVE   HACYU      TO   HAH-F02.
     READ   HACHEDF
            INVALID      MOVE   SPACE    TO   OKRICD
                         MOVE   SPACE    TO   OKRI1 OKRI2 OKRI3
                         MOVE   ZERO     TO   TEKCD
                         MOVE   SPACE    TO   TEKNM1 TEKNM2
                         MOVE   SPACE    TO   MEMO
            NOT INVALID  MOVE   HAH-F26  TO   OKRICD
                         MOVE   HAH-F27  TO   OKRI1
                         MOVE   HAH-F28  TO   OKRI2
                         MOVE   HAH-F29  TO   OKRI3
                         MOVE   HAH-F321 TO   TEKCD
                         MOVE   HAH-F322 TO   TEKNM1
                         MOVE   HAH-F323 TO   TEKNM2
                         MOVE   HAH-F15  TO   MEMO
     END-READ.
*担当者退避
*****MOVE    HAH-F05    TO   WK-TANTO.
*担当者退避
     MOVE    NYU-F33    TO   TANCD.
*明細表示
     MOVE      ZERO     TO  END-FLG.
     PERFORM   VARYING   I  FROM  1  BY  1  UNTIL  I > 6  OR
                                     END-FLG  =  9
*       明細存在区分
        MOVE    1             TO    IN-FLG(NYU-F05)
*       商品ＣＤ
        MOVE    NYU-F15       TO    SYOCD(NYU-F05)
*       品単ＣＤ
        MOVE    NYU-F16(1:5)  TO    HIN1(NYU-F05)
        MOVE    NYU-F16(6:2)  TO    HIN2(NYU-F05)
        MOVE    NYU-F16(8:1)  TO    HIN3(NYU-F05)
*       数量
        MOVE    NYU-F18       TO    SURYO(NYU-F05)
                                    WK-SURYO(NYU-F05)
*       商品＋品単
        MOVE    NYU-F15       TO    WK-HAM-F06(NYU-F05)
        MOVE    NYU-F16       TO    WK-HAM-F07(NYU-F05)
*       _番
        MOVE    NYU-F17       TO    WK-HAM-F08(NYU-F05)
*完了区分を索引する
        MOVE    NYU-F06       TO    KANRYO(NYU-F05)
        MOVE    NYU-F06       TO    WK-KANRYO(NYU-F05)
*発注残数量をＷＫへセット
        MOVE    NYU-F25       TO    WK-ZANSURYO(NYU-F05)
*発注明細読込み
        MOVE    NYU-F02       TO    HAM-F02
        MOVE    NYU-F05       TO    HAM-F03
        READ    HACMEIF
                INVALID   MOVE   1  TO   MST-INV
        END-READ
*       倉庫ＣＤ
        MOVE    NYU-F27       TO    WK-HAH-F17(NYU-F05)
*       商品ＣＤ変更区分
        MOVE    HAM-F18       TO    WK-HAM-F18(NYU-F05)
*       商品ＣＤ
        MOVE    NYU-F15       TO    WK-HAM-F06(NYU-F05)
*       品単ＣＤ
        MOVE    NYU-F16       TO    WK-HAM-F07(NYU-F05)
*       発注数
        MOVE    HAM-F09       TO    WK-HAM-F09(NYU-F05)
*       同一商品入庫総数
        MOVE    HAM-F10       TO    WK-HAM-F10(NYU-F05)
*       元入庫数量
        MOVE    NYU-F18       TO    WK-HAM-F101(NYU-F05)
*       入庫Ｆ商品変更区分
        MOVE    NYU-F35       TO    WK-NYU-F35(NYU-F05)
*       入庫ファイルの仕入金額が　ゼロの時
*       発注ファイルからセットする　
        IF      WK-DSOKO = "01"
*            仕入単価＝０で相殺区分＝１以外
             IF ( NYU-F20     =   ZERO ) AND
                ( SOSAI   NOT =   1    ) AND
                ( SOSAI   NOT =   3    )
*               発注明細Ｆより仕入単価区分、仕入単価セット
                IF     MST-INV   =  ZERO
                         MOVE    HAM-F11 TO   TAN1(NYU-F05)
                                              WK-TAN1(NYU-F05)
                         MOVE    HAM-F12 TO   SITAN(NYU-F05)
                                              WK-SITAN(NYU-F05)
                END-IF
             ELSE
*               入庫Ｆより仕入単価区分、仕入単価セット
                MOVE   NYU-F19   TO  TAN1(NYU-F05)
                                     WK-TAN1(NYU-F05)
                MOVE   NYU-F20   TO  SITAN(NYU-F05)
                                     WK-SITAN(NYU-F05)
             END-IF
*               仕入単価区分判定
                EVALUATE  TAN1(NYU-F05)
                    WHEN  "1"
                          COMPUTE  WK-SIIRE  =
                                   SURYO(NYU-F05) * SITAN(NYU-F05)
                          COMPUTE  WK-SIIREG =
                                   WK-SIIREG + WK-SIIRE
                    WHEN  SPACE
                          COMPUTE  WK-SIIRE  =
                                   SURYO(NYU-F05) * SITAN(NYU-F05)
                          COMPUTE  WK-SIIREG =
                                   WK-SIIREG + WK-SIIRE
                    WHEN  "3"
                          COMPUTE  WK-SIIREG =
                                   WK-SIIREG + SITAN(NYU-F05)
                END-EVALUATE
        END-IF
*       原価単価区分
        MOVE    NYU-F21          TO  TAN2(NYU-F05)
                                     WK-TAN2(NYU-F05)
*       原価単価
        MOVE    NYU-F22          TO  GENTAN(NYU-F05)
                                     WK-GENTAN(NYU-F05)
*       原価単価区分
        EVALUATE  NYU-F21
            WHEN  "1"
                  COMPUTE  WK-GENKA  =
                           SURYO(NYU-F05) * GENTAN(NYU-F05)
                  COMPUTE  WK-GENKAG =
                           WK-GENKAG + WK-GENKA
            WHEN  SPACE
                  COMPUTE  WK-GENKA  =
                           SURYO(NYU-F05) * GENTAN(NYU-F05)
                  COMPUTE  WK-GENKAG =
                           WK-GENKAG + WK-GENKA
            WHEN  "3"
                  COMPUTE  WK-GENKAG =
                           WK-GENKAG + GENTAN(NYU-F05)
        END-EVALUATE
*       販売単価
        MOVE    NYU-F23          TO  HANTAN(NYU-F05)
*       明細備考
        MOVE    NYU-F24          TO  MBIKOU(NYU-F05)
*       商品名称取得
        MOVE    NYU-F15          TO  MEI-F011
        MOVE    NYU-F16          TO  MEI-F012
        READ    HMEIMS
               INVALID      MOVE   ALL NC"＊" TO SYOMEI(NYU-F05)
               NOT INVALID  MOVE   MEI-F02    TO SYOMEI(NYU-F05)
        END-READ
        PERFORM  NYU-READ4
     END-PERFORM.
*    本社の場合、仕入金額をセット
     IF  WK-DSOKO = "01"
         MOVE   WK-SIIREG        TO  SIG
     END-IF.
*    原価金額をセット
     MOVE       WK-GENKAG        TO  GEG.
*    画面（相殺区分、発注番号、発注番号枝をワークへ退避）
     MOVE       SOSAI            TO  WK-SOSAI.
     MOVE       HACYU            TO  WK-HACYU.
     MOVE       HACYUE           TO  WK-HACYUE.
 TBL-SET2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイルＲＥＡＤ（表示時）
*--------------------------------------------------------------*
 NYU-READ4          SECTION.
     READ   NYKFILF   NEXT
            AT   END    MOVE   9   TO   END-FLG
                        GO         TO   NYU-READ4-EXIT
     END-READ.
*    登録モードで相殺区分＝１、２の場合
     IF     KBN  =  1  AND  SOSAI  =  1  OR  2  OR  3  OR  4
*           伝区、発注番号、発注番号枝、相殺区分
            IF     NYU-F01  =  DENKU  AND  NYU-F02  =  HACYU AND
                   NYU-F03  =  HACYUE  AND  NYU-F04  =  WK-SOUSAI
                   CONTINUE
            ELSE
                   MOVE    9       TO    END-FLG
            END-IF
     ELSE
*           伝区、発注番号、発注番号枝、相殺区分
            IF     NYU-F01  =  DENKU  AND  NYU-F02  =  HACYU AND
                   NYU-F03  =  HACYUE  AND  NYU-F04  =  SOSAI
                   CONTINUE
            ELSE
                   MOVE    9       TO    END-FLG
            END-IF
     END-IF.
 NYU-READ4-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6      倉庫マスタＲＥＡＤ                          *
*--------------------------------------------------------------*
 SOK-READ           SECTION.
     READ   ZSOKMS
            INVALID      MOVE   ALL NC"＊"  TO  SOKNM
            NOT INVALID  MOVE   SOK-F02     TO  SOKNM
     END-READ.
 SOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6      仕入先マスタＲＥＡＤ                        *
*--------------------------------------------------------------*
 SHI-READ           SECTION.
     READ   ZSHIMS
            INVALID      MOVE   ALL NC"＊"  TO  SIRNM
                         MOVE   1           TO  MST-INV
            NOT INVALID  MOVE   SHI-F02     TO  SIRNM
                         MOVE   ZERO        TO  MST-INV
     END-READ.
 SHI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6      得意先マスタＲＥＡＤ                        *
*--------------------------------------------------------------*
 TOK-READ           SECTION.
     READ   HTOKMS
            INVALID      MOVE   SPACE       TO  TOKNM
            NOT INVALID  MOVE   TOK-F02     TO  TOKNM
     END-READ.
 TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6      店舗マスタＲＥＡＤ                          *
*--------------------------------------------------------------*
 TEN-READ           SECTION.
     READ   HTENMS
            INVALID      MOVE   SPACE       TO  NONNM
            NOT INVALID  MOVE   TEN-F02     TO  NONNM
     END-READ.
 TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6      担当者マスタＲＥＡＤ     2008.08.08         *
*--------------------------------------------------------------*
 TAN-READ           SECTION.
     READ   HTANMS
            INVALID      MOVE   SPACE       TO  TANNM
                         MOVE   1           TO  MST-INV
            NOT INVALID  MOVE   TAN-F03     TO  TANNM
                         MOVE   ZERO        TO  MST-INV
     END-READ.
 TAN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3          ＧＨＥＡＤ１　入力  (GR-NO = 3)
*--------------------------------------------------------------*
 DSP-HEAD1-RTN     SECTION.
*本社以外での実行時は計上区分／仕入先ＣＤは未入力とする。
     IF   WK-DSOKO NOT = "01"
          MOVE   "X"    TO   EDIT-STATUS  OF  SSIME
          MOVE   "X"    TO   EDIT-STATUS  OF  KEIJYO
          MOVE   "X"    TO   EDIT-STATUS  OF  SIRCD
     END-IF.
*
*担当者コードセット　　　　　　**  2008.08.11
*****MOVE    LINK-M-TANCD    TO   TANCD.
**
     MOVE     G002           TO   PFGID.
     MOVE     "KEIJOU"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      6         TO   GR-NO
     WHEN     PF05
              MOVE      99        TO   GR-NO
     WHEN     PF06
              PERFORM  DSP-BODY-CLR
              PERFORM  WK-BODY-CLR
              MOVE      2         TO   GR-NO
     WHEN     ENT
              PERFORM   CHK-1-RTN
              IF   ERR-FLG  =  ZERO
                   MOVE      4         TO   GR-NO
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              MOVE      MSG01     TO   MSG
     END-EVALUATE.
*
 DSP-HEAD1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4          ＨＥＡＤ部チェック　　　　　　　　*
*--------------------------------------------------------------*
 CHK-1-RTN       SECTION.
     MOVE      ZERO          TO   ERR-FLG.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
*計上チェック
     IF   WK-DSOKO = "01"
          IF   KEIJYO  NOT NUMERIC
               MOVE  "R"     TO   EDIT-OPTION OF KEIJYO
               MOVE  "C"     TO   EDIT-CURSOR OF KEIJYO
               MOVE  MSG04   TO   MSG
               MOVE   30     TO   ERR-FLG
          END-IF
          IF   KEIJYO  = 1  OR  0
               CONTINUE
          ELSE
               MOVE  "R"     TO   EDIT-OPTION OF KEIJYO
               MOVE  "C"     TO   EDIT-CURSOR OF KEIJYO
               MOVE  MSG04   TO   MSG
               MOVE   30     TO   ERR-FLG
          END-IF
     END-IF.
*担当者コード入力＆コードチェック  2008.08.08
     MOVE    LINK-M-BUMON    TO   TAN-F01.
     MOVE    TANCD      TO   TAN-F02.
     PERFORM TAN-READ.
     IF      MST-INV   =    1
          MOVE   "R"   TO   EDIT-OPTION  OF  TANCD
          IF   ERR-FLG  =  ZERO
               MOVE   "C"     TO   EDIT-CURSOR OF TANCD
               MOVE    MSG33  TO  MSG
               MOVE    30     TO  ERR-FLG
          END-IF
     END-IF.
*日付チェック（納入日チェック）
     PERFORM  DATA-CHK.
*2013/12/26 TEST↓
     DISPLAY "1. WK-HEN-DATE2=" WK-HEN-DATE2 UPON CONS
*2013/12/26 TEST↑
*支払締日チェック（システム日付（年月）－１から＋１までの範囲）
     IF      WK-DSOKO  =  "01"
       IF    SSIME  NOT  NUMERIC
             MOVE   "R"   TO   EDIT-OPTION  OF  SSIME
             IF   ERR-FLG  =  ZERO
                  MOVE   "C"     TO   EDIT-CURSOR OF SSIME
                  MOVE    MSG28  TO  MSG
                  MOVE    30     TO  ERR-FLG
             END-IF
       ELSE
             MOVE  SYS-DATE(1:4) TO  WK-CHK1-DT   WK-CHK2-DT
             ADD   -1            TO  WK-CHK1-DT2
             IF    WK-CHK1-DT2  <= ZERO
                   ADD    -1     TO  WK-CHK1-DT1
                   MOVE   12     TO  WK-CHK1-DT2
             END-IF
             ADD    1            TO  WK-CHK2-DT2
             IF    WK-CHK2-DT2  >  12
                   ADD     1     TO  WK-CHK2-DT1
                   MOVE    1     TO  WK-CHK2-DT2
             END-IF
             IF    WK-CHK1-DT  <=  SSIME
             AND   WK-CHK2-DT  >=  SSIME
                   CONTINUE
             ELSE
                   MOVE   "R"   TO   EDIT-OPTION  OF  SSIME
                   IF   ERR-FLG  =  ZERO
                        MOVE   "C"     TO   EDIT-CURSOR OF SSIME
                        MOVE    MSG28  TO  MSG
                        MOVE    30     TO  ERR-FLG
                   END-IF
             END-IF
       END-IF
     END-IF.
*仕入先コード入力＆コードチェック
     MOVE    SIRCD      TO   SHI-F01.
     PERFORM SHI-READ.
     IF      MST-INV   =    1
          MOVE   "R"   TO   EDIT-OPTION  OF  SIRCD
          IF   ERR-FLG  =  ZERO
               MOVE   "C"     TO   EDIT-CURSOR OF SIRCD
               MOVE    MSG22  TO  MSG
               MOVE    30     TO  ERR-FLG
          END-IF.
*送料区分チェック
     IF   SORYCD    = 0  OR   8  OR  9
          CONTINUE
     ELSE
          MOVE   "R"   TO   EDIT-OPTION  OF  SORYCD
          IF   ERR-FLG  =  ZERO
               MOVE   "C"     TO   EDIT-CURSOR OF SORYCD
               MOVE    MSG06  TO  MSG
               MOVE    30     TO  ERR-FLG
          END-IF
     END-IF.
*送料チェック
     IF   SORYO   NOT NUMERIC
          MOVE   "R"   TO   EDIT-OPTION  OF  SORYO
          IF   ERR-FLG  =  ZERO
               MOVE   "C"     TO   EDIT-CURSOR OF SORYO
               MOVE    MSG07  TO   MSG
               MOVE    30     TO   ERR-FLG
          END-IF
     END-IF.
*2013/12/18↓
*計上税区分チェック
*2013/12/26↓
*    IF       ZEIKU   IS  NOT  NUMERIC
*             MOVE    0        TO   ZEIKU
*             MOVE    SPACE    TO   ZEINM
*    END-IF.
*2013/12/26↑
*2013/12/26↓
*    IF       ZEIKU   NOT  =  0
     MOVE     SPACE   TO   ZEINM.
     IF       ZEIKU   NOT  =  " "
*2013/12/26↑
              MOVE    "50"     TO   JYO-F01
              MOVE    ZEIKU    TO   JYO-F02
              PERFORM JYO-RD-RTN
              IF      JYO-INV-FLG  =  1
                      MOVE     MSG34     TO   MSG
                      MOVE     30        TO   ERR-FLG
                      MOVE     "C"       TO   EDIT-CURSOR OF ZEIKU
              ELSE
                      MOVE     JYO-F03   TO   ZEINM
*2013/12/26↓
*2013/12/26 TEST↓
*           DISPLAY "2. WK-HEN-DATE2=" WK-HEN-DATE2 UPON CONS
*           DISPLAY "WK-CHK-NOUHIN=" WK-CHK-NOUHIN UPON CONS
*           DISPLAY "JYO-F04=" JYO-F04       UPON CONS
*2013/12/26 TEST↑
                      IF   WK-CHK-NOUHIN   <    JYO-F04
                           MOVE MSG35 TO   MSG
                           MOVE  35   TO   ERR-FLG
                           MOVE  "C"  TO   EDIT-CURSOR OF ZEIKU
                      END-IF
*2013/12/26↑
              END-IF
     END-IF.
*2013/12/18↑
*
*
 CHK-1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5          納入日チェック
*--------------------------------------------------------------*
 DATA-CHK                 SECTION.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     NOUNYU    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              CONTINUE
     ELSE
              MOVE   "R"        TO   EDIT-OPTION OF NOUNYU
              IF    ERR-FLG   = ZERO
                    MOVE    "C"       TO   EDIT-CURSOR OF NOUNYU
                    MOVE     MSG05    TO   MSG
                    MOVE     2        TO   ERR-FLG
              END-IF
     END-IF.
*
     MOVE  NOUNYU    TO        WK-NOUNYUX WK-HEN-DATE1.
     IF    WK-NOUNYU1X   >  89
           COMPUTE  WK-HEN-DATE2 =  19000000 + WK-HEN-DATE1
     ELSE
           COMPUTE  WK-HEN-DATE2 =  20000000 + WK-HEN-DATE1
     END-IF.
     MOVE  WK-HEN-DATE2      TO   WK-NOUNYU.
*2013/12/26↓
     MOVE  WK-HEN-DATE2      TO   WK-CHK-NOUHIN.
*2013/12/26↑
     IF    WK-NOUNYU4             <        WK-SIME4
           MOVE   "R"        TO   EDIT-OPTION OF NOUNYU
           IF    ERR-FLG   = ZERO
                 MOVE    "C"       TO   EDIT-CURSOR OF NOUNYU
                 MOVE     MSG21    TO   MSG
                 MOVE     2        TO   ERR-FLG
           END-IF
     END-IF.
*
 DATA-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3          ＢＯＤＹ部入力     (GR-NO = 4)
*--------------------------------------------------------------*
 DSP-BODY1-RTN     SECTION.
*本社／倉庫使用時、商品ＣＤプロテクト
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I > 6
*        本社以外で実行の場合（商品、品単プロテクト）
         IF   WK-DSOKO  NOT = "01"
              MOVE   "X"    TO   EDIT-STATUS  OF  SYOCD(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  HIN1(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  HIN2(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  HIN3(I)
         END-IF
*        明細フラグ＝１以外の場合
         IF   IN-FLG(I) NOT = 1
              MOVE   "X"    TO   EDIT-STATUS  OF  KANRYO(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  SYOCD(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  HIN1(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  HIN2(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  HIN3(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  SURYO(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  TAN1(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  SITAN(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  TAN2(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  GENTAN(I)
         ELSE
              IF  WK-DSOKO NOT = "01"
                  MOVE   "X"    TO   EDIT-STATUS  OF  TAN1(I)
                  MOVE   "X"    TO   EDIT-STATUS  OF  SITAN(I)
              END-IF
              IF  KBN NOT = 1
*                 仕入伝票以外は対象外
                  IF   DENKU     NOT = 50  AND  60
                       MOVE   "X"    TO   EDIT-STATUS
                                          OF  KANRYO(I)
                  END-IF
              END-IF
         END-IF
     END-PERFORM.
*
     MOVE     G002           TO   PFGID.
     MOVE     "BODY"         TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      6         TO   GR-NO
     WHEN     PF05
              MOVE      99        TO   GR-NO
     WHEN     PF06
              MOVE      3         TO   GR-NO
     WHEN     ENT
              PERFORM   CHK-2-RTN
              IF   ERR-FLG  =  ZERO
                   PERFORM   TOT-SEC
                   PERFORM   SURYO-CHK-RTN
                   IF  ERR-FLG NOT = ZERO
                       MOVE     "ALLF" TO   DSP-GRP
                       PERFORM  900-DSP-WRITE
*****                  MOVE  ZERO      TO   ERR-FLG
                   END-IF
                   MOVE      5         TO   GR-NO
                   MOVE      "Y"       TO   KAKUN
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              MOVE      MSG01     TO   MSG
     END-EVALUATE.
 DSP-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4          ＢＯＤＹ部チェック
*--------------------------------------------------------------*
 CHK-2-RTN          SECTION.
     MOVE     ZERO                TO   ERR-FLG.
*属性クリア
     PERFORM  CLR-BODY-RTN.
*
     PERFORM  VARYING  I  FROM  1 BY  1  UNTIL  I > 6
          IF  IN-FLG(I)  = 1
*完了区分チェック
*## 00/05/31  IF  KBN = 1  AND SOSAI = 0
******************IF   KANRYO(I)  = 0  OR  1  OR  9
                  IF   KANRYO(I)  = 0  OR  1
                       CONTINUE
                  ELSE
                       MOVE   "R"   TO  EDIT-OPTION OF KANRYO(I)
                       IF  ERR-FLG = ZERO
                           MOVE  "C"    TO
                                         EDIT-CURSOR OF KANRYO(I)
                           MOVE   MSG10 TO  MSG
                           MOVE   1     TO  ERR-FLG
                       END-IF
                  END-IF
*## 00/05/31  END-IF
*既に商品ＣＤの変更が行われている場合変更はできない
*** 商品コード右詰処理  ***
                  IF   SYOCD(I)      NOT = SPACE
                       PERFORM VARYING IXB FROM 1 BY 1
                               UNTIL   IXB    >     7
                          PERFORM VARYING IXC FROM 8 BY -1
                               UNTIL   IXC    <     2
                            IF SYOCD  (I)(IXC:1)  =  SPACE
                               COMPUTE IXD   =   IXC   -   1
                               MOVE  SYOCD  (I)(IXD:1)     TO
                                     SYOCD  (I)(IXC:1)
                               MOVE  SPACE                TO
                                     SYOCD  (I)(IXD:1)
                            END-IF
                          END-PERFORM
                       END-PERFORM
**
                       PERFORM VARYING IXB FROM 1 BY 1
                               UNTIL  (IXB   >      7 ) OR
                              (SYOCD (I)  (IXB:1) NOT =  SPACE)
                               IF SYOCD (I)  (IXB:1)  =  SPACE
                                  MOVE       "0"     TO
                                             SYOCD(I)(IXB:1)
                               END-IF
                       END-PERFORM
                  END-IF
              IF  WK-HAM-F18(I)   =         "1"
              AND WK-NYU-F35(I)   =         "1"
                  IF (WK-HAM-F06(I)  NOT =  SYOCD(I)) OR
                     (WK-HAM-F071(I) NOT =  HIN1(I)) OR
                     (WK-HAM-F072(I) NOT =  HIN2(I)) OR
                     (WK-HAM-F073(I) NOT =  HIN3(I))
                       MOVE   "R"   TO  EDIT-OPTION OF SYOCD(I)
                                        EDIT-OPTION OF HIN1(I)
                                        EDIT-OPTION OF HIN2(I)
                                        EDIT-OPTION OF HIN3(I)
                       MOVE   "C"   TO  EDIT-CURSOR OF SYOCD(I)
                                        EDIT-CURSOR OF HIN1(I)
                                        EDIT-CURSOR OF HIN2(I)
                                        EDIT-CURSOR OF HIN3(I)
                       IF  ERR-FLG  NOT =   1
                           MOVE   MSG24 TO  MSG
                       END-IF
                       MOVE     1   TO  ERR-FLG
                  END-IF
              ELSE
                  IF (WK-HAM-F06(I)  NOT =  SYOCD(I)) OR
                     (WK-HAM-F071(I) NOT =  HIN1(I)) OR
                     (WK-HAM-F072(I) NOT =  HIN2(I)) OR
                     (WK-HAM-F073(I) NOT =  HIN3(I))
                      MOVE  "1"  TO     WK-HAM-F181(I)
*                     IF  WK-HAM-F18(I) = "1"
*                     AND WK-NYU-F35(I) =  SPACE
*                         IF  ERR-FLG  NOT =   1
*                             MOVE   MSG29 TO  MSG
*                         END-IF
*                         MOVE     1   TO  ERR-FLG
*                     ELSE
*新しい商品ＣＤで_番を索引
                          IF      TOKCD      NOT =     ZERO
                              MOVE    TOKCD        TO   SHO-F01
                              MOVE    SYOCD(I)     TO   SHO-F031
                              MOVE    HIN1(I)     TO   SHO-F0321
                              MOVE    HIN2(I)     TO   SHO-F0322
                              MOVE    HIN3(I)     TO   SHO-F0323
                              READ    HSHOTBL
                               INVALID     KEY
                               MOVE SPACE     TO   WK-HAM-F081(I)
                               NOT INVALID KEY
                               MOVE SHO-F08   TO   WK-HAM-F081(I)
                              END-READ
                          ELSE
                            MOVE    SPACE     TO   WK-HAM-F081(I)
                          END-IF
*                     END-IF
                  ELSE
                      IF  WK-HAM-F18(I)  =  SPACE
                      OR  WK-NYU-F35(I)  =  SPACE
                          MOVE  SPACE TO    WK-HAM-F181(I)
                      END-IF
                  END-IF
              END-IF
*商品ＣＤチェック
              IF       WK-HAM-F181(I)   = "1"
                  MOVE     SYOCD(I)   TO     MEI-F011
                  MOVE     HIN1(I)   TO     MEI-F012(1:5)
                  MOVE     HIN2(I)   TO     MEI-F012(6:2)
                  MOVE     HIN3(I)   TO     MEI-F012(8:1)
                  MOVE     ZERO      TO     MST-INV
                  READ    HMEIMS
                    INVALID
                          MOVE   1   TO     MST-INV
                    NOT INVALID
                          MOVE MEI-F02  TO  SYOMEI(I)
                          MOVE   "1" TO     WK-HAM-F181(I)
                  END-READ
                  IF       MST-INV   =      1
                        MOVE   "R"   TO  EDIT-OPTION OF KANRYO(I)
                                         EDIT-OPTION OF HIN1(I)
                                         EDIT-OPTION OF HIN2(I)
                                         EDIT-OPTION OF HIN3(I)
                        MOVE   "C"   TO  EDIT-CURSOR OF KANRYO(I)
                                         EDIT-CURSOR OF HIN1(I)
                                         EDIT-CURSOR OF HIN2(I)
                                         EDIT-CURSOR OF HIN3(I)
                        IF  ERR-FLG  NOT =   1
                            MOVE   MSG23 TO  MSG
                        END-IF
                        MOVE     1   TO  ERR-FLG
                  END-IF
              END-IF
*数量チェック
              IF  SURYO(I)  NOT NUMERIC
                  MOVE   "R"    TO    EDIT-OPTION  OF  SURYO(I)
                  IF  ERR-FLG = ZERO
                      MOVE   "C"      TO
                                         EDIT-CURSOR OF SURYO(I)
                      MOVE    MSG12   TO    MSG
                      MOVE    1       TO    ERR-FLG
                  END-IF
              END-IF
*単価区分（仕入単価）チェック
              IF  WK-DSOKO = "01"
                  IF  TAN1(I) = SPACE  OR "1" OR "3" OR "4"  OR
                             "5" OR "7" OR "9"
*2008/08/29***********IF  TAN1(I) = SPACE
*                         MOVE   SYOCD(I)  TO    MEI-F011
*                         MOVE   HIN1(I)   TO    MEI-F012(1:5)
*                         MOVE   HIN2(I)   TO    MEI-F012(6:2)
*                         MOVE   HIN3(I)   TO    MEI-F012(8:1)
*                         READ   HMEIMS
*                                INVALID
*                                    MOVE  ZERO      TO  SITAN(I)
*                                NOT INVALID
*                                    MOVE  MEI-F041  TO  SITAN(I)
*                         END-READ
**********************END-IF
                      IF  TAN1(I) = "5"  OR  "6"  OR  "7"
                          MOVE   ZERO      TO    SITAN(I)
                      END-IF
                  ELSE
                      MOVE  "R"   TO   EDIT-OPTION OF TAN1(I)
                      IF  ERR-FLG = ZERO
                          MOVE  "C"      TO
                                         EDIT-CURSOR OF TAN1(I)
                          MOVE   MSG11   TO  MSG
                          MOVE   1       TO  ERR-FLG
                      END-IF
                  END-IF
*仕単／金額チェック
                  IF  SITAN(I)  NOT NUMERIC
                      MOVE   "R"   TO   EDIT-OPTION  OF  SITAN(I)
                      IF  ERR-FLG = ZERO
                          MOVE  "C"       TO
                                           EDIT-CURSOR OF SITAN(I)
                          MOVE   MSG13    TO   MSG
                          MOVE   1        TO   ERR-FLG
                      END-IF
                  END-IF
              END-IF
*単価区分（原価単価）チェック
              IF  TAN2(I) = SPACE  OR "1" OR "3" OR "4"  OR
                         "5" OR "7" OR "9"
*2008/08/29*******IF  TAN2(I) = SPACE
*                     MOVE   SYOCD(I)  TO    MEI-F011
*                     MOVE   HIN1(I)   TO    MEI-F012(1:5)
*                     MOVE   HIN2(I)   TO    MEI-F012(6:2)
*                     MOVE   HIN3(I)   TO    MEI-F012(8:1)
*                     READ   HMEIMS
*                            INVALID
*                                MOVE  ZERO      TO  GENTAN(I)
*                            NOT INVALID
*                                MOVE  MEI-F042  TO  GENTAN(I)
*                     END-READ
******************END-IF
                  IF  TAN2(I) = "5"  OR  "6"  OR  "7"
                      MOVE   ZERO      TO    GENTAN(I)
                  END-IF
              ELSE
                  MOVE  "R"   TO   EDIT-OPTION OF TAN2(I)
                  IF  ERR-FLG = ZERO
                      MOVE  "C"      TO   EDIT-CURSOR OF TAN2(I)
                      MOVE   MSG11   TO  MSG
                      MOVE   1       TO  ERR-FLG
                  END-IF
              END-IF
*原単／金額チェック
              IF  GENTAN(I)  NOT NUMERIC
                  MOVE   "R"   TO   EDIT-OPTION  OF  GENTAN(I)
                  IF  ERR-FLG = ZERO
                      MOVE  "C"       TO EDIT-CURSOR OF GENTAN(I)
                      MOVE   MSG14    TO   MSG
                      MOVE   1        TO   ERR-FLG
                  END-IF
              END-IF
          END-IF
     END-PERFORM.
 CHK-2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4          数量入力チェック
* 登録時：発注数量以上の入荷数量を入力した場合、ワーニング発行
* 修正時：発注数量以上の入荷数量を入力した場合、ワーニング発行
*         前回の入荷数量以上を入力した場合、ワーニング発行
*         （入力間違いを防ぐ為に、ワーニングでチェックを促す）
*--------------------------------------------------------------*
 SURYO-CHK-RTN      SECTION.
*属性クリア
     PERFORM  CLR-BODY-RTN.
*
     PERFORM  VARYING  I  FROM  1 BY  1  UNTIL  I > 6
          IF  IN-FLG(I)  = 1
*完了区分チェック
              EVALUATE  KBN
               WHEN  1
               IF  SURYO(I)  >  WK-HAM-F09(I)
                   MOVE   "R"   TO  EDIT-OPTION OF SURYO(I)
                   IF  ERR-FLG = ZERO
                       MOVE "C" TO  EDIT-CURSOR OF SURYO(I)
                       MOVE MSG30 TO  MSG
                       MOVE 1     TO  ERR-FLG
                   END-IF
               END-IF
               WHEN  2
               IF  SURYO(I)  >  WK-HAM-F09(I)
               OR  SURYO(I)  >  WK-HAM-F101(I)
                   MOVE   "R"   TO  EDIT-OPTION OF SURYO(I)
                   IF  ERR-FLG = ZERO
                       MOVE "C" TO  EDIT-CURSOR OF SURYO(I)
                       MOVE MSG30 TO  MSG
                       MOVE 1     TO  ERR-FLG
                   END-IF
               END-IF
               WHEN  OTHER
               CONTINUE
              END-EVALUATE
          END-IF
     END-PERFORM.
*
 SURYO-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4          合計算出
*--------------------------------------------------------------*
 TOT-SEC          SECTION.
     MOVE      ZERO        TO    WK-TAIHI.
     PERFORM   VARYING  I  FROM  1  BY  1  UNTIL I  > 6
          IF  IN-FLG(I) = 1  AND    KANRYO(I) NOT =  9
              IF  WK-DSOKO = "01"
                  EVALUATE  TAN1(I)
                      WHEN  "1"
                            COMPUTE  WK-SIIRE  =
                                         SURYO(I) * SITAN(I)
                            COMPUTE  WK-SIIREG =
                                         WK-SIIREG + WK-SIIRE
                      WHEN  SPACE
                            COMPUTE  WK-SIIRE  =
                                         SURYO(I) * SITAN(I)
                            COMPUTE  WK-SIIREG =
                                         WK-SIIREG + WK-SIIRE
                      WHEN  "3"
                            COMPUTE  WK-SIIREG =
                                         WK-SIIREG + SITAN(I)
                  END-EVALUATE
              END-IF
              EVALUATE  TAN2(I)
                  WHEN  "1"
                        COMPUTE  WK-GENKA  =
                                     SURYO(I) * GENTAN(I)
                        COMPUTE  WK-GENKAG =
                                     WK-GENKAG + WK-GENKA
                  WHEN  SPACE
                        COMPUTE  WK-GENKA  =
                                     SURYO(I) * GENTAN(I)
                        COMPUTE  WK-GENKAG =
                                     WK-GENKAG + WK-GENKA
                  WHEN  "3"
                        COMPUTE  WK-GENKAG =
                                     WK-GENKAG + GENTAN(I)
              END-EVALUATE
          END-IF
     END-PERFORM.
     IF   WK-DSOKO  =  "01"
          MOVE     WK-SIIREG      TO   SIG
     END-IF.
     MOVE          WK-GENKAG      TO   GEG.
 TOT-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3          確認入力      (GR-NO = 5)
*--------------------------------------------------------------*
 DSP-KAKU-RTN     SECTION.
*****MOVE     ZERO           TO   ERR-FLG.
     MOVE      G002          TO   PFGID.
     MOVE     "KAKU"         TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      6         TO   GR-NO
     WHEN     PF05
              MOVE      99        TO   GR-NO
     WHEN     PF06
              IF    KBN  =   3
              OR    KBN  =   1  AND  SOSAI = 1
              OR    KBN  =   3  AND  SOSAI = 1
              OR    KBN  =   1  AND  SOSAI = 3
              OR    KBN  =   3  AND  SOSAI = 3
                    PERFORM   DSP-BODY-CLR
                    PERFORM   WK-BODY-CLR
                    MOVE      2         TO   GR-NO
              ELSE
                    MOVE      4         TO   GR-NO
              END-IF
     WHEN     ENT
              MOVE   ZERO              TO   ERR-FLG
              IF   ( KAKUN  =  "H"        )
              AND  ( SOSAI  =   1   OR  3 )
**************DISPLAY "A-11" UPON STAT
                   MOVE      MSG18     TO   MSG
                   MOVE      1         TO   ERR-FLG
                   GO                  TO   DSP-KAKU-EXIT
              END-IF
**************DISPLAY "A-2" UPON STAT
              IF   KAKUN  =  "Y"  OR  "H"
                   IF   ERR-FLG  NOT = ZERO
                        MOVE   SPACE        TO   MSG
                        PERFORM   900-DSP-WRITE
                   END-IF
                   EVALUATE  KBN
                       WHEN  1      PERFORM    WRITE-SEC
                       WHEN  2      PERFORM    UPDATA-SEC
                       WHEN  3      PERFORM    DELETE-SEC
                   END-EVALUATE
**************DISPLAY "A-3" UPON STAT
                   IF   KAKUN  =  "Y"
                        IF  SOSAI = 1  AND  KBN  =  1
                            MOVE   2    TO   SOSAI
                            MOVE   2    TO   GR-NO
                            GO          TO   DSP-KAKU-EXIT
                        END-IF
**************DISPLAY "A-4" UPON STAT
                        IF  SOSAI = 3  AND  KBN  =  1
                            MOVE   4    TO   SOSAI
                            MOVE   2    TO   GR-NO
                            GO          TO   DSP-KAKU-EXIT
                        END-IF
                        MOVE    6       TO   GR-NO
                   ELSE
**************DISPLAY "A-5" UPON STAT
                        MOVE    2       TO   GR-NO
                        PERFORM         DSP-BODY-CLR
                        PERFORM         WK-BODY-CLR
                   END-IF
              ELSE
                   MOVE      1          TO   ERR-FLG
                   MOVE      MSG15      TO   MSG
              END-IF
**************DISPLAY "A-6" UPON STAT
     WHEN     OTHER
              MOVE      MSG01     TO   MSG
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 DSP-KAKU-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  4          登録処理
*--------------------------------------------------------------*
 WRITE-SEC            SECTION.
     MOVE     HACYU       TO    HAH-F02.
     READ     HACHEDF
              INVALID   DISPLAY "*E  HACHEDF  READ INVALID "
                        HAH-F02  UPON STAT
     END-READ.
     MOVE     HAH-F33     TO    WK-EDA.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
          IF  IN-FLG(I)  =  1
              IF  SOSAI   =  1  OR  2  OR  3  OR  4
                  PERFORM   NYU-WT-SEC
                  PERFORM   ZAI-REWT-SEC
              ELSE
                  EVALUATE   KANRYO(I)
                      WHEN   "1"
                             PERFORM   NYU-WT-SEC
                             PERFORM   ZAI-REWT-SEC
                      WHEN   "9"
                             PERFORM   ZAI-REWT-SEC
                      WHEN   "0"
                             PERFORM   NYU-WT-SEC
                             PERFORM   ZAI-REWT-SEC
                  END-EVALUATE
              END-IF
          END-IF
     END-PERFORM.
     IF   SOSAI  =  ZERO
*         最終枝番カウントアップ
          ADD      1           TO    HAH-F33
*         仕入先ＣＤ
          MOVE     SIRCD       TO    HAH-F06
          REWRITE  HAH-REC
     END-IF.
 WRITE-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5          入庫ファイルＷＲＩＴＥ
*--------------------------------------------------------------*
 NYU-WT-SEC                 SECTION.
*入庫レコード初期化
     MOVE     SPACE    TO      NYU-REC.
     INITIALIZE                NYU-REC.
*伝票区分
     MOVE     DENKU    TO      NYU-F01.
*発注番号
     MOVE     HACYU    TO      NYU-F02.
*発注番号（枝）
     IF  KBN = 1  AND  SOSAI = 1  OR  2  OR  3  OR  4
         MOVE HACYUE   TO      NYU-F03
     ELSE
         MOVE WK-EDA   TO      NYU-F03
     END-IF.
*相殺区分
     MOVE     SOSAI    TO      NYU-F04.
*行番号
     MOVE     I        TO      NYU-F05.
*量販店番号
     MOVE     RYOHAN   TO      NYU-F07.
*仕入先ＣＤ
     MOVE     SIRCD    TO      NYU-F08.
*取引先ＣＤ
     IF       TOKCD  NOT NUMERIC
              CONTINUE
     ELSE
              MOVE     TOKCD    TO      NYU-F09
     END-IF.
*税区分
     MOVE     ZEIKU     TO      NYU-F10.
*送料ＣＤ／送料
     MOVE     SORYCD    TO      NYU-F111.
     MOVE     SORYO     TO      NYU-F112.
*納入先ＣＤ
     IF       NONCD  NOT NUMERIC
              CONTINUE
     ELSE
              MOVE     NONCD    TO      NYU-F12
     END-IF.
*商品ＣＤ
     MOVE     SYOCD(I) TO      NYU-F15.
*品単ＣＤ
     MOVE     HIN1(I)  TO      NYU-F16(1:5).
     MOVE     HIN2(I)  TO      NYU-F16(6:2).
     MOVE     HIN3(I)  TO      NYU-F16(8:1).
*数量
     MOVE     SURYO(I) TO      NYU-F18.
*完了区分
     MOVE     KANRYO(I) TO     NYU-F06.
*発注残数
     MOVE     WK-SURYO(I) TO   NYU-F25.
*本社の場合
     IF   WK-DSOKO = "01"
          MOVE   TAN1(I)  TO   NYU-F19
          MOVE   SITAN(I) TO   NYU-F20
          MOVE   KEIJYO   TO   NYU-F30
     END-IF.
     MOVE     TAN2(I)    TO    NYU-F21.
     MOVE     GENTAN(I)  TO    NYU-F22.
     MOVE     HANTAN(I)  TO    NYU-F23.
*倉庫ＣＤ
     MOVE     SOKCD      TO        NYU-F27.
*請求区分
     MOVE     SEIKYU     TO        NYU-F28.
*発注明細Ｆ読込み
     MOVE     HACYU      TO        HAM-F02.
     MOVE     I          TO        HAM-F03.
     READ     HACMEIF
              INVALID
*                  備考
                   MOVE   SPACE    TO  NYU-F24
*                  _番
                   MOVE   SPACE    TO  NYU-F17
*                  発注日
                   MOVE   ZERO     TO  NYU-F13
              NOT  INVALID
*                  備考
                   MOVE   HAM-F16  TO  NYU-F24
*                  _番
                   MOVE   HAM-F08  TO  NYU-F17
*                  発注日
                   MOVE   HAM-F19  TO  NYU-F13
     END-READ.
*納入日
     MOVE    "3"         TO        LINK-IN-KBN.
     MOVE     NOUNYU     TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING      LINK-IN-KBN
                                   LINK-IN-YMD6
                                   LINK-IN-YMD8
                                   LINK-OUT-RET
                                   LINK-OUT-YMD8.
     MOVE     LINK-OUT-YMD8 TO     NYU-F26  HAM-F17.
*支払締日
     MOVE     SSIME      TO        WK-SSIME-YYMM.
     MOVE     01         TO        WK-SSIME-DD.
     MOVE    "3"         TO        LINK-IN-KBN.
     MOVE     WK-SSIME   TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING      LINK-IN-KBN
                                   LINK-IN-YMD6
                                   LINK-IN-YMD8
                                   LINK-OUT-RET
                                   LINK-OUT-YMD8.
     MOVE LINK-OUT-YMD8(1:6) TO    NYU-F34.
*登録日
*****DISPLAY "SYS-DATE1 = " SYS-DATE UPON STAT.
     MOVE    "3"         TO        LINK-IN-KBN.
     MOVE     SYS-DATE   TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING      LINK-IN-KBN
                                   LINK-IN-YMD6
                                   LINK-IN-YMD8
                                   LINK-OUT-RET
                                   LINK-OUT-YMD8.
     MOVE     LINK-OUT-YMD8 TO     NYU-F98.
*商品ＣＤが変更されている場合
     IF       WK-HAM-F181(I) = "1"
              MOVE     "1"      TO   NYU-F35
     END-IF.
*担当者ＣＤセット
**##  UPD  2008/08/11
***  DISPLAY "WK-TANTO = " WK-TANTO UPON STAT.
***  MOVE     WK-TANTO          TO   NYU-F33.
*****DISPLAY "TANCD    = " TANCD    UPON STAT.
*****MOVE     TANCD             TO   NYU-F33.
     MOVE     LINK-M-TANCD      TO   NYU-F33.
*納入予定日
     MOVE         HAH-F11           TO      NYU-F14.
*入庫ファイル更新（新規レコード追加）
     WRITE    NYU-REC.
*発注明細ファイル更新
*登録／相殺の場合
     IF  KBN = 01  AND SOSAI = 1  OR 2  OR  3  OR  4
         IF   SOSAI  = 1  AND  DENKU = 50
              COMPUTE  HAM-F10   =   HAM-F10  -  SURYO(I)
         ELSE
              COMPUTE  HAM-F10   =   HAM-F10  +  SURYO(I)
         END-IF
     ELSE
         MOVE     KANRYO(I)    TO    HAM-F05
         ADD      SURYO(I)     TO    HAM-F10
         MOVE    "3"           TO    LINK-IN-KBN
         MOVE     SYS-DATE     TO    LINK-IN-YMD6
*********DISPLAY "SYS-DATE = " SYS-DATE UPON STAT
         CALL    "SKYDTCKB" USING    LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8
*********DISPLAY "LINK-YMD = " LINK-OUT-YMD8 UPON STAT
         MOVE     LINK-OUT-YMD8 TO   HAH-F99  HAM-F99
*      入庫数が発注数を越えている時　発注数を入庫数と同じにする
         IF       HAM-F10     >     HAM-F09
                  MOVE              HAM-F10 TO HAM-F09
         END-IF
*       完了区分が　完納または取消で
*       入庫数が発注数よりも少ない時　発注数を入庫数と同じにする
         IF       KANRYO(I)     =     1 OR 9
            IF    HAM-F10     >     HAM-F09
                  MOVE              HAM-F10 TO HAM-F09
            END-IF
         END-IF
*       商品ＣＤが変わっている時
         IF       WK-HAM-F181(I) =  "1"
                  MOVE  SYOCD(I)    TO      HAM-F06
                  MOVE  HIN1(I)     TO      HAM-F07(1:5)
                  MOVE  HIN2(I)     TO      HAM-F07(6:2)
                  MOVE  HIN3(I)     TO      HAM-F07(8:1)
                  MOVE  WK-HAM-F081(I)  TO  HAM-F08
                  MOVE  "1"         TO      HAM-F18
          END-IF
     END-IF.
*    発注明細Ｆ更新
     REWRITE  HAM-REC.
*
 NYU-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      修正処理                                    *
*--------------------------------------------------------------*
 UPDATA-SEC        SECTION.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I > 6
          IF  IN-FLG(I)  =  1
*             入庫ファイル読込み
              MOVE    HACYU  TO   NYU-F02
              MOVE    HACYUE TO   NYU-F03
              MOVE    SOSAI  TO   NYU-F04
              MOVE    I      TO   NYU-F05
              READ    NYKFILF
                      INVALID  DISPLAY "*E  NYKFILF READ INVALID"
                               " NYU-F02 = " NYU-F02 " NYU-F03 = "
                               NYU-F03 "NYU-F04 = " NYU-F04
                               UPON  STAT
                               STOP RUN
                      NOT INVALID
*                     本社の場合（計上区分／単位区分／仕入単価）
                         IF  WK-DSOKO = "01"
                             MOVE  KEIJYO    TO   NYU-F30
                             MOVE  TAN1(I)   TO   NYU-F19
                             MOVE  SITAN(I)  TO   NYU-F20
                         END-IF
*                        納入日西暦変換
                         MOVE "3"            TO   LINK-IN-KBN
                         MOVE  NOUNYU        TO   LINK-IN-YMD6
                         CALL "SKYDTCKB" USING    LINK-IN-KBN
                                                  LINK-IN-YMD6
                                                  LINK-IN-YMD8
                                                  LINK-OUT-RET
                                                  LINK-OUT-YMD8
                         MOVE  LINK-OUT-YMD8 TO   HAH-F26
*                        送料ＣＤ／送料
                         MOVE    SORYCD      TO   NYU-F111
                         MOVE    SORYO       TO   NYU-F112
*                        入庫数
                         MOVE    SURYO(I)    TO   NYU-F18
*                        完了区分
                         MOVE    KANRYO(I)   TO   NYU-F06
*                        単価区分／原価単価
                         MOVE    TAN2(I)     TO   NYU-F21
                         MOVE    GENTAN(I)   TO   NYU-F22
*                        更新日西暦変換
                         MOVE "3"            TO   LINK-IN-KBN
                         MOVE  SYS-DATE      TO   LINK-IN-YMD6
                         CALL "SKYDTCKB" USING    LINK-IN-KBN
                                                  LINK-IN-YMD6
                                                  LINK-IN-YMD8
                                                  LINK-OUT-RET
                                                  LINK-OUT-YMD8
                         MOVE LINK-OUT-YMD8  TO   NYU-F99
*2013/12/18↓
*                        税区分
                         MOVE  ZEIKU         TO   NYU-F10
*2013/12/18↑
              END-READ
*             在庫マスタ更新
              PERFORM   ZAI-REWT-SEC
*             発注残数－入庫数より差を求める
              COMPUTE   WK-SA    =  WK-SURYO(I)  -  NYU-F18
*             商品ＣＤ変更時
              IF           WK-HAM-F181(I) =  "1"
                           MOVE  SYOCD(I)    TO NYU-F15
                           MOVE  HIN1(I)     TO NYU-F16(1:5)
                           MOVE  HIN2(I)     TO NYU-F16(6:2)
                           MOVE  HIN3(I)     TO NYU-F16(8:1)
                           MOVE  WK-HAM-F081(I) TO NYU-F17
              END-IF
*             納入日
              MOVE    "3"         TO        LINK-IN-KBN
              MOVE     NOUNYU     TO        LINK-IN-YMD6
              CALL    "SKYDTCKB" USING      LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              MOVE     LINK-OUT-YMD8 TO     NYU-F26
*             支払締日
              MOVE     SSIME      TO        WK-SSIME-YYMM
              MOVE     01         TO        WK-SSIME-DD
              MOVE    "3"         TO        LINK-IN-KBN
              MOVE     WK-SSIME   TO        LINK-IN-YMD6
              CALL    "SKYDTCKB" USING      LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              MOVE LINK-OUT-YMD8(1:6) TO    NYU-F34
*             仕入先ＣＤ変更時
              MOVE      SIRCD     TO    NYU-F08
*商品ＣＤが変更されている場合
              IF       WK-HAM-F181(I) = "1"
                       MOVE     "1"      TO   NYU-F35
              END-IF
*             入庫ファイル更新
              DISPLAY "TANCD REW= " TANCD    UPON STAT
**************MOVE     TANCD        TO   NYU-F33
              MOVE     LINK-M-TANCD TO   NYU-F33
              REWRITE   NYU-REC
*
              MOVE    HACYU    TO     HAM-F02
              MOVE    I        TO     HAM-F03
              READ    HACMEIF  INVALID
                      DISPLAY "*E  HACMEIF READ INVALID"
                      NYU-F02 "." NYU-F03
                      UPON  STAT
                      STOP RUN
              END-READ
*             入庫数－発注残数から求めた残数
              COMPUTE   HAM-F10  =  HAM-F10  -  WK-SA
*             入庫数が発注数を越えている時
*             発注数を入庫数と同じにする
              IF       HAM-F10     >     HAM-F09
                       MOVE  HAM-F10 TO  HAM-F09
              END-IF
*             完了区分
              MOVE     KANRYO(I)     TO  HAM-F05
*             商品ＣＤが変わっている時
              IF       WK-HAM-F181(I) = "1"
*                      商品ＣＤ
                       MOVE  SYOCD(I)    TO HAM-F06
*                      品単
                       MOVE  HIN1(I)     TO HAM-F07(1:5)
                       MOVE  HIN2(I)     TO HAM-F07(6:2)
                       MOVE  HIN3(I)     TO HAM-F07(8:1)
*                      _番
                       MOVE  WK-HAM-F081(I) TO HAM-F08
*                      商品ＣＤ変更区分
                       MOVE  WK-HAM-F181(I) TO HAM-F18
              END-IF
*             発注明細Ｆ更新
              REWRITE   HAM-REC
*             発注ヘッダＦ読込み
              MOVE    HACYU    TO     HAH-F02
              READ    HACHEDF  INVALID
                      DISPLAY "*E  HACHEDF READ INVALID"
                      HAH-F02
                      UPON  STAT
                      STOP RUN
              END-READ
*             仕入先ＣＤ
              MOVE      SIRCD       TO    HAH-F06
*             発注ヘッダＦ更新
              REWRITE   HAH-REC
          END-IF
     END-PERFORM.
*    相殺区分／発注番号／発注番号枝番
     MOVE      SOSAI          TO   WK-SOSAI.
     MOVE      HACYU          TO   WK-HACYU.
     MOVE      HACYUE         TO   WK-HACYUE.
*
 UPDATA-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4          削除処理
*--------------------------------------------------------------*
 DELETE-SEC      SECTION.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
          IF  IN-FLG(I) = 1
*             入庫ファイル読込み
              MOVE    HACYU  TO   NYU-F02
              MOVE    HACYUE TO   NYU-F03
              MOVE    SOSAI  TO   NYU-F04
              MOVE    I      TO   NYU-F05
              READ    NYKFILF
                      INVALID  DISPLAY "*E  NYKFILF READ INVALID"
                               " NYU-F02 = " NYU-F02 " NYU-F03 = "
                               NYU-F03
                               UPON  STAT
                               STOP RUN
              END-READ
*             在庫マスタ更新
              PERFORM   ZAI-REWT-SEC
*             入庫ファイル削除
              DELETE  NYKFILF
*             発注明細ファイル更新
              MOVE    HACYU   TO   HAM-F02
              MOVE    I       TO   HAM-F03
              READ    HACMEIF  INVALID
                         DISPLAY "*E  HACMEIF READ INVALID"
                         HAM-F02 "." HAM-F03
                         UPON  STAT
                         STOP RUN
              END-READ
*             入庫数の戻し
              IF  SOSAI  =  1  AND    DENKU   =  50
                  COMPUTE  HAM-F10 = HAM-F10  +  NYU-F18
              ELSE
                  COMPUTE  HAM-F10 = HAM-F10  -  NYU-F18
              END-IF
*             発注明細完了区分の戻し
              MOVE  ZERO          TO   HAM-F05
*             更新日西暦変換
              MOVE "3"            TO   LINK-IN-KBN
              MOVE  SYS-DATE      TO   LINK-IN-YMD6
              CALL "SKYDTCKB" USING    LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8
              MOVE LINK-OUT-YMD8 TO    HAM-F99
*             発注明細ファイル更新
              REWRITE   HAM-REC
          END-IF
     END-PERFORM.
*
     MOVE      SOSAI          TO   WK-SOSAI.
     MOVE      HACYU          TO   WK-HACYU.
     MOVE      HACYUE         TO   WK-HACYUE.
*
 DELETE-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5          商品在庫マスタＲＥＷＲＩＴＥ
*--------------------------------------------------------------*
 ZAI-REWT-SEC          SECTION.
*商品ＣＤを変更した場合
*****DISPLAY "WK-HAM-F181(I) = " WK-HAM-F181(I) UPON CONS.
     IF  WK-HAM-F181(I)   =    "1"
         IF  KBN  =  2
             PERFORM               ZAI-SHO-CHG
             GO TO                 ZAI-REWT-EXIT
         END-IF
     END-IF.
*    在庫マスタ更新
     PERFORM    ZAI-READ.
*    在庫マスタ未存在時、レコード作成
     IF  INV-FLG = 1
         MOVE    SPACE    TO   ZAI-REC
         INITIALIZE            ZAI-REC
     END-IF.
*
     EVALUATE   TRUE
         WHEN  (DENKU = 50  AND  SOSAI = 0  AND  KBN = 1)  OR
               (DENKU = 50  AND  SOSAI = 2  AND  KBN = 1)  OR
               (DENKU = 50  AND  SOSAI = 1  AND  KBN = 3)  OR
               (DENKU = 51  AND  SOSAI = 1  AND  KBN = 1)  OR
               (DENKU = 51  AND  SOSAI = 2  AND  KBN = 3)  OR
               (DENKU = 50  AND  SOSAI = 4  AND  KBN = 1)  OR
               (DENKU = 50  AND  SOSAI = 3  AND  KBN = 3)  OR
               (DENKU = 51  AND  SOSAI = 3  AND  KBN = 1)  OR
               (DENKU = 51  AND  SOSAI = 4  AND  KBN = 3)
*               完了区分が”９”以外、現在庫数に数量を加算
                IF      KANRYO(I)   NOT =   9
                  COMPUTE  ZAI-F04  =  ZAI-F04  +  SURYO(I)
                END-IF
*               納品日西暦変換
                MOVE "3"            TO   LINK-IN-KBN
                MOVE  NOUNYU        TO   LINK-IN-YMD6
                CALL "SKYDTCKB" USING    LINK-IN-KBN
                                         LINK-IN-YMD6
                                         LINK-IN-YMD8
                                         LINK-OUT-RET
                                         LINK-OUT-YMD8
                MOVE  LINK-OUT-YMD8 TO   WK-HEN-DATE2
                MOVE  WK-HEN-DATE2  TO   WK-NOUNYU
                IF       WK-SIME4 >= WK-NOUNYU4
                  IF      KANRYO(I)   NOT =   9
*                        当月入庫数加算
                         COMPUTE  ZAI-F07
                                  =  ZAI-F07  +  SURYO(I)
*                        当月入出庫数加算
                         COMPUTE  ZAI-F06
                                  =  ZAI-F06  +  SURYO(I)
                   END-IF
                ELSE
                  IF      KANRYO(I)   NOT =   9
*                        次月入庫数加算
                         COMPUTE  ZAI-F11
                                  =  ZAI-F11  +  SURYO(I)
                   END-IF
                END-IF
*
                IF  INV-FLG = 1
                    CONTINUE
                ELSE
*                相殺区分＝０以外で、完了区分＝９以外の時
                 IF  SOSAI         = ZERO
*****************AND KANRYO(I) NOT = 9
*                   未入庫数更新
                    IF      KANRYO(I)   =  ZERO
*                           未入庫数更新－画面数量
                            COMPUTE  ZAI-F26  =
                                     ZAI-F26  -  SURYO(I)
                    ELSE
*                           未入庫数更新－発注残数
                            COMPUTE  ZAI-F26  =
                                     ZAI-F26  -  WK-SURYOX(I)
                    END-IF
*                   未入庫数が０以下の場合、未入庫数へ０をセット
                    IF      ZAI-F26   <       ZERO
                            MOVE      ZERO    TO    ZAI-F26
                    END-IF
                 END-IF
                END-IF
         WHEN  (DENKU = 50  AND  SOSAI = 0  AND  KBN = 2)  OR
               (DENKU = 50  AND  SOSAI = 2  AND  KBN = 2)  OR
               (DENKU = 51  AND  SOSAI = 2  AND  KBN = 2)  OR
               (DENKU = 50  AND  SOSAI = 4  AND  KBN = 2)  OR
               (DENKU = 51  AND  SOSAI = 4  AND  KBN = 2)
*               発注残数－画面数量より、数量差を求める
                COMPUTE  WK-SA    =  WK-SURYO(I)  -  SURYO(I)
*               現在庫数へ数量差更新
                COMPUTE  ZAI-F04  =  ZAI-F04     -  WK-SA
                MOVE "3"            TO   LINK-IN-KBN
                MOVE  NOUNYU        TO   LINK-IN-YMD6
                CALL "SKYDTCKB" USING    LINK-IN-KBN
                                         LINK-IN-YMD6
                                         LINK-IN-YMD8
                                         LINK-OUT-RET
                                         LINK-OUT-YMD8
                MOVE  LINK-OUT-YMD8 TO   WK-HEN-DATE2
                MOVE  WK-HEN-DATE2  TO   WK-NOUNYU
                IF       WK-SIME4 >= WK-NOUNYU4
*                        当月入庫数更新
                         COMPUTE  ZAI-F07
                                  =  ZAI-F07     -  WK-SA
*                        当月入出庫数更新
                         COMPUTE  ZAI-F06
                                  =  ZAI-F06     -  WK-SA
                ELSE
*                        次月入庫数更新
                         COMPUTE  ZAI-F11
                                  =  ZAI-F11     -  WK-SA
                END-IF
*
                IF  INV-FLG = 1
                    CONTINUE
                ELSE
                 IF SOSAI    = ZERO
********************完納区分が分納の場合
*************       COMPUTE  ZAI-F26  =  ZAI-F26     +  WK-SA
*************       IF       ZAI-F26  <  ZERO
*************                MOVE        ZERO        TO ZAI-F26
*************       END-IF
*************       IF       KANRYO(I)  NOT =  ZERO
*************                COMPUTE  WK-ZAN-SURYO =
*************                         WK-SURYO(I) + WK-SA
*************                COMPUTE  ZAI-F26 = ZAI-F26  -
*************                                   WK-ZAN-SURYO
*************                IF     ZAI-F26  <  ZERO
*************                       MOVE  ZERO  TO  ZAI-F26
*************                END-IF
********************END-IF
********************元完納＝０、新完納＝０
                    IF       WK-KANRYO(I)  =  ZERO
                    AND      KANRYO(I)     =  ZERO
                             COMPUTE ZAI-F26 = ZAI-F26 + WK-SA
                    END-IF
********************元完納＝０、新完納＝１
                    IF       WK-KANRYO(I)  =  ZERO
                    AND      KANRYO(I)     =  1
*****************************COMPUTE ZAI-F26 = ZAI-F26 + WK-SA
*************                COMPUTE  WK-ZAN-SURYO =
*************                         WK-SURYO(I) + WK-SA
*************                COMPUTE  ZAI-F26 = ZAI-F26  -
*****************************                   WK-ZAN-SURYO
*********DISPLAY "ZAI-F26       = "  ZAI-F26        UPON CONS
*********DISPLAY "WK-ZANSURYO   = "  WK-ZANSURYO(I) UPON CONS
*********DISPLAY "SURYO         = "  SURYO(I)       UPON CONS
*********DISPLAY "WK-SURYO      = "  WK-SURYO(I)    UPON CONS
                             COMPUTE  ZAI-F26 = WK-ZANSURYO(I) -
                                      WK-SURYO(I) - ZAI-F26
                    END-IF
********************元完納＝１、新完納＝０
                    IF       WK-KANRYO(I)  =  1
                    AND      KANRYO(I)     =  0
                             COMPUTE ZAI-F26 = WK-ZANSURYO(I) -
                                               SURYO(I)
                    END-IF
                    IF       ZAI-F26  <  ZERO
                             MOVE        ZERO        TO ZAI-F26
                    END-IF
********************
                 END-IF
                END-IF
         WHEN  (DENKU = 50  AND  SOSAI = 0  AND  KBN = 3)  OR
               (DENKU = 50  AND  SOSAI = 1  AND  KBN = 1)  OR
               (DENKU = 50  AND  SOSAI = 2  AND  KBN = 3)  OR
               (DENKU = 51  AND  SOSAI = 1  AND  KBN = 3)  OR
               (DENKU = 51  AND  SOSAI = 2  AND  KBN = 1)  OR
               (DENKU = 50  AND  SOSAI = 3  AND  KBN = 1)  OR
               (DENKU = 50  AND  SOSAI = 4  AND  KBN = 3)  OR
               (DENKU = 51  AND  SOSAI = 3  AND  KBN = 3)  OR
               (DENKU = 51  AND  SOSAI = 4  AND  KBN = 1)
*               現在庫数－画面数量
*2000/05/16 12:00:00
                COMPUTE  ZAI-F04  =  ZAI-F04     -  SURYO(I)
                MOVE "3"            TO   LINK-IN-KBN
                MOVE  NOUNYU        TO   LINK-IN-YMD6
                CALL "SKYDTCKB" USING    LINK-IN-KBN
                                         LINK-IN-YMD6
                                         LINK-IN-YMD8
                                         LINK-OUT-RET
                                         LINK-OUT-YMD8
                MOVE  LINK-OUT-YMD8 TO   WK-HEN-DATE2
                MOVE  WK-HEN-DATE2  TO   WK-NOUNYU
                IF     WK-SIME4   >= WK-NOUNYU4
*                      当月入庫数更新
                       COMPUTE  ZAI-F07  =
                                     ZAI-F07     -  SURYO(I)
*                      当月入出庫数更新
                       COMPUTE  ZAI-F06  =
                                     ZAI-F06     -  SURYO(I)
                ELSE
*                      次月入庫数更新
                       COMPUTE  ZAI-F11  =
                                     ZAI-F11     -  SURYO(I)
                END-IF
*
                IF  INV-FLG = 1
                    CONTINUE
                ELSE
*                未出庫数更新
                 IF SOSAI    = ZERO
                    COMPUTE  ZAI-F26  =  ZAI-F26     +  SURYO(I)
                    IF       ZAI-F26  <  ZERO
                             MOVE        ZERO        TO ZAI-F26
                    END-IF
                 END-IF
                END-IF
         WHEN   OTHER
                GO       TO          ZAI-REWT-EXIT
     END-EVALUATE.
*
 ZAI-REWT-90.
*
     IF  INV-FLG = 1
         PERFORM    ZAI-WRITE
     ELSE
         REWRITE    ZAI-REC
     END-IF.
*
 ZAI-REWT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*
*--------------------------------------------------------------*
 ZAI-SHO-CHG           SECTION.
*商品ＣＤが変更された場合
*元の在庫数を変更
     MOVE   WK-HAH-F17(I)   TO   ZAI-F01.
     MOVE   WK-HAM-F06(I)   TO   ZAI-F021.
     MOVE   WK-HAM-F07(I)   TO   ZAI-F022.
     MOVE   WK-HAM-F08(I)   TO   ZAI-F03.
*
     READ   ZAMZAIF
            INVALID  MOVE    1      TO    INV-FLG
            NOT INVALID
                     MOVE    ZERO   TO    INV-FLG
     END-READ.
*在庫マスタ未存在時
*****DISPLAY "INV-FLG = " INV-FLG UPON CONS.
     IF      INV-FLG        =    1
             GO TO               ZAI-SHO-50
     END-IF.
*現在庫数をへらす
     IF      KANRYO(I)        =    ZERO  OR  1
             IF   KBN = 1
                  COMPUTE ZAI-F04     =
                          ZAI-F04     -    WK-HAM-F10(I)
             ELSE
                  COMPUTE ZAI-F04     =
                          ZAI-F04     -    WK-HAM-F101(I)
             END-IF
     END-IF.
*未入庫数を減らす
     IF      WK-HAM-F09(I) NOT = WK-HAM-F10(I)
*            発注数－入庫数
             COMPUTE WK-HACHU-SA =
                                      WK-HAM-F09(I)
                                 -    WK-HAM-F10(I)
             IF      WK-HACHU-SA >    ZERO
                     COMPUTE ZAI-F26     =
                                      ZAI-F26 - WK-HACHU-SA
             END-IF
     END-IF.
*当月発注数を減らす
     IF      WK-HAM-F09(I) NOT = WK-HAM-F10(I)
             COMPUTE ZAI-F20 = ZAI-F20 - WK-HAM-F09(I)
     ELSE
             COMPUTE ZAI-F20 = ZAI-F20 - SURYO(I)
     END-IF.
*変更日更新
     MOVE "3"            TO   LINK-IN-KBN
     MOVE  SYS-DATE      TO   LINK-IN-YMD6
     CALL "SKYDTCKB" USING    LINK-IN-KBN
                              LINK-IN-YMD6
                              LINK-IN-YMD8
                              LINK-OUT-RET
                              LINK-OUT-YMD8
     MOVE  LINK-OUT-YMD8 TO   ZAI-F99.
*    完了区分＝９（取消）の場合
     IF      KANRYO(I)        =    9
             REWRITE             ZAI-REC
             GO TO               ZAI-SHO-EXIT
     END-IF.
*入出庫数
     MOVE "3"            TO   LINK-IN-KBN
     MOVE  NOUNYU        TO   LINK-IN-YMD6
     CALL "SKYDTCKB" USING    LINK-IN-KBN
                              LINK-IN-YMD6
                              LINK-IN-YMD8
                              LINK-OUT-RET
                              LINK-OUT-YMD8
     MOVE  LINK-OUT-YMD8 TO   WK-HEN-DATE2
     MOVE  WK-HEN-DATE2  TO   WK-NOUNYU.
     IF       WK-SIME4      >=   WK-NOUNYU4
              IF  KBN = 1
*                 当月入庫数更新
                  COMPUTE  ZAI-F07 = ZAI-F07 - WK-HAM-F10(I)
*                 当月入出庫数更新
                  COMPUTE  ZAI-F06 = ZAI-F06 - WK-HAM-F10(I)
              ELSE
*                 当月入庫数更新
                  COMPUTE  ZAI-F07 = ZAI-F07 - WK-HAM-F101(I)
*                 当月入出庫数更新
                  COMPUTE  ZAI-F06 = ZAI-F06 - WK-HAM-F101(I)
              END-IF
     ELSE
              IF  KBN = 1
*                 次月入庫数更新
                  COMPUTE  ZAI-F11 = ZAI-F11 - WK-HAM-F10(I)
              ELSE
*                 次月入庫数更新
                  COMPUTE  ZAI-F11 = ZAI-F11 - WK-HAM-F101(I)
              END-IF
     END-IF.
*在庫マスタ更新
     REWRITE      ZAI-REC.
*
 ZAI-SHO-50.
*
     MOVE   WK-HAH-F17(I)   TO   ZAI-F01.
     MOVE   SYOCD(I)        TO   ZAI-F021.
     MOVE   HIN1(I)         TO   ZAI-F022(1:5).
     MOVE   HIN2(I)         TO   ZAI-F022(6:2).
     MOVE   HIN3(I)         TO   ZAI-F022(8:1).
     MOVE   WK-HAM-F081(I)  TO   ZAI-F03.
*
     READ   ZAMZAIF
            INVALID  MOVE    1      TO    INV-FLG
            NOT INVALID
                     MOVE    ZERO   TO    INV-FLG
     END-READ.
*在庫マスタ未存在の場合
     IF      INV-FLG        =    1
             MOVE SPACE     TO   ZAI-REC
             INITIALIZE          ZAI-REC
     END-IF.
*現在庫数
     ADD     SURYO(I)       TO   ZAI-F04.
*未入庫数（発注数－数量）
     IF      WK-HAM-F09(I) NOT = WK-HAM-F10(I)
             COMPUTE WK-HACHU-SA = WK-HAM-F09(I) - WK-HAM-F10(I)
             COMPUTE ZAI-F26 = ZAI-F26 + WK-HACHU-SA
     END-IF.
*当月発注数
     IF      WK-HAM-F09(I) NOT = WK-HAM-F10(I)
             COMPUTE ZAI-F20 = ZAI-F20 + WK-HAM-F09(I)
     ELSE
             COMPUTE ZAI-F20 = ZAI-F20 + SURYO(I)
     END-IF.
*入出庫数
     MOVE "3"            TO   LINK-IN-KBN.
     MOVE  NOUNYU        TO   LINK-IN-YMD6.
     CALL "SKYDTCKB" USING    LINK-IN-KBN
                              LINK-IN-YMD6
                              LINK-IN-YMD8
                              LINK-OUT-RET
                              LINK-OUT-YMD8.
     MOVE  LINK-OUT-YMD8 TO   WK-HEN-DATE2.
     MOVE  WK-HEN-DATE2  TO   WK-NOUNYU.
     IF       WK-SIME4      >=   WK-NOUNYU4
*             当月入庫数
              COMPUTE  ZAI-F07   =    ZAI-F07  +  SURYO(I)
*             当月入出庫数
              COMPUTE  ZAI-F06   =    ZAI-F06  +  SURYO(I)
     ELSE
*             次月入庫数
              COMPUTE  ZAI-F11   =    ZAI-F11  +  SURYO(I)
     END-IF.
*登録日／変更日更新
     MOVE "3"            TO   LINK-IN-KBN
     MOVE  SYS-DATE      TO   LINK-IN-YMD6
     CALL "SKYDTCKB" USING    LINK-IN-KBN
                              LINK-IN-YMD6
                              LINK-IN-YMD8
                              LINK-OUT-RET
                              LINK-OUT-YMD8
*    在庫マスタ未存在時、キー部セット
     IF      INV-FLG        =    1
*            取引先
             MOVE   WK-HAH-F17(I)   TO   ZAI-F01
*            商品ＣＤ／品単ＣＤ
             MOVE   SYOCD(I)        TO   ZAI-F021
             MOVE   HIN1(I)         TO   ZAI-F022(1:5)
             MOVE   HIN2(I)         TO   ZAI-F022(6:2)
             MOVE   HIN3(I)         TO   ZAI-F022(8:1)
*            _番
             MOVE   WK-HAM-F081(I)  TO   ZAI-F03
*            商品名カナ取得
             MOVE    SYOCD(I)        TO   MEI-F011
             MOVE    HIN1(I)         TO   MEI-F0121
             MOVE    HIN2(I)         TO   MEI-F0122
             MOVE    HIN3(I)         TO   MEI-F0123
             READ    HMEIMS
                INVALID      MOVE SPACE    TO ZAI-F30
                NOT INVALID  MOVE MEI-F031 TO ZAI-F30
             END-READ
*            取引先ＣＤ
             IF  TOKCD     NOT       NUMERIC
                 MOVE   ZERO         TO   ZAI-F29
             ELSE
                 MOVE   TOKCD        TO   ZAI-F29
             END-IF
*            登録日セット
             MOVE   LINK-OUT-YMD8   TO   ZAI-F98
*            在庫マスタ作成
             WRITE     ZAI-REC
     ELSE
*            更新日セット
             MOVE   LINK-OUT-YMD8   TO   ZAI-F99
*            在庫マスタ作成
             REWRITE   ZAI-REC
     END-IF.
*
 ZAI-SHO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6      商品在庫マスタＲＥＡＤ　　　　　　　　　　　*
*--------------------------------------------------------------*
 ZAI-READ                SECTION.
     IF   KBN = 1  AND  SOSAI = 0
          MOVE   WK-HAH-F17(I) TO   ZAI-F01
          MOVE   WK-HAM-F06(I) TO   ZAI-F021
          MOVE   WK-HAM-F07(I) TO   ZAI-F022
          MOVE   WK-HAM-F08(I) TO   ZAI-F03
     ELSE
          MOVE   NYU-F27    TO   ZAI-F01
          MOVE   NYU-F15    TO   ZAI-F021
          MOVE   NYU-F16    TO   ZAI-F022
          MOVE   NYU-F17    TO   ZAI-F03
     END-IF.
     READ   ZAMZAIF
            INVALID  MOVE    1      TO    INV-FLG
            NOT INVALID
                     MOVE    ZERO   TO    INV-FLG
     END-READ.
 ZAI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          商品在庫マスタＷＲＩＴＥ
*--------------------------------------------------------------*
 ZAI-WRITE               SECTION.
     IF   KBN = 1  AND  SOSAI = 0
          MOVE   WK-HAH-F17(I) TO   ZAI-F01
          MOVE   WK-HAM-F06(I) TO   ZAI-F021
          MOVE   WK-HAM-F07(I) TO   ZAI-F022
          MOVE   WK-HAM-F08(I) TO   ZAI-F03
          MOVE   HAH-F07       TO   ZAI-F29
     ELSE
          MOVE   NYU-F27    TO   ZAI-F01
          MOVE   NYU-F15    TO   ZAI-F021
          MOVE   NYU-F16    TO   ZAI-F022
          MOVE   NYU-F17    TO   ZAI-F03
          MOVE   NYU-F09    TO   ZAI-F29
     END-IF.
     WRITE  ZAI-REC.
 ZAI-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  ALL    ワークテーブルクリア
*--------------------------------------------------------------*
 WK-BODY-CLR          SECTION.
*
     MOVE     SPACE    TO   WK-BODY.
     INITIALIZE             WK-BODY.
*
 WK-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  ALL    ＢＯＤＹクリア                              *
*--------------------------------------------------------------*
 DSP-BODY-CLR         SECTION.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  > 6
        MOVE  SPACE    TO   MAS001(I)
     END-PERFORM.
     MOVE     SPACE    TO   KAKUN.
 DSP-BODY-CLR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3          ＢＯＤＹクリア     (GR-NO = 6)
*--------------------------------------------------------------*
 DSP-CLR-RTN          SECTION.
     MOVE     SPACE   TO  FNY00101.
     MOVE     SPACE   TO  DSP-CNTL.
     PERFORM  VARYING I  FROM  1  BY  1  UNTIL  I  >  6
        MOVE  I       TO  GYO(I)
     END-PERFORM.
     MOVE     WK-SYORI    TO   KBN.
     EVALUATE  KBN
         WHEN  1    MOVE  NC"登録"   TO  KBNNM
         WHEN  2    MOVE  NC"修正"   TO  KBNNM
         WHEN  3    MOVE  NC"削除"   TO  KBNNM
     END-EVALUATE.
     PERFORM   CLR-HEAD-RTN.
     PERFORM   CLR-BODY-RTN.
     PERFORM   CLR-TAIL-RTN.
     MOVE      ZERO       TO   PF-FLG  WK-SA.
     MOVE      ZERO       TO   WK-KEY.
     MOVE     "FNY00101"  TO   DSP-FMT.
     MOVE     "ALLF"      TO   DSP-GRP.
     MOVE     2           TO   GR-NO.
     MOVE      ZERO       TO   WK-TAIHI.
     PERFORM   WK-BODY-CLR.
     PERFORM   DSP-BODY-CLR.
 DSP-CLR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL         画面ＲＥＡＤ
*--------------------------------------------------------------*
 DSP-RD-RTN           SECTION.
*行番号セット
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL I  >  6
        MOVE  I              TO   GYO(I)
     END-PERFORM.
*エラーフラグ＝０の場合、メッセージ欄非表示
     IF       ERR-FLG  =  0
              MOVE      SPACE               TO   MSG
     END-IF.
*
     MOVE     HEN-DATE            TO   SDATE.
     MOVE     HEN-TIME            TO   STIME.
     MOVE     HEN-TOKHAN-AREA     TO   TOKHAN.
*
     IF   KBN     =  1
          MOVE    LINK-M-TANCD    TO   TANCD
          MOVE    "X"             TO   EDIT-STATUS OF TANCD
     ELSE
          MOVE    " "             TO   EDIT-STATUS OF TANCD
     END-IF.
     MOVE     "ALLF"         TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     "NE"           TO   DSP-PRO.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPF.
     MOVE     SPACE          TO   DSP-PRO.
 DSP-RD-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL         画面ＷＲＩＴＥ
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FNY00101.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL        ＨＥＡＤ　属性クリア
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF SOSAI
                                  EDIT-CURSOR OF KEIJYO
                                  EDIT-CURSOR OF NOUNYU
                                  EDIT-CURSOR OF HACYU
                                  EDIT-CURSOR OF HACYUE
                                  EDIT-CURSOR OF SORYCD
                                  EDIT-CURSOR OF SORYO
                                  EDIT-CURSOR OF OKRICD
                                  EDIT-CURSOR OF SIRCD
                                  EDIT-CURSOR OF SSIME.
     MOVE     "M"            TO   EDIT-OPTION OF KBN
                                  EDIT-OPTION OF SOSAI
                                  EDIT-OPTION OF KEIJYO
                                  EDIT-OPTION OF NOUNYU
                                  EDIT-OPTION OF HACYU
                                  EDIT-OPTION OF HACYUE
                                  EDIT-OPTION OF SORYCD
                                  EDIT-OPTION OF SORYO
                                  EDIT-OPTION OF OKRICD
                                  EDIT-OPTION OF SIRCD
                                  EDIT-OPTION OF SSIME.
     MOVE     " "            TO   EDIT-CURSOR OF TANCD.
     MOVE     "M"            TO   EDIT-OPTION OF TANCD.
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL        ＢＯＤＹ　属性クリア
*--------------------------------------------------------------*
 CLR-BODY-RTN           SECTION.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
     MOVE     "M"        TO   EDIT-OPTION OF KANRYO(I)
                              EDIT-OPTION OF SURYO(I)
                              EDIT-OPTION OF TAN1(I)
                              EDIT-OPTION OF SITAN(I)
                              EDIT-OPTION OF TAN2(I)
                              EDIT-OPTION OF GENTAN(I)
                              EDIT-OPTION OF KANRYO(I)
                              EDIT-OPTION OF HIN1(I)
                              EDIT-OPTION OF HIN2(I)
                              EDIT-OPTION OF HIN3(I)
     MOVE     " "        TO   EDIT-CURSOR OF KANRYO(I)
                              EDIT-CURSOR OF SURYO(I)
                              EDIT-CURSOR OF TAN1(I)
                              EDIT-CURSOR OF SITAN(I)
                              EDIT-CURSOR OF TAN2(I)
                              EDIT-CURSOR OF GENTAN(I)
                              EDIT-CURSOR OF KANRYO(I)
                              EDIT-CURSOR OF HIN1(I)
                              EDIT-CURSOR OF HIN2(I)
                              EDIT-CURSOR OF HIN3(I)
     END-PERFORM.
 CLR-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL        ＴＡＩＬ　属性クリア
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF KAKUN.
     MOVE     "M"        TO   EDIT-OPTION OF KAKUN.
 CLR-TAIL-EXIT.
     EXIT.
*2013/12/18↓
*--------------------------------------------------------------*
*    LEVEL  ALL   条件ファイル　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 JYO-RD-RTN           SECTION.
     MOVE     0         TO   JYO-INV-FLG.
     READ     HJYOKEN   INVALID   KEY
              MOVE      1         TO   JYO-INV-FLG
              GO   TO   JYO-RD-EXIT
     END-READ.
 JYO-RD-EXIT.
     EXIT.
*2013/12/18↑
*-----------------<< PROGRAM END >>----------------------------*

```
