****************************************************************
*                                                              *
*    顧客名　　　　　：　_　サカタのタネ　　　　　　　　　　　*
*    業務名　　　　　：　在庫管理システム　　　　　　　　　　　*
*    サブシステム名　：　発注管理　                            *
*    モジュール名　　：　発注消込み                            *
*    プログラムＩＤ　：　ＺＨＡ０１００Ｏ                      *
*    作成日　　　　　：　93/05/06                              *
*    作成者　　　　　：　NAV                                   *
*    更新日　　　　　：　                                      *
*    更新者　　　　　：　                                      *
*    更新内容　　　　：　                                      *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZHA0100O.
 AUTHOR.                M.S.
 DATE-WRITTEN.          93/05/06.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-6500.
 OBJECT-COMPUTER.       K-6500.
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
*----<< 発注ファイル >>-*
     SELECT   ZHACHDT   ASSIGN    TO        DA-01-VI-ZHACHDT1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
*****                   RECORD    KEY       HAC-F01  HAC-F02
                        RECORD    KEY       HAC-F02  HAC-F03
                                            HAC-F04  HAC-F05
                        FILE      STATUS    HAC-ST1.
*----<< 入庫ファイル >>-*
     SELECT   ZNYUKDT   ASSIGN    TO        DA-01-VI-ZNYUKDT1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
*****                   RECORD    KEY       NYU-F01  NYU-F02
                        RECORD    KEY                NYU-F02
                                            NYU-F03  NYU-F04
                                            NYU-F05
                        FILE      STATUS    NYU-ST1.
*----<< 商品在庫マスタ >>-*
     SELECT   ZZAIMS    ASSIGN    TO        DA-01-VI-ZZAIMS1
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
*----<< 発注ファイル >>-*
 FD  ZHACHDT
                        LABEL     RECORD     IS  STANDARD.
     COPY     ZHACHDT.
*----<< 入庫ファイル >>-*
 FD  ZNYUKDT
                        LABEL     RECORD     IS  STANDARD.
     COPY     ZNYUKDT   OF   XFDLIB    JOINING   NYU  AS   PREFIX.
*----<< 商品在庫マスタ >>-*
 FD  ZZAIMS
                        LABEL     RECORD     IS  STANDARD.
     COPY     ZZAIMS    OF   XFDLIB    JOINING   ZAI  AS   PREFIX.
*----<< 商品ＣＤ変換ＴＢＬ>>-*
 FD  HSHOTBL
                        LABEL     RECORD     IS  STANDARD.
     COPY     HSHOTBL   OF   XFDLIB    JOINING   SHO  AS   PREFIX.
*----<< 画面ファイル >>-*
 FD  DSPF.
     COPY     ZHA0100   OF   XMDLIB.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  PGM-ID                  PIC  X(08)     VALUE  "ZHA0100O".
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
     03  HAC-ST1             PIC  X(02).
     03  NYU-ST1             PIC  X(02).
     03  ZAI-ST1             PIC  X(02).
     03  SHO-ST1             PIC  X(02).
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
******                          １２３４５６７８９０１２３４
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       32.
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
 01  WK-BODY.
     03  WK-MEISAI           OCCURS  6.
         05  IN-FLG          PIC  9(01)         VALUE ZERO.
         05  WK-R011         PIC  S9(06)V9(02)  VALUE ZERO.
         05  WK-R012         PIC  X(01)         VALUE SPACE.
         05  WK-R013         PIC  S9(07)V9(02)  VALUE ZERO.
         05  WK-R014         PIC  X(01)         VALUE SPACE.
         05  WK-R015         PIC  S9(07)V9(02)  VALUE ZERO.
*\\  93.06.02 START \\\
         05  WK-R011X        PIC  S9(06)V9(02)  VALUE ZERO.
         05  WK-HAC-F45      PIC   X(01)        VALUE SPACE.
         05  WK-HAC-F451     PIC   X(01)        VALUE SPACE.
         05  WK-HAC-F20      PIC   X(08)        VALUE SPACE.
         05  WK-HAC-F21.
             07  WK-HAC-F211 PIC   X(05)        VALUE SPACE.
             07  WK-HAC-F212 PIC   X(02)        VALUE SPACE.
             07  WK-HAC-F213 PIC   X(01)        VALUE SPACE.
         05  WK-HAC-F22      PIC  S9(08)V99     VALUE ZERO.
         05  WK-HAC-F23      PIC  S9(08)V99     VALUE ZERO.
         05  WK-HAC-F31      PIC   X(02)        VALUE SPACE.
         05  WK-HAC-F30      PIC   X(06)        VALUE SPACE.
         05  WK-HAC-F301     PIC   X(06)        VALUE SPACE.
*\\  93.06.02 END   \\\
 01  SHO-CHG                     PIC  9(01)  VALUE ZERO.
*
 01  HIZ-CHK.
     03  WK-AMARI                PIC  9V99.
     03  WK-SHO                  PIC  9(03).
     03  WK-MATUBI               PIC  9(02).
*
 01  WK-WKSTN                PIC  9(02)   VALUE  ZERO.
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
*01  WK-R004.
*    03  WK-R0044            PIC  9(04).
*    03  WK-R0042            PIC  9(02).
 01  WK-R004.
     03  WK-R0044            PIC  9(06).
     03  WK-R0042            PIC  9(02).
 01  WK-R004X.
     03  WK-R0041X           PIC  9(02).
     03  WK-R0042X           PIC  9(02).
     03  WK-R0043X           PIC  9(02).
 01  WK-HEN-DATE1            PIC  9(06)   VALUE  ZERO.
 01  WK-HEN-DATE2            PIC  9(08)   VALUE  ZERO.
 01  WK-EDA                  PIC  9(02)   VALUE  ZERO.
 01  WK-TUKI                 PIC  9(02)   VALUE  ZERO.
 01  WK-GYO                  PIC  9(02)   VALUE  ZERO.
 01  WK-SYORI                PIC  9(01)   VALUE  ZERO.
 01  WK-SOUSAI               PIC  9(01)   VALUE  ZERO.
 01  WK-DENKU                PIC  9(02)   VALUE  ZERO.
 01  WK-SA                   PIC  S9(06)V9(02)  VALUE  ZERO.
 01  WK-KEY.
     03  WK-W002             PIC  9(01)   VALUE  ZERO.
     03  WK-R002             PIC  9(01)   VALUE  ZERO.
     03  WK-R005             PIC  9(07)   VALUE  ZERO.
     03  WK-R006             PIC  9(02)   VALUE  ZERO.
 01  WK-KEY2.
     03  WK-HAC31            PIC  X(02)   VALUE  SPACE.
     03  WK-HAC20            PIC  X(08)   VALUE  SPACE.
     03  WK-HAC21            PIC  X(08)   VALUE  SPACE.
     03  WK-HAC30            PIC  X(06)   VALUE  SPACE.
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
     03  END-FLG             PIC  9(01)   VALUE  ZERO.
     03  ERR-FLG             PIC  9(02)   VALUE  ZERO.
     03  INV-FLG             PIC  9(01)   VALUE  ZERO.
     03  MST-INV             PIC  9(01)   VALUE  ZERO.
     03  HEN-FLG             PIC  9(01)   VALUE  ZERO.
     03  PF-FLG              PIC  9(01)   VALUE  ZERO.
     03  GR-NO               PIC  9(02)   VALUE  ZERO.
*93.10.18 START
 01  WK-HACHU-SA             PIC S9(08)V99  VALUE ZERO.
*93.10.18 END
 01  LINK-AREA2.
     03  LINK-IN-KBN         PIC  X(01).
     03  LINK-IN-YMD6        PIC  9(06).
     03  LINK-IN-YMD8        PIC  9(08).
     03  LINK-OUT-RET        PIC  X(01).
     03  LINK-OUT-YMD8       PIC  9(08).
*---<< ｻﾌﾞﾙｰﾁﾝ LINK AREA >>-*
 LINKAGE                     SECTION.
 01  LINK-AREA.
     03  LINK-WKSTN          PIC  X(08).
******************************************************************
 PROCEDURE                 DIVISION  USING      LINK-AREA.
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
*----------  発注ファイル   -----------------------------------*
 HAC-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   ZHACHDT.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"発注ファイル異常！"
              "ST1=" HAC-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  入庫ファイル   -----------------------------------*
 NYU-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   ZNYUKDT.
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
     USE         AFTER       EXCEPTION PROCEDURE   ZZAIMS.
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
*
     MOVE        ZERO      TO        FLAGS.
*
     OPEN     INPUT     HTOKMS
                        HTENMS
                        ZSHIMS
                        HJYOKEN
                        HMEIMS
                        ZSOKMS
                        HSHOTBL.
     OPEN     I-O       ZHACHDT
                        ZNYUKDT
                        ZZAIMS
                        DSPF.
*
     MOVE     "65"           TO   JYO-F01.
     MOVE     LINK-WKSTN     TO   JYO-F02.
     READ     HJYOKEN
              INVALID
              DISPLAY   "HJYOKEN INV KEY=65"  UPON STAT
              MOVE      99        TO   GR-NO
              GO   TO   INIT-EXIT
     END-READ.
     MOVE     JYO-F04        TO   WK-WKSTN.
*
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
     PERFORM     DSP-R001-RTN     UNTIL     GR-NO  NOT  =  1.
* 相殺区分・発注_入力
     PERFORM     DSP-R002-RTN     UNTIL     GR-NO  NOT  =  2.
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
                        ZHACHDT
                        ZNYUKDT
                        ZZAIMS
                        HSHOTBL
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
     MOVE     SPACE          TO   ZHA0100.
     MOVE     SPACE          TO   DSP-CNTL.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL I  >  6
        MOVE  I              TO   W101(I)
     END-PERFORM.
     MOVE     "ZHA0100"      TO   DSP-FMT.
     MOVE     "ALLF"         TO   DSP-GRP.
     MOVE      ZERO          TO   WK-TAIHI.
     PERFORM   WK-BODY-CLR.
*
     MOVE     1              TO   GR-NO.
 DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3          処理区分　入力　    (GR-NO = 1)　　　
*--------------------------------------------------------------*
 DSP-R001-RTN           SECTION.
     MOVE     G003           TO   W035.
     MOVE     "SYORI"        TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
     WHEN     PF05
              MOVE      99        TO   GR-NO
     WHEN     ENT
              PERFORM   CHK-R001-RTN
              IF   ERR-FLG  =  ZERO
                 MOVE     2              TO   GR-NO
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              MOVE      MSG01     TO   W034
     END-EVALUATE.
*
 DSP-R001-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      処理区分　チェック　       　　　　　
*--------------------------------------------------------------*
 CHK-R001-RTN           SECTION.
     MOVE     ZERO      TO     ERR-FLG.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
     IF       R001  IS  NOT  NUMERIC
              MOVE      0         TO   R001
     END-IF.
     IF       R001  NOT  =  1  AND  2  AND  3
              MOVE      2         TO   ERR-FLG
              MOVE     "R"        TO   EDIT-OPTION OF R001
              MOVE      MSG02     TO   W034
     ELSE
              EVALUATE  R001
                  WHEN  1     MOVE   NC"登録"  TO  W001
                              MOVE   1         TO  WK-SYORI
                  WHEN  2     MOVE   NC"修正"  TO  W001
                              MOVE   2         TO  WK-SYORI
                  WHEN  3     MOVE   NC"削除"  TO  W001
                              MOVE   3         TO  WK-SYORI
              END-EVALUATE
     END-IF.
 CHK-R001-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3          相殺区分　入力　    (GR-NO = 2)　　　
*--------------------------------------------------------------*
 DSP-R002-RTN       SECTION.
     MOVE     G001           TO   W035.
     MOVE     "SOUSAI"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      ZERO      TO   PF-FLG
              MOVE      6         TO   GR-NO
     WHEN     PF05
              MOVE      99        TO   GR-NO
     WHEN     PF06
              MOVE      1         TO   GR-NO
     WHEN     PF11
              IF   R001  =  1
                   MOVE     MSG19      TO   W034
                   MOVE     1          TO   ERR-FLG
              ELSE
                   PERFORM   MAEREC-DSP
                   IF  ERR-FLG = ZERO
                       PERFORM   TOT-SEC
                   END-IF
              END-IF
     WHEN     PF12
              IF   R001  =  1
                   MOVE     MSG19      TO   W034
                   MOVE     1          TO   ERR-FLG
              ELSE
                   PERFORM   BACREC-DSP
                   IF  ERR-FLG = ZERO
                       PERFORM   TOT-SEC
                   END-IF
              END-IF
     WHEN     ENT
              PERFORM   CHK-R002-RTN
              IF   ERR-FLG  =  ZERO
                   IF   PF-FLG = ZERO
                        PERFORM   DSP-SEC
                        MOVE   ZERO   TO  WK-TAIHI
                        PERFORM   TOT-SEC
                   END-IF
                   MOVE   ZERO        TO   PF-FLG
                   IF  R001 = 3   OR  R001 = 1  AND  R002 = 1
                       MOVE  "Y"      TO   R016
                       MOVE   5       TO   GR-NO
                   ELSE
                       MOVE   3       TO   GR-NO
                   END-IF
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              MOVE      MSG01     TO   W034
     END-EVALUATE.
*
 DSP-R002-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4          前レコード表示             　　　　　
*--------------------------------------------------------------*
 MAEREC-DSP             SECTION.
     MOVE   ZERO        TO    ERR-FLG.
     PERFORM   CLR-HEAD-RTN.
     IF     R002  NUMERIC   AND  R005  NUMERIC  AND  R006 NUMERIC
            MOVE     R002   TO   WK-R002
            MOVE     R005   TO   WK-R005
            MOVE     R006   TO   WK-R006
     ELSE
            IF     WK-R002  =  ZERO  AND  WK-R005  =  ZERO  AND
                   WK-R006  =  ZERO
                   MOVE     1         TO   ERR-FLG
                   MOVE     MSG16     TO   W034
                   GO                 TO   MAEREC-DSP-EXIT
            END-IF
     END-IF.
     MOVE   WK-R005     TO     NYU-F02.
     MOVE   WK-R006     TO     NYU-F03.
     MOVE   WK-R002     TO     NYU-F04.
     MOVE   1           TO     NYU-F05.
     START  ZNYUKDT   KEY  IS  <  NYU-F02  NYU-F03
                                  NYU-F04  NYU-F05
                                  WITH  REVERSED
            INVALID   MOVE     MSG16       TO   W034
                      MOVE     1           TO   ERR-FLG
                      GO                   TO   MAEREC-DSP-EXIT
     END-START.
 MAEREC-DSP-01.
     READ   ZNYUKDT   NEXT
            AT  END   MOVE    MSG16        TO   W034
                      MOVE    1            TO   ERR-FLG
                      GO                   TO   MAEREC-DSP-EXIT
     END-READ.
     IF     NYU-F28   =  1    OR   NYU-F04  =  1   OR   2
            GO        TO      MAEREC-DSP-01
     END-IF.
     IF     WK-WKSTN  NOT = 1 AND  WK-WKSTN NOT = NYU-F24
            GO        TO      MAEREC-DSP-01
     END-IF.
     IF     NYU-F04   =  0    AND  NYU-F01  =  50  OR  60
            CONTINUE
     ELSE
            GO        TO      MAEREC-DSP-01
     END-IF.
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
     PERFORM   CLR-HEAD-RTN.
     IF     WK-R002  =  ZERO  AND  WK-R005  =  ZERO  AND
            WK-R006  =  ZERO  AND  R002  NOT NUMERIC  AND
            R005  NOT NUMERIC  AND  R006  NOT NUMERIC
            MOVE     ZERO      TO   NYU-F02
            MOVE     ZERO      TO   NYU-F03
            MOVE     ZERO      TO   NYU-F04
            MOVE     ZERO      TO   NYU-F05
            START  ZNYUKDT   KEY  IS  >=  NYU-F02  NYU-F03
                                          NYU-F04  NYU-F05
                   INVALID   MOVE     MSG17       TO   W034
                             MOVE     1           TO   ERR-FLG
                             GO               TO   BACREC-DSP-EXIT
            END-START
     ELSE
            IF  WK-R002 = ZERO AND WK-R005 = ZERO AND
                WK-R006 = ZERO
                MOVE    R005   TO     WK-R005
                MOVE    R006   TO     WK-R006
                MOVE    R002   TO     WK-R002
            END-IF
            MOVE   WK-R005     TO     NYU-F02
            MOVE   WK-R006     TO     NYU-F03
            MOVE   WK-R002     TO     NYU-F04
            MOVE   6           TO     NYU-F05
            START  ZNYUKDT   KEY  IS  >  NYU-F02  NYU-F03
                                         NYU-F04  NYU-F05
                   INVALID   MOVE     MSG17       TO   W034
                             MOVE     1           TO   ERR-FLG
                             GO              TO   BACREC-DSP-EXIT
            END-START
     END-IF.
 BACREC-DSP-01.
     READ  ZNYUKDT    NEXT
           AT  END    MOVE     MSG17       TO   W034
                      MOVE     1           TO   ERR-FLG
                      GO                   TO   BACREC-DSP-EXIT
     END-READ.
     IF    NYU-F28  =  1    OR    NYU-F04  =  1   OR   2
           GO               TO    BACREC-DSP-01
     END-IF.
     IF    WK-WKSTN  NOT = 1 AND  WK-WKSTN NOT = NYU-F24
           GO               TO    BACREC-DSP-01
     END-IF.
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
 CHK-R002-RTN       SECTION.
*属性クリア
     MOVE     ZERO                TO   ERR-FLG.
     PERFORM  CLR-HEAD-RTN.
     IF       R002  IS  NOT  NUMERIC
              MOVE     "R"        TO   EDIT-OPTION OF R002
              MOVE     "C"        TO   EDIT-CURSOR OF R002
              MOVE      26        TO   ERR-FLG
              MOVE      MSG03     TO   W034
     END-IF.
     IF       R005  IS  NOT  NUMERIC
              MOVE     "R"        TO   EDIT-OPTION OF R005
              IF   ERR-FLG  = ZERO
                   MOVE  "C"      TO   EDIT-CURSOR OF R005
                   MOVE   MSG20   TO   W034
              END-IF
              MOVE      26        TO   ERR-FLG
     END-IF.
     IF       R006  IS  NOT  NUMERIC
              MOVE     "R"        TO   EDIT-OPTION OF R006
              IF   ERR-FLG  = ZERO
                   MOVE  "C"      TO   EDIT-CURSOR OF R006
                   MOVE   MSG20   TO   W034
              END-IF
              MOVE      26        TO   ERR-FLG
     END-IF.
     IF       R002  > 2
              MOVE     "R"        TO   EDIT-OPTION OF R002
              IF   ERR-FLG  = ZERO
                   MOVE  "C"      TO   EDIT-CURSOR OF R002
                   MOVE   MSG03   TO   W034
              END-IF
              MOVE      1         TO   ERR-FLG
     END-IF.
     IF    ERR-FLG NOT = ZERO
           GO        TO     CHK-R002-EXIT
     END-IF.
*
     EVALUATE   R001
         WHEN   1       PERFORM  TOU-CHK
         WHEN   2       PERFORM  SYU-CHK
         WHEN   3       PERFORM  DEL-CHK
     END-EVALUATE.
*
 CHK-R002-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5          登録時チェック
*--------------------------------------------------------------*
 TOU-CHK            SECTION.
     EVALUATE   R002
         WHEN   0
             MOVE  R005    TO    HAC-F02
             MOVE  R006    TO    HAC-F03
             MOVE  ZERO    TO    HAC-F04
             MOVE  1       TO    HAC-F05
             START   ZHACHDT   KEY IS >= HAC-F02  HAC-F03
                                         HAC-F04  HAC-F05
                     INVALID   MOVE   MSG08  TO   W034
                               MOVE   "R"    TO
                                             EDIT-OPTION OF R002
                                             EDIT-OPTION OF R005
                                             EDIT-OPTION OF R006
                               MOVE    1     TO   ERR-FLG
                               GO            TO   TOU-CHK-EXIT
             END-START
             PERFORM   HAC-READ
         WHEN   OTHER
             MOVE  R005    TO    NYU-F02
             MOVE  R006    TO    NYU-F03
             MOVE  ZERO    TO    NYU-F04
             MOVE  1       TO    NYU-F05
             START   ZNYUKDT   KEY IS >=          NYU-F02  NYU-F03
                                         NYU-F04  NYU-F05
                     INVALID KEY
                               MOVE   MSG08  TO   W034
                               MOVE   "R"    TO
                                             EDIT-OPTION OF R002
                                             EDIT-OPTION OF R005
                                             EDIT-OPTION OF R006
                               MOVE    1     TO   ERR-FLG
                               GO            TO   TOU-CHK-EXIT
             END-START
*
             MOVE      ZERO    TO   SONZAI-F   SONZAI-F2
             IF  R002  = 1
                 PERFORM   NYU-READ
             ELSE
                 PERFORM   NYU-READ5
             END-IF
     END-EVALUATE.
 TOU-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          発注ファイルＲＥＡＤ
*--------------------------------------------------------------*
 HAC-READ            SECTION.
     READ   ZHACHDT  NEXT
            AT   END     MOVE   MSG08   TO   W034
                         MOVE   "R"     TO   EDIT-OPTION  OF R002
                                             EDIT-OPTION  OF R005
                                             EDIT-OPTION  OF R006
                         MOVE    1      TO   ERR-FLG
                         GO             TO   HAC-READ-EXIT
     END-READ.
     IF     HAC-F05 = ZERO
            GO        TO    HAC-READ
     END-IF.
     IF     HAC-F05 > 6
            MOVE    MSG08  TO     W034
            MOVE    1      TO     ERR-FLG
            MOVE    "R"    TO     EDIT-OPTION OF R002
                                  EDIT-OPTION OF R005
                                  EDIT-OPTION OF R006
            GO             TO     HAC-READ-EXIT
     END-IF.
     IF     HAC-F37  = 1
            GO             TO     HAC-READ
     ELSE
            IF   HAC-F36  =  ZERO  OR  HAC-F42  =  ZERO
                 GO        TO     HAC-READ
            END-IF
     END-IF.
     IF     WK-WKSTN = 01
            IF  HAC-F02 = R005  AND  HAC-F03 = R006  AND
                HAC-F04 = R002
                IF  (HAC-F06 = ZERO  AND  HAC-F01 = 50)  OR
                    (HAC-F06 = ZERO  AND  HAC-F01 = 60)
                    CONTINUE
                ELSE
                    GO            TO  HAC-READ
                END-IF
            ELSE
                   MOVE    MSG08  TO     W034
                   MOVE    1      TO     ERR-FLG
                   MOVE    "R"    TO     EDIT-OPTION OF R002
                                         EDIT-OPTION OF R005
                                         EDIT-OPTION OF R006
            END-IF
     ELSE
            IF     HAC-F02 = R005  AND  HAC-F03 = R006  AND
                   HAC-F04 = R002  AND  HAC-F31 = WK-WKSTN
                   IF  HAC-F06 = 0  AND  HAC-F01 = 50   OR
                       HAC-F06 = 0  AND  HAC-F01 = 60
                       CONTINUE
                   ELSE
                       GO         TO     HAC-READ
                   END-IF
            ELSE
                   MOVE    MSG08  TO     W034
                   MOVE    1      TO     ERR-FLG
                   MOVE    "R"    TO     EDIT-OPTION OF R002
                                         EDIT-OPTION OF R005
                                         EDIT-OPTION OF R006
            END-IF
     END-IF.
 HAC-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイルＲＥＡＤ（登録）相殺　１
*--------------------------------------------------------------*
 NYU-READ            SECTION.
     READ   ZNYUKDT  NEXT
            AT   END   IF    SONZAI-F = 1
                             GO     TO  NYU-READ-01
                       ELSE
                             MOVE  MSG08  TO  W034
                             MOVE  "R"    TO  EDIT-OPTION OF R002
                                              EDIT-OPTION OF R005
                                              EDIT-OPTION OF R006
                             MOVE   1     TO  ERR-FLG
                             GO           TO  NYU-READ-EXIT
                       END-IF
     END-READ.
     IF     WK-WKSTN = 01
            IF     NYU-F02 = R005  AND  NYU-F03 = R006  AND
                   NYU-F04 = ZERO  AND  NYU-F28 = 1
                       IF  NYU-F23(1:6) NOT > WK-SIME6X
                           MOVE   MSG08 TO   W034
                           MOVE   1     TO   ERR-FLG
                           MOVE  "R"    TO   EDIT-OPTION OF R002
                                         EDIT-OPTION OF R005
                                         EDIT-OPTION OF R006
                       ELSE

                           MOVE      1     TO   SONZAI-F
                           MOVE    NYU-F01 TO   WK-DENKU
                           GO              TO   NYU-READ
                       END-IF
            ELSE
                   IF  NYU-F02 = R005 AND NYU-F03 = R006  AND
                       NYU-F04 = 1
                       MOVE  1      TO   ERR-FLG
                       MOVE  MSG08  TO   W034
                       MOVE  "R"    TO   EDIT-OPTION OF R002
                                         EDIT-OPTION OF R005
                                         EDIT-OPTION OF R006
                       GO           TO   NYU-READ-EXIT
                   END-IF
                   IF  SONZAI-F = 1
                       CONTINUE
                   ELSE
                       MOVE   1     TO  ERR-FLG
                       MOVE   MSG08 TO   W034
                       MOVE  "R"    TO   EDIT-OPTION OF R002
                                         EDIT-OPTION OF R005
                                         EDIT-OPTION OF R006
                   END-IF
            END-IF
     ELSE
            IF     NYU-F02 = R005  AND  NYU-F03 = R006  AND
                   NYU-F04 = ZERO  AND  NYU-F24 = WK-WKSTN  AND
                   NYU-F28 = 1
                   MOVE      1     TO   SONZAI-F
                   MOVE    NYU-F01 TO   WK-DENKU
                   GO              TO   NYU-READ
            ELSE
                   IF  NYU-F02 = R005 AND NYU-F03 = R006  AND
                       NYU-F04 = 1
                       MOVE  MSG08  TO   W034
                       MOVE  1      TO   ERR-FLG
                       MOVE  "R"    TO   EDIT-OPTION OF R002
                                         EDIT-OPTION OF R005
                                         EDIT-OPTION OF R006
                       GO           TO   NYU-READ-EXIT
                   END-IF
                   IF  SONZAI-F = 1
                       CONTINUE
                   ELSE
                       MOVE   1     TO   ERR-FLG
                       MOVE   MSG08 TO   W034
                       MOVE  "R"    TO   EDIT-OPTION OF R002
                                         EDIT-OPTION OF R005
                                         EDIT-OPTION OF R006
                   END-IF
            END-IF
     END-IF.
 NYU-READ-01.
     MOVE   R005       TO   NYU-F02.
     MOVE   R006       TO   NYU-F03.
     MOVE   ZERO       TO   NYU-F04.
     MOVE   1          TO   NYU-F05.
     START   ZNYUKDT   KEY IS >= NYU-F02  NYU-F03
                                 NYU-F04  NYU-F05
             INVALID  DISPLAY  "*E  ZNYUKDT  READ INVALID  "
                  "NYU-F01 = " NYU-F01 " NYU-F02 = " NYU-F02
                 " NYU-F03 = " NYU-F03 " NYU-F04 = " NYU-F04
                   UPON  STAT
                   STOP  RUN
     END-START.
     READ   ZNYUKDT   NEXT
            AT  END   DISPLAY  "*E  ZNYUKDT  READ INVALID  "
                  "NYU-F01 = " NYU-F01 " NYU-F02 = " NYU-F02
                 " NYU-F03 = " NYU-F03 " NYU-F04 = " NYU-F04
                   UPON  STAT
                   STOP  RUN
     END-READ.
 NYU-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイルＲＥＡＤ（登録）相殺　２
*--------------------------------------------------------------*
 NYU-READ5           SECTION.
     READ   ZNYUKDT  NEXT
            AT   END   IF    SONZAI-F = 1 AND  SONZAI-F2 = 1
                             GO     TO  NYU-READ5-01
                       ELSE
                             MOVE  MSG08  TO  W034
                             MOVE  "R"    TO  EDIT-OPTION OF R002
                                              EDIT-OPTION OF R005
                                              EDIT-OPTION OF R006
                             MOVE   1     TO  ERR-FLG
                             GO           TO  NYU-READ5-EXIT
                       END-IF
     END-READ.
     IF     WK-WKSTN  =  01
            IF     NYU-F02 = R005  AND  NYU-F03 = R006  AND
                   NYU-F04 = ZERO  AND  NYU-F28 = 1
                   MOVE   1        TO   SONZAI-F
                   MOVE   NYU-F01  TO   WK-DENKU
                   GO              TO   NYU-READ5
            ELSE
                   IF  NYU-F02 = R005 AND NYU-F03 = R006  AND
                       NYU-F04 = 1
                       MOVE  1      TO   SONZAI-F2
                       GO           TO   NYU-READ5
                   ELSE
                       IF  NYU-F02 = R005 AND NYU-F03 = R006  AND
                           NYU-F04 = 2
                           MOVE  MSG08  TO   W034
                           MOVE  1      TO   ERR-FLG
                           MOVE  "R"    TO   EDIT-OPTION OF R002
                                             EDIT-OPTION OF R005
                                             EDIT-OPTION OF R006
                           GO           TO   NYU-READ5-EXIT
                       END-IF
                       IF  SONZAI-F = 1 AND  SONZAI-F2 = 1
                           CONTINUE
                       ELSE
                           MOVE   MSG08 TO   W034
                           MOVE   1     TO   ERR-FLG
                           MOVE  "R"    TO   EDIT-OPTION OF R002
                                             EDIT-OPTION OF R005
                                             EDIT-OPTION OF R006
                       END-IF
                   END-IF
            END-IF
     ELSE
            IF     NYU-F02 = R005  AND  NYU-F03 = R006  AND
                   NYU-F04 = ZERO  AND  NYU-F28 = 1  AND
                   NYU-F24 = WK-WKSTN
                   MOVE   1        TO   SONZAI-F
                   MOVE   NYU-F01  TO   WK-DENKU
                   GO              TO   NYU-READ5
            ELSE
                   IF  NYU-F02 = R005 AND NYU-F03 = R006  AND
                       NYU-F04 = 1
                       MOVE  1      TO   SONZAI-F2
                       GO           TO   NYU-READ5
                   ELSE
                       IF  NYU-F02 = R005 AND NYU-F03 = R006 AND
                           NYU-F04 = 2
                           MOVE  MSG08  TO   W034
                           MOVE  1      TO   ERR-FLG
                           MOVE  "R"    TO   EDIT-OPTION OF R002
                                             EDIT-OPTION OF R005
                                             EDIT-OPTION OF R006
                           GO           TO   NYU-READ5-EXIT
                       END-IF
                       IF  SONZAI-F = 1 AND  SONZAI-F2 = 1
                           CONTINUE
                       ELSE
                           MOVE   MSG08 TO   W034
                           MOVE   1     TO   ERR-FLG
                           MOVE  "R"    TO   EDIT-OPTION OF R002
                                             EDIT-OPTION OF R005
                                             EDIT-OPTION OF R006
                       END-IF
                   END-IF
            END-IF
     END-IF.
 NYU-READ5-01.
     MOVE   R005       TO   NYU-F02.
     MOVE   R006       TO   NYU-F03.
     MOVE   1          TO   NYU-F04.
     MOVE   1          TO   NYU-F05.
     START   ZNYUKDT   KEY IS >= NYU-F02  NYU-F03
                                 NYU-F04  NYU-F05
             INVALID  DISPLAY  "*E  ZNYUKDT  READ INVALID  "
                  "NYU-F01 = " NYU-F01 " NYU-F02 = " NYU-F02
                 " NYU-F03 = " NYU-F03 " NYU-F04 = " NYU-F04
                   UPON  STAT
                   STOP  RUN
     END-START.
     READ   ZNYUKDT   NEXT
            AT  END   DISPLAY  "*E  ZNYUKDT  READ INVALID  "
                  "NYU-F01 = " NYU-F01 " NYU-F02 = " NYU-F02
                 " NYU-F03 = " NYU-F03 " NYU-F04 = " NYU-F04
                   UPON  STAT
                   STOP  RUN
     END-READ.
 NYU-READ5-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5          修正時チェック
*--------------------------------------------------------------*
 SYU-CHK            SECTION.
     EVALUATE   R002
         WHEN   0
             MOVE   50     TO    NYU-F01
             MOVE   R005   TO    NYU-F02
             MOVE   R006   TO    NYU-F03
             MOVE   R002   TO    NYU-F04
             MOVE   1      TO    NYU-F05
             START   ZNYUKDT   KEY IS >= NYU-F02  NYU-F03
                                         NYU-F04  NYU-F05
                     INVALID   MOVE   MSG08  TO   W034
                               MOVE   "R"    TO
                                             EDIT-OPTION OF R002
                                             EDIT-OPTION OF R005
                                             EDIT-OPTION OF R006
                               MOVE   1      TO   ERR-FLG
                               GO            TO   SYU-CHK-EXIT
             END-START
             PERFORM   NYU-READ2
         WHEN   2
             MOVE   R005   TO    NYU-F02
             MOVE   R006   TO    NYU-F03
             MOVE   R002   TO    NYU-F04
             MOVE   1      TO    NYU-F05
             START   ZNYUKDT   KEY IS >= NYU-F02  NYU-F03
                                         NYU-F04  NYU-F05
                     INVALID   MOVE   MSG08  TO   W034
                               MOVE   "R"    TO
                                             EDIT-OPTION OF R002
                                             EDIT-OPTION OF R005
                                             EDIT-OPTION OF R006
                               MOVE   1      TO   ERR-FLG
                               GO            TO   SYU-CHK-EXIT
             END-START
             PERFORM   NYU-READ3
         WHEN   1
             MOVE    1     TO   ERR-FLG
             MOVE    MSG08 TO   W034
             MOVE    "R"   TO   EDIT-OPTION OF R002
                                EDIT-OPTION OF R005
                                EDIT-OPTION OF R006
     END-EVALUATE.
 SYU-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイルＲＥＡＤ（修正）相殺　０
*--------------------------------------------------------------*
 NYU-READ2           SECTION.
     READ   ZNYUKDT  NEXT
            AT   END   MOVE  MSG08  TO  W034
                       MOVE  "R"    TO  EDIT-OPTION OF R002
                                        EDIT-OPTION OF R005
                                        EDIT-OPTION OF R006
                       MOVE   1     TO  ERR-FLG
                       GO           TO  NYU-READ2-EXIT
     END-READ.
     IF     WK-WKSTN  =  01
            IF   NYU-F02 = R005  AND  NYU-F03 = R006  AND
                 NYU-F04 = R002
                 IF  NYU-F28 = ZERO   AND  NYU-F01 = 50 OR 60
                     CONTINUE
                 ELSE
                     GO        TO    NYU-READ2
                 END-IF
            ELSE
                 MOVE  1      TO   ERR-FLG
                 MOVE  MSG08  TO   W034
                 MOVE  "R"    TO   EDIT-OPTION OF R002
                                   EDIT-OPTION OF R005
                                   EDIT-OPTION OF R006
                 GO           TO   NYU-READ2-EXIT
            END-IF
     ELSE
            IF   NYU-F02 = R005  AND  NYU-F03 = R006  AND
                 NYU-F04 = R002
                 IF  NYU-F28 = ZERO  AND NYU-F01 = 50 AND
                     NYU-F24 = WK-WKSTN  OR
                     NYU-F28 = ZERO  AND NYU-F01 = 60 AND
                     NYU-F24 = WK-WKSTN
                     CONTINUE
                 ELSE
                     GO         TO   NYU-READ2
                 END-IF
            ELSE
                 MOVE    1      TO   ERR-FLG
                 MOVE    MSG08  TO   W034
                 MOVE    "R"    TO   EDIT-OPTION OF R002
                                     EDIT-OPTION OF R005
                                     EDIT-OPTION OF R006
             END-IF
     END-IF.
 NYU-READ2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイルＲＥＡＤ（修正）相殺　２
*--------------------------------------------------------------*
 NYU-READ3           SECTION.
     READ   ZNYUKDT  NEXT
            AT   END   MOVE  MSG08  TO  W034
                       MOVE  "R"    TO  EDIT-OPTION OF R002
                                        EDIT-OPTION OF R005
                                        EDIT-OPTION OF R006
                       MOVE   1     TO  ERR-FLG
                       GO           TO  NYU-READ3-EXIT
     END-READ.
     IF     WK-WKSTN  =  01
            IF   NYU-F02 = R005  AND  NYU-F03 = R006  AND
                 NYU-F04 = R002
                 IF  NYU-F28 = ZERO
                     CONTINUE
                 ELSE
                     GO       TO   NYU-READ3
                 END-IF
            ELSE
                 MOVE  1      TO   ERR-FLG
                 MOVE  MSG08  TO   W034
                 MOVE  "R"    TO   EDIT-OPTION OF R002
                                   EDIT-OPTION OF R005
                                   EDIT-OPTION OF R006
                 GO           TO   NYU-READ3-EXIT
            END-IF
     ELSE
            IF   NYU-F02 = R005  AND  NYU-F03 = R006  AND
                 NYU-F04 = R002
                 IF  NYU-F28 = ZERO  AND NYU-F24 = WK-WKSTN
                     CONTINUE
                 ELSE
                     GO         TO    NYU-READ3
                 END-IF
            ELSE
                 MOVE    1      TO    ERR-FLG
                 MOVE    MSG08  TO   W034
                 MOVE   "R"     TO   EDIT-OPTION OF R002
                                     EDIT-OPTION OF R005
                                     EDIT-OPTION OF R006
            END-IF
     END-IF.
 NYU-READ3-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5          削除時チェック
*--------------------------------------------------------------*
 DEL-CHK           SECTION.
     EVALUATE   R002
         WHEN   0
             MOVE   R005   TO    NYU-F02
             MOVE   R006   TO    NYU-F03
             MOVE   R002   TO    NYU-F04
             MOVE   1      TO    NYU-F05
             START   ZNYUKDT   KEY IS >= NYU-F02  NYU-F03
                                         NYU-F04  NYU-F05
                     INVALID   MOVE   MSG08  TO   W034
                               MOVE   "R"    TO
                                             EDIT-OPTION OF R002
                                             EDIT-OPTION OF R005
                                             EDIT-OPTION OF R006
                               MOVE   1      TO   ERR-FLG
                               GO            TO   DEL-CHK-EXIT
             END-START
             PERFORM   NYU-READ6
         WHEN   OTHER
             MOVE   R005   TO    NYU-F02
             MOVE   R006   TO    NYU-F03
             MOVE   1      TO    NYU-F04
             MOVE   1      TO    NYU-F05
             START   ZNYUKDT   KEY IS >= NYU-F02  NYU-F03
                                         NYU-F04  NYU-F05
                     INVALID   MOVE   MSG08  TO   W034
                               MOVE   "R"    TO
                                             EDIT-OPTION OF R002
                                             EDIT-OPTION OF R005
                                             EDIT-OPTION OF R006
                               MOVE   1      TO   ERR-FLG
                               GO            TO   DEL-CHK-EXIT
             END-START
             MOVE      ZERO    TO    SONZAI-F  SONZAI-F2
             PERFORM   NYU-READ7
     END-EVALUATE.
 DEL-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイルＲＥＡＤ（削除）相殺　０    *
*--------------------------------------------------------------*
 NYU-READ6         SECTION.
     READ    ZNYUKDT   NEXT
             AT  END
                       MOVE   MSG08  TO   W034
                       MOVE   "R"    TO
                                     EDIT-OPTION OF R002
                                     EDIT-OPTION OF R005
                                     EDIT-OPTION OF R006
                       MOVE   1      TO   ERR-FLG
                       GO            TO   NYU-READ6-EXIT
     END-READ.
     IF     NYU-F02 = R005  AND  NYU-F03 = R006  AND
            NYU-F04 = ZERO  AND  NYU-F28 = 1
            MOVE   MSG08  TO   W034
            MOVE   "R"    TO
                          EDIT-OPTION OF R002
                          EDIT-OPTION OF R005
                          EDIT-OPTION OF R006
            MOVE   1      TO   ERR-FLG
            GO            TO   NYU-READ6-EXIT
     ELSE
            IF  NYU-F02 = R005 AND NYU-F03 = R006  AND
                NYU-F04 = ZERO AND NYU-F01 = 50  OR  60
                IF  WK-WKSTN = 01  OR  WK-WKSTN = NYU-F24
                    CONTINUE
                ELSE
                    MOVE   MSG08  TO   W034
                    MOVE   "R"    TO
                                  EDIT-OPTION OF R002
                                  EDIT-OPTION OF R005
                                  EDIT-OPTION OF R006
                    MOVE   1      TO   ERR-FLG
                    GO            TO   NYU-READ6-EXIT
                END-IF
            ELSE
                MOVE   MSG08  TO   W034
                MOVE   "R"    TO
                              EDIT-OPTION OF R002
                              EDIT-OPTION OF R005
                              EDIT-OPTION OF R006
                MOVE   1      TO   ERR-FLG
            END-IF
     END-IF.
 NYU-READ6-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイルＲＥＡＤ（削除）相殺　１，２*
*--------------------------------------------------------------*
 NYU-READ7         SECTION.
     READ   ZNYUKDT  NEXT
            AT  END  IF  SONZAI-F = 1
                         GO       TO   NYU-READ7-01
                     ELSE
                         MOVE   MSG08  TO   W034
                         MOVE   "R"    TO
                                       EDIT-OPTION OF R002
                                       EDIT-OPTION OF R005
                                       EDIT-OPTION OF R006
                         MOVE   1      TO   ERR-FLG
                         GO            TO   NYU-READ7-EXIT
                     END-IF
     END-READ.
     IF     NYU-F02  =  R005  AND  NYU-F03 = R006  AND
            NYU-F04  =  1
            IF  NYU-F01 = 50 OR 51 OR 52 OR 55 OR 60 OR 61 OR
                NYU-F01 = 70 AND NYU-F07 = "29099999" OR
                NYU-F01 = 71 AND NYU-F07 = "29099999" OR
                NYU-F01 = 80 AND NYU-F07 = "29099999" OR
                NYU-F01 = 81 AND NYU-F07 = "29099999"
                IF   WK-WKSTN = 01  OR  WK-WKSTN = NYU-F24
                     MOVE   1     TO   SONZAI-F
                     GO           TO   NYU-READ7
                ELSE
                     MOVE   MSG08  TO   W034
                     MOVE   "R"    TO
                                   EDIT-OPTION OF R002
                                   EDIT-OPTION OF R005
                                   EDIT-OPTION OF R006
                     MOVE   1      TO   ERR-FLG
                     GO            TO   NYU-READ7-EXIT
                END-IF
            ELSE
                MOVE   MSG08  TO   W034
                MOVE   "R"    TO
                              EDIT-OPTION OF R002
                              EDIT-OPTION OF R005
                              EDIT-OPTION OF R006
                MOVE   1      TO   ERR-FLG
                GO            TO   NYU-READ7-EXIT
     ELSE
            IF NYU-F02  =  R005  AND  NYU-F03 = R006  AND
               NYU-F04  =  2
               IF  NYU-F01 = 50 OR 51 OR 52 OR 55 OR 60 OR 61 OR
                   NYU-F01 = 70 AND NYU-F07 = "29099999" OR
                   NYU-F01 = 71 AND NYU-F07 = "29099999" OR
                   NYU-F01 = 80 AND NYU-F07 = "29099999" OR
                   NYU-F01 = 81 AND NYU-F07 = "29099999"
                   IF   WK-WKSTN = 01  OR  WK-WKSTN = NYU-F24
                        MOVE   1     TO   SONZAI-F2
                   END-IF
               END-IF
     END-IF.
     EVALUATE  R002
         WHEN  1
               IF   SONZAI-F = 1  AND SONZAI-F2 = ZERO
                    CONTINUE
               ELSE
                    MOVE   MSG08  TO   W034
                    MOVE   "R"    TO
                                  EDIT-OPTION OF R002
                                  EDIT-OPTION OF R005
                                  EDIT-OPTION OF R006
                    MOVE   1      TO   ERR-FLG
                    GO            TO   NYU-READ7-EXIT
               END-IF
         WHEN  2
               IF   SONZAI-F = 1  AND SONZAI-F2 = 1
                    CONTINUE
               ELSE
                    MOVE   MSG08  TO   W034
                    MOVE   "R"    TO
                                  EDIT-OPTION OF R002
                                  EDIT-OPTION OF R005
                                  EDIT-OPTION OF R006
                    MOVE   1      TO   ERR-FLG
                    GO            TO   NYU-READ7-EXIT
               END-IF
     END-EVALUATE.
 NYU-READ7-01.
     MOVE   R005    TO   NYU-F02.
     MOVE   R006    TO   NYU-F03.
     MOVE   R002    TO   NYU-F04.
     MOVE   1       TO   NYU-F05.
     START   ZNYUKDT   KEY IS >= NYU-F02  NYU-F03
                                 NYU-F04  NYU-F05
             INVALID   DISPLAY  "*E  ZNYUKDT  READ  INVALID "
                       "NYU-F02 = " NYU-F02 " NYU-F03 = " NYU-F03
                       " NYU-F04 = " NYU-F04  UPON  STAT
                       STOP  RUN
     END-START.
     READ   ZNYUKDT    NEXT
            AT   END   DISPLAY  "*E  ZNYUKDT  READ  INVALID "
                       "NYU-F02 = " NYU-F02 " NYU-F03 = " NYU-F03
                       " NYU-F04 = " NYU-F04  UPON  STAT
                       STOP  RUN
     END-READ.
 NYU-READ7-EXIT.
*--------------------------------------------------------------*
*    LEVEL  4          画面ＷＲＩＴＥ
*--------------------------------------------------------------*
 DSP-SEC           SECTION.
     IF   R001 = 1  AND  R002 = 0
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
     IF   WK-WKSTN = 01
          MOVE   HAC-F35    TO   R003
     END-IF.
     MOVE   HAC-F01      TO   W002.
     MOVE   SYS-DATE     TO   R004.
     MOVE   HAC-F13      TO   W004.
     MOVE   HAC-F31      TO   W005.
     MOVE   HAC-F12(3:6) TO   W007.
     MOVE   HAC-F07      TO   W008.
     MOVE   HAC-F33      TO   W009.
     MOVE   HAC-F34      TO   W010.
     MOVE   HAC-F08      TO   W011.
     MOVE   HAC-F16      TO   R007.
     MOVE   HAC-F17      TO   R008.
     MOVE   HAC-F15      TO   W013.
     MOVE   HAC-F18      TO   W014.
*****DISPLAY HAC-F10  UPON CONS.

     IF     HAC-F18  NOT =    ZERO
            MOVE   HAC-F10      TO   W015
     END-IF.
     IF     HAC-F19  NOT =    ZERO
            MOVE   HAC-F19      TO   W017
     END-IF.
     EVALUATE   HAC-F01
         WHEN   50     MOVE   NC"仕入"  TO  W003
         WHEN   60     MOVE   NC"直送"  TO  W003
     END-EVALUATE.
     MOVE   HAC-F31    TO   SOK-F01.
     PERFORM  SOK-READ.
     MOVE   HAC-F08    TO   SHI-F01.
     PERFORM  SHI-READ.
     MOVE   HAC-F10    TO   TOK-F01.
     IF     HAC-F10    NOT = ZERO
            PERFORM    TOK-READ
     END-IF.
     MOVE   HAC-F10    TO   TEN-F52.
     MOVE   HAC-F19    TO   TEN-F011.
     IF     HAC-F19    NOT = ZERO
            PERFORM  TEN-READ
     END-IF.
     MOVE   HAC-F05    TO   WK-GYO.
     MOVE   W002       TO   HA0-F01.
     MOVE   R005       TO   HA0-F02.
     MOVE   R006       TO   HA0-F03.
     MOVE   R002       TO   HA0-F04.
     MOVE   ZERO       TO   HA0-F05.
     READ   ZHACHDT
            INVALID      MOVE    SPACE   TO   R009  W019
                                              W020  W021
            NOT INVALID  MOVE    HA0-F07 TO   R009
                         MOVE    HA0-F08 TO   W019
                         MOVE    HA0-F09 TO   W020
                         MOVE    HA0-F10 TO   W021
     END-READ.
     MOVE   W002       TO   HA0-F01.
     MOVE   R005       TO   HA0-F02.
     MOVE   R006       TO   HA0-F03.
     MOVE   R002       TO   HA0-F04.
     MOVE   80         TO   HA0-F05.
     READ   ZHACHDT
            INVALID      MOVE    ZERO    TO   W029
                         MOVE    SPACE   TO   W030  W031
            NOT INVALID  MOVE    HA0-F11 TO   W029
                         MOVE    HA0-F12 TO   W030
                         MOVE    HA0-F13 TO   W031
     END-READ.
     MOVE   W002       TO   HAC-F01.
     MOVE   R005       TO   HAC-F02.
     MOVE   R006       TO   HAC-F03.
     MOVE   R002       TO   HAC-F04.
     MOVE   WK-GYO     TO   HAC-F05.
     READ   ZHACHDT
              INVALID  DISPLAY  "*E  ZHACHDT  READ INVALID  "
                       "HAC-F01 = " W002 " HAC-F02 = " R005
                       " HAC-F03 = " HAC-F03  UPON STAT
                       STOP  RUN
     END-READ.
     MOVE   ZERO         TO   END-FLG.
     PERFORM   VARYING   I  FROM  1  BY  1  UNTIL  I > 6  OR
                                     END-FLG  =  9
        MOVE    1             TO    IN-FLG(HAC-F05)
        MOVE   "1"            TO    R010(HAC-F05)
        MOVE    HAC-F20       TO    W022(HAC-F05)
        MOVE    HAC-F21(1:5)  TO    W023(HAC-F05)
        MOVE    HAC-F21(6:2)  TO    W024(HAC-F05)
        MOVE    HAC-F21(8:1)  TO    W025(HAC-F05)
*93.10.18 START
        MOVE    HAC-F45          TO  WK-HAC-F45(HAC-F05)
        MOVE    HAC-F20          TO  WK-HAC-F20(HAC-F05)
        MOVE    HAC-F21          TO  WK-HAC-F21(HAC-F05)
        MOVE    HAC-F22          TO  WK-HAC-F22(HAC-F05)
        MOVE    HAC-F23          TO  WK-HAC-F23(HAC-F05)
        MOVE    HAC-F30          TO  WK-HAC-F30(HAC-F05)
        MOVE    HAC-F31          TO  WK-HAC-F31(HAC-F05)
*93.10.18 END
        COMPUTE   WK-R011(HAC-F05)     =     HAC-F22 -  HAC-F23
        COMPUTE   WK-R011X(HAC-F05)    =     HAC-F22 -  HAC-F23
        IF    WK-R011(HAC-F05)  >=  0
              COMPUTE   R011(HAC-F05)  =     HAC-F22 -  HAC-F23
        ELSE
              MOVE      ZERO           TO    R011(HAC-F05)
        END-IF
        IF    WK-WKSTN = 01
              MOVE   HAC-F24   TO  R012(HAC-F05)
                                   WK-R012(HAC-F05)
              MOVE   HAC-F25   TO  R013(HAC-F05)
                                   WK-R013(HAC-F05)
              EVALUATE  HAC-F24
                  WHEN  "1"
                        COMPUTE  WK-SIIRE  =
                                 R011(HAC-F05) * R013(HAC-F05)
                        COMPUTE  WK-SIIREG =
                                 WK-SIIREG + WK-SIIRE
                  WHEN  SPACE
                        COMPUTE  WK-SIIRE  =
                                 R011(HAC-F05) * R013(HAC-F05)
                        COMPUTE  WK-SIIREG =
                                 WK-SIIREG + WK-SIIRE
                  WHEN  "3"
                        COMPUTE  WK-SIIREG =
                                 WK-SIIREG + R013(HAC-F05)
              END-EVALUATE
        END-IF
        MOVE    HAC-F26          TO  R014(HAC-F05)
                                     WK-R014(HAC-F05)
        MOVE    HAC-F27          TO  R015(HAC-F05)
                                     WK-R015(HAC-F05)
        EVALUATE  HAC-F26
            WHEN  "1"
                  COMPUTE  WK-GENKA  =
                           R011(HAC-F05) * R015(HAC-F05)
                  COMPUTE  WK-GENKAG =
                           WK-GENKAG + WK-GENKA
            WHEN  SPACE
                  COMPUTE  WK-GENKA  =
                           R011(HAC-F05) * R015(HAC-F05)
                  COMPUTE  WK-GENKAG =
                           WK-GENKAG + WK-GENKA
            WHEN  "3"
                  COMPUTE  WK-GENKAG =
                           WK-GENKAG + R015(HAC-F05)
        END-EVALUATE
        MOVE    HAC-F28          TO  W026(HAC-F05)
        MOVE    HAC-F29          TO  W028(HAC-F05)
        MOVE    HAC-F20          TO  MEI-F011
        MOVE    HAC-F21          TO  MEI-F012
        READ    HMEIMS
                INVALID      MOVE ALL NC"＊" TO  W027(HAC-F05)
                NOT INVALID  MOVE MEI-F02    TO  W027(HAC-F05)
        END-READ
        PERFORM  HAC-READ2
     END-PERFORM.
     IF  WK-WKSTN = 01
         MOVE   WK-SIIREG        TO  W032
     END-IF.
     MOVE       WK-GENKAG        TO  W033.
 TBL-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          発注ファイルＲＥＡＤ（表示時）
*--------------------------------------------------------------*
 HAC-READ2          SECTION.
     READ   ZHACHDT   NEXT
            AT  END   MOVE   9     TO  END-FLG
                      GO           TO  HAC-READ2-EXIT
     END-READ.
     IF     HAC-F02  =  R005 AND
            HAC-F03  =  R006  AND  HAC-F04  =  R002
            IF  HAC-F05 > 6
                MOVE    9      TO  END-FLG
                GO      TO         HAC-READ2-EXIT
            END-IF
            IF  HAC-F06 NOT = 0
                GO      TO         HAC-READ2
            ELSE
                CONTINUE
            END-IF
     ELSE
            MOVE    9       TO    END-FLG
     END-IF.
  HAC-READ2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5          画面セット（入庫ファイルより）
*--------------------------------------------------------------*
 TBL-SET2           SECTION.
     IF   WK-WKSTN = 01
          MOVE   NYU-F27    TO   R003
     END-IF.
     IF   PF-FLG = 1
          MOVE   NYU-F02    TO   R005
          MOVE   NYU-F03    TO   R006
          MOVE   NYU-F04    TO   R002
     END-IF.
     MOVE   NYU-F04      TO   WK-SOUSAI.
     MOVE   NYU-F01      TO   W002.
*****MOVE   SYS-DATE     TO   R004.
     MOVE   NYU-F23      TO   R004.
     MOVE   NYU-F24      TO   W005.
     MOVE   NYU-F29(3:6) TO   W007.
     MOVE   NYU-F06      TO   W008.
     MOVE   NYU-F25      TO   W009.
     MOVE   NYU-F26      TO   W010.
     MOVE   NYU-F07      TO   W011.
     MOVE   NYU-F10      TO   R007.
     MOVE   NYU-F11      TO   R008.
     MOVE   NYU-F09      TO   W013.
     MOVE   SPACE        TO   W014.
     IF     NYU-F08  NOT =    ZERO
            MOVE   NYU-F08      TO   W015
     END-IF.
     IF     NYU-F12  NOT =    ZERO
            MOVE   NYU-F12      TO   W017
     END-IF.
     EVALUATE   NYU-F01
         WHEN   50     MOVE   NC"仕入"    TO  W003
         WHEN   51     MOVE   NC"返品"    TO  W003
         WHEN   52     MOVE   NC"仕値引"  TO  W003
         WHEN   55     MOVE   NC"仕値増"  TO  W003
         WHEN   60     MOVE   NC"直送"    TO  W003
         WHEN   61     MOVE   NC"直返品"  TO  W003
         WHEN   70     MOVE   NC"振替"    TO  W003
         WHEN   71     MOVE   NC"振返品"  TO  W003
         WHEN   80     MOVE   NC"経振替"  TO  W003
         WHEN   81     MOVE   NC"経振返"  TO  W003
         WHEN  OTHER   MOVE   ALL NC"＊"  TO  W003
     END-EVALUATE.
     MOVE   NYU-F24    TO   SOK-F01.
     PERFORM  SOK-READ.
     MOVE   NYU-F07    TO   SHI-F01.
     PERFORM  SHI-READ.
     MOVE   NYU-F08    TO   TOK-F01.
     PERFORM  TOK-READ.
     MOVE   NYU-F08    TO   TEN-F52.
     MOVE   NYU-F12    TO   TEN-F011.
     PERFORM  TEN-READ.
     MOVE   R005       TO   HA0-F02.
     MOVE   R006       TO   HA0-F03.
     MOVE   R002       TO   HA0-F04.
     MOVE   ZERO       TO   HA0-F05.
     READ   ZHACHDT
            INVALID      MOVE    SPACE   TO   R009  W019
                                              W020  W021
            NOT INVALID  MOVE    HA0-F07 TO   R009
                         MOVE    HA0-F08 TO   W019
                         MOVE    HA0-F09 TO   W020
                         MOVE    HA0-F10 TO   W021
     END-READ.
     MOVE   R005       TO   HA0-F02.
     MOVE   R006       TO   HA0-F03.
     MOVE   R002       TO   HA0-F04.
     MOVE   80         TO   HA0-F05.
     READ   ZHACHDT
            INVALID      MOVE    ZERO    TO   W029
                         MOVE    SPACE   TO   W030  W031
            NOT INVALID  MOVE    HA0-F11 TO   W029
                         MOVE    HA0-F12 TO   W030
                         MOVE    HA0-F13 TO   W031
     END-READ.
     MOVE      ZERO     TO  END-FLG.
     PERFORM   VARYING   I  FROM  1  BY  1  UNTIL  I > 6  OR
                                     END-FLG  =  9
        MOVE    1             TO    IN-FLG(NYU-F05)
********MOVE    SPACE         TO    R010(NYU-F05)
        MOVE    NYU-F13       TO    W022(NYU-F05)
        MOVE    NYU-F14(1:5)  TO    W023(NYU-F05)
        MOVE    NYU-F14(6:2)  TO    W024(NYU-F05)
        MOVE    NYU-F14(8:1)  TO    W025(NYU-F05)
        MOVE    NYU-F15       TO    R011(NYU-F05)
                                    WK-R011(NYU-F05)
*93.10.18 START
        MOVE    NYU-F13       TO    WK-HAC-F20(NYU-F05)
        MOVE    NYU-F14       TO    WK-HAC-F21(NYU-F05)
*完了区分を索引する
        MOVE    ZERO          TO    MST-INV
        MOVE    NYU-F02       TO    HAC-F02
        MOVE    ZERO          TO    HAC-F03
        MOVE    ZERO          TO    HAC-F04
        MOVE    NYU-F05       TO    HAC-F05
        READ    ZHACHDT
             INVALID
                 MOVE    1    TO    MST-INV
        END-READ
*
        MOVE    HAC-F06       TO    R010(NYU-F05)
        MOVE    HAC-F45       TO    WK-HAC-F45(NYU-F05)
        MOVE    HAC-F22       TO    WK-HAC-F22(NYU-F05)
        MOVE    HAC-F23       TO    WK-HAC-F23(NYU-F05)
        MOVE    HAC-F30       TO    WK-HAC-F30(NYU-F05)
        MOVE    HAC-F31       TO    WK-HAC-F31(NYU-F05)
*93.10.18 END
*
        IF      WK-WKSTN = 01
*\\     93.06.07 START \\\
*       入庫ファイルの仕入金額が　ゼロの時
*       発注ファイルからセットする　
*94.06.22 STR
************ IF NYU-F17       =     ZERO
***********  DISPLAY "NYU-F17= " NYU-F17   UPON CONS
***********  DISPLAY "NYU-F16= " NYU-F16   UPON CONS
***********  DISPLAY "HAC-F24= " HAC-F24   UPON CONS
***********  DISPLAY "HAC-F25= " HAC-F25   UPON CONS
***********  DISPLAY "MST-INV= " MST-INV   UPON CONS
             IF ( NYU-F17     =   ZERO ) AND
                ( R002        NOT =  1 )
*94.06.22 END
                IF     MST-INV   =  ZERO
                         MOVE    HAC-F24 TO   R012(NYU-F05)
                                              WK-R012(NYU-F05)
* 94/06/18 START *
**94/06/22************IF R002    NOT =     2
                         MOVE    HAC-F25 TO   R013(NYU-F05)
                                              WK-R013(NYU-F05)
**94/06/22************END-IF
* 94/06/18 END   *
                END-IF
*
*************** MOVE   NYU-F02    TO   HAC-F02
*************** MOVE   ZERO       TO   HAC-F03
*************** MOVE   ZERO       TO   HAC-F04
*************** MOVE   NYU-F05    TO   HAC-F05
*************** READ   ZHACHDT
***************        INVALID
***************          CONTINUE
*************** NOT INVALID
***************          MOVE    HAC-F24 TO   R012(NYU-F05)
***************                               WK-R012(NYU-F05)
***************          MOVE    HAC-F25 TO   R013(NYU-F05)
***************                               WK-R013(NYU-F05)
*************** END-READ
             ELSE

                MOVE   NYU-F16   TO  R012(NYU-F05)
                                     WK-R012(NYU-F05)
                MOVE   NYU-F17   TO  R013(NYU-F05)
                                     WK-R013(NYU-F05)
             END-IF
*\\     93.06.07 END   \\\
****************EVALUATE  NYU-F16
                EVALUATE  R012(NYU-F05)
                    WHEN  "1"
                          COMPUTE  WK-SIIRE  =
                                   R011(NYU-F05) * R013(NYU-F05)
                          COMPUTE  WK-SIIREG =
                                   WK-SIIREG + WK-SIIRE
                    WHEN  SPACE
                          COMPUTE  WK-SIIRE  =
                                   R011(NYU-F05) * R013(NYU-F05)
                          COMPUTE  WK-SIIREG =
                                   WK-SIIREG + WK-SIIRE
                    WHEN  "3"
                          COMPUTE  WK-SIIREG =
                                   WK-SIIREG + R013(NYU-F05)
                END-EVALUATE
        END-IF
        MOVE    NYU-F18          TO  R014(NYU-F05)
                                     WK-R014(NYU-F05)
        MOVE    NYU-F19          TO  R015(NYU-F05)
                                     WK-R015(NYU-F05)
        EVALUATE  NYU-F18
            WHEN  "1"
                  COMPUTE  WK-GENKA  =
                           R011(NYU-F05) * R015(NYU-F05)
                  COMPUTE  WK-GENKAG =
                           WK-GENKAG + WK-GENKA
            WHEN  SPACE
                  COMPUTE  WK-GENKA  =
                           R011(NYU-F05) * R015(NYU-F05)
                  COMPUTE  WK-GENKAG =
                           WK-GENKAG + WK-GENKA
            WHEN  "3"
                  COMPUTE  WK-GENKAG =
                           WK-GENKAG + R015(NYU-F05)
        END-EVALUATE
        MOVE    NYU-F20          TO  W026(NYU-F05)
        MOVE    NYU-F21          TO  W028(NYU-F05)
        MOVE    NYU-F13          TO  MEI-F011
        MOVE    NYU-F14          TO  MEI-F012
        READ    HMEIMS
                INVALID      MOVE   ALL NC"＊" TO  W027(NYU-F05)
                NOT INVALID  MOVE   MEI-F02    TO  W027(NYU-F05)
        END-READ
        PERFORM  NYU-READ4
     END-PERFORM.
     IF  WK-WKSTN = 01
         MOVE   WK-SIIREG        TO  W032
     END-IF.
     MOVE       WK-GENKAG        TO  W033.
     MOVE       R002             TO  WK-R002.
     MOVE       R005             TO  WK-R005.
     MOVE       R006             TO  WK-R006.
 TBL-SET2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6          入庫ファイルＲＥＡＤ（表示時）
*--------------------------------------------------------------*
 NYU-READ4          SECTION.
     READ   ZNYUKDT   NEXT
            AT   END    MOVE   9   TO   END-FLG
                        GO         TO   NYU-READ4-EXIT
     END-READ.
     IF     R001     =  1     AND  R002     =  1  OR  2
            IF     NYU-F01  =  W002  AND  NYU-F02  =  R005 AND
                   NYU-F03  =  R006  AND  NYU-F04  =  WK-SOUSAI
                   CONTINUE
            ELSE
                   MOVE    9       TO    END-FLG
            END-IF
     ELSE
            IF     NYU-F01  =  W002  AND  NYU-F02  =  R005 AND
                   NYU-F03  =  R006  AND  NYU-F04  =  R002
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
            INVALID      MOVE   ALL NC"＊"  TO  W006
            NOT INVALID  MOVE   SOK-F02     TO  W006
     END-READ.
 SOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6      仕入先マスタＲＥＡＤ                        *
*--------------------------------------------------------------*
 SHI-READ           SECTION.
     READ   ZSHIMS
            INVALID      MOVE   ALL NC"＊"  TO  W012
                         MOVE   1           TO  MST-INV
            NOT INVALID  MOVE   SHI-F02     TO  W012
                         MOVE   ZERO        TO  MST-INV
     END-READ.
 SHI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6      得意先マスタＲＥＡＤ                        *
*--------------------------------------------------------------*
 TOK-READ           SECTION.
     READ   HTOKMS
            INVALID      MOVE   SPACE       TO  W016
            NOT INVALID  MOVE   TOK-F02     TO  W016
     END-READ.
 TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6      店舗マスタＲＥＡＤ                          *
*--------------------------------------------------------------*
 TEN-READ           SECTION.
     READ   HTENMS
            INVALID      MOVE   SPACE       TO  W018
            NOT INVALID  MOVE   TEN-F02     TO  W018
     END-READ.
 TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3          ＧＨＥＡＤ１　入力  (GR-NO = 3)
*--------------------------------------------------------------*
 DSP-HEAD1-RTN     SECTION.
     IF   WK-WKSTN NOT = 01
          MOVE   "X"    TO   EDIT-STATUS  OF  R003
*93.10.18 START
          MOVE   "X"    TO   EDIT-STATUS  OF  W011
     END-IF.
     MOVE     G002           TO   W035.
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
              MOVE      MSG01     TO   W034
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
     IF   WK-WKSTN = 01
          IF   R003  NOT NUMERIC
               MOVE  "R"     TO   EDIT-OPTION OF R003
               MOVE  "C"     TO   EDIT-CURSOR OF R003
               MOVE  MSG04   TO   W034
               MOVE   30     TO   ERR-FLG
          END-IF
          IF   R003  = 1  OR  0
               CONTINUE
          ELSE
               MOVE  "R"     TO   EDIT-OPTION OF R003
               MOVE  "C"     TO   EDIT-CURSOR OF R003
               MOVE  MSG04   TO   W034
               MOVE   30     TO   ERR-FLG
          END-IF
     END-IF.
*日付チェック
     PERFORM  DATA-CHK.
*送料区分チェック
     IF   R007    = 0  OR   8  OR  9
          CONTINUE
     ELSE
          MOVE   "R"   TO   EDIT-OPTION  OF  R007
          IF   ERR-FLG  =  ZERO
               MOVE   "C"     TO   EDIT-CURSOR OF R007
               MOVE    MSG06  TO  W034
               MOVE    30     TO  ERR-FLG
          END-IF
     END-IF.
*送料チェック
     IF   R008   NOT NUMERIC
          MOVE   "R"   TO   EDIT-OPTION  OF  R008
          IF   ERR-FLG  =  ZERO
               MOVE   "C"     TO   EDIT-CURSOR OF R008
               MOVE    MSG07  TO   W034
               MOVE    30     TO   ERR-FLG
          END-IF
     END-IF.
*93.10.18 START
*仕入先コード入力＆コードチェック
     MOVE    W011      TO   SHI-F01.
     PERFORM SHI-READ.
     IF      MST-INV   =    1
          MOVE   "R"   TO   EDIT-OPTION  OF  W011
          IF   ERR-FLG  =  ZERO
               MOVE   "C"     TO   EDIT-CURSOR OF W011
               MOVE    MSG22  TO  W034
               MOVE    30     TO  ERR-FLG
          END-IF.
*
 CHK-1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5          納入日チェック
*--------------------------------------------------------------*
 DATA-CHK                 SECTION.
***  MOVE  R004       TO   WK-DATE.
*    IF    WK-MM      >=  1   AND   WK-MM      <=  12
*          IF    WK-MM       =  2
*                DIVIDE  4   INTO    WK-MM
*                            GIVING      WK-SHO
*                            REMAINDER   WK-AMARI
*                IF   WK-AMARI  =  ZERO
*                     MOVE   29    TO   WK-MATUBI
*                ELSE
*                     MOVE   28    TO   WK-MATUBI
*                END-IF
*          ELSE
*                IF   R002  =  4  OR  6  OR  9  OR  11
*                     MOVE   30    TO   WK-MATUBI
*                ELSE
*                     MOVE   31    TO   WK-MATUBI
*                END-IF
*          END-IF
*          IF    WK-DD       >=  1      AND
*                WK-DD       <=  WK-MATUBI
*                CONTINUE
*          ELSE
*                MOVE   "R"        TO   EDIT-OPTION OF R004
*                IF    ERR-FLG   = ZERO
*                      MOVE    "C"       TO   EDIT-CURSOR OF R004
*                      MOVE     MSG05    TO   W034
*                      MOVE     2        TO   ERR-FLG
*                END-IF
*          END-IF
*    ELSE
*                MOVE   "R"        TO   EDIT-OPTION OF R004
*                IF    ERR-FLG   = ZERO
*                      MOVE    "C"       TO   EDIT-CURSOR OF R004
*                      MOVE     MSG05    TO   W034
*                      MOVE     2        TO   ERR-FLG
*                END-IF
***  END-IF.
     MOVE    "1"        TO        LINK-IN-KBN.
     MOVE     R004      TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              CONTINUE
     ELSE
              MOVE   "R"        TO   EDIT-OPTION OF R004
              IF    ERR-FLG   = ZERO
                    MOVE    "C"       TO   EDIT-CURSOR OF R004
                    MOVE     MSG05    TO   W034
                    MOVE     2        TO   ERR-FLG
              END-IF
     END-IF.
*
*****MOVE  R004    TO        WK-R004.
     MOVE  R004    TO        WK-R004X WK-HEN-DATE1.
     IF    WK-R0041X   >  89
           COMPUTE  WK-HEN-DATE2 =  19000000 + WK-HEN-DATE1
     ELSE
           COMPUTE  WK-HEN-DATE2 =  20000000 + WK-HEN-DATE1
     END-IF.
     MOVE  WK-HEN-DATE2      TO   WK-R004.
*****DISPLAY "WK-R0044 = " WK-R0044 UPON CONS.
*****DISPLAY "WK-SIME4 = " WK-SIME4 UPON CONS.
     IF    WK-R0044             <        WK-SIME4
           MOVE   "R"        TO   EDIT-OPTION OF R004
           IF    ERR-FLG   = ZERO
                 MOVE    "C"       TO   EDIT-CURSOR OF R004
                 MOVE     MSG21    TO   W034
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
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I > 6
*93.10.18 START
         IF   WK-WKSTN  NOT = 01
              MOVE   "X"    TO   EDIT-STATUS  OF  W022(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  W023(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  W024(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  W025(I)
         END-IF
*93.10.18 END
         IF   IN-FLG(I) NOT = 1
              MOVE   "X"    TO   EDIT-STATUS  OF  R010(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  R011(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  R012(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  R013(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  R014(I)
              MOVE   "X"    TO   EDIT-STATUS  OF  R015(I)
         ELSE
              IF  WK-WKSTN NOT = 01
                  MOVE   "X"    TO   EDIT-STATUS  OF  R012(I)
                  MOVE   "X"    TO   EDIT-STATUS  OF  R013(I)
              END-IF
              IF  R001 NOT = 1
*93.10.18 START
*仕入伝票以外は対象外
                  IF   W002     NOT = 50  AND  60
                       MOVE   "X"    TO   EDIT-STATUS
                                          OF  R010(I)
                  END-IF
*93.10.18 END
              END-IF
         END-IF
     END-PERFORM.
     MOVE     G002           TO   W035.
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
                   MOVE      5         TO   GR-NO
                   MOVE      "Y"       TO   R016
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              MOVE      MSG01     TO   W034
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
              IF  R001 = 1  AND R002 = 0
                  IF   R010(I)  = 0  OR  1  OR  9
                       CONTINUE
                  ELSE
                       MOVE   "R"   TO  EDIT-OPTION OF R010(I)
                       IF  ERR-FLG = ZERO
                           MOVE  "C"    TO
                                         EDIT-CURSOR OF R010(I)
                           MOVE   MSG10 TO  W034
                           MOVE   1     TO  ERR-FLG
                       END-IF
                  END-IF
              END-IF
*93.10.18 START
*既に商品ＣＤの変更が行われている場合変更はできない
*93.10.19 TEST
*** 商品コード右詰処理  ***
                  IF   W022(I)      NOT = SPACE
                       PERFORM VARYING IXB FROM 1 BY 1
                               UNTIL   IXB    >     7
                          PERFORM VARYING IXC FROM 8 BY -1
                               UNTIL   IXC    <     2
                            IF W022  (I)(IXC:1)  =  SPACE
                               COMPUTE IXD   =   IXC   -   1
                               MOVE  W022  (I)(IXD:1)     TO
                                     W022  (I)(IXC:1)
                               MOVE  SPACE                TO
                                     W022  (I)(IXD:1)
                            END-IF
                          END-PERFORM
                       END-PERFORM
**
                       PERFORM VARYING IXB FROM 1 BY 1
                               UNTIL  (IXB   >      7 ) OR
                              (W022 (I)  (IXB:1) NOT =  SPACE)
                               IF W022 (I)  (IXB:1)  =  SPACE
                                  MOVE       "0"     TO
                                             W022(I)(IXB:1)
                               END-IF
                       END-PERFORM
                  END-IF
******************************
**************DISPLAY  WK-HAC-F45(I)   UPON CONS
              IF  WK-HAC-F45(I)   =         "1"
                  IF (WK-HAC-F20(I)  NOT =  W022(I)) OR
                     (WK-HAC-F211(I) NOT =  W023(I)) OR
                     (WK-HAC-F212(I) NOT =  W024(I)) OR
                     (WK-HAC-F213(I) NOT =  W025(I))
                       MOVE   "R"   TO  EDIT-OPTION OF W022(I)
                                        EDIT-OPTION OF W023(I)
                                        EDIT-OPTION OF W024(I)
                                        EDIT-OPTION OF W025(I)
                       MOVE   "C"   TO  EDIT-CURSOR OF W022(I)
                                        EDIT-CURSOR OF W023(I)
                                        EDIT-CURSOR OF W024(I)
                                        EDIT-CURSOR OF W025(I)
                       IF  ERR-FLG  NOT =   1
                           MOVE   MSG24 TO  W034
                       END-IF
                       MOVE     1   TO  ERR-FLG
                  END-IF
              ELSE
                  IF (WK-HAC-F20(I)  NOT =  W022(I)) OR
                     (WK-HAC-F211(I) NOT =  W023(I)) OR
                     (WK-HAC-F212(I) NOT =  W024(I)) OR
                     (WK-HAC-F213(I) NOT =  W025(I))
                      MOVE  "1"  TO     WK-HAC-F451(I)
*新しい商品ＣＤで_番を索引
                      IF      W015      NOT =     ZERO
                              MOVE    W015        TO   SHO-F01
                              MOVE    W022(I)     TO   SHO-F031
                              MOVE    W023(I)     TO   SHO-F0321
                              MOVE    W024(I)     TO   SHO-F0322
                              MOVE    W025(I)     TO   SHO-F0323
                              READ    HSHOTBL
                               INVALID     KEY
                                MOVE SPACE     TO   WK-HAC-F301(I)
                               NOT INVALID KEY
                                MOVE SHO-F08   TO   WK-HAC-F301(I)
                              END-READ
                      ELSE
                           MOVE    SPACE       TO   WK-HAC-F301(I)
                      END-IF
*
                  ELSE
                      MOVE  SPACE TO    WK-HAC-F451(I)
                  END-IF
              END-IF
*
*商品ＣＤチェック
              IF       WK-HAC-F451(I)   = "1"
                  MOVE     W022(I)   TO     MEI-F011
                  MOVE     W023(I)   TO     MEI-F012(1:5)
                  MOVE     W024(I)   TO     MEI-F012(6:2)
                  MOVE     W025(I)   TO     MEI-F012(8:1)
                  MOVE     ZERO      TO     MST-INV
                  READ    HMEIMS
                    INVALID
                          MOVE   1   TO     MST-INV
                    NOT INVALID
                          MOVE MEI-F02  TO  W027(I)
                          MOVE   "1" TO     WK-HAC-F451(I)
                  END-READ
                  IF       MST-INV   =      1
                           MOVE   "R"   TO  EDIT-OPTION OF W022(I)
                                            EDIT-OPTION OF W023(I)
                                            EDIT-OPTION OF W024(I)
                                            EDIT-OPTION OF W025(I)
                           MOVE   "C"   TO  EDIT-CURSOR OF W022(I)
                                            EDIT-CURSOR OF W023(I)
                                            EDIT-CURSOR OF W024(I)
                                            EDIT-CURSOR OF W025(I)
                           IF  ERR-FLG  NOT =   1
                               MOVE   MSG23 TO  W034
                           END-IF
                           MOVE     1   TO  ERR-FLG
                  END-IF
              END-IF
*
*数量チェック
              IF  R011(I)  NOT NUMERIC
                  MOVE   "R"    TO    EDIT-OPTION  OF  R011(I)
                  IF  ERR-FLG = ZERO
                      MOVE   "C"      TO
                                         EDIT-CURSOR OF R011(I)
                      MOVE    MSG12   TO    W034
                      MOVE    1       TO    ERR-FLG
                  END-IF
              END-IF
*単価区分（仕入単価）チェック
              IF  WK-WKSTN = 01
                  IF  R012(I) = SPACE  OR "1" OR "3" OR "4"  OR
                             "5" OR "7" OR "9"
                      IF  R012(I) = SPACE
                          MOVE   W022(I)   TO    MEI-F011
                          MOVE   W023(I)   TO    MEI-F012(1:5)
                          MOVE   W024(I)   TO    MEI-F012(6:2)
                          MOVE   W025(I)   TO    MEI-F012(8:1)
                          READ   HMEIMS
                                 INVALID
                                     MOVE  ZERO      TO  R013(I)
                                 NOT INVALID
                                     MOVE  MEI-F041  TO  R013(I)
                          END-READ
                      END-IF
                      IF  R012(I) = "5"  OR  "6"  OR  "7"
                          MOVE   ZERO      TO    R013(I)
                      END-IF
                  ELSE
                      MOVE  "R"   TO   EDIT-OPTION OF R012(I)
                      IF  ERR-FLG = ZERO
                          MOVE  "C"      TO
                                         EDIT-CURSOR OF R012(I)
                          MOVE   MSG11   TO  W034
                          MOVE   1       TO  ERR-FLG
                      END-IF
                  END-IF
*仕単／金額チェック
                  IF  R013(I)  NOT NUMERIC
                      MOVE   "R"   TO   EDIT-OPTION  OF  R013(I)
                      IF  ERR-FLG = ZERO
                          MOVE  "C"       TO
                                           EDIT-CURSOR OF R013(I)
                          MOVE   MSG13    TO   W034
                          MOVE   1        TO   ERR-FLG
                      END-IF
                  END-IF
              END-IF
*単価区分（原価単価）チェック
              IF  R014(I) = SPACE  OR "1" OR "3" OR "4"  OR
                         "5" OR "7" OR "9"
                  IF  R014(I) = SPACE
                      MOVE   W022(I)   TO    MEI-F011
                      MOVE   W023(I)   TO    MEI-F012(1:5)
                      MOVE   W024(I)   TO    MEI-F012(6:2)
                      MOVE   W025(I)   TO    MEI-F012(8:1)
                      READ   HMEIMS
                             INVALID
                                 MOVE  ZERO      TO  R015(I)
                             NOT INVALID
                                 MOVE  MEI-F042  TO  R015(I)
                      END-READ
                  END-IF
                  IF  R014(I) = "5"  OR  "6"  OR  "7"
                      MOVE   ZERO      TO    R015(I)
                  END-IF
              ELSE
                  MOVE  "R"   TO   EDIT-OPTION OF R014(I)
                  IF  ERR-FLG = ZERO
                      MOVE  "C"      TO   EDIT-CURSOR OF R014(I)
                      MOVE   MSG11   TO  W034
                      MOVE   1       TO  ERR-FLG
                  END-IF
              END-IF
*原単／金額チェック
              IF  R015(I)  NOT NUMERIC
                  MOVE   "R"   TO   EDIT-OPTION  OF  R015(I)
                  IF  ERR-FLG = ZERO
                      MOVE  "C"       TO   EDIT-CURSOR OF R015(I)
                      MOVE   MSG14    TO   W034
                      MOVE   1        TO   ERR-FLG
                  END-IF
              END-IF
          END-IF
     END-PERFORM.
 CHK-2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4          合計算出
*--------------------------------------------------------------*
 TOT-SEC          SECTION.
     MOVE      ZERO        TO    WK-TAIHI.
     PERFORM   VARYING  I  FROM  1  BY  1  UNTIL I  > 6
          IF  IN-FLG(I) = 1  AND    R010(I) NOT =  9
              IF  WK-WKSTN = 01
                  EVALUATE  R012(I)
                      WHEN  "1"
                            COMPUTE  WK-SIIRE  =
                                         R011(I) * R013(I)
                            COMPUTE  WK-SIIREG =
                                         WK-SIIREG + WK-SIIRE
                      WHEN  SPACE
                            COMPUTE  WK-SIIRE  =
                                         R011(I) * R013(I)
                            COMPUTE  WK-SIIREG =
                                         WK-SIIREG + WK-SIIRE
                      WHEN  "3"
                            COMPUTE  WK-SIIREG =
                                         WK-SIIREG + R013(I)
                  END-EVALUATE
              END-IF
              EVALUATE  R014(I)
                  WHEN  "1"
                        COMPUTE  WK-GENKA  =
                                     R011(I) * R015(I)
                        COMPUTE  WK-GENKAG =
                                     WK-GENKAG + WK-GENKA
                  WHEN  SPACE
                        COMPUTE  WK-GENKA  =
                                     R011(I) * R015(I)
                        COMPUTE  WK-GENKAG =
                                     WK-GENKAG + WK-GENKA
                  WHEN  "3"
                        COMPUTE  WK-GENKAG =
                                     WK-GENKAG + R015(I)
              END-EVALUATE
          END-IF
     END-PERFORM.
     IF   WK-WKSTN  =  01
          MOVE     WK-SIIREG      TO   W032
     END-IF.
     MOVE          WK-GENKAG      TO   W033.
 TOT-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3          確認入力      (GR-NO = 5)
*--------------------------------------------------------------*
 DSP-KAKU-RTN     SECTION.
     MOVE     ZERO           TO   ERR-FLG.
     MOVE      G002          TO   W035.
     MOVE     "KAKU"         TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      6         TO   GR-NO
     WHEN     PF05
              MOVE      99        TO   GR-NO
     WHEN     PF06
              IF    R001   =  3    OR   R001  =  1 AND  R002 = 1
                    PERFORM   DSP-BODY-CLR
                    PERFORM   WK-BODY-CLR
                    MOVE      2         TO   GR-NO
              ELSE
                    MOVE      4         TO   GR-NO
              END-IF
     WHEN     ENT
              IF   R016  =  "H"  AND  R002  =  1
                   MOVE      MSG18     TO   W034
                   MOVE      1         TO   ERR-FLG
                   GO                  TO   DSP-KAKU-EXIT
              END-IF
******        IF   R016  =  "N"
**                 MOVE      SPACE     TO   W034
**                 MOVE      2         TO   GR-NO
**                 GO                  TO   DSP-KAKU-EXIT
******        END-IF
              IF   R016  =  "Y"  OR  "H"
                   IF   ERR-FLG  NOT = ZERO
                        MOVE   SPACE        TO   W034
                        PERFORM   900-DSP-WRITE
                   END-IF
                   EVALUATE  R001
                       WHEN  1      PERFORM    WRITE-SEC
                       WHEN  2      PERFORM    UPDATA-SEC
                       WHEN  3      PERFORM    DELETE-SEC
                   END-EVALUATE
                   IF   R016  =  "Y"
                        IF  R002 = 1  AND  R001  =  1
                            MOVE   2    TO   R002
                            MOVE   2    TO   GR-NO
                            GO          TO   DSP-KAKU-EXIT
                        END-IF
                        MOVE    6       TO   GR-NO
                   ELSE
                        MOVE    2       TO   GR-NO
                        PERFORM         DSP-BODY-CLR
                        PERFORM         WK-BODY-CLR
                   END-IF
              ELSE
                   MOVE      1          TO   ERR-FLG
                   MOVE      MSG15      TO   W034
              END-IF
     WHEN     OTHER
              MOVE      MSG01     TO   W034
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 DSP-KAKU-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  4          登録処理
*--------------------------------------------------------------*
 WRITE-SEC            SECTION.
     MOVE     R005        TO    HAC-F02.
     MOVE     R006        TO    HAC-F03.
     MOVE     ZERO        TO    HAC-F04.
     MOVE     1           TO    HAC-F05.
     READ     ZHACHDT
              INVALID   DISPLAY "*E  ZHACHDT  READ INVALID "
                        HAC-F02 "." HAC-F03 "." HAC-F04 "."
                        HAC-F05     UPON  STAT
     END-READ.
     MOVE     HAC-F38     TO    WK-EDA.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
          IF  IN-FLG(I)  =  1
              IF  R002   =  1  OR  2
                  PERFORM   NYU-WT-SEC
                  PERFORM   ZAI-REWT-SEC
              ELSE
                  EVALUATE   R010(I)
                      WHEN   "1"
                             PERFORM   NYU-WT-SEC
                             PERFORM   HAC-REWT-SEC
                             PERFORM   ZAI-REWT-SEC
                      WHEN   "9"
                             PERFORM   HAC-REWT-SEC
                             PERFORM   ZAI-REWT-SEC
                      WHEN   "0"
                             PERFORM   NYU-WT-SEC
                             PERFORM   HAC-REWT-SEC
                             PERFORM   ZAI-REWT-SEC
                  END-EVALUATE
              END-IF
          END-IF
     END-PERFORM.
     IF   R002  =  ZERO
          MOVE     R005        TO    HAC-F02
          MOVE     R006        TO    HAC-F03
          MOVE     ZERO        TO    HAC-F04
          MOVE     1           TO    HAC-F05
          READ     ZHACHDT
                   INVALID   DISPLAY "*E  ZHACHDT  READ INVALID "
                             HAC-F02 "." HAC-F03 "." HAC-F04 "."
                             HAC-F05     UPON  STAT
          END-READ
          ADD      1           TO    HAC-F38
          MOVE     W011        TO    HAC-F08
          REWRITE  HAC-REC
     END-IF.
 WRITE-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5          入庫ファイルＷＲＩＴＥ
*--------------------------------------------------------------*
 NYU-WT-SEC                 SECTION.
*
     MOVE     SPACE    TO      NYU-REC.
     INITIALIZE                NYU-REC.
     MOVE     W002     TO      NYU-F01.
     MOVE     R005     TO      NYU-F02.
     IF  R001 = 1  AND  R002 = 1  OR  2
         MOVE     R006     TO      NYU-F03
     ELSE
         MOVE     WK-EDA   TO      NYU-F03
     END-IF.
     MOVE     R002     TO      NYU-F04.
     MOVE     I        TO      NYU-F05.
     MOVE     W008     TO      NYU-F06.
     MOVE     W011     TO      NYU-F07.
     IF       W015  NOT NUMERIC
              CONTINUE
     ELSE
              MOVE     W015    TO      NYU-F08
     END-IF.
     MOVE     W013     TO      NYU-F09.
     MOVE     R007     TO      NYU-F10.
     MOVE     R008     TO      NYU-F11.
     IF       W017  NOT NUMERIC
              CONTINUE
     ELSE
              MOVE     W017    TO      NYU-F12
     END-IF.
     MOVE     W022(I)  TO      NYU-F13.
     MOVE     W023(I)  TO      NYU-F14(1:5).
     MOVE     W024(I)  TO      NYU-F14(6:2).
     MOVE     W025(I)  TO      NYU-F14(8:1).
     MOVE     R011(I)  TO      NYU-F15.
     MOVE     R010(I)  TO      NYU-F91.
     MOVE     WK-R011(I)    TO NYU-F31.
     IF   WK-WKSTN = 01
          MOVE   R012(I)      TO    NYU-F16
          MOVE   R013(I)      TO    NYU-F17
          MOVE   R003         TO    NYU-F27
     END-IF.
     MOVE     R014(I)  TO       NYU-F18.
     MOVE     R015(I)  TO       NYU-F19.
     MOVE     W026(I)  TO       NYU-F20.
     IF    R004(1:2)  >=  93
           MOVE  19    TO      NYU-F23(1:2)
     ELSE
           MOVE  20    TO      NYU-F23(1:2)
     END-IF.
     MOVE     R004     TO       NYU-F23(3:6).
     MOVE     W005     TO       NYU-F24.
     MOVE     W009     TO       NYU-F25.
     MOVE     W010     TO       NYU-F26.
*
     MOVE     R005     TO       HAC-F02.
     MOVE     ZERO     TO       HAC-F03.
     MOVE     ZERO     TO       HAC-F04.
     MOVE     I        TO       HAC-F05.
     READ     ZHACHDT
              INVALID
                   MOVE   SPACE    TO  NYU-F21
                   MOVE   SPACE    TO  NYU-F22
                   MOVE   ZERO     TO  NYU-F29
              NOT  INVALID
                   MOVE   HAC-F29  TO  NYU-F21
                   MOVE   HAC-F30  TO  NYU-F22
                   MOVE   HAC-F12  TO  NYU-F29
     END-READ.
     IF    SYS-DATE(1:2)   >=  93
           MOVE  19    TO      NYU-F98(1:2)
     ELSE
           MOVE  20    TO      NYU-F98(1:2)
     END-IF.
     MOVE   SYS-DATE   TO      NYU-F98(3:6).
*****IF     NYU-F15    NOT =   ZERO
            WRITE    NYU-REC.
*****END-IF.
*
     IF  W002 = 52  OR  55
         CONTINUE
     ELSE
         IF  R001 = 01  AND R002 = 1  OR 2
             IF   R002  = 1  AND  W002 = 50
                  COMPUTE  HAC-F23   =   HAC-F23  -  R011(I)
             ELSE
                  COMPUTE  HAC-F23   =   HAC-F23  +  R011(I)
             END-IF
         ELSE
             MOVE     R010(I)     TO    HAC-F06
             ADD      R011(I)     TO    HAC-F23
             IF       SYS-YY     >=  93
                      MOVE  19    TO    HAC-F99(1:2)
             ELSE
                      MOVE  20    TO    HAC-F99(1:2)
             END-IF
             MOVE     SYS-DATE    TO    HAC-F99(3:6)
*93.10.18 START
*入庫数が発注数を越えている時　発注数を入庫数と同じにする
             IF       HAC-F23     >     HAC-F22
                      MOVE              HAC-F23 TO HAC-F22
             END-IF
*完了区分が　完納または取消で
*入庫数が発注数よりも少ない時　発注数を入庫数と同じにする
             IF       R010(I)     =     1 OR 9
                IF    HAC-F22     >     HAC-F23
                      MOVE              HAC-F23 TO HAC-F22
                END-IF
             END-IF
*商品ＣＤが変わっている時
             IF       WK-HAC-F451(I) =  "1"
                      MOVE  W022(I)     TO      HAC-F20
                      MOVE  W023(I)     TO      HAC-F21(1:5)
                      MOVE  W024(I)     TO      HAC-F21(6:2)
                      MOVE  W025(I)     TO      HAC-F21(8:1)
                      MOVE  WK-HAC-F301(I)  TO  HAC-F30
             END-IF
*93.10.18 END
         END-IF
         MOVE     W011      TO   HAC-F08
         REWRITE  HAC-REC
     END-IF.
 NYU-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5          発注ファイルＲＥＷＲＩＴＥ　　　　*
*--------------------------------------------------------------*
 HAC-REWT-SEC         SECTION.
*
     MOVE     R005     TO       HAC-F02.
     MOVE     R006     TO       HAC-F03.
     MOVE     ZERO     TO       HAC-F04.
     MOVE     I        TO       HAC-F05.
     READ     ZHACHDT
              INVALID  DISPLAY  "*E  ZHACHDT  READ INVALID  "
                       "HAC-F01 = " W002 " HAC-F02 = " R005
                       " HAC-F03 = " HAC-F03  UPON STAT
                       STOP  RUN
              NOT  INVALID
                   IF  R010(I)  =  "9"
                       MOVE   "9"         TO  HAC-F06
                       IF   SYS-YY      >=  93
                             MOVE  19    TO      HAC-F99(1:2)
                       ELSE
                             MOVE  20    TO      HAC-F99(1:2)
                       END-IF
                       MOVE    SYS-DATE   TO  HAC-F99(3:6)
                       MOVE  HAC-F31     TO      WK-HAC31
                       MOVE  HAC-F20     TO      WK-HAC20
                       MOVE  HAC-F21     TO      WK-HAC21
                       MOVE  HAC-F30     TO      WK-HAC30
                   ELSE
*****                  COMPUTE  HAC-F38  =  WK-EDA + 1
                       IF    SYS-YY     >=  93
                             MOVE  19    TO      HAC-F99(1:2)
                       ELSE
                             MOVE  20    TO      HAC-F99(1:2)
                       END-IF
                       MOVE  SYS-DATE    TO      HAC-F99(3:6)
                       MOVE  HAC-F31     TO      WK-HAC31
                       MOVE  HAC-F20     TO      WK-HAC20
                       MOVE  HAC-F21     TO      WK-HAC21
                       MOVE  HAC-F30     TO      WK-HAC30
                   END-IF
     END-READ.
*93.10.18 START
     IF       WK-HAC-F451(I) =    "1"
              MOVE     "1"   TO   HAC-F45
     END-IF.
*
*93.10.18 END
*
     MOVE     W011          TO   HAC-F08.
     REWRITE  HAC-REC.
*
 HAC-REWT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      修正処理                                    *
*--------------------------------------------------------------*
 UPDATA-SEC        SECTION.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I > 6
          IF  IN-FLG(I)  =  1
              MOVE    R005   TO   NYU-F02
              MOVE    R006   TO   NYU-F03
              MOVE    R002   TO   NYU-F04
              MOVE    I      TO   NYU-F05
              READ    ZNYUKDT
                      INVALID  DISPLAY "*E  ZNYUKDT READ INVALID"
                               " NYU-F01 = " NYU-F01 " NYU-F02 = "
                               NYU-F02 "NYU-F03 = " NYU-F03
                               UPON  STAT
                               STOP RUN
                      NOT INVALID
                               IF  WK-WKSTN = 01
                                   MOVE  R003    TO   NYU-F27
                                   MOVE  R012(I) TO   NYU-F16
                                   MOVE  R013(I) TO   NYU-F17
                               END-IF
                               IF  R004(1:2)  >=  93
                                   MOVE  19      TO   NYU-F23(1:2)
                               ELSE
                                   MOVE  20      TO   NYU-F23(1:2)
                               END-IF
                               MOVE    R004     TO    NYU-F23(3:6)
                               MOVE    R007      TO   NYU-F10
                               MOVE    R008      TO   NYU-F11
                               MOVE    R011(I)   TO   NYU-F15
                               MOVE    R010(I)   TO   NYU-F91
                               MOVE    R014(I)   TO   NYU-F18
                               MOVE    R015(I)   TO   NYU-F19
                               IF    SYS-DATE(1:2)   >=  93
                                     MOVE  19  TO  NYU-F99(1:2)
                               ELSE
                                     MOVE  20  TO  NYU-F99(1:2)
                               END-IF
                               MOVE  SYS-DATE  TO  NYU-F99(3:6)
              END-READ
              PERFORM   ZAI-REWT-SEC
              COMPUTE   WK-SA    =  WK-R011(I)  -  NYU-F15
*93.10.18 TEST
*
              IF           WK-HAC-F451(I) =  "1"
                           MOVE  W022(I)     TO NYU-F13
                           MOVE  W023(I)     TO NYU-F14(1:5)
                           MOVE  W024(I)     TO NYU-F14(6:2)
                           MOVE  W025(I)     TO NYU-F14(8:1)
                           MOVE   WK-HAC-F301(I) TO NYU-F22
              END-IF
              MOVE      W011     TO    NYU-F07
*
              REWRITE   NYU-REC
*
              IF  W002 = 52 OR 55
                  CONTINUE
              ELSE
                  MOVE    R005     TO     HAC-F02
                  MOVE    ZERO     TO     HAC-F03
                  MOVE    ZERO     TO     HAC-F04
                  MOVE    I        TO     HAC-F05
                  READ    ZHACHDT  INVALID
                          DISPLAY "*E  ZNYUKDT READ INVALID"
                          NYU-F02 "." NYU-F03 "." NYU-F04
                          UPON  STAT
                          STOP RUN
                  END-READ
                  COMPUTE   HAC-F23  =  HAC-F23  -  WK-SA
*93.10.18 START
*入庫数が発注数を越えている時　発注数を入庫数と同じにする
                  IF       HAC-F23     >     HAC-F22
                           MOVE              HAC-F23 TO HAC-F22
                  END-IF
                  MOVE     R010(I)     TO    HAC-F06
*商品ＣＤが変わっている時
                  IF       WK-HAC-F451(I) =  1
                           MOVE  W022(I)     TO HAC-F20
                           MOVE  W023(I)     TO HAC-F21(1:5)
                           MOVE  W024(I)     TO HAC-F21(6:2)
                           MOVE  W025(I)     TO HAC-F21(8:1)
                           MOVE  WK-HAC-F301(I) TO
                                                HAC-F30
                  END-IF
                  MOVE      W011       TO    HAC-F08
*93.10.18 END
                  REWRITE   HAC-REC
              END-IF
          END-IF
     END-PERFORM.
*
     MOVE      R002          TO   WK-R002.
     MOVE      R005          TO   WK-R005.
     MOVE      R006          TO   WK-R006.
*
 UPDATA-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4          削除処理
*--------------------------------------------------------------*
 DELETE-SEC      SECTION.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
          IF  IN-FLG(I) = 1
              MOVE    R005   TO   NYU-F02
              MOVE    R006   TO   NYU-F03
              MOVE    R002   TO   NYU-F04
              MOVE    I      TO   NYU-F05
              READ    ZNYUKDT
                      INVALID  DISPLAY "*E  ZNYUKDT READ INVALID"
                               " NYU-F01 = " NYU-F01 " NYU-F02 = "
                               NYU-F02 "NYU-F03 = " NYU-F03
                               UPON  STAT
                               STOP RUN
              END-READ
              PERFORM   ZAI-REWT-SEC
              DELETE  ZNYUKDT
              IF  W002 = 52 OR 55
                  CONTINUE
              ELSE
                  MOVE    R005   TO   HAC-F02
                  MOVE    ZERO   TO   HAC-F03
                  MOVE    ZERO   TO   HAC-F04
                  MOVE    I      TO   HAC-F05
                  READ    ZHACHDT  INVALID
                             DISPLAY "*E  ZHACHDT READ INVALID"
                             HAC-F02 "." HAC-F03 "." HAC-F04
                             UPON  STAT
                             STOP RUN
                  END-READ
                  IF  R002  =  1  AND    W002   =  50
                      COMPUTE  HAC-F23 = HAC-F23  +  NYU-F15
                  ELSE
                      COMPUTE  HAC-F23 = HAC-F23  -  NYU-F15
                  END-IF
                  IF  SYS-YY  >=  93
                      MOVE    19     TO    HAC-F99(1:2)
                  ELSE
                      MOVE    20     TO    HAC-F99(1:2)
                  END-IF
                  MOVE    SYS-DATE   TO    HAC-F99(3:6)
                  MOVE      W011     TO    HAC-F08
                  REWRITE   HAC-REC
              END-IF
          END-IF
     END-PERFORM.
*
     MOVE      R002          TO   WK-R002.
     MOVE      R005          TO   WK-R005.
     MOVE      R006          TO   WK-R006.
*
 DELETE-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5          商品在庫マスタＲＥＷＲＩＴＥ
*--------------------------------------------------------------*
 ZAI-REWT-SEC          SECTION.
*
*93.10.19 START
     IF  WK-HAC-F451(I)   =    "1"
         PERFORM               ZAI-SHO-CHG
         GO TO                 ZAI-REWT-EXIT
     END-IF.
*93.10.19 END
     PERFORM    ZAI-READ.
     IF  INV-FLG = 1
         MOVE    SPACE    TO   ZAI-REC
         INITIALIZE            ZAI-REC
     END-IF.
***
     EVALUATE   W002
         WHEN   70
         WHEN   80
                PERFORM    DEN-70-SEC
         WHEN   71
         WHEN   81
                PERFORM    DEN-71-SEC
     END-EVALUATE.
*
     IF  W002   =          70 OR 80 OR 71 OR 81
         GO TO             ZAI-REWT-90
     END-IF.
*
     EVALUATE   TRUE
         WHEN  (W002 = 50  AND  R002 = 0  AND  R001 = 1)  OR
               (W002 = 50  AND  R002 = 2  AND  R001 = 1)  OR
               (W002 = 50  AND  R002 = 1  AND  R001 = 3)  OR
               (W002 = 51  AND  R002 = 1  AND  R001 = 1)  OR
               (W002 = 51  AND  R002 = 2  AND  R001 = 3)
                IF      R010(I)   NOT =   9
                  COMPUTE  ZAI-F06  =  ZAI-F06  +  R011(I)
                END-IF
****************COMPUTE  ZAI-F08  =  ZAI-F08  +  R011(I)
*2000/1/3 HEN **MOVE     R004     TO WK-R004
                MOVE  R004    TO        WK-R004X WK-HEN-DATE1
                IF    WK-R0041X   >  89
                  COMPUTE WK-HEN-DATE2 = 19000000 + WK-HEN-DATE1
                ELSE
                  COMPUTE WK-HEN-DATE2 = 20000000 + WK-HEN-DATE1
                END-IF
                MOVE  WK-HEN-DATE2      TO   WK-R004
                IF       WK-SIME4 >= WK-R0044
                  IF      R010(I)   NOT =   9
                         COMPUTE  ZAI-F10
                                  =  ZAI-F10  +  R011(I)
                         COMPUTE  ZAI-F08
                                  =  ZAI-F08  +  R011(I)
                   END-IF
                ELSE
                  IF      R010(I)   NOT =   9
                         COMPUTE  ZAI-F17
                                  =  ZAI-F17  +  R011(I)
                         COMPUTE  ZAI-F19
                                  =  ZAI-F19  +  R011(I)
                   END-IF
                END-IF
*
                IF  INV-FLG = 1
                    CONTINUE
****************    MOVE ZAI-F06  TO ZAI-F08
                ELSE
                 IF R002    = ZERO
*\\  93.06.02 START \\\
                    IF      R010(I)   =  ZERO
                            COMPUTE  ZAI-F12  =
                                     ZAI-F12  -  R011(I)
                    ELSE
                            COMPUTE  ZAI-F12  =
                                     ZAI-F12  -  WK-R011X(I)
                    END-IF
*
                    IF      ZAI-F12   <       ZERO
                            MOVE      ZERO    TO    ZAI-F12
                    END-IF
*\\  93.06.02 START \\\
*
                 END-IF
                END-IF
         WHEN  (W002 = 50  AND  R002 = 0  AND  R001 = 2)  OR
               (W002 = 50  AND  R002 = 2  AND  R001 = 2)  OR
               (W002 = 51  AND  R002 = 2  AND  R001 = 2)
                COMPUTE  WK-SA    =  WK-R011(I)  -  R011(I)
                COMPUTE  ZAI-F06  =  ZAI-F06     -  WK-SA
****************COMPUTE  ZAI-F08  =  ZAI-F08     -  WK-SA
*2000/1/3 HEN **MOVE     R004     TO WK-R004
                MOVE  R004    TO        WK-R004X WK-HEN-DATE1
                IF    WK-R0041X   >  89
                  COMPUTE WK-HEN-DATE2 = 19000000 + WK-HEN-DATE1
                ELSE
                  COMPUTE WK-HEN-DATE2 = 20000000 + WK-HEN-DATE1
                END-IF
                MOVE  WK-HEN-DATE2      TO   WK-R004
                IF       WK-SIME4 >= WK-R0044
                         COMPUTE  ZAI-F10
                                  =  ZAI-F10     -  WK-SA
                         COMPUTE  ZAI-F08
                                  =  ZAI-F08     -  WK-SA
                ELSE
                         COMPUTE  ZAI-F17
                                  =  ZAI-F17     -  WK-SA
                         COMPUTE  ZAI-F19
                                  =  ZAI-F19     -  WK-SA
                END-IF
*
                IF  INV-FLG = 1
                    CONTINUE
********************MOVE ZAI-F06  TO   ZAI-F08
                ELSE
                 IF R002    = ZERO
                    COMPUTE  ZAI-F12  =  ZAI-F12     +  WK-SA
                    IF       ZAI-F12  <  ZERO
                             MOVE        ZERO        TO ZAI-F12
                    END-IF
                 END-IF
                END-IF
         WHEN  (W002 = 50  AND  R002 = 0  AND  R001 = 3)  OR
               (W002 = 50  AND  R002 = 1  AND  R001 = 1)  OR
               (W002 = 50  AND  R002 = 2  AND  R001 = 3)  OR
               (W002 = 51  AND  R002 = 1  AND  R001 = 3)  OR
               (W002 = 51  AND  R002 = 2  AND  R001 = 1)
                COMPUTE  ZAI-F06  =  ZAI-F06     -  R011(I)
****************COMPUTE  ZAI-F08  =  ZAI-F08     -  R011(I)
*2000/1/3 HEN **MOVE   R004      TO  WK-R004
                MOVE  R004    TO        WK-R004X WK-HEN-DATE1
                IF    WK-R0041X   >  89
                  COMPUTE WK-HEN-DATE2 = 19000000 + WK-HEN-DATE1
                ELSE
                  COMPUTE WK-HEN-DATE2 = 20000000 + WK-HEN-DATE1
                END-IF
                MOVE  WK-HEN-DATE2      TO   WK-R004
                IF     WK-SIME4   >= WK-R0044
                       COMPUTE  ZAI-F10  =
                                     ZAI-F10     -  R011(I)
                       COMPUTE  ZAI-F08  =
                                     ZAI-F08     -  R011(I)
                ELSE
                       COMPUTE  ZAI-F17  =
                                     ZAI-F17     -  R011(I)
                       COMPUTE  ZAI-F19  =
                                     ZAI-F19     -  R011(I)
                END-IF
*
                IF  INV-FLG = 1
                    CONTINUE
********************MOVE ZAI-F06  TO   ZAI-F08
                ELSE
                 IF R002    = ZERO
                    COMPUTE  ZAI-F12  =  ZAI-F12     +  R011(I)
                    IF       ZAI-F12  <  ZERO
                             MOVE        ZERO        TO ZAI-F12
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
*
*商品ＣＤが変更された場合
*元の在庫数を変更
     MOVE   WK-HAC-F31(I)   TO   ZAI-F01.
     MOVE   WK-HAC-F20(I)   TO   ZAI-F021.
     MOVE   WK-HAC-F21(I)   TO   ZAI-F022.
     MOVE   WK-HAC-F30(I)   TO   ZAI-F03.
*
     READ   ZZAIMS
            INVALID  MOVE    1      TO    INV-FLG
            NOT INVALID
                     MOVE    ZERO   TO    INV-FLG
     END-READ.
*
     IF      INV-FLG        =    1
             GO TO               ZAI-SHO-50
     END-IF.
*現在庫数をへらす
     IF      R010(I)        =    ZERO  OR  1
             COMPUTE ZAI-F06     =
                     ZAI-F06     -    WK-HAC-F23(I)
     END-IF.
*未入庫数をへらす
     IF      WK-HAC-F22(I) NOT = WK-HAC-F23(I)
             COMPUTE WK-HACHU-SA =
                                      WK-HAC-F22(I)
                                 -    WK-HAC-F23(I)
             IF      WK-HACHU-SA >    ZERO
                     COMPUTE ZAI-F12     =
                                      ZAI-F12 - WK-HACHU-SA
             END-IF
     END-IF.
*
     IF      R010(I)        =    9
             REWRITE             ZAI-REC
             GO TO               ZAI-SHO-EXIT
     END-IF.
*
*入出庫数
*2000/1/3 HEN **
*****MOVE    R004           TO   WK-R004.
     MOVE  R004    TO        WK-R004X WK-HEN-DATE1.
     IF    WK-R0041X   >  89
           COMPUTE WK-HEN-DATE2 = 19000000 + WK-HEN-DATE1
     ELSE
           COMPUTE WK-HEN-DATE2 = 20000000 + WK-HEN-DATE1
     END-IF.
     MOVE  WK-HEN-DATE2      TO   WK-R004.
     IF       WK-SIME4      >=   WK-R0044
              COMPUTE  ZAI-F10 = ZAI-F10 - WK-HAC-F23(I)
              COMPUTE  ZAI-F08 = ZAI-F08 - WK-HAC-F23(I)
     ELSE
              COMPUTE  ZAI-F17 = ZAI-F17 - WK-HAC-F23(I)
              COMPUTE  ZAI-F19 = ZAI-F19 - WK-HAC-F23(I)
     END-IF.
*
     REWRITE      ZAI-REC.
*
 ZAI-SHO-50.
*
     MOVE   WK-HAC-F31(I)   TO   ZAI-F01.
     MOVE   W022(I)         TO   ZAI-F021.
     MOVE   W023(I)         TO   ZAI-F022(1:5).
     MOVE   W024(I)         TO   ZAI-F022(6:2).
     MOVE   W025(I)         TO   ZAI-F022(8:1).
     MOVE   WK-HAC-F301(I)  TO   ZAI-F03.
*
     READ   ZZAIMS
            INVALID  MOVE    1      TO    INV-FLG
            NOT INVALID
                     MOVE    ZERO   TO    INV-FLG
     END-READ.
*
     IF      INV-FLG        =    1
             MOVE SPACE     TO   ZAI-REC
             INITIALIZE          ZAI-REC
     END-IF.
*
*現在庫数
     ADD     R011(I)        TO   ZAI-F06.
*未入庫数
     COMPUTE WK-HACHU-SA    =    WK-HAC-F22(I) - R011(I).
     IF      WK-HACHU-SA    >    ZERO
             ADD     WK-HACHU-SA TO   ZAI-F12
     END-IF.
*入出庫数
*2000/1/3 HEN **
*****MOVE    R004           TO   WK-R004.
     MOVE  R004    TO        WK-R004X WK-HEN-DATE1.
     IF    WK-R0041X   >  89
           COMPUTE WK-HEN-DATE2 = 19000000 + WK-HEN-DATE1
     ELSE
           COMPUTE WK-HEN-DATE2 = 20000000 + WK-HEN-DATE1
     END-IF.
     MOVE  WK-HEN-DATE2      TO   WK-R004.
     IF       WK-SIME4      >=   WK-R0044
              COMPUTE  ZAI-F10   =    ZAI-F10  +  R011(I)
              COMPUTE  ZAI-F08   =    ZAI-F08  +  R011(I)
     ELSE
              COMPUTE  ZAI-F17   =    ZAI-F17  +  R011(I)
              COMPUTE  ZAI-F19   =    ZAI-F19  +  R011(I)
     END-IF.
     IF      INV-FLG        =    1
             MOVE   WK-HAC-F31(I)   TO   ZAI-F01
             MOVE   W022(I)         TO   ZAI-F021
             MOVE   W023(I)         TO   ZAI-F022(1:5)
             MOVE   W024(I)         TO   ZAI-F022(6:2)
             MOVE   W025(I)         TO   ZAI-F022(8:1)
             MOVE   WK-HAC-F301(I)  TO   ZAI-F03
*
             WRITE     ZAI-REC
     ELSE
             REWRITE   ZAI-REC
     END-IF.
*
 ZAI-SHO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*
*--------------------------------------------------------------*
 DEN-70-SEC              SECTION.
*　振替伝票／経費振替　処理
     IF  W011          NOT  =    "29099999"
         GO TO                    DEN-70-EXIT
     END-IF.
*
     EVALUATE   R001   ALSO     R002
*        赤伝登録　（在庫　＋）
         WHEN   1      ALSO     1
                COMPUTE  ZAI-F06  =  ZAI-F06  +  R011(I)
                COMPUTE  ZAI-F08  =  ZAI-F08  +  R011(I)
*        黒伝登録　（在庫　－）
         WHEN   1      ALSO     2
                COMPUTE  ZAI-F06  =  ZAI-F06  -  R011(I)
                COMPUTE  ZAI-F08  =  ZAI-F08  -  R011(I)
*        黒伝修正　（在庫　－　ｏｒ　＋）
         WHEN   2      ALSO     2
                COMPUTE  WK-SA    =  WK-R011(I)  -  R011(I)
                COMPUTE  ZAI-F06  =  ZAI-F06     +  WK-SA
                COMPUTE  ZAI-F08  =  ZAI-F08     +  WK-SA
*        赤伝削除　（在庫　－）
         WHEN   3      ALSO     1
                COMPUTE  ZAI-F06  =  ZAI-F06  -  R011(I)
                COMPUTE  ZAI-F08  =  ZAI-F08  -  R011(I)
*        黒伝登録　（在庫　＋）
         WHEN   3      ALSO     2
                COMPUTE  ZAI-F06  =  ZAI-F06  +  R011(I)
                COMPUTE  ZAI-F08  =  ZAI-F08  +  R011(I)
     END-EVALUATE.
*
*
 DEN-70-EXIT.
     EXIT.
*--------------------------------------------------------------*
*
*--------------------------------------------------------------*
 DEN-71-SEC              SECTION.
*　振替返品／経費振替返品　処理
     IF  W011          NOT  =    "29099999"
         GO TO                    DEN-71-EXIT
     END-IF.
*
     EVALUATE   R001   ALSO     R002
*        赤伝登録　（在庫　－）
         WHEN   1      ALSO     1
                COMPUTE  ZAI-F06  =  ZAI-F06  -  R011(I)
                COMPUTE  ZAI-F08  =  ZAI-F08  -  R011(I)
*        黒伝登録　（在庫　＋）
         WHEN   1      ALSO     2
                COMPUTE  ZAI-F06  =  ZAI-F06  +  R011(I)
                COMPUTE  ZAI-F08  =  ZAI-F08  +  R011(I)
*        黒伝修正　（在庫　－　ｏｒ　＋）
         WHEN   2      ALSO     2
                COMPUTE  WK-SA    =  WK-R011(I)  -  R011(I)
                COMPUTE  ZAI-F06  =  ZAI-F06     -  WK-SA
                COMPUTE  ZAI-F08  =  ZAI-F08     -  WK-SA
*        赤伝削除　（在庫　＋）
         WHEN   3      ALSO     1
                COMPUTE  ZAI-F06  =  ZAI-F06  +  R011(I)
                COMPUTE  ZAI-F08  =  ZAI-F08  +  R011(I)
*        黒伝登録　（在庫　－）
         WHEN   3      ALSO     2
                COMPUTE  ZAI-F06  =  ZAI-F06  -  R011(I)
                COMPUTE  ZAI-F08  =  ZAI-F08  -  R011(I)
     END-EVALUATE.
*
*
 DEN-71-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6      商品在庫マスタＲＥＡＤ　　　　　　　　　　　*
*--------------------------------------------------------------*
 ZAI-READ                SECTION.
     IF   R001 = 1  AND  R002 = 0
          MOVE   WK-HAC31   TO   ZAI-F01
          MOVE   WK-HAC20   TO   ZAI-F021
          MOVE   WK-HAC21   TO   ZAI-F022
          MOVE   WK-HAC30   TO   ZAI-F03
     ELSE
          MOVE   NYU-F24    TO   ZAI-F01
          MOVE   NYU-F13    TO   ZAI-F021
          MOVE   NYU-F14    TO   ZAI-F022
          MOVE   NYU-F22    TO   ZAI-F03
     END-IF.
     READ   ZZAIMS
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
     IF   R001 = 1  AND  R002 = 0
          MOVE   HAC-F31    TO   ZAI-F01
          MOVE   HAC-F20    TO   ZAI-F021
          MOVE   HAC-F21    TO   ZAI-F022
          MOVE   HAC-F30    TO   ZAI-F03
          MOVE   HAC-F10    TO   ZAI-F05
     ELSE
          MOVE   NYU-F24    TO   ZAI-F01
          MOVE   NYU-F13    TO   ZAI-F021
          MOVE   NYU-F14    TO   ZAI-F022
          MOVE   NYU-F22    TO   ZAI-F03
          MOVE   NYU-F08    TO   ZAI-F05
     END-IF.
     WRITE  ZAI-REC.
 ZAI-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  ALL    ワークテーブルクリア
*--------------------------------------------------------------*
 WK-BODY-CLR          SECTION.
**** PERFORM  VARYING  I  FROM  1  BY  1  UNTIL I  >  6
****    MOVE  ZERO     TO   IN-FLG(I)  WK-R011(I)  WK-R011X(I)
***                         WK-R013(I) WK-R015(I)
***     MOVE  SPACE     TO  WK-R012(I) WK-R014(I)
***  END-PERFORM.
*93.12.07
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
     MOVE     SPACE    TO   R016.
 DSP-BODY-CLR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3          ＢＯＤＹクリア     (GR-NO = 6)
*--------------------------------------------------------------*
 DSP-CLR-RTN          SECTION.
     MOVE     SPACE   TO  ZHA0100.
     MOVE     SPACE   TO  DSP-CNTL.
     PERFORM  VARYING I  FROM  1  BY  1  UNTIL  I  >  6
        MOVE  I       TO  W101(I)
     END-PERFORM.
     MOVE     WK-SYORI    TO   R001.
     EVALUATE  R001
         WHEN  1    MOVE  NC"登録"   TO  W001
         WHEN  2    MOVE  NC"修正"   TO  W001
         WHEN  3    MOVE  NC"削除"   TO  W001
     END-EVALUATE.
     PERFORM   CLR-HEAD-RTN.
     PERFORM   CLR-BODY-RTN.
     PERFORM   CLR-TAIL-RTN.
     MOVE      ZERO       TO   PF-FLG  WK-SA.
     MOVE      ZERO       TO   WK-KEY.
     MOVE     "ZHA0100"   TO   DSP-FMT.
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
     IF       ERR-FLG  =  0
              MOVE      SPACE               TO   W034
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
     WRITE    ZHA0100.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL        ＨＥＡＤ　属性クリア
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF R002
                                  EDIT-CURSOR OF R003
                                  EDIT-CURSOR OF R004
                                  EDIT-CURSOR OF R005
                                  EDIT-CURSOR OF R006
                                  EDIT-CURSOR OF R007
                                  EDIT-CURSOR OF R008
                                  EDIT-CURSOR OF R009
                                  EDIT-CURSOR OF W011.
     MOVE     "M"            TO   EDIT-OPTION OF R001
                                  EDIT-OPTION OF R002
                                  EDIT-OPTION OF R003
                                  EDIT-OPTION OF R004
                                  EDIT-OPTION OF R005
                                  EDIT-OPTION OF R006
                                  EDIT-OPTION OF R007
                                  EDIT-OPTION OF R008
                                  EDIT-OPTION OF R009
                                  EDIT-OPTION OF W011.
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL        ＢＯＤＹ　属性クリア
*--------------------------------------------------------------*
 CLR-BODY-RTN           SECTION.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
     MOVE     "M"        TO   EDIT-OPTION OF R010(I)
                              EDIT-OPTION OF R011(I)
                              EDIT-OPTION OF R012(I)
                              EDIT-OPTION OF R013(I)
                              EDIT-OPTION OF R014(I)
                              EDIT-OPTION OF R015(I)
                              EDIT-OPTION OF W022(I)
                              EDIT-OPTION OF W023(I)
                              EDIT-OPTION OF W024(I)
                              EDIT-OPTION OF W025(I)
     MOVE     " "        TO   EDIT-CURSOR OF R010(I)
                              EDIT-CURSOR OF R011(I)
                              EDIT-CURSOR OF R012(I)
                              EDIT-CURSOR OF R013(I)
                              EDIT-CURSOR OF R014(I)
                              EDIT-CURSOR OF R015(I)
                              EDIT-CURSOR OF W022(I)
                              EDIT-CURSOR OF W023(I)
                              EDIT-CURSOR OF W024(I)
                              EDIT-CURSOR OF W025(I)
     END-PERFORM.
 CLR-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL        ＴＡＩＬ　属性クリア
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF R016.
     MOVE     "M"        TO   EDIT-OPTION OF R016.
 CLR-TAIL-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
