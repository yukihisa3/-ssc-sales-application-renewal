# SPD0120I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SPD0120I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　販売管理システム　　　　　　　　　　　*
*    モジュール名　　：　ＰＣ側手書取込データ量販店入力　　　　*
*    作成日／更新日　：　09/10/19                              *
*    作成者／更新者　：　NAV                                   *
*    更新日／更新者　：　
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SPD0120I.
 AUTHOR.                N.K.
 DATE-WRITTEN.          92/11/27.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
***************************************************************
 INPUT-OUTPUT           SECTION.
***************************************************************
 FILE-CONTROL.
*----<< 取引先マスタ >>-*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-ST1   TOK-ST2.
*----<< 店舗マスタ >>-*
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE      STATUS    TEN-ST1   TEN-ST2.
*----<< 商品変換テーブル >>-*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       STB-F01   STB-F02
                        FILE      STATUS    STB-ST1   STB-ST2.
*----<< 商品名称マスタ >>-*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       MEI-F01
                        FILE      STATUS    MEI-ST1   MEI-ST2.
*----<< 条件ファイル >>-*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-ST1   JYO-ST2.
*----<< 量販データ >>-*
     SELECT   PCRYOJF   ASSIGN    TO        DA-01-VI-PCRYOJL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       RYO-F011  RYO-F012
                                            RYO-F02
                        FILE      STATUS    RYO-ST1   RYO-ST2.
*----<< 画面ファイル >>-*
     SELECT   DSPFILE   ASSIGN    TO        GS-DSPF
                        SYMBOLIC  DESTINATION        "DSP"
                        DESTINATION-1       DSP-WS
                        FORMAT              DSP-FMT
                        GROUP               DSP-GRP
                        PROCESSING  MODE    DSP-PRO
*******************     UNIT      CONTROL   DSP-UNIT
                        SELECTED  FUNCTION  DSP-FNC
                        FILE      STATUS    DSP-ST1   DSP-ST2.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*----<< 取引先マスタ >>-*
 FD  HTOKMS             BLOCK     CONTAINS   8   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HTOKMS    OF   XFDLIB    JOINING   TOK  AS   PREFIX.
*----<< 店舗マスタ >>-*
 FD  HTENMS             BLOCK     CONTAINS   8   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HTENMS    OF   XFDLIB    JOINING   TEN  AS   PREFIX.
*----<< 商品変換テーブル >>-*
 FD  HSHOTBL            BLOCK     CONTAINS   12  RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HSHOTBL   OF   XFDLIB    JOINING   STB  AS   PREFIX.
*----<< 商品名称マスタ >>-*
 FD  HMEIMS             BLOCK     CONTAINS   6   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HMEIMS    OF   XFDLIB    JOINING   MEI  AS   PREFIX.
*----<< 条件ファイル >>-*
 FD  HJYOKEN            BLOCK     CONTAINS   6   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HJYOKEN   OF   XFDLIB    JOINING   JYO  AS   PREFIX.
*----<< 伝票データ >>-*
 FD  PCRYOJF            BLOCK     CONTAINS   4   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     PCRYOJF   OF   XFDLIB    JOINING   RYO  AS   PREFIX.
*----<< 画面ファイル >>-*
 FD  DSPFILE.
     COPY     FPD01201   OF   XMDLIB.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  PGM-ID                  PIC  X(08)     VALUE  "SPD0120I".
 01  ACOS-DATE               PIC  9(08)     VALUE  ZERO.
 01  GETUDO                  PIC  S9(02)    VALUE  ZERO.
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  DSP-CNTL.
     03  DSP-WS              PIC  X(08).
     03  DSP-FMT             PIC  X(08).
     03  DSP-GRP             PIC  X(08).
     03  DSP-PRO             PIC  X(02).
     03  DSP-FNC             PIC  X(04).
     03  DSP-ST1             PIC  X(02).
     03  DSP-ST2             PIC  X(04).
     03  DSP-CON             PIC  X(06).
     03  WK-GRP              PIC  X(08).
 01  STATUS-AREA.
     03  IN-DATA             PIC  X(01).
     03  TOK-STATUS.
         05  TOK-ST1         PIC  X(02).
         05  TOK-ST2         PIC  X(04).
     03  TEN-STATUS.
         05  TEN-ST1         PIC  X(02).
         05  TEN-ST2         PIC  X(04).
     03  STB-STATUS.
         05  STB-ST1         PIC  X(02).
         05  STB-ST2         PIC  X(04).
     03  MEI-STATUS.
         05  MEI-ST1         PIC  X(02).
         05  MEI-ST2         PIC  X(04).
     03  JYO-STATUS.
         05  JYO-ST1         PIC  X(02).
         05  JYO-ST2         PIC  X(04).
     03  RYO-STATUS.
         05  RYO-ST1         PIC  X(02).
         05  RYO-ST2         PIC  X(04).
*----<< ﾃﾞｰﾀ ﾃｰﾌﾞﾙ >>-*
 01  FILLER.
     03  WK-BDY              OCCURS    5.
         05  FILLER          OCCURS    10.
             07  FILLER      PIC  X(05).
             07  WK-TENCD    PIC  9(05).
         05  FILLER          OCCURS    10.
             07  FILLER      PIC  X(03).
             07  WK-PROTECT  PIC  X(01).
             07  FILLER      PIC  X(01).
             07  WK-SURYO    PIC  9(05).
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ﾃ-ﾌﾞﾙ >>-*
 01  FNC-TABLE.
     03  ENT                 PIC  X(04)  VALUE "E000".
     03  PF04                PIC  X(04)  VALUE "F004".
     03  PF05                PIC  X(04)  VALUE "F005".
     03  PF06                PIC  X(04)  VALUE "F006".
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｶﾞｲﾀﾞﾝｽ ｴﾘｱ >>-*
 01  GUIDE-AREA.
     03  G001                PIC  N(30)  VALUE
         NC"_取消　_終了".
     03  G002                PIC  N(30)  VALUE
         NC"_取消　_項目戻り".
     03  G003                PIC  N(30)  VALUE
         NC"_取消　_明細終了　_項目戻り".
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
                             NC"ＰＦキーが違います".
     03  MSG02               PIC  N(20)  VALUE
                             NC"処理区分が違います".
     03  MSG03               PIC  N(20)  VALUE
                             NC"メモ_エラー".
     03  MSG04               PIC  N(20)  VALUE
                             NC"このメモ_は登録済です".
     03  MSG05               PIC  N(20)  VALUE
                             NC"このメモ_は未登録です".
     03  MSG06               PIC  N(20)  VALUE
                             NC"出荷場所未登録".
     03  MSG07               PIC  N(20)  VALUE
                             NC"伝発場所未登録".
     03  MSG08               PIC  N(20)  VALUE
                             NC"日付の入力が違います．".
     03  MSG09               PIC  N(20)  VALUE
                             NC"取引先マスタ未登録".
     03  MSG10               PIC  N(20)  VALUE
                             NC"商品未登録".
     03  MSG11               PIC  N(20)  VALUE
                             NC"摘要コード未登録".
     03  MSG12               PIC  N(20)  VALUE
                             NC"伝発は０か９で入力".
     03  MSG13               PIC  N(20)  VALUE
                             NC"Ｙを入力".
     03  MSG14               PIC  N(20)  VALUE
                             NC"Ｙを入力".
     03  MSG15               PIC  N(20)  VALUE
                             NC"担当者コードを入力してください".
     03  MSG16               PIC  N(20)  VALUE
                             NC"このメモ_は使用出来ません".
     03  MSG17               PIC  N(20)  VALUE
                             NC"このメモ_は範囲外です".
     03  MSG18               PIC  N(20)  VALUE
                             NC"納品日が締日以前です。".
     03  MSG19               PIC  N(20)  VALUE
         NC"修正削除を行なうメモ_を入力して下さい。".
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       19.
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME.
         05  WK-HH                PIC  9(02)  VALUE  ZERO.
         05  WK-MN                PIC  9(02)  VALUE  ZERO.
         05  WK-SS                PIC  9(02)  VALUE  ZERO.
         05  FILLER               PIC  9(02)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
 01  HENKAN-TI               PIC  9(04).
 01  START-YYMM              PIC  9(06).
 01  FILLER                  REDEFINES      START-YYMM.
     03  START-YY            PIC  9(04).
     03  START-MM            PIC  9(02).
 01  END-YYMM                PIC  9(06).
 01  FILLER                  REDEFINES      END-YYMM.
     03  END-YY              PIC  9(04).
     03  END-MM              PIC  9(02).
 01  CHK-DATE                PIC  9(08).
 01  FILLER                  REDEFINES      CHK-DATE.
     03  CHK-YYMM            PIC  9(06).
     03  FILLER              REDEFINES      CHK-YYMM.
         05  CHK-YY          PIC  9(04).
         05  CHK-MM          PIC  9(02).
     03  CHK-DD              PIC  9(02).
 01  CHK-DATE-WORK.
     03  CHK-01              PIC  9(04).
     03  CHK-02              PIC  9(02).
     03  MATUBI              PIC  X(24)  VALUE
         "312831303130313130313031".
     03  FILLER              REDEFINES   MATUBI.
         05  WK-MATUBI       PIC  9(02)  OCCURS  12.
 01  YOBI-NO                 PIC  9(01).
 01  WK-MEMO-NO              PIC  9(08).
 01  FILLER                  REDEFINES   WK-MEMO-NO.
     03  WK-MEMO-NO-1        PIC  9(04).
     03  WK-MEMO-NO-2        PIC  9(04).
 01  WK-MEMO-NO-3            PIC  9(04).
 01  GET-MEMO-NO             PIC  9(08).
 01  FILLER                  REDEFINES   GET-MEMO-NO.
     03  GET-MEMO-NO-1       PIC  9(04).
     03  GET-MEMO-NO-2       PIC  9(04).
 01  WK-R00016               PIC  X(13).
 01  WK-MEMO-NO1             PIC  9(04)  VALUE  ZERO.
 01  WK-MEMO-NO2             PIC  9(04)  VALUE  ZERO.
 01  INDEXES.
     03  I                   PIC  9(02).
     03  J                   PIC  9(02).
     03  K                   PIC  9(02).
     03  X                   PIC  9(03).
 01  FLAGS.
     03  INV-FLG             PIC  9(01).
     03  END-FLG             PIC  9(01).
     03  ERR-FLG             PIC  9(02).
     03  HEN-FLG             PIC  9(01).
     03  HEN-FLG2            PIC  9(01).
     03  GR-NO               PIC  9(02).
     03  DSP-OPEN-FLG        PIC  9(01).
     03  TOK-OPEN-FLG        PIC  9(01).
     03  TEN-OPEN-FLG        PIC  9(01).
     03  STB-OPEN-FLG        PIC  9(01).
     03  MEI-OPEN-FLG        PIC  9(01).
     03  JYO-OPEN-FLG        PIC  9(01).
     03  RYO-OPEN-FLG        PIC  9(01).
     03  HAND-FLG            PIC  9(01).
 01  COUNTERS.
     03  NUM-CNT             PIC  9(02).
 01  BREAK-KEY.
     03  NEW.
         05  NEW-01.
             07  NEW-011     PIC  9(04).
             07  NEW-012     PIC  9(04).
         05  NEW-02          PIC  9(01).
     03  OLD.
         05  OLD-01.
             07  OLD-011     PIC  9(04).
             07  OLD-012     PIC  9(04).
         05  OLD-02          PIC  9(01).
 01  DSP-WORK.
     03  WK-R00001           PIC  9(01).
     03  WK-R00002           PIC  X(02).
     03  WK-R00008           PIC  9(06).
     03  WK-R00009           PIC  9(06).
     03  WK-HED              PIC  X(341).
 01  WK-R020-AREA.
     03  WK-R020I-X.
         05  WK-R020I        PIC  X(01)  OCCURS  8.
     03  WK-R020O-X.
         05  WK-R020O        PIC  X(01)  OCCURS  8.
     03  WK-R020             PIC  9(8).
 01  WK-R021-AREA.
     03  WK-R021I-X.
         05  WK-R021I        PIC  X(01)  OCCURS  5.
     03  WK-R021O-X.
         05  WK-R021O        PIC  X(01)  OCCURS  5.
     03  WK-R021             PIC  X(05).
*-- 1999/10/05 テーブルを７テーブルから１５テーブルへ変更する。
 01  CONV-AREA.
     03  WK-NUM-1.
         05  WK-NUM-TBL-1    PIC  X(01)  OCCURS  15.
     03  WK-NUM-2.
         05  WK-NUM-TBL-2    PIC  X(01)  OCCURS  15.
     03  NUM-FLG-TABLE.
         05  NUM-FLG-TBL     PIC  9(01)  OCCURS  15.
*---<< ｶﾞﾒﾝﾊﾞｯｸｱｯﾌﾟ(ﾎﾞﾃﾞｨｰﾌﾞ) >>---*
 01  WK-B-BAK.
     03  WK-BODY-BAK         OCCURS    500.
         05  WK-TENCD-B      PIC  9(05)  VALUE  ZERO.
         05  WK-TENSU-B      PIC  9(05)  VALUE  ZERO.
 01  CNT                     PIC  9(03)  VALUE  ZERO.
 01  B-FLG                   PIC  9(01)  VALUE  ZERO.
 01  CHK-FLG                 PIC  X(03)  VALUE  SPACE.
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*---<< ｻﾌﾞﾙｰﾁﾝ LINK AREA >>-*
 01  LINK-AREA.
     03  LINK-IN.
         05  LI-KBN          PIC  9(01).
         05  LI-KETA         PIC  9(01).
         05  LI-START        PIC  9(09).
         05  LI-END          PIC  9(09).
         05  LI-RYONO        PIC  9(09).
     03  LINK-OUT.
         05  LO-ERR          PIC  9(01).
         05  LO-NEXT         PIC  9(09).
***
 LINKAGE          SECTION.
 01  LINK-MAIN-AREA.
     03  LNK-M-TAN           PIC  X(02).
*
******************************************************************
 PROCEDURE                 DIVISION  USING  LINK-MAIN-AREA.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   0     エラー処理　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DECLARATIVES.
*----------  取引先マスタ　------------------------------------*
 TOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HTOKMS.
     ACCEPT      WK-DATE    FROM  DATE.
     ACCEPT      WK-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"取引先マスタ異常！"
              "ST1=" TOK-ST1 " ST2=" TOK-ST2 " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       STB-OPEN-FLG  =  1  CLOSE    HSHOTBL.
     IF       MEI-OPEN-FLG  =  1  CLOSE    HMEIMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       RYO-OPEN-FLG  =  1  CLOSE    PCRYOJF.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     255            TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  店舗マスタ　--------------------------------------*
 TEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HTENMS.
     ACCEPT      WK-DATE    FROM  DATE.
     ACCEPT      WK-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"店舗マスタ異常！"
              "ST1=" TEN-ST1 " ST2=" TEN-ST2 " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       STB-OPEN-FLG  =  1  CLOSE    HSHOTBL.
     IF       MEI-OPEN-FLG  =  1  CLOSE    HMEIMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       RYO-OPEN-FLG  =  1  CLOSE    PCRYOJF.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     255            TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  商品変換テーブル　--------------------------------*
 STB-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HSHOTBL.
     ACCEPT      WK-DATE    FROM  DATE.
     ACCEPT      WK-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"商品変換テーブル異常！"
              "ST1=" STB-ST1 " ST2=" STB-ST2 " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       STB-OPEN-FLG  =  1  CLOSE    HSHOTBL.
     IF       MEI-OPEN-FLG  =  1  CLOSE    HMEIMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       RYO-OPEN-FLG  =  1  CLOSE    PCRYOJF.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     255            TO   PROGRAM-STATUS.
     STOP     RUN.
*----------   商品名称マスタ　 --------------------------------*
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HMEIMS.
     ACCEPT      WK-DATE    FROM  DATE.
     ACCEPT      WK-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"商品名称マスタ異常！"
              "ST1=" MEI-ST1 " ST2=" MEI-ST2 " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       STB-OPEN-FLG  =  1  CLOSE    HSHOTBL.
     IF       MEI-OPEN-FLG  =  1  CLOSE    HMEIMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       RYO-OPEN-FLG  =  1  CLOSE    PCRYOJF.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     255            TO   PROGRAM-STATUS.
     STOP     RUN.
*----------   条件ファイル　-----------------------------------*
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HJYOKEN.
     ACCEPT      WK-DATE    FROM  DATE.
     ACCEPT      WK-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"条件ファイル異常！"
              "ST1=" JYO-ST1 " ST2=" JYO-ST2 " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       STB-OPEN-FLG  =  1  CLOSE    HSHOTBL.
     IF       MEI-OPEN-FLG  =  1  CLOSE    HMEIMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       RYO-OPEN-FLG  =  1  CLOSE    PCRYOJF.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     255            TO   PROGRAM-STATUS.
     STOP     RUN.
*----------    伝票データＦ -----------------------------------*
 RYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   PCRYOJF.
     ACCEPT      WK-DATE    FROM  DATE.
     ACCEPT      WK-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"量販データＦ異常！"
              "ST1=" RYO-ST1 " ST2=" RYO-ST2 " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       STB-OPEN-FLG  =  1  CLOSE    HSHOTBL.
     IF       MEI-OPEN-FLG  =  1  CLOSE    HMEIMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       RYO-OPEN-FLG  =  1  CLOSE    PCRYOJF.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     255            TO   PROGRAM-STATUS.
     STOP     RUN.
*----------    表示ファイル -----------------------------------*
 DSP-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   DSPFILE.
     ACCEPT      WK-DATE    FROM  DATE.
     ACCEPT      WK-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"表示ファイル異常！"
              "ST1=" DSP-ST1 " ST2=" DSP-ST2 " "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       STB-OPEN-FLG  =  1  CLOSE    HSHOTBL.
     IF       MEI-OPEN-FLG  =  1  CLOSE    HMEIMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       RYO-OPEN-FLG  =  1  CLOSE    PCRYOJF.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     255            TO   PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO    =    99.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
**** ACCEPT      SYS-DATE    FROM  DATE.
**** ACCEPT      SYS-TIME    FROM  TIME.
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
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     DISPLAY  "*** " PGM-ID " START "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ***"  UPON STAT.
*
     MOVE        ZERO      TO        FLAGS.
*
     OPEN     INPUT     HTOKMS.
     MOVE     1              TO   TOK-OPEN-FLG.
     OPEN     INPUT     HTENMS.
     MOVE     1              TO   TEN-OPEN-FLG.
     OPEN     INPUT     HSHOTBL.
     MOVE     1              TO   STB-OPEN-FLG.
     OPEN     INPUT     HMEIMS.
     MOVE     1              TO   MEI-OPEN-FLG.
     OPEN     I-O       HJYOKEN.
     MOVE     1              TO   JYO-OPEN-FLG.
     OPEN     I-O       PCRYOJF.
     MOVE     1              TO   RYO-OPEN-FLG.
     OPEN     I-O       DSPFILE.
     MOVE     1              TO   DSP-OPEN-FLG.
*
*条件Ｆ（経理月）
     MOVE     "58"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG  =  1
              DISPLAY   "HJYOKEN INV KEY=58"  UPON STAT
              MOVE      99        TO   GR-NO
              GO   TO   100-INIT-RTN-EXIT
     END-IF.
     MOVE     JYO-F04        TO   GETUDO.
     COMPUTE  GETUDO   =     JYO-F04 - 1.
*
     IF       GETUDO   <     1
              ADD      12    TO   GETUDO
     END-IF.
*
     MOVE     "57"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG  =  1
              DISPLAY   "HJYOKEN INV KEY=57"  UPON STAT
              MOVE      99        TO   GR-NO
              GO   TO   100-INIT-RTN-EXIT
     END-IF.
     MOVE     JYO-F04        TO   HENKAN-TI.
     REWRITE  JYO-REC.
*条件Ｆ（ＡＣＯＳ用締日）
     MOVE     "99"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG  =  1
              DISPLAY   "HJYOKEN INV KEY=99"  UPON STAT
              MOVE      99        TO   GR-NO
              GO   TO   100-INIT-RTN-EXIT
     END-IF.
     EVALUATE GETUDO
         WHEN 1
              MOVE      JYO-F04        TO   ACOS-DATE
         WHEN 2
              MOVE      JYO-F05        TO   ACOS-DATE
         WHEN 3
              MOVE      JYO-F06        TO   ACOS-DATE
         WHEN 4
              MOVE      JYO-F07        TO   ACOS-DATE
         WHEN 5
              MOVE      JYO-F08        TO   ACOS-DATE
         WHEN 6
              MOVE      JYO-F09        TO   ACOS-DATE
         WHEN 7
              MOVE      JYO-F10        TO   ACOS-DATE
         WHEN 8
              MOVE      JYO-F11        TO   ACOS-DATE
         WHEN 9
              MOVE      JYO-F12        TO   ACOS-DATE
         WHEN 10
              MOVE      JYO-F12A       TO   ACOS-DATE
         WHEN 11
              MOVE      JYO-F12B       TO   ACOS-DATE
         WHEN 12
              MOVE      JYO-F12C       TO   ACOS-DATE
         WHEN OTHER
              DISPLAY  "### ｹﾞﾂﾄﾞ ｲｼﾞｮｳ  ｹﾞﾂﾄﾞ= " GETUDO " ###"
                                       UPON CONS
              MOVE      99        TO   GR-NO
              GO   TO   100-INIT-RTN-EXIT
     END-EVALUATE.
*
*
     MOVE     WK-Y         TO   START-YY
                                  END-YY.
     MOVE     WK-M         TO   START-MM
                                  END-MM.
     ADD      1              TO   END-MM.
     IF       END-YY  >  89
              MOVE    1900   TO   HENKAN-TI
     ELSE
              MOVE    2000   TO   HENKAN-TI
     END-IF.
     IF       END-MM  >  12
              MOVE      1         TO   END-MM
              COMPUTE   END-YY   =  HENKAN-TI  +  END-YY  +  1
     ELSE
              COMPUTE   END-YY   =  HENKAN-TI  +  END-YY
     END-IF.
     SUBTRACT 1              FROM START-MM.
     IF       START-YY  >  89
              MOVE    1900   TO   HENKAN-TI
     ELSE
              MOVE    2000   TO   HENKAN-TI
     END-IF.
     IF       START-MM  <  1
              MOVE      12        TO   START-MM
              COMPUTE   START-YY  =  HENKAN-TI  +  START-YY  -  1
     ELSE
              COMPUTE   START-YY  =  HENKAN-TI  +  START-YY
     END-IF.
     REWRITE JYO-REC.
     MOVE     0              TO   GR-NO.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
* 画面初期表示
     PERFORM     210-DSP-INIT     UNTIL     GR-NO  NOT  =  0.
* 処理区分入力
     PERFORM     220-DSP-R010     UNTIL     GR-NO  NOT  =  1.
* 担当者入力
     PERFORM     230-DSP-R020     UNTIL     GR-NO  NOT  =  2.
* メモ_入力
     PERFORM     240-DSP-R030     UNTIL     GR-NO  NOT  =  3.
* 出荷場所入力
     PERFORM     250-DSP-R040     UNTIL     GR-NO  NOT  =  4.
* 伝発場所入力
     PERFORM     260-DSP-R050     UNTIL     GR-NO  NOT  =  5.
* 注文_入力
     PERFORM     270-DSP-R060     UNTIL     GR-NO  NOT  =  6.
* 注文日入力
     PERFORM     280-DSP-R070     UNTIL     GR-NO  NOT  =  7.
* 納品日入力
     PERFORM     290-DSP-R080     UNTIL     GR-NO  NOT  =  8.
* 分類，商区，伝票入力
     PERFORM     2A0-DSP-R090     UNTIL     GR-NO  NOT  =  9.
* 取引先コード入力
     PERFORM     2B0-DSP-R100     UNTIL     GR-NO  NOT  =  10.
* 量販商品コード
     PERFORM     2C0-DSP-R110     UNTIL     GR-NO  NOT  =  11.
* 商品名入力
     PERFORM     2D0-DSP-R120     UNTIL     GR-NO  NOT  =  12.
* 原価単価，売価単価入力
     PERFORM     2E0-DSP-R130     UNTIL     GR-NO  NOT  =  13.
* 商品ＣＤ，品，単入力
     PERFORM     2L0-DSP-R190     UNTIL     GR-NO  NOT  =  19.
* 摘要コード入力
     PERFORM     2F0-DSP-R140     UNTIL     GR-NO  NOT  =  14.
* 摘要名入力
     PERFORM     2G0-DSP-R150     UNTIL     GR-NO  NOT  =  15.
* 備考入力
     PERFORM     2H0-DSP-R160     UNTIL     GR-NO  NOT  =  16.
* 伝発入力
     PERFORM     2I0-DSP-R170     UNTIL     GR-NO  NOT  =  17.
* 数量入力
     PERFORM     2J0-DSP-R180     UNTIL     GR-NO  NOT  =  18.
* 確認入力
     PERFORM     2K0-DSP-KKNN     UNTIL     GR-NO  NOT  =  90.
 200-MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE              HTOKMS
                        HTENMS
                        HSHOTBL
                        HMEIMS
                        HJYOKEN
                        PCRYOJF
                        DSPFILE.
*
     ACCEPT      WK-DATE    FROM  DATE.
     ACCEPT      WK-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " END   "
              WK-Y "." WK-M "." WK-D " "
              WK-HH ":" WK-MN ":" WK-SS " ***"  UPON STAT.
 300-END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL    ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE     SPACE          TO   FPD01201.
     MOVE     HEN-DATE       TO   SDATE.
     MOVE     HEN-TIME       TO   STIME.
     MOVE     WK-DATE        TO   R00008
                                  R00009.
     MOVE     LNK-M-TAN      TO   R00002.
     MOVE     "SPD0120I"     TO   R001.
     MOVE     "FPD01201"     TO   R002.
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FPD01201"     TO   DSP-FMT.
     MOVE     "ALLF"         TO   DSP-GRP.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      処理区分　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 220-DSP-R010           SECTION.
     MOVE     G001           TO   GUIDE.
     MOVE     "R00001"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   220-DSP-R010-EXIT
     WHEN     PF05
              MOVE      99        TO   GR-NO
              GO   TO   220-DSP-R010-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   220-DSP-R010-EXIT
     END-EVALUATE.
*
     IF       R00001  IS  NOT  NUMERIC
              MOVE      0         TO   R00001
     END-IF.
     IF       R00001  NOT  =  1  AND  2  AND  3
              MOVE      2         TO   ERR-FLG
              GO   TO   220-DSP-R010-EXIT
     END-IF.
*
     IF       R00001  =  1
              PERFORM   900-MEMO-GET
              MOVE      WK-MEMO-NO-1   TO   R00003
              MOVE      1              TO   WK-MEMO-NO-2
              MOVE      WK-MEMO-NO-2   TO   R00004
              MOVE      1              TO   YOBI-NO
              MOVE      4         TO   GR-NO
     ELSE
              MOVE      3         TO   GR-NO
     END-IF.
 220-DSP-R010-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      担当者　　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 230-DSP-R020           SECTION.
     MOVE     ZERO           TO   WK-B-BAK B-FLG.
     MOVE     1              TO   CNT.
     MOVE     G002           TO   GUIDE.
     MOVE     "R00002"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   230-DSP-R020-EXIT
     WHEN     PF06
              IF   R00001  =  1
                   MOVE      1         TO   GR-NO
              ELSE
                   MOVE      3         TO   GR-NO
              END-IF
              GO   TO   230-DSP-R020-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   230-DSP-R020-EXIT
     END-EVALUATE.
     IF       R00002  = "00"  OR  "99"
              MOVE      15        TO   ERR-FLG
              GO   TO   230-DSP-R020-EXIT
     END-IF.
*
     IF       R00001  =  1
              MOVE      3         TO   GR-NO
     ELSE
              MOVE      4         TO   GR-NO
     END-IF.
 230-DSP-R020-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      メモ_　　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 240-DSP-R030           SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "GRP001"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   240-DSP-R030-EXIT
     WHEN     PF06
              IF   R00001  =  1
                   MOVE      2         TO   GR-NO
              ELSE
                   MOVE      1         TO   GR-NO
              END-IF
              GO   TO   240-DSP-R030-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   240-DSP-R030-EXIT
     END-EVALUATE.
*
     IF       R00003  IS  NOT  NUMERIC
     OR       R00003  =  ZERO
              MOVE      0         TO   R00003
              MOVE      19        TO   ERR-FLG
              GO   TO   240-DSP-R030-EXIT
     END-IF.
     IF       R00004  IS  NOT  NUMERIC
     OR       R00004  =  ZERO
              MOVE      0         TO   R00004
              MOVE      19        TO   ERR-FLG
              GO   TO   240-DSP-R030-EXIT
     END-IF.
*
*  全レコード解放（排他制御用）
     CLOSE    PCRYOJF.
     OPEN     I-O   PCRYOJF.
*
     MOVE     R00003         TO   RYO-F011.
     MOVE     R00004         TO   RYO-F012.
     MOVE     0              TO   RYO-F02.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     START    PCRYOJF   KEY  NOT  <  RYO-F011    RYO-F012
                                     RYO-F02
              INVALID   KEY
                   MOVE      HIGH-VALUE     TO   BREAK-KEY
     END-START.
     IF       NEW  NOT  =  HIGH-VALUE
              PERFORM   900-RYO-READ
     END-IF.
*
     IF       NEW  NOT  =  HIGH-VALUE
     AND      R00001  =  1
              MOVE      4         TO   ERR-FLG
              GO   TO   240-DSP-R030-EXIT
     END-IF.
     IF       NEW  =  HIGH-VALUE
     AND      R00001   NOT  =  1
              MOVE      5         TO   ERR-FLG
              GO   TO   240-DSP-R030-EXIT
     END-IF.
*
     MOVE     1              TO   YOBI-NO.
     IF       R00001  NOT  =  1
              PERFORM   900-RYO-TO-DSP
     END-IF.
*
     EVALUATE R00001
         WHEN   2
              MOVE      4         TO   GR-NO
         WHEN   3
              MOVE      "Y"       TO   KKNN
              MOVE      90        TO   GR-NO
     END-EVALUATE.
 240-DSP-R030-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      出荷場所　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 250-DSP-R040           SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "R00005"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   250-DSP-R040-EXIT
     WHEN     PF06
              IF   R00001  =  1
                   MOVE      3         TO   GR-NO
              ELSE
                   MOVE      2         TO   GR-NO
              END-IF
              GO   TO   250-DSP-R040-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   250-DSP-R040-EXIT
     END-EVALUATE.
*
     MOVE     "20"           TO   JYO-F01.
     MOVE     R00005         TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG  =  1
              MOVE      6         TO   ERR-FLG
              GO   TO   250-DSP-R040-EXIT
     END-IF.
     REWRITE JYO-REC.
*
     MOVE     5              TO   GR-NO.
 250-DSP-R040-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      伝発場所　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 260-DSP-R050           SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "R00006"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   260-DSP-R050-EXIT
     WHEN     PF06
              MOVE      4         TO   GR-NO
              GO   TO   260-DSP-R050-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   260-DSP-R050-EXIT
     END-EVALUATE.
*
     MOVE     "20"           TO   JYO-F01.
     MOVE     R00006         TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG  =  1
              MOVE      7         TO   ERR-FLG
              GO   TO   260-DSP-R050-EXIT
     END-IF.
     REWRITE JYO-REC.
*
     MOVE     6              TO   GR-NO.
 260-DSP-R050-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      注文_　　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 270-DSP-R060           SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "R00007"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   270-DSP-R060-EXIT
     WHEN     PF06
              MOVE      5         TO   GR-NO
              GO   TO   270-DSP-R060-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   270-DSP-R060-EXIT
     END-EVALUATE.
*
     MOVE     R00007         TO   WK-NUM-1.
     PERFORM  900-NUM-CONV.
     MOVE     WK-NUM-2       TO   R00007.
*
     MOVE     7              TO   GR-NO.
 270-DSP-R060-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      注文日　　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 280-DSP-R070           SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "R00008"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   280-DSP-R070-EXIT
     WHEN     PF06
              MOVE      6         TO   GR-NO
              GO   TO   280-DSP-R070-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   280-DSP-R070-EXIT
     END-EVALUATE.
*
     MOVE     R00008         TO   CHK-DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        R00008    TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD.
     IF          LINK-OUT-RET   =    ZERO
                 MOVE   ZERO            TO   ERR-FLG
                 MOVE   LINK-OUT-YMD   TO   CHK-DATE
                 PERFORM  900-DATE-CHECK
     ELSE
                 MOVE   8       TO   ERR-FLG
     END-IF.
*--
     IF       ERR-FLG  NOT  =  0
              GO   TO   280-DSP-R070-EXIT
     END-IF.
*
     MOVE     8              TO   GR-NO.
 280-DSP-R070-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      納品日　　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 290-DSP-R080           SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "R00009"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   290-DSP-R080-EXIT
     WHEN     PF06
              MOVE      7         TO   GR-NO
              GO   TO   290-DSP-R080-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   290-DSP-R080-EXIT
     END-EVALUATE.
*
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        R00009    TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD.
     IF          LINK-OUT-RET   =    ZERO
                 MOVE   ZERO            TO   ERR-FLG
                 MOVE   LINK-OUT-YMD   TO   CHK-DATE
                 PERFORM  900-DATE-CHECK
     ELSE
                 MOVE   8       TO   ERR-FLG
     END-IF.
*****
     IF       ERR-FLG  NOT  =  0
              GO   TO   290-DSP-R080-EXIT
     END-IF.
*
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        R00009    TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD.
     IF       LINK-OUT-YMD     >    ACOS-DATE
              IF   R00009(3:2)    =    GETUDO
                   MOVE 18        TO   ERR-FLG
                   GO   TO   290-DSP-R080-EXIT
              END-IF
     END-IF.
*--
*
     MOVE     9              TO   GR-NO.
 290-DSP-R080-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      分類、商区、伝票　入力　　　　　　　　　　  *
*--------------------------------------------------------------*
 2A0-DSP-R090           SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "GRP002"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   2A0-DSP-R090-EXIT
     WHEN     PF06
              MOVE      8         TO   GR-NO
              GO   TO   2A0-DSP-R090-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   2A0-DSP-R090-EXIT
     END-EVALUATE.
*
     MOVE     10             TO   GR-NO.
 2A0-DSP-R090-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      取引先Ｃ　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 2B0-DSP-R100           SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "R00013"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   2B0-DSP-R100-EXIT
     WHEN     PF06
              MOVE      9         TO   GR-NO
              GO   TO   2B0-DSP-R100-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   2B0-DSP-R100-EXIT
     END-EVALUATE.
*
     IF       R00013  IS  NOT  NUMERIC
              MOVE      0         TO   R00013
     END-IF.
     MOVE     R00013         TO   TOK-F01.
     PERFORM  900-TOK-READ.
     IF       INV-FLG  =  1
              MOVE      9         TO   ERR-FLG
              GO   TO   2B0-DSP-R100-EXIT
     END-IF.
     MOVE     TOK-F03        TO   R00014.
*
     IF       R00001  =  1
              MOVE      R00013    TO   TEN-F52
              MOVE      0         TO   TEN-F011
              MOVE      0         TO   END-FLG
              START     HTENMS    KEY  NOT  <  TEN-F52  TEN-F011
                        INVALID  KEY
                        MOVE      1         TO   END-FLG
              END-START
              IF   END-FLG  =  0
                   PERFORM   900-TEN-READ
              END-IF
              IF   END-FLG  =  0
                   PERFORM  VARYING I FROM 1 BY 1 UNTIL  I > 5
                        MOVE     SPACE      TO   WK-BDY (I)
                        PERFORM  VARYING  J  FROM  1  BY  1
                                                  UNTIL  J  > 10
                                                  OR  END-FLG = 1
                             MOVE   TEN-F011 TO   WK-TENCD (I J)
                             PERFORM  900-TEN-READ
                        END-PERFORM
                        MOVE     WK-BDY (I) TO   BDY (I)
                   END-PERFORM
              END-IF
     END-IF.
*
     MOVE     11             TO   GR-NO.
 2B0-DSP-R100-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      量販商品コード　入力　　　　　　　　　　　　*
*--------------------------------------------------------------*
 2C0-DSP-R110           SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     R00016         TO   WK-R00016.
     MOVE     "R00016"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   2C0-DSP-R110-EXIT
     WHEN     PF06
              MOVE      10        TO   GR-NO
              GO   TO   2C0-DSP-R110-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   2C0-DSP-R110-EXIT
     END-EVALUATE.
*
     MOVE     R00013         TO   STB-F01.
     MOVE     R00016         TO   STB-F02.
     PERFORM  900-STB-READ.
     IF       INV-FLG  =  1
              MOVE      10        TO   ERR-FLG
              GO   TO   2C0-DSP-R110-EXIT
     END-IF.
     MOVE     STB-F031       TO   MEI-F011.
     MOVE     STB-F032       TO   MEI-F012.
     PERFORM  900-MEI-READ.
     IF       INV-FLG  =  1
              MOVE      10        TO   ERR-FLG
              GO   TO   2C0-DSP-R110-EXIT
     END-IF.
     MOVE     MEI-F93        TO   HEN-FLG.
*
     IF       R00016  NOT  =  WK-R00016
              MOVE      MEI-F031       TO   R00017
              MOVE      MEI-F032       TO   R00018
              MOVE      STB-F05        TO   R00015
              MOVE      STB-F06        TO   R00019
              MOVE      STB-F031       TO   R00020
              MOVE      STB-F032 (1:5) TO   R00021
              MOVE      STB-F032 (6:2) TO   R00022
              MOVE      STB-F032 (8:1) TO   R00023
     END-IF.
*
     MOVE     12             TO   GR-NO.
 2C0-DSP-R110-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      商品名　入力　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 2D0-DSP-R120           SECTION.
     IF       HEN-FLG  NOT  =  1
              IF   DSP-FNC  =  ENT
                   MOVE      13        TO   GR-NO
              ELSE
                   MOVE      11        TO   GR-NO
              END-IF
              GO   TO   2D0-DSP-R120-EXIT
     END-IF.
*
     MOVE     G002           TO   GUIDE.
     MOVE     "GRP003"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   2D0-DSP-R120-EXIT
     WHEN     PF06
              MOVE      11        TO   GR-NO
              GO   TO   2D0-DSP-R120-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   2D0-DSP-R120-EXIT
     END-EVALUATE.
*
     MOVE     13             TO   GR-NO.
 2D0-DSP-R120-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      原価単価，売価単価　入力　　　　　　　　　　*
*--------------------------------------------------------------*
 2E0-DSP-R130           SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "GRP004"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   2E0-DSP-R130-EXIT
     WHEN     PF06
              MOVE      12        TO   GR-NO
              GO   TO   2E0-DSP-R130-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   2E0-DSP-R130-EXIT
     END-EVALUATE.
*
     MOVE     19             TO   GR-NO.
 2E0-DSP-R130-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      商品ＣＤ，品，単　入力　　　　　　　　　　　*
*--------------------------------------------------------------*
 2L0-DSP-R190           SECTION.
     IF       HEN-FLG  NOT  =  1
              IF   DSP-FNC  =  ENT
                   MOVE      14        TO   GR-NO
              ELSE
                   MOVE      13        TO   GR-NO
              END-IF
              GO   TO   2L0-DSP-R190-EXIT
     END-IF.
*
     MOVE     G002           TO   GUIDE.
     MOVE     "GRP007"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   2L0-DSP-R190-EXIT
     WHEN     PF06
              MOVE      13        TO   GR-NO
              GO   TO   2L0-DSP-R190-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   2L0-DSP-R190-EXIT
     END-EVALUATE.
*
*商品ＣＤ数字変換
*８桁　頭０詰
     IF  R00020    NOT NUMERIC
     AND R00020    NOT = SPACE
         MOVE   R00020     TO  WK-R020I-X
         MOVE   ZERO       TO  J  HEN-FLG2
         PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
                                          OR HEN-FLG2 NOT = 0
           IF  WK-R020I(I)  NUMERIC
               ADD    1   TO  J
               MOVE   WK-R020I(I)  TO  WK-R020O(J)
           ELSE
             IF  WK-R020I(I)  NOT = SPACE
               MOVE   1   TO  HEN-FLG2
             END-IF
           END-IF
         END-PERFORM
         IF  HEN-FLG2 = ZERO
           MOVE   ZERO             TO  WK-R020
           COMPUTE I = 9 - J
           MOVE   WK-R020O-X(1:J)  TO  WK-R020(I:J)
           MOVE   WK-R020          TO  R00020
         END-IF
     END-IF.
*５桁　頭空白詰
     IF  R00021     NOT = SPACE
     AND R00021   (5:1) = SPACE
         MOVE   R00021     TO  WK-R021I-X
         MOVE   ZERO       TO  J
         PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
           IF  WK-R021I(I)  NOT = SPACE
               ADD    1   TO  J
               MOVE   WK-R021I(I)  TO  WK-R021O(J)
           END-IF
         END-PERFORM
         MOVE   SPACE            TO  WK-R021
         COMPUTE I = 6 - J
         MOVE   WK-R021O-X(1:J)  TO  WK-R021(I:J)
         MOVE   WK-R021          TO  R00021
     END-IF.
*
     MOVE     R00020         TO   MEI-F011.
     MOVE     R00021         TO   MEI-F012 (1:5).
     MOVE     R00022         TO   MEI-F012 (6:2).
     MOVE     R00023         TO   MEI-F012 (8:1).
     PERFORM  900-MEI-READ.
     IF       INV-FLG  =  1
              MOVE      10        TO   ERR-FLG
              GO   TO   2L0-DSP-R190-EXIT
     END-IF.
*
     MOVE     14             TO   GR-NO.
 2L0-DSP-R190-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      摘要コード　入力　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 2F0-DSP-R140           SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "R00024"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   2F0-DSP-R140-EXIT
     WHEN     PF06
              MOVE      19        TO   GR-NO
              GO   TO   2F0-DSP-R140-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   2F0-DSP-R140-EXIT
     END-EVALUATE.
*
     IF       R00024  IS  NOT  NUMERIC
              MOVE      0         TO   R00024
     END-IF.
*
     IF       R00024  =  0
              MOVE      SPACE     TO   R00025
                                       R00026
     END-IF.
     IF       R00024  NOT  =  0  AND  99
              MOVE      "80"           TO   JYO-F01
              MOVE      R00023         TO   JYO-F02
              PERFORM   900-JYO-READ
              IF   INV-FLG  =  1
                   MOVE      11        TO   ERR-FLG
                   GO   TO   2F0-DSP-R140-EXIT
              END-IF
              IF   R00024  =  SPACE
              AND  R00025  =  SPACE
                   MOVE      JYO-F14   TO   R00025
                   MOVE      JYO-F15   TO   R00026
              END-IF
     END-IF.
*
     IF       R00024  =  0
              MOVE      16        TO   GR-NO
     ELSE
              MOVE      15        TO   GR-NO
     END-IF.
 2F0-DSP-R140-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      摘要名　　　入力　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 2G0-DSP-R150           SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "GRP005"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   2G0-DSP-R150-EXIT
     WHEN     PF06
              MOVE      14        TO   GR-NO
              GO   TO   2G0-DSP-R150-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   2G0-DSP-R150-EXIT
     END-EVALUATE.
*
     MOVE     16             TO   GR-NO.
 2G0-DSP-R150-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      備考　入力　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 2H0-DSP-R160           SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "R00027"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   2H0-DSP-R160-EXIT
     WHEN     PF06
              IF   R00024  =  0
                   MOVE      14        TO   GR-NO
              ELSE
                   MOVE      15        TO   GR-NO
              END-IF
              GO   TO   2H0-DSP-R160-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   2H0-DSP-R160-EXIT
     END-EVALUATE.
*
     MOVE     17             TO   GR-NO.
 2H0-DSP-R160-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      伝発　　　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 2I0-DSP-R170           SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "R00028"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   2I0-DSP-R170-EXIT
     WHEN     PF06
              MOVE      16        TO   GR-NO
              GO   TO   2I0-DSP-R170-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   2I0-DSP-R170-EXIT
     END-EVALUATE.
*
     IF       R00028  IS  NOT  NUMERIC
              MOVE      0         TO   R00028
     END-IF.
*
     IF       R00028  NOT  =  0  AND  9
              MOVE      12        TO   ERR-FLG
              GO   TO   2I0-DSP-R170-EXIT
     END-IF.
*
     MOVE     18             TO   GR-NO.
 2I0-DSP-R170-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      数量　入力　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 2J0-DSP-R180           SECTION.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  5
              MOVE      BDY (I)   TO   WK-BDY (I)
              PERFORM  VARYING  J  FROM  1  BY  1  UNTIL  J  >  10
                   IF   WK-TENCD (I J)  IS  NUMERIC
                        MOVE      SPACE     TO   WK-PROTECT (I J)
                   ELSE
                        MOVE      "X"       TO   WK-PROTECT (I J)
                   END-IF
              END-PERFORM
              MOVE      WK-BDY (I)     TO   BDY (I)
     END-PERFORM.
*
     MOVE     G002           TO   GUIDE.
     MOVE     "GRP006"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   2J0-DSP-R180-EXIT
     WHEN     PF06
              MOVE      17        TO   GR-NO
              GO   TO   2J0-DSP-R180-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   2J0-DSP-R180-EXIT
     END-EVALUATE.
*
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  5
              MOVE      BDY (I)   TO   WK-BDY (I)
              PERFORM  VARYING  J  FROM  1  BY  1  UNTIL  J  >  10
                   IF   WK-TENCD (I J)  IS  NUMERIC
                   AND  WK-SURYO (I J)  IS  NOT  NUMERIC
                        MOVE      0         TO   WK-SURYO (I J)
                   END-IF
              END-PERFORM
              MOVE      WK-BDY (I)     TO   BDY (I)
     END-PERFORM.
*初期確認が”Ｂ”の場合は次も”Ｂ”とする。
     IF       B-FLG  =  ZERO
              MOVE   "Y"     TO   KKNN
     ELSE
              MOVE   "B"     TO   KKNN
     END-IF.
*
     MOVE     90             TO   GR-NO.
 2J0-DSP-R180-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　入力　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 2K0-DSP-KKNN          SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "KKNN"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   2K0-DSP-KKNN-EXIT
     WHEN     PF06
              EVALUATE  R00001
                   WHEN  1
                   WHEN  2
                        MOVE      18        TO   GR-NO
                   WHEN  3
                        MOVE      3         TO   GR-NO
              END-EVALUATE
              GO   TO   2K0-DSP-KKNN-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   2K0-DSP-KKNN-EXIT
     END-EVALUATE.
*
     IF       KKNN  NOT  =  "Y"
              MOVE      14        TO   ERR-FLG
              GO   TO   2K0-DSP-KKNN-EXIT
     END-IF.
*
     EVALUATE   R00001
         WHEN  1
              PERFORM   900-RYO-WRITE
         WHEN  2
              PERFORM   900-RYO-WRITE
         WHEN  3
              PERFORM   900-RYO-DELETE
     END-EVALUATE.
*
     IF  KKNN  =  "Y"
         MOVE           ZERO        TO       B-FLG
         EVALUATE  R00001
           WHEN  1
              IF   END-FLG  =  0
              AND  YOBI-NO  NOT  =  9
                   PERFORM  VARYING I FROM 1 BY 1 UNTIL  I > 5
                        MOVE     SPACE      TO   WK-BDY (I)
                        PERFORM  VARYING  J  FROM  1  BY  1
                                                  UNTIL  J  > 10
                                                  OR  END-FLG = 1
                             MOVE   TEN-F011 TO   WK-TENCD (I J)
                             PERFORM  900-TEN-READ
                        END-PERFORM
                        MOVE     WK-BDY (I) TO   BDY (I)
                   END-PERFORM
                   ADD       1         TO   YOBI-NO
                   MOVE      18        TO   GR-NO
              ELSE
                   MOVE      R00001    TO   WK-R00001
                   MOVE      R00002    TO   WK-R00002
                   MOVE      R00008    TO   WK-R00008
                   MOVE      R00009    TO   WK-R00009
                   MOVE      SPACE     TO   FPD01201
                   MOVE      HEN-DATE       TO   SDATE
                   MOVE      HEN-TIME       TO   STIME
                   MOVE      WK-R00001      TO   R00001
                   MOVE      WK-R00002      TO   R00002
                   MOVE      WK-R00008      TO   R00008
                   MOVE      WK-R00009      TO   R00009
                   MOVE      "SPD0120I"     TO   R001
                   MOVE      "FPD01201"     TO   R002
                   MOVE      WK-MEMO-NO-1   TO   R00003
                   ADD       1              TO   WK-MEMO-NO-2
                   IF  WK-MEMO-NO-2  >=  9999
                       PERFORM 900-MEMO-GET
                       MOVE  1              TO   WK-MEMO-NO-2
                   END-IF
                   MOVE      WK-MEMO-NO-2   TO   R00004
                   MOVE      4              TO   GR-NO
              END-IF
              GO   TO   2K0-DSP-KKNN-EXIT
           WHEN  2
              PERFORM   900-RYO-READ
              IF   NEW  NOT  = HIGH-VALUE
                   PERFORM   900-RYO-TO-DSP
                   MOVE      18        TO   GR-NO
              ELSE
                   MOVE      R00001    TO   WK-R00001
                   MOVE      R00002    TO   WK-R00002
                   MOVE      R00008    TO   WK-R00008
                   MOVE      R00009    TO   WK-R00009
                   MOVE      SPACE     TO   FPD01201
                   MOVE      HEN-DATE       TO   SDATE
                   MOVE      HEN-TIME       TO   STIME
                   MOVE      WK-R00001      TO   R00001
                   MOVE      WK-R00002      TO   R00002
                   MOVE      WK-R00008      TO   R00008
                   MOVE      WK-R00009      TO   R00009
                   MOVE      "SPD0120I"     TO   R001
                   MOVE      "FPD01201"     TO   R002
                   MOVE      3              TO   GR-NO
              END-IF
              GO   TO   2K0-DSP-KKNN-EXIT
           WHEN  3
              MOVE      R00001    TO   WK-R00001
              MOVE      R00002    TO   WK-R00002
              MOVE      R00008    TO   WK-R00008
              MOVE      R00009    TO   WK-R00009
              MOVE      SPACE     TO   FPD01201
              MOVE      HEN-DATE       TO   SDATE
              MOVE      HEN-TIME       TO   STIME
              MOVE      WK-R00001      TO   R00001
              MOVE      WK-R00002      TO   R00002
              MOVE      WK-R00008      TO   R00008
              MOVE      WK-R00009      TO   R00009
              MOVE      "SPD0120I"     TO   R001
              MOVE      "FPD01201"     TO   R002
              MOVE      3              TO   GR-NO
              GO   TO   2K0-DSP-KKNN-EXIT
     END-IF.
 2K0-DSP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     量販データ登録・修正　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 900-RYO-WRITE          SECTION.
     INITIALIZE    RYO-REC.
     MOVE     R00003         TO   RYO-F011.
     MOVE     R00004         TO   RYO-F012.
     MOVE     YOBI-NO        TO   RYO-F02.
     PERFORM  900-RYO-RAN-READ.
*
*
     MOVE     LNK-M-TAN      TO   RYO-F03.
     MOVE     R00005         TO   RYO-F04.
     MOVE     R00006         TO   RYO-F05.
     MOVE     R00007         TO   RYO-F63.
*--
     MOVE     "3"            TO   LINK-IN-KBN.
     MOVE     R00008         TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"  USING   LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     MOVE    LINK-OUT-YMD   TO   RYO-F071.
*--
     MOVE     "3"            TO   LINK-IN-KBN.
     MOVE     R00009         TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"  USING   LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     MOVE    LINK-OUT-YMD   TO   RYO-F072.
*--
     MOVE     R00010         TO   RYO-F08.
     MOVE     R00011         TO   RYO-F09.
     MOVE     R00012         TO   RYO-F10.
     MOVE     R00013         TO   RYO-F11.
     MOVE     R00016         TO   RYO-F121.
     MOVE     R00017         TO   RYO-F1211.
     MOVE     R00018         TO   RYO-F1212.
     MOVE     R00015         TO   RYO-F131.
     MOVE     R00019         TO   RYO-F132.
     MOVE     R00025         TO   RYO-F141.
     MOVE     R00026         TO   RYO-F142.
     MOVE     R00027         TO   RYO-F15.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  5
         MOVE     BDY (I)    TO   WK-BDY (I)
         PERFORM  VARYING  J  FROM  1  BY  1  UNTIL  J  > 10
              COMPUTE   K  =  (I - 1) * 10 + J
              IF   WK-TENCD (I J)  IS  NUMERIC
                   MOVE      WK-TENCD (I J)    TO   RYO-F161 (K)
                   MOVE      WK-SURYO (I J)    TO   RYO-F162 (K)
              ELSE
                   MOVE      0                 TO   RYO-F161 (K)
                                                    RYO-F162 (K)
              END-IF
         END-PERFORM
     END-PERFORM.
     IF       R00001  =  1
              MOVE      9         TO   RYO-F573
     END-IF.
     MOVE     0              TO   RYO-F58.
     MOVE     HENKAN-TI      TO   RYO-F59.
     MOVE     R00028         TO   RYO-F60.
     MOVE     R00024         TO   RYO-F61.
     MOVE     R00020         TO   RYO-F62 (1:8)
     MOVE     R00021         TO   RYO-F62 (9:5)
     MOVE     R00022         TO   RYO-F62 (14:2)
     MOVE     R00023         TO   RYO-F62 (16:1)
     MOVE     "3"            TO   LINK-IN-KBN.
     MOVE     SYS-DATE       TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"  USING   LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD  TO   RYO-F99.
     MOVE     SPACE         TO   RYO-F574 RYO-F575 RYO-F576
                                 RYO-F577 RYO-F578 RYO-F579
                                 RYO-F57A RYO-F57B RYO-F57C
                                 RYO-F57D.
*--
     IF       INV-FLG  =  1
              WRITE     RYO-REC
     ELSE
              REWRITE   RYO-REC
     END-IF.
 900-RYO-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     量販データ削除　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 900-RYO-DELETE         SECTION.
     MOVE     R00003         TO   RYO-F011.
     MOVE     R00004         TO   RYO-F012.
     MOVE     0              TO   RYO-F02.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     START    PCRYOJF  KEY  NOT  <  RYO-F011  RYO-F012
                                    RYO-F02
              INVALID  KEY
                   MOVE      HIGH-VALUE     TO   BREAK-KEY
     END-START.
     PERFORM  900-RYO-READ.
     PERFORM  UNTIL  NEW  =  HIGH-VALUE
              DELETE         PCRYOJF  RECORD
              PERFORM  900-RYO-READ
     END-PERFORM.
 900-RYO-DELETE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     量販データ読込み　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 900-RYO-TO-DSP         SECTION.
     MOVE     RYO-F02        TO   YOBI-NO.
     MOVE     RYO-F03        TO   R00002.
     MOVE     RYO-F04        TO   R00005.
     MOVE     RYO-F05        TO   R00006.
     MOVE     RYO-F63        TO   R00007.
     MOVE     RYO-F071       TO   R00008.
     MOVE     RYO-F072       TO   R00009.
     MOVE     RYO-F08        TO   R00010.
     MOVE     RYO-F09        TO   R00011.
     MOVE     RYO-F10        TO   R00012.
     MOVE     RYO-F11        TO   R00013
                                  TOK-F01.
     PERFORM  900-TOK-READ.
     IF       INV-FLG  =  0
              MOVE      TOK-F03   TO   R00014
     ELSE
              MOVE      NC"＊＊＊＊＊＊＊＊＊＊"  TO   R00014
     END-IF.
     MOVE     RYO-F131       TO   R00015.
     MOVE     RYO-F121       TO   R00016.
     MOVE     RYO-F1211      TO   R00017.
     MOVE     RYO-F1212      TO   R00018.
     MOVE     RYO-F132       TO   R00019.
     MOVE     RYO-F07        TO   TEN-F011.
     MOVE     RYO-F62 (1:8)  TO   R00020.
     MOVE     RYO-F62 (9:5)  TO   R00021.
     MOVE     RYO-F62 (14:2) TO   R00022.
     MOVE     RYO-F62 (16:1) TO   R00023.
     MOVE     RYO-F61        TO   R00024.
     MOVE     RYO-F141       TO   R00025.
     MOVE     RYO-F142       TO   R00026.
     MOVE     RYO-F15        TO   R00027.
     MOVE     RYO-F60        TO   R00028.
     IF  RYO-F574 = "E"
         MOVE NC"！エラー有り！" TO ERRMSG
     ELSE
         MOVE SPACE          TO ERRMSG  R003
     END-IF.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  5
         MOVE     SPACE      TO   WK-BDY (I)
         PERFORM  VARYING  J  FROM  1  BY  1  UNTIL  J  > 10
              COMPUTE   K  =  (I - 1) * 10 + J
              IF   RYO-F161 (K)  NOT  =  0
                   MOVE      RYO-F161 (K)      TO   WK-TENCD (I J)
                   MOVE      RYO-F162 (K)      TO   WK-SURYO (I J)
              END-IF
         END-PERFORM
         MOVE     WK-BDY (I)      TO   BDY (I)
     END-PERFORM.
  900-RYO-TO-DSP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     IF       ERR-FLG  =  0
              MOVE      SPACE               TO   ERR
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   ERR
     END-IF.
     MOVE     "ALLF"         TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     IF       ERR-FLG  NOT  =  0
              MOVE      "AL"           TO   DSP-PRO
              MOVE      0              TO   ERR-FLG
     ELSE
              MOVE      "NE"           TO   DSP-PRO
     END-IF.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE          TO   DSP-PRO.
 900-DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FPD01201.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     メモ番号取得                                *
*--------------------------------------------------------------*
 900-MEMO-GET           SECTION.
     MOVE     54             TO   JYO-F01.
     MOVE     "PCDATAWK"     TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG  =  1
              DISPLAY   NC"メモ_採番エラー"
                        JYO-F01 JYO-F02       UPON CONS
              STOP  RUN
     END-IF.
     MOVE     JYO-F04        TO   WK-MEMO-NO-1.
     MOVE     JYO-F06        TO   WK-MEMO-NO-3.
     ADD      1              TO   WK-MEMO-NO-1.
     IF       WK-MEMO-NO-1  >=  WK-MEMO-NO-3
              MOVE  JYO-F05  TO   WK-MEMO-NO-1
     END-IF.
*
     MOVE     WK-MEMO-NO-1   TO   JYO-F04.
     MOVE     1              TO   WK-MEMO-NO-2.
     REWRITE  JYO-REC.
 900-MEMO-GET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     日付チェック　　                            *
*--------------------------------------------------------------*
 900-DATE-CHECK         SECTION.
     IF     ( CHK-YYMM  <  START-YYMM
     OR       CHK-YYMM  >  END-YYMM )
              MOVE      10        TO   ERR-FLG
              MOVE      8         TO   ERR-FLG
              GO   TO   900-DATE-CHECK-EXIT
     END-IF.
 900-DATE-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   数値変換　　　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 900-NUM-CONV           SECTION.
     IF       WK-NUM-1  =  SPACE
              MOVE      WK-NUM-1       TO   WK-NUM-2
              GO   TO   900-NUM-CHECK-EXIT
     END-IF.
     MOVE     ALL "1"        TO   NUM-FLG-TABLE.
     MOVE     0              TO   J.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  15
              IF   WK-NUM-TBL-1 (I)  IS  NUMERIC
                   ADD       1         TO   J
              ELSE
                   IF   WK-NUM-TBL-1  (I)  NOT  =  SPACE
                        MOVE      "0"       TO   NUM-FLG-TBL (I)
                   END-IF
              END-IF
     END-PERFORM.
     IF       NUM-FLG-TABLE  NOT  =  ALL "1"
              MOVE      WK-NUM-1       TO   WK-NUM-2
              GO   TO   900-NUM-CHECK-EXIT
     END-IF.
     COMPUTE  J  =  15  -  J.
*--
     MOVE     0              TO   K.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  15
              IF   I  NOT  >  J
                   MOVE      ZERO      TO   WK-NUM-TBL-2 (I)
              ELSE
                   ADD       1         TO   K
                   PERFORM   UNTIL  WK-NUM-TBL-1 (K)  IS  NUMERIC
                        ADD       1         TO   K
                   END-PERFORM
                   MOVE      WK-NUM-TBL-1 (K) TO  WK-NUM-TBL-2 (I)
              END-IF
     END-PERFORM.
 900-NUM-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   取引先マスタ　ＲＥＡＤ　　　　　　　　　　　 *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HTOKMS    INVALID   KEY
              MOVE      1         TO   INV-FLG
              GO   TO   900-TOK-READ-EXIT
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   店舗マスタ　ＲＥＡＤ　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     READ     HTENMS    NEXT      AT   END
              MOVE      1         TO   END-FLG
              GO   TO   900-TEN-READ-EXIT
     END-READ.
     IF       TEN-F52  NOT  =  R00013
              MOVE      1         TO   END-FLG
              GO   TO   900-TEN-READ-EXIT
     END-IF.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品変換テーブル　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 900-STB-READ           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HSHOTBL   INVALID   KEY
              MOVE      1         TO   INV-FLG
              GO   TO   900-STB-READ-EXIT
     END-READ.
 900-STB-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品名称マスタ　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 900-MEI-READ           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HMEIMS    INVALID   KEY
              MOVE      1         TO   INV-FLG
              GO   TO   900-MEI-READ-EXIT
     END-READ.
 900-MEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   条件ファイル　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 900-JYO-READ           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HJYOKEN   INVALID   KEY
              MOVE      1         TO   INV-FLG
              GO   TO   900-JYO-READ-EXIT
     END-READ.
 900-JYO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   伝票データＦ　　　ＲＥＡＤ　（ＲＡＮ）　　　 *
*--------------------------------------------------------------*
 900-RYO-RAN-READ       SECTION.
     MOVE     0              TO   INV-FLG.
     READ     PCRYOJF   INVALID  KEY
              MOVE      1         TO   INV-FLG
              GO   TO   900-RYO-RAN-READ-EXIT
     END-READ.
 900-RYO-RAN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   伝票データＦ　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 900-RYO-READ           SECTION.
     READ     PCRYOJF   NEXT      AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-RYO-READ-EXIT
     END-READ.
     IF       RYO-F011  NOT  =  R00003
     OR       RYO-F012  NOT  =  R00004
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-RYO-READ-EXIT
     END-IF.
*
     MOVE     RYO-F011       TO   NEW-011.
     MOVE     RYO-F012       TO   NEW-012.
     MOVE     RYO-F02        TO   NEW-02.
 900-RYO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   伝票データＦ　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 TEN-KAKU-SEC           SECTION.
*
     MOVE    SPACE     TO     CHK-FLG.
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 300
                                      OR CHK-FLG = "CHK"
             IF      WK-TENCD(I J) = WK-TENCD-B(X)
                     IF  WK-TENSU-B(X) NOT = ZERO
                         MOVE WK-TENSU-B(X) TO WK-SURYO(I J)
                         MOVE "CHK"         TO CHK-FLG
                     END-IF
             END-IF
     END-PERFORM.
*
 TEN-KAKU-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
