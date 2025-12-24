# SKY1403I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKY1403I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　オフライン出荷システム　　　　　　　　*
*    モジュール名　　：　スポット出荷入力（オフライン）　　　　*
*    作成日／更新日　：　1999/10/19                            *
*    作成者／更新者　：　NAV                                   *
*    更新日／更新者　：　                                 *
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKY1403I.
 AUTHOR.                T.TAKAHASHI.
 DATE-WRITTEN.          99/10/19.
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
*----<< 倉庫別_番マスタ >>-*
     SELECT   JHMTANF   ASSIGN    TO        DA-01-VI-JHMTANL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TAN-F01   TAN-F02
                        FILE      STATUS    TAN-ST1   TAN-ST2.
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
     SELECT   JHTSPTF   ASSIGN    TO        DA-01-VI-JHTSPTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       SPT-F011  SPT-F012
                                            SPT-F02
                        FILE      STATUS    SPT-ST1   SPT-ST2.
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
*----<< 倉庫別_番マスタ  >>-*
 FD  JHMTANF
                        LABEL     RECORD     IS  STANDARD.
     COPY     JHMTANF   OF   XFDLIB    JOINING   TAN  AS   PREFIX.
*----<< 商品名称マスタ >>-*
 FD  HMEIMS             BLOCK     CONTAINS   6   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HMEIMS    OF   XFDLIB    JOINING   MEI  AS   PREFIX.
*----<< 条件ファイル >>-*
 FD  HJYOKEN            BLOCK     CONTAINS   6   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HJYOKEN   OF   XFDLIB    JOINING   JYO  AS   PREFIX.
*----<< 伝票データ >>-*
 FD  JHTSPTF            BLOCK     CONTAINS   4   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     JHTSPTF   OF   XFDLIB    JOINING   SPT  AS   PREFIX.
*----<< 画面ファイル >>-*
 FD  DSPFILE.
     COPY     FKY14031   OF   XMDLIB.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  PGM-ID                  PIC  X(08)     VALUE  "SSY0601I".
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
     03  TAN-STATUS.
         05  TAN-ST1         PIC  X(02).
         05  TAN-ST2         PIC  X(04).
     03  MEI-STATUS.
         05  MEI-ST1         PIC  X(02).
         05  MEI-ST2         PIC  X(04).
     03  JYO-STATUS.
         05  JYO-ST1         PIC  X(02).
         05  JYO-ST2         PIC  X(04).
     03  SPT-STATUS.
         05  SPT-ST1         PIC  X(02).
         05  SPT-ST2         PIC  X(04).
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
                             NC"Ｙ，Ｈを入力".
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
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       18.
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
 01  WK-MEMO-NO              PIC  9(06).
 01  FILLER                  REDEFINES   WK-MEMO-NO.
     03  WK-MEMO-NO-1        PIC  9(04).
     03  WK-MEMO-NO-2        PIC  9(02).
 01  GET-MEMO-NO             PIC  9(06).
 01  FILLER                  REDEFINES   GET-MEMO-NO.
     03  GET-MEMO-NO-1       PIC  9(04).
     03  GET-MEMO-NO-2       PIC  9(02).
 01  WK-R00016               PIC  X(13).
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
     03  TAN-OPEN-FLG        PIC  9(01).
     03  MEI-OPEN-FLG        PIC  9(01).
     03  JYO-OPEN-FLG        PIC  9(01).
     03  SPT-OPEN-FLG        PIC  9(01).
     03  HAND-FLG            PIC  9(01).
 01  COUNTERS.
     03  NUM-CNT             PIC  9(02).
 01  BREAK-KEY.
     03  NEW.
         05  NEW-01.
             07  NEW-011     PIC  9(04).
             07  NEW-012     PIC  9(02).
         05  NEW-02          PIC  9(01).
     03  OLD.
         05  OLD-01.
             07  OLD-011     PIC  9(04).
             07  OLD-012     PIC  9(02).
         05  OLD-02          PIC  9(01).
 01  DSP-WORK.
     03  WK-KBN           PIC  9(01).
     03  WK-HATDT           PIC  9(06).
     03  WK-NOUDT           PIC  9(06).
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
 01  LINK-YMD-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
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
*システム日付変換用
  01  G-DATE.
     03  G-DATE-YY                PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  G-DATE-MM                PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  G-DATE-DD                PIC  9(02)  VALUE  ZERO.
*システム日付変換用
  01  G-TIME.
     03  G-TIME-HH                PIC  Z9.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  G-TIME-MM                PIC  Z9.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  G-TIME-SS                PIC  Z9.
*システム日付保存用
  01  WK-SAV-DATE.
     03  WK-SAV-YY                PIC  9(04)  VALUE  ZERO.
     03  WK-SAV-MM                PIC  9(02)  VALUE  ZERO.
     03  WK-SAV-DD                PIC  9(02)  VALUE  ZERO.
*****************************************************************
 LINKAGE                   SECTION.
 01  LINK-SOKCD              PIC  9(02).
******************************************************************
 PROCEDURE                 DIVISION   USING  LINK-SOKCD.
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
              "ST1=" TOK-ST1 " ST2=" TOK-ST2 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       TAN-OPEN-FLG  =  1  CLOSE    JHMTANF.
     IF       MEI-OPEN-FLG  =  1  CLOSE    HMEIMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       SPT-OPEN-FLG  =  1  CLOSE    JHTSPTF.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     255            TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  店舗マスタ　--------------------------------------*
 TEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HTENMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"店舗マスタ異常！"
              "ST1=" TEN-ST1 " ST2=" TEN-ST2 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       TAN-OPEN-FLG  =  1  CLOSE    JHMTANF.
     IF       MEI-OPEN-FLG  =  1  CLOSE    HMEIMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       SPT-OPEN-FLG  =  1  CLOSE    JHTSPTF.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     255            TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  倉庫別_番マスタ　--------------------------------*
 TAN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   JHMTANF.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"倉庫別_番マスタ異常！"
              "ST1=" TAN-ST1 " ST2=" TAN-ST2 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       TAN-OPEN-FLG  =  1  CLOSE    JHMTANF.
     IF       MEI-OPEN-FLG  =  1  CLOSE    HMEIMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       SPT-OPEN-FLG  =  1  CLOSE    JHTSPTF.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     255            TO   PROGRAM-STATUS.
     STOP     RUN.
*----------   商品名称マスタ　 --------------------------------*
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HMEIMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"商品名称マスタ異常！"
              "ST1=" MEI-ST1 " ST2=" MEI-ST2 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       TAN-OPEN-FLG  =  1  CLOSE    JHMTANF.
     IF       MEI-OPEN-FLG  =  1  CLOSE    HMEIMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       SPT-OPEN-FLG  =  1  CLOSE    JHTSPTF.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     255            TO   PROGRAM-STATUS.
     STOP     RUN.
*----------   条件ファイル　-----------------------------------*
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HJYOKEN.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"条件ファイル異常！"
              "ST1=" JYO-ST1 " ST2=" JYO-ST2 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       TAN-OPEN-FLG  =  1  CLOSE    JHMTANF.
     IF       MEI-OPEN-FLG  =  1  CLOSE    HMEIMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       SPT-OPEN-FLG  =  1  CLOSE    JHTSPTF.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     255            TO   PROGRAM-STATUS.
     STOP     RUN.
*----------   スポット出荷ファイル  ---------------------------*
 SPT-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   JHTSPTF.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"量販データＦ異常！"
              "ST1=" SPT-ST1 " ST2=" SPT-ST2 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       TAN-OPEN-FLG  =  1  CLOSE    JHMTANF.
     IF       MEI-OPEN-FLG  =  1  CLOSE    HMEIMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       SPT-OPEN-FLG  =  1  CLOSE    JHTSPTF.
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     255            TO   PROGRAM-STATUS.
     STOP     RUN.
*----------    表示ファイル -----------------------------------*
 DSP-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   DSPFILE.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"表示ファイル異常！"
              "ST1=" DSP-ST1 " ST2=" DSP-ST2 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     IF       DSP-OPEN-FLG  =  1  CLOSE    DSPFILE.
     IF       TOK-OPEN-FLG  =  1  CLOSE    HTOKMS.
     IF       TEN-OPEN-FLG  =  1  CLOSE    HTENMS.
     IF       TAN-OPEN-FLG  =  1  CLOSE    JHMTANF.
     IF       MEI-OPEN-FLG  =  1  CLOSE    HMEIMS.
     IF       JYO-OPEN-FLG  =  1  CLOSE    HJYOKEN.
     IF       SPT-OPEN-FLG  =  1  CLOSE    JHTSPTF.
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
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " START "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STAT.
*
     MOVE        ZERO      TO        FLAGS.
*
     OPEN     INPUT     HTOKMS.
     MOVE     1              TO   TOK-OPEN-FLG.
     OPEN     INPUT     HTENMS.
     MOVE     1              TO   TEN-OPEN-FLG.
     OPEN     INPUT     JHMTANF.
     MOVE     1              TO   TAN-OPEN-FLG.
     OPEN     INPUT     HMEIMS.
     MOVE     1              TO   MEI-OPEN-FLG.
     OPEN     I-O       HJYOKEN.
     MOVE     1              TO   JYO-OPEN-FLG.
     OPEN     I-O       JHTSPTF.
     MOVE     1              TO   SPT-OPEN-FLG.
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
     REWRITE  JYO-REC.
*
     MOVE     SYS-YY         TO   START-YY
                                  END-YY.
     MOVE     SYS-MM         TO   START-MM
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
*
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
     PERFORM     220-DSP-KBN      UNTIL     GR-NO  NOT  =  1.
* メモ_入力
     PERFORM     230-DSP-MEMO     UNTIL     GR-NO  NOT  =  2.
* ヘッダ部入力
     PERFORM     240-DSP-HEAD     UNTIL     GR-NO  NOT  =  3.
* 数量入力
     PERFORM     250-DSP-SURYO    UNTIL     GR-NO  NOT  =  4.
* 確認入力
     PERFORM     260-DSP-KAKUNIN  UNTIL     GR-NO  NOT  =  5.
 200-MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE              HTOKMS
                        HTENMS
                        JHMTANF
                        HMEIMS
                        HJYOKEN
                        JHTSPTF
                        DSPFILE.
*
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " END   "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STAT.
 300-END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL    ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE     SPACE          TO   FKY14031.
*-- 1999/10/05 日付ﾁｪｯｸ変更
     INITIALIZE                   LINK-YMD-AREA.
     MOVE       "3"          TO   LINK-IN-KBN.
     MOVE        SYS-DATE    TO   LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING LINK-IN-KBN
                                   LINK-IN-YMD6
                                   LINK-IN-YMD8
                                   LINK-OUT-RET
                                   LINK-OUT-YMD8.
     MOVE     LINK-OUT-YMD8        TO  WK-SAV-DATE.
     MOVE     WK-SAV-YY            TO  G-DATE-YY.
     MOVE     WK-SAV-MM            TO  G-DATE-MM.
     MOVE     WK-SAV-DD            TO  G-DATE-DD.
     MOVE     G-DATE               TO  SDATE.
     MOVE     WK-SAV-DATE(3:6)     TO  HATDT NOUDT.
*システム時間セット
     MOVE     SYS-TIME(1:2)        TO  G-TIME-HH.
     MOVE     SYS-TIME(3:2)        TO  G-TIME-MM.
     MOVE     SYS-TIME(5:2)        TO  G-TIME-SS.
     MOVE     G-TIME               TO  STIME.
*プログラムＩＤセット
     MOVE     "SKY1403I"           TO  PGID.
*ＦＯＲＭＩＤセット
     MOVE     "FKY14031"           TO  FORMID.
*倉庫コードセット
     MOVE     LINK-SOKCD           TO  SOKCD.
*
     MOVE     SPACE                TO  DSP-CNTL.
     MOVE     "FKY14031"           TO  DSP-FMT.
     MOVE     "ALLF"               TO  DSP-GRP.
*
     MOVE     1                    TO  GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      処理区分　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 220-DSP-KBN            SECTION.
     MOVE     G001           TO   GUIDE.
     MOVE     "SYORI"        TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   220-DSP-KBN-EXIT
     WHEN     PF05
              MOVE      99        TO   GR-NO
              GO   TO   220-DSP-KBN-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   220-DSP-KBN-EXIT
     END-EVALUATE.
*
     IF       KBN  IS  NOT  NUMERIC
              MOVE      0         TO   KBN
     END-IF.
     IF       KBN  NOT  =  1  AND  2  AND  3
              MOVE      2         TO   ERR-FLG
              GO   TO   220-DSP-KBN-EXIT
     END-IF.
*
     MOVE     2         TO   GR-NO.
 220-DSP-KBN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      メモ_　　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 230-DSP-MEMO           SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "MEMO"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   230-DSP-MEMO-EXIT
     WHEN     PF06
              MOVE      1         TO   GR-NO
              GO   TO   230-DSP-MEMO-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   230-DSP-MEMO-EXIT
     END-EVALUATE.
*
     IF       MEMO1  IS  NOT  NUMERIC
              MOVE      0         TO   MEMO1
     END-IF.
     IF       MEMO2  IS  NOT  NUMERIC
              MOVE      0         TO   MEMO2
     END-IF.
*
     MOVE     0              TO   HAND-FLG.
     IF       KBN  =  1
     AND      MEMO1  =  0
     AND      MEMO2  =  0
              PERFORM   900-MEMO-GET
              MOVE      GET-MEMO-NO-1  TO   MEMO1
              MOVE      GET-MEMO-NO-2  TO   MEMO2
     ELSE
              IF   MEMO2  = 0
                   MOVE      3         TO   ERR-FLG
                   GO   TO   230-DSP-MEMO-EXIT
              END-IF
              IF   KBN  = 1
                 MOVE     1              TO   HAND-FLG
                 MOVE     52             TO   JYO-F01
                 MOVE     DSP-WS         TO   JYO-F02
                 PERFORM  900-JYO-READ
                 IF       INV-FLG  =  1
                          DISPLAY   NC"メモ_採番エラー"
                                    JYO-F01 JYO-F02   UPON CONS
                          STOP  RUN
                 END-IF
                 MOVE     MEMO1         TO   WK-MEMO-NO-1
                 MOVE     MEMO2         TO   WK-MEMO-NO-2
                 IF       WK-MEMO-NO  <  JYO-F04
                          MOVE      16        TO   ERR-FLG
                          GO   TO   230-DSP-MEMO-EXIT
                 END-IF
                 IF       WK-MEMO-NO  <  JYO-F05
                 OR       WK-MEMO-NO  >  JYO-F06
                          MOVE      17        TO   ERR-FLG
                          GO   TO   230-DSP-MEMO-EXIT
                 END-IF
              END-IF
     END-IF.
*  全レコード解放（排他制御用）
     CLOSE    JHTSPTF.
     OPEN     I-O   JHTSPTF.
*
     MOVE     MEMO1         TO   SPT-F011.
     MOVE     MEMO2         TO   SPT-F012.
     MOVE     0              TO   SPT-F02.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     START    JHTSPTF   KEY  NOT  <  SPT-F011    SPT-F012
                                     SPT-F02
              INVALID   KEY
                   MOVE      HIGH-VALUE     TO   BREAK-KEY
     END-START.
     IF       NEW  NOT  =  HIGH-VALUE
              PERFORM   900-RYO-READ
     END-IF.
*
     IF       NEW  NOT  =  HIGH-VALUE
     AND      KBN  =  1
              MOVE      4         TO   ERR-FLG
              GO   TO   230-DSP-MEMO-EXIT
     END-IF.
     IF       NEW  =  HIGH-VALUE
     AND      KBN   NOT  =  1
              MOVE      5         TO   ERR-FLG
              GO   TO   230-DSP-MEMO-EXIT
     END-IF.
*
     MOVE     1              TO   YOBI-NO.
     IF       KBN  NOT  =  1
              PERFORM   900-RYO-TO-DSP
     END-IF.
*
     EVALUATE KBN
         WHEN   1
              MOVE      3         TO   GR-NO
         WHEN   2
              MOVE      3         TO   GR-NO
         WHEN   3
              MOVE      "Y"       TO   KKNN
              MOVE      5         TO   GR-NO
     END-EVALUATE.
 230-DSP-MEMO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      商品情報入力                                *
*--------------------------------------------------------------*
 240-DSP-HEAD           SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "HEAD1"        TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   240-DSP-HEAD-EXIT
     WHEN     PF06
              MOVE      2         TO   GR-NO
              GO   TO   240-DSP-HEAD-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   240-DSP-HEAD-EXIT
     END-EVALUATE.
*
     MOVE     HATDT         TO   CHK-DATE.
*-- 1999/10/05 日付ﾁｪｯｸ変更
     INITIALIZE                     LINK-YMD-AREA.
     MOVE       "3"         TO      LINK-IN-KBN.
     MOVE        HATDT      TO      LINK-IN-YMD6.
     CALL       "SKYDTCKB"  USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
                 MOVE   ZERO            TO   ERR-FLG
                 MOVE   LINK-OUT-YMD8   TO   CHK-DATE
                 PERFORM  900-DATE-CHECK
     ELSE
                 MOVE   8       TO   ERR-FLG
     END-IF.
*-- 1999/10/05 日付ﾁｪｯｸ変更
     INITIALIZE                      LINK-YMD-AREA.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        NOUDT    TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
                 MOVE   ZERO            TO   ERR-FLG
                 MOVE   LINK-OUT-YMD8   TO   CHK-DATE
                 PERFORM  900-DATE-CHECK
     ELSE
                 MOVE   8       TO   ERR-FLG
                 GO   TO   240-DSP-HEAD-EXIT
     END-IF.
*
     IF       TOKCD  IS  NOT  NUMERIC
              MOVE      0         TO   TOKCD
     END-IF.
     MOVE     TOKCD         TO   TOK-F01.
     PERFORM  900-TOK-READ.
     IF       INV-FLG  =  1
              MOVE      9         TO   ERR-FLG
              GO   TO   240-DSP-HEAD-EXIT
     END-IF.
     MOVE     TOK-F03        TO   TOKNM.
*
     IF       KBN  =  1
              MOVE      TOKCD     TO   TEN-F52
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
*
*商品ＣＤ数字変換
*８桁　頭０詰
     IF  SYOCD1    NOT NUMERIC
     AND SYOCD1    NOT = SPACE
         MOVE   SYOCD1     TO  WK-R020I-X
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
           MOVE   WK-R020          TO  SYOCD1
         END-IF
     END-IF.
*５桁　頭空白詰
     IF  SYOCD2     NOT = SPACE
     AND SYOCD2   (5:1) = SPACE
         MOVE   SYOCD2     TO  WK-R021I-X
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
         MOVE   WK-R021          TO  SYOCD2
     END-IF.
*
     MOVE     SYOCD1         TO   MEI-F011        TAN-F021.
     MOVE     SYOCD2         TO   MEI-F012 (1:5)  TAN-F022.
     MOVE     SYOCD3         TO   MEI-F012 (6:2)  TAN-F023.
     MOVE     SYOCD4         TO   MEI-F012 (8:1)  TAN-F024.
     PERFORM  900-MEI-READ.
     IF       INV-FLG  =  1
              MOVE      10        TO   ERR-FLG
              GO   TO   240-DSP-HEAD-EXIT
     END-IF.
     MOVE     MEI-F031       TO   SYOKN1.
     MOVE     MEI-F032       TO   SYOKN2.
     MOVE     MEI-F042       TO   GENTAN.
     MOVE     MEI-F043       TO   BAITAN.
*倉庫別_番マスタ検索
     MOVE     LINK-SOKCD     TO   TAN-F01.
     PERFORM  900-JHMTANF-READ.
     IF       INV-FLG  =  1
              MOVE      10        TO   ERR-FLG
              GO   TO   240-DSP-HEAD-EXIT
     END-IF.
     MOVE     TAN-F03        TO   TANCD.
*
     MOVE     4              TO   GR-NO.
 240-DSP-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      数量　入力　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 250-DSP-SURYO          SECTION.
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
     MOVE     "BODY1"        TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   250-DSP-SURYO-EXIT
     WHEN     PF06
              MOVE      3         TO   GR-NO
              GO   TO   250-DSP-SURYO-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   250-DSP-SURYO-EXIT
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
     MOVE   "Y"     TO   KKNN.
*
     MOVE     5              TO   GR-NO.
 250-DSP-SURYO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　入力　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 260-DSP-KAKUNIN       SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "KKNN"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
              GO   TO   260-DSP-KAKUNIN-EXIT
     WHEN     PF06
              EVALUATE  KBN
                   WHEN  1
                   WHEN  2
                        MOVE      4         TO   GR-NO
                   WHEN  3
                        MOVE      2         TO   GR-NO
              END-EVALUATE
              GO   TO   260-DSP-KAKUNIN-EXIT
     WHEN     ENT
              CONTINUE
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
              GO   TO   260-DSP-KAKUNIN-EXIT
     END-EVALUATE.
*
     IF       KKNN  NOT  =  "Y"
     AND      KBN  =  1
              MOVE      13        TO   ERR-FLG
              GO   TO   260-DSP-KAKUNIN-EXIT
     END-IF.
     IF       KKNN  NOT  =  "Y"
     AND      KBN  NOT  =  1
              MOVE      14        TO   ERR-FLG
              GO   TO   260-DSP-KAKUNIN-EXIT
     END-IF.
*
     IF       KKNN  =  SPACE
              GO   TO   260-DSP-KAKUNIN-EXIT
     END-IF.
     EVALUATE   KBN
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
         EVALUATE  KBN
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
                   MOVE      4         TO   GR-NO
              ELSE
                   IF  HAND-FLG = 1
                       PERFORM   900-MEMO-HAND
                   END-IF
                   MOVE      KBN    TO   WK-KBN
                   MOVE      HATDT    TO   WK-HATDT
                   MOVE      NOUDT    TO   WK-NOUDT
                   MOVE      SPACE          TO   FKY14031
                   MOVE      WK-KBN         TO   KBN
*システム日付セット
                   MOVE      WK-SAV-YY      TO   G-DATE-YY
                   MOVE      WK-SAV-MM      TO   G-DATE-MM
                   MOVE      WK-SAV-DD      TO   G-DATE-DD
                   MOVE      G-DATE         TO   SDATE
                   MOVE      WK-SAV-DATE(3:6)    TO   HATDT NOUDT
*システム時間セット
                   MOVE      SYS-TIME(1:2)  TO   G-TIME-HH
                   MOVE      SYS-TIME(3:2)  TO   G-TIME-MM
                   MOVE      SYS-TIME(5:2)  TO   G-TIME-SS
                   MOVE      G-TIME         TO   STIME
                   MOVE      2              TO   GR-NO
              END-IF
              GO   TO   260-DSP-KAKUNIN-EXIT
           WHEN  2
              PERFORM   900-RYO-READ
              IF   NEW  NOT  = HIGH-VALUE
                   PERFORM   900-RYO-TO-DSP
                   MOVE      4         TO   GR-NO
              ELSE
                   MOVE      KBN            TO   WK-KBN
                   MOVE      HATDT          TO   WK-HATDT
                   MOVE      NOUDT          TO   WK-NOUDT
                   MOVE      SPACE          TO   FKY14031
                   MOVE      WK-KBN         TO   KBN
*システム日付セット
                   MOVE      WK-SAV-YY      TO   G-DATE-YY
                   MOVE      WK-SAV-MM      TO   G-DATE-MM
                   MOVE      WK-SAV-DD      TO   G-DATE-DD
                   MOVE      G-DATE         TO   SDATE
                   MOVE      WK-SAV-DATE(3:6)    TO   HATDT NOUDT
*システム時間セット
                   MOVE      SYS-TIME(1:2)  TO   G-TIME-HH
                   MOVE      SYS-TIME(3:2)  TO   G-TIME-MM
                   MOVE      SYS-TIME(5:2)  TO   G-TIME-SS
                   MOVE      G-TIME         TO   STIME
                   MOVE      2              TO   GR-NO
              END-IF
              GO   TO   260-DSP-KAKUNIN-EXIT
           WHEN  3
              MOVE      KBN            TO   WK-KBN
              MOVE      HATDT          TO   WK-HATDT
              MOVE      NOUDT          TO   WK-NOUDT
              MOVE      SPACE          TO   FKY14031
              MOVE      WK-KBN         TO   KBN
*システム日付セット
              MOVE      WK-SAV-YY      TO   G-DATE-YY
              MOVE      WK-SAV-MM      TO   G-DATE-MM
              MOVE      WK-SAV-DD      TO   G-DATE-DD
              MOVE      G-DATE         TO   SDATE
              MOVE      WK-SAV-DATE(3:6)    TO   HATDT NOUDT
*システム時間セット
              MOVE      SYS-TIME(1:2)  TO   G-TIME-HH
              MOVE      SYS-TIME(3:2)  TO   G-TIME-MM
              MOVE      SYS-TIME(5:2)  TO   G-TIME-SS
              MOVE      G-TIME         TO   STIME
              MOVE      2              TO   GR-NO
              GO   TO   260-DSP-KAKUNIN-EXIT
     END-IF.
 260-DSP-KAKUNIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     量販データ登録・修正　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 900-RYO-WRITE          SECTION.
     INITIALIZE    SPT-REC.
     MOVE     MEMO1          TO   SPT-F011.
     MOVE     MEMO2          TO   SPT-F012.
     MOVE     YOBI-NO        TO   SPT-F02.
     PERFORM  900-RYO-RAN-READ.
*-- 1999/10/05 日付ﾁｪｯｸ変更
     INITIALIZE                   LINK-YMD-AREA.
     MOVE     "3"            TO   LINK-IN-KBN.
     MOVE     HATDT          TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"  USING   LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     MOVE    LINK-OUT-YMD8   TO   SPT-F071.
*-- 1999/10/05 日付ﾁｪｯｸ変更
     INITIALIZE                    LINK-YMD-AREA.
     MOVE     "3"            TO   LINK-IN-KBN.
     MOVE     NOUDT          TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"  USING   LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     MOVE    LINK-OUT-YMD8   TO   SPT-F072.
     MOVE     TOKCD         TO   SPT-F11.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  5
         MOVE     BDY (I)    TO   WK-BDY (I)
         PERFORM  VARYING  J  FROM  1  BY  1  UNTIL  J  > 10
              COMPUTE   K  =  (I - 1) * 10 + J
              IF   WK-TENCD (I J)  IS  NUMERIC
                   MOVE      WK-TENCD (I J)    TO   SPT-F161 (K)
                   MOVE      WK-SURYO (I J)    TO   SPT-F162 (K)
              ELSE
                   MOVE      0                 TO   SPT-F161 (K)
                                                    SPT-F162 (K)
              END-IF
         END-PERFORM
     END-PERFORM.
     IF       KBN  =  1
              MOVE      9         TO   SPT-F573
     END-IF.
     MOVE     0              TO   SPT-F58.
     MOVE     TANCD          TO   SPT-F06.
     MOVE     SYOCD1         TO   SPT-F62 (1:8).
     MOVE     SYOCD2         TO   SPT-F62 (9:5).
     MOVE     SYOCD3         TO   SPT-F62 (14:2).
     MOVE     SYOCD4         TO   SPT-F62 (16:1).
     MOVE     SYOKN1         TO   SPT-F1211.
     MOVE     SYOKN2         TO   SPT-F1212.
*--  1999/10/05  日付ﾁｪｯｸ変更
     INITIALIZE                   LINK-YMD-AREA.
     MOVE     "3"            TO   LINK-IN-KBN.
     MOVE     SYS-DATE       TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"  USING   LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     MOVE     LINK-OUT-YMD8  TO   SPT-F99.
*--
     IF       INV-FLG  =  1
              WRITE     SPT-REC
     ELSE
              REWRITE   SPT-REC
     END-IF.
 900-RYO-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     量販データ削除　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 900-RYO-DELETE         SECTION.
     MOVE     MEMO1          TO   SPT-F011.
     MOVE     MEMO2          TO   SPT-F012.
     MOVE     0              TO   SPT-F02.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     START    JHTSPTF  KEY  NOT  <  SPT-F011  SPT-F012
                                    SPT-F02
              INVALID  KEY
                   MOVE      HIGH-VALUE     TO   BREAK-KEY
     END-START.
     PERFORM  900-RYO-READ.
     PERFORM  UNTIL  NEW  =  HIGH-VALUE
              DELETE         JHTSPTF  RECORD
              PERFORM  900-RYO-READ
     END-PERFORM.
 900-RYO-DELETE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     量販データ読込み　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 900-RYO-TO-DSP         SECTION.
     MOVE     SPT-F02        TO   YOBI-NO.
*--
     MOVE     SPT-F071       TO   HATDT.
     MOVE     SPT-F072       TO   NOUDT.
     MOVE     SPT-F11        TO   TOKCD
                                  TOK-F01.
     PERFORM  900-TOK-READ.
     IF       INV-FLG  =  0
              MOVE      TOK-F03   TO   TOKNM
     ELSE
              MOVE      NC"＊＊＊＊＊＊＊＊＊＊"  TO   TOKNM
     END-IF.
     MOVE     SPT-F07        TO   TEN-F011.
     MOVE     SPT-F62 (1:8)  TO   SYOCD1 MEI-F011      TAN-F021.
     MOVE     SPT-F62 (9:5)  TO   SYOCD2 MEI-F012(1:5) TAN-F022.
     MOVE     SPT-F62 (14:2) TO   SYOCD3 MEI-F012(6:2) TAN-F023.
     MOVE     SPT-F62 (16:1) TO   SYOCD4 MEI-F012(8:1) TAN-F024.
*
     PERFORM  900-MEI-READ.
     IF       INV-FLG  =  1
              MOVE     ALL "*"    TO   SYOKN1
              MOVE     ALL "*"    TO   SYOKN2
              MOVE     ZERO       TO   GENTAN
              MOVE     ZERO       TO   BAITAN
     ELSE
              MOVE     MEI-F031   TO   SYOKN1
              MOVE     MEI-F032   TO   SYOKN2
              MOVE     MEI-F042   TO   GENTAN
              MOVE     MEI-F043   TO   BAITAN
     END-IF.
*倉庫別_番マスタ検索
     MOVE     LINK-SOKCD     TO   TAN-F01.
     PERFORM  900-JHMTANF-READ.
     IF       INV-FLG  =  1
              MOVE     SPACE          TO   TANCD
     ELSE
              MOVE     TAN-F03        TO   TANCD
     END-IF.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  5
         MOVE     SPACE      TO   WK-BDY (I)
         PERFORM  VARYING  J  FROM  1  BY  1  UNTIL  J  > 10
              COMPUTE   K  =  (I - 1) * 10 + J
              IF   SPT-F161 (K)  NOT  =  0
                   MOVE      SPT-F161 (K)      TO   WK-TENCD (I J)
                   MOVE      SPT-F162 (K)      TO   WK-SURYO (I J)
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
     MOVE      SPACE               TO  DSP-PRO.
*プログラムＩＤセット
     MOVE     "SKY1403I"           TO  PGID.
*ＦＯＲＭＩＤセット
     MOVE     "FKY14031"           TO  FORMID.
*倉庫コードセット
     MOVE     LINK-SOKCD           TO  SOKCD.
     WRITE    FKY14031.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     メモ番号取得                                *
*--------------------------------------------------------------*
 900-MEMO-GET           SECTION.
     MOVE     52             TO   JYO-F01.
     MOVE     DSP-WS         TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG  =  1
              DISPLAY   NC"メモ_採番エラー"
                        JYO-F01 JYO-F02       UPON CONS
              STOP  RUN
     END-IF.
     MOVE     JYO-F04        TO   WK-MEMO-NO
                                  GET-MEMO-NO.
     ADD      1              TO   WK-MEMO-NO-2.
     IF       WK-MEMO-NO-2  =  0
              ADD       1         TO   WK-MEMO-NO-1
              MOVE      1         TO   WK-MEMO-NO-2
              IF   WK-MEMO-NO  >  JYO-F06
              OR   WK-MEMO-NO-1 =  ZERO
                   MOVE      JYO-F05   TO   WK-MEMO-NO
              END-IF
     END-IF.
     MOVE     WK-MEMO-NO     TO   JYO-F04.
     REWRITE  JYO-REC.
 900-MEMO-GET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     メモ番号更新（手入力時）
*--------------------------------------------------------------*
 900-MEMO-HAND          SECTION.
     MOVE     52             TO   JYO-F01.
     MOVE     DSP-WS         TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG  =  1
              DISPLAY   NC"メモ_採番エラー"
                        JYO-F01 JYO-F02  UPON CONS
              STOP  RUN
     END-IF.
     MOVE     MEMO1         TO   WK-MEMO-NO-1.
     MOVE     MEMO2         TO   WK-MEMO-NO-2.
     ADD      1              TO   WK-MEMO-NO-2.
     IF       WK-MEMO-NO-2  =  0
              MOVE      1         TO   WK-MEMO-NO-2
              ADD       1         TO   WK-MEMO-NO-1
              IF   WK-MEMO-NO   >  JYO-F06
              OR   WK-MEMO-NO-1 =  ZERO
                   MOVE      JYO-F05   TO   WK-MEMO-NO
              END-IF
     END-IF.
     MOVE     WK-MEMO-NO     TO   JYO-F04.
     REWRITE  JYO-REC.
 900-MEMO-HAND-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     日付チェック　　                            *
*--------------------------------------------------------------*
 900-DATE-CHECK         SECTION.
     IF     ( CHK-YYMM  <  START-YYMM
     OR       CHK-YYMM  >  END-YYMM )
              MOVE      10        TO   ERR-FLG
*-- 1999/10/05 ｴﾗｰﾌﾗｸﾞ変更  10 ---> 8
              MOVE      8         TO   ERR-FLG
*--
              GO   TO   900-DATE-CHECK-EXIT
     END-IF.
 900-DATE-CHECK-EXIT.
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
     IF       TEN-F52  NOT  =  TOKCD
              MOVE      1         TO   END-FLG
              GO   TO   900-TEN-READ-EXIT
     END-IF.
 900-TEN-READ-EXIT.
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
*    LEVEL  ALL   倉庫別_番マスタ　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 900-JHMTANF-READ       SECTION.
     MOVE     0         TO   INV-FLG.
     READ     JHMTANF   INVALID   KEY
              MOVE      1         TO   INV-FLG
              GO   TO   900-JHMTANF-READ-EXIT
     END-READ.
 900-JHMTANF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   伝票データＦ　　　ＲＥＡＤ　（ＲＡＮ）　　　 *
*--------------------------------------------------------------*
 900-RYO-RAN-READ       SECTION.
     MOVE     0              TO   INV-FLG.
     READ     JHTSPTF   INVALID  KEY
              MOVE      1         TO   INV-FLG
              GO   TO   900-RYO-RAN-READ-EXIT
     END-READ.
 900-RYO-RAN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   伝票データＦ　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 900-RYO-READ           SECTION.
     READ     JHTSPTF   NEXT      AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-RYO-READ-EXIT
     END-READ.
     IF       SPT-F011  NOT  =  MEMO1
     OR       SPT-F012  NOT  =  MEMO2
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-RYO-READ-EXIT
     END-IF.
*
     MOVE     SPT-F011       TO   NEW-011.
     MOVE     SPT-F012       TO   NEW-012.
     MOVE     SPT-F02        TO   NEW-02.
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
