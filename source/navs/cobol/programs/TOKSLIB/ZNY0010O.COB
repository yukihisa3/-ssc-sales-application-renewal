****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）                    *
*    業務名　　　　　：　在庫管理システム                      *
*    モジュール名　　：　作業実績入力                          *
*    作成日　　　　　：  93/04/30                              *
*    作成者　　　　　：  NAV                                   *
*    更新日　　　　　：  **/**/**                              *
*    更新者　　　　　：                                        *
*    更新内容　　　　：                                        *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZNY0010O.
 AUTHOR.                K.S.
 DATE-WRITTEN.          93/04/30.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STA.
***************************************************************
 INPUT-OUTPUT           SECTION.
***************************************************************
 FILE-CONTROL.
*----<< 作業実績ファイル >>-*
     SELECT   ZSGYODT   ASSIGN    TO        DA-01-VI-ZSGYODT1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       SGY-F01   SGY-F02
                        FILE      STATUS    SGY-ST1.
*----<< 商品在庫マスタ >>-*
     SELECT   ZZAIMS    ASSIGN    TO        DA-01-VI-ZZAIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       ZAI-F01
                                            ZAI-F02
                                            ZAI-F03
                        FILE      STATUS    ZAI-ST1.
*----<< 条件ファイル >>-*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-ST1.
*----<< 倉庫マスタ >>-*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE      STATUS    SOK-ST1.
*----<< 商品名称マスタ >>-*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       MEI-F011  MEI-F0121
                                            MEI-F0122 MEI-F0123
                        FILE      STATUS    MEI-ST1.
*----<< 画面ファイル >>-*
     SELECT   DSPFILE   ASSIGN    TO        GS-DSPF
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
*----<< 作業実績ファイル >>-*
 FD  ZSGYODT            LABEL     RECORD     IS  STANDARD.
     COPY     ZSGYODT1  OF   XFDLIB    JOINING   SGY  AS   PREFIX.
*----<< 商品在庫マスタ >>-*
 FD  ZZAIMS             LABEL     RECORD     IS  STANDARD.
     COPY     ZZAIMS1   OF   XFDLIB    JOINING   ZAI  AS   PREFIX.
*----<< 条件ファイル >>-*
 FD  HJYOKEN            LABEL     RECORD     IS  STANDARD.
     COPY     JYOKEN1   OF   XFDLIB    JOINING   JYO  AS   PREFIX.
*----<< 倉庫マスタ >>-*
 FD  ZSOKMS             LABEL     RECORD     IS  STANDARD.
     COPY     ZSOKMS    OF   XFDLIB    JOINING   SOK  AS   PREFIX.
*----<< 商品名称マスタ >>-*
 FD  HMEIMS             LABEL     RECORD     IS  STANDARD.
     COPY     MEIMS1    OF   XFDLIB    JOINING   MEI  AS   PREFIX.
*----<< 画面ファイル >>-*
 FD  DSPFILE.
     COPY     ZNY0010   OF   XMDLIB.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  PGM-ID                  PIC  X(08)  VALUE  "ZNY0010O".
 01  XXX                     PIC  9(09)V99.
 01  SAV-YMD                 PIC  9(06)  VALUE  ZERO.
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  DSP-CNTL.
     03  DSP-FMT             PIC  X(08)  VALUE  SPACE.
     03  DSP-GRP             PIC  X(08)  VALUE  SPACE.
     03  DSP-PRO             PIC  X(02)  VALUE  SPACE.
     03  DSP-FNC             PIC  X(04)  VALUE  SPACE.
     03  DSP-ST.
       05  DSP-ST1           PIC  X(02)  VALUE  SPACE.
       05  DSP-ST2           PIC  X(04)  VALUE  SPACE.
     03  DSP-CON             PIC  X(06)  VALUE  SPACE.
     03  WK-GRP.
       05  WK-GRP-BODY       PIC  X(04)  VALUE  SPACE.
       05  WK-GRP-LINE       PIC  9(02)  VALUE  ZERO.
       05  FILLER            PIC  X(02)  VALUE  SPACE.
*----<< ｽﾃｰﾀｽ ｴﾘｱ >>-*
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  SGY-ST.
         05  SGY-ST1         PIC  X(02)  VALUE  SPACE.
         05  SGY-ST2         PIC  X(04)  VALUE  SPACE.
     03  ZAI-ST.
         05  ZAI-ST1         PIC  X(02)  VALUE  SPACE.
         05  ZAI-ST2         PIC  X(04)  VALUE  SPACE.
     03  JYO-ST.
         05  JYO-ST1         PIC  X(02)  VALUE  SPACE.
         05  JYO-ST2         PIC  X(04)  VALUE  SPACE.
     03  SOK-ST.
         05  SOK-ST1         PIC  X(02)  VALUE  SPACE.
         05  SOK-ST2         PIC  X(04)  VALUE  SPACE.
     03  MEI-ST.
         05  MEI-ST1         PIC  X(02)  VALUE  SPACE.
         05  MEI-ST2         PIC  X(04)  VALUE  SPACE.
*----<< ﾃﾞｰﾀ ﾃｰﾌﾞﾙ >>-*
 01  TABLE-AREA.
     03  TBL                 OCCURS    24.
         05  FILLER          PIC  X(05).
         05  TBL-GYO101      PIC  9(02).
         05  FILLER          PIC  X(05).
         05  TBL-TNA101      PIC  X(01).
*        05  TBL-TNA101      PIC  X(03).
         05  FILLER          PIC  X(05).
         05  TBL-TNA201      PIC  X(03).
*        05  TBL-TNA201      PIC  X(01).
         05  FILLER          PIC  X(05).
         05  TBL-TNA301      PIC  X(02).
         05  FILLER          PIC  X(05).
         05  TBL-HINCD       PIC  X(08).
         05  FILLER          PIC  X(05).
         05  TBL-TAN101      PIC  X(05).
         05  FILLER          PIC  X(05).
         05  TBL-TAN201      PIC  X(02).
         05  FILLER          PIC  X(05).
         05  TBL-TAN301      PIC  X(01).
         05  FILLER          PIC  X(05).
         05  TBL-SURYO       PIC  9(07)V99.
         05  FILLER          PIC  X(05).
         05  TBL-STK101      PIC  X(04).
         05  FILLER          PIC  X(05).
         05  TBL-STK201      PIC  X(01).
         05  FILLER          PIC  X(05).
         05  TBL-BIKOU       PIC  X(10).
         05  FILLER          PIC  X(05).
         05  TBL-NYSKBN      PIC  X(01).
         05  FILLER          PIC  X(05).
         05  TBL-HINMEI      PIC  N(30).
     03  TBL-SAKJYO          PIC  X(01)     OCCURS  24.
     03  TBL-WTANA1          PIC  X(01)     OCCURS  24.
     03  TBL-WTANA2          PIC  X(03)     OCCURS  24.
*    03  TBL-WTANA1          PIC  X(03)     OCCURS  24.
*    03  TBL-WTANA2          PIC  X(01)     OCCURS  24.
     03  TBL-WTANA3          PIC  X(02)     OCCURS  24.
     03  TBL-WHINCD          PIC  X(08)     OCCURS  24.
     03  TBL-WHTAN1          PIC  X(05)     OCCURS  24.
     03  TBL-WHTAN2          PIC  X(02)     OCCURS  24.
     03  TBL-WHTAN3          PIC  X(01)     OCCURS  24.
     03  TBL-WSURYO          PIC  9(07)V99  OCCURS  24.
     03  TBL-WNYUSKBN        PIC  9(01)     OCCURS  24.
 01  WK-TBL-AREA.
     03  WK-TBL              OCCURS    24.
         05  T-TNA101        PIC  X(01).
         05  T-TNA201        PIC  X(03).
*        05  T-TNA101        PIC  X(03).
*        05  T-TNA201        PIC  X(01).
         05  T-TNA301        PIC  X(02).
         05  T-HINCD         PIC  X(08).
         05  T-TAN101        PIC  X(05).
         05  T-TAN201        PIC  X(02).
         05  T-TAN301        PIC  X(01).
         05  T-SURYO         PIC  9(07)V99.
         05  T-STK101        PIC  X(04).
         05  T-STK201        PIC  X(01).
         05  T-BIKOU         PIC  X(10).
         05  T-NYSKBN        PIC  X(01).
         05  T-HINMEI        PIC  N(30).
         05  T-WTANA1        PIC  X(01).
         05  T-WTANA2        PIC  X(03).
*        05  T-WTANA1        PIC  X(03).
*        05  T-WTANA2        PIC  X(01).
         05  T-WTANA3        PIC  X(02).
         05  T-WHINCD        PIC  X(08).
         05  T-WHTAN1        PIC  X(05).
         05  T-WHTAN2        PIC  X(02).
         05  T-WHTAN3        PIC  X(01).
         05  T-WNYUSKBN      PIC  9(01).
*画面退避用
     COPY   ZNY0010  OF XMDLIB  JOINING   SAV  AS   PREFIX.
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｶﾞｲﾀﾞﾝｽ ｴﾘｱ >>-*
 01  GUIDE-AREA.
     03  G001                PIC  N(30)  VALUE
         NC"_取消_終了".
     03  G002                PIC  N(30)  VALUE
         NC"_取消_終了_項目戻り".
     03  G003                PIC  N(30)  VALUE
         NC"_取消_終了_項目戻り_前頁_次頁".
     03  G004                PIC  N(30)  VALUE
         NC"_取消_終了_項目戻り_前レコード_次レコード".
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
         NC"表示中以外のＰＦキーは使用できません！".
     03  MSG02               PIC  N(20)  VALUE
         NC"処理区分に誤りがあります。".
     03  MSG03               PIC  N(20)  VALUE
         NC"作業実績ファイルに未登録です。".
     03  MSG04               PIC  N(20)  VALUE
         NC"既に登録されています。".
     03  MSG05               PIC  N(20)  VALUE
         NC"日付の入力に誤りがあります。".
     03  MSG06               PIC  N(20)  VALUE
         NC"作業区分に誤りがあります。".
     03  MSG07               PIC  N(20)  VALUE
         NC"作業場所に誤りがあります。".
     03  MSG08               PIC  N(20)  VALUE
         NC"商品名称マスタに未登録です。".
     03  MSG09               PIC  N(20)  VALUE
         NC"数量に０は入力出来ません。".
     03  MSG10               PIC  N(20)  VALUE
         NC"入出庫区分に誤りがあります。".
     03  MSG11               PIC  N(20)  VALUE
         NC"Ｙ，Ｈ，Ｂのいずれかを入力して下さい。".
     03  MSG12               PIC  N(20)  VALUE
         NC"入庫の複数入力はできません。".
     03  MSG13               PIC  N(20)  VALUE
         NC"明細を入力して下さい。".
     03  MSG14               PIC  N(20)  VALUE
         NC"前頁はありません。".
     03  MSG15               PIC  N(20)  VALUE
         NC"次頁はありません。".
     03  MSG16               PIC  N(20)  VALUE
         NC"前レコードは存在しません。".
     03  MSG17               PIC  N(20)  VALUE
         NC"次レコードは存在しません。".
     03  MSG18               PIC  N(20)  VALUE
         NC"入庫、または出庫のみの入力はできません。".
     03  MSG19               PIC  N(20)  VALUE
         NC"ＹまたはＨを入力して下さい。".
     03  MSG20               PIC  N(20)  VALUE
         NC"次頁の入力はできません。".
     03  MSG21               PIC  N(20)  VALUE
         NC"出庫を入力して下さい。".
     03  MSG22               PIC  N(20)  VALUE
         NC"入庫を入力して下さい。".
     03  MSG23               PIC  N(20)  VALUE
         NC"対象の_に商品が有りません。".
     03  MSG24               PIC  N(20)  VALUE
         NC"担当者コードに誤りがあります。".
     03  MSG25               PIC  N(20)  VALUE
         NC"数量が未入力です。".
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       25.
*----<< ﾜｰｸ ｴﾘｱ >>-*
 01  WK-AREA.
     03  WK-BASYO            PIC  9(02)  VALUE  ZERO.
     03  WK-SIMEBI           PIC  9(08)  VALUE  ZERO.
     03  WK-SYSDATE          PIC  9(08)  VALUE  ZERO.
     03  WK-SGYDATE          PIC  9(08)  VALUE  ZERO.
     03  WK-SEIREKI          PIC  9(04)  VALUE  ZERO.
     03  WK-SURYO            PIC S9(07)V99 VALUE  ZERO.
     03  WK-CNT              PIC  9(02)  OCCURS 4.
     03  WK-F01              PIC  9(07)  VALUE  ZERO.
*----<< ｼｽﾃﾑ ﾋﾂﾞｹ･ｼﾞｶﾝ ｴﾘｱ >>-*
 01  SYS-DATE                PIC  9(06).
 01  FILLER                  REDEFINES   SYS-DATE.
     03  SYS-YY              PIC  9(02).
     03  SYS-MM              PIC  9(02).
     03  SYS-DD              PIC  9(02).
*----<< ｼｽﾃﾑ ﾋﾂﾞｹ･ｼﾞｶﾝ ｴﾘｱ >>-*
 01  SYS-DATE2               PIC  9(08).
 01  FILLER                  REDEFINES   SYS-DATE2.
     03  SYS-YYYY2.
         05  SYS-YY1-2       PIC  9(02).
         05  SYS-YY2-2       PIC  9(02).
     03  SYS-MM2             PIC  9(02).
     03  SYS-DD2             PIC  9(02).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
*\\
 01  ZAI-SIME.
     03  ZAI-SIME1           PIC  9(06)    VALUE ZERO.
     03  ZAI-SIME1R          REDEFINES     ZAI-SIME1.
         05  ZAI-SIME1R1     PIC  9(04).
         05  ZAI-SIME1R2     PIC  9(02).
     03  ZAI-SIME2           PIC  9(02)    VALUE ZERO.
*
 01  ZAI-SIMER               REDEFINES ZAI-SIME
                             PIC  9(08).
*
*----<< ﾋﾂﾞｹﾁｪｯｸ ﾜｰｸ ｴﾘｱ >>-*
 01  CHK-DATEX               PIC  9(08).
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
 01  INDEXES.
     03  I                   PIC  9(02)  VALUE  ZERO.
     03  IX                  PIC  9(02)  VALUE  ZERO.
     03  J                   PIC  9(02)  VALUE  ZERO.
     03  K                   PIC  9(02)  VALUE  ZERO.
     03  L                   PIC  9(02)  VALUE  ZERO.
     03  S                   PIC  9(02)  VALUE  ZERO.
     03  T                   PIC  9(02)  VALUE  ZERO.
     03  IXB                 PIC  9(02)  VALUE  ZERO.
     03  IXC                 PIC  9(02)  VALUE  ZERO.
     03  IXD                 PIC  9(02)  VALUE  ZERO.
 01  FLAGS.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  INV-FLG             PIC  9(01)  VALUE  ZERO.
     03  NYU-FLG             PIC  X(01)  VALUE  SPACE.
     03  SYU-FLG             PIC  X(01)  VALUE  SPACE.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  CHK-FLG             PIC  X(01)  VALUE  SPACE.
     03  TBL-FLG             PIC  9(01)  VALUE  ZERO.
     03  REW-FLG             PIC  9(01)  VALUE  ZERO.
     03  SGY-FLG             PIC  9(01)  VALUE  ZERO.
     03  SHORI-F             PIC  9(02)  VALUE  ZERO.
 01  COUNTERS.
     03  PAGE-CNT            PIC  9(02)  VALUE  ZERO.
     03  NYU-CNT             PIC  9(02)  VALUE  ZERO.
     03  SYU-CNT             PIC  9(02)  VALUE  ZERO.
     03  TBL-CNT             PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  ZERO.
     03  MAX-PAGE            PIC  9(01)  VALUE  ZERO.
 01  DSP-WORK.
     03  WK-TNABN.
         05  WK-TNA101       PIC  X(01)  VALUE  SPACE.
         05  WK-TNA201       PIC  X(03)  VALUE  SPACE.
*        05  WK-TNA101       PIC  X(03)  VALUE  SPACE.
*        05  WK-TNA201       PIC  X(01)  VALUE  SPACE.
         05  WK-TNA301       PIC  X(02)  VALUE  SPACE.
     03  WK-HINTAN.
         05  WK-TAN101       PIC  X(05)  VALUE  SPACE.
         05  WK-TAN201       PIC  X(02)  VALUE  SPACE.
         05  WK-TAN301       PIC  X(01)  VALUE  SPACE.
     03  WK-STKNO.
         05  WK-STK101       PIC  X(04)  VALUE  SPACE.
         05  WK-STK201       PIC  X(01)  VALUE  SPACE.
     03  WK-HINMEI           PIC  N(30)  VALUE  SPACE.
 01  WK-HINTAN-AREA.
     03  WK-TAN1I-X.
         05  WK-TAN1I        PIC  X(01)  OCCURS 5.
     03  WK-TAN1O-X.
         05  WK-TAN1O        PIC  X(01)  OCCURS 5.
     03  WK-TAN1             PIC  X(05).
*----<< 追加ワークエリア >>----  98/01/29  開始
*****日付入力許容範囲（年月日）
 01  WK-HANI-ARE.
     03  WK-HANI1            PIC  9(08)  VALUE  ZERO.
     03  WK-HANI2            PIC  9(08)  VALUE  ZERO.
*****日付編集
 01  WK-HENKAN.
     03  WK-HENKAN-1         PIC  9(04)  VALUE  ZERO.
     03  WK-HENKAN-2         PIC  9(02)  VALUE  ZERO.
     03  WK-HENKAN-3         PIC  9(02)  VALUE  ZERO.
*****在庫締年月
 01  WK-ZAIKO-SIME           PIC  9(09).
 01  FILLER                  REDEFINES   WK-ZAIKO-SIME.
     03  WK-ZAIKO-SIME-0     PIC  9(01).
     03  WK-ZAIKO-SIME-1     PIC  9(04).
     03  WK-ZAIKO-SIME-2     PIC  9(02).
     03  WK-ZAIKO-SIME-3     PIC  9(02).
*****日付入力許容範囲（月）
 01  WK-H-TUKI.
     03  WK-H-TUKI-1         PIC  9(02)  VALUE  ZERO.
     03  WK-H-TUKI-2         PIC  9(02)  VALUE  ZERO.
*----<< 追加ワークエリア >>----  98/01/29  終了
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*---<< LINK AREA >>-*
 LINKAGE                   SECTION.
 01  LINK-AREA.
     03  LINK-WSMEI          PIC  X(08).
*
******************************************************************
 PROCEDURE         DIVISION      USING     LINK-AREA.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   0     エラー処理　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DECLARATIVES.
*----------  作業実績ファイル　--------------------------------*
 SGY-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   ZSGYODT.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"作業実績ファイル異常！"
              "ST1=" SGY-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STA.
*
     ACCEPT   IN-DATA        FROM STA.
     STOP     RUN.
*----------  商品在庫マスタ　--------------------------------*
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   ZZAIMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"商品在庫マスタ異常！"
              "ST1=" ZAI-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STA.
*
     ACCEPT   IN-DATA        FROM STA.
     STOP     RUN.
*----------   条件ファイル　-----------------------------------*
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HJYOKEN.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"条件ファイル異常！"
              "ST1=" JYO-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STA.
*
     ACCEPT   IN-DATA        FROM STA.
     STOP     RUN.
*----------   倉庫マスタ -----------------------------------*
 SOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   ZSOKMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"倉庫マスタ異常！"
              "ST1=" SOK-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STA.
*
     ACCEPT   IN-DATA        FROM STA.
     STOP     RUN.
*----------   商品名称マスタ　 --------------------------------*
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HMEIMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"商品名称マスタ異常！"
              "ST1=" MEI-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STA.
*
     ACCEPT   IN-DATA        FROM STA.
     STOP     RUN.
*----------    表示ファイル -----------------------------------*
 DSP-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   DSPFILE.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"表示ファイル異常！"
              "ST1=" DSP-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STA.
*
     ACCEPT   IN-DATA        FROM STA.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 PROG-CNTL          SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     END-FLG  =   "END".
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 INIT-RTN           SECTION.
     ACCEPT      SYS-DATE    FROM  DATE.
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
     MOVE      LINK-OUT-YMD       TO   SYS-DATE2.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " START "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STA.
* ファイル　ＯＰＥＮ
     OPEN     I-O       ZSGYODT.
     OPEN     I-O       ZZAIMS.
     OPEN     I-O       HJYOKEN.
     OPEN     INPUT     ZSOKMS.
     OPEN     INPUT     HMEIMS.
     OPEN     I-O       DSPFILE.
*\\
     MOVE      99            TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     MOVE     "ZAI"          TO   JYO-F02.
     READ     HJYOKEN
          INVALID
              DISPLAY   "HJYOKEN INV KEY=99"  UPON STA
              MOVE      "END"     TO   END-FLG
              GO   TO   INIT-EXIT
     END-READ.
*
**** DISPLAY "F01  = " JYO-F01 UPON CONS.
**** DISPLAY "F02  = " JYO-F02 UPON CONS.
**** DISPLAY "F05  = " JYO-F05 UPON CONS.
     MOVE     JYO-F05        TO   XXX.
*****DISPLAY "F05  = " XXX     UPON CONS.
     MOVE     JYO-F05        TO   ZAI-SIME1.
     ADD      1              TO   ZAI-SIME1.
     IF       ZAI-SIME1R2    >    12
              MOVE 1         TO   ZAI-SIME1R2
              ADD  1         TO   ZAI-SIME1R1
     END-IF.
     MOVE     31             TO   ZAI-SIME2.
*----<< 在庫締年月セット >>----  98/01/29  開始
     MOVE     JYO-F04        TO   WK-ZAIKO-SIME.
*----<< 在庫締年月セット >>----  98/01/29  終了
*\\
* 西暦の取得
     MOVE     "57"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-READ-RTN.
     IF       INV-FLG    =   1
              DISPLAY   "HJYOKEN INV KEY=57"  UPON STA
              MOVE      "END"     TO   END-FLG
              GO   TO   INIT-EXIT
     END-IF.
     MOVE     JYO-F04        TO   WK-SEIREKI.
***** < 93/06/01 START > *****
     REWRITE  JYO-REC.
***** < 93/06/01 START > *****
* 場所の取得
     MOVE     "65"           TO   JYO-F01.
     MOVE     LINK-WSMEI     TO   JYO-F02.
     PERFORM  JYO-READ-RTN.
     IF       INV-FLG   =    1
              DISPLAY   "HJYOKEN INV KEY=65"  UPON STA
              MOVE      "END"     TO   END-FLG
              GO   TO   INIT-EXIT
     END-IF.
     MOVE     JYO-F04        TO   WK-BASYO.
***** < 93/06/01 START > *****
     REWRITE  JYO-REC.
***** < 93/06/01 START > *****
*
     MOVE     0              TO   SHORI-F.
 INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 MAIN-RTN           SECTION.
     EVALUATE    SHORI-F
       WHEN      0      PERFORM   DSP-INIT-RTN
       WHEN      1      PERFORM   DSP-SRKBN-RTN
       WHEN      2      PERFORM   DSP-SGYNO-RTN
       WHEN      3      PERFORM   DSP-GRP003-RTN
       WHEN      4      PERFORM   DSP-BODY-RTN
       WHEN      5      PERFORM   DSP-KAKNIN-RTN
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 END-RTN            SECTION.
     CLOSE              ZSGYODT
                        ZZAIMS
                        HJYOKEN
                        ZSOKMS
                        HMEIMS
                        DSPFILE.
*
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " END   "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STA.
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL    初期画面表示処理                            *
*--------------------------------------------------------------*
 DSP-INIT-RTN           SECTION.
     MOVE     SPACE          TO   ZNY0010.
     MOVE     SPACE          TO   TABLE-AREA.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-BODY-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE     "ZNY0010"      TO   DSP-FMT.
     MOVE     "ALLF"         TO   DSP-GRP.
*
     MOVE     1              TO   SHORI-F.
     MOVE     1              TO   PAGE-CNT   WK-CNT(1).
     MOVE     0              TO   MAX-LINE.
 DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      処理区分　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-SRKBN-RTN          SECTION.
     MOVE     G001           TO   GUIDE.
     MOVE     "GRP001"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SHORI-F
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "E000"
              PERFORM   CHK-SRKBN-RTN
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 DSP-SRKBN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      処理区分　チェック　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-SRKBN-RTN          SECTION.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
     IF       SRKBN    IS  NOT    NUMERIC
              MOVE      0         TO   SRKBN
     END-IF.
*
     EVALUATE          SRKBN
       WHEN     1
              MOVE     NC"登録"   TO   SRMEI
              PERFORM  CHK-SGYNO-RTN
              MOVE      3         TO   SHORI-F
       WHEN     2
              MOVE     NC"修正"   TO   SRMEI
              MOVE      2         TO   SHORI-F
       WHEN     3
              MOVE     NC"削除"   TO   SRMEI
              MOVE      2         TO   SHORI-F
       WHEN     9
              MOVE     NC"照会"   TO   SRMEI
              MOVE      2         TO   SHORI-F
       WHEN   OTHER
              MOVE      2         TO   ERR-FLG
              MOVE     "R"        TO   EDIT-OPTION OF SRKBN
     END-EVALUATE.
 CHK-SRKBN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      作業_　　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-SGYNO-RTN      SECTION.
     MOVE     0         TO        MAX-LINE.
     IF       SRKBN     =    1
              MOVE      G002      TO   GUIDE
     ELSE
              MOVE      G004      TO   GUIDE
     END-IF.
     MOVE     "GRP002"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SHORI-F
     WHEN     "F005"
              MOVE     "END"      TO   END-FLG
     WHEN     "F006"
              PERFORM   CLR-HEAD-RTN
              MOVE      1         TO   SHORI-F
     WHEN     "F011"
              PERFORM   ZEN-REC-RTN
              IF   ERR-FLG     =  ZERO
                   IF   SRKBN     =    2
                        MOVE      4    TO   SHORI-F
                   ELSE
                        MOVE     "Y"   TO   KAKNIN
                        MOVE      5    TO   SHORI-F
                   END-IF
              END-IF
     WHEN     "F012"
              PERFORM   JI-REC-RTN
              IF   ERR-FLG     =  ZERO
                   IF   SRKBN     =    2
                        MOVE      4    TO   SHORI-F
                   ELSE
                        MOVE     "Y"   TO   KAKNIN
                        MOVE      5    TO   SHORI-F
                   END-IF
              END-IF
     WHEN     "E000"
              PERFORM   CHK-SGYNO-RTN
              IF   ERR-FLG     =  ZERO
                   IF   SRKBN     =    2
                        MOVE      4    TO   SHORI-F
                   ELSE
                        MOVE     "Y"   TO   KAKNIN
                        MOVE      5    TO   SHORI-F
                   END-IF
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 DSP-SGYNO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      作業_　　チェック　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-SGYNO-RTN      SECTION.
*全レコード解放（排他制御用）
     CLOSE       ZSGYODT.
     OPEN        I-O          ZSGYODT.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
     IF       SGYNO   IS NOT  NUMERIC
              MOVE     0      TO   SGYNO
     END-IF.
*
     IF       SRKBN    =      1
              PERFORM  SGYNO-SET-RTN
     END-IF.
*伝票_チェック
     MOVE     SGYNO          TO   SGY-F01.
     MOVE     0              TO   SGY-F02.
     START    ZSGYODT  KEY   >=   SGY-F01    SGY-F02
         INVALID   KEY
              MOVE     1          TO   SGY-FLG
         NOT  INVALID  KEY
              PERFORM  SGY-READ-RTN
     END-START.
*  登録の時
     IF       SGY-FLG        =     0    AND
              SRKBN          =     1
              MOVE      4          TO   ERR-FLG
              MOVE     "R"         TO   EDIT-OPTION OF SGYNO
              GO   TO   CHK-SGYNO-EXIT
     END-IF.
*  登録以外の時
     IF       SGY-FLG        =     1    AND
              SRKBN     NOT  =     1
              MOVE      3          TO   ERR-FLG
              MOVE     "R"         TO   EDIT-OPTION OF SGYNO
              GO   TO   CHK-SGYNO-EXIT
     END-IF.
*
     IF       SRKBN     =    1
              GO   TO   CHK-SGYNO-EXIT
     END-IF.
*画面＆ＴＢＬクリア
     MOVE     SRKBN          TO    SAV-SRKBN.
     MOVE     SRMEI          TO    SAV-SRMEI.
     MOVE     HEAD1          TO    SAV-HEAD1.
     MOVE     SPACE          TO    ZNY0010   TABLE-AREA.
     MOVE     SAV-SRKBN      TO    SRKBN.
     MOVE     SAV-SRMEI      TO    SRMEI.
     MOVE     SAV-HEAD1      TO    HEAD1.
*伝票画面へ表示
     PERFORM  SGY-DSP-RTN.
*
 CHK-SGYNO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      前ページ　チェック　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 ZEN-REC-RTN      SECTION.
*全レコード解放（排他制御用）
     CLOSE       ZSGYODT.
     OPEN        I-O          ZSGYODT.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
     IF       SGYNO   IS NOT  NUMERIC
              MOVE     0      TO   SGYNO
     END-IF.
*伝票_チェック
     MOVE     SGYNO           TO   SGY-F01.
     MOVE     0               TO   SGY-F02.
     START    ZSGYODT  KEY     <   SGY-F01   SGY-F02
                                      WITH   REVERSED  ORDER
         INVALID   KEY
              MOVE     1      TO   SGY-FLG
         NOT  INVALID  KEY
              PERFORM  SGY-READ1-RTN
              IF   SGY-FLG    =    0
                   MOVE       SGY-F01   TO   WK-F01
                   MOVE       WK-F01    TO   SGY-F01
                   MOVE       0         TO   SGY-F02
                   START      ZSGYODT  KEY   >=   SGY-F01  SGY-F02
                      INVALID KEY
                              MOVE      1    TO   SGY-FLG
                      NOT  INVALID    KEY
                              PERFORM   SGY-READ1-RTN
                   END-START
              END-IF
     END-START.
     IF       SGY-FLG   =     1
              MOVE      16         TO   ERR-FLG
              MOVE     "R"         TO   EDIT-OPTION OF SGYNO
              GO   TO   ZEN-REC-EXIT
     END-IF.
*画面＆ＴＢＬクリア
     MOVE     SRKBN           TO   SAV-SRKBN.
     MOVE     SRMEI           TO   SAV-SRMEI.
     MOVE     HEAD1           TO   SAV-HEAD1.
     MOVE     SPACE           TO   ZNY0010   TABLE-AREA.
     MOVE     SAV-SRKBN       TO   SRKBN.
     MOVE     SAV-SRMEI       TO   SRMEI.
     MOVE     SAV-HEAD1       TO   HEAD1.
*伝票画面へ表示
     PERFORM  SGY-DSP-RTN.
*
 ZEN-REC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      次ページ　チェック　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 JI-REC-RTN      SECTION.
*全レコード解放（排他制御用）
     CLOSE       ZSGYODT.
     OPEN        I-O          ZSGYODT.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
     IF       SGYNO   IS NOT  NUMERIC
              MOVE     0      TO   SGYNO
     END-IF.
*伝票_チェック
     MOVE     SGYNO           TO   SGY-F01.
     MOVE     99              TO   SGY-F02.
     START    ZSGYODT  KEY    >    SGY-F01    SGY-F02
         INVALID   KEY
              MOVE     1      TO   SGY-FLG
         NOT  INVALID  KEY
              PERFORM  SGY-READ1-RTN
     END-START.
     IF       SGY-FLG  =      1
              MOVE     17     TO   ERR-FLG
              MOVE    "R"     TO   EDIT-OPTION OF SGYNO
              GO   TO  JI-REC-EXIT
     END-IF.
*画面＆ＴＢＬクリア
     MOVE     SRKBN           TO   SAV-SRKBN.
     MOVE     SRMEI           TO   SAV-SRMEI.
     MOVE     HEAD1           TO   SAV-HEAD1.
     MOVE     SPACE           TO   ZNY0010   TABLE-AREA.
     MOVE     SAV-SRKBN       TO   SRKBN.
     MOVE     SAV-SRMEI       TO   SRMEI.
     MOVE     SAV-HEAD1       TO   HEAD1.
*伝票画面へ表示
     PERFORM  SGY-DSP-RTN.
*
 JI-REC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＲＰ００３　入力　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-GRP003-RTN     SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "GRP003"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SHORI-F
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "F006"
              PERFORM   CLR-HEAD-RTN
              MOVE      1         TO   SHORI-F
     WHEN     "E000"
              PERFORM   CHK-GRP003-RTN
              IF  ERR-FLG         =    ZERO
                  MOVE  4         TO   SHORI-F
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-GRP003-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＲＰ００３　チェック　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-GRP003-RTN     SECTION.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
***** 93/07/01 担当者追加
*担当者
     IF       TANTO    IS NOT     NUMERIC
              MOVE     24         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF TANTO
              MOVE     "R"        TO   EDIT-OPTION OF TANTO
     END-IF.
*作業完成日
 CHK-GRP003-01.
     IF       SGYMD    IS NOT     NUMERIC
              MOVE      0    TO   SGYMD
     END-IF.
*  日付論理チェック
     MOVE     SGYMD          TO   CHK-DATE.
     PERFORM  YMD-CHK-RTN.
     IF       ERR-FLG   =    5
              MOVE     "C"        TO   EDIT-CURSOR OF SGYMD
              MOVE     "R"        TO   EDIT-OPTION OF SGYMD
              GO   TO   CHK-GRP003-02
     END-IF.
     MOVE     SGYMD          TO   CHK-DATEX.
     IF       CHK-DATEX <         930101
              ADD    20000000     TO   CHK-DATEX
     ELSE
              ADD    19000000     TO   CHK-DATEX
     END-IF.
*  締日の取得
     MOVE     "99"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-READ-RTN.
     IF       INV-FLG   =    1
              DISPLAY   "HJYOKEN INV KEY=99"  UPON STA
              MOVE      "END"     TO   END-FLG
              GO   TO   CHK-GRP003-EXIT
     END-IF.
     EVALUATE SGYMD (3:2)
         WHEN 01   MOVE      JYO-F04        TO   WK-SIMEBI
         WHEN 02   MOVE      JYO-F05        TO   WK-SIMEBI
         WHEN 03   MOVE      JYO-F06        TO   WK-SIMEBI
         WHEN 04   MOVE      JYO-F07        TO   WK-SIMEBI
         WHEN 05   MOVE      JYO-F08        TO   WK-SIMEBI
         WHEN 06   MOVE      JYO-F09        TO   WK-SIMEBI
         WHEN 07   MOVE      JYO-F10        TO   WK-SIMEBI
         WHEN 08   MOVE      JYO-F11        TO   WK-SIMEBI
         WHEN 09   MOVE      JYO-F12        TO   WK-SIMEBI
         WHEN 10   MOVE      JYO-F12A       TO   WK-SIMEBI
         WHEN 11   MOVE      JYO-F12B       TO   WK-SIMEBI
         WHEN 12   MOVE      JYO-F12C       TO   WK-SIMEBI
     END-EVALUATE.
***** < 93/06/01 START > *****
     REWRITE  JYO-REC.
***** < 93/06/01 START > *****
*
*## 1999/12/20 NAV
*****COMPUTE  WK-SYSDATE  =  WK-SEIREKI  *  10000  +  SYS-DATE.
     MOVE     SYS-DATE2      TO          WK-SYSDATE
*****COMPUTE  WK-SGYDATE  =  WK-SEIREKI  *  10000  +  SGYMD.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SGYMD               TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WK-SGYDATE.
     IF       WK-SIMEBI   <  WK-SYSDATE
              IF   ERR-FLG   =   ZERO
                   MOVE      5    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF SGYMD
              MOVE     "R"        TO   EDIT-OPTION OF SGYMD
     END-IF.
***** < 94/06/18 START > *****
     IF       WK-SYSDATE     <    WK-SGYDATE
              IF   ERR-FLG   =   ZERO
                   MOVE      5    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF SGYMD
              MOVE     "R"        TO   EDIT-OPTION OF SGYMD
     END-IF.
 CHKRTN-010.
*----<< 追加 >>----  98/01/29  開始  許容範囲エラー追加
*----<許容範囲月算出>
     MOVE   WK-ZAIKO-SIME-2  TO   WK-H-TUKI-1.
 CHKRTN-020.
*****DISPLAY "WK-H-TUKI-2     = " WK-H-TUKI-2     UPON CONS.
*****DISPLAY "WK-ZAIKO-SIME-0 = " WK-ZAIKO-SIME-0 UPON CONS.
*****DISPLAY "WK-ZAIKO-SIME-1 = " WK-ZAIKO-SIME-1 UPON CONS.
*****DISPLAY "WK-ZAIKO-SIME-2 = " WK-ZAIKO-SIME-2 UPON CONS.
*****DISPLAY "WK-ZAIKO-SIME-3 = " WK-ZAIKO-SIME-3 UPON CONS.
     COMPUTE WK-H-TUKI-2 = WK-ZAIKO-SIME-2  -  1.
 CHKRTN-030.
     IF     WK-H-TUKI-2  =  ZERO
            MOVE   12        TO   WK-H-TUKI-2
     END-IF.
 CHKRTN-040.
*----<許容範囲終了年月日>
     EVALUATE   WK-H-TUKI-1
         WHEN      01    MOVE  JYO-F04     TO   WK-HANI1
         WHEN      02    MOVE  JYO-F05     TO   WK-HANI1
         WHEN      03    MOVE  JYO-F06     TO   WK-HANI1
         WHEN      04    MOVE  JYO-F07     TO   WK-HANI1
         WHEN      05    MOVE  JYO-F08     TO   WK-HANI1
         WHEN      06    MOVE  JYO-F09     TO   WK-HANI1
         WHEN      07    MOVE  JYO-F10     TO   WK-HANI1
         WHEN      08    MOVE  JYO-F11     TO   WK-HANI1
         WHEN      09    MOVE  JYO-F12     TO   WK-HANI1
         WHEN      10    MOVE  JYO-F12A    TO   WK-HANI1
         WHEN      11    MOVE  JYO-F12B    TO   WK-HANI1
         WHEN      12    MOVE  JYO-F12C    TO   WK-HANI1
     END-EVALUATE.
 CHKRTN-050.
*----<許容範囲開始年月日>
     EVALUATE   WK-H-TUKI-2
         WHEN      01    MOVE  JYO-F04     TO   WK-HANI2
         WHEN      02    MOVE  JYO-F05     TO   WK-HANI2
         WHEN      03    MOVE  JYO-F06     TO   WK-HANI2
         WHEN      04    MOVE  JYO-F07     TO   WK-HANI2
         WHEN      05    MOVE  JYO-F08     TO   WK-HANI2
         WHEN      06    MOVE  JYO-F09     TO   WK-HANI2
         WHEN      07    MOVE  JYO-F10     TO   WK-HANI2
         WHEN      08    MOVE  JYO-F11     TO   WK-HANI2
         WHEN      09    MOVE  JYO-F12     TO   WK-HANI2
         WHEN      10    MOVE  JYO-F12A    TO   WK-HANI2
         WHEN      11    MOVE  JYO-F12B    TO   WK-HANI2
         WHEN      12    MOVE  JYO-F12C    TO   WK-HANI2
     END-EVALUATE.
 CHKRTN-060.
*
     REWRITE  JYO-REC.
*----<許容範囲チェック>
     MOVE     WK-HANI2   TO  WK-HENKAN.
*-----<追加 98/02/13 START>
     IF  WK-SGYDATE  <=  WK-HANI1
         MOVE   01       TO  WK-HENKAN-3
     ELSE
         ADD    1        TO  WK-HENKAN-2
         IF  WK-HENKAN-2  >  12
             ADD    1    TO  WK-HENKAN-1
             MOVE   01   TO  WK-HENKAN-2
         END-IF
         MOVE   01       TO  WK-HENKAN-3
     END-IF.
*-----<追加 98/02/13 END  >
     MOVE     01         TO  WK-HENKAN-3.
     MOVE     WK-HENKAN  TO  WK-HANI2.
*****DISPLAY "2-WK-HANI1 = " WK-HANI1 UPON CONS.
*****DISPLAY "2-WK-HANI2 = " WK-HANI2 UPON CONS.
*****DISPLAY "2-WK-SGY-D = " WK-SGYDATE UPON CONS.
 CHKRTN-070.
     IF  ( WK-HANI2   <=  WK-SGYDATE )
*****AND ( WK-HANI1   >=  WK-SGYDATE )
         CONTINUE
     ELSE
         IF  ERR-FLG       =   0
             MOVE   5          TO   ERR-FLG
         END-IF
         MOVE   "C"        TO   EDIT-CURSOR  OF  SGYMD
         MOVE   "R"        TO   EDIT-OPTION  OF  SGYMD
     END-IF.
*----<< 追加 >>----  98/01/29  終了  許容範囲エラー追加
***** < 94/06/18 START > *****
*作業区分チェック
 CHK-GRP003-02.
*****IF       SGKBN     IS NOT    NUMERIC
*****         MOVE      0         TO   SGKBN
*****END-IF.
*
     MOVE     "71"           TO   JYO-F01.
     MOVE     SGKBN          TO   JYO-F02.
     PERFORM  JYO-READ-RTN.
     IF       INV-FLG  =  1
              IF   ERR-FLG   =   ZERO
                   MOVE      6    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF SGKBN
              MOVE     "R"        TO   EDIT-OPTION OF SGKBN
              GO   TO   CHK-GRP003-03
*93.10.16 START
     ELSE
        IF    JYO-F04  NOT =           ZERO AND 1
              IF   ERR-FLG   =   ZERO
                   MOVE      6    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF SGKBN
              MOVE     "R"        TO   EDIT-OPTION OF SGKBN
              GO   TO   CHK-GRP003-03
        END-IF
     END-IF.
*93.10.16 END
*
     MOVE     JYO-F03             TO   SGMEI.
***** < 93/06/01 START > *****
     REWRITE  JYO-REC.
***** < 93/06/01 START > *****
*作業場所チェック
 CHK-GRP003-03.
     MOVE     BASYO     TO        SOK-F01.
     PERFORM  SOK-READ-RTN.
*\\
*****IF       INV-FLG   =    1    OR
*****         BASYO     =    "01"
     IF       INV-FLG   =    1
              IF   ERR-FLG   =    ZERO
                   MOVE      7    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF BASYO
              MOVE     "R"        TO   EDIT-OPTION OF BASYO
              GO   TO   CHK-GRP003-EXIT
     END-IF.
     MOVE     SOK-F02             TO   BASYOM.
 CHK-GRP003-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      伝票　画面へ表示　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 SGY-DSP-RTN            SECTION.
*データ表示
     PERFORM   HEAD-SET-RTN.
     PERFORM   VARYING  I  FROM  1  BY  1  UNTIL  I  >  24  OR
                                            SGY-FLG  =  1
          PERFORM    SGY-TBL-SET-RTN
          PERFORM    SGY-READ-RTN
          ADD  1        TO   MAX-LINE
     END-PERFORM.
*
     MOVE      1        TO   PAGE-CNT.
     MOVE      0        TO   MAX-PAGE.
     PERFORM   SGY-DSP-SET-RTN.
 SGY-DSP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3        ボディ部　入力                            *
*--------------------------------------------------------------*
 DSP-BODY-RTN       SECTION.
     MOVE     G002         TO   GUIDE.
     MOVE     "BODY"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SHORI-F
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "F006"
              PERFORM   CLR-BODY-RTN
              IF   SRKBN   =      1
                   MOVE    3      TO   SHORI-F
              ELSE
                   MOVE    2      TO   SHORI-F
              END-IF
     WHEN     "E000"
              PERFORM   CLR-BODY-RTN
              IF   SRKBN   =      2
                   MOVE WK-CNT(PAGE-CNT)   TO  T
              END-IF
              PERFORM   VARYING   L   FROM   1   BY   1
                                      UNTIL  L    >   6
                   PERFORM        CHK-BODY-RTN
              END-PERFORM
              IF   ERR-FLG    =   ZERO
                   IF   SRKBN     =   1
                        PERFORM   CHK-BODY1-RTN
                   END-IF
                   PERFORM        CHK-NYSKBN-RTN
              END-IF
              IF   ERR-FLG    =   ZERO
                   PERFORM        SGY-DSP-SET-RTN
                   MOVE   "Y"     TO    KAKNIN
                   MOVE    5      TO    SHORI-F
                   PERFORM        CLR-BODY-RTN
                   PERFORM        CHK-ZAI-RTN
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 DSP-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  3       ＢＯＤＹ　チェック                         *
*--------------------------------------------------------------*
 CHK-BODY-RTN         SECTION.
*テーブル添字算出
     IF     SRKBN         =    1
            COMPUTE   T   =  ( PAGE-CNT  *  6 )  -  6  +  L
     ELSE
            MOVE      0                   TO    TBL-FLG
            MOVE      T        TO     J
            PERFORM   VARYING  IX  FROM   J   BY  1
                                   UNTIL  TBL-FLG   =  1
                IF    TBL-SAKJYO(IX)  NOT =   1
                      MOVE     IX     TO      T
                      MOVE     1      TO      TBL-FLG
                END-IF
            END-PERFORM
     END-IF.
*未入力行チェック無し
     IF     HINCD (L)     =    SPACE  AND
            TAN101(L)     =    SPACE  AND
            TAN201(L)     =    SPACE  AND
            TAN301(L)     =    SPACE
            IF (SRKBN     =    1)     OR
               (SRKBN     =    2      AND
                MAX-LINE  <    T)
                GO   TO   CHK-BODY-01
            END-IF
     ELSE
            IF  MAX-LINE  <    T
                MOVE    T      TO   MAX-LINE
                IF   SRKBN     =    2
                     COMPUTE   GYO101(L) = TBL-GYO101(T - 1) + 1
                END-IF
            END-IF
     END-IF.
*** 商品コード右詰処理  *** 93/07/01 追加
     PERFORM VARYING IXB    FROM  1   BY   1
             UNTIL   IXB    >     7
         PERFORM VARYING IXC    FROM  8   BY  -1
                 UNTIL   IXC    <     2
             IF  HINCD (L)(IXC:1)   =  SPACE
                 COMPUTE IXD    =     IXC   -   1
                 MOVE  HINCD (L)(IXD:1)   TO   HINCD (L)(IXC:1)
                 MOVE  SPACE              TO   HINCD (L)(IXD:1)
             END-IF
         END-PERFORM
     END-PERFORM.
**
     PERFORM     VARYING    IXB   FROM    1   BY   1
                 UNTIL      (IXB   >      7 ) OR
                 (HINCD (L)(IXB:1) NOT =  SPACE)
         IF      HINCD (L)(IXB:1)        =   SPACE
                 MOVE       "0"   TO      HINCD (L)(IXB:1)
         END-IF
     END-PERFORM.
**
*品単５桁　頭空白詰
     IF     TAN101(L)    NOT =  SPACE   AND
            TAN101(L)(5:1)   =  SPACE
            MOVE   TAN101(L)    TO      WK-TAN1I-X
            MOVE   ZERO         TO      J
            PERFORM  VARYING I  FROM    1  BY  1  UNTIL  I  >  5
              IF   WK-TAN1I(I)  NOT =   SPACE
                ADD      1         TO   J
                MOVE  WK-TAN1I(I)  TO   WK-TAN1O(J)
             END-IF
            END-PERFORM
            MOVE   SPACE           TO   WK-TAN1
            COMPUTE      I   =  6   -   J
            MOVE   WK-TAN1O-X(1:J) TO   WK-TAN1(I:J)
            MOVE   WK-TAN1         TO   TAN101(L)
     END-IF.
*商品コードチェック
     MOVE   HINCD(L)               TO   MEI-F011.
     MOVE   TAN101(L)              TO   MEI-F0121.
     MOVE   TAN201(L)              TO   MEI-F0122.
     MOVE   TAN301(L)              TO   MEI-F0123.
     PERFORM       MEI-READ-RTN.
     IF      INV-FLG         =     1
             IF   ERR-FLG    =     ZERO
                  MOVE       8     TO   ERR-FLG
             END-IF
             MOVE     "C"   TO     EDIT-CURSOR OF HINCD (L)
             MOVE     "R"   TO     EDIT-OPTION OF HINCD (L)
             MOVE     "R"   TO     EDIT-OPTION OF TAN101(L)
             MOVE     "R"   TO     EDIT-OPTION OF TAN201(L)
             MOVE     "R"   TO     EDIT-OPTION OF TAN301(L)
     ELSE
             MOVE      MEI-F021    TO   WK-HINMEI(1:15)
             MOVE      MEI-F022    TO   WK-HINMEI(16:15)
             MOVE      WK-HINMEI   TO   HINMEI(L)
     END-IF.
*数量チェック
     IF  SRKBN         =    1
*93/07/01 ST 追加
         IF  SURYO(L)     NOT NUMERIC
             IF   ERR-FLG   =      ZERO
                  MOVE     25      TO   ERR-FLG
             END-IF
             MOVE     "C"   TO     EDIT-CURSOR OF SURYO(L)
             MOVE     "R"   TO     EDIT-OPTION OF SURYO(L)
*93/07/01 ED 追加
         ELSE
             IF  SURYO(L)       =   ZERO
                 IF   ERR-FLG   =      ZERO
                      MOVE      9      TO   ERR-FLG
                 END-IF
                 MOVE     "C"   TO     EDIT-CURSOR OF SURYO(L)
                 MOVE     "R"   TO     EDIT-OPTION OF SURYO(L)
             END-IF
         END-IF
     ELSE
         IF  SURYO (L)      =  ZERO
             IF  NYSKBN(L)      =  "1"
                 IF   ERR-FLG   =    ZERO
                      MOVE      9    TO   ERR-FLG
                 END-IF
                 MOVE     "C"   TO   EDIT-CURSOR OF SURYO(L)
                 MOVE     "R"   TO   EDIT-OPTION OF SURYO(L)
             END-IF
         ELSE
             MOVE     SPACE     TO   TBL-SAKJYO(T)
         END-IF
     END-IF.
*入出庫区分チェック
     IF  NYSKBN(L)  NOT =   "1" AND  "2"
         IF  ERR-FLG    =   ZERO
             MOVE     10    TO  ERR-FLG
         END-IF
             MOVE     "C"   TO  EDIT-CURSOR OF NYSKBN(L)
             MOVE     "R"   TO  EDIT-OPTION OF NYSKBN(L)
     END-IF.
*テーブルセット
 CHK-BODY-01.
     MOVE    GYO101(L)      TO  TBL-GYO101(T).
     MOVE    TNA101(L)      TO  TBL-TNA101(T).
     MOVE    TNA201(L)      TO  TBL-TNA201(T).
     MOVE    TNA301(L)      TO  TBL-TNA301(T).
     MOVE    HINCD (L)      TO  TBL-HINCD (T).
     MOVE    TAN101(L)      TO  TBL-TAN101(T).
     MOVE    TAN201(L)      TO  TBL-TAN201(T).
     MOVE    TAN301(L)      TO  TBL-TAN301(T).
     MOVE    SURYO (L)      TO  TBL-SURYO (T).
     MOVE    STK101(L)      TO  TBL-STK101(T).
     MOVE    STK201(L)      TO  TBL-STK201(T).
     MOVE    BIKOU (L)      TO  TBL-BIKOU (T).
     MOVE    NYSKBN(L)      TO  TBL-NYSKBN(T).
     MOVE    HINMEI(L)      TO  TBL-HINMEI(T).
     ADD     1              TO  T.
 CHK-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ１　　チェック　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-BODY1-RTN         SECTION.
*
     MOVE     SPACE    TO    WK-TBL-AREA.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  24
              MOVE    TBL-TNA101(I)  TO   T-TNA101(I)
              MOVE    TBL-TNA201(I)  TO   T-TNA201(I)
              MOVE    TBL-TNA301(I)  TO   T-TNA301(I)
              MOVE    TBL-HINCD (I)  TO   T-HINCD (I)
              MOVE    TBL-TAN101(I)  TO   T-TAN101(I)
              MOVE    TBL-TAN201(I)  TO   T-TAN201(I)
              MOVE    TBL-TAN301(I)  TO   T-TAN301(I)
              MOVE    TBL-SURYO (I)  TO   T-SURYO (I)
              MOVE    TBL-STK101(I)  TO   T-STK101(I)
              MOVE    TBL-STK201(I)  TO   T-STK201(I)
              MOVE    TBL-BIKOU (I)  TO   T-BIKOU (I)
              MOVE    TBL-NYSKBN(I)  TO   T-NYSKBN(I)
              MOVE    TBL-HINMEI(I)  TO   T-HINMEI(I)
              MOVE    TBL-WTANA1(I)  TO   T-WTANA1(I)
              MOVE    TBL-WTANA2(I)  TO   T-WTANA2(I)
              MOVE    TBL-WTANA3(I)  TO   T-WTANA3(I)
              MOVE    TBL-WHINCD(I)  TO   T-WHINCD(I)
              MOVE    TBL-WHTAN1(I)  TO   T-WHTAN1(I)
              MOVE    TBL-WHTAN2(I)  TO   T-WHTAN2(I)
              MOVE    TBL-WHTAN3(I)  TO   T-WHTAN3(I)
              MOVE    TBL-WNYUSKBN(I)  TO T-WNYUSKBN(I)
     END-PERFORM.
*明細行詰めチェック
     MOVE     SPACE    TO    TABLE-AREA.
     MOVE     0        TO    J.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  24
       IF     T-HINCD  (I)   =     SPACE  AND
              T-TAN101 (I)   =     SPACE  AND
              T-TAN201 (I)   =     SPACE  AND
              T-TAN301 (I)   =     SPACE
              CONTINUE
       ELSE
              ADD     1              TO   J
              MOVE    J              TO   TBL-GYO101(J)
              MOVE    T-TNA101(I)    TO   TBL-TNA101(J)
              MOVE    T-TNA201(I)    TO   TBL-TNA201(J)
              MOVE    T-TNA301(I)    TO   TBL-TNA301(J)
              MOVE    T-HINCD (I)    TO   TBL-HINCD (J)
              MOVE    T-TAN101(I)    TO   TBL-TAN101(J)
              MOVE    T-TAN201(I)    TO   TBL-TAN201(J)
              MOVE    T-TAN301(I)    TO   TBL-TAN301(J)
              MOVE    T-SURYO (I)    TO   TBL-SURYO (J)
              MOVE    T-STK101(I)    TO   TBL-STK101(J)
              MOVE    T-STK201(I)    TO   TBL-STK201(J)
              MOVE    T-BIKOU (I)    TO   TBL-BIKOU (J)
              MOVE    T-NYSKBN(I)    TO   TBL-NYSKBN(J)
              MOVE    T-HINMEI(I)    TO   TBL-HINMEI(J)
              MOVE    T-WTANA1(I)    TO   TBL-WTANA1(I)
              MOVE    T-WTANA2(I)    TO   TBL-WTANA2(I)
              MOVE    T-WTANA3(I)    TO   TBL-WTANA3(I)
              MOVE    T-WHINCD(I)    TO   TBL-WHINCD(I)
              MOVE    T-WHTAN1(I)    TO   TBL-WHTAN1(I)
              MOVE    T-WHTAN2(I)    TO   TBL-WHTAN2(I)
              MOVE    T-WHTAN3(I)    TO   TBL-WHTAN3(I)
              MOVE    T-WNYUSKBN(I)  TO   TBL-WNYUSKBN(I)
       END-IF
     END-PERFORM.
*
     MOVE     J       TO     MAX-LINE.
     PERFORM  SGY-DSP-SET-RTN.
     PERFORM  CLR-BODY-RTN.
*全未入力チェック
     IF  MAX-LINE       =  0
              MOVE     13    TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR OF HINCD (1)
              MOVE     "R"   TO   EDIT-OPTION OF HINCD (1)
              MOVE     "R"   TO   EDIT-OPTION OF TAN101(1)
              MOVE     "R"   TO   EDIT-OPTION OF TAN201(1)
              MOVE     "R"   TO   EDIT-OPTION OF TAN301(1)
              GO    TO  CHK-BODY1-EXIT
     END-IF.
*１行のみはエラー
     IF  MAX-LINE       =  1
              IF   NYSKBN(1)    =      "1"
                   MOVE     21    TO   ERR-FLG
              ELSE
                   MOVE     22    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF HINCD (2)
              MOVE     "R"   TO   EDIT-OPTION OF HINCD (2)
              MOVE     "R"   TO   EDIT-OPTION OF TAN101(2)
              MOVE     "R"   TO   EDIT-OPTION OF TAN201(2)
              MOVE     "R"   TO   EDIT-OPTION OF TAN301(2)
     END-IF.
 CHK-BODY1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  3       入出区分　チェック                         *
*--------------------------------------------------------------*
 CHK-NYSKBN-RTN         SECTION.
     MOVE    0         TO   NYU-CNT    SYU-CNT.
     MOVE    SPACE     TO   NYU-FLG    SYU-FLG.
*入出庫区分チェック
     PERFORM VARYING   S  FROM  1  BY  1
                                UNTIL      S         > 24     OR
                                       TBL-NYSKBN(S) = SPACE
         IF  TBL-SAKJYO(S)  NOT =  1
             IF    TBL-NYSKBN(S)   =   "1"
                   ADD     1    TO     NYU-CNT
                   MOVE    1    TO     NYU-FLG
             ELSE
                   ADD     1    TO     SYU-CNT
                   MOVE    1    TO     SYU-FLG
             END-IF
         END-IF
     END-PERFORM.
*１のみ入力
     IF      NYU-FLG   =   SPACE
             PERFORM   VARYING  S  FROM  1  BY  1
                                UNTIL    S        >  6      OR
                                       NYSKBN(S)  =  SPACE
                 IF    NYSKBN(S)  =    "2"     AND
                       NYU-FLG    =    SPACE
                       MOVE   1   TO   NYU-FLG
                 ELSE
                       MOVE  18   TO   ERR-FLG
                       MOVE  "C"  TO   EDIT-CURSOR OF NYSKBN(S)
                       MOVE  "R"  TO   EDIT-OPTION OF NYSKBN(S)
                 END-IF
             END-PERFORM
             GO   TO   CHK-NYSKBN-EXIT
     END-IF.
*０のみ入力
     IF      SYU-FLG   =   SPACE
             PERFORM   VARYING  S  FROM  1  BY  1
                                UNTIL    S        >  6      OR
                                       NYSKBN(S)  =  SPACE
                 IF    NYSKBN(S)  =    "1"     AND
                       SYU-FLG    =    SPACE
                       MOVE   1   TO   SYU-FLG
                 ELSE
                       MOVE  18   TO   ERR-FLG
                       MOVE  "C"  TO   EDIT-CURSOR OF NYSKBN(S)
                       MOVE  "R"  TO   EDIT-OPTION OF NYSKBN(S)
                 END-IF
             END-PERFORM
             GO   TO   CHK-NYSKBN-EXIT
     END-IF.
*１の複数入力
***  IF      NYU-CNT   >    1
***          PERFORM   VARYING  S  FROM  1  BY  1
***                             UNTIL      S       >  6      OR
***                                     NYSKBN(S)  =  SPACE
***              IF    NYSKBN(S)   =    "1"
***                    MOVE   12        TO  ERR-FLG
***                    MOVE   "C"  TO   EDIT-CURSOR OF NYSKBN(S)
***                    MOVE   "R"  TO   EDIT-OPTION OF NYSKBN(S)
***              END-IF
***          END-PERFORM
***          GO   TO   CHK-NYSKBN-EXIT
***  END-IF.
*数量チェック
     MOVE WK-CNT(PAGE-CNT)     TO  T.
     PERFORM  VARYING  L  FROM  1  BY  1
                                  UNTIL (   L      >  6    ) OR
                                        (HINCD (L) =  SPACE  AND
                                         TAN101(L) =  SPACE  AND
                                         TAN201(L) =  SPACE  AND
                                         TAN301(L) =  SPACE)
*ＴＢＬ添字判定
             MOVE       0       TO    TBL-FLG
             MOVE       T       TO    J
             PERFORM  VARYING  IX  FROM  J  BY  1
                                        UNTIL  TBL-FLG  =  1
                IF   TBL-SAKJYO(IX)  NOT = 1
                     MOVE      IX     TO   T
                     MOVE      1      TO   TBL-FLG
                END-IF
             END-PERFORM
*修正時、行削除チェック
             IF  SURYO (L)     =      ZERO
                 SUBTRACT  1   FROM   SYU-CNT
                 IF  SYU-CNT   =      ZERO
                     MOVE    9        TO   ERR-FLG
                     MOVE   "C"       TO   EDIT-CURSOR OF SURYO(L)
                     MOVE   "R"       TO   EDIT-OPTION OF SURYO(L)
                 ELSE
                     MOVE  1   TO     TBL-SAKJYO(T)
                 END-IF
             END-IF
             ADD     1         TO     T
*在庫マスタ存在チェック
             MOVE    BASYO     TO     ZAI-F01
             MOVE    HINCD (L) TO     ZAI-F021
             MOVE    TAN101(L) TO     ZAI-F022(1:5)
             MOVE    TAN201(L) TO     ZAI-F022(6:2)
             MOVE    TAN301(L) TO     ZAI-F022(8:1)
             MOVE    TNA101(L) TO     ZAI-F03(1:1)
             MOVE    TNA201(L) TO     ZAI-F03(2:3)
             MOVE    TNA301(L) TO     ZAI-F03(5:2)
             PERFORM ZAI-READ-RTN
             IF      INV-FLG    =     1
                 MOVE    "R"   TO     EDIT-OPTION OF HINCD (L)
                 MOVE    "R"   TO     EDIT-OPTION OF TAN101(L)
                 MOVE    "R"   TO     EDIT-OPTION OF TAN201(L)
                 MOVE    "R"   TO     EDIT-OPTION OF TAN301(L)
                 MOVE    "R"   TO     EDIT-OPTION OF TNA101(L)
                 MOVE    "R"   TO     EDIT-OPTION OF TNA201(L)
                 MOVE    "R"   TO     EDIT-OPTION OF TNA301(L)
             ELSE
                 REWRITE  ZAI-REC
             END-IF
     END-PERFORM.
 CHK-NYSKBN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　入力　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-KAKNIN-RTN      SECTION.
*在庫マスタ存在チェック
*    PERFORM  CLR-BODY-RTN.
*    PERFORM  CHK-ZAI-RTN.
*
     MOVE     G003             TO   GUIDE.
     MOVE     "KKA001"         TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SHORI-F
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "F006"
              MOVE      SPACE     TO   KAKNIN
              PERFORM   CLR-TAIL-RTN
              IF   SRKBN      =   1    OR   2
                   MOVE       4   TO   SHORI-F
              ELSE
                   MOVE       2   TO   SHORI-F
              END-IF
     WHEN     "F007"
              IF   PAGE-CNT   =   1
                   MOVE      14   TO   ERR-FLG
              ELSE
                   COMPUTE   PAGE-CNT  =   PAGE-CNT   -   1
                   PERFORM   SGY-DSP-SET-RTN
                   IF  SRKBN  =   1    OR  2
                       MOVE   4   TO   SHORI-F
                   END-IF
*93/07/01  ST *********
                   PERFORM   CLR-BODY-RTN
*93/07/01  ED *********
              END-IF
     WHEN     "F008"
              IF   SRKBN      =   1
                IF HINCD(6)            =  SPACE    OR
                   TBL-HINCD(24)   NOT =  SPACE
                   MOVE      20    TO     ERR-FLG
                ELSE
                   ADD       1     TO     PAGE-CNT
                   COMPUTE   T  = ( PAGE-CNT * 6 )  -  6  +  1
                   MOVE      T     TO     WK-CNT(PAGE-CNT)
                   IF  TBL-HINCD (T)   =   SPACE  AND
                       TBL-TAN101(T)   =   SPACE  AND
                       TBL-TAN201(T)   =   SPACE  AND
                       TBL-TAN301(T)   =   SPACE
                       MOVE     SRKBN      TO     SAV-SRKBN
                       MOVE     SRMEI      TO     SAV-SRMEI
                       MOVE     HEAD1      TO     SAV-HEAD1
                       MOVE     SPACE      TO     ZNY0010
                       MOVE     SAV-SRKBN  TO     SRKBN
                       MOVE     SAV-SRMEI  TO     SRMEI
                       MOVE     SAV-HEAD1  TO     HEAD1
                       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
                           COMPUTE  T = ( PAGE-CNT * 6 ) - 6 + I
                           MOVE   T        TO     GYO101(I)
                       END-PERFORM
                   ELSE
                       PERFORM   SGY-DSP-SET-RTN
                   END-IF
*93/07/01  ST *********
                   PERFORM   CLR-BODY-RTN
*93/07/01  ED *********
                   MOVE      4         TO  SHORI-F
                END-IF
              ELSE
                IF PAGE-CNT  =  4  OR
                   HINCD(6)  =  SPACE
                   MOVE      15        TO  ERR-FLG
                ELSE
                   ADD       1         TO  PAGE-CNT
                   MOVE      T         TO  WK-CNT(PAGE-CNT)
                   PERFORM   SGY-DSP-SET-RTN
*93/07/01  ST *********
                   PERFORM   CLR-BODY-RTN
*93/07/01  ED *********
                   IF  SRKBN =  2
                       MOVE  4         TO  SHORI-F
                   END-IF
                END-IF
              END-IF
     WHEN     "E000"
              PERFORM  CHK-KAKNIN-RTN
              IF   ERR-FLG   =  ZERO
                   IF  SRKBN    NOT =  9
                       PERFORM  SGY-UPDT-RTN
                   END-IF
                   PERFORM   KAKNIN-AFTER-RTN
                   PERFORM   CLR-BODY-RTN
                   PERFORM   CLR-TAIL-RTN
              END-IF
     WHEN     OTHER
              MOVE     1        TO     ERR-FLG
     END-EVALUATE.
 DSP-KAKNIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  3       在庫マスタチェック                         *
*--------------------------------------------------------------*
 CHK-ZAI-RTN            SECTION.
*在庫マスタ存在チェック
     PERFORM  VARYING  L  FROM  1  BY  1
                                  UNTIL (   L      >  6    ) OR
                                        (HINCD (L) =  SPACE  AND
                                         TAN101(L) =  SPACE  AND
                                         TAN201(L) =  SPACE  AND
                                         TAN301(L) =  SPACE)
             MOVE    BASYO     TO     ZAI-F01
             MOVE    HINCD (L) TO     ZAI-F021
             MOVE    TAN101(L) TO     ZAI-F022(1:5)
             MOVE    TAN201(L) TO     ZAI-F022(6:2)
             MOVE    TAN301(L) TO     ZAI-F022(8:1)
             MOVE    TNA101(L) TO     ZAI-F03(1:1)
             MOVE    TNA201(L) TO     ZAI-F03(2:3)
             MOVE    TNA301(L) TO     ZAI-F03(5:2)
             PERFORM ZAI-READ-RTN
             IF      INV-FLG    =     1
                 MOVE     23   TO     ERR-FLG
                 MOVE    "R"   TO     EDIT-OPTION OF HINCD (L)
                 MOVE    "R"   TO     EDIT-OPTION OF TAN101(L)
                 MOVE    "R"   TO     EDIT-OPTION OF TAN201(L)
                 MOVE    "R"   TO     EDIT-OPTION OF TAN301(L)
                 MOVE    "R"   TO     EDIT-OPTION OF TNA101(L)
                 MOVE    "R"   TO     EDIT-OPTION OF TNA201(L)
                 MOVE    "R"   TO     EDIT-OPTION OF TNA301(L)
             ELSE
                 REWRITE  ZAI-REC
             END-IF
     END-PERFORM.
 CHK-ZAI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　　　　チェック　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-KAKNIN-RTN     SECTION.
*
     IF       KAKNIN   NOT  =  "Y" AND  "H"  AND  "B"
              IF   SRKBN    =   1
                   MOVE    11      TO   ERR-FLG
              ELSE
                   MOVE    19      TO   ERR-FLG
              END-IF
              MOVE      "R"        TO   EDIT-OPTION OF KAKNIN
              GO   TO   CHK-KAKNIN-EXIT
     END-IF.
     IF       KAKNIN        =  "B" AND
              SRKBN    NOT  =   1
              MOVE      19         TO   ERR-FLG
              MOVE     "R"         TO   EDIT-OPTION OF KAKNIN
     END-IF.
 CHK-KAKNIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      伝票　更新　処理　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 SGY-UPDT-RTN           SECTION.
     EVALUATE    SRKBN
         WHEN    1
              PERFORM   SGY-WRITE-RTN
*伝票_更新処理
*\\  93.06.24
*********     MOVE      "62"          TO   JYO-F01
*********     MOVE      SPACE         TO   JYO-F02
*********     PERFORM   JYO-READ-RTN
*********     IF   INV-FLG    =    1
*********          DISPLAY   "HJYOKEN INV KEY=62"  UPON STA
*********          STOP       RUN
*********     END-IF
*********     IF   SGYNO      =    9999999
*********          MOVE       1       TO   JYO-F04
*********          ELSE
*********          ADD        1       TO   JYO-F04
*********     END-IF
**************REWRITE   JYO-REC
         WHEN    2
              PERFORM   SGY-REWRITE-RTN
         WHEN    3
              PERFORM   SGY-DELETE-RTN
     END-EVALUATE.
 SGY-UPDT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認後画面　処理　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 KAKNIN-AFTER-RTN          SECTION.
*
     MOVE     ZERO      TO                  SAV-YMD.
*
     IF       KAKNIN    =  "Y"
              MOVE      SRKBN          TO   SAV-SRKBN
              MOVE      SRMEI          TO   SAV-SRMEI
              MOVE      SPACE          TO   ZNY0010   TABLE-AREA
              MOVE      SAV-SRKBN      TO   SRKBN
              MOVE      SAV-SRMEI      TO   SRMEI
              IF   SRKBN    =    1
                   PERFORM  SGYNO-SET-RTN
                   MOVE     3          TO   SHORI-F
              ELSE
                   MOVE     2          TO   SHORI-F
              END-IF
              MOVE      1              TO   PAGE-CNT
              MOVE      0              TO   MAX-LINE
     END-IF.
     IF       KAKNIN    =  "H"
         IF   SRKBN     =   1
              MOVE      SRKBN          TO   SAV-SRKBN
              MOVE      SRMEI          TO   SAV-SRMEI
              MOVE      HEAD1          TO   SAV-HEAD1
              MOVE      SPACE          TO   ZNY0010   TABLE-AREA
              MOVE      SAV-SRKBN      TO   SRKBN
              MOVE      SAV-SRMEI      TO   SRMEI
              MOVE      SAV-HEAD1      TO   HEAD1
              MOVE      SGYMD          TO   SAV-YMD
              PERFORM   SGYNO-SET-RTN
              MOVE      1              TO   PAGE-CNT
              MOVE      0              TO   MAX-LINE
              MOVE      3              TO   SHORI-F
         ELSE
              PERFORM   JI-REC-RTN
              MOVE      SPACE          TO   BODY1
              MOVE      0              TO   MAX-LINE   MAX-PAGE
              MOVE      2              TO   SHORI-F
         END-IF
     END-IF.
     IF       KAKNIN    =   "B"
              MOVE      SRKBN          TO   SAV-SRKBN
              MOVE      SRMEI          TO   SAV-SRMEI
              MOVE      BODY1          TO   SAV-BODY1
              MOVE      SPACE          TO   ZNY0010  TABLE-AREA
              MOVE      SAV-BODY1      TO   BODY1
              MOVE      SAV-SRKBN      TO   SRKBN
              MOVE      SAV-SRMEI      TO   SRMEI
              PERFORM   SGYNO-SET-RTN
              MOVE      1              TO   PAGE-CNT
              MOVE      0              TO   MAX-LINE  MAX-PAGE
              MOVE      3              TO   SHORI-F
     END-IF.
 KAKNIN-AFTER-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     作業実績ファイル登録　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 SGY-WRITE-RTN          SECTION.
*明細行登録
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  MAX-LINE
              MOVE     SPACE       TO   SGY-REC
              INITIALIZE           SGY-REC
              PERFORM  SGY-SET-RTN
              WRITE    SGY-REC
              PERFORM  ZAI-REWRITE-RTN
     END-PERFORM.
 SGY-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     作業実績ファイル更新　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 SGY-REWRITE-RTN          SECTION.
*明細行更新
     MOVE     SGYNO          TO   SGY-F01.
     MOVE     0              TO   SGY-F02.
     MOVE     0              TO   SGY-FLG.
     START    ZSGYODT KEY    >=   SGY-F01  SGY-F02
        INVALID  KEY
              MOVE      1    TO   SGY-FLG
        NOT   INVALID  KEY
              PERFORM  SGY-READ-RTN
     END-START.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL   I   >  MAX-LINE
*数量０は削除
              IF       TBL-SAKJYO(I)   =   1
                       PERFORM  ZAI-REWRITE-RTN
                       DELETE   ZSGYODT    RECORD
                       PERFORM  SGY-READ-RTN
              ELSE
                IF  SGY-FLG     =      0
                       PERFORM  ZAI-REWRITE-RTN
                       PERFORM  SGY-SET-RTN
                       REWRITE  SGY-REC
                       PERFORM  SGY-READ-RTN
                ELSE
                       MOVE     SPACE  TO  SGY-REC
                       INITIALIZE      SGY-REC
                       PERFORM  ZAI-REWRITE-RTN
                       PERFORM  SGY-SET-RTN
                       WRITE    SGY-REC
                END-IF
              END-IF
     END-PERFORM.
 SGY-REWRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     作業実績ファイル削除　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 SGY-DELETE-RTN         SECTION.
     MOVE     SGYNO          TO   SGY-F01.
     MOVE     0              TO   SGY-F02.
     MOVE     0              TO   SGY-FLG.
     START    ZSGYODT KEY    >=   SGY-F01  SGY-F02
        INVALID  KEY
              MOVE     1     TO   SGY-FLG
        NOT   INVALID  KEY
              PERFORM  SGY-READ-RTN
     END-START.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I     >  MAX-LINE
                                       OR     SGY-FLG  =  1
              PERFORM  ZAI-REWRITE-RTN
              MOVE     1     TO   SGY-F97
              REWRITE  SGY-REC
              PERFORM  SGY-READ-RTN
     END-PERFORM.
 SGY-DELETE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     表示項目を作業実績ファイルに転送　　　　　　 *
*--------------------------------------------------------------*
 SGY-SET-RTN             SECTION.
*
     MOVE     SGYNO          TO   SGY-F01.
     MOVE     TBL-GYO101(I)  TO   SGY-F02.
* 93/07/01 担当者追加
     MOVE     TANTO          TO   SGY-F14.
* 93/07/01 担当者追加
     MOVE     SGKBN          TO   SGY-F03.
     MOVE     BASYO          TO   SGY-F04.
*## 1999/12/20 NAV
*****COMPUTE  SGY-F05  =  WK-SEIREKI  *  10000  +  SGYMD.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SGYMD               TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SGY-F05.
     MOVE     TBL-NYSKBN(I)  TO   SGY-F06.
     MOVE     TBL-STK101(I)  TO   WK-STK101.
     MOVE     TBL-STK201(I)  TO   WK-STK201.
     MOVE     WK-STKNO       TO   SGY-F07.
     MOVE     TBL-HINCD (I)  TO   SGY-F08.
     MOVE     TBL-TAN101(I)  TO   WK-TAN101.
     MOVE     TBL-TAN201(I)  TO   WK-TAN201.
     MOVE     TBL-TAN301(I)  TO   WK-TAN301.
     MOVE     WK-HINTAN      TO   SGY-F09.
     MOVE     TBL-TNA101(I)  TO   WK-TNA101.
     MOVE     TBL-TNA201(I)  TO   WK-TNA201.
     MOVE     TBL-TNA301(I)  TO   WK-TNA301.
     MOVE     WK-TNABN       TO   SGY-F10.
     MOVE     TBL-SURYO (I)  TO   SGY-F11.
     MOVE     TBL-BIKOU (I)  TO   SGY-F12.
     IF       SRKBN          =    1    OR
              SGY-FLG        =    1
***********COMPUTE  SGY-F98  =  WK-SEIREKI  *  10000  +  SYS-DATE
              MOVE SYS-DATE2 TO   SGY-F98
     ELSE
***********COMPUTE  SGY-F99  =  WK-SEIREKI  *  10000  +  SYS-DATE
              MOVE SYS-DATE2 TO   SGY-F99
     END-IF.
 SGY-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     商品在庫マスタ更新処理                       *
*--------------------------------------------------------------*
 ZAI-REWRITE-RTN         SECTION.
     IF       SRKBN          NOT =    2
              GO   TO    ZAI-REWRITE-01
     END-IF.
*修正前更新処理
     IF       TBL-TNA101(I)      =    TBL-WTANA1(I)   AND
              TBL-TNA201(I)      =    TBL-WTANA2(I)   AND
              TBL-TNA301(I)      =    TBL-WTANA3(I)   AND
              TBL-HINCD (I)      =    TBL-WHINCD(I)   AND
              TBL-TAN101(I)      =    TBL-WHTAN1(I)   AND
              TBL-TAN201(I)      =    TBL-WHTAN1(I)   AND
              TBL-TAN301(I)      =    TBL-WHTAN3(I)   AND
              TBL-NYSKBN(I)      =    TBL-WNYUSKBN(I)
          CONTINUE
     ELSE
          MOVE     BASYO               TO     ZAI-F01
          MOVE     TBL-WHINCD(I)       TO     ZAI-F021
          MOVE     TBL-WHTAN1(I)       TO     ZAI-F022(1:5)
          MOVE     TBL-WHTAN2(I)       TO     ZAI-F022(6:2)
          MOVE     TBL-WHTAN3(I)       TO     ZAI-F022(8:1)
          MOVE     TBL-WTANA1(I)       TO     ZAI-F03(1:1)
          MOVE     TBL-WTANA2(I)       TO     ZAI-F03(2:3)
          MOVE     TBL-WTANA3(I)       TO     ZAI-F03(5:2)
*         MOVE     TBL-WTANA1(I)       TO     ZAI-F031
*         MOVE     TBL-WTANA2(I)       TO     ZAI-F032
*         MOVE     TBL-WTANA3(I)       TO     ZAI-F033
          PERFORM  ZAI-READ-RTN
          IF       INV-FLG        =   0
              IF   TBL-WNYUSKBN(I)       =  "1"
                   PERFORM        ZAI-WIN-RTN
              ELSE
                   PERFORM        ZAI-WOUT-RTN
              END-IF
              REWRITE  ZAI-REC
          ELSE
              MOVE     SPACE           TO     ZAI-REC
              INITIALIZE          ZAI-REC
              MOVE     BASYO           TO     ZAI-F01
              MOVE     TBL-WHINCD(I)   TO     ZAI-F021
              MOVE     TBL-WHTAN1(I)   TO     ZAI-F022(1:5)
              MOVE     TBL-WHTAN2(I)   TO     ZAI-F022(6:2)
              MOVE     TBL-WHTAN3(I)   TO     ZAI-F022(8:1)
              MOVE     TBL-WTANA1(I)   TO     ZAI-F03(1:1)
              MOVE     TBL-WTANA2(I)   TO     ZAI-F03(2:3)
              MOVE     TBL-WTANA3(I)   TO     ZAI-F03(5:2)
*             MOVE     TBL-WTANA1(I)   TO     ZAI-F031
*             MOVE     TBL-WTANA2(I)   TO     ZAI-F032
*             MOVE     TBL-WTANA3(I)   TO     ZAI-F033
              IF   TBL-WNYUSKBN(I)       =  "1"
                   PERFORM        ZAI-WIN-RTN
              ELSE
                   PERFORM        ZAI-WOUT-RTN
              END-IF
*---<  93/05/26   START  >---*
*---<  商品名称（カナ）取得  >---*
              MOVE     ZAI-F021        TO      MEI-F011
              MOVE     ZAI-F022        TO      MEI-F012
              PERFORM  MEI-READ-RTN
              MOVE     MEI-F031        TO      ZAI-F04
*---<  93/05/26   END    >---*
              WRITE               ZAI-REC
          END-IF
          MOVE     1                   TO     REW-FLG
     END-IF.
*
 ZAI-REWRITE-01.
     MOVE     BASYO               TO     ZAI-F01.
     MOVE     TBL-HINCD (I)       TO     ZAI-F021.
     MOVE     TBL-TAN101(I)       TO     ZAI-F022(1:5).
     MOVE     TBL-TAN201(I)       TO     ZAI-F022(6:2).
     MOVE     TBL-TAN301(I)       TO     ZAI-F022(8:1).
     MOVE     TBL-TNA101(I)       TO     ZAI-F03(1:1).
     MOVE     TBL-TNA201(I)       TO     ZAI-F03(2:3).
     MOVE     TBL-TNA301(I)       TO     ZAI-F03(5:2).
*    MOVE     TBL-TNA101(I)       TO     ZAI-F031.
*    MOVE     TBL-TNA201(I)       TO     ZAI-F032.
*    MOVE     TBL-TNA301(I)       TO     ZAI-F033.
     PERFORM  ZAI-READ-RTN.
     IF       INV-FLG        =   0
         IF   TBL-NYSKBN(I)       =  "1"
              PERFORM        ZAI-IN-RTN
         ELSE
              PERFORM        ZAI-OUT-RTN
         END-IF
         REWRITE  ZAI-REC
     ELSE
         MOVE     SPACE           TO     ZAI-REC
         INITIALIZE          ZAI-REC
         MOVE     BASYO           TO     ZAI-F01
         MOVE     TBL-HINCD (I)   TO     ZAI-F021
         MOVE     TBL-TAN101(I)   TO     ZAI-F022(1:5)
         MOVE     TBL-TAN201(I)   TO     ZAI-F022(6:2)
         MOVE     TBL-TAN301(I)   TO     ZAI-F022(8:1)
         MOVE     TBL-TNA101(I)   TO     ZAI-F03(1:1)
         MOVE     TBL-TNA201(I)   TO     ZAI-F03(2:3)
         MOVE     TBL-TNA301(I)   TO     ZAI-F03(5:2)
*        MOVE     TBL-TNA101(I)   TO     ZAI-F031
*        MOVE     TBL-TNA201(I)   TO     ZAI-F032
*        MOVE     TBL-TNA301(I)   TO     ZAI-F033
         IF   TBL-NYSKBN(I)       =  "1"
              PERFORM        ZAI-IN-RTN
         ELSE
              PERFORM        ZAI-OUT-RTN
         END-IF
*---<  93/05/26   START  >---*
*---<  商品名称（カナ）取得  >---*
         MOVE     ZAI-F021        TO      MEI-F011
         MOVE     ZAI-F022        TO      MEI-F012
         PERFORM  MEI-READ-RTN
         MOVE     MEI-F031        TO      ZAI-F04
*---<  93/05/26   END    >---*
         WRITE               ZAI-REC
     END-IF.
 ZAI-REWRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   ワーク在庫 数量セット （入庫）
*--------------------------------------------------------------*
 ZAI-WIN-RTN         SECTION.
* ワークの数量セット（入庫データ）
     SUBTRACT  SGY-F11        FROM  ZAI-F06.
     IF  SGY-F05       >      ZAI-SIMER
         SUBTRACT  SGY-F11    FROM  ZAI-F19
         SUBTRACT  SGY-F11    FROM  ZAI-F17
     ELSE
         SUBTRACT  SGY-F11    FROM  ZAI-F08
         SUBTRACT  SGY-F11    FROM  ZAI-F10
     END-IF.
 ZAI-WIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   ワーク在庫 数量セット（出庫）
*--------------------------------------------------------------*
 ZAI-WOUT-RTN        SECTION.
* ワークの数量セット（出庫データ）
     ADD       SGY-F11        TO    ZAI-F06.
     IF  SGY-F05       >      ZAI-SIMER
         ADD       SGY-F11    TO    ZAI-F19
         SUBTRACT  SGY-F11    FROM  ZAI-F18
     ELSE
         ADD       SGY-F11    TO    ZAI-F08
         SUBTRACT  SGY-F11    FROM  ZAI-F11
     END-IF.

 ZAI-WOUT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫 数量セット （入庫）
*--------------------------------------------------------------*
 ZAI-IN-RTN         SECTION.
* 商品在庫マスタの数量セット（入庫データ）
     IF     SRKBN     =      1       OR
            REW-FLG   =      1
            ADD       TBL-SURYO(I)   TO    ZAI-F06
         IF  CHK-DATEX     >      ZAI-SIMER
            ADD       TBL-SURYO(I)   TO    ZAI-F19
            ADD       TBL-SURYO(I)   TO    ZAI-F17
         ELSE
            ADD       TBL-SURYO(I)   TO    ZAI-F08
            ADD       TBL-SURYO(I)   TO    ZAI-F10
         END-IF
            MOVE      0              TO    REW-FLG
            GO    TO  ZAI-IN-EXIT
     END-IF.
     IF     TBL-SAKJYO(I)    =  1
            SUBTRACT  SGY-F11        FROM  ZAI-F06
         IF  CHK-DATEX     >      ZAI-SIMER
            SUBTRACT  SGY-F11        FROM  ZAI-F19
            SUBTRACT  SGY-F11        FROM  ZAI-F17
         ELSE
            SUBTRACT  SGY-F11        FROM  ZAI-F08
            SUBTRACT  SGY-F11        FROM  ZAI-F10
         END-IF
            GO    TO  ZAI-IN-EXIT
     END-IF.
     IF     SRKBN     =      3
            SUBTRACT  TBL-SURYO(I)   FROM  ZAI-F06
         IF  CHK-DATEX     >      ZAI-SIMER
            SUBTRACT  TBL-SURYO(I)   FROM  ZAI-F19
            SUBTRACT  TBL-SURYO(I)   FROM  ZAI-F17
         ELSE
            SUBTRACT  TBL-SURYO(I)   FROM  ZAI-F08
            SUBTRACT  TBL-SURYO(I)   FROM  ZAI-F10
         END-IF
            GO    TO  ZAI-IN-EXIT
     END-IF.
     COMPUTE   WK-SURYO   =  TBL-SURYO(I)  -  SGY-F11.
     ADD    WK-SURYO         TO    ZAI-F06.
     IF  CHK-DATEX     >      ZAI-SIMER
         ADD       WK-SURYO       TO    ZAI-F19
         ADD       WK-SURYO       TO    ZAI-F17
     ELSE
         ADD       WK-SURYO       TO    ZAI-F08
         ADD       WK-SURYO       TO    ZAI-F10
     END-IF.
 ZAI-IN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫 数量セット（出庫）
*--------------------------------------------------------------*
 ZAI-OUT-RTN        SECTION.
* 商品在庫マスタの数量セット（出庫データ）
     IF     SRKBN     =      1       OR
            REW-FLG   =      1
            SUBTRACT  TBL-SURYO(I)   FROM  ZAI-F06
        IF  CHK-DATEX     >      ZAI-SIMER
            SUBTRACT  TBL-SURYO(I)   FROM  ZAI-F19
            ADD       TBL-SURYO(I)   TO    ZAI-F18
        ELSE
            SUBTRACT  TBL-SURYO(I)   FROM  ZAI-F08
            ADD       TBL-SURYO(I)   TO    ZAI-F11
        END-IF
            MOVE      0              TO    REW-FLG
            GO    TO  ZAI-OUT-EXIT
     END-IF.
     IF     TBL-SAKJYO(I)    =  1
            ADD       SGY-F11        TO    ZAI-F06
        IF  CHK-DATEX     >      ZAI-SIMER
            ADD       SGY-F11        TO    ZAI-F19
            SUBTRACT  SGY-F11        FROM  ZAI-F18
        ELSE
            ADD       SGY-F11        TO    ZAI-F08
            SUBTRACT  SGY-F11        FROM  ZAI-F11
        END-IF
            GO    TO  ZAI-OUT-EXIT
     END-IF.
     IF     SRKBN     =      3
            ADD       TBL-SURYO(I)   TO    ZAI-F06
        IF  CHK-DATEX     >      ZAI-SIMER
            ADD       TBL-SURYO(I)   TO    ZAI-F19
            SUBTRACT  TBL-SURYO(I)   FROM  ZAI-F18
        ELSE
            ADD       TBL-SURYO(I)   TO    ZAI-F08
            SUBTRACT  TBL-SURYO(I)   FROM  ZAI-F11
        END-IF
            GO    TO  ZAI-OUT-EXIT
     END-IF.
     COMPUTE      WK-SURYO   =   TBL-SURYO(I)  -  SGY-F11.
     SUBTRACT     WK-SURYO   FROM    ZAI-F06.
     IF  CHK-DATEX     >      ZAI-SIMER
         SUBTRACT     WK-SURYO   FROM    ZAI-F19
         ADD          WK-SURYO   TO      ZAI-F18
     ELSE
         SUBTRACT     WK-SURYO   FROM    ZAI-F08
         ADD          WK-SURYO   TO      ZAI-F11
     END-IF.
 ZAI-OUT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     明細　表示                                   *
*--------------------------------------------------------------*
 SGY-DSP-SET-RTN         SECTION.
     MOVE     0                     TO  TBL-FLG   S.
     MOVE     WK-CNT(PAGE-CNT)      TO  TBL-CNT.
     MOVE     SPACE                 TO  BODY1     KAKNIN.
     PERFORM  VARYING   IX   FROM   TBL-CNT  BY   1
                                    UNTIL     S   >=  6      OR
                                             IX   >   MAX-LINE
              IF    TBL-SAKJYO(IX) NOT   =    1
                    ADD      1      TO        S
                    MOVE TBL (IX)   TO        MEISAI(S)
                    COMPUTE  T   =  IX   +    1
              END-IF
     END-PERFORM.
     IF    SRKBN    =        1
           PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
              COMPUTE  T  =   ( PAGE-CNT * 6 )  -  6  +  I
              MOVE     T            TO   GYO101(I)
           END-PERFORM
     END-IF.
 SGY-DSP-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ヘッド部読込　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 HEAD-SET-RTN         SECTION.
     MOVE     SGY-F01             TO   SGYNO.
* 93/07/01 *ST*
     MOVE     SGY-F14             TO   TANTO.
* 93/07/01 *ED*
     MOVE     SGY-F05             TO   SGYMD.
     MOVE     SGY-F05             TO   CHK-DATEX.
     MOVE     SGY-F03             TO   SGKBN.
     MOVE     "71"                TO   JYO-F01.
     MOVE     SGY-F03             TO   JYO-F02.
     PERFORM  JYO-READ-RTN.
     IF       INV-FLG   =   0
              MOVE      JYO-F03   TO   SGMEI
***** < 93/06/01 START > *****
              REWRITE  JYO-REC
***** < 93/06/01  END  > *****
     ELSE
              MOVE      SPACE     TO   SGMEI
     END-IF.
     MOVE     SGY-F04             TO   BASYO   SOK-F01.
     PERFORM  SOK-READ-RTN.
     IF       INV-FLG   =   0
              MOVE      SOK-F02   TO   BASYOM
     ELSE
              MOVE      SPACE     TO   BASYOM
     END-IF.
  HEAD-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     明細テーブルセット　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 SGY-TBL-SET-RTN         SECTION.
     MOVE     SGY-F02        TO   TBL-GYO101(I).
     MOVE     SGY-F10(1:1)   TO   TBL-TNA101(I).
     MOVE     SGY-F10(2:3)   TO   TBL-TNA201(I).
     MOVE     SGY-F10(5:2)   TO   TBL-TNA301(I).
*    MOVE     SGY-F10(1:3)   TO   TBL-TNA101(I).
*    MOVE     SGY-F10(4:1)   TO   TBL-TNA201(I).
*    MOVE     SGY-F10(5:2)   TO   TBL-TNA301(I).
     MOVE     SGY-F08        TO   TBL-HINCD (I).
     MOVE     SGY-F09(1:5)   TO   TBL-TAN101(I).
     MOVE     SGY-F09(6:2)   TO   TBL-TAN201(I).
     MOVE     SGY-F09(8:1)   TO   TBL-TAN301(I).
     MOVE     SGY-F11        TO   TBL-SURYO (I).
     MOVE     SGY-F07(1:4)   TO   TBL-STK101(I).
     MOVE     SGY-F07(5:1)   TO   TBL-STK201(I).
     MOVE     SGY-F12        TO   TBL-BIKOU (I).
     MOVE     SGY-F06        TO   TBL-NYSKBN(I).
     MOVE     SGY-F08        TO   MEI-F011.
     MOVE     SGY-F09(1:5)   TO   MEI-F0121.
     MOVE     SGY-F09(6:2)   TO   MEI-F0122.
     MOVE     SGY-F09(8:1)   TO   MEI-F0123.
     PERFORM  MEI-READ-RTN.
     IF       INV-FLG   =    0
              MOVE      MEI-F021  TO   TBL-HINMEI(I)(1:15)
              MOVE      MEI-F022  TO   TBL-HINMEI(I)(16:15)
     ELSE
              MOVE      SPACE     TO   TBL-HINMEI(I)
     END-IF.
*変更前在庫ＫＥＹ退避
     MOVE     SGY-F10(1:1)   TO   TBL-WTANA1(I).
     MOVE     SGY-F10(2:3)   TO   TBL-WTANA2(I).
     MOVE     SGY-F10(5:2)   TO   TBL-WTANA3(I).
*    MOVE     SGY-F10(1:3)   TO   TBL-WTANA1(I).
*    MOVE     SGY-F10(4:1)   TO   TBL-WTANA2(I).
*    MOVE     SGY-F10(5:2)   TO   TBL-WTANA3(I).
     MOVE     SGY-F08        TO   TBL-WHINCD(I).
     MOVE     SGY-F09(1:5)   TO   TBL-WHTAN1(I).
     MOVE     SGY-F09(6:2)   TO   TBL-WHTAN2(I).
     MOVE     SGY-F09(8:1)   TO   TBL-WHTAN3(I).
     MOVE     SGY-F06        TO   TBL-WNYUSKBN(I).
     MOVE     SGY-F11        TO   TBL-WSURYO(I).
  SGY-TBL-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     画面ＲＥＡＤ　　　　                        *
*--------------------------------------------------------------*
 DSP-RD-RTN           SECTION.
     IF       ERR-FLG   =    0
              MOVE      SPACE              TO   GMSG
     ELSE
              MOVE      MSG-TBL(ERR-FLG)   TO   GMSG
     END-IF.
     MOVE     "ALLF"    TO      DSP-GRP.
     PERFORM  DSP-WR-RTN.
*
     IF       ERR-FLG   NOT =   0
              MOVE      "AL"    TO   DSP-PRO
              MOVE      0       TO   ERR-FLG
     ELSE
              MOVE      "NE"    TO   DSP-PRO
     END-IF.
*
     MOVE     WK-GRP            TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE             TO   DSP-PRO.
 DSP-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL      画面ＷＲＩＴＥ　                           *
*--------------------------------------------------------------*
 DSP-WR-RTN             SECTION.
     MOVE     SPACE        TO   DSP-PRO.
     WRITE    ZNY0010.
 DSP-WR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     伝票番号取得・場所自動設定                  *
*--------------------------------------------------------------*
 SGYNO-SET-RTN          SECTION.
*伝票_採番処理
     MOVE     "62"                TO   JYO-F01.
     MOVE     SPACE               TO   JYO-F02.
     PERFORM  JYO-READ-RTN.
     IF       INV-FLG        =    1
              DISPLAY   "HJYOKEN INV KEY=62"  UPON STA
              STOP      RUN
     END-IF.
     MOVE     JYO-F04             TO   SGYNO.
*
     IF       SGYNO      =    9999999
              MOVE       1       TO   JYO-F04
     ELSE
              ADD        1       TO   JYO-F04
     END-IF.
***** < 93/06/01 START > *****
     REWRITE  JYO-REC.
***** < 93/06/01 START > *****
*作業完成日システム日付設定
     IF       SAV-YMD       =          ZERO
*## 1999/12/20 NAV
****     MOVE     SYS-YY    TO   SGYMD(1:2)
****     MOVE     SYS-MM    TO   SGYMD(3:2)
****     MOVE     SYS-DD    TO   SGYMD(5:2)
         MOVE     SYS-YY2-2 TO   SGYMD(1:2)
         MOVE     SYS-MM2   TO   SGYMD(3:2)
         MOVE     SYS-DD2   TO   SGYMD(5:2)
     END-IF.
*場所自動設定
     IF       WK-BASYO  NOT  =    1
              MOVE  WK-BASYO      TO   BASYO
              MOVE  "X"           TO   EDIT-STATUS OF BASYO
              MOVE  WK-BASYO      TO   SOK-F01
              PERFORM   SOK-READ-RTN
              IF        INV-FLG   =    0
                        MOVE  SOK-F02  TO   BASYOM
              ELSE
                        MOVE  SPACE    TO   BASYOM
              END-IF
     END-IF.
*行_自動設定
     PERFORM  VARYING  I  FROM  1  BY  1    UNTIL  I  >  6
              MOVE     I               TO   GYO101(I)
     END-PERFORM.
 SGYNO-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     日付チェック　　                            *
*--------------------------------------------------------------*
 YMD-CHK-RTN            SECTION.
***  COMPUTE  CHK-YY    =  WK-SEIREKI  +  CHK-YY.
***  DIVIDE   CHK-YY  BY   4  GIVING   CHK-01  REMAINDER  CHK-02.
***  IF       CHK-02    =  0
***           MOVE     29         TO   WK-MATUBI(2)
***  ELSE
***           MOVE     28         TO   WK-MATUBI(2)
***  END-IF.
***  IF       CHK-MM    =  0   OR  >   12
***           IF        ERR-FLG  =  ZERO
***                    MOVE      5         TO   ERR-FLG
***           END-IF
***           GO   TO   YMD-CHK-RTN-EXIT
***  END-IF.
***  IF       CHK-DD    =  0
***  OR       CHK-DD    >  WK-MATUBI(CHK-MM)
***           IF        ERR-FLG  =  ZERO
***                    MOVE      5         TO   ERR-FLG
***           END-IF
************* MOVE      5         TO   ERR-FLG
***  END-IF.
*## 1999/12/20 NAV
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     CHK-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF        LINK-OUT-RET  NOT =  ZERO
               MOVE      5         TO   ERR-FLG
     END-IF.
 YMD-CHK-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫マスタ　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 ZAI-READ-RTN           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     ZZAIMS    INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 ZAI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品名称マスタ　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 MEI-READ-RTN           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HMEIMS    INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 MEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   条件ファイル　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 JYO-READ-RTN           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HJYOKEN   INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 JYO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   倉庫マスタ　　　ＲＥＡＤ                     *
*--------------------------------------------------------------*
 SOK-READ-RTN           SECTION.
     MOVE     0              TO   INV-FLG.
     READ     ZSOKMS   INVALID  KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 SOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   作業実績マスタ　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 SGY-READ-RTN           SECTION.
     MOVE     0                  TO   SGY-FLG.
     READ     ZSGYODT   NEXT     AT   END
              MOVE      1        TO   SGY-FLG
              GO   TO   SGY-READ-EXIT
     END-READ.
     IF       SGY-F01  NOT  =  SGYNO
              MOVE      1        TO   SGY-FLG
              GO   TO   SGY-READ-EXIT
     END-IF.
     IF       SGY-F13       =    1    OR
              SGY-F97       =    1
              GO   TO   SGY-READ-RTN
     END-IF.
     IF       WK-BASYO  NOT =    1    AND   SGY-F04
              GO   TO   SGY-READ-RTN
     END-IF.
     IF       SRKBN         =    9
              REWRITE   SGY-REC
     END-IF.
 SGY-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   作業実績マスタ　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 SGY-READ1-RTN           SECTION.
     MOVE     0                  TO   SGY-FLG.
     READ     ZSGYODT   NEXT     AT   END
              MOVE      1        TO   SGY-FLG
              GO   TO   SGY-READ1-EXIT
     END-READ.
     IF       SGY-F13       =    1    OR
              SGY-F97       =    1
              GO   TO   SGY-READ1-RTN
     END-IF.
     IF       WK-BASYO  NOT =    1    AND   SGY-F04
              GO   TO   SGY-READ1-RTN
     END-IF.
     IF       SRKBN         =    9
              REWRITE   SGY-REC
     END-IF.
 SGY-READ1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF SRKBN
*93/07/01 担当者追加
                                  EDIT-CURSOR OF TANTO
*93/07/01 担当者追加
                                  EDIT-CURSOR OF SGYNO
                                  EDIT-CURSOR OF SGYMD
                                  EDIT-CURSOR OF SGKBN
                                  EDIT-CURSOR OF BASYO.
     MOVE     "M"            TO   EDIT-OPTION OF SRKBN
*93/07/01 担当者追加
                                  EDIT-OPTION OF TANTO
*93/07/01 担当者追加
                                  EDIT-OPTION OF SGYNO
                                  EDIT-OPTION OF SGYMD
                                  EDIT-OPTION OF SGKBN
                                  EDIT-OPTION OF BASYO.
     IF    WK-BASYO   =      1
           MOVE      " "     TO   EDIT-STATUS OF BASYO
     ELSE
           MOVE      "X"     TO   EDIT-STATUS OF BASYO
     END-IF.
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY-RTN           SECTION.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
        MOVE    " "      TO   EDIT-CURSOR OF TNA101(I)
                              EDIT-CURSOR OF TNA201(I)
                              EDIT-CURSOR OF TNA301(I)
                              EDIT-CURSOR OF HINCD (I)
                              EDIT-CURSOR OF TAN101(I)
                              EDIT-CURSOR OF TAN201(I)
                              EDIT-CURSOR OF TAN301(I)
                              EDIT-CURSOR OF SURYO (I)
                              EDIT-CURSOR OF STK101(I)
                              EDIT-CURSOR OF STK201(I)
                              EDIT-CURSOR OF BIKOU (I)
                              EDIT-CURSOR OF NYSKBN(I)
                              EDIT-STATUS OF TNA101(I)
                              EDIT-STATUS OF TNA201(I)
                              EDIT-STATUS OF TNA301(I)
                              EDIT-STATUS OF HINCD (I)
                              EDIT-STATUS OF TAN101(I)
                              EDIT-STATUS OF TAN201(I)
                              EDIT-STATUS OF TAN301(I)
                              EDIT-STATUS OF SURYO (I)
                              EDIT-STATUS OF STK101(I)
                              EDIT-STATUS OF STK201(I)
                              EDIT-STATUS OF BIKOU (I)
                              EDIT-STATUS OF NYSKBN(I)
        MOVE    "M"      TO   EDIT-OPTION OF TNA101(I)
                              EDIT-OPTION OF TNA201(I)
                              EDIT-OPTION OF TNA301(I)
                              EDIT-OPTION OF HINCD (I)
                              EDIT-OPTION OF TAN101(I)
                              EDIT-OPTION OF TAN201(I)
                              EDIT-OPTION OF TAN301(I)
                              EDIT-OPTION OF SURYO (I)
                              EDIT-OPTION OF STK101(I)
                              EDIT-OPTION OF STK201(I)
                              EDIT-OPTION OF BIKOU (I)
                              EDIT-OPTION OF NYSKBN(I)
     END-PERFORM.
 CLR-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF KAKNIN.
     MOVE     "M"        TO   EDIT-OPTION OF KAKNIN.
 CLR-TAIL-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
