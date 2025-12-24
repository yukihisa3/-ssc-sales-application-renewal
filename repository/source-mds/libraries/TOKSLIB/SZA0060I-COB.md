# SZA0060I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SZA0060I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）                    *
*    業務名　　　　　：　在庫管理システム                      *
*    モジュール名　　：　作業実績入力                          *
*    作成日　　　　　：  00/05/15                              *
*    作成者　　　　　：  NAV                                   *
*    更新日　　　　　：  05/02/03                              *
*    更新者　　　　　：  NAV T                                 *
*    更新内容　　　　：　部門ＣＤ追加                          *
*    更新日　　　　　：  06/01/16                              *
*    更新者　　　　　：  ＮＡＶ松野                          *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0060I.
 AUTHOR.                N.T.
 DATE-WRITTEN.          00/05/15.
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
     SELECT   SGYFILF   ASSIGN    TO        DA-01-VI-SGYFILL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       SGY-F01   SGY-F02
                        FILE      STATUS    SGY-ST1.
*----<< 商品在庫マスタ >>-*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
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
*----<< 作業区分マスタ >>-*
     SELECT   SGYKBMF   ASSIGN    TO        DA-01-VI-SGYKBML1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       KBM-F01
                        FILE      STATUS    KBM-ST1.
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
 FD  SGYFILF            LABEL     RECORD     IS  STANDARD.
     COPY     SGYFILF   OF   XFDLIB    JOINING   SGY  AS PREFIX.
*----<< 商品在庫マスタ >>-*
 FD  ZAMZAIF            LABEL     RECORD     IS  STANDARD.
     COPY     ZAMZAIF   OF   XFDLIB    JOINING   ZAI  AS PREFIX.
*----<< 条件ファイル >>-*
 FD  HJYOKEN            LABEL     RECORD     IS  STANDARD.
     COPY     JYOKEN1   OF   XFDLIB    JOINING   JYO  AS PREFIX.
*----<< 倉庫マスタ >>-*
 FD  ZSOKMS             LABEL     RECORD     IS  STANDARD.
     COPY     ZSOKMS    OF   XFDLIB    JOINING   SOK  AS PREFIX.
*----<< 商品名称マスタ >>-*
 FD  HMEIMS             LABEL     RECORD     IS  STANDARD.
     COPY     MEIMS1    OF   XFDLIB    JOINING   MEI  AS PREFIX.
*----<< 作業区分マスタ >>-*
 FD  SGYKBMF            LABEL     RECORD     IS  STANDARD.
     COPY     SGYKBMF   OF   XFDLIB    JOINING   KBM  AS PREFIX.
*----<< 画面ファイル >>-*
 FD  DSPFILE.
     COPY     FZA00601  OF   XMDLIB.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  PGM-ID                  PIC  X(08)  VALUE  "SZA0060I".
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
     03  KBM-ST.
         05  KBM-ST1         PIC  X(02)  VALUE  SPACE.
         05  KBM-ST2         PIC  X(04)  VALUE  SPACE.
*入力情報の退避領域
 01  TABLE-AREA.
     03  TBL-DSP             OCCURS    24.
         05  TBL-D-GYO101    PIC  9(02).
         05  TBL-D-HINMEI    PIC  N(30).
         05  TBL-D-MISYK     PIC S9(07)V99.
         05  TBL-D-MIHIK     PIC S9(07)V99.
     03  TBL-NEW             OCCURS    24.
         05  TBL-N-NYSKBN    PIC  X(01).
         05  TBL-N-TNA.
           07  TBL-N-TNA101  PIC  X(01).
           07  TBL-N-TNA201  PIC  X(03).
           07  TBL-N-TNA301  PIC  X(02).
         05  TBL-N-HINCD     PIC  X(08).
         05  TBL-N-TAN.
           07  TBL-N-TAN101  PIC  X(05).
           07  TBL-N-TAN201  PIC  X(02).
           07  TBL-N-TAN301  PIC  X(01).
         05  TBL-N-SURYO     PIC S9(07)V99.
         05  TBL-N-STK.
           07  TBL-N-STK101  PIC  X(05).
           07  TBL-N-STK201  PIC  X(01).
         05  TBL-N-BIKOU     PIC  X(10).
     03  TBL-N-MISYK         PIC S9(07)V99   OCCURS 24.
     03  TBL-N-MIHIK         PIC S9(07)V99   OCCURS 24.
     03  TBL-OLD             OCCURS    24.
         05  TBL-O-NYSKBN    PIC  X(01).
         05  TBL-O-TNA.
           07  TBL-O-TNA101  PIC  X(01).
           07  TBL-O-TNA201  PIC  X(03).
           07  TBL-O-TNA301  PIC  X(02).
         05  TBL-O-HINCD     PIC  X(08).
         05  TBL-O-TAN.
           07  TBL-O-TAN101  PIC  X(05).
           07  TBL-O-TAN201  PIC  X(02).
           07  TBL-O-TAN301  PIC  X(01).
         05  TBL-O-SURYO     PIC S9(07)V99.
         05  TBL-O-STK.
           07  TBL-O-STK101  PIC  X(05).
           07  TBL-O-STK201  PIC  X(01).
         05  TBL-O-BIKOU     PIC  X(10).
     03  TBL-O-MISYK         PIC S9(07)V99   OCCURS 24.
     03  TBL-O-MIHIK         PIC S9(07)V99   OCCURS 24.
*画面退避用
     COPY   FZA00601  OF XMDLIB  JOINING   SAV  AS   PREFIX.
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
     03  MSG26               PIC  N(20)  VALUE
         NC"未出庫に誤りがあります。".
     03  MSG27               PIC  N(20)  VALUE
         NC"未出庫数、変更・削除時注意して下さい。".
     03  MSG28               PIC  N(20)  VALUE
         NC"出庫数と入庫数は同じにして下さい。".
     03  MSG29               PIC  N(20)  VALUE
         NC"入庫と同じ商品が入力されています。".
     03  MSG30               PIC  N(20)  VALUE
         NC"出庫と同じ商品が入力されています。".
     03  MSG31               PIC  N(20)  VALUE
         NC"_移動は、同一商品ＣＤのみ入力できます。".
     03  MSG32               PIC  N(20)  VALUE
         NC"計上部門ＣＤが取得できません。".
     03  MSG33               PIC  N(20)  VALUE
         NC"計上部門ＣＤ入力エラーです。".
     03  MSG34               PIC  N(20)  VALUE
         NC"計上部門を入力して下さい。".
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       34.
*----<< ﾜｰｸ ｴﾘｱ >>-*
 01  WK-AREA.
     03  WK-BASYO1           PIC  X(02)  VALUE  SPACE.
     03  WK-BASYO2           PIC  X(02)  VALUE  SPACE.
     03  WK-SIMEBI           PIC  9(08)  VALUE  ZERO.
     03  WK-SYSDATE          PIC  9(08)  VALUE  ZERO.
     03  WK-SGYDATE          PIC  9(08)  VALUE  ZERO.
     03  WK-F01              PIC  9(07)  VALUE  ZERO.
     03  WK-KBM-F04          PIC  X(01).
     03  WK-KBM-F05          PIC  X(01).
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
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  CHK-FLG             PIC  X(01)  VALUE  SPACE.
     03  TBL-FLG             PIC  9(01)  VALUE  ZERO.
     03  SGY-FLG             PIC  9(01)  VALUE  ZERO.
     03  SHORI-F             PIC  9(02)  VALUE  ZERO.
     03  NYU-FLG             PIC  X(01).
     03  SYU-FLG             PIC  X(01).
     03  WRITE-FLG           PIC  9(01).
 01  COUNTERS.
     03  PAGE-CNT            PIC  9(02)  VALUE  ZERO.
     03  TBL-CNT             PIC  9(02)  VALUE  ZERO.
     03  MAX-GYO             PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  ZERO.
     03  MAX-PAGE            PIC  9(01)  VALUE  ZERO.
 01  DSP-WORK.
     03  WK-TNABN.
         05  WK-TNA101       PIC  X(01)  VALUE  SPACE.
         05  WK-TNA201       PIC  X(03)  VALUE  SPACE.
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
*
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
*
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
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
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
     USE         AFTER       EXCEPTION PROCEDURE   SGYFILF.
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
     USE         AFTER       EXCEPTION PROCEDURE   ZAMZAIF.
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
*----------   作業区分マスタ　 --------------------------------*
 KBM-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   SGYKBMF.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"作業区分マスタ異常！"
              "ST1=" KBM-ST1                 " "
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
*画面日付・時刻編集
     MOVE      SYS-DATE2(1:4)     TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE2(5:2)     TO   HEN-DATE-MM.
     MOVE      SYS-DATE2(7:2)     TO   HEN-DATE-DD.
     MOVE      SYS-TIME(1:2)      TO   HEN-TIME-HH.
     MOVE      SYS-TIME(3:2)      TO   HEN-TIME-MM.
     MOVE      SYS-TIME(5:2)      TO   HEN-TIME-SS.
* ファイル　ＯＰＥＮ
     OPEN     I-O       SGYFILF.
     OPEN     I-O       ZAMZAIF.
     OPEN     I-O       HJYOKEN.
     OPEN     INPUT     ZSOKMS.
     OPEN     INPUT     HMEIMS.
     OPEN     INPUT     SGYKBMF.
     OPEN     I-O       DSPFILE.
*在庫締日取得
     MOVE      99            TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     MOVE     "ZAI"          TO   JYO-F02.
     READ     HJYOKEN
          INVALID
              DISPLAY   "HJYOKEN INV KEY=99"  UPON STA
              MOVE      "END"     TO   END-FLG
              GO   TO   INIT-EXIT
     END-READ.
     MOVE     JYO-F05        TO   ZAI-SIME1.
     ADD      1              TO   ZAI-SIME1.
     IF       ZAI-SIME1R2    >    12
              MOVE 1         TO   ZAI-SIME1R2
              ADD  1         TO   ZAI-SIME1R1
     END-IF.
     MOVE     31             TO   ZAI-SIME2.
     MOVE     JYO-F04        TO   WK-ZAIKO-SIME.
* 場所の取得
     MOVE     "65"           TO   JYO-F01.
     MOVE     LINK-WSMEI     TO   JYO-F02.
     PERFORM  JYO-READ-RTN.
     IF       INV-FLG   =    1
              DISPLAY   "HJYOKEN INV KEY=65" LINK-WSMEI UPON STA
              MOVE      "END"     TO   END-FLG
              GO   TO   INIT-EXIT
     END-IF.
     MOVE     JYO-F14(1:2)   TO   WK-BASYO1.
     MOVE     JYO-F15(1:2)   TO   WK-BASYO2.
     REWRITE  JYO-REC.
*特販部名称編集
     MOVE     SPACE               TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE    "99"                 TO   JYO-F01.
     MOVE    "BUMON"              TO   JYO-F02.
     READ     HJYOKEN
       INVALID KEY
              MOVE NC"＊＊＊＊＊＊"   TO   HEN-TOKHAN
       NOT INVALID KEY
              MOVE JYO-F03            TO   HEN-TOKHAN
     END-READ.
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
     CLOSE              SGYFILF
                        ZAMZAIF
                        HJYOKEN
                        ZSOKMS
                        HMEIMS
                        SGYKBMF
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
     MOVE     SPACE          TO   FZA00601.
     MOVE     SPACE          TO   TABLE-AREA.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-BODY-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE     "FZA00601"     TO   DSP-FMT.
     MOVE     "ALLF"         TO   DSP-GRP.
*
     MOVE     1              TO   SHORI-F.
     MOVE     1              TO   PAGE-CNT.
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
              CLOSE        SGYFILF
              OPEN    I-O  SGYFILF
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
              CLOSE        SGYFILF
              OPEN    I-O  SGYFILF
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
     CLOSE       SGYFILF.
     OPEN        I-O          SGYFILF.
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
 CHK010.
     MOVE     SGYNO          TO   SGY-F01.
     MOVE     0              TO   SGY-F02.
     START    SGYFILF  KEY   >=   SGY-F01    SGY-F02
         INVALID   KEY
              MOVE     1          TO   SGY-FLG
         NOT  INVALID  KEY
**************DISPLAY "CHR011" UPON CONS
              PERFORM  SGY-READ-RTN
     END-START.
 CHK020.
*  登録の時
     IF       SGY-FLG        =     0    AND
              SRKBN          =     1
              MOVE      4          TO   ERR-FLG
              MOVE     "R"         TO   EDIT-OPTION OF SGYNO
              GO   TO   CHK-SGYNO-EXIT
     END-IF.
 CHK030.
*  登録以外の時
     IF       SGY-FLG        =     1    AND
              SRKBN     NOT  =     1
              MOVE      3          TO   ERR-FLG
              MOVE     "R"         TO   EDIT-OPTION OF SGYNO
              GO   TO   CHK-SGYNO-EXIT
     END-IF.
*
 CHK030.
     IF       SRKBN     =    1
              GO   TO   CHK-SGYNO-EXIT
     END-IF.
*画面＆ＴＢＬクリア
     MOVE     SRKBN          TO    SAV-SRKBN.
     MOVE     SRMEI          TO    SAV-SRMEI.
     MOVE     HEAD1          TO    SAV-HEAD1.
     MOVE     SPACE          TO    FZA00601   TABLE-AREA.
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
     CLOSE       SGYFILF.
     OPEN        I-O          SGYFILF.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
     IF       SGYNO   IS NOT  NUMERIC
              MOVE     0      TO   SGYNO
     END-IF.
*伝票_チェック
     MOVE     SGYNO           TO   SGY-F01.
     MOVE     0               TO   SGY-F02.
     START    SGYFILF  KEY     <   SGY-F01   SGY-F02
                                      WITH   REVERSED  ORDER
         INVALID   KEY
              MOVE     1      TO   SGY-FLG
         NOT  INVALID  KEY
              PERFORM  SGY-READ1-RTN
              IF   SGY-FLG    =    0
                   MOVE       SGY-F01   TO   WK-F01
                   MOVE       WK-F01    TO   SGY-F01
                   MOVE       0         TO   SGY-F02
                   START      SGYFILF  KEY   >=   SGY-F01  SGY-F02
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
     MOVE     SPACE           TO   FZA00601   TABLE-AREA.
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
     CLOSE       SGYFILF.
     OPEN        I-O          SGYFILF.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
     IF       SGYNO   IS NOT  NUMERIC
              MOVE     0      TO   SGYNO
     END-IF.
*伝票_チェック
     MOVE     SGYNO           TO   SGY-F01.
     MOVE     99              TO   SGY-F02.
     START    SGYFILF  KEY    >    SGY-F01    SGY-F02
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
     MOVE     SPACE           TO   FZA00601   TABLE-AREA.
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
                  MOVE  1         TO   PAGE-CNT
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
*
***** 担当者 ****
     IF       TANTO    =   SPACE
              MOVE     24         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF TANTO
              MOVE     "R"        TO   EDIT-OPTION OF TANTO
     END-IF.
***** 作業区分チェック *****
 CHK-GRP003-10.
     MOVE     SPACE          TO   SGMEI.
     MOVE     SPACE          TO   OUTKB     INKB.
     MOVE     SGKBN          TO   KBM-F01.
     PERFORM  KBM-READ-RTN.
     IF       INV-FLG  =  1
              IF   ERR-FLG   =   ZERO
                   MOVE      6    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF SGKBN
              MOVE     "R"        TO   EDIT-OPTION OF SGKBN
     ELSE
        IF    KBM-F03  =  1
              MOVE     KBM-F02    TO   SGMEI
              MOVE     KBM-F04    TO   WK-KBM-F04
                                       OUTKB
              MOVE     KBM-F05    TO   WK-KBM-F05
                                       INKB
        ELSE
              IF   ERR-FLG   =   ZERO
                   MOVE      6    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF SGKBN
              MOVE     "R"        TO   EDIT-OPTION OF SGKBN
        END-IF
     END-IF.
***** 作業完成日 *****
 CHK-GRP003-20.
     DISPLAY "ABC" UPON CONS.
     IF       SGYMD    IS NOT     NUMERIC
              MOVE      0    TO   SGYMD
     END-IF.
*  日付論理チェック
     MOVE     "1"            TO   LINK-IN-KBN.
     MOVE     SGYMD          TO   LINK-IN-YMD6.
     MOVE     ZERO           TO   LINK-IN-YMD8.
     MOVE     ZERO           TO   LINK-OUT-RET.
     MOVE     ZERO           TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"  USING   LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET  NOT =  ZERO
              IF   ERR-FLG   =   ZERO
                   MOVE      5    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF SGYMD
              MOVE     "R"        TO   EDIT-OPTION OF SGYMD
          DISPLAY "AAA" UPON CONS
              GO   TO   CHK-GRP003-30
     END-IF.
 CHK-GRP003-21.
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
     REWRITE  JYO-REC.
*締日－１算出 2001/10/09  追加
     COMPUTE  WK-SIMEBI  =  WK-SIMEBI  -  1.
*
     MOVE     SYS-DATE2           TO   WK-SYSDATE.
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
     MOVE     LINK-OUT-YMD        TO   WK-SGYDATE.
     IF       WK-SIMEBI   <  WK-SYSDATE
              IF   ERR-FLG   =   ZERO
                   MOVE      5    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF SGYMD
              MOVE     "R"        TO   EDIT-OPTION OF SGYMD
          DISPLAY "BBB" UPON CONS
              GO   TO   CHK-GRP003-30
     END-IF.
 CHK-GRP003-22.
     IF       WK-SYSDATE  <  WK-SGYDATE
              IF   ERR-FLG   =   ZERO
                   MOVE      5    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF SGYMD
              MOVE     "R"        TO   EDIT-OPTION OF SGYMD
          DISPLAY "CCC" UPON CONS
              GO   TO   CHK-GRP003-30
     END-IF.
*----<許容範囲月算出>
     MOVE     WK-ZAIKO-SIME-2     TO   WK-H-TUKI-1.
     COMPUTE  WK-H-TUKI-2  =  WK-ZAIKO-SIME-2  -  1.
     IF       WK-H-TUKI-2  =  ZERO
              MOVE   12           TO   WK-H-TUKI-2
     END-IF.
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
     MOVE     WK-HANI2            TO   WK-HENKAN.
     IF       WK-SGYDATE  <=  WK-HANI1
              MOVE   01           TO   WK-HENKAN-3
     ELSE
              ADD    1            TO   WK-HENKAN-2
              IF     WK-HENKAN-2  >  12
                     ADD    1     TO   WK-HENKAN-1
                     MOVE   01    TO   WK-HENKAN-2
              END-IF
              MOVE   01           TO   WK-HENKAN-3
     END-IF.
     MOVE     WK-HENKAN           TO   WK-HANI2.
 CHK-GRP003-23.
*----<許容範囲チェック>
     IF       WK-HANI2   <=  WK-SGYDATE
              CONTINUE
     ELSE
              IF  ERR-FLG       =   0
                  MOVE   5        TO   ERR-FLG
              END-IF
              MOVE   "C"          TO   EDIT-CURSOR  OF  SGYMD
              MOVE   "R"          TO   EDIT-OPTION  OF  SGYMD
          DISPLAY "DDD" UPON CONS
     END-IF.
 CHK-GRP003-24.
***** 作業場所チェック *****
 CHK-GRP003-30.
     IF       WK-BASYO1   =  "01"     OR
              WK-BASYO2   =  "01"
              MOVE     SPACE      TO   BASYOM
              MOVE     BASYO      TO   SOK-F01
              PERFORM  SOK-READ-RTN
              IF       INV-FLG   =    ZERO
                  MOVE    SOK-F02     TO   BASYOM
              ELSE
                  IF   ERR-FLG   =    ZERO
                       MOVE      7    TO   ERR-FLG
                  END-IF
                  MOVE    "C"         TO   EDIT-CURSOR OF BASYO
                  MOVE    "R"         TO   EDIT-OPTION OF BASYO
              END-IF
     END-IF.
***** 計上部門チェック *****
 CHK-GRP003-35.
     IF       BUMON  NOT NUMERIC
     OR       BUMON  =  ZERO
**************計上部門ＣＤ編集
              MOVE    "20"                 TO   JYO-F01
              MOVE    BASYO                TO   JYO-F02
              PERFORM JYO-READ-RTN
              IF   INV-FLG  =  1
                   IF   ERR-FLG   =    ZERO
                        MOVE      32   TO   ERR-FLG
                   END-IF
                   MOVE    "C"         TO   EDIT-CURSOR OF BASYO
                   MOVE    "R"         TO   EDIT-OPTION OF BASYO
                   MOVE    "R"         TO   EDIT-OPTION OF BUMON
              ELSE
                   IF   JYO-F05  =  ZERO
                     IF   ERR-FLG   =    ZERO
                          MOVE    32   TO   ERR-FLG
                     END-IF
                     MOVE    "C"       TO   EDIT-CURSOR OF BASYO
                     MOVE    "R"       TO   EDIT-OPTION OF BASYO
                     MOVE    "R"       TO   EDIT-OPTION OF BUMON
                   ELSE
                     MOVE   JYO-F05    TO   BUMON
                   END-IF
              END-IF
              MOVE    "22"                 TO   JYO-F01
              MOVE    BUMON                TO   JYO-F02
              PERFORM JYO-READ-RTN
              IF   INV-FLG  =  1
                   IF   ERR-FLG   =    ZERO
                        MOVE      33   TO   ERR-FLG
                   END-IF
                   MOVE    "C"         TO   EDIT-CURSOR OF BUMON
                   MOVE    "R"         TO   EDIT-OPTION OF BUMON
                   MOVE ALL NC"＊"     TO   BUMONN
              ELSE
                   MOVE     JYO-F03    TO   BUMONN
              END-IF
     ELSE
**************計上部門ＣＤ編集
              MOVE    "20"                 TO   JYO-F01
              MOVE    BASYO                TO   JYO-F02
              PERFORM JYO-READ-RTN
              IF   INV-FLG  =  1
                   IF   ERR-FLG   =    ZERO
                        MOVE      32   TO   ERR-FLG
                   END-IF
                   MOVE    "C"         TO   EDIT-CURSOR OF BASYO
                   MOVE    "R"         TO   EDIT-OPTION OF BASYO
                   MOVE    "R"         TO   EDIT-OPTION OF BUMON
*             ELSE
*                  IF   JYO-F05  =  ZERO
*                    IF   ERR-FLG   =    ZERO
*                         MOVE    32   TO   ERR-FLG
*                    END-IF
*                    MOVE    "C"       TO   EDIT-CURSOR OF BASYO
*                    MOVE    "R"       TO   EDIT-OPTION OF BASYO
*                    MOVE    "R"       TO   EDIT-OPTION OF BUMON
*                  ELSE
*                    MOVE   JYO-F05    TO   BUMON
*                  END-IF
              END-IF
              MOVE    "22"                 TO   JYO-F01
              MOVE    BUMON                TO   JYO-F02
              PERFORM JYO-READ-RTN
              IF   INV-FLG  =  1
                   IF   ERR-FLG   =    ZERO
                        MOVE      33   TO   ERR-FLG
                   END-IF
                   MOVE    "C"         TO   EDIT-CURSOR OF BUMON
                   MOVE    "R"         TO   EDIT-OPTION OF BUMON
              ELSE
                   MOVE     JYO-F03    TO   BUMONN
              END-IF
     END-IF.
***** 未出庫チェック *****
 CHK-GRP003-40.
     IF       SGKBN   NOT =  "55"
            IF    MISYUK NOT NUMERIC   OR   MISYUK =  0
                  MOVE    SPACE       TO   MISYUX
                  MOVE    "M"         TO   EDIT-OPTION OF MISYUK
            ELSE
                  IF   ERR-FLG   =    ZERO
                       MOVE     26    TO   ERR-FLG
                  END-IF
                  MOVE    "C"         TO   EDIT-CURSOR OF MISYUK
                  MOVE    "R"         TO   EDIT-OPTION OF MISYUK
            END-IF
     ELSE
            IF    MISYUK NOT NUMERIC
                  MOVE    0           TO   MISYUK
            END-IF
            IF    MISYUK  =   0  OR  1
                  CONTINUE
            ELSE
                  IF   ERR-FLG   =    ZERO
                       MOVE     26    TO   ERR-FLG
                  END-IF
                  MOVE    "C"         TO   EDIT-CURSOR OF MISYUK
                  MOVE    "R"         TO   EDIT-OPTION OF MISYUK
            END-IF
     END-IF.
 CHK-GRP003-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      伝票　画面へ表示　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 SGY-DSP-RTN            SECTION.
     MOVE      SPACE             TO   TABLE-AREA.
*データ退避
     PERFORM   HEAD-SET-RTN.
     PERFORM   VARYING  I  FROM  1  BY  1  UNTIL  I  >  24  OR
                                            SGY-FLG  =  1
          PERFORM    SGY-TBL-SET-RTN
          PERFORM    SGY-READ-RTN
          ADD  1        TO   MAX-LINE
     END-PERFORM.
*データ表示
     MOVE      1        TO   PAGE-CNT.
     MOVE      0        TO   MAX-PAGE.
     PERFORM   SGY-DSP-SET-RTN.
 SGY-DSP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3        ボディ部　入力                            *
*--------------------------------------------------------------*
 DSP-BODY-RTN       SECTION.
     PERFORM  PRO-BODY-RTN.
*
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
              PERFORM   CHK-BODY-RTN
                        VARYING   L   FROM   1   BY   1
                                      UNTIL  L    >   6
              IF   ERR-FLG    =   ZERO
                   PERFORM   CHK-BODY1-RTN
              END-IF
              IF   ERR-FLG    =   ZERO
                   PERFORM        SGY-DSP-SET-RTN
                   MOVE   "Y"     TO    KAKNIN
                   MOVE    5      TO    SHORI-F
                   PERFORM        CLR-BODY-RTN
                   PERFORM        CHK-BODY9-RTN
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
     COMPUTE   T   =  ( PAGE-CNT  *  6 )  -  6  +  L.
     MOVE   SPACE                  TO   TBL-NEW (T).
*未入力行チェック無し
     IF     HINCD (L)     =    SPACE  AND
            TAN101(L)     =    SPACE  AND
            TAN201(L)     =    SPACE  AND
            TAN301(L)     =    SPACE
            IF (SRKBN     =    1)               OR
               (SRKBN     =    2      AND
               (TBL-D-GYO101(T) NOT NUMERIC  OR
                TBL-D-GYO101(T) =  ZERO    ))   OR
               (SRKBN     =    2      AND
                TBL-D-GYO101(T) >  MAX-GYO  )
                GO   TO   CHK-BODY-EXIT
            END-IF
     END-IF.
*修正時、追加明細の行番号を採番する
     IF     SRKBN     =    2
         PERFORM VARYING  J  FROM  T   BY  -1
                 UNTIL    J  =   0
                    OR  ( GYO101(L) NUMERIC
                      AND GYO101(L) NOT = ZERO )
                 IF   TBL-D-GYO101(J) NUMERIC   AND
                      TBL-D-GYO101(J) NOT =  ZERO
                      COMPUTE GYO101(L) = TBL-D-GYO101(J) + 1
                 END-IF
         END-PERFORM
     END-IF.
*** 商品コード右詰処理  ***
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
*** 品単５桁　頭空白詰 ***
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
*** 入力チェック ***
*
*入出庫区分
     IF  NYSKBN(L)  NOT =   "1" AND  "2"
         IF  ERR-FLG    =   ZERO
             MOVE     10    TO  ERR-FLG
         END-IF
             MOVE     "C"   TO  EDIT-CURSOR OF NYSKBN(L)
             MOVE     "R"   TO  EDIT-OPTION OF NYSKBN(L)
     ELSE
*出庫：入庫（１：１）
        IF     WK-KBM-F04 = "1"   AND   WK-KBM-F05 = "1"
            IF    L = 2
               IF   NYSKBN(1) = "1"  AND  NYSKBN(2) = "1"
                  IF  ERR-FLG    =   ZERO
                      MOVE     21    TO  ERR-FLG
                  END-IF
                  MOVE  "C"    TO    EDIT-CURSOR OF NYSKBN(L)
                  MOVE  "R"    TO    EDIT-OPTION OF NYSKBN(L)
               END-IF
               IF   NYSKBN(1) = "2"  AND  NYSKBN(2) = "2"
                  IF  ERR-FLG    =   ZERO
                      MOVE     22    TO  ERR-FLG
                  END-IF
                  MOVE  "C"    TO    EDIT-CURSOR OF NYSKBN(L)
                  MOVE  "R"    TO    EDIT-OPTION OF NYSKBN(L)
               END-IF
            END-IF
        END-IF
*出庫：入庫（Ｎ：１）　１行目は入庫・２行目以降は出庫
        IF     WK-KBM-F04 NOT = "1"   AND   WK-KBM-F05 = "1"
            IF    T = 1  AND   NYSKBN(L) NOT =  "1"
                  IF  ERR-FLG    =   ZERO
                      MOVE     22    TO  ERR-FLG
                  END-IF
                  MOVE  "C"    TO    EDIT-CURSOR OF NYSKBN(L)
                  MOVE  "R"    TO    EDIT-OPTION OF NYSKBN(L)
            END-IF
            IF    T > 1  AND   NYSKBN(L) NOT =  "2"
                  IF  ERR-FLG    =   ZERO
                      MOVE     21    TO  ERR-FLG
                  END-IF
                  MOVE  "C"    TO    EDIT-CURSOR OF NYSKBN(L)
                  MOVE  "R"    TO    EDIT-OPTION OF NYSKBN(L)
            END-IF
        END-IF
*出庫：入庫（１：Ｎ）　１行目は出庫・２行目以降は入庫
        IF     WK-KBM-F04 = "1"   AND   WK-KBM-F05 NOT = "1"
            IF    T = 1  AND   NYSKBN(L) NOT =  "2"
                  IF  ERR-FLG    =   ZERO
                      MOVE     21    TO  ERR-FLG
                  END-IF
                  MOVE  "C"    TO    EDIT-CURSOR OF NYSKBN(L)
                  MOVE  "R"    TO    EDIT-OPTION OF NYSKBN(L)
            END-IF
            IF    T > 1  AND   NYSKBN(L) NOT =  "1"
                  IF  ERR-FLG    =   ZERO
                      MOVE     22    TO  ERR-FLG
                  END-IF
                  MOVE  "C"    TO    EDIT-CURSOR OF NYSKBN(L)
                  MOVE  "R"    TO    EDIT-OPTION OF NYSKBN(L)
            END-IF
        END-IF
     END-IF.
*
*商品コード
     MOVE   HINCD (L)              TO   MEI-F011.
     MOVE   TAN101(L)              TO   MEI-F0121.
     MOVE   TAN201(L)              TO   MEI-F0122.
     MOVE   TAN301(L)              TO   MEI-F0123.
     PERFORM       MEI-READ-RTN.
     IF      INV-FLG         =     1
             MOVE      SPACE       TO   HINMEI(L)
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
*
*数量
     IF  SRKBN         =    1
*登録時は必須入力
         IF  SURYO(L)     NOT NUMERIC
             IF   ERR-FLG   =      ZERO
                  MOVE     25      TO   ERR-FLG
             END-IF
             MOVE     "C"   TO     EDIT-CURSOR OF SURYO(L)
             MOVE     "R"   TO     EDIT-OPTION OF SURYO(L)
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
*修正時のゼロ入力は行削除扱い
*  但し、１行目は必須入力・入庫の場合はゼロ入力不可
         IF  ( T            =  1     AND
               SURYO (L)    =  ZERO )     OR
             ( SURYO (L)    =  ZERO  AND
               NYSKBN(L)    =  "1"  )
                 IF   ERR-FLG   =    ZERO
                      MOVE      9    TO   ERR-FLG
                 END-IF
                 MOVE     "C"   TO   EDIT-CURSOR OF SURYO(L)
                 MOVE     "R"   TO   EDIT-OPTION OF SURYO(L)
         END-IF
     END-IF.
*１：１の場合、出庫数＝入庫数でなければいけない
     IF  WK-KBM-F04    =  "1"    AND
         WK-KBM-F05    =  "1"    AND
         SURYO(1)  NUMERIC       AND
         SURYO(2)  NUMERIC       AND
         SURYO(1)  NOT =  SURYO(2) AND
         SGKBN     NOT =  "H2"
         IF   ERR-FLG   =    ZERO
              MOVE     28    TO   ERR-FLG
         END-IF
         MOVE     "C"   TO   EDIT-CURSOR OF SURYO(L)
         MOVE     "R"   TO   EDIT-OPTION OF SURYO(L)
     END-IF.
*１：１の場合で、_移動の場合、入／出庫の商品ＣＤは同一
     IF  WK-KBM-F04    =  "1"       AND
         WK-KBM-F05    =  "1"       AND
         SGKBN         =  "55"      AND
         L             =   2        AND
       ( HINCD(1)      NOT =  HINCD(2)  OR
         TAN101(1)     NOT =  TAN101(2) OR
         TAN201(1)     NOT =  TAN201(2) OR
         TAN301(1)     NOT =  TAN301(2) )
         IF   ERR-FLG   =    ZERO
              MOVE     31    TO   ERR-FLG
         END-IF
         MOVE     "C"   TO   EDIT-CURSOR OF HINCD(1)
         MOVE     "R"   TO   EDIT-OPTION OF HINCD(1)
         MOVE     "R"   TO   EDIT-OPTION OF TAN101(1)
         MOVE     "R"   TO   EDIT-OPTION OF TAN201(1)
         MOVE     "R"   TO   EDIT-OPTION OF TAN301(1)
         MOVE     "R"   TO   EDIT-OPTION OF HINCD(2)
         MOVE     "R"   TO   EDIT-OPTION OF TAN101(2)
         MOVE     "R"   TO   EDIT-OPTION OF TAN201(2)
         MOVE     "R"   TO   EDIT-OPTION OF TAN301(2)
     END-IF.
*////2000.11.24 START /////
*作業区分G1（セット組）、G5（バラシ）の時
*商品・品単の重複チェック追加（入庫と出庫）
     IF (SGKBN     =   "G1"  OR  "G5" ) AND
        ( HINCD(L)  NOT=      SPACE    )
         IF   PAGE-CNT  =    1
              IF   L    >=   2
                   IF   HINCD (1)      =   HINCD (L)  AND
                        TAN101(1)      =   TAN101(L)  AND
                        TAN201(1)      =   TAN201(L)  AND
                        TAN301(1)      =   TAN301(L)
                        IF   ERR-FLG   =   ZERO
                             IF   SGKBN    =    "G1"
                                  MOVE     29    TO   ERR-FLG
                             ELSE
                                  MOVE     30    TO   ERR-FLG
                             END-IF
                        END-IF
                        MOVE  "C" TO   EDIT-CURSOR OF HINCD (L)
                        MOVE  "R" TO   EDIT-OPTION OF HINCD (L)
                        MOVE  "R" TO   EDIT-OPTION OF TAN101(L)
                        MOVE  "R" TO   EDIT-OPTION OF TAN201(L)
                        MOVE  "R" TO   EDIT-OPTION OF TAN301(L)
                   END-IF
              END-IF
         ELSE
              IF   TBL-N-HINCD (1)    =    HINCD (L)  AND
                   TBL-N-TAN101(1)    =    TAN101(L)  AND
                   TBL-N-TAN201(1)    =    TAN201(L)  AND
                   TBL-N-TAN301(1)    =    TAN301(L)
                   IF   ERR-FLG   =   ZERO
                        IF   SGKBN    =    "G1"
                             MOVE     29    TO   ERR-FLG
                        ELSE
                             MOVE     30    TO   ERR-FLG
                        END-IF
                   END-IF
                   MOVE  "C" TO   EDIT-CURSOR OF HINCD (L)
                   MOVE  "R" TO   EDIT-OPTION OF HINCD (L)
                   MOVE  "R" TO   EDIT-OPTION OF TAN101(L)
                   MOVE  "R" TO   EDIT-OPTION OF TAN201(L)
                   MOVE  "R" TO   EDIT-OPTION OF TAN301(L)
              END-IF
         END-IF
     END-IF.
*//// 2000.11.24 END ////
*未出庫数・未引当数の表示
     IF  MISYUK  NUMERIC    AND    MISYUK = 1
         PERFORM   CHK-MISYK-RTN
     END-IF.
*
 CHK-BODY-01.
     IF      ERR-FLG   NOT =  ZERO
             GO        TO       CHK-BODY-EXIT
     END-IF.
     IF      SURYO(L)      =  ZERO
             GO        TO       CHK-BODY-EXIT
     END-IF.
*テーブルセット
     MOVE    GYO101(L)      TO  TBL-D-GYO101(T).
     MOVE    TNA101(L)      TO  TBL-N-TNA101(T).
     MOVE    TNA201(L)      TO  TBL-N-TNA201(T).
     MOVE    TNA301(L)      TO  TBL-N-TNA301(T).
     MOVE    HINCD (L)      TO  TBL-N-HINCD (T).
     MOVE    TAN101(L)      TO  TBL-N-TAN101(T).
     MOVE    TAN201(L)      TO  TBL-N-TAN201(T).
     MOVE    TAN301(L)      TO  TBL-N-TAN301(T).
     MOVE    SURYO (L)      TO  TBL-N-SURYO (T).
     MOVE    STK101(L)      TO  TBL-N-STK101(T).
     MOVE    STK201(L)      TO  TBL-N-STK201(T).
     MOVE    BIKOU (L)      TO  TBL-N-BIKOU (T).
     MOVE    NYSKBN(L)      TO  TBL-N-NYSKBN(T).
     MOVE    HINMEI(L)      TO  TBL-D-HINMEI(T).
     MOVE    MISYK (L)      TO  TBL-D-MISYK (T).
     MOVE    MIHIK (L)      TO  TBL-D-MIHIK (T).
 CHK-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ（未出庫数・未引当数の表示）　　　　*
*--------------------------------------------------------------*
 CHK-MISYK-RTN         SECTION.
*_移動（出１：入１）で未出庫区分がＯＮの場合だけ当処理を行う！
*在庫マスタより未出庫数・未引当数を取得
     MOVE    BASYO          TO  ZAI-F01.
     MOVE    HINCD (L)      TO  ZAI-F021.
     MOVE    TAN101(L)      TO  ZAI-F022(1:5).
     MOVE    TAN201(L)      TO  ZAI-F022(6:2).
     MOVE    TAN301(L)      TO  ZAI-F022(8:1).
     MOVE    TNA101(L)      TO  ZAI-F03(1:1).
     MOVE    TNA201(L)      TO  ZAI-F03(2:3).
     MOVE    TNA301(L)      TO  ZAI-F03(5:2)
     PERFORM ZAI-READ-RTN.
     IF      INV-FLG    =     1
             MOVE     0           TO   MISYK(L)
                                       MIHIK(L)
     ELSE
             MOVE     ZAI-F27     TO   MISYK(L)
             MOVE     ZAI-F28     TO   MIHIK(L)
             REWRITE  ZAI-REC
     END-IF.
*修正時、変更前情報を戻す
     IF      SRKBN      =     2
             IF   TBL-O-NYSKBN(L)  =  NYSKBN(L)  AND
                  TBL-O-HINCD (L)  =  HINCD (L)  AND
                  TBL-O-TAN101(L)  =  TAN101(L)  AND
                  TBL-O-TAN201(L)  =  TAN201(L)  AND
                  TBL-O-TAN301(L)  =  TAN301(L)  AND
                  TBL-O-TNA101(L)  =  TNA101(L)  AND
                  TBL-O-TNA201(L)  =  TNA201(L)  AND
                  TBL-O-TNA301(L)  =  TNA301(L)
                  IF    TBL-O-NYSKBN(L)  =  1
                      IF    TBL-O-MISYK(L) > MISYK(L)
                          MOVE     0              TO   MISYK(L)
                      ELSE
                          SUBTRACT TBL-O-MISYK(L) FROM MISYK(L)
                      END-IF
                      IF    TBL-O-MIHIK(L) > MIHIK(L)
                          MOVE     0              TO   MIHIK(L)
                      ELSE
                          SUBTRACT TBL-O-MIHIK(L) FROM MIHIK(L)
                      END-IF
                  ELSE
                       ADD      TBL-O-MISYK(L)    TO   MISYK(L)
                       ADD      TBL-O-MIHIK(L)    TO   MIHIK(L)
                  END-IF
             END-IF
     END-IF.
 CHK-MISYK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ１　　チェック　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-BODY1-RTN         SECTION.
*入庫・出庫は各１明細以上ないといけない
     MOVE     SPACE         TO  NYU-FLG   SYU-FLG.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  24
           EVALUATE    TBL-N-NYSKBN (I)
              WHEN  "1"     MOVE  "1"     TO  NYU-FLG
              WHEN  "2"     MOVE  "1"     TO  SYU-FLG
           END-EVALUATE
     END-PERFORM.
     IF  NYU-FLG = SPACE  AND  SYU-FLG = SPACE
          MOVE     13        TO   ERR-FLG
          MOVE     "C"       TO   EDIT-CURSOR OF HINCD (1)
          MOVE     "R"       TO   EDIT-OPTION OF HINCD (1)
          MOVE     "R"       TO   EDIT-OPTION OF TAN101(1)
          MOVE     "R"       TO   EDIT-OPTION OF TAN201(1)
          MOVE     "R"       TO   EDIT-OPTION OF TAN301(1)
          GO    TO                CHK-BODY1-EXIT
     END-IF.
     IF  NYU-FLG = SPACE   OR  SYU-FLG = SPACE
         IF  SRKBN   =  1
              IF   NYSKBN(1)    =      "1"
                   MOVE     21    TO   ERR-FLG
              ELSE
                   MOVE     22    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF NYSKBN(2)
              MOVE     "R"   TO   EDIT-OPTION OF NYSKBN(2)
         ELSE
              MOVE     9     TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR OF SURYO (2)
              MOVE     "R"   TO   EDIT-OPTION OF SURYO (2)
         END-IF
         GO     TO                CHK-BODY1-EXIT
     END-IF.
*
*明細行詰めチェック
     MOVE     0        TO    J.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  24
       IF     TBL-NEW(I)  NOT =   SPACE
              ADD    1               TO   J
       END-IF
       IF     TBL-NEW(I)  NOT =   SPACE   AND
              I           NOT =   J
              MOVE   TBL-NEW (I)     TO   TBL-NEW (J)
              MOVE   SPACE           TO   TBL-NEW (I)
              MOVE   TBL-DSP (I)     TO   TBL-DSP (J)
              INITIALIZE                  TBL-DSP (I)
       END-IF
     END-PERFORM.
*
     MOVE     J       TO     MAX-LINE.
     PERFORM  SGY-DSP-SET-RTN.
     PERFORM  CLR-BODY-RTN.
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
*    LEBEL  3       チェックＯＫ、ワーニング表示               *
*--------------------------------------------------------------*
 CHK-BODY9-RTN          SECTION.
*
     PERFORM  VARYING  L  FROM  1  BY  1
                                UNTIL (   L      >  6    ) OR
                                      (HINCD (L) =  SPACE  AND
                                       TAN101(L) =  SPACE  AND
                                       TAN201(L) =  SPACE  AND
                                       TAN301(L) =  SPACE)
*在庫マスタ存在チェック
             PERFORM   CHK-MSG1-RTN
*未出庫、数量チェック
             IF     MISYUK NUMERIC  AND  MISYUK = 1
                    PERFORM   CHK-MSG2-RTN
             END-IF
     END-PERFORM.
 CHK-BODY9-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  3       在庫マスタチェック                         *
*--------------------------------------------------------------*
 CHK-MSG1-RTN           SECTION.
     MOVE    BASYO          TO     ZAI-F01.
     MOVE    HINCD (L)      TO     ZAI-F021.
     MOVE    TAN101(L)      TO     ZAI-F022(1:5).
     MOVE    TAN201(L)      TO     ZAI-F022(6:2).
     MOVE    TAN301(L)      TO     ZAI-F022(8:1).
     MOVE    TNA101(L)      TO     ZAI-F03(1:1).
     MOVE    TNA201(L)      TO     ZAI-F03(2:3).
     MOVE    TNA301(L)      TO     ZAI-F03(5:2).
     PERFORM ZAI-READ-RTN.
     IF      INV-FLG    =     1
             IF     ERR-FLG   =   ZERO
                    MOVE     23   TO     ERR-FLG
             END-IF
             MOVE    "R"    TO     EDIT-OPTION OF HINCD (L)
             MOVE    "R"    TO     EDIT-OPTION OF TAN101(L)
             MOVE    "R"    TO     EDIT-OPTION OF TAN201(L)
             MOVE    "R"    TO     EDIT-OPTION OF TAN301(L)
             MOVE    "R"    TO     EDIT-OPTION OF TNA101(L)
             MOVE    "R"    TO     EDIT-OPTION OF TNA201(L)
             MOVE    "R"    TO     EDIT-OPTION OF TNA301(L)
     ELSE
             REWRITE  ZAI-REC
     END-IF.
 CHK-MSG1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  3       未出庫、数量チェック                       *
*--------------------------------------------------------------*
 CHK-MSG2-RTN           SECTION.
*出庫の場合、数量が全部移動可能かチェック
     IF    ( NYSKBN (L) =  2 )       AND
           ( SURYO(L)   >  MISYK(L)   OR
             SURYO(L)   >  MIHIK(L) )
             IF     ERR-FLG   =   ZERO
                    MOVE     27   TO     ERR-FLG
             END-IF
             MOVE    "R"    TO     EDIT-OPTION OF SURYO (L)
             GO      TO            CHK-MSG2-EXIT
     END-IF.
*修正時、数量が全部戻せるか（入庫の戻し）チェック
     IF      SRKBN           =  2    AND
             TBL-O-NYSKBN(L) =  1
             CONTINUE
     ELSE
             GO      TO            CHK-MSG2-EXIT
     END-IF.
     MOVE    BASYO            TO    ZAI-F01.
     MOVE    TBL-O-HINCD(L)   TO    ZAI-F021.
     MOVE    TBL-O-TAN  (L)   TO    ZAI-F022.
     MOVE    TBL-O-TNA  (L)   TO    ZAI-F03.
     PERFORM ZAI-READ-RTN.
     IF      INV-FLG  =  0
             REWRITE  ZAI-REC
     ELSE
             MOVE     0       TO    ZAI-F27   ZAI-F28
     END-IF.
     IF      TBL-N-NYSKBN(L)  =  TBL-O-NYSKBN(L)  AND
             TBL-N-HINCD (L)  =  TBL-O-HINCD (L)  AND
             TBL-N-TAN   (L)  =  TBL-O-TAN   (L)  AND
             TBL-N-TNA   (L)  =  TBL-O-TNA   (L)
             IF  ( TBL-O-MISYK(L) > ZAI-F27 + TBL-O-MISYK(L) ) OR
                 ( TBL-O-MIHIK(L) > ZAI-F28 + TBL-O-MIHIK(L) )
                 IF     ERR-FLG   =   ZERO
                        MOVE     27   TO     ERR-FLG
                 END-IF
                 MOVE   "R"    TO     EDIT-OPTION OF SURYO (L)
             END-IF
     ELSE
             IF  ( TBL-O-MISYK(L) > ZAI-F27 ) OR
                 ( TBL-O-MIHIK(L) > ZAI-F28 )
                 IF     ERR-FLG   =   ZERO
                        MOVE     27   TO     ERR-FLG
                 END-IF
                 MOVE   "R"    TO     EDIT-OPTION OF SURYO (L)
             END-IF
     END-IF.
 CHK-MSG2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　入力　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-KAKNIN-RTN      SECTION.
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
                   PERFORM   CLR-BODY-RTN
                   IF  SRKBN  =   1    OR  2
                       MOVE   4   TO   SHORI-F
                   END-IF
              END-IF
     WHEN     "F008"
              IF   SRKBN      =   1
                IF HINCD(6)            =  SPACE    OR
                   TBL-N-HINCD(24) NOT =  SPACE
                   MOVE      20    TO     ERR-FLG
                ELSE
                   ADD       1     TO     PAGE-CNT
                   COMPUTE   T  = ( PAGE-CNT * 6 )  -  6  +  1
                   IF  TBL-N-HINCD (T) =  SPACE  AND
                       TBL-N-TAN101(T) =  SPACE  AND
                       TBL-N-TAN201(T) =  SPACE  AND
                       TBL-N-TAN301(T) =  SPACE
                       MOVE     SRKBN      TO     SAV-SRKBN
                       MOVE     SRMEI      TO     SAV-SRMEI
                       MOVE     HEAD1      TO     SAV-HEAD1
                       MOVE     SPACE      TO     FZA00601
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
                   PERFORM   CLR-BODY-RTN
                   MOVE      4         TO  SHORI-F
                END-IF
              ELSE
                IF PAGE-CNT  =  4  OR
                   HINCD(6)  =  SPACE
                   MOVE      15        TO  ERR-FLG
                ELSE
                   ADD       1         TO  PAGE-CNT
                   PERFORM   SGY-DSP-SET-RTN
                   PERFORM   CLR-BODY-RTN
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
     MOVE     "3"            TO   LINK-IN-KBN.
     MOVE     SGYMD          TO   LINK-IN-YMD6.
     MOVE     ZERO           TO   LINK-IN-YMD8.
     MOVE     ZERO           TO   LINK-OUT-RET.
     MOVE     ZERO           TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"  USING   LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD   TO   WK-SGYDATE.
*在庫マスタ更新
     IF       SRKBN     =   2   OR  3
              PERFORM   ZAI-OLD-RTN
     END-IF.
     IF       SRKBN     =   1   OR  2
              PERFORM   ZAI-NEW-RTN
     END-IF.
*作業実績ファイル更新
     EVALUATE    SRKBN
         WHEN    1
              PERFORM   SGY-WRITE-RTN
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
              MOVE      SPACE          TO   FZA00601   TABLE-AREA
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
              MOVE      SPACE          TO   FZA00601   TABLE-AREA
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
              MOVE      SPACE          TO   FZA00601   TABLE-AREA
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
************* 登録
              PERFORM  SGY-SET-RTN
              WRITE    SGY-REC
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
     START    SGYFILF KEY    >=   SGY-F01  SGY-F02
        INVALID  KEY
              MOVE      1    TO   SGY-FLG
        NOT   INVALID  KEY
              PERFORM  SGY-READ-RTN
     END-START.
     MOVE     1              TO   I.
     PERFORM              UNTIL   I   >  24
              IF       SGY-FLG     =   0
                IF     TBL-NEW(I)  =   SPACE   OR
                       TBL-D-GYO101(I) NOT =  SGY-F02
************* 行削除（物理削除）
                       DELETE   SGYFILF    RECORD
                ELSE
************* 修正
                       PERFORM  SGY-SET-RTN
                       REWRITE  SGY-REC
                       ADD      1          TO   I
                END-IF
                PERFORM  SGY-READ-RTN
              ELSE
                IF     TBL-NEW(I)  NOT =  SPACE
************* 行追加
                       PERFORM  SGY-SET-RTN
                       WRITE    SGY-REC
                END-IF
                       ADD      1          TO   I
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
     START    SGYFILF KEY    >=   SGY-F01  SGY-F02
        INVALID  KEY
              MOVE     1     TO   SGY-FLG
        NOT   INVALID  KEY
              PERFORM  SGY-READ-RTN
     END-START.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  SGY-FLG = 1
************* 削除（論理削除）
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
     IF       SRKBN          =    1    OR
              SGY-FLG        =    1
              MOVE   SPACE            TO  SGY-REC
              INITIALIZE                  SGY-REC
              MOVE   SGYNO           TO   SGY-F01
              MOVE   TBL-D-GYO101(I) TO   SGY-F02
              MOVE   SYS-DATE2       TO   SGY-F98
     ELSE
              MOVE   SYS-DATE2       TO   SGY-F99
     END-IF.
*
     MOVE     TANTO             TO   SGY-F14.
     MOVE     SGKBN             TO   SGY-F03.
     MOVE     BASYO             TO   SGY-F04.
     MOVE     WK-SGYDATE        TO   SGY-F05.
     MOVE     TBL-N-NYSKBN(I)   TO   SGY-F06.
     MOVE     TBL-N-STK   (I)   TO   SGY-F07.
     MOVE     TBL-N-HINCD (I)   TO   SGY-F08.
     MOVE     TBL-N-TAN   (I)   TO   SGY-F09.
     MOVE     TBL-N-TNA   (I)   TO   SGY-F10.
     MOVE     TBL-N-SURYO (I)   TO   SGY-F11.
     MOVE     TBL-N-BIKOU (I)   TO   SGY-F12.
     IF       SGKBN          =   "55"
              MOVE   MISYUK           TO   SGY-F15
              MOVE   TBL-N-MISYK(I)   TO   SGY-F16
              MOVE   TBL-N-MIHIK(I)   TO   SGY-F17
     END-IF.
     MOVE     BUMON             TO   SGY-F95.
 SGY-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     在庫マスタ更新（変更前情報）                 *
*--------------------------------------------------------------*
 ZAI-OLD-RTN             SECTION.
*出庫の戻し（プラス）
     PERFORM  VARYING I FROM 1 BY 1
                        UNTIL ( I > 24 )
                        OR    ( TBL-O-HINCD(I)  = SPACE
                          AND   TBL-O-TAN  (I)  = SPACE )
        IF    TBL-O-NYSKBN(I)  =  2
              MOVE   BASYO           TO  ZAI-F01
              MOVE   TBL-O-HINCD(I)  TO  ZAI-F021
              MOVE   TBL-O-TAN  (I)  TO  ZAI-F022
              MOVE   TBL-O-TNA  (I)  TO  ZAI-F03
              PERFORM   ZAI-READ-RTN
              IF     INV-FLG   =  0
                     MOVE    0             TO   WRITE-FLG
                     MOVE    SYS-DATE2     TO   ZAI-F99
              ELSE
                     MOVE    1             TO   WRITE-FLG
                     PERFORM   ZAI-INIT-RTN
                     MOVE    SYS-DATE2     TO   ZAI-F98
              END-IF
                   ADD      TBL-O-SURYO(I)    TO   ZAI-F04
              IF       WK-SGYDATE    >  ZAI-SIMER
                   SUBTRACT TBL-O-SURYO(I)  FROM   ZAI-F12
              ELSE
                   ADD      TBL-O-SURYO(I)    TO   ZAI-F06
                   SUBTRACT TBL-O-SURYO(I)  FROM   ZAI-F08
              END-IF
              IF ( MISYUK  NUMERIC ) AND ( MISYUK = 1 )
                   ADD      TBL-O-MISYK(I)    TO   ZAI-F27
                   ADD      TBL-O-MIHIK(I)    TO   ZAI-F28
              END-IF
              IF     WRITE-FLG =  0
                     REWRITE ZAI-REC
              ELSE
                     WRITE   ZAI-REC
              END-IF
        END-IF
     END-PERFORM.
*入庫の戻し（マイナス）
     PERFORM  VARYING I FROM 1 BY 1
                        UNTIL ( I > 24 )
                        OR    ( TBL-O-HINCD(I)  = SPACE
                          AND   TBL-O-TAN  (I)  = SPACE )
        IF    TBL-O-NYSKBN(I)  =  1
              MOVE   BASYO           TO  ZAI-F01
              MOVE   TBL-O-HINCD(I)  TO  ZAI-F021
              MOVE   TBL-O-TAN  (I)  TO  ZAI-F022
              MOVE   TBL-O-TNA  (I)  TO  ZAI-F03
              PERFORM   ZAI-READ-RTN
              IF     INV-FLG   =  0
                     MOVE    0             TO   WRITE-FLG
                     MOVE    SYS-DATE2     TO   ZAI-F99
              ELSE
                     MOVE    1             TO   WRITE-FLG
                     PERFORM   ZAI-INIT-RTN
                     MOVE    SYS-DATE2     TO   ZAI-F98
              END-IF
                   SUBTRACT TBL-O-SURYO(I)  FROM   ZAI-F04
              IF       WK-SGYDATE    >  ZAI-SIMER
                   SUBTRACT TBL-O-SURYO(I)  FROM   ZAI-F11
              ELSE
                   SUBTRACT TBL-O-SURYO(I)  FROM   ZAI-F06
                   SUBTRACT TBL-O-SURYO(I)  FROM   ZAI-F07
              END-IF
              IF ( MISYUK  NUMERIC ) AND ( MISYUK = 1 )
                IF   TBL-O-MISYK(I)  >  ZAI-F27
                   MOVE     0                 TO   ZAI-F27
                ELSE
                   SUBTRACT TBL-O-MISYK(I)  FROM   ZAI-F27
                END-IF
                IF   TBL-O-MIHIK(I)  >  ZAI-F28
                   MOVE     0                 TO   ZAI-F28
                ELSE
                   SUBTRACT TBL-O-MIHIK(I)  FROM   ZAI-F28
                END-IF
              END-IF
              IF     WRITE-FLG =  0
                     REWRITE ZAI-REC
              ELSE
                     WRITE   ZAI-REC
              END-IF
        END-IF
     END-PERFORM.
 ZAI-OLD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     在庫マスタ更新（変更後情報）                 *
*--------------------------------------------------------------*
 ZAI-NEW-RTN             SECTION.
*出庫（マイナス）
     PERFORM  VARYING I FROM 1 BY 1
                        UNTIL ( I > 24 )
                        OR    ( TBL-N-HINCD(I)  = SPACE
                          AND   TBL-N-TAN  (I)  = SPACE )
        IF    TBL-N-NYSKBN(I)  =  2
              MOVE   BASYO           TO  ZAI-F01
              MOVE   TBL-N-HINCD(I)  TO  ZAI-F021
              MOVE   TBL-N-TAN  (I)  TO  ZAI-F022
              MOVE   TBL-N-TNA  (I)  TO  ZAI-F03
              PERFORM   ZAI-READ-RTN
              IF     INV-FLG   =  0
                     MOVE    0             TO   WRITE-FLG
                     MOVE    SYS-DATE2     TO   ZAI-F99
              ELSE
                     MOVE    2             TO   WRITE-FLG
                     PERFORM   ZAI-INIT-RTN
                     MOVE    SYS-DATE2     TO   ZAI-F98
              END-IF
                   SUBTRACT TBL-N-SURYO(I)  FROM   ZAI-F04
              IF       WK-SGYDATE    >  ZAI-SIMER
                   ADD      TBL-N-SURYO(I)    TO   ZAI-F12
              ELSE
                   SUBTRACT TBL-N-SURYO(I)  FROM   ZAI-F06
                   ADD      TBL-N-SURYO(I)    TO   ZAI-F08
              END-IF
              IF ( MISYUK  NUMERIC ) AND ( MISYUK = 1 )
                IF   TBL-N-SURYO(I)  >  ZAI-F27
                   MOVE     ZAI-F27           TO   TBL-N-MISYK(I)
                   MOVE     0                 TO   ZAI-F27
                ELSE
                   SUBTRACT TBL-N-SURYO(I)  FROM   ZAI-F27
                   MOVE     TBL-N-SURYO(I)    TO   TBL-N-MISYK(I)
                END-IF
                IF   TBL-N-SURYO(I)  >  ZAI-F28
                   MOVE     ZAI-F28           TO   TBL-N-MIHIK(I)
                   MOVE     0                 TO   ZAI-F28
                ELSE
                   SUBTRACT TBL-N-SURYO(I)  FROM   ZAI-F28
                   MOVE     TBL-N-SURYO(I)    TO   TBL-N-MIHIK(I)
                END-IF
              END-IF
              IF     WRITE-FLG =  0
                     REWRITE ZAI-REC
              ELSE
                     WRITE   ZAI-REC
              END-IF
        END-IF
     END-PERFORM.
*未出庫、出庫可能数を入庫数とする
     IF   MISYUK  NUMERIC   AND   MISYUK = 1
          IF   TBL-N-NYSKBN (1)  =  2
               MOVE   TBL-N-MISYK(1)    TO   TBL-N-MISYK(2)
               MOVE   TBL-N-MIHIK(1)    TO   TBL-N-MIHIK(2)
          ELSE
               MOVE   TBL-N-MISYK(2)    TO   TBL-N-MISYK(1)
               MOVE   TBL-N-MIHIK(2)    TO   TBL-N-MIHIK(1)
          END-IF
     ELSE
               MOVE   0                 TO   TBL-N-MISYK(1)
                                             TBL-N-MISYK(2)
                                             TBL-N-MIHIK(1)
                                             TBL-N-MIHIK(2)
     END-IF.
*入庫（プラス）
     PERFORM  VARYING I FROM 1 BY 1
                        UNTIL ( I > 24 )
                        OR    ( TBL-N-HINCD(I)  = SPACE
                          AND   TBL-N-TAN  (I)  = SPACE )
        IF    TBL-N-NYSKBN(I)  =  1
              MOVE   BASYO           TO  ZAI-F01
              MOVE   TBL-N-HINCD(I)  TO  ZAI-F021
              MOVE   TBL-N-TAN  (I)  TO  ZAI-F022
              MOVE   TBL-N-TNA  (I)  TO  ZAI-F03
              PERFORM   ZAI-READ-RTN
              IF     INV-FLG   =  0
                     MOVE    0             TO   WRITE-FLG
                     MOVE    SYS-DATE2     TO   ZAI-F99
              ELSE
                     MOVE    2             TO   WRITE-FLG
                     PERFORM   ZAI-INIT-RTN
                     MOVE    SYS-DATE2     TO   ZAI-F98
              END-IF
                   ADD      TBL-N-SURYO(I)    TO   ZAI-F04
              IF       WK-SGYDATE    >  ZAI-SIMER
                   ADD      TBL-N-SURYO(I)    TO   ZAI-F11
              ELSE
                   ADD      TBL-N-SURYO(I)    TO   ZAI-F06
                   ADD      TBL-N-SURYO(I)    TO   ZAI-F07
              END-IF
              IF ( MISYUK  NUMERIC ) AND ( MISYUK = 1 )
                   ADD      TBL-N-MISYK(I)    TO   ZAI-F27
                   ADD      TBL-N-MIHIK(I)    TO   ZAI-F28
              END-IF
              IF     WRITE-FLG =  0
                     REWRITE ZAI-REC
              ELSE
                     WRITE   ZAI-REC
              END-IF
        END-IF
     END-PERFORM.
 ZAI-NEW-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL     在庫マスタ（初期セット）
*--------------------------------------------------------------*
 ZAI-INIT-RTN        SECTION.
     MOVE     SPACE                   TO  ZAI-REC
     INITIALIZE                           ZAI-REC
     MOVE     BASYO                   TO  ZAI-F01.
     IF       WRITE-FLG  =   1
              MOVE   TBL-O-HINCD(I)   TO  ZAI-F021
              MOVE   TBL-O-TAN  (I)   TO  ZAI-F022
              MOVE   TBL-O-TNA  (I)   TO  ZAI-F03
     ELSE
              MOVE   TBL-N-HINCD(I)   TO  ZAI-F021
              MOVE   TBL-N-TAN  (I)   TO  ZAI-F022
              MOVE   TBL-N-TNA  (I)   TO  ZAI-F03
     END-IF.
*---<  商品名称（カナ）取得  >---*
     MOVE     ZAI-F021            TO      MEI-F011.
     MOVE     ZAI-F022            TO      MEI-F012.
     PERFORM  MEI-READ-RTN.
     MOVE     MEI-F031            TO      ZAI-F30.
 ZAI-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     明細　表示                                   *
*--------------------------------------------------------------*
 SGY-DSP-SET-RTN         SECTION.
     MOVE     SPACE                 TO  BODY1     KAKNIN.
*
     IF    SRKBN    =        1
           PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
              COMPUTE  T  =   ( PAGE-CNT * 6 )  -  6  +  I
              MOVE     T            TO   GYO101(I)
           END-PERFORM
     END-IF.
*
     COMPUTE  IX  =  ( PAGE-CNT  *  6 )   -  6.
     PERFORM  VARYING  S  FROM  1  BY  1   UNTIL   S  > 6
              ADD      1        TO     IX
              IF  ( SRKBN    NOT =   1 )      AND
                  ( TBL-D-GYO101(IX) NUMERIC  AND
                    TBL-D-GYO101(IX) NOT =  ZERO )
                    MOVE TBL-D-GYO101(IX)  TO  GYO101(S)
              END-IF
              IF    TBL-NEW(IX)  NOT =  SPACE
                    MOVE TBL-N-TNA101(IX)  TO  TNA101(S)
                    MOVE TBL-N-TNA201(IX)  TO  TNA201(S)
                    MOVE TBL-N-TNA301(IX)  TO  TNA301(S)
                    MOVE TBL-N-HINCD (IX)  TO  HINCD (S)
                    MOVE TBL-N-TAN101(IX)  TO  TAN101(S)
                    MOVE TBL-N-TAN201(IX)  TO  TAN201(S)
                    MOVE TBL-N-TAN301(IX)  TO  TAN301(S)
                    MOVE TBL-N-SURYO (IX)  TO  SURYO (S)
                    MOVE TBL-N-STK101(IX)  TO  STK101(S)
                    MOVE TBL-N-STK201(IX)  TO  STK201(S)
                    MOVE TBL-N-BIKOU (IX)  TO  BIKOU (S)
                    MOVE TBL-N-NYSKBN(IX)  TO  NYSKBN(S)
                    MOVE TBL-D-HINMEI(IX)  TO  HINMEI(S)
                IF  ( MISYUK NUMERIC    AND    MISYUK = 1 )
                    MOVE TBL-D-MISYK (IX)  TO  MISYK (S)
                    MOVE TBL-D-MIHIK (IX)  TO  MIHIK (S)
                END-IF
              END-IF
     END-PERFORM.
 SGY-DSP-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ヘッド部読込　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 HEAD-SET-RTN         SECTION.
     MOVE     SGY-F01             TO   SGYNO.
     MOVE     SGY-F14             TO   TANTO.
     MOVE     SGY-F05             TO   SGYMD.
     MOVE     SGY-F03             TO   SGKBN.
     MOVE     SGY-F03             TO   KBM-F01.
     PERFORM  KBM-READ-RTN.
     IF       INV-FLG   =   0
              MOVE      KBM-F02   TO   SGMEI
              MOVE      KBM-F04   TO   WK-KBM-F04
                                       OUTKB
              MOVE      KBM-F05   TO   WK-KBM-F05
                                       INKB
     ELSE
              MOVE      SPACE     TO   SGMEI   OUTKB  INKB
     END-IF.
     MOVE     SGY-F04             TO   BASYO   SOK-F01.
     PERFORM  SOK-READ-RTN.
     IF       INV-FLG   =   0
              MOVE      SOK-F02   TO   BASYOM
     ELSE
              MOVE      SPACE     TO   BASYOM
     END-IF.
     IF       SGY-F03   =   "55"
              MOVE      SGY-F15   TO   MISYUK
     ELSE
              MOVE      SPACE     TO   MISYUX
     END-IF.
     MOVE     SGY-F95             TO   BUMON.
     MOVE    "22"                 TO   JYO-F01
     MOVE    BUMON                TO   JYO-F02
     PERFORM JYO-READ-RTN
     IF   INV-FLG  =  1
             MOVE   ALL NC"＊"    TO   BUMONN
     ELSE
             MOVE     JYO-F03     TO   BUMONN
     END-IF.
  HEAD-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     明細テーブルセット　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 SGY-TBL-SET-RTN         SECTION.
     MOVE     SGY-F02        TO   TBL-D-GYO101(I).
     MOVE     SGY-F10        TO   TBL-N-TNA   (I).
     MOVE     SGY-F08        TO   TBL-N-HINCD (I).
     MOVE     SGY-F09        TO   TBL-N-TAN   (I).
     MOVE     SGY-F11        TO   TBL-N-SURYO (I).
     MOVE     SGY-F07        TO   TBL-N-STK   (I).
     MOVE     SGY-F12        TO   TBL-N-BIKOU (I).
     MOVE     SGY-F06        TO   TBL-N-NYSKBN(I).
*商品名
     MOVE     SGY-F08        TO   MEI-F011.
     MOVE     SGY-F09        TO   MEI-F012.
     PERFORM  MEI-READ-RTN.
     IF       INV-FLG   =    0
              MOVE      MEI-F021  TO   TBL-D-HINMEI(I)(1:15)
              MOVE      MEI-F022  TO   TBL-D-HINMEI(I)(16:15)
     ELSE
              MOVE      SPACE     TO   TBL-D-HINMEI(I)
     END-IF.
*未出庫数・未引当数
     IF       SGY-F15    =   1
              MOVE     SGY-F04        TO   ZAI-F01
              MOVE     SGY-F08        TO   ZAI-F021
              MOVE     SGY-F09        TO   ZAI-F022
              MOVE     SGY-F10        TO   ZAI-F03
              PERFORM  ZAI-READ-RTN
              IF       INV-FLG   =    0
                       MOVE  ZAI-F27      TO   TBL-D-MISYK(I)
                       MOVE  ZAI-F28      TO   TBL-D-MIHIK(I)
              ELSE
                       MOVE  ZERO         TO   TBL-D-MISYK(I)
                                               TBL-D-MIHIK(I)
              END-IF
              IF       SGY-F06   =    1
                   IF    SGY-F16 > TBL-D-MISYK (I)
                       MOVE     0         TO   TBL-D-MISYK(I)
                   ELSE
                       SUBTRACT SGY-F16   FROM TBL-D-MISYK(I)
                   END-IF
                   IF    SGY-F17 > TBL-D-MIHIK (I)
                       MOVE     0         TO   TBL-D-MIHIK(I)
                   ELSE
                       SUBTRACT SGY-F17   FROM TBL-D-MIHIK(I)
                   END-IF
              ELSE
                       ADD      SGY-F16   TO   TBL-D-MISYK(I)
                       ADD      SGY-F17   TO   TBL-D-MIHIK(I)
              END-IF
     END-IF.
*変更前情報の退避
     MOVE     TBL-NEW (I)    TO   TBL-OLD    (I).
     MOVE     SGY-F16        TO   TBL-O-MISYK(I).
     MOVE     SGY-F17        TO   TBL-O-MIHIK(I).
*最大行番号の退避
     MOVE     SGY-F02        TO   MAX-GYO.
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
     MOVE     HEN-DATE     TO   SDATE.
     MOVE     HEN-TIME     TO   STIME.
     MOVE     HEN-TOKHAN-AREA     TO   TOKHAN.
*
     MOVE     SPACE        TO   DSP-PRO.
     WRITE    FZA00601.
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
     REWRITE  JYO-REC.
*
*作業完成日システム日付設定
     IF       SAV-YMD       =          ZERO
         MOVE     SYS-YY2-2 TO   SGYMD(1:2)
         MOVE     SYS-MM2   TO   SGYMD(3:2)
         MOVE     SYS-DD2   TO   SGYMD(5:2)
     END-IF.
*場所自動設定
     IF       WK-BASYO1      =    "01"     OR
              WK-BASYO2      =    "01"
              CONTINUE
     ELSE
              MOVE  WK-BASYO1     TO   BASYO
              MOVE  "X"           TO   EDIT-STATUS OF BASYO
              IF   WK-BASYO1 NOT = "6A"
                   MOVE  "X"  TO   EDIT-STATUS OF BUMON
              END-IF
              MOVE  WK-BASYO1     TO   SOK-F01
              PERFORM   SOK-READ-RTN
              IF        INV-FLG   =    0
                        MOVE  SOK-F02  TO   BASYOM
              ELSE
                        MOVE  SPACE    TO   BASYOM
              END-IF
**************計上部門ＣＤ編集
               MOVE     SPACE               TO   JYO-REC
               INITIALIZE                        JYO-REC
               MOVE    "20"                 TO   JYO-F01
               MOVE    WK-BASYO1            TO   JYO-F02
               PERFORM JYO-READ-RTN
               IF   INV-FLG  =  1
                    IF   ERR-FLG   =    ZERO
                         MOVE      32   TO   ERR-FLG
                    END-IF
                    MOVE    "C"         TO   EDIT-CURSOR OF BUMON
                    MOVE    "R"         TO   EDIT-OPTION OF BUMON
               ELSE
                    MOVE     JYO-F05    TO   BUMON
               END-IF
               MOVE    "22"                 TO   JYO-F01
               MOVE    BUMON                TO   JYO-F02
               PERFORM JYO-READ-RTN
               IF   INV-FLG  =  1
                    IF   ERR-FLG   =    ZERO
                         MOVE      33   TO   ERR-FLG
                    END-IF
                    MOVE    "C"         TO   EDIT-CURSOR OF BUMON
                    MOVE    "R"         TO   EDIT-OPTION OF BUMON
               ELSE
                    MOVE     JYO-F03    TO   BUMONN
               END-IF
     END-IF.
*行_自動設定
     PERFORM  VARYING  I  FROM  1  BY  1    UNTIL  I  >  6
              MOVE     I               TO   GYO101(I)
     END-PERFORM.
 SGYNO-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫マスタ　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 ZAI-READ-RTN           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     ZAMZAIF    INVALID   KEY
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
*    LEVEL  ALL   作業区分マスタ　ＲＥＡＤ                     *
*--------------------------------------------------------------*
 KBM-READ-RTN           SECTION.
     MOVE     0              TO   INV-FLG.
     READ     SGYKBMF  INVALID  KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 KBM-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   作業実績マスタ　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 SGY-READ-RTN           SECTION.
     MOVE     0                  TO   SGY-FLG.
     READ     SGYFILF   NEXT     AT   END
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
     IF       WK-BASYO1 NOT =  "01"    AND
              WK-BASYO2 NOT =  "01"    AND
              WK-BASYO1 NOT =  SGY-F04
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
     READ     SGYFILF   NEXT     AT   END
              MOVE      1        TO   SGY-FLG
              GO   TO   SGY-READ1-EXIT
     END-READ.
     IF       SGY-F13       =    1    OR
              SGY-F97       =    1
              GO   TO   SGY-READ1-RTN
     END-IF.
     IF       WK-BASYO1 NOT =  "01"   AND
              WK-BASYO2 NOT =  "01"   AND
              WK-BASYO1 NOT =  SGY-F04
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
                                  EDIT-CURSOR OF TANTO
                                  EDIT-CURSOR OF SGYNO
                                  EDIT-CURSOR OF SGYMD
                                  EDIT-CURSOR OF SGKBN
                                  EDIT-CURSOR OF BASYO
                                  EDIT-CURSOR OF BUMON
                                  EDIT-CURSOR OF MISYUK.
     MOVE     "M"            TO   EDIT-OPTION OF SRKBN
                                  EDIT-OPTION OF TANTO
                                  EDIT-OPTION OF SGYNO
                                  EDIT-OPTION OF SGYMD
                                  EDIT-OPTION OF SGKBN
                                  EDIT-OPTION OF BASYO
                                  EDIT-OPTION OF BUMON
                                  EDIT-OPTION OF MISYUK.
     IF    WK-BASYO1  =     "01"      OR
           WK-BASYO2  =     "01"
           MOVE      " "     TO   EDIT-STATUS OF BASYO
           MOVE      " "     TO   EDIT-STATUS OF BUMON
     ELSE
           MOVE      "X"     TO   EDIT-STATUS OF BASYO
           IF  WK-BASYO1  NOT = "6A"
               MOVE  "X"  TO   EDIT-STATUS OF BUMON
           END-IF
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
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ　プロテクト　　　　　　　　　　　　*
*--------------------------------------------------------------*
 PRO-BODY-RTN           SECTION.
     PERFORM  VARYING  I  FROM  3  BY  1  UNTIL  I  >  6
        IF    WK-KBM-F04 = "1"  AND  WK-KBM-F05 = "1"
              MOVE  "X"       TO   EDIT-STATUS OF TNA101(I)
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
        ELSE
              MOVE  " "       TO   EDIT-STATUS OF TNA101(I)
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
        END-IF
     END-PERFORM.
 PRO-BODY-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
