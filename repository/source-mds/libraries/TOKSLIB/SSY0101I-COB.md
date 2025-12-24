# SSY0101I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY0101I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　販売管理システム　　　　　　　　　　　*
*    モジュール名　　：　手書伝票　入力　　　　　　　　　　　　*
*    作成日　　　　　：　99/09/22                              *
*    作成者　　　　　：　NAV YOSHIDA.                          *
*                                                              *
*    更新日　　　　　：　93/03/12  93/03/27  93/05/24          *
*      更新者　　　　：　NAV T.A    T.A       K.S              *
*      更新内容　　　：　バグ対応　仕様変更　追加仕様          *
*    更新日　　　　　：　93/05/14                              *
*      更新者　　　　：　 T.A                                  *
*      更新内容　　　：　仕様変更　（赤伝　修正削除プロテクト）*
*    更新日　　　　　：　93/05/24                              *
*      更新者　　　　：　  .S                                  *
*                    ：　追加仕様　（在庫マスタ更新）　　　　　*
*    更新日　　　　　：　94/05/16                              *
*      更新者　　　　：　 Y.N                                  *
*      更新内容　　　：　仕様変更　（画面に出荷日追加・在庫更新*
*    更新日　　　　　：　97/04/24                              *
*      更新者　　　　：　 K.Y                                  *
*      更新内容　　　：　仕様変更　（エンチョー手書きチェックデ*
*                                    ジットはずし自動採番は有り*
*    更新日　　　　　：　98/09/17                              *
*      更新者　　　　：　 T.T                                  *
*      更新内容　　　：　仕様追加　（伝票区分”４１，４２”の入*
*                                    力時のチェックの追加）    *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY0101I.
 AUTHOR.                NAV Y.YOSHIDA.
 DATE-WRITTEN.          99/09/22.
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
                        FILE      STATUS    TOK-ST1.
*----<< 店舗マスタ >>-*
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE      STATUS    TEN-ST1.
*----<< 商品変換テーブル >>-*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       STB-F01   STB-F02
                        FILE      STATUS    STB-ST1.
*----<< 商品名称マスタ >>-*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       MEI-F01
                        FILE      STATUS    MEI-ST1.
*----<< 条件ファイル >>-*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-ST1.
*----<< 伝票データ >>-*
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F03
                        FILE      STATUS    DEN-ST1.
***   93/05/24  START   ***
*----<< 商品変換テーブル４ >>-*
     SELECT   SHOTBL4   ASSIGN    TO        DA-01-VI-SHOTBL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       SHT-F01   SHT-F031
                                            SHT-F032
                        FILE      STATUS    SHT-ST1.
*----<< 商品在庫マスタ >>-*
     SELECT   ZZAIMS    ASSIGN    TO        DA-01-VI-ZZAIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       ZAI-F01
                                            ZAI-F02
                                            ZAI-F03
                        FILE      STATUS    ZAI-ST1.
***   93/05/24    END   ***
*----<< 画面ファイル >>-*
     SELECT   DSPFILE   ASSIGN    TO        GS-DSPF
                        SYMBOLIC  DESTINATION        "DSP"
*******************     DESTINATION-1       DSP-WS
                        FORMAT              DSP-FMT
                        GROUP               DSP-GRP
                        PROCESSING  MODE    DSP-PRO
*******************     UNIT      CONTROL   DSP-UNIT
                        SELECTED  FUNCTION  DSP-FNC
                        FILE      STATUS    DSP-ST1.
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
 FD  SHTDENF            BLOCK     CONTAINS   4   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     SHTDENF   OF   XFDLIB    JOINING   DEN  AS   PREFIX.
***   93/05/24  START   ***
*----<< 商品変換テーブル４ >>-*
 FD  SHOTBL4            BLOCK     CONTAINS   12  RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HSHOTBL   OF   XFDLIB    JOINING   SHT  AS   PREFIX.
*----<< 商品在庫マスタ >>-*
 FD  ZZAIMS             LABEL     RECORD     IS  STANDARD.
     COPY     ZZAIMS    OF   XFDLIB    JOINING   ZAI  AS   PREFIX.
***   93/05/24    END   ***
*----<< 画面ファイル >>-*
 FD  DSPFILE.
     COPY     FSY01011  OF   XMDLIB.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  PGM-ID                  PIC  X(08)     VALUE  "SSY0101I".
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
***   93/05/24  START   ***
     03  SHT-STATUS.
         05  SHT-ST1         PIC  X(02).
         05  SHT-ST2         PIC  X(04).
     03  ZAI-STATUS.
         05  ZAI-ST1         PIC  X(02).
         05  ZAI-ST2         PIC  X(04).
***   93/05/24    END   ***
     03  DEN-STATUS.
         05  DEN-ST1         PIC  X(02).
         05  DEN-ST2         PIC  X(04).
*----<< ﾃﾞｰﾀ ﾃｰﾌﾞﾙ >>-*
 01  TABLE-AREA.
     03  TBL                 OCCURS    12.
         05  FILLER          PIC  X(05).
         05  TBL-R10101      PIC  X(01).
         05  FILLER          PIC  X(05).
         05  TBL-R10201      PIC  9(02).
         05  FILLER          PIC  X(05).
         05  TBL-R10301      PIC  X(08).
         05  FILLER          PIC  X(05).
         05  TBL-R10401      PIC  X(05).
         05  FILLER          PIC  X(05).
         05  TBL-R10501      PIC  X(02).
         05  FILLER          PIC  X(05).
         05  TBL-R11501      PIC  X(01).
         05  FILLER          PIC  X(05).
         05  TBL-R10601      PIC  9(08)V99.
         05  FILLER          PIC  X(05).
         05  TBL-R10701      PIC  X(01).
         05  FILLER          PIC  X(05).
         05  TBL-R10801      PIC  9(08)V99.
         05  FILLER          PIC  X(05).
         05  TBL-R10901      PIC  9(08)V99.
         05  FILLER          PIC  X(05).
         05  TBL-R11001      PIC  N(02).
         05  FILLER          PIC  X(05).
         05  TBL-R11101      PIC  X(15).
         05  FILLER          PIC  X(05).
         05  TBL-R11201      PIC  X(15).
         05  FILLER          PIC  X(05).
         05  TBL-R11301      PIC  X(05).
         05  FILLER          PIC  X(05).
         05  TBL-R11401      PIC  X(10).
         05  TBL-SHOCD       PIC  X(16).
         05  TBL-MAESUU      PIC S9(09)V99.
         05  TBL-MAEGTAN     PIC S9(09)V99.
         05  TBL-MAEBTAN     PIC S9(09)V99.
         05  TBL-MAEGKIN     PIC S9(11).
         05  TBL-MAEBKIN     PIC S9(11).
*
     03  TBL-SIIGEN          PIC  9(09)V99  OCCURS  12.
***   93/05/24  START   ***
     03  TBL-XX.
         05  TBL-X                   OCCURS  12.
             07  TBL-JHINCD          PIC  X(08).
             07  TBL-HINTAN          PIC  X(08).
             07  TBL-TANABN          PIC  X(06).
             07  TBL-WSOKCD          PIC  X(02).
             07  TBL-WHINCD          PIC  X(08).
             07  TBL-WHTAN           PIC  X(08).
             07  TBL-WSURYO          PIC  9(09)V99.
             07  TBL-WTANA           PIC  X(06).
             07  TBL-HIKIATE         PIC  X(01).
             07  TBL-SYUKKA          PIC  9(06).
***   93/05/24    END   ***
 01  WK-HIKIATE                      PIC  S9(10)V99.
*画面退避用
     COPY   FSY01011 OF XMDLIB  JOINING   SAV  AS   PREFIX.
*
***  93/05/08  START  ***
 01  SAVE-AREA.
     03  XSAV-SHOCD          PIC  X(16)     OCCURS  12.
 01  SAV-MAESORYO            PIC S9(11)     VALUE   ZERO.
 01  XIDX                    PIC  9(02).
 01  WK-CHK                  PIC  9(09)     VALUE   ZERO.
***  93/05/08  END    ***
 01  WK-R00007               PIC  9(10).
 01  WK-R00007-R             REDEFINES      WK-R00007.
     03 WK-R00007-RR         PIC  9(01)     OCCURS  10.
 01  WK-KETA                 PIC  9(02)     VALUE   ZERO.
 01  WK-KETA-R               PIC  9(02)     VALUE   ZERO.
*
 01  WK-TAIHI.
     03  WK-F23              PIC  9(09).
     03  WK-F276             PIC  9(01).
     03  WK-F27B             PIC  9(01).
     03  WK-R00023           PIC  9(02).
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
         NC"_取消_終了".
     03  G002                PIC  N(30)  VALUE
         NC"_取消_項目戻り".
     03  G003                PIC  N(30)  VALUE
         NC"_取消_項目戻り_次頁".
     03  G004                PIC  N(30)  VALUE
         NC"_取消_項目戻り_前頁".
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
                             NC"ＰＦキーが違います".
     03  MSG02               PIC  N(20)  VALUE
                             NC"処理区分が違います".
     03  MSG03               PIC  N(20)  VALUE
                             NC"請求区分が違います".
     03  MSG04               PIC  N(20)  VALUE
                             NC"取引先マスタ未登録".
     03  MSG05               PIC  N(20)  VALUE
                             NC"チェックデジットが違います．".
     03  MSG06               PIC  N(20)  VALUE
                             NC"伝票区分が違います".
     03  MSG07               PIC  N(20)  VALUE
                             NC"店舗マスタ未登録".
     03  MSG08               PIC  N(20)  VALUE
                             NC"出荷場所未登録".
     03  MSG09               PIC  N(20)  VALUE
                             NC"伝発場所未登録".
     03  MSG10               PIC  N(20)  VALUE
                             NC"日付の入力が違います．".
     03  MSG11               PIC  N(20)  VALUE
                             NC"伝発は０または９を入力".
     03  MSG12               PIC  N(20)  VALUE
                             NC"空白，１，９を入力".
     03  MSG13               PIC  N(20)  VALUE
                             NC"商品未登録".
     03  MSG14               PIC  N(20)  VALUE
                             NC"単価区分が違います".
     03  MSG15               PIC  N(20)  VALUE
                             NC"Ｙ，Ｈ，Ｂを入力".
     03  MSG16               PIC  N(20)  VALUE
                             NC"Ｙを入力".
     03  MSG17               PIC  N(20)  VALUE
                             NC"摘要コード未登録".
     03  MSG18               PIC  N(20)  VALUE
                             NC"伝票が入力済です".
     03  MSG19               PIC  N(20)  VALUE
                             NC"伝票が未入力です".
     03  MSG20               PIC  N(20)  VALUE
                             NC"オンラインデータです".
     03  MSG21               PIC  N(20)  VALUE
                             NC"２画面目があります．".
     03  MSG22               PIC  N(20)  VALUE
                             NC"修正できません．".
     03  MSG23               PIC  N(20)  VALUE
                             NC"削除できません．".
     03  MSG24               PIC  N(20)  VALUE
                             NC"担当者コードを入力してください".
     03  MSG25               PIC  N(20)  VALUE
                             NC"納品日がＡＣＯＳ締日以前です".
     03  MSG26               PIC  N(20)  VALUE
                             NC"相殺区分がちがいます".
     03  MSG27               PIC  N(20)  VALUE
                             NC"売上データが未作成です".
     03  MSG28               PIC  N(20)  VALUE
                             NC"０は入力出来ません".
     03  MSG29               PIC  N(20)  VALUE
                             NC"明細を入力して下さい".
     03  MSG30               PIC  N(20)  VALUE
                             NC"赤伝は修正できません".
     03  MSG31               PIC  N(20)  VALUE
                             NC"赤伝は削除できません".
     03  MSG32               PIC  N(20)  VALUE
                             NC"相殺処理はできません".
***  94.05.16  START  ***
     03  MSG33               PIC  N(20)  VALUE
                             NC"出荷日がＡＣＯＳ締日以前です".
     03  MSG34               PIC  N(20)  VALUE
                             NC"出荷日が納品日以降です".
     03  MSG35               PIC  N(20)  VALUE
                             NC"出荷日は締日以前にできません".
     03  MSG36               PIC  N(20)  VALUE
     NC"既に返品データが存在します。注意必要！！".
     03  MSG37               PIC  N(20)  VALUE
     NC"既に値引データが存在します。注意必要！！".
     03  MSG38               PIC  N(20)  VALUE
     NC"返品データ合計と同じです。登録不可！！".
     03  MSG39               PIC  N(20)  VALUE
     NC"値引データ合計と同じです。登録不可！！".
***  94.05.16   END   ***
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       39.
 01  SORYO-CD                PIC  X(08)  VALUE  "00999961".
 01  WK-SYS-DATE             PIC  9(08).
 01  FILLER                  REDEFINES   WK-SYS-DATE.
     03  WK-SYS-YY           PIC  9(04).
     03  WK-SYS-MM           PIC  9(02).
     03  WK-SYS-DD           PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY            PIC  9(04).
     03  FILLER              PIC  X(01)     VALUE "/".
     03  WK-SYSMM            PIC  Z9.
     03  FILLER              PIC  X(01)     VALUE "/".
     03  WK-SYSDD            PIC  Z9.
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
 01  SYS-TIME2               PIC  9(08).
 01  FILLER            REDEFINES  SYS-TIME2.
     03  SYS-TIMEW           PIC  9(06).
     03  FILLER              PIC  9(02).
*
 01  HENKAN-TI               PIC  9(04).
 01  GETUDO                  PIC  9(02).
 01  GETUDO2                 PIC S9(02).
 01  GETUDO2-YMD.
     03  GETUDO2-YY          PIC  9(04).
     03  GETUDO2-MM          PIC  9(02).
     03  GETUDO2-DD          PIC  9(02).
 01  GETUDO2-YMDR      REDEFINES  GETUDO2-YMD.
     03  GETUDO2-YYMMR       PIC  9(06).
     03  FILLER              PIC  X(02).
 01  WK-DEN112               PIC  9(08).
 01  FILLER                  REDEFINES      WK-DEN112.
     03  WK-DEN112YM         PIC  9(06).
     03  WK-DEN112D          PIC  9(02).
***  94.05.16  START  ***
 01  WK-DEN113               PIC  9(08).
 01  FILLER                  REDEFINES      WK-DEN113.
     03  WK-DEN113YM         PIC  9(06).
     03  WK-DEN113D          PIC  9(02).
***  94.05.16   END   ***
 01  ACOS-DATE               PIC  9(08).
 01  FILLER                  REDEFINES      ACOS-DATE.
     03  ACOS-YYMM           PIC  9(06).
     03  ACOS-DD             PIC  9(02).
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
*94/06/18
 01  SAV-NOU-DATE.
     03  SAV-NOU-YYMM        PIC  9(06).
     03  SAV-NOU-DD          PIC  9(02).
 01  SAV-CYU-DATE            PIC  9(08)    VALUE ZERO.
 01  SAV-SYU-DATE            PIC  9(08)    VALUE ZERO.
 01  SAV-ZENGETU             PIC  9(08).
 01  SAV-ACOS-YYMM.
     03  SAV-ACOS-YY         PIC  9(04).
     03  SAV-ACOS-MM         PIC  9(02).
 01  SAV-ACOS-MMS            PIC  9(02).
***
 01  CHK-DATE-WORK.
     03  CHK-01              PIC  9(04).
     03  CHK-02              PIC  9(02).
     03  MATUBI              PIC  X(24)  VALUE
         "312831303130313130313031".
     03  FILLER              REDEFINES   MATUBI.
         05  WK-MATUBI       PIC  9(02)  OCCURS  12.
 01  INDEXES.
     03  I                   PIC  9(02).
     03  J                   PIC  9(02).
     03  K                   PIC  9(02).
     03  L                   PIC  9(02).
     03  T                   PIC  9(02).
 01  FLAGS.
     03  GPAGE-FLG           PIC  9(01).
     03  END-FLG             PIC  9(01).
     03  INV-FLG             PIC  9(01).
     03  ERR-FLG             PIC  9(02).
     03  HEN-FLG             PIC  9(01).
***   93/05/24  START   ***
     03  ZAI-FLG             PIC  9(01).
     03  REW-FLG             PIC  9(01).
***   93/05/24    END   ***
     03  GR-NO               PIC  9(02).
     03  DSP-OPEN-FLG        PIC  9(01).
     03  TOK-OPEN-FLG        PIC  9(01).
     03  TEN-OPEN-FLG        PIC  9(01).
     03  STB-OPEN-FLG        PIC  9(01).
     03  MEI-OPEN-FLG        PIC  9(01).
     03  JYO-OPEN-FLG        PIC  9(01).
***   93/05/24  START   ***
     03  SHT-OPEN-FLG        PIC  9(01).
     03  ZAI-OPEN-FLG        PIC  9(01).
***   93/05/24    END   ***
     03  DEN-OPEN-FLG        PIC  9(01).
 01  COUNTERS.
     03  P-CNT               PIC  9(02).
     03  L-MAX               PIC  9(02).
     03  NUM-CNT             PIC  9(02).
 01  SURYO-WORK.
     03  WK-R10801           PIC  9(08)V99.
     03  WK-R10901           PIC  9(08)V99.
***   93/05/24  START   ***
     03  WK-SURYO            PIC S9(08)V99.
 01  WK-YYMM.
     03  WK-YY               PIC  9(02).
     03  WK-MM               PIC  9(02).
 01  WK-SIME                 PIC  9(04).
***   93/05/24    END   ***
 01  DSP-WORK.
     03  WK-R00002           PIC  9(01).
     03  WK-R10201           PIC  9(02).
 01  WK-R103-AREA.
     03  WK-R103I-X.
         05  WK-R103I        PIC  X(01)  OCCURS  8.
     03  WK-R103O-X.
         05  WK-R103O        PIC  X(01)  OCCURS  8.
     03  WK-R103             PIC  9(8).
 01  WK-R104-AREA.
     03  WK-R104I-X.
         05  WK-R104I        PIC  X(01)  OCCURS  5.
     03  WK-R104O-X.
         05  WK-R104O        PIC  X(01)  OCCURS  5.
     03  WK-R104             PIC  X(05).
 01  CONV-AREA.
     03  WK-NUM-1.
         05  WK-NUM-TBL-1    PIC  X(01)  OCCURS  15.
     03  WK-NUM-2.
         05  WK-NUM-TBL-2    PIC  X(01)  OCCURS  15.
     03  NUM-FLG-TABLE.
         05  NUM-FLG-TBL     PIC  9(01)  OCCURS  15.
*伝票区分”４１，４２”チェックワーク##### 98/09/17 追加 START
 01  WK-DENPYO-CHK.
     03  WK-R00026           PIC  9(08)V9(02)  VALUE  ZERO.
     03  WK-R00027           PIC  9(08)V9(02)  VALUE  ZERO.
 01  WK-DENKU-AREA.
     03  DEN-KAKU-CHK        PIC  9(01)  VALUE  ZERO.
     03  WK-DENKU            PIC  9(02)  VALUE  ZERO.
*************************************##### 98/09/17 追加 END
*---<< ｻﾌﾞﾙｰﾁﾝ LINK AREA >>-*
 01  LINK-AREA.
     03  LINK-IN.
         05  LI-KBN          PIC  9(01).
         05  LI-KETA         PIC  9(01).
         05  LI-START        PIC  9(09).
         05  LI-END          PIC  9(09).
         05  LI-DENNO        PIC  9(09).
     03  LINK-OUT.
         05  LO-ERR          PIC  9(01).
         05  LO-NEXT         PIC  9(09).
*
 01  LINK-AREA2.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
******************************************************************
 PROCEDURE                 DIVISION.
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
     MOVE     "4000"         TO   PROGRAM-STATUS.
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
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  商品変換テーブル　--------------------------------*
 STB-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HSHOTBL.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"商品変換テーブル異常！"
              "ST1=" STB-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     "4000"         TO   PROGRAM-STATUS.
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
     MOVE     "4000"         TO   PROGRAM-STATUS.
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
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
***   93/05/24  START   ***
*----------  商品変換テーブル４　------------------------------*
 SHT-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   SHOTBL4.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"商品変換テーブル４異常！"
              "ST1=" SHT-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
*----------   商品在庫マスタ  ---------------------------------*
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
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
***   93/05/24    END   ***
*----------    伝票データＦ -----------------------------------*
 DEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   SHTDENF.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"伝票データＦ異常！"
              "ST1=" DEN-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
*----------    表示ファイル -----------------------------------*
 DSP-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   DSPFILE.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"表示ファイル異常！"
              "ST1=" DSP-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 PROG-CNTL          SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     GR-NO    =    99.
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 INIT-RTN           SECTION.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " START "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STAT.
*    システム日付８桁変換
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   WK-SYS-DATE
     ELSE
         MOVE ZERO           TO   WK-SYS-DATE
     END-IF.
*
     MOVE     WK-SYS-YY      TO   WK-SYSYY.
     MOVE     WK-SYS-MM      TO   WK-SYSMM.
     MOVE     WK-SYS-DD      TO   WK-SYSDD.
*
     MOVE        ZERO      TO        FLAGS.
*
     OPEN     I-O       HTOKMS.
     MOVE     1              TO   TOK-OPEN-FLG.
     OPEN     INPUT     HTENMS.
     MOVE     1              TO   TEN-OPEN-FLG.
     OPEN     INPUT     HSHOTBL.
     MOVE     1              TO   STB-OPEN-FLG.
     OPEN     INPUT     HMEIMS.
     MOVE     1              TO   MEI-OPEN-FLG.
     OPEN     INPUT     HJYOKEN.
     MOVE     1              TO   JYO-OPEN-FLG.
     OPEN     I-O       SHTDENF.
     MOVE     1              TO   DEN-OPEN-FLG.
***   93/05/24  START   ***
     OPEN     INPUT     SHOTBL4.
     MOVE     1              TO   SHT-OPEN-FLG.
     OPEN     I-O       ZZAIMS.
     MOVE     1              TO   ZAI-OPEN-FLG.
***   93/05/24    END   ***
     OPEN     I-O       DSPFILE.
     MOVE     1              TO   DSP-OPEN-FLG.
*条件Ｆ（西暦変換）
*****MOVE     "57"           TO   JYO-F01.
*****MOVE     SPACE          TO   JYO-F02.
*****PERFORM  JYO-RD-RTN.
*****IF       INV-FLG  =  1
*****         DISPLAY   "HJYOKEN INV KEY=57"  UPON STAT
*****         MOVE      99        TO   GR-NO
*****         GO   TO   INIT-EXIT
*****END-IF.
*****MOVE     JYO-F04        TO   HENKAN-TI.
*条件Ｆ（経理月）
     MOVE     "58"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       INV-FLG  =  1
              DISPLAY   "HJYOKEN INV KEY=58"  UPON STAT
              MOVE      99        TO   GR-NO
              GO   TO   INIT-EXIT
     END-IF.
     MOVE     JYO-F04        TO   GETUDO.
     MOVE     JYO-F04        TO   GETUDO2.
*条件Ｆ（ＡＣＯＳ用締日）
     MOVE     "99"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       INV-FLG  =  1
              DISPLAY   "HJYOKEN INV KEY=99"  UPON STAT
              MOVE      99        TO   GR-NO
              GO   TO   INIT-EXIT
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
              GO   TO   INIT-EXIT
     END-EVALUATE.
*
     MOVE     ACOS-DATE      TO   GETUDO2-YMD.
     MOVE     99             TO   GETUDO2-DD.
     IF       GETUDO2        =    12
              COMPUTE        GETUDO2-YY     =
                             GETUDO2-YY     -        1
              MOVE     12    TO             GETUDO2-MM
     ELSE
              MOVE      GETUDO2   TO        GETUDO2-MM
     END-IF.
*
**** DISPLAY  "ACOS-DATE   = " ACOS-DATE   UPON CONS.
**** DISPLAY  "GETUDO2-YMD = " GETUDO2-YMD UPON CONS.
*
     MOVE     SYS-YY         TO   START-YY
                                  END-YY.
     MOVE     SYS-MM         TO   START-MM
                                  END-MM.
     ADD      1              TO   END-MM.
     IF       END-MM  >  12
              MOVE      1         TO   END-MM
**************COMPUTE   END-YY   =  HENKAN-TI  +  END-YY  +  1
              COMPUTE   END-YY   =  WK-SYS-YY  +  1
     ELSE
**************COMPUTE   END-YY   =  HENKAN-TI  +  END-YY
              MOVE      WK-SYS-YY   TO      END-YY
     END-IF.
     SUBTRACT 1              FROM START-MM.
     IF       START-MM  <  1
              MOVE      12        TO   START-MM
**************COMPUTE   START-YY  =  HENKAN-TI  +  START-YY  -  1
              COMPUTE   START-YY  =  WK-SYS-YY  -  1
     ELSE
**************COMPUTE   START-YY  =  HENKAN-TI  +  START-YY
              MOVE      WK-SYS-YY      TO   START-YY
     END-IF.
*
     MOVE     GETUDO2-YYMMR  TO      START-YYMM.
**** DISPLAY  "START-YYMM  = " START-YYMM  UPON CONS.
**** DISPLAY  "END-YYMM    = " END-YYMM  UPON CONS.
*
***   93/05/24  START   ***
*条件Ｆ（在庫締日）
     MOVE     "99"           TO   JYO-F01.
     MOVE     "ZAI"          TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       INV-FLG  =  1
              DISPLAY   "HJYOKEN INV KEY=99"  UPON STAT
              MOVE      99        TO   GR-NO
              GO   TO   INIT-EXIT
     END-IF.
     MOVE     JYO-F05        TO   WK-SIME.
     MOVE     WK-SIME        TO   WK-YYMM.
     ADD      1              TO   WK-MM.
     IF       WK-MM     >    12
              ADD       1    TO   WK-YY
              MOVE      1    TO   WK-MM
     END-IF.
     MOVE     WK-YYMM        TO   WK-SIME.
*****DISPLAY  "WK-SIME     = " WK-SIME     UPON CONS.
***   93/05/24    END   ***
*
     MOVE     0              TO   GR-NO.
 INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 MAIN-RTN           SECTION.
* 画面初期表示
     PERFORM     DSP-INIT-RTN     UNTIL     GR-NO  NOT  =  0.
* 処理区分入力
     PERFORM     DSP-R001-RTN     UNTIL     GR-NO  NOT  =  1.
* 相殺区分入力
     PERFORM     DSP-R002-RTN     UNTIL     GR-NO  NOT  =  2.
* ＧＨＥＡＤ１入力
     PERFORM     DSP-GHEAD1-RTN   UNTIL     GR-NO  NOT  =  3.
* ＧＨＥＡＤ２入力
     PERFORM     DSP-GHEAD2-RTN   UNTIL     GR-NO  NOT  =  4.
* ＧＢＯＤＹ１入力
     PERFORM     DSP-GBODY1-RTN   UNTIL     GR-NO  NOT  =  5.
* 確認入力
     PERFORM     DSP-KKNN-RTN     UNTIL     GR-NO  NOT  =  90.
 MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 END-RTN            SECTION.
     CLOSE              HTOKMS
                        HTENMS
                        HSHOTBL
***   93/05/24  START   ***
                        SHOTBL4
***   93/05/24    END   ***
                        HMEIMS
                        HJYOKEN
                        ZZAIMS
                        SHTDENF
                        DSPFILE.
*
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " END   "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STAT.
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL    ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 DSP-INIT-RTN           SECTION.
     MOVE     SPACE          TO   FSY01011.
     MOVE     SPACE          TO   TABLE-AREA.
     INITIALIZE                   TBL-XX.
     MOVE     ZERO           TO   WK-TAIHI.
     MOVE     WK-SYSYMD      TO   R00000.
     MOVE     SYS-DATE       TO   R00015
                                  R00016.
**********************************R00021.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  10
              IF   I  <  7
                   MOVE      I         TO   R10201(I)
              END-IF
              MOVE     I          TO   TBL-R10201(I)
     END-PERFORM.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-BODY-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSY01011"     TO   DSP-FMT.
     MOVE     "ALLF"         TO   DSP-GRP.
*
     MOVE     1              TO   GR-NO.
     MOVE     1              TO   P-CNT.
 DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      処理区分　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-R001-RTN           SECTION.
     MOVE     G001           TO   GUIDE.
     MOVE     "R00001"       TO   WK-GRP.
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
     END-EVALUATE.
*
 DSP-R001-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      処理区分　チェック　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-R001-RTN           SECTION.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
     IF       R00001  IS  NOT  NUMERIC
              MOVE      0         TO   R00001
     END-IF.
     IF       R00001  NOT  =  1  AND  2  AND  3  AND  9
              MOVE      2         TO   ERR-FLG
              MOVE     "R"        TO   EDIT-OPTION OF R00001
     END-IF.
 CHK-R001-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      相殺区分　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-R002-RTN       SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "R00002"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
     WHEN     PF06
              MOVE      1         TO   GR-NO
     WHEN     ENT
              PERFORM   CHK-R002-RTN
              IF   ERR-FLG  =  ZERO
                   MOVE      3         TO   GR-NO
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-R002-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      相殺区分　チェック　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-R002-RTN       SECTION.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
     IF       R00002  IS  NOT  NUMERIC
              MOVE      0         TO   R00002
     END-IF.
*
     IF       R00002  =  9
              MOVE      26        TO   ERR-FLG
              MOVE     "R"        TO   EDIT-OPTION OF R00002
     END-IF.
*
     IF       R00002  NOT =       ZERO
              MOVE      32        TO   ERR-FLG
              MOVE     "R"        TO   EDIT-OPTION OF R00002
     END-IF.
*
 CHK-R002-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＨＥＡＤ１　入力　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-GHEAD1-RTN     SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "GHEAD1"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
     WHEN     PF06
                   MOVE      2         TO   GR-NO
     WHEN     ENT
              PERFORM   CHK-1-RTN
*##### 98/09/18 追加 START #####
              IF   ERR-FLG  =  ZERO  OR 21 OR 36 OR 37
*##### 98/09/18 追加 END   #####
                IF ( R00001 = 1  AND R00002 = 1 OR 3 OR 5 OR 7 )
                OR ( R00001 = 3  OR  9 )
                     MOVE      "Y"       TO   KKNN
                     MOVE      90        TO   GR-NO
                ELSE
                     MOVE      4         TO   GR-NO
                END-IF
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-GHEAD1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＨＥＡＤ２　入力　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-GHEAD2-RTN     SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "GHEAD2"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
     WHEN     PF06
              MOVE      3         TO   GR-NO
     WHEN     ENT
              PERFORM   CHK-2-RTN
              IF   ERR-FLG  = ZERO
                   MOVE      5         TO   GR-NO
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-GHEAD2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      チェック１　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-1-RTN       SECTION.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
*取引先チェック
     IF       R00005  IS  NOT  NUMERIC
              MOVE      0         TO   R00005
     END-IF.
     MOVE     R00005         TO   TOK-F01.
     PERFORM  900-TOK-READ.
     IF       INV-FLG  =  1
              MOVE      4         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF R00005
              MOVE     "R"        TO   EDIT-OPTION OF R00005
              GO   TO   CHK-1-EXIT
     ELSE
              MOVE     TOK-F03        TO   R00006
              PERFORM  900-L-MAX-GET
     END-IF.
*伝票_チェック
     IF       R00007  IS  NOT  NUMERIC
              MOVE      0         TO   R00007
     END-IF.
     IF       R00007  =  0
     AND      R00001  =  1
     AND      R00002  =  0
              PERFORM   900-DENNO-GET
     ELSE
              PERFORM   900-DENNO-CHECK
     END-IF.
*伝区チェック
     IF       R00008  IS  NOT  NUMERIC
              MOVE      0         TO   R00008
     END-IF.
     MOVE     "01"           TO   JYO-F01.
     MOVE     R00008         TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       INV-FLG  =  1
              MOVE      6         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF R00008
              MOVE     "R"        TO   EDIT-OPTION OF R00008
              GO   TO   CHK-1-EXIT
     ELSE
**************　伝票区分＝５０，５１，５２，５５はエラー
              IF   JYO-F02  =  "50      "
                            OR "51      "
                            OR "52      "
                            OR "55      "
                   MOVE  6  TO  ERR-FLG
                   MOVE "C" TO  EDIT-CURSOR OF R00008
                   MOVE "R" TO  EDIT-OPTION OF R00008
                   GO  TO   CHK-1-EXIT
              END-IF
              MOVE     JYO-F03        TO   R00009
     END-IF.
*伝票チェック
     PERFORM  CHK-DEN-RTN.
*伝票区分”４１，４２”の時のチェックフラグの解除
     MOVE         ZERO             TO    DEN-KAKU-CHK.
     MOVE         ZERO             TO    WK-R00026.
     MOVE         ZERO             TO    WK-R00027.
     IF  R00001 = 1
     AND R00002 = 0
*##### 98/09/17 追加 START #####
*伝票区分”４１，４２”の時，処理追加（エラーフラグが０の時）
         IF   ERR-FLG  =  ZERO
              PERFORM   DENPYO-ST-RTN
              IF  END-FLG = ZERO
                  PERFORM DENPYO-READ-RTN
              END-IF
              IF  DEN-KAKU-CHK  =  1
                  EVALUATE R00008
                      WHEN 41
                      MOVE  37 TO  ERR-FLG
********************* MOVE "V" TO  EDIT-OPTION OF ERR
                      WHEN 42
                      MOVE  36 TO  ERR-FLG
********************* MOVE "V" TO  EDIT-OPTION OF ERR
                  END-EVALUATE
              ELSE
                  MOVE  0      TO  ERR-FLG
              END-IF
         END-IF
*##### 98/09/17 追加 END   #####
         GO   TO   CHK-1-EXIT
     END-IF.
*画面＆ＴＢＬクリア
     MOVE      R00001    TO   SAV-R00001.
     MOVE      R00002    TO   SAV-R00002.
     MOVE      R00015    TO   SAV-R00015.
     MOVE      R00016    TO   SAV-R00016.
***  94.05.16 START  ***
     MOVE      R00021    TO   SAV-R00021.
***  94.05.16  END   ***
     MOVE      HEAD1     TO   SAV-HEAD1.
     MOVE      SPACE     TO   FSY01011   TABLE-AREA.
     INITIALIZE               TBL-XX.
     MOVE      ZERO      TO   WK-TAIHI.
     MOVE      SAV-HEAD1      TO   HEAD1.
     MOVE      WK-SYSYMD      TO   R00000.
     MOVE      SAV-R00001     TO   R00001.
     MOVE      SAV-R00002     TO   R00002.
     MOVE      SAV-R00015     TO   R00015.
     MOVE      SAV-R00016     TO   R00016.
***  94.05.16 START  ***
     MOVE      SAV-R00021     TO   R00021.
***  94.05.16  END   ***
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  10
       IF   I  <  7
            MOVE      I         TO   R10201 (I)
       END-IF
       MOVE     I          TO   TBL-R10201 (I)
     END-PERFORM.
     IF  END-FLG   = 1
              GO   TO   CHK-1-EXIT
     END-IF.
*伝票画面へ表示
     PERFORM  DEN-DSP-RTN.
     PERFORM  DSP-GOKEI-RTN.
*93/05/14 START *
*赤伝　修正削除プロテクト
     IF  (R00002  = 1 OR 3 OR 5 OR 7 OR 9)
     AND (R00001  = 2 OR 3)
          MOVE     "C"        TO   EDIT-CURSOR OF R00002
          MOVE     "R"        TO   EDIT-OPTION OF R00002
          MOVE      2         TO   GR-NO
          EVALUATE  R00001
            WHEN    2         MOVE   30   TO   ERR-FLG
            WHEN    3         MOVE   31   TO   ERR-FLG
          END-EVALUATE
     END-IF.
*93/05/14 END *
 CHK-1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      伝票　画面へ表示　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-DEN-RTN            SECTION.
*  全レコード解放（排他制御用）
     CLOSE    SHTDENF.
     OPEN     I-O   SHTDENF.
*
     MOVE     R00005         TO   DEN-F01.
     MOVE     R00007         TO   DEN-F02.
     MOVE     R00002         TO   DEN-F04  WK-R00002.
     MOVE     R00008         TO   DEN-F051.
     MOVE     0              TO   DEN-F03.
     MOVE     0              TO   END-FLG.
     START    SHTDENF   KEY  NOT  <  DEN-F01     DEN-F02
                                     DEN-F04     DEN-F051
                                     DEN-F03
         INVALID   KEY
              MOVE      1         TO   END-FLG
         NOT  INVALID   KEY
              PERFORM   900-DEN-READ
     END-START.
     IF       END-FLG  =  1
     AND      R00001  NOT  =  1
              MOVE      19        TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF R00007
              MOVE     "R"        TO   EDIT-OPTION OF R00007
              GO   TO   CHK-DEN-EXIT
     END-IF.
     IF       END-FLG  =  0
     AND      R00001   =  1
              MOVE      18        TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF R00007
              MOVE     "R"        TO   EDIT-OPTION OF R00007
              GO   TO   CHK-DEN-EXIT
     END-IF.
*ＡＣＯＳ締日チェック
     IF       R00001  =  2  OR  3
**************COMPUTE  WK-SYS-DATE = HENKAN-TI * 10000 + SYS-DATE
              MOVE     DEN-F112   TO   WK-DEN112
              IF   DEN-F112  NOT  =  0
              AND  ACOS-DATE  <  WK-SYS-DATE
              AND  WK-DEN112YM   <   ACOS-YYMM
                   MOVE      25      TO   ERR-FLG
                   MOVE     "C"      TO   EDIT-CURSOR OF R00007
                   MOVE     "R"      TO   EDIT-OPTION OF R00007
                   GO   TO   CHK-DEN-EXIT
              END-IF
***  94.05.16 START  ***
              MOVE     DEN-F113   TO   WK-DEN113
              IF   DEN-F113  NOT  =  0
               AND ACOS-DATE      <  WK-SYS-DATE
               AND WK-DEN113YM    <  ACOS-YYMM
                   MOVE      33      TO   ERR-FLG
                   MOVE     "C"      TO   EDIT-CURSOR OF R00007
                   MOVE     "R"      TO   EDIT-OPTION OF R00007
                   GO   TO   CHK-DEN-EXIT
              END-IF
***  94.05.16  END   ***
     END-IF.
*売上作成ＦＬＧチェック
     IF   R00001 = 2
     AND  DEN-F277 NOT =  ZERO
          MOVE      22      TO   ERR-FLG
          MOVE     "C"      TO   EDIT-CURSOR OF R00007
          MOVE     "R"      TO   EDIT-OPTION OF R00007
          GO   TO   CHK-DEN-EXIT
     END-IF.
     IF   R00001 = 3
     AND  DEN-F277 NOT =  ZERO
          MOVE      23      TO   ERR-FLG
          MOVE     "C"      TO   EDIT-CURSOR OF R00007
          MOVE     "R"      TO   EDIT-OPTION OF R00007
          GO   TO   CHK-DEN-EXIT
     END-IF.
*オンラインＦＬＧチェック
     IF   R00001   =  3
     AND  DEN-F274 NOT = ZERO
          MOVE     20       TO   ERR-FLG
          MOVE     "C"      TO   EDIT-CURSOR OF R00007
          MOVE     "R"      TO   EDIT-OPTION OF R00007
          GO   TO   CHK-DEN-EXIT
     END-IF.
*相殺登録時のデータ表示
     IF       R00001  =  1
     AND      R00002  NOT  =  0
              MOVE      R00005         TO   DEN-F01
              MOVE      R00007         TO   DEN-F02
              COMPUTE   WK-R00002  =  R00002  -  1
              MOVE      WK-R00002      TO   DEN-F04
              MOVE      R00008         TO   DEN-F051
              MOVE      0              TO   DEN-F03
              MOVE      0              TO   END-FLG
              START     SHTDENF   KEY  NOT  <  DEN-F01   DEN-F02
                                               DEN-F04   DEN-F051
                                               DEN-F03
                INVALID   KEY
                   MOVE      1         TO   END-FLG
                NOT  INVALID   KEY
                   PERFORM   900-DEN-READ
              END-START
              IF   END-FLG  =  1
                   MOVE      19        TO   ERR-FLG
                   MOVE     "C"      TO   EDIT-CURSOR OF R00007
                   MOVE     "R"      TO   EDIT-OPTION OF R00007
                   GO   TO   CHK-DEN-EXIT
              END-IF
              IF ( R00002  =  1  OR  3  OR  5  OR  7 )
              AND (DEN-F277  NOT  =  9)
                   MOVE      27        TO   ERR-FLG
                   MOVE     "C"      TO   EDIT-CURSOR OF R00007
                   MOVE     "R"      TO   EDIT-OPTION OF R00007
                   GO   TO   CHK-DEN-EXIT
              END-IF
**************COMPUTE  WK-SYS-DATE = HENKAN-TI * 10000 + SYS-DATE
              MOVE     DEN-F112   TO   WK-DEN112
              IF   DEN-F112  NOT  =  0
              AND  ACOS-DATE  <  WK-SYS-DATE
              AND  WK-DEN112YM   <   ACOS-YYMM
                   MOVE      25        TO   ERR-FLG
                   MOVE     "C"      TO   EDIT-CURSOR OF R00007
                   MOVE     "R"      TO   EDIT-OPTION OF R00007
                   GO   TO   CHK-DEN-EXIT
              END-IF
***  94.05.16 START  ***
              MOVE     DEN-F113   TO   WK-DEN113
              IF   DEN-F113  NOT  =  0
               AND ACOS-DATE      <  WK-SYS-DATE
               AND WK-DEN113YM    <  ACOS-YYMM
                   MOVE      33      TO   ERR-FLG
                   MOVE     "C"      TO   EDIT-CURSOR OF R00007
                   MOVE     "R"      TO   EDIT-OPTION OF R00007
                   GO   TO   CHK-DEN-EXIT
              END-IF
***  94.05.16  END   ***
     END-IF.
 CHK-DEN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      伝票　画面へ表示　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DEN-DSP-RTN            SECTION.
*データ表示
     MOVE      ZERO   TO  GPAGE-FLG.
     PERFORM   900-DEN-TO-HED.
     PERFORM   VARYING  I  FROM  1  BY  1
               UNTIL    I  >  10
               OR       END-FLG  =  1
          PERFORM   900-DEN-TO-TBL
          PERFORM   900-DEN-READ
     END-PERFORM.
     IF  GPAGE-FLG = 1
     AND ERR-FLG = ZERO
          MOVE      21        TO   ERR-FLG
     END-IF
     MOVE      1         TO   P-CNT.
     PERFORM   900-TBL-TO-DSP.
*
 DEN-DSP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      チェック３　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-2-RTN          SECTION.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
*担当者チェック
     IF       R00003  IS  NOT  NUMERIC
              MOVE      0         TO   R00003
     END-IF.
     IF       R00003  =  0  OR  99
              MOVE      24        TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF R00003
              MOVE     "R"        TO   EDIT-OPTION OF R00003
              GO   TO   CHK-2-EXIT
     END-IF.
*請求区分チェック
     IF       R00004  IS  NOT  NUMERIC
              MOVE      0         TO   R00004
     END-IF.
     IF       R00004  NOT  =  0  AND  9
              MOVE      3         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF R00004
              MOVE     "R"        TO   EDIT-OPTION OF R00004
              GO   TO   CHK-2-EXIT
     END-IF.
*店舗Ｃチェック
     IF       R00010  IS  NOT  NUMERIC
              MOVE      0         TO   R00010
     END-IF.
     MOVE     R00005         TO   TEN-F52.
     MOVE     R00010         TO   TEN-F011.
*\\  93.07.02 START \\
*    店舗コードが　０の時　店舗マスタのチェックは
*    行わない
     MOVE     ZERO           TO   INV-FLG.
     IF       R00010         NOT =     ZERO
              PERFORM        900-TEN-READ
        IF    INV-FLG  =  1
              MOVE      7         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF R00010
              MOVE     "R"        TO   EDIT-OPTION OF R00010
              GO   TO   CHK-2-EXIT
        ELSE
              MOVE     TEN-F03    TO   R00011
        END-IF
     ELSE
              MOVE     SPACE      TO   R00011
*--- 94.05.16 START ---*
              MOVE     SPACE      TO   TEN-F04
*--- 94.05.16  END  ---*
     END-IF.
*\\
*出場チェック
     MOVE     "20"           TO   JYO-F01.
     MOVE     R00012         TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       INV-FLG  =  1
              MOVE      8         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF R00012
              MOVE     "R"        TO   EDIT-OPTION OF R00012
              GO   TO   CHK-2-EXIT
     END-IF.
*伝場チェック
     MOVE     "20"           TO   JYO-F01.
     MOVE     R00013         TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       INV-FLG  =  1
              MOVE      9         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF R00013
              MOVE     "R"        TO   EDIT-OPTION OF R00013
              GO   TO   CHK-2-EXIT
     END-IF.
*注文_チェック
     MOVE     R00014         TO   WK-NUM-1.
     PERFORM  900-NUM-CONV.
     MOVE     WK-NUM-2       TO   R00014.
*注文日チェック
     IF       R00015  NOT NUMERIC
     OR       R00015  =  0
        MOVE  SYS-DATE       TO   R00015
     END-IF.
     MOVE     R00015         TO   CHK-DATE.
     PERFORM  900-DATE-CHECK.
     IF   ERR-FLG   NOT =  0
              MOVE     "C"        TO   EDIT-CURSOR OF R00015
              MOVE     "R"        TO   EDIT-OPTION OF R00015
              GO   TO   CHK-2-EXIT
     ELSE
              MOVE      CHK-DATE       TO   SAV-CYU-DATE
     END-IF.
*納品日チェック
     IF       R00016  NOT  =  0
         MOVE      R00016         TO   CHK-DATE
         PERFORM   900-DATE-CHECK
*94/06/18
         MOVE      CHK-DATE       TO   SAV-NOU-DATE
*
         IF   ERR-FLG   =  0
**************COMPUTE  WK-SYS-DATE = HENKAN-TI * 10000 + SYS-DATE
              IF   ACOS-DATE  <  WK-SYS-DATE
              AND  CHK-YYMM   <  ACOS-YYMM
                   MOVE      25        TO   ERR-FLG
                   MOVE     "C"        TO   EDIT-CURSOR OF R00016
                   MOVE     "R"        TO   EDIT-OPTION OF R00016
                   GO   TO   CHK-2-EXIT
              END-IF
         ELSE
              MOVE     "C"        TO   EDIT-CURSOR OF R00016
              MOVE     "R"        TO   EDIT-OPTION OF R00016
              GO   TO   CHK-2-EXIT
         END-IF
     END-IF.
*伝発チェック
     IF       R00020  IS  NOT  NUMERIC
              MOVE      0         TO   R00020
     END-IF.
     IF       R00020  NOT  =  0  AND  9
              MOVE      11        TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF R00020
              MOVE     "R"        TO   EDIT-OPTION OF R00020
              GO   TO   CHK-2-EXIT
     END-IF.
***  94.05.16 START ***
*出荷日チェック
     IF       R00021  IS  NOT  NUMERIC
              MOVE      0         TO   R00021
              MOVE      0         TO   SAV-SYU-DATE
     END-IF.
**** IF       R00021       =  ZERO
****     MOVE R00016       TO     R00021
**** END-IF
*
     IF       R00021  NOT  =  0
         MOVE      R00021         TO   CHK-DATE
         PERFORM   900-DATE-CHECK
         IF   ERR-FLG   =  0
              MOVE     CHK-DATE   TO   SAV-SYU-DATE
**************COMPUTE  WK-SYS-DATE = HENKAN-TI * 10000 + SYS-DATE
              IF   ACOS-DATE  <  WK-SYS-DATE
              AND  CHK-YYMM   <  ACOS-YYMM
                   MOVE      33        TO   ERR-FLG
                   MOVE     "C"        TO   EDIT-CURSOR OF R00021
                   MOVE     "R"        TO   EDIT-OPTION OF R00021
                   GO   TO   CHK-2-EXIT
              END-IF
         ELSE
              MOVE     "C"        TO   EDIT-CURSOR OF R00021
              MOVE     "R"        TO   EDIT-OPTION OF R00021
              GO   TO   CHK-2-EXIT
         END-IF
     END-IF.
*94/06/18 START *
*納品日が締日以降の場合出荷日を前月には出来ない
     MOVE     ACOS-YYMM      TO   SAV-ACOS-YYMM.
**** COMPUTE  SAV-ACOS-MMS   =
****          SAV-ACOS-MM    -    1.
***  IF       SAV-ACOS-MMS   <    ZERO
****          MOVE     1     TO   SAV-ACOS-MM
****          SUBTRACT 1     FROM SAV-ACOS-YY
**** ELSE
****          MOVE SAV-ACOS-MMS   TO  SAV-ACOS-MM
*****END-IF.
*
**** DISPLAY "SAV-ACOS-YYMM   " SAV-ACOS-YYMM UPON CONS.
**** DISPLAY "SAV-NOU-DATE    " SAV-NOU-DATE  UPON CONS.
**** DISPLAY "ACOS-DATE       " ACOS-DATE     UPON CONS.
**** DISPLAY "CHK-DATE        " CHK-DATE      UPON CONS.
*
*
     IF       SAV-NOU-DATE   >    ACOS-DATE
          IF  SAV-ACOS-YYMM  >    CHK-DATE(1:6)
              MOVE      35        TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF R00021
              MOVE     "R"        TO   EDIT-OPTION OF R00021
          END-IF
     END-IF.
*94/06/18 END *
*
     IF  R00016   NOT =      ZERO
      IF R00021   >  R00016
         MOVE      34        TO   ERR-FLG
         MOVE     "C"        TO   EDIT-CURSOR OF R00021
         MOVE     "R"        TO   EDIT-OPTION OF R00021
      END-IF
     END-IF.
***  94.05.16  END  ***
 CHK-2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＢＯＤＹ１　入力　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-GBODY1-RTN     SECTION.
     IF       TOK-F82  >  6
              IF   P-CNT  =  1
                   MOVE      G003      TO   GUIDE
              ELSE
                   MOVE      G004      TO   GUIDE
              END-IF
     ELSE
              MOVE      G002      TO   GUIDE
     END-IF.
     MOVE     "GBODY1"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
     WHEN     PF06
              MOVE      4         TO   GR-NO
     WHEN     PF11
              IF   P-CNT  =  1
              OR   TOK-F82  NOT  >  6
                   MOVE      1         TO   ERR-FLG
              ELSE
                   PERFORM  CLR-BODY-RTN
                   PERFORM  VARYING  L  FROM 1  BY  1
                            UNTIL   L  >  6
                            OR      ERR-FLG  NOT = ZERO
                     PERFORM   CHK-BODY-RTN
                   END-PERFORM
                   PERFORM   CHK-BODY1-RTN
                   IF   ERR-FLG  =  ZERO
                      PERFORM   CHK-TAIL-RTN
                      IF   ERR-FLG  =  ZERO
                        MOVE      1         TO   P-CNT
                        PERFORM   900-TBL-TO-DSP
                      END-IF
                   END-IF
              END-IF
     WHEN     PF12
              IF   P-CNT  =  2
              OR   TOK-F82  NOT  >  6
                   MOVE      1         TO   ERR-FLG
              ELSE
                   PERFORM  CLR-BODY-RTN
                   PERFORM  VARYING  L  FROM 1  BY  1
                            UNTIL   L  >  6
                            OR      ERR-FLG  NOT = ZERO
                     PERFORM   CHK-BODY-RTN
                   END-PERFORM
                   PERFORM   CHK-BODY1-RTN
                   IF   ERR-FLG  =  ZERO
                      PERFORM   CHK-TAIL-RTN
                      IF   ERR-FLG  =  ZERO
                        MOVE      2         TO   P-CNT
                        PERFORM   900-TBL-TO-DSP
                      END-IF
                   END-IF
              END-IF
     WHEN     ENT
              PERFORM  CLR-BODY-RTN
              PERFORM  VARYING  L  FROM 1  BY  1
                       UNTIL   L  >  6
                       OR      ERR-FLG  NOT = ZERO
                PERFORM   CHK-BODY-RTN
              END-PERFORM
              PERFORM   CHK-BODY1-RTN
              IF   ERR-FLG  =  ZERO
                 PERFORM   CHK-TAIL-RTN
                 IF   ERR-FLG  =  ZERO
                   MOVE      "Y"       TO   KKNN
                   MOVE      90        TO   GR-NO
                 END-IF
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 DSP-GBODY1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  3       ＢＯＤＹ　チェック　　　　 　　　　　　　*
*--------------------------------------------------------------*
 CHK-BODY-RTN         SECTION.
*テーブル添字算出
     IF     P-CNT  =  1
            MOVE      L      TO    T
     ELSE
            COMPUTE   T   =  L  +  6
     END-IF.
*行削除チェック
     IF     R00001 = 1 AND R00002 = ZERO AND R10101(L) = "9"
            MOVE     SPACE      TO   R10101(L)
     END-IF.
     IF     R10101(L)  NOT  =  SPACE  AND  "1" AND  "9"
            MOVE      12        TO   ERR-FLG
            MOVE     "C"        TO   EDIT-CURSOR OF R10101(L)
            MOVE     "R"        TO   EDIT-OPTION OF R10101(L)
            GO   TO   CHK-BODY-EXIT
     END-IF.
*
     MOVE      R10101(L) TO   TBL-R10101(T).
     IF   R10101(L)  =  "9"
        MOVE      NC"削除"       TO   R11001(L)  TBL-R11001(T)
        GO   TO   CHK-BODY-EXIT
     ELSE
        MOVE      SPACE          TO   R11001(L)  TBL-R11001(T)
     END-IF.
*量販商品コードチェック
     IF       R10301 (L)  =  SPACE
     AND      R10401 (L)  =  SPACE
     AND      R10501 (L)  =  SPACE
     AND      R11501 (L)  =  SPACE
            MOVE      TBL-R10201(T)  TO   WK-R10201
            MOVE      SPACE          TO   TBL (T)
            INITIALIZE                    TBL (T)
            MOVE      SPACE          TO   TBL-X (T)
            INITIALIZE                    TBL-X (T)
            MOVE      WK-R10201      TO   TBL-R10201 (T)
            GO   TO   CHK-BODY-EXIT
     END-IF.
*商品ＣＤ数字変換
*８桁　頭０詰
     IF  R10101(L)     = "1"
     AND R10301(L) NOT NUMERIC
     AND R10301(L) NOT = SPACE
         MOVE   R10301(L)  TO  WK-R103I-X
         MOVE   ZERO       TO  J  HEN-FLG
         PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
                                          OR HEN-FLG NOT = 0
           IF  WK-R103I(I)  NUMERIC
               ADD    1   TO  J
               MOVE   WK-R103I(I)  TO  WK-R103O(J)
           ELSE
             IF  WK-R103I(I)  NOT = SPACE
               MOVE   1   TO  HEN-FLG
             END-IF
           END-IF
         END-PERFORM
         IF  HEN-FLG = ZERO
           MOVE   ZERO             TO  WK-R103
           COMPUTE I = 9 - J
           MOVE   WK-R103O-X(1:J)  TO  WK-R103(I:J)
           MOVE   WK-R103          TO  R10301(L)
         END-IF
     END-IF.
*５桁　頭空白詰
     IF  R10101(L)     = "1"
     AND R10401(L)  NOT = SPACE
     AND R10401(L)(5:1) = SPACE
         MOVE   R10401(L)  TO  WK-R104I-X
         MOVE   ZERO       TO  J
         PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
           IF  WK-R104I(I)  NOT = SPACE
               ADD    1   TO  J
               MOVE   WK-R104I(I)  TO  WK-R104O(J)
           END-IF
         END-PERFORM
         MOVE   SPACE            TO  WK-R104
         COMPUTE I = 6 - J
         MOVE   WK-R104O-X(1:J)  TO  WK-R104(I:J)
         MOVE   WK-R104          TO  R10401(L)
     END-IF.
*
***  93/05/08  START  ***
     MOVE     TBL-SHOCD(T)       TO  XSAV-SHOCD (T).
***  93/05/08  END    ***
     MOVE     SPACE     TO   TBL-SHOCD(T).
*送料チェック
     IF       R10301(L)  =  SORYO-CD
     AND      R10401(L)  =  SPACE
     AND      R10501(L)  =  SPACE
     AND      R11501(L)  =  SPACE
            MOVE      0         TO   R10601(L)
            MOVE      SPACE     TO   R10701(L)
            MOVE      0         TO   R10901(L)
            MOVE      "ｿｳﾘｮｳ"   TO   R11101(L)
            MOVE      SPACE     TO   R11201(L)
            MOVE      SPACE     TO   R11301(L)
            MOVE      SPACE     TO   R11401(L)
            MOVE      0         TO   MEI-F041
     END-IF.
*削除ＦＬＧ＝空白
     IF       R10301(L)  NOT =  SORYO-CD
     AND    ( R10101(L)      =  SPACE      OR   "1")
       MOVE      R00005    TO   STB-F01
       MOVE      R10301(L) TO   STB-F02 (1:8)
       MOVE      R10401(L) TO   STB-F02 (9:5)
       PERFORM   900-STB-READ
       IF   INV-FLG  =  1
            MOVE R10301(L) TO   MEI-F011
            MOVE R10401(L) TO   MEI-F012(1:5)
            MOVE R10501(L) TO   MEI-F012(6:2)
            MOVE R11501(L) TO   MEI-F012(8:1)
            PERFORM   900-MEI-READ
            IF   INV-FLG  =  1
              MOVE     "C"   TO   EDIT-CURSOR OF R10301(L)
              MOVE     "R"   TO   EDIT-OPTION OF R10301(L)
              MOVE     "R"   TO   EDIT-OPTION OF R10401(L)
              MOVE     "R"   TO   EDIT-OPTION OF R10501(L)
              MOVE     "R"   TO   EDIT-OPTION OF R11501(L)
              MOVE      13   TO   ERR-FLG
              GO   TO   CHK-BODY-EXIT
            END-IF
***   93/05/24  START   ***
            MOVE      R00005    TO   SHT-F01
            MOVE      R10301(L) TO   SHT-F031
            MOVE      R10401(L) TO   SHT-F032(1:5)
            MOVE      R10501(L) TO   SHT-F032(6:2)
            MOVE      R11501(L) TO   SHT-F032(8:1)
            PERFORM   SHT-READ-RTN
*\\
            IF    INV-FLG       =    1
                  MOVE      SPACE     TO   TBL-TANABN(T)
            ELSE
                  MOVE      SHT-F08   TO   TBL-TANABN(T)
            END-IF
            MOVE      R10301(L) TO   TBL-JHINCD(T)
            MOVE R10401(L) TO   TBL-HINTAN(T)(1:5)
            MOVE R10501(L) TO   TBL-HINTAN(T)(6:2)
            MOVE R11501(L) TO   TBL-HINTAN(T)(8:1)
*\\
***   93/05/24    END   ***
            IF  (R10301(L) NOT = TBL-R10301(T)
             OR  R10401(L) NOT = TBL-R10401(T)
             OR  R10501(L) NOT = TBL-R10501(T)
             OR  R11501(L) NOT = TBL-R11501(T))
                 EVALUATE  R10701(L)
                   WHEN     " "
                      COMPUTE  R10801(L)  =  MEI-F042  *  TOK-F55
                      COMPUTE  R10901(L)  =  MEI-F043  *  TOK-F55
                   WHEN     "1"
                   WHEN     "3"
                    IF  R10801(L)  NOT NUMERIC
                    OR  R10801(L)  = ZERO
                      COMPUTE  R10801(L)  =  MEI-F042  *  TOK-F55
                    END-IF
                    IF  R10901(L)  NOT NUMERIC
                    OR  R10901(L)  = ZERO
                      COMPUTE  R10901(L)  =  MEI-F043  *  TOK-F55
                    END-IF
                 END-EVALUATE
                 IF  (MEI-F93 = ZERO)
                 OR  (MEI-F93 = 1
                 AND  R11101(L) = SPACE
                 AND  R11201(L) = SPACE)
                   MOVE MEI-F031  TO   R11101(L)
                   MOVE MEI-F032  TO   R11201(L)
                 END-IF
            END-IF
            COMPUTE   WK-R10801  =  MEI-F042  *  TOK-F55
            COMPUTE   WK-R10901  =  MEI-F043  *  TOK-F56
       ELSE
            MOVE      STB-F031       TO   MEI-F011
            MOVE      STB-F032       TO   MEI-F012
            PERFORM   900-MEI-READ
            IF   INV-FLG  =  1
              MOVE      13   TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR OF R10301(L)
              MOVE     "R"   TO   EDIT-OPTION OF R10301(L)
              MOVE     "R"   TO   EDIT-OPTION OF R10401(L)
              MOVE     "R"   TO   EDIT-OPTION OF R10501(L)
              MOVE     "R"   TO   EDIT-OPTION OF R11501(L)
              GO   TO   CHK-BODY-EXIT
            END-IF
            IF (R10301(L) NOT = TBL-R10301(T)
            OR  R10401(L) NOT = TBL-R10401(T)
            OR  R10501(L) NOT = TBL-R10501(T)
            OR  R11501(L) NOT = TBL-R11501(T))
                 EVALUATE  R10701(L)
                   WHEN     " "
                        MOVE      STB-F05   TO   R10801(L)
                        MOVE      STB-F06   TO   R10901(L)
                   WHEN     "1"
                   WHEN     "3"
                    IF  R10801(L)  NOT NUMERIC
                    OR  R10801(L)  = ZERO
                        MOVE      STB-F05   TO   R10801(L)
                    END-IF
                    IF  R10901(L)  NOT NUMERIC
                    OR  R10901(L)  = ZERO
                        MOVE      STB-F06   TO   R10901(L)
                    END-IF
                 END-EVALUATE
                 IF  (MEI-F93 = ZERO)
                 OR  (MEI-F93 = 1
                 AND  R11101(L) = SPACE
                 AND  R11201(L) = SPACE)
                   MOVE      MEI-F031  TO   R11101(L)
                   MOVE      MEI-F032  TO   R11201(L)
                 END-IF
            END-IF
***** TEST *******
*********** DISPLAY   "1 = " TBL-SHOCD (T) UPON CONS
***** TEST *******

            MOVE      STB-F031  TO   TBL-SHOCD(T)(1:8)
            MOVE      STB-F032  TO   TBL-SHOCD(T)(9:8)
            MOVE      STB-F05   TO   WK-R10801
            MOVE      STB-F06   TO   WK-R10901
***   93/05/24  START   ***
            MOVE      STB-F08   TO   TBL-TANABN(T)
            MOVE      STB-F031  TO   TBL-JHINCD(T)
            MOVE      STB-F032  TO   TBL-HINTAN(T)
***   93/05/24    END   ***
***  93/05/08  START  ***
            IF        MEI-F93   =    1
                 IF   XSAV-SHOCD(T)  NOT =     SPACE
                      MOVE XSAV-SHOCD(T) TO TBL-SHOCD(T)
                 END-IF
            END-IF
*
*******     DISPLAY   "2 = " TBL-SHOCD (T) UPON CONS
***  93/05/08  END    ***
       END-IF
     END-IF.
*削除ＦＬＧ＝１
     IF       R10301(L)  NOT =  SORYO-CD
     AND      R10101(L)      =  "1"
            MOVE R10301(L) TO   MEI-F011
            MOVE R10401(L) TO   MEI-F012(1:5)
            MOVE R10501(L) TO   MEI-F012(6:2)
            MOVE R11501(L) TO   MEI-F012(8:1)
            PERFORM   900-MEI-READ
            IF   INV-FLG  =  1
              MOVE     "C"   TO   EDIT-CURSOR OF R10301(L)
              MOVE     "R"   TO   EDIT-OPTION OF R10301(L)
              MOVE     "R"   TO   EDIT-OPTION OF R10401(L)
              MOVE     "R"   TO   EDIT-OPTION OF R10501(L)
              MOVE     "R"   TO   EDIT-OPTION OF R11501(L)
              MOVE      13   TO   ERR-FLG
              GO   TO   CHK-BODY-EXIT
            END-IF
            IF  (R10301(L) NOT = TBL-R10301(T)
             OR  R10401(L) NOT = TBL-R10401(T)
             OR  R10501(L) NOT = TBL-R10501(T)
             OR  R11501(L) NOT = TBL-R11501(T))
                 EVALUATE  R10701(L)
                   WHEN     " "
                      COMPUTE  R10801(L)  =  MEI-F042  *  TOK-F55
                      COMPUTE  R10901(L)  =  MEI-F043  *  TOK-F55
                   WHEN     "1"
                   WHEN     "3"
                    IF  R10801(L)  NOT NUMERIC
                    OR  R10801(L)  = ZERO
                      COMPUTE  R10801(L)  =  MEI-F042  *  TOK-F55
                    END-IF
                    IF  R10901(L)  NOT NUMERIC
                    OR  R10901(L)  = ZERO
                      COMPUTE  R10901(L)  =  MEI-F043  *  TOK-F55
                    END-IF
                 END-EVALUATE
                 IF  (MEI-F93 = ZERO)
                 OR  (MEI-F93 = 1
                 AND  R11101(L) = SPACE
                 AND  R11201(L) = SPACE)
                   MOVE MEI-F031  TO   R11101(L)
                   MOVE MEI-F032  TO   R11201(L)
                 END-IF
            END-IF
            COMPUTE   WK-R10801  =  MEI-F042  *  TOK-F55
            COMPUTE   WK-R10901  =  MEI-F043  *  TOK-F56
     END-IF.

     MOVE      R10301(L) TO   TBL-R10301(T).
     MOVE      R10401(L) TO   TBL-R10401(T).
     MOVE      R10501(L) TO   TBL-R10501(T).
     MOVE      R11501(L) TO   TBL-R11501(T).
     MOVE      R11101(L) TO   TBL-R11101(T).
     MOVE      R11201(L) TO   TBL-R11201(T).
     MOVE      MEI-F041  TO   TBL-SIIGEN(T).
*数量　チェック
     IF     R10601(L)  IS  NOT  NUMERIC
            MOVE      0         TO   R10601(L)
     END-IF.
     MOVE     R10601(L) TO   TBL-R10601(T).
*単価区分　チェック
     IF     R10701(L)  NOT  =  " "  AND  "1"  AND  "3"
                               AND  "4"  AND  "5"  AND  "6"
                               AND  "7"
            MOVE      14   TO   ERR-FLG
            MOVE     "C"   TO   EDIT-CURSOR OF R10701(L)
            MOVE     "R"   TO   EDIT-OPTION OF R10701(L)
            GO   TO   CHK-BODY-EXIT
     END-IF.
*********
** 98/07/16 TAKAHASHI ﾍﾝｺｳ START**
     IF       R10701(L)  =  SPACE
        IF    R10301(L)  =  SORYO-CD
        AND   R10401(L)  =  SPACE
        AND   R10501(L)  =  SPACE
        AND   R11501(L)  =  SPACE
              CONTINUE
        ELSE
              MOVE      WK-R10801      TO   R10801(L)
              MOVE      WK-R10901      TO   R10901(L)
        END-IF
     END-IF.
** 98/07/16 TAKAHASHI ﾍﾝｺｳ END  **
     IF       R10701(L)  =  "4"  OR  "5"  OR  "6"  OR  "7"
              MOVE      ZERO           TO   R10801(L)
                                            R10901(L)
     END-IF.
*原価単価　チェック
     IF     ( R10701(L)       =  " "  OR   "1"  OR   "3" )
        AND ( R10801(L)       =  ZERO )
              MOVE      28        TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR OF R10801(L)
              MOVE     "R"   TO   EDIT-OPTION OF R10801(L)
              GO   TO   CHK-BODY-EXIT
     END-IF.
*
     MOVE      R10701(L) TO   TBL-R10701(T).
     MOVE      R10801(L) TO   TBL-R10801(T).
     MOVE      R10901(L) TO   TBL-R10901(T).
     MOVE      R11301(L) TO   TBL-R11301(T).
     MOVE      R11401(L) TO   TBL-R11401(T).
 CHK-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ１　　チェック　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-BODY1-RTN         SECTION.
*合計算出
     PERFORM  DSP-GOKEI-RTN.
*量販商品コードチェック
     PERFORM  VARYING   I FROM 1 BY 1 UNTIL I > 6
       IF       R10301 (I)  =  SPACE
       AND      R10401 (I)  =  SPACE
       AND      R10501 (I)  =  SPACE
       AND      R11501 (I)  =  SPACE
            IF  R10201(I)  NUMERIC
              MOVE      R10201(I)     TO    WK-R10201
            ELSE
              MOVE      ZERO          TO    WK-R10201
            END-IF
            MOVE      SPACE     TO   BDY(I)
            MOVE     "M"        TO   EDIT-OPTION OF R10101(I)
                                     EDIT-OPTION OF R10201(I)
                                     EDIT-OPTION OF R10301(I)
                                     EDIT-OPTION OF R10401(I)
                                     EDIT-OPTION OF R10501(I)
                                     EDIT-OPTION OF R11501(I)
                                     EDIT-OPTION OF R10601(I)
                                     EDIT-OPTION OF R10701(I)
                                     EDIT-OPTION OF R10801(I)
                                     EDIT-OPTION OF R10901(I)
                                     EDIT-OPTION OF R11001(I)
                                     EDIT-OPTION OF R11101(I)
                                     EDIT-OPTION OF R11201(I)
                                     EDIT-OPTION OF R11301(I)
                                     EDIT-OPTION OF R11401(I)
            IF  WK-R10201 NOT = ZERO
              MOVE      WK-R10201      TO   R10201(I)
            END-IF
       END-IF
     END-PERFORM.
 CHK-BODY1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　　　チェック　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-TAIL-RTN         SECTION.
*属性クリア
     PERFORM  CLR-TAIL-RTN.
*明細入力　チェック
     IF       R00001      =  1
     AND      R00002      =  0
       PERFORM  VARYING   I FROM 1 BY 1 UNTIL I > 10
          IF  TBL-R10301 (I)  NOT =  SPACE
              MOVE  90    TO   I
          END-IF
       END-PERFORM
       IF   I  <   90
            MOVE      29             TO   ERR-FLG
            GO   TO   CHK-TAIL-EXIT
       END-IF
     END-IF.
*摘要コード　チェック
     IF       R00023  IS  NOT  NUMERIC
              MOVE    ZERO        TO   R00023
     END-IF.
     IF       R00023  =   ZERO
              MOVE      SPACE     TO   R00024
              MOVE      SPACE     TO   R00025
     END-IF.
*
     IF       R00023  NOT  =  ZERO   AND  99
              MOVE      "80"           TO   JYO-F01
              MOVE      R00023         TO   JYO-F02
              PERFORM   JYO-RD-RTN
              IF   INV-FLG  =  1
                   MOVE     "C"   TO   EDIT-CURSOR OF R00023
                   MOVE     "R"   TO   EDIT-OPTION OF R00023
                   MOVE      17   TO   ERR-FLG
                   GO   TO   CHK-TAIL-EXIT
              END-IF
              IF   R00024  =  SPACE
              AND  R00025  =  SPACE
              OR   R00023  NOT = WK-R00023
                   MOVE      JYO-F14   TO   R00024
                   MOVE      JYO-F15   TO   R00025
              END-IF
     END-IF.
     MOVE     R00023          TO   WK-R00023.
*
*摘要名チェック
*
*送料チェック
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  10
              IF   TBL-R10301 (I)  =  SORYO-CD
              AND  TBL-R10401 (I)  =  SPACE
              AND  TBL-R10501 (I)  =  SPACE
              AND  TBL-R11501 (I)  =  SPACE
                   MOVE      0         TO   R00028
              END-IF
     END-PERFORM.
*
     IF       R00028  IS  NOT  NUMERIC
              MOVE      0         TO   R00028
     END-IF.
*
 CHK-TAIL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　入力　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-KKNN-RTN      SECTION.
*****IF       R00001  =  3  OR  9
*****AND      TOK-F82  >  6
     IF       TOK-F82  >  6
              IF   P-CNT  =  1
                   MOVE      G003      TO   GUIDE
              ELSE
                   MOVE      G004      TO   GUIDE
              END-IF
     ELSE
              MOVE      G002           TO   GUIDE
     END-IF.
     MOVE     "KKNN"         TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
     EVALUATE DSP-FNC
     WHEN     PF04
              MOVE      0         TO   GR-NO
     WHEN     PF06
              IF ( R00001  =  1
              AND  R00002  =  1  OR  3  OR  5  OR  7 )
              OR ( R00001  =  3  OR  9 )
                   MOVE      3         TO   GR-NO
              ELSE
                   MOVE      5         TO   GR-NO
              END-IF
     WHEN     PF11
*94.05.16*****IF   R00001  =  3  OR  9
**************AND  TOK-F82  >  6
              IF   TOK-F82  >  6
              AND  P-CNT  =  2
                   MOVE      1         TO   P-CNT
                   PERFORM   900-TBL-TO-DSP
              ELSE
                   MOVE      1         TO   ERR-FLG
              END-IF
     WHEN     PF12
*94.05.16*****IF   R00001  =  3  OR  9
**************AND  TOK-F82  >  6
              IF   TOK-F82  >  6
              AND  P-CNT  =  1
                   MOVE      2         TO   P-CNT
                   PERFORM   900-TBL-TO-DSP
              ELSE
                   MOVE      1         TO   ERR-FLG
              END-IF
     WHEN     ENT
              PERFORM   CHK-KKNN-RTN
*##### 98/09/18 追加 START #####
************* DISPLAY "DEN-KAKU-CHK = " DEN-KAKU-CHK UPON CONS
              IF  DEN-KAKU-CHK  =  1
                  PERFORM   DENKU-ERR-RTN
              END-IF
*##### 98/09/18 追加 END   #####
              IF   ERR-FLG  =  ZERO
                   IF  R00001  NOT = 9
                       PERFORM   DEN-UPDT-RTN
                   END-IF
                   PERFORM   KKNN-AFTER-RTN
              END-IF
*##### 00/02/14 追加 START #####
              MOVE      1         TO   P-CNT
*##### 00/02/14 追加 END   #####
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　　　　チェック　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-KKNN-RTN     SECTION.
     MOVE     "M"        TO   EDIT-OPTION OF KKNN.
     IF       KKNN  NOT  =  "Y"  AND  "H"  AND  "B"
     AND      R00001  =  1
     AND      R00002  =  0
              MOVE      15        TO   ERR-FLG
              MOVE     "R"        TO   EDIT-OPTION OF KKNN
              GO   TO   CHK-KKNN-EXIT
     END-IF.
     IF       KKNN  NOT  =  "Y"
     AND    ( R00001  NOT  =  1
       OR   ( R00001  =  1
         AND  R00002  NOT  =  0 ))
              MOVE      16        TO   ERR-FLG
              MOVE     "R"        TO   EDIT-OPTION OF KKNN
              GO   TO   CHK-KKNN-EXIT
     END-IF.
 CHK-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      伝票　更新　処理　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DEN-UPDT-RTN          SECTION.
     EVALUATE   R00001
         WHEN  1
         WHEN  2
              PERFORM   900-DEN-DELETE
              PERFORM   900-DEN-WRITE
         WHEN  3
              PERFORM   900-DEN-DELETE
     END-EVALUATE.
 DEN-UPDT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認後画面　処理　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 KKNN-AFTER-RTN          SECTION.
*
     IF       KKNN  =  "Y"
     AND   (  R00001  NOT  =  1
       OR   ( R00001  =  1
        AND   R00002  =  0  OR  2  OR  4  OR  6  OR  8 ))
              MOVE      R00001    TO   SAV-R00001
              MOVE      R00015    TO   SAV-R00015
              MOVE      R00016    TO   SAV-R00016
***  94.05.16 START  ***
              MOVE      R00021    TO   SAV-R00021
***  94.05.16  END   ***
              MOVE      SPACE     TO   FSY01011  TABLE-AREA
              INITIALIZE               TBL-XX
              PERFORM   VARYING  I  FROM  1  BY  1  UNTIL I  >  10
                        IF   I  <  7
                             MOVE      I         TO   R10201 (I)
                        END-IF
                        MOVE      I         TO   TBL-R10201 (I)
              END-PERFORM
              MOVE      WK-SYSYMD      TO   R00000
              MOVE      SAV-R00001     TO   R00001
              MOVE      SAV-R00015     TO   R00015
              MOVE      SAV-R00016     TO   R00016
***  94.05.16 START  ***
              MOVE      SAV-R00021     TO   R00021
***  94.05.16  END   ***
              MOVE      2              TO   GR-NO
              GO   TO   KKNN-AFTER-EXIT
     END-IF.
     IF       KKNN  =  "Y"
     AND      R00001  =  1
     AND      R00002  =  1  OR  3  OR  5  OR  7
              ADD       1         TO   R00002
              MOVE      5         TO   GR-NO
              GO   TO   KKNN-AFTER-EXIT
     END-IF.
     IF       KKNN  =  "H"
              MOVE      HED       TO   SAV-HED
              MOVE      SPACE     TO   FSY01011
                                       TABLE-AREA
              INITIALIZE               TBL-XX
              PERFORM   VARYING  I  FROM  1  BY  1  UNTIL I  >  10
                        IF   I  <  7
                             MOVE      I         TO   R10201 (I)
                        END-IF
                        MOVE      I         TO   TBL-R10201 (I)
              END-PERFORM
              MOVE      SAV-HED   TO   HED
              PERFORM   900-DENNO-GET
              MOVE      1         TO   P-CNT
              MOVE      5         TO   GR-NO
     END-IF.
     IF       KKNN  =  "B"
              MOVE      R00001         TO   SAV-R00001
              MOVE      R00015         TO   SAV-R00015
              MOVE      R00016         TO   SAV-R00016
***  94.05.16 START  ***
              MOVE      R00021         TO   SAV-R00021
***  94.05.16  END   ***
              MOVE      SPACE          TO   HED
              MOVE      WK-SYSYMD      TO   R00000
              MOVE      SAV-R00001     TO   R00001
              MOVE      SAV-R00015     TO   R00015
              MOVE      SAV-R00016     TO   R00016
***  94.05.16 START  ***
              MOVE      SAV-R00021     TO   R00021
***  94.05.16  END   ***
              MOVE      1              TO   P-CNT
              MOVE      2              TO   GR-NO
     END-IF.
 KKNN-AFTER-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     伝票データ登録・修正　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 900-DEN-WRITE          SECTION.
*明細行更新
     MOVE          1         TO   J.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  TOK-F82
***   93/05/24  START   ***
       IF   TBL-R10101(I)       =  "9"
         IF R00008     =        40 OR 41
                PERFORM   ZAI-REWRITE-RTN
         END-IF
       ELSE
            IF  (TBL-R10301(I)  NOT  =  SPACE
            OR   TBL-R10401(I)  NOT  =  SPACE
            OR   TBL-R10501(I)  NOT  =  SPACE
            OR   TBL-R11501(I)  NOT  =  SPACE )
              MOVE      0         TO   ZAI-FLG
              IF R00008 =       40 OR 41
                  PERFORM   ZAI-REWRITE-RTN
              END-IF
              INITIALIZE     DEN-REC
              MOVE TBL-R10201(I)  TO   DEN-F03
              PERFORM   900-DSP-TO-DEN
              WRITE     DEN-REC
            END-IF
       END-IF
***   93/05/24    END   ***
     END-PERFORM.
*  摘要更新
     IF       R00024  NOT  =  SPACE
     OR       R00025  NOT  =  SPACE
     OR     ( R00023  IS  NUMERIC
       AND    R00023  NOT  =  ZERO )
              INITIALIZE     DEN-REC
              MOVE      80             TO   DEN-F03
              MOVE      R00023         TO   DEN-F1411 (1:2)
              MOVE      R00024         TO   DEN-F1421
              MOVE      R00025         TO   DEN-F1422
              IF        R00001    =    2
                        MOVE      1    TO   DEN-F53
              END-IF
              PERFORM   900-DSP-TO-DEN-2
              WRITE     DEN-REC
     END-IF.
*  送料更新
     IF     ( R00028  IS  NUMERIC
       AND    R00028  NOT  =  ZERO )
              INITIALIZE     DEN-REC
              MOVE      90             TO   DEN-F03
              MOVE      "00999961"     TO   DEN-F1411
              MOVE      R00028         TO   DEN-F181
              EVALUATE  R00001
                  WHEN  1
                        MOVE      R00028         TO   DEN-F521
                  WHEN  2
                        MOVE      SAV-MAESORYO   TO   DEN-F521
                        MOVE      1              TO   DEN-F53
              END-EVALUATE
              PERFORM   900-DSP-TO-DEN-2
              WRITE     DEN-REC
     END-IF.
 900-DEN-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     伝票データ削除　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 900-DEN-DELETE         SECTION.
     MOVE     R00005         TO   DEN-F01.
     MOVE     R00007         TO   DEN-F02.
     MOVE     R00002         TO   DEN-F04 WK-R00002.
     MOVE     R00008         TO   DEN-F051.
     MOVE     0              TO   DEN-F03.
     MOVE     0              TO   END-FLG.
     START    SHTDENF  KEY  NOT  <  DEN-F01  DEN-F02  DEN-F04
                                    DEN-F051 DEN-F03
        INVALID  KEY
              MOVE      1        TO   END-FLG
        NOT   INVALID  KEY
              PERFORM  900-DEN-READ
     END-START.
***   93/05/24  START   ***
*    MOVE     1              TO  I.
     PERFORM  UNTIL  END-FLG  =  1
              IF  R00001      =  3
                  MOVE    DEN-F03    TO    I
                  IF     R00008  =    40 OR 41
                         PERFORM    ZAI-REWRITE-RTN
                  END-IF
              END-IF
              DELETE         SHTDENF  RECORD
              PERFORM  900-DEN-READ
*             ADD    1       TO  I
***   93/05/24    END   ***
     END-PERFORM.
 900-DEN-DELETE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     表示項目を伝票データに転送　　　　　　　　　 *
*--------------------------------------------------------------*
 900-DSP-TO-DEN         SECTION.
     PERFORM  900-DSP-TO-DEN-2.
     IF       TBL-SHOCD (I)  = SPACE
              MOVE      TBL-R10301 (I)      TO   DEN-F1411
              MOVE      TBL-R10401 (I)      TO   DEN-F1412 (1:5)
              MOVE      TBL-R10501 (I)      TO   DEN-F1412 (6:2)
              MOVE      TBL-R11501 (I)      TO   DEN-F1412 (8:1)
              MOVE      SPACE               TO   DEN-F25
     ELSE
              MOVE      TBL-SHOCD (I) (1:8) TO   DEN-F1411
              MOVE      TBL-SHOCD (I) (9:8) TO   DEN-F1412
              MOVE      TBL-R10301 (I)      TO   DEN-F25 (1:8)
              MOVE      TBL-R10401 (I)      TO   DEN-F25 (9:5)
     END-IF.
     MOVE     TBL-R11101 (I) TO   DEN-F1421.
     MOVE     TBL-R11201 (I) TO   DEN-F1422.
     MOVE     TBL-R10601 (I) TO   DEN-F15.
     MOVE     TBL-R10701 (I) TO   DEN-F16.
     MOVE     TBL-SIIGEN (I) TO   DEN-F171.
*
***  93/05/08  START  ***
     IF       TBL-R10701 (I) =    "3"
              MOVE     TBL-R10801 (I) TO   DEN-F181
              MOVE     TBL-R10901 (I) TO   DEN-F182
     ELSE
              MOVE     TBL-R10801 (I) TO   DEN-F172
              MOVE     TBL-R10901 (I) TO   DEN-F173
              COMPUTE  DEN-F181  =  DEN-F172  *  DEN-F15
              COMPUTE  DEN-F182  =  DEN-F173  *  DEN-F15
     END-IF.
*
     IF       TBL-R10701(I)  =    SPACE
         IF   R00001         =    1
              MOVE     "1"   TO   DEN-F16
         END-IF
     END-IF.
***  93/05/08  END    ***
     MOVE     0              TO   DEN-F19.
     COMPUTE  DEN-F20  =  DEN-F181  -  (DEN-F171 * DEN-F15).
     MOVE     TBL-R11301 (I) TO   DEN-F21.
     MOVE     TBL-R11401 (I) TO   DEN-F22.
***   93/05/24  START   ***
*在庫更新したなら引落フラグに１
     IF       ZAI-FLG        =    1
              MOVE     1     TO   DEN-F27C
     END-IF.
***   93/05/24    END   ***
*93.10.15
     IF       TBL-HIKIATE(I)  =   "1"
              MOVE     1     TO   DEN-F27D
     END-IF.
*
     IF  R00001    =    1
*        登録時、訂正前項目セット
         MOVE      TBL-R10601(I)      TO   DEN-F50
         IF   TBL-R10701 (I) =    "3"
              MOVE     TBL-R10801 (I) TO   DEN-F521
              MOVE     TBL-R10901 (I) TO   DEN-F522
         ELSE
              MOVE     TBL-R10801 (I) TO   DEN-F512
              MOVE     TBL-R10901 (I) TO   DEN-F513
              COMPUTE  DEN-F521  =  DEN-F172  *  DEN-F15
              COMPUTE  DEN-F522  =  DEN-F173  *  DEN-F15
         END-IF
     END-IF.
     IF  R00001    =    2
              MOVE      1              TO   DEN-F53
              IF        TBL-MAESUU(I)   NOT  NUMERIC
                        MOVE   ZERO          TO DEN-F50
              ELSE
                        MOVE   TBL-MAESUU(I) TO DEN-F50
              END-IF
              IF        TBL-MAEGTAN(I)   NOT  NUMERIC
                        MOVE   ZERO          TO DEN-F512
              ELSE
                        MOVE   TBL-MAEGTAN(I) TO DEN-F512
              END-IF
              IF        TBL-MAEBTAN(I)  NOT  NUMERIC
                        MOVE   ZERO          TO DEN-F513
              ELSE
                        MOVE   TBL-MAEBTAN(I) TO DEN-F513
              END-IF
              IF        TBL-MAEGKIN(I)  NOT  NUMERIC
                        MOVE   ZERO    TO   DEN-F521
              ELSE
                        MOVE   TBL-MAEGKIN(I) TO DEN-F521
              END-IF
              IF        TBL-MAEBKIN(I)  NOT  NUMERIC
                        MOVE   ZERO    TO   DEN-F522
              ELSE
                        MOVE   TBL-MAEBKIN(I) TO DEN-F522
              END-IF
*****         MOVE      TBL-MAESUU (I) TO   DEN-F50
*****         MOVE      TBL-MAEGTAN(I) TO   DEN-F512
*****         MOVE      TBL-MAEBTAN(I) TO   DEN-F513
*****         MOVE      TBL-MAEGKIN(I) TO   DEN-F521
*****         MOVE      TBL-MAEBKIN(I) TO   DEN-F522
     END-IF.
*
 900-DSP-TO-DEN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     表示項目を伝票データに転送　　　　　　　　　 *
*--------------------------------------------------------------*
 900-DSP-TO-DEN-2       SECTION.
     MOVE     R00005         TO   DEN-F01.
     MOVE     R00007         TO   DEN-F02.
     MOVE     R00002         TO   DEN-F04.
     MOVE     R00008         TO   DEN-F051.
     MOVE     R00009         TO   DEN-F052.
     MOVE     R00003         TO   DEN-F06.
     MOVE     R00010         TO   DEN-F07.
     MOVE     R00012         TO   DEN-F08.
     MOVE     R00013         TO   DEN-F09.
*****MOVE     R00014         TO   DEN-F10.
     MOVE     R00014         TO   DEN-F44.
*    注文日
*****COMPUTE  DEN-F111  =  HENKAN-TI * 10000 +  R00015.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     R00015    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   DEN-F111
     ELSE
         MOVE ZERO           TO   DEN-F111
     END-IF.
**** 納品日 ***
     IF       R00016  =  ZERO
              MOVE      0         TO   DEN-F112
     ELSE
**************COMPUTE   DEN-F112  =  HENKAN-TI * 10000 +  R00016
              MOVE     "3"        TO        LINK-IN-KBN
              MOVE      R00016    TO        LINK-IN-YMD6
              CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              IF   LINK-OUT-RET   =    ZERO
                   MOVE      LINK-OUT-YMD8  TO   DEN-F112
              ELSE
                   MOVE      ZERO           TO   DEN-F112
              END-IF
     END-IF.
**** 出荷日 ***
     IF       R00021  =  ZERO
              MOVE      0         TO   DEN-F113
     ELSE
**************COMPUTE   DEN-F113  =  HENKAN-TI * 10000 +  R00021
              MOVE     "3"        TO        LINK-IN-KBN
              MOVE      R00021    TO        LINK-IN-YMD6
              CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              IF   LINK-OUT-RET   =    ZERO
                   MOVE      LINK-OUT-YMD8  TO   DEN-F113
              ELSE
                   MOVE      ZERO           TO   DEN-F113
              END-IF
     END-IF.
***  94.05.16  END  ***
     MOVE     R00017         TO   DEN-F12.
     MOVE     R00018         TO   DEN-F131.
     MOVE     R00019         TO   DEN-F132.
     MOVE     R00004         TO   DEN-F133.
     MOVE     R00020         TO   DEN-F134.
     MOVE     TOK-F52        TO   DEN-F24.
     MOVE     SPACE          TO   DEN-F25.
     MOVE     0              TO   DEN-F28.
*****MOVE     HENKAN-TI      TO   DEN-F29.
     MOVE     0              TO   DEN-F98.
*****COMPUTE  DEN-F99  =  HENKAN-TI  *  10000  +  SYS-DATE.
     MOVE     WK-SYS-DATE    TO   DEN-F99.
     MOVE     TOK-F84        TO   DEN-F272.
     MOVE     TEN-F04        TO   DEN-F30.
     EVALUATE  R00001
        WHEN    "1"
           MOVE   R00007    TO  DEN-F23
           MOVE   TOK-F89   TO  DEN-F276
        WHEN    "2"
           MOVE   9        TO   DEN-F273
           MOVE   WK-F23    TO  DEN-F23
           MOVE   WK-F276   TO  DEN-F276
           MOVE   WK-F27B   TO  DEN-F27B
     END-EVALUATE.
 900-DSP-TO-DEN-2-EXIT.
     EXIT.
***   93/05/24  START   ***
*--------------------------------------------------------------*
*    LEVEL  4     商品在庫マスタ更新処理                       *
*--------------------------------------------------------------*
 ZAI-REWRITE-RTN         SECTION.
*登録時、納入日＞システム日付　ＯＲ
*修正・削除時、在庫引落フラグ≠１なら 更新しない
****** TEST ******
**** DISPLAY "R00001  =   " R00001 UPON CONS.
**** DISPLAY "R00021  =   " R00021 UPON CONS.
**** DISPLAY "SYS-D   =   " SYS-DATE   UPON CONS.
***  94.05.16 START  ***
**** IF     ( R00001             =    1           AND
****          R00021             >    SYS-DATE )  OR
****        ( R00001         NOT =    1           AND
****          DEN-F27C       NOT =    1 )         OR
****
****          R00021             =    ZERO
****
****          PERFORM    ZAI-YOTEI-RTN
****
****          GO   TO    ZAI-REWRITE-EXIT
**** END-IF.
     IF     ( R00001             =    1           AND
**************R00021             >    SYS-DATE )
              SAV-SYU-DATE       >    WK-SYS-DATE )
              PERFORM    ZAI-YOTEI-RTN
              GO   TO    ZAI-REWRITE-EXIT
     END-IF.
*
     IF     ( R00001         NOT =    1           AND
              DEN-F27C       NOT =    1 )
              PERFORM    ZAI-YOTEI-RTN
              GO   TO    ZAI-REWRITE-EXIT
     END-IF.
*
     IF       R00021             =    ZERO
              PERFORM    ZAI-YOTEI-RTN
              GO   TO    ZAI-REWRITE-EXIT
     END-IF.
* 94.08.02
*
     IF       R00001         NOT =    2
              GO   TO    ZAI-REWRITE-01
     END-IF.
*\\
     IF       TBL-R10101(I)    =    "9"
              GO   TO    ZAI-REWRITE-01
     END-IF.
*
     IF       TBL-WTANA (I)      =    SPACE   AND
              TBL-WHINCD(I)      =    SPACE   AND
              TBL-WHTAN (I)      =    SPACE
              GO   TO    ZAI-REWRITE-01
     END-IF.
*修正前更新処理
     IF       R00021             =    TBL-SYUKKA(I)      AND
              R00012             =    TBL-WSOKCD(I)      AND
              TBL-TANABN(I)      =    TBL-WTANA (I)      AND
              TBL-JHINCD(I)      =    TBL-WHINCD(I)      AND
*\\
              TBL-HINTAN(I)      =    TBL-WHTAN (I)
*
************* TBL-R10401(I)      =    TBL-WHTAN (I)(1:5) AND
************* TBL-R10501(I)      =    TBL-WHTAN (I)(6:2) AND
************* TBL-R11501(I)      =    TBL-WHTAN (I)(8:1)
*
          CONTINUE
     ELSE
          MOVE     TBL-WSOKCD(I)       TO     ZAI-F01
          MOVE     TBL-WHINCD(I)       TO     ZAI-F021
          MOVE     TBL-WHTAN (I)       TO     ZAI-F022
          MOVE     TBL-WTANA (I)(1:3)  TO     ZAI-F031
          MOVE     TBL-WTANA (I)(4:1)  TO     ZAI-F032
          MOVE     TBL-WTANA (I)(5:2)  TO     ZAI-F033
          PERFORM  ZAI-READ-RTN
          IF       INV-FLG        =   0
              IF   R00002  =  0  OR  2  OR  4  OR  6  OR  8
                   IF   R00008    =   40
                        PERFORM        ZAI-WIN-RTN
                   ELSE
                        PERFORM        ZAI-WOUT-RTN
                   END-IF
              ELSE
                   IF   R00008    =   40
                        PERFORM        ZAI-WOUT-RTN
                   ELSE
                        PERFORM        ZAI-WIN-RTN
                   END-IF
              END-IF
              REWRITE  ZAI-REC
          ELSE
              MOVE     SPACE           TO     ZAI-REC
              INITIALIZE          ZAI-REC
              MOVE     TBL-WSOKCD(I)   TO     ZAI-F01
              MOVE     TBL-WHINCD(I)   TO     ZAI-F021
              MOVE     TBL-WHTAN (I)   TO     ZAI-F022
              MOVE     TBL-WTANA(I)(1:3)  TO  ZAI-F031
              MOVE     TBL-WTANA(I)(4:1)  TO  ZAI-F032
              MOVE     TBL-WTANA(I)(5:2)  TO  ZAI-F033
              MOVE     ZAI-F021           TO  MEI-F011
              MOVE     ZAI-F022           TO  MEI-F012
              PERFORM  900-MEI-READ
              MOVE     MEI-F031           TO  ZAI-F04
              MOVE     R00005             TO  ZAI-F05
              IF   R00002  =  0  OR  2  OR  4  OR  6  OR  8
                   IF   R00008    =   40
                        PERFORM        ZAI-WIN-RTN
                   ELSE
                        PERFORM        ZAI-WOUT-RTN
                   END-IF
              ELSE
                   IF   R00008    =   40
                        PERFORM        ZAI-WOUT-RTN
                   ELSE
                        PERFORM        ZAI-WIN-RTN
                   END-IF
              END-IF
              WRITE               ZAI-REC
          END-IF
          MOVE     1              TO   REW-FLG
     END-IF.
*
 ZAI-REWRITE-01.
*****IF       TBL-R10101(I)    =    "9"
*****         GO  TO  ZAI-REWRITE-EXIT
*****END-IF.
*
     MOVE     R00012              TO     ZAI-F01.
     MOVE     TBL-JHINCD(I)       TO     ZAI-F021.
*\\
     MOVE     TBL-HINTAN(I)       TO     ZAI-F022.
*
*****MOVE     TBL-R10401(I)       TO     ZAI-F022(1:5).
*****MOVE     TBL-R10501(I)       TO     ZAI-F022(6:2).
*****MOVE     TBL-R11501(I)       TO     ZAI-F022(8:1).
*
     MOVE     TBL-TANABN(I)(1:3)  TO     ZAI-F031.
     MOVE     TBL-TANABN(I)(4:1)  TO     ZAI-F032.
     MOVE     TBL-TANABN(I)(5:2)  TO     ZAI-F033.
     PERFORM  ZAI-READ-RTN.
     IF       INV-FLG        =   0
         IF   R00002  =  0  OR  2  OR  4  OR  6  OR  8
              IF   R00008    =   40
                   PERFORM        ZAI-OUT-RTN
              ELSE
                   PERFORM        ZAI-IN-RTN
              END-IF
         ELSE
              IF   R00008    =   40
                   PERFORM        ZAI-IN-RTN
              ELSE
                   PERFORM        ZAI-OUT-RTN
              END-IF
         END-IF
         REWRITE  ZAI-REC
     ELSE
         MOVE     SPACE           TO     ZAI-REC
         INITIALIZE          ZAI-REC
         MOVE     R00012          TO     ZAI-F01
         MOVE     TBL-JHINCD(I)   TO     ZAI-F021
*\\
         MOVE     TBL-HINTAN(I)   TO     ZAI-F022
*
*********MOVE     TBL-R10401(I)   TO     ZAI-F022(1:5)
*********MOVE     TBL-R10501(I)   TO     ZAI-F022(6:2)
*********MOVE     TBL-R11501(I)   TO     ZAI-F022(8:1)
*
         MOVE     TBL-TANABN(I)(1:3)   TO     ZAI-F031
         MOVE     TBL-TANABN(I)(4:1)   TO     ZAI-F032
         MOVE     TBL-TANABN(I)(5:2)   TO     ZAI-F033
         MOVE     ZAI-F021           TO  MEI-F011
         MOVE     ZAI-F022           TO  MEI-F012
         PERFORM  900-MEI-READ
         MOVE     MEI-F031           TO  ZAI-F04
         MOVE     R00005             TO  ZAI-F05
         IF   R00002  =  0  OR  2  OR  4  OR  6  OR  8
              IF   R00008    =   40
                   PERFORM        ZAI-OUT-RTN
              ELSE
                   PERFORM        ZAI-IN-RTN
              END-IF
         ELSE
              IF   R00008    =   40
                   PERFORM        ZAI-IN-RTN
              ELSE
                   PERFORM        ZAI-OUT-RTN
              END-IF
         END-IF
         WRITE               ZAI-REC
     END-IF.
     MOVE     1              TO   ZAI-FLG.
     IF       R00001         =     2             AND
**************R00021         >     SYS-DATE      AND
              SAV-SYU-DATE   >     WK-SYS-DATE   AND
              R00008         =     40
              MOVE           ZERO  TO  ZAI-FLG
     END-IF.
 ZAI-REWRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     商品在庫マスタ更新処理                       *
*--------------------------------------------------------------*
 ZAI-YOTEI-RTN           SECTION.
*
*****DISPLAY "ZAI-YOTEI-RTN"   UPON CONS.
*    売上伝票以外は処理対象外
     IF      R00008    NOT =      40
             GO  TO               ZAI-YOTEI-EXT
     END-IF.
*    在庫引落しされているデータは対象外
     IF      R00001    NOT =      1
        IF   DEN-F27C      =      1
             GO  TO               ZAI-YOTEI-EXT
        END-IF
     END-IF.
*    相殺区分が　ゼロ以外は対象外
     IF      R00002    NOT =      ZERO
             GO  TO               ZAI-YOTEI-EXT
     END-IF.
*
     IF      R00001        =      1
             GO  TO               ZAI-YOTEI-50
     END-IF.
*
*    修正前の数量をマイナスする
 ZAI-YOTEI-00.
*
**** DISPLAY "SURYO " TBL-WSURYO(I)  UPON CONS.
**** DISPLAY "106   " TBL-R10601(I)  UPON CONS.
     IF       TBL-WSOKCD(I)       =      SPACE       AND
              TBL-WHINCD(I)       =      SPACE       AND
              TBL-WHTAN (I)       =      SPACE       AND
              TBL-WTANA (I)       =      SPACE
              GO  TO                     ZAI-YOTEI-50
     END-IF.
*
     MOVE     TBL-WSOKCD(I)       TO     ZAI-F01.
     MOVE     TBL-WHINCD(I)       TO     ZAI-F021.
     MOVE     TBL-WHTAN (I)       TO     ZAI-F022.
     MOVE     TBL-WTANA (I)(1:3)  TO     ZAI-F031.
     MOVE     TBL-WTANA (I)(4:1)  TO     ZAI-F032.
     MOVE     TBL-WTANA (I)(5:2)  TO     ZAI-F033.
     PERFORM  ZAI-READ-RTN.
*
     IF       INV-FLG        =   0
              SUBTRACT       TBL-WSURYO(I) FROM ZAI-F13
        IF    ZAI-F13        <   ZERO
              MOVE           ZERO      TO     ZAI-F13
        END-IF
*93.10.15
        IF    TBL-HIKIATE(I) =   "1"
              SUBTRACT       TBL-WSURYO(I) FROM ZAI-F14
              IF   ZAI-F14   <   ZERO
                   MOVE  ZERO TO ZAI-F14
              END-IF
        END-IF
*93.10.15 END
*
              REWRITE        ZAI-REC
     ELSE
              MOVE     SPACE           TO     ZAI-REC
              INITIALIZE          ZAI-REC
              MOVE     TBL-WSOKCD(I)       TO     ZAI-F01
              MOVE     TBL-WHINCD(I)       TO     ZAI-F021
              MOVE     TBL-WHTAN (I)       TO     ZAI-F022
              MOVE     TBL-WTANA (I)(1:3)  TO     ZAI-F031
              MOVE     TBL-WTANA (I)(4:1)  TO     ZAI-F032
              MOVE     TBL-WTANA (I)(5:2)  TO     ZAI-F033
              MOVE     ZAI-F021           TO  MEI-F011
              MOVE     ZAI-F022           TO  MEI-F012
              PERFORM  900-MEI-READ
              MOVE     MEI-F031           TO  ZAI-F04
              MOVE     R00005             TO  ZAI-F05
              SUBTRACT       TBL-WSURYO(I) FROM ZAI-F13
              IF    ZAI-F13        <   ZERO
                    MOVE           ZERO      TO     ZAI-F13
              END-IF
              WRITE    ZAI-REC
     END-IF.
*****DISPLAY  "AAAAA"    TBL-R10601(I) UPON CONS.
*    行削除の場合　ＥＸＩＴ　へ
*--- 94.05.16 START ---*
*****IF   R10101(I)         =    "9"
     IF   TBL-R10101(I)     =    "9"
          GO  TO            ZAI-YOTEI-EXT
     END-IF.
*    削除処理の場合　ＥＸＩＴ　へ
     IF      R00001        =      3
             GO  TO               ZAI-YOTEI-EXT
     END-IF.
*
*    修正ごの数量をプラススする
 ZAI-YOTEI-50.
*****DISPLAY  "BBBBB"    TBL-R10601(I) UPON CONS.
*****DISPLAY  "XXXXX"    TBL-JHINCD(I) UPON CONS.
*
     MOVE         R00012          TO     ZAI-F01.
     MOVE     TBL-JHINCD(I)       TO     ZAI-F021.
     MOVE     TBL-HINTAN(I)       TO     ZAI-F022.
     MOVE     TBL-TANABN(I)(1:3)  TO     ZAI-F031.
     MOVE     TBL-TANABN(I)(4:1)  TO     ZAI-F032.
     MOVE     TBL-TANABN(I)(5:2)  TO     ZAI-F033.
     PERFORM  ZAI-READ-RTN.
     IF       INV-FLG        =   0
              ADD TBL-R10601(I)   TO     ZAI-F13
*93.10.15
              COMPUTE  WK-HIKIATE =      ZAI-F06 - ZAI-F14
                                       - TBL-R10601(I)
              IF  WK-HIKIATE      >=     ZERO
                  ADD  TBL-R10601(I)  TO ZAI-F14
                  MOVE "1"        TO     TBL-HIKIATE(I)
              ELSE
                  MOVE SPACE      TO     TBL-HIKIATE(I)
              END-IF
*
*93.10.15 END
*
              REWRITE             ZAI-REC
     ELSE
              MOVE     SPACE               TO     ZAI-REC
              INITIALIZE                          ZAI-REC
              MOVE         R00012          TO     ZAI-F01
              MOVE     TBL-JHINCD(I)       TO     ZAI-F021
              MOVE     TBL-HINTAN(I)       TO     ZAI-F022
              MOVE     TBL-TANABN(I)(1:3)  TO     ZAI-F031
              MOVE     TBL-TANABN(I)(4:1)  TO     ZAI-F032
              MOVE     TBL-TANABN(I)(5:2)  TO     ZAI-F033
              MOVE     ZAI-F021           TO  MEI-F011
              MOVE     ZAI-F022           TO  MEI-F012
              PERFORM  900-MEI-READ
              MOVE     MEI-F031           TO  ZAI-F04
              MOVE     R00005             TO  ZAI-F05
*
              ADD TBL-R10601(I)   TO     ZAI-F13
              WRITE               ZAI-REC
     END-IF.
*
 ZAI-YOTEI-EXT.
*
*--------------------------------------------------------------*
*    LEVEL  ALL   ワーク在庫 数量セット （入庫）
*--------------------------------------------------------------*
 ZAI-WIN-RTN         SECTION.
* ワークの数量セット（入庫データ）
     ADD        TBL-WSURYO(I)   TO     ZAI-F06.
* 出荷日ベースの更新（当月・次月入出庫／出庫）
* 出荷日が０の時、納品日ベースの更新
     IF      TBL-SYUKKA(I)(1:4)   >   WK-SIME
             ADD           TBL-WSURYO(I)   TO     ZAI-F19
             SUBTRACT      TBL-WSURYO(I)   FROM   ZAI-F18
             SUBTRACT      TBL-WSURYO(I)   FROM   ZAI-F205
         ELSE
             ADD           TBL-WSURYO(I)   TO     ZAI-F08
             SUBTRACT      TBL-WSURYO(I)   FROM   ZAI-F11
             SUBTRACT      TBL-WSURYO(I)   FROM   ZAI-F204
     END-IF.
***  94.05.16  END   ***
*
 ZAI-WIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   ワーク在庫 数量セット（出庫）
*--------------------------------------------------------------*
 ZAI-WOUT-RTN        SECTION.
* ワークの数量セット（出庫データ）
     SUBTRACT   TBL-WSURYO(I)   FROM   ZAI-F06.
* 出荷日ベースの更新（当月・次月入出庫／出庫）
* 出荷日が０の時、納品日ベースの更新
     IF      TBL-SYUKKA(I)(1:4)   >   WK-SIME
             SUBTRACT   TBL-WSURYO(I)  FROM   ZAI-F19
             ADD        TBL-WSURYO(I)  TO     ZAI-F18
             ADD        TBL-WSURYO(I)  TO     ZAI-F205
         ELSE
             SUBTRACT   TBL-WSURYO(I)  FROM   ZAI-F08
             ADD        TBL-WSURYO(I)  TO     ZAI-F11
             ADD        TBL-WSURYO(I)  TO     ZAI-F204
     END-IF.
***  94.05.16  END   ***
*
 ZAI-WOUT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫 数量セット （入庫）
*--------------------------------------------------------------*
 ZAI-IN-RTN         SECTION.
* 商品在庫マスタの数量セット（入庫データ）
     IF     R00001    =      1        AND
            TBL-R10101(I)    =  "9"
            GO    TO  ZAI-IN-EXIT
     END-IF
*
     IF     R00001    =      1        OR
            REW-FLG   =      1
            ADD       TBL-R10601(I)   TO    ZAI-F06
* 出荷日ベースの更新（当月・次月入出庫／出庫）
* 出荷日が０の時、納品日ベースの更新
            IF       R00021(1:4)   >   WK-SIME
                  ADD      TBL-R10601(I)    TO    ZAI-F19
                  SUBTRACT TBL-R10601(I)    FROM  ZAI-F18
                  SUBTRACT TBL-R10601(I)    FROM  ZAI-F205
               ELSE
                  ADD      TBL-R10601(I)    TO    ZAI-F08
                  SUBTRACT TBL-R10601(I)    FROM  ZAI-F11
                  SUBTRACT TBL-R10601(I)    FROM  ZAI-F204
            END-IF
***  94.05.16  END   ***
            MOVE      0               TO    REW-FLG
            GO    TO  ZAI-IN-EXIT
     END-IF.
     IF     TBL-R10101(I)    =  "9"  OR
            R00001           =   3
            SUBTRACT  TBL-WSURYO(I)  FROM   ZAI-F06
* 出荷日ベースの更新（当月・次月入出庫／出庫）
* 出荷日が０の時、納品日ベースの更新
               IF    R00021(1:4)   >   WK-SIME
                  SUBTRACT   TBL-WSURYO(I)  FROM   ZAI-F19
                  ADD        TBL-WSURYO(I)  TO     ZAI-F18
                  ADD        TBL-WSURYO(I)  TO     ZAI-F205
               ELSE
                  SUBTRACT   TBL-WSURYO(I)  FROM   ZAI-F08
                  ADD        TBL-WSURYO(I)  TO     ZAI-F11
                  ADD        TBL-WSURYO(I)  TO     ZAI-F204
               END-IF
***  94.05.16  END   ***
            GO    TO  ZAI-IN-EXIT
     END-IF.
*****DISPLAY  "TBL-R10601-I= " TBL-R10601(I)   UPON CONS.
*****DISPLAY  "TBL-WSURYO-I= " TBL-WSURYO(I)   UPON CONS.
     COMPUTE   WK-SURYO   =  TBL-R10601(I)  -  TBL-WSURYO(I).
     ADD    WK-SURYO         TO    ZAI-F06.
*****ADD    WK-SURYO         TO    ZAI-F08.
*****ADD    WK-SURYO         TO    ZAI-F204.
* 出荷日ベースの更新（当月・次月入出庫／出庫）
* 出荷日が０の時、納品日ベースの更新
        IF     R00021(1:4)   >  WK-SIME
            ADD    WK-SURYO       TO     ZAI-F19
            SUBTRACT  WK-SURYO    FROM   ZAI-F18
            SUBTRACT  WK-SURYO    FROM   ZAI-F205
        ELSE
            ADD    WK-SURYO       TO     ZAI-F08
            SUBTRACT  WK-SURYO    FROM   ZAI-F11
            SUBTRACT  WK-SURYO    FROM   ZAI-F204
     END-IF.
***  94.05.16  END   ***
 ZAI-IN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫 数量セット（出庫）
*--------------------------------------------------------------*
 ZAI-OUT-RTN        SECTION.
* 商品在庫マスタの数量セット（出庫データ）
     IF     R00001    =      1        AND
            TBL-R10101(I)    =  "9"
            GO    TO  ZAI-OUT-EXIT
     END-IF
*修正処理時　出荷日が本日日付より大きい時予定処理へ
     IF     R00001    =      2        AND
************R00021    >      SYS-DATE AND
            SAV-SYU-DATE  >  WK-SYS-DATE AND
            R00008    =      40
            ADD        TBL-R10601(I)  TO   ZAI-F13
            COMPUTE    WK-CHK     =   ZAI-F06 - ZAI-F14
            IF         WK-CHK    >=   TBL-R10601(I)
                       ADD       TBL-R10601(I)  TO   ZAI-F14
                       MOVE      "1"  TO   TBL-HIKIATE(I)
            END-IF
            GO    TO  ZAI-OUT-EXIT
     END-IF.
*
     IF     R00001    =      1        OR
            REW-FLG   =      1
            SUBTRACT  TBL-R10601(I)   FROM  ZAI-F06
* 出荷日ベースの更新（当月・次月入出庫／出庫）
               IF    R00021(1:4)   >   WK-SIME
                  SUBTRACT   TBL-R10601(I)  FROM   ZAI-F19
                  ADD        TBL-R10601(I)  TO     ZAI-F18
                  ADD        TBL-R10601(I)  TO     ZAI-F205
               ELSE
                  SUBTRACT   TBL-R10601(I)  FROM   ZAI-F08
                  ADD        TBL-R10601(I)  TO     ZAI-F11
                  ADD        TBL-R10601(I)  TO     ZAI-F204
               END-IF
***  94.05.16  END   ***
            MOVE      0               TO    REW-FLG
            GO    TO  ZAI-OUT-EXIT
     END-IF.
     IF     TBL-R10101(I)    =  "9"  OR
            R00001           =   3
            ADD       TBL-WSURYO(I)   TO    ZAI-F06
* 出荷日ベースの更新（当月・次月入出庫／出庫）
               IF    R00021(1:4)   >   WK-SIME
                  ADD       TBL-WSURYO(I)     TO    ZAI-F19
                  SUBTRACT  TBL-WSURYO(I)     FROM  ZAI-F18
                  SUBTRACT  TBL-WSURYO(I)     FROM  ZAI-F205
               ELSE
                  ADD       TBL-WSURYO(I)     TO    ZAI-F08
                  SUBTRACT  TBL-WSURYO(I)     FROM  ZAI-F11
                  SUBTRACT  TBL-WSURYO(I)     FROM  ZAI-F204
               END-IF
***  94.05.16  END   ***
            GO    TO  ZAI-OUT-EXIT
     END-IF.
*****DISPLAY  "TBL-R10601-O= " TBL-R10601(I)   UPON CONS.
*****DISPLAY  "TBL-WSURYO-O= " TBL-WSURYO(I)   UPON CONS.
     COMPUTE      WK-SURYO   =   TBL-R10601(I)  -  TBL-WSURYO(I).
     SUBTRACT     WK-SURYO   FROM    ZAI-F06.
* 出荷日ベースの更新（当月・次月入出庫／出庫）
         IF  R00021(1:4)   >   WK-SIME
             SUBTRACT   WK-SURYO FROM    ZAI-F19
             ADD        WK-SURYO TO      ZAI-F18
             ADD        WK-SURYO TO      ZAI-F205
         ELSE
             SUBTRACT   WK-SURYO FROM    ZAI-F08
             ADD        WK-SURYO TO      ZAI-F11
             ADD        WK-SURYO TO      ZAI-F204
     END-IF.
***  94.05.16  END   ***
 ZAI-OUT-EXIT.
     EXIT.
***   93/05/24    END   ***
*--------------------------------------------------------------*
*    LEVEL  4     テーブル表示　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 900-TBL-TO-DSP         SECTION.
     PERFORM  900-L-MAX-GET.
     IF       P-CNT  =  1
              PERFORM   VARYING  I  FROM  1  BY  1
                                    UNTIL  I  >  L-MAX
                   MOVE      TBL (I)        TO   BDY (I)
              END-PERFORM
     ELSE
              MOVE      1         TO   J
              PERFORM   VARYING  I  FROM  7  BY  1
                                    UNTIL  I  >  TOK-F82
                   MOVE      TBL (I)        TO   BDY (J)
                   ADD       1              TO   J
              END-PERFORM
     END-IF.
 900-TBL-TO-DSP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     最大表示行数取得　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 900-L-MAX-GET          SECTION.
     IF       TOK-F82  >  6
              IF   P-CNT  =  1
                   MOVE      6         TO   L-MAX
              ELSE
                   IF   TOK-F82  >  10
                        MOVE      4         TO   L-MAX
                   ELSE
                        COMPUTE   L-MAX  =  TOK-F82  -  6
                   END-IF
              END-IF
     ELSE
              MOVE      TOK-F82   TO   L-MAX
     END-IF.
*
     COMPUTE  I  =  L-MAX  +  1.
     PERFORM  VARYING  I  FROM  I  BY  1  UNTIL  I  >  6
              MOVE      SPACE     TO   BDY (I)
     END-PERFORM.
     COMPUTE  I  =  TOK-F82  +  1.
     PERFORM  VARYING  I  FROM  I  BY  1  UNTIL  I  >  10
              MOVE      SPACE     TO   TBL (I)
              MOVE      SPACE     TO   TBL-X (I)
              INITIALIZE               TBL-X (I)
     END-PERFORM.
  900-L-MAX-GET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ヘッド部読込　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 900-DEN-TO-HED         SECTION.
     MOVE     DEN-F06        TO   R00003.
     MOVE     DEN-F133       TO   R00004.
     MOVE     DEN-F01        TO   R00005
                                  TOK-F01.
     PERFORM  900-TOK-READ.
     IF       INV-FLG  =  0
              MOVE      TOK-F03   TO   R00006
     ELSE
              MOVE      NC"＊＊＊＊＊＊＊＊＊＊"  TO   R00006
     END-IF.
     MOVE     DEN-F02        TO   R00007.
     MOVE     DEN-F051       TO   R00008.
     MOVE     DEN-F052       TO   R00009.
     MOVE     DEN-F07        TO   R00010.
     MOVE     DEN-F01        TO   TEN-F52.
     MOVE     DEN-F07        TO   TEN-F011.
*
*\\  93.07.02 START \\
     MOVE     ZERO           TO   INV-FLG.
     IF       DEN-F07        NOT =    ZERO
              PERFORM  900-TEN-READ
        IF    INV-FLG  =  0
              MOVE      TEN-F03    TO   R00011
        ELSE
              MOVE      NC"＊＊＊＊＊＊＊＊＊＊"  TO   R00011
        END-IF
     ELSE
              MOVE      SPACE                     TO   R00011
     END-IF.
*
*\\
     MOVE     DEN-F08        TO   R00012.
     MOVE     DEN-F09        TO   R00013.
*****MOVE     DEN-F10        TO   R00014.
     MOVE     DEN-F44        TO   R00014.
     MOVE     DEN-F111       TO   R00015.
     MOVE     DEN-F112       TO   R00016.
***  94.05.16 START ***
     MOVE     DEN-F113       TO   R00021.
***  94.05.16  END  ***
     MOVE     DEN-F12        TO   R00017.
     MOVE     DEN-F131       TO   R00018.
     MOVE     DEN-F132       TO   R00019.
     MOVE     DEN-F134       TO   R00020.
*****
     MOVE     DEN-F23        TO   WK-F23.
     MOVE     DEN-F276       TO   WK-F276.
     MOVE     DEN-F27B       TO   WK-F27B.
  900-DEN-TO-HED-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     テーブル読込　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 900-DEN-TO-TBL         SECTION.
     IF       DEN-F03  =  80
              MOVE      DEN-F1411 (1:2)     TO   R00023 WK-R00023
              MOVE      DEN-F1421           TO   R00024
              MOVE      DEN-F1422           TO   R00025
              GO   TO   900-DEN-TO-TBL-EXIT
     END-IF.
     IF       DEN-F03  =  90
              MOVE      DEN-F181  TO   R00028
              MOVE      DEN-F521  TO   SAV-MAESORYO
              GO   TO   900-DEN-TO-TBL-EXIT
     END-IF.
*
     MOVE     SPACE          TO   TBL-R10101 (DEN-F03).
     IF       DEN-F25  =  SPACE
              MOVE      DEN-F1411      TO   TBL-R10301 (DEN-F03)
              MOVE      DEN-F1412(1:5) TO   TBL-R10401 (DEN-F03)
              MOVE      DEN-F1412(6:2) TO   TBL-R10501 (DEN-F03)
              MOVE      DEN-F1412(8:1) TO   TBL-R11501 (DEN-F03)
              MOVE      SPACE          TO   TBL-SHOCD (DEN-F03)
     ELSE
              MOVE      DEN-F25 (1:8)  TO   TBL-R10301 (DEN-F03)
              MOVE      DEN-F25 (9:5)  TO   TBL-R10401 (DEN-F03)
              MOVE      SPACE          TO   TBL-R10501 (DEN-F03)
              MOVE      SPACE          TO   TBL-R11501 (DEN-F03)
              MOVE      DEN-F1411 TO   TBL-SHOCD (DEN-F03) (1:8)
              MOVE      DEN-F1412 TO   TBL-SHOCD (DEN-F03) (9:8)
     END-IF.
     MOVE     DEN-F15        TO   TBL-R10601 (DEN-F03).
     MOVE     DEN-F16        TO   TBL-R10701 (DEN-F03).
*
***  93/05/08  START  ***
     IF       DEN-F16        =    "3"
              MOVE  DEN-F181 TO   TBL-R10801 (DEN-F03)
              MOVE  DEN-F182 TO   TBL-R10901 (DEN-F03)
     ELSE
              MOVE  DEN-F172 TO   TBL-R10801 (DEN-F03)
              MOVE  DEN-F173 TO   TBL-R10901 (DEN-F03)
     END-IF.
***  93/05/08  END    ***
     MOVE     DEN-F1421      TO   TBL-R11101 (DEN-F03).
     MOVE     DEN-F1422      TO   TBL-R11201 (DEN-F03).
     MOVE     DEN-F21        TO   TBL-R11301 (DEN-F03).
     MOVE     DEN-F22        TO   TBL-R11401 (DEN-F03).
     MOVE     DEN-F171       TO   TBL-SIIGEN (DEN-F03).
***   93/05/24  START   ***
*更新前在庫ＫＥＹ退避
     MOVE     DEN-F113       TO   TBL-SYUKKA (DEN-F03).
     MOVE     DEN-F08        TO   TBL-WSOKCD (DEN-F03).
     MOVE     DEN-F15        TO   TBL-WSURYO (DEN-F03).
     MOVE     DEN-F1412      TO   TBL-WHTAN  (DEN-F03).
     MOVE     DEN-F27D       TO   TBL-HIKIATE(DEN-F03).
     MOVE     DEN-F01        TO   STB-F01.
*****MOVE     DEN-F1411      TO   STB-F02(1:8).
*****MOVE     DEN-F1412      TO   STB-F02(9:5).
     MOVE     DEN-F25        TO   STB-F02.
     PERFORM  900-STB-READ.
     IF       INV-FLG   =    0
              MOVE  STB-F08  TO   TBL-WTANA  (DEN-F03)
              MOVE  STB-F031 TO   TBL-WHINCD (DEN-F03)
              MOVE  STB-F032 TO   TBL-WHTAN  (DEN-F03)
     ELSE
              MOVE  DEN-F01       TO  SHT-F01
              MOVE  DEN-F1411     TO  SHT-F031
              MOVE  DEN-F1412     TO  SHT-F032
              PERFORM   SHT-READ-RTN
*\\
              IF    INV-FLG       =   ZERO
                    MOVE  SHT-F08  TO   TBL-WTANA  (DEN-F03)
              ELSE
                    MOVE  SPACE    TO   TBL-WTANA  (DEN-F03)
              END-IF
              MOVE  DEN-F1411      TO   TBL-WHINCD (DEN-F03)
              MOVE  DEN-F1412      TO   TBL-WHTAN  (DEN-F03)
     END-IF.
     MOVE     TBL-WTANA  (DEN-F03)    TO  TBL-TANABN (DEN-F03).
     MOVE     TBL-WHINCD (DEN-F03)    TO  TBL-JHINCD (DEN-F03).
     MOVE     TBL-WHTAN  (DEN-F03)    TO  TBL-HINTAN (DEN-F03).
***   93/05/24    END   ***
*    更新前項目退避
**** MOVE     DEN-F50        TO   TBL-MAESUU (DEN-F03).
**** MOVE     DEN-F512       TO   TBL-MAEGTAN(DEN-F03).
**** MOVE     DEN-F513       TO   TBL-MAEBTAN(DEN-F03).
**** MOVE     DEN-F521       TO   TBL-MAEGKIN(DEN-F03).
**** MOVE     DEN-F522       TO   TBL-MAEBKIN(DEN-F03).
     IF       DEN-F50    NOT  NUMERIC
              MOVE   ZERO         TO   TBL-MAESUU(DEN-F03)
     ELSE
              MOVE   DEN-F50      TO   TBL-MAESUU(DEN-F03)
     END-IF.
     IF       DEN-F512   NOT  NUMERIC
              MOVE   ZERO         TO   TBL-MAEGTAN(DEN-F03)
     ELSE
              MOVE   DEN-F512     TO   TBL-MAEGTAN(DEN-F03)
     END-IF.
     IF       DEN-F513   NOT  NUMERIC
              MOVE   ZERO         TO   TBL-MAEBTAN(DEN-F03)
     ELSE
              MOVE   DEN-F513     TO   TBL-MAEBTAN(DEN-F03)
     END-IF.
     IF       DEN-F521   NOT  NUMERIC
              MOVE   ZERO         TO   TBL-MAEGKIN(DEN-F03)
     ELSE
              MOVE   DEN-F521     TO   TBL-MAEGKIN(DEN-F03)
     END-IF.
     IF       DEN-F522   NOT  NUMERIC
              MOVE   ZERO         TO   TBL-MAEBKIN(DEN-F03)
     ELSE
              MOVE   DEN-F522     TO   TBL-MAEBKIN(DEN-F03)
     END-IF.
*
*次頁存在チェック
     IF       DEN-F03  >  6
              MOVE     1     TO  GPAGE-FLG
     END-IF.
*
  900-DEN-TO-TBL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     合計算出                          *
*--------------------------------------------------------------*
 DSP-GOKEI-RTN           SECTION.
     MOVE     ZERO           TO   R00026   R00027.
     PERFORM  VARYING  I  FROM  1  BY  1   UNTIL  I  >  10
        IF   TBL-R10101 (I) =    " "  OR  "1"
              IF       TBL-R10801 (I)  IS  NUMERIC
                IF    (TBL-R10301 (I) =    SORYO-CD
                AND    TBL-R10401 (I) =    SPACE
                AND    TBL-R10501 (I) =    SPACE
                AND    TBL-R11501 (I) =    SPACE)
                OR    (TBL-R10701 (I) =    "3")
                   ADD       TBL-R10801 (I)      TO   R00026
                ELSE
                  IF   TBL-R10601 (I)  IS  NUMERIC
                       COMPUTE R00026 = R00026 + (TBL-R10601(I)
                                               *  TBL-R10801(I))
                  END-IF
                END-IF
              END-IF
              IF       TBL-R10901 (I)  IS  NUMERIC
                IF    (TBL-R10301 (I) =    SORYO-CD
                AND    TBL-R10401 (I) =    SPACE
                AND    TBL-R10501 (I) =    SPACE
                AND    TBL-R11501 (I) =    SPACE)
                OR    (TBL-R10701 (I) =    "3")
                   ADD       TBL-R10901 (I)      TO   R00027
                ELSE
                   IF  TBL-R10601 (I)  IS  NUMERIC
                       COMPUTE R00027 = R00027 + (TBL-R10601(I)
                                               *  TBL-R10901(I))
                   END-IF
                END-IF
              END-IF
        END-IF
     END-PERFORM.
 DSP-GOKEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 DSP-RD-RTN           SECTION.
*
     MOVE    "SSY0101I"      TO        PGID.
     MOVE    "FSY01011"      TO        FORM.
     ACCEPT   SYS-TIME2      FROM      TIME.
     MOVE     SYS-TIMEW      TO        SYSTIM.
     IF       ERR-FLG  =  0
              MOVE      SPACE               TO   ERR
*##### 98/09/18 追加 START #####
              MOVE      "D"      TO   EDIT-OPTION OF ERR
*##### 98/09/18 追加 END   #####
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
 DSP-RD-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FSY01011.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     伝票番号取得                                *
*--------------------------------------------------------------*
 900-DENNO-GET          SECTION.
     MOVE     R00005         TO   TOK-F01.
     PERFORM  900-TOK-READ.
     MOVE     TOK-F54        TO   R00007.
     INITIALIZE         LINK-AREA.
     MOVE     TOK-F93        TO   LI-KBN.
     MOVE     TOK-F92        TO   LI-KETA.
     MOVE     TOK-F57        TO   LI-START.
     MOVE     TOK-F58        TO   LI-END.
     MOVE     TOK-F54        TO   LI-DENNO.
     CALL     "OSKTCDCK"     USING     LINK-AREA.
     IF       LO-ERR  =  0
              MOVE      LO-NEXT   TO   TOK-F54
     ELSE
              DISPLAY   NC"伝票_採番エラー"  UPON CONS
              STOP  RUN
     END-IF.
     REWRITE  TOK-REC.
 900-DENNO-GET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     伝票番号チェック                            *
*--------------------------------------------------------------*
 900-DENNO-CHECK        SECTION.
     INITIALIZE         LINK-AREA.
     MOVE     TOK-F93        TO   LI-KBN.
     MOVE     TOK-F92        TO   LI-KETA.
     MOVE     TOK-F57        TO   LI-START.
     MOVE     TOK-F58        TO   LI-END.
     MOVE     R00007         TO   LI-DENNO.
*ドイト用特別　ロジック
     IF       R00005         =    10545
              MOVE     ZERO  TO   WK-R00007
              MOVE     ZERO  TO   WK-KETA-R
              MOVE     R00007 TO  WK-R00007
        PERFORM   VARYING    WK-KETA       FROM 10 BY -1
                  UNTIL      WK-KETA  =    1
                         OR  WK-R00007-RR(WK-KETA) =   ZERO
                             ADD      1    TO      WK-KETA-R
        END-PERFORM
*
********DISPLAY "*** ="    WK-KETA-R UPON CONS
        IF    WK-KETA-R      =        7
              MOVE     7         TO   LI-KETA
              MOVE     9999999   TO   LI-END
        END-IF
     END-IF.
*
*エンチョー用　ロジック
     IF       R00005         =    24279
              GO TO    900-DENNO-CHECK-EXIT
     END-IF.
*カーマ用　ロジック
     IF       R00005         =    13938
              GO TO    900-DENNO-CHECK-EXIT
     END-IF.
*カインズ用　ロジック
     IF       R00005         =    921084
              GO TO    900-DENNO-CHECK-EXIT
     END-IF.
*
     CALL     "OSKTCDCK"     USING     LINK-AREA.
     IF       LO-ERR  NOT  =  0
              MOVE      5         TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR OF R00007
              MOVE     "R"   TO   EDIT-OPTION OF R00007
     END-IF.
 900-DENNO-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     日付チェック　　                            *
*--------------------------------------------------------------*
 900-DATE-CHECK         SECTION.
*
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     CHK-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   NOT =     ZERO
              MOVE      10             TO   ERR-FLG
     ELSE
              MOVE      LINK-OUT-YMD8  TO   CHK-DATE
     END-IF.
*
*****DISPLAY "#######   "   START-YYMM UPON CONS.
**** COMPUTE  CHK-YY  =  HENKAN-TI  +  CHK-YY.
**** DIVIDE   CHK-YY  BY  4  GIVING  CHK-01  REMAINDER  CHK-02.
**** IF       CHK-02  =  0
****          MOVE      29        TO   WK-MATUBI (2)
**** ELSE
****          MOVE      28        TO   WK-MATUBI (2)
**** END-IF.
**** IF       CHK-MM  =  0  OR  >  12
****          MOVE      10        TO   ERR-FLG
****          GO   TO   900-DATE-CHECK-EXIT
**** END-IF.
**** IF       CHK-DD  =  0
**** OR       CHK-DD  >  WK-MATUBI (CHK-MM)
****          MOVE      10        TO   ERR-FLG
****          GO   TO   900-DATE-CHECK-EXIT
**** END-IF.
     IF     ( CHK-YYMM  <  START-YYMM
     OR       CHK-YYMM  >  END-YYMM )
              MOVE      10        TO   ERR-FLG
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
     REWRITE  TOK-REC.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   店舗マスタ　ＲＥＡＤ　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HTENMS    INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品変換テーブル　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 900-STB-READ           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HSHOTBL   INVALID   KEY
              MOVE      1         TO   INV-FLG
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
     END-READ.
 900-MEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   条件ファイル　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 JYO-RD-RTN           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HJYOKEN   INVALID   KEY
              MOVE      1         TO   INV-FLG
              GO   TO   JYO-RD-EXIT
     END-READ.
 JYO-RD-EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   伝票データＦ　　　ＲＥＡＤ　（ＲＡＮ）　　　 *
*--------------------------------------------------------------*
 900-DEN-RAN-READ       SECTION.
     MOVE     0              TO   INV-FLG.
     READ     SHTDENF   INVALID  KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 900-DEN-RAN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   伝票データＦ　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     READ     SHTDENF   NEXT      AT   END
              MOVE      1         TO   END-FLG
              GO   TO   900-DEN-READ-EXIT
     END-READ.
     IF       DEN-F01  NOT  =  R00005
     OR       DEN-F02  NOT  =  R00007
     OR       DEN-F04  NOT  =  WK-R00002
     OR       DEN-F051 NOT  =  R00008
              MOVE      1         TO   END-FLG
              REWRITE   DEN-REC
              GO   TO   900-DEN-READ-EXIT
     END-IF.
     IF       R00001  =  9
     AND      DEN-F277  NOT  =  9
              REWRITE   DEN-REC
              GO   TO   900-DEN-READ
     END-IF.
 900-DEN-READ-EXIT.
     EXIT.
***   93/05/24  END     ***
*--------------------------------------------------------------*
*    LEVEL  ALL   商品変換テーブル４　ＲＥＡＤ　　　　　　　　 *
*--------------------------------------------------------------*
 SHT-READ-RTN            SECTION.
     MOVE     0         TO   INV-FLG.
     READ     SHOTBL4   INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 SHT-READ-EXIT.
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
***   93/05/24  END     ***
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF R00001
                                  EDIT-CURSOR OF R00002
                                  EDIT-CURSOR OF R00003
                                  EDIT-CURSOR OF R00004
                                  EDIT-CURSOR OF R00005
                                  EDIT-CURSOR OF R00007
                                  EDIT-CURSOR OF R00008
                                  EDIT-CURSOR OF R00010
                                  EDIT-CURSOR OF R00012
                                  EDIT-CURSOR OF R00013
                                  EDIT-CURSOR OF R00014
                                  EDIT-CURSOR OF R00015
                                  EDIT-CURSOR OF R00016
***   94.05.16  START   ***
                                  EDIT-CURSOR OF R00021
***   94.05.16   END    ***
                                  EDIT-CURSOR OF R00017
                                  EDIT-CURSOR OF R00018
                                  EDIT-CURSOR OF R00019
                                  EDIT-CURSOR OF R00020.
     MOVE     "M"            TO   EDIT-OPTION OF R00001
                                  EDIT-OPTION OF R00002
                                  EDIT-OPTION OF R00003
                                  EDIT-OPTION OF R00004
                                  EDIT-OPTION OF R00005
                                  EDIT-OPTION OF R00007
                                  EDIT-OPTION OF R00008
                                  EDIT-OPTION OF R00010
                                  EDIT-OPTION OF R00012
                                  EDIT-OPTION OF R00013
                                  EDIT-OPTION OF R00014
                                  EDIT-OPTION OF R00015
                                  EDIT-OPTION OF R00016
***   94.05.16  START   ***
                                  EDIT-OPTION OF R00021
***   94.05.16   END    ***
                                  EDIT-OPTION OF R00017
                                  EDIT-OPTION OF R00018
                                  EDIT-OPTION OF R00019
                                  EDIT-OPTION OF R00020.
*##### 98/09/18 追加 START #####
**** MOVE     " "        TO       EDIT-OPTION OF ERR.
*##### 98/09/18 追加 END   #####
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY-RTN           SECTION.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
     MOVE     " "        TO   EDIT-CURSOR OF R10101(I)
                              EDIT-CURSOR OF R10201(I)
                              EDIT-CURSOR OF R10301(I)
                              EDIT-CURSOR OF R10401(I)
                              EDIT-CURSOR OF R10501(I)
                              EDIT-CURSOR OF R11501(I)
                              EDIT-CURSOR OF R10601(I)
                              EDIT-CURSOR OF R10701(I)
                              EDIT-CURSOR OF R10801(I)
                              EDIT-CURSOR OF R10901(I)
                              EDIT-CURSOR OF R11001(I)
                              EDIT-CURSOR OF R11101(I)
                              EDIT-CURSOR OF R11201(I)
                              EDIT-CURSOR OF R11301(I)
                              EDIT-CURSOR OF R11401(I)
     MOVE     "M"        TO   EDIT-OPTION OF R10101(I)
                              EDIT-OPTION OF R10201(I)
                              EDIT-OPTION OF R10301(I)
                              EDIT-OPTION OF R10401(I)
                              EDIT-OPTION OF R10501(I)
                              EDIT-OPTION OF R11501(I)
                              EDIT-OPTION OF R10601(I)
                              EDIT-OPTION OF R10701(I)
                              EDIT-OPTION OF R10801(I)
                              EDIT-OPTION OF R10901(I)
                              EDIT-OPTION OF R11001(I)
                              EDIT-OPTION OF R11101(I)
                              EDIT-OPTION OF R11201(I)
                              EDIT-OPTION OF R11301(I)
                              EDIT-OPTION OF R11401(I)
     END-PERFORM.
 CLR-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF R00023
                              EDIT-CURSOR OF R00024
                              EDIT-CURSOR OF R00025
                              EDIT-CURSOR OF R00028
                              EDIT-CURSOR OF KKNN.
     MOVE     "M"        TO   EDIT-OPTION OF R00023
                              EDIT-OPTION OF R00024
                              EDIT-OPTION OF R00025
                              EDIT-OPTION OF R00026
*##### 98/09/18 追加 START #####
****                          EDIT-OPTION OF R00027
****                          EDIT-OPTION OF R00028
****                          EDIT-OPTION OF ERR
*##### 98/09/18 追加 END   #####
                              EDIT-OPTION OF KKNN.
 CLR-TAIL-EXIT.
     EXIT.
*#############################################################*
*伝票区分”４１，４２”の時の入力チェック追加 98/09/17 NAV T.T*
*#############################################################*
*--------------------------------------------------------------*
*    LEVEL  3      伝票区分”４１，４２”　チェック　　　　　　*
*--------------------------------------------------------------*
 DENPYO-ST-RTN          SECTION.
*キーのセット
*取引先コード
     MOVE     R00005         TO   DEN-F01.
*伝票_
     MOVE     R00007         TO   DEN-F02.
*相殺区分
     MOVE     R00002         TO   DEN-F04  WK-R00002.
*伝票区分
     EVALUATE R00008
         WHEN  41
              MOVE  42       TO   DEN-F051
              MOVE  42       TO   WK-DENKU
         WHEN  42
              MOVE  41       TO   DEN-F051
              MOVE  41       TO   WK-DENKU
     END-EVALUATE.
*行_
     MOVE     0              TO   DEN-F03.
*終了フラグ
     MOVE     0              TO   END-FLG.
     START    SHTDENF   KEY  NOT  <  DEN-F01     DEN-F02
                                     DEN-F04     DEN-F051
                                     DEN-F03
         INVALID   KEY
              MOVE      1         TO   END-FLG
     END-START.
*
 DENPYO-ST-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   伝票データＦ　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 DENPYO-READ-RTN        SECTION.
     READ     SHTDENF   NEXT      AT   END
              MOVE      1         TO   END-FLG
              GO   TO   DENPYO-READ-EXIT
     END-READ.
     IF       DEN-F01  NOT  =  R00005
     OR       DEN-F02  NOT  =  R00007
     OR       DEN-F04  NOT  =  R00002
              MOVE      1         TO   END-FLG
              GO                  TO   DENPYO-READ-EXIT
     ELSE
              IF   DEN-F051  NOT =  WK-DENKU
                   MOVE 1         TO   END-FLG
                   GO             TO   DENPYO-READ-EXIT
              END-IF
     END-IF.
*データ存在確認
     MOVE     1         TO     DEN-KAKU-CHK.
*伝票合計計算
     PERFORM  DENPYO-GOKEI-RTN.
*次レコード読込みへ
     GO                 TO     DENPYO-READ-RTN.
 DENPYO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     伝票区分”４１，４２”合計計算    *
*--------------------------------------------------------------*
 DENPYO-GOKEI-RTN           SECTION.
     IF   DEN-F172  IS  NUMERIC
          IF  (DEN-F1411  =  SORYO-CD AND  DEN-F1412  =  SPACE)
          OR  (DEN-F16    =  "3")
               ADD  DEN-F172     TO   WK-R00026
          ELSE
               IF   DEN-F15      IS   NUMERIC
                    COMPUTE WK-R00026 = WK-R00026 + (DEN-F15
                                                 *  DEN-F172)
               END-IF
          END-IF
     END-IF.
     IF   DEN-F173  IS  NUMERIC
          IF  (DEN-F1411  =  SORYO-CD AND  DEN-F1412  =  SPACE)
          OR  (DEN-F16    =  "3")
               ADD  DEN-F173     TO   WK-R00027
          ELSE
               IF   DEN-F15      IS   NUMERIC
                    COMPUTE WK-R00027 = WK-R00027 + (DEN-F15
                                                 *  DEN-F173)
               END-IF
          END-IF
     END-IF.
 DENPYO-GOKEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     登録合計と合計額のチェック処理    *
*--------------------------------------------------------------*
 DENKU-ERR-RTN              SECTION.
*登録済合計額と入力合計額のチェック
**** DISPLAY "WK-R00026 = " WK-R00026 UPON CONS.
**** DISPLAY "WK-R00027 = " WK-R00027 UPON CONS.
**** DISPLAY "R00026    = " R00026    UPON CONS.
**** DISPLAY "R00027    = " R00027    UPON CONS.
     IF   WK-R00026   =  R00026
     AND  WK-R00027   =  R00027
          EVALUATE R00008
              WHEN 41
              MOVE  39 TO  ERR-FLG
****          MOVE "C" TO  EDIT-CURSOR OF KKNN
****          MOVE "R" TO  EDIT-OPTION OF R00026
****          MOVE "R" TO  EDIT-OPTION OF R00027
              WHEN 42
              MOVE  38 TO  ERR-FLG
****          MOVE "C" TO  EDIT-CURSOR OF KKNN
****          MOVE "R" TO  EDIT-OPTION OF R00026
****          MOVE "R" TO  EDIT-OPTION OF R00027
          END-EVALUATE
**** ELSE
****      MOVE   "C"   TO  EDIT-CURSOR OF KKNN
****      MOVE   "M"   TO  EDIT-OPTION OF R00026
****      MOVE   "M"   TO  EDIT-OPTION OF R00027
     END-IF.
*
 DENKU-ERR-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
