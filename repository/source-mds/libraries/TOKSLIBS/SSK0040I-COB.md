# SSK0040I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSK0040I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　基幹システム　　　　　　　　　　　*
*    業務名　　　　　　　：　ケーヨー受領・返品（伝票レス）    *
*    モジュール名　　　　：　返品形状確認                      *
*    作成日　　　　　　　：　2014/04/02                        *
*    作成者　　　　　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　検収日・店舗・伝票番号で　　　　　*
*                            返品累積データを呼び出し、　　    *
*                            返品伝票計上に必要な情報の入力    *
*　　　　　　　　　　　　　　及び、計上区分の入力を行う。　　　*
*    変更日　　　　　　　：　2014/04/10                        *
*    変更内容　　　　　　：　ＳＨＯＴＢＬ４→２に変更　　　　　*
*    　　　　　　　　　　：　出荷場所もキーとする　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSK0040I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2014/04/02.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 画面ファイル >>----*
     SELECT   DSPFILE   ASSIGN         01-GS-DSPF
                        FORMAT         DSP-FMT
                        GROUP          DSP-GRP
                        PROCESSING     DSP-PRO
                        UNIT CONTROL   DSP-CON
                        FUNCTION       DSP-FNC
                        STATUS         DSP-ST.
*----<< 返品累積ファイル >>----*
     SELECT   KEIJHRL2  ASSIGN         DA-01-VI-KEIJHRL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE DYNAMIC
                        RECORD    KEY  KEI-F03   KEI-F02
                                       KEI-F04   KEI-F05
                        STATUS         KEIJHRL2-ST.
*----<<商品変換テーブル１ >>----*
     SELECT   SHOTBL1   ASSIGN         DA-01-VI-SHOTBL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TB1-F01   TB1-F02
                        FILE STATUS    SHOTBL1-ST.
*----<<商品変換テーブル２ >>----*
     SELECT   SHOTBL2   ASSIGN              DA-01-VI-SHOTBL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TB2-F01
                                            TB2-F04
                                            TB2-F031
                                            TB2-F032
                        FILE STATUS         SHOTBL2-ST.
*----<<商品名称マスタ >>----*
     SELECT   MEIMS1    ASSIGN              DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F012
                        FILE STATUS         MEIMS1-ST.
*----<< 取引先マスタ >>----*
     SELECT   TOKMS2    ASSIGN              DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        STATUS              TOKMS2-ST.
*----<< 店舗マスタ >>----*
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE      STATUS    TENMS1-ST.
*----<< 倉庫マスタ >>----*
     SELECT   ZSOKMS1   ASSIGN              DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        STATUS              ZSOKMS1-ST.
*----<< 担当者マスタ >>----*
     SELECT   TANMS1    ASSIGN    TO        DA-01-VI-TANMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TAN-F01 TAN-F02
                        FILE      STATUS    TANMS1-ST.
*----<< 条件ファイル >>-*
     SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYOKEN1-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 画面ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSK00401  OF        XMDLIB.
*----<< 返品累積ファイル >>--*
 FD  KEIJHRL2           LABEL     RECORD   IS   STANDARD.
     COPY     KEIJHRF   OF        XFDLIB
              JOINING   KEI       PREFIX.
*----<< 商品変換テーブル１ >>--*
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TB1       PREFIX.
*----<< 商品変換テーブル２ >>--*
 FD  SHOTBL2            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TB2       PREFIX.
*----<< 商品名称マスタ >>--*
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*----<< 取引先マスタ >>--*
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 倉庫マスタ >>--*
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1   OF        XFDLIB
              JOINING   SOK       PREFIX.
*---<<  担当者マスタ  >>---*
 FD  TANMS1.
     COPY     TANMS1    OF        XFDLIB
              JOINING   TAN       PREFIX.
*----<< 条件ファイル >>--*
 FD  JYOKEN1            LABEL RECORD   IS   STANDARD.
     COPY     JYOKEN1   OF        XFDLIB
              JOINING   JYO       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  KOTEI.
     03  W-TORICD            PIC  9(08)  VALUE  173.
 01  FLAGS.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  KEP-FLG             PIC  X(01)  VALUE  SPACE.
     03  TANMS1-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  TOKMS2-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  SHOTBL1-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  SHOTBL2-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  MEIMS1-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  TENMS1-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  ZSOKMS1-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  JYOKEN1-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG             PIC  X(03)  VALUE  SPACE.
     03  WK-B-HIDUKE         PIC  9(08)  VALUE  ZERO.
     03  WK-B-TIME           PIC  9(04)  VALUE  ZERO.
     03  WK-B-TORICD         PIC  9(08)  VALUE  ZERO.
     03  WK-B-SOKCD          PIC  X(02)  VALUE  SPACE.
     03  ERK-FLG             PIC  9(01)  VALUE  ZERO.
     03  WK-NOUHIN           PIC  9(08)  VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
*01  DSP-ST            PIC  X(02).
 01  KEIJHRL2-ST       PIC  X(02).
 01  SHOTBL1-ST        PIC  X(02).
 01  SHOTBL2-ST        PIC  X(02).
 01  MEIMS1-ST         PIC  X(02).
 01  TOKMS2-ST         PIC  X(02).
 01  TENMS1-ST         PIC  X(02).
 01  ZSOKMS1-ST        PIC  X(02).
 01  TANMS1-ST         PIC  X(02).
 01  JYOKEN1-ST        PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*
 01  WK-SYS-DATE             PIC  9(08).
 01  FILLER                  REDEFINES   WK-SYS-DATE.
     03  WK-SYS-YY           PIC  9(04).
     03  WK-SYS-MM           PIC  9(02).
     03  WK-SYS-DD           PIC  9(02).
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
 01  WK-DEN113               PIC  9(08).
 01  FILLER                  REDEFINES      WK-DEN113.
     03  WK-DEN113YM         PIC  9(06).
     03  WK-DEN113D          PIC  9(02).
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
 01  SAV-NOU-DATE2           PIC  9(08)    VALUE ZERO.
 01  SAV-NOU-DATE.
     03  SAV-NOU-YYMM        PIC  9(06).
     03  SAV-NOU-DD          PIC  9(02).
 01  SAV-HNOU-DATE2          PIC  9(08)    VALUE ZERO.
 01  SAV-HNOU-DATE.
     03  SAV-HNOU-YYMM       PIC  9(06).
     03  SAV-HNOU-DD         PIC  9(02).
 01  SAV-HTENCD              PIC  9(05)    VALUE ZERO.
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
*
 01  INDEXES.
     03  I                   PIC  9(02).
     03  J                   PIC  9(02).
     03  K                   PIC  9(02).
     03  L                   PIC  9(02).
     03  T                   PIC  9(02).
     03  IL                  PIC  9(02).
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
 01  WK-GRP             PIC  X(08).
 01  DSP-CNTL.
     03  DSP-ST         PIC  X(02).
     03  DSP-ST2        PIC  X(04).
     03  DSP-FMT        PIC  X(08).
     03  DSP-GRP        PIC  X(08).
     03  DSP-PRO        PIC  X(02).
     03  DSP-FNC        PIC  X(04).
     03  DSP-CON        PIC  X(06).
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ﾃ-ﾌﾞﾙ >>-*
 01  FNC-TABLE.
     03  ENT            PIC  X(04)     VALUE     "E000".
     03  PF04           PIC  X(04)     VALUE     "F004".
     03  PF05           PIC  X(04)     VALUE     "F005".
     03  PF06           PIC  X(04)     VALUE     "F006".
     03  PF11           PIC  X(04)     VALUE     "F011".
     03  PF12           PIC  X(04)     VALUE     "F012".
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE00       PIC  N(40)  VALUE   NC"_終　了".
 01  GUIDE01       PIC  N(40)  VALUE   NC"_取　消　_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
     NC"_取　消　_終　了　_項目戻り".
 01  GUIDE03       PIC  N(40)  VALUE
     NC"_取　消　_終　了　_項目戻り　_次　頁".
 01  GUIDE04       PIC  N(40)  VALUE
     NC"_取　消　_終　了　_項目戻り　_前　頁".
 01  GUIDE05       PIC  N(40)  VALUE
     NC"_取　消　_終　了　_項目戻り　_前　頁　_次　頁".
*
 01  MSG-AREA.
     03  MSG01               PIC  N(30)  VALUE
              NC"ＰＦキーが違います".
     03  MSG02               PIC  N(30)  VALUE
              NC"検収日を入力して下さい。".
     03  MSG03               PIC  N(30)  VALUE
              NC"日付論理エラー".
     03  MSG04               PIC  N(30)  VALUE
              NC"店舗ＣＤを入力して下さい".
     03  MSG05               PIC  N(30)  VALUE
              NC"店舗マスタに未登録です".
     03  MSG06               PIC  N(30)  VALUE
              NC"伝票ＮＯを入力して下さい。".
     03  MSG07               PIC  N(30)  VALUE
              NC"対象データが存在しません".
     03  MSG08               PIC  N(30)  VALUE
              NC"既に計上済の伝票です".
     03  MSG09               PIC  N(30)  VALUE
              NC"入力された変更伝票区分は使用できません".
     03  MSG10               PIC  N(30)  VALUE
              NC"前頁はありません。".
     03  MSG11               PIC  N(30)  VALUE
              NC"次頁はありません。".
     03  MSG12               PIC  N(30)  VALUE
              NC"実検収日を入力して下さい".
     03  MSG13               PIC  N(30)  VALUE
              NC"実検収日がＡＣＯＳ締日以前です".
     03  MSG14               PIC  N(30)  VALUE
              NC"出場を入力してください".
     03  MSG15               PIC  N(30)  VALUE
              NC"倉庫マスタに未登録です".
     03  MSG16               PIC  N(30)  VALUE
              NC"請求区分エラー".
     03  MSG17               PIC  N(30)  VALUE
              NC"商品名称マスタに存在しません".
     03  MSG18               PIC  N(30)  VALUE
              NC"計上区分エラー".
     03  MSG19               PIC  N(30)  VALUE
              NC"Ｙを入力".
     03  MSG20               PIC  N(30)  VALUE
              NC"請求区分を入力して下さい　　".
     03  MSG21               PIC  N(30)  VALUE
              NC"商品名称マスタ未登録の明細があります。　".
     03  MSG22               PIC  N(30)  VALUE
              NC"変更伝区エラー　　　　　　　".
     03  MSG23               PIC  N(30)  VALUE
              NC"日付の入力が違います　　　　".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(30)  OCCURS      23.
*
 01  WRK-AREA.
*
     03  WRK-HEAD.
*          検収日（検収日（ケーヨー））
           05  WRK-KNDATE        PIC  9(08).
     03  WRK-HEADCHG.
*          伝区（伝票区分（ケーヨー））
           05  WRK-DKBN          PIC  9(02).
*          変更伝区（計上伝票区分）
           05  WRK-HDKBN         PIC  9(02).
*          実検収日（検収日（サカタ））
           05  WRK-JKNDAT        PIC  9(08).
*          出荷場所
           05  WRK-BASYOC        PIC  X(02).
*          請求区分
           05  WRK-SEIKBN        PIC  9(01).
*
     03  WRK-MEISAI          OCCURS    999.
*          行番号
           05  WRK-GYO           PIC  9(02).
*          ＪＡＮＣＤ
           05  WRK-JANCD         PIC  X(13).
*          サカタ商品ＣＤ
           05  WRK-SYOCD         PIC  X(08).
*          サカタ品単１
           05  WRK-HIN1          PIC  X(05).
*          サカタ品単２
           05  WRK-HIN2          PIC  X(02).
*          サカタ品単３
           05  WRK-HIN3          PIC  X(01).
*          商品名１
           05  WRK-MEI1          PIC  X(15).
*          商品名２
           05  WRK-MEI2          PIC  X(15).
*          数量（返品数量）
           05  WRK-SURYOU        PIC S9(05)V9.
*          原価単価
           05  WRK-GTAN          PIC S9(07)V99.
*          原価金額
           05  WRK-GKIN          PIC S9(09).
*          明細備考
           05  WRK-MBIKOU        PIC  X(10).
*
     03  WRK-TAIL.
*          摘要１（伝票備考１）
           05  WRK-TEKI1         PIC  X(15).
*          摘要２（伝票備考２）
           05  WRK-TEKI2         PIC  X(15).
*          計上区分　　
           05  WRK-KEIJO         PIC  X(01).
*
     03  WRK-BK.
*          計上区分（初期ＲＥＡＤ時）
           05  BK-KEIJO          PIC  X(01).
     03  WRK-GK.
*          計上区分（初期ＲＥＡＤ時）
           05  WRK-GKINT         PIC S9(10)  VALUE  ZERO.
*
 01  WRK-NAME.
     03  WRK-KANA            PIC  X(15)        VALUE SPACE.
*
*計算領域
 01  WRK-AREA2.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*
 01  CNT-AREA.
     03  IX                  PIC  9(04)  VALUE  ZERO.
     03  IY                  PIC  9(04)  VALUE  ZERO.
     03  IZ                  PIC  9(04)  VALUE  ZERO.
     03  CNT-MEISAI          PIC  9(04)  VALUE  ZERO.
     03  CNT-PAGE            PIC  9(04)  VALUE  ZERO.
     03  CNT-MAXPAGE         PIC  9(04)  VALUE  ZERO.
     03  AMARI               PIC  9(04)  VALUE  ZERO.
*
 01  SEC-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
***************************************************************
 LINKAGE                    SECTION.
*    PARA-IN
 01  PARA-BUMON             PIC X(04).
 01  PARA-TANTOU            PIC X(02).
*
****************************************************************
 PROCEDURE     DIVISION     USING   PARA-BUMON  PARA-TANTOU.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー制御　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 画面ファイル >>--*
 DSPFILE-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSPFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSK0040I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"     TO       PROGRAM-STATUS.
     STOP     RUN.
*----<< 返品累積ファイル >>--*
 KEIJHRL2-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KEIJHRL2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSK0040I KEIJHRL2 ERROR " KEIJHRL2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"     TO       PROGRAM-STATUS.
     STOP     RUN.
*----<< 商品変換テーブル１ >>--*
 SHOTBL1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHOTBL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSK0040I SHOTBL1 ERROR " SHOTBL1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"     TO       PROGRAM-STATUS.
     STOP     RUN.
*----<< 商品変換テーブル２ >>--*
 SHOTBL2-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHOTBL2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSK0040I SHOTBL2 ERROR " SHOTBL2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"     TO       PROGRAM-STATUS.
     STOP     RUN.
*----<< 商品名称マスタ >>--*
 MEIMS1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      MEIMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSK0040I MEIMS1 ERROR " MEIMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"     TO       PROGRAM-STATUS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 TOKMS2-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOKMS2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSK0040I TOKMS2 ERROR " TOKMS2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"     TO       PROGRAM-STATUS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 TENMS1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TENMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSK0040I TENMS1 ERROR " TENMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"     TO       PROGRAM-STATUS.
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 ZSOKMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSK0040I ZSOKMS1 ERROR " ZSOKMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"     TO       PROGRAM-STATUS.
     STOP     RUN.
*----<< 担当者マスタ >>--*
 TANMS1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TANMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSK0040I TANMS1 ERROR " TANMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"     TO       PROGRAM-STATUS.
     STOP     RUN.
*----<< 条件ファイル >>--*
 JYOKEN1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JYOKEN1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSK0040I JYOKEN1  ERROR " JYOKEN1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"     TO       PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     プログラムコントロール                      *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
*
     PERFORM  100-INIT-RTN.
*
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO    =    99.
*
     PERFORM  300-END-RTN.
*
     STOP RUN.
*
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      初期処理                                    *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*
     MOVE    "100-INIT-RTN"       TO   S-NAME.
*
     INITIALIZE                   WRK-AREA.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
         MOVE LINK-OUT-YMD8  TO   WK-SYS-DATE
     ELSE
         MOVE ZERO           TO   SYS-DATEW
         MOVE ZERO           TO   WK-SYS-DATE
     END-IF.
*
     MOVE     SYS-YYW        TO   WK-SYSYY.
     MOVE     SYS-MMW        TO   WK-SYSMM.
     MOVE     SYS-DDW        TO   WK-SYSDD.
*
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSK0040I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     I-O       KEIJHRL2.
     OPEN     INPUT     SHOTBL1.
     OPEN     INPUT     SHOTBL2.
     OPEN     INPUT     MEIMS1.
     OPEN     INPUT     TOKMS2.
     OPEN     INPUT     TENMS1.
     OPEN     INPUT     ZSOKMS1.
     OPEN     INPUT     TANMS1.
     OPEN     INPUT     JYOKEN1.
*
*条件Ｆ（経理月）
     MOVE     "58"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG  =  1
              DISPLAY   "HJYOKEN INV KEY=58"  UPON STAT
              MOVE      99        TO   GR-NO
              GO   TO   100-INIT-RTN-EXIT
     END-IF.
     MOVE     JYO-F04        TO   GETUDO.
     MOVE     JYO-F04        TO   GETUDO2.
*TEST↓
*    DISPLAY "-----------------------------" UPON CONS.
*    DISPLAY NC"取得：条件Ｆ（経理月）＝"  GETUDO   UPON CONS.
*    DISPLAY "-----------------------------" UPON CONS.
*TEST↑
*条件Ｆ（ＡＣＯＳ用締日）
     MOVE     "99"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG  =  1
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
*TEST↓
*    DISPLAY "-------------------------------------" UPON CONS.
*    DISPLAY NC"条件Ｆ（ＡＣＯＳ締日）＝"  ACOS-DATE UPON CONS.
*    DISPLAY "-------------------------------------" UPON CONS.
*TEST↑
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
     MOVE     SYS-YY         TO   START-YY
                                  END-YY.
     MOVE     SYS-MM         TO   START-MM
                                  END-MM.
     ADD      1              TO   END-MM.
     IF       END-MM  >  12
              MOVE      1         TO   END-MM
              COMPUTE   END-YY   =  WK-SYS-YY  +  1
     ELSE
              MOVE      WK-SYS-YY   TO      END-YY
     END-IF.
     SUBTRACT 1              FROM START-MM.
     IF       START-MM  <  1
              MOVE      12        TO   START-MM
              COMPUTE   START-YY  =  WK-SYS-YY  -  1
     ELSE
              MOVE      WK-SYS-YY      TO   START-YY
     END-IF.
*
     MOVE     GETUDO2-YYMMR  TO      START-YYMM.
*
*TEST↓
*    DISPLAY "------------------------------" UPON CONS.
*    DISPLAY NC"算出：開始日付＝"  START-YYMM UPON CONS.
*    DISPLAY NC"算出：終了日付＝"  END-YYMM   UPON CONS.
*    DISPLAY "------------------------------" UPON CONS.
*TEST↑
*
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   GR-NO.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      メイン処理                                  *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*
*----<< 画面初期表示 >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*
*----<< ヘッド　呼び出しＫＥＹ入力 >>-*
     PERFORM  220-INP-KEYGRP UNTIL     GR-NO    NOT  =    1.
*
*----<< ヘッド　変更可部の入力 >>-*
     PERFORM  220-INP-CHGGRP UNTIL     GR-NO    NOT  =    2.
*
*----<< 明細    変更・入力 >>-*
     PERFORM  220-INP-BODY   UNTIL     GR-NO    NOT  =    3.
*
*----<< ＴＡＩＬ　入力 >>-*
     PERFORM  220-INP-TAIL   UNTIL     GR-NO    NOT  =    4.
*
*----<< 確認入力 >>-*
     PERFORM  230-INP-ENDCHK UNTIL     GR-NO    NOT  =    9.
*
*----<< データ更新処理 >>-*
     PERFORM  240-UPDATE-SEC UNTIL     GR-NO    NOT  =    10.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      終了処理                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     CLOSE    DSPFILE.
     CLOSE    KEIJHRL2.
     CLOSE    SHOTBL1.
     CLOSE    SHOTBL2.
     CLOSE    MEIMS1.
     CLOSE    TOKMS2.
     CLOSE    TENMS1.
     CLOSE    ZSOKMS1.
     CLOSE    TANMS1.
     CLOSE    JYOKEN1.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSK0040I END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS     UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      画面初期表示                                *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
*
     MOVE    "210-DSP-INIT"       TO   S-NAME.
*
     MOVE     SPACE          TO   FSK00401.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-CHG-RTN.
     PERFORM  CLR-BODY-RTN.
     PERFORM  CLR-TAIL-RTN.
     PERFORM  CLR-END-RTN.
*
     MOVE    "SSK0040I"      TO   PGID.
     MOVE    "FSK00401"      TO   FORM.
*    担当者
     MOVE     PARA-BUMON     TO   TAN-F01.
     MOVE     PARA-TANTOU    TO   TANCD TAN-F02.
     PERFORM  TANMS1-READ-SEC.
     IF  TANMS1-INV-FLG = "INV"
         MOVE ALL NC"＊"     TO   TANNM
     ELSE
         MOVE TAN-F03        TO   TANNM
     END-IF.
*    取引先
     MOVE     W-TORICD       TO   TORICD TOK-F01.
     PERFORM  TOKMS2-READ-SEC.
     IF  TOKMS2-INV-FLG = "INV"
         MOVE ALL NC"＊"     TO   TORINM
     ELSE
         MOVE TOK-F03        TO   TORINM
     END-IF.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSK00401"     TO   DSP-FMT.
     MOVE     "ALLF"         TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
*
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ヘッド　呼び出しＫＥＹ入力                  *
*--------------------------------------------------------------*
 220-INP-KEYGRP         SECTION.
*
     MOVE     "220-INP-KEYGRP"    TO   S-NAME.
*
     MOVE     "KEYGRP"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF04
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN ENT
              INITIALIZE               WRK-AREA
              INITIALIZE               CNT-AREA
              PERFORM   CLR-HEAD-RTN
              PERFORM   CLR-CHG-RTN
              PERFORM   CLR-BODY-RTN
              PERFORM   CLR-TAIL-RTN
              PERFORM   220-KEYGRP-CHECK-SEC
              IF        ERR-FLG   =    ZERO
***                     画面セット処理
                        MOVE      1    TO   CNT-PAGE
                        PERFORM   GAMEN-SET-SEC
                        MOVE      2    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-KEYGRP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＫＥＹチェック                              *
*--------------------------------------------------------------*
 220-KEYGRP-CHECK-SEC    SECTION.
*
     MOVE     "220-KEYGRP-CHECK-SEC"    TO   S-NAME.

     MOVE     ZERO      TO        ERR-FLG.
*検収日チェック
*    　　　　未入力
     IF     ( KNDATE    IS NOT NUMERIC ) OR
            ( KNDATE    =      ZERO    )
              IF   ERR-FLG   =    ZERO
                   MOVE      2    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF KNDATE
              MOVE     "R"   TO   EDIT-OPTION OF KNDATE
     END-IF.
*    　　　　異常日付
     IF     ( KNDATE    IS NUMERIC     ) AND
            ( KNDATE    NOT =     ZERO )
              MOVE     "3"        TO        LINK-IN-KBN
              MOVE      KNDATE    TO        LINK-IN-YMD6
              CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              IF   LINK-OUT-RET   NOT =     ZERO
                   IF   ERR-FLG   =    ZERO
                        MOVE      3    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF KNDATE
                   MOVE     "R"   TO   EDIT-OPTION OF KNDATE
              ELSE
                   MOVE     LINK-OUT-YMD8   TO   WRK-KNDATE
              END-IF
     END-IF.
*店舗ＣＤチェック
     IF     ( TENCD     IS NUMERIC     ) AND
            ( TENCD     NOT =     ZERO )
              MOVE      SPACE     TO   TEN-REC
              INITIALIZE               TEN-REC
              MOVE      W-TORICD  TO   TEN-F52
              MOVE      TENCD     TO   TEN-F011
              PERFORM   TENMS1-READ-SEC
              IF    TENMS1-INV-FLG = "INV"
                    IF    ERR-FLG  =  ZERO
                          MOVE     5  TO   ERR-FLG
                    END-IF
                    MOVE  ALL NC"＊"  TO   TENNM
                    MOVE  "C"         TO   EDIT-CURSOR OF TENCD
                    MOVE  "R"         TO   EDIT-OPTION OF TENCD
              ELSE
                    MOVE  TEN-F03     TO   TENNM
              END-IF
     ELSE
              IF  ERR-FLG   =    ZERO
                  MOVE      4    TO   ERR-FLG
              END-IF
              MOVE    "C"   TO   EDIT-CURSOR OF TENCD
              MOVE    "R"   TO   EDIT-OPTION OF TENCD
     END-IF.
*伝票ＮＯチェック
*    　　　　未入力
     IF     ( DENNO     IS NOT NUMERIC ) OR
            ( DENNO     =      ZERO    )
              IF   ERR-FLG   =    ZERO
                   MOVE      6    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF DENNO
              MOVE     "R"   TO   EDIT-OPTION OF DENNO
     END-IF.
*
*返品累積ファイル存在チェック
     IF  ERR-FLG        =    ZERO
         PERFORM   220-SONZAI-CHECK
         IF   ERR-FLG   =    ZERO
              MOVE      ZERO      TO   IX
*             対象データ退避処理
              PERFORM   220-TAIHI-SEC
***
              IF   CNT-MEISAI     =    ZERO
                   MOVE      7    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF KNDATE
                   MOVE     "R"   TO   EDIT-OPTION OF KNDATE
                   MOVE     "R"   TO   EDIT-OPTION OF TENCD
                   MOVE     "R"   TO   EDIT-OPTION OF DENNO
                   GO   TO   220-KEYGRP-CHECK-EXIT
              END-IF
***
              IF        CNT-MEISAI     <=   6
                   MOVE      1         TO   CNT-MAXPAGE
              ELSE
                   DIVIDE    CNT-MEISAI     BY   6
                             GIVING    CNT-MAXPAGE
                             REMAINDER      AMARI
                   IF   AMARI     NOT =     ZERO
                        ADD  1    TO   CNT-MAXPAGE
                   END-IF
              END-IF
         END-IF
     END-IF.
*
 220-KEYGRP-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ヘッド（変更可部）～ＴＡＩＬ　              *
*--------------------------------------------------------------*
 GAMEN-SET-SEC          SECTION.
*
     MOVE    "GAMEN-SET-SEC"      TO   S-NAME.
*
*--------------------
*  ヘッド変更可部
*--------------------
*    伝区（ケーヨー）
     MOVE     WRK-DKBN            TO   DKBN.
*    伝区名---条件Ｆ（KEY=49）
     MOVE     49             TO   JYO-F01.
     MOVE     DKBN           TO   JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG = "INV"
              MOVE  NC"売上伝票"  TO   DKBNNM
     ELSE
              MOVE  JYO-F03       TO   DKBNNM
     END-IF.
*    変更伝区（サカタ）
     MOVE     WRK-HDKBN           TO   HDKBN.
*    伝区名---条件Ｆ（KEY=01）
     MOVE     01             TO   JYO-F01.
     MOVE     HDKBN          TO   JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG = "INV"
              MOVE  ALL NC"＊"    TO   HDENKN
     ELSE
              MOVE  JYO-F03       TO   HDENKN
     END-IF.
*    実検収日
     MOVE     WRK-JKNDAT          TO   JKNDAT.
*    出場
     MOVE     WRK-BASYOC          TO   BASYOC  SOK-F01.
     PERFORM  ZSOKMS1-READ-SEC.
     IF       ZSOKMS1-INV-FLG = "INV"
              MOVE  ALL NC"＊"    TO   BASYON
     ELSE
              MOVE  SOK-F02       TO   BASYON
     END-IF.
*    請求区分
     MOVE     WRK-SEIKBN          TO   SEIKBN.
*-------------
*  明細
*-------------
     COMPUTE  IY   =    CNT-PAGE  *    6    -    6.
     PERFORM  VARYING   IZ   FROM  1   BY   1    UNTIL IZ > 6
         COMPUTE   IX   =    IY   +    IZ
         IF ( WRK-GYO  (IX)  NOT =     ZERO )
              MOVE SPACE               TO   BDY   (IZ)
              MOVE WRK-GYO   (IX)      TO   GYO   (IZ)
              MOVE WRK-JANCD (IX)      TO   JANCD (IZ)
              MOVE WRK-SYOCD (IX)      TO   SYOCD (IZ)
              MOVE WRK-HIN1  (IX)      TO   HIN1  (IZ)
              MOVE WRK-HIN2  (IX)      TO   HIN2  (IZ)
              MOVE WRK-HIN3  (IX)      TO   HIN3  (IZ)
              MOVE WRK-MEI1  (IX)      TO   MEI1  (IZ)
              MOVE WRK-MEI2  (IX)      TO   MEI2  (IZ)
              MOVE WRK-SURYOU(IX)      TO   SURYOU(IZ)
              MOVE WRK-GTAN  (IX)      TO   GTAN  (IZ)
              MOVE WRK-GKIN  (IX)      TO   GKIN  (IZ)
              MOVE WRK-MBIKOU(IX)      TO   MBIKOU(IZ)
         ELSE
              MOVE SPACE               TO   BDY   (IZ)
              MOVE "X"  TO EDIT-STATUS OF   SYOCD (IZ)
              MOVE "X"  TO EDIT-STATUS OF   HIN1  (IZ)
              MOVE "X"  TO EDIT-STATUS OF   HIN2  (IZ)
              MOVE "X"  TO EDIT-STATUS OF   HIN3  (IZ)
              MOVE "X"  TO EDIT-STATUS OF   MEI1  (IZ)
              MOVE "X"  TO EDIT-STATUS OF   MEI2  (IZ)
              MOVE "X"  TO EDIT-STATUS OF   SURYOU(IZ)
              MOVE "X"  TO EDIT-STATUS OF   GTAN  (IZ)
              MOVE "X"  TO EDIT-STATUS OF   MBIKOU(IZ)
         END-IF
     END-PERFORM.
*-------------
*  ＴＡＩＬ
*-------------
*    摘要１（伝票備考１）
     MOVE     WRK-TEKI1           TO   TEKI1.
*    摘要２（伝票備考２）
     MOVE     WRK-TEKI2           TO   TEKI2.
*    計上区分
     MOVE     WRK-KEIJO           TO   KEIJO.
*    原価金額合計
     MOVE     WRK-GKINT           TO   GKINT.
*
 GAMEN-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      返品累積データ存在チェック                  *
*--------------------------------------------------------------*
 220-SONZAI-CHECK       SECTION.
     MOVE    "220-SONZAI-CHECK"  TO   S-NAME.
*
     MOVE     SPACE          TO   KEI-REC.
     INITIALIZE                   KEI-REC.
*             検収日
     MOVE     WRK-KNDATE     TO   KEI-F03.
*             店舗ＣＤ
     MOVE     TENCD          TO   KEI-F02.
*             伝票ＮＯ
     MOVE     DENNO          TO   KEI-F04.
*             行
     MOVE     ZERO           TO   KEI-F05.
*
     START    KEIJHRL2  KEY  >=   KEI-F03   KEI-F02
                                  KEI-F04   KEI-F05
*        対象データなし
         INVALID   KEY
              MOVE      7    TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF KNDATE
              MOVE     "R"        TO   EDIT-OPTION OF KNDATE
              MOVE     "R"        TO   EDIT-OPTION OF TENCD
              MOVE     "R"        TO   EDIT-OPTION OF DENNO
              GO   TO   220-SONZAI-CHECK-EXIT
         NOT INVALID
              READ      KEIJHRL2    NEXT
*               対象データなし
                AT END
                   MOVE      7    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF KNDATE
                   MOVE     "R"   TO   EDIT-OPTION OF KNDATE
                   MOVE     "R"   TO   EDIT-OPTION OF TENCD
                   MOVE     "R"   TO   EDIT-OPTION OF DENNO
                   GO   TO   220-SONZAI-CHECK-EXIT
                NOT AT END
*                  ＫＥＹチェック
                   IF ( WRK-KNDATE =    KEI-F03 ) AND
                      ( TENCD      =    KEI-F02 ) AND
                      ( DENNO      =    KEI-F04 )
                        CONTINUE
                   ELSE
                        MOVE      7    TO   ERR-FLG
                        MOVE "C"  TO   EDIT-CURSOR OF KNDATE
                        MOVE "R"  TO   EDIT-OPTION OF KNDATE
                        MOVE "R"  TO   EDIT-OPTION OF TENCD
                        MOVE "R"  TO   EDIT-OPTION OF DENNO
                        GO   TO   220-SONZAI-CHECK-EXIT
                   END-IF
*                  計上済（計上区分・計上日）チェック
                   IF  ( KEI-F80       = "1" ) AND
                       ( KEI-F86   NOT =  0  )
                        MOVE      8    TO   ERR-FLG
                        MOVE "C"  TO   EDIT-CURSOR OF KNDATE
                        MOVE "R"  TO   EDIT-OPTION OF KNDATE
                        MOVE "R"  TO   EDIT-OPTION OF TENCD
                        MOVE "R"  TO   EDIT-OPTION OF DENNO
                        GO   TO   220-SONZAI-CHECK-EXIT
                   ELSE
                        CONTINUE
                   END-IF
              END-READ
     END-START.
 220-SONZAI-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      返品累積データ　ワークへの退避              *
*--------------------------------------------------------------*
 220-TAIHI-SEC          SECTION.
     MOVE     "220-TAIHI-SEC"     TO   S-NAME.
*
     IF     ( WRK-KNDATE =    KEI-F03 ) AND
            ( TENCD      =    KEI-F02 ) AND
            ( DENNO      =    KEI-F04 )
*           レコードカウント
              ADD       1         TO   IX
*           ヘッド（変更可部）
*             伝区（伝票区分（ケーヨー））
              MOVE      KEI-F06   TO   WRK-DKBN
*             変更伝区（計上伝票区分）
              MOVE      KEI-F23   TO   WRK-HDKBN
*             実検収日（検収日（サカタ））
              MOVE      KEI-F07   TO   WRK-JKNDAT
*             出場（出荷場所）
              MOVE      KEI-F08   TO   WRK-BASYOC
*             請求区分
              MOVE      KEI-F22   TO   WRK-SEIKBN
*           明細
*             行（行番号）
              MOVE      KEI-F05   TO   WRK-GYO    (IX)
*             ＪＡＮＣＤ（ＪＡＮＣＤ）
              MOVE      KEI-F09   TO   WRK-JANCD  (IX)
*             サカタ商品ＣＤ（サカタ商品ＣＤ）
              MOVE      KEI-F10   TO   WRK-SYOCD  (IX)
*             品単１（サカタ品単１）
              MOVE      KEI-F11   TO   WRK-HIN1   (IX)
*             品単２（サカタ品単２）
              MOVE      KEI-F12   TO   WRK-HIN2   (IX)
*             品単３（サカタ品単３）
              MOVE      KEI-F13   TO   WRK-HIN3   (IX)
*             商品名１（商品名１）
              MOVE      KEI-F14   TO   WRK-MEI1   (IX)
*             商品名２（商品名２）
              MOVE      KEI-F15   TO   WRK-MEI2   (IX)
*             数量（返品数量）
              MOVE      KEI-F16   TO   WRK-SURYOU (IX)
*             原価単価（原価単価）
              MOVE      KEI-F17   TO   WRK-GTAN   (IX)
*             原価金額（原価金額）
              MOVE      KEI-F18   TO   WRK-GKIN   (IX)
*             明細備考（明細備考）
              MOVE      KEI-F21   TO   WRK-MBIKOU (IX)
*           ＴＡＩＬ
*             摘要１（伝票備考１）
              MOVE      KEI-F24   TO   WRK-TEKI1
*             摘要２（伝票備考２）
              MOVE      KEI-F25   TO   WRK-TEKI2
*             原価金額合計
              ADD       KEI-F18   TO   WRK-GKINT
*             計上区分　　
              MOVE      KEI-F80   TO   WRK-KEIJO
                                       BK-KEIJO
*
              GO        TO        220-TAIHI-010
     ELSE
              MOVE      IX        TO   CNT-MEISAI
              GO        TO        220-TAIHI-EXIT
     END-IF.
*
 220-TAIHI-010.
*
     READ     KEIJHRL2  NEXT
         AT   END
              MOVE      IX        TO   CNT-MEISAI
         NOT AT END
              GO   TO   220-TAIHI-SEC
     END-READ.
*
 220-TAIHI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ヘッド　変更可部の入力                      *
*--------------------------------------------------------------*
 220-INP-CHGGRP         SECTION.
*
     MOVE     "220-INP-CHGGRP"    TO   S-NAME.
*
     MOVE     "CHGGRP"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
     MOVE     ZERO      TO        ERR-FLG.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF06
              PERFORM   CLR-CHG-RTN
              PERFORM   CLR-BODY-RTN
              MOVE      1         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-CHG-RTN
              PERFORM   CLR-BODY-RTN
              PERFORM   220-CHGGRP-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      3    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE           1    TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-CHGGRP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ヘッド　変更可部　入力チェック              *
*--------------------------------------------------------------*
 220-CHGGRP-CHECK-SEC    SECTION.
*
     MOVE     "220-CHGGRP-CHECK-SEC"    TO   S-NAME.
*
     MOVE     ZERO      TO        ERR-FLG.
*
 220-CHGGRP-CHECK-01.
*変更伝区チェック
 220-CHGGRP-CHECK-01-01.
*伝区４１・４２のみＯＫ
     IF       HDKBN           =   41  OR  42
              CONTINUE
     ELSE
              IF   ERR-FLG   =    ZERO
                   MOVE  9             TO   ERR-FLG
              END-IF
              MOVE  "C"           TO   EDIT-CURSOR OF HDKBN
              MOVE  "R"           TO   EDIT-OPTION OF HDKBN
     END-IF.
 220-CHGGRP-CHECK-01-02.
*    条件Ｆ（KEY=01）
     MOVE     01             TO   JYO-F01.
     MOVE     HDKBN          TO   JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG = "INV"
              IF   ERR-FLG   =    ZERO
                   MOVE  ALL NC"＊"    TO   HDENKN
                   MOVE  22            TO   ERR-FLG
              END-IF
              MOVE  "C"           TO   EDIT-CURSOR OF HDKBN
              MOVE  "R"           TO   EDIT-OPTION OF HDKBN
     ELSE
              MOVE  JYO-F03       TO   HDENKN
              MOVE  HDKBN         TO   WRK-HDKBN
     END-IF.
*
 220-CHGGRP-CHECK-03.
*実検収日チェック
 220-CHGGRP-CHECK-03-01.
*   未入力
     IF     ( JKNDAT    IS NOT NUMERIC ) OR
            ( JKNDAT    =      ZERO    )
              IF   ERR-FLG   =    ZERO
                   MOVE      12   TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF JKNDAT
              MOVE     "R"   TO   EDIT-OPTION OF JKNDAT
     END-IF.
 220-CHGGRP-CHECK-03-02.
*   異常日付
     IF     ( JKNDAT    IS NUMERIC     ) AND
            ( JKNDAT    NOT =     ZERO )
              MOVE     "3"        TO        LINK-IN-KBN
              MOVE      JKNDAT    TO        LINK-IN-YMD6
              CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              IF   LINK-OUT-RET   NOT =     ZERO
                   IF   ERR-FLG   =    ZERO
                        MOVE      3    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF JKNDAT
                   MOVE     "R"   TO   EDIT-OPTION OF JKNDAT
              ELSE
                   MOVE     LINK-OUT-YMD8   TO   WRK-JKNDAT
              END-IF
     END-IF.
*
 220-CHGGRP-CHECK-03-03.
*   規定範囲外日付
     MOVE     WRK-JKNDAT      TO   CHK-DATE.
*   規定範囲外日付１
*TEST↓
*    DISPLAY "-----------------------------" UPON CONS.
*    DISPLAY NC"規定範囲外日付_"            UPON CONS.
*    DISPLAY NC"許可開始年月＝" START-YYMM   UPON CONS.
*    DISPLAY NC"許可終了年月＝" END-YYMM     UPON CONS.
*    DISPLAY NC"実検収日　　＝" CHK-YYMM     UPON CONS.
*TEST↑
     IF     ( WRK-JKNDAT      NOT  =  0      )  AND
            ( CHK-YYMM    <   START-YYMM     )  OR
            ( CHK-YYMM    >   END-YYMM       )
              IF   ERR-FLG    =  ZERO
                   MOVE       23   TO   ERR-FLG
              END-IF
              MOVE "C"  TO   EDIT-CURSOR OF JKNDAT
              MOVE "R"  TO   EDIT-OPTION OF JKNDAT
*TEST↓
*             DISPLAY NC"－－－＞　ＮＧ　"  UPON CONS
*             DISPLAY "-----------------------------" UPON CONS
*TEST↑
              GO        TO    220-CHGGRP-CHECK-04
*TEST↓
*    ELSE
*             DISPLAY NC"－－－＞　ＯＫ　"  UPON CONS
*             DISPLAY "-----------------------------" UPON CONS
*TEST↑
     END-IF.
*
 220-CHGGRP-CHECK-03-04.
*   規定範囲外日付２
*TEST↓
*    DISPLAY "-----------------------------" UPON CONS.
*    DISPLAY NC"規定範囲外日付_"            UPON CONS.
*    DISPLAY NC"ＡＣＯＳ締日＝" ACOS-DATE    UPON CONS.
*    DISPLAY NC"システム日付＝" SYS-DATEW    UPON CONS.
*    DISPLAY NC"実検収日年月＝" CHK-YYMM     UPON CONS.
*    DISPLAY NC"ＡＣＯＳ年月＝" ACOS-YYMM    UPON CONS.
*TEST↑
     IF     ( WRK-JKNDAT      NOT  =  0      )  AND
            ( ACOS-DATE       <  SYS-DATEW   )  AND
*           ( WK-DEN112YM     <  ACOS-YYMM   )
            ( CHK-YYMM        <  ACOS-YYMM   )
              IF   ERR-FLG    =  ZERO
                   MOVE       13   TO   ERR-FLG
              END-IF
              MOVE "C"  TO   EDIT-CURSOR OF JKNDAT
              MOVE "R"  TO   EDIT-OPTION OF JKNDAT
*TEST↓
*             DISPLAY NC"－－－＞　ＮＧ　"  UPON CONS
*             DISPLAY "-----------------------------" UPON CONS
*    ELSE
*             DISPLAY NC"－－－＞　ＯＫ　"  UPON CONS
*             DISPLAY "-----------------------------" UPON CONS
*TEST↑
     END-IF.
*
 220-CHGGRP-CHECK-04.
*出荷場所チェック
*        倉庫マスタ存在チェック
     IF  BASYOC    NOT =   SPACE
         INITIALIZE               SOK-REC
         MOVE      BASYOC    TO   SOK-F01
         PERFORM   ZSOKMS1-READ-SEC
         IF  ZSOKMS1-INV-FLG = "INV"
             IF   ERR-FLG   =    ZERO
                  MOVE      15   TO   ERR-FLG
             END-IF
             MOVE ALL NC"＊"     TO   BASYON
             MOVE "C"            TO   EDIT-CURSOR OF BASYOC
             MOVE "R"            TO   EDIT-OPTION OF BASYOC
         ELSE
             MOVE SOK-F02        TO   BASYON
             MOVE BASYOC         TO   WRK-BASYOC
         END-IF
     ELSE
*        未入力
         IF   ERR-FLG   =    ZERO
              MOVE      14   TO   ERR-FLG
         END-IF
         MOVE ALL NC"＊"     TO   BASYON
         MOVE "C"            TO   EDIT-CURSOR OF BASYOC
         MOVE "R"            TO   EDIT-OPTION OF BASYOC
     END-IF.
*
 220-CHGGRP-CHECK-05.
*請求区分チェック
 220-CHGGRP-CHECK-05-01.
*    　　　　未入力
     IF     ( SEIKBN    IS NOT NUMERIC )
              IF   ERR-FLG   =    ZERO
                   MOVE      20   TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF SEIKBN
              MOVE     "R"   TO   EDIT-OPTION OF SEIKBN
     END-IF.
 220-CHGGRP-CHECK-05-02.
*    条件Ｆ（KEY=48）
     MOVE     48             TO   JYO-F01.
     MOVE     SEIKBN         TO   JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG = "INV"
              IF   ERR-FLG   =    ZERO
                   MOVE  16            TO   ERR-FLG
              END-IF
              MOVE  "C"           TO   EDIT-CURSOR OF SEIKBN
              MOVE  "R"           TO   EDIT-OPTION OF SEIKBN
     ELSE
              MOVE   SEIKBN       TO   WRK-SEIKBN
     END-IF.
*
 220-CHGGRP-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      明細　　変更・入力                          *
*--------------------------------------------------------------*
 220-INP-BODY           SECTION.
*
     MOVE     "220-INP-BODY"      TO   S-NAME.
*
     MOVE     "BODY"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
     MOVE     ZERO      TO        ERR-FLG.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF06
              PERFORM   GAMEN-TAIHI-SEC
              PERFORM   CLR-CHG-RTN
              PERFORM   CLR-BODY-RTN
              MOVE      2         TO   GR-NO
         WHEN PF11
              PERFORM 900-DSP-WRITE2
              PERFORM   MEISAI-CHK-SEC
              IF   ERR-FLG  =  ZERO
                   PERFORM   CLR-CHG-RTN
                   PERFORM   CLR-BODY-RTN
                   IF   CNT-PAGE  <=   1
                        PERFORM   GAMEN-TAIHI-SEC
                        MOVE      10   TO   ERR-FLG
                   ELSE
                        PERFORM   GAMEN-TAIHI-SEC
                        IF   ERR-FLG   =    ZERO
                             ADD       -1   TO   CNT-PAGE
                             PERFORM   GAMEN-SET-SEC
                        END-IF
                   END-IF
              END-IF
         WHEN PF12
              PERFORM 900-DSP-WRITE2
              PERFORM   MEISAI-CHK-SEC
              IF   ERR-FLG  =  ZERO
                   PERFORM   CLR-CHG-RTN
                   PERFORM   CLR-BODY-RTN
                   IF   CNT-PAGE  =    CNT-MAXPAGE
                        PERFORM   GAMEN-TAIHI-SEC
                        MOVE      11   TO   ERR-FLG
                   ELSE
                        PERFORM   GAMEN-TAIHI-SEC
                        IF   ERR-FLG   =    ZERO
                             ADD       1    TO   CNT-PAGE
                             PERFORM   GAMEN-SET-SEC
                        END-IF
                   END-IF
              END-IF
         WHEN ENT
              PERFORM   GAMEN-TAIHI-SEC
              PERFORM   220-WRKTBL-CHECK-SEC
              PERFORM   GAMEN-SET-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      4    TO   GR-NO
                        PERFORM   CLR-CHG-RTN
                        PERFORM   CLR-BODY-RTN
              END-IF
         WHEN OTHER
              MOVE           1    TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      明細チェック（テーブル全明細）              *
*--------------------------------------------------------------*
 220-WRKTBL-CHECK-SEC    SECTION.
*
     MOVE     "220-WRKTBL-CHECK-SEC"    TO   S-NAME.
*
     MOVE     ZERO      TO        ERR-FLG.
*
*サカタコード→名称マスタ存在チェック
     PERFORM  VARYING  IZ   FROM  1  BY  1
                            UNTIL    IZ  >  CNT-MEISAI
              MOVE     WRK-SYOCD(IZ)     TO     MEI-F01
              MOVE     WRK-HIN1 (IZ)     TO     MEI-F0121
              MOVE     WRK-HIN2 (IZ)     TO     MEI-F0122
              MOVE     WRK-HIN3 (IZ)     TO     MEI-F0123
              PERFORM  MEIMS1-READ-SEC
              IF       MEIMS1-INV-FLG   =  "INV"
                  IF   ERR-FLG   =    ZERO
                       MOVE  21  TO   ERR-FLG
                       MOVE  "C" TO   EDIT-CURSOR OF SYOCD(01)
                  END-IF
              ELSE
                  IF   WRK-MEI1(IZ)  =    SPACE
                       MOVE  MEI-F031 TO   WRK-MEI1(IZ)
                       MOVE  MEI-F032 TO   WRK-MEI2(IZ)
                  END-IF
              END-IF
     END-PERFORM.
*
 220-WRKTBL-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ変更・入力                          *
*--------------------------------------------------------------*
 220-INP-TAIL           SECTION.
*
     MOVE     "220-INP-TAIL"      TO   S-NAME.
*
     MOVE     "TEKIYO"  TO        WK-GRP.
     PERFORM  900-DSP-READ.
     MOVE     ZERO      TO        ERR-FLG.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF06
              PERFORM   GAMEN-TAIHI-SEC
              PERFORM   CLR-BODY-RTN
              PERFORM   CLR-TAIL-RTN
              MOVE      3         TO   GR-NO
         WHEN ENT
              PERFORM   GAMEN-TAIHI-SEC
              PERFORM   220-TAIL-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
                        PERFORM   CLR-BODY-RTN
                        PERFORM   CLR-TAIL-RTN
              END-IF
         WHEN OTHER
              MOVE           1    TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-TAIL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬチェック　　　　　　　　　　        *
*--------------------------------------------------------------*
 220-TAIL-CHECK-SEC    SECTION.
*
     MOVE     "220-TAIL-CHECK-SEC"    TO   S-NAME.
*
     MOVE     ZERO      TO        ERR-FLG.
*
*計上区分チェック
*    条件Ｆ（KEY=47）
     MOVE     47             TO   JYO-F01.
     MOVE     KEIJO          TO   JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG = "INV"
              MOVE  ALL NC"＊"    TO   KEIJON
              MOVE  18            TO   ERR-FLG
              MOVE  "C"           TO   EDIT-CURSOR OF KEIJO
              MOVE  "R"           TO   EDIT-OPTION OF KEIJO
     ELSE
              MOVE  JYO-F03       TO   KEIJON
     END-IF.

*
 220-TAIL-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      画面項目退避                                *
*--------------------------------------------------------------*
 GAMEN-TAIHI-SEC        SECTION.
*
     MOVE     "GAMEN-TAIHI-SEC"   TO   S-NAME.
*
     PERFORM  900-DSP-WRITE2.
*    MOVE     JTUNOU    TO   WRK-JTUNOU.
     COMPUTE  IY   =    CNT-PAGE  *   6     -     6.
     PERFORM  VARYING   IZ   FROM  1  BY  1  UNTIL  IZ  >  6
         COMPUTE   IX   =    IY   +    IZ
              IF   GYO(IZ)   IS NUMERIC
*                明細
*                  行（行番号）
                   MOVE      GYO  (IZ)  TO   WRK-GYO    (IX)
*                  ＪＡＮＣＤ
                   MOVE      JANCD (IZ) TO   WRK-JANCD  (IX)
*                  サカタ商品ＣＤ
                   MOVE      SYOCD (IZ) TO   WRK-SYOCD  (IX)
*                  品単１
                   MOVE      HIN1  (IZ) TO   WRK-HIN1   (IX)
*                  品単２
                   MOVE      HIN2  (IZ) TO   WRK-HIN2   (IX)
*                  品単３
                   MOVE      HIN3  (IZ) TO   WRK-HIN3   (IX)
*                  商品名１
                   MOVE      MEI1  (IZ) TO   WRK-MEI1   (IX)
*                  商品名２
                   MOVE      MEI2  (IZ) TO   WRK-MEI2   (IX)
*                  数量
                   MOVE      SURYOU(IZ) TO   WRK-SURYOU (IX)
*                  原価単価
                   MOVE      GTAN  (IZ) TO   WRK-GTAN   (IX)
*                  原価金額
                   COMPUTE   GKIN  (IZ) =    SURYOU(IZ) * GTAN(IZ)
                   MOVE      GKIN  (IZ) TO   WRK-GKIN   (IX)
*                  明細備考
                   MOVE      MBIKOU(IZ) TO   WRK-MBIKOU (IX)
              ELSE
                   MOVE      "X"  TO   EDIT-STATUS OF GYO   (IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF JANCD (IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF SYOCD (IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF HIN1  (IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF HIN2  (IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF HIN3  (IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF MEI1  (IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF MEI2  (IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF SURYOU(IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF GTAN  (IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF GKIN  (IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF MBIKOU(IZ)
              END-IF
     END-PERFORM.
*
*  ＴＡＩＬ
*    摘要１
     MOVE      TEKI1     TO   WRK-TEKI1
*    摘要２
     MOVE      TEKI2     TO   WRK-TEKI2
*    計上区分　　
     MOVE      KEIJO     TO   WRK-KEIJO
*    COMPUTE  SAI  =    WRK-HSUKEI     -    WRK-JTUNOU.
*    原価金額合計　再集計
     MOVE     ZERO           TO   WRK-GKINT.
     PERFORM  VARYING   IX   FROM      1    BY   1
                             UNTIL     IX   >    CNT-MEISAI
         ADD  WRK-GKIN(IX)   TO   WRK-GKINT
*TEST↓
*        DISPLAY NC"原価金額＝" WRK-GKIN(IX) UPON CONS
*        DISPLAY NC"原価金額合計＝" WRK-GKINT UPON CONS
*TEST↑
     END-PERFORM.
     MOVE     WRK-GKINT      TO   GKINT.
*    MOVE     WRK-JTUNOU     TO   JTUNOU.
*
 GAMEN-TAIHI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認入力　　　                              *
*--------------------------------------------------------------*
 230-INP-ENDCHK         SECTION.
*
     MOVE     "230-INP-ENDCHK"    TO   S-NAME.
*
     MOVE     "ENDCHK"            TO   WK-GRP.
     PERFORM   900-DSP-READ.
     PERFORM   GAMEN-TAIHI-SEC.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF06
              MOVE      4         TO   GR-NO
         WHEN ENT
              MOVE      ZERO      TO   ERR-FLG
              PERFORM   CLR-TAIL-RTN
              IF        ENDCHK    NOT =    "Y"
                        MOVE      19   TO   ERR-FLG
              ELSE
                        MOVE      10   TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 230-INP-ENDCHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      返品計上累積データ更新                      *
*--------------------------------------------------------------*
 240-UPDATE-SEC           SECTION.
*
     MOVE     "240-UPDATE-SEC"    TO   S-NAME.
*
 240-UPDATE-00.
*TEST↓
*    MOVE     "240-UPDATE-00"    TO   S-NAME.
*TEST↑
     MOVE     1              TO   IX.
*
 240-UPDATE-01.
*TEST↓
*    MOVE     "240-UPDATE-01"    TO   S-NAME.
*TEST↑
     IF       IX             >    CNT-MEISAI
              GO             TO   240-UPDATE-99
     END-IF.
*
 240-UPDATE-02.
*TEST↓
*    MOVE     "240-UPDATE-02"    TO   S-NAME.
*TEST↑
     MOVE     SPACE          TO   KEI-REC.
     INITIALIZE                   KEI-REC.
*             検収日
     MOVE     WRK-KNDATE     TO   KEI-F03.
*             店舗ＣＤ
     MOVE     TENCD          TO   KEI-F02.
*             伝票ＮＯ
     MOVE     DENNO          TO   KEI-F04.
*             行
     MOVE     WRK-GYO(IX)    TO   KEI-F05.
*
     START    KEIJHRL2  KEY  >=   KEI-F03   KEI-F02
                                  KEI-F04   KEI-F05
*        対象データなし
         INVALID   KEY
              DISPLAY  "***********************************"
                                                     UPON CONS
              DISPLAY  "SSK0040I  KEIJHRL2 START INVALID ]]"
                                                     UPON CONS
              DISPLAY  "***********************************"
                                                     UPON CONS
              GO   TO   240-UPDATE-SEC-EXIT
     END-START.
*
 240-UPDATE-03.
*TEST↓
*    MOVE     "240-UPDATE-03"    TO   S-NAME.
*TEST↑
*
     READ     KEIJHRL2  NEXT
         AT END
              GO   TO   240-UPDATE-SEC-EXIT
         NOT AT END
              IF ( WRK-KNDATE     =    KEI-F03 ) AND
                 ( TENCD          =    KEI-F02 ) AND
                 ( DENNO          =    KEI-F04 ) AND
                 ( WRK-GYO(IX)    =    KEI-F05 )
*                  計上済（計上区分・計上日）チェック
                   IF  ( KEI-F80       = 1 ) AND
                       ( KEI-F86   NOT = 0 )
                         DISPLAY
                             "***********************************"
                                                       UPON CONS
                         DISPLAY
                           NC"返品累積データが異常です！"
                                                       UPON CONS
                         DISPLAY
                           NC"既に計上済になっています！"
                                                       UPON CONS
                         DISPLAY  NC"検収日＝" KNDATE  UPON CONS
                         DISPLAY  NC"店舗Ｃ＝" TENCD   UPON CONS
                         DISPLAY  NC"伝票番＝" DENNO   UPON CONS
                         DISPLAY  NC"行番号＝" WRK-GYO(IX)
                                                       UPON CONS
                         GO   TO   240-UPDATE-SEC-EXIT
                   ELSE
                         GO   TO   240-UPDATE-04
                   END-IF
              ELSE
                   GO   TO   240-UPDATE-SEC-EXIT
              END-IF
     END-READ.
*
 240-UPDATE-04.
*TEST↓
*    MOVE     "240-UPDATE-04"    TO   S-NAME.
*TEST↑
*  検収日（サカタ）
     MOVE     WRK-JKNDAT      TO       KEI-F07.
*  出荷場所
     MOVE     WRK-BASYOC      TO       KEI-F08.
*  ＪＡＮＣＤ
     MOVE     WRK-JANCD(IX)   TO       KEI-F09.
*  サカタ商品ＣＤ
     MOVE     WRK-SYOCD(IX)   TO       KEI-F10.
*  サカタ品単１
     MOVE     WRK-HIN1   (IX) TO       KEI-F11.
*  サカタ品単２
     MOVE     WRK-HIN2   (IX) TO       KEI-F12.
*  サカタ品単３
     MOVE     WRK-HIN3   (IX) TO       KEI-F13.
*  商品名１
     MOVE     WRK-MEI1   (IX) TO       KEI-F14.
*  商品名２
     MOVE     WRK-MEI2   (IX) TO       KEI-F15.
*  返品数量
     MOVE     WRK-SURYOU (IX) TO       KEI-F16.
*  原価単価
     MOVE     WRK-GTAN   (IX) TO       KEI-F17.
*  原価金額
     MOVE     WRK-GKIN   (IX) TO       KEI-F18.
*  明細備考
     MOVE     WRK-MBIKOU (IX) TO       KEI-F21.
*  請求区分
     MOVE     WRK-SEIKBN      TO       KEI-F22.
*  計上伝票区分
     MOVE     WRK-HDKBN       TO       KEI-F23.
*  伝票備考１
     MOVE     WRK-TEKI1       TO       KEI-F24.
*  伝票備考２
     MOVE     WRK-TEKI2       TO       KEI-F25.
*  棚番  ※商品変換ＴＢＬ１ｏｒ４からセット
     MOVE     173             TO       TB1-F01.
     MOVE     WRK-JANCD(IX)   TO       TB1-F02.
     PERFORM  SHOTBL1-READ-SEC
     IF       SHOTBL1-INV-FLG = "   "
              MOVE    TB1-F08          TO       KEI-F26
     ELSE
              MOVE    173              TO       TB2-F01
*↓変更2014.4.10
*↓TEST
*             DISPLAY "BASYOC=" WRK-BASYOC UPON CONS
*↑TEST
              MOVE    WRK-BASYOC       TO       TB2-F04
*↑変更2014.4.10
              MOVE    WRK-SYOCD(IX)    TO       TB2-F031
              MOVE    WRK-HIN1 (IX)    TO       TB2-F0321
              MOVE    WRK-HIN2 (IX)    TO       TB2-F0322
              MOVE    WRK-HIN3 (IX)    TO       TB2-F0323
              PERFORM SHOTBL2-READ-SEC
              IF      SHOTBL2-INV-FLG = "   "
                      MOVE    TB2-F08  TO       KEI-F26
              END-IF
     END-IF.
*  計上区分　　
     MOVE     WRK-KEIJO       TO       KEI-F80.
*  入力担当者部門ＣＤ
*    ※計上区分が初期表示時から変更された場合のみ
     IF       BK-KEIJO   NOT =         WRK-KEIJO
              MOVE       PARA-BUMON    TO  KEI-F81
     END-IF.
*  入力担当者ＣＤ
*    ※計上区分が初期表示時から変更された場合のみ
     IF       BK-KEIJO   NOT =         WRK-KEIJO
              MOVE       PARA-TANTOU   TO  KEI-F82
     END-IF.
*  入力日
*    ※計上区分が初期表示時から変更された場合のみ
     IF       BK-KEIJO   NOT =         WRK-KEIJO
              MOVE       SYS-DATEW     TO  KEI-F83
     END-IF.
*  更新日付
     MOVE     SYS-DATEW        TO      KEI-F96.
*  更新時刻
     MOVE     SYS-TIMEW        TO      KEI-F97.
*  更新部門ＣＤ
     MOVE     PARA-BUMON       TO      KEI-F98.
*  更新担当者ＣＤ
     MOVE     PARA-TANTOU      TO      KEI-F99.
*
 240-UPDATE-05.
*TEST↓
*    MOVE     "240-UPDATE-05"    TO   S-NAME.
*TEST↑
*TEST↓
*    DISPLAY NC"更新対象レコード"  UPON CONS.
*    DISPLAY NC"ケーヨー検収日＝" KEI-F03 UPON CONS.
*    DISPLAY NC"店舗コード　　＝" KEI-F02 UPON CONS.
*    DISPLAY NC"伝票番号　　　＝" KEI-F04 UPON CONS.
*    DISPLAY NC"行番号　　　　＝" KEI-F05 UPON CONS.
*TEST↑
     REWRITE  KEI-REC.
     ADD      1         TO        IX.
     GO                 TO        240-UPDATE-01.
*
 240-UPDATE-99.
*TEST↓
*    MOVE     "240-UPDATE-99"    TO   S-NAME.
*TEST↑
*画面初期化ＳＥＣへ
     MOVE     0                   TO   GR-NO.
*
 240-UPDATE-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     画面ＷＲＩＴＥ　全項目                      *
*--------------------------------------------------------------*
 900-DSP-WRITE2         SECTION.
*
     MOVE     "ALLF"              TO   DSP-GRP.
     MOVE     WK-SYSYMD           TO   SDATE.
     ACCEPT   SYS-TIME2           FROM TIME.
     MOVE     SYS-TIMEW           TO   STIME.
*
     PERFORM  900-DSP-WRITE.
*
 900-DSP-WRITE2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     画面　ＲＥＡＤ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
*
     MOVE     "900-DSP-READ"      TO   S-NAME.
*
     MOVE     "ALLF"         TO   DSP-GRP.
     IF       ERR-FLG   =    0
              MOVE      SPACE               TO   MSGSPC
              MOVE      "D"      TO   EDIT-OPTION OF MSGSPC
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   MSGSPC
     END-IF.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   FNCSPC
     ELSE
              IF   GR-NO     =    2    OR   4   OR   9
                   MOVE      GUIDE02   TO   FNCSPC
              END-IF
              IF   GR-NO     =    3
                   IF   CNT-MEISAI     <=   6
                        MOVE      GUIDE02   TO   FNCSPC
                   ELSE
                        IF   CNT-PAGE  <=   1
                             MOVE      GUIDE03   TO   FNCSPC
                        ELSE
                             IF   CNT-PAGE  =    CNT-MAXPAGE
                                  MOVE      GUIDE04   TO   FNCSPC
                             ELSE
                                  MOVE      GUIDE05   TO   FNCSPC
                             END-IF
                        END-IF
                   END-IF
              END-IF
     END-IF.
     MOVE     WK-SYSYMD           TO   SDATE.
     ACCEPT   SYS-TIME2           FROM TIME.
     MOVE     SYS-TIMEW           TO   STIME.
*
     PERFORM  900-DSP-WRITE.
*
     IF       MSGSPC    NOT  =    SPACE
              MOVE "AL"      TO   DSP-PRO
     ELSE
              MOVE "NE"      TO   DSP-PRO
     END-IF.
     MOVE     SPACE          TO   MSGSPC.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE          TO   DSP-PRO.
*
 900-DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     画面　ＷＲＩＴＥ                            *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
*
     MOVE     "900-DSP-WRITE"     TO   S-NAME.
*
     MOVE     SPACE               TO   DSP-PRO.
     WRITE    FSK00401.
*
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF KNDATE
                                  EDIT-CURSOR OF TENCD
                                  EDIT-CURSOR OF DENNO.
     MOVE     "M"            TO   EDIT-OPTION OF KNDATE
                                  EDIT-OPTION OF TENCD
                                  EDIT-OPTION OF DENNO.
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　変更部　属性クリア　　　　　　　　*
*--------------------------------------------------------------*
 CLR-CHG-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF DKBN
                                  EDIT-CURSOR OF HDKBN
                                  EDIT-CURSOR OF JKNDAT
                                  EDIT-CURSOR OF BASYOC
                                  EDIT-CURSOR OF SEIKBN.
     MOVE     "M"            TO   EDIT-OPTION OF DKBN
                                  EDIT-OPTION OF HDKBN
                                  EDIT-OPTION OF JKNDAT
                                  EDIT-OPTION OF BASYOC
                                  EDIT-OPTION OF SEIKBN.
 CLR-CHG-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ属性クリア　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY-RTN          SECTION.
*
     PERFORM  VARYING   IX   FROM  1   BY   1    UNTIL  IX > 6
         MOVE     " "        TO   EDIT-CURSOR OF GYO(IX)
         MOVE     "M"        TO   EDIT-OPTION OF GYO(IX)
         MOVE     " "        TO   EDIT-STATUS OF GYO(IX)
         MOVE     " "        TO   EDIT-CURSOR OF SYOCD(IX)
         MOVE     "M"        TO   EDIT-OPTION OF SYOCD(IX)
         MOVE     " "        TO   EDIT-STATUS OF SYOCD(IX)
         MOVE     " "        TO   EDIT-CURSOR OF HIN1(IX)
         MOVE     "M"        TO   EDIT-OPTION OF HIN1(IX)
         MOVE     " "        TO   EDIT-STATUS OF HIN1(IX)
         MOVE     " "        TO   EDIT-CURSOR OF HIN2(IX)
         MOVE     "M"        TO   EDIT-OPTION OF HIN2(IX)
         MOVE     " "        TO   EDIT-STATUS OF HIN2(IX)
         MOVE     " "        TO   EDIT-CURSOR OF HIN3(IX)
         MOVE     "M"        TO   EDIT-OPTION OF HIN3(IX)
         MOVE     " "        TO   EDIT-STATUS OF HIN3(IX)
         MOVE     " "        TO   EDIT-CURSOR OF MEI1(IX)
         MOVE     "M"        TO   EDIT-OPTION OF MEI1(IX)
         MOVE     " "        TO   EDIT-STATUS OF MEI1(IX)
         MOVE     " "        TO   EDIT-CURSOR OF MEI2(IX)
         MOVE     "M"        TO   EDIT-OPTION OF MEI2(IX)
         MOVE     " "        TO   EDIT-STATUS OF MEI2(IX)
         MOVE     " "        TO   EDIT-CURSOR OF SURYOU(IX)
         MOVE     "M"        TO   EDIT-OPTION OF SURYOU(IX)
         MOVE     " "        TO   EDIT-STATUS OF SURYOU(IX)
         MOVE     " "        TO   EDIT-CURSOR OF GTAN(IX)
         MOVE     "M"        TO   EDIT-OPTION OF GTAN(IX)
         MOVE     " "        TO   EDIT-STATUS OF GTAN(IX)
         MOVE     " "        TO   EDIT-CURSOR OF MBIKOU(IX)
         MOVE     "M"        TO   EDIT-OPTION OF MBIKOU(IX)
         MOVE     " "        TO   EDIT-STATUS OF MBIKOU(IX)
     END-PERFORM.
*
 CLR-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF TEKI1.
     MOVE     "M"        TO   EDIT-OPTION OF TEKI1.
     MOVE     " "        TO   EDIT-CURSOR OF TEKI2.
     MOVE     "M"        TO   EDIT-OPTION OF TEKI2.
     MOVE     " "        TO   EDIT-CURSOR OF KEIJO.
     MOVE     "M"        TO   EDIT-OPTION OF KEIJO.
     MOVE     " "        TO   EDIT-CURSOR OF ENDCHK.
     MOVE     "M"        TO   EDIT-OPTION OF ENDCHK.
 CLR-TAIL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＥＮＤ　　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-END-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF ENDCHK.
     MOVE     "M"        TO   EDIT-OPTION OF ENDCHK.
 CLR-END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細チェック（現在表示画面）　　                          *
*--------------------------------------------------------------*
 MEISAI-CHK-SEC         SECTION.
*
     MOVE     "MEISAI-CHK-SEC"         TO   S-NAME.
*
     MOVE      ZERO                    TO   ERR-FLG.
*
*サカタコード→名称マスタ存在チェック
     PERFORM  VARYING  IZ   FROM  1  BY  1
              UNTIL  ( IZ  >  6   OR    GYO(IZ) = 0 )
              MOVE     SYOCD(IZ)  TO    MEI-F01
              MOVE     HIN1 (IZ)  TO    MEI-F0121
              MOVE     HIN2 (IZ)  TO    MEI-F0122
              MOVE     HIN3 (IZ)  TO    MEI-F0123
              PERFORM  MEIMS1-READ-SEC
              IF       MEIMS1-INV-FLG   =  "INV"
                  IF   ERR-FLG   =    ZERO
                       MOVE  17  TO   ERR-FLG
                       MOVE  "C" TO   EDIT-CURSOR OF SYOCD (IZ)
                  END-IF
                  MOVE     "R"   TO   EDIT-OPTION OF SYOCD (IZ)
                  MOVE     "R"   TO   EDIT-OPTION OF HIN1  (IZ)
                  MOVE     "R"   TO   EDIT-OPTION OF HIN2  (IZ)
                  MOVE     "R"   TO   EDIT-OPTION OF HIN3  (IZ)
              ELSE
*                 商品名１が空白の場合
                  IF   MEI1(IZ)       =    SPACE
                       MOVE  MEI-F031 TO   MEI1(IZ)
                       MOVE  MEI-F032 TO   MEI2(IZ)
                  END-IF
              END-IF
     END-PERFORM.
*
 MEISAI-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    店舗マスタ　読込
*--------------------------------------------------------------*
 TENMS1-READ-SEC       SECTION.
*
     READ  TENMS1
           INVALID
           MOVE  "INV"  TO   TENMS1-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   TENMS1-INV-FLG
     END-READ.
*
 TENMS1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    担当者マスタ　読込
*--------------------------------------------------------------*
 TANMS1-READ-SEC       SECTION.
*
     READ  TANMS1
           INVALID
           MOVE  "INV"  TO   TANMS1-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   TANMS1-INV-FLG
     END-READ.
*
 TANMS1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    取引先マスタ　読込
*--------------------------------------------------------------*
 TOKMS2-READ-SEC       SECTION.
*
     READ  TOKMS2
           INVALID
           MOVE  "INV"  TO   TOKMS2-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   TOKMS2-INV-FLG
     END-READ.
*
 TOKMS2-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    条件ファイル　読込
*--------------------------------------------------------------*
 JYOKEN1-READ-SEC       SECTION.
*
     READ  JYOKEN1
           INVALID
           MOVE  "INV"  TO   JYOKEN1-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   JYOKEN1-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    倉庫マスタ　読込
*--------------------------------------------------------------*
 ZSOKMS1-READ-SEC       SECTION.
*
     READ  ZSOKMS1
           INVALID
           MOVE  "INV"  TO   ZSOKMS1-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   ZSOKMS1-INV-FLG
     END-READ.
*
 ZSOKMS1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    商品名称マスタ読込
*--------------------------------------------------------------*
 MEIMS1-READ-SEC       SECTION.
*
     READ  MEIMS1
           INVALID
           MOVE  "INV"  TO   MEIMS1-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   MEIMS1-INV-FLG
     END-READ.
*
 MEIMS1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    商品変換ＴＢＬ１読込
*--------------------------------------------------------------*
 SHOTBL1-READ-SEC       SECTION.
*
     READ  SHOTBL1
           INVALID
           MOVE  "INV"  TO   SHOTBL1-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   SHOTBL1-INV-FLG
     END-READ.
*
 SHOTBL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    商品変換ＴＢＬ４読込
*--------------------------------------------------------------*
 SHOTBL2-READ-SEC       SECTION.
*
     READ  SHOTBL2
           INVALID
           MOVE  "INV"  TO   SHOTBL2-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   SHOTBL2-INV-FLG
     END-READ.
*
 SHOTBL2-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
