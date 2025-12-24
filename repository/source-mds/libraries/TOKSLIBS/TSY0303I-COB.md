# TSY0303I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/TSY0303I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信システム　　　　　　　　　　*
*    業務名　　　　　　　：　訂正按分入力                      *
*    モジュール名　　　　：　訂正按分入力                      *
*    作成日／更新日　　　：　99/09/20                          *
*    作成者／更新者　　　：　ＮＡＶ吉田　　　　　　　　　　　　*
*    処理概要　　　　　　：　画面より、バッチ_、倉庫コード
*                            商品コードを入力し、該当の商品    *
*                            のデータを表示させ、発注数の訂    *
*　　　　　　　　　　　　　　正按分入力を行う。　　　　　　　　*
*                            10/05/25 欠品区分に追加　高橋　   *
*                            11/10/12 YOSHIDA.M                *
*                                     基幹サーバ統合           *
*      2013/11/27 高橋/NAV 障害修正　　　　 　　　　　　　
*　    　明細の２ページ目以降でＰＦ６を行なうと明細数量が
*　    　異常な値になる現象を修正
*        ＬＩＮＫＳ連携対応
*      2018/03/14 高橋/NAV 受注残Ｆ更新
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            TSY0303I.
 AUTHOR.                NAV Y.YOSHIDA.
 DATE-WRITTEN.          99/09/20.
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
*----<< 表示ファイル >>--*
     SELECT   DSPFILE   ASSIGN         01-GS-DSPF
                        FORMAT         DSP-FMT
                        GROUP          DSP-GRP
                        PROCESSING     DSP-PRO
                        UNIT CONTROL   DSP-CON
                        FUNCTION       DSP-FNC
                        STATUS         DSP-ST.
*----<< 伝票データ >>--*
     SELECT   SHTDENLB  ASSIGN         DA-01-VI-SHTDENLB
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F46   DEN-F47
                                       DEN-F01   DEN-F48
                                       DEN-F25   DEN-F07
                                       DEN-F02
                        STATUS         SHTDENLB-ST.
*----<< 伝票データ >>--*
     SELECT   SHTDENLA  ASSIGN         DA-01-VI-SHTDENLA
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DE2-F46   DE2-F47
                                       DE2-F01   DE2-F48
                                       DE2-F02   DE2-F04
***2011.10.12 ST
                                       DE2-F051  DE2-F07
                                       DE2-F112  DE2-F03
***2011.10.12 EN
                        STATUS         SHTDENLA-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 倉庫マスタ >>--*
     SELECT   ZSOKMS1   ASSIGN         DA-01-VI-ZSOKMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SOK-F01
                        STATUS         ZSOKMS1-ST.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN         DA-01-VI-SHOTBL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TBL-F01   TBL-F02
                        FILE STATUS    SHOTBL1-ST.
*商品名称マスタ
     SELECT   MEIMS1    ASSIGN         DA-01-VI-MEIMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  MEI-F011
                                       MEI-F012
                        FILE STATUS    MEIMS1-ST.
*----<< 店舗マスタ >>-*
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE      STATUS    TENMS1-ST.
*---<<  商品在庫マスタ  >>---*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-ST.
*---<<  欠品明細マスタ  >>---*
     SELECT   MSTKEPF   ASSIGN    TO        DA-01-VI-MSTKEPL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   KEP-F01
                        FILE      STATUS    IS   KEP-ST.
*---<<  欠品明細マスタ  >>---*
     SELECT   MSTKMEF   ASSIGN    TO        DA-01-VI-MSTKMEL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   KME-F01
                                                 KME-F02
                        FILE      STATUS    IS   KME-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     MSY03031  OF        XMDLIB.
*----<< 伝票データ >>--*
 FD  SHTDENLB           LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 伝票データ >>--*
 FD  SHTDENLA           LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DE2       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 倉庫マスタ >>--*
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1   OF        XFDLIB
              JOINING   SOK       PREFIX.
*----<< 商品変換テーブル >>--*
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
*----<< 商品名称マスタ >>--*
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*----<< 店舗マスタ >>--*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*---<<  商品在庫マスタ  >>---*
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*----<< 欠品名称マスタ >>--*
 FD  MSTKEPF            LABEL RECORD   IS   STANDARD.
     COPY     MSTKEPF   OF        XFDLIB
              JOINING   KEP       PREFIX.
*---<<  欠品明細マスタ  >>---*
 FD  MSTKMEF.
     COPY     MSTKMEF   OF        XFDLIB
              JOINING   KME       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  KEP-FLG             PIC  X(01)  VALUE  SPACE.
     03  MEIMST-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  MSTKEPF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  MSTKMEF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG             PIC  X(03)  VALUE  SPACE.
     03  WK-B-HIDUKE         PIC  9(08)  VALUE  ZERO.
     03  WK-B-TIME           PIC  9(04)  VALUE  ZERO.
     03  WK-B-TORICD         PIC  9(08)  VALUE  ZERO.
     03  WK-B-SOKCD          PIC  X(02)  VALUE  SPACE.
     03  ERK-FLG             PIC  9(01)  VALUE  ZERO.
     03  WK-NOUHIN           PIC  9(08)  VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHTDENLB-ST       PIC  X(02).
 01  SHTDENLA-ST       PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  ZSOKMS1-ST        PIC  X(02).
 01  SHOTBL1-ST        PIC  X(02).
 01  MEIMS1-ST         PIC  X(02).
 01  TENMS1-ST         PIC  X(02).
 01  ZAI-ST            PIC  X(02).
 01  KEP-ST            PIC  X(02).
 01  KME-ST            PIC  X(02).
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
              NC"バッチ_を入力して下さい。".
     03  MSG03               PIC  N(30)  VALUE
              NC"バッチ_に誤りがあります。".
     03  MSG04               PIC  N(30)  VALUE
              NC"取引先コードが違います。".
     03  MSG05               PIC  N(30)  VALUE
              NC"売上伝票データに存在しません。".
     03  MSG06               PIC  N(30)  VALUE
              NC"取引先コードを入力して下さい。".
     03  MSG07               PIC  N(30)  VALUE
              NC"訂正商品が違います。".
     03  MSG08               PIC  N(30)  VALUE
              NC"倉庫が違います。".
     03  MSG09               PIC  N(30)  VALUE
              NC"訂正商品を入力して下さい。".
     03  MSG10               PIC  N(30)  VALUE
              NC"前頁はありません。".
     03  MSG11               PIC  N(30)  VALUE
              NC"次頁はありません。".
     03  MSG12               PIC  N(30)  VALUE
              NC"実納品数と訂正後発注数が一致してません。".
     03  MSG13               PIC  N(30)  VALUE
              NC"Ｙを入力".
     03  MSG14               PIC  N(30)  VALUE
              NC"訂正後発注数が発注数を超えています。".
     03  MSG15               PIC  N(30)  VALUE
              NC"実納品数が発注数量を超えています。".
     03  MSG16               PIC  N(30)  VALUE
     NC"全ての発注数量を”０”にしました。確認して下さい。".
     03  MSG17               PIC  N(30)  VALUE
              NC"納品日に誤りがあります。".
     03  MSG18               PIC  N(30)  VALUE
     NC"入力の納品日は、売上伝票データに存在しません。".
****10/01/20  欠品区分追加開始　大野
     03  MSG19               PIC  N(30)  VALUE
     NC"欠品制御区分には空白か１を入力して下さい。".
****10/01/20  欠品区分追加終了　大野
     03  MSG20               PIC  N(30)  VALUE
     NC"欠品商品です。欠品区分を入力して下さい。".
     03  MSG21               PIC  N(30)  VALUE
     NC"欠品入力対象外です。他入力で修正して下さい。".
     03  MSG22               PIC  N(30)  VALUE
     NC"欠品明細マスタに存在しません。確認して下さい。".
     03  MSG23               PIC  N(30)  VALUE
     NC"欠品の場合、代表の欠品区分を入力して下さい。".
*
*
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(30)  OCCURS      23.
*
 01  WRK-AREA.
     03  WRK-MEISAI          OCCURS    999.
       05  WRK-TENPO         PIC  9(05).
       05  WRK-HSUU          PIC S9(07)V99.
       05  WRK-TSUU          PIC S9(07)V99.
       05  WRK-DENNO         PIC  9(09).
       05  WRK-GYONO         PIC  9(02).
       05  WRK-MAESUU        PIC S9(07)V99.
       05  WRK-NOHIN         PIC  9(08).
       05  WRK-ERR           PIC  9(01).
*2010/5/25 追加
       05  WRK-KEPPN         PIC  X(01).
       05  WRK-MAEKEP        PIC  X(01).
*
     03  WRK-HSUKEI          PIC S9(07)V99.
     03  WRK-TSUKEI          PIC S9(07)V99.
     03  WRK-JTUNOU          PIC S9(07)V99.
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
*#2018/03/14 NAV ST
*受注残ファイル更新用ワーク
 01  PARA-AREA.
     03  PARA-KOSINKBN         PIC X(01).
     03  PARA-DTKBN            PIC X(01).
     03  PARA-TOKCD            PIC 9(08).
     03  PARA-DENNO            PIC 9(09).
     03  PARA-GYO              PIC 9(02).
     03  PARA-SOUSAI           PIC 9(01).
     03  PARA-DENKU            PIC 9(02).
     03  PARA-TENCD            PIC 9(05).
     03  PARA-SOKCD            PIC X(02).
     03  PARA-HATYU            PIC 9(08).
     03  PARA-NOUHIN           PIC 9(08).
     03  PARA-SYUKA            PIC 9(08).
     03  PARA-SAKATACD         PIC X(08).
     03  PARA-HINTAN1          PIC X(05).
     03  PARA-HINTAN2          PIC X(02).
     03  PARA-HINTAN3          PIC X(01).
     03  PARA-TANABAN          PIC X(06).
     03  PARA-JYUTYUSU         PIC 9(09).
     03  PARA-SYUKASU          PIC 9(09).
     03  PARA-GENKA            PIC 9(09).
     03  PARA-BAIKA            PIC 9(09).
     03  PARA-GENKAKIN         PIC 9(09).
     03  PARA-BAIKAKIN         PIC 9(09).
     03  PARA-AITECD           PIC X(13).
     03  PARA-HIKIATE          PIC X(01).
     03  PARA-UPBUMON          PIC X(04).
     03  PARA-UPTANCD          PIC X(02).
     03  PARA-JDATE            PIC 9(08).
     03  PARA-JTIME            PIC 9(04).
   01  PARA-BUMON              PIC X(04).
   01  PARA-TANCD              PIC X(02).
*#2018/03/14 NAV ED
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 表示ファイル >>--*
 DSPFILE-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSPFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY0303I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    ZSOKMS1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   MSTKEPF   MSTKMEF.
     STOP     RUN.
*----<< 伝票データ >>--*
 SHTDENLB-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENLB.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY0303I SHTDENLB ERROR " SHTDENLB-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    ZSOKMS1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   MSTKEPF   MSTKMEF.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY0303I HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    ZSOKMS1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   MSTKEPF   MSTKMEF.
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 ZSOKMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY0303I ZSOKMS1 ERROR " ZSOKMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    ZSOKMS1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   MSTKEPF   MSTKMEF.
     STOP     RUN.
*----<< 商品変換テーブル >>--*
 SHOTBL1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHOTBL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY0303I SHOTBL1 ERROR " SHOTBL1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    ZSOKMS1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   MSTKEPF   MSTKMEF.
     STOP     RUN.
*----<< 商品名称マスタ >>--*
 MEIMS1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      MEIMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY0303I MEIMS1 ERROR " MEIMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    ZSOKMS1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   MSTKEPF   MSTKMEF.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 TENMS1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TENMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY0303I TENMS1 ERROR " TENMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    ZSOKMS1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   MSTKEPF   MSTKMEF.
     STOP     RUN.
*----<< 在庫マスタ >>--*
 ZAMZAIF-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZAMZAIF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY0303I TENMS1 ERROR " ZAI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    ZSOKMS1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   MSTKEPF   MSTKMEF.
     STOP     RUN.
*----<< 欠品名称マスタ >>--*
 MSTKEPF-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      MSTKEPF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY0303I MSTKEPL1 ERROR " KEP-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    ZSOKMS1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   MSTKEPF   MSTKMEF.
     STOP     RUN.
*----<< 欠品明細マスタ >>--*
 MSTKMEF-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      MSTKMEF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY0303I MSTKMEL1 ERROR " KEP-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    ZSOKMS1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   MSTKEPF   MSTKMEF.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
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
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     SYS-YYW        TO   WK-SYSYY.
     MOVE     SYS-MMW        TO   WK-SYSMM.
     MOVE     SYS-DDW        TO   WK-SYSDD.
*
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** TSY0303I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     SHTDENLB.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     ZSOKMS1.
     OPEN     INPUT     SHOTBL1.
     OPEN     INPUT     MEIMS1.
     OPEN     INPUT     TENMS1.
     OPEN     INPUT     MSTKEPF  MSTKMEF.
     OPEN     I-O       SHTDENLA.
     OPEN     I-O       ZAMZAIF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   GR-NO.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*----<< ﾊﾝｲ ｼﾃｲ ｶﾞﾒﾝ ｸﾘｱ >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< ﾊﾞｯﾁNO. ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ﾒｲｻｲ    ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP02  UNTIL     GR-NO    NOT  =    2.
*----<< ﾒｲｻｲ    ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP03  UNTIL     GR-NO    NOT  =    3.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ｳﾘｱｹﾞ ﾃﾞﾝﾋﾟｮｳ ｺｳｼﾝ ｼｮﾘ >>-*
     PERFORM  240-UPDATE-SEC UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    SHTDENLB.
     CLOSE    HTOKMS.
     CLOSE    ZSOKMS1.
     CLOSE    SHOTBL1.
     CLOSE    MEIMS1.
     CLOSE    TENMS1.
     CLOSE    SHTDENLA.
     CLOSE    MSTKEPF.
     CLOSE    MSTKMEF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** TSY0303I END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE    "210-DSP-INIT"       TO   S-NAME.
     MOVE     SPACE          TO   MSY03031.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-BODY1-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "TSY0303I"      TO   PGID.
     MOVE    "MSY03031"      TO   FORM.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "MSY03031"     TO   DSP-FMT.
     MOVE     "SCREFX"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*毎回バッチ番号を入力させない。
     IF        CHK-FLG  =  SPACE
               MOVE   " "    TO   EDIT-STATUS OF JDATE
               MOVE   " "    TO   EDIT-STATUS OF JTIME
               MOVE   " "    TO   EDIT-STATUS OF TORICD
               MOVE   " "    TO   EDIT-STATUS OF SOKO
     ELSE
               MOVE   "X"    TO   EDIT-STATUS OF JDATE
               MOVE   "X"    TO   EDIT-STATUS OF JTIME
               MOVE   "X"    TO   EDIT-STATUS OF TORICD
               MOVE   "X"    TO   EDIT-STATUS OF SOKO
               MOVE WK-B-HIDUKE TO JDATE
               MOVE WK-B-TIME   TO JTIME
               MOVE WK-B-TORICD TO TORICD
               MOVE WK-B-SOKCD  TO SOKO
     END-IF.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO.  ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "GRP001"       TO   WK-GRP.
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
              PERFORM   CLR-BODY1-RTN
              PERFORM   220-GRP01-CHECK-SEC
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
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO. ﾁｪｯｸ                                *
*--------------------------------------------------------------*
 220-GRP01-CHECK-SEC    SECTION.
     MOVE     "220-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
     MOVE     SPACE     TO        MEIMST-INV-FLG.
*
     IF     ( JDATE     =    ZERO ) AND
            ( JTIME     =    ZERO ) AND
            ( TORICD    =    ZERO )
              MOVE      2         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JTIME
              MOVE     "R"        TO   EDIT-OPTION OF TORICD
     END-IF.
*    受信日付チェック
     IF     ( JDATE     IS NUMERIC     ) AND
            ( JDATE     NOT =     ZERO )
              MOVE     "2"        TO        LINK-IN-KBN
              MOVE      JDATE     TO        LINK-IN-YMD8
              CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              IF   LINK-OUT-RET   NOT =     ZERO
                   IF   ERR-FLG   =    ZERO
                        MOVE      3    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JDATE
              END-IF
     END-IF.
*    受信時間
     IF  JTIME     NOT NUMERIC
         MOVE      ZERO      TO   JTIME
     END-IF.
*    取引先チェック
     IF  TORICD    IS NUMERIC     AND
         TORICD    NOT =     ZERO
         MOVE      SPACE     TO   TOK-REC
         INITIALIZE               TOK-REC
         MOVE      TORICD    TO   TOK-F01
         READ      HTOKMS
             INVALID
                   IF   ERR-FLG   =    ZERO
                        MOVE      4    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF TORICD
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
         END-READ
     ELSE
         IF   ERR-FLG   =    ZERO
              MOVE      6    TO   ERR-FLG
         END-IF
         MOVE     "C"   TO   EDIT-CURSOR OF TORICD
         MOVE     "R"   TO   EDIT-OPTION OF TORICD
     END-IF.
*    欠品区分入力対象取引先か判断
     IF  TORICD    IS NUMERIC     AND
         TORICD    NOT =     ZERO
         MOVE      SPACE     TO   TOK-REC
         INITIALIZE               TOK-REC
         MOVE      TORICD    TO   KEP-F01
         PERFORM  MSTKEPF-READ-SEC
         IF  MSTKEPF-INV-FLG = "INV"
             IF   ERR-FLG   =    ZERO
                  MOVE      21   TO   ERR-FLG
             END-IF
             MOVE     "C"   TO   EDIT-CURSOR OF TORICD
             MOVE     "R"   TO   EDIT-OPTION OF TORICD
         END-IF
     END-IF.
*    倉庫コードチェック
*****IF     ( SOKO      IS   NUMERIC   )   AND
*****       ( SOKO      NOT =     ZERO )
     IF       SOKO      NOT =     SPACE
              MOVE      SOKO      TO   SOK-F01
              READ      ZSOKMS1
                  INVALID  KEY
                        MOVE      SPACE     TO   SOKONM
                        IF   ERR-FLG   =    ZERO
                             MOVE      8    TO   ERR-FLG
                        END-IF
                        MOVE     "C"   TO   EDIT-CURSOR OF SOKO
                        MOVE     "R"   TO   EDIT-OPTION OF SOKO
                  NOT INVALID  KEY
                        MOVE      SOK-F02   TO   SOKONM
              END-READ
     ELSE
              MOVE      SPACE     TO   SOKONM
              IF        ERR-FLG   =    ZERO
                        MOVE      8    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF SOKO
              MOVE     "R"   TO   EDIT-OPTION OF SOKO
     END-IF.
*    相手商品コードチェック
     IF  SHOCD     NOT =     SPACE
*        商品変換テーブル検索
         MOVE      SPACE     TO        TBL-REC
         INITIALIZE                    TBL-REC
         MOVE      TORICD    TO        TBL-F01
         MOVE      SHOCD     TO        TBL-F02
         READ      SHOTBL1
             INVALID
                   MOVE      SPACE     TO   SMEI1     SMEI2
                   IF   ERR-FLG   =    ZERO
                        MOVE      7    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF SHOCD
                   MOVE     "R"   TO   EDIT-OPTION OF SHOCD
             NOT INVALID
*                  商品名称マスタ検索
                   MOVE      SPACE     TO   MEI-REC
                   INITIALIZE               MEI-REC
                   MOVE      TBL-F031  TO   MEI-F011
                   MOVE      TBL-F032  TO   MEI-F012
                   READ      MEIMS1
                        INVALID
                             MOVE "INV"     TO   MEIMST-INV-FLG
                             MOVE SPACE     TO   SMEI1 SMEI2
                                                 WRK-KANA
                        NOT INVALID
                             MOVE MEI-F021  TO   SMEI1
                             MOVE MEI-F022  TO   SMEI2
                             MOVE MEI-F031  TO   WRK-KANA
                   END-READ
         END-READ
     ELSE
         MOVE      SPACE     TO   SMEI1     SMEI2
         IF   ERR-FLG   =    ZERO
              MOVE      9    TO   ERR-FLG
         END-IF
         MOVE     "C"   TO   EDIT-CURSOR OF SHOCD
         MOVE     "R"   TO   EDIT-OPTION OF SHOCD
     END-IF.
*****20000502 ﾂｲｶ **************************
*    納品日チェック
     MOVE     ZERO      TO        WK-NOUHIN.
     IF     ( NOUHIN    IS NUMERIC     ) AND
            ( NOUHIN    NOT =     ZERO )
              MOVE     "3"        TO        LINK-IN-KBN
              MOVE      NOUHIN    TO        LINK-IN-YMD6
              CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              IF   LINK-OUT-RET   NOT =     ZERO
                   IF   ERR-FLG   =    ZERO
                        MOVE      17   TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF NOUHIN
                   MOVE     "R"   TO   EDIT-OPTION OF NOUHIN
              ELSE
                   MOVE      LINK-OUT-YMD8  TO   WK-NOUHIN
              END-IF
     END-IF.
*****20000502 ﾂｲｶ **************************
*    売上伝票データ存在チェック
     IF  ERR-FLG        =    ZERO
         PERFORM   220-SONZAI-CHECK
         IF   ERR-FLG   =    ZERO
              MOVE      ZERO      TO   IX
*             対象データ退避処理
              PERFORM   220-TAIHI-SEC
***
              IF   CNT-MEISAI     =    ZERO
                   MOVE      18   TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF NOUHIN
                   MOVE     "R"   TO   EDIT-OPTION OF NOUHIN
                   GO   TO   220-GRP01-CHECK-EXIT
              END-IF
***
              IF        CNT-MEISAI     <=   11
                   MOVE      1         TO   CNT-MAXPAGE
              ELSE
                   DIVIDE    CNT-MEISAI     BY   11
                             GIVING    CNT-MAXPAGE
                             REMAINDER      AMARI
                   IF   AMARI     NOT =     ZERO
                        ADD  1    TO   CNT-MAXPAGE
                   END-IF
              END-IF
         END-IF
     END-IF.
*
 220-GRP01-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶﾞﾒﾝ ﾒｲｻｲ ｾｯﾄ                               *
*--------------------------------------------------------------*
 GAMEN-SET-SEC          SECTION.
     MOVE    "GAMEN-SET-SEC"      TO   S-NAME.
*
     MOVE     WRK-HSUKEI          TO   HACSUU
                                       HSUKEI.
     MOVE     WRK-JTUNOU          TO   JTUNOU
     MOVE     WRK-TSUKEI          TO   TSUKEI.
*****COMPUTE  SAI  =    WRK-HSUKEI     -    WRK-JTUNOU.
*
     COMPUTE  IY   =    CNT-PAGE  *    11   -    11.
     PERFORM  VARYING   IZ   FROM  1   BY   1    UNTIL IZ > 11
         COMPUTE   IX   =    IY   +    IZ
         IF ( WRK-TENPO(IX)  NOT =     ZERO ) AND
            ( WRK-DENNO(IX)  NOT =     ZERO )
              MOVE      SPACE          TO   MAS001(IZ)
              MOVE      WRK-TENPO(IX)  TO   TENPO(IZ)
              MOVE      TORICD         TO   TEN-F52
              MOVE      WRK-TENPO(IX)  TO   TEN-F011
              READ      TENMS1
                   INVALID   KEY
                        MOVE      SPACE     TO   TENPNM(IZ)
                   NOT INVALID
                        MOVE      TEN-F02   TO   TENPNM(IZ)
              END-READ
              MOVE WRK-HSUU (IX)       TO   HSUU (IZ)
              MOVE WRK-TSUU (IX)       TO   TEISUU(IZ)
              MOVE WRK-DENNO(IX)       TO   DENNO (IZ)
              MOVE WRK-NOHIN(IX)       TO   NOHIN (IZ)
              MOVE WRK-KEPPN(IX)       TO   KEPPN (IZ)
         ELSE
              MOVE      SPACE          TO   MAS001(IZ)
              MOVE      "X"  TO   EDIT-STATUS OF TEISUU(IZ)
              MOVE      "X"  TO   EDIT-STATUS OF KEPPN (IZ)
         END-IF
     END-PERFORM.
*
 GAMEN-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｳﾘｱｹﾞﾃﾞﾝﾋﾟｮｳ ｿﾝｻﾞｲ ﾁｪｯｸ                     *
*--------------------------------------------------------------*
 220-SONZAI-CHECK       SECTION.
     MOVE    "220-SONZAI-CHECK"  TO   S-NAME.
*
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     JDATE          TO   DEN-F46.
     MOVE     JTIME          TO   DEN-F47.
     MOVE     TORICD         TO   DEN-F01.
     MOVE     SOKO           TO   DEN-F48.
     MOVE     SHOCD          TO   DEN-F25.
     START    SHTDENLB  KEY  >=   DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F25   DEN-F07
                                  DEN-F02
         INVALID   KEY
              MOVE      5    TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JTIME
              MOVE     "R"        TO   EDIT-OPTION OF TORICD
              MOVE     "R"        TO   EDIT-OPTION OF SOKO
              MOVE     "R"        TO   EDIT-OPTION OF SHOCD
              GO   TO   220-SONZAI-CHECK-EXIT
         NOT INVALID
              READ      SHTDENLB
                AT END
                   MOVE      5    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JTIME
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
                   MOVE     "R"   TO   EDIT-OPTION OF SOKO
                   MOVE     "R"   TO   EDIT-OPTION OF SHOCD
                   GO   TO   220-SONZAI-CHECK-EXIT
                NOT AT END
                   IF ( JDATE     =    DEN-F46 ) AND
                      ( JTIME     =    DEN-F47 ) AND
                      ( TORICD    =    DEN-F01 ) AND
                      ( SOKO      =    DEN-F48 ) AND
                      ( SHOCD     =    DEN-F25 )
                        CONTINUE
                   ELSE
                        MOVE      5    TO   ERR-FLG
                        MOVE "C"  TO   EDIT-CURSOR OF JDATE
                        MOVE "R"  TO   EDIT-OPTION OF JDATE
                        MOVE "R"  TO   EDIT-OPTION OF JTIME
                        MOVE "R"  TO   EDIT-OPTION OF TORICD
                        MOVE "R"  TO   EDIT-OPTION OF SOKO
                        MOVE "R"  TO   EDIT-OPTION OF SHOCD
                        GO   TO   220-SONZAI-CHECK-EXIT
                   END-IF
              END-READ
     END-START.
 220-SONZAI-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｰﾀﾀｲﾋ ｼﾖﾘ                                 *
*--------------------------------------------------------------*
 220-TAIHI-SEC          SECTION.
     MOVE     "220-TAIHI-SEC"     TO   S-NAME.
*
     IF     ( JDATE     =    DEN-F46 ) AND
            ( JTIME     =    DEN-F47 ) AND
            ( TORICD    =    DEN-F01 ) AND
            ( SOKO      =    DEN-F48 ) AND
            ( SHOCD     =    DEN-F25 )
              IF   WK-NOUHIN      NOT =     ZERO
                   IF   WK-NOUHIN      =    DEN-F112
                        CONTINUE
                   ELSE
                        GO   TO   220-TAIHI-010
                   END-IF
              END-IF
              ADD       1         TO   IX
              MOVE      DEN-F07   TO   WRK-TENPO  (IX)
              MOVE      DEN-F50   TO   WRK-HSUU   (IX)
              MOVE      DEN-F15   TO   WRK-TSUU   (IX)
                                       WRK-MAESUU (IX)
              MOVE      DEN-F02   TO   WRK-DENNO  (IX)
              MOVE      DEN-F03   TO   WRK-GYONO  (IX)
              MOVE      DEN-F112  TO   WRK-NOHIN  (IX)
**************MOVE      DEN-F31   TO   WRK-KEPPN  (IX)
              MOVE      DEN-F411  TO   WRK-KEPPN  (IX)
                                       WRK-MAEKEP (IX)
              ADD       DEN-F50   TO   WRK-HSUKEI
              ADD       DEN-F15   TO   WRK-TSUKEI
              GO        TO        220-TAIHI-010
     ELSE
              MOVE      IX        TO   CNT-MEISAI
              GO        TO        220-TAIHI-EXIT
     END-IF.
*
 220-TAIHI-010.
*
     READ     SHTDENLB  NEXT
         AT   END
              MOVE      IX        TO   CNT-MEISAI
         NOT AT END
              GO   TO   220-TAIHI-SEC
     END-READ.
*
 220-TAIHI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｼｭﾂﾘｮｸｼﾞｭﾝ ﾆｭｳﾘｮｸ                           *
*--------------------------------------------------------------*
 220-INP-GRP02          SECTION.
     MOVE     "220-INP-GRP02"     TO   S-NAME.
     MOVE     "GRP002"       TO   WK-GRP.
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
              PERFORM   CLR-BODY1-RTN
              MOVE      1         TO   GR-NO
              MOVE      SPACE     TO   SURYO
              MOVE      SPACE     TO   GOKEI
              PERFORM   VARYING   IZ   FROM  1  BY  1
                                       UNTIL    IZ  >  11
                   MOVE      SPACE     TO   MAS001(IZ)
              END-PERFORM
         WHEN ENT
              PERFORM   CLR-BODY1-RTN
              MOVE      JTUNOU    TO   WRK-JTUNOU
              PERFORM   220-GRP02-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      3    TO   GR-NO
              END-IF
              COMPUTE   SAI = HACSUU - JTUNOU
         WHEN OTHER
              MOVE           1    TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾒｲｻｲ    ﾁｪｯｸ                                *
*--------------------------------------------------------------*
 220-GRP02-CHECK-SEC    SECTION.
     MOVE     "220-CRP02-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
     IF       HACSUU    <    WRK-JTUNOU
              MOVE      15   TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR OF JTUNOU
              MOVE     "R"   TO   EDIT-OPTION OF JTUNOU
     END-IF.
****10/01/20  欠品区分追加開始　大野
*欠品区分の入力チェック
     IF  KEPKBN    NOT =   SPACE
         INITIALIZE               KME-REC
         MOVE      TORICD    TO   KME-F01
         MOVE      KEPKBN    TO   KME-F02
         PERFORM MSTKMEF-READ-SEC
         IF  MSTKMEF-INV-FLG = "INV"
             IF   ERR-FLG   =    ZERO
                  MOVE      22   TO   ERR-FLG
             END-IF
             MOVE     "C"   TO   EDIT-CURSOR OF KEPKBN
             MOVE     "R"   TO   EDIT-OPTION OF KEPKBN
         END-IF
     ELSE
         IF  HACSUU  NOT =  WRK-JTUNOU
             IF   ERR-FLG   =    ZERO
                  MOVE      23   TO   ERR-FLG
             END-IF
             MOVE     "C"   TO   EDIT-CURSOR OF KEPKBN
             MOVE     "R"   TO   EDIT-OPTION OF KEPKBN
         ELSE
             MOVE     SPACE TO   KEPKBN
         END-IF
     END-IF.
****10/05/27  欠品区分
 220-GRP02-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｼｭﾂﾘｮｸｼﾞｭﾝ ﾆｭｳﾘｮｸ2                          *
*--------------------------------------------------------------*
 220-INP-GRP03          SECTION.
     MOVE     "220-INP-GRP03"     TO   S-NAME.
     MOVE      ZERO      TO   WRK-TSUKEI
     PERFORM   VARYING   IX   FROM  1  BY  1
                         UNTIL     IX   >  CNT-MEISAI
***    実納品数ゼロ時，訂正後発注数をオールゼロ表示
         IF    JTUNOU   =    ZERO  AND
               ERR-FLG  =    ZERO
               MOVE      ZERO      TO   WRK-TSUU(IX)
               IF  WRK-KEPPN(IX) = SPACE
                   MOVE      KEPKBN    TO   WRK-KEPPN(IX)
               END-IF
         END-IF
*
         ADD       WRK-TSUU(IX)  TO    WRK-TSUKEI
     END-PERFORM.
*
     PERFORM   GAMEN-SET-SEC.
     MOVE     "GRP003"       TO   WK-GRP.
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
***         項目戻り時、現在の訂正後発注数をワークへ書き出し
*## 2013/11/27 NAV ST 以下、コメントアウト
*##           COMPUTE  IY    =    CNT-MEISAI * 11 - 11
*##           PERFORM   VARYING   IZ  FROM  1  BY  1
*##                               UNTIL     IZ > 11
*##                COMPUTE  IX    =  IY  +  IZ
*##                PERFORM  VARYING  IX FROM 1 BY 1
*##                               UNTIL     IX > 11
*##                     MOVE  TEISUU(IX)    TO   WRK-TSUU(IX)
*##                     MOVE  KEPPN (IX)    TO   WRK-KEPPN(IX)
*##                END-PERFORM
*##           END-PERFORM
*## 2013/11/27 NAV ED 以上、コメントアウト
*
*## 2013/11/27 NAV ST 以下追加　画面退避ＳＥＣ追加
              PERFORM   GAMEN-TAIHI-SEC
*## 2013/11/27 NAV ED 以下追加　画面退避ＳＥＣ追加
              PERFORM   CLR-BODY1-RTN
              MOVE      2         TO   GR-NO
         WHEN PF11
              PERFORM 900-DSP-WRITE2
              PERFORM   MEISAI-CHK-SEC
              IF   ERR-FLG  =  ZERO
                   PERFORM   CLR-BODY1-RTN
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
                   PERFORM   CLR-BODY1-RTN
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
**************PERFORM   CLR-BODY1-RTN
              PERFORM   GAMEN-TAIHI-SEC
              PERFORM   220-GRP03-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
                        PERFORM   CLR-BODY1-RTN
              END-IF
         WHEN OTHER
              MOVE           1    TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP03-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾒｲｻｲ    ﾁｪｯｸ2                               *
*--------------------------------------------------------------*
 220-GRP03-CHECK-SEC    SECTION.
     MOVE     "220-CRP03-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
*****IF       WRK-JTUNOU     NOT =     WRK-TSUKEI
     IF       WRK-JTUNOU     NOT =     WRK-TSUKEI
              IF   ERR-FLG   =    ZERO
                   MOVE      12   TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF TEISUU
              MOVE     "R"   TO   EDIT-OPTION OF TEISUU
     END-IF.
*    訂正後発注数は発注数を超えてはダメ
     PERFORM  VARYING   IZ   FROM  1  BY  1  UNTIL  IZ  >  11
      IF TEISUU(IZ)     IS NUMERIC
         IF HSUU(IZ)       <    TEISUU(IZ)
            IF   ERR-FLG   =    ZERO
                 MOVE      14   TO   ERR-FLG
            END-IF
            MOVE     "C"   TO   EDIT-CURSOR OF TEISUU(IZ)
            MOVE     "R"   TO   EDIT-OPTION OF TEISUU(IZ)
         ELSE
*******************訂正された場合、欠品区分が未指定はエラー
            IF HSUU(IZ)  NOT =  TEISUU(IZ)
               IF  KEPPN(IZ) = SPACE
                   IF KEPKBN NOT = SPACE
                      MOVE KEPKBN TO  KEPPN(IZ)
                   ELSE
                      IF   ERR-FLG   =    ZERO
                           MOVE      20   TO   ERR-FLG
                      END-IF
                      MOVE  "C"  TO  EDIT-CURSOR OF KEPPN(IZ)
                      MOVE  "R"  TO  EDIT-OPTION OF KEPPN(IZ)
                   END-IF
               ELSE
                   MOVE   TORICD    TO  KME-F01
                   MOVE   KEPPN(IZ) TO  KME-F02
                   PERFORM MSTKMEF-READ-SEC
                   IF MSTKMEF-INV-FLG = "INV"
                      IF   ERR-FLG   =    ZERO
                           MOVE      20   TO   ERR-FLG
                      END-IF
                      MOVE  "C"  TO  EDIT-CURSOR OF KEPPN(IZ)
                      MOVE  "R"  TO  EDIT-OPTION OF KEPPN(IZ)
                   END-IF
               END-IF
            ELSE
                   MOVE SPACE TO  KEPPN(IZ)
            END-IF
         END-IF
      END-IF
     END-PERFORM.
*
 220-GRP03-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶﾞﾒﾝ ﾀｲﾋ ｼｮﾘ                                *
*--------------------------------------------------------------*
 GAMEN-TAIHI-SEC        SECTION.
     MOVE     "GAMEN-TAIHI-SEC"   TO   S-NAME.
*
     PERFORM 900-DSP-WRITE2.
     MOVE     JTUNOU    TO   WRK-JTUNOU.
     COMPUTE  IY   =    CNT-PAGE  *    11   -    11.
     PERFORM  VARYING   IZ   FROM  1  BY  1  UNTIL  IZ  >  11
         COMPUTE   IX   =    IY   +    IZ
              IF   TEISUU(IZ)     IS NUMERIC
                   MOVE      TEISUU(IZ)     TO   WRK-TSUU(IX)
                   MOVE      KEPPN(IZ)      TO   WRK-KEPPN(IX)
              ELSE
                   MOVE      "X"  TO   EDIT-STATUS OF TEISUU(IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF KEPPN(IZ)
              END-IF
     END-PERFORM.
*
     COMPUTE  SAI  =    WRK-HSUKEI     -    WRK-JTUNOU.
*    合計計算
     MOVE     ZERO           TO   WRK-TSUKEI.
     PERFORM  VARYING   IX   FROM      1    BY   1
                             UNTIL     IX   >    CNT-MEISAI
         ADD  WRK-TSUU(IX)   TO   WRK-TSUKEI
     END-PERFORM.
     MOVE     WRK-TSUKEI     TO   TSUKEI.
     MOVE     WRK-JTUNOU     TO   JTUNOU.
*
 GAMEN-TAIHI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "230-INP-KKNN"      TO   S-NAME.
     MOVE     "KKNN"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
     PERFORM   GAMEN-TAIHI-SEC.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF06
              MOVE      3         TO   GR-NO
         WHEN PF11
                   PERFORM 900-DSP-WRITE2
                   IF   CNT-PAGE  <=   1
                        MOVE      10   TO   ERR-FLG
                   ELSE
                        ADD       -1   TO   CNT-PAGE
                        PERFORM   GAMEN-SET-SEC
                   END-IF
         WHEN PF12
                   PERFORM 900-DSP-WRITE2
                   IF   CNT-PAGE  =    CNT-MAXPAGE
                        MOVE      11   TO   ERR-FLG
                   ELSE
                        ADD       1    TO   CNT-PAGE
                        PERFORM   GAMEN-SET-SEC
                   END-IF
         WHEN ENT
              MOVE      ZERO      TO   ERR-FLG
              PERFORM   CLR-TAIL-RTN
              IF        KKNN      NOT =    "Y"
                        MOVE      13   TO   ERR-FLG
              ELSE
                        MOVE      10   TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾟﾗﾒﾀ ｼｭﾂﾘｮｸ                                *
*--------------------------------------------------------------*
 240-UPDATE-SEC           SECTION.
     MOVE     "240-UPDATE-SEC"    TO   S-NAME.
*訂正後数量／金額／在庫数更新
     PERFORM  VARYING   IX   FROM      1    BY   1
                             UNTIL     IX   >    CNT-MEISAI
         IF   WRK-TSUU(IX)   NOT =     WRK-MAESUU(IX)
              PERFORM   KOUSIN1-SEC
         END-IF
     END-PERFORM.
*訂正区分更新
     PERFORM  VARYING   IX   FROM      1    BY   1
                             UNTIL     IX   >    CNT-MEISAI
         IF   WRK-TSUU(IX)   NOT =     WRK-MAESUU(IX)
              PERFORM   KOUSIN2-SEC
         END-IF
     END-PERFORM.
****10/01/20  欠品区分追加開始　大野
*欠品区分更新
     PERFORM  VARYING   IX   FROM      1    BY   1
                             UNTIL     IX   >    CNT-MEISAI
              PERFORM   KOUSIN3-SEC
     END-PERFORM.
****10/01/20  欠品区分追加終了　大野
*取消を押さない限り、バッチ番号は固定表示
     MOVE    "CHK"                TO   CHK-FLG.
     MOVE     JDATE               TO   WK-B-HIDUKE.
     MOVE     JTIME               TO   WK-B-TIME.
     MOVE     TORICD              TO   WK-B-TORICD.
     MOVE     SOKO                TO   WK-B-SOKCD.
*画面初期化ＳＥＣへ
     MOVE     0                   TO   GR-NO.
*
 240-UPDATE-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL         ｺｳｼﾝ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 KOUSIN1-SEC             SECTION.
     MOVE    "KOUSIN1-SEC"   TO   S-NAME.
*
     MOVE     SPACE          TO   DE2-REC.
     INITIALIZE                   DE2-REC  WRK-ERR(IX).
     MOVE     JDATE          TO   DE2-F46.
     MOVE     JTIME          TO   DE2-F47.
     MOVE     TORICD         TO   DE2-F01.
     MOVE     SOKO           TO   DE2-F48.
     MOVE     WRK-DENNO(IX)  TO   DE2-F02.
     MOVE     ZERO           TO   DE2-F04.
     MOVE     40             TO   DE2-F051.
***2011.10.12 ST
***  START    SHTDENLA  KEY  >=   DE2-F46   DE2-F47
***                               DE2-F01   DE2-F48
***                               DE2-F02   DE2-F04
***                               DE2-F051  DE2-F03
     START    SHTDENLA  KEY  >=   DE2-F46   DE2-F47
                                  DE2-F01   DE2-F48
                                  DE2-F02   DE2-F04
                                  DE2-F051  DE2-F07
                                  DE2-F112  DE2-F03
***2011.10.12 EN
         INVALID   KEY
              DISPLAY  "TSY0303I  SHTDENLA START INV 1"
                                       UPON CONS
              GO   TO   KOUSIN1-SEC-EXIT
     END-START.
*
 KOUSIN1-010.
*
     READ     SHTDENLA
         AT END
              GO   TO   KOUSIN1-SEC-EXIT
         NOT AT END
              IF ( JDATE          =    DE2-F46 ) AND
                 ( JTIME          =    DE2-F47 ) AND
                 ( TORICD         =    DE2-F01 ) AND
                 ( SOKO           =    DE2-F48 ) AND
                 ( WRK-DENNO(IX)  =    DE2-F02 ) AND
                 ( ZERO           =    DE2-F04 ) AND
*## 2000/01/28 NAV START ##*
                 ( 9          NOT =    DE2-F277 )
*## 2000/01/28 NAV END   ##*
*                  訂正フラグ
                   IF   DE2-F03   =    WRK-GYONO(IX)
*                       在庫マスタ更新
                        PERFORM   ZAIKO-SEC
                        MOVE      WRK-TSUU(IX)   TO   DE2-F15
                        COMPUTE   DE2-F181       =
                                  WRK-TSUU(IX)   *    DE2-F172
                        COMPUTE   DE2-F182       =
                                  WRK-TSUU(IX)   *    DE2-F173
                        IF   KEP-FLG   =    SPACE
                             MOVE      1         TO   DE2-F27D
                        ELSE
                             MOVE      ZERO      TO   DE2-F27D
                        END-IF
                   END-IF
*******************2018/03/14 NAV ST
                   PERFORM  JYUZANF-UPD-SEC
*******************2018/03/14 NAV ED
*実発注数量と訂正後発注数量比較
                   IF   DE2-F15  NOT =  DE2-F50
                        MOVE     1               TO   WRK-ERR(IX)
                   END-IF
                   REWRITE  DE2-REC
                   GO   TO  KOUSIN1-010
              END-IF
     END-READ.
*
 KOUSIN1-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL         ｺｳｼﾝ ｼｮﾘ（訂正区分伝票単位更新）            *
*--------------------------------------------------------------*
 KOUSIN2-SEC             SECTION.
     MOVE    "KOUSIN2-SEC"   TO   S-NAME.
*
     MOVE     SPACE          TO   DE2-REC.
     INITIALIZE                   DE2-REC.
     MOVE     JDATE          TO   DE2-F46.
     MOVE     JTIME          TO   DE2-F47.
     MOVE     TORICD         TO   DE2-F01.
     MOVE     SOKO           TO   DE2-F48.
     MOVE     WRK-DENNO(IX)  TO   DE2-F02.
     MOVE     ZERO           TO   DE2-F04.
     MOVE     40             TO   DE2-F051.
***2011.10.12 ST
***  START    SHTDENLA  KEY  >=   DE2-F46   DE2-F47
***                               DE2-F01   DE2-F48
***                               DE2-F02   DE2-F04
***                               DE2-F051  DE2-F03
     START    SHTDENLA  KEY  >=   DE2-F46   DE2-F47
                                  DE2-F01   DE2-F48
                                  DE2-F02   DE2-F04
                                  DE2-F051  DE2-F07
                                  DE2-F112  DE2-F03
***2011.10.12 EN
         INVALID   KEY
              DISPLAY  "TSY0303I  SHTDENLA START INV 2"
                                       UPON CONS
              GO   TO   KOUSIN2-SEC-EXIT
     END-START.
*
 KOUSIN2-010.
*
     READ     SHTDENLA
         AT END
              GO   TO   KOUSIN2-SEC-EXIT
         NOT AT END
              IF ( JDATE          =    DE2-F46 ) AND
                 ( JTIME          =    DE2-F47 ) AND
                 ( TORICD         =    DE2-F01 ) AND
                 ( SOKO           =    DE2-F48 ) AND
                 ( WRK-DENNO(IX)  =    DE2-F02 ) AND
                 ( ZERO           =    DE2-F04 ) AND
*## 2000/01/28 NAV START ##*
                 ( 9          NOT =    DE2-F277 )
*## 2000/01/28 NAV END   ##*
*                  訂正フラグ
                   IF   WRK-ERR(IX)  =  ZERO
                        MOVE      ZERO  TO     DE2-F53
                        MOVE      99    TO     DE2-F06
                   ELSE
                        MOVE      1     TO     DE2-F53
                        MOVE      99    TO     DE2-F06
                   END-IF
*## 2013/11/27 NAV ST ＬＩＮＫＳ連携対応追加
*物流連携フラグ、物流連携日をクリアする。
                   MOVE   SPACE   TO  DE2-F68  DE2-F69
*## 2013/11/27 NAV ED ＬＩＮＫＳ連携対応追加
                   REWRITE  DE2-REC
                   GO   TO  KOUSIN2-010
              END-IF
     END-READ.
*
 KOUSIN2-SEC-EXIT.
     EXIT.
*
****10/01/20  欠品区分追加開始　大野
*--------------------------------------------------------------*
*    LEVEL         ｺｳｼﾝ ｼｮﾘ（欠品制御区分更新）　　            *
*--------------------------------------------------------------*
 KOUSIN3-SEC             SECTION.
     MOVE    "KOUSIN3-SEC"   TO   S-NAME.
*
     MOVE     SPACE          TO   DE2-REC.
     INITIALIZE                   DE2-REC.
     MOVE     JDATE          TO   DE2-F46.
     MOVE     JTIME          TO   DE2-F47.
     MOVE     TORICD         TO   DE2-F01.
     MOVE     SOKO           TO   DE2-F48.
     MOVE     WRK-DENNO(IX)  TO   DE2-F02.
     MOVE     ZERO           TO   DE2-F04.
     MOVE     40             TO   DE2-F051.
***2011.10.12 ST
***  START    SHTDENLA  KEY  >=   DE2-F46   DE2-F47
***                               DE2-F01   DE2-F48
***                               DE2-F02   DE2-F04
***                               DE2-F051  DE2-F03
     START    SHTDENLA  KEY  >=   DE2-F46   DE2-F47
                                  DE2-F01   DE2-F48
                                  DE2-F02   DE2-F04
                                  DE2-F051  DE2-F07
                                  DE2-F112  DE2-F03
***2011.10.12 EN
         INVALID   KEY
              DISPLAY  "TSY0303I  SHTDENLA START INV 3"
                                       UPON CONS
              GO   TO   KOUSIN3-SEC-EXIT
     END-START.
*
 KOUSIN3-010.
*
     READ     SHTDENLA
         AT END
              GO   TO   KOUSIN3-SEC-EXIT
         NOT AT END
*            入力された検索条件と同じものか判定
             IF ( JDATE          =    DE2-F46 ) AND
                ( JTIME          =    DE2-F47 ) AND
                ( TORICD         =    DE2-F01 ) AND
                ( SOKO           =    DE2-F48 ) AND
                ( WRK-DENNO(IX)  =    DE2-F02 ) AND
                ( ZERO           =    DE2-F04 ) AND
                ( 9          NOT =    DE2-F277 )
*               欠品区分をセット
                IF   DE2-F03   =    WRK-GYONO(IX)
                   IF WRK-TSUU(IX) NOT =  WRK-HSUU(IX)
**********************MOVE WRK-KEPPN(IX) TO DE2-F31
                      MOVE WRK-KEPPN(IX) TO DE2-F411
                   ELSE
**********************MOVE SPACE         TO DE2-F31
                      MOVE SPACE         TO DE2-F411
                   END-IF
                   REWRITE  DE2-REC
                   GO   TO  KOUSIN3-010
                ELSE
                   GO   TO  KOUSIN3-010
                END-IF
             END-IF
     END-READ.
*
 KOUSIN3-SEC-EXIT.
     EXIT.
*
****10/01/20  欠品区分追加終了　大野
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-SEC              SECTION.
*
     MOVE   "ZAIKO-SEC"      TO   S-NAME.
     MOVE    SPACE           TO   KEP-FLG.
*商品在庫マスタ存在チェック
     MOVE    DE2-F08         TO   ZAI-F01.
     MOVE    DE2-F1411       TO   ZAI-F021.
     MOVE    DE2-F1412       TO   ZAI-F022.
     MOVE    DE2-F49         TO   ZAI-F03.
     READ    ZAMZAIF
             INVALID
             PERFORM   ZAIKO-UPDATE1-SEC
             NOT  INVALID
             PERFORM   ZAIKO-UPDATE2-SEC
     END-READ.
*
 ZAIKO-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE1-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE1-SEC" TO   S-NAME.
*商品在庫Ｍが未存在の為、在庫マスタ作成
     MOVE      "1"           TO   KEP-FLG.
     IF        MEIMST-INV-FLG      =   "INV"
               GO  TO   ZAIKO-UPDATE1-EXIT
     END-IF.
*商品在庫マスタ初期化
      MOVE      SPACE         TO   ZAI-REC.
      INITIALIZE                   ZAI-REC.
*商品在庫マスタ項目セット
      MOVE      DE2-F08       TO   ZAI-F01.
      MOVE      DE2-F1411     TO   ZAI-F021.
      MOVE      DE2-F1412     TO   ZAI-F022.
      MOVE      DE2-F49       TO   ZAI-F03.
*未出庫数＝未出庫数＋数量
      COMPUTE   ZAI-F27       =    ZAI-F27  +  WRK-TSUU(IX).
*取引先
      MOVE      TORICD        TO   ZAI-F29.
*カナ名称
      MOVE      WRK-KANA      TO   ZAI-F30.
      MOVE      SYS-DATEW     TO   ZAI-F98.
      MOVE      SYS-DATEW     TO   ZAI-F99.
      WRITE     ZAI-REC.
*
 ZAIKO-UPDATE1-EXIT.
      EXIT.
****************************************************************
*              在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE2-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE2-SEC" TO   S-NAME.
     INITIALIZE    WRK-AREA2.
*
     IF  DE2-F27D      =     1
*        引当済数に数量減算
         COMPUTE  ZAI-F28   =   ZAI-F28  -  WRK-MAESUU(IX)
*        未出庫数に数量減算
         COMPUTE  ZAI-F27   =   ZAI-F27  -  WRK-MAESUU(IX)
     ELSE
*        未出庫数に数量減算
         COMPUTE  ZAI-F27   =   ZAI-F27  -  WRK-MAESUU(IX)
     END-IF.
*
*引当後在庫数チェック
*    現在庫数－引当済数＝引当可能在庫数
     COMPUTE   WRK-ZAI   =   ZAI-F04  -  ZAI-F28.
*    引当可能在庫数－発注数量＝引当後在庫数
     COMPUTE   WRK-HIK   =   WRK-ZAI  -  WRK-TSUU(IX).
     IF  WRK-HIK  <  0
         MOVE      "1"      TO   KEP-FLG
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =   ZAI-F27  +  WRK-TSUU(IX)
         MOVE     SYS-DATEW       TO   ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     ELSE
*        引当済数に数量加算
         COMPUTE  ZAI-F28   =   ZAI-F28  +  WRK-TSUU(IX)
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =   ZAI-F27  +  WRK-TSUU(IX)
         MOVE     SYS-DATEW       TO   ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     END-IF.
*
 ZAIKO-UPDATE2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-WRITE2         SECTION.
*
     MOVE     "SCREEN"       TO   DSP-GRP.
     MOVE     WK-SYSYMD           TO   SYSYMD.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW           TO   SYSTIM.
*
     PERFORM  900-DSP-WRITE.
*
 900-DSP-WRITE2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "900-DSP-READ"      TO   S-NAME.
*欠品名称セット
     IF   TORICD  NOT =  ZERO
     AND  TORICD  NUMERIC
          MOVE   TORICD      TO   KEP-F01
          PERFORM MSTKEPF-READ-SEC
          IF  MSTKEPF-INV-FLG = "INV"
              MOVE  SPACE    TO   KEPMSG
          ELSE
              MOVE  KEP-F02  TO   KEPMSG
          END-IF
     ELSE
          MOVE   SPACE       TO   KEPMSG
     END-IF.
*
     MOVE     "SCREEN"       TO   DSP-GRP.
     IF       ERR-FLG   =    0
              MOVE      SPACE               TO   MSG
              MOVE      "D"      TO   EDIT-OPTION OF MSG
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   MSG
     END-IF.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   GUIDE
     ELSE
         IF   GR-NO     =    2
              MOVE      GUIDE02   TO   GUIDE
         ELSE
           IF   CNT-MEISAI     <=   11
                MOVE      GUIDE02   TO   GUIDE
           ELSE
                IF   CNT-PAGE  <=   1
                     MOVE      GUIDE03   TO   GUIDE
                ELSE
                     IF   CNT-PAGE  =    CNT-MAXPAGE
                          MOVE      GUIDE04   TO   GUIDE
                     ELSE
                          MOVE      GUIDE05   TO   GUIDE
                     END-IF
                END-IF
           END-IF
         END-IF
     END-IF.
     MOVE     WK-SYSYMD           TO   SYSYMD.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW           TO   SYSTIM.
*
     PERFORM  900-DSP-WRITE.
*
     IF       MSG  NOT  =    SPACE
              MOVE "AL"      TO   DSP-PRO
     ELSE
              MOVE "NE"      TO   DSP-PRO
     END-IF.
     MOVE     SPACE          TO   MSG.
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
     MOVE     "900-DSP-WRITE"     TO   S-NAME.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    MSY03031.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF JDATE
                                  EDIT-CURSOR OF JTIME
                                  EDIT-CURSOR OF TORICD
                                  EDIT-CURSOR OF SOKO
                                  EDIT-CURSOR OF SHOCD
                                  EDIT-CURSOR OF NOUHIN.
     MOVE     "M"            TO   EDIT-OPTION OF JDATE
                                  EDIT-OPTION OF JTIME
                                  EDIT-OPTION OF TORICD
                                  EDIT-OPTION OF SOKO
                                  EDIT-OPTION OF SHOCD
                                  EDIT-OPTION OF NOUHIN.
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ１属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY1-RTN          SECTION.
*
     MOVE     " "            TO   EDIT-CURSOR OF JTUNOU.
     MOVE     "M"            TO   EDIT-OPTION OF JTUNOU.
****10/01/20  欠品区分追加開始　大野
     MOVE     " "            TO   EDIT-CURSOR OF KEPKBN.
     MOVE     "M"            TO   EDIT-OPTION OF KEPKBN.
****10/01/20  欠品区分追加終了　大野
     PERFORM  VARYING   IX   FROM  1   BY   1    UNTIL  IX > 11
         MOVE     " "        TO   EDIT-CURSOR OF TEISUU(IX)
         MOVE     "M"        TO   EDIT-OPTION OF TEISUU(IX)
         MOVE     " "        TO   EDIT-STATUS OF TEISUU(IX)
         MOVE     "M"        TO   EDIT-OPTION OF KEPPN(IX)
         MOVE     " "        TO   EDIT-STATUS OF KEPPN(IX)
     END-PERFORM.
*
 CLR-BODY1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF KKNN.
     MOVE     "M"        TO   EDIT-OPTION OF KKNN.
 CLR-TAIL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細情報　発注数　訂正数チェック                          *
*--------------------------------------------------------------*
 MEISAI-CHK-SEC         SECTION.
     MOVE     "MEISAI-CHK-SEC"         TO   S-NAME.
     MOVE      ZERO                    TO   ERR-FLG.
*    訂正後発注数は発注数を超えてはダメ
     PERFORM  VARYING   IZ   FROM  1  BY  1  UNTIL  IZ  >  11
*********IF   TEISUU(IZ)     IS NUMERIC
*             IF   HSUU(IZ)       <    TEISUU(IZ)
*                  IF   ERR-FLG   =    ZERO
*                       MOVE      14   TO   ERR-FLG
*                  END-IF
*                  MOVE     "C"   TO   EDIT-CURSOR OF TEISUU(IZ)
*                  MOVE     "R"   TO   EDIT-OPTION OF TEISUU(IZ)
*             END-IF
*********END-IF
      IF TEISUU(IZ)     IS NUMERIC
         IF HSUU(IZ)       <    TEISUU(IZ)
            IF   ERR-FLG   =    ZERO
                 MOVE      14   TO   ERR-FLG
            END-IF
            MOVE     "C"   TO   EDIT-CURSOR OF TEISUU(IZ)
            MOVE     "R"   TO   EDIT-OPTION OF TEISUU(IZ)
         ELSE
*******************訂正された場合、欠品区分が未指定はエラー
            IF HSUU(IZ)  NOT =  TEISUU(IZ)
               IF  KEPPN(IZ) = SPACE
                   IF KEPKBN NOT = SPACE
                      MOVE KEPKBN TO  KEPPN(IZ)
                   ELSE
                      IF   ERR-FLG   =    ZERO
                           MOVE      20   TO   ERR-FLG
                      END-IF
                      MOVE  "C"  TO  EDIT-CURSOR OF KEPPN(IZ)
                      MOVE  "R"  TO  EDIT-OPTION OF KEPPN(IZ)
                   END-IF
               ELSE
                   MOVE   TORICD    TO  KME-F01
                   MOVE   KEPPN(IZ) TO  KME-F02
                   PERFORM MSTKMEF-READ-SEC
                   IF MSTKMEF-INV-FLG = "INV"
                      IF   ERR-FLG   =    ZERO
                           MOVE      20   TO   ERR-FLG
                      END-IF
                      MOVE  "C"  TO  EDIT-CURSOR OF KEPPN(IZ)
                      MOVE  "R"  TO  EDIT-OPTION OF KEPPN(IZ)
                   END-IF
               END-IF
            ELSE
                   MOVE SPACE TO  KEPPN(IZ)
            END-IF
         END-IF
      END-IF
     END-PERFORM.
*
 MEISAI-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    欠品名称マスタ　読込
*--------------------------------------------------------------*
 MSTKEPF-READ-SEC       SECTION.
*
     READ  MSTKEPF
           INVALID
           MOVE  "INV"  TO   MSTKEPF-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   MSTKEPF-INV-FLG
     END-READ.
*
 MSTKEOF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    欠品明細マスタ　読込
*--------------------------------------------------------------*
 MSTKMEF-READ-SEC       SECTION.
*
     READ  MSTKMEF
           INVALID
           MOVE  "INV"  TO   MSTKMEF-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   MSTKMEF-INV-FLG
     END-READ.
*
 MSTKEOF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    受注残ファイル更新処理
*--------------------------------------------------------------*
 JYUZANF-UPD-SEC        SECTION.
*
     MOVE     "JYUZANF-UPD-SEC"        TO   S-NAME.
*
     MOVE      "2"                     TO   PARA-KOSINKBN.
     MOVE      "1"                     TO   PARA-DTKBN.
     MOVE      DE2-F01                 TO   PARA-TOKCD.
     MOVE      DE2-F02                 TO   PARA-DENNO.
     MOVE      DE2-F03                 TO   PARA-GYO.
     MOVE      DE2-F04                 TO   PARA-SOUSAI.
     MOVE      DE2-F051                TO   PARA-DENKU.
     MOVE      DE2-F07                 TO   PARA-TENCD.
     MOVE      DE2-F08                 TO   PARA-SOKCD.
     MOVE      DE2-F111                TO   PARA-HATYU.
     MOVE      DE2-F112                TO   PARA-NOUHIN.
     MOVE      DE2-F113                TO   PARA-SYUKA.
     MOVE      DE2-F1411               TO   PARA-SAKATACD.
     MOVE      DE2-F1412(1:5)          TO   PARA-HINTAN1.
     MOVE      DE2-F1412(6:2)          TO   PARA-HINTAN2.
     MOVE      DE2-F1412(8:1)          TO   PARA-HINTAN3.
     MOVE      DE2-F49                 TO   PARA-TANABAN.
     MOVE      DE2-F50                 TO   PARA-JYUTYUSU.
     MOVE      DE2-F15                 TO   PARA-SYUKASU.
     MOVE      DE2-F172                TO   PARA-GENKA.
     MOVE      DE2-F173                TO   PARA-BAIKA.
     MOVE      DE2-F181                TO   PARA-GENKAKIN.
     MOVE      DE2-F182                TO   PARA-BAIKAKIN.
     MOVE      DE2-F25                 TO   PARA-AITECD.
     MOVE      DE2-F27D                TO   PARA-HIKIATE.
     MOVE      "2920"                  TO   PARA-UPBUMON
                                            PARA-BUMON.
     MOVE      "99"                    TO   PARA-UPTANCD
                                            PARA-TANCD.
     MOVE      DE2-F46                 TO   PARA-JDATE.
     MOVE      DE2-F47                 TO   PARA-JTIME.
*サブルーチンコール
     CALL    "SJZ0110B" USING PARA-AREA PARA-BUMON PARA-TANCD.
*
 JYUZANF-UPD-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
