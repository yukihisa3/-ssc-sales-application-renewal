***********************************************************
*
*    顧客名　　　　　：（株）サカタのタネ殿
*    サブシステム　　：ＨＧ基幹システム苗業務連携
*    業務名　　　　　：
*    モジュール名　　：小売発注連携リスト出力
*    作成日／更新日　：2011/11/11
*    作成者／更新者　：飯田/NAV
*    処理概要　　　　：
*      苗業務システムへ連携したデータを、商品・店合計数
*      にて把握すためのリスト。
*    作成日／更新日　：2012/07/26
*    作成者／更新者　：高橋/NAV
*    処理概要　　　　：小売出荷対象商品が分かる様に変更
*                    ：商品名をマスタ出力をやめて、データ
*                    　内の商品名表示に変更
*    作成日／更新日　：2012/03/08
*    作成者／更新者　：高橋/NAV
*    処理概要　　　　：出力をレーザープリンタに変更する。
*                    ：改頁判定を、５９→４９に変更する。
*
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0130L.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2011/11/01.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE    IS  CONS
     YA         IS  PIT-2
     YA-21      IS  PIT-2-2W
     YA-22      IS  PIT-2-2W2H
     YB         IS  PIT-1V5
     YB-21      IS  PIT-1V5-2W
     YB-22      IS  PIT-1V5-2W2H.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.

* 小売連携リストワーク SF
     SELECT  NARLSTF
       ASSIGN      TO  NARLSTF
       ORGANIZATION    SEQUENTIAL
       FILE      STATUS    LSW-ST.

* 連携Ｎｏ管理テーブル
     SELECT  NARKANF
       ASSIGN    TO    NARKANL1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         KAN-F01 *> 連携Ｎｏ
       FILE STATUS     KAN-ST.

* 取引先マスタ
     SELECT  HTOKMS
       ASSIGN    TO    TOKMS2
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         TOK-F01 *> 相手取引先コード
       FILE STATUS     TOK-ST.

* 商品名称マスタ
     SELECT  HMEIMS
       ASSIGN    TO    MEIMS1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         MEI-F011  *> 商品コード
         MEI-F0121 *> 単１
         MEI-F0122 *> 単２
         MEI-F0123 *> 単３
       FILE STATUS     MEI-ST.

* 倉庫マスタ
     SELECT  ZSOKMS
       ASSIGN    TO        ZSOKMS1
       ORGANIZATION        INDEXED
       ACCESS    MODE      RANDOM
       RECORD    KEY
         SOK-F01 *> 倉庫コード
       FILE      STATUS    SOK-ST.

* プリントファイル
     SELECT  PRTFILE
****** ASSIGN    TO  SP-31
       ASSIGN    TO  LP-04
       FILE  STATUS  PRT-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 小売連携リストワーク SF                            *
****************************************************************
 FD  NARLSTF
     LABEL     RECORD    IS   STANDARD.
     COPY      NARLSTF   OF   XFDLIB
     JOINING   LSW       AS   PREFIX.
****************************************************************
*    FILE = 連携Ｎｏ管理テーブル                               *
****************************************************************
 FD  NARKANF
     LABEL     RECORD    IS   STANDARD.
     COPY      NARKANF   OF   XFDLIB
     JOINING   KAN       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
     LABEL     RECORD    IS   STANDARD.
     COPY      HTOKMS    OF   XFDLIB
     JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 商品名称マスタ                                     *
****************************************************************
 FD  HMEIMS
     LABEL     RECORD    IS   STANDARD.
     COPY      HMEIMS   OF   XFDLIB
     JOINING   MEI       AS   PREFIX.
****************************************************************
*    FILE = 倉庫マスタ                                         *
****************************************************************
 FD  ZSOKMS
     LABEL     RECORD    IS   STANDARD.
     COPY      ZSOKMS    OF   XFDLIB
     JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = プリントファイル                                   *
****************************************************************
 FD  PRTFILE
     LABEL RECORD OMITTED.

 01  PRT-REC.
     03  FILLER             PIC  X(300).

****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  LSW-ST             PIC  X(02).
     03  KAN-ST             PIC  X(02).
     03  TOK-ST             PIC  X(02).
     03  PRT-ST             PIC  X(02).
     03  MEI-ST             PIC  X(02).
     03  SOK-ST             PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG            PIC  X(03)  VALUE  SPACE.
     03  FG-NARLSTF-END     PIC  X(03).
     03  FG-NARKANF-INV     PIC  9(01).
     03  FG-HTOKMS-INV      PIC  9(01).
     03  FG-ZSOKMS-INV      PIC  9(01).
     03  FG-HMEIMS-INV      PIC  9(01).
     03  FG-HD-PRT          PIC  9(01).
     03  FG-HDPRT-CHK       PIC  9(01).
*ワーク領域
 01  WRK-AREA.
     03  S-NAME-SV          PIC  X(30).
     03  CT-PG              PIC  9(08).
     03  CT-IN              PIC  9(08).
     03  CT-LINE            PIC  9(03).
     03  WK-CT-LINE         PIC  9(03).
     03  WK-JIKKO-LINE      PIC  9(03).
     03  WK-YMD             PIC  9(08).
     03  WK-YMDR  REDEFINES WK-YMD.
       05  WK-YMD-Y         PIC  9(04).
       05  WK-YMD-M         PIC  9(02).
       05  WK-YMD-D         PIC  9(02).

     03  WK-CMPYMD.
       05  WK-CMPYMD-Y      PIC S9(04).
       05  WK-CMPYMD-M      PIC S9(02).
       05  WK-CMPYMD-D      PIC S9(02).

     03  WK-SYO             PIC  9(06).
     03  WK-AMARI           PIC  9(06).
     03  WK-MATUBI          PIC  9(02).

     03  IX                 PIC  9(04).
     03  IX2                PIC  9(04).
     03  IX-GYO             PIC  9(04).
     03  IX-GYOMAX          PIC  9(04).
     03  IX-GYOMAX2         PIC  9(04).

     03  WK-REN-NO-X        PIC  X(09).
     03  WK-REN-NO   REDEFINES WK-REN-NO-X
                            PIC  9(09).
     03  BRK-RENNO          PIC  X(09).
     03  BRK-BMNCD          PIC  X(04).
     03  BRK-ROUTE          PIC  X(10).
     03  BRK-NOHNBI         PIC  9(08).
     03  BRK-SHOCD          PIC  X(08).
     03  BRK-HINTAN         PIC  X(08).
     03  BRK-TENCD          PIC  9(06).
*****2012/07/26↓商品ＣＤブレイクチェック ST
     03  BRK-SYOJAN         PIC  X(16).
     03  LSW-SYOJAN         PIC  X(16).
*****2012/07/26↓商品ＣＤブレイクチェック ED

* 合計集計領域
 01  WK-GOK.
     03  WK-GOK-G2  OCCURS 4.
       05  WK-GOK-SURYO     PIC S9(05).
       05  WK-GOK-GKINGK    PIC S9(09).
       05  WK-GOK-UKINGK    PIC S9(09).
* 行格納領域
 01  WK-GYO.
     03  WK-GYO-SHOCD       PIC  X(08).
     03  WK-GYO-HINTAN      PIC  X(08).
     03  WK-GYO-AIT-SHOCD   PIC  X(13).
*****2012/07/26 ADD    ST
     03  WK-GYO-KOURI-KBN   PIC  X(01).
     03  WK-GYO-SYOHIN1     PIC  X(15).
     03  WK-GYO-SYOHIN2     PIC  X(15).
*****2012/07/26 ADD    ED
     03  WK-GYO-GTANKA      PIC  9(09)V99.
     03  WK-GYO-UTANKA      PIC  9(09)V99.
     03  WK-GYO-SURY-GK     PIC S9(09)V99.
     03  WK-GYO-G2.
       05  WK-GYO-G2  OCCURS 100. *> 行数
         07  WK-GYO-G3  OCCURS 10. *> 1行のMAX.
           09  WK-GYO-TENCD PIC  X(06).
           09  WK-GYO-SURY  PIC S9(09)V99.

     03  WK-GYO-G2R  REDEFINES WK-GYO-G2.
       05  WK-GYO-G2R  OCCURS 1000. *> 店
         07  WK-GYO-TENCD-R PIC  X(06).
         07  WK-GYO-SURY-R  PIC S9(09)V99.

* 行集計領域
 01  WK-GSUM.
     03  WK-GSUM-G2R.
       05  WK-GSUM-TENCD-R  PIC  X(06).
       05  WK-GSUM-SURY-R   PIC S9(09)V99.

     COPY  NARLSTF OF XFDLIB  JOINING WK-GYO  AS PREFIX.

*  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)  VALUE " *** ".
     03  S-NAME             PIC  X(30).

*　システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME            PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE            PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE           PIC  9(08)  VALUE  ZERO.

*　日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY    PIC  9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM      PIC  9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD      PIC  9(02)  VALUE  ZERO.

     03  WK-REN-NO-X        PIC  X(09).
     03  WK-REN-NO   REDEFINES WK-REN-NO-X
                            PIC  9(09).
 01  TB-CVT-SU.
     03  TB-CVT-SU          PIC  X(11)  VALUE
         "0123456789 ".
     03  TB-CVT-SUR  REDEFINES TB-CVT-SU
                            PIC  X(01)  OCCURS 11.

     03  TB-CVT-SU-N        PIC  N(11)  VALUE
       NC"０１２３４５６７８９　".
     03  TB-CVT-SU-NR  REDEFINES TB-CVT-SU-N
                            PIC  N(01)  OCCURS 11.

 01  WK-RENNO-G.
     03  WK-RENNO           PIC  X(09).
     03  WK-RENNOR-9  REDEFINES  WK-RENNO
                            PIC  9(09).
     03  WK-RENNOR    REDEFINES  WK-RENNO
                            PIC  X(01)  OCCURS 9.

     03  WK-RENNO-N         PIC  N(09).
     03  WK-RENNO-NR  REDEFINES  WK-RENNO-N
                            PIC  N(01)  OCCURS 9.

 01  HEAD01  CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(08)  VALUE "SNA0130L".
     03  FILLER  PIC  X(105) VALUE SPACE.
     03  HD01-Y  PIC  9(04).
     03  FILLER  PIC  N(01)  VALUE NC"年".
     03  HD01-M  PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"月".
     03  HD01-D  PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"日".
     03  FILLER  PIC  X(02)  VALUE SPACE.
     03  HD01-PG PIC  ZZ9.
     03  FILLER  PIC  N(01)  VALUE NC"頁".

 01  HEAD02.
     03  FILLER  PIC  X(43)  VALUE SPACE.
     03  FILLER  PIC  N(11)  VALUE
     NC"＜小売発注連携リスト＞"
                      CHARACTER TYPE IS PIT-2-2W2H.
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  N(01)  VALUE NC"（"
                      CHARACTER TYPE IS PIT-2.
     03  HD02-JYUN  PIC  N(05)
                      CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  N(01)  VALUE NC"）"
                      CHARACTER TYPE IS PIT-2.

 01  HEAD03.
     03  FILLER  PIC  N(05)  VALUE NC"取引先　："
                      CHARACTER TYPE IS PIT-2.
     03  HD03-TORCD  PIC  9(08).
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  HD03-TORNM  PIC  N(15)
                      CHARACTER TYPE IS PIT-1V5.
     03  FILLER  PIC  N(01)  VALUE SPACE
                      CHARACTER TYPE IS PIT-1V5.
     03  FILLER  PIC  X(09)  VALUE SPACE.
     03  FILLER  PIC  N(01)  VALUE NC"（"
                      CHARACTER TYPE IS PIT-1V5-2W.
     03  HD03-TEGONL  PIC  N(05)
                      CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER  PIC  N(01)  VALUE NC"）"
                      CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER  PIC  X(32)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"発注日："
                      CHARACTER TYPE IS PIT-2.
     03  HD03-Y  PIC  9(04).
     03  FILLER  PIC  N(01)  VALUE NC"年"
                      CHARACTER TYPE IS PIT-2.
     03  HD03-M  PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"月"
                      CHARACTER TYPE IS PIT-2.
     03  HD03-D  PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"日"
                      CHARACTER TYPE IS PIT-2.

 01  HEAD04.
     03  FILLER  PIC  N(05)  VALUE NC"出荷場所："
                      CHARACTER TYPE IS PIT-2.
     03  HD04-BASHO   PIC  X(02).
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  HD04-BASHONM PIC  N(18)
                      CHARACTER TYPE IS PIT-1V5.
     03  FILLER  PIC  X(06)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"連携_："
                      CHARACTER TYPE IS PIT-1V5-2W.
     03  HD04-RENNO   PIC  N(09)
                      CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER  PIC  X(20)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"納品日："
                      CHARACTER TYPE IS PIT-2.
     03  HD04-Y  PIC  9(04).
     03  FILLER  PIC  N(01)  VALUE NC"年"
                      CHARACTER TYPE IS PIT-2.
     03  HD04-M  PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"月"
                      CHARACTER TYPE IS PIT-2.
     03  HD04-D  PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"日"
                      CHARACTER TYPE IS PIT-2.

 01  HEAD05.
     03  FILLER  PIC  N(05)  VALUE NC"部門　　："
                      CHARACTER TYPE IS PIT-2.
     03  HD05-BMNCD   PIC  X(04).
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"ルート："
                      CHARACTER TYPE IS PIT-2.
     03  HD05-ROUTE   PIC  X(10).
     03  FILLER  PIC  X(16)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"状態："
                      CHARACTER TYPE IS PIT-1V5-2W.
     03  HD05-STS  PIC  N(05)
                      CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER  PIC  X(32)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"実納日："
                      CHARACTER TYPE IS PIT-2.
     03  HD05-Y  PIC  9(04).
     03  FILLER  PIC  N(01)  VALUE NC"年"
                      CHARACTER TYPE IS PIT-2.
     03  HD05-M  PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"月"
                      CHARACTER TYPE IS PIT-2.
     03  HD05-D  PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"日"
                      CHARACTER TYPE IS PIT-2.

 01  HEAD06  CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  N(07)  VALUE NC"自社商品コード".
     03  FILLER  PIC  X(03)  VALUE SPACE.
     03  FILLER  PIC  N(05)  VALUE NC"商　品　名".
     03  FILLER  PIC  X(22)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"原単価".
     03  FILLER  PIC  X(03)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"数量合計".
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"（店）".
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"（店）".
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"（店）".
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"（店）".
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"（店）".
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"（店）".
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"（店）".
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"（店）".
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"（店）".
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"（店）".

 01  HEAD07  CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  N(07)  VALUE NC"相手商品コード".
     03  FILLER  PIC  X(03)  VALUE SPACE.
     03  FILLER  PIC  N(05)  VALUE NC"規　格　　".
     03  FILLER  PIC  X(22)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"売単価".
     03  FILLER  PIC  X(13)  VALUE SPACE.
     03  FILLER  PIC  N(02)  VALUE NC"数量".
     03  FILLER  PIC  X(03)  VALUE SPACE.
     03  FILLER  PIC  N(02)  VALUE NC"数量".
     03  FILLER  PIC  X(03)  VALUE SPACE.
     03  FILLER  PIC  N(02)  VALUE NC"数量".
     03  FILLER  PIC  X(03)  VALUE SPACE.
     03  FILLER  PIC  N(02)  VALUE NC"数量".
     03  FILLER  PIC  X(03)  VALUE SPACE.
     03  FILLER  PIC  N(02)  VALUE NC"数量".
     03  FILLER  PIC  X(03)  VALUE SPACE.
     03  FILLER  PIC  N(02)  VALUE NC"数量".
     03  FILLER  PIC  X(03)  VALUE SPACE.
     03  FILLER  PIC  N(02)  VALUE NC"数量".
     03  FILLER  PIC  X(03)  VALUE SPACE.
     03  FILLER  PIC  N(02)  VALUE NC"数量".
     03  FILLER  PIC  X(03)  VALUE SPACE.
     03  FILLER  PIC  N(02)  VALUE NC"数量".
     03  FILLER  PIC  X(03)  VALUE SPACE.
     03  FILLER  PIC  N(02)  VALUE NC"数量".

 01  YKSEN.
     03  FILLER  PIC  X(135)  VALUE ALL "-".

 01  MEISAI01.
     03  MS01-SHOCD   PIC  X(08).
     03  MS01-HINTAN  PIC  X(08).
     03  FILLER       PIC  X(01).
     03  MS01-SHONM1  PIC  X(15).
     03  MS01-SHONM2  PIC  X(15).
     03  FILLER       PIC  X(01).
     03  MS01-GTANKA  PIC  ZZZZZZ9.99.
     03  FILLER       PIC  X(01).
     03  MS01-SURY-GK PIC  Z(06).
     03  FILLER       PIC  X(01).
     03  MS01-TEN-G.
      05  MS01-TEN-G2  OCCURS 10.
        07  MS01-TENCD  PIC  X(06).
        07  FILLER      PIC  X(01).

 01  MEISAI02  CHARACTER TYPE IS PIT-2.
     03  MS02-AIT-SHOCD  PIC  X(13).
     03  FILLER          PIC  X(04).
     03  MS02-KIKAKU     PIC  X(15).
*****↓2012/07/26 UPDATE ST
*****03  FILLER          PIC  X(16).
     03  FILLER          PIC  X(03).
     03  MS02-KOURI-KBN  PIC  N(06).
     03  FILLER          PIC  X(01).
*****↑2012/07/26 UPDATE ED
     03  MS02-UTANKA     PIC  Z(07).
     03  FILLER          PIC  X(11).
     03  MS02-SURY-G.
      05  MS02-SURY-G2  OCCURS 10.
        07  MS02-SURY    PIC  Z(05).
        07  FILLER       PIC  X(02).

 01  GOKEI01  CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(15)  VALUE SPACE.
     03  GK01-GKSYURUI   PIC  N(05).
     03  FILLER  PIC  X(02)  VALUE SPACE.

     03  FILLER  PIC  N(04)  VALUE NC"数量合計".
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  GK01-SURY-GK    PIC  ZZZ,ZZ9.
     03  FILLER  PIC  X(02)  VALUE SPACE.

     03  FILLER  PIC  N(04)  VALUE NC"原価金額".
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  GK01-GKINGK     PIC   ----,---,--9.
     03  FILLER  PIC  X(02)  VALUE SPACE.

     03  FILLER  PIC  N(04)  VALUE NC"平均原価".
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  GK01-GHEIKN     PIC  Z,ZZZ,ZZ9.
     03  FILLER  PIC  X(02)  VALUE SPACE.

     03  FILLER  PIC  N(04)  VALUE NC"売上金額".
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  GK01-UKINGK      PIC  ----,---,--9.
     03  FILLER  PIC  X(02)  VALUE SPACE.

     03  FILLER  PIC  N(04)  VALUE NC"平均売価".
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  GK01-UHEIKN      PIC  Z,ZZZ,ZZ9.

 01  FILE-ERR.
     03  LSW-ERR           PIC  N(20)  VALUE
         NC"小売連携リストワークエラー".
     03  KAN-ERR           PIC  N(20)  VALUE
         NC"連携Ｎｏ管理テーブルエラー".
     03  TOK-ERR           PIC  N(20)  VALUE
         NC"取引先マスタエラー".
     03  MEI-ERR           PIC  N(20)  VALUE
         NC"商品名称マスタエラー".
     03  SOK-ERR           PIC  N(20)  VALUE
         NC"倉庫マスタエラー".
     03  PRT-ERR           PIC  N(20)  VALUE
         NC"プリントファイルエラー".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
 LINKAGE               SECTION.
****************************************************************
* 入力パラメータ
*  出力順序区分
*    "1":相手コード順、"2":自社コード順
 01  PAR-JYUN-KBN          PIC  X(01).

* 出力パラメータ
*
**************************************************************
 PROCEDURE             DIVISION  USING PAR-JYUN-KBN.
**************************************************************
 DECLARATIVES.
 LSW-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARLSTF.
     DISPLAY     LSW-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     LSW-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARKANF.
     DISPLAY     KAN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KAN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     MEI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 PRJ-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS START"       TO  S-NAME.

     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL END-FLG = "END".
     PERFORM  END-SEC.

     STOP RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "INIT-SEC"       TO  S-NAME.

     DISPLAY   "**  START SNA0130L  **"  UPON CONS.

     PERFORM  SDATE-GET-SEC.
*ファイルのＯＰＥＮ
     OPEN  INPUT  NARLSTF.
     OPEN  I-O    NARKANF.
     OPEN  INPUT  HTOKMS.
     OPEN  INPUT HMEIMS.
     OPEN  INPUT ZSOKMS.
     OPEN  OUTPUT PRTFILE.
*ワークの初期化
     INITIALIZE  WRK-AREA.
     INITIALIZE  FLG-AREA.

     MOVE  SPACE            TO  FG-NARLSTF-END.
     PERFORM  RD-NARLSTF-SEC.
     IF  FG-NARLSTF-END = "END"
         MOVE  "END"        TO  END-FLG
     END-IF.

 INIT-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 INIT-EXIT.
     EXIT.
****************************************************************
*    システム日付取得                                          *
****************************************************************
 SDATE-GET-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "SDATE-GET-SEC"  TO  S-NAME.
*システム日付・時刻の取得
     ACCEPT  WK-DATE   FROM DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  SYS-DATE.
     ACCEPT  WK-TIME   FROM TIME.

 SDATE-GET-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 SDATE-GET-EXIT.
     EXIT.
****************************************************************
*    小売連携リストワーク読込み　                              *
****************************************************************
 RD-NARLSTF-SEC        SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-NARLSTF-SEC" TO  S-NAME.

* 並び順
*   出力順区分="1":相手コード順
*     1.連携Ｎ-
*     2.部門（分類）
*     3.ルート
*     4.納品日
*     5.相手商品コード
*     6.店コード
*
*   出力順区分="2":自社コード順
*     1.連携Ｎ-
*     2.部門（分類）
*     3.ルート
*     4.納品日
*     5.自社商品コード
*     6.自社品単コード
*     7.店コード

     READ  NARLSTF
       AT  END
         MOVE  "END"        TO  FG-NARLSTF-END
         GO TO  RD-NARLSTF-090
     END-READ.

     ADD 1   TO  CT-IN.

     IF  LSW-F07 NOT NUMERIC *> 注文日（発注日）
         MOVE  ZERO         TO  LSW-F07
     END-IF.

     IF  LSW-F08 NOT NUMERIC *> 納品日
         MOVE  ZERO         TO  LSW-F08
     END-IF.

     IF  LSW-F09 NOT NUMERIC *> 実納品日
         MOVE  ZERO         TO  LSW-F09
     END-IF.

     IF  LSW-F10 NOT NUMERIC *> 店舗コード
         MOVE  ZERO         TO  LSW-F10
     END-IF.

*****2012/07/26↓ブレイクキーサカタ商品＋品単又はＪＡＮ
     MOVE      SPACE        TO  LSW-SYOJAN.
     IF    PAR-JYUN-KBN = 1
           MOVE  LSW-F21    TO  LSW-SYOJAN
     ELSE
           MOVE  LSW-F19    TO  LSW-SYOJAN(1:8)
           MOVE  LSW-F20    TO  LSW-SYOJAN(9:8)
     END-IF.
*****2012/07/26↑ブレイクキーサカタ商品＋品単又はＪＡＮ

 RD-NARLSTF-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 RD-NARLSTF-EXIT.
     EXIT.

****************************************************************
*             メイン処理                             2.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "MAIN-SEC"       TO  S-NAME.

     INITIALIZE  WK-GOK.
*2013/03/08 NAV ST 行数変更
*****MOVE  59               TO  CT-LINE.
     MOVE  49               TO  CT-LINE.
*2013/03/08 NAV ED 行数変更

     PERFORM  UNTIL FG-NARLSTF-END = "END"
       PERFORM  MAINB-SEC

     END-PERFORM.

     PERFORM  GOK-SOK-PRT-SEC.
     MOVE  "END"            TO  END-FLG.
 MAIN-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 MAIN-EXIT.
     EXIT.
****************************************************************
*  メインＢ処理                                                *
****************************************************************
 MAINB-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "MAINB-SEC"      TO  S-NAME.

     MOVE  LSW-F01          TO  BRK-RENNO.
     MOVE  LSW-F02          TO  BRK-BMNCD.

     PERFORM  UNTIL FG-NARLSTF-END = "END"
                 OR LSW-F01    NOT = BRK-RENNO
                 OR LSW-F02    NOT = BRK-BMNCD
       PERFORM  MAINC-SEC

     END-PERFORM.

     PERFORM  GOK-GOK-PRT-SEC.

*2013/03/08 NAV ST 行数を変更
*****MOVE  59               TO  CT-LINE.
     MOVE  49               TO  CT-LINE.
*2013/03/08 NAV ED 行数を変更

 MAINB-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 MAINB-EXIT.
     EXIT.
****************************************************************
*  メインＣ処理                                                *
****************************************************************
 MAINC-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "MAINC-SEC"      TO  S-NAME.

     MOVE  LSW-F16          TO  BRK-ROUTE.

     PERFORM  UNTIL FG-NARLSTF-END = "END"
                   OR LSW-F01    NOT = BRK-RENNO
                   OR LSW-F02    NOT = BRK-BMNCD
                   OR LSW-F16    NOT = BRK-ROUTE
       PERFORM  MAIND-SEC

     END-PERFORM.

     PERFORM  GOK-CHU-PRT-SEC.

     IF     LSW-F01 NOT = BRK-RENNO
         OR LSW-F02 NOT = BRK-BMNCD
         CONTINUE
     ELSE
*2013/03/08 NAV ST 行数を変更
*********MOVE  59           TO  CT-LINE
         MOVE  49           TO  CT-LINE
*2013/03/08 NAV ED 行数を変更
     END-IF.

 MAINC-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 MAINC-EXIT.
     EXIT.
****************************************************************
*  メインＤ処理                                                *
****************************************************************
 MAIND-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "MAIND-SEC"      TO  S-NAME.

     MOVE  LSW-F08          TO  BRK-NOHNBI.

     PERFORM  UNTIL FG-NARLSTF-END = "END"
                 OR LSW-F01    NOT = BRK-RENNO
                 OR LSW-F02    NOT = BRK-BMNCD
                 OR LSW-F16    NOT = BRK-ROUTE
                 OR LSW-F08    NOT = BRK-NOHNBI

       PERFORM  MAINE-SEC

     END-PERFORM.

     PERFORM  GOK-SYO-PRT-SEC.
     IF     LSW-F01 NOT = BRK-RENNO
         OR LSW-F02 NOT = BRK-BMNCD
         OR LSW-F16 NOT = BRK-ROUTE
         CONTINUE
     ELSE
*2013/03/08 NAV ST 行数を変更
*********MOVE  59           TO  CT-LINE
         MOVE  49           TO  CT-LINE
*2013/03/08 NAV ED 行数を変更
     END-IF.

 MAIND-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 MAIND-EXIT.
     EXIT.
****************************************************************
*  メインＥ処理                                                *
****************************************************************
 MAINE-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "MAINE-SEC"      TO  S-NAME.

     INITIALIZE  WK-GYO. *> 行情報格納領域の初期化

     MOVE  ZERO             TO  IX-GYO.
     MOVE  ZERO             TO  IX-GYOMAX.
     MOVE  LSW-F19          TO  WK-GYO-SHOCD.
     MOVE  LSW-F20          TO  WK-GYO-HINTAN.
     MOVE  LSW-F21          TO  WK-GYO-AIT-SHOCD.
     MOVE  LSW-F23          TO  WK-GYO-GTANKA.
     MOVE  LSW-F24          TO  WK-GYO-UTANKA.
*****2012/07/26 ADD    ST
     MOVE  LSW-F29          TO  WK-GYO-KOURI-KBN.
     MOVE  LSW-F30          TO  WK-GYO-SYOHIN1.
     MOVE  LSW-F31          TO  WK-GYO-SYOHIN2.
*****2012/07/26 ADD    ED
     MOVE  LSW-REC          TO  WK-GYO-REC. *>先頭ﾚｺｰﾄﾞを退避

     MOVE  LSW-F19          TO  BRK-SHOCD.
     MOVE  LSW-F20          TO  BRK-HINTAN.

*****2012/07/26↓商品ＣＤブレイクキーセット ST
     MOVE  SPACE            TO  BRK-SYOJAN.
     IF    PAR-JYUN-KBN = 1
           MOVE  LSW-F21    TO  BRK-SYOJAN
     ELSE
           MOVE  LSW-F19    TO  BRK-SYOJAN(1:8)
           MOVE  LSW-F20    TO  BRK-SYOJAN(9:8)
     END-IF.
*****2012/07/26↑商品ＣＤブレイクキーセット ED

* １商品分をワークに退避してから作表する。
     PERFORM  UNTIL FG-NARLSTF-END = "END"
                 OR LSW-F01    NOT = BRK-RENNO
                 OR LSW-F02    NOT = BRK-BMNCD
                 OR LSW-F16    NOT = BRK-ROUTE
                 OR LSW-F08    NOT = BRK-NOHNBI
*****************2012/07/26↓ブレイク変更　自社商品又はＪＡＮ
*****************OR LSW-F19    NOT = BRK-SHOCD
*****************OR LSW-F20    NOT = BRK-HINTAN
                 OR LSW-SYOJAN NOT = BRK-SYOJAN
*****************2012/07/26↑ブレイク変更　自社商品又はＪＡＮ

       PERFORM  MAINF-SEC

     END-PERFORM.

     PERFORM  PRT-SEC.

 MAINE-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 MAINE-EXIT.
     EXIT.
****************************************************************
*  メインＦ処理                                                *
****************************************************************
 MAINF-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "MAINF-SEC"      TO  S-NAME.

     INITIALIZE  WK-GSUM.
     MOVE  LSW-F10          TO  BRK-TENCD.

     PERFORM  UNTIL FG-NARLSTF-END = "END"
                 OR LSW-F01    NOT = BRK-RENNO
                 OR LSW-F02    NOT = BRK-BMNCD
                 OR LSW-F16    NOT = BRK-ROUTE
                 OR LSW-F08    NOT = BRK-NOHNBI
*****************2012/07/26↓ブレイク変更　自社商品又はＪＡＮ
*****************OR LSW-F19    NOT = BRK-SHOCD
*****************OR LSW-F20    NOT = BRK-HINTAN
                 OR LSW-SYOJAN NOT = BRK-SYOJAN
*****************2012/07/26↑ブレイク変更　自社商品又はＪＡＮ
                 OR LSW-F10    NOT = BRK-TENCD

         PERFORM  SUM-GYO-SEC
         PERFORM  RD-NARLSTF-SEC *> 次レコード読込み

     END-PERFORM.

     PERFORM  SV-GYO-SEC.

 MAINF-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 MAINF-EXIT.
     EXIT.
****************************************************************
*  行集計処理                                                  *
****************************************************************
 SUM-GYO-SEC           SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "SUM-GYO-SEC"    TO  S-NAME.

     IF  LSW-F10 NOT = ZERO
         MOVE  LSW-F10      TO  WK-GSUM-TENCD-R
     ELSE
         MOVE  SPACE        TO  WK-GSUM-TENCD-R
     END-IF.

* 店の合計数量
     COMPUTE  WK-GSUM-SURY-R =
         WK-GSUM-SURY-R + LSW-F22.

* 商品の合計数量
     COMPUTE  WK-GYO-SURY-GK =
         WK-GYO-SURY-GK + LSW-F22.

* 合計行の集計
     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 4
       *> 小計、納入日
       *> 中計、ルート
       *> 合計、連携Ｎｏ、部門
       *> 総計、全体

       COMPUTE  WK-GOK-SURYO  (IX) =
           WK-GOK-SURYO  (IX) +  LSW-F22

       COMPUTE  WK-GOK-GKINGK (IX) =
           WK-GOK-GKINGK (IX) +  LSW-F25

       COMPUTE  WK-GOK-UKINGK (IX) =
           WK-GOK-UKINGK (IX) +  LSW-F26

     END-PERFORM.

 SUM-GYO-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 SUM-GYO-EXIT.
     EXIT.
****************************************************************
*  行情報退避処理                                              *
****************************************************************
 SV-GYO-SEC            SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "SV-GYO-SEC"     TO  S-NAME.

     IF  IX-GYO >= 1000
         DISPLAY "SNA0130L  TBL-OVER ???"  UPON CONS
         MOVE  4000         TO  PROGRAM-STATUS
         EXIT PROGRAM
     END-IF.

     ADD  1   TO  IX-GYO.
     MOVE  IX-GYO           TO  IX-GYOMAX.

     MOVE  WK-GSUM-TENCD-R  TO  WK-GYO-TENCD-R (IX-GYO).
     MOVE  WK-GSUM-SURY-R   TO  WK-GYO-SURY-R  (IX-GYO).

 SV-GYO-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 SV-GYO-EXIT.
     EXIT.
****************************************************************
*  印刷処理                                                    *
****************************************************************
 PRT-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "PRT-SEC"        TO  S-NAME.

     PERFORM  PRTB-SEC.

 PRT-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 PRT-EXIT.
     EXIT.
****************************************************************
*  印刷Ｂ処理                                                  *
****************************************************************
 PRTB-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "PRTB-SEC"       TO  S-NAME.

     MOVE  SPACE            TO  MEISAI01.
     MOVE  SPACE            TO  MEISAI02.

     MOVE  WK-GYO-SHOCD     TO  MS01-SHOCD.
     MOVE  WK-GYO-HINTAN    TO  MS01-HINTAN.

* 商品名称マスタ
     MOVE  WK-GYO-SHOCD     TO  MEI-F011. *> 商品CD
     MOVE  WK-GYO-HINTAN    TO  MEI-F012. *> 品単
     PERFORM  RD-HMEIMS-SEC.
     IF  FG-HMEIMS-INV = ZERO
         MOVE  MEI-F031     TO  MS01-SHONM1
         MOVE  MEI-F032     TO  MS01-SHONM2
     END-IF.
*****2012/07/26↓ADD    ST
     MOVE  WK-GYO-SYOHIN1   TO  MS01-SHONM1
     MOVE  WK-GYO-SYOHIN2   TO  MS01-SHONM2
*****2012/07/26↓ADD    ED

     MOVE  WK-GYO-GTANKA    TO  MS01-GTANKA.
     MOVE  WK-GYO-SURY-GK   TO  MS01-SURY-GK.

     MOVE  WK-GYO-AIT-SHOCD TO  MS02-AIT-SHOCD.
     MOVE  SPACE            TO  MS02-KIKAKU.
*****↓2012/07/26 ADD    ST
     IF    WK-GYO-KOURI-KBN = "1"
           MOVE  NC"☆小売出荷☆"  TO  MS02-KOURI-KBN
     ELSE
           MOVE  SPACE             TO  MS02-KOURI-KBN
     END-IF.
     MOVE  WK-GYO-UTANKA    TO  MS02-UTANKA.

     DIVIDE  10  INTO IX-GYOMAX
       GIVING    IX-GYOMAX2
       REMAINDER WK-AMARI.

     IF  WK-AMARI NOT = ZERO
         ADD  1   TO IX-GYOMAX2
     END-IF.

     PERFORM  VARYING IX-GYO  FROM 1 BY 1
              UNTIL   IX-GYO > IX-GYOMAX2  *> 行

       PERFORM  PRTC-SEC

     END-PERFORM.

*改頁を判定せず線を引く。（前行が59行の場合60行目に印刷）×
* 改頁を判定せず線を引く。(前行が49行の場合50行目に印刷）
     WRITE  PRT-REC  FROM YKSEN  AFTER 1.
     ADD  1   TO  CT-LINE.

 PRTB-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 PRTB-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ検索                                        *
****************************************************************
 RD-HMEIMS-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-HMEIMS-SEC"  TO  S-NAME.

     READ  HMEIMS
       INVALID
         MOVE  1            TO  FG-HMEIMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HMEIMS-INV
     END-READ.

 PRT-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 PRT-EXIT.
     EXIT.
****************************************************************
*  印刷Ｃ処理                                                  *
****************************************************************
 PRTC-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "PRTC-SEC"       TO  S-NAME.

     MOVE  2                TO  WK-JIKKO-LINE.
     PERFORM  HD-PRT-SEC.

     IF  IX-GYO = 1 *> 先頭行のみ商品ＣＤ等は表示する。
         MOVE SPACE         TO  MS01-TEN-G
         MOVE SPACE         TO  MS02-SURY-G
     ELSE           *> 2行目移行は空白にする。
         MOVE  SPACE        TO  MEISAI01
         MOVE  SPACE        TO  MEISAI02
     END-IF.

     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 10  *> 列
       MOVE  WK-GYO-TENCD (IX-GYO IX) TO  MS01-TENCD (IX)
       MOVE  WK-GYO-SURY  (IX-GYO IX) TO  MS02-SURY  (IX)

     END-PERFORM.

     WRITE  PRT-REC  FROM  MEISAI01  AFTER 1.
     WRITE  PRT-REC  FROM  MEISAI02  AFTER 1.

     ADD 2  TO CT-LINE.

 PRTC-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 PRTC-EXIT.
     EXIT.
****************************************************************
*  ヘッダ印刷処理                                              *
****************************************************************
 HD-PRT-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "HC-PRT-SEC"     TO  S-NAME.

     COMPUTE  WK-CT-LINE = CT-LINE + WK-JIKKO-LINE.

*2013/03/08 NAV ST 行数を変更する
*****IF  WK-CT-LINE > 59  *> 改頁の判定
     IF  WK-CT-LINE > 49  *> 改頁の判定
*2013/03/08 NAV ED 行数を変更する
         MOVE  1            TO  FG-HD-PRT
     ELSE
         MOVE  ZERO         TO  FG-HD-PRT
     END-IF.

     IF  FG-HDPRT-CHK = 1 *> 改頁のチェックのみ
         MOVE  ZERO            TO  FG-HDPRT-CHK
         GO TO  HD-PRT-090
     END-IF.

     IF  FG-HD-PRT = 1 *> 改頁
         PERFORM  HD-PRTB-SEC
     END-IF.

 HD-PRT-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 HD-PRT-EXIT.
     EXIT.
****************************************************************
*  ヘッダ印刷Ｂ処理                                            *
****************************************************************
 HD-PRTB-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "HC-PRTB-SEC"    TO  S-NAME.

* 処理日
     MOVE  SYS-DATE (1:4)   TO  HD01-Y.
     MOVE  SYS-DATE (5:2)   TO  HD01-M.
     MOVE  SYS-DATE (7:2)   TO  HD01-D.

* ページ
     ADD  1   TO  CT-PG.
     MOVE  CT-PG            TO  HD01-PG.

* 並び順
     IF  PAR-JYUN-KBN = "1"
         MOVE  NC"相手商品順"    TO  HD02-JYUN
     ELSE
         MOVE  NC"自社商品順"    TO  HD02-JYUN
     END-IF.

* 取引先ＣＤ
     MOVE  WK-GYO-F04       TO  HD03-TORCD.
* 取引先名
*  取引先マスタ
     MOVE  WK-GYO-F04       TO  TOK-F01 *> 相手取引先コード
     PERFORM  RD-HTOKMS-SEC.
     IF  FG-HTOKMS-INV = ZERO
         MOVE  TOK-F02      TO  HD03-TORNM
     END-IF.
* オンライン／手書き
     IF  WK-GYO-F15 = "1"
         MOVE  NC"　手　書　"    TO  HD03-TEGONL
     ELSE
         MOVE  NC"オンライン"    TO  HD03-TEGONL
     END-IF.

* 発注日
     MOVE  WK-GYO-F07(1:4)  TO  HD03-Y.
     MOVE  WK-GYO-F07(5:2)  TO  HD03-M.
     MOVE  WK-GYO-F07(7:2)  TO  HD03-D.

* 場所
     MOVE  WK-GYO-F18       TO  HD04-BASHO.

* 場所名
*  倉庫マスタ
     MOVE  WK-GYO-F18       TO  SOK-F01.
     PERFORM  RD-ZSOKMS-SEC.
     IF  FG-ZSOKMS-INV = ZERO
         MOVE  SOK-F02      TO  HD04-BASHONM
     END-IF.

* 連携Ｎｏ
*  連携Ｎ-を日本語変換する。
     MOVE  ALL NC"？"       TO  WK-RENNO-N.
     MOVE  WK-GYO-F01       TO  WK-RENNO.
     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 9
       PERFORM  VARYING  IX2  FROM 1 BY 1
                UNTIL    IX2 > 11

         IF  TB-CVT-SUR (IX2) = WK-RENNOR (IX)
             MOVE  TB-CVT-SU-NR (IX2)  TO WK-RENNO-NR (IX)
             MOVE  11       TO  IX2
         END-IF
       END-PERFORM

     END-PERFORM.

     MOVE  WK-RENNO-N       TO  HD04-RENNO.
* 納品日
     MOVE  WK-GYO-F08(1:4)  TO  HD04-Y.
     MOVE  WK-GYO-F08(5:2)  TO  HD04-M.
     MOVE  WK-GYO-F08(7:2)  TO  HD04-D.

* 部門ＣＤ
     MOVE  WK-GYO-F02       TO  HD05-BMNCD.
* ルート
     MOVE  WK-GYO-F16       TO  HD05-ROUTE.
* 状態

     MOVE  WK-GYO-F01       TO  KAN-F01.
     PERFORM  RD-NARKANF-SEC.
     IF  FG-NARKANF-INV = ZERO
         EVALUATE  TRUE
           WHEN  KAN-F04 = "1"  *> 未送信
             MOVE  NC"未送信"      TO  HD05-STS

           WHEN  KAN-F04 = "2"  *> 苗側未取込
             MOVE  NC"苗側未取込"  TO  HD05-STS

           WHEN  KAN-F04 = "3"  *> 取込済
             MOVE  NC"取込み済"    TO  HD05-STS

           WHEN  KAN-F04 = "4"  *> 未取消
             MOVE  NC"未取消し"    TO  HD05-STS

           WHEN  KAN-F04 = "5"  *> 取消済み
             MOVE  NC"取消済"      TO  HD05-STS

         END-EVALUATE
     END-IF.

* 実納品日
     MOVE  WK-GYO-F09(1:4)  TO  HD05-Y.
     MOVE  WK-GYO-F09(5:2)  TO  HD05-M.
     MOVE  WK-GYO-F09(7:2)  TO  HD05-D.

     IF  CT-PG > 1
         MOVE  SPACE        TO  PRT-REC
         WRITE  PRT-REC  AFTER PAGE
     END-IF.

     WRITE  PRT-REC  FROM HEAD01  AFTER 0.
     WRITE  PRT-REC  FROM HEAD02  AFTER 1.
     WRITE  PRT-REC  FROM HEAD03  AFTER 1.
     WRITE  PRT-REC  FROM HEAD04  AFTER 1.
     WRITE  PRT-REC  FROM HEAD05  AFTER 1.
     WRITE  PRT-REC  FROM HEAD06  AFTER 2.
     WRITE  PRT-REC  FROM HEAD07  AFTER 1.
     MOVE  SPACE            TO  PRT-REC.
     WRITE  PRT-REC  AFTER 1.

     MOVE  9                TO  CT-LINE.

 HD-PRTB-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 HD-PRTB-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ検索                                          *
****************************************************************
 RD-HTOKMS-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-HTOKMS-SEC"  TO  S-NAME.

     READ  HTOKMS
       INVALID
         MOVE  1            TO  FG-HTOKMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HTOKMS-INV
     END-READ.

 RD-ZSOKMS-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 RD-HTOKMS-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ検索                                            *
****************************************************************
 RD-ZSOKMS-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-ZSOKMS-SEC"  TO  S-NAME.

     READ  ZSOKMS
       INVALID
         MOVE  1            TO  FG-ZSOKMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-ZSOKMS-INV
     END-READ.

 RD-ZSOKMS-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 RD-ZSOKMS-EXIT.
     EXIT.
****************************************************************
*    連携Ｎｏ管理テーブル検索                                  *
****************************************************************
 RD-NARKANF-SEC        SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-NARKANF-SEC"  TO  S-NAME.

     READ  NARKANF
       INVALID
         MOVE  1            TO  FG-NARKANF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-NARKANF-INV
     END-READ.

 RD-NARKANF-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 RD-NARKANF-EXIT.
     EXIT.
****************************************************************
*  印刷処理（小計）                                            *
****************************************************************
 GOK-SYO-PRT-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "GOK-SYO-PRT-SEC"  TO  S-NAME.

     MOVE  NC"【小　計】"   TO  GK01-GKSYURUI.
     MOVE  WK-GOK-SURYO (1) TO  GK01-SURY-GK.
     MOVE  WK-GOK-GKINGK(1) TO  GK01-GKINGK.

     IF  WK-GOK-SURYO (1) NOT = ZERO
         COMPUTE  GK01-GHEIKN  =
             WK-GOK-GKINGK(1) /  WK-GOK-SURYO (1)
     ELSE
         MOVE  ZERO         TO  GK01-GHEIKN
     END-IF.

     MOVE  WK-GOK-UKINGK(1) TO  GK01-UKINGK,

     IF  WK-GOK-SURYO (1) NOT = ZERO
         COMPUTE  GK01-UHEIKN  =
             WK-GOK-UKINGK(1) /  WK-GOK-SURYO (1)
     ELSE
         MOVE  ZERO         TO  GK01-UHEIKN
     END-IF.

     MOVE  ZERO             TO  WK-GOK-SURYO  (1).
     MOVE  ZERO             TO  WK-GOK-GKINGK (1).
     MOVE  ZERO             TO  WK-GOK-UKINGK (1).

     IF  CT-LINE NOT = 60
         WRITE  PRT-REC  FROM GOKEI01  AFTER 2
         ADD  2   TO  CT-LINE
     ELSE *> 60行目に線を印刷した場合
         WRITE  PRT-REC  FROM GOKEI01  AFTER 1
         ADD  1   TO  CT-LINE
     END-IF.

 GOK-SYO-PRT-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 GOK-SYO-PRT-EXIT.
     EXIT.
****************************************************************
*  印刷処理（中計）                                            *
****************************************************************
 GOK-CHU-PRT-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "GOK-CHU-PRT-SEC"        TO  S-NAME.

     MOVE  NC"【中　計】"   TO  GK01-GKSYURUI.
     MOVE  WK-GOK-SURYO (2) TO  GK01-SURY-GK.
     MOVE  WK-GOK-GKINGK(2) TO  GK01-GKINGK.

     IF  WK-GOK-SURYO (2) NOT = ZERO
         COMPUTE  GK01-GHEIKN  =
             WK-GOK-GKINGK(2) /  WK-GOK-SURYO (2)
     ELSE
         MOVE  ZERO         TO  GK01-GHEIKN
     END-IF.

     MOVE  WK-GOK-UKINGK(2) TO  GK01-UKINGK,

     IF  WK-GOK-SURYO (2) NOT = ZERO
         COMPUTE  GK01-UHEIKN  =
             WK-GOK-UKINGK(2) /  WK-GOK-SURYO (2)
     ELSE
         MOVE  ZERO         TO  GK01-UHEIKN
     END-IF.

     MOVE  ZERO             TO  WK-GOK-SURYO  (2).
     MOVE  ZERO             TO  WK-GOK-GKINGK (2).
     MOVE  ZERO             TO  WK-GOK-UKINGK (2).

     WRITE  PRT-REC  FROM GOKEI01  AFTER 1.

     ADD  1   TO  CT-LINE.

 GOK-CHU-PRT-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 GOK-CHU-PRT-EXIT.
     EXIT.
****************************************************************
*  印刷処理（合計）                                            *
****************************************************************
 GOK-GOK-PRT-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "GOK-GOK-PRT-SEC"        TO  S-NAME.

     MOVE  NC"【合　計】"   TO  GK01-GKSYURUI.
     MOVE  WK-GOK-SURYO (3) TO  GK01-SURY-GK.
     MOVE  WK-GOK-GKINGK(3) TO  GK01-GKINGK.

     IF  WK-GOK-SURYO (3) NOT = ZERO
         COMPUTE  GK01-GHEIKN  =
             WK-GOK-GKINGK(3) /  WK-GOK-SURYO (3)
     ELSE
         MOVE  ZERO         TO  GK01-GHEIKN
     END-IF.

     MOVE  WK-GOK-UKINGK(3) TO  GK01-UKINGK,

     IF  WK-GOK-SURYO (3) NOT = ZERO
         COMPUTE  GK01-UHEIKN  =
             WK-GOK-UKINGK(3) /  WK-GOK-SURYO (3)
     ELSE
         MOVE  ZERO         TO  GK01-UHEIKN
     END-IF.

     MOVE  ZERO             TO  WK-GOK-SURYO  (3).
     MOVE  ZERO             TO  WK-GOK-GKINGK (3).
     MOVE  ZERO             TO  WK-GOK-UKINGK (3).

     WRITE  PRT-REC  FROM GOKEI01  AFTER 1.

     ADD  1   TO  CT-LINE.

 GOK-GOK-PRT-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 GOK-GOK-PRT-EXIT.
     EXIT.
****************************************************************
*  印刷処理（総計）                                            *
****************************************************************
 GOK-SOK-PRT-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "GOK-SOK-PRT-SEC"        TO  S-NAME.

     MOVE  NC"【総合計】"   TO  GK01-GKSYURUI.
     MOVE  WK-GOK-SURYO (4) TO  GK01-SURY-GK.
     MOVE  WK-GOK-GKINGK(4) TO  GK01-GKINGK.

     IF  WK-GOK-SURYO (4) NOT = ZERO
         COMPUTE  GK01-GHEIKN  =
             WK-GOK-GKINGK(4) /  WK-GOK-SURYO (4)
     ELSE
         MOVE  ZERO         TO  GK01-GHEIKN
     END-IF.

     MOVE  WK-GOK-UKINGK(4) TO  GK01-UKINGK,

     IF  WK-GOK-SURYO (4) NOT = ZERO
         COMPUTE  GK01-UHEIKN  =
             WK-GOK-UKINGK(4) /  WK-GOK-SURYO (4)
     ELSE
         MOVE  ZERO         TO  GK01-UHEIKN
     END-IF.

     WRITE  PRT-REC  FROM GOKEI01  AFTER 1.

     ADD  1   TO  CT-LINE.

 GOK-SOK-PRT-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 GOK-SOK-PRT-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "END-SEC"        TO  S-NAME.

     DISPLAY  "SNA0130L NARLSTF IN =" CT-IN  UPON CONS.
     DISPLAY  "SNA0130L PRTF  PAGE =" CT-PG  UPON CONS.

*ファイル ＣＬＯＳＥ
     CLOSE  NARLSTF.
     CLOSE  NARKANF.
     CLOSE  HTOKMS.
     CLOSE  HMEIMS.
     CLOSE  ZSOKMS.
     CLOSE  PRTFILE.
     DISPLAY   "**  END   SNA0130L  **"  UPON CONS.

 END-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 END-EXIT.
     EXIT.
*****************<<  SNA0130L   END PROGRAM  >>******************
