****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷業務　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援システム            *
*    モジュール名　　　　：　出荷作場修正（数量訂正）入力　　　*
*    作成日／作成者　　　：　2015/05/26 NAV高橋
*    処理概要　　　　　　：　画面より、バッチ_、倉庫コード    *
*                            商品コードを入力し、該当の商品    *
*                            のデータを表示させ、発注数の訂    *
*　　　　　　　　　　　　　　正按分入力を行う。　　　　　　　　*
*                            また、出荷作場の修正を行なう。    *
*    更新日／更新者　　　：　2015/08/14 INOUE                  *
*                            指定条件に合致するレコードが　    *
*                            全て発注確定済の場合の　　　　    *
*                            エラーメッセージを適正化。　　    *
*    更新日／更新者　　　：　2016/08/25 INOUE                  *
*                            本発ＥＤＩ対応　　　　　　　　    *
*    更新日／更新者　　　：　2016/09/15 TAKAHAHSI              *
*                            発注数、訂正数判断の変更　　　    *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY3812K.
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/05/26.
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
                                       DE2-F051  DE2-F07
                                       DE2-F112  DE2-F03
                        WITH  DUPLICATES
                        STATUS         SHTDENLA-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 倉庫マスタ >>--*
     SELECT   SAKUBAL1   ASSIGN         DA-01-VI-SAKUBAL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SOK-F01
                        STATUS         SAKUBAL1-ST.
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
*---<<  ナフコ基本情報ファイル  >>---*
     SELECT   NFJOHOF   ASSIGN    TO        DA-01-VI-NFJOHOL2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JOH-F02  JOH-F03
                                                 JOH-F04  JOH-F05
                                                 JOH-F06  JOH-F07
                                                 JOH-F08  JOH-F09
                        WITH  DUPLICATES
                        FILE      STATUS    IS   JOH-ST.
*----<< ＯＮＬ状況再計算Ｆ >>-*
     SELECT   LNKSAIF   ASSIGN         DA-01-VI-LNKSAIL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SAI-F01  SAI-F02  SAI-F03
                        STATUS         SAI-ST.
*↓2016/08/25
*ナフコ商品マスタ
     SELECT   NFSHOMS1  ASSIGN    TO        DA-01-VI-NFSHOMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F02
                        FILE      STATUS    IS   SHO-ST.
*ナフコ店舗マスタ
     SELECT   NFTENMS1  ASSIGN     TO       DA-01-VI-NFTENMS1
                        ORGANIZATION        INDEXED
                        ACCESS     MODE     RANDOM
                        RECORD     KEY      NFT-F01 NFT-F02
                        FILE    STATUS      NFT-ST.
*ナフコ店舗パターンマスタ
     SELECT   NFTEPTL1  ASSIGN    TO        DA-01-VI-NFTEPTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEP-F01
                                                 TEP-F02
                        FILE      STATUS    IS   TEP-ST.
*↑2016/08/25
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSY38121  OF        XMDLIB.
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
 FD  SAKUBAL1            LABEL RECORD   IS   STANDARD.
     COPY     SAKUBAL1   OF        XFDLIB
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
 FD  NFJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOF   OF        XFDLIB
              JOINING   JOH       PREFIX.
*----<< ＯＮＬ状況再計算Ｆ >>-*
 FD  LNKSAIF.
     COPY     LNKSAIF   OF        XFDLIB
              JOINING   SAI       PREFIX.
*↓2016/08/25
*----<< ナフコ商品マスタ >>--*
 FD  NFSHOMS1           LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS1  OF        XFDLIB
              JOINING   SHO       PREFIX.
*----<< ナフコ店舗マスタ >>--*
 FD  NFTENMS1           LABEL RECORD   IS   STANDARD.
     COPY     NFTENMS1  OF        XFDLIB
              JOINING   NFT       PREFIX.
*----<< ナフコ店舗パターンマスタ >>--*
 FD  NFTEPTL1           LABEL RECORD   IS   STANDARD.
     COPY     NFTEPTL1  OF        XFDLIB
              JOINING   TEP       PREFIX.
*↑2016/08/25
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  KEP-FLG             PIC  X(01)  VALUE  SPACE.
     03  MEIMST-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  SAKUBAL1-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  NFJOHOF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  LNKSAIF-INV-FLG     PIC  X(03)  VALUE  SPACE.
*↓2016/08/25
     03  NFSHOMS1-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  NFTENMS1-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  NFTEPTL1-INV-FLG    PIC  X(03)  VALUE  SPACE.
*↑2016/08/25
     03  CHK-FLG             PIC  X(03)  VALUE  SPACE.
     03  WK-B-HIDUKE         PIC  9(08)  VALUE  ZERO.
     03  WK-B-TIME           PIC  9(04)  VALUE  ZERO.
     03  WK-B-TORICD         PIC  9(08)  VALUE  ZERO.
     03  WK-B-SOKCD          PIC  X(02)  VALUE  SPACE.
     03  ERK-FLG             PIC  9(01)  VALUE  ZERO.
     03  WK-NOUHIN           PIC  9(08)  VALUE  ZERO.
     03  SAKUBA-CHG-FLG      PIC  X(01)  VALUE  SPACE.
*2015/08/14↓
     03  DEN-HIT-FLG         PIC  X(01)  VALUE  SPACE.
     03  JOH-HIT-FLG         PIC  X(01)  VALUE  SPACE.
*2015/08/14↑
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHTDENLB-ST       PIC  X(02).
 01  SHTDENLA-ST       PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  SAKUBAL1-ST       PIC  X(02).
 01  SHOTBL1-ST        PIC  X(02).
 01  MEIMS1-ST         PIC  X(02).
 01  TENMS1-ST         PIC  X(02).
 01  ZAI-ST            PIC  X(02).
 01  JOH-ST            PIC  X(02).
 01  SAI-ST            PIC  X(02).
*↓2016/08/25
 01  SHO-ST            PIC  X(02).
 01  NFT-ST            PIC  X(02).
 01  TEP-ST            PIC  X(02).
*↑2016/08/25
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
*↓2016/08/25
*    NC"入力の納品日は、売上伝票データに存在しません。".
     NC"この条件は、売上伝票データに存在しません。".
*↑2016/08/25
****10/01/20  欠品区分追加開始　大野
     03  MSG19               PIC  N(30)  VALUE
     NC"欠品制御区分には空白か１を入力して下さい。".
****10/01/20  欠品区分追加終了　大野
     03  MSG20               PIC  N(30)  VALUE
     NC"欠品商品です。欠品区分を入力して下さい。".
     03  MSG21               PIC  N(30)  VALUE
     NC"欠品入力対象外です。他入力で修正して下さい。".
     03  MSG22               PIC  N(30)  VALUE
     NC"作場マスタに存在しません。確認して下さい。".
     03  MSG23               PIC  N(30)  VALUE
     NC"振分倉庫ＣＤは必ず指定して下さい。".
*2015/08/14↓
     03  MSG24               PIC  N(30)  VALUE
     NC"この条件は、全て発注確定済です。".
*2015/08/14↑
*↓2016/08/25
     03  MSG25               PIC  N(30)  VALUE
     NC"ナフコ商品マスタに商品分類コードが未登録です。".
*↑2016/08/25
*
*
*
 01  FILLER                  REDEFINES   MSG-AREA.
*2015/08/14↓
*    03  MSG-TBL             PIC  N(30)  OCCURS      23.
*2015/08/14↑
*↓2016/08/25
*    03  MSG-TBL             PIC  N(30)  OCCURS      24.
     03  MSG-TBL             PIC  N(30)  OCCURS      25.
*↑2016/08/25
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
       05  WRK-SAKUBA        PIC  X(02).
       05  WRK-MAE-SAKUBA    PIC  X(02).
*↓2016/08/25
       05  WRK-MPTN          PIC  X(02).
       05  WRK-MAREA         PIC  X(01).
*↑2016/08/25
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
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY3812K DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    SAKUBAL1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   NFJOHOF   LNKSAIF.
*↓2016/08/25
     CLOSE    NFSHOMS1  NFTENMS1  NFTEPTL1.
*↑2016/08/25
     STOP     RUN.
*----<< 伝票データ >>--*
 SHTDENLB-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENLB.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY3812K SHTDENLB ERROR " SHTDENLB-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    SAKUBAL1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   NFJOHOF   LNKSAIF.
*↓2016/08/25
     CLOSE    NFSHOMS1  NFTENMS1  NFTEPTL1.
*↑2016/08/25
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY3812K HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    SAKUBAL1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   NFJOHOF   LNKSAIF.
*↓2016/08/25
     CLOSE    NFSHOMS1  NFTENMS1  NFTEPTL1.
*↑2016/08/25
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 SAKUBAL1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SAKUBAL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY3812K SAKUBAL1 ERROR " SAKUBAL1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    SAKUBAL1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   NFJOHOF   LNKSAIF.
*↓2016/08/25
     CLOSE    NFSHOMS1  NFTENMS1  NFTEPTL1.
*↑2016/08/25
     STOP     RUN.
*----<< 商品変換テーブル >>--*
 SHOTBL1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHOTBL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY3812K SHOTBL1 ERROR " SHOTBL1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    SAKUBAL1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   NFJOHOF   LNKSAIF.
*↓2016/08/25
     CLOSE    NFSHOMS1  NFTENMS1  NFTEPTL1.
*↑2016/08/25
     STOP     RUN.
*----<< 商品名称マスタ >>--*
 MEIMS1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      MEIMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY3812K MEIMS1 ERROR " MEIMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    SAKUBAL1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   NFJOHOF   LNKSAIF.
*↓2016/08/25
     CLOSE    NFSHOMS1  NFTENMS1  NFTEPTL1.
*↑2016/08/25
     STOP     RUN.
*----<< 店舗マスタ >>--*
 TENMS1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TENMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY3812K TENMS1 ERROR " TENMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    SAKUBAL1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   NFJOHOF   LNKSAIF.
*↓2016/08/25
     CLOSE    NFSHOMS1  NFTENMS1  NFTEPTL1.
*↑2016/08/25
     STOP     RUN.
*----<< 在庫マスタ >>--*
 ZAMZAIF-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZAMZAIF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY3812K ZAMZAIL1 ERROR " ZAI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    SAKUBAL1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   NFJOHOF   LNKSAIF.
*↓2016/08/25
     CLOSE    NFSHOMS1  NFTENMS1  NFTEPTL1.
*↑2016/08/25
     STOP     RUN.
*----<< ナフコ基本情報ファイル >>--*
 NFJOHOF-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFJOHOF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY3812K NFJOHOL2 ERROR " JOH-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    SAKUBAL1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   NFJOHOF   LNKSAIF.
*↓2016/08/25
     CLOSE    NFSHOMS1  NFTENMS1  NFTEPTL1.
*↑2016/08/25
     STOP     RUN.
*----<< ＯＮＬ状況再計算Ｆ >>-*
 LNKSAIF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      LNKSAIF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY0301I LNKSAIF ERROR " SAI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENLB  HTOKMS    SAKUBAL1    DSPFILE
              SHOTBL1   MEIMS1    TENMS1     SHTDENLA
              ZAMZAIF   NFJOHOF   LNKSAIF.
*↓2016/08/25
     CLOSE    NFSHOMS1  NFTENMS1  NFTEPTL1.
*↑2016/08/25
     STOP     RUN.
*
*↓2016/08/25
*----<< ナフコ商品マスタ >>-*
 NFSHOMS1-ERR            SECTION.
     USE AFTER  EXCEPTION PROCEDURE      NFSHOMS1.
     ACCEPT     SYS-DATE  FROM DATE.
     ACCEPT     SYS-TIME  FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY    "### SSY0301I NFSHOMS1 ERROR " SHO-ST " "
                SYS-YY "." SYS-MM "." SYS-DD " "
                SYS-HH ":" SYS-MN ":" SYS-SS " ###" UPON CONS.
     DISPLAY    SEC-NAME                            UPON CONS.
     CLOSE      SHTDENLB  HTOKMS    SAKUBAL1    DSPFILE
                SHOTBL1   MEIMS1    TENMS1      SHTDENLA
                ZAMZAIF   NFJOHOF   LNKSAIF
                NFSHOMS1  NFTENMS1  NFTEPTL1.
     STOP       RUN.
*----<< ナフコ店舗マスタ >>-*
 NFTENMS1-ERR            SECTION.
     USE AFTER  EXCEPTION PROCEDURE      NFTENMS1.
     ACCEPT     SYS-DATE  FROM DATE.
     ACCEPT     SYS-TIME  FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY    "### SSY0301I NFTENMS1 ERROR " NFT-ST " "
                SYS-YY "." SYS-MM "." SYS-DD " "
                SYS-HH ":" SYS-MN ":" SYS-SS " ###" UPON CONS.
     DISPLAY    SEC-NAME                            UPON CONS.
     CLOSE      SHTDENLB  HTOKMS    SAKUBAL1    DSPFILE
                SHOTBL1   MEIMS1    TENMS1      SHTDENLA
                ZAMZAIF   NFJOHOF   LNKSAIF
                NFSHOMS1  NFTENMS1  NFTEPTL1.
     STOP       RUN.
*----<< ナフコ店舗パターンマスタ >>-*
 NFTEPTL1-ERR            SECTION.
     USE AFTER  EXCEPTION PROCEDURE      NFTEPTL1.
     ACCEPT     SYS-DATE  FROM DATE.
     ACCEPT     SYS-TIME  FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY    "### SSY0301I NFTEPTL1 ERROR " TEP-ST " "
                SYS-YY "." SYS-MM "." SYS-DD " "
                SYS-HH ":" SYS-MN ":" SYS-SS " ###" UPON CONS.
     DISPLAY    SEC-NAME                            UPON CONS.
     CLOSE      SHTDENLB  HTOKMS    SAKUBAL1    DSPFILE
                SHOTBL1   MEIMS1    TENMS1      SHTDENLA
                ZAMZAIF   NFJOHOF   LNKSAIF
                NFSHOMS1  NFTENMS1  NFTEPTL1.
     STOP       RUN.
*↑2016/08/25
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
     DISPLAY  "*** SSY3812K START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     SHTDENLB.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     SAKUBAL1.
     OPEN     INPUT     SHOTBL1.
     OPEN     INPUT     MEIMS1.
     OPEN     INPUT     TENMS1.
     OPEN     I-O       SHTDENLA.
     OPEN     I-O       ZAMZAIF.
     OPEN     I-O       LNKSAIF.
     OPEN     I-O       NFJOHOF.
*↓2016/08/25
     OPEN     INPUT     NFSHOMS1.
     OPEN     INPUT     NFTENMS1.
     OPEN     INPUT     NFTEPTL1.
*↑2016/08/25
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
     CLOSE    SAKUBAL1.
     CLOSE    SHOTBL1.
     CLOSE    MEIMS1.
     CLOSE    TENMS1.
     CLOSE    SHTDENLA.
     CLOSE    NFJOHOF.
     CLOSE    LNKSAIF.
*↓2016/08/25
     CLOSE    NFSHOMS1.
     CLOSE    NFTENMS1.
     CLOSE    NFTEPTL1.
*↑2016/08/25
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY3812K END   *** "
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
     MOVE     SPACE          TO   FSY38121.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-BODY1-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SSY3812K"      TO   PGID.
     MOVE    "FSY38121"      TO   FORM.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSY38121"     TO   DSP-FMT.
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
************************訂正数計を表示しておく2015/06/15
                        MOVE WRK-TSUKEI TO  JTUNOU
                        MOVE      2    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ヘッド部チェック                            *
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
*    倉庫コードチェック
*****IF     ( SOKO      IS   NUMERIC   )   AND
*****       ( SOKO      NOT =     ZERO )
     IF       SOKO      NOT =     SPACE
              MOVE      SOKO      TO   SOK-F01
              READ      SAKUBAL1
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
*↓2016/08/25
*    商品分類コード取得・表示
     MOVE     SHOCD     TO   SHO-F02
     READ     NFSHOMS1
              INVALID
                        MOVE "????????"  TO  SYOBUN
*↓TEST
*             DISPLAY   "SYOBUN????????"  UPON CONS
*↑TEST
                        IF   ERR-FLG     =   ZERO
                             MOVE   25   TO  ERR-FLG
*↓TEST
*             DISPLAY   "25 --> ERR-FLG"  UPON CONS
*↑TEST
                        END-IF
                        MOVE   "R"  TO   EDIT-OPTION OF SYOBUN
              NOT INVALID
                        MOVE      SHO-F45   TO   SYOBUN
     END-READ.
*↑2016/08/25
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
*    売上伝票データ存在チェック
     IF  ERR-FLG        =    ZERO
         PERFORM   220-SONZAI-CHECK
         IF   ERR-FLG   =    ZERO
              MOVE      ZERO      TO   IX
*             対象データ退避処理
*2015/08/14↓
              MOVE     " "        TO   DEN-HIT-FLG
              MOVE     " "        TO   JOH-HIT-FLG
*2015/08/14↑
              PERFORM   220-TAIHI-SEC
***
*2015/08/14↓
*             IF   CNT-MEISAI     =    ZERO
              IF   DEN-HIT-FLG    =    " "
*2015/08/14↑
                   MOVE      18   TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF NOUHIN
                   MOVE     "R"   TO   EDIT-OPTION OF NOUHIN
*↓2016/08/25
                   MOVE     "R"   TO   EDIT-OPTION OF PATAN
                   MOVE     "R"   TO   EDIT-OPTION OF AREACD
*↑2016/08/25
                   GO   TO   220-GRP01-CHECK-EXIT
              END-IF
*2015/08/14↓
              IF   JOH-HIT-FLG    =    " "
                   MOVE      24   TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JTIME
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
                   MOVE     "R"   TO   EDIT-OPTION OF SOKO
                   MOVE     "R"   TO   EDIT-OPTION OF SHOCD
                   MOVE     "R"   TO   EDIT-OPTION OF NOUHIN
                   GO   TO   220-GRP01-CHECK-EXIT
              END-IF
*2015/08/14↑
***
              IF        CNT-MEISAI     <=   10
                   MOVE      1         TO   CNT-MAXPAGE
              ELSE
                   DIVIDE    CNT-MEISAI     BY   10
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
     COMPUTE  IY   =    CNT-PAGE  *    10   -    10.
     PERFORM  VARYING   IZ   FROM  1   BY   1    UNTIL IZ > 10
         COMPUTE   IX   =    IY   +    IZ
**       DISPLAY "WRK-TENPO = " WRK-TENPO(IX) UPON CONS
**       DISPLAY "WRK-DENNO = " WRK-DENNO(IX) UPON CONS
**       DISPLAY "IX        = " IX            UPON CONS
**       DISPLAY "IZ        = " IZ            UPON CONS
         IF ( WRK-TENPO(IX)  NOT =     ZERO ) AND
            ( WRK-DENNO(IX)  NOT =     ZERO )
**       DISPLAY "AAAAAAAAAAAA" UPON CONS
              MOVE      SPACE          TO   MAS001(IZ)
              MOVE      WRK-TENPO(IX)  TO   TENPO(IZ)
              MOVE      TORICD         TO   TEN-F52
              MOVE      WRK-TENPO(IX)  TO   TEN-F011
              READ      TENMS1
                   INVALID   KEY
                        MOVE      SPACE     TO   TENPNM(IZ)
                   NOT INVALID
                        MOVE      TEN-F03   TO   TENPNM(IZ)
              END-READ
*↓2016/08/25
              MOVE WRK-MPTN (IX)       TO   MPTN (IZ)
              MOVE WRK-MAREA(IX)       TO   MAREA(IZ)
*↑2016/08/25
              MOVE WRK-HSUU (IX)       TO   HSUU (IZ)
              MOVE WRK-TSUU (IX)       TO   TEISUU(IZ)
              MOVE WRK-DENNO(IX)       TO   DENNO (IZ)
              MOVE WRK-NOHIN(IX)       TO   NOHIN (IZ)
              MOVE WRK-SAKUBA(IX)      TO   CHGMSB(IZ)
         ELSE
*********DISPLAY "BBBBBBBBBBBB" UPON CONS
              MOVE      SPACE          TO   MAS001(IZ)
              MOVE      "X"  TO   EDIT-STATUS OF TEISUU(IZ)
              MOVE      "X"  TO   EDIT-STATUS OF CHGMSB(IZ)
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
*
     IF     ( JDATE     =    DEN-F46 ) AND
            ( JTIME     =    DEN-F47 ) AND
            ( TORICD    =    DEN-F01 ) AND
            ( SOKO      =    DEN-F48 ) AND
            ( SHOCD     =    DEN-F25 )
              IF   WK-NOUHIN      NOT =     ZERO
                   IF   WK-NOUHIN      =    DEN-F112
*2015/08/14↓
                        MOVE "1"      TO    DEN-HIT-FLG
*2015/08/14↑
                        CONTINUE
                   ELSE
                        GO   TO   220-TAIHI-010
                   END-IF
              END-IF
*↓2016/08/25
*           ナフコ店舗パターンマスタチェック
              MOVE DEN-F07    TO    TEP-F01
              MOVE SHO-F45    TO    TEP-F02
              PERFORM               NFTEPTL1-READ-SEC
*             非存在時
              IF   NFTEPTL1-INV-FLG =  "INV"
*          　　　　パターン指定時→表示非対象
                   IF   PATAN   NOT =  SPACE
                        GO             TO     220-TAIHI-010
                   ELSE
*          　　　　パターン無指定時→表示対象
*          　　　　　　　　　　　　　ただし明細部には"??"表示
                        MOVE  "??"     TO     TEP-F03
                   END-IF
              END-IF
*             存在＆指定パターンと不一致→表示非対象
              IF   PATAN        NOT =  SPACE
                   IF   TEP-F03 NOT =  PATAN
                        GO      TO     220-TAIHI-010
                   END-IF
              END-IF
*
*           ナフコ店舗マスタチェック
              MOVE TORICD     TO    NFT-F01
              MOVE DEN-F07    TO    NFT-F02
              PERFORM               NFTENMS1-READ-SEC
*             非存在時
              IF   NFTENMS1-INV-FLG = "INV"
*          　　　　エリア指定時→表示非対象
                   IF AREACD    NOT =  SPACE
                      GO        TO     220-TAIHI-010
                   ELSE
*          　　　　エリア無指定時→表示対象
*          　　　　　　　　　　　　ただし明細部には"?"表示
                      MOVE  "?"     TO  NFT-F13
                   END-IF
              END-IF
*             存在＆指定エリアと不一致→表示非対象
              IF   AREACD     NOT = SPACE
                   IF NFT-F13 NOT = AREACD
                      GO      TO    220-TAIHI-010
                   END-IF
              END-IF
*↑2016/08/25
*
*2015/08/14↓
              MOVE "1"        TO    DEN-HIT-FLG
*2015/08/14↑
*
**************　　　　　　　　　　　↓発注確定済ＦＬＧ
**************基本情報ファイル索引　計上ＦＬＧチェック　開始
              MOVE DEN-F46   TO   JOH-F02
              MOVE DEN-F47   TO   JOH-F03
              MOVE DEN-F01   TO   JOH-F04
              MOVE DEN-F48   TO   JOH-F05
              MOVE DEN-F07   TO   JOH-F06
              MOVE DEN-F02   TO   JOH-F07
              MOVE DEN-F03   TO   JOH-F08
              MOVE DEN-F112  TO   JOH-F09
              PERFORM NFJOHOF-READ-SEC
              IF  NFJOHOF-INV-FLG = "INV"
                  GO         TO   220-TAIHI-010
              ELSE
******************↓発注確定済
******************計上済は対象としない
                  IF  JOH-F34  =  "1"
                      GO     TO   220-TAIHI-010
*2015/08/14↓
                  ELSE
                      MOVE   "1"  TO   JOH-HIT-FLG
                  END-IF
*2015/08/14↑
              END-IF
**************　　　　　　　　　　　↓発注確定済ＦＬＧ
**************基本情報ファイル索引　計上ＦＬＧチェック　終了
              ADD       1         TO   IX
              MOVE      DEN-F07   TO   WRK-TENPO  (IX)
*↓2016/08/25
              MOVE      TEP-F03   TO   WRK-MPTN   (IX)
              MOVE      NFT-F13   TO   WRK-MAREA  (IX)
*↑2016/08/25
              MOVE      DEN-F50   TO   WRK-HSUU   (IX)
              MOVE      DEN-F15   TO   WRK-TSUU   (IX)
                                       WRK-MAESUU (IX)
              MOVE      DEN-F02   TO   WRK-DENNO  (IX)
              MOVE      DEN-F03   TO   WRK-GYONO  (IX)
              MOVE      DEN-F112  TO   WRK-NOHIN  (IX)
              MOVE      DEN-F48   TO   WRK-SAKUBA (IX)
                                       WRK-MAE-SAKUBA (IX)
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
*    LEVEL  3      一括変更部　入力　                          *
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
                                       UNTIL    IZ  >  10
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
              MOVE      SPACE     TO   SAKUBA-CHG-FLG
         WHEN OTHER
              MOVE           1    TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      一括変更部　チェック                        *
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
*変更作場ＣＤ入力チェック
     IF  CHGSKB    NOT =   SPACE
         INITIALIZE               SOK-REC
         MOVE      CHGSKB    TO   SOK-F01
         PERFORM SAKUBAL1-READ-SEC
         IF  SAKUBAL1-INV-FLG = "INV"
             IF   ERR-FLG   =    ZERO
                  MOVE      22   TO   ERR-FLG
             END-IF
             MOVE     "C"   TO   EDIT-CURSOR OF CHGSKB
             MOVE     "R"   TO   EDIT-OPTION OF CHGSKB
         ELSE
             MOVE  SOK-F02  TO   CHGSKN
         END-IF
     END-IF.
*
 220-GRP02-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      明細（訂正数・作場）入力                    *
*--------------------------------------------------------------*
 220-INP-GRP03          SECTION.
     MOVE     "220-INP-GRP03"     TO   S-NAME.
     MOVE      ZERO      TO   WRK-TSUKEI
     PERFORM   VARYING   IX   FROM  1  BY  1
                         UNTIL     IX   >  CNT-MEISAI
*      実納品数ゼロ時，訂正後発注数をオールゼロ表示
         IF    JTUNOU   =    ZERO  AND
               ERR-FLG  =    ZERO
               MOVE      ZERO      TO   WRK-TSUU(IX)
         END-IF
*      一括変更作場ＣＤ指定時→全明細の作場を置換え
         IF    SAKUBA-CHG-FLG = SPACE
               IF  CHGSKB NOT = SPACE
                   MOVE   CHGSKB   TO   WRK-SAKUBA(IX)
               END-IF
         END-IF
*
*↓2016/08/25
*      一括変更数量指定時→全明細の訂正後発注数を置換え
         IF  ( TEISEI  IS     NUMERIC      )   AND
             ( TEISEI  NOT =  ZERO         )   AND
*##2016/09/15 NAV ST 発注までをOKとする。
*************( TEISEI     <   WRK-HSUU(IX) )
             ( TEISEI     <=  WRK-HSUU(IX) )
*##2016/09/15 NAV END
               MOVE    TEISEI      TO   WRK-TSUU(IX)
         END-IF
*↑2016/08/25
*
         ADD       WRK-TSUU(IX)  TO    WRK-TSUKEI
*
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
*    LEVEL  3      明細（訂正数・作場）チェック                *
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
     PERFORM  VARYING   IZ   FROM  1  BY  1  UNTIL  IZ  >  10
      IF TEISUU(IZ)     IS NUMERIC
         IF HSUU(IZ)       <    TEISUU(IZ)
            IF   ERR-FLG   =    ZERO
                 MOVE      14   TO   ERR-FLG
            END-IF
            MOVE     "C"   TO   EDIT-CURSOR OF TEISUU(IZ)
            MOVE     "R"   TO   EDIT-OPTION OF TEISUU(IZ)
         END-IF
******
         IF  CHGMSB(IZ)       =    SPACE
             IF   ERR-FLG     =    ZERO
                  MOVE   23   TO   ERR-FLG
             END-IF
             MOVE     "C"     TO   EDIT-CURSOR OF CHGMSB(IZ)
             MOVE     "R"     TO   EDIT-OPTION OF CHGMSB(IZ)
         ELSE
*#2016/09/13 NAV ST
*************MOVE  CHGMSB(IZ) TO   JOH-F01
             MOVE  CHGMSB(IZ) TO   SOK-F01
*#2016/09/13 NAV ED
             PERFORM SAKUBAL1-READ-SEC
             IF SAKUBAL1-INV-FLG = "INV"
                IF   ERR-FLG   =    ZERO
                     MOVE      22   TO   ERR-FLG
                END-IF
                MOVE  "C"  TO  EDIT-CURSOR OF CHGMSB(IZ)
                MOVE  "R"  TO  EDIT-OPTION OF CHGMSB(IZ)
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
     COMPUTE  IY   =    CNT-PAGE  *    10   -    10.
     PERFORM  VARYING   IZ   FROM  1  BY  1  UNTIL  IZ  >  10
         COMPUTE   IX   =    IY   +    IZ
              IF   TEISUU(IZ)     IS NUMERIC
                   MOVE      TEISUU(IZ)     TO   WRK-TSUU(IX)
                   MOVE      CHGMSB(IZ)     TO   WRK-SAKUBA(IX)
              ELSE
                   MOVE      "X"  TO   EDIT-STATUS OF TEISUU(IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF CHGMSB(IZ)
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
              MOVE      1         TO   SAKUBA-CHG-FLG
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
*    LEVEL  3      更新制御　　　                              *
*--------------------------------------------------------------*
 240-UPDATE-SEC           SECTION.
     MOVE     "240-UPDATE-SEC"    TO   S-NAME.
*
*****更新バッチ_を表示する
     DISPLAY NC"＃更新バッチ_＝" JDATE "-" JTIME "-" TORICD
             NC"　＃"  UPON CONS.
*****連携状況再更新ファイル存在チェック
     MOVE     JDATE     TO   SAI-F01.
     MOVE     JTIME     TO   SAI-F02.
     MOVE     TORICD    TO   SAI-F03.
     PERFORM  LNKSAIF-READ-SEC.
     IF  LNKSAIF-INV-FLG = "INV"
*********レコード出力を行なう。
         MOVE SPACE     TO   SAI-REC
         INITIALIZE          SAI-REC
         MOVE JDATE     TO   SAI-F01
         MOVE JTIME     TO   SAI-F02
         MOVE TORICD    TO   SAI-F03
         WRITE SAI-REC
     END-IF.
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
*振分倉庫更新
     PERFORM  VARYING   IX   FROM      1    BY   1
                             UNTIL     IX   >    CNT-MEISAI
              PERFORM   KOUSIN3-SEC
     END-PERFORM.
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
*## 2015/10/07 NAV ST 店舗ＣＤチェックも追加
     MOVE     40             TO   DE2-F051.
     MOVE     WRK-TENPO(IX)  TO   DE2-F07.
*## 2015/10/07 NAV ED 店舗ＣＤチェックも追加
     START    SHTDENLA  KEY  >=   DE2-F46   DE2-F47
                                  DE2-F01   DE2-F48
                                  DE2-F02   DE2-F04
                                  DE2-F051  DE2-F07
                                  DE2-F112  DE2-F03
         INVALID   KEY
              DISPLAY  "SSY3812K  SHTDENLA START INV 1"
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
*## 2015/10/07 NAV ST 店舗ＣＤチェックも追加
                 ( WRK-TENPO(IX)  =    DE2-F07 ) AND
*## 2015/10/07 NAV ED 店舗ＣＤチェックも追加
                 ( 9          NOT =    DE2-F277 )
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
*実発注数量と訂正後発注数量比較
                   IF   DE2-F15  NOT =  DE2-F50
                        MOVE     1               TO   WRK-ERR(IX)
                   END-IF
*物流連携フラグ、物流連携日をクリアする。
                   MOVE   SPACE   TO  DE2-F68  DE2-F69
                   REWRITE  DE2-REC
*↓TEST
**   DISPLAY  "KOUSIN1-SEC"  UPON CONS
**   DISPLAY  " REWRITE DE2-REC OK"  UPON CONS
**   DISPLAY  NC"　受信日　　＝"   DE2-F46  UPON CONS
**   DISPLAY  NC"　受信時刻　＝"   DE2-F47  UPON CONS
**   DISPLAY  NC"　取引先　　＝"   DE2-F01  UPON CONS
**   DISPLAY  NC"　倉庫　　　＝"   DE2-F48  UPON CONS
**   DISPLAY  NC"　店舗　　　＝"   DE2-F07  UPON CONS
**   DISPLAY  NC"　伝票番号　＝"   DE2-F02  UPON CONS
**   DISPLAY  NC"　行　　　　＝"   DE2-F03  UPON CONS
*↑TEST
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
*## 2015/10/07 NAV ST 店舗ＣＤチェックも追加
     MOVE     40             TO   DE2-F051.
     MOVE     WRK-TENPO(IX)  TO   DE2-F07.
*## 2015/10/07 NAV ED 店舗ＣＤチェックも追加
     START    SHTDENLA  KEY  >=   DE2-F46   DE2-F47
                                  DE2-F01   DE2-F48
                                  DE2-F02   DE2-F04
                                  DE2-F051  DE2-F07
                                  DE2-F112  DE2-F03
         INVALID   KEY
              DISPLAY  "SSY3812K  SHTDENLA START INV 2"
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
*## 2015/10/07 NAV ST 店舗ＣＤチェックも追加
                 ( WRK-TENPO(IX)  =    DE2-F07 ) AND
*## 2015/10/07 NAV ED 店舗ＣＤチェックも追加
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
*## 2013/11/27 NAV ST ＬＩＮ3Ｓ連携対応追加
*物流連携フラグ、物流連携日をクリアする。
                   MOVE   SPACE   TO  DE2-F68  DE2-F69
                   REWRITE  DE2-REC
*↓TEST
**   DISPLAY  "KOUSIN2-SEC"  UPON CONS
**   DISPLAY  " REWRITE DE2-REC OK"  UPON CONS
**   DISPLAY  NC"　受信日　　＝"   DE2-F46  UPON CONS
**   DISPLAY  NC"　受信時刻　＝"   DE2-F47  UPON CONS
**   DISPLAY  NC"　取引先　　＝"   DE2-F01  UPON CONS
**   DISPLAY  NC"　倉庫　　　＝"   DE2-F48  UPON CONS
**   DISPLAY  NC"　店舗　　　＝"   DE2-F07  UPON CONS
**   DISPLAY  NC"　伝票番号　＝"   DE2-F02  UPON CONS
**   DISPLAY  NC"　行　　　　＝"   DE2-F03  UPON CONS
*↑TEST
                   GO   TO  KOUSIN2-010
              END-IF
     END-READ.
*
 KOUSIN2-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL         ｺｳｼﾝ ｼｮﾘ（振分倉庫ＣＤ更新）　　            *
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
*## 2015/10/07 NAV ST 店舗ＣＤチェックも追加
     MOVE     40             TO   DE2-F051.
     MOVE     WRK-TENPO(IX)  TO   DE2-F07.
*## 2015/10/07 NAV ED 店舗ＣＤチェックも追加
     START    SHTDENLA  KEY  >=   DE2-F46   DE2-F47
                                  DE2-F01   DE2-F48
                                  DE2-F02   DE2-F04
                                  DE2-F051  DE2-F07
                                  DE2-F112  DE2-F03
         INVALID   KEY
              DISPLAY  "SSY3812K  SHTDENLA START INV 3"
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
*## 2015/10/07 NAV ST 店舗ＣＤチェックも追加
                ( WRK-TENPO(IX)  =    DE2-F07 ) AND
*## 2015/10/07 NAV ED 店舗ＣＤチェックも追加
                ( 9          NOT =    DE2-F277 )
*               振分倉庫をセット
****************IF DE2-F48   NOT =    WRK-SAKUBA(IX)
****************IF DE2-F48   NOT =    WRK-MAE-SAKUBA(IX)
************    IF DE2-F48   =    WRK-SAKUBA(IX)
************    AND DE2-F15  =    WRK-TSUU (IX)
************        CONTINUE
************    ELSE
*振分倉庫コードを更新する。
                   MOVE  WRK-SAKUBA(IX)          TO   DE2-F48
                   REWRITE  DE2-REC
*↓TEST
**   DISPLAY  "KOUSIN3-SEC"  UPON CONS
**   DISPLAY  " REWRITE DE2-REC OK"  UPON CONS
**   DISPLAY  NC"　受信日　　＝"   DE2-F46  UPON CONS
**   DISPLAY  NC"　受信時刻　＝"   DE2-F47  UPON CONS
**   DISPLAY  NC"　取引先　　＝"   DE2-F01  UPON CONS
**   DISPLAY  NC"　倉庫　　　＝"   DE2-F48  UPON CONS
**   DISPLAY  NC"　店舗　　　＝"   DE2-F07  UPON CONS
**   DISPLAY  NC"　伝票番号　＝"   DE2-F02  UPON CONS
**   DISPLAY  NC"　行　　　　＝"   DE2-F03  UPON CONS
*↑TEST
*******************ナフコ基本情報ファイルを更新する。
                   MOVE  JDATE         TO  JOH-F02
                   MOVE  JTIME         TO  JOH-F03
                   MOVE  TORICD        TO  JOH-F04
                   MOVE  SOKO          TO  JOH-F05
                   MOVE  WRK-MAE-SAKUBA(IX)  TO JOH-F05
                   MOVE  WRK-TENPO(IX) TO  JOH-F06
                   MOVE  WRK-DENNO(IX) TO  JOH-F07
                   MOVE  WRK-GYONO(IX) TO  JOH-F08
                   MOVE  WRK-NOHIN(IX) TO  JOH-F09
******DISPLAY "JDATE          = " JDATE          UPON CONS
*     DISPLAY "JTIME          = " JTIME          UPON CONS
*     DISPLAY "TORICD         = " TORICD         UPON CONS
*     DISPLAY "WRK-MAE-SAKUBA = " WRK-MAE-SAKUBA(IX) UPON CONS
*     DISPLAY "WRK-TENPO(IX)  = " WRK-TENPO(IX)  UPON CONS
*     DISPLAY "WRK-DENNO(IX)  = " WRK-DENNO(IX)  UPON CONS
*     DISPLAY "WRK-GYONO(IX)  = " WRK-GYONO(IX)  UPON CONS
******DISPLAY "WRK-NOHIN(IX)  = " WRK-NOHIN(IX)  UPON CONS
                   PERFORM NFJOHOF-READ-SEC
****  DISPLAY "NFJOHOF-INV-FLG = " NFJOHOF-INV-FLG UPON CONS
                   IF  NFJOHOF-INV-FLG = SPACE
                       MOVE WRK-TSUU(IX)   TO  JOH-F20
                       MOVE WRK-SAKUBA(IX) TO  JOH-F05
                       REWRITE  JOH-REC
                   ELSE
                       DISPLAY "## NFJOHOF READ ERR ##" UPON CONS
                       DISPLAY "## JOH-F02 = " JOH-F02  UPON CONS
                       DISPLAY "## JOH-F03 = " JOH-F03  UPON CONS
                       DISPLAY "## JOH-F04 = " JOH-F04  UPON CONS
                       DISPLAY "## JOH-F05 = " JOH-F05  UPON CONS
                       DISPLAY "## JOH-F06 = " JOH-F06  UPON CONS
                       DISPLAY "## JOH-F07 = " JOH-F07  UPON CONS
                       DISPLAY "## JOH-F08 = " JOH-F08  UPON CONS
                       DISPLAY "## JOH-F09 = " JOH-F09  UPON CONS
                       STOP  RUN
                   END-IF
**************  END-IF
             END-IF
     END-READ.
*
 KOUSIN3-SEC-EXIT.
     EXIT.
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
*************DISPLAY "AAA20150616" UPON CONS
             PERFORM   ZAIKO-UPDATE1-SEC
             NOT  INVALID
*************DISPLAY "BBB20150616" UPON CONS
*            DISPLAY "ZAI-F01  =  " ZAI-F01  UPON CONS
*            DISPLAY "ZAI-F021 =  " ZAI-F021 UPON CONS
*            DISPLAY "ZAI-F022 =  " ZAI-F022 UPON CONS
*************DISPLAY "ZAI-F03  =  " ZAI-F03  UPON CONS
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
           IF   CNT-MEISAI     <=   10
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
*TEST
*    IF  WK-GRP = "GRP003"
*             DISPLAY "TEISUU(1)=" TEISUU(1) UPON CONS
*    END-IF.
*TEST
     MOVE     SPACE          TO   DSP-PRO.
 900-DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
     MOVE     "900-DSP-WRITE"     TO   S-NAME.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FSY38121.
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
*↓2016/08/25 パターン・エリア
     MOVE     " "            TO   EDIT-CURSOR OF PATAN
                                  EDIT-CURSOR OF AREACD.
*↑2016/08/25
*
     MOVE     "M"            TO   EDIT-OPTION OF JDATE
                                  EDIT-OPTION OF JTIME
                                  EDIT-OPTION OF TORICD
                                  EDIT-OPTION OF SOKO
                                  EDIT-OPTION OF SHOCD
                                  EDIT-OPTION OF NOUHIN.
*↓2016/08/25 パターン・エリア
     MOVE     "M"            TO   EDIT-OPTION OF PATAN
                                  EDIT-OPTION OF AREACD
                                  EDIT-OPTION OF SYOBUN.
*↑2016/08/25
*
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      一括変更部・明細部クリア　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY1-RTN          SECTION.
*
     MOVE     " "            TO   EDIT-CURSOR OF JTUNOU.
     MOVE     "M"            TO   EDIT-OPTION OF JTUNOU.
****10/01/20  作場追加開始　大野
     MOVE     " "            TO   EDIT-CURSOR OF CHGSKB.
     MOVE     "M"            TO   EDIT-OPTION OF CHGSKB.
****10/01/20  作場追加終了　大野
*
*↓2016/08/25 一括変更数量
     MOVE     "M"            TO   EDIT-OPTION OF TEISEI.
*↑2016/08/25
*
     PERFORM  VARYING   IX   FROM  1   BY   1    UNTIL  IX > 10
         MOVE     " "        TO   EDIT-CURSOR OF TEISUU(IX)
         MOVE     "M"        TO   EDIT-OPTION OF TEISUU(IX)
         MOVE     " "        TO   EDIT-STATUS OF TEISUU(IX)
         MOVE     "M"        TO   EDIT-OPTION OF CHGMSB(IX)
         MOVE     " "        TO   EDIT-STATUS OF CHGMSB(IX)
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
     PERFORM  VARYING   IZ   FROM  1  BY  1  UNTIL  IZ  >  10
      IF TEISUU(IZ)     IS NUMERIC
         IF HSUU(IZ)       <    TEISUU(IZ)
            IF   ERR-FLG   =    ZERO
                 MOVE      14   TO   ERR-FLG
            END-IF
            MOVE     "C"   TO   EDIT-CURSOR OF TEISUU(IZ)
            MOVE     "R"   TO   EDIT-OPTION OF TEISUU(IZ)
         END-IF
      END-IF
     END-PERFORM.
*
 MEISAI-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    作場マスタ　読込
*--------------------------------------------------------------*
 SAKUBAL1-READ-SEC       SECTION.
*
     READ  SAKUBAL1
           INVALID
           MOVE  "INV"  TO   SAKUBAL1-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   SAKUBAL1-INV-FLG
     END-READ.
*
 SAKUBAL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ナフコ基本情報ファイル　読込
*--------------------------------------------------------------*
 NFJOHOF-READ-SEC        SECTION.
*
     READ  NFJOHOF
           INVALID
           MOVE  "INV"  TO   NFJOHOF-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   NFJOHOF-INV-FLG
     END-READ.
*
 NFJOHOF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                ＯＮＬ状況再計算Ｆ読込                        *
*--------------------------------------------------------------*
 LNKSAIF-READ-SEC          SECTION.
*
     MOVE      "LNKSAIF-READ-SEC" TO    S-NAME.
*
     READ      LNKSAIF
               INVALID
               MOVE      "INV"    TO    LNKSAIF-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    LNKSAIF-INV-FLG
     END-READ.
*
 LNKSAIF-READ-EXIT.
     EXIT.
*↓2016/08/25
*--------------------------------------------------------------*
*    ナフコ店舗マスタ読込
*--------------------------------------------------------------*
 NFTENMS1-READ-SEC        SECTION.
*
     MOVE      "NFTENMS1-READ-SEC" TO   S-NAME.
*
     READ       NFTENMS1
        INVALID
                MOVE  "INV"        TO   NFTENMS1-INV-FLG
        NOT INVALID
                MOVE  SPACE        TO   NFTENMS1-INV-FLG
     END-READ.
*
 NFTENMS1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ナフコ店舗パターンマスタ読込
*--------------------------------------------------------------*
 NFTEPTL1-READ-SEC        SECTION.
*
     MOVE      "NFTEPTL1-READ-SEC" TO   S-NAME.
*
     READ  NFTEPTL1
        INVALID
                MOVE  "INV"        TO   NFTEPTL1-INV-FLG
        NOT INVALID
                MOVE  SPACE        TO   NFTEPTL1-INV-FLG
     END-READ.
*
 NFTEPTL1-READ-EXIT.
     EXIT.
*↑2016/08/25
*-----------------<< PROGRAM END >>----------------------------*
