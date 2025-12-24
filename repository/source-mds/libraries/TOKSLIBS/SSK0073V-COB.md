# SSK0073V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSK0073V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ケーヨー伝票レス　　　　　　　　　*
*    モジュール名　　　　：　売上データリストＣＳＶデータ作成　*
*    作成日／作成者　　　：　14/03/24  YOSHIDA/M               *
*    処理概要　　　　　　：　ケーヨー様売上・返品伝票について、*
*    　　　　　　　　　　　　指定された範囲のリストＣＳＶデータ*
*    　　　　　　　　　　　　を出力する。　　　　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSK0073V.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.         CONSOLE   IS        CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<売上抽出データ >>*********************************
     SELECT   KEIURWL1           ASSIGN    TO   DA-01-VI-KEIURWL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    URW-F07
                                                URW-F02
                                                URW-F051
                                                URW-F04
                                                URW-F112
                                                URW-F03
                                 STATUS         URW-STATUS.
*
****<<取引先マスタ　　　　　　 >>*******************************
     SELECT   TOKMS2            ASSIGN    TO   DA-01-VI-TOKMS2
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TOK-F01
                                 STATUS         TOK-STATUS.
*
****<<店舗マスタ　　　　　　 >>*********************************
     SELECT   TENMS1             ASSIGN    TO   DA-01-VI-TENMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TEN-F52  TEN-F011
                                 STATUS         TEN-STATUS.
*
****<<条件ファイル　　　　　 >>*********************************
     SELECT   JYOKEN1            ASSIGN    TO   DA-01-VI-JYOKEN1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    JYO-F01  JYO-F02
                                 STATUS         JYO-STATUS.
*
*****<<売上CSVデータ　　>>***********************************
     SELECT   KEICSVUR           ASSIGN    TO   KEICSVUR
                                 STATUS         CSV-STATUS.
*                                                              *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ケーヨー　売上抽出データ　　　　　　               *
*--------------------------------------------------------------*
 FD  KEIURWL1           LABEL RECORD   IS   STANDARD.
     COPY     KEIURWF   OF        XFDLIB
              JOINING   URW       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 取引先マスタ　　　　　　　　　
*--------------------------------------------------------------*
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     TOKMS2    OF        XFDLIB
              JOINING   TOK       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 店舗マスタ　　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 条件ファイル　　　　　　　　　                     *
*--------------------------------------------------------------*
 FD  JYOKEN1             LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN    OF        XFDLIB
              JOINING    JYO       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 返品データＣＳＶ　　　　　　　　                   *
*--------------------------------------------------------------*
 FD  KEICSVUR           BLOCK CONTAINS 1    RECORDS.
 01  CSV-REC.
     03  FILLER         PIC       X(500).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                PIC  X(03)  VALUE  SPACE.
 01  TOKMS2-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  TENMS1-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  JYOKEN1-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  RD-TOK-FLG             PIC  X(01)  VALUE  SPACE.
 01  RD-TEN-FLG             PIC  X(01)  VALUE  SPACE.
 01  RD-JYO-FLG             PIC  X(01)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  URW-STATUS             PIC  X(02).
 01  TEN-STATUS             PIC  X(02).
 01  JYO-STATUS             PIC  X(02).
 01  TOK-STATUS             PIC  X(02).
 01  CSV-STATUS             PIC  X(02).
*
 01  BRK-KEY.
     03  BRK-TORICD         PIC  9(08)  VALUE  ZERO.
     03  BRK-TENCD          PIC  9(05)  VALUE  ZERO.
     03  BRK-DENNO          PIC  9(09)  VALUE  ZERO.
     03  BRK-DNKNO          PIC  9(02)  VALUE  ZERO.
     03  BRK-NOHINBI        PIC  9(08)  VALUE  ZERO.
     03  BRK-TORIMEI        PIC  N(15).
     03  BRK-TENMEI         PIC  N(15).
     03  BRK-DNKNM          PIC  N(05).
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD             PIC  9(06)  VALUE  ZERO.
     03  SYS-DATEW          PIC  9(08)  VALUE  ZERO.
     03  SYS-DATE-R         REDEFINES SYS-DATEW.
         05  SYS-YY         PIC  9(04).
         05  SYS-MM         PIC  9(02).
         05  SYS-DD         PIC  9(02).
***** システム時刻ワーク
 01  SYSTEM-TIME.
     03  SYS-HH             PIC  9(02).
     03  SYS-MN             PIC  9(02).
     03  SYS-SS             PIC  9(02).
*
*--------------
 01  WK-DENKEI.
     03  WK-SURYO-DEN       PIC S9(09)V99.
     03  WK-GENKINGAKU-DEN  PIC S9(11).
     03  WK-BAIKINGAKU-DEN  PIC S9(11).
***** カウンタ
 01  READ-CNT               PIC  9(07)  VALUE  ZERO.
 01  OUTPUT-CNT             PIC  9(07)  VALUE  ZERO.
*タイトルエリア
*見出しエリア
 01  WK-HEAD1.
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(14)  VALUE
                            NC"＜ケーヨー売上データリスト＞".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"発行日：".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD01-YYYYMMDD      PIC  9(08).
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(05)  VALUE  NC"発行時刻：".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD01-HHMMSS        PIC  9(06).
 01  WK-HEAD2.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"発注日：".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-HFROM         PIC  9(08)  VALUE  ZERO.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(01)  VALUE  NC"～".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-HTO           PIC  9(08)  VALUE  ZERO.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"納品日：".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-NFROM         PIC  9(08)  VALUE  ZERO.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(01)  VALUE  NC"～".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-NTO           PIC  9(08)  VALUE  ZERO.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(03)  VALUE  NC"店舗：".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-TENCDF        PIC  9(05)  VALUE  ZERO.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(01)  VALUE  NC"～".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-TENCDT        PIC  9(05)  VALUE  ZERO.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(05)  VALUE  NC"伝票番号：".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-DENF          PIC  9(09)  VALUE  ZERO.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(01)  VALUE  NC"～".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-DENT          PIC  9(09)  VALUE  ZERO.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(05)  VALUE  NC"出荷場所：".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-SHUBF         PIC  X(02).
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(01)  VALUE  NC"～".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-SHUBT         PIC  X(02).
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(05)  VALUE  NC"伝票区分：".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-DENKF         PIC  X(02).
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(01)  VALUE  NC"～".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-DENKT         PIC  X(02).
 01  WK-HEAD3.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(05)  VALUE  NC"取引先ＣＤ".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"取引先名".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(02)  VALUE  NC"店舗".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(03)  VALUE  NC"店舗名".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"伝票番号".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(03)  VALUE  NC"行番号".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"相殺区分".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"伝票区分".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(03)  VALUE  NC"発注日".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(03)  VALUE  NC"納品日".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(03)  VALUE  NC"出荷日".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"出荷場所".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(06)  VALUE  NC"相手商品ＣＤ".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(05)  VALUE  NC"サカタ商品".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(03)  VALUE  NC"商品名".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(02)  VALUE  NC"数量".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"単価区分".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"原価単価".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"原価金額".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"売価単価".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"売価金額".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"明細備考".
     03  FILLER             PIC  X(01)  VALUE  X"29".

*明細エリア
 01  WK-MEISAI1.
     03  WK-DENMEISAI.
         05  CSV-A00        PIC  X(01).
         05  MD01-TORICD    PIC  9(08).
         05  CSV-A01        PIC  X(01).
         05  CSV-A01A       PIC  X(01).
         05  MD01-TORINM    PIC  N(15).
         05  CSV-A01B       PIC  X(01).
         05  CSV-A02        PIC  X(01).
         05  MD01-TENCD     PIC  9(05).
         05  CSV-A03        PIC  X(01).
         05  CSV-A03A       PIC  X(01).
         05  MD01-TENMEI    PIC  N(10).
         05  CSV-A03B       PIC  X(01).
         05  CSV-A04        PIC  X(01).
         05  MD01-DENNO     PIC  9(09).
     03  CSV-A05            PIC  X(01).
     03  MD01-GYONO         PIC  9(02).
     03  CSV-A06            PIC  X(01).
     03  MD01-SOSAIK        PIC  9(01).
     03  CSV-A07            PIC  X(01).
     03  MD01-DENK          PIC  X(02).
     03  MD01-DENKC         PIC  X(01).
     03  CSV-A07A           PIC  X(01).
     03  MD01-DENKNM        PIC  N(04).
     03  CSV-A07B           PIC  X(01).
     03  CSV-A08            PIC  X(01).
     03  MD01-HACHUBI       PIC  9(08).
     03  CSV-A09            PIC  X(01).
     03  MD01-NOHINBI       PIC  9(08).
     03  CSV-A10            PIC  X(01).
     03  MD01-SHUKABI       PIC  9(08).
     03  CSV-A11            PIC  X(01).
     03  MD01-SHUKABHO      PIC  X(02).
     03  CSV-A12            PIC  X(01).
     03  MD01-ASHOCD        PIC  X(13).
     03  CSV-A13            PIC  X(01).
     03  MD01-SHOCD         PIC  X(08).
     03  MD01-HINTAN        PIC  X(08).
     03  CSV-A14            PIC  X(01).
     03  MD01-SHONM1        PIC  X(15).
     03  MD01-SHONM2        PIC  X(15).
     03  CSV-A15            PIC  X(01).
     03  MD01-SURYO         PIC  --------9.99.
     03  CSV-A16            PIC  X(01).
     03  MD01-TANKAKBN      PIC  X(01).
     03  CSV-A17            PIC  X(01).
     03  MD01-GENKA         PIC  --------9.99.
     03  CSV-A18            PIC  X(01).
     03  MD01-GENKINGAKU    PIC  ----------9.
     03  CSV-A19            PIC  X(01).
     03  MD01-BAIKA         PIC  --------9.99.
     03  CSV-A20            PIC  X(01).
     03  MD01-BAIKINGAKU    PIC  ----------9.
     03  CSV-A21            PIC  X(01).
     03  MD01-MEISAIBK      PIC  X(10).
 01  WK-MEISAI2.
     03  CSV-DENNO          PIC  X(85).
     03  CSV-B01            PIC  X(01).
     03  CSV-B02            PIC  X(01).
     03  CSV-B03            PIC  X(01).
     03  CSV-B04            PIC  X(01).
     03  CSV-B05            PIC  X(01).
     03  CSV-B06            PIC  X(01).
     03  CSV-B07            PIC  X(01).
     03  CSV-B07A           PIC  X(01).
     03  GK01-DENMEIT1      PIC  N(04).
     03  CSV-B07B           PIC  X(01).
     03  GK01-DENMEI1       PIC  X(15).
     03  GK01-DENMEI2       PIC  X(15).
     03  CSV-B08A           PIC  X(01).
     03  GK01-DENMEIT2      PIC  N(01).
     03  CSV-B08B           PIC  X(01).
     03  CSV-B09            PIC  X(01).
     03  CSV-B10            PIC  X(01).
     03  CSV-B11            PIC  X(01).
     03  CSV-B11A           PIC  X(01).
     03  GK01-TAITOL        PIC  N(04).
     03  CSV-B11B           PIC  X(01).
     03  CSV-B12            PIC  X(01).
     03  GK01-SURYO         PIC  --------9.99.
     03  CSV-B13            PIC  X(01).
     03  CSV-B14            PIC  X(01).
     03  CSV-B15            PIC  X(01).
     03  GK01-GENKINGAKU    PIC  ----------9.
     03  CSV-B16            PIC  X(01).
     03  CSV-B17            PIC  X(01).
     03  GK01-BAIKINGAKU    PIC  ----------9.
     03  CSV-B18            PIC  X(01).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSK0073V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0073V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER         PIC   X(04)  VALUE
                       "### ".
         05  ERR-PG-ID      PIC   X(08)  VALUE
                       "SSK0073V".
         05  FILLER         PIC   X(10)  VALUE
                       " ABEND ###".
*
     03  MSG-ABEND2.
         05  FILLER         PIC   X(04)  VALUE
                       "### ".
         05  ERR-FL-ID      PIC   X(08).
         05  FILLER         PIC   X(04)  VALUE
                       " ST-".
         05  ERR-STCD       PIC   X(02).
         05  FILLER         PIC   X(04)  VALUE
                       " ###".
*
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPG= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-SKBN              PIC   X(01).
 01  PARA-DKBN              PIC   X(01).
 01  PARA-DFROM             PIC   9(08).
 01  PARA-DTO               PIC   9(08).
 01  PARA-KKBN              PIC   X(01).
 01  PARA-TANFROM           PIC   X(02).
 01  PARA-TANTO             PIC   X(02).
 01  PARA-DENK1             PIC   X(02).
 01  PARA-DENK2             PIC   X(02).
 01  PARA-DENK3             PIC   X(02).
 01  PARA-DENK4             PIC   X(02).
 01  PARA-DENK5             PIC   X(02).
 01  PARA-TENFROM           PIC   9(05).
 01  PARA-TENTO             PIC   9(05).
 01  PARA-DENNFROM          PIC   9(09).
 01  PARA-DENNTO            PIC   9(09).
 01  PARA-SKBFROM           PIC   X(02).
 01  PARA-SKBTO             PIC   X(02).
 01  PARA-DENKFROM          PIC   X(02).
 01  PARA-DENKTO            PIC   X(02).
*
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-SKBN
                                           PARA-DKBN
                                           PARA-DFROM
                                           PARA-DTO
                                           PARA-KKBN
                                           PARA-TANFROM
                                           PARA-TANTO
                                           PARA-DENK1
                                           PARA-DENK2
                                           PARA-DENK3
                                           PARA-DENK4
                                           PARA-DENK5
                                           PARA-TENFROM
                                           PARA-TENTO
                                           PARA-DENNFROM
                                           PARA-DENNTO
                                           PARA-SKBFROM
                                           PARA-SKBTO
                                           PARA-DENKFROM
                                           PARA-DENKTO.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           KEIURWL1.
     MOVE     "KEIURWL1"          TO        ERR-FL-ID.
     MOVE     URW-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TENMS1.
     MOVE     "TENMS1  "          TO        ERR-FL-ID.
     MOVE     TEN-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE JYOKEN1.
     MOVE     "JYOKEN1 "          TO        ERR-FL-ID.
     MOVE     JYO-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC4         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TOKMS2.
     MOVE     "TOKMS2  "          TO        ERR-FL-ID.
     MOVE     TOK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC5         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE KEICSVUR.
     MOVE     "KEICSVUR"          TO        ERR-FL-ID.
     MOVE     CSV-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSK0073V-START         SECTION.
*
     MOVE   "SSK0073V-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     IF   END-FLG  NOT =  "END"
          PERFORM    MAIN-SEC  UNTIL  END-FLG   =  "END"
     END-IF.
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSK0073V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     KEIURWL1.
     OPEN     INPUT     TOKMS2.
     OPEN     INPUT     TENMS1.
     OPEN     INPUT     JYOKEN1.
     OPEN     OUTPUT    KEICSVUR.
     DISPLAY  MSG-START UPON CONS.
*
*
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
     ACCEPT    SYSTEM-TIME       FROM      TIME.
*
     MOVE  SPACE                TO   URW-REC.
     INITIALIZE                      URW-REC.
*
     PERFORM  KEIURWL1-RD-SEC.
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し＃" UPON CONS
           MOVE     4001          TO   PROGRAM-STATUS
           STOP     RUN
     END-IF.
     PERFORM   MIDASISET-SEC.
     MOVE     SPACE               TO   CSV-REC.
     MOVE     WK-HEAD1            TO   CSV-REC.
     WRITE    CSV-REC.
     MOVE     SPACE               TO   CSV-REC.
     MOVE     WK-HEAD2            TO   CSV-REC.
     WRITE    CSV-REC.
     MOVE     SPACE               TO   CSV-REC.
     MOVE     WK-HEAD3            TO   CSV-REC.
     WRITE    CSV-REC.
     ADD      3                   TO   OUTPUT-CNT.
*　ブレイクキー設定
     MOVE     URW-F07             TO   BRK-TENCD.
     MOVE     URW-F02             TO   BRK-DENNO.
     MOVE     URW-F051            TO   BRK-DNKNO.
     MOVE     URW-F112            TO   BRK-NOHINBI.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ケーヨー　売上データ読み込み
****************************************************************
 KEIURWL1-RD-SEC            SECTION.
*
     MOVE    "KEIURWL1-RD-SEC"    TO   S-NAME.
*
     READ     KEIURWL1
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    KEIURWL1-RD-EXIT
     END-READ.
*
     ADD   1      TO  READ-CNT.
*
 KEIURWL1-RD-EXIT.
     EXIT.
***************************************************************
*             取引先マスタ読込
***************************************************************
 TOKMS2-READ-SEC       SECTION.
*
     MOVE    "TOKMS2-READ-SEC" TO        S-NAME.
*
     READ     TOKMS2
              INVALID      MOVE  "INV"    TO   TOKMS2-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   TOKMS2-INV-FLG
     END-READ.
*
 TOKMS2-READ-EXIT.
     EXIT.
***************************************************************
*             店舗マスタ読込
***************************************************************
 TENMS1-READ-SEC        SECTION.
*
     MOVE    "TENMS1-READ-SEC"  TO        S-NAME.
*
     READ     TENMS1
              INVALID      MOVE  "INV"    TO   TENMS1-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   TENMS1-INV-FLG
     END-READ.
*
 TENMS1-READ-EXIT.
     EXIT.
***************************************************************
*             条件ファイル読込
***************************************************************
 JYOKEN1-READ-SEC       SECTION.
*
     MOVE    "JYOKEN1-READ-SEC" TO        S-NAME.
*
     READ     JYOKEN1
              INVALID      MOVE  "INV"    TO   JYOKEN1-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   JYOKEN1-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*初期化
     MOVE     SPACE               TO   CSV-REC.
     MOVE     SPACE               TO   WK-MEISAI1.
     INITIALIZE                        WK-MEISAI1.
*項目転送
     MOVE   X"28"                 TO   CSV-A01A  CSV-A03A
                                       CSV-A07A.
     MOVE   X"29"                 TO   CSV-A01B  CSV-A03B
                                       CSV-A07B.
*
     MOVE     ","                 TO   CSV-A00   CSV-A01
                                       CSV-A02   CSV-A03
                                       CSV-A04   CSV-A05
                                       CSV-A06   CSV-A07
                                       CSV-A08   CSV-A09
                                       CSV-A10   CSV-A11
                                       CSV-A12   CSV-A13
                                       CSV-A14   CSV-A15
                                       CSV-A16   CSV-A17
                                       CSV-A18   CSV-A19
                                       CSV-A20   CSV-A21.
*
*  店舗・伝票番号ブレイク時
     IF      ( URW-F07   NOT =   BRK-TENCD )  OR
             ( URW-F02   NOT =   BRK-DENNO )  OR
             ( URW-F051  NOT =   BRK-DNKNO )  OR
             ( URW-F112  NOT =   BRK-NOHINBI )
*  合計
              PERFORM   DENKEI-SEC
              MOVE   ZERO         TO    WK-DENKEI
              INITIALIZE                WK-DENKEI
              IF  ( URW-F07   NOT =   BRK-TENCD )
                  MOVE  SPACE         TO    RD-TEN-FLG
              END-IF
              MOVE  URW-F07       TO    BRK-TENCD
              MOVE  URW-F02       TO    BRK-DENNO
              MOVE  URW-F051      TO    BRK-DNKNO
              MOVE  URW-F112      TO    BRK-NOHINBI
              MOVE  SPACE         TO    RD-TOK-FLG
              MOVE  SPACE         TO    RD-JYO-FLG
     END-IF.
*  明細行編集１
*  取引先ＣＤ
     MOVE     URW-F01           TO    MD01-TORICD.
*  取引先正式名
     MOVE     URW-F01           TO    TOK-F01.
     IF  RD-TOK-FLG = SPACE
         PERFORM  TOKMS2-READ-SEC
         IF  TOKMS2-INV-FLG = "INV"
           MOVE ALL NC"＊"      TO    MD01-TORINM  BRK-TORIMEI
         ELSE
           MOVE TOK-F02         TO    MD01-TORINM  BRK-TORIMEI
         END-IF
         MOVE  "1"              TO    RD-TOK-FLG
     ELSE
         MOVE   BRK-TORIMEI     TO    MD01-TORINM
     END-IF.
*  店舗ＣＤ
     MOVE     URW-F07           TO    MD01-TENCD
*  店舗正式名
     MOVE     URW-F07           TO    TEN-F011
     MOVE     173               TO    TEN-F52
     IF  RD-TEN-FLG = SPACE
         PERFORM  TENMS1-READ-SEC
         IF  TENMS1-INV-FLG = "INV"
           MOVE ALL NC"＊"      TO    MD01-TENMEI  BRK-TENMEI
         ELSE
           MOVE TEN-F03         TO    MD01-TENMEI  BRK-TENMEI
         END-IF
         MOVE  "1"              TO    RD-TEN-FLG
     ELSE
         MOVE  BRK-TENMEI       TO    MD01-TENMEI
     END-IF.
*  伝票番号
     MOVE     URW-F02           TO    MD01-DENNO.
*  行番号
     MOVE     URW-F03           TO    MD01-GYONO.
*  相殺区分
     MOVE     URW-F04           TO    MD01-SOSAIK.
*  伝区
     MOVE     URW-F051          TO    MD01-DENK.
     MOVE     ":"               TO    MD01-DENKC.
*  伝区名称
     MOVE     1                 TO    JYO-F01.
     MOVE     URW-F051          TO    JYO-F02.
     IF  RD-JYO-FLG = SPACE
         PERFORM  JYOKEN1-READ-SEC
         IF  JYOKEN1-INV-FLG = "INV"
           MOVE ALL NC"＊"      TO    MD01-DENKNM  BRK-DNKNM
         ELSE
           MOVE JYO-F03         TO    MD01-DENKNM  BRK-DNKNM
         END-IF
         MOVE  "1"              TO    RD-JYO-FLG
     ELSE
         MOVE  BRK-DNKNM        TO    MD01-DENKNM
     END-IF.
*  発注日
     MOVE     URW-F111             TO        MD01-HACHUBI.
*  納品日
     MOVE     URW-F112             TO        MD01-NOHINBI.
*  出荷日
     MOVE     URW-F113             TO        MD01-SHUKABI.
*  出荷場所
     MOVE     URW-F08              TO        MD01-SHUKABHO.
*  相手先商品ＣＤ
     MOVE     URW-F25              TO        MD01-ASHOCD.
*  サカタ商品
     MOVE     URW-F1411            TO        MD01-SHOCD.
     MOVE     URW-F1412            TO        MD01-HINTAN.
*  商品名
     MOVE     URW-F1421            TO        MD01-SHONM1.
     MOVE     URW-F1422            TO        MD01-SHONM2.
*  数量　
     MOVE     URW-F15              TO        MD01-SURYO.
*  単価区分
     MOVE     URW-F16              TO        MD01-TANKAKBN.
*  原価単価
     MOVE     URW-F172             TO        MD01-GENKA.
*  原価金額
     MOVE     URW-F181             TO        MD01-GENKINGAKU.
*  売価単価
     MOVE     URW-F173             TO        MD01-BAIKA.
*  売価金額
     MOVE     URW-F182             TO        MD01-BAIKINGAKU.
*  明細備考
     MOVE     URW-F22              TO        MD01-MEISAIBK.
*  合計備考
     IF  ( URW-F03    =   80 )
         MOVE     URW-F1421           TO   GK01-DENMEI1
         MOVE     URW-F1422           TO   GK01-DENMEI2
         PERFORM  KEIURWL1-RD-SEC
         GO    TO     MAIN-EXIT
     ELSE
         MOVE     SPACE               TO   GK01-DENMEI1
         MOVE     SPACE               TO   GK01-DENMEI2
     END-IF.
*--------------
*  伝票計加算
*--------------
*  数量　
     COMPUTE  WK-SURYO-DEN       = WK-SURYO-DEN    +  URW-F15.
*  原価金額
     COMPUTE  WK-GENKINGAKU-DEN  = WK-GENKINGAKU-DEN  +  URW-F181.
*  売価金額
     COMPUTE  WK-BAIKINGAKU-DEN  = WK-BAIKINGAKU-DEN  +  URW-F182.
*レコードセット
     MOVE     WK-MEISAI1           TO        CSV-REC.
     MOVE     WK-DENMEISAI         TO        CSV-DENNO.
*
     WRITE    CSV-REC.
     ADD      1                    TO        OUTPUT-CNT.
*    次レコード読込み
     PERFORM  KEIURWL1-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*             見出し編集処理                1.2                *
****************************************************************
 MIDASISET-SEC             SECTION.
*
     MOVE    "MIDASISET-SEC"    TO   S-NAME.
*システム日付・時刻セット
     MOVE     SYS-DATE-R        TO   HD01-YYYYMMDD.
     MOVE     SYSTEM-TIME       TO   HD01-HHMMSS.
*日付区分・日付
     EVALUATE   PARA-DKBN
          WHEN   "4"
              MOVE     PARA-DFROM         TO   HD02-HFROM
              MOVE     PARA-DTO           TO   HD02-HTO
          WHEN   "3"
              MOVE     PARA-DFROM         TO   HD02-NFROM
              MOVE     PARA-DTO           TO   HD02-NTO
          WHEN   OTHER
              MOVE     ZERO               TO   HD02-HFROM
                                               HD02-NFROM
                                               HD02-HTO
                                               HD02-NTO
     END-EVALUATE.
*店舗
     MOVE     PARA-TENFROM      TO   HD02-TENCDF.
     MOVE     PARA-TENTO        TO   HD02-TENCDT.
*伝票番号
     MOVE     PARA-DENNFROM     TO   HD02-DENF.
     MOVE     PARA-DENNTO       TO   HD02-DENT.
*出荷場所
     MOVE     PARA-SKBFROM      TO   HD02-SHUBF.
     MOVE     PARA-SKBTO        TO   HD02-SHUBT.
*伝票区分
     MOVE     PARA-DENKFROM     TO   HD02-DENKF.
     MOVE     PARA-DENKTO       TO   HD02-DENKT.
*
 MIDASISET-EXIT.
     EXIT.
****************************************************************
*             伝票合計出力　                　　
****************************************************************
 DENKEI-SEC             SECTION.
*
     MOVE     "DENKEI-SEC"        TO   S-NAME.
*初期化
     MOVE     SPACE               TO   CSV-REC.
*項目転送
     MOVE     X"28"               TO   CSV-B07A  CSV-B08A
                                       CSV-B11A.
     MOVE     X"29"               TO   CSV-B07B  CSV-B08B
                                       CSV-B11B.
*
     MOVE     ","                 TO   CSV-B01   CSV-B02
                                       CSV-B03   CSV-B04
                                       CSV-B05   CSV-B06
                                       CSV-B07   CSV-B09
                                       CSV-B10   CSV-B11
                                       CSV-B12   CSV-B13
                                       CSV-B14   CSV-B15
                                       CSV-B16   CSV-B17
                                       CSV-B18.
*--------------
*  合計転送
*--------------
*  タイトル転送
     MOVE     NC"＜備考："        TO   GK01-DENMEIT1.
     MOVE     NC"＞"              TO   GK01-DENMEIT2.
     MOVE     NC"＜合計＞"        TO   GK01-TAITOL.
*  数量　
     MOVE     WK-SURYO-DEN        TO   GK01-SURYO.
*  原価金額
     MOVE     WK-GENKINGAKU-DEN   TO   GK01-GENKINGAKU.
*  売価金額
     MOVE     WK-BAIKINGAKU-DEN   TO   GK01-BAIKINGAKU.
*
*伝票合計出力
*レコードセット
     MOVE     WK-MEISAI2          TO   CSV-REC.
*
     WRITE    CSV-REC.
     ADD      1                   TO   OUTPUT-CNT.
*
 DENKEI-EXIT.
     EXIT.
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
* 伝票計出力
     IF  READ-CNT  > 0
         PERFORM   DENKEI-SEC
     END-IF.
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
*
     CLOSE    KEIURWL1 TOKMS2 TENMS1 JYOKEN1 KEICSVUR.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.

```
