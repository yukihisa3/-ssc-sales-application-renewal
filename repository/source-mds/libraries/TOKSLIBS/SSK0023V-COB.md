# SSK0023V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSK0023V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ケーヨー伝票レス　　　　　　　　　*
*    業務名　　　　　　　：　受領処理　　　　　　              *
*    モジュール名　　　　：　受領書ＣＳＶデータ出力　          *
*    作成日／更新日　　　：　14/03/12                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　ケーヨー受領書データをＣＳＶに　　*
*    　　　　　　　　　　　　出力する。　　　　　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSK0023V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/03/12.
*
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
*
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<受領抽出データ >>*********************************
     SELECT   KEIJYWL2           ASSIGN    TO   DA-01-VI-KEIJYWL2
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JYW-F04
                                                JYW-F05
                                                JYW-F06
                                 STATUS         JYW-STATUS.
*
****<<店舗マスタ　　　　　　 >>*********************************
     SELECT   TENMS1             ASSIGN    TO   DA-01-VI-TENMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TEN-F52  TEN-F011
                                 STATUS         TEN-STATUS.
*
****<<条件ファル　　　　　　 >>*********************************
     SELECT   JYOKEN1            ASSIGN    TO   DA-01-VI-JYOKEN1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    JYO-F01  JYO-F02
                                 STATUS         JYO-STATUS.
*
*****<<受領CSVデータ >>*************************************
     SELECT   KEICSVJR           ASSIGN    TO   KEICSVJR
                                STATUS         CSV-STATUS.
*                                                              *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ケーヨー　受領抽出データ　　　　　　               *
*--------------------------------------------------------------*
 FD  KEIJYWL2           LABEL RECORD   IS   STANDARD.
     COPY     KEIJYWF   OF        XFDLIB
              JOINING   JYW       PREFIX.
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
              JOINING   JYO       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ受領データＣＳＶ　　　　　                   *
*--------------------------------------------------------------*
 FD  KEICSVJR           BLOCK CONTAINS 1    RECORDS.
 01  CSV-REC.
     03  FILLER         PIC       X(500).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  END-FLG2                     PIC       X(03)  VALUE  SPACE.
 01  SET-FLG                      PIC       X(03)  VALUE  SPACE.
 01  TENMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  JYOKEN1-INV-FLG              PIC       X(03)  VALUE  SPACE.
 01  RD1-FLG                      PIC       X(01)  VALUE  SPACE.
 01  RD2-FLG                      PIC       X(01)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  JYW-STATUS                   PIC       X(02).
 01  TEN-STATUS                   PIC       X(02).
 01  JYO-STATUS                   PIC       X(02).
 01  CSV-STATUS                   PIC       X(02).
*
 01  BRK-KEY.
     03  BRK-TENCD                PIC      S9(05)  VALUE  ZERO.
     03  BRK-DENNO                PIC      S9(09)  VALUE  ZERO.
     03  BRK-TENMEI               PIC      N(15).
     03  BRK-DNKNM                PIC      N(05).
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC       9(06)  VALUE  ZERO.
     03  SYS-DATEW                PIC       9(08)  VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES SYS-DATEW.
         05  SYS-YY               PIC       9(04).
         05  SYS-MM               PIC       9(02).
         05  SYS-DD               PIC       9(02).
***** システム時刻ワーク
 01  SYSTEM-TIME.
     03  SYS-HH                   PIC  9(02).
     03  SYS-MN                   PIC  9(02).
     03  SYS-SS                   PIC  9(02).
*
*--------------
 01  WK-DENKEI.
     03  WK-SHUKASU-DEN           PIC      S9(06)V9.
     03  WK-KENSHUSU-DEN          PIC      S9(06)V9.
     03  WK-KINGAKU-DEN           PIC      S9(07).
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  READ2-CNT                    PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
 01  IX1                          PIC       9(04)  VALUE  ZERO.
*タイトルエリア
*見出しエリア
 01  WK-HEAD1.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(09)  VALUE  NC"＜ケーヨー受領書＞".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"発行日：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD01-YYYYMMDD PIC 9(08).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"発行時刻：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD01-HHMMSS   PIC 9(06).
 01  WK-HEAD3.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"発行区分：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD03-DKBN     PIC X(01).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  HD03-DKBNNM           PIC  N(03).
     03  FILLER        PIC X(01)  VALUE  X"29".
 01  WK-HEAD4.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"受信日：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD04-JFROM   PIC 9(08)  VALUE  ZERO.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(01)  VALUE  NC"～".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD04-JTO      PIC 9(08)  VALUE  ZERO.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"検収日：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD04-KFROM   PIC 9(08)  VALUE  ZERO.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(01)  VALUE  NC"～".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD04-KTO      PIC 9(08)  VALUE  ZERO.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"納品日：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD04-NFROM   PIC 9(08)  VALUE  ZERO.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(01)  VALUE  NC"～".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD04-NTO      PIC 9(08)  VALUE  ZERO.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"発注日：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD04-HFROM   PIC 9(08)  VALUE  ZERO.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(01)  VALUE  NC"～".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD04-HTO      PIC 9(08)  VALUE  ZERO.
 01  WK-HEAD5.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC  N(05)  VALUE  NC"伝票区分：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD05-DENK1    PIC X(02).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD05-DENK2    PIC X(02).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD05-DENK3    PIC X(02).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD05-DENK4    PIC X(02).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD05-DENK5    PIC X(02).
 01  WK-HEAD6.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(04)  VALUE  NC"店舗ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(03)  VALUE  NC"店舗名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(04)  VALUE  NC"伝票番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(04)  VALUE  NC"伝票区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(03)  VALUE  NC"発注日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(03)  VALUE  NC"納品日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(03)  VALUE  NC"検収日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(04)  VALUE  NC"出荷場所".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(04)  VALUE  NC"振分場所".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(04)  VALUE  NC"ルート".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(01)  VALUE  NC"行".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(05)  VALUE  NC"ＪＡＮＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(03)  VALUE  NC"商品名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER          PIC  N(05)  VALUE  NC"サカタ商品".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(03)  VALUE  NC"出荷数".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(03)  VALUE  NC"検収数".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(04)  VALUE  NC"原価単価".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(04)  VALUE  NC"原価金額".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(02)  VALUE  NC"差数".
     03  FILLER        PIC X(01)  VALUE  X"29".

*明細エリア
 01  WK-MEISAI1.
     03  WK-DENMEISAI.
         05  CSV-A00       PIC X(01).
         05  MD01-TENCD               PIC       9(05).
         05  CSV-A01       PIC X(01).
         05  CSV-A01A       PIC X(01).
         05  MD01-TENMEI              PIC       N(15).
         05  CSV-A01B       PIC X(01).
         05  CSV-A02       PIC X(01).
         05  MD02-DENNO               PIC       9(09).
         05  CSV-A03       PIC X(01).
         05  MD02-DENKU               PIC       X(02).
         05  MD02-DENKUA               PIC      X(01).
         05  CSV-A03A      PIC X(01).
         05  MD02-DNKNM               PIC       N(05).
         05  CSV-A03B      PIC X(01).
         05  CSV-A04       PIC X(01).
         05  MD02-HACHUBI             PIC       9(08).
         05  CSV-A05       PIC X(01).
         05  MD02-NOHINBI             PIC       9(08).
         05  CSV-A06       PIC X(01).
         05  MD02-KENSHUBI            PIC       9(08).
     03  CSV-A07       PIC X(01).
     03  MD02-SKBCD               PIC       X(02).
     03  CSV-A08       PIC X(01).
     03  MD02-FRBCD               PIC       X(02).
     03  CSV-A09       PIC X(01).
     03  MD02-ROUTE               PIC       X(08).
     03  CSV-A10       PIC X(01).
     03  MD03-GYO                 PIC       9(02).
     03  CSV-A11       PIC X(01).
     03  MD03-JANCD               PIC       X(13).
     03  CSV-A12       PIC X(01).
     03  MD03-SHONM1              PIC       X(15).
     03  MD03-SHONM2              PIC       X(15).
     03  CSV-A13       PIC X(01).
     03  MD03-SHOCD               PIC       X(08).
     03  MD03-HINTAN1             PIC       X(05).
     03  MD03-HINTAN2             PIC       X(02).
     03  MD03-HINTAN3             PIC       X(01).
     03  CSV-A14       PIC X(01).
     03  MD03-SHUKASU             PIC      -----9.9.
     03  CSV-A15       PIC X(01).
     03  MD03-KENSHUSU            PIC      -----9.9.
     03  CSV-A16       PIC X(01).
     03  MD03-GENKA               PIC      ------9.99.
     03  CSV-A17       PIC X(01).
     03  MD03-KINGAKU             PIC      --------9.
     03  CSV-A18       PIC X(01).
     03  MD03-SASU                PIC      -----9.9.
 01  WK-MEISAI2.
     03  CSV-DENNO     PIC X(92).
     03  CSV-B01       PIC X(01).
     03  CSV-B02       PIC X(01).
     03  CSV-B03       PIC X(01).
     03  CSV-B04       PIC X(01).
     03  CSV-B05       PIC X(01).
     03  CSV-B06       PIC X(01).
     03  CSV-B07       PIC X(01).
     03  CSV-B07A      PIC X(01).
     03  GK01-TAITOL              PIC     N(04).
     03  CSV-B07B      PIC X(01).
     03  CSV-B08       PIC X(01).
     03  GK01-SHUKASU             PIC    -----9.9.
     03  CSV-B09       PIC X(01).
     03  GK01-KENSHUSU            PIC    -----9.9.
     03  CSV-B10       PIC X(01).
     03  CSV-B11       PIC X(01).
     03  GK01-KINGAKU             PIC    --------9.
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSK0023V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0023V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSK0023V".
         05  FILLER               PIC       X(10)  VALUE
                       " ABEND ###".
*
     03  MSG-ABEND2.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-FL-ID            PIC       X(08).
         05  FILLER               PIC       X(04)  VALUE
                       " ST-".
         05  ERR-STCD             PIC       X(02).
         05  FILLER               PIC       X(04)  VALUE
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
                        PROCEDURE           KEIJYWL2.
     MOVE     "KEIJYWL2"          TO        ERR-FL-ID.
     MOVE     JYW-STATUS          TO        ERR-STCD.
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
                        PROCEDURE KEICSVJR.
     MOVE     "KEICSVJR"          TO        ERR-FL-ID.
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
 SSK0023V-START         SECTION.
*
     MOVE   "SSK0023V-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     IF   END-FLG  NOT =  "END"
          PERFORM    MAIN-SEC  UNTIL  END-FLG   =  "END"
     END-IF.
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSK0023V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     KEIJYWL2.
     OPEN     INPUT     TENMS1.
     OPEN     INPUT     JYOKEN1.
     OPEN     OUTPUT    KEICSVJR.
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
*    MOVE  SPACE                TO   JYW-REC.
*    INITIALIZE                      JYW-REC.
*
*    START  KEIJYWL2  KEY  >=   JYW-F04  JYW-F05  JYW-F06
*        INVALID   KEY
*           MOVE     "END"      TO   END-FLG
*           DISPLAY NC"＃対象データ無し１＃" UPON CONS
*           DISPLAY "JYW-F04=" JYW-F04       UPON CONS
*           DISPLAY "JYW-F05=" JYW-F05       UPON CONS
*           DISPLAY "JYW-F06=" JYW-F06       UPON CONS
*           GO                  TO   INIT-EXIT
*    END-START
*
     PERFORM  KEIJYWL2-RD-SEC.
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
     PERFORM   MIDASISET-SEC.
     MOVE     SPACE               TO   CSV-REC.
     MOVE     WK-HEAD1            TO   CSV-REC.
     WRITE    CSV-REC.
     MOVE     SPACE               TO   CSV-REC.
     MOVE     WK-HEAD3            TO   CSV-REC.
     WRITE    CSV-REC.
     MOVE     SPACE               TO   CSV-REC.
     MOVE     WK-HEAD4            TO   CSV-REC.
     WRITE    CSV-REC.
     MOVE     WK-HEAD5            TO   CSV-REC.
     WRITE    CSV-REC.
     MOVE     WK-HEAD6            TO   CSV-REC.
     WRITE    CSV-REC.
     ADD      5            TO   OUTPUT-CNT.
*　ブレイクキー設定
     MOVE  JYW-F04    TO       BRK-TENCD.
     MOVE  JYW-F05    TO       BRK-DENNO.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ケーヨー　受領累積データ読み込み　　　
****************************************************************
 KEIJYWL2-RD-SEC            SECTION.
*
     MOVE    "KEIJYWL2-RD-SEC"    TO   S-NAME.
*
     READ     KEIJYWL2
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    KEIJYWL2-RD-EXIT
     END-READ.
*
     ADD   1      TO  READ-CNT.
*
 KEIJYWL2-RD-EXIT.
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
     MOVE   X"28"                 TO   CSV-A01A  CSV-A03A.
     MOVE   X"29"                 TO   CSV-A01B  CSV-A03B.
*
     MOVE     ","                 TO   CSV-A00  CSV-A01 CSV-A02
                                       CSV-A03 CSV-A04
                                       CSV-A05 CSV-A06
                                       CSV-A07 CSV-A08
                                       CSV-A09 CSV-A10
                                       CSV-A11 CSV-A12
                                       CSV-A13 CSV-A14
                                       CSV-A15 CSV-A16
                                       CSV-A17 CSV-A18.
*
*  店舗・伝票番号ブレイク時
     IF      ( JYW-F04   NOT =   BRK-TENCD )  OR
             ( JYW-F05   NOT =   BRK-DENNO )
*        合計
              PERFORM   DENKEI-SEC
              MOVE   ZERO         TO    WK-DENKEI
              INITIALIZE                WK-DENKEI
              IF  ( JYW-F04   NOT =   BRK-TENCD )
                  MOVE  SPACE         TO    RD1-FLG
              END-IF
              MOVE  JYW-F04       TO    BRK-TENCD
              MOVE  JYW-F05       TO    BRK-DENNO
              MOVE  SPACE         TO    RD2-FLG
     END-IF.
*  明細行編集１
     MOVE     JYW-F04           TO    MD01-TENCD
*  店舗正式名
     MOVE     JYW-F04           TO    TEN-F011
     MOVE     173               TO    TEN-F52
     IF  RD1-FLG = SPACE
         PERFORM  TENMS1-READ-SEC
         IF  TENMS1-INV-FLG = "INV"
           MOVE ALL NC"＊"      TO    MD01-TENMEI  BRK-TENMEI
         ELSE
           MOVE TEN-F02         TO    MD01-TENMEI  BRK-TENMEI
         END-IF
         MOVE  "1"              TO    RD1-FLG
     ELSE
         MOVE  BRK-TENMEI     TO    MD01-TENMEI
     END-IF.
*  明細行編集２
*  伝票番号
     MOVE     JYW-F05              TO        MD02-DENNO.
*  伝区
     MOVE     JYW-F07              TO        MD02-DENKU.
     MOVE     ":"                  TO        MD02-DENKUA.
*  発注日
     MOVE     JYW-F24              TO        MD02-HACHUBI.
*  納品日
     MOVE     JYW-F09              TO        MD02-NOHINBI.
*  検収日
     MOVE     JYW-F10              TO        MD02-KENSHUBI.
*  出荷場所
     MOVE     JYW-F27              TO        MD02-SKBCD.
*  振分場所
     MOVE     JYW-F29              TO        MD02-FRBCD.
*  ルート
     MOVE     JYW-F28              TO        MD02-ROUTE.
     IF  RD2-FLG = SPACE
*  伝区名称
         MOVE     15               TO    JYO-F01
         MOVE     JYW-F07          TO    JYO-F02
         PERFORM  JYOKEN1-READ-SEC
         IF   JYOKEN1-INV-FLG = "INV"
              MOVE  ALL NC"＊"     TO    MD02-DNKNM  BRK-DNKNM
         ELSE
              MOVE  JYO-F03        TO    MD02-DNKNM  BRK-DNKNM
         END-IF
         MOVE  "1"              TO    RD2-FLG
     ELSE
         MOVE  BRK-DNKNM        TO    MD02-DNKNM
     END-IF.
*  行番
     MOVE     JYW-F06              TO        MD03-GYO.
*  JANCD
     MOVE     JYW-F11              TO        MD03-JANCD.
*  商品名
     MOVE     JYW-F20              TO        MD03-SHONM1.
     MOVE     JYW-F21              TO        MD03-SHONM2.
*  商品ＣＤ
     MOVE     JYW-F16              TO        MD03-SHOCD.
*  品単１
     MOVE     JYW-F17              TO        MD03-HINTAN1.
*  品単２
     MOVE     JYW-F18              TO        MD03-HINTAN2.
*  品単３
     MOVE     JYW-F19              TO        MD03-HINTAN3.
*  出荷数　
     MOVE     JYW-F23              TO        MD03-SHUKASU.
*  検収数　
     MOVE     JYW-F12              TO        MD03-KENSHUSU.
*  原価単価
     MOVE     JYW-F13              TO        MD03-GENKA.
*  原価金額
     MOVE     JYW-F14              TO        MD03-KINGAKU.
*  差数
     COMPUTE  MD03-SASU    =   JYW-F23 - JYW-F12.
*--------------
*  伝票計加算
*--------------
*  出荷数　
     COMPUTE  WK-SHUKASU-DEN    = WK-SHUKASU-DEN  +  JYW-F23.
*  検収数
     COMPUTE  WK-KENSHUSU-DEN = WK-KENSHUSU-DEN   +  JYW-F12.
*  原価金額
     COMPUTE  WK-KINGAKU-DEN  = WK-KINGAKU-DEN  +  JYW-F14.
*レコードセット
     MOVE     WK-MEISAI1   TO   CSV-REC.
     MOVE     WK-DENMEISAI TO   CSV-DENNO.
*
     WRITE    CSV-REC.
     ADD      1            TO   OUTPUT-CNT.
*    次レコード読込み
     PERFORM  KEIJYWL2-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*             見出し編集処理                1.2                *
****************************************************************
 MIDASISET-SEC             SECTION.
*
     MOVE    "MIDASISET-SEC"              TO    S-NAME.
*システム日付・時刻セット
     MOVE     SYS-DATE-R        TO   HD01-YYYYMMDD.
     MOVE     SYSTEM-TIME       TO   HD01-HHMMSS.
*日付区分・日付
     MOVE     PARA-DKBN         TO   HD03-DKBN.
     EVALUATE   PARA-DKBN
          WHEN   "1"
              MOVE     NC"受信日："         TO   HD03-DKBNNM
              MOVE     PARA-DFROM         TO   HD04-JFROM
              MOVE     PARA-DTO           TO   HD04-JTO
          WHEN   "2"
              MOVE     NC"検収日："         TO   HD03-DKBNNM
              MOVE     PARA-DFROM         TO   HD04-KFROM
              MOVE     PARA-DTO           TO   HD04-KTO
          WHEN   "3"
              MOVE     NC"納品日："         TO   HD03-DKBNNM
              MOVE     PARA-DFROM         TO   HD04-NFROM
              MOVE     PARA-DTO           TO   HD04-NTO
          WHEN   OTHER
              MOVE     NC"発注日："         TO   HD03-DKBNNM
              MOVE     PARA-DFROM         TO   HD04-HFROM
              MOVE     PARA-DTO           TO   HD04-HTO
     END-EVALUATE.
*伝区
     MOVE     PARA-DENK1        TO   HD05-DENK1.
     MOVE     PARA-DENK2        TO   HD05-DENK2.
     MOVE     PARA-DENK3        TO   HD05-DENK3.
     MOVE     PARA-DENK4        TO   HD05-DENK4.
     MOVE     PARA-DENK5        TO   HD05-DENK5.
*
*
 MIDASISET-EXIT.
     EXIT.
****************************************************************
*             伝票合計出力　                　　
****************************************************************
 DENKEI-SEC             SECTION.
*
     MOVE    "DENKEI-SEC"        TO   S-NAME.
*初期化
     MOVE     SPACE               TO   CSV-REC.
*    MOVE     SPACE               TO   WK-MEISAI2.
*    INITIALIZE                        WK-MEISAI2.
*項目転送
     MOVE   X"28"                 TO   CSV-B07A.
     MOVE   X"29"                 TO   CSV-B07B.
*
     MOVE     ","                 TO   CSV-B01 CSV-B02
                                       CSV-B03 CSV-B04
                                       CSV-B05 CSV-B06
                                       CSV-B07 CSV-B08
                                       CSV-B09 CSV-B10
                                       CSV-B11.
*--------------
*  合計転送
*--------------
*  タイトル転送
*    MOVE    WK-DENMEISAI        TO   CSV-DENNO.
*  タイトル転送
     MOVE     NC"＜合計＞"       TO   GK01-TAITOL.
*  出荷数　
     MOVE     WK-SHUKASU-DEN     TO   GK01-SHUKASU.
*  検収数
     MOVE     WK-KENSHUSU-DEN    TO   GK01-KENSHUSU.
*  原価金額
     MOVE     WK-KINGAKU-DEN     TO   GK01-KINGAKU.
*
*伝票合計出力
*レコードセット
     MOVE     WK-MEISAI2    TO   CSV-REC.
*
     WRITE    CSV-REC.
     ADD      1            TO   OUTPUT-CNT.
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
     CLOSE    KEIJYWL2 TENMS1 JYOKEN1  KEICSVJR.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.

```
