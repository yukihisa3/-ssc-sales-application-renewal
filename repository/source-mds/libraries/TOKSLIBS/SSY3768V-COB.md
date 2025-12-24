# SSY3768V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3768V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩシステム　　　　　　*
*    業務名　　　　　　　：　受領処理                          *
*    モジュール名　　　　：　受領書出力　　　　　　　　　      *
*    作成日／更新日　　　：　11/01/27                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受領書を出力する。　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY3768V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/10/15.
*
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
*
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS        CONS
                        YA    IS PITCH-20        *> 2.0ピッチ
                        YA-21 IS PITCH-20-YKBAI  *> 2.0ピッチ、
                        YB    IS PITCH-15        *> 1.5ピッチ
                        YB-21 IS PITCH-15-YKBAI  *> 1.5ピッチ、
                        YB-21 IS PITCH-30.       *> 3.0ピッチ
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<受領累積マスタ ＫＥＹ１>>*************************
     SELECT   NFNJYRF3            ASSIGN    TO   DA-01-VI-NFNJYRL3
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JY3-F01  JY3-F05
                                                JY3-F07  JY3-F04
                                                JY3-F08
                                 STATUS         JY3-STATUS.
****<<受領累積マスタ ＫＥＹ１>>*************************
     SELECT   NFNJYRF2            ASSIGN    TO   DA-01-VI-NFNJYRL2
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JY2-F02
                                                JY2-F05  JY2-F07
                                                JY2-F04  JY2-F08
                                 STATUS         JY2-STATUS.
*****<<  プリント　Ｆ   >>**************************************
     SELECT   PRINTF    ASSIGN    TO        LP-04-PRTF.
*
*****<<出荷指示ﾃﾞｰﾀCSV >>*************************************
     SELECT   NFJYRTXT          ASSIGN    TO   NFJYRTXT
                                STATUS         NFJ-STATUS.
*                                                              *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ナフコ　　受領累積ファイル　　　　　               *
*--------------------------------------------------------------*
 FD  NFNJYRF3            LABEL RECORD   IS   STANDARD.
     COPY     NFNJYRF    OF        XFDLIB
              JOINING   JY3       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ　　受領累積ファイル　　　　　               *
*--------------------------------------------------------------*
 FD  NFNJYRF2            LABEL RECORD   IS   STANDARD.
     COPY     NFNJYRF    OF        XFDLIB
              JOINING   JY2       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = プリントファイル                                   *
*--------------------------------------------------------------*
 FD  PRINTF.
 01  P-REC                        PIC       X(200).
*
*--------------------------------------------------------------*
*    FILE = ナフコ受領データＣＳＶ　　　　　                   *
*--------------------------------------------------------------*
 FD  NFJYRTXT          BLOCK CONTAINS 1    RECORDS.
 01  NFJ-REC.
     03  FILLER         PIC       X(500).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*
     COPY     NFNJYRF   OF   XFDLIB   JOINING   JYU    PREFIX.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  JY3-STATUS                   PIC       X(02).
 01  JY2-STATUS                   PIC       X(02).
 01  NFJ-STATUS                   PIC       X(02).
*
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC       9(06)  VALUE  ZERO.
     03  SYS-DATEW                PIC       9(08)  VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES SYS-DATEW.
         05  SYS-YY               PIC       9(04).
         05  SYS-MM               PIC       9(02).
         05  SYS-DD               PIC       9(02).
*
 01  WK-AREA.
     03  IX1                      PIC       9(02)  VALUE  ZERO.
     03  SET-FLG                  PIC       X(01)  VALUE  SPACE.
     03  PRT-FLG                  PIC       X(01)  VALUE  SPACE.
*
***** カウンタ
 01  P-CNT                        PIC       9(04)  VALUE  ZERO.
 01  L-CNT                        PIC       9(03)  VALUE  ZERO.
 01  CNT-READ                     PIC       9(06)  VALUE  ZERO.
 01  MEI-CNT                      PIC       9(05)  VALUE  ZERO.
 01  INPUT-CNT                    PIC       9(07)  VALUE  ZERO.
 01  TAI-CNT                      PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
*
 01  WK-BACHINO.
     03  WK-BACHI-YMD             PIC       9(08)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  WK-BACHI-TIME            PIC       9(04)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  WK-BACHI-TORICD          PIC       9(08)  VALUE  ZERO.
*
 01  BRK-KEY.
     03  BRK-TENCD                PIC       9(03)  VALUE  ZERO.
     03  BRK-DENCD                PIC       9(09)  VALUE  ZERO.
*
 01  WK-AREA.
     03  WK-SURYO                 PIC      S9(06).
     03  WK-GENKA                 PIC      S9(09).
     03  WK-SURYOR                PIC      S9(05)V9(01).
     03  WK-GENKAR                PIC      S9(07)V9(02).
     03  WK-KINGAKU               PIC      S9(10).
*
*--------------
 01  WK-DENKEI.
     03  WK-SURYO-DEN             PIC      S9(07)V9.
     03  WK-GENKA-DEN             PIC      S9(07)V99.
     03  WK-KINGAKU-DEN           PIC      S9(10).
*
 01  WK-TENKEI.
     03  WK-SURYO-TEN             PIC      S9(07)V9.
     03  WK-GENKA-TEN             PIC      S9(07)V99.
     03  WK-KINGAKU-TEN           PIC      S9(10).
*
 01  WK-SOKEI.
     03  WK-SURYO-KEI             PIC      S9(07)V9.
     03  WK-GENKA-KEI             PIC      S9(07)V99.
     03  WK-KINGAKU-KEI           PIC      S9(10).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3768V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3768V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY3768V".
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
***** 見出し行１
 01  HD01.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  X(08)  VALUE  "SSY3768V".
     03  FILLER                PIC  X(44)  VALUE  SPACE.
     03  FILLER                PIC  N(10)
                               VALUE NC"＜　ナフコ受領書　＞"
                               CHARACTER  TYPE  IS PITCH-30.
     03  FILLER                PIC  X(30)  VALUE  SPACE.
     03  HD01-YY               PIC  9999.
     03  FILLER                PIC  N(01)  VALUE  NC"年"
                               CHARACTER  TYPE  IS PITCH-20.
     03  HD01-MM               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"月"
                               CHARACTER  TYPE IS PITCH-20.
     03  HD01-DD               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"日"
                               CHARACTER  TYPE IS PITCH-20.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  HD01-PCNT             PIC  ZZ9.
     03  FILLER                PIC  N(02)  VALUE  NC"頁"
                               CHARACTER  TYPE IS PITCH-20.
***** 見出し行２
 01  HD02.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"計上日："
                               CHARACTER  TYPE IS PITCH-15.
     03  HD02-STYMD            PIC  9(08).
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  "-"  .
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD02-EDYMD            PIC  9(08).
     03  FILLER                PIC  X(180) VALUE  SPACE.
***** 見出し行３
 01  HD03.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(02)  VALUE  NC"赤黒"
                               CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                PIC  X(01)  VALUE  "(".
     03  FILLER                PIC  N(04)  VALUE  NC"赤黒区分"
                               CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                PIC  X(05)  VALUE  ")=>0:".
     03  FILLER                PIC  N(01)  VALUE  NC"黒"
                               CHARACTER   TYPE IS   PITCH-20.
     03  FILLER                PIC  X(01)  VALUE  ",".
     03  FILLER                PIC  N(01)  VALUE  NC"赤"
                               CHARACTER   TYPE IS   PITCH-20.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(08)  VALUE
                               NC"受（受領区分）⇒"
                               CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                PIC  X(03)  VALUE  "00:".
     03  FILLER                PIC  N(04)  VALUE  NC"返品値引"
                               CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                PIC  X(01)  VALUE  ",".
     03  FILLER                PIC  X(03)  VALUE  "01:".
     03  FILLER                PIC  N(02)  VALUE  NC"ＯＫ"
                               CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                PIC  X(01)  VALUE  ",".
     03  FILLER                PIC  X(03)  VALUE  "02:".
     03  FILLER                PIC  N(02)  VALUE  NC"ＮＧ"
                               CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                PIC  X(01)  VALUE  ",".
     03  FILLER                PIC  X(03)  VALUE  "03:".
     03  FILLER                PIC  N(02)  VALUE  NC"赤黒"
                               CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(08)  VALUE
                               NC"理（理由区分）⇒"
                               CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                PIC  N(06)  VALUE
                               NC"区分一覧参照"
                               CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  X(02)  VALUE  "CD".
     03  FILLER                PIC  N(14)  VALUE
                               NC"（理由区分）⇒区分一覧参照　"
                               CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                PIC  X(180) VALUE  SPACE.
***** 見出し行４
 01  HD04.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"店舗情報"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(12)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"計上日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"伝区"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"赤黒"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"伝票番号"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"行"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"商品"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(32)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"受領数量"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"原価単価"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(07)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"原価金額"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"受"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"理"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  X(02)  VALUE  "CD".
*
***** 線
 01  SEN1.
     03  FILLER                   PIC       X(40)  VALUE
         "========================================".
     03  FILLER                   PIC       X(40)  VALUE
         "========================================".
     03  FILLER                   PIC       X(40)  VALUE
         "========================================".
     03  FILLER                   PIC       X(16)  VALUE
         "================".
*
***** 線
 01  SEN2.
     03  FILLER                   PIC       X(40)  VALUE
         "----------------------------------------".
     03  FILLER                   PIC       X(40)  VALUE
         "----------------------------------------".
     03  FILLER                   PIC       X(40)  VALUE
         "----------------------------------------".
     03  FILLER                   PIC       X(16)  VALUE
         "----------------".
*
***** 明細行
 01  MD01                         CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TENCD               PIC       XXX.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TENMEI              PIC       N(10).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-KEIJYOBI            PIC       99999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-DENKU               PIC       99.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-DNKNM               PIC       N(02).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-AKAKURO             PIC       9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-DENNO               PIC       99999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-GYO                 PIC       99.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SHOCD               PIC       99999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SHONM               PIC       X(25).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-JYURYOSU            PIC       -,---,--9.9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-GENKA               PIC       -,---,--9.99.
*****03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-KINGAKU             PIC       ----,---,--9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-JYU                 PIC       99.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-RIYU                PIC       99.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-RICD                PIC       99.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-RINM                PIC       N(10).
*
***** 合計行
 01  GK01             CHARACTER      TYPE IS     PITCH-15.
     03  FILLER                   PIC       X(77)  VALUE  SPACE.
     03  GK01-TAITOL              PIC       N(06).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  GK01-SURYO               PIC       -,---,--9.9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  GK01-GENKA               PIC       -,---,--9.99.
     03  GK01-KINGAKU             PIC       ----,---,--9.
     03  FILLER                   PIC       X(180) VALUE  SPACE.
*見出しエリア
 01  WK-HEAD.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"データ受信日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"仕入計上日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"伝票区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"赤黒区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"店舗ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"店舗名称".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"伝票番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"行番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"元伝票番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"商品ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"ＪＡＮＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"商品名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"規格名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"数量".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"原単価（税抜）".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"売単価（税抜）".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"受領区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"理由区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"理由ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"理由名".
     03  FILLER        PIC X(01)  VALUE  X"29".

*明細エリア
 01  WK-MEISAI.
     03  NFJ-F01       PIC 9(08).  *>データ受信日
     03  NFJ-A01       PIC X(01).
     03  NFJ-F02       PIC 9(08).  *>仕入計上日
     03  NFJ-A02       PIC X(01).
     03  NFJ-F03       PIC 9(02).  *>伝票区分
     03  NFJ-A03       PIC X(01).
     03  NFJ-F04       PIC 9(02).  *>赤黒区分
     03  NFJ-A04       PIC X(01).
     03  NFJ-F05       PIC X(03).  *>店舗ＣＤ
     03  NFJ-A05       PIC X(01).
     03  NFJ-F06S      PIC X(01).
     03  NFJ-F06       PIC N(10).  *>店舗名
     03  NFJ-F06E      PIC X(01).
     03  NFJ-A06       PIC X(01).
     03  NFJ-F07       PIC 9(08).  *>伝票番号
     03  NFJ-A07       PIC X(01).
     03  NFJ-F08       PIC 9(02).  *>行番号
     03  NFJ-A08       PIC X(01).
     03  NFJ-F09       PIC 9(08).  *>元伝票番号
     03  NFJ-A09       PIC X(01).
     03  NFJ-F10       PIC 9(08).  *>商品ＣＤ
     03  NFJ-A10       PIC X(01).
     03  NFJ-F11       PIC X(13).  *>ＪＡＮＣＤ
     03  NFJ-A11       PIC X(01).
     03  NFJ-F12       PIC X(25).  *>商品名
     03  NFJ-A12       PIC X(01).
     03  NFJ-F13       PIC X(25).  *>規格名
     03  NFJ-A13       PIC X(01).
     03  NFJ-F14F      PIC X(01).  *>符号
     03  NFJ-F14       PIC 9(09).  *>数量
     03  NFJ-A14       PIC X(01).
     03  NFJ-F15       PIC 9(09).  *>原価単価
     03  NFJ-A15       PIC X(01).
     03  NFJ-F16       PIC 9(09).  *>売価単価
     03  NFJ-A16       PIC X(01).
     03  NFJ-F17       PIC 9(02).  *>受領区分
     03  NFJ-A17       PIC X(01).
     03  NFJ-F18       PIC 9(02).  *>理由区分
     03  NFJ-A18       PIC X(01).
     03  NFJ-F19       PIC 9(02).  *>理由ＣＤ
     03  NFJ-A19       PIC X(01).
     03  NFJ-F20S      PIC X(01).
     03  NFJ-F20       PIC N(10).  *>理由名
     03  NFJ-F20E      PIC X(01).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-KBN               PIC   X(01).
 01  PARA-JYUSIN-ST         PIC   9(08).
 01  PARA-JYUSIN-ED         PIC   9(08).
 01  PARA-SIIRED-ST         PIC   9(08).
 01  PARA-SIIRED-ED         PIC   9(08).
 01  PARA-LIST-KBN          PIC   X(01).
 01  PARA-LIST-KENSU        PIC   9(07).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-KBN
                                           PARA-JYUSIN-ST
                                           PARA-JYUSIN-ED
                                           PARA-SIIRED-ST
                                           PARA-SIIRED-ED
                                           PARA-LIST-KBN
                                           PARA-LIST-KENSU.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           NFNJYRF3.
     MOVE     "NFNJYRL3"          TO        ERR-FL-ID.
     MOVE     JY3-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           NFNJYRF2.
     MOVE     "NFNJYRL2"          TO        ERR-FL-ID.
     MOVE     JY2-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY3768V-START         SECTION.
*
     MOVE   "SSY3768V-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
*
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SSY3768V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     DISPLAY "PARA-LIST-KBN = " PARA-LIST-KBN UPON CONS.
*
     OPEN     INPUT     NFNJYRF3  NFNJYRF2.
     OPEN     OUTPUT    PRINTF.
*ＣＳＶの場合はＣＳＶファイルをＯＰＥＮする。
     IF  PARA-LIST-KBN  = "2"
         OPEN OUTPUT    NFJYRTXT
     END-IF.
*
     MOVE     ZERO           TO    WK-DENKEI
                                   WK-TENKEI
                                   WK-SOKEI.
     INITIALIZE                    WK-DENKEI
                                   WK-TENKEI
                                   WK-SOKEI.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     99             TO    L-CNT.
*システム日付取得→８桁変換
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
*
     IF  PARA-KBN = "1" *>受信日で出力の時
         MOVE  SPACE              TO   JY3-REC
         INITIALIZE                    JY3-REC
         MOVE  PARA-JYUSIN-ST     TO   JY3-F01
*
         START  NFNJYRF3  KEY  >=   JY3-F01  JY3-F05
                                    JY3-F07  JY3-F04  JY3-F08
                INVALID   KEY
                MOVE     "END"      TO   END-FLG
                DISPLAY NC"＃対象データ無し１＃" UPON CONS
                GO                  TO   INIT-EXIT
         END-START
*
         PERFORM  NFNJYRF3-RD-SEC
         IF    END-FLG   =   "END"
               DISPLAY NC"＃対象データ無し２＃" UPON CONS
               GO                  TO   INIT-EXIT
         END-IF
         MOVE     PARA-JYUSIN-ST   TO   HD02-STYMD
         MOVE     PARA-JYUSIN-ED   TO   HD02-EDYMD
     ELSE
         MOVE  SPACE              TO   JY2-REC
         INITIALIZE                    JY2-REC
         MOVE  PARA-SIIRED-ST     TO   JY2-F02
*
         START  NFNJYRF2  KEY  >=   JY2-F02  JY2-F05
                                    JY2-F07  JY2-F04  JY2-F08
                INVALID   KEY
                MOVE     "END"      TO   END-FLG
                DISPLAY NC"＃対象データ無し３＃" UPON CONS
                GO                  TO   INIT-EXIT
         END-START
*
         PERFORM  NFNJYRF2-RD-SEC
         IF    END-FLG   =   "END"
               DISPLAY NC"＃対象データ無し４＃" UPON CONS
               GO                  TO   INIT-EXIT
         END-IF
         MOVE     PARA-SIIRED-ST   TO   HD02-STYMD
         MOVE     PARA-SIIRED-ED   TO   HD02-EDYMD
     END-IF.
*
*　ブレイクキー設定
     MOVE  JYU-F05    TO       BRK-TENCD.
     MOVE  JYU-F07    TO       BRK-DENCD.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ　受領累積ファイル読込（キー１）
****************************************************************
 NFNJYRF3-RD-SEC            SECTION.
*
     MOVE    "NFNJYRF3-RD-SEC"    TO   S-NAME.
*
     READ     NFNJYRF3
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    NFNJYRF3-RD-EXIT
     END-READ.
*
     ADD      1                   TO   INPUT-CNT.
*件数表示
     IF   INPUT-CNT (5:3) = "000" OR "500"
          DISPLAY "IN-CNT  = " INPUT-CNT   UPON CONS
     END-IF.
*該当範囲チェック
     IF   JY3-F01   <   PARA-JYUSIN-ST AND
          JY3-F01   >   PARA-JYUSIN-ED
          GO     TO     NFNJYRF3-RD-SEC
     END-IF.
*ワークレコードへセット
     MOVE        JY3-REC          TO   JYU-REC.
*対象件数カウント
     ADD         1                TO   TAI-CNT.
*
 NFNJYRF3-RD-EXIT.
     EXIT.
****************************************************************
*    ナフコ　受領累積ファイル読込（キー２）
****************************************************************
 NFNJYRF2-RD-SEC            SECTION.
*
     MOVE    "NFNJYRF2-RD-SEC"    TO   S-NAME.
*
     READ     NFNJYRF2
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    NFNJYRF2-RD-EXIT
     END-READ.
*
     ADD      1                   TO   INPUT-CNT.
*件数表示
     IF   INPUT-CNT (5:3) = "000" OR "500"
          DISPLAY "INPUT-CNT  = " INPUT-CNT   UPON CONS
     END-IF.
*該当範囲チェック
     IF   JY2-F02   <   PARA-SIIRED-ST AND
          JY2-F02   >   PARA-SIIRED-ED
          GO     TO     NFNJYRF2-RD-SEC
     END-IF.
*ワークレコードへセット
     MOVE        JY2-REC          TO   JYU-REC.
*対象件数カウント
     ADD         1                TO   TAI-CNT.
*
 NFNJYRF2-RD-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO    S-NAME.
*    帳票区分により判断
     IF  PARA-LIST-KBN = "1" *>受信日で出力の時
         PERFORM  LST-SEC
     ELSE
         PERFORM  CSV-SEC
     END-IF.
*    次レコード読込み
     IF  PARA-KBN = "1" *>受信日で出力の時
         PERFORM  NFNJYRF3-RD-SEC
     ELSE
         PERFORM  NFNJYRF2-RD-SEC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             帳票出力処理制御
****************************************************************
 LST-SEC                SECTION.
*
     MOVE    "LST-SEC"            TO    S-NAME.
*明細行初期化
     MOVE    SPACE                TO    MD01.
*  改頁チェック
     IF       L-CNT     >    58
              PERFORM  MIDASI-SEC
              MOVE    "1"         TO    PRT-FLG
     END-IF.
*店舗ＣＤブレイク時
     IF       JYU-F05   NOT =   BRK-TENCD
**************伝票合計
              PERFORM   DENKEI-SEC
              MOVE   ZERO         TO    WK-DENKEI
              INITIALIZE                WK-DENKEI
**************店舗合計
              PERFORM   TENKEI-SEC
              MOVE   ZERO         TO    WK-TENKEI
              INITIALIZE                WK-TENKEI
              MOVE   JYU-F05      TO    BRK-TENCD
              MOVE   JYU-F07      TO    BRK-DENCD
              MOVE    "1"         TO    PRT-FLG
     END-IF.
*  伝票ＣＤブレイク時
     IF       JYU-F07   NOT =   BRK-DENCD
*        伝票合計
              PERFORM   DENKEI-SEC
              MOVE   ZERO         TO    WK-DENKEI
              INITIALIZE                WK-DENKEI
              MOVE   JYU-F07      TO    BRK-DENCD
              MOVE    "1"         TO    PRT-FLG
     END-IF.
*  明細行編集出力（店舗名の出力）
     IF       PRT-FLG       =   "1"
              MOVE   JYU-F05      TO    MD01-TENCD
              MOVE   JYU-F19      TO    MD01-TENMEI
              MOVE   SPACE        TO    PRT-FLG
     END-IF.
*  計上日
     MOVE     JYU-F02              TO        MD01-KEIJYOBI.
*  伝区
     MOVE     JYU-F03              TO        MD01-DENKU.
*  伝区名称
     EVALUATE   JYU-F03
         WHEN   "01"
                MOVE     NC"仕入"  TO    MD01-DNKNM
         WHEN   "02"
                MOVE     NC"仕入"  TO    MD01-DNKNM
         WHEN   "11"
                MOVE     NC"返品"  TO    MD01-DNKNM
         WHEN   "12"
                MOVE     NC"値引"  TO    MD01-DNKNM
         WHEN   "61"
                MOVE     NC"相殺"  TO    MD01-DNKNM
     END-EVALUATE.
*  赤黒
     MOVE     JYU-F04              TO        MD01-AKAKURO.
*  伝票番号
     MOVE     JYU-F07              TO        MD01-DENNO.
*  行番
     MOVE     JYU-F08              TO        MD01-GYO.
*  商品ＣＤ　
     MOVE     JYU-F10              TO        MD01-SHOCD.
*  商品名
     MOVE     JYU-F20              TO        MD01-SHONM
*  受領数　
     MOVE     JYU-F12              TO        WK-SURYO.
     COMPUTE  WK-SURYOR   =   WK-SURYO  /  10.
     EVALUATE JYU-F03 ALSO JYU-F04
         WHEN 1       ALSO 1
              COMPUTE WK-SURYOR = WK-SURYOR * -1
         WHEN 2       ALSO 1
              COMPUTE WK-SURYOR = WK-SURYOR * -1
         WHEN 11      ALSO 1
              COMPUTE WK-SURYOR = WK-SURYOR * -1
         WHEN 12      ALSO 1
              COMPUTE WK-SURYOR = WK-SURYOR * -1
     END-EVALUATE.
     MOVE     WK-SURYOR            TO        MD01-JYURYOSU.
*  原価単価
     MOVE     JYU-F13              TO        WK-GENKA.
     COMPUTE  WK-GENKAR   =    WK-GENKA  /  100.
     MOVE     WK-GENKAR            TO        MD01-GENKA.
*  原価金額
     COMPUTE  WK-KINGAKU  =    WK-SURYOR *   WK-GENKA.
     MOVE     WK-KINGAKU           TO        MD01-KINGAKU.
*  受領区分
     MOVE     JYU-F15              TO        MD01-JYU.
*  理由区分
     MOVE     JYU-F16              TO        MD01-RIYU.
*  理由ＣＤ
     MOVE     JYU-F17              TO        MD01-RICD.
*  理由名称
     EVALUATE JYU-F16 ALSO JYU-F17
         WHEN   1     ALSO     1
              MOVE NC"仕入先都合"        TO  MD01-RINM
         WHEN   1     ALSO     2
              MOVE NC"ナフコ都合"        TO  MD01-RINM
         WHEN   1     ALSO     3
              MOVE NC"その他"            TO  MD01-RINM
         WHEN   2     ALSO     1
              MOVE NC"不良品"            TO  MD01-RINM
         WHEN   2     ALSO     9
              MOVE NC"本部指示"          TO  MD01-RINM
         WHEN   3     ALSO     1
              MOVE NC"店舗コード違い"    TO  MD01-RINM
         WHEN   3     ALSO     2
              MOVE NC"仕入先違い"        TO  MD01-RINM
         WHEN   3     ALSO     3
              MOVE NC"日付違い"          TO  MD01-RINM
         WHEN   3     ALSO     4
              MOVE NC"原価違い"          TO  MD01-RINM
         WHEN   3     ALSO     5
              MOVE NC"売価違い"          TO  MD01-RINM
         WHEN   3     ALSO     6
              MOVE NC"数量違い"          TO  MD01-RINM
         WHEN   3     ALSO     7
              MOVE NC"金額違い"          TO  MD01-RINM
         WHEN   3     ALSO     8
              MOVE NC"部門違い"          TO  MD01-RINM
         WHEN   3     ALSO     9
              MOVE NC"商品ＣＤ違い"      TO  MD01-RINM
         WHEN   7     ALSO     1
              MOVE NC"ＶＤ仮引"          TO  MD01-RINM
         WHEN   7     ALSO     2
              MOVE NC"ＶＤ清算"          TO  MD01-RINM
         WHEN   7     ALSO     3
              MOVE NC"ＶＤ前期末清算"    TO  MD01-RINM
         WHEN   8     ALSO     4
              MOVE NC"ＶＤ清算"          TO  MD01-RINM
         WHEN   8     ALSO     5
              MOVE NC"違算調整"          TO  MD01-RINM
         WHEN   8     ALSO    99
              MOVE NC"本部指示"          TO  MD01-RINM
         WHEN   9     ALSO    99
              MOVE NC"本部指示"          TO  MD01-RINM
         WHEN  14     ALSO     1
              MOVE NC"数量不足"          TO  MD01-RINM
         WHEN  14     ALSO     2
              MOVE NC"不良品"            TO  MD01-RINM
         WHEN OTHER
              MOVE SPACE                 TO  MD01-RINM
     END-EVALUATE.
*--------------
*  伝票計加算
*--------------
*  受領数　
     COMPUTE  WK-SURYO-DEN    =   WK-SURYO-DEN     +  WK-SURYOR.
*  原価単価
     COMPUTE  WK-GENKA-DEN    =   WK-GENKA-DEN     +  WK-GENKAR.
*  原価金額
     COMPUTE  WK-KINGAKU-DEN  =   WK-KINGAKU-DEN   +  WK-KINGAKU.
*
*--------------
*  店舗計加算
*--------------
*  受領数　
     COMPUTE  WK-SURYO-TEN    =   WK-SURYO-TEN     +  WK-SURYOR.
*  原価単価
     COMPUTE  WK-GENKA-TEN    =   WK-GENKA-TEN     +  WK-GENKAR.
*  原価金額
     COMPUTE  WK-KINGAKU-TEN  =   WK-KINGAKU-TEN   +  WK-KINGAKU.
*
*--------------
*  総合計加算
*--------------
*  受領数　
     COMPUTE  WK-SURYO-KEI    =   WK-SURYO-KEI     +  WK-SURYOR.
*  原価単価
     COMPUTE  WK-GENKA-KEI    =   WK-GENKA-KEI     +  WK-GENKAR.
*  原価金額
     COMPUTE  WK-KINGAKU-KEI  =   WK-KINGAKU-KEI   +  WK-KINGAKU.
*
*--------------
*  明細行出力
*--------------
     WRITE  P-REC  FROM  MD01  AFTER 1.
     ADD      1              TO   L-CNT.
*
 LST-EXIT.
     EXIT.
****************************************************************
*             見出し出力処理                1.2                *
****************************************************************
 CSV-SEC                SECTION.
*
     MOVE    "CSV-SEC"                 TO    S-NAME.
*初期化
     MOVE     SPACE               TO   WK-MEISAI  NFJ-REC.
     INITIALIZE                        WK-MEISAI  NFJ-REC.
*ヘッダ出力制御
     IF  OUTPUT-CNT = ZERO
         MOVE   SPACE             TO   NFJ-REC
         INITIALIZE                    NFJ-REC
         MOVE   WK-HEAD           TO   NFJ-REC
         WRITE  NFJ-REC
         ADD    1                 TO   OUTPUT-CNT
         MOVE   SPACE             TO   WK-MEISAI
         INITIALIZE                    WK-MEISAI
     END-IF.
*項目転送
     MOVE   X"28"                 TO   NFJ-F20S.
     MOVE   X"29"                 TO   NFJ-F20E.
     MOVE   X"28"                 TO   NFJ-F06S.
     MOVE   X"29"                 TO   NFJ-F06E.
*
     MOVE     ","                 TO   NFJ-A01 NFJ-A02
     MOVE     ","                 TO   NFJ-A03 NFJ-A04
     MOVE     ","                 TO   NFJ-A05 NFJ-A06
     MOVE     ","                 TO   NFJ-A07 NFJ-A08
     MOVE     ","                 TO   NFJ-A09 NFJ-A10
     MOVE     ","                 TO   NFJ-A11 NFJ-A12
     MOVE     ","                 TO   NFJ-A13 NFJ-A14
     MOVE     ","                 TO   NFJ-A15 NFJ-A16
     MOVE     ","                 TO   NFJ-A17 NFJ-A18
     MOVE     ","                 TO   NFJ-A19.
*
     MOVE     JYU-F01      TO   NFJ-F01. *>データ受信日
     MOVE     JYU-F02      TO   NFJ-F02. *>仕入計上日
     MOVE     JYU-F03      TO   NFJ-F03. *>伝票区分
     MOVE     JYU-F04      TO   NFJ-F04. *>赤黒区分
     MOVE     JYU-F05      TO   NFJ-F05. *>店舗ＣＤ
     MOVE     JYU-F19      TO   NFJ-F06. *>店舗名
     MOVE     JYU-F07      TO   NFJ-F07. *>伝票番号
     MOVE     JYU-F08      TO   NFJ-F08. *>行番号
     MOVE     JYU-F09      TO   NFJ-F09. *>元伝票番号
     MOVE     JYU-F10      TO   NFJ-F10. *>商品ＣＤ
     MOVE     JYU-F11      TO   NFJ-F11. *>ＪＡＮＣＤ
     MOVE     JYU-F20      TO   NFJ-F12. *>商品名
     MOVE     JYU-F21      TO   NFJ-F13. *>規格名
*****MOVE     JYU-F12      TO   NFJ-F14. *>数量
     MOVE     JYU-F12      TO   WK-SURYO.
     COMPUTE  WK-SURYOR   =   WK-SURYO  /  10.
     EVALUATE JYU-F03 ALSO JYU-F04
         WHEN 1       ALSO 1
              MOVE "-"     TO   NFJ-F14F
         WHEN 2       ALSO 1
              MOVE "-"     TO   NFJ-F14F
         WHEN 11      ALSO 1
              MOVE "-"     TO   NFJ-F14F
         WHEN 12      ALSO 1
              MOVE "-"     TO   NFJ-F14F
         WHEN OTHER
              MOVE "0"     TO   NFJ-F14F
     END-EVALUATE.
     MOVE     WK-SURYOR    TO   NFJ-F14. *>数量
*****MOVE     JYU-F13      TO   NFJ-F15. *>原価単価
*  原価単価
     MOVE     JYU-F13      TO   WK-GENKA.
     COMPUTE  WK-GENKAR   =    WK-GENKA  /  100.
     MOVE     WK-GENKAR    TO   NFJ-F15. *>原価単価
     MOVE     JYU-F14      TO   NFJ-F16. *>売価単価
     MOVE     JYU-F15      TO   NFJ-F17. *>受領区分
     MOVE     JYU-F16      TO   NFJ-F18. *>理由区分
     MOVE     JYU-F17      TO   NFJ-F19. *>理由ＣＤ
*****MOVE     JYU-F11      TO   NFJ-F20. *>理由名
*  理由名称
     EVALUATE JYU-F16 ALSO JYU-F17
         WHEN   1     ALSO     1
              MOVE NC"仕入先都合"        TO  NFJ-F20
         WHEN   1     ALSO     2
              MOVE NC"ナフコ都合"        TO  NFJ-F20
         WHEN   1     ALSO     3
              MOVE NC"その他"            TO  NFJ-F20
         WHEN   2     ALSO     1
              MOVE NC"不良品"            TO  NFJ-F20
         WHEN   2     ALSO     9
              MOVE NC"本部指示"          TO  NFJ-F20
         WHEN   3     ALSO     1
              MOVE NC"店舗コード違い"    TO  NFJ-F20
         WHEN   3     ALSO     2
              MOVE NC"仕入先違い"        TO  NFJ-F20
         WHEN   3     ALSO     3
              MOVE NC"日付違い"          TO  NFJ-F20
         WHEN   3     ALSO     4
              MOVE NC"原価違い"          TO  NFJ-F20
         WHEN   3     ALSO     5
              MOVE NC"売価違い"          TO  NFJ-F20
         WHEN   3     ALSO     6
              MOVE NC"数量違い"          TO  NFJ-F20
         WHEN   3     ALSO     7
              MOVE NC"金額違い"          TO  NFJ-F20
         WHEN   3     ALSO     8
              MOVE NC"部門違い"          TO  NFJ-F20
         WHEN   3     ALSO     9
              MOVE NC"商品ＣＤ違い"      TO  NFJ-F20
         WHEN   7     ALSO     1
              MOVE NC"ＶＤ仮引"          TO  NFJ-F20
         WHEN   7     ALSO     2
              MOVE NC"ＶＤ清算"          TO  NFJ-F20
         WHEN   7     ALSO     3
              MOVE NC"ＶＤ前期末清算"    TO  NFJ-F20
         WHEN   8     ALSO     4
              MOVE NC"ＶＤ清算"          TO  NFJ-F20
         WHEN   8     ALSO     5
              MOVE NC"違算調整"          TO  NFJ-F20
         WHEN   8     ALSO    99
              MOVE NC"本部指示"          TO  NFJ-F20
         WHEN   9     ALSO    99
              MOVE NC"本部指示"          TO  NFJ-F20
         WHEN  14     ALSO     1
              MOVE NC"数量不足"          TO  NFJ-F20
         WHEN  14     ALSO     2
              MOVE NC"不良品"            TO  NFJ-F20
         WHEN OTHER
              MOVE NC"理由なし"          TO  NFJ-F20
     END-EVALUATE.
     MOVE     WK-MEISAI    TO   NFJ-REC.
*
     WRITE    NFJ-REC
     ADD      1            TO   OUTPUT-CNT.
*
 CSV-EXIT.
     EXIT.
****************************************************************
*             見出し出力処理                1.2                *
****************************************************************
 MIDASI-SEC             SECTION.
*
     MOVE    "MIDASI-SEC"              TO    S-NAME.
*改頁
     IF       P-CNT  >  ZERO
              MOVE   SPACE   TO   P-REC
              WRITE  P-REC   AFTER PAGE
     END-IF.
*システム日付セット
     MOVE     SYS-YY            TO   HD01-YY.
     MOVE     SYS-MM            TO   HD01-MM.
     MOVE     SYS-DD            TO   HD01-DD.
*頁セット
     ADD      1              TO   P-CNT.
     MOVE     P-CNT          TO   HD01-PCNT.
*ヘッダー出力
     WRITE    P-REC     FROM      HD01      AFTER     1.
     WRITE    P-REC     FROM      HD02      AFTER     2.
     WRITE    P-REC     FROM      HD03      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
     WRITE    P-REC     FROM      HD04      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
*
     MOVE     8         TO        L-CNT.
*
 MIDASI-EXIT.
     EXIT.
****************************************************************
*             伝票合計出力　                　　
****************************************************************
 DENKEI-SEC             SECTION.
*
     MOVE    "DENKEI-SEC"        TO   S-NAME.
*改頁
     IF       L-CNT  >=  58
              PERFORM  MIDASI-SEC
     END-IF.
*
*--------------
*  合計転送
*--------------
*  タイトル転送
     MOVE     NC"＜伝票合計＞"   TO   GK01-TAITOL.
*  受領数　
     MOVE     WK-SURYO-DEN       TO   GK01-SURYO.
*  原価単価
     MOVE     WK-GENKA-DEN       TO   GK01-GENKA.
*  原価金額
     MOVE     WK-KINGAKU-DEN     TO   GK01-KINGAKU.
*
*伝票合計出力
     WRITE    P-REC     FROM      GK01      AFTER     1.
*
     ADD      1         TO        L-CNT.
*
*  線出力
     IF       L-CNT  >=  58
              PERFORM  MIDASI-SEC
     END-IF.
     WRITE    P-REC     FROM  SEN2  AFTER 1.
     ADD      1         TO    L-CNT.
*
 DENKEI-EXIT.
     EXIT.
*
****************************************************************
*             店舗合計出力　                　　
****************************************************************
 TENKEI-SEC             SECTION.
*
     MOVE    "TENKEI-SEC"        TO   S-NAME.
*改頁
     IF       L-CNT  >=  58
              PERFORM  MIDASI-SEC
     END-IF.
*
*--------------
*  合計転送
*--------------
*  タイトル転送
     MOVE     NC"＜店舗合計＞"   TO   GK01-TAITOL.
*  受領数　
     MOVE     WK-SURYO-TEN       TO   GK01-SURYO.
*  原価単価
     MOVE     WK-GENKA-TEN       TO   GK01-GENKA.
*  原価金額
     MOVE     WK-KINGAKU-TEN     TO   GK01-KINGAKU.
*
*伝票合計出力
     WRITE    P-REC     FROM      GK01      AFTER     1.
*
     ADD      1         TO        L-CNT.
*
*  線出力
     IF       L-CNT  >=  58
              PERFORM  MIDASI-SEC
     END-IF.
     WRITE    P-REC     FROM  SEN2  AFTER 1.
     ADD      1         TO    L-CNT.
*
 TENKEI-EXIT.
     EXIT.
*
****************************************************************
*             総合計出力　　                　　
****************************************************************
 SOUGOKEI-SEC           SECTION.
*
     MOVE    "TENKEI-SEC"        TO   S-NAME.
*改頁
     IF       L-CNT  >=  58
              PERFORM  MIDASI-SEC
     END-IF.
*
*--------------
*  合計転送
*--------------
*  タイトル転送
     MOVE     NC"＜総合計　＞"   TO   GK01-TAITOL.
*  受領数　
     MOVE     WK-SURYO-KEI       TO   GK01-SURYO.
*  原価単価
     MOVE     WK-GENKA-KEI       TO   GK01-GENKA.
*  原価金額
     MOVE     WK-KINGAKU-KEI     TO   GK01-KINGAKU.
*
*伝票合計出力
     WRITE    P-REC     FROM      GK01      AFTER     1.
*
     ADD      1         TO        L-CNT.
*
*  線出力
     IF       L-CNT  >=  58
              PERFORM  MIDASI-SEC
     END-IF.
     WRITE    P-REC     FROM  SEN2  AFTER 1.
     ADD      1         TO    L-CNT.
*
 SOUGOKEI-EXIT.
     EXIT.
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     IF  PARA-LIST-KBN = "1"
         IF      L-CNT   NOT =  99
* 商品合計出力
                 PERFORM   DENKEI-SEC
* 商品合計出力
                 PERFORM   TENKEI-SEC
* 総合計出力
                 PERFORM   SOUGOKEI-SEC
         END-IF
     END-IF.
*
     DISPLAY  MSG-END   UPON CONS.
*
     CLOSE    NFNJYRF3  NFNJYRF2  PRINTF.
*ＣＳＶの場合はＣＳＶファイルをＯＰＥＮする。
     IF  PARA-LIST-KBN  = "2"
         MOVE   OUTPUT-CNT        TO   PARA-LIST-KENSU
         CLOSE  NFJYRTXT
     ELSE
         MOVE   ZERO              TO   PARA-LIST-KENSU
     END-IF.
*
 END-EXIT.
     EXIT.
*

```
