# SJR0470V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJR0470V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受領返品　　　　　　　            *
*    サブシステム　　　　：　返品自動計上　　　　　　　　      *
*    モジュール名　　　　：　返品自動計上確認リストＣＳＶ出力　*
*    　　　　　　　　　　　　（通常／纏め共通）　　　　　　　　*
*    作成日／作成者　　　：　2021/11/01 INOUE                  *
*    処理概要　　　　　　：　パラメタを受取り、指定条件の返品　*
*                            計上確認リストＣＳＶを出力する。　*
*    更新日／更新者　　　：　                                  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SJR0470V.
*                  流用:SJR0370V.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.         CONSOLE   IS        CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<返品抽出データ >>*********************************
     SELECT   AUTWHEL1           ASSIGN    TO   DA-01-VI-AUTWHEL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    WHE-F01
                                                WHE-F03
                                                WHE-F02
                                                WHE-F04
                                                WHE-F05
                                 STATUS         WHE-STATUS.
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
****<<担当者ファイル　　　　 >>*********************************
     SELECT   TANMS1             ASSIGN    TO   DA-01-VI-TANMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TAN-F01  TAN-F02
                                 STATUS         TAN-STATUS.
*
*****<<返品CSVデータ >>*************************************
     SELECT   AUTHPCSV           ASSIGN    TO   AUTHPCSV
                                 STATUS         CSV-STATUS.
*                                                              *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ケーヨー　返品抽出データ　　　　　　               *
*--------------------------------------------------------------*
 FD  AUTWHEL1           LABEL RECORD   IS   STANDARD.
     COPY     COMWHEF   OF        XFDLIB
              JOINING   WHE       PREFIX.
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
*    FILE = 担当者ファイル　　　　　　　　                     *
*--------------------------------------------------------------*
 FD  TANMS1              LABEL RECORD   IS   STANDARD.
     COPY     TANMS1     OF        XFDLIB
              JOINING    TAN       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 返品データＣＳＶ　　　　　　　　                   *
*--------------------------------------------------------------*
 FD  AUTHPCSV           BLOCK CONTAINS 1    RECORDS.
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
 01  TANMS1-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  RD-TOK-FLG             PIC  X(01)  VALUE  SPACE.
 01  RD-TEN-FLG             PIC  X(01)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  WHE-STATUS             PIC  X(02).
 01  TEN-STATUS             PIC  X(02).
 01  JYO-STATUS             PIC  X(02).
 01  TOK-STATUS             PIC  X(02).
 01  TAN-STATUS             PIC  X(02).
 01  CSV-STATUS             PIC  X(02).
*
 01  BRK-KEY.
     03  BRK-TORICD         PIC  9(08)  VALUE  ZERO.
     03  BRK-TENCD          PIC  9(05)  VALUE  ZERO.
     03  BRK-DENNO          PIC  9(09)  VALUE  ZERO.
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
     03  WK-SURYO-DEN       PIC S9(06)V9.
     03  WK-KINGAKU-DEN     PIC S9(09).
***** カウンタ
 01  READ-CNT               PIC  9(07)  VALUE  ZERO.
 01  OUTPUT-CNT             PIC  9(07)  VALUE  ZERO.
*タイトルエリア
*見出しエリア
 01  WK-HEAD1.
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(16)  VALUE
                       NC"【自動】　＜返品計上確認リスト＞".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  HD01-JIKKBN        PIC  N(08).
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
     03  FILLER             PIC  N(05)  VALUE  NC"計上区分：".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-KJKBN         PIC  X(01).
     03  HD02-KJKBNC        PIC  X(01).
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  HD02-KJKBNNM       PIC  N(04).
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"担当者：".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-TFROM         PIC  9(02)  VALUE  ZERO.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(01)  VALUE  NC"～".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-TTO           PIC  9(02)  VALUE  ZERO.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"検収日：".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-KFROM         PIC  9(08)  VALUE  ZERO.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(01)  VALUE  NC"～".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-KTO           PIC  9(08)  VALUE  ZERO.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"入力日：".
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
     03  FILLER             PIC  N(04)  VALUE  NC"計上日：".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-KJFROM        PIC  9(08)  VALUE  ZERO.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(01)  VALUE  NC"～".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-KJTO          PIC  9(08)  VALUE  ZERO.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"作成日：".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-SFROM         PIC  9(08)  VALUE  ZERO.
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(01)  VALUE  NC"～".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  HD02-STO           PIC  9(08)  VALUE  ZERO.
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
     03  FILLER             PIC  N(04)  VALUE  NC"伝票区分".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(03)  VALUE  NC"検収日".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"実検収日".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(03)  VALUE  NC"入力日".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(03)  VALUE  NC"計上日".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"出荷場所".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(05)  VALUE  NC"入力者ＣＤ".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"入力者名".
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
     03  FILLER             PIC  N(04)  VALUE  NC"原価単価".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"原価金額".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  FILLER             PIC  X(01)  VALUE  ",".
     03  FILLER             PIC  X(01)  VALUE  X"28".
     03  FILLER             PIC  N(04)  VALUE  NC"明細備考".
     03  FILLER             PIC  X(01)  VALUE  X"29".
     03  HEAD3-01           PIC  X(01)  VALUE  SPACE.
     03  HEAD3-02           PIC  X(01)  VALUE  SPACE.
     03  HEAD3-03           PIC  N(04)  VALUE  SPACE.
     03  HEAD3-04           PIC  X(01)  VALUE  SPACE.
     03  HEAD3-05           PIC  X(01)  VALUE  SPACE.
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
         05  MD01-TENMEI    PIC  N(15).
         05  CSV-A03B       PIC  X(01).
         05  CSV-A04        PIC  X(01).
         05  MD01-DENNO     PIC  9(09).
         05  CSV-A05        PIC  X(01).
         05  MD01-GYONO     PIC  X(02).
     03  CSV-A06            PIC  X(01).
     03  MD01-DKBN          PIC  X(02).
     03  MD01-DKBNC         PIC  X(01).
     03  CSV-A06A           PIC  X(01).
     03  MD01-DKBNNM        PIC  N(04).
     03  CSV-A06B           PIC  X(01).
     03  CSV-A07            PIC  X(01).
     03  MD01-KENSHBI       PIC  9(08).
     03  CSV-A08            PIC  X(01).
     03  MD01-JIKENSHBI     PIC  9(08).
     03  CSV-A09            PIC  X(01).
     03  MD01-NYRYOKUBI     PIC  9(08).
     03  CSV-A10            PIC  X(01).
     03  MD01-KEIJYOBI      PIC  9(08).
     03  CSV-A11            PIC  X(01).
     03  MD01-SHUKABHO      PIC  X(02).
     03  CSV-A12            PIC  X(01).
     03  MD01-NYRYOKUCD     PIC  X(02).
     03  CSV-A13            PIC  X(01).
     03  CSV-A13A           PIC  X(01).
     03  MD01-NYRYOKUNM     PIC  N(10).
     03  CSV-A13B           PIC  X(01).
     03  CSV-A14            PIC  X(01).
     03  MD01-ASHOCD        PIC  X(13).
     03  CSV-A15            PIC  X(01).
     03  MD01-SHOCD         PIC  X(08).
     03  MD01-HINTAN1       PIC  X(05).
     03  MD01-HINTAN2       PIC  X(02).
     03  MD01-HINTAN3       PIC  X(01).
     03  CSV-A16            PIC  X(01).
     03  MD01-SHONM1        PIC  X(15).
     03  MD01-SHONM2        PIC  X(15).
     03  CSV-A17            PIC  X(01).
     03  MD01-SURYO         PIC  -------9.9.
     03  CSV-A18            PIC  X(01).
     03  MD01-GENKA         PIC  -------9.99.
     03  CSV-A19            PIC  X(01).
     03  MD01-KINGAKU       PIC  ---------9.
     03  CSV-A20            PIC  X(01).
     03  MD01-MEISAIBK      PIC  X(10).
 01  WK-MEISAI2.
     03  CSV-DENNO          PIC  X(92).
     03  GK01-DENGYO        PIC  X(02).
     03  CSV-B01            PIC  X(01).
     03  CSV-B01A           PIC  X(01).
     03  GK01-DENMEIT1      PIC  N(04).
     03  CSV-B01B           PIC  X(01).
     03  GK01-DENMEI1       PIC  X(15).
     03  GK01-DENMEI2       PIC  X(15).
     03  CSV-B02A           PIC  X(01).
     03  GK01-DENMEIT2      PIC  N(01).
     03  CSV-B02B           PIC  X(01).
     03  CSV-B03            PIC  X(01).
     03  CSV-B04            PIC  X(01).
     03  CSV-B05            PIC  X(01).
     03  CSV-B06            PIC  X(01).
     03  CSV-B07            PIC  X(01).
     03  CSV-B08            PIC  X(01).
     03  CSV-B09            PIC  X(01).
     03  CSV-B10            PIC  X(01).
     03  CSV-B11            PIC  X(01).
     03  CSV-B12            PIC  X(01).
     03  CSV-B12A           PIC  X(01).
     03  GK01-TAITOL        PIC  N(04).
     03  CSV-B12B           PIC  X(01).
     03  CSV-B13            PIC  X(01).
     03  GK01-SURYO         PIC  -------9.9.
     03  CSV-B14            PIC  X(01).
     03  CSV-B15            PIC  X(01).
     03  GK01-KINGAKU       PIC  ---------9.
     03  CSV-B16            PIC  X(01).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR0470V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0470V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER         PIC   X(04)  VALUE
                       "### ".
         05  ERR-PG-ID      PIC   X(08)  VALUE
                       "SJR0470V".
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
 01  PARA-JIKKBN            PIC   X(01).
 01  PARA-KEIKBN            PIC   X(01).
 01  PARA-TOKCD             PIC   9(08).
 01  PARA-TANST             PIC   X(02).
 01  PARA-TANED             PIC   X(02).
 01  PARA-DKBN              PIC   X(01).
 01  PARA-AFROM             PIC   9(08).
 01  PARA-ATO               PIC   9(08).
 01  PARA-NFROM             PIC   9(08).
 01  PARA-NTO               PIC   9(08).
 01  PARA-KFROM             PIC   9(08).
 01  PARA-KTO               PIC   9(08).
 01  PARA-SFROM             PIC   9(08).
 01  PARA-STO               PIC   9(08).
*
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
 PROCEDURE              DIVISION USING PARA-JIKKBN
                                       PARA-KEIKBN
                                       PARA-TOKCD
                                       PARA-TANST
                                       PARA-TANED
                                       PARA-DKBN
                                       PARA-AFROM
                                       PARA-ATO
                                       PARA-NFROM
                                       PARA-NTO
                                       PARA-KFROM
                                       PARA-KTO
                                       PARA-SFROM
                                       PARA-STO.
*
****************************************************************
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           AUTWHEL1.
     MOVE     "AUTWHEL1"          TO        ERR-FL-ID.
     MOVE     WHE-STATUS          TO        ERR-STCD.
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
                        PROCEDURE TANMS1.
     MOVE     "TANMS1  "          TO        ERR-FL-ID.
     MOVE     TAN-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC6         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE AUTHPCSV.
     MOVE     "AUTHPCSV"          TO        ERR-FL-ID.
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
 SJR0470V-START         SECTION.
*
     MOVE   "SJR0470V-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     IF   END-FLG  NOT =  "END"
          PERFORM    MAIN-SEC  UNTIL  END-FLG   =  "END"
     END-IF.
     PERFORM            END-SEC.
     STOP               RUN.
*
 SJR0470V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     AUTWHEL1.
     OPEN     INPUT     TOKMS2.
     OPEN     INPUT     TENMS1.
     OPEN     INPUT     JYOKEN1.
     OPEN     INPUT     TANMS1.
     OPEN     OUTPUT    AUTHPCSV.
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
     MOVE  SPACE                TO   WHE-REC.
     INITIALIZE                      WHE-REC.
*
     PERFORM  AUTWHEL1-RD-SEC.
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
*伝票纏め時制御
     IF  PARA-JIKKBN  =  "1"
         MOVE "("                 TO   HEAD3-01
         MOVE X"28"               TO   HEAD3-02
         MOVE NC"伝票番号"        TO   HEAD3-03
         MOVE X"29"               TO   HEAD3-04
         MOVE ")"                 TO   HEAD3-05
     ELSE
         MOVE SPACE               TO   HEAD3-01 HEAD3-02 HEAD3-03
                                       HEAD3-04 HEAD3-05
     END-IF.
*
     MOVE     WK-HEAD3            TO   CSV-REC.
     WRITE    CSV-REC.
     ADD      3                   TO   OUTPUT-CNT.
*　ブレイクキー設定
     MOVE     WHE-F02             TO   BRK-TENCD.
     MOVE     WHE-F04             TO   BRK-DENNO.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ケーヨー　返品抽出データ読み込み　　　
****************************************************************
 AUTWHEL1-RD-SEC            SECTION.
*
     MOVE    "AUTWHEL1-RD-SEC"    TO   S-NAME.
*
     READ     AUTWHEL1
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    AUTWHEL1-RD-EXIT
     END-READ.
*
     ADD   1      TO  READ-CNT.
*
 AUTWHEL1-RD-EXIT.
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
***************************************************************
*             担当者ファイル読込
***************************************************************
 TANMS1-READ-SEC       SECTION.
*
     MOVE    "TANMS1-READ-SEC" TO        S-NAME.
*
     READ     TANMS1
              INVALID      MOVE  "INV"    TO   TANMS1-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   TANMS1-INV-FLG
     END-READ.
*
 TANMS1-READ-EXIT.
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
                                       CSV-A06A  CSV-A13A.
     MOVE   X"29"                 TO   CSV-A01B  CSV-A03B
                                       CSV-A06B  CSV-A13B.
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
                                       CSV-A20.
*
*  店舗・伝票番号ブレイク時
     IF      ( WHE-F02   NOT =   BRK-TENCD )  OR
             ( WHE-F04   NOT =   BRK-DENNO )
*  合計
              PERFORM   DENKEI-SEC
              MOVE   ZERO         TO    WK-DENKEI
              INITIALIZE                WK-DENKEI
              IF  ( WHE-F02   NOT =   BRK-TENCD )
                  MOVE  SPACE         TO    RD-TEN-FLG
              END-IF
              MOVE  WHE-F02       TO    BRK-TENCD
              MOVE  WHE-F04       TO    BRK-DENNO
              MOVE  SPACE         TO    RD-TOK-FLG
     END-IF.
*  明細行編集１
*  取引先ＣＤ
     MOVE     WHE-F01           TO    MD01-TORICD.
*  取引先正式名
     MOVE     WHE-F01           TO    TOK-F01.
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
     MOVE     WHE-F02           TO    MD01-TENCD
*  店舗正式名
     MOVE     WHE-F02           TO    TEN-F011
     MOVE     WHE-F01           TO    TEN-F52
     IF  RD-TEN-FLG = SPACE
         PERFORM  TENMS1-READ-SEC
         IF  TENMS1-INV-FLG = "INV"
           MOVE ALL NC"＊"      TO    MD01-TENMEI  BRK-TENMEI
         ELSE
           MOVE TEN-F02         TO    MD01-TENMEI  BRK-TENMEI
         END-IF
         MOVE  "1"              TO    RD-TEN-FLG
     ELSE
         MOVE  BRK-TENMEI       TO    MD01-TENMEI
     END-IF.
*  伝票番号
     MOVE     WHE-F04           TO    MD01-DENNO.
*  行番号
     MOVE     WHE-F05           TO    MD01-GYONO.
*  伝区
     MOVE     WHE-F23           TO    MD01-DKBN.
     MOVE     ":"               TO    MD01-DKBNC.
*  伝区名称
     MOVE     1                    TO        JYO-F01.
     MOVE     WHE-F23              TO        JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF   JYOKEN1-INV-FLG = "INV"
          MOVE  ALL NC"＊"         TO        MD01-DKBNNM
     ELSE
          MOVE  JYO-F03            TO        MD01-DKBNNM
     END-IF.
*  検収日
     MOVE     WHE-F03              TO        MD01-KENSHBI.
*  実検収日
     MOVE     WHE-F07              TO        MD01-JIKENSHBI.
*  入力日
     MOVE     WHE-F83              TO        MD01-NYRYOKUBI.
*  計上日
     MOVE     WHE-F86              TO        MD01-KEIJYOBI.
*  出荷場所
     MOVE     WHE-F08              TO        MD01-SHUKABHO.
*  入力者ＣＤ
     MOVE     WHE-F82              TO        MD01-NYRYOKUCD.
*  担当者名
     MOVE     WHE-F81              TO        TAN-F01
     MOVE     WHE-F82              TO        TAN-F02
     PERFORM  TANMS1-READ-SEC.
     IF  TANMS1-INV-FLG = "INV"
         MOVE ALL NC"＊"           TO        MD01-NYRYOKUNM
     ELSE
         MOVE TAN-F03              TO        MD01-NYRYOKUNM
     END-IF.
*  相手先商品ＣＤ
     MOVE     WHE-F09              TO        MD01-ASHOCD.
*  商品ＣＤ
     MOVE     WHE-F10              TO        MD01-SHOCD.
*  品単１
     MOVE     WHE-F11              TO        MD01-HINTAN1.
*  品単２
     MOVE     WHE-F12              TO        MD01-HINTAN2.
*  品単３
     MOVE     WHE-F13              TO        MD01-HINTAN3.
*  商品名
     MOVE     WHE-F14              TO        MD01-SHONM1.
     MOVE     WHE-F15              TO        MD01-SHONM2.
*  数量　
     MOVE     WHE-F16              TO        MD01-SURYO.
*  原価単価
     MOVE     WHE-F17              TO        MD01-GENKA.
*  原価金額
     MOVE     WHE-F18              TO        MD01-KINGAKU.
*  明細備考
     MOVE     WHE-F21              TO        MD01-MEISAIBK.
*  伝票備考
     MOVE     WHE-F24              TO        GK01-DENMEI1.
     MOVE     WHE-F25              TO        GK01-DENMEI2.
*--------------
*  伝票計加算
*--------------
*  数量　
     COMPUTE  WK-SURYO-DEN    = WK-SURYO-DEN    +  WHE-F16.
*  原価金額
     COMPUTE  WK-KINGAKU-DEN  = WK-KINGAKU-DEN  +  WHE-F18.
*レコードセット
     MOVE     WK-MEISAI1           TO        CSV-REC.
     MOVE     WK-DENMEISAI         TO        CSV-DENNO.
*
     WRITE    CSV-REC.
     ADD      1                    TO        OUTPUT-CNT.
*    次レコード読込み
     PERFORM  AUTWHEL1-RD-SEC.
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
*計上区分
     MOVE     PARA-KEIKBN         TO   HD02-KJKBN.
     MOVE     ":"               TO   HD02-KJKBNC.
     EVALUATE   PARA-KEIKBN
          WHEN   "1"
              MOVE     NC"未確認"         TO   HD02-KJKBNNM
          WHEN   "2"
              MOVE     NC"未計上"         TO   HD02-KJKBNNM
          WHEN   "3"
              MOVE     NC"計上済"         TO   HD02-KJKBNNM
          WHEN   OTHER
              MOVE     NC"取消分"         TO   HD02-KJKBNNM
     END-EVALUATE.
*担当者
     MOVE     PARA-TANST        TO   HD02-TFROM.
     MOVE     PARA-TANED        TO   HD02-TTO.
*実行区分
     IF    PARA-JIKKBN  =  SPACE
           MOVE NC"（　通　常　）　"  TO  HD01-JIKKBN
     ELSE
           MOVE NC"（伝票まとめ分）"  TO  HD01-JIKKBN
     END-IF.
*日付区分・日付
     EVALUATE   PARA-DKBN
          WHEN   "1"
              MOVE     PARA-AFROM         TO   HD02-KFROM
              MOVE     PARA-ATO           TO   HD02-KTO
          WHEN   "2"
              MOVE     PARA-NFROM         TO   HD02-NFROM
              MOVE     PARA-NTO           TO   HD02-NTO
          WHEN   "3"
              MOVE     PARA-KFROM         TO   HD02-KJFROM
              MOVE     PARA-KTO           TO   HD02-KJTO
          WHEN   "4"
              MOVE     PARA-SFROM         TO   HD02-SFROM
              MOVE     PARA-STO           TO   HD02-STO
          WHEN   OTHER
              MOVE     ZERO               TO   HD02-KJFROM
                                               HD02-KFROM
                                               HD02-NFROM
                                               HD02-KJTO
                                               HD02-KTO
                                               HD02-NTO
     END-EVALUATE.
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
     MOVE     X"28"               TO   CSV-B01A  CSV-B02A
                                       CSV-B12A.
     MOVE     X"29"               TO   CSV-B01B  CSV-B02B
                                       CSV-B12B.
*
     MOVE     ","                 TO   CSV-B01   CSV-B03
                                       CSV-B04   CSV-B05
                                       CSV-B06   CSV-B07
                                       CSV-B08   CSV-B09
                                       CSV-B10   CSV-B11
                                       CSV-B12   CSV-B13
                                       CSV-B14   CSV-B15
                                       CSV-B16.
*--------------
*  合計転送
*--------------
*  タイトル転送
     MOVE     NC"＜備考："       TO   GK01-DENMEIT1.
     MOVE     NC"＞"             TO   GK01-DENMEIT2.
     MOVE     NC"＜合計＞"       TO   GK01-TAITOL.
*  行
*****MOVE     "80"               TO   GK01-DENGYO.
     MOVE     "  "               TO   GK01-DENGYO.
*  数量　
     MOVE     WK-SURYO-DEN       TO   GK01-SURYO.
*  原価金額
     MOVE     WK-KINGAKU-DEN     TO   GK01-KINGAKU.
*
*伝票合計出力
*レコードセット
     MOVE     WK-MEISAI2         TO   CSV-REC.
*
     WRITE    CSV-REC.
     ADD      1                  TO   OUTPUT-CNT.
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
     CLOSE    AUTWHEL1 TOKMS2 TENMS1 JYOKEN1 TANMS1 AUTHPCSV.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.

```
