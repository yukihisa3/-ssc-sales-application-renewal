****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受領返品　　　　                  *
*    モジュール名　　　　：　共通受領書発行　　　　　　　      *
*    作成日／作成者　　　：　2017/09/22 INOUE                  *
*    処理概要　　　　　　：　取引先共通受領書を出力する。　　  *
*    流用　　　　　　　　：　SJR0260L                          *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SJR0260L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2017/09/22.
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
****<<受領書印刷ワーク >>*********************************
     SELECT   COMJRWK1           ASSIGN    TO   DA-01-VI-COMJRWK1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JYW-F03
                                                JYW-F07
                                                JYW-F08
                                                JYW-F51
                                                JYW-F04
                                                JYW-F31
                                 STATUS         JYW-STATUS.
*
****<<取引先マスタ　　　　　 >>*********************************
     SELECT   TOKMS2             ASSIGN    TO   DA-01-VI-TOKMS2
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TOK-F01
                                 STATUS         TOK-STATUS.
*
*****<<  プリント　Ｆ   >>**************************************
     SELECT   PRINTF    ASSIGN    TO        LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = 受領書印刷ワーク　　　　　　　　　　               *
*--------------------------------------------------------------*
 FD  COMJRWK1           LABEL RECORD   IS   STANDARD.
     COPY     COMJRWK1  OF        XFDLIB
              JOINING   JYW       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 取引先マスタ　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     TOKMS2    OF        XFDLIB
              JOINING   TOK       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = プリントファイル                                   *
*--------------------------------------------------------------*
 FD  PRINTF.
 01  P-REC                        PIC       X(200).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  TOKMS2-INV-FLG               PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  JYW-STATUS                   PIC       X(02).
 01  TOK-STATUS                   PIC       X(02).
 01  PRT-STATUS                   PIC       X(02).
*
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
 01  WK-AREA.
     03  IX1                      PIC       9(02)  VALUE  ZERO.
     03  SET-FLG                  PIC       X(01)  VALUE  SPACE.
     03  RD1-FLG                  PIC       X(01)  VALUE  SPACE.
     03  RD2-FLG                  PIC       X(01)  VALUE  SPACE.
*
***** カウンタ
 01  P-CNT                        PIC       9(04)  VALUE  ZERO.
 01  L-CNT                        PIC       9(03)  VALUE  ZERO.
 01  CNT-READ                     PIC       9(06)  VALUE  ZERO.
 01  MEI-CNT                      PIC       9(05)  VALUE  ZERO.
*
 01  WK-BACHINO.
     03  WK-BACHI-YMD             PIC       9(08)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  WK-BACHI-TIME            PIC       9(04)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  WK-BACHI-TORICD          PIC       9(08)  VALUE  ZERO.
*
 01  BRK-KEY.
     03  BRK-TENCD                PIC       9(05)  VALUE  ZERO.
     03  BRK-KDATE                PIC       9(08)  VALUE  ZERO.
     03  BRK-DENKU                PIC       X(02)  VALUE  SPACE.
     03  BRK-DENNO                PIC       9(10)  VALUE  ZERO.
 01  WRT-KEY.
     03  WRT-TENCD                PIC       9(05)  VALUE  ZERO.
     03  WRT-KDATE                PIC       9(08)  VALUE  ZERO.
     03  WRT-DENKU                PIC       X(02)  VALUE  SPACE.
     03  WRT-DENNO                PIC       9(10)  VALUE  ZERO.
*
 01  WK-AREA.
     03  WK-SHUKASU               PIC      S9(06).
     03  WK-KENSHUSU              PIC      S9(06).
     03  WK-GENKA                 PIC      S9(09).
     03  WK-SASU                  PIC      S9(07).
     03  WK-SHUKASUR              PIC      S9(05)V9(01).
     03  WK-KENSHUSUR             PIC      S9(05)V9(01).
     03  WK-GENKAR                PIC      S9(07)V9(02).
     03  WK-KINGAKU               PIC      S9(10).
     03  WK-SASUR                 PIC      S9(05)V9(02).
*
*--------------
 01  WK-DENKEI.
     03  WK-SURYOU-DEN      PIC   9(07).
     03  WK-KINGAKU-DEN     PIC   9(09).
*
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR0260L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0260L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SJR0260L".
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
     03  FILLER                PIC  X(08)  VALUE  "SJR0260L".
     03  FILLER                PIC  X(24)  VALUE  SPACE.
     03  FILLER                CHARACTER  TYPE  IS PITCH-30.
         05  FILLER            PIC  N(03)  VALUE NC"＊＊＊".
         05  FILLER            PIC  N(01)  VALUE NC"　".
         05  FILLER            PIC  N(03)  VALUE NC"受領書".
         05  FILLER            PIC  N(01)  VALUE NC"　".
         05  FILLER            PIC  N(04)  VALUE NC"＊＊＊　".
     03  HD01-HENKU            PIC  N(04)
                               CHARACTER  TYPE  IS PITCH-30.
     03  FILLER                PIC  X(25)  VALUE  SPACE.
     03  HD01-YY               PIC  9999.
     03  FILLER                PIC  N(01)  VALUE  NC"年"
                               CHARACTER  TYPE  IS PITCH-20.
     03  HD01-MM               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"月"
                               CHARACTER  TYPE IS PITCH-20.
     03  HD01-DD               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"日"
                               CHARACTER  TYPE IS PITCH-20.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD01-PCNT             PIC  ZZ9.
     03  FILLER                PIC  N(02)  VALUE  NC"頁"
                               CHARACTER  TYPE IS PITCH-20.
***** 見出し行２
*01  HD02.
*    03  FILLER                PIC  X(120)  VALUE  SPACE.
*    03  HD02-HH               PIC  99.
*    03  FILLER                PIC  N(01)  VALUE  NC"："
*                              CHARACTER  TYPE  IS PITCH-20.
*    03  HD02-MN               PIC  Z9.
*    03  FILLER                PIC  N(01)  VALUE  NC"："
*                              CHARACTER  TYPE IS PITCH-20.
*    03  HD02-SS               PIC  Z9.
***** 見出し行３
 01  HD03.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(05)  VALUE  NC"日付区分："
                               CHARACTER  TYPE IS PITCH-20.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
*    03  HD03-DKBN             PIC  X(01).
*    03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD03-DKBNNM           PIC  N(03)
                               CHARACTER  TYPE IS PITCH-20.
***** 見出し行４
 01  HD04.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                CHARACTER   TYPE IS   PITCH-20.
         05  FILLER            PIC  N(01)  VALUE  NC"受".
         05  FILLER            PIC  X(01)  VALUE    " ".
         05  FILLER            PIC  N(01)  VALUE  NC"信".
         05  FILLER            PIC  X(01)  VALUE    " ".
         05  FILLER            PIC  N(02)  VALUE  NC"日：".
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD04-JFYY             PIC  X(04)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-JFMM             PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-JFDD             PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(01)  VALUE  NC"～"
                               CHARACTER  TYPE IS PITCH-20.
     03  HD04-JTYY             PIC  X(04)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-JTMM             PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-JTDD             PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"計上日："
                               CHARACTER   TYPE IS   PITCH-20.
     03  HD04-KFYY             PIC  X(04)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-KFMM             PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-KFDD             PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(01)  VALUE  NC"～"
                               CHARACTER  TYPE IS PITCH-20.
     03  HD04-KTYY             PIC  X(04)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-KTMM             PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-KTDD             PIC  X(02)  VALUE  SPACE.
***** 見出し行５
*01  HD05.
*    03  FILLER                PIC  X(01)  VALUE  SPACE.
*    03  FILLER                PIC  N(05)  VALUE  NC"伝票区分："
*                              CHARACTER  TYPE IS PITCH-15.
*    03  FILLER                PIC  X(01)  VALUE  SPACE.
*    03  HD05-DENK1            PIC  X(02).
*    03  FILLER                PIC  X(01)  VALUE  SPACE.
*    03  HD05-DENK2            PIC  X(02).
*    03  FILLER                PIC  X(01)  VALUE  SPACE.
*    03  HD05-DENK3            PIC  X(02).
*    03  FILLER                PIC  X(01)  VALUE  SPACE.
*    03  HD05-DENK4            PIC  X(02).
*    03  FILLER                PIC  X(01)  VALUE  SPACE.
*    03  HD05-DENK5            PIC  X(02).
***** 見出し行６
*01  HD06.
*    03  FILLER               PIC  X(01)  VALUE  SPACE.
*    03  FILLER               PIC  N(04)  VALUE  NC"店舗情報"
*                             CHARACTER  TYPE IS PITCH-20.
***** 見出し行７
 01  HD07.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE  NC"バッチ日付"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"時刻"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"取引先"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(28)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"伝票番号"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE  NC"元伝票番号"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"相手伝区"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"店舗Ｃ"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"計上日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"分類ＣＤ"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"訂区"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"自社伝区"
                              CHARACTER  TYPE IS PITCH-15.
***** 見出し行８
 01  HD08.
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"行"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"赤黒"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(07)
                              VALUE  NC"相手商品コード"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"商品名"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(35)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"数量"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"原価単価"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(05)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"原価金額"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"返品理由"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"訂区"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(07)
                              VALUE  NC"自社商品コード"
                              CHARACTER  TYPE IS PITCH-20.
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
***** 空白行
 01  SEN3.
     03  FILLER                   PIC       X(136)  VALUE SPACE.
*
***** 明細行
 01  MD01                         CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-BDATEYYYY           PIC       9999.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  MD01-BDATEMM             PIC       99.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  MD01-BDATEDD             PIC       99.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-BTIMEHH             PIC       99.
     03  FILLER                   PIC       X(01)  VALUE  ":".
     03  MD01-BTIMEMM             PIC       99.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TOKCD               PIC       9(08).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TOKNM               PIC       N(16).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-DENNO               PIC       9999999999.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-MOTODENNO           PIC       9999999999.
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  MD01-AITEDENKU           PIC       X(02).
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  MD01-TENCD               PIC       99999.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-KDATEYYYY           PIC       9999.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  MD01-KDATEMM             PIC       99.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  MD01-KDATEDD             PIC       99.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-BUNRUI              PIC       X(10).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-TEISEIH             PIC       X(01).
     03  FILLER                   PIC       X(05)  VALUE  SPACE.
     03  MD01-JIDENKU             PIC       9(02).
*
 01  MD02                         CHARACTER   TYPE IS   PITCH-20.
     03  FILLER                   PIC       X(04)  VALUE  SPACE.
     03  MD02-GYO                 PIC       Z9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-AKAKURO             PIC       N(01).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-AITESYOCD           PIC       X(14).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-AITESYONM1          PIC       X(35).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-SURYOU              PIC       Z,ZZZ,ZZ9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-GTANKA              PIC       ZZ,ZZZ,ZZ9.99.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-GKINGAKU            PIC       ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-RIYUU               PIC       X(03).
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  MD02-TEISEIM             PIC       X(04).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       X(01)  VALUE  "(".
     03  MD02-JISYOCD             PIC       X(08).
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  MD02-HINTAN1             PIC       X(05).
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  MD02-HINTAN2             PIC       X(02).
     03  FILLER                   PIC       X(02)  VALUE  "-".
     03  MD02-HINTAN3             PIC       X(01).
     03  FILLER                   PIC       X(01)  VALUE  ")".
 01  MD03.
     03  FILLER                   PIC       X(27)  VALUE  SPACE.
     03  MD03-AITESYONM2          PIC       X(35).
*
***** 合計行
 01  GK01.
     03  FILLER                   PIC     X(52)  VALUE  SPACE.
     03  FILLER                   PIC     X(02)  VALUE  "( ".
     03  GK01-TAITOL              PIC     N(03)
                                  VALUE   NC"合　計"
                                  CHARACTER   TYPE IS   PITCH-20.
     03  FILLER                   PIC     X(03)  VALUE  SPACE.
     03  GK01-SURYOU              PIC     Z,ZZZ,ZZ9.
     03  FILLER                   PIC     X(16)  VALUE  SPACE.
     03  GK01-KINGAKU             PIC     ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC     X(02)  VALUE  " )".
*
****************************************************************
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
****************************************************************
 LINKAGE                SECTION.
 01  PARA-IN-DATEKBN        PIC   X(01).
 01  PARA-IN-FROMDATE       PIC   9(08).
 01  PARA-IN-TODATE         PIC   9(08).
 01  PARA-IN-HNPKBN         PIC   X(01).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-IN-DATEKBN
                                           PARA-IN-FROMDATE
                                           PARA-IN-TODATE
                                           PARA-IN-HNPKBN.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           COMJRWK1.
     MOVE     "COMJRWK1"          TO        ERR-FL-ID.
     MOVE     JYW-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           TOKMS2.
     MOVE     "TOKMS2  "          TO        ERR-FL-ID.
     MOVE     TOK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           PRINTF.
     MOVE     "PRINTF  "          TO        ERR-FL-ID.
     MOVE     PRT-STATUS          TO        ERR-STCD.
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
 SJR0260L-START         SECTION.
*
     MOVE   "SJR0260L-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
*
     IF    END-FLG    NOT =  "END"
           PERFORM    MAIN-SEC  UNTIL     END-FLG   =  "END"
     END-IF.
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SJR0260L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     COMJRWK1.
     OPEN     INPUT     TOKMS2.
     OPEN     OUTPUT    PRINTF.
*
     MOVE     ZERO           TO    WK-DENKEI.
     INITIALIZE                    WK-DENKEI.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     99             TO    L-CNT.
*    MOVE     1              TO    PRT-FLG.
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
            MOVE    ZERO        TO   SYS-DATEW
     END-IF.
     ACCEPT SYSTEM-TIME         FROM TIME.
*
     MOVE  SPACE                TO   JYW-REC.
     INITIALIZE                      JYW-REC.
*
     START  COMJRWK1  KEY  >=   JYW-F03  JYW-F07  JYW-F08
                                JYW-F51  JYW-F04  JYW-F31
         INVALID   KEY
            MOVE     "END"      TO   END-FLG
            DISPLAY NC"＃対象データ無し１＃" UPON CONS
            GO                  TO   INIT-EXIT
     END-START
*
     PERFORM  COMJRWK1-RD-SEC.
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
     PERFORM   MIDASISET-SEC.
*　ブレイクキー設定
     MOVE  JYW-F07    TO       BRK-TENCD.
     MOVE  JYW-F08    TO       BRK-KDATE.
     MOVE  JYW-F51    TO       BRK-DENKU.
     MOVE  JYW-F04    TO       BRK-DENNO.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    受領書印刷ワーク読み込み　　　
****************************************************************
 COMJRWK1-RD-SEC            SECTION.
*
     MOVE    "COMJRWK1-RD-SEC"    TO   S-NAME.
*
     READ     COMJRWK1
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    COMJRWK1-RD-EXIT
     END-READ.
*
*
 COMJRWK1-RD-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO    S-NAME.
*
 MAIN-01.
*  店舗・伝票番号ブレイク時
     IF      ( JYW-F07   NOT =   BRK-TENCD )  OR
             ( JYW-F08   NOT =   BRK-KDATE )  OR
             ( JYW-F51   NOT =   BRK-DENKU )  OR
             ( JYW-F04   NOT =   BRK-DENNO )
*        合計
              PERFORM   DENKEI-SEC
              MOVE   ZERO         TO    WK-DENKEI
              INITIALIZE                WK-DENKEI
              IF  ( JYW-F07   NOT =   BRK-TENCD )
                  MOVE  SPACE         TO    RD1-FLG
              END-IF
              MOVE  JYW-F07       TO    BRK-TENCD
              MOVE  JYW-F08       TO    BRK-KDATE
              MOVE  JYW-F51       TO    BRK-DENKU
              MOVE  JYW-F04       TO    BRK-DENNO
              MOVE  SPACE         TO    RD2-FLG
     END-IF.
*
 MAIN-02.
*明細行編集１--------------------------------------------------
*  バッチ日付
     MOVE     JYW-F01(1:4)         TO        MD01-BDATEYYYY.
     MOVE     JYW-F01(5:2)         TO        MD01-BDATEMM.
     MOVE     JYW-F01(7:2)         TO        MD01-BDATEDD.
*  バッチ時刻
     MOVE     JYW-F02(3:2)         TO        MD01-BTIMEHH.
     MOVE     JYW-F02(5:2)         TO        MD01-BTIMEMM.
*  取引先CD
     MOVE     JYW-F03              TO        MD01-TOKCD  TOK-F01.
*  取引先名
     PERFORM  TOKMS2-READ-SEC.
     IF       TOKMS2-INV-FLG = "INV"
              MOVE ALL NC"＊"   TO    MD01-TOKNM
     ELSE
              MOVE TOK-F02      TO    MD01-TOKNM
     END-IF.
     MOVE  "1"              TO    RD1-FLG.
*    IF  RD1-FLG = SPACE
*        PERFORM  TOKMS2-READ-SEC
*        IF  TOKMS2-INV-FLG = "INV"
*          MOVE ALL NC"＊"      TO    MD01-TENMEI
*        ELSE
*          MOVE TOK-F02         TO    MD01-TENMEI
*        END-IF
*        MOVE  "1"              TO    RD1-FLG
*    END-IF.
*  伝票番号
     MOVE     JYW-F04              TO        MD01-DENNO.
*  元伝票番号
     MOVE     JYW-F05              TO        MD01-MOTODENNO.
*  相手伝区
     MOVE     JYW-F06              TO        MD01-AITEDENKU.
*  店舗ＣＤ
     MOVE     JYW-F07              TO        MD01-TENCD.
*  計上日
     MOVE     JYW-F08(1:4)         TO        MD01-KDATEYYYY.
     MOVE     JYW-F08(5:2)         TO        MD01-KDATEMM.
     MOVE     JYW-F08(7:2)         TO        MD01-KDATEDD.
*  分類ＣＤ
     MOVE     JYW-F09              TO        MD01-BUNRUI.
*  訂正区分Ｈ
     MOVE     JYW-F10              TO        MD01-TEISEIH.
*  自社伝区
     MOVE     JYW-F51              TO        MD01-JIDENKU.
*------------------------------------------------------------
 MAIN-03.
*明細行編集２
*  行
     MOVE     JYW-F31              TO        MD02-GYO.
*  赤黒　　
     IF       JYW-F32     =        ZERO
              MOVE     NC"黒"      TO        MD02-AKAKURO
     ELSE
              MOVE     NC"赤"      TO        MD02-AKAKURO
     END-IF.
*  相手商品コード
     MOVE     JYW-F33              TO        MD02-AITESYOCD.
*  商品名１
     MOVE     JYW-F37              TO        MD02-AITESYONM1.
*  商品名２
     MOVE     JYW-F38              TO        MD03-AITESYONM2.
*  数量
     MOVE     JYW-F34              TO        MD02-SURYOU.
*  原単価
     MOVE     JYW-F35              TO        MD02-GTANKA.
*  原価金額
     MOVE     JYW-F36              TO        MD02-GKINGAKU.
*  返品理由
     MOVE     JYW-F39              TO        MD02-RIYUU.
*  訂正区分Ｍ
     MOVE     JYW-F40              TO        MD02-TEISEIM.
*  自社商品コード
     MOVE     JYW-F52              TO        MD02-JISYOCD.
*  品単１
     MOVE     JYW-F53              TO        MD02-HINTAN1.
*  品単２
     MOVE     JYW-F54              TO        MD02-HINTAN2.
*  品単３
     MOVE     JYW-F55              TO        MD02-HINTAN3.
*
*--------------
*  伝票計加算
*--------------
*  受領/返品数
     COMPUTE  WK-SURYOU-DEN   = WK-SURYOU-DEN   +  JYW-F34.
*  原価金額
     COMPUTE  WK-KINGAKU-DEN  = WK-KINGAKU-DEN  +  JYW-F36.
*
*--------------
*  明細行出力
*--------------
 MAIN-04.
*  改頁チェック
     IF       L-CNT     >    49
              PERFORM  MIDASI-SEC
              PERFORM  HED-SEC
              WRITE    P-REC      FROM  HD08  AFTER 2
              ADD      2          TO    L-CNT
              MOVE  JYW-F07       TO    WRT-TENCD
              MOVE  JYW-F08       TO    WRT-KDATE
              MOVE  JYW-F51       TO    WRT-DENKU
              MOVE  JYW-F04       TO    WRT-DENNO
*???          GO    TO     COMJRWK1-RD
     END-IF.
 MAIN-05.
     IF   ( JYW-F07   NOT =   WRT-TENCD ) OR
          ( JYW-F08   NOT =   WRT-KDATE ) OR
          ( JYW-F51   NOT =   WRT-DENKU ) OR
          ( JYW-F04   NOT =   WRT-DENNO )
         WRITE  P-REC  FROM  HD07  AFTER 1
         WRITE  P-REC  FROM  MD01  AFTER 1
         WRITE  P-REC  FROM  HD08  AFTER 2
         ADD    4            TO    L-CNT
         IF       L-CNT     >    49
              PERFORM  MIDASI-SEC
              PERFORM  HED-SEC
              WRITE    P-REC      FROM  HD08  AFTER 2
              ADD      2          TO    L-CNT
              WRITE    P-REC      FROM  MD02  AFTER 1
              WRITE    P-REC      FROM  MD03  AFTER 1
              ADD      2          TO    L-CNT
         ELSE
              WRITE    P-REC      FROM  MD02  AFTER 1
              WRITE    P-REC      FROM  MD03  AFTER 1
              ADD      2          TO    L-CNT
              IF       L-CNT     >    49
                  PERFORM  MIDASI-SEC
                  PERFORM  HED-SEC
                  WRITE    P-REC  FROM  HD08  AFTER 2
                  ADD      2      TO    L-CNT
              END-IF
         END-IF
         MOVE  JYW-F07       TO    WRT-TENCD
         MOVE  JYW-F08       TO    WRT-KDATE
         MOVE  JYW-F51       TO    WRT-DENKU
         MOVE  JYW-F04       TO    WRT-DENNO
     ELSE
*???     IF      ( JYW-F04   NOT =   WRT-DENNO )
             WRITE  P-REC  FROM  MD02  AFTER 1
             WRITE  P-REC  FROM  MD03  AFTER 1
             ADD    2            TO    L-CNT
             IF       L-CNT     >    49
                  PERFORM  MIDASI-SEC
                  PERFORM  HED-SEC
                  WRITE    P-REC  FROM  HD08  AFTER 2
                  ADD      2      TO    L-CNT
             END-IF
*???         MOVE  JYW-F04       TO   WRT-DENNO
*???     END-IF
     END-IF.
*
 COMJRWK1-RD.
*??? WRITE  P-REC  FROM  MD03  AFTER 1.
*??? ADD    1            TO   L-CNT.
*    次レコード読込み
     PERFORM  COMJRWK1-RD-SEC.
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
     MOVE     SYS-YY            TO   HD01-YY.
     MOVE     SYS-MM            TO   HD01-MM.
     MOVE     SYS-DD            TO   HD01-DD.
*    MOVE     SYS-HH            TO   HD02-HH.
*    MOVE     SYS-MN            TO   HD02-MN.
*    MOVE     SYS-SS            TO   HD02-SS.
*返品区分
     EVALUATE   PARA-IN-HNPKBN
          WHEN   " "
              MOVE     NC"（全て）"            TO   HD01-HENKU
          WHEN   "1"
              MOVE     NC"（返品）"            TO   HD01-HENKU
          WHEN   "2"
              MOVE     NC"　　　　"            TO   HD01-HENKU
     END-EVALUATE.
*
*日付区分・日付
     EVALUATE   PARA-IN-DATEKBN
          WHEN   " "
              MOVE     NC"受信日"              TO   HD03-DKBNNM
              MOVE     PARA-IN-FROMDATE(1:4)   TO   HD04-JFYY
              MOVE     PARA-IN-FROMDATE(5:2)   TO   HD04-JFMM
              MOVE     PARA-IN-FROMDATE(7:2)   TO   HD04-JFDD
              MOVE     PARA-IN-TODATE(1:4)     TO   HD04-JTYY
              MOVE     PARA-IN-TODATE(5:2)     TO   HD04-JTMM
              MOVE     PARA-IN-TODATE(7:2)     TO   HD04-JTDD
          WHEN   "1"
              MOVE     NC"計上日"              TO   HD03-DKBNNM
              MOVE     PARA-IN-FROMDATE(1:4)   TO   HD04-KFYY
              MOVE     PARA-IN-FROMDATE(5:2)   TO   HD04-KFMM
              MOVE     PARA-IN-FROMDATE(7:2)   TO   HD04-KFDD
              MOVE     PARA-IN-TODATE(1:4)     TO   HD04-KTYY
              MOVE     PARA-IN-TODATE(5:2)     TO   HD04-KTMM
              MOVE     PARA-IN-TODATE(7:2)     TO   HD04-KTDD
     END-EVALUATE.
*
 MIDASISET-EXIT.
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
*
*頁セット
     ADD      1              TO   P-CNT.
     MOVE     P-CNT          TO   HD01-PCNT.
*ヘッダー出力
     WRITE    P-REC     FROM      HD01      AFTER     2.
     WRITE    P-REC     FROM      HD03      AFTER     2.
     WRITE    P-REC     FROM      HD04      AFTER     1.
     WRITE    P-REC     FROM      SEN2      AFTER     1.
     WRITE    P-REC     FROM      HD07      AFTER     1.
*    WRITE    P-REC     FROM      HD08      AFTER     2.
*
     MOVE     7          TO        L-CNT.
*
 MIDASI-EXIT.
     EXIT.
****************************************************************
*             ヘッダ出力処理　              1.3                *
****************************************************************
 HED-SEC             SECTION.
*
     MOVE    "HED-SEC"              TO    S-NAME.
     WRITE    P-REC     FROM      MD01      AFTER     1.
*    WRITE    P-REC     FROM      HD08      AFTER     2.
     ADD      1          TO        L-CNT.
*
 HED-EXIT.
     EXIT.
****************************************************************
*             伝票合計出力　                　　
****************************************************************
 DENKEI-SEC             SECTION.
*
     MOVE    "DENKEI-SEC"        TO   S-NAME.
*改頁
     IF       L-CNT  >  49
              PERFORM  MIDASI-SEC
              PERFORM  HED-SEC
     END-IF.
*
*--------------
*  合計転送
*--------------
*  タイトル転送
     MOVE     NC"合　計"         TO   GK01-TAITOL.
*  受領/返品数
     MOVE     WK-SURYOU-DEN      TO   GK01-SURYOU.
*  原価金額
     MOVE     WK-KINGAKU-DEN     TO   GK01-KINGAKU.
*
*伝票合計出力
     WRITE    P-REC     FROM      GK01      AFTER     1.
*
     ADD      1         TO        L-CNT.
*
*  線出力
     IF    END-FLG    NOT =  "END"
         IF       L-CNT  >  49
                  PERFORM  MIDASI-SEC
         END-IF
         WRITE    P-REC     FROM  SEN2  AFTER 2
*        IF      ( JYW-F04   NOT =   BRK-TENCD )
*                 WRITE    P-REC     FROM  SEN2  AFTER 1
*        ELSE
*                 WRITE    P-REC     FROM  SEN3  AFTER 1
*        END-IF
         ADD      2         TO    L-CNT
     END-IF.
*
 DENKEI-EXIT.
     EXIT.
*
***************************************************************
*             取引先マスタ読込
***************************************************************
 TOKMS2-READ-SEC        SECTION.
*
     MOVE    "TOKMS2-READ-SEC"  TO        S-NAME.
*
     READ     TOKMS2
              INVALID      MOVE  "INV"    TO   TOKMS2-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   TOKMS2-INV-FLG
     END-READ.
*
 TOKMS2-READ-EXIT.
     EXIT.
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     IF      L-CNT   NOT =  99
* 伝票計出力
         IF       L-CNT  >  49
                  PERFORM  MIDASI-SEC
                  PERFORM  HED-SEC
         END-IF
         PERFORM   DENKEI-SEC
     END-IF.
*
     DISPLAY  NC"出力ページ数＝" P-CNT UPON CONS.
     DISPLAY  MSG-END   UPON CONS.
*
     CLOSE    COMJRWK1 TOKMS2   PRINTF.
*
 END-EXIT.
     EXIT.
*
