****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ケーヨー伝票レス　　　　　　　　　*
*    業務名　　　　　　　：　ケーヨー伝票レス                  *
*    モジュール名　　　　：　返品計上確認リスト発行　　　　　　*
*    作成日／更新日　　　：　14/03/19                          *
*    作成者／更新者　　　：　NAV三浦
*    処理概要　　　　　　：　返品計上確認リストを出力する。　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSK0052L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/03/19.
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
                        YA-22 IS PITCH-22        *> 2.0ピッチ
                        YA-21 IS PITCH-20-YKBAI  *> 2.0ピッチ
                        YB    IS PITCH-15        *> 1.5ピッチ
                        YB-21 IS PITCH-15-YKBAI  *> 1.5ピッチ
                        YB-21 IS PITCH-30.       *> 3.0ピッチ
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<返品抽出データ >>*********************************
     SELECT   KEIJHWL1           ASSIGN    TO   DA-01-VI-KEIJHWL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    HWK-F02
                                                HWK-F04
                                                HWK-F05
                                 STATUS         HWK-STATUS.
*
****<<店舗マスタ　　　　　　 >>*********************************
     SELECT   TENMS1             ASSIGN    TO   DA-01-VI-TENMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TEN-F52  TEN-F011
                                 STATUS         TEN-STATUS.
*
****<<取引先マスタ　　　　　　 >>*******************************
     SELECT   TOKMS2            ASSIGN    TO   DA-01-VI-TOKMS2
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TOK-F01
                                 STATUS         TOK-STATUS.
*
****<<条件ファル　　　　　　 >>*********************************
     SELECT   JYOKEN1            ASSIGN    TO   DA-01-VI-JYOKEN1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    JYO-F01  JYO-F02
                                 STATUS         JYO-STATUS.
*
****<<担当者マスタ　　　　　 >>*********************************
     SELECT   TANMS1             ASSIGN    TO   DA-01-VI-TANMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TAN-F01  TAN-F02
                                 STATUS         TAN-STATUS.
*
*****<<  プリント　Ｆ   >>**************************************
     SELECT   PRINTF    ASSIGN    TO        LP-04-PRTF.
*
*                                                                *
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ケーヨー　返品抽出データ　　　　　　               *
*--------------------------------------------------------------*
 FD  KEIJHWL1           LABEL RECORD   IS   STANDARD.
     COPY     KEIJHWF   OF        XFDLIB
              JOINING   HWK       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 店舗マスタ　　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 取引先マスタ　　　　　　　　　
*--------------------------------------------------------------*
 FD  TOKMS2            LABEL RECORD   IS   STANDARD.
     COPY     TOKMS2    OF        XFDLIB
              JOINING   TOK       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 条件ファイル　　　　　　　　　                     *
*--------------------------------------------------------------*
 FD  JYOKEN1             LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN    OF        XFDLIB
              JOINING   JYO       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 担当者マスタ　　　　　　　　　                     *
*--------------------------------------------------------------*
 FD  TANMS1             LABEL RECORD   IS   STANDARD.
     COPY     TANMS1    OF        XFDLIB
              JOINING   TAN       PREFIX.
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
 01  TOKMS2-INV-FLG              PIC       X(03)  VALUE  SPACE.
 01  TENMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  JYOKEN1-INV-FLG              PIC       X(03)  VALUE  SPACE.
 01  TANMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  HWK-STATUS                   PIC       X(02).
 01  TOK-STATUS                   PIC       X(02).
 01  TEN-STATUS                   PIC       X(02).
 01  JYO-STATUS                   PIC       X(02).
 01  TAN-STATUS                   PIC       X(02).
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
*
 01  BRK-KEY.
     03  BRK-TENCD                PIC       9(05)  VALUE  ZERO.
     03  BRK-DENNO                PIC       9(09)  VALUE  ZERO.
     03  BRK-DENK                 PIC       9(02)  VALUE  ZERO.
     03  BRK-NOHINBI              PIC       9(08)  VALUE  ZERO.
*
*--------------
 01  WK-DENKEI.
     03  WK-SURYO-DEN             PIC      S9(09)V99.
     03  WK-KINGAKU-DEN           PIC      S9(11).
     03  WK-BAIGAKU-DEN           PIC      S9(11).
*
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSK0052L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0052L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSK0052L".
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
     03  FILLER                PIC  X(08)  VALUE  "SSK0052L".
     03  FILLER                PIC  X(23)  VALUE  SPACE.
     03  FILLER                PIC  N(15)
                       VALUE NC"＜ケーヨー返品計上確認リスト＞"
                               CHARACTER  TYPE  IS PITCH-22.
     03  FILLER                PIC  X(22)  VALUE  SPACE.
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
     03  FILLER                PIC  X(116)  VALUE  SPACE.
     03  HD02-HH               PIC  99.
     03  FILLER                PIC  N(01)  VALUE  NC"："
                               CHARACTER  TYPE  IS PITCH-20.
     03  HD02-MN               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"："
                               CHARACTER  TYPE IS PITCH-20.
     03  HD02-SS               PIC  Z9.
***** 見出し行３
 01  HD03.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(05)  VALUE  NC"計上区分："
                               CHARACTER   TYPE IS   PITCH-20.
     03  HD03-KKBN             PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD03-KKBNNM           PIC  N(04)
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"担当者："
                               CHARACTER   TYPE IS   PITCH-20.
     03  HD03-TANF             PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(01)  VALUE  NC"～"
                               CHARACTER  TYPE IS PITCH-20.
     03  HD03-TANT             PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"検収日："
                               CHARACTER  TYPE IS PITCH-20.
     03  HD03-AFYY             PIC  9999   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-AFMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-AFDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  N(01)  VALUE  NC"～"
                               CHARACTER  TYPE IS PITCH-20.
     03  HD03-ATYY             PIC  9999   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-ATMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-ATDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"入力日："
                               CHARACTER   TYPE IS   PITCH-20.
     03  HD03-NFYY             PIC  9999   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-NFMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-NFDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  N(01)  VALUE  NC"～"
                               CHARACTER  TYPE IS PITCH-20.
     03  HD03-NTYY             PIC  9999   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-NTMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-NTDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"計上日："
                               CHARACTER   TYPE IS   PITCH-20.
     03  HD03-KFYY             PIC  9999   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-KFMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-KFDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  N(01)  VALUE  NC"～"
                               CHARACTER  TYPE IS PITCH-20.
     03  HD03-KTYY             PIC  9999   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-KTMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-KTDD             PIC  99     VALUE  ZERO.
***** 見出し行４
 01  HD04.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"取引先"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(19)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"伝票番号"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"出場"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"店舗"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(17)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"伝区"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(10)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"検収日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"実検収日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE  NC"入力担当者"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(10)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"入力日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"計上日"
                              CHARACTER  TYPE IS PITCH-20.
***** 見出し行５
 01  HD05.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"行"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE  NC"量販店商品"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(35)  VALUE  SPACE.
     03  FILLER               PIC  X(02)  VALUE  "( ".
     03  FILLER       PIC  N(09)  VALUE  NC"サカタ商品コード　"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  ")".
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"数量"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"原価単価"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"原価金額"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"明細備考"
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
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  MD01-TORICD              PIC       99999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TORINM              PIC       N(10).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-DENNO               PIC       999999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SKBCD               PIC       X(02).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-TENCD               PIC       99999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TENMEI              PIC       N(10).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-DENKU               PIC       X(02).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-DNKNM               PIC       N(04).
     03  FILLER                   PIC       X(04)  VALUE  SPACE.
     03  MD01-KENSHUBI            PIC       99999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-JKENSHUBI           PIC       99999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TANCD               PIC       X(02).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TANNM               PIC       N(10).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-NYURYOKUBI          PIC       99999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-KEIJOBI             PIC       99999999.
 01  MD02.
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  MD02-GYO                 PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-JANCD               PIC       X(13).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-SHONM1              PIC       X(15).
     03  MD02-SHONM2              PIC       X(15).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       X(01)  VALUE  "(".
     03  MD02-SHOCD               PIC       X(08).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-HINTAN1             PIC       X(05).
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  MD02-HINTAN2             PIC       X(02).
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  MD02-HINTAN3             PIC       X(01).
     03  FILLER                   PIC       X(01)  VALUE  ")".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-SURYO               PIC       -,---,--9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-GENKA               PIC       -,---,--9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-KINGAKU             PIC       ---,---,--9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-BIKO                PIC       X(10).
***** 合計行
 01  GK01.
     03  FILLER                   PIC     X(03)  VALUE  SPACE.
     03  GK01-BIKO1               PIC  N(04) VALUE  NC"＜備考："
                                  CHARACTER   TYPE IS   PITCH-20.
     03  GK01-MEISAIBIKO1         PIC       X(15).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  GK01-MEISAIBIKO2         PIC       X(15).
     03  GK01-BIKO2               PIC   N(01) VALUE  NC"＞"
                                  CHARACTER   TYPE IS   PITCH-20.
     03  FILLER                   PIC     X(01)  VALUE  SPACE.
     03  GK01-GOKEI        PIC  N(06) VALUE  NC"＜伝票合計＞"
                                  CHARACTER   TYPE IS   PITCH-20.
     03  FILLER                   PIC       X(16)  VALUE  SPACE.
     03  GK01-SURYO               PIC       -,---,--9.
     03  FILLER                   PIC       X(12)  VALUE  SPACE.
     03  GK01-KINGAKU             PIC       ---,---,--9.
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
                        PROCEDURE           KEIJHWL1.
     MOVE     "KEIJHWL1"          TO        ERR-FL-ID.
     MOVE     HWK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
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
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TOKMS2.
     MOVE     "TOKMS2 "          TO        ERR-FL-ID.
     MOVE     TOK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC4         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE JYOKEN1.
     MOVE     "JYOKEN1 "          TO        ERR-FL-ID.
     MOVE     JYO-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC5         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TANMS1.
     MOVE     "TANMS1 "          TO        ERR-FL-ID.
     MOVE     TAN-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSK0052L-START         SECTION.
*
     MOVE   "SSK0052L-START"      TO   S-NAME.
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
 SSK0052L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     KEIJHWL1.
     OPEN     INPUT     TENMS1.
     OPEN     INPUT     TOKMS2.
     OPEN     INPUT     JYOKEN1.
     OPEN     INPUT     TANMS1.
     OPEN     OUTPUT    PRINTF.
*
     MOVE     ZERO           TO    WK-DENKEI.
     INITIALIZE                    WK-DENKEI.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     99             TO    L-CNT.
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
     MOVE  SPACE                TO   HWK-REC.
     INITIALIZE                      HWK-REC.
*
     START  KEIJHWL1  KEY  >= HWK-F02 HWK-F04 HWK-F05
         INVALID   KEY
            MOVE     "END"      TO   END-FLG
            DISPLAY NC"＃対象データ無し１＃" UPON CONS
            GO                  TO   INIT-EXIT
     END-START
*
     PERFORM  KEIJHWL1-RD-SEC.
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
     PERFORM   MIDASISET-SEC.
*ブレイクキー設定
     MOVE  HWK-F02    TO       BRK-TENCD.
     MOVE  HWK-F04    TO       BRK-DENNO.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ケーヨー　売上累積データ読み込み　　　
****************************************************************
 KEIJHWL1-RD-SEC            SECTION.
*
     MOVE    "KEIJHWL1-RD-SEC"    TO   S-NAME.
*
     READ     KEIJHWL1
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    KEIJHWL1-RD-EXIT
     END-READ.
*
*
 KEIJHWL1-RD-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO    S-NAME.
*
*  ブレイク時
     IF      ( HWK-F02   NOT =   BRK-TENCD )  OR
             ( HWK-F04   NOT =   BRK-DENNO )
*        合計
              PERFORM   DENKEI-SEC
              MOVE   ZERO         TO    WK-DENKEI
              INITIALIZE                WK-DENKEI
              MOVE  SPACE         TO    RD1-FLG
              MOVE  "1"           TO    SET-FLG
              MOVE  HWK-F02       TO    BRK-TENCD
              MOVE  HWK-F04       TO    BRK-DENNO
     END-IF.
*  明細行編集１
*  取引先
     MOVE     HWK-F01           TO    MD01-TORICD
*  伝票番号
     MOVE     HWK-F04           TO    MD01-DENNO.
*  出荷場所
     MOVE     HWK-F08           TO    MD01-SKBCD.
*  店舗ＣＤ
     MOVE     HWK-F02           TO    MD01-TENCD
*  伝区
*XXX MOVE     HWK-F06          TO    MD01-DENKU.
     MOVE     HWK-F23          TO    MD01-DENKU.
*  検収日
     MOVE     HWK-F03          TO    MD01-KENSHUBI.
*  実検収日
     MOVE     HWK-F07          TO    MD01-JKENSHUBI.
*  担当者
     MOVE     HWK-F82          TO    MD01-TANCD.
*  入力日
     MOVE     HWK-F83          TO    MD01-NYURYOKUBI.
*  計上日
     MOVE     HWK-F86          TO    MD01-KEIJOBI.
     IF  RD1-FLG = SPACE
*  取引先正式名
         MOVE     HWK-F01           TO    TOK-F01
         PERFORM  TOKMS2-READ-SEC
         IF  TOKMS2-INV-FLG = "INV"
           MOVE ALL NC"＊"      TO    MD01-TORINM
         ELSE
           MOVE TOK-F03         TO    MD01-TORINM
         END-IF
*  店舗略名
         MOVE     HWK-F02           TO    TEN-F011
         MOVE     173               TO    TEN-F52
         PERFORM  TENMS1-READ-SEC
         IF  TENMS1-INV-FLG = "INV"
           MOVE ALL NC"＊"      TO    MD01-TENMEI
         ELSE
           MOVE TEN-F03         TO    MD01-TENMEI
         END-IF
*  伝区名称
         MOVE     1                 TO    JYO-F01
*XXX     MOVE     HWK-F06           TO    JYO-F02
         MOVE     HWK-F23           TO    JYO-F02
         PERFORM  JYOKEN1-READ-SEC
         IF   JYOKEN1-INV-FLG = "INV"
              MOVE  ALL NC"＊"     TO        MD01-DNKNM
         ELSE
              MOVE  JYO-F03        TO        MD01-DNKNM
         END-IF
*  担当者名
         MOVE     HWK-F81           TO    TAN-F01
         MOVE     HWK-F82           TO    TAN-F02
         PERFORM  TANMS1-READ-SEC
         IF   TANMS1-INV-FLG = "INV"
              MOVE  ALL NC"＊"     TO        MD01-TANNM
         ELSE
              MOVE  TAN-F03        TO        MD01-TANNM
         END-IF
         MOVE  "1"              TO    RD1-FLG
     END-IF.
*
*  明細行編集２
*  行番
     MOVE     HWK-F05              TO        MD02-GYO.
*  指定商品コード
     MOVE     HWK-F09              TO        MD02-JANCD.
*  商品名
     MOVE     HWK-F14            TO        MD02-SHONM1.
     MOVE     HWK-F15            TO        MD02-SHONM2.
*  サカタ商品
     MOVE     HWK-F10            TO        MD02-SHOCD.
     MOVE     HWK-F11            TO        MD02-HINTAN1.
     MOVE     HWK-F12            TO        MD02-HINTAN2.
     MOVE     HWK-F13            TO        MD02-HINTAN3.
*  数量
     MOVE     HWK-F16              TO        MD02-SURYO.
*  原価単価
     MOVE     HWK-F17             TO        MD02-GENKA.
*  原価金額
     MOVE     HWK-F18             TO        MD02-KINGAKU.
*  備考
     MOVE     HWK-F21              TO        MD02-BIKO.
*--------------
*  伝票計加算
*--------------
*  伝票備考
     MOVE     HWK-F24              TO        GK01-MEISAIBIKO1.
     MOVE     HWK-F25              TO        GK01-MEISAIBIKO2.
*  数量　
     COMPUTE  WK-SURYO-DEN     = WK-SURYO-DEN     +  HWK-F16.
*  原価金額
     COMPUTE  WK-KINGAKU-DEN   = WK-KINGAKU-DEN   +  HWK-F18.
*
*--------------
*  明細行出力
*--------------
*  改頁チェック
     IF       L-CNT     >    54
              PERFORM  MIDASI-SEC
              PERFORM  HED-SEC
     END-IF.
     IF       SET-FLG   = "1"
              PERFORM  HED-SEC
     END-IF.
     IF       L-CNT     >    54
              PERFORM  MIDASI-SEC
              PERFORM  HED-SEC
     END-IF.
     WRITE  P-REC  FROM  MD02  AFTER 1.
     ADD    1            TO    L-CNT.
*    次レコード読込み
     PERFORM  KEIJHWL1-RD-SEC.
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
     MOVE     SYS-HH            TO   HD02-HH.
     MOVE     SYS-MN            TO   HD02-MN.
     MOVE     SYS-SS            TO   HD02-SS.
*計上区分・日付
     MOVE     PARA-KKBN         TO   HD03-KKBN.
     EVALUATE   PARA-KKBN
          WHEN   "1"
              MOVE    NC"未確認"           TO   HD03-KKBNNM
          WHEN   "2"
              MOVE    NC"未計上"           TO   HD03-KKBNNM
          WHEN   "3"
              MOVE    NC"計上済"           TO   HD03-KKBNNM
          WHEN   "4"
              MOVE    NC"取消分"           TO   HD03-KKBNNM
     END-EVALUATE.
     EVALUATE   PARA-DKBN
          WHEN   "2"
              MOVE     PARA-DFROM(1:4)    TO   HD03-AFYY
              MOVE     PARA-DFROM(5:2)    TO   HD03-AFMM
              MOVE     PARA-DFROM(7:2)    TO   HD03-AFDD
              MOVE     PARA-DTO(1:4)      TO   HD03-ATYY
              MOVE     PARA-DTO(5:2)      TO   HD03-ATMM
              MOVE     PARA-DTO(7:2)      TO   HD03-ATDD
          WHEN   "6"
              MOVE     PARA-DFROM(1:4)    TO   HD03-NFYY
              MOVE     PARA-DFROM(5:2)    TO   HD03-NFMM
              MOVE     PARA-DFROM(7:2)    TO   HD03-NFDD
              MOVE     PARA-DTO(1:4)      TO   HD03-NTYY
              MOVE     PARA-DTO(5:2)      TO   HD03-NTMM
              MOVE     PARA-DTO(7:2)      TO   HD03-NTDD
          WHEN   "7"
              MOVE     PARA-DFROM(1:4)    TO   HD03-KFYY
              MOVE     PARA-DFROM(5:2)    TO   HD03-KFMM
              MOVE     PARA-DFROM(7:2)    TO   HD03-KFDD
              MOVE     PARA-DTO(1:4)      TO   HD03-KTYY
              MOVE     PARA-DTO(5:2)      TO   HD03-KTMM
              MOVE     PARA-DTO(7:2)      TO   HD03-KTDD
     END-EVALUATE.
*担当者
     MOVE     PARA-TANFROM        TO   HD03-TANF.
     MOVE     PARA-TANTO          TO   HD03-TANT.
*
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
     WRITE    P-REC     FROM      HD01      AFTER     3.
     WRITE    P-REC     FROM      HD02      AFTER     1.
     WRITE    P-REC     FROM      HD03      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
     WRITE    P-REC     FROM      HD04      AFTER     1.
     WRITE    P-REC     FROM      HD05      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
*
     MOVE     10        TO        L-CNT.
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
     ADD      1          TO        L-CNT.
     MOVE    SPACE                  TO    SET-FLG.
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
     IF       L-CNT  >  54
              PERFORM  MIDASI-SEC
              PERFORM  HED-SEC
     END-IF.
*
*--------------
*  合計転送
*--------------
*  数量　
     MOVE     WK-SURYO-DEN       TO   GK01-SURYO.
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
         IF       L-CNT  >  54
              PERFORM  MIDASI-SEC
         END-IF
         WRITE    P-REC     FROM  SEN2  AFTER 1
         ADD      1         TO    L-CNT
     END-IF.
*
 DENKEI-EXIT.
     EXIT.
*
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
*             担当者マスタ読込
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
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     IF      L-CNT   NOT =  99
* 伝票計出力
         IF       L-CNT  >  54
              PERFORM  MIDASI-SEC
              PERFORM  HED-SEC
         END-IF
         PERFORM   DENKEI-SEC
     END-IF.
*
     DISPLAY  MSG-END   UPON CONS.
*
     CLOSE    KEIJHWL1 TENMS1 TOKMS2 JYOKEN1 TANMS1 PRINTF.
*
 END-EXIT.
     EXIT.
*
