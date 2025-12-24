****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受領返品取込機能構築　　　　      *
*    業務名　　　　　　　：　返品伝票計上処理　　　            *
*    モジュール名　　　　：　返品計上確認リスト発行　　　　　　*
*    作成日／更新日　　　：　2017/09/11                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　パラメタを受取り、指定条件の返品　*
*                            計上確認リストを発行する。　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SJR0310L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          17/09/11.
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
                        YB-22 IS PITCH-30.       *> 3.0ピッチ
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<返品抽出データ >>*********************************
     SELECT   COMWHEL1           ASSIGN    TO   DA-01-VI-COMWHEL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    WHE-F01
                                                WHE-F03
                                                WHE-F02
                                                WHE-F04
                                                WHE-F05
                                 STATUS         WHE-STATUS.
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
 FD  COMWHEL1           LABEL RECORD   IS   STANDARD.
     COPY     COMWHEF   OF        XFDLIB
              JOINING   WHE       PREFIX.
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
 01  TOKMS2-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  TENMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  JYOKEN1-INV-FLG              PIC       X(03)  VALUE  SPACE.
 01  TANMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  WHE-STATUS                   PIC       X(02).
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
         05  ST-PG          PIC   X(08)  VALUE "SJR0310L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0310L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SJR0310L".
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
     03  FILLER                PIC  X(08)  VALUE  "SJR0310L".
     03  FILLER                PIC  X(31)  VALUE  SPACE.
     03  FILLER                PIC  N(11)
                       VALUE NC"＜返品計上確認リスト＞"
                               CHARACTER  TYPE  IS PITCH-22.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  HD01-JIKKBN           PIC  N(08)  VALUE  NC"年"
                               CHARACTER  TYPE  IS PITCH-15-YKBAI.
     03  FILLER                PIC  X(04)  VALUE  SPACE.
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
                               CHARACTER   TYPE IS   PITCH-15.
     03  HD03-KKBN             PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD03-KKBNNM           PIC  N(04)
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"担当者："
                               CHARACTER   TYPE IS   PITCH-15.
     03  HD03-TANF             PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(01)  VALUE  NC"～"
                               CHARACTER  TYPE IS PITCH-15.
     03  HD03-TANT             PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"検収日："
                               CHARACTER  TYPE IS PITCH-15.
     03  HD03-AFYY             PIC  99   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-AFMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-AFDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  N(01)  VALUE  NC"～"
                               CHARACTER  TYPE IS PITCH-15.
     03  HD03-ATYY             PIC  99   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-ATMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-ATDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"入力日："
                               CHARACTER   TYPE IS   PITCH-15.
     03  HD03-NFYY             PIC  99   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-NFMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-NFDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  N(01)  VALUE  NC"～"
                               CHARACTER  TYPE IS PITCH-15.
     03  HD03-NTYY             PIC  99   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-NTMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-NTDD             PIC  99     VALUE  ZERO.
*****03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"計上日："
                               CHARACTER   TYPE IS   PITCH-15.
     03  HD03-KFYY             PIC  99   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-KFMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-KFDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  N(01)  VALUE  NC"～"
                               CHARACTER  TYPE IS PITCH-15.
     03  HD03-KTYY             PIC  99   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-KTMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-KTDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"作成日："
                               CHARACTER   TYPE IS   PITCH-15.
     03  HD03-SFYY             PIC  99   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-SFMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-SFDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  N(01)  VALUE  NC"～"
                               CHARACTER  TYPE IS PITCH-15.
     03  HD03-STYY             PIC  99   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-STMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-STDD             PIC  99     VALUE  ZERO.
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
     03  HD05-01              PIC  X(01)  VALUE  SPACE.
     03  HD05-02              PIC  N(04)  VALUE  SPACE
                              CHARACTER  TYPE IS PITCH-20.
     03  HD05-03              PIC  X(01)  VALUE  SPACE.
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
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
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
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           COMWHEL1.
     MOVE     "COMWHEL1"          TO        ERR-FL-ID.
     MOVE     WHE-STATUS          TO        ERR-STCD.
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
 SJR0310L-START         SECTION.
*
     MOVE   "SJR0310L-START"      TO   S-NAME.
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
 SJR0310L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     COMWHEL1.
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
     MOVE  SPACE                TO   WHE-REC.
     INITIALIZE                      WHE-REC.
*
     START  COMWHEL1  KEY  >= WHE-F01 WHE-F03 WHE-F02
                              WHE-F04 WHE-F05
         INVALID   KEY
            MOVE     "END"      TO   END-FLG
            DISPLAY NC"＃対象データ無し１＃" UPON CONS
            GO                  TO   INIT-EXIT
     END-START
*
     PERFORM  COMWHEL1-RD-SEC.
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
     PERFORM   MIDASISET-SEC.
*ブレイクキー設定
     MOVE  WHE-F02    TO       BRK-TENCD.
     MOVE  WHE-F04    TO       BRK-DENNO.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ケーヨー　売上累積データ読み込み　　　
****************************************************************
 COMWHEL1-RD-SEC            SECTION.
*
     MOVE    "COMWHEL1-RD-SEC"    TO   S-NAME.
*
     READ     COMWHEL1
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    COMWHEL1-RD-EXIT
     END-READ.
*
*
 COMWHEL1-RD-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO    S-NAME.
*
*  ブレイク時
     IF      ( WHE-F02   NOT =   BRK-TENCD )  OR
             ( WHE-F04   NOT =   BRK-DENNO )
*        合計
              PERFORM   DENKEI-SEC
              MOVE   ZERO         TO    WK-DENKEI
              INITIALIZE                WK-DENKEI
              MOVE  SPACE         TO    RD1-FLG
              MOVE  "1"           TO    SET-FLG
              MOVE  WHE-F02       TO    BRK-TENCD
              MOVE  WHE-F04       TO    BRK-DENNO
     END-IF.
*  明細行編集１
*  取引先
     MOVE     WHE-F01           TO    MD01-TORICD
*  伝票番号
     MOVE     WHE-F04           TO    MD01-DENNO.
*  出荷場所
     MOVE     WHE-F08           TO    MD01-SKBCD.
*  店舗ＣＤ
     MOVE     WHE-F02           TO    MD01-TENCD
*  伝区
*XXX MOVE     WHE-F06          TO    MD01-DENKU.
     MOVE     WHE-F23          TO    MD01-DENKU.
*  検収日
     MOVE     WHE-F03          TO    MD01-KENSHUBI.
*  実検収日
     MOVE     WHE-F07          TO    MD01-JKENSHUBI.
*  担当者
     MOVE     WHE-F82          TO    MD01-TANCD.
*  入力日
     MOVE     WHE-F83          TO    MD01-NYURYOKUBI.
*  計上日
     MOVE     WHE-F86          TO    MD01-KEIJOBI.
     IF  RD1-FLG = SPACE
*  取引先正式名
         MOVE     WHE-F01           TO    TOK-F01
         PERFORM  TOKMS2-READ-SEC
         IF  TOKMS2-INV-FLG = "INV"
           MOVE ALL NC"＊"      TO    MD01-TORINM
         ELSE
           MOVE TOK-F03         TO    MD01-TORINM
         END-IF
*  店舗略名
         MOVE     WHE-F02           TO    TEN-F011
         MOVE     WHE-F01           TO    TEN-F52
         PERFORM  TENMS1-READ-SEC
         IF  TENMS1-INV-FLG = "INV"
           MOVE ALL NC"＊"      TO    MD01-TENMEI
         ELSE
           MOVE TEN-F03         TO    MD01-TENMEI
         END-IF
*  伝区名称
         MOVE     1                 TO    JYO-F01
         MOVE     WHE-F23           TO    JYO-F02
         PERFORM  JYOKEN1-READ-SEC
         IF   JYOKEN1-INV-FLG = "INV"
              MOVE  ALL NC"＊"     TO        MD01-DNKNM
         ELSE
              MOVE  JYO-F03        TO        MD01-DNKNM
         END-IF
*  担当者名
         MOVE     WHE-F81           TO    TAN-F01
         MOVE     WHE-F82           TO    TAN-F02
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
     MOVE     WHE-F05              TO        MD02-GYO.
*  指定商品コード
     MOVE     WHE-F09              TO        MD02-JANCD.
*  商品名
     MOVE     WHE-F14              TO        MD02-SHONM1.
     MOVE     WHE-F15              TO        MD02-SHONM2.
*  サカタ商品
     MOVE     WHE-F10              TO        MD02-SHOCD.
     MOVE     WHE-F11              TO        MD02-HINTAN1.
     MOVE     WHE-F12              TO        MD02-HINTAN2.
     MOVE     WHE-F13              TO        MD02-HINTAN3.
*  数量
     MOVE     WHE-F16              TO        MD02-SURYO.
*  原価単価
     MOVE     WHE-F17              TO        MD02-GENKA.
*  原価金額
     MOVE     WHE-F18              TO        MD02-KINGAKU.
*  備考
     MOVE     WHE-F21              TO        MD02-BIKO.
*  伝票纏めの場合
     IF  PARA-JIKKBN  =  "1"
         MOVE SPACE                TO        MD02-BIKO
         MOVE WHE-F28              TO        MD02-BIKO
     END-IF.
*
*--------------
*  伝票計加算
*--------------
*  伝票備考
     MOVE     WHE-F24              TO        GK01-MEISAIBIKO1.
     MOVE     WHE-F25              TO        GK01-MEISAIBIKO2.
*  数量　
     COMPUTE  WK-SURYO-DEN     = WK-SURYO-DEN     +  WHE-F16.
*  原価金額
     COMPUTE  WK-KINGAKU-DEN   = WK-KINGAKU-DEN   +  WHE-F18.
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
     PERFORM  COMWHEL1-RD-SEC.
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
*実行区分
     IF    PARA-JIKKBN  =  SPACE
           MOVE NC"（　通　常　）　"  TO  HD01-JIKKBN
     ELSE
           MOVE NC"（伝票まとめ分）"  TO  HD01-JIKKBN
     END-IF.
*計上区分・日付
     MOVE     PARA-KEIKBN       TO   HD03-KKBN.
     EVALUATE   PARA-KEIKBN
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
          WHEN   "1"
              MOVE     PARA-AFROM(1:4)    TO   HD03-AFYY
              MOVE     PARA-AFROM(5:2)    TO   HD03-AFMM
              MOVE     PARA-AFROM(7:2)    TO   HD03-AFDD
              MOVE     PARA-ATO(1:4)      TO   HD03-ATYY
              MOVE     PARA-ATO(5:2)      TO   HD03-ATMM
              MOVE     PARA-ATO(7:2)      TO   HD03-ATDD
          WHEN   "2"
              MOVE     PARA-NFROM(1:4)    TO   HD03-NFYY
              MOVE     PARA-NFROM(5:2)    TO   HD03-NFMM
              MOVE     PARA-NFROM(7:2)    TO   HD03-NFDD
              MOVE     PARA-NTO(1:4)      TO   HD03-NTYY
              MOVE     PARA-NTO(5:2)      TO   HD03-NTMM
              MOVE     PARA-NTO(7:2)      TO   HD03-NTDD
          WHEN   "3"
              MOVE     PARA-KFROM(1:4)    TO   HD03-KFYY
              MOVE     PARA-KFROM(5:2)    TO   HD03-KFMM
              MOVE     PARA-KFROM(7:2)    TO   HD03-KFDD
              MOVE     PARA-KTO(1:4)      TO   HD03-KTYY
              MOVE     PARA-KTO(5:2)      TO   HD03-KTMM
              MOVE     PARA-KTO(7:2)      TO   HD03-KTDD
          WHEN   "4"
              MOVE     PARA-SFROM(1:4)    TO   HD03-SFYY
              MOVE     PARA-SFROM(5:2)    TO   HD03-SFMM
              MOVE     PARA-SFROM(7:2)    TO   HD03-SFDD
              MOVE     PARA-STO(1:4)      TO   HD03-STYY
              MOVE     PARA-STO(5:2)      TO   HD03-STMM
              MOVE     PARA-STO(7:2)      TO   HD03-STDD
     END-EVALUATE.
*担当者
     MOVE     PARA-TANST          TO   HD03-TANF.
     MOVE     PARA-TANED          TO   HD03-TANT.
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
*伝票纏めの場合、ヘッダ表示
     IF   PARA-JIKKBN  =  "1"
          MOVE  "("          TO   HD05-01
          MOVE  NC"伝票番号" TO   HD05-02
          MOVE  ")"          TO   HD05-03
     ELSE
          MOVE  SPACE        TO   HD05-01  HD05-02  HD05-03
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
     CLOSE    COMWHEL1 TENMS1 TOKMS2 JYOKEN1 TANMS1 PRINTF.
*
 END-EXIT.
     EXIT.
*
