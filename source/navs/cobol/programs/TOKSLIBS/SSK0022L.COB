****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ケーヨー伝票レス　　　　　　　　　*
*    業務名　　　　　　　：　ケーヨー伝票レス                  *
*    モジュール名　　　　：　受領書発行　　　　　　　　　      *
*    作成日／更新日　　　：　14/03/10                          *
*    作成者／更新者　　　：　NAV三浦                         *
*    処理概要　　　　　　：　受領書を出力する。　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSK0022L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/03/10.
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
****<<倉庫マスタ　　　　　　 >>*********************************
     SELECT   ZSOKMS1            ASSIGN    TO   DA-01-VI-ZSOKMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    SOK-F01
                                 STATUS         SOK-STATUS.
*
****<<条件ファル　　　　　　 >>*********************************
     SELECT   JYOKEN1            ASSIGN    TO   DA-01-VI-JYOKEN1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    JYO-F01  JYO-F02
                                 STATUS         JYO-STATUS.
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
*    FILE = 倉庫マスタ　　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 条件ファイル　　　　　　　　　                     *
*--------------------------------------------------------------*
 FD  JYOKEN1             LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN    OF        XFDLIB
              JOINING   JYO       PREFIX.
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
 01  ZSOKMS1-INV-FLG              PIC       X(03)  VALUE  SPACE.
 01  TENMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  JYOKEN1-INV-FLG              PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  JYW-STATUS                   PIC       X(02).
 01  SOK-STATUS                   PIC       X(02).
 01  TEN-STATUS                   PIC       X(02).
 01  JYO-STATUS                   PIC       X(02).
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
     03  BRK-TENCD                PIC      S9(05)  VALUE  ZERO.
     03  BRK-DENNO                PIC      S9(09)  VALUE  ZERO.
 01  WRT-KEY.
     03  WRT-TENCD                PIC      S9(05)  VALUE  ZERO.
     03  WRT-DENNO                PIC      S9(09)  VALUE  ZERO.
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
     03  WK-SHUKASU-DEN           PIC      S9(06)V9.
     03  WK-KENSHUSU-DEN          PIC      S9(06)V9.
     03  WK-KINGAKU-DEN           PIC      S9(09).
*
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSK0022L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0022L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSK0022L".
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
     03  FILLER                PIC  X(08)  VALUE  "SSK0022L".
     03  FILLER                PIC  X(34)  VALUE  SPACE.
     03  FILLER                PIC  N(12)
                               VALUE NC"＜　ケーヨー受領書　＞"
                               CHARACTER  TYPE  IS PITCH-30.
     03  FILLER                PIC  X(38)  VALUE  SPACE.
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
     03  FILLER                PIC  X(120)  VALUE  SPACE.
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
     03  FILLER                PIC  N(05)  VALUE  NC"日付区分："
                               CHARACTER  TYPE IS PITCH-15.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD03-DKBN             PIC  X(01).
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD03-DKBNNM           PIC  N(03)
                               CHARACTER  TYPE IS PITCH-15.
***** 見出し行４
 01  HD04.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(05)  VALUE  NC"受信日　："
                               CHARACTER   TYPE IS   PITCH-15.
     03  HD04-JFYY             PIC  9999   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-JFMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-JFDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  N(01)  VALUE  NC"～"
                               CHARACTER  TYPE IS PITCH-15.
     03  HD04-JTYY             PIC  9999   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-JTMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-JTDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"検収日："
                               CHARACTER   TYPE IS   PITCH-15.
     03  HD04-KFYY             PIC  9999   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-KFMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-KFDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  N(01)  VALUE  NC"～"
                               CHARACTER  TYPE IS PITCH-15.
     03  HD04-KTYY             PIC  9999   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-KTMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-KTDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"納品日："
                               CHARACTER   TYPE IS   PITCH-15.
     03  HD04-NFYY             PIC  9999   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-NFMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-NFDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  N(01)  VALUE  NC"～"
                               CHARACTER  TYPE IS PITCH-15.
     03  HD04-NTYY             PIC  9999   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-NTMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-NTDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"発注日："
                               CHARACTER   TYPE IS   PITCH-15.
     03  HD04-HFYY             PIC  9999   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-HFMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-HFDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  N(01)  VALUE  NC"～"
                               CHARACTER  TYPE IS PITCH-15.
     03  HD04-HTYY             PIC  9999   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-HTMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD04-HTDD             PIC  99     VALUE  ZERO.
***** 見出し行５
 01  HD05.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(05)  VALUE  NC"伝票区分："
                               CHARACTER  TYPE IS PITCH-15.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD05-DENK1            PIC  X(02).
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD05-DENK2            PIC  X(02).
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD05-DENK3            PIC  X(02).
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD05-DENK4            PIC  X(02).
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD05-DENK5            PIC  X(02).
***** 見出し行６
 01  HD06.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"店舗情報"
                              CHARACTER  TYPE IS PITCH-20.
***** 見出し行７
 01  HD07.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"伝票番号"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"伝票区分"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"発注日"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"納品日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"検収日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"出荷場所"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(07)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"振分場所"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(07)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"ルート"
                              CHARACTER  TYPE IS PITCH-20.
***** 見出し行８
 01  HD08.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"行"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE  NC"ＪＡＮＣＤ"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(05)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"商品名"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(26)  VALUE  SPACE.
     03  FILLER               PIC  X(01)  VALUE  "(".
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER          PIC  N(07)  VALUE  NC"サカタ商品情報"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  X(01)  VALUE  ")".
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  X(01)  VALUE  "(".
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"出荷数量"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  ")".
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"検収数"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"原価単価"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"原価金額"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(09)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"差数"
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
     03  MD01-TENCD               PIC       99999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TENMEI              PIC       N(15).
 01  MD02                         CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-DENNO               PIC       999999999.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-DENKU               PIC       X(02).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-DNKNM               PIC       N(05).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-HACHUYYYY           PIC       9999.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  MD02-HACHUMM             PIC       99.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  MD02-HACHUDD             PIC       99.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-NOHINYYYY           PIC       9999.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  MD02-NOHINMM             PIC       99.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  MD02-NOHINDD             PIC       99.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-KENSHUYYYY          PIC       9999.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  MD02-KENSHUMM            PIC       99.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  MD02-KENSHUDD            PIC       99.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-SKBCD               PIC       X(02).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-SKBNM               PIC       N(05).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-FRBCD               PIC       X(02).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-FRBNM               PIC       N(05).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-ROUTE               PIC       X(08).
 01  MD03.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD03-GYO                 PIC       Z9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD03-JANCD               PIC       X(13).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD03-SHONM1              PIC       X(15).
     03  MD03-SHONM2              PIC       X(15).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       X(01)  VALUE "(".
     03  MD03-SHOCD               PIC       X(08).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD03-HINTAN1             PIC       X(05).
     03  FILLER                   PIC       X(01)  VALUE "-".
     03  MD03-HINTAN2             PIC       X(02).
     03  FILLER                   PIC       X(01)  VALUE "-".
     03  MD03-HINTAN3             PIC       X(01).
     03  FILLER                   PIC       X(01)  VALUE ")".
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       X(01)  VALUE "(".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD03-SHUKASU             PIC       --,--9.9.
     03  FILLER                   PIC       X(01)  VALUE ")".
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD03-KENSHUSU            PIC       --,--9.9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD03-GENKA               PIC       -,---,--9.99.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD03-KINGAKU             PIC       ---,---,--9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       X(01)  VALUE "(".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD03-SASU                PIC       --,--9.99.
     03  FILLER                   PIC       X(01)  VALUE ")".
***** 合計行
 01  GK01.
     03  FILLER                   PIC     X(69)  VALUE  SPACE.
     03  GK01-TAITOL              PIC     N(02) VALUE  NC"合計"
                               CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                   PIC     X(03)  VALUE  SPACE.
     03  FILLER                   PIC     X(01)  VALUE "(".
     03  GK01-SHUKASU             PIC     ---,--9.9.
     03  FILLER                   PIC     X(01)  VALUE ")".
     03  FILLER                   PIC     X(01)  VALUE  SPACE.
     03  GK01-KENSHUSU            PIC     ---,--9.9.
     03  FILLER                   PIC     X(16)  VALUE  SPACE.
     03  GK01-KINGAKU             PIC     ---,---,--9.
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
                        PROCEDURE ZSOKMS1.
     MOVE     "ZSOKMS1 "          TO        ERR-FL-ID.
     MOVE     SOK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
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
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSK0022L-START         SECTION.
*
     MOVE   "SSK0022L-START"      TO   S-NAME.
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
 SSK0022L-END.
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
     OPEN     INPUT     ZSOKMS1.
     OPEN     INPUT     JYOKEN1.
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
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
     ACCEPT    SYSTEM-TIME       FROM      TIME.
*
     MOVE  SPACE                TO   JYW-REC.
     INITIALIZE                      JYW-REC.
*
     START  KEIJYWL2  KEY  >=   JYW-F04  JYW-F05  JYW-F06
         INVALID   KEY
            MOVE     "END"      TO   END-FLG
            DISPLAY NC"＃対象データ無し１＃" UPON CONS
            GO                  TO   INIT-EXIT
     END-START
*
     PERFORM  KEIJYWL2-RD-SEC.
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
     PERFORM   MIDASISET-SEC.
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
*
 KEIJYWL2-RD-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO    S-NAME.
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
           MOVE ALL NC"＊"      TO    MD01-TENMEI
         ELSE
           MOVE TEN-F02         TO    MD01-TENMEI
         END-IF
         MOVE  "1"              TO    RD1-FLG
     END-IF.
*  明細行編集２
*  伝票番号
     MOVE     JYW-F05              TO        MD02-DENNO.
*  伝区
     MOVE     JYW-F07              TO        MD02-DENKU.
*  発注日
     MOVE     JYW-F24(1:4)         TO        MD02-HACHUYYYY.
     MOVE     JYW-F24(5:2)         TO        MD02-HACHUMM.
     MOVE     JYW-F24(7:2)         TO        MD02-HACHUDD.
*  納品日
     MOVE     JYW-F09(1:4)         TO        MD02-NOHINYYYY.
     MOVE     JYW-F09(5:2)         TO        MD02-NOHINMM.
     MOVE     JYW-F09(7:2)         TO        MD02-NOHINDD.
*  検収日
     MOVE     JYW-F10(1:4)         TO        MD02-KENSHUYYYY.
     MOVE     JYW-F10(5:2)         TO        MD02-KENSHUMM.
     MOVE     JYW-F10(7:2)         TO        MD02-KENSHUDD.
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
              MOVE  ALL NC"＊"     TO        MD02-DNKNM
         ELSE
              MOVE  JYO-F03        TO        MD02-DNKNM
         END-IF
*  出荷場所名
         MOVE     JYW-F27           TO    SOK-F01
         PERFORM  ZSOKMS1-READ-SEC
         IF   ZSOKMS1-INV-FLG = "INV"
              MOVE  ALL NC"＊"     TO        MD02-SKBNM
         ELSE
              MOVE  SOK-F02        TO        MD02-SKBNM
         END-IF
*  振分場所名
         MOVE     JYW-F29           TO    SOK-F01
         PERFORM  ZSOKMS1-READ-SEC
         IF   ZSOKMS1-INV-FLG = "INV"
              MOVE  ALL NC"＊"     TO        MD02-FRBNM
         ELSE
              MOVE  SOK-F02        TO        MD02-FRBNM
         END-IF
         MOVE  "1"              TO    RD2-FLG
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
*
*--------------
*  明細行出力
*--------------
*  改頁チェック
     IF       L-CNT     >    54
              PERFORM  MIDASI-SEC
              PERFORM  HED-SEC
              MOVE  JYW-F04       TO    WRT-TENCD
              MOVE  JYW-F05       TO    WRT-DENNO
              GO    TO     KEIJYWL2-RD
     END-IF.
     IF   ( JYW-F04   NOT =   WRT-TENCD )
         WRITE  P-REC  FROM  MD01  AFTER 1
         ADD    1            TO    L-CNT
         IF       L-CNT     >    54
              PERFORM  MIDASI-SEC
              PERFORM  HED-SEC
         ELSE
              WRITE  P-REC  FROM  MD02  AFTER 1
              ADD    1            TO    L-CNT
              IF       L-CNT     >    54
                  PERFORM  MIDASI-SEC
                  PERFORM  HED-SEC
              END-IF
         END-IF
         MOVE  JYW-F04       TO    WRT-TENCD
         MOVE  JYW-F05       TO    WRT-DENNO
     ELSE
         IF      ( JYW-F05   NOT =   WRT-DENNO )
             WRITE  P-REC  FROM  MD02  AFTER 1
             ADD    1            TO    L-CNT
             IF       L-CNT     >    54
                  PERFORM  MIDASI-SEC
                  PERFORM  HED-SEC
             END-IF
             MOVE  JYW-F05       TO   WRT-DENNO
         END-IF
     END-IF.
 KEIJYWL2-RD.
     WRITE  P-REC  FROM  MD03  AFTER 1.
     ADD    1            TO   L-CNT.
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
     MOVE     SYS-YY            TO   HD01-YY.
     MOVE     SYS-MM            TO   HD01-MM.
     MOVE     SYS-DD            TO   HD01-DD.
     MOVE     SYS-HH            TO   HD02-HH.
     MOVE     SYS-MN            TO   HD02-MN.
     MOVE     SYS-SS            TO   HD02-SS.
*日付区分・日付
     MOVE     PARA-DKBN         TO   HD03-DKBN.
     EVALUATE   PARA-DKBN
          WHEN   "1"
              MOVE     NC"受信日"         TO   HD03-DKBNNM
              MOVE     PARA-DFROM(1:4)    TO   HD04-JFYY
              MOVE     PARA-DFROM(5:2)    TO   HD04-JFMM
              MOVE     PARA-DFROM(7:2)    TO   HD04-JFDD
              MOVE     PARA-DTO(1:4)      TO   HD04-JTYY
              MOVE     PARA-DTO(5:2)      TO   HD04-JTMM
              MOVE     PARA-DTO(7:2)      TO   HD04-JTDD
          WHEN   "2"
              MOVE     NC"検収日"         TO   HD03-DKBNNM
              MOVE     PARA-DFROM(1:4)    TO   HD04-KFYY
              MOVE     PARA-DFROM(5:2)    TO   HD04-KFMM
              MOVE     PARA-DFROM(7:2)    TO   HD04-KFDD
              MOVE     PARA-DTO(1:4)      TO   HD04-KTYY
              MOVE     PARA-DTO(5:2)      TO   HD04-KTMM
              MOVE     PARA-DTO(7:2)      TO   HD04-KTDD
          WHEN   "3"
              MOVE     NC"納品日"         TO   HD03-DKBNNM
              MOVE     PARA-DFROM(1:4)    TO   HD04-NFYY
              MOVE     PARA-DFROM(5:2)    TO   HD04-NFMM
              MOVE     PARA-DFROM(7:2)    TO   HD04-NFDD
              MOVE     PARA-DTO(1:4)      TO   HD04-NTYY
              MOVE     PARA-DTO(5:2)      TO   HD04-NTMM
              MOVE     PARA-DTO(7:2)      TO   HD04-NTDD
          WHEN   OTHER
              MOVE     NC"発注日"         TO   HD03-DKBNNM
              MOVE     PARA-DFROM(1:4)    TO   HD04-HFYY
              MOVE     PARA-DFROM(5:2)    TO   HD04-HFMM
              MOVE     PARA-DFROM(7:2)    TO   HD04-HFDD
              MOVE     PARA-DTO(1:4)      TO   HD04-HTYY
              MOVE     PARA-DTO(5:2)      TO   HD04-HTMM
              MOVE     PARA-DTO(7:2)      TO   HD04-HTDD
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
     WRITE    P-REC     FROM      HD04      AFTER     1.
     WRITE    P-REC     FROM      HD05      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
     WRITE    P-REC     FROM      HD06      AFTER     1.
     WRITE    P-REC     FROM      HD07      AFTER     1.
     WRITE    P-REC     FROM      HD08      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
*
     MOVE     13         TO        L-CNT.
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
     WRITE    P-REC     FROM      MD02      AFTER     1.
     ADD      2          TO        L-CNT.
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
*  タイトル転送
     MOVE     NC"合計"           TO   GK01-TAITOL.
*  出荷数　
     MOVE     WK-SHUKASU-DEN     TO   GK01-SHUKASU.
*  検収数
     MOVE     WK-KENSHUSU-DEN    TO   GK01-KENSHUSU.
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
         IF      ( JYW-F04   NOT =   BRK-TENCD )
             WRITE    P-REC     FROM  SEN2  AFTER 1
         ELSE
             WRITE    P-REC     FROM  SEN3  AFTER 1
         END-IF
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
*             倉庫マスタ読込
***************************************************************
 ZSOKMS1-READ-SEC       SECTION.
*
     MOVE    "ZSOKMS1-READ-SEC" TO        S-NAME.
*
     READ     ZSOKMS1
              INVALID      MOVE  "INV"    TO   ZSOKMS1-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   ZSOKMS1-INV-FLG
     END-READ.
*
 ZSOKMS1-READ-EXIT.
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
     CLOSE    KEIJYWL2 TENMS1 ZSOKMS1 JYOKEN1  PRINTF.
*
 END-EXIT.
     EXIT.
*
