# SSY3779L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3779L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷依頼リスト出力　　　　　*
*    業務名　　　　　　　：　出荷処理                         *
*    モジュール名　　　　：　出荷依頼リスト出力（店品別）      *
*    作成日／更新日　　　：　10/10/13                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　出荷依頼リストを出力する。　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY3779L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/10/13.
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
****<<基本情報Ｆ　管理番号用 >>*********************************
     SELECT   NFJOHOF9           ASSIGN    TO   DA-01-VI-NFJOHOL9
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JH1-F01  JH1-F05
                                                JH1-F09  JH1-F06
                                                JH1-F13
                                 STATUS         JH1-STATUS.
****<<基本情報Ｆ　バッチ_用 >>********************************
     SELECT   NFJOHOF8           ASSIGN    TO   DA-01-VI-NFJOHOL8
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JH2-F02  JH2-F03
                                                JH2-F04  JH2-F05
                                                JH2-F09  JH2-F06
                                                JH2-F13
                                 STATUS         JH2-STATUS.
*
****<<店舗マスタ　　　　　　 >>*********************************
     SELECT   TENMS1             ASSIGN    TO   DA-01-VI-TENMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TEN-F52  TEN-F011
                                 STATUS         TEN-STATUS.
*
****<<商品変換テーブル　　　 >>*********************************
     SELECT   HSHOTBL             ASSIGN    TO   DA-01-VI-SHOTBL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    SHT-F01  SHT-F02
                                 STATUS         SHT-STATUS.
*
****<<ナフコ用商品名称マスタ >>*********************************
     SELECT   NFSHOMS             ASSIGN    TO   DA-01-VI-NFSHOMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    SHO-F01
                                 STATUS         SHO-STATUS.
*
****<<条件マスタ　　　　　　 >>*********************************
     SELECT   JYOKEN             ASSIGN    TO   DA-01-VI-JYOKEN1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    JYO-F01  JYO-F02
                                 STATUS         JYO-STATUS.
*
****<<作場マスタ　　　　　　 >>*********************************
     SELECT   SAKUBAF            ASSIGN    TO   DA-01-VI-SAKUBAL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    SAK-F01
                                 STATUS         SAK-STATUS.
*
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
*    FILE = ナフコ　　基本情報ファイル　管理_用
*--------------------------------------------------------------*
 FD  NFJOHOF9           LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOF   OF        XFDLIB
              JOINING   JH1       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ　　基本情報ファイル　バッチ_用
*--------------------------------------------------------------*
 FD  NFJOHOF8           LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOF   OF        XFDLIB
              JOINING   JH2       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 店舗マスタ　　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ　　商品変換ＴＢＬ　　　                     *
*--------------------------------------------------------------*
 FD  HSHOTBL             LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL    OF        XFDLIB
              JOINING   SHT       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ　　商品名称マスタ　　　                     *
*--------------------------------------------------------------*
 FD  NFSHOMS             LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS    OF        XFDLIB
              JOINING   SHO       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ　　条件マスタ　　　　　                     *
*--------------------------------------------------------------*
 FD  JYOKEN              LABEL RECORD   IS   STANDARD.
     COPY     JYOKEN1    OF        XFDLIB
              JOINING   JYO       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ　　作場マスタ　　　　　                     *
*--------------------------------------------------------------*
 FD  SAKUBAF             LABEL RECORD   IS   STANDARD.
     COPY     SAKUBAF    OF        XFDLIB
              JOINING   SAK       PREFIX.
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
*  作業用 基本情報ファイル.
     COPY     NFJOHOF   OF        XFDLIB
              JOINING   JH3       PREFIX.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  CHK-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  JH1-STATUS                   PIC       X(02).
 01  JH2-STATUS                   PIC       X(02).
 01  TEN-STATUS                   PIC       X(02).
 01  SHO-STATUS                   PIC       X(02).
 01  SHT-STATUS                   PIC       X(02).
 01  JYO-STATUS                   PIC       X(02).
 01  SAK-STATUS                   PIC       X(02).
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
*
 01  WK-BACHINO.
     03  WK-BACHI-YMD             PIC       9(08)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  WK-BACHI-TIME            PIC       9(04)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  WK-BACHI-TORICD          PIC       9(08)  VALUE  ZERO.
*
 01  BRK-KEY.
     03  BRK-SAKCD                PIC       X(02)  VALUE  SPACE.
     03  BRK-SHOCD                PIC       X(08)  VALUE  SPACE.
     03  BRK-TENCD                PIC       9(05)  VALUE  ZERO.
     03  BRK-NOUDT                PIC       9(08)  VALUE  ZERO.
*
 01  WK-AREA.
     03  WK-SURYO                 PIC       9(09)V9(01).
     03  WK-GENKA                 PIC       9(09)V9(02).
     03  WK-KINGAKU               PIC       9(09).
*
 01  WK-KEY.
     03  WK-HATYU-DATE            PIC       9(08)  VALUE  ZERO.
     03  WK-NOUHN-DATE            PIC       9(08)  VALUE  ZERO.
     03  WK-TENPO-CD              PIC       9(04)  VALUE  ZERO.
     03  WK-DENPYO-NO             PIC       9(07)  VALUE  ZERO.
     03  WK-DENPYO-NO1            PIC       9(07)  VALUE  ZERO.
*
*--------------
 01  WK-GOKEI-TEN.
     03  WK-HSURYO-TEN            PIC       9(08).
     03  WK-TSURYO-TEN            PIC       9(08).
     03  WK-SOTEISU-TEN           PIC       9(08)V9.
     03  WK-SYUSEISU-TEN          PIC       9(08)V9.
*
 01  WK-GOKEI-TOTAL.
     03  WK-HSURYO-TAL            PIC       9(08).
     03  WK-TSURYO-TAL            PIC       9(08).
     03  WK-SOTEISU-TAL           PIC       9(08)V9.
     03  WK-SYUSEISU-TAL          PIC       9(08)V9.
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3779L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3779L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY3779L".
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
     03  FILLER                PIC  X(08)  VALUE  "SSY3779L".
     03  FILLER                PIC  X(31)  VALUE  SPACE.
     03  FILLER                PIC  N(15)
            VALUE NC"＜ナフコ作場別出荷依頼リスト＞"
                      CHARACTER  TYPE  IS PITCH-30.
     03  FILLER                PIC  X(26)  VALUE  SPACE.
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
     03  HD02-SYUBETUX  CHARACTER  TYPE IS   PITCH-30.
       05  FILLER              PIC  N(01)  VALUE  NC"『".
       05  HD02-SYUBETU        PIC  N(03)  VALUE  SPACE.
       05  FILLER              PIC  N(01)  VALUE  NC"』".
     03  FILLER                PIC  X(15)  VALUE  SPACE.
     03  FILLER              PIC  N(06)  VALUE  NC"（管理番号："
                               CHARACTER   TYPE IS   PITCH-20.
     03  HD02-KANRINO          PIC  X(08).
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(05)  VALUE  NC"バッチ_："
                               CHARACTER   TYPE IS   PITCH-20.
     03  HD02-BACHINO          PIC  X(22).
     03  FILLER                PIC  N(01)  VALUE  NC"）"
                               CHARACTER   TYPE IS   PITCH-20.
***** 見出し行３
 01  HD03                      CHARACTER   TYPE IS   PITCH-20.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"作場名：".
     03  HD03-SAKUBCD          PIC  X(02).
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD03-SAKBMEI          PIC  N(10).
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(06)  VALUE
                               NC"納品指定日：".
     03  HD03-NOUYMD           PIC  9(08).
***** 見出し行４
 01  HD04          CHARACTER      TYPE IS     PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"店舗情報".
     03  FILLER               PIC  X(185) VALUE  SPACE.
*
***** 見出し行５
 01  HD05          CHARACTER      TYPE IS     PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"相手商品".
     03  FILLER               PIC  X(04)  VALUE  "(ｻｶﾀ".
     03  FILLER               PIC  N(02)  VALUE  NC"商品".
     03  FILLER               PIC  X(01)  VALUE  ")".
     03  FILLER               PIC  X(07)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE
                              NC"商　品　名".
     03  FILLER               PIC  X(180) VALUE  SPACE.
*
 01  HD06          CHARACTER      TYPE IS     PITCH-20.
     03  FILLER               PIC  N(01)  VALUE  NC"（".
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE
                              NC"ＪＡＮＣＤ".
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"）".
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"発単".
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE
                              NC"規　格　名".
     03  FILLER               PIC  X(17)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"発注数".
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"訂正数".
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"想定箱数".
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"修正箱数".
     03  FILLER               PIC  X(07)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"店入荷日".
     03  FILLER               PIC  X(08)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"出荷日".
     03  FILLER               PIC  X(05)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"訂正".
     03  FILLER               PIC  X(180) VALUE  SPACE.
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
         "                          --------------".
     03  FILLER                   PIC       X(40)  VALUE
         "----------------------------------------".
     03  FILLER                   PIC       X(40)  VALUE
         "----------------------------------------".
     03  FILLER                   PIC       X(16)  VALUE
         "----------------".
*
***** 線
 01  SEN3.
     03  FILLER                   PIC       X(40)  VALUE
         "----------------------------------------".
     03  FILLER                   PIC       X(40)  VALUE
         "----------------------------------------".
     03  FILLER                   PIC       X(40)  VALUE
         "----------------------------------------".
     03  FILLER                   PIC       X(16)  VALUE
         "----------------".
*
***** 明細行１
 01  MD01            CHARACTER      TYPE IS     PITCH-15.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TENCD               PIC       XXXXX.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TENMEI              PIC       N(16).
     03  FILLER                   PIC       X(180) VALUE  SPACE.
*
***** 明細行２
 01  MD02            CHARACTER      TYPE IS     PITCH-15.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-SHOCD               PIC       XXXXXXXX.
     03  FILLER                   PIC       X(01)  VALUE  "(".
     03  MD02-SAKTCD              PIC       XXXXXXXX.
     03  FILLER                   PIC       X(01)  VALUE  ")".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-SHOHIN              PIC       N(16).
     03  FILLER                   PIC       X(180) VALUE  SPACE.
*
***** 明細行３
 01  MD03.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       X(01)  VALUE  "(".
     03  MD03-JANCD               PIC       XXXXXXXXXXXXX.
     03  FILLER                   PIC       X(01)  VALUE  ")".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       X(01)  VALUE  "(".
     03  MD03-TANI                PIC       ZZZ,ZZ9.
     03  FILLER                   PIC       X(01)  VALUE  ")".
     03  MD03-KIKAKU              PIC       N(16)
                                  CHARACTER TYPE IS PITCH-15.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD03-HSURYO              PIC       ZZZ,ZZ9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD03-KAKKOS1             PIC       N(01)
                                  CHARACTER TYPE IS PITCH-20.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD03-TSURYO              PIC       ZZZ,ZZZ.
     03  MD03-KAKKOE1             PIC       N(01)
                                  CHARACTER TYPE IS PITCH-20.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD03-KAKKOS2             PIC       N(01)
                                  CHARACTER TYPE IS PITCH-20.
     03  MD03-SOTEISU             PIC       ZZZ,ZZZ.Z.
     03  MD03-KAKKOE2             PIC       N(01)
                                  CHARACTER TYPE IS PITCH-20.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD03-KAKKOS3             PIC       N(01)
                                  CHARACTER TYPE IS PITCH-20.
     03  MD03-SYUSEISU            PIC       ZZZ,ZZZ.Z.
     03  MD03-KAKKOE3             PIC       N(01)
                                  CHARACTER TYPE IS PITCH-20.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD03-KAKKOS4             PIC       N(01)
                                  CHARACTER TYPE IS PITCH-20.
     03  MD03-TENYMD              PIC       99999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD03-KAKKOE4             PIC       N(01)
                                  CHARACTER TYPE IS PITCH-20.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD03-KAKKOS5             PIC       N(01)
                                  CHARACTER TYPE IS PITCH-20.
     03  MD03-SYUYMD              PIC       99999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD03-KAKKOE5             PIC       N(01)
                                  CHARACTER TYPE IS PITCH-20.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD03-KAKKOS6             PIC       N(01)
                                  CHARACTER TYPE IS PITCH-20.
     03  MD03-TEIKBN              PIC       XX.
     03  MD03-KAKKOE6             PIC       N(01)
                                  CHARACTER TYPE IS PITCH-20.
     03  FILLER                   PIC       X(180) VALUE  SPACE.
*
***** 合計行
 01  GK01             CHARACTER      TYPE IS     PITCH-20.
     03  FILLER                   PIC       X(41)  VALUE  SPACE.
     03  GK01-TAITOL              PIC       N(03).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  GK01-HSURYO              PIC       ZZ,ZZZ,ZZ9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  GK01-TSURYO              PIC       ZZ,ZZZ,ZZ9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  GK01-SOTEISU             PIC       ZZ,ZZZ,ZZZ.Z.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  GK01-SYUSEISU            PIC       ZZ,ZZZ,ZZZ.Z.
     03  FILLER                   PIC       X(180) VALUE  SPACE.
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
 01  PARA-KANRINO           PIC   9(08).
 01  PARA-BACHI-YMD         PIC   9(08).
 01  PARA-BACHI-TIME        PIC   9(04).
 01  PARA-BACHI-TORICD      PIC   9(08).
 01  PARA-SAKUCD            PIC   X(02).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-KBN
                                           PARA-KANRINO
                                           PARA-BACHI-YMD
                                           PARA-BACHI-TIME
                                           PARA-BACHI-TORICD
                                           PARA-SAKUCD.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFJOHOF9.
     MOVE     "NFJOHOF9"          TO        ERR-FL-ID.
     MOVE     JH1-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFJOHOF8.
     MOVE     "NFJOHOF8"          TO        ERR-FL-ID.
     MOVE     JH2-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TENMS1.
     MOVE     "TENMS1  "          TO        ERR-FL-ID.
     MOVE     TEN-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC4         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE HSHOTBL.
     MOVE     "HSHOTBL "          TO        ERR-FL-ID.
     MOVE     SHT-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC5         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFSHOMS.
     MOVE     "NFSHOMS "          TO        ERR-FL-ID.
     MOVE     SHO-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC6         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE SAKUBAF.
     MOVE     "SAKUBAF "          TO        ERR-FL-ID.
     MOVE     SAK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY3779L-START         SECTION.
*
     MOVE   "SSY3779L-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
*
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SSY3779L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     NFJOHOF9.
     OPEN     INPUT     NFJOHOF8.
     OPEN     INPUT     TENMS1.
     OPEN     INPUT     HSHOTBL.
     OPEN     INPUT     NFSHOMS.
     OPEN     INPUT     SAKUBAF.
     OPEN     OUTPUT    PRINTF.
*
     MOVE     ZERO           TO    WK-GOKEI-TEN
                                   WK-GOKEI-TOTAL.
     INITIALIZE                    WK-GOKEI-TEN
                                   WK-GOKEI-TOTAL.
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
*
     IF   PARA-KANRINO  NOT =  ZERO
*
          MOVE  SPACE                TO   JH1-REC
          INITIALIZE                      JH1-REC
          MOVE  PARA-KANRINO         TO   JH1-F01
          MOVE  PARA-SAKUCD          TO   JH1-F05
*
          START  NFJOHOF9  KEY  >=   JH1-F01  JH1-F05  JH1-F09
                                     JH1-F06  JH1-F13
                 INVALID   KEY
                 MOVE     "END"      TO   END-FLG
                 GO                  TO   INIT-EXIT
          END-START
*
          IF       END-FLG   =   "END"
                   DISPLAY NC"＃対象データ１無し＃" UPON CONS
                   GO                  TO   INIT-EXIT
          ELSE
              PERFORM  NFJOHOF9-RD-SEC
              IF  END-FLG   =   "END"
                  DISPLAY NC"＃対象データ無し２＃" UPON CONS
                  GO                  TO   INIT-EXIT
              ELSE
                  MOVE  JH3-F05    TO       BRK-SAKCD
                  MOVE  JH3-F13    TO       BRK-SHOCD
                  MOVE  JH3-F06    TO       BRK-TENCD
                  MOVE  JH3-F09    TO       BRK-NOUDT
                  MOVE    "1"         TO    PRT-FLG
              END-IF
          END-IF
     ELSE
          IF  PARA-BACHI-YMD     NOT =  ZERO  AND
              PARA-BACHI-TIME    NOT =  ZERO  AND
              PARA-BACHI-TORICD  NOT =  ZERO
*
             MOVE  SPACE                TO   JH2-REC
             INITIALIZE                      JH2-REC
             MOVE  PARA-BACHI-YMD       TO   JH2-F02
             MOVE  PARA-BACHI-TIME      TO   JH2-F03
             MOVE  PARA-BACHI-TORICD    TO   JH2-F04
             MOVE  PARA-SAKUCD          TO   JH2-F05
*
             START  NFJOHOF8  KEY  >=   JH2-F02  JH2-F03  JH2-F04
                                        JH2-F05  JH2-F09  JH2-F06
                                        JH2-F13
                 INVALID   KEY
                 MOVE     "END"      TO   END-FLG
             END-START
*
             IF      END-FLG   =   "END"
                     DISPLAY NC"＃対象データ無し３＃" UPON CONS
                     GO                  TO   INIT-EXIT
             ELSE
                 PERFORM  NFJOHOF8-RD-SEC
                 IF  END-FLG   =   "END"
                     DISPLAY NC"＃対象データ無し４＃" UPON CONS
                     GO                  TO   INIT-EXIT
                 ELSE
                     MOVE  JH3-F05    TO       BRK-SAKCD
                     MOVE  JH3-F13    TO       BRK-SHOCD
                     MOVE  JH3-F06    TO       BRK-TENCD
                     MOVE  JH3-F09    TO       BRK-NOUDT
                     MOVE    "1"         TO    PRT-FLG
                 END-IF
             END-IF
          ELSE
              DISPLAY NC"＃パラメータ異常＃" UPON CONS
              MOVE  "4010"       TO   PROGRAM-STATUS
              MOVE  "END"        TO   END-FLG
          END-IF
     END-IF.
*
*見出し出力
     PERFORM  MIDASI-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO    S-NAME.
*  改頁チェック
     IF       L-CNT     >    58
              PERFORM  MIDASI-SEC
              MOVE    "1"         TO    PRT-FLG
     END-IF.
*出力領域初期化
     MOVE     SPACE               TO    MD03.
*カッコを転送
     MOVE     NC"【"              TO    MD03-KAKKOS1.
*    MOVE     NC"【"              TO    MD03-KAKKOS2.
     MOVE     NC"【"              TO    MD03-KAKKOS3.
     MOVE     NC"＜"              TO    MD03-KAKKOS4.
     MOVE     NC"＜"              TO    MD03-KAKKOS5.
     MOVE     NC"＜"              TO    MD03-KAKKOS6.
     MOVE     NC"】"              TO    MD03-KAKKOE1.
*    MOVE     NC"】"              TO    MD03-KAKKOE2.
     MOVE     NC"】"              TO    MD03-KAKKOE3.
     MOVE     NC"＞"              TO    MD03-KAKKOE4.
     MOVE     NC"＞"              TO    MD03-KAKKOE5.
     MOVE     NC"＞"              TO    MD03-KAKKOE6.
*  作場ＣＤブレイク時
     IF       JH3-F05   NOT =   BRK-SAKCD
     OR       JH3-F09   NOT =   BRK-NOUDT
*      合計行出力
              PERFORM   TENGOKEI-SEC
*      総合計出力
              PERFORM   SOUGOKEI-SEC
              MOVE   ZERO         TO    WK-GOKEI-TOTAL
              INITIALIZE                WK-GOKEI-TOTAL
*      線出力
              WRITE    P-REC      FROM  SEN2  AFTER 1
              ADD      1          TO    L-CNT
*
              MOVE     99             TO    L-CNT
              PERFORM   MIDASI-SEC
              MOVE    "1"         TO    PRT-FLG
*
              MOVE    ZERO        TO    WK-GOKEI-TEN
                                        WK-GOKEI-TOTAL
              INITIALIZE                WK-GOKEI-TEN
                                        WK-GOKEI-TOTAL
*
              MOVE  JH3-F05    TO       BRK-SAKCD
              MOVE  JH3-F13    TO       BRK-SHOCD
              MOVE  JH3-F06    TO       BRK-TENCD
              MOVE  JH3-F09    TO       BRK-NOUDT
     END-IF.
*
*  店舗ＣＤブレイク時（明細１行目出力）
     IF       JH3-F06   NOT =   BRK-TENCD   OR
              PRT-FLG       =   "1"
              MOVE    SPACE       TO    PRT-FLG
*      合計行出力
              IF   L-CNT  >  10
                   PERFORM   TENGOKEI-SEC
              END-IF
**************MOVE   ZERO         TO    WK-GOKEI-TEN
**************INITIALIZE                WK-GOKEI-TEN
*
              MOVE    JH3-F06              TO   MD01-TENCD
*
              MOVE    WK-BACHI-TORICD      TO   TEN-F52
              MOVE    JH3-F06              TO   TEN-F011
*
              READ     TENMS1
                   INVALID  KEY
                       MOVE  ALL NC"＊"    TO   MD01-TENMEI
                   NOT INVALID  KEY
                       MOVE  TEN-F03       TO   MD01-TENMEI
              END-READ
*
              WRITE    P-REC      FROM  MD01
              ADD      1          TO    L-CNT
*
              MOVE     JH3-F06    TO    BRK-TENCD
     END-IF.
*
*  明細行２設定
     MOVE     JH3-F13            TO  MD02-SHOCD
     MOVE     JH3-F15            TO  MD02-SAKTCD
     MOVE     JH3-F13            TO  SHO-F01
     READ    NFSHOMS
             INVALID  KEY
                 MOVE   ALL  NC"＊"   TO  MD02-SHOHIN
                 MOVE   ALL  NC"＊"   TO  MD03-KIKAKU
             NOT INVALID  KEY
                 MOVE    SHO-F05      TO  MD02-SHOHIN
                 MOVE    SHO-F06      TO  MD03-KIKAKU
     END-READ.
     MOVE   JH3-F14              TO  MD03-JANCD
*
     MOVE   JH3-F18              TO  MD03-TANI
*
*
*  発注数量
     MOVE     JH3-F19              TO        MD03-HSURYO.
*  訂正数
     IF       JH3-F19  NOT =  JH3-F20
              MOVE     JH3-F20     TO        MD03-TSURYO
     END-IF.
*  想定箱数
     MOVE     JH3-F21              TO        MD03-SOTEISU.
*  修正箱数
     IF       JH3-F21  NOT =  JH3-F22
              MOVE     JH3-F22     TO        MD03-SYUSEISU
     END-IF.
*  店入荷日
     MOVE     JH3-F11              TO        MD03-TENYMD.
*  出荷日
     MOVE     JH3-F10              TO        MD03-SYUYMD.
*  訂正区分
     MOVE     JH3-F23              TO        MD03-TEIKBN.
*--------------
*  店合計加算
*--------------
*  発注数量
     COMPUTE  WK-HSURYO-TEN   =   WK-HSURYO-TEN    +  JH3-F19.
*  訂正数
     COMPUTE  WK-TSURYO-TEN   =   WK-TSURYO-TEN    +  JH3-F20.
*  想定箱数
     COMPUTE  WK-SOTEISU-TEN  =   WK-SOTEISU-TEN   +  JH3-F21.
*  修正箱数
     COMPUTE  WK-SYUSEISU-TEN =   WK-SYUSEISU-TEN  +  JH3-F22.
*
*--------------
*  総合計加算
*--------------
*  発注数量
     COMPUTE  WK-HSURYO-TAL   =   WK-HSURYO-TAL    +  JH3-F19.
*  訂正数
     COMPUTE  WK-TSURYO-TAL   =   WK-TSURYO-TAL    +  JH3-F20.
*  想定箱数
     COMPUTE  WK-SOTEISU-TAL  =   WK-SOTEISU-TAL   +  JH3-F21.
*  修正箱数
     COMPUTE  WK-SYUSEISU-TAL =   WK-SYUSEISU-TAL  +  JH3-F22.
*
*--------------
*  明細行出力
*--------------
     WRITE  P-REC  FROM  MD02  AFTER 1.
     WRITE  P-REC  FROM  MD03  AFTER 1.
     WRITE  P-REC  FROM  SEN2  AFTER 1.
     ADD      3              TO   L-CNT.
*
*    次レコード読込み
     IF       PARA-KANRINO  NOT =  ZERO
              PERFORM  NFJOHOF9-RD-SEC
     ELSE
              PERFORM  NFJOHOF8-RD-SEC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
*
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
* 商品合計出力
     PERFORM   TENGOKEI-SEC.
* 総合計出力
     PERFORM   SOUGOKEI-SEC.
*
     DISPLAY  MSG-END   UPON CONS.
*
     CLOSE    NFJOHOF9 NFJOHOF8  TENMS1    HSHOTBL
              NFSHOMS  JYOKEN    SAKUBAF   PRINTF.
*
 END-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ　基本情報ファイル　管理_用　　
****************************************************************
 NFJOHOF9-RD-SEC            SECTION.
*
     MOVE    "NFJOHOF9-RD-SEC"    TO   S-NAME.
*
     READ     NFJOHOF9
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    NFJOHOF9-RD-EXIT
     END-READ.
*管理_のチェック
     IF   PARA-KANRINO  >  ZERO
          IF   JH1-F01   >   PARA-KANRINO
               MOVE   "END"            TO   END-FLG
               GO     TO    NFJOHOF9-RD-EXIT
          END-IF
     END-IF.
*倉庫ＣＤのチェック
     IF   PARA-SAKUCD  NOT =  SPACE
          IF   JH1-F05   NOT =  PARA-SAKUCD
               GO   TO   NFJOHOF9-RD-SEC
          END-IF
     END-IF.
*
     IF   PARA-KBN      =  "2"
          IF   JH1-F17   NOT =   "2"
               GO     TO    NFJOHOF9-RD-SEC
          END-IF
     END-IF.
*
     MOVE    JH1-REC            TO   JH3-REC.
*
 NFJOHOF9-RD-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ　基本情報ファイル　バッチ_用
****************************************************************
 NFJOHOF8-RD-SEC             SECTION.
*
     MOVE    "JOHOF2-RD-SEC"    TO   S-NAME.
*
     READ     NFJOHOF8
          AT END
              MOVE     "END"      TO        END-FLG
              GO   TO   NFJOHOF8-RD-EXIT
     END-READ.
* 出荷確定データ読飛
     IF   JH2-F26  =   "1"
          GO     TO    NFJOHOF8-RD-SEC
     END-IF.
*バッチ_チェック
     IF   JH2-F02 NOT = PARA-BACHI-YMD     OR
          JH2-F03 NOT = PARA-BACHI-TIME    OR
          JH2-F04 NOT = PARA-BACHI-TORICD
          MOVE   "END"            TO   END-FLG
          GO   TO   NFJOHOF8-RD-EXIT
     END-IF.
*倉庫ＣＤのチェック
     IF   PARA-SAKUCD  NOT =  SPACE
          IF   JH2-F05   NOT =  PARA-SAKUCD
               GO   TO   NFJOHOF8-RD-SEC
          END-IF
     END-IF.
*オンライン手書区分判断
     IF   PARA-KBN      =  "2"
          IF   JH1-F17   NOT =   "2"
               GO     TO    NFJOHOF8-RD-SEC
          END-IF
     END-IF.
*
     MOVE    JH2-REC            TO   JH3-REC.
*
 NFJOHOF8-RD-EXIT.
     EXIT.
*
****************************************************************
*             見出し出力処理                1.2                *
****************************************************************
 MIDASI-SEC             SECTION.
*
     MOVE    "MIDASI-SEC"              TO    S-NAME.
*改頁
     IF       L-CNT  >=  58
              IF  P-CNT  >  ZERO
                  MOVE   SPACE   TO   P-REC
                  WRITE  P-REC   AFTER PAGE
              END-IF
     END-IF.
*システム日付セット
     MOVE     SYS-YY            TO   HD01-YY.
     MOVE     SYS-MM            TO   HD01-MM.
     MOVE     SYS-DD            TO   HD01-DD.
*
     IF    PARA-KBN   =  "1"
           MOVE  NC"ＯＮＬ"     TO   HD02-SYUBETU
     ELSE
           MOVE  NC"手書　"     TO   HD02-SYUBETU
     END-IF.
     MOVE     PARA-KANRINO      TO   HD02-KANRINO.
     MOVE     PARA-BACHI-YMD    TO   WK-BACHI-YMD.
     MOVE     PARA-BACHI-TIME   TO   WK-BACHI-TIME.
     MOVE     PARA-BACHI-TORICD TO   WK-BACHI-TORICD.
     MOVE     WK-BACHINO        TO   HD02-BACHINO.
*  作場名称取得
     MOVE    JH3-F05            TO   HD03-SAKUBCD.
     MOVE    JH3-F05            TO   SAK-F01.
*
     READ     SAKUBAF
          INVALID  KEY
              MOVE  ALL NC"＊"    TO   HD03-SAKBMEI
          NOT INVALID  KEY
              MOVE  SAK-F02       TO   HD03-SAKBMEI
     END-READ.
*納品指定日
*****MOVE     JH3-F10           TO   HD03-NOUYMD.
     MOVE     JH3-F09           TO   HD03-NOUYMD.
*頁セット
     ADD      1              TO   P-CNT.
     MOVE     P-CNT          TO   HD01-PCNT.
*ヘッダー出力
     WRITE    P-REC     FROM      HD01      AFTER     1.
     WRITE    P-REC     FROM      HD02      AFTER     1.
     WRITE    P-REC     FROM      HD03      AFTER     2.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
     WRITE    P-REC     FROM      HD04      AFTER     1.
     WRITE    P-REC     FROM      HD05      AFTER     1.
     WRITE    P-REC     FROM      HD06      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
*
     MOVE     10        TO        L-CNT.
     MOVE     "CHK"     TO        CHK-FLG.
*
 MIDASI-EXIT.
     EXIT.
*
****************************************************************
*             店舗合計出力　                　　
****************************************************************
 TENGOKEI-SEC           SECTION.
*
     MOVE    "TENGOKEI-SEC"            TO    S-NAME.
*改頁
*****IF       L-CNT  >=  58
*             PERFORM  MIDASI-SEC
*****END-IF.
*
*--------------
*  合計転送
*--------------
*  タイトル転送
     MOVE     NC"合　計"         TO   GK01-TAITOL.
*  発注数量
     MOVE     WK-HSURYO-TEN      TO   GK01-HSURYO.
*  訂正数
     MOVE     WK-TSURYO-TEN      TO   GK01-TSURYO.
*  想定箱数
     MOVE     WK-SOTEISU-TEN     TO   GK01-SOTEISU.
*  修正箱数
     MOVE     WK-SYUSEISU-TEN    TO   GK01-SYUSEISU.
*
*商品合計出力
     WRITE    P-REC     FROM     GK01      AFTER     1.
*
     ADD      1                  TO        L-CNT.
*
     MOVE   ZERO                 TO        WK-GOKEI-TEN.
*  線出力
     WRITE    P-REC     FROM  SEN3  AFTER 1.
     ADD      1         TO    L-CNT.
*
 TENGOKEI-EXIT.
     EXIT.
*
*
****************************************************************
*             総合計出力　　                　　
****************************************************************
 SOUGOKEI-SEC           SECTION.
*
     MOVE    "SOUGOKEI-SEC"            TO    S-NAME.
*改頁
*****IF       L-CNT  >=  58
*             PERFORM  MIDASI-SEC
*****END-IF.
*
*--------------
*  合計転送
*--------------
*  タイトル転送
     MOVE     NC"総合計"         TO   GK01-TAITOL.
*  発注数量
     MOVE     WK-HSURYO-TAL      TO   GK01-HSURYO.
*  訂正数
     MOVE     WK-TSURYO-TAL      TO   GK01-TSURYO.
*  想定箱数
     MOVE     WK-SOTEISU-TAL     TO   GK01-SOTEISU.
*  修正箱数
     MOVE     WK-SYUSEISU-TAL    TO   GK01-SYUSEISU.
*
*合計出力
     WRITE    P-REC     FROM      GK01      AFTER     1.
*
     MOVE     1         TO        L-CNT.
*
 SOUGOKEI-EXIT.
     EXIT.

```
