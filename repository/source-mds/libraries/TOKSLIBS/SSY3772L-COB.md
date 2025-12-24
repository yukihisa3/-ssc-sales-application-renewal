# SSY3772L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3772L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩシステム　　　　　　*
*    業務名　　　　　　　：　支払処理                          *
*    モジュール名　　　　：　支払明細書出力　　　　　　　      *
*    作成日／更新日　　　：　10/10/18                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　支払明細書を出力する。　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY3772L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/10/18.
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
****<<支払明細ファイル       >>********************************
     SELECT   NFSIHAF            ASSIGN    TO   DA-01-VI-NFSIHAL2
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    SIH-F05  SIH-F04
                                                SIH-F03  SIH-F01
                                                SIH-F02  SIH-F06
                                                SIH-F08
                                 STATUS         SIH-STATUS.
*
****<<条件マスタ　　　　　　 >>*********************************
     SELECT   JYOKEN             ASSIGN    TO   DA-01-VI-JYOKEN1
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
*    FILE = ナフコ　　支払明細ファイル　　　　　               *
*--------------------------------------------------------------*
 FD  NFSIHAF            LABEL RECORD   IS   STANDARD.
     COPY     NFSIHAF   OF        XFDLIB
              JOINING   SIH       PREFIX.
*
*--------------------------------------------------------------*
 FD  JYOKEN              LABEL RECORD   IS   STANDARD.
     COPY     JYOKEN1    OF        XFDLIB
              JOINING   JYO       PREFIX.
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
*
**** ステイタス　エリア
 01  SIH-STATUS                   PIC       X(02).
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
*
 01  WK-AREA.
     03  IX1                      PIC       9(02)  VALUE  ZERO.
     03  SET-FLG                  PIC       X(01)  VALUE  SPACE.
     03  PRT-FLG                  PIC       X(01)  VALUE  SPACE.
*
 01  WK-KEY.
     03  WK-KEY1                  PIC       9(02)  VALUE  ZERO.
     03  WK-KEY2                  PIC       9(02)  VALUE  ZERO.
     03  FILLER                   PIC       X(08)  VALUE  SPACE.
*
***** カウンタ
 01  P-CNT                        PIC       9(04)  VALUE  ZERO.
 01  L-CNT                        PIC       9(03)  VALUE  ZERO.
 01  CNT-READ                     PIC       9(06)  VALUE  ZERO.
 01  MEI-CNT                      PIC       9(05)  VALUE  ZERO.
*
 01  WK-YMD.
     03  WK-YY                    PIC       9(04)  VALUE  ZERO.
     03  WK-MM                    PIC       9(02)  VALUE  ZERO.
     03  WK-DD                    PIC       9(02)  VALUE  ZERO.
*
 01  WK-YMDR.
     03  WK-YYR                   PIC       9(04)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  WK-MMR                   PIC       9(02)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  WK-DDR                   PIC       9(02)  VALUE  ZERO.
*
 01  BRK-KEY.
     03  BRK-F03                  PIC       9(06)  VALUE  ZERO.
     03  BRK-F04                  PIC       9(08)  VALUE  ZERO.
     03  BRK-F08                  PIC       9(08)  VALUE  ZERO.
     03  BRK-F01                  PIC       9(08)  VALUE  ZERO.
     03  BRK-F05                  PIC       9(02)  VALUE  ZERO.
     03  BRK-TORIMEI              PIC       N(20)  VALUE  SPACE.
*
 01  WK-AREA.
     03  WK-ZEI-RITU              PIC       9(04)V9(01).
     03  WK-RITU                  PIC       9(03)V9(01).
     03  WK-NEBIKI                PIC       9(04)V9(02).
     03  WK-NEBIRITU              PIC       9(03)V9(02).
*
*--------------
 01  WK-TENKEI.
     03  WK-GENKA1-TEN            PIC       9(10).
     03  WK-GENKA2-TEN            PIC       9(10).
     03  WK-GENTAN1-TEN           PIC       9(10).
     03  WK-GENTAN2-TEN           PIC       9(10).
     03  WK-NEBIKI1-TEN           PIC       9(10).
     03  WK-NEBIKI2-TEN           PIC       9(10).
     03  WK-SHKGENKA-TEN          PIC       9(10).
*
 01  WK-TRIKEI.
     03  WK-GENKA1-TRI            PIC       9(10).
     03  WK-GENKA2-TRI            PIC       9(10).
     03  WK-GENTAN1-TRI           PIC       9(10).
     03  WK-GENTAN2-TRI           PIC       9(10).
     03  WK-NEBIKI1-TRI           PIC       9(10).
     03  WK-NEBIKI2-TRI           PIC       9(10).
     03  WK-SHKGENKA-TRI          PIC       9(10).
*
 01  WK-KEI.
     03  WK-GENKA1-KEI            PIC       9(10).
     03  WK-GENKA2-KEI            PIC       9(10).
     03  WK-GENTAN1-KEI           PIC       9(10).
     03  WK-GENTAN2-KEI           PIC       9(10).
     03  WK-NEBIKI1-KEI           PIC       9(10).
     03  WK-NEBIKI2-KEI           PIC       9(10).
     03  WK-SHKGENKA-KEI          PIC       9(10).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3772L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3772L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY3772L".
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
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  X(08)  VALUE  "SSY3772L".
     03  FILLER                PIC  X(28)  VALUE  SPACE.
     03  FILLER                PIC  N(13)
             VALUE NC"＜　支　払　明　細　書　＞"
                               CHARACTER  TYPE  IS PITCH-30.
     03  FILLER                PIC  X(28)  VALUE  SPACE.
     03  FILLER                PIC  N(04)
                               VALUE NC"作成日："
                               CHARACTER  TYPE  IS PITCH-15.
     03  HD01-YY               PIC  9999.
     03  FILLER                PIC  N(01)  VALUE  NC"年"
                               CHARACTER  TYPE  IS PITCH-20.
     03  HD01-MM               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"月"
                               CHARACTER  TYPE IS PITCH-20.
     03  HD01-DD               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"日"
                               CHARACTER  TYPE IS PITCH-20.
     03  FILLER                PIC  X(03)  VALUE  SPACE.
     03  HD01-PCNT             PIC  ZZ9.
     03  FILLER                PIC  N(02)  VALUE  NC"頁"
                               CHARACTER  TYPE IS PITCH-20.
***** 見出し行２
 01  HD02.
     03  FILLER                PIC  X(47)  VALUE  SPACE.
     03  FILLER                PIC  N(06)
                               VALUE  NC"＜取引種別："
                               CHARACTER  TYPE IS PITCH-20.
     03  HD02-SYUBETUS         PIC  N(04)
                               CHARACTER  TYPE IS PITCH-20.
     03  FILLER                PIC  N(04) VALUE  NC"＞"
                               CHARACTER  TYPE IS PITCH-20.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  X(180) VALUE  SPACE.
***** 見出し行３
 01  HD03.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"取引先："
                               CHARACTER  TYPE  IS PITCH-15.
     03  HD03-TORICD           PIC  999999.
     03  HD03-TORIMEI          PIC  N(20)
                               CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                PIC  X(61)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"　締日："
                               CHARACTER  TYPE  IS PITCH-15.
     03  HD03-YY               PIC  9999.
     03  FILLER                PIC  N(01)  VALUE  NC"年"
                               CHARACTER  TYPE  IS PITCH-20.
     03  HD03-MM               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"月"
                               CHARACTER  TYPE IS PITCH-20.
     03  HD03-DD               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"日"
                               CHARACTER  TYPE IS PITCH-20.
     03  FILLER                PIC  X(180) VALUE  SPACE.
***** 見出し行４
 01  HD04.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"店舗"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"店舗名称"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(20)  VALUE  SPACE.
     03  FILLER               PIC  N(06)  VALUE  NC"元伝票番号"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(12)  VALUE  SPACE.
     03  FILLER               PIC  N(08)
                              VALUE  NC"原価金額（税抜）"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(08)  VALUE
                              NC"特売原価（税抜）"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(08)  VALUE
                              NC"値引金額（税抜）"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(08)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE
                              NC"　消費税"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"税率"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"値引率　"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
**   03  FILLER               PIC  N(04)  VALUE  NC"受領日　"
**                            CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(180) VALUE  SPACE.
*
***** 見出し行５
 01  HD05.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"部門"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"部門名称"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(20)  VALUE  SPACE.
     03  FILLER               PIC  N(06)  VALUE  NC"　伝票番号"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  N(04)  VALUE  NC"受領日　"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  N(08)
                              VALUE  NC"原価金額（税込）"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(08)  VALUE
                              NC"特売原価（税込）"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(08)  VALUE
                              NC"値引金額（税込）"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(08)  VALUE
                              NC"差引原価（税込）"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"備考"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(180) VALUE  SPACE.
*
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
 01  MD01.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-TENCD               PIC       XXX.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TENMEI              PIC       N(16)
                                  CHARACTER  TYPE IS PITCH-15.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-MOTDENNO            PIC       X(08).
     03  FILLER                   PIC       X(12)  VALUE  SPACE.
     03  MD01-GENKA-KINGAKU       PIC       Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TOK-GENKA           PIC       Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-NEBI-KINGAKU        PIC       Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-ZEI-GAKU            PIC       Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-ZEIRITU             PIC       ZZ9.9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-NEBIRITU            PIC       ZZZ9.99.
     03  FILLER                   PIC       X(180) VALUE  SPACE.
*
***** 明細行
 01  MD02.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-BMNCD               PIC       XXX.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-BMNMEI              PIC       N(16)
                                  CHARACTER  TYPE IS PITCH-15.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-DENNO               PIC       X(08).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-JRYYMD              PIC       X(10).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-GENKA-KINGAKU       PIC       Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-TOK-GENKA           PIC       Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-NEBI-KINGAKU        PIC       Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-SASI-GENKA          PIC       Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-BIKO                PIC       N(13)
                                  CHARACTER  TYPE IS PITCH-15.
     03  FILLER                   PIC       X(180) VALUE  SPACE.
*
***** 合計行1
 01  GK01.
     03  FILLER                   PIC   X(52)  VALUE  SPACE.
     03  GK01-GENKA-KINGAKU       PIC   Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC   X(01)  VALUE  SPACE.
     03  GK01-TOK-GENKA           PIC   Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC   X(01)  VALUE  SPACE.
     03  GK01-NEBI-KINGAKU        PIC   Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC   X(180) VALUE  SPACE.
*
***** 合計行２
 01  GK02             CHARACTER      TYPE IS     PITCH-15.
     03  FILLER                   PIC   X(36)  VALUE  SPACE.
     03  FILLER                   PIC   X(02)  VALUE  "**".
     03  FILLER                   PIC   X(01)  VALUE  SPACE.
     03  GK02-TAITOL              PIC   N(06).
     03  FILLER                   PIC   X(01)  VALUE  SPACE.
     03  FILLER                   PIC   X(02)  VALUE  "**".
     03  FILLER                   PIC   X(01)  VALUE  SPACE.
     03  GK02-GENKA-KINGAKU       PIC   Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC   X(01)  VALUE  SPACE.
     03  GK02-TOK-GENKA           PIC   Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC   X(01)  VALUE  SPACE.
     03  GK02-NEBI-KINGAKU        PIC   Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC   X(01)  VALUE  SPACE.
     03  GK02-SAHI-GENKA          PIC   Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC   X(01)  VALUE  SPACE.
     03  GK02-BIKO                PIC   N(13).
     03  FILLER                   PIC   X(180) VALUE  SPACE.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           NFSIHAF.
     MOVE     "NFSIHAF"           TO        ERR-FL-ID.
     MOVE     SIH-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE JYOKEN.
     MOVE     "JYOKEN  "          TO        ERR-FL-ID.
     MOVE     JYO-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY3772L-START         SECTION.
*
     MOVE   "SSY3772L-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
*
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SSY3772L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     NFSIHAF.
     OPEN     INPUT     JYOKEN.
     OPEN     OUTPUT    PRINTF.
*
     MOVE     ZERO           TO    WK-TENKEI
                                   WK-TRIKEI
                                   WK-KEI.
     INITIALIZE                    WK-TENKEI
                                   WK-TRIKEI
                                   WK-KEI.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     99             TO    L-CNT.
     MOVE     1              TO    P-CNT.
*  伝票種別名設定
     EVALUATE   SIH-F05
         WHEN   1
                MOVE     NC"仕入"  TO    HD02-SYUBETUS
         WHEN   2
                MOVE     NC"仕入"  TO    HD02-SYUBETUS
         WHEN   11
                MOVE     NC"返品"  TO    HD02-SYUBETUS
         WHEN   12
                MOVE     NC"値引"  TO    HD02-SYUBETUS
         WHEN   61
                MOVE     NC"相殺"  TO    HD02-SYUBETUS
     END-EVALUATE.
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
     MOVE  SPACE                TO   SIH-REC
     INITIALIZE                      SIH-REC
*
     PERFORM  NFSIHAF-RD-SEC
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し１＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
*
*　ブレイクキー設定
     MOVE  SIH-F03    TO       BRK-F03.
     MOVE  SIH-F04    TO       BRK-F04.
     MOVE  SIH-F08    TO       BRK-F08.
     MOVE  SIH-F01    TO       BRK-F01.
     MOVE  SIH-F05    TO       BRK-F05.
     MOVE  SIH-F23    TO       BRK-TORIMEI.
*
     MOVE  "1"        TO       PRT-FLG.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ　支払明細ファイル読み込み　　　
****************************************************************
 NFSIHAF-RD-SEC            SECTION.
*
     MOVE    "NFSIHAF-RD-SEC"    TO   S-NAME.
*
     READ     NFSIHAF
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    NFSIHAF-RD-EXIT
     END-READ.
*
 NFSIHAF-RD-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO    S-NAME.
*
     MOVE    SPACE                TO    MD01
                                        MD02.
     INITIALIZE                         MD01
                                        MD02.
*  改頁チェック
     IF       L-CNT     >    54
          IF  PRT-FLG   =  "1"
              MOVE    SPACE       TO    PRT-FLG
              PERFORM   MIDASI-SEC
          END-IF
*
          IF   L-CNT    >    7
*
*      店合計行出力
*             PERFORM   TENKEI-SEC
*             MOVE   ZERO         TO    WK-TENKEI
*             INITIALIZE                WK-TENKEI
*
*      ページ合計出力
              PERFORM   PAGEKEI-SEC
              MOVE   ZERO         TO    WK-KEI
              INITIALIZE                WK-KEI
              PERFORM  MIDASI-SEC
          END-IF
     END-IF.
*
*  伝票区分ブレイク時、取引先ブレイク時
     IF       SIH-F03   NOT =   BRK-F03     OR
              SIH-F05   NOT =   BRK-F05
*
*      店合計行出力
              PERFORM   TENKEI-SEC
              MOVE   ZERO         TO    WK-TENKEI
              INITIALIZE                WK-TENKEI
*
*      取引先合計出力
              PERFORM   TRIKEI-SEC
              MOVE   ZERO         TO    WK-TRIKEI
              INITIALIZE                WK-TRIKEI
*
*      ページ合計出力
              PERFORM   PAGEKEI-SEC
              MOVE   ZERO         TO    WK-KEI
              INITIALIZE                WK-KEI
*
              MOVE   99           TO    L-CNT
*  伝票種別名設定
              EVALUATE   SIH-F05
                 WHEN   "01"
                        MOVE     NC"仕入"  TO    HD02-SYUBETUS
                 WHEN   "02"
                        MOVE     NC"仕入"  TO    HD02-SYUBETUS
                 WHEN   "11"
                        MOVE     NC"返品"  TO    HD02-SYUBETUS
                 WHEN   "12"
                        MOVE     NC"値引"  TO    HD02-SYUBETUS
                 WHEN   "61"
                        MOVE     NC"相殺"  TO    HD02-SYUBETUS
              END-EVALUATE
              PERFORM   MIDASI-SEC
*　ブレイクキー設定
              MOVE  SIH-F03    TO       BRK-F03
              MOVE  SIH-F04    TO       BRK-F04
              MOVE  SIH-F08    TO       BRK-F08
              MOVE  SIH-F01    TO       BRK-F01
              MOVE  SIH-F05    TO       BRK-F05
              MOVE  SIH-F23    TO       BRK-TORIMEI
     END-IF.
*
*  店コードレイク時
     IF       SIH-F01   NOT =   BRK-F01
*
*      店合計行出力
              PERFORM   TENKEI-SEC
              MOVE   ZERO         TO    WK-TENKEI
              INITIALIZE                WK-TENKEI
*
*　ブレイクキー設定
              MOVE  SIH-F03    TO       BRK-F03
              MOVE  SIH-F04    TO       BRK-F04
              MOVE  SIH-F08    TO       BRK-F08
              MOVE  SIH-F01    TO       BRK-F01
              MOVE  SIH-F05    TO       BRK-F05
              MOVE  SIH-F23    TO       BRK-TORIMEI
     END-IF.
*
*  明細行編集出力（１行目）
*  店舗ＣＤ
     MOVE     SIH-F01              TO        MD01-TENCD.
*  店舗名称
     MOVE     SIH-F24              TO        MD01-TENMEI.
*  元伝票番号
     MOVE     SIH-F07              TO        MD01-MOTDENNO.
*  原価金額（税抜）
     MOVE     SIH-F13              TO        MD01-GENKA-KINGAKU.
*  特売原価（税抜）
     MOVE     SIH-F14              TO        MD01-TOK-GENKA.
*  値引金額（税抜）
     MOVE     SIH-F15              TO        MD01-NEBI-KINGAKU.
*  消費税　
     MOVE     SIH-F16              TO        MD01-ZEI-GAKU.
*  税率
     MOVE     SIH-F11              TO        WK-ZEI-RITU.
     COMPUTE  WK-RITU    =  WK-ZEI-RITU   /  10.
     MOVE     WK-RITU              TO        MD01-ZEIRITU.
*  値引率
     MOVE     SIH-F12              TO        WK-NEBIKI.
     COMPUTE  WK-NEBIRITU     =   WK-NEBIKI /  100.
     MOVE     WK-NEBIRITU          TO        MD01-NEBIRITU.
*
*  明細行編集出力（２行目）
*  店舗ＣＤ
     MOVE     SIH-F02              TO        MD02-BMNCD.
*  店舗名称
     MOVE     SIH-F25              TO        MD02-BMNMEI.
*  元伝票番号
     MOVE     SIH-F06              TO        MD02-DENNO.
*  受領日
     MOVE     SIH-F08              TO        WK-YMD.
     MOVE     WK-YY                TO        WK-YYR.
     MOVE     WK-MM                TO        WK-MMR.
     MOVE     WK-DD                TO        WK-DDR.
     MOVE     WK-YMDR              TO        MD02-JRYYMD.
*  原価金額（税込）
     MOVE     SIH-F17              TO        MD02-GENKA-KINGAKU.
*  特売原価（税込）
     MOVE     SIH-F18              TO        MD02-TOK-GENKA.
*  値引金額（税込）
     MOVE     SIH-F19              TO        MD02-NEBI-KINGAKU.
*  差引原価（税込）
     MOVE     SIH-F20              TO        MD02-SASI-GENKA.
*  備考
     MOVE     "88"                 TO        JYO-F01.
     MOVE     SIH-F21              TO        WK-KEY1.
     MOVE     SIH-F22              TO        WK-KEY2.
     MOVE     WK-KEY               TO        JYO-F02.
*
     READ     JYOKEN
          INVALID  KEY
              MOVE  SPACE         TO   MD02-BIKO
          NOT INVALID  KEY
              MOVE  JYO-F03       TO   MD02-BIKO
     END-READ.
*--------------
*  店舗合計加算
*--------------
*  原価金額（税抜）
     ADD      SIH-F13             TO   WK-GENKA1-TEN.
*  特売原価（税抜）
     ADD      SIH-F14             TO   WK-GENTAN1-TEN.
*  値引金額（税抜）
     ADD      SIH-F15             TO   WK-NEBIKI1-TEN.
*  原価金額（税込）
     ADD      SIH-F13             TO   WK-GENKA2-TEN.
*  特売原価（税込）
     ADD      SIH-F14             TO   WK-GENTAN2-TEN.
*  値引金額（税込）
     ADD      SIH-F15             TO   WK-NEBIKI2-TEN.
*  差引原価（税抜）
     ADD      SIH-F20             TO   WK-SHKGENKA-TEN.
*
*--------------
*  取引先計加算
*--------------
*  原価金額（税抜）
     ADD      SIH-F13             TO   WK-GENKA1-TRI.
*  特売原価（税抜）
     ADD      SIH-F14             TO   WK-GENTAN1-TRI.
*  値引金額（税抜）
     ADD      SIH-F15             TO   WK-NEBIKI1-TRI.
*  原価金額（税込）
     ADD      SIH-F13             TO   WK-GENKA2-TRI.
*  特売原価（税込）
     ADD      SIH-F14             TO   WK-GENTAN2-TRI.
*  値引金額（税込）
     ADD      SIH-F15             TO   WK-NEBIKI2-TRI.
*  差引原価（税抜）
     ADD      SIH-F20             TO   WK-SHKGENKA-TRI.
*
*--------------
*  総合計加算
*--------------
*  原価金額（税抜）
     ADD      SIH-F13             TO   WK-GENKA1-KEI.
*  特売原価（税抜）
     ADD      SIH-F14             TO   WK-GENTAN1-KEI.
*  値引金額（税抜）
     ADD      SIH-F15             TO   WK-NEBIKI1-KEI.
*  原価金額（税込）
     ADD      SIH-F13             TO   WK-GENKA2-KEI.
*  特売原価（税込）
     ADD      SIH-F14             TO   WK-GENTAN2-KEI.
*  値引金額（税込）
     ADD      SIH-F15             TO   WK-NEBIKI2-KEI.
*  差引原価（税抜）
     ADD      SIH-F20             TO   WK-SHKGENKA-KEI.
*
*--------------
*  明細行出力
*--------------
     WRITE  P-REC  FROM  MD01  AFTER 1.
     WRITE  P-REC  FROM  MD02  AFTER 1.
     ADD      2              TO   L-CNT.
*
*    次レコード読込み
     PERFORM  NFSIHAF-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*             見出し出力処理                1.2                *
****************************************************************
 MIDASI-SEC             SECTION.
*
     MOVE    "MIDASI-SEC"              TO    S-NAME.
*改頁
     IF       L-CNT  >   56    AND
              P-CNT  >    1
              MOVE   SPACE   TO   P-REC
              WRITE  P-REC   AFTER PAGE
     END-IF.
*システム日付セット
     MOVE     SYS-YY            TO   HD01-YY.
     MOVE     SYS-MM            TO   HD01-MM.
     MOVE     SYS-DD            TO   HD01-DD.

*
     MOVE     BRK-F03           TO   HD03-TORICD.
     MOVE     BRK-TORIMEI       TO   HD03-TORIMEI.
     MOVE     BRK-F04           TO   WK-YMD.
     MOVE     WK-YY             TO   HD03-YY.
     MOVE     WK-MM             TO   HD03-MM.
     MOVE     WK-DD             TO   HD03-DD.
*
*頁セット
     MOVE     P-CNT          TO   HD01-PCNT.
*ヘッダー出力
     WRITE    P-REC     FROM      HD01      AFTER     1.
     WRITE    P-REC     FROM      HD02      AFTER     1.
     WRITE    P-REC     FROM      HD03      AFTER     1.
     WRITE    P-REC     FROM      HD04      AFTER     2.
     WRITE    P-REC     FROM      HD05      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
*
     MOVE     7         TO        L-CNT.
     ADD      1         TO        P-CNT.
*
 MIDASI-EXIT.
     EXIT.
****************************************************************
*             店舗合計出力　                　　
****************************************************************
 TENKEI-SEC             SECTION.
*
     MOVE    "TENKEI-SEC"        TO   S-NAME.
*改頁
     IF       L-CNT  >   56
              PERFORM  MIDASI-SEC
     END-IF.
*
*--------------
*  合計転送
*--------------
     MOVE     SPACE              TO   P-REC.
     INITIALIZE                       P-REC.
     MOVE     SPACE              TO   GK01.
     INITIALIZE                       GK01.
     MOVE     SPACE              TO   GK02.
     INITIALIZE                       GK02.
*  タイトル転送
     MOVE     NC"　　店舗合計"   TO   GK02-TAITOL.
*  原価金額（税抜）
     MOVE     WK-GENKA1-TEN      TO   GK01-GENKA-KINGAKU.
*  特売原価（税抜）
     MOVE     WK-GENTAN1-TEN     TO   GK01-TOK-GENKA.
*  値引金額（税抜）
     MOVE     WK-NEBIKI1-TEN     TO   GK01-NEBI-KINGAKU.
*  原価金額（税込）
     MOVE     WK-GENKA2-TEN      TO   GK02-GENKA-KINGAKU.
*  特売原価（税込）
     MOVE     WK-GENTAN2-TEN     TO   GK02-TOK-GENKA.
*  値引金額（税込）
     MOVE     WK-NEBIKI2-TEN     TO   GK02-NEBI-KINGAKU.
*  差引原価（税込）
     MOVE     WK-SHKGENKA-TEN    TO   GK02-SAHI-GENKA.
*
*伝票合計出力
     WRITE    P-REC     FROM      GK01      AFTER     1.
     WRITE    P-REC     FROM      GK02      AFTER     1.
*
     ADD      2         TO        L-CNT.
*
*  線出力
*    IF       L-CNT  >   56
*             PERFORM  MIDASI-SEC
*    END-IF.
*    WRITE    P-REC     FROM  SEN2  AFTER 1.
*    ADD      1         TO    L-CNT.
*
 TENKEI-EXIT.
     EXIT.
****************************************************************
*             取引先合計出力                　　
****************************************************************
 TRIKEI-SEC             SECTION.
*
     MOVE    "TRIKEI-SEC"        TO   S-NAME.
*改頁
     IF       L-CNT  >   56
              PERFORM  MIDASI-SEC
     END-IF.
*
*--------------
*  合計転送
*--------------
     MOVE     SPACE              TO   P-REC.
     INITIALIZE                       P-REC.
     MOVE     SPACE              TO   GK01.
     INITIALIZE                       GK01.
     MOVE     SPACE              TO   GK02.
     INITIALIZE                       GK02.
*  タイトル転送
     MOVE     NC"取引種別合計"   TO   GK02-TAITOL.
*  原価金額（税抜）
     MOVE     WK-GENKA1-KEI      TO   GK01-GENKA-KINGAKU.
*  特売原価（税抜）
     MOVE     WK-GENTAN1-KEI     TO   GK01-TOK-GENKA.
*  値引金額（税抜）
     MOVE     WK-NEBIKI1-KEI     TO   GK01-NEBI-KINGAKU.
*  原価金額（税込）
     MOVE     WK-GENKA2-KEI      TO   GK02-GENKA-KINGAKU.
*  特売原価（税込）
     MOVE     WK-GENTAN2-KEI     TO   GK02-TOK-GENKA.
*  値引金額（税込）
     MOVE     WK-NEBIKI2-KEI     TO   GK02-NEBI-KINGAKU.
*  差引原価（税込）
     MOVE     WK-SHKGENKA-KEI    TO   GK02-SAHI-GENKA.
*
*合計出力
     WRITE    P-REC     FROM      GK01      AFTER     1.
     WRITE    P-REC     FROM      GK02      AFTER     1.
*
     ADD      2         TO        L-CNT.
*
*  線出力
*    IF       L-CNT  >=  58
*             PERFORM  MIDASI-SEC
*    END-IF.
*    WRITE    P-REC     FROM  SEN2  AFTER 1.
*    ADD      1         TO    L-CNT.
*
 TRIKEI-EXIT.
     EXIT.
****************************************************************
*             ページ合計出力　              　　
****************************************************************
 PAGEKEI-SEC            SECTION.
*
     MOVE    "PAGEKEI-SEC"       TO   S-NAME.
*改頁
     IF       L-CNT  >   56
              PERFORM  MIDASI-SEC
     END-IF.
*
*--------------
*  合計転送
*--------------
*  タイトル転送
     MOVE     NC"　ページ合計"   TO   GK02-TAITOL.
*  原価金額（税抜）
     MOVE     WK-GENKA1-KEI      TO   GK01-GENKA-KINGAKU.
*  特売原価（税抜）
     MOVE     WK-GENTAN1-KEI     TO   GK01-TOK-GENKA.
*  値引金額（税抜）
     MOVE     WK-NEBIKI1-KEI     TO   GK01-NEBI-KINGAKU.
*  原価金額（税込）
     MOVE     WK-GENKA2-KEI      TO   GK02-GENKA-KINGAKU.
*  特売原価（税込）
     MOVE     WK-GENTAN2-KEI     TO   GK02-TOK-GENKA.
*  値引金額（税込）
     MOVE     WK-NEBIKI2-KEI     TO   GK02-NEBI-KINGAKU.
*  差引原価（税込）
     MOVE     WK-SHKGENKA-KEI    TO   GK02-SAHI-GENKA.
*
*合計出力
     WRITE    P-REC     FROM      GK01      AFTER     1.
     WRITE    P-REC     FROM      GK02      AFTER     1.
*
     ADD      2         TO        L-CNT.
*
*  線出力
*    IF       L-CNT  >=  58
*             PERFORM  MIDASI-SEC
*    END-IF.
*    WRITE    P-REC     FROM  SEN2  AFTER 1.
*    ADD      1         TO    L-CNT.
*
 PAGEKEI-EXIT.
     EXIT.
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
* 店舗合計出力
*  ページ合計出力
     IF   L-CNT    >    54
          PERFORM   PAGEKEI-SEC
**        MOVE   ZERO         TO    WK-KEI
**        INITIALIZE                WK-KEI
     END-IF.
*
     PERFORM   TENKEI-SEC.
*
* 取引先合計出力
*  ページ合計出力
     IF   L-CNT    >    54
          PERFORM   PAGEKEI-SEC
**        MOVE   ZERO         TO    WK-KEI
**        INITIALIZE                WK-KEI
     END-IF.
*
     PERFORM   TRIKEI-SEC.
*
* 総合計出力
*  ページ合計出力
     IF   L-CNT    >    54
          PERFORM   PAGEKEI-SEC
          MOVE   ZERO         TO    WK-KEI
          INITIALIZE                WK-KEI
     END-IF.
*
***  PERFORM   PAGEKEI-SEC.
*
     DISPLAY  MSG-END   UPON CONS.
*
     CLOSE    NFSIHAF  JYOKEN  PRINTF.
*
 END-EXIT.
     EXIT.
*

```
