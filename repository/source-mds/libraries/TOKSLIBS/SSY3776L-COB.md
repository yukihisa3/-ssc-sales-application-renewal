# SSY3776L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3776L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩシステム　　　　　　*
*    業務名　　　　　　　：　出荷処理                         *
*    モジュール名　　　　：　作場マスタリスト出力　　　　      *
*    作成日／更新日　　　：　10/10/14                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　作場マスタリストを出力する。　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY3776L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/10/14.
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
*
****<<作場マスタ　　　　　　 >>*********************************
     SELECT   SAKUBAF            ASSIGN    TO   DA-01-VI-SAKUBAL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    SAK-F01
                                 STATUS         SAK-STATUS.
*
****<<条件マスタ　　　　　　 >>*********************************
     SELECT   JYOKEN             ASSIGN    TO   DA-01-VI-JYOKEN1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    JYO-F01  JYO-F02
                                 STATUS         JYO-STATUS.
*
****<<担当者マスタ >>*********************************
     SELECT   HTANMS              ASSIGN    TO   DA-01-VI-TANMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TAN-F01
                                                TAN-F02
                                 STATUS         TAN-STATUS.
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
*    FILE = ナフコ　　作場マスタ　　　　　                     *
*--------------------------------------------------------------*
 FD  SAKUBAF             LABEL RECORD   IS   STANDARD.
     COPY     SAKUBAF    OF        XFDLIB
              JOINING   SAK       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ　　条件マスタ　　　　　                     *
*--------------------------------------------------------------*
 FD  JYOKEN              LABEL RECORD   IS   STANDARD.
     COPY     JYOKEN1    OF        XFDLIB
              JOINING   JYO       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 担当者マスタ　　　　　　　　　                     *
*--------------------------------------------------------------*
 FD  HTANMS              LABEL RECORD   IS   STANDARD.
     COPY     HTANMS     OF        XFDLIB
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
 01  CHK-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  SAK-STATUS                   PIC       X(02).
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
*
 01  WK-AREA.
     03  IX1                      PIC       9(02)  VALUE  ZERO.
     03  SET-FLG                  PIC       X(01)  VALUE  SPACE.
     03  PRT-FLG                  PIC       X(01)  VALUE  SPACE.
*
***** カウンタ
 01  P-CNT                        PIC       9(04)  VALUE  ZERO.
 01  L-CNT                        PIC       9(03)  VALUE  ZERO.
 01  L-CNT2                       PIC       9(03)  VALUE  ZERO.
 01  CNT-READ                     PIC       9(06)  VALUE  ZERO.
 01  MEI-CNT                      PIC       9(05)  VALUE  ZERO.
*
 01  WK-YMD.
     03  WK-YY                    PIC       9(04).
     03  WK-MM                    PIC       9(02).
     03  WK-DD                    PIC       9(02).
 01  WK-YMDR.
     03  WK-YYR                   PIC       9(04).
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  WK-MMR                   PIC       9(02).
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  WK-DDR                   PIC       9(02).
*
 01  WK-TIME.
     03  WK-THH                   PIC       9(02).
     03  WK-TMM                   PIC       9(02).
     03  WK-TSS                   PIC       9(02).
 01  WK-TIMER.
     03  WK-THHR                  PIC       9(02).
     03  FILLER                   PIC       X(01)  VALUE  ":".
     03  WK-TMMR                  PIC       9(02).
     03  FILLER                   PIC       X(01)  VALUE  ":".
     03  WK-TSSR                  PIC       9(02).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3776L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3776L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY3776L".
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
 01  HD01             CHARACTER  TYPE  IS PITCH-30.
     03  FILLER                PIC  X(41)  VALUE  SPACE.
     03  FILLER                PIC  N(01)  VALUE  NC"＜".
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(01)  VALUE  NC"作".
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(01)  VALUE  NC"場".
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(01)  VALUE  NC"マ".
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(01)  VALUE  NC"ス".
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(01)  VALUE  NC"タ".
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(01)  VALUE  NC"リ".
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(01)  VALUE  NC"ス".
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(01)  VALUE  NC"ト".
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(01)  VALUE  NC"＞".
     03  FILLER                PIC  X(180) VALUE  SPACE.
*
***** 見出し行２
 01  HD02             CHARACTER  TYPE  IS PITCH-20.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  X(08)  VALUE  "SSY3776L".
     03  FILLER                PIC  X(105) VALUE  SPACE.
     03  HD02-YY               PIC  9999.
     03  FILLER                PIC  N(01)  VALUE  NC"年".
     03  HD02-MM               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"月".
     03  HD02-DD               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"日".
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  HD02-PCNT             PIC  ZZ9.
     03  FILLER                PIC  N(02)  VALUE  NC"頁".
     03  FILLER                PIC  X(100) VALUE  SPACE.
*
***** 見出し行３
 01  HD03                      CHARACTER   TYPE IS   PITCH-20.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(06)  VALUE  NC"作場情報".
     03  FILLER                PIC  X(33)  VALUE  SPACE.
     03  FILLER                PIC  N(02)  VALUE  NC"直送".
     03  FILLER                PIC  X(12)  VALUE  SPACE.
     03  FILLER                PIC  N(05)  VALUE
                               NC"直送先ＣＤ".
*
***** 線
 01  SEN1.
     03  FILLER                   PIC       X(40)  VALUE
         "----------------------------------------".
     03  FILLER                   PIC       X(40)  VALUE
         "----------------------------------------".
     03  FILLER                   PIC       X(40)  VALUE
         "----------------------------------------".
     03  FILLER                   PIC       X(17)  VALUE
         "-----------------".
*
***** 明細行
 01  MD01            CHARACTER      TYPE IS     PITCH-20.
     03  FILLER                   PIC       X(01).
     03  MD01-SAKBCD              PIC       X(02).
     03  FILLER                   PIC       X(01).
     03  MD01-SAKBMEI             PIC       N(20).
     03  FILLER                   PIC       X(02).
     03  MD01-KBN                 PIC       X(01).
     03  FILLER                   PIC       X(01).
     03  MD01-KBNMEI              PIC       N(06).
     03  FILLER                   PIC       X(02).
     03  MD01-TYOKUS              PIC       9(02).
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
                        PROCEDURE SAKUBAF.
     MOVE     "SAKUBAF "          TO        ERR-FL-ID.
     MOVE     SAK-STATUS          TO        ERR-STCD.
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
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE HTANMS.
     MOVE     "HTANMS  "          TO        ERR-FL-ID.
     MOVE     TAN-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY3776L-START         SECTION.
*
     MOVE   "SSY3776L-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
*
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SSY3776L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SAKUBAF.
     OPEN     INPUT     JYOKEN.
     OPEN     INPUT     HTANMS.
     OPEN     OUTPUT    PRINTF.
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
*　作場マスタ初期読込
     PERFORM  SAKUBAF-RD-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ　作場マスタ読込　　
****************************************************************
 SAKUBAF-RD-SEC             SECTION.
*
     MOVE    "SAKUBAF-RD-SEC"     TO   S-NAME.
*
     READ     SAKUBAF
          AT END
              MOVE     "END"      TO   END-FLG
     END-READ.
*
 SAKUBAF-RD-EXIT.
     EXIT.
*
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
     MOVE     SYS-YY            TO   HD02-YY.
     MOVE     SYS-MM            TO   HD02-MM.
     MOVE     SYS-DD            TO   HD02-DD.
*
*頁セット
     ADD      1              TO   P-CNT.
     MOVE     P-CNT          TO   HD02-PCNT.
*ヘッダー出力
     WRITE    P-REC     FROM      HD01      AFTER     1.
     WRITE    P-REC     FROM      HD02      AFTER     1.
     WRITE    P-REC     FROM      HD03      AFTER     2.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
*
     MOVE     7         TO        L-CNT.
*
 MIDASI-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO    S-NAME.
*  改頁チェック
     IF       L-CNT     >=  56
              PERFORM  MIDASI-SEC
     END-IF.
*
* レコード初期化
     MOVE     SPACE              TO  MD01.
     INITIALIZE                      MD01.
*
* 明細項目転送
*  作場ＣＤ
     MOVE     SAK-F01            TO  MD01-SAKBCD.
*  作場名称
     MOVE     SAK-F02            TO  MD01-SAKBMEI.
*  直送
     MOVE     SAK-F03            TO  MD01-KBN.
*  直送名称
     MOVE     "89"               TO  JYO-F01.
     MOVE     SAK-F03            TO  JYO-F02.
     READ     JYOKEN
              INVALID  KEY
                 MOVE   ALL  NC"＊"   TO  MD01-KBNMEI
             NOT INVALID  KEY
                 MOVE    JYO-F03      TO  MD01-KBNMEI
     END-READ.
*  直送先ＣＤ
     MOVE     SAK-F04            TO  MD01-TYOKUS.
*
*--------------
*  明細行出力
*--------------
     WRITE  P-REC  FROM  MD01  AFTER 1.
     ADD      1              TO   L-CNT.
     ADD      1              TO   L-CNT2.
*
     IF     L-CNT2   =  5
            WRITE  P-REC  FROM  SEN1  AFTER 1
            ADD    1         TO   L-CNT
            MOVE   ZERO      TO   L-CNT2
     END-IF.
*
*    次レコード読込み
     PERFORM  SAKUBAF-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     DISPLAY  MSG-END   UPON CONS.
*
     CLOSE    SAKUBAF  JYOKEN    HTANMS  PRINTF.
*
 END-EXIT.
     EXIT.
*

```
