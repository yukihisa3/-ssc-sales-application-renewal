# SBZ0050L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SBZ0050L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　部門間在庫移動機能　　　　　      *
*    業務名　　　　　　　：　部門間在庫移動　　　　            *
*    モジュール名　　　　：　部門間在庫移動リスト　　　　　　　*
*    作成日／更新日　　　：　2018/01/23                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　パラメタを受取り、指定条件の　　　*
*                            リストを発行する。　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SBZ0050L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2018/01/23.
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
****<<振替移動実績Ｆ（作成日）>>**************************
     SELECT   FURIDOL5           ASSIGN    TO   DA-01-VI-FURIDOL5
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    FUR5-F92
                                                FUR5-F05
                                                FUR5-F06
                                                FUR5-F03
                                                FUR5-F04
                                                FUR5-F01
                                                FUR5-F02
                                                FUR5-F07
                                 STATUS         FUR5-STATUS.
*
****<<振替移動実績Ｆ（計上日）>>**************************
     SELECT   FURIDOL2           ASSIGN    TO   DA-01-VI-FURIDOL2
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    FUR2-F87
                                                FUR2-F05
                                                FUR2-F06
                                                FUR2-F03
                                                FUR2-F04
                                                FUR2-F01
                                                FUR2-F02
                                                FUR2-F07
                                 STATUS         FUR2-STATUS.
*
****<<振替移動実績Ｆ（修正日）>>**************************
     SELECT   FURIDOL3           ASSIGN    TO   DA-01-VI-FURIDOL3
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    FUR3-F89
                                                FUR3-F05
                                                FUR3-F06
                                                FUR3-F03
                                                FUR3-F04
                                                FUR3-F01
                                                FUR3-F02
                                                FUR3-F07
                                 STATUS         FUR3-STATUS.
*
****<<名称マスタ　　　　　　 >>*********************************
     SELECT   MEIMS1             ASSIGN    TO   DA-01-VI-MEIMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    MEI-F011
                                                MEI-F0121
                                                MEI-F0122
                                                MEI-F0123
                                 STATUS         MEI-STATUS.
*
****<<倉庫マスタ　　　　　　 >>*******************************
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
*    FILE = 振替移動実績Ｆ（作成日）　                         *
*--------------------------------------------------------------*
 FD  FURIDOL5           LABEL RECORD   IS   STANDARD.
     COPY     FURIDOL5  OF        XFDLIB
              JOINING   FUR5      PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 振替移動実績Ｆ（計上日）　                         *
*--------------------------------------------------------------*
 FD  FURIDOL2           LABEL RECORD   IS   STANDARD.
     COPY     FURIDOL2  OF        XFDLIB
              JOINING   FUR2      PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 振替移動実績Ｆ（修正日）　                         *
*--------------------------------------------------------------*
 FD  FURIDOL3           LABEL RECORD   IS   STANDARD.
     COPY     FURIDOL3  OF        XFDLIB
              JOINING   FUR3      PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 名称マスタ　　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     MEIMS1    OF        XFDLIB
              JOINING   MEI       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 倉庫マスタ　　　　　　　　　
*--------------------------------------------------------------*
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1    OF        XFDLIB
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
 01  ZSOKMS1-INV-FLG              PIC       X(03)  VALUE  SPACE.
 01  MEIMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  JYOKEN1-INV-FLG              PIC       X(03)  VALUE  SPACE.
 01  TANMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  FUR5-STATUS                   PIC       X(02).
 01  FUR2-STATUS                   PIC       X(02).
 01  FUR3-STATUS                   PIC       X(02).
 01  SOK-STATUS                   PIC       X(02).
 01  MEI-STATUS                   PIC       X(02).
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
     03  BRK-MOTOBUMCD        PIC       X(04)  VALUE  SPACE.
     03  BRK-MOTOSOKCD        PIC       X(02)  VALUE  SPACE.
     03  BRK-DATE             PIC       9(08)  VALUE  ZERO.
     03  BRK-SAKIBUMCD        PIC       X(04)  VALUE  SPACE.
     03  BRK-SAKISOKCD        PIC       X(02)  VALUE  SPACE.
     03  BRK-DENNO            PIC       9(09)  VALUE  ZERO.
*    03  BRK-NOHINBI          PIC       9(08)  VALUE  ZERO.
*
*--------------
 01  WK-DENKEI.
     03  WK-SURYO-DEN             PIC      S9(09)V999.
     03  WK-KINGAKU-DEN           PIC      S9(11).
     03  WK-BAIGAKU-DEN           PIC      S9(11).
*
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBZ0050L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBZ0050L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SBZ0050L".
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
     03  FILLER                PIC  X(08)  VALUE  "SBZ0050L".
     03  FILLER                PIC  X(13)  VALUE  SPACE.
     03  HD01-DKBN             PIC  N(07)
                               CHARACTER  TYPE  IS PITCH-20.
     03  FILLER                PIC  X(08)  VALUE  SPACE.
     03  FILLER                PIC  N(12)
                       VALUE NC"＜部門間在庫移動リスト＞"
                               CHARACTER  TYPE  IS PITCH-22.
     03  FILLER                PIC  X(22)  VALUE  SPACE.
*    03  HD01-JIKKBN           PIC  N(08)  VALUE  NC"年"
*                              CHARACTER  TYPE  IS PITCH-15-YKBAI.
*    03  FILLER                PIC  X(04)  VALUE  SPACE.
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
***** 見出し行025
 01  HD025.
     03  FILLER                PIC  X(09)   VALUE  SPACE.
     03  HD025-NN              PIC  N(02)
                               CHARACTER  TYPE  IS PITCH-20.
     03  FILLER                PIC  N(02) VALUE   NC"日："
                               CHARACTER  TYPE  IS PITCH-20.
     03  HD025-YMD             PIC  X(10).
***** 見出し行３
 01  HD03.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(08)
                               VALUE  NC"振替元部門ＣＤ："
                               CHARACTER   TYPE IS   PITCH-20.
     03  HD03-MOTOBUCD         PIC  X(04)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD03-MOTOBUNM         PIC  N(08)
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(08)
                               VALUE  NC"振替元倉庫ＣＤ："
                               CHARACTER   TYPE IS   PITCH-20.
     03  HD03-MOTOSOKCD        PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD03-MOTOSOKNM        PIC  N(08)
                              CHARACTER  TYPE IS PITCH-20.
***** 見出し行４
 01  HD04.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE  NC"振替先部門"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(12)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE  NC"振替先倉庫"
                              CHARACTER  TYPE IS PITCH-20.
***** 見出し行５
 01  HD05.
     03  FILLER               PIC  X(85)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"作成日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(05)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"計上日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(05)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"修正日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"修正担当"
                              CHARACTER  TYPE IS PITCH-20.
***** 見出し行６
 01  HD06.
     03  FILLER               PIC  X(07)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"伝票"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  "NO".
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"行"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  "NO".
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"振替日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(07)
                              VALUE  NC"サカタ商品ＣＤ"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(07)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"商品名"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(30)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"移動数"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(09)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"単　価"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(05)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"備　考"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  X(03)  VALUE  "ERR".
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
 01  MD01                         CHARACTER   TYPE IS   PITCH-20.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SAKIBUCD            PIC       X(04).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SAKIBUNM            PIC       N(08).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SAKISOKCD           PIC       X(02).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SAKISOKNM           PIC       N(08).
 01  MD015                        CHARACTER   TYPE IS   PITCH-20.
     03  FILLER                   PIC       X(83)  VALUE  SPACE.
     03  MD015-SAKUSEI-DATE       PIC       X(10).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD015-KEIJOU-DATE        PIC       X(10).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD015-SYUSEI-DATE        PIC       X(10).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD015-SYUSEI-TANTOU      PIC       X(07).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD015-SYUSEI-TANTOUNM    PIC       N(05).
 01  MD02.
     03  FILLER                   PIC       X(07)  VALUE  SPACE.
     03  MD02-DENNO               PIC       X(09).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-GYONO               PIC       Z9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-HURIKAE-DATE        PIC       X(10).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-SHOCD               PIC       X(08).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-HINTAN1             PIC       X(05).
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  MD02-HINTAN2             PIC       X(02).
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  MD02-HINTAN3             PIC       X(01).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-SHONM1              PIC       X(15).
     03  MD02-SHONM2              PIC       X(15).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-SURYO               PIC       ---,---,--9.999.
*    03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-GENKA               PIC       ---,---,--9.999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-BIKO                PIC       X(10).
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  MD02-ERR                 PIC       X(01).
***** 合計行
 01  GK01.
     03  FILLER                   PIC     X(76)  VALUE  SPACE.
     03  GK01-GOKEI               PIC     N(04)
                                  VALUE   NC"伝票合計"
                                  CHARACTER   TYPE IS   PITCH-20.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  GK01-SURYO               PIC       ---,---,--9.999.
*
 COPY    FURIDOF   OF      XFDLIB.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-DKBN              PIC   X(01).
 01  PARA-DFROM             PIC   9(08).
 01  PARA-DTO               PIC   9(08).
 01  PARA-OUTKBN            PIC   X(01).
 01  PARA-PRTKBN            PIC   X(01).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION USING PARA-DKBN
                                       PARA-DFROM
                                       PARA-DTO
                                       PARA-OUTKBN
                                       PARA-PRTKBN.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           FURIDOL5.
     MOVE     "FURIDOL5"          TO        ERR-FL-ID.
     MOVE     FUR5-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           FURIDOL2.
     MOVE     "FURIDOL2"          TO        ERR-FL-ID.
     MOVE     FUR2-STATUS         TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           FURIDOL3.
     MOVE     "FURIDOL3"          TO        ERR-FL-ID.
     MOVE     FUR3-STATUS         TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC4         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE MEIMS1.
     MOVE     "MEIMS1  "          TO        ERR-FL-ID.
     MOVE     MEI-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC5         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE ZSOKMS1.
     MOVE     "ZSOKMS1 "          TO        ERR-FL-ID.
     MOVE     SOK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC6         SECTION.
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
 FILEERROR-SEC7         SECTION.
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
 SBZ0050L-START         SECTION.
*
     MOVE   "SBZ0050L-START"      TO   S-NAME.
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
 SBZ0050L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     EVALUATE PARA-DKBN
         WHEN "1"
              OPEN      INPUT     FURIDOL5
         WHEN "2"
              OPEN      INPUT     FURIDOL2
         WHEN "3"
              OPEN      INPUT     FURIDOL3
     END-EVALUATE.
     OPEN     INPUT     MEIMS1.
     OPEN     INPUT     ZSOKMS1.
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
     MOVE  SPACE                TO   FUR5-REC FUR2-REC FUR3-REC.
     INITIALIZE                      FUR5-REC FUR2-REC FUR3-REC.

     EVALUATE PARA-DKBN
         WHEN   "1"
             MOVE       PARA-DFROM          TO   FUR5-F92
             MOVE       SPACE               TO   FUR5-F05
                                                 FUR5-F06
                                                 FUR5-F03
                                                 FUR5-F04
             MOVE       ZERO                TO   FUR5-F01
                                                 FUR5-F02
                                                 FUR5-F07
*
             START      FURIDOL5  KEY  >=        FUR5-F92
                                                 FUR5-F05
                                                 FUR5-F06
                                                 FUR5-F03
                                                 FUR5-F04
                                                 FUR5-F01
                                                 FUR5-F02
                                                 FUR5-F07
                INVALID KEY
                        MOVE     "END"      TO   END-FLG
                        DISPLAY NC"＃＃対象データなし！＃＃"
                                                 UPON CONS
                        GO                  TO   INIT-EXIT
             END-START
*
         WHEN   "2"
             MOVE       PARA-DFROM          TO   FUR2-F87
             MOVE       SPACE               TO   FUR2-F05
                                                 FUR2-F06
                                                 FUR2-F03
                                                 FUR2-F04
             MOVE       ZERO                TO   FUR2-F01
                                                 FUR2-F02
                                                 FUR2-F07
*
             START      FURIDOL2  KEY  >=        FUR2-F87
                                                 FUR2-F05
                                                 FUR2-F06
                                                 FUR2-F03
                                                 FUR2-F04
                                                 FUR2-F01
                                                 FUR2-F02
                                                 FUR2-F07
                INVALID KEY
                        MOVE     "END"      TO   END-FLG
                        DISPLAY NC"＃＃対象データなし！＃＃"
                                                 UPON CONS
                        GO                  TO   INIT-EXIT
             END-START
*
         WHEN   "3"
             MOVE       PARA-DFROM          TO   FUR3-F89
             MOVE       SPACE               TO   FUR3-F05
                                                 FUR3-F06
                                                 FUR3-F03
                                                 FUR3-F04
             MOVE       ZERO                TO   FUR3-F01
                                                 FUR3-F02
                                                 FUR3-F07
*
             START      FURIDOL3  KEY  >=        FUR3-F89
                                                 FUR3-F05
                                                 FUR3-F06
                                                 FUR3-F03
                                                 FUR3-F04
                                                 FUR3-F01
                                                 FUR3-F02
                                                 FUR3-F07
                INVALID KEY
                        MOVE     "END"      TO   END-FLG
                        DISPLAY NC"＃＃対象データなし！＃＃"
                                                 UPON CONS
                        GO                  TO   INIT-EXIT
             END-START
     END-EVALUATE.
*
 INIT-010.
*
     EVALUATE PARA-DKBN
         WHEN   "1"
             PERFORM  FURIDOL5-RD-SEC
             IF       END-FLG  = "END"
                      DISPLAY NC"＃＃対象データなし！＃＃"
                                                  UPON CONS
                      GO                  TO   INIT-EXIT
             END-IF
         WHEN   "2"
             PERFORM  FURIDOL2-RD-SEC
             IF       END-FLG  = "END"
                      DISPLAY NC"＃＃対象データなし！＃＃"
                                                  UPON CONS
                      GO                  TO   INIT-EXIT
             END-IF
         WHEN   "3"
             PERFORM  FURIDOL3-RD-SEC
             IF       END-FLG  = "END"
                      DISPLAY NC"＃＃対象データなし！＃＃"
                                                  UPON CONS
                      GO                  TO   INIT-EXIT
             END-IF
     END-EVALUATE.
*
     PERFORM   MIDASISET-SEC.
*
*ブレイクキー設定
     MOVE  F05         TO       BRK-MOTOBUMCD.
     MOVE  F06         TO       BRK-MOTOSOKCD.
     EVALUATE PARA-DKBN
         WHEN "1"
              MOVE F92 TO       BRK-DATE
         WHEN "2"
              MOVE F87 TO       BRK-DATE
         WHEN "3"
              MOVE F89 TO       BRK-DATE
     END-EVALUATE.
     MOVE  F03         TO       BRK-SAKIBUMCD.
     MOVE  F04         TO       BRK-SAKISOKCD.
     MOVE  F01         TO       BRK-DENNO.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    振替移動実績Ｆ（作成日）読み込み　　　
****************************************************************
 FURIDOL5-RD-SEC            SECTION.
*
     MOVE    "FURIDOL5-RD-SEC"    TO   S-NAME.
*
     READ     FURIDOL5
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    FURIDOL5-RD-EXIT
          NOT AT END
              MOVE      FUR5-REC     TO   REC
              ADD       1            TO   CNT-READ
     END-READ.
*
     IF       FUR5-F92 > PARA-DTO
              MOVE     "END"      TO   END-FLG
              GO     TO    FURIDOL5-RD-EXIT
     END-IF.
*
 FURIDOL5-RD-EXIT.
     EXIT.
****************************************************************
*    振替移動実績Ｆ（計上日）読み込み　　　
****************************************************************
 FURIDOL2-RD-SEC            SECTION.
*
     MOVE    "FURIDOL2-RD-SEC"    TO   S-NAME.
*
     READ     FURIDOL2
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    FURIDOL2-RD-EXIT
          NOT AT END
              MOVE      FUR2-REC     TO   REC
              ADD       1            TO   CNT-READ
     END-READ.
*
     IF       FUR2-F87 > PARA-DTO
              MOVE     "END"      TO   END-FLG
              GO     TO    FURIDOL2-RD-EXIT
     END-IF.
*
 FURIDOL2-RD-EXIT.
     EXIT.
****************************************************************
*    振替移動実績Ｆ（修正日）読み込み　　　
****************************************************************
 FURIDOL3-RD-SEC            SECTION.
*
     MOVE    "FURIDOL3-RD-SEC"    TO   S-NAME.
*
     READ     FURIDOL3
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    FURIDOL3-RD-EXIT
          NOT AT END
              MOVE      FUR3-REC     TO   REC
              ADD       1            TO   CNT-READ
     END-READ.
*
     IF       FUR3-F89 > PARA-DTO
              MOVE     "END"      TO   END-FLG
              GO     TO    FURIDOL3-RD-EXIT
     END-IF.
*
 FURIDOL3-RD-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO    S-NAME.
*
*  振替元部門・振替元倉庫・日付ブレイク時
     IF     ( F05        NOT =   BRK-MOTOBUMCD ) OR
            ( F06        NOT =   BRK-MOTOSOKCD ) OR
            ( PARA-DKBN = "1" AND F92 NOT = BRK-DATE ) OR
            ( PARA-DKBN = "2" AND F87 NOT = BRK-DATE ) OR
            ( PARA-DKBN = "3" AND F89 NOT = BRK-DATE )
*T↓
*             DISPLAY "F05=" F05 UPON CONS
*             DISPLAY "F06=" F06 UPON CONS
*T↑
*        合計
              PERFORM   DENKEI-SEC
              MOVE   ZERO         TO    WK-DENKEI
              INITIALIZE                WK-DENKEI
              MOVE  SPACE         TO    RD1-FLG
              MOVE  SPACE         TO    RD2-FLG
              MOVE  "1"           TO    SET-FLG
              MOVE  F05           TO    BRK-MOTOBUMCD
              MOVE  F06           TO    BRK-MOTOSOKCD
              IF    PARA-DKBN = "1"
                    MOVE  F92     TO    BRK-DATE
              END-IF
              IF    PARA-DKBN = "2"
                    MOVE  F87     TO    BRK-DATE
              END-IF
              IF    PARA-DKBN = "3"
                    MOVE  F89     TO    BRK-DATE
              END-IF
              MOVE  F01           TO    BRK-DENNO
              MOVE  F03           TO    BRK-SAKIBUMCD
              MOVE  F04           TO    BRK-SAKISOKCD
*        改頁
              MOVE  99            TO    L-CNT
     END-IF.
*
*  振替先部門・振替先倉庫ブレイク時
     IF     ( F03        NOT =   BRK-SAKIBUMCD ) OR
            ( F04        NOT =   BRK-SAKISOKCD )
*        合計
              PERFORM   DENKEI-SEC
              MOVE   ZERO         TO    WK-DENKEI
              INITIALIZE                WK-DENKEI
              MOVE  SPACE         TO    RD1-FLG
              MOVE  SPACE         TO    RD2-FLG
              MOVE  "1"           TO    SET-FLG
              MOVE  F03           TO    BRK-SAKIBUMCD
              MOVE  F04           TO    BRK-SAKISOKCD
              MOVE  F01           TO    BRK-DENNO
     END-IF.
*
*  伝票_ブレイク時
     IF        F01        NOT =   BRK-DENNO
*        合計
              PERFORM   DENKEI-SEC
              MOVE   ZERO         TO    WK-DENKEI
              INITIALIZE                WK-DENKEI
              MOVE  SPACE         TO    RD2-FLG
*             MOVE  "1"           TO    SET-FLG
              MOVE  F01           TO    BRK-DENNO
     END-IF.
*  改頁時、先頭行に伝票番号出力のための制御
     IF       L-CNT      >       54
              MOVE  SPACE         TO    RD2-FLG
     END-IF.
*
*  明細行編集１
*  振替先部門・振替先倉庫
     IF  RD1-FLG = SPACE
         MOVE     F03            TO    MD01-SAKIBUCD
                                       JYO-F02
         MOVE     22             TO    JYO-F01
         PERFORM  JYOKEN1-READ-SEC
         IF  JYOKEN1-INV-FLG = "INV"
             MOVE ALL NC"＊"     TO    MD01-SAKIBUNM
         ELSE
             MOVE JYO-F03       TO    MD01-SAKIBUNM
         END-IF
*
         MOVE   F04             TO    MD01-SAKISOKCD
                                      SOK-F01
         PERFORM  ZSOKMS1-READ-SEC
         IF  ZSOKMS1-INV-FLG = "INV"
             MOVE ALL NC"＊"    TO    MD01-SAKISOKNM
         ELSE
             MOVE SOK-F02       TO    MD01-SAKISOKNM
         END-IF
         MOVE  "1"              TO    RD1-FLG
     END-IF.
*  明細行編集015
*  作成日
     IF       F92     = ZERO
              MOVE    SPACE     TO    MD015-SAKUSEI-DATE
     ELSE
              MOVE    F92(1:4)  TO    MD015-SAKUSEI-DATE(1:4)
              MOVE    "/"       TO    MD015-SAKUSEI-DATE(5:1)
              MOVE    F92(5:2)  TO    MD015-SAKUSEI-DATE(6:2)
              MOVE    "/"       TO    MD015-SAKUSEI-DATE(8:1)
              MOVE    F92(7:2)  TO    MD015-SAKUSEI-DATE(9:2)
     END-IF.
*  計上日
*T↓
*    IF       F01     = 18
*             DISPLAY "DEN=" F01 UPON CONS
*             DISPLAY "KEIJOBI=" F87 UPON CONS
*    END-IF.
*T↑
     IF       F87     = ZERO
              MOVE    SPACE     TO    MD015-KEIJOU-DATE
     ELSE
              MOVE    F87(1:4)  TO    MD015-KEIJOU-DATE(1:4)
              MOVE    "/"       TO    MD015-KEIJOU-DATE(5:1)
              MOVE    F87(5:2)  TO    MD015-KEIJOU-DATE(6:2)
              MOVE    "/"       TO    MD015-KEIJOU-DATE(8:1)
              MOVE    F87(7:2)  TO    MD015-KEIJOU-DATE(9:2)
     END-IF.
*  修正日
*T↓
*    IF       F01     = 18
*             DISPLAY "DEN=" F01 UPON CONS
*             DISPLAY "SYUSEIBI=" F89 UPON CONS
*    END-IF.
*T↑
     IF       F89     = ZERO
              MOVE    SPACE     TO    MD015-SYUSEI-DATE
     ELSE
              MOVE    F89(1:4)  TO    MD015-SYUSEI-DATE(1:4)
              MOVE    "/"       TO    MD015-SYUSEI-DATE(5:1)
              MOVE    F89(5:2)  TO    MD015-SYUSEI-DATE(6:2)
              MOVE    "/"       TO    MD015-SYUSEI-DATE(8:1)
              MOVE    F89(7:2)  TO    MD015-SYUSEI-DATE(9:2)
     END-IF.
*  修正担当
     IF     ( F90     = SPACE ) OR
            ( F91     = SPACE )
              MOVE    SPACE     TO    MD015-SYUSEI-TANTOU
              MOVE    SPACE     TO    MD015-SYUSEI-TANTOUNM
     ELSE
              MOVE    F90       TO    MD015-SYUSEI-TANTOU(1:4)
              MOVE    "-"       TO    MD015-SYUSEI-TANTOU(5:1)
              MOVE    F91       TO    MD015-SYUSEI-TANTOU(6:2)
              MOVE    F90       TO    TAN-F01
              MOVE    F91       TO    TAN-F02
              PERFORM TANMS1-READ-SEC
              IF      TANMS1-INV-FLG = "INV"
                      MOVE  ALL NC"＊" TO  MD015-SYUSEI-TANTOUNM
              ELSE
                      MOVE  TAN-F03    TO  MD015-SYUSEI-TANTOUNM
              END-IF
     END-IF.
*
*  明細行編集２
*  伝票番号
     IF       RD2-FLG =  SPACE
              MOVE    F01        TO    MD02-DENNO
     ELSE
              MOVE    SPACE      TO    MD02-DENNO
     END-IF.
*  振替日
     MOVE     F07                TO    MD02-HURIKAE-DATE.
*
*  行番
     MOVE     F02                   TO        MD02-GYONO.
*  サカタ商品
     MOVE     F08                   TO        MD02-SHOCD
                                              MEI-F011.
     MOVE     F09                   TO        MD02-HINTAN1
                                              MEI-F0121.
     MOVE     F10                   TO        MD02-HINTAN2
                                              MEI-F0122.
     MOVE     F11                   TO        MD02-HINTAN3
                                              MEI-F0123.
*  商品名
     PERFORM  MEIMS1-READ-SEC.
     IF   MEIMS1-INV-FLG = "INV"
          MOVE  ALL "*"        TO        MD02-SHONM1 MD02-SHONM2
     ELSE
          MOVE  MEI-F031       TO        MD02-SHONM1
          MOVE  MEI-F032       TO        MD02-SHONM2
     END-IF.
*  移動数
     MOVE     F12                   TO        MD02-SURYO.
*  原価単価
     MOVE     F13                   TO        MD02-GENKA.
*  備考
     MOVE     F14                   TO        MD02-BIKO.
*  ERR
     IF       F83     =  "1"
              MOVE       "E"        TO        MD02-ERR
     ELSE
              MOVE       " "        TO        MD02-ERR
     END-IF.
*
*--------------
*  伝票計加算
*--------------
*  伝票備考
*    MOVE     FUR5-F24              TO        GK01-MEISAIBIKO1.
*    MOVE     FUR5-F25              TO        GK01-MEISAIBIKO2.
*  数量　
     COMPUTE  WK-SURYO-DEN     = WK-SURYO-DEN     +  F12.
*  原価金額
*    COMPUTE  WK-KINGAKU-DEN   = WK-KINGAKU-DEN   +  FUR5-F18.
*
*--------------
*  明細行出力
*--------------
*  改頁チェック
     IF       L-CNT     >    54
              PERFORM  MIDASI-SEC
              PERFORM  HED-SEC
              MOVE     SPACE      TO    RD2-FLG
     END-IF.
     IF       SET-FLG   = "1"
              PERFORM  HED-SEC
     END-IF.
     IF       L-CNT     >    54
              PERFORM  MIDASI-SEC
              PERFORM  HED-SEC
     END-IF.

     IF       RD2-FLG  =  SPACE
              WRITE  P-REC  FROM  MD015 AFTER 1
              ADD    1            TO    L-CNT
              MOVE  "1"           TO    RD2-FLG
     END-IF.
     WRITE  P-REC  FROM  MD02  AFTER 1.
     ADD    1            TO    L-CNT.
*    次レコード読込み
*    PERFORM  FURIDOL5-RD-SEC.
     EVALUATE PARA-DKBN
         WHEN   "1"
             PERFORM  FURIDOL5-RD-SEC
         WHEN   "2"
             PERFORM  FURIDOL2-RD-SEC
         WHEN   "3"
             PERFORM  FURIDOL3-RD-SEC
     END-EVALUATE.
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
*
*タイトル日付
     IF    PARA-DKBN    =  "1"
           MOVE NC"＜　作成日　＞"    TO  HD01-DKBN
     END-IF.
     IF    PARA-DKBN    =  "2"
           MOVE NC"＜　計上日　＞"    TO  HD01-DKBN
     END-IF.
     IF    PARA-DKBN    =  "3"
           MOVE NC"＜　修正日　＞"    TO  HD01-DKBN
     END-IF.
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
*ヘッダ日付
     IF    PARA-DKBN    =  "1"
           MOVE NC"作成日"            TO  HD025-NN
           MOVE F92(1:4)              TO  HD025-YMD(1:4)
           MOVE "/"                   TO  HD025-YMD(5:1)
           MOVE F92(5:2)              TO  HD025-YMD(6:2)
           MOVE "/"                   TO  HD025-YMD(8:1)
           MOVE F92(7:2)              TO  HD025-YMD(9:2)
     END-IF.
     IF    PARA-DKBN    =  "2"
           MOVE NC"計上日"            TO  HD025-NN
           MOVE F87(1:4)              TO  HD025-YMD(1:4)
           MOVE "/"                   TO  HD025-YMD(5:1)
           MOVE F87(5:2)              TO  HD025-YMD(6:2)
           MOVE "/"                   TO  HD025-YMD(8:1)
           MOVE F87(7:2)              TO  HD025-YMD(9:2)
     END-IF.
     IF    PARA-DKBN    =  "3"
           MOVE NC"修正日"            TO  HD025-NN
           MOVE F89(1:4)              TO  HD025-YMD(1:4)
           MOVE "/"                   TO  HD025-YMD(5:1)
           MOVE F89(5:2)              TO  HD025-YMD(6:2)
           MOVE "/"                   TO  HD025-YMD(8:1)
           MOVE F89(7:2)              TO  HD025-YMD(9:2)
     END-IF.
*
*振替元部門
     MOVE     F05                     TO  HD03-MOTOBUCD.
     MOVE     22                      TO  JYO-F01.
     MOVE     F05                     TO  JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG = "INV"
              MOVE ALL NC"＊"         TO  HD03-MOTOBUNM
     ELSE
              MOVE JYO-F03            TO  HD03-MOTOBUNM
     END-IF.
*振替元倉庫
     MOVE     F06                     TO  HD03-MOTOSOKCD
                                          SOK-F01.
*T↓
*    DISPLAY  "HD03-MOTOSOKCD=" HD03-MOTOSOKCD UPON CONS.
*T↑
     PERFORM  ZSOKMS1-READ-SEC.
     IF       ZSOKMS1-INV-FLG = "INV"
              MOVE ALL NC"＊"         TO  HD03-MOTOSOKNM
     ELSE
              MOVE SOK-F02            TO  HD03-MOTOSOKNM
     END-IF.
*担当者
*    MOVE     PARA-TANST          TO   HD03-TANF.
*    MOVE     PARA-TANED          TO   HD03-TANT.
*
*伝票纏めの場合、ヘッダ表示
*    IF   PARA-JIKKBN  =  "1"
*         MOVE  "("          TO   HD05-01
*         MOVE  NC"伝票番号" TO   HD05-02
*         MOVE  ")"          TO   HD05-03
*    ELSE
*         MOVE  SPACE        TO   HD05-01  HD05-02  HD05-03
*    END-IF.
*
*頁セット
     ADD      1              TO   P-CNT.
     MOVE     P-CNT          TO   HD01-PCNT.
*ヘッダー出力
     WRITE    P-REC     FROM      HD01      AFTER     1.
     WRITE    P-REC     FROM      HD02      AFTER     1.
     WRITE    P-REC     FROM      HD025     AFTER     2.
     WRITE    P-REC     FROM      HD03      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
     WRITE    P-REC     FROM      HD04      AFTER     1.
     WRITE    P-REC     FROM      HD05      AFTER     1.
     WRITE    P-REC     FROM      HD06      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
*
     MOVE     11        TO        L-CNT.
*
 MIDASI-EXIT.
     EXIT.
****************************************************************
*             ヘッダ出力処理　              1.3                *
****************************************************************
 HED-SEC             SECTION.
*
     MOVE    "HED-SEC"              TO    S-NAME.
     WRITE    P-REC      FROM       MD01  AFTER     1.
     ADD      1                     TO    L-CNT.
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
*    MOVE     WK-KINGAKU-DEN     TO   GK01-KINGAKU.
*
*伝票合計出力
     WRITE    P-REC     FROM      GK01      AFTER     1.
*
     ADD      1         TO        L-CNT.
*
*  線出力
     IF   END-FLG    NOT =  "END"
         IF       L-CNT  >  55
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
*             名称マスタ読込
***************************************************************
 MEIMS1-READ-SEC        SECTION.
*
     MOVE    "MEIMS1-READ-SEC"  TO        S-NAME.
*
     READ     MEIMS1
              INVALID      MOVE  "INV"    TO   MEIMS1-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   MEIMS1-INV-FLG
     END-READ.
*
 MEIMS1-READ-EXIT.
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
     EVALUATE PARA-DKBN
         WHEN "1"
              CLOSE  FURIDOL5
         WHEN "2"
              CLOSE  FURIDOL2
         WHEN "3"
              CLOSE  FURIDOL3
     END-EVALUATE.
     CLOSE    MEIMS1 ZSOKMS1 JYOKEN1 TANMS1 PRINTF.
*
 END-EXIT.
     EXIT.
*

```
