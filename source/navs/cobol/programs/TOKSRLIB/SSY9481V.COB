****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ヨドバシ　　　　　　              *
*    モジュール名　　　　：　ヨドバシ受注データ作成　　        *
*    作成日／作成者　　　：　2023/01/30 INOUE                  *
*    処理概要　　　　　　：　受注確認用データとして　　　　　　*
*    　　　　　　　　　　　　ＣＳＶ形式で出力する。　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY9481V.
*                  流用:SSY9430V
 AUTHOR.                NAV-ASSIST.
 DATE-WRITTEN.          2023/01/30.
*
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
*
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<基本情報ファイル >>*************************
     SELECT   YODJOHL4           ASSIGN    TO   DA-01-VI-YODJOHL4
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JOH-F15
                                                JOH-F16
                                                JOH-F17
                                                JOH-F28
                                                JOH-F14
                                                JOH-F18
                                                JOH-F19
                                 STATUS         JOH-STATUS.
*
****<<倉庫マスタ　　　　　 >>*********************************
*    SELECT   ZSOKMS1            ASSIGN    TO   DA-01-VI-ZSOKMS1
*                                ORGANIZATION   INDEXED
*                                ACCESS  MODE   RANDOM
*                                RECORD  KEY    SOK-F01
*                                STATUS         SOK-STATUS.
*
****<<売上伝票Ｆ　　　　　 >>*********************************
     SELECT   SHTDENLJ           ASSIGN    TO   DA-01-VI-SHTDENLJ
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    DEN-F46
                                                DEN-F47
                                                DEN-F01
                                                DEN-F112
                                                DEN-F48
                                                DEN-F02
                                                DEN-F04
                                                DEN-F051
                                                DEN-F07
                                                DEN-F03
                                 STATUS         DEN-STATUS.
*
****<<担当者マスタ　　　　 >>*********************************
*    SELECT   TANMS1             ASSIGN    TO   DA-01-VI-TANMS1
*                                ORGANIZATION   INDEXED
*                                ACCESS  MODE   RANDOM
*                                RECORD  KEY    TAN-F01
*                                               TAN-F02
*                                STATUS         TAN-STATUS.
*
*****<<ＣＳＶデータ　　      >>*******************************
     SELECT   SNDJYUXX          ASSIGN    TO   SNDJYUXX
                                STATUS         CSV-STATUS.
*                                                              *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = 基本情報ファイル                         *
*--------------------------------------------------------------*
 FD  YODJOHL4           LABEL RECORD   IS   STANDARD.
     COPY     YODJOHL4  OF        XFDLIB
              JOINING   JOH       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 倉庫マスタ　　　　　　　　                       *
*--------------------------------------------------------------*
*FD  ZSOKMS1             LABEL RECORD   IS   STANDARD.
*    COPY     ZSOKMS1    OF        XFDLIB
*             JOINING   SOK       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 売上伝票Ｆ　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  SHTDENLJ           LABEL RECORD   IS   STANDARD.
     COPY     SHTDENLJ  OF        XFDLIB
              JOINING   DEN       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 担当者マスタ　　　　　　　                       *
*--------------------------------------------------------------*
*FD  TANMS1              LABEL RECORD   IS   STANDARD.
*    COPY     TANMS1     OF        XFDLIB
*             JOINING   TAN       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ＣＳＶデータ　　　　　                   *
*--------------------------------------------------------------*
 FD  SNDJYUXX           BLOCK CONTAINS 10   RECORDS.
 01  CSV-REC.
     03  FILLER         PIC       X(401).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  END-FLG2                     PIC       X(03)  VALUE  SPACE.
 01  SET-FLG                      PIC       X(03)  VALUE  SPACE.
 01  ZSOKMS1-INV-FLG              PIC       X(03)  VALUE  SPACE.
 01  SHTDENLJ-INV-FLG             PIC       X(03)  VALUE  SPACE.
 01  TANMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  RD1-FLG                      PIC       X(01)  VALUE  SPACE.
 01  RD2-FLG                      PIC       X(01)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  JOH-STATUS                   PIC       X(02).
 01  SOK-STATUS                   PIC       X(02).
 01  DEN-STATUS                   PIC       X(02).
 01  TAN-STATUS                   PIC       X(02).
 01  CSV-STATUS                   PIC       X(02).
*
*01  BRK-KEY.
*    03  BRK-TENCD                PIC       9(05)  VALUE  ZERO.
*    03  BRK-KDATE                PIC       9(08)  VALUE  ZERO.
*    03  BRK-DENKU                PIC       X(02)  VALUE  SPACE.
*    03  BRK-DENNO                PIC       9(10)  VALUE  ZERO.
*
*    03  BRK-TENMEI               PIC      N(15).
*    03  BRK-DNKNM                PIC      N(05).
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
***** 日付ワーク
 01  WK-HIZUKE.
     03  WK-DATE                  PIC  9(08)  VALUE  ZERO.
     03  WK-DATE-R                REDEFINES WK-DATE.
         05  WK-DATE-YYYY         PIC  9(04).
         05  WK-DATE-MM           PIC  9(02).
         05  WK-DATE-DD           PIC  9(02).
***** 時刻ワーク
 01  WK-JIKOKU.
     03  WK-TIME                  PIC  9(06)  VALUE  ZERO.
     03  WK-TIME-R                REDEFINES WK-TIME.
         05  WK-TIME-HH           PIC  9(02).
         05  WK-TIME-MM           PIC  9(02).
         05  WK-TIME-SS           PIC  9(02).
***** 時刻ワーク２
 01  WK-JIKOKU2.
     03  WK-TIME2                  PIC  9(04)  VALUE  ZERO.
     03  WK-TIME2-R                REDEFINES WK-TIME2.
         05  WK-TIME2-HH           PIC  9(02).
         05  WK-TIME2-MM           PIC  9(02).
***** 範囲チェックワーク
 01  CHK-WORK.
     03  CHK-PARA-FROM.
         05  PARA-FROM-DATE       PIC       9(08).
         05  PARA-FROM-TIME       PIC       9(06).
     03  CHK-PARA-FROM-R          REDEFINES CHK-PARA-FROM.
         05  PARA-FROM-DATETIME   PIC       9(14).
*
     03  CHK-PARA-TO.
         05  PARA-TO-DATE         PIC       9(08).
         05  PARA-TO-TIME         PIC       9(06).
     03  CHK-PARA-TO-R            REDEFINES CHK-PARA-TO.
         05  PARA-TO-DATETIME     PIC       9(14).
*
     03  CHK-DATA.
         05  DATA-DATE            PIC       9(08).
         05  DATA-TIME            PIC       9(06).
     03  CHK-DATA-R               REDEFINES CHK-DATA.
         05  DATA-DATETIME        PIC       9(14).
*
 01  WK-TENCD.
     03  WK-TENCD1                PIC       9(01)  VALUE ZERO.
     03  WK-TENCD2                PIC       X(04)  VALUE ZERO.
*--------------
*01  WK-DENKEI.
*    03  WK-KENSHUSU-DEN          PIC       9(07).
*    03  WK-KINGAKU-DEN           PIC       9(12).
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  READ2-CNT                    PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
 01  IX1                          PIC       9(04)  VALUE  ZERO.
*タイトルエリア
*見出しエリア
     COPY     SNDJYU01   OF        XFDLIB
              JOINING    HED       PREFIX.
*01  WK-HEAD1.
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(19)  VALUE
*        NC"＜ヨドバシ基本情報確認ＣＳＶ＞".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"出力日：".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  C02           PIC 9(08).
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(05)  VALUE  NC"出力時刻：".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  C04           PIC 9(06).
*01  WK-HEAD3.
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(05)  VALUE  NC"指定条件　".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(09)  VALUE  NC"実行日付（開始）：".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  H301          PIC X(10).
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(09)  VALUE  NC"実行時刻（開始）：".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  H302          PIC X(08).
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(01)  VALUE  NC"～".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(09)  VALUE  NC"実行日付（終了）：".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  H303          PIC X(10).
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(09)  VALUE  NC"実行時刻（終了）：".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  H304          PIC X(08).
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(03)  VALUE  NC"区分：".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  H305          PIC N(09).
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*01  WK-HEAD6.
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"実行日付".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"実行時刻".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(02)  VALUE  NC"区分".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(05)  VALUE  NC"担当者ＣＤ".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"担当者名".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(05)  VALUE  NC"バッチ日付".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(05)  VALUE  NC"バッチ時刻".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(06)  VALUE  NC"バッチ取引先".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"倉庫ＣＤ".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(03)  VALUE  NC"倉庫名".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(03)  VALUE  NC"納品日".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"店舗ＣＤ".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(03)  VALUE  NC"店舗名".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"伝票番号".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(03)  VALUE  NC"行番号".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(05)  VALUE  NC"変更納品日".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(05)  VALUE  NC"エラー区分".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"エラー１".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"エラー２".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"エラー３".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"エラー４".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"エラー５".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"エラー６".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"エラー７".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"エラー８".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"エラー９".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(05)  VALUE  NC"エラー１０".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*
*明細エリア
     COPY     SNDJYU02   OF        XFDLIB
              JOINING    MEI       PREFIX.
*01  WK-MEISAI1.
*    03  MK101         PIC X(01).
*    03  M102          PIC X(10).
*    03  MK102         PIC X(01).
*    03  M103          PIC X(08).
*    03  MK103         PIC X(01).
*    03  M104          PIC X(08).
*    03  MK104         PIC X(01).
*    03  MS105         PIC X(01).
*    03  M105          PIC N(16).
*    03  ME105         PIC X(01).
*    03  MK105         PIC X(01).
*    03  M106          PIC 9(10).
*    03  MK106         PIC X(01).
*    03  M107          PIC 9(10).
*    03  MK107         PIC X(01).
*    03  M108          PIC X(02).
*    03  MK108         PIC X(01).
*    03  M109          PIC 9(05).
*    03  MK109         PIC X(01).
*    03  M110          PIC 9(08).
*    03  MK110         PIC X(01).
*    03  M111          PIC X(10).
*    03  MK111         PIC X(01).
*    03  M112          PIC X(01).
*    03  MK112         PIC X(01).
*    03  M113          PIC X(02).
*    03  MK113         PIC X(01).
*    03  M114          PIC 9(02).
*    03  MK114         PIC X(01).
*    03  MS115         PIC X(01).
*    03  M115          PIC N(01).
*    03  ME115         PIC X(01).
*    03  MK115         PIC X(01).
*    03  M116          PIC X(14).
*    03  MK116         PIC X(01).
*    03  M117          PIC X(35).
*    03  MK117         PIC X(01).
*    03  M118          PIC X(35).
*    03  MK118         PIC X(01).
*    03  M119          PIC 9(06).
*    03  MK119         PIC X(01).
*    03  M120          PIC 9(08).
*    03  MK120         PIC X(01).
*    03  M121          PIC 9(09).
*    03  MK121         PIC X(01).
*    03  M122          PIC X(03).
*    03  MK122         PIC X(01).
*    03  M123          PIC X(03).
*    03  MK123         PIC X(01).
*    03  M124          PIC X(21).
*    03  MK124         PIC X(01).
*01  WK-MEISAI2.
*    03  GK00          PIC X(17).
*    03  GS01          PIC X(01).
*    03  G01           PIC N(02)  VALUE  NC"合計".
*    03  GE01          PIC X(01).
*    03  GK01          PIC X(01).
*    03  G02           PIC 9(07).
*    03  GK02          PIC X(02).
*    03  G03           PIC 9(12).
*    03  GK03          PIC X(01).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY9481V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY9481V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY9481V".
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
         05  FILLER         PIC   X(09)  VALUE " IN   = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUT  = ".
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
 01  PARA-IN-BDATE       PIC   9(08).
 01  PARA-IN-BTIME       PIC   9(04).
 01  PARA-IN-BTORI       PIC   9(08).
 01  PARA-IN-SOKO        PIC   X(02).
 01  PARA-IN-NDATEF      PIC   9(08).
 01  PARA-IN-NDATET      PIC   9(08).
 01  PARA-IN-KAKTEI      PIC   X(01).
 01  PARA-OUT-CNT        PIC   9(07).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING
                                           PARA-IN-BDATE
                                           PARA-IN-BTIME
                                           PARA-IN-BTORI
                                           PARA-IN-SOKO
                                           PARA-IN-NDATEF
                                           PARA-IN-NDATET
                                           PARA-IN-KAKTEI
                                           PARA-OUT-CNT.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           YODJOHL4.
     MOVE     "YODJOHL4"          TO        ERR-FL-ID.
     MOVE     JOH-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
*FILEERROR-SEC2         SECTION.
*    USE AFTER          EXCEPTION
*                       PROCEDURE ZSOKMS1.
*    MOVE     "ZSOKMS1  "         TO        ERR-FL-ID.
*    MOVE     SOK-STATUS          TO        ERR-STCD.
*    DISPLAY  MSG-ABEND1          UPON      CONS.
*    DISPLAY  MSG-ABEND2          UPON      CONS.
*    DISPLAY  SEC-NAME            UPON      CONS.
*    MOVE     4000                TO   PROGRAM-STATUS.
*    STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE SHTDENLJ.
     MOVE     "SHTDENLJ"          TO        ERR-FL-ID.
     MOVE     DEN-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
*FILEERROR-SEC4         SECTION.
*    USE AFTER          EXCEPTION
*                       PROCEDURE TANMS1.
*    MOVE     "TANMS1   "         TO        ERR-FL-ID.
*    MOVE     TAN-STATUS          TO        ERR-STCD.
*    DISPLAY  MSG-ABEND1          UPON      CONS.
*    DISPLAY  MSG-ABEND2          UPON      CONS.
*    DISPLAY  SEC-NAME            UPON      CONS.
*    MOVE     4000                TO   PROGRAM-STATUS.
*    STOP     RUN.
*
 FILEERROR-SEC5         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE SNDJYUXX.
     MOVE     "SNDJYUXX"          TO        ERR-FL-ID.
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
 SSY9481V-START         SECTION.
*
     MOVE   "SSY9481V-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     IF   END-FLG  NOT =  "END"
          PERFORM    MAIN-SEC  UNTIL  END-FLG   =  "END"
     END-IF.
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSY9481V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     YODJOHL4.
     OPEN     INPUT     SHTDENLJ.
     OPEN     OUTPUT    SNDJYUXX.
     DISPLAY  MSG-START UPON CONS.
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
     MOVE  SPACE                TO   JOH-REC.
     INITIALIZE                      JOH-REC.
     MOVE  PARA-IN-BDATE        TO   JOH-F15.
     MOVE  PARA-IN-BTIME        TO   JOH-F16.
     MOVE  PARA-IN-BTORI        TO   JOH-F17.
     MOVE  PARA-IN-SOKO         TO   JOH-F28.
     MOVE  PARA-IN-NDATEF       TO   JOH-F14.
*
     START  YODJOHL4  KEY  >=   JOH-F15  JOH-F16  JOH-F17
                                JOH-F28  JOH-F14  JOH-F18
                                JOH-F19
         INVALID   KEY
            MOVE     "END"      TO   END-FLG
            DISPLAY NC"＃対象データ無し１＃" UPON CONS
            DISPLAY "JOH-F15=" JOH-F15       UPON CONS
            DISPLAY "JOH-F16=" JOH-F16       UPON CONS
            DISPLAY "JOH-F17=" JOH-F17       UPON CONS
            DISPLAY "JOH-F28=" JOH-F28       UPON CONS
            DISPLAY "JOH-F14=" JOH-F14       UPON CONS
            GO                  TO   INIT-EXIT
     END-START.
*  指示範囲保管
     MOVE   PARA-IN-NDATEF      TO   PARA-FROM-DATE.
     MOVE   PARA-IN-NDATET      TO   PARA-TO-DATE.
*
     PERFORM  YODJOHL4-RD-SEC.
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
     PERFORM  MIDASISET-SEC.
     WRITE    CSV-REC    FROM   HED-REC.
     ADD      1            TO   OUTPUT-CNT.
*　ブレイクキー設定
*    MOVE  JOH-F07    TO       BRK-TENCD.
*    MOVE  JOH-F08    TO       BRK-KDATE.
*    MOVE  JOH-F51    TO       BRK-DENKU.
*    MOVE  JOH-F04    TO       BRK-DENNO.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    基本情報ファイル読み込み　　　
****************************************************************
 YODJOHL4-RD-SEC            SECTION.
*
     MOVE    "YODJOHL4-RD-SEC"    TO   S-NAME.
*
 YODJOHL4-RD-01.
     READ     YODJOHL4
          AT END
              MOVE     "END"      TO   END-FLG
              GO                  TO   YODJOHL4-RD-EXIT
     END-READ.
*
 YODJOHL4-RD-02.
     IF       JOH-F15   NOT   =   PARA-IN-BDATE
              MOVE     "END"      TO   END-FLG
              GO                  TO   YODJOHL4-RD-EXIT
     END-IF.
*
 YODJOHL4-RD-03.
     IF       JOH-F16   NOT   =   PARA-IN-BTIME
              GO                  TO   YODJOHL4-RD-01
     END-IF.
*
 YODJOHL4-RD-04.
     IF       JOH-F17   NOT   =   PARA-IN-BTORI
              MOVE     "END"      TO   END-FLG
              GO                  TO   YODJOHL4-RD-EXIT
     END-IF.
*
 YODJOHL4-RD-05.
     IF       JOH-F28   NOT   =   PARA-IN-SOKO
              GO                  TO   YODJOHL4-RD-01
     END-IF.
*
 YODJOHL4-RD-06.
     IF     ( JOH-F14         <   PARA-IN-NDATEF )
        OR  ( JOH-F14         >   PARA-IN-NDATET )
              GO                  TO   YODJOHL4-RD-01
     END-IF.
*
 YODJOHL4-RD-07.
     IF       PARA-IN-KAKTEI  =   "1"
        IF    JOH-F36     NOT =   " "
              GO                  TO   YODJOHL4-RD-01
        END-IF
     END-IF.
     IF       PARA-IN-KAKTEI  =   "2"
        IF    JOH-F36         =   " "
              GO                  TO   YODJOHL4-RD-01
        END-IF
     END-IF.
*
     ADD   1      TO  READ-CNT.
*
 YODJOHL4-RD-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*初期化
     MOVE     SPACE               TO   CSV-REC.
     MOVE     SPACE               TO   MEI-REC.
     INITIALIZE                        MEI-REC.
*項目転送
     MOVE   X"28"                 TO   MEI-F08S.
     MOVE   X"29"                 TO   MEI-F08E.
*
     MOVE   ","                   TO   MEI-K01
                                       MEI-K02
                                       MEI-K03
                                       MEI-K04
                                       MEI-K05
                                       MEI-K06
                                       MEI-K07
                                       MEI-K08
                                       MEI-K09
                                       MEI-K10
                                       MEI-K11
                                       MEI-K12
                                       MEI-K13
                                       MEI-K14
                                       MEI-K15
                                       MEI-K16
                                       MEI-K17
                                       MEI-K18
                                       MEI-K19
                                       MEI-K20.
*
*  明細行編集
*  購買発注番号
     MOVE     JOH-F02           TO   MEI-F01.
*  発注伝票日付
     MOVE     JOH-F03           TO   WK-DATE.
     MOVE     WK-DATE-YYYY      TO   MEI-F02(1:4).
     MOVE     "/"               TO   MEI-F02(5:1).
     MOVE     WK-DATE-MM        TO   MEI-F02(6:2).
     MOVE     "/"               TO   MEI-F02(8:1).
     MOVE     WK-DATE-DD        TO   MEI-F02(9:2).
*  仕入先コード
     MOVE     JOH-F04           TO   MEI-F03.
*  発注先コード
     MOVE     JOH-F05           TO   MEI-F04.
*  出荷先コード
     MOVE     JOH-F06           TO   MEI-F05.
*  明細行番号
     MOVE     JOH-F07           TO   MEI-F06.
*  品目番号
     MOVE     JOH-F08           TO   MEI-F07.
*  商品名
     MOVE     JOH-F38           TO   MEI-F08.
*  サカタ商品＋品単
     MOVE     JOH-F15           TO   DEN-F46.
     MOVE     JOH-F16           TO   DEN-F47.
     MOVE     JOH-F17           TO   DEN-F01.
     MOVE     JOH-F14           TO   DEN-F112.
     MOVE     JOH-F28           TO   DEN-F48.
     MOVE     JOH-F18           TO   DEN-F02.
     MOVE     0                 TO   DEN-F04.
     MOVE     40                TO   DEN-F051.
     MOVE     JOH-F06(1:4)      TO   WK-TENCD2.
     MOVE     WK-TENCD          TO   DEN-F07.
     MOVE     JOH-F19           TO   DEN-F03.
     PERFORM  SHTDENLJ-READ-SEC.
     IF       SHTDENLJ-INV-FLG = "INV"
              MOVE ALL "*"      TO    MEI-F09
     ELSE
              MOVE DEN-F1411    TO    MEI-F09(1:8)
              MOVE DEN-F1412    TO    MEI-F09(9:8)
     END-IF.
*  明細フリーテキスト１
     MOVE     JOH-F09           TO   MEI-F10.
*  お客様伝票番号
     MOVE     JOH-F11           TO   MEI-F11.
*  配送場所
     MOVE     JOH-F12           TO   MEI-F12.
*  発注数量
     MOVE     JOH-F13           TO   MEI-F13.
*  納入期日
     MOVE     JOH-F14           TO   WK-DATE.
     MOVE     WK-DATE-YYYY      TO   MEI-F14(1:4).
     MOVE     "/"               TO   MEI-F14(5:1).
     MOVE     WK-DATE-MM        TO   MEI-F14(6:2).
     MOVE     "/"               TO   MEI-F14(8:1).
     MOVE     WK-DATE-DD        TO   MEI-F14(9:2).
*  バッチ日付
     MOVE     JOH-F15           TO   MEI-F15.
*  バッチ時刻
     MOVE     JOH-F16           TO   MEI-F16.
*  バッチ取引先
     MOVE     JOH-F17           TO   MEI-F17.
*  基幹伝票番号
     MOVE     JOH-F18           TO   MEI-F18.
*  基幹行番号
     MOVE     JOH-F19           TO   MEI-F19.
*  倉庫　　　
     MOVE     JOH-F28           TO   MEI-F20.
*
*レコードセット
     MOVE     MEI-REC      TO   CSV-REC.
*
     WRITE    CSV-REC.
     ADD      1            TO   OUTPUT-CNT.
*    次レコード読込み
     PERFORM  YODJOHL4-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*             見出し編集処理                1.2                *
****************************************************************
 MIDASISET-SEC             SECTION.
*
     MOVE    "MIDASISET-SEC"            TO   S-NAME.
*
     MOVE     SPACE                     TO   HED-REC.
     INITIALIZE                              HED-REC.
*
     MOVE     X"28"                     TO   HED-S01
                                             HED-S02
                                             HED-S03
                                             HED-S04
                                             HED-S05
                                             HED-S06
                                             HED-S07
                                             HED-S08
                                             HED-S09
                                             HED-S10
                                             HED-S11
                                             HED-S12
                                             HED-S13
                                             HED-S14
                                             HED-S15
                                             HED-S16
                                             HED-S17
                                             HED-S18
                                             HED-S19
                                             HED-S20.
     MOVE     X"29"                     TO   HED-E01
                                             HED-E02
                                             HED-E03
                                             HED-E04
                                             HED-E05
                                             HED-E06
                                             HED-E07
                                             HED-E08
                                             HED-E09
                                             HED-E10
                                             HED-E11
                                             HED-E12
                                             HED-E13
                                             HED-E14
                                             HED-E15
                                             HED-E16
                                             HED-E17
                                             HED-E18
                                             HED-E19
                                             HED-E20.
     MOVE     ","                       TO   HED-K01
                                             HED-K02
                                             HED-K03
                                             HED-K04
                                             HED-K05
                                             HED-K06
                                             HED-K07
                                             HED-K08
                                             HED-K09
                                             HED-K10
                                             HED-K11
                                             HED-K12
                                             HED-K13
                                             HED-K14
                                             HED-K15
                                             HED-K16
                                             HED-K17
                                             HED-K18
                                             HED-K19
                                             HED-K20.
*
     MOVE     NC"購買発注番号"          TO   HED-F01.
     MOVE     NC"発注伝票番号"          TO   HED-F02.
     MOVE     NC"仕入先コード"          TO   HED-F03.
     MOVE     NC"発注先コード"          TO   HED-F04.
     MOVE     NC"出荷先コード"          TO   HED-F05.
     MOVE     NC"明細行番号"            TO   HED-F06.
     MOVE     NC"品目番号"              TO   HED-F07.
     MOVE     NC"商品名"                TO   HED-F08.
     MOVE     NC"サカタ商品ＣＤ"        TO   HED-F09.
     MOVE     NC"明細フリーテキスト１"  TO   HED-F10.
     MOVE     NC"お客様伝票番号"        TO   HED-F11.
     MOVE     NC"配送場所"              TO   HED-F12.
     MOVE     NC"発注数量"              TO   HED-F13.
     MOVE     NC"納入期日"              TO   HED-F14.
     MOVE     NC"バッチ（日）"          TO   HED-F15.
     MOVE     NC"バッチ（時）"          TO   HED-F16.
     MOVE     NC"バッチ（取）"          TO   HED-F17.
     MOVE     NC"基幹伝票番号"          TO   HED-F18.
     MOVE     NC"基幹行番号"            TO   HED-F19.
     MOVE     NC"倉庫"                  TO   HED-F20.
*
 MIDASISET-EXIT.
     EXIT.
****************************************************************
*             伝票合計出力　                　　
****************************************************************
*DENKEI-SEC             SECTION.
*
*    MOVE    "DENKEI-SEC"        TO   S-NAME.
*初期化
*    MOVE     SPACE               TO   CSV-REC.
*    MOVE     SPACE               TO   WK-MEISAI2.
*    INITIALIZE                        WK-MEISAI2.
*項目転送
*    MOVE     X"28"               TO   GS01.
*    MOVE     X"29"               TO   GE01.
*    MOVE     ","                 TO   GK01  GK03.
*    MOVE     ALL ","             TO   GK00  GK02.
*--------------
*  合計転送
*--------------
*  タイトル転送
*    MOVE    WK-DENMEISAI        TO   CSV-DENNO.
*  タイトル転送
*    MOVE     NC"＜合計＞"       TO   GK01-TAITOL.
*  数量
*    MOVE     WK-KENSHUSU-DEN    TO   G02.
*  原価金額
*    MOVE     WK-KINGAKU-DEN     TO   G03.
*
*伝票合計出力
*レコードセット
*    MOVE     WK-MEISAI2         TO   CSV-REC.
*
*    WRITE    CSV-REC.
*    ADD      1                  TO   OUTPUT-CNT.
*
*DENKEI-EXIT.
*    EXIT.
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
* 伝票計出力
*    IF  READ-CNT  > 0
*        PERFORM   DENKEI-SEC
*    END-IF.
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
     MOVE     OUTPUT-CNT     TO   PARA-OUT-CNT.
*
     CLOSE    YODJOHL4  SHTDENLJ  SNDJYUXX.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.
***************************************************************
*             倉庫マスタ読込
***************************************************************
*ZSOKMS1-READ-SEC        SECTION.
*
*    MOVE    "ZSOKMS1-READ-SEC"  TO        S-NAME.
*
*    READ     ZSOKMS1
*             INVALID      MOVE  "INV"    TO   ZSOKMS1-INV-FLG
*             NOT  INVALID MOVE  SPACE    TO   ZSOKMS1-INV-FLG
*    END-READ.
*
*ZSOKMS1-READ-EXIT.
*    EXIT.
***************************************************************
*             売上伝票Ｆ読込
***************************************************************
 SHTDENLJ-READ-SEC        SECTION.
*
     MOVE    "SHTDENLJ-READ-SEC"   TO        S-NAME.
*
     READ     SHTDENLJ
              INVALID      MOVE  "INV"    TO   SHTDENLJ-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   SHTDENLJ-INV-FLG
     END-READ.
*
 SHTDENLJ-READ-EXIT.
     EXIT.
***************************************************************
*             担当者マスタ読込
***************************************************************
*TANMS1-READ-SEC        SECTION.
*
*    MOVE    "TANMS1-READ-SEC"   TO        S-NAME.
*
*    READ     TANMS1
*             INVALID      MOVE  "INV"    TO   TANMS1-INV-FLG
*             NOT  INVALID MOVE  SPACE    TO   TANMS1-INV-FLG
*    END-READ.
*
*TANMS1-READ-EXIT.
*    EXIT.
