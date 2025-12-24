****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ヨドバシ　　　　　　              *
*    モジュール名　　　　：　ヨドバシ納期変更出荷確定　        *
*    　　　　　　　　　　　　確認ＣＳＶ出力　　　　　　        *
*    作成日／作成者　　　：　2022/02/21 INOUE                  *
*    流用　　　　　　　　：　SJR0270V                          *
*    処理概要　　　　　　：　納期変更出荷確定データを　　　　　*
*    　　　　　　　　　　　　ＣＳＶ形式で出力する。　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY9430V.
 AUTHOR.                NAV-ASSIST.
 DATE-WRITTEN.          2022/02/21.
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
****<<納期変更出荷確定履歴ワーク >>*************************
     SELECT   YODNKKL2           ASSIGN    TO   DA-01-VI-YODNKKL2
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    NKK-F02
                                                NKK-F03
                                                NKK-F01
                                                NKK-F04
                                                NKK-F05
                                                NKK-F06
                                                NKK-F07
                                                NKK-F08
                                                NKK-F09
                                                NKK-F10
                                                NKK-F11
                                 STATUS         NKK-STATUS.
*
****<<倉庫マスタ　　　　　 >>*********************************
     SELECT   ZSOKMS1            ASSIGN    TO   DA-01-VI-ZSOKMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    SOK-F01
                                 STATUS         SOK-STATUS.
*
****<<店舗マスタ　　　　　 >>*********************************
     SELECT   TENMS1             ASSIGN    TO   DA-01-VI-TENMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TEN-F52
                                                TEN-F011
                                 STATUS         TEN-STATUS.
*
****<<担当者マスタ　　　　 >>*********************************
     SELECT   TANMS1             ASSIGN    TO   DA-01-VI-TANMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TAN-F01
                                                TAN-F02
                                 STATUS         TAN-STATUS.
*
*****<<ＣＳＶデータ　　      >>*******************************
     SELECT   YODNKKCV          ASSIGN    TO   YODNKKCV
                                STATUS         CSV-STATUS.
*                                                              *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = 納期変更出荷確定履歴ワーク                         *
*--------------------------------------------------------------*
 FD  YODNKKL2           LABEL RECORD   IS   STANDARD.
     COPY     YODNKKL2  OF        XFDLIB
              JOINING   NKK       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 倉庫マスタ　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  ZSOKMS1             LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1    OF        XFDLIB
              JOINING   SOK       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 店舗マスタ　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  TENMS1              LABEL RECORD   IS   STANDARD.
     COPY     TENMS1     OF        XFDLIB
              JOINING   TEN       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 担当者マスタ　　　　　　　                       *
*--------------------------------------------------------------*
 FD  TANMS1              LABEL RECORD   IS   STANDARD.
     COPY     TANMS1     OF        XFDLIB
              JOINING   TAN       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ＣＳＶデータ　　　　　                   *
*--------------------------------------------------------------*
 FD  YODNKKCV           BLOCK CONTAINS 13   RECORDS.
 01  CSV-REC.
     03  FILLER         PIC       X(300).
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
 01  TENMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  TANMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  RD1-FLG                      PIC       X(01)  VALUE  SPACE.
 01  RD2-FLG                      PIC       X(01)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  NKK-STATUS                   PIC       X(02).
 01  SOK-STATUS                   PIC       X(02).
 01  TEN-STATUS                   PIC       X(02).
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
 01  WK-HEAD1.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(19)  VALUE
         NC"＜ヨドバシ納期変更出荷確定確認ＣＳＶ＞".
*    03  C00           PIC N(04).
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"出力日：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  C02           PIC 9(08).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"出力時刻：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  C04           PIC 9(06).
 01  WK-HEAD3.
*    03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"指定条件　".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(09)  VALUE  NC"実行日付（開始）：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  H301          PIC X(10).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(09)  VALUE  NC"実行時刻（開始）：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  H302          PIC X(08).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(01)  VALUE  NC"～".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(09)  VALUE  NC"実行日付（終了）：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  H303          PIC X(10).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(09)  VALUE  NC"実行時刻（終了）：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  H304          PIC X(08).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"区分：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  H305          PIC N(09).
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
*01  WK-HEAD4.
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"受信日：".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  H203          PIC 9(08)  VALUE  ZERO.
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(01)  VALUE  NC"～".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  H205          PIC 9(08)  VALUE  ZERO.
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(04)  VALUE  NC"計上日：".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  H207          PIC 9(08)  VALUE  ZERO.
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC N(01)  VALUE  NC"～".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  H209          PIC 9(08)  VALUE  ZERO.
*    03  FILLER        PIC X(01)  VALUE  ",".
*01  WK-HEAD5.
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  FILLER        PIC X(01)  VALUE  X"28".
*    03  FILLER        PIC  N(05)  VALUE  NC"伝票区分：".
*    03  FILLER        PIC X(01)  VALUE  X"29".
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  HD05-DENK1    PIC X(02).
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  HD05-DENK2    PIC X(02).
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  HD05-DENK3    PIC X(02).
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  HD05-DENK4    PIC X(02).
*    03  FILLER        PIC X(01)  VALUE  ",".
*    03  HD05-DENK5    PIC X(02).
 01  WK-HEAD6.
*    03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"実行日付".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"実行時刻".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"担当者ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"担当者名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"バッチ日付".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"バッチ時刻".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"バッチ取引先".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"倉庫ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"倉庫名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"納品日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"店舗ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"店舗名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"伝票番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"行番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"変更納品日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"エラー区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"エラー１".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"エラー２".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"エラー３".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"エラー４".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"エラー５".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"エラー６".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"エラー７".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"エラー８".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"エラー９".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"エラー１０".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
*
*明細エリア
     COPY     YODNKKCV   OF        XFDLIB
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
         05  ST-PG          PIC   X(08)  VALUE "SSY9430V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY9430V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY9430V".
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
 01  PARA-IN-FROMDATE       PIC   9(08).
 01  PARA-IN-TODATE         PIC   9(08).
 01  PARA-IN-FROMTIME       PIC   9(06).
 01  PARA-IN-TOTIME         PIC   9(06).
 01  PARA-IN-OUTKBN         PIC   X(01).
 01  PARA-OUT-CNT           PIC   9(07).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING
                                           PARA-IN-FROMDATE
                                           PARA-IN-TODATE
                                           PARA-IN-FROMTIME
                                           PARA-IN-TOTIME
                                           PARA-IN-OUTKBN
                                           PARA-OUT-CNT.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           YODNKKL2.
     MOVE     "YODNKKL2"          TO        ERR-FL-ID.
     MOVE     NKK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE ZSOKMS1.
     MOVE     "ZSOKMS1  "         TO        ERR-FL-ID.
     MOVE     SOK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TENMS1.
     MOVE     "TENMS1   "         TO        ERR-FL-ID.
     MOVE     TEN-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC4         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TANMS1.
     MOVE     "TANMS1   "         TO        ERR-FL-ID.
     MOVE     TAN-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC5         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE YODNKKCV.
     MOVE     "YODNKKCV"          TO        ERR-FL-ID.
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
 SSY9430V-START         SECTION.
*
     MOVE   "SSY9430V-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     IF   END-FLG  NOT =  "END"
          PERFORM    MAIN-SEC  UNTIL  END-FLG   =  "END"
     END-IF.
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSY9430V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     YODNKKL2.
     OPEN     INPUT     ZSOKMS1 TENMS1 TANMS1.
     OPEN     OUTPUT    YODNKKCV.
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
     MOVE  SPACE                TO   NKK-REC.
     INITIALIZE                      NKK-REC.
     MOVE  PARA-IN-FROMDATE     TO   NKK-F02.
     MOVE  PARA-IN-FROMTIME     TO   NKK-F03.
     MOVE  PARA-IN-OUTKBN       TO   NKK-F01.
*
     START  YODNKKL2  KEY  >=   NKK-F02  NKK-F03  NKK-F01
                                NKK-F04  NKK-F05  NKK-F06
                                NKK-F07  NKK-F08  NKK-F09
                                NKK-F10  NKK-F11
         INVALID   KEY
            MOVE     "END"      TO   END-FLG
            DISPLAY NC"＃対象データ無し１＃" UPON CONS
            DISPLAY "NKK-F02=" NKK-F02       UPON CONS
            DISPLAY "NKK-F03=" NKK-F03       UPON CONS
            DISPLAY "NKK-F01=" NKK-F01       UPON CONS
            GO                  TO   INIT-EXIT
     END-START.
*  指示範囲保管
     MOVE   PARA-IN-FROMDATE      TO   PARA-FROM-DATE.
     MOVE   PARA-IN-FROMTIME      TO   PARA-FROM-TIME.
     MOVE   PARA-IN-TODATE        TO   PARA-TO-DATE.
     MOVE   PARA-IN-TOTIME        TO   PARA-TO-TIME.
*
     PERFORM  YODNKKL2-RD-SEC.
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
     PERFORM  MIDASISET-SEC.
     MOVE     SPACE               TO   CSV-REC.
     MOVE     WK-HEAD1            TO   CSV-REC.
     WRITE    CSV-REC.
     MOVE     SPACE               TO   CSV-REC.
     MOVE     WK-HEAD3            TO   CSV-REC.
     WRITE    CSV-REC.
     MOVE     SPACE               TO   CSV-REC.
*    MOVE     WK-HEAD4            TO   CSV-REC.
*    WRITE    CSV-REC.
*    MOVE     WK-HEAD5            TO   CSV-REC.
*    WRITE    CSV-REC.
     MOVE     WK-HEAD6            TO   CSV-REC.
     WRITE    CSV-REC.
     ADD      3            TO   OUTPUT-CNT.
*　ブレイクキー設定
*    MOVE  NKK-F07    TO       BRK-TENCD.
*    MOVE  NKK-F08    TO       BRK-KDATE.
*    MOVE  NKK-F51    TO       BRK-DENKU.
*    MOVE  NKK-F04    TO       BRK-DENNO.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    納期変更出荷確定履歴ワーク読み込み　　　
****************************************************************
 YODNKKL2-RD-SEC            SECTION.
*
     MOVE    "YODNKKL2-RD-SEC"    TO   S-NAME.
*
     READ     YODNKKL2
          AT END
              MOVE     "END"      TO   END-FLG
              GO                  TO   YODNKKL2-RD-EXIT
     END-READ.
*
     MOVE     NKK-F02             TO   DATA-DATE.
     MOVE     NKK-F03             TO   DATA-TIME.
     IF       DATA-DATETIME   >   PARA-TO-DATETIME
              MOVE     "END"      TO   END-FLG
              GO                  TO   YODNKKL2-RD-EXIT
     END-IF.
     IF       PARA-IN-OUTKBN  =   "1"
        IF    NKK-F01     NOT =   "1"
              GO                  TO   YODNKKL2-RD-SEC
        END-IF
     END-IF.
     IF       PARA-IN-OUTKBN  =   "2"
        IF    NKK-F01     NOT =   "2"
              GO                  TO   YODNKKL2-RD-SEC
        END-IF
     END-IF.
*
     ADD   1      TO  READ-CNT.
*
 YODNKKL2-RD-EXIT.
     EXIT.
***************************************************************
*             倉庫マスタ読込
***************************************************************
 ZSOKMS1-READ-SEC        SECTION.
*
     MOVE    "ZSOKMS1-READ-SEC"  TO        S-NAME.
*
     READ     ZSOKMS1
              INVALID      MOVE  "INV"    TO   ZSOKMS1-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   ZSOKMS1-INV-FLG
     END-READ.
*
 ZSOKMS1-READ-EXIT.
     EXIT.
***************************************************************
*             店舗マスタ読込
***************************************************************
 TENMS1-READ-SEC        SECTION.
*
     MOVE    "TENMS1-READ-SEC"   TO        S-NAME.
*
     READ     TENMS1
              INVALID      MOVE  "INV"    TO   TENMS1-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   TENMS1-INV-FLG
     END-READ.
*
 TENMS1-READ-EXIT.
     EXIT.
***************************************************************
*             担当者マスタ読込
***************************************************************
 TANMS1-READ-SEC        SECTION.
*
     MOVE    "TANMS1-READ-SEC"   TO        S-NAME.
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
     MOVE     SPACE               TO   MEI-REC.
     INITIALIZE                        MEI-REC.
*項目転送
     MOVE   X"28"                 TO   MEI-S031
                                       MEI-S051
                                       MEI-S101
                                       MEI-S131
                                       MEI-S171
                                       MEI-S181
                                       MEI-S191
                                       MEI-S201
                                       MEI-S211
                                       MEI-S221
                                       MEI-S231
                                       MEI-S241
                                       MEI-S251
                                       MEI-S261
                                       MEI-S271.
     MOVE   X"29"                 TO   MEI-S032
                                       MEI-S052
                                       MEI-S102
                                       MEI-S132
                                       MEI-S172
                                       MEI-S182
                                       MEI-S192
                                       MEI-S202
                                       MEI-S212
                                       MEI-S222
                                       MEI-S232
                                       MEI-S242
                                       MEI-S252
                                       MEI-S262
                                       MEI-S272.
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
                                       MEI-K20
                                       MEI-K21
                                       MEI-K22
                                       MEI-K23
                                       MEI-K24
                                       MEI-K25
                                       MEI-K26
                                       MEI-K27.
*
*  ブレイク時
*    IF      ( NKK-F07   NOT =   BRK-TENCD )  OR
*            ( NKK-F08   NOT =   BRK-KDATE )  OR
*            ( NKK-F51   NOT =   BRK-DENKU )  OR
*            ( NKK-F04   NOT =   BRK-DENNO )
*        合計
*             PERFORM   DENKEI-SEC
*             MOVE   ZERO         TO    WK-DENKEI
*             INITIALIZE                WK-DENKEI
*             IF  ( NKK-F04   NOT =   BRK-TENCD )
*                 MOVE  SPACE         TO    RD1-FLG
*             END-IF
*             MOVE  NKK-F07       TO    BRK-TENCD
*             MOVE  NKK-F08       TO    BRK-KDATE
*             MOVE  NKK-F51       TO    BRK-DENKU
*             MOVE  NKK-F04       TO    BRK-DENNO
*             MOVE  SPACE         TO    RD2-FLG
*    END-IF.
*  明細行編集
*  実行日付
     MOVE     NKK-F02           TO   WK-DATE.
     MOVE     WK-DATE-YYYY      TO   MEI-F01(1:4).
     MOVE     "/"               TO   MEI-F01(5:1).
     MOVE     WK-DATE-MM        TO   MEI-F01(6:2).
     MOVE     "/"               TO   MEI-F01(8:1).
     MOVE     WK-DATE-DD        TO   MEI-F01(9:2).
*  実行時刻
     MOVE     NKK-F03           TO   WK-TIME.
     MOVE     WK-TIME-HH        TO   MEI-F02(1:2).
     MOVE     ":"               TO   MEI-F02(3:1).
     MOVE     WK-TIME-MM        TO   MEI-F02(4:2).
     MOVE     ":"               TO   MEI-F02(6:1).
     MOVE     WK-TIME-SS        TO   MEI-F02(7:2).
*  区分
     EVALUATE NKK-F01
          WHEN   "1"
              MOVE     NC"納期変更"            TO   MEI-F03
          WHEN   "2"
              MOVE     NC"出荷確定"            TO   MEI-F03
          WHEN    OTHER
              MOVE     NC"？？？？"            TO   MEI-F03
     END-EVALUATE.
*  担当者ＣＤ
     MOVE     NKK-F61           TO    MEI-F04  TAN-F02.
     MOVE     "2920"            TO             TAN-F01.
*  担当者名
     PERFORM  TANMS1-READ-SEC.
     IF       TANMS1-INV-FLG = "INV"
              MOVE ALL NC"＊"   TO    MEI-F05
     ELSE
              MOVE TAN-F03      TO    MEI-F05
     END-IF.
*  バッチ日付
     MOVE     NKK-F04           TO   WK-DATE.
     MOVE     WK-DATE-YYYY      TO   MEI-F06(1:4).
     MOVE     "/"               TO   MEI-F06(5:1).
     MOVE     WK-DATE-MM        TO   MEI-F06(6:2).
     MOVE     "/"               TO   MEI-F06(8:1).
     MOVE     WK-DATE-DD        TO   MEI-F06(9:2).
*  バッチ時刻
     MOVE     NKK-F05           TO   WK-TIME2.
     MOVE     WK-TIME2-HH       TO   MEI-F07(1:2).
     MOVE     ":"               TO   MEI-F07(3:1).
     MOVE     WK-TIME2-MM       TO   MEI-F07(4:2).
*  バッチ取引先
     MOVE     NKK-F06           TO   MEI-F08.
*  倉庫　　　
     MOVE     NKK-F07           TO    MEI-F09  SOK-F01.
*  倉庫名
     PERFORM  ZSOKMS1-READ-SEC.
     IF       ZSOKMS1-INV-FLG = "INV"
              MOVE ALL NC"＊"   TO    MEI-F10
     ELSE
              MOVE SOK-F02      TO    MEI-F10
     END-IF.
*  納品日
     MOVE     NKK-F08           TO   WK-DATE.
     MOVE     WK-DATE-YYYY      TO   MEI-F11(1:4).
     MOVE     "/"               TO   MEI-F11(5:1).
     MOVE     WK-DATE-MM        TO   MEI-F11(6:2).
     MOVE     "/"               TO   MEI-F11(8:1).
     MOVE     WK-DATE-DD        TO   MEI-F11(9:2).
*  店舗ＣＤ
     MOVE     NKK-F09           TO    MEI-F12  TEN-F011.
     MOVE     NKK-F06           TO             TEN-F52.
*  店舗名
     PERFORM  TENMS1-READ-SEC.
     IF       TENMS1-INV-FLG = "INV"
              MOVE ALL NC"＊"   TO    MEI-F13
     ELSE
              MOVE TEN-F02      TO    MEI-F13
     END-IF.
*  伝票番号
     MOVE     NKK-F10           TO    MEI-F14.
*  行番号
     MOVE     NKK-F11           TO    MEI-F15.
*  変更納品日
     MOVE     NKK-F12           TO   WK-DATE.
     MOVE     WK-DATE-YYYY      TO   MEI-F16(1:4).
     MOVE     "/"               TO   MEI-F16(5:1).
     MOVE     WK-DATE-MM        TO   MEI-F16(6:2).
     MOVE     "/"               TO   MEI-F16(8:1).
     MOVE     WK-DATE-DD        TO   MEI-F16(9:2).
*  エラー区分
     IF       NKK-F50         = "1"
              MOVE  NC"○"      TO   MEI-F17
     END-IF.
*  エラー区分１
     IF       NKK-F51         = "1"
              MOVE  NC"○"      TO   MEI-F18
     END-IF.
*  エラー区分２
     IF       NKK-F52         = "1"
              MOVE  NC"○"      TO   MEI-F19
     END-IF.
*  エラー区分３
     IF       NKK-F53         = "1"
              MOVE  NC"○"      TO   MEI-F20
     END-IF.
*  エラー区分４
     IF       NKK-F54         = "1"
              MOVE  NC"○"      TO   MEI-F21
     END-IF.
*  エラー区分５
     IF       NKK-F55         = "1"
              MOVE  NC"○"      TO   MEI-F22
     END-IF.
*  エラー区分６
     IF       NKK-F56         = "1"
              MOVE  NC"○"      TO   MEI-F23
     END-IF.
*  エラー区分７
     IF       NKK-F57         = "1"
              MOVE  NC"○"      TO   MEI-F24
     END-IF.
*  エラー区分８
     IF       NKK-F58         = "1"
              MOVE  NC"○"      TO   MEI-F25
     END-IF.
*  エラー区分９
     IF       NKK-F59         = "1"
              MOVE  NC"○"      TO   MEI-F26
     END-IF.
*  エラー区分１０
     IF       NKK-F60         = "1"
              MOVE  NC"○"      TO   MEI-F27
     END-IF.
*
*レコードセット
     MOVE     MEI-REC      TO   CSV-REC.
*
     WRITE    CSV-REC.
     ADD      1            TO   OUTPUT-CNT.
*    次レコード読込み
     PERFORM  YODNKKL2-RD-SEC.
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
     MOVE     SYS-DATE-R        TO   C02.
     MOVE     SYSTEM-TIME       TO   C04.
*指定値セット　　　　　　　
*　実行日付開始　　　　　　
     MOVE     PARA-IN-FROMDATE  TO   WK-DATE.
     MOVE     WK-DATE-YYYY      TO   H301(1:4).
     MOVE     "/"               TO   H301(5:1).
     MOVE     WK-DATE-MM        TO   H301(6:2).
     MOVE     "/"               TO   H301(8:1).
     MOVE     WK-DATE-DD        TO   H301(9:2).
*　実行時刻開始　　　　　　
     MOVE     PARA-IN-FROMTIME  TO   WK-TIME.
     MOVE     WK-TIME-HH        TO   H302(1:2).
     MOVE     ":"               TO   H302(3:1).
     MOVE     WK-TIME-MM        TO   H302(4:2).
     MOVE     ":"               TO   H302(6:1).
     MOVE     WK-TIME-SS        TO   H302(7:2).
*　実行日付終了　　　　　　
     MOVE     PARA-IN-TODATE    TO   WK-DATE.
     MOVE     WK-DATE-YYYY      TO   H303(1:4).
     MOVE     "/"               TO   H303(5:1).
     MOVE     WK-DATE-MM        TO   H303(6:2).
     MOVE     "/"               TO   H303(8:1).
     MOVE     WK-DATE-DD        TO   H303(9:2).
*　実行時刻終了　　　　　　
     MOVE     PARA-IN-TOTIME    TO   WK-TIME.
     MOVE     WK-TIME-HH        TO   H304(1:2).
     MOVE     ":"               TO   H304(3:1).
     MOVE     WK-TIME-MM        TO   H304(4:2).
     MOVE     ":"               TO   H304(6:1).
     MOVE     WK-TIME-SS        TO   H304(7:2).
*　区分
     EVALUATE PARA-IN-OUTKBN
          WHEN   " "
              MOVE     NC"納期変更／出荷確定"  TO   H305
          WHEN   "1"
              MOVE     NC"納期変更"            TO   H305
          WHEN   "2"
              MOVE     NC"出荷確定"            TO   H305
     END-EVALUATE.
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
     CLOSE    YODNKKL2 ZSOKMS1 TENMS1 TANMS1 YODNKKCV.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.
