# SBZ0050V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SBZ0050V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　部門間在庫移動機能　　　　　      *
*    業務名　　　　　　　：　部門間在庫移動　　　　            *
*    モジュール名　　　　：　部門間在庫移動データＣＳＶ出力　　*
*    作成日／更新日　　　：　2018/01/25                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　パラメタを受取り、　　　　　　　　*
*                          　範囲内のＣＳＶを出力する。　　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    　　　　　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SBZ0050V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2017/01/25.
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
****<<伝票纏め返品実績データ >>*******************************
*    SELECT   COMRMHF           ASSIGN    TO   DA-01-VI-COMRMHL7
*                                ORGANIZATION   INDEXED
*                                ACCESS  MODE   SEQUENTIAL
*                                RECORD  KEY    RMH-F78
*                                               RMH-F79
*                                               RMH-F01
*                                               RMH-F02
*                                               RMH-F03
*                                               RMH-F04
*                                               RMH-F05
*                                STATUS         RMH-STATUS.
*
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
****<<倉庫マスタ　　　　　　 >>*********************************
     SELECT   ZSOKMS1            ASSIGN    TO   DA-01-VI-ZSOKMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    SOK-F01
                                 STATUS         SOK-STATUS.

****<<名称マスタ　　　　　 >>*********************************
     SELECT   MEIMS1             ASSIGN    TO   DA-01-VI-MEIMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    MEI-F011
                                                MEI-F0121
                                                MEI-F0122
                                                MEI-F0123
                                 STATUS         MEI-STATUS.

****<<条件ファル　　　　　　 >>*********************************
     SELECT   JYOKEN1            ASSIGN    TO   DA-01-VI-JYOKEN1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    JYO-F01  JYO-F02
                                 STATUS         JYO-STATUS.

****<<担当者マスタ　　　　　 >>*********************************
     SELECT   TANMS1             ASSIGN    TO   DA-01-VI-TANMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TAN-F01  TAN-F02
                                 STATUS         TAN-STATUS.

*****<<部門間在庫移動データＣＳＶ>>***************************
     SELECT   FURIDCSV           ASSIGN    TO   FURIDCSV
                                 STATUS         CSV-STATUS.
*                                                              *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = 伝票纏め返品実績データ　　　　　　　               *
*--------------------------------------------------------------*
*FD  COMRMHF           LABEL RECORD   IS   STANDARD.
*    COPY     COMRMHF   OF        XFDLIB
*             JOINING   RMH       PREFIX.
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
*    FILE = 倉庫マスタ　　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  ZSOKMS1             LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1    OF        XFDLIB
              JOINING   SOK       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 名称マスタ　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     MEIMS1    OF        XFDLIB
              JOINING   MEI       PREFIX.
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
 FD  TANMS1              LABEL RECORD   IS   STANDARD.
     COPY     HTANMS     OF        XFDLIB
              JOINING   TAN       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 部門間在庫移動データＣＳＶ　　　                   *
*--------------------------------------------------------------*
 FD  FURIDCSV           BLOCK CONTAINS 1    RECORDS.
 01  CSV-REC.
     03  FILLER         PIC       X(500).
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
 01  MEIMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  JYOKEN1-INV-FLG              PIC       X(03)  VALUE  SPACE.
 01  TANMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  RD1-FLG                      PIC       X(01)  VALUE  SPACE.
 01  RD2-FLG                      PIC       X(01)  VALUE  SPACE.
*
**** ステイタス　エリア
*01  RMH-STATUS                   PIC       X(02).
 01  FUR5-STATUS                  PIC       X(02).
 01  FUR2-STATUS                  PIC       X(02).
 01  FUR3-STATUS                  PIC       X(02).
 01  SOK-STATUS                   PIC       X(02).
 01  MEI-STATUS                   PIC       X(02).
 01  JYO-STATUS                   PIC       X(02).
 01  TAN-STATUS                   PIC       X(02).
 01  CSV-STATUS                   PIC       X(02).
*
 01  BRK-KEY.
*    03  BRK-TENCD                PIC      S9(05)  VALUE  ZERO.
     03  BRK-DENNO                PIC       9(09)  VALUE  ZERO.
*    03  BRK-TENMEI               PIC      N(15).
*    03  BRK-TOKMEI               PIC      N(15).
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
*
*--------------
 01  WK-DENKEI.
     03  WK-SURYO-DEN             PIC      S9(09)V999.
*    03  WK-KENSHUSU-DEN          PIC      S9(06)V9.
*    03  WK-KINGAKU-DEN           PIC      S9(07).
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  READ2-CNT                    PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
 01  IX1                          PIC       9(04)  VALUE  ZERO.
*タイトルエリア
*見出しエリア
 01  WK-HEAD1.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(12)  VALUE
                       NC"＜部門間在庫移動データ＞".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"日付：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD01-YYYYMMDD PIC X(10).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"時刻：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD01-HHMMSS   PIC X(08).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"日付区分：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  HD01-DKBN     PIC N(03).
     03  FILLER        PIC X(01)  VALUE  X"29".
 01  WK-HEAD2.
*    03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"日付範囲：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD02-FR-DATE  PIC X(10).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(01)  VALUE  NC"～".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD02-TO-DATE  PIC X(10).
 01  WK-HEAD3.
*    03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"振替元部門ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"振替元部門名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"振替先部門ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"振替先部門名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"倉庫ＣＤ".
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
     03  FILLER        PIC N(03)  VALUE  NC"振替日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"サカタ商品ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"品単１".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"品単２".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"品単３".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"商品名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"移動数".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"単価".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"備考".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"作成日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"計上日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"修正日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"修正担当者ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"修正担当者名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(08)  VALUE  NC"ＡＣＯＳ計上区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"エラー区分".
     03  FILLER        PIC X(01)  VALUE  X"29".

*明細エリア
 01  WK-MEISAI1.
     03  WK-DENMEISAI1.
*A00***************
*        05  CSV-A00                  PIC  X(01).
*A01***************
         05  MD01-MOTOBUCD            PIC  X(04).
         05  CSV-A01                  PIC  X(01).
*A02***************
         05  CSV-A02A                 PIC  X(01).
         05  MD01-MOTOBUNM            PIC  N(10).
         05  CSV-A02B                 PIC  X(01).
         05  CSV-A02                  PIC  X(01).
*A03***************
         05  MD01-SAKIBUCD            PIC  X(04).
         05  CSV-A03                  PIC  X(01).
*A04***************
         05  CSV-A04A                 PIC  X(01).
         05  MD01-SAKIBUNM            PIC  N(10).
         05  CSV-A04B                 PIC  X(01).
         05  CSV-A04                  PIC  X(01).
*A05***************
         05  MD01-MOTOSOKCD           PIC  X(02).
         05  CSV-A05                  PIC  X(01).
*A06***************
         05  MD01-DENNO               PIC  9(09).
         05  CSV-A06                  PIC  X(01).
*A07***************
         05  MD01-GYONO               PIC  9(02).
         05  CSV-A07                  PIC  X(01).
*A08***************
         05  MD01-FURI-DATE           PIC  X(10).
         05  CSV-A08                  PIC  X(01).
*
     03  WK-DENMEISAI2.
*A09***************
         05  MD01-SYOCD               PIC  X(08).
         05  CSV-A09                  PIC  X(01).
*A10***************
         05  MD01-HINCD1              PIC  X(05).
         05  CSV-A10                  PIC  X(01).
*A11***************
         05  MD01-HINCD2              PIC  X(02).
         05  CSV-A11                  PIC  X(01).
*A12***************
         05  MD01-HINCD3              PIC  X(01).
         05  CSV-A12                  PIC  X(01).
*A13***************
         05  MD01-SYONM               PIC  X(30).
         05  CSV-A13                  PIC  X(01).
*A14***************
         05  MD01-SURYO               PIC ---------9.999.
         05  CSV-A14                  PIC  X(01).
*A15***************
         05  MD01-TANKA               PIC ---------9.999.
         05  CSV-A15                  PIC  X(01).
*A16***************
         05  MD01-BIKOU               PIC  X(10).
         05  CSV-A16                  PIC  X(01).
*A17***************
         05  MD01-SAKUSEI-DATE        PIC  X(10).
         05  CSV-A17                  PIC  X(01).
*A18***************
         05  MD01-KEIJO-DATE          PIC  X(10).
         05  CSV-A18                  PIC  X(01).
*A19***************
         05  MD01-SYUSEI-DATE         PIC  X(10).
         05  CSV-A19                  PIC  X(01).
*A20***************
         05  MD01-TANCD               PIC  X(02).
         05  CSV-A20                  PIC  X(01).
*A21***************
         05  CSV-A21A                 PIC  X(01).
         05  MD01-TANNM               PIC  N(10).
         05  CSV-A21B                 PIC  X(01).
         05  CSV-A21                  PIC  X(01).
*A22***************
         05  MD01-ACOS                PIC  X(01).
         05  CSV-A22                  PIC  X(01).
*A23***************
         05  MD01-ERR                 PIC  X(01).
         05  CSV-A23                  PIC  X(01).
*
*合計行
 01  WK-MEISAI2.
     03  CSV-KYOTU                    PIC  X(84).
     03  CSV-B01                      PIC  X(01).
     03  CSV-B02                      PIC  X(01).
     03  CSV-B03                      PIC  X(01).
     03  CSV-B04                      PIC  X(01).
*    03  CSV-B05                      PIC  X(01).
     03  CSV-B06A                     PIC  X(01).
     03  GK01-TAITOL                  PIC  N(06).
     03  CSV-B06B                     PIC  X(01).
     03  CSV-B07                      PIC  X(01).
     03  GK01-GKSURYO                 PIC  ---------9.999.
     03  CSV-B08                      PIC  X(01).
*    03  CSV-B09                      PIC  X(01).
*    03  GK01-KINGAKU                 PIC  --------9.
*
 01  WK-DENMEISAI1-SAV.
     03  WK-DENMEISAI1-BAK            PIC  X(084)  VALUE  SPACE.
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBZ0050V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBZ0050V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SBZ0050V".
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
*FILEERROR-SEC1         SECTION.
*    USE AFTER          EXCEPTION
*                       PROCEDURE           COMRMHF.
*    MOVE     "COMRMHF"           TO        ERR-FL-ID.
*    MOVE     RMH-STATUS          TO        ERR-STCD.
*    DISPLAY  MSG-ABEND1          UPON      CONS.
*    DISPLAY  MSG-ABEND2          UPON      CONS.
*    DISPLAY  SEC-NAME            UPON      CONS.
*    MOVE     4000                TO   PROGRAM-STATUS.
*    STOP     RUN.
*
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
                        PROCEDURE ZSOKMS1.
     MOVE     "ZSOKMS1  "         TO        ERR-FL-ID.
     MOVE     SOK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC5         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE MEIMS1.
     MOVE     "MEIMS1  "          TO        ERR-FL-ID.
     MOVE     MEI-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
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
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC7         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TANMS1.
     MOVE     "TANMS1  "          TO        ERR-FL-ID.
     MOVE     TAN-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC8         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE FURIDCSV.
     MOVE     "FURIDCSV"          TO        ERR-FL-ID.
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
 SBZ0050V-START         SECTION.
*
     MOVE   "SBZ0050V-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     IF   END-FLG  NOT =  "END"
          PERFORM    MAIN-SEC  UNTIL  END-FLG   =  "END"
     END-IF.
     PERFORM            END-SEC.
     STOP               RUN.
*
 SBZ0050V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
*    OPEN     INPUT     COMRMHF.
     EVALUATE PARA-DKBN
         WHEN "1"
              OPEN      INPUT     FURIDOL5
         WHEN "2"
              OPEN      INPUT     FURIDOL2
         WHEN "3"
              OPEN      INPUT     FURIDOL3
     END-EVALUATE.
     OPEN     INPUT     ZSOKMS1.
     OPEN     INPUT     MEIMS1.
     OPEN     INPUT     JYOKEN1.
     OPEN     INPUT     TANMS1.
     OPEN     OUTPUT    FURIDCSV.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO           TO   WK-DENKEI.
     INITIALIZE                   WK-DENKEI.
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
*    MOVE  SPACE                TO   RMH-REC.
*    INITIALIZE                      RMH-REC.
*
*    MOVE  PARA-BT-DATE         TO   RMH-F78.
*    MOVE  PARA-BT-TIME         TO   RMH-F79.
*    MOVE  PARA-BT-TOKCD        TO   RMH-F01.
*    START  COMRMHF  KEY  >= RMH-F78 RMH-F79 RMH-F01
*                            RMH-F02 RMH-F03 RMH-F04
*                            RMH-F05
*        INVALID   KEY
*           MOVE     "END"      TO   END-FLG
*           DISPLAY NC"＃対象データ無し１＃" UPON CONS
*           GO                  TO   INIT-EXIT
*    END-START
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
*
*    PERFORM  COMRMHF-RD-SEC.
*    IF    END-FLG   =   "END"
*          DISPLAY NC"＃対象データ無し２＃" UPON CONS
*          GO                  TO   INIT-EXIT
*    END-IF.
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
     MOVE     SPACE               TO   CSV-REC.
     MOVE     WK-HEAD1            TO   CSV-REC.
     WRITE    CSV-REC.
     MOVE     SPACE               TO   CSV-REC.
     MOVE     WK-HEAD2            TO   CSV-REC.
     WRITE    CSV-REC.
     MOVE     SPACE               TO   CSV-REC.
     MOVE     WK-HEAD3            TO   CSV-REC.
     WRITE    CSV-REC.
     ADD      3                   TO   OUTPUT-CNT.
*
*　ブレイクキー設定
*    MOVE  RMH-F02                TO   BRK-TENCD.
     MOVE  F01                    TO   BRK-DENNO.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ケーヨー　受領累積データ読み込み　　　
****************************************************************
*COMRMHF-RD-SEC            SECTION.
*
*    MOVE    "COMRMHF-RD-SEC"    TO   S-NAME.
*
*    READ     COMRMHF
*         AT END
*             MOVE     "END"      TO   END-FLG
*             GO     TO    COMRMHF-RD-EXIT
*    END-READ.
*
*    ADD   1      TO  READ-CNT.
*
*    IF  PARA-BT-DATE   NOT =  RMH-F78
*    OR  PARA-BT-TIME   NOT =  RMH-F79
*    OR  PARA-BT-TOKCD  NOT =  RMH-F01
*        MOVE     "END"           TO   END-FLG
*    END-IF.
*
*COMRMHF-RD-EXIT.
*    EXIT.
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
*         NOT AT END
*             MOVE      FUR5-REC     TO   REC
*             ADD       1            TO   READ-CNT
     END-READ.
*
     IF       FUR5-F92 > PARA-DTO
              MOVE     "END"      TO   END-FLG
              GO     TO    FURIDOL5-RD-EXIT
     END-IF.
*
     MOVE     FUR5-REC            TO   REC.
     ADD      1                   TO   READ-CNT.
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
*         NOT AT END
*             MOVE      FUR2-REC     TO   REC
*             ADD       1            TO   READ-CNT
     END-READ.
*
     IF       FUR2-F87 > PARA-DTO
              MOVE     "END"      TO   END-FLG
              GO     TO    FURIDOL2-RD-EXIT
     END-IF.
*
     MOVE     FUR2-REC            TO   REC.
     ADD      1                   TO   READ-CNT.
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
*         NOT AT END
*             MOVE      FUR3-REC     TO   REC
*             ADD       1            TO   READ-CNT
     END-READ.
*
     IF       FUR3-F89 > PARA-DTO
              MOVE     "END"      TO   END-FLG
              GO     TO    FURIDOL3-RD-EXIT
     END-IF.
*
     MOVE     FUR3-REC            TO   REC.
     ADD      1                   TO   READ-CNT.
*
 FURIDOL3-RD-EXIT.
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
*             担当者マスタ読込
***************************************************************
 TANMS1-READ-SEC        SECTION.
*
     MOVE    "TANMS1-READ-SEC"  TO        S-NAME.
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
     MOVE     SPACE               TO   WK-MEISAI1.
     INITIALIZE                        WK-MEISAI1.
*項目転送
     MOVE   X"28"                 TO   CSV-A02A  CSV-A04A
                                       CSV-A21A.
     MOVE   X"29"                 TO   CSV-A02B  CSV-A04B
                                       CSV-A21B.
*
     MOVE     ","                 TO   CSV-A01 CSV-A02
                                       CSV-A03 CSV-A04 CSV-A05
                                       CSV-A06 CSV-A07 CSV-A08
                                       CSV-A09 CSV-A10 CSV-A11
                                       CSV-A12 CSV-A13 CSV-A14
                                       CSV-A15 CSV-A16 CSV-A17
                                       CSV-A18 CSV-A19 CSV-A20
                                       CSV-A21 CSV-A22 CSV-A23.
*
*  伝票番号ブレイク時
     IF      ( F01       NOT =   BRK-DENNO )
*        合計
              PERFORM   DENKEI-SEC
              MOVE   ZERO         TO    WK-DENKEI
              INITIALIZE                WK-DENKEI
*             IF  ( RMH-F04   NOT =   BRK-TENCD )
*                 MOVE  SPACE     TO    RD1-FLG
*             END-IF
*             MOVE  RMH-F02       TO    BRK-TENCD
              MOVE  F01           TO    BRK-DENNO
              MOVE  SPACE         TO    RD2-FLG
     END-IF.
*  明細行編集
*　振替元部門
     MOVE     F05                 TO    MD01-MOTOBUCD
                                        JYO-F02.
     MOVE     22                  TO    JYO-F01.
     PERFORM  JYOKEN1-READ-SEC.
     IF   JYOKEN1-INV-FLG = "INV"
          MOVE ALL NC"＊"         TO    MD01-MOTOBUNM
     ELSE
          MOVE JYO-F03            TO    MD01-MOTOBUNM
     END-IF.
*　振替先部門
     MOVE     F03                 TO    MD01-SAKIBUCD
                                        JYO-F02.
     MOVE     22                  TO    JYO-F01.
     PERFORM  JYOKEN1-READ-SEC.
     IF   JYOKEN1-INV-FLG = "INV"
          MOVE ALL NC"＊"         TO    MD01-SAKIBUNM
     ELSE
          MOVE JYO-F03            TO    MD01-SAKIBUNM
     END-IF.
*　倉庫
     MOVE     F06                 TO    MD01-MOTOSOKCD
                                        SOK-F01.
*  伝票番号
     MOVE     F01                 TO    MD01-DENNO.
*  行番号
     MOVE     F02                 TO    MD01-GYONO.
*  振替日
     MOVE     F07                 TO    MD01-FURI-DATE.
*  サカタ商品ＣＤ
     MOVE     F08                 TO    MD01-SYOCD  MEI-F011.
*  品単１
     MOVE     F09                 TO    MD01-HINCD1 MEI-F0121.
*  品単２
     MOVE     F10                 TO    MD01-HINCD2 MEI-F0122.
*  品単３
     MOVE     F11                 TO    MD01-HINCD3 MEI-F0123.
*  商品名
     PERFORM  MEIMS1-READ-SEC.
     IF   MEIMS1-INV-FLG = "INV"
          MOVE  ALL "*"        TO       MD01-SYONM
     ELSE
          MOVE  MEI-F031       TO       MD01-SYONM(1:15)
          MOVE  MEI-F032       TO       MD01-SYONM(16:15)
     END-IF.
*  数量
     MOVE     F12                 TO    MD01-SURYO.
*  単価
     MOVE     F13                 TO    MD01-TANKA.
*  原価金額
*    MOVE     RMH-F18             TO    MD01-GENKA-KINGAKU.
*  備考
     MOVE     F14                 TO    MD01-BIKOU.
*  作成日
*    MOVE     RMH-F03(1:4)        TO    MD01-KENSYUBI(1:4).
*    MOVE     "/"                 TO    MD01-KENSYUBI(5:1).
*    MOVE     RMH-F03(5:2)        TO    MD01-KENSYUBI(6:2).
*    MOVE     "/"                 TO    MD01-KENSYUBI(8:1).
*    MOVE     RMH-F03(7:2)        TO    MD01-KENSYUBI(9:2).
*  作成日
     IF       F92     = ZERO
              MOVE    SPACE     TO    MD01-SAKUSEI-DATE
     ELSE
              MOVE    F92(1:4)  TO    MD01-SAKUSEI-DATE(1:4)
              MOVE    "/"       TO    MD01-SAKUSEI-DATE(5:1)
              MOVE    F92(5:2)  TO    MD01-SAKUSEI-DATE(6:2)
              MOVE    "/"       TO    MD01-SAKUSEI-DATE(8:1)
              MOVE    F92(7:2)  TO    MD01-SAKUSEI-DATE(9:2)
     END-IF.
*  計上日
*    MOVE     RMH-F07(1:4)        TO    MD01-JKENSYUBI(1:4).
*    MOVE     "/"                 TO    MD01-JKENSYUBI(5:1).
*    MOVE     RMH-F07(5:2)        TO    MD01-JKENSYUBI(6:2).
*    MOVE     "/"                 TO    MD01-JKENSYUBI(8:1).
*    MOVE     RMH-F07(7:2)        TO    MD01-JKENSYUBI(9:2).
     IF       F87     = ZERO
              MOVE    SPACE     TO    MD01-KEIJO-DATE
     ELSE
              MOVE    F87(1:4)  TO    MD01-KEIJO-DATE(1:4)
              MOVE    "/"       TO    MD01-KEIJO-DATE(5:1)
              MOVE    F87(5:2)  TO    MD01-KEIJO-DATE(6:2)
              MOVE    "/"       TO    MD01-KEIJO-DATE(8:1)
              MOVE    F87(7:2)  TO    MD01-KEIJO-DATE(9:2)
     END-IF.
*  修正日
*    MOVE     RMH-F83(1:4)        TO    MD01-NYURYOKUBI(1:4).
*    MOVE     "/"                 TO    MD01-NYURYOKUBI(5:1).
*    MOVE     RMH-F83(5:2)        TO    MD01-NYURYOKUBI(6:2).
*    MOVE     "/"                 TO    MD01-NYURYOKUBI(8:1).
*    MOVE     RMH-F83(7:2)        TO    MD01-NYURYOKUBI(9:2).
     IF       F89     = ZERO
              MOVE    SPACE     TO    MD01-SYUSEI-DATE
     ELSE
              MOVE    F89(1:4)  TO    MD01-SYUSEI-DATE(1:4)
              MOVE    "/"       TO    MD01-SYUSEI-DATE(5:1)
              MOVE    F89(5:2)  TO    MD01-SYUSEI-DATE(6:2)
              MOVE    "/"       TO    MD01-SYUSEI-DATE(8:1)
              MOVE    F89(7:2)  TO    MD01-SYUSEI-DATE(9:2)
     END-IF.
*  修正担当者
*    MOVE     RMH-F82             TO    MD01-NYURYOKU.
*    MOVE     RMH-F81             TO    TAN-F01.
*    MOVE     RMH-F82             TO    TAN-F02.
*    PERFORM  TANMS1-READ-SEC.
*    IF  TANMS1-INV-FLG = "INV"
*        MOVE ALL NC"＊"          TO    MD01-NYURYOKUNM
*    ELSE
*        MOVE TAN-F03             TO    MD01-NYURYOKUNM
*    END-IF.
     IF     ( F90     = SPACE ) OR
            ( F91     = SPACE )
              MOVE    SPACE     TO    MD01-TANCD
              MOVE    SPACE     TO    MD01-TANNM
     ELSE
              MOVE    F90       TO    TAN-F01
              MOVE    F91       TO    TAN-F02  MD01-TANCD
              PERFORM TANMS1-READ-SEC
              IF      TANMS1-INV-FLG = "INV"
                      MOVE  ALL NC"＊" TO  MD01-TANNM
              ELSE
                      MOVE  TAN-F03    TO  MD01-TANNM
              END-IF
     END-IF.
*  ACOS計上区分
     MOVE     F86                 TO    MD01-ACOS.
*  ERR区分
     IF       F83     =  "1"
              MOVE       "E"      TO        MD01-ERR
     ELSE
              MOVE       " "      TO        MD01-ERR
     END-IF.
*
*--------------
*  伝票計加算
*--------------
*  移動数　
     COMPUTE  WK-SURYO-DEN    = WK-SURYO-DEN  +  F12.
*  原価金額
*    COMPUTE  WK-KINGAKU-DEN  = WK-KINGAKU-DEN  +  RMH-F18.
*レコードセット
     MOVE     WK-MEISAI1   TO   CSV-REC.
     MOVE     99           TO   MD01-GYONO.
     MOVE     WK-DENMEISAI1 TO  WK-DENMEISAI1-SAV.
*
     WRITE    CSV-REC.
     ADD      1            TO   OUTPUT-CNT.
*    次レコード読込み
*    PERFORM  COMRMHF-RD-SEC.
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
     MOVE     SYS-YY            TO   HD01-YYYYMMDD(1:4).
     MOVE     "/"               TO   HD01-YYYYMMDD(5:1).
     MOVE     SYS-MM            TO   HD01-YYYYMMDD(6:2).
     MOVE     "/"               TO   HD01-YYYYMMDD(8:1).
     MOVE     SYS-DD            TO   HD01-YYYYMMDD(9:2).
     MOVE     SYSTEM-TIME       TO   HD01-HHMMSS.
     MOVE     SYS-HH            TO   HD01-HHMMSS(1:2).
     MOVE     ":"               TO   HD01-HHMMSS(3:1).
     MOVE     SYS-MN            TO   HD01-HHMMSS(4:2).
     MOVE     ":"               TO   HD01-HHMMSS(6:1).
     MOVE     SYS-SS            TO   HD01-HHMMSS(7:2).
*日付区分
     IF    PARA-DKBN    =  "1"
           MOVE NC"作成日"      TO   HD01-DKBN
     END-IF.
     IF    PARA-DKBN    =  "2"
           MOVE NC"計上日"      TO   HD01-DKBN
     END-IF.
     IF    PARA-DKBN    =  "3"
           MOVE NC"修正日"      TO   HD01-DKBN
     END-IF.
*日付範囲
     MOVE     PARA-DFROM  (1:4) TO   HD02-FR-DATE(1:4).
     MOVE     "/"               TO   HD02-FR-DATE(5:1).
     MOVE     PARA-DFROM  (5:2) TO   HD02-FR-DATE(6:2).
     MOVE     "/"               TO   HD02-FR-DATE(8:1).
     MOVE     PARA-DFROM  (7:2) TO   HD02-FR-DATE(9:2).
*
     MOVE     PARA-DTO    (1:4) TO   HD02-TO-DATE(1:4).
     MOVE     "/"               TO   HD02-TO-DATE(5:1).
     MOVE     PARA-DTO    (5:2) TO   HD02-TO-DATE(6:2).
     MOVE     "/"               TO   HD02-TO-DATE(8:1).
     MOVE     PARA-DTO    (7:2) TO   HD02-TO-DATE(9:2).
*
 MIDASISET-EXIT.
     EXIT.
****************************************************************
*             伝票合計出力　                　　
****************************************************************
 DENKEI-SEC             SECTION.
*
     MOVE    "DENKEI-SEC"        TO   S-NAME.
*初期化
     MOVE     SPACE               TO   CSV-REC.
*    MOVE     SPACE               TO   WK-MEISAI2.
*    INITIALIZE                        WK-MEISAI2.
*項目転送
     MOVE   X"28"                 TO   CSV-B06A.
     MOVE   X"29"                 TO   CSV-B06B.
*
     MOVE     ","                 TO   CSV-B01 CSV-B02
                                       CSV-B03 CSV-B04
                                       CSV-B07 CSV-B08.
*--------------
*  合計転送
*--------------
*  タイトル転送
     MOVE    WK-DENMEISAI1-SAV    TO   CSV-KYOTU.
*  タイトル転送
     MOVE     NC"＜伝票合計＞"    TO   GK01-TAITOL.
*  移動数　
     MOVE     WK-SURYO-DEN        TO   GK01-GKSURYO.
*  原価金額
*    MOVE     WK-KINGAKU-DEN      TO   GK01-KINGAKU.
*
*伝票合計出力
*レコードセット
     MOVE     WK-MEISAI2    TO   CSV-REC.
*
     WRITE    CSV-REC.
     ADD      1            TO   OUTPUT-CNT.
*
 DENKEI-EXIT.
     EXIT.
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
* 伝票計出力
     IF  READ-CNT  > 0
         PERFORM   DENKEI-SEC
     END-IF.
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
*
*    CLOSE    COMRMHF ZSOKMS1 MEIMS1 JYOKEN1 TANMS1 FURIDCSV.
     EVALUATE PARA-DKBN
         WHEN "1"
              CLOSE  FURIDOL5
         WHEN "2"
              CLOSE  FURIDOL2
         WHEN "3"
              CLOSE  FURIDOL3
     END-EVALUATE.
     CLOSE    ZSOKMS1 MEIMS1 JYOKEN1 TANMS1 FURIDCSV.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.

```
