# SJR0400V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJR0400V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受領返品取込機能構築　　　　      *
*    業務名　　　　　　　：　伝票纏め返品　　　　　            *
*    モジュール名　　　　：　伝票纏め結果リストＣＳＶ　　　　　*
*    作成日／更新日　　　：　2017/08/21                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　パラメタを受取り、伝票纏め返品累積*
*                          データより範囲内のＣＳＶを出力する。*
*    更新日／更新者　　　：　2017/11/22 INOUE                  *
*    更新概要　　　　　　：　・伝票合計行の行番号を９９に変更　*
*    　　　　　　　　　　：　・受入ＮＧ修正　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SJR0400V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          17/08/21.
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
     SELECT   COMRMHF           ASSIGN    TO   DA-01-VI-COMRMHL7
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    RMH-F78
                                                RMH-F79
                                                RMH-F01
                                                RMH-F02
                                                RMH-F03
                                                RMH-F04
                                                RMH-F05
                                 STATUS         RMH-STATUS.
*
****<<店舗マスタ　　　　　　 >>*********************************
     SELECT   TENMS1             ASSIGN    TO   DA-01-VI-TENMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TEN-F52  TEN-F011
                                 STATUS         TEN-STATUS.
*
****<<取引先マスタ　　　　　 >>*********************************
     SELECT   TOKMS2             ASSIGN    TO   DA-01-VI-TOKMS2
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
*****<<伝票纏め結果リストＣＳＶ>>***************************
     SELECT   COMMCSVD           ASSIGN    TO   COMMCSVD
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
 FD  COMRMHF           LABEL RECORD   IS   STANDARD.
     COPY     COMRMHF   OF        XFDLIB
              JOINING   RMH       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 店舗マスタ　　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 取引先マスタ　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
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
 FD  TANMS1              LABEL RECORD   IS   STANDARD.
     COPY     HTANMS     OF        XFDLIB
              JOINING   TAN       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ受領データＣＳＶ　　　　　                   *
*--------------------------------------------------------------*
 FD  COMMCSVD           BLOCK CONTAINS 1    RECORDS.
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
 01  TENMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  TOKMS2-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  JYOKEN1-INV-FLG              PIC       X(03)  VALUE  SPACE.
 01  TANMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  RD1-FLG                      PIC       X(01)  VALUE  SPACE.
 01  RD2-FLG                      PIC       X(01)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  RMH-STATUS                   PIC       X(02).
 01  TEN-STATUS                   PIC       X(02).
 01  TOK-STATUS                   PIC       X(02).
 01  JYO-STATUS                   PIC       X(02).
 01  TAN-STATUS                   PIC       X(02).
 01  CSV-STATUS                   PIC       X(02).
*
 01  BRK-KEY.
     03  BRK-TENCD                PIC      S9(05)  VALUE  ZERO.
     03  BRK-DENNO                PIC      S9(09)  VALUE  ZERO.
     03  BRK-TENMEI               PIC      N(15).
     03  BRK-TOKMEI               PIC      N(15).
     03  BRK-DNKNM                PIC      N(05).
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
     03  WK-SURYO-DEN             PIC      S9(06)V9.
     03  WK-KENSHUSU-DEN          PIC      S9(06)V9.
     03  WK-KINGAKU-DEN           PIC      S9(07).
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  READ2-CNT                    PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
 01  IX1                          PIC       9(04)  VALUE  ZERO.
*タイトルエリア
*見出しエリア
 01  WK-HEAD1.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(11)  VALUE
                       NC"＜伝票纏め結果リスト＞".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"発行日：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD01-YYYYMMDD PIC X(10).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"発行時刻：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD01-HHMMSS   PIC X(08).
 01  WK-HEAD2.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"バッチ日付：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD02-BT-DATE  PIC X(10).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"バッチ時刻：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD02-BT-TIME  PIC X(05).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"バッチ取引先：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  HD02-BT-TOKCD PIC 9(08).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  HD02-BT-TOKNM PIC N(15).
     03  FILLER        PIC X(01)  VALUE  X"29".
 01  WK-HEAD3.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(05)  VALUE  NC"取引先ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(04)  VALUE  NC"取引先名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(04)  VALUE  NC"店舗ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER               PIC  N(03)  VALUE  NC"店舗名".
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
     03  FILLER        PIC N(06)  VALUE NC"相手伝票区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE NC"自社伝票区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"検収日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"実検収日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"入力日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"計上日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"出荷場所".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"入力者ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"入力者".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"相手商品ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"サカタ商品".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"商品名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"数量".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"原価単価".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"原価金額".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(08)  VALUE  NC"備考（個別番号）".
     03  FILLER        PIC X(01)  VALUE  X"29".

*明細エリア
 01  WK-MEISAI1.
     03  WK-DENMEISAI1.
*A00***************
         05  CSV-A00                  PIC  X(01).
*A01***************
*↓2017/11/22NG修正
***      05  MD01-TOKCD               PIC  9(05).
         05  MD01-TOKCD               PIC  9(08).
*↑2017/11/22NG修正
         05  CSV-A01                  PIC  X(01).
*A02***************
         05  CSV-A02A                 PIC  X(01).
         05  MD01-TOKNM               PIC  N(15).
         05  CSV-A02B                 PIC  X(01).
         05  CSV-A02                  PIC  X(01).
*A03***************
         05  MD01-TENCD               PIC  9(05).
         05  CSV-A03                  PIC  X(01).
*A04***************
         05  CSV-A04A                 PIC  X(01).
         05  MD01-TENNM               PIC  N(15).
         05  CSV-A04B                 PIC  X(01).
         05  CSV-A04                  PIC  X(01).
*A05***************
         05  MD01-DENNO               PIC  9(09).
         05  CSV-A05                  PIC  X(01).
*A06***************
         05  MD01-GYO                 PIC  9(02).
         05  CSV-A06                  PIC  X(01).
*A07***************
         05  MD01-ADENKU              PIC  X(02).
         05  CSV-A07                  PIC  X(01).
*A08***************
         05  MD01-JDENKU              PIC  X(02).
         05  CSV-A08K                 PIC  X(01).
         05  CSV-A08A                 PIC  X(01).
         05  MD01-JDENKUNM            PIC  N(04).
         05  CSV-A08B                 PIC  X(01).
         05  CSV-A08                  PIC  X(01).
*A09***************
         05  MD01-KENSYUBI            PIC  X(10).
         05  CSV-A09                  PIC  X(01).
*A10***************
         05  MD01-JKENSYUBI           PIC  X(10).
         05  CSV-A10                  PIC  X(01).
*A11***************
         05  MD01-NYURYOKUBI          PIC  X(10).
         05  CSV-A11                  PIC  X(01).
*A12***************
         05  MD01-KEIJYOBI            PIC  X(10).
         05  CSV-A12                  PIC  X(01).
*A13***************
         05  MD01-SYUKABASYO          PIC  X(02).
         05  CSV-A13                  PIC  X(01).
*A14***************
         05  MD01-NYURYOKU            PIC  X(02).
         05  CSV-A14                  PIC  X(01).
*A15***************
         05  CSV-A15A                 PIC  X(01).
         05  MD01-NYURYOKUNM          PIC  N(10).
         05  CSV-A15B                 PIC  X(01).
         05  CSV-A15                  PIC  X(01).
     03  WK-DENMEISAI2.
*A16***************
         05  MD01-AITECD              PIC  X(13).
         05  CSV-A16                  PIC  X(01).
*A17***************
         05  MD01-SKTSYOCD            PIC  X(19).
         05  CSV-A17                  PIC  X(01).
*A18***************
         05  MD01-SKTSYONM1           PIC  X(15).
         05  MD01-SKTSYONM2           PIC  X(15).
         05  CSV-A18                  PIC  X(01).
*A19***************
         05  MD01-SURYO               PIC  --------9.99.
         05  CSV-A19                  PIC  X(01).
*A20***************
         05  MD01-GENKA-TANKA         PIC  --------9.99.
         05  CSV-A20                  PIC  X(01).
*A21***************
         05  MD01-GENKA-KINGAKU       PIC  --------9.99.
         05  CSV-A21                  PIC  X(01).
*A22***************
         05  MD01-MEISAI-BIKO         PIC  X(10).
*合計行
 01  WK-MEISAI2.
*↓2017/11/22NG修正
***  03  CSV-KYOTU                    PIC  X(182).
     03  CSV-KYOTU                    PIC  X(185).
*↑2017/11/22NG修正
     03  CSV-B01                      PIC  X(01).
     03  CSV-B02                      PIC  X(01).
     03  CSV-B03A                     PIC  X(01).
     03  GK01-TAITOL                  PIC  N(04).
     03  CSV-B03B                     PIC  X(01).
     03  CSV-B03                      PIC  X(01).
     03  GK01-GKSURYO                 PIC  --------9.99.
     03  CSV-B04                      PIC  X(01).
     03  CSV-B05                      PIC  X(01).
     03  GK01-KINGAKU                 PIC  --------9.
*
 01  WK-DENMEISAI1-SAV.
*↓2017/11/22NG修正
***  03  WK-DENMEISAI1-BAK            PIC  X(182)  VALUE  SPACE.
     03  WK-DENMEISAI1-BAK            PIC  X(185)  VALUE  SPACE.
*↑2017/11/22NG修正
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR0400V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0400V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SJR0400V".
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
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-BUMON             PIC   X(04).
 01  PARA-TANCD             PIC   X(02).
 01  PARA-BT-DATE           PIC   9(08).
 01  PARA-BT-TIME           PIC   9(04).
 01  PARA-BT-TOKCD          PIC   9(08).
*
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-BUMON
                                           PARA-TANCD
                                           PARA-BT-DATE
                                           PARA-BT-TIME
                                           PARA-BT-TOKCD.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           COMRMHF.
     MOVE     "COMRMHF"           TO        ERR-FL-ID.
     MOVE     RMH-STATUS          TO        ERR-STCD.
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
                        PROCEDURE TOKMS2.
     MOVE     "TOKMS2  "          TO        ERR-FL-ID.
     MOVE     TOK-STATUS          TO        ERR-STCD.
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
 FILEERROR-SEC5         SECTION.
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
 FILEERROR-SEC6         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE COMMCSVD.
     MOVE     "COMMCSVD"          TO        ERR-FL-ID.
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
 SJR0400V-START         SECTION.
*
     MOVE   "SJR0400V-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     IF   END-FLG  NOT =  "END"
          PERFORM    MAIN-SEC  UNTIL  END-FLG   =  "END"
     END-IF.
     PERFORM            END-SEC.
     STOP               RUN.
*
 SJR0400V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     COMRMHF.
     OPEN     INPUT     TENMS1.
     OPEN     INPUT     TOKMS2.
     OPEN     INPUT     JYOKEN1.
     OPEN     INPUT     TANMS1.
     OPEN     OUTPUT    COMMCSVD.
     DISPLAY  MSG-START UPON CONS.
*
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
     MOVE  SPACE                TO   RMH-REC.
     INITIALIZE                      RMH-REC.
*
     MOVE  PARA-BT-DATE         TO   RMH-F78.
     MOVE  PARA-BT-TIME         TO   RMH-F79.
     MOVE  PARA-BT-TOKCD        TO   RMH-F01.
     START  COMRMHF  KEY  >= RMH-F78 RMH-F79 RMH-F01
                             RMH-F02 RMH-F03 RMH-F04
                             RMH-F05
         INVALID   KEY
            MOVE     "END"      TO   END-FLG
            DISPLAY NC"＃対象データ無し１＃" UPON CONS
            GO                  TO   INIT-EXIT
     END-START
*
*
     PERFORM  COMRMHF-RD-SEC.
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
     PERFORM   MIDASISET-SEC.
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
*　ブレイクキー設定
     MOVE  RMH-F02                TO   BRK-TENCD.
     MOVE  RMH-F04                TO   BRK-DENNO.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ケーヨー　受領累積データ読み込み　　　
****************************************************************
 COMRMHF-RD-SEC            SECTION.
*
     MOVE    "COMRMHF-RD-SEC"    TO   S-NAME.
*
     READ     COMRMHF
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    COMRMHF-RD-EXIT
     END-READ.
*
     ADD   1      TO  READ-CNT.
*
     IF  PARA-BT-DATE   NOT =  RMH-F78
     OR  PARA-BT-TIME   NOT =  RMH-F79
     OR  PARA-BT-TOKCD  NOT =  RMH-F01
         MOVE     "END"           TO   END-FLG
     END-IF.
*
 COMRMHF-RD-EXIT.
     EXIT.
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
 TOKMS2-READ-SEC        SECTION.
*
     MOVE    "TOKMS2-READ-SEC"  TO        S-NAME.
*
     READ     TOKMS2
              INVALID      MOVE  "INV"    TO   TOKMS2-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   TOKMS2-INV-FLG
     END-READ.
*
 TOKMS2-READ-EXIT.
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
                                       CSV-A08A  CSV-A15A.
     MOVE   X"29"                 TO   CSV-A02B  CSV-A04B
                                       CSV-A08B  CSV-A15B.
*
     MOVE     ","                 TO   CSV-A00 CSV-A01 CSV-A02
                                       CSV-A03 CSV-A04 CSV-A05
                                       CSV-A06 CSV-A07 CSV-A08
                                       CSV-A09 CSV-A10 CSV-A11
                                       CSV-A12 CSV-A13 CSV-A14
                                       CSV-A15 CSV-A16 CSV-A17
                                       CSV-A18 CSV-A19 CSV-A20
                                       CSV-A21.
*
*  店舗・伝票番号ブレイク時
     IF      ( RMH-F02   NOT =   BRK-TENCD )  OR
             ( RMH-F04   NOT =   BRK-DENNO )
*        合計
              PERFORM   DENKEI-SEC
              MOVE   ZERO         TO    WK-DENKEI
              INITIALIZE                WK-DENKEI
              IF  ( RMH-F04   NOT =   BRK-TENCD )
                  MOVE  SPACE         TO    RD1-FLG
              END-IF
              MOVE  RMH-F02       TO    BRK-TENCD
              MOVE  RMH-F04       TO    BRK-DENNO
              MOVE  SPACE         TO    RD2-FLG
     END-IF.
*  明細行編集
*　取引先ＣＤ／取引先名
     MOVE     RMH-F01             TO    MD01-TOKCD.
     MOVE     RMH-F01             TO    TOK-F01.
     PERFORM  TOKMS2-READ-SEC.
     IF   TOKMS2-INV-FLG = "INV"
          MOVE ALL NC"＊"         TO    MD01-TOKNM
     ELSE
          MOVE TOK-F02            TO    MD01-TOKNM
     END-IF.
*　店舗ＣＤ
     MOVE     RMH-F02             TO    MD01-TENCD.
*  店舗正式名
     MOVE     RMH-F02             TO    TEN-F011.
     MOVE     RMH-F01             TO    TEN-F52.
     PERFORM  TENMS1-READ-SEC.
     IF  TENMS1-INV-FLG = "INV"
         MOVE ALL NC"＊"          TO    MD01-TENNM
     ELSE
         MOVE TEN-F02             TO    MD01-TENNM
     END-IF.
*  伝票番号
     MOVE     RMH-F04             TO    MD01-DENNO.
*  行番号
     MOVE     RMH-F05             TO    MD01-GYO.
*  相手伝区
     MOVE     RMH-F06             TO    MD01-ADENKU.
*  自社伝区
     MOVE     RMH-F23             TO    MD01-JDENKU.
     MOVE     1                   TO    JYO-F01.
     MOVE     RMH-F23             TO    JYO-F02.
     MOVE     ":"                 TO    CSV-A08K.
     PERFORM  JYOKEN1-READ-SEC.
     IF  JYOKEN1-INV-FLG = "INV"
         MOVE ALL NC"＊"          TO    MD01-JDENKUNM
     ELSE
         MOVE JYO-F03             TO    MD01-JDENKUNM
     END-IF.
*  検収日
     MOVE     RMH-F03(1:4)        TO    MD01-KENSYUBI(1:4).
     MOVE     "/"                 TO    MD01-KENSYUBI(5:1).
     MOVE     RMH-F03(5:2)        TO    MD01-KENSYUBI(6:2).
     MOVE     "/"                 TO    MD01-KENSYUBI(8:1).
     MOVE     RMH-F03(7:2)        TO    MD01-KENSYUBI(9:2).
*  実検収日
     MOVE     RMH-F07(1:4)        TO    MD01-JKENSYUBI(1:4).
     MOVE     "/"                 TO    MD01-JKENSYUBI(5:1).
     MOVE     RMH-F07(5:2)        TO    MD01-JKENSYUBI(6:2).
     MOVE     "/"                 TO    MD01-JKENSYUBI(8:1).
     MOVE     RMH-F07(7:2)        TO    MD01-JKENSYUBI(9:2).
*  入力日
     MOVE     RMH-F83(1:4)        TO    MD01-NYURYOKUBI(1:4).
     MOVE     "/"                 TO    MD01-NYURYOKUBI(5:1).
     MOVE     RMH-F83(5:2)        TO    MD01-NYURYOKUBI(6:2).
     MOVE     "/"                 TO    MD01-NYURYOKUBI(8:1).
     MOVE     RMH-F83(7:2)        TO    MD01-NYURYOKUBI(9:2).
*  計上日
     MOVE     RMH-F86(1:4)        TO    MD01-KEIJYOBI(1:4).
     MOVE     "/"                 TO    MD01-KEIJYOBI(5:1).
     MOVE     RMH-F86(5:2)        TO    MD01-KEIJYOBI(6:2).
     MOVE     "/"                 TO    MD01-KEIJYOBI(8:1).
     MOVE     RMH-F86(7:2)        TO    MD01-KEIJYOBI(9:2).
*  出荷場所
     MOVE     RMH-F08             TO    MD01-SYUKABASYO.
*  入力者
     MOVE     RMH-F82             TO    MD01-NYURYOKU.
*  入力者名
     MOVE     RMH-F81             TO    TAN-F01.
     MOVE     RMH-F82             TO    TAN-F02.
     PERFORM  TANMS1-READ-SEC.
     IF  TANMS1-INV-FLG = "INV"
         MOVE ALL NC"＊"          TO    MD01-NYURYOKUNM
     ELSE
         MOVE TAN-F03             TO    MD01-NYURYOKUNM
     END-IF.
*  JANCD
     MOVE     RMH-F09             TO    MD01-AITECD.
*  商品ＣＤ
     MOVE     RMH-F10             TO    MD01-SKTSYOCD(1:8).
     MOVE     SPACE               TO    MD01-SKTSYOCD(9:1).
     MOVE     RMH-F11             TO    MD01-SKTSYOCD(10:5).
     MOVE     "-"                 TO    MD01-SKTSYOCD(15:1).
     MOVE     RMH-F12             TO    MD01-SKTSYOCD(16:2).
     MOVE     "-"                 TO    MD01-SKTSYOCD(18:1).
     MOVE     RMH-F13             TO    MD01-SKTSYOCD(19:1).
*  商品名
     MOVE     RMH-F14             TO    MD01-SKTSYONM1.
     MOVE     RMH-F15             TO    MD01-SKTSYONM2.
*  数量
     MOVE     RMH-F16             TO    MD01-SURYO.
*  原価単価
     MOVE     RMH-F17             TO    MD01-GENKA-TANKA.
*  原価金額
     MOVE     RMH-F18             TO    MD01-GENKA-KINGAKU.
*  備考
     MOVE     RMH-F28             TO    MD01-MEISAI-BIKO.
*--------------
*  伝票計加算
*--------------
*  出荷数　
     COMPUTE  WK-SURYO-DEN    = WK-SURYO-DEN  +  RMH-F16.
*  原価金額
     COMPUTE  WK-KINGAKU-DEN  = WK-KINGAKU-DEN  +  RMH-F18.
*レコードセット
     MOVE     WK-MEISAI1   TO   CSV-REC.
     MOVE     99           TO   MD01-GYO.
     MOVE     WK-DENMEISAI1 TO  WK-DENMEISAI1-SAV.
*
     WRITE    CSV-REC.
     ADD      1            TO   OUTPUT-CNT.
*    次レコード読込み
     PERFORM  COMRMHF-RD-SEC.
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
*****MOVE     SYS-DATE-R        TO   HD01-YYYYMMDD.
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
*バッチ番号セット
     MOVE     PARA-BT-DATE(1:4) TO   HD02-BT-DATE(1:4).
     MOVE     "/"               TO   HD02-BT-DATE(5:1).
     MOVE     PARA-BT-DATE(5:2) TO   HD02-BT-DATE(6:2).
     MOVE     "/"               TO   HD02-BT-DATE(8:1).
     MOVE     PARA-BT-DATE(7:2) TO   HD02-BT-DATE(9:2).
     MOVE     PARA-BT-TIME(1:2) TO   HD02-BT-TIME(1:2).
     MOVE     ":"               TO   HD02-BT-TIME(3:1).
     MOVE     PARA-BT-TIME(3:2) TO   HD02-BT-TIME(4:2).
     MOVE     PARA-BT-TOKCD     TO   HD02-BT-TOKCD.
*****取引先名取得
     MOVE     PARA-BT-TOKCD     TO   TOK-F01.
     PERFORM  TOKMS2-READ-SEC.
     IF       TOKMS2-INV-FLG  =  "INV"
              MOVE   ALL NC"＊" TO   HD02-BT-TOKNM
     ELSE
              MOVE   TOK-F02    TO   HD02-BT-TOKNM
     END-IF.
*
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
     MOVE   X"28"                 TO   CSV-B03A.
     MOVE   X"29"                 TO   CSV-B03B.
*
     MOVE     ","                 TO   CSV-B01 CSV-B02
                                       CSV-B03 CSV-B04
                                       CSV-B05.
*--------------
*  合計転送
*--------------
*  タイトル転送
     MOVE    WK-DENMEISAI1-SAV    TO   CSV-KYOTU.
*  タイトル転送
     MOVE     NC"＜合計＞"        TO   GK01-TAITOL.
*  出荷数　
     MOVE     WK-SURYO-DEN        TO   GK01-GKSURYO.
*  原価金額
     MOVE     WK-KINGAKU-DEN      TO   GK01-KINGAKU.
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
     CLOSE    COMRMHF TENMS1 TOKMS2 JYOKEN1 TANMS1 COMMCSVD.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.

```
