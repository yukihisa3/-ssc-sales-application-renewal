****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　　　　　　　　　　　              *
*    モジュール名　　　　：　注文日別受注データ　　　　        *
*    　　　　　　　　　　　　ＣＳＶ出力　　　　　　　　        *
*    作成日／作成者　　　：　2023/07/24 MIURA                  *
*    流用　　　　　　　　：　SSY9430V                          *
*    処理概要　　　　　　：　注文日別受注データを　　　　　　　*
*    　　　　　　　　　　　　ＣＳＶ形式で出力する。　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            CHU0040V.
 AUTHOR.                NAV-ASSIST.
 DATE-WRITTEN.          2023/07/24.
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
*注文日別受注抽出データ
     SELECT   CHUMONL1  ASSIGN    TO        DA-01-VI-CHUMONL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       MON-F01   MON-F02
                                            MON-F04   MON-F051
                                            MON-F07   MON-F112
                                            MON-F03
                        FILE      STATUS    IS   MON-STATUS.
*SUB商品名称マスタ
     SELECT   SUBMEIL1  ASSIGN    TO        DA-01-VI-SUBMEIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SUB-F011  SUB-F0121
                                            SUB-F0122 SUB-F0123
                        FILE      STATUS    IS   SUB-STATUS.
*
****<<取引先マスタ　　　　 >>*********************************
     SELECT   TOKMS2             ASSIGN    TO   DA-01-VI-TOKMS2
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TOK-F01
                                 STATUS         TOK-STATUS.
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
****<<条件マスタ　　　　 >>*********************************
     SELECT   JYOKEN1            ASSIGN    TO   DA-01-VI-JYOKEN1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    JYO-F01
                                                JYO-F02
                                 STATUS         JYO-STATUS.
*
*****<<ＣＳＶデータ　　      >>*******************************
     SELECT   CHUMONDT          ASSIGN    TO   CHUMONDT
                                STATUS         CSV-STATUS.
*                                                              *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
******************************************************************
*    注文日別受注抽出データ
******************************************************************
 FD  CHUMONL1           LABEL RECORD   IS   STANDARD.
     COPY     CHUMONF   OF        XFDLIB
              JOINING   MON       PREFIX.
******************************************************************
*    SUB商品名称マスタ
******************************************************************
 FD  SUBMEIL1           LABEL RECORD   IS   STANDARD.
     COPY     SUBMEIF   OF        XFDLIB
              JOINING   SUB       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 取引先マスタ　　　　　　　                       *
*--------------------------------------------------------------*
 FD  TOKMS2              LABEL RECORD   IS   STANDARD.
     COPY     TOKMS2     OF        XFDLIB
              JOINING   TOK       PREFIX.
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
*--------------------------------------------------------------*
*    FILE = 条件マスタ　　　　　　　                       *
*--------------------------------------------------------------*
 FD  JYOKEN1             LABEL RECORD   IS   STANDARD.
     COPY     JYOKEN1    OF        XFDLIB
              JOINING   JYO       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ＣＳＶデータ　　　　　                   *
*--------------------------------------------------------------*
 FD  CHUMONDT           BLOCK CONTAINS 1   RECORDS.
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
 01  SUBMEIL1-INV-FLG             PIC       X(03)  VALUE  SPACE.
 01  TOKMS2-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  ZSOKMS1-INV-FLG              PIC       X(03)  VALUE  SPACE.
 01  TENMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  TANMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  JYOKEN1-INV-FLG              PIC       X(03)  VALUE  SPACE.
 01  RD1-FLG                      PIC       X(01)  VALUE  SPACE.
 01  RD2-FLG                      PIC       X(01)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  MON-STATUS                   PIC       X(02).
 01  SUB-STATUS                   PIC       X(02).
 01  TOK-STATUS                   PIC       X(02).
 01  SOK-STATUS                   PIC       X(02).
 01  TEN-STATUS                   PIC       X(02).
 01  TAN-STATUS                   PIC       X(02).
 01  JYO-STATUS                   PIC       X(02).
 01  CSV-STATUS                   PIC       X(02).
*01  BRK-KEY.
*    03  BRK-TENCD                PIC       9(05)  VALUE  ZERO.
*    03  BRK-TORCD                PIC       9(08)  VALUE  ZERO.
*    03  BRK-TANCD                PIC       X(02)  VALUE  SPACE.
*    03  BRK-SOKCD                PIC       9(10)  VALUE  ZERO.
*
*    03  BRK-TENMEI               PIC      N(15).
*    03  BRK-DNKNM                PIC      N(05).
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
*
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  READ2-CNT                    PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
 01  IX1                          PIC       9(04)  VALUE  ZERO.
*タイトルエリア
*見出しエリア
 01  WK-HEAD.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE
         NC"取引先ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"取引先名".
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
     03  FILLER        PIC N(04)  VALUE  NC"伝票区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"担当者".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"担当者名".
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
     03  FILLER        PIC N(04)  VALUE  NC"出荷場所".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"倉庫名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"発注日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"納品日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"相手商品ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(09)  VALUE  NC"たねまるＪＡＮＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"サカタ商品ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"サカタ品単ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"商品名１".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"商品名２".
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
     03  FILLER        PIC N(04)  VALUE  NC"売価単価".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"原価金額".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"売価金額".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"オーダー区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(08)  VALUE  NC"たねまる連携番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"抽出区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"登録日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"受信日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"商品カテゴリ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"商品カテゴリ名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"分類区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"分類区分名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"予備項目".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
*明細エリア
     COPY     CHUMONDT   OF        XFDLIB
              JOINING    MEI       PREFIX.
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "CHU0040V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "CHU0040V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "CHU0040V".
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
*LINKAGE                SECTION.
*01  PARA-IN-FROMDATE       PIC   9(08).
*01  PARA-IN-TODATE         PIC   9(08).
*01  PARA-IN-FROMTIME       PIC   9(06).
*01  PARA-IN-TOTIME         PIC   9(06).
*01  PARA-IN-OUTKBN         PIC   X(01).
*01  PARA-OUT-CNT           PIC   9(07).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION.
*PROCEDURE              DIVISION  USING
*                                          PARA-IN-FROMDATE
*                                          PARA-IN-TODATE
*                                          PARA-IN-FROMTIME
*                                          PARA-IN-TOTIME
*                                          PARA-IN-OUTKBN
*                                          PARA-OUT-CNT.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           CHUMONL1.
     MOVE     "CHUMONL1"          TO        ERR-FL-ID.
     MOVE     MON-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE SUBMEIL1.
     MOVE     "SUBMEIL1 "         TO        ERR-FL-ID.
     MOVE     SUB-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TOKMS2.
     MOVE     "TOKMS2   "         TO        ERR-FL-ID.
     MOVE     TOK-STATUS          TO        ERR-STCD.
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
                        PROCEDURE TENMS1.
     MOVE     "TENMS1  "          TO        ERR-FL-ID.
     MOVE     TEN-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC6         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE JYOKEN1.
     MOVE     "JYOKEN1  "         TO        ERR-FL-ID.
     MOVE     JYO-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC7         SECTION.
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
 FILEERROR-SEC8         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE CHUMONDT.
     MOVE     "CHUMONDT"          TO        ERR-FL-ID.
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
 CHU0040V-START         SECTION.
*
     MOVE   "CHU0040V-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     IF   END-FLG  NOT =  "END"
          PERFORM    MAIN-SEC  UNTIL  END-FLG   =  "END"
     END-IF.
     PERFORM            END-SEC.
     STOP               RUN.
*
 CHU0040V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     CHUMONL1.
     OPEN     INPUT     ZSOKMS1 TENMS1 TANMS1 TOKMS2 JYOKEN1
                        SUBMEIL1.
     OPEN     OUTPUT    CHUMONDT.
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
     MOVE  SPACE                TO   MON-REC.
     INITIALIZE                      MON-REC.
     START  CHUMONL1  KEY  >=   MON-F01  MON-F02  MON-F04
                                MON-F051 MON-F07  MON-F112
                                MON-F03
         INVALID   KEY
            MOVE     "END"      TO   END-FLG
            DISPLAY NC"＃対象データ無し１＃" UPON CONS
            GO                  TO   INIT-EXIT
     END-START.
*
     PERFORM  CHUMONL1-RD-SEC.
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
     MOVE     SPACE               TO   CSV-REC.
     MOVE     WK-HEAD             TO   CSV-REC.
     WRITE    CSV-REC.
     ADD      1            TO   OUTPUT-CNT.
*　ブレイクキー設定
*    MOVE  MON-F07    TO       BRK-TENCD.
*    MOVE  MON-F01    TO       BRK-TORCD.
*    MOVE  MON-F06    TO       BRK-TANCD.
*    MOVE  MON-F08    TO       BRK-SOKCD.
*    MOVE  MON-F1411  TO       BRK-SSCD.
*    MOVE  MON-F1412  TO       BRK-SHCD.
*    MOVE  MON-F70    TO       BRK-CATEGORY.
*    MOVE  MON-F71    TO       BRK-BUNRUI.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    注文日別受注データ読み込み　　　
****************************************************************
 CHUMONL1-RD-SEC            SECTION.
*
     MOVE    "CHUMONL1-RD-SEC"    TO   S-NAME.
*
     READ     CHUMONL1
          AT END
              MOVE     "END"      TO   END-FLG
              GO                  TO   CHUMONL1-RD-EXIT
     END-READ.
*
     ADD   1      TO  READ-CNT.
*
 CHUMONL1-RD-EXIT.
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
*             サブ商品名称マスタ読込
***************************************************************
 SUBMEIL1-READ-SEC        SECTION.
*
     MOVE    "SUBMEIL1-READ-SEC"   TO        S-NAME.
*
     READ     SUBMEIL1
              INVALID      MOVE  "INV"    TO  SUBMEIL1-INV-FLG
              NOT  INVALID MOVE  SPACE    TO  SUBMEIL1-INV-FLG
     END-READ.
*
 SUBMEIL1-READ-EXIT.
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
***************************************************************
*             条件マスタ読込
***************************************************************
 JYOKEN1-READ-SEC        SECTION.
*
     MOVE    "JYOKEN1-READ-SEC"   TO        S-NAME.
*
     READ     JYOKEN1
              INVALID      MOVE  "INV"    TO   JYOKEN1-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   JYOKEN1-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
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
     MOVE   X"28"                 TO   MEI-S01
                                       MEI-S02
                                       MEI-S03
                                       MEI-S04
                                       MEI-S05
                                       MEI-S06
                                       MEI-S07
                                       MEI-S08.
     MOVE   X"29"                 TO   MEI-E01
                                       MEI-E02
                                       MEI-E03
                                       MEI-E04
                                       MEI-E05
                                       MEI-E06
                                       MEI-E07
                                       MEI-E08.
*
     MOVE   ","                   TO   MEI-C01
                                       MEI-C02
                                       MEI-C03
                                       MEI-C04
                                       MEI-C05
                                       MEI-C06
                                       MEI-C07
                                       MEI-C08
                                       MEI-C09
                                       MEI-C10
                                       MEI-C11
                                       MEI-C12
                                       MEI-C13
                                       MEI-C14
                                       MEI-C15
                                       MEI-C16
                                       MEI-C17
                                       MEI-C18
                                       MEI-C19
                                       MEI-C20
                                       MEI-C21
                                       MEI-C22
                                       MEI-C23
                                       MEI-C24
                                       MEI-C25
                                       MEI-C26
                                       MEI-C27
                                       MEI-C28
                                       MEI-C29
                                       MEI-C30
                                       MEI-C31
                                       MEI-C32
                                       MEI-C33.
*
*  明細行編集
*  取引先CD
     MOVE     MON-F01           TO   MEI-F01 TOK-F01.
*  取引先名
     PERFORM  TOKMS2-READ-SEC.
     IF       TOKMS2-INV-FLG = "INV"
              MOVE ALL NC"＊"   TO    MEI-F02
     ELSE
              MOVE TOK-F02      TO    MEI-F02
     END-IF.
*  伝票番号
     MOVE     MON-F02           TO   MEI-F03.
*  行番号
     MOVE     MON-F03           TO   MEI-F04.
*  伝票区分
     MOVE     MON-F051          TO   MEI-F05.
*  担当者ＣＤ
     MOVE     MON-F06           TO    MEI-F06  TAN-F02.
     MOVE     "2920"            TO             TAN-F01.
*  担当者名
     PERFORM  TANMS1-READ-SEC.
     IF       TANMS1-INV-FLG = "INV"
              MOVE ALL NC"＊"   TO    MEI-F07
     ELSE
              MOVE TAN-F03      TO    MEI-F07
     END-IF.
*  店舗ＣＤ
     MOVE     MON-F07           TO    MEI-F08  TEN-F011.
     MOVE     MON-F01           TO             TEN-F52.
*  店舗名
     PERFORM  TENMS1-READ-SEC.
     IF       TENMS1-INV-FLG = "INV"
              MOVE ALL NC"＊"   TO    MEI-F09
     ELSE
              MOVE TEN-F02      TO    MEI-F09
     END-IF.
*  出荷場所　
     MOVE     MON-F08           TO    MEI-F10  SOK-F01.
*  倉庫名
     PERFORM  ZSOKMS1-READ-SEC.
     IF       ZSOKMS1-INV-FLG = "INV"
              MOVE ALL NC"＊"   TO    MEI-F11
     ELSE
              MOVE SOK-F02      TO    MEI-F11
     END-IF.
*  発注日
     MOVE     MON-F111          TO   WK-DATE.
     MOVE     WK-DATE-YYYY      TO   MEI-F12(1:4).
     MOVE     "/"               TO   MEI-F12(5:1).
     MOVE     WK-DATE-MM        TO   MEI-F12(6:2).
     MOVE     "/"               TO   MEI-F12(8:1).
     MOVE     WK-DATE-DD        TO   MEI-F12(9:2).
*  納品日
     MOVE     MON-F112          TO   WK-DATE.
     MOVE     WK-DATE-YYYY      TO   MEI-F13(1:4).
     MOVE     "/"               TO   MEI-F13(5:1).
     MOVE     WK-DATE-MM        TO   MEI-F13(6:2).
     MOVE     "/"               TO   MEI-F13(8:1).
     MOVE     WK-DATE-DD        TO   MEI-F13(9:2).
*  相手商品CD
     MOVE     MON-F25           TO    MEI-F14.
*  たねまるJANCD
     MOVE     MON-F1411         TO    MEI-F16 SUB-F011.
     MOVE     MON-F1412         TO    MEI-F17 SUB-F012.
     PERFORM  SUBMEIL1-READ-SEC.
     IF       SUBMEIL1-INV-FLG = "INV"
              MOVE ALL NC"＊"   TO    MEI-F18
              MOVE ALL NC"＊"   TO    MEI-F19
     ELSE
              MOVE SUB-D01      TO    MEI-F15
              MOVE SUB-F021     TO    MEI-F18
              MOVE SUB-F022     TO    MEI-F19
              MOVE SUB-D03      TO    MEI-F25
     END-IF.
*  数量
     MOVE     MON-F15           TO   MEI-F20.
*  原価単価
     MOVE     MON-F172          TO   MEI-F21.
*  売価単価
     MOVE     MON-F173          TO   MEI-F22.
*  原価金額
     MOVE     MON-F181          TO   MEI-F23.
*  売価金額
     MOVE     MON-F182          TO   MEI-F24.
*  たねまる連携番号
     MOVE     MON-D99           TO   MEI-F26.
*  登録日
     MOVE     MON-F62           TO   WK-DATE.
     MOVE     WK-DATE-YYYY      TO   MEI-F28(1:4).
     MOVE     "/"               TO   MEI-F28(5:1).
     MOVE     WK-DATE-MM        TO   MEI-F28(6:2).
     MOVE     "/"               TO   MEI-F28(8:1).
     MOVE     WK-DATE-DD        TO   MEI-F28(9:2).
*  受信日
     MOVE     MON-F46           TO   WK-DATE.
     MOVE     WK-DATE-YYYY      TO   MEI-F29(1:4).
     MOVE     "/"               TO   MEI-F29(5:1).
     MOVE     WK-DATE-MM        TO   MEI-F29(6:2).
     MOVE     "/"               TO   MEI-F29(8:1).
     MOVE     WK-DATE-DD        TO   MEI-F29(9:2).
*  商品カテゴリ
     MOVE     10                TO   JYO-F01.
     MOVE     MON-F70           TO   MEI-F30 JYO-F02.
*  商品カテゴリ名
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG = "INV"
              MOVE ALL NC"＊"   TO    MEI-F31
     ELSE
              MOVE JYO-F03      TO    MEI-F31
     END-IF.
*  分類区分
     MOVE     91                TO   JYO-F01.
     MOVE     MON-F71           TO   MEI-F32 JYO-F02.
*  分類区分名
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG = "INV"
              MOVE ALL NC"＊"   TO    MEI-F33
     ELSE
              MOVE JYO-F03      TO    MEI-F33
     END-IF.
*
*レコードセット
     MOVE     MEI-REC      TO   CSV-REC.
*
     WRITE    CSV-REC.
     ADD      1            TO   OUTPUT-CNT.
*    次レコード読込み
     PERFORM  CHUMONL1-RD-SEC.
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
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
*    MOVE     OUTPUT-CNT     TO   PARA-OUT-CNT.
*
     CLOSE    CHUMONL1 SUBMEIL1 TOKMS2 JYOKEN1
              ZSOKMS1 TENMS1 TANMS1 CHUMONDT.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.
