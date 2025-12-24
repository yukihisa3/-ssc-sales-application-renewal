# SJR0270V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJR0270V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受領返品　　　　　　              *
*    モジュール名　　　　：　共通受領書ＣＳＶデータ出力        *
*    作成日／作成者　　　：　2017/09/28 INOUE                  *
*    流用　　　　　　　　：　SSK0023V                          *
*    処理概要　　　　　　：　共通受領書データをＣＳＶ形式で　　*
*    　　　　　　　　　　　　出力する。　　　　　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SJR0270V.
 AUTHOR.                NAV-ASSIST.
 DATE-WRITTEN.          2017/09/28.
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
****<<受領書印刷ワーク >>*********************************
     SELECT   COMJRWK1           ASSIGN    TO   DA-01-VI-COMJRWK1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JYW-F03
                                                JYW-F07
                                                JYW-F08
                                                JYW-F51
                                                JYW-F04
                                                JYW-F31
                                 STATUS         JYW-STATUS.
*
****<<取引先マスタ　　　　　 >>*********************************
     SELECT   TOKMS2             ASSIGN    TO   DA-01-VI-TOKMS2
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TOK-F01
                                 STATUS         TOK-STATUS.
*
*****<<受領書CSVデータ       >>*******************************
     SELECT   COMJRCSV          ASSIGN    TO   COMJRCSV
                                STATUS         CSV-STATUS.
*                                                              *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = 受領書印刷ワーク                                   *
*--------------------------------------------------------------*
 FD  COMJRWK1           LABEL RECORD   IS   STANDARD.
     COPY     COMJRWK1  OF        XFDLIB
              JOINING   JYW       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 取引先マスタ　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     TOKMS2    OF        XFDLIB
              JOINING   TOK       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 受領データＣＳＶ　　　　　                   *
*--------------------------------------------------------------*
 FD  COMJRCSV           BLOCK CONTAINS 1    RECORDS.
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
 01  TOKMS2-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  RD1-FLG                      PIC       X(01)  VALUE  SPACE.
 01  RD2-FLG                      PIC       X(01)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  JYW-STATUS                   PIC       X(02).
 01  TOK-STATUS                   PIC       X(02).
 01  CSV-STATUS                   PIC       X(02).
*
 01  BRK-KEY.
     03  BRK-TENCD                PIC       9(05)  VALUE  ZERO.
     03  BRK-KDATE                PIC       9(08)  VALUE  ZERO.
     03  BRK-DENKU                PIC       X(02)  VALUE  SPACE.
     03  BRK-DENNO                PIC       9(10)  VALUE  ZERO.
*
     03  BRK-TENMEI               PIC      N(15).
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
     03  WK-KENSHUSU-DEN          PIC       9(07).
     03  WK-KINGAKU-DEN           PIC       9(12).
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  READ2-CNT                    PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
 01  IX1                          PIC       9(04)  VALUE  ZERO.
*タイトルエリア
*見出しエリア
 01  WK-HEAD1.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"＜受領書＞".
     03  C00           PIC N(04).
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"発行日：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  C02           PIC 9(08).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"発行時刻：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  C04           PIC 9(06).
 01  WK-HEAD3.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"日付区分：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  H103          PIC X(01).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  H104          PIC N(03).
     03  FILLER        PIC X(01)  VALUE  X"29".
 01  WK-HEAD4.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"受信日：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  H203          PIC 9(08)  VALUE  ZERO.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(01)  VALUE  NC"～".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  H205          PIC 9(08)  VALUE  ZERO.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"計上日：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  H207          PIC 9(08)  VALUE  ZERO.
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(01)  VALUE  NC"～".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  H209          PIC 9(08)  VALUE  ZERO.
     03  FILLER        PIC X(01)  VALUE  ",".
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
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"バッチ日付".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"時刻".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"取引先ＣＤ".
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
     03  FILLER        PIC N(05)  VALUE  NC"元伝票番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"相手伝区".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"店舗ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"計上日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"分類ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"訂区".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"自社伝区".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(01)  VALUE  NC"行".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"赤黒".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"相手商品ＣＤ".
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
     03  FILLER        PIC N(03)  VALUE  NC"原単価".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"原価金額".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"返品理由".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"訂区".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"自社商品ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".

*明細エリア
 01  WK-MEISAI1.
     03  MK101         PIC X(01).
     03  M102          PIC 9(08).
     03  MK102         PIC X(01).
     03  M103          PIC 9(04).
     03  MK103         PIC X(01).
     03  M104          PIC X(08).
     03  MK104         PIC X(01).
     03  MS105         PIC X(01).
     03  M105          PIC N(16).
     03  ME105         PIC X(01).
     03  MK105         PIC X(01).
     03  M106          PIC 9(10).
     03  MK106         PIC X(01).
     03  M107          PIC 9(10).
     03  MK107         PIC X(01).
     03  M108          PIC X(02).
     03  MK108         PIC X(01).
     03  M109          PIC 9(05).
     03  MK109         PIC X(01).
     03  M110          PIC 9(08).
     03  MK110         PIC X(01).
     03  M111          PIC X(10).
     03  MK111         PIC X(01).
     03  M112          PIC X(01).
     03  MK112         PIC X(01).
     03  M113          PIC X(02).
     03  MK113         PIC X(01).
     03  M114          PIC 9(02).
     03  MK114         PIC X(01).
     03  MS115         PIC X(01).
     03  M115          PIC N(01).
     03  ME115         PIC X(01).
     03  MK115         PIC X(01).
     03  M116          PIC X(14).
     03  MK116         PIC X(01).
     03  M117          PIC X(35).
     03  MK117         PIC X(01).
     03  M118          PIC X(35).
     03  MK118         PIC X(01).
     03  M119          PIC 9(06).
     03  MK119         PIC X(01).
     03  M120          PIC 9(08).
     03  MK120         PIC X(01).
     03  M121          PIC 9(09).
     03  MK121         PIC X(01).
     03  M122          PIC X(03).
     03  MK122         PIC X(01).
     03  M123          PIC X(03).
     03  MK123         PIC X(01).
     03  M124          PIC X(21).
     03  MK124         PIC X(01).
 01  WK-MEISAI2.
     03  GK00          PIC X(17).
     03  GS01          PIC X(01).
     03  G01           PIC N(02)  VALUE  NC"合計".
     03  GE01          PIC X(01).
     03  GK01          PIC X(01).
     03  G02           PIC 9(07).
     03  GK02          PIC X(02).
     03  G03           PIC 9(12).
     03  GK03          PIC X(01).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR0270V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0270V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SJR0270V".
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
 01  PARA-IN-DATEKBN        PIC   X(01).
 01  PARA-IN-FROMDATE       PIC   9(08).
 01  PARA-IN-TODATE         PIC   9(08).
 01  PARA-IN-HNPKBN         PIC   X(01).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-IN-DATEKBN
                                           PARA-IN-FROMDATE
                                           PARA-IN-TODATE
                                           PARA-IN-HNPKBN.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           COMJRWK1.
     MOVE     "COMJRWK1"          TO        ERR-FL-ID.
     MOVE     JYW-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
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
                        PROCEDURE COMJRCSV.
     MOVE     "COMJRCSV"          TO        ERR-FL-ID.
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
 SJR0270V-START         SECTION.
*
     MOVE   "SJR0270V-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     IF   END-FLG  NOT =  "END"
          PERFORM    MAIN-SEC  UNTIL  END-FLG   =  "END"
     END-IF.
     PERFORM            END-SEC.
     STOP               RUN.
*
 SJR0270V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     COMJRWK1.
     OPEN     INPUT     TOKMS2.
     OPEN     OUTPUT    COMJRCSV.
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
*    MOVE  SPACE                TO   JYW-REC.
*    INITIALIZE                      JYW-REC.
*
*    START  COMJRWK1  KEY  >=   JYW-F04  JYW-F05  JYW-F06
*        INVALID   KEY
*           MOVE     "END"      TO   END-FLG
*           DISPLAY NC"＃対象データ無し１＃" UPON CONS
*           DISPLAY "JYW-F04=" JYW-F04       UPON CONS
*           DISPLAY "JYW-F05=" JYW-F05       UPON CONS
*           DISPLAY "JYW-F06=" JYW-F06       UPON CONS
*           GO                  TO   INIT-EXIT
*    END-START
*
     PERFORM  COMJRWK1-RD-SEC.
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
     PERFORM   MIDASISET-SEC.
     MOVE     SPACE               TO   CSV-REC.
     MOVE     WK-HEAD1            TO   CSV-REC.
     WRITE    CSV-REC.
     MOVE     SPACE               TO   CSV-REC.
     MOVE     WK-HEAD3            TO   CSV-REC.
     WRITE    CSV-REC.
     MOVE     SPACE               TO   CSV-REC.
     MOVE     WK-HEAD4            TO   CSV-REC.
     WRITE    CSV-REC.
*    MOVE     WK-HEAD5            TO   CSV-REC.
*    WRITE    CSV-REC.
     MOVE     WK-HEAD6            TO   CSV-REC.
     WRITE    CSV-REC.
     ADD      4            TO   OUTPUT-CNT.
*　ブレイクキー設定
     MOVE  JYW-F07    TO       BRK-TENCD.
     MOVE  JYW-F08    TO       BRK-KDATE.
     MOVE  JYW-F51    TO       BRK-DENKU.
     MOVE  JYW-F04    TO       BRK-DENNO.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ケーヨー　受領累積データ読み込み　　　
****************************************************************
 COMJRWK1-RD-SEC            SECTION.
*
     MOVE    "COMJRWK1-RD-SEC"    TO   S-NAME.
*
     READ     COMJRWK1
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    COMJRWK1-RD-EXIT
     END-READ.
*
     ADD   1      TO  READ-CNT.
*
 COMJRWK1-RD-EXIT.
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
     MOVE   X"28"                 TO   MS105 MS115.
     MOVE   X"29"                 TO   ME105 ME115.
*
     MOVE   ","                   TO   MK101 MK102 MK103 MK104
                                       MK105 MK106 MK107 MK108
                                       MK109 MK110 MK111 MK112
                                       MK113 MK114 MK115 MK116
                                       MK117 MK118 MK119 MK120
                                       MK121 MK122 MK123 MK124.
*
*  ブレイク時
     IF      ( JYW-F07   NOT =   BRK-TENCD )  OR
             ( JYW-F08   NOT =   BRK-KDATE )  OR
             ( JYW-F51   NOT =   BRK-DENKU )  OR
             ( JYW-F04   NOT =   BRK-DENNO )
*        合計
              PERFORM   DENKEI-SEC
              MOVE   ZERO         TO    WK-DENKEI
              INITIALIZE                WK-DENKEI
              IF  ( JYW-F04   NOT =   BRK-TENCD )
                  MOVE  SPACE         TO    RD1-FLG
              END-IF
              MOVE  JYW-F07       TO    BRK-TENCD
              MOVE  JYW-F08       TO    BRK-KDATE
              MOVE  JYW-F51       TO    BRK-DENKU
              MOVE  JYW-F04       TO    BRK-DENNO
              MOVE  SPACE         TO    RD2-FLG
     END-IF.
*  明細行編集
*  バッチ日付
     MOVE     JYW-F01           TO    M102.
*  時刻
     MOVE     JYW-F02           TO    M103.
*  取引先ＣＤ
     MOVE     JYW-F03           TO    M104  TOK-F01.
*  取引先名
     PERFORM  TOKMS2-READ-SEC.
     IF       TOKMS2-INV-FLG = "INV"
              MOVE ALL NC"＊"   TO    M105
     ELSE
              MOVE TOK-F02      TO    M105
     END-IF.
*  伝票番号
     MOVE     JYW-F04           TO    M106.
*  元伝票番号
     MOVE     JYW-F05           TO    M107.
*  相手伝区
     MOVE     JYW-F06           TO    M108.
*  店舗ＣＤ
     MOVE     JYW-F07           TO    M109.
*  計上日
     MOVE     JYW-F08           TO    M110.
*  分類ＣＤ
     MOVE     JYW-F09           TO    M111.
*  訂正区分Ｈ
     MOVE     JYW-F10           TO    M112.
*  自社伝区
     MOVE     JYW-F51           TO    M113.
*  行
     MOVE     JYW-F31           TO    M114.
*  赤黒
     IF       JYW-F32    =      ZERO
              MOVE   NC"黒"     TO    M115
     ELSE
              MOVE   NC"赤"     TO    M115
     END-IF.
*  相手商品ＣＤ
     MOVE     JYW-F33           TO    M116.
*  商品名１・２
     MOVE     JYW-F37           TO    M117.
     MOVE     JYW-F38           TO    M118.
*  数量　
     MOVE     JYW-F34           TO    M119.
*  原単価
     MOVE     JYW-F35           TO    M120.
*  原価金額
     MOVE     JYW-F36           TO    M121.
*  返品理由
     MOVE     JYW-F39           TO    M122.
*  訂正区分Ｍ
     MOVE     JYW-F40           TO    M123.
*  自社商品ＣＤ
     MOVE     "("               TO    M124(1:1).
     MOVE     JYW-F52           TO    M124(2:8).
*  品単１
     MOVE     "-"               TO    M124(10:1).
     MOVE     JYW-F53           TO    M124(11:5).
*  品単２
     MOVE     "-"               TO    M124(16:1).
     MOVE     JYW-F54           TO    M124(17:2).
*  品単３
     MOVE     "-"               TO    M124(19:1).
     MOVE     JYW-F55           TO    M124(20:1).
     MOVE     ")"               TO    M124(21:1).
*--------------
*  伝票計加算
*--------------
*  検収数
     COMPUTE  WK-KENSHUSU-DEN = WK-KENSHUSU-DEN +  JYW-F34.
*  原価金額
     COMPUTE  WK-KINGAKU-DEN  = WK-KINGAKU-DEN  +  JYW-F36.
*レコードセット
     MOVE     WK-MEISAI1   TO   CSV-REC.
*    MOVE     WK-DENMEISAI TO   CSV-DENNO.
*
     WRITE    CSV-REC.
     ADD      1            TO   OUTPUT-CNT.
*    次レコード読込み
     PERFORM  COMJRWK1-RD-SEC.
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
*返品区分
     EVALUATE PARA-IN-HNPKBN
          WHEN   " "
              MOVE     NC"（全て）"       TO   C00
          WHEN   "1"
              MOVE     NC"（返品）"       TO   C00
          WHEN   "2"
              MOVE     NC"　　　　"       TO   C00
     END-EVALUATE.
*
*日付区分・日付
     MOVE     PARA-IN-DATEKBN   TO   H103.
     EVALUATE PARA-IN-DATEKBN
          WHEN   " "
              MOVE     NC"受信日："       TO   H104
              MOVE     PARA-IN-FROMDATE   TO   H203
              MOVE     PARA-IN-TODATE     TO   H205
          WHEN   "1"
              MOVE     NC"計上日："       TO   H104
              MOVE     PARA-IN-FROMDATE   TO   H207
              MOVE     PARA-IN-TODATE     TO   H209
     END-EVALUATE.
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
     MOVE     X"28"               TO   GS01.
     MOVE     X"29"               TO   GE01.
     MOVE     ","                 TO   GK01  GK03.
     MOVE     ALL ","             TO   GK00  GK02.
*--------------
*  合計転送
*--------------
*  タイトル転送
*    MOVE    WK-DENMEISAI        TO   CSV-DENNO.
*  タイトル転送
*    MOVE     NC"＜合計＞"       TO   GK01-TAITOL.
*  数量
     MOVE     WK-KENSHUSU-DEN    TO   G02.
*  原価金額
     MOVE     WK-KINGAKU-DEN     TO   G03.
*
*伝票合計出力
*レコードセット
     MOVE     WK-MEISAI2         TO   CSV-REC.
*
     WRITE    CSV-REC.
     ADD      1                  TO   OUTPUT-CNT.
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
     CLOSE    COMJRWK1 TOKMS2 COMJRCSV.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.

```
