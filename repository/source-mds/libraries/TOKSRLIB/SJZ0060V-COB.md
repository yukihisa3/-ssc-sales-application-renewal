# SJZ0060V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJZ0060V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受注状況処理会機能　　　　　　　　*
*    業務名　　　　　　　：　受注状況データ　　　　　          *
*    モジュール名　　　　：　受注状況データＣＳＶ出力　　　　　*
*    作成日／更新日　　　：  18/04/17                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
**履歴**********************************************************
*    2018/04/17  高橋　　新規作成　　　　　　　　　　　　　　　*
*    0000/00/00　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SJZ0060V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2018/04/17.
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
****<<受注残ファイル（Ｌ３）>>****************************
     SELECT   JYUZANL3           ASSIGN    TO   DA-01-VI-JYUZANL3
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    ZAN3-F13
                                                ZAN3-F14
                                                ZAN3-F15
                                                ZAN3-F16
                                                ZAN3-F09
                                                ZAN3-F03
                                                ZAN3-F17
                                                ZAN3-F11
                                 STATUS         ZAN3-STATUS.
****<<受注残ファイル（Ｌ４）>>****************************
     SELECT   JYUZANL4           ASSIGN    TO   DA-01-VI-JYUZANL4
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    ZAN4-F09
                                                ZAN4-F13
                                                ZAN4-F14
                                                ZAN4-F15
                                                ZAN4-F16
                                                ZAN4-F17
                                                ZAN4-F11
                                 STATUS         ZAN4-STATUS.
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

****<<取引先マスタ　　　　　 >>*********************************
     SELECT   TOKMS2             ASSIGN    TO   DA-01-VI-TOKMS2
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TOK-F01
                                 STATUS         TOK-STATUS.
*****<<受注残ＣＳＶデータ>>***********************************
     SELECT   JYUZANWK           ASSIGN    TO   JYUZANWK
                                 STATUS         CSV-STATUS.
*                                                              *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*--------------------------------------------------------------*
*    FILE = 受注残ファイル（ＬＦ３）　                         *
*--------------------------------------------------------------*
 FD  JYUZANL3           LABEL RECORD   IS   STANDARD.
     COPY     JYUZANF   OF        XFDLIB
              JOINING   ZAN3      PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 受注残ファイル（ＬＦ４）　                         *
*--------------------------------------------------------------*
 FD  JYUZANL4           LABEL RECORD   IS   STANDARD.
     COPY     JYUZANF   OF        XFDLIB
              JOINING   ZAN4      PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 倉庫マスタ　　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  ZSOKMS1             LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1    OF        XFDLIB
              JOINING   SOK       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 商品名称マスタ　　　　　　                       *
*--------------------------------------------------------------*
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     MEIMS1    OF        XFDLIB
              JOINING   MEI       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 取引先マスタ　　　　　　　　　                     *
*--------------------------------------------------------------*
 FD  TOKMS2              LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS     OF        XFDLIB
              JOINING   TOK       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 受注残ＣＳＶデータ　　　　　　　                   *
*--------------------------------------------------------------*
 FD  JYUZANWK           BLOCK CONTAINS 1    RECORDS.
 01  CSV-REC.
     03  FILLER         PIC       X(2000).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*受注残ＣＳＶレコード
     COPY   JYUZANWK OF XFDLIB  JOINING   ZWK  AS   PREFIX.
*受注残ファイル退避
     COPY   JYUZANF  OF XFDLIB  JOINING   ZAN  AS   PREFIX.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  END-FLG2                     PIC       X(03)  VALUE  SPACE.
 01  SET-FLG                      PIC       X(03)  VALUE  SPACE.
 01  ZSOKMS1-INV-FLG              PIC       X(03)  VALUE  SPACE.
 01  MEIMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  TOKMS2-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  RD1-FLG                      PIC       X(01)  VALUE  SPACE.
 01  RD2-FLG                      PIC       X(01)  VALUE  SPACE.
 01  READ-FLG                     PIC       X(01)  VALUE  SPACE.
 01  RD3-CNT                      PIC       9(07)  VALUE  ZERO.
 01  RD4-CNT                      PIC       9(07)  VALUE  ZERO.
*
**** ステイタス　エリア
 01  ZAN3-STATUS                  PIC       X(02).
 01  ZAN4-STATUS                  PIC       X(02).
 01  SOK-STATUS                   PIC       X(02).
 01  MEI-STATUS                   PIC       X(02).
 01  TOK-STATUS                   PIC       X(02).
 01  CSV-STATUS                   PIC       X(02).
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
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  READ2-CNT                    PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
*タイトルエリア
*見出しエリア
 01  WK-HEAD1.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(08)  VALUE
                       NC"＜受注残データ＞".
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
 01  WK-HEAD2.
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
     03  FILLER        PIC N(03)  VALUE  NC"行番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"伝票区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"出荷倉庫".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"出荷倉庫名".
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
     03  FILLER        PIC N(02)  VALUE  NC"_番".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"受注数".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"出荷数".
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
     03  FILLER        PIC N(07)  VALUE  NC"売価金額".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"相手商品ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"在庫引当区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"オンライン区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"受信日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"受信時刻".
     03  FILLER        PIC X(01)  VALUE  X"29".

*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJZ0060V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJZ0060V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SJZ0060V".
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
 01  PARA-SOKCD             PIC   X(02).
 01  PARA-SYOCD1            PIC   X(08).
 01  PARA-SYOCD2            PIC   X(08).
 01  PARA-TOKCD             PIC   9(08).
*
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION USING PARA-SOKCD
                                       PARA-SYOCD1
                                       PARA-SYOCD2
                                       PARA-TOKCD.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           JYUZANL3.
     MOVE     "JYUZANL3"          TO        ERR-FL-ID.
     MOVE     ZAN3-STATUS         TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           JYUZANL4.
     MOVE     "JYUZANL4"          TO        ERR-FL-ID.
     MOVE     ZAN4-STATUS         TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
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
 FILEERROR-SEC4         SECTION.
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
 FILEERROR-SEC5         SECTION.
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
 FILEERROR-SEC6         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE JYUZANWK.
     MOVE     "JYUZANWK"          TO        ERR-FL-ID.
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
 SJZ0060V-START         SECTION.
*
     MOVE   "SJZ0060V-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     IF   END-FLG  NOT =  "END"
          PERFORM    MAIN-SEC  UNTIL  END-FLG   =  "END"
     END-IF.
     PERFORM            END-SEC.
     STOP               RUN.
*
 SJZ0060V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
*読込ファイル判定
     IF   PARA-SOKCD  =  SPACE
          MOVE "1"                TO   READ-FLG
          DISPLAY "READ-KEY = L3" UPON CONS
     ELSE
          MOVE "2"                TO   READ-FLG
          DISPLAY "READ-KEY = L4" UPON CONS
     END-IF.
*
     IF  READ-FLG = "1"
         OPEN     INPUT     JYUZANL3
     END-IF.
     IF  READ-FLG = "2"
         OPEN     INPUT     JYUZANL4
     END-IF.
     OPEN     INPUT     ZSOKMS1.
     OPEN     INPUT     MEIMS1.
     OPEN     INPUT     TOKMS2.
     OPEN     OUTPUT    JYUZANWK.
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
     MOVE  SPACE                TO   ZAN3-REC ZAN4-REC.
     INITIALIZE                      ZAN3-REC ZAN4-REC.

     EVALUATE READ-FLG
         WHEN   "1"
             MOVE       PARA-SYOCD1         TO   ZAN3-F13
*
             START      JYUZANL3  KEY  >=        ZAN3-F13
                                                 ZAN3-F14
                                                 ZAN3-F15
                                                 ZAN3-F16
                                                 ZAN3-F09
                                                 ZAN3-F03
                                                 ZAN3-F17
                                                 ZAN3-F11
                INVALID KEY
                        MOVE     "END"      TO   END-FLG
                        DISPLAY NC"＃＃対象データなし！＃＃"
                                                 UPON CONS
                        GO                  TO   INIT-EXIT
             END-START
*
         WHEN   "2"
             MOVE       PARA-SOKCD          TO   ZAN4-F09
             MOVE       PARA-SYOCD1         TO   ZAN4-F13
*
             START      JYUZANL4  KEY  >=        ZAN4-F09
                                                 ZAN4-F13
                                                 ZAN4-F14
                                                 ZAN4-F15
                                                 ZAN4-F16
                                                 ZAN4-F17
                                                 ZAN4-F11
                INVALID KEY
                        MOVE     "END"      TO   END-FLG
                        DISPLAY NC"＃＃対象データなし！＃＃"
                                                 UPON CONS
                        GO                  TO   INIT-EXIT
             END-START
     END-EVALUATE.
 INIT-010.
*
     EVALUATE READ-FLG
         WHEN   "1"
             PERFORM  JYUZANL3-RD-SEC
             IF       END-FLG  = "END"
                      DISPLAY NC"＃＃対象データなし！＃＃"
                                                  UPON CONS
                      GO                  TO   INIT-EXIT
             END-IF
         WHEN   "2"
             PERFORM  JYUZANL4-RD-SEC
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
     ADD      2                   TO   OUTPUT-CNT.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    受注残ファイル（Ｌ３）読込
****************************************************************
 JYUZANL3-RD-SEC            SECTION.
*
     MOVE    "JYUZANL3-RD-SEC"    TO   S-NAME.
*
     READ     JYUZANL3
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    JYUZANL3-RD-EXIT
          NOT AT END
              ADD       1         TO   RD3-CNT
     END-READ.
*件数表示
     IF  RD3-CNT(5:3)  =  "000" OR "500"
         DISPLAY "JYUZANL3-READ-CNT = " RD3-CNT UPON CONS
     END-IF.
*サカタ商品ＣＤチェック
     IF       ZAN3-F13  >  PARA-SYOCD2
              MOVE     "END"      TO   END-FLG
              GO     TO    JYUZANL3-RD-EXIT
     END-IF.
*取引先ＣＤチェック
     IF       PARA-TOKCD  NOT =  ZERO
              IF   PARA-TOKCD  =  ZAN3-F03
                   CONTINUE
              ELSE
                   GO     TO      JYUZANL3-RD-SEC
              END-IF
     END-IF.
*受注残ファイル退避
     MOVE     ZAN3-REC    TO      ZAN-REC.
*
 JYUZANL3-RD-EXIT.
     EXIT.
****************************************************************
*    受注残ファイル（Ｌ４）読込
****************************************************************
 JYUZANL4-RD-SEC            SECTION.
*
     MOVE    "JYUZANL4-RD-SEC"    TO   S-NAME.
*
     READ     JYUZANL4
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    JYUZANL4-RD-EXIT
          NOT AT END
              ADD       1         TO   RD4-CNT
     END-READ.
*件数表示
     IF  RD4-CNT(5:3)  =  "000" OR "500"
         DISPLAY "JYUZANL4-READ-CNT = " RD4-CNT UPON CONS
     END-IF.
*倉庫ＣＤチェック
     IF       ZAN4-F09  =  PARA-SOKCD
              CONTINUE
     ELSE
              MOVE     "END"      TO   END-FLG
              GO     TO    JYUZANL4-RD-EXIT
     END-IF.
*サカタ商品ＣＤチェック
     IF       ZAN4-F13  >  PARA-SYOCD2
              MOVE     "END"      TO   END-FLG
              GO     TO    JYUZANL4-RD-EXIT
     END-IF.
*取引先ＣＤチェック
     IF       PARA-TOKCD  NOT =  ZERO
              IF   PARA-TOKCD  =  ZAN4-F03
                   CONTINUE
              ELSE
                   GO     TO      JYUZANL4-RD-SEC
              END-IF
     END-IF.
*受注残ファイル退避
     MOVE     ZAN4-REC    TO      ZAN-REC.
*
 JYUZANL4-RD-EXIT.
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
     MOVE     SPACE               TO   ZWK-REC.
*項目転送
     MOVE   X"28"                 TO   ZWK-S11  ZWK-S21
                                       ZWK-S31.
     MOVE   X"29"                 TO   ZWK-S12  ZWK-S22
                                       ZWK-S32.
*
     MOVE     ","                 TO   ZWK-K01 ZWK-K02
                                       ZWK-K03 ZWK-K04 ZWK-K05
                                       ZWK-K06 ZWK-K07 ZWK-K08
                                       ZWK-K09 ZWK-K10 ZWK-K11
                                       ZWK-K12 ZWK-K13 ZWK-K14
                                       ZWK-K15 ZWK-K16 ZWK-K17
                                       ZWK-K18 ZWK-K19 ZWK-K20
                                       ZWK-K21 ZWK-K22 ZWK-K23
                                       ZWK-K24 ZWK-K25.
*　取引先ＣＤ
     MOVE     ZAN-F03             TO    ZWK-F01.
*  取引先名
     MOVE     ZAN-F03             TO    TOK-F01.
     PERFORM  TOKMS2-READ-SEC
     IF       TOKMS2-INV-FLG = "INV"
              MOVE  ALL NC"＊"    TO    ZWK-F02
     ELSE
              MOVE  TOK-F02       TO    ZWK-F02
     END-IF.
*  伝票番号
     MOVE     ZAN-F04             TO    ZWK-F03.
*  行番号
     MOVE     ZAN-F05             TO    ZWK-F04.
*  伝票区分
     MOVE     ZAN-F07             TO    ZWK-F05.
*  出荷倉庫
     MOVE     ZAN-F09             TO    ZWK-F06.
*  出荷倉庫名
     MOVE     ZAN-F09             TO    SOK-F01.
     PERFORM  ZSOKMS1-READ-SEC
     IF       ZSOKMS1-INV-FLG = "INV"
              MOVE  ALL NC"＊"    TO    ZWK-F07
     ELSE
              MOVE  SOK-F02       TO    ZWK-F07
     END-IF.
*  発注日
     MOVE     ZAN-F10             TO    ZWK-F08.
*  納品日
     MOVE     ZAN-F11             TO    ZWK-F09.
*  サカタ商品ＣＤ
     MOVE     ZAN-F13             TO    ZWK-F10.
*  品単１
     MOVE     ZAN-F14             TO    ZWK-F11.
*  品単２
     MOVE     ZAN-F15             TO    ZWK-F12.
*  品単３
     MOVE     ZAN-F16             TO    ZWK-F13.
*  商品名
     MOVE     ZAN-F13             TO    MEI-F011.
     MOVE     ZAN-F14             TO    MEI-F0121.
     MOVE     ZAN-F15             TO    MEI-F0122.
     MOVE     ZAN-F16             TO    MEI-F0123.
     PERFORM  MEIMS1-READ-SEC.
     IF   MEIMS1-INV-FLG = "INV"
          MOVE  ALL NC"＊"     TO       ZWK-F14
     ELSE
          MOVE  MEI-F021       TO       ZWK-F14(1:15)
          MOVE  MEI-F022       TO       ZWK-F14(16:15)
     END-IF.
*  _番
     MOVE     ZAN-F17             TO    ZWK-F15.
*  受注数量
     MOVE     ZAN-F18             TO    ZWK-F16.
*  出荷数量
     MOVE     ZAN-F19             TO    ZWK-F17.
*  原価単価
     MOVE     ZAN-F20             TO    ZWK-F18.
*  売価単価
     MOVE     ZAN-F21             TO    ZWK-F19.
*  原価金額
     MOVE     ZAN-F22             TO    ZWK-F20.
*  売価金額
     MOVE     ZAN-F23             TO    ZWK-F21.
*  相手商品
     MOVE     ZAN-F24             TO    ZWK-F22.
*  在庫引当区分
     MOVE     ZAN-F25             TO    ZWK-F23.
*  オンライン区分
     MOVE     ZAN-F26             TO    ZWK-F24.
*  受信日
     MOVE     ZAN-F01             TO    ZWK-F25.
*  受信時刻
     MOVE     ZAN-F02             TO    ZWK-F26.
*
     MOVE     ZWK-REC             TO    CSV-REC.
     WRITE    CSV-REC.
     ADD      1            TO   OUTPUT-CNT.
*    次レコード読込み
     EVALUATE READ-FLG
         WHEN   "1"
             PERFORM  JYUZANL3-RD-SEC
         WHEN   "2"
             PERFORM  JYUZANL4-RD-SEC
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
*
 MIDASISET-EXIT.
     EXIT.
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     EVALUATE READ-FLG
         WHEN "1"
              CLOSE  JYUZANL3
              MOVE  RD3-CNT       TO  READ-CNT
         WHEN "2"
              CLOSE  JYUZANL4
              MOVE  RD4-CNT       TO  READ-CNT
     END-EVALUATE.
*
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
*
     CLOSE    ZSOKMS1 MEIMS1 TOKMS2 JYUZANWK.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.

```
