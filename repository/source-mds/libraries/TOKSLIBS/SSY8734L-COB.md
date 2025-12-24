# SSY8734L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY8734L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＤＣＭＪＡＰＡＮ物品受領書　　　　*
*    業務名　　　　　　　：　物品受領書                        *
*    モジュール名　　　　：　物品受領書                        *
*    作成日／更新日　　　：　07/05/29                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　ＤＣＭＪＡＰＡＮ物品受領書を　　  *
*    　　　　　　　　　　　　発行する。　　　　　　　　　　　  *
*    作成日／更新日　　　：　19/09/17                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　消費税軽減税率対応　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY8734L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/29.
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
****<<DCMJAPAN物品受領データ >>*********************************
     SELECT   DCMJYRF            ASSIGN    TO   DA-01-VI-DCMJYRL2
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JYU-F00  JYU-F01
                                                JYU-F04  JYU-F02
                                                JYU-F03  JYU-F06
                                                JYU-F07
                                 STATUS         JYU-STATUS.
*
*****<<  受領ﾃﾞｰﾀCSV   >>**************************************
     SELECT   DCMJYRDT           ASSIGN    TO   DCMJYRDT
                                 STATUS         DCM-STATUS.
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ＤＣＭＪＡＰＡＮ　物品受領書                       *
*--------------------------------------------------------------*
 FD  DCMJYRF            LABEL RECORD   IS   STANDARD.
     COPY     DCMJYRF   OF        XFDLIB
              JOINING   JYU       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ＤＣＭＪＡＰＡＮ　物品ＣＳＶデータ                 *
*--------------------------------------------------------------*
 FD  DCMJYRDT           BLOCK CONTAINS 1    RECORDS.
 01  DCM-REC.
     03  FILLER         PIC       X(500).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*ヘッダ情報格納領域
     COPY   DCMJYRDT OF XFDLIB  JOINING   DJR  AS   PREFIX.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  CHK-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  JYU-STATUS                   PIC       X(02).
 01  DCM-STATUS                   PIC       X(02).
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
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
*タイトルエリア
 01  WK-TAITL.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"取引先".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(02)  VALUE  "CD".
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
     03  FILLER        PIC N(02)  VALUE  NC"店舗".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(02)  VALUE  "CD".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"店舗名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"伝票".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(02)  VALUE  "NO".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(01)  VALUE  NC"行".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"伝区".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"商品".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(02)  VALUE  "CD".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"ＪＡＮＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"商品名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"発注数".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"検収数".
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
     03  FILLER        PIC N(05)  VALUE  NC"納品書番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
*#2018/04/06 NAV ST
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"処理種別".
     03  FILLER        PIC X(01)  VALUE  X"29".
*#2018/04/06 NAV ED
*#2019/09/17 NAV ST 消費税軽減税率対応
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"税率".
     03  FILLER        PIC X(01)  VALUE  X"29".
*#2019/09/17 NAV ED 消費税軽減税率対応
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY8734L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8734L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY8734L".
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
 01  PARA-TOKCD             PIC   9(08).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-TOKCD.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE DCMJYRF.
     MOVE     "DCMJYRF "          TO        ERR-FL-ID.
     MOVE     JYU-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE DCMJYRDT.
     MOVE     "DCMJYRDT"          TO        ERR-FL-ID.
     MOVE     DCM-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY8734L-START         SECTION.
*
     MOVE   "SSY8734L-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSY8734L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     DCMJYRF.
     OPEN     OUTPUT    DCMJYRDT.
     DISPLAY  MSG-START UPON CONS.
*
     DISPLAY "PARA-TOKCD = " PARA-TOKCD UPON CONS.
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
     PERFORM  DCMJYRF-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     ELSE
              MOVE   SPACE         TO        DCM-REC
              INITIALIZE                     DCM-REC
              MOVE   WK-TAITL      TO        DCM-REC
              WRITE  DCM-REC
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ホーマック物品受領書データ読込み
****************************************************************
 DCMJYRF-RD-SEC             SECTION.
*
     MOVE    "DCMJYRF-RD-SEC"    TO   S-NAME.
*
     READ     DCMJYRF
          AT END
              MOVE     "END"      TO        END-FLG
          NOT  AT  END
              ADD       1         TO        READ-CNT
     END-READ.
*
 DCMJYRF-RD-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*初期化
     MOVE     SPACE               TO   DJR-REC.
     INITIALIZE                        DJR-REC.
*項目転送
     MOVE     ","                 TO   DJR-A01 DJR-A02 DJR-A03.
     MOVE     ","                 TO   DJR-A04 DJR-A05 DJR-A06.
     MOVE     ","                 TO   DJR-A07 DJR-A08 DJR-A09.
     MOVE     ","                 TO   DJR-A10 DJR-A11 DJR-A12.
     MOVE     ","                 TO   DJR-A13 DJR-A14 DJR-A15.
*#2019/09/17 NAV ST 消費税軽減税率対応
     MOVE     ","                 TO   DJR-A16 DJR-A17.
*#2019/09/17 NAV ED 消費税軽減税率対応
*
     MOVE     JYU-F00             TO   DJR-F01.
     MOVE     JYU-F02             TO   DJR-F02.
     MOVE     JYU-F03             TO   DJR-F03.
     MOVE     JYU-F04             TO   DJR-F04.
     MOVE     JYU-F05             TO   DJR-F05.
     MOVE     JYU-F06             TO   DJR-F06.
     MOVE     JYU-F07             TO   DJR-F07.
     MOVE     JYU-F08             TO   DJR-F08.
     MOVE     JYU-F09             TO   DJR-F09.
     MOVE     JYU-F15             TO   DJR-F10.
     MOVE     JYU-F10             TO   DJR-F11.
     MOVE     JYU-F11             TO   DJR-F12.
     MOVE     JYU-F12             TO   DJR-F13.
     MOVE     JYU-F13             TO   DJR-F14.
     MOVE     JYU-F14             TO   DJR-F15
     MOVE     JYU-F16             TO   DJR-F16
*#2018/04/06 NAV ST
     MOVE     JYU-F17             TO   DJR-F17
*#2018/04/06 NAV ED
*#2019/09/17 NAV ST 消費税軽減税率対応
     COMPUTE  DJR-F18  =  JYU-F18  /  1000.
*#2019/09/17 NAV ED 消費税軽減税率対応
     MOVE     DJR-REC             TO   DCM-REC.
     WRITE    DCM-REC.
     ADD      1                   TO   OUTPUT-CNT.
*    次レコード読込み
     PERFORM  DCMJYRF-RD-SEC.
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
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
*
     CLOSE    DCMJYRF   DCMJYRDT.
*
 END-EXIT.
     EXIT.
********************<<  PUROGRAM  END  >>*************************

```
