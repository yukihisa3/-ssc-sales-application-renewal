# SSI7302B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSI7302B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ダイユーエイト新ＥＤＩシステム　　*
*    業務名　　　　　　　：　支払ＣＳＶデータ作成              *
*    モジュール名　　　　：　支払ＣＳＶデータ作成              *
*    作成日／更新日　　　：　10/07/02                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　ダイユーエイト支払データを支払ＣＳ*
*    　　　　　　　　　　　　Ｖデータに出力する。　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSI7302B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/07/02.
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
****<<ダイユーエイト支払データ>>********************************
     SELECT   DYSIHF            ASSIGN    TO   DA-01-VI-DYSIHL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    DYS-F00  DYS-F01
                                                DYS-F02  DYS-F03
                                                DYS-F04  DYS-F05
                                                DYS-F06
                                 STATUS         DYS-STATUS.
*
*****<<  支払ﾃﾞｰﾀCSV   >>**************************************
     SELECT   DYSIHDT           ASSIGN    TO   DYSIHDT
                                 STATUS         DSD-STATUS.
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ダイユーエイト支払データ                           *
*--------------------------------------------------------------*
 FD  DYSIHF            LABEL RECORD   IS   STANDARD.
     COPY     DYSIHF   OF        XFDLIB
              JOINING   DYS       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ダイユーエイト　支払ＣＳＶデータ                   *
*--------------------------------------------------------------*
 FD  DYSIHDT           BLOCK CONTAINS 1    RECORDS.
 01  DSD-REC.
     03  FILLER         PIC       X(500).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*ヘッダ情報格納領域
*    COPY   DYSIHDT OF XFDLIB  JOINING   DJR  AS   PREFIX.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  CHK-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  DYS-STATUS                   PIC       X(02).
 01  DSD-STATUS                   PIC       X(02).
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
     03  FILLER        PIC N(03)  VALUE  NC"支払月".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"店舗".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(02)  VALUE  "CD".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"伝票日付".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"部門".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"伝票番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"伝票合計金額".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"相殺コード".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"相殺名称".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"支払区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"支払期間Ｓ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"支払期間Ｅ".
     03  FILLER        PIC X(01)  VALUE  X"29".
*明細エリア
 01  WK-MEISAI.
     03  DSD-F01       PIC 9(08).
     03  DSD-A01       PIC X(01).
     03  DSD-F02       PIC 9(06).
     03  DSD-A02       PIC X(01).
     03  DSD-F03       PIC 9(05).
     03  DSD-A03       PIC X(01).
     03  DSD-F04       PIC 9(08).
     03  DSD-A04       PIC X(01).
     03  DSD-F05       PIC X(04).
     03  DSD-A05       PIC X(01).
     03  DSD-F06       PIC 9(09).
     03  DSD-A06       PIC X(01).
     03  DSD-F07       PIC -99999999999.
     03  DSD-A07       PIC X(01).
     03  DSD-F08       PIC X(04).
     03  DSD-A08       PIC X(01).
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  DSD-F09       PIC N(15).
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  DSD-A09       PIC X(01).
     03  DSD-F10       PIC X(01).
     03  DSD-A10       PIC X(01).
     03  DSD-F11       PIC 9(08).
     03  DSD-A11       PIC X(01).
     03  DSD-F12       PIC 9(08).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSI7302B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSI7302B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSI7302B".
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
                        PROCEDURE DYSIHF.
     MOVE     "DYSIHF "          TO        ERR-FL-ID.
     MOVE     DYS-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE DYSIHDT.
     MOVE     "DYSIHDT"          TO        ERR-FL-ID.
     MOVE     DSD-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSI7302B-START         SECTION.
*
     MOVE   "SSI7302B-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSI7302B-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     DYSIHF.
     OPEN     OUTPUT    DYSIHDT.
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
*
     PERFORM  DYSIHF-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     ELSE
              MOVE   SPACE         TO        DSD-REC
*             INITIALIZE                     DSD-REC
              MOVE   WK-TAITL      TO        DSD-REC
              WRITE  DSD-REC
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ホーマック物品受領書データ読込み
****************************************************************
 DYSIHF-RD-SEC             SECTION.
*
     MOVE    "DYSIHF-RD-SEC"    TO   S-NAME.
*
     READ     DYSIHF
          AT END
              MOVE     "END"      TO        END-FLG
          NOT  AT  END
              ADD       1         TO        READ-CNT
     END-READ.
*
 DYSIHF-RD-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*初期化
*    MOVE     SPACE               TO   DJR-REC.
*    INITIALIZE                        DJR-REC.
*項目転送
     MOVE     ","                 TO   DSD-A01 DSD-A02 DSD-A03.
     MOVE     ","                 TO   DSD-A04 DSD-A05 DSD-A06.
     MOVE     ","                 TO   DSD-A07 DSD-A08 DSD-A09.
     MOVE     ","                 TO   DSD-A10 DSD-A11.
*
     MOVE     DYS-F00             TO   DSD-F01.
     MOVE     DYS-F01             TO   DSD-F02.
     MOVE     DYS-F02             TO   DSD-F03.
     MOVE     DYS-F03             TO   DSD-F04.
     MOVE     DYS-F05             TO   DSD-F05.
     MOVE     DYS-F06             TO   DSD-F06.
     MOVE     DYS-F07             TO   DSD-F07.
     MOVE     DYS-F08             TO   DSD-F08.
     MOVE     DYS-F09             TO   DSD-F09.
     MOVE     DYS-F10             TO   DSD-F10.
     MOVE     DYS-F11             TO   DSD-F11.
     MOVE     DYS-F12             TO   DSD-F12.
     MOVE     WK-MEISAI           TO   DSD-REC.
     WRITE    DSD-REC.
     ADD      1                   TO   OUTPUT-CNT.
*    次レコード読込み
     PERFORM  DYSIHF-RD-SEC.
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
     CLOSE    DYSIHF   DYSIHDT.
     DISPLAY  MSG-END  UPON  CONS.
*
 END-EXIT.
     EXIT.
********************<<  PUROGRAM  END  >>*************************

```
