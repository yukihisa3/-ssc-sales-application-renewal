****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ダイユーエイト新ＥＤＩシステム　　*
*    業務名　　　　　　　：　受領ＣＳＶデータ作成              *
*    モジュール名　　　　：　受領ＣＳＶデータ作成              *
*    作成日／更新日　　　：　10/07/02                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　ダイユーエイト受領データを受領ＣＳ*
*    　　　　　　　　　　　　Ｖデータに出力する。　　　　　　  *
*    　　　　　　　　　　　　2019/05/24 NAV TAKAHASHI          *
*    　　　　　　　　　　　　　数量／単価編集　　　　　        *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY7315B.
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
****<<ダイユーエイト受領データ>>********************************
     SELECT   DYJRYOF            ASSIGN    TO   DA-01-VI-DYJRYOL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    DYJ-F00  DYJ-F01
                                                DYJ-F02  DYJ-F03
                                                DYJ-F04  DYJ-F05
                                                DYJ-F06  DYJ-F07
                                                DYJ-F08  DYJ-F09
                                 STATUS         DYJ-STATUS.
*
*****<<  受領ﾃﾞｰﾀCSV   >>**************************************
     SELECT   DYJRYDT           ASSIGN    TO   DYJRYDT
                                 STATUS         DJD-STATUS.
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ダイユーエイト受領データ                           *
*--------------------------------------------------------------*
 FD  DYJRYOF            LABEL RECORD   IS   STANDARD.
     COPY     DYJRYOF   OF        XFDLIB
              JOINING   DYJ       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ダイユーエイト　受領ＣＳＶデータ                   *
*--------------------------------------------------------------*
 FD  DYJRYDT           BLOCK CONTAINS 1    RECORDS.
 01  DJD-REC.
     03  FILLER         PIC       X(500).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*ヘッダ情報格納領域
*    COPY   DYJRYDT OF XFDLIB  JOINING   DJR  AS   PREFIX.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  CHK-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  DYJ-STATUS                   PIC       X(02).
 01  DJD-STATUS                   PIC       X(02).
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
     03  FILLER        PIC N(02)  VALUE  NC"商品".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(02)  VALUE  "CD".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"ＪＡＮＣＤ".
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
     03  FILLER        PIC N(04)  VALUE  NC"納品番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
*明細エリア
 01  WK-MEISAI.
     03  DJD-F01       PIC 9(08).
     03  DJD-A01       PIC X(01).
     03  DJD-F02       PIC 9(08).
     03  DJD-A02       PIC X(01).
     03  DJD-F03       PIC 9(08).
     03  DJD-A03       PIC X(01).
     03  DJD-F04       PIC 9(04).
     03  DJD-A04       PIC X(01).
     03  DJD-F05       PIC X(15).
     03  DJD-A05       PIC X(01).
     03  DJD-F06       PIC 9(09).
     03  DJD-A06       PIC X(01).
     03  DJD-F07       PIC 9(02).
     03  DJD-A07       PIC X(01).
     03  DJD-F08       PIC 9(02).
     03  DJD-A08       PIC X(01).
*****03  DJD-F09       PIC 9(14).
     03  DJD-F09       PIC X(14).
     03  DJD-A09       PIC X(01).
     03  DJD-F10       PIC X(13).
     03  DJD-A10       PIC X(01).
     03  DJD-F111      PIC X(15).
     03  DJD-A111      PIC X(01).
     03  DJD-F112      PIC X(15).
     03  DJD-A112      PIC X(01).
     03  DJD-F12       PIC 9(07)V9(01).
     03  DJD-A12       PIC X(01).
     03  DJD-F13       PIC -9999999.9.
     03  DJD-A13       PIC X(01).
     03  DJD-F14       PIC 9(10)V9(02).
     03  DJD-A14       PIC X(01).
     03  DJD-F15       PIC -99999999.
     03  DJD-A15       PIC X(01).
     03  DJD-F16       PIC X(09).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7315B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7315B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY7315B".
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
                        PROCEDURE DYJRYOF.
     MOVE     "DYJRYOF "          TO        ERR-FL-ID.
     MOVE     DYJ-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE DYJRYDT.
     MOVE     "DYJRYDT"          TO        ERR-FL-ID.
     MOVE     DJD-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY7315B-START         SECTION.
*
     MOVE   "SSY7315B-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSY7315B-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     DYJRYOF.
     OPEN     OUTPUT    DYJRYDT.
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
     PERFORM  DYJRYOF-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     ELSE
              MOVE   SPACE         TO        DJD-REC
*             INITIALIZE                     DJD-REC
              MOVE   WK-TAITL      TO        DJD-REC
              WRITE  DJD-REC
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ホーマック物品受領書データ読込み
****************************************************************
 DYJRYOF-RD-SEC             SECTION.
*
     MOVE    "DYJRYOF-RD-SEC"    TO   S-NAME.
*
     READ     DYJRYOF
          AT END
              MOVE     "END"      TO        END-FLG
          NOT  AT  END
              ADD       1         TO        READ-CNT
     END-READ.
*
 DYJRYOF-RD-EXIT.
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
     MOVE     ","                 TO   DJD-A01 DJD-A02 DJD-A03.
     MOVE     ","                 TO   DJD-A04 DJD-A05 DJD-A06.
     MOVE     ","                 TO   DJD-A07 DJD-A08 DJD-A09.
     MOVE     ","                 TO   DJD-A10 DJD-A111.
     MOVE     ","                 TO   DJD-A112 DJD-A12.
     MOVE     ","                 TO   DJD-A13 DJD-A14 DJD-A15.
*
     MOVE     DYJ-F00             TO   DJD-F01.
     MOVE     DYJ-F02             TO   DJD-F02.
     MOVE     DYJ-F03             TO   DJD-F03.
     MOVE     DYJ-F04             TO   DJD-F04.
     MOVE     DYJ-F05             TO   DJD-F05.
     MOVE     DYJ-F06             TO   DJD-F06.
     MOVE     DYJ-F07             TO   DJD-F07.
     MOVE     DYJ-F08             TO   DJD-F08.
     MOVE     DYJ-F09             TO   DJD-F09.
     MOVE     DYJ-F15             TO   DJD-F10.
     MOVE     DYJ-F101            TO   DJD-F111.
     MOVE     DYJ-F102            TO   DJD-F112.
     MOVE     DYJ-F11             TO   DJD-F12.
*#2019/05/24 NAV ST
     COMPUTE  DJD-F12  =  DYJ-F11  /   10.
*#2019/05/24 NAV ED
     MOVE     DYJ-F12             TO   DJD-F13.
     MOVE     DYJ-F13             TO   DJD-F14.
     MOVE     DYJ-F14             TO   DJD-F15.
     MOVE     DYJ-F16             TO   DJD-F16.
*#2019/05/24 NAV ST
     COMPUTE  DJD-F14  =  DYJ-F13  /   100.
***  COMPUTE  DJD-F15  =  DYJ-F14  /   10.
*#2019/05/24 NAV ED
     MOVE     WK-MEISAI           TO   DJD-REC.
     WRITE    DJD-REC.
     ADD      1                   TO   OUTPUT-CNT.
*    次レコード読込み
     PERFORM  DYJRYOF-RD-SEC.
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
     CLOSE    DYJRYOF   DYJRYDT.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.
