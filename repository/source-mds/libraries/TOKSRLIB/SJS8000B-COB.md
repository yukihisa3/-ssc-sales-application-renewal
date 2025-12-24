# SJS8000B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJS8000B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　種子実績管理システム　　　　　　　*
*    業務名　　　　　　　：　種子実績連携データ作成            *
*    モジュール名　　　　：　種子実績連携データ作成            *
*    作成日／更新日　　　：　2021/12/03                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　種子実績連携データをＣＳＶ形式に　*
*    　　　　　　　　　　　　出力する。　　　　　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SJS8000B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          21/12/03.
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
****<<種子実績連携データ>>**************************************
     SELECT   PCJISWF            ASSIGN    TO   DA-01-VI-PCJISWL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    SJS-F01  SJS-F02
                                                SJS-F03  SJS-F04
                                                SJS-F23  SJS-F22
                                                SJS-F06  SJS-F11
                                                SJS-F07  SJS-F08
                                 STATUS         SJS-STATUS.
*
*****<<ＣＳＶ形式データ>>**************************************
     SELECT   PCJISWWK           ASSIGN    TO   PCJISWWK
                                 STATUS         SJW-STATUS.
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = 種子実績連携データ　　　　                         *
*--------------------------------------------------------------*
 FD  PCJISWF            LABEL RECORD   IS   STANDARD.
     COPY     PCJISWF   OF        XFDLIB
              JOINING   SJS       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ＣＳＶ形式データ　　　　　　　　　                 *
*--------------------------------------------------------------*
 FD  PCJISWWK           BLOCK CONTAINS 1    RECORDS.
 01  SJW-REC.
     03  FILLER         PIC       X(500).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  CHK-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  SJS-STATUS                   PIC       X(02).
 01  SJW-STATUS                   PIC       X(02).
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
*明細エリア
 01  WK-MEISAI.
     03  SJW-F01       PIC X(04).
     03  SJW-A01       PIC X(01).
     03  SJW-F02       PIC 9(08).
     03  SJW-A02       PIC X(01).
     03  SJW-F03       PIC 9(05).
     03  SJW-A03       PIC X(01).
     03  SJW-F04       PIC 9(02).
     03  SJW-A04       PIC X(01).
     03  SJW-F05       PIC X(02).
     03  SJW-A05       PIC X(01).
     03  SJW-F06       PIC 9(01).
     03  SJW-A06       PIC X(01).
     03  SJW-F07       PIC 9(09).
     03  SJW-A07       PIC X(01).
     03  SJW-F08       PIC 9(02).
     03  SJW-A08       PIC X(01).
     03  SJW-F09       PIC X(08).
     03  SJW-A09       PIC X(01).
     03  SJW-F10       PIC X(08).
     03  SJW-A10       PIC X(01).
     03  SJW-F11       PIC X(08).
     03  SJW-A11       PIC X(01).
     03  SJW-F12       PIC X(13).
     03  SJW-A12       PIC X(01).
     03  SJW-F13       PIC X(08).
     03  SJW-A13       PIC X(01).
     03  SJW-F14       PIC X(05).
     03  SJW-A14       PIC X(01).
     03  SJW-F15       PIC X(02).
     03  SJW-A15       PIC X(01).
     03  SJW-F16       PIC X(01).
     03  SJW-A16       PIC X(01).
     03  SJW-F17       PIC 9(07).
     03  SJW-A17       PIC X(01).
     03  SJW-F18       PIC 9(07).
     03  SJW-A18       PIC X(01).
     03  SJW-F19       PIC 9999999.99.
     03  SJW-A19       PIC X(01).
     03  SJW-F20       PIC 9999999.99.
     03  SJW-A20       PIC X(01).
     03  SJW-F21       PIC 9999999.99.
     03  SJW-A21       PIC X(01).
     03  SJW-F22       PIC X(01).
     03  SJW-A22       PIC X(01).
     03  SJW-F23       PIC X(02).
     03  SJW-A23       PIC X(01).
     03  SJW-F24       PIC X(01).
     03  SJW-A24       PIC X(01).
     03  SJW-F25       PIC X(01).
     03  SJW-A25       PIC X(01).
     03  SJW-F26       PIC X(08).
     03  SJW-A26       PIC X(01).
     03  SJW-F27       PIC X(06).
     03  SJW-A27       PIC X(01).
     03  SJW-F28       PIC X(08).
     03  SJW-A28       PIC X(01).
     03  SJW-F29       PIC X(06).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJS8000B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJS8000B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SJS8000B".
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
                        PROCEDURE PCJISWF.
     MOVE     "PCJISWF "          TO        ERR-FL-ID.
     MOVE     SJS-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE PCJISWWK.
     MOVE     "PCJISWWK"          TO        ERR-FL-ID.
     MOVE     SJW-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SJS8000B-START         SECTION.
*
     MOVE   "SJS8000B-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SJS8000B-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     PCJISWF.
     OPEN     OUTPUT    PCJISWWK.
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
     PERFORM  PCJISWF-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    種子実績連携データ読込　　　　　
****************************************************************
 PCJISWF-RD-SEC             SECTION.
*
     MOVE    "PCJISWF-RD-SEC"    TO   S-NAME.
*
     READ     PCJISWF
          AT END
              MOVE     "END"      TO        END-FLG
          NOT  AT  END
              ADD       1         TO        READ-CNT
     END-READ.
*
 PCJISWF-RD-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*初期化
     MOVE     SPACE               TO   WK-MEISAI.
     INITIALIZE                        WK-MEISAI.
*項目転送
     MOVE     ","                 TO   SJW-A01 SJW-A02 SJW-A03.
     MOVE     ","                 TO   SJW-A04 SJW-A05 SJW-A06.
     MOVE     ","                 TO   SJW-A07 SJW-A08 SJW-A09.
     MOVE     ","                 TO   SJW-A10 SJW-A11 SJW-A12.
     MOVE     ","                 TO   SJW-A13 SJW-A14 SJW-A15.
     MOVE     ","                 TO   SJW-A16 SJW-A17 SJW-A18.
     MOVE     ","                 TO   SJW-A16 SJW-A17 SJW-A18.
     MOVE     ","                 TO   SJW-A19 SJW-A20 SJW-A21.
     MOVE     ","                 TO   SJW-A22 SJW-A23 SJW-A24.
     MOVE     ","                 TO   SJW-A25 SJW-A26 SJW-A27.
     MOVE     ","                 TO   SJW-A28.
*
     MOVE     SJS-F01             TO   SJW-F01.
     MOVE     SJS-F02             TO   SJW-F02.
     MOVE     SJS-F03             TO   SJW-F03.
     MOVE     SJS-F04             TO   SJW-F04.
     MOVE     SJS-F05             TO   SJW-F05.
     MOVE     SJS-F06             TO   SJW-F06.
     MOVE     SJS-F07             TO   SJW-F07.
     MOVE     SJS-F08             TO   SJW-F08.
     MOVE     SJS-F09             TO   SJW-F09.
     MOVE     SJS-F10             TO   SJW-F10.
     MOVE     SJS-F11             TO   SJW-F11.
     MOVE     SJS-F12             TO   SJW-F12.
     MOVE     SJS-F13             TO   SJW-F13.
     MOVE     SJS-F14             TO   SJW-F14.
     MOVE     SJS-F15             TO   SJW-F15.
     MOVE     SJS-F16             TO   SJW-F16.
     MOVE     SJS-F17             TO   SJW-F17.
     MOVE     SJS-F18             TO   SJW-F18.
     MOVE     SJS-F19             TO   SJW-F19.
     MOVE     SJS-F20             TO   SJW-F20.
     MOVE     SJS-F21             TO   SJW-F21.
     MOVE     SJS-F22             TO   SJW-F22.
     MOVE     SJS-F23             TO   SJW-F23.
     MOVE     SJS-F24             TO   SJW-F24.
     MOVE     SJS-F95             TO   SJW-F25.
     MOVE     SJS-F96             TO   SJW-F26.
     MOVE     SJS-F97             TO   SJW-F27.
     MOVE     SJS-F98             TO   SJW-F28.
     MOVE     SJS-F99             TO   SJW-F29.
     MOVE     WK-MEISAI           TO   SJW-REC.
     WRITE    SJW-REC.
     ADD      1                   TO   OUTPUT-CNT.
*    次レコード読込み
     PERFORM  PCJISWF-RD-SEC.
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
*
     CLOSE    PCJISWF   PCJISWWK.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.

```
