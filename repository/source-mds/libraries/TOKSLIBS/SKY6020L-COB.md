# SKY6020L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SKY6020L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ジョイ物品受領書　　　　　　　　　*
*    業務名　　　　　　　：　物品受領書                        *
*    モジュール名　　　　：　物品受領書                        *
*    作成日／更新日　　　：　08/02/04                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　ジョイ物品受領書を発行する。　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SKY6020L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/02/04.
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
****<<ｼﾞｮｲ物品受領データ >>*************************************
     SELECT   JYURYOF            ASSIGN    TO   DA-01-VI-JYURYOL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JYU-F01  JYU-F02
                                                JYU-F03  JYU-F04
                                                JYU-F06  JYU-F07
                                 STATUS         JYU-STATUS.
*
*****<<  受領ﾃﾞｰﾀCSV   >>**************************************
     SELECT   JYURYOSF           ASSIGN    TO   JYURYOSF
                                 STATUS         JOY-STATUS.
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ジョイ　物品受領書　　　　　                       *
*--------------------------------------------------------------*
 FD  JYURYOF            LABEL RECORD   IS   STANDARD.
     COPY     JYURYOF   OF        XFDLIB
              JOINING   JYU       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ジョイ　物品ＣＳＶデータ　　　　　                 *
*--------------------------------------------------------------*
 FD  JYURYOSF           BLOCK CONTAINS 26   RECORDS.
 01  JOY-REC.
     03  FILLER         PIC       X(154).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*ヘッダ情報格納領域
     COPY   JYURYOSF OF XFDLIB  JOINING   JJR  AS   PREFIX.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  CHK-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  JYU-STATUS                   PIC       X(02).
 01  JOY-STATUS                   PIC       X(02).
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
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SKY6020L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKY6020L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SKY6020L".
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
                        PROCEDURE JYURYOF.
     MOVE     "JYURYOF "          TO        ERR-FL-ID.
     MOVE     JYU-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE JYURYOSF.
     MOVE     "JYURYOSF"          TO        ERR-FL-ID.
     MOVE     JOY-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SKY6020L-START         SECTION.
*
     MOVE   "SKY6020L-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SKY6020L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     JYURYOF.
     OPEN     OUTPUT    JYURYOSF.
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
     PERFORM  JYURYOF-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     ELSE
              MOVE   SPACE         TO        JOY-REC
*             INITIALIZE                     JOY-REC
              MOVE   WK-TAITL      TO        JOY-REC
              WRITE  JOY-REC
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ジョイ物品受領書データ読込み
****************************************************************
 JYURYOF-RD-SEC             SECTION.
*
     MOVE    "JYURYOF-RD-SEC"    TO   S-NAME.
*
     READ     JYURYOF
          AT END
              MOVE     "END"      TO        END-FLG
          NOT  AT  END
              ADD       1         TO        READ-CNT
     END-READ.
*
 JYURYOF-RD-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*初期化
     MOVE     SPACE               TO   JJR-REC.
     INITIALIZE                        JJR-REC.
*項目転送
     MOVE     ","                 TO   JJR-A01 JJR-A02 JJR-A03.
     MOVE     ","                 TO   JJR-A04 JJR-A05 JJR-A06.
     MOVE     ","                 TO   JJR-A07 JJR-A08 JJR-A09.
     MOVE     ","                 TO   JJR-A10 JJR-A11 JJR-A12.
     MOVE     ","                 TO   JJR-A13 JJR-A14.
*
     MOVE     JYU-F01             TO   JJR-F01.
     MOVE     JYU-F02             TO   JJR-F02.
     MOVE     JYU-F03             TO   JJR-F03.
     MOVE     JYU-F04             TO   JJR-F04.
     MOVE     JYU-F05             TO   JJR-F05.
     MOVE     JYU-F06             TO   JJR-F06.
     MOVE     JYU-F07             TO   JJR-F07.
     MOVE     JYU-F08             TO   JJR-F08.
     MOVE     JYU-F09             TO   JJR-F09.
     MOVE     JYU-F15             TO   JJR-F10.
     MOVE     JYU-F10             TO   JJR-F11.
     MOVE     JYU-F11             TO   JJR-F12.
     MOVE     JYU-F12             TO   JJR-F13.
     MOVE     JYU-F13             TO   JJR-F14.
     MOVE     JYU-F14             TO   JJR-F15
     MOVE     JJR-REC             TO   JOY-REC.
     WRITE    JOY-REC.
     ADD      1                   TO   OUTPUT-CNT.
*    次レコード読込み
     PERFORM  JYURYOF-RD-SEC.
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
     CLOSE    JYURYOF   JYURYOSF.
*
 END-EXIT.
     EXIT.
********************<<  PUROGRAM  END  >>*************************

```
