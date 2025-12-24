# SSY9015B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY9015B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　オージョイフル物品受領書　　　　  *
*    業務名　　　　　　　：　物品受領書                        *
*    モジュール名　　　　：　物品受領書データ削除              *
*    作成日／更新日　　　：　07/05/29                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　指定された年月度以前のデータを    *
*    　　　　　　　　　　　　削除する。　　　　　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY9015B.
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
     SELECT   OJFJYRF            ASSIGN    TO   DA-01-VI-OJFJYRL3
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JYU-F17  JYU-F00
                                                JYU-F04  JYU-F03
                                                JYU-F06  JYU-F07
                                 STATUS         JYU-STATUS.
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
 FD  OJFJYRF            LABEL RECORD   IS   STANDARD.
     COPY     OJFJYRF   OF        XFDLIB
              JOINING   JYU       PREFIX.
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  CHK-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  JYU-STATUS                   PIC       X(02).
*
 01  NENGETUDO                    PIC       9(06).
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
 01  P-CNT                        PIC       9(04)  VALUE  ZERO.
 01  L-CNT                        PIC       9(03)  VALUE  ZERO.
 01  DEL-CNT                      PIC       9(06)  VALUE  ZERO.
 01  MEI-CNT                      PIC       9(05)  VALUE  ZERO.
*
 01  WK-DATE-HENKAN.
     03  WK-HEN-YYYY              PIC       9(04)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  WK-HEN-MM                PIC       9(02)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  WK-HEN-DD                PIC       9(02)  VALUE  ZERO.
*
 01  WK-KEY.
     03  WK-HATYU-DATE            PIC       9(08)  VALUE  ZERO.
     03  WK-NOUHN-DATE            PIC       9(08)  VALUE  ZERO.
     03  WK-TENPO-CD              PIC       9(04)  VALUE  ZERO.
     03  WK-DENPYO-NO             PIC       9(07)  VALUE  ZERO.
     03  WK-DENPYO-NO1            PIC       9(07)  VALUE  ZERO.
*
 01  WK-GOKEI.
     03  WK-DENPYO-KEI            PIC       9(08)  VALUE  ZERO.
     03  WK-TENPO-KEI             PIC       9(08)  VALUE  ZERO.
     03  WK-SOGOKEI               PIC       9(08)  VALUE  ZERO.
     03  WK-HATYU-SU              PIC      S9(06)V9 VALUE ZERO.
     03  WK-KENSYU-SU             PIC      S9(06)V9 VALUE ZERO.
     03  WK-GENKA-KIN             PIC      S9(08)  VALUE  ZERO.
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY9015B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY9015B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY9015B".
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
                        PROCEDURE OJFJYRF.
     MOVE     "OJFJYRF "          TO        ERR-FL-ID.
     MOVE     JYU-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY9015B-START         SECTION.
*
     MOVE   "SSY9015B-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSY9015B-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       OJFJYRF.
     DISPLAY  MSG-START UPON CONS.
*
     DISPLAY  NC"削除する基準年月度を入力（６桁）" UPON CONS.
*
     ACCEPT   NENGETUDO FROM CONS.
*
     IF       NENGETUDO  NOT NUMERIC
              DISPLAY NC"＃＃年月度を入力して下さい。＃＃"
                      UPON CONS
              STOP  RUN
     END-IF.
*
     ACCEPT   SYSYMD    FROM      DATE.
*
     PERFORM  OJFJYRF-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    オージョイフル物品受領データ読込
****************************************************************
 OJFJYRF-RD-SEC             SECTION.
*
     MOVE    "OJFJYRF-RD-SEC"    TO   S-NAME.
*
     READ     OJFJYRF
          AT END
              MOVE     "END"      TO        END-FLG
              GO                  TO        OJFJYRF-RD-EXIT
     END-READ.
*指定された年月度を超えた場合、終了
     IF       NENGETUDO  <  JYU-F17
              MOVE     "END"      TO        END-FLG
              GO                  TO        OJFJYRF-RD-EXIT
     END-IF.
*
 OJFJYRF-RD-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    レコード削除
     DELETE   OJFJYRF.
     ADD      1                   TO   DEL-CNT.
*    次レコード読込み
     PERFORM  OJFJYRF-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     DISPLAY NC"＃＃削除件数" " = " DEL-CNT UPON CONS.
*
     CLOSE    OJFJYRF.
*
 END-EXIT.
     EXIT.
*
********************<<  PUROGRAM  END  >>*************************

```
