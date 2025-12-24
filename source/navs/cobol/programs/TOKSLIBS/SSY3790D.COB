****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩシステム　　　　　　*
*    業務名　　　　　　　：　受領処理                          *
*    モジュール名　　　　：　受領累積データ削除　　　　　      *
*    作成日／更新日　　　：　2013/12/24                        *
*    作成者／更新者　　　：　NAV TAKAHAHSI                     *
*    処理概要　　　　　　：　受領累積データの削除を行なう。　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY3790D.
 AUTHOR.                NAV.
 DATE-WRITTEN.          13/12/24.
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
****<<受領累積マスタ >>*********************************
     SELECT   NFJYURF            ASSIGN    TO   DA-01-VI-NFJYURL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JYU-F02  JYU-F05
                                                JYU-F03  JYU-F04
                                                JYU-F07  JYU-F08
                                 STATUS         JYU-STATUS.
****<<受領累積バックアップ >>***************************
     SELECT  NFJYRBK             ASSIGN    TO   DA-01-S-NFJYRBK
                                 STATUS         JYD-STATUS.
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ナフコ受領累積ファイル　　　　　　　               *
*--------------------------------------------------------------*
 FD  NFJYURF            LABEL RECORD   IS   STANDARD.
     COPY     NFJYURF   OF        XFDLIB
              JOINING   JYU       PREFIX.
*--------------------------------------------------------------*
*    FILE = 受領累積バックアップ　　　　　　　　               *
*--------------------------------------------------------------*
 FD  NFJYRBK           BLOCK     CONTAINS  20   RECORDS
                       LABEL     RECORD    IS   STANDARD.
     COPY     NFJYURF   OF        XFDLIB
              JOINING   JYD       PREFIX.
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  JYU-STATUS                   PIC       X(02).
 01  JYD-STATUS                   PIC       X(02).
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
 01  CNT-READ                     PIC       9(06)  VALUE  ZERO.
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3790D".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3790D".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY3790D".
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
         05  FILLER         PIC   X(09)  VALUE " DEL  = ".
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
 01  PARA-STYMD             PIC   9(08).
 01  PARA-EDYMD             PIC   9(08).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-STYMD
                                           PARA-EDYMD.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           NFJYURF.
     MOVE     "NFJYURF"           TO        ERR-FL-ID.
     MOVE     JYU-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           NFJYRBK.
     MOVE     "NFJYRBK"           TO        ERR-FL-ID.
     MOVE     JYD-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY3790D-START         SECTION.
*
     MOVE   "SSY3790D-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
*
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SSY3790D-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       NFJYURF.
     OPEN     OUTPUT    NFJYRBK.
*
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
     MOVE  ZERO                 TO   IN-CNT  OUT-CNT.
*
     MOVE  SPACE                TO   JYU-REC
     INITIALIZE                      JYU-REC
     MOVE  PARA-STYMD           TO   JYU-F02
*
     START  NFJYURF  KEY  >=   JYU-F02  JYU-F05  JYU-F03
                               JYU-F04  JYU-F07  JYU-F08
         INVALID   KEY
            MOVE     "END"      TO   END-FLG
            DISPLAY NC"＃対象データ無し１＃" UPON CONS
            GO                  TO   INIT-EXIT
     END-START
*
     PERFORM  NFJYURF-RD-SEC
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ　受領累積ファイル読み込み　　　
****************************************************************
 NFJYURF-RD-SEC            SECTION.
*
     MOVE    "NFJYURF-RD-SEC"    TO   S-NAME.
*
     READ     NFJYURF
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    NFJYURF-RD-EXIT
     END-READ.
*
     ADD      1      TO               IN-CNT.
     IF  IN-CNT(4:3) = "000" OR "500"
         DISPLAY "IN-CNT = " IN-CNT UPON CONS
     END-IF.
*
     IF   JYU-F02   >  PARA-EDYMD
          MOVE     "END"      TO   END-FLG
     END-IF.
*
 NFJYURF-RD-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO    S-NAME.
*    削除前にレコードをバックアップ
     MOVE    SPACE                TO    JYD-REC.
     INITIALIZE                         JYD-REC.
     MOVE    JYU-REC              TO    JYD-REC.
     WRITE   JYD-REC.
*    レコードを削除する。
     DELETE  NFJYURF.
     ADD     1                    TO    OUT-CNT.
*    次レコード読込み
     PERFORM  NFJYURF-RD-SEC.
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
     DISPLAY  MSG-IN    UPON CONS.
     DISPLAY  MSG-OUT   UPON CONS.
     DISPLAY  MSG-END   UPON CONS.
*
     CLOSE    NFJYURF.
*
 END-EXIT.
     EXIT.
*
