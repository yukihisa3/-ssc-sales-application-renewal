****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　　　　　　　　　　　　　　　　　　*
*    モジュール名　　　　：　支払データ分割　　　　　　　　　　*
*    作成日／更新日　　　：　00/02/14                          *
*    作成者／更新者　　　：　HAGIWARA                          *
*    処理概要　　　　　　：　（ＣＶＣＳ）オンラインデータから　*
*                            支払合計ファイルを作成する　　　　*
*                            ホームワイド用　　　　　          *
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSI0190B.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          00/02/14.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   SIHAHW  ASSIGN    TO        SIHAHW
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DEN-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*支払合計ファイル
     SELECT   SITGKF    ASSIGN    TO        SITGKF
                        ACCESS    MODE      SEQUENTIAL
                        FILE  STATUS   IS   GKF-STATUS
                        ORGANIZATION        SEQUENTIAL.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２５６　  ＢＦ＝　１
******************************************************************
 FD  SIHAHW
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
     03  DEN-01                  OCCURS 4.
         05  DEN-01A             PIC  X(01).
         05  DEN-01B             PIC  X(63).
******************************************************************
*    支払合計ファイル
******************************************************************
 FD  SITGKF.
 01  GKF-REC.
     03  GKF-F01           PIC  X(01).
     03  GKF-F02           PIC  X(63).
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
*
*01  WK-AREA.
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  GKF-STATUS        PIC  X(02).
*
 01  MSG-AREA.
*    03  MSG-START.
*        05  FILLER         PIC   X(05)  VALUE " *** ".
*        05  ST-PG          PIC   X(08)  VALUE "SSI0190B".
*        05  FILLER         PIC   X(11)  VALUE
*                                        " START *** ".
*    03  MSG-END.
*        05  FILLER         PIC   X(05)  VALUE " *** ".
*        05  END-PG         PIC   X(08)  VALUE "SSI0190B".
*        05  FILLER         PIC   X(11)  VALUE
*                                        " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSI0190B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*    03  MSG-IN.
*        05  FILLER         PIC   X(05)  VALUE " *** ".
*        05  FILLER         PIC   X(09)  VALUE " INPUT = ".
*        05  IN-CNT         PIC   9(06).
*        05  FILLER         PIC   X(05)  VALUE " *** ".
*    03  MSG-OUT.
*        05  FILLER         PIC   X(05)  VALUE " *** ".
*        05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
*        05  OUT-CNT        PIC   9(06).
*        05  FILLER         PIC   X(05)  VALUE " *** ".
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SIHAHW.
     MOVE      "SIHAHW"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SITGKF.
     MOVE      "SITGKF"     TO   AB-FILE.
     MOVE      GKF-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SIHAHW.
     OPEN     OUTPUT    SITGKF.
*    DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG.
*
     READ     SIHAHW
              AT END    MOVE      9         TO  END-FG
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
     PERFORM  VARYING   IDX      FROM      1  BY  1
              UNTIL     IDX      >        4
*
         IF   DEN-01A(IDX)   NOT = SPACE
              MOVE      DEN-01A(IDX)        TO   GKF-F01
              MOVE      DEN-01B(IDX)        TO   GKF-F02
*
              WRITE    GKF-REC
         END-IF
     END-PERFORM.
*
     READ     SIHAHW
              AT END    MOVE      9         TO  END-FG
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*    DISPLAY   MSG-IN    UPON CONS.
*    DISPLAY   MSG-OUT   UPON CONS.
*    DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     SIHAHW  SITGKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
