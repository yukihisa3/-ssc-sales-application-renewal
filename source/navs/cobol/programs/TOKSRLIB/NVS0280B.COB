****************************************************************
*                                                              *
*    顧客名　　　　　：　（株）サカタのタネ殿　　　　　　　　　*
*    サブシステム　　：　Ｄ３６５連携　　　　　　　　　　　　　*
*    モジュール名　　：　ＵＲＩＫＥＪＦ実納品日初期化　　　　　*
*    作成日　　　　　：　2022/03/10                            *
*    作成者　　　　　：　NAV TAKAHASHI                         *
*    処理概要　　　　：　ＵＲＩＫＥＪＦを読み、追加された実納　*
*                        品日項目の初期化を行う。　　　　　　　*
*                        ０→Ｆ５１へセット　　　　　　　　　　*
*    更新日　　　　　：　                                      *
*    更新者　　　　　：　                                      *
*    更新概要　　　　：　                                      *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NVS0280B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2022/03/10.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上計上ファイル
     SELECT   URIKEJL4  ASSIGN    TO        DA-01-VI-URIKEJL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KEJ-F48
                                            KEJ-F49
                                            KEJ-F01
                                            KEJ-F02
                                            KEJ-F04
                                            KEJ-F03
                                            KEJ-F05
                                            KEJ-F06
                                            KEJ-F07
                        FILE  STATUS   IS   KEJ-STATUS.
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*   売上計上ファイル
******************************************************************
 FD  URIKEJL4
                        LABEL RECORD   IS   STANDARD.
     COPY     URIKEJL4  OF        XFDLIB
              JOINING   KEJ  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  UPD-CNT                 PIC  9(08)     VALUE  ZERO.
 01  IXA                     PIC  9(02)     VALUE  ZERO.
*
 01  WK-PARA-IN-AREA.
     03  WK-PARA             OCCURS  12.
       05  WK-SDENNO         PIC  9(09).
       05  WK-EDENNO         PIC  9(09).
 01  FLG-AREA.
     03  URIKEJL4-END        PIC  X(03)     VALUE SPACE.
     03  SHTDENL1-INV        PIC  X(03)     VALUE SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE            PIC 9(06).
     03  SYS-DATEW           PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  KEJ-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  SHO-STATUS        PIC  X(02).
     03  SH1-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NVS0280B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVS0280B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVS0280B".
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
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " UPDATE= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE             DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   URIKEJL4.
     MOVE      "URIKEJL4"   TO   AB-FILE.
     MOVE      KEJ-STATUS   TO   AB-STS.
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
     OPEN     I-O       URIKEJL4.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    UPD-CNT.
     MOVE     ZERO      TO        IN-CNT    UPD-CNT.
*
     PERFORM  URIKEJL4-READ-SEC.
     IF       URIKEJL4-END  =  "END"
              DISPLAY NC"売上計上ファイル対象なし" UPON CONS
              MOVE    9     TO    END-FG
              GO            TO    INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
 MAIN-01.
     MOVE     ZERO          TO    KEJ-F51.
     REWRITE  KEJ-REC.
     ADD      1             TO    UPD-CNT.
*
 MAIN-99.
*
     PERFORM  URIKEJL4-READ-SEC.
     IF       URIKEJL4-END  =  "END"
              MOVE    9     TO    END-FG
              GO            TO    MAIN-EXIT
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上計上ファイル順ＲＥＡＤ　　　　　　　　　　　*
****************************************************************
 URIKEJL4-READ-SEC       SECTION.
*
     MOVE     "URIKEJL4-READ-SEC"  TO      S-NAME.
*
*売上計上ファイル順ＲＥＡＤ
     READ     URIKEJL4
         AT END
              MOVE   "END"      TO    URIKEJL4-END
              GO                TO    URIKEJL4-READ-EXIT
         NOT AT END
              ADD      1              TO    RD-CNT
     END-READ.
*
 URIKEJL4-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      UPD-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     URIKEJL4.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
