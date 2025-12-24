****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    モジュール名　　　　：　バッチ_変換サブルーチン　　　　　*
*    　　　　　　　　　　　　（時刻＋１分）                    *
*    作成日　　　　　　　：　2021/01/06                        *
*    作成者　　　　　　　：　INOUE                             *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY1293B.
*                 流用：SSY4466B ジュンテンドー
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/01/06.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY1293B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1293B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1293B".
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
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
*バッチ時刻編集
 01  WK-BATTI-HHMM.
     03  WK-HH             PIC        9(02)  VALUE  ZERO.
     03  WK-MM             PIC        9(02)  VALUE  ZERO.
 01  WK-BATTI-HHMM-2       PIC        9(04)  VALUE  ZERO.
*
 01  BATTI-JIKAN           PIC        9(08)  VALUE  ZERO.
 01  BATTI-JIKOKU          PIC        9(04)  VALUE  ZERO.
 01  BATTI-TOKCD           PIC        9(08)  VALUE  ZERO.
 01  BATTI-JIKOKU-FLG      PIC        X(01)  VALUE  SPACE.
 01  BATTI-CHK             PIC        X(01)  VALUE  SPACE.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-JDATE         PIC   9(08).
 01  PARA-IN-JTIME         PIC   9(04).
 01  PARA-IN-JTORICD       PIC   9(08).
 01  PARA-OUT-JTIME        PIC   9(04).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-JDATE
                                       PARA-IN-JTIME
                                       PARA-IN-JTORICD
                                       PARA-OUT-JTIME.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG    =    "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     SPACE     TO        END-FLG.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*バッチ時刻＋１分算出
     MOVE        ZERO           TO   WK-BATTI-HHMM
                                     WK-BATTI-HHMM-2.
     MOVE        PARA-IN-JTIME  TO   WK-BATTI-HHMM.
     ADD         1              TO   WK-MM.
     IF   WK-MM  =  60
          ADD    1              TO   WK-HH
          MOVE   ZERO           TO   WK-MM
          MOVE   WK-BATTI-HHMM  TO   WK-BATTI-HHMM-2
     ELSE
          MOVE   WK-BATTI-HHMM  TO   WK-BATTI-HHMM-2
     END-IF.
     DISPLAY NC"＃バッチ時間２" " = " WK-HH ":" WK-MM
             UPON CONS.
     DISPLAY NC"＃バッチ２" " = " WK-BATTI-HHMM-2 UPON CONS.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
     MOVE    "END"               TO   END-FLG.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      WK-BATTI-HHMM-2   TO     PARA-OUT-JTIME.
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
