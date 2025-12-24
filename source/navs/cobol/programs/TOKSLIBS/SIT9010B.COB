****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＩＴ統制システム　　　　　　　　　*
*    モジュール名　　　　：　マスタ更新履歴ファイル削除        *
*    作成日／更新日　　　：　2009/03/17                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　３ヶ月以前の更新日付のレコードの  *
*                            削除をおこなう。　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SIT9010B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          09/03/17.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*マスタ更新履歴ファイル
     SELECT   MSTLOGF   ASSIGN    TO        DA-01-VI-MSTLOGL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       LOG-F05
                                            LOG-F06
                        FILE      STATUS    LOG-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    マスタ更新履歴ファイル　　　　
******************************************************************
 FD  MSTLOGF
                        LABEL RECORD   IS   STANDARD.
     COPY     MSTLOGF   OF        XFDLIB
              JOINING   LOG  AS   PREFIX.
*
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  DEL-CNT             PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
     03  SYS-DATEYMD   REDEFINES  SYS-DATEW.
       05  SYS-DATEW-YYYY  PIC 9(04).
       05  SYS-DATEW-MM    PIC 9(02).
       05  SYS-DATEW-DD    PIC 9(02).
     03  SELECT-YMD.
       05  SELECT-YYYY      PIC  9(04).
       05  SELECT-MM        PIC  9(02).
       05  SELECT-DD        PIC  9(02).
 01  WK-ST.
     03  LOG-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SIT9010B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SIT9010B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SIT9010B".
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
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MSTLOGF.
     MOVE      "MSTLOGL2"   TO   AB-FILE.
     MOVE      LOG-STATUS   TO   AB-STS.
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
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       MSTLOGF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
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
         MOVE    "END"          TO   END-FLG
     END-IF.
*
     IF  SYS-DATEW-MM  >  03
         COMPUTE  SELECT-MM  =  SYS-DATEW-MM  - 03
         MOVE    SYS-DATEW-YYYY   TO   SELECT-YYYY
         MOVE    SYS-DATEW-DD     TO   SELECT-DD
     ELSE
         COMPUTE  SELECT-MM  = ( SYS-DATEW-MM +  12 ) -  03
         COMPUTE  SELECT-YYYY  =  SYS-DATEW-YYYY  -  01
         MOVE    SYS-DATEW-DD     TO   SELECT-DD
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
     PERFORM  READ-LOG-SEC.
     IF  END-FLG  =  "END"  GO  TO  MAIN-EXIT.
* 更新日付の判定
     IF  LOG-F05  <  SELECT-YMD
         PERFORM  DELETE-LOG-SEC
     ELSE
         MOVE   "END"   TO   END-FLG
     END-IF.
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
     DISPLAY "ﾀｲｼﾖｳﾋｽﾞｹ(YYYYMMDD)  = " SELECT-YMD  UPON CONS.
     DISPLAY "ﾏｽﾀｺｳｼﾝﾘﾚｷ F   READ  CNT = " READ-CNT  UPON CONS.
     DISPLAY "ﾏｽﾀｺｳｼﾝﾘﾚｷ F    DEL  CNT = " DEL-CNT   UPON CONS.
*
     CLOSE     MSTLOGF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　履歴ファイル読込処理
****************************************************************
 READ-LOG-SEC       SECTION.
*
     MOVE     "READ-LOG-SEC"      TO   S-NAME.
     READ     MSTLOGF
         AT   END
              MOVE    "END"       TO   END-FLG
              GO  TO   READ-LOG-EXIT
     END-READ.
*
     ADD    1   TO  READ-CNT.
*
*
 READ-LOG-EXIT.
     EXIT.
****************************************************************
*　　　　　履歴ファイル削除処理
****************************************************************
 DELETE-LOG-SEC     SECTION.
*
     MOVE     "DELETE-LOG-SEC" TO   S-NAME.
     DELETE   MSTLOGF.
     ADD      1                   TO   DEL-CNT.
*
 DELETE-LOG-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
