# SSY7892B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY7892B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　出荷情報データ削除　　　　　　　　*
*    作成日／更新日　　　：　2008/11/13                        *
*    作成者／更新者　　　：　松野　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタより受取ったキー情報により*
*                            出荷情報データを削除する。　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY7892B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/11/13.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*基本情報データ
     SELECT   RHSYUKF   ASSIGN    TO        DA-01-VI-RHSYUKL5
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JOH-F01   JOH-F02
                                            JOH-F03   JOH-F04
                                            JOH-F08   JOH-F05
                                            JOH-F06   JOH-F07
                        FILE  STATUS   IS   JOH-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
*基本情報データ
 FD  RHSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     RHSYUKF   OF        XFDLIB
              JOINING   JOH       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  REWT-CNT                PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  CHK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7892B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7892B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7892B".
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
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKCD             PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE  PARA-JTIME
                                       PARA-TORICD PARA-SOKCD
                                       PARA-NOUDT.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RHSYUKF.
     MOVE      "RHSYUKF "   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
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
              UNTIL     END-FLG   =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       RHSYUKF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     SPACE     TO        END-FLG.
     DISPLAY "PARA-JDATE  = " PARA-JDATE   UPON CONS.
     DISPLAY "PARA-JTIME  = " PARA-JTIME   UPON CONS.
     DISPLAY "PARA-TORICD = " PARA-TORICD  UPON CONS.
     DISPLAY "PARA-SOKCD  = " PARA-SOKCD   UPON CONS.
     DISPLAY "PARA-NOUDT  = " PARA-NOUDT   UPON CONS.
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
*
     MOVE     SPACE          TO   JOH-REC.
     INITIALIZE                   JOH-REC.
     MOVE     PARA-JDATE     TO   JOH-F01.
     MOVE     PARA-JTIME     TO   JOH-F02.
     MOVE     PARA-TORICD    TO   JOH-F03.
     MOVE     PARA-SOKCD     TO   JOH-F04.
     MOVE     PARA-NOUDT     TO   JOH-F08.
     START    RHSYUKF   KEY  >=   JOH-F01   JOH-F02
                                  JOH-F03   JOH-F04
                                  JOH-F08   JOH-F05
                                  JOH-F06   JOH-F07
         INVALID   KEY
              MOVE      "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*
     PERFORM  RHSYUKF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　リック情報ファイル
****************************************************************
 RHSYUKF-READ-SEC   SECTION.
*
     READ     RHSYUKF
              AT END    MOVE   "END"        TO  END-FLG
                        GO                  TO  RHSYUKF-READ-EXIT
              NOT AT END
                        ADD       1    TO   RD-CNT
     END-READ.
*
     IF   RD-CNT(6:3)  =  "000" OR "500"
          DISPLAY "READ-CNT = " RD-CNT     UPON CONS
     END-IF.
 READ010.
*指定されたバッチ番号のみ
     IF     ( PARA-JDATE     =    JOH-F01 ) AND
            ( PARA-JTIME     =    JOH-F02 )
              CONTINUE
     ELSE
              MOVE   "END"        TO  END-FLG
              GO                  TO  RHSYUKF-READ-EXIT
     END-IF.
 READ020.
*取引先ＣＤチェック
     IF       PARA-TORICD    =    ZERO
              CONTINUE
     ELSE
*             DISPLAY "PARA-TORICD = " PARA-TORICD UPON CONS
*             DISPLAY "JOH-K03     = " JOH-K03     UPON CONS
              IF   PARA-TORICD  =  JOH-F03
                   CONTINUE
              ELSE
                   GO                  TO  RHSYUKF-READ-SEC
              END-IF
     END-IF.
 READ030.
*倉庫ＣＤチェック
     IF       PARA-SOKCD     =    SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKCD  =  JOH-F04
                   CONTINUE
              ELSE
                   GO                  TO  RHSYUKF-READ-SEC
              END-IF
     END-IF.
 READ040.
*納品日ＣＤチェック
     IF       PARA-NOUDT     =    ZERO
              CONTINUE
     ELSE
              IF   PARA-NOUDT  =  JOH-F08
                   CONTINUE
              ELSE
                   GO                  TO  RHSYUKF-READ-SEC
              END-IF
     END-IF.
*
 RHSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*リック情報ファイル読込み
     MOVE     SPACE              TO   JOH-F11.
     MOVE     ZERO               TO   JOH-F12.
     REWRITE  JOH-REC.
     ADD      1                  TO   REWT-CNT.
*
     PERFORM RHSYUKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     CLOSE     RHSYUKF.
*
     DISPLAY "READ-CNT   =  "  RD-CNT      UPON CONS.
     DISPLAY "REWT-CNT   =  "  REWT-CNT    UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
