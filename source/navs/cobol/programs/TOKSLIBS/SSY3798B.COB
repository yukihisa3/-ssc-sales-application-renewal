****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩシステム　　　　　　*
*    業務名　　　　　　　：　出荷処理　　　　　　　　　　　　　*
*    モジュール名　　　　：　基本情報ファイル削除　　　　      *
*    作成日／更新日　　　：　2012/01/10                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　指定された管理番号で基本情報Ｆを　*
*                            削除する。　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3798B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/01/10.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*基本情報ファイル
     SELECT   NFJOHOF   ASSIGN    TO        DA-01-VI-NFJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JHO-F01   JHO-F05
                                            JHO-F06   JHO-F07
                                            JHO-F08   JHO-F09
                        FILE      STATUS    JHO-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    出荷情報ファイル
******************************************************************
 FD  NFJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOF   OF        XFDLIB
              JOINING   JHO       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-DEN-F112             PIC  9(08)     VALUE  ZERO.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  DEL-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
*
*ワーク項目
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
     03  SYS-TIME          PIC 9(08).
 01  SYS-TIMEW.
     03  SYS-TIMEHM        PIC  9(04).
     03  FILER             PIC  9(04).
 01  WK-ST.
     03  JHO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3798B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3798B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3798B".
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
 01  PARA-KANRINO           PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-KANRINO.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFJOHOF.
     MOVE      "NFJOHOL1 "   TO   AB-FILE.
     MOVE      JHO-STATUS   TO   AB-STS.
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
     OPEN     I-O       NFJOHOF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     ACCEPT      SYS-TIME  FROM      TIME.
     MOVE        SYS-TIME  TO        SYS-TIMEW.
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
*    基本情報ファイルスタート
     MOVE        SPACE          TO   JHO-REC.
     INITIALIZE                      JHO-REC.
*
     MOVE        PARA-KANRINO   TO   JHO-F01.
     START  NFJOHOF  KEY  IS  >=  JHO-F01  JHO-F05  JHO-F06
                                  JHO-F07  JHO-F08  JHO-F09
            INVALID
            MOVE   "END"        TO   END-FLG
            DISPLAY NC"＃ＳＴＡＲＴ　ＲＥＡＤ＃" UPON CONS
            MOVE    4010        TO   PROGRAM-STATUS
            GO                  TO   INIT-EXIT
     END-START.
*    基本情報ファイル読み込み
     PERFORM NFJOHOF-READ-SEC.
*
     IF  END-FLG = "END"
            MOVE   "END"        TO   END-FLG
            DISPLAY NC"＃初回　　　　ＲＥＡＤ＃" UPON CONS
            MOVE    4010        TO   PROGRAM-STATUS
            GO                  TO   INIT-EXIT
     ELSE
            IF   JHO-F02  NOT =  99999999
                 MOVE   "END"        TO   END-FLG
                 DISPLAY NC"＃手書データ以外です。＃" UPON CONS
                 MOVE    4010        TO   PROGRAM-STATUS
                 GO                  TO   INIT-EXIT
            END-IF
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　基本情報ファイル読込
****************************************************************
 NFJOHOF-READ-SEC    SECTION.
*
     READ     NFJOHOF
              NEXT  AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  NFJOHOF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
     IF  READ-CNT(6:3) = "000" OR "500"
         DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*
     IF  PARA-KANRINO < JHO-F01
         MOVE     "END"    TO  END-FLG
     END-IF.
*
 NFJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    基本情報ファイルを削除する。
*    基本情報ファイル削除
     DELETE  NFJOHOF.
     ADD          1              TO   DEL-CNT.
 MAIN010.
*    基本情報ファイル読込
     PERFORM NFJOHOF-READ-SEC.
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
     DISPLAY "## NFJOHOF READ   CNT = " READ-CNT  UPON CONS.
     DISPLAY "## NFJOHOF DELETE CNT = " DEL-CNT   UPON CONS.
*
     CLOSE     NFJOHOF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
