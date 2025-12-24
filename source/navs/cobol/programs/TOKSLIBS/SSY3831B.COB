****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩシステム　　　　　　*
*    業務名　　　　　　　：　出荷処理　　　　　　　　　　　　　*
*    モジュール名　　　　：　ナフコ発注確定処理                *
*    　　　　　　　　　　　　（オンライン／本発）              *
*    作成日／更新日　　　：　2015/05/01                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　発注確定を行なった管理番号　　　　*
*                            (オンラインの場合は管理番号+作場)
*                            レコードを発注確定済みのステータス
*                            に更新する。
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3831B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/05/01.
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
     SELECT   NFJOHOL1  ASSIGN    TO        DA-01-VI-NFJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JOH-F01
                                            JOH-F05
                                            JOH-F06
                                            JOH-F07
                                            JOH-F08
                                            JOH-F09
                        FILE      STATUS    JOH-STATUS.
*本発基本情報ファイル
     SELECT   NHJOHOL1  ASSIGN    TO        DA-01-VI-NHJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HON-F01
                                            HON-F05
                                            HON-F06
                                            HON-F07
                                            HON-F08
                                            HON-F09
                        FILE      STATUS    HON-STATUS.
*数量訂正ファイル
     SELECT   NFSUTEL1  ASSIGN    TO        DA-01-VI-NFSUTEL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       STE-F01   STE-F05
                                            STE-F06   STE-F07
                                            STE-F08   STE-F09
                        FILE  STATUS   IS   STE-STATUS.
*箱数ファイル
     SELECT   NFHAKOL1  ASSIGN    TO        DA-01-VI-NFHAKOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HAK-F01   HAK-F05
                                            HAK-F06   HAK-F07
                                            HAK-F08
                        FILE STATUS    IS   HAK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    基本情報ファイル　　　
******************************************************************
 FD  NFJOHOL1           LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOL1  OF        XFDLIB
              JOINING   JOH       PREFIX.
******************************************************************
*    本発基本情報ファイル　　　
******************************************************************
 FD  NHJOHOL1           LABEL RECORD   IS   STANDARD.
     COPY     NHJOHOL1  OF        XFDLIB
              JOINING   HON       PREFIX.
******************************************************************
*    数量訂正ファイル　　　
******************************************************************
 FD  NFSUTEL1           LABEL RECORD   IS   STANDARD.
     COPY     NFSUTEL1  OF        XFDLIB
              JOINING   STE       PREFIX.
******************************************************************
*    箱数ファイル
******************************************************************
 FD  NFHAKOL1           LABEL RECORD   IS   STANDARD.
     COPY     NFHAKOL1  OF        XFDLIB
              JOINING   HAK  AS   PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  END-FLG1                PIC  X(03)     VALUE  SPACE.
 01  END-FLG2                PIC  X(03)     VALUE  SPACE.
 01  END-FLG3                PIC  X(03)     VALUE  SPACE.
 01  END-FLG4                PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT1           PIC  9(08)     VALUE  ZERO.
     03  READ-CNT2           PIC  9(08)     VALUE  ZERO.
     03  READ-CNT3           PIC  9(08)     VALUE  ZERO.
     03  READ-CNT4           PIC  9(08)     VALUE  ZERO.
     03  UPD-CNT1            PIC  9(08)     VALUE  ZERO.
     03  UPD-CNT2            PIC  9(08)     VALUE  ZERO.
     03  UPD-CNT3            PIC  9(08)     VALUE  ZERO.
     03  UPD-CNT4            PIC  9(08)     VALUE  ZERO.
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
     03  JOH-STATUS        PIC  X(02).
     03  HON-STATUS        PIC  X(02).
     03  STE-STATUS        PIC  X(02).
     03  HAK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3831B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3831B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3831B".
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
     03  MSG-UPDT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " UPDATE= ".
         05  UPDT-CNT       PIC   9(08).
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
*   確定種類（1：オンライン　2：本発）
 01  PARA-IN-SYURUI         PIC   X(01).
*   確定者所属部門
 01  PARA-IN-BUMONCD        PIC   X(04).
*   確定担当者
 01  PARA-IN-TANTOUCD       PIC   X(02).
*   確定管理番号
 01  PARA-IN-KANRINO        PIC   9(08).
*   確定作場（オンラインのみ）
 01  PARA-IN-SAKUBACD       PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-SYURUI
                                       PARA-IN-BUMONCD
                                       PARA-IN-TANTOUCD
                                       PARA-IN-KANRINO
                                       PARA-IN-SAKUBACD.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFJOHOL1.
     MOVE      "NFJOHOL1"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NHJOHOL1.
     MOVE      "NHJOHOL1"   TO   AB-FILE.
     MOVE      HON-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSUTEL1.
     MOVE      "NFSUTEL1"   TO   AB-FILE.
     MOVE      STE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFHAKOL1.
     MOVE      "NFHAKOL1"   TO   AB-FILE.
     MOVE      HAK-STATUS   TO   AB-STS.
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
     PERFORM  MAIN-SEC.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       NFJOHOL1  NHJOHOL1  NFSUTEL1  NFHAKOL1.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
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
*１．基本情報ファイル更新
     PERFORM NFJOHOL1-SEC.
*
*２．本発基本情報ファイル更新（本発の確定時のみ）
     IF  PARA-IN-SYURUI   = "2"
             PERFORM NHJOHOL1-SEC
     END-IF.
*
*３．数量訂正ファイル更新
     PERFORM NFSUTEL1-SEC.
*
*４．箱数ファイル更新
     PERFORM NFHAKOL1-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　１．【基本情報ファイル更新制御】
****************************************************************
 NFJOHOL1-SEC           SECTION.
*
     PERFORM  NFJOHOL1-START-SEC.
     IF  END-FLG1 = "END"
         DISPLAY NC"＃基本情報　データなし＃" UPON CONS
         MOVE    4010        TO   PROGRAM-STATUS
         STOP    RUN
     END-IF.
*    基本情報ファイル読込
     PERFORM  NFJOHOL1-READ-SEC.
     IF  END-FLG1 = "END"
         DISPLAY NC"＃基本情報　データなし＃" UPON CONS
         MOVE    4010        TO   PROGRAM-STATUS
         STOP    RUN
     END-IF.
*    繰返更新する。
     PERFORM  NFJOHOL1-REWRITE-SEC  UNTIL  END-FLG1 = "END".
*
 NFJOHOL1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　基本情報ファイルスタート
*--------------------------------------------------------------*
 NFJOHOL1-START-SEC   SECTION.
*
*    基本情報ファイルスタート
     MOVE        SPACE         TO  JOH-REC.
     INITIALIZE                    JOH-REC.
*
     MOVE   PARA-IN-KANRINO    TO  JOH-F01.
     MOVE   PARA-IN-SAKUBACD   TO  JOH-F05.
     START  NFJOHOL1  KEY  IS  >=  JOH-F01  JOH-F05  JOH-F06
                                   JOH-F07  JOH-F08  JOH-F09
            INVALID
            MOVE   "END"        TO   END-FLG1
     END-START.
*
 NFJOHOL1-START-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　基本情報ファイル読込
*--------------------------------------------------------------*
 NFJOHOL1-READ-SEC    SECTION.
*
     READ     NFJOHOL1
              NEXT  AT  END
                  MOVE     "END"    TO  END-FLG1
                  GO                TO  NFJOHOL1-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT1
     END-READ.
*
     IF  READ-CNT1(6:3) = "000" OR "500"
         DISPLAY "READ-CNT1 = " READ-CNT1 UPON CONS
     END-IF.
*
     IF  PARA-IN-KANRINO < JOH-F01
         MOVE     "END"    TO  END-FLG1
         GO                TO  NFJOHOL1-READ-EXIT
     END-IF.
*倉庫ＣＤのチェック（オンラインのみ）
     IF  PARA-IN-SYURUI       =  "1"
         IF  PARA-IN-SAKUBACD  =  JOH-F05
             CONTINUE
         ELSE
             MOVE "END"    TO  END-FLG1
             GO            TO  NFJOHOL1-READ-EXIT
         END-IF
     END-IF.
*
 NFJOHOL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　基本情報ファイル更新
*--------------------------------------------------------------*
 NFJOHOL1-REWRITE-SEC    SECTION.
*
*   発注確定済ＦＬＧ
     MOVE    "1"                 TO   JOH-F34.
*   発注確定処理日
     MOVE     SYS-DATEW          TO   JOH-F35.
*   発注確定処理時刻
     MOVE     SYS-TIME(1:6)      TO   JOH-F36.
*   発注確定者所属部門
     MOVE     PARA-IN-BUMONCD    TO   JOH-F37.
*   発注確定担当者
     MOVE     PARA-IN-TANTOUCD   TO   JOH-F38.
*
     REWRITE  JOH-REC.
*
     ADD     1                   TO   UPD-CNT1.
*
     PERFORM NFJOHOL1-READ-SEC.
*
 NFJOHOL1-REWRITE-EXIT.
     EXIT.
****************************************************************
*　２．【本発基本情報ファイル更新制御】
****************************************************************
 NHJOHOL1-SEC           SECTION.
*
     PERFORM  NHJOHOL1-START-SEC.
     IF  END-FLG2 = "END"
         DISPLAY NC"＃本発基本　データなし＃" UPON CONS
         MOVE    4010        TO   PROGRAM-STATUS
         STOP    RUN
     END-IF.
*    本発基本情報ファイル読込
     PERFORM  NHJOHOL1-READ-SEC.
     IF  END-FLG2 = "END"
         DISPLAY NC"＃本発基本　データなし＃" UPON CONS
         MOVE    4010        TO   PROGRAM-STATUS
         STOP    RUN
     END-IF.
*    繰返更新する。
     PERFORM  NHJOHOL1-REWRITE-SEC  UNTIL  END-FLG2 = "END".
*
 NHJOHOL1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　本発基本情報ファイルスタート
*--------------------------------------------------------------*
 NHJOHOL1-START-SEC   SECTION.
*
*    本発基本情報ファイルスタート
     MOVE        SPACE         TO  HON-REC.
     INITIALIZE                    HON-REC.
*
     MOVE   PARA-IN-KANRINO    TO  HON-F01.
     MOVE   PARA-IN-SAKUBACD   TO  HON-F05.
     START  NHJOHOL1  KEY  IS  >=  HON-F01  HON-F05  HON-F06
                                   HON-F07  HON-F08  HON-F09
            INVALID
            MOVE   "END"        TO   END-FLG2
     END-START.
*
 NHJOHOL1-START-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　本発基本情報ファイル読込
*--------------------------------------------------------------*
 NHJOHOL1-READ-SEC    SECTION.
*
     READ     NHJOHOL1
              NEXT  AT  END
                  MOVE     "END"    TO  END-FLG2
                  GO                TO  NHJOHOL1-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT2
     END-READ.
*
     IF  READ-CNT2(6:3) = "000" OR "500"
         DISPLAY "READ-CNT2 = " READ-CNT2 UPON CONS
     END-IF.
*
     IF  PARA-IN-KANRINO < HON-F01
         MOVE     "END"    TO  END-FLG2
         GO                TO  NHJOHOL1-READ-EXIT
     END-IF.
*倉庫ＣＤのチェック（オンラインのみ）
     IF  PARA-IN-SYURUI       =  "1"
         IF  PARA-IN-SAKUBACD  =  HON-F05
             CONTINUE
         ELSE
             MOVE "END"    TO  END-FLG2
             GO            TO  NHJOHOL1-READ-EXIT
         END-IF
     END-IF.
*
 NHJOHOL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　本発基本情報ファイル更新
*--------------------------------------------------------------*
 NHJOHOL1-REWRITE-SEC    SECTION.
*
*   発注確定済ＦＬＧ
     MOVE    "1"                 TO   HON-F34.
*   発注確定処理日
     MOVE     SYS-DATEW          TO   HON-F35.
*   発注確定処理時刻
     MOVE     SYS-TIME(1:6)      TO   HON-F36.
*   発注確定者所属部門
     MOVE     PARA-IN-BUMONCD    TO   HON-F37.
*   発注確定担当者
     MOVE     PARA-IN-TANTOUCD   TO   HON-F38.
*
     REWRITE  HON-REC.
*
     ADD     1                   TO   UPD-CNT2.
*
     PERFORM NHJOHOL1-READ-SEC.
*
 NHJOHOL1-REWRITE-EXIT.
     EXIT.
****************************************************************
*　３．【数量訂正ファイル更新制御】
****************************************************************
 NFSUTEL1-SEC           SECTION.
*
     PERFORM  NFSUTEL1-START-SEC.
     IF  END-FLG3 = "END"
         DISPLAY NC"＃数量訂正　データなし＃" UPON CONS
         MOVE    4010        TO   PROGRAM-STATUS
         STOP    RUN
     END-IF.
*    数量訂正ファイル読込
     PERFORM  NFSUTEL1-READ-SEC.
     IF  END-FLG3 = "END"
         DISPLAY NC"＃数量訂正　データなし＃" UPON CONS
         MOVE    4010        TO   PROGRAM-STATUS
         STOP    RUN
     END-IF.
*    繰返更新する。
     PERFORM  NFSUTEL1-REWRITE-SEC  UNTIL  END-FLG3 = "END".
*
 NFSUTEL1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　数量訂正ファイルスタート
*--------------------------------------------------------------*
 NFSUTEL1-START-SEC   SECTION.
*
*    数量訂正ファイルスタート
     MOVE        SPACE         TO  STE-REC.
     INITIALIZE                    STE-REC.
*
     MOVE   PARA-IN-KANRINO    TO  STE-F01.
     MOVE   PARA-IN-SAKUBACD   TO  STE-F05.
     START  NFSUTEL1  KEY  IS  >=  STE-F01  STE-F05  STE-F06
                                   STE-F07  STE-F08  STE-F09
            INVALID
            MOVE   "END"        TO   END-FLG3
     END-START.
*
 NFSUTEL1-START-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　数量訂正ファイル読込
*--------------------------------------------------------------*
 NFSUTEL1-READ-SEC    SECTION.
*
     READ     NFSUTEL1
              NEXT  AT  END
                  MOVE     "END"    TO  END-FLG3
                  GO                TO  NFSUTEL1-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT3
     END-READ.
*
     IF  READ-CNT3(6:3) = "000" OR "500"
         DISPLAY "READ-CNT3 = " READ-CNT3 UPON CONS
     END-IF.
*
     IF  PARA-IN-KANRINO < STE-F01
         MOVE     "END"    TO  END-FLG3
         GO                TO  NFSUTEL1-READ-EXIT
     END-IF.
*倉庫ＣＤのチェック（オンラインのみ）
     IF  PARA-IN-SYURUI       =  "1"
         IF  PARA-IN-SAKUBACD  =  STE-F05
             CONTINUE
         ELSE
             MOVE "END"    TO  END-FLG3
             GO            TO  NFSUTEL1-READ-EXIT
         END-IF
     END-IF.
*
 NFSUTEL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　数量訂正ファイル更新
*--------------------------------------------------------------*
 NFSUTEL1-REWRITE-SEC    SECTION.
*
*   発注確定済ＦＬＧ
     MOVE    "1"                 TO   STE-F91.
*   発注確定処理日
     MOVE     SYS-DATEW          TO   STE-F92.
*   発注確定処理時刻
     MOVE     SYS-TIME(1:6)      TO   STE-F93.
*   発注確定者所属部門
     MOVE     PARA-IN-BUMONCD    TO   STE-F94.
*   発注確定担当者
     MOVE     PARA-IN-TANTOUCD   TO   STE-F95.
*
     REWRITE  STE-REC.
*
     ADD     1                   TO   UPD-CNT3.
*
     PERFORM NFSUTEL1-READ-SEC.
*
 NFSUTEL1-REWRITE-EXIT.
     EXIT.
****************************************************************
*　４．【箱数ファイル更新制御】
****************************************************************
 NFHAKOL1-SEC           SECTION.
*
     PERFORM  NFHAKOL1-START-SEC.
     IF  END-FLG4 = "END"
         DISPLAY NC"＃箱数　　　データなし＃" UPON CONS
         MOVE    4010        TO   PROGRAM-STATUS
         STOP    RUN
     END-IF.
*    箱数ファイル読込
     PERFORM  NFHAKOL1-READ-SEC.
     IF  END-FLG4 = "END"
         DISPLAY NC"＃箱数　　　データなし＃" UPON CONS
         MOVE    4010        TO   PROGRAM-STATUS
         STOP    RUN
     END-IF.
*    繰返更新する。
     PERFORM  NFHAKOL1-REWRITE-SEC  UNTIL  END-FLG4 = "END".
*
 NFHAKOL1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　箱数ファイルスタート
*--------------------------------------------------------------*
 NFHAKOL1-START-SEC   SECTION.
*
*    箱数ファイルスタート
     MOVE        SPACE          TO   HAK-REC.
     INITIALIZE                      HAK-REC.
*
     MOVE   PARA-IN-KANRINO     TO   HAK-F01.
     MOVE   PARA-IN-SAKUBACD    TO   HAK-F05.
     START  NFHAKOL1  KEY  IS   >=   HAK-F01  HAK-F05  HAK-F06
                                     HAK-F07  HAK-F08
            INVALID
            MOVE   "END"        TO   END-FLG4
     END-START.
*
 NFHAKOL1-START-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　箱数ファイル読込
*--------------------------------------------------------------*
 NFHAKOL1-READ-SEC    SECTION.
*
     READ     NFHAKOL1
              NEXT  AT  END
                  MOVE     "END"    TO  END-FLG4
                  GO                TO  NFHAKOL1-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT4
     END-READ.
*
     IF  READ-CNT4(6:3) = "000" OR "500"
         DISPLAY "READ-CNT4 = " READ-CNT4 UPON CONS
     END-IF.
*
     IF  PARA-IN-KANRINO < HAK-F01
         MOVE     "END"    TO  END-FLG4
         GO                TO  NFHAKOL1-READ-EXIT
     END-IF.
*倉庫ＣＤのチェック（オンラインのみ）
     IF  PARA-IN-SYURUI       =  "1"
         IF  PARA-IN-SAKUBACD  =  HAK-F05
             CONTINUE
         ELSE
             MOVE "END"    TO  END-FLG4
             GO            TO  NFHAKOL1-READ-EXIT
         END-IF
     END-IF.
*
 NFHAKOL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　箱数ファイル更新
*--------------------------------------------------------------*
 NFHAKOL1-REWRITE-SEC    SECTION.
*
*   発注確定済ＦＬＧ
     MOVE    "1"                 TO   HAK-F93.
*   発注確定処理日
     MOVE     SYS-DATEW          TO   HAK-F94.
*   発注確定処理時刻
     MOVE     SYS-TIME(1:6)      TO   HAK-F95.
*   発注確定者所属部門
     MOVE     PARA-IN-BUMONCD    TO   HAK-F96.
*   発注確定担当者
     MOVE     PARA-IN-TANTOUCD   TO   HAK-F97.
*
     REWRITE  HAK-REC.
*
     ADD     1                   TO   UPD-CNT4.
*
     PERFORM NFHAKOL1-READ-SEC.
*
 NFHAKOL1-REWRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "## NFJOHOL1 READ   CNT = " READ-CNT1 UPON CONS.
     DISPLAY "## NFJOHOL1 FLGUPD CNT = " UPD-CNT1  UPON CONS.
     DISPLAY "## NHJOHOL1 READ   CNT = " READ-CNT2 UPON CONS.
     DISPLAY "## NHJOHOL1 FLGUPD CNT = " UPD-CNT2  UPON CONS.
     DISPLAY "## NFSUTEL1 READ   CNT = " READ-CNT3 UPON CONS.
     DISPLAY "## NFSUTEL1 FLGUPD CNT = " UPD-CNT3  UPON CONS.
     DISPLAY "## NFHAKOL1 READ   CNT = " READ-CNT4 UPON CONS.
     DISPLAY "## NFHAKOL1 FLGUPD CNT = " UPD-CNT4  UPON CONS.
*
     CLOSE     NFJOHOL1  NHJOHOL1  NFSUTEL1  NFHAKOL1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
