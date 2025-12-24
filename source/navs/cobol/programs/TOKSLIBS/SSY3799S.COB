****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩシステム　　　　　　*
*    業務名　　　　　　　：　出荷処理　　　　　　　　　　　　　*
*    モジュール名　　　　：　数量確定／箱確定ファイルＦＬＧ解除*
*    作成日／更新日　　　：　2012/01/10                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　指定された管理番号で数量確定／箱　*
*                            確定ファイルの送信ＦＬＧを解除する*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3799S.
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
*数量訂正ファイル
     SELECT   NFSUTEF   ASSIGN    TO        DA-01-VI-NFSUTEL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       STE-F01   STE-F05
                                            STE-F08   STE-F06
                                            STE-F07   STE-F09
                        FILE  STATUS   IS   STE-STATUS.
*箱数ファイル
     SELECT   NFHAKOF   ASSIGN    TO        DA-01-VI-NFHAKOL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HAK-F01   HAK-F05
                                            HAK-F08   HAK-F06
                                            HAK-F07
                        FILE STATUS    IS   HAK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    数量訂正ファイル　　　
******************************************************************
 FD  NFSUTEF            LABEL RECORD   IS   STANDARD.
     COPY     NFSUTEF   OF        XFDLIB
              JOINING   STE       PREFIX.
******************************************************************
*    出荷情報エラーファイル
******************************************************************
 FD  NFHAKOF
                        LABEL RECORD   IS   STANDARD.
     COPY     NFHAKOF   OF        XFDLIB
              JOINING   HAK  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  END-FLG1                PIC  X(03)     VALUE  SPACE.
 01  END-FLG2                PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT1           PIC  9(08)     VALUE  ZERO.
     03  READ-CNT2           PIC  9(08)     VALUE  ZERO.
     03  UPD-CNT1            PIC  9(08)     VALUE  ZERO.
     03  UPD-CNT2            PIC  9(08)     VALUE  ZERO.
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
     03  STE-STATUS        PIC  X(02).
     03  HAK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3799S".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3799S".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3799S".
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
 01  PARA-SOKCD             PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-KANRINO
                                       PARA-SOKCD.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSUTEF.
     MOVE      "NFSUTEL4"   TO   AB-FILE.
     MOVE      STE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFHAKOF.
     MOVE      "NFHAKOL4"   TO   AB-FILE.
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
     OPEN     I-O       NFSUTEF  NFHAKOF.
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
*　　数量訂正ファイル読込
****************************************************************
 NFSUTEF-START-SEC   SECTION.
*
*    数量訂正ファイルスタート
     MOVE        SPACE          TO   STE-REC.
     INITIALIZE                      STE-REC.
*
     MOVE        PARA-KANRINO   TO   STE-F01.
     MOVE        PARA-SOKCD     TO   STE-F05.
     START  NFSUTEF  KEY  IS  >=  STE-F01  STE-F05  STE-F08
                                  STE-F06  STE-F07  STE-F09
            INVALID
            MOVE   "END"        TO   END-FLG1
     END-START.
*
 NFSUTEF-START-EXIT.
     EXIT.
****************************************************************
*　　数量訂正ファイル読込
****************************************************************
 NFSUTEF-READ-SEC    SECTION.
*
     READ     NFSUTEF
              NEXT  AT  END
                  MOVE     "END"    TO  END-FLG1
                  GO                TO  NFSUTEF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT1
     END-READ.
*
     IF  READ-CNT1(6:3) = "000" OR "500"
         DISPLAY "READ-CNT1 = " READ-CNT1 UPON CONS
     END-IF.
*
     IF  PARA-KANRINO < STE-F01
         MOVE     "END"    TO  END-FLG1
         GO                TO  NFSUTEF-READ-EXIT
     END-IF.
*倉庫ＣＤのチェック
     IF  PARA-SOKCD  NOT =  SPACE
         IF  PARA-SOKCD  =  STE-F05
             CONTINUE
         ELSE
             MOVE "END"    TO  END-FLG1
             GO            TO  NFSUTEF-READ-EXIT
         END-IF
     END-IF.
*
 NFSUTEF-READ-EXIT.
     EXIT.
****************************************************************
*　　箱確定ファイル読込
****************************************************************
 NFHAKOF-START-SEC   SECTION.
*
*    数量訂正ファイルスタート
     MOVE        SPACE          TO   HAK-REC.
     INITIALIZE                      HAK-REC.
*
     MOVE        PARA-KANRINO   TO   HAK-F01.
     MOVE        PARA-SOKCD     TO   HAK-F05.
     START  NFHAKOF  KEY  IS  >=  HAK-F01  HAK-F05  HAK-F08
                                  HAK-F06  HAK-F07
            INVALID
            MOVE   "END"        TO   END-FLG2
     END-START.
*
 NFHAKOF-START-EXIT.
     EXIT.
****************************************************************
*　　箱確定ファイル読込
****************************************************************
 NFHAKOF-READ-SEC    SECTION.
*
     READ     NFHAKOF
              NEXT  AT  END
                  MOVE     "END"    TO  END-FLG2
                  GO                TO  NFHAKOF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT2
     END-READ.
*
     IF  READ-CNT2(6:3) = "000" OR "500"
         DISPLAY "READ-CNT2 = " READ-CNT2 UPON CONS
     END-IF.
*
     IF  PARA-KANRINO < HAK-F01
         MOVE     "END"    TO  END-FLG2
         GO                TO  NFHAKOF-READ-EXIT
     END-IF.
*倉庫ＣＤのチェック
     IF  PARA-SOKCD  NOT =  SPACE
         IF  PARA-SOKCD  =  HAK-F05
             CONTINUE
         ELSE
             MOVE "END"    TO  END-FLG2
             GO            TO  NFHAKOF-READ-EXIT
         END-IF
     END-IF.
*
 NFHAKOF-READ-EXIT.
     EXIT.
****************************************************************
*　　数量確定ファイル解除制御
****************************************************************
 NFSUTEF-SEC           SECTION.
*
     PERFORM  NFSUTEF-START-SEC.
     IF  END-FLG1 = "END"
         DISPLAY NC"＃数量訂正　スタート　＃" UPON CONS
         MOVE    4010        TO   PROGRAM-STATUS
         GO                  TO   NFSUTEF-EXIT
     END-IF.
*    数量確定ファイル読込
     PERFORM  NFSUTEF-READ-SEC.
*    繰返削除する。
     PERFORM  NFSUTEF-REWRITE-SEC  UNTIL  END-FLG1 = "END".
*
 NFSUTEF-EXIT.
     EXIT.
****************************************************************
*　　数量確定ファイル解除
****************************************************************
 NFSUTEF-REWRITE-SEC    SECTION.
*
     MOVE    SPACE               TO   STE-F98.
     MOVE    ZERO                TO   STE-F99.
     REWRITE  STE-REC.
*
     ADD     1                   TO   UPD-CNT1.
*
     PERFORM NFSUTEF-READ-SEC.
*
 NFSUTEF-REWRITE-EXIT.
     EXIT.
****************************************************************
*　　数量確定ファイル解除制御
****************************************************************
 NFHAKOF-SEC           SECTION.
*
     PERFORM  NFHAKOF-START-SEC.
     IF  END-FLG2 = "END"
         DISPLAY NC"＃箱訂正　　スタート　＃" UPON CONS
         MOVE    4010        TO   PROGRAM-STATUS
         GO                  TO   NFHAKOF-EXIT
     END-IF.
*    数量確定ファイル読込
     PERFORM  NFHAKOF-READ-SEC.
*    繰返削除する。
     PERFORM  NFHAKOF-REWRITE-SEC  UNTIL  END-FLG2 = "END".
*
 NFHAKOF-EXIT.
     EXIT.
****************************************************************
*　　数量確定ファイル解除
****************************************************************
 NFHAKOF-REWRITE-SEC    SECTION.
*
     MOVE    SPACE               TO   HAK-F98.
     MOVE    ZERO                TO   HAK-F99.
     REWRITE  HAK-REC.
*
     ADD     1                   TO   UPD-CNT2.
*
     PERFORM NFHAKOF-READ-SEC.
*
 NFHAKOF-REWRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     PERFORM NFSUTEF-SEC.
*
     PERFORM NFHAKOF-SEC.
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
     DISPLAY "## NFSUTEF READ   CNT = " READ-CNT1 UPON CONS.
     DISPLAY "## NFSUTEF FLGUPD CNT = " UPD-CNT1  UPON CONS.
     DISPLAY "## NFHAKOF READ   CNT = " READ-CNT2 UPON CONS.
     DISPLAY "## NFHAKOF FLGUPD CNT = " UPD-CNT2  UPON CONS.
*
     CLOSE     NFSUTEF  NFHAKOF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
