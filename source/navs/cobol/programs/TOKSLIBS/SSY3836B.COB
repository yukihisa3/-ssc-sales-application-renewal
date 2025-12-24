****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　発注業務　　　　　　　　　　　　　*
*    モジュール名　　　　：　ナフコ発注確定取消し処理　　　　　*
*    作成日／更新日　　　：　2015/06/15                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　発注確定を行なった管理番号　　　　*
*                           （オンラインの場合は管理番号+作場)
*                            を発注未確定状態に戻す。
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3836B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/06/15.
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
     SELECT   NFSUTEL4  ASSIGN    TO        DA-01-VI-NFSUTEL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       STE-F01   STE-F05
                                            STE-F08   STE-F06
                                            STE-F07   STE-F09
                        FILE  STATUS   IS   STE-STATUS.
*箱数ファイル
     SELECT   NFHAKOL4  ASSIGN    TO        DA-01-VI-NFHAKOL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HAK-F01   HAK-F05
                                            HAK-F08   HAK-F06
                                            HAK-F07
                        FILE STATUS    IS   HAK-STATUS.
*本発基本情報ファイル
     SELECT   NHJOHOL1  ASSIGN    TO        DA-01-VI-NHJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       NHJ-F01   NHJ-F05
                                            NHJ-F06   NHJ-F07
                                            NHJ-F08   NHJ-F09
                        FILE      STATUS    NHJ-STATUS.
*基本情報ファイル
     SELECT   NFJOHOL1  ASSIGN    TO        DA-01-VI-NFJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       NFJ-F01   NFJ-F05
                                            NFJ-F06   NFJ-F07
                                            NFJ-F08   NFJ-F09
                        FILE      STATUS    NFJ-STATUS.
*売上伝票ファイル
     SELECT   SHTDENL1  ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE STATUS    IS   DEN-STATUS.
*商品在庫マスタ
     SELECT   ZAMZAIL1  ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       ZAI-F01
                                            ZAI-F02
                                            ZAI-F03
                        FILE      STATUS    ZAI-STATUS.
*商品コード変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SHO-F01
                                            SHO-F02
                        FILE      STATUS    SHO-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    数量訂正ファイル　　　
******************************************************************
 FD  NFSUTEL4           LABEL RECORD   IS   STANDARD.
     COPY     NFSUTEL4  OF        XFDLIB
              JOINING   STE       PREFIX.
******************************************************************
*    出荷情報エラーファイル
******************************************************************
 FD  NFHAKOL4           LABEL RECORD   IS   STANDARD.
     COPY     NFHAKOL4  OF        XFDLIB
              JOINING   HAK  AS   PREFIX.
******************************************************************
*    本発基本情報ファイル
******************************************************************
 FD  NHJOHOL1
     LABEL       RECORD      IS        STANDARD.
     COPY        NHJOHOL1    OF        XFDLIB
     JOINING     NHJ         AS        PREFIX.
******************************************************************
*基本情報ファイル
******************************************************************
 FD  NFJOHOL1
     LABEL       RECORD      IS        STANDARD.
     COPY        NFJOHOL1    OF        XFDLIB
     JOINING     NFJ         AS        PREFIX.
******************************************************************
*売上伝票ファイル
******************************************************************
 FD  SHTDENL1
     LABEL       RECORD      IS        STANDARD.
     COPY        SHTDENL1    OF        XFDLIB
     JOINING     DEN         AS        PREFIX.
******************************************************************
*商品在庫マスタ
******************************************************************
 FD  ZAMZAIL1
     LABEL       RECORD      IS        STANDARD.
     COPY        ZAMZAIL1    OF        XFDLIB
     JOINING     ZAI         AS        PREFIX.
******************************************************************
*商品変換テーブル
******************************************************************
 FD  SHOTBL1
     LABEL       RECORD      IS        STANDARD.
     COPY        SHOTBL1     OF        XFDLIB
     JOINING     SHO         AS        PREFIX.
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
     03  READ-CNT3           PIC  9(08)     VALUE  ZERO.
     03  READ-CNT4           PIC  9(08)     VALUE  ZERO.
     03  READ-CNT5           PIC  9(08)     VALUE  ZERO.
     03  READ-CNT6           PIC  9(08)     VALUE  ZERO.
     03  UPD-CNT1            PIC  9(08)     VALUE  ZERO.
     03  UPD-CNT2            PIC  9(08)     VALUE  ZERO.
     03  UPD-CNT3            PIC  9(08)     VALUE  ZERO.
     03  UPD-CNT4            PIC  9(08)     VALUE  ZERO.
     03  UPD-CNT6            PIC  9(08)     VALUE  ZERO.
     03  DEL-CNT4            PIC  9(08)     VALUE  ZERO.
     03  DEL-CNT5            PIC  9(08)     VALUE  ZERO.
 01  WK-FLG.
     03  SHOTBL1-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENL1-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  KEP-FLG             PIC  X(01)     VALUE  SPACE.
*計算領域
 01  WRK-AREA.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*
*ワーク項目
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
     03  SYS-TIME          PIC 9(08).
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
 01  SYS-TIMEW.
     03  SYS-TIMEHM        PIC  9(04).
     03  FILER             PIC  9(04).
 01  WK-ST.
     03  STE-STATUS        PIC  X(02).
     03  HAK-STATUS        PIC  X(02).
     03  NHJ-STATUS        PIC  X(02).
     03  NFJ-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  ZAI-STATUS        PIC  X(02).
     03  SHO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3836B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3836B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3836B".
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
 01  LINK-IN-SYURUI         PIC   X(01).
 01  LINK-IN-KANRINO        PIC   9(08).
 01  LINK-IN-SAKUBACD       PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING LINK-IN-SYURUI
                                       LINK-IN-KANRINO
                                       LINK-IN-SAKUBACD.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSUTEL4.
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
                        PROCEDURE   NFHAKOL4.
     MOVE      "NFHAKOL4"   TO   AB-FILE.
     MOVE      HAK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NHJOHOL1.
     MOVE      "NHJOHOL1"   TO   AB-FILE.
     MOVE      NHJ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFJOHOL1.
     MOVE      "NFJOHOL1"   TO   AB-FILE.
     MOVE      NFJ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENL1.
     MOVE      "SHTDENL1"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZAMZAIL1.
     MOVE      "ZAMZAIL1"   TO   AB-FILE.
     MOVE      ZAI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1 "   TO   AB-FILE.
     MOVE      SHO-STATUS   TO   AB-STS.
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
*
     OPEN     I-O       NFSUTEL4  NFHAKOL4  NHJOHOL1  NFJOHOL1
                        SHTDENL1  ZAMZAIL1.
     OPEN     INPUT     SHOTBL1.
*
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
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW WK-DATE8
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
*数量訂正ファイル取消制御
     PERFORM NFSUTEL4-SEC.
*箱数ファイル取消制御
     PERFORM NFHAKOL4-SEC.
*本発基本情報ファイル取消制御（本発指定時のみ）
     MOVE    SPACE   TO    END-FLG1.
     IF      LINK-IN-SYURUI  =  "2"
             PERFORM NHJOHOL1-SEC
     END-IF.
*基本情報・売上・在庫　取消制御
     MOVE    SPACE   TO    END-FLG1.
     PERFORM NFJOHOL1-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　数量訂正ファイル取消制御
****************************************************************
 NFSUTEL4-SEC           SECTION.
*
     MOVE    "NFSUTEL4-SEC" TO   S-NAME.
*
     PERFORM  NFSUTEL4-START-SEC.
     IF  END-FLG1 = "END"
         DISPLAY NC"＃数量訂正データなし　＃" UPON CONS
         MOVE    4010        TO   PROGRAM-STATUS
         GO                  TO   NFSUTEL4-EXIT
     END-IF.
*    数量訂正ファイル読込
     PERFORM  NFSUTEL4-READ-SEC.
*    繰返取消する。
     PERFORM  NFSUTEL4-REWRITE-SEC  UNTIL  END-FLG1 = "END".
*
 NFSUTEL4-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　数量訂正ファイルスタート
*--------------------------------------------------------------*
 NFSUTEL4-START-SEC   SECTION.
*
     MOVE     "NFSUTEL4-START-SEC" TO   S-NAME.
*
*    数量訂正ファイルスタート
     MOVE        SPACE          TO   STE-REC.
     INITIALIZE                      STE-REC.
*
     MOVE   LINK-IN-KANRINO     TO   STE-F01.
     MOVE   LINK-IN-SAKUBACD    TO   STE-F05.
     START  NFSUTEL4  KEY   IS  >=   STE-F01  STE-F05  STE-F08
                                     STE-F06  STE-F07  STE-F09
            INVALID
            MOVE   "END"        TO   END-FLG1
     END-START.
*
 NFSUTEL4-START-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　数量訂正ファイル読込
*--------------------------------------------------------------*
 NFSUTEL4-READ-SEC    SECTION.
*
     MOVE     "NFSUTEL4-READ-SEC" TO   S-NAME.
*
     READ     NFSUTEL4
              NEXT  AT  END
                  MOVE     "END"    TO  END-FLG1
                  GO                TO  NFSUTEL4-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT1
     END-READ.
*
     IF  READ-CNT1(6:3) = "000" OR "500"
         DISPLAY "READ-CNT1 = " READ-CNT1 UPON CONS
     END-IF.
*
     IF  LINK-IN-KANRINO < STE-F01
         MOVE     "END"    TO  END-FLG1
     END-IF.
     IF  LINK-IN-SYURUI  = "1"
         IF  LINK-IN-SAKUBACD < STE-F05
             MOVE     "END"    TO  END-FLG1
         END-IF
     END-IF.
*
 NFSUTEL4-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　数量訂正ファイル取消
*--------------------------------------------------------------*
 NFSUTEL4-REWRITE-SEC    SECTION.
*
     MOVE   "NFSUTEL4-REWRITE-SEC" TO   S-NAME.
*
     MOVE    SPACE               TO   STE-F91.
     MOVE    ZERO                TO   STE-F92.
     MOVE    ZERO                TO   STE-F93.
     MOVE    SPACE               TO   STE-F94.
     MOVE    SPACE               TO   STE-F95.
     REWRITE  STE-REC.
*
     ADD     1                   TO   UPD-CNT1.
*
     PERFORM NFSUTEL4-READ-SEC.
*
 NFSUTEL4-REWRITE-EXIT.
     EXIT.
****************************************************************
*　　箱数ファイル取消制御
****************************************************************
 NFHAKOL4-SEC           SECTION.
*
     MOVE     "NFHAKOL4-SEC" TO   S-NAME.
*
     PERFORM  NFHAKOL4-START-SEC.
     IF  END-FLG2 = "END"
         DISPLAY NC"＃箱数データなし　　　＃" UPON CONS
         MOVE    4010        TO   PROGRAM-STATUS
         GO                  TO   NFHAKOL4-EXIT
     END-IF.
*    箱数ファイル読込
     PERFORM  NFHAKOL4-READ-SEC.
*    繰返取消する。
     PERFORM  NFHAKOL4-REWRITE-SEC  UNTIL  END-FLG2 = "END".
*
 NFHAKOL4-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　箱数ファイルスタート
*--------------------------------------------------------------*
 NFHAKOL4-START-SEC   SECTION.
*
     MOVE     "NFHAKOL4-START-SEC" TO   S-NAME.
*
*    箱数ファイルスタート
     MOVE        SPACE          TO   HAK-REC.
     INITIALIZE                      HAK-REC.
*
     MOVE   LINK-IN-KANRINO     TO   HAK-F01.
     MOVE   LINK-IN-SAKUBACD    TO   HAK-F05.
     START  NFHAKOL4  KEY  IS   >=   HAK-F01  HAK-F05  HAK-F08
                                     HAK-F06  HAK-F07
            INVALID
            MOVE   "END"        TO   END-FLG2
     END-START.
*
 NFHAKOL4-START-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　箱数ファイル読込
*--------------------------------------------------------------*
 NFHAKOL4-READ-SEC    SECTION.
*
     MOVE     "NFHAKOL4-READ-SEC" TO   S-NAME.
*
     READ     NFHAKOL4
              NEXT  AT  END
                  MOVE     "END"    TO  END-FLG2
                  GO                TO  NFHAKOL4-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT2
     END-READ.
*
     IF  READ-CNT2(6:3) = "000" OR "500"
         DISPLAY "READ-CNT2 = " READ-CNT2 UPON CONS
     END-IF.
*
     IF  LINK-IN-KANRINO < HAK-F01
         MOVE     "END"    TO  END-FLG2
     END-IF.
     IF  LINK-IN-SYURUI  = "1"
         IF  LINK-IN-SAKUBACD < HAK-F05
             MOVE     "END"    TO  END-FLG2
         END-IF
     END-IF.
*
 NFHAKOL4-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　箱数ファイル取消
*--------------------------------------------------------------*
 NFHAKOL4-REWRITE-SEC    SECTION.
*
     MOVE   "NFHAKOL4-REWRITE-SEC" TO   S-NAME.
*
     MOVE    SPACE               TO   HAK-F93.
     MOVE    ZERO                TO   HAK-F94.
     MOVE    ZERO                TO   HAK-F95.
     MOVE    SPACE               TO   HAK-F96.
     MOVE    SPACE               TO   HAK-F97.
     REWRITE  HAK-REC.
*
     ADD     1                   TO   UPD-CNT2.
*
     PERFORM NFHAKOL4-READ-SEC.
*
 NFHAKOL4-REWRITE-EXIT.
     EXIT.
****************************************************************
*　　本発基本情報ファイル取消制御
****************************************************************
 NHJOHOL1-SEC           SECTION.
*
     MOVE    "NHJOHOL1-SEC" TO   S-NAME.
*
     PERFORM  NHJOHOL1-START-SEC.
     IF  END-FLG1 = "END"
         DISPLAY NC"＃本発基本データなし　＃" UPON CONS
         MOVE    4010        TO   PROGRAM-STATUS
         GO                  TO   NHJOHOL1-EXIT
     END-IF.
*    本発基本ファイル読込
     PERFORM  NHJOHOL1-READ-SEC.
*    繰返取消する。
     PERFORM  NHJOHOL1-REWRITE-SEC  UNTIL  END-FLG1 = "END".
*
 NHJOHOL1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　本発基本ファイルスタート
*--------------------------------------------------------------*
 NHJOHOL1-START-SEC   SECTION.
*
     MOVE     "NHJOHOL1-START-SEC" TO   S-NAME.
*
*    本発基本ファイルスタート
     MOVE        SPACE          TO   NHJ-REC.
     INITIALIZE                      NHJ-REC.
*
     MOVE   LINK-IN-KANRINO     TO   NHJ-F01.
     MOVE   LINK-IN-SAKUBACD    TO   NHJ-F05.
     START  NHJOHOL1  KEY   IS  >=   NHJ-F01  NHJ-F05  NHJ-F06
                                     NHJ-F07  NHJ-F08  NHJ-F09
            INVALID
            MOVE   "END"        TO   END-FLG1
     END-START.
*
 NHJOHOL1-START-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　本発基本ファイル読込
*--------------------------------------------------------------*
*
 NHJOHOL1-READ-SEC    SECTION.
*
     MOVE     "NHJOHOL1-READ-SEC" TO   S-NAME.
*
     READ     NHJOHOL1
              NEXT  AT  END
                  MOVE     "END"    TO  END-FLG1
                  GO                TO  NHJOHOL1-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT3
     END-READ.
*
     IF  READ-CNT3(6:3) = "000" OR "500"
         DISPLAY "READ-CNT3 = " READ-CNT3 UPON CONS
     END-IF.
*
     IF  LINK-IN-KANRINO < NHJ-F01
         MOVE     "END"    TO  END-FLG1
     END-IF.
*    IF  LINK-IN-SYURUI  = "1"
*        IF  LINK-IN-SAKUBACD < NHJ-F05
*            MOVE     "END"    TO  END-FLG1
*        END-IF
*    END-IF.
*
 NHJOHOL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　本発基本ファイル取消
*--------------------------------------------------------------*
 NHJOHOL1-REWRITE-SEC    SECTION.
*
     MOVE    "NHJOHOL1-REWRITE-SEC" TO   S-NAME.
*
     MOVE    SPACE               TO   NHJ-F34.
     MOVE    ZERO                TO   NHJ-F35.
     MOVE    ZERO                TO   NHJ-F36.
     MOVE    SPACE               TO   NHJ-F37.
     MOVE    SPACE               TO   NHJ-F38.
     REWRITE  NHJ-REC.
*
     ADD     1                   TO   UPD-CNT3.
*
     PERFORM NHJOHOL1-READ-SEC.
*
 NHJOHOL1-REWRITE-EXIT.
     EXIT.
****************************************************************
*　　基本・売上・在庫    取消制御
****************************************************************
 NFJOHOL1-SEC           SECTION.
*
     MOVE     "NFJOHOL1-SEC" TO   S-NAME.
*
     PERFORM  NFJOHOL1-START-SEC.
     IF  END-FLG1 = "END"
         DISPLAY NC"＃基本情報データなし　＃" UPON CONS
         MOVE    4010        TO   PROGRAM-STATUS
         GO                  TO   NFJOHOL1-EXIT
     END-IF.
*    基本情報ファイル読込
     PERFORM  NFJOHOL1-READ-SEC.
*    繰返取消する。
     PERFORM  NFJOHOL1-REWRITE-SEC  UNTIL  END-FLG1 = "END".
*
 NFJOHOL1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　基本情報ファイルスタート
*--------------------------------------------------------------*
 NFJOHOL1-START-SEC   SECTION.
*
     MOVE     "NFJOHOL1-START-SEC" TO   S-NAME.
*
*    基本情報ファイルスタート
     MOVE        SPACE          TO   NFJ-REC.
     INITIALIZE                      NFJ-REC.
*
     MOVE   LINK-IN-KANRINO     TO   NFJ-F01.
     MOVE   LINK-IN-SAKUBACD    TO   NFJ-F05.
     START  NFJOHOL1  KEY   IS  >=   NFJ-F01  NFJ-F05  NFJ-F06
                                     NFJ-F07  NFJ-F08  NFJ-F09
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
     MOVE     "NFJOHOL1-READ-SEC" TO   S-NAME.
*
     READ     NFJOHOL1
              NEXT  AT  END
                  MOVE     "END"    TO  END-FLG1
                  GO                TO  NFJOHOL1-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT4
     END-READ.
*
     IF  READ-CNT4(6:3) = "000" OR "500"
         DISPLAY "READ-CNT4 = " READ-CNT4 UPON CONS
     END-IF.
*
     IF  LINK-IN-KANRINO < NFJ-F01
         MOVE     "END"    TO  END-FLG1
     END-IF.
     IF  LINK-IN-SYURUI  = "1"
         IF  LINK-IN-SAKUBACD < NFJ-F05
             MOVE     "END"    TO  END-FLG1
         END-IF
     END-IF.
*
 NFJOHOL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　売上伝票・在庫・基本情報ファイル取消
*--------------------------------------------------------------*
 NFJOHOL1-REWRITE-SEC    SECTION.
*
     MOVE     "NFJOHOL1-REWRITE-SEC" TO   S-NAME.
*
*    オンライン管理番号の場合は、売上・在庫の取消しは行なわない
     IF       LINK-IN-SYURUI  =  "1"
              GO   TO    NFJOHOL1-REWRITE-04
     END-IF.
*
*売上伝票・在庫　取消し
 NFJOHOL1-REWRITE-01.
*    売上伝票存在チェック（なければ異常）
     PERFORM  SHTDENL1-READ-SEC.
     IF       SHTDENL1-INV-FLG   =  "INV"
              DISPLAY  NC"売上伝票データなし？？" UPON CONS
              DISPLAY  NC"自社伝票番号＝" NFJ-F07 UPON CONS
              DISPLAY  NC"店舗コード　＝" NFJ-F06 UPON CONS
              DISPLAY  NC"納品日　　　＝" NFJ-F09 UPON CONS
              GO   TO    NFJOHOL1-REWRITE-04
     ELSE
              ADD  1     TO   READ-CNT5
     END-IF.
 NFJOHOL1-REWRITE-02.
*    商品変換ＴＢＬチェック→在庫存在チェック・取消し
     PERFORM     SHOTBL1-READ-SEC.
     IF  SHOTBL1-INV-FLG  =  SPACE
         PERFORM  ZAIKO-SEC
     END-IF.
*
 NFJOHOL1-REWRITE-03.
*    売上伝票削除
     DELETE   SHTDENL1.
     ADD  1   TO   DEL-CNT5.
*
*
*基本情報　取消し
 NFJOHOL1-REWRITE-04.
*    オンライン管理番号の場合→FLG更新
*    本発管理番号の場合→レコード削除
     IF      LINK-IN-SYURUI  = "1"
             MOVE    SPACE               TO   NFJ-F34
             MOVE    ZERO                TO   NFJ-F35
             MOVE    ZERO                TO   NFJ-F36
             MOVE    SPACE               TO   NFJ-F37
             MOVE    SPACE               TO   NFJ-F38
             REWRITE NFJ-REC
             ADD     1                   TO   UPD-CNT4
     ELSE
             DELETE  NFJOHOL1
             ADD     1                   TO   DEL-CNT4
     END-IF.
*
     PERFORM NFJOHOL1-READ-SEC.
*
 NFJOHOL1-REWRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　売上伝票ファイル検索
*--------------------------------------------------------------*
 SHTDENL1-READ-SEC    SECTION.
     MOVE     "SHTDENL1-READ-SEC" TO   S-NAME.
*           　　　　　　　　　　　　 　 取引先ＣＤ
     MOVE     NFJ-F04               TO  DEN-F01.
*           　　　　　　　　　　　　 　 自社伝票ＮＯ
     MOVE     NFJ-F07               TO  DEN-F02.
*           　　　　　　　　　　　　 　 相殺区分
     MOVE     0                     TO  DEN-F04.
*           　　　　　　　　　　　　 　 伝区ＣＤ
     MOVE     40                    TO  DEN-F051.
*           　　　　　　　　　　　　 　 店ＣＤ
     MOVE     NFJ-F06               TO  DEN-F07.
*           　　　　　　　　　　　　 　 納品日
     MOVE     NFJ-F09               TO  DEN-F112.
*           　　　　　　　　　　　　 　 行
     MOVE     NFJ-F08               TO  DEN-F03.
*
     READ     SHTDENL1
              INVALID
                  MOVE     "INV"    TO  SHTDENL1-INV-FLG
              NOT INVALID
                  MOVE     "   "    TO  SHTDENL1-INV-FLG
     END-READ.
*
 SHTDENL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                商品変換テーブル読込み                        *
*--------------------------------------------------------------*
 SHOTBL1-READ-SEC          SECTION.
*
     MOVE      "SHOTBL1-READ-SEC" TO    S-NAME.
*
     MOVE      DEN-F01     TO     SHO-F01.
*    MOVE      NFJ-F04     TO     SHO-F01.
     MOVE      DEN-F25     TO     SHO-F02.
*    MOVE      NFJ-F13     TO     SHO-F02.
     READ      SHOTBL1
               INVALID
*TEST
*         DISPLAY "INV" UPON CONS
*         DISPLAY "DEN-F01=" DEN-F01 UPON CONS
*         DISPLAY "DEN-F25=" DEN-F25 UPON CONS
*TEST
               MOVE      "INV"    TO    SHOTBL1-INV-FLG
               MOVE      ZERO     TO    SHO-F09
               NOT  INVALID
*TEST
*         DISPLAY "NOT-INV" UPON CONS
*         DISPLAY "DEN-F01=" DEN-F01 UPON CONS
*         DISPLAY "DEN-F25=" DEN-F25 UPON CONS
*         DISPLAY "SHO-F08=" SHO-F08 UPON CONS
*TEST
               MOVE      SPACE    TO    SHOTBL1-INV-FLG
     END-READ.
*
 SHOTBL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      2.2     在庫引当                                        *
*--------------------------------------------------------------*
 ZAIKO-SEC              SECTION.
*
     MOVE     "ZAIKO-SEC"         TO   S-NAME.
*在庫マスタ存在チェック
     MOVE    DEN-F08         TO   ZAI-F01.
     MOVE    DEN-F1411       TO   ZAI-F021.
     MOVE    DEN-F1412       TO   ZAI-F022.
     MOVE    SHO-F08         TO   ZAI-F03.
     READ    ZAMZAIL1
             INVALID
                  DISPLAY  NC"在庫マスタなし" UPON CONS
                  DISPLAY  NC"倉庫＝" DEN-F08   UPON CONS
                  DISPLAY  NC"商品＝" DEN-F1411 UPON CONS
                  DISPLAY  NC"品単＝" DEN-F1412 UPON CONS
                  DISPLAY  NC"棚番＝" SHO-F08   UPON CONS
                  GO   TO  ZAIKO-EXIT
             NOT  INVALID
                  ADD  1   TO    READ-CNT6
                  PERFORM  ZAIKO-UPDATE2-SEC
     END-READ.
*
 ZAIKO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      2.2     在庫引当   ※在庫戻し
*--------------------------------------------------------------*
 ZAIKO-UPDATE2-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE2-SEC" TO   S-NAME.
*引当後在庫数チェック
*          引当可能在庫数=   現在庫数 － 引当済数
     COMPUTE   WRK-ZAI   =   ZAI-F04  -  ZAI-F28.
*          引当後在庫数  =   引当可能在庫数 + 発注数量
     COMPUTE   WRK-HIK   =      WRK-ZAI     +  DEN-F15.
     IF  WRK-HIK  <  0
         MOVE      "1"      TO   KEP-FLG
*        未出庫数に数量から減算
         COMPUTE  ZAI-F27   =    ZAI-F27  -  DEN-F15
         MOVE     WK-DATE8  TO   ZAI-F99
*        在庫マスタ更新
         REWRITE  ZAI-REC
         ADD      1         TO   UPD-CNT6
     ELSE
         MOVE     SPACE          TO   KEP-FLG
*        引当済数に数量から減算
         COMPUTE  ZAI-F28   =    ZAI-F28  -  DEN-F15
*        未出庫数に数量から減算
         COMPUTE  ZAI-F27   =    ZAI-F27  -  DEN-F15
         MOVE     WK-DATE8  TO   ZAI-F99
*        在庫マスタ更新
         REWRITE  ZAI-REC
         ADD      1         TO   UPD-CNT6
     END-IF.
*
 ZAIKO-UPDATE2-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "## NFSUTEL4 READ   CNT = " READ-CNT1 UPON CONS.
     DISPLAY "## NFSUTEL4 FLGUPD CNT = " UPD-CNT1  UPON CONS.
     DISPLAY "## NFHAKOL4 READ   CNT = " READ-CNT2 UPON CONS.
     DISPLAY "## NFHAKOL4 FLGUPD CNT = " UPD-CNT2  UPON CONS.
     DISPLAY "## NHJOHOL1 READ   CNT = " READ-CNT3 UPON CONS.
     DISPLAY "## NHJOHOL1 FLGUPD CNT = " UPD-CNT3 UPON CONS.
     DISPLAY "## NFJOHOL1 READ   CNT = " READ-CNT4 UPON CONS.
     DISPLAY "## NFJOHOL1 FLGUPD CNT = " UPD-CNT4 UPON CONS.
     DISPLAY "## NFJOHOL1 DELETE CNT = " DEL-CNT4 UPON CONS.
     DISPLAY "## SHTDENL1 READ   CNT = " READ-CNT5 UPON CONS.
     DISPLAY "## SHTDENL1 DELETE CNT = " DEL-CNT5 UPON CONS.
     DISPLAY "## ZAMZAIL1 READ   CNT = " READ-CNT6 UPON CONS.
     DISPLAY "## ZAMZAIL1 UPDATE CNT = " UPD-CNT6 UPON CONS.
*
     CLOSE     NFSUTEL4  NFHAKOL4  NHJOHOL1  NFJOHOL1
               SHTDENL1  ZAMZAIL1  SHOTBL1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
