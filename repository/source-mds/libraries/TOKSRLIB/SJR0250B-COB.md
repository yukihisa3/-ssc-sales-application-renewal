# SJR0250B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJR0250B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受領返品　　　　　　              *
*    モジュール名　　　　：　共通受領書データ抽出　　　　　    *
*    作成日／作成者　　　：　2017/09/21 INOUE                  *
*    処理概要　　　　　　：　受け取った各パラメタより、　　    *
*                            受領書・ＣＳＶ出力用データを      *
*                            抽出する。                        *
*    流用　　　　　　　　：　SSK0021B                          *
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJR0250B.
 AUTHOR.                NAV-ASSIST.
 DATE-WRITTEN.          2017/09/21.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受領返品累積Ｆ（受信日）
     SELECT   COMRUIL1  ASSIGN    TO        DA-01-VI-COMRUIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JR1-F03
                                            JR1-F01
                                            JR1-F02
                                            JR1-F04
                                            JR1-F31
                        FILE      STATUS    IS   JR1-STATUS.
*受領返品累積Ｆ（計上日）
     SELECT   COMRUIL2  ASSIGN    TO        DA-01-VI-COMRUIL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JR2-F03
                                            JR2-F08
                                            JR2-F04
                                            JR2-F31
                        FILE      STATUS    IS   JR2-STATUS.
*受領書印刷ワーク
     SELECT   COMJRWKF  ASSIGN    TO        DA-01-VS-COMJRWKF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   JWK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受領返品累積Ｆ（受信日）
******************************************************************
 FD  COMRUIL1
                        LABEL RECORD   IS   STANDARD.
     COPY     COMRUIL1  OF        XFDLIB
              JOINING   JR1  AS   PREFIX.
*
******************************************************************
*    受領返品累積Ｆ（計上日）
******************************************************************
 FD  COMRUIL2
                        LABEL RECORD   IS   STANDARD.
     COPY     COMRUIL2  OF        XFDLIB
              JOINING   JR2  AS   PREFIX.
*
******************************************************************
*    受領書印刷ワーク
******************************************************************
 FD  COMJRWKF
                        LABEL RECORD   IS   STANDARD.
     COPY     COMJRWKF  OF        XFDLIB
              JOINING   JWK       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
 01  WK-ST.
     03  JR1-STATUS        PIC  X(02).
     03  JR2-STATUS        PIC  X(02).
     03  JWK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR0250B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0250B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0250B".
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
 COPY     COMRUIF   OF      XFDLIB.
*
******************************************************************
 LINKAGE                SECTION.
 01  LINK-IN-TOKCD        PIC 9(08).
 01  LINK-IN-DATEKBN      PIC X(01).
 01  LINK-IN-FROMDATE     PIC 9(08).
 01  LINK-IN-TODATE       PIC 9(08).
 01  LINK-IN-HNPKBN       PIC X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE       DIVISION  USING     LINK-IN-TOKCD
                                     LINK-IN-DATEKBN
                                     LINK-IN-FROMDATE
                                     LINK-IN-TODATE
                                     LINK-IN-HNPKBN.
******************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRUIL1.
     MOVE      "COMRUIL1"   TO   AB-FILE.
     MOVE      JR1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRUIL2.
     MOVE      "COMRUIL2"   TO   AB-FILE.
     MOVE      JR2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMJRWKF.
     MOVE      "COMJRWKF "  TO   AB-FILE.
     MOVE      JWK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
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
     EVALUATE LINK-IN-DATEKBN
         WHEN " "
              OPEN      INPUT     COMRUIL1
         WHEN "1"
              OPEN      INPUT     COMRUIL2
     END-EVALUATE.
     OPEN     OUTPUT    COMJRWKF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
     MOVE     SPACE     TO  JR1-REC JR2-REC REC.
     INITIALIZE             JR1-REC JR2-REC REC.
*
     EVALUATE LINK-IN-DATEKBN
         WHEN   " "
             MOVE       LINK-IN-TOKCD       TO   JR1-F03
             MOVE       LINK-IN-FROMDATE    TO   JR1-F01
             MOVE       ZERO                TO   JR1-F02
                                                 JR1-F04
                                                 JR1-F31
*
             START      COMRUIL1  KEY  >=        JR1-F03
                                                 JR1-F01
                                                 JR1-F02
                                                 JR1-F04
                                                 JR1-F31
                INVALID KEY
                        MOVE      9         TO   END-FG
                        GO                  TO   INIT-EXIT
             END-START
*
         WHEN   "1"
             MOVE       LINK-IN-TOKCD       TO   JR2-F03
             MOVE       LINK-IN-FROMDATE    TO   JR2-F08
             MOVE       ZERO                TO   JR2-F04
                                                 JR2-F31
*
             START      COMRUIL2  KEY  >=        JR2-F03
                                                 JR2-F08
                                                 JR2-F04
                                                 JR2-F31
                INVALID KEY
                        MOVE      9         TO   END-FG
                        GO                  TO   INIT-EXIT
             END-START
     END-EVALUATE.
*
 INIT-010.
*
     EVALUATE LINK-IN-DATEKBN
         WHEN   " "
             READ     COMRUIL1
               AT END      MOVE      9            TO   END-FG
                           GO                     TO   INIT-EXIT
               NOT AT END  MOVE      JR1-REC      TO   REC
                           ADD       1            TO   RD-CNT
             END-READ
         WHEN   "1"
             READ     COMRUIL2
               AT END      MOVE      9            TO   END-FG
                           GO                     TO   INIT-EXIT
               NOT AT END  MOVE      JR2-REC      TO   REC
                           ADD       1            TO   RD-CNT
             END-READ
     END-EVALUATE.
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
*    DISPLAY "REC = "  REC UPON CONS.
*
*    取引先ＣＤ
     IF       F03     NOT =   LINK-IN-TOKCD
              MOVE    9       TO   END-FG
              GO              TO   MAIN-EXIT
     END-IF.
*
*    日付（ＴＯ）
     EVALUATE LINK-IN-DATEKBN
         WHEN " "
               IF     ( LINK-IN-TODATE      <    F01 )
                        MOVE      9         TO   END-FG
                        GO                  TO   MAIN-EXIT
               ELSE
                        CONTINUE
               END-IF
         WHEN "1"
               IF     ( LINK-IN-TODATE      <    F08 )
                        MOVE      9         TO   END-FG
                        GO                  TO   MAIN-EXIT
               ELSE
                        CONTINUE
               END-IF
     END-EVALUATE.
*
*    自社伝票区分
     EVALUATE LINK-IN-HNPKBN
         WHEN " "
               CONTINUE
         WHEN "1"
               IF     ( F51  =  "41" ) OR
                      ( F51  =  "42" )
                        CONTINUE
               ELSE
                        GO                  TO   MAIN-010
               END-IF
         WHEN "2"
               IF     ( F51  NOT =  "41" ) AND
                      ( F51  NOT =  "42" )
                        CONTINUE
               ELSE
                        GO                  TO   MAIN-010
               END-IF
     END-EVALUATE.
*
*受領書印刷ワーク出力
     MOVE     SPACE          TO   JWK-REC.
     INITIALIZE                   JWK-REC.
     MOVE     REC            TO   JWK-REC.
     WRITE    JWK-REC.
     ADD      1              TO   WRT-CNT.
*
 MAIN-010.
*
     EVALUATE LINK-IN-DATEKBN
         WHEN   " "
             READ     COMRUIL1
               AT     END    MOVE      9         TO   END-FG
               NOT AT END
                             MOVE      JR1-REC   TO   REC
                             ADD       1         TO   RD-CNT
             END-READ
         WHEN   "1"
             READ     COMRUIL2
               AT     END    MOVE      9         TO   END-FG
               NOT AT END
                             MOVE      JR2-REC   TO   REC
                             ADD       1         TO   RD-CNT
             END-READ
     END-EVALUATE.
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
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     COMJRWKF.
     EVALUATE LINK-IN-DATEKBN
         WHEN   " "
             CLOSE     COMRUIL1
         WHEN   "1"
             CLOSE     COMRUIL2
     END-EVALUATE.
     IF  OUT-CNT = ZERO
         MOVE      4001         TO   PROGRAM-STATUS
     END-IF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
