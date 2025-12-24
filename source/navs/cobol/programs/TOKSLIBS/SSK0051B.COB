****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ケーヨー伝票レス　　　　　　　　　*
*    業務名　　　　　　　：　ケーヨー伝票レス                  *
*    モジュール名　　　　：　返品実績データ抽出　　　　　　    *
*    作成日／更新日　　　：　14/03/19                          *
*    作成者／更新者　　　：　ＮＡＶ三浦　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取った各パラメタより、累積    *
*                            データから、返品計上確認リスト・  *
*                            ＣＳＶ用にデータを抽出する。      *
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSK0051B.
 AUTHOR.                NAV MIURA.
 DATE-WRITTEN.          14/03/19.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*返品累積データ（検収日）
     SELECT   KEIJHRL3  ASSIGN    TO        DA-01-VI-KEIJHRL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HR3-F80 HR3-F07
                                            HR3-F82
                        FILE  STATUS   IS   HR3-STATUS.
*返品累積データ（入力日）
     SELECT   KEIJHRL4  ASSIGN    TO        DA-01-VI-KEIJHRL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HR4-F80 HR4-F83
                                            HR4-F82
                        FILE  STATUS   IS   HR4-STATUS.
*返品累積データ（計上日）
     SELECT   KEIJHRL5  ASSIGN    TO        DA-01-VI-KEIJHRL5
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HR5-F80 HR5-F86
                                            HR5-F82
                        FILE  STATUS   IS   HR5-STATUS.
*返品抽出データ
     SELECT   KEIJHWF   ASSIGN    TO        DA-01-VS-KEIJHWF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HWK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    返品累積データ（検収日）
******************************************************************
 FD  KEIJHRL3
                        LABEL RECORD   IS   STANDARD.
     COPY     KEIJHRF   OF        XFDLIB
              JOINING   HR3  AS   PREFIX.
*
******************************************************************
*    返品累積データ（入力日）
******************************************************************
 FD  KEIJHRL4
                        LABEL RECORD   IS   STANDARD.
     COPY     KEIJHRF   OF        XFDLIB
              JOINING   HR4  AS   PREFIX.
*
******************************************************************
*    返品累積データ（計上日）
******************************************************************
 FD  KEIJHRL5
                        LABEL RECORD   IS   STANDARD.
     COPY     KEIJHRF   OF        XFDLIB
              JOINING   HR5  AS   PREFIX.
*
******************************************************************
*    返品抽出データ
******************************************************************
 FD  KEIJHWF
                        LABEL RECORD   IS   STANDARD.
     COPY     KEIJHWF   OF        XFDLIB
              JOINING   HWK       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
 01  WK-ST.
     03  HR3-STATUS        PIC  X(02).
     03  HR4-STATUS        PIC  X(02).
     03  HR5-STATUS        PIC  X(02).
     03  HWK-STATUS        PIC  X(02).
 01  WK-HIDUKE               PIC  9(08)     VALUE  ZERO.
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSK0051B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0051B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0051B".
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
 COPY     KEIJHRF   OF        XFDLIB.
 LINKAGE                SECTION.
 01  PARA-SKBN              PIC   X(01).
 01  PARA-DKBN              PIC   X(01).
 01  PARA-DFROM             PIC   9(08).
 01  PARA-DTO               PIC   9(08).
 01  PARA-KKBN              PIC   X(01).
 01  PARA-TANFROM           PIC   X(02).
 01  PARA-TANTO             PIC   X(02).
 01  PARA-DENK1             PIC   X(02).
 01  PARA-DENK2             PIC   X(02).
 01  PARA-DENK3             PIC   X(02).
 01  PARA-DENK4             PIC   X(02).
 01  PARA-DENK5             PIC   X(02).
 01  PARA-TENFROM           PIC   9(05).
 01  PARA-TENTO             PIC   9(05).
 01  PARA-DENNFROM          PIC   9(09).
 01  PARA-DENNTO            PIC   9(09).
 01  PARA-SKBFROM           PIC   X(02).
 01  PARA-SKBTO             PIC   X(02).
 01  PARA-DENKFROM          PIC   X(02).
 01  PARA-DENKTO            PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-SKBN
                                       PARA-DKBN
                                       PARA-DFROM
                                       PARA-DTO
                                       PARA-KKBN
                                       PARA-TANFROM
                                       PARA-TANTO
                                       PARA-DENK1
                                       PARA-DENK2
                                       PARA-DENK3
                                       PARA-DENK4
                                       PARA-DENK5
                                       PARA-TENFROM
                                       PARA-TENTO
                                       PARA-DENNFROM
                                       PARA-DENNTO
                                       PARA-SKBFROM
                                       PARA-SKBTO
                                       PARA-DENKFROM
                                       PARA-DENKTO.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIJHRL3.
     MOVE      "KEIJHRL3"   TO   AB-FILE.
     MOVE      HR3-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIJHRL4.
     MOVE      "KEIJHRL4"   TO   AB-FILE.
     MOVE      HR4-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIJHRL5.
     MOVE      "KEIJHRL5"   TO   AB-FILE.
     MOVE      HR5-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIJHWF.
     MOVE      "KEIJHWF "   TO   AB-FILE.
     MOVE      HWK-STATUS   TO   AB-STS.
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
     EVALUATE PARA-DKBN
         WHEN   " "
             OPEN     INPUT     KEIJHRL3
         WHEN   "2"
             OPEN     INPUT     KEIJHRL3
         WHEN   "6"
             OPEN     INPUT     KEIJHRL4
         WHEN   "7"
             OPEN     INPUT     KEIJHRL5
     END-EVALUATE.
     OPEN     OUTPUT    KEIJHWF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
     MOVE     SPACE  TO  HR3-REC HR4-REC HR5-REC REC.
     INITIALIZE      HR3-REC HR4-REC HR5-REC REC.
     EVALUATE PARA-KKBN
         WHEN   "1"
             EVALUATE PARA-DKBN
                 WHEN   " "
                  MOVE   SPACE    TO   HR3-F80
                 WHEN   "2"
                  MOVE   SPACE    TO   HR3-F80
                 WHEN   "6"
                  MOVE   SPACE    TO   HR4-F80
                 WHEN   OTHER
                  MOVE   SPACE    TO   HR5-F80
             END-EVALUATE
         WHEN   "2"
             EVALUATE PARA-DKBN
                 WHEN   " "
                  MOVE   "1"    TO   HR3-F80
                  MOVE   ZERO          TO   HR3-F07
                 WHEN   "2"
                  MOVE   "1"    TO   HR3-F80
                  MOVE   PARA-DFROM    TO   HR3-F07
                 WHEN   "6"
                  MOVE   "1"    TO   HR4-F80
                  MOVE   PARA-DFROM    TO   HR4-F83
                 WHEN   OTHER
                  MOVE   "1"    TO   HR5-F80
                  MOVE   PARA-DFROM  TO   HR5-F86
             END-EVALUATE
         WHEN   "3"
             EVALUATE PARA-DKBN
                 WHEN   " "
                  MOVE   "1"    TO   HR3-F80
                  MOVE   ZERO          TO   HR3-F07
                 WHEN   "2"
                  MOVE   "1"    TO   HR3-F80
                  MOVE   PARA-DFROM    TO   HR3-F07
                 WHEN   "6"
                  MOVE   "1"    TO   HR4-F80
                  MOVE   PARA-DFROM    TO   HR4-F83
                 WHEN   OTHER
                  MOVE   "1"    TO   HR5-F80
                  MOVE   PARA-DFROM   TO   HR5-F86
             END-EVALUATE
         WHEN   "4"
             EVALUATE PARA-DKBN
                 WHEN   " "
                  MOVE   "9"    TO   HR3-F80
                  MOVE   ZERO          TO   HR3-F07
                 WHEN   "2"
                  MOVE   "9"    TO   HR3-F80
                  MOVE   PARA-DFROM    TO   HR3-F07
                 WHEN   "6"
                  MOVE   "9"    TO   HR4-F80
                  MOVE   PARA-DFROM    TO   HR4-F83
                 WHEN   OTHER
                  MOVE   "9"    TO   HR5-F80
                  MOVE   ZERO   TO   HR5-F86
             END-EVALUATE
     END-EVALUATE.
     EVALUATE PARA-DKBN
         WHEN   " "
            START    KEIJHRL3  KEY  >= HR3-F80 HR3-F07 HR3-F82
                INVALID   KEY
                   MOVE      9    TO   END-FG
                   GO   TO   INIT-EXIT
             END-START
         WHEN   "2"
            START    KEIJHRL3  KEY  >= HR3-F80 HR3-F07 HR3-F82
                INVALID   KEY
                   MOVE      9    TO   END-FG
                   GO   TO   INIT-EXIT
             END-START
         WHEN   "6"
            START    KEIJHRL4  KEY  >= HR4-F80 HR4-F83 HR4-F82
                INVALID   KEY
                   MOVE      9    TO   END-FG
                   GO   TO   INIT-EXIT
             END-START
         WHEN   "7"
            START    KEIJHRL5  KEY  >= HR5-F80 HR5-F86 HR5-F82
                INVALID   KEY
                   MOVE      9    TO   END-FG
                   GO   TO   INIT-EXIT
             END-START
     END-EVALUATE.
*
 INIT-010.
*
     EVALUATE PARA-DKBN
         WHEN   " "
             READ     KEIJHRL3
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HR3-REC   TO  REC
                                MOVE      HR3-F07   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "2"
             READ     KEIJHRL3
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HR3-REC   TO  REC
                                MOVE      HR3-F07   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "6"
             READ     KEIJHRL4
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HR4-REC   TO  REC
                                MOVE      HR4-F83   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "7"
             READ     KEIJHRL5
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HR5-REC   TO  REC
                                MOVE      HR5-F86   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
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
     EVALUATE PARA-KKBN
         WHEN   "1"
*            計上区分
             IF     ( F80   NOT = SPACE )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
             ELSE
                GO   TO        MAIN-001
             END-IF
         WHEN   "2"
*            計上区分
             IF     ( F80   NOT = "1" )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
             END-IF
*            計上日
             IF     ( F86      >    ZERO )
                GO        TO        MAIN-010
             END-IF
         WHEN   "3"
*            計上区分
             IF     ( F80   NOT = "1" )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
             END-IF
*            計上日
             IF     ( F86      =    ZERO )
                GO        TO        MAIN-010
             END-IF
         WHEN   "4"
*            計上区分
             IF     ( F80   NOT = "9" )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
             END-IF
*            計上日
             IF  ( PARA-KKBN = "7" )
                GO        TO        MAIN-TAN
             END-IF
     END-EVALUATE.
 MAIN-HIZUKE.
*    日付TO
     IF     ( PARA-DTO      <    WK-HIDUKE )
           MOVE      9         TO   END-FG
           GO        TO        MAIN-EXIT
     END-IF.
 MAIN-TAN.
*    担当者
     IF     ( PARA-TANFROM  NOT =  SPACE ) AND
            ( PARA-TANFROM  >  F82 )
                GO        TO        MAIN-010
     END-IF.
     IF     ( PARA-TANTO  NOT =  SPACE ) AND
            ( PARA-TANTO  <  F82 )
                GO        TO        MAIN-010
     END-IF.
*
 MAIN-001.
*返品抽出データ出力
     MOVE     SPACE          TO   HWK-REC.
     INITIALIZE                   HWK-REC.
     MOVE     REC        TO   HWK-REC.
     WRITE    HWK-REC.
     ADD      1              TO   WRT-CNT.
*
 MAIN-010.
*
     EVALUATE PARA-DKBN
         WHEN   " "
             READ     KEIJHRL3
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HR3-REC   TO  REC
                                MOVE      HR3-F07   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "2"
             READ     KEIJHRL3
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HR3-REC   TO  REC
                                MOVE      HR3-F07   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "6"
             READ     KEIJHRL4
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HR4-REC   TO  REC
                                MOVE      HR4-F83   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "7"
             READ     KEIJHRL5
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HR5-REC   TO  REC
                                MOVE      HR5-F86   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
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
     CLOSE     KEIJHWF.
     EVALUATE PARA-DKBN
         WHEN   " "
             CLOSE     KEIJHRL3
         WHEN   "2"
             CLOSE     KEIJHRL3
         WHEN   "6"
             CLOSE     KEIJHRL4
         WHEN   "7"
             CLOSE     KEIJHRL5
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
