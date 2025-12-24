****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ケーヨー伝票レス　　　　　　　　　*
*    業務名　　　　　　　：　ケーヨー伝票レス                  *
*    モジュール名　　　　：　受領実績データ抽出（共通）　　    *
*    作成日／更新日　　　：　14/03/10                          *
*    作成者／更新者　　　：　ＮＡＶ三浦　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取った各パラメタより、累積    *
*                            データから、受領書・ＣＳＶ出力用  *
*                            にデータを抽出する。              *
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSK0021B.
 AUTHOR.                NAV MIURA.
 DATE-WRITTEN.          14/03/10.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受領累積データ（受信日）
     SELECT   KEIJYRL3  ASSIGN    TO        DA-01-VI-KEIJYRL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JL3-F01
                        FILE  STATUS   IS   JL3-STATUS.
*受領累積データ（検収日）
     SELECT   KEIJYRL4  ASSIGN    TO        DA-01-VI-KEIJYRL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JL4-F10
                        FILE  STATUS   IS   JL4-STATUS.
*受領累積データ（納品日）
     SELECT   KEIJYRL5  ASSIGN    TO        DA-01-VI-KEIJYRL5
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JL5-F09
                        FILE  STATUS   IS   JL5-STATUS.
*受領累積データ（発注日）
     SELECT   KEIJYRL6  ASSIGN    TO        DA-01-VI-KEIJYRL6
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JL6-F24
                        FILE  STATUS   IS   JL6-STATUS.
*受領実績抽出データ
     SELECT   KEIJYWF   ASSIGN    TO        DA-01-VS-KEIJYWF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   JWK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受領累積データ（受信日）
******************************************************************
 FD  KEIJYRL3
                        LABEL RECORD   IS   STANDARD.
     COPY     KEIJYRF   OF        XFDLIB
              JOINING   JL3  AS   PREFIX.
*
******************************************************************
*    受領累積データ（検収日）
******************************************************************
 FD  KEIJYRL4
                        LABEL RECORD   IS   STANDARD.
     COPY     KEIJYRF   OF        XFDLIB
              JOINING   JL4  AS   PREFIX.
*
******************************************************************
*    受領累積データ（納品日）
******************************************************************
 FD  KEIJYRL5
                        LABEL RECORD   IS   STANDARD.
     COPY     KEIJYRF   OF        XFDLIB
              JOINING   JL5  AS   PREFIX.
*
******************************************************************
*    受領累積データ（発注日）
******************************************************************
 FD  KEIJYRL6
                        LABEL RECORD   IS   STANDARD.
     COPY     KEIJYRF   OF        XFDLIB
              JOINING   JL6  AS   PREFIX.
*
******************************************************************
*    受領実績抽出データ
******************************************************************
 FD  KEIJYWF
                        LABEL RECORD   IS   STANDARD.
     COPY     KEIJYRF   OF        XFDLIB
              JOINING   JWK       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
 01  WK-ST.
     03  JL3-STATUS        PIC  X(02).
     03  JL4-STATUS        PIC  X(02).
     03  JL5-STATUS        PIC  X(02).
     03  JL6-STATUS        PIC  X(02).
     03  JWK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSK0021B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0021B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0021B".
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
 COPY     KEIJYRF   OF        XFDLIB.
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
                        PROCEDURE   KEIJYRL3.
     MOVE      "KEIJYRL3"   TO   AB-FILE.
     MOVE      JL3-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIJYRL4.
     MOVE      "KEIJYRL4"   TO   AB-FILE.
     MOVE      JL4-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIJYRL5.
     MOVE      "KEIJYRL5"   TO   AB-FILE.
     MOVE      JL5-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIJYRL6.
     MOVE      "KEIJYRL6"   TO   AB-FILE.
     MOVE      JL6-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIJYWF.
     MOVE      "KEIJYWF "   TO   AB-FILE.
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
     EVALUATE PARA-DKBN
         WHEN   "1"
             OPEN     INPUT     KEIJYRL3
         WHEN   "2"
             OPEN     INPUT     KEIJYRL4
         WHEN   "3"
             OPEN     INPUT     KEIJYRL5
         WHEN   OTHER
             OPEN     INPUT     KEIJYRL6
     END-EVALUATE.
     OPEN     OUTPUT    KEIJYWF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
     MOVE     SPACE  TO  JL3-REC JL4-REC JL5-REC JL6-REC REC.
     INITIALIZE      JL3-REC JL4-REC JL5-REC JL6-REC REC.
     EVALUATE PARA-DKBN
         WHEN   "1"
             MOVE     PARA-DFROM     TO   JL3-F01
             START    KEIJYRL3  KEY  >=   JL3-F01
                INVALID   KEY
                   MOVE      9    TO   END-FG
                   GO   TO   INIT-EXIT
             END-START
         WHEN   "2"
             MOVE     PARA-DFROM     TO   JL4-F10
             START    KEIJYRL4  KEY  >=   JL4-F10
                INVALID   KEY
                   MOVE      9    TO   END-FG
                   GO   TO   INIT-EXIT
             END-START
         WHEN   "3"
             MOVE     PARA-DFROM     TO   JL5-F09
             START    KEIJYRL5  KEY  >=   JL5-F09
                INVALID   KEY
                   MOVE      9    TO   END-FG
                   GO   TO   INIT-EXIT
             END-START
         WHEN   OTHER
             MOVE     PARA-DFROM     TO   JL6-F24
             START    KEIJYRL6  KEY  >=   JL6-F24
                INVALID   KEY
                   MOVE      9    TO   END-FG
                   GO   TO   INIT-EXIT
             END-START
     END-EVALUATE.
*
 INIT-010.
*
     EVALUATE PARA-DKBN
         WHEN   "1"
             READ     KEIJYRL3
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      JL3-REC   TO  REC
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "2"
             READ     KEIJYRL4
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      JL4-REC   TO  REC
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "3"
             READ     KEIJYRL5
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      JL5-REC   TO  REC
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   OTHER
             READ     KEIJYRL6
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      JL6-REC   TO  REC
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
*    DISPLAY "REC = "  REC UPON CONS.
     EVALUATE PARA-DKBN
         WHEN   "1"
*            日付TO
             IF     ( PARA-DTO      <    F01 )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
             ELSE
                CONTINUE
             END-IF
         WHEN   "2"
*            日付TO
             IF     ( PARA-DTO      <    F10 )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
             ELSE
                CONTINUE
             END-IF
         WHEN   "3"
*            日付TO
             IF     ( PARA-DTO      <    F09 )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
             ELSE
                CONTINUE
             END-IF
         WHEN   OTHER
*            日付TO
             IF     ( PARA-DTO      <    F24 )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
             ELSE
                CONTINUE
             END-IF
     END-EVALUATE.
*    伝区１～５＝伝票区分（ケーヨー）
     IF  (PARA-DENK1      =     SPACE) AND
         (PARA-DENK2      =     SPACE) AND
         (PARA-DENK3      =     SPACE) AND
         (PARA-DENK4      =     SPACE) AND
         (PARA-DENK5      =     SPACE)
          CONTINUE
     ELSE
         IF  (PARA-DENK1      =     F07) OR
             (PARA-DENK2      =     F07) OR
             (PARA-DENK3      =     F07) OR
             (PARA-DENK4      =     F07) OR
             (PARA-DENK5      =     F07)
                   CONTINUE
         ELSE
              GO   TO        MAIN-010
         END-IF
     END-IF.
*
*受領実績抽出データ出力
     MOVE     SPACE          TO   JWK-REC.
     INITIALIZE                   JWK-REC.
     MOVE     REC        TO   JWK-REC.
     WRITE    JWK-REC.
     ADD      1              TO   WRT-CNT.
*
 MAIN-010.
*
     EVALUATE PARA-DKBN
         WHEN   "1"
             READ     KEIJYRL3
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      JL3-REC   TO  REC
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "2"
             READ     KEIJYRL4
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      JL4-REC   TO  REC
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "3"
             READ     KEIJYRL5
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      JL5-REC   TO  REC
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   OTHER
             READ     KEIJYRL6
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      JL6-REC   TO  REC
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
     CLOSE     KEIJYWF.
     EVALUATE PARA-DKBN
         WHEN   "1"
             CLOSE     KEIJYRL3
         WHEN   "2"
             CLOSE     KEIJYRL4
         WHEN   "3"
             CLOSE     KEIJYRL5
         WHEN   OTHER
             CLOSE     KEIJYRL6
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