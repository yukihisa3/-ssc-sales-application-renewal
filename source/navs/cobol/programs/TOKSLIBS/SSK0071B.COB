****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ケーヨー伝票レス　　　　　　　　　*
*    業務名　　　　　　　：　ケーヨー伝票レス                  *
*    モジュール名　　　　：　売上実績データ抽出（共通）　　    *
*    作成日／更新日　　　：　14/03/13                          *
*    作成者／更新者　　　：　ＮＡＶ三浦　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取った各パラメタより、累積    *
*                            データから、売上リスト・ＣＳＶ　  *
*                            出力用にデータを抽出する。        *
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSK0071B.
 AUTHOR.                NAV MIURA.
 DATE-WRITTEN.          14/03/13.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上累積データ（納品日）
     SELECT   KEIURIL3  ASSIGN    TO        DA-01-VI-KEIURIL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JL3-F01 JL3-F112
                        FILE  STATUS   IS   JL3-STATUS.
*売上累積データ（発注日）
     SELECT   KEIURIL2  ASSIGN    TO        DA-01-VI-KEIURIL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JL2-F01 JL2-F111
                        FILE  STATUS   IS   JL2-STATUS.
*売上抽出データ
     SELECT   KEIURWF   ASSIGN    TO        DA-01-VS-KEIURWF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   UWK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上累積データ（納品日）
******************************************************************
 FD  KEIURIL3
                        LABEL RECORD   IS   STANDARD.
     COPY     KEIURIF   OF        XFDLIB
              JOINING   JL3  AS   PREFIX.
*
******************************************************************
*    売上累積データ（発注日）
******************************************************************
 FD  KEIURIL2
                        LABEL RECORD   IS   STANDARD.
     COPY     KEIURIF   OF        XFDLIB
              JOINING   JL2  AS   PREFIX.
*
******************************************************************
*    売上抽出データ
******************************************************************
 FD  KEIURWF
                        LABEL RECORD   IS   STANDARD.
     COPY     KEIURWF   OF        XFDLIB
              JOINING   UWK       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
 01  WK-ST.
     03  JL3-STATUS        PIC  X(02).
     03  JL2-STATUS        PIC  X(02).
     03  UWK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSK0071B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0071B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0071B".
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
 COPY     KEIURIF   OF        XFDLIB.
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
                        PROCEDURE   KEIURIL2.
     MOVE      "KEIURIL2"   TO   AB-FILE.
     MOVE      JL2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIURIL3.
     MOVE      "KEIURIL3"   TO   AB-FILE.
     MOVE      JL3-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIURWF.
     MOVE      "KEIURWF "   TO   AB-FILE.
     MOVE      UWK-STATUS   TO   AB-STS.
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
         WHEN   "4"
             OPEN     INPUT     KEIURIL2
         WHEN   OTHER
             OPEN     INPUT     KEIURIL3
     END-EVALUATE.
     OPEN     OUTPUT    KEIURWF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
     MOVE     SPACE  TO  JL3-REC JL2-REC REC.
     INITIALIZE      JL3-REC JL2-REC REC.
     EVALUATE PARA-DKBN
         WHEN   "4"
             MOVE     173            TO   JL2-F01
             MOVE     PARA-DFROM     TO   JL2-F111
             START    KEIURIL2  KEY  >=   JL2-F01 JL2-F111
                INVALID   KEY
                   MOVE      9    TO   END-FG
                   GO   TO   INIT-EXIT
             END-START
         WHEN   OTHER
             MOVE     173            TO   JL3-F01
             MOVE     PARA-DFROM     TO   JL3-F112
             START    KEIURIL3  KEY  >=   JL3-F01 JL3-F112
                INVALID   KEY
                   MOVE      9    TO   END-FG
                   GO   TO   INIT-EXIT
             END-START
     END-EVALUATE.
*
 INIT-010.
*
     EVALUATE PARA-DKBN
         WHEN   "4"
             READ     KEIURIL2
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      JL2-REC   TO  REC
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   OTHER
             READ     KEIURIL3
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      JL3-REC   TO  REC
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
         WHEN   "4"
*            日付TO
             IF     ( PARA-DTO      <    F111 )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
             ELSE
                CONTINUE
             END-IF
         WHEN   OTHER
*            日付TO
             IF     ( PARA-DTO      <    F112 )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
             ELSE
                CONTINUE
             END-IF
     END-EVALUATE.
*    店舗ＣＤ範囲
     IF  (PARA-TENFROM       >     F07) OR
         (PARA-TENTO       <     F07)
              GO   TO        MAIN-010
     END-IF.
*    伝票番号範囲
     IF  (PARA-DENNFROM       >     F02) OR
         (PARA-DENNTO       <     F02)
              GO   TO        MAIN-010
     END-IF.
*    出荷場所範囲
     IF  (PARA-SKBFROM     =  SPACE ) AND
         (PARA-SKBTO       =  SPACE)
                CONTINUE
     ELSE
         IF  (PARA-SKBFROM     >  F08 ) OR
             (PARA-SKBTO       <  F08)
              GO   TO        MAIN-010
         END-IF
     END-IF.
*    伝票区分範囲
     IF  (PARA-DENKFROM     =  SPACE ) AND
         (PARA-DENKTO       =  SPACE)
                CONTINUE
     ELSE
         IF  (PARA-DENKFROM     >  F051 ) OR
             (PARA-DENKTO       <  F051)
              GO   TO        MAIN-010
         END-IF
     END-IF.
*
*売上抽出データ出力
     MOVE     SPACE          TO   UWK-REC.
     INITIALIZE                   UWK-REC.
     MOVE     REC        TO   UWK-REC.
     WRITE    UWK-REC.
     ADD      1              TO   WRT-CNT.
*
 MAIN-010.
*
     EVALUATE PARA-DKBN
         WHEN   "4"
             READ     KEIURIL2
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      JL2-REC   TO  REC
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   OTHER
             READ     KEIURIL3
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      JL3-REC   TO  REC
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
     CLOSE     KEIURWF.
     EVALUATE PARA-DKBN
         WHEN   "4"
             CLOSE     KEIURIL2
         WHEN   OTHER
             CLOSE     KEIURIL3
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