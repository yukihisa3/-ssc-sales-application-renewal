# SJR0360B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJR0360B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受領返品取込機能構築　　　　      *
*    業務名　　　　　　　：　受領返品取込機能　　　            *
*    モジュール名　　　　：　返品計上データ抽出　　　　　　　　*
*    作成日／更新日　　　：　2017/09/12                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　パラメタを受取り、返品累積データ　*
*                            より対象データ抽出する。　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJR0360B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          17/09/12.
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
     SELECT   COMRHEL4  ASSIGN    TO        DA-01-VI-COMRHEL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HE4-F80 HE4-F01
                                            HE4-F07 HE4-F82
                        FILE  STATUS   IS   HE4-STATUS.
*返品累積データ（入力日）
     SELECT   COMRHEL5  ASSIGN    TO        DA-01-VI-COMRHEL5
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HE5-F80 HE5-F01
                                            HE5-F83 HE5-F82
                        FILE  STATUS   IS   HE5-STATUS.
*返品累積データ（計上日）
     SELECT   COMRHEL8  ASSIGN    TO        DA-01-VI-COMRHEL8
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HE8-F80 HE8-F01
                                            HE8-F86 HE8-F82
                        FILE  STATUS   IS   HE8-STATUS.
*返品累積データ（作成日）
     SELECT   COMRHEL9  ASSIGN    TO        DA-01-VI-COMRHEL9
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HE9-F80 HE9-F01
                                            HE9-F30 HE9-F82
                        FILE  STATUS   IS   HE9-STATUS.
*返品抽出データ
     SELECT   COMWHEF   ASSIGN    TO        DA-01-VI-COMWHEL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       WHE-F01 WHE-F03
                                            WHE-F02 WHE-F04
                                            WHE-F05
                        FILE      STATUS    WHE-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    返品累積データ（検収日）
******************************************************************
 FD  COMRHEL4
                        LABEL RECORD   IS   STANDARD.
     COPY     COMRHEF   OF        XFDLIB
              JOINING   HE4  AS   PREFIX.
*
******************************************************************
*    返品累積データ（入力日）
******************************************************************
 FD  COMRHEL5
                        LABEL RECORD   IS   STANDARD.
     COPY     COMRHEF   OF        XFDLIB
              JOINING   HE5  AS   PREFIX.
*
******************************************************************
*    返品累積データ（計上日）
******************************************************************
 FD  COMRHEL8
                        LABEL RECORD   IS   STANDARD.
     COPY     COMRHEF   OF        XFDLIB
              JOINING   HE8  AS   PREFIX.
*
******************************************************************
*    返品累積データ（作成日）
******************************************************************
 FD  COMRHEL9
                        LABEL RECORD   IS   STANDARD.
     COPY     COMRHEF   OF        XFDLIB
              JOINING   HE9  AS   PREFIX.
*
******************************************************************
*    返品抽出データ
******************************************************************
 FD  COMWHEF
                        LABEL RECORD   IS   STANDARD.
     COPY     COMWHEF   OF        XFDLIB
              JOINING   WHE       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  PARA-DFROM              PIC  9(08)     VALUE  ZERO.
 01  PARA-DTO                PIC  9(08)     VALUE  ZERO.
*
 01  WK-ST.
     03  HE4-STATUS        PIC  X(02).
     03  HE5-STATUS        PIC  X(02).
     03  HE8-STATUS        PIC  X(02).
     03  HE9-STATUS        PIC  X(02).
     03  WHE-STATUS        PIC  X(02).
 01  WK-HIDUKE               PIC  9(08)     VALUE  ZERO.
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR0360B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0360B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0360B".
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
 COPY     COMRHEF   OF        XFDLIB.
 LINKAGE                SECTION.
 01  PARA-KEIKBN            PIC   X(01).
 01  PARA-TOKCD             PIC   9(08).
 01  PARA-TANST             PIC   X(02).
 01  PARA-TANED             PIC   X(02).
 01  PARA-DKBN              PIC   X(01).
 01  PARA-AFROM             PIC   9(08).
 01  PARA-ATO               PIC   9(08).
 01  PARA-NFROM             PIC   9(08).
 01  PARA-NTO               PIC   9(08).
 01  PARA-KFROM             PIC   9(08).
 01  PARA-KTO               PIC   9(08).
 01  PARA-SFROM             PIC   9(08).
 01  PARA-STO               PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-KEIKBN
                                       PARA-TOKCD
                                       PARA-TANST
                                       PARA-TANED
                                       PARA-DKBN
                                       PARA-AFROM
                                       PARA-ATO
                                       PARA-NFROM
                                       PARA-NTO
                                       PARA-KFROM
                                       PARA-KTO
                                       PARA-SFROM
                                       PARA-STO.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRHEL4.
     MOVE      "COMRHEL4"   TO   AB-FILE.
     MOVE      HE4-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRHEL5.
     MOVE      "COMRHEL5"   TO   AB-FILE.
     MOVE      HE5-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRHEL8.
     MOVE      "COMRHEL8"   TO   AB-FILE.
     MOVE      HE8-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRHEL9.
     MOVE      "COMRHEL9"   TO   AB-FILE.
     MOVE      HE9-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMWHEF.
     MOVE      "COMWHEL1"   TO   AB-FILE.
     MOVE      WHE-STATUS   TO   AB-STS.
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
*****DISPLAY "PARA-KEIKBN  = " PARA-KEIKBN UPON CONS.
*    DISPLAY "PARA-TOKCD   = " PARA-TOKCD UPON CONS.
*    DISPLAY "PARA-TANST   = " PARA-TANST UPON CONS.
*    DISPLAY "PARA-TANED   = " PARA-TANED UPON CONS.
*    DISPLAY "PARA-DKBN    = " PARA-DKBN UPON CONS.
*    DISPLAY "PARA-AFROM   = " PARA-AFROM UPON CONS.
*    DISPLAY "PARA-ATO     = " PARA-ATO UPON CONS.
*    DISPLAY "PARA-NFROM   = " PARA-NFROM UPON CONS.
*    DISPLAY "PARA-NTO     = " PARA-NTO UPON CONS.
*    DISPLAY "PARA-KFROM   = " PARA-KFROM UPON CONS.
*    DISPLAY "PARA-KTO     = " PARA-KTO UPON CONS.
*    DISPLAY "PARA-SFROM   = " PARA-SFROM UPON CONS.
*****DISPLAY "PARA-STO     = " PARA-STO UPON CONS.
     MOVE     "INIT-SEC"          TO   S-NAME.
     EVALUATE PARA-DKBN
         WHEN   " "
             OPEN     INPUT     COMRHEL4
         WHEN   "1"
             OPEN     INPUT     COMRHEL4
         WHEN   "2"
             OPEN     INPUT     COMRHEL5
         WHEN   "3"
             OPEN     INPUT     COMRHEL8
         WHEN   "4"
             OPEN     INPUT     COMRHEL9
         WHEN   OTHER
             OPEN     INPUT     COMRHEL4
     END-EVALUATE.
     OPEN     OUTPUT    COMWHEF.
     DISPLAY  MSG-START UPON CONS.
*日付区分により、日付範囲指定をセット
     EVALUATE  PARA-DKBN
         WHEN  " "    *>以外
               MOVE   ZERO        TO       PARA-DFROM
               MOVE   99999999    TO       PARA-DTO
         WHEN  "1"    *>検収日
               MOVE   PARA-AFROM  TO       PARA-DFROM
               MOVE   PARA-ATO    TO       PARA-DTO
         WHEN  "2"    *>入力日
               MOVE   PARA-NFROM  TO       PARA-DFROM
               MOVE   PARA-NTO    TO       PARA-DTO
         WHEN  "3"    *>計上日
               MOVE   PARA-KFROM  TO       PARA-DFROM
               MOVE   PARA-KTO    TO       PARA-DTO
         WHEN  "4"    *>作成日
               MOVE   PARA-SFROM  TO       PARA-DFROM
               MOVE   PARA-STO    TO       PARA-DTO
     END-EVALUATE.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
     MOVE     SPACE  TO  HE4-REC HE5-REC HE8-REC HE9-REC.
     INITIALIZE          HE4-REC HE5-REC HE8-REC HE9-REC.
*****計上区分
     EVALUATE PARA-KEIKBN
         WHEN   "1"      *>未確認
*************日付区分
             EVALUATE PARA-DKBN
                 WHEN   " "
                  MOVE   SPACE    TO   HE4-F80
                  MOVE   PARA-TOKCD TO HE4-F01
                 WHEN   "1"
                  MOVE   SPACE    TO   HE4-F80
                  MOVE   PARA-TOKCD TO HE4-F01
                 WHEN   "2"
                  MOVE   SPACE    TO   HE5-F80
                  MOVE   PARA-TOKCD TO HE5-F01
                 WHEN   "3"
                  MOVE   SPACE    TO   HE8-F80
                  MOVE   PARA-TOKCD TO HE8-F01
                 WHEN   "4"
                  MOVE   SPACE    TO   HE9-F80
                  MOVE   PARA-TOKCD TO HE9-F01
                 WHEN   OTHER
                  MOVE   SPACE    TO   HE4-F80
                  MOVE   PARA-TOKCD TO HE4-F01
             END-EVALUATE
         WHEN   "2"      *>未計上
             EVALUATE PARA-DKBN
                 WHEN   " "
                  MOVE   "1"        TO HE4-F80
                  MOVE   PARA-TOKCD TO HE4-F01
                  MOVE   ZERO       TO HE4-F07
                 WHEN   "1"
                  MOVE   "1"        TO HE4-F80
                  MOVE   PARA-TOKCD TO HE4-F01
                  MOVE   PARA-DFROM TO HE4-F07
                 WHEN   "2"
                  MOVE   "1"        TO HE5-F80
                  MOVE   PARA-TOKCD TO HE5-F01
                  MOVE   PARA-DFROM TO HE5-F83
                 WHEN   "3"
                  MOVE   "1"        TO HE8-F80
                  MOVE   PARA-TOKCD TO HE8-F01
                  MOVE   PARA-DFROM TO HE8-F86
                 WHEN   "4"
                  MOVE   "1"        TO HE9-F80
                  MOVE   PARA-TOKCD TO HE9-F01
                  MOVE   PARA-DFROM TO HE9-F30
                 WHEN   OTHER
                  MOVE   "1"        TO HE4-F80
                  MOVE   PARA-TOKCD TO HE4-F01
                  MOVE   ZERO       TO HE4-F07
             END-EVALUATE
         WHEN   "3"      *>計上済
             EVALUATE PARA-DKBN
                 WHEN   " "
                  MOVE   "1"        TO HE4-F80
                  MOVE   PARA-TOKCD TO HE4-F01
                  MOVE   ZERO       TO HE4-F07
                 WHEN   "1"
                  MOVE   "1"        TO HE4-F80
                  MOVE   PARA-TOKCD TO HE4-F01
                  MOVE   PARA-DFROM TO HE4-F07
                 WHEN   "2"
                  MOVE   "1"        TO HE5-F80
                  MOVE   PARA-TOKCD TO HE5-F01
                  MOVE   PARA-DFROM TO HE5-F83
                 WHEN   "3"
                  MOVE   "1"        TO HE8-F80
                  MOVE   PARA-TOKCD TO HE8-F01
                  MOVE   PARA-DFROM TO HE8-F86
                 WHEN   "4"
                  MOVE   "1"        TO HE9-F80
                  MOVE   PARA-TOKCD TO HE9-F01
                  MOVE   PARA-DFROM TO HE9-F30
                 WHEN   OTHER
                  MOVE   "1"        TO HE4-F80
                  MOVE   PARA-TOKCD TO HE4-F01
                  MOVE   ZERO       TO HE4-F07
             END-EVALUATE
         WHEN   "4"      *>取消分
             EVALUATE PARA-DKBN
                 WHEN   " "
                  MOVE   "9"        TO HE4-F80
                  MOVE   PARA-TOKCD TO HE4-F01
                  MOVE   ZERO       TO HE4-F07
                 WHEN   "1"
                  MOVE   "9"        TO HE4-F80
                  MOVE   PARA-TOKCD TO HE4-F01
                  MOVE   PARA-AFROM TO HE4-F07
                 WHEN   "2"
                  MOVE   "9"        TO HE5-F80
                  MOVE   PARA-TOKCD TO HE5-F01
                  MOVE   PARA-NFROM TO HE5-F83
                 WHEN   "3"
                  MOVE   "9"        TO HE8-F80
                  MOVE   PARA-TOKCD TO HE8-F01
                  MOVE   PARA-KFROM TO HE8-F86
                 WHEN   "4"
                  MOVE   "9"        TO HE9-F80
                  MOVE   PARA-TOKCD TO HE9-F01
                  MOVE   PARA-SFROM TO HE9-F30
                 WHEN   OTHER
                  MOVE   "9"        TO HE4-F80
                  MOVE   PARA-TOKCD TO HE4-F01
                  MOVE   ZERO       TO HE4-F07
             END-EVALUATE
     END-EVALUATE.
     EVALUATE PARA-DKBN
         WHEN   " "
            START    COMRHEL4  KEY  >= HE4-F80 HE4-F01 HE4-F07
                                       HE4-F82
                INVALID   KEY
                   MOVE      9    TO   END-FG
                   GO   TO   INIT-EXIT
             END-START
         WHEN   "1"
            START    COMRHEL4  KEY  >= HE4-F80 HE4-F01 HE4-F07
                                       HE4-F82
                INVALID   KEY
                   MOVE      9    TO   END-FG
                   GO   TO   INIT-EXIT
             END-START
         WHEN   "2"
            START    COMRHEL5  KEY  >= HE5-F80 HE5-F01 HE5-F83
                                       HE5-F82
                INVALID   KEY
                   MOVE      9    TO   END-FG
                   GO   TO   INIT-EXIT
             END-START
         WHEN   "3"
            START    COMRHEL8  KEY  >= HE8-F80 HE8-F01 HE8-F86
                                       HE8-F82
                INVALID   KEY
                   MOVE      9    TO   END-FG
                   GO   TO   INIT-EXIT
             END-START
         WHEN   "4"
************DISPLAY "HE9-F80 1 = " HE9-F80  UPON CONS
*           DISPLAY "HE9-F01 1 = " HE9-F01  UPON CONS
*           DISPLAY "HE9-F30 1 = " HE9-F30  UPON CONS
************DISPLAY "HE9-F82 1 = " HE9-F82  UPON CONS
            START    COMRHEL9  KEY  >= HE9-F80 HE9-F01 HE9-F30
                                       HE9-F82
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
             READ     COMRHEL4
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HE4-REC   TO  REC
                                MOVE      HE4-F07   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "1"
             READ     COMRHEL4
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HE4-REC   TO  REC
                                MOVE      HE4-F07   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "2"
             READ     COMRHEL5
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HE5-REC   TO  REC
                                MOVE      HE5-F83   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "3"
             READ     COMRHEL8
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HE8-REC   TO  REC
                                MOVE      HE8-F86   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "4"
             READ     COMRHEL9
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HE9-REC   TO  REC
                                MOVE      HE8-F30   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
             END-READ
************DISPLAY "HE9-F80 2 = " HE9-F80  UPON CONS
*           DISPLAY "HE9-F01 2 = " HE9-F01  UPON CONS
*           DISPLAY "HE9-F30 2 = " HE9-F30  UPON CONS
*           DISPLAY "HE9-F82 2 = " HE9-F82  UPON CONS
************DISPLAY "END-FG  2 = " END-FG   UPON CONS
         WHEN   OTHER
             READ     COMRHEL4
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HE4-REC   TO  REC
                                MOVE      HE4-F07   TO  WK-HIDUKE
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
 MAIN-000.
     EVALUATE PARA-KEIKBN
         WHEN   "1"         *>未確認
*            計上区分
             IF     ( F80   NOT = SPACE )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
             ELSE
                IF  ( F01   NOT = PARA-TOKCD )
                      MOVE      9         TO   END-FG
                      GO        TO        MAIN-EXIT
                ELSE
                      GO        TO        MAIN-001
                END-IF
             END-IF
         WHEN   "2"         *>未計上
*            計上区分
             IF     ( F80   NOT = "1" )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
             END-IF
*            取引先ＣＤ
             IF     ( F01   NOT =  PARA-TOKCD )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
              END-IF
*            計上日チェック
             IF     ( F86   NOT =   ZERO )
                GO        TO        MAIN-010
             END-IF
         WHEN   "3"         *>計上済
*            計上区分
             IF     ( F80   NOT = "1" )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
             END-IF
*            取引先ＣＤ
             IF     ( F01   NOT =  PARA-TOKCD )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
              END-IF
*************DISPLAY "F86 = " F86 UPON CONS
*            計上日チェック
             IF     ( F86       =   ZERO )
                GO        TO        MAIN-010
             END-IF
         WHEN   "4"         *>取消分
*            計上区分
             IF     ( F80   NOT = "9" )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
             END-IF
*            取引先ＣＤ
             IF     ( F01   NOT =  PARA-TOKCD )
                MOVE      9         TO   END-FG
                GO        TO        MAIN-EXIT
              END-IF
     END-EVALUATE.
 MAIN-HIZUKE.
*    日付TO
*****DISPLAY "PARA-DTO  = " PARA-DTO  UPON CONS.
*****DISPLAY "WK-HIDUKE = " WK-HIDUKE UPON CONS.
     IF     ( PARA-DTO      <    WK-HIDUKE )
           MOVE      9         TO   END-FG
           GO        TO        MAIN-EXIT
     END-IF.
 MAIN-TAN.
*    担当者
*****DISPLAY "PARA-TANST = " PARA-TANST UPON CONS
*    DISPLAY "PARA-TANED = " PARA-TANED UPON CONS
*****DISPLAY "F82        = " F82        UPON CONS
     IF     ( PARA-TANST  <=  F82 ) AND
            ( PARA-TANED  >=  F82 )
              CONTINUE
     ELSE
                GO        TO        MAIN-010
     END-IF.
*
 MAIN-001.
*返品抽出データ出力
     MOVE     SPACE          TO   WHE-REC.
     INITIALIZE                   WHE-REC.
     MOVE     REC            TO   WHE-REC.
     WRITE    WHE-REC.
     ADD      1              TO   WRT-CNT.
*
 MAIN-010.
*
     EVALUATE PARA-DKBN
         WHEN   " "
             READ     COMRHEL4
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HE4-REC   TO  REC
                                MOVE      HE4-F07   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "1"
             READ     COMRHEL4
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HE4-REC   TO  REC
                                MOVE      HE4-F07   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "2"
             READ     COMRHEL5
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HE5-REC   TO  REC
                                MOVE      HE5-F83   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "3"
             READ     COMRHEL8
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HE8-REC   TO  REC
                                MOVE      HE8-F86   TO  WK-HIDUKE
                                ADD       1    TO   RD-CNT
             END-READ
         WHEN   "4"
             READ     COMRHEL9
                      AT END    MOVE      9         TO  END-FG
                      NOT AT END
                                MOVE      HE9-REC   TO  REC
                                MOVE      HE8-F30   TO  WK-HIDUKE
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
     CLOSE     COMWHEF.
     EVALUATE PARA-DKBN
         WHEN   " "
             CLOSE     COMRHEL4
         WHEN   "1"
             CLOSE     COMRHEL4
         WHEN   "2"
             CLOSE     COMRHEL5
         WHEN   "3"
             CLOSE     COMRHEL8
         WHEN   "4"
             CLOSE     COMRHEL9
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
