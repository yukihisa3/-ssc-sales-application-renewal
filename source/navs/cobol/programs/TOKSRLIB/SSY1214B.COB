****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　　　　　  *
*    サブシステム　　　　：　コーナン　　　ＥＤＩ　　　　　　  *
*    モジュール名　　　　：　ＰＯＲケース用ピッキングリスト　　*
*    　　　　　　　　　　　　データ抽出　　　　　　　　　　　　*
*    作成日／作成者　　　：　2021/01/18 INOUE                  *
*    処理概要　　　　　　：　条件に従い、リスト発行対象　　　　*
*                            データを抽出する　　　　　　　　　*
*    更新履歴                                                  *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY1214B.
*                  流用:SSY1211B
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/01/18.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 基本情報ファイル >>--*
     SELECT   KNJOHOF   ASSIGN    TO        DA-01-VI-KNJOHOL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JOH-F001  JOH-F002
                                            JOH-F003  JOH-F004
                                            JOH-FA02  JOH-FA07
                                            JOH-FA08  JOH-FA03
                                            JOH-FA04  JOH-FA05
                                            JOH-FC01  JOH-FA06
                                            JOH-FB05
                        FILE  STATUS   IS   JOH-STATUS.
*----<< 売上伝票ファイル >>----*
     SELECT   SHTDENLA  ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*----<< ピッキングリスト発行ワーク >>----*
     SELECT   KNPKLCWK  ASSIGN    TO        DA-01-VS-KNPKLCWK
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    OUT-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    基本情報ファイル
******************************************************************
 FD  KNJOHOF            LABEL     RECORD   IS   STANDARD.
     COPY     KNJOHOF   OF        XFDLIB
              JOINING   JOH       PREFIX.
******************************************************************
*    売上伝票ファイル
******************************************************************
 FD  SHTDENLA           LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
******************************************************************
*    ピッキングリスト発行ワーク
******************************************************************
 FD  KNPKLCWK           LABEL     RECORD   IS   STANDARD.
     COPY     KNPKLCWK  OF        XFDLIB
              JOINING   OUT  AS   PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  WRT-CNT             PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  KNPKLCWK-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  SHTDENLA-INV-FLG    PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
 01  WK-PARA-IN-TORICD       PIC  9(08)     VALUE  ZERO.
*
*退避
 01  BK-C128-REC             PIC  X(128)    VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME           PIC   9(08)  VALUE  ZERO.
*日付の編集
 01  WK-HDATE.
     03  WK-HDATE1         PIC 9(02).
     03  WK-HDATE2         PIC 9(06).
 01  WK-NDATE.
     03  WK-NDATE1         PIC 9(02).
     03  WK-NDATE2         PIC 9(06).
*
 01  WK-ST.
     03  JOH-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  IN-STATUS         PIC  X(02).
     03  OUT-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY1214B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1214B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1214B".
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
 01  PARA-IN-JDATE             PIC   9(08).
 01  PARA-IN-JTIME             PIC   9(04).
 01  PARA-IN-TORICD            PIC   9(08).
 01  PARA-IN-SOKO              PIC   X(02).
 01  PARA-IN-SCENTER           PIC   9(05).
 01  PARA-IN-ECENTER           PIC   9(05).
 01  PARA-IN-SROUTE            PIC   9(02).
 01  PARA-IN-EROUTE            PIC   9(02).
 01  PARA-IN-SBUMON            PIC   9(02).
 01  PARA-IN-EBUMON            PIC   9(02).
 01  PARA-IN-HDATE             PIC   9(08).
 01  PARA-IN-NDATE             PIC   9(08).
 01  PARA-IN-STANA             PIC   X(06).
 01  PARA-IN-ETANA             PIC   X(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING
                                  PARA-IN-JDATE
                                  PARA-IN-JTIME
                                  PARA-IN-TORICD
                                  PARA-IN-SOKO
                                  PARA-IN-SCENTER
                                  PARA-IN-ECENTER
                                  PARA-IN-SROUTE
                                  PARA-IN-EROUTE
                                  PARA-IN-SBUMON
                                  PARA-IN-EBUMON
                                  PARA-IN-HDATE
                                  PARA-IN-NDATE
                                  PARA-IN-STANA
                                  PARA-IN-ETANA.
*
******************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNJOHOF.
     MOVE      "KNJOHOL2 "   TO   AB-FILE.
     MOVE      JOH-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLA.
     MOVE      "SHTDENLA"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKLCWK.
     MOVE      "KNPKLCWK"   TO   AB-FILE.
     MOVE      OUT-STATUS   TO   AB-STS.
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
     OPEN     INPUT     KNJOHOF  SHTDENLA.
     OPEN     OUTPUT    KNPKLCWK.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG SHTDENLA-INV-FLG.
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
     IF  PARA-IN-TORICD  =  23631
         MOVE    2363           TO   WK-PARA-IN-TORICD
     ELSE
         MOVE    PARA-IN-TORICD TO   WK-PARA-IN-TORICD
     END-IF.
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
*    基本情報ファイル　スタート
*T
*    DISPLAY  "PARA-IN-JDATE  =" PARA-IN-JDATE UPON CONS.
*    DISPLAY  "PARA-IN-JTIME  =" PARA-IN-JTIME UPON CONS.
*    DISPLAY  "PARA-IN-TORICD =" PARA-IN-TORICD UPON CONS.
*    DISPLAY  "PARA-IN-SOKO   =" PARA-IN-SOKO   UPON CONS.
*    DISPLAY  "PARA-IN-SCENTER=" PARA-IN-SCENTER UPON CONS.
*    DISPLAY  "PARA-IN-ECENTER=" PARA-IN-ECENTER UPON CONS.
*    DISPLAY  "PARA-IN-SROUTE =" PARA-IN-SROUTE  UPON CONS.
*    DISPLAY  "PARA-IN-EROUTE =" PARA-IN-EROUTE  UPON CONS.
*    DISPLAY  "PARA-IN-SBUMON =" PARA-IN-SBUMON  UPON CONS.
*    DISPLAY  "PARA-IN-EBUMON =" PARA-IN-EBUMON  UPON CONS.
*    DISPLAY  "PARA-IN-HDATE  =" PARA-IN-HDATE   UPON CONS.
*    DISPLAY  "PARA-IN-NDATE  =" PARA-IN-NDATE   UPON CONS.
*    DISPLAY  "PARA-IN-STANA  =" PARA-IN-STANA   UPON CONS.
*    DISPLAY  "PARA-IN-ETANA  =" PARA-IN-ETANA   UPON CONS.
*T
     MOVE      SPACE           TO   JOH-REC.
     INITIALIZE                     JOH-REC.
     MOVE      PARA-IN-JDATE   TO   JOH-F001.
     MOVE      PARA-IN-JTIME   TO   JOH-F002.
     MOVE      PARA-IN-TORICD  TO   JOH-F003.
     MOVE      PARA-IN-SOKO    TO   JOH-F004.
     MOVE      PARA-IN-SCENTER TO   JOH-FA02.
     MOVE      PARA-IN-TORICD  TO   JOH-FA07.
     IF  PARA-IN-TORICD  =  23631
               MOVE  2363      TO   JOH-FA07
     END-IF.
     MOVE      PARA-IN-SROUTE  TO   JOH-FA08.
     MOVE      PARA-IN-SBUMON  TO   JOH-FA03.
     MOVE      PARA-IN-HDATE   TO   WK-HDATE.
     MOVE      WK-HDATE2       TO   JOH-FA04.
     MOVE      PARA-IN-NDATE   TO   WK-NDATE.
     MOVE      WK-NDATE2       TO   JOH-FA05.
     MOVE      PARA-IN-STANA   TO   JOH-FC01.
     MOVE      ZERO            TO   JOH-FA06.
     MOVE      SPACE           TO   JOH-FB05.
     START     KNJOHOF   KEY   >=   JOH-F001  JOH-F002
                                    JOH-F003  JOH-F004
                                    JOH-FA02  JOH-FA07
                                    JOH-FA08  JOH-FA03
                                    JOH-FA04  JOH-FA05
                                    JOH-FC01  JOH-FA06
                                    JOH-FB05
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*
*    基本情報ファイル読込み
     PERFORM KNJOHOF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 KNJOHOF-READ-SEC    SECTION.
*
     MOVE    "KNJOHOF-READ-SEC"    TO  S-NAME.
*
     READ     KNJOHOF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KNJOHOF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 KNJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"              TO   S-NAME.
*
*対象データチェック
 MAIN010.
*    ケースのみ対象
     IF   JOH-FB12          =     1
          CONTINUE
     ELSE
          GO             TO   MAIN999
     END-IF.
 MAIN011.
     IF ( PARA-IN-JDATE     =    JOH-F001 ) AND
        ( PARA-IN-JTIME     =    JOH-F002 ) AND
        ( PARA-IN-TORICD    =    JOH-F003 )
*       ( PARA-IN-TORICD    =    JOH-FA07 )
        CONTINUE
     ELSE
        MOVE  "END"      TO   END-FLG
        GO               TO   MAIN-EXIT
     END-IF.
*
 MAIN012.
     IF  PARA-IN-SOKO   NOT =  SPACE
         IF     JOH-F004     =      PARA-IN-SOKO
                CONTINUE
         ELSE
                MOVE  "END"      TO   END-FLG
                GO               TO   MAIN-EXIT
         END-IF
     END-IF.
*
 MAIN013.
     IF  PARA-IN-SCENTER   IS     NUMERIC
         IF     JOH-FA02    >=      PARA-IN-SCENTER
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN014.
     IF  PARA-IN-ECENTER   IS     NUMERIC
         IF     JOH-FA02    <=  PARA-IN-ECENTER
                CONTINUE
         ELSE
                GO          TO   MAIN999
        END-IF
     END-IF.
*
 MAIN015.
     IF  PARA-IN-TORICD IS     NUMERIC
*********IF     JOH-FA07     =  PARA-IN-TORICD
         IF     JOH-FA07     =  WK-PARA-IN-TORICD
                CONTINUE
         ELSE
                GO          TO   MAIN999
        END-IF
     END-IF.
*
 MAIN016.
     IF  PARA-IN-SROUTE IS     NUMERIC
         IF     JOH-FA08    >=  PARA-IN-SROUTE
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN017.
     IF  PARA-IN-EROUTE IS     NUMERIC
         IF     JOH-FA08    <=  PARA-IN-EROUTE
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN018.
     IF  PARA-IN-SBUMON IS     NUMERIC
         IF     JOH-FA03    >=  PARA-IN-SBUMON
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN019.
     IF  PARA-IN-EBUMON IS     NUMERIC
         IF     JOH-FA03    <=  PARA-IN-EBUMON
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN020.
     IF  PARA-IN-HDATE  NOT = ZERO
         IF     JOH-FA04     =  WK-HDATE2
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN021.
     IF  PARA-IN-NDATE  NOT = ZERO
         IF     JOH-FA05     =  WK-NDATE2
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN022.
     IF  PARA-IN-STANA  NOT =  SPACE
         IF     JOH-FC01    >=  PARA-IN-STANA
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN023.
     IF  PARA-IN-ETANA  NOT =  SPACE
         IF     JOH-FC01    <=  PARA-IN-ETANA
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
*売上伝票ファイル　検索
 MAIN024.
     MOVE     PARA-IN-JDATE(1:4)     TO  DEN-F461.
     MOVE     PARA-IN-JDATE(5:2)     TO  DEN-F462.
     MOVE     PARA-IN-JDATE(7:2)     TO  DEN-F463.
     MOVE     PARA-IN-JTIME(1:2)     TO  DEN-F471.
     MOVE     PARA-IN-JTIME(3:2)     TO  DEN-F472.
     MOVE     PARA-IN-TORICD         TO  DEN-F01.
     MOVE     JOH-F004               TO  DEN-F48.
     MOVE     JOH-FA06               TO  DEN-F02.
     MOVE     0                      TO  DEN-F04.
     MOVE     40                     TO  DEN-F051.
     MOVE     JOH-FA02               TO  DEN-F07.
*    MOVE     JOH-FA05               TO  DEN-F112.
     MOVE    "3"                     TO  LINK-IN-KBN.
     MOVE     JOH-FA05               TO  LINK-IN-YMD6.
     CALL    "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   DEN-F112
     ELSE
         MOVE    ZERO           TO   DEN-F112
     END-IF.
     MOVE     1                      TO  DEN-F03.
     READ     SHTDENLA
       INVALID
             DISPLAY NC"売上伝票ファイルなし！" UPON CONS
             DISPLAY NC"バッチ（日）＝" PARA-IN-JDATE UPON CONS
             DISPLAY NC"バッチ（時）＝" PARA-IN-JTIME UPON CONS
             DISPLAY NC"バッチ（取）＝" PARA-IN-TORICD UPON CONS
             DISPLAY NC"振分倉庫　　＝" JOH-F004       UPON CONS
             DISPLAY NC"伝票番号　　＝" JOH-FA06       UPON CONS
             DISPLAY NC"相殺区分　　＝" "0"            UPON CONS
             DISPLAY NC"伝区コード　＝" "40"           UPON CONS
             DISPLAY NC"店舗コード　＝" JOH-FA02       UPON CONS
             DISPLAY NC"納品日　　　＝" JOH-FA05       UPON CONS
             DISPLAY NC"行　　　　　＝" "1"            UPON CONS
             MOVE    4010            TO  PROGRAM-STATUS
             MOVE    "END"           TO  END-FLG
             GO                      TO  MAIN-EXIT
     END-READ.
*
     MOVE    JOH-REC                 TO  OUT-REC.
     MOVE    DEN-F27D                TO  OUT-FC12.
     WRITE   OUT-REC.
     ADD     1                       TO  WRT-CNT.
*
 MAIN999.
*    基本情報ファイル読込み
     PERFORM KNJOHOF-READ-SEC.
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
     DISPLAY  NC"ケースラベルＦ" "IN  = " READ-CNT  UPON CONS.
     DISPLAY  NC"抽出データ　　" "OUT = " WRT-CNT   UPON CONS.
*    DISPLAY  NC"抽出対象外　　" "SKIP= " SKIP-CNT  UPON CONS.
*
     CLOSE     KNJOHOF  KNPKLCWK  SHTDENLA.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
