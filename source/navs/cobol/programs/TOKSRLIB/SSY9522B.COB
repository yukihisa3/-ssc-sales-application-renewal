****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩ　出荷　　　　　　　　　　　*
*    サブシステム　　　　：　ヨドバシ　　　ＥＤＩ　　　　　　　*
*    モジュール名　　　　：　ヨドバシ出荷通知番号採番　　　　　*
*    作成日／作成者　　　：　2023/03/17 NAV INOUE              *
*    処理概要　　　　　　：　ヨドバシへ返送する　　　　　　　　*
*                            出荷通知番号を採番する。　　　　　*
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY9522B.
*                  流用:SSY9473B.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/08/13.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*出荷通知抽出データ
     SELECT   YODSNDL4  ASSIGN    TO        DA-01-VI-YODSNDL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       IS   IN-F11
                                                 IN-F12
                                                 IN-F13
                                                 IN-F16
                                                 IN-F10
                                                 IN-F02
                                                 IN-F09
                        FILE      STATUS    IN-STATUS.
*店舗マスタ
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52
                                            TEN-F011
                        FILE      STATUS    TEN-STATUS.
*基本情報ファイル
     SELECT   YODJOHL2  ASSIGN    TO        DA-01-VI-YODJOHL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JOH-F15
                                            JOH-F16
                                            JOH-F17
                                            JOH-F03
                                            JOH-F18
                                            JOH-F19
                        FILE      STATUS    JOH-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*出荷通知抽出データ
******************************************************************
 FD  YODSNDL4            LABEL RECORD   IS   STANDARD.
     COPY     YODSNDL4   OF        XFDLIB
              JOINING    IN   AS   PREFIX.
*****************************************************************
*店舗マスタ
*****************************************************************
 FD  TENMS1
                       LABEL RECORD   IS   STANDARD.
     COPY              TENMS1         OF   XFDLIB
     JOINING           TEN            AS   PREFIX.
*
******************************************************************
*基本情報ファイル
******************************************************************
 FD  YODJOHL2            LABEL RECORD   IS   STANDARD.
     COPY     YODJOHL2   OF        XFDLIB
              JOINING    JOH       PREFIX.

*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  IN-STATUS           PIC  X(02).
     03  TEN-STATUS          PIC  X(02).
     03  JOH-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  TENMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
*カウンター領域
 01  WK-CNT.
     03  RD-CNT              PIC  9(08)     VALUE  ZERO.
     03  SEL-CNT             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT             PIC  9(08)     VALUE  ZERO.
     03  CNT-TEN-F18         PIC  9(07)     VALUE  ZERO.
     03  SET-NO.
         05  SET-NO-1        PIC  9(04)     VALUE  ZERO.
         05  SET-NO-2        PIC  9(06)     VALUE  ZERO.
     03  SET-NO-R
         REDEFINES SET-NO    PIC  9(10).
*
*ブレイクキー
 01  BRK-IN-F10              PIC  X(17)     VALUE  SPACE.
 01  BRK-IN-F02              PIC  9(08)     VALUE  ZERO.
 01  BRK-IN-F09              PIC  X(04)     VALUE  SPACE.
 01  WRK-IN-F09              PIC  X(04)     VALUE  SPACE.
*
*数字編集用
 01  WK-TANKA-X.
     03  WK-TANKA            PIC  9(07).99.
 01  WK-RITU-X.
     03  WK-RITU             PIC  9(03).9.
*システム日付編集領域
 01  WK-AREA.
     03  SYS-DATE            PIC  9(06).
     03  SYS-DATEW           PIC  9(08).
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY9522B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY9522B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY9522B".
         05  FILLER          PIC  X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  AB-FILE         PIC  X(08).
         05  FILLER          PIC  X(06)  VALUE " ST = ".
         05  AB-STS          PIC  X(02).
         05  FILLER          PIC  X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  FILLER          PIC  X(07)  VALUE " SEC = ".
         05  S-NAME          PIC  X(30).
     03  MSG-IN.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 元データ読込件数   = ".
         05  MSG-IN01        PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(23)  VALUE
                             " 通知番号採番件数  = ".
         05  MSG-OUT01       PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
*日付変換サブルーチン用領域
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC   X(01).
     03  LINK-IN-YMD6        PIC   9(06).
     03  LINK-IN-YMD8        PIC   9(08).
     03  LINK-OUT-RET        PIC   X(01).
     03  LINK-OUT-YMD8       PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-BDATE           PIC   9(08).
 01  PARA-IN-BTIME           PIC   9(04).
 01  PARA-IN-BTORI           PIC   9(08).
 01  PARA-IN-SOKO            PIC   X(02).
 01  PARA-IN-NDATES          PIC   9(08).
 01  PARA-IN-NDATEE          PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING
                                  PARA-IN-BDATE
                                  PARA-IN-BTIME
                                  PARA-IN-BTORI
                                  PARA-IN-SOKO
                                  PARA-IN-NDATES
                                  PARA-IN-NDATEE.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODSNDL4.
     MOVE      "YODSNDL4"   TO   AB-FILE.
     MOVE      IN-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODJOHL2.
     MOVE      "YODJOHL2"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
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
*
*初期処理
     PERFORM  INIT-SEC.
*採番
     PERFORM  MAIN1-SEC
              UNTIL     END-FLG   =  "END".
*終了処理
     PERFORM  END-SEC.
     STOP  RUN.
*
 GENERAL-PROCESS-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     DISPLAY  MSG-START UPON CONS.
*
*    DISPLAY  "JOBKIND=" PARA-IN-JOBKIND  UPON CONS.
*
     OPEN     I-O       YODSNDL4.
     OPEN     I-O       TENMS1.
     OPEN     I-O       YODJOHL2.
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
     MOVE    "20"       TO   SYS-DATEW(1:2).
     MOVE    SYS-DATE   TO   SYS-DATEW(3:6).
*
*出荷通知抽出データＳＴＡＲＴ
     PERFORM YODSNDL4-START-SEC.
*
*判定
     IF   END-FLG  =  "END"
          DISPLAY "＃＃採番対象データ無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
     END-IF.
*
*出荷通知抽出データ読込み
     PERFORM YODSNDL4-READ-SEC.
*
*終了判定
     IF   END-FLG  =  "END"
          DISPLAY "＃＃採番対象データ無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
     ELSE
*       出荷先コード保管
          MOVE     IN-F10   TO    BRK-IN-F10
          MOVE     IN-F02   TO    BRK-IN-F02
          MOVE     IN-F09(5:4)   TO    BRK-IN-F09
     END-IF.
*
*店舗マスタ（出荷先コード）検索
     MOVE     IN-F13        TO    TEN-F52.
     MOVE     IN-F10(1:4)   TO    TEN-F011.
     READ     TENMS1
          INVALID
              DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
              DISPLAY NC"＃　店舗マスタなし　＃" UPON CONS
              DISPLAY NC"＃　　コード＝　" IN-F10(1:5) UPON CONS
              DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
              MOVE    4010    TO     PROGRAM-STATUS
              STOP    RUN
          NOT INVALID
              MOVE    TEN-F18 TO     CNT-TEN-F18
              COMPUTE CNT-TEN-F18 =  CNT-TEN-F18 + 1
              IF      CNT-TEN-F18 >  999999
                      MOVE  1     TO CNT-TEN-F18
              END-IF
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　出荷抽出データＳＴＡＲＴ　　　　　　　　　　　　　　　　　*
****************************************************************
 YODSNDL4-START-SEC    SECTION.
*
     MOVE    "YODSNDL4-START-SEC"   TO   S-NAME.
*
     MOVE     SPACE                 TO    IN-REC.
     INITIALIZE                           IN-REC.
     MOVE     PARA-IN-BDATE         TO    IN-F11.
     MOVE     PARA-IN-BTIME         TO    IN-F12.
     MOVE     PARA-IN-BTORI         TO    IN-F13.
     MOVE     PARA-IN-SOKO          TO    IN-F16.
     MOVE     PARA-IN-NDATES        TO    IN-F02.
*
 YODSNDL4-START-01.
     START    YODSNDL4 KEY IS >= IN-F11 IN-F12 IN-F13 IN-F16
                                 IN-F10 IN-F02 IN-F09
              INVALID
                  MOVE     "END"    TO  END-FLG
                  GO                TO  YODSNDL4-START-EXIT
     END-START.
*
 YODSNDL4-START-EXIT.
     EXIT.
****************************************************************
*　　出荷抽出データ読込　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 YODSNDL4-READ-SEC    SECTION.
*
     MOVE    "YODSNDL4-READ-SEC"    TO   S-NAME.
*
 YODSNDL4-READ-01.
     READ     YODSNDL4
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  YODSNDL4-READ-EXIT
     END-READ.
*
     ADD      1     TO     RD-CNT.
*
*条件判定
*  全件対象
     ADD      1     TO     SEL-CNT.
*
 YODSNDL4-READ-02.
*    DISPLAY "PARA-IN-BDATE = " PARA-IN-BDATE UPON CONS.
*    DISPLAY "PARA-IN-BTIME = " PARA-IN-BTIME UPON CONS.
*    DISPLAY "PARA-IN-BTORI = " PARA-IN-BTORI UPON CONS.
*    DISPLAY "IN-F11        = " IN-F11        UPON CONS.
*    DISPLAY "IN-F12        = " IN-F12        UPON CONS.
*    DISPLAY "IN-F13        = " IN-F13        UPON CONS.
     IF  ( IN-F11    =      PARA-IN-BDATE ) AND
         ( IN-F12    =      PARA-IN-BTIME ) AND
         ( IN-F13    =      PARA-IN-BTORI )
           CONTINUE
     ELSE
           MOVE    "END"    TO  END-FLG
           GO               TO  YODSNDL4-READ-EXIT
     END-IF.
*
 YODSNDL4-READ-03.
     IF    PARA-IN-SOKO  =  SPACE
           CONTINUE
     ELSE
           IF   IN-F16   =  PARA-IN-SOKO
                CONTINUE
           ELSE
                MOVE    "END"    TO  END-FLG
                GO               TO  YODSNDL4-READ-EXIT
           END-IF
     END-IF.
*
 YODSNDL4-READ-04.
     IF  ( IN-F02       >=  PARA-IN-NDATES ) AND
         ( IN-F02       <=  PARA-IN-NDATEE )
           CONTINUE
     ELSE
           GO               TO  YODSNDL4-READ-01
     END-IF.
*
 YODSNDL4-READ-05.
     IF  ( IN-F18        =  ZERO           ) AND
         ( IN-F20        =  ZERO           )
           CONTINUE
     ELSE
           GO               TO  YODSNDL4-READ-01
     END-IF.
*
 YODSNDL4-READ-06.
*配送場所確認
     IF   IN-F09(5:4)  = "0010" OR "0050"
          MOVE   IN-F09(5:4)  TO    WRK-IN-F09
     ELSE
          MOVE   "9999"       TO    WRK-IN-F09
     END-IF.
*
 YODSNDL4-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理１　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN1-SEC     SECTION.
*
     MOVE    "MAIN1-SEC"     TO      S-NAME.
*
*  出荷先コード(出荷通知番号採番単位)　ブレイク判定
     IF       IN-F10   =  BRK-IN-F10
     AND      IN-F02   =  BRK-IN-F02
     AND      WRK-IN-F09  =  BRK-IN-F09
              CONTINUE
     ELSE
              MOVE     CNT-TEN-F18   TO   TEN-F18
              REWRITE  TEN-REC
              MOVE     IN-F13        TO   TEN-F52
              MOVE     IN-F10(1:4)   TO   TEN-F011
              READ     TENMS1
                  INVALID
                       DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃"
                                                 UPON CONS
                       DISPLAY NC"＃　店舗マスタなし　＃"
                                                 UPON CONS
                       DISPLAY NC"＃　　コード＝　" IN-F10(1:5)
                                                 UPON CONS
                       DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃"
                                                 UPON CONS
                       MOVE    4010    TO     PROGRAM-STATUS
                       STOP    RUN
                  NOT INVALID
                       MOVE    TEN-F18 TO      CNT-TEN-F18
                       COMPUTE CNT-TEN-F18  =  CNT-TEN-F18 + 1
                       IF      CNT-TEN-F18  >  999999
                               MOVE  1      TO CNT-TEN-F18
                       END-IF
*                      出荷先コード保管
                       MOVE     IN-F10   TO    BRK-IN-F10
                       MOVE     IN-F02   TO    BRK-IN-F02
                       IF   IN-F09(5:4)  = "0010" OR "0050"
                            MOVE   IN-F09(5:4)  TO    BRK-IN-F09
                       ELSE
                            MOVE   "9999"       TO    BRK-IN-F09
                       END-IF
              END-READ
     END-IF.
*
*出荷通知番号
     MOVE    IN-F10(1:4)       TO  SET-NO-1.
     MOVE    CNT-TEN-F18       TO  SET-NO-2.
*出荷通知番号更新→抽出ファイル
     MOVE    SET-NO-R          TO  IN-F17.
     REWRITE IN-REC.
*
*出荷通知番号更新→基本情報ファイル
     MOVE    IN-F11            TO  JOH-F15.
     MOVE    IN-F12            TO  JOH-F16.
     MOVE    IN-F13            TO  JOH-F17.
     MOVE    IN-F06            TO  JOH-F03.
     MOVE    IN-F14            TO  JOH-F18.
     MOVE    IN-F15            TO  JOH-F19.
     READ    YODJOHL2
      INVALID
             DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃"
                                      UPON CONS
             DISPLAY NC"＃　基本情報Ｆなし　＃"
                                      UPON CONS
             DISPLAY NC"＃　　受信日＝　" IN-F11
                                      UPON CONS
             DISPLAY NC"＃　　時刻　＝　" IN-F12
                                      UPON CONS
             DISPLAY NC"＃　　取引先＝　" IN-F13
                                      UPON CONS
             DISPLAY NC"＃　　注文日＝　" IN-F06
                                      UPON CONS
             DISPLAY NC"＃　　伝票　＝　" IN-F14
                                      UPON CONS
             DISPLAY NC"＃　　行　　＝　" IN-F15
                                      UPON CONS
             DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃"
                                      UPON CONS
             MOVE    4010    TO     PROGRAM-STATUS
             STOP    RUN
      NOT INVALID
             MOVE    SET-NO-R       TO  JOH-F29
             REWRITE    JOH-REC
     END-READ.
*  出力件数カウント
     ADD      1         TO    OUT-CNT.
*
*  出荷通知抽出データ読込み
     PERFORM YODSNDL4-READ-SEC.
*
 MAIN1-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE     CNT-TEN-F18   TO   TEN-F18.
     REWRITE  TEN-REC.
*
     MOVE     RD-CNT     TO      MSG-IN01.
     MOVE     OUT-CNT    TO      MSG-OUT01.
     DISPLAY  MSG-IN     UPON    CONS.
     DISPLAY  MSG-OUT    UPON    CONS.
*
     CLOSE    YODSNDL4   TENMS1  YODJOHL2.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
