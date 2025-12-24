****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩ　出荷　　　　　　　　　　　*
*    サブシステム　　　　：　ヨドバシ　　　ＥＤＩ　　　　　　　*
*    モジュール名　　　　：　ヨドバシ出荷通知データ作成　　　　*
*    　　　　　　　　　　　　（本社確定）　　　　　　　　　　　*
*    作成日／作成者　　　：　2023/05/01 NAV TAKAHASHI          *
*    処理概要　　　　　　：　ヨドバシへ返送する　　　　　　　　*
*                            出荷通知データを作成する。　　　　*
*                            売上伝票Ｆの付番ＦＬＧを更新する。*
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY9543B.
*                  流用:SSY9403B.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2023/03/28.
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
*----<< 売上伝票ファイル >>----*
     SELECT   SHTDENLJ  ASSIGN              DA-01-VI-SHTDENLJ
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F46
                                            DEN-F47
                                            DEN-F01
                                            DEN-F112
                                            DEN-F48
                                            DEN-F02
                                            DEN-F04
                                            DEN-F051
                                            DEN-F07
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*店舗マスタ
*    SELECT   YODJOHL2    ASSIGN    TO        DA-01-VI-YODJOHL2
*                       ORGANIZATION        INDEXED
*                       ACCESS    MODE      RANDOM
*                       RECORD    KEY       JOH-F52
*                                           JOH-F011
*                       FILE      STATUS    JOH-STATUS.
*出荷通知ＣＳＶ
     SELECT   YODSNDSF  ASSIGN    TO        DA-01-S-YODSNDSF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CSV-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*出荷通知抽出データ
******************************************************************
 FD  YODSNDL4            LABEL RECORD   IS   STANDARD.
     COPY     YODSNDL4   OF        XFDLIB
              JOINING    IN   AS   PREFIX.
******************************************************************
*基本情報ファイル
******************************************************************
 FD  YODJOHL2            LABEL RECORD   IS   STANDARD.
     COPY     YODJOHL2   OF        XFDLIB
              JOINING    JOH       PREFIX.
******************************************************************
*    売上伝票ファイル
******************************************************************
 FD  SHTDENLJ            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENLJ   OF        XFDLIB
              JOINING    DEN       PREFIX.
*****************************************************************
*店舗マスタ
*****************************************************************
*FD  YODJOHL2
*                      LABEL RECORD   IS   STANDARD.
*    COPY              YODJOHL2         OF   XFDLIB
*    JOINING           TEN            AS   PREFIX.
*
******************************************************************
*出荷通知ＣＳＶ
******************************************************************
 FD  YODSNDSF            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS   29        RECORDS.
     COPY     YODSNDSF   OF        XFDLIB
              JOINING    CSV       PREFIX.

*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  IN-STATUS           PIC  X(02).
     03  JOH-STATUS          PIC  X(02).
     03  CSV-STATUS          PIC  X(02).
     03  DEN-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  YODJOHL2-INV-FLG          PIC  X(03)     VALUE  SPACE.
*カウンター領域
 01  WK-CNT.
     03  RD-CNT              PIC  9(08)     VALUE  ZERO.
     03  SEL-CNT             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT             PIC  9(08)     VALUE  ZERO.
     03  CNT-JOH-F18         PIC  9(07)     VALUE  ZERO.
     03  SET-CSV-F01.
         05  SET-CSV-F01-1   PIC  9(04)     VALUE  ZERO.
         05  SET-CSV-F01-2   PIC  9(06)     VALUE  ZERO.
     03  SET-CSV-F01-R
         REDEFINES SET-CSV-F01    PIC  9(10).
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
 01  SYS-TIME                PIC  9(08).
 01  FILLER                  REDEFINES      SYS-TIME.
     03  SYS-TIMEW           PIC  9(06).
     03  FILLER              PIC  9(02).
*店舗ＣＤ変換
 01  WK-DEN-F07              PIC  X(04).
 01  FILLER                  REDEFINES      WK-DEN-F07.
     03  WK-DEN-F07-R        PIC  9(04).
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY9543B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY9543B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY9543B".
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
         05  FILLER          PIC  X(24)  VALUE
                             " CSVデータ作成件数  = ".
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
 01  PARA-OUT-CNT            PIC   9(07).
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
                                             PARA-IN-NDATEE
                                             PARA-OUT-CNT.
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
                        PROCEDURE   YODJOHL2.
     MOVE      "YODJOHL2"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODSNDSF.
     MOVE      "YODSNDSF"   TO   AB-FILE.
     MOVE      CSV-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLJ.
     MOVE      "SHTDENLJ"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
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
*ＣＳＶ出力
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
     OPEN     I-O       YODJOHL2.
     OPEN     I-O       SHTDENLJ.
     OPEN     OUTPUT    YODSNDSF.
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
         ACCEPT  SYS-TIME  FROM  TIME.
*
*出荷通知抽出データＳＴＡＲＴ
     PERFORM YODSNDL4-START-SEC.
*
*判定
     IF   END-FLG  =  "END"
          DISPLAY "＃＃出荷通知データ無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
          GO                TO    INIT-EXIT
     END-IF.
*
*出荷通知抽出データ読込み
     PERFORM YODSNDL4-READ-SEC.
*
*終了判定
     IF   END-FLG  =  "END"
          DISPLAY "＃＃出力対象データ無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
          GO                TO    INIT-EXIT
     ELSE
*       出荷先コード保管
          MOVE     IN-F10   TO    BRK-IN-F10
          MOVE     IN-F02   TO    BRK-IN-F02
          MOVE     IN-F09(5:4)   TO    BRK-IN-F09
     END-IF.
*
*基本情報ファイル検索
*    MOVE     IN-F13        TO    JOH-F52.
*    MOVE     IN-F10(1:4)   TO    JOH-F011.
*    READ     YODJOHL2
*         INVALID
*             DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
*             DISPLAY NC"＃　店舗マスタなし　＃" UPON CONS
*             DISPLAY NC"＃　　コード＝　" IN-F10(1:5) UPON CONS
*             DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
*             MOVE    4010    TO     PROGRAM-STATUS
*             STOP    RUN
*         NOT INVALID
*             MOVE    JOH-F18 TO     CNT-JOH-F18
*             COMPUTE CNT-JOH-F18 =  CNT-JOH-F18 + 1
*             IF      CNT-JOH-F18 >  999999
*                     MOVE  1     TO CNT-JOH-F18
*             END-IF
*    END-READ.
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
 YODSNDL3-READ-02.
     IF  ( IN-F11    =      PARA-IN-BDATE ) AND
         ( IN-F12    =      PARA-IN-BTIME ) AND
         ( IN-F13    =      PARA-IN-BTORI )
           CONTINUE
     ELSE
           MOVE    "END"    TO  END-FLG
           GO               TO  YODSNDL4-READ-EXIT
     END-IF.
*
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
     IF  ( IN-F02       >=  PARA-IN-NDATES ) AND
         ( IN-F02       <=  PARA-IN-NDATEE )
           CONTINUE
     ELSE
           GO               TO  YODSNDL4-READ-01
     END-IF.
*
*確定済み判定
     IF  ( IN-F17   NOT =  ZERO ) AND
         ( IN-F18   NOT =  ZERO ) AND
         ( IN-F20       =  ZERO )
           CONTINUE
     ELSE
           GO       TO     YODSNDL4-READ-01
     END-IF.
*
*
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
*    IF       IN-F10   =  BRK-IN-F10
*    AND      IN-F02   =  BRK-IN-F02
*    AND      WRK-IN-F09  =  BRK-IN-F09
*             CONTINUE
*    ELSE
*             MOVE     CNT-JOH-F18   TO   JOH-F18
*             REWRITE  JOH-REC
*             MOVE     IN-F13        TO   JOH-F52
*             MOVE     IN-F10(1:4)   TO   JOH-F011
*             READ     YODJOHL2
*                 INVALID
*                      DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃"
*                                                UPON CONS
*                      DISPLAY NC"＃　店舗マスタなし　＃"
*                                                UPON CONS
*                      DISPLAY NC"＃　　コード＝　" IN-F10(1:5)
*                                                UPON CONS
*                      DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃"
*                                                UPON CONS
*                      MOVE    4010    TO     PROGRAM-STATUS
*                      STOP    RUN
*                 NOT INVALID
*                      MOVE    JOH-F18 TO      CNT-JOH-F18
*                      COMPUTE CNT-JOH-F18  =  CNT-JOH-F18 + 1
*                      IF      CNT-JOH-F18  >  999999
*                              MOVE  1      TO CNT-JOH-F18
*                      END-IF
*                      出荷先コード保管
*                      MOVE     IN-F10   TO    BRK-IN-F10
*                      MOVE     IN-F02   TO    BRK-IN-F02
*                      IF   IN-F09(5:4)  = "0010" OR "0050"
*                           MOVE   IN-F09(5:4)  TO    BRK-IN-F09
*                      ELSE
*                           MOVE   "9999"       TO    BRK-IN-F09
*                      END-IF
*             END-READ
*    END-IF.
*
*  出荷通知ＣＳＶ初期値設定
     PERFORM  YODSNDSF-INIT-SEC.
*  出荷通知ＣＳＶ　　編集
     PERFORM  YODSNDSF-SET-SEC.
*  レコード出力
     WRITE    CSV-REC.
*  出力件数カウント
     ADD      1         TO    OUT-CNT.
*
*  出荷通知抽出データ更新
     MOVE    SYS-DATEW     TO     IN-F20.
     MOVE    SYS-TIMEW     TO     IN-F21.
     REWRITE IN-REC.
*
*  基本情報ファイル更新
     MOVE    SPACE         TO     JOH-REC.
     INITIALIZE                   JOH-REC.
     MOVE    IN-F11        TO     JOH-F15.
     MOVE    IN-F12        TO     JOH-F16.
     MOVE    IN-F13        TO     JOH-F17.
     MOVE    IN-F06        TO     JOH-F03.
     MOVE    IN-F14        TO     JOH-F18.
     MOVE    IN-F15        TO     JOH-F19.
     READ    YODJOHL2
             INVALID
                  DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃"  UPON CONS
                  DISPLAY NC"＃　基本情報　なし　＃"  UPON CONS
                  DISPLAY NC"＃　　受信日＝　" IN-F11 UPON CONS
                  DISPLAY NC"＃　　時刻　＝　" IN-F12 UPON CONS
                  DISPLAY NC"＃　　取引先＝　" IN-F13 UPON CONS
                  DISPLAY NC"＃　　注文日＝　" IN-F06 UPON CONS
                  DISPLAY NC"＃　　伝票　＝　" IN-F14 UPON CONS
                  DISPLAY NC"＃　　行　　＝　" IN-F15 UPON CONS
                  DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃"  UPON CONS
                  MOVE    4010    TO     PROGRAM-STATUS
                  STOP    RUN
             NOT INVALID
                  MOVE    SYS-DATEW     TO     JOH-F26
                  MOVE    SYS-TIMEW     TO     JOH-F27
*-                MOVE    IN-F17        TO     JOH-F29
                  REWRITE JOH-REC
     END-READ.
*  売上伝票ファイル更新
     MOVE    SPACE         TO     DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE    JOH-F15       TO     DEN-F46.
     MOVE    JOH-F16       TO     DEN-F47.
     MOVE    JOH-F17       TO     DEN-F01.
     MOVE    JOH-F14       TO     DEN-F112.
     MOVE    JOH-F28       TO     DEN-F48.
     MOVE    JOH-F18       TO     DEN-F02.
     MOVE    ZERO          TO     DEN-F04.
     MOVE    40            TO     DEN-F051.
     MOVE    JOH-F06(1:4)  TO     WK-DEN-F07.
     MOVE    WK-DEN-F07-R  TO     DEN-F07.
     MOVE    JOH-F19       TO     DEN-F03.
     READ    SHTDENLJ
             INVALID
             DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃"    UPON CONS
             DISPLAY NC"＃　売上伝票　なし　＃"    UPON CONS
             DISPLAY NC"＃　　受信日＝　" DEN-F46  UPON CONS
             DISPLAY NC"＃　　時刻　＝　" DEN-F47  UPON CONS
             DISPLAY NC"＃　　取引先＝　" DEN-F01  UPON CONS
             DISPLAY NC"＃　　納品日＝　" DEN-F112 UPON CONS
             DISPLAY NC"＃　　伝票　＝　" DEN-F02  UPON CONS
             DISPLAY NC"＃　　行　　＝　" DEN-F03  UPON CONS
             DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃"    UPON CONS
             MOVE    4010    TO     PROGRAM-STATUS
             STOP    RUN
             NOT INVALID
                  MOVE    9             TO     DEN-F276
                  REWRITE DEN-REC
     END-READ.
*
*  出荷通知抽出データ読込み
     PERFORM YODSNDL4-READ-SEC.
*
 MAIN1-EXIT.
     EXIT.
****************************************************************
*出荷通知ＣＳＶ   初期値設定
****************************************************************
 YODSNDSF-INIT-SEC    SECTION.
*
     MOVE    "YODSNDSF-INIT-SEC" TO  S-NAME.
*
     MOVE     SPACE          TO   CSV-REC.
*カンマ
     MOVE    ","             TO   CSV-K01
                                  CSV-K02
                                  CSV-K03
                                  CSV-K04
                                  CSV-K05
                                  CSV-K06
                                  CSV-K07
                                  CSV-K08.
*
 YODSNDSF-INIT-EXIT.
     EXIT.
****************************************************************
*出荷通知ＣＳＶ   SET
****************************************************************
 YODSNDSF-SET-SEC     SECTION.
*
     MOVE   "AMZCHKC4-SET-SEC" TO  S-NAME.
*
*出荷通知番号
*    MOVE    IN-F10(1:4)       TO  SET-CSV-F01-1.
*    MOVE    CNT-JOH-F18       TO  SET-CSV-F01-2.
*    MOVE    SET-CSV-F01-R     TO  CSV-F01.
     MOVE    IN-F17            TO  CSV-F01.
*納入予定日付
     MOVE    IN-F02            TO  CSV-F02.
*明細行番号
     MOVE    IN-F03            TO  CSV-F03.
*品目番号
     MOVE    IN-F04            TO  CSV-F04.
*数量
     MOVE    IN-F05            TO  CSV-F05.
*発注伝票日付
     MOVE    IN-F06            TO  CSV-F06.
*参照伝票番号
     MOVE    IN-F07            TO  CSV-F07.
*参照行番号
     MOVE    IN-F08            TO  CSV-F08.
*配送場所
     MOVE    IN-F09            TO  CSV-F09.
*
 YODSNDSF-SET-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
*    MOVE     CNT-JOH-F18   TO   JOH-F18.
*    REWRITE  JOH-REC.
*
     MOVE     RD-CNT     TO      MSG-IN01.
     MOVE     OUT-CNT    TO      MSG-OUT01 PARA-OUT-CNT.
     DISPLAY  MSG-IN     UPON    CONS.
     DISPLAY  MSG-OUT    UPON    CONS.
*
     CLOSE    YODSNDL4   YODJOHL2  YODSNDSF  SHTDENLJ.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
