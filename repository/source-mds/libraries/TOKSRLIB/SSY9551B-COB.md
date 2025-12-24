# SSY9551B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY9551B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩ　出荷　　　　　　　　　　　*
*    サブシステム　　　　：　ヨドバシ　　　ＥＤＩ　　　　　　　*
*    モジュール名　　　　：　ヨドバシ出荷確定取消処理　　　　　*
*    作成日／作成者　　　：　2023/04/04 NAV INOUE              *
*    処理概要　　　　　　：　基本情報ファイル・出荷通知抽出　　*
*                            データの確定状態を取り消す　　　　*
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY9551B.
*                  流用:SSY9542B.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2023/04/04.
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
     SELECT   YODSNDL5  ASSIGN    TO        DA-01-VI-YODSNDL5
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       IS   IN-F11
                                                 IN-F12
                                                 IN-F13
                                                 IN-F16
                                                 IN-F02
                                                 IN-F14
                                                 IN-F15
                        FILE      STATUS    IN-STATUS.
*基本情報ファイル
     SELECT   YODJOHL4  ASSIGN    TO        DA-01-VI-YODJOHL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JOH-F15
                                            JOH-F16
                                            JOH-F17
                                            JOH-F28
                                            JOH-F14
                                            JOH-F18
                                            JOH-F19
                        FILE      STATUS    JOH-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*出荷通知抽出データ
******************************************************************
 FD  YODSNDL5            LABEL RECORD   IS   STANDARD.
     COPY     YODSNDL5   OF        XFDLIB
              JOINING    IN   AS   PREFIX.
******************************************************************
*基本情報ファイル
******************************************************************
 FD  YODJOHL4            LABEL RECORD   IS   STANDARD.
     COPY     YODJOHL4   OF        XFDLIB
              JOINING    JOH       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  IN-STATUS           PIC  X(02).
     03  JOH-STATUS          PIC  X(02).
*    03  CSV-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  YODJOHL4-INV-FLG          PIC  X(03)     VALUE  SPACE.
*カウンター領域
 01  WK-CNT.
     03  RD1-CNT             PIC  9(08)     VALUE  ZERO.
     03  RD2-CNT             PIC  9(08)     VALUE  ZERO.
     03  SEL-CNT             PIC  9(08)     VALUE  ZERO.
     03  RW1-CNT             PIC  9(08)     VALUE  ZERO.
     03  RW2-CNT             PIC  9(08)     VALUE  ZERO.
     03  CNT-JOH-F18         PIC  9(07)     VALUE  ZERO.
     03  SET-CSV-F01.
         05  SET-CSV-F01-1   PIC  9(04)     VALUE  ZERO.
         05  SET-CSV-F01-2   PIC  9(06)     VALUE  ZERO.
     03  SET-CSV-F01-R
         REDEFINES SET-CSV-F01    PIC  9(10).
 01  DEN-CNT                 PIC  9(02)     VALUE  ZERO.
*
*ＷＯＲＫ
 01  CHK-DENNO               PIC  9(09)     VALUE  ZERO.
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
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY9551B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY9551B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY9551B".
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
     03  MSG-IN1.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 出荷通知読込件数  = ".
         05  MSG-IN101       PIC  Z,ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT1.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 出荷確定取消件数  = ".
         05  MSG-OUT101      PIC  Z,ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-IN2.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 基本情報読込件数  = ".
         05  MSG-IN201       PIC  Z,ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT2.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 出荷確定取消件数  = ".
         05  MSG-OUT201      PIC  Z,ZZZ,ZZ9.
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
 01  PARA-IN-NDATE           PIC   9(08).
 01  PARA-IN-DENNO.
     03  PARA-DENNO-01       PIC   9(09).
     03  PARA-DENNO-02       PIC   9(09).
     03  PARA-DENNO-03       PIC   9(09).
     03  PARA-DENNO-04       PIC   9(09).
     03  PARA-DENNO-05       PIC   9(09).
     03  PARA-DENNO-06       PIC   9(09).
     03  PARA-DENNO-07       PIC   9(09).
     03  PARA-DENNO-08       PIC   9(09).
     03  PARA-DENNO-09       PIC   9(09).
     03  PARA-DENNO-10       PIC   9(09).
 01  PARA-IN-TAISYO          PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING
                                             PARA-IN-BDATE
                                             PARA-IN-BTIME
                                             PARA-IN-BTORI
                                             PARA-IN-SOKO
                                             PARA-IN-NDATE
                                             PARA-IN-DENNO
                                             PARA-IN-TAISYO.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODSNDL5.
     MOVE      "YODSNDL5"   TO   AB-FILE.
     MOVE      IN-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODJOHL4.
     MOVE      "YODJOHL4"   TO   AB-FILE.
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
*出荷確定取消　出荷通知抽出データ
     MOVE     1                   TO   DEN-CNT.
     PERFORM  MAIN1-SEC
*             UNTIL     END-FLG   =  "END"
*                   OR  DEN-CNT   >   10.
              UNTIL     DEN-CNT   >   10.
*出荷確定取消　基本情報ファイル
     MOVE     1                   TO   DEN-CNT.
     MOVE     SPACE               TO   END-FLG.
     PERFORM  MAIN2-SEC
*             UNTIL     END-FLG   =  "END"
*                   OR  DEN-CNT   >   10.
              UNTIL     DEN-CNT   >   10.
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
     OPEN     I-O       YODSNDL5.
     OPEN     I-O       YODJOHL4.
*    OPEN     OUTPUT    YODSNDSF.
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
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理１　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN1-SEC     SECTION.
*
     MOVE    "MAIN1-SEC"     TO      S-NAME.
*
 MAIN1-01.
     MOVE     SPACE          TO      IN-REC.
     INITIALIZE                      IN-REC.
     MOVE     PARA-IN-BDATE  TO      IN-F11.
     MOVE     PARA-IN-BTIME  TO      IN-F12.
     MOVE     PARA-IN-BTORI  TO      IN-F13.
     MOVE     PARA-IN-SOKO   TO      IN-F16.
     MOVE     PARA-IN-NDATE  TO      IN-F02.
     EVALUATE DEN-CNT
       WHEN   1
              MOVE   PARA-DENNO-01   TO  IN-F14 CHK-DENNO
       WHEN   2
              MOVE   PARA-DENNO-02   TO  IN-F14 CHK-DENNO
       WHEN   3
              MOVE   PARA-DENNO-03   TO  IN-F14 CHK-DENNO
       WHEN   4
              MOVE   PARA-DENNO-04   TO  IN-F14 CHK-DENNO
       WHEN   5
              MOVE   PARA-DENNO-05   TO  IN-F14 CHK-DENNO
       WHEN   6
              MOVE   PARA-DENNO-06   TO  IN-F14 CHK-DENNO
       WHEN   7
              MOVE   PARA-DENNO-07   TO  IN-F14 CHK-DENNO
       WHEN   8
              MOVE   PARA-DENNO-08   TO  IN-F14 CHK-DENNO
       WHEN   9
              MOVE   PARA-DENNO-09   TO  IN-F14 CHK-DENNO
       WHEN   10
              MOVE   PARA-DENNO-10   TO  IN-F14 CHK-DENNO
     END-EVALUATE.
*
*出荷通知抽出データＳＴＡＲＴ
     MOVE    SPACE          TO    END-FLG.
     PERFORM YODSNDL5-START-SEC.
*
*判定
     IF   END-FLG  =  "END"
          MOVE    SPACE     TO    END-FLG
          GO                TO    MAIN1-99
     END-IF.
*
 MAIN1-02.
*出荷通知抽出データ読込み
     PERFORM YODSNDL5-READ-SEC.
*
*終了判定
     IF   END-FLG  =  "END"
          MOVE    SPACE     TO    END-FLG
          GO                TO    MAIN1-99
     END-IF.
*
 MAIN1-03.
*レコード更新
     IF      PARA-IN-TAISYO  =  1
             MOVE    ZERO          TO     IN-F18 IN-F19
     ELSE
             MOVE    ZERO          TO     IN-F20 IN-F21
     END-IF.
*
     REWRITE    IN-REC.
*
*更新件数カウント
     ADD          1                TO     RW1-CNT.
     GO                            TO     MAIN1-02.
*
 MAIN1-99.
     ADD          1                TO     DEN-CNT.
*
 MAIN1-EXIT.
     EXIT.
****************************************************************
*　　出荷抽出データＳＴＡＲＴ　　　　　　　　　　　　　　　　　*
****************************************************************
 YODSNDL5-START-SEC    SECTION.
*
     MOVE    "YODSNDL5-START-SEC"   TO   S-NAME.
*
 YODSNDL5-START-01.
     START    YODSNDL5 KEY IS >= IN-F11 IN-F12 IN-F13 IN-F16
                                 IN-F02 IN-F14 IN-F15
              INVALID
                  MOVE     "END"    TO  END-FLG
                  GO                TO  YODSNDL5-START-EXIT
     END-START.
*
 YODSNDL5-START-EXIT.
     EXIT.
****************************************************************
*　　出荷抽出データ読込　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 YODSNDL5-READ-SEC    SECTION.
*
     MOVE    "YODSNDL5-READ-SEC"    TO   S-NAME.
*
 YODSNDL5-READ-01.
     READ     YODSNDL5
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  YODSNDL5-READ-EXIT
     END-READ.
*
*条件判定
 YODSNDL3-READ-02.
     IF  ( IN-F11    =      PARA-IN-BDATE ) AND
         ( IN-F12    =      PARA-IN-BTIME ) AND
         ( IN-F13    =      PARA-IN-BTORI )
           CONTINUE
     ELSE
           MOVE    "END"    TO  END-FLG
           GO               TO  YODSNDL5-READ-EXIT
     END-IF.
*
*倉庫
     IF    PARA-IN-SOKO  =  SPACE
           CONTINUE
     ELSE
           IF   IN-F16   =  PARA-IN-SOKO
                CONTINUE
           ELSE
                MOVE    "END"    TO  END-FLG
                GO               TO  YODSNDL5-READ-EXIT
           END-IF
     END-IF.
*
*納品日
     IF    IN-F02        =  PARA-IN-NDATE
           CONTINUE
     ELSE
           MOVE    "END"    TO  END-FLG
           GO               TO  YODSNDL5-READ-EXIT
     END-IF.
*
*伝票番号
     IF    IN-F14       =   CHK-DENNO
           CONTINUE
     ELSE
           MOVE    "END"    TO  END-FLG
           GO               TO  YODSNDL5-READ-EXIT
     END-IF.
*
     ADD      1     TO     RD1-CNT.
*
 YODSNDL5-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理２　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN2-SEC     SECTION.
*
     MOVE    "MAIN2-SEC"     TO      S-NAME.
*
 MAIN2-01.
     MOVE     SPACE          TO      JOH-REC.
     INITIALIZE                      JOH-REC.
     MOVE     PARA-IN-BDATE  TO      JOH-F15.
     MOVE     PARA-IN-BTIME  TO      JOH-F16.
     MOVE     PARA-IN-BTORI  TO      JOH-F17.
     MOVE     PARA-IN-SOKO   TO      JOH-F28.
     MOVE     PARA-IN-NDATE  TO      JOH-F14.
     EVALUATE DEN-CNT
       WHEN   1
              MOVE   PARA-DENNO-01   TO  JOH-F18 CHK-DENNO
       WHEN   2
              MOVE   PARA-DENNO-02   TO  JOH-F18 CHK-DENNO
       WHEN   3
              MOVE   PARA-DENNO-03   TO  JOH-F18 CHK-DENNO
       WHEN   4
              MOVE   PARA-DENNO-04   TO  JOH-F18 CHK-DENNO
       WHEN   5
              MOVE   PARA-DENNO-05   TO  JOH-F18 CHK-DENNO
       WHEN   6
              MOVE   PARA-DENNO-06   TO  JOH-F18 CHK-DENNO
       WHEN   7
              MOVE   PARA-DENNO-07   TO  JOH-F18 CHK-DENNO
       WHEN   8
              MOVE   PARA-DENNO-08   TO  JOH-F18 CHK-DENNO
       WHEN   9
              MOVE   PARA-DENNO-09   TO  JOH-F18 CHK-DENNO
       WHEN   10
              MOVE   PARA-DENNO-10   TO  JOH-F18 CHK-DENNO
     END-EVALUATE.
*
*基本情報ＳＴＡＲＴ
     MOVE    SPACE          TO    END-FLG.
     PERFORM YODJOHL4-START-SEC.
*
*判定
     IF   END-FLG  =  "END"
          MOVE    SPACE     TO    END-FLG
          GO                TO    MAIN2-99
     END-IF.
*
 MAIN2-02.
*基本情報読込み
     PERFORM YODJOHL4-READ-SEC.
*
*終了判定
     IF   END-FLG  =  "END"
          MOVE    SPACE     TO    END-FLG
          GO                TO    MAIN2-99
     END-IF.
*
 MAIN2-03.
*レコード更新
     IF      PARA-IN-TAISYO  =  1
             MOVE    ZERO          TO     JOH-F34 JOH-F35
     ELSE
             MOVE    ZERO          TO     JOH-F26 JOH-F27
     END-IF.
*
     REWRITE    JOH-REC.
*
*更新件数カウント
     ADD          1                TO     RW2-CNT.
     GO                            TO     MAIN2-02.
*
 MAIN2-99.
     ADD          1                TO     DEN-CNT.
*
 MAIN2-EXIT.
     EXIT.
****************************************************************
*　　基本情報Ｆ　　ＳＴＡＲＴ　　　　　　　　　　　　　　　　　*
****************************************************************
 YODJOHL4-START-SEC    SECTION.
*
     MOVE    "YODJOHL4-START-SEC"   TO   S-NAME.
*
 YODJOHL4-START-01.
     START    YODJOHL4 KEY IS >= JOH-F15 JOH-F16 JOH-F17 JOH-F28
                                 JOH-F14 JOH-F18 JOH-F19
              INVALID
                  MOVE     "END"    TO  END-FLG
                  GO                TO  YODJOHL4-START-EXIT
     END-START.
*
 YODJOHL4-START-EXIT.
     EXIT.
****************************************************************
*　　基本情報Ｆ　　読込　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 YODJOHL4-READ-SEC    SECTION.
*
     MOVE    "YODJOHL4-READ-SEC"    TO   S-NAME.
*
 YODJOHL4-READ-01.
     READ     YODJOHL4
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  YODJOHL4-READ-EXIT
     END-READ.
*
*条件判定
 YODJOHL4-READ-02.
     IF  ( JOH-F15    =      PARA-IN-BDATE ) AND
         ( JOH-F16    =      PARA-IN-BTIME ) AND
         ( JOH-F17    =      PARA-IN-BTORI )
           CONTINUE
     ELSE
           MOVE    "END"    TO  END-FLG
           GO               TO  YODJOHL4-READ-EXIT
     END-IF.
*
*倉庫
 YODJOHL4-READ-03.
     IF    PARA-IN-SOKO  =  SPACE
           CONTINUE
     ELSE
           IF   JOH-F28   =  PARA-IN-SOKO
                CONTINUE
           ELSE
                MOVE    "END"    TO  END-FLG
                GO               TO  YODJOHL4-READ-EXIT
           END-IF
     END-IF.
*
*納品日
 YODJOHL4-READ-04.
     IF    JOH-F14        =  PARA-IN-NDATE
           CONTINUE
     ELSE
           MOVE    "END"    TO  END-FLG
           GO               TO  YODJOHL4-READ-EXIT
     END-IF.
*
*伝票番号
 YODJOHL4-READ-05.
     IF    JOH-F18       =   CHK-DENNO
           CONTINUE
     ELSE
           MOVE    "END"    TO  END-FLG
           GO               TO  YODJOHL4-READ-EXIT
     END-IF.
*
     ADD      1     TO     RD2-CNT.
*
 YODJOHL4-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE     RD1-CNT    TO      MSG-IN101.
     MOVE     RW1-CNT    TO      MSG-OUT101.
     MOVE     RD2-CNT    TO      MSG-IN201.
     MOVE     RW2-CNT    TO      MSG-OUT201.
     DISPLAY  MSG-IN1     UPON    CONS.
     DISPLAY  MSG-OUT1    UPON    CONS.
     DISPLAY  MSG-IN2     UPON    CONS.
     DISPLAY  MSG-OUT2    UPON    CONS.
*
     CLOSE    YODSNDL5   YODJOHL4.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
