# SSY9531B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY9531B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩ　出荷　　　　　　　　　　　*
*    サブシステム　　　　：　ヨドバシ　　　ＥＤＩ　　　　　　　*
*    モジュール名　　　　：　ヨドバシ出荷通知確定処理（倉庫）　*
*    作成日／作成者　　　：　2023/03/37 NAV INOUE              *
*    処理概要　　　　　　：　出荷通知番号を　　　　　　　　　　*
*                            倉庫確定状態にする。　　　　　　　*
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY9531B.
*                  流用:SSY9522B.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2023/03/27.
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
     SELECT   YODSNDL3  ASSIGN    TO        DA-01-VI-YODSNDL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       IS   IN-F11
                                                 IN-F12
                                                 IN-F13
                                                 IN-F16
                                                 IN-F02
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
 FD  YODSNDL3            LABEL RECORD   IS   STANDARD.
     COPY     YODSNDL3   OF        XFDLIB
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
     03  TEN-STATUS          PIC  X(02).
     03  JOH-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  TENMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
*カウンター領域
 01  WK-CNT.
     03  RD1-CNT              PIC  9(08)     VALUE  ZERO.
     03  RD2-CNT              PIC  9(08)     VALUE  ZERO.
     03  SEL-CNT             PIC  9(08)     VALUE  ZERO.
     03  RW1-CNT             PIC  9(08)     VALUE  ZERO.
     03  RW2-CNT             PIC  9(08)     VALUE  ZERO.
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
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY9531B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY9531B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY9531B".
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
                             " 抽出ＤＴ読込件数  = ".
         05  MSG-IN101       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT1.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 抽出ＤＴ更新件数  = ".
         05  MSG-OUT101      PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-IN2.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 基本情報読込件数  = ".
         05  MSG-IN201       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT2.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 基本情報更新件数  = ".
         05  MSG-OUT201      PIC  ZZZ,ZZ9.
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
                        PROCEDURE   YODSNDL3.
     MOVE      "YODSNDL3"   TO   AB-FILE.
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
*更新
     PERFORM  MAIN1-SEC
              UNTIL     END-FLG   =  "END".
     MOVE     SPACE               TO  END-FLG.
     PERFORM  MAIN2-SEC
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
     OPEN     I-O       YODSNDL3.
     OPEN     I-O       YODJOHL4.
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
     PERFORM YODSNDL3-START-SEC.
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
     PERFORM YODSNDL3-READ-SEC.
*
*終了判定
     IF   END-FLG  =  "END"
          DISPLAY "＃＃出荷通知データ無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
          GO                TO    INIT-EXIT
     END-IF.
*
*基本情報データＳＴＡＲＴ
     PERFORM YODJOHL4-START-SEC.
*
*判定
     IF   END-FLG  =  "END"
          DISPLAY "＃＃基本情報データ無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
          GO                TO    INIT-EXIT
     END-IF.
*
*基本情報ファイル読込み
     PERFORM YODJOHL4-READ-SEC.
*
*終了判定
     IF   END-FLG  =  "END"
          DISPLAY "＃＃基本情報データ無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
          GO                TO    INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　出荷抽出データＳＴＡＲＴ　　　　　　　　　　　　　　　　　*
****************************************************************
 YODSNDL3-START-SEC    SECTION.
*
     MOVE    "YODSNDL3-START-SEC"   TO   S-NAME.
*
     MOVE     SPACE                 TO    IN-REC.
     INITIALIZE                           IN-REC.
     MOVE     PARA-IN-BDATE         TO    IN-F11.
     MOVE     PARA-IN-BTIME         TO    IN-F12.
     MOVE     PARA-IN-BTORI         TO    IN-F13.
     MOVE     PARA-IN-SOKO          TO    IN-F16.
     MOVE     PARA-IN-NDATES        TO    IN-F02.
*
 YODSNDL3-START-01.
     START    YODSNDL3 KEY IS >= IN-F11 IN-F12 IN-F13 IN-F16
                                 IN-F02
              INVALID
                  MOVE     "END"    TO  END-FLG
                  GO                TO  YODSNDL3-START-EXIT
     END-START.
*
 YODSNDL3-START-EXIT.
     EXIT.
****************************************************************
*　　基本情報データＳＴＡＲＴ　　　　　　　　　　　　　　　　　*
****************************************************************
 YODJOHL4-START-SEC    SECTION.
*
     MOVE    "YODJOHL4-START-SEC"   TO   S-NAME.
*
     MOVE     SPACE                 TO    JOH-REC.
     INITIALIZE                           JOH-REC.
     MOVE     PARA-IN-BDATE         TO    JOH-F15.
     MOVE     PARA-IN-BTIME         TO    JOH-F16.
     MOVE     PARA-IN-BTORI         TO    JOH-F17.
     MOVE     PARA-IN-SOKO          TO    JOH-F28.
     MOVE     PARA-IN-NDATES        TO    JOH-F14.
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
*　　　　　　　メイン処理１　出荷通知抽出データ更新　　　　　　*
****************************************************************
 MAIN1-SEC     SECTION.
*
     MOVE    "MAIN1-SEC"     TO      S-NAME.
*
*出荷通知抽出ファイル更新
     MOVE    SYS-DATEW         TO  IN-F18.
     MOVE    SYS-TIMEW         TO  IN-F19.
     REWRITE IN-REC.
*
*  出力件数カウント
     ADD      1         TO    RW1-CNT.
*
 MAIN1-99.
*  出荷通知抽出データ読込み
     PERFORM YODSNDL3-READ-SEC.
*
 MAIN1-EXIT.
     EXIT.
****************************************************************
*　　出荷抽出データ読込　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 YODSNDL3-READ-SEC    SECTION.
*
     MOVE    "YODSNDL3-READ-SEC"    TO   S-NAME.
*
 YODSNDL3-READ-01.
     READ     YODSNDL3
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  YODSNDL3-READ-EXIT
     END-READ.
*
     ADD      1     TO     RD1-CNT.
*
*条件判定
 YODSNDL3-READ-02.
     IF  ( IN-F11    =      PARA-IN-BDATE ) AND
         ( IN-F12    =      PARA-IN-BTIME ) AND
         ( IN-F13    =      PARA-IN-BTORI )
           CONTINUE
     ELSE
           MOVE    "END"    TO  END-FLG
           GO               TO  YODSNDL3-READ-EXIT
     END-IF.
*
     IF    PARA-IN-SOKO  =  SPACE
           CONTINUE
     ELSE
           IF   IN-F16   =  PARA-IN-SOKO
                CONTINUE
           ELSE
                MOVE    "END"    TO  END-FLG
                GO               TO  YODSNDL3-READ-EXIT
           END-IF
     END-IF.
*
     IF  ( IN-F02       >=  PARA-IN-NDATES ) AND
         ( IN-F02       <=  PARA-IN-NDATEE )
           CONTINUE
     ELSE
           GO               TO  YODSNDL3-READ-01
     END-IF.
*
*確定済み判定
     IF  ( IN-F18   NOT =  ZERO ) OR
         ( IN-F20   NOT =  ZERO )
           GO       TO     YODSNDL3-READ-01
     END-IF.
*
 YODSNDL3-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理２　基本情報データ更新　　　　　　　　*
****************************************************************
 MAIN2-SEC     SECTION.
*
     MOVE    "MAIN2-SEC"     TO      S-NAME.
*
*基本情報ファイル更新
     MOVE    SYS-DATEW         TO  JOH-F34.
     MOVE    SYS-TIMEW         TO  JOH-F35.
     REWRITE JOH-REC.
*
*  出力件数カウント
     ADD      1         TO    RW2-CNT.
*
 MAIN2-99.
*  基本情報ファイル読込み
     PERFORM YODJOHL4-READ-SEC.
*
 MAIN2-EXIT.
     EXIT.
****************************************************************
*　　基本情報ファイル読込　　　　　　　　　　　　　　　　　　　*
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
     ADD      1     TO     RD2-CNT.
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
     IF  ( JOH-F14       >=  PARA-IN-NDATES ) AND
         ( JOH-F14       <=  PARA-IN-NDATEE )
           CONTINUE
     ELSE
           GO               TO  YODJOHL4-READ-01
     END-IF.
*
*確定済み判定
     IF  ( JOH-F34   NOT =  ZERO ) OR
         ( JOH-F26   NOT =  ZERO )
           GO       TO     YODJOHL4-READ-01
     END-IF.
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
     DISPLAY  MSG-IN1    UPON    CONS.
     DISPLAY  MSG-OUT1   UPON    CONS.
     DISPLAY  MSG-IN2    UPON    CONS.
     DISPLAY  MSG-OUT2   UPON    CONS.
*
     CLOSE    YODSNDL3   YODJOHL4.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
