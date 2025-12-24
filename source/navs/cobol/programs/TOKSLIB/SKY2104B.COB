****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　オンライン管理　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　伝票データ並び替え出力処理        *
*    作成日／更新日　　　：　00/11/20                          *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取ったパラメタより、出力する  *
*                            並び順のファイルを読み、伝票発行  *
*                            用Ｆへ出力する。                  *
*                            （_番順追加）（センター区分順）  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKY2104B.
 AUTHOR.                NAV T.TAKAHSHI.
 DATE-WRITTEN.          00/11/20.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*伝票ファイル（伝票_順）
     SELECT   ONLDT1    ASSIGN    TO        DA-01-VI-ONLDT1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DT1-F01   DT1-F02
                                            DT1-F03
                        FILE  STATUS   IS   DT1-STATUS.
*伝票ファイル（店舗_順）
     SELECT   ONLDT2    ASSIGN    TO        DA-01-VI-ONLDT2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DT2-F01   DT2-F07
                                            DT2-F02   DT2-F03
                        FILE  STATUS   IS   DT2-STATUS.
*伝票ファイル（納品日順）
     SELECT   ONLDT3    ASSIGN    TO        DA-01-VI-ONLDT3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DT3-F01   DT3-F112
                                            DT3-F07   DT3-F02
                                            DT3-F03
                        FILE  STATUS   IS   DT3-STATUS.
*伝票ファイル（部門順）
     SELECT   ONLDT5    ASSIGN    TO        DA-01-VI-ONLDT5
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DT5-F12   DT5-F07
                                            DT5-F112  DT5-F02
                                            DT5-F03
                        FILE  STATUS   IS   DT5-STATUS.
*伝票ファイル（センター区分順）
     SELECT   ONLDT6    ASSIGN    TO        DA-01-VI-ONLDT6
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DT6-F278  DT6-F12
                                            DT6-F07   DT6-F112
                                            DT6-F02   DT6-F03
                        FILE  STATUS   IS   DT6-STATUS.
*オンライン伝票データファイル
     SELECT   JHSDENF   ASSIGN    TO        DA-01-S-DENPYOSF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   DEN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    伝票番号順
******************************************************************
 FD  ONLDT1
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DT1  AS   PREFIX.
*
******************************************************************
*    店舗ＣＤ順
******************************************************************
 FD  ONLDT2
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DT2  AS   PREFIX.
*
******************************************************************
*    納品日順
******************************************************************
 FD  ONLDT3
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DT3  AS   PREFIX.
*
******************************************************************
*    部門順
******************************************************************
 FD  ONLDT5
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DT5  AS   PREFIX.
*
******************************************************************
*    センター区分順
******************************************************************
 FD  ONLDT6
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DT6  AS   PREFIX.
*
******************************************************************
*    オンライン伝票データファイル
******************************************************************
 FD  JHSDENF            BLOCK     CONTAINS  1    RECORDS.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  READ-CNT                PIC  9(08)     VALUE  ZERO.
 01  CRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  SKIP-CNT                PIC  9(08)     VALUE  ZERO.
 01  CHK-DATE                PIC  9(08)     VALUE  ZERO.
 01  WK-DT5-F112             PIC  9(08)     VALUE  ZERO.
 01  PAR-INV-FLG             PIC  X(03)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DT1-STATUS        PIC  X(02).
     03  DT2-STATUS        PIC  X(02).
     03  DT3-STATUS        PIC  X(02).
     03  DT5-STATUS        PIC  X(02).
     03  DT6-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SKY2104B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKY2104B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKY2104B".
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
*パラメタファイルよりセット
 LINKAGE                    SECTION.
 01  PARA-KBN               PIC   9(01).
 01  PARA-NOUDT1            PIC   9(08).
 01  PARA-NOUDT2            PIC   9(08).
 01  PARA-BUMON1            PIC   X(04).
 01  PARA-BUMON2            PIC   X(04).
 01  PARA-CENR1             PIC   X(01).
 01  PARA-CENR2             PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-KBN
                                       PARA-NOUDT1
                                       PARA-NOUDT2
                                       PARA-BUMON1
                                       PARA-BUMON2
                                       PARA-CENR1
                                       PARA-CENR2.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ONLDT1.
     MOVE      "ONLDT1  "   TO   AB-FILE.
     MOVE      DT1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ONLDT2.
     MOVE      "ONLDT2  "   TO   AB-FILE.
     MOVE      DT2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ONLDT3.
     MOVE      "ONLDT3  "   TO   AB-FILE.
     MOVE      DT3-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ONLDT6.
     MOVE      "ONLDT6  "   TO   AB-FILE.
     MOVE      DT6-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ONLDT5.
     MOVE      "ONLDT5  "   TO   AB-FILE.
     MOVE      DT5-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDENF.
     MOVE      "JHSDENF  "   TO   AB-FILE.
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
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL  END-FLG  =  "END".
     PERFORM  END-SEC.
*
     STOP  RUN.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     EVALUATE PARA-KBN
         WHEN 1  OPEN  INPUT  ONLDT1
         WHEN 2  OPEN  INPUT  ONLDT2
         WHEN 3  OPEN  INPUT  ONLDT3
         WHEN 4  OPEN  INPUT  ONLDT5
         WHEN 5  OPEN  INPUT  ONLDT6
     END-EVALUATE.
     OPEN     OUTPUT    JHSDENF.
     DISPLAY "KBN    = " PARA-KBN    UPON CONS.
     DISPLAY "NOUDT1 = " PARA-NOUDT1 UPON CONS.
     DISPLAY "NOUDT2 = " PARA-NOUDT2 UPON CONS.
     DISPLAY "BUMON1 = " PARA-BUMON1 UPON CONS.
     DISPLAY "BUMON2 = " PARA-BUMON2 UPON CONS.
     DISPLAY "CENR1  = " PARA-CENR1  UPON CONS.
     DISPLAY "CENR2  = " PARA-CENR2  UPON CONS.
*件数初期化
     MOVE     ZERO                TO   READ-CNT CRT-CNT SKIP-CNT.
*ファイル読込処理判定
     EVALUATE PARA-KBN
         WHEN 1 PERFORM ONLDT1-READ-SEC
         WHEN 2 PERFORM ONLDT2-READ-SEC
         WHEN 3 PERFORM ONLDT3-READ-SEC
         WHEN 4 PERFORM ONLDT5-READ-SEC
         WHEN 5 PERFORM ONLDT6-READ-SEC
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
*ファイルレコード出力
     MOVE     SPACE               TO   DEN-REC.
     INITIALIZE                        DEN-REC.
*レコード出力
     EVALUATE  PARA-KBN
         WHEN  1  MOVE  DT1-REC   TO   DEN-REC
         WHEN  2  MOVE  DT2-REC   TO   DEN-REC
         WHEN  3  MOVE  DT3-REC   TO   DEN-REC
         WHEN  4  MOVE  DT5-REC   TO   DEN-REC
         WHEN  5  MOVE  DT6-REC   TO   DEN-REC
     END-EVALUATE.
*レコード書込み
*****DISPLAY "DEN-F01 = " DEN-F01 UPON CONS.
     WRITE     DEN-REC.
     ADD       1                  TO   CRT-CNT.
*ファイル読込
     EVALUATE PARA-KBN
         WHEN 1 PERFORM ONLDT1-READ-SEC
         WHEN 2 PERFORM ONLDT2-READ-SEC
         WHEN 3 PERFORM ONLDT3-READ-SEC
         WHEN 4 PERFORM ONLDT5-READ-SEC
         WHEN 5 PERFORM ONLDT6-READ-SEC
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
     EVALUATE PARA-KBN
         WHEN 1  CLOSE ONLDT1
         WHEN 2  CLOSE ONLDT2
         WHEN 3  CLOSE ONLDT3
         WHEN 4  CLOSE ONLDT5
         WHEN 5  CLOSE ONLDT6
     END-EVALUATE.
     CLOSE     JHSDENF.
*
     DISPLAY "READ-CNT = " READ-CNT UPON CONS.
     DISPLAY "CRT-CNT  = " CRT-CNT  UPON CONS.
     DISPLAY "SKIP-CNT = " SKIP-CNT UPON CONS.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　伝票番号順読込　　　　　　　　　　　　　　　　　*
****************************************************************
 ONLDT1-READ-SEC  SECTION.
*
     MOVE     "ONLDT1-READ-SEC"   TO   SEC-NAME.
*
     READ      ONLDT1   AT  END
               MOVE     "END"     TO   END-FLG
               NOT  AT  END
               ADD       1        TO   READ-CNT
     END-READ.
*
 ONLDT1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　店舗番号順読込　　　　　　　　　　　　　　　　　*
****************************************************************
 ONLDT2-READ-SEC  SECTION.
*
     MOVE     "ONLDT2-READ-SEC"   TO   SEC-NAME.
*
     READ      ONLDT2   AT  END
               MOVE     "END"     TO   END-FLG
               NOT  AT  END
               ADD       1        TO   READ-CNT
     END-READ.
*
 ONLDT2-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　納品日順読込　　　　　　　　　　　　　　　　　　*
****************************************************************
 ONLDT3-READ-SEC  SECTION.
*
     MOVE     "ONLDT3-READ-SEC"   TO   SEC-NAME.
*
     READ      ONLDT3   AT  END
               MOVE     "END"     TO   END-FLG
               GO                 TO   ONLDT3-READ-EXIT
               NOT  AT  END
               ADD       1        TO   READ-CNT
     END-READ.
*
     MOVE      DT3-F112           TO   CHK-DATE.
     IF        CHK-DATE   >=   PARA-NOUDT1
     AND       CHK-DATE   <=   PARA-NOUDT2
               CONTINUE
     ELSE
               ADD        1       TO   SKIP-CNT
               GO                 TO   ONLDT3-READ-SEC
     END-IF.
*
 ONLDT3-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　_番順読込                                      *
****************************************************************
 ONLDT5-READ-SEC  SECTION.
*
     MOVE     "ONLDT5-READ-SEC"   TO   SEC-NAME.
*
     READ      ONLDT5   AT  END
               MOVE     "END"     TO   END-FLG
               GO                 TO   ONLDT5-READ-EXIT
               NOT  AT  END
               ADD       1        TO   READ-CNT
     END-READ.
 ONLDT5-010.
*納品日範囲チェック
     MOVE      DT5-F112           TO   CHK-DATE.
     IF        CHK-DATE   >=   PARA-NOUDT1
     AND       CHK-DATE   <=   PARA-NOUDT2
               CONTINUE
     ELSE
               ADD        1       TO   SKIP-CNT
               GO                 TO   ONLDT5-READ-SEC
     END-IF.
 ONLDT5-020.
*部門ＣＤ範囲チェック
*****DISPLAY "DT5-F12 = " DT5-F12 UPON CONS.
     IF        DT5-F12   >=   PARA-BUMON1
     AND       DT5-F12   <=   PARA-BUMON2
               CONTINUE
     ELSE
               ADD        1       TO   SKIP-CNT
               GO                 TO   ONLDT5-READ-SEC
     END-IF.
*
 ONLDT5-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　センター区分順
****************************************************************
 ONLDT6-READ-SEC  SECTION.
*
     MOVE     "ONLDT6-READ-SEC"   TO   SEC-NAME.
*
     READ      ONLDT6   AT  END
               MOVE     "END"     TO   END-FLG
               GO                 TO   ONLDT6-READ-EXIT
               NOT  AT  END
               ADD       1        TO   READ-CNT
     END-READ.
 ONLDT6-010.
*納品日範囲チェック
     MOVE      DT6-F112           TO   CHK-DATE.
     IF        CHK-DATE   >=   PARA-NOUDT1
     AND       CHK-DATE   <=   PARA-NOUDT2
               CONTINUE
     ELSE
               ADD        1       TO   SKIP-CNT
               GO                 TO   ONLDT6-READ-SEC
     END-IF.
 ONLDT6-020.
*部門ＣＤ範囲チェック
     IF        DT6-F12   >=   PARA-BUMON1
     AND       DT6-F12   <=   PARA-BUMON2
               CONTINUE
     ELSE
               ADD        1       TO   SKIP-CNT
               GO                 TO   ONLDT6-READ-SEC
     END-IF.
*センター区分範囲チェック
     IF        DT6-F278  >=   PARA-CENR1
     AND       DT6-F278  <=   PARA-CENR2
               CONTINUE
     ELSE
               ADD        1       TO   SKIP-CNT
               GO                 TO   ONLDT6-READ-SEC
     END-IF.
*
 ONLDT6-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
