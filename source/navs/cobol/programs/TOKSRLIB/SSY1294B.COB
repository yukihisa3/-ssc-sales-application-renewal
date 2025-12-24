****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　　　　　　*
*    サブシステム　　　　：　コーナン　　ＥＤＩ　　　　　　　　*
*    モジュール名　　　　：　一括ＰＤラベルファイル削除　　　　*
*    作成日／更作成者　　：　2021/01/21 INOUE                  *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を削除する。　　　　　　　　　　　*
*    更新日／更新者　　　：　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY1294B.
*                  流用:SSY1295B
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/01/21.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*一括ＰＤラベルファイル
     SELECT   KNBULKF   ASSIGN    TO        DA-01-VI-KNBULKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       BUL-F001   BUL-F002
                                            BUL-F003   BUL-F004
                                            BUL-FA05   BUL-FA01
                                            BUL-FA02   BUL-FA03
                                            BUL-FA06   BUL-FA04
                        FILE      STATUS    BUL-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    一括ＰＤラベルファイル
******************************************************************
 FD  KNBULKF            LABEL RECORD   IS   STANDARD.
     COPY     KNBULKF   OF        XFDLIB
              JOINING   BUL       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  DEL-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  KNBULKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
 01  WK-PARA-TORICD          PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-NDATE.
     03  WK-NDATE1         PIC 9(02).
     03  WK-NDATE2         PIC 9(06).
 01  WK-HDATE.
     03  WK-HDATE1         PIC 9(02).
     03  WK-HDATE2         PIC 9(06).
 01  WK-ST.
     03  BUL-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY1294B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1294B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1294B".
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
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-STEN              PIC   9(05).
 01  PARA-ETEN              PIC   9(05).
 01  PARA-SROUTE            PIC   9(02).
 01  PARA-EROUTE            PIC   9(02).
 01  PARA-SBUMON            PIC   9(02).
 01  PARA-EBUMON            PIC   9(02).
 01  PARA-HDATE             PIC   9(08).
 01  PARA-NDATE             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-STEN
                                       PARA-ETEN
                                       PARA-SROUTE
                                       PARA-EROUTE
                                       PARA-SBUMON
                                       PARA-EBUMON
                                       PARA-HDATE
                                       PARA-NDATE.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNBULKF.
     MOVE      "KNBULKF "   TO   AB-FILE.
     MOVE      BUL-STATUS   TO   AB-STS.
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
     OPEN     I-O       KNBULKF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
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
*+
     IF   PARA-TORICD  =  23631
          MOVE   2363           TO   WK-PARA-TORICD
     ELSE
          MOVE   PARA-TORICD    TO   WK-PARA-TORICD
     END-IF.
*    一括ＰＤラベルファイルスタート
     MOVE     SPACE          TO   BUL-REC.
     INITIALIZE                   BUL-REC.
     MOVE     PARA-JDATE     TO   BUL-F001.
     MOVE     PARA-JTIME     TO   BUL-F002.
     MOVE     PARA-TORICD    TO   BUL-F003.
     MOVE     PARA-SOKO      TO   BUL-F004.
     MOVE     PARA-NDATE     TO   WK-NDATE.
     MOVE     WK-NDATE2      TO   BUL-FA05.
     MOVE     PARA-STEN      TO   BUL-FA01.
     MOVE     PARA-TORICD    TO   BUL-FA02.
     IF       PARA-TORICD  =  23631
              MOVE    2363   TO   BUL-FA02
     END-IF.
     MOVE     PARA-SROUTE    TO   BUL-FA03.
     MOVE     PARA-SBUMON    TO   BUL-FA06.
     MOVE     PARA-HDATE     TO   WK-HDATE.
     MOVE     WK-HDATE2      TO   BUL-FA04.
     START    KNBULKF   KEY  >=   BUL-F001   BUL-F002   BUL-F003
                                  BUL-F004   BUL-FA05   BUL-FA01
                                  BUL-FA02   BUL-FA03   BUL-FA06
                                  BUL-FA04
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    一括ＰＤラベルファイル読込み
     PERFORM KNBULKF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 KNBULKF-READ-SEC    SECTION.
*
     READ     KNBULKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KNBULKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  BUL-F001
     AND      PARA-JTIME  =  BUL-F002
     AND      PARA-TORICD =  BUL-F003
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  KNBULKF-READ-EXIT
     END-IF.
*    取引先のチェック
*    IF       PARA-TORICD  =  ZERO
*             CONTINUE
*    ELSE
*             IF   PARA-TORICD  =  BUL-F003
*                  CONTINUE
*             ELSE
*                  MOVE     "END"        TO  END-FLG
*                  GO                    TO  KNBULKF-READ-EXIT
*             END-IF
*    END-IF.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  BUL-F004
                   CONTINUE
              ELSE
                   GO            TO  KNBULKF-READ-SEC
              END-IF
     END-IF.
*    納品日のチェック
     IF       PARA-NDATE  =  ZERO
              CONTINUE
     ELSE
              MOVE PARA-NDATE TO WK-NDATE
              IF  WK-NDATE2   =  BUL-FA05
                  CONTINUE
              ELSE
                  GO        TO   KNBULKF-READ-SEC
              END-IF
     END-IF.
*
*    センターチェック
     IF  PARA-STEN      IS     NUMERIC
         IF     BUL-FA01    >=    PARA-STEN
                CONTINUE
         ELSE
                GO          TO   KNBULKF-READ-SEC
         END-IF
     END-IF.
     IF  PARA-ETEN      IS     NUMERIC
         IF     BUL-FA01    <=    PARA-ETEN
                CONTINUE
         ELSE
                GO          TO   KNBULKF-READ-SEC
        END-IF
     END-IF.
*
*    取引先チェック
     IF  PARA-TORICD IS     NUMERIC
*********IF     BUL-FA02     =  PARA-TORICD
         IF     BUL-FA02     =  WK-PARA-TORICD
                CONTINUE
         ELSE
                GO          TO  KNBULKF-READ-SEC
        END-IF
     END-IF.
*
*    ルートチェック
     IF  PARA-SROUTE IS     NUMERIC
         IF     BUL-FA03    >=  PARA-SROUTE
                CONTINUE
         ELSE
                GO          TO  KNBULKF-READ-SEC
         END-IF
     END-IF.
     IF  PARA-EROUTE IS     NUMERIC
         IF     BUL-FA03    <=  PARA-EROUTE
                CONTINUE
         ELSE
                GO          TO  KNBULKF-READ-SEC
         END-IF
     END-IF.
*
*    部門チェック
     IF  PARA-SBUMON IS     NUMERIC
         IF     BUL-FA06    >=  PARA-SBUMON
                CONTINUE
         ELSE
                GO          TO  KNBULKF-READ-SEC
         END-IF
     END-IF.
     IF  PARA-EBUMON IS     NUMERIC
         IF     BUL-FA06    <=  PARA-EBUMON
                CONTINUE
         ELSE
                GO          TO  KNBULKF-READ-SEC
         END-IF
     END-IF.
*
*    発注日チェック
     IF  PARA-HDATE  NOT = ZERO
         IF     BUL-FA04     =  WK-HDATE2
                CONTINUE
         ELSE
                GO          TO   KNBULKF-READ-SEC
         END-IF
     END-IF.
*
 KNBULKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    一括ＰＤラベルファイル削除
     DELETE   KNBULKF.
     ADD      1                   TO   DEL-CNT.
 MAIN010.
*    一括ＰＤラベルファイル読込み
     PERFORM KNBULKF-READ-SEC.
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
     DISPLAY NC"ラベルファイル" "READ CNT = " READ-CNT
                                              UPON CONS.
     DISPLAY NC"ラベルファイル" "DELE CNT = " DEL-CNT
                                              UPON CONS.
*
     CLOSE     KNBULKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
