****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　　　　　  *
*    サブシステム　　　　：　コーナン　　　ＥＤＩ　　　　　　  *
*    モジュール名　　　　：　一括ＰＤラベル荷番採番　　　　　　*
*    　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*    作成日／作成者　　　：　2021/01/21 INOUE                  *
*    処理概要　　　　　　：　一括ＰＤラベルに印字する「荷番」　*
*                            を採番する。　　　　　　　　　　　*
*    更新履歴                                                  *
*    更新日／更新者　　　：　2021/12/20 INOUE                  *
*    更新概要　　　　　　：　個口（分子）・個口（分母）項目　　*
*    　　　　　　　　　　　　追加によりＩＮＰＵＴのＬＦを　　　*
*    　　　　　　　　　　　　１から５に変更　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY1225B.
*                  流用:SSY1224B
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
*----<< 一括ＰＤラベルファイル >>--*
*↓2021.12.20
*    SELECT   KNBULKF   ASSIGN    TO        DA-01-VI-KNBULKL1
*                       ORGANIZATION        INDEXED
*                       ACCESS    MODE      DYNAMIC
*                       RECORD    KEY       BUL-F001  BUL-F002
*                                           BUL-F003  BUL-F004
*                                           BUL-FA05  BUL-FA01
*                                           BUL-FA02  BUL-FA03
*                                           BUL-FA06  BUL-FA04
*                       FILE  STATUS   IS   BUL-STATUS.
     SELECT   KNBULKF   ASSIGN    TO        DA-01-VI-KNBULKL5
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       BUL-F001  BUL-F002
                                            BUL-F003  BUL-F004
                                            BUL-FA05  BUL-FA01
                                            BUL-FA02  BUL-FA03
                                            BUL-FA06  BUL-FA04
                                            BUL-FD01
                        FILE  STATUS   IS   BUL-STATUS.
*↑2021.12.20
*----<< 店舗マスタ >>----*
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    一括ＰＤラベルファイル
******************************************************************
 FD  KNBULKF            LABEL     RECORD   IS   STANDARD.
     COPY     KNBULKF   OF        XFDLIB
              JOINING   BUL       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1             LABEL     RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN  AS   PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  WRT-CNT             PIC  9(08)     VALUE  ZERO.
     03  RWT-CNT             PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  KNBULKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENLA-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  TENMS1-INV-FLG      PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
 01  CNT-NIBAN               PIC  9(05)     VALUE  ZERO.
 01  HIT-FLG                 PIC  X(03)     VALUE  SPACE.
 01  BRK-FLG                 PIC  X(03)     VALUE  SPACE.
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
*    Ｃ／Ｄ計算サブ（パラメタ用）
 01  LINK-AREA2.
     03  LINK-IN.
*        05  LI-CODE         PIC  X(21).
         05  LI-CODE         PIC  X(19).
     03  LINK-OUT.
         05  LO-ERR          PIC  9(01).
*        05  LO-CODE         PIC  X(22).
         05  LO-CODE         PIC  X(20).
*
*
*ブレイク項目
 01  BRK-F001              PIC  9(08)   VALUE ZERO.
 01  BRK-F002              PIC  9(04)   VALUE ZERO.
 01  BRK-F003              PIC  9(08)   VALUE ZERO.
 01  BRK-F004              PIC  X(02)   VALUE SPACE.
 01  BRK-FA05              PIC  9(06)   VALUE ZERO.
 01  BRK-FA01              PIC  9(04)   VALUE ZERO.
 01  BRK-FA02              PIC  9(04)   VALUE ZERO.
 01  BRK-FA03              PIC  9(02)   VALUE ZERO.
 01  BRK-FA06              PIC  9(02)   VALUE ZERO.
 01  BRK-FA04              PIC  9(06)   VALUE ZERO.
*
 01  WK-ST.
     03  BUL-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  IN-STATUS         PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY1225B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1225B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1225B".
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
 01  PARA-IN-BUMON             PIC   X(04).
 01  PARA-IN-TANTOU            PIC   X(02).
 01  PARA-IN-JDATE             PIC   9(08).
 01  PARA-IN-JTIME             PIC   9(04).
 01  PARA-IN-TORICD            PIC   9(08).
 01  PARA-IN-SOKO              PIC   X(02).
 01  PARA-IN-STEN              PIC   9(05).
 01  PARA-IN-ETEN              PIC   9(05).
 01  PARA-IN-SROUTE            PIC   9(02).
 01  PARA-IN-EROUTE            PIC   9(02).
 01  PARA-IN-SBUMON            PIC   9(02).
 01  PARA-IN-EBUMON            PIC   9(02).
 01  PARA-IN-HDATE             PIC   9(08).
 01  PARA-IN-NDATE             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING
                                  PARA-IN-BUMON
                                  PARA-IN-TANTOU
                                  PARA-IN-JDATE
                                  PARA-IN-JTIME
                                  PARA-IN-TORICD
                                  PARA-IN-SOKO
                                  PARA-IN-STEN
                                  PARA-IN-ETEN
                                  PARA-IN-SROUTE
                                  PARA-IN-EROUTE
                                  PARA-IN-SBUMON
                                  PARA-IN-EBUMON
                                  PARA-IN-HDATE
                                  PARA-IN-NDATE.
*
******************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNBULKF.
     MOVE      "KNBULKL5 "   TO   AB-FILE.
     MOVE      BUL-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1"     TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
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
     OPEN     I-O       KNBULKF  TENMS1.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG SHTDENLA-INV-FLG
                                             TENMS1-INV-FLG.
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
*    一括ＰＤラベルファイル　スタート
*T
*    DISPLAY  "PARA-IN-JDATE  =" PARA-IN-JDATE  UPON CONS.
*    DISPLAY  "PARA-IN-JTIME  =" PARA-IN-JTIME  UPON CONS.
*    DISPLAY  "PARA-IN-TORICD =" PARA-IN-TORICD UPON CONS.
*    DISPLAY  "PARA-IN-SOKO   =" PARA-IN-SOKO   UPON CONS.
*    DISPLAY  "PARA-IN-STEN   =" PARA-IN-STEN   UPON CONS.
*    DISPLAY  "PARA-IN-ETEN   =" PARA-IN-ETEN   UPON CONS.
*    DISPLAY  "PARA-IN-SROUTE =" PARA-IN-SROUTE UPON CONS.
*    DISPLAY  "PARA-IN-EROUTE =" PARA-IN-EROUTE UPON CONS.
*    DISPLAY  "PARA-IN-SBUMON =" PARA-IN-SBUMON UPON CONS.
*    DISPLAY  "PARA-IN-EBUMON =" PARA-IN-EBUMON UPON CONS.
*    DISPLAY  "PARA-IN-HDATE  =" PARA-IN-HDATE  UPON CONS.
*    DISPLAY  "PARA-IN-NDATE  =" PARA-IN-NDATE  UPON CONS.
*T
     MOVE      SPACE           TO   BUL-REC.
     INITIALIZE                     BUL-REC.
     MOVE      PARA-IN-JDATE   TO   BUL-F001.
     MOVE      PARA-IN-JTIME   TO   BUL-F002.
     MOVE      PARA-IN-TORICD  TO   BUL-F003.
     MOVE      PARA-IN-SOKO    TO   BUL-F004.
     MOVE      PARA-IN-NDATE   TO   WK-NDATE.
     MOVE      WK-NDATE2       TO   BUL-FA05.
     MOVE      PARA-IN-STEN    TO   BUL-FA01.
     MOVE      PARA-IN-TORICD  TO   BUL-FA02.
     IF        PARA-IN-TORICD  =  23631
               MOVE  2363      TO   BUL-FA02
     END-IF.
     MOVE      PARA-IN-SROUTE  TO   BUL-FA03.
     MOVE      PARA-IN-SBUMON  TO   BUL-FA06.
     MOVE      PARA-IN-HDATE   TO   WK-HDATE.
     MOVE      WK-HDATE2       TO   BUL-FA04.
     START     KNBULKF   KEY   >=   BUL-F001  BUL-F002
                                    BUL-F003  BUL-F004
                                    BUL-FA05  BUL-FA01
                                    BUL-FA02  BUL-FA03
                                    BUL-FA06  BUL-FA04
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*
*    一括ＰＤラベルファイル読込み
     PERFORM KNBULKF-READ-SEC.
     IF      END-FLG  =  "END"
             DISPLAY NC"処理対象なし" UPON CONS
*            GO           TO          INIT-EXIT
             CLOSE      KNBULKF       TENMS1
             STOP                     RUN
     ELSE
             MOVE    "HIT"        TO  HIT-FLG
             MOVE    BUL-F001     TO  BRK-F001
             MOVE    BUL-F002     TO  BRK-F002
             MOVE    BUL-F003     TO  BRK-F003
             MOVE    BUL-F004     TO  BRK-F004
             MOVE    BUL-FA05     TO  BRK-FA05
             MOVE    BUL-FA01     TO  BRK-FA01
     END-IF.
*    店舗マスタ検索（荷番取得）
     PERFORM TENMS1-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　一括ＰＤラベルファイル読込み　　　　　　　　　　*
****************************************************************
 KNBULKF-READ-SEC    SECTION.
*
     MOVE    "KNBULKF-READ-SEC"    TO  S-NAME.
*
 KNBULKF-READ-000.
     READ     KNBULKF
*             AT  END
         NEXT AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KNBULKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
*対象データチェック
*
 KNBULKF-READ-001.
     IF ( PARA-IN-JDATE     =    BUL-F001 ) AND
        ( PARA-IN-JTIME     =    BUL-F002 ) AND
        ( PARA-IN-TORICD    =    BUL-F003 )
        CONTINUE
     ELSE
        MOVE  "END"      TO   END-FLG
        GO               TO   KNBULKF-READ-EXIT
     END-IF.
*
 KNBULKF-READ-002.
     IF  PARA-IN-SOKO   NOT =  SPACE
         IF     BUL-F004     =      PARA-IN-SOKO
                CONTINUE
         ELSE
                MOVE  "END"      TO   END-FLG
                GO               TO   KNBULKF-READ-EXIT
         END-IF
     END-IF.
*
 KNBULKF-READ-003.
     IF  PARA-IN-NDATE  NOT = ZERO
         IF     BUL-FA05     =  WK-NDATE2
                CONTINUE
         ELSE
                GO          TO   KNBULKF-READ-000
         END-IF
     END-IF.
 KNBULKF-READ-004.
     IF  PARA-IN-STEN      IS     NUMERIC
         IF     BUL-FA01    >=    PARA-IN-STEN
                CONTINUE
         ELSE
                GO          TO   KNBULKF-READ-000
         END-IF
     END-IF.
*
 KNBULKF-READ-005.
     IF  PARA-IN-ETEN      IS     NUMERIC
         IF     BUL-FA01    <=    PARA-IN-ETEN
                CONTINUE
         ELSE
                GO          TO   KNBULKF-READ-000
        END-IF
     END-IF.
*
 KNBULKF-READ-006.
     IF  PARA-IN-TORICD IS     NUMERIC
*********IF     BUL-FA02     =  PARA-IN-TORICD
         IF     BUL-FA02     =  WK-PARA-IN-TORICD
                CONTINUE
         ELSE
                GO          TO   KNBULKF-READ-000
        END-IF
     END-IF.
*
 KNBULKF-READ-007.
     IF  PARA-IN-SROUTE IS     NUMERIC
         IF     BUL-FA03    >=  PARA-IN-SROUTE
                CONTINUE
         ELSE
                GO          TO   KNBULKF-READ-000
         END-IF
     END-IF.
*
 KNBULKF-READ-008.
     IF  PARA-IN-EROUTE IS     NUMERIC
         IF     BUL-FA03    <=  PARA-IN-EROUTE
                CONTINUE
         ELSE
                GO          TO   KNBULKF-READ-000
         END-IF
     END-IF.
*
 KNBULKF-READ-009.
     IF  PARA-IN-SBUMON IS     NUMERIC
         IF     BUL-FA06    >=  PARA-IN-SBUMON
                CONTINUE
         ELSE
                GO          TO   KNBULKF-READ-000
         END-IF
     END-IF.
*
 KNBULKF-READ-010.
     IF  PARA-IN-EBUMON IS     NUMERIC
         IF     BUL-FA06    <=  PARA-IN-EBUMON
                CONTINUE
         ELSE
                GO          TO   KNBULKF-READ-000
         END-IF
     END-IF.
*
 KNBULKF-READ-011.
     IF  PARA-IN-HDATE  NOT = ZERO
         IF     BUL-FA04     =  WK-HDATE2
                CONTINUE
         ELSE
                GO          TO   KNBULKF-READ-000
         END-IF
     END-IF.
*
 KNBULKF-READ-012.
     IF  BUL-FB06           =    ZERO
         CONTINUE
     ELSE
         GO                 TO   KNBULKF-READ-000
     END-IF.
*
 KNBULKF-READ-EXIT.
     EXIT.
****************************************************************
*　　店舗マスタ検索（荷番取得）
****************************************************************
 TENMS1-READ-SEC  SECTION.
*
     MOVE     "TENMS1-READ-SEC"    TO      S-NAME.
*
     MOVE     BUL-FA02               TO  TEN-F52.
     MOVE     BUL-FA01               TO  TEN-F011.
     READ     TENMS1
       INVALID
             DISPLAY NC"店舗マスタなし！" UPON CONS
             DISPLAY NC"取引先コード＝" PARA-IN-TORICD UPON CONS
             DISPLAY NC"店舗コード　＝" BUL-F001       UPON CONS
             MOVE    4010            TO  PROGRAM-STATUS
             MOVE    "END"           TO  END-FLG
             GO                      TO  MAIN-EXIT
       NOT INVALID
*            荷番保管
             MOVE    TEN-F51         TO  CNT-NIBAN
     END-READ.
*
 TENMS1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"              TO   S-NAME.
*
*ブレイクチェック
*
 MAIN001.
     IF ( BUL-F001    =    BRK-F001  ) AND
        ( BUL-F002    =    BRK-F002  ) AND
        ( BUL-F003    =    BRK-F003  ) AND
        ( BUL-F004    =    BRK-F004  ) AND
        ( BUL-FA05    =    BRK-FA05  ) AND
        ( BUL-FA01    =    BRK-FA01  )
          CONTINUE
     ELSE
*         店舗マスタ更新（荷番）
          MOVE        CNT-NIBAN    TO    TEN-F51
          REWRITE     TEN-REC
          MOVE        "BRK"        TO    BRK-FLG
          MOVE        "HIT"        TO    HIT-FLG
*
*         ブレイクキー入替
          MOVE        BUL-F001     TO    BRK-F001
          MOVE        BUL-F002     TO    BRK-F002
          MOVE        BUL-F003     TO    BRK-F003
          MOVE        BUL-F004     TO    BRK-F004
          MOVE        BUL-FA05     TO    BRK-FA05
          MOVE        BUL-FA01     TO    BRK-FA01
*         店舗マスタ検索（荷番取得）
          PERFORM     TENMS1-READ-SEC
     END-IF.
*
 MAIN002.
*    荷番カウントアップ
     IF   CNT-NIBAN   =        99999
          MOVE   1    TO       CNT-NIBAN
     ELSE
          ADD    1    TO       CNT-NIBAN
     END-IF.
*    チェックデジット計算
     PERFORM                   900-CD-GET.
*
 MAIN003.
*    一括ＰＤラベルファイル更新
     PERFORM                   KNBULKF-REWRITE-SEC.
*
 MAIN999.
*    一括ＰＤラベルファイル読込み
     MOVE        SPACE         TO      HIT-FLG.
     PERFORM                   KNBULKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     納品管理番号（Ｃ/Ｄ）取得
*                  モジュラス１０　ウェイト３
*--------------------------------------------------------------*
 900-CD-GET          SECTION.
*
     MOVE     "900-CD-GET"     TO      S-NAME.
*
     INITIALIZE                        LINK-AREA2.
*
*   区分
     MOVE     BUL-FB01         TO      LI-CODE(1:1).
*   取引先コード
     MOVE     BUL-FB02         TO      LI-CODE(2:4).
*   ルートコード
     MOVE     BUL-FB03         TO      LI-CODE(6:2).
*   部門コード
     MOVE     BUL-FB04         TO      LI-CODE(8:4).
*   センターコード
     MOVE     BUL-FB05         TO      LI-CODE(12:3).
*   荷番　　
     MOVE     CNT-NIBAN        TO      LI-CODE(15:5).
*
     CALL     "SKYSBCK5"       USING   LINK-AREA2.
*T↓
*    DISPLAY  "*** LI-CODE= "  LI-CODE  " ***"    UPON CONS.
*    DISPLAY  "*** LO-CODE= "  LO-CODE  " ***"    UPON CONS.
*T↑
     IF       LO-ERR   NOT =   0
              DISPLAY NC"ＰＤコード採番エラー"    UPON CONS
              DISPLAY "LO-ERR  = "  LO-ERR        UPON CONS
              MOVE    "4010"        TO            PROGRAM-STATUS
              STOP  RUN
     END-IF.
*
 900-CD-GET-EXIT.
     EXIT.
****************************************************************
*　　一括ＰＤラベルファイル更新
****************************************************************
 KNBULKF-REWRITE-SEC  SECTION.
*
     MOVE     "KNBULKF-REWRITE-SEC"   TO   S-NAME.
*
     MOVE      CNT-NIBAN           TO      BUL-FB06.
     MOVE      LO-CODE(20:1)       TO      BUL-FB07.
     MOVE      SYS-DATEW           TO      BUL-FC05.
     MOVE      WK-TIME(1:6)        TO      BUL-FC06.
     MOVE      PARA-IN-BUMON       TO      BUL-FC07.
     MOVE      PARA-IN-TANTOU      TO      BUL-FC08.
     REWRITE   BUL-REC.
     ADD       1                   TO      RWT-CNT.
*
 KNBULKF-REWRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"    TO    S-NAME.
*
*    店舗マスタ更新（荷番）
*****IF       HIT-FLG  =  "HIT"
*             MOVE     CNT-NIBAN    TO    TEN-F51
*             REWRITE  TEN-REC
*****END-IF.
     MOVE     CNT-NIBAN    TO    TEN-F51.
     REWRITE  TEN-REC.
*
     DISPLAY  NC"ラベルファイル" "IN  = " READ-CNT  UPON CONS.
*    DISPLAY  NC"ラベルファイル" "OUT = " WRT-CNT   UPON CONS.
     DISPLAY  NC"ラベルファイル" "RWT = " RWT-CNT   UPON CONS.
*
     CLOSE     KNBULKF  TENMS1.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
