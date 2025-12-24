****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　　　　　  *
*    サブシステム　　　　：　コーナン　　　ＥＤＩ　　　　　　  *
*    モジュール名　　　　：　一括ＰＤラベル発行データ作成　　　*
*    　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*    作成日／作成者　　　：　2021/01/26 INOUE                  *
*    処理概要　　　　　　：　条件に従い、一括ＰＤラベル発行用　*
*                            データ（ＣＳＶ）を作成する。　　　*
*    更新履歴                                                  *
*    更新日／更新者　　　：　2021/12/20 INOUE                  *
*    更新概要　　　　　　：　転送変更：個口（分子）　　　　　　*
*    　　　　　　　　　　　　　　　　　個口（分母）　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY1227B.
*                  流用:SSY1224B
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/01/26.
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
     SELECT   KNBULKF   ASSIGN    TO        DA-01-VI-KNBULKL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
*                       ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       BUL-F001  BUL-F002
                                            BUL-F003  BUL-F004
                                            BUL-FA01  BUL-FA02
                                            BUL-FA03  BUL-FA06
                                            BUL-FA04  BUL-FA05
                                            BUL-FB06
                        FILE  STATUS   IS   BUL-STATUS.
*----<< 一括ＰＤラベル発行データ >>----*
     SELECT   KNBULKDT  ASSIGN    TO        DA-01-S-KNBULKDT
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CSV-STATUS.
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
*    一括ＰＤラベル発行データ
******************************************************************
 FD  KNBULKDT           LABEL     RECORD   IS   STANDARD.
     COPY     KNBULKDT  OF        XFDLIB
              JOINING   CSV  AS   PREFIX.
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
     03  RWT-CNT             PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  KNBULKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENLA-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  TENMS1-INV-FLG      PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
 01  IX                      PIC  9(05)     VALUE  ZERO.
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
*ブレイク項目
 01  BRK-F001              PIC  9(08)   VALUE ZERO.
 01  BRK-F002              PIC  9(04)   VALUE ZERO.
 01  BRK-F003              PIC  9(08)   VALUE ZERO.
 01  BRK-F004              PIC  X(02)   VALUE SPACE.
 01  BRK-FA01              PIC  9(04)   VALUE ZERO.
 01  BRK-FA02              PIC  9(06)   VALUE ZERO.
 01  BRK-FA03              PIC  9(02)   VALUE ZERO.
 01  BRK-FA06              PIC  9(02)   VALUE ZERO.
 01  BRK-FA04              PIC  9(06)   VALUE ZERO.
 01  BRK-FA05              PIC  9(06)   VALUE ZERO.
*
 01  WK-ST.
     03  BUL-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  IN-STATUS         PIC  X(02).
     03  CSV-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY1227B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1227B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1227B".
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
     MOVE      "KNBULKL2 "   TO   AB-FILE.
     MOVE      BUL-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNBULKDT.
     MOVE      "KNBULKDT"   TO   AB-FILE.
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
     OPEN     INPUT     KNBULKF.
     OPEN     OUTPUT    KNBULKDT.
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
     MOVE      PARA-IN-STEN    TO   BUL-FA01.
     MOVE      PARA-IN-TORICD  TO   BUL-FA02.
     IF  PARA-IN-TORICD  =  23631
         MOVE  2363            TO   BUL-FA02
     END-IF.
     MOVE      PARA-IN-SROUTE  TO   BUL-FA03.
     MOVE      PARA-IN-SBUMON  TO   BUL-FA06.
     MOVE      PARA-IN-HDATE   TO   WK-HDATE.
     MOVE      WK-HDATE2       TO   BUL-FA04.
     MOVE      PARA-IN-NDATE   TO   WK-NDATE.
     MOVE      WK-NDATE2       TO   BUL-FA05.
     START     KNBULKF   KEY   >=   BUL-F001  BUL-F002
                                    BUL-F003  BUL-F004
                                    BUL-FA01  BUL-FA02
                                    BUL-FA03  BUL-FA06
                                    BUL-FA04  BUL-FA05
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*
*    一括ＰＤラベルファイル読込み
     PERFORM KNBULKF-READ-SEC.
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
     READ     KNBULKF
*             AT  END
         NEXT AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KNBULKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 KNBULKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"              TO   S-NAME.
*
*対象データチェック
*
 MAIN001.
     IF ( PARA-IN-JDATE     =    BUL-F001 ) AND
        ( PARA-IN-JTIME     =    BUL-F002 ) AND
        ( PARA-IN-TORICD    =    BUL-F003 )
        CONTINUE
     ELSE
        MOVE  "END"      TO   END-FLG
        GO               TO   MAIN-EXIT
     END-IF.
*
 MAIN002.
     IF  PARA-IN-SOKO   NOT =  SPACE
         IF     BUL-F004     =      PARA-IN-SOKO
                CONTINUE
         ELSE
                MOVE  "END"      TO   END-FLG
                GO               TO   MAIN-EXIT
         END-IF
     END-IF.
*
 MAIN003.
     IF  PARA-IN-NDATE  NOT = ZERO
         IF     BUL-FA05     =  WK-NDATE2
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
 MAIN004.
     IF  PARA-IN-STEN      IS     NUMERIC
         IF     BUL-FA01    >=    PARA-IN-STEN
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN005.
     IF  PARA-IN-ETEN      IS     NUMERIC
         IF     BUL-FA01    <=    PARA-IN-ETEN
                CONTINUE
         ELSE
                GO          TO   MAIN999
        END-IF
     END-IF.
*
 MAIN006.
     IF  PARA-IN-TORICD IS     NUMERIC
*********IF     BUL-FA02     =  PARA-IN-TORICD
         IF     BUL-FA02     =  WK-PARA-IN-TORICD
                CONTINUE
         ELSE
                GO          TO   MAIN999
        END-IF
     END-IF.
*
 MAIN007.
     IF  PARA-IN-SROUTE IS     NUMERIC
         IF     BUL-FA03    >=  PARA-IN-SROUTE
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN008.
     IF  PARA-IN-EROUTE IS     NUMERIC
         IF     BUL-FA03    <=  PARA-IN-EROUTE
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN009.
     IF  PARA-IN-SBUMON IS     NUMERIC
         IF     BUL-FA06    >=  PARA-IN-SBUMON
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN010.
     IF  PARA-IN-EBUMON IS     NUMERIC
         IF     BUL-FA06    <=  PARA-IN-EBUMON
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN011.
     IF  PARA-IN-HDATE  NOT = ZERO
         IF     BUL-FA04     =  WK-HDATE2
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
*
 MAIN888.
*
     PERFORM KNBULKDT-WRITE-SEC.
     MOVE    BUL-F001                TO  BRK-F001.
     MOVE    BUL-F002                TO  BRK-F002.
     MOVE    BUL-F003                TO  BRK-F003.
     MOVE    BUL-F004                TO  BRK-F004.
     MOVE    BUL-FA01                TO  BRK-FA01.
     MOVE    BUL-FA02                TO  BRK-FA02.
     MOVE    BUL-FA03                TO  BRK-FA03.
     MOVE    BUL-FA06                TO  BRK-FA06.
     MOVE    BUL-FA04                TO  BRK-FA04.
     MOVE    BUL-FA05                TO  BRK-FA05.
*
 MAIN999.
*    一括ＰＤラベルファイル読込み
     PERFORM KNBULKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　一括ＰＤラベル発行データ出力処理
****************************************************************
 KNBULKDT-WRITE-SEC  SECTION.
*
     MOVE     "KNBULKDT-WRITE-SEC" TO      S-NAME.
*
     MOVE      SPACE               TO      CSV-REC.
     INITIALIZE                            CSV-REC.
     MOVE      ","                 TO      CSV-F001K
                                           CSV-F002K
                                           CSV-F003K
                                           CSV-F004K
                                           CSV-FA01K
                                           CSV-FA02K
                                           CSV-FA03K
                                           CSV-FA04K
                                           CSV-FA05K
                                           CSV-FA06K
                                           CSV-FA07K
                                           CSV-FA08K
                                           CSV-FA09K
                                           CSV-FA10K
                                           CSV-FA11K
                                           CSV-FA12K
                                           CSV-FB07K
                                           CSV-FC01K
                                           CSV-FC02K
                                           CSV-FD01K
                                           CSV-FD02K
                                           CSV-FD03K
                                           CSV-FD04K
                                           CSV-FD05K
                                           CSV-FD06K
                                           CSV-FD07K
                                           CSV-FD08K
                                           CSV-FD09K
                                           CSV-FILK.
     MOVE      X"28"               TO      CSV-FA09S
                                           CSV-FA10S.
     MOVE      X"29"               TO      CSV-FA09E
                                           CSV-FA10E.
     MOVE      BUL-F001            TO      CSV-F001.
     MOVE      BUL-F002            TO      CSV-F002.
     MOVE      BUL-F003            TO      CSV-F003.
     MOVE      BUL-F004            TO      CSV-F004.
     MOVE      BUL-FA01            TO      CSV-FA01.
     MOVE      BUL-FA02            TO      CSV-FA02.
     MOVE      BUL-FA03            TO      CSV-FA03.
     MOVE      BUL-FA04            TO      CSV-FA04.
     MOVE      BUL-FA05            TO      CSV-FA05.
     MOVE      BUL-FA06            TO      CSV-FA06.
     MOVE      BUL-FA07            TO      CSV-FA07.
     MOVE      BUL-FA08            TO      CSV-FA08.
     MOVE      BUL-FA09            TO      CSV-FA09.
     MOVE      BUL-FA10            TO      CSV-FA10.
     MOVE      BUL-FA11            TO      CSV-FA11.
     MOVE      BUL-FA12            TO      CSV-FA12.
     MOVE      BUL-FB01            TO      CSV-FB01.
     MOVE      BUL-FB02            TO      CSV-FB02.
     MOVE      BUL-FB03            TO      CSV-FB03.
     MOVE      BUL-FB04            TO      CSV-FB04.
     MOVE      BUL-FB05            TO      CSV-FB05.
     MOVE      BUL-FB06            TO      CSV-FB06.
     MOVE      BUL-FB07            TO      CSV-FB07.
*↓2021/12/20
*    MOVE      ZERO                TO      CSV-FC01.
     MOVE      BUL-FD01            TO      CSV-FC01.
*    MOVE      ZERO                TO      CSV-FC02.
     MOVE      BUL-FD02            TO      CSV-FC02.
*↑2021/12/20
     MOVE      SYS-DATEW           TO      CSV-FD01.
     MOVE      WK-TIME(1:6)        TO      CSV-FD02.
     MOVE      PARA-IN-BUMON       TO      CSV-FD03.
     MOVE      PARA-IN-TANTOU      TO      CSV-FD04.
     WRITE     CSV-REC.
     ADD       1                   TO      WRT-CNT.
*
 KNBULKDT-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY  NC"ＰＤファイル"   "IN  = " READ-CNT  UPON CONS.
     DISPLAY  NC"発行データ　"   "OUT = " WRT-CNT   UPON CONS.
*    DISPLAY  NC"　　　　　　"   "RWT = " RWT-CNT   UPON CONS.
*
     CLOSE     KNBULKF  KNBULKDT.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
