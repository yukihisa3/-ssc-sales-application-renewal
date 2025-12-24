****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　　　　　  *
*    サブシステム　　　　：　コーナン　　　ＥＤＩ　　　　　　  *
*    モジュール名　　　　：　店直ＰＤラベルファイル作成　　　　*
*    　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*    作成日／作成者　　　：　2021/10/14 INOUE                  *
*    処理概要　　　　　　：　条件に従い、店直ＰＤラベルファイル*
*                            を箱数分作成する。　　　　　　　　*
*    更新履歴                                                  *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY1350B.
*                  流用:SSY1224B
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/10/14.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 箱数ファイル >>--*
     SELECT   KNHAKOF   ASSIGN    TO        DA-01-VI-KNHAKOL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
*                       ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HAK-F001  HAK-F002
                                            HAK-F003  HAK-F004
                                            HAK-FA05  HAK-FA01
                                            HAK-FA02  HAK-FA03
                                            HAK-FA06  HAK-FA04
                        FILE  STATUS   IS   HAK-STATUS.
*----<< 一括ＰＤラベルファイル >>----*
     SELECT   KNBULKF   ASSIGN    TO        DA-01-VI-KNBULKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       BUL-F001  BUL-F002
                                            BUL-F003  BUL-F004
                                            BUL-FA05  BUL-FA01
                                            BUL-FA02  BUL-FA03
                                            BUL-FA06  BUL-FA04
                        FILE      STATUS    BUL-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    箱数ファイル
******************************************************************
 FD  KNHAKOF            LABEL     RECORD   IS   STANDARD.
     COPY     KNHAKOF   OF        XFDLIB
              JOINING   HAK       PREFIX.
******************************************************************
*    一括ＰＤラベルファイル
******************************************************************
 FD  KNBULKF            LABEL     RECORD   IS   STANDARD.
     COPY     KNBULKF   OF        XFDLIB
              JOINING   BUL  AS   PREFIX.
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
 01  BRK-FA05              PIC  9(06)   VALUE ZERO.
 01  BRK-FA01              PIC  9(04)   VALUE ZERO.
 01  BRK-FA02              PIC  9(04)   VALUE ZERO.
 01  BRK-FA03              PIC  9(02)   VALUE ZERO.
 01  BRK-FA06              PIC  9(02)   VALUE ZERO.
 01  BRK-FA04              PIC  9(06)   VALUE ZERO.
*
 01  WK-ST.
     03  HAK-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  IN-STATUS         PIC  X(02).
     03  BUL-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY1350B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1350B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1350B".
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
                        PROCEDURE   KNHAKOF.
     MOVE      "KNHAKOL3 "   TO   AB-FILE.
     MOVE      HAK-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNBULKF.
     MOVE      "KNBULKF"    TO   AB-FILE.
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
*    OPEN     INPUT     SHTDENLA  TENMS1.
     OPEN     I-O       KNHAKOF  KNBULKF.
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
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
     IF   PARA-IN-TORICD  =  23631
          MOVE 2363             TO     WK-PARA-IN-TORICD
     ELSE
          MOVE PARA-IN-TORICD   TO     WK-PARA-IN-TORICD
     END-IF.
*    箱数ファイル　スタート
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
     MOVE      SPACE           TO   HAK-REC.
     INITIALIZE                     HAK-REC.
     MOVE      PARA-IN-JDATE   TO   HAK-F001.
     MOVE      PARA-IN-JTIME   TO   HAK-F002.
     MOVE      PARA-IN-TORICD  TO   HAK-F003.
     MOVE      PARA-IN-SOKO    TO   HAK-F004.
     MOVE      PARA-IN-NDATE   TO   WK-NDATE.
     MOVE      WK-NDATE2       TO   HAK-FA05.
     MOVE      PARA-IN-STEN    TO   HAK-FA01.
     MOVE      PARA-IN-TORICD  TO   HAK-FA02.
     IF        PARA-IN-TORICD  =  23631
               MOVE    2363    TO   HAK-FA02
     END-IF.
     MOVE      PARA-IN-SROUTE  TO   HAK-FA03.
     MOVE      PARA-IN-SBUMON  TO   HAK-FA06.
     MOVE      PARA-IN-HDATE   TO   WK-HDATE.
     MOVE      WK-HDATE2       TO   HAK-FA04.
     START     KNHAKOF   KEY   >=   HAK-F001  HAK-F002
                                    HAK-F003  HAK-F004
                                    HAK-FA05  HAK-FA01
                                    HAK-FA02  HAK-FA03
                                    HAK-FA06  HAK-FA04
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*
*    箱数ファイル読込み
     PERFORM KNHAKOF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 KNHAKOF-READ-SEC    SECTION.
*
     MOVE    "KNHAKOF-READ-SEC"    TO  S-NAME.
*
     READ     KNHAKOF
*             AT  END
         NEXT AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KNHAKOF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 KNHAKOF-READ-EXIT.
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
     IF ( PARA-IN-JDATE     =    HAK-F001 ) AND
        ( PARA-IN-JTIME     =    HAK-F002 ) AND
        ( PARA-IN-TORICD    =    HAK-F003 )
        CONTINUE
     ELSE
        MOVE  "END"      TO   END-FLG
        GO               TO   MAIN-EXIT
     END-IF.
*
 MAIN002.
     IF  PARA-IN-SOKO   NOT =  SPACE
         IF     HAK-F004     =      PARA-IN-SOKO
                CONTINUE
         ELSE
                MOVE  "END"      TO   END-FLG
                GO               TO   MAIN-EXIT
         END-IF
     END-IF.
*
 MAIN003.
     IF  PARA-IN-NDATE  NOT = ZERO
         IF     HAK-FA05     =  WK-NDATE2
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
 MAIN004.
     IF  PARA-IN-STEN      IS     NUMERIC
         IF     HAK-FA01    >=    PARA-IN-STEN
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN005.
     IF  PARA-IN-ETEN      IS     NUMERIC
         IF     HAK-FA01    <=    PARA-IN-ETEN
                CONTINUE
         ELSE
                GO          TO   MAIN999
        END-IF
     END-IF.
*
 MAIN006.
     IF  PARA-IN-TORICD IS     NUMERIC
*********IF     HAK-FA02     =  PARA-IN-TORICD
         IF     HAK-FA02     =  WK-PARA-IN-TORICD
                CONTINUE
         ELSE
                GO          TO   MAIN999
        END-IF
     END-IF.
*
 MAIN007.
     IF  PARA-IN-SROUTE IS     NUMERIC
         IF     HAK-FA03    >=  PARA-IN-SROUTE
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN008.
     IF  PARA-IN-EROUTE IS     NUMERIC
         IF     HAK-FA03    <=  PARA-IN-EROUTE
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN009.
     IF  PARA-IN-SBUMON IS     NUMERIC
         IF     HAK-FA06    >=  PARA-IN-SBUMON
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN010.
     IF  PARA-IN-EBUMON IS     NUMERIC
         IF     HAK-FA06    <=  PARA-IN-EBUMON
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN011.
     IF  PARA-IN-HDATE  NOT = ZERO
         IF     HAK-FA04     =  WK-HDATE2
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
*
 MAIN012.
     IF  HAK-FB01    NOT = ZERO
         CONTINUE
     ELSE
         GO          TO   MAIN999
     END-IF.
*
 MAIN888.
*
     MOVE    1                       TO  IX.
     PERFORM KNBULKF-WRITE-SEC  UNTIL    IX > HAK-FB01.
     MOVE    HAK-F001                TO  BRK-F001.
     MOVE    HAK-F002                TO  BRK-F002.
     MOVE    HAK-F003                TO  BRK-F003.
     MOVE    HAK-F004                TO  BRK-F004.
     MOVE    HAK-FA05                TO  BRK-FA05.
     MOVE    HAK-FA01                TO  BRK-FA01.
     MOVE    HAK-FA02                TO  BRK-FA02.
     MOVE    HAK-FA03                TO  BRK-FA03.
     MOVE    HAK-FA06                TO  BRK-FA06.
     MOVE    HAK-FA04                TO  BRK-FA04.
     PERFORM KNHAKOF-REWRITE-SEC.
*
 MAIN999.
*    箱数ファイル読込み
     PERFORM KNHAKOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　一括ＰＤラベルファイル出力処理
****************************************************************
 KNBULKF-WRITE-SEC  SECTION.
*
     MOVE     "KNBULKF-WRITE-SEC"  TO      S-NAME.
*
     MOVE      SPACE               TO      BUL-REC.
     INITIALIZE                            BUL-REC.
     MOVE      HAK-F0              TO      BUL-F0.
     MOVE      HAK-FA              TO      BUL-FA.
     MOVE      "3"                 TO      BUL-FB01.
     MOVE      HAK-FA02            TO      BUL-FB02.
     MOVE      HAK-FA03            TO      BUL-FB03.
     MOVE      HAK-FA04(3:4)       TO      BUL-FB04.
     MOVE      HAK-FA01            TO      BUL-FB05.
     MOVE      ZERO                TO      BUL-FB06.
     MOVE      ZERO                TO      BUL-FB07.
     MOVE      SYS-DATEW           TO      BUL-FC01.
     MOVE      WK-TIME(1:6)        TO      BUL-FC02.
     MOVE      PARA-IN-BUMON       TO      BUL-FC03.
     MOVE      PARA-IN-TANTOU      TO      BUL-FC04.
     WRITE     BUL-REC.
     ADD       1                   TO      IX.
     ADD       1                   TO      WRT-CNT.
*
 KNBULKF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　箱数ファイル更新　　　　　　　　　　　　　　　　　　　*
****************************************************************
 KNHAKOF-REWRITE-SEC  SECTION.
*
     MOVE     "KNHAKOF-REWRITE-SEC"   TO   S-NAME.
*
     MOVE      "1"                 TO      HAK-FC09.
     MOVE      SYS-DATEW           TO      HAK-FC10.
     MOVE      WK-TIME(1:6)        TO      HAK-FC11.
     MOVE      PARA-IN-BUMON       TO      HAK-FC12.
     MOVE      PARA-IN-TANTOU      TO      HAK-FC13.
     REWRITE   HAK-REC.
     ADD       1                   TO      RWT-CNT.
*
 KNHAKOF-REWRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY  NC"箱数ファイル"   "IN  = " READ-CNT  UPON CONS.
     DISPLAY  NC"ラベルファイル" "OUT = " WRT-CNT   UPON CONS.
     DISPLAY  NC"箱数ファイル"   "RWT = " RWT-CNT   UPON CONS.
*
     CLOSE     KNHAKOF  KNBULKF.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
