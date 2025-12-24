****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　　　　　  *
*    サブシステム　　　　：　コーナン　　　ＥＤＩ　　　　　　  *
*    モジュール名　　　　：　取引先ＰＤデータ抽出　　　　　　　*
*    作成日／作成者　　　：　2021/02/01 INOUE                  *
*    処理概要　　　　　　：　条件に従い、　　　　　　　　　　　*
*                            取引先ＰＤデータを抽出する　　　　*
*    更新履歴                                                  *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY1234B.
*                  流用:SSY1211B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/02/01.
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
     SELECT   KNBULKF   ASSIGN    TO        DA-01-VI-KNBULKL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       BUL-F001  BUL-F002
                                            BUL-F003  BUL-F004
                                            BUL-FA05
                                            BUL-FB
                        FILE  STATUS   IS   BUL-STATUS.
*----<< 取引先ＰＤ抽出データ >>----*
     SELECT   KNSNDPDW  ASSIGN    TO        DA-01-VS-KNSNDPDW
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    OUT-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    一括ＰＤファイル
******************************************************************
 FD  KNBULKF            LABEL     RECORD   IS   STANDARD.
     COPY     KNBULKF   OF        XFDLIB
              JOINING   BUL       PREFIX.
******************************************************************
*    取引先ＰＤ抽出データ
******************************************************************
 FD  KNSNDPDW           LABEL     RECORD   IS   STANDARD.
     COPY     KNSNDPDW  OF        XFDLIB
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
     03  KNSNDPDW-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  SHTDENLA-INV-FLG    PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
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
     03  BUL-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  IN-STATUS         PIC  X(02).
     03  OUT-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY1234B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1234B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1234B".
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
                                  PARA-IN-NDATE.
*
******************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNBULKF.
     MOVE      "KNBULKL3 "   TO   AB-FILE.
     MOVE      BUL-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNSNDPDW.
     MOVE      "KNSNDPDW"   TO   AB-FILE.
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
     OPEN     I-O       KNBULKF.
     OPEN     OUTPUT    KNSNDPDW.
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
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
*    一括ＰＤファイル　スタート
*T
*    DISPLAY  "PARA-IN-JDATE  =" PARA-IN-JDATE UPON CONS.
*    DISPLAY  "PARA-IN-JTIME  =" PARA-IN-JTIME UPON CONS.
*    DISPLAY  "PARA-IN-TORICD =" PARA-IN-TORICD UPON CONS.
*    DISPLAY  "PARA-IN-SOKO   =" PARA-IN-SOKO   UPON CONS.
*T
     MOVE      SPACE           TO   BUL-REC.
     INITIALIZE                     BUL-REC.
     MOVE      PARA-IN-JDATE   TO   BUL-F001.
     MOVE      PARA-IN-JTIME   TO   BUL-F002.
     MOVE      PARA-IN-TORICD  TO   BUL-F003.
     MOVE      PARA-IN-SOKO    TO   BUL-F004.
     MOVE      PARA-IN-NDATE   TO   WK-NDATE.
     MOVE      WK-NDATE2       TO   BUL-FA05.
     MOVE      ZERO            TO   BUL-FB01.
     MOVE      ZERO            TO   BUL-FB02.
     MOVE      ZERO            TO   BUL-FB03.
     MOVE      ZERO            TO   BUL-FB04.
     MOVE      ZERO            TO   BUL-FB05.
     MOVE      ZERO            TO   BUL-FB06.
     MOVE      ZERO            TO   BUL-FB07.
     START     KNBULKF   KEY   >=   BUL-F001  BUL-F002
                                    BUL-F003  BUL-F004
                                    BUL-FA05  BUL-FB
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*
*    一括ＰＤファイル読込み
     PERFORM KNBULKF-READ-SEC.
     IF      END-FLG  NOT = "END"
             CONTINUE
     ELSE
             MOVE     4010          TO  PROGRAM-STATUS
             STOP     RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 KNBULKF-READ-SEC    SECTION.
*
     MOVE    "KNBULKF-READ-SEC"    TO  S-NAME.
*
     READ     KNBULKF
              AT  END
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
 MAIN011.
     IF ( PARA-IN-JDATE     =    BUL-F001 ) AND
        ( PARA-IN-JTIME     =    BUL-F002 ) AND
        ( PARA-IN-TORICD    =    BUL-F003 )
        CONTINUE
     ELSE
        MOVE  "END"      TO   END-FLG
        GO               TO   MAIN-EXIT
     END-IF.
*
 MAIN012.
     IF  PARA-IN-SOKO   NOT =  SPACE
         IF     BUL-F004     =      PARA-IN-SOKO
                CONTINUE
         ELSE
                MOVE  "END"      TO   END-FLG
                GO               TO   MAIN-EXIT
         END-IF
     END-IF.
*
 MAIN021.
     IF  PARA-IN-NDATE  NOT = ZERO
         IF     BUL-FA05     =  WK-NDATE2
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
     MOVE    BUL-REC                 TO  OUT-REC.
     WRITE   OUT-REC.
     ADD     1                       TO  WRT-CNT.
*
 MAIN888.
     MOVE    "1"                     TO  BUL-FC09.
     MOVE    SYS-DATEW               TO  BUL-FC10.
     MOVE    WK-TIME(1:6)            TO  BUL-FC11.
     MOVE    PARA-IN-BUMON           TO  BUL-FC12.
     MOVE    PARA-IN-TANTOU          TO  BUL-FC13.
     REWRITE BUL-REC.
*
 MAIN999.
*    一括ＰＤファイル読込み
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
     DISPLAY   NC"一括ＰＤＦ" "IN  = " READ-CNT  UPON CONS.
     DISPLAY   NC"抽出データ" "OUT = " WRT-CNT   UPON CONS.
*    DISPLAY   NC"抽出対象外" "SKIP= " SKIP-CNT  UPON CONS.
*
     IF        WRT-CNT  =  ZERO
               MOVE    4010    TO    PROGRAM-STATUS
               STOP    RUN
     END-IF.
*
     CLOSE     KNBULKF  KNSNDPDW.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
