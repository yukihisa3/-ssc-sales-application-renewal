****************************************************************
*    （営業所－データ抽出バージョン）                          *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　営業所データ連携                  *
*    業務名　　　　　　　：　営業所データ連携                  *
*    モジュール名　　　　：　手書伝票データ更新処理            *
*    作成日／更新日　　　：　2000/03/30                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　受信した本社手書伝票データを，    *
*                            売上伝票ファイルへ更新する。      *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SEG0100B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/03/30.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信用伝票データ
     SELECT   JHTDENF   ASSIGN    TO        JHTDENF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SDE-STATUS.
*
*売上伝票データ
     SELECT   SHTDENL1  ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENL1
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    受信用売上伝票データ
******************************************************************
 FD  JHTDENF            LABEL RECORD   IS   STANDARD.
     COPY     JHTDENF   OF        XFDLIB
              JOINING   SDE       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  READ-CNT                PIC  9(08)     VALUE  ZERO.
 01  DEL-CNT                 PIC  9(08)     VALUE  ZERO.
 01  ADD-CNT                 PIC  9(08)     VALUE  ZERO.
 01  IXA                     PIC  9(02)     VALUE  ZERO.
 01  WRK-SDE-F02             PIC  9(09)     VALUE  ZERO.
*
 01  WK-PARA-AREA.
     03  WK-PARA             OCCURS  12.
       05  WK-TORICD         PIC  9(08).
       05  WK-SDENNO         PIC  9(09).
       05  WK-EDENNO         PIC  9(09).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE            PIC 9(06).
     03  SYS-DATEW           PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  SDE-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SEG0100B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SEG0100B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SEG0100B".
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
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHTDENF.
     MOVE      "JHTDENF "   TO   AB-FILE.
     MOVE      SDE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENL1.
     MOVE      "SHTDENL1"   TO   AB-FILE.
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
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
     STOP  RUN.
*
 GENERAL-PROCESS-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN     INPUT     JHTDENF.
     OPEN     I-O       SHTDENL1.
     DISPLAY  MSG-START UPON CONS.
*
     PERFORM JHTDENF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*伝票_ブレイク判定
     IF       SDE-F02  NOT =  WRK-SDE-F02
*             売上伝票削除
              PERFORM SHTDENF-START-SEC
              MOVE     SDE-F02     TO      WRK-SDE-F02
     END-IF.
*売上伝票レコード初期化
     MOVE     SPACE                TO      DEN-REC.
     INITIALIZE                            DEN-REC.
*受信データ_売上伝票
     MOVE     SDE-REC              TO      DEN-REC.
*レコード追加
     WRITE    DEN-REC.
     ADD      1                    TO      ADD-CNT
*受信手書伝票データ読込み
     PERFORM JHTDENF-READ-SEC.
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
     DISPLAY "## ｼﾞｭｼﾝｹﾝｽｳ  = " READ-CNT " ##" UPON CONS.
     DISPLAY "## ｻｸｼﾞｮｹﾝｽｳ  = " DEL-CNT  " ##" UPON CONS.
     DISPLAY "## ﾂｲｶ  ｹｹﾝｽｳ = " ADD-CNT  " ##" UPON CONS.
*
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     SHTDENL1  JHTDENF.
*
 END-EXIT.
     EXIT.
****************************************************************
*           受信手書伝票データ読込み                           *
****************************************************************
 JHTDENF-READ-SEC      SECTION.
*
     MOVE     "JHTDENF-READ-SEC" TO   S-NAME.
*
     READ   JHTDENF  AT  END
            MOVE         9       TO   END-FG
            NOT  AT  END
            ADD          1       TO   READ-CNT
     END-READ.
*
 JHTDEN-READ-EXIT.
     EXIT.
****************************************************************
*           売上伝票データスタート                             *
****************************************************************
 SHTDENF-START-SEC     SECTION.
*
     MOVE     "SHTDENF-START-SEC " TO S-NAME.
*
     MOVE     SDE-F01     TO      DEN-F01.
     MOVE     SDE-F02     TO      DEN-F02.
     MOVE     SDE-F04     TO      DEN-F04.
     MOVE     SDE-F051    TO      DEN-F051.
     MOVE     SDE-F03     TO      DEN-F03.
     START    SHTDENL1  KEY  IS  >= DEN-F01  DEN-F02  DEN-F04
                                    DEN-F051 DEN-F03
              INVALID
              GO                  TO   SHTDENF-START-EXIT
     END-START.
*
 START001.
*
     READ   SHTDENL1  NEXT  AT  END
            GO               TO   SHTDENF-START-EXIT
     END-READ.
*
     IF      SDE-F01  =  DEN-F01
     AND     SDE-F02  =  DEN-F02
             DELETE  SHTDENL1
             ADD     1      TO   DEL-CNT
             GO             TO   START001
     END-IF.
*
 SHTDENF-START-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
