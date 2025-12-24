# NVS0220B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVS0220B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　（株）サカタのタネ殿　　　　　　　　　*
*    サブシステム　　：　Ｄ３６５連携　　　　　　　　　　　　　*
*    モジュール名　　：　Ｄ３６５計上エラー復旧　　　　　　　　*
*    作成日　　　　　：　2021/06/10                            *
*    作成者　　　　　：　INOUE                                 *
*    処理概要　　　　：　受け取ったパラメタに合致する　　　　　*
*                        売上伝票レコードの　　　　　　　　　　*
*                        項目をクリアする　　　　　　　　　　　*
*    更新日　　　　　：　                                      *
*    更新者　　　　　：　                                      *
*    更新概要　　　　：　                                      *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NVS0220B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/06/10.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENL1  ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F01
                                            DEN-F02
                                            DEN-F04
                                            DEN-F051
                                            DEN-F07
                                            DEN-F112
                                            DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*ＳＵＢ商品変換テーブル
     SELECT   SUBTBLF   ASSIGN    TO        DA-01-VI-SUBTBLL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       STB-F01   STB-F02
                        FILE STATUS    IS   STB-STATUS.
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
*    ＳＵＢ商品変換テーブル
******************************************************************
 FD  SUBTBLF            LABEL RECORD   IS   STANDARD.
     COPY     SUBTBLF   OF        XFDLIB
              JOINING   STB       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  UPD-CNT                 PIC  9(08)     VALUE  ZERO.
 01  IXA                     PIC  9(02)     VALUE  ZERO.
*
 01  WK-PARA-IN-AREA.
     03  WK-PARA             OCCURS  12.
       05  WK-SDENNO         PIC  9(09).
       05  WK-EDENNO         PIC  9(09).
 01  FLG-AREA.
     03  SHTDENL1-END        PIC  X(03)     VALUE SPACE.
     03  SHTDENL1-INV        PIC  X(03)     VALUE SPACE.
     03  SUBTBLF-INV-FLG     PIC  X(03)     VALUE SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE            PIC 9(06).
     03  SYS-DATEW           PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  STB-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NVS0220B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVS0220B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVS0220B".
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
         05  FILLER         PIC   X(09)  VALUE " UPDATE= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 LINKAGE                SECTION.
 01  PARA-TOKCD             PIC   9(08).
 01  PARA-DENNO             PIC   9(09).
 01  PARA-SOUSAI            PIC   9(01).
 01  PARA-DENKU             PIC   9(02).
 01  PARA-TENPO             PIC   9(05).
 01  PARA-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE             DIVISION    USING
                       PARA-TOKCD
                       PARA-DENNO
                       PARA-SOUSAI
                       PARA-DENKU
                       PARA-TENPO
                       PARA-NOUDT.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
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
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SUBTBLF.
     MOVE      "SUBTBLL1"   TO   AB-FILE.
     MOVE      STB-STATUS   TO   AB-STS.
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
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SUBTBLF.
     OPEN     I-O       SHTDENL1.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    UPD-CNT.
     MOVE     ZERO      TO        IN-CNT    UPD-CNT.
*
     MOVE     SPACE           TO  DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     PARA-TOKCD      TO  DEN-F01.
     MOVE     PARA-DENNO      TO  DEN-F02.
     MOVE     PARA-SOUSAI     TO  DEN-F04.
     MOVE     PARA-DENKU      TO  DEN-F051.
     MOVE     PARA-TENPO      TO  DEN-F07.
     MOVE     PARA-NOUDT      TO  DEN-F112.
     PERFORM  SHTDENL1-START-SEC.
     IF       SHTDENL1-END  =  "END"
              DISPLAY NC"売上計上ファイル対象なし１" UPON CONS
              MOVE    9     TO    END-FG
              GO            TO    INIT-EXIT
     END-IF.

     PERFORM  SHTDENL1-READ-SEC.
     IF       SHTDENL1-END  =  "END"
              DISPLAY NC"売上計上ファイル対象なし２" UPON CONS
              MOVE    9     TO    END-FG
              GO            TO    INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
 MAIN-01.
     MOVE      0              TO   DEN-F277.
     MOVE     "00000000"      TO   DEN-F45.
     MOVE      9              TO   DEN-F27C.
*    ＳＵＢ商品変換ＴＢＬ読込
     PERFORM   SUBTBLF-READ-SEC.
     IF  SUBTBLF-INV-FLG =  "INV"
         MOVE  SPACE          TO   DEN-F32
     ELSE
         MOVE  STB-F19        TO   DEN-F32
     END-IF.
     REWRITE                       DEN-REC.
     ADD       1              TO   UPD-CNT.
*
 MAIN-99.
*
     PERFORM  SHTDENL1-READ-SEC.
     IF       SHTDENL1-END  =  "END"
              MOVE    9     TO    END-FG
              GO            TO    MAIN-EXIT
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上計上ファイルＳＴＡＲＴ　　　　　　　　　　　*
****************************************************************
 SHTDENL1-START-SEC       SECTION.
*
     MOVE     "SHTDENL1-START-SEC"  TO      S-NAME.
*
     START SHTDENL1  KEY >= DEN-F01
                            DEN-F02
                            DEN-F04
                            DEN-F051
                            DEN-F07
                            DEN-F112
                            DEN-F03
         INVALID
              MOVE   "END"      TO    SHTDENL1-END
     END-START.
*
 SHTDENL1-START-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上計上ファイル順ＲＥＡＤ　　　　　　　　　　　*
****************************************************************
 SHTDENL1-READ-SEC       SECTION.
*
     MOVE     "SHTDENL1-READ-SEC"  TO      S-NAME.
*
*売上計上ファイル順ＲＥＡＤ
     READ     SHTDENL1
         AT END
              MOVE   "END"      TO    SHTDENL1-END
              GO                TO    SHTDENL1-READ-EXIT
         NOT AT END
              ADD      1              TO    RD-CNT
     END-READ.
*
**** DISPLAY "DEN-F01  = " DEN-F01   " - " PARA-TOKCD  UPON CONS.
*    DISPLAY "DEN-F02  = " DEN-F02   " - " PARA-DENNO  UPON CONS.
*    DISPLAY "DEN-F04  = " DEN-F04   " - " PARA-SOUSAI UPON CONS.
*    DISPLAY "DEN-F051 = " DEN-F051  " - " PARA-DENKU  UPON CONS.
*    DISPLAY "DEN-F07  = " DEN-F07   " - " PARA-TENPO  UPON CONS.
**** DISPLAY "DEN-F112 = " DEN-F112  " - " PARA-NOUDT  UPON CONS.
     IF ( DEN-F01   =  PARA-TOKCD    ) AND
        ( DEN-F02   =  PARA-DENNO    ) AND
        ( DEN-F04   =  PARA-SOUSAI   ) AND
        ( DEN-F051  =  PARA-DENKU    ) AND
        ( DEN-F07   =  PARA-TENPO    ) AND
        ( DEN-F112  =  PARA-NOUDT    )
          CONTINUE
     ELSE
          MOVE   "END"      TO    SHTDENL1-END
     END-IF.
*
 SHTDENL1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＳＵＢ商品変換ＴＢＬ読込　　　　　　　　　　　　*
****************************************************************
 SUBTBLF-READ-SEC        SECTION.
*
     MOVE     "SUBTBLF-READ-SEC"   TO      S-NAME.
*
     MOVE      DEN-F01             TO      STB-F01.
     MOVE      DEN-F25             TO      STB-F02.
     READ      SUBTBLF
               INVALID     MOVE  "INV"   TO  SUBTBLF-INV-FLG
               NOT INVALID MOVE  SPACE   TO  SUBTBLF-INV-FLG
     END-READ.
*
 SUBTBLF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      UPD-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     SUBTBLF  SHTDENL1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
