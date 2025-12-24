# STE0020B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/STE0020B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　手書発注集計表　                  *
*    モジュール名　　　　：　手書発注集計表データ抽出（商品順）*
*    作成日／作成者　　　：　99/11/22  /  HAGIWARA             *
*    処理概要　　　　　　：　受け取った各パラメタより、該当    *
*                            のデータを売上伝票データファイル  *
*                            より抽出する。                    *
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            STE0020B.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          99/11/22.
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
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*出荷明細ワークファイル
     SELECT   TEGHACF   ASSIGN    TO        DA-01-S-TEGHACF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HAC-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    発注集計表データファイル
******************************************************************
 FD  TEGHACF            BLOCK     CONTAINS  5    RECORDS.
     COPY     TEGHACF   OF        XFDLIB
              JOINING   HAC       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  HAC-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "STE0020B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STE0020B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STE0020B".
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
 LINKAGE                SECTION.
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SDENNO            PIC   9(09).
 01  PARA-EDENNO            PIC   9(09).
 01  PARA-OUTNO             PIC   9(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-TORICD
                                       PARA-SDENNO
                                       PARA-EDENNO
                                       PARA-OUTNO.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENF.
     MOVE      "SHTDENF"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGHACF.
     MOVE      "TEGHACF "   TO   AB-FILE.
     MOVE      HAC-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2"     TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
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
     OPEN     INPUT     SHTDENF  TOKMS2.
     OPEN     OUTPUT    TEGHACF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
*    得意先マスタ検索
     MOVE     SPACE          TO   TOK-REC
     INITIALIZE                   TOK-REC
     MOVE     PARA-TORICD    TO   TOK-F01
     READ     TOKMS2
         INVALID
              MOVE SPACE     TO   TOK-REC
              INITIALIZE          TOK-REC
     END-READ.
*
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     PARA-TORICD    TO   DEN-F01.
     MOVE     PARA-SDENNO    TO   DEN-F02.
     MOVE     ZERO           TO   DEN-F04.
     MOVE     ZERO           TO   DEN-F051.
     MOVE     ZERO           TO   DEN-F03.
     START    SHTDENF  KEY   >=   DEN-F01   DEN-F02
                                  DEN-F04   DEN-F051
                                  DEN-F03
         INVALID   KEY
              MOVE      9    TO   END-FG
              GO   TO   INIT-EXIT
         NOT INVALID
***      売上ファイル存在チェック
              PERFORM  DEN-READ-SEC
              IF       END-FG  =  9
                       GO         TO   INIT-EXIT
              END-IF
***           開始伝票_がゼロの時，取引先コードのみチェック
              IF   PARA-SDENNO  = ZERO
                   IF  PARA-TORICD =    DEN-F01
                       CONTINUE
                   ELSE
                       MOVE   9   TO   END-FG
                       GO         TO   INIT-EXIT
                   END-IF
***           ゼロ以外，開始伝票_までチェック
              ELSE
                   IF  PARA-TORICD     =    DEN-F01 AND
                       PARA-SDENNO     =    DEN-F02
                       CONTINUE
                   ELSE
                       MOVE  9    TO   END-FG
                       GO         TO   INIT-EXIT
                   END-IF
              END-IF
     END-START.
*
 INIT-010.
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
     IF     ( PARA-TORICD     =    DEN-F01 ) AND
            ( PARA-EDENNO     >    DEN-F02 )
              CONTINUE
     ELSE
              MOVE      9         TO   END-FG
              GO        TO        MAIN-EXIT
     END-IF.
*
*出荷明細ワーク出力
     MOVE     SPACE          TO   HAC-REC.
     INITIALIZE                   HAC-REC.
     MOVE     DEN-F46        TO   HAC-F011.
     MOVE     DEN-F47        TO   HAC-F012.
     MOVE     DEN-F01        TO   HAC-F013.
     MOVE     DEN-F07        TO   HAC-F02.
     MOVE     DEN-F48        TO   HAC-F03.
     MOVE     DEN-F12        TO   HAC-F04.
     MOVE     DEN-F111       TO   HAC-F05.
     MOVE     DEN-F112       TO   HAC-F06.
     MOVE     DEN-F25        TO   HAC-F07.
     MOVE     DEN-F1411      TO   HAC-F081.
     MOVE     DEN-F1412      TO   HAC-F082.
     MOVE     DEN-F49        TO   HAC-F09.
     MOVE     SPACE          TO   HAC-F10.
     MOVE     DEN-F1421      TO   HAC-F111.
     MOVE     DEN-F1422      TO   HAC-F112.
     MOVE     SPACE          TO   HAC-F12.
     MOVE     DEN-F15        TO   HAC-F13.
     MOVE     DEN-F15        TO   HAC-F14.
     MOVE     DEN-F172       TO   HAC-F15.
     MOVE     DEN-F173       TO   HAC-F16.
*
     MOVE     TOK-F04        TO   HAC-F17.
     WRITE    HAC-REC.
     ADD      1              TO   WRT-CNT.
*
 MAIN-010.
*
     PERFORM  DEN-READ-SEC.
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
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     SHTDENF  TOKMS2   TEGHACF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　手書伝票ファイル読込み　　　　　　　　　　　　　*
****************************************************************
 DEN-READ-SEC  SECTION.
*
     MOVE     "DEN-READ-SEC"  TO      S-NAME.
*
     READ     SHTDENF
              AT END    MOVE      9         TO  END-FG
                        GO                  TO  DEN-READ-EXIT
              NOT AT END
                   ADD  1    TO   RD-CNT
     END-READ.
*
     IF     DEN-F03   >   10
            GO               TO   DEN-READ-SEC
     END-IF.
*
 DEN-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
