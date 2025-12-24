****************************************************************
*                                                              *
*    顧客名　　　　　：　（株）サカタのタネ殿　　　　　　　　　*
*    サブシステム　　：　Ｄ３６５連携　　　　　　　　　　　　　*
*    モジュール名　　：　連携店舗変更更新（手書）　　　　　　　*
*    作成日　　　　　：　2021/06/10                            *
*    作成者　　　　　：　INOUE                                 *
*    処理概要　　　　：　受け取ったパラメタに合致する　　　　　*
*                        売上伝票レコードに　　　　　　　　　　*
*                        需要家ＩＤをセットする。　　　　　　　*
*    更新日　　　　　：　                                      *
*    更新者　　　　　：　                                      *
*    更新概要　　　　：　                                      *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            TSY0804B.
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
     SELECT   SHTDENLC  ASSIGN    TO        DA-01-VI-SHTDENLC
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F277  DEN-F274
                                            DEN-F09   DEN-F01
                                            DEN-F02   DEN-F04
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.

*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENLC
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
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
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE            PIC 9(06).
     03  SYS-DATEW           PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  SDE-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  SHO-STATUS        PIC  X(02).
     03  SH1-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "TSY0804B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "TSY0804B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "TSY0804B".
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
 01  PARA-IN-BTORI          PIC   9(08).
 01  PARA-IN-CHGTEN         PIC   9(05).
 01  PARA-IN-CHGJYK         PIC   X(10).
 01  PARA-IN-SDENNO01       PIC   9(09).
 01  PARA-IN-EDENNO01       PIC   9(09).
 01  PARA-IN-SDENNO02       PIC   9(09).
 01  PARA-IN-EDENNO02       PIC   9(09).
 01  PARA-IN-SDENNO03       PIC   9(09).
 01  PARA-IN-EDENNO03       PIC   9(09).
 01  PARA-IN-SDENNO04       PIC   9(09).
 01  PARA-IN-EDENNO04       PIC   9(09).
 01  PARA-IN-SDENNO05       PIC   9(09).
 01  PARA-IN-EDENNO05       PIC   9(09).
 01  PARA-IN-SDENNO06       PIC   9(09).
 01  PARA-IN-EDENNO06       PIC   9(09).
 01  PARA-IN-SDENNO07       PIC   9(09).
 01  PARA-IN-EDENNO07       PIC   9(09).
 01  PARA-IN-SDENNO08       PIC   9(09).
 01  PARA-IN-EDENNO08       PIC   9(09).
 01  PARA-IN-SDENNO09       PIC   9(09).
 01  PARA-IN-EDENNO09       PIC   9(09).
 01  PARA-IN-SDENNO10       PIC   9(09).
 01  PARA-IN-EDENNO10       PIC   9(09).
 01  PARA-IN-SDENNO11       PIC   9(09).
 01  PARA-IN-EDENNO11       PIC   9(09).
 01  PARA-IN-SDENNO12       PIC   9(09).
 01  PARA-IN-EDENNO12          PIC   9(09).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE             DIVISION    USING
                       PARA-IN-BTORI
                       PARA-IN-CHGTEN
                       PARA-IN-CHGJYK
                       PARA-IN-SDENNO01  PARA-IN-EDENNO01
                       PARA-IN-SDENNO02  PARA-IN-EDENNO02
                       PARA-IN-SDENNO03  PARA-IN-EDENNO03
                       PARA-IN-SDENNO04  PARA-IN-EDENNO04
                       PARA-IN-SDENNO05  PARA-IN-EDENNO05
                       PARA-IN-SDENNO06  PARA-IN-EDENNO06
                       PARA-IN-SDENNO07  PARA-IN-EDENNO07
                       PARA-IN-SDENNO08  PARA-IN-EDENNO08
                       PARA-IN-SDENNO09  PARA-IN-EDENNO09
                       PARA-IN-SDENNO10  PARA-IN-EDENNO10
                       PARA-IN-SDENNO11  PARA-IN-EDENNO11
                       PARA-IN-SDENNO12  PARA-IN-EDENNO12.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLC.
     MOVE      "SHTDENLC"   TO   AB-FILE.
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
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       SHTDENLC.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    UPD-CNT.
     MOVE     ZERO      TO        IN-CNT    UPD-CNT.
*
*    パラメタ退避
     MOVE     SPACE               TO   WK-PARA-IN-AREA.
     INITIALIZE                        WK-PARA-IN-AREA.
     MOVE     PARA-IN-SDENNO01       TO   WK-SDENNO (1).
     MOVE     PARA-IN-EDENNO01       TO   WK-EDENNO (1).
     MOVE     PARA-IN-SDENNO02       TO   WK-SDENNO (2).
     MOVE     PARA-IN-EDENNO02       TO   WK-EDENNO (2).
     MOVE     PARA-IN-SDENNO03       TO   WK-SDENNO (3).
     MOVE     PARA-IN-EDENNO03       TO   WK-EDENNO (3).
     MOVE     PARA-IN-SDENNO04       TO   WK-SDENNO (4).
     MOVE     PARA-IN-EDENNO04       TO   WK-EDENNO (4).
     MOVE     PARA-IN-SDENNO05       TO   WK-SDENNO (5).
     MOVE     PARA-IN-EDENNO05       TO   WK-EDENNO (5).
     MOVE     PARA-IN-SDENNO06       TO   WK-SDENNO (6).
     MOVE     PARA-IN-EDENNO06       TO   WK-EDENNO (6).
     MOVE     PARA-IN-SDENNO07       TO   WK-SDENNO (7).
     MOVE     PARA-IN-EDENNO07       TO   WK-EDENNO (7).
     MOVE     PARA-IN-SDENNO08       TO   WK-SDENNO (8).
     MOVE     PARA-IN-EDENNO08       TO   WK-EDENNO (8).
     MOVE     PARA-IN-SDENNO09       TO   WK-SDENNO (9).
     MOVE     PARA-IN-EDENNO09       TO   WK-EDENNO (9).
     MOVE     PARA-IN-SDENNO10       TO   WK-SDENNO(10).
     MOVE     PARA-IN-EDENNO10       TO   WK-EDENNO(10).
     MOVE     PARA-IN-SDENNO11       TO   WK-SDENNO(11).
     MOVE     PARA-IN-EDENNO11       TO   WK-EDENNO(11).
     MOVE     PARA-IN-SDENNO12       TO   WK-SDENNO(12).
     MOVE     PARA-IN-EDENNO12       TO   WK-EDENNO(12).
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
     PERFORM  VARYING   IXA  FROM      1    BY   1
              UNTIL     IXA  >    12
         IF ( WK-SDENNO(IXA)      NOT =     ZERO ) AND
            ( WK-EDENNO(IXA)      NOT =     ZERO )
*
              MOVE      SPACE          TO   DEN-REC
              INITIALIZE                    DEN-REC
              MOVE      0              TO   DEN-F277
              MOVE      0              TO   DEN-F274
              MOVE      PARA-IN-BTORI  TO   DEN-F01
              MOVE      WK-SDENNO(IXA) TO   DEN-F02
              START     SHTDENLC  KEY  >=   DEN-F277  DEN-F274
                                            DEN-F09   DEN-F01
                                            DEN-F02   DEN-F04
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                  INVALID   KEY
                        CONTINUE
                  NOT INVALID
                        READ     SHTDENLC
                            AT END     CONTINUE
                            NOT AT END
                                 ADD       1    TO   RD-CNT
                                 PERFORM   HENSYU-SEC
                        END-READ
              END-START
         END-IF
     END-PERFORM.
*
     MOVE     9    TO   END-FG.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　編集処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 HENSYU-SEC             SECTION.
*
     MOVE    "HENSYU-SEC"  TO    S-NAME.
*
 HENSYU-001.
*売上データ作成区分・オンライン区分
     IF     ( DEN-F277      NOT =   9            ) AND
            ( DEN-F274          =   0            )
              CONTINUE
     ELSE
              GO            TO    HENSYU-EXIT
     END-IF.
*
*HENSYU-002.
*取引先ＣＤ
     IF       DEN-F01           =     PARA-IN-BTORI
              CONTINUE
     ELSE
              GO            TO    HENSYU-999
     END-IF.
*
 HENSYU-004.
*店舗・伝票番号・相殺区分・伝区コード
     IF     ( DEN-F07           =     PARA-IN-CHGTEN ) AND
            ( DEN-F02          >=     WK-SDENNO(IXA) ) AND
            ( DEN-F02          <=     WK-EDENNO(IXA) ) AND
            ( DEN-F04           =     0              ) AND
            ( DEN-F051          =     40             )
*             計上需要家ＩＤ置換
              MOVE     PARA-IN-CHGJYK TO    DEN-D85
              REWRITE  DEN-REC
              ADD      1              TO    UPD-CNT
     END-IF.
*
 HENSYU-999.
*売上伝票ファイル順ＲＥＡＤ
     READ     SHTDENLC
         AT END
              MOVE     9              TO    END-FG
         NOT AT END
              ADD      1              TO    RD-CNT
              GO                      TO    HENSYU-SEC
     END-READ.
*
 HENSYU-EXIT.
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
     CLOSE     SHTDENLC.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
