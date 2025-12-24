****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　新規取引先オンライン（リック）　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　受領データ編集２　　　　　　　　　*
*    作成日／更新日　　　：　08/04/14                          *
*    作成者／更新者　　　：　ＮＡＶ　佐藤　　　　　　　　　　　*
*    処理概要　　　　　　：　受領データを読み、共通仕様の受領　*
*                            データに出力する　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJH8925B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/04/15.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受領データ
     SELECT   RCJRYOF   ASSIGN    TO        DA-01-VI-RCJRYOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JRY-K01   JRY-K02
                                            JRY-K03   JRY-K04
                                            JRY-K05   JRY-K06
                                            JRY-K07   JRY-K08
                        FILE  STATUS   IS   JRY-STATUS.
*共通受領データ
     SELECT   DCMJYUF   ASSIGN    TO        DA-01-VI-DCMJYUL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DCJ-F00   DCJ-F01
                                            DCJ-F02   DCJ-F03
                                            DCJ-F04
                                            DCJ-F06   DCJ-F07
                        FILE  STATUS   IS   DCJ-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受領データ
******************************************************************
 FD  RCJRYOF            LABEL RECORD   IS   STANDARD.
     COPY     RCJRYOF   OF        XFDLIB
              JOINING   JRY  AS   PREFIX.
******************************************************************
*    共通受領データ
******************************************************************
 FD  DCMJYUF            LABEL RECORD   IS   STANDARD.
     COPY     DCMJYUF   OF        XFDLIB
              JOINING   DCJ  AS   PREFIX.
******************************************************************
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D             PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU               PIC  9(08)     VALUE  ZERO.
 01  CNT-GYO                 PIC  9(02)     VALUE  ZERO.
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
*
*
 01  WK-AREA.
     03  WK-GYO            PIC 9(02).
     03  WK-DENNO          PIC 9(08).
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DCJ-STATUS        PIC  X(02).
     03  JRY-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJH8925B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH8925B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH8925B".
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
 01  PARA-AREA.
     03  PARA-JDATE         PIC   9(08).
     03  PARA-JTIME         PIC   9(04).
     03  PARA-KSYU          PIC   X(01).
     03  PARA-YUSEN         PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-AREA.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RCJRYOF.
     MOVE      "RCJRYOL1"   TO   AB-FILE.
     MOVE      JRY-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DCMJYUF.
     MOVE      "DCMJYUL1"   TO   AB-FILE.
     MOVE      DCJ-STATUS   TO   AB-STS.
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
     INITIALIZE                   WK-AREA.
     OPEN     INPUT     RCJRYOF
     OPEN     OUTPUT    DCMJYUF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
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
     READ     RCJRYOF
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
     PERFORM     DCMJYUF-TENSO-SEC.
*
     READ     RCJRYOF
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　共通受領データ出力　　　                          *
****************************************************************
 DCMJYUF-TENSO-SEC             SECTION.
*
     MOVE    "DCMJYUF-TENSO-SEC"   TO        S-NAME.
*取引先ＣＤ
     MOVE     JRY-F15          TO        DCJ-F00.
*受信日
     MOVE     ZERO             TO        DCJ-F01.
*発注日
     MOVE     ZERO             TO        DCJ-F02.
*納品日
     MOVE     JRY-F04          TO        DCJ-F03.
*店舗ＣＤ
     MOVE     JRY-F14          TO        DCJ-F04.
*店名
     MOVE     JRY-F18          TO        DCJ-F05.
*伝票番号　　
     MOVE     JRY-F02          TO        DCJ-F06.
*行番号
     MOVE     JRY-M03          TO        DCJ-F07.
*伝票区分
     MOVE     JRY-F06          TO        DCJ-F08.
*商品ＣＤ
     MOVE     ZERO             TO        DCJ-F09.
*商品名1
     MOVE    JRY-M04           TO        DCJ-F101.
*商品名2
     MOVE    JRY-M06           TO        DCJ-F102.
*発注数
     MOVE    JRY-M10           TO        DCJ-F11.
*検収数
     MOVE    JRY-M12           TO        DCJ-F12.
*原単価
     MOVE    JRY-M16           TO        DCJ-F13.
*原価金額
     COMPUTE DCJ-F14 = JRY-M12 * JRY-M16.
*ＪＡＮＣＤ
     MOVE    JRY-M02           TO        DCJ-F15.
     WRITE   DCJ-REC.
     ADD     1             TO        WRT-CNT .
 RCJRY-TENSO-EXIT.
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
     CLOSE     DCMJYUF   RCJRYOF
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
