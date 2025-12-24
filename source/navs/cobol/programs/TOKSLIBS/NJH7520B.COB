****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　オンラインデータ作成　　　　　　　*
*    作成日／更新日　　　：　15/12/01                          *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　（ＣＶＣＳ）オンラインデータから　*
*                            変換伝票データファイルを作成する　*
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*    複数仕入先対応　ＤＣＭサンワ植物（受領）：３９２４　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NJH7520B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/12/01.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   CVCSG001  ASSIGN    TO        DA-01-S-CVCSG001
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DEN-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*店舗マスタ
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52  TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*ＤＣＭサンワ発注基本情報
     SELECT   SWJYURF   ASSIGN    TO        DA-01-VI-SWJYURL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYR-F01  JYR-F02
                                            JYR-F03  JYR-F04
                                            JYR-F06  JYR-F06
                                            JYR-F07
                        FILE  STATUS   IS   JYR-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　１２８　  ＢＦ＝　１
******************************************************************
 FD  CVCSG001
                        BLOCK CONTAINS      13   RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
     03  DEN-01                  OCCURS  2.
         05  DEN-01A             PIC  X(01).
         05  DEN-01B             PIC  X(127).
     03  DEN-02                  PIC  X(44).
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
*    ＤＣＭサンワ発注基本情報
******************************************************************
 FD  SWJYURF
                        LABEL RECORD   IS   STANDARD.
     COPY     SWJYURF   OF        XFDLIB
              JOINING   JYR  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*受信データ格納
     COPY   SWJYURF  OF XFDLIB  JOINING   JWW  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  IDY                     PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  JYR-CNT                 PIC  9(08)     VALUE  ZERO.
 01  SONZAI-CNT              PIC  9(08)     VALUE  ZERO.
 01  HTENMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  SWJYURF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  CHK-FLG                 PIC  X(01)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  JYR-STATUS        PIC  X(02).
*明細行退避
 01  WK-MEISAI-BODY.
     03  MEISAI-BODY       OCCURS  6.
         05  MEISAI01      PIC  X(06).
         05  MEISAI02      PIC  9(08).
         05  MEISAI03      PIC  9(02).
         05  MEISAI04      PIC  X(112).
 01  WK-OPTION-BODY.
     03  OPTION-400        PIC  X(128).
     03  OPTION-500        PIC  X(128).
     03  OPTION-BODY       OCCURS  6.
         05  OPTION-600    PIC  X(128).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH7520B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH7520B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH7520B".
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
                        PROCEDURE   CVCSG001.
     MOVE      "CVCSG001"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SWJYURF.
     MOVE      "SWJYURL1"   TO   AB-FILE.
     MOVE      JYR-STATUS   TO   AB-STS.
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
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"         TO        S-NAME.
*
     OPEN     INPUT     CVCSG001  TENMS1.
     OPEN     I-O       SWJYURF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        JWW-REC   CHK-FLG.
     INITIALIZE                   JWW-REC.
 INIT-040.
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
         MOVE    ZERO             TO   SYS-DATEW
     END-IF.
 INIT-050.
*
     READ     CVCSG001
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
     MOVE    "MAIN-SEC"          TO        S-NAME.
*
     PERFORM  VARYING   IDX      FROM      1  BY  1
              UNTIL     IDX      >        2
*
         IF    DEN-01A(IDX)    =    "A"
               MOVE      SPACE       TO    JWW-F100
               INITIALIZE                  JWW-F100
               MOVE      DEN-01(IDX) TO    JWW-F100
          END-IF
*伝票ヘッダ処理
         IF    DEN-01A(IDX)    =    "B"
               IF  CHK-FLG  =  "1"
                   PERFORM  EDIT2-SEC
               END-IF
               MOVE      "1"         TO    CHK-FLG
               MOVE      SPACE       TO    JWW-F200
               MOVE      SPACE       TO    JWW-F300
               MOVE      SPACE       TO    JWW-F400
               MOVE      SPACE       TO    JWW-F500
               MOVE      SPACE       TO    JWW-F600
               INITIALIZE                  JWW-F200
               INITIALIZE                  JWW-F300
               INITIALIZE                  JWW-F400
               INITIALIZE                  JWW-F500
               INITIALIZE                  JWW-F600
               MOVE      SPACE       TO    WK-MEISAI-BODY
               INITIALIZE                  WK-MEISAI-BODY
               MOVE      SPACE       TO    WK-OPTION-BODY
               INITIALIZE                  WK-OPTION-BODY
               MOVE      SPACE       TO    JWW-F200
               INITIALIZE                  JWW-F200
               MOVE      DEN-01(IDX) TO    JWW-F200
          END-IF
*明細行
          IF   DEN-01A(IDX)    =    "D"
               MOVE      DEN-01(IDX) TO    JWW-F300
               MOVE      DEN-01(IDX) TO    MEISAI-BODY(JWW-F305)
          END-IF
*明細フッダ
         IF    DEN-01A(IDX)    =    "E"
               MOVE      SPACE       TO    JWW-F400
               INITIALIZE                  JWW-F400
               MOVE      DEN-01(IDX) TO    JWW-F400
               MOVE      DEN-01(IDX) TO    OPTION-400
         END-IF
*明細ヘッダ漢字
         IF    DEN-01A(IDX)    =    "F"
               MOVE      SPACE       TO    JWW-F500
               INITIALIZE                  JWW-F500
               MOVE      DEN-01(IDX) TO    JWW-F500
               MOVE      DEN-01(IDX) TO    OPTION-500
         END-IF
*明細行漢字
         IF    DEN-01A(IDX)    =    "G"
               MOVE      SPACE       TO    JWW-F600
               INITIALIZE                  JWW-F600
               MOVE      DEN-01(IDX) TO    JWW-F600
               MOVE      DEN-01(IDX) TO    OPTION-600(JWW-F604)
         END-IF
     END-PERFORM.
*
     READ   CVCSG001
         AT END
            MOVE     9           TO    END-FG
         NOT AT END
            ADD      1           TO    RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT2-SEC             SECTION.
*
     MOVE    "EDIT2-SEC"         TO        S-NAME.
*
     PERFORM  EDIT-SEC.
*
     PERFORM VARYING IDY FROM 1 BY 1 UNTIL IDY           > 6
                                     OR    MEISAI01(IDY) = SPACE
             MOVE  JWW-REC          TO  JYR-REC
             MOVE  MEISAI-BODY(IDY) TO  JYR-F300
             MOVE  JYR-F305         TO  JYR-F07
             MOVE  OPTION-400       TO  JYR-F400
             MOVE  OPTION-500       TO  JYR-F500
             MOVE  OPTION-600(IDY)  TO  JYR-F600
             PERFORM  SWJYURF-READ-SEC
             IF  SWJYURF-INV-FLG = "INV"
                 WRITE JYR-REC
                 ADD   1            TO  JYR-CNT
             ELSE
                 ADD   1            TO  SONZAI-CNT
             END-IF
     END-PERFORM.
*
 EDIT2-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"          TO        S-NAME.
*受信日
     MOVE    PARA-JDATE          TO        JWW-F01.
*受信時刻
     MOVE    PARA-JTIME          TO        JWW-F02.
*取引先ＣＤセット
     MOVE        SPACE           TO        TEN-REC.
     INITIALIZE                            TEN-REC.
     MOVE        JWW-F203        TO        TEN-F52.
     MOVE        JWW-F204        TO        TEN-F011.
     READ    TENMS1
       INVALID     MOVE  "INV"   TO        HTENMS-INV-FLG
       NOT INVALID MOVE  SPACE   TO        HTENMS-INV-FLG
     END-READ.
*計上取引先判定
     IF  HTENMS-INV-FLG = SPACE
         IF      TEN-F73 = SPACE
                 MOVE     3924   TO        JWW-F03
         ELSE
                 MOVE     39241  TO        JWW-F03
         END-IF
     ELSE
         MOVE     3924           TO        JWW-F03
     END-IF.
*店舗ＣＤ
     MOVE    JWW-F204            TO        JWW-F04.
*検収日
     MOVE    JWW-F209            TO        JWW-F05.
*納品伝票番号
     MOVE    JWW-F205            TO        JWW-F06.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"          TO        S-NAME.
*
     IF  CHK-FLG  >  ZERO
         PERFORM  EDIT2-SEC
     END-IF.
*
     MOVE      RD-CNT            TO        IN-CNT.
     MOVE      JYR-CNT           TO        OUT-CNT.
     DISPLAY   MSG-IN            UPON      CONS.
     DISPLAY   MSG-OUT           UPON      CONS.
     DISPLAY   MSG-END           UPON      CONS.
*
     CLOSE     CVCSG001  SWJYURF  TENMS1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＤＣＭサンワ受領ファイル検索　　　　　　　　　　*
****************************************************************
 SWJYURF-READ-SEC      SECTION.
*
     MOVE     "SWJYURF-READ-SEC" TO        S-NAME.
*
     READ  SWJYURF
           INVALID
           MOVE   "INV"          TO        SWJYURF-INV-FLG
           NOT  INVALID
           MOVE   SPACE          TO        SWJYURF-INV-FLG
    END-READ.
*
 SWJYURF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
