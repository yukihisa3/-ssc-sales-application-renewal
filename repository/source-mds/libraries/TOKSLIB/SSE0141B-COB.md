# SSE0141B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSE0141B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　配信ファイル作成　　　　　　　　　*
*    作成日／更新日　　　：　92/12/05                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　抽出した請求合計ファイルより配信　*
*                        ：　ファイルを出力する。　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE0141B.
 AUTHOR.                S.K  SANKYO.
 DATE-WRITTEN.          92/12/05.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< ワークファイル >>--*
     SELECT   WORK01    ASSIGN         DA-01-S-WORK01
                        ORGANIZATION   SEQUENTIAL
                        STATUS         WORK-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< 配信データ >>--*
     SELECT   CVCSKHAI  ASSIGN         DA-01-S-CVCSKHAI
                        ORGANIZATION   SEQUENTIAL
                        STATUS    CVCSKHAI-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< ワークファイル >>--*
 FD  WORK01             LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 配信データ >>--*
 FD  CVCSKHAI           LABEL     RECORD   IS   STANDARD.
 01  ONL-REC.
     03  ONL-RECX       OCCURS    2.
         05  ONL-F.
             07  ONL-F01     PIC  X(01).
             07  ONL-F02     PIC  X(02).
             07  FILLER      PIC  X(12).
             07  ONL-F03     PIC  9(06).
             07  ONL-F041    PIC  9(06).
             07  ONL-F042    PIC  X(02).
             07  ONL-F051    PIC  X(06).
             07  ONL-F052    PIC  X(02).
             07  ONL-F06     PIC  X(08).
             07  FILLER      PIC  X(83).
         05  ONL-H           REDEFINES ONL-F.
             07  ONL-H01     PIC  X(01).
             07  ONL-H02     PIC  X(02).
             07  ONL-H03     PIC  9(04).
             07  ONL-H04     PIC  X(20).
             07  ONL-H05     PIC  9(04).
             07  FILLER      PIC  X(97).
         05  FILLER          REDEFINES ONL-F.
             07  ONL-M01     PIC  X(01).
             07  ONL-M02     PIC  X(02).
             07  ONL-M       OCCURS    4.
                 09  ONL-M03      PIC  X(01).
                 09  ONL-M04      PIC  9(05).
                 09  ONL-M05      PIC  9(06).
                 09  ONL-M06      PIC  X(09).
                 09  ONL-M07      PIC -9(09).
             07  FILLER      PIC  X(01).
         05  ONL-G           REDEFINES ONL-F.
             07  ONL-G01     PIC  X(01).
             07  ONL-G02     PIC  X(02).
             07  ONL-G03     PIC  9(06).
             07  ONL-G04     PIC -9(10).
             07  ONL-GF      PIC  X(108).
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
     03  DATA-CNT       PIC  9(04)   VALUE  ZERO.
 01  IDX.
     03  I              PIC  9(03).
     03  J              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  TOK-ST             PIC  X(02).
 01  WORK-ST            PIC  X(02).
 01  CVCSKHAI-ST        PIC  X(02).
*
*----<< ｼｭｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  KEI-KEN        PIC S9(11)     PACKED-DECIMAL.
     03  KEI-KIN        PIC S9(11)     PACKED-DECIMAL.
     03  KEI-KEN-B      PIC S9(11)     PACKED-DECIMAL.
     03  KEI-KIN-B      PIC S9(11)     PACKED-DECIMAL.
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(47)  VALUE  SPACE.
         05  FILLER     PIC  N(12)  VALUE
                        NC"【　配信データリスト　】".
         05  FILLER     PIC  X(27)  VALUE  SPACE.
         05  FILLER     PIC  X(05)  VALUE  "DATE:".
         05  HD-011     PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  HD-013     PIC  Z9.
 01  HEAD02.
         05  FILLER     PIC  X(110) VALUE  SPACE.
         05  FILLER     PIC  X(05)  VALUE  "PAGE:".
         05  HD-02      PIC  ZZZ9.
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(10)  VALUE  SPACE.
         05  FILLER     PIC  N(03)  VALUE  NC"配信先".
         05  FILLER     PIC  X(35)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"伝票枚数".
         05  FILLER     PIC  X(08)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"請求金額".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(09).
         05  ME-03      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-04      PIC  N(16).
         05  FILLER     PIC  X(02).
         05  ME-05      PIC  ZZZ,ZZ9.
         05  FILLER     PIC  X(02).
         05  ME-06      PIC  --,---,---,--9.
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ワークファイル >>--*
 WORK01-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      WORK01.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE0141B WORK01 ERROR " WORK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    WORK01    HTOKMS    CVCSKHAI  PRTF.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE0141B HTOKMS ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
***  CLOSE    WORK01    HTOKMS    CVCSKHAI  PRTF.
     STOP     RUN.
*----<< 配信データ >>--*
 CVCSKHAI-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CVCSKHAI.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE0141B CVCSKHAI ERROR " CVCSKHAI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    WORK01    HTOKMS    CVCSKHAI  PRTF.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     IF       END-FLG   =    0
              PERFORM   200-MAIN-RTN
     END-IF.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE0141B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     WORK01.
     OPEN     INPUT     HTOKMS.
     OPEN     OUTPUT    CVCSKHAI.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     INITIALIZE         IDX.
     INITIALIZE         GOKEI-AREA.
     MOVE     99             TO   LINE-CNT.
*
     PERFORM  900-SEI-READ.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*---<< ﾌｧｲﾙ ﾍｯﾀﾞｰ >>---*
     MOVE     SPACE          TO   ONL-REC.
*    MOVE     "A"            TO   ONL-F01 (01).
*    MOVE     "11"           TO   ONL-F02 (01).
*    MOVE     SYS-DATE       TO   ONL-F03 (01).
*    MOVE     173            TO   ONL-F041(01).
*    MOVE     "00"           TO   ONL-F042(01).
*    MOVE     "499900"       TO   ONL-F051(01).
*    MOVE     SPACE          TO   ONL-F052(01).
*    MOVE     "00001500"     TO   ONL-F06 (01).
*---<< ﾒｲｻｲ ﾍｯﾀﾞｰ >>---*
     MOVE     ZERO           TO   J.
     MOVE     "B"            TO   ONL-H01 (01).
     MOVE     "11"           TO   ONL-H02 (01).
     MOVE     173            TO   ONL-H03 (01).
     MOVE     "ｶﾌﾞｼｷｶﾞｲｼｬ ｻｶﾀﾉﾀﾈ"
                             TO   ONL-H04 (01).
     MOVE     SEI-F02(1:4)   TO   ONL-H05 (01).
*****WRITE    ONL-REC.
*****ADD      1              TO   OUT-CNT.
*
*---<< ﾒｲｻｲ ｼｭﾂﾘｮｸ >>---*
     MOVE     2              TO   I.
     PERFORM  210-SEI-OUT    UNTIL     END-FLG   =    1
                                OR     DATA-CNT  >=   9000.
*
*---<< ｺﾞｳｹｲ ｼｭﾂﾘｮｸ >>---*
     ADD      1              TO   I.
     IF       I    >    2
     AND      END-FLG   =  1
              WRITE     ONL-REC
              ADD       1         TO   OUT-CNT
              MOVE      SPACE     TO   ONL-REC
              MOVE      1         TO   I
     ELSE
              MOVE      SPACE     TO   ONL-REC
              MOVE      1         TO   I
     END-IF.
     MOVE     "D"            TO   ONL-G01 (I).
     MOVE     "11"           TO   ONL-G02 (I).
     MOVE     KEI-KEN        TO   ONL-G03 (I).
     MOVE     KEI-KIN        TO   ONL-G04 (I).
     MOVE     SPACE          TO   ONL-GF  (I).
     WRITE    ONL-REC.
     ADD      1              TO   OUT-CNT.
     ADD      KEI-KEN        TO   KEI-KEN-B.
     ADD      KEI-KIN        TO   KEI-KIN-B.
     DISPLAY "KEI-KEN = " KEI-KEN UPON CONS.
     DISPLAY "KEI-KIN = " KEI-KIN UPON CONS.
     MOVE     ZERO           TO   KEI-KEN KEI-KIN DATA-CNT.
*
     IF       END-FLG  NOT =  1
              GO             TO   200-MAIN-RTN
     END-IF.
*
*---<< ｺﾞｳｹｲ ｲﾝｻﾂ >>---*
     PERFORM  220-PRINT.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    WORK01.
     CLOSE    HTOKMS.
     CLOSE    CVCSKHAI.
     CLOSE    PRTF.
*
     DISPLAY "+++ SSE0141B INPUT =" IN-CNT  " +++" UPON CONS.
     DISPLAY "+++ SSE0141B OUTPUT=" OUT-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE0141B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS        UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      ｾｲｷｭｳ ﾃﾞｰﾀ ｼｮﾘ                              *
*--------------------------------------------------------------*
 210-SEI-OUT            SECTION.
     ADD      1              TO   J.
     IF       J         >    4
              ADD       1         TO   I
              MOVE      1         TO   J
              IF        I    >    2
                        WRITE     ONL-REC
                        ADD       1         TO   OUT-CNT
                        ADD       2         TO   DATA-CNT
                        IF        DATA-CNT  >=  9000
                                  GO        TO  210-SEI-OUT-EXIT
                        END-IF
                        MOVE      SPACE     TO   ONL-REC
                        MOVE      1         TO   I
              END-IF
     END-IF.
*
*---<< ﾒｲｻｲ ｾｯﾄ >>---*
     MOVE     "C"            TO   ONL-M01 (I).
     MOVE     "11"           TO   ONL-M02 (I).
     MOVE     "5"            TO   ONL-M03 (I J).
     MOVE     SEI-F03        TO   ONL-M04 (I J).
     MOVE     SEI-F04        TO   ONL-M05 (I J).
     MOVE     SEI-F05        TO   ONL-M06 (I J).
     MOVE     SEI-F06        TO   ONL-M07 (I J).
     ADD      1              TO   KEI-KEN.
     ADD      SEI-F06        TO   KEI-KIN.
*****ADD      1              TO   DATA-CNT.
*
     PERFORM  900-SEI-READ.
 210-SEI-OUT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 220-PRINT              SECTION.
     IF       LINE-CNT  >    63
              PERFORM   221-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
*****MOVE     SEI-F01        TO   ME-03.
*****MOVE     SEI-F01        TO   TOK-F01.
     MOVE     173            TO   ME-03.
     MOVE     173            TO   TOK-F01.
     PERFORM  900-TOK-READ.
     MOVE     TOK-F03        TO   ME-04.
     MOVE     KEI-KEN-B      TO   ME-05.
     MOVE     KEI-KIN-B      TO   ME-06.
*
     WRITE    PRT-REC   FROM MEIS01    AFTER     1.
     ADD      1         TO   LINE-CNT.
 220-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 221-HEAD-PRINT         SECTION.
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*
     ADD      1              TO   PAGE-CNT.
     MOVE     SYS-YY         TO   HD-011.
     MOVE     SYS-MM         TO   HD-012.
     MOVE     SYS-DD         TO   HD-013.
     MOVE     PAGE-CNT       TO   HD-02.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     2.
     WRITE    PRT-REC   FROM      P-SPACE   AFTER     1.
     MOVE     7              TO   LINE-CNT.
 221-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
     READ     WORK01    AT   END
              MOVE      1         TO   END-FLG
              GO   TO   900-SEI-READ-EXIT
     END-READ.
*
     ADD      1              TO   IN-CNT.
 900-SEI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
