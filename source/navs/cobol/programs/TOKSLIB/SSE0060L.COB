****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　請求合計一覧表作成　　　　　　　　*
*    作成日／更新日　　　：　92/12/03                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求合計ファイルより請求合計一覧表*
*                        ：　を印刷する。　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE0060L.
 AUTHOR.                S.K  SANKYO.
 DATE-WRITTEN.          92/12/03.
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
*----<< 請求合計ファイル >>--*
     SELECT   HSEIGKF   ASSIGN         DA-01-VI-SEIGKF1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01   SEI-F05
                        STATUS         SEI-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 請求合計ファイル >>--*
 FD  HSEIGKF            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
 01  IDX.
     03  I              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SEI-ST             PIC  X(02).
 01  TOK-ST             PIC  X(02).
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
 01  WK-DATE            PIC  9(06).
 01  FILLER             REDEFINES      WK-DATE.
     03  WK-YY          PIC  9(02).
     03  WK-MM          PIC  9(02).
     03  WK-DD          PIC  9(02).
*
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  KEI-TBL        OCCURS    2.
         05  KEI-KEN    PIC  S9(12)V99   PACKED-DECIMAL.
         05  KEI-SUU    PIC  S9(12)V99   PACKED-DECIMAL.
         05  KEI-KIN    PIC  S9(12)V99   PACKED-DECIMAL.
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-TOR                        PIC  9(08).
     03  OLD.
         05  OLD-TOR                        PIC  9(08).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(41)  VALUE  SPACE.
         05  FILLER     PIC  N(11)  VALUE
                        NC"【　請求合計一覧表　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(27)  VALUE  SPACE.
         05  FILLER     PIC  N(03)  VALUE  NC"処理日".
         05  FILLER     PIC  X(01)  VALUE  ":".
         05  HD-011     PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  X(03)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"頁".
         05  FILLER     PIC  X(01)  VALUE  ":".
         05  HD-02      PIC  ZZ9.
 01  HEAD02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(30)  VALUE  SPACE.
         05  FILLER     PIC  N(05)  VALUE  NC"取　引　先".
         05  FILLER     PIC  X(23)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"伝票枚数".
         05  FILLER     PIC  X(05)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"数量合計".
         05  FILLER     PIC  X(06)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"金額合計".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(30)     VALUE  SPACE.
         05  ME-03      PIC  9(08).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME-04      PIC  N(10).
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  ME-05      PIC  ZZ,ZZ9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"枚".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  ME-06      PIC  ---,---,--9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  ME-07      PIC  ----,---,--9.
 01  MEIS02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(54)     VALUE  SPACE.
         05  FILLER     PIC  N(09)     VALUE
                             NC"＊＊　合　計　＊＊".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME-08      PIC  ----,---,--9.
         05  ME-09      PIC  --,---,---,--9.
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 請求合計ファイル >>--*
 SEI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSEIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### OSKT300 HSEIGKF ERROR " SEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HSEIGKF   HTOKMS    PRTF.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE0060L HTOKMS ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HSEIGKF   HTOKMS    PRTF.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     OLD  =    HIGH-VALUE.
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
     DISPLAY  "*** SSE0060L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HSEIGKF.
     OPEN     INPUT     HTOKMS.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     MOVE     99             TO   LINE-CNT.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     INITIALIZE                   GOKEI-AREA.
*
     PERFORM  900-SEI-READ.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     IF       OLD       NOT  =    LOW-VALUE
     AND      NEW       NOT  =    OLD
              PERFORM   210-MEIS01-PRINT
     END-IF.
     IF       OLD       NOT  =    LOW-VALUE
     AND      NEW            =    HIGH-VALUE
              PERFORM   220-MEIS02-PRINT
     END-IF.
*
     MOVE     NEW            TO   OLD.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   230-SYUKEI
                        VARYING   I    FROM 1    BY   1
                        UNTIL     I    >    2
              PERFORM   900-SEI-READ
     END-IF.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HSEIGKF.
     CLOSE    HTOKMS.
     CLOSE    PRTF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE0060L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 210-MEIS01-PRINT       SECTION.
     IF       LINE-CNT  >    63
              PERFORM   211-HEAD-PRINT
     END-IF.
*
     MOVE     OLD-TOR        TO   ME-03.
     MOVE     OLD-TOR        TO   TOK-F01.
     PERFORM  900-TOK-READ.
     MOVE     TOK-F03        TO   ME-04.
     MOVE     KEI-KEN (01)   TO   ME-05.
     MOVE     KEI-SUU (01)   TO   ME-06.
     MOVE     KEI-KIN (01)   TO   ME-07.
*
     WRITE    PRT-REC        FROM MEIS01    AFTER     1.
     ADD      1              TO   LINE-CNT.
     INITIALIZE              KEI-TBL (01).
 210-MEIS01-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 220-MEIS02-PRINT       SECTION.
     IF       LINE-CNT  >    62
              PERFORM   211-HEAD-PRINT
     END-IF.
*
     MOVE     KEI-SUU (02)   TO   ME-08.
     MOVE     KEI-KIN (02)   TO   ME-09.
*
     WRITE    PRT-REC        FROM MEIS02    AFTER     2.
     ADD      2              TO   LINE-CNT.
     INITIALIZE              KEI-TBL (02).
 220-MEIS02-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ｼｭｳｹｲ                                        *
*--------------------------------------------------------------*
 230-SYUKEI             SECTION.
     ADD      1              TO   KEI-KEN (I).
     ADD      SEI-F09        TO   KEI-SUU (I).
     ADD      SEI-F06        TO   KEI-KIN (I).
*****IF       SEI-F07  =  40
****          ADD      SEI-F09        TO   KEI-SUU (I)
****          ADD      SEI-F06        TO   KEI-KIN (I)
**** ELSE
****          ADD      SEI-F09        TO   KEI-SUU (I)
****          COMPUTE KEI-KIN(I) = KEI-KIN(I) - SEI-F09
*****END-IF.
*
 230-SYUKEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 211-HEAD-PRINT         SECTION.
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
     WRITE    PRT-REC   FROM      HEAD02    AFTER     3.
     WRITE    PRT-REC   FROM      P-SPACE   AFTER     1.
     MOVE     7              TO   LINE-CNT.
 211-HEAD-PRINT-EXIT.
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
     READ     HSEIGKF   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-SEI-READ-EXIT
     END-READ.
*
     IF       SEI-F08   =   9
*******OR     SEI-F06   <   ZERO
*******OR   ((SEI-F06   <   ZERO )   AND
*******      (SEI-F05(1:2)  NOT  =  "07"))
              GO   TO   900-SEI-READ
     END-IF.
*
     MOVE     SEI-F01        TO   NEW-TOR.
 900-SEI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
