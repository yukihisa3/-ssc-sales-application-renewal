****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　支払明細表作成　　　　　　　　　　*
*    作成日／更新日　　　：　92/12/02                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払合計ファイルより支払明細表を　*
*                        ：　印刷する。　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI0010L.
 AUTHOR.                S.K  SANKYO.
 DATE-WRITTEN.          92/12/02.
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
*----<< 支払合計ファイル >>--*
     SELECT   HSHIGKF   ASSIGN         DA-01-S-HSHIGKF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SHI-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払合計ファイル >>--*
 FD  HSHIGKF            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS 20   RECORDS.
     COPY     SITGKFA   OF        XFDLIB
              JOINING   SHI       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
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
 01  SHI-ST             PIC  X(02).
 01  TOK-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
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
     03  KEI-TBL        OCCURS    3.
         05  KEI-KIN    PIC  S9(12)V99   PACKED-DECIMAL.
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-TEN                        PIC  X(05).
         05  NEW-01.
             07  NEW-ITEM                   PIC  X(20).
             07  NEW-02.
                 09  NEW-TOR                PIC  X(04).
     03  OLD.
         05  OLD-TEN                        PIC  X(05).
         05  OLD-01.
             07  OLD-ITEM                   PIC  X(20).
             07  OLD-02.
                 09  OLD-TOR                PIC  X(04).
     03  OLD-F16                            PIC  X(01).
*
 01  OLD-F15                 PIC  9(05).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-03      PIC  9(08).
         05  FILLER     PIC  X(08)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  N(09)     VALUE
                        NC"【　支払明細書　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"処理日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-011     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-02      PIC  ZZ9.
 01  HEAD02.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  HD-04      PIC  N(10).
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(56)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"締切日：".
         05  HD-061     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-062     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD-063     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"現手".
 01  HEAD04.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"項目：".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-05      PIC  X(20).
         05  FILLER     PIC  X(27)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"支払日：".
         05  HD-071     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-072     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD-073     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-221     PIC  X(01).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-222     PIC  X(01).
 01  HEAD05.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店　舗".
*********05  FILLER     PIC  X(20)     VALUE  SPACE.
         05  FILLER     PIC  X(15)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(06)     VALUE  NC"年　月　日".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  N(03)     VALUE  NC"伝票_".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"支払金額".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"備　考".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
*****03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(04).
         05  FILLER     PIC  X(01).
         05  ME-08      PIC  9(05).
         05  FILLER     PIC  X(01).
         05  ME-09      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-101     PIC  ZZ.
         05  ME-101P    PIC  X(01).
         05  ME-102     PIC  ZZ.
         05  ME-102P    PIC  X(01).
         05  ME-103     PIC  ZZ.
         05  FILLER     PIC  X(01).
         05  ME-11      PIC  X(09).
         05  FILLER     PIC  X(01).
         05  ME-12      PIC  ----,---,--9.
         05  FILLER     PIC  X(01).
         05  ME-13      PIC  X(19).
 01  MEIS02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(28)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"＊＊".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"店".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"合".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"計".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"＊＊".
         05  FILLER     PIC  X(14)     VALUE  SPACE.
         05  ME-14      PIC  ----,---,--9.
 01  MEIS03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(28)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"＊＊".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"項".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"目".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"計".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"＊＊".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME-15      PIC  ----,---,--9.
 01  MEIS04.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(28)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"＊＊".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"ページ計".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"＊＊".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME-16      PIC  ----,---,--9.
*
 01  HEAD06.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(19)  VALUE  SPACE.
         05  FILLER     PIC  N(06)  VALUE  NC"照合御支払高".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(08)  VALUE  NC"返品未照合相殺高".
         05  FILLER     PIC  X(06)  VALUE  SPACE.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  N(03)  VALUE  NC"相殺高".
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(03)  VALUE  SPACE.
         05  FILLER     PIC  N(06)  VALUE  NC"合計御支払高".
         05  FILLER     PIC  X(03)  VALUE  SPACE.
         05  FILLER     PIC  N(06)  VALUE  NC"請求不照合高".
 01  MEIS05.
         05  FILLER     PIC  X(16).
         05  ME-17      PIC  ----,---,--9.
         05  FILLER     PIC  X(01).
         05  ME-18      PIC  ----,---,--9.
         05  ME-19      PIC  ----,---,--9.
         05  ME-20      PIC  ----,---,--9.
         05  ME-21      PIC  ----,---,--9.
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計ファイル >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0010L HSHIGKF ERROR " SHI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HSHIGKF   HTOKMS    HTENMS    PRTF.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0010L HTOKMS ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HSHIGKF   HTOKMS    HTENMS    PRTF.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0010L HTENMS ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HSHIGKF   HTOKMS    HTENMS    PRTF.
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
     DISPLAY  "*** SSI0010L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HSHIGKF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     MOVE     99             TO   LINE-CNT.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     INITIALIZE                   GOKEI-AREA.
*
     PERFORM  900-SHI-READ.
*
     IF       NEW       NOT  =    HIGH-VALUE
              MOVE      SHI-F15        TO   OLD-F15
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     IF       OLD       NOT  =    LOW-VALUE
     AND (    NEW       NOT  =    OLD
         OR   SHI-F16        =    SPACE     )
     AND      OLD-F16   NOT  =    SPACE
              PERFORM   220-MEIS02-PRINT
     END-IF.
     IF       OLD       NOT  =    LOW-VALUE
     AND (    NEW-01    NOT  =    OLD-01
         OR   SHI-F16        =    SPACE     )
     AND      OLD-F16   NOT  =    SPACE
              PERFORM   230-MEIS03-PRINT
     END-IF.
*
     IF       SHI-F15   NOT  =    OLD-F15
              PERFORM   211-HEAD-PRINT
              MOVE      SHI-F15        TO   OLD-F15
     END-IF.
*
     IF       NEW       NOT  =    HIGH-VALUE
              IF   SHI-F16   =    SPACE
                   PERFORM   250-MEIS05-PRINT
              ELSE
                   PERFORM   210-MEIS01-PRINT
              END-IF
     END-IF.
*
     MOVE     SHI-F16        TO   OLD-F16.
     MOVE     NEW            TO   OLD.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   260-SYUKEI
                        VARYING   I    FROM 1    BY   1
                        UNTIL     I    >    3
              PERFORM   900-SHI-READ
     END-IF.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HSHIGKF.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
     CLOSE    PRTF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI0010L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 210-MEIS01-PRINT       SECTION.
     IF       LINE-CNT  >    61
**** OR       NEW-01    NOT  =    OLD-01
              PERFORM   211-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
     IF       SHI-F19   NOT  =    ZERO
              MOVE     SHI-F19        TO   ME-08
     END-IF.
     MOVE     SHI-F09        TO   TEN-F52.
     MOVE     SHI-F19        TO   TEN-F011.
     PERFORM  900-TEN-READ.
     MOVE     TEN-F03        TO   ME-09.
     MOVE     SHI-F20        TO   WK-DATE.
**** IF       SHI-F20   NOT  =    ZERO
              MOVE     WK-YY          TO   ME-101.
              MOVE     "."            TO   ME-101P.
              MOVE     WK-MM          TO   ME-102.
              MOVE     "."            TO   ME-102P.
              MOVE     WK-DD          TO   ME-103.
**** END-IF.
     MOVE     SHI-F21        TO   ME-11.
     MOVE     SHI-F22        TO   ME-12.
     EVALUATE SHI-F21(1:2)
         WHEN "71"
              MOVE "ﾃﾞﾝﾋﾟｮｳﾀﾞｲ･ｼｮﾘﾀﾞｲ  "    TO   ME-13
         WHEN "72"
              MOVE "ﾌｳﾄｳﾀﾞｲ            "    TO   ME-13
         WHEN "73"
              MOVE "ｺﾝﾋﾟｭｰﾀﾃﾞﾝﾋﾟｮｳﾉﾊｲｿｳ"    TO   ME-13
         WHEN "74"
              MOVE "ﾃｶﾞｷﾃﾞﾝﾋﾟｮｳﾀﾞｲ     "    TO   ME-13
         WHEN "75"
              MOVE "ﾌﾞｯﾋﾟﾝｼﾞｭﾘｮｳｼｮﾍﾝｿｳ "    TO   ME-13
         WHEN "76"
              MOVE "ｵﾝﾗｲﾝﾃｽｳﾘｮｳ        "    TO   ME-13
         WHEN "77"
              MOVE "ﾜﾘﾓﾄﾞｼｷﾝ           "    TO   ME-13
         WHEN "78"
              MOVE "ｾﾝｺｰｳﾝｿｳﾘｮｳ        "    TO   ME-13
         WHEN "79"
              MOVE "ｿﾉﾀ                "    TO   ME-13
         WHEN "80"
              MOVE "ｼｮｳﾋｾﾞｲ            "    TO   ME-13
         WHEN "81"
              MOVE "ｼｮｳﾋｾﾞｲ 5%         "    TO   ME-13
         WHEN "82"
              MOVE "ｼｮｳﾋｾﾞｲ 3%         "    TO   ME-13
         WHEN "83"
              MOVE "KJﾍﾝﾋﾟﾝﾌﾞﾝ ｳﾝｿｳﾋ   "    TO   ME-13
         WHEN OTHER
              MOVE SHI-F23                  TO   ME-13
     END-EVALUATE.
*
     IF       NEW       NOT  =    OLD
     AND      LINE-CNT  NOT  =    10
              WRITE     PRT-REC   FROM MEIS01    AFTER     2
              ADD       2         TO   LINE-CNT
     ELSE
              WRITE     PRT-REC   FROM MEIS01    AFTER     1
              ADD       1         TO   LINE-CNT
     END-IF.
 210-MEIS01-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 220-MEIS02-PRINT       SECTION.
     IF       LINE-CNT  >    61
              PERFORM   211-HEAD-PRINT
     END-IF.
*
     MOVE     KEI-KIN (01)   TO   ME-14.
     WRITE    PRT-REC        FROM MEIS02    AFTER     2.
     ADD      2              TO   LINE-CNT.
     INITIALIZE              KEI-TBL (01).
 220-MEIS02-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 230-MEIS03-PRINT       SECTION.
     IF       LINE-CNT  >    61
              PERFORM   211-HEAD-PRINT
     END-IF.
*
     MOVE     KEI-KIN (02)   TO   ME-15.
     WRITE    PRT-REC        FROM MEIS03    AFTER     2.
     ADD      2              TO   LINE-CNT.
     INITIALIZE              KEI-TBL (02).
 230-MEIS03-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 240-MEIS04-PRINT       SECTION.
     MOVE     KEI-KIN (03)   TO   ME-16.
     WRITE    PRT-REC        FROM MEIS04    AFTER     2.
     ADD      2              TO   LINE-CNT.
     INITIALIZE              KEI-TBL (03).
 240-MEIS04-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 250-MEIS05-PRINT       SECTION.
     IF       LINE-CNT  >    56
              PERFORM   211-HEAD-PRINT
     ELSE
              PERFORM   240-MEIS04-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS05.
     MOVE     SHI-F26        TO   ME-17.
     MOVE     SHI-F27        TO   ME-18.
     MOVE     SHI-F28        TO   ME-19.
     MOVE     SHI-F29        TO   ME-20.
     MOVE     SHI-F30        TO   ME-21.
*
     WRITE    PRT-REC        FROM HEAD06    AFTER     3.
     WRITE    PRT-REC        FROM MEIS05    AFTER     2.
     ADD      5              TO   LINE-CNT.
 250-MEIS05-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ｼｭｳｹｲ                                        *
*--------------------------------------------------------------*
 260-SYUKEI             SECTION.
     ADD      SHI-F22        TO   KEI-KIN (I).
 260-SYUKEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 211-HEAD-PRINT         SECTION.
     IF       OLD       NOT  =    LOW-VALUE
              PERFORM   240-MEIS04-PRINT
     END-IF.
*
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
     MOVE     SHI-F09        TO   HD-03.
     MOVE     SHI-F09        TO   TOK-F01.
     PERFORM  900-TOK-READ.
     MOVE     TOK-F03        TO   HD-04.
     MOVE     SHI-F14        TO   HD-05.
     MOVE     SHI-F11(1:2)   TO   HD-061.
     MOVE     SHI-F11(3:2)   TO   HD-062.
     MOVE     SHI-F11(5:2)   TO   HD-063.
     MOVE     SHI-F12(1:2)   TO   HD-071.
     MOVE     SHI-F12(3:2)   TO   HD-072.
     MOVE     SHI-F12(5:2)   TO   HD-073.
     MOVE     SPACE          TO   HD-221.
     MOVE     SPACE          TO   HD-222.
     IF       SHI-F13   =    "0"
              MOVE      "*"       TO   HD-221
     END-IF.
     IF       SHI-F13   =    "1"
              MOVE      "*"       TO   HD-222
     END-IF.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD05    AFTER     3.
     WRITE    PRT-REC   FROM      P-SPACE   AFTER     1.
     MOVE     10             TO   LINE-CNT.
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
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払合計ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SHI-READ           SECTION.
     READ     HSHIGKF   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-SHI-READ-EXIT
     END-READ.
*
     MOVE     SHI-F19        TO   NEW-TEN.
     MOVE     SHI-F09        TO   NEW-TOR.
     MOVE     SHI-F14        TO   NEW-ITEM.
 900-SHI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
