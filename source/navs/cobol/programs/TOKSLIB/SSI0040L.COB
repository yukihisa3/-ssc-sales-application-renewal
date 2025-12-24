****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　トークオンライン　　　　　　　　　*
*    モジュール名　　　　：　支払明細表作成　　　　　　　　　　*
*    作成日／更新日　　　：　95/07/25                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払合計ファイルより支払明細表を　*
*                        ：　印刷する。　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI0040L.
 AUTHOR.                TOMIZAWA.
 DATE-WRITTEN.          93/07/06.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
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
     BLOCK              CONTAINS       20   RECORDS.
     COPY    SITGKFA    OF        XFDLIB
             JOINING    SHI       PREFIX.
*
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<<       >>--*
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
*ファイルステイタス
 01  SHI-ST             PIC  X(02).
 01  TOK-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
*
 01  SIHARAI.
     03  FILLER         PIC  X(07)         VALUE SPACE.
     03  SHI-01         PIC  X(02)         VALUE SPACE.
     03  SHI-02         PIC  X(02)         VALUE SPACE.
     03  SHI-03         PIC  X(02)         VALUE SPACE.
 01  SIMEBI.
     03  FILLER         PIC  X(07)         VALUE SPACE.
     03  SHM-01         PIC  X(02)         VALUE SPACE.
     03  SHM-02         PIC  X(02)         VALUE SPACE.
     03  SHM-03         PIC  X(02)         VALUE SPACE.

*ＷＯＲＫエリア
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
 01  WK-SHIME.
     03  WK-YY-SM       PIC  9(02).
     03  WK-MM-SM       PIC  9(02).
     03  WK-DD-SM       PIC  9(02).
*
 01  WK-SHIHA.
     03  WK-YY-SH       PIC  9(02).
     03  WK-MM-SH       PIC  9(02).
     03  WK-DD-SH       PIC  9(02).
 01  WK-YY4             PIC  9(04).
 01  KOTAE              PIC  9(04).
 01  AMARI              PIC  9(04).
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
 01  OLD-F24                 PIC  9(05).
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
         05  HD-061     PIC  XX.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-062     PIC  XX.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD-063     PIC  XX.
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
         05  HD-071     PIC  XX.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-072     PIC  XX.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD-073     PIC  XX.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-221     PIC  X(01).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-222     PIC  X(01).
 01  HEAD05.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店　舗".
         05  FILLER     PIC  X(20)     VALUE  SPACE.
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
     03  FILLER         CHARACTER TYPE MODE-2.
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
     DISPLAY  "### VDA0920 HSHIGKF ERROR " SHI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### VDA0920 HTOKMS ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### VDA0920 HTENMS ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"    TO        PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PROG-CNTL              SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN       UNTIL     OLD  =    HIGH-VALUE.
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   WK-SHIME       FROM DATE.
     ACCEPT   WK-SHIHA       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI0040L START *** "
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
     PERFORM  SHI-READ.
*
     IF       NEW       NOT  =    HIGH-VALUE
              MOVE      SHI-F24        TO   OLD-F24
     END-IF.
*
*締切日設定
     IF       WK-MM-SM  =         1
              MOVE      12        TO        WK-MM-SM
              SUBTRACT  1         FROM      WK-YY-SM
     ELSE
              SUBTRACT  1         FROM      WK-MM-SM
     END-IF.
*
     EVALUATE WK-MM-SM
              WHEN      1
                    MOVE    31    TO        WK-DD-SM
              WHEN      2
                    MOVE    28    TO        WK-DD-SM
              WHEN      3
                    MOVE    31    TO        WK-DD-SM
              WHEN      4
                    MOVE    30    TO        WK-DD-SM
              WHEN      5
                    MOVE    31    TO        WK-DD-SM
              WHEN      6
                    MOVE    30    TO        WK-DD-SM
              WHEN      7
                    MOVE    31    TO        WK-DD-SM
              WHEN      8
                    MOVE    31    TO        WK-DD-SM
              WHEN      9
                    MOVE    30    TO        WK-DD-SM
              WHEN      10
                    MOVE    31    TO        WK-DD-SM
              WHEN      11
                    MOVE    30    TO        WK-DD-SM
              WHEN      12
                    MOVE    31    TO        WK-DD-SM
     END-EVALUATE.
*
     IF       WK-MM-SM  =   2
         IF       WK-YY-SM  <         90
              COMPUTE       WK-YY4    =     WK-YY-SM + 2000
         ELSE
              COMPUTE       WK-YY4    =     WK-YY-SM + 1900
         END-IF
*
         DIVIDE   WK-YY4    BY       4      GIVING  KOTAE
                                     REMAINDER      AMARI
*
         IF       AMARI         =    ZERO
                  MOVE               29   TO   WK-DD-SM
         END-IF
     END-IF.
*支払日設定
     IF       WK-MM-SH  =         12
              MOVE      1         TO        WK-MM-SH
              ADD       1         TO        WK-YY-SH
     ELSE
              ADD       1         TO        WK-MM-SH
     END-IF.
*
     EVALUATE WK-MM-SH
              WHEN      1
                    MOVE    31    TO        WK-DD-SH
              WHEN      2
                    MOVE    28    TO        WK-DD-SH
              WHEN      3
                    MOVE    31    TO        WK-DD-SH
              WHEN      4
                    MOVE    30    TO        WK-DD-SH
              WHEN      5
                    MOVE    31    TO        WK-DD-SH
              WHEN      6
                    MOVE    30    TO        WK-DD-SH
              WHEN      7
                    MOVE    31    TO        WK-DD-SH
              WHEN      8
                    MOVE    31    TO        WK-DD-SH
              WHEN      9
                    MOVE    30    TO        WK-DD-SH
              WHEN      10
                    MOVE    31    TO        WK-DD-SH
              WHEN      11
                    MOVE    30    TO        WK-DD-SH
              WHEN      12
                    MOVE    31    TO        WK-DD-SH
     END-EVALUATE.
*
     IF       WK-MM-SH  =   2
         IF       WK-YY-SH  <         90
              COMPUTE       WK-YY4    =     WK-YY-SH + 2000
         ELSE
              COMPUTE       WK-YY4    =     WK-YY-SH + 1900
         END-IF
*
         DIVIDE   WK-YY4        BY   4      GIVING    KOTAE
                                            REMAINDER AMARI
*
         IF       AMARI         =    ZERO
                  MOVE               29   TO   WK-DD-SH
         END-IF
     END-IF.
*
*
 INIT-RTN-99.
*

 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
     IF       OLD       NOT  =    LOW-VALUE
     AND (    NEW       NOT  =    OLD )
              PERFORM   MEIS02-PRINT
     END-IF.
     IF       OLD       NOT  =    LOW-VALUE
     AND (    NEW-01    NOT  =    OLD-01  )
              PERFORM   MEIS03-PRINT
     END-IF.
*
*****IF       SHI-F24   NOT  =    OLD-F24
*****         PERFORM   HEAD-PRINT
*****         MOVE      SHI-F24        TO   OLD-F24
*****END-IF.
     IF       NEW       NOT  =    HIGH-VALUE
         IF   OLD-ITEM  NOT  =    NEW-ITEM
              PERFORM   HEAD-PRINT
              MOVE      SHI-F24        TO   OLD-F24
         END-IF
     END-IF.
*
     IF       NEW       NOT  =    HIGH-VALUE
              PERFORM   MEIS01-PRINT
     END-IF.
*
     MOVE     NEW            TO   OLD.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   SYUKEI-RTN
                        VARYING   I    FROM 1    BY   1
                        UNTIL     I    >    3
              PERFORM   SHI-READ
     END-IF.
 MAIN-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　終了処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
     CLOSE    HSHIGKF.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
     CLOSE    PRTF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI0040L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　明細印刷　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MEIS01-PRINT           SECTION.
     IF       LINE-CNT  >    61
              PERFORM   HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
     IF       SHI-F19   NOT  =    ZERO
              MOVE     SHI-F19        TO   ME-08
     END-IF.
     MOVE     SHI-F32        TO   TEN-F52.
     MOVE     SHI-F19        TO   TEN-F011.
     PERFORM  TEN-READ.
     MOVE     TEN-F03        TO   ME-09.
     MOVE     SHI-F20        TO   WK-DATE.
*
     MOVE     WK-YY          TO   ME-101.
     MOVE     "."            TO   ME-101P.
     MOVE     WK-MM          TO   ME-102.
     MOVE     "."            TO   ME-102P.
     MOVE     WK-DD          TO   ME-103.
*
     MOVE     SHI-F21        TO   ME-11.
     MOVE     SHI-F22        TO   ME-12.
*
     IF       NEW       NOT  =    OLD
     AND      LINE-CNT  NOT  =    10
              WRITE     PRT-REC   FROM MEIS01    AFTER     2
              ADD       2         TO   LINE-CNT
     ELSE
              WRITE     PRT-REC   FROM MEIS01    AFTER     1
              ADD       1         TO   LINE-CNT
     END-IF.
 MEIS01-PRINT-EXIT.
     EXIT.
****************************************************************
*　　　　明細印刷　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MEIS02-PRINT           SECTION.
     IF       LINE-CNT  >    61
              PERFORM   HEAD-PRINT
     END-IF.
*
     MOVE     KEI-KIN (01)   TO   ME-14.
     WRITE    PRT-REC        FROM MEIS02    AFTER     2.
     ADD      2              TO   LINE-CNT.
     INITIALIZE              KEI-TBL (01).
 MEIS02-PRINT-EXIT.
     EXIT.
****************************************************************
*　　　　明細印刷　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MEIS03-PRINT           SECTION.
     IF       LINE-CNT  >    61
              PERFORM   HEAD-PRINT
     END-IF.
*
     MOVE     KEI-KIN (02)   TO   ME-15.
     WRITE    PRT-REC        FROM MEIS03    AFTER     2.
     ADD      2              TO   LINE-CNT.
     INITIALIZE              KEI-TBL (02).
 MEIS03-PRINT-EXIT.
     EXIT.
****************************************************************
*　　　　明細印刷　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MEIS04-PRINT           SECTION.
     MOVE     KEI-KIN (03)   TO   ME-16.
     WRITE    PRT-REC        FROM MEIS04    AFTER     2.
     ADD      2              TO   LINE-CNT.
     INITIALIZE              KEI-TBL (03).
 MEIS04-PRINT-EXIT.
     EXIT.
****************************************************************
*　　　　明細印刷　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MEIS05-PRINT           SECTION.
     IF       LINE-CNT  >    56
              PERFORM   HEAD-PRINT
     ELSE
              PERFORM   MEIS04-PRINT
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
 MEIS05-PRINT-EXIT.
     EXIT.
****************************************************************
*　　　　集計処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 SYUKEI-RTN             SECTION.
*
     ADD      SHI-F22        TO   KEI-KIN (I).
*
 SYUKEI-EXIT.
     EXIT.
****************************************************************
*　　　　タイトル印刷　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 HEAD-PRINT             SECTION.
     IF       OLD       NOT  =    LOW-VALUE
              PERFORM   MEIS04-PRINT
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
     MOVE     SHI-F32        TO   HD-03.
     MOVE     SHI-F09        TO   TOK-F01.
     PERFORM  TOK-READ.
     MOVE     TOK-F03        TO   HD-04.
     MOVE     SHI-F14        TO   HD-05.
***  MOVE     SHI-01         TO   HD-071.
***  MOVE     SHI-02         TO   HD-072.
***  MOVE     SHI-03         TO   HD-073.
***  MOVE     SHM-01         TO   HD-061.
***  MOVE     SHM-02         TO   HD-062.
***  MOVE     SHM-03         TO   HD-063.
     MOVE     SPACE          TO   HD-221.
     MOVE     SPACE          TO   HD-222.
     IF       SHI-F13   =    "0"
              MOVE      "*"       TO   HD-221
     END-IF.
     IF       SHI-F13   =    "1"
              MOVE      "*"       TO   HD-222
     END-IF.
*
*
     MOVE         WK-YY-SM      TO   HD-061.
     MOVE         WK-MM-SM      TO   HD-062.
     MOVE         WK-DD-SM      TO   HD-063.
     MOVE         WK-YY-SH      TO   HD-071.
     MOVE         WK-MM-SH      TO   HD-072.
     MOVE         WK-DD-SH      TO   HD-073.
*****DISPLAY      WK-DD-SH   UPON CONS.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD05    AFTER     3.
     WRITE    PRT-REC   FROM      P-SPACE   AFTER     1.
     MOVE     10             TO   LINE-CNT.
 HEAD-PRINT-EXIT.
     EXIT.
****************************************************************
*　　　　取引先マスタＲＥＡＤ　　　　　　　　　　　　　　　　　*
****************************************************************
 TOK-READ               SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 TOK-READ-EXIT.
     EXIT.
****************************************************************
*　　　　店舗マスタＲＥＡＤ　　　　　　　　　　　　　　　　　　*
****************************************************************
 TEN-READ               SECTION.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F03
     END-READ.
 TEN-READ-EXIT.
     EXIT.
****************************************************************
*　　　　支払合計ファイルＲＥＡＤ　　　　　　　　　　　　　　　*
****************************************************************
 SHI-READ               SECTION.
     READ     HSHIGKF   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   SHI-READ-EXIT
     END-READ.
*
     MOVE     SHI-F19        TO   NEW-TEN.
     MOVE     SHI-F09        TO   NEW-TOR.
     MOVE     SHI-F14        TO   NEW-ITEM.
 SHI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
