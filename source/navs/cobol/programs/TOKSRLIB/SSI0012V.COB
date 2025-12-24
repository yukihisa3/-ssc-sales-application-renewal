****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　支払明細データＣＳＶ出力　　　　　*
*    作成日／更新日　　　：　17/11/27                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払合計ファイルより支払明細データ*
*                        ：　ＣＳＶ出力を行なう。　　　　　　　*
*    新オンラインバージョン                                    *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI0012V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          01/05/08.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払合計ファイル >>--*
     SELECT   SITGKFA   ASSIGN         DA-01-S-SITGKFA
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
*----<< ＣＳＶ出力ファイル >>-*
     SELECT   KEISHIDT  ASSIGN    TO        DA-01-VS-KEISHIDT
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE  STATUS   IS   CSV-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払合計ファイル >>--*
 FD  SITGKFA            LABEL RECORD   IS   STANDARD
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
*----<< ＣＳＶ出力ファイル >>-*
 FD  KEISHIDT           LABEL RECORD   IS   STANDARD.
     COPY     KEISHIDT  OF        XFDLIB
              JOINING   CSV       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03)  VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)  VALUE  ZERO.
     03  WT-CNT         PIC  9(07)  VALUE  ZERO.
 01  IDX.
     03  I              PIC  9(03)  VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHI-ST             PIC  X(02).
 01  TOK-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
 01  CSV-ST             PIC  X(02).
*
 01  CHK-FLG            PIC  X(03)  VALUE  SPACE.
 01  CHK-FLG2           PIC  X(03)  VALUE  SPACE.
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
     USE AFTER     EXCEPTION PROCEDURE      SITGKFA.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0012V SITGKFA ERROR " SHI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0012V HTOKMS ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0012V HTENMS ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< ＣＳＶ出力ファイル >>-*
 KEISHIDT-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KEISHIDT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0012V KEISHIDT ERROR " CSV-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
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
     DISPLAY  "*** SSI0012V START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     SITGKFA.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    KEISHIDT.
*
     MOVE     LOW-VALUE      TO   BREAK-KEY.
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
*
     IF       SHI-F15   NOT  =    OLD-F15
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
              PERFORM   900-SHI-READ
     END-IF.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    SITGKFA.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
     CLOSE    KEISHIDT.
*
     DISPLAY "WT-CNT = " WT-CNT  UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI0012V END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 210-MEIS01-PRINT       SECTION.
*
     IF       NEW-TEN  <  OLD-TEN
              MOVE    99     TO    LINE-CNT
     END-IF.
*
     IF       SHI-F19  =  SPACE
     AND      CHK-FLG2 =  SPACE
              MOVE    99     TO    LINE-CNT
              MOVE   "CHK"   TO    CHK-FLG2
     END-IF.
*
     IF       SHI-F19  NOT =  SPACE
     AND      CHK-FLG2     =  "CHK"
              MOVE    99     TO    LINE-CNT
              MOVE    SPACE  TO    CHK-FLG2
     END-IF.
*
     MOVE     SPACE          TO   CSV-REC.
     INITIALIZE                   CSV-REC.
     MOVE     SHI-F09        TO   CSV-F01.
     MOVE     SHI-F11        TO   CSV-F02.
     MOVE     SHI-F12        TO   CSV-F03.
     MOVE     SHI-F09        TO   TEN-F52.
*****IF       SHI-F19  NOT NUMERIC
     IF       SHI-F21  =  SPACE
              GO             TO   210-MEIS01-PRINT-EXIT
     END-IF.
     IF       SHI-F19  =  SPACE
              MOVE  "00000"  TO   SHI-F19
     END-IF.
     MOVE     SHI-F19        TO   TEN-F011.
     MOVE     SHI-F19        TO   CSV-F04.
     PERFORM  900-TEN-READ.
     MOVE     TEN-F03        TO   CSV-F05.
     MOVE     SHI-F21        TO   CSV-F06.
     MOVE     SHI-F22        TO   CSV-F07.
     EVALUATE SHI-F21(1:2)
      WHEN "71"
           MOVE "ﾃﾞﾝﾋﾟｮｳﾀﾞｲ･ｼｮﾘﾀﾞｲ  "    TO   CSV-F08
      WHEN "72"
           MOVE "ﾌｳﾄｳﾀﾞｲ            "    TO   CSV-F08
      WHEN "73"
           MOVE "ｺﾝﾋﾟｭｰﾀﾃﾞﾝﾋﾟｮｳﾉﾊｲｿｳ"    TO   CSV-F08
      WHEN "74"
           MOVE "ﾃｶﾞｷﾃﾞﾝﾋﾟｮｳﾀﾞｲ     "    TO   CSV-F08
      WHEN "75"
           MOVE "ﾌﾞｯﾋﾟﾝｼﾞｭﾘｮｳｼｮﾍﾝｿｳ "    TO   CSV-F08
      WHEN "76"
           MOVE "ｵﾝﾗｲﾝﾃｽｳﾘｮｳ        "    TO   CSV-F08
      WHEN "77"
           MOVE "ﾜﾘﾓﾄﾞｼｷﾝ           "    TO   CSV-F08
      WHEN "78"
           MOVE "ｾﾝｺｰｳﾝｿｳﾘｮｳ        "    TO   CSV-F08
      WHEN "79"
           MOVE "ｿﾉﾀ                "    TO   CSV-F08
      WHEN "80"
           MOVE "ｼｮｳﾋｾﾞｲ            "    TO   CSV-F08
      WHEN "81"
           MOVE "ｼｮｳﾋｾﾞｲ 5%         "    TO   CSV-F08
      WHEN "82"
           MOVE "ｼｮｳﾋｾﾞｲ 3%         "    TO   CSV-F08
      WHEN "83"
           MOVE "KJﾍﾝﾋﾟﾝﾌﾞﾝ ｳﾝｿｳﾋ   "    TO   CSV-F08
      WHEN OTHER
           MOVE SHI-F23                  TO   CSV-F08
     END-EVALUATE.
*
     IF       SHI-F19  NOT =  "00000"
              MOVE SPACE                    TO   CSV-F08
     END-IF.
*
     WRITE  CSV-REC.
*
     ADD      1               TO   WT-CNT.
*
 210-MEIS01-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 250-MEIS05-PRINT       SECTION.
*
     MOVE     SPACE          TO   CSV-REC.
     INITIALIZE                   CSV-REC.
     MOVE     SHI-F26        TO   CSV-F09.
     MOVE     SHI-F27        TO   CSV-F10.
     MOVE     SHI-F28        TO   CSV-F11.
     MOVE     SHI-F29        TO   CSV-F12.
     MOVE     SHI-F30        TO   CSV-F13.
*
     WRITE  CSV-REC.
*
     ADD      1               TO   WT-CNT.
*
 250-MEIS05-PRINT-EXIT.
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
     READ     SITGKFA   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-SHI-READ-EXIT
     END-READ.
**** 2000/11/29 NAV START
*項目名称が"ﾍﾝﾋﾟﾝｿｳｻｲﾀﾞｶ"の場合，読み飛ばしを行う。
     IF       SHI-F14  =  "ﾍﾝﾋﾟﾝｿｳｻｲﾀﾞｶ        "
              MOVE   "CHK"   TO   CHK-FLG
     ELSE
              MOVE   SPACE   TO   CHK-FLG
     END-IF.
**** 2000/11/29 NAV END
     MOVE     SHI-F19        TO   NEW-TEN.
     MOVE     SHI-F09        TO   NEW-TOR.
     MOVE     SHI-F14        TO   NEW-ITEM.
 900-SHI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
