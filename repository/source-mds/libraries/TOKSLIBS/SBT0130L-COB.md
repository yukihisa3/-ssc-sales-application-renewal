# SBT0130L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBT0130L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　ＬＩＮＫＳ連携　　　　　　　　　　*
*    モジュール名　　　　：　手書未連携データリスト　　　　　　*
*    作成日／更新日　　　：　12/10/05                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　リストワークファイルより未連携　　*
*                        ：　データリストを発行する　　　　　　*
*                            （手書伝票チェックリスト流用）    *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBT0130L.
 AUTHOR.                NAV T.M.
 DATE-WRITTEN.          12/10/05.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< リストファイル　>>--*
     SELECT   SBTLSTF   ASSIGN         DA-01-VI-SBTLSTL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  LST-F01   LST-F02
                                       LST-F04   LST-F051
                                       LST-F07   LST-F112
                                       LST-F03
                        STATUS         LST-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         HTENMS-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< リストファイル >>--*
 FD  SBTLSTF            LABEL     RECORD   IS   STANDARD.
     COPY     SBTLSTF   OF        XFDLIB
              JOINING   LST       PREFIX.
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
 01  FLAGS.
     03  SKIP-FLG       PIC  9(01).
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
 01  IDX.
     03  I              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  LST-ST            PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  HTENMS-ST         PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  WK-32          PIC  X(25).
     03  WK-33          PIC  S9(12)V99   PACKED-DECIMAL.
     03  WK-34          PIC  S9(12)V99   PACKED-DECIMAL.
     03  WK-35          PIC  S9(12)V99   PACKED-DECIMAL.
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-01.
             07  NEW-TOR                    PIC  9(08).
             07  NEW-DEN                    PIC  X(09).
             07  NEW-KUB                    PIC  9(01).
             07  NEW-DKU                    PIC  9(02).
             07  NEW-TEN                    PIC  9(05).
             07  NEW-NOU                    PIC  9(08).
     03  OLD.
         05  OLD-01.
             07  OLD-TOR                    PIC  9(08).
             07  OLD-DEN                    PIC  X(09).
             07  OLD-KUB                    PIC  9(01).
             07  OLD-DKU                    PIC  9(02).
             07  OLD-TEN                    PIC  9(05).
             07  OLD-NOU                    PIC  9(08).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  X(08)     VALUE  "SBT0130L".
         05  FILLER     PIC  X(20)     VALUE  SPACE.
         05  FILLER     PIC  N(16)     VALUE
                        NC"【　手書　未連携データリスト　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(27)     VALUE  SPACE.
         05  HD-011     PIC  9999.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-012     PIC  99.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD-013     PIC  99.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD-02      PIC  ZZ9.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
 01  HEAD011.
     03  FILLER.
         05  FILLER     PIC  X(116)    VALUE  SPACE.
         05  HD-0111    PIC  99.
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-0112    PIC  99.
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-0113    PIC  99.
 01  HEAD02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"相".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(22)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"伝票_".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"出場".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝場".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"店舗".
         05  FILLER     PIC  X(20)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝区".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"注文_".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"注文日".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"納品日".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"分類".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(02)     VALUE  NC"商区".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝票".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝発".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"請区".
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"行".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"量販商品".
         05  FILLER     PIC  X(50)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"単".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価単価".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価金額".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価単価".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価金額".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(03)     VALUE  NC"スト_".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  N(03)     VALUE  NC"備　考".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(02).
         05  ME-03      PIC  9(01).
         05  FILLER     PIC  X(01).
         05  ME-04      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-05      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-06      PIC  X(09).
         05  FILLER     PIC  X(02).
         05  ME-07      PIC  X(02).
         05  FILLER     PIC  X(03).
         05  ME-08      PIC  X(02).
         05  FILLER     PIC  X(02).
         05  ME-09      PIC  9(05).
         05  FILLER     PIC  X(01).
         05  ME-10      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-11      PIC  9(02).
         05  FILLER     PIC  X(01).
         05  ME-12      PIC  N(04).
         05  FILLER     PIC  X(01).
         05  ME-13      PIC  X(07).
         05  FILLER     PIC  X(01).
         05  ME-14      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-15      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-16      PIC  X(04).
         05  FILLER     PIC  X(03).
         05  ME-17      PIC  X(02).
         05  FILLER     PIC  X(02).
         05  ME-18      PIC  X(02).
         05  FILLER     PIC  X(03).
         05  ME-19      PIC  9(01).
         05  FILLER     PIC  X(05).
     03  FILLER         CHARACTER TYPE MODE-2.
         05  ME-20      PIC  N(02).
 01  MEIS02.
     03  FILLER.
         05  FILLER     PIC  X(01).
         05  ME-21      PIC  Z9.
         05  FILLER     PIC  X(01).
         05  ME-22      PIC  X(13).
         05  FILLER     PIC  X(01).
         05  ME-23      PIC  X(30).
         05  ME-24      PIC  --,---,--9.99.
         05  FILLER     PIC  X(02).
         05  ME-25      PIC  X(01).
         05  ME-26      PIC  --,---,--9.99.
         05  ME-27      PIC  ---,---,--9.
         05  ME-28      PIC  --,---,--9.99.
         05  ME-29      PIC  ---,---,--9.
         05  FILLER     PIC  X(02).
         05  ME-30      PIC  X(05).
         05  FILLER     PIC  X(02).
         05  ME-31      PIC  X(10).
 01  MEIS03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  ME-32L     PIC  N(03)     VALUE  NC"（備考".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME-32      PIC  X(25).
         05  ME-32R     PIC  N(01)     VALUE  NC"）".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"合　計".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  ME-33      PIC  ---,---,--9.99.
         05  FILLER     PIC  X(15)     VALUE  SPACE.
         05  ME-34      PIC  ----,---,--9.
         05  FILLER     PIC  X(12)     VALUE  SPACE.
         05  ME-35      PIC  ----,---,--9.
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE             PIC  X(136)    VALUE  ALL   "-".
*
 01  MSG-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< リストファイル >>--*
 SBTLSTF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SBTLSTF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBT0130L SBTLSTF ERROR " LST-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SBTLSTF   HTOKMS    HTENMS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBT0130L HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SBTLSTF   HTOKMS    HTENMS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBT0130L HTENMS ERROR " HTENMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SBTLSTF   HTOKMS    HTENMS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     SYS-YYW        TO   WK-SYSYY.
     MOVE     SYS-MMW        TO   WK-SYSMM.
     MOVE     SYS-DDW        TO   WK-SYSDD.
*
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SBT0130L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     SBTLSTF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
     PERFORM  240-PRINT.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    SBTLSTF.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SBT0130L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｲﾝｻﾂ                                        *
*--------------------------------------------------------------*
 240-PRINT              SECTION.
     MOVE     "240-PRINT"    TO   S-NAME.
     OPEN     OUTPUT    PRTF.
     MOVE     99             TO   LINE-CNT.
     MOVE     0              TO   PAGE-CNT.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     INITIALIZE                   GOKEI-AREA.
*
*    DISPLAY " OLD = "  OLD UPON CONS.
*    DISPLAY " NEW = "  NEW UPON CONS.

     PERFORM  900-LST-START-READ.
     PERFORM  241-LIST-PRINT
                        UNTIL     OLD  =    HIGH-VALUE.
     CLOSE    PRTF.
*
 240-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｲﾝｻﾂ                                        *
*--------------------------------------------------------------*
 241-LIST-PRINT         SECTION.
     MOVE    "241-LIST-PRINT"     TO   S-NAME.
     IF       OLD  NOT  =    LOW-VALUE
     AND      NEW  NOT  =    OLD
              PERFORM   2413-MEIS03-PRINT
     END-IF.
     IF       NEW  NOT  =    HIGH-VALUE
     AND      NEW  NOT  =    OLD
              PERFORM   2411-MEIS01-PRINT
     END-IF.
     IF       NEW       NOT  =    HIGH-VALUE
     AND      LST-F03   NOT  =    80
              PERFORM   2412-MEIS02-PRINT
     END-IF.
*
     MOVE     NEW            TO   OLD.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   2414-SYUKEI
              PERFORM   900-LST-READ
     END-IF.
 241-LIST-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2411-MEIS01-PRINT      SECTION.
     MOVE    "2411-MEIS01-PRINT"  TO   S-NAME.
     IF       LINE-CNT  >    62
              PERFORM   2415-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
     MOVE     LST-F04        TO   ME-03.
     MOVE     LST-F01        TO   ME-04.
     MOVE     LST-F01        TO   TOK-F01.
     PERFORM  900-TOK-READ.
     MOVE     TOK-F03        TO   ME-05.
     MOVE     LST-F02        TO   ME-06.
     MOVE     LST-F08        TO   ME-07.
     MOVE     LST-F09        TO   ME-08.
     MOVE     LST-F07        TO   ME-09.
     MOVE     LST-F01        TO   TEN-F52.
     MOVE     LST-F07        TO   TEN-F011.
     PERFORM  900-TEN-READ.
     MOVE     TEN-F03        TO   ME-10.
     MOVE     LST-F051       TO   ME-11.
     MOVE     LST-F052       TO   ME-12.
     MOVE     LST-F44(9:7)   TO   ME-13.
     MOVE     LST-F111       TO   ME-14.
     MOVE     LST-F112       TO   ME-15.
     MOVE     LST-F12        TO   ME-16.
     MOVE     LST-F131       TO   ME-17.
     MOVE     LST-F132       TO   ME-18.
     MOVE     LST-F134       TO   ME-19.
     IF       LST-F133  =    9
              MOVE      NC"請求"  TO   ME-20
     ELSE
              MOVE      SPACE     TO   ME-20
     END-IF.
*
     WRITE    PRT-REC        FROM MEIS01    AFTER     2.
     WRITE    PRT-REC        FROM P-SPACE   AFTER     1.
     ADD      3              TO   LINE-CNT.
 2411-MEIS01-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2412-MEIS02-PRINT      SECTION.
     MOVE    "2412-MEIS02-PRINT"  TO   S-NAME.
     IF       LINE-CNT  >    63
              PERFORM   2415-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS02.
     MOVE     LST-F03        TO   ME-21.
     IF       LST-F25   NOT  =    SPACE
              MOVE      LST-F25   TO   ME-22
     ELSE
              MOVE      LST-F141  TO   ME-22
     END-IF.
     MOVE     LST-F142       TO   ME-23.
     MOVE     LST-F15        TO   ME-24.
     MOVE     LST-F16        TO   ME-25.
     MOVE     LST-F172       TO   ME-26.
     MOVE     LST-F181       TO   ME-27.
     MOVE     LST-F173       TO   ME-28.
     MOVE     LST-F182       TO   ME-29.
     MOVE     LST-F21        TO   ME-30.
     MOVE     LST-F22        TO   ME-31.
*
     IF       LINE-CNT  <    7
              WRITE     PRT-REC   FROM P-SPACE   AFTER     1
              ADD       1         TO   LINE-CNT
     END-IF.
     WRITE    PRT-REC        FROM MEIS02    AFTER     1.
     ADD      1              TO   LINE-CNT.
 2412-MEIS02-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2413-MEIS03-PRINT      SECTION.
     MOVE    "2413-MEIS03-PRINT"  TO   S-NAME.
     IF       LINE-CNT  >    62
              PERFORM   2415-HEAD-PRINT
     END-IF.
*
     IF       WK-32     NOT  =    SPACE
              MOVE      WK-32          TO   ME-32
              MOVE      NC"備考（"     TO   ME-32L
              MOVE      NC"）"         TO   ME-32R
     ELSE
              MOVE      SPACE          TO   ME-32
              MOVE      SPACE          TO   ME-32L
              MOVE      SPACE          TO   ME-32R
     END-IF.
     MOVE     WK-33          TO   ME-33.
     MOVE     WK-34          TO   ME-34.
     MOVE     WK-35          TO   ME-35.
*
     IF       LINE-CNT  <    7
              WRITE     PRT-REC   FROM P-SPACE   AFTER     1
              ADD       1         TO   LINE-CNT
     END-IF.
     WRITE    PRT-REC        FROM MEIS03    AFTER     2.
     WRITE    PRT-REC        FROM P-LINE    AFTER     1.
     ADD      3              TO   LINE-CNT.
     INITIALIZE              GOKEI-AREA.
 2413-MEIS03-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ｼｭｳｹｲ                                        *
*--------------------------------------------------------------*
 2414-SYUKEI            SECTION.
     MOVE    "2414-SYUKEI"   TO   S-NAME.
     IF       LST-F03   =    80
              MOVE      LST-F142       TO   WK-32
     ELSE
         ADD      LST-F15        TO   WK-33
         ADD      LST-F181       TO   WK-34
         ADD      LST-F182       TO   WK-35
     END-IF.
 2414-SYUKEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 2415-HEAD-PRINT        SECTION.
     MOVE    "2415-HEAD-PRINT"    TO   S-NAME.
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*
     ADD      1                   TO   PAGE-CNT.
     MOVE     SYS-YYW             TO   HD-011.
     MOVE     SYS-MMW             TO   HD-012.
     MOVE     SYS-DDW             TO   HD-013.
     MOVE     SYS-HH              TO   HD-0111.
     MOVE     SYS-MN              TO   HD-0112.
     MOVE     SYS-SS              TO   HD-0113.
     MOVE     PAGE-CNT            TO   HD-02.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD011   AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     MOVE     7              TO   LINE-CNT.
 2415-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-LST-START-READ     SECTION.
     MOVE     "900-LST-START-READ"     TO   S-NAME.
     MOVE     SPACE          TO   LST-REC.
     INITIALIZE                   LST-REC.
     START    SBTLSTF   KEY  >=   LST-F01   LST-F02
                                  LST-F04   LST-F051
                                  LST-F07   LST-F112   LST-F03
              INVALID   KEY
                        MOVE HIGH-VALUE     TO   NEW
     END-START.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   900-LST-READ
     END-IF.
 900-LST-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 READ                          *
*--------------------------------------------------------------*
 900-LST-READ           SECTION.
     MOVE     "900-LST-READ"      TO   S-NAME.
     READ     SBTLSTF   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-LST-READ-EXIT
     END-READ.
*
     IF       NEW       NOT  =    HIGH-VALUE
              MOVE      LST-F01        TO   NEW-TOR
              MOVE      LST-F02        TO   NEW-DEN
              MOVE      LST-F04        TO   NEW-KUB
              MOVE      LST-F051       TO   NEW-DKU
              MOVE      LST-F07        TO   NEW-TEN
              MOVE      LST-F112       TO   NEW-NOU
     END-IF.
 900-LST-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     MOVE     "900-TOK-READ"      TO   S-NAME.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     MOVE     "900-TEN-READ"      TO   S-NAME.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
