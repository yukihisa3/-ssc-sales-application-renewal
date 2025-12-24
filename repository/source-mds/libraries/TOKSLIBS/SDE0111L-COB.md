# SDE0111L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SDE0111L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　伝票ＥＸＣＥＬ取込　　　　　　　　*
*    モジュール名　　　　：　伝票ＥＸＣＥＬ　伝票変換リスト発行*
*    作成日／更新日　　　：　2016/10/14                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　基本売上伝票データを読み、伝票変換*
*                        ：　リストを作成する。　　　　　　　　*
*　　　　　　　　　　　　　　（明細を出力しないバージョン）　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SDE0111L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2016/10/14.
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
*----<< 伝票データ >>--*
     SELECT   URIXXXF   ASSIGN         DA-01-VI-URIXXXL3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  URI-F01   URI-F07
                                       URI-F02   URI-F03
                        STATUS         URIXXXF-ST.
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
*----<< 担当者マスタ >>--*
     SELECT   HTANMS    ASSIGN         DA-01-VI-TANMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TAN-F01   TAN-F02
                        STATUS         HTANMS-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 伝票データ >>--*
 FD  URIXXXF            LABEL     RECORD   IS   STANDARD.
     COPY     URIXXXF   OF        XFDLIB
              JOINING   URI       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 担当者マスタ >>--*
 FD  HTANMS             LABEL RECORD   IS   STANDARD.
     COPY     HTANMS    OF        XFDLIB
              JOINING   TAN       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  HTENMS-INV-FLG PIC  X(03)  VALUE  SPACE.
     03  HTANMS-INV-FLG PIC  X(03)  VALUE  SPACE.
     03  JYOKEN-INV-FLG PIC  X(03)  VALUE  SPACE.
     03  END-FLG        PIC  X(03)  VALUE  SPACE.
*
 01  COUNTER.
     03  LINE-CNT       PIC  9(03)   VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)   VALUE  ZERO.
     03  READ-CNT       PIC  9(07)   VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  URIXXXF-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  HTENMS-ST         PIC  X(02).
 01  HTANMS-ST         PIC  X(02).
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
     03  WK-27          PIC  X(30).
     03  WK-28          PIC  S9(12)V99   PACKED-DECIMAL.
     03  WK-29          PIC  S9(12)V99   PACKED-DECIMAL.
     03  WK-30          PIC  S9(12)V99   PACKED-DECIMAL.
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-01.
             07  NEW-TOR                    PIC  9(08).
             07  NEW-TEN                    PIC  9(05).
             07  NEW-DEN                    PIC  9(09).
     03  OLD.
         05  OLD-01.
             07  OLD-TOR                    PIC  9(08).
             07  OLD-TEN                    PIC  9(05).
             07  OLD-DEN                    PIC  9(09).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
*
 01  HEAD00.
     03  FILLER         PIC   X(116)   VALUE  SPACE.
     03  FILLER         PIC   X(19)    VALUE
         "+-----+-----+-----+".
*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(08)     VALUE  "SDE0111L".
         05  FILLER     PIC  X(25)     VALUE  SPACE.
         05  FILLER     PIC  N(18)     VALUE
             NC"＜（伝票ＥＸＣＥＬ）伝票変換リスト＞".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(10)     VALUE  SPACE.
         05  HD-011     PIC  9999.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-02      PIC  ZZ9.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
     03  FILLER         PIC  X(19)     VALUE
         "!     !     !     !".
*
 01  HEAD02.
     03  FILLER         PIC  X(101)    VALUE  SPACE.
     03  HD-03          PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE  ":".
     03  HD-04          PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE  ":".
     03  HD-05          PIC  Z9.
     03  FILLER         PIC  X(07)     VALUE  SPACE.
     03  FILLER         PIC  X(19)     VALUE
         "!     !     !     !".
*
 01  HEAD03.
     03  FILLER         PIC  X(116)    VALUE  SPACE.
     03  FILLER         PIC  X(19)     VALUE
         "!     !     !     !".
*
 01  HEAD04.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"変換担当者：".
         05  HD-06      PIC  X(07).
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD-07      PIC  N(10).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"変換日付：".
         05  HD-08      PIC  X(10).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-09      PIC  X(08).
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(02)     VALUE  NC"　　".
         05  FILLER     PIC  X(57)     VALUE  SPACE.
         05  FILLER     PIC  X(19)     VALUE
             "+-----+-----+-----+".
*
 01  HEAD05.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"取引先：".
         05  HD-10      PIC  9(08).
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD-11      PIC  N(10).
*
 01  HEAD06.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"相".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝票".
         05  FILLER     PIC  X(02)     VALUE  "NO".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"出場".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝場".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"店舗".
         05  FILLER     PIC  X(18)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝区".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"注文".
         05  FILLER     PIC  X(02)     VALUE  "NO".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"注文日".
         05  FILLER     PIC  X(10)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"納品日".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"分類".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"商区".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝票".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝発".
*
 01  HEAD07.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"行".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"量販店商品".
         05  FILLER     PIC  X(47)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"単".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価単価".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価金額".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価単価".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価金額".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"スト_".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"備　考".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01).
         05  ME-01      PIC  9(01).
         05  FILLER     PIC  X(01).
         05  ME-02      PIC  9(09).
         05  FILLER     PIC  X(02).
         05  ME-03      PIC  X(02).
         05  FILLER     PIC  X(03).
         05  ME-04      PIC  X(02).
         05  FILLER     PIC  X(02).
         05  ME-05      PIC  9(05).
         05  FILLER     PIC  X(02).
         05  ME-06      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-07      PIC  X(02).
         05  FILLER     PIC  X(01).
         05  ME-08      PIC  N(04).
         05  FILLER     PIC  X(01).
         05  ME-09      PIC  X(07).
         05  FILLER     PIC  X(05).
         05  ME-10      PIC  9(08).
         05  FILLER     PIC  X(06).
         05  ME-11      PIC  9(08).
         05  FILLER     PIC  X(02).
         05  ME-12      PIC  X(04).
         05  FILLER     PIC  X(05).
         05  ME-13      PIC  X(02).
         05  FILLER     PIC  X(03).
         05  ME-14      PIC  X(02).
         05  FILLER     PIC  X(05).
         05  ME-15      PIC  9(01).
*
 01  MEIS02.
     03  FILLER.
         05  ME-16      PIC  Z9.
         05  FILLER     PIC  X(01).
         05  ME-17      PIC  X(13).
         05  FILLER     PIC  X(02).
         05  ME-18      PIC  X(30).
         05  FILLER     PIC  X(01).
         05  ME-19      PIC  --,---,--9.99.
         05  FILLER     PIC  X(01).
         05  ME-20      PIC  X(01).
         05  FILLER     PIC  X(01).
         05  ME-21      PIC  --,---,--9.99.
         05  ME-22      PIC  ---,---,--9.
         05  ME-23      PIC  --,---,--9.99.
         05  ME-24      PIC  ---,---,--9.
         05  FILLER     PIC  X(02).
         05  ME-25      PIC  X(05).
         05  FILLER     PIC  X(02).
         05  ME-26      PIC  X(10).
*
 01  MEIS03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  ME-27L     PIC  N(03)     VALUE  NC"（備考".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME-27      PIC  X(30).
         05  ME-27R     PIC  N(01)     VALUE  NC"）".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"合　計".
         05  ME-28      PIC  ---,---,--9.99.
         05  FILLER     PIC  X(15)     VALUE  SPACE.
         05  ME-29      PIC  ----,---,--9.
         05  FILLER     PIC  X(12)     VALUE  SPACE.
         05  ME-30      PIC  ----,---,--9.
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE             PIC  X(136)    VALUE  ALL   "-".
 01  P-LINE1            PIC  X(136)    VALUE  ALL   "=".
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
 LINKAGE                SECTION.
 01  PARA-TOKCD             PIC   9(08).
 01  PARA-TENST             PIC   9(05).
 01  PARA-TENED             PIC   9(05).
 01  PARA-TANCD             PIC   X(02).
 01  PARA-BUMON             PIC   X(04).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-TOKCD
                                         PARA-TENST
                                         PARA-TENED
                                         PARA-TANCD
                                         PARA-BUMON.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 伝票データ >>--*
 URIXXXF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      URIXXXF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SDE0111L URIXXXF ERROR " URIXXXF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    URIXXXF   HTOKMS    HTENMS    HTANMS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SDE0111L HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    URIXXXF   HTOKMS    HTENMS    HTANMS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SDE0111L HTENMS ERROR " HTENMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    URIXXXF   HTOKMS    HTENMS    HTANMS.
     STOP     RUN.
*----<< 担当者マスタ >>--*
 HTANMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTANMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SDE0111L HTANMS ERROR " HTANMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    URIXXXF   HTOKMS    HTENMS    HTANMS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG = "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*システム日付取得
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
*システム時刻取得
     ACCEPT   SYS-TIME       FROM TIME.
*システム時刻表示
     DISPLAY  "*** SDE0111L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     URIXXXF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     INPUT     HTANMS.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     99                  TO   LINE-CNT.
     MOVE     ZERO                TO   PAGE-CNT.
     INITIALIZE                        BREAK-KEY  GOKEI-AREA.
*基本売上伝票データスタート
     MOVE     SPACE               TO   URI-REC.
     INITIALIZE                        URI-REC.
     MOVE     PARA-TOKCD          TO   URI-F01.
     MOVE     PARA-TENST          TO   URI-F07.
     START  URIXXXF  KEY  IS  >=  URI-F01  URI-F07  URI-F02
                                  URI-F03
            INVALID
            DISPLAY NC"＃＃出力対象ＤＴ無ＳＴ！＃＃" UPON CONS
            STOP  RUN
     END-START.
*
     PERFORM  URIXXXF-READ-SEC.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*
     PERFORM  LIST-PRINT-SEC.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     PERFORM GOKEI-PRINT-SEC.
*
     CLOSE    URIXXXF.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
     CLOSE    HTANMS.
     CLOSE    PRTF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SDE0111L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     DISPLAY NC"読込枚数" " = " READ-CNT  NC"件" UPON CONS.
     DISPLAY NC"出力枚数" " = " PAGE-CNT  NC"枚" UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｲﾝｻﾂ                                        *
*--------------------------------------------------------------*
 LIST-PRINT-SEC         SECTION.
*
     MOVE    "LIST-PRINT-SEC"     TO   S-NAME.
*キー項目ブレイク（取引先、店舗、伝票番号）
     IF       NEW  NOT  =    OLD
              IF  PAGE-CNT  >  ZERO
                  PERFORM  GOKEI-PRINT-SEC
              END-IF
              PERFORM   MEISAI1-PRINT-SEC
     END-IF.
*明細印刷
     PERFORM  MEISAI2-PRINT-SEC.
     PERFORM  SYUKEI-SEC.
*
     PERFORM  URIXXXF-READ-SEC.
*
 LIST-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 MEISAI1-PRINT-SEC      SECTION.
*
     MOVE    "MEISAI1-PRINT-SEC"  TO   S-NAME.
*
     IF       LINE-CNT >      53
     OR       NEW-TOR  NOT =  OLD-TOR
              PERFORM   HEAD-PRINT-SEC
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
     MOVE     URI-F04        TO   ME-01.
     MOVE     URI-F02        TO   ME-02.
     MOVE     URI-F08        TO   ME-03.
     MOVE     URI-F09        TO   ME-04.
     MOVE     URI-F07        TO   ME-05.
     MOVE     URI-F01        TO   TEN-F52.
     MOVE     URI-F07        TO   TEN-F011.
     PERFORM  900-TEN-READ.
     MOVE     TEN-F03        TO   ME-06.
     MOVE     URI-F051       TO   ME-07.
     MOVE     URI-F052       TO   ME-08.
     MOVE     SPACE          TO   ME-09.
     MOVE     URI-F111       TO   ME-10.
     MOVE     URI-F112       TO   ME-11.
     MOVE     URI-F12        TO   ME-12.
     MOVE     URI-F131       TO   ME-13.
     MOVE     URI-F132       TO   ME-14.
     MOVE     URI-F134       TO   ME-15.
*
     WRITE    PRT-REC        FROM MEIS01    AFTER     1.
*****MOVE     SPACE          TO   PRT-REC.
*****WRITE    PRT-REC        AFTER     1.
*
*****ADD      2              TO   LINE-CNT.
     ADD      1              TO   LINE-CNT.
*
 MEISAI1-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 MEISAI2-PRINT-SEC      SECTION.
*
     MOVE    "MEISAI2-PRINT-SEC"  TO   S-NAME.
*
     IF       LINE-CNT  >    53
              PERFORM   HEAD-PRINT-SEC
     END-IF.
*
     MOVE     SPACE          TO   MEIS02.
     MOVE     URI-F03        TO   ME-16.
     IF       URI-F25   NOT  =    SPACE
              MOVE      URI-F25   TO   ME-17
     ELSE
              MOVE      URI-F141  TO   ME-17
     END-IF.
     MOVE     URI-F142       TO   ME-18.
     MOVE     URI-F15        TO   ME-19.
     MOVE     URI-F16        TO   ME-20.
     MOVE     URI-F172       TO   ME-21.
     MOVE     URI-F181       TO   ME-22.
     MOVE     URI-F173       TO   ME-23.
     MOVE     URI-F182       TO   ME-24.
     MOVE     URI-F21        TO   ME-25.
     MOVE     URI-F22        TO   ME-26.
*
*****WRITE    PRT-REC        FROM MEIS02    AFTER     1.
*
*****ADD      1              TO   LINE-CNT.
*
 MEISAI2-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    合計行印刷
*--------------------------------------------------------------*
 GOKEI-PRINT-SEC        SECTION.
     MOVE    "GOKEI-PRINT-SEC"    TO   S-NAME.
*
     IF       LINE-CNT  >    53
              PERFORM   HEAD-PRINT-SEC
     END-IF.
*
     IF       WK-27     NOT  =    SPACE
              MOVE      WK-27          TO   ME-27
              MOVE      NC"備考（"     TO   ME-27L
              MOVE      NC"）"         TO   ME-27R
     ELSE
              MOVE      SPACE          TO   ME-27
              MOVE      SPACE          TO   ME-27L
              MOVE      SPACE          TO   ME-27R
     END-IF.
     MOVE     WK-28          TO   ME-28.
     MOVE     WK-29          TO   ME-29.
     MOVE     WK-30          TO   ME-30.
*
*****WRITE    PRT-REC        FROM MEIS03    AFTER     2.
     WRITE    PRT-REC        FROM MEIS03    AFTER     1.
     WRITE    PRT-REC        FROM P-LINE    AFTER     1.
*****ADD      3              TO   LINE-CNT.
     ADD      2              TO   LINE-CNT.
     INITIALIZE              GOKEI-AREA.
*
 GOKEI-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ｼｭｳｹｲ                                        *
*--------------------------------------------------------------*
 SYUKEI-SEC             SECTION.
*
     MOVE    "SYUKEI-SEC"    TO   S-NAME.
*
     ADD      URI-F15        TO   WK-28.
     ADD      URI-F181       TO   WK-29.
     ADD      URI-F182       TO   WK-30.
*
 SYUKEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 HEAD-PRINT-SEC         SECTION.
*
     MOVE    "HEAD-PRINT-SEC"     TO   S-NAME.
*改頁判定
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*
     ADD      1                   TO   PAGE-CNT.
     MOVE     SYS-YYW             TO   HD-011.
     MOVE     SYS-MMW             TO   HD-012.
     MOVE     SYS-DDW             TO   HD-013.
     MOVE     PAGE-CNT            TO   HD-02.
     MOVE     SYS-HH              TO   HD-03.
     MOVE     SYS-MN              TO   HD-04.
     MOVE     SYS-SS              TO   HD-05.
*担当者マスタ索引
     MOVE     PARA-BUMON          TO   TAN-F01.
     MOVE     PARA-TANCD          TO   TAN-F02.
     PERFORM  900-TAN-READ.
     MOVE     PARA-BUMON          TO   HD-06(1:4).
     MOVE     "-"                 TO   HD-06(5:1).
     MOVE     PARA-TANCD          TO   HD-06(6:2).
     MOVE     TAN-F03             TO   HD-07.
*変換日付時刻
     MOVE     URI-F62(1:4)        TO   HD-08(1:4).
     MOVE     "/"                 TO   HD-08(5:1).
     MOVE     URI-F62(5:2)        TO   HD-08(6:2).
     MOVE     "/"                 TO   HD-08(8:1).
     MOVE     URI-F62(7:2)        TO   HD-08(9:2).
     MOVE     URI-F67(1:2)        TO   HD-09(1:2).
     MOVE     ":"                 TO   HD-09(3:1).
     MOVE     URI-F67(3:2)        TO   HD-09(4:2).
     MOVE     ":"                 TO   HD-09(6:1).
     MOVE     URI-F67(5:2)        TO   HD-09(7:2).
*取引先マスタ索引
     MOVE     URI-F01             TO   TOK-F01  HD-10.
     PERFORM 900-TOK-READ.
     MOVE     TOK-F03             TO   HD-11.
*
     WRITE    PRT-REC   FROM      HEAD00    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD05    AFTER     1.
     WRITE    PRT-REC   FROM      P-LINE1   AFTER     1.
     WRITE    PRT-REC   FROM      HEAD06    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD07    AFTER     1.
     WRITE    PRT-REC   FROM      P-LINE1   AFTER     1.
*
     MOVE     10             TO   LINE-CNT.
 HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 READ                          *
*--------------------------------------------------------------*
 URIXXXF-READ-SEC       SECTION.
*
     MOVE     "URIXXXF-READ-SEC"  TO   S-NAME.
*
     READ     URIXXXF   AT   END
              MOVE      "END"     TO   END-FLG
              GO   TO   URIXXXF-READ-EXIT
     END-READ.
*
     ADD      1                   TO   READ-CNT.
*
     IF  READ-CNT(5:3) = "500" OR "000"
         DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*
 URIXXXF-010.
*取引先ＣＤチェック
     IF  PARA-TOKCD  NOT =  ZERO
         IF       URI-F01  >  PARA-TOKCD
                  MOVE      "END"     TO   END-FLG
                  GO   TO   URIXXXF-READ-EXIT
         END-IF
     END-IF.
 URIXXXF-020.
*店舗ＣＤチェック
     IF       URI-F07  >  PARA-TENED
              MOVE      "END"     TO   END-FLG
              GO   TO   URIXXXF-READ-EXIT
     END-IF.
*
     IF       URI-F03  =  80
              MOVE      URI-F1421 TO   WK-27(1:15)
              MOVE      URI-F1422 TO   WK-27(16:15)
              GO                  TO   URIXXXF-READ-SEC
     END-IF.
*
     MOVE     NEW                 TO   OLD.
*
     MOVE     URI-F01             TO   NEW-TOR.
     MOVE     URI-F07             TO   NEW-TEN.
     MOVE     URI-F02             TO   NEW-DEN.
*
 URIXXXF-READ-EXIT.
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
*--------------------------------------------------------------*
*    LEVEL ALL    担当者マスタ　READ                           *
*--------------------------------------------------------------*
 900-TAN-READ           SECTION.
     MOVE     "900-TAN-READ"      TO   S-NAME.
     READ     HTANMS    INVALID
              MOVE      SPACE          TO   TAN-F03
     END-READ.
 900-TAN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
