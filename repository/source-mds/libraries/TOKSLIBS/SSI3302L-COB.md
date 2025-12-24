# SSI3302L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSI3302L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　山新支払照合                      *
*    モジュール名　　　　：　支払明細表作成　　　　　　　　　　*
*    作成日／更新日　　　：　00/08/02                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払情報ファイルより支払明細表を　*
*                        ：　印刷する。　　　　　　　　　　　　*
*    更新日　　　　　　　：　2012/09/05
*    修正概要　　　　　　：　新レイアウト対応                  *
*    更新日　　　　　　　：　2020/01/28
*    修正概要　　　　　　：　消費税率出力追加　                *
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI3302L.
 AUTHOR.                Y.Y.
 DATE-WRITTEN.          00/08/02.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         YA        IS   YA
         YB        IS   PITCH-15
         YB-21     IS   BAIKAKU-15
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払情報データ >>--*
     SELECT   SSIHARAD  ASSIGN         DA-01-S-SITGKFE
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SSI-ST.
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
 FD  SSIHARAD           LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS 63   RECORDS.
     COPY     SITGKFE   OF        XFDLIB
              JOINING   SSI       PREFIX.
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
     03  READ-CNT       PIC  9(03).
 01  IDX.
     03  I              PIC  9(03).
 01  ID-PROGRAM.
     03  PG-ID          PIC  X(08)     VALUE  "SSI3302L".
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SSI-ST             PIC  X(02).
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
*税率変換
 01  ZEIRITU-WK         PIC  X(02).
 01  FILLER             REDEFINES      ZEIRITU-WK.
     03  WK-ZEIRITU     PIC  9(02).
*
 01  SIHARAI-JYOUHO.
     03  WK-TORICD      PIC  9(06)    VALUE  ZERO.
     03  WK-NENGETU     PIC  9(06)    VALUE  ZERO.
*
 01  WK-SSIF05          PIC  S9(11)   VALUE  ZERO.
 01  WK-TENF05          PIC  S9(11)   VALUE  ZERO.
 01  WK-GOKEIF05        PIC  S9(11)   VALUE  ZERO.
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-TEN                        PIC  9(06).
         05  NEW-DENNO                      PIC  9(08).
     03  OLD.
         05  OLD-TEN                        PIC  9(06).
         05  OLD-DENNO                      PIC  9(08).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
*****03  FILLER         CHARACTER TYPE YA.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-03      PIC  9(06).
         05  FILLER     PIC  X(08)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE BAIKAKU-15.
         05  FILLER     PIC  N(09)     VALUE
                        NC"【　支払明細書　】".
*****03  FILLER         CHARACTER TYPE MODE-2.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"処理日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-011     PIC  99.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-02      PIC  ZZ9.
 01  HEAD02.
     03  FILLER         CHARACTER TYPE PITCH-15.
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  HD-04      PIC  N(10).
 01  HEAD03.
*****03  FILLER         CHARACTER TYPE MODE-2.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(55)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"対象年月：".
         05  HD-061     PIC  9999.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-062     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
*
 01  HEAD04.
*****03  FILLER         CHARACTER TYPE MODE-2.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  N(20)     VALUE
             NC"____________________".
         05  FILLER     PIC  N(20)     VALUE
             NC"____________________".
 01  HEAD05.
*****03  FILLER         CHARACTER TYPE MODE-2.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票区分".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店　舗".
         05  FILLER     PIC  X(14)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"伝票_".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"支払金額".
*#2020/01/28 NAV ST
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"税率".
*#2020/01/28 NAV ED
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"備　考".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-15.
*******  05  ME-075     PIC  N(06).
         05  FILLER     PIC  X(03).
         05  ME-07      PIC  X(02).
         05  FILLER     PIC  X(04).
         05  FILLER     PIC  X(02).
         05  ME-08      PIC  9(02).
         05  FILLER     PIC  X(01).
         05  ME-09      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  FILLER     PIC  X(01).
******   05  ME-11      PIC  X(09).
         05  FILLER     PIC  X(01).
         05  ME-11      PIC  X(08).
         05  FILLER     PIC  X(01).
         05  ME-12      PIC  ----,---,--9.
         05  FILLER     PIC  X(05).
*#2020/01/28 NAV ST
         05  ME-13      PIC  Z9.
*#2020/01/28 NAV ED
 01  MEIS02.
*****03  FILLER         CHARACTER TYPE MODE-2.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(14)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"＊＊　店".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"合".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"計　＊＊".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  ME-14      PIC  ----,---,--9.
 01  MEIS03.
*****03  FILLER         CHARACTER TYPE MODE-2.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(14)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"＊＊　総".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"合".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"計　＊＊".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  ME-15      PIC  ----,---,--9.
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払情報データ >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SSIHARAD.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  SITGKFE   ERROR " SSI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  HTOKMS   ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  HTENMS   ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*スタートメッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "***   " PG-ID " START  *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     PERFORM  100-INIT-RTN.
**** PERFORM  200-MAIN-RTN   UNTIL     OLD  =    HIGH-VALUE.
     PERFORM  200-MAIN-RTN   UNTIL     NEW  =    HIGH-VALUE.
     PERFORM  300-END-RTN.
*
*終了メッサージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "***   " PG-ID " END    *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*ファイルのオープン
     OPEN     INPUT     SSIHARAD.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     MOVE     99             TO   LINE-CNT.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
*支払情報データ初期読込み
     PERFORM  900-SSI-READ.
     MOVE     NEW            TO   OLD.
**
*支払前回情報退避
     IF       NEW  NOT =     HIGH-VALUE
              MOVE  SSI-F01    TO   WK-TORICD
              MOVE  SSI-F06    TO   WK-NENGETU
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*明細
     IF       NEW       NOT  =    HIGH-VALUE
              PERFORM   210-MEIS01-PRINT
     END-IF.
*
     IF       NEW  NOT  =    HIGH-VALUE
*キーの入れ替え
              MOVE     NEW            TO   OLD
              PERFORM   900-SSI-READ
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*** 合計出力制御
     IF  READ-CNT  >  ZERO
         PERFORM   220-MEIS02-PRINT
     END-IF.
*ファイルのクローズ
     CLOSE    SSIHARAD.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
     CLOSE    PRTF.
*
     DISPLAY "* SITGKFE (IN)=" READ-CNT " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" PAGE-CNT " *" UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ﾒｲｻｲ ｲﾝｻﾂ（明細行出力）                      *
*--------------------------------------------------------------*
 210-MEIS01-PRINT       SECTION.
 210-MEIS01-010.
     IF       LINE-CNT  >    60
              PERFORM   211-HEAD-PRINT
     END-IF.
***  店舗ブレーク判定
**   IF       NEW       NOT =  OLD
     IF       NEW-TEN   NOT =  OLD-TEN
*
              GO  TO   210-MEIS01-100
     END-IF.
*
     MOVE     SPACE               TO   MEIS01.
*伝票区分
**   EVALUATE SSI-F04
*        WHEN     "10"
*        MOVE   NC"納品伝票"      TO   ME-075
*        WHEN     "20"
*        MOVE   NC"返品値引伝票"  TO   ME-075
*        WHEN     OTHER
*        MOVE   ALL NC"＊"        TO   ME-075
**   END-EVALUATE.
     MOVE     SSI-F04         TO   ME-07.
*店舗コード
     IF       SSI-F02   NOT  =    ZERO
              MOVE      SSI-F02   TO   ME-08
     END-IF.
*店舗名称取得
     MOVE     SSI-F01        TO   TEN-F52.
     MOVE     SSI-F02        TO   TEN-F011.
     PERFORM  900-TEN-READ.
     IF       TEN-F03   =    SPACE
              MOVE      ALL NC"＊"     TO   ME-09
     ELSE
              MOVE      TEN-F03        TO   ME-09
     END-IF.
*伝票_
     MOVE     SSI-F03        TO   ME-11.
*支払金額
*
     IF       SSI-F04   =    51
              COMPUTE   ME-12     =    SSI-F05  *  -1
              COMPUTE   WK-SSIF05  =   SSI-F05  *  -1
     ELSE
              MOVE      SSI-F05        TO   ME-12
              MOVE      SSI-F05        TO   WK-SSIF05
     END-IF.
*#2020/01/28 NAV ST
     MOVE     SSI-FIL(1:2)             TO   ZEIRITU-WK.
     MOVE     WK-ZEIRITU               TO   ME-13.
*#2020/01/28 NAV ED
*
     ADD      WK-SSIF05      TO  WK-TENF05.
     WRITE    PRT-REC   FROM MEIS01    AFTER     1.
     ADD      1         TO   LINE-CNT.
     GO       TO   210-MEIS01-PRINT-EXIT.
*
 210-MEIS01-100.
*
     PERFORM  220-MEIS02-PRINT.
     MOVE     NEW      TO    OLD.
     GO  TO   210-MEIS01-010.
*
*
 210-MEIS01-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ（店舗計）                          *
*--------------------------------------------------------------*
 220-MEIS02-PRINT       SECTION.
*
**** IF       NEW-TOR  NOT  =  OLD-TOR
****          GO   TO   220-MEIS02-010
**** END-IF.
*
     MOVE     WK-TENF05      TO   ME-14.
     ADD      WK-TENF05      TO   WK-GOKEIF05.
***
     WRITE    PRT-REC        FROM MEIS02    AFTER     2.
     MOVE     SPACE          TO   PRT-REC.
     WRITE    PRT-REC                       AFTER     1.
     ADD      3              TO   LINE-CNT.
     MOVE     ZERO           TO   WK-TENF05.
*
     IF       NEW  NOT  =  HIGH-VALUE
              GO   TO   220-MEIS02-200
     END-IF.
 220-MEIS02-010.
*
     MOVE     WK-GOKEIF05    TO   ME-15.
**
     WRITE    PRT-REC        FROM MEIS03    AFTER     2.
     MOVE     SPACE          TO   PRT-REC.
     WRITE    PRT-REC                       AFTER     1.
     ADD      3              TO   LINE-CNT.
     MOVE     ZERO           TO   WK-GOKEIF05.
     MOVE     99             TO   LINE-CNT.
**
 220-MEIS02-200.
*
 220-MEIS02-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ（ＨＥＡＤプリント）          *
*--------------------------------------------------------------*
 211-HEAD-PRINT         SECTION.
*
*改頁制御
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*ページカウンター
     ADD      1                   TO   PAGE-CNT.
*システム日付
     MOVE     SYS-YY              TO   HD-011.
     MOVE     SYS-MM              TO   HD-012.
     MOVE     SYS-DD              TO   HD-013.
*頁
     MOVE     PAGE-CNT            TO   HD-02.
*取引先コード
     MOVE     WK-TORICD           TO   HD-03.
*取引先名取得
     MOVE     WK-TORICD           TO   TOK-F01.
     PERFORM  900-TOK-READ.
     MOVE     TOK-F03             TO   HD-04.
*請求データ対象年月
     MOVE     WK-NENGETU(1:4)     TO   HD-061.
     MOVE     WK-NENGETU(5:2)     TO   HD-062.
*ＨＥＡＤ出力
     WRITE    PRT-REC   FROM      HEAD01    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD05    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     MOVE     7         TO        LINE-CNT.
*
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
*    LEVEL ALL    支払情報データ　　 READ                      *
*--------------------------------------------------------------*
 900-SSI-READ           SECTION.
     READ     SSIHARAD  AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-SSI-READ-EXIT
     END-READ.
*
     ADD      1              TO   READ-CNT.
     MOVE     SSI-F02        TO   NEW-TEN.
     MOVE     SSI-F03        TO   NEW-DENNO.
 900-SSI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
