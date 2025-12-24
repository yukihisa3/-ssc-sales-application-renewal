****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　支払照合（ジョイフル本田）        *
*    モジュール名　　　　：　支払明細表作成　　　　　　　　　　*
*    作成日／更新日　　　：　05/10/21                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払合計ファイルより支払明細表を　*
*                        ：　印刷する。　　　　　　　　　　　　*
*    作成日／更新日　　　：　17/04/17                          *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　店舗ＣＤ＝４桁対応　　　　　　　　*
*                        ：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI7104L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          05/10/21.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         YB        IS   PITCH-15
         YB-21     IS   BAIKAKU-15
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.

 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払明細データ >>--*
     SELECT   SITGK71   ASSIGN         DA-01-VI-SITGK711
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SGK-F01   SGK-F02
                                       SGK-F04   SGK-F05
                                       SGK-F06
                        STATUS         SGK-ST.
*----<< 支払合計データ >>--*
     SELECT   SITGG71   ASSIGN         DA-01-VI-SITGG711
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SGG-F01
                        STATUS         SGG-ST.
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
*----<< 支払明細ファイル >>--*
 FD  SITGK71            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  1   RECORDS.
     COPY     SITGK71   OF        XFDLIB
              JOINING   SGK       PREFIX.
*----<< 支払合計ファイル >>--*
 FD  SITGG71            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  1   RECORDS.
     COPY     SITGG71   OF        XFDLIB
              JOINING   SGG       PREFIX.
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
     03  CNT-READ       PIC  9(07).
 01  IDX.
     03  I              PIC  9(03).
 01  ID-PROGRAM.
     03  PG-ID          PIC  X(08)     VALUE  "SSI7104L".
 01  TEN-FLG            PIC  9(01).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SGK-ST             PIC  X(02).
 01  SGG-ST             PIC  X(02).
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
 01  SIHARAI-JYOUHO.
     03  WK-SIMEBI      PIC  9(06)    VALUE  ZERO.
     03  WK-KINGAK      PIC S9(10)    VALUE  ZERO.
     03  WK-TENKEI      PIC S9(10)    VALUE  ZERO.
     03  WK-SOKEI       PIC S9(10)    VALUE  ZERO.
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-SIMEBI                     PIC  9(06).
         05  NEW-TEN                        PIC  9(05).
     03  OLD.
         05  OLD-SIMEBI                     PIC  9(06).
         05  OLD-TEN                        PIC  9(05).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(08)     VALUE "SSI7104L".
         05  FILLER     PIC  X(38)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE BAIKAKU-15.
         05  FILLER     PIC  N(09)     VALUE
                        NC"【　支払明細書　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(38)     VALUE  SPACE.
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
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"取引先：".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-03      PIC  9(04)     VALUE  2243.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD-04      PIC  N(07)
                             VALUE     NC"ジョイフル本田".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"締切日付：".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-051     PIC  99.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-052     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-053     PIC  Z9.
*
 01  HEAD04.
     03  FILLER     PIC  X(40)     VALUE
         "========================================".
     03  FILLER     PIC  X(40)     VALUE
         "========================================".
     03  FILLER     PIC  X(40)     VALUE
         "========================================".
     03  FILLER     PIC  X(16)     VALUE
         "================".
 01  HEAD05.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"店舗コード".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗名".
         05  FILLER     PIC  X(26)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"取引日付".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票区分".
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票番号".
         05  FILLER     PIC  X(14)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"金　額".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"備　考".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-15.
*********#2017/04/17 NAV ST
*********05  FILLER     PIC  X(07).
         05  FILLER     PIC  X(05).
*********05  ME-01      PIC  ZZ.
         05  ME-01      PIC  ZZZZ.
*********#2017/04/17 NAV ST
         05  FILLER     PIC  X(08).
         05  ME-02      PIC  N(15).
         05  FILLER     PIC  X(06).
         05  ME-031     PIC  99.
         05  SURASYU1   PIC  X(01).
         05  ME-032     PIC  Z9.
         05  SURASYU2   PIC  X(01).
         05  ME-033     PIC  Z9.
         05  FILLER     PIC  X(07).
         05  ME-041     PIC  9(02).
         05  FILLER     PIC  X(01)   VALUE   ":".
         05  ME-042     PIC  N(02).
         05  FILLER     PIC  X(07).
         05  ME-05      PIC  9(06).
         05  FILLER     PIC  X(06).
         05  ME-06      PIC  ----,---,--9.
         05  FILLER     PIC  X(05).
         05  ME-07      PIC  N(10).
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  ME-08      PIC  X(01).
 01  MEIS02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(64)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"＊＊　店".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"合".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"計　＊＊".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  ME-15      PIC  ----,---,--9.
 01  MEIS03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(64)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"＊＊　総".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"合".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"計　＊＊".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  ME-16      PIC  ----,---,--9.
         05  FILLER     PIC  N(05)     VALUE  NC"（消費税：".
         05  ME-17      PIC  ----,---,--9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"税込合計：".
         05  ME-18      PIC  ----,---,--9.
         05  FILLER     PIC  N(01)     VALUE  NC"）".
*
 01  SEN1.
     03  FILLER     PIC  X(40)     VALUE  SPACE.
     03  FILLER     PIC  X(40)     VALUE
         "----------------------------------------".
     03  FILLER     PIC  X(40)     VALUE
         "----------------------------------------".
     03  FILLER     PIC  X(16)     VALUE
         "----------------".
 01  SEN2.
     03  FILLER     PIC  X(40)     VALUE
         "----------------------------------------".
     03  FILLER     PIC  X(40)     VALUE
         "----------------------------------------".
     03  FILLER     PIC  X(40)     VALUE
         "----------------------------------------".
     03  FILLER     PIC  X(16)     VALUE
         "----------------".
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
*----<< 支払明細データ >>--*
 SGK-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SITGK71.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  SITGK71   ERROR " SGK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 支払合計データ >>--*
 SGG-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SITGG71.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  SITGG71   ERROR " SGG-ST " "
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
     PERFORM  200-MAIN-RTN   UNTIL     OLD  =    HIGH-VALUE.
     PERFORM  300-END-RTN.
*
*終了メッセージ出力
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
     OPEN     INPUT     SITGK71.
     OPEN     INPUT     SITGG71.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     MOVE     99             TO   LINE-CNT.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     MOVE     ZERO           TO   TEN-FLG.
*支払明細データ初期読込み
     PERFORM  900-SGK-READ.
*
     IF       NEW       NOT =     HIGH-VALUE
              MOVE      SGK-F01   TO   WK-SIMEBI
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     IF       NEW-TEN   NOT =     OLD-TEN
     AND      CNT-READ      >     1
*             店舗合計出力
              PERFORM   TENKEI-WRITE-SEC
**************IF  NEW-SIMEBI NOT = OLD-SIMEBI
******************MOVE     NEW-SIMEBI   TO   WK-SIMEBI
******************PERFORM  211-HEAD-PRINT
**************END-IF
     END-IF.
*キーの入れ替え
     MOVE     NEW            TO   OLD.
*明細
     IF       NEW       NOT  =    HIGH-VALUE
              PERFORM   210-MEIS01-PRINT
*
              PERFORM   900-SGK-READ
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
*    総合計出力
     IF       CNT-READ       >    ZERO
              PERFORM   SOKEI-WRITE-SEC
     END-IF.
*ファイルのクローズ
     CLOSE    SITGK71.
     CLOSE    SITGG71.
     CLOSE    HTENMS.
     CLOSE    PRTF.
*
     DISPLAY "* SITGK71 (IN)=" CNT-READ " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" PAGE-CNT " *" UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ﾒｲｻｲ ｲﾝｻﾂ（明細行出力）                      *
*--------------------------------------------------------------*
 210-MEIS01-PRINT       SECTION.
     IF       LINE-CNT  >    56
              PERFORM   211-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE               TO   MEIS01.
*
     IF  TEN-FLG  NOT = 0
*店舗コード
         MOVE     SGK-F02        TO   ME-01
*店舗名称取得
         MOVE     2243           TO   TEN-F52
         MOVE     SGK-F02        TO   TEN-F011
         PERFORM  900-TEN-READ
         IF       TEN-F03   =    SPACE
                  MOVE      ALL NC"＊"     TO   ME-02
         ELSE
                  MOVE      TEN-F03        TO   ME-02
         END-IF
     END-IF.
*取引日
     MOVE     SGK-F04(1:2)   TO   ME-031.
     MOVE     SGK-F04(3:2)   TO   ME-032.
     MOVE     SGK-F04(5:2)   TO   ME-033.
     MOVE     "/"            TO   SURASYU1
                                  SURASYU2.
*伝票区分
     EVALUATE SGK-F05
         WHEN     "31"
         MOVE      31             TO   ME-041
         MOVE   NC"仕入"          TO   ME-042
         WHEN     "21"
         MOVE      21             TO   ME-041
         MOVE   NC"返品"          TO   ME-042
         WHEN     "11"
         MOVE      11             TO   ME-041
         MOVE   NC"値引"          TO   ME-042
         WHEN     OTHER
         MOVE   ALL "*"           TO   ME-041
         MOVE   ALL NC"＊"        TO   ME-042
     END-EVALUATE.
*伝票_
     MOVE     SGK-F06        TO   ME-05.
*備考
     EVALUATE SGK-F09
         WHEN    "20"
              MOVE      NC"２０　支払"      TO   ME-07
         WHEN    "30"
              MOVE      NC"３０　支払訂正"  TO   ME-07
         WHEN    "40"
              MOVE      NC"４０　未検収"    TO   ME-07
              GO        TO    210-MEIS01-PRINT-EXIT
         WHEN     OTHER
              MOVE      ALL NC"＊"          TO   ME-07
     END-EVALUATE.
*支払金額
*****MOVE     SGK-F07        TO   ME-06.
*****ADD      SGK-F07        TO   WK-TENKEI.
     MOVE     SGK-F07        TO   WK-KINGAK.
     IF       SGK-F05   =    21   OR   11
              COMPUTE   WK-KINGAK = WK-KINGAK * (-1)
     END-IF.
     MOVE     WK-KINGAK      TO   ME-06.
     ADD      WK-KINGAK      TO   WK-TENKEI.
*
     WRITE    PRT-REC   FROM MEIS01    AFTER     1.
     ADD      1         TO   LINE-CNT.
*
 210-MEIS01-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      店舗計出力                                  *
*--------------------------------------------------------------*
 TENKEI-WRITE-SEC       SECTION.
*
     IF       LINE-CNT  >    56
              PERFORM   211-HEAD-PRINT
     END-IF.
*
     MOVE     WK-TENKEI      TO   ME-15.
     WRITE    PRT-REC        FROM SEN1      AFTER     1.
     WRITE    PRT-REC        FROM MEIS02    AFTER     1.
*****MOVE     SPACE          TO   PRT-REC.
*****WRITE    PRT-REC                       AFTER     1.
     WRITE    PRT-REC        FROM SEN2      AFTER     1.
     ADD      3              TO   LINE-CNT.
     MOVE     ZERO           TO   WK-TENKEI.
     MOVE     1              TO   TEN-FLG.
*
 TENKEI-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5      総合計出力                                  *
*--------------------------------------------------------------*
 SOKEI-WRITE-SEC        SECTION.
*
     IF       LINE-CNT  >    56
              PERFORM   211-HEAD-PRINT
     END-IF.
*
     MOVE     WK-SIMEBI      TO   SGG-F01.
     READ     SITGG71.
*****MOVE     WK-SOKEI       TO   ME-16.
     MOVE     SGG-F05        TO   ME-16.
     MOVE     SGG-F06        TO   ME-17.
     MOVE     SGG-F07        TO   ME-18.
*****WRITE    PRT-REC        FROM MEIS03    AFTER     2.
     WRITE    PRT-REC        FROM MEIS03    AFTER     1.
     MOVE     SPACE          TO   PRT-REC.
     WRITE    PRT-REC                       AFTER     1.
     ADD      3              TO   LINE-CNT.
*
 SOKEI-WRITE-EXIT.
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
*締切日付
     MOVE     WK-SIMEBI(1:2)      TO   HD-051.
     MOVE     WK-SIMEBI(3:2)      TO   HD-052.
     MOVE     WK-SIMEBI(5:2)      TO   HD-053.
*ＨＥＡＤ出力
     WRITE    PRT-REC   FROM      HEAD01    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD05    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     MOVE     7         TO        LINE-CNT.
     MOVE     1         TO        TEN-FLG.
*
 211-HEAD-PRINT-EXIT.
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
*    LEVEL ALL    支払合計データ　　 READ                      *
*--------------------------------------------------------------*
 900-SGK-READ           SECTION.
     READ     SITGK71   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-SGK-READ-EXIT
     END-READ.
*
     IF       SGK-F09  =  "40"
              GO             TO   900-SGK-READ
     END-IF.
     MOVE     SGK-F01        TO   NEW-SIMEBI.
     MOVE     SGK-F02        TO   NEW-TEN.
     IF       NEW-TEN    NOT =    OLD-TEN
              MOVE      1    TO   TEN-FLG
     ELSE
              MOVE     ZERO  TO   TEN-FLG
     END-IF.
     ADD      1              TO   CNT-READ.
     IF       CNT-READ       =    1
         MOVE      SGK-F01   TO   OLD-SIMEBI
         MOVE      SGK-F02   TO   NEW-TEN
     END-IF.
*
 900-SGK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
