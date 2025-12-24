# SSI8704P

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSI8704P.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　支払照合（ＤＣＭＪＡＰＡＮ）      *
*    モジュール名　　　　：　不照合リスト作成　　　　　　　　　*
*    作成日／更新日　　　：　07/05/29                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払合計ファイルと請求合計ファイル*
*                        ：　を比較して不照合リストを作成する。*
*                            消費税を追加                      *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI8704P.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/29.
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
     SELECT   SIHARASF  ASSIGN         DA-01-S-SIHARASF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SSI-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   SETGK871  ASSIGN         DA-01-VI-SETGK871
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01   SEI-F05
                        STATUS         SEI-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-ST.
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
*----<< 支払合計ファイル >>--*
 FD  SIHARASF           LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  40  RECORDS.
     COPY     SIHARASF  OF        XFDLIB
              JOINING   SSI       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  SETGK871           LABEL RECORD   IS   STANDARD.
     COPY     SETGK871  OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
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
 01  FLAGS.
     03  SSI-END        PIC  9(01)     VALUE  ZERO.
     03  SEI-END        PIC  9(01)     VALUE  ZERO.
     03  INV-FLG        PIC  9(01)     VALUE  ZERO.
     03  CHK-FLG        PIC  X(01)     VALUE  SPACE.
     03  OUT-FLG        PIC  9(01)     VALUE  ZERO.
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03)     VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)     VALUE  ZERO.
     03  IN-CNT         PIC  9(09)     VALUE  ZERO.
*
     03  CNT-7          PIC  9(06)     VALUE  ZERO.
     03  CNT-8          PIC  9(06)     VALUE  ZERO.
     03  CNT-9          PIC  9(06)     VALUE  ZERO.
 01  IDX.
     03  I              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SSI-ST             PIC  X(02).
 01  SEI-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
 01  TOK-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  PG-ID              PIC  X(08)     VALUE   "SSI8704P".
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
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  SSI-KEY.
         05  SSI-DENNO  PIC  X(09)    VALUE  SPACE.
     03  SEI-KEY.
         05  SEI-DENNO  PIC  X(09)    VALUE  SPACE.
*
 01  WK-TORICD          PIC  9(08)    VALUE  ZERO.
*
 01  WK-DENNO-9         PIC  X(09).
 01  WK-DENNO-R         REDEFINES     WK-DENNO-9.
     03  HEN-DENNO-A    PIC  X(03).
     03  HEN-DENNO      PIC  X(06).
*
 01  WK-KINGAKU         PIC S9(09)    VALUE  ZERO.
 01  WK-KINGAKU1        PIC S9(09)    VALUE  ZERO.
*----<< ｶｳﾝﾄｴﾘｱ >>--*
 01  CNT-AREA.
     03  CNT-UNMATCH    PIC  9(05)    VALUE  ZERO.
     03  CNT-OK         PIC  9(05)    VALUE  ZERO.
     03  CNT-SEI        PIC  9(05)    VALUE  ZERO.
     03  CNT-SSI        PIC  9(05)    VALUE  ZERO.
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  X(08)     VALUE  "SSI8704P".
         05  FILLER     PIC  X(35)     VALUE  SPACE.
         05  FILLER     PIC  N(10)     VALUE
                        NC"【　不照合リスト　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(29)     VALUE  SPACE.
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
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  N(06)     VALUE  NC"伝票区分".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店　舗".
         05  FILLER     PIC  X(19)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"伝票_".
         05  FILLER     PIC  X(16)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"支払金額".
         05  FILLER     PIC  X(11)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"請求金額".
         05  FILLER     PIC  X(11)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"差額金額".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(08) VALUE  NC"レコード区分内容".
         05  FILLER     PIC  X(04) VALUE  SPACE.
         05  FILLER     PIC  N(05) VALUE  NC"データ内容".
*
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"取引先：".
         05  HD-031     PIC  9(06).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-032     PIC  N(15).
*
 01  HEAD04.
     03  FILLER         PIC  X(136)    VALUE  ALL "=".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  ME-03      PIC  N(06).
         05  FILLER     PIC  X(05).
         05  ME-05      PIC  9(05).
         05  FILLER     PIC  X(01).
         05  ME-06      PIC  N(10).
         05  FILLER     PIC  X(02).
         05  ME-08      PIC  X(09).
         05  FILLER     PIC  X(04).
         05  ME-09      PIC  ----,---,--9.
         05  FILLER     PIC  X(04).
         05  ME-091     PIC  ----,---,--9.
         05  FILLER     PIC  X(04).
         05  ME-092     PIC  ----,---,--9.
         05  FILLER     PIC  X(02).
         05  ME-10      PIC  N(10).
         05  FILLER     PIC  X(02).
         05  ME-12      PIC  N(10).
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
 01  TAIL01.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(30)     VALUE  SPACE.
         05  FILLER     PIC  N(07)     VALUE
                        NC"　　　　　　　".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  X(17)     VALUE  SPACE.
         05  FILLER     PIC  N(07)     VALUE
                        NC"金額不一致件数".
         05  TA-06      PIC  ZZZ,ZZ9.
*
 LINKAGE                SECTION.
 01  PARA-TORICD        PIC  9(08).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-TORICD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計ファイル >>--*
 SSI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SIHARASF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SIHARASF  ERROR " SSI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 SETGK871-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGK871.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SETGK871  ERROR " SEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " HTENMS   ERROR " TEN-ST " "
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
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*プログラム開始メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*プログラムコントロール
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     SSI-END   =    1
                                   AND SEI-END   =    1.
     PERFORM  300-END-RTN.
*プログラム終了メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*ファイルのオープン
     OPEN     INPUT     SIHARASF.
     OPEN     INPUT     SETGK871.
     OPEN     INPUT     HTENMS.
     OPEN     INPUT     HTOKMS.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     ZERO           TO   CNT-AREA.
     MOVE     ZERO           TO   SSI-END SEI-END.
     MOVE     SPACE          TO   BREAK-KEY.
     MOVE     99             TO   LINE-CNT.
     INITIALIZE         BREAK-KEY.
*
     PERFORM  900-SSI-READ.
     MOVE     SSI-F03        TO   WK-TORICD.
*****DISPLAY "SSI-F01 = " SSI-F01 UPON CONS.
*請求合計Fスタート
     MOVE     SPACE          TO   SEI-REC.
     INITIALIZE                   SEI-REC.
     MOVE     PARA-TORICD    TO   SEI-F01.
     START  SETGK871  KEY  IS  >=  SEI-F01  SEI-F05
            INVALID
            MOVE  HIGH-VALUE TO   SEI-KEY
            MOVE  1          TO   SEI-END
     END-START.
*
     PERFORM  900-SEI-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*****DISPLAY "SSI-KEY = " SSI-KEY UPON CONS.
*****DISPLAY "SEI-KEY = " SEI-KEY UPON CONS.
     EVALUATE  TRUE
*請求データなしチェック
         WHEN      SSI-KEY   <    SEI-KEY
               MOVE     "1"       TO      CHK-FLG
               PERFORM  210-HENSYU-RTN
               PERFORM  900-SSI-READ
*正常データ（金額アンマッチチェック）
         WHEN      SSI-KEY   =    SEI-KEY
               MOVE     "2"       TO      CHK-FLG
               PERFORM  210-HENSYU-RTN
               PERFORM  900-SSI-READ
               PERFORM  900-SEI-READ
*支払情報データなしチェック
         WHEN      SSI-KEY   >    SEI-KEY
               MOVE     "3"       TO      CHK-FLG
               PERFORM  220-HENSYU-RTN
               PERFORM  900-SEI-READ
     END-EVALUATE.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*    テイル印刷
     PERFORM  TAIL-RTN.
*ファイルのクローズ
     CLOSE    SIHARASF.
     CLOSE    SETGK871.
     CLOSE    HTENMS.
     CLOSE    HTOKMS.
     CLOSE    PRTF.
*データ内容メッセージ
     DISPLAY "ｾｲｷｭｳﾃﾞｰﾀ ﾅｼ = "  CNT-SEI     UPON CONS.
     DISPLAY "ｼﾊﾗｲ ﾃﾞｰﾀ ﾅｼ = "  CNT-SSI     UPON CONS.
     DISPLAY "ｷﾝｶﾞｸﾁｶﾞｲ ﾅｼ = "  CNT-UNMATCH UPON CONS.
     DISPLAY "OKﾃﾞｰﾀ       = "  CNT-OK      UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 210-HEAD-PRINT         SECTION.
*ＨＥＡＤ印字
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
     END-IF.
*
     ADD      1              TO   PAGE-CNT.
     MOVE     SYS-YY         TO   HD-011.
     MOVE     SYS-MM         TO   HD-012.
     MOVE     SYS-DD         TO   HD-013.
     MOVE     PAGE-CNT       TO   HD-02.
     MOVE     PARA-TORICD    TO   HD-031.
*取引先名取得
     MOVE     PARA-TORICD    TO   TOK-F01.
     PERFORM  900-TOK-READ.
     MOVE     TOK-F02        TO   HD-032.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     MOVE     7              TO   LINE-CNT.
*
 210-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      明細行印字                                  *
*--------------------------------------------------------------*
 210-HENSYU-RTN        SECTION.
*ＨＥＡＤ印字
     IF       LINE-CNT  >    56
              PERFORM   210-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
*伝票区分名称セット
     EVALUATE SSI-F06
         WHEN  "*"
              MOVE  NC"未請求　"       TO   ME-03
         WHEN  "#"
              MOVE  NC"違算　　"       TO   ME-03
         WHEN  "ﾋ"
              MOVE  NC"非課税　"       TO   ME-03
         WHEN OTHER
              MOVE  SPACE              TO   ME-03
     END-EVALUATE.
     MOVE     SSI-F01                  TO   TEN-F52.
*店舗コード
     MOVE     SSI-F02                  TO   ME-05  TEN-F011.
*店舗名称
     PERFORM  900-TEN-READ.
     IF       TEN-F03  =  SPACE
              MOVE      ALL NC"＊"     TO   ME-06
     ELSE
              MOVE      TEN-F03        TO   ME-06
     END-IF.
*伝票番号
     MOVE     SSI-F04                  TO   ME-08.
*支払金額
     MOVE     SSI-F05                  TO   WK-KINGAKU.
*レコード区分内容，量販伝票_内容，データ内容
     EVALUATE CHK-FLG
         WHEN "1"
              MOVE    SSI-F05               TO   ME-09
              MOVE    ZERO                  TO   ME-091
              MOVE    SSI-F05               TO   ME-092
              MOVE NC"請求データなし"       TO   ME-10
              ADD     1                     TO   CNT-SEI
              MOVE    0                     TO   OUT-FLG
         WHEN "2"
              IF      SEI-F06  =  WK-KINGAKU
                      IF   SSI-F06  =  "*" OR "#" OR "ﾋ"
                           MOVE  NC"ＮＧデータ"  TO   ME-10
                           IF  SSI-F06 = "*"
                               MOVE  NC"未請求　　"  TO   ME-12
                           END-IF
                           IF  SSI-F06 = "#"
                               MOVE  NC"違算　　　"  TO   ME-12
                           END-IF
                           IF  SSI-F06 = "ﾋ"
                               MOVE  NC"非課税　　"  TO   ME-12
                           END-IF
                           ADD   1               TO   CNT-UNMATCH
                           MOVE  0               TO   OUT-FLG
                      ELSE
                           MOVE  NC"ＯＫデータ"  TO   ME-10
                           ADD   1               TO   CNT-OK
                           MOVE  1               TO   OUT-FLG
                      END-IF
              ELSE
                      MOVE  NC"ＮＧデータ"  TO   ME-10
                      MOVE  NC"金額不一致"  TO   ME-12
                      ADD   1               TO   CNT-UNMATCH
                      MOVE  0               TO   OUT-FLG
              END-IF
              MOVE  WK-KINGAKU      TO   ME-09
              MOVE  SEI-F06         TO   ME-091
              COMPUTE ME-092 = WK-KINGAKU - SEI-F06
     END-EVALUATE.
*明細行印字
     IF       OUT-FLG  =  ZERO
              WRITE  PRT-REC  FROM  MEIS01  AFTER  1
              ADD    1       TO   LINE-CNT
     ELSE
              MOVE   ZERO    TO   OUT-FLG
     END-IF.
*
 210-HENSYU-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      明細行印字（レコード区分＝３）              *
*--------------------------------------------------------------*
 220-HENSYU-RTN        SECTION.
*ＨＥＡＤ印字
     IF       LINE-CNT  >    56
              PERFORM   210-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
*伝票区分名称セット
     EVALUATE SEI-F07
         WHEN  00
              MOVE  NC"支払予定"       TO   ME-03
         WHEN  01
              MOVE  NC"請求違算"       TO   ME-03
         WHEN OTHER
              MOVE  ALL NC"＊"         TO   ME-03
     END-EVALUATE.
     MOVE     SEI-F01                  TO   TEN-F52.
*店舗コード
     MOVE     SEI-F03                  TO   ME-05  TEN-F011.
*店舗名称
     PERFORM  900-TEN-READ.
     IF       TEN-F03  =  SPACE
              MOVE      ALL NC"＊"     TO   ME-06
     ELSE
              MOVE      TEN-F03        TO   ME-06
     END-IF.
*伝票番号
     MOVE     SEI-F05                  TO   ME-08.
*支払金額
     MOVE     ZERO                     TO   ME-09.
     MOVE     SEI-F06                  TO   ME-091.
     COMPUTE  ME-092  =  ZERO  -  SEI-F06.
*レコード区分内容，量販伝票_内容，データ内容
     MOVE     NC"支払データなし"       TO   ME-10.
     ADD      1                        TO   CNT-SSI.
*明細行印字
     WRITE    PRT-REC   FROM      MEIS01    AFTER     1.
     ADD      1         TO        LINE-CNT.
*
 220-HENSYU-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       テイル部印刷                                *
*--------------------------------------------------------------*
 TAIL-RTN               SECTION.
     IF       LINE-CNT  >    56
              PERFORM   210-HEAD-PRINT
     END-IF.
*
     MOVE     CNT-UNMATCH    TO   TA-06.
*
     WRITE    PRT-REC   FROM      TAIL01    AFTER     2.
 TAIL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計Ｆ　　 READ                          *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
     IF       SEI-END   =   1
              GO        TO        900-SEI-READ-EXIT
     END-IF.
     READ     SETGK871   AT  END
              MOVE   HIGH-VALUE   TO   SEI-KEY
              MOVE      1         TO   SEI-END
              GO        TO        900-SEI-READ-EXIT
              NOT AT END
              MOVE   SEI-F05      TO   SEI-KEY
     END-READ.
*指定された取引先のみ
     IF       SEI-F01  >  PARA-TORICD
              MOVE   HIGH-VALUE   TO   SEI-KEY
              MOVE      1         TO   SEI-END
     END-IF.
 900-SEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払合計ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SSI-READ           SECTION.
     IF       SSI-END   =   1
              GO        TO        900-SSI-READ-EXIT
     END-IF.
     READ     SIHARASF
         AT END
              MOVE      HIGH-VALUE     TO   SSI-KEY
              MOVE      1              TO   SSI-END
              GO        TO        900-SSI-READ-EXIT
         NOT AT END
              MOVE      SSI-F04        TO   SSI-KEY
              ADD       1              TO   IN-CNT
     END-READ.
*指定された取引先のみ
     IF       SSI-F01  NOT =  PARA-TORICD
              GO        TO        900-SSI-READ
     END-IF.
*
 900-SSI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     READ     HTENMS    INVALID
              MOVE      SPACE     TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F02
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
