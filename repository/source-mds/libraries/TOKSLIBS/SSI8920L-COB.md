# SSI8920L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSI8920L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　支払照合                          *
*    モジュール名　　　　：　不照合リスト作成　　　　　　　　　*
*    作成日／更新日　　　：　08/04/16                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払合計ファイルと請求合計ファイル*
*                        ：　を比較して不照合リストを作成する。*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI8920L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/04/16.
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
*----<< 支払明細ファイル >>--*
     SELECT   SKTSIHF   ASSIGN         DA-01-VI-SKTSIHL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SIH-F01   SIH-F07
                        STATUS         SIH-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   SETGK89   ASSIGN         DA-01-VI-SETGK891
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
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払合計ファイル >>--*
 FD  SKTSIHF            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  1   RECORDS.
     COPY     SKTSIHF   OF        XFDLIB
              JOINING   SIH       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  SETGK89            LABEL RECORD   IS   STANDARD.
     COPY     SETGK89   OF        XFDLIB
              JOINING   SEI       PREFIX.
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
     03  SIH-END        PIC  9(01)     VALUE  ZERO.
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
 01  SIH-ST             PIC  X(02).
 01  SEI-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  PG-ID              PIC  X(08)     VALUE   "SSI8920L".
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
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
     03  SIH-KEY.
         05  SIH-DENNO  PIC  X(09)    VALUE  SPACE.
     03  SEI-KEY.
         05  SEI-DENNO  PIC  X(09)    VALUE  SPACE.
 01  HEN-DENNO-0.
     03  HEN-DENNO-1    PIC  X(02)    VALUE  SPACE.
     03  HEN-DENNO-2    PIC  X(07)    VALUE  SPACE.
 01  WK-DENPYO          PIC  X(09)    VALUE  SPACE.
*
 01  WK-TORICD          PIC  9(08)    VALUE  ZERO.
*
 01  WK-DENNO-9         PIC  9(09).
 01  WK-DENNO-R         REDEFINES     WK-DENNO-9.
     03  HEN-DENNO      PIC  X(09).
*
 01  WK-KINGAKU         PIC S9(09)    VALUE  ZERO.
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
         05  FILLER     PIC  X(53)     VALUE  SPACE.
         05  FILLER     PIC  N(12)     VALUE
                        NC"＊＊　不照合リスト　＊＊".
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(32)     VALUE  SPACE.
         05  HD-011     PIC  9999.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  HD-014     PIC  ZZ9.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
 01  HEAD02.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"買掛締日付：".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-021     PIC  99.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-022     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD-023     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
*
 01  HEAD03.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"店舗コード".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗名".
         05  FILLER     PIC  X(27)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票番号".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"検収日".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"請求金額".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"支払金額".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"データ内容".
*
 01  HEAD04.
     03  FILLER         PIC  X(136)    VALUE  ALL "-".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(06).
         05  ME-01      PIC  ZZZZ9.
         05  FILLER     PIC  X(06).
         05  ME-02      PIC  N(15).
         05  FILLER     PIC  X(03).
         05  ME-03      PIC  999999999.
         05  FILLER     PIC  X(03).
         05  ME-YY      PIC  99.
         05  ME-NEN     PIC  N(01).
         05  ME-MM      PIC  Z9.
         05  ME-TUKI    PIC  N(01).
         05  ME-DD      PIC  Z9.
         05  ME-DAY     PIC  N(01).
         05  FILLER     PIC  X(03).
         05  ME-04      PIC  ---,---,--9.
         05  FILLER     PIC  X(04).
         05  ME-05      PIC  ---,---,--9.
         05  FILLER     PIC  X(03).
         05  ME-06      PIC  N(15).
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
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
*----<< 支払合計ファイル >>--*
 SIH-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SKTSIHF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SKTSIHF  ERROR " SIH-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 SETGK89-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGK89.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SETGK89  ERROR " SEI-ST " "
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
     PERFORM  200-MAIN-RTN   UNTIL     SIH-END   =    1
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
     OPEN     INPUT     SKTSIHF.
     OPEN     INPUT     SETGK89.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     ZERO           TO   CNT-AREA.
     MOVE     ZERO           TO   SIH-END SEI-END.
     MOVE     SPACE          TO   BREAK-KEY.
     MOVE     99             TO   LINE-CNT.
     INITIALIZE         BREAK-KEY.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
     PERFORM  900-SIH-READ.
     MOVE     SIH-F01        TO   WK-TORICD.
     PERFORM  900-SEI-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*    IF  CNT-SEI < 50
*        DISPLAY "SIH-KEY = " SIH-KEY.
*        DISPLAY "SEI-KEY = " SEI-KEY.
*    END-IF.
     EVALUATE  TRUE
*請求データなしチェック
         WHEN      SIH-KEY   <    SEI-KEY
               MOVE     "1"       TO      CHK-FLG
               PERFORM  210-HENSYU-RTN
               PERFORM  900-SIH-READ
*正常データ（金額アンマッチチェック）
         WHEN      SIH-KEY   =    SEI-KEY
               MOVE     "2"       TO      CHK-FLG
               PERFORM  210-HENSYU-RTN
               PERFORM  900-SIH-READ
               PERFORM  900-SEI-READ
*支払情報データなしチェック
         WHEN      SIH-KEY   >    SEI-KEY
               MOVE     "3"       TO      CHK-FLG
               PERFORM  220-HENSYU-RTN
               PERFORM  900-SEI-READ
     END-EVALUATE.
*    DISPLAY   "CHK-FLG = " CHK-FLG.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルのクローズ
     CLOSE    SKTSIHF.
     CLOSE    SETGK89.
     CLOSE    HTENMS.
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
     MOVE     SYS-DATEW(1:4) TO   HD-011.
     MOVE     SYS-DATEW(5:2) TO   HD-012.
     MOVE     SYS-DATEW(7:2) TO   HD-013.
     MOVE     PAGE-CNT       TO   HD-014.
*
     MOVE     SIH-F13(3:2)   TO   HD-021.
     MOVE     SIH-F13(5:2)   TO   HD-022.
     MOVE     SIH-F13(7:2)   TO   HD-023.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     MOVE     8              TO   LINE-CNT.
*
 210-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      明細行印字                                  *
*--------------------------------------------------------------*
 210-HENSYU-RTN        SECTION.
*ＨＥＡＤ印字
     IF       LINE-CNT  >    62
              PERFORM   210-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
     MOVE     SIH-F01                  TO   TEN-F52.
*店舗コード
     MOVE     SIH-F03                  TO   ME-01  TEN-F011.
*店舗名称
     PERFORM  900-TEN-READ.
     IF       TEN-F03  =  SPACE
              MOVE      ALL NC"＊"     TO   ME-02
     ELSE
              MOVE      TEN-F03        TO   ME-02
     END-IF.
*伝票番号
     MOVE     SIH-KEY                  TO   ME-03.
*検収日　
     MOVE     SIH-F04(3:2)             TO   ME-YY.
     MOVE     NC"年"                   TO   ME-NEN.
     MOVE     SIH-F04(5:2)             TO   ME-MM.
     MOVE     NC"月"                   TO   ME-TUKI.
     MOVE     SIH-F04(7:2)             TO   ME-DD.
     MOVE     NC"日"                   TO   ME-DAY.
*請求金額
     MOVE     SEI-F06        TO   ME-04.
*支払金額
     MOVE     SIH-F08        TO   ME-05.
*レコード区分内容，量販伝票_内容，データ内容
     EVALUATE CHK-FLG
         WHEN "1"
              MOVE NC"請求データなし"       TO   ME-06
              ADD     1                     TO   CNT-SEI
         WHEN "2"
              IF      SEI-F06  =  SIH-F08
                      MOVE  NC"ＯＫデータ"  TO   ME-06
                      ADD   1               TO   CNT-OK
                      MOVE  1               TO   OUT-FLG
              ELSE
*                     MOVE  NC"ＮＧデータ"  TO   ME-06
                      MOVE  NC"金額不一致"  TO   ME-06
                      ADD   1               TO   CNT-UNMATCH
              END-IF
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
     IF       LINE-CNT  >    62
              PERFORM   210-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
*
     MOVE     SEI-F01                  TO   TEN-F52.
*店舗コード
     MOVE     SEI-F03                  TO   ME-01  TEN-F011.
*店舗名称
     PERFORM  900-TEN-READ.
     IF       TEN-F03  =  SPACE
              MOVE      ALL NC"＊"     TO   ME-02
     ELSE
              MOVE      TEN-F03        TO   ME-02
     END-IF.
*伝票番号
     MOVE     SEI-KEY                  TO   ME-03.
*検収日　
     MOVE     SEI-F04(1:2)             TO   ME-YY.
     MOVE     NC"年"                   TO   ME-NEN.
     MOVE     SEI-F04(3:2)             TO   ME-MM.
     MOVE     NC"月"                   TO   ME-TUKI.
     MOVE     SEI-F04(5:2)             TO   ME-DD.
     MOVE     NC"日"                   TO   ME-DAY.
*請求金額
     MOVE     SEI-F06                  TO   ME-04.
*支払金額
     MOVE     SIH-F08                  TO   ME-05.
*レコード区分内容，量販伝票_内容，データ内容
     IF       CHK-FLG  = "3"
              MOVE     NC"支払データなし"   TO   ME-06
              ADD      1                    TO   CNT-SSI
     END-IF.
*明細行印字
     WRITE    PRT-REC   FROM      MEIS01    AFTER     1.
     ADD      1         TO        LINE-CNT.
*
 220-HENSYU-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計Ｆ　　 READ                          *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
     IF       SEI-END   =   1
              GO        TO        900-SEI-READ-EXIT
     END-IF.
     READ     SETGK89   AT  END
              MOVE   HIGH-VALUE   TO   SEI-KEY
              MOVE      1         TO   SEI-END
              NOT AT END
**************MOVE   SEI-F05      TO   WK-DENNO-9
**************MOVE   HEN-DENNO    TO   SEI-KEY
              MOVE   SEI-F05      TO   SEI-KEY
     END-READ.
*    IF       WK-TORICD   <  SEI-F01
*             MOVE   HIGH-VALUE   TO   SEI-KEY
*             MOVE      1         TO   SEI-END
*    END-IF.
 900-SEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払合計ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SIH-READ           SECTION.
     IF       SIH-END   =   1
              GO        TO        900-SIH-READ-EXIT
     END-IF.
     READ     SKTSIHF
         AT END
              MOVE      HIGH-VALUE     TO   SIH-KEY
              MOVE      1              TO   SIH-END
         NOT AT END
*             伝票番号
              MOVE      SIH-F07        TO   WK-DENNO-9
              MOVE      HEN-DENNO      TO   WK-DENPYO
              MOVE      WK-DENPYO      TO   SIH-KEY
              ADD       1              TO   IN-CNT
     END-READ.
*
 900-SIH-READ-EXIT.
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
*-----------------<< PROGRAM END >>----------------------------*

```
