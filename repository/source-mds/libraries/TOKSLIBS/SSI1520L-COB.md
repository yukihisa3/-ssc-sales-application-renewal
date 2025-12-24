# SSI1520L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSI1520L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　セキチュー支払照合　　　　　　　　*
*    モジュール名　　　　：　不照合リスト作成　植物　　　　　　*
*    作成日／更新日　　　：　10/04/21                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払合計ファイルと請求合計ファイル*
*                        ：　を比較して不照合リストを作成する。*
*    新支払データ対応                                          *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI1520L.
 AUTHOR.                OONO.
 DATE-WRITTEN.          10/04/21.
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
     SELECT   SEKSSIF  ASSIGN         DA-01-S-SEKSSIF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SEK-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   HSEIGKF   ASSIGN         DA-01-VI-SEIGKF21
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
 FD  SEKSSIF           LABEL RECORD   IS   STANDARD
                       BLOCK CONTAINS 40   RECORDS.
     COPY     SEKSSIF   OF        XFDLIB
              JOINING   SEK       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  HSEIGKF            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
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
     03  SEK-END        PIC  9(01)     VALUE  ZERO.
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
 01  SEK-ST             PIC  X(02).
 01  SEI-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  PG-ID              PIC  X(08)     VALUE   "SSI1520L".
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
     03  SEK-KEY.
         05  SEK-DENNO  PIC  X(09)    VALUE  SPACE.
     03  SEI-KEY.
         05  SEI-DENNO  PIC  X(09)    VALUE  SPACE.
*
 01  WK-TORICD          PIC  X(06).
 01  WK-TORICD-R        REDEFINES     WK-TORICD.
     03  HEN-TORICD     PIC  9(06).
*
*----<< 取引先変換（６桁用） >>--*
 01  WK-TORI            PIC  X(06).
 01  WK-TORI-R          REDEFINES     WK-TORI.
     03  HEN-TORI       PIC  9(06).
*
*----<< 取引先変換（８桁用） >>--*
 01  WK-TORI-8          PIC  X(08).
 01  WK-TORI-8-R        REDEFINES     WK-TORI-8.
     03  HEN-TORI-8     PIC  9(08).
*
*----<< 店舗変換 >>--*
 01  WK-TEN             PIC  X(05).
 01  WK-TEN-R           REDEFINES     WK-TEN.
     03  HEN-TEN        PIC  9(05).
*
*----<< 金額データ変換 >>--*
 01  WK-SIHARAI         PIC  X(09).
 01  WK-SIHARAI-R       REDEFINES     WK-SIHARAI.
     03  HEN-SIHARAI    PIC  9(09).
 01  WK-KINGAKU         PIC S9(09).
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
         05  FILLER     PIC  X(42)     VALUE  SPACE.
         05  FILLER     PIC  N(13)     VALUE
                        NC"【　不照合リスト　植物　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(26)     VALUE  SPACE.
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
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  N(06)     VALUE  NC"伝票区分".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店　舗".
         05  FILLER     PIC  X(17)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"検".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"収".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"伝票_".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"請求金額".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(08) VALUE  NC"レコード区分内容".
         05  FILLER     PIC  X(04) VALUE  SPACE.
         05  FILLER     PIC  N(08) VALUE  NC"訂正時元伝票_".
         05  FILLER     PIC  X(05) VALUE  SPACE.
         05  FILLER     PIC  N(05) VALUE  NC"データ内容".
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  ME-03      PIC  N(06).
         05  FILLER     PIC  X(05).
         05  ME-04      PIC  9(08).
         05  FILLER     PIC  X(02).
         05  ME-05      PIC  9(05).
         05  FILLER     PIC  X(01).
         05  ME-06      PIC  N(10).
         05  FILLER     PIC  X(02).
         05  ME-071     PIC  Z9.
         05  ME-071P    PIC  X(01).
         05  ME-072     PIC  Z9.
         05  ME-072P    PIC  X(01).
         05  ME-073     PIC  Z9.
         05  FILLER     PIC  X(02).
         05  ME-08      PIC  X(09).
         05  FILLER     PIC  X(01).
         05  ME-09      PIC  ----,---,--9.
         05  FILLER     PIC  X(02).
         05  ME-10      PIC  N(10).
         05  FILLER     PIC  X(02).
         05  ME-11      PIC  X(09).
         05  FILLER     PIC  X(07).
         05  ME-12      PIC  N(10).
 01  MEIS02.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(90)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"相殺伝票用".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  ME-13      PIC  N(16).
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
 01  TAIL01.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(10)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"区分ー７".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  TA-01      PIC  ZZZ,ZZ9.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"区分ー８".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  TA-02      PIC  ZZZ,ZZ9.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"区分ー９".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  TA-03      PIC  ZZZ,ZZ9.
         05  FILLER     PIC  X(13)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"総件数".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  TA-04      PIC  ZZZ,ZZ9.
 01  TAIL02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(30)     VALUE  SPACE.
         05  FILLER     PIC  N(07)     VALUE
                        NC"　　　　　　　".
*********05  TA-05      PIC  ZZZ,ZZ9.
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  X(17)     VALUE  SPACE.
         05  FILLER     PIC  N(07)     VALUE
                        NC"金額不一致件数".
         05  TA-06      PIC  ZZZ,ZZ9.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計ファイル >>--*
 SEK-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SEKSSIF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID " SEKSSIF ERROR " SEK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 HSEIGKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSEIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID " HSEIGKF  ERROR " SEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
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
     PERFORM  200-MAIN-RTN   UNTIL     SEK-END   =    1
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
     OPEN     INPUT     SEKSSIF.
     OPEN     INPUT     HSEIGKF.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     ZERO           TO   CNT-AREA.
     MOVE     ZERO           TO   SEK-END SEI-END.
     MOVE     SPACE          TO   BREAK-KEY.
     MOVE     99             TO   LINE-CNT.
     INITIALIZE         BREAK-KEY.
*
     PERFORM  900-SEK-READ.
     MOVE     926061         TO   WK-TORICD.
*****PERFORM  900-SEI-ST-READ.
     PERFORM  900-SEI-READ.
*****DISPLAY "SEK-END = " SEK-END UPON CONS.
*****DISPLAY "SEI-END = " SEI-END UPON CONS.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     EVALUATE  TRUE
*請求データなしチェック
         WHEN      SEK-KEY   <    SEI-KEY
               MOVE     "1"       TO      CHK-FLG
               PERFORM  210-HENSYU-RTN
               PERFORM  900-SEK-READ
*正常データ（金額アンマッチチェック）
         WHEN      SEK-KEY   =    SEI-KEY
               MOVE     "2"       TO      CHK-FLG
               PERFORM  210-HENSYU-RTN
               PERFORM  900-SEK-READ
               PERFORM  900-SEI-READ
*支払情報データなしチェック
         WHEN      SEK-KEY   >    SEI-KEY
*********DISPLAY "SEK-KEY = " SEK-KEY UPON CONS
*********DISPLAY "SEI-KEY = " SEI-KEY UPON CONS
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
     CLOSE    SEKSSIF.
     CLOSE    HSEIGKF.
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
     MOVE     SYS-YY         TO   HD-011.
     MOVE     SYS-MM         TO   HD-012.
     MOVE     SYS-DD         TO   HD-013.
     MOVE     PAGE-CNT       TO   HD-02.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     2.
     WRITE    PRT-REC   FROM      P-SPACE   AFTER     1.
     MOVE     6              TO   LINE-CNT.
*
 210-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      明細行印字（レコード区分＝１，２）          *
*--------------------------------------------------------------*
 210-HENSYU-RTN        SECTION.
*ＨＥＡＤ印字
     IF       LINE-CNT  >    63
              PERFORM   210-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
*伝票区分名称セット
     EVALUATE SEK-F07
         WHEN "51"
              MOVE  NC"定番仕入"       TO  ME-03
         WHEN "52"
              MOVE  NC"特販仕入"       TO  ME-03
         WHEN "53"
              MOVE  NC"スポット仕入"   TO  ME-03
         WHEN "54"
              MOVE  NC"客注仕入"       TO  ME-03
         WHEN "55"
              MOVE  NC"返品"           TO  ME-03
         WHEN "56"
              MOVE  NC"値引"           TO  ME-03
         WHEN "57"
              MOVE  NC"リベート"       TO  ME-03
         WHEN "58"
              MOVE  NC"相殺"           TO  ME-03
     END-EVALUATE.
*取引先コード
     MOVE     926061                   TO   WK-TORI.
     MOVE     HEN-TORI                 TO   ME-04  TEN-F52.
*店舗コード
     MOVE     SEK-F05                  TO   ME-05  TEN-F011.
*店舗名称
     PERFORM   900-TEN-READ.
     IF        TEN-F03  =  SPACE
               MOVE      ALL NC"＊"    TO   ME-06
     ELSE
               MOVE      TEN-F03       TO   ME-06
     END-IF.
*検収日（編集）
     MOVE     SEK-F08                  TO   WK-DATE.
     MOVE     WK-YY                    TO   ME-071.
     MOVE     "."                      TO   ME-071P.
     MOVE     WK-MM                    TO   ME-072.
     MOVE     "."                      TO   ME-072P.
     MOVE     WK-DD                    TO   ME-073.
*伝票番号
     MOVE     SEK-F04                  TO   ME-08.
*支払金額
     MOVE     SEK-F10                  TO   ME-09.
*レコード区分内容，量販伝票_内容，データ内容
     EVALUATE CHK-FLG
         WHEN "1"
              MOVE NC"請求データなし"       TO   ME-10
              ADD     1                     TO   CNT-SEI
         WHEN "2"
              IF      SEI-F06  =  WK-KINGAKU
                      MOVE  NC"ＯＫデータ"  TO   ME-10
                      ADD   1               TO   CNT-OK
                      MOVE  0               TO   OUT-FLG
              ELSE
                      MOVE  NC"ＮＧデータ"  TO   ME-10
                      MOVE  NC"金額不一致"  TO   ME-12
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
*****ADD      1              TO   LINE-CNT.
*
 210-HENSYU-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      明細行印字（レコード区分＝３）              *
*--------------------------------------------------------------*
 220-HENSYU-RTN        SECTION.
*ＨＥＡＤ印字
     IF       LINE-CNT  >    63
              PERFORM   210-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
*伝票区分名称セット
     EVALUATE SEI-F08
         WHEN "51"
              MOVE  NC"定番仕入　　"   TO  ME-03
         WHEN "52"
              MOVE  NC"特販仕入　　"   TO  ME-03
         WHEN "53"
              MOVE  NC"スポット仕入"   TO  ME-03
         WHEN "54"
              MOVE  NC"客注仕入　　"   TO  ME-03
         WHEN "55"
              MOVE  NC"返品　　　　"   TO  ME-03
         WHEN "56"
              MOVE  NC"値引　　　　"   TO  ME-03
         WHEN "57"
              MOVE  NC"リベート　　"   TO  ME-03
         WHEN "58"
              MOVE  NC"相殺　　　　"   TO  ME-03
         WHEN OTHER
              MOVE  NC"　　　　　　"   TO  ME-03
     END-EVALUATE.
*取引先コード
     MOVE     SEI-F01                  TO   WK-TORI-8.
     MOVE     HEN-TORI-8               TO   ME-04  TEN-F52.
*店舗コード
*****MOVE     SEI-F03                  TO   WK-TEN.
*****MOVE     HEN-TEN                  TO   ME-05  TEN-F11.
     MOVE     SEI-F03                  TO   ME-05  TEN-F011.
*****DISPLAY "TEN-F52 = " TEN-F52 UPON CONS.
*****DISPLAY "TEN-F011 = " TEN-F011 UPON CONS.
*店舗名称
     PERFORM   900-TEN-READ.
     IF        TEN-F03  =  SPACE
               MOVE      ALL NC"＊"    TO   ME-06
     ELSE
               MOVE      TEN-F03       TO   ME-06
     END-IF.
*****DISPLAY "TEN-F03 = " TEN-F03 UPON CONS.
*検収日（編集）
     MOVE     SEI-F04                  TO   WK-DATE.
     MOVE     WK-YY                    TO   ME-071.
     MOVE     "."                      TO   ME-071P.
     MOVE     WK-MM                    TO   ME-072.
     MOVE     "."                      TO   ME-072P.
     MOVE     WK-DD                    TO   ME-073.
*伝票番号
     MOVE     SEI-F05                  TO   ME-08.
*支払金額
     MOVE     SEI-F06                  TO   ME-09.
*レコード区分内容，量販伝票_内容，データ内容
     MOVE NC"支払データなし"           TO   ME-10.
     ADD     1                         TO   CNT-SSI.
*明細行印字
     WRITE    PRT-REC        FROM MEIS01    AFTER     1.
     ADD      1              TO   LINE-CNT.
*
 220-HENSYU-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       テイル部印刷                                *
*--------------------------------------------------------------*
 TAIL-RTN               SECTION.
     IF       LINE-CNT  >    63
              PERFORM   210-HEAD-PRINT
     END-IF.
*
     MOVE     CNT-UNMATCH    TO   TA-06.
*
     WRITE    PRT-REC   FROM      TAIL02    AFTER     2.
 TAIL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計Ｆ　　 READ                          *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
     IF       SEI-END   =   1
              GO        TO        900-SEI-READ-EXIT
     END-IF.
     READ     HSEIGKF   AT  END
              MOVE   HIGH-VALUE   TO   SEI-KEY
              MOVE      1         TO   SEI-END
              NOT AT END
              MOVE   SEI-F05      TO   SEI-KEY
     END-READ.
     IF       HEN-TORICD  <  SEI-F01
              MOVE   HIGH-VALUE   TO   SEI-KEY
              MOVE      1         TO   SEI-END
     END-IF.
 900-SEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払合計ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SEK-READ           SECTION.
     IF       SEK-END   =   1
**************DISPLAY "SEK-END(1) = " SEK-END UPON CONS
              GO        TO        900-SEK-READ-EXIT
     END-IF.
     READ     SEKSSIF
         AT END
              MOVE  HIGH-VALUE    TO   SEK-KEY
              MOVE      1         TO   SEK-END
**************DISPLAY "SEK-END(2) = " SEK-END UPON CONS
         NOT AT END
              MOVE   SEK-F04      TO   SEK-KEY
              ADD       1         TO   IN-CNT
     END-READ.
*
 900-SEK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計ファイルスタート                     *
*--------------------------------------------------------------*
*900-SEI-ST-READ           SECTION.
****
**** MOVE     SEK-F25             TO   WK-TORI.
**** MOVE     HEN-TORI            TO   SEI-F01.
**** MOVE     SEK-F21             TO   SEI-F05.
**** START    HSEIGKF  KEY  IS  >=     SEI-F01 SEI-F05
****          INVALID
****          MOVE      1         TO   SEI-END
**** END-START.
****
*900-SEI-ST-READ-EXIT.
*    EXIT.
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
