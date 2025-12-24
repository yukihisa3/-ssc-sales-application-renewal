# SSY3773L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3773L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　ナフコ　支払照合                  *
*    モジュール名　　　　：　不照合リスト作成　　　　　　　　　*
*    作成日／更新日　　　：　2010/10/21                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：                                    *
*      支払合計ファイルと請求合計ファイルを比較して不照合      *
*      リストを作成する。                                      *
*      支払明細ファイルは伝票番号順でファイル内がユニークに    *
*      なっていることを想定する。                              *
*      請求合計ファイルは取引先ＣＤ、伝票番号順でファイル内    *
*      がユニークになっていることを想定する。                  *
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY3773L.
 AUTHOR.                S.I.
 DATE-WRITTEN.          2010/10/21.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
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
     SELECT   NFSIHAF  ASSIGN          DA-01-VI-NFSIHAL3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SIH-F06 *> 伝票番号
                        STATUS         SIH-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   SETGKFT   ASSIGN         DA-01-VI-SETGKFA3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01 *> 取引先ＣＤ
                                       SEI-F05 *> 伝票番号
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
*----<< 支払明細ファイル >>--*
 FD  NFSIHAF           LABEL RECORD   IS   STANDARD
                       BLOCK CONTAINS  4   RECORDS.
     COPY     NFSIHAF  OF        XFDLIB
              JOINING  SIH       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  SETGKFT            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFT   OF        XFDLIB
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
 01  PG-ID              PIC  X(08)     VALUE   "SSY3773L".
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
     05  SIH-KEY        PIC  9(08)  VALUE  ZERO.
     05  SEI-KEY-X.
       07  SEI-KEY      PIC  9(09)  VALUE  ZERO.
*
 01  WK-TORICD          PIC  9(08) VALUE  ZERO.
 01  BR-TORCD           PIC  9(08) VALUE ZERO.
 01  BR-TENCD           PIC  9(05) VALUE ZERO.
*
*伝票番号変換（文字⇒数値）
 01  WK-DENNO-9         PIC  X(09).
 01  WK-DENNO-R         REDEFINES     WK-DENNO-9.
     03  HEN-DENNO-A    PIC  9(09).
*
 01  WK-KINGAKU         PIC S9(09)  VALUE  ZERO.
 01  WK-TENNM           PIC  N(15)  VALUE SPACE.
*----<< ｶｳﾝﾄｴﾘｱ >>--*
 01  CNT-AREA.
     03  CNT-UNMATCH    PIC  9(05)    VALUE  ZERO.
     03  CNT-OK         PIC  9(05)    VALUE  ZERO.
     03  CNT-SEI        PIC  9(05)    VALUE  ZERO.
     03  CNT-SSI        PIC  9(05)    VALUE  ZERO.
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER  CHARACTER TYPE MODE-1.
       05  FILLER      PIC  X(08)  VALUE "SSY3773L".
       05  FILLER      PIC  X(49)  VALUE SPACE.
       05  FILLER      PIC  N(11)  VALUE
           NC"不　照　合　リ　ス　ト".
       05  FILLER      PIC  X(24)  VALUE SPACE.
       05  FILLER      PIC  N(03)  VALUE NC"処理日".
       05  FILLER      PIC  X(01)  VALUE ":".
       05  HD-011      PIC  9(04).
       05  FILLER      PIC  N(01)  VALUE NC"年".
       05  HD-012      PIC  Z9.
       05  FILLER      PIC  N(01)  VALUE NC"月".
       05  HD-013      PIC  Z9.
       05  FILLER      PIC  N(01)  VALUE NC"日".
       05  FILLER      PIC  X(03)  VALUE SPACE.
       05  HD-02       PIC  ZZ9.
       05  FILLER      PIC  N(01)  VALUE NC"頁".
 01  HEAD02.
     03  FILLER  CHARACTER TYPE MODE-1.
         05  FILLER    PIC  X(03) VALUE  SPACE.
         05  FILLER    PIC  N(05) VALUE  NC"店舗コード".
         05  FILLER    PIC  X(03) VALUE  SPACE.
         05  FILLER    PIC  N(03) VALUE  NC"店舗名".
         05  FILLER    PIC  X(27) VALUE  SPACE.
         05  FILLER    PIC  N(04) VALUE  NC"伝票番号".
         05  FILLER    PIC  X(06) VALUE  SPACE.
         05  FILLER    PIC  N(03) VALUE  NC"検収日".
         05  FILLER    PIC  X(08) VALUE  SPACE.
         05  FILLER    PIC  N(04) VALUE  NC"請求金額".
         05  FILLER    PIC  X(07) VALUE  SPACE.
         05  FILLER    PIC  N(04) VALUE  NC"支払金額".
         05  FILLER    PIC  X(04) VALUE  SPACE.
         05  FILLER    PIC  N(05) VALUE  NC"データ内容".
*
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-1.
         05  FILLER     PIC  N(04)     VALUE  NC"取引先：".
         05  HD-032     PIC  N(15).
*
 01  HEAD04.
     05  FILLER         PIC  X(135)  VALUE  ALL "-".
*
 01  MEISAI.
     03  FILLER  CHARACTER TYPE MODE-1.
       05  FILLER           PIC  X(05).
       05  MEI-TENCD        PIC  9(05).
       05  FILLER           PIC  X(06).
       05  MEI-TENNM        PIC  N(15).
       05  FILLER           PIC  X(03).
       05  MEI-DENNO-X.
         07  MEI-DENNO      PIC  9(08).
       05  FILLER           PIC  X(03).
       05  MEI-KENSY-Y      PIC  99.
       05  MEI-KENSY-YN     PIC  N(01).
       05  MEI-KENSY-M      PIC  Z9.
       05  MEI-KENSY-MN     PIC  N(01).
       05  MEI-KENSY-D      PIC  Z9.
       05  MEI-KENSY-DN     PIC  N(01).
       05  FILLER           PIC  X(03).
       05  MEI-SEIKY-GK     PIC  ---,---,--9.
       05  FILLER           PIC  X(04).
       05  MEI-SIHARAI-GK   PIC  ---,---,--9.
       05  FILLER           PIC  X(03).
       05  MEI-DT-NAIYO     PIC  N(10).


 01  P-SPACE                PIC  X(01)  VALUE SPACE.

*日付変換サブルーチン用ワーク
 01  SKYDTCKB-AREA.
     03  SKYDTCKB-IN-KBN          PIC  X(01).
     03  SKYDTCKB-IN-YMD6         PIC  9(06).
     03  SKYDTCKB-IN-YMD8         PIC  9(08).
     03  SKYDTCKB-OUT-RET         PIC  X(01).
     03  SKYDTCKB-OUT-YMD         PIC  9(08).
 01  DATE-AREA.
     03  SYS-DATE8                PIC  9(08)  VALUE  ZERO.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計ファイル >>--*
 NFSIHAF-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFSIHAF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " NFSIHAF ERROR " SIH-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 SETGKFT-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGKFT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SETGKFT  ERROR " SEI-ST " "
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
     PERFORM  200-MAIN-RTN   UNTIL     SIH-END   =    9
                                   AND SEI-END   =    9.
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
* ファイルのオープン
     OPEN  INPUT  NFSIHAF.
     OPEN  INPUT  SETGKFT.
     OPEN  INPUT  HTENMS.
     OPEN  OUTPUT PRTF.
* << ﾜｰｸ ｼｮｷｾｯﾄ >>
     INITIALIZE  FLAGS.
     INITIALIZE  COUNTERS.
     MOVE  ZERO             TO  CNT-AREA.
     MOVE  SPACE            TO  BREAK-KEY.
     MOVE  99               TO  LINE-CNT.
     INITIALIZE  BREAK-KEY.

* システム日付取得
     ACCEPT  WK-DATE  FROM DATE.
     MOVE  "3"              TO  SKYDTCKB-IN-KBN.
     MOVE  WK-DATE          TO  SKYDTCKB-IN-YMD6.
     MOVE  ZERO             TO  SKYDTCKB-IN-YMD8.
     MOVE  ZERO             TO  SKYDTCKB-OUT-RET.
     MOVE  ZERO             TO  SKYDTCKB-OUT-YMD.
     CALL  "SKYDTCKB"  USING SKYDTCKB-IN-KBN
                             SKYDTCKB-IN-YMD6
                             SKYDTCKB-IN-YMD8
                             SKYDTCKB-OUT-RET
                             SKYDTCKB-OUT-YMD.
     MOVE  SKYDTCKB-OUT-YMD TO  SYS-DATE8.

     MOVE  ZERO             TO  SIH-END.
     PERFORM  900-SIHARAI-READ.

     MOVE  1                TO  SEI-END.
     MOVE  137607           TO  SEI-F01. *> 取引先ＣＤ
     MOVE  ZERO             TO  SEI-F05. *> 伝票番号
     PERFORM  900-SEIKY-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払合計ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SIHARAI-READ           SECTION.
     IF  SIH-END = 9
         GO TO  900-SIHARAI-READ-EXIT
     END-IF.

     READ  NFSIHAF
       AT END
         MOVE  99999999     TO  SIH-KEY
         MOVE  9            TO  SIH-END
       NOT AT END
         MOVE  SIH-F06      TO  SIH-KEY
         ADD  1  TO IN-CNT
     END-READ.

 900-SIHARAI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計Ｆ　　 READ                          *
*--------------------------------------------------------------*
 900-SEIKY-READ           SECTION.
     IF  SEI-END = 9
         GO TO  900-SEIKY-READ-EXIT
     END-IF.

     IF  SEI-END = 1
         START  SETGKFT  KEY >= SEI-F01 *> 取引先ＣＤ
                                SEI-F05 *> 伝票番号
           INVALID
             MOVE  999999999  TO  SEI-KEY
             MOVE  9          TO  SEI-END
             GO TO  900-SEIKY-READ-EXIT
         END-START

         MOVE  ZERO         TO  SEI-END

     END-IF.

     READ  SETGKFT
       AT END
         MOVE  999999999    TO  SEI-KEY
         MOVE  9            TO  SEI-END
         GO TO  900-SEIKY-READ-EXIT
     END-READ.

     IF  SEI-F01 > 137607 *> ナフコ取引先コード
         MOVE  999999999    TO  SEI-KEY
         MOVE  9            TO  SEI-END
         GO TO  900-SEIKY-READ-EXIT
     END-IF.

     MOVE  SEI-F05          TO  SEI-KEY-X.

 900-SEIKY-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.

     EVALUATE  TRUE
       WHEN  SIH-KEY < SEI-KEY
                     *> 請求データなしチェック
         MOVE  "1"          TO  CHK-FLG
         PERFORM  210-EDWT-SIHARAI-RTN
         PERFORM  900-SIHARAI-READ

       WHEN  SIH-KEY = SEI-KEY
                     *> 正常データ（金額アンマッチチェック）
         MOVE  "2"          TO  CHK-FLG
         PERFORM  210-EDWT-SIHARAI-RTN
         PERFORM  900-SIHARAI-READ
         PERFORM  900-SEIKY-READ

       WHEN  SIH-KEY > SEI-KEY
                     *> 支払情報データなしチェック
         MOVE  "3"          TO  CHK-FLG
         PERFORM  210-EDWT-SEIKY
         PERFORM  900-SEIKY-READ
     END-EVALUATE.

 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      明細行印字（比較状態区分＝１，２）          *
*--------------------------------------------------------------*
 210-EDWT-SIHARAI-RTN        SECTION.
* ＨＥＡＤ印字
     IF  LINE-CNT > 62
         PERFORM  210-HEAD-PRINT
     END-IF.

     MOVE  SPACE            TO  MEISAI.
     MOVE  SIH-F01          TO  MEI-TENCD.   *> 店舗コード
     IF     SIH-F03  NOT = BR-TORCD
         OR SIH-F01  NOT = BR-TENCD
         MOVE  SIH-F03      TO  BR-TORCD
         MOVE  SIH-F01      TO  BR-TENCD
         MOVE  SIH-F01      TO  TEN-F011
         MOVE  SIH-F03      TO  TEN-F52
         PERFORM  900-TEN-READ
         IF  TEN-F02 = SPACE
             MOVE  ALL NC"＊"    TO  WK-TENNM *>店舗名称
         ELSE
             MOVE  TEN-F02       TO  WK-TENNM *>店舗名称
         END-IF
     END-IF.

     MOVE  WK-TENNM         TO  MEI-TENNM *>店舗名称
     MOVE  SIH-KEY          TO  MEI-DENNO.   *> 伝票番号
     MOVE  SIH-F08 (3:2)    TO  MEI-KENSY-Y. *> 検収日
     MOVE  NC"年"           TO  MEI-KENSY-YN.
     MOVE  SIH-F08 (5:2)    TO  MEI-KENSY-M.
     MOVE  NC"月"           TO  MEI-KENSY-MN.
     MOVE  SIH-F08 (7:2)    TO  MEI-KENSY-D.
     MOVE  NC"日"           TO  MEI-KENSY-DN.

     EVALUATE  CHK-FLG
       WHEN  "1"
         MOVE  ZERO             TO  MEI-SEIKY-GK   *> 請求額
         MOVE  SIH-F13          TO  MEI-SIHARAI-GK *> 支払額
         MOVE  NC"請求データなし"  TO  MEI-DT-NAIYO
         ADD  1  TO  CNT-SEI

       WHEN  "2"
         MOVE  SEI-F06             TO  MEI-SEIKY-GK   *> 請求額
         MOVE  SIH-F13             TO  MEI-SIHARAI-GK *> 支払額
         IF  SIH-F13 = SEI-F06 *> 請求金額と支払金額の比較
             MOVE  NC"ＯＫデータ"  TO  MEI-DT-NAIYO
             ADD  1  TO  CNT-OK
             MOVE  1               TO  OUT-FLG
         ELSE
             MOVE  NC"金額不一致"  TO  MEI-DT-NAIYO
             ADD  1  TO  CNT-UNMATCH
         END-IF
     END-EVALUATE.

* 明細行印字
     IF  OUT-FLG = ZERO
         WRITE  PRT-REC  FROM MEISAI  AFTER 1
         ADD  1  TO  LINE-CNT
     ELSE
         MOVE  ZERO         TO  OUT-FLG
     END-IF.

 210-EDWT-SIHARAI-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 210-HEAD-PRINT         SECTION.
* ＨＥＡＤ印字
     IF  PAGE-CNT > 0
         WRITE  PRT-REC  FROM P-SPACE  AFTER PAGE
     END-IF.

     ADD  1  TO  PAGE-CNT.
     MOVE  SYS-DATE8 (1:4)  TO  HD-011.
     MOVE  SYS-DATE8 (5:2)  TO  HD-012.
     MOVE  SYS-DATE8 (7:2)  TO  HD-013.
     MOVE  PAGE-CNT         TO  HD-02.
* 取引先名
     MOVE  NC"株式会社ナフコ"  TO  HD-032.

     WRITE  PRT-REC   FROM HEAD01  AFTER 2.
     WRITE  PRT-REC   FROM HEAD03  AFTER 2.
     WRITE  PRT-REC   FROM HEAD04  AFTER 1.
     WRITE  PRT-REC   FROM HEAD02  AFTER 1.
     WRITE  PRT-REC   FROM HEAD04  AFTER 1.
     MOVE  7                TO LINE-CNT.

 210-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      明細行印字（比較状態区分＝３）              *
*--------------------------------------------------------------*
 210-EDWT-SEIKY        SECTION.
* ＨＥＡＤ印字
     IF  LINE-CNT > 62
         PERFORM  210-HEAD-PRINT
     END-IF.

     MOVE  SPACE            TO  MEISAI.

     MOVE  SEI-F03          TO  MEI-TENCD. *> 店舗コード

     IF     SEI-F01  NOT = BR-TORCD
         OR SEI-F03  NOT = BR-TENCD
         MOVE  SEI-F01      TO  BR-TORCD
         MOVE  SEI-F03      TO  BR-TENCD
         MOVE  SEI-F03      TO  TEN-F011
         MOVE  SEI-F01      TO  TEN-F52
         PERFORM  900-TEN-READ
         IF  TEN-F02 = SPACE
             MOVE  ALL NC"＊"    TO  WK-TENNM *>店舗名称
         ELSE
             MOVE  TEN-F02       TO  WK-TENNM *>店舗名称
         END-IF
     END-IF.

     MOVE  WK-TENNM         TO  MEI-TENNM. *>店舗名称
     MOVE  SEI-KEY          TO  MEI-DENNO. *> 伝票番号
     MOVE  SEI-F04 (1:2)    TO  MEI-KENSY-Y. *> 検収日
     MOVE  NC"年"           TO  MEI-KENSY-YN.
     MOVE  SEI-F04 (3:2)    TO  MEI-KENSY-M.
     MOVE  NC"月"           TO  MEI-KENSY-MN.
     MOVE  SEI-F04 (5:2)    TO  MEI-KENSY-D.
     MOVE  NC"日"           TO  MEI-KENSY-DN.
     MOVE  SEI-F06          TO  MEI-SEIKY-GK.   *> 請求額
     MOVE  ZERO             TO  MEI-SIHARAI-GK. *> 支払額
     MOVE  NC"支払データなし"  TO  MEI-DT-NAIYO. *> データ内容

     ADD  1  TO  CNT-SSI.

* 明細行印字
     WRITE  PRT-REC  FROM MEISAI  AFTER 1.
     ADD  1  TO  LINE-CNT.

 210-EDWT-SEIKY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     READ  HTENMS
       INVALID
         MOVE SPACE         TO  TEN-F02
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
* ファイルのクローズ
     CLOSE  NFSIHAF.
     CLOSE  SETGKFT.
     CLOSE  HTENMS.
     CLOSE  PRTF.
* データ内容メッセージ
     DISPLAY "ｾｲｷｭｳﾃﾞｰﾀ ﾅｼ = "  CNT-SEI     UPON CONS.
     DISPLAY "ｼﾊﾗｲ ﾃﾞｰﾀ ﾅｼ = "  CNT-SSI     UPON CONS.
     DISPLAY "ｷﾝｶﾞｸﾁｶﾞｲ ﾅｼ = "  CNT-UNMATCH UPON CONS.
     DISPLAY "OKﾃﾞｰﾀ       = "  CNT-OK      UPON CONS.

 300-END-RTN-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
