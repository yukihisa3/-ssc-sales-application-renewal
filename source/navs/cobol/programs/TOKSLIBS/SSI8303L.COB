****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　支払照合（ホーマックＷｅｂ）      *
*    モジュール名　　　　：　支払差異明細書作成　　　　　　　　*
*    作成日／更新日　　　：　06/11/06                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払合計ファイルより支払差異明細　*
*                        ：　書を作成する。　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI8303L.
 AUTHOR.                Y.Y.
 DATE-WRITTEN.          06/11/06.
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
         YB        IS   YB
         YB-21     IS   YB-21
         YA-22     IS   YA-22
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払情報データ >>--*
     SELECT   SITME832  ASSIGN         DA-01-VI-SITME832
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SIH-F01  SIH-F09
                                       SIH-F02  SIH-F03
                                       SIH-F04  SIH-F05
                        STATUS         SIH-ST.
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
 FD  SITME832           LABEL RECORD   IS   STANDARD.
     COPY     SITME83   OF        XFDLIB
              JOINING   SIH       PREFIX.
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
     03  READ-CNT       PIC  9(07).
 01  FLGS.
     03  END-FLG        PIC  X(03)     VALUE  SPACE.
     03  HTENMS-INV-FLG PIC  X(03)     VALUE  SPACE.
 01  IDX.
     03  I              PIC  9(03).
 01  ID-PROGRAM.
     03  PG-ID          PIC  X(08)     VALUE  "SSI8303L".
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SIH-ST             PIC  X(02).
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
*
 01  SIHARAI-JYOUHO.
     03  WK-TORICD      PIC  9(08)    VALUE  ZERO.
     03  WK-TENCD       PIC  9(08)    VALUE  ZERO.
     03  WK-TENCD1      PIC  9(08)    VALUE  ZERO.
     03  WK-TENPO-KEI1  PIC S9(12)    VALUE  ZERO.
     03  WK-TENPO-KEI2  PIC S9(12)    VALUE  ZERO.
     03  WK-TENPO-KEI3  PIC S9(12)    VALUE  ZERO.
     03  WK-TENPO-KEI4  PIC S9(12)    VALUE  ZERO.
     03  WK-JIGYO-KEI1  PIC S9(12)    VALUE  ZERO.
     03  WK-JIGYO-KEI2  PIC S9(12)    VALUE  ZERO.
     03  WK-JIGYO-KEI3  PIC S9(12)    VALUE  ZERO.
     03  WK-JIGYO-KEI4  PIC S9(12)    VALUE  ZERO.
     03  WK-SOUGK-KEI1  PIC S9(12)    VALUE  ZERO.
     03  WK-SOUGK-KEI2  PIC S9(12)    VALUE  ZERO.
     03  WK-SOUGK-KEI3  PIC S9(12)    VALUE  ZERO.
     03  WK-SOUGK-KEI4  PIC S9(12)    VALUE  ZERO.
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-TOKCD  PIC  9(08).
     03  OLD.
         05  OLD-TOKCD  PIC  9(08).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD01.
     03  FILLER         CHARACTER  TYPE  IS  YA.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"事業部名：".
         05  HD01-JIGYO PIC  N(10).
         05  FILLER     PIC  X(24)     VALUE  SPACE.
     03  FILLER         CHARACTER  TYPE  IS  YB-21.
         05  FILLER     PIC  N(11)     VALUE
                        NC"＊＊支払差異明細書＊＊".
         05  FILLER     PIC  X(34)     VALUE  SPACE.
     03  FILLER         CHARACTER  TYPE  IS  YA.
         05  HD01-YYYY  PIC  9999.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD01-MM    PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD01-DD    PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD01-PAGE  PIC  ZZ9.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
*
 01  HD02.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"締　　日：".
         05  HD02-YYYY  PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD02-MM    PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD02-DD    PIC  Z9.
*線１
 01  SEN01.
     03  FILLER         PIC  X(136)    VALUE  ALL "=".
*線２
 01  SEN02.
     03  FILLER         PIC  X(37)     VALUE  SPACE.
     03  FILLER         PIC  X(99)     VALUE  ALL "-".
*線３
 01  SEN03.
     03  FILLER         PIC  X(136)    VALUE  ALL "-".
*
 01  HD03.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"店舗情報".
         05  FILLER     PIC  X(21)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"納".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"品".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"伝票_".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"伝票明細金額".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"伝票合計金額".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"請求金額".
         05  FILLER     PIC  X(11)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"差額".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"摘　要".
*
 01  MS01.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-TENCD PIC  ZZZZ9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-TENNM PIC  N(10).
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-YYYY  PIC  9999.
         05  MS01-KU1   PIC  X(01).
         05  MS01-MM    PIC  Z9.
         05  MS01-KU2   PIC  X(01).
         05  MS01-DD    PIC  Z9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-DENNO PIC  9(07).
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-SKIN1 PIC  ---,---,---,--9.
         05  MS01-SKIN2 PIC  ---,---,---,--9.
         05  MS01-SKIN3 PIC  ---,---,---,--9.
         05  MS01-SKIN4 PIC  ---,---,---,--9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-TEKIY PIC  N(10).
 01  MS02.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(37)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"＃店舗合計＃".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS02-TGKIN1 PIC  ---,---,---,--9.
         05  MS02-TGKIN2 PIC  ---,---,---,--9.
         05  MS02-TGKIN3 PIC  ---,---,---,--9.
         05  MS02-TGKIN4 PIC  ---,---,---,--9.
 01  MS03.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(37)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"＃事業部計＃".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS03-JGKIN1 PIC  ---,---,---,--9.
         05  MS03-JGKIN2 PIC  ---,---,---,--9.
         05  MS03-JGKIN3 PIC  ---,---,---,--9.
         05  MS03-JGKIN4 PIC  ---,---,---,--9.
 01  MS04.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(37)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"＃総合計　＃".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS04-SGKIN1 PIC  ---,---,---,--9.
         05  MS04-SGKIN2 PIC  ---,---,---,--9.
         05  MS04-SGKIN3 PIC  ---,---,---,--9.
         05  MS04-SGKIN4 PIC  ---,---,---,--9.
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD8           PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-SIMEBI        PIC  9(08).
 01  PARA-TORICD        PIC  9(08).
****************************************************************
 PROCEDURE              DIVISION USING PARA-SIMEBI PARA-TORICD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払情報データ >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SITME832.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  SITME832  ERROR " SIH-ST " "
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
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG = "END".
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
     OPEN     INPUT     SITME832.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    PRTF.
*
     DISPLAY "SIMEBI = " PARA-SIMEBI  UPON CONS.
     DISPLAY "TORICD = " PARA-TORICD  UPON CONS.
*
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
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     MOVE     99             TO   LINE-CNT.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
*支払情報データスタート
     MOVE     SPACE          TO   SIH-REC.
     INITIALIZE                   SIH-REC.
     MOVE     PARA-SIMEBI    TO   SIH-F01.
     MOVE     "2"            TO   SIH-F09.
     MOVE     PARA-TORICD    TO   SIH-F02.
     START  SITME832  KEY  IS  >=  SIH-F01  SIH-F09  SIH-F02
                                   SIH-F03  SIH-F04  SIH-F05
            INVALID
            DISPLAY NC"＃＃対象データ無し＃＃" UPON CONS
     END-START.
*支払情報データ初期読込み
     PERFORM  900-SIT-READ.
*
     IF   END-FLG NOT = "END"
          MOVE  SIH-F02      TO   WK-TORICD
          MOVE  SIH-F03      TO   WK-TENCD
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*取引先ＣＤブレイク
     IF    WK-TORICD  NOT =  SIH-F02
***********店舗計出力
           PERFORM   TENKEI-WT-SEC
***********事業部計出力
           PERFORM   JIGYOB-WT-SEC
           MOVE    SIH-F02   TO   WK-TORICD
           MOVE    SIH-F03   TO   WK-TENCD
           MOVE    99        TO   LINE-CNT
     END-IF.
*店舗ＣＤブレイク
     IF    WK-TENCD   NOT =  SIH-F03
***********店舗計出力
           PERFORM   TENKEI-WT-SEC
           MOVE    SIH-F03   TO   WK-TENCD
     END-IF.
*明細
     PERFORM  MEISAI-WT-SEC.
*
     PERFORM  900-SIT-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
*    総合計出力
     IF       PAGE-CNT       >    ZERO
              PERFORM   TENKEI-WT-SEC
              PERFORM   JIGYOB-WT-SEC
              PERFORM   SOKEI-WT-SEC
     END-IF.
*ファイルのクローズ
     CLOSE    SITME832.
     CLOSE    HTENMS.
     CLOSE    PRTF.
*
     DISPLAY "* SITGKFF (IN)=" READ-CNT " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" PAGE-CNT " *" UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ﾒｲｻｲ ｲﾝｻﾂ（明細行出力）                      *
*--------------------------------------------------------------*
 MEISAI-WT-SEC          SECTION.
*
     IF       LINE-CNT  >    60
              PERFORM   HEAD-WT-SEC
     END-IF.
*
     MOVE     SPACE               TO   MS01.
*店舗情報取得
     IF   WK-TENCD1  NOT =  SIH-F03
*         店舗コード
          MOVE     SIH-F03        TO   MS01-TENCD
*         店舗名
          MOVE     SIH-F03        TO   WK-TENCD1
          MOVE     SIH-F02        TO   TEN-F52
          MOVE     SIH-F03        TO   TEN-F011
          PERFORM  900-TEN-READ
          IF       HTENMS-INV-FLG = "INV"
                   MOVE      ALL NC"＊"     TO   MS01-TENNM
          ELSE
                   MOVE      TEN-F03        TO   MS01-TENNM
          END-IF
     END-IF.
*納品日
     MOVE     SIH-F04(1:4)        TO  MS01-YYYY.
     MOVE     "/"                 TO  MS01-KU1.
     MOVE     SIH-F04(5:2)        TO  MS01-MM.
     MOVE     "/"                 TO  MS01-KU2.
     MOVE     SIH-F04(7:2)        TO  MS01-DD.
*伝票番号
     MOVE     SIH-F05             TO  MS01-DENNO.
*伝票明細金額
     MOVE     SIH-F11             TO  MS01-SKIN1.
     ADD      SIH-F11             TO  WK-TENPO-KEI1.
     ADD      SIH-F11             TO  WK-JIGYO-KEI1.
     ADD      SIH-F11             TO  WK-SOUGK-KEI1.
*伝票合計金額
     MOVE     SIH-F13             TO  MS01-SKIN2.
     ADD      SIH-F13             TO  WK-TENPO-KEI2.
     ADD      SIH-F13             TO  WK-JIGYO-KEI2.
     ADD      SIH-F13             TO  WK-SOUGK-KEI2.
*請求金額
     MOVE     SIH-F15             TO  MS01-SKIN3.
     ADD      SIH-F15             TO  WK-TENPO-KEI3.
     ADD      SIH-F15             TO  WK-JIGYO-KEI3.
     ADD      SIH-F15             TO  WK-SOUGK-KEI3.
*伝票明細金額
     MOVE     SIH-F17             TO  MS01-SKIN4.
     ADD      SIH-F17             TO  WK-TENPO-KEI4.
     ADD      SIH-F17             TO  WK-JIGYO-KEI4.
     ADD      SIH-F17             TO  WK-SOUGK-KEI4.
*摘要
     EVALUATE SIH-F18
         WHEN 1     MOVE NC"違算"   TO  MS01-TEKIY
         WHEN 2     MOVE NC"未納品" TO  MS01-TEKIY
         WHEN OTHER MOVE SPACE      TO  MS01-TEKIY
     END-EVALUATE.
*
     WRITE    PRT-REC   FROM MS01    AFTER     1.
     ADD      1         TO   LINE-CNT.
*
 210-MEIS01-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      店舗計出力                                  *
*--------------------------------------------------------------*
 TENKEI-WT-SEC       SECTION.
*
     IF       LINE-CNT  >    60
              PERFORM   HEAD-WT-SEC
     END-IF.
*
     MOVE     WK-TENPO-KEI1  TO   MS02-TGKIN1.
     MOVE     WK-TENPO-KEI2  TO   MS02-TGKIN2.
     MOVE     WK-TENPO-KEI3  TO   MS02-TGKIN3.
     MOVE     WK-TENPO-KEI4  TO   MS02-TGKIN4.
     WRITE    PRT-REC        FROM SEN02     AFTER     1.
     WRITE    PRT-REC        FROM MS02      AFTER     1.
     WRITE    PRT-REC        FROM SEN03     AFTER     1.
     ADD      3              TO   LINE-CNT.
     MOVE     ZERO           TO   WK-TENPO-KEI1.
     MOVE     ZERO           TO   WK-TENPO-KEI2.
     MOVE     ZERO           TO   WK-TENPO-KEI3.
     MOVE     ZERO           TO   WK-TENPO-KEI4.
*
 TENKEI-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      店舗計出力                                  *
*--------------------------------------------------------------*
 JIGYOB-WT-SEC       SECTION.
*
     IF       LINE-CNT  >    60
              PERFORM   HEAD-WT-SEC
     END-IF.
*
     MOVE     WK-JIGYO-KEI1  TO   MS03-JGKIN1.
     MOVE     WK-JIGYO-KEI2  TO   MS03-JGKIN2.
     MOVE     WK-JIGYO-KEI3  TO   MS03-JGKIN3.
     MOVE     WK-JIGYO-KEI4  TO   MS03-JGKIN4.
     WRITE    PRT-REC        FROM SEN02     AFTER     1.
     WRITE    PRT-REC        FROM MS03      AFTER     1.
     WRITE    PRT-REC        FROM SEN03     AFTER     1.
     ADD      3              TO   LINE-CNT.
     MOVE     ZERO           TO   WK-JIGYO-KEI1.
     MOVE     ZERO           TO   WK-JIGYO-KEI2.
     MOVE     ZERO           TO   WK-JIGYO-KEI3.
     MOVE     ZERO           TO   WK-JIGYO-KEI4.
*
 JIGYOB-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5      総合計出力                                  *
*--------------------------------------------------------------*
 SOKEI-WT-SEC           SECTION.
*
     IF       LINE-CNT  >    60
              PERFORM   HEAD-WT-SEC
     END-IF.
*
     MOVE     WK-SOUGK-KEI1  TO   MS04-SGKIN1.
     MOVE     WK-SOUGK-KEI2  TO   MS04-SGKIN2.
     MOVE     WK-SOUGK-KEI3  TO   MS04-SGKIN3.
     MOVE     WK-SOUGK-KEI4  TO   MS04-SGKIN4.
     WRITE    PRT-REC        FROM MS04      AFTER     2.
*
 SOKEI-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ（ＨＥＡＤプリント）          *
*--------------------------------------------------------------*
 HEAD-WT-SEC           SECTION.
*改頁制御
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*ページカウンター
     ADD      1                   TO   PAGE-CNT.
*事業部名称
     EVALUATE SIH-F02
         WHEN 880    MOVE NC"（資材）関東事業部"   TO HD01-JIGYO
         WHEN 882    MOVE NC"（資材）東北事業部"   TO HD01-JIGYO
         WHEN 883    MOVE NC"（資材）北海道事業部" TO HD01-JIGYO
         WHEN 1427   MOVE NC"（植物）関東事業部"   TO HD01-JIGYO
         WHEN 14272  MOVE NC"（植物）東北事業部"   TO HD01-JIGYO
         WHEN 14273  MOVE NC"（植物）北海道事業部" TO HD01-JIGYO
         WHEN OTHER  MOVE NC"＊＊＊＊＊＊＊＊＊＊" TO HD01-JIGYO
     END-EVALUATE.
*システム日付
     MOVE     SYS-YYW             TO   HD01-YYYY.
     MOVE     SYS-MMW             TO   HD01-MM.
     MOVE     SYS-DDW             TO   HD01-DD.
*頁
     MOVE     PAGE-CNT            TO   HD01-PAGE.
*システム日付
     MOVE     SIH-F01(1:4)        TO   HD02-YYYY.
     MOVE     SIH-F01(5:2)        TO   HD02-MM.
     MOVE     SIH-F01(7:2)        TO   HD02-DD.
*ＨＥＡＤ出力
     WRITE    PRT-REC   FROM      HD01      AFTER     2.
     WRITE    PRT-REC   FROM      HD02      AFTER     1.
     WRITE    PRT-REC   FROM      SEN01     AFTER     2.
     WRITE    PRT-REC   FROM      HD03      AFTER     1.
     WRITE    PRT-REC   FROM      SEN01     AFTER     1.
     MOVE     7         TO        LINE-CNT.
     MOVE     ZERO      TO        WK-TENCD1.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     READ     HTENMS    INVALID
              MOVE      "INV"          TO   HTENMS-INV-FLG
              NOT  INVALID
              MOVE      SPACE          TO   HTENMS-INV-FLG
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払合計データ　　 READ                      *
*--------------------------------------------------------------*
 900-SIT-READ           SECTION.
     READ     SITME832  NEXT   AT   END
              MOVE     "END"   TO   END-FLG
              GO   TO   900-SIT-READ-EXIT
              NOT  AT  END
              ADD       1      TO   READ-CNT
     END-READ.
*
     IF   READ-CNT(5:3)  =  "000" OR "500"
          DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*締日のチェック
     IF    PARA-SIMEBI NOT = SIH-F01
              MOVE     "END"   TO   END-FLG
              GO   TO   900-SIT-READ-EXIT
     END-IF.
*発注／差異区分
*****IF    SIH-F09  =  "2"
*****         MOVE     "END"   TO   END-FLG
*****         GO   TO   900-SIT-READ-EXIT
*****END-IF.
*取引先ＣＤチェック
     IF    PARA-TORICD  =  ZERO
              GO   TO   900-SIT-READ-EXIT
     ELSE
           IF  PARA-TORICD = SIH-F02
               CONTINUE
           ELSE
              MOVE     "END"   TO   END-FLG
           END-IF
     END-IF.
*
 900-SIT-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
