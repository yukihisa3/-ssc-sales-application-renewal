# SSI8910L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSI8910L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　支払照合（リック）　　　　　      *
*    モジュール名　　　　：　支払明細表作成　　　　　　　　　　*
*    作成日／更新日　　　：　08/04/16                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払合計ファイルより支払明細表を　*
*                        ：　印刷する。　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI8910L.
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
         YA        IS   YA
         YB        IS   YB
         YB-21     IS   YB-21
         YA-22     IS   YA-22
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払明細データ >>--*
     SELECT   SKTSIHF   ASSIGN         DA-01-VI-SKTSIHL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SIH-F11  SIH-F01
                                       SIH-F02  SIH-F03
                                       SIH-F04
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
 FD  SKTSIHF           LABEL RECORD   IS   STANDARD.
     COPY     SKTSIHF   OF        XFDLIB
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
     03  SIH-FLG        PIC  X(01)     VALUE  SPACE.
     03  HTENMS-INV-FLG PIC  X(03)     VALUE  SPACE.
 01  IDX.
     03  I              PIC  9(03).
 01  ID-PROGRAM.
     03  PG-ID          PIC  X(08)     VALUE  "SSI8910L".
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SIH-ST             PIC  X(02).
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
     03  WK-SIHKBN      PIC  X(01)    VALUE  ZERO.
     03  WK-TENPO-KEI   PIC S9(09)    VALUE  ZERO.
     03  WK-SIHGK-KEI   PIC S9(09)    VALUE  ZERO.
     03  WK-SOSAI-KEI   PIC S9(09)    VALUE  ZERO.
     03  WK-SOUGK-KEI   PIC S9(09)    VALUE  ZERO.
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
     03  FILLER         CHARACTER  TYPE  IS  YB-21.
         05  FILLER     PIC  X(45)     VALUE  SPACE.
         05  FILLER     PIC  N(11)     VALUE
                        NC"＊＊　支払明細書　＊＊".
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
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"取引先：".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD02-TORCD PIC  999999999.
 01  HD03.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"買掛締日付：".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD03-YY    PIC  99.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD03-MM    PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD03-DD    PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
 01  HD04.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(07)     VALUE NC"（買掛締期間：".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD04-YYS   PIC  99.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD04-MMS   PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD04-DDS   PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  N(01)     VALUE  NC"～".
         05  HD04-YYE   PIC  99.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD04-MME   PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD04-DDE   PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
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
 01  HD05.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"店舗コード".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗名".
         05  FILLER     PIC  X(29)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票区分".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票番号".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"検収日".
         05  FILLER     PIC  X(12)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"金額".
*
 01  HD06.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(48)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"【相殺明細】".
*
 01  MS01.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-TENCD PIC  ZZZZ9.
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  MS01-TENNM PIC  N(15).
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  MS01-DENKB PIC  99.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-DENKM PIC  N(02).
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  MS01-DENNO PIC  99999999.
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  MS01-YY    PIC  99.
         05  MS01-NEN   PIC  N(01)     VALUE  NC"年".
         05  MS01-MM    PIC  Z9.
         05  MS01-TUK   PIC  N(01)     VALUE  NC"月".
         05  MS01-DD    PIC  Z9.
         05  MS01-DAY   PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  MS01-SKIN  PIC  ---,---,--9.
 01  MS02.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(84)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"＃店舗計＃".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-TGKIN PIC  ---,---,--9.
 01  MS03.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(84)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"＃支払計＃".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS03-SIKIN PIC  ---,---,--9.
 01  MS04.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(50)     VALUE  SPACE.
         05  MS04-SOSCD PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS04-SOSME PIC  N(15).
         05  FILLER     PIC  X(10)     VALUE  SPACE.
         05  MS04-SOSKN PIC  ---,---,--9.
 01  MS05.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(83)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"＃相殺計＃".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS05-SOKEI PIC  ---,---,--9.
 01  MS06.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(83)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"＃総合計＃".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS06-SGKIN PIC  ---,---,--9.
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
*----<< 支払明細データ >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SKTSIHF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  SKTSIHF  ERROR " SIH-ST " "
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
*
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
     OPEN     INPUT     SKTSIHF.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    PRTF.
*
*    DISPLAY "SIMEBI = " PARA-SIMEBI  UPON CONS.
*    DISPLAY "TORICD = " PARA-TORICD  UPON CONS.
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
*    MOVE     LOW-VALUE      TO   BREAK-KEY.
*支払明細データスタート
*    MOVE     SPACE          TO   SIH-REC.
*    INITIALIZE                   SIH-REC.
*    MOVE     PARA-SIMEBI    TO   SIH-F02.
*    MOVE     PARA-TORICD    TO   SIH-F01.
*    START  SKTSIHF  KEY  IS  >=  SIH-F11  SIH-F01  SIH-F02
*                                  SIH-F03  SIH-F04
*           INVALID
*           DISPLAY NC"＃＃対象データ無し＃＃" UPON CONS
*    END-START.
*支払情報データ初期読込み
     PERFORM  900-SIH-READ.
*
     IF   END-FLG NOT = "END"
          MOVE  SIH-F01      TO   WK-TORICD
          MOVE  SIH-F03      TO   WK-TENCD
          MOVE  SIH-F11      TO   WK-SIHKBN
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*取引先ＣＤブレイク
     IF    WK-TORICD  NOT =  SIH-F01
***********店舗計出力
           PERFORM   TENKEI-WT-SEC
***********支払計出力
           IF    WK-SIHKBN  NOT =  "2"
                 PERFORM   SIHKEI-WT-SEC
           END-IF
           MOVE    SIH-F01   TO   WK-TORICD
           MOVE    SIH-F03   TO   WK-TENCD
           MOVE    99        TO   LINE-CNT
     END-IF.
*
*店舗ＣＤブレイク
     IF    WK-TENCD   NOT =  SIH-F03
******     店舗計出力
           PERFORM   TENKEI-WT-SEC
           MOVE    SIH-F03   TO   WK-TENCD
     END-IF.
*支払区分ブレイク
     IF    WK-SIHKBN  NOT =  SIH-F11
           MOVE    SIH-F11   TO   WK-SIHKBN
           IF    ( WK-SIHKBN  =  "1" )
           AND   ( LINE-CNT   NOT =  99 )
******     支払計出力
                 PERFORM   SIHKEI-WT-SEC
**               MOVE    SIH-F11   TO   WK-SIHKBN
           END-IF
     END-IF.
*
*明細
     PERFORM  MEISAI-WT-SEC.
*
*支区＝２の時、相殺セット
     IF    WK-SIHKBN  =  "2"
*******    相殺明細出力
           MOVE   "1"  TO   SIH-FLG
           PERFORM   SOSAI-WT-SEC
     END-IF.
*
     PERFORM  900-SIH-READ.
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
         IF       WK-SIHKBN  =  2
              PERFORM   SOSKEI-WT-SEC
         END-IF
              PERFORM   SOKEI-WT-SEC
     END-IF.
*ファイルのクローズ
     CLOSE    SKTSIHF.
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
**********MOVE     SIH-F02        TO   TEN-F52
          MOVE     1994           TO   TEN-F52
          MOVE     SIH-F03        TO   TEN-F011
          PERFORM  900-TEN-READ
          IF       HTENMS-INV-FLG = "INV"
                   MOVE      ALL NC"＊"     TO   MS01-TENNM
          ELSE
                   MOVE      TEN-F03        TO   MS01-TENNM
          END-IF
     END-IF.
*伝票区分
     MOVE     SIH-F05(1:2)        TO  MS01-DENKB.
     EVALUATE SIH-F05(1:2)
         WHEN "10" MOVE NC"ＴＡ" TO  MS01-DENKM
         WHEN "20" MOVE NC"手書" TO  MS01-DENKM
         WHEN "30" MOVE NC"返品" TO  MS01-DENKM
     END-EVALUATE.
*伝票番号
     MOVE     SIH-F07             TO  MS01-DENNO.
*検収日　
     MOVE     SIH-F04(3:2)        TO  MS01-YY.
     MOVE     NC"年"              TO  MS01-NEN.
     MOVE     SIH-F04(5:2)        TO  MS01-MM.
     MOVE     NC"月"              TO  MS01-TUK.
     MOVE     SIH-F04(7:2)        TO  MS01-DD.
     MOVE     NC"日"              TO  MS01-DAY.
*支払金額
     MOVE     SIH-F08             TO  MS01-SKIN.
     ADD      SIH-F08             TO  WK-TENPO-KEI.
     ADD      SIH-F08             TO  WK-SIHGK-KEI.
**   ADD      SIH-F07             TO  WK-SOUGK-KEI.
*
     WRITE    PRT-REC   FROM MS01    AFTER     1.
     ADD      1         TO   LINE-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     相殺明細出力　　　　　　　                   *
*--------------------------------------------------------------*
 SOSAI-WT-SEC           SECTION.
*
     IF       LINE-CNT  >    60
              PERFORM   HEAD-WT-SEC
     END-IF.
*
     IF  SIH-FLG = "1"
         WRITE    PRT-REC        FROM HD06      AFTER     2
         ADD      2          TO   LINE-CNT
         MOVE     SPACE      TO   SIH-FLG
     END-IF.
*相殺コード
**** IF   WK-TENCD1  NOT =  SIH-F03
     MOVE     SIH-F09        TO   MS04-SOSCD.
*相殺名称
     MOVE     SIH-F10        TO   MS04-SOSME.
*相殺金額
     MOVE     SIH-F08             TO  MS04-SOSKN.
     ADD      SIH-F08             TO  WK-SOSAI-KEI.
*
     WRITE    PRT-REC   FROM MS04    AFTER     1.
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
     MOVE     WK-TENPO-KEI   TO   MS02-TGKIN.
     WRITE    PRT-REC        FROM MS02      AFTER     2.
     ADD      2              TO   LINE-CNT.
     MOVE     ZERO           TO   WK-TENPO-KEI.
*
 TENKEI-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      支払計出力                                  *
*--------------------------------------------------------------*
 SIHKEI-WT-SEC       SECTION.
*
     IF       LINE-CNT  >    60
              PERFORM   HEAD-WT-SEC
     END-IF.
*
     MOVE     WK-SIHGK-KEI   TO   MS03-SIKIN.
     WRITE    PRT-REC        FROM MS03      AFTER     1.
     ADD      1              TO   LINE-CNT.
*    MOVE     ZERO           TO   WK-SIHGK-KEI.
*
 SIHKEI-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      相殺計出力                                  *
*--------------------------------------------------------------*
 SOSKEI-WT-SEC       SECTION.
*
     IF       LINE-CNT  >    60
              PERFORM   HEAD-WT-SEC
     END-IF.
*
     MOVE     WK-SOSAI-KEI   TO   MS05-SOKEI.
     WRITE    PRT-REC        FROM MS05      AFTER     2.
     ADD      2              TO   LINE-CNT.
     MOVE     ZERO           TO   WK-SOSAI-KEI.
*
 SOSKEI-WT-EXIT.
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
     COMPUTE  WK-SOUGK-KEI = WK-SIHGK-KEI - WK-SOSAI-KEI.
*
     MOVE     WK-SOUGK-KEI   TO   MS06-SGKIN.
     WRITE    PRT-REC        FROM MS06      AFTER     1.
     ADD      1              TO   LINE-CNT.
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
*システム日付
     MOVE     SYS-YYW             TO   HD01-YYYY.
     MOVE     SYS-MMW             TO   HD01-MM.
     MOVE     SYS-DDW             TO   HD01-DD.
*頁
     MOVE     PAGE-CNT            TO   HD01-PAGE.
*取引先ＣＤ
     MOVE     SIH-F01             TO   HD02-TORCD.
*買掛締日付
     MOVE     SIH-F13(3:2)        TO   HD03-YY.
     MOVE     SIH-F13(5:2)        TO   HD03-MM.
     MOVE     SIH-F13(7:2)        TO   HD03-DD.
*買掛締日付開始
     MOVE     SIH-F12(3:2)        TO   HD04-YYS.
     MOVE     SIH-F12(5:2)        TO   HD04-MMS.
     MOVE     SIH-F12(7:2)        TO   HD04-DDS.
*買掛締日付終了
     MOVE     SIH-F13(3:2)        TO   HD04-YYE.
     MOVE     SIH-F13(5:2)        TO   HD04-MME.
     MOVE     SIH-F13(7:2)        TO   HD04-DDE.
*ＨＥＡＤ出力
     WRITE    PRT-REC   FROM      HD01      AFTER     1.
     WRITE    PRT-REC   FROM      HD02      AFTER     1.
     WRITE    PRT-REC   FROM      HD03      AFTER     1.
     WRITE    PRT-REC   FROM      HD04      AFTER     1.
     WRITE    PRT-REC   FROM      SEN01     AFTER     1.
     WRITE    PRT-REC   FROM      HD05      AFTER     1.
     WRITE    PRT-REC   FROM      SEN01     AFTER     1.
     MOVE     8         TO        LINE-CNT.
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
 900-SIH-READ           SECTION.
     READ     SKTSIHF  NEXT   AT   END
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
*    IF    PARA-SIMEBI NOT = SIH-F02
*             MOVE     "END"   TO   END-FLG
*             GO   TO   900-SIT-READ-EXIT
*    END-IF.
*取引先ＣＤチェック
*    IF    PARA-TORICD  =  ZERO
*             GO   TO   900-SIT-READ-EXIT
*    ELSE
*          IF  PARA-TORICD = SIH-F01
*              CONTINUE
*          ELSE
*             MOVE     "END"   TO   END-FLG
*          END-IF
*    END-IF.
*
 900-SIT-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
