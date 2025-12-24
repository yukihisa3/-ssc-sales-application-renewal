# SJS0051L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SJS0051L.COB`

## ソースコード

```cobol
****************************************************************
*
*    ユーザ　　　　名：　サカタのタネ　　　殿
*    システム　　　名：　実績管理システム
*    プログラム　　名：　取引先別商品別売上実績表
*    作成者　　　　　：　飯田/NAV　　　
*    作成日　　　　　：　2011.10.24
*    更新履歴      　：
*
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SJS0051L.
 AUTHOR.             NAV.
 DATE-WRITTEN.       00.06.12.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
         YA        IS   YA
         YA-21     IS   YA-21
         YB        IS   YB
     CONSOLE      IS     CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*画面Ｆ
     SELECT   DSPF           ASSIGN               TO  GS-DSPF
                             ORGANIZATION         IS  SEQUENTIAL
                             ACCESS MODE          IS  SEQUENTIAL
                             SYMBOLIC DESTINATION IS "DSP"
                             PROCESSING MODE      IS  DSP-PRO
                             GROUP                IS  DSP-GRP
                             FORMAT               IS  DSP-FMT
                             SELECTED FUNCTION    IS  DSP-FNC
                             FILE STATUS          IS  DSP-STA.
*実績集計ファイル
     SELECT   JISSYUF        ASSIGN        TO  01-VI-JISSYUL6
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  DYNAMIC
                             RECORD KEY    IS
                               JIS-F01  *> 売上仕入区分
                               JIS-F021 *> 年
                               JIS-F022 *> 月
                               JIS-F03  *> 得意先ＣＤ
                               JIS-F13  *> サカタ２０分類
                               JIS-F12  *> 相手商品ＣＤ
                               JIS-F051 *> 商品ＣＤ
                               JIS-F052 *> 品単ＣＤ
                                           WITH DUPLICATES
                             FILE STATUS   IS  JIS-STA.
*部門取引先マスタ
     SELECT   BUTOKMF        ASSIGN        TO  01-VI-BUTOKML1
                             ORGANIZATION  IS  INDEXED
                             ACCESS  MODE  IS  RANDOM
                             RECORD  KEY   IS  BUT-F01
                             FILE STATUS   IS  BUT-STA.

*条件ファイル
     SELECT   HJYOKEN        ASSIGN        TO  01-VI-JYOKEN1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  JYO-F01
                                               JYO-F02
                             FILE STATUS   IS  JYO-STA.
*プリントファイル
     SELECT   PRINTF    ASSIGN  TO   LP-04.
*
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*画面Ｆ
 FD  DSPF.
     COPY     FJS00511  OF  XMDLIB
     JOINING  DSP      AS  PREFIX.
*実績集計ファイル
 FD  JISSYUF.
     COPY     JISSYUF  OF  XFDLIB
     JOINING  JIS      AS  PREFIX.
*部門取引先Ｍ
 FD  BUTOKMF.
     COPY     BUTOKMF  OF  XFDLIB
     JOINING  BUT      AS  PREFIX.
*条件ファイル
 FD  HJYOKEN.
     COPY     HJYOKEN  OF  XFDLIB
     JOINING  JYO      AS  PREFIX.
*プリントファイル
 FD    PRINTF    LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
*
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*画面制御用
 01  DSP-CONTROL.
     03  DSP-PRO             PIC  X(02).
     03  DSP-GRP             PIC  X(08).
     03  DSP-FMT             PIC  X(08).
     03  DSP-FNC             PIC  X(04).
*ステータス
 01  STA-AREA.
     03  DSP-STA             PIC  X(02).
     03  JIS-STA             PIC  X(02).
     03  BUT-STA             PIC  X(02).
     03  JYO-STA             PIC  X(02).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR1       REDEFINES      DATE-AREA.
     03  SYS-YM                   PIC  9(06).
     03  FILLER                   PIC  9(02).
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*メッセージテーブル
 01  MSG-TBL.
     03  MSG-NO01            PIC  N(20)  VALUE
         NC"誤ったＰＦキーが押されました".
     03  MSG-NO02            PIC  N(20)  VALUE
         NC"開始が終了を越えています".
     03  MSG-NO03            PIC  N(20)  VALUE
         NC"対象データはありません".
     03  MSG-NO04            PIC  N(20)  VALUE
         NC"対象データ抽出中です".
     03  MSG-NO05            PIC  N(20)  VALUE
         NC"年月が違います".
     03  MSG-NO06            PIC  N(20)  VALUE
         NC"サカタ２０分類に誤りがあります".
     03  MSG-NO07            PIC  N(20)  VALUE
         NC"　".
     03  MSG-NO08            PIC  N(20)  VALUE
         NC"　".
     03  MSG-NO09            PIC  N(20)  VALUE
         NC"　".
     03  MSG-NO10            PIC  N(20)  VALUE
         NC"　".
 01  TBL-MSG-R   REDEFINES    MSG-TBL.
     03  TBL-MSG             PIC  N(20)  OCCURS  10   TIMES.
*ＰＦキー
 01  PFK-TBL.
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_取消　_終了".
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_取消　_終了　_項目戻り".
 01  TBL-PFK-R       REDEFINES    PFK-TBL.
     03  TBL-PFK             PIC  N(30)  OCCURS  2   TIMES.
*
 01  FLG-AREA.
     03  MAIN-FLG            PIC  9(02)  VALUE  ZERO.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  PFK-FLG             PIC  9(01)  VALUE  ZERO.
     03  JIS-FLG             PIC  9(01)  VALUE  ZERO.
     03  FG-JISSYUF-END      PIC  9(01)  VALUE  ZERO.
     03  FG-HJYOKEN-INV      PIC  9(01).
     03  FG-BUTOKMF-INV      PIC  9(01).
*
 01  CNT-AREA.
     03  PAGE-CNT            PIC  9(07)  VALUE  ZERO.
     03  LINE-CNT            PIC  9(02)  VALUE  ZERO.
     03  WK-LINE-CNT         PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  66.
     03  IN-CNT              PIC  9(07)  VALUE  ZERO.
     03  OT-CNT              PIC  9(07)  VALUE  ZERO.
*商品コード右詰め
 01  WK-SHOCD.
     03  WK-SHO              PIC  X(01)  OCCURS 8.
 01  IX                      PIC  9(02).
 01  WK-BNR20                PIC  X(02).
*キー領域
 01  INF-KEY.
     03  INF-F02.
       05  INF-F021          PIC  9(04).
       05  INF-F022          PIC  9(02).
     03  INF-F03             PIC  X(08).
     03  INF-F13             PIC  X(02).
     03  INF-F12             PIC  X(13).

 01  BRK-KEY.
     03  BRK-F02.
       05  BRK-F021          PIC  9(04).
       05  BRK-F022          PIC  9(02).
     03  BRK-F03             PIC  X(08).
     03  BRK-F13             PIC  X(02).
     03  BRK-F12             PIC  X(13).

*実績集計ファイル退避
     COPY  JISSYUF OF XFDLIB  JOINING JISW  AS PREFIX.
*集計用領域
 01  WK-SYUKEI-AREA.
     03  WK-S-AREA.
       05  WK-S-SURYO        PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-S-KINGAK       PIC S9(13)     PACKED-DECIMAL.
       05  WK-S-HEPSU        PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-S-HEPGAK       PIC S9(13)     PACKED-DECIMAL.
       05  WK-S-NEBSU        PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-S-NEBGAK       PIC S9(13)     PACKED-DECIMAL.
     03  WK-G-AREA.
       05  WK-G-AREA2  OCCURS 3.
         07  WK-G-SURYO      PIC S9(13)V99  PACKED-DECIMAL.
         07  WK-G-KINGAK     PIC S9(13)     PACKED-DECIMAL.
         07  WK-G-HEPSU      PIC S9(13)V99  PACKED-DECIMAL.
         07  WK-G-HEPGAK     PIC S9(13)     PACKED-DECIMAL.
         07  WK-G-NEBSU      PIC S9(13)V99  PACKED-DECIMAL.
         07  WK-G-NEBGAK     PIC S9(13)     PACKED-DECIMAL.
*
****  見出し行１             ****
 01  MIDASI1  CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(01)  VALUE  SPACE.
     02  FILLER             PIC  X(08)  VALUE  "SJS0051L".
     02  FILLER             PIC  X(31)  VALUE  SPACE.
     02  FILLER             PIC  N(22)  VALUE
         NC"※※　取引先別相手商品ＣＤ別売上実績表　※※".
     02  FILLER             PIC  X(30)  VALUE  SPACE.
     02  H1-YY              PIC  9(04).
     02  FILLER             PIC  N(01)  VALUE  NC"年".
     02  H1-MM              PIC  Z9.
     02  FILLER             PIC  N(01)  VALUE  NC"月".
     02  H1-DD              PIC  Z9.
     02  FILLER             PIC  N(01)  VALUE  NC"日".
     02  H1-PAGE            PIC  ZZZ9.
     02  FILLER             PIC  N(01)  VALUE  NC"頁".
****  見出し行２             ****
 01  MIDASI2  CHARACTER TYPE IS YA.
     02  FILLER             PIC  X(01)  VALUE  SPACE.
     02  FILLER             PIC  N(05)  VALUE  NC"対象年月：".
     02  H2-YY              PIC  9(04).
     02  FILLER             PIC  N(01)  VALUE  NC"年".
     02  H2-MM              PIC  Z9.
     02  FILLER             PIC  N(02)  VALUE  NC"月度".
****  見出し行３             ****
 01  MIDASI3  CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(01)  VALUE  SPACE.
     02  FILLER             PIC  N(01)  VALUE  NC"取".
     02  FILLER             PIC  X(01)  VALUE  SPACE.
     02  FILLER             PIC  N(01)  VALUE  NC"引".
     02  FILLER             PIC  X(01)  VALUE  SPACE.
     02  FILLER             PIC  N(01)  VALUE  NC"先".
     02  FILLER             PIC  N(01)  VALUE  NC"：".
     02  H3-TOKCD           PIC  9(08).
     02  FILLER             PIC  X(01)  VALUE  SPACE.
     02  H3-TOKNM           PIC  N(18).
****  見出し行３－２             ****
 01  MIDASI3-2  CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(04)  VALUE  SPACE.
     02  FILLER             PIC  N(01)  VALUE  NC"分".
     02  FILLER             PIC  X(01)  VALUE  SPACE.
     02  FILLER             PIC  N(01)  VALUE  NC"類".
     02  FILLER             PIC  N(01)  VALUE  NC"：".
     02  H3-2-BNR20         PIC  X(02).
     02  FILLER             PIC  X(01)  VALUE  SPACE.
     02  H3-2-BNR20NM       PIC  N(10).
****  見出し行４             ****
 01  MIDASI4  CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(01)  VALUE  SPACE.
     02  FILLER             PIC  N(06)  VALUE
         NC"相手商品ＣＤ".
     02  FILLER             PIC  X(02)  VALUE  SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"商品ＣＤ".
     02  FILLER             PIC  X(01)  VALUE  SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"品単ＣＤ".
     02  FILLER             PIC  X(01)  VALUE  SPACE.
     02  FILLER             PIC  N(09)  VALUE
         NC"相手商品名（カナ）".
     02  FILLER             PIC  X(22)  VALUE  SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"売上数量".
     02  FILLER             PIC  X(08)  VALUE  SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"返品数量".
     02  FILLER             PIC  X(08)  VALUE  SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"値引数量".
     02  FILLER             PIC  X(08)  VALUE  SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"数量数量".
****  見出し行５             ****
 01  MIDASI5  CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(73)  VALUE  SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"売上金額".
     02  FILLER             PIC  X(08)  VALUE  SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"返品金額".
     02  FILLER             PIC  X(08)  VALUE  SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"値引金額".
     02  FILLER             PIC  X(08)  VALUE  SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"金額合計".
****  見出し行９             ****
 01  MIDASI9  CHARACTER TYPE IS  YA.
     02  FILLER             PIC  N(68)  VALUE  ALL NC"─".
****  明細行１               ****
 01  MEISAI1  CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(01).
     02  PRT-AITSHO         PIC  X(13).
     02  FILLER             PIC  X(01).
     02  PRT-SHOCD          PIC  X(08).
     02  FILLER             PIC  X(01).
     02  PRT-HINTAN         PIC  X(08).
     02  FILLER             PIC  X(01).
     02  PRT-SHOKN1         PIC  X(15).
     02  FILLER             PIC  X(10).
     02  PRT-GOKEI          PIC  N(06).
     02  PRT-SURYO          PIC  ---,---,--9.99.
     02  FILLER             PIC  X(02).
     02  PRT-HEPSU          PIC  ---,---,--9.99.
     02  FILLER             PIC  X(02).
     02  PRT-NEBSU          PIC  ---,---,--9.99.
     02  FILLER             PIC  X(02).
     02  PRT-SA-SU          PIC  ---,---,--9.99.
****  明細行２               ****
 01  MEISAI2.
     02  FILLER             PIC  X(33).
     02  PRT2-SHOKN2        PIC  X(15).
     02  FILLER             PIC  X(22).
     02  PRT2-KINGAK        PIC  ---,---,--9.
     02  FILLER             PIC  X(05).
     02  PRT2-HEPGAK        PIC  ---,---,--9.
     02  FILLER             PIC  X(05).
     02  PRT2-NEBGAK        PIC  ---,---,--9.
     02  FILLER             PIC  X(05).
     02  PRT2-SA-GAK        PIC  ---,---,--9.
****  明細行９               ****
 01  MEISAI9.
     02  FILLER  OCCURS 68.
       03  FILLER            PIC  X(02)  VALUE  "- ".
*
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SJS0051L".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*=============================================================
 LINKAGE             SECTION.
*=============================================================
   01  LINK-SOKCD            PIC  X(02).
   01  LINK-DSOKCD           PIC  X(02).
******************************************************************
 PROCEDURE               DIVISION      USING    LINK-SOKCD
                                                LINK-DSOKCD.
******************************************************************
 DECLARATIVES.
*画面Ｆ
 DSP-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       DSPF.
     MOVE    "DSPF"        TO    ERR-FL-ID.
     MOVE     DSP-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*実績集計ファイル
 JIS-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       JISSYUF.
     MOVE    "JISSYUF"     TO    ERR-FL-ID.
     MOVE     JIS-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*取引先Ｍ
 BUT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       BUTOKMF.
     MOVE    "BUTOKML1"    TO    ERR-FL-ID.
     MOVE     BUT-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*条件ファイル
 JYO-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       HJYOKEN.
     MOVE    "JYOKEN1"     TO    ERR-FL-ID.
     MOVE     JYO-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  SJS0051L   START  **"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SJS0051L    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN  I-O    DSPF.
     OPEN  INPUT  JISSYUF.
     OPEN  INPUT  BUTOKMF.
     OPEN  INPUT  HJYOKEN.
     OPEN  OUTPUT PRINTF.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY H1-YY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM   H1-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD   H1-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*初期画面表示へ
     MOVE  1                TO    MAIN-FLG.
 INIT-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
     EVALUATE  MAIN-FLG
*初期画面表示
         WHEN   1      PERFORM  DSP-INIT-SEC
*ＢＯＤＹ部入力
         WHEN   2      PERFORM  DSP-BODY-SEC
*確認入力
         WHEN   3      PERFORM  DSP-KAKU-SEC
*更新処理
         WHEN   4      PERFORM  PRINT-SEC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
*=============================================================
*                 終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE  DSPF.
     CLOSE  JISSYUF.
     CLOSE  BUTOKMF.
     CLOSE  HJYOKEN.
     CLOSE  PRINTF.
*
     DISPLAY "* JISSYUF (IN)=" IN-CNT   " *" UPON CONS.
     DISPLAY "* JISSYUF (OT)=" OT-CNT   " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" PAGE-CNT " *" UPON CONS.
 END-EXIT.
     EXIT.
*=============================================================
*                画面初期表示処理
*=============================================================
 DSP-INIT-SEC       SECTION.
*初期画面の処理
     MOVE  SPACE            TO    DSP-CONTROL.
     MOVE "FJS00511"        TO    DSP-FMT.
     MOVE  SPACE            TO    DSP-FJS00511.
     PERFORM  OPT-CLR-SEC.
*年月
     MOVE  SYS-YM           TO    DSP-STYM     DSP-EDYM.
*ＢＯＤＹ部入力へ
     MOVE  2                TO    MAIN-FLG.
 DSP-INIT-EXIT.
     EXIT.
*=============================================================
*                項目属性初期化
*=============================================================
 OPT-CLR-SEC        SECTION.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-STYM.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-EDYM.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-STTOR.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-EDTOR.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-STSHO.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-EDSHO.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-SBNR20.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-EBNR20.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-STYM.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-EDYM.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-STTOR.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-EDTOR.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-STSHO.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-EDSHO.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-SBNR20.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-EBNR20.
 OPT-CLR-EXIT.
     EXIT.
*=============================================================
*                ＢＯＤＹ部入力処理
*=============================================================
 DSP-BODY-SEC       SECTION.
*画面表示
     MOVE      1        TO  PFK-FLG.
     PERFORM   DSP-WT-SEC.
*画面入力
     MOVE      "BODY"   TO  DSP-GRP.
     PERFORM   DSP-RD-SEC.
*ＰＦ判定
     EVALUATE  DSP-FNC
          WHEN "E000"
                          PERFORM  CHK-BODY-SEC
                          IF    ERR-FLG = ZERO
                                MOVE    3    TO   MAIN-FLG
                          END-IF
          WHEN "F004"
                          MOVE  1      TO  MAIN-FLG
          WHEN "F005"
                          MOVE  99     TO  MAIN-FLG
          WHEN "F006"
                          CONTINUE
          WHEN OTHER
                          MOVE  1      TO  ERR-FLG
     END-EVALUATE.
 DSP-BODY-EXIT.
     EXIT.
*=============================================================
*                ＢＯＤＹ部入力チェック
*=============================================================
 CHK-BODY-SEC   SECTION.
*開始年月
     MOVE  "2"              TO  LINK-IN-KBN.
     MOVE  DSP-STYM         TO  LINK-IN-YMD8(1:6).
     MOVE  "01"             TO  LINK-IN-YMD8(7:2).
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     IF  LINK-OUT-RET NOT = ZERO
         IF  ERR-FLG  =  ZERO
             MOVE   5       TO  ERR-FLG
         END-IF
         MOVE  "R"          TO  EDIT-OPTION OF DSP-STYM
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-STYM
     END-IF.
 CHK-STYM-EXIT.
*終了年月
     MOVE  "2"              TO  LINK-IN-KBN.
     MOVE  DSP-EDYM         TO  LINK-IN-YMD8(1:6).
     MOVE  "01"             TO  LINK-IN-YMD8(7:2).
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     IF  LINK-OUT-RET  NOT =  ZERO
         IF  ERR-FLG  =  ZERO
             MOVE   5        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION OF DSP-EDYM
         MOVE  "C"           TO  EDIT-CURSOR OF DSP-EDYM
         GO TO  CHK-EDYM-EXIT
     END-IF.

     IF      ERR-FLG = ZERO
         AND DSP-STYM > DSP-EDYM
         IF  ERR-FLG = ZERO
             MOVE  2        TO  ERR-FLG
         END-IF
         MOVE  "R"          TO  EDIT-OPTION OF DSP-STYM
         MOVE  "R"          TO  EDIT-OPTION OF DSP-EDYM
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-STYM
     END-IF.
 CHK-EDYM-EXIT.
*開始取引先
     IF  DSP-STTOR NOT NUMERIC
         MOVE  0             TO  DSP-STTOR
     END-IF.
 CHK-STTOR-EXIT.
*終了取引先
     IF  DSP-EDTOR  NOT NUMERIC
         MOVE  99999999 TO  DSP-EDTOR
     END-IF.

     IF  ERR-FLG NOT = ZERO
         GO TO  CHK-EDTOR-EXIT
     END-IF.

     IF  DSP-STTOR > DSP-EDTOR
         IF  ERR-FLG = ZERO
             MOVE  2         TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION OF DSP-STTOR
         MOVE  "R"           TO  EDIT-OPTION OF DSP-EDTOR
         MOVE  "C"           TO  EDIT-CURSOR OF DSP-STTOR
     END-IF.
 CHK-EDTOR-EXIT.
*開始相手商品ＣＤ
     IF  DSP-EDSHO = SPACE
         MOVE  "9999999999999"    TO  DSP-EDSHO
     END-IF.
 CHK-STSHO-EXIT.
*終了相手商品ＣＤ
     IF  ERR-FLG NOT = ZERO
         GO TO  CHK-EDSHO-EXIT
     END-IF.

     IF  DSP-STSHO > DSP-EDSHO
         IF  ERR-FLG  =  ZERO
             MOVE   2        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION OF DSP-STSHO
         MOVE  "R"           TO  EDIT-OPTION OF DSP-EDSHO
         MOVE  "C"           TO  EDIT-CURSOR OF DSP-STSHO
     END-IF.
 CHK-EDSHO-EXIT.
*開始サカタ２０分類
     IF  DSP-SBNR20 = SPACE
         GO TO  CHK-SBNR20-EXIT
     END-IF.

     MOVE  DSP-SBNR20        TO  WK-BNR20.
     PERFORM  HJYOKEN-RD-SEC.
     IF  FG-HJYOKEN-INV = 1
         IF  ERR-FLG  =  ZERO
             MOVE   6       TO  ERR-FLG
         END-IF
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SBNR20
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SBNR20
     END-IF.

 CHK-SBNR20-EXIT.
*終了サカタ２０分類
     IF  DSP-EBNR20 = SPACE OR ALL "9"
         MOVE  "99"         TO  DSP-EBNR20
     ELSE
         MOVE  DSP-EBNR20   TO  WK-BNR20
         PERFORM  HJYOKEN-RD-SEC
         IF  FG-HJYOKEN-INV = 1
             IF  ERR-FLG  =  ZERO
                 MOVE   6   TO  ERR-FLG
             END-IF
             MOVE  "R"      TO  EDIT-OPTION OF DSP-EBNR20
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-EBNR20
             GO TO  CHK-EBNR20-EXIT
         END-IF

     END-IF.

     IF  ERR-FLG NOT = ZERO
         GO TO  CHK-EBNR20-EXIT
     END-IF.

     IF  DSP-SBNR20 > DSP-EBNR20
         IF  ERR-FLG  =  ZERO
             MOVE  2         TO  ERR-FLG
         END-IF
         MOVE  "C"           TO  EDIT-CURSOR OF DSP-SBNR20
         MOVE  "R"           TO  EDIT-OPTION OF DSP-SBNR20
         MOVE  "R"           TO  EDIT-OPTION OF DSP-EBNR20
     END-IF.

 CHK-EBNR20-EXIT.
     IF  ERR-FLG NOT = ZERO
         GO TO  CHK-BODY-EXIT
     END-IF.

     MOVE  ZERO              TO  IN-CNT.
     MOVE  ZERO              TO  OT-CNT.
     INITIALIZE  JIS-REC.
     MOVE  "1"               TO  JIS-F01. *> 売上
     MOVE  DSP-STYM          TO  JIS-F02. *> 年月
     MOVE  DSP-STTOR         TO  JIS-F03. *> 得意先ＣＤ
     MOVE  DSP-SBNR20        TO  JIS-F13. *> サカタ２０分類
     MOVE  DSP-STSHO         TO  JIS-F12. *> 相手商品ＣＤ

     MOVE  8                 TO  FG-JISSYUF-END.
     PERFORM  JIS-READ-SEC.
*対象データ存在ＣＨＫ
     IF  FG-JISSYUF-END  =  9
         MOVE  3            TO  ERR-FLG
     END-IF.

 CHK-BODY-EXIT.
     EXIT.
*=============================================================
*  条件ファイル読込み処理
*=============================================================
 HJYOKEN-RD-SEC        SECTION.

     MOVE  "10"             TO  JYO-F01.
     MOVE  WK-BNR20         TO  JYO-F02.

     READ  HJYOKEN
       INVALID
         MOVE  1            TO  FG-HJYOKEN-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HJYOKEN-INV
     END-READ.

 HJYOKEN-RD-EXIT.
     EXIT.
*=============================================================
*                実績集計ファイル（読み込み）
*=============================================================
 JIS-READ-SEC    SECTION.
     IF FG-JISSYUF-END =  8
        MOVE  ZERO          TO  FG-JISSYUF-END

        START  JISSYUF  KEY >=  JIS-F01  *> 売上仕入区分
                                JIS-F021 *> 年
                                JIS-F022 *> 月
                                JIS-F03  *> 得意先ＣＤ
                                JIS-F13  *> サカタ２０分類
                                JIS-F12  *> 相手商品ＣＤ
                                JIS-F051 *> 商品ＣＤ
                                JIS-F052 *> 品単ＣＤ
          INVALID
            MOVE  9            TO  FG-JISSYUF-END
            GO TO  JIS-READ-EXIT
        END-START
     END-IF.

*リード
     READ  JISSYUF NEXT
       AT END
         MOVE  9            TO  FG-JISSYUF-END
         GO TO  JIS-READ-EXIT
     END-READ.

     ADD  1   TO  IN-CNT.

*売上仕入区分
     IF  JIS-F01 NOT = "1"
         MOVE  9            TO  FG-JISSYUF-END
         GO TO  JIS-READ-EXIT
     END-IF.

*年月
     IF  JIS-F02 > DSP-EDYM
         MOVE  9            TO  FG-JISSYUF-END
         GO TO  JIS-READ-EXIT
     END-IF.

*取引先ＣＤ
     IF     JIS-F03 < DSP-STTOR
         OR JIS-F03 > DSP-EDTOR
         GO TO  JIS-READ-SEC
     END-IF.

*サカタ２０分類
     IF     JIS-F13 < DSP-SBNR20
         OR JIS-F13 > DSP-EBNR20
         GO TO  JIS-READ-SEC
     END-IF.

*相手商品
     IF     JIS-F12 < DSP-STSHO
         OR JIS-F12 > DSP-EDSHO
         GO TO  JIS-READ-SEC
     END-IF.

*倉庫
     IF  LINK-DSOKCD NOT = "01"
         IF  LINK-SOKCD NOT = JIS-F04
             GO TO  JIS-READ-SEC
         END-IF
     END-IF.

*キー退避
     MOVE  JIS-F02          TO  INF-F02. *> 年月
     MOVE  JIS-F03          TO  INF-F03. *> 取引先ＣＤ
     MOVE  JIS-F13          TO  INF-F13. *> サカタ２０分類
     MOVE  JIS-F12          TO  INF-F12. *> 相手商品ＣＤ

     ADD  1   TO  OT-CNT.

 JIS-READ-EXIT.
     EXIT.
*=============================================================
*                確認入力処理
*=============================================================
 DSP-KAKU-SEC       SECTION.
*画面表示
     MOVE  2                    TO  PFK-FLG.
     PERFORM  DSP-WT-SEC.
*画面入力
     MOVE     "TAIL"            TO  DSP-GRP.
     PERFORM  DSP-RD-SEC.
*ＰＦ判定
     EVALUATE  DSP-FNC
         WHEN "E000"
                          PERFORM  CHK-KAKU-SEC
         WHEN "F004"
                          MOVE  1       TO   MAIN-FLG
         WHEN "F005"
                          MOVE  99      TO   MAIN-FLG
         WHEN "F006"
                          MOVE  2       TO   MAIN-FLG
         WHEN OTHER
                          MOVE  1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
*=============================================================
*                確認入力チェック（対象データ存在ＣＨＫ）
*=============================================================
 CHK-KAKU-SEC   SECTION.
     MOVE  ZERO              TO  ERR-FLG.

     IF  ERR-FLG = ZERO
         MOVE  4            TO  ERR-FLG
         PERFORM  DSP-WT-SEC
         MOVE  4            TO  MAIN-FLG
         INITIALIZE  WK-SYUKEI-AREA
     ELSE
         MOVE  99           TO  MAIN-FLG
     END-IF.

 CHK-KAKU-EXIT.
     EXIT.
*=============================================================
*                画面表示処理
*=============================================================
 DSP-WT-SEC       SECTION.
*ＰＦキー設定
     MOVE  TBL-PFK(PFK-FLG)      TO  DSP-PFKEY.
*エラー設定
     IF    ERR-FLG   NOT = ZERO
       MOVE  TBL-MSG(ERR-FLG)    TO  DSP-ERRMSG
     END-IF.
     MOVE     HEN-DATE           TO  DSP-SDATE.
     MOVE     HEN-TIME           TO  DSP-STIME.
*画面表示
     MOVE "SCREEN"               TO  DSP-GRP.
     MOVE  SPACE                 TO  DSP-PRO.
     WRITE DSP-FJS00511.
*エラークリア
     MOVE  ZERO                  TO  ERR-FLG.
     MOVE  SPACE                 TO  DSP-ERRMSG.
 DSP-WT-EXIT.
     EXIT.
*=============================================================
*                画面読込処理
*=============================================================
 DSP-RD-SEC        SECTION.
     MOVE "NE"                   TO  DSP-PRO.
     READ  DSPF.
     PERFORM       OPT-CLR-SEC.
 DSP-RD-EXIT.
     EXIT.
*=============================================================
*                帳票出力
*=============================================================
 PRINT-SEC           SECTION.

     INITIALIZE  WK-S-AREA.
     INITIALIZE  WK-G-AREA.
     MOVE  66               TO  LINE-CNT.
     MOVE  ZERO             TO  IN-CNT.
     MOVE  ZERO             TO  OT-CNT.

     MOVE  ZERO             TO  FG-JISSYUF-END.
     INITIALIZE  JIS-REC.

     MOVE  "1"              TO  JIS-F01. *> 売上
     MOVE  DSP-STYM         TO  JIS-F02. *> 年月
     MOVE  DSP-STTOR        TO  JIS-F03. *> 得意先ＣＤ
     MOVE  DSP-SBNR20       TO  JIS-F13. *> サカタ２０分類
     MOVE  DSP-STSHO        TO  JIS-F12. *> 相手商品ＣＤ
     MOVE  8                TO  FG-JISSYUF-END.
     PERFORM  JIS-READ-SEC.

     IF  FG-JISSYUF-END = 9
         MOVE  3            TO  ERR-FLG
     END-IF.

     PERFORM  UNTIL FG-JISSYUF-END = 9
       PERFORM  PRINTB-SEC

     END-PERFORM.

     MOVE  99               TO  MAIN-FLG.

 PRINT-EXIT.
     EXIT.
*=============================================================
*  帳票出力Ｂ
*=============================================================
 PRINTB-SEC           SECTION.

     MOVE  INF-KEY          TO  BRK-KEY.

     PERFORM  UNTIL FG-JISSYUF-END = 9
                 OR INF-F02 NOT = BRK-F02  *> 年月ブレーク

       IF INF-KEY NOT = BRK-KEY
          MOVE  INF-KEY     TO  BRK-KEY
       END-IF

       PERFORM  UNTIL FG-JISSYUF-END = 9
                 OR INF-F02 NOT = BRK-F02  *> 年月ブレーク
                 OR INF-F03 NOT = BRK-F03  *> 取引先ＣＤ

         IF INF-KEY NOT = BRK-KEY
            MOVE  INF-KEY   TO  BRK-KEY
         END-IF

         PERFORM  UNTIL FG-JISSYUF-END = 9
                     OR INF-F02 NOT = BRK-F02  *> 年月ブレーク
                     OR INF-F03 NOT = BRK-F03  *> 取引先ＣＤ
                     OR INF-F13 NOT = BRK-F13  *> サカタ２０分類
           PERFORM  PRINTC-SEC

         END-PERFORM

         PERFORM  GOKEI-PRINT-SEC *> 分類合計
         IF      FG-JISSYUF-END = ZERO
             AND INF-F02 = BRK-F02  *> 年月
             AND INF-F03 = BRK-F03  *> 取引先ＣＤ
             MOVE  66       TO  LINE-CNT
         END-IF

       END-PERFORM

       PERFORM  GOKEI2-PRINT-SEC *> 取引先合計
       IF      FG-JISSYUF-END = ZERO
           AND INF-F02 = BRK-F02  *> 年月
           MOVE  66         TO  LINE-CNT
       END-IF

     END-PERFORM.

     PERFORM  GOKEI3-PRINT-SEC. *> 月計
     MOVE  66               TO  LINE-CNT.

 PRINTB-EXIT.
     EXIT.
*=============================================================
*  帳票出力Ｃ
*=============================================================
 PRINTC-SEC           SECTION.

     IF INF-KEY NOT = BRK-KEY
        MOVE  INF-KEY       TO  BRK-KEY
     END-IF.

*  集計の先頭レコードを退避。
     MOVE  JIS-REC          TO  JISW-REC.

     PERFORM  UNTIL FG-JISSYUF-END = 9
                 OR INF-KEY    NOT = BRK-KEY *> 集計行ブレーク

       PERFORM  SUM-SEC
       PERFORM  JIS-READ-SEC

     END-PERFORM.

     PERFORM  BODY-PRINT-SEC.

 PRINTC-EXIT.
     EXIT.
*=============================================================
*  入力データ集計処理
*=============================================================
 SUM-SEC           SECTION.
*相手商品で集計
     ADD  JIS-F06   TO  WK-S-SURYO.
     ADD  JIS-F07   TO  WK-S-KINGAK.
     ADD  JIS-F08   TO  WK-S-HEPSU.
     ADD  JIS-F09   TO  WK-S-HEPGAK.
     ADD  JIS-F11   TO  WK-S-NEBSU.
     ADD  JIS-F10   TO  WK-S-NEBGAK.

     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 3
       ADD  JIS-F06   TO  WK-G-SURYO  (IX)
       ADD  JIS-F07   TO  WK-G-KINGAK (IX)
       ADD  JIS-F08   TO  WK-G-HEPSU  (IX)
       ADD  JIS-F09   TO  WK-G-HEPGAK (IX)
       ADD  JIS-F11   TO  WK-G-NEBSU  (IX)
       ADD  JIS-F10   TO  WK-G-NEBGAK (IX)
     END-PERFORM.

 SUM-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 BODY-PRINT-SEC             SECTION.
     COMPUTE WK-LINE-CNT =  LINE-CNT + 2.
     PERFORM  HEAD-PRINT-SEC.

     MOVE  SPACE            TO  MEISAI1.
     MOVE  SPACE            TO  MEISAI2.

     MOVE  JISW-F12         TO  PRT-AITSHO.
     MOVE  JISW-F051        TO  PRT-SHOCD.
     MOVE  JISW-F052        TO  PRT-HINTAN.
     MOVE  JISW-F15         TO  PRT-SHOKN1.
     MOVE  SPACE            TO  PRT-GOKEI.
     MOVE  WK-S-SURYO       TO  PRT-SURYO.
     MOVE  WK-S-HEPSU       TO  PRT-HEPSU.
     MOVE  WK-S-NEBSU       TO  PRT-NEBSU.
     COMPUTE  PRT-SA-SU = WK-S-SURYO - WK-S-HEPSU.

     MOVE  JISW-F16         TO  PRT2-SHOKN2.
     MOVE  WK-S-KINGAK      TO  PRT2-KINGAK.
     MOVE  WK-S-HEPGAK      TO  PRT2-HEPGAK.
     MOVE  WK-S-NEBGAK      TO  PRT2-NEBGAK.
     COMPUTE  PRT2-SA-GAK =
         WK-S-KINGAK - WK-S-HEPGAK - WK-S-NEBGAK.
*印刷
     WRITE  P-REC  FROM MEISAI1  AFTER 1.
     WRITE  P-REC  FROM MEISAI2  AFTER 1.
     ADD  2   TO  LINE-CNT.

     INITIALIZE  WK-S-AREA.

 BODY-PRINT-EXIT.
     EXIT.
*=============================================================
*                ＨＥＡＤ部　印刷処理
*=============================================================
 HEAD-PRINT-SEC      SECTION.
     IF  WK-LINE-CNT >= MAX-LINE
         PERFORM  HEAD-PRINTB-SEC
     END-IF.

 HEAD-PRINT-EXIT.
     EXIT.
*=============================================================
*                ＨＥＡＤ部　印刷Ｂ処理
*=============================================================
 HEAD-PRINTB-SEC     SECTION.
     ADD  1   TO  PAGE-CNT.

     MOVE  BRK-F13          TO  WK-BNR20.
     PERFORM  HJYOKEN-RD-SEC.

     MOVE  BRK-F03          TO  BUT-F01.
     READ  BUTOKMF
       INVALID
         MOVE  1            TO  FG-BUTOKMF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-BUTOKMF-INV
     END-READ.

     MOVE  PAGE-CNT         TO  H1-PAGE.

     MOVE  SYS-DATE(1:4)    TO  H1-YY.
     MOVE  SYS-DATE(5:2)    TO  H1-MM.
     MOVE  SYS-DATE(7:2)    TO  H1-DD.

     MOVE  BRK-F021         TO  H2-YY.
     MOVE  BRK-F022         TO  H2-MM.

     MOVE BRK-F03           TO  H3-TOKCD.
     IF  FG-BUTOKMF-INV = ZERO
         MOVE  BUT-F02      TO  H3-TOKNM
     ELSE
         MOVE  SPACE        TO  H3-TOKNM
     END-IF.

     MOVE  BRK-F13          TO  H3-2-BNR20.
     IF  FG-HJYOKEN-INV = ZERO
         MOVE  JYO-F03      TO  H3-2-BNR20NM
     ELSE
         MOVE  SPACE        TO  H3-2-BNR20NM
     END-IF.

     IF  PAGE-CNT NOT = 1
         MOVE  SPACE        TO  P-REC
         WRITE  P-REC  AFTER PAGE
     END-IF.

     WRITE  P-REC  FROM MIDASI1    AFTER 2.
     WRITE  P-REC  FROM MIDASI2    AFTER 1.
     WRITE  P-REC  FROM MIDASI3    AFTER 1.
     WRITE  P-REC  FROM MIDASI3-2  AFTER 1.
     WRITE  P-REC  FROM MIDASI9    AFTER 1.
     WRITE  P-REC  FROM MIDASI4    AFTER 1.
     WRITE  P-REC  FROM MIDASI5    AFTER 1.
     WRITE  P-REC  FROM MIDASI9    AFTER 1.

     MOVE  10                TO  LINE-CNT.
 HEAD-PRINTB-EXIT.
     EXIT.
*=============================================================
*                合計印刷処理（サカタ２０分類）
*=============================================================
 GOKEI-PRINT-SEC            SECTION.
     COMPUTE WK-LINE-CNT =  LINE-CNT + 4.
     PERFORM  HEAD-PRINT-SEC.
*明細部の編集
     MOVE  SPACE            TO  MEISAI1.
     MOVE  SPACE            TO  MEISAI2.

     MOVE  NC"　分類合計：" TO  PRT-GOKEI.
     MOVE  WK-G-SURYO (1)   TO  PRT-SURYO.
     MOVE  WK-G-HEPSU (1)   TO  PRT-HEPSU.
     MOVE  WK-G-NEBSU (1)   TO  PRT-NEBSU.
     COMPUTE  PRT-SA-SU = WK-G-SURYO (1) - WK-G-HEPSU (1).

     MOVE  WK-G-KINGAK (1)  TO  PRT2-KINGAK.
     MOVE  WK-G-HEPGAK (1)  TO  PRT2-HEPGAK.
     MOVE  WK-G-NEBGAK (1)  TO  PRT2-NEBGAK.
     COMPUTE  PRT2-SA-GAK =
         WK-G-KINGAK (1) - WK-G-HEPGAK (1) - WK-G-NEBGAK (1).
*印刷
     WRITE  P-REC  FROM MEISAI9  AFTER 1.
     WRITE  P-REC  FROM MEISAI1  AFTER 1.
     WRITE  P-REC  FROM MEISAI2  AFTER 1.
     WRITE  P-REC  FROM MEISAI9  AFTER 1.

     INITIALIZE  WK-G-AREA2 (1).

 GOKEI-PRINT-EXIT.
     EXIT.
*=============================================================
*                合計印刷処理（取引先）
*=============================================================
 GOKEI2-PRINT-SEC            SECTION.
     COMPUTE WK-LINE-CNT =  LINE-CNT + 3.
     PERFORM  HEAD-PRINT-SEC.
*明細部の編集
     MOVE  SPACE            TO  MEISAI1.
     MOVE  SPACE            TO  MEISAI2.

     MOVE  NC"取引先合計：" TO  PRT-GOKEI.
     MOVE  WK-G-SURYO (2)   TO  PRT-SURYO.
     MOVE  WK-G-HEPSU (2)   TO  PRT-HEPSU.
     MOVE  WK-G-NEBSU (2)   TO  PRT-NEBSU.
     COMPUTE  PRT-SA-SU = WK-G-SURYO (2) - WK-G-HEPSU (2).

     MOVE  WK-G-KINGAK (2)  TO  PRT2-KINGAK.
     MOVE  WK-G-HEPGAK (2)  TO  PRT2-HEPGAK.
     MOVE  WK-G-NEBGAK (2)  TO  PRT2-NEBGAK.
     COMPUTE  PRT2-SA-GAK =
         WK-G-KINGAK (2) - WK-G-HEPGAK (2) - WK-G-NEBGAK (2).
*印刷
     WRITE  P-REC  FROM MEISAI1  AFTER 1.
     WRITE  P-REC  FROM MEISAI2  AFTER 1.
     WRITE  P-REC  FROM MEISAI9  AFTER 1.

     INITIALIZE  WK-G-AREA2 (2).

 GOKEIS-PRINT-EXIT.
     EXIT.
*=============================================================
*                合計印刷処理（年月）
*=============================================================
 GOKEI3-PRINT-SEC           SECTION.

     COMPUTE WK-LINE-CNT =  LINE-CNT + 3.
     PERFORM  HEAD-PRINT-SEC.
*明細部の編集
     MOVE  SPACE            TO  MEISAI1.
     MOVE  SPACE            TO  MEISAI2.

     MOVE  NC"総　合　計：" TO  PRT-GOKEI.
     MOVE  WK-G-SURYO (3)   TO  PRT-SURYO.
     MOVE  WK-G-HEPSU (3)   TO  PRT-HEPSU.
     MOVE  WK-G-NEBSU (3)   TO  PRT-NEBSU.
     COMPUTE  PRT-SA-SU = WK-G-SURYO (3) - WK-G-HEPSU (3).

     MOVE  WK-G-KINGAK (3)  TO  PRT2-KINGAK.
     MOVE  WK-G-HEPGAK (3)  TO  PRT2-HEPGAK.
     MOVE  WK-G-NEBGAK (3)  TO  PRT2-NEBGAK.
     COMPUTE  PRT2-SA-GAK =
         WK-G-KINGAK (3) - WK-G-HEPGAK (3) - WK-G-NEBGAK (3).
*印刷
     WRITE  P-REC  FROM MEISAI1  AFTER 1.
     WRITE  P-REC  FROM MEISAI2  AFTER 1.
     WRITE  P-REC  FROM MEISAI9  AFTER 1.

     INITIALIZE  WK-G-AREA2 (3).

 GOKEI3-PRINT-EXIT.
     EXIT.

```
