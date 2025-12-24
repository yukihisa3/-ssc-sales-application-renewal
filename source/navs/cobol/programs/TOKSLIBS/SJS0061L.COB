****************************************************************
*
*    ユーザ　　　　名：　サカタのタネ　　　殿
*    システム　　　名：　実績管理システム
*    プログラム　　名：　相手商品ＣＤ別売上実績表
*    作成者　　　　　：　飯田/NAV　　
*    作成日　　　　　：　2011.10.20
*    更新履歴        ：
*
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SJS0061L.
 AUTHOR.             NAV.
 DATE-WRITTEN.       2011.10.20.
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
     SELECT   JISSYUF        ASSIGN        TO  01-VI-JISSYUL5
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  DYNAMIC
                             RECORD KEY    IS
                               JIS-F01  *> 売上仕入区分
                               JIS-F021 *> 年
                               JIS-F022 *> 月
                               JIS-F13  *> サカタ２０分類
                               JIS-F12  *> 相手商品ＣＤ
                               JIS-F051 *> 商品ＣＤ
                               JIS-F052 *> 品単ＣＤ
                                           WITH DUPLICATES
                             FILE STATUS   IS  JIS-STA.

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
     COPY     FJS00611  OF  XMDLIB
     JOINING  DSP      AS  PREFIX.
*実績集計ファイル
 FD  JISSYUF.
     COPY     JISSYUF  OF  XFDLIB
     JOINING  JIS      AS  PREFIX.
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
     03  MEI-STA             PIC  X(02).
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
 01  TBL-MSG-R   REDEFINES    MSG-TBL.
     03  TBL-MSG             PIC  N(20)  OCCURS  5   TIMES.
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
     03  FG-JISSYUF-END             PIC  9(01)  VALUE  ZERO.
*
 01  CNT-AREA.
     03  PAGE-CNT            PIC  9(07)  VALUE  ZERO.
     03  LINE-CNT            PIC  9(02)  VALUE  ZERO.
     03  LINE-ADD-CNT        PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  66.
     03  IN-CNT              PIC  9(07)  VALUE  ZERO.
     03  OT-CNT              PIC  9(07)  VALUE  ZERO.
*商品コード右詰め
 01  WK-SHOCD.
     03  WK-SHO              PIC  X(01)  OCCURS 8.
 01  IX                      PIC  9(02).
*キー領域
 01  INF-KEY.
     03  INF-F02.
       05  INF-F021          PIC  9(04).
       05  INF-F022          PIC  9(02).
     03  INF-F13             PIC  X(02).
     03  INF-F12             PIC  X(13).
     03  INF-F05.
       05  INF-F051          PIC  X(08).
       05  INF-F052          PIC  X(08).

 01  BRK-KEY.
     03  BRK-F02.
       05  BRK-F021          PIC  9(04).
       05  BRK-F022          PIC  9(02).
     03  BRK-F13             PIC  X(02).
     03  BRK-F12             PIC  X(13).
     03  BRK-F05.
       05  BRK-F051          PIC  X(08).
       05  BRK-F052          PIC  X(08).

*実績集計ファイル退避
     COPY  JISSYUF OF XFDLIB  JOINING JISW  AS PREFIX.
*集計用領域
 01  WK-SYUKEI-AREA.
     03  WK-S-AREA. *> 明細集計用
       05  WK-S-SURYO        PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-S-KINGAK       PIC S9(13)     PACKED-DECIMAL.
       05  WK-S-HEPSU        PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-S-HEPGAK       PIC S9(13)     PACKED-DECIMAL.
       05  WK-S-NEBSU        PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-S-NEBGAK       PIC S9(13)     PACKED-DECIMAL.
     03  WK-G-AREA. *> サカタ２０分類集計用
       05  WK-G-SURYO        PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-G-KINGAK       PIC S9(13)     PACKED-DECIMAL.
       05  WK-G-HEPSU        PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-G-HEPGAK       PIC S9(13)     PACKED-DECIMAL.
       05  WK-G-NEBSU        PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-G-NEBGAK       PIC S9(13)     PACKED-DECIMAL.
     03  WK-T-AREA. *> 総合計用
       05  WK-T-SURYO        PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-T-KINGAK       PIC S9(13)     PACKED-DECIMAL.
       05  WK-T-HEPSU        PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-T-HEPGAK       PIC S9(13)     PACKED-DECIMAL.
       05  WK-T-NEBSU        PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-T-NEBGAK       PIC S9(13)     PACKED-DECIMAL.
*
****  見出し行１             ****
 01  MIDASI1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SJS0061L".
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  FILLER              PIC  N(18)  VALUE
         NC"※※　相手商品ＣＤ別売上実績表　※※"
         CHARACTER TYPE IS  YA-21.
     02  FILLER              PIC  X(13)  VALUE  SPACE.
     02  H1-YY               PIC  9(04).
     02  FILLER              PIC  N(01)  VALUE  NC"年"
         CHARACTER TYPE IS  YA.
     02  H1-MM               PIC  Z9.
     02  FILLER              PIC  N(01)  VALUE  NC"月"
         CHARACTER TYPE IS  YA.
     02  H1-DD               PIC  Z9.
     02  FILLER              PIC  N(01)  VALUE  NC"日"
         CHARACTER TYPE IS  YA.
     02  H1-PAGE             PIC  ZZZ9.
     02  FILLER              PIC  N(01)  VALUE  NC"頁"
         CHARACTER TYPE IS  YA.

****  見出し行２             ****
 01  MIDASI2  CHARACTER TYPE IS  YA.
     02  FILLER              PIC  X(52)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"【".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H2-YY               PIC  9(04).
     02  FILLER              PIC  N(01)  VALUE  NC"年".
     02  H2-MM               PIC  Z9.
     02  FILLER              PIC  N(02)  VALUE  NC"月度".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"】".

****  見出し行２-２             ****
 01  MIDASI2-2  CHARACTER TYPE IS  YA.
     02  FILLER              PIC  X(52)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"【".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"分類".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  HD2-2-20BNR-NM      PIC  N(10)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"】".

****  見出し行３             ****
 01  MIDASI3  CHARACTER TYPE IS  YA.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(06)  VALUE
         NC"相手商品ＣＤ".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"商品ＣＤ".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"品単ＣＤ".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(09)  VALUE
         NC"相手商品名（カナ）".
     02  FILLER              PIC  X(24)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"売上数量".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"返品数量".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"値引数量".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"数量合計".
****  見出し行４             ****
 01  MIDASI4        CHARACTER     TYPE   IS   YA.
     02  FILLER              PIC  X(75)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"売上金額".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"返品金額".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"値引金額".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"金額合計".
****  見出し行９             ****
 01  MIDASI9  CHARACTER TYPE IS  YA.
     02  FILLER              PIC  N(68)  VALUE  ALL NC"─".
****  明細行１               ****
 01  MEISAI1.
     02  FILLER              PIC  X(01).
     02  PRT-AITSHO          PIC  X(13)B.
     02  PRT-SHOCD           PIC  X(08)B.
     02  PRT-HINTAN          PIC  X(08)B.
     02  PRT-AITSHO-NM1      PIC  X(15).
     02  PRT-AITSHO-NM2      PIC  X(15).
     02  FILLER              PIC  X(01).
     02  PRT-GOK-HYOJI       PIC  N(04)
         CHARACTER TYPE IS  YA.
     02  PRT-SURYO           PIC  ---,---,--9.99.
     02  FILLER              PIC  X(02).
     02  PRT-HEPSU           PIC  ---,---,--9.99.
     02  FILLER              PIC  X(02).
     02  PRT-NEBSU           PIC  ---,---,--9.99.
     02  FILLER              PIC  X(02).
     02  PRT-SA-SU           PIC  ---,---,--9.99.
****  明細行２               ****
 01  MEISAI2.
     02  FILLER              PIC  X(72).
     02  PRT-KINGAK          PIC  ---,---,--9.
     02  FILLER              PIC  X(05).
     02  PRT-HEPGAK          PIC  ---,---,--9.
     02  FILLER              PIC  X(05).
     02  PRT-NEBAK           PIC  ---,---,--9.
     02  FILLER              PIC  X(05).
     02  PRT-SA-GAK          PIC  ---,---,--9.
****  明細行９               ****
 01  MEISAI9.
     02  FILLER  OCCURS 68.
       03  FILLER            PIC  X(02)  VALUE  "- ".
*
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SJS0061L".
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
     MOVE    "JISSYUL3"    TO    ERR-FL-ID.
     MOVE     JIS-STA      TO    ERR-STCD.
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
     DISPLAY  "**  SJS0061L   START  **"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SJS0061L    END   **"   UPON  CONS.
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
     MOVE "FJS00611"        TO    DSP-FMT.
     MOVE  SPACE            TO    DSP-FJS00611.
     PERFORM  OPT-CLR-SEC.
*年月
     MOVE  SYS-YM           TO    DSP-STYM     DSP-EDYM.
*ＢＯＤＹ部入力へ
     MOVE  2                TO    MAIN-FLG.
 DSP-INIT-EXIT.
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
         IF  ERR-FLG = ZERO
             MOVE  5        TO  ERR-FLG
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
     IF  LINK-OUT-RET NOT = ZERO
         IF  ERR-FLG = ZERO
             MOVE   5        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION OF DSP-EDYM
         MOVE  "C"           TO  EDIT-CURSOR OF DSP-EDYM
         GO TO  CHK-EDYM-EXIT
     END-IF.

     IF       ERR-FLG  = ZERO
         AND  DSP-STYM > DSP-EDYM
         IF  ERR-FLG = ZERO
             MOVE   2        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION OF DSP-STYM
         MOVE  "R"           TO  EDIT-OPTION OF DSP-EDYM
         MOVE  "C"           TO  EDIT-CURSOR OF DSP-STYM
     END-IF.

 CHK-EDYM-EXIT.

*開始相手商品

 CHK-STSHO-EXIT.

*終了相手商品
     IF  DSP-EDSHO = SPACE
         MOVE  ALL "9"       TO  DSP-EDSHO
     END-IF.

     IF  DSP-STSHO  > DSP-EDSHO
         IF  ERR-FLG  =  ZERO
             MOVE 2          TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION OF DSP-STSHO
         MOVE  "R"           TO  EDIT-OPTION OF DSP-EDSHO
         MOVE  "C"           TO  EDIT-CURSOR OF DSP-STSHO
     END-IF.

 CHK-EDSHO-EXIT.

*開始サカタ２０分類

 CHK-S20BNR-EXIT.

*終了サカタ２０分類
     IF  DSP-E20BNR = SPACE
         MOVE  "99"          TO  DSP-E20BNR
     END-IF.

     IF  DSP-S20BNR  > DSP-E20BNR
         IF  ERR-FLG  =  ZERO
             MOVE 2          TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION OF DSP-S20BNR
         MOVE  "R"           TO  EDIT-OPTION OF DSP-E20BNR
         MOVE  "C"           TO  EDIT-CURSOR OF DSP-S20BNR
     END-IF.

 CHK-E20BNR-EXIT.

     IF  ERR-FLG NOT = ZERO
         GO TO  CHK-BODY-EXIT
     END-IF.

     MOVE  ZERO              TO  IN-CNT.
     MOVE  ZERO              TO  OT-CNT.

* 対象データ存在ＣＨＫ
     INITIALIZE  JIS-REC.
     MOVE  "1"               TO  JIS-F01. *> 売上
     MOVE  DSP-STYM          TO  JIS-F02. *> 年月
     MOVE  DSP-S20BNR        TO  JIS-F13. *> サカタ２０分類
     MOVE  DSP-STSHO         TO  JIS-F12. *> 相手商品ＣＤ
     MOVE  8                 TO  FG-JISSYUF-END.
     PERFORM  JIS-READ-SEC.
     IF  FG-JISSYUF-END  =  9
         MOVE  3             TO  ERR-FLG
     END-IF.

 CHK-BODY-EXIT.
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
*                実績集計ファイル（読み込み）
*=============================================================
 JIS-READ-SEC    SECTION.
     IF FG-JISSYUF-END =  8
        MOVE  ZERO          TO  FG-JISSYUF-END

        START  JISSYUF  KEY >= JIS-F01
                               JIS-F021
                               JIS-F022
                               JIS-F13
                               JIS-F12
                               JIS-F051
                               JIS-F052
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

*サカタ２０分類
     IF  JIS-F13 > DSP-E20BNR
         MOVE  9            TO  FG-JISSYUF-END
         GO TO  JIS-READ-EXIT
     END-IF.

*相手商品
     IF      JIS-F02 = DSP-EDYM
         AND JIS-F12 > DSP-EDSHO
         MOVE  9            TO  FG-JISSYUF-END
         GO TO  JIS-READ-EXIT
     END-IF.

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
     MOVE  JIS-F02          TO  INF-F02.
     MOVE  JIS-F13          TO  INF-F13.
     MOVE  JIS-F12          TO  INF-F12.
     MOVE  JIS-F05          TO  INF-F05.

     ADD  1   TO  OT-CNT.

 JIS-READ-EXIT.
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
     WRITE DSP-FJS00611.
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
*                項目属性初期化
*=============================================================
 OPT-CLR-SEC        SECTION.
     MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-STYM.
     MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-EDYM.
     MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-STSHO.
     MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-EDSHO.
     MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-S20BNR.
     MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-E20BNR.
     MOVE "M"            TO  EDIT-OPTION  OF  DSP-STYM.
     MOVE "M"            TO  EDIT-OPTION  OF  DSP-EDYM.
     MOVE "M"            TO  EDIT-OPTION  OF  DSP-STSHO.
     MOVE "M"            TO  EDIT-OPTION  OF  DSP-EDSHO.
     MOVE "M"            TO  EDIT-OPTION  OF  DSP-S20BNR.
     MOVE "M"            TO  EDIT-OPTION  OF  DSP-E20BNR.
 OPT-CLR-EXIT.
     EXIT.
*=============================================================
*                帳票出力
*=============================================================
 PRINT-SEC           SECTION.

     INITIALIZE  WK-S-AREA.
     INITIALIZE  WK-G-AREA.
     INITIALIZE  WK-T-AREA.

     MOVE  ZERO             TO  FG-JISSYUF-END.
     INITIALIZE  JIS-REC.

     MOVE  66               TO  LINE-CNT.
     MOVE  ZERO             TO  IN-CNT.
     MOVE  ZERO             TO  OT-CNT.


     MOVE  "1"              TO  JIS-F01. *> 売上
     MOVE  DSP-STYM         TO  JIS-F02. *> 年月
     MOVE  DSP-S20BNR       TO  JIS-F13. *> サカタ２０分類
     MOVE  DSP-STSHO        TO  JIS-F12. *> 相手商品ＣＤ
     MOVE  8                TO  FG-JISSYUF-END.
     PERFORM  JIS-READ-SEC.

     IF  FG-JISSYUF-END = 9
         MOVE  3            TO  ERR-FLG
         GO TO  PRINT-090
     END-IF.

     PERFORM  UNTIL FG-JISSYUF-END = 9
       PERFORM  PRINTB-SEC

     END-PERFORM.

     PERFORM  SOGOKEI-PRINT-SEC.

 PRINT-090.
     MOVE  99               TO  MAIN-FLG.

 PRINT-EXIT.
     EXIT.
*=============================================================
*  帳票出力Ｂ
*=============================================================
 PRINTB-SEC           SECTION.

     MOVE  INF-KEY          TO  BRK-KEY.
     PERFORM  UNTIL FG-JISSYUF-END = 9
                 OR INF-F02    NOT = BRK-F02  *> 年月ブレーク
                 OR INF-F13    NOT = BRK-F13  *> サカタ２０分類
       PERFORM  PRINTC-SEC

     END-PERFORM.

     PERFORM  GOKEI-PRINT-SEC.
     IF FG-JISSYUF-END = 9
        CONTINUE
     ELSE
        MOVE  66            TO  LINE-CNT
     END-IF.


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
* 相手商品で明細集計
     ADD  JIS-F06   TO  WK-S-SURYO.
     ADD  JIS-F07   TO  WK-S-KINGAK.
     ADD  JIS-F08   TO  WK-S-HEPSU.
     ADD  JIS-F09   TO  WK-S-HEPGAK.
     ADD  JIS-F11   TO  WK-S-NEBSU.
     ADD  JIS-F10   TO  WK-S-NEBGAK.

* サカタ２０分類
     ADD  JIS-F06   TO  WK-G-SURYO.
     ADD  JIS-F07   TO  WK-G-KINGAK.
     ADD  JIS-F08   TO  WK-G-HEPSU.
     ADD  JIS-F09   TO  WK-G-HEPGAK.
     ADD  JIS-F11   TO  WK-G-NEBSU.
     ADD  JIS-F10   TO  WK-G-NEBGAK.

* 総合計
     ADD  JIS-F06   TO  WK-T-SURYO.
     ADD  JIS-F07   TO  WK-T-KINGAK.
     ADD  JIS-F08   TO  WK-T-HEPSU.
     ADD  JIS-F09   TO  WK-T-HEPGAK.
     ADD  JIS-F11   TO  WK-T-NEBSU.
     ADD  JIS-F10   TO  WK-T-NEBGAK.

 SUM-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 BODY-PRINT-SEC             SECTION.
     MOVE  3                TO  LINE-ADD-CNT.
     PERFORM  HEAD-PRINT-SEC.

     MOVE  SPACE            TO  MEISAI1.
     MOVE  SPACE            TO  MEISAI2.

     MOVE  JISW-F12         TO  PRT-AITSHO.
     MOVE  JISW-F051        TO  PRT-SHOCD.
     MOVE  JISW-F052        TO  PRT-HINTAN.
     MOVE  JISW-F15         TO  PRT-AITSHO-NM1.
     MOVE  JISW-F16         TO  PRT-AITSHO-NM2.
     MOVE  SPACE            TO  PRT-GOK-HYOJI.
     MOVE  WK-S-SURYO       TO  PRT-SURYO.
     MOVE  WK-S-HEPSU       TO  PRT-HEPSU.
     MOVE  WK-S-NEBSU       TO  PRT-NEBSU.
     COMPUTE  PRT-SA-SU = WK-S-SURYO - WK-S-HEPSU.

     MOVE  WK-S-KINGAK      TO  PRT-KINGAK.
     MOVE  WK-S-HEPGAK      TO  PRT-HEPGAK.
     MOVE  WK-S-NEBGAK      TO  PRT-NEBAK.
     COMPUTE  PRT-SA-GAK =
         WK-S-KINGAK - WK-S-HEPGAK - WK-S-NEBGAK.
*印刷
     WRITE  P-REC  FROM MEISAI1  AFTER 1.
     WRITE  P-REC  FROM MEISAI2  AFTER 1.
     WRITE  P-REC  FROM MEISAI9  AFTER 1.
     ADD  3   TO  LINE-CNT.

     INITIALIZE  WK-S-AREA.

 BODY-PRINT-EXIT.
     EXIT.
*=============================================================
*                ＨＥＡＤ部　印刷処理
*=============================================================
 HEAD-PRINT-SEC      SECTION.
     IF  LINE-CNT + LINE-ADD-CNT > MAX-LINE
         PERFORM  HEAD-PRINTB-SEC
     END-IF.

 HEAD-PRINT-EXIT.
     EXIT.
*=============================================================
*                ＨＥＡＤ部　印刷Ｂ処理
*=============================================================
 HEAD-PRINTB-SEC     SECTION.
     ADD  1   TO  PAGE-CNT.
     MOVE  PAGE-CNT         TO  H1-PAGE.

     MOVE  BRK-F021         TO  H2-YY.
     MOVE  BRK-F022         TO  H2-MM.

     MOVE  "10"             TO  JYO-F01.
     MOVE  BRK-F13          TO  JYO-F02.
     READ  HJYOKEN
       INVALID
         MOVE  SPACE        TO  HD2-2-20BNR-NM
       NOT INVALID
         MOVE  JYO-F03      TO  HD2-2-20BNR-NM
     END-READ.

     IF  PAGE-CNT NOT = 1
         MOVE  SPACE        TO  P-REC
         WRITE  P-REC  AFTER PAGE
     END-IF.

     WRITE  P-REC  FROM MIDASI1    AFTER 2.
     WRITE  P-REC  FROM MIDASI2    AFTER 1.
     WRITE  P-REC  FROM MIDASI2-2  AFTER 1.
     WRITE  P-REC  FROM MIDASI9    AFTER 1.
     WRITE  P-REC  FROM MIDASI3    AFTER 1.
     WRITE  P-REC  FROM MIDASI4    AFTER 1.
     WRITE  P-REC  FROM MIDASI9    AFTER 1.

     MOVE  9                TO  LINE-CNT.
 HEAD-PRINTB-EXIT.
     EXIT.
*=============================================================
*                合計印刷処理
*=============================================================
 GOKEI-PRINT-SEC            SECTION.
* 改ページ
     MOVE  3                TO  LINE-ADD-CNT.
     PERFORM  HEAD-PRINT-SEC.

* 明細部の編集
     MOVE  SPACE            TO  MEISAI1.
     MOVE  SPACE            TO  MEISAI2.

     MOVE  NC"分類計："     TO  PRT-GOK-HYOJI.
     MOVE  WK-G-SURYO       TO  PRT-SURYO.
     MOVE  WK-G-HEPSU       TO  PRT-HEPSU.
     MOVE  WK-G-NEBSU       TO  PRT-NEBSU.
     COMPUTE  PRT-SA-SU = WK-G-SURYO - WK-G-HEPSU.

     MOVE  WK-G-KINGAK      TO  PRT-KINGAK.
     MOVE  WK-G-HEPGAK      TO  PRT-HEPGAK.
     MOVE  WK-G-NEBGAK      TO  PRT-NEBAK.
     COMPUTE  PRT-SA-GAK =
         WK-G-KINGAK - WK-G-HEPGAK - WK-G-NEBGAK.
* 印刷
     WRITE  P-REC  FROM MEISAI1  AFTER 1.
     WRITE  P-REC  FROM MEISAI2  AFTER 1.
     WRITE  P-REC  FROM MEISAI9  AFTER 1.

     ADD  3   TO  LINE-CNT.
     INITIALIZE  WK-G-AREA.

 GOKEI-PRINT-EXIT.
     EXIT.
*=============================================================
*                総合計印刷処理
*=============================================================
 SOGOKEI-PRINT-SEC            SECTION.
* 改ページ
     MOVE  3                TO  LINE-ADD-CNT.
     PERFORM  HEAD-PRINT-SEC.

* 明細部の編集
     MOVE  SPACE            TO  MEISAI1.
     MOVE  SPACE            TO  MEISAI2.

     MOVE  NC"総合計："     TO  PRT-GOK-HYOJI.
     MOVE  WK-T-SURYO       TO  PRT-SURYO.
     MOVE  WK-T-HEPSU       TO  PRT-HEPSU.
     MOVE  WK-T-NEBSU       TO  PRT-NEBSU.
     COMPUTE  PRT-SA-SU = WK-T-SURYO - WK-T-HEPSU.

     MOVE  WK-T-KINGAK      TO  PRT-KINGAK.
     MOVE  WK-T-HEPGAK      TO  PRT-HEPGAK.
     MOVE  WK-T-NEBGAK      TO  PRT-NEBAK.
     COMPUTE  PRT-SA-GAK =
         WK-T-KINGAK - WK-T-HEPGAK - WK-T-NEBGAK.
* 印刷
     WRITE  P-REC  FROM MEISAI1  AFTER 1.
     WRITE  P-REC  FROM MEISAI2  AFTER 1.
     WRITE  P-REC  FROM MEISAI9  AFTER 1.

     ADD  3   TO  LINE-CNT.
     INITIALIZE  WK-T-AREA.

 SOGOKEI-PRINT-EXIT.
     EXIT.
