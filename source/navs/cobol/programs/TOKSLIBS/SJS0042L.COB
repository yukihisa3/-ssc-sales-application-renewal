****************************************************************
*
*    ユーザ　　　　名：　サカタのタネ　　　殿
*    システム　　　名：　実績管理システム
*    プログラム　　名：　日別取引先別売上実績表
*    作成者　　　　　：　飯田/NAV　
*    作成日　　　　　：　2011.11.21
*    更新履歴        ：
*
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SJS0042L.
 AUTHOR.             NAV.
 DATE-WRITTEN.       2011.11.21.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
         YA        IS   YA
         YA-21     IS   YA-21
         YB        IS   YB
     CONSOLE       IS   CONS.
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
     SELECT   RUISEKF        ASSIGN        TO  01-VI-RUISEKL5
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  DYNAMIC
                             RECORD KEY    IS
                               RUI-F09 *> 出荷日
                               RUI-F06 *> 振替元/振替先
                               RUI-F24 *> サカタ２０分類
                             FILE STATUS   IS  RUI-STA.
*部門取引先マスタ
     SELECT   BUTOKMF        ASSIGN        TO  01-VI-BUTOKML1
                             ORGANIZATION  IS  INDEXED
                             ACCESS  MODE  IS  RANDOM
                             RECORD  KEY   IS  BUT-F01
                             FILE STATUS   IS  BUT-STA.
*条件ファイル
     SELECT   HJYOKEN        ASSIGN        TO  01-VI-JYOKEN1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  JYO-F01
                                               JYO-F02
                             FILE STATUS   IS  JYO-STA.
*プリントファイル
     SELECT   PRINTF         ASSIGN        TO  LP-04.
*
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*画面Ｆ
 FD  DSPF.
     COPY     FJS00421  OF  XMDLIB
     JOINING  DSP      AS  PREFIX.
*実績集計ファイル
 FD  RUISEKF.
     COPY     RUISEKF  OF  XFDLIB
     JOINING  RUI      AS  PREFIX.
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
     03  RUI-STA             PIC  X(02).
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
     03  MSG-NO01           PIC  N(20)  VALUE
         NC"誤ったＰＦキーが押されました".
     03  MSG-NO02           PIC  N(20)  VALUE
         NC"開始が終了を越えています".
     03  MSG-NO03           PIC  N(20)  VALUE
         NC"対象データはありません".
     03  MSG-NO04           PIC  N(20)  VALUE
         NC"対象データ抽出中です".
     03  MSG-NO05           PIC  N(20)  VALUE
         NC"出力年月日に誤りが有ります".
     03  MSG-NO06           PIC  N(20)  VALUE
         NC"サカタ２０分類条件に誤りが有ります".
     03  MSG-NO07           PIC  N(20)  VALUE
         NC"　".
     03  MSG-NO08           PIC  N(20)  VALUE
         NC"　".
     03  MSG-NO09           PIC  N(20)  VALUE
         NC"　".
     03  MSG-NO10           PIC  N(20)  VALUE
         NC"　".
 01  TBL-MSG-R  REDEFINES MSG-TBL.
     03  TBL-MSG            PIC  N(20)  OCCURS 10 TIMES.
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
     03  FG-RUISEKF-END      PIC  9(01)  VALUE  ZERO.
     03  FG-HJYOKEN-END      PIC  X(03).
     03  FG-BUTOKMF-INV      PIC  9(01).
*
 01  CNT-AREA.
     03  PAGE-CNT            PIC  9(07)  VALUE  ZERO.
     03  LINE-CNT            PIC  9(02)  VALUE  ZERO.
     03  WK-LINE-CNT         PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  62.
     03  IN-CNT              PIC  9(07)  VALUE  ZERO.
     03  OT-CNT              PIC  9(07)  VALUE  ZERO.
*キー領域
 01  INF-KEY.
     03  INF-F09             PIC  9(08).
     03  INF-F06             PIC  X(08).
     03  INF-F24             PIC  X(02).

 01  BRK-KEY.
     03  BRK-F09             PIC  9(08).
     03  BRK-F06             PIC  X(08).
     03  BRK-F24             PIC  X(02).

 01  IX                      PIC  9(04).
 01  IX-BNR20                PIC  9(04).
 01  WK-SHO                  PIC  9(04).
 01  WK-AMARI                PIC  9(04).
 01  WK-DMY-ACCEPT           PIC  X(01).

*実績累積ファイル退避
     COPY  RUISEKF OF XFDLIB  JOINING RUIW  AS PREFIX.

* サカタ２０分類
 01  TB-BNR20-AREA.
     05  TB-BNR20-G  OCCURS 20.
       07  TB-BNR20         PIC  X(02).
       07  TB-BNR20NM       PIC  N(10).

*集計用領域
 01  WK-SYUKEI-AREA.
     03  WK-S-AREA.
       05  WK-S-AREA2  OCCURS 20. *> サカタ２０分類・明細
         07  WK-S-SURYO     PIC S9(13)V99  PACKED-DECIMAL.
         07  WK-S-KINGAK    PIC S9(13)     PACKED-DECIMAL.
         07  WK-S-HEPSU     PIC S9(13)V99  PACKED-DECIMAL.
         07  WK-S-HEPGAK    PIC S9(13)     PACKED-DECIMAL.
         07  WK-S-NEBSU     PIC S9(13)V99  PACKED-DECIMAL.
         07  WK-S-NEBGAK    PIC S9(13)     PACKED-DECIMAL.
     03  WK-G-AREA.               *> 取引先合計
       05  WK-G-SURYO       PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-G-KINGAK      PIC S9(13)     PACKED-DECIMAL.
       05  WK-G-HEPSU       PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-G-HEPGAK      PIC S9(13)     PACKED-DECIMAL.
       05  WK-G-NEBSU       PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-G-NEBGAK      PIC S9(13)     PACKED-DECIMAL.
     03  WK-T-AREA.               *> 総合計
       05  WK-T-SURYO       PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-T-KINGAK      PIC S9(13)     PACKED-DECIMAL.
       05  WK-T-HEPSU       PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-T-HEPGAK      PIC S9(13)     PACKED-DECIMAL.
       05  WK-T-NEBSU       PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-T-NEBGAK      PIC S9(13)     PACKED-DECIMAL.
*
****  見出し行１             ****
 01  MIDASI1  CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(01)  VALUE SPACE.
     02  FILLER             PIC  X(08)  VALUE "SJS0042L".
     02  FILLER             PIC  X(36)  VALUE SPACE.
     02  FILLER             PIC  N(17)  VALUE
         NC"※※　日別取引先別売上実績表　※※".
     02  FILLER             PIC  X(35)  VALUE SPACE.
     02  H1-YY              PIC  9(04).
     02  FILLER             PIC  N(01)  VALUE NC"年".
     02  H1-MM              PIC  Z9.
     02  FILLER             PIC  N(01)  VALUE NC"月".
     02  H1-DD              PIC  Z9.
     02  FILLER             PIC  N(01)  VALUE NC"日".
     02  H1-PAGE            PIC  ZZZ9.
     02  FILLER             PIC  N(01)  VALUE NC"頁".

****  見出し行２             ****
 01  MIDASI2  CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(52)  VALUE  SPACE.
     02  FILLER             PIC  N(01)  VALUE  NC"【".
     02  FILLER             PIC  X(01)  VALUE  SPACE.
     02  H2-YY              PIC  9(04).
     02  FILLER             PIC  N(01)  VALUE  NC"年".
     02  H2-MM              PIC  Z9.
     02  FILLER             PIC  N(01)  VALUE  NC"月".
     02  H2-DD              PIC  Z9.
     02  FILLER             PIC  N(01)  VALUE  NC"日".
     02  FILLER             PIC  X(01)  VALUE  SPACE.
     02  FILLER             PIC  N(01)  VALUE  NC"】".

****  見出し行３             ****
 01  MIDASI3.
     02  FILLER             PIC  X(01)  VALUE SPACE.
     02  FILLER             PIC  N(05)  VALUE
         NC"取引先名称"
         CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(28)  VALUE SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"売上数量"
         CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(03)  VALUE SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"売上金額"
         CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(06)  VALUE SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"返品数量"
         CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(03)  VALUE SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"返品金額"
         CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(06)  VALUE SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"値引数量"
         CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(03)  VALUE SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"値引金額"
         CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(05)  VALUE SPACE.
     02  FILLER             PIC  N(06)  VALUE
         NC"差引数量合計"
         CHARACTER TYPE IS  YB.
     02  FILLER             PIC  X(02)  VALUE SPACE.
     02  FILLER             PIC  N(06)  VALUE
         NC"差引金額合計"
         CHARACTER TYPE IS  YB.

****  見出し行３－２           ****
 01  MIDASI3-2.
     02  FILLER             PIC  X(01)  VALUE SPACE.
     02  FILLER             PIC  N(05)  VALUE
         NC"分類名称"
         CHARACTER TYPE IS  YA.

****  見出し行９             ****
 01  MIDASI9  CHARACTER TYPE IS  YA.
     02  FILLER             PIC  N(68)  VALUE  ALL NC"─".

****  明細行１               ****
 01  MEISAI1  CHARACTER TYPE IS  YB.
     02  FILLER             PIC  X(01).
     02  PRT-TOKCD          PIC  9(08).
     02  FILLER             PIC  N(01).
     02  PRT-TOKNM          PIC  N(15).

****  明細行２               ****
 01  MEISAI2  CHARACTER TYPE IS  YB.
     02  FILLER             PIC  X(01).
     02  PRT2-BNR20         PIC  X(02).
     02  FILLER             PIC  X(06).
     02  FILLER             PIC  N(01).
     02  PRT2-BNR20NM       PIC  N(15).
     02  PRT2-SURYO         PIC  ---,---,--9.99.
     02  PRT2-KINGAK        PIC  ---,---,--9.
     02  PRT2-HEPSU         PIC  ---,---,--9.99.
     02  PRT2-HEPGAK        PIC  ---,---,--9.
     02  PRT2-NEBSU         PIC  ---,---,--9.99.
     02  PRT2-NEBGAK        PIC  ---,---,--9.
     02  PRT2-SA-SU         PIC  ---,---,--9.99.
     02  PRT2-SA-GAK        PIC  ---,---,--9.

****  明細行９               ****
 01  MEISAI9.
     02  FILLER  OCCURS 68.
       03  FILLER           PIC  X(02)  VALUE  "- ".
*
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SJS0042L".
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
 RUI-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       RUISEKF.
     MOVE    "RUISYUL1"    TO    ERR-FL-ID.
     MOVE     RUI-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*部門取引先Ｍ
 BUT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       BUTOKMF.
     MOVE    "BUTOKMF"     TO    ERR-FL-ID.
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
     DISPLAY  "**  SJS0042L   START  **"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SJS0042L    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN     I-O         DSPF.
     OPEN     INPUT       RUISEKF.
     OPEN     INPUT       BUTOKMF.
     OPEN     INPUT       HJYOKEN.
     OPEN     OUTPUT      PRINTF.
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
*サカタ２０分類テーブル設定
     PERFORM  TB-BNR20-SET-SEC.

*初期画面表示へ
     MOVE  1                TO    MAIN-FLG.
 INIT-EXIT.
     EXIT.
*=============================================================
*  サカタ２０分類テーブル設定処理
*=============================================================
 TB-BNR20-SET-SEC           SECTION.
     INITIALIZE  TB-BNR20-AREA.
     MOVE  ZERO             TO  IX.

     INITIALIZE  JYO-REC.
     MOVE  10               TO  JYO-F01.
     MOVE  LOW-VALUE        TO  FG-HJYOKEN-END.

     PERFORM  HJYOKEN-RD-SEC.
     IF  FG-HJYOKEN-END = "END"
         DISPLAY "SAKATA 20BUNRUI DATA NOT FOUND ???"
             UPON CONS
         MOVE  "4000"       TO  PROGRAM-STATUS
         EXIT PROGRAM
     END-IF.

     PERFORM  UNTIL  FG-HJYOKEN-END = "END"
       IF  IX = 20
           DISPLAY "BUNR20 TBL OVER ???"  UPON CONS
           MOVE  "4000"     TO  PROGRAM-STATUS
           EXIT PROGRAM
       END-IF

       ADD 1  TO IX
       MOVE  JYO-F02        TO  TB-BNR20   (IX)
       MOVE  JYO-F03        TO  TB-BNR20NM (IX)
       PERFORM  HJYOKEN-RD-SEC

     END-PERFORM.

 TB-BNR20-SET-EXIT.
     EXIT.
*=============================================================
*  サカタ２０分類テーブル設定処理
*=============================================================
 HJYOKEN-RD-SEC             SECTION.
     IF  FG-HJYOKEN-END = LOW-VALUE
         MOVE  SPACE        TO  FG-HJYOKEN-END
         START  HJYOKEN  KEY >= JYO-F01
                                JYO-F02
           INVALID
             MOVE  "END"    TO  FG-HJYOKEN-END
             GO TO  HJYOKEN-RD-EXIT
         END-START
     END-IF.

     READ  HJYOKEN
       AT END
         MOVE  "END"        TO  FG-HJYOKEN-END
         GO TO  HJYOKEN-RD-EXIT
     END-READ.

     IF  JYO-F01 NOT = "10"
         MOVE  "END"        TO  FG-HJYOKEN-END
         GO TO  HJYOKEN-RD-EXIT
     END-IF.

 HJYOKEN-RD-EXIT.
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
     CLOSE      DSPF.
     CLOSE      RUISEKF.
     CLOSE      BUTOKMF.
     CLOSE      HJYOKEN.
     CLOSE      PRINTF.
*
     DISPLAY "* RUISEKF (IN)=" IN-CNT   " *" UPON CONS.
     DISPLAY "* RUISEKF (OT)=" OT-CNT   " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" PAGE-CNT " *" UPON CONS.
 END-EXIT.
     EXIT.
*=============================================================
*                画面初期表示処理
*=============================================================
 DSP-INIT-SEC       SECTION.
*初期画面の処理
     MOVE  SPACE            TO    DSP-CONTROL.
     MOVE "FJS00421"        TO    DSP-FMT.
     MOVE  SPACE            TO    DSP-FJS00421.
     PERFORM  OPT-CLR-SEC.
*出荷日
     MOVE  SYS-DATE         TO    DSP-STYMD.
     MOVE  SYS-DATE         TO    DSP-EDYMD.
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
*開始年月日
     MOVE  "2"              TO  LINK-IN-KBN.
     MOVE  DSP-STYMD        TO  LINK-IN-YMD8.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     IF  LINK-OUT-RET NOT = ZERO
         IF  ERR-FLG  =  ZERO
             MOVE  5        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-STYMD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-STYMD
     END-IF.

 CHK-STYMDD-EXIT.
*終了年月日
     MOVE  "2"              TO  LINK-IN-KBN.
     MOVE  DSP-EDYMD        TO  LINK-IN-YMD8.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     IF  LINK-OUT-RET NOT = ZERO
         IF  ERR-FLG = ZERO
             MOVE  5        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-EDYMD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-EDYMD
         GO TO  CHK-ETYMD-EXIT
     END-IF.

     IF       ERR-FLG = ZERO
         AND  DSP-STYMD > DSP-EDYMD
         IF  ERR-FLG = ZERO
             MOVE  2        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-STYMD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-STYMD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-EDYMD
     END-IF.

 CHK-ETYMD-EXIT.
*開始取引先
 CHK-STTOR-EXIT.
*終了取引先
     IF  DSP-STTOR NOT NUMERIC
         MOVE  0            TO  DSP-STTOR
     END-IF.

     IF  DSP-EDTOR NOT NUMERIC
         MOVE  99999999     TO  DSP-EDTOR
     END-IF.

     IF  ERR-FLG NOT = ZERO
         GO TO  CHK-EDTOR-EXIT
     END-IF.

     IF  DSP-STTOR > DSP-EDTOR
         IF  ERR-FLG = ZERO
             MOVE  2        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-STTOR
         MOVE  "R"          TO  EDIT-OPTION OF DSP-STTOR
         MOVE  "R"          TO  EDIT-OPTION OF DSP-EDTOR
     END-IF.

 CHK-EDTOR-EXIT.
*サカタ２０分類
     IF  DSP-BNR20 NOT = SPACE AND "1"
         IF  ERR-FLG = ZERO
             MOVE  6        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-BNR20
         MOVE  "R"          TO  EDIT-OPTION OF DSP-BNR20
     END-IF.

 CHK-20BNR-EXIT.
     IF  ERR-FLG NOT = ZERO
         GO TO  CHK-BODY-EXIT
     END-IF.

     MOVE ZERO               TO  IN-CNT.
     MOVE ZERO               TO  OT-CNT.

     INITIALIZE  RUI-REC.
     MOVE  DSP-STYMD         TO  RUI-F09. *> 出荷日
     MOVE  DSP-STTOR         TO  RUI-F06. *> 振替元/振替先

     MOVE  8                 TO  FG-RUISEKF-END.
     PERFORM  RUI-READ-SEC.
*対象データ存在ＣＨＫ
     IF  FG-RUISEKF-END  =  9
         MOVE  3            TO  ERR-FLG
     END-IF.

 CHK-BODY-EXIT.
     EXIT.
*=============================================================
*                実績集計ファイル（読み込み）
*=============================================================
 RUI-READ-SEC    SECTION.
     IF FG-RUISEKF-END =  8
        MOVE  ZERO          TO  FG-RUISEKF-END

        START  RUISEKF  KEY >= RUI-F09
                               RUI-F06
                               RUI-F24
          INVALID
            MOVE  9            TO  FG-RUISEKF-END
            GO TO  RUI-READ-EXIT
        END-START
     END-IF.

*リード
     READ  RUISEKF NEXT
       AT END
         MOVE  9            TO  FG-RUISEKF-END
         GO TO  RUI-READ-EXIT
     END-READ.

     ADD  1   TO  IN-CNT.

*年月日
     IF  RUI-F09 > DSP-EDYMD
         MOVE 9             TO  FG-RUISEKF-END
         GO TO  RUI-READ-EXIT
     END-IF.
*取引先
     IF  DSP-STYMD = DSP-EDYMD
         IF  RUI-F06 > DSP-EDTOR
             MOVE  9        TO  FG-RUISEKF-END
             GO TO  RUI-READ-EXIT
         END-IF
     ELSE
         IF     RUI-F06 < DSP-STTOR
             OR RUI-F06 > DSP-EDTOR
             GO TO  RUI-READ-SEC
         END-IF
     END-IF.
*倉庫
     IF  LINK-DSOKCD = "01"
         CONTINUE
     ELSE
         IF  RUI-F17 = LINK-SOKCD
             CONTINUE
         ELSE
             GO TO  RUI-READ-SEC
         END-IF
     END-IF.

*  RUI-F02、伝票区分
*    40:売上
*    41:返品
*    42:値引

     IF  RUI-F02 = 40 OR 41 OR 42
         CONTINUE
     ELSE
         GO TO  RUI-READ-SEC
     END-IF.

*キー退避
     MOVE  RUI-F09          TO  INF-F09.
     MOVE  RUI-F06          TO  INF-F06.
     MOVE  RUI-F24          TO  INF-F24.

     ADD  1   TO  OT-CNT.

 RUI-READ-EXIT.
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
     WRITE DSP-FJS00421.
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
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-STYMD.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-EDYMD.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-STTOR.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-EDTOR.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-BNR20.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-STYMD.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-EDYMD.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-STTOR.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-EDTOR.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-BNR20.

 OPT-CLR-EXIT.
     EXIT.
*=============================================================
*                帳票出力
*=============================================================
 PRINT-SEC           SECTION.

     MOVE ZERO               TO  IN-CNT.
     MOVE ZERO               TO  OT-CNT.

     INITIALIZE  RUI-REC.
     MOVE 66                TO  LINE-CNT.

     MOVE  DSP-STYMD         TO  RUI-F09. *> 年月日
     MOVE  DSP-STTOR         TO  RUI-F06. *> 取引先ＣＤ
     MOVE  8                 TO  FG-RUISEKF-END.

     PERFORM  RUI-READ-SEC.

     IF  FG-RUISEKF-END = 9
         MOVE  3            TO  ERR-FLG
     END-IF.

     PERFORM  UNTIL FG-RUISEKF-END = 9
       PERFORM  PRINTB-SEC

     END-PERFORM.

     MOVE  99               TO  MAIN-FLG.

 PRINT-EXIT.
     EXIT.
*=============================================================
*  帳票出力Ｂ
*=============================================================
 PRINTB-SEC           SECTION.

     INITIALIZE  WK-T-AREA.

*  集計の先頭レコードを退避。
     MOVE  RUI-REC          TO  RUIW-REC.

     MOVE  RUI-F09      TO  BRK-F09 *> 出荷日

     PERFORM  UNTIL FG-RUISEKF-END = 9
                 OR RUI-F09    NOT = BRK-F09 *> 出荷日
       PERFORM  PRINTC-SEC

     END-PERFORM.

     PERFORM  GOKEI2-PRINT-SEC.
     MOVE 66                TO  LINE-CNT.

 PRINTB-EXIT.
     EXIT.

*=============================================================
*  帳票出力Ｃ
*=============================================================
 PRINTC-SEC           SECTION.

     INITIALIZE  WK-S-AREA.
     INITIALIZE  WK-G-AREA.
     MOVE  RUI-F06          TO  BRK-F06. *> 振替元/振替先
     *> 集計の先頭レコードを退避。
     MOVE  RUI-REC          TO  RUIW-REC.

     PERFORM  UNTIL FG-RUISEKF-END = 9
                 OR RUI-F09    NOT = BRK-F09 *> 出荷日
                 OR RUI-F06    NOT = BRK-F06 *> 振替元/振替先
       PERFORM  SUM-SEC
       PERFORM  RUI-READ-SEC

     END-PERFORM.

     PERFORM  PRINTD-SEC.

 PRINTC-EXIT.
     EXIT.
*=============================================================
*  入力データ集計処理
*=============================================================
 SUM-SEC           SECTION.
     MOVE  ZERO             TO  IX-BNR20.

     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 20
       IF  TB-BNR20 (IX) = RUI-F24
           MOVE  IX         TO  IX-BNR20
           MOVE  20         TO  IX
       END-IF
     END-PERFORM.

     IF IX-BNR20 = ZERO
        *> 範囲外があった場合その他（CD="20"）とする。
        MOVE  19            TO  IX-BNR20
     END-IF.

* 赤黒区分の判定（奇数は赤伝）
     DIVIDE 2  INTO RUI-F05
         GIVING    WK-SHO
         REMAINDER WK-AMARI.

     EVALUATE  TRUE
       WHEN  RUI-F02 = 40 *> 売上
         IF WK-AMARI = ZERO
            ADD  RUI-F13    TO  WK-S-SURYO  (IX-BNR20)
            ADD  RUI-F15    TO  WK-S-KINGAK (IX-BNR20)
            ADD  RUI-F13    TO  WK-G-SURYO
            ADD  RUI-F15    TO  WK-G-KINGAK
         ELSE
            COMPUTE  WK-S-SURYO  (IX-BNR20) =
                WK-S-SURYO  (IX-BNR20) + (RUI-F13 * -1)
            COMPUTE  WK-S-KINGAK (IX-BNR20) =
                WK-S-KINGAK (IX-BNR20) + (RUI-F15 * -1)
            COMPUTE  WK-G-SURYO       =
                WK-G-SURYO       + (RUI-F13 * -1)
            COMPUTE  WK-G-KINGAK      =
                WK-G-KINGAK      + (RUI-F15 * -1)
         END-IF

       WHEN  RUI-F02 = 41 *> 返品
         IF WK-AMARI = ZERO
            ADD  RUI-F13    TO  WK-S-HEPSU  (IX-BNR20)
            ADD  RUI-F15    TO  WK-S-HEPGAK (IX-BNR20)
            ADD  RUI-F13    TO  WK-G-HEPSU
            ADD  RUI-F15    TO  WK-G-HEPGAK
         ELSE
            COMPUTE  WK-S-HEPSU  (IX-BNR20) =
                WK-S-HEPSU  (IX-BNR20) + (RUI-F13 * -1)
            COMPUTE  WK-S-HEPGAK (IX-BNR20) =
                WK-S-HEPGAK (IX-BNR20) + (RUI-F15 * -1)
            COMPUTE  WK-G-HEPSU       =
                WK-G-HEPSU       + (RUI-F13 * -1)
            COMPUTE  WK-G-HEPGAK      =
                WK-G-HEPGAK      + (RUI-F15 * -1)
         END-IF

       WHEN  RUI-F02 = 42 *> 値引
         IF WK-AMARI = ZERO
            ADD  RUI-F13    TO  WK-S-NEBSU  (IX-BNR20)
            ADD  RUI-F15    TO  WK-S-NEBGAK (IX-BNR20)
            ADD  RUI-F13    TO  WK-G-NEBSU
            ADD  RUI-F15    TO  WK-G-NEBGAK
         ELSE
            COMPUTE  WK-S-NEBSU  (IX-BNR20) =
                WK-S-NEBSU  (IX-BNR20) + (RUI-F13 * -1)
            COMPUTE  WK-S-NEBGAK (IX-BNR20) =
                WK-S-NEBGAK (IX-BNR20) + (RUI-F15 * -1)
            COMPUTE  WK-G-NEBSU       =
                WK-G-NEBSU       + (RUI-F13 * -1)
            COMPUTE  WK-G-NEBGAK      =
                WK-G-NEBGAK      + (RUI-F15 * -1)
         END-IF

     END-EVALUATE.

 SUM-EXIT.
     EXIT.
*=============================================================
*  帳票出力（明細、取引先合計）
*=============================================================
 PRINTD-SEC            SECTION.
     PERFORM  BODY-PRINT-SEC.
     PERFORM  GOKEI-PRINT-SEC.

* 総合計の集計
     COMPUTE  WK-T-SURYO  = WK-T-SURYO  + WK-G-SURYO.
     COMPUTE  WK-T-KINGAK = WK-T-KINGAK + WK-G-KINGAK.

     COMPUTE  WK-T-HEPSU  = WK-T-HEPSU  + WK-G-HEPSU.
     COMPUTE  WK-T-HEPGAK = WK-T-HEPGAK + WK-G-HEPGAK.

     COMPUTE  WK-T-NEBSU  = WK-T-NEBSU  + WK-G-NEBSU.
     COMPUTE  WK-T-NEBGAK = WK-T-NEBGAK + WK-G-NEBGAK.

 PRINTD-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 BODY-PRINT-SEC             SECTION.
     IF  DSP-BNR20 = "1" *> サカタ２０分類実績分のみ
         IF      WK-G-SURYO   = ZERO
             AND WK-G-KINGAK  = ZERO
             AND WK-G-HEPSU   = ZERO
             AND WK-G-HEPGAK  = ZERO
             AND WK-G-NEBSU   = ZERO
             AND WK-G-NEBGAK  = ZERO
             GO TO  BODY-PRINT-EXIT
         END-IF
     END-IF.

     IF  DSP-BNR20 = "1" *> サカタ２０分類実績分のみ
         MOVE LINE-CNT           TO  WK-LINE-CNT
         ADD  1  TO WK-LINE-CNT  *> 取引先ＣＤ
         PERFORM  VARYING IX  FROM 1 BY 1 *> 明細
                  UNTIL   IX > 20
           IF     WK-S-SURYO  (IX) = ZERO
              AND WK-S-KINGAK (IX) = ZERO
              AND WK-S-HEPSU  (IX) = ZERO
              AND WK-S-HEPGAK (IX) = ZERO
              AND WK-S-NEBSU  (IX) = ZERO
              AND WK-S-NEBGAK (IX) = ZERO
              CONTINUE
           ELSE
             ADD  1  TO WK-LINE-CNT
           END-IF

         END-PERFORM

         ADD  2  TO WK-LINE-CNT *> 取引先計

         PERFORM  HEAD-PRINT-SEC
     ELSE                *> サカタ２０分類全て
         COMPUTE  WK-LINE-CNT = LINE-CNT + 23
         PERFORM  HEAD-PRINT-SEC
     END-IF.

     PERFORM  BODY-PRINTB-SEC. *> 取引先行

     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 20
       PERFORM  BODY-PRINTB2-SEC *> 明細行

     END-PERFORM.

 BODY-PRINT-EXIT.
     EXIT.
*=============================================================
*                明細印刷Ｂ処理（取引先）
*=============================================================
 BODY-PRINTB-SEC             SECTION.

     IF  DSP-BNR20 = "1" *> サカタ２０分類実績分のみ
         COMPUTE  WK-LINE-CNT = LINE-CNT + 1
         PERFORM  HEAD-PRINT-SEC
     END-IF.

     MOVE  RUIW-F06         TO  BUT-F01.
     READ  BUTOKMF
       INVALID
         MOVE 1             TO  FG-BUTOKMF-INV
       NOT INVALID
         MOVE ZERO          TO  FG-BUTOKMF-INV
     END-READ.

     MOVE  SPACE            TO  MEISAI1.

     MOVE  RUIW-F06         TO  PRT-TOKCD.
     IF FG-BUTOKMF-INV = ZERO
        MOVE  BUT-F02       TO  PRT-TOKNM
     END-IF.

     WRITE  P-REC  FROM MEISAI1  AFTER 1.
     ADD  1   TO  LINE-CNT.

 BODY-PRINTB-EXIT.
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
 HEAD-PRINTB-SEC      SECTION.
     ADD  1   TO  PAGE-CNT.
     MOVE  PAGE-CNT         TO  H1-PAGE.

     MOVE  RUIW-F09 (1:4)   TO  H2-YY.
     MOVE  RUIW-F09 (5:2)   TO  H2-MM.
     MOVE  RUIW-F09 (7:2)   TO  H2-DD.

     IF  PAGE-CNT NOT = 1
         MOVE  SPACE        TO  P-REC
         WRITE  P-REC  AFTER PAGE
     END-IF.

     WRITE  P-REC  FROM MIDASI1    AFTER 2.
     WRITE  P-REC  FROM MIDASI2    AFTER 1.
     WRITE  P-REC  FROM MIDASI9    AFTER 1.
     WRITE  P-REC  FROM MIDASI3    AFTER 1.
     WRITE  P-REC  FROM MIDASI3-2  AFTER 1.
     WRITE  P-REC  FROM MIDASI9    AFTER 1.

     MOVE  8                TO  LINE-CNT.
 HEAD-PRINTB-EXIT.
     EXIT.
*=============================================================
*  明細印刷Ｂ２処理（明細）
*=============================================================
 BODY-PRINTB2-SEC             SECTION.
     IF  DSP-BNR20 = "1" *> サカタ２０分類実績分のみ
         IF      WK-S-SURYO  (IX) = ZERO
             AND WK-S-KINGAK (IX) = ZERO
             AND WK-S-HEPSU  (IX) = ZERO
             AND WK-S-HEPGAK (IX) = ZERO
             AND WK-S-NEBSU  (IX) = ZERO
             AND WK-S-NEBGAK (IX) = ZERO
             GO TO  BODY-PRINTB2-EXIT
         END-IF
     END-IF.

     IF  DSP-BNR20 = "1" *> サカタ２０分類実績分のみ
         COMPUTE  WK-LINE-CNT = LINE-CNT + 1
         PERFORM  HEAD-PRINT-SEC

     END-IF.

     MOVE  SPACE            TO  MEISAI2.

     MOVE  TB-BNR20    (IX) TO  PRT2-BNR20.
     MOVE  TB-BNR20NM  (IX) TO  PRT2-BNR20NM.
     MOVE  WK-S-SURYO  (IX) TO  PRT2-SURYO.
     MOVE  WK-S-KINGAK (IX) TO  PRT2-KINGAK.
     MOVE  WK-S-HEPSU  (IX) TO  PRT2-HEPSU.
     MOVE  WK-S-HEPGAK (IX) TO  PRT2-HEPGAK.
     MOVE  WK-S-NEBSU  (IX) TO  PRT2-NEBSU.
     MOVE  WK-S-NEBGAK (IX) TO  PRT2-NEBGAK.
     COMPUTE  PRT2-SA-SU = WK-S-SURYO (IX) - WK-S-HEPSU (IX).
     COMPUTE  PRT2-SA-GAK =
         WK-S-KINGAK (IX) - WK-S-HEPGAK (IX) - WK-S-NEBGAK (IX).
*印刷
     WRITE  P-REC  FROM MEISAI2  AFTER 1.
     ADD  1   TO  LINE-CNT.

 BODY-PRINTB2-EXIT.
     EXIT.
*=============================================================
*  合計印刷処理（取引先計）
*=============================================================
 GOKEI-PRINT-SEC            SECTION.
     IF  DSP-BNR20 = "1" *> サカタ２０分類実績分のみ
         COMPUTE  WK-LINE-CNT = LINE-CNT + 2
         PERFORM  HEAD-PRINT-SEC

     END-IF.

     MOVE  SPACE            TO  MEISAI2.

     MOVE  NC"　　　　　　　　　　　取引先計"
                            TO  PRT2-BNR20NM.
     MOVE  WK-G-SURYO       TO  PRT2-SURYO.
     MOVE  WK-G-HEPSU       TO  PRT2-HEPSU.
     MOVE  WK-G-NEBSU       TO  PRT2-NEBSU.
     COMPUTE  PRT2-SA-SU = WK-G-SURYO - WK-G-HEPSU.

     MOVE  WK-G-KINGAK      TO  PRT2-KINGAK.
     MOVE  WK-G-HEPGAK      TO  PRT2-HEPGAK.
     MOVE  WK-G-NEBGAK      TO  PRT2-NEBGAK.
     COMPUTE  PRT2-SA-GAK =
         WK-G-KINGAK - WK-G-HEPGAK - WK-G-NEBGAK.
*印刷
     WRITE  P-REC  FROM MEISAI2  AFTER 1.
     WRITE  P-REC  FROM MEISAI9  AFTER 1.
     ADD  2   TO  LINE-CNT.


 GOKEI-PRINT-EXIT.
     EXIT.
*=============================================================
*  合計印刷処理（総合計）
*=============================================================
 GOKEI2-PRINT-SEC           SECTION.
     COMPUTE  WK-LINE-CNT = LINE-CNT + 2.
     PERFORM  HEAD-PRINT-SEC.

     MOVE  SPACE            TO  MEISAI2.

     MOVE  NC"　　　　　　　　　　　総合計　"
                            TO  PRT2-BNR20NM.
     MOVE  WK-T-SURYO       TO  PRT2-SURYO.
     MOVE  WK-T-HEPSU       TO  PRT2-HEPSU.
     MOVE  WK-T-NEBSU       TO  PRT2-NEBSU.
     COMPUTE  PRT2-SA-SU = WK-T-SURYO - WK-T-HEPSU.

     MOVE  WK-T-KINGAK      TO  PRT2-KINGAK.
     MOVE  WK-T-HEPGAK      TO  PRT2-HEPGAK.
     MOVE  WK-T-NEBGAK      TO  PRT2-NEBGAK.
     COMPUTE  PRT2-SA-GAK =
         WK-T-KINGAK - WK-T-HEPGAK - WK-T-NEBGAK.
*印刷
     WRITE  P-REC  FROM MEISAI2  AFTER 1.
     WRITE  P-REC  FROM MEISAI9  AFTER 1.
     ADD  2   TO  LINE-CNT.


 GOKEI2-PRINT-EXIT.
     EXIT.
