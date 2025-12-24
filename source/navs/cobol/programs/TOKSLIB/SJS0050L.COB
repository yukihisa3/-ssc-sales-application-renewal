****************************************************************
*                                                              *
*    ユーザ　　　　名：　サカタのたね　　　殿                  *
*    システム　　　名：　実績管理システム                      *
*    プログラム　　名：　取引先別商品別売上実績表              *
*    作成者　　　　　：　ＮＡＶ　　　　　                      *
*    作成日　　　　　：　2000.06.12      UPDATE: YYYY.MM.DD    *
*                                                              *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SJS0050L.
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
     SELECT   JISSYUF        ASSIGN        TO  01-VI-JISSYUL2
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  DYNAMIC
                             RECORD KEY    IS  JIS-F01
                                               JIS-F02
                                               JIS-F03
                                               JIS-F05
                                           WITH DUPLICATES
                             FILE STATUS   IS  JIS-STA.
*部門取引先マスタ
     SELECT   BUTOKMF        ASSIGN        TO  01-VI-BUTOKML1
                             ORGANIZATION  IS  INDEXED
                             ACCESS  MODE  IS  RANDOM
                             RECORD  KEY   IS  BUT-F01
                             FILE STATUS   IS  BUT-STA.
*商品名称マスタ
     SELECT   MEIMS          ASSIGN        TO  01-VI-MEIMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  MEI-F011
                                               MEI-F012
                             FILE STATUS   IS  MEI-STA.

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
     COPY     FJS00501  OF  XMDLIB
     JOINING  DSP      AS  PREFIX.
*実績集計ファイル
 FD  JISSYUF.
     COPY     JISSYUF  OF  XFDLIB
     JOINING  JIS      AS  PREFIX.
*部門取引先Ｍ
 FD  BUTOKMF.
     COPY     BUTOKMF  OF  XFDLIB
     JOINING  BUT      AS  PREFIX.
*商品名称Ｍ
 FD  MEIMS.
     COPY     HMEIMS   OF  XFDLIB
     JOINING  MEI      AS  PREFIX.
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
     03  MEI-STA             PIC  X(02).
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
     03  JIS-FLG             PIC  9(01)  VALUE  ZERO.
*
 01  CNT-AREA.
     03  PAGE-CNT            PIC  9(07)  VALUE  ZERO.
     03  LINE-CNT            PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  62.
     03  IN-CNT              PIC  9(07)  VALUE  ZERO.
     03  OT-CNT              PIC  9(07)  VALUE  ZERO.
*商品コード右詰め
 01  WK-SHOCD.
     03  WK-SHO              PIC  X(01)  OCCURS 8.
 01  IX                      PIC  9(02).
*キー領域
 01  NEW-KEY.
     03  NEW-F02.
       05  NEW-F021          PIC  9(04).
       05  NEW-F022          PIC  9(02).
     03  NEW-F03             PIC  9(08).
     03  NEW-F05.
       05  NEW-F051          PIC  X(08).
       05  NEW-F052          PIC  X(08).
 01  OLD-KEY.
     03  OLD-F02.
       05  OLD-F021          PIC  9(04).
       05  OLD-F022          PIC  9(02).
     03  OLD-F03             PIC  9(08).
     03  OLD-F05.
       05  OLD-F051          PIC  X(08).
       05  OLD-F052          PIC  X(08).
*集計用領域
 01  WK-SYUKEI-AREA.
     03  WK-S-AREA.
       05  WK-S-SURYO        PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-S-KINGAK       PIC S9(13)     PACKED-DECIMAL.
       05  WK-S-HEPSU        PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-S-HEPGAK       PIC S9(13)     PACKED-DECIMAL.
       05  WK-S-NEBIK        PIC S9(13)     PACKED-DECIMAL.
     03  WK-G-AREA.
       05  WK-G-SURYO        PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-G-KINGAK       PIC S9(13)     PACKED-DECIMAL.
       05  WK-G-HEPSU        PIC S9(13)V99  PACKED-DECIMAL.
       05  WK-G-HEPGAK       PIC S9(13)     PACKED-DECIMAL.
       05  WK-G-NEBIK        PIC S9(13)     PACKED-DECIMAL.
*
****  見出し行１             ****
 01  MIDASI1        CHARACTER     TYPE   IS   YA.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SJS0050L".
     02  FILLER              PIC  X(35)  VALUE  SPACE.
     02  FILLER              PIC  N(18)  VALUE
         NC"※※　取引先別商品別売上実績表　※※".
     02  FILLER              PIC  X(34)  VALUE  SPACE.
     02  H1-YY               PIC  9(04).
     02  FILLER              PIC  N(01)  VALUE  NC"年".
     02  H1-MM               PIC  Z9.
     02  FILLER              PIC  N(01)  VALUE  NC"月".
     02  H1-DD               PIC  Z9.
     02  FILLER              PIC  N(01)  VALUE  NC"日".
     02  H1-PAGE             PIC  ZZZ9.
     02  FILLER              PIC  N(01)  VALUE  NC"頁".
****  見出し行２             ****
 01  MIDASI2        CHARACTER     TYPE   IS   YA.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE  NC"対象年月：".
     02  H2-YY               PIC  9(04).
     02  FILLER              PIC  N(01)  VALUE  NC"年".
     02  H2-MM               PIC  Z9.
     02  FILLER              PIC  N(02)  VALUE  NC"月度".
****  見出し行３             ****
 01  MIDASI3        CHARACTER     TYPE   IS   YA.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE  NC"　取引先：".
     02  H3-TOKCD            PIC  9(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H3-TOKNM            PIC  N(15).
****  見出し行４             ****
 01  MIDASI4        CHARACTER     TYPE   IS   YA.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"商品コード".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"商品名".
     02  FILLER              PIC  X(46)  VALUE  "(ｶﾅ)".
     02  FILLER              PIC  N(04)  VALUE
         NC"売上数量".
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"返品数量".
     02  FILLER              PIC  X(26)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"数量合計".
****  見出し行５             ****
 01  MIDASI5        CHARACTER     TYPE   IS   YA.
     02  FILLER              PIC  X(19)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"商品名".
     02  FILLER              PIC  X(01)  VALUE  "(".
     02  FILLER              PIC  N(02)  VALUE
         NC"漢字".
     02  FILLER              PIC  X(41)  VALUE  ")".
     02  FILLER              PIC  N(04)  VALUE
         NC"売上金額".
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"返品金額".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"値引金額".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"金額合計".
****  見出し行９             ****
 01  MIDASI9        CHARACTER     TYPE   IS   YA.
     02  FILLER              PIC  N(68)  VALUE  ALL NC"─".
****  明細行１               ****
 01  MEISAI1        CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01).
     02  PRT-SHOCD           PIC  X(08)B.
     02  PRT-HINTAN          PIC  X(08)B.
     02  PRT-SHOKN1          PIC  X(15).
     02  PRT-SHOKN2          PIC  X(15).
     02  FILLER              PIC  X(03).
     02  PRT-GOKEI           PIC  N(08).
     02  PRT-SURYO           PIC  ---,---,---,--9.99.
     02  PRT-HEPSU           PIC  ---,---,---,--9.99.
     02  FILLER              PIC  X(16).
     02  PRT-SA-SU           PIC  ---,---,---,--9.99.
****  明細行２               ****
 01  MEISAI2        CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(19).
     02  PRT-SHONM1          PIC  N(15).
     02  PRT-SHONM2          PIC  N(15).
     02  PRT-KINGAK          PIC  ---,---,---,--9BBB.
     02  PRT-HEPGAK          PIC  ---,---,---,--9BB.
     02  PRT-NEBIK           PIC  ---,---,---,--9BB.
     02  PRT-SA-GAK          PIC  ---,---,---,--9.
****  明細行９               ****
 01  MEISAI9.
     02  FILLER                   OCCURS  68.
       03  FILLER            PIC  X(01)  VALUE  "-".
       03  FILLER            PIC  X(01)  VALUE  SPACE.
*
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SJS0050L".
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
     MOVE    "JISSYUL2"    TO    ERR-FL-ID.
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
*商品名称Ｍ
 MEI-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       MEIMS.
     MOVE    "MEIMS1"      TO    ERR-FL-ID.
     MOVE     MEI-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  SJS0050L   START  **"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SJS0050L    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN     I-O         DSPF.
     OPEN     INPUT       JISSYUF.
     OPEN     INPUT       BUTOKMF.
     OPEN     INPUT       MEIMS.
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
     CLOSE      DSPF.
     CLOSE      JISSYUF.
     CLOSE      BUTOKMF.
     CLOSE      MEIMS.
     CLOSE      PRINTF.
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
     MOVE "FJS00501"        TO    DSP-FMT.
     MOVE  SPACE            TO    DSP-FJS00501.
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
*年月
     MOVE     "2"                 TO   LINK-IN-KBN.
     MOVE     DSP-STYM            TO   LINK-IN-YMD8(1:6).
     MOVE     "01"                TO   LINK-IN-YMD8(7:2).
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF        LINK-OUT-RET  =  ZERO
         CONTINUE
     ELSE
         IF  ERR-FLG  =  ZERO
             MOVE   5        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-STYM
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-STYM
     END-IF.
     MOVE     "2"                 TO   LINK-IN-KBN.
     MOVE     DSP-EDYM            TO   LINK-IN-YMD8(1:6).
     MOVE     "01"                TO   LINK-IN-YMD8(7:2).
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF        LINK-OUT-RET  =  ZERO
         CONTINUE
     ELSE
         IF  ERR-FLG  =  ZERO
             MOVE   5        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-EDYM
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-EDYM
     END-IF.
     IF        ERR-FLG      =   ZERO   AND
               DSP-STYM     >   DSP-EDYM
         IF  ERR-FLG  =  ZERO
             MOVE   2        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-STYM
                                 EDIT-OPTION  OF  DSP-EDYM
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-STYM
     END-IF.
*取引先
     IF        DSP-STTOR   NOT  NUMERIC
         MOVE  0             TO  DSP-STTOR
     END-IF.
     IF        DSP-EDTOR   NOT  NUMERIC
         MOVE  99999999      TO  DSP-EDTOR
     END-IF.
     IF        DSP-STTOR    >   DSP-EDTOR
         IF  ERR-FLG  =  ZERO
             MOVE   2        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-STTOR
                                 EDIT-OPTION  OF  DSP-EDTOR
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-STTOR
     END-IF.
*商品
     PERFORM   SHO-SET-SEC.
     IF        DSP-EDSHO    =   SPACE
         MOVE  "99999999"    TO  DSP-EDSHO
     END-IF.
     IF        DSP-STSHO    >   DSP-EDSHO
         IF  ERR-FLG  =  ZERO
             MOVE   2        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-STSHO
                                 EDIT-OPTION  OF  DSP-EDSHO
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-STSHO
     END-IF.
 CHK-BODY-EXIT.
     EXIT.
*=============================================================
*                商品コード右詰
*=============================================================
 SHO-SET-SEC        SECTION.
     IF      DSP-STSHO   NOT =  SPACE
             MOVE    DSP-STSHO       TO  WK-SHOCD
             PERFORM              UNTIL  WK-SHO(8) NOT = SPACE
                  PERFORM VARYING IX FROM 7 BY -1 UNTIL IX = 0
                        MOVE WK-SHO(IX)  TO  WK-SHO(IX + 1)
                  END-PERFORM
                  MOVE    ZERO       TO  WK-SHO(1)
             END-PERFORM
             MOVE  WK-SHOCD          TO  DSP-STSHO
     END-IF.
     IF      DSP-EDSHO   NOT =  SPACE
             MOVE    DSP-EDSHO       TO  WK-SHOCD
             PERFORM              UNTIL  WK-SHO(8) NOT = SPACE
                  PERFORM VARYING IX FROM 7 BY -1 UNTIL IX = 0
                        MOVE WK-SHO(IX)  TO  WK-SHO(IX + 1)
                  END-PERFORM
                  MOVE    ZERO       TO  WK-SHO(1)
             END-PERFORM
             MOVE  WK-SHOCD          TO  DSP-EDSHO
     END-IF.
 SHO-SET-EXIT.
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
     MOVE  ZERO      TO      ERR-FLG.
*実績集計ファイル（スタート）
     MOVE  ZERO              TO  JIS-FLG.
     INITIALIZE                  JIS-REC.
     MOVE  "1"               TO  JIS-F01.
     MOVE  DSP-STYM          TO  JIS-F02.
     MOVE  DSP-STTOR         TO  JIS-F03.
     MOVE  DSP-STSHO         TO  JIS-F051.
     START JISSYUF   KEY >=      JIS-F01
                                 JIS-F02
                                 JIS-F03
                                 JIS-F05
         INVALID
            MOVE   9         TO  JIS-FLG
         NOT  INVALID
*実績集計ファイル（読み込み）
            PERFORM  JIS-READ-SEC
     END-START.
*対象データ存在ＣＨＫ
     IF  JIS-FLG  =  9
         MOVE    3        TO    ERR-FLG
     END-IF.
*
     IF  ERR-FLG  =  ZERO
         MOVE    4        TO    ERR-FLG
         PERFORM  DSP-WT-SEC
         MOVE    4        TO    MAIN-FLG
         INITIALIZE             WK-SYUKEI-AREA
     ELSE
*****    MOVE    2        TO    MAIN-FLG
         MOVE    99       TO    MAIN-FLG
     END-IF.
 CHK-KAKU-EXIT.
     EXIT.
*=============================================================
*                実績集計ファイル（読み込み）
*=============================================================
 JIS-READ-SEC    SECTION.
*リード
     READ  JISSYUF     NEXT
         AT END
           MOVE   9        TO  JIS-FLG
           GO              TO  JIS-READ-EXIT
     END-READ.
     ADD   1                     TO  IN-CNT.
*売上仕入区分
     IF   JIS-F01  NOT =  "1"
           MOVE   9        TO  JIS-FLG
           GO              TO  JIS-READ-EXIT
     END-IF.
*年月
     IF   JIS-F02     >   DSP-EDYM
           MOVE   9        TO  JIS-FLG
           GO              TO  JIS-READ-EXIT
     END-IF.
*取引先
     IF   JIS-F02     =   DSP-EDYM
     AND  JIS-F03     >   DSP-EDTOR
           MOVE   9        TO  JIS-FLG
           GO              TO  JIS-READ-EXIT
     END-IF.
     IF   JIS-F03     <   DSP-STTOR
     OR   JIS-F03     >   DSP-EDTOR
           GO              TO  JIS-READ-SEC
     END-IF.
*商品
     IF   JIS-F02     =   DSP-EDYM
     AND  JIS-F03     =   DSP-EDTOR
     AND  JIS-F051    >   DSP-EDSHO
           MOVE   9        TO  JIS-FLG
           GO              TO  JIS-READ-EXIT
     END-IF.
     IF   JIS-F051    <   DSP-STSHO
     OR   JIS-F051    >   DSP-EDSHO
           GO              TO  JIS-READ-SEC
     END-IF.
*倉庫
     IF     LINK-DSOKCD NOT =  "01"
       IF   LINK-SOKCD  NOT =  JIS-F04
           GO              TO  JIS-READ-SEC
       END-IF
     END-IF.
*キー退避
     MOVE  JIS-F02               TO  NEW-F02.
     MOVE  JIS-F03               TO  NEW-F03.
     MOVE  JIS-F05               TO  NEW-F05.
     ADD   1                     TO  OT-CNT.
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
     WRITE DSP-FJS00501.
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
     MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-STYM
                             EDIT-CURSOR  OF  DSP-EDYM
                             EDIT-CURSOR  OF  DSP-STTOR
                             EDIT-CURSOR  OF  DSP-EDTOR
                             EDIT-CURSOR  OF  DSP-STSHO
                             EDIT-CURSOR  OF  DSP-EDSHO.
     MOVE "M"            TO  EDIT-OPTION  OF  DSP-STYM
                             EDIT-OPTION  OF  DSP-EDYM
                             EDIT-OPTION  OF  DSP-STTOR
                             EDIT-OPTION  OF  DSP-EDTOR
                             EDIT-OPTION  OF  DSP-STSHO
                             EDIT-OPTION  OF  DSP-EDSHO.
 OPT-CLR-EXIT.
     EXIT.
*=============================================================
*                帳票出力
*=============================================================
 PRINT-SEC           SECTION.
     MOVE  NEW-KEY          TO   OLD-KEY.
*商品で集計
     ADD   JIS-F06          TO   WK-S-SURYO.
     ADD   JIS-F07          TO   WK-S-KINGAK.
     ADD   JIS-F08          TO   WK-S-HEPSU.
     ADD   JIS-F09          TO   WK-S-HEPGAK.
     ADD   JIS-F10          TO   WK-S-NEBIK.
*実績集計ファイル（読み込み）
     PERFORM  JIS-READ-SEC.
*明細行の出力
     IF    JIS-FLG      =  9
     OR    OLD-KEY  NOT =  NEW-KEY
           PERFORM  BODY-PRINT-SEC
     END-IF.
*合計行の出力
     IF    JIS-FLG      =  9
     OR    OLD-F02  NOT =  NEW-F02
     OR    OLD-F03  NOT =  NEW-F03
           PERFORM  GOKEI-PRINT-SEC
     END-IF.
     IF    JIS-FLG      =  9
           MOVE   99         TO   MAIN-FLG
     END-IF.
 PRINT-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 BODY-PRINT-SEC             SECTION.
*改ページ
     IF    LINE-CNT    >=   MAX-LINE
     OR    PAGE-CNT     =   ZERO
           PERFORM  HEAD-PRINT-SEC
     END-IF.
*明細部の編集
     MOVE  SPACE            TO   MEISAI1  MEISAI2.
*商品
     MOVE  OLD-F051         TO   MEI-F011 PRT-SHOCD.
     MOVE  OLD-F052         TO   MEI-F012 PRT-HINTAN.
     READ  MEIMS
         INVALID  KEY
             CONTINUE
         NOT INVALID  KEY
             MOVE  MEI-F031 TO   PRT-SHOKN1
             MOVE  MEI-F032 TO   PRT-SHOKN2
             MOVE  MEI-F021 TO   PRT-SHONM1
             MOVE  MEI-F022 TO   PRT-SHONM2
     END-READ.
*
     MOVE  WK-S-SURYO       TO   PRT-SURYO.
     MOVE  WK-S-HEPSU       TO   PRT-HEPSU.
     COMPUTE  PRT-SA-SU   =  WK-S-SURYO   -  WK-S-HEPSU.
*
     MOVE  WK-S-KINGAK      TO   PRT-KINGAK.
     MOVE  WK-S-HEPGAK      TO   PRT-HEPGAK.
     MOVE  WK-S-NEBIK       TO   PRT-NEBIK.
     COMPUTE  PRT-SA-GAK  =  WK-S-KINGAK
                           - WK-S-HEPGAK  -  WK-S-NEBIK.
*印刷
     WRITE    P-REC      FROM    MEISAI1    AFTER  1.
     WRITE    P-REC      FROM    MEISAI2    AFTER  1.
     WRITE    P-REC      FROM    MEISAI9    AFTER  1.
     ADD      3            TO    LINE-CNT.
*合計加算
     ADD      WK-S-SURYO    TO   WK-G-SURYO.
     ADD      WK-S-KINGAK   TO   WK-G-KINGAK.
     ADD      WK-S-HEPSU    TO   WK-G-HEPSU.
     ADD      WK-S-HEPGAK   TO   WK-G-HEPGAK.
     ADD      WK-S-NEBIK    TO   WK-G-NEBIK.
     INITIALIZE             WK-S-AREA.
 BODY-PRINT-EXIT.
     EXIT.
*=============================================================
*                ＨＥＡＤ部　印刷処理
*=============================================================
 HEAD-PRINT-SEC      SECTION.
     ADD   1                     TO  PAGE-CNT.
     MOVE  PAGE-CNT              TO  H1-PAGE.
*
     MOVE  OLD-F021              TO  H2-YY.
     MOVE  OLD-F022              TO  H2-MM.
*取引先
     MOVE  OLD-F03          TO   BUT-F01  H3-TOKCD.
     READ  BUTOKMF
         INVALID  KEY
             MOVE  ALL NC"＊"    TO   H3-TOKNM
         NOT INVALID  KEY
             MOVE  BUT-F02       TO   H3-TOKNM
     END-READ.
*
     IF      PAGE-CNT NOT =   1
             MOVE     SPACE   TO      P-REC
             WRITE    P-REC   AFTER   PAGE
     END-IF.
     WRITE   P-REC     FROM    MIDASI1     AFTER  2.
     WRITE   P-REC     FROM    MIDASI2     AFTER  1.
     WRITE   P-REC     FROM    MIDASI3     AFTER  1.
     WRITE   P-REC     FROM    MIDASI9     AFTER  1.
     WRITE   P-REC     FROM    MIDASI4     AFTER  1.
     WRITE   P-REC     FROM    MIDASI5     AFTER  1.
     WRITE   P-REC     FROM    MIDASI9     AFTER  1.
*
     MOVE    8                   TO  LINE-CNT.
 HEAD-PRINT-EXIT.
     EXIT.
*=============================================================
*                合計印刷処理
*=============================================================
 GOKEI-PRINT-SEC            SECTION.
*改ページ
     IF    LINE-CNT    >=   MAX-LINE
           PERFORM  HEAD-PRINT-SEC
     END-IF.
*明細部の編集
     MOVE  SPACE            TO   MEISAI1   MEISAI2.
*
     MOVE  NC"　【取引先合計】"  TO  PRT-GOKEI.
*
     MOVE  WK-G-SURYO       TO   PRT-SURYO.
     MOVE  WK-G-HEPSU       TO   PRT-HEPSU.
     COMPUTE  PRT-SA-SU  =  WK-G-SURYO   -  WK-G-HEPSU.
*
     MOVE  WK-G-KINGAK      TO   PRT-KINGAK.
     MOVE  WK-G-HEPGAK      TO   PRT-HEPGAK.
     MOVE  WK-G-NEBIK       TO   PRT-NEBIK.
     COMPUTE  PRT-SA-GAK =  WK-G-KINGAK
                          - WK-G-HEPGAK  -  WK-G-NEBIK.
*印刷
     WRITE    P-REC      FROM    MEISAI1    AFTER  1.
     WRITE    P-REC      FROM    MEISAI2    AFTER  1.
     MOVE     99           TO    LINE-CNT.
*
     INITIALIZE             WK-G-AREA.
 GOKEI-PRINT-EXIT.
     EXIT.
