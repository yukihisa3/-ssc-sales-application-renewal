*************************************************************
* 顧客名　　　　: 株式会社サカタのタネ
* 業務名　　　　: ＥＤＩ受信サーバー
* サブシステム名:
* モジュール名　: 回線環境マスタリスト出力
* 作成日／作成者: 10/08/16  大野
* 処理概要　　　: 回線管理マスタより回線管理マスタリストを
*                出力する.
* 使用ＭＥＤ　　:
*-----------------------------------------------------------*
* 修正日／修正者: 2012/10/31  武井
* 修正内容      : 流通ＢＭＳ対応
*************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNJ0020L.
*
 ENVIRONMENT           DIVISION.
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
          YA      IS   CHR-2
          YB-21   IS   CHR-21
          YB      IS   CHR-15
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*回線管理マスタ
     SELECT  JSMKAIF   ASSIGN    TO   JSMKAIL1
             ORGANIZATION        IS   INDEXED
             ACCESS MODE         IS   SEQUENTIAL
             FILE STATUS         IS   KAI-ST
             RECORD  KEY         IS   KAI-F01
                                      KAI-F02.
*プリントファイル
     SELECT  PRTF      ASSIGN         LP-04.
*
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*環境管理マスタ
 FD  JSMKAIF
                       LABEL     RECORD    IS   STANDARD
                       BLOCK     CONTAINS  41   RECORDS.
                       COPY      JSMKAIL1  OF   XFDLIB
                       JOINING   KAI       AS   PREFIX.
*プリンター
 FD  PRTF
                       LABEL     RECORD    IS   OMITTED.
 01  PRT-REC           PIC  X(200).
*
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*共通領域
 01  PG-ID                       PIC  X(08)   VALUE  "SNJ0020L".
*ステータス領域
 01  STATUS-AREA.
     03  KAI-ST                  PIC  X(02).
*プログラムスイッチ（画面遷移制御）
 01  PSW-AREA.
     03  PSW                     PIC  X(01).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME.
         05  WK-HH               PIC  9(02).
         05  WK-MN               PIC  9(02).
         05  WK-SS               PIC  9(02).
 01  DATE-AREA.
     03  WK-YS                   PIC  9(02).
     03  WK-DATE.
         05  WK-Y                PIC  9(02).
         05  WK-M                PIC  9(02).
         05  WK-D                PIC  9(02).
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                PIC  9(08).
*フラグ
 01  FLG-AREA.
     03  END-FLG                 PIC  X(03)  VALUE  SPACE.
     03  FLG-READ                PIC  X(01)  VALUE  SPACE.
     03  FLG-INV                 PIC  X(01).
*カウンタ領域
 01  COUNTER-AREA.
     03  CNT-PG                  PIC  9(04)  VALUE  ZERO.
     03  L-CNT                   PIC  9(03)  VALUE  ZERO.
*印刷定義エリア
 01  HD1.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  X(08)  VALUE
                                 "SNJ0020L".
     03  FILLER                  PIC  X(27)  VALUE  SPACE.
     03  FILLER                  PIC  N(19)  VALUE
         NC"※※　ＥＤＩ回線管理マスタリスト　※※"
         CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(17)  VALUE  SPACE.
     03  HD1-02                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
         CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
         CHARACTER  TYPE  IS  CHR-2.
     03  HD1-04                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD1-05                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
         CHARACTER  TYPE  IS  CHR-2.
 01  HD2.
     03  FILLER                  PIC  X(22)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"回線種別"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(06)  VALUE  SPACE.
     03  FILLER                  PIC  N(06)  VALUE
                                 NC"回線制御番号"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"端末名"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(07)  VALUE  SPACE.
     03  FILLER                  PIC  X(09)  VALUE
                                 "ﾄﾘｶﾞｰﾌｧｲﾙ".
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"名"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  FILLER                  PIC  X(08)  VALUE
                                 "ｱﾝｻｰﾌｧｲﾙ".
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"名"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  FILLER                  PIC  X(11)  VALUE
                                 "ﾊﾟﾗﾒｰﾀｰﾌｧｲﾙ".
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"名"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  FILLER                  PIC  N(02)  VALUE
                                 NC"使用"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE
                                 "FLG".
     03  FILLER                  PIC  X(18)  VALUE  SPACE.
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"──────────────────".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
 01  DT1                         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(22)  VALUE  SPACE.
     03  DT1-01                  PIC  X(01).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-02                  PIC  N(04).
     03  FILLER                  PIC  X(07)  VALUE  SPACE.
     03  DT1-03                  PIC  9(01).
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  DT1-04                  PIC  X(08).
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  DT1-05                  PIC  X(08).
     03  FILLER                  PIC  X(06)  VALUE  SPACE.
     03  DT1-06                  PIC  X(08).
     03  FILLER                  PIC  X(06)  VALUE  SPACE.
     03  DT1-07                  PIC  X(08).
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  DT1-08                  PIC  9(01).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-09                  PIC  N(03).
     03  FILLER                  PIC  X(18)  VALUE  SPACE.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN                 PIC X(01).
 01  LINK-IN-YMD6                PIC 9(06).
 01  LINK-IN-YMD8                PIC 9(08).
 01  LINK-OUT-RET                PIC X(01).
 01  LINK-OUT-YMD                PIC 9(08).
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 FILEERR-SEC01         SECTION.
     USE     AFTER     EXCEPTION      PROCEDURE     JSMKAIF.
     DISPLAY "**ABEND FILE NAME = JHMKATF  **"         UPON CONS.
     DISPLAY "**ABEND STATUS    = " KAI-ST "       **" UPON CONS.
     ACCEPT  WK-TIME  FROM  TIME.
     DISPLAY "*** " PG-ID " ABEND (" WK-Y "/" WK-M "/"
             WK-D " " WK-HH ":" WK-MN ":" WK-SS ") ***"
             UPON  CONS.
     MOVE    "4010"         TO   PROGRAM-STATUS.
     STOP    RUN.
 END  DECLARATIVES.
*=============================================================
*       0.0       コントロール
*=============================================================
 CONTROL-SEC           SECTION.
***
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG  =  "END".
     PERFORM   END-SEC.
***
     STOP    RUN.
 CONTROL-END.
     EXIT.
*=============================================================
*      1.0      初期処理
*=============================================================
 INIT-SEC              SECTION.
     MOVE   99   TO        L-CNT.
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
     ACCEPT    WK-TIME          FROM   TIME.
     DISPLAY "*** " PG-ID " START (" WK-Y "." WK-M "." WK-D " "
             WK-HH "." WK-MN "." WK-SS ") ***"    UPON  CONS.
**
*ファイルのＯＰＥＮ
     OPEN    INPUT     JSMKAIF.
     OPEN    OUTPUT    PRTF.
*ワークの初期化
     INITIALIZE   FLG-AREA.
*回線種別マスタ
     PERFORM      JSMKAIF-READ-SEC.
*
 INIT-END.
     EXIT.
*=============================================================
*       2.0       メイン処理
*=============================================================
 MAIN-SEC              SECTION.
*ブレイクチェック（行FULL）
     IF     L-CNT     > 60
            PERFORM     HEAD-SUB
     END-IF.
*明細出力
     PERFORM    BODY-SUB.
*回線種別マスタ
     PERFORM      JSMKAIF-READ-SEC.
*
 MAIN-END.
     EXIT.
*=============================================================
*       2.1.1     ＨＥＡＤ部編集処理
*=============================================================
 HEAD-SUB              SECTION.
*改頁判定
     IF      CNT-PG  >   ZERO
             MOVE    SPACE       TO   PRT-REC
             WRITE   PRT-REC     AFTER   PAGE
     END-IF.
     MOVE    ZERO    TO     L-CNT.
*頁カウント
     ADD     1                   TO   CNT-PG.
*作成日付
*年
     MOVE    SYS-DATE(1:4)       TO   HD1-02.
*月
     MOVE    SYS-DATE(5:2)       TO   HD1-03.
*日
     MOVE    SYS-DATE(7:2)       TO   HD1-04.
*頁
     MOVE    CNT-PG              TO   HD1-05.
*ヘッダ行印字
     WRITE   PRT-REC   FROM   HD1   AFTER  1.
     WRITE   PRT-REC   FROM   SEN   AFTER  2.
     WRITE   PRT-REC   FROM   HD2   AFTER  1.
     WRITE   PRT-REC   FROM   SEN   AFTER  1.
**** MOVE    SPACE            TO     PRT-REC.
**** WRITE   PRT-REC   AFTER  1.
     MOVE    5      TO      L-CNT.
*
 HEAD-SUB-END.
     EXIT.
*=============================================================
*       2.1.2     ＢＯＤＹ部  転送処理
*=============================================================
 BODY-SUB            SECTION.
*改頁チェック
     IF    L-CNT     >  60
           PERFORM   HEAD-SUB
     END-IF.
*明細行初期化
     INITIALIZE                              DT1.
     MOVE  SPACE                  TO         DT1.
*回線種別
     MOVE  KAI-F01                TO     DT1-01.
*** 2012/10/31
     MOVE    SPACE                TO     DT1-02.
     IF    DT1-01 = "1"
           MOVE    NC"ＩＳＤＮ"   TO     DT1-02
**** ELSE
     END-IF.
     IF    DT1-01 = "2"
           MOVE    NC"公衆回線"   TO     DT1-02
     END-IF.
     IF    DT1-01 = "3"
           MOVE    NC"ＢＭＳ　"   TO     DT1-02
     END-IF.
***
*回線制御番号
     MOVE  KAI-F02                TO     DT1-03.
*端末名
     MOVE  KAI-F03                TO     DT1-04.
*トリガーファイル名
     MOVE  KAI-F04                TO     DT1-05.
*アンサーファイル名
     MOVE  KAI-F05                TO     DT1-06.
*パラメーターファイル名
     MOVE  KAI-F06                TO     DT1-07.
*使用フラグ
     MOVE  KAI-F07                TO     DT1-08.
     IF    DT1-08 = "1"
           MOVE    NC"使用中"     TO     DT1-09
     ELSE
           MOVE    NC"未使用"     TO     DT1-09
     END-IF.
*明細行印字
     WRITE   PRT-REC   FROM    DT1    AFTER   1.
*明細行カウント
     ADD     1                    TO        L-CNT.
*
 BODY-SUB-END.
     EXIT.
*=============================================================
*       2.3       回線種別マスタ　ＲＥＡＤ
*=============================================================
 JSMKAIF-READ-SEC        SECTION.
*
     READ    JSMKAIF
             AT  END
             MOVE    "END"       TO   END-FLG
     END-READ.
*
 JSMKAIF-READ-EXIT.
     EXIT.
*=============================================================
*       3.0       終了処理
*=============================================================
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             JSMKAIF
                       PRTF.
**
     DISPLAY "*** " "出力枚数" " = " CNT-PG " "
             "ページ" " ***"    UPON  CONS.
**
     ACCEPT  WK-TIME             FROM      TIME.
     DISPLAY "*** " PG-ID " END   (" WK-Y "." WK-M "." WK-D " "
              WK-HH "." WK-MN "." WK-SS ") ***"    UPON  CONS.
 END-END.
     EXIT.
*****************<<  SNJ0020L END PROGRAM  >>******************
