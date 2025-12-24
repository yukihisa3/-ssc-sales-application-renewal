*************************************************************
* 顧客名　　　　: 株式会社サカタのタネ
* 業務名　　　　: 受配信管理サブシステム
* サブシステム名:
* モジュール名　: 回線環境マスタリスト発行
* 作成日／作成者: 99/08/25
* 処理概要　　　: 回線種別マスタより回線環境マスタリストを
*                出力する.
* 使用ＭＥＤ　　:
*-----------------------------------------------------------*
* 修正日／修正者: 99年8月30日／岡部
* 修正内容      :
*************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJH0009L.
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
*回線種別マスタ
     SELECT  JHMKAIF   ASSIGN    TO   JHMKAIF
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
*環境種別マスタ
 FD  JHMKAIF
                       LABEL     RECORD    IS   STANDARD
                       BLOCK     CONTAINS  81   RECORDS.
                       COPY      JHMKAIF   OF   XFDLIB
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
 01  PG-ID                       PIC  X(08)   VALUE  "SJH0009L".
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
                                 "SJH0009L".
     03  FILLER                  PIC  X(33)  VALUE  SPACE.
     03  FILLER                  PIC  N(16)  VALUE
         NC"※※　回線環境マスタリスト　※※"
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
     03  FILLER                  PIC  X(26)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"回線種別"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"優先順位"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"回線使用"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(06)  VALUE
                                 "ﾗｲﾌﾞﾗﾘ".
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"名"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  FILLER                  PIC  N(02)  VALUE
                                 NC"集信"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(04)  VALUE
                                 "ﾌｧｲﾙ".
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"名"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  FILLER                  PIC  N(02)  VALUE
                                 NC"管理"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(04)  VALUE
                                 "ﾌｧｲﾙ".
     03  FILLER                  PIC  N(02)  VALUE
                                 NC"創成"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE
                                 "ID".
     03  FILLER                  PIC  X(38)  VALUE  SPACE.
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
     03  FILLER                  PIC  X(26)  VALUE  SPACE.
     03  DT1-01                  PIC  X(01).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-02                  PIC  N(04).
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  DT1-03                  PIC  9(01).
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  DT1-04                  PIC  X(08).
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  DT1-05                  PIC  X(08).
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  DT1-06                  PIC  X(08).
     03  FILLER                  PIC  X(40)  VALUE  SPACE.
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
     USE     AFTER     EXCEPTION      PROCEDURE     JHMKAIF.
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
     OPEN    INPUT     JHMKAIF.
     OPEN    OUTPUT    PRTF.
*ワークの初期化
     INITIALIZE   FLG-AREA.
*回線種別マスタ
     PERFORM      JHMKAIF-READ-SEC.
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
     PERFORM      JHMKAIF-READ-SEC.
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
     IF    DT1-01 = "I"
           MOVE    NC"ＩＳＤＮ"   TO     DT1-02
     ELSE
           MOVE    NC"公衆回線"   TO     DT1-02
     END-IF.
*優先順位
     MOVE  KAI-F02                TO     DT1-03.
*使用回線ﾗｲﾌﾞﾗﾘ名
     MOVE  KAI-F03                TO     DT1-04.
*集信ﾌｧｲﾙ名
     MOVE  KAI-F04                TO     DT1-05.
*管理ﾌｧｲﾙ創成ID
     MOVE  KAI-F06                TO     DT1-06.
*明細行印字
     WRITE   PRT-REC   FROM    DT1    AFTER   1.
*明細行カウント
     ADD     1                   TO        L-CNT.
*
 BODY-SUB-END.
     EXIT.
*=============================================================
*       2.3       回線種別マスタ　ＲＥＡＤ
*=============================================================
 JHMKAIF-READ-SEC        SECTION.
*
     READ    JHMKAIF
             AT  END
             MOVE    "END"       TO   END-FLG
     END-READ.
*
 JHMKAIF-READ-EXIT.
     EXIT.
*=============================================================
*       3.0       終了処理
*=============================================================
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             JHMKAIF
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
*****************<<  SJH0009L END PROGRAM  >>******************
