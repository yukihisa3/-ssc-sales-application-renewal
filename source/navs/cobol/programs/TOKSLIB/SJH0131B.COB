****************************************************************
*                                                              *
*    ユーザ　　　　名：　サカタのたね　　　殿                  *
*    システム　　　名：　基幹システム改善                      *
*    プログラム　　名：　日次更新条件マスタ　パラメタ受渡      *
*    作成者　　　　　：　ＮＡＶ　　　　　                      *
*    作成日　　　　　：　2005.12.19      UPDATE: YYYY.MM.DD    *
*                                                              *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SJH0130B.
 AUTHOR.             NAV.
 DATE-WRITTEN.       05.12.19.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS     CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*日次更新条件マスタ
     SELECT   JHMNITF        ASSIGN        TO  01-VI-JHMNITL1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  NIT-F01
                             FILE STATUS   IS  NIT-STA.

*
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*企画調達取引先ﾏｽﾀ
 FD  JHMNITF.
     COPY     JHMNITF  OF  XFDLIB
     JOINING  NIT      AS  PREFIX.
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*ステータス
 01  STA-AREA.
     03  NIT-STA             PIC  X(02).
*カウント
 01  CNT-AREA.
     03  IN-CNT                   PIC  9(07)  VALUE  ZERO.
     03  OT-CNT                   PIC  9(07)  VALUE  ZERO.
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
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
*
 01  WK-SDATE                PIC  9(08).
 01  WK-SDATE-R   REDEFINES   WK-SDATE.
     03  FILLER              PIC  X(02).
     03  WK-SYMD.
       05  WK-SYY            PIC  9(02).
       05  WK-SMM            PIC  9(02).
       05  WK-SDD            PIC  9(02).
 01  WK-EDATE                PIC  9(08).
 01  WK-EDATE-R   REDEFINES   WK-EDATE.
     03  FILLER              PIC  X(02).
     03  WK-EYMD.
       05  WK-EYY            PIC  9(02).
       05  WK-EMM            PIC  9(02).
       05  WK-EDD            PIC  9(02).
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SJH0130B".
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
   01  LINK-IN-DATE          PIC  9(08).
   01  LINK-OUT-DATE         PIC  9(08).
   01  LINK-OUT-JKKBN        PIC  X(01).
   01  LINK-OUT-KDTIME       PIC  9(04).
   01  LINK-OUT-HACDAY       PIC  9(08).
   01  LINK-OUT-NOUDAY       PIC  9(08).
   01  LINK-OUT-SYUDAY       PIC  9(08).
   01  LINK-OUT-JKKEKA       PIC  X(01).
******************************************************************
 PROCEDURE           DIVISION  USING   LINK-IN-DATE
                                       LINK-OUT-DATE
                                       LINK-OUT-JKKBN
                                       LINK-OUT-KDTIME
                                       LINK-OUT-HACDAY
                                       LINK-OUT-NOUDAY
                                       LINK-OUT-SYUDAY
                                       LINK-OUT-JKKEKA.
******************************************************************
 DECLARATIVES.
*日次更新条件マスタ
 NIT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       JHMNITF.
     MOVE    "JHMNITL1"    TO    ERR-FL-ID.
     MOVE     NIT-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  SJH0130B   START  **"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SJH0130B    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN     INPUT       JHMNITF.
 INIT-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
*    ファイル読込
     MOVE    LINK-IN-DATE        TO        NIT-F01.
     READ    JHMNITF
        INVALID KEY
             DISPLAY NC"対象データが存在しません" UPON CONS
             STOP RUN
        NOT INVALID
             MOVE     NIT-F01    TO        LINK-OUT-DATE
             MOVE     NIT-F02    TO        LINK-OUT-JKKBN
             MOVE     NIT-F03    TO        LINK-OUT-KDTIME
             MOVE     NIT-F05    TO        LINK-OUT-HACDAY
             MOVE     NIT-F06    TO        LINK-OUT-NOUDAY
             MOVE     NIT-F07    TO        LINK-OUT-SYUDAY
             MOVE     NIT-F08    TO        LINK-OUT-JKKEKA
     END-READ.
 MAIN-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      JHMNITF.
     DISPLAY " LINK-IN-DATE = " LINK-IN-DATE UPON CONS.
     DISPLAY " LINK-OUT-JKKBN = " LINK-OUT-JKKBN UPON CONS.
     DISPLAY " LINK-OUT-KDTIME = " LINK-OUT-KDTIME UPON CONS.
     DISPLAY " LINK-OUT-HACDAY = " LINK-OUT-HACDAY UPON CONS.
     DISPLAY " LINK-OUT-NOUDAY = " LINK-OUT-NOUDAY UPON CONS.
     DISPLAY " LINK-OUT-SYUDAY = " LINK-OUT-SYUDAY UPON CONS.
     DISPLAY " LINK-OUT-JKKEKA = " LINK-OUT-JKKEKA UPON CONS.
 END-EXIT.
     EXIT.
