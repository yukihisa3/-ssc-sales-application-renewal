****************************************************************
*                                                              *
*    ユーザ　　　　名：　サカタのタネ　　　殿                  *
*    システム　　　名：　日次自動処理　　                      *
*    プログラム　　名：　日次更新条件マスタ一括作成　　　　    *
*    作成者　　　　　：　ＮＡＶ　　　　　                      *
*    作成日　　　　　：　2008.05.29      UPDATE: YYYY.MM.DD    *
*                                                              *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         HIDUKEPG.
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
*日付曜日ファイル
     SELECT   HIDUKEF        ASSIGN        TO  01-VI-HIDUKEL1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  HIZ-F01
                             FILE STATUS   IS  HIZ-STA.

*
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*日付曜日ファイル
 FD  HIDUKEF.
     COPY     HIDUKEF  OF  XFDLIB
     JOINING  HIZ      AS  PREFIX.
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*ステータス
 01  STA-AREA.
     03  HIZ-STA             PIC  X(02).
*カウント
 01  CNT-AREA.
     03  IN-CNT                   PIC  9(07)  VALUE  ZERO.
     03  OT-CNT                   PIC  9(07)  VALUE  ZERO.
*ＦＬＧ
 01  WK-FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
*推移日付
 01  WK-DATA-AREA.
     03  WK-UP-HIDUKE             PIC  9(08)  VALUE  ZERO.
     03  WK-UP-YOUBI              PIC  9(01)  VALUE  ZERO.
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
         05  FILLER          PIC  X(12)  VALUE  "### HIDUKEPG".
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
   01  LINK-HIDUKE           PIC  9(08).
   01  LINK-YOUBI            PIC  9(01).
******************************************************************
 PROCEDURE           DIVISION  USING   LINK-HIDUKE
                                       LINK-YOUBI.
******************************************************************
 DECLARATIVES.
*日次更新条件マスタ
 HIZ-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       HIDUKEF.
     MOVE    "HIDUKEL1"    TO    ERR-FL-ID.
     MOVE     HIZ-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  HIDUKEPG   START  **"   UPON  CONS.
*
     PERFORM  IHIZ-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
     DISPLAY  "**  HIDUKEPG    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 IHIZ-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN     I-O         HIDUKEF.
*
     MOVE     LINK-HIDUKE TO    WK-UP-HIDUKE.
     MOVE     LINK-YOUBI  TO    WK-UP-YOUBI.
*
 IHIZ-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
*＋１日取得
     MOVE       "5"           TO     LINK-IN-KBN.
     MOVE        1            TO     LINK-IN-YMD6.
     MOVE        WK-UP-HIDUKE TO     LINK-IN-YMD8.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD   TO   WK-UP-HIDUKE
     ELSE
         DISPLAY "## PROGRAM ABEND ##"  UPON CONS
         STOP  RUN
     END-IF.
*
     ADD     1                  TO   WK-UP-YOUBI.
*
     IF      WK-UP-YOUBI  >  7
             MOVE     1         TO   WK-UP-YOUBI
     END-IF.
*
     MOVE    SPACE              TO   HIZ-REC.
     INITIALIZE                      HIZ-REC.
*
     MOVE    WK-UP-HIDUKE       TO   HIZ-F01.
     MOVE    WK-UP-YOUBI        TO   HIZ-F02.
     EVALUATE HIZ-F02
         WHEN 1  MOVE NC"月"    TO   HIZ-F03
         WHEN 2  MOVE NC"火"    TO   HIZ-F03
         WHEN 3  MOVE NC"水"    TO   HIZ-F03
         WHEN 4  MOVE NC"木"    TO   HIZ-F03
         WHEN 5  MOVE NC"金"    TO   HIZ-F03
         WHEN 6  MOVE NC"土"    TO   HIZ-F03
         WHEN 7  MOVE NC"日"    TO   HIZ-F03
     END-EVALUATE.
*
     WRITE   HIZ-REC.
     ADD     1                  TO   OT-CNT.
*
     IF      WK-UP-HIDUKE  >  20301231
             MOVE  "END"        TO   END-FLG
     END-IF.
*
 MAIN-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      HIDUKEF.
     DISPLAY NC"作成件数" " = " OT-CNT UPON CONS.
 END-EXIT.
     EXIT.
