****************************************************************
*                                                              *
*    ユーザ　　　　名：　サカタのたね　　　殿                  *
*    システム　　　名：　基幹システム改善                      *
*    プログラム　　名：　日次更新条件マスタ　結果更新　　      *
*    作成者　　　　　：　ＮＡＶ　　　　　                      *
*    作成日　　　　　：　2005.12.19      UPDATE: YYYY.MM.DD    *
*                                                              *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SJH0150B.
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
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SJH0150B".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
*=============================================================
 LINKAGE             SECTION.
*=============================================================
   01  LINK-IN-DATE          PIC  9(08).
   01  LINK-IN-JKKEKA        PIC  X(01).
******************************************************************
 PROCEDURE           DIVISION  USING   LINK-IN-DATE
                                       LINK-IN-JKKEKA.
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
     DISPLAY  "**  SJH0150B   START  **"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SJH0150B    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN     I-O        JHMNITF.
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
             DISPLAY NC"対象日付が存在しません" UPON CONS
             STOP RUN
        NOT INVALID
             MOVE     LINK-IN-JKKEKA   TO  NIT-F08
             REWRITE  NIT-REC
     END-READ.
 MAIN-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      JHMNITF.
     DISPLAY " LINK-IN-DATE   = " LINK-IN-DATE UPON CONS.
     DISPLAY " LINK-IN-JKKEKA = " LINK-IN-JKKEKA UPON CONS.
 END-EXIT.
     EXIT.
