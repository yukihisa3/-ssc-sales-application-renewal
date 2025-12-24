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
*
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC   X(01).
     03  LINK-IN-YMD6        PIC   9(06).
     03  LINK-IN-YMD8        PIC   9(08).
     03  LINK-OUT-RET        PIC   X(01).
     03  LINK-OUT-YMD8       PIC   9(08).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE            PIC 9(06).
     03  SYS-DATEW           PIC 9(08).
     03  WK-ZJIKAN           PIC 9(02).
*=============================================================
 LINKAGE             SECTION.
*=============================================================
 01  LINK-OUT-DATE         PIC  9(08).
 01  LINK-OUT-JKKBN        PIC  X(01).
 01  LINK-OUT-KDTIME       PIC  9(04).
 01  LINK-OUT-HACDAY       PIC  9(08).
 01  LINK-OUT-NOUDAY       PIC  9(08).
 01  LINK-OUT-SYUDAY       PIC  9(08).
 01  LINK-OUT-JKKEKA       PIC  X(01).
 01  LINK-OUT-ZJIKAN       PIC  9(02).
 01  LINK-OUT-GETUJI       PIC  9(01).
******************************************************************
 PROCEDURE           DIVISION  USING   LINK-OUT-DATE
                                       LINK-OUT-JKKBN
                                       LINK-OUT-KDTIME
                                       LINK-OUT-HACDAY
                                       LINK-OUT-NOUDAY
                                       LINK-OUT-SYUDAY
                                       LINK-OUT-JKKEKA
                                       LINK-OUT-ZJIKAN
                                       LINK-OUT-GETUJI.
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
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
 INIT-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
*    ファイル読込
     MOVE    SYS-DATEW          TO   NIT-F01 LINK-OUT-DATE.
     READ    JHMNITF
        INVALID KEY
             DISPLAY NC"対象データが存在しません" UPON CONS
             STOP RUN
        NOT INVALID
*************MOVE     NIT-F01    TO        LINK-OUT-DATE
             MOVE     NIT-F02    TO        LINK-OUT-JKKBN
             MOVE     NIT-F03(1:2) TO      WK-ZJIKAN
             COMPUTE  LINK-OUT-ZJIKAN = WK-ZJIKAN - 1
             MOVE     NIT-F03    TO        LINK-OUT-KDTIME
             MOVE     NIT-F05    TO        LINK-OUT-HACDAY
             MOVE     NIT-F06    TO        LINK-OUT-NOUDAY
             MOVE     NIT-F07    TO        LINK-OUT-SYUDAY
             MOVE     NIT-F08    TO        LINK-OUT-JKKEKA
             MOVE     NIT-F10    TO        LINK-OUT-GETUJI
     END-READ.
 MAIN-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      JHMNITF.
     DISPLAY " LINK-OUT-DATE   = " LINK-OUT-DATE   UPON CONS.
     DISPLAY " LINK-OUT-JKKBN  = " LINK-OUT-JKKBN  UPON CONS.
     DISPLAY " LINK-OUT-KDTIME = " LINK-OUT-KDTIME UPON CONS.
     DISPLAY " LINK-OUT-HACDAY = " LINK-OUT-HACDAY UPON CONS.
     DISPLAY " LINK-OUT-NOUDAY = " LINK-OUT-NOUDAY UPON CONS.
     DISPLAY " LINK-OUT-SYUDAY = " LINK-OUT-SYUDAY UPON CONS.
     DISPLAY " LINK-OUT-JKKEKA = " LINK-OUT-JKKEKA UPON CONS.
     DISPLAY " LINK-OUT-ZJIKAN = " LINK-OUT-ZJIKAN UPON CONS.
     DISPLAY " LINK-OUT-GETUJI = " LINK-OUT-GETUJI UPON CONS.
 END-EXIT.
     EXIT.
