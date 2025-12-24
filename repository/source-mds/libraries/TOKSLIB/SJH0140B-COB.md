# SJH0140B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJH0140B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    ユーザ　　　　名：　サカタのたね　　　殿                  *
*    システム　　　名：　基幹システム改善                      *
*    プログラム　　名：　日次更新条件マスタ　条件Ｆ出力　      *
*    作成者　　　　　：　ＮＡＶ　　　　　                      *
*    作成日　　　　　：　2005.12.19      UPDATE: YYYY.MM.DD    *
*                                                              *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SJH0140B.
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
*条件ファイル　　　　
     SELECT   HJYOKEN        ASSIGN        TO  01-VI-JYOKEN1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  JYO-F01 JYO-F02
                             FILE STATUS   IS  JYO-STA.

*
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*条件ファイル　　　　
 FD  HJYOKEN.
     COPY     HJYOKEN  OF  XFDLIB
     JOINING  JYO      AS  PREFIX.
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*ステータス
 01  STA-AREA.
     03  JYO-STA             PIC  X(02).
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SJH0140B".
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
   01  LINK-IN-HACDAY        PIC  9(08).
   01  LINK-IN-NOUDAY        PIC  9(08).
   01  LINK-IN-SYUDAY        PIC  9(08).
******************************************************************
 PROCEDURE           DIVISION  USING   LINK-IN-HACDAY
                                       LINK-IN-NOUDAY
                                       LINK-IN-SYUDAY.
******************************************************************
 DECLARATIVES.
*条件ファイル　　　
 JYO-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       HJYOKEN.
     MOVE    "JYOKEN1 "    TO    ERR-FL-ID.
     MOVE     JYO-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  SJH0140B   START  **"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SJH0140B    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN     I-O         HJYOKEN.
 INIT-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
*    ファイル読込
     MOVE    99                  TO        JYO-F01.
     MOVE    "DAY"               TO        JYO-F02.
     READ    HJYOKEN
        INVALID KEY
             DISPLAY NC"条件ファイルエラー" UPON CONS
             STOP RUN
        NOT INVALID
             MOVE     LINK-IN-HACDAY  TO   JYO-F05
             MOVE     LINK-IN-NOUDAY  TO   JYO-F06
             MOVE     LINK-IN-SYUDAY  TO   JYO-F07
             REWRITE  JYO-REC
     END-READ.
 MAIN-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      HJYOKEN.
 END-EXIT.
     EXIT.

```
