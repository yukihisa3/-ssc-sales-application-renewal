# SNJ9090B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNJ9090B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＥＤＩ受信サーバ　　　　　　　　　*
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　アンサーファイルステータス更新    *
*    作成日／更新日　　　：　10/09/03                          *
*    作成者／更新者　　　：　ＮＡＶ阿部                        *
*    処理概要　　　　　　：　アンサーファイルを読込み、終了ステ*
*                            ータスに”ＺＺＺＺ”を更新する。　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNJ9090B.
 AUTHOR.               ABE.
 DATE-WRITTEN.         XX/XX/XX.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*アンサーファイル
     SELECT  ANSER     ASSIGN    TO        DA-01-S-ANSER
                       FILE      STATUS    ANS-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = アンサーファイル                                   *
****************************************************************
 FD  ANSER             BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ANSER     OF   XFDLIB
                       JOINING   ANS       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  ANS-ST                   PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  ANS-ERR           PIC N(15) VALUE
         NC"アンサーファイルエラー".
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*パラメタ取得
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 ANS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ANSER.
     MOVE        ANS-ST    TO        E-ST.
     MOVE        "ANSER"   TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     ANS-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC.
     PERFORM   END-SEC.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN     OUTPUT  ANSER.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*パラメタファイル読込み
 MAIN010.
*    READ  ANSER
*       AT  END
*          MOVE    SPACE     TO   ANS-REC
*          MOVE    ","       TO   ANS-C01
*          MOVE    ","       TO   ANS-C02
*          MOVE    "ZZZZ"    TO   ANS-F02
*          WRITE   ANS-REC
*       NOT  AT END
*          MOVE    "ZZZZ"    TO   ANS-F02
*          REWRITE ANS-REC
*    END-READ.
*
     MOVE    SPACE     TO   ANS-REC.
     MOVE    ","       TO   ANS-C01.
     MOVE    ","       TO   ANS-C02.
     MOVE    "ZZZZ"    TO   ANS-F02.
     WRITE   ANS-REC.
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     ANSER.
*
 END-EXIT.
     EXIT.
*****************<<  SNJ9090B   END PROGRAM  >>******************

```
