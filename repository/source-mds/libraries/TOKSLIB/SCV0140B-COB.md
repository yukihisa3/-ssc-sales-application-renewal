# SCV0140B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SCV0140B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　受信結果セット                    *
*    作成日／更新日　　　：　99/10/07                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　受信パラメタＦへ受信時間をセットす*
*                            る。                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SCV0140B.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         99/10/07.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受信パラマタファイル
     SELECT  PARAFILE  ASSIGN    TO        DA-01-S-PARAFILE
                       FILE      STATUS    PAR-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 受信パラメタファイル                               *
****************************************************************
 FD  PARAFILE          BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      PARAINS1  OF   XFDLIB
                       JOINING   PAR       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  PAR-ST                   PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  PAR-ERR           PIC N(15) VALUE
         NC"パラメタファイルエラー".
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
 01  LINK-JTIMES           PIC 9(04).
 01  LINK-JTIMEE           PIC 9(04).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-JTIMES
                                           LINK-JTIMEE.
**************************************************************
 DECLARATIVES.
 PAR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PARAFILE.
     MOVE        PAR-ST    TO        E-ST.
     MOVE        "PARAFILE" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     PAR-ERR   UPON      CONS.
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
     OPEN      I-O    PARAFILE.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*パラメタファイル読込み
     READ  PARAFILE  AT  END
           DISPLAY "## ﾊﾟﾗﾒﾀﾌｧｲﾙ READ ｲｼﾞｮｳ##" UPON CONS
           MOVE    "4000"    TO   PROGRAM-STATUS
           GO                TO   MAIN-EXIT
     END-READ.
*受取ったパラメタをファイル項目へセット
     MOVE      LINK-JTIMES   TO   PAR-F12.
     MOVE      LINK-JTIMEE   TO   PAR-F13.
*パラメタファイル更新
     REWRITE   PAR-REC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     PARAFILE.
*
 END-EXIT.
     EXIT.
*****************<<  SCV0130B   END PROGRAM  >>******************

```
