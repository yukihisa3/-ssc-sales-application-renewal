****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　伝票更新Ｆ区分初期化              *
*    作成日／更新日　　　：　2001/02/26                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SCV0230B.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         01/02/26.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*実行制御マスタ
     SELECT  JHMJIKF   ASSIGN    TO        DA-01-VI-JHMJIKL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       JIK-F01
                       FILE      STATUS    JIK-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 実行制御マスタ                                     *
****************************************************************
 FD  JHMJIKF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMJIKF   OF   XFDLIB
                       JOINING   JIK       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  JIK-ST                   PIC  X(02).
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  JIK-ERR           PIC N(15) VALUE
                        NC"実行制御マスタエラー　".
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
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 JIK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMJIKF.
     MOVE        JIK-ST    TO        E-ST.
     MOVE        "JHMJIKF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JIK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
***  ファイルのオープン
     OPEN      I-O      JHMJIKF.
***  実行制御マスタチェック
     MOVE     "D"                 TO   JIK-F01.
     READ      JHMJIKF
               INVALID
               DISPLAY "ﾃﾞﾝｺｳｼﾝFLG INV KEY = " JIK-F01 UPON CONS
               GO                 TO   PROCESS010
               NOT  INVALID
               MOVE    "0"        TO   JIK-F03
     END-READ.
***  ファイルの更新
     REWRITE   JIK-REC.
 PROCESS010.
***  ファイルのクローズ
     CLOSE     JHMJIKF.
***  プログラム終了
     STOP  RUN.
 PROCESS-EXD.
     EXIT.
*****************<<  SCV0230B   END PROGRAM  >>******************
