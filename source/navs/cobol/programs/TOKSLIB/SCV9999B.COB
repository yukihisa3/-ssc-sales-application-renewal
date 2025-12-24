****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理サブシステム            *
*    業務名　　　　　　　：　伝票更新業務                      *
*    モジュール名　　　　：　在庫引当／売上更新処理            *
*    作成日／更新日　　　：　1999/09/28                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　ＶＬＤＦに終了情報を投入する。   *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SCV9999B.
 AUTHOR.                NAV T.TAKAHASHI.
 DATE-WRITTEN.          99/09/28.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ＶＬＤ５００
     SELECT   VLD500    ASSIGN    TO        VLD500
                        FILE  STATUS   IS   VLD-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    ＶＬＤ５００
******************************************************************
 FD  VLD500.
 01  VLD-REC.
     03  VLD-F01           PIC  X(02).
     03  VLD-F02           PIC  9(03).
     03  VLD-F03           PIC  X(02).
     03  VLD-F04           PIC  X(08).
     03  VLD-F05           PIC  9(06).
     03  VLD-F06           PIC  9(01).
     03  VLD-F07           PIC  X(02).
     03  VLD-F08           PIC  9(02).
     03  VLD-F09           PIC  9(02).
     03  VLD-F10           PIC  9(04).
     03  VLD-F11           PIC  9(08).
     03  VLD-F12           PIC  9(04).
     03  VLD-F13           PIC  9(08).
     03  FILLER            PIC  X(48).
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*終了フラグ
 01  END-FG                       PIC  9(01)  VALUE  ZERO.
*ステータスワーク
 01  STATUS-AREA.
     03  VLD-STATUS               PIC  X(02)  VALUE  SPACE.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  VLD-ERR           PIC N(15) VALUE
         NC"ＶＬＤ５００Ｆエラー".
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
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
*
 VLD-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE VLD500.
     MOVE      VLD-STATUS  TO        E-ST.
     MOVE        "VLD500"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     VLD-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
*ＶＬＤＦ更新終了メッセージ
     DISPLAY NC"＃＃伝票更新　終了　更新　開始＃＃" UPON CONS.
*ＶＬＤＦのＯＰＥＮ
     OPEN     OUTPUT    VLD500.
*ＶＬＤＦ終了情報投入
     MOVE      SPACE              TO    VLD-REC.
     INITIALIZE                         VLD-REC.
     MOVE      500                TO    VLD-F02.
     MOVE      "NW"               TO    VLD-F03.
     MOVE      52                 TO    VLD-F10.
     MOVE      99999999           TO    VLD-F11.
     MOVE      9999               TO    VLD-F12.
     MOVE      99999999           TO    VLD-F13.
     WRITE     VLD-REC.
*ＶＬＤＦのＣＬＯＳＥ
     CLOSE     VLD500.
*プログラム停止
     STOP  RUN.
*
 PROCESS-END.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
