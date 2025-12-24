****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｄ３６５連携管理　　　　　　　　　*
*    業務名　　　　　　　：　送受信制御　                      *
*    モジュール名　　　　：　回線管理Ｍチェック＆更新          *
*    作成日／更新日　　　：　2020/06/15                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　回線管理Ｍを読み、使用ＦＬＧが使用*
*                            済、使用未を判定し、更新する。    *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NVS0140B.
 AUTHOR.               T.TAKAHASHI
 DATE-WRITTEN.         2020/06/15.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*回線種別マスタ
     SELECT  JSMKAIF   ASSIGN    TO        DA-01-VI-JSMKAIL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       KAI-F01
                                           KAI-F02
                       FILE      STATUS    KAI-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 回線種別マスタ                                     *
****************************************************************
 FD  JSMKAIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMKAIF   OF   XFDLIB
                       JOINING   KAI       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KAI-ST                   PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  KAI-ERR           PIC N(15) VALUE
         NC"回線種別マスタエラー".
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
*
 01  LINK-KEKA             PIC X(01).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-KEKA.
**************************************************************
 DECLARATIVES.
 TJS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMKAIF.
     MOVE        KAI-ST    TO        E-ST.
     MOVE        "JSMKAIF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KAI-ERR   UPON      CONS.
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
     OPEN      I-O     JSMKAIF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*ファイルＫＥＹ項目セット
     MOVE      "D"           TO   KAI-F01.
     MOVE       1            TO   KAI-F02.
*ファイル読込み
     READ  JSMKAIF
           INVALID
           DISPLAY NC"＃＃回線管理Ｍ未存在！！＃＃" UPON CONS
           MOVE    4000      TO   PROGRAM-STATUS
           STOP  RUN
     END-READ.
*
     DISPLAY "KAI-F07 = " KAI-F07  UPON CONS.
     IF  KAI-F07  =  "1"
         MOVE    "1"         TO   LINK-KEKA
         DISPLAY NC"＃＃回線使用中！！＃＃" UPON CONS
     ELSE
         DISPLAY NC"＃＃回線未使用！！＃＃" UPON CONS
         MOVE    "0"         TO   LINK-KEKA
         MOVE    "1"         TO   KAI-F07
         REWRITE  KAI-REC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     JSMKAIF.
*
 END-EXIT.
     EXIT.
*****************<<  SCV0060B   END PROGRAM  >>******************
