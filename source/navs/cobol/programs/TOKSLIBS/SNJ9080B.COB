****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　回線フラグクリア                  *
*    作成日／更新日　　　：　10/09/03                          *
*    作成者／更新者　　　：　ＮＡＶ阿部                        *
*    処理概要　　　　　　：　回線種別マスタの使用中回線のフラグ*
*                            をクリアする。                    *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNJ9080B.
 AUTHOR.               ABE.
 DATE-WRITTEN.         10/09/03.
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
 01  LINK-LINE             PIC X(01).
 01  LINK-YUSEN            PIC 9(01).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-LINE
                                           LINK-YUSEN.
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
*    DISPLAY "LINK-LINE  = "  LINK-LINE   UPON CONS.
*    DISPLAY "LINK-YUSEN = "  LINK-YUSEN  UPON CONS.
*ファイルＫＥＹ項目セット
     MOVE      LINK-LINE     TO   KAI-F01.
     MOVE      LINK-YUSEN    TO   KAI-F02.
*ファイル読込み
     READ      JSMKAIF
               INVALID
                 DISPLAY "## ｶｲｾﾝ ｼｮｷｶ ERR ##"           UPON CONS
                 DISPLAY "## ｶｲｾﾝ = " KAI-F01 "      ##" UPON CONS
                 DISPLAY "## ﾕｳｾﾝ = " KAI-F02 "      ##" UPON CONS
                 STOP  RUN
               NOT  INVALID
                 MOVE  SPACE   TO   KAI-F07
                 MOVE  SPACE   TO   KAI-F08
                 REWRITE KAI-REC
     END-READ.
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