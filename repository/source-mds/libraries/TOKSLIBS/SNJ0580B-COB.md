# SNJ0580B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNJ0580B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＥＤＩ受信サーバ　　　　　　　　　*
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　回線管理マスタ初期化　　　        *
*    作成日／更新日　　　：　10/08/05                          *
*    作成者／更新者　　　：　ＮＡＶ大野                        *
*    処理概要　　　　　　：　パラメータで渡される回線種別、回線*
*                            制御番号でＥＤＩ回線管理マスタを読*
*                            込み、使用フラグの更新を行う。    *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNJ0580B.
 AUTHOR.               OONO.
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
*EDI回線管理マスタ
     SELECT   JSMKAIF  ASSIGN    TO        DA-01-VI-JSMKAIL2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       KAI-F03  KAI-F01
                                           KAI-F02
                       FILE  STATUS    IS  KAI-ST.
*
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*
******************************************************************
*    EDI回線管理マスタ
******************************************************************
 FD  JSMKAIF           LABEL     RECORD    IS   STANDARD.
                       COPY      JSMKAIL2  OF   XFDLIB
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
         NC"ＥＤＩ回線管理マスタエラー".
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
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*パラメタ取得
 01  LINK-KAISHU           PIC X(01).
 01  LINK-KSNO             PIC 9(01).
 01  LINK-TANNM            PIC X(08).
 01  LINK-KEKKA            PIC X(01).
*
**************************************************************
 PROCEDURE             DIVISION      USING    LINK-KAISHU
                                              LINK-KSNO
                                              LINK-TANNM
                                              LINK-KEKKA.
**************************************************************
 DECLARATIVES.
 KAI-ERR                   SECTION.
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
*    EDI回線管理マスタの初期化
     MOVE      SPACE         TO   KAI-REC.
     INITIALIZE                   KAI-REC.
*
*EDI回線管理マスタのキー項目を設定
     MOVE      LINK-TANNM    TO   KAI-F03.
     MOVE      LINK-KAISHU   TO   KAI-F01.
     MOVE      LINK-KSNO     TO   KAI-F02.
*EDI回線管理マスタの読込み
     READ      JSMKAIF
               INVALID
                  MOVE "9"        TO   LINK-KEKKA
                  GO    TO        MAIN-EXIT
     END-READ.
*
*EDI回線管理マスタの使用ＦＬＧ項目を設定
     MOVE      "0"           TO   KAI-F07.
*    EDI回線管理マスタ更新
     REWRITE   KAI-REC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*
*ファイルのＯＰＥＮ
     CLOSE     JSMKAIF.
*
 END-EXIT.
     EXIT.
*****************<<  SNJ0580B   END PROGRAM  >>******************

```
