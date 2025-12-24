# SNJ0530B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNJ0530B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＥＤＩ受信サーバ　　　　　　　　　*
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　受信パラメタファイル作成          *
*    作成日／更新日　　　：　10/08/03                          *
*    作成者／更新者　　　：　ＮＡＶ大野                        *
*    処理概要　　　　　　：　受取ったパラメタ及びＥＤＩ管理マス*
*                            タ・ＥＤＩ回線管理マスタより、パラ*
*                            メタＦを作成する。                *
*                            10/08/26                          *
*                            パラメータでトリガーＦ名・アンサー*
*                            Ｆ名・ＪＯＢ名・受配信Ｆを出力する*
*                            処理を追加。                      *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNJ0530B.
 AUTHOR.               OONO.
 DATE-WRITTEN.         10/08/26.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受信パラメタファイル
     SELECT   PARAFIL   ASSIGN    TO       DA-01-S-PARAFIL
                       FILE   STATUS   IS  PAR-ST.
*ＥＤＩ受信管理マスタ
     SELECT   JSMEDIL1 ASSIGN    TO        DA-01-VI-JSMEDIL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       EDI-F01  EDI-F02
                                           EDI-F03
                       FILE  STATUS    IS  EDI-ST.
*
*ＥＤＩ回線管理マスタ
     SELECT   JSMKAIL1 ASSIGN    TO        DA-01-VI-JSMKAIL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       KAI-F01
                                           KAI-F02
                       FILE  STATUS    IS  KAI-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 受信パラメタファイル                               *
****************************************************************
 FD  PARAFIL           BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      PARAFIL   OF   XFDLIB
                       JOINING   PAR       AS   PREFIX.
******************************************************************
*    ＥＤＩ受信管理マスタ
******************************************************************
 FD  JSMEDIL1          LABEL     RECORD    IS   STANDARD.
                       COPY      JSMEDIL1  OF   XFDLIB
                       JOINING   EDI       AS   PREFIX.
*
******************************************************************
*    ＥＤI回線管理マスタ
******************************************************************
 FD  JSMKAIL1          LABEL     RECORD    IS   STANDARD.
                       COPY      JSMKAIL1  OF   XFDLIB
                       JOINING   KAI       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  PAR-ST                   PIC  X(02).
     03  EDI-ST                   PIC  X(02).
     03  KAI-ST                   PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  PAR-ERR           PIC N(15) VALUE
         NC"パラメタファイルエラー".
     03  EDI-ERR           PIC N(15) VALUE
         NC"ＥＤＩ受信管理マスタエラー".
     03  KAI-ERR           PIC N(15) VALUE
         NC"ＥＤＩ回線管理マスタエラー".
*読込フラグ領域
 01  FLG-AREA.
     03  EDI-FLG           PIC  X(01) VALUE SPACE.
     03  KAI-FLG           PIC  X(01) VALUE SPACE.
*読込・書込カウント領域
 01  CNT-AREA.
     03  EDI-CNT           PIC  9(02) VALUE ZERO.
     03  KAI-CNT           PIC  9(02) VALUE ZERO.
     03  WRITE-CNT         PIC  9(02) VALUE ZERO.
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
 01  LINK-JDATE            PIC 9(08).
 01  LINK-JTIME            PIC 9(04).
 01  LINK-DTKBN            PIC X(02).
 01  LINK-JHKBN            PIC X(01).
 01  LINK-TORICD           PIC 9(08).
 01  LINK-KAISHU           PIC X(01).
 01  LINK-KAISNO           PIC 9(01).
 01  LINK-KEKKA            PIC X(01).
 01  LINK-TRGERF           PIC X(08).
 01  LINK-ANSERF           PIC X(08).
 01  LINK-JOBNM            PIC X(08).
 01  LINK-HAISINF          PIC X(08).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-JDATE
                                           LINK-JTIME
                                           LINK-DTKBN
                                           LINK-JHKBN
                                           LINK-TORICD
                                           LINK-KAISHU
                                           LINK-KAISNO
                                           LINK-KEKKA
                                           LINK-TRGERF
                                           LINK-ANSERF
                                           LINK-JOBNM
                                           LINK-HAISINF.
**************************************************************
 DECLARATIVES.
 PAR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PARAFIL.
     MOVE        PAR-ST    TO        E-ST.
     MOVE        "PARAFIL" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     PAR-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 EDI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMEDIL1.
     MOVE        EDI-ST    TO        E-ST.
     MOVE        "JSMEDIL1" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     EDI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KAI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMKAIL1.
     MOVE        KAI-ST    TO        E-ST.
     MOVE        "JSMKAIL1" TO       E-FILE.
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
     OPEN      INPUT   JSMEDIL1   JSMKAIL1.
     OPEN      OUTPUT  PARAFIL.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*    パラメタファイルレコードの初期化
     MOVE      SPACE         TO   PAR-REC.
     INITIALIZE                   PAR-REC.
*
*ＥＤＩ受信管理マスタのキー項目を設定
     MOVE      LINK-DTKBN    TO   EDI-F01.
     MOVE      LINK-JHKBN    TO   EDI-F02.
     MOVE      LINK-TORICD   TO   EDI-F03.
*ＥＤＩ受信管理マスタの読込
     READ      JSMEDIL1
               INVALID
                  MOVE "9"        TO   EDI-FLG
               NOT INVALID
                  ADD   1         TO   EDI-CNT
     END-READ.
*ＥＤＩ回線管理マスタのキー項目を設定
     MOVE      LINK-KAISHU   TO   KAI-F01.
     MOVE      LINK-KAISNO   TO   KAI-F02.
     DISPLAY " LINK-KAISHU = " LINK-KAISHU  UPON CONS.
     DISPLAY " LINK-KAISNO = " LINK-KAISNO  UPON CONS.
*ＥＤＩ回線管理マスタの読込
     READ      JSMKAIL1
               INVALID
                  MOVE "9"        TO   KAI-FLG
               NOT INVALID
                  ADD   1         TO   KAI-CNT
     END-READ.
*
*マスタ読込みが行えたか判定
     IF  EDI-FLG = "9"  OR  KAI-FLG = "9"
         MOVE  "9"      TO  LINK-KEKKA
     ELSE
*        受取ったパラメタをファイル項目へセット
         MOVE      LINK-JDATE    TO   PAR-F01
         MOVE      LINK-JTIME    TO   PAR-F02
         MOVE      LINK-TORICD   TO   PAR-F03
         MOVE      LINK-DTKBN    TO   PAR-F04
*
*        パラメタファイルへ項目セット
         MOVE  EDI-F00 TO   PAR-E00
         MOVE  KAI-F00 TO   PAR-K00
*        パラメタファイル更新
         WRITE     PAR-REC
*
*2010/08/26 ADD BGN
*        出力するパラメタの値をセット
         MOVE      KAI-F04       TO   LINK-TRGERF
         MOVE      KAI-F05       TO   LINK-ANSERF
         MOVE      EDI-F15       TO   LINK-JOBNM
         MOVE      EDI-F12       TO   LINK-HAISINF
*2010/08/26 ADD END
         MOVE      " "           TO   LINK-KEKKA
*
*        読込カウンタに１足す
         ADD   1       TO   WRITE-CNT
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*
     DISPLAY     "EDT-CNT = "  EDI-CNT   UPON      CONS.
     DISPLAY     "KAI-CNT = "  KAI-CNT   UPON      CONS.
     DISPLAY     "WRITE-CNT = "  WRITE-CNT  UPON      CONS.
*ファイルのＯＰＥＮ
     CLOSE     JSMEDIL1 JSMKAIL1 PARAFIL.
*
 END-EXIT.
     EXIT.
*****************<<  SNJ0530B   END PROGRAM  >>******************

```
