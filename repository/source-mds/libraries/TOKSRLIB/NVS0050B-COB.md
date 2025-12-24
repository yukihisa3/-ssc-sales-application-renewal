# NVS0050B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVS0050B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　ＡＮＳＥＲファイル待受＆判定　    *
*    作成日／更新日　　　：　19/12/25                          *
*    作成者／更新者　　　：　ＮＡＶ                            *
*    処理概要　　　　　　：　ＡＮＳＥＲファイルを読み、パラメー*
*                            タの終了ステータス、処理結果に出力*
*                            する。また、送受信履歴ファイルに  *
*                            履歴情報を更新する。              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NVS0050B.
 AUTHOR.               HASHIMOTO.
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
     SELECT   SNDANSR  ASSIGN    TO       DA-01-S-SNDANSER
                       FILE   STATUS   IS  SND-ST.
*ＮＡＶＳ履歴管理ファイル
     SELECT   SNDRCVF  ASSIGN    TO        DA-01-VI-SNDRCVL3
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       RCV-F93  RCV-F011
                       FILE  STATUS    IS  RCV-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = アンサーファイル                                   *
****************************************************************
 FD  SNDANSR           BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SNDANSER  OF   XFDLIB
                       JOINING   ANS       AS   PREFIX.
******************************************************************
*    ＮＡＶＳ履歴管理ファイル
******************************************************************
 FD  SNDRCVF           LABEL     RECORD    IS   STANDARD.
                       COPY      SNDRCVL3  OF   XFDLIB
                       JOINING   RCV       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  SND-ST                   PIC  X(02).
     03  RCV-ST                   PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  ANS-ERR           PIC N(15) VALUE
         NC"アンサーファイルエラー".
     03  RCV-ERR           PIC N(15) VALUE
         NC"ＮＡＶＳ履歴管理ファイル".
*ワーク領域
 01  WK-AREA.
     03  WK-KANRINO               PIC  9(04) VALUE ZERO.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
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
*日付
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*時刻
 01  TIME-AREA.
     03  WK-TIME            PIC   9(06)  VALUE  ZERO.
     03  WK-TIMESS          PIC   9(02)  VALUE  ZERO.
*
*------------------------------------------------------------*
 LINKAGE              SECTION.
* -----------------------------------------------------------*
*パラメタ取得
 01  LINK-STATUS           PIC X(04).
 01  LINK-KEKKA            PIC X(01).
*
**************************************************************
 PROCEDURE             DIVISION      USING    LINK-STATUS
                                              LINK-KEKKA.
**************************************************************
 DECLARATIVES.
 ANS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SNDANSR.
     MOVE        SND-ST    TO        E-ST.
     MOVE        "SNDANSR" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     RCV-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 RCV-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SNDRCVF.
     MOVE        RCV-ST    TO        E-ST.
     MOVE        "SNDRCVF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     RCV-ERR   UPON      CONS.
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
     DISPLAY "##ANSERFILE CHK START" UPON CONS.
*ファイルのＯＰＥＮ
     OPEN      INPUT   SNDANSR.
     OPEN      I-O     SNDRCVF.
*
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
*システム日付取得
     ACCEPT    TIME-AREA        FROM   TIME.
*
     MOVE      SPACE            TO   LINK-KEKKA.
     MOVE      SPACE            TO   LINK-STATUS.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
*パラメータファイルの読込
     DISPLAY "## MAIN-010 ##" UPON CONS.
     READ      SNDANSR
               AT END
                  MOVE "1"        TO   LINK-KEKKA
                  GO    TO        MAIN-EXIT
     END-READ.
*
     DISPLAY "## MAIN-020 ##" UPON CONS.
*処理結果
     MOVE      " "           TO   LINK-KEKKA
*返信結果区分
     MOVE      ANS-F12       TO   LINK-STATUS.
*ＮＡＶＳ履歴管理ファイルのキー項目を設定
     MOVE      ANS-F01       TO   RCV-F011.
     MOVE      ANS-F13       TO   RCV-F93.
*ＮＡＶＳ履歴管理ファイルの読込
     READ      SNDRCVF
               INVALID
               DISPLAY "## MAIN-021 ##" UPON CONS
               GO    TO        MAIN-WRITE
     END-READ.
*
*カンマ","
     MOVE      ","           TO   RCV-A011.
*送受信区分
     MOVE      ANS-F02       TO   RCV-F012.
*カンマ","
     MOVE      ","           TO   RCV-A012.
*自動手動区分
     MOVE      ANS-F03       TO   RCV-F013.
*カンマ","
     MOVE      ","           TO   RCV-A013.
*送受信データ種別区分
     MOVE      ANS-F04       TO   RCV-F014.
*カンマ","
     MOVE      ","           TO   RCV-A014.
*送受信データ件数
     MOVE      ANS-F05       TO   RCV-F015.
*カンマ","
     MOVE      ","           TO   RCV-A015.
*指示日付
     MOVE      ANS-F06       TO   RCV-F016.
*カンマ","
     MOVE      ","           TO   RCV-A016.
*指示時刻
     MOVE      ANS-F07       TO   RCV-F017.
*カンマ","
     MOVE      ","           TO   RCV-A017.
*指示結果区分
     MOVE      ANS-F08       TO   RCV-F018.
*カンマ","
     MOVE      ","           TO   RCV-A018.
*指示担当者CD
     MOVE      ANS-F09       TO   RCV-F019.
*カンマ","
     MOVE      ","           TO   RCV-A019.
*結果返信日付
     MOVE      ANS-F10       TO   RCV-F01A.
*カンマ","
     MOVE      ","           TO   RCV-A01A.
*結果返信時刻
     MOVE      ANS-F11       TO   RCV-F01B.
*カンマ","
     MOVE      ","           TO   RCV-A01B.
*返品結果区分
     MOVE      ANS-F12       TO   RCV-F01C.
*カンマ","
     MOVE      ","           TO   RCV-A01C.
*実行NO
     MOVE      ANS-F13       TO   RCV-F01D.
*カンマ","
     MOVE      ","           TO   RCV-A01D.
*更新担当者CD
     MOVE      ANS-F09       TO   RCV-F97.
*更新日付
     MOVE      SYS-DATEW     TO   RCV-F98.
*更新時刻
     MOVE      WK-TIME       TO   RCV-F99.
*    ＮＡＶＳ履歴管理ファイル更新
     REWRITE   RCV-REC.
     GO    TO        MAIN-EXIT.
*
 MAIN-WRITE.
     DISPLAY "## MAIN-030 ##" UPON CONS.
*    ＮＡＶＳ履歴管理ファイルレコードの初期化
     MOVE      SPACE         TO   RCV-REC.
     INITIALIZE                   RCV-REC.
*送受信指示_
     MOVE      ANS-F01       TO   RCV-F01.
*カンマ","
     MOVE      ","           TO   RCV-A011.
*送受信区分
     MOVE      ANS-F02       TO   RCV-F012.
*カンマ","
     MOVE      ","           TO   RCV-A012.
*自動手動区分
     MOVE      ANS-F03       TO   RCV-F013.
*カンマ","
     MOVE      ","           TO   RCV-A013.
*送受信データ種別区分
     MOVE      ANS-F04       TO   RCV-F014.
*カンマ","
     MOVE      ","           TO   RCV-A014.
*送受信データ件数
     MOVE      ANS-F05       TO   RCV-F015.
*カンマ","
     MOVE      ","           TO   RCV-A015.
*指示日付
     MOVE      ANS-F06       TO   RCV-F016.
*カンマ","
     MOVE      ","           TO   RCV-A016.
*指示時刻
     MOVE      ANS-F07       TO   RCV-F017.
*カンマ","
     MOVE      ","           TO   RCV-A017.
*指示結果区分
     MOVE      ANS-F08       TO   RCV-F018.
*カンマ","
     MOVE      ","           TO   RCV-A018.
*指示担当者CD
     MOVE      ANS-F09       TO   RCV-F019.
*カンマ","
     MOVE      ","           TO   RCV-A019.
*結果返信日付
     MOVE      ANS-F10       TO   RCV-F01A.
*カンマ","
     MOVE      ","           TO   RCV-A01A.
*結果返信時刻
     MOVE      ANS-F11       TO   RCV-F01B.
*カンマ","
     MOVE      ","           TO   RCV-A01B.
*返品結果区分
     MOVE      ANS-F12       TO   RCV-F01C.
*カンマ","
     MOVE      ","           TO   RCV-A01C.
*実行NO
     MOVE      ANS-F13       TO   RCV-F01D.
*カンマ","
     MOVE      ","           TO   RCV-A01D.
*実行_
     MOVE      ANS-F13       TO   RCV-F93.
*登録担当者CD
     MOVE      ANS-F09       TO   RCV-F94.
*登録日付
     MOVE      SYS-DATEW     TO   RCV-F95.
*登録時刻
     MOVE      WK-TIME       TO   RCV-F96.
*    ＮＡＶＳ履歴管理ファイル登録
     WRITE     RCV-REC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*
     DISPLAY "終了ステータス：" LINK-STATUS UPON CONS.
     DISPLAY "処理結果　　　：" LINK-KEKKA  UPON CONS.
*ファイルのＯＰＥＮ
     CLOSE     SNDRCVF  SNDANSR.
*
 END-EXIT.
     EXIT.
*****************<<  NVS0050B   END PROGRAM  >>******************

```
