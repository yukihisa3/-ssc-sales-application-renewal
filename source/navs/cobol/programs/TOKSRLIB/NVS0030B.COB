****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　トリガーファイル作成＆履歴更新    *
*    作成日／更新日　　　：　19/12/19                          *
*    作成者／更新者　　　：　ＮＡＶ                            *
*    処理概要　　　　　　：　パラメータファイルを読み、トリガー*
*                            ファイルの作成及び、ＮＡＶＳ送受信*
*                            履歴管理ファイルを更新する。      *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NVS0030B.
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
*受信パラメタファイル
     SELECT   PARAFIL   ASSIGN    TO       DA-01-S-D365PARA
                       FILE   STATUS   IS  PAR-ST.
*トリガーファイル
     SELECT   TRIGGER  ASSIGN    TO        DA-01-S-SNDTRIGG
                       FILE  STATUS    IS  TRG-ST.
*ＮＡＶＳ履歴管理ファイル
     SELECT   SNDRCVF  ASSIGN    TO        DA-01-VI-SNDRCVL3
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       RCV-F93  RCV-F011
                       FILE  STATUS    IS  RCV-ST.
*
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
                       COPY      D365PARA  OF   XFDLIB
                       JOINING   PAR       AS   PREFIX.
******************************************************************
*    トリガーファイル
******************************************************************
 FD  TRIGGER           BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SNDTRIGG  OF   XFDLIB
                       JOINING   TRG       AS   PREFIX.
*
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
     03  PAR-ST                   PIC  X(02).
     03  TRG-ST                   PIC  X(02).
     03  RCV-ST                   PIC  X(02).
*WRK-TRIGGER
*    COPY     SNDTRIGG
*    DISJOINING XXX JOINING WRK AS PREFIX.
     COPY      SNDTRIGG  OF   XFDLIB
     JOINING   WRK       AS   PREFIX.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  PAR-ERR           PIC N(15) VALUE
         NC"パラメタファイルエラー".
     03  TRG-ERR           PIC N(15) VALUE
         NC"トリガーファイルエラー".
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
*------------------------------------------------------------*
*パラメタ取得
 01  LINK-KEKKA            PIC X(01).
*
**************************************************************
 PROCEDURE             DIVISION      USING    LINK-KEKKA.
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
 TRG-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE TRIGGER.
     MOVE        TRG-ST    TO        E-ST.
     MOVE        "TRIGGER" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TRG-ERR   UPON      CONS.
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
*    DISPLAY "***  NVS0030B  ***"  UPON CONS.
*ファイルのＯＰＥＮ
     OPEN      INPUT   PARAFIL.
     OPEN      I-O     SNDRCVF.
     OPEN      OUTPUT  TRIGGER.
*
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
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*    トリガーファイルレコードの初期化
     MOVE      SPACE         TO   TRG-REC.
     INITIALIZE                   TRG-REC.
     MOVE      SPACE         TO   LINK-KEKKA.
*
*パラメータファイルの読込
     READ      PARAFIL
               AT END
                  MOVE "9"        TO   LINK-KEKKA
                  GO    TO        MAIN-EXIT
     END-READ.
*送受信指示_
     MOVE      PAR-F02       TO   TRG-F01.
*カンマ","
     MOVE      ","           TO   TRG-A01.
*送受信区分
     MOVE      PAR-F03       TO   TRG-F02.
*カンマ","
     MOVE      ","           TO   TRG-A02.
*自動手動区分
     MOVE      PAR-F04       TO   TRG-F03.
*カンマ","
     MOVE      ","           TO   TRG-A03.
*送受信データ種別区分
     MOVE      PAR-F05       TO   TRG-F04.
*カンマ","
     MOVE      ","           TO   TRG-A04.
*送受信データ件数
     MOVE      ZERO          TO   TRG-F05.
*カンマ","
     MOVE      ","           TO   TRG-A05.
*指示日付
     MOVE      PAR-F06       TO   TRG-F06.
*カンマ","
     MOVE      ","           TO   TRG-A06.
*指示時刻
     MOVE      PAR-F07       TO   TRG-F07.
*カンマ","
     MOVE      ","           TO   TRG-A07.
*指示結果区分
     MOVE      SPACE         TO   TRG-F08.
*カンマ","
     MOVE      ","           TO   TRG-A08.
*指示担当者CD
     MOVE      SPACE         TO   TRG-F09.
*カンマ","
     MOVE      ","           TO   TRG-A09.
*結果返信日付
     MOVE      ZERO          TO   TRG-F10.
*カンマ","
     MOVE      ","           TO   TRG-A10.
*結果返信時刻
     MOVE      ZERO          TO   TRG-F11.
*カンマ","
     MOVE      ","           TO   TRG-A11.
*返品結果区分
     MOVE      SPACE         TO   TRG-F12.
*カンマ","
     MOVE      ","           TO   TRG-A12.
*実行NO
     MOVE      PAR-F01       TO   TRG-F13.
*カンマ","
     MOVE      ","           TO   TRG-A13.
*予備領域
     MOVE      SPACE         TO   TRG-FIL.
*
     MOVE      TRG-REC       TO   WRK-REC.
*    トリガーファイル登録
     WRITE     TRG-REC.
*
*ＮＡＶＳ履歴管理ファイルのキー項目を設定
     MOVE      PAR-F02       TO   RCV-F011.
     MOVE      PAR-F01       TO   RCV-F93.
*ＮＡＶＳ履歴管理ファイルの読込
     READ      SNDRCVF
               INVALID
               GO    TO        MAIN-WRITE
     END-READ.
*
*カンマ","
     MOVE      WRK-A01       TO   RCV-A011.
*送受信区分
     MOVE      WRK-F02       TO   RCV-F012.
*カンマ","
     MOVE      WRK-A02       TO   RCV-A012.
*自動手動区分
     MOVE      WRK-F03       TO   RCV-F013.
*カンマ","
     MOVE      WRK-A03       TO   RCV-A013.
*送受信データ種別区分
     MOVE      WRK-F04       TO   RCV-F014.
*カンマ","
     MOVE      WRK-A04       TO   RCV-A014.
*送受信データ件数
     MOVE      WRK-F05       TO   RCV-F015.
*カンマ","
     MOVE      WRK-A05       TO   RCV-A015.
*指示日付
     MOVE      WRK-F06       TO   RCV-F016.
*カンマ","
     MOVE      WRK-A06       TO   RCV-A016.
*指示時刻
     MOVE      WRK-F07       TO   RCV-F017.
*カンマ","
     MOVE      WRK-A07       TO   RCV-A017.
*指示結果区分
     MOVE      WRK-F08       TO   RCV-F018.
*カンマ","
     MOVE      WRK-A08       TO   RCV-A018.
*指示担当者CD
     MOVE      WRK-F09       TO   RCV-F019.
*カンマ","
     MOVE      WRK-A09       TO   RCV-A019.
*結果返信日付
     MOVE      WRK-F10       TO   RCV-F01A.
*カンマ","
     MOVE      WRK-A10       TO   RCV-A01A.
*結果返信時刻
     MOVE      WRK-F11       TO   RCV-F01B.
*カンマ","
     MOVE      WRK-A11       TO   RCV-A01B.
*返品結果区分
     MOVE      WRK-F12       TO   RCV-F01C.
*カンマ","
     MOVE      WRK-A12       TO   RCV-A01C.
*実行NO
     MOVE      WRK-F13       TO   RCV-F01D.
*カンマ","
     MOVE      WRK-A13       TO   RCV-A01D.
*更新担当者CD
     MOVE      PAR-F08       TO   RCV-F97.
*更新日付
     MOVE      SYS-DATEW     TO   RCV-F98.
*更新時刻
     MOVE      WK-TIME       TO   RCV-F99.
*    ＮＡＶＳ履歴管理ファイル更新
     REWRITE   RCV-REC.
     GO    TO        MAIN-EXIT.
*
 MAIN-WRITE.
*    ＮＡＶＳ履歴管理ファイルレコードの初期化
     MOVE      SPACE         TO   RCV-REC.
     INITIALIZE                   RCV-REC.
*送受信指示レコードにトリガーファイルを設定
     MOVE      WRK-REC       TO   RCV-F01.
*実行_
     MOVE      PAR-F01       TO   RCV-F93.
*送受信指示_
     MOVE      PAR-F02       TO   RCV-F011.
*登録担当者CD
     MOVE      PAR-F08       TO   RCV-F94.
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
*    DISPLAY "処理結果　　　：" LINK-KEKKA UPON CONS.
*ファイルのＯＰＥＮ
     CLOSE     PARAFIL SNDRCVF TRIGGER.
*
 END-EXIT.
     EXIT.
*****************<<  NVS0030B   END PROGRAM  >>******************
