****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　トリガーファイル作成＆履歴更新    *
*    作成日／更新日　　　：　19/12/19                          *
*    作成者／更新者　　　：　ＮＡＶ                            *
*    処理概要　　　　　　：　パラメータを受取り、パラアメータ情*
*                            報をＮＡＶＳ送受信履歴管理ファイル*
*                            に更新する。                      *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NVS0040B.
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
*ＮＡＶＳ履歴管理ファイル
     SELECT   SNDRCVF  ASSIGN    TO        DA-01-VI-SNDRCVL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       RCV-F011
                       FILE  STATUS    IS  RCV-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
******************************************************************
*    ＮＡＶＳ履歴管理ファイル
******************************************************************
 FD  SNDRCVF           LABEL     RECORD    IS   STANDARD.
                       COPY      SNDRCVL1  OF   XFDLIB
                       JOINING   RCV       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  RCV-ST                   PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
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
 01  PARA-AREA.
     03  PARA-BUMON    PIC  X(04).   *>部門CD
     03  PARA-TANTO    PIC  X(02).   *>担当者CD
     03  PARA-F011     PIC  9(10).   *>送受信指示_
     03  PARA-F012     PIC  X(01).   *>送受信区分
     03  PARA-F013     PIC  X(01).   *>自動手動区分
     03  PARA-F014     PIC  X(02).   *>送受信データ種別区分
     03  PARA-F015     PIC  9(07).   *>送受信データ件数
     03  PARA-F016     PIC  9(08).   *>指示日付
     03  PARA-F017     PIC  9(06).   *>指示時刻
     03  PARA-F018     PIC  X(04).   *>指示結果区分
     03  PARA-F019     PIC  X(02).   *>指示担当者CD
     03  PARA-F01A     PIC  9(08).   *>結果返信日付
     03  PARA-F01B     PIC  9(06).   *>結果返信時刻
     03  PARA-F01C     PIC  X(04).   *>返信結果区分
     03  PARA-F01D     PIC  9(07).   *>実行_
*
**************************************************************
 PROCEDURE             DIVISION      USING    PARA-AREA.
**************************************************************
 DECLARATIVES.
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
*ファイルのＯＰＥＮ
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
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
*ＮＡＶＳ履歴管理ファイルのキー項目を設定
     MOVE      PARA-F011     TO   RCV-F011.
*ＮＡＶＳ履歴管理ファイルの読込
     READ      SNDRCVF
               INVALID
               GO    TO        MAIN-WRITE
     END-READ.
*
*カンマ","
     MOVE      ","           TO   RCV-A011.
*送受信区分
     MOVE      PARA-F012     TO   RCV-F012.
*カンマ","
     MOVE      ","           TO   RCV-A012.
*自動手動区分
     MOVE      PARA-F013     TO   RCV-F013.
*カンマ","
     MOVE      ","           TO   RCV-A013.
*送受信データ種別区分
     MOVE      PARA-F014     TO   RCV-F014.
*カンマ","
     MOVE      ","           TO   RCV-A014.
*送受信データ件数
     MOVE      PARA-F015     TO   RCV-F015.
*カンマ","
     MOVE      ","           TO   RCV-A015.
*指示日付
     MOVE      PARA-F016     TO   RCV-F016.
*カンマ","
     MOVE      ","           TO   RCV-A016.
*指示時刻
     MOVE      PARA-F017     TO   RCV-F017.
*カンマ","
     MOVE      ","           TO   RCV-A017.
*指示結果区分
     MOVE      PARA-F018     TO   RCV-F018.
*カンマ","
     MOVE      ","           TO   RCV-A018.
*指示担当者CD
     MOVE      PARA-F019     TO   RCV-F019.
*カンマ","
     MOVE      ","           TO   RCV-A019.
*結果返信日付
     MOVE      PARA-F01A     TO   RCV-F01A.
*カンマ","
     MOVE      ","           TO   RCV-A01A.
*結果返信時刻
     MOVE      PARA-F01B     TO   RCV-F01B.
*カンマ","
     MOVE      ","           TO   RCV-A01B.
*返品結果区分
     MOVE      PARA-F01C     TO   RCV-F01C.
*カンマ","
     MOVE      ","           TO   RCV-A01C.
*実行NO
     MOVE      PARA-F01D     TO   RCV-F01D.
*カンマ","
     MOVE      ","           TO   RCV-A01D.
*実行_
     MOVE      PARA-F01D     TO   RCV-F93.
*更新担当者CD
     MOVE      PARA-F019     TO   RCV-F97.
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
*送受信指示_
     MOVE      PARA-F011     TO   RCV-F011.
*カンマ","
     MOVE      ","           TO   RCV-A011.
*送受信区分
     MOVE      PARA-F012     TO   RCV-F012.
*カンマ","
     MOVE      ","           TO   RCV-A012.
*自動手動区分
     MOVE      PARA-F013     TO   RCV-F013.
*カンマ","
     MOVE      ","           TO   RCV-A013.
*送受信データ種別区分
     MOVE      PARA-F014     TO   RCV-F014.
*カンマ","
     MOVE      ","           TO   RCV-A014.
*送受信データ件数
     MOVE      PARA-F015     TO   RCV-F015.
*カンマ","
     MOVE      ","           TO   RCV-A015.
*指示日付
     MOVE      PARA-F016     TO   RCV-F016.
*カンマ","
     MOVE      ","           TO   RCV-A016.
*指示時刻
     MOVE      PARA-F017     TO   RCV-F017.
*カンマ","
     MOVE      ","           TO   RCV-A017.
*指示結果区分
     MOVE      PARA-F018     TO   RCV-F018.
*カンマ","
     MOVE      ","           TO   RCV-A018.
*指示担当者CD
     MOVE      PARA-F019     TO   RCV-F019.
*カンマ","
     MOVE      ","           TO   RCV-A019.
*結果返信日付
     MOVE      PARA-F01A     TO   RCV-F01A.
*カンマ","
     MOVE      ","           TO   RCV-A01A.
*結果返信時刻
     MOVE      PARA-F01B     TO   RCV-F01B.
*カンマ","
     MOVE      ","           TO   RCV-A01B.
*返品結果区分
     MOVE      PARA-F01C     TO   RCV-F01C.
*カンマ","
     MOVE      ","           TO   RCV-A01C.
*実行NO
     MOVE      PARA-F01D     TO   RCV-F01D.
*カンマ","
     MOVE      ","           TO   RCV-A01D.
*実行_
     MOVE      PARA-F01D     TO   RCV-F93.
*登録担当者CD
     MOVE      PARA-F019     TO   RCV-F94.
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
*ファイルのＯＰＥＮ
     CLOSE     SNDRCVF.
*
 END-EXIT.
     EXIT.
*****************<<  NVS0040B   END PROGRAM  >>******************
