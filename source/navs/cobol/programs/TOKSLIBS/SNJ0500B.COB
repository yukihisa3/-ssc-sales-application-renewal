****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　自動受信時間監視／受信処理実行    *
*    作成日／更新日　　　：　2010/08/16                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　自動受信の起動制御を行う。　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNJ0500B.
 AUTHOR.               ABE.
 DATE-WRITTEN.         10/08/16.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*当日スケジュールマスタ
     SELECT  JSMDAYF   ASSIGN    TO        DA-01-VI-JSMDAYL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       TJS-F01
                                           TJS-F02
                                           TJS-F03
                       FILE      STATUS    TJS-ST.
*ＥＤＩ回線種別マスタ
     SELECT  JSMKAIF   ASSIGN    TO        DA-01-VI-JSMKAIL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KAI-F01
                                           KAI-F02
                       FILE      STATUS    KAI-ST.
*ＥＤＩ回線種別マスタ（固定端末取得）
     SELECT  JSMKAIF2  ASSIGN    TO        DA-01-VI-JSMKAIL2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KAI2-F03
                                           KAI2-F01
                                           KAI2-F02
                       FILE      STATUS    KAI2-ST.
*ＥＤＩ管理マスタ
     SELECT  JSMEDIF   ASSIGN    TO        DA-01-VI-JSMEDIL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       EDI-F01
                                           EDI-F02
                                           EDI-F03
                       FILE      STATUS    EDI-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 当日スケジュールマスタ                             *
****************************************************************
 FD  JSMDAYF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMDAYF   OF   XFDLIB
                       JOINING   TJS       AS   PREFIX.
****************************************************************
*    FILE = ＥＤＩ回線種別マスタ                               *
****************************************************************
 FD  JSMKAIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMKAIF   OF   XFDLIB
                       JOINING   KAI       AS   PREFIX.
****************************************************************
*    FILE = ＥＤＩ回線種別マスタ（固定端末取得）
****************************************************************
 FD  JSMKAIF2
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMKAIF   OF   XFDLIB
                       JOINING   KAI2      AS   PREFIX.
****************************************************************
*    FILE = ＥＤＩ管理マスタ                                   *
****************************************************************
 FD  JSMEDIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMEDIF   OF   XFDLIB
                       JOINING   EDI       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TJS-ST                   PIC  X(02).
     03  KAI-ST                   PIC  X(02).
     03  KAI2-ST                  PIC  X(02).
     03  EDI-ST                   PIC  X(02).
*区分、ＦＬＧエリア
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  EDI-INV-FLG              PIC  X(03)  VALUE  SPACE.
     03  GO-FLG                   PIC  X(02)  VALUE  SPACE.
*システム日付格納
 01  SYS-DATE                     PIC  9(06)  VALUE  ZERO.
*システム時間格納
 01  WK-TIME.
     03  WK-TIME-1                PIC  9(04)  VALUE  ZERO.
     03  WK-TIME-2                PIC  9(04)  VALUE  ZERO.
*時間編集領域
 01  WK-DATE.
     03  WK-YYYY                  PIC  9(04)  VALUE  ZERO.
     03  WK-MM                    PIC  9(02)  VALUE  ZERO.
     03  WK-DD                    PIC  9(02)  VALUE  ZERO.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  TJS-ERR           PIC N(15) VALUE
         NC"当日スケジュールマスタエラー".
     03  KAI-ERR           PIC N(15) VALUE
         NC"回線種別マスタエラー".
     03  EDI-ERR           PIC N(15) VALUE
         NC"ＥＤＩ管理マスタエラー".
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
*受信パスワード編集エリア
 01  WK-PASS.
     03  PASS-YOKYU               PIC  X(02).
     03  PASS-ID                  PIC  X(02).
     03  PASS-CENTER              PIC  X(06).
     03  PASS-DATA                PIC  X(02).
     03  PASS-SIKIBETU            PIC  X(01).
*受信モード
 01  WK-JYUKBN                    PIC  X(01)  VALUE  "1".
*伝票更新起動チェック用
 01  LINK-CHK                     PIC  X(01)  VALUE  SPACE.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 TJS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMDAYF.
     MOVE        TJS-ST    TO        E-ST.
     MOVE        "JSMDAYF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TJS-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
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
 KAI2-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMKAIF2.
     MOVE        KAI2-ST    TO        E-ST.
     MOVE        "JSMKAIF2" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KAI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 EDI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMEDIF.
     MOVE        EDI-ST    TO        E-ST.
     MOVE        "JSMEDIF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     EDI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
*ＰＧ開始メッセージ
*    システム日付／時刻取得
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC    UNTIL   END-FLG  =  "END".
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
     OPEN      I-O     JSMDAYF.
     OPEN      I-O     JSMKAIF.
     OPEN      I-O     JSMKAIF2.
     OPEN      INPUT   JSMEDIF.
*システム日付取得／時刻取得
     ACCEPT    SYS-DATE    FROM    DATE.
     ACCEPT    WK-TIME     FROM    TIME.
*システム日付６桁→８桁変換（サブ日付チェック／変換）
 INIT010.
     MOVE      "3"         TO      LINK-IN-KBN.
     MOVE      SYS-DATE    TO      LINK-IN-YMD6.
     MOVE      ZERO        TO      LINK-IN-YMD8.
     MOVE      ZERO        TO      LINK-OUT-RET.
     MOVE      ZERO        TO      LINK-OUT-YMD.
     CALL     "SKYDTCKB"   USING   LINK-IN-KBN
                                   LINK-IN-YMD6
                                   LINK-IN-YMD8
                                   LINK-OUT-RET
                                   LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD TO     WK-DATE.
*ＰＧスタートメッセージ
     DISPLAY "***ｼﾞﾄﾞｳｼﾞｭｼﾝ ｶｲｼ -> " WK-YYYY "/" WK-MM "/" WK-DD
         " " WK-TIME-1(1:2) ":" WK-TIME-1(3:2) "***" UPON CONS.
*伝票更新処理起動確認
 INIT020.
     CALL     "SCV0020B"   USING   LINK-CHK.
*    ﾁｪｯｸ結果判定
     IF        LINK-CHK  =  "0"
               DISPLAY NC"伝票更新開始" UPON CONS
               CALL "DENSTART"
     END-IF.
*当日スケジュールマスタスタート
 INIT030.
     MOVE      WK-DATE      TO     TJS-F01.
     MOVE      WK-TIME-1    TO     TJS-F02.
     MOVE      ZERO         TO     TJS-F03.
     START     JSMDAYF KEY IS  >=  TJS-F01 TJS-F02 TJS-F03
               INVALID
               DISPLAY NC"＊＊＊＊＊＊＊＊＊＊＊" UPON CONS
               DISPLAY NC"＊本日の自動受信なし＊" UPON CONS
               DISPLAY NC"＊＊＊＊＊＊＊＊＊＊＊" UPON CONS
               MOVE    "END"      TO     END-FLG
               GO                 TO     INIT-EXIT
     END-START.
*当日スケジュールマスタ初期読込み
 INIT040.
     PERFORM   JSMDAYF-READ-SEC.
     IF        END-FLG  =  "END"
               DISPLAY NC"＊＊＊＊＊＊＊＊＊＊＊" UPON CONS
               DISPLAY NC"＊本日の自動受信なし＊" UPON CONS
               DISPLAY NC"＊＊＊＊＊＊＊＊＊＊＊" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*時間監視
 MAIN010.
*システム時間取得
     ACCEPT    WK-TIME       FROM      TIME.
*受信時間とシステム時間比較
     IF        TJS-F02     >   WK-TIME-1
               GO              TO   MAIN010
     END-IF.
     DISPLAY "ｼﾞｭｼﾝSTART = " WK-TIME-1(1:2) ":"
                             WK-TIME-1(3:2)  UPON CONS.
*ＥＤＩ管理マスタ（受信情報取得）
     PERFORM   JSMEDIF-READ-SEC.
*ＥＤＩ管理マスタ存在チェック
     IF        EDI-INV-FLG  =  "INV"
               DISPLAY "***EDIｶﾝﾘﾏｽﾀ ｼｭﾄｸ ERR***"  UPON CONS
               DISPLAY "***ﾄﾘﾋｷｻｷCD  = " TJS-F03   UPON CONS
               DISPLAY "***ｼﾞｭｼﾝTIME = " TJS-F02   UPON CONS
***************MOVE    "END"             TO     END-FLG
               MOVE    "4000"            TO     PROGRAM-STATUS
*当日スケジュールマスタ結果更新
*    処理結果を、”自動受信Ｅ”に変更する。
               MOVE    "K553"            TO     TJS-F15
               REWRITE   TJS-REC
               GO                        TO     MAIN050
     END-IF.
*回線取得
 MAIN020.
     IF    EDI-F06  =  SPACE
           PERFORM   JSMKAIF-READ-SEC
           IF        END-FLG  =  "END"
                     GO           TO         MAIN-EXIT
           END-IF
     ELSE
           PERFORM   JSMKAIF2-READ-SEC
           IF        END-FLG =  "END"
                     GO           TO         MAIN-EXIT
           END-IF
     END-IF.
*    回線使用状況チェック
     IF        GO-FLG   =  "NG"
               GO           TO         MAIN020
     END-IF.
 MAIN030.
*    受信処理投入
*  （受信日付／受信時間／データ種別／受配信区分／取引先ＣＤ
*   ／回線種別／回線制御番号／端末名／パラメータファイル名）
*当日スケジュールマスタ開放
*    受信状況状態を、”受信中”に変更する。
     MOVE      "K521"       TO         TJS-F15.
     REWRITE   TJS-REC.
*受信制御ジョブ起動
 MAIN040.
     DISPLAY "FURIWAKR START" UPON CONS.
     DISPLAY "KAI-F01 = " KAI-F01 UPON CONS.
     DISPLAY "KAI-F02 = " KAI-F02 UPON CONS.
     DISPLAY "KAI-F03 = " KAI-F03 UPON CONS.
     CALL "CEDISBMJ" USING TJS-F01 TJS-F02 EDI-F01
                           EDI-F02 TJS-F03 KAI-F01
************************** TJS-F16 TJS-F03 KAI-F01
                           KAI-F02 KAI-F03 KAI-F06
                           WK-JYUKBN.
     DISPLAY "FURIWAKR END  " UPON CONS.
*
 MAIN050.
*当日スケジュールマスタ読込み
     PERFORM   JSMDAYF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"            TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     JSMDAYF  JSMKAIF  JSMKAIF2  JSMEDIF.
*実行管理ファイルクリア
     CALL      "PCV00310".
*ＰＧスタートメッセージ
     ACCEPT    WK-TIME  FROM     TIME.
     DISPLAY "***ｼﾞﾄﾞｳｼﾞｭｼﾝ ｵﾜﾘ -> " WK-YYYY "/" WK-MM "/" WK-DD
         " " WK-TIME-1(1:2) ":" WK-TIME-1(3:2) "***" UPON CONS.
*
 END-EXIT.
     EXIT.
****************************************************************
*             当日スケジュールマスタ読込み           2.1       *
****************************************************************
 JSMDAYF-READ-SEC      SECTION.
     MOVE     "JSMDAYF-READ-SEC"   TO   S-NAME.
*当日スケジュールマスタ読込み
     READ      JSMDAYF    AT   END
               MOVE     "END"      TO   END-FLG
               GO                  TO   JSMDAYF-READ-EXIT
     END-READ.
*スケジュール日付チェック
     IF        WK-DATE   NOT =   TJS-F01
               MOVE     "END"      TO   END-FLG
               GO                  TO   JSMDAYF-READ-EXIT
     END-IF.
*実行済確認
     IF        TJS-F10   =   1
               GO                  TO   JSMDAYF-READ-SEC
     END-IF.
*
 JSMDAYF-READ-EXIT.
     EXIT.
****************************************************************
*             ＥＤＩ管理マスタ読込み                 2.2       *
****************************************************************
 JSMEDIF-READ-SEC      SECTION.
     MOVE     "JSMEDIF-READ-SEC"   TO   S-NAME.
*ＥＤＩ管理マスタ読込み
     MOVE      TJS-F04             TO   EDI-F01.
     MOVE      "1"                 TO   EDI-F02.
     MOVE      TJS-F03             TO   EDI-F03.
     READ      JSMEDIF    INVALID
               MOVE     "INV"      TO   EDI-INV-FLG
               NOT  INVALID
               MOVE     SPACE      TO   EDI-INV-FLG
     END-READ.
*
 JSMEDIF-READ-EXIT.
     EXIT.
****************************************************************
*             回線管理マスタ読込み                   2.3       *
****************************************************************
 JSMKAIF-READ-SEC      SECTION.
     MOVE     "JSMKAIF-READ-SEC"   TO   S-NAME.
*回線管理マスタスタート
     MOVE      SPACE               TO   GO-FLG.
     MOVE      EDI-F04             TO   KAI-F01.
     MOVE      ZERO                TO   KAI-F02.
     START     JSMKAIF   KEY  IS   >=   KAI-F01 KAI-F02
               INVALID
               DISPLAY "***ｶｲｾﾝﾏｽﾀ  START ERR1***"  UPON CONS
               DISPLAY "***ｶｲｾﾝﾒｲｼｮｳ = " KAI-F01   UPON CONS
               MOVE    "END"             TO     END-FLG
               MOVE    "4000"            TO     PROGRAM-STATUS
               GO                        TO     JSMKAIF-READ-EXIT
     END-START.
*スタート後初期読込み
     READ      JSMKAIF   AT  END
               DISPLAY "***ｶｲｾﾝﾏｽﾀ RD-INV ERR2***"  UPON CONS
               DISPLAY "***ｶｲｾﾝﾒｲｼｮｳ = " KAI-F01   UPON CONS
               MOVE    "END"             TO     END-FLG
               MOVE    "4000"            TO     PROGRAM-STATUS
               GO                        TO     JSMKAIF-READ-EXIT
               NOT  AT  END
               IF       EDI-F04    NOT =    KAI-F01
                    DISPLAY "***ｶｲｾﾝﾏｽﾀ RD-INV ERR3***"  UPON CONS
                    DISPLAY "***ｶｲｾﾝﾒｲｼｮｳ = " KAI-F01   UPON CONS
                    MOVE    "END"    TO     END-FLG
                    MOVE    "4000"   TO     PROGRAM-STATUS
                    REWRITE  KAI-REC
                    GO               TO     JSMKAIF-READ-EXIT
               END-IF
     END-READ.
*回線状況チェック
 KAISEN010.
     IF        KAI-F07  =  SPACE
               MOVE     "OK"             TO     GO-FLG
               MOVE      1               TO     KAI-F07
               MOVE      EDI-F03         TO     KAI-F08
               REWRITE   KAI-REC
     ELSE
               READ      JSMKAIF   AT  END
                         MOVE    "NG"    TO     GO-FLG
                         NOT  AT  END
                         IF   KAI-F01  NOT =  EDI-F04
                              MOVE  "NG" TO     GO-FLG
                              REWRITE    KAI-REC
                         ELSE
                              GO         TO     KAISEN010
                         END-IF
               END-READ
     END-IF.
*
 JSMKAIF-READ-EXIT.
     EXIT.
****************************************************************
*             回線管理マスタ読込み（固定端末指定）   2.4       *
****************************************************************
 JSMKAIF2-READ-SEC     SECTION.
     MOVE     "JSMKAIF2-READ-SEC"  TO   S-NAME.
*回線管理マスタスタート
     MOVE      SPACE               TO   GO-FLG.
     MOVE      EDI-F06             TO   KAI2-F03.
     MOVE      EDI-F04             TO   KAI2-F01.
     MOVE      ZERO                TO   KAI2-F02.
     START     JSMKAIF2 KEY IS  >=  KAI2-F03 KAI2-F01 KAI2-F02
               INVALID
               DISPLAY "***ｶｲｾﾝﾏｽﾀ F2 START ERR1***"  UPON CONS
               DISPLAY "***ｶｲｾﾝﾀﾝﾏﾂ = " KAI2-F03   UPON CONS
               MOVE    "END"             TO     END-FLG
               MOVE    "4000"            TO     PROGRAM-STATUS
               GO                        TO     JSMKAIF-READ-EXIT
     END-START.
*スタート後初期読込み
     READ      JSMKAIF2  AT  END
               DISPLAY "***ｶｲｾﾝﾏｽﾀ F2 RD-INV ERR2***" UPON CONS
               DISPLAY "***ｶｲｾﾝﾒｲｼｮｳ = " KAI-F01   UPON CONS
               MOVE    "END"             TO     END-FLG
               MOVE    "4000"            TO     PROGRAM-STATUS
               GO                        TO     JSMKAIF-READ-EXIT
               NOT  AT  END
               IF       EDI-F06    NOT =    KAI2-F03
                 DISPLAY "***ｶｲｾﾝﾏｽﾀ F2 RD-INV ERR3***" UPON CONS
                 DISPLAY "***ｶｲｾﾝﾒｲｼｮｳ = " KAI2-F01   UPON CONS
                    MOVE    "END"    TO     END-FLG
                    MOVE    "4000"   TO     PROGRAM-STATUS
                    REWRITE  KAI2-REC
                    GO               TO     JSMKAIF2-READ-EXIT
               END-IF
     END-READ.
*回線状況チェック
 KAISEN010-2.
     IF        KAI2-F07  =  SPACE
         IF  ( KAI2-F03  =  EDI-F06 ) AND
             ( KAI2-F01  =  EDI-F04 )
               MOVE     "OK"             TO     GO-FLG
               MOVE     KAI2-REC         TO     KAI-REC
               MOVE      1               TO     KAI2-F07
               MOVE      EDI-F03         TO     KAI2-F08
               REWRITE   KAI2-REC
         ELSE
               READ      JSMKAIF2  AT  END
                         MOVE    "NG"    TO     GO-FLG
                         NOT  AT  END
                         IF   KAI2-F03  NOT =  EDI-F06
                              MOVE  "NG" TO     GO-FLG
***************************** REWRITE    KAI2-REC
                         ELSE
                              GO         TO     KAISEN010-2
                         END-IF
               END-READ
         END-IF
     ELSE
               READ      JSMKAIF2  AT  END
                         MOVE    "NG"    TO     GO-FLG
                         NOT  AT  END
                         IF ( KAI2-F03  NOT =  EDI-F06 ) AND
                            ( KAI2-F01  NOT =  EDI-F04 )
                              MOVE  "NG" TO     GO-FLG
***************************** REWRITE    KAI2-REC
                         ELSE
                              GO         TO     KAISEN010-2
                         END-IF
               END-READ
     END-IF.
*
 JSMKAIF2-READ-EXIT.
     EXIT.
*****************<<  SNJ0500B   END PROGRAM  >>******************
