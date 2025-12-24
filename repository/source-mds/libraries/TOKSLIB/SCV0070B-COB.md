# SCV0070B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SCV0070B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　自動受信時間監視／受信処理実行    *
*    作成日／更新日　　　：　99/09/20  2005/04/28              *
*    作成者／更新者　　　：　NAV       NAV                     *
*    処理概要　　　　　　：　自動受信の起動制御を行う。　　　　*
*    2004/04/28　複数起動バーションに変更                      *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SCV0070B.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         99/09/20.
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
     SELECT  JHMTJSF   ASSIGN    TO        DA-01-VI-JHMTJSL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       TJS-F01
                                           TJS-F02
                                           TJS-F03
                       FILE      STATUS    TJS-ST.
*回線種別マスタ
     SELECT  JHMKAIF   ASSIGN    TO        DA-01-VI-JHMKAIL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KAI-F01
                                           KAI-F02
                       FILE      STATUS    KAI-ST.
*ＥＯＳ管理マスタ
     SELECT  JHMEOSF   ASSIGN    TO        DA-01-VI-JHMEOSL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       EOS-F01
                                           EOS-F02
                                           EOS-F03
                       FILE      STATUS    EOS-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 当日スケジュールマスタ                             *
****************************************************************
 FD  JHMTJSF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMTJSF   OF   XFDLIB
                       JOINING   TJS       AS   PREFIX.
****************************************************************
*    FILE = 回線種別マスタ                                     *
****************************************************************
 FD  JHMKAIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMKAIF   OF   XFDLIB
                       JOINING   KAI       AS   PREFIX.
****************************************************************
*    FILE = ＥＯＳ管理マスタ                                   *
****************************************************************
 FD  JHMEOSF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMEOSF   OF   XFDLIB
                       JOINING   EOS       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TJS-ST                   PIC  X(02).
     03  KAI-ST                   PIC  X(02).
     03  EOS-ST                   PIC  X(02).
*区分、ＦＬＧエリア
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  EOS-INV-FLG              PIC  X(03)  VALUE  SPACE.
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
     03  EOS-ERR           PIC N(15) VALUE
         NC"ＥＯＳ管理マスタエラー".
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
     USE         AFTER     EXCEPTION PROCEDURE JHMTJSF.
     MOVE        TJS-ST    TO        E-ST.
     MOVE        "JHMTJSF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TJS-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KAI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMKAIF.
     MOVE        KAI-ST    TO        E-ST.
     MOVE        "JHMKAIF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KAI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 EOS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMEOSF.
     MOVE        EOS-ST    TO        E-ST.
     MOVE        "JHMEOSF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     EOS-ERR   UPON      CONS.
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
     OPEN      I-O     JHMTJSF.
     OPEN      I-O     JHMKAIF.
     OPEN      INPUT   JHMEOSF.
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
     START     JHMTJSF KEY IS  >=  TJS-F01 TJS-F02 TJS-F03
               INVALID
               DISPLAY NC"＊＊＊＊＊＊＊＊＊＊＊" UPON CONS
               DISPLAY NC"＊本日の自動受信なし＊" UPON CONS
               DISPLAY NC"＊＊＊＊＊＊＊＊＊＊＊" UPON CONS
               MOVE    "END"      TO     END-FLG
               GO                 TO     INIT-EXIT
     END-START.
*当日スケジュールマスタ初期読込み
 INIT040.
     PERFORM   JHMTJSF-READ-SEC.
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
*ＥＯＳ管理マスタ（受信情報取得）
     PERFORM   JHMEOSF-READ-SEC.
*ＥＯＳ管理マスタ存在チェック
     IF        EOS-INV-FLG  =  "INV"
               DISPLAY "***EOSｶﾝﾘﾏｽﾀ ｼｭﾄｸ ERR***"  UPON CONS
               DISPLAY "***ﾄﾘﾋｷｻｷCD  = " TJS-F03   UPON CONS
               DISPLAY "***ｼﾞｭｼﾝTIME = " TJS-F02   UPON CONS
               MOVE    "END"             TO     END-FLG
               MOVE    "4000"            TO     PROGRAM-STATUS
               GO                        TO     MAIN-EXIT
     END-IF.
*回線取得
 MAIN020.
     PERFORM   JHMKAIF-READ-SEC.
     IF        END-FLG  =  "END"
               GO           TO         MAIN-EXIT
     END-IF.
*    回線使用状況チェック
     IF        GO-FLG   =  "NG"
               GO           TO         MAIN020
     END-IF.
*受信処理起動
*    受信パスワード編集
     MOVE      "01"         TO         PASS-YOKYU.
     MOVE      EOS-F05      TO         PASS-ID.
     MOVE      EOS-F06      TO         PASS-CENTER.
     MOVE      EOS-F10      TO         PASS-DATA.
     MOVE      EOS-F07      TO         PASS-SIKIBETU.
*    受信処理投入
*  （受信パスワード／データ編集ＩＤ／回線／優先／回線ライブラリ
*   ／集信ファイル名／起動ＪＯＢ／受信日付／受信時間／取引先）
*当日スケジュールマスタ開放
*    受信状況状態を、”受信中”に変更する。
     MOVE      1            TO         TJS-F11.
     REWRITE   TJS-REC.
*受信制御ジョブ起動
     CALL "PEOSSBMJ" USING TJS-F01 TJS-F02 TJS-F03
                           WK-PASS EOS-F13 KAI-F01
                           KAI-F02 KAI-F03 EOS-F14
                           KAI-F07 EOS-F12.
*当日スケジュールマスタ読込み
     PERFORM   JHMTJSF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"            TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     JHMTJSF  JHMKAIF  JHMEOSF.
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
 JHMTJSF-READ-SEC      SECTION.
     MOVE     "JHMTJSF-READ-SEC"   TO   S-NAME.
*当日スケジュールマスタ読込み
     READ      JHMTJSF    AT   END
               MOVE     "END"      TO   END-FLG
               GO                  TO   JHMTJSF-READ-EXIT
     END-READ.
*スケジュール日付チェック
     IF        WK-DATE   NOT =   TJS-F01
               MOVE     "END"      TO   END-FLG
               GO                  TO   JHMTJSF-READ-EXIT
     END-IF.
*実行済確認
     IF        TJS-F10   =   1
               GO                  TO   JHMTJSF-READ-SEC
     END-IF.
*
 JHMTJSF-READ-EXIT.
     EXIT.
****************************************************************
*             ＥＯＳ管理マスタ読込み                 2.2       *
****************************************************************
 JHMEOSF-READ-SEC      SECTION.
     MOVE     "JHMEOSF-READ-SEC"   TO   S-NAME.
*ＥＯＳ管理マスタ読込み
     MOVE      "01"                TO   EOS-F01.
     MOVE      "1"                 TO   EOS-F02.
     MOVE      TJS-F03             TO   EOS-F03.
     READ      JHMEOSF    INVALID
               MOVE     "INV"      TO   EOS-INV-FLG
               NOT  INVALID
               MOVE     SPACE      TO   EOS-INV-FLG
     END-READ.
*
 JHMEOSF-READ-EXIT.
     EXIT.
****************************************************************
*             回線管理マスタ読込み                   2.3       *
****************************************************************
 JHMKAIF-READ-SEC      SECTION.
     MOVE     "JHMKAIF-READ-SEC"   TO   S-NAME.
*回線管理マスタスタート
     MOVE      SPACE               TO   GO-FLG.
     MOVE      EOS-F04             TO   KAI-F01.
     MOVE      ZERO                TO   KAI-F02.
     START     JHMKAIF   KEY  IS   >=   KAI-F01 KAI-F02
               INVALID
               DISPLAY "***ｶｲｾﾝﾏｽﾀ  START ERR1***"  UPON CONS
               DISPLAY "***ｶｲｾﾝﾒｲｼｮｳ = " KAI-F01   UPON CONS
               MOVE    "END"             TO     END-FLG
               MOVE    "4000"            TO     PROGRAM-STATUS
               GO                        TO     JHMKAIF-READ-EXIT
     END-START.
*スタート後初期読込み
     READ      JHMKAIF   AT  END
               DISPLAY "***ｶｲｾﾝﾏｽﾀ RD-INV ERR2***"  UPON CONS
               DISPLAY "***ｶｲｾﾝﾒｲｼｮｳ = " KAI-F01   UPON CONS
               MOVE    "END"             TO     END-FLG
               MOVE    "4000"            TO     PROGRAM-STATUS
               GO                        TO     JHMKAIF-READ-EXIT
               NOT  AT  END
               IF       EOS-F04    NOT =    KAI-F01
                    DISPLAY "***ｶｲｾﾝﾏｽﾀ RD-INV ERR3***"  UPON CONS
                    DISPLAY "***ｶｲｾﾝﾒｲｼｮｳ = " KAI-F01   UPON CONS
                    MOVE    "END"    TO     END-FLG
                    MOVE    "4000"   TO     PROGRAM-STATUS
                    REWRITE  KAI-REC
                    GO               TO     JHMKAIF-READ-EXIT
               END-IF
     END-READ.
*回線状況チェック
 KAISEN010.
     IF        KAI-F05  =  ZERO
               MOVE     "OK"             TO     GO-FLG
               MOVE      1               TO     KAI-F05
               REWRITE   KAI-REC
     ELSE
               READ      JHMKAIF   AT  END
                         MOVE    "NG"    TO     GO-FLG
                         NOT  AT  END
                         IF   KAI-F01  NOT =  EOS-F04
                              MOVE  "NG" TO     GO-FLG
                              REWRITE    KAI-REC
                         ELSE
                              GO         TO     KAISEN010
                         END-IF
               END-READ
     END-IF.
*
 JHMKAIF-READ-EXIT.
     EXIT.
*****************<<  SCV0070B   END PROGRAM  >>******************

```
