# SNJ0600B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNJ0600B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　手動配信時間監視／配信処理実行    *
*    作成日／更新日　　　：　10/08/24                          *
*    作成者／更新者　　　：　ＮＡＶ阿部                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNJ0600B.
 AUTHOR.               ABE.
 DATE-WRITTEN.         10/08/24.
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
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TJS-F01
                                           TJS-F02
                                           TJS-F03
                       FILE      STATUS    TJS-ST.
*回線種別マスタ
     SELECT  JSMKAIF   ASSIGN    TO        DA-01-VI-JSMKAIL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KAI-F01
                                           KAI-F02
                       FILE      STATUS    KAI-ST.
*回線種別マスタ（固定端末回線）
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
*    FILE = 回線種別マスタ                                     *
****************************************************************
 FD  JSMKAIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMKAIF   OF   XFDLIB
                       JOINING   KAI       AS   PREFIX.
****************************************************************
*    FILE = 回線種別マスタ（固定端末）                         *
****************************************************************
 FD  JSMKAIF2
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMKAIF   OF   XFDLIB
                       JOINING   KAI2      AS   PREFIX.
****************************************************************
*    FILE = ＥＯＳ管理マスタ                                   *
****************************************************************
 FD  JSMEDIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMEDIF   OF   XFDLIB
                       JOINING   EDI       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*
*  FILE = 回線種別マスタワーク領域
   COPY      JSMKAIF   OF   XFDLIB
             JOINING   KAI3      AS   PREFIX.
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
*配信パスワード編集エリア
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
*通信モード
 01  WK-LINK-JYUKBN               PIC  X(01)  VALUE  SPACE.
*
**************************************************************
 LINKAGE               SECTION.
 01  LINK-DTKBN        PIC  X(02).
 01  LINK-TOKCD        PIC  9(08).
 01  LINK-JYUKBN       PIC  9(01).
**************************************************************
 PROCEDURE             DIVISION USING LINK-DTKBN
                                      LINK-TOKCD
                                      LINK-JYUKBN.
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
*
*    DISPLAY  "DTKBN  = "  LINK-DTKBN   UPON CONS.
*    DISPLAY  "TOKCD  = "  LINK-TOKCD   UPON CONS.
*    DISPLAY  "JYUKBN = "  LINK-JYUKBN  UPON CONS.
*ファイルのＯＰＥＮ
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
     DISPLAY "*** ｼｭﾄﾞｳﾊｲｼﾝ ｶｲｼ-> " WK-YYYY "/" WK-MM "/" WK-DD
         " " WK-TIME-1(1:2) ":" WK-TIME-1(3:2) "***" UPON CONS.
*伝票更新処理起動確認
 INIT020.
     CALL     "SCV0020B"   USING   LINK-CHK.
*    ﾁｪｯｸ結果判定
     IF        LINK-CHK  =  "0"
               DISPLAY NC"伝票更新開始" UPON CONS
               CALL "DENSTART"
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
     DISPLAY "ﾊｲｼﾝSTART = " WK-TIME-1(1:2) ":"
                             WK-TIME-1(3:2)  UPON CONS.
*ＥＯＳ管理マスタ（配信情報取得）
     PERFORM   JSMEDIF-READ-SEC.
*ＥＤＩ管理マスタ存在チェック
     IF        EDI-INV-FLG  =  "INV"
               DISPLAY "***EDIｶﾝﾘﾏｽﾀ ｼｭﾄｸ ERR***"  UPON CONS
               DISPLAY "***ﾄﾘﾋｷｻｷCD  = " TJS-F03   UPON CONS
               DISPLAY "***ﾊｲｼﾝTIME = " TJS-F02   UPON CONS
               MOVE    "END"             TO     END-FLG
               MOVE    "4000"            TO     PROGRAM-STATUS
               GO                        TO     MAIN-EXIT
     END-IF.
*回線取得
 MAIN020.
     IF  EDI-F06  =  SPACE
         PERFORM   JSMKAIF-READ-SEC
         IF        END-FLG  =  "END"
                   GO           TO         MAIN-EXIT
         END-IF
     ELSE
         PERFORM   JSMKAIF2-READ-SEC
         IF        END-FLG  =  "END"
                   GO           TO         MAIN-EXIT
         END-IF
     END-IF.
     IF        END-FLG  =  "END"
               GO           TO         MAIN-EXIT
     END-IF.
*回線使用状況チェック
     IF        GO-FLG   =  "NG"
               MOVE   "END" TO         END-FLG
               DISPLAY "***ｶｲｾﾝｼﾖｳﾁｭｳ1        ]]]***" UPON CONS
               DISPLAY "***ｻｲﾄﾞ ｼﾞｯｺｳｼﾃ ｸﾀﾞｻｲ ]]]***" UPON CONS
               GO           TO         MAIN-EXIT
     END-IF.
*配信処理起動
*    配信処理投入
*  （配信日／配信時間／データ区分／配信配信区分／取引先ＣＤ
*   ／回線種別／回線制御番号／端末名／パラメータファイル名）
*当日スケジュールマスタ作成
 TOJITU.
     OPEN      I-O    JSMDAYF.
     MOVE      SPACE        TO      TJS-REC.
     INITIALIZE                     TJS-REC.
     MOVE      WK-DATE      TO      TJS-F01.
     MOVE      WK-TIME-1    TO      TJS-F02.
     MOVE      LINK-TOKCD   TO      TJS-F03.
     MOVE      LINK-DTKBN   TO      TJS-F04.
*通信モード判定
     IF  LINK-JYUKBN = "2"
         MOVE  LINK-JYUKBN  TO      TJS-F16
                                    WK-LINK-JYUKBN
     ELSE
         MOVE  "4"          TO      TJS-F16
                                    WK-LINK-JYUKBN
     END-IF.
 TOJITU1.
     WRITE     TJS-REC.
     IF        TJS-ST  =  "22"
              DISPLAY NC"＃＃再度画面より配信処理＃＃" UPON CONS
              DISPLAY NC"＃＃を行なって下さい。　＃＃" UPON CONS
              STOP  RUN
     ELSE
              IF  TJS-ST  NOT =  "00"
                  MOVE    TJS-ST    TO    E-ST
                  MOVE    "JSMDAYF" TO    E-FILE
                  DISPLAY SEC-NAME  UPON  CONS
                  DISPLAY ERR-FILE  UPON  CONS
                  DISPLAY ERR-NAME  UPON  CONS
                  DISPLAY TJS-ERR   UPON  CONS
                  MOVE    "4000"    TO    PROGRAM-STATUS
                  STOP    RUN
              END-IF
     END-IF.
     CLOSE     JSMDAYF.
*配信プログラム起動
     DISPLAY  "KAI3-F01 = " KAI3-F01  UPON CONS.
     DISPLAY  "KAI3-F02 = " KAI3-F02  UPON CONS.
     DISPLAY  "KAI3-F03 = " KAI3-F03  UPON CONS.
     DISPLAY  "KAI3-F06 = " KAI3-F06  UPON CONS.
*
     CALL      "CEDISBMJ"   USING   WK-DATE
                                    WK-TIME-1
                                    EDI-F01
*********************************** LINK-JYUKBN
                                    EDI-F02
                                    LINK-TOKCD
                                    KAI3-F01
                                    KAI3-F02
                                    KAI3-F03
                                    KAI3-F06
                                    WK-LINK-JYUKBN.
*処理終了定義
     MOVE     "END"         TO      END-FLG.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"            TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     JSMKAIF  JSMEDIF.
     CLOSE     JSMKAIF2.
*実行管理ファイルクリア
     CALL      "PCV00310".
*ＰＧスタートメッセージ
     ACCEPT    WK-TIME  FROM     TIME.
     DISPLAY "***ｼｭﾄﾞｳﾊｲｼﾝ END -> " WK-YYYY "/" WK-MM "/" WK-DD
         " " WK-TIME-1(1:2) ":" WK-TIME-1(3:2) "***" UPON CONS.
*
 END-EXIT.
     EXIT.
****************************************************************
*             ＥＤＩ管理マスタ読込み                 2.2       *
****************************************************************
 JSMEDIF-READ-SEC      SECTION.
     MOVE     "JSMEDIF-READ-SEC"   TO   S-NAME.
*ＥＯＳ管理マスタ読込み
     MOVE      LINK-DTKBN          TO   EDI-F01.
     MOVE      "2"                 TO   EDI-F02.
     MOVE      LINK-TOKCD          TO   EDI-F03.
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
*２重起動チェック
*回線管理マスタスタート
     MOVE      SPACE               TO   GO-FLG.
     MOVE      EDI-F04             TO   KAI-F01.
     MOVE      ZERO                TO   KAI-F02.
     START     JSMKAIF   KEY  IS   >=   KAI-F01 KAI-F02
               INVALID
               DISPLAY "***ｶｲｾﾝﾏｽﾀ  START ERR***"  UPON CONS
               DISPLAY "***ｶｲｾﾝﾒｲｼｮｳ = " KAI-F01   UPON CONS
               MOVE    "END"             TO     END-FLG
               MOVE    "4000"            TO     PROGRAM-STATUS
               GO                        TO     JSMKAIF-READ-EXIT
     END-START.
*スタート後初期読込み
     READ      JSMKAIF   AT  END
               DISPLAY "***ｶｲｾﾝﾏｽﾀ RD-INV ERR***"  UPON CONS
               DISPLAY "***ｶｲｾﾝﾒｲｼｮｳ = " KAI-F01   UPON CONS
               MOVE    "END"             TO     END-FLG
               MOVE    "4000"            TO     PROGRAM-STATUS
               GO                        TO     JSMKAIF-READ-EXIT
               NOT  AT  END
               IF       EDI-F04    NOT =    KAI-F01
                    DISPLAY "***ｶｲｾﾝﾏｽﾀ RD-INV ERR***"  UPON CONS
                    DISPLAY "***ｶｲｾﾝﾒｲｼｮｳ = " KAI-F01   UPON CONS
                    MOVE    "END"    TO     END-FLG
                    MOVE    "4000"   TO     PROGRAM-STATUS
                    REWRITE  KAI-REC
                    GO               TO     JSMKAIF-READ-EXIT
               END-IF
     END-READ.
*回線状況チェック
 KAISEN010.
     IF        KAI-F07  =  "1"         AND
               KAI-F08  =  LINK-TOKCD
               MOVE   "END" TO         END-FLG
               DISPLAY "***ﾊｲｼﾝｼｮﾘ 2ｼﾞｭｳｷﾄﾞｳ ]]]***" UPON CONS
               GO           TO         MAIN-EXIT
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
*ＥＤＩ回線管理マスタフラグ使用フラグ、使用取引先更新
*ＥＤＩ回線管理マスタスタート
     MOVE      SPACE               TO   GO-FLG.
     MOVE      EDI-F04             TO   KAI-F01.
     MOVE      ZERO                TO   KAI-F02.
     START     JSMKAIF   KEY  IS   >=   KAI-F01 KAI-F02
               INVALID
               DISPLAY "***ｶｲｾﾝﾏｽﾀ  START ERR***"  UPON CONS
               DISPLAY "***ｶｲｾﾝﾒｲｼｮｳ = " KAI-F01   UPON CONS
               MOVE    "END"             TO     END-FLG
               MOVE    "4000"            TO     PROGRAM-STATUS
               GO                        TO     JSMKAIF-READ-EXIT
     END-START.
*スタート後初期読込み
     READ      JSMKAIF   AT  END
               DISPLAY "***ｶｲｾﾝﾏｽﾀ RD-INV ERR***"  UPON CONS
               DISPLAY "***ｶｲｾﾝﾒｲｼｮｳ = " KAI-F01   UPON CONS
               MOVE    "END"             TO     END-FLG
               MOVE    "4000"            TO     PROGRAM-STATUS
               GO                        TO     JSMKAIF-READ-EXIT
               NOT  AT  END
               IF       EDI-F04    NOT =    KAI-F01
                    DISPLAY "***ｶｲｾﾝﾏｽﾀ RD-INV ERR***"  UPON CONS
                    DISPLAY "***ｶｲｾﾝﾒｲｼｮｳ = " KAI-F01   UPON CONS
                    MOVE    "END"    TO     END-FLG
                    MOVE    "4000"   TO     PROGRAM-STATUS
                    REWRITE  KAI-REC
                    GO               TO     JSMKAIF-READ-EXIT
               END-IF
     END-READ.
*回線状況チェック
 KAISEN020.
     IF     (  KAI-F07  =  SPACE  OR
               KAI-F07  =  ZERO   )  AND
               KAI-F08  =  SPACE
               MOVE     "OK"             TO     GO-FLG
               MOVE      1               TO     KAI-F07
               MOVE      LINK-TOKCD      TO     KAI-F08
               REWRITE   KAI-REC
     ELSE
               READ      JSMKAIF   AT  END
                         MOVE    "NG"    TO     GO-FLG
                         NOT  AT  END
                         IF   KAI-F01  NOT =  EDI-F04
                              MOVE  "NG" TO     GO-FLG
                              REWRITE    KAI-REC
                         ELSE
                              GO         TO     KAISEN020
                         END-IF
               END-READ
     END-IF.
*
     MOVE      KAI-REC           TO     KAI3-REC.
*
 JSMKAIF-READ-EXIT.
     EXIT.
****************************************************************
*             回線管理マスタ読込み（固定端末）       2.4       *
****************************************************************
 JSMKAIF2-READ-SEC      SECTION.
     MOVE     "JSMKAIF2-READ-SEC"   TO   S-NAME.
*２重起動チェック
*回線管理マスタスタート
     MOVE      SPACE               TO   GO-FLG.
     MOVE      EDI-F06             TO   KAI2-F03.
     MOVE      EDI-F04             TO   KAI2-F01.
     MOVE      ZERO                TO   KAI2-F02.
     START     JSMKAIF2  KEY IS >= KAI2-F03 KAI2-F01 KAI2-F02
               INVALID
               DISPLAY "***ｶｲｾﾝﾏｽﾀ F2 START ERR***"  UPON CONS
               DISPLAY "***ｶｲｾﾝﾀﾝﾏﾂ = " KAI2-F03   UPON CONS
               MOVE    "END"         TO     END-FLG
               MOVE    "4000"        TO     PROGRAM-STATUS
               GO                    TO     JSMKAIF2-READ-EXIT
     END-START.
 KAISEN010-0.
*スタート後初期読込み
     READ      JSMKAIF2  AT  END
               DISPLAY "***ｶｲｾﾝﾏｽﾀ F2 RD-INV ERR***"  UPON CONS
               DISPLAY "***ｶｲｾﾝﾀﾝﾏﾂ = " KAI2-F03   UPON CONS
               MOVE    "END"         TO     END-FLG
               MOVE    "4000"        TO     PROGRAM-STATUS
               GO                    TO     JSMKAIF2-READ-EXIT
               NOT  AT  END
               IF       EDI-F06    NOT =    KAI2-F03
***              DISPLAY "***ｶｲｾﾝﾏｽﾀ F2 RD-INV ERR***"  UPON CONS
***              DISPLAY "***ｶｲｾﾝﾀﾝﾏﾂ  = " KAI2-F03   UPON CONS
***                 MOVE    "END"    TO     END-FLG
***                 MOVE    "4000"   TO     PROGRAM-STATUS
***                 REWRITE  KAI2-REC
****                GO               TO     JSMKAIF2-READ-EXIT
                    GO               TO     KAISEN010-0
               END-IF
     END-READ.
     DISPLAY  "EDI-F06  = " EDI-F06   UPON CONS.
     DISPLAY  "KAI2-F03 = " KAI2-F03  UPON CONS.
     DISPLAY  "KAI2-F01 = " KAI2-F01  UPON CONS.
     DISPLAY  "KAI2-F02 = " KAI2-F02  UPON CONS.
*回線状況チェック
 KAISEN010-1.
     IF        KAI2-F07  =  "1"         AND
               KAI2-F08  =  LINK-TOKCD
               MOVE   "END" TO         END-FLG
               DISPLAY "***ｼﾞｭｼﾝｼｮﾘ 2ｼﾞｭｳｷﾄﾞｳ ]]]***" UPON CONS
               GO           TO         MAIN-EXIT
**   ELSE
**             READ      JSMKAIF2   AT  END
**                       MOVE    "NG"    TO     GO-FLG
**                       NOT  AT  END
**                       IF   KAI2-F01  NOT =  EDI-F04
**                            MOVE  "NG" TO     GO-FLG
**                            REWRITE    KAI2-REC
**                       ELSE
**                            GO         TO     KAISEN010-1
**                       END-IF
**             END-READ
     END-IF.
*
*ＥＤＩ回線管理マスタフラグ使用フラグ、使用取引先更新
*ＥＤＩ回線管理マスタスタート
**   MOVE      SPACE               TO   GO-FLG.
**   MOVE      EDI-F06             TO   KAI2-F03.
**   MOVE      EDI-F04             TO   KAI2-F01.
**   MOVE      ZERO                TO   KAI2-F02.
**   START     JSMKAIF2 KEY IS  >=  KAI2-F03 KAI2-F01 KAI2-F02
**             INVALID
**             DISPLAY "***ｶｲｾﾝﾏｽﾀ F2 START ERR***"  UPON CONS
**             DISPLAY "***ｶｲｾﾝﾀﾝﾏﾂ = " KAI2-F03   UPON CONS
**             MOVE    "END"       TO     END-FLG
**             MOVE    "4000"      TO     PROGRAM-STATUS
**             GO                  TO     JSMKAIF2-READ-EXIT
**   END-START.
*スタート後初期読込み
**   READ      JSMKAIF2  AT  END
**             DISPLAY "***ｶｲｾﾝﾏｽﾀ F2 RD-INV ERR***"  UPON CONS
**             DISPLAY "***ｶｲｾﾝﾀﾝﾏﾂ = " KAI2-F03   UPON CONS
**             MOVE    "END"             TO     END-FLG
**             MOVE    "4000"            TO     PROGRAM-STATUS
**             GO                        TO     JSMKAIF-READ-EXIT
**             NOT  AT  END
**             IF       EDI-F06    NOT =    KAI2-F03
**               DISPLAY "***ｶｲｾﾝﾏｽﾀ F2 RD-INV ERR***"  UPON CONS
**               DISPLAY "***ｶｲｾﾝﾀﾝﾏﾂ = " KAI-F01   UPON CONS
**                  MOVE    "END"    TO     END-FLG
**                  MOVE    "4000"   TO     PROGRAM-STATUS
**                  REWRITE  KAI2-REC
**                  GO               TO     JSMKAIF2-READ-EXIT
**             END-IF
**   END-READ.
*回線状況チェック
 KAISEN020-1.
     IF     (  KAI2-F07  =  SPACE  OR
               KAI2-F07  =  ZERO   )  AND
               KAI2-F08  =  SPACE
               MOVE     "OK"             TO     GO-FLG
               MOVE      1               TO     KAI2-F07
               MOVE      LINK-TOKCD      TO     KAI2-F08
               REWRITE   KAI2-REC
     ELSE
               MOVE      "NG"            TO     GO-FLG
**             READ      JSMKAIF2   AT  END
**                       NOT  AT  END
**                       IF   KAI2-F03  NOT =  EDI-F06
**                            MOVE  "NG" TO     GO-FLG
**                            REWRITE    KAI2-REC
**                       ELSE
**                            GO         TO     KAISEN020-1
**                       END-IF
**             END-READ
     END-IF.
*
     MOVE      KAI2-REC           TO     KAI3-REC.
*
 JSMKAIF2-READ-EXIT.
     EXIT.
*****************<<  SNJ0600B   END PROGRAM  >>******************

```
