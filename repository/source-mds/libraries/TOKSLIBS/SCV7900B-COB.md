# SCV7900B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SCV7900B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　自動受信時間監視／受信処理実行    *
*    作成日／更新日　　　：　07/05/22                          *
*    作成者／更新者　　　：　ＮＡＶ松野                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                            （ダイキ用）                      *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SCV7900B.
 AUTHOR.               MATUSNO.
 DATE-WRITTEN.         07/05/22.
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
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TJS-F01
                                           TJS-F02
                                           TJS-F03
                       FILE      STATUS    TJS-ST.
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
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TJS-ST                   PIC  X(02).
*区分、ＦＬＧエリア
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
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
*取引先ＣＤ
 01  WK-TORIHIKICD.
     03  WK-TORICD                PIC  9(08)  VALUE  100403.
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
**************************************************************
 LINKAGE               SECTION.
 01  LINK-HIDUKE           PIC 9(08).
 01  LINK-JIKAN            PIC 9(04).
 01  LINK-TOKCD            PIC 9(08).
 01  LINK-LINE             PIC X(01).
 01  LINK-YUSEN            PIC 9(01).
 01  LINK-LIBNM            PIC X(08).
 01  LINK-FILNM            PIC X(08).
 01  LINK-JKEKA            PIC 9(02).
**************************************************************
**************************************************************
 PROCEDURE             DIVISION
                       USING  LINK-HIDUKE
                              LINK-JIKAN
                              LINK-TOKCD
                              LINK-LINE
                              LINK-YUSEN
                              LINK-LIBNM
                              LINK-FILNM
                              LINK-JKEKA.
**************************************************************
 DECLARATIVES.
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
     DISPLAY "***ｼｭﾄﾞｳｼﾞｭｼﾝ ｶｲｼ -> " WK-YYYY "/" WK-MM "/" WK-DD
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
     MOVE      LINK-JIKAN    TO        WK-TIME-1.
     DISPLAY "ｼﾞｭｼﾝSTART = " WK-TIME-1(1:2) ":"
                             WK-TIME-1(3:2)  UPON CONS.
*受信処理起動
*    受信処理投入
*  （受信パスワード／データ編集ＩＤ／回線／優先／回線ライブラリ
*   ／集信ファイル名／起動ＪＯＢ／受信日付／受信時間／取引先）
*当日スケジュールマスタ作成
 TOJITU.
     OPEN      I-O    JHMTJSF.
     MOVE      SPACE        TO      TJS-REC.
     INITIALIZE                     TJS-REC.
     MOVE      WK-DATE      TO      TJS-F01.
     MOVE      WK-TIME-1    TO      TJS-F02.
     MOVE      WK-TORICD    TO      TJS-F03.
 TOJITU1.
     WRITE     TJS-REC.
     IF        TJS-ST  =  "22"
              DISPLAY NC"＃＃再度画面より受信処理＃＃" UPON CONS
              DISPLAY NC"＃＃を行なって下さい。　＃＃" UPON CONS
              STOP  RUN
     ELSE
              IF  TJS-ST  NOT =  "00"
                  MOVE    TJS-ST    TO    E-ST
                  MOVE    "JHMTJSF" TO    E-FILE
                  DISPLAY SEC-NAME  UPON  CONS
                  DISPLAY ERR-FILE  UPON  CONS
                  DISPLAY ERR-NAME  UPON  CONS
                  DISPLAY TJS-ERR   UPON  CONS
                  MOVE    "4000"    TO    PROGRAM-STATUS
                  STOP    RUN
              END-IF
     END-IF.
     CLOSE     JHMTJSF.
*受信プログラム起動
*****MOVE      WK-DATE      TO     LINK-HIDUKE.
*****MOVE      WK-TIME-1    TO     LINK-JIKAN.
     MOVE      WK-TORICD    TO     LINK-TOKCD.
     MOVE      "I"          TO     LINK-LINE.
     MOVE       1           TO     LINK-YUSEN.
     MOVE      "ONLBLIB "   TO     LINK-LIBNM.
     MOVE      "DAIKI1  "   TO     LINK-FILNM.
     MOVE      ZERO         TO     LINK-JKEKA.
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
*実行管理ファイルクリア
     CALL      "PCV00310".
*ＰＧスタートメッセージ
     ACCEPT    WK-TIME  FROM     TIME.
     DISPLAY "***ｼｭﾄﾞｳｼﾞｭｼﾝ ｵﾜﾘ -> " WK-YYYY "/" WK-MM "/" WK-DD
         " " WK-TIME-1(1:2) ":" WK-TIME-1(3:2) "***" UPON CONS.
*
 END-EXIT.
     EXIT.
*****************<<  SCV7900B   END PROGRAM  >>******************

```
