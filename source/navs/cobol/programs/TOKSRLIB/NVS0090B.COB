****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　Ｄ３６５自動送受信処理監視　　　　*
*    作成日／作成者　　　：　2020/02/24   ASS.II               *
*    処理内容　　　　　　：　送受信時刻にいたった　　　　　　　*
*    　　　　　　　　　　　　D365当日送受信制御レコードより　
*                            D365送受信処理を実行する。　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NVS0090B.
 AUTHOR.               ASS.II.
 DATE-WRITTEN.         20/02/24.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*D365当日送受信制御ファイル
     SELECT  D365DAYF  ASSIGN    TO        DA-01-VI-D365DAY1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       TJS-F01
                                           TJS-F02
                       FILE      STATUS    TJS-ST.
*実行制御マスタ
     SELECT  JSMJIKF   ASSIGN    TO        DA-01-VI-JSMJIKL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       JIK-F01
                       FILE      STATUS    JIK-ST.
*条件ファイル
     SELECT   JYOKEN1        ASSIGN        TO  DA-01-VI-JYOKEN1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  JYO-F01
                                               JYO-F02
                             FILE STATUS   IS  JYO-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = D365当日送受信制御ファイル                         *
****************************************************************
 FD  D365DAYF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      D365DAYF  OF   XFDLIB
                       JOINING   TJS       AS   PREFIX.
****************************************************************
*    FILE = 実行制御マスタ                                     *
****************************************************************
 FD  JSMJIKF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMJIKF   OF   XFDLIB
                       JOINING   JIK       AS   PREFIX.
*
****************************************************************
*    FILE = 条件ファイル                               *
****************************************************************
 FD  JYOKEN1.
     COPY     HJYOKEN  OF  XFDLIB
     JOINING  JYO      AS  PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TJS-ST                   PIC  X(02).
     03  JIK-ST                   PIC  X(02).
     03  JYO-ST                   PIC  X(02).
*区分、ＦＬＧエリア
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  JIK-INV-FLG              PIC  X(03)  VALUE  SPACE.
     03  JYO-INV-FLG              PIC  X(03)  VALUE  SPACE.
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
         NC"Ｄ３６５当日送受信制御エラー".
     03  JIK-ERR           PIC N(15) VALUE
         NC"実行制御マスタエラー".
     03  JYO-ERR           PIC N(15) VALUE
         NC"条件ファイルエラー".
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
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*実行区分
 01  LINK-KBN              PIC X(01).
*実行担当者
 01  LINK-TANT              PIC X(02).
**************************************************************
 PROCEDURE             DIVISION   USING   LINK-KBN  LINK-TANT.
**************************************************************
 DECLARATIVES.
 TJS-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE D365DAYF.
     MOVE        TJS-ST    TO        E-ST.
     MOVE        "D365DAYF" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TJS-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JIK-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMJIKF.
     MOVE        JIK-ST    TO        E-ST.
     MOVE        "JSMJIKF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JIK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JYOKEN1.
     MOVE        JYO-ST    TO        E-ST.
     MOVE        "JYOKEN1" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYO-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
**********************
*    MOVE    "0"  TO  LINK-KBN.
*    MOVE    "01" TO  LINK-TANT.
*
     DISPLAY "実行区分  =" LINK-KBN.
     DISPLAY "実行担当者=" LINK-TANT.
**********************
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
     OPEN      I-O     D365DAYF.
     OPEN      I-O     JSMJIKF.
     OPEN      I-O     JYOKEN1.
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
*Ｄ３６５当日送受信制御スタート
 INIT030.
     MOVE      WK-DATE      TO     TJS-F01.
     MOVE      WK-TIME-1    TO     TJS-F02.
     START     D365DAYF KEY IS  >=  TJS-F01 TJS-F02
               INVALID
               DISPLAY NC"＊＊＊＊＊＊＊＊＊＊＊" UPON CONS
               DISPLAY NC"＊本日の自動受信なし＊" UPON CONS
               DISPLAY NC"＊＊＊＊＊＊＊＊＊＊＊" UPON CONS
               MOVE    "END"      TO     END-FLG
               GO                 TO     INIT-EXIT
     END-START.
*Ｄ３６５当日送受信制御初期読込み
 INIT040.
     PERFORM   D365DAYF-READ-SEC.
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
 MAIN020.
*実行制御マスタ（受信情報取得）
     PERFORM   JSMJIKF-READ-SEC.
*実行制御マスタ存在チェック
     IF        JIK-INV-FLG  =  "INV"
               DISPLAY "***ｼﾞｯｺｳｾｲｷﾞｮﾏｽﾀ ｼｭﾄｸ ERR***"  UPON CONS
               DISPLAY "***ｸﾌﾞﾝ      = " JIK-F01   UPON CONS
               MOVE     "END"            TO   END-FLG
               MOVE    "4000"            TO     PROGRAM-STATUS
               GO                        TO     MAIN-EXIT
     END-IF.
*起動確認チェック
     IF    JIK-F03  =  ZERO
           MOVE  "OK"  TO  GO-FLG
*実行制御更新
           MOVE  "1"   TO  JIK-F03
           REWRITE   JIK-REC
     ELSE
           MOVE  "NG"  TO  GO-FLG
     END-IF.
*実行制御チェック
*****DISPLAY "GO-FLG = " GO-FLG  UPON  CONS.
     IF        GO-FLG   =  "NG"
               GO           TO         MAIN020
     END-IF.
*条件ファイル取得
     PERFORM   JYOKEN1-READ-SEC.
*実行制御マスタ存在チェック
     IF        JYO-INV-FLG  =  "INV"
               DISPLAY "***ｼﾞｮｳｹﾝﾌｧｲﾙ ｼｭﾄｸ ERR***"  UPON CONS
               DISPLAY "***F01      = " JYO-F01   UPON CONS
               DISPLAY "***F02      = " JYO-F02   UPON CONS
               MOVE     "END"            TO   END-FLG
               MOVE    "4000"            TO     PROGRAM-STATUS
               GO                        TO     MAIN-EXIT
     END-IF.
*条件ファイル更新
     ADD    1        TO  JYO-F04.
     IF     JYO-F04  >   JYO-F06
            MOVE  JYO-F05  TO  JYO-F04
     END-IF.
     REWRITE   JYO-REC.
*Ｄ３６５当日送受信制御更新
     MOVE      "KKKK"      TO         TJS-F08.
     MOVE      JYO-F04     TO         TJS-F09.
     REWRITE   TJS-REC.
*D365送受信処理起動
*************************
*     CALL "CNVSSBM2" USING TJS-F01 TJS-F02 TJS-F03 JYO-F04.
*************************
     DISPLAY "TJS-F01=" TJS-F01 UPON CONS.
     DISPLAY "TJS-F02=" TJS-F02 UPON CONS.
     DISPLAY "TJS-F03=" TJS-F03 UPON CONS.
     DISPLAY "JYO-F04=" JYO-F04 UPON CONS.
*************************
*Ｄ３６５当日送受信制御読込み
     PERFORM   D365DAYF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"            TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     D365DAYF  JSMJIKF  JYOKEN1.
*ＰＧスタートメッセージ
     ACCEPT    WK-TIME  FROM     TIME.
     DISPLAY "***ｼﾞﾄﾞｳｼﾞｭｼﾝ ｵﾜﾘ -> " WK-YYYY "/" WK-MM "/" WK-DD
         " " WK-TIME-1(1:2) ":" WK-TIME-1(3:2) "***" UPON CONS.
*
 END-EXIT.
     EXIT.
****************************************************************
*             Ｄ３６５当日送受信制御読込み           2.1       *
****************************************************************
 D365DAYF-READ-SEC     SECTION.
     MOVE     "JD365DAYF-READ-SEC" TO   S-NAME.
*当日送受信制御読込み
     READ      D365DAYF  AT   END
               MOVE     "END"      TO   END-FLG
               GO                  TO   D365DAYF-READ-EXIT
     END-READ.
*スケジュール日付チェック
     IF        WK-DATE   NOT =   TJS-F01
               MOVE     "END"      TO   END-FLG
               GO                  TO   D365DAYF-READ-EXIT
     END-IF.
*
 D365DAYF-READ-EXIT.
     EXIT.
****************************************************************
*             実行制御マスタ 読込み                 2.2
****************************************************************
 JSMJIKF-READ-SEC      SECTION.
     MOVE     "JSMJIKF-READ-SEC"   TO   S-NAME.
*実行制御マスタ読込み
     MOVE      "N"                 TO   JIK-F01.
     READ      JSMJIKF    INVALID
               MOVE     "INV"      TO   JIK-INV-FLG
               NOT  INVALID
               MOVE     SPACE      TO   JIK-INV-FLG
     END-READ.
*
 JSMJIKF-READ-EXIT.
     EXIT.
****************************************************************
*             条件マスタ読込み                        2.3
****************************************************************
 JYOKEN1-READ-SEC      SECTION.
     MOVE     "JYOKEN1-READ-SEC"   TO   S-NAME.
*条件マスタ読込み
     MOVE      "99"                TO   JYO-F01.
     MOVE      "D365"              TO   JYO-F02.
     READ      JYOKEN1    INVALID
               MOVE     "INV"      TO   JYO-INV-FLG
               NOT  INVALID
               MOVE     SPACE      TO   JYO-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
     EXIT.
*****************<<  NVS0090B   END PROGRAM  >>******************
