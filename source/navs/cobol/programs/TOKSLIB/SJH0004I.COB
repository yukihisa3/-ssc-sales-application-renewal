****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＥＯＳ管理　　　　　　　　        *
*    モジュール名　　　　：　基本スケジュールマスタ保守       *
*    作成日／更新日　　　：　99/09/09                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJH0004I.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         99/09/09.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*画面ファイル
*基本スケジュールマスタ
     SELECT  JHMKIHF   ASSIGN    TO        DA-01-VI-JHMKIHL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       KIH-F01
                                           KIH-F02
                       FILE      STATUS    KIH-ST.
*ＥＯＳ管理マスタ
     SELECT  JHMEOSF   ASSIGN    TO        DA-01-VI-JHMEOSL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       EOS-F01
                                           EOS-F02
                                           EOS-F03
                       FILE      STATUS    EOS-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*画面定義ファイル
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 基本スケジュールマスタ                             *
****************************************************************
 FD  JHMKIHF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMKIHF   OF   XFDLIB
                       JOINING   KIH       AS   PREFIX.
****************************************************************
*    FILE = ＥＯＳ管理マスタ                                   *
****************************************************************
 FD  JHMEOSF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMEOSF   OF   XFDLIB
                       JOINING   EOS       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FJH00041  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KIH-ST                   PIC  X(02).
     03  EOS-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  JHMEOSF-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  RD-FLG                   PIC  X(03)  VALUE  SPACE.
     03  GYO-CNT                  PIC  9(02)  VALUE  ZERO.
     03  CHK-CNT                  PIC  9(01)  VALUE  ZERO.
     03  CHK-MEI                  PIC  9(02)  VALUE  ZERO.
     03  P-CNT                    PIC  9(01)  VALUE  ZERO.
     03  S-CNT                    PIC  9(01)  VALUE  ZERO.
     03  C-CNT                    PIC  9(01)  VALUE  ZERO.
     03  X                        PIC  9(02)  VALUE  ZERO.
     03  Y                        PIC  9(02)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了　　　　　　　　　　　".
     03  PF-MSG2.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了_項目戻し_前頁_次頁".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(20).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"曜日を入力して下さい。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"時間を入力して下さい。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先コードを入力して下さい。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"前頁はありません。　　　".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"次頁はありません。　　　".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"受信時間論理エラー。　　".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"分は、３０分単位で入力して下さい。".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"明細を入力して下さい。　　　　　　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  9   PIC   N(20).
*基本スケジュール退避エリア
 01  TABLE-AREA.
     03  TBL-YOUBI                PIC   9(01).
     03  TABLE1      OCCURS  6.
         05  TABLE2  OCCURS  15.
             07  TBL-GYO          PIC   9(02).
             07  TBL-JIKAN        PIC   9(04).
             07  TBL-TOKCD        PIC   9(08).
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  KIH-ERR           PIC N(15) VALUE
                        NC"基本スケジュールエラー".
     03  EOS-ERR           PIC N(15) VALUE
                        NC"ＥＯＳ管理マスタエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  DSP-ERR           PIC N(15) VALUE
                        NC"画面ファイルエラー".
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
 KIH-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMKIHF.
     MOVE        KIH-ST    TO        E-ST.
     MOVE        "JHMKIHF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KIH-ERR   UPON      CONS.
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
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     MOVE        TOK-ST    TO        E-ST.
     MOVE        "HTOKMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE        "DSPFILE" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DSP-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
***
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
***
     STOP    RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
     ACCEPT    WK-TIME          FROM   TIME.
*ファイルのＯＰＥＮ
     OPEN      I-O              JHMKIHF.
     OPEN      I-O              DSPFILE.
     OPEN      INPUT            JHMEOSF HTOKMS.
*ワークの初期化
     INITIALIZE                 FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
     PERFORM  INIT-DSP-SEC.
*ヘッド入力へ
     MOVE    "1"                  TO   PSW.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     EVALUATE      PSW
*処理区分入力(2.1)
         WHEN      "1"       PERFORM   DSP-HEAD-SEC
*明細入力    (2.2)
         WHEN      "2"       PERFORM   DSP-BODY-SEC
*確認入力    (2.3)
         WHEN      "3"       PERFORM   DSP-KAKU-SEC
*以外
         WHEN      OTHER     CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"          TO   S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE             DSPFILE   JHMKIHF
                       JHMEOSF   HTOKMS.
**
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*画面の初期化
     MOVE    SPACE                TO   DSP-FJH00041.
*システム日付転送
     MOVE    SYS-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:6)         TO   DSP-STIME.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
*
 INIT-DSP-EXIT.
     EXIT.
****************************************************************
*             処理区分入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-HEAD-SEC          SECTION.
     MOVE     "DSP-HEAD-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD-CHK-SEC
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
         WHEN   OTHER
                MOVE     4       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*             曜日チェック                           2.1.1     *
****************************************************************
 HEAD-CHK-SEC             SECTION.
     MOVE     "HEAD-CHK-SEC"     TO   S-NAME.
*曜日チェック（未入力／範囲外）
     IF       DSP-YOUBI      NOT NUMERIC
     OR       DSP-YOUBI      =   ZERO
              MOVE   1       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-YOUBI
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-YOUBI
     ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-YOUBI
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-YOUBI
     END-IF.
*処理区分＝１，２，３以外はエラー
     IF  ( DSP-YOUBI    NUMERIC )
     AND ( DSP-YOUBI    =   1 OR 2 OR 3 OR 4 OR 5 )
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-YOUBI
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-YOUBI
     ELSE
              MOVE   1       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-YOUBI
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-YOUBI
     END-IF.
*エラーフラグがゼロの場合
     IF       ERR-FLG = ZERO
              PERFORM MST-WORK-SEC
              MOVE    1      TO  P-CNT
              PERFORM WORK-DSP-SEC
              MOVE   "2"     TO  PSW
******DISPLAY "TBL-GYO(1 1)   = " TBL-GYO(1 1)    UPON CONS
******DISPLAY "TBL-JIKAN(1 1) = " TBL-JIKAN(1 1)  UPON CONS
******DISPLAY "TBL-GYO(1 2)   = " TBL-GYO(1 2)    UPON CONS
******DISPLAY "TBL-JIKAN(1 2) = " TBL-JIKAN(1 2)  UPON CONS
******DISPLAY "TBL-GYO(1 3)   = " TBL-GYO(1 3)    UPON CONS
******DISPLAY "TBL-JIKAN(1 3) = " TBL-JIKAN(1 3)  UPON CONS
     END-IF.
*
 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*             明細項目　入力( PSW = 2 )              2.2       *
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE     "DSP-BODY-SEC"       TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                MOVE    ZERO      TO   ERR-FLG CHK-MEI
                PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 15
                                OR (  DSP-JIKAN(Y) NOT NUMERIC
                                  AND DSP-TOKCD(Y) NOT NUMERIC )
****************DISPLAY "DSP-JIKAN = " DSP-JIKAN(Y) UPON CONS
****************DISPLAY "Y         = " Y            UPON CONS
                        IF  ( DSP-JIKAN(Y) NOT = ZERO
                        AND   DSP-JIKAN(Y)       NUMERIC )
                        OR  ( DSP-TOKCD(Y) NOT = ZERO
                        AND   DSP-TOKCD(Y)       NUMERIC )
                            PERFORM   BODY-CHK-SEC
                            ADD  1    TO   CHK-MEI
                        END-IF
                END-PERFORM
                IF   ERR-FLG  =  ZERO
                     IF   ( CHK-MEI = ZERO ) AND ( P-CNT = 1 )
                          MOVE  9   TO     ERR-FLG
                     ELSE
                          MOVE "3"  TO     PSW
                     END-IF
                END-IF
*取消
         WHEN   "F004"
                MOVE     1       TO   P-CNT
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*前頁
         WHEN   "F011"
                COMPUTE C-CNT = P-CNT - 1
                IF      C-CNT = ZERO
                        MOVE  5  TO  ERR-FLG
                ELSE
                        PERFORM DSP-WORK-SEC
                        COMPUTE P-CNT = P-CNT - 1
                        PERFORM INIT-DSP-SEC
                        PERFORM WORK-DSP-SEC
                END-IF
*次頁
         WHEN   "F012"
**********DISPLAY "P-CNT1 = " P-CNT UPON CONS
                MOVE    P-CNT   TO      S-CNT
                COMPUTE C-CNT = P-CNT + 1
                IF      C-CNT > 6
                        MOVE   6   TO   ERR-FLG
                ELSE
******************IF  CHK-CNT = ZERO
                  IF   ( DSP-JIKAN(15)  =  ZERO  OR
                         DSP-JIKAN(15)  NOT  NUMERIC )
                  AND (  DSP-TOKCD(15)  =  ZERO  OR
                         DSP-TOKCD(15)  NOT  NUMERIC )
                        MOVE   6   TO   ERR-FLG
                  ELSE
                        PERFORM DSP-WORK-SEC
                        COMPUTE P-CNT = P-CNT + 1
                        PERFORM INIT-DSP-SEC
                        PERFORM WORK-DSP-SEC
                  END-IF
                END-IF
**********DISPLAY "P-CNT2 = " P-CNT UPON CONS
         WHEN   OTHER
                MOVE     4       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック                       2.2.1     *
****************************************************************
 BODY-CHK-SEC          SECTION.
     MOVE     "BODY-CHK-SEC"      TO   S-NAME.
*
*時間
     IF       DSP-JIKAN(Y)  NUMERIC
     AND      DSP-JIKAN(Y)  NOT =  ZERO
**************PERFORM  DABURI-CHK-SEC
              MOVE   DSP-JIKAN(Y)  TO  WK-JIKAN
              IF     WK-HH  <=  ZERO  OR  WK-HH  >  24
              OR     WK-MM  <   ZERO  OR  WK-MM  >  59
                     IF     ERR-FLG  =  ZERO
                            MOVE    7    TO    ERR-FLG
                     END-IF
                     MOVE "R"    TO EDIT-OPTION OF DSP-JIKAN(Y)
                     MOVE "C"    TO EDIT-CURSOR OF DSP-JIKAN(Y)
              ELSE
*******           IF WK-MM  =  ZERO  OR  WK-MM  =  30
                     MOVE "M"    TO EDIT-OPTION OF DSP-JIKAN(Y)
                     MOVE  SPACE TO EDIT-CURSOR OF DSP-JIKAN(Y)
*******           ELSE
*******              IF     ERR-FLG  =  ZERO
*******                     MOVE    8    TO    ERR-FLG
*******              END-IF
*******              MOVE "R"    TO EDIT-OPTION OF DSP-JIKAN(Y)
*******              MOVE "C"    TO EDIT-CURSOR OF DSP-JIKAN(Y)
*******           END-IF
              END-IF
     ELSE
          IF  DSP-JIKAN(Y) NOT = ZERO
              IF  ERR-FLG  =  ZERO
                  MOVE   2    TO   ERR-FLG
              END-IF
              MOVE  "R"      TO  EDIT-OPTION   OF  DSP-JIKAN(Y)
              MOVE  "C"      TO  EDIT-CURSOR   OF  DSP-JIKAN(Y)
          END-IF
     END-IF.
*
*取引先コード
     IF       DSP-TOKCD(Y)   NOT  NUMERIC
     OR       DSP-TOKCD(Y)   =   ZERO
              IF  ERR-FLG  =  ZERO
                  MOVE   3    TO   ERR-FLG
              END-IF
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-TOKCD(Y)
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-TOKCD(Y)
     ELSE
              MOVE     DSP-TOKCD(Y)     TO TOK-F01
              PERFORM  HTOKMS-READ-SEC
              IF       HTOKMS-INV-FLG = SPACE
                       MOVE  TOK-F03    TO DSP-TOKNM(Y)
                       MOVE "M"   TO EDIT-OPTION OF DSP-TOKCD(Y)
                       MOVE SPACE TO EDIT-CURSOR OF DSP-TOKCD(Y)
              ELSE
                       MOVE  ALL NC"＊" TO DSP-TOKNM(Y)
                       IF  ERR-FLG  =  ZERO
                           MOVE   3    TO   ERR-FLG
                       END-IF
                       MOVE "R"   TO EDIT-OPTION OF DSP-TOKCD(Y)
                       MOVE "C"   TO EDIT-CURSOR OF DSP-TOKCD(Y)
              END-IF
     END-IF.
*
*得意先コードから回線種別の取得
     IF      ERR-FLG  =  ZERO
             MOVE    "01"           TO    EOS-F01
             MOVE      1            TO    EOS-F02
             MOVE     DSP-TOKCD(Y)  TO    EOS-F03
             PERFORM JHMEOSF-READ-SEC
*******DISPLAY "INV-FLG = " JHMEOSF-INV-FLG UPON CONS
             IF      JHMEOSF-INV-FLG  =  SPACE
                     MOVE    EOS-F04       TO DSP-SEN(Y)
                     EVALUATE EOS-F04
                         WHEN "I"
                         MOVE NC"ＩＳＤＮ" TO DSP-SENNM(Y)
                         WHEN "T"
                         MOVE NC"公衆回線" TO DSP-SENNM(Y)
                         WHEN "P"
                         MOVE NC"ＰＣ全銀" TO DSP-SENNM(Y)
                     END-EVALUATE
             END-IF
     END-IF.
*
 BODY-CHK-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 4 ）            2.4
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE     "DSP-KAKU-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   DSP-WORK-SEC
                PERFORM   JHMKIHF-DEL-SEC
                PERFORM   JHMKIHF-WRITE-SEC
                PERFORM   INIT-DSP-SEC
                MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                MOVE     1       TO   P-CNT
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "2"      TO   PSW
*前頁
         WHEN   "F011"
                COMPUTE C-CNT = P-CNT - 1
                IF      C-CNT = ZERO
                        MOVE  5  TO  ERR-FLG
                ELSE
                        PERFORM DSP-WORK-SEC
                        COMPUTE P-CNT = P-CNT - 1
                        PERFORM INIT-DSP-SEC
                        PERFORM WORK-DSP-SEC
                        MOVE "2" TO  PSW
                END-IF
*次頁
         WHEN   "F012"
**********DISPLAY "P-CNT3 = " P-CNT UPON CONS
                MOVE    P-CNT   TO      S-CNT
                COMPUTE C-CNT = P-CNT + 1
                IF      C-CNT > 6
                        MOVE   6   TO   ERR-FLG
                ELSE
******************IF  CHK-CNT = ZERO
                  IF   ( DSP-JIKAN(15)  =  ZERO  OR
                         DSP-JIKAN(15)  NOT  NUMERIC )
                  AND  ( DSP-TOKCD(15)  =  ZERO  OR
                         DSP-TOKCD(15)  NOT  NUMERIC )
                      MOVE   6     TO    ERR-FLG
                  ELSE
                      PERFORM DSP-WORK-SEC
                      COMPUTE P-CNT = P-CNT + 1
                      PERFORM INIT-DSP-SEC
                      PERFORM WORK-DSP-SEC
                      MOVE  "2"    TO    PSW
                  END-IF
                END-IF
**********DISPLAY "P-CNT4 = " P-CNT UPON CONS
         WHEN   OTHER
                MOVE     4       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.
*エラーメッセージセット
     IF    ERR-FLG   =    ZERO
           MOVE    SPACE              TO   DSP-ERRMSG
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-ERRMSG
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
*曜日
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-PFGAID
*明細／確認
         WHEN   "2"   WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FJH00041"          TO   DSP-FMT.
     WRITE    DSP-FJH00041.
     PERFORM  DSP-SYOKI-SEC.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面読込処理                                     *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE     "DSP-READ-SEC"      TO   S-NAME.
*
     MOVE    "NE"                 TO   DSP-PRO.
*
*    MOVE    "SCREEN"             TO   DSP-GRP.
     EVALUATE   PSW
*曜日
         WHEN   "1"
                MOVE    "HEAD"    TO   DSP-GRP
*明細
         WHEN   "2"
                MOVE    "BODY"    TO   DSP-GRP
*確認
         WHEN   "3"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FJH00041"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "DSP-SYOKI-SEC"    TO   S-NAME.
*リバース，カーソルパーク解除
***  曜日
     MOVE "M"   TO EDIT-OPTION OF DSP-YOUBI.
     MOVE SPACE TO EDIT-CURSOR OF DSP-YOUBI.
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 15
***  時間
             MOVE "M"   TO EDIT-OPTION OF DSP-JIKAN(X)
             MOVE SPACE TO EDIT-CURSOR OF DSP-JIKAN(X)
***  取引先
             MOVE "M"   TO EDIT-OPTION OF DSP-TOKCD(X)
             MOVE SPACE TO EDIT-CURSOR OF DSP-TOKCD(X)
     END-PERFORM.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             マスタ→ワーク（退避）                           *
****************************************************************
 MST-WORK-SEC          SECTION.
     MOVE     "MST-WORK-SEC"   TO   S-NAME.
*ワークテーブルクリア
     MOVE     SPACE            TO   TABLE-AREA.
     INITIALIZE                     TABLE-AREA.
*曜日のワーク退避
     MOVE     DSP-YOUBI        TO   TBL-YOUBI.
*ＳＥＱの作成
     MOVE     ZERO             TO   GYO-CNT.
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 6
             PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 15
                     ADD  1       TO GYO-CNT
                     MOVE GYO-CNT TO TBL-GYO(X Y)
             END-PERFORM
     END-PERFORM.
*曜日により基本スケジュールマスタスタート
     PERFORM JHMKIHF-START-SEC.
     IF     CHK-FLG  =  "CHK"
            GO                 TO     MST-WORK-EXIT
     END-IF.
*
*基本スケジュールマスタ読込み
     PERFORM  JHMKIHF-READ-SEC.
*基本スケジュールマスタを読込みながらワークへセット
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 6
                                   OR DSP-YOUBI NOT = KIH-F01
                                   OR RD-FLG = "END"
             PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 15
                                   OR DSP-YOUBI NOT = KIH-F01
                                   OR RD-FLG = "END"
                     MOVE KIH-F02 TO TBL-JIKAN(X Y)
                     MOVE KIH-F03 TO TBL-TOKCD(X Y)
                     PERFORM  JHMKIHF-READ-SEC
             END-PERFORM
     END-PERFORM.
     CLOSE  JHMKIHF.
     OPEN   I-O  JHMKIHF.
**
 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ読込み                     *
****************************************************************
 JHMKIHF-START-SEC     SECTION.
*
     MOVE     "JHMKIHF-START-SEC" TO   S-NAME.
*曜日により基本スケジュールマスタスタート
     MOVE       SPACE          TO     RD-FLG.
     MOVE       SPACE          TO     CHK-FLG.
     MOVE       DSP-YOUBI      TO     KIH-F01.
     MOVE       ZERO           TO     KIH-F02.
     START  JHMKIHF  KEY  IS  >=  KIH-F01  KIH-F02
            INVALID
            MOVE    "CHK"      TO     CHK-FLG
     END-START.
*
 JHMKIHF-START-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ読込み                     *
****************************************************************
 JHMKIHF-READ-SEC      SECTION.
     MOVE     "JHMKIHF-READ-SEC"  TO   S-NAME.
*マスタ読込み
     READ JHMKIHF NEXT AT END
          MOVE   "END"    TO   RD-FLG
     END-READ.
*
 JHMKIHF-READ-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                                 *
****************************************************************
 WORK-DSP-SEC          SECTION.
     MOVE     "WORK-DSP-SEC"      TO   S-NAME.
*項目画面セット
     MOVE      ZERO               TO   CHK-CNT.
     MOVE      TBL-YOUBI          TO   DSP-YOUBI.
     PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 15
         MOVE    TBL-GYO(P-CNT Y)   TO DSP-GYO(Y)
*********DISPLAY "DSP-GYO = " DSP-GYO(Y) UPON CONS
         IF  TBL-JIKAN(P-CNT Y)  NOT =  ZERO
         AND TBL-TOKCD(P-CNT Y)  NOT =  ZERO
             MOVE    TBL-JIKAN(P-CNT Y) TO DSP-JIKAN(Y)
             MOVE    TBL-TOKCD(P-CNT Y) TO DSP-TOKCD(Y)
*********DISPLAY "DSP-JIKAN = " DSP-JIKAN(Y) UPON CONS
*********DISPLAY "DSP-TOKCD = " DSP-TOKCD(Y) UPON CONS
             MOVE    TBL-TOKCD(P-CNT Y) TO TOK-F01
             PERFORM HTOKMS-READ-SEC
             IF      HTOKMS-INV-FLG  =  SPACE
                     MOVE  TOK-F03     TO DSP-TOKNM(Y)
             ELSE
                     MOVE  ALL NC"＊"  TO DSP-TOKNM(Y)
             END-IF
             MOVE    "01"              TO EOS-F01
             MOVE    1                 TO EOS-F02
             MOVE    TBL-TOKCD(P-CNT Y) TO EOS-F03
             PERFORM JHMEOSF-READ-SEC
             IF      JHMEOSF-INV-FLG  =  SPACE
                     MOVE    EOS-F04       TO DSP-SEN(Y)
                     EVALUATE EOS-F04
                         WHEN "I"
                         MOVE NC"ＩＳＤＮ" TO DSP-SENNM(Y)
                         WHEN "T"
                         MOVE NC"公衆回線" TO DSP-SENNM(Y)
                         WHEN "P"
                         MOVE NC"ＰＣ全銀" TO DSP-SENNM(Y)
                     END-EVALUATE
             ELSE
                     MOVE  "*"         TO DSP-SEN(Y)
                     MOVE  ALL NC"＊"  TO DSP-SENNM(Y)
             END-IF
             ADD     1                 TO CHK-CNT
         END-IF
     END-PERFORM.
*
 WORK-DSP-EXIT.
     EXIT.
****************************************************************
*             画面→ワーク退避                                 *
****************************************************************
 DSP-WORK-SEC          SECTION.
     MOVE     "DSP-WORK-SEC"      TO   S-NAME.
*項目画面セット
     PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 15
         IF  ( DSP-JIKAN(Y) NUMERIC )
         AND ( DSP-TOKCD(Y) NOT = ZERO AND DSP-TOKCD(Y) NUMERIC )
             MOVE    DSP-JIKAN(Y)   TO  TBL-JIKAN(P-CNT Y)
             MOVE    DSP-TOKCD(Y)   TO  TBL-TOKCD(P-CNT Y)
         END-IF
     END-PERFORM.
*
 DSP-WORK-EXIT.
     EXIT.
****************************************************************
*             取引先マスタ読込み                     3.0       *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
*
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
     READ HTOKMS  INVALID
          MOVE    "INV"     TO    HTOKMS-INV-FLG
          NOT  INVALID
          MOVE    SPACE     TO    HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             ＥＯＳ管理マスタ読込み                           *
****************************************************************
 JHMEOSF-READ-SEC      SECTION.
*
     MOVE     "JHEOSF-READ-SEC"   TO   S-NAME.
     READ JHMEOSF INVALID
          MOVE    "INV"     TO    JHMEOSF-INV-FLG
          NOT  INVALID
          MOVE    SPACE     TO    JHMEOSF-INV-FLG
     END-READ.
*
 JHMEOSF-READ-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ削除位置づけ               *
****************************************************************
 JHMKIHF-DEL-SEC       SECTION.
     MOVE     "JHKIHD-DEL-SEC"    TO   S-NAME.
*
     MOVE     SPACE               TO   RD-FLG.
     PERFORM JHMKIHF-START-SEC.
     IF      CHK-FLG = "CHK"
             GO         TO       JHMKIHF-DEL-EXIT
     END-IF.
     PERFORM JHMKIHF-READ-SEC.
     PERFORM DLT-SEC UNTIL DSP-YOUBI  NOT =  KIH-F01
                        OR RD-FLG     =      "END".
*
 JHMKIHF-DEL-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ削除                       *
****************************************************************
 DLT-SEC               SECTION.
     MOVE     "DLT-SEC"           TO   S-NAME.
*
     DELETE  JHMKIHF.
     PERFORM JHMKIHF-READ-SEC.
*****DISPLAY "RD-FLG = " RD-FLG
*
 DLT-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ作成                       *
****************************************************************
 JHMKIHF-WRITE-SEC     SECTION.
     MOVE     "JHMKIHF-WRITE-SEC" TO   S-NAME.
*基本スケジュールマスタを読込みながらワークへセット
     MOVE      1                  TO   Y.
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 6
                              OR ( TBL-JIKAN(X Y) = ZERO AND
                                   TBL-TOKCD(X Y) = ZERO )
*************DISPLAY "X-1 = " X UPON CONS
*************DISPLAY "Y-1 = " Y UPON CONS
             PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 15
                              OR ( TBL-JIKAN(X Y) = ZERO AND
                                   TBL-TOKCD(X Y) = ZERO )
                 IF  TBL-JIKAN(X Y)  NOT =  ZERO
**********DISPLAY "TBL-JIKAN = " TBL-JIKAN(X Y) UPON CONS
**********DISPLAY "X         = " X              UPON CONS
**********DISPLAY "Y         = " Y              UPON CONS
                     MOVE SPACE          TO KIH-REC
                     INITIALIZE             KIH-REC
                     MOVE DSP-YOUBI      TO KIH-F01
                     MOVE TBL-JIKAN(X Y) TO KIH-F02
                     MOVE TBL-TOKCD(X Y) TO KIH-F03
                     WRITE KIH-REC
                 END-IF
             END-PERFORM
             MOVE    1     TO       Y
     END-PERFORM.
*
 JHMKIHF-WRITE-EXIT.
     EXIT.
*****************<<  SJH0004I   END PROGRAM  >>******************
