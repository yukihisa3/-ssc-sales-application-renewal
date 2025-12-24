****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷検品システム                  *
*    業務名　　　　　　　：　マスタ連携                        *
*    モジュール名　　　　：　倉庫別検品取引先設定マスタ保守    *
*    作成日／更新日　　　：　2000/10/06                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SKE0030I.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         00/10/06.
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
*倉庫別検品取引先設定マスタ
     SELECT  SOKKENF   ASSIGN    TO        DA-01-VI-SOKKENL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       KEN-F01
                                           KEN-F02
                       FILE      STATUS    KEN-ST.
*倉庫マスタ
     SELECT  ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SOK-F01
                       FILE      STATUS    SOK-ST.
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
*    FILE = 倉庫別検品取引先設定マスタ                         *
****************************************************************
 FD  SOKKENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SOKKENF   OF   XFDLIB
                       JOINING   KEN       AS   PREFIX.
****************************************************************
*    FILE = 倉庫マスタ                                         *
****************************************************************
 FD  ZSOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
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
                       COPY      FKE00301  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KEN-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
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
     03  ZSOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
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
      VALUE NC"_取消_項目戻し_終了".
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫ＣＤを入力して下さい。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫マスタ未登録です。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先マスタ未登録です。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先ＣＤに重複入力があります。".
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
*倉庫別検品取引先設定マスタ
 01  TABLE-AREA.
     03  TBL-SOKCD                PIC   X(02).
     03  TABLE1                   OCCURS  15.
         05  TBL-GYO              PIC   9(02).
         05  TBL-TOKCD            PIC   9(08).
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  KEN-ERR           PIC N(15) VALUE
                        NC"倉庫検品取引先Ｍエラー".
     03  EOS-ERR           PIC N(15) VALUE
                        NC"倉庫マスタエラー".
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
 KEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SOKKENF.
     MOVE        KEN-ST    TO        E-ST.
     MOVE        "SOKKENF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KEN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     MOVE        SOK-ST    TO        E-ST.
     MOVE       "ZSOKMS1"  TO        E-FILE.
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
     OPEN      I-O              SOKKENF.
     OPEN      I-O              DSPFILE.
     OPEN      INPUT            ZSOKMS  HTOKMS.
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
     CLOSE             DSPFILE   SOKKENF
                       ZSOKMS    HTOKMS.
**
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*画面の初期化
     MOVE    SPACE                TO   DSP-FKE00301.
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
*             倉庫チェック                           2.1.1     *
****************************************************************
 HEAD-CHK-SEC             SECTION.
     MOVE     "HEAD-CHK-SEC"     TO   S-NAME.
*曜日チェック（未入力／範囲外）
     IF       DSP-SOKCD      =   SPACE
     OR       DSP-SOKCD      =   ZERO
              MOVE   1       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SOKCD
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SOKCD
     ELSE
              PERFORM ZSOKMS-READ-SEC
              IF  ZSOKMS-INV-FLG = "INV"
                  MOVE   2   TO  ERR-FLG
                  MOVE  "R"  TO  EDIT-OPTION  OF  DSP-SOKCD
                  MOVE  "C"  TO  EDIT-CURSOR  OF  DSP-SOKCD
              ELSE
                  MOVE  SOK-F02  TO  DSP-SOKNM
                  MOVE  "M"  TO  EDIT-OPTION  OF  DSP-SOKCD
                  MOVE SPACE TO  EDIT-CURSOR  OF  DSP-SOKCD
              END-IF
     END-IF.
*エラーフラグがゼロの場合
     IF       ERR-FLG = ZERO
              PERFORM MST-WORK-SEC
              MOVE    1      TO  P-CNT
              PERFORM WORK-DSP-SEC
              MOVE   "2"     TO  PSW
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
                        IF  ( DSP-TOKCD(Y) NOT = ZERO
                        AND   DSP-TOKCD(Y)       NUMERIC )
                            PERFORM   BODY-CHK1-SEC
                            ADD  1    TO   CHK-MEI
                        END-IF
                END-PERFORM
                IF   ERR-FLG  =  ZERO
                     PERFORM  BODY-CHK2-SEC
                     IF  ERR-FLG = ZERO
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
         WHEN   OTHER
                MOVE     4       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック                       2.2.1     *
****************************************************************
 BODY-CHK1-SEC         SECTION.
     MOVE     "BODY-CHK1-SEC"     TO   S-NAME.
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
 BODY-CHK1-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック                       2.2.1     *
****************************************************************
 BODY-CHK2-SEC         SECTION.
     MOVE     "BODY-CHK2-SEC"     TO   S-NAME.
*    ダブリ入力チェック
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 15
             IF  DSP-TOKCD(X) NUMERIC
             AND DSP-TOKCD(X) NOT = ZERO
                 PERFORM  BODY-CHK3-SEC
             END-IF
     END-PERFORM.
*
 BODY-CHK2-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック                       2.2.1     *
****************************************************************
 BODY-CHK3-SEC         SECTION.
     MOVE     "BODY-CHK3-SEC"     TO   S-NAME.
*    ダブリ入力チェック
     PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 15
          IF  DSP-TOKCD(Y)  NUMERIC
          AND DSP-TOKCD(Y)  NOT =  ZERO
             IF X NOT = Y
                IF DSP-TOKCD(X) = DSP-TOKCD(Y)
                   IF  ERR-FLG  =  ZERO
                       MOVE   5    TO   ERR-FLG
                   END-IF
                   MOVE "R"   TO EDIT-OPTION OF DSP-TOKCD(Y)
                   MOVE "C"   TO EDIT-CURSOR OF DSP-TOKCD(Y)
                END-IF
             END-IF
          END-IF
     END-PERFORM.
*
 BODY-CHK3-EXIT.
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
                PERFORM   SOKKENF-DEL-SEC
                PERFORM   SOKKENF-WRITE-SEC
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
     MOVE  PF-MSG1                    TO   DSP-PFGAID.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FKE00301"          TO   DSP-FMT.
     WRITE    DSP-FKE00301.
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

     MOVE    "FKE00301"           TO   DSP-FMT.
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
     MOVE "M"   TO EDIT-OPTION OF DSP-SOKCD.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SOKCD.
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 15
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
*倉庫ＣＤのワーク退避
     MOVE     DSP-SOKCD        TO   TBL-SOKCD.
*ＳＥＱの作成
     MOVE     ZERO             TO   GYO-CNT.
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 15
             ADD  1       TO GYO-CNT
             MOVE GYO-CNT TO TBL-GYO(X)
     END-PERFORM.
*曜日により基本スケジュールマスタスタート
     PERFORM SOKKENF-START-SEC.
     IF     CHK-FLG  =  "CHK"
            GO                 TO     MST-WORK-EXIT
     END-IF.
*
*基本スケジュールマスタ読込み
     PERFORM  SOKKENF-READ-SEC.
*基本スケジュールマスタを読込みながらワークへセット
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 15
                                   OR DSP-SOKCD NOT = KEN-F01
                                   OR RD-FLG = "END"
             MOVE KEN-F02 TO TBL-TOKCD(X)
             PERFORM  SOKKENF-READ-SEC
     END-PERFORM.
     CLOSE  SOKKENF.
     OPEN   I-O  SOKKENF.
**
 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ読込み                     *
****************************************************************
 SOKKENF-START-SEC     SECTION.
*
     MOVE     "SOKKENF-START-SEC" TO   S-NAME.
*曜日により基本スケジュールマスタスタート
     MOVE       SPACE          TO     RD-FLG.
     MOVE       SPACE          TO     CHK-FLG.
     MOVE       DSP-SOKCD      TO     KEN-F01.
     MOVE       ZERO           TO     KEN-F02.
     START  SOKKENF  KEY  IS  >=  KEN-F01  KEN-F02
            INVALID
            MOVE    "CHK"      TO     CHK-FLG
     END-START.
*
 SOKKENF-START-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ読込み                     *
****************************************************************
 SOKKENF-READ-SEC      SECTION.
     MOVE     "SOKKENF-READ-SEC"  TO   S-NAME.
*マスタ読込み
     READ SOKKENF NEXT AT END
          MOVE   "END"    TO   RD-FLG
     END-READ.
*
 SOKKENF-READ-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                                 *
****************************************************************
 WORK-DSP-SEC          SECTION.
     MOVE     "WORK-DSP-SEC"      TO   S-NAME.
*項目画面セット
     MOVE      ZERO               TO   CHK-CNT.
     MOVE      TBL-SOKCD          TO   DSP-SOKCD.
     PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 15
         MOVE    TBL-GYO(Y)   TO DSP-GYO(Y)
         IF  TBL-TOKCD(Y)  NOT =  ZERO
             MOVE    TBL-TOKCD(Y) TO DSP-TOKCD(Y)
             MOVE    TBL-TOKCD(Y) TO TOK-F01
             PERFORM HTOKMS-READ-SEC
             IF      HTOKMS-INV-FLG  =  SPACE
                     MOVE  TOK-F03     TO DSP-TOKNM(Y)
             ELSE
                     MOVE  ALL NC"＊"  TO DSP-TOKNM(Y)
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
         IF ( DSP-TOKCD(Y) NOT = ZERO AND DSP-TOKCD(Y) NUMERIC )
             MOVE    DSP-TOKCD(Y)   TO  TBL-TOKCD(Y)
         ELSE
             MOVE    ZERO           TO  TBL-TOKCD(Y)
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
 ZSOKMS-READ-SEC       SECTION.
*
     MOVE     "ZSOKMS-READ-SEC"   TO   S-NAME.
*
     MOVE      DSP-SOKCD          TO   SOK-F01.
     READ ZSOKMS INVALID
          MOVE    "INV"     TO    ZSOKMS-INV-FLG
          NOT  INVALID
          MOVE    SPACE     TO    ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ削除位置づけ               *
****************************************************************
 SOKKENF-DEL-SEC       SECTION.
     MOVE     "JHKIHD-DEL-SEC"    TO   S-NAME.
*
     MOVE     SPACE               TO   RD-FLG.
     PERFORM SOKKENF-START-SEC.
     IF      CHK-FLG = "CHK"
             GO         TO       SOKKENF-DEL-EXIT
     END-IF.
     PERFORM SOKKENF-READ-SEC.
     PERFORM DLT-SEC UNTIL DSP-SOKCD  NOT =  KEN-F01
                        OR RD-FLG     =      "END".
*
 SOKKENF-DEL-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ削除                       *
****************************************************************
 DLT-SEC               SECTION.
     MOVE     "DLT-SEC"           TO   S-NAME.
*
     DELETE  SOKKENF.
     PERFORM SOKKENF-READ-SEC.
*
 DLT-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ作成                       *
****************************************************************
 SOKKENF-WRITE-SEC     SECTION.
     MOVE     "SOKKENF-WRITE-SEC" TO   S-NAME.
*基本スケジュールマスタを読込みながらワークへセット
     MOVE      1                  TO   Y.
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 15
                 IF  TBL-TOKCD(X)  NOT =  ZERO
                     MOVE SPACE          TO KEN-REC
                     INITIALIZE             KEN-REC
                     MOVE DSP-SOKCD      TO KEN-F01
                     MOVE TBL-TOKCD(X)   TO KEN-F02
                     MOVE SYS-DATE       TO KEN-F03
                     MOVE WK-TIME(1:6)   TO KEN-F04
                     MOVE SYS-DATE       TO KEN-F05
                     MOVE WK-TIME(1:6)   TO KEN-F06
                     WRITE KEN-REC
                 END-IF
*****************MOVE    1     TO       Y
     END-PERFORM.
*
 SOKKENF-WRITE-EXIT.
     EXIT.
*****************<<  SKE0030I   END PROGRAM  >>******************
