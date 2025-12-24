****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　入荷管理システム                  *
*    業務名　　　　　　　：　入荷実績抽出処理                  *
*    モジュール名　　　　：　入荷実績抽出処理抽出指示　　　　  *
*    作成日／更新日　　　：　2012/07/06                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNT0010I.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         12/07/06.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*倉庫マスタ
     SELECT  ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SOK-F01
                       FILE      STATUS    SOK-ST.
*仕入先マスタ
     SELECT  ZSHIMS    ASSIGN    TO        DA-01-VI-ZSHIMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SHI-F01
                       FILE      STATUS    SHI-ST.
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
*    FILE = 倉庫マスタ　　                                     *
****************************************************************
 FD  ZSOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 仕入先マスタ                                       *
****************************************************************
 FD  ZSHIMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSHIMS    OF   XFDLIB
                       JOINING   SHI       AS   PREFIX.
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
                       COPY      FNT00101  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  SOK-ST                   PIC  X(02).
     03  SHI-ST                   PIC  X(02).
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
     03  ZSHIMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  ZSOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  RD-FLG                   PIC  X(03)  VALUE  SPACE.
     03  GYO-CNT                  PIC  9(02)  VALUE  ZERO.
     03  CHK-CNT                  PIC  9(01)  VALUE  ZERO.
     03  CHK-MEI                  PIC  9(02)  VALUE  ZERO.
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
             VALUE NC"年月を入力して下さい。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"年月論理エラーです。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫マスタ未登録です。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"仕入先マスタ未登録です。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先マスタ未登録です。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです。".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  9   PIC   N(20).
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
***
 01  CHK-DATE-WORK.
     03  CHK-01              PIC  9(02).
     03  CHK-02              PIC  9(02).
     03  MATUBI              PIC  X(24)  VALUE
         "312831303130313130313031".
     03  FILLER              REDEFINES   MATUBI.
         05  WK-MATUBI       PIC  9(02)  OCCURS  12.
*
 01  WK-ST-DATE.
     03  WK-ST-YYMM.
         05   WK-ST-YY       PIC  9(04)  VALUE  ZERO.
         05   WK-ST-MM       PIC  9(02)  VALUE  ZERO.
     03  WK-ST-DD            PIC  9(02)  VALUE  ZERO.
*
 01  WK-ED-DATE.
     03  WK-ED-YYMM.
         05   WK-ED-YY       PIC  9(04)  VALUE  ZERO.
         05   WK-ED-MM       PIC  9(02)  VALUE  ZERO.
     03  WK-ED-DD            PIC  9(02)  VALUE  ZERO.
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
 01  LINK-OUT-YMD8         PIC 9(08).
*
 LINKAGE               SECTION.
 01 PARA-DATE-ST           PIC 9(08).
 01 PARA-DATE-ED           PIC 9(08).
 01 PARA-SOKCD             PIC X(02).
 01 PARA-SIRCD1            PIC 9(08).
 01 PARA-SIRCD2            PIC 9(08).
 01 PARA-SIRCD3            PIC 9(08).
 01 PARA-SIRCD4            PIC 9(08).
 01 PARA-SIRCD5            PIC 9(08).
 01 PARA-SIRCD6            PIC 9(08).
 01 PARA-SIRCD7            PIC 9(08).
 01 PARA-SIRCD8            PIC 9(08).
 01 PARA-SIRCD9            PIC 9(08).
 01 PARA-SIRCD10           PIC 9(08).
 01 PARA-TORCD1            PIC 9(08).
 01 PARA-TORCD2            PIC 9(08).
 01 PARA-TORCD3            PIC 9(08).
 01 PARA-TORCD4            PIC 9(08).
 01 PARA-TORCD5            PIC 9(08).
 01 PARA-TORCD6            PIC 9(08).
 01 PARA-TORCD7            PIC 9(08).
 01 PARA-TORCD8            PIC 9(08).
 01 PARA-TORCD9            PIC 9(08).
 01 PARA-TORCD10           PIC 9(08).
**************************************************************
 PROCEDURE             DIVISION  USING  PARA-DATE-ST
                                        PARA-DATE-ED
                                        PARA-SOKCD
                                        PARA-SIRCD1
                                        PARA-SIRCD2
                                        PARA-SIRCD3
                                        PARA-SIRCD4
                                        PARA-SIRCD5
                                        PARA-SIRCD6
                                        PARA-SIRCD7
                                        PARA-SIRCD8
                                        PARA-SIRCD9
                                        PARA-SIRCD10
                                        PARA-TORCD1
                                        PARA-TORCD2
                                        PARA-TORCD3
                                        PARA-TORCD4
                                        PARA-TORCD5
                                        PARA-TORCD6
                                        PARA-TORCD7
                                        PARA-TORCD8
                                        PARA-TORCD9
                                        PARA-TORCD10.
**************************************************************
 DECLARATIVES.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     MOVE        SOK-ST    TO        E-ST.
     MOVE        "ZSOKMS1" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KEN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SHI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSHIMS.
     MOVE        SHI-ST    TO        E-ST.
     MOVE       "ZSHIMS1"  TO        E-FILE.
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
     MOVE     ZERO                TO   LINK-OUT-YMD8.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
     MOVE      LINK-OUT-YMD8      TO   DATE-AREA.
     ACCEPT    WK-TIME          FROM   TIME.
*ファイルのＯＰＥＮ
     OPEN      I-O              DSPFILE.
     OPEN      INPUT            ZSOKMS  ZSHIMS  HTOKMS.
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
*日付、倉庫入力(1.1)
         WHEN      "1"       PERFORM   DSP-HEAD-SEC
*明細入力１  (2.2)
         WHEN      "2"       PERFORM   DSP-BODY-SEC
*明細入力２  (2.3)
         WHEN      "3"       PERFORM   DSP-BODY2-SEC
*確認入力    (2.4)
         WHEN      "4"       PERFORM   DSP-KAKU-SEC
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
     CLOSE             DSPFILE   ZSHIMS
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
     MOVE    SPACE                TO   DSP-FNT00101.
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
                MOVE    4010     TO   PROGRAM-STATUS
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*             倉庫チェック                           2.1.1     *
****************************************************************
 HEAD-CHK-SEC             SECTION.
     MOVE     "HEAD-CHK-SEC"     TO   S-NAME.
*年月チェック
     IF  DSP-NENGET     =   ZERO
     OR  DSP-NENGET     NOT NUMERIC
         MOVE   1       TO  ERR-FLG
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-NENGET
         MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-NENGET
     ELSE
         MOVE  "2"        TO        LINK-IN-KBN
         MOVE  DSP-NENGET TO        WK-ST-YYMM  WK-ED-YYMM
         MOVE  01         TO        WK-ST-DD
         MOVE  WK-ST-DATE TO        LINK-IN-YMD8
         CALL  "SKYDTCKB" USING     LINK-IN-KBN
                                    LINK-IN-YMD6
                                    LINK-IN-YMD8
                                    LINK-OUT-RET
                                    LINK-OUT-YMD8
         IF   LINK-OUT-RET   NOT =     ZERO
              IF        ERR-FLG   =    ZERO
                        MOVE      2    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF DSP-NENGET
              MOVE     "R"        TO   EDIT-OPTION OF DSP-NENGET
              GO                  TO   HEAD-010
         END-IF
*********末日取得
         DIVIDE   WK-ST-YY       BY   4
                  GIVING    CHK-01    REMAINDER      CHK-02
         IF       CHK-02    =    0
                  MOVE      29        TO   WK-MATUBI (2)
         ELSE
                  MOVE      28        TO   WK-MATUBI (2)
         END-IF
         MOVE     WK-MATUBI(WK-ST-MM) TO   WK-ED-DD
         MOVE     WK-ST-DATE          TO   DSP-NENGST
         MOVE     WK-ED-DATE          TO   DSP-NENGED
         MOVE     "M"   TO  EDIT-OPTION  OF  DSP-NENGET
         MOVE     SPACE TO  EDIT-CURSOR  OF  DSP-NENGET
     END-IF.
 HEAD-010.
*倉庫コードチェック
     IF       DSP-SOKCD      =   SPACE
              MOVE  NC"全倉庫"   TO  DSP-SOKNM
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SOKCD
              MOVE  SPACE    TO  EDIT-CURSOR  OF  DSP-SOKCD
     ELSE
              MOVE  DSP-SOKCD TO SOK-F01
              PERFORM ZSOKMS-READ-SEC
              IF  ZSOKMS-INV-FLG = "INV"
                  MOVE   3   TO  ERR-FLG
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
                MOVE    ZERO      TO   ERR-FLG
                PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 10
                        IF  ( DSP-SIRCD(Y) NOT = ZERO
                        AND   DSP-SIRCD(Y)       NUMERIC )
                            PERFORM   BODY-CHK1-SEC
                        END-IF
                END-PERFORM
                IF   ERR-FLG  =  ZERO
                     MOVE "3"    TO   PSW
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    4010     TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
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
     IF       DSP-SIRCD(Y)   NOT  NUMERIC
     OR       DSP-SIRCD(Y)   =   ZERO
              IF  ERR-FLG  =  ZERO
                  MOVE   4    TO   ERR-FLG
              END-IF
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-SIRCD(Y)
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-SIRCD(Y)
     ELSE
              MOVE     DSP-SIRCD(Y)     TO SHI-F01
              PERFORM  ZSHIMS-READ-SEC
              IF       ZSHIMS-INV-FLG = SPACE
                       MOVE  SHI-F02    TO DSP-SIRNM(Y)
                       MOVE "M"   TO EDIT-OPTION OF DSP-SIRCD(Y)
                       MOVE SPACE TO EDIT-CURSOR OF DSP-SIRCD(Y)
              ELSE
                       MOVE  ALL NC"＊" TO DSP-SIRNM(Y)
                       IF  ERR-FLG  =  ZERO
                           MOVE   4    TO   ERR-FLG
                       END-IF
                       MOVE "R"   TO EDIT-OPTION OF DSP-SIRCD(Y)
                       MOVE "C"   TO EDIT-CURSOR OF DSP-SIRCD(Y)
              END-IF
     END-IF.
*
 BODY-CHK1-EXIT.
     EXIT.
****************************************************************
*             明細項目　入力( PSW = 2 )              2.2       *
****************************************************************
 DSP-BODY2-SEC         SECTION.
     MOVE     "DSP-BODY2-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                MOVE    ZERO      TO   ERR-FLG
                PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 10
                        IF  ( DSP-TORCD(Y) NOT = ZERO
                        AND   DSP-TORCD(Y)       NUMERIC )
                            PERFORM   BODY-CHK2-SEC
                        END-IF
                END-PERFORM
                IF   ERR-FLG  =  ZERO
                     MOVE "4"    TO   PSW
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    4010     TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE    "2"      TO   PSW
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック                       2.2.1     *
****************************************************************
 BODY-CHK2-SEC         SECTION.
     MOVE     "BODY-CHK2-SEC"     TO   S-NAME.
*
*取引先コード
     IF       DSP-TORCD(Y)   NOT  NUMERIC
     OR       DSP-TORCD(Y)   =   ZERO
              IF  ERR-FLG  =  ZERO
                  MOVE   5    TO   ERR-FLG
              END-IF
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-TORCD(Y)
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-TORCD(Y)
     ELSE
              MOVE     DSP-TORCD(Y)     TO TOK-F01
              PERFORM  HTOKMS-READ-SEC
              IF       HTOKMS-INV-FLG = SPACE
                       MOVE  TOK-F02    TO DSP-TORNM(Y)
                       MOVE "M"   TO EDIT-OPTION OF DSP-TORCD(Y)
                       MOVE SPACE TO EDIT-CURSOR OF DSP-TORCD(Y)
              ELSE
                       MOVE  ALL NC"＊" TO DSP-SIRNM(Y)
                       IF  ERR-FLG  =  ZERO
                           MOVE   5    TO   ERR-FLG
                       END-IF
                       MOVE "R"   TO EDIT-OPTION OF DSP-TORCD(Y)
                       MOVE "C"   TO EDIT-CURSOR OF DSP-TORCD(Y)
              END-IF
     END-IF.
*
 BODY-CHK2-EXIT.
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
                PERFORM   PARA-SET-SEC
                MOVE    "END"    TO   END-FLG
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    4010     TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE    "3"      TO   PSW
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
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
           MOVE    SPACE              TO   DSP-MSGSPC
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-MSGSPC
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     MOVE  PF-MSG1                    TO   DSP-FNCSPC.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FNT00101"          TO   DSP-FMT.
     WRITE    DSP-FNT00101.
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
                MOVE    "GRP001"  TO   DSP-GRP
*明細１
         WHEN   "2"
                MOVE    "GRP002"  TO   DSP-GRP
*明細２
         WHEN   "3"
                MOVE    "GRP003"  TO   DSP-GRP
*確認
         WHEN   "4"
                MOVE    "ENDCHK"  TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FNT00101"           TO   DSP-FMT.
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
***  年月
     MOVE "M"                    TO EDIT-OPTION OF DSP-NENGET.
     MOVE SPACE                  TO EDIT-CURSOR OF DSP-NENGET.
     MOVE "M"                    TO EDIT-OPTION OF DSP-SOKCD.
     MOVE SPACE                  TO EDIT-CURSOR OF DSP-SOKCD.
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 15
***  取引先
          MOVE "M"               TO EDIT-OPTION OF DSP-SIRCD(X)
          MOVE SPACE             TO EDIT-CURSOR OF DSP-SIRCD(X)
          MOVE "M"               TO EDIT-OPTION OF DSP-TORCD(X)
          MOVE SPACE             TO EDIT-CURSOR OF DSP-TORCD(X)
     END-PERFORM.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*     パラメタセット
****************************************************************
 PARA-SET-SEC          SECTION.
     MOVE     "PARA-SET-SEC"   TO   S-NAME.
*日付範囲
     MOVE     DSP-NENGST       TO   PARA-DATE-ST.
     MOVE     DSP-NENGED       TO   PARA-DATE-ED.
*倉庫ＣＤ
     MOVE     DSP-SOKCD        TO   PARA-SOKCD.
*仕入先コード
     IF  DSP-SIRCD(01)  =  ZERO
     OR  DSP-SIRCD(01)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-SIRCD1
     ELSE
         MOVE  DSP-SIRCD(01)   TO   PARA-SIRCD1
     END-IF.
     IF  DSP-SIRCD(02)  =  ZERO
     OR  DSP-SIRCD(02)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-SIRCD2
     ELSE
         MOVE  DSP-SIRCD(02)   TO   PARA-SIRCD2
     END-IF.
     IF  DSP-SIRCD(03)  =  ZERO
     OR  DSP-SIRCD(03)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-SIRCD3
     ELSE
         MOVE  DSP-SIRCD(03)   TO   PARA-SIRCD3
     END-IF.
     IF  DSP-SIRCD(04)  =  ZERO
     OR  DSP-SIRCD(04)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-SIRCD4
     ELSE
         MOVE  DSP-SIRCD(04)   TO   PARA-SIRCD4
     END-IF.
     IF  DSP-SIRCD(05)  =  ZERO
     OR  DSP-SIRCD(05)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-SIRCD5
     ELSE
         MOVE  DSP-SIRCD(05)   TO   PARA-SIRCD5
     END-IF.
     IF  DSP-SIRCD(06)  =  ZERO
     OR  DSP-SIRCD(06)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-SIRCD6
     ELSE
         MOVE  DSP-SIRCD(06)   TO   PARA-SIRCD6
     END-IF.
     IF  DSP-SIRCD(07)  =  ZERO
     OR  DSP-SIRCD(07)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-SIRCD7
     ELSE
         MOVE  DSP-SIRCD(07)   TO   PARA-SIRCD7
     END-IF.
     IF  DSP-SIRCD(08)  =  ZERO
     OR  DSP-SIRCD(08)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-SIRCD8
     ELSE
         MOVE  DSP-SIRCD(08)   TO   PARA-SIRCD8
     END-IF.
     IF  DSP-SIRCD(09)  =  ZERO
     OR  DSP-SIRCD(09)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-SIRCD9
     ELSE
         MOVE  DSP-SIRCD(09)   TO   PARA-SIRCD9
     END-IF.
     IF  DSP-SIRCD(10)  =  ZERO
     OR  DSP-SIRCD(10)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-SIRCD10
     ELSE
         MOVE  DSP-SIRCD(10)   TO   PARA-SIRCD10
     END-IF.
*取引先コード
     IF  DSP-TORCD(01)  =  ZERO
     OR  DSP-TORCD(01)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-TORCD1
     ELSE
         MOVE  DSP-TORCD(01)   TO   PARA-TORCD1
     END-IF.
     IF  DSP-TORCD(02)  =  ZERO
     OR  DSP-TORCD(02)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-TORCD2
     ELSE
         MOVE  DSP-TORCD(02)   TO   PARA-TORCD2
     END-IF.
     IF  DSP-TORCD(03)  =  ZERO
     OR  DSP-TORCD(03)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-TORCD3
     ELSE
         MOVE  DSP-TORCD(03)   TO   PARA-TORCD3
     END-IF.
     IF  DSP-TORCD(04)  =  ZERO
     OR  DSP-TORCD(04)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-TORCD4
     ELSE
         MOVE  DSP-TORCD(04)   TO   PARA-TORCD4
     END-IF.
     IF  DSP-TORCD(05)  =  ZERO
     OR  DSP-TORCD(05)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-TORCD5
     ELSE
         MOVE  DSP-TORCD(05)   TO   PARA-TORCD5
     END-IF.
     IF  DSP-TORCD(06)  =  ZERO
     OR  DSP-TORCD(06)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-TORCD6
     ELSE
         MOVE  DSP-TORCD(06)   TO   PARA-TORCD6
     END-IF.
     IF  DSP-TORCD(07)  =  ZERO
     OR  DSP-TORCD(07)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-TORCD7
     ELSE
         MOVE  DSP-TORCD(07)   TO   PARA-TORCD7
     END-IF.
     IF  DSP-TORCD(08)  =  ZERO
     OR  DSP-TORCD(08)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-TORCD8
     ELSE
         MOVE  DSP-TORCD(08)   TO   PARA-TORCD8
     END-IF.
     IF  DSP-TORCD(09)  =  ZERO
     OR  DSP-TORCD(09)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-TORCD9
     ELSE
         MOVE  DSP-TORCD(09)   TO   PARA-TORCD9
     END-IF.
     IF  DSP-TORCD(10)  =  ZERO
     OR  DSP-TORCD(10)  NOT  NUMERIC
         MOVE  ZERO            TO   PARA-TORCD10
     ELSE
         MOVE  DSP-TORCD(10)   TO   PARA-TORCD10
     END-IF.
**
 PARA-SET-EXIT.
     EXIT.
****************************************************************
*             取引先マスタ読込み                     3.0       *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
*
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
*
     READ HTOKMS  INVALID
          MOVE    "INV"     TO    HTOKMS-INV-FLG
          NOT  INVALID
          MOVE    SPACE     TO    HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ読込
****************************************************************
 ZSOKMS-READ-SEC       SECTION.
*
     MOVE     "ZSOKMS-READ-SEC"   TO   S-NAME.
*
     READ ZSOKMS INVALID
          MOVE    "INV"     TO    ZSOKMS-INV-FLG
          NOT  INVALID
          MOVE    SPACE     TO    ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
****************************************************************
*    仕入先マスタ読込
****************************************************************
 ZSHIMS-READ-SEC       SECTION.
*
     MOVE     "ZSHIMS-READ-SEC"   TO   S-NAME.
*
     READ ZSHIMS INVALID
          MOVE    "INV"     TO    ZSHIMS-INV-FLG
          NOT  INVALID
          MOVE    SPACE     TO    ZSHIMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
*****************<<  SNT0010I   END PROGRAM  >>******************
