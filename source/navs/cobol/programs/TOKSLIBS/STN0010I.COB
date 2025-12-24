****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　富岡配送センター専用              *
*    業務名　　　　　　　：　_番表　　　　                    *
*    モジュール名　　　　：　_番表発行指示                    *
*    作成日／更新日　　　：　2006/05/23                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　画面より，_番表出力に必要な情報  *
*                            と入力する。　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           STN0010I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2006/05/23.
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
     SELECT  ZSOKMS    ASSIGN    TO        ZSOKMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SOK-F01
                       FILE      STATUS    SOK-ST.
*画面定義ファイル
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 倉庫マスタ
****************************************************************
 FD  ZSOKMS
                       BLOCK     CONTAINS  20   RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FTN00101  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  SOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG                 PIC  X(03)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
***  バッチ_の売上伝票Ｆへの存在チェック
     03  KEN-FLG                  PIC  9(01)  VALUE  ZERO.
     03  ZSOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  明細ブレイク判定
     03  WK-DENNO                 PIC  9(08)  VALUE  ZERO.
***  ラインカウント
     03  L-CNT                    PIC  9(02)  VALUE  ZERO.
***  ページカウント
     03  P-CNT                    PIC  9(03)  VALUE  ZERO.
***  伝票_１枚目判定
     03  RD-SW                    PIC  9(01)  VALUE  ZERO.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
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
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.

*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY          PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM            PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD            PIC   9(02)  VALUE  ZERO.
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(15)
             VALUE NC"Ｆ４：取消　Ｆ５：終了".
     03  PF-MSG2.
         05  FILLER               PIC   N(15)
             VALUE NC"Ｆ４：取消Ｆ５：終了Ｆ６：戻し".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(15).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫ＣＤを入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"開始が終了を超えています".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
            VALUE NC"データ作成区分を入力して下さい".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  4   PIC   N(20).
*
 01  FILE-ERR.
     03  SOK-ERR           PIC N(20) VALUE
                        NC"倉庫マスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                   SECTION.
 01  PARA-IN-SOKCD         PIC X(02).
 01  PARA-IN-DSOKCD        PIC X(02).
 01  PARA-OUT-SOKCD        PIC X(02).
 01  PARA-OUT-TANAST       PIC X(06).
 01  PARA-OUT-TANAED       PIC X(06).
 01  PARA-OUT-DATA         PIC 9(01).
**************************************************************
**************************************************************
 PROCEDURE             DIVISION  USING
                                 PARA-IN-SOKCD
                                 PARA-IN-DSOKCD
                                 PARA-OUT-SOKCD
                                 PARA-OUT-TANAST
                                 PARA-OUT-TANAED
                                 PARA-OUT-DATA.
**************************************************************
 DECLARATIVES.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS START"     TO   S-NAME.
***
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
***
     STOP    RUN.
 PROCESS-EXIT.
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
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム時間取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     ZSOKMS.
*ワークの初期化
     INITIALIZE         FLG-AREA.
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
*パラメタ入力
         WHEN      "1"  PERFORM   DSP-PARA-SEC
*確認入力
         WHEN      "2"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-PARA-SEC         SECTION.
     MOVE     "DSP-PARA-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK-SEC
                IF       ERR-FLG = ZERO
                         MOVE   "2"   TO   PSW
                END-IF
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     4       TO   ERR-FLG
                GO       TO      DSP-PARA-SEC
     END-EVALUATE.
*
 DSP-PARA-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック                       2.1.1     *
****************************************************************
 PARA-CHK-SEC             SECTION.
     MOVE     "PARA-CHK-SEC"     TO   S-NAME.
*
*倉庫ＣＤチェック
***  倉庫ＣＤ未入力チェック
     IF       DSP-SOKCD   =  SPACE
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SOKCD
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SOKCD
              GO             TO   PARA-CHK-EXIT
     ELSE
***           倉庫マスタチェック
              MOVE     DSP-SOKCD      TO   SOK-F01
              PERFORM  ZSOKMS-READ-SEC
              IF       ZSOKMS-INV-FLG =  "INV"
                       MOVE  1     TO ERR-FLG
                       MOVE  "R"   TO EDIT-OPTION OF DSP-SOKCD
                       MOVE  "C"   TO EDIT-CURSOR OF DSP-SOKCD
                       MOVE ALL NC"＊" TO DSP-SOKNM
                       GO          TO PARA-CHK-EXIT
              ELSE
                       MOVE SOK-F02 TO DSP-SOKNM
                       MOVE  "M"   TO EDIT-OPTION OF DSP-SOKCD
                       MOVE  SPACE TO EDIT-CURSOR OF DSP-SOKCD
              END-IF
*
     END-IF.
*
*_番チェック
     IF       DSP-TANAED = SPACE
              MOVE "999999" TO DSP-TANAED
     END-IF.
*範囲チェック
     IF       DSP-TANAST  >  DSP-TANAED
              MOVE   2       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-TANAST
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-TANAED
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-TANAST
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-TANAST
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-TANAST
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-TANAED
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-TANAED
     END-IF.
*データ作成区分
     IF       DSP-DATAK = 1 OR 2
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-DATAK
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-DATAK
     ELSE
              MOVE  3     TO ERR-FLG
              MOVE "R"    TO EDIT-OPTION OF DSP-DATAK
              MOVE "C"    TO EDIT-CURSOR OF DSP-DATAK
              GO          TO PARA-CHK-EXIT
     END-IF.
*
 PARA-CHK-EXIT.
     EXIT.
****************************************************************
*             確認項目入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-KAKU-SEC         SECTION.
     MOVE     "DSP-KAKU-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM PARA-OUTPUT-SEC
                MOVE    "END"        TO   END-FLG
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     4       TO   ERR-FLG
                GO       TO      DSP-KAKU-SEC
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＶＬＤ５００出力処理                            *
****************************************************************
 PARA-OUTPUT-SEC       SECTION.
*
     MOVE   "PARA-OUTPUT-SEC" TO   S-NAME.
*
     MOVE      DSP-SOKCD          TO    PARA-OUT-SOKCD.
     MOVE      DSP-TANAST         TO    PARA-OUT-TANAST.
     MOVE      DSP-TANAED         TO    PARA-OUT-TANAED.
     MOVE      DSP-DATAK          TO    PARA-OUT-DATA.
*
 PARA-OUTPUT-EXIT.
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
     EVALUATE   PSW
***      パラメタ項目
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-FNCSPC
***      確認
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
***      その他
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-FNCSPC
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FTN00101"          TO   DSP-FMT.
     WRITE    DSP-FTN00101.
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
     EVALUATE   PSW
*パラメタ項目
         WHEN   "1"
                MOVE    "PARGRP"  TO   DSP-GRP
*確認
         WHEN   "2"
                MOVE    "CHKGRP"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FTN00101"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*画面の初期化
     MOVE    SPACE                TO   DSP-FTN00101.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
     IF      PARA-IN-DSOKCD  =  "01"
             MOVE    " "    TO   EDIT-STATUS OF DSP-SOKCD
     ELSE
              MOVE    PARA-IN-SOKCD  TO   SOK-F01 DSP-SOKCD
              READ      ZSOKMS
                  INVALID  KEY
                        MOVE      ALL NC"＊" TO   DSP-SOKNM
              NOT INVALID  KEY
                        MOVE      SOK-F02    TO   DSP-SOKNM
              END-READ
              MOVE    "X"    TO   EDIT-STATUS OF  DSP-SOKCD
     END-IF.
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*
*リバース，カーソルパーク解除
***  倉庫ＣＤ
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SOKCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SOKCD.
***  _番開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TANAST.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TANAST.
***  _番終了
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TANAED.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TANAED.
***  データ区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DATAK.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DATAK.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               6.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             ZSOKMS DSPFILE.
**
 END-EXIT.
     EXIT.
****************************************************************
*             在庫マスタ読み込み                     6.0       *
****************************************************************
 ZSOKMS-READ-SEC       SECTION.
*
     READ ZSOKMS
          INVALID      MOVE  "INV"  TO   ZSOKMS-INV-FLG
          NOT INVALID  MOVE  SPACE  TO   ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
*****************<<  STN0010I   END PROGRAM  >>******************
