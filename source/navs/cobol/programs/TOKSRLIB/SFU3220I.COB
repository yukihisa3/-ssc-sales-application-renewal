****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＡＣＯＳ振替　　　　　　　　　　　*
*    業務名　　　　　　　：　ＡＣＯＳ振替処理改善　　　　　　　*
*    モジュール名　　　　：　ＡＣＯＳ振替更新ＤＴリスト発行指示*
*    作成日／更新日　　　：　16/12/27                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　画面より帳票の発行条件を入力する。*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　      *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SFU3220I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         16/12/27.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     YA           IS   CHR-2
     YB-21        IS   CHR-21
     YB           IS   CHR-15
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*画面ファイル
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
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FFU32201  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
*カウンタ領域
 01  COUNTER.
     03  READ-CNT                 PIC  9(05)  VALUE  ZERO.
     03  SKIP-CNT                 PIC  9(05)  VALUE  ZERO.
     03  OUT-CNT                  PIC  9(05)  VALUE  ZERO.
***  バッチ_の売上伝票Ｆへの存在チェック
     03  DEN-FLG                  PIC  9(01)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
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
*
*発注日、納品日編集
 01  WK-HENKAN-DATE               PIC   9(08)  VALUE  ZERO.
 01  WK-OUT-DATE.
     03  WK-OUT-YYYY              PIC   9(04)  VALUE  ZERO.
     03  WK-OUT-MM                PIC   9(02)  VALUE  ZERO.
     03  WK-OUT-DD                PIC   9(02)  VALUE  ZERO.
*
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
             VALUE NC"_取消　_終了".
     03  PF-MSG2.
         05  FILLER               PIC   N(15)
             VALUE NC"_取消　_終了　_項目戻し".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(15).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"開始が終了を超えています".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"日付論理エラーです！".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"受信日は必ず入力して下さい。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"発行区分が違います".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"出力区分が違います".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"伝票区分が存在しません！".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"ＤＴ区分が違います。".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG12.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  12  PIC   N(20).
*
 01  FILE-ERR.
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
 01  PARA-SOKCD                  PIC  X(02).
 01  PARA-DSOKCD                 PIC  X(02).
 01  PARA-JDTST                  PIC  9(08).
 01  PARA-JDTED                  PIC  9(08).
 01  PARA-SOKST                  PIC  X(02).
 01  PARA-SOKED                  PIC  X(02).
 01  PARA-SKDST                  PIC  9(08).
 01  PARA-SKDED                  PIC  9(08).
 01  PARA-SYOST                  PIC  X(08).
 01  PARA-SYOED                  PIC  X(08).
 01  PARA-HKKBN                  PIC  X(01).
 01  PARA-SRKBN                  PIC  X(01).
 01  PARA-DENK1                  PIC  X(02).
 01  PARA-DENK2                  PIC  X(02).
 01  PARA-DENK3                  PIC  X(02).
 01  PARA-DENK4                  PIC  X(02).
 01  PARA-DENK5                  PIC  X(02).
 01  PARA-DENK6                  PIC  X(02).
 01  PARA-DENK7                  PIC  X(02).
 01  PARA-DENK8                  PIC  X(02).
 01  PARA-OUTKBN                 PIC  X(01).
**************************************************************
 PROCEDURE             DIVISION  USING  PARA-SOKCD  PARA-DSOKCD
                                        PARA-JDTST  PARA-JDTED
                                        PARA-SOKST  PARA-SOKED
                                        PARA-SKDST  PARA-SKDED
                                        PARA-SYOST  PARA-SYOED
                                        PARA-HKKBN  PARA-SRKBN
                                        PARA-DENK1  PARA-DENK2
                                        PARA-DENK3  PARA-DENK4
                                        PARA-DENK5  PARA-DENK6
                                        PARA-DENK7  PARA-DENK8
                                        PARA-OUTKBN.
**************************************************************
 DECLARATIVES.
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
*ワークの初期化
     INITIALIZE         FLG-AREA.
*カウンタの初期化
     INITIALIZE         COUNTER.
*
     MOVE     SPACE               TO   DSP-PRO.
     PERFORM  INIT-DSP-SEC.
*ヘッド入力へ
     MOVE    "1"                  TO   PSW.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*画面の初期化
     MOVE    SPACE                TO   DSP-FFU32201.
*    MOVE   "FFU32201"            TO   DSP-FMT.
*システム日付転送
     MOVE    SYS-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:6)         TO   DSP-STIME.
*倉庫コードパラメタより倉庫コード入力チェック
     DISPLAY "PARA-DSOKCD = " PARA-DSOKCD UPON CONS.
     IF       PARA-DSOKCD  =  "01"  OR  "88"
              MOVE    " "    TO   EDIT-STATUS OF DSP-SOKST
              MOVE    " "    TO   EDIT-STATUS OF DSP-SOKED
     ELSE
              MOVE    PARA-SOKCD  TO   DSP-SOKST DSP-SOKED
              MOVE    "X"    TO   EDIT-STATUS OF DSP-SOKST
              MOVE    "X"    TO   EDIT-STATUS OF DSP-SOKED
     END-IF.
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
*
 INT-DSP-EXIT.
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
*パラメタ出力
         WHEN      "3"  PERFORM   PARA-SET-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               6.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             DSPFILE.
**
 END-EXIT.
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
                IF      ERR-FLG   =   ZERO
                        PERFORM  DSP-SYOKI-SEC
                        MOVE     "2"  TO   PSW
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
                MOVE     3       TO   ERR-FLG
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
     MOVE      ZERO              TO   ERR-FLG.
*
*受信日開始チェック
***  受信日開始　未入力チェック
     IF       DSP-JYUST  NOT NUMERIC
         OR   DSP-JYUST  =  ZERO
              MOVE  ZERO     TO   DSP-JYUST
     ELSE
***           論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-JYUST      TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  2        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JYUST
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JYUST
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JYUST
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-JYUST
     END-IF.
***  受信日終了　未入力チェック
     IF       DSP-JYUED  NOT NUMERIC
         OR   DSP-JYUED  =  ZERO
         OR   DSP-JYUED  =  99999999
              MOVE  99999999 TO   DSP-JYUED
     ELSE
***           論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-JYUED      TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  2        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JYUED
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JYUED
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JYUED
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-JYUED
     END-IF.
*受信日範囲チェック
     IF       DSP-JYUST   >  DSP-JYUED
              MOVE  1        TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JYUST
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JYUED
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JYUST
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JYUST
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JYUED
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-JYUST
     END-IF.
*#####################
*倉庫ＣＤチェック
***  倉庫ＣＤ開始未入力チェック
     IF       DSP-SOKST  =  SPACE
              MOVE  SPACE    TO   DSP-SOKST
     END-IF.
***  倉庫ＣＤ終了未入力チェック
     IF       DSP-SOKED  =  SPACE
              MOVE  "99"     TO   DSP-SOKED
     END-IF.
***  倉庫ＣＤ範囲大小チェック
     IF       DSP-SOKST  >  DSP-SOKED
              MOVE  1        TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SOKST
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SOKED
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SOKST
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SOKST
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SOKED
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SOKST
     END-IF.
*出荷日開始チェック
***  出荷日開始　未入力チェック
     IF       DSP-SYUST  NOT NUMERIC
         OR   DSP-SYUST  =  ZERO
              MOVE  ZERO     TO   DSP-SYUST
     ELSE
***           論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-SYUST      TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  2        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SYUST
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SYUST
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SYUST
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SYUST
     END-IF.
***  出荷日終了　未入力チェック
     IF       DSP-SYUED  NOT NUMERIC
         OR   DSP-SYUED  =  ZERO
         OR   DSP-SYUED  =  99999999
              MOVE  99999999 TO   DSP-SYUED
     ELSE
***           論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-SYUED      TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  2        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SYUED
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SYUED
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SYUED
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SYUED
     END-IF.
*出荷日範囲チェック
     IF       DSP-SYUST   >  DSP-SYUED
              MOVE  1        TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SYUST
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SYUED
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SYUST
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SYUST
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SYUED
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SYUST
     END-IF.
*#####################
*サカタ商品ＣＤチェック
***  サカタ商品ＣＤ開始未入力チェック
     IF       DSP-SYOST  =  SPACE
              MOVE  SPACE    TO   DSP-SYOST
     END-IF.
***  サカタ商品ＣＤ終了未入力チェック
     IF       DSP-SYOED  =  SPACE
              MOVE  "99999999" TO DSP-SYOED
     END-IF.
***  サカタ商品ＣＤ範囲大小チェック
     IF       DSP-SYOST  >  DSP-SYOED
              MOVE  1        TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SYOST
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SYOED
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SYOST
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SYOST
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SYOED
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SYOST
     END-IF.
*#####################
*発行区分チェック
     IF       DSP-HAKKBN  =  SPACE  OR  "1"
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-HAKKBN
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-HAKKBN
     ELSE
              MOVE  5        TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HAKKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-HAKKBN
              GO             TO   PARA-CHK-EXIT
     END-IF.
*出力区分
     IF       DSP-SYUKBN  =  SPACE  OR  "1"
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SYUKBN
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SYUKBN
     ELSE
              MOVE  6        TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SYUKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SYUKBN
              GO             TO   PARA-CHK-EXIT
     END-IF.
*#####################
*伝票区分チェック
***  伝票区分１
     IF       DSP-DENK1  =  SPACE
              MOVE  SPACE    TO   DSP-DENK1
     ELSE
              IF  DSP-DENK1 = "20" OR "70" OR "71" OR "35" OR
                              "36" OR "40" OR "41" OR "50" OR
                              "51" OR "30" OR "31" OR "32"
                  MOVE  "M"   TO EDIT-OPTION OF DSP-DENK1
                  MOVE  SPACE TO EDIT-CURSOR OF DSP-DENK1
              ELSE
                  MOVE  7     TO ERR-FLG
                  MOVE  "R"   TO EDIT-OPTION OF DSP-DENK1
                  MOVE  "C"   TO EDIT-CURSOR OF DSP-DENK1
                  GO          TO PARA-CHK-EXIT
              END-IF
     END-IF.
***  伝票区分２
     IF       DSP-DENK2  =  SPACE
              MOVE  SPACE    TO   DSP-DENK2
     ELSE
              IF  DSP-DENK2 = "20" OR "70" OR "71" OR "35" OR
                              "36" OR "40" OR "41" OR "50" OR
                              "51" OR "30" OR "31" OR "32"
                  MOVE  "M"   TO EDIT-OPTION OF DSP-DENK2
                  MOVE  SPACE TO EDIT-CURSOR OF DSP-DENK2
              ELSE
                  MOVE  7     TO ERR-FLG
                  MOVE  "R"   TO EDIT-OPTION OF DSP-DENK2
                  MOVE  "C"   TO EDIT-CURSOR OF DSP-DENK2
                  GO          TO PARA-CHK-EXIT
              END-IF
     END-IF.
***  伝票区分３
     IF       DSP-DENK3  =  SPACE
              MOVE  SPACE    TO   DSP-DENK3
     ELSE
              IF  DSP-DENK3 = "20" OR "70" OR "71" OR "35" OR
                              "36" OR "40" OR "41" OR "50" OR
                              "51" OR "30" OR "31" OR "32"
                  MOVE  "M"   TO EDIT-OPTION OF DSP-DENK3
                  MOVE  SPACE TO EDIT-CURSOR OF DSP-DENK3
              ELSE
                  MOVE  7     TO ERR-FLG
                  MOVE  "R"   TO EDIT-OPTION OF DSP-DENK3
                  MOVE  "C"   TO EDIT-CURSOR OF DSP-DENK3
                  GO          TO PARA-CHK-EXIT
              END-IF
     END-IF.
***  伝票区分４
     IF       DSP-DENK4  =  SPACE
              MOVE  SPACE    TO   DSP-DENK4
     ELSE
              IF  DSP-DENK4 = "20" OR "70" OR "71" OR "35" OR
                              "36" OR "40" OR "41" OR "50" OR
                              "51" OR "30" OR "31" OR "32"
                  MOVE  "M"   TO EDIT-OPTION OF DSP-DENK4
                  MOVE  SPACE TO EDIT-CURSOR OF DSP-DENK4
              ELSE
                  MOVE  7     TO ERR-FLG
                  MOVE  "R"   TO EDIT-OPTION OF DSP-DENK4
                  MOVE  "C"   TO EDIT-CURSOR OF DSP-DENK4
                  GO          TO PARA-CHK-EXIT
              END-IF
     END-IF.
***  伝票区分５
     IF       DSP-DENK5  =  SPACE
              MOVE  SPACE    TO   DSP-DENK5
     ELSE
              IF  DSP-DENK5 = "20" OR "70" OR "71" OR "35" OR
                              "36" OR "40" OR "41" OR "50" OR
                              "51" OR "30" OR "31" OR "32"
                  MOVE  "M"   TO EDIT-OPTION OF DSP-DENK5
                  MOVE  SPACE TO EDIT-CURSOR OF DSP-DENK5
              ELSE
                  MOVE  7     TO ERR-FLG
                  MOVE  "R"   TO EDIT-OPTION OF DSP-DENK5
                  MOVE  "C"   TO EDIT-CURSOR OF DSP-DENK5
                  GO          TO PARA-CHK-EXIT
              END-IF
     END-IF.
***  伝票区分６
     IF       DSP-DENK6  =  SPACE
              MOVE  SPACE    TO   DSP-DENK6
     ELSE
              IF  DSP-DENK6 = "20" OR "70" OR "71" OR "35" OR
                              "36" OR "40" OR "41" OR "50" OR
                              "51" OR "30" OR "31" OR "32"
                  MOVE  "M"   TO EDIT-OPTION OF DSP-DENK6
                  MOVE  SPACE TO EDIT-CURSOR OF DSP-DENK6
              ELSE
                  MOVE  7     TO ERR-FLG
                  MOVE  "R"   TO EDIT-OPTION OF DSP-DENK6
                  MOVE  "C"   TO EDIT-CURSOR OF DSP-DENK6
                  GO          TO PARA-CHK-EXIT
              END-IF
     END-IF.
***  伝票区分７
     IF       DSP-DENK7  =  SPACE
              MOVE  SPACE    TO   DSP-DENK7
     ELSE
              IF  DSP-DENK7 = "20" OR "70" OR "71" OR "35" OR
                              "36" OR "40" OR "41" OR "50" OR
                              "51" OR "30" OR "31" OR "32"
                  MOVE  "M"   TO EDIT-OPTION OF DSP-DENK7
                  MOVE  SPACE TO EDIT-CURSOR OF DSP-DENK7
              ELSE
                  MOVE  7     TO ERR-FLG
                  MOVE  "R"   TO EDIT-OPTION OF DSP-DENK7
                  MOVE  "C"   TO EDIT-CURSOR OF DSP-DENK7
                  GO          TO PARA-CHK-EXIT
              END-IF
     END-IF.
***  伝票区分８
     IF       DSP-DENK8  =  SPACE
              MOVE  SPACE    TO   DSP-DENK8
     ELSE
              IF  DSP-DENK8 = "20" OR "70" OR "71" OR "35" OR
                              "36" OR "40" OR "41" OR "50" OR
                              "51" OR "30" OR "31" OR "32"
                  MOVE  "M"   TO EDIT-OPTION OF DSP-DENK8
                  MOVE  SPACE TO EDIT-CURSOR OF DSP-DENK8
              ELSE
                  MOVE  7     TO ERR-FLG
                  MOVE  "R"   TO EDIT-OPTION OF DSP-DENK8
                  MOVE  "C"   TO EDIT-CURSOR OF DSP-DENK8
                  GO          TO PARA-CHK-EXIT
              END-IF
     END-IF.
***  ＤＴ区分
     IF       DSP-DTKBN  =  SPACE  OR  "1"  OR  "2"
              MOVE  "M"   TO EDIT-OPTION OF DSP-DTKBN
              MOVE  SPACE TO EDIT-CURSOR OF DSP-DTKBN
     ELSE
              MOVE  8     TO ERR-FLG
              MOVE  "R"   TO EDIT-OPTION OF DSP-DTKBN
              MOVE  "C"   TO EDIT-CURSOR OF DSP-DTKBN
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
                MOVE    "3"      TO   PSW
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
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-KAKU-SEC
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 PARA-SET-SEC          SECTION.
     MOVE     "PARA-SET-SEC"      TO   S-NAME.
*
*受信日開始
     MOVE      DSP-JYUST          TO   PARA-JDTST.
*受信日終了
     MOVE      DSP-JYUED          TO   PARA-JDTED.
*倉庫ＣＤ開始
     MOVE      DSP-SOKST          TO   PARA-SOKST.
*倉庫ＣＤ終了
     MOVE      DSP-SOKED          TO   PARA-SOKED.
*出荷日開始
     MOVE      DSP-SYUST          TO   PARA-SKDST.
*出荷日終了
     MOVE      DSP-SYUED          TO   PARA-SKDED.
*サカタ商品ＣＤ開始
     MOVE      DSP-SYOST          TO   PARA-SYOST.
*サカタ商品ＣＤ終了
     MOVE      DSP-SYOED          TO   PARA-SYOED.
*発行区分
     MOVE      DSP-HAKKBN         TO   PARA-HKKBN.
*出力区分
     MOVE      DSP-SYUKBN         TO   PARA-SRKBN.
*伝票区分１
     MOVE      DSP-DENK1          TO   PARA-DENK1.
*伝票区分２
     MOVE      DSP-DENK2          TO   PARA-DENK2.
*伝票区分３
     MOVE      DSP-DENK3          TO   PARA-DENK3.
*伝票区分４
     MOVE      DSP-DENK4          TO   PARA-DENK4.
*伝票区分５
     MOVE      DSP-DENK5          TO   PARA-DENK5.
*伝票区分６
     MOVE      DSP-DENK6          TO   PARA-DENK6.
*伝票区分７
     MOVE      DSP-DENK7          TO   PARA-DENK7.
*伝票区分８
     MOVE      DSP-DENK8          TO   PARA-DENK8.
*パラメタ表示
*****DISPLAY "PARA-JDTST = " PARA-JDTST  UPON CONS.
*    DISPLAY "PARA-JDTED = " PARA-JDTED  UPON CONS.
*    DISPLAY "PARA-SOKST = " PARA-SOKST  UPON CONS.
*    DISPLAY "PARA-SOKED = " PARA-SOKED  UPON CONS.
*    DISPLAY "PARA-SKDST = " PARA-SKDST  UPON CONS.
*    DISPLAY "PARA-SKDED = " PARA-SKDED  UPON CONS.
*    DISPLAY "PARA-SYOST = " PARA-SYOST  UPON CONS.
*    DISPLAY "PARA-SYOED = " PARA-SYOED  UPON CONS.
*    DISPLAY "PARA-HKKBN = " PARA-HKKBN  UPON CONS.
*****DISPLAY "PARA-SRKBN = " PARA-SRKBN  UPON CONS.
*ＤＴ区分
     MOVE      DSP-DTKBN          TO   PARA-OUTKBN.
*終了セット
     MOVE      "END"              TO   END-FLG.
*
 PARA-SET-EXIT.
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
     MOVE    "SFU3220I"          TO   DSP-PGID.
     MOVE    "FFU32201"          TO   DSP-FORMID.
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FFU32201"          TO   DSP-FMT.
     WRITE    DSP-FFU32201.
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
                MOVE    "GRP001"  TO   DSP-GRP
*確認
         WHEN   "2"
                MOVE    "ENDCHK"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FFU32201"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*
*リバース，カーソルパーク解除
***  受信日開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JYUST.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JYUST.
***  受信日終了
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JYUED.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JYUED.
***  倉庫ＣＤ開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SOKST.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SOKST.
***  倉庫ＣＤ終了
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SOKED.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SOKED.
***  出荷日開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SYUST.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SYUST.
***  出荷日終了
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SYUED.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SYUED.
***  サカタ商品ＣＤ開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SYOST.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SYOST.
***  サカタ商品ＣＤ終了
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SYOED.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SYOED.
***  発行区分　　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-HAKKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-HAKKBN.
***  出力区分　　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SYUKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SYUKBN.
***  伝票区分１　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENK1.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DENK1.
***  伝票区分２　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENK2.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DENK2.
***  伝票区分３　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENK3.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DENK3.
***  伝票区分４　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENK4.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DENK4.
***  伝票区分５　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENK5.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DENK5.
***  伝票区分６　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENK6.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DENK6.
***  伝票区分７　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENK7.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DENK7.
***  伝票区分８　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENK8.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DENK8.
***  ＤＴ区分　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DTKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DTKBN.
*
 DSP-SYOKI-EXIT.
     EXIT.
*****************<<  SFU3220I   END PROGRAM  >>******************
