****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　営利種子ストックＮＯ管理　　　　　*
*    業務名　　　　　　　：　営利種子ストックＮＯ管理　        *
*    モジュール名　　　　：　ストックＮＯ実績データ出力指示    *
*    作成日／更新日　　　：　20/07/13                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　画面よりパラメタを入力する。　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　            *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SST0040I.
 AUTHOR.               HAGIWARA.
 DATE-WRITTEN.         20/07/13.
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
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        TOKMS2
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
*ストックＮＯ管理ファイル
     SELECT  STNOKRF   ASSIGN    TO        STNOKRL2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       STN-F03
                                           STN-F05
                                           STN-F04
                                           STN-F06
                                           STN-F07
                                           STN-F08
                       FILE      STATUS    STN-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
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
                       COPY      FST00401  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
****************************************************************
*    FILE = ストックＮＯ管理ファイル（Ｌ２）                   *
****************************************************************
 FD  STNOKRF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      STNOKRF   OF   XFDLIB
                       JOINING   STN       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
     03  STN-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  TAISYO-FLG               PIC  X(01)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  END1-FLG                 PIC  9(01)  VALUE  ZERO.
*カウンタ領域
 01  COUNTER.
     03  READ-CNT                 PIC  9(05)  VALUE  ZERO.
     03  SKIP-CNT                 PIC  9(05)  VALUE  ZERO.
     03  TAISYO-CNT               PIC  9(06)  VALUE  ZERO.
     03  WK-GK-SYUKASU            PIC  9(06)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  インデックス
     03  IXA                      PIC  9(02)  VALUE  ZERO.
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
             VALUE NC"取引先ＣＤを入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先マスタに登録されていません".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"ストックＮＯ入力チェックエラー".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"出荷数チェックエラー".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい値を入力してください".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"売上伝票ファイルに登録されていません".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫マスタに登録されていません".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"開始納品日を正しく入力して下さい".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"終了納品日を正しく入力して下さい".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"開始が終了を超えています".
     03  ERR-MSG12.
         05  FILLER              PIC   N(20)
             VALUE NC"対象データがありません".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  12  PIC   N(20).
*
 01  FILE-ERR.
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
     03  STN-ERR           PIC N(20) VALUE
                        NC"ストックＮＯ管理Ｆエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                   SECTION.
 01  PARA-SOKO                   PIC  X(02).
 01  PARA-DSOKO                  PIC  X(02).
 01  PARA-TOKCD                  PIC  9(08).
 01  PARA-NOUDTS                 PIC  9(08).
 01  PARA-NOUDTE                 PIC  9(08).
 01  PARA-SOKCDS                 PIC  X(02).
 01  PARA-SOKCDE                 PIC  X(02).
 01  PARA-JANCDS                 PIC  X(13).
 01  PARA-JANCDE                 PIC  X(13).
 01  PARA-STNOS                  PIC  X(06).
 01  PARA-STNOE                  PIC  X(06).
 01  PARA-KBN1                   PIC  X(01).
 01  PARA-KBN2                   PIC  X(01).
**************************************************************
 PROCEDURE             DIVISION  USING  PARA-SOKO
                                        PARA-DSOKO
                                        PARA-TOKCD
                                        PARA-NOUDTS
                                        PARA-NOUDTE
                                        PARA-SOKCDS
                                        PARA-SOKCDE
                                        PARA-JANCDS
                                        PARA-JANCDE
                                        PARA-STNOS
                                        PARA-STNOE
                                        PARA-KBN1
                                        PARA-KBN2.
**************************************************************
 DECLARATIVES.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 STN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE STNOKRF.
     DISPLAY     STN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     STN-ST    UPON      CONS.
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
     OPEN     INPUT     HTOKMS   STNOKRF.
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
     MOVE    SPACE                TO   DSP-FST00401.
*    MOVE   "FST00401"            TO   DSP-FMT.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SYSYMD.
*システム時間転送
     MOVE    WK-TIME(1:6)         TO   DSP-SYSTIM.
*倉庫コードパラメタより倉庫コード入力チェック
     IF       PARA-DSOKO  =  "01"  OR  "88"
              MOVE    " "    TO   EDIT-STATUS OF DSP-SOKOS
              MOVE    " "    TO   EDIT-STATUS OF DSP-SOKOE
     ELSE
              MOVE PARA-SOKO TO   DSP-SOKOS  DSP-SOKOE
              MOVE    "X"    TO   EDIT-STATUS OF DSP-SOKOS
              MOVE    "X"    TO   EDIT-STATUS OF DSP-SOKOE
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
     CLOSE             DSPFILE  HTOKMS  STNOKRF.
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
*バッチ_（取引先）チェック
***  バッチ_（取引先）未入力チェック
     IF       DSP-TORICD  NOT NUMERIC
         OR   DSP-TORICD  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-TORICD
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-TORICD
              GO             TO   PARA-CHK-EXIT
     ELSE
***           取引先ＲＥＡＤ
              MOVE     DSP-TORICD TO   TOK-F01
              READ     HTOKMS
              INVALID
                MOVE   2     TO   ERR-FLG
                MOVE  "R"    TO   EDIT-OPTION  OF  DSP-TORICD
                MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-TORICD
                MOVE   SPACE TO   DSP-TORINM
                GO           TO   PARA-CHK-EXIT
              NOT INVALID
                MOVE  "M"    TO   EDIT-OPTION  OF  DSP-TORICD
                MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-TORICD
                MOVE  TOK-F03  TO DSP-TORINM
              END-READ
     END-IF.
*納品日範囲チェック
*    （開始納品日チェック）
     IF       DSP-NOUDTS  NOT NUMERIC
     OR       DSP-NOUDTS  =   ZERO
              MOVE     ZERO  TO   DSP-NOUDTS
     END-IF.
*    （終了納品日チェック）
     IF       DSP-NOUDTE  NOT NUMERIC
     OR       DSP-NOUDTE  =   ZERO
              MOVE  99999999 TO   DSP-NOUDTE
     END-IF.
*    開始納品日論理チェック
     IF       DSP-NOUDTS  NOT =  ZERO
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-NOUDTS     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE   9       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NOUDTS
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NOUDTS
                   GO             TO   PARA-CHK-EXIT
              ELSE
                   MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUDTS
                   MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NOUDTS
              END-IF
     END-IF.
*    終了納品日論理チェック
     IF       DSP-NOUDTE  NOT =  99999999
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-NOUDTE     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  10       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NOUDTE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NOUDTE
                   GO             TO   PARA-CHK-EXIT
              ELSE
                   MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUDTE
                   MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NOUDTE
              END-IF
     END-IF.
*納品日入力範囲チェック
     IF       DSP-NOUDTS  >  DSP-NOUDTE
              MOVE  11       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NOUDTS
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NOUDTE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NOUDTS
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUDTS
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUDTE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NOUDTS
     END-IF.
*
*倉庫範囲チェック
*    （開始倉庫ＣＤチェック）
     IF       DSP-SOKOS   =   SPACE
              MOVE     SPACE TO   DSP-SOKOS
     END-IF.
*    （終了倉庫ＣＤチェック）
     IF       DSP-SOKOE   =   SPACE
              MOVE  "99"     TO   DSP-SOKOE
     END-IF.
*倉庫入力範囲チェック
     IF       DSP-SOKOS   >  DSP-SOKOE
              MOVE  11       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SOKOS
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SOKOE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SOKOS
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SOKOS
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SOKOE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SOKOS
     END-IF.
*相手商品ＣＤチェック
*    （開始相手ＣＤチェック）
     IF       DSP-JANST  =   SPACE
              MOVE     SPACE TO   DSP-JANST
     END-IF.
*    （終了相手ＣＤチェック）
     IF       DSP-JANED  =   SPACE
              MOVE  ALL "9"  TO   DSP-JANED
     END-IF.
*相手商品ＣＤ入力範囲チェック
     IF       DSP-JANST  >  DSP-JANED
              MOVE  11       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JANST
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JANED
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JANST
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JANST
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JANED
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-JANST
     END-IF.
*ストックＮＯチェック
*    （開始ストックＮＯチェック）
     IF       DSP-STNOST  =   SPACE
              MOVE     SPACE TO   DSP-STNOST
     END-IF.
*    （終了ストックＮＯチェック）
     IF       DSP-STNOED  =   SPACE
              MOVE  ALL "9"  TO   DSP-STNOED
     END-IF.
*ストックＮＯ入力範囲チェック
     IF       DSP-STNOST  >  DSP-STNOED
              MOVE  11       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-STNOST
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-STNOED
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-STNOST
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-STNOST
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-STNOED
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-STNOST
     END-IF.
*ストックＮＯ入力チェック
     IF       DSP-STNOCK   NOT =    SPACE   AND   "1"
              MOVE   4       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-STNOCK
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-STNOCK
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-STNOCK
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-STNOCK
     END-IF.
*出荷数力チェック
     IF       DSP-SYUSUC   NOT =    SPACE   AND   "1"
              MOVE   5       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SYUSUC
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SYUSUC
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SYUSUC
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SYUSUC
     END-IF.
*対象データ存在チェック
     PERFORM SONZAI-CHK-SEC.
     IF  TAISYO-CNT  =  ZERO
              MOVE   12      TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-TORICD
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NOUDTS
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NOUDTE
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SOKOS
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SOKOE
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JANST
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JANED
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-STNOST
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-STNOED
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-STNOCK
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SYUSUC
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-TORICD
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-TORICD
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUDTS
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUDTE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SOKOS
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SOKOE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JANST
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JANED
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-STNOST
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-STNOED
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-STNOCK
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SYUSUC
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-TORICD
     END-IF.
*
 PARA-CHK-EXIT.
     EXIT.
****************************************************************
*             確認項目入力( PSW = 2 )                2.2       *
****************************************************************
 SONZAI-CHK-SEC       SECTION.
     MOVE     "SONZAI-CHK-SEC"   TO   S-NAME.
*
*ストックＮＯ管理ファイル存在チェック
     MOVE     ZERO           TO   TAISYO-CNT.
     MOVE     SPACE          TO   STN-REC.
     INITIALIZE                   STN-REC.
     MOVE     DSP-TORICD     TO   STN-F03.
     MOVE     DSP-NOUDTS     TO   STN-F05.
     MOVE     DSP-SOKOS      TO   STN-F04.
     START    STNOKRF   KEY  IS   >=   STN-F03
                                       STN-F05
                                       STN-F04
                                       STN-F06
                                       STN-F07
                                       STN-F08
        INVALID
              MOVE  "1"      TO   TAISYO-FLG
              GO             TO   SONZAI-CHK-EXIT
     END-START.
*
 SOKZAI-CHK-010.
     READ    STNOKRF
             AT  END
             GO              TO   SONZAI-CHK-EXIT
     END-READ.
*取引先ＣＤチェック
     IF  DSP-TORICD  NOT =  STN-F03
         GO                  TO   SONZAI-CHK-EXIT
     END-IF.
*納品日範囲チェック
     IF  DSP-NOUDTS  <=  STN-F05
     AND DSP-NOUDTE  >=  STN-F05
         CONTINUE
     ELSE
         GO                  TO   SONZAI-CHK-EXIT
     END-IF.
*倉庫ＣＤ範囲チェック        CONTINUE
     IF  DSP-SOKOS   <=  STN-F04
     AND DSP-SOKOE   >=  STN-F04
         CONTINUE
     ELSE
         GO                  TO   SOKZAI-CHK-010
     END-IF.
*相手商品ＣＤ範囲チェック        CONTINUE
     IF  DSP-JANST  <=  STN-F19
     AND DSP-JANED  >=  STN-F19
         CONTINUE
     ELSE
         GO                  TO   SOKZAI-CHK-010
     END-IF.
*ストックＮＯ範囲チェック        CONTINUE
     IF  DSP-STNOST  <=  STN-F30
     AND DSP-STNOED  >=  STN-F30
         GO                  TO   SOKZAI-CHK-020
     END-IF.
     IF  DSP-STNOST  <=  STN-F32
     AND DSP-STNOED  >=  STN-F32
         GO                  TO   SOKZAI-CHK-020
     END-IF.
     IF  DSP-STNOST  <=  STN-F34
     AND DSP-STNOED  >=  STN-F34
         GO                  TO   SOKZAI-CHK-020
     END-IF.
     IF  DSP-STNOST  <=  STN-F36
     AND DSP-STNOED  >=  STN-F36
         GO                  TO   SOKZAI-CHK-020
     END-IF.
     IF  DSP-STNOST  <=  STN-F38
     AND DSP-STNOED  >=  STN-F38
         CONTINUE
     ELSE
         GO                  TO   SOKZAI-CHK-010
     END-IF.
 SOKZAI-CHK-020.
*ストックＮＯ入力チェック
     IF  DSP-STNOCK  =  "1"
         IF  STN-F30  =  SPACE
         AND STN-F32  =  SPACE
         AND STN-F34  =  SPACE
         AND STN-F36  =  SPACE
         AND STN-F38  =  SPACE
             CONTINUE
         ELSE
             GO              TO   SOKZAI-CHK-010
         END-IF
     END-IF.
*出荷数チェック
     IF  DSP-SYUSUC  =  "1"
         COMPUTE WK-GK-SYUKASU =  STN-F31 + STN-F33 +  STN-F35 +
                 STN-F37 + STN-F39
         IF  WK-GK-SYUKASU NOT = STN-F14
             CONTINUE
         ELSE
             GO              TO   SOKZAI-CHK-010
         END-IF
     END-IF.
*
     ADD     1               TO   TAISYO-CNT.
*
 SONZAI-CHK-EXIT.
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
                MOVE    DSP-TORICD    TO    PARA-TOKCD
                MOVE    DSP-NOUDTS    TO    PARA-NOUDTS
                MOVE    DSP-NOUDTE    TO    PARA-NOUDTE
                MOVE    DSP-SOKOS     TO    PARA-SOKCDS
                MOVE    DSP-SOKOE     TO    PARA-SOKCDE
                MOVE    DSP-JANST     TO    PARA-JANCDS
                MOVE    DSP-JANED     TO    PARA-JANCDE
                MOVE    DSP-STNOST    TO    PARA-STNOS
                MOVE    DSP-STNOED    TO    PARA-STNOE
                MOVE    DSP-STNOCK    TO    PARA-KBN1
                MOVE    DSP-SYUSUC    TO    PARA-KBN2
                MOVE    "END"    TO   END-FLG
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
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.
*エラーメッセージセット
     IF    ERR-FLG   =    ZERO
           MOVE    SPACE              TO   DSP-MSG
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-MSG
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
***      パラメタ項目
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-GUIDE
***      確認
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-GUIDE
***      その他
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-GUIDE
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FST00401"          TO   DSP-FMT.
     WRITE    DSP-FST00401.
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
                MOVE    "GRP01"   TO   DSP-GRP
*確認
         WHEN   "2"
                MOVE    "KKNN"    TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FST00401"           TO   DSP-FMT.
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
***  バッチ_（取引先）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TORICD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TORICD.
***  開始納品日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NOUDTS.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NOUDTS.
***  終了納品日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NOUDTE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NOUDTE.
***  開始倉庫コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SOKOS.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SOKOS.
***  終了倉庫コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SOKOE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SOKOE.
***  開始相手商品ＣＤ
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JANST.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JANST.
***  終了相手商品ＣＤ
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JANED.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JANED.
***  開始ストックＮＯ
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-STNOST.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-STNOST.
***  終了ストックＮＯ
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-STNOED.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-STNOED.
***  ストックＮＯ入力チェック
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-STNOCK.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-STNOCK.
***  出荷数チェック
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SYUSUC.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SYUSUC.
*
 DSP-SYOKI-EXIT.
     EXIT.
*****************<<  SST0040I   END PROGRAM  >>******************
