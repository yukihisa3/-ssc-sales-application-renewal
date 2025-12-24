****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理システム　　　　　　　　　*
*    業務名　　　　　　　：　欠品リスト　　　　　　　　        *
*    モジュール名　　　　：　オンラインデータ欠品              *
*    作成日／更新日　　　：　05/10/06                          *
*    作成者／更新者　　　：　ＮＡＶ松野                        *
*    処理概要　　　　　　：　画面よりパラメタを入力する。　　　*
*    更新日／更新者　　　：　11/10/11 /YOSHIDA.M               *
*    更新概要　　　　　　：　基幹サーバ統合                    *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY0806I.
 AUTHOR.               HAGIWARA.
 DATE-WRITTEN.         05/10/06.
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
*売上伝票ファイル
     SELECT  SHTDENF   ASSIGN    TO        SHTDENLA
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       DEN-F46
                                           DEN-F47
                                           DEN-F01
                                           DEN-F48
                                           DEN-F02
                                           DEN-F04
***2011.10.11 ST
***                                        DEN-F132
                                           DEN-F051
                                           DEN-F07
                                           DEN-F112
***2011.10.11 EN
                                           DEN-F03
                       FILE      STATUS    DEN-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
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
*伝票データワーク(伝票単位順）
     SELECT  KPPFILL1  ASSIGN    TO        KPPFILL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       KP1-F46
                                           KP1-F47
                                           KP1-F01
                                           KP1-F48
                                           KP1-F02
                                           KP1-F04
                                           KP1-F132
                                           KP1-F07
                                           KP1-F112
                                           KP1-F03
                       FILE      STATUS    KP1-ST.
*伝票データワーク(商品単位順）
     SELECT  KPPFILL2  ASSIGN    TO        KPPFILL2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       KP2-F46
                                           KP2-F47
                                           KP2-F01
                                           KP2-F48
                                           KP2-F25
                                           KP2-F02
                                           KP2-F07
                                           KP2-F112
                                           KP2-F03
                       FILE      STATUS    KP2-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 売上伝票ファイル                     *
****************************************************************
 FD  SHTDENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SHTDENF   OF   XFDLIB
                       JOINING   DEN       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*  FILE= 倉庫マスタ                                          *
****************************************************************
 FD  ZSOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY08021  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
****************************************************************
*    FILE = 伝票データワーク（伝票単位順）                     *
****************************************************************
 FD  KPPFILL1
                       LABEL     RECORD    IS   STANDARD.
                       COPY      KPPFILL1  OF   XFDLIB
                       JOINING   KP1       AS   PREFIX.
****************************************************************
*    FILE = 伝票データワーク（商品単位順）                     *
****************************************************************
 FD  KPPFILL2
                       LABEL     RECORD    IS   STANDARD.
                       COPY      KPPFILL2  OF   XFDLIB
                       JOINING   KP2       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DEN-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
     03  KP1-ST                   PIC  X(02).
     03  KP2-ST                   PIC  X(02).
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
     03  END1-FLG                 PIC  9(01)  VALUE  ZERO.
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
***  明細ブレイク判定
     03  WK-DENNO                 PIC  9(09)  VALUE  ZERO.
***  倉庫ブレイク判定
     03  WK-SOKO                  PIC  X(02)  VALUE  ZERO.
***  ラインカウント
     03  L-CNT                    PIC  9(02)  VALUE  ZERO.
***  ページカウント
     03  P-CNT                    PIC  9(03)  VALUE  ZERO.
***  伝票_１枚目判定
     03  RD-SW                    PIC  9(01)  VALUE  ZERO.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
***  倉庫コード
     03  SOKCD                    PIC  X(02)  VALUE  ZERO.
***  欠品数量計（伝票毎）
     03  WK-KEPPINKEI1            PIC  9(09)  VALUE  ZERO.
***  欠品総原価計（伝票毎）
     03  WK-GENKAKEI1             PIC  9(09)  VALUE  ZERO.
***  欠品総売価計（伝票毎）
     03  WK-BAIKAKEI1             PIC  9(09)  VALUE  ZERO.
***  欠品数量計（総合計）
     03  WK-KEPPINKEI2            PIC  9(09)  VALUE  ZERO.
***  欠品総原価計（総合計）
     03  WK-GENKAKEI2             PIC  9(09)  VALUE  ZERO.
***  欠品総売価計（総合計）
     03  WK-BAIKAKEI2             PIC  9(09)  VALUE  ZERO.
***  インデックス
     03  IXA                      PIC  9(02)  VALUE  ZERO.
***  部門ＣＤ右詰用エリア
 01  WK-BUMON.
     03  WK-BUMON1           PIC  X(01)   OCCURS   4.
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
             VALUE NC"バッチ_を入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先マスタに登録されていません".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_（日付）論理エラー".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_（時間）論理エラー".
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
     03  DEN-ERR           PIC N(20) VALUE
                     NC"売上伝票ファイルエラー".
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  SOK-ERR           PIC N(20) VALUE
                        NC"倉庫マスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
     03  KP1-ERR           PIC N(20) VALUE
                        NC"欠品ファイルエラー１".
     03  KP2-ERR           PIC N(20) VALUE
                        NC"欠品ファイルエラー２".
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
 01  PARA-NOUDT1                 PIC  9(08).
 01  PARA-NOUDT2                 PIC  9(08).
 01  PARA-KBN                    PIC  9(01).
**************************************************************
 PROCEDURE             DIVISION  USING  PARA-SOKO   PARA-DSOKO
                                        PARA-NOUDT1 PARA-NOUDT2
                                        PARA-KBN.
**************************************************************
 DECLARATIVES.
 DEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     DEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
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
 KP1-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE KPPFILL1.
     DISPLAY     KP1-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KP1-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KP2-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE KPPFILL2.
     DISPLAY     KP2-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KP2-ST    UPON      CONS.
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
     OPEN     I-O       DSPFILE   KPPFILL1 KPPFILL2.
     OPEN     INPUT     SHTDENF   HTOKMS   ZSOKMS.
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
     MOVE    SPACE                TO   DSP-FSY08021.
*    MOVE   "FKY13011"            TO   DSP-FMT.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
*倉庫コードパラメタより倉庫コード入力チェック
     IF       PARA-DSOKO  =  "01"  OR  "88"
              MOVE    " "    TO   EDIT-STATUS OF DSP-SOKCD
     ELSE
              MOVE      PARA-SOKO TO   SOK-F01   DSP-SOKCD
              READ      ZSOKMS
                  INVALID  KEY
                        MOVE      ALL NC"＊" TO  DSP-SOKNM
              NOT INVALID  KEY
                        MOVE      SOK-F02    TO  DSP-SOKNM
              END-READ
              MOVE    "X"    TO   EDIT-STATUS OF DSP-SOKCD
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
*データスタート
         WHEN      "3"  PERFORM   DEN-START-SEC
*データ読込
         WHEN      "4"  PERFORM   DEN-READ-SEC
                        UNTIL     END1-FLG  = 9
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
     CLOSE             SHTDENF  HTOKMS  DSPFILE  ZSOKMS
                       KPPFILL1 KPPFILL2.
     IF      PROGRAM-STATUS NOT = 4010
             DISPLAY "### READ-CNT " READ-CNT UPON CONS
             DISPLAY "### SKIP-CNT " SKIP-CNT UPON CONS
             DISPLAY "### OUT -CNT " OUT-CNT UPON CONS
     END-IF.
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
*バッチ_（日付）チェック
***  バッチ_（日付）未入力チェック
     IF       DSP-BTDATE  NOT NUMERIC
         OR   DSP-BTDATE  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTDATE
              GO             TO   PARA-CHK-EXIT
     ELSE
***           バッチ_（日付）論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-BTDATE     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  4        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTDATE
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-BTDATE
     END-IF.
*
*バッチ_（時間）チェック
***  バッチ_（時間）未入力チェック
     IF       DSP-BTTIME  NOT NUMERIC
         OR   DSP-BTTIME  =   ZERO
                   MOVE   1       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTTIME
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTTIME
                   GO             TO   PARA-CHK-EXIT
     ELSE
***           バッチ_（時間）論理チェック
              MOVE     DSP-BTTIME    TO  WK-JIKAN
              IF       WK-HH  <=  ZERO  OR  WK-HH  >  24 OR
                       WK-MM  <   ZERO  OR  WK-MM  >  59
                       IF     ERR-FLG  =  ZERO
                              MOVE    5    TO    ERR-FLG
                       END-IF
                       MOVE "R"    TO EDIT-OPTION OF DSP-BTTIME
                       MOVE "C"    TO EDIT-CURSOR OF DSP-BTTIME
                       GO          TO PARA-CHK-EXIT
              ELSE
                       MOVE "M"    TO EDIT-OPTION OF DSP-BTTIME
                       MOVE SPACE  TO EDIT-CURSOR OF DSP-BTTIME
              END-IF
     END-IF.
*
*バッチ_（取引先）チェック
***  バッチ_（取引先）未入力チェック
     IF       DSP-BTTORI  NOT NUMERIC
         OR   DSP-BTTORI  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTTORI
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTTORI
              GO             TO   PARA-CHK-EXIT
     ELSE
***           取引先ＲＥＡＤ
              MOVE     DSP-BTTORI TO   TOK-F01
              READ     HTOKMS
              INVALID
                     MOVE   2     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-BTTORI
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-BTTORI
                     MOVE   SPACE TO   DSP-TORINM
                     GO           TO   PARA-CHK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-BTTORI
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-BTTORI
                     MOVE  TOK-F03  TO DSP-TORINM
              END-READ
     END-IF.
*
*    倉庫コードチェック
     IF     ( DSP-SOKCD NOT =  SPACE   )
     AND    ( DSP-SOKCD NOT =  "00"    )
              MOVE      DSP-SOKCD TO   SOK-F01
              READ      ZSOKMS
                  INVALID  KEY
                        MOVE      SPACE     TO   DSP-SOKNM
                        IF   ERR-FLG   =    ZERO
                             MOVE      8    TO   ERR-FLG
                        END-IF
                        MOVE  "C"  TO  EDIT-CURSOR OF DSP-SOKCD
                        MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKCD
              NOT INVALID  KEY
                        MOVE      SOK-F02   TO   DSP-SOKNM
                        MOVE      DSP-SOKCD TO   SOKCD
                        MOVE  "M"  TO  EDIT-CURSOR OF DSP-SOKCD
                        MOVE SPACE TO  EDIT-OPTION OF DSP-SOKCD
              END-READ
     ELSE
              MOVE      NC"全倉庫"   TO DSP-SOKNM
***  全件時，ＺＥＲＯから９９
              MOVE      ZERO         TO SOKCD
              MOVE      SPACE        TO DSP-SOKCD
              MOVE      "M"          TO EDIT-CURSOR OF DSP-SOKCD
              MOVE      SPACE        TO EDIT-OPTION OF DSP-SOKCD
     END-IF.
*納品日範囲チェック
*    （開始納品日チェック）
     IF       DSP-NOUDT1  NOT NUMERIC
     OR       DSP-NOUDT1  =   ZERO
              MOVE     ZERO  TO   DSP-NOUDT1
     END-IF.
*    （終了納品日チェック）
     IF       DSP-NOUDT2  NOT NUMERIC
     OR       DSP-NOUDT2  =   ZERO
              MOVE  99999999 TO   DSP-NOUDT2
     END-IF.
*    開始納品日論理チェック
     IF       DSP-NOUDT1  NOT =  ZERO
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-NOUDT1     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE   9       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NOUDT1
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NOUDT1
                   GO             TO   PARA-CHK-EXIT
              ELSE
                   MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUDT1
                   MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NOUDT1
              END-IF
     END-IF.
*    終了納品日論理チェック
     IF       DSP-NOUDT2  NOT =  99999999
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-NOUDT2     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  10       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NOUDT2
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NOUDT2
                   GO             TO   PARA-CHK-EXIT
              ELSE
                   MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUDT2
                   MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NOUDT2
              END-IF
     END-IF.
*納品日入力範囲チェック
     IF       DSP-NOUDT1  >  DSP-NOUDT2
              MOVE  11       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NOUDT1
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NOUDT2
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NOUDT1
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUDT2
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUDT2
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NOUDT1
     END-IF.
*部門コード右詰
*開始
*    IF  DSP-BUMON1    NOT =     SPACE
*        MOVE   DSP-BUMON1   TO   WK-BUMON
*        PERFORM   UNTIL  WK-BUMON1(4) NOT = SPACE
*            PERFORM VARYING IXA FROM  3 BY -1 UNTIL IXA = 0
*                  MOVE   WK-BUMON1(IXA) TO  WK-BUMON1(IXA + 1)
*            END-PERFORM
*            MOVE      ZERO      TO  WK-BUMON1(1)
*        END-PERFORM
*        MOVE      WK-BUMON      TO   DSP-BUMON1
*    ELSE
*        MOVE   "0000"           TO   DSP-BUMON1
*    END-IF.
*終了
*    IF  DSP-BUMON2    NOT =     SPACE
*        MOVE   DSP-BUMON2   TO   WK-BUMON
*        PERFORM   UNTIL  WK-BUMON1(4) NOT = SPACE
*            PERFORM VARYING IXA FROM  3 BY -1 UNTIL IXA = 0
*                  MOVE   WK-BUMON1(IXA) TO  WK-BUMON1(IXA + 1)
*            END-PERFORM
*            MOVE      ZERO      TO  WK-BUMON1(1)
*        END-PERFORM
*        MOVE      WK-BUMON      TO   DSP-BUMON2
*    ELSE
*        MOVE   "9999"           TO   DSP-BUMON2
*    END-IF.
     IF   DSP-BUMON2 = SPACE
         MOVE   "9999"           TO   DSP-BUMON2
     END-IF.
*部門入力範囲チェック
     IF       DSP-BUMON1  >  DSP-BUMON2
              MOVE  11       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BUMON1
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BUMON2
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BUMON1
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-BUMON2
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-BUMON2
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-BUMON1
     END-IF.
*伝票出力順未入力チェック
     IF       DSP-KBN    NOT =    1   AND   2
              MOVE   6       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-KBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-KBN
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-KBN
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-KBN
     END-IF.
*売上伝票存在チェック
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     DSP-BTDATE     TO   DEN-F46.
     MOVE     DSP-BTTIME     TO   DEN-F47.
     MOVE     DSP-BTTORI     TO   DEN-F01.
     MOVE     DSP-SOKCD      TO   DEN-F48.
***2011.10.11 ST
***  START    SHTDENF   KEY  IS   >=   DEN-F46
***                                    DEN-F47
***                                    DEN-F01
***                                    DEN-F48
     START    SHTDENF   KEY  IS   >=   DEN-F46
                                       DEN-F47
                                       DEN-F01
                                       DEN-F48
                                       DEN-F02
                                       DEN-F04
                                       DEN-F051
                                       DEN-F07
                                       DEN-F112
                                       DEN-F03
***2011.10.11 EN
        INVALID
              MOVE   7       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTTIME
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTTORI
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTDATE
        NOT INVALID
              IF     DSP-SOKCD NOT = SPACE  AND  "00"
                     MOVE   SOKCD    TO     DEN-F48
              END-IF
              READ  SHTDENF
                 AT END
                    MOVE   7   TO   ERR-FLG
                    MOVE  "R"  TO   EDIT-OPTION  OF  DSP-BTDATE
                    MOVE  "R"  TO   EDIT-OPTION  OF  DSP-BTTIME
                    MOVE  "R"  TO   EDIT-OPTION  OF  DSP-BTTORI
                    MOVE  "C"  TO   EDIT-CURSOR  OF  DSP-BTDATE
                    GO   TO    PARA-CHK-EXIT
                 NOT AT END
                  IF   DSP-SOKCD NOT = SPACE AND "00"
                    IF       DEN-F46     =    DSP-BTDATE
                    AND      DEN-F47     =    DSP-BTTIME
                    AND      DEN-F01     =    DSP-BTTORI
                    AND      DEN-F48     =    DSP-SOKCD
                             CONTINUE
                    ELSE
                    MOVE   7   TO   ERR-FLG
                    MOVE  "R"  TO   EDIT-OPTION  OF  DSP-BTDATE
                    MOVE  "R"  TO   EDIT-OPTION  OF  DSP-BTTIME
                    MOVE  "R"  TO   EDIT-OPTION  OF  DSP-BTTORI
                    MOVE  "R"  TO   EDIT-OPTION  OF  DSP-SOKCD
                    MOVE  "C"  TO   EDIT-CURSOR  OF  DSP-BTDATE
                    GO   TO    PARA-CHK-EXIT
                    END-IF
                  ELSE
                    IF       DEN-F46     =    DSP-BTDATE
                    AND      DEN-F47     =    DSP-BTTIME
                    AND      DEN-F01     =    DSP-BTTORI
                             CONTINUE
                    ELSE
                    MOVE   7   TO   ERR-FLG
                    MOVE  "R"  TO   EDIT-OPTION  OF  DSP-BTDATE
                    MOVE  "R"  TO   EDIT-OPTION  OF  DSP-BTTIME
                    MOVE  "R"  TO   EDIT-OPTION  OF  DSP-BTTORI
                    MOVE  "C"  TO   EDIT-CURSOR  OF  DSP-BTDATE
                    GO   TO    PARA-CHK-EXIT
                    END-IF
                  END-IF
              END-READ
     END-START.
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
                MOVE    DSP-NOUDT1    TO    PARA-NOUDT1
                MOVE    DSP-NOUDT2    TO    PARA-NOUDT2
                MOVE    DSP-KBN       TO    PARA-KBN
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
*             売上伝票ファイルＳＴＡＲＴ                       *
****************************************************************
 DEN-START-SEC         SECTION.
     MOVE     "DEN-START-SEC"     TO   S-NAME.
*
***2011.10.11 ST
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
***2011.10.11 EN
     MOVE     DSP-BTDATE     TO   DEN-F46.
     MOVE     DSP-BTTIME     TO   DEN-F47.
     MOVE     DSP-BTTORI     TO   DEN-F01.
     MOVE     DSP-SOKCD      TO   DEN-F48.
***2011.10.11 ST
***  START    SHTDENF   KEY  IS   >=   DEN-F46
***                                    DEN-F47
***                                    DEN-F01
***                                    DEN-F48
     START    SHTDENF   KEY  IS   >=   DEN-F46
                                       DEN-F47
                                       DEN-F01
                                       DEN-F48
                                       DEN-F02
                                       DEN-F04
                                       DEN-F051
                                       DEN-F07
                                       DEN-F112
                                       DEN-F03
***2011.10.11 EN
        INVALID
              MOVE   7       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTTIME
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTTORI
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTDATE
        NOT INVALID
              MOVE  "4"      TO   PSW
     END-START.
 DEN-START-EXIT.
     EXIT.
****************************************************************
*             伝票データ読込                                   *
****************************************************************
 DEN-READ-SEC          SECTION.
     MOVE     "DEN-READ-SEC"   TO   S-NAME.
*伝票データ読込
     READ     SHTDENF     AT   END
              MOVE        9    TO   END1-FLG
              MOVE     "END"   TO   END-FLG
              GO               TO   DEN-READ-EXIT
         NOT         AT   END
              ADD         1    TO   READ-CNT
     END-READ.
*バッチ範囲指定
     IF       DEN-F46     =    DSP-BTDATE
     AND      DEN-F47     =    DSP-BTTIME
     AND      DEN-F01     =    DSP-BTTORI
              CONTINUE
     ELSE
              ADD         1    TO   SKIP-CNT
              MOVE        9    TO   END1-FLG
              MOVE     "END"   TO   END-FLG
              GO               TO   DEN-READ-EXIT
     END-IF.
*倉庫コード　　
     IF       DSP-SOKCD   =    SPACE
     OR       DSP-SOKCD   =    "00"
              CONTINUE
     ELSE
              IF  DEN-F48  NOT =  DSP-SOKCD
                  ADD         1    TO   SKIP-CNT
******************MOVE        9    TO   END1-FLG
******************MOVE     "END"   TO   END-FLG
******************GO               TO   DEN-READ-EXIT
                  GO               TO   DEN-READ-SEC
              END-IF
     END-IF.
*納品日範囲指定
     IF       DEN-F112   >=    DSP-NOUDT1
     AND      DEN-F112   <=    DSP-NOUDT2
              CONTINUE
     ELSE
              ADD         1    TO   SKIP-CNT
              GO               TO   DEN-READ-SEC
     END-IF.
*部門範囲指定
     IF       DEN-F12    >=    DSP-BUMON1
     AND      DEN-F12    <=    DSP-BUMON2
              CONTINUE
     ELSE
              ADD         1    TO   SKIP-CNT
              GO               TO   DEN-READ-SEC
     END-IF.
     ADD      1          TO    OUT-CNT.
*出力順選択
     IF       DEN-F50   NOT =   DEN-F15
        IF       DSP-KBN          =    1
                 MOVE     DEN-REC   TO    KP1-REC
                 READ     KPPFILL1
                       INVALID  KEY
                          WRITE     KP1-REC
                 END-READ
        ELSE
                 MOVE     DEN-REC   TO    KP2-REC
                 READ     KPPFILL2
                       INVALID  KEY
                          WRITE     KP2-REC
                 END-READ
        END-IF
     END-IF.
 DEN-READ-EXIT.
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
     MOVE    "FSY08021"          TO   DSP-FMT.
     WRITE    DSP-FSY08021.
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
     MOVE    "FSY08021"           TO   DSP-FMT.
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
***  バッチ_（日付）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTDATE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTDATE.
***  バッチ_（時間）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTTIME.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTTIME.
***  バッチ_（取引先）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTTORI.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTTORI.
***  開始倉庫コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SOKCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SOKCD.
***  開始納品日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NOUDT1.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NOUDT1.
***  終了納品日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NOUDT2.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NOUDT2.
***  開始部門　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BUMON1.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BUMON1.
***  終了部門　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BUMON2.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BUMON2.
***  出力順　　　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KBN.
*
 DSP-SYOKI-EXIT.
     EXIT.
*****************<<  SSY0806I   END PROGRAM  >>******************
