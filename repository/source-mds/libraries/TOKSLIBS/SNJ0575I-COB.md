# SNJ0575I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNJ0575I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理サブシステム　　　　　　*
*    業務名　　　　　　　：　受信件数管理                      *
*    モジュール名　　　　：　受信件数リスト範囲入力            *
*    作成日／更新日　　　：　99/11/15                          *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　画面より，バッチ_を入力し，バッチ*
*                            _をパラメタに渡す。              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNJ0575I.
 AUTHOR.               HAGIWARA.
 DATE-WRITTEN.         99/11/15.
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
*売上伝票ファイル
     SELECT  JSMKENF   ASSIGN    TO        JSMKENL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KEN-F01
                                           KEN-F02
                                           KEN-F03
                                           KEN-F04
                       FILE      STATUS    KEN-ST.
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
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 受信件数マスタ                                     *
****************************************************************
 FD  JSMKENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMKENF   OF   XFDLIB
                       JOINING   KEN       AS   PREFIX.
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
                       COPY      FNJ05751  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KEN-ST                   PIC  X(02).
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
     03  READ-FLG                 PIC  X(03)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
***  バッチ_の売上伝票Ｆへの存在チェック
     03  KEN-FLG                  PIC  9(01)  VALUE  ZERO.
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
             VALUE NC"受信件数マスタに登録されていません".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  7   PIC   N(20).
*
 01  FILE-ERR.
     03  KEN-ERR           PIC N(20) VALUE
                     NC"受信件数マスタエラー".
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
**************************************************************
 LINKAGE               SECTION.
 01  LINK-JDATE            PIC  9(08).
 01  LINK-JTIME            PIC  9(04).
 01  LINK-TORICD           PIC  9(08).
**************************************************************
 PROCEDURE             DIVISION      USING     LINK-JDATE
                                               LINK-JTIME
                                               LINK-TORICD.
**************************************************************
 DECLARATIVES.
 KEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMKENF.
     DISPLAY     KEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
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
     OPEN     INPUT     JSMKENF   HTOKMS.
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
*売上伝票ファイル存在チェック
     PERFORM  KEN-START-SEC.
     IF       KEN-FLG   =    9
              MOVE      7    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTTIME
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTTORI
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-BTDATE
              MOVE           ZERO      TO    KEN-FLG
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "2"   TO    PSW
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-BTTIME
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-BTTORI
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-BTDATE
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
                MOVE    DSP-BTDATE   TO   LINK-JDATE
                MOVE    DSP-BTTIME   TO   LINK-JTIME
                MOVE    DSP-BTTORI   TO   LINK-TORICD
          DISPLAY "BTDATE = " LINK-JDATE  UPON CONS
          DISPLAY "BTTIME = " LINK-JTIME  UPON CONS
          DISPLAY "BTTORI = " LINK-TORICD UPON CONS
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
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-KAKU-SEC
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             受信件数マスタＳＴＡＲＴ                         *
****************************************************************
 KEN-START-SEC         SECTION.
     MOVE     "DEN-START-SEC"     TO   S-NAME.
*
     MOVE     DSP-BTDATE     TO   KEN-F01.
     MOVE     DSP-BTTIME     TO   KEN-F02.
     MOVE     DSP-BTTORI     TO   KEN-F03.
*****MOVE     ZERO           TO   KEN-F04.
     MOVE     SPACE          TO   KEN-F04.
     START    JSMKENF   KEY  IS   >=   KEN-F01 KEN-F02 KEN-F03
                                       KEN-F04
     INVALID
              MOVE      9    TO   KEN-FLG
     NOT INVALID
              PERFORM  KEN-READ-SEC
     END-START.
 KEN-START-EXIT.
     EXIT.
****************************************************************
*             受信件数マスタＲＥＡＤ                           *
****************************************************************
 KEN-READ-SEC          SECTION.
     MOVE     "KEN-READ-SEC"      TO   S-NAME.
*
     READ     JSMKENF       AT    END
              MOVE          9     TO   KEN-FLG
              GO                  TO   KEN-READ-EXIT
     END-READ.
     IF       KEN-F01   NOT =     DSP-BTDATE
          OR  KEN-F02   NOT =     DSP-BTTIME
          OR  KEN-F03   NOT =     DSP-BTTORI
              MOVE      9         TO   KEN-FLG
              GO                  TO   KEN-READ-EXIT
     END-IF.
*
 KEN-READ-EXIT.
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
     MOVE    "FNJ05751"          TO   DSP-FMT.
     WRITE    DSP-FNJ05751.
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
     MOVE    "FNJ05751"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FNJ05751.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
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
***  バッチ_（日付）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTDATE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTDATE.
***  バッチ_（時間）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTTIME.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTTIME.
***  バッチ_（取引先）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTTORI.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTTORI.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               6.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             JSMKENF HTOKMS DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SNJ0575I   END PROGRAM  >>******************

```
