# SSY0502I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY0502I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理サブシステム　　　　　　　*
*    業務名　　　　　　　：　出荷完了　　　　　　　　　　　    *
*    モジュール名　　　　：　出荷完了入力（単）　　　　　　    *
*    作成日／更新日　　　：　99/11/09                          *
*    作成者／更新者　　　：　ＮＡＶ萩原                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY0502I.
 AUTHOR.               HAGIWARA.
 DATE-WRITTEN.         99/11/09.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受信件数マスタ
     SELECT  JHMKENF   ASSIGN    TO        JHMKENL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
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
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 受信件数マスタ                                     *
****************************************************************
 FD  JHMKENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMKENF   OF   XFDLIB
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
*    FILE = 倉庫マスタ                                         *
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
                       COPY      FSY05021  OF   XMDLIB
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
     03  INV-FLG                  PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  READ-FLG                 PIC  X(03)  VALUE  SPACE.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SAKKBN-FLG               PIC  9(01)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*
*システム日付／時刻
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
             VALUE NC"バッチ_を正しく入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"キーが無効です".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_（日付）論理エラー".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_（時間）論理エラー".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先マスタに存在しません".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫コードを入力して下さい".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫マスタに存在しません".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"入力したバッチ_のデータは存在しません".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"受信時間を入力して下さい".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"受信時間論理エラー".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい値を入力して下さい".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  11  PIC   N(20).
 01  FILE-ERR.
     03  KEN-ERR           PIC N(20) VALUE
                        NC"受信件数マスタエラー".
     03  SOK-ERR           PIC N(20) VALUE
                        NC"倉庫マスタエラー".
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
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 KEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMKENF.
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
     OPEN     I-O       JHMKENF   DSPFILE.
     OPEN     INPUT     HTOKMS    ZSOKMS.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
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
*初期画面表示
         WHEN      "1"  PERFORM   DSP-INIT-SEC
*バッチ、倉庫ＣＤ入力
         WHEN      "2"  PERFORM   DSP-HEAD-SEC
*明細項目入力
         WHEN      "3"  PERFORM   DSP-BODY-SEC
*確認入力
         WHEN      "4"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             初期画面表示( PSW = 1 )                2.1       *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "DSP-INIT-SEC"      TO   S-NAME.
*画面の初期化
     MOVE    SPACE                TO   DSP-FSY05021.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
*
     MOVE    "2"                  TO   PSW.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                     2.1.1     *
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
***  倉庫コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SOKOCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SOKOCD.
***  受信時間
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JTIME.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JTIME.
***  伝票発行
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENCHK.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DENCHK.
***  発注集計表
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-HACCHK.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-HACCHK.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             キー項目入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-HEAD-SEC         SECTION.
     MOVE     "DSP-HEAD-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
*        WHEN   "F006"
*               MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   DSP-INIT-SEC
         WHEN   OTHER
                MOVE     2       TO   ERR-FLG
                GO       TO      DSP-HEAD-SEC
     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*             キー項目チェック                       2.2.1     *
****************************************************************
 HEAD-CHK-SEC         SECTION.
     MOVE     "HEAD-CHK-SEC"     TO   S-NAME.
*バッチ_（日付）チェック
***  バッチ_（日付）未入力チェック
     IF       DSP-BTDATE  NOT NUMERIC
         OR   DSP-BTDATE  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTDATE
              GO             TO   HEAD-CHK-EXIT
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
                   MOVE   3       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTDATE
                   GO             TO   HEAD-CHK-EXIT
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
                   GO             TO   HEAD-CHK-EXIT
     ELSE
***           バッチ_（時間）論理チェック
              MOVE     DSP-BTTIME    TO  WK-JIKAN
              IF       WK-HH  <=  ZERO  OR  WK-HH  >  24 OR
                       WK-MM  <   ZERO  OR  WK-MM  >  59
                       IF     ERR-FLG  =  ZERO
                              MOVE    4    TO    ERR-FLG
                       END-IF
                       MOVE "R"    TO EDIT-OPTION OF DSP-BTTIME
                       MOVE "C"    TO EDIT-CURSOR OF DSP-BTTIME
                       GO          TO HEAD-CHK-EXIT
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
              GO             TO   HEAD-CHK-EXIT
     ELSE
***           取引先ＲＥＡＤ
              MOVE     DSP-BTTORI TO   TOK-F01
              READ     HTOKMS
              INVALID
                     MOVE   5     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-BTTORI
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-BTTORI
                     MOVE   SPACE TO   DSP-TORINM
                     GO           TO   HEAD-CHK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-BTTORI
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-BTTORI
                     MOVE  TOK-F03  TO DSP-TORINM
              END-READ
     END-IF.
*倉庫コードチェック
***  倉庫コード未入力チェック
*****IF       DSP-SOKOCD  NOT NUMERIC
*****    OR   DSP-SOKOCD  =  ZERO
     IF       DSP-SOKOCD  =  SPACE
              MOVE   6       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SOKOCD
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SOKOCD
              GO             TO   HEAD-CHK-EXIT
     ELSE
***           倉庫マスタＲＥＡＤ
              MOVE     DSP-SOKOCD TO   SOK-F01
              READ     ZSOKMS
              INVALID
                     MOVE   7     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SOKOCD
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SOKOCD
                     MOVE   SPACE TO   DSP-SOKONM
                     GO           TO   HEAD-CHK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SOKOCD
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SOKOCD
                     MOVE  SOK-F02  TO DSP-SOKONM
              END-READ
     END-IF.
*
*出荷場所別件数ファイルチェック
     PERFORM  JHMKENF-READ.
     IF       INV-FLG   =    9
              MOVE      8    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTTIME
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTTORI
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SOKOCD
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-BTDATE
              MOVE           ZERO      TO    INV-FLG
              GO             TO   HEAD-CHK-EXIT
     ELSE
              MOVE     "3"   TO    PSW
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-BTTIME
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-BTTORI
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SOKOCD
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-BTDATE
     END-IF.
*明細部の初期表示を行う
*****PERFORM  JHMKENF-READ-SEC.
*****IF       READ-FLG NOT = "END"
              PERFORM  MOVE-DSP-SEC.
*****END-IF.
*
*****CLOSE    JHMKENF.
*****OPEN     I-O    JHMKENF.
*
 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*             明細項目　入力( PSW = 3 )              2.3       *
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE     "DSP-BODY-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   BODY-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "2"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
****************PERFORM   DSP-INIT-SEC
         WHEN   OTHER
                MOVE     2       TO   ERR-FLG
                GO       TO      DSP-BODY-SEC
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック                       2.3.1     *
****************************************************************
 BODY-CHK-SEC          SECTION.
     MOVE     "BODY-CHK-SEC"      TO   S-NAME.
*
*受信時間チェック
***  受信時間未入力チェック
     IF       DSP-JTIME  NOT NUMERIC
         OR   DSP-JTIME  =   ZERO
                   MOVE   9       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JTIME
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JTIME
                   GO             TO   BODY-CHK-EXIT
     ELSE
***           受信時間論理チェック
              MOVE     DSP-JTIME    TO  WK-JIKAN
              IF       WK-HH  <=  ZERO  OR  WK-HH  >  24 OR
                       WK-MM  <   ZERO  OR  WK-MM  >  59
                       IF     ERR-FLG  =  ZERO
                              MOVE    10   TO    ERR-FLG
                       END-IF
                       MOVE "R"    TO EDIT-OPTION OF DSP-JTIME
                       MOVE "C"    TO EDIT-CURSOR OF DSP-JTIME
                       GO          TO HEAD-CHK-EXIT
              ELSE
                       MOVE "M"    TO EDIT-OPTION OF DSP-JTIME
                       MOVE SPACE  TO EDIT-CURSOR OF DSP-JTIME
              END-IF
     END-IF.
*伝票発行チェック
     IF       DSP-DENCHK   NOT = SPACE
         AND  DSP-DENCHK   NOT = "Y"
                   MOVE   11      TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-DENCHK
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-DENCHK
                   GO             TO   BODY-CHK-EXIT
     ELSE
                   MOVE  "M"      TO   EDIT-OPTION  OF  DSP-DENCHK
                   MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-DENCHK
     END-IF.
*
*発注集計表チェック
     IF       DSP-HACCHK   NOT = SPACE
         AND  DSP-HACCHK   NOT = "Y"
                   MOVE   11      TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HACCHK
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-HACCHK
                   GO             TO   BODY-CHK-EXIT
     ELSE
                   MOVE  "M"      TO   EDIT-OPTION  OF  DSP-HACCHK
                   MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-HACCHK
     END-IF.
*
     MOVE     "4"       TO   PSW.
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
                PERFORM     FILE-WRT-SEC
                MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "3"       TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
         WHEN   OTHER
                MOVE     2       TO   ERR-FLG
                GO       TO      DSP-KAKU-SEC
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*    出荷場所別件数Ｆ　更新　　　　　　　　　　　2.4.1         *
****************************************************************
 FILE-WRT-SEC           SECTION.
     MOVE     "FILE-WRT-SEC"      TO   S-NAME.
*レコード初期クリア
*    MOVE     SPACE               TO   KEN-REC.
*    INITIALIZE                        KEN-REC.
*キー項目転送
***  バッチ_
*    MOVE     DSP-BTDATE          TO   KEN-F01.
*    MOVE     DSP-BTTIME          TO   KEN-F02.
*    MOVE     DSP-BTTORI          TO   KEN-F03.
***  倉庫コード
*    MOVE     DSP-SOKOCD          TO   KEN-F04.
*明細項目
***  受信時間
     MOVE     DSP-JTIME           TO   KEN-F07.
***  伝票発行
     IF       DSP-DENCHK     =   "Y"
              MOVE     1111                TO   KEN-F09
     ELSE
              MOVE     ZERO                TO   KEN-F09
     END-IF.
***  発注集計表
     IF       DSP-HACCHK     =   "Y"
              MOVE     1111                TO   KEN-F08
     ELSE
              MOVE     ZERO                TO   KEN-F08
     END-IF.
*
*****PERFORM  MOVE-JHMKENF.
*
     REWRITE  KEN-REC.
*
 FILE-WRT-EXIT.
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
*処理区分
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-PFGAID
*キー項目
         WHEN   "2"
                MOVE    PF-MSG-R(1)        TO   DSP-PFGAID
*明細項目
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
*確認
         WHEN   "4"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-PFGAID
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FSY05021"          TO   DSP-FMT.
     WRITE    DSP-FSY05021.
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
*キー項目
         WHEN   "2"
                MOVE    "KEY"     TO   DSP-GRP
*明細項目
         WHEN   "3"
                MOVE    "BODY"    TO   DSP-GRP
*確認
         WHEN   "4"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FSY05021"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             項目転送  （出荷場所別件数Ｆ　 → 画面）
****************************************************************
 MOVE-DSP-SEC           SECTION.
*    MOVE     "MOVE-DSP-SEC"      TO   S-NAME.
*
*受信時間
     MOVE    KEN-F07              TO   DSP-JTIME.
*伝票発行
     IF      KEN-F09  = 1111
             MOVE       "Y"       TO   DSP-DENCHK
     ELSE
             MOVE       SPACE     TO   DSP-DENCHK
     END-IF.
*発注集計表
     IF      KEN-F08  = 1111
             MOVE       "Y"       TO   DSP-HACCHK
     ELSE
             MOVE       SPACE     TO   DSP-HACCHK
     END-IF.
*
 MOVE-DSP-EXIT.
     EXIT.
****************************************************************
*             出荷場所別件数Ｆ　　ＲＥＡＤ　                   *
****************************************************************
 JHMKENF-READ             SECTION.
     MOVE     "JHMKENF-READ"      TO   S-NAME.
*
     MOVE     ZERO                TO   INV-FLG.
     MOVE     DSP-BTDATE          TO   KEN-F01.
     MOVE     DSP-BTTIME          TO   KEN-F02.
     MOVE     DSP-BTTORI          TO   KEN-F03.
     MOVE     DSP-SOKOCD          TO   KEN-F04.
     READ     JHMKENF
        INVALID
              MOVE      9         TO   INV-FLG
     END-READ.
 JHMKENF-READ-EXIT.
     EXIT.
****************************************************************
*             出荷場所別件数Ｆ　　ＲＥＡＤ                     *
****************************************************************
*JHMKENF-READ             SECTION.
*    MOVE     "JHMKENF-READ"      TO   S-NAME.
*
*    MOVE    ZERO                 TO   READ-FLG　.
*    MOVE    DSP-BTDATE
*出荷場所別件数Ｆ　ＲＥＡＤ
*    READ    JHMKENF    AT   END
*            MOVE      "END"      TO   READ-FLG
*            MOVE      "1"        TO   PSW
*    END-READ.
*ブレイク判定
*    IF      DSP-BTDATE      NOT  =    KEN-F01
*       OR   DSP-BTTIME      NOT  =    KEN-F02
*       OR   DSP-BTTORI      NOT  =    KEN-F03
*       OR   DSP-SOKOCD      NOT  =    KEN-F04
*            MOVE      "END"      TO   READ-FLG
*            MOVE      "1"        TO   PSW
*    END-FLG.
*
*JHMKENF-READ-EXIT.
*    EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             JHMKENF  HTOKMS  ZSOKMS  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SJH0016I   END PROGRAM  >>******************

```
