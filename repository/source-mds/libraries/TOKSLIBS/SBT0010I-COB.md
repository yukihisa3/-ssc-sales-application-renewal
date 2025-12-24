# SBT0010I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBT0010I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷連携サブシステム　　　　　　　*
*    業務名　　　　　　　：　出荷連携データ抽出指示            *
*    モジュール名　　　　：　出荷連携データ抽出指示　　        *
*    作成日／更新日　　　：　12/10/02                          *
*    作成者／更新者　　　：　MIURA                             *
*    処理概要　　　　　　：　出荷連携データの抽出条件指定　　　*
*    更新日／更新者　　　：　　　　　　　　　　　　　　　      *
*    修正概要　　　　　　：　　　　　　　　                    *
*                        ：　　　　　　　　　　　　　　        *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SBT0010I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         12/10/02.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
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
*倉庫マスタ
     SELECT  ZSOKMS    ASSIGN    TO        ZSOKMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SOK-F01
                       FILE      STATUS    SOK-ST.
*担当者マスタ
     SELECT  HTANMS    ASSIGN    TO        TANMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TAN-F01 TAN-F02
                       FILE      STATUS    TAN-ST.
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
*  FILE= 取引先マスタ                                        *
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
*  FILE= 担当者マスタ                                        *
****************************************************************
 FD  HTANMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTANMS    OF   XFDLIB
                       JOINING   TAN       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FBT00101  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TAN-ST                   PIC  X(02).
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
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
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
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08).
*
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.
*
*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY          PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM            PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD            PIC   9(02)  VALUE  ZERO.
*取引先チェック
 01  WK-TORICD.
     03  WK-TORI                  PIC   9(08)  VALUE  ZERO.
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
         05  FILLER              PIC   N(30)
             VALUE NC"送信区分が誤りです".
     03  ERR-MSG2.
         05  FILLER              PIC   N(30)
          VALUE NC"抽出区分の何れかに（Ｙ）を入力して下さい".
     03  ERR-MSG3.
         05  FILLER              PIC   N(30)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(30)
             VALUE NC"日付を入力して下さい".
     03  ERR-MSG5.
         05  FILLER              PIC   N(30)
             VALUE NC"送信区分が誤りです".
     03  ERR-MSG6.
         05  FILLER              PIC   N(30)
             VALUE NC"倉庫コードを入力してください".
     03  ERR-MSG7.
         05  FILLER              PIC   N(30)
             VALUE NC"倉庫マスタに登録されていません".
     03  ERR-MSG8.
         05  FILLER              PIC   N(30)
             VALUE NC"横持日を入力してください".
     03  ERR-MSG9.
         05  FILLER              PIC   N(30)
 VALUE NC"条件に誤りがないか確認し、ＥＮＴＥＲを実行して下さい".
     03  ERR-MSG10.
         05  FILLER              PIC   N(30)
         VALUE NC"抽出区分に誤りがあります".
     03  ERR-MSG11               PIC  N(30)  VALUE
              NC"バッチ_を入力して下さい。".
     03  ERR-MSG12               PIC  N(30)  VALUE
              NC"バッチ_に誤りがあります。".
     03  ERR-MSG13               PIC  N(30)  VALUE
              NC"取引先コードが違います。".
     03  ERR-MSG14           PIC  N(30)  VALUE
              NC"伝票ＮＯが誤りです".
     03  ERR-MSG15           PIC  N(30)  VALUE
              NC"店舗が誤りです".
     03  ERR-MSG16           PIC  N(30)  VALUE
              NC"開始が終了を越えています。".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  16  PIC   N(30).
*
 01  FILE-ERR.
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  TAN-ERR           PIC N(20) VALUE
                        NC"担当者マスタエラー".
     03  SOK-ERR           PIC N(20) VALUE
                        NC"倉庫マスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
 01  JYOKN-MSG.
     03  CHU-MSG           PIC N(04) VALUE
                        NC"抽出対象".
     03  SAI-MSG           PIC N(04) VALUE
                        NC"再送対象".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  LINK-IN-BMNCD           PIC  X(04).
 01  LINK-IN-TANCD           PIC  X(02).
 01  LINK-IN-SOKO            PIC  X(02).
 01  LINK-IN-DSOKO           PIC  X(02).
 01  LINK-OUT-BMNCD          PIC  X(04).
 01  LINK-OUT-TANCD          PIC  X(02).
 01  LINK-OUT-SNDKBN         PIC  X(01).
 01  LINK-OUT-CHKBNO         PIC  X(01).
 01  LINK-OUT-CHKBNT         PIC  X(01).
 01  LINK-OUT-CHKBNY         PIC  X(01).
 01  LINK-OUT-SOKO           PIC  X(02).
 01  LINK-OUT-BTBI           PIC  X(08).
 01  LINK-OUT-BTJKK          PIC  X(04).
 01  LINK-OUT-BTTORI         PIC  X(08).
 01  LINK-OUT-ONOBIF         PIC  X(08).
 01  LINK-OUT-ONOBIT         PIC  X(08).
 01  LINK-OUT-TENPOF         PIC  X(05).
 01  LINK-OUT-TENPOT         PIC  X(05).
 01  LINK-OUT-TORICD         PIC  X(08).
 01  LINK-OUT-DENNOF         PIC  X(09).
 01  LINK-OUT-DENNOT         PIC  X(09).
 01  LINK-OUT-TNOBIF         PIC  X(08).
 01  LINK-OUT-TNOBIT         PIC  X(08).
 01  LINK-OUT-YKMCBI         PIC  X(08).
*
**************************************************************
 PROCEDURE             DIVISION   USING  LINK-IN-BMNCD
                       LINK-IN-TANCD LINK-IN-SOKO LINK-IN-DSOKO
                       LINK-OUT-BMNCD  LINK-OUT-TANCD
                       LINK-OUT-SNDKBN LINK-OUT-CHKBNO
                       LINK-OUT-CHKBNT LINK-OUT-CHKBNY
                       LINK-OUT-SOKO   LINK-OUT-BTBI
                       LINK-OUT-BTJKK  LINK-OUT-BTTORI
                       LINK-OUT-ONOBIF LINK-OUT-ONOBIT
                       LINK-OUT-TENPOF LINK-OUT-TENPOT
                       LINK-OUT-TORICD
                       LINK-OUT-DENNOF LINK-OUT-DENNOT
                       LINK-OUT-TNOBIF LINK-OUT-TNOBIT
                       LINK-OUT-YKMCBI.
**************************************************************
 DECLARATIVES.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TAN-ST    UPON      CONS.
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
     ACCEPT    WK-TIME          FROM   TIME.
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     HTANMS   ZSOKMS HTOKMS.
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
*オンライン条件入力
         WHEN      "2"  PERFORM   DSP-ONLN-SEC
*手書条件入力
         WHEN      "3"  PERFORM   DSP-TEGK-SEC
*横持条件入力
         WHEN      "4"  PERFORM   DSP-YKMC-SEC
*確認入力
         WHEN      "9"  PERFORM   DSP-KAKU-SEC
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
*             オンライン　( PSW = 2 )                2.2       *
****************************************************************
 DSP-ONLN-SEC         SECTION.
     MOVE     "DSP-ONLN-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK2-SEC
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
                MOVE     3       TO   ERR-FLG
                GO       TO      DSP-ONLN-SEC
     END-EVALUATE.
*
 DSP-ONLN-EXIT.
     EXIT.
****************************************************************
*             手書入力( PSW = 3 )                2.3       *
****************************************************************
 DSP-TEGK-SEC         SECTION.
     MOVE     "DSP-TEGK-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK3-SEC
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
                MOVE     3       TO   ERR-FLG
                GO       TO      DSP-TEGK-SEC
     END-EVALUATE.
*
 DSP-TEGK-EXIT.
     EXIT.
****************************************************************
*             横持ち　　　( PSW = 4 )                2.4       *
****************************************************************
 DSP-YKMC-SEC         SECTION.
     MOVE     "DSP-YKMC-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK4-SEC
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
                MOVE     3       TO   ERR-FLG
                GO       TO      DSP-YKMC-SEC
     END-EVALUATE.
*
 DSP-YKMC-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック                       2.1.1     *
****************************************************************
 PARA-CHK-SEC             SECTION.
     MOVE     "PARA-CHK-SEC"     TO   S-NAME.
*
*送信区分チェック
     IF       DSP-SNDKBN   = " "
         MOVE    "新規"   TO   DSP-SKBNM
         MOVE    SPACE    TO   DSP-YKMCBI(1:8)
         MOVE    "M"      TO   EDIT-OPTION  OF  DSP-YKMCBI
     ELSE
         IF  DSP-SNDKBN   = "1"
             MOVE    "再送"   TO  DSP-SKBNM
         ELSE
             MOVE  1        TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SNDKBN
             MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SNDKBN
             GO             TO   PARA-CHK-EXIT
         END-IF
     END-IF.
     MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SNDKBN.
     MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SNDKBN.
*抽出区分（オン）チェック
     IF  DSP-CHKBNO = "Y"
         MOVE  "M"      TO   EDIT-OPTION  OF  DSP-CHKBNO
         MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-CHKBNO
     ELSE
         IF  DSP-CHKBNO   = " "
             MOVE  "M"      TO   EDIT-OPTION  OF  DSP-CHKBNO
             MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-CHKBNO
         ELSE
             MOVE  10       TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-CHKBNO
             MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-CHKBNO
             GO             TO   PARA-CHK-EXIT
         END-IF
     END-IF.
*抽出区分（手書）チェック
     IF  DSP-CHKBNT = "Y"
         MOVE  "M"      TO   EDIT-OPTION  OF  DSP-CHKBNT
         MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-CHKBNT
     ELSE
         IF  DSP-CHKBNT   = " "
             MOVE  "M"      TO   EDIT-OPTION  OF  DSP-CHKBNT
             MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-CHKBNT
         ELSE
             MOVE  10       TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-CHKBNT
             MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-CHKBNT
             GO             TO   PARA-CHK-EXIT
         END-IF
     END-IF.
*抽出区分（横持）チェック
     IF  DSP-CHKBNY  = "Y"
         MOVE  "M"      TO   EDIT-OPTION  OF  DSP-CHKBNY
         MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-CHKBNY
     ELSE
         IF  DSP-CHKBNY  = " "
             IF  DSP-CHKBNO  = " "  AND  DSP-CHKBNT  = " "
                 MOVE  2        TO   ERR-FLG
                 MOVE  "R"      TO   EDIT-OPTION  OF  DSP-CHKBNO
                 MOVE  "R"      TO   EDIT-OPTION  OF  DSP-CHKBNY
                 MOVE  "R"      TO   EDIT-OPTION  OF  DSP-CHKBNT
                 MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-CHKBNO
                 GO             TO   PARA-CHK-EXIT
             ELSE
                 MOVE  "M"      TO   EDIT-OPTION  OF  DSP-CHKBNY
                 MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-CHKBNY
             END-IF
         ELSE
             MOVE  10       TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-CHKBNY
             MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-CHKBNY
             GO             TO   PARA-CHK-EXIT
         END-IF
     END-IF.
*
*抽出倉庫コードチェック
     IF   DSP-SOKOCD   =   " "
          MOVE   6     TO   ERR-FLG
          MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SOKOCD
          MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SOKOCD
          GO           TO   PARA-CHK-EXIT
     ELSE
***  倉庫マスタＲＥＡＤ
          MOVE      DSP-SOKOCD     TO   SOK-F01
          READ      ZSOKMS
          INVALID
                 MOVE   7     TO   ERR-FLG
                 MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SOKOCD
                 MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SOKOCD
                 GO           TO   PARA-CHK-EXIT
          NOT INVALID
                 MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SOKOCD
                 MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SOKOCD
                 MOVE  SOK-F02 TO   DSP-CHUSKN
          END-READ
     END-IF.
*
     IF  DSP-CHKBNY  = "Y"
         MOVE    "4"       TO    PSW
         IF   DSP-SNDKBN = "1"
              MOVE  SAI-MSG   TO   DSP-YJYOKN
         ELSE
              MOVE  CHU-MSG   TO   DSP-YJYOKN
              MOVE    "9"       TO    PSW
              MOVE    SPACE    TO  DSP-YKMCBI(1:8)
         END-IF
     ELSE
         MOVE    SPACE    TO  DSP-YKMCBI(1:8)
         MOVE    SPACE    TO  DSP-YJYOKN
         MOVE    "M"      TO  EDIT-OPTION  OF  DSP-YKMCBI
     END-IF.
     IF  DSP-CHKBNT  = "Y"
         MOVE    "3"       TO    PSW
         IF   DSP-SNDKBN = "1"
              MOVE  SAI-MSG  TO   DSP-TJYOKN
         ELSE
              MOVE  CHU-MSG  TO   DSP-TJYOKN
         END-IF
     ELSE
         MOVE    SPACE    TO  DSP-TORICD(1:8)
         MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TORICD
         MOVE    SPACE    TO  DSP-TTORIN
         MOVE    SPACE    TO  DSP-TNOBIF(1:8)
         MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TNOBIF
         MOVE    SPACE    TO  DSP-TNOBIT(1:8)
         MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TNOBIT
         MOVE    SPACE    TO  DSP-DENNOF(1:9)
         MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENNOF
         MOVE    SPACE    TO  DSP-DENNOT(1:9)
         MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENNOT
         MOVE    SPACE    TO  DSP-TJYOKN
     END-IF.
     IF  DSP-CHKBNO  = "Y"
         MOVE    "2"       TO    PSW
         IF   DSP-SNDKBN = "1"
              MOVE  SAI-MSG  TO   DSP-OJYOKN
         ELSE
              MOVE  CHU-MSG  TO   DSP-OJYOKN
         END-IF
     ELSE
         MOVE    SPACE    TO  DSP-BTBI(1:8)
         MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTBI
         MOVE    SPACE    TO  DSP-BTJKK(1:4)
         MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTJKK
         MOVE    SPACE    TO  DSP-BTTORI(1:8)
         MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTTORI
         MOVE    SPACE    TO  DSP-OTORIN
         MOVE    SPACE    TO  DSP-ONOBIF(1:8)
         MOVE    "M"      TO  EDIT-OPTION  OF  DSP-ONOBIF
         MOVE    SPACE    TO  DSP-ONOBIT(1:8)
         MOVE    "M"      TO  EDIT-OPTION  OF  DSP-ONOBIT
         MOVE    SPACE    TO  DSP-TENPOF(1:5)
         MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TENPOF
         MOVE    SPACE    TO  DSP-TENPOT(1:5)
         MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TENPOT
         MOVE    SPACE    TO  DSP-OJYOKN
     END-IF.
*    MOVE     9      TO   ERR-FLG
*
 PARA-CHK-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック２                     2.1.2     *
****************************************************************
 PARA-CHK2-SEC             SECTION.
     MOVE     "PARA-CHK2-SEC"     TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
     IF     ( DSP-BTBI    =    ZERO ) AND
            ( DSP-BTJKK   =    ZERO ) AND
            ( DSP-BTTORI  =    ZERO )
              MOVE      11        TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF DSP-BTBI
              MOVE     "R"        TO   EDIT-OPTION OF DSP-BTBI
              MOVE     "R"        TO   EDIT-OPTION OF DSP-BTJKK
              MOVE     "R"        TO   EDIT-OPTION OF DSP-BTTORI
              GO   TO   PARA-CHK2-EXIT
     END-IF.
*    受信日付チェック
     IF  DSP-BTBI  NOT =     ZERO
         MOVE     "2"        TO        LINK-IN-KBN
         MOVE      DSP-BTBI  TO        LINK-IN-YMD8
         CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         IF   LINK-OUT-RET   NOT =     ZERO
              IF        ERR-FLG   =    ZERO
                        MOVE      4   TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF DSP-BTBI
              MOVE     "R"        TO   EDIT-OPTION OF DSP-BTBI
              GO   TO   PARA-CHK2-EXIT
         ELSE
              MOVE     SPACE      TO   EDIT-CURSOR OF DSP-BTBI
              MOVE     "M"        TO   EDIT-OPTION OF DSP-BTBI
         END-IF
     END-IF.
*    受信時間
     IF  DSP-BTJKK NOT NUMERIC
         MOVE      ZERO      TO   DSP-BTJKK
     ELSE
         MOVE     SPACE      TO   EDIT-CURSOR OF DSP-BTJKK
         MOVE     "M"        TO   EDIT-OPTION OF DSP-BTJKK
     END-IF.
     IF  DSP-BTTORI NOT =     ZERO
         MOVE      SPACE     TO   TOK-REC
         INITIALIZE               TOK-REC
         MOVE      DSP-BTTORI  TO   TOK-F01
         READ      HTOKMS
             INVALID
                   MOVE      SPACE     TO   DSP-OTORIN
                   IF   ERR-FLG   =    ZERO
                        MOVE      13   TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF DSP-BTTORI
                   MOVE     "R"   TO   EDIT-OPTION OF DSP-BTTORI
                   GO   TO   PARA-CHK2-EXIT
             NOT INVALID
                   MOVE      TOK-F02   TO   DSP-OTORIN
                   MOVE     SPACE TO   EDIT-CURSOR OF DSP-BTTORI
                   MOVE     "M"   TO   EDIT-OPTION OF DSP-BTTORI
         END-READ
     END-IF.
*開始納品日未入力時（初期値セット）
     IF  DSP-ONOBIF  NOT  NUMERIC
              MOVE     ZERO  TO   DSP-ONOBIF
     END-IF.
*終了納品日未入力時（初期値セット）
     IF  DSP-ONOBIT  NOT  NUMERIC
              MOVE  99999999 TO   DSP-ONOBIT
     END-IF.
*納品日範囲大小チェック
     IF   DSP-ONOBIF  >  DSP-ONOBIT
              IF    ERR-FLG = ZERO
                    MOVE     16    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF DSP-ONOBIF
              MOVE     "R"   TO   EDIT-OPTION OF DSP-ONOBIF
              MOVE     "R"   TO   EDIT-OPTION OF DSP-ONOBIT
              GO   TO   PARA-CHK2-EXIT
     ELSE
              MOVE     SPACE TO   EDIT-CURSOR OF DSP-ONOBIF
              MOVE     "M"   TO   EDIT-OPTION OF DSP-ONOBIF
              MOVE     "M"   TO   EDIT-OPTION OF DSP-ONOBIT
     END-IF.
*日付論理チェック（開始納品日）
     IF  DSP-ONOBIF  NOT =  ZERO
              MOVE    "2"        TO        LINK-IN-KBN
              MOVE  DSP-ONOBIF   TO        LINK-IN-YMD8
              CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD
              IF       LINK-OUT-RET   =    ZERO
                       MOVE SPACE  TO   EDIT-CURSOR OF DSP-ONOBIF
                       MOVE "M"    TO   EDIT-OPTION OF DSP-ONOBIF
              ELSE
                       IF  ERR-FLG = ZERO
                           MOVE      4    TO   ERR-FLG
                       END-IF
                       MOVE "C"    TO   EDIT-CURSOR OF DSP-ONOBIF
                       MOVE "R"    TO   EDIT-OPTION OF DSP-ONOBIF
                       GO   TO   PARA-CHK2-EXIT
              END-IF
     END-IF.
*日付論理チェック（終了納品日）
     IF   DSP-ONOBIT  NOT =  99999999
              MOVE    "2"        TO        LINK-IN-KBN
              MOVE   DSP-ONOBIT  TO        LINK-IN-YMD8
              CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD
              IF       LINK-OUT-RET   =    ZERO
                       MOVE SPACE  TO   EDIT-CURSOR OF DSP-ONOBIT
                       MOVE "M"    TO   EDIT-OPTION OF DSP-ONOBIT
              ELSE
                       IF  ERR-FLG = ZERO
                           MOVE      4    TO   ERR-FLG
                       END-IF
                       MOVE "C"    TO   EDIT-CURSOR OF DSP-ONOBIT
                       MOVE "R"    TO   EDIT-OPTION OF DSP-ONOBIT
                       GO   TO   PARA-CHK2-EXIT
              END-IF
     END-IF.
*開始店舗未入力時（初期値セット）
     IF  DSP-TENPOF  NOT  NUMERIC
          MOVE  ZERO     TO   DSP-TENPOF
     END-IF.
*終了店舗未入力時（初期値セット）
     IF  DSP-TENPOT  NOT  NUMERIC
           MOVE  99999    TO   DSP-TENPOT
     END-IF.
*店舗範囲大小チェック
     IF   DSP-TENPOF  >  DSP-TENPOT
              IF    ERR-FLG = ZERO
                    MOVE     16    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF DSP-TENPOF
              MOVE     "R"   TO   EDIT-OPTION OF DSP-TENPOF
              MOVE     "R"   TO   EDIT-OPTION OF DSP-TENPOF
              GO   TO   PARA-CHK2-EXIT
     ELSE
              MOVE     SPACE TO   EDIT-CURSOR OF DSP-TENPOF
              MOVE     "M"   TO   EDIT-OPTION OF DSP-TENPOF
              MOVE     "M"   TO   EDIT-OPTION OF DSP-TENPOT
     END-IF.
     IF  DSP-CHKBNT  = "Y"
         MOVE    "3"       TO    PSW
         GO   TO   PARA-CHK2-EXIT
     END-IF.
     IF  DSP-CHKBNY  = "Y"   AND DSP-SNDKBN = "1"
         MOVE    "4"       TO    PSW
         GO   TO   PARA-CHK2-EXIT
     END-IF.
     MOVE    "9"     TO    PSW
     MOVE     9      TO    ERR-FLG
 PARA-CHK2-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック３                     2.1.2     *
****************************************************************
 PARA-CHK3-SEC             SECTION.
     MOVE     "PARA-CHK3-SEC"     TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*    取引先チェック
     IF  DSP-TORICD NOT =     ZERO
         MOVE      SPACE     TO   TOK-REC
         INITIALIZE               TOK-REC
         MOVE      DSP-TORICD  TO   TOK-F01
         READ      HTOKMS
             INVALID
                   MOVE      SPACE     TO   DSP-TTORIN
                   IF   ERR-FLG   =    ZERO
                        MOVE      13   TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF DSP-TORICD
                   MOVE     "R"   TO   EDIT-OPTION OF DSP-TORICD
                   GO   TO   PARA-CHK3-EXIT
             NOT INVALID
                   MOVE      TOK-F02   TO   DSP-TTORIN
                   MOVE    SPACE  TO   EDIT-CURSOR OF DSP-TORICD
                   MOVE    "M"    TO   EDIT-OPTION OF DSP-TORICD
         END-READ
     END-IF.
*開始伝票未入力時（初期値セット）
     IF  DSP-DENNOF  NOT  NUMERIC
          MOVE  ZERO     TO   DSP-DENNOF
     END-IF.
*終了伝票未力時（初期値セット）
     IF  DSP-DENNOT  NOT  NUMERIC
           MOVE  999999999 TO   DSP-DENNOT
     END-IF.
*伝票範囲大小チェック
     IF    DSP-DENNOF >  DSP-DENNOT
              IF    ERR-FLG = ZERO
                    MOVE     16    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF DSP-DENNOF
              MOVE     "R"   TO   EDIT-OPTION OF DSP-DENNOF
              MOVE     "R"   TO   EDIT-OPTION OF DSP-DENNOT
              GO   TO   PARA-CHK3-EXIT
     ELSE
              MOVE     SPACE TO   EDIT-CURSOR OF DSP-DENNOF
              MOVE     "M"   TO   EDIT-OPTION OF DSP-DENNOF
              MOVE     "M"   TO   EDIT-OPTION OF DSP-DENNOT
     END-IF.
*開始納品日未入力時（初期値セット）
     IF  DSP-TNOBIF  NOT  NUMERIC
              MOVE     ZERO  TO   DSP-TNOBIF
     END-IF.
*終了納品日未入力時（初期値セット）
     IF  DSP-TNOBIT  NOT  NUMERIC
              MOVE  99999999 TO   DSP-TNOBIT
     END-IF.
*納品日範囲大小チェック
     IF   DSP-TNOBIF  >  DSP-TNOBIT
              IF    ERR-FLG = ZERO
                    MOVE     16    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF DSP-TNOBIF
              MOVE     "R"   TO   EDIT-OPTION OF DSP-TNOBIF
              MOVE     "R"   TO   EDIT-OPTION OF DSP-TNOBIT
              GO   TO   PARA-CHK3-EXIT
     ELSE
              MOVE     SPACE TO   EDIT-CURSOR OF DSP-TNOBIF
              MOVE     "M"   TO   EDIT-OPTION OF DSP-TNOBIF
              MOVE     "M"   TO   EDIT-OPTION OF DSP-TNOBIT
     END-IF.
*日付論理チェック（開始納品日）
     IF  DSP-TNOBIF  NOT =  ZERO
              MOVE    "2"        TO        LINK-IN-KBN
              MOVE  DSP-TNOBIF   TO        LINK-IN-YMD8
              CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD
              IF       LINK-OUT-RET   =    ZERO
                       MOVE SPACE  TO   EDIT-CURSOR OF DSP-TNOBIF
                       MOVE "M"    TO   EDIT-OPTION OF DSP-TNOBIF
              ELSE
                       IF  ERR-FLG = ZERO
                           MOVE      4    TO   ERR-FLG
                       END-IF
                       MOVE "C"    TO   EDIT-CURSOR OF DSP-TNOBIF
                       MOVE "R"    TO   EDIT-OPTION OF DSP-TNOBIF
                       GO   TO   PARA-CHK3-EXIT
              END-IF
     END-IF.
*日付論理チェック（終了納品日）
     IF   DSP-TNOBIT  NOT =  99999999
              MOVE    "2"        TO        LINK-IN-KBN
              MOVE   DSP-TNOBIT  TO        LINK-IN-YMD8
              CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD
              IF       LINK-OUT-RET   =    ZERO
                       MOVE SPACE  TO   EDIT-CURSOR OF DSP-TNOBIT
                       MOVE "M"    TO   EDIT-OPTION OF DSP-TNOBIT
              ELSE
                       IF  ERR-FLG = ZERO
                           MOVE      4    TO   ERR-FLG
                       END-IF
                       MOVE "C"    TO   EDIT-CURSOR OF DSP-TNOBIT
                       MOVE "R"    TO   EDIT-OPTION OF DSP-TNOBIT
                       GO   TO   PARA-CHK3-EXIT
              END-IF
     END-IF.
     IF  DSP-CHKBNY  = "Y"   AND  DSP-SNDKBN = "1"
         MOVE    "4"       TO    PSW
     ELSE
         MOVE    "9"     TO    PSW
         MOVE     9      TO    ERR-FLG
     END-IF.
 PARA-CHK3-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック４                     2.1.2     *
****************************************************************
 PARA-CHK4-SEC             SECTION.
     MOVE     "PARA-CHK4-SEC"     TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*横持日チェック
***  横持日未入力チェック
     IF       DSP-SNDKBN = "1"
         IF       DSP-YKMCBI  NOT NUMERIC
             OR   DSP-YKMCBI  =  ZERO
              MOVE   8       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-YKMCBI
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-YKMCBI
              GO             TO   PARA-CHK4-EXIT
         ELSE
***           横持日論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-YKMCBI     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  4        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF DSP-YKMCBI
                   MOVE  "C"      TO   EDIT-CURSOR  OF DSP-YKMCBI
                   GO             TO   PARA-CHK4-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-YKMCBI
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-YKMCBI
         END-IF
     END-IF.
     MOVE    "9"     TO    PSW.
     MOVE     9      TO    ERR-FLG.
 PARA-CHK4-EXIT.
     EXIT.
****************************************************************
*             確認項目入力( PSW = 9 )                2.2       *
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
                MOVE    DSP-SNDKBN    TO  LINK-OUT-SNDKBN
                MOVE    DSP-CHKBNO    TO  LINK-OUT-CHKBNO
                MOVE    DSP-CHKBNT    TO  LINK-OUT-CHKBNT
                MOVE    DSP-CHKBNY    TO  LINK-OUT-CHKBNY
                MOVE    DSP-SOKOCD    TO  LINK-OUT-SOKO
                MOVE    DSP-YKMCBI    TO  LINK-OUT-YKMCBI
                MOVE    DSP-BTBI      TO  LINK-OUT-BTBI
                MOVE    DSP-BTJKK     TO  LINK-OUT-BTJKK
                MOVE    DSP-BTTORI    TO  LINK-OUT-BTTORI
                MOVE    DSP-ONOBIF    TO  LINK-OUT-ONOBIF
                MOVE    DSP-ONOBIT    TO  LINK-OUT-ONOBIT
                MOVE    DSP-TENPOF    TO  LINK-OUT-TENPOF
                MOVE    DSP-TENPOT    TO  LINK-OUT-TENPOT
                MOVE    DSP-TORICD    TO  LINK-OUT-TORICD
                MOVE    DSP-DENNOF    TO  LINK-OUT-DENNOF
                MOVE    DSP-DENNOT    TO  LINK-OUT-DENNOT
                MOVE    DSP-TNOBIF    TO  LINK-OUT-TNOBIF
                MOVE    DSP-TNOBIT    TO  LINK-OUT-TNOBIT
                MOVE    LINK-IN-BMNCD TO  LINK-OUT-BMNCD
                MOVE    LINK-IN-TANCD TO  LINK-OUT-TANCD
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
           MOVE    SPACE              TO   DSP-ERRMSG
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-ERRMSG
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
***      パラメタ項目
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-PFGAID
***      確認
         WHEN   "9"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
***      その他
         WHEN   OTHER
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FBT00101"          TO   DSP-FMT.
     WRITE    DSP-FBT00101.
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
*オンライン
         WHEN   "2"
                MOVE    "GRP002"  TO   DSP-GRP
*手書項目
         WHEN   "3"
                MOVE    "GRP003"  TO   DSP-GRP
*横持項目
         WHEN   "4"
                MOVE    "GRP004"  TO   DSP-GRP
*確認
         WHEN   "9"
                MOVE    "KAKU"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FBT00101"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FBT00101.
*システム日付転送
     MOVE    SYS-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:6)         TO   DSP-STIME.
     MOVE    "SBT0010I"           TO   DSP-PGID.
*連携担当者
     PERFORM      DSP-TANTO-SEC.
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             連携担当者表示                                   *
****************************************************************
 DSP-TANTO-SEC         SECTION.
     MOVE     "DSP-TANTO-SEC"      TO   S-NAME.
*
*担当者マスタ取得
     MOVE      LINK-IN-BMNCD     TO   TAN-F01 DSP-RNTAN1.
*    MOVE      "-"       TO   DSP-RNRAN(5:1).
     MOVE      LINK-IN-TANCD     TO   TAN-F02 DSP-RNTAN2.
     READ      HTANMS
         INVALID  KEY
             MOVE      SPACE     TO   DSP-RNTANN
         NOT INVALID  KEY
             MOVE      TAN-F03(1:10)  TO  DSP-RNTANN
     END-READ.
*
 DSP-TANTO-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*
*リバース，カーソルパーク解除
***  送信区分　　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SNDKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SNDKBN.
***  抽出区分（オン）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-CHKBNO.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-CHKBNO.
***  抽出区分（手書）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-CHKBNT.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-CHKBNT.
***  抽出区分（横持）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-CHKBNY.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-CHKBNY.
***  抽出倉庫コード
     IF      LINK-IN-DSOKO  =  "99"
         MOVE      LINK-IN-SOKO  TO   SOK-F01   DSP-SOKOCD
         READ      ZSOKMS
             INVALID  KEY
                 MOVE    SPACE     TO   DSP-CHUSKN
             NOT INVALID  KEY
                 MOVE    SOK-F02   TO   DSP-CHUSKN
         END-READ
         MOVE    "X"    TO   EDIT-STATUS OF DSP-SOKOCD
     ELSE
         MOVE    SPACE  TO    DSP-SOKOCD
         MOVE    "M"    TO  EDIT-OPTION  OF  DSP-SOKOCD
         MOVE    SPACE  TO  EDIT-CURSOR  OF  DSP-SOKOCD
     END-IF.
***  バッチＮＯ　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTBI.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTBI.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTJKK.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTJKK.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTTORI.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTTORI.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-OTORIN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-OTORIN.
***  納品日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-ONOBIF.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-ONOBIF.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-ONOBIT.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-ONOBIT.
***  店舗
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TENPOF.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TENPOF.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TENPOT.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TENPOT.
***  取引先　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TORICD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TORICD.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TTORIN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TTORIN.
***  納品日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TNOBIF.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TNOBIF.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TNOBIT.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TNOBIT.
***  伝票ＮＯ
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENNOF.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DENNOF.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENNOT.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DENNOT.
***  横持日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-YKMCBI.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-YKMCBI.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      HTOKMS HTANMS  ZSOKMS  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SBT0160I   END PROGRAM  >>******************

```
