# SJH0016I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJH0016I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　スポット受信スケジュール管理      *
*    モジュール名　　　　：　スポット受信スケジュールマスタ    *
*    作成日／更新日　　　：　99/09/10                          *
*    作成者／更新者　　　：　ＮＡＶ萩原                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJH0016I.
 AUTHOR.               HAGIWARA.
 DATE-WRITTEN.         99/09/10.
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
*スポット受信スケジュールマスタ
     SELECT  JHMSJSF   ASSIGN    TO        JHMSJSL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SJS-F02
                                           SJS-F03
                       FILE      STATUS    SJS-ST.
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
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = スポット受信スケジュールマスタ                     *
****************************************************************
 FD  JHMSJSF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMSJSF   OF   XFDLIB
                       JOINING   SJS       AS   PREFIX.
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
                       COPY      FJH00161  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  SJS-ST                   PIC  X(02).
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
     03  SYS-DATE                 PIC  9(08)  VALUE  ZERO.
*受信日付チェック用
* 01  JUSIN-DATE.
*    03  JUSIN-DATEY              PIC  9(04)  VALUE  ZERO.
*    03  JUSIN-DATEM              PIC  9(02)  VALUE  ZERO.
*    03  JUSIN-DATED              PIC  9(02)  VALUE  ZERO.
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
         05  FILLER              PIC   N(25)
             VALUE NC"正しい値を入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(25)
             VALUE NC"取引先マスタに登録されていません".
     03  ERR-MSG3.
         05  FILLER              PIC   N(25)
             VALUE
             NC"スポット受信スケジュールマスタに登録済です".
     03  ERR-MSG4.
         05  FILLER              PIC   N(25)
             VALUE
             NC"スポット受信スケジュールマスタに未登録です".
     03  ERR-MSG5.
         05  FILLER              PIC   N(25)
             VALUE NC"取引先コードを入力して下さい".
     03  ERR-MSG6.
         05  FILLER              PIC   N(25)
             VALUE NC"無効キーです".
     03  ERR-MSG7.
         05  FILLER              PIC   N(25)
             VALUE NC"作成済みです".
     03  ERR-MSG8.
         05  FILLER              PIC   N(25)
             VALUE NC"受信日付を入力して下さい".
     03  ERR-MSG9.
         05  FILLER              PIC   N(25)
             VALUE NC"受信日付論理エラー".
     03  ERR-MSG10.
         05  FILLER              PIC   N(25)
             VALUE NC"受信時間論理エラー".
     03  ERR-MSG11.
         05  FILLER              PIC   N(25)
             VALUE NC"分は，３０分単位で入力して下さい".
     03  ERR-MSG12.
         05  FILLER              PIC   N(25)
             VALUE NC"受信時間を入力して下さい".
     03  ERR-MSG13.
         05  FILLER              PIC   N(25)
             VALUE NC"処理区分を入力してください".
     03  ERR-MSG14.
         05  FILLER              PIC   N(25)
             VALUE NC"受信日付は当日以降の日付を入力して下さい".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  14  PIC   N(25).
*
 01  FILE-ERR.
     03  SJS-ERR           PIC N(20) VALUE
                     NC"スポット受信スケジュールマスタエラー".
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
 SJS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMSJSF.
     DISPLAY     SJS-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SJS-ST    UPON      CONS.
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
     OPEN     I-O       JHMSJSF   DSPFILE.
     OPEN     INPUT     HTOKMS.
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
*処理区分入力
         WHEN      "1"  PERFORM   DSP-HEAD1-SEC
*キー項目入力
         WHEN      "2"  PERFORM   DSP-HEAD2-SEC
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
*             処理区分入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-HEAD1-SEC         SECTION.
     MOVE     "DSP-HEAD1-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD1-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-HEAD1-SEC
     END-EVALUATE.
*
 DSP-HEAD1-EXIT.
     EXIT.
****************************************************************
*             処理区分チェック                       2.1.1     *
****************************************************************
 HEAD1-CHK-SEC             SECTION.
*処理区分 未入力はエラー
     IF       DSP-SHORI      NOT NUMERIC OR
              DSP-SHORI      =    ZERO
              MOVE      13        TO   ERR-FLG
              GO                  TO   HEAD1-CHK-EXIT
     END-IF.
*処理区分＝１，２，３以外はエラー
     IF    DSP-SHORI    =   1   OR   2   OR   3
              MOVE     "2"        TO   PSW
*             同一モードでループさせるため
              MOVE     DSP-SHORI  TO   SAV-SHORI
     ELSE
              MOVE     1          TO   ERR-FLG
     END-IF.
*
 HEAD1-CHK-EXIT.
     EXIT.
****************************************************************
*             キー項目入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-HEAD2-SEC         SECTION.
     MOVE     "DSP-HEAD2-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD2-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-HEAD2-SEC
     END-EVALUATE.
*
 DSP-HEAD2-EXIT.
     EXIT.
****************************************************************
*             キー項目チェック                       2.2.1     *
****************************************************************
 HEAD2-CHK-SEC         SECTION.
     MOVE     "HEAD2-CHK-SEC"     TO   S-NAME.
*キー項目 未入力チェック
***  受信日
     MOVE     DSP-JSINYY     TO   WK-CHKDATE-YYYY
     MOVE     DSP-JSINMM     TO   WK-CHKDATE-MM
     MOVE     DSP-JSINDD     TO   WK-CHKDATE-DD
*
     IF       DSP-JSINYY  NOT NUMERIC OR
              DSP-JSINMM  NOT NUMERIC OR
              DSP-JSINDD  NOT NUMERIC OR
              DSP-JSINYY  = ZERO  OR
              DSP-JSINMM  = ZERO  OR
              DSP-JSINDD  = ZERO
              MOVE   8       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION   OF  DSP-JSINYY
              MOVE  "R"      TO   EDIT-OPTION   OF  DSP-JSINMM
              MOVE  "R"      TO   EDIT-OPTION   OF  DSP-JSINDD
              MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-JSINYY
              GO             TO   HEAD2-CHK-EXIT
     ELSE
***           受信日付の範囲チェック（当日以降）
************  MOVE     DSP-JSINYY     TO   JUSIN-DATEY
************  MOVE     DSP-JSINMM     TO   JUSIN-DATEM
************  MOVE     DSP-JSINDD     TO   JUSIN-DATED
              IF       WK-CHKDATE     >=   SYS-DATE
                       MOVE  "M"   TO  EDIT-OPTION  OF  DSP-JSINYY
                       MOVE  "M"   TO  EDIT-OPTION  OF  DSP-JSINMM
                       MOVE  "M"   TO  EDIT-OPTION  OF  DSP-JSINDD
                       MOVE  SPACE TO  EDIT-CURSOR  OF  DSP-JSINYY
              ELSE
                       MOVE  14   TO  ERR-FLG
                       MOVE  "R"   TO  EDIT-OPTION  OF  DSP-JSINYY
                       MOVE  "R"   TO  EDIT-OPTION  OF  DSP-JSINMM
                       MOVE  "R"   TO  EDIT-OPTION  OF  DSP-JSINDD
                       MOVE  "C"   TO  EDIT-CURSOR  OF  DSP-JSINYY
                       GO          TO  HEAD2-CHK-EXIT
              END-IF
***          システム日付の論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     WK-CHKDATE     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET NOT = 9
                   MOVE  "M"   TO  EDIT-OPTION  OF  DSP-JSINYY
                   MOVE  "M"   TO  EDIT-OPTION  OF  DSP-JSINMM
                   MOVE  "M"   TO  EDIT-OPTION  OF  DSP-JSINDD
                   MOVE  SPACE TO  EDIT-CURSOR  OF  DSP-JSINYY
              ELSE
                   MOVE  9     TO  ERR-FLG
                   MOVE  "R"   TO  EDIT-OPTION  OF  DSP-JSINYY
                   MOVE  "R"   TO  EDIT-OPTION  OF  DSP-JSINMM
                   MOVE  "R"   TO  EDIT-OPTION  OF  DSP-JSINDD
                   MOVE  "C"   TO  EDIT-CURSOR  OF  DSP-JSINYY
                   GO          TO  HEAD2-CHK-EXIT
              END-IF
     END-IF.
*
***  受信時間
     IF       DSP-JUSINT    NUMERIC
     AND      DSP-JUSINT    NOT =  ZERO
              MOVE   DSP-JUSINT    TO  WK-JIKAN
              IF     WK-HH  <=  ZERO  OR  WK-HH  >  24
              OR     WK-MM  <   ZERO  OR  WK-MM  >  59
                     IF     ERR-FLG  =  ZERO
                            MOVE    10   TO    ERR-FLG
                     END-IF
                     MOVE "R"    TO EDIT-OPTION OF DSP-JUSINT
                     MOVE "C"    TO EDIT-CURSOR OF DSP-JUSINT
                     GO          TO HEAD2-CHK-EXIT
              ELSE
                  IF WK-MM  =  ZERO  OR  WK-MM  =  30
                     MOVE "M"    TO EDIT-OPTION OF DSP-JUSINT
                     MOVE  SPACE TO EDIT-CURSOR OF DSP-JUSINT
                  ELSE
                     IF     ERR-FLG  =  ZERO
                            MOVE    11   TO    ERR-FLG
                     END-IF
                     MOVE "R"    TO EDIT-OPTION OF DSP-JUSINT
                     MOVE "C"    TO EDIT-CURSOR OF DSP-JUSINT
                     GO          TO HEAD2-CHK-EXIT
                  END-IF
              END-IF
     ELSE
          IF  DSP-JUSINT   NOT NUMERIC OR
              DSP-JUSINT   = ZERO
              IF  ERR-FLG  =  ZERO
                  MOVE   12   TO   ERR-FLG
              END-IF
              MOVE  "R"      TO  EDIT-OPTION   OF  DSP-JUSINT
              MOVE  "C"      TO  EDIT-CURSOR   OF  DSP-JUSINT
              GO             TO  HEAD2-CHK-EXIT
          END-IF
     END-IF.
*
*スポット受信スケジュールマスタ ＲＥＡＤ
     PERFORM   JHMSJSF-READ.
***  処理区分＝１（登録）
     IF       DSP-SHORI   =   1
***           データが存在すればエラー
              IF    READ-FLG   =   ZERO
                    MOVE     3            TO   ERR-FLG
***           データが存在しなければＯＫ（ボディ入力へ）
              ELSE
***                 作成区分出力
                    MOVE    "1"           TO   DSP-SAKKBN
                    MOVE    NC"未作成"    TO   DSP-SAKNM
*
                    MOVE    "3"           TO   PSW
              END-IF
     ELSE
***  処理区分＝２（修正），３（削除）
*           IF    SAKKBN-FLG  = 1
***           データが存在すればＯＫ
              IF  READ-FLG   =   ZERO
***               作成区分が未作成ならば
                  IF  SJS-F01  =  "1"
                      PERFORM   MOVE-DSP-SEC
***                   処理区分＝２（修正）は，ボディ入力へ
                      IF  DSP-SHORI  =  2
                          MOVE    "3"          TO   PSW
                      ELSE
***                   処理区分＝３（削除）は，確認入力へ
                          MOVE    "4"          TO   PSW
***                       取引先名称取得
                          PERFORM   HTOKMS-READ
                          IF        READ-FLG   =   ZERO
                                    MOVE  TOK-F03  TO  DSP-TORINM
                          ELSE
                                    MOVE  SPACE    TO  DSP-TORINM
                          END-IF

                      END-IF
***               作成区分が作成済であれば
                  ELSE
                        MOVE     7     TO  ERR-FLG
                  END-IF
***         データが存在しなければエラー
            ELSE
                  MOVE     4            TO   ERR-FLG
            END-IF
     END-IF.
*
 HEAD2-CHK-EXIT.
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
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
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
*取引先
     IF       DSP-TORICD  NOT NUMERIC OR
              DSP-TORICD  =    ZERO
              MOVE   5    TO   ERR-FLG
              MOVE  "R"   TO   EDIT-OPTION    OF  DSP-TORICD
              MOVE  "C"   TO   EDIT-CURSOR    OF  DSP-TORICD
              GO          TO   BODY-CHK-EXIT
     ELSE
***       取引先コード存在チェック（取引先マスタ ＲＥＡＤ）
              PERFORM   HTOKMS-READ
              IF        READ-FLG    =   ZERO
***           存在すればＯＫ（取引先名→画面）
                    MOVE    TOK-F03        TO   DSP-TORINM
                    MOVE  "M"     TO  EDIT-OPTION  OF  DSP-TORICD
                    MOVE   SPACE  TO  EDIT-CURSOR  OF  DSP-TORICD
              ELSE
***           存在しなければＮＧ（空白→画面）
                    MOVE    SPACE          TO   DSP-TORINM
                    MOVE     2             TO   ERR-FLG
                    MOVE  "R"   TO   EDIT-OPTION   OF  DSP-TORICD
                    MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-TORICD
                    GO                     TO   BODY-CHK-EXIT
              END-IF
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
                EVALUATE  DSP-SHORI
*-------------------登録
                    WHEN  "1"
                          PERFORM     FILE-WRT-SEC
*-------------------修正
                    WHEN  "2"
                          PERFORM     FILE-UPD-SEC
*-------------------削除
                    WHEN  "3"
                          PERFORM     FILE-DLT-SEC
                END-EVALUATE
                PERFORM   INIT-DSP-SEC
                MOVE    "2"      TO   PSW
                MOVE   SAV-SHORI TO   DSP-SHORI
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                IF  DSP-SHORI   =  1   OR   2
                    MOVE    "3"       TO   PSW
                ELSE
                    MOVE    "1"       TO   PSW
                END-IF
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
*    スポット受信マスタ更新　処理区分＝１（登録）2.4.1         *
****************************************************************
 FILE-WRT-SEC           SECTION.
     MOVE     "FILE-WRT-SEC"      TO   S-NAME.
*レコード初期クリア
     MOVE     SPACE               TO   SJS-REC.
     INITIALIZE                        SJS-REC.
*キー項目転送
***  受信日
     MOVE     WK-CHKDATE          TO   SJS-F02.
***  受信時間
     MOVE     DSP-JUSINT          TO   SJS-F03.
*明細項目
     PERFORM  MOVE-JHMSJSF.
*スポット受信スケジュールマスタ登録
     WRITE    SJS-REC.
*
 FILE-WRT-EXIT.
     EXIT.
****************************************************************
*    スポット受信マスタ更新　処理区分＝２（修正）  2.4.2       *
****************************************************************
 FILE-UPD-SEC           SECTION.
     MOVE     "FILE-UPD-SEC"      TO   S-NAME.
*キー項目転送
***  受信日
     MOVE     WK-CHKDATE          TO   SJS-F02.
***  受信時間
     MOVE     DSP-JUSINT          TO   SJS-F03.
*スポット受信マスタ ＲＥＡＤ（存在すればレコード更新）
     PERFORM   JHMSJSF-READ.
     IF    READ-FLG   =   ZERO
           PERFORM   MOVE-JHMSJSF
           REWRITE   SJS-REC
     ELSE
           DISPLAY
           NC"未登録です　受信日　＝"  WK-CHKDATE UPON CONS
           DISPLAY
           NC"　　　　　　受信時間＝"  DSP-JUSINT UPON CONS
     END-IF.
*
 FILE-UPD-EXIT.
     EXIT.
****************************************************************
*    スポット受信マスタ更新　処理区分＝３（削除）  2.4.3       *
****************************************************************
 FILE-DLT-SEC           SECTION.
     MOVE     "FILE-DLT-SEC"      TO   S-NAME.
*キー項目転送
***  受信日
     MOVE     WK-CHKDATE          TO   SJS-F02.
***  受信時間
     MOVE     DSP-JUSINT          TO   SJS-F03.
*スポット受信マスタＲＥＡＤ（存在すればレコード削除）
     PERFORM   JHMSJSF-READ.
     IF    READ-FLG   =   ZERO
           DELETE    JHMSJSF
     ELSE
           DISPLAY
           NC"未登録です　受信日　＝"  WK-CHKDATE UPON CONS
           DISPLAY
           NC"　　　　　　受信時間＝"  DSP-JUSINT UPON CONS
     END-IF.
*
 FILE-DLT-EXIT.
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
*処理区分
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-FNCSPC
*キー項目
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
*明細項目
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
*確認
         WHEN   "4"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-FNCSPC
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FJH00161"          TO   DSP-FMT.
     WRITE    DSP-FJH00161.
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
*処理区分
         WHEN   "1"
                MOVE    "SMODE"   TO   DSP-GRP
*キー項目
         WHEN   "2"
                MOVE    "KEYGRP"  TO   DSP-GRP
*明細項目
         WHEN   "3"
                MOVE    "MAIN"    TO   DSP-GRP
*確認
         WHEN   "4"
                MOVE    "CHKGRP"  TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FJH00161"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             項目転送  （スポット受信マスタ → 画面）
****************************************************************
 MOVE-DSP-SEC           SECTION.
     MOVE     "MOVE-DSP-SEC"      TO   S-NAME.
*
*取引先
     MOVE    SJS-F04              TO   DSP-TORICD.
*作成区分
     MOVE    SJS-F01              TO   DSP-SAKKBN.
     IF      SJS-F01  = 1
             MOVE       NC"未作成"     TO   DSP-SAKNM
     ELSE
             MOVE       NC"作成済"     TO   DSP-SAKNM
     END-IF.
*
 MOVE-DSP-EXIT.
     EXIT.
****************************************************************
*             項目転送  （画面 → スポット受信マスタ）
****************************************************************
 MOVE-JHMSJSF        SECTION.
     MOVE     "MOVE-JHMSJSF-SEC"  TO   S-NAME.
*
*取引先
     MOVE    DSP-TORICD           TO   SJS-F04.
*作成区分
     IF      DSP-SHORI  =  1
             MOVE       "1"       TO   SJS-F01
     END-IF.
*スケジュール作成日，時間
     IF      SJS-F05  =  ZERO
             MOVE       SYS-DATE       TO   SJS-F05
             MOVE       WK-TIME(1:4)   TO   SJS-F06
*スケジュール更新日，時間
     ELSE
             MOVE       SYS-DATE       TO   SJS-F07
             MOVE       WK-TIME(1:4)   TO   SJS-F08
     END-IF.
*
 MOVE-JHMSJSF-EXIT.
     EXIT.
****************************************************************
*             スポット受信マスタ　ＲＥＡＤ                     *
****************************************************************
 JHMSJSF-READ             SECTION.
     MOVE     "JHMSJSF-READ"      TO   S-NAME.
*
     MOVE    ZERO                 TO   READ-FLG.
     MOVE    ZERO                 TO   SAKKBN-FLG.
*スポット受信マスタＲＥＡＤ（該当データ無時，READ-FLG=1）
     MOVE    WK-CHKDATE           TO   SJS-F02.
     MOVE    DSP-JUSINT           TO   SJS-F03.
     READ    JHMSJSF
         INVALID
             MOVE    1            TO   READ-FLG
*        NOT INVALID
*            作成区分＝1（未作成）
*            IF    SJS-F01   =    "1"
*                  MOVE      1    TO   SAKKBN-FLG
*            END-IF
     END-READ.
*
 JHMSJSF-READ-EXIT.
     EXIT.
****************************************************************
*               取引先マスタ　ＲＥＡＤ                         *
****************************************************************
 HTOKMS-READ             SECTION.
     MOVE     "HTOKMS-READ"       TO   S-NAME.
*
     MOVE    ZERO                 TO   READ-FLG.
*取引先ＲＥＡＤ（該当データ無，READ-FLG=1）
     MOVE    DSP-TORICD           TO   TOK-F01.
     READ    HTOKMS
         INVALID
             MOVE    1            TO   READ-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*画面の初期化
     MOVE    SPACE                TO   DSP-FJH00161.
*システム日付転送
     MOVE    SYS-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:6)         TO   DSP-STIME.
*リバース，カーソルパーク解除
***  受信日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JSINYY.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JSINYY.
***  受信時間
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JUSINT.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JUSINT.
***  取引先
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TORICD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TORICD.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             JHMSJSF  HTOKMS  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SJH0016I   END PROGRAM  >>******************

```
