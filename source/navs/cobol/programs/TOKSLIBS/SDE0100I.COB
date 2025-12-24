****************************************************************
*    顧客名　　　　　　　：（株）サカタのタネ殿　　　　　　　　*
*    業務名　　　　　　　：　伝票ＥＸＣＥＬ連携　　            *
*    モジュール名　　　　：　伝票関係リスト発行指示　　        *
*    作成日／作成者　　　：　2016/09/15  INOUE                 *
*    処理概要　　　　　　：　伝票ＥＸＣＥＬ関連帳票の発行指示　*
*    変更日／変更者　　　：　                                  *
*    変更内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SDE0100I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2016/09/15.
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
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*担当者マスタ
     SELECT  HTANMS    ASSIGN    TO        TANMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TAN-F01 TAN-F02
                       FILE      STATUS    TAN-ST.
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
                       COPY      FDE01001  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
****************************************************************
*  FILE= 取引先マスタ                                        *
****************************************************************
 FD  HTOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*  FILE= 担当者マスタ                                        *
****************************************************************
 FD  HTANMS
*                      BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTANMS    OF   XFDLIB
                       JOINING   TAN       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  TAN-ST                   PIC  X(02).
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
     03  PARA-HAKO01-ERR          PIC  X(03)  VALUE  SPACE.
     03  PARA-HAKO02-ERR          PIC  X(03)  VALUE  SPACE.
     03  PARA-HAKO03-ERR          PIC  X(03)  VALUE  SPACE.
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
*            VALUE NC"_取消　_終了".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(15).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(30)
             VALUE NC"発行帳票区分が誤りです".
     03  ERR-MSG2.
         05  FILLER              PIC   N(30)
             VALUE NC"出力区分が誤りです".
     03  ERR-MSG3.
         05  FILLER              PIC   N(30)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(30)
             VALUE NC"発行帳票区分が無指定です".
     03  ERR-MSG5.
         05  FILLER              PIC   N(30)
             VALUE NC"　　　　　　　　　".
     03  ERR-MSG6.
         05  FILLER              PIC   N(30)
             VALUE NC"取引先マスタに登録されていません".
     03  ERR-MSG7.
         05  FILLER              PIC   N(30)
             VALUE NC"担当者マスタに登録されていません".
     03  ERR-MSG8.
         05  FILLER              PIC   N(30)
             VALUE NC"　　　　　　　　　　　　".
     03  ERR-MSG9.
         05  FILLER              PIC   N(30)
 VALUE NC"条件に誤りがないか確認し、ＥＮＴＥＲを実行して下さい".
     03  ERR-MSG10.
         05  FILLER              PIC   N(30)
         VALUE NC"　　　　　　　　　　　　".
     03  ERR-MSG11               PIC  N(30)  VALUE
              NC"　　　　　　　　　　　　　".
     03  ERR-MSG12               PIC  N(30)  VALUE
              NC"　　　　　　　　　　　　　".
     03  ERR-MSG13               PIC  N(30)  VALUE
              NC"　　　　　　　　　　　　".
     03  ERR-MSG14           PIC  N(30)  VALUE
              NC"　　　　　　　　　".
     03  ERR-MSG15           PIC  N(30)  VALUE
              NC"　　　　　　　".
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
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
*                       NC"再送対象".
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
 01  LINK-IN-WKSTN            PIC  X(08).
 01  LINK-IN-FILNO            PIC  9(03).
 01  LINK-IN-BUMONCD          PIC  X(04).
 01  LINK-IN-TANCD            PIC  X(02).
 01  LINK-IN-PHASE            PIC  X(01).
 01  LINK-IN-DATE1            PIC  9(08).
 01  LINK-IN-TIME1            PIC  9(06).
 01  LINK-IN-DATE2            PIC  9(08).
 01  LINK-IN-TIME2            PIC  9(06).
 01  LINK-IN-DATE3            PIC  9(08).
 01  LINK-IN-TIME3            PIC  9(06).
 01  LINK-OUT-HAKKO1          PIC  X(01).
 01  LINK-OUT-HAKKO2          PIC  X(01).
 01  LINK-OUT-HAKKO3          PIC  X(01).
 01  LINK-OUT-OUTPUT          PIC  X(01).
 01  LINK-OUT-TOKCD           PIC  9(08).
 01  LINK-OUT-TENPOF          PIC  9(05).
 01  LINK-OUT-TENPOT          PIC  9(05).
*
**************************************************************
 PROCEDURE             DIVISION   USING
                                         LINK-IN-WKSTN
                                         LINK-IN-FILNO
                                         LINK-IN-BUMONCD
                                         LINK-IN-TANCD
                                         LINK-IN-PHASE
                                         LINK-IN-DATE1
                                         LINK-IN-TIME1
                                         LINK-IN-DATE2
                                         LINK-IN-TIME2
                                         LINK-IN-DATE3
                                         LINK-IN-TIME3
                                         LINK-OUT-HAKKO1
                                         LINK-OUT-HAKKO2
                                         LINK-OUT-HAKKO3
                                         LINK-OUT-OUTPUT
                                         LINK-OUT-TOKCD
                                         LINK-OUT-TENPOF
                                         LINK-OUT-TENPOT.
**************************************************************
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
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
*↓IN-PARA表示
     DISPLAY "---SDE0100I-IN--"                  UPON CONS.
     DISPLAY "LINK-IN-WKSTN  =" LINK-IN-WKSTN    UPON CONS.
     DISPLAY "LINK-IN-FILNO  =" LINK-IN-FILNO    UPON CONS.
     DISPLAY "LINK-IN-BUMONCD=" LINK-IN-BUMONCD  UPON CONS.
     DISPLAY "LINK-IN-TANCD  =" LINK-IN-TANCD    UPON CONS.
     DISPLAY "LINK-IN-PHASE  =" LINK-IN-PHASE    UPON CONS.
     DISPLAY "LINK-IN-DATE1  =" LINK-IN-DATE1    UPON CONS.
     DISPLAY "LINK-IN-TIME1  =" LINK-IN-TIME1    UPON CONS.
     DISPLAY "LINK-IN-DATE2  =" LINK-IN-DATE2    UPON CONS.
     DISPLAY "LINK-IN-TIME2  =" LINK-IN-TIME2    UPON CONS.
     DISPLAY "LINK-IN-DATE3  =" LINK-IN-DATE3    UPON CONS.
     DISPLAY "LINK-IN-TIME3  =" LINK-IN-TIME3    UPON CONS.
*↑IN-PARA表示
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTANMS.
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
*             パラメタチェック                       2.1.1     *
****************************************************************
 PARA-CHK-SEC             SECTION.
     MOVE     "PARA-CHK-SEC"     TO   S-NAME.
*
*帳票発行区分チェック
     MOVE      "   "    TO   PARA-HAKO01-ERR.
     MOVE      "   "    TO   PARA-HAKO02-ERR.
     MOVE      "   "    TO   PARA-HAKO03-ERR.
     IF  DSP-HAKO01 = "Y"
         MOVE  "M"      TO   EDIT-OPTION  OF  DSP-HAKO01
         MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-HAKO01
     ELSE
         IF  DSP-HAKO01  = " "
             MOVE  "M"      TO   EDIT-OPTION  OF  DSP-HAKO01
             MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-HAKO01
         ELSE
             MOVE   1       TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HAKO01
             MOVE  "ERR"    TO   PARA-HAKO01-ERR
         END-IF
     END-IF.
*
     IF  DSP-HAKO02 = "Y"
         MOVE  "M"      TO   EDIT-OPTION  OF  DSP-HAKO02
         MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-HAKO02
     ELSE
         IF  DSP-HAKO02  = " "
             MOVE  "M"      TO   EDIT-OPTION  OF  DSP-HAKO02
             MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-HAKO02
         ELSE
             MOVE   1       TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HAKO02
             MOVE  "ERR"    TO   PARA-HAKO02-ERR
         END-IF
     END-IF.
*
     IF  DSP-HAKO03 = "Y"
         MOVE  "M"      TO   EDIT-OPTION  OF  DSP-HAKO03
         MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-HAKO03
     ELSE
         IF  DSP-HAKO03  = " "
             MOVE  "M"      TO   EDIT-OPTION  OF  DSP-HAKO03
             MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-HAKO03
         ELSE
             MOVE   1       TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HAKO03
             MOVE  "ERR"    TO   PARA-HAKO03-ERR
         END-IF
     END-IF.
*
     IF   PARA-HAKO03-ERR   =    "ERR"
          MOVE     "C"      TO   EDIT-CURSOR  OF  DSP-HAKO03
     END-IF.
     IF   PARA-HAKO02-ERR   =    "ERR"
          MOVE     "C"      TO   EDIT-CURSOR  OF  DSP-HAKO02
     END-IF.
     IF   PARA-HAKO01-ERR   =    "ERR"
          MOVE     "C"      TO   EDIT-CURSOR  OF  DSP-HAKO01
     END-IF.
     IF ( PARA-HAKO03-ERR   =    "ERR"  ) OR
        ( PARA-HAKO02-ERR   =    "ERR"  ) OR
        ( PARA-HAKO01-ERR   =    "ERR"  )
          GO                TO   PARA-CHK-EXIT
     END-IF.
*
     IF ( DSP-HAKO01  = " " ) AND
        ( DSP-HAKO02  = " " ) AND
        ( DSP-HAKO03  = " " )
          MOVE      4       TO   ERR-FLG
          MOVE     "C"      TO   EDIT-CURSOR  OF  DSP-HAKO01
          MOVE     "R"      TO   EDIT-OPTION  OF  DSP-HAKO01
          MOVE     "R"      TO   EDIT-OPTION  OF  DSP-HAKO02
          MOVE     "R"      TO   EDIT-OPTION  OF  DSP-HAKO03
          GO                TO   PARA-CHK-EXIT
     END-IF.
*
*取引先チェック
     IF ( DSP-TOKCD    NOT NUMERIC ) OR
        ( DSP-TOKCD    =   ZERO    )
          MOVE   ZERO         TO   DSP-TOKCD
          MOVE  "M"           TO   EDIT-OPTION  OF  DSP-TOKCD
          MOVE  SPACE         TO   EDIT-CURSOR  OF  DSP-TOKCD
     ELSE
*       取引先マスタＲＥＡＤ
          MOVE   DSP-TOKCD    TO   TOK-F01
          READ   HTOKMS
          INVALID
                 MOVE   6     TO   ERR-FLG
                 MOVE  "R"    TO   EDIT-OPTION  OF  DSP-TOKCD
                 MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-TOKCD
                 GO           TO   PARA-CHK-EXIT
          NOT INVALID
                 MOVE  "M"    TO   EDIT-OPTION  OF  DSP-TOKCD
                 MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-TOKCD
          END-READ
     END-IF.
*
*開始店舗未入力時（初期値セット）
     IF  DSP-TENST  NOT  NUMERIC
              MOVE     ZERO  TO   DSP-TENST
     END-IF.
*終了店舗未入力時（初期値セット）
     IF  DSP-TENED  NOT  NUMERIC
              MOVE  99999999 TO   DSP-TENED
     END-IF.
*範囲大小チェック
     IF   DSP-TENST  >  DSP-TENED
              IF    ERR-FLG = ZERO
                    MOVE     16    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF DSP-TENST
              MOVE     "R"   TO   EDIT-OPTION OF DSP-TENST
              MOVE     "R"   TO   EDIT-OPTION OF DSP-TENED
              GO   TO   PARA-CHK-EXIT
     ELSE
              MOVE     SPACE TO   EDIT-CURSOR OF DSP-TENST
              MOVE     "M"   TO   EDIT-OPTION OF DSP-TENST
              MOVE     "M"   TO   EDIT-OPTION OF DSP-TENED
     END-IF.
*出力区分チェック
     IF  DSP-PRTKBN = "1"
         MOVE  "M"      TO   EDIT-OPTION  OF  DSP-PRTKBN
         MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-PRTKBN
     ELSE
         IF  DSP-PRTKBN  = " "
             MOVE  "M"      TO   EDIT-OPTION  OF  DSP-PRTKBN
             MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-PRTKBN
         ELSE
             MOVE   2       TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-PRTKBN
             MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-PRTKBN
         END-IF
     END-IF.
*
     IF    ERR-FLG  =  0
           MOVE     9     TO   PSW
     END-IF.
*
 PARA-CHK-EXIT.
     EXIT.
****************************************************************
*             確認項目入力( PSW = 9 )                2.2       *
****************************************************************
 DSP-KAKU-SEC         SECTION.
     MOVE     "DSP-KAKU-SEC"     TO   S-NAME.
*
     MOVE       9                TO   ERR-FLG.
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN  "E000"
                MOVE    DSP-HAKO01   TO  LINK-OUT-HAKKO1
                MOVE    DSP-HAKO02   TO  LINK-OUT-HAKKO2
                MOVE    DSP-HAKO03   TO  LINK-OUT-HAKKO3
                MOVE    DSP-PRTKBN   TO  LINK-OUT-OUTPUT
                MOVE    DSP-TOKCD    TO  LINK-OUT-TOKCD
                MOVE    DSP-TENST    TO  LINK-OUT-TENPOF
                MOVE    DSP-TENED    TO  LINK-OUT-TENPOT
                MOVE   "END"         TO  END-FLG
*↓OUT-PARA表示
     DISPLAY "---SDE0100I-OUT-"                  UPON CONS
     DISPLAY "LINK-OUT-HAKKO1="  LINK-OUT-HAKKO1 UPON CONS
     DISPLAY "LINK-OUT-HAKKO2="  LINK-OUT-HAKKO2 UPON CONS
     DISPLAY "LINK-OUT-HAKKO3="  LINK-OUT-HAKKO3 UPON CONS
     DISPLAY "LINK-OUT-OUTPUT="  LINK-OUT-OUTPUT UPON CONS
     DISPLAY "LINK-OUT-TOKCDT="  LINK-OUT-TOKCD  UPON CONS
     DISPLAY "LINK-OUT-TENPOF="  LINK-OUT-TENPOF UPON CONS
     DISPLAY "LINK-OUT-TENPOT="  LINK-OUT-TENPOT UPON CONS
*↑OUT-PARA表示
*終了
         WHEN  "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*項目戻し
         WHEN  "F006"
                MOVE    "1"      TO   PSW
*取消
         WHEN  "F004"
                MOVE    "1"      TO   PSW
                PERFORM  INIT-DSP-SEC
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
         WHEN   "9"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
***      その他
         WHEN   OTHER
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FDE01001"          TO   DSP-FMT.
     WRITE    DSP-FDE01001.
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
                MOVE    "BODY01"  TO   DSP-GRP
*
*        WHEN   "2"
*               MOVE    "GRP002"  TO   DSP-GRP
*
*        WHEN   "3"
*               MOVE    "GRP003"  TO   DSP-GRP
*
*        WHEN   "4"
*               MOVE    "GRP004"  TO   DSP-GRP
*確認
         WHEN   "9"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FDE01001"           TO   DSP-FMT.
     READ     DSPFILE.
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
     MOVE    SPACE                TO   DSP-FDE01001.
*システム日付転送
     MOVE    SYS-DATE(1:4)        TO   DSP-SDATE(1:4).
     MOVE    "/"                  TO   DSP-SDATE(5:1).
     MOVE    SYS-DATE(5:2)        TO   DSP-SDATE(6:2).
     MOVE    "/"                  TO   DSP-SDATE(8:1).
     MOVE    SYS-DATE(7:2)        TO   DSP-SDATE(9:2).
*システム時間転送
     MOVE    WK-TIME(1:2)         TO   DSP-STIME(1:2).
     MOVE    ":"                  TO   DSP-STIME(3:1).
     MOVE    WK-TIME(3:2)         TO   DSP-STIME(4:2).
     MOVE    ":"                  TO   DSP-STIME(6:1).
     MOVE    WK-TIME(5:2)         TO   DSP-STIME(7:2).
     MOVE    "SDE0100I"           TO   DSP-PGID.
*実行状況セット
     IF     LINK-IN-PHASE   =     "1"
*
            MOVE   NC"○"              TO   DSP-KEK01
*
            MOVE   LINK-IN-DATE1(1:4)  TO   DSP-KEK011(1:4)
            MOVE   "/"                 TO   DSP-KEK011(5:1)
            MOVE   LINK-IN-DATE1(5:2)  TO   DSP-KEK011(6:2)
            MOVE   "/"                 TO   DSP-KEK011(8:1)
            MOVE   LINK-IN-DATE1(7:2)  TO   DSP-KEK011(9:2)
*
            MOVE   LINK-IN-TIME1(1:2)  TO   DSP-KEK012(1:2)
            MOVE   ":"                 TO   DSP-KEK012(3:1)
            MOVE   LINK-IN-TIME1(3:2)  TO   DSP-KEK012(4:2)
     END-IF.
*
     IF     LINK-IN-PHASE   =     "2"
*
            MOVE   NC"○"              TO   DSP-KEK01
*
            MOVE   LINK-IN-DATE1(1:4)  TO   DSP-KEK011(1:4)
            MOVE   "/"                 TO   DSP-KEK011(5:1)
            MOVE   LINK-IN-DATE1(5:2)  TO   DSP-KEK011(6:2)
            MOVE   "/"                 TO   DSP-KEK011(8:1)
            MOVE   LINK-IN-DATE1(7:2)  TO   DSP-KEK011(9:2)
*
            MOVE   LINK-IN-TIME1(1:2)  TO   DSP-KEK012(1:2)
            MOVE   ":"                 TO   DSP-KEK012(3:1)
            MOVE   LINK-IN-TIME1(3:2)  TO   DSP-KEK012(4:2)
*
            MOVE   NC"○"              TO   DSP-KEK02
*
            MOVE   LINK-IN-DATE2(1:4)  TO   DSP-KEK021(1:4)
            MOVE   "/"                 TO   DSP-KEK021(5:1)
            MOVE   LINK-IN-DATE2(5:2)  TO   DSP-KEK021(6:2)
            MOVE   "/"                 TO   DSP-KEK021(8:1)
            MOVE   LINK-IN-DATE2(7:2)  TO   DSP-KEK021(9:2)
*
            MOVE   LINK-IN-TIME2(1:2)  TO   DSP-KEK022(1:2)
            MOVE   ":"                 TO   DSP-KEK022(3:1)
            MOVE   LINK-IN-TIME2(3:2)  TO   DSP-KEK022(4:2)
     END-IF.
*
     IF     LINK-IN-PHASE   =     "3"
*
            MOVE   NC"○"              TO   DSP-KEK01
*
            MOVE   LINK-IN-DATE1(1:4)  TO   DSP-KEK011(1:4)
            MOVE   "/"                 TO   DSP-KEK011(5:1)
            MOVE   LINK-IN-DATE1(5:2)  TO   DSP-KEK011(6:2)
            MOVE   "/"                 TO   DSP-KEK011(8:1)
            MOVE   LINK-IN-DATE1(7:2)  TO   DSP-KEK011(9:2)
*
            MOVE   LINK-IN-TIME1(1:2)  TO   DSP-KEK012(1:2)
            MOVE   ":"                 TO   DSP-KEK012(3:1)
            MOVE   LINK-IN-TIME1(3:2)  TO   DSP-KEK012(4:2)
*
            MOVE   NC"○"              TO   DSP-KEK02
*
            MOVE   LINK-IN-DATE2(1:4)  TO   DSP-KEK021(1:4)
            MOVE   "/"                 TO   DSP-KEK021(5:1)
            MOVE   LINK-IN-DATE2(5:2)  TO   DSP-KEK021(6:2)
            MOVE   "/"                 TO   DSP-KEK021(8:1)
            MOVE   LINK-IN-DATE2(7:2)  TO   DSP-KEK021(9:2)
*
            MOVE   LINK-IN-TIME2(1:2)  TO   DSP-KEK022(1:2)
            MOVE   ":"                 TO   DSP-KEK022(3:1)
            MOVE   LINK-IN-TIME2(3:2)  TO   DSP-KEK022(4:2)
*
            MOVE   NC"○"              TO   DSP-KEK03
*
            MOVE   LINK-IN-DATE3(1:4)  TO   DSP-KEK031(1:4)
            MOVE   "/"                 TO   DSP-KEK031(5:1)
            MOVE   LINK-IN-DATE3(5:2)  TO   DSP-KEK031(6:2)
            MOVE   "/"                 TO   DSP-KEK031(8:1)
            MOVE   LINK-IN-DATE3(7:2)  TO   DSP-KEK031(9:2)
*
            MOVE   LINK-IN-TIME3(1:2)  TO   DSP-KEK032(1:2)
            MOVE   ":"                 TO   DSP-KEK032(3:1)
            MOVE   LINK-IN-TIME3(3:2)  TO   DSP-KEK032(4:2)
     END-IF.
*
*WKSTN名セット
     MOVE   LINK-IN-WKSTN          TO   DSP-WKSTN.
*ファイル番号セット
     MOVE   LINK-IN-FILNO          TO   DSP-FILNO.
*実行担当者セット
     MOVE   LINK-IN-BUMONCD        TO   TAN-F01 DSP-TANCD(1:4).
     MOVE   LINK-IN-TANCD          TO   TAN-F02 DSP-TANCD(6:2).
     MOVE   "-"                    TO           DSP-TANCD(5:1).
     READ   HTANMS
            INVALID
                 MOVE   "R"        TO   EDIT-OPTION  OF  DSP-TANCD
            NOT INVALID
                 MOVE   "M"        TO   EDIT-OPTION  OF  DSP-TANCD
                 MOVE   TAN-F03    TO   DSP-TANNM
     END-READ.
*
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
***  帳票発行区分1
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-HAKO01.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-HAKO01.
***  帳票発行区分2
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-HAKO02.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-HAKO02.
***  帳票発行区分3
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-HAKO03.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-HAKO03.
***  取引先ＣＤ
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TOKCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TOKCD.
***  店舗ＣＤ開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TENST.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TENST.
***  店舗ＣＤ終了
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TENED.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TENED.
***  出力区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-PRTKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-PRTKBN.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      HTOKMS HTANMS DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SDE0100I   END PROGRAM  >>******************
