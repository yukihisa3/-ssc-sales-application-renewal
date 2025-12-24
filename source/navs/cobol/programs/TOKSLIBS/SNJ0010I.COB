****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　回線種別　　　　　　　　　        *
*    モジュール名　　　　：　回線種別マスタ保守                *
*    作成日／更新日　　　：　10/07/30                          *
*    作成者／更新者　　　：　ＮＡＶ大野                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    更新日／更新者　　　：　2012/10/31
*    修正概要　　　　　　：　流通ＢＭＳ対応　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNJ0010I.
 AUTHOR.               OONO.
 DATE-WRITTEN.         10/07/30.
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
*回線種別マスタ
     SELECT  JSMKAIF   ASSIGN    TO        JSMKAIL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       KAI-F01
                                           KAI-F02
                       FILE      STATUS    KAI-ST.
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
*    FILE = 回線種別マスタ                                     *
****************************************************************
 FD  JSMKAIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMKAIF   OF   XFDLIB
                       JOINING   KAI       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FNJ0010I  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KAI-ST                   PIC  X(02).
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
     03  MSG-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
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
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  WK-DATE                      PIC  9(06)  VALUE  ZERO.
 01  SYS-DATE                     PIC  9(08).
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
             VALUE NC"正しい処理区分を入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい回線種別を入力して下さい".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"回線種別マスタに登録済です".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"回線種別マスタに未登録です".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"データを入力して下さい".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"１又は２を入力して下さい".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"０以外で値を入力して下さい".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"空白又は１を入力して下さい".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"端末名を入力して下さい".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"トリガーファイル名を入力して下さい".
     03  ERR-MSG12.
         05  FILLER              PIC   N(20)
             VALUE NC"アンサーファイル名を入力して下さい".
     03  ERR-MSG13.
         05  FILLER              PIC   N(20)
             VALUE NC"パラメータファイル名を入力して下さい".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  13  PIC   N(20).
 01  MSG-AREA.
     03  MSG-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"登録が行われました".
     03  MSG-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"更新が行われました".
     03  MSG-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"削除が行われました".
 01  MSG-AREA-R      REDEFINES     MSG-AREA.
     03  MSG-MSG-R   OCCURS   3  PIC   N(20).
*
 01  FILE-ERR.
     03  KAI-ERR           PIC N(15) VALUE
                        NC"回線種別マスタエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  DSP-ERR           PIC N(15) VALUE
                        NC"画面ファイルエラー".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN        PIC X(01).
 01  LINK-IN-YMD6       PIC 9(06).
 01  LINK-IN-YMD8       PIC 9(08).
 01  LINK-OUT-RET       PIC X(01).
 01  LINK-OUT-YMD       PIC 9(08).
*
 LINKAGE           SECTION.
 01  PARA-TANTO         PIC X(02).
**************************************************************
 PROCEDURE             DIVISION
                                  USING  PARA-TANTO.
**************************************************************
 DECLARATIVES.
 KAI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMKAIF.
     DISPLAY     KAI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KAI-ST    UPON      CONS.
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
     OPEN     I-O       JSMKAIF.
     OPEN     I-O       DSPFILE.
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
              MOVE      1         TO   ERR-FLG
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
                PERFORM   CLR-HEAD2-RTN
                PERFORM   HEAD2-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                PERFORM   CLR-HEAD2-RTN
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
***  回線種別
*****IF       DSP-KAISHU  =  "I"  OR  "T"
***  2012/10/31
*****IF       DSP-KAISHU  =  "1"  OR  "2"
     IF       DSP-KAISHU  =  "1"  OR  "2"  OR  "3"
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-KAISHU
              MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-KAISHU
     ELSE
              IF   ERR-FLG   =    ZERO
                   MOVE      7    TO   ERR-FLG
              END-IF
              MOVE  "R"      TO   EDIT-OPTION   OF  DSP-KAISHU
              MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-KAISHU
*             GO             TO   HEAD2-CHK-EXIT
     END-IF.
*
***  回線制御番号
     IF       DSP-KAINO   NOT NUMERIC OR
              DSP-KAINO    =  ZERO
              IF   ERR-FLG   =    ZERO
                   MOVE      8    TO   ERR-FLG
              END-IF
              MOVE  "R"      TO   EDIT-OPTION   OF  DSP-KAINO
              MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-KAINO
*             GO             TO   HEAD2-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-KAINO
              MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-KAINO
     END-IF.
*
*    エラーがある場合はマスタ読込みしない
     IF       ERR-FLG        NOT  ZERO
              GO             TO   HEAD2-CHK-EXIT
     END-IF.
*
*回線種別マスタ ＲＥＡＤ
     PERFORM   JSMKAIF-READ.
***  処理区分＝１（登録）
     IF       DSP-SHORI   =   1
***           データが存在すればエラー
              IF    READ-FLG   =   ZERO
                    MOVE     3            TO   ERR-FLG
              ELSE
***           データが存在しなければＯＫ（ボディ入力へ）
                    MOVE    "3"           TO   PSW
***           登録時は使用ＦＬＧに空白を初期表示し入力不可に
                    MOVE     SPACE        TO   DSP-USEFLG
                    MOVE    "X"  TO  EDIT-STATUS OF DSP-USEFLG
              END-IF
     ELSE
***  処理区分＝２（修正），３（削除）
*             使用ＦＬＧを入力可に
              MOVE    " "  TO  EDIT-STATUS OF DSP-USEFLG
***           データが存在すればＯＫ
              IF    READ-FLG   =   ZERO
                    PERFORM   MOVE-DSP-SEC
***                 処理区分＝２（修正）は，ボディ入力へ
                    IF    DSP-SHORI  =  2
                          MOVE    "3"          TO   PSW
                    ELSE
***                 処理区分＝３（削除）は，確認入力へ
                          MOVE    "4"          TO   PSW
                    END-IF
              ELSE
***        データが存在しなければエラー
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
                PERFORM   CLR-BODY1-RTN
                PERFORM   BODY-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                PERFORM   CLR-BODY1-RTN
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
     MOVE     ZERO                TO   ERR-FLG.
*
*端末名
     IF       DSP-TANNM  =    SPACE
              IF   ERR-FLG   =    ZERO
                   MOVE      10   TO   ERR-FLG
              END-IF
              MOVE  "R"   TO   EDIT-OPTION    OF  DSP-TANNM
              MOVE  "C"   TO   EDIT-CURSOR    OF  DSP-TANNM
     END-IF.
*トリガーファイル名
     IF       DSP-TGFNM  =    SPACE
              IF   ERR-FLG   =    ZERO
                   MOVE      11   TO   ERR-FLG
              END-IF
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-TGFNM
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-TGFNM
     END-IF.
*アンサーファイル名
     IF       DSP-ASFNM  =    SPACE
              IF   ERR-FLG   =    ZERO
                   MOVE      12   TO   ERR-FLG
              END-IF
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-ASFNM
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-ASFNM
     END-IF.
*パラメータファイル名
     IF       DSP-PMFNM  =    SPACE
              IF   ERR-FLG   =    ZERO
                   MOVE      13   TO   ERR-FLG
              END-IF
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-PMFNM
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-PMFNM
     END-IF.
*使用ＦＬＧ
     IF     ( DSP-USEFLG  NOT =    SPACE )   AND
            ( DSP-USEFLG  NOT =    "1"   )
              IF   ERR-FLG   =    ZERO
                   MOVE      9    TO   ERR-FLG
              END-IF
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-USEFLG
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-USEFLG
*             GO          TO   BODY-CHK-EXIT
     ELSE
              IF       DSP-USEFLG     =  SPACE OR  1
                   IF    DSP-USEFLG  = SPACE
                         MOVE    NC"未使用"  TO  DSP-UFNM
                   ELSE
                         MOVE    NC"使用中"  TO  DSP-UFNM
                   END-IF
              ELSE
                   IF   ERR-FLG   =    ZERO
                        MOVE      9    TO   ERR-FLG
                   END-IF
                   MOVE  "R"     TO  EDIT-OPTION  OF  DSP-USEFLG
                   MOVE  "C"     TO  EDIT-CURSOR  OF  DSP-USEFLG
              END-IF
     END-IF.
*
     IF   ERR-FLG   =    ZERO
          MOVE     "4"       TO   PSW
     END-IF.
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
                    MOVE    "2"       TO   PSW
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
*        回線種別マスタ更新　処理区分＝１（登録）  2.4.1     *
****************************************************************
 FILE-WRT-SEC           SECTION.
     MOVE     "FILE-WRT-SEC"      TO   S-NAME.
*レコード初期クリア
     MOVE     SPACE               TO   KAI-REC.
     INITIALIZE                        KAI-REC.
*キー項目転送
***  回線種別
     MOVE     DSP-KAISHU          TO   KAI-F01.
***  回線制御番号
     MOVE     DSP-KAINO           TO   KAI-F02.
*明細項目
     PERFORM  MOVE-JSMKAIF.
*
***  登録担当者
     MOVE     PARA-TANTO          TO   KAI-F94.
***  登録日
     MOVE     SYS-DATE            TO   KAI-F95.
***  登録時間
     MOVE     WK-TIME(1:4)        TO   KAI-F96.
*回線種別マスタ登録
     WRITE    KAI-REC.
*マスタ登録のメッセージフラグ
     MOVE     1                   TO   MSG-FLG.
*
 FILE-WRT-EXIT.
     EXIT.
****************************************************************
*        回線種別マスタ更新　処理区分＝２（修正）  2.4.2     *
****************************************************************
 FILE-UPD-SEC           SECTION.
     MOVE     "FILE-UPD-SEC"      TO   S-NAME.
*キー項目転送
***  回線種別
     MOVE     DSP-KAISHU          TO   KAI-F01.
***  回線制御番号
     MOVE     DSP-KAINO           TO   KAI-F02.
*回線種別マスタ ＲＥＡＤ（存在すればレコード更新）
     PERFORM   JSMKAIF-READ.
     IF    READ-FLG   =   ZERO
           PERFORM   MOVE-JSMKAIF
*
***        更新担当者
           MOVE     PARA-TANTO    TO   KAI-F97
***        更新日
           MOVE     SYS-DATE      TO   KAI-F98
***        更新時間
           MOVE     WK-TIME(1:4)  TO   KAI-F99
*          更新処理
           REWRITE   KAI-REC
*          マスタ更新のメッセージフラグ
           MOVE      2            TO   MSG-FLG
     ELSE
           DISPLAY
           NC"未登録です　回線種別＝"   DSP-KAISHU  UPON CONS
           DISPLAY
           NC"　　　　回線制御番号＝"  DSP-KAINO  UPON CONS
     END-IF.
*
 FILE-UPD-EXIT.
     EXIT.
****************************************************************
*        回線種別マスタ更新　処理区分＝３（削除）  2.4.3     *
****************************************************************
 FILE-DLT-SEC           SECTION.
     MOVE     "FILE-DLT-SEC"      TO   S-NAME.
*キー項目転送
***  回線種別
     MOVE     DSP-KAISHU          TO   KAI-F01.
***  回線制御番号
     MOVE     DSP-KAINO           TO   KAI-F02.
*回線種別マスタ ＲＥＡＤ（存在すればレコード削除）
     PERFORM   JSMKAIF-READ.
     IF    READ-FLG   =   ZERO
           DELETE    JSMKAIF
*          マスタ更新のメッセージフラグ
           MOVE     3                   TO   MSG-FLG
     ELSE
           DISPLAY
           NC"未登録です　回線種別＝"  DSP-KAISHU  UPON CONS
           DISPLAY
           NC"　　　　回線制御番号＝"  DSP-KAINO   UPON CONS
     END-IF.
*
 FILE-DLT-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.
*
     MOVE    "SNJ0010I"      TO   DSP-PGID.
     MOVE    "FNJ00101"      TO   DSP-FORM.
*
*メッセージセット
     IF    ERR-FLG   =    ZERO
           IF    MSG-FLG   =    ZERO
                 MOVE    SPACE              TO   DSP-MSGSPC
           ELSE
                 MOVE    MSG-MSG-R(MSG-FLG) TO   DSP-MSGSPC
                 MOVE    ZERO               TO   MSG-FLG
           END-IF
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
     MOVE    "FNJ0010I"          TO   DSP-FMT.
     WRITE    DSP-FNJ0010I.
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

     MOVE    "FNJ0010I"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             項目転送  （回線種別マスタ → 画面）
****************************************************************
 MOVE-DSP-SEC           SECTION.
     MOVE     "MOVE-DSP-SEC"      TO   S-NAME.
*
*端末名
     MOVE    KAI-F03              TO   DSP-TANNM.
*トリガーファイル名
     MOVE    KAI-F04              TO   DSP-TGFNM.
*アンサーファイル名
     MOVE    KAI-F05              TO   DSP-ASFNM.
*パラメータファイル名
     MOVE    KAI-F06              TO   DSP-PMFNM.
*使用ＦＬＧ
     MOVE    KAI-F07              TO   DSP-USEFLG.
     IF      KAI-F07  =  SPACE
             MOVE       NC"未使用"     TO   DSP-UFNM
     ELSE
             MOVE       NC"使用中"     TO   DSP-UFNM
     END-IF.
*
 MOVE-DSP-EXIT.
     EXIT.
****************************************************************
*             項目転送  （画面 → 回線種別マスタ）
****************************************************************
 MOVE-JSMKAIF        SECTION.
     MOVE     "MOVE-JSMKAIF-SEC"  TO   S-NAME.
*
*端末名
     MOVE    DSP-TANNM            TO   KAI-F03.
*トリガーファイル名
     MOVE    DSP-TGFNM            TO   KAI-F04.
*アンサーファイル名
     MOVE    DSP-ASFNM            TO   KAI-F05.
*パラメータファイル名
     MOVE    DSP-PMFNM            TO   KAI-F06.
*使用ＦＬＧ
     MOVE    DSP-USEFLG           TO   KAI-F07.
*
 MOVE-JSMKAIF-EXIT.
     EXIT.
****************************************************************
*             回線種別マスタ　ＲＥＡＤ                         *
****************************************************************
 JSMKAIF-READ             SECTION.
     MOVE     "JSMKAIF-READ"      TO   S-NAME.
*
     MOVE    ZERO                 TO   READ-FLG.
*回線種別マスタＲＥＡＤ（該当データ無時，READ-FLG=1）
     MOVE    DSP-KAISHU           TO   KAI-F01.
     MOVE    DSP-KAINO            TO   KAI-F02.
     READ    JSMKAIF
         INVALID
             MOVE    1            TO   READ-FLG
     END-READ.
*
 JSMKAIF-READ-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*
     MOVE     "FNJ0010I"          TO   DSP-FORM.
     MOVE     "SNJ0010I"          TO   DSP-PGID.
*
*画面の初期化
     MOVE    SPACE                TO   DSP-FNJ0010I.
*システム日付転送
     MOVE    SYS-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:6)         TO   DSP-STIME.
*リバース，カーソルパーク解除
***  回線種別
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KAISHU.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KAISHU.
***  回線制御番号
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KAINO.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KAINO.
***  端末名
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TANNM.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TANNM.
***  トリガーファイル名
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TGFNM.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TGFNM.
***  アンサーファイル名
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-ASFNM.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-ASFNM.
***  パラメータファイル名
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-PMFNM.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-PMFNM.
***  使用ＦＬＧ
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-USEFLG.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-USEFLG.
*
 INT-DSP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ１属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD2-RTN          SECTION.
*
     MOVE     " "            TO   EDIT-CURSOR OF DSP-KAISHU
                                  EDIT-CURSOR OF DSP-KAINO.
     MOVE     "M"            TO   EDIT-OPTION OF DSP-KAISHU
                                  EDIT-OPTION OF DSP-KAINO.
 CLR-HEAD2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ２属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY1-RTN          SECTION.
*
     MOVE     " "            TO   EDIT-CURSOR OF DSP-TANNM
                                  EDIT-CURSOR OF DSP-TGFNM
                                  EDIT-CURSOR OF DSP-ASFNM
                                  EDIT-CURSOR OF DSP-PMFNM
                                  EDIT-CURSOR OF DSP-USEFLG.
     MOVE     "M"            TO   EDIT-OPTION OF DSP-TANNM
                                  EDIT-OPTION OF DSP-TGFNM
                                  EDIT-OPTION OF DSP-ASFNM
                                  EDIT-OPTION OF DSP-PMFNM
                                  EDIT-OPTION OF DSP-USEFLG.
 CLR-BODY1-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             DSPFILE
                       JSMKAIF.
**
 END-EXIT.
     EXIT.
*****************<<  SNJ0010I   END PROGRAM  >>******************
