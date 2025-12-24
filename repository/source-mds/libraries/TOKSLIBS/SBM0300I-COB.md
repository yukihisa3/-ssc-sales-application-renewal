# SBM0300I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBM0300I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　流通ＢＭＳオンライン　　　　　　　*
*    業務名　　　　　　　：　マスタ管理　　　　　　　　        *
*    モジュール名　　　　：　納品センターマスタ保守            *
*    作成日／更新日　　　：　2012/11/05                        *
*    作成者／更新者　　　：　ＮＡＶ武井                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SBM0300I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         12/11/05.
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
*納品センターマスタ
     SELECT  SOKCETF   ASSIGN    TO        SOKCETL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SCT-F01  SCT-F02
                                           SCT-F03
                       FILE      STATUS    SCT-ST.
*画面定義ファイル
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*
*倉庫マスタ
     SELECT     ZSOKMS     ASSIGN    TO    DA-01-VI-ZSOKMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SOK-F01
                           FILE      STATUS    SOK-ST.
*店舗マスタ
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE      STATUS    TEN-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 納品センターマスタ                                 *
****************************************************************
 FD  SOKCETF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SOKCETF   OF   XFDLIB
                       JOINING   SCT       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FBM0300I  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
*倉庫マスタ
 FD  ZSOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSOKMS    OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
*店舗マスタ
 FD  HTENMS.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  SCT-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
**
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
 01  FORM-PGID              PIC   X(08)  VALUE "SBM0300I".
 01  FORM-NAME              PIC   X(08)  VALUE "FBM0300I".
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  MSG-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
 01  WKSOK-MSTFLG                 PIC  X(01).
 01  WKTOK-MSTFLG                 PIC  X(01).
 01  WKTEN-MSTFLG                 PIC  X(01).
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
             VALUE NC"正しい納品センターを入力して下さい".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"納品センターマスタに登録済です".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"納品センターマスタに未登録です".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"データを入力して下さい".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫マスタに未登録です".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先マスタに未登録です".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"店舗マスタに未登録です".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"エラーです　　　　　　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  10  PIC   N(20).
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
     03  SCT-ERR           PIC N(15) VALUE
                        NC"納品センターマスタエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  DSP-ERR           PIC N(15) VALUE
                        NC"画面ファイルエラー".
     03  SOK-ERR           PIC N(15) VALUE
                        NC"倉庫マスタエラー".
     03  TEN-ERR           PIC N(15) VALUE
                        NC"店舗マスタエラー".
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
 SCT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SOKCETF.
     DISPLAY     SCT-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SCT-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TEN-ST    UPON      CONS.
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
     OPEN     I-O       SOKCETF   DSPFILE.
     OPEN     INPUT     ZSOKMS  HTOKMS  HTENMS.
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
     IF       DSP-SOKCD   NOT  =  SPACE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SOKCD
              MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SOKCD
     ELSE
              IF   ERR-FLG   =    ZERO
                   MOVE      7    TO   ERR-FLG
              END-IF
              MOVE  "R"      TO   EDIT-OPTION   OF  DSP-SOKCD
              MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-SOKCD
*             GO             TO   HEAD2-CHK-EXIT
     END-IF.
*
***  取引先コード
     IF       DSP-TOKCD   NOT NUMERIC OR
              DSP-TOKCD    =  ZERO
              IF   ERR-FLG   =    ZERO
                   MOVE      8    TO   ERR-FLG
              END-IF
              MOVE  "R"      TO   EDIT-OPTION   OF  DSP-TOKCD
              MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-TOKCD
*             GO             TO   HEAD2-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-TOKCD
              MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-TOKCD
     END-IF.
*** 店舗コード
     IF       DSP-TENCD   NOT NUMERIC OR
              DSP-TENCD    =  ZERO
              IF   ERR-FLG   =    ZERO
                   MOVE      8    TO   ERR-FLG
              END-IF
              MOVE  SPACE    TO   DSP-TENNM
              MOVE  "R"      TO   EDIT-OPTION   OF  DSP-TENCD
              MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-TENCD
*             GO             TO   HEAD2-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-TENCD
              MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-TENCD
     END-IF.
*
     MOVE    SPACE           TO  WKSOK-MSTFLG
                                 WKTOK-MSTFLG
                                 WKTEN-MSTFLG.
*
** 倉庫マスタ
     MOVE  DSP-SOKCD             TO  SOK-F01.
     READ  ZSOKMS
         INVALID
***   存在しなければＮＧ（空白→画面）
           MOVE  SPACE       TO  DSP-SOKNM
           MOVE  "N"         TO  WKSOK-MSTFLG
*
         NOT  INVALID
***  存在すればＯＫ
           MOVE  SOK-F02       TO  DSP-SOKNM
           MOVE  "M"           TO  EDIT-OPTION   OF  DSP-SOKCD
           MOVE   SPACE        TO  EDIT-CURSOR   OF  DSP-SOKCD
     END-READ.
*削除は参照マスタ判定　不要
     IF  DSP-SHORI    =   3
         GO  TO   HEAD2-CHK-100
     END-IF.
     IF  WKSOK-MSTFLG  NOT  =  SPACE
         IF  ERR-FLG  =  ZERO
             MOVE    7     TO  ERR-FLG
         END-IF
         MOVE  "R"         TO  EDIT-OPTION   OF  DSP-SOKCD
         MOVE  "C"         TO  EDIT-CURSOR   OF  DSP-SOKCD
     END-IF.
 HEAD2-CHK-100.
** 取引先マスタ
     MOVE  DSP-TOKCD             TO  TOK-F01.
     READ  HTOKMS
         INVALID
***   存在しなければＮＧ（空白→画面）
           MOVE  SPACE       TO  DSP-TOKNM
           MOVE  "N"         TO  WKTOK-MSTFLG
*
         NOT  INVALID
***  存在すればＯＫ
           MOVE  TOK-F02       TO  DSP-TOKNM
           MOVE  "M"           TO  EDIT-OPTION   OF  DSP-TOKCD
           MOVE   SPACE        TO  EDIT-CURSOR   OF  DSP-TOKCD
     END-READ.
*削除は参照マスタ判定　不要
     IF  DSP-SHORI    =   3
         GO  TO   HEAD2-CHK-200
     END-IF.
     IF  WKTOK-MSTFLG  NOT  =  SPACE
         IF  ERR-FLG  =  ZERO
             MOVE    8     TO  ERR-FLG
         END-IF
         MOVE  "R"         TO  EDIT-OPTION   OF  DSP-TOKCD
         MOVE  "C"         TO  EDIT-CURSOR   OF  DSP-TOKCD
     END-IF.
 HEAD2-CHK-200.
** 店舗マスタ
     MOVE  DSP-TENCD             TO  TEN-F011.
     MOVE  DSP-TOKCD             TO  TEN-F52.
     READ  HTENMS
         INVALID
***   存在しなければＮＧ（空白→画面）
           MOVE  SPACE       TO  DSP-TENNM
           MOVE  "N"         TO  WKTEN-MSTFLG
*
         NOT  INVALID
***  存在すればＯＫ
           MOVE  TEN-F02       TO  DSP-TENNM
           MOVE  "M"           TO  EDIT-OPTION   OF  DSP-TENCD
           MOVE   SPACE        TO  EDIT-CURSOR   OF  DSP-TENCD
     END-READ.
*削除は参照マスタ判定　不要
     IF  DSP-SHORI    =   3
         GO  TO   HEAD2-CHK-300
     END-IF.
     IF  WKTEN-MSTFLG  NOT  =  SPACE
         IF  ERR-FLG  =  ZERO
             MOVE    9     TO  ERR-FLG
         END-IF
         MOVE  "R"         TO  EDIT-OPTION   OF  DSP-TENCD
         MOVE  "C"         TO  EDIT-CURSOR   OF  DSP-TENCD
     END-IF.
 HEAD2-CHK-300.
*    エラーがある場合はマスタ読込みしない
     IF       ERR-FLG        NOT  =  ZERO
              GO             TO   HEAD2-CHK-EXIT
     END-IF.
*納品センターマスタ ＲＥＡＤ
     PERFORM   SOKCETF-READ.
*
***  処理区分＝１（登録）
     IF       DSP-SHORI   =   1
***           データが存在すればエラー
              IF    READ-FLG   =   ZERO
                    MOVE     3            TO   ERR-FLG
              ELSE
***           データが存在しなければＯＫ（ボディ入力へ）
                    MOVE    "3"           TO   PSW
              END-IF
     ELSE
***  処理区分＝２（修正），３（削除）
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
*納品センターコード
     IF       DSP-NCENCD  =  ZERO
              IF   ERR-FLG   =    ZERO
                   MOVE      10   TO   ERR-FLG
              END-IF
              MOVE  "R"   TO   EDIT-OPTION    OF  DSP-NCENCD
              MOVE  "C"   TO   EDIT-CURSOR    OF  DSP-NCENCD
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
*    納品センターマスタ更新　処理区分＝１（登録）  2.4.1     *
****************************************************************
 FILE-WRT-SEC           SECTION.
     MOVE     "FILE-WRT-SEC"      TO   S-NAME.
*レコード初期クリア
     MOVE     SPACE               TO   SCT-REC.
     INITIALIZE                        SCT-REC.
*キー項目転送
***  倉庫コード
     MOVE     DSP-SOKCD           TO   SCT-F01.
***  取引先コード
     MOVE     DSP-TOKCD           TO   SCT-F02.
***  店舗コード
     MOVE     DSP-TENCD           TO   SCT-F03.
*明細項目 *************************************
     MOVE     DSP-NCENCD          TO   SCT-F04.
*
***  登録担当者
     MOVE     PARA-TANTO          TO   SCT-F94.
***  登録日
     MOVE     SYS-DATE            TO   SCT-F95.
***  登録時間
     MOVE     WK-TIME(1:4)        TO   SCT-F96.
*マスタ登録
     WRITE    SCT-REC.
*マスタ登録のメッセージフラグ
     MOVE     1                   TO   MSG-FLG.
*
 FILE-WRT-EXIT.
     EXIT.
****************************************************************
*       納品センターマスタ更新　処理区分＝２（修正）  2.4.2 *
****************************************************************
 FILE-UPD-SEC           SECTION.
     MOVE     "FILE-UPD-SEC"      TO   S-NAME.
*キー項目転送
***  倉庫コード
     MOVE     DSP-SOKCD           TO   SCT-F01.
***  取引先コード
     MOVE     DSP-TOKCD           TO   SCT-F02.
***  店舗コード
     MOVE     DSP-TENCD           TO   SCT-F03.
*マスタ ＲＥＡＤ（存在すればレコード更新）
     PERFORM   SOKCETF-READ.
     IF    READ-FLG   =   ZERO
***  明細転送
           MOVE     DSP-NCENCD    TO   SCT-F04
*
***        更新担当者
           MOVE     PARA-TANTO    TO   SCT-F97
***        更新日
           MOVE     SYS-DATE      TO   SCT-F98
***        更新時間
           MOVE     WK-TIME(1:4)  TO   SCT-F99
*          更新処理
           REWRITE   SCT-REC
*          マスタ更新のメッセージフラグ
           MOVE      2            TO   MSG-FLG
     ELSE
           DISPLAY
           NC"未登録です　キー＝"   DSP-SOKCD
           DSP-TOKCD  DSP-TENCD        UPON  CONS
     END-IF.
*
 FILE-UPD-EXIT.
     EXIT.
****************************************************************
*        納品センターマスタ更新　処理区分＝３（削除）  2.4.3 *
****************************************************************
 FILE-DLT-SEC           SECTION.
     MOVE     "FILE-DLT-SEC"      TO   S-NAME.
*キー項目転送
***  倉庫コード
     MOVE     DSP-SOKCD           TO   SCT-F01.
***  取引先コード
     MOVE     DSP-TOKCD           TO   SCT-F02.
***  店舗コード
     MOVE     DSP-TENCD           TO   SCT-F03.
*マスタ ＲＥＡＤ（存在すればレコード削除）
     PERFORM   SOKCETF-READ.
     IF    READ-FLG   =   ZERO
           DELETE    SOKCETF
*          マスタ更新のメッセージフラグ
           MOVE     3                   TO   MSG-FLG
     ELSE
           DISPLAY
           NC"未登録です　キー項目＝"  DSP-SOKCD
                      DSP-TOKCD  DSP-TENCD  UPON  CONS
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
     MOVE    FORM-PGID       TO   DSP-PGID.
     MOVE    FORM-NAME       TO   DSP-FORM.
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
***  MOVE    FORM-NAME           TO   DSP-FMT.
     MOVE    "FBM0300I"          TO   DSP-FMT.
     WRITE    DSP-FBM0300I.
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

***  MOVE    FORM-NAME            TO   DSP-FMT.
     MOVE    "FBM0300I"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             項目転送  （納品センターマスタ → 画面）
****************************************************************
 MOVE-DSP-SEC           SECTION.
     MOVE     "MOVE-DSP-SEC"      TO   S-NAME.
*
*納品センターコード
     MOVE    SCT-F04              TO   DSP-NCENCD.
*
 MOVE-DSP-EXIT.
     EXIT.
****************************************************************
*         納品センターマスタ　ＲＥＡＤ                         *
****************************************************************
 SOKCETF-READ             SECTION.
     MOVE     "SOKCETF-READ"      TO   S-NAME.
*
     MOVE    ZERO                 TO   READ-FLG.
*キー項目転送
***  倉庫コード
     MOVE     DSP-SOKCD           TO   SCT-F01.
***  取引先コード
     MOVE     DSP-TOKCD           TO   SCT-F02.
***  店舗コード
     MOVE     DSP-TENCD           TO   SCT-F03.
*マスタＲＥＡＤ（該当データ無時，READ-FLG=1）
*
     READ    SOKCETF
         INVALID
             MOVE    1            TO   READ-FLG
     END-READ.
*
 SOKCETF-READ-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*
     MOVE     FORM-NAME           TO   DSP-FORM.
     MOVE     FORM-PGID           TO   DSP-PGID.
*
*画面の初期化
     MOVE    SPACE                TO   DSP-FBM0300I.
*システム日付転送
     MOVE    SYS-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:6)         TO   DSP-STIME.
*リバース，カーソルパーク解除
***
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SOKCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SOKCD.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SOKNM.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SOKNM.
***
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TOKCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TOKCD.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TOKNM.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TOKNM.
***
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TENCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TENCD.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TENNM.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TENNM.
***  納品センターコード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NCENCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NCENCD.
*
 INT-DSP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ１属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD2-RTN          SECTION.
*
     MOVE     " "            TO   EDIT-CURSOR OF DSP-SOKCD
                                  EDIT-CURSOR OF DSP-TOKCD
                                  EDIT-CURSOR OF DSP-TENCD.
     MOVE     "M"            TO   EDIT-OPTION OF DSP-SOKCD
                                  EDIT-OPTION OF DSP-TOKCD
                                  EDIT-OPTION OF DSP-TENCD.
 CLR-HEAD2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ２属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY1-RTN          SECTION.
*
     MOVE     " "            TO   EDIT-CURSOR OF DSP-NCENCD.
     MOVE     "M"            TO   EDIT-OPTION OF DSP-NCENCD.
 CLR-BODY1-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE    SOKCETF  DSPFILE.
     CLOSE    ZSOKMS   HTOKMS   HTENMS.
**
 END-EXIT.
     EXIT.
*****************<<  SBM0300I   END PROGRAM  >>******************

```
