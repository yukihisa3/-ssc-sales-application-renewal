****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　回線種別　　　　　　　　　        *
*    モジュール名　　　　：　回線種別マスタ保守                *
*    作成日／更新日　　　：　99/09/03                          *
*    作成者／更新者　　　：　ＮＡＶ萩原                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJH0008I.
 AUTHOR.               HAGIWARA.
 DATE-WRITTEN.         99/09/03.
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
     SELECT  JHMKAIF   ASSIGN    TO        JHMKAIL1
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
 FD  JHMKAIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMKAIF   OF   XFDLIB
                       JOINING   KAI       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FJH00081  OF   XMDLIB
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
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
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
             VALUE NC"正しい番号を入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"該当データが存在しません".
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
             VALUE NC"Ｉ又はＴを入力して下さい".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"０又は１を入力して下さい".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS   8  PIC   N(20).
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
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 KAI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMKAIF.
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
     OPEN     I-O       JHMKAIF.
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
***  回線種別
     IF       DSP-KAICD   =  "I"  OR  "T"
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-KAICD
              MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-KAICD
     ELSE
              MOVE   7       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION   OF  DSP-KAICD
              MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-KAICD
              GO             TO   HEAD2-CHK-EXIT
     END-IF.
*
***  優先順位
     IF       DSP-YUNO    NOT NUMERIC OR
              DSP-YUNO    =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION   OF  DSP-YUNO
              MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-YUNO
              GO             TO   HEAD2-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-YUNO
              MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-YUNO
     END-IF.
*
*
*回線種別マスタ ＲＥＡＤ
     PERFORM   JHMKAIF-READ.
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
*回線ＬＩＢ名称
     IF       DSP-KLIBNM  =    SPACE
              MOVE   5    TO   ERR-FLG
              MOVE  "R"   TO   EDIT-OPTION    OF  DSP-KLIBNM
              MOVE  "C"   TO   EDIT-CURSOR    OF  DSP-KLIBNM
              GO          TO   BODY-CHK-EXIT
     ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-KLIBNM
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-KLIBNM
     END-IF.
*集信ファイル名称
     IF       DSP-SYUFNM  =    SPACE
              MOVE   5    TO   ERR-FLG
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-SYUFNM
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-SYUFNM
              GO          TO   BODY-CHK-EXIT
     ELSE
              MOVE  "M"     TO  EDIT-OPTION  OF  DSP-SYUFNM
              MOVE   SPACE  TO  EDIT-CURSOR  OF  DSP-SYUFNM
     END-IF.
*使用状態
     IF       DSP-SIYOCD     NOT NUMERIC
              MOVE   8    TO   ERR-FLG
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-SIYOCD
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-SIYOCD
              GO          TO   BODY-CHK-EXIT
     ELSE
              IF       DSP-SIYOCD     =  ZERO OR  1
                   IF    DSP-SIYOCD  = ZERO
                         MOVE    NC"未使用"  TO  DSP-SIYONM
                   ELSE
                         MOVE    NC"使用中"  TO  DSP-SIYONM
                   END-IF
                   MOVE  "M"     TO  EDIT-OPTION  OF  DSP-SIYOCD
                   MOVE   SPACE  TO  EDIT-CURSOR  OF  DSP-SIYOCD
              ELSE
                   MOVE   8      TO  ERR-FLG
                   MOVE  "R"     TO  EDIT-OPTION  OF  DSP-SIYOCD
                   MOVE  "C"     TO  EDIT-CURSOR  OF  DSP-SIYOCD
                   GO            TO  BODY-CHK-EXIT
              END-IF
     END-IF.
*
*集信ファイル創成
     IF       DSP-SYUCRT  =  SPACE
              MOVE   5    TO   ERR-FLG
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-SYUCRT
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-SYUCRT
              GO          TO   BODY-CHK-EXIT
     ELSE
              MOVE  "M"     TO  EDIT-OPTION  OF  DSP-SYUCRT
              MOVE   SPACE  TO  EDIT-CURSOR  OF  DSP-SYUCRT
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
*        回線種別マスタ更新　処理区分＝１（登録）  2.4.1     *
****************************************************************
 FILE-WRT-SEC           SECTION.
     MOVE     "FILE-WRT-SEC"      TO   S-NAME.
*レコード初期クリア
     MOVE     SPACE               TO   KAI-REC.
     INITIALIZE                        KAI-REC.
*キー項目転送
***  回線種別
     MOVE     DSP-KAICD           TO   KAI-F01.
***  優先順位
     MOVE     DSP-YUNO            TO   KAI-F02.
*明細項目
     PERFORM  MOVE-JHMKAIF.
*回線種別マスタ登録
     WRITE    KAI-REC.
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
     MOVE     DSP-KAICD           TO   KAI-F01.
***  優先順位
     MOVE     DSP-YUNO            TO   KAI-F02.
*回線種別マスタ ＲＥＡＤ（存在すればレコード更新）
     PERFORM   JHMKAIF-READ.
     IF    READ-FLG   =   ZERO
           PERFORM   MOVE-JHMKAIF
           REWRITE   KAI-REC
     ELSE
           DISPLAY
           NC"未登録です　回線種別＝"   DSP-KAICD  UPON CONS
           DISPLAY
           NC"　　　　　　優先順位＝"  DSP-YUNO   UPON CONS
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
     MOVE     DSP-KAICD           TO   KAI-F01.
***  優先順位
     MOVE     DSP-YUNO            TO   KAI-F02.
*回線種別マスタ ＲＥＡＤ（存在すればレコード削除）
     PERFORM   JHMKAIF-READ.
     IF    READ-FLG   =   ZERO
           DELETE    JHMKAIF
     ELSE
           DISPLAY
           NC"未登録です　回線種別＝"  DSP-KAICD  UPON CONS
           DISPLAY
           NC"　　　　　　優先順位＝"  DSP-YUNO   UPON CONS
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
     MOVE    "FJH00081"          TO   DSP-FMT.
     WRITE    DSP-FJH00081.
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

     MOVE    "FJH00081"           TO   DSP-FMT.
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
*回線ＬＩＢ名称
     MOVE    KAI-F03              TO   DSP-KLIBNM.
*集信ファイル名称
     MOVE    KAI-F04              TO   DSP-SYUFNM.
*使用状態
     MOVE    KAI-F05              TO   DSP-SIYOCD.
     IF      KAI-F05  =  0
             MOVE       NC"未使用"     TO   DSP-SIYONM
     ELSE
             MOVE       NC"使用中"     TO   DSP-SIYONM
     END-IF.
*集信ファイル創成
     MOVE    KAI-F06              TO   DSP-SYUCRT.
*
 MOVE-DSP-EXIT.
     EXIT.
****************************************************************
*             項目転送  （画面 → 回線種別マスタ）
****************************************************************
 MOVE-JHMKAIF        SECTION.
     MOVE     "MOVE-JHMKAIF-SEC"  TO   S-NAME.
*
*回線ＬＩＢ名称
     MOVE    DSP-KLIBNM           TO   KAI-F03.
*集信ファイル名称
     MOVE    DSP-SYUFNM           TO   KAI-F04.
*使用状態
     MOVE    DSP-SIYOCD           TO   KAI-F05.
*集信ファイル創成
     MOVE    DSP-SYUCRT           TO   KAI-F06.
*
 MOVE-JHMKAIF-EXIT.
     EXIT.
****************************************************************
*             回線種別マスタ　ＲＥＡＤ                         *
****************************************************************
 JHMKAIF-READ             SECTION.
     MOVE     "JHMKAIF-READ"      TO   S-NAME.
*
     MOVE    ZERO                 TO   READ-FLG.
*回線種別マスタＲＥＡＤ（該当データ無時，READ-FLG=1）
     MOVE    DSP-KAICD            TO   KAI-F01.
     MOVE    DSP-YUNO             TO   KAI-F02.
     READ    JHMKAIF
         INVALID
             MOVE    1            TO   READ-FLG
     END-READ.
*
 JHMKAIF-READ-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*画面の初期化
     MOVE    SPACE                TO   DSP-FJH00081.
*システム日付転送
     MOVE    SYS-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:6)         TO   DSP-STIME.
*リバース，カーソルパーク解除
***  回線種別
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KAICD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KAICD.
***  優先順位
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-YUNO.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-YUNO.
***  回線ＬＩＢ名称
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KLIBNM.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KLIBNM.
***  集信ファイル名称
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SYUFNM.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SYUFNM.
***  使用状態
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SIYOCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SIYOCD.
***  集信ファイル創成
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SYUCRT.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SYUCRT.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             DSPFILE
                       JHMKAIF.
**
 END-EXIT.
     EXIT.
*****************<<  SJH0008I   END PROGRAM  >>******************
