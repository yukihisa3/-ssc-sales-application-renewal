****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ホーマックセンター納品対応        *
*    業務名　　　　　　　：　センター納品対応　　　　          *
*    モジュール名　　　　：　オフライン納品ラベル発行指示      *
*    作成日／更新日　　　：  14/08/20                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY87AAI.
 AUTHOR.               NAV.
 DATE-WRITTEN.         14/08/19.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
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
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY87AA1  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
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
             VALUE NC"選択した発注種別区分に誤りがあります。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"選択した形状区分に誤りがあります。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"部門コードが違います。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"発行枚数を入力して下さい。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　　".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　　　　　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  9   PIC   N(20).
*
 01  FILE-ERR.
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
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
 01  LINK-KBN1           PIC  X(02).
 01  LINK-KBN2           PIC  X(02).
 01  LINK-BUMON          PIC  9(03).
 01  LINK-SURYO          PIC  9(03).
*
**************************************************************
 PROCEDURE             DIVISION   USING  LINK-KBN1
                                         LINK-KBN2
                                         LINK-BUMON
                                         LINK-SURYO.
**************************************************************
 DECLARATIVES.
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
*発注種別区分チェック
     IF       DSP-KBN1  =  "1" OR "2" OR "3" OR "4" OR "5"
              CONTINUE
     ELSE
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-KBN1
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-KBN1
     END-IF.
*形状区分
     IF       DSP-KBN2  =  "1" OR "2" OR "3" OR "4"
              CONTINUE
     ELSE
              IF     ERR-FLG  =  ZERO
                     MOVE    2    TO    ERR-FLG
              END-IF
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-KBN2
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-KBN2
     END-IF.
*部門入力
     IF       DSP-BUMON  NOT NUMERIC
     OR       DSP-BUMON  =   ZERO
              IF     ERR-FLG  =  ZERO
                     MOVE    4    TO    ERR-FLG
              END-IF
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BUMON
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BUMON
     ELSE
         EVALUATE     DSP-BUMON
         WHEN    001
           MOVE NC"園芸用品・大型機械"     TO   DSP-BUMONN
         WHEN    002
           MOVE NC"農業・業務資材"         TO   DSP-BUMONN
         WHEN    003
           MOVE NC"用土・肥料・薬品"       TO   DSP-BUMONN
         WHEN    004
           MOVE NC"植物"                   TO   DSP-BUMONN
         WHEN    005
           MOVE NC"エクステリア・屋外資材" TO   DSP-BUMONN
         WHEN    006
           MOVE NC"作業用品"               TO   DSP-BUMONN
         WHEN    007
           MOVE NC"金物"                   TO   DSP-BUMONN
         WHEN    008
           MOVE NC"工具"                   TO   DSP-BUMONN
         WHEN    009
           MOVE NC"塗料・補修"             TO   DSP-BUMONN
         WHEN    010
           MOVE NC"木材・建築資材"         TO   DSP-BUMONN
         WHEN    011
           MOVE NC"カー用品"               TO   DSP-BUMONN
         WHEN    012
           MOVE NC"スポーツ・玩具"         TO   DSP-BUMONN
         WHEN    013
           MOVE NC"サイクル・レジャー"     TO   DSP-BUMONN
         WHEN    014
           MOVE NC"ペット"                 TO   DSP-BUMONN
         WHEN    015
           MOVE NC"日用消耗品"             TO   DSP-BUMONN
         WHEN    016
           MOVE NC"文具"                   TO   DSP-BUMONN
         WHEN    017
           MOVE NC"ダイニング・キッチン"   TO   DSP-BUMONN
         WHEN    018
           MOVE NC"バス・トイレタリー"     TO   DSP-BUMONN
         WHEN    019
           MOVE NC"ＨＢＣ"                 TO   DSP-BUMONN
         WHEN    020
           MOVE NC"食品・酒"               TO   DSP-BUMONN
         WHEN    021
           MOVE NC"インテリア"             TO   DSP-BUMONN
         WHEN    022
           MOVE NC"寝具"                   TO   DSP-BUMONN
         WHEN    023
           MOVE NC"家具収納"               TO   DSP-BUMONN
         WHEN    024
           MOVE NC"家庭電器"               TO   DSP-BUMONN
         WHEN    025
           MOVE NC"冷暖房・住宅設備"       TO   DSP-BUMONN
         WHEN    026
           MOVE NC"電材・照明"             TO   DSP-BUMONN
         WHEN    027
           MOVE NC"ＡＶ情報・カウンター商品" TO   DSP-BUMONN
         WHEN    028
           MOVE NC"テナント植物"            TO   DSP-BUMONN
         WHEN    029
           MOVE NC"テナントペット"          TO   DSP-BUMONN
         WHEN    030
           MOVE NC"灯油"                    TO   DSP-BUMONN
         WHEN    091
           MOVE NC"工事費"                  TO   DSP-BUMONN
         WHEN    095
           MOVE NC"催事"                    TO   DSP-BUMONN
         WHEN    OTHER
           MOVE NC"＊＊＊＊＊＊"            TO   DSP-BUMONN
             IF     ERR-FLG  =  ZERO
                     MOVE    4    TO    ERR-FLG
              END-IF
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-KBN1
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-KBN2
         END-EVALUATE
     END-IF.
*発行枚数
     IF       DSP-MAISU  NOT NUMERIC
     OR       DSP-MAISU  =   ZERO
              IF     ERR-FLG  =  ZERO
                     MOVE    5    TO    ERR-FLG
              END-IF
              MOVE "R"    TO EDIT-OPTION OF DSP-MAISU
              MOVE "C"    TO EDIT-CURSOR OF DSP-MAISU
     END-IF.
*
     IF  ERR-FLG  NOT =  ZERO
         GO            TO    PARA-CHK-EXIT
     END-IF.
*
     MOVE    "2"       TO    PSW.
     PERFORM  DSP-SYOKI-SEC.
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
                MOVE    DSP-KBN1      TO   LINK-KBN1
                MOVE    DSP-KBN2      TO   LINK-KBN2
                MOVE    DSP-BUMON     TO   LINK-BUMON
                MOVE    DSP-MAISU     TO   LINK-SURYO
                MOVE    "END"         TO   END-FLG
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
     MOVE    "FSY87AA1"          TO   DSP-FMT.
     WRITE    DSP-FSY87AA1.
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
     MOVE    "FSY87AA1"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FSY87AA1.
*システム日付転送
     MOVE    SYS-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:6)         TO   DSP-STIME.
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
***  発注種別区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KBN1.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KBN1.
***  形状区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KBN2.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KBN2.
***  部門
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BUMON.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BUMON.
***  発行枚数
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-MAISU.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-MAISU.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY87AAI   END PROGRAM  >>******************
