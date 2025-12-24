****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＤＣＭセンター納品対応            *
*    業務名　　　　　　　：　納品ラベル発行　　　　　          *
*    モジュール名　　　　：　納品ラベル抽出　　　　            *
*    作成日／更新日　　　：  21/02/25                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NSY1200I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         21/02/25.
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
*店舗マスタ
     SELECT  HTENMS    ASSIGN    TO        TENMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TEN-F52
                                           TEN-F011
                       FILE      STATUS    TEN-ST.
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
*    FILE = 取引先マスタ
****************************************************************
 FD  HTOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 店舗マスタ
****************************************************************
 FD  HTENMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTENMS    OF   XFDLIB
                       JOINING   TEN       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY12001  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TOK-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
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
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HTENMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
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
             VALUE NC"店舗ＣＤを入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"部門ＣＤを指定して下さい".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"個口数を入力して下さい".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"店舗マスタ未登録です".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先マスタを入力して下さい".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先マスタ未登録です".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  11  PIC   N(20).
*
 01  FILE-ERR.
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  TEN-ERR           PIC N(20) VALUE
                        NC"店舗マスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面エラー".
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
 01  LINK-STTEN          PIC  9(05).
 01  LINK-BUMON          PIC  9(03).
 01  LINK-KOGUTI         PIC  9(03).
 01  LINK-TOKCD          PIC  9(08).
*
**************************************************************
 PROCEDURE             DIVISION   USING  LINK-STTEN
                                         LINK-BUMON
                                         LINK-KOGUTI
                                         LINK-TOKCD.
**************************************************************
 DECLARATIVES.
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
*
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
     OPEN     INPUT     HTOKMS  HTENMS.
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
                IF   ERR-FLG = ZERO
                     MOVE "2"    TO   PSW
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
*
     MOVE     ZERO               TO   ERR-FLG.
     PERFORM  DSP-SYOKI-SEC.
*取引先コード範囲
     IF     DSP-TOKCD  NOT NUMERIC
     OR     DSP-TOKCD  =   ZERO
            MOVE   6       TO   ERR-FLG
            MOVE  "R"      TO   EDIT-OPTION  OF  DSP-TOKCD
            MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-TOKCD
            GO             TO   PARA-CHK-EXIT
     ELSE
            MOVE DSP-TOKCD  TO  TOK-F01
            PERFORM  HTOKMS-READ-SEC
            IF   HTOKMS-INV-FLG = "INV"
                 MOVE   7       TO   ERR-FLG
                 MOVE  "R"      TO   EDIT-OPTION  OF  DSP-TOKCD
                 MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-TOKCD
                 GO             TO   PARA-CHK-EXIT
            ELSE
                 MOVE  TOK-F03  TO   DSP-TOKNM
            END-IF
     END-IF.
*店舗コード範囲
     IF     DSP-STTEN  NOT NUMERIC
     OR     DSP-STTEN  =   ZERO
            MOVE   1       TO   ERR-FLG
            MOVE  "R"      TO   EDIT-OPTION  OF  DSP-STTEN
            MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-STTEN
            GO             TO   PARA-CHK-EXIT
     ELSE
            MOVE DSP-TOKCD  TO  TEN-F52
            MOVE DSP-STTEN  TO  TEN-F011
            PERFORM  HTENMS-READ-SEC
            IF   HTENMS-INV-FLG = "INV"
                 MOVE   5       TO   ERR-FLG
                 MOVE  "R"      TO   EDIT-OPTION  OF  DSP-STTEN
                 MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-STTEN
                 GO             TO   PARA-CHK-EXIT
            ELSE
                 MOVE  TEN-F02  TO   DSP-TENNM
            END-IF
     END-IF.
*部門コードチェック
     IF     DSP-BUMON  NOT NUMERIC
     OR     DSP-BUMON  =   ZERO
            MOVE   2       TO   ERR-FLG
            MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BUMON
            MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BUMON
            GO             TO   PARA-CHK-EXIT
     END-IF.
*
     EVALUATE    DSP-BUMON
         WHEN    001
         MOVE NC"園芸用品・大型機械"            TO   DSP-BUMNM
         WHEN    002
         MOVE NC"農業・業務資材"                TO   DSP-BUMNM
         WHEN    003
         MOVE NC"用土・肥料・薬品"              TO   DSP-BUMNM
         WHEN    004
         MOVE NC"植物"                          TO   DSP-BUMNM
         WHEN    005
         MOVE NC"エクステリア・屋外資材"        TO   DSP-BUMNM
         WHEN    006
         MOVE NC"作業用品"                      TO   DSP-BUMNM
         WHEN    007
         MOVE NC"金物"                          TO   DSP-BUMNM
         WHEN    008
         MOVE NC"工具"                          TO   DSP-BUMNM
         WHEN    009
         MOVE NC"塗料・補修"                    TO   DSP-BUMNM
         WHEN    010
         MOVE NC"木材・建築資材"                TO   DSP-BUMNM
         WHEN    011
         MOVE NC"カー用品"                      TO   DSP-BUMNM
         WHEN    012
         MOVE NC"スポーツ・玩具"                TO   DSP-BUMNM
         WHEN    013
         MOVE NC"サイクル・レジャー"            TO   DSP-BUMNM
         WHEN    014
         MOVE NC"ペット"                        TO   DSP-BUMNM
         WHEN    015
         MOVE NC"日用消耗品"                    TO   DSP-BUMNM
         WHEN    016
         MOVE NC"文具"                          TO   DSP-BUMNM
         WHEN    017
         MOVE NC"ダイニング・キッチン"          TO   DSP-BUMNM
         WHEN    018
         MOVE NC"バス・トイレタリー"            TO   DSP-BUMNM
         WHEN    019
         MOVE NC"ＨＢＣ"                        TO   DSP-BUMNM
         WHEN    020
         MOVE NC"食品・酒"                      TO   DSP-BUMNM
         WHEN    021
         MOVE NC"インテリア"                    TO   DSP-BUMNM
         WHEN    022
         MOVE NC"寝具"                          TO   DSP-BUMNM
         WHEN    023
         MOVE NC"家具収納"                      TO   DSP-BUMNM
         WHEN    024
         MOVE NC"家庭電器"                      TO   DSP-BUMNM
         WHEN    025
         MOVE NC"冷暖房・住宅設備"              TO   DSP-BUMNM
         WHEN    026
         MOVE NC"電材・照明"                    TO   DSP-BUMNM
         WHEN    027
         MOVE NC"ＡＶ情報・カウンター商品"      TO   DSP-BUMNM
         WHEN    028
         MOVE NC"テナント植物"                  TO   DSP-BUMNM
         WHEN    029
         MOVE NC"テナントペット"                TO   DSP-BUMNM
         WHEN    030
         MOVE NC"灯油"                          TO   DSP-BUMNM
         WHEN    091
         MOVE NC"工事費"                        TO   DSP-BUMNM
         WHEN    095
         MOVE NC"催事"                          TO   DSP-BUMNM
         WHEN    OTHER
            MOVE   2       TO   ERR-FLG
            MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BUMON
            MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BUMON
            GO             TO   PARA-CHK-EXIT
     END-EVALUATE.
*個口数入力
     IF     DSP-KOGUTI NOT NUMERIC
     OR     DSP-KOGUTI =   ZERO
            MOVE   4       TO   ERR-FLG
            MOVE  "R"      TO   EDIT-OPTION  OF  DSP-KOGUTI
            MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-KOGUTI
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
                MOVE    DSP-STTEN     TO   LINK-STTEN
                MOVE    DSP-BUMON     TO   LINK-BUMON
                MOVE    DSP-KOGUTI    TO   LINK-KOGUTI
                MOVE    DSP-TOKCD     TO   LINK-TOKCD
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
     MOVE    "FSY12001"          TO   DSP-FMT.
     WRITE    DSP-FSY12001.
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
                MOVE    "HANI"    TO   DSP-GRP
*確認
         WHEN   "2"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FSY12001"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FSY12001.
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
*エラー初期化
     MOVE    ZERO                 TO   ERR-FLG.
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
***  個口数
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KOGUTI.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KOGUTI.
***  部門コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BUMON.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BUMON.
***  店舗ＣＤ開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-STTEN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-STTEN.
***  取引先ＣＤ　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TOKCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TOKCD.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             取引先マスタ読込み
****************************************************************
 HTOKMS-READ-SEC       SECTION.
*
     READ   HTOKMS
            INVALID       MOVE  "INV"    TO  HTOKMS-INV-FLG
            NOT INVALID   MOVE  SPACE    TO  HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             店舗マスタ読込み
****************************************************************
 HTENMS-READ-SEC       SECTION.
*
     READ   HTENMS
            INVALID       MOVE  "INV"    TO  HTENMS-INV-FLG
            NOT INVALID   MOVE  SPACE    TO  HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             HTOKMS  HTENMS  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY7981I   END PROGRAM  >>******************
