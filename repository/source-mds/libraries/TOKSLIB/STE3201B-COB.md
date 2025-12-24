# STE3201B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/STE3201B.COB`

## ソースコード

```cobol
***********************************************************
*   顧客名　　    ：   _サカタのタネ殿                   *
*   システム名    ：   販売管理システム                   *
*   サブシステム名：   手書伝票発行                       *
*   プログラム名　：   カインズ                           *
*   プログラムID  ：   STE3201B（備考欄印字機能有り）     *
*   使用伝票      ：   ＜ターンアラウンドタイプ_型＞     *
*   作成者        ：   ナブ・アシスト                     *
*   作成日        ：   1999.05.19                         *
*   更新日／更新者：   2011.10.07 /YOSHIDA.M              *
*   更新概要      ：   基幹サーバ統合                     *
*   更新日／更新者：   2019.09.19 /T.TAKAHASHI            *
*   更新概要      ：   消費税率軽減税率対応　             *
***********************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            STE3201B.
 AUTHOR.                T.TAKAHASHI.
 DATE-WRITTEN.          99/05/19.
******************************************************************
 ENVIRONMENT            DIVISION.
******************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*伝票データ
     SELECT   HKYOTU1        ASSIGN    TO        DA-01-VI-JHTTEGL1
                             ORGANIZATION   IS   INDEXED
                             ACCESS    MODE IS   SEQUENTIAL
                             RECORD    KEY  IS   KYO-F01
                                                 KYO-F02
                                                 KYO-F04
                                                 KYO-F051
***2011.10.07 ST
                                                 KYO-F07
                                                 KYO-F112
***2011.10.07 EN
                                                 KYO-F03
                             FILE STATUS    IS   KYO-ST.
*プリンター
     SELECT     PRTF         ASSIGN    TO        GS-PRTF
                             DESTINATION        "PRT"
                             FORMAT              PRT-FORM
                             GROUP               PRT-GRP
                             PROCESSING          PRT-PROC
                             UNIT CONTROL        PRT-CTL
                             FILE      STATUS    PRT-ST.
*画面ファイル
     SELECT   DSPF           ASSIGN    TO        GS-DSPF
                             FORMAT              DSP-FMT
                             GROUP               DSP-GRP
                             PROCESSING          DSP-PRO
                             FUNCTION            DSP-FNC
                             STATUS              DSP-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*伝票データ
 FD  HKYOTU1
     BLOCK       CONTAINS  16        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        SHTDENF   OF        XFDLIB
     JOINING     KYO       AS        PREFIX.
*プリンター
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FTE32011  OF        XMDLIB.
*表示ファイル
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FTE32012  OF        XMDLIB.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*****ファイルステイタス
 01  ST-AREA.
     03  KYO-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
     03  PRT-ST                   PIC  X(02).
     03  IN-DATA                  PIC  X(01).
*****表示パラメータ
 01  FORM-PARA.
     03  DSP-FMT                  PIC X(08).
     03  DSP-PRO                  PIC X(02).
     03  DSP-GRP                  PIC X(08).
     03  DSP-FNC                  PIC X(04).
     03  DSP-CONTROL.
         05  DSP-CNTRL            PIC X(04).
         05  DSP-STR-PG           PIC X(02).
     03  PRT-FORM                 PIC X(08).
     03  PRT-PROC                 PIC X(02).
     03  PRT-GRP                  PIC X(08).
     03  PRT-CTL.
         05  PRT-CNTRL            PIC X(04).
         05  PRT-STR-PG           PIC X(02).
*****フラグエリア
 01  FLG-AREA.
     03  END-FLG                  PIC  9(01)     VALUE   ZERO.
     03  MAIN-END                 PIC  9(01)     VALUE   ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE   ZERO.
*****カウンタ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
*****日付
 01  DATE-WORK.
     03  DATE-HENKAN              PIC  9(08)     VALUE   ZERO.
*****ｺﾞｳｹｲ ｴﾘｱ
 01  GOUKEI.
     03  G-GENKA                  PIC  9(09)     VALUE   ZERO.
     03  G-BAIKA                  PIC  9(09)     VALUE   ZERO.
     03  G-SURYO                  PIC S9(09)V99  VALUE   ZERO.
     03  HENKAN-SURYO             PIC S9(09)V99  VALUE   ZERO.
     03  HENKAN-GENKA             PIC S9(09)V99  VALUE   ZERO.
     03  HENKAN-BAIKA             PIC S9(09)V99  VALUE   ZERO.
*****ｹｲｻﾝ ｴﾘｱ
 01  KEISAN.
     03  W-GENKA                  PIC  9(09)     VALUE   ZERO.
     03  W-BAIKA                  PIC  9(09)     VALUE   ZERO.
*****ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  WK-F111                  PIC  9(08)     VALUE   ZERO.
     03  WK-F15                   PIC  9(09)V99  VALUE   ZERO.
     03  WK-F172                  PIC  9(09)V99  VALUE   ZERO.
     03  WK-MAI                   PIC  9(06)     VALUE   ZERO.
     03  WRK-R040                 PIC  9(01)     VALUE   ZERO.
     03  RD-SW                    PIC  9(01)     VALUE   ZERO.
     03  I                        PIC  9(01)     VALUE   ZERO.
     03  DENPYO.
         05  FILLER               PIC  X(22)     VALUE
             "ｼｲﾚ ﾃﾞﾝﾋﾟｮｳ ﾊｯｺｳ ﾏｲｽｳ ".
         05  CNT-DENPYO           PIC  9(09)     VALUE   ZERO.
*****ﾃﾞﾝﾋﾟﾖｳ ｷ-
     03  WRK-DENNO.
         05  WK-DEN               PIC  9(09)     VALUE   ZERO.
     03  DENPYO-END               PIC  9(01)     VALUE   ZERO.
     03  OLD-F02                  PIC  9(09)     VALUE   ZERO.
     03  NEW-F02                  PIC  9(09)     VALUE   ZERO.
*#2019/09/18 NAV ST  消費税率取得サブ用
 01  WK-ZEIHENKAN.
     03  LINK-ZEIKBN              PIC  X(01).
     03  LINK-ZEIDATE             PIC  9(08).
     03  LINK-ZEIERR              PIC  X(01).
     03  LINK-ZEIRITU             PIC  9(05).
     03  LINK-DZEIRITU            PIC  9(05).
     03  LINK-ZEIRITU-H           PIC  9(03)V9(02).
     03  LINK-DZEIRITU-H          PIC  9(03)V9(02).
*#2019/09/18 NAV ED  消費税率取得サブ用
*****ﾒｯｾ-ｼﾞ ｴﾘｱ
 01  MSG-AREA.
     03  MSG01                PIC  N(30)     VALUE
         NC"対象データありません".
     03  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください".
     03  MSG03                PIC  N(30)     VALUE
         NC"『手書き伝票発行中』".
     03  MSG04                PIC  N(30)     VALUE
         NC"無効キーです".
     03  MSG05                PIC  N(30)     VALUE
         NC"開始が終了をこえています".
     03  MSG06                PIC  N(30)     VALUE
         NC"対象伝票がありません".
*****ファイルエラー
 01  KYO-ERR                      PIC  N(11)     VALUE
         NC"伝票データ　異常！！".
 01  DSP-ERR                      PIC  N(11)     VALUE
         NC"画面ファイル　異常！！".
 01  PRT-ERR                      PIC  N(11)     VALUE
         NC"プリンター　異常！！".
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
*****伝票データ
 KYO-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HKYOTU1.
     DISPLAY  KYO-ERR          UPON    CONS.
     DISPLAY  KYO-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
*****プリンター
 PRT-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      PRTF.
     DISPLAY  PRT-ERR          UPON    CONS.
     DISPLAY  PRT-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
*****画面ファイル
 DSP-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DSPF.
     DISPLAY  DSP-ERR          UPON    CONS.
     DISPLAY  DSP-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
******************************************************************
*            M  A  I  N          M  O  D  U  L  E                *
******************************************************************
 PROC-SEC                  SECTION.
*
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     MAIN-END  =  1.
     PERFORM     END-SEC.
*
     STOP        RUN.
*
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
*****各ファイルオープン
     OPEN        I-O       DSPF
                 INPUT     HKYOTU1
                 OUTPUT    PRTF.
*****画面初期化
     MOVE     SPACE              TO   FTE32012.
     MOVE     ZERO               TO   WK-DEN.
     MOVE     ZERO               TO   CNT-DENPYO.
*////PERFORM  DSP-WRT-SEC.
*****帳票初期化
     MOVE        SPACE     TO        FTE32011.
*****伝票枚数カウント
     MOVE     ZERO         TO    WK-MAI.
***2011.10.07 ST
     MOVE     SPACE        TO    KYO-REC.
     INITIALIZE                  KYO-REC.
***2011.10.07 EN
     MOVE     921084       TO    KYO-F01.
     MOVE     ZERO         TO    KYO-F02.
     MOVE     ZERO         TO    KYO-F04
                                 KYO-F051
                                 KYO-F03.
*****上記，条件以上にてファイル読込みスタート
***2011.10.07 ST
***  START    HKYOTU1       KEY  IS >=
***           KYO-F01 KYO-F02 KYO-F04 KYO-F051 KYO-F03
     START    HKYOTU1       KEY  IS >=
              KYO-F01  KYO-F02 KYO-F04 KYO-F051 KYO-F07
              KYO-F112 KYO-F03
***2011.10.07 EN
          INVALID  KEY
              DISPLAY NC"出力対象無し" UPON CONS
              STOP  RUN
     END-START.
*****伝票カウント処理へ
     PERFORM  DEN-CNT-SEC
              UNTIL         DENPYO-END = 9.
*****伝票枚数がゼロの時，メッセージ出力
     IF       WK-MAI        =      ZERO
              DISPLAY NC"出力対象無し" UPON CONS
              STOP  RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                       伝票_カウント                    *
***********************************************************
 DEN-CNT-SEC               SECTION.
*****キーの入れ替え
     MOVE     NEW-F02      TO   OLD-F02.
*****売上ファイル読込み（手書伝票用）
     READ     HKYOTU1
          AT END    MOVE   9    TO    DENPYO-END
          GO TO                       DEN-CNT-EXIT
     END-READ.
*****取引先コードチェック
     IF       KYO-F01      >    921084
          MOVE      9           TO    DENPYO-END
          GO TO                       DEN-CNT-EXIT
     END-IF.
*****キーのセット（伝票番号）
     MOVE     KYO-F02  TO        NEW-F02.
*****伝票出力総数カウント
     IF   OLD-F02      NOT =     NEW-F02
          ADD       1  TO        WK-MAI
     END-IF.
*
 DEN-CNT-EXIT.
     EXIT.
***********************************************************
*                       画面処理                          *
***********************************************************
 MAIN-SEC                  SECTION.
*****画面の表示
     PERFORM     DSP-WRT-SEC.
 MAIN-010.
*****入力グループのセット
     MOVE         "KUBUN"    TO        DSP-GRP.
     MOVE        SPACE       TO        ERRMSG.
*****画面の読込み
     PERFORM     DSP-RD-SEC.
*****ＰＦキーの判定
     EVALUATE    DSP-FNC
       WHEN
        "F005"
           MOVE    1         TO        MAIN-END
           GO                TO        MAIN-EXIT
       WHEN
        "E000"
           IF    KUBUN       NOT =     1 AND 2 AND 3
                 MOVE  MSG02       TO        ERRMSG
                 PERFORM                     DSP-WRT-SEC
                 GO                TO        MAIN-010
           END-IF
       WHEN
         OTHER
           MOVE    MSG04     TO        ERRMSG
           PERFORM                     DSP-WRT-SEC
           GO                TO        MAIN-010
     END-EVALUATE.
*****処理区分の判定
     MOVE        SPACE       TO        ERRMSG.
     PERFORM     DSP-WRT-SEC.
     EVALUATE      KUBUN
*********テストプリント
         WHEN      "1"
                   PERFORM   TEST-PRT-SEC
*********全伝票印字
         WHEN      "2"
                   MOVE      ZERO      TO   I
                   MOVE      ZERO      TO   KAIDEN
                   MOVE      ALL "9"   TO   ENDDEN
                   MOVE      MSG03     TO   ERRMSG
                   PERFORM                  DSP-WRT-SEC
                   PERFORM   KYO-START-SEC
                   PERFORM   KYO-READ-SEC
                             UNTIL     END-FLG   =   1
                   IF       (WK-DEN  NOT =   ZERO)
                             PERFORM   TAIL-EDT-SEC
                             PERFORM   DENP-WRT-SEC
                   END-IF

*********伝票_範囲指定
         WHEN      "3"
                   MOVE      ZERO      TO   I
                   PERFORM   KEY-IN-SEC
                   IF        MAIN-END  =  1
                             GO          TO   MAIN-EXIT
                   ELSE
                             MOVE MSG03  TO   ERRMSG
                             PERFORM          DSP-WRT-SEC
                   END-IF
                   PERFORM   KYO-START-SEC
                   PERFORM   KYO-READ-SEC
                             UNTIL     END-FLG   =   1
                   IF       (WK-DEN  NOT =   ZERO)
                             PERFORM   TAIL-EDT-SEC
                             PERFORM   DENP-WRT-SEC
                   END-IF
*********処理区分　１，２，３以外の時，エラー表示
         WHEN      OTHER
                   MOVE      MSG02     TO   ERRMSG
     END-EVALUATE.
*****画面再表示の為，プリントファイルクローズ
     CLOSE         PRTF.
     OPEN          OUTPUT    PRTF.
*****画面初期化
     MOVE          SPACE     TO  FTE32012.
     MOVE          ZERO      TO  WK-DEN.
     MOVE          ZERO      TO  END-FLG.
*****画面初期表示
     PERFORM       DSP-WRT-SEC.
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       伝票データ　ＳＴＡＲＴ　         *
**********************************************************
 KYO-START-SEC             SECTION.
*****スタートキーのセット
***2011.10.07 ST
     MOVE     SPACE        TO    KYO-REC.
     INITIALIZE                  KYO-REC.
***2011.10.07 EN
     MOVE     921084       TO    KYO-F01.
     MOVE     KAIDEN       TO    KYO-F02.
     MOVE     ZERO         TO    KYO-F04
                                 KYO-F051
                                 KYO-F03.
*****取引先コード，開始伝票_以上で売上ファイルスタート
***2011.10.07 ST
***  START    HKYOTU1       KEY  IS >=
***           KYO-F01 KYO-F02 KYO-F04 KYO-F051 KYO-F03
     START    HKYOTU1       KEY  IS >=
              KYO-F01  KYO-F02 KYO-F04 KYO-F051 KYO-F07
              KYO-F112 KYO-F03
***2011.10.07 EN
          INVALID  KEY
              MOVE     1    TO    END-FLG
              GO TO         KYO-START-EXIT
     END-START.
*
 KYO-START-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
*****各ファイルのクローズ
     CLOSE    HKYOTU1   PRTF.
*
 END-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 HEAD-EDT-SEC                 SECTION.
*****社名
     MOVE     "ｶｲﾝｽﾞ"             TO   HD01.
*****店舗名称
     MOVE     KYO-F30             TO   HD02.
*****法人コード
     MOVE     24                  TO   HOJIN.
*****店舗コード
     MOVE     KYO-F07             TO   HD03.
*****分類コード
     MOVE     KYO-F12             TO   HD04.
*****伝票区分
*****MOVE     KYO-F051            TO   HD05.
     MOVE     KYO-F132            TO   HD05.
*****伝票番号
     MOVE     KYO-F02             TO   HD06.
*****取引先コード
     MOVE     KYO-F01             TO   HD08.
*****連絡欄（取引先名称）
     MOVE     "(ｶﾌﾞ)ｻｶﾀﾉﾀﾈ"       TO   HD10.
*****発注日（パックタイプ変換）
     MOVE     KYO-F112            TO   DATE-HENKAN.
     MOVE     DATE-HENKAN(3:6)    TO   HD11.
*****納品日
*****便
*#2019/09/18 NAV ST 消費税軽減税率対応
*****消費税率取得サブ
     INITIALIZE                        WK-ZEIHENKAN.
     IF       KYO-F34  =  SPACE  OR "0"
              MOVE   "0"          TO   LINK-ZEIKBN
     ELSE
              MOVE   KYO-F34      TO   LINK-ZEIKBN
     END-IF.
     MOVE     KYO-F112            TO   LINK-ZEIDATE.
*
     CALL  "SKYTAXPG"  USING LINK-ZEIKBN LINK-ZEIDATE LINK-ZEIERR
                             LINK-ZEIRITU LINK-DZEIRITU.
     IF     LINK-ZEIERR  NOT =  "0"
            MOVE      ZERO        TO  LINK-DZEIRITU-H
            MOVE      ZERO        TO  LINK-ZEIRITU-H
     ELSE
            COMPUTE  LINK-ZEIRITU-H =  LINK-ZEIRITU / 100
            COMPUTE  LINK-DZEIRITU-H =  LINK-DZEIRITU / 100
     END-IF.
*
     IF     LINK-ZEIERR  =  "1"
            MOVE     LINK-DZEIRITU-H   TO  WTAX
            MOVE     "%"               TO  WTAXN
     ELSE
            MOVE     LINK-ZEIRITU-H    TO  WTAX
            MOVE     "%"               TO  WTAXN
     END-IF.
*#2019/09/18 NAV ED 消費税軽減税率対応
*
 HEAD-EDT-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 BODY-EDT-SEC                  SECTION.
     ADD         1         TO        I.
*****商品名
     MOVE     KYO-F1421           TO   DM01(I).
     MOVE     KYO-F1422           TO   DM02(I).
*****商品コード／ＪＡＮコード
     MOVE     KYO-F25             TO   DM03(I).
*****色／入数
*****サイズ／ケース
*****単位
*****数量
     IF  KYO-F1411  =  "00999961"
         MOVE 1                   TO   KYO-F15
     END-IF.
     MOVE     KYO-F15             TO   HENKAN-SURYO.
     MOVE     HENKAN-SURYO(1:9)   TO   DM07(I).
     MOVE     HENKAN-SURYO(10:1)  TO   DM08(I).
     ADD      HENKAN-SURYO        TO   G-SURYO.
*****原価単価（パックタイプ変換）
     MOVE     KYO-F172            TO   HENKAN-GENKA.
     MOVE     HENKAN-GENKA(1:9)   TO   DM09(I).
     MOVE     HENKAN-GENKA(10:2)  TO   DM10(I).
*****原価金額
     COMPUTE  W-GENKA  =  KYO-F15  *  KYO-F172.
     MOVE     W-GENKA             TO   DM11(I).
*****売価単価（パックタイプ変換）
     MOVE     KYO-F173            TO   HENKAN-BAIKA.
     MOVE     HENKAN-BAIKA(1:9)   TO   DM12(I).
*****売価金額
     COMPUTE  W-BAIKA  =  KYO-F15  *  KYO-F173.
     MOVE     W-BAIKA             TO   DM13(I).
*****原単価・売単価加算
     ADD      W-GENKA             TO   G-GENKA.
     ADD      W-BAIKA             TO   G-BAIKA.
 BODY-EDT-EXIT.
     EXIT.
**********************************************************
*                 合計データ編集書き出し                 *
**********************************************************
 TAIL-EDT-SEC                  SECTION.
*****数量合計
     MOVE     G-SURYO(1:9)        TO   TL01.
     MOVE     G-SURYO(10:1)       TO   TL02.
*****原価金額合計
     MOVE     G-GENKA             TO   TL03.
*****売価金額合計
     MOVE     G-BAIKA             TO   TL04.
******数量合計・原単価合計・売単価合計の初期化
     MOVE     ZERO                TO   G-SURYO.
     MOVE     ZERO                TO   G-GENKA.
     MOVE     ZERO                TO   G-BAIKA.
 TAIL-EDT-EXIT.
     EXIT.
**********************************************************
*                 データ編集書き出し                     *
**********************************************************
 DENP-WRT-SEC                  SECTION.
*****帳票書き出し
     MOVE       "FTE32011"
                           TO        PRT-FORM.
     MOVE       "SCREEN"   TO        PRT-GRP.
     WRITE       FTE32011.
*****帳票初期化
     MOVE        SPACE     TO        FTE32011.
     MOVE        ZERO      TO        I.
 DENP-WRT-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                           2.2       *
****************************************************************
 DSP-WRT-SEC          SECTION.
*****画面表示
     MOVE     SPACE     TO        FORM-PARA.
     MOVE     WK-MAI    TO        DENMAI.
     MOVE    "SCREEN"   TO        DSP-GRP.
     MOVE    "FTE32012" TO        DSP-FMT.
     WRITE    FTE32012.
*****メッセージのクリア
     MOVE     SPACE     TO        ERRMSG.
*
 DSP-WRT-EXIT.
     EXIT.
****************************************************************
*             画面ＲＥＡＤ処理                      2.3        *
****************************************************************
 DSP-RD-SEC           SECTION.
*****画面読込み制御記号転送
     MOVE    "NE"       TO        DSP-PRO.
     READ     DSPF.
*
 DSP-RD-EXIT.
     EXIT.
****************************************************************
*             範囲指定処理                            2.4      *
****************************************************************
 KEY-IN-SEC             SECTION.
 KEY-IN-01.
*****入力グループのセット
     MOVE         "NE"       TO   DSP-PRO.
     MOVE         "KEY01"    TO   DSP-GRP.
*****画面の読込み
     PERFORM       DSP-RD-SEC.
*****ＰＦキーの判定
     EVALUATE      DSP-FNC
         WHEN     "F005"
                   MOVE      1         TO   MAIN-END
                   GO                  TO   KEY-IN-EXIT
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      MSG04     TO   ERRMSG
                   GO                  TO   KEY-IN-EXIT
     END-EVALUATE.
*****エラーチェック
*****開始伝票番号入力チェック
     IF  (KAIDEN  NOT  NUMERIC)
         MOVE    ZERO      TO        KAIDEN.
*****終了伝票番号入力チェック
     IF  (ENDDEN  NOT  NUMERIC)
     OR  (ENDDEN = ZERO)
         MOVE  ALL "9"     TO        ENDDEN.
*****出力区分チェック
     IF  (KUBUN  =  "1"  OR  "2"  OR  "3")
         MOVE   "M"        TO        EDIT-OPTION  OF    KUBUN
         MOVE    SPACE     TO        EDIT-CURSOR  OF    KUBUN
       ELSE
         MOVE   "R"        TO        EDIT-OPTION  OF    KUBUN
         MOVE   "C"        TO        EDIT-CURSOR  OF    KUBUN
         MOVE    MSG02     TO        ERRMSG
         PERFORM DSP-WRT-SEC
         GO                TO        KEY-IN-01.
*****伝票_大小チェック
     IF  (KAIDEN > ENDDEN)
         MOVE   "R"        TO        EDIT-OPTION  OF    KAIDEN
         MOVE   "R"        TO        EDIT-OPTION  OF    ENDDEN
         MOVE   "C"        TO        EDIT-CURSOR  OF    KAIDEN
         MOVE    MSG05     TO        ERRMSG
         PERFORM DSP-WRT-SEC
         GO                TO        KEY-IN-01
       ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF    KAIDEN
         MOVE   "M"        TO        EDIT-OPTION  OF    ENDDEN
         MOVE    SPACE     TO        EDIT-CURSOR  OF    KAIDEN
     END-IF.
*****項目表示
     PERFORM     DSP-WRT-SEC.
*
 KEY-IN-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                  2.5.1      *
****************************************************************
 KYO-READ-SEC            SECTION.
*****売上ファイルの読込み
 READ-000.
     READ     HKYOTU1   AT        END
              MOVE      1         TO   END-FLG
              MOVE      MSG01     TO   ERRMSG
              GO                  TO   KYO-READ-EXIT
     END-READ.
*****取引先コードが範囲外の時対象外
     IF       KYO-F01  >   921084
              MOVE      1         TO   END-FLG
              GO                  TO   KYO-READ-EXIT
     END-IF.
*****開始伝票_以下の時読み飛ばし
     IF      (KYO-F02  <   KAIDEN)
              GO                  TO   READ-000
     END-IF.
*****終了伝票_以下の時読み飛ばし
     IF      (KYO-F02  >   ENDDEN)
              MOVE      1         TO   END-FLG
              GO                  TO   KYO-READ-EXIT
     END-IF.
*****格納伝票番号＝ＺＥＲＯの時
     IF      (WK-DEN  =   ZERO)
              PERFORM   HEAD-EDT-SEC
              MOVE      KYO-F02   TO   WK-DEN.
*****行_＝８０の時（備考欄のセット）
     IF      (KYO-F03  =   80)
              MOVE      KYO-F1421 TO   BIKO1
              MOVE      KYO-F1422 TO   BIKO2
              GO                  TO   READ-000.
*****行_＝９０の時
     IF      (KYO-F03  =   90)
              GO                  TO   READ-000.
 READ-010.
*****伝票_ブレイク時処理
     IF       KYO-F02   NOT =     WK-DEN
              PERFORM   TAIL-EDT-SEC
              PERFORM   DENP-WRT-SEC
              PERFORM   HEAD-EDT-SEC
              MOVE      KYO-F02   TO   WK-DEN
     END-IF.
*****ボディーセット処理
     PERFORM  BODY-EDT-SEC.
*
 KYO-READ-EXIT.
     EXIT.
****************************************************************
*             テストプリント                        2.6        *
****************************************************************
 TEST-PRT-SEC           SECTION.
*****明細エリア転送
     PERFORM   VARYING   I   FROM  1  BY  1   UNTIL   I  >  6

          MOVE   ALL "9"         TO   DM04(I) DM05(I)
                                      DM07(I) DM08(I) DM09(I)
                                      DM10(I) DM11(I) DM12(I)
                                      DM13(I)
          MOVE   ALL "*"         TO   DM01(I) DM02(I) DM03(I)
     END-PERFORM.
*****数字タイプ転送
     MOVE   ALL "9"              TO   HD03    HD05    HD06
                                      HD08    HD10    HOJIN
                                      HD11    TL01
                                      TL02    TL03    TL04.
*#2019/09/17 NAV ST
     MOVE   ALL "9"              TO   WTAX.
*#2019/09/17 NAV ED
*****文字タイプ転送
     MOVE   ALL "*"              TO   HD01    HD02    HD04
                                      BIKO1   BIKO2.
*#2019/09/17 NAV ST
     MOVE   ALL "*"              TO   WTAXN.
*#2019/09/17 NAV ED
*
*****伝票発行
     PERFORM  DENP-WRT-SEC.
*****テストプリント終了
     MOVE       1                TO   END-FLG.
*
 TEST-PRT-EXIT.
     EXIT.
******************************************************************
 END PROGRAM  STE3201B.
******************************************************************

```
