# STE3102B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/STE3102B.COB`

## ソースコード

```cobol
***********************************************************
*   顧客名　　    ：   _サカタのタネ殿                   *
*   システム名    ：   販売管理システム                   *
*   サブシステム名：   手書伝票発行　                     *
*   プログラム名　：   カーマ２１手書伝票発行　　　       *
*   プログラムID  ：   STE3102B                           *
*   作成者        ：   ナブ・アシスト　                   *
*   作成日        ：   1999.05.28                         *
*   更新日／更新者：   2011.10.07 / YOSHIDA.M             *
*   更新概要      ：   基幹サーバ統合                     *
***********************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            STE3102B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          99/05/28.
*
******************************************************************
*                                                                *
 ENVIRONMENT            DIVISION.
*                                                                *
******************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
            YA     IS   YA.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*    伝票データ
     SELECT   HKYOTU1            ASSIGN    TO    DA-01-VI-JHTTEGL1
                                 ORGANIZATION   IS   INDEXED
                                 ACCESS    MODE IS   SEQUENTIAL
                                 RECORD    KEY  IS   KYO-F01
                                                     KYO-F02
                                                     KYO-F04
                                                     KYO-F051
***2011.10.07(KYO-F07,KYO-F112)
                                                     KYO-F07
                                                     KYO-F112
                                                     KYO-F03
                                 FILE STATUS    IS   KYO-ST.
*    画面ファイル
     SELECT   DSPFILE            ASSIGN    TO        GS-DSPF
                                 FORMAT              DSP-FMT
                                 GROUP               DSP-GRP
                                 PROCESSING          DSP-PRO
                                 FUNCTION            DSP-FNC
                                 STATUS              DSP-ST.
*    プリントファイル
     SELECT   PRINTF             ASSIGN    TO     LP-04-PRTF.
*--------------------------------------------------------------*
 DATA                   DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*****＜伝票データ＞*****
 FD  HKYOTU1
     BLOCK       CONTAINS  16        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        SHTDENF   OF        XFDLIB
     JOINING     KYO       AS        PREFIX.
*****＜画面　ファイル＞*****
 FD  DSPFILE            LABEL RECORD   IS   OMITTED.
*
     COPY    FTE31021   OF   XMDLIB.
*
*****＜プリント　ファイル＞*****
 FD  PRINTF                       LINAGE    IS   30   LINES.
 01  PRT-REC                      PIC       X(200).
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
***  ｽﾃｰﾀｽ ｴﾘｱ
 01  STATUS-AREA.
     03  KYO-ST                   PIC  X(02).
     03  PRT-STATUS               PIC  X(02).
***  ｶﾞﾒﾝ ｺﾝﾄﾛｰﾙ ｴﾘｱ
 01  DSP-CNTL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
     03  DSP-ST                   PIC  X(02).
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)     VALUE   ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE   ZERO.
     03  SYORI-FLG                PIC  X(03)     VALUE   SPACE.
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
***  備考
 01  WK-BIKO.
     03  WK-BIKO01                PIC  X(20)     VALUE   SPACE.
     03  WK-BIKO02                PIC  X(20)     VALUE   SPACE.
***  ｺﾞｳｹｲ ｴﾘｱ
 01  GOUKEI.
     03  G-GENKA                  PIC  9(09)     VALUE   ZERO.
     03  G-BAIKA                  PIC  9(09)     VALUE   ZERO.
***  ｹｲｻﾝ ｴﾘｱ
 01  KEISAN.
     03  W-GENKA                  PIC  9(09)V9   VALUE   ZERO.
     03  W-BAIKA                  PIC  9(09)     VALUE   ZERO.
*    03  W-SURYO                  PIC  9(06)V9   VALUE   ZERO.
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  WK-TORICD                PIC  9(08)     VALUE  13938.
     03  WK-F111                  PIC  9(08)     VALUE   ZERO.
     03  WK-F112                  PIC  9(08)     VALUE   ZERO.
     03  WK-F172                  PIC  9(09)V99  VALUE   ZERO.
     03  WRK-MAI                  PIC  9(06)     VALUE   ZERO.
     03  WRK-R040                 PIC  9(01)     VALUE   ZERO.
     03  RD-SW                    PIC  9(01)     VALUE   ZERO.
     03  AAA                      PIC  X(01)     VALUE   SPACE.
     03  DENPYO.
         05  FILLER               PIC  X(22)     VALUE
             "ｼｲﾚ ﾃﾞﾝﾋﾟｮｳ ﾊｯｺｳ ﾏｲｽｳ ".
         05  CNT-DENPYO           PIC  9(09)     VALUE   ZERO.
*--- ﾃﾞﾝﾋﾟﾖｳ ｷ-
     03  WRK-DENNO.
         05  DENNO1               PIC  9(08)     VALUE   ZERO.
*        05  SINSEINO             PIC  9(08)     VALUE   ZERO.
     03  DENPYO-END               PIC  9(01)     VALUE   ZERO.
     03  OLD-F02                  PIC  9(09)     VALUE   ZERO.
     03  NEW-F02                  PIC  9(09)     VALUE   ZERO.
*    ﾒﾂｾ-ｼﾞ ｴﾘｱ
 01  MSG-AREA.
     03  MSG-FIELD.
         05  MSG01                PIC  N(30)     VALUE
         NC"出力対象データがありません。　".
         05  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください。".
         05  MSG03                PIC  N(30)     VALUE
         NC"伝票発行中".
         05  MSG04                PIC  N(30)     VALUE
         NC"無効キーです".
         05  MSG04                PIC  N(30)     VALUE
         NC"無効キーです".
     03  MSG-FIELD-R     REDEFINES    MSG-FIELD.
         05  MSG-TBL     OCCURS     4      PIC  N(30).
*
 01  FILE-ERR010                  PIC  N(10)     VALUE
         NC"プリンター　異常！！".
 01  FILE-ERR020                  PIC  N(14)     VALUE
         NC"印刷を続けますか？　Ｙ／Ｎ".
 01  FILE-ERR030                  PIC  N(11)     VALUE
         NC"伝票データＦ　異常！！".
 01  FILE-ERR040                  PIC  N(11)     VALUE
         NC"画面ファイル　異常！！".
***  ﾌﾟﾘﾝﾄ ｴﾘｱ
*    ﾐﾀﾞｼ
 01  HEAD-00.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  HD001                    PIC  X(08) VALUE "ｶｰﾏ21   ".
*
 01  HEAD-01   CHARACTER          TYPE IS        YA.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  HD01                     PIC  X(10).
     03  FILLER                   PIC  X(15)     VALUE   SPACE.
     03  HD02                     PIC  ZZZZ9.
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  HD03                     PIC  X(04).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD04                     PIC  X(02).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD05                     PIC  ZZZ999999.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD06                     PIC  ZZZZZ9.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  HD061                    PIC  N(10).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD07                     PIC  ZZZZZZ.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD08                     PIC  ZZZZZZ.
     03  FILLER                   PIC  X(31)     VALUE   SPACE.
*
*    ﾒｲｻｲ
 01  BODY-01.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  MS01                     PIC  X(15).
     03  FILLER                   PIC  X(114)    VALUE   SPACE.
*
 01  BODY-02.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  MS02                     PIC  X(15).
     03  FILLER                   PIC  X(08)     VALUE   SPACE.
     03  MS03                     PIC  X(13).
     03  FILLER                   PIC  X(16)     VALUE   SPACE.
     03  MS04                     PIC  ZZZ9.
     03  FILLER                   PIC  X(14)     VALUE   SPACE.
     03  MS05                     PIC  ZZZZZ9.
     03  MS06                     PIC  ZZ.
     03  MS07                     PIC  ZZZZZZZZ9.
     03  MS08                     PIC  ZZZZZ9.
     03  MS09                     PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(27)     VALUE   SPACE.
*
*    ｺﾞｳｹｲ
 01  TAIL-01.
     03  FILLER                   PIC  X(85)     VALUE   SPACE.
     03  TL01                     PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  TL02                     PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(27)     VALUE   SPACE.
 01  TAIL-02.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  BIKO01                   PIC  X(20).
 01  TAIL-03.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  BIKO02                   PIC  X(20).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
******************************************************************
*                       SHORI                         0.0.0      *
******************************************************************
 DECLARATIVES.
*--- << ﾌﾟﾘﾝﾀ- ｴﾗ- >> ---*
 000-PRINTF-ERR         SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      PRINTF.
     DISPLAY  FILE-ERR010      UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     STOP     RUN.
*--- << ﾃﾞﾝﾋﾟｮｳ ﾃﾞ-ﾀ ｴﾗ- >> ---*
 000-MAST-ERR           SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HKYOTU1.
     DISPLAY  FILE-ERR030      UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     DISPLAY  KYO-ST           UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     STOP     RUN.
*--- << ｶﾞﾒﾝ    ｴﾗ- >> ---*
 000-DSPFILE-ERR        SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DSPFILE.
     DISPLAY  FILE-ERR040      UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     DISPLAY  DSP-ST           UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     STOP     RUN.
 END DECLARATIVES.
******************************************************************
*            M  A  I  N          M  O  D  U  L  E                *
******************************************************************
 CONTROL-START          SECTION.
*
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC
                        UNTIL     END-FLG  =  "END".
     PERFORM            END-SEC.
*
 CONTROL-END.
     STOP     RUN.
****************************************************************
*             初期処理                              1.0        *
****************************************************************
 INIT-SEC               SECTION.
*    ファイルのＯＰＥＮ
     OPEN     INPUT     HKYOTU1.
     OPEN     I-O       DSPFILE.
*    ワーク初期化
     MOVE     ZERO               TO   DENNO1.
     MOVE     ZERO               TO   CNT-DENPYO.
     MOVE     SPACE              TO   END-FLG.
     MOVE     SPACE              TO   FTE31021.
*伝票枚数カウント
     MOVE     ZERO         TO    WRK-MAI.
***2011.10.07 ST
     MOVE     SPACE        TO    KYO-REC.
     INITIALIZE                  KYO-REC.
***2011.10.07 EN
     MOVE     WK-TORICD    TO    KYO-F01.
     MOVE     ZERO         TO    KYO-F02.
     MOVE     ZERO         TO    KYO-F04
                                 KYO-F051
                                 KYO-F03.
*
     START    HKYOTU1       KEY  IS >=
***2011.10.07 ST
***           KYO-F01 KYO-F02 KYO-F04 KYO-F051 KYO-F03
              KYO-F01  KYO-F02 KYO-F04 KYO-F051 KYO-F07
              KYO-F112 KYO-F03
***2011.10.07 EN
          INVALID  KEY
              DISPLAY NC"出力対象無し" UPON CONS
              STOP  RUN
     END-START.
*
     PERFORM  DEN-CNT-SEC
              UNTIL         DENPYO-END = 9.
*    出力枚数ゼロ件の場合
     IF       WRK-MAI       =      ZERO
              DISPLAY NC"出力対象無し" UPON CONS
              STOP  RUN
     END-IF.
 INIT-EXIT.
     EXIT.
****************************************************************
*                       伝票_カウント                1.1
****************************************************************
 DEN-CNT-SEC               SECTION.
*
     MOVE     NEW-F02      TO   OLD-F02.
*
 DEN000.
     READ     HKYOTU1
              AT END  MOVE  9   TO    DENPYO-END
              GO                TO    DEN-CNT-EXIT
     END-READ.
*    取引先コードブレイクチェック
     IF       KYO-F01      >    WK-TORICD
              MOVE      9       TO    DENPYO-END
              GO                TO    DEN-CNT-EXIT
     END-IF.
*    行_＝８０は，読み飛ばし
     IF       KYO-F03      =    80
              GO                TO    DEN-CNT-SEC
     END-IF.
*    カーマ２１店舗のみ抽出
     IF       KYO-F27F =  1
              CONTINUE
     ELSE
              GO                TO    DEN-CNT-SEC
     END-IF.
*    キーの入れ替え
     MOVE     KYO-F02           TO     NEW-F02.
*    伝票ブレイクチェック
     IF   OLD-F02     NOT =     NEW-F02
          ADD       1           TO     WRK-MAI
***       DISPLAY "OLD-F02 = " OLD-F02 UPON CONS
***       DISPLAY "NEW-F02 = " NEW-F02 UPON CONS
***       DISPLAY "KYO-F07 = " KYO-F07 UPON CONS
     END-IF.
*
 DEN-CNT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
*    画面の初期化
     MOVE          SPACE     TO   SYORI-FLG.
     MOVE          SPACE     TO   FTE31021.
*    画面表示
     PERFORM       DSP-WRITE-SEC.
*    キー入力
     MOVE         "MD04R"    TO   DSP-GRP.
     PERFORM       DSP-READ-SEC.
*    アテンション判定
     EVALUATE      DSP-FNC
         WHEN     "F005"
                   MOVE     "END"      TO   END-FLG
                   GO                  TO   MAIN-EXIT
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
     END-EVALUATE.
*処理対象未入力時処理
     IF   MD04  NOT  NUMERIC
          MOVE     2    TO   ERR-FLG
          PERFORM       ERR-MSG-SEC
          GO       TO   MAIN-SEC
     ELSE
          MOVE  MD04    TO   WRK-R040
     END-IF.
*
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*    プリントファイルのＯＰＥＮ
     OPEN     OUTPUT    PRINTF.
*    処理判定
     EVALUATE      MD04
*        テストプリント
         WHEN      1
                   PERFORM   TEST-PRT-SEC
*        全プリント
         WHEN      2
                   MOVE      ZERO      TO   MD02
                   MOVE      ALL "9"   TO   MD03
                   MOVE      3         TO   ERR-FLG
                   PERFORM   ERR-MSG-SEC
                   PERFORM   DSP-WRITE-SEC
                   CLOSE     HKYOTU1
                   OPEN      INPUT     HKYOTU1
***2011.10.07 ST
                   MOVE     SPACE        TO    KYO-REC
                   INITIALIZE                  KYO-REC
***2011.10.07 EN
                   MOVE     WK-TORICD    TO    KYO-F01
                   MOVE     ZERO         TO    KYO-F02
                   MOVE     ZERO         TO    KYO-F04
                                         KYO-F051
                                         KYO-F03
*
                   START    HKYOTU1       KEY  IS >=
***2011.10.07 ST
***                 KYO-F01 KYO-F02 KYO-F04 KYO-F051 KYO-F03
                    KYO-F01  KYO-F02 KYO-F04 KYO-F051 KYO-F07
                    KYO-F112 KYO-F03
***2011.10.07 EN
                   END-START
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     SYORI-FLG =   "END"
*        範囲指定
         WHEN      3
                   PERFORM   KEY-IN-SEC
                   IF        END-FLG   =   "END"
                             GO        TO   MAIN-EXIT
                   END-IF
                   MOVE      3         TO   ERR-FLG
                   PERFORM   ERR-MSG-SEC
                   PERFORM   DSP-WRITE-SEC
                   CLOSE     HKYOTU1
                   OPEN      INPUT     HKYOTU1
***2011.10.07 ST
                   MOVE     SPACE        TO    KYO-REC
                   INITIALIZE                  KYO-REC
***2011.10.07 EN
                   MOVE     WK-TORICD    TO    KYO-F01
                   MOVE     ZERO         TO    KYO-F02
                   MOVE     ZERO         TO    KYO-F04
                                         KYO-F051
                                         KYO-F03
*
                   START    HKYOTU1       KEY  IS >=
***2011.10.07 ST
***                 KYO-F01 KYO-F02 KYO-F04 KYO-F051 KYO-F03
                    KYO-F01  KYO-F02 KYO-F04 KYO-F051 KYO-F07
                    KYO-F112 KYO-F03
***2011.10.07 EN
                   END-START
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
***                          UNTIL     END-FLG   =   "END"
                             UNTIL     SYORI-FLG =   "END"
         WHEN      OTHER
                   MOVE      2         TO   ERR-FLG
     END-EVALUATE.
*    プリントファイルＣＬＯＳＥ
     CLOSE         PRINTF    HKYOTU1.
*    次対象の入力為，ＲＥＡＤスイッチＯＦＦ
     MOVE          ZERO      TO       RD-SW.
*    伝票データＦＯＰＥＮ
     OPEN          INPUT     HKYOTU1.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             エラーメッセージセット                2.1        *
****************************************************************
 ERR-MSG-SEC            SECTION.
*
     IF       ERR-FLG   =   ZERO
              MOVE   SPACE                 TO   MD05
     ELSE
              MOVE   MSG-TBL ( ERR-FLG )   TO   MD05
              MOVE   ZERO                  TO   ERR-FLG
     END-IF.
*
 ERR-MSG-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                           2.2       *
****************************************************************
 DSP-WRITE-SEC          SECTION.
*    伝票総枚数セット
     MOVE     WRK-MAI   TO        MD01.
     MOVE     SPACE     TO        DSP-CNTL.
     MOVE    "SCREEN"   TO        DSP-GRP.
     MOVE    "FTE31021" TO        DSP-FMT.
*    画面表示
     WRITE    FTE31021.
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面ＲＥＡＤ処理                      2.3        *
****************************************************************
 DSP-READ-SEC           SECTION.
*
     MOVE    "NE"       TO        DSP-PRO.
     READ     DSPFILE.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             範囲指定処理                            2.4      *
****************************************************************
 KEY-IN-SEC             SECTION.
*
 KEY-IN-01.
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*    範囲指定
     MOVE         "KEY01"    TO   DSP-GRP.
     MOVE          ZERO      TO   MD02      MD03.
*    画面読込み
     PERFORM       DSP-READ-SEC.
*    アテンション判定
     EVALUATE      DSP-FNC
*        終了
         WHEN     "F005"
                   MOVE     "END"      TO   END-FLG
                   GO                  TO   KEY-IN-EXIT
*        実行
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
                   GO                  TO   KEY-IN-EXIT
     END-EVALUATE.
*    範囲指定大小チェック
     IF            MD02   >  MD03
                   MOVE      2         TO   ERR-FLG
                   GO                  TO   KEY-IN-01
     END-IF.
*    未入力時（全データ対象）
     IF           (MD02   =  ZERO)     AND
                  (MD03   =  ZERO)
                   MOVE      ZERO      TO   MD02
                   MOVE      ALL "9"   TO   MD03
     END-IF.
*
 KEY-IN-EXIT.
     EXIT.
****************************************************************
*             伝票出力処理                          2.5        *
****************************************************************
 DENP-WT-SEC            SECTION.
*    伝票_のブレイク判定
     IF       KYO-F02   NOT =     DENNO1
              PERFORM   TAIL-WT-SEC
              PERFORM   HEAD-WT-SEC
              MOVE      KYO-F02   TO   DENNO1
*             MOVE      KYO-F     TO   SINSEINO
     END-IF.
*    行カウンタの判定
     IF       L-CNT        >=     23
              PERFORM   TAIL-WT-SEC
              PERFORM   HEAD-WT-SEC
     END-IF.
*    項目初期化
     INITIALIZE                  BODY-01
                                 BODY-02.
*    明細項目セット
*    商品名（１）
     MOVE     KYO-F1421          TO   MS01.
*    商品名（２）
     MOVE     KYO-F1422          TO   MS02.
*    取引先商品コード
     MOVE     KYO-F25            TO   MS03.
*    数量
     MOVE     KYO-F15            TO   MS04.
*    原価単価
     MOVE     KYO-F172           TO   WK-F172.
     MOVE     WK-F172(1:9)       TO   MS05.
     MOVE     WK-F172(10:2)      TO   MS06.
*    原価金額
     COMPUTE  W-GENKA   =    KYO-F15  *  KYO-F172
     IF       W-GENKA(10:1)  >  4
              ADD  1             TO   W-GENKA
     END-IF.
     MOVE     W-GENKA            TO   MS07.
     COMPUTE  G-GENKA   =    G-GENKA  +  W-GENKA.
*    売価単価
     MOVE     KYO-F173           TO   MS08.
*    売価金額
     COMPUTE  W-BAIKA   =    KYO-F15  *  KYO-F173
     MOVE     W-BAIKA            TO   MS09.
     COMPUTE  G-BAIKA   =    G-BAIKA  +  W-BAIKA.
*
     WRITE    PRT-REC        FROM     BODY-01   AFTER     1.
     WRITE    PRT-REC        FROM     BODY-02   AFTER     1.
*
     ADD      2                  TO   L-CNT.
*
     PERFORM  FL-READ-SEC.
*
     IF       SYORI-FLG   =        "END"
              PERFORM   TAIL-WT-SEC
     END-IF.
 DENP-WT-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                  2.5.1      *
****************************************************************
 FL-READ-SEC            SECTION.
*    伝票データ読込み
 READ-000.
     READ     HKYOTU1   AT        END
              MOVE     "END"      TO   SYORI-FLG
              GO                  TO   FL-READ-EXIT
     END-READ.
*    取引先コードブレイクチェック
     IF       KYO-F01 >    WK-TORICD
              MOVE     "END"      TO   SYORI-FLG
              GO                  TO   FL-READ-EXIT
     END-IF.
*    カーマ２１店舗のみ抽出
*****IF       KYO-F07  =  214  OR  264
     IF       KYO-F27F =  1
              CONTINUE
     ELSE
              GO                  TO   FL-READ-SEC
     END-IF.
     IF       KYO-F03 >    20
              IF  KYO-F03  =  80
                  MOVE KYO-F1421  TO   WK-BIKO01
                  MOVE KYO-F1422  TO   WK-BIKO02
              END-IF
              GO                  TO   READ-000
     END-IF.
*    伝票番号範囲指定大小チェック
     IF       KYO-F02 <    MD02
              GO                  TO   READ-000
     END-IF.
     IF       KYO-F02 >    MD03
              GO                  TO   READ-000
     END-IF.
     IF       KYO-F02 <    MD02
              GO                  TO   READ-000
     END-IF.
 READ-010.
*    １件目時処理
     IF       RD-SW     =    0
              MOVE      KYO-F02   TO   DENNO1
              PERFORM   HEAD-WT-SEC
              MOVE      1         TO   RD-SW
     END-IF.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             ヘッド部出力処理                      2.5.2      *
****************************************************************
 HEAD-WT-SEC            SECTION.
*    ヘッド部項目初期化
     INITIALIZE                  HEAD-01.
*--- 項目セット
*    店舗名称
     MOVE     KYO-F30            TO    HD01.
*    店舗コード
     MOVE     KYO-F07            TO    HD02.
*    分類コード
     MOVE     KYO-F12            TO    HD03.
*    伝票区分
     MOVE     KYO-F132           TO    HD04.
*    伝票_
     MOVE     KYO-F02            TO    HD05.
*    取引先コード
     MOVE     KYO-F01            TO    HD06.
     MOVE     SPACE              TO    HD061.
     MOVE     NC"_　サカタのタネ　　" TO HD061.
*    発注日
     MOVE     KYO-F111           TO    WK-F111.
     MOVE     WK-F111(3:6)       TO    HD07.
*    納品日
     MOVE     KYO-F112           TO    WK-F112.
     MOVE     WK-F112(3:6)       TO    HD08.
*
     WRITE    PRT-REC          FROM   HEAD-00    AFTER    5.
     WRITE    PRT-REC          FROM   HEAD-01    AFTER    2.
*    印字位置送り
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  3.
*    行カウンタ
     MOVE     11               TO     L-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             テール部出力処理                      2.5.3      *
****************************************************************
 TAIL-WT-SEC            SECTION.
*    テイル部初期化
     INITIALIZE                  TAIL-01 TAIL-02 TAIL-03.
*    原価金額合計
     MOVE   G-GENKA              TO   TL01.
*    売価金額合計
     MOVE   G-BAIKA              TO   TL02.
*    備考欄セット
     MOVE   WK-BIKO01            TO   BIKO01.
     MOVE   WK-BIKO02            TO   BIKO02.
*    仕入申請書_
*****MOVE   SINSEINO             TO   TL03.
*    テイル印字制御
     COMPUTE   CNT-AFTER   =     25  -  L-CNT.
*    テイル行の印字
     WRITE    PRT-REC          FROM   TAIL-01   AFTER   CNT-AFTER.
*    備考印字
     WRITE    PRT-REC          FROM   TAIL-02   AFTER   2.
     WRITE    PRT-REC          FROM   TAIL-03   AFTER   1.
*    次頁へ改頁
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  PAGE.
*    ワーク初期化
     MOVE     ZERO             TO     G-GENKA G-BAIKA.
     MOVE     SPACE            TO     BIKO01  BIKO02
                                      WK-BIKO01 WK-BIKO02.
*    伝票枚数
     ADD      1                TO     CNT-DENPYO.
*
 TAIL-WT-EXIT.
     EXIT.
****************************************************************
*             テストプリント                        2.6        *
****************************************************************
 TEST-PRT-SEC           SECTION.

     INITIALIZE                  HEAD-01
                                 BODY-01  BODY-02
                                 TAIL-01  TAIL-02  TAIL-03.
*    テスト印字項目セット
     MOVE   ALL "9"              TO   HD02    HD05    HD06
                                      HD07    HD08
                                      MS04    MS05    MS06
                                      MS07    MS08    MS09
                                      TL01    TL02
     MOVE   ALL "*"              TO   HD01    HD03    HD04
                                      MS01    MS02    MS03
                                      BIKO01  BIKO02.
*    ヘッド印字
     WRITE    PRT-REC          FROM   HEAD-00    AFTER    5.
     WRITE    PRT-REC          FROM   HEAD-01    AFTER    2.
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  3.
*    明細印字
     PERFORM   VARYING   L-CNT      FROM  1  BY  1
                                        UNTIL   L-CNT     >  6
              WRITE  PRT-REC   FROM   BODY-01   AFTER     1
              WRITE  PRT-REC   FROM   BODY-02   AFTER     1
     END-PERFORM.
*    テイル印字
     WRITE    PRT-REC          FROM   TAIL-01   AFTER     2.
     WRITE    PRT-REC          FROM   TAIL-02   AFTER     2.
     WRITE    PRT-REC          FROM   TAIL-03   AFTER     1.
*
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  PAGE.
*
 TEST-PRT-EXIT.
     EXIT.
****************************************************************
*             終了処理                              3.0        *
****************************************************************
 END-SEC                SECTION.
*    ファイルのＣＬＯＳＥ
     CLOSE    HKYOTU1   DSPFILE.
*
 END-EXIT.
     EXIT.
******************************************************************
 END PROGRAM  STE3102B.
******************************************************************

```
