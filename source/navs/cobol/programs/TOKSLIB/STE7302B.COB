***********************************************************
*   顧客名　　    ：   ■サカタのタネ殿                   *
*   システム名    ：   販売管理システム                   *
*   サブシステム名：   手書伝票発行　                     *
*   プログラム名　：   ダイユーエイト手書伝票発行　　     *
*   プログラムID  ：   STE7302B                           *
*   作成者        ：   NAV                                *
*   作成日        ：   2005.07.26                         *
*   更新日／更新者：   2011.10.07 / YOSHIDA.M             *
*   更新概要      ：   基幹サーバ統合                     *
***********************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            STE7302B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          04/11/18.
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
            YA     IS   YA
            YB     IS   YB.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*伝票データ
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
*店舗マスタ
     SELECT   HTENMS             ASSIGN    TO    DA-01-VI-TENMS1
                                 ORGANIZATION   IS   INDEXED
                                 ACCESS    MODE IS   RANDOM
                                 RECORD    KEY  IS   TEN-F52
                                                     TEN-F011
                                 FILE STATUS    IS   TEN-ST.
*商品名称マスタ
     SELECT   HMEIMS             ASSIGN    TO    DA-01-VI-MEIMS1
                                 ORGANIZATION   IS   INDEXED
                                 ACCESS    MODE IS   RANDOM
                                 RECORD    KEY  IS   MEI-F011
                                                     MEI-F0121
                                                     MEI-F0122
                                                     MEI-F0123
                                 FILE STATUS    IS   MEI-ST.
*画面ファイル
     SELECT   DSPFILE            ASSIGN    TO        GS-DSPF
                                 FORMAT              DSP-FMT
                                 GROUP               DSP-GRP
                                 PROCESSING          DSP-PRO
                                 FUNCTION            DSP-FNC
                                 STATUS              DSP-ST.
*プリントファイル
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
*****＜店舗マスタ＞*****
 FD  HTENMS
     BLOCK       CONTAINS  8         RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTENMS    OF        XFDLIB
     JOINING     TEN       AS        PREFIX.
*****＜商品名称マスタ＞*****
 FD  HMEIMS
     BLOCK       CONTAINS  6         RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HMEIMS    OF        XFDLIB
     JOINING     MEI       AS        PREFIX.
*****＜画面　ファイル＞*****
 FD  DSPFILE            LABEL RECORD   IS   OMITTED.
*
     COPY    FTE73011   OF   XMDLIB.
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
     03  TEN-ST                   PIC  X(02).
     03  MEI-ST                   PIC  X(02).
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
     03  HTENMS-INV-FLG           PIC  X(03)     VALUE   SPACE.
     03  HMEIMS-INV-FLG           PIC  X(03)     VALUE   SPACE.
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
***  ｺﾞｳｹｲ ｴﾘｱ
 01  GOUKEI.
     03  G-SURYO                  PIC  9(09)     VALUE   ZERO.
     03  G-GENKA                  PIC  9(09)     VALUE   ZERO.
     03  G-BAIKA                  PIC  9(09)     VALUE   ZERO.
***  ｹｲｻﾝ ｴﾘｱ
 01  KEISAN.
     03  W-GENKA                  PIC  9(09)V9   VALUE   ZERO.
     03  W-BAIKA                  PIC  9(09)     VALUE   ZERO.
*    03  W-SURYO                  PIC  9(06)V9   VALUE   ZERO.
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  WK-TORICD                PIC  9(08)     VALUE   12251.
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
     03  WK-BIKO01                PIC  X(15)     VALUE   SPACE.
     03  WK-BIKO02                PIC  X(15)     VALUE   SPACE.
     03  WK-TANT01                PIC  X(04)     VALUE   SPACE.
     03  WK-TANT02                PIC  X(04)     VALUE   SPACE.
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
         NC"　　　　　　　　　　　　　　　".
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
 01  FILE-ERR050                  PIC  N(11)     VALUE
         NC"店舗マスタ　　異常！！".
 01  FILE-ERR060                  PIC  N(11)     VALUE
         NC"商品名称Ｍ　　異常！！".
***  ﾌﾟﾘﾝﾄ ｴﾘｱ
*    ﾐﾀﾞｼ
 01  HEAD-99.
     03  FILLER                   PIC  X(14)     VALUE   SPACE.
     03  HD99                     PIC  X(02).
*    ﾐﾀﾞｼ
 01  HEAD-00                      CHARACTER  TYPE  YA.
     03  FILLER                   PIC  X(08)     VALUE   SPACE.
     03  FILLER                   PIC  N(09)     VALUE
         NC"ダイユーエイト".
*
 01  HEAD-01.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  HD01                     PIC  N(10)
                                  CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  HD02                     PIC  99999.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  HD03                     PIC  X(04).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD04                     PIC  X(02).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD05                     PIC  9999999.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD06                     PIC  99999.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  HD07                     PIC  N(10)
                                  CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD08                     PIC  999999.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD09                     PIC  ZZZ.
*
*    ﾒｲｻｲ
 01  BODY-01.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  MS01                     PIC  X(15).
*
 01  BODY-02.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  MS02                     PIC  X(15).
     03  FILLER                   PIC  X(10)     VALUE   SPACE.
     03  MS03                     PIC  X(13).
     03  FILLER                   PIC  X(13)     VALUE   SPACE.
     03  MS04                     PIC  ZZZ9.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  MS05                     PIC  ZZZZZ9.
     03  MS06                     PIC  ZZ.
     03  MS07                     PIC  ZZZZZZZZ9.
     03  MS08                     PIC  ZZZZZ9.
     03  MS09                     PIC  ZZZZZZZZ9.
*
*    ｺﾞｳｹｲ
 01  TAIL-01.
     03  FILLER                   PIC  X(57)     VALUE   SPACE.
     03  TL01                     PIC  ZZZZ9.
     03  FILLER                   PIC  X(13)     VALUE   SPACE.
     03  TL02                     PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  TL03                     PIC  ZZZZZZZZ9.
 01  TAIL-02.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  BIKO01                   PIC  X(15).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  TANT01                   PIC  X(04).
 01  TAIL-03.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  BIKO02                   PIC  X(15).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  TANT02                   PIC  X(04).
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
 000-TRN-ERR            SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HKYOTU1.
     DISPLAY  FILE-ERR030      UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     DISPLAY  KYO-ST           UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     STOP     RUN.
*--- << ﾏｽﾀ ｴﾗ- >> ---*
 000-MST1-ERR           SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HTENMS.
     DISPLAY  FILE-ERR050      UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     DISPLAY  TEN-ST           UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     STOP     RUN.
 000-MST2-ERR           SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HMEIMS.
     DISPLAY  FILE-ERR060      UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     DISPLAY  MEI-ST           UPON    CONS.
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
     OPEN     INPUT     HKYOTU1  HTENMS  HMEIMS.
     OPEN     I-O       DSPFILE.
*    ワーク初期化
     MOVE     ZERO               TO   DENNO1.
     MOVE     ZERO               TO   CNT-DENPYO.
     MOVE     SPACE              TO   END-FLG.
     MOVE     SPACE              TO   FTE73011.
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
*
     IF       WRK-MAI       =      ZERO
              DISPLAY NC"出力対象無し" UPON CONS
              STOP  RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                       伝票■カウント                1.1      *
****************************************************************
 DEN-CNT-SEC               SECTION.
*
     MOVE     NEW-F02      TO   OLD-F02.
*
     READ     HKYOTU1
          AT END    MOVE   9    TO    DENPYO-END
          GO TO                       DEN-CNT-EXIT
     END-READ.
*    取引先コードブレイクチェック
     IF       KYO-F01      >    WK-TORICD
          MOVE      9           TO    DENPYO-END
          GO TO                       DEN-CNT-EXIT
     END-IF.
*
     MOVE     KYO-F02  TO        NEW-F02.
*
     IF   OLD-F02      NOT =     NEW-F02
          ADD       1  TO        WRK-MAI
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
     MOVE          SPACE     TO   FTE73011.
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
     MOVE    "FTE73011" TO        DSP-FMT.
*    画面表示
     WRITE    FTE73011.
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
*    伝票■のブレイク判定
     IF       KYO-F02   NOT =     DENNO1
              PERFORM   TAIL-WT-SEC
              PERFORM   HEAD-WT-SEC
              MOVE      KYO-F02   TO   DENNO1
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
     MOVE     KYO-F1421          TO   MS01.
     MOVE     KYO-F1422          TO   MS02.
*    取引先商品コード
     MOVE     KYO-F25            TO   MS03.
*    数量
     MOVE     KYO-F15            TO   MS04.
     ADD      KYO-F15            TO   G-SURYO.
*    原価単価
     MOVE     KYO-F172           TO   WK-F172.
     MOVE     WK-F172(1:9)       TO   MS05.
     MOVE     WK-F172(10:2)      TO   MS06.
*    原価金額
     COMPUTE  W-GENKA   =    KYO-F15  *  KYO-F172
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
              MOVE      1         TO   ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO   FL-READ-EXIT
     END-READ.
     IF       KYO-F01 >    WK-TORICD
              GO                  TO   READ-000
              MOVE     "END"      TO   SYORI-FLG
              MOVE      1         TO   ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO   FL-READ-EXIT
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
*    区分
     MOVE     SPACE              TO    HD99.
*    店舗名称取得
     MOVE     WK-TORICD          TO   TEN-F52.
     MOVE     KYO-F07            TO   TEN-F011.
     PERFORM  HTENMS-READ-SEC.
     IF       HTENMS-INV-FLG = "INV"
              MOVE  ALL NC"＊"   TO   HD01
     ELSE
              MOVE  TEN-F03      TO   HD01
     END-IF.
*    分類コード
     MOVE     KYO-F12            TO    HD03.
*    店舗コード
     MOVE     KYO-F07            TO    HD02.
*    伝票区分
     MOVE     KYO-F132           TO    HD04.
*    伝票■
     MOVE     KYO-F02            TO    HD05.
*    取引先コード
*****MOVE     KYO-F01            TO    HD06.
     MOVE     1225               TO    HD06.
     MOVE     NC"サカタのタネ"   TO    HD07.
*    納品日
     MOVE     KYO-F112           TO    WK-F112.
     MOVE     WK-F112(3:6)       TO    HD08.
*
     WRITE    PRT-REC     FROM   HEAD-99    AFTER    2.
     WRITE    PRT-REC     FROM   HEAD-00    AFTER    3.
     WRITE    PRT-REC     FROM   HEAD-01    AFTER    2.
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
     INITIALIZE                  TAIL-01  TAIL-02  TAIL-03.
*    数量
     MOVE   G-SURYO              TO   TL01.
*    原価金額合計
     MOVE   G-GENKA              TO   TL02.
*    売価金額合計
     MOVE   G-BAIKA              TO   TL03.
*    備考
     MOVE   WK-BIKO01            TO   BIKO01.
     MOVE   WK-BIKO02            TO   BIKO02.
*    発注担当者
     MOVE   WK-TANT01            TO   TANT01.
     MOVE   WK-TANT02            TO   TANT02.
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
     MOVE     ZERO             TO     G-GENKA
                                      G-BAIKA
                                      G-SURYO.
     MOVE     SPACE            TO     WK-BIKO01 WK-BIKO02
                                      WK-TANT01 WK-TANT02.
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
                                 BODY-01 BODY-02
                                 TAIL-01 TAIL-02 TAIL-03.
*    テスト印字項目セット
     MOVE   ALL "9"              TO   HD03    HD05    HD06
                                      HD08    HD09
                                      MS04    MS05    MS06
                                      MS07    MS08    MS09
                                      TL01    TL02    TL03.
     MOVE   ALL "*"              TO   HD02    HD04    MS03
                                      BIKO01  BIKO02  MS01  MS02
                                      TANT01  TANT02.
     MOVE   ALL NC"＊"           TO   HD01    HD07.
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
*
*    ファイルのＣＬＯＳＥ
     CLOSE    HKYOTU1   DSPFILE.
*
 END-EXIT.
     EXIT.
****************************************************************
*             店舗マスタ読込み                      ALL        *
****************************************************************
 HTENMS-READ-SEC        SECTION.
*
     READ  HTENMS
           INVALID      MOVE "INV" TO  HTENMS-INV-FLG
           NOT  INVALID MOVE SPACE TO  HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*             商品名称マスタ読込み                  ALL        *
****************************************************************
 HMEIMS-READ-SEC        SECTION.
*
     READ  HMEIMS
           INVALID      MOVE "INV" TO  HMEIMS-INV-FLG
           NOT  INVALID MOVE SPACE TO  HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
******************************************************************
 END PROGRAM  STE7302B.
******************************************************************
