****************************************************************
*         SYSTEM･･･_サカタのタネ　共通システム                *
*         PG-NAME･･･手書伝票発行                               *
*         PG-ID･･･STE3301B                                     *
*                                            DATE. 99.05.24    *
*                                            BY. HAGIWARA      *
*         UPDATE. 11.10.07 / YOSHIDA.M                         *
*                 基幹サーバ統合                               *
*         UPDATE. 13.03.26 / NAV TAKAHASHI                     *
*                 社名変更に伴う作業（社名変更）               *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            STE3301B.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          99/05/24.
*REKYOKS.
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
*----<< 取引先マスタ >>-*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-ST.
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
     COPY    FTE33011   OF   XMDLIB.
*
*----<< 取引先マスタ >>-*
 FD  HTOKMS             BLOCK     CONTAINS   8   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HTOKMS    OF   XFDLIB    JOINING   TOK  AS   PREFIX.
*****＜プリント　ファイル＞*****
 FD  PRINTF                       LINAGE    IS   30   LINES.
 01  PRT-REC                      PIC       X(200).
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
***  ｽﾃｰﾀｽ ｴﾘｱ
 01  STATUS-AREA.
     03  KYO-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
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
     03  CHK-FLG                  PIC  X(03)     VALUE   SPACE.
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
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
     03  WK-BIKO1                 PIC  X(15)     VALUE   SPACE.
     03  WK-BIKO2                 PIC  X(15)     VALUE   SPACE.
*2013/03/25 NAV ST
*****03  WK-TOKKN                 PIC  X(15)     VALUE
*****                                            "ｼﾞｮｲﾌﾙﾔﾏｼﾝ".
     03  WK-TOKKN                 PIC  X(15)     VALUE
                                                 "ﾔﾏｼﾝ".
*2013/03/25 NAV ED
*    ﾒﾂｾ-ｼﾞ ｴﾘｱ
 01  MSG-AREA.
     03  MSG-FIELD.
         05  MSG01                PIC  N(30)     VALUE
         NC"　　　　　　　　　　　　　　　".
         05  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください。".
         05  MSG03                PIC  N(30)     VALUE
         NC"伝票発行中。".
         05  MSG04                PIC  N(30)     VALUE
         NC"無効キーです。".
         05  MSG05                PIC  N(30)     VALUE
         NC"取引先コードを入力してください。".
         05  MSG06                PIC  N(30)     VALUE
         NC"取引先Ｍに存在しません。取引先Ｍを登録して下さい。".
         05  MSG07                PIC  N(30)     VALUE
         NC"指定された取引先の伝票データが存在しません。".
         05  MSG08                PIC  N(30)     VALUE
         NC"伝票枚数カウント中！！".
     03  MSG-FIELD-R     REDEFINES    MSG-FIELD.
         05  MSG-TBL     OCCURS     8      PIC  N(30).
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
         NC"得意先マスタ　異常！！".
***  ﾌﾟﾘﾝﾄ ｴﾘｱ
*    ﾐﾀﾞｼ
 01  HEAD-00.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  HD001                    PIC  X(15).
     03  FILLER                   PIC  X(34)     VALUE   SPACE.
     03  HD002                    PIC  999999.
*
 01  HEAD-01   CHARACTER          TYPE IS        YA.
     03  FILLER                   PIC  X(68)     VALUE   SPACE.
     03  JISYANM                  PIC  N(07).
*
 01  HEAD-02.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  HD01                     PIC  X(15).
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  HD02                     PIC  ZZZ999.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD03                     PIC  X(04).
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  HD04                     PIC  X(02).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD05                     PIC  999999.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD06                     PIC  ZZZZZ9.
*
*    ﾒｲｻｲ
 01  BODY-01.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  MS01                     PIC  X(15).
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  MS031                    PIC  X(08).
     03  FILLER                   PIC  X(99)    VALUE   SPACE.
*
 01  BODY-02.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  MS02                     PIC  X(15).
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  MS032                    PIC  X(05).
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  MS04                     PIC  ZZZZ9.
     03  MS05                     PIC  Z.
     03  FILLER                   PIC  X(10)     VALUE   SPACE.
     03  MS06                     PIC  ZZZZZ9.
     03  MS07                     PIC  ZZ.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  MS08                     PIC  ZZZZZZZ9.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  MS09                     PIC  ZZZZZ9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  MS10                     PIC  ZZZZZZZ9.
*    ｺﾞｳｹｲ
 01  TAIL-01.
     03  FILLER                   PIC  X(52)     VALUE   SPACE.
     03  TL01                     PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(09)     VALUE   SPACE.
     03  TL02                     PIC  ZZZZZZZZ9.
*    ﾋﾞｺｳ
 01  BIKO-01.
     03  FILLER                   PIC  X(30)     VALUE   SPACE.
     03  BIK-01                   PIC  X(15).
 01  BIKO-02.
     03  FILLER                   PIC  X(30)     VALUE   SPACE.
     03  BIK-02                   PIC  X(15).
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
     OPEN     INPUT     HTOKMS.
     OPEN     I-O       DSPFILE.
     OPEN     OUTPUT    PRINTF.
*    ワーク初期化
     MOVE     ZERO         TO    DENNO1.
     MOVE     ZERO         TO    CNT-DENPYO.
     MOVE     SPACE        TO    END-FLG.
     MOVE     SPACE        TO    FTE33011.
     MOVE     SPACE       TO    SYORI-FLG.
     MOVE     ZERO         TO    WRK-MAI.
     MOVE     ZERO         TO    DENPYO-END.
*****売上ファイルスタート*****
***2011.10.07 ST
     MOVE     SPACE        TO    KYO-REC.
     INITIALIZE                  KYO-REC.
***2011.10.07 EN
     MOVE     1041         TO    KYO-F01.
     MOVE     ZERO         TO    KYO-F02  KYO-F04.
     MOVE     ZERO         TO    KYO-F051 KYO-F03.
***2011.10.07 ST
***  START  HKYOTU1  KEY  IS >=  KYO-F01 KYO-F02 KYO-F04
***                              KYO-F051 KYO-F03
     START  HKYOTU1  KEY  IS >=  KYO-F01 KYO-F02 KYO-F04
                                 KYO-F051 KYO-F07 KYO-F112
                                 KYO-F03
***2011.10.07 EN
          INVALID  KEY
              DISPLAY NC"山新対象データ無し" UPON CONS
              STOP RUN
     END-START.
*////MOVE    8         TO        ERR-FLG.
*////PERFORM           ERR-MSG-SEC.
*////MOVE    ZERO      TO        ERR-FLG.
*////PERFORM           DSP-WRITE-SEC.
*****指定取引先手書伝票枚数カウント*****
     PERFORM  DEN-CNT-SEC  UNTIL  DENPYO-END = 9.
*****伝票枚数チェック*****
     IF       WRK-MAI       =      ZERO
              DISPLAY NC"山新対象データ無し" UPON CONS
              STOP RUN
     ELSE
              MOVE    SPACE     TO        ERRMSG
     END-IF.
     CLOSE    HKYOTU1.
     OPEN     INPUT  HKYOTU1.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
 MAIN010.
*****画面表示*****
     PERFORM       DSP-WRITE-SEC.
*****キー入力*****
     MOVE         "MD04R"    TO   DSP-GRP.
     PERFORM       DSP-READ-SEC.
*****アテンション判定*****
     EVALUATE      DSP-FNC
         WHEN     "F004"
                   GO                  TO   MAIN-EXIT
         WHEN     "F005"
                   MOVE     "END"      TO   END-FLG
                   GO                  TO   MAIN-EXIT
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
     END-EVALUATE.
*****処理対象未入力時処理*****
     IF   KUBUN  NOT  NUMERIC
          MOVE     2    TO   ERR-FLG
          PERFORM       ERR-MSG-SEC
          GO       TO   MAIN-EXIT
     ELSE
          MOVE  KUBUN   TO   WRK-R040
     END-IF.
*
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*****処理判定*****
     EVALUATE      KUBUN
*********テストプリント*********
         WHEN      1
                   PERFORM   TEST-PRT-SEC
*********全プリント*********
         WHEN      2
                   MOVE      ZERO      TO   KAIDEN
                   MOVE      ALL "9"   TO   ENDDEN
                   MOVE      3         TO   ERR-FLG
                   PERFORM   ERR-MSG-SEC
                   PERFORM   DSP-WRITE-SEC
                   CLOSE     HKYOTU1
                   OPEN      INPUT     HKYOTU1
***2011.10.07 ST
                   MOVE     SPACE      TO   KYO-REC
                   INITIALIZE               KYO-REC
***2011.10.07 EN
                   MOVE      1041      TO   KYO-F01
                   MOVE      ZERO      TO   KYO-F02
                   MOVE      ZERO      TO   KYO-F04
                                            KYO-F051
                                            KYO-F03
*
                   START    HKYOTU1       KEY  IS >=
***2011.10.07 ST
***                    KYO-F01 KYO-F02 KYO-F04 KYO-F051 KYO-F03
                       KYO-F01  KYO-F02 KYO-F04 KYO-F051 KYO-F07
                       KYO-F112 KYO-F03
***2011.10.07 EN
                   END-START
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     SYORI-FLG =   "END"
*********範囲指定*********
         WHEN      3
                   PERFORM   KEY-IN-SEC
                   IF        END-FLG   =   "END"
                             GO        TO   MAIN-EXIT
                   END-IF
                   IF        CHK-FLG   =   "CHK"
                             GO        TO   MAIN-EXIT
                   END-IF
                   MOVE      3         TO   ERR-FLG
                   PERFORM   ERR-MSG-SEC
                   PERFORM   DSP-WRITE-SEC
                   CLOSE     HKYOTU1
                   OPEN      INPUT     HKYOTU1
***2011.10.07 ST
                   MOVE     SPACE      TO   KYO-REC
                   INITIALIZE               KYO-REC
***2011.10.07 EN
                   MOVE      1041      TO   KYO-F01
                   MOVE      ZERO      TO   KYO-F02
                   MOVE      ZERO      TO   KYO-F04
                                            KYO-F051
                                            KYO-F03
*
                   START    HKYOTU1       KEY  IS >=
***2011.10.07 ST
***                   KYO-F01 KYO-F02 KYO-F04 KYO-F051 KYO-F03
                      KYO-F01  KYO-F02 KYO-F04 KYO-F051 KYO-F07
                      KYO-F112 KYO-F03
***2011.10.07 EN
                   END-START
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     SYORI-FLG =   "END"
         WHEN      OTHER
                   MOVE      2         TO   ERR-FLG
     END-EVALUATE.
*****伝票Ｆ、プリントＦ　ＣＬＯＳＥ
     CLOSE         PRINTF    HKYOTU1.
*****次対象の入力為，ＲＥＡＤスイッチＯＦＦ
     MOVE          ZERO      TO       RD-SW.
*****伝票Ｆ、プリントＦ　ＯＰＥＮ
     OPEN     INPUT     HKYOTU1.
     OPEN     OUTPUT    PRINTF.
*****伝票_ブレイクキーの初期化
     MOVE          ZERO      TO       NEW-F02.
     MOVE          ZERO      TO       OLD-F02.
     MOVE          SPACE     TO       SYORI-FLG.
     MOVE          ZERO      TO       DENPYO-END.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*                       伝票_カウント                1.1
****************************************************************
 DEN-CNT-SEC               SECTION.
*
     MOVE     NEW-F02      TO   OLD-F02.
*
     READ     HKYOTU1      AT END
              MOVE     9    TO    DENPYO-END
              GO TO    DEN-CNT-EXIT
     END-READ.
*    取引先コードブレイクチェック
     IF       KYO-F01      >    1041
              MOVE     9    TO    DENPYO-END
              GO TO    DEN-CNT-EXIT
     END-IF.
*
     MOVE     KYO-F02  TO        NEW-F02.
*
     IF       OLD-F02      NOT =     NEW-F02
              ADD      1    TO       WRK-MAI
     END-IF.
*
 DEN-CNT-EXIT.
     EXIT.
****************************************************************
*             エラーメッセージセット                2.1        *
****************************************************************
 ERR-MSG-SEC            SECTION.
*
     IF       ERR-FLG   =   ZERO
              MOVE   SPACE                 TO   ERRMSG
     ELSE
              MOVE   MSG-TBL ( ERR-FLG )   TO   ERRMSG
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
     MOVE     WRK-MAI   TO        DENMAI.
     MOVE     SPACE     TO        DSP-CNTL.
     MOVE    "SCREEN"   TO        DSP-GRP.
     MOVE    "FTE33011" TO        DSP-FMT.
*    画面表示
     WRITE    FTE33011.
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
     MOVE          ZERO      TO   KAIDEN    ENDDEN.
     MOVE          SPACE     TO   CHK-FLG.
*    画面読込み
     PERFORM       DSP-READ-SEC.
*    アテンション判定
     EVALUATE      DSP-FNC
*        取消
         WHEN     "F004"
                   MOVE     "CHK"      TO   CHK-FLG
                   GO                  TO   KEY-IN-EXIT
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
     IF            KAIDEN   >  ENDDEN
                   MOVE      2         TO   ERR-FLG
                   GO                  TO   KEY-IN-01
     END-IF.
*    未入力時（全データ対象）
     IF           (KAIDEN  =  ZERO)     AND
                  (ENDDEN  =  ZERO)
                   MOVE      ZERO      TO   KAIDEN
                   MOVE      ALL "9"   TO   ENDDEN
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
*    取引先商品コード(上段)
     MOVE     KYO-F25(1:8)       TO   MS031.
*    商品名（２）
     MOVE     KYO-F1422          TO   MS02.
*    取引先商品コード(下段)
     MOVE     KYO-F25(9:5)       TO   MS032.
*    数量
     MOVE     KYO-F15            TO   WK-F172.
     MOVE     WK-F172(1:9)       TO   MS04.
     MOVE     WK-F172(10:1)      TO   MS05.
*    原価単価
     MOVE     KYO-F172           TO   WK-F172.
     MOVE     WK-F172(1:9)       TO   MS06.
     MOVE     WK-F172(10:2)      TO   MS07.
*    原価金額
     COMPUTE  W-GENKA   =    KYO-F15  *  KYO-F172
     MOVE     W-GENKA            TO   MS08.
     COMPUTE  G-GENKA   =    G-GENKA  +  W-GENKA.
*    売価単価
     MOVE     KYO-F173           TO   MS09.
*    売価金額
     COMPUTE  W-BAIKA   =    KYO-F15  *  KYO-F173
     MOVE     W-BAIKA            TO   MS10.
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
     IF       KYO-F01   >   1041
              MOVE     "END"      TO   SYORI-FLG
              MOVE      1         TO   ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO   FL-READ-EXIT
     END-IF.
     IF       KYO-F03 >    20
**************備考欄の退避
              IF  KYO-F03  =  80
                  MOVE     KYO-F1421  TO  WK-BIKO1
                  MOVE     KYO-F1422  TO  WK-BIKO2
              END-IF
              GO                  TO   READ-000
     END-IF.
*    全プリント時判定
**** IF       KUBUN   =    2
****          GO                  TO   READ-010
**** END-IF.
*    伝票番号範囲指定大小チェック
     IF       KYO-F02 >    ENDDEN
              GO                  TO   READ-000
     END-IF.
     IF       KYO-F02 <    KAIDEN
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
     INITIALIZE                  HEAD-00.
     INITIALIZE                  HEAD-01.
     INITIALIZE                  HEAD-02.
*--- 項目セット
*    取引先名称
     MOVE     WK-TOKKN           TO    HD001.
*    伝票_
     MOVE     KYO-F02            TO    HD002.
*    店舗名称
     MOVE     KYO-F30            TO    HD01.
*    店舗コード
     MOVE     KYO-F07            TO    HD02.
*    分類コード
     MOVE     KYO-F12            TO    HD03.
*    伝票区分
     MOVE     KYO-F132           TO    HD04.
*    納品日
     MOVE     KYO-F112           TO    WK-F112.
     MOVE     WK-F112(3:6)       TO    HD05.
*    取引先コード
     MOVE     KYO-F01            TO    HD06.
*    取引先コード
     MOVE     NC"_サカタのタネ" TO    JISYANM.
*
     WRITE    PRT-REC          FROM   HEAD-00    AFTER    6.
     WRITE    PRT-REC          FROM   HEAD-01    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-02    AFTER    1.
*    印字位置送り
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  2.
*    行カウンタ
     MOVE     10               TO     L-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             テール部出力処理                      2.5.3      *
****************************************************************
 TAIL-WT-SEC            SECTION.
*    テイル部初期化
     INITIALIZE                  TAIL-01.
*    原価金額合計
     MOVE   G-GENKA              TO   TL01.
*    売価金額合計
     MOVE   G-BAIKA              TO   TL02.
*    備考欄の転送
     MOVE   WK-BIKO1             TO   BIK-01.
     MOVE   WK-BIKO2             TO   BIK-02.
*    テイル印字制御
     COMPUTE   CNT-AFTER   =     24  -  L-CNT.
*    テイル行の印字
     WRITE    PRT-REC          FROM   TAIL-01   AFTER   CNT-AFTER.
     WRITE    PRT-REC          FROM   BIKO-01   AFTER   2.
     WRITE    PRT-REC          FROM   BIKO-02   AFTER   1.
*    次頁へ改頁
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  PAGE.
*    ワーク初期化
     MOVE     ZERO             TO     G-GENKA
                                      G-BAIKA.
     MOVE     SPACE            TO     WK-BIKO1.
     MOVE     SPACE            TO     WK-BIKO2.
*    伝票枚数
     ADD      1                TO     CNT-DENPYO.
*
 TAIL-WT-EXIT.
     EXIT.
****************************************************************
*             テストプリント                        2.6        *
****************************************************************
 TEST-PRT-SEC           SECTION.
*    テスト印字項目セット
     MOVE   ALL "9"              TO   HD002   HD02    HD04
                                      HD05    HD06
                                      MS04    MS05    MS06
                                      MS07    MS08    MS09
                                      MS10
                                      TL01    TL02
     MOVE   ALL "*"              TO   HD001   HD01    HD03
                                      MS01    MS02    MS031
                                      MS032
                                      BIK-01  BIK-02.
     MOVE   ALL NC"＊"           TO   JISYANM.
*    ヘッド印字
     WRITE    PRT-REC          FROM   HEAD-00    AFTER    6.
     WRITE    PRT-REC          FROM   HEAD-01    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-02    AFTER    1.
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  2.
*    明細印字
     PERFORM   VARYING   L-CNT      FROM  1  BY  1
                                        UNTIL   L-CNT     >  6
              WRITE  PRT-REC   FROM   BODY-01   AFTER     1
              WRITE  PRT-REC   FROM   BODY-02   AFTER     1
     END-PERFORM.
*    テイル印字
     WRITE    PRT-REC          FROM   TAIL-01   AFTER     2.
     WRITE    PRT-REC          FROM   BIKO-01   AFTER     2.
     WRITE    PRT-REC          FROM   BIKO-02   AFTER     1.
*
**** MOVE     SPACE            TO     PRT-REC.
**** WRITE    PRT-REC          AFTER  PAGE.
*
 TEST-PRT-EXIT.
     EXIT.
****************************************************************
*             終了処理                              3.0        *
****************************************************************
 END-SEC                SECTION.
*
*    IF       WRK-R040   NOT  =    3
*             DISPLAY   DENPYO    UPON      CONS
*             ACCEPT    AAA       FROM      CONS
*    END-IF.
*    ファイルのＣＬＯＳＥ
     CLOSE    HKYOTU1   DSPFILE   HTOKMS.
*
 END-EXIT.
     EXIT.
******************************************************************
 END PROGRAM  STE3301B.
******************************************************************
