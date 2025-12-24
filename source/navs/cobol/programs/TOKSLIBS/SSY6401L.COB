****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　サンライズ伝票発行　　　　　　　　*
*    作成日／更新日　　　：　05/02/15                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    伝票フォーマット　　：　ＴＡ_型
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY6401L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          05/02/15.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YA             IS        YA
         CONSOLE        IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*伝票データ（共通）　　
     SELECT   SZSHIRED  ASSIGN    TO        SZSHIRED
                        ACCESS    MODE IS   SEQUENTIAL
                        STATUS              DEN-ST.
*画面定義ファイル　　
     SELECT   DSPFILE   ASSIGN    TO        GS-DSPF
                        FORMAT              DSP-FMT
                        GROUP               DSP-GRP
                        PROCESSING          DSP-PRO
                        FUNCTION            DSP-FNC
                        STATUS              DSP-ST.
*プリント定義ファイル　　
     SELECT   PRTFILE   ASSIGN    TO        LP-04-PRTF
                        STATUS              PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
****************************************************************
*  FILE =伝票データ（共通）                                    *
****************************************************************
 FD  SZSHIRED
                        BLOCK    CONTAINS  1    RECORDS
                        LABEL    RECORD    IS   STANDARD.
                        COPY     SZSHIRED  OF   XFDLIB
                        JOINING  DEN       AS   PREFIX.
****************************************************************
*    FILE = ｶﾞﾒﾝ  F                                            *
****************************************************************
 FD  DSPFILE
                        LABEL RECORD   IS   OMITTED.
*
     COPY    FSY11011   OF   XMDLIB.
*
****************************************************************
*    FILE = ﾌﾟﾘﾝﾄ ﾌｱｲﾙ                                         *
****************************************************************
 FD  PRTFILE
                        LABEL RECORD   IS   OMITTED
                        LINAGE IS      30   LINES
                        DATA  RECORD   IS   PRT-REC.
 01  PRT-REC.
     03  FILLER                   PIC  X(200).
****************************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
***  ｽﾃｰﾀｽｴﾘｱ
 01  STATUS-AREA.
     03  DEN-ST                   PIC  X(02).
     03  PRT-ST                   PIC  X(02).
***  ｶﾞﾒﾝ ｺﾝﾄﾛｰﾙ ｴﾘｱ
 01  DSP-CNTL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
     03  DSP-ST                   PIC  X(02).
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)     VALUE  ZERO.
     03  PROGRAM-END              PIC  X(03)     VALUE  ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE  ZERO.
     03  KAI-FLG                  PIC  9(01)     VALUE  ZERO.
     03  SYU-FLG                  PIC  9(01)     VALUE  ZERO.
     03  ZEN-FLG                  PIC  9(01)     VALUE  ZERO.
     03  TS-FLG                   PIC  9(01)     VALUE  ZERO.
***  ｶｳﾝﾄ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(07)     VALUE  ZERO.
     03  CNT-AFTER                PIC  9(07)     VALUE  ZERO.
***  ｺﾞｳｹｲ
 01  GOUKEI.
     03  GENKINKEI                PIC  9(09)     VALUE  ZERO.
     03  BAIKINKEI                PIC  9(09)     VALUE  ZERO.
     03  SURYOKEI                 PIC  9(06)V9   VALUE  ZERO.
     03  TSGENKEI                 PIC  9(09)     VALUE  ZERO.
     03  TSBAIKEI                 PIC  9(09)     VALUE  ZERO.
     03  TSSURYOKEI               PIC  9(06)V9   VALUE  ZERO.
***  ﾜｰｸ ｴﾘｱ
 01  WRK-AREA.
     03  WK-MAI                   PIC  9(06)     VALUE  ZERO.
     03  RD-SW                    PIC  9(01)     VALUE  ZERO.
     03  SYS-DATE                 PIC  9(6)      VALUE  ZERO.
     03  SYS-TIME                 PIC  9(8)      VALUE  ZERO.
     03  IN-DATA                  PIC  X(01).
     03  DEN-NO                   PIC  9(09)     VALUE  ZERO.
     03  CNT-DENPYO               PIC  9(07)     VALUE  ZERO.
     03  ZERO-FLG                 PIC  9(01)     VALUE  ZERO.
     03  I                        PIC  9(02)     VALUE  ZERO.
     03  J                        PIC  9(02)     VALUE  ZERO.
     03  WK-DEN-NO                PIC  9(09)     VALUE  ZERO.
     03  WK-SINSEINO              PIC  9(08)     VALUE  ZERO.
     03  WK-GYONO                 PIC  9(01)     VALUE  ZERO.
     03  WK-TEKIYO                PIC  X(20)     VALUE  SPACE.
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
***  ﾃﾞﾝﾋﾟﾖｳ ｷ-
 01  WRK-DENNO.
     03  DENNO1                   PIC  9(09).
***  ﾒｯｾｰｼﾞｴﾘｱ
 01  MSG-AREA.
     03  MSG-FIELD.
         05  MSG01                PIC  N(30)     VALUE
         NC"　　　　　　　　　　　　　　　".
         05  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください。".
         05  MSG03                PIC  N(30)     VALUE
         NC"仕入伝票発行中".
         05  MSG04                PIC  N(30)     VALUE
         NC"無効キーです".
     03  MSG-FIELD-R     REDEFINES    MSG-FIELD.
         05  MSG-TBL     OCCURS     4      PIC  N(30).
*
 01  FILE-ERR010                  PIC  N(15)     VALUE
         NC"プリンター　異常！！".
 01  FILE-ERR020                  PIC  N(15)     VALUE
         NC"仕入ファイル　異常！！".
 01  FILE-ERR030                  PIC  N(15)     VALUE
         NC"画面ファイル　異常！！".
***  ﾍﾝｼｭｳ ｴﾘｱ
 01  WK-SURYO                     PIC  9(06)V9.
 01  WK-SURYO-R  REDEFINES  WK-SURYO.
     03  WK-SURYO1                PIC  9(06).
     03  WK-SURYO2                PIC  9(01).
 01  WK-GENKA                     PIC  9(06)V99.
 01  WK-GENKA-R  REDEFINES  WK-GENKA.
     03  WK-GENKA1                PIC  9(06).
     03  WK-GENKA2                PIC  9(02).
***  画面用日付取得
 01  WK-DATE8.
     03  WK-DATE8-YY1             PIC  9(02).
     03  WK-DATE8-YY2             PIC  9(06).
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                   PIC  X(79)     VALUE   SPACE.
     03  HD1-01                   PIC  N(01)
                                  CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC  X(11)     VALUE   SPACE.
     03  HD1-02                   PIC  X(10).
*
 01  HD2.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  HD2-01                   PIC  X(20).
*
 01  HD3.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  HD3-01                   PIC  X(20).
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  HD3-02                   PIC  9(06).
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  HD3-03                   PIC  9(02).
     03  HD3-04                   PIC  X(02).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD3-05                   PIC  X(02).
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  HD3-06                   PIC  9(07).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD3-07                   PIC  9(06).
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  HD3-08                   PIC  X(20).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD3-09                   PIC  9(06).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD3-10                   PIC  9(06).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD3-11                   PIC  ZZZ.
*
*--------------------------------------------------------------*
*    明細                                                      *
*--------------------------------------------------------------*
*
 01  DT1.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  DT1-011                  PIC  X(15).
     03  DT1-012                  PIC  X(05).
     03  DT1-013                  PIC  X(05).
     03  FILLER                   PIC  X(13)     VALUE   SPACE.
     03  DT1-02                   PIC  X(07).
     03  DT1-03                   PIC  X(05).
     03  FILLER                   PIC  X(30)     VALUE   SPACE.
     03  DT1-04                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  DT1-05                   PIC  ZZZZZZZZ9.
*
 01  DT2.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  DT2-01                   PIC  X(25).
     03  DT2-02                   PIC  X(13).
     03  DT2-03                   PIC  ZZZZZZZ.
     03  DT2-04                   PIC  ZZZZZ.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  DT2-05                   PIC  X(02).
     03  DT2-061                  PIC  ZZZZ9.
     03  DT2-062                  PIC  X.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  DT2-071                  PIC  ZZZZZ9.
     03  DT2-072                  PIC  X.
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  DT2-081                  PIC  ZZZZZ9.
     03  DT2-082                  PIC  XX.
     03  DT2-09                   PIC  ZZZZZZZZ9.
     03  DT2-10                   PIC  ZZZZZ9.
     03  DT2-11                   PIC  ZZZZZZZZ9.
*伝票訂正用（ＤＴ）
 01  DT21.
     03  FILLER                   PIC  X(58)     VALUE SPACE.
     03  FILLER                   PIC  X(06)      VALUE
                                                 "======".
     03  FILLER                   PIC  X(21)     VALUE SPACE.
     03  FILLER                   PIC  X(09)     VALUE
                                                 "=========".
     03  FILLER                   PIC  X(06)     VALUE SPACE.
     03  FILLER                   PIC  X(09)     VALUE
                                                 "=========".
*--------------------------------------------------------------*
*    テール行                                                  *
*--------------------------------------------------------------*
 01  TL1.
     03  FILLER                   PIC  X(58)     VALUE   SPACE.
     03  TL1-011                  PIC  ZZZZ9.
     03  TL1-012                  PIC  X.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  TL1-021                  PIC  ZZZZZ9.
     03  TL1-022                  PIC  X.
     03  FILLER                   PIC  X(12)     VALUE   SPACE.
     03  TL1-03                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  TL1-04                   PIC  ZZZZZZZZ9.
*伝票訂正用（ＴＬ）
 01  TL11.
     03  FILLER                   PIC  X(58)     VALUE SPACE.
     03  FILLER                   PIC  X(06)     VALUE
                                                 "======".
     03  FILLER                   PIC  X(21)     VALUE SPACE.
     03  FILLER                   PIC  X(09)     VALUE
                                                 "=========".
     03  FILLER                   PIC  X(06)     VALUE SPACE.
     03  FILLER                   PIC  X(09)     VALUE
                                                 "=========".
*
*伝票訂正用（合計欄）
 01  TL2.
     03  FILLER                   PIC  X(71)     VALUE SPACE.
     03  TL2-01                   PIC  X(18).
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  TL2-02                   PIC  X(18).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
 DECLARATIVES.
*--- << プリンタエラー >> ---*
 000-PRTFILE-ERR        SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      PRTFILE.
     DISPLAY  FILE-ERR010         UPON    CONS.
     DISPLAY  SEC-NAME            UPON    CONS.
     DISPLAY  PRT-ST              UPON    CONS.
     ACCEPT   IN-DATA             FROM    CONS.
     MOVE     "4000"              TO      PROGRAM-STATUS.
     STOP     RUN.
*--- << 仕入データエラー >> ---*
 000-MAST-ERR           SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      SZSHIRED.
     DISPLAY  FILE-ERR020         UPON    CONS.
     DISPLAY  SEC-NAME            UPON    CONS.
     DISPLAY  DEN-ST              UPON    CONS.
     ACCEPT   IN-DATA             FROM    CONS.
     MOVE     "4000"              TO      PROGRAM-STATUS.
     STOP     RUN.
*--- << 画面エラー >> ---*
 000-DSP-ERR            SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DSPFILE.
     DISPLAY  FILE-ERR030         UPON    CONS.
     DISPLAY  SEC-NAME            UPON    CONS.
     DISPLAY  DSP-ST              UPON    CONS.
     ACCEPT   IN-DATA             FROM    CONS.
     MOVE     "4000"              TO      PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*           　M A I N             M O D U L E         0.0      *
****************************************************************
 PROCESS-START          SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     PROGRAM-END    =   "END".
     PERFORM  END-SEC.
     STOP     RUN.
****************************************************************
*             初期処理                                1.0      *
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN     INPUT     SZSHIRED.
     OPEN     I-O       DSPFILE.
     OPEN     OUTPUT    PRTFILE.
*    初期値セット
     MOVE     ZERO                TO   DENNO1.
     MOVE     ZERO                TO   CNT-DENPYO.
     MOVE     SPACE               TO   END-FLG.
     MOVE     SPACE               TO   FSY11011.
     MOVE      1                  TO   WK-GYONO.
*    伝票総枚数読込み
     PERFORM  DEN-CNT-SEC  UNTIL  END-FLG = "END".
*    伝票枚数チェック
     IF       WK-MAI  <=  ZERO
              DISPLAY NC"対象データ無し" UPON CONS
              MOVE    "END"      TO   PROGRAM-END
              GO                 TO   INIT-EXIT
     END-IF.
*    終了フラグクリア
     MOVE     SPACE              TO   END-FLG.
     CLOSE    SZSHIRED.
*
     OPEN     INPUT      SZSHIRED.
 INIT-EXIT.
     EXIT.
****************************************************************
*             伝票データファイル読込み                2.0      *
****************************************************************
 DEN-CNT-SEC            SECTION.
*
     READ     SZSHIRED AT  END
              MOVE     "END"     TO   END-FLG
              GO                 TO   DEN-CNT-EXIT
     END-READ.
*    サンライズ以外は読み飛ばし（取引先）
     IF       DEN-F01  NOT =  820
                       GO       TO   DEN-CNT-SEC
     END-IF.
 DEN-CNT-01.
*    伝票枚数カウント
     IF       DEN-F02  NOT =  WK-DEN-NO
              ADD    1        TO      WK-MAI
              MOVE   DEN-F02  TO      WK-DEN-NO
     END-IF.
*
 DEN-CNT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
     MOVE          "MAIN-SEC"          TO   S-NAME.
*
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*
     MOVE          "MD04R"             TO   DSP-GRP.
     PERFORM       DSP-READ-SEC.
*
     EVALUATE      DSP-FNC
         WHEN      "F005"
                   MOVE     "END"      TO   PROGRAM-END
                   GO                  TO   MAIN-EXIT
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
                   GO                  TO   MAIN-SEC
     END-EVALUATE.
*
     IF            MD04      NOT  NUMERIC
                   MOVE      2         TO   ERR-FLG
                   GO                  TO   MAIN-SEC
     END-IF.
*
     EVALUATE      MD04
         WHEN      1
                   PERFORM   TEST-PRT-SEC
         WHEN      2
                   MOVE      SPACE     TO   END-FLG
                   MOVE      ZERO      TO   MD02
                   MOVE      ALL "9"   TO   MD03
***                伝票データ発行中メッセージ表示
                   MOVE     3          TO   ERR-FLG
                   PERFORM   ERR-MSG-SEC
                   PERFORM   DSP-WRITE-SEC
***
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     END-FLG   =   "END"
                   CLOSE     SZSHIRED  PRTFILE
                   OPEN      INPUT     SZSHIRED
                   OPEN      OUTPUT    PRTFILE
                   MOVE      1         TO   ERR-FLG
         WHEN      3
                   MOVE      SPACE     TO   END-FLG
                   PERFORM   KEY-IN-SEC
***                伝票データ発行中メッセージ表示
                   IF       END-FLG  NOT = "END"
                            MOVE     3          TO   ERR-FLG
                            PERFORM   ERR-MSG-SEC
                            PERFORM   DSP-WRITE-SEC
                   END-IF
***
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     END-FLG   =   "END"
                   CLOSE     SZSHIRED  PRTFILE
                   OPEN      INPUT     SZSHIRED
                   OPEN      OUTPUT    PRTFILE
                   MOVE      1         TO   ERR-FLG
         WHEN      OTHER
                   MOVE      2         TO   ERR-FLG
                   GO                  TO   MAIN-SEC
     END-EVALUATE.
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
     MOVE          ZERO           TO   DEN-NO
                                       KAI-FLG
                                       SYU-FLG
                                       ZEN-FLG
                                       RD-SW.
     MOVE          SPACE          TO   END-FLG.
     MOVE          1              TO   WK-GYONO.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             範囲指定処理                            2.1      *
****************************************************************
 KEY-IN-SEC             SECTION.
     MOVE          "KEI-IN"       TO   S-NAME.
*
 KEY-IN-01.
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*
     MOVE         "NE"            TO   DSP-PRO.
     MOVE         "KEY01"         TO   DSP-GRP.
     MOVE          ZERO           TO   MD02      MD03.
*
     PERFORM       DSP-READ-SEC.
*
     EVALUATE      DSP-FNC
         WHEN     "F005"
                   MOVE     "END"      TO   END-FLG
                   GO                  TO   KEY-IN-EXIT
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
                   GO                  TO   KEY-IN-01
     END-EVALUATE.
*
     IF           (MD02   =  ZERO)     AND
                  (MD03   =  ZERO)
                   MOVE      ZERO      TO   MD02
                   MOVE      ALL "9"   TO   MD03
                   MOVE      1         TO   ZEN-FLG
     END-IF.
*
 KEY-IN-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                    2.2      *
****************************************************************
 FL-READ-SEC            SECTION.
     MOVE     "FL-READ"           TO        S-NAME.
*
 READ-000.
     READ     SZSHIRED    AT      END
              MOVE       "END"    TO        END-FLG
              MOVE        1       TO        ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO        FL-READ-EXIT
     END-READ.
     DISPLAY "DEN-F03 XXXXX = " DEN-F03  UPON CONS.
*MD04(印字パターン)=全件数印刷時
     IF       MD04    =    2  OR  ZEN-FLG   = 1
              GO                  TO        READ-020
     END-IF.
*
     IF       KAI-FLG   =    1    GO  TO    READ-010.
     IF       DEN-F02   =    MD02
              MOVE      1         TO        KAI-FLG
         ELSE
              GO        TO        FL-READ-SEC.
 READ-010.
     IF       ( SYU-FLG     = 1      )  AND
              ( DEN-F02 NOT = DEN-NO )
              MOVE      "END"     TO        END-FLG
              GO        TO        FL-READ-EXIT.
     IF       DEN-F02   =    MD03
              MOVE      DEN-F02   TO        DEN-NO
              MOVE      1         TO        SYU-FLG.
 READ-020.
*
     IF       RD-SW     =    0
              MOVE      DEN-F02   TO   DENNO1
              PERFORM   HEAD-WT-SEC
              MOVE      1         TO   RD-SW
     END-IF.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             伝票出力処理                            2.3      *
****************************************************************
 DENP-WT-SEC            SECTION.
     MOVE     "DENP-WT"           TO        S-NAME.
*伝票　ブレイクチェック
     IF       DEN-F02   NOT =     DENNO1
              PERFORM   TAIL-WT-SEC
              PERFORM   HEAD-WT-SEC
              MOVE      DEN-F02   TO        DENNO1
              MOVE      1         TO        WK-GYONO
     END-IF.
*
*明細部セット
     MOVE     SPACE     TO       DT1  DT2.
*
*伝票行_により明細印字位置の判定を行う
     DISPLAY "DEN-F03   = "  DEN-F03     UPON CONS.
     DISPLAY "WK-GYONO  = "  WK-GYONO    UPON CONS.
     IF       DEN-F03   NOT =   WK-GYONO
         MOVE     SPACE               TO     PRT-REC
         WRITE    PRT-REC             AFTER  2
         ADD      2                   TO     L-CNT
         ADD      1                   TO     WK-GYONO
         GO   TO        DENP-WT-SEC
     END-IF.

*明細１行目
***  メーカーブランド名
     MOVE     DEN-A22             TO        DT1-011.
***  規格数値
     MOVE     DEN-A24             TO        DT1-012.
***  規格単位
     MOVE     DEN-A25             TO        DT1-013.
***  色
     MOVE     DEN-A26             TO        DT1-02.
***  サイズ
     MOVE     DEN-A27             TO        DT1-03.
***  訂正伝票時処理
     IF       DEN-F15   NOT =     DEN-F50
***           訂正後原価金額
              COMPUTE  DT1-04 =   DEN-F15 * DEN-F172
***           訂正後売価金額
              COMPUTE  DT1-05 =   DEN-F15 * DEN-F173
     END-IF.
*
*明細２行目
***  商品名（規格）
     MOVE     DEN-A23             TO        DT2-01.
***  商品コード（ＪＡＮＣＤ）
     MOVE     DEN-A21             TO        DT2-02.
***  入数
     MOVE     DEN-A28             TO        DT2-03.
***  発注単位数（ケース）
     MOVE     DEN-A29             TO        DT2-04.
***  単位
*
***  数量
     MOVE     DEN-F50             TO        WK-SURYO.
***  数量（整数部）
     MOVE     WK-SURYO1           TO        DT2-061.
***  数量（端数部）
     IF       WK-SURYO2   =      ZERO
              MOVE     SPACE      TO        DT2-062
     ELSE
              MOVE     WK-SURYO2  TO        DT2-062
     END-IF.
***  数量計
     COMPUTE  SURYOKEI  =   SURYOKEI  +  DEN-F50.

***  訂正後数量
     IF       DEN-F15   NOT =     DEN-F50
         MOVE      DEN-F15   TO       WK-SURYO
***      訂正後数量（整数部）
         MOVE     WK-SURYO1            TO        DT2-071
***      訂正後数量（端数部）
         IF       WK-SURYO2  =         ZERO
                  MOVE     SPACE       TO        DT2-072
         ELSE
                  MOVE     WK-SURYO2   TO        DT2-072
         END-IF
***      訂正数量計
         COMPUTE  TSSURYOKEI  =  TSSURYOKEI  +  DEN-F15
     END-IF.
***  原単価
     MOVE     DEN-F512            TO        WK-GENKA.
***  原単価（整数部）
     MOVE     WK-GENKA1           TO        DT2-081.
***  原単価（端数部）
     IF       WK-GENKA2   =       ZERO
              MOVE     SPACE      TO        DT2-082
     ELSE
              MOVE WK-GENKA2      TO        DT2-082
     END-IF.
***  原価金額
     MOVE     DEN-F521            TO        DT2-09.
***  売価単価
     MOVE     DEN-F513            TO        DT2-10.
***  売価金額
     MOVE     DEN-F522            TO        DT2-11.
*
*原価・売価合計計算
     COMPUTE  GENKINKEI = GENKINKEI + DEN-F521.
     COMPUTE  BAIKINKEI = BAIKINKEI + DEN-F522.
*訂正原価・売価合計計算
***  訂正有時
     IF       DEN-F15   NOT =     DEN-F50
              COMPUTE   TSGENKEI  = DEN-F15 * DEN-F172 + TSGENKEI
              COMPUTE   TSBAIKEI  = DEN-F15 * DEN-F173 + TSBAIKEI
     ELSE
***  訂正無時
              COMPUTE   TSGENKEI  = TSGENKEI + DEN-F521
              COMPUTE   TSBAIKEI  = TSBAIKEI + DEN-F522
     END-IF.
*
*明細１行目出力　　　
     WRITE    PRT-REC   FROM      DT1       AFTER     1.
*明細２行目出力
     WRITE    PRT-REC   FROM      DT2       AFTER     1.
*訂正伝票時処理（明細２行目二重線）
     IF       DEN-F15   NOT = DEN-F50
              WRITE     PRT-REC   FROM   DT21  AFTER     0
     END-IF.
*
     ADD      2                   TO        L-CNT.
*
 DENP-010.
*
     PERFORM  FL-READ-SEC.
     IF       END-FLG   =         "END"
              PERFORM   TAIL-WT-SEC
     END-IF.
*明細印字位置判定用
     ADD      1                   TO        WK-GYONO.
*
 DENP-WT-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部出力処理                        2.3.1    *
****************************************************************
 HEAD-WT-SEC            SECTION.
     MOVE     "HEAD-WT"           TO        S-NAME.
     MOVE     SPACE               TO        HD1 HD2 HD3.
*
*ヘッダ１行目
*訂正フラグチェック(TL-WRITE-SECで使用）
     IF       DEN-F53 = 1
              MOVE      1         TO        TS-FLG
              MOVE   NC"○"       TO        HD1-01
     END-IF.
***  Ｅ欄
     MOVE     DEN-A13             TO        HD1-02.
*
*ヘッダ２行目
***  社名
     MOVE     DEN-A10             TO        HD2-01.
*
*ヘッダ３行目
***  店名
     MOVE     DEN-A11             TO        HD3-01.
***  店舗ＣＤ
     MOVE     DEN-A04             TO        HD3-02.
***  分類ＣＤ
     MOVE     DEN-A05             TO        HD3-03.
     MOVE     DEN-A14             TO        HD3-04.
***  伝票区分
     MOVE     DEN-A06             TO        HD3-05.
***  伝票_
     MOVE     DEN-F02             TO        HD3-06.
***  取引先コード
     MOVE     DEN-F01             TO        HD3-07.
***  取引先名
     MOVE     DEN-A12             TO        HD3-08.
***  発注日
     MOVE     DEN-A08             TO        HD3-09.
***  納品日
     IF       DEN-A09 NOT = ZERO
              MOVE     DEN-A09    TO        HD3-10
     END-IF.
***  便（引取のとき）
     MOVE     DEN-A15             TO        HD3-11.
*
*ヘッダ部出力
     WRITE    PRT-REC          FROM   HD1    AFTER    4.
     WRITE    PRT-REC          FROM   HD2    AFTER    2.
     WRITE    PRT-REC          FROM   HD3    AFTER    2.
*
     MOVE     SPACE               TO     PRT-REC.
     WRITE    PRT-REC             AFTER  3.
*
     MOVE     11                  TO     L-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             テール部出力処理                        2.3.2    *
****************************************************************
 TAIL-WT-SEC            SECTION.
     MOVE     "TAIL-WT"           TO        S-NAME.
     MOVE     SPACE               TO        TL1  TL2.
*
*テール１行目
***  数量合計
     MOVE   SURYOKEI              TO        WK-SURYO.
***  数量合計（整数部）
     MOVE     WK-SURYO1           TO        TL1-011.
***  数量合計（端数部）
     IF       WK-SURYO2   =       ZERO
              MOVE     SPACE      TO        TL1-012
     ELSE
              MOVE     WK-SURYO2  TO        TL1-012
     END-IF.

*    訂正後数量合計
*****IF       TS-FLG =  1
     IF       GENKINKEI  NOT =  TSGENKEI
     OR       BAIKINKEI  NOT =  TSBAIKEI
              MOVE      TSSURYOKEI     TO   WK-SURYO
***           訂正後数量合計（整数部）
              MOVE     WK-SURYO1            TO        TL1-021
***           訂正後数量合計（端数部）
              IF       WK-SURYO2  =         ZERO
                       MOVE     SPACE       TO        TL1-022
              ELSE
                       MOVE     WK-SURYO2   TO        TL1-022
              END-IF
     END-IF.
***  原価金額合計
*****DISPLAY "GENKINKEI = " GENKINKEI UPON CONS.
     MOVE   GENKINKEI             TO        TL1-03.
***  売価金額合計
*****DISPLAY "BAIKINKEI = " BAIKINKEI UPON CONS.
     MOVE   BAIKINKEI             TO        TL1-04.

*テール２行目
***  訂正後合計欄が１マス２バイトである為の処理
***  訂正後原価金額合計
*****IF       TS-FLG =  1
     IF       GENKINKEI  NOT =  TSGENKEI
     OR       BAIKINKEI  NOT =  TSBAIKEI
              MOVE      ZERO      TO        ZERO-FLG
              MOVE      SPACE     TO        TL2-01
              IF        TSGENKEI  =  ZERO
                MOVE    ZERO      TO        TL2-01(17:1)
              ELSE
                PERFORM  VARYING  I FROM  1  BY  1  UNTIL I > 9
                   COMPUTE  J  =  I  *  2
                   IF  TSGENKEI(I:1)  NOT = ZERO
                       MOVE   1                 TO   ZERO-FLG
                       MOVE   TSGENKEI(I:1)     TO   TL2-01(J:1)
                   ELSE
                       IF      ZERO-FLG   =    1
                               MOVE     ZERO   TO    TL2-01(J:1)
                       END-IF
                   END-IF
                END-PERFORM
              END-IF
***  訂正後売価金額合計
              MOVE      ZERO      TO        ZERO-FLG
              MOVE      SPACE     TO        TL2-02
              IF        TSBAIKEI  =  ZERO
              MOVE      ZERO      TO        TL2-02(17:1)
              ELSE
                PERFORM  VARYING  I FROM  1  BY  1  UNTIL I > 9
                   COMPUTE  J  =  I  *  2
                   IF  TSBAIKEI(I:1)     NOT = ZERO
                       MOVE   1                 TO   ZERO-FLG
                       MOVE   TSBAIKEI(I:1)     TO   TL2-02(J:1)
                   ELSE
                       IF      ZERO-FLG   =    1
                               MOVE     ZERO   TO    TL2-02(J:1)
                       END-IF
                   END-IF
                END-PERFORM
              END-IF
     END-IF.
*
     COMPUTE  CNT-AFTER    =      25   -    L-CNT.
*
*テール部出力
***  テール１行目
     WRITE    PRT-REC        FROM      TL1  AFTER   CNT-AFTER.
***  伝票訂正（二重線）
*****IF       TS-FLG =  1
     IF       GENKINKEI  NOT =  TSGENKEI
     OR       BAIKINKEI  NOT =  TSBAIKEI
              WRITE     PRT-REC   FROM      TL11      AFTER  0
     END-IF.
***  テール２行目
     WRITE    PRT-REC        FROM      TL2  AFTER  3.
*改頁指示
     MOVE     SPACE     TO        PRT-REC.
     WRITE    PRT-REC   AFTER     PAGE.
*ワーク項目初期化
     MOVE     ZERO                TO        GENKINKEI
                                            BAIKINKEI
                                            SURYOKEI
                                            TSGENKEI
                                            TSBAIKEI
                                            TSSURYOKEI
                                            TS-FLG
                                            L-CNT.
*伝票枚数カウント
     ADD      1                   TO        CNT-DENPYO.
*
 TAIL-WT-EXIT.
     EXIT.
****************************************************************
*             テストプリント　                        2.4      *
****************************************************************
 TEST-PRT-SEC            SECTION.
     MOVE     "TEST-PRT"          TO      S-NAME.
*項目クリア
     MOVE     SPACE               TO   HD1 HD2 HD3
                                       DT1 DT2
                                       TL1 TL2.
*テスト印字項目セット
     MOVE   ALL "9"              TO   HD3-02  HD3-03  HD3-06
                                      HD3-09  HD3-10  HD3-11
                                      DT2-02  DT2-03  DT2-04
                                      DT2-061 DT2-071 DT2-081
                                      DT2-09  DT2-10  DT2-11
                                      TL1-011 TL1-021 TL1-03
                                      TL1-04.
     MOVE   ALL "*"              TO   HD1-02  HD2-01  HD3-01
                                      HD3-04  HD3-08
                                      DT1-011 DT1-012 DT1-013
                                      DT1-02  DT1-03  DT2-01
                                      DT2-062 DT2-072.
     MOVE   NC"＊"               TO   HD1-01.
*ヘッダ部
     WRITE    PRT-REC          FROM   HD1    AFTER    4.
     WRITE    PRT-REC          FROM   HD2    AFTER    2.
     WRITE    PRT-REC          FROM   HD3    AFTER    2.
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  3.
*明細部
     PERFORM   VARYING   L-CNT      FROM  1  BY  1
                                        UNTIL   L-CNT     >  6
              WRITE  PRT-REC   FROM   DT1       AFTER     1
              WRITE  PRT-REC   FROM   DT2       AFTER     1
     END-PERFORM.
*テール部
     WRITE    PRT-REC          FROM   TL1       AFTER     2.
*
     CLOSE    PRTFILE.
     OPEN     OUTPUT    PRTFILE.
     MOVE     ZERO      TO        L-CNT.
*
 TEST-PRT-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC          SECTION.
     MOVE     "DSP-WRITE"         TO   S-NAME.
*
     MOVE     SPACE               TO        DSP-CNTL.
     MOVE     "SCREEN"            TO        DSP-GRP.
     MOVE     "FSY11011"          TO        DSP-FMT.
*項目設定
***  伝票枚数
     MOVE     WK-MAI              TO        MD01.
***  プログラムＩＤ
     MOVE     "SSY6401L"          TO        PGID.
***  日付
     ACCEPT   SYS-DATE            FROM      DATE.
     IF       SYS-DATE(1:2)  >=   89
              MOVE      19        TO        WK-DATE8-YY1
     ELSE
              MOVE      20        TO        WK-DATE8-YY1
     END-IF.
     MOVE     SYS-DATE            TO        WK-DATE8-YY2.
     MOVE     WK-DATE8            TO        SDATE.
***  時間
     ACCEPT   SYS-TIME            FROM      TIME.
     MOVE     SYS-TIME(1:6)       TO        STIME.
***  伝票タイプ
     MOVE     "ＴＡ_型"          TO        DTYPE.
***  取引先名
     MOVE     "サンライズ殿向け"  TO        TORINM.
*画面出力
     WRITE    FSY11011.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面ＲＥＡＤ処理                                 *
****************************************************************
 DSP-READ-SEC           SECTION.
     MOVE    "DSP-READ"           TO   S-NAME.
*
     MOVE    "NE"                 TO        DSP-PRO.
     READ     DSPFILE.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             メッセージセット                                 *
****************************************************************
 ERR-MSG-SEC            SECTION.
     MOVE     "ERR-MSG"           TO   S-NAME.
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
*             終了処理                                3.0      *
****************************************************************
 END-SEC                SECTION.
     MOVE     "END-SEC"           TO   S-NAME.
*
     CLOSE    SZSHIRED   PRTFILE    DSPFILE.
*
 END-EXIT.
     EXIT.
