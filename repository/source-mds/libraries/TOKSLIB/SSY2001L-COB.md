# SSY2001L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY2001L.COB`

## ソースコード

```cobol
******************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ケーヨー新オンライン　　　        *
*    モジュール名　　　　：　ケーヨー新オンライン仕入伝票発行　*
*    作成日／更新日　　　：　99/11/01                          *
*    作成者／更新者　　　：　ＮＡＶ萩原                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    伝票フォーマット　　：　ＴＡ_型                          *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY2001L.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          99/11/01.
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
*ケーヨー新伝票データ（共通）　　
     SELECT   KSHIREDN  ASSIGN    TO        KSHIREDN
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
*  FILE =ケーヨー新伝票データ（共通）                          *
****************************************************************
 FD  KSHIREDN
                        BLOCK    CONTAINS  1    RECORDS
                        LABEL    RECORD    IS   STANDARD.
                        COPY     KSHIREDN  OF   XFDLIB
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
     03  TOTALPAGE-CNT            PIC  9(04)     VALUE  ZERO.
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
***  数量
 01  WK-SURYO                     PIC  9(06)V9.
 01  WK-SURYO-R  REDEFINES  WK-SURYO.
     03  WK-SURYO1                PIC  9(06).
     03  WK-SURYO2                PIC  9(01).
***  原価単価
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
     03  FILLER                   PIC  X(18)    VALUE SPACE.
     03  HD1-01                   PIC  X(25).
     03  FILLER                   PIC  X(60)    VALUE SPACE.
     03  HD1-02                   PIC  9(06).
 01  HD2.
     03  FILLER                   PIC  X(51)    VALUE SPACE.
     03  HD2-01                   PIC  X(14).
 01  HD3.
     03  FILLER                   PIC  X(07)    VALUE SPACE.
     03  HD3-01                   PIC  X(20).
     03  FILLER                   PIC  X(44).
     03  HD3-02                   PIC  X(20)    VALUE SPACE.
 01  HD4.
     03  FILLER                   PIC  X(07)     VALUE SPACE.
     03  HD4-01                   PIC  X(20).
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  HD4-02                   PIC  X(12).
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  HD4-03                   PIC  X(04).
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  HD4-04                   PIC  X(02).
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  HD4-05                   PIC  X(11).
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  HD4-06                   PIC  X(04).
     03  FILLER                   PIC  X(06)     VALUE SPACE.
     03  HD4-07                   PIC  X(20).
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  HD4-08                   PIC  9(06).
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  HD4-09                   PIC  9(06).
*
*--------------------------------------------------------------*
*    明細                                                      *
*--------------------------------------------------------------*
*
 01  DT1.
     03  FILLER                   PIC  X(05)     VALUE SPACE.
     03  DT1-01                   PIC  X(25).
     03  FILLER                   PIC  X(55)     VALUE   SPACE.
     03  DT1-02                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06).
     03  DT1-03                   PIC  ZZZZZZZZ9.
 01  DT2.
     03  FILLER                   PIC  X(05)     VALUE SPACE.
     03  DT2-01                   PIC  X(25).
     03  DT2-02                   PIC  X(13).
     03  FILLER                   PIC  X(15)     VALUE SPACE.
     03  DT2-031                  PIC  ZZZZ9.
     03  DT2-032                  PIC  Z.
     03  FILLER                   PIC  X(02)     VALUE SPACE.
     03  DT2-041                  PIC  ZZZZZ9.
     03  DT2-042                  PIC  Z.
     03  FILLER                   PIC  X(04)     VALUE SPACE.
     03  DT2-051                  PIC  ZZZZZ9.
     03  DT2-052                  PIC  ZZ.
     03  DT2-06                   PIC  ZZZZZZZZ9.
     03  DT2-07                   PIC  ZZZZZ9.
     03  DT2-08                   PIC  ZZZZZZZZ9.
*
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
*    ｺﾞｳｹｲ
 01  TL1.
     03  FILLER                   PIC  X(85)     VALUE SPACE.
     03  TL1-01                   PIC  ZZZZZZZZZ.
     03  FILLER                   PIC  X(06)     VALUE SPACE.
     03  TL1-02                   PIC  ZZZZZZZZZ.
*伝票訂正用（ＴＬ）
 01  TL11.
*****03  FILLER                   PIC  X(58)     VALUE SPACE.
*****03  FILLER                   PIC  X(06)      VALUE
*****                                            "======".
     03  FILLER                   PIC  X(85)     VALUE SPACE.
     03  FILLER                   PIC  X(09)     VALUE
                                                 "=========".
     03  FILLER                   PIC  X(06)     VALUE SPACE.
     03  FILLER                   PIC  X(09)     VALUE
                                                 "=========".
*伝票訂正合計欄
 01  TL2.
     03  FILLER                   PIC  X(72)     VALUE SPACE.
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
     USE AFTER          EXCEPTION      PROCEDURE      KSHIREDN.
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
     OPEN     INPUT     KSHIREDN.
     OPEN     I-O       DSPFILE.
     OPEN     OUTPUT    PRTFILE.
*    初期値セット
     MOVE     ZERO                TO   DENNO1.
     MOVE     ZERO                TO   CNT-DENPYO.
     MOVE     SPACE               TO   END-FLG.
     MOVE     SPACE               TO   FSY11011.
*    伝票総枚数読込み
*    READ     KSHIREDN   AT  END
*             MOVE       "END"    TO   PROGRAM-END
*             GO                  TO   INIT-EXIT
*    END-READ.
*　　画面表示用伝票枚数
*    MOVE     DEN-F40             TO  WK-MAI.
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
     CLOSE    KSHIREDN.
*
     OPEN     INPUT      KSHIREDN.
 INIT-EXIT.
     EXIT.
****************************************************************
*             伝票データファイル読込み                2.0      *
****************************************************************
 DEN-CNT-SEC            SECTION.
*
     READ     KSHIREDN AT  END
              MOVE     "END"     TO   END-FLG
              GO                 TO   DEN-CNT-EXIT
     END-READ.
*    ケーヨー新以外は読み飛ばし（取引先）
     IF       DEN-F01  NOT =  173
              GO                 TO   DEN-CNT-SEC
     END-IF.
*    伝票枚数カウント
     IF       DEN-A03  NOT =  WK-DEN-NO
              ADD    1        TO      WK-MAI
              MOVE   DEN-A03  TO      WK-DEN-NO
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
                   CLOSE     KSHIREDN  PRTFILE
                   OPEN      INPUT     KSHIREDN
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
                   CLOSE     KSHIREDN  PRTFILE
                   OPEN      INPUT     KSHIREDN
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
     READ     KSHIREDN    AT      END
              MOVE       "END"    TO        END-FLG
              MOVE        1       TO        ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO        FL-READ-EXIT
     END-READ.
***
*行_０の時
     IF       DEN-A23 =    ZERO
              GO                  TO        READ-000
     END-IF.
*MD04(印字パターン)=全件数印刷時
     IF       MD04    =    2  OR  ZEN-FLG   =  1
              GO                  TO        READ-020
     END-IF.
*
     IF       KAI-FLG   =    1    GO  TO    READ-010.
     IF       DEN-A03   =    MD02
              MOVE      1         TO        KAI-FLG
         ELSE
              GO        TO        FL-READ-SEC.
 READ-010.
     IF       ( SYU-FLG     = 1      )  AND
              ( DEN-A03 NOT = DEN-NO )
              MOVE      "END"     TO        END-FLG
              GO        TO        FL-READ-EXIT.
     IF       DEN-A03   =    MD03
              MOVE      DEN-A03   TO        DEN-NO
              MOVE      1         TO        SYU-FLG.
 READ-020.
*
     IF       RD-SW     =    0
              MOVE      DEN-A03   TO   DENNO1
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
     IF       DEN-A03   NOT =               DENNO1
              PERFORM   TAIL-WT-SEC
              PERFORM   HEAD-WT-SEC
              MOVE      DEN-A03   TO        DENNO1
     END-IF.
*
*明細部セット
     MOVE      SPACE     TO       DT1  DT2.

*明細１行目
***  商品名１
     MOVE     DEN-A34              TO        DT1-01.
***  訂正伝票時処理
     IF        DEN-F15   NOT =    DEN-F50
***            訂正後原価金額
               COMPUTE  DT1-02 =  DEN-F15 * DEN-F172
***            訂正後売価金額
               COMPUTE  DT1-03 =  DEN-F15 * DEN-F173
     END-IF.
*明細２行目
***  規格
     MOVE     DEN-A35             TO        DT2-01.
***  商品コード
     MOVE     DEN-A24             TO        DT2-02.
***  数量
     MOVE     DEN-F50             TO        WK-SURYO.
***  数量（整数部）
     MOVE     WK-SURYO1           TO        DT2-031.
***  数量（端数部）
     IF       WK-SURYO2   =      ZERO
              MOVE     ZERO       TO        DT2-032
     ELSE
              MOVE     WK-SURYO2  TO        DT2-032
     END-IF.
*
     COMPUTE  SURYOKEI  =   SURYOKEI  +  DEN-F50.
*    訂正後数量
     IF       DEN-F15   NOT =     DEN-F50
              MOVE      DEN-F15   TO       WK-SURYO
***           訂正後数量（整数部）
              MOVE     WK-SURYO1            TO        DT2-041
***           訂正後数量（端数部）
              IF       WK-SURYO2  =         ZERO
                       MOVE       ZERO      TO        DT2-042
              ELSE
                       MOVE     WK-SURYO2   TO        DT2-042
              END-IF
*
              COMPUTE  TSSURYOKEI  =  TSSURYOKEI  +  DEN-F15
     END-IF.
***  原単価
     MOVE     DEN-F512            TO        WK-GENKA.
***  原単価（整数部）
     MOVE     WK-GENKA1           TO        DT2-051.
***  原単価（端数部）
     IF       WK-GENKA2   =       ZERO
              MOVE     ZERO       TO        DT2-052
     ELSE
              MOVE WK-GENKA2      TO        DT2-052
     END-IF.
***  原価金額
     MOVE     DEN-F521            TO   DT2-06.
***  売価単価
     MOVE     DEN-F513            TO   DT2-07.
***  売価金額
     MOVE     DEN-F522            TO   DT2-08.
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
     PERFORM  FL-READ-SEC.
     IF       END-FLG   =         "END"
              PERFORM   TAIL-WT-SEC
     END-IF.
*
 DENP-WT-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部出力処理                        2.3.1    *
****************************************************************
 HEAD-WT-SEC            SECTION.
     MOVE     "HEAD-WT"           TO        S-NAME.
*ヘッダ項目初期化
     MOVE     SPACE               TO        HD1 HD2 HD3 HD4.
*
     ADD      1                   TO        TOTALPAGE-CNT.
*
*ヘッダ１行目
***  伝票メッセージ（Ｄ欄）
     MOVE     DEN-A12A            TO        HD1-01.
***  総枚数（Ｅ欄）
     MOVE     TOTALPAGE-CNT       TO        HD1-02.
*ヘッダ２行目
***  商品区分名称
     MOVE     DEN-A20             TO        HD2-01.
*ヘッダ３行目
***  発注企業名
     MOVE     DEN-A15             TO        HD3-01.
***  取引先名
     MOVE     DEN-A17             TO        HD3-02.
*ヘッダ４行目
***  店舗名
     MOVE     DEN-A16             TO        HD4-01.
***  店コード
     MOVE     DEN-A042(3:3)       TO        HD4-02.
***  分類コード
     MOVE     DEN-A05             TO        HD4-03.
***  伝票区分
     MOVE     DEN-A06             TO        HD4-04.
***  伝票番号
     MOVE     DEN-A03(1:2)        TO        HD4-05(1:2).
     MOVE     "-"                 TO        HD4-05(3:1).
     MOVE     DEN-A03(3:3)        TO        HD4-05(4:3).
     MOVE     "-"                 TO        HD4-05(7:1).
     MOVE     DEN-A03(6:4)        TO        HD4-05(8:4).
***  取引先コード
     MOVE     DEN-A091(3:4)       TO        HD4-06.
***  電話番号
     MOVE     DEN-A18             TO        HD4-07.
***  発注日
     MOVE     DEN-A07             TO        HD4-08.
***  納品日
     MOVE     DEN-A08             TO        HD4-09.
*訂正フラグチェック(TL-WRITE-SECで使用）
     IF       DEN-F53 = 1
              MOVE                1    TO   TS-FLG
     END-IF.
***
*ヘッダ部出力
     WRITE    PRT-REC        FROM      HD1       AFTER    3.
     WRITE    PRT-REC        FROM      HD2       AFTER    1.
     WRITE    PRT-REC        FROM      HD3       AFTER    1.
     WRITE    PRT-REC        FROM      HD4       AFTER    2.
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
*テール１行目
***  数量合計
***  MOVE   SURYOKEI              TO        WK-SURYO.
***  数量合計（整数部）
***  MOVE     WK-SURYO1           TO        TL1-011.
***  数量合計（端数部）
***  IF       WK-SURYO2   =       ZERO
***           MOVE     ZERO       TO        TL1-012
***  ELSE
***           MOVE     WK-SURYO2  TO        TL1-012
***  END-IF.
***  訂正後数量合計
***  IF       TS-FLG =  1
***           MOVE      TSSURYOKEI     TO   WK-SURYO
***           訂正後数量合計（整数部）
***           MOVE     WK-SURYO1            TO        TL1-021
***           訂正後数量合計（端数部）
***           IF       WK-SURYO2  =         ZERO
***                    MOVE       ZERO      TO        TL1-022
***           ELSE
***                    MOVE     WK-SURYO2   TO        TL1-022
***           END-IF
***  END-IF.
***  原価金額合計
     MOVE   GENKINKEI             TO        TL1-01.
***  売価金額合計
     MOVE   BAIKINKEI             TO        TL1-02.
*テール２行目
***  訂正後合計欄が１マス２バイトである為の処理
***  訂正後原価金額合計
***  IF       TS-FLG  = 1
     IF       GENKINKEI   NOT =    TSGENKEI
     OR       BAIKINKEI   NOT =    TSBAIKEI
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
***  IF       TS-FLG =  1
     IF       GENKINKEI   NOT =    TSGENKEI
     OR       BAIKINKEI   NOT =    TSBAIKEI
              WRITE     PRT-REC   FROM      TL11      AFTER  0
     END-IF.
***  テール２行目
     WRITE    PRT-REC        FROM      TL2  AFTER  4
*
*
     MOVE     SPACE     TO        PRT-REC.
     WRITE    PRT-REC   AFTER     PAGE.
*
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
*
*項目クリア
     MOVE     SPACE               TO   HD1 HD2 HD3 HD4
                                       DT1 DT2
                                       TL1 TL2.
*ヘッダ部
     MOVE     ALL "*"             TO        HD1-01.
     MOVE     9999                TO        HD1-02.
*********
     MOVE     ALL "*"             TO        HD2-01.
*********
     MOVE     ZERO                TO        L-CNT.
     MOVE     ALL "*"             TO        HD3-01
                                            HD3-02.
*********
     MOVE     ALL "*"             TO        HD4-01
                                            HD4-02
                                            HD4-03
                                            HD4-04
                                            HD4-05
                                            HD4-06
                                            HD4-07.
     MOVE     999999              TO        HD4-08
                                            HD4-09.
     WRITE    PRT-REC   FROM      HD1       AFTER     3.
     WRITE    PRT-REC   FROM      HD2       AFTER     1.
     WRITE    PRT-REC   FROM      HD3       AFTER     1.
     WRITE    PRT-REC   FROM      HD4       AFTER     2.
     MOVE     SPACE               TO        PRT-REC.
     WRITE    PRT-REC          AFTER  3.
 TEST-01.
*明細部
     MOVE     SPACE               TO        DT1      DT2.
     IF       L-CNT   =   6       GO  TO    TEST-02.
     MOVE     ALL "*"             TO        DT1-01
                                            DT2-01
                                            DT2-02.
     MOVE     999999              TO        DT2-031.
     MOVE     9                   TO        DT2-032.
     MOVE     999999              TO        DT2-051.
     MOVE     99                  TO        DT2-052.
     MOVE     999999999           TO        DT2-06.
     MOVE     999999999           TO        DT2-07.
     MOVE     999999999           TO        DT2-08.
     WRITE    PRT-REC   FROM      DT1       AFTER     1.
     WRITE    PRT-REC   FROM      DT2       AFTER     1.
     ADD      1                   TO        L-CNT.
     GO       TO        TEST-01.
 TEST-02.
*テール部
     MOVE     SPACE               TO        TL1.
     MOVE     999999999           TO        TL1-01.
     MOVE     999999999           TO        TL1-02.
     WRITE    PRT-REC   FROM      TL1       AFTER     2.
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
     MOVE     "SSY2001L"          TO        PGID.
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
     MOVE     "ケーヨー新殿向け"    TO        TORINM.
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
     CLOSE    KSHIREDN   PRTFILE    DSPFILE.
*
 END-EXIT.
     EXIT.

```
