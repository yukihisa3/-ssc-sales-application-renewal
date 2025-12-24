****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ホーマグリーンオンライン          *
*    モジュール名　　　　：　ＴＣ納品（納品シール発行）        *
*    作成日／更新日　　　：　2000/10/13                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    伝票フォーマット　　：　専用用紙                          *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY3905L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/10/13.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YB             IS        YB
         CONSOLE        IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ホーマック伝票データ（共通）
     SELECT   HMSHIRED  ASSIGN    TO        HMSHIRED
                        ACCESS    MODE IS   SEQUENTIAL
                        STATUS              DEN-ST.
*画面定義ファイル　　
     SELECT   DSPFILE   ASSIGN    TO        GS-DSPF
                        FORMAT              DSP-FMT
                        GROUP               DSP-GRP
                        PROCESSING          DSP-PRO
                        FUNCTION            DSP-FNC
                        FILE STATUS         DSP-ST.
*----<< 帳票ファイル >>-*
     SELECT  PRTFILE   ASSIGN     TO        GS-XU04LP
                       DESTINATION          "PRT"
                       FORMAT               PRT-FORM
                       GROUP                PRT-GRP
                       PROCESSING           PRT-PROC
                       UNIT CONTROL         PRT-CTL
                       FILE STATUS          PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
****************************************************************
*  FILE =ホーマック伝票データ（共通）                          *
****************************************************************
 FD  HMSHIRED
                        BLOCK    CONTAINS  1    RECORDS
                        LABEL    RECORD    IS   STANDARD.
                        COPY     HMSHIRED OF   XFDLIB
                        JOINING  DEN       AS   PREFIX.
****************************************************************
*    FILE = ｶﾞﾒﾝ  F                                            *
****************************************************************
 FD  DSPFILE
                        LABEL RECORD   IS   OMITTED.
*
     COPY    FSY39051   OF   XMDLIB.
****************************************************************
*    FILE = ﾌﾟﾘﾝﾄ F                                            *
****************************************************************
 FD  PRTFILE.
     COPY    FSY39052   OF   XMDLIB.
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
*    ＦＯＲＭ制御領域（帳票）
 01  PRT-FORM                    PIC  X(08).
 01  PRT-PROC                    PIC  X(02).
 01  PRT-GRP                     PIC  X(08).
 01  PRT-CTL.
     03  PRT-CNTRL               PIC  X(04).
     03  PRT-STR-PG              PIC  X(02).
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
     03  IX                       PIC  9(01)     VALUE  ZERO.
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
     03  WK-DEN-A181              PIC  X(05)     VALUE  SPACE.
     03  WK-DEN-A12               PIC  9(04)     VALUE  ZERO.
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
***  ﾃﾞﾝﾋﾟﾖｳ ｷ-
 01  WRK-DENNO.
     03  DENNO1                   PIC  9(09).
***  連番変換
 01  WRK-HEN.
     03  WRK-HEN01                PIC  X(01).
     03  WRK-HEN011               PIC  X(05).
     03  WRK-HEN02                PIC  9(04).
     03  WRK-HEN021               PIC  X(01).
     03  WRK-HEN03                PIC  X(01).
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
 01  WK-SURYO                     PIC  9(05)V9.
 01  WK-SURYO-R  REDEFINES  WK-SURYO.
     03  WK-SURYO1                PIC  9(05).
     03  WK-SURYO2                PIC  9(01).
 01  WK-GENKA                     PIC  9(06)V99.
 01  WK-GENKA-R  REDEFINES  WK-GENKA.
     03  WK-GENKA1                PIC  9(06).
     03  WK-GENKA2                PIC  9(02).
 01  WK-SYOCD                     PIC  X(13).
 01  WK-SYOCD-R  REDEFINES  WK-SYOCD.
     03  WK-HEN-SYOCD             PIC  9(13).
***  画面用日付取得
 01  WK-DATE8.
     03  WK-DATE8-YY1             PIC  9(02).
     03  WK-DATE8-YY2             PIC  9(06).
***  出力取引先ＣＤ
 01  WK-TOKCD                     PIC  9(08)   VALUE  298.
*01  WK-TOKCD                     PIC  9(08)   VALUE  999999.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
 DECLARATIVES.
*--- << プリンタエラー >> ---*
 000-PRTFILE-ERR        SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      PRTFILE.
     DISPLAY  FILE-ERR010         UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     DISPLAY  PRT-ST              UPON      CONS.
     ACCEPT   IN-DATA             FROM      CONS.
     MOVE     "4000"              TO        PROGRAM-STATUS.
     STOP     RUN.
*--- << 仕入データエラー >> ---*
 000-MAST-ERR           SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HMSHIRED.
     DISPLAY  FILE-ERR020         UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     DISPLAY  DEN-ST              UPON      CONS.
     ACCEPT   IN-DATA             FROM      CONS.
     MOVE     "4000"              TO        PROGRAM-STATUS.
     STOP     RUN.
*--- << 画面エラー >> ---*
 000-DSP-ERR            SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DSPFILE.
     DISPLAY  FILE-ERR030         UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     DISPLAY  DSP-ST              UPON      CONS.
     ACCEPT   IN-DATA             FROM      CONS.
     MOVE     "4000"              TO        PROGRAM-STATUS.
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
     OPEN     INPUT     HMSHIRED.
     OPEN     I-O       DSPFILE.
     OPEN     OUTPUT    PRTFILE.
*    初期値セット
     MOVE     ZERO                TO   DENNO1.
     MOVE     ZERO                TO   CNT-DENPYO.
     MOVE     SPACE               TO   END-FLG.
     MOVE     SPACE               TO   FSY39051.
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
     CLOSE    HMSHIRED.
*
     OPEN     INPUT      HMSHIRED.
 INIT-EXIT.
     EXIT.
****************************************************************
*             伝票データファイル読込み                2.0      *
****************************************************************
 DEN-CNT-SEC            SECTION.
*
     READ     HMSHIRED  AT  END
              MOVE     "END"     TO   END-FLG
              GO                 TO   DEN-CNT-EXIT
     END-READ.
*    ホーマック以外は読み飛ばし（取引先）
     IF       DEN-F01  NOT =  WK-TOKCD
              GO                 TO   DEN-CNT-SEC
     END-IF.
*    納品区分＝”１”（センター）以外読み飛ばし
*****DISPLAY "DEN-A15 = " DEN-A15 UPON CONS.
     IF       DEN-A15  NOT =  1
              GO                 TO   DEN-CNT-SEC
     END-IF.
*    伝票枚数カウント
     ADD      1                  TO   WK-MAI.
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
*                  プリントエリア初期化
                   MOVE     SPACE          TO   FSY39052
                   MOVE     ZERO           TO   IX
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     END-FLG   =   "END"
                   IF  IX  >  ZERO
*                      納品シール発行
                       PERFORM PRINT-SEC
                   END-IF
                   CLOSE     HMSHIRED  PRTFILE
                   OPEN      INPUT     HMSHIRED
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
*                  プリントエリア初期化
                   MOVE     SPACE          TO   FSY39052
                   MOVE     ZERO           TO   IX
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     END-FLG   =   "END"
                   IF  IX  >  ZERO
*                      納品シール発行
                       PERFORM PRINT-SEC
                   END-IF
                   CLOSE     HMSHIRED  PRTFILE
                   OPEN      INPUT     HMSHIRED
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
     READ     HMSHIRED    AT      END
              MOVE       "END"    TO        END-FLG
              MOVE        1       TO        ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO        FL-READ-EXIT
     END-READ.
*行_０の時
     IF       DEN-A03 =    ZERO
              GO                  TO        READ-000
     END-IF.
*    ホーマック以外は読み飛ばし（取引先）
     IF       DEN-F01  NOT =  WK-TOKCD
              GO                 TO   FL-READ-SEC
     END-IF.
*    納品区分＝”１”（センター）以外読み飛ばし
     IF       DEN-A15  NOT =  1
              GO                 TO   FL-READ-SEC
     END-IF.
*MD04(印字パターン)=全件数印刷時
     IF       MD04    =    2  OR  ZEN-FLG   = 1
              GO                  TO        FL-READ-EXIT
     END-IF.
*    印字開始ＦＬＧ＝ＯＮの時
     IF       KAI-FLG   =    1    GO  TO    READ-010.
*    開始印字位置判定
     IF       DEN-F02   =    MD02
              MOVE      1         TO        KAI-FLG
     ELSE
              GO                  TO        FL-READ-SEC
     END-IF.
 READ-010.
*    最終終了判定
     IF       ( SYU-FLG     = 1      )  AND
              ( DEN-F02 NOT = DEN-NO )
              MOVE      "END"     TO        END-FLG
              GO                  TO        FL-READ-EXIT
     END-IF.
*    終了伝票番号判定
     IF       DEN-F02   =    MD03
              MOVE      DEN-F02   TO        DEN-NO
              MOVE      1         TO        SYU-FLG
     END-IF.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             伝票出力処理                            2.3      *
****************************************************************
 DENP-WT-SEC            SECTION.
     MOVE     "DENP-WT"           TO        S-NAME.
***  カウンター
     ADD       1                  TO   IX.
***  カウンターチェック
     IF    IX  >  4
*          納品シール印字
           PERFORM PRINT-SEC
           MOVE   1               TO   IX
     END-IF.
***  タイトル行セット
     MOVE     NC"【店舗】"        TO   T01(IX).
     MOVE     NC"【伝票_】"      TO   T02(IX).
     MOVE     ALL NC"―"          TO   T03(IX).
     MOVE     NC"【品番】"        TO   T04(IX).
     MOVE     NC"【数量】"        TO   T05(IX).
     MOVE     ALL NC"―"          TO   T06(IX).
     MOVE     NC"【連番】"        TO   T07(IX).
     MOVE     NC"【部門】"        TO   T08(IX).
     MOVE     NC"【売価】"        TO   T09(IX).
     MOVE     "-"                 TO   T10(IX).
***  店舗ＣＤ
     MOVE     DEN-A081            TO   K01(IX).
***  店舗名称
     MOVE     DEN-A09             TO   K02(IX).
***  伝票番号
     MOVE     DEN-A03             TO   K03(IX).
***  品番
***  MOVE     DEN-A27             TO   K04(IX).
     MOVE     DEN-A201            TO   K04(IX).
***  商品名称１
     MOVE     DEN-A211            TO   K05(IX).
***  商品名称２
     MOVE     DEN-A212            TO   K06(IX).
***  数量
     MOVE     DEN-A22             TO   K07(IX).
***  連番１
     MOVE     DEN-A12(1:3)        TO   K08(IX).
***  連番２
     MOVE     DEN-A12(4:1)        TO   K09(IX).
***  部門
     MOVE     DEN-A101            TO   K10(IX).
***  売価単価
     MOVE     DEN-A25             TO   K11(IX).
*
     PERFORM  FL-READ-SEC.
*
 DENP-WT-EXIT.
     EXIT.
****************************************************************
*             テストプリント　                        2.4      *
****************************************************************
 TEST-PRT-SEC            SECTION.
     MOVE     "TEST-PRT"          TO      S-NAME.
*テスト印字項目セット
*    プリントエリア初期化
     MOVE     SPACE      TO       FSY39052.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 4
             MOVE ALL NC"Ｎ" TO T01(IX) T02(IX) T03(IX) T04(IX)
                                T05(IX) T06(IX) T07(IX) T08(IX)
                                T09(IX)
             MOVE "X"        TO T10(IX)
             MOVE ALL "9"    TO K01(IX) K03(IX) K07(IX) K08(IX)
                                K09(IX) K10(IX) K11(IX)
             MOVE ALL "X"    TO K02(IX) K04(IX) K05(IX) K06(IX)
     END-PERFORM.
*    納品シール印字
     PERFORM PRINT-SEC.
*
     CLOSE    PRTFILE.
     OPEN     OUTPUT    PRTFILE.
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
     MOVE     "FSY39051"          TO        DSP-FMT.
*項目設定
***  伝票枚数
     MOVE     WK-MAI              TO        MD01.
***  プログラムＩＤ
     MOVE     "SSY3905L"          TO        PGID.
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
     MOVE     "専用納品シール"    TO        DTYPE.
***  取引先名
     MOVE     "ホーマック殿向け"  TO        TORINM.
*画面出力
     WRITE    FSY39051.
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
     CLOSE    HMSHIRED   PRTFILE    DSPFILE.
*
 END-EXIT.
     EXIT.
****************************************************************
*             納品シール印字                          3.0      *
****************************************************************
 PRINT-SEC              SECTION.
     MOVE     "PRINT-SEC"         TO   S-NAME.
*    印字制御項目セット
     MOVE     SPACE               TO   PRT-PROC.
     MOVE     SPACE               TO   PRT-CTL.
     MOVE     SPACE               TO   PRT-FORM.
     MOVE    "FSY39052"           TO   PRT-FORM.
     MOVE    "SCREEN"             TO   PRT-GRP.
*    納品シール発行
     WRITE    FSY39052.
*    プリントエリア初期化
     MOVE     SPACE               TO   FSY39052.
*
 PRINT-EXIT.
     EXIT.
