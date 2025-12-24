****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　イオンオンライン　　　　　　　　　*
*    モジュール名　　　　：　イオンオンライン仕入伝票発行　　　*
*    作成日／更新日　　　：　2002/09/25                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    伝票フォーマット　　：　ＴＡ２型                          *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY4501L.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          99/08/16.
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
*イオン伝票データ（共通）　　
     SELECT   IOSHIRED  ASSIGN    TO        IOSHIRED
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
*  FILE =イオン伝票データ（共通）                              *
****************************************************************
 FD  IOSHIRED
                        BLOCK    CONTAINS  1    RECORDS
                        LABEL    RECORD    IS   STANDARD.
                        COPY     IOSHIRED  OF   XFDLIB
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
     03  CHK-FLG                  PIC  X(01)     VALUE  SPACE.
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
     03  WK-JYODAN                PIC  X(16)     VALUE  SPACE.
     03  WK-TIRASI                PIC  X(04)     VALUE  SPACE.
     03  KESU                     PIC  9(06)     VALUE  ZERO.
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
     03  FILLER                   PIC  X(19)     VALUE   SPACE.
     03  HD1-01                   PIC  X(04).
     03  HD1-02                   PIC  9(05).
*
 01  HD2                          CHARACTER  TYPE   IS   YA.
     03  FILLER                   PIC  X(19)     VALUE   SPACE.
     03  HD2-01                   PIC  X(04).
     03  HD2-02                   PIC  X(15).
     03  FILLER                   PIC  X(41)     VALUE   SPACE.
     03  HD2-03                   PIC  N(01).
*
 01  HD3.
     03  FILLER                   PIC  X(19)     VALUE   SPACE.
     03  HD3-01                   PIC  X(04).
     03  HD3-02                   PIC  X(01).
     03  HD3-03                   PIC  X(01).
     03  FILLER                   PIC  X(68)     VALUE   SPACE.
     03  HD3-04                   PIC  X(06).
     03  HD3-05                   PIC  X(06).
*
 01  HD4.
     03  FILLER                   PIC  X(08)     VALUE   SPACE.
     03  HD4-01                   PIC  X(12).
*
 01  HD5.
     03  FILLER                   PIC  X(08)     VALUE   SPACE.
     03  HD5-01                   PIC  X(12).
     03  FILLER                   PIC  X(12)     VALUE   SPACE.
     03  HD5-02                   PIC  X(06).
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  HD5-03                   PIC  X(04).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD5-04                   PIC  X(02).
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  HD5-05                   PIC  9(07).
     03  HD5-06                   PIC  Z99999.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  HD5-07                   PIC  X(20).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD5-08                   PIC  9(06).
     03  HD5-09                   PIC  9(06).
*****03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD5-10                   PIC  X(02).
*
*--------------------------------------------------------------*
*    明細                                                      *
*--------------------------------------------------------------*
*
 01  DT1                          CHARACTER  TYPE  IS   YA.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  DT1-01                   PIC  X(20).
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  DT1-02                   PIC  X(13).
     03  FILLER                   PIC  X(21)     VALUE   SPACE.
     03  DT1-03                   PIC  N(01).
     03  FILLER                   PIC  X(19)     VALUE   SPACE.
     03  DT1-04                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06).
     03  DT1-05                   PIC  ZZZZZZZZ9.
*
 01  DT21.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  DT21-01                  PIC  X(20).
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  DT21-03                  PIC  X(13).
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  DT21-04                  PIC  ZZZ.
*****03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  DT21-05                  PIC  ZZZZZ.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  DT21-06                  PIC  X(01).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  DT21-071                 PIC  ZZZZ9.
     03  DT21-072                 PIC  Z.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  DT21-081                 PIC  ZZZZZ9.
     03  DT21-082                 PIC  Z.
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  DT21-091                 PIC  ZZZZZ9.
     03  DT21-092                 PIC  99.
     03  DT21-10                  PIC  ZZZZZZZZ9.
     03  DT21-11                  PIC  ZZZZZ9.
     03  DT21-12                  PIC  ZZZZZZZZ9.
*
 01  DT22.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  DT22-01                  PIC  X(20).
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  DT22-03                  PIC  X(13).
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  DT22-04                  PIC  X(04).
     03  DT22-05                  PIC  ZZZZZ.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  DT22-06                  PIC  X(01).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  DT22-071                 PIC  ZZZZZ.
     03  DT22-072                 PIC  Z.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  DT22-081                 PIC  ZZZZZ9.
     03  DT22-082                 PIC  Z.
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  DT22-091                 PIC  ZZZZZ9.
     03  DT22-092                 PIC  99.
     03  DT22-10                  PIC  ZZZZZZZZ9.
     03  DT22-11                  PIC  ZZZZZ9.
     03  DT22-12                  PIC  ZZZZZZZZ9.

*伝票訂正用（ＤＴ）
 01  DT23.
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
*
 01  TL1.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  TL2-01                   PIC  X(15).
     03  FILLER                   PIC  X(64)     VALUE   SPACE.
     03  TL1-01                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  TL1-02                   PIC  ZZZZZZZZ9.
 01  TL2.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  TL3-01                   PIC  X(09).
     03  TL3-02                   PIC  X(02).
     03  TL3-03                   PIC  X(01).
     03  TL3-04                   PIC  X(02).
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  TL3-05                   PIC  X(02).
     03  TL3-06                   PIC  ZZZZZZ.
     03  TL3-07                   PIC  X(03).
     03  FILLER                   PIC  X(51)     VALUE   SPACE.
     03  TL2-02                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  TL2-03                   PIC  ZZZZZZZZ9.
*伝票訂正用（ＴＬ）
 01  TL21.
     03  FILLER                   PIC  X(64)     VALUE SPACE.
*    03  FILLER                   PIC  X(06)      VALUE
*                                                "======".
     03  FILLER                   PIC  X(21)     VALUE SPACE.
     03  FILLER                   PIC  X(09)     VALUE
                                                 "=========".
     03  FILLER                   PIC  X(06)     VALUE SPACE.
     03  FILLER                   PIC  X(09)     VALUE
                                                 "=========".
 01  TL3.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  TL4-01                   PIC  X(09).
     03  TL4-02                   PIC  X(02).
     03  TL4-03                   PIC  X(01).
     03  TL4-04                   PIC  X(02).
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  TL4-05                   PIC  X(02).
     03  TL4-06                   PIC  ZZZZZZ.
     03  TL4-07                   PIC  X(03).
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
     USE AFTER          EXCEPTION      PROCEDURE      IOSHIRED.
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
     OPEN     INPUT     IOSHIRED.
     OPEN     I-O       DSPFILE.
     OPEN     OUTPUT    PRTFILE.
*    初期値セット
     MOVE     ZERO                TO   DENNO1.
     MOVE     ZERO                TO   CNT-DENPYO.
     MOVE     ZERO                TO   KESU.
     MOVE     SPACE               TO   END-FLG.
     MOVE     SPACE               TO   FSY11011.
*    伝票総枚数読込み
*////READ     IOSHIRED   AT  END
*////         MOVE       "END"    TO   PROGRAM-END
*////         GO                  TO   INIT-EXIT
*////END-READ.
*　　画面表示用伝票枚数
     PERFORM  DEN-CNT-SEC  UNTIL  END-FLG = "END".
*    伝票枚数チェック
     IF       WK-MAI  <=  ZERO
              DISPLAY NC"対象データ無し" UPON CONS
              MOVE    "END"      TO   PROGRAM-END
              GO                 TO   INIT-EXIT
     END-IF.
*////MOVE     DEN-F40             TO  WK-MAI.
     CLOSE    IOSHIRED.
*
     OPEN     INPUT      IOSHIRED.
 INIT-EXIT.
     EXIT.
****************************************************************
*             伝票データファイル読込み                2.0      *
****************************************************************
 DEN-CNT-SEC            SECTION.
*
     READ     IOSHIRED AT  END
              MOVE     "END"     TO   END-FLG
              GO                 TO   DEN-CNT-EXIT
     END-READ.
*    イオン以外は読み飛ばし（取引先）
     IF       DEN-F01  NOT =  87372
              GO                 TO   DEN-CNT-SEC
     END-IF.
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
                   CLOSE     IOSHIRED  PRTFILE
                   OPEN      INPUT     IOSHIRED
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
                   CLOSE     IOSHIRED  PRTFILE
                   OPEN      INPUT     IOSHIRED
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
     READ     IOSHIRED    AT      END
              MOVE       "END"    TO        END-FLG
              MOVE        1       TO        ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO        FL-READ-EXIT
     END-READ.
*行_０の時
     IF       DEN-A04 =    ZERO
              GO                  TO        READ-000
     END-IF.
*MD04(印字パターン)=全件数印刷時
     IF       MD04    =    2  OR  ZEN-FLG   = 1
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
     MOVE      SPACE     TO       CHK-FLG.
     MOVE      SPACE     TO       DT1  DT21  DT22.
*明細１行目
*    品名
     MOVE     DEN-A16             TO   DT1-01.
*    取引先品番（ＪＡＮコード）
     MOVE     DEN-A15             TO   DT1-02.
***  訂正伝票時処理
     IF        DEN-F15   NOT =    DEN-F50
***            行_◯表示
               MOVE      NC"◯"   TO   DT1-03
***            訂正後原価金額
               COMPUTE  DT1-04 =  DEN-F15 * DEN-F172
***            訂正後売価金額
               COMPUTE  DT1-05 =  DEN-F15 * DEN-F173
     END-IF.
*明細２行目
*    規格
     MOVE     DEN-A17             TO   DT21-01  DT22-01.
*    クラス
*****MOVE     DEN-A24             TO   DT22-02.
*    商品コード
     MOVE     DEN-A35             TO   DT21-03  DT22-03.
*    入数
     IF       DEN-A22   =   SPACE
              MOVE      ZERO      TO   DT21-04
              MOVE      ZERO      TO   DT21-05
              MOVE      SPACE     TO   DT21-06
     ELSE
              IF  DEN-A22 = "1"
                  MOVE  "1"       TO   CHK-FLG
                  MOVE  "*ｺｽｳ"    TO   DT22-04
                  MOVE   ZERO     TO   DT22-05
                  MOVE   SPACE    TO   DT22-06
              ELSE
                  MOVE   DEN-A23  TO   DT21-04 DT22-04
******************COMPUTE DT21-05 = ( DEN-F15 / DEN-A23 ) + 0.9
******************COMPUTE DT22-05 = ( DEN-F15 / DEN-A23 ) + 0.9
******************COMPUTE KESU = KESU + (DEN-F15 / DEN-A23 + 0.9)
                  COMPUTE DT21-05 = ( DEN-F50 / DEN-A23 ) + 0.9
                  COMPUTE DT22-05 = ( DEN-F50 / DEN-A23 ) + 0.9
                  COMPUTE KESU = KESU + (DEN-F50 / DEN-A23 + 0.9)
                  MOVE   DEN-A22  TO   DT21-06 DT22-06
              END-IF
     END-IF.
***  数量
     MOVE     DEN-F50             TO        WK-SURYO.
***  数量（整数部）
     MOVE     WK-SURYO1           TO        DT21-071 DT22-071.
***  数量（端数部）
     IF       WK-SURYO2   =      ZERO
              MOVE     ZERO       TO        DT21-072 DT22-072
     ELSE
              MOVE     WK-SURYO2  TO        DT21-072 DT22-072
     END-IF.
*
*    COMPUTE  SURYOKEI  =   SURYOKEI  +  DEN-F50.
*    訂正後数量
     IF       DEN-F15   NOT =     DEN-F50
              MOVE      DEN-F15   TO       WK-SURYO
***           訂正後数量（整数部）
              MOVE     WK-SURYO1  TO        DT21-081 DT22-081
***           訂正後数量（端数部）
              IF       WK-SURYO2  =         ZERO
                       MOVE  ZERO       TO  DT21-082 DT22-082
              ELSE
                       MOVE  WK-SURYO2  TO  DT21-082 DT22-082
              END-IF
*
*             COMPUTE  TSSURYOKEI  =  TSSURYOKEI  +  DEN-F15
     END-IF.
***  原単価
     MOVE     DEN-F512            TO        WK-GENKA.
***  原単価（整数部）
     MOVE     WK-GENKA1           TO        DT21-091 DT22-091.
***  原単価（端数部）
     IF       WK-GENKA2   =       ZERO
              MOVE     ZERO       TO        DT21-092 DT22-092
     ELSE
              MOVE WK-GENKA2      TO        DT21-092 DT22-092
     END-IF.
***  原価金額
     MOVE     DEN-F521            TO        DT21-10  DT22-10.
***  売価単価
     MOVE     DEN-F513            TO        DT21-11 DT22-11.
***  売価金額
     MOVE     DEN-F522            TO        DT21-12 DT22-12.
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
***  訂正無時（TAIL-WT-SEC で使用）
              COMPUTE   TSGENKEI  = TSGENKEI + DEN-F521
              COMPUTE   TSBAIKEI  = TSBAIKEI + DEN-F522
     END-IF.
*
*明細１行目出力　　　
     WRITE    PRT-REC   FROM      DT1       AFTER     1.
*明細２行目出力
     IF    CHK-FLG  = SPACE
              WRITE     PRT-REC   FROM   DT21   AFTER 1
     ELSE
              WRITE     PRT-REC   FROM   DT22   AFTER 1
     END-IF.
*訂正伝票時処理（明細２行目二重線）
     IF       DEN-F15   NOT = DEN-F50
              WRITE     PRT-REC   FROM   DT23  AFTER     0
     END-IF.
*
     ADD      2                   TO        L-CNT.
*
     PERFORM  FL-READ-SEC.
     IF       END-FLG   =         "END"
              PERFORM   TAIL-WT-SEC
     END-IF.
 DENP-WT-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部出力処理                        2.3.1    *
****************************************************************
 HEAD-WT-SEC            SECTION.
     MOVE     "HEAD-WT"           TO        S-NAME.
     MOVE     SPACE               TO        HD1 HD2 HD3
                                            HD4 HD5.
*
*ヘッダ１行目
***  Ｄ欄（上段）
     MOVE     "SEQ-"              TO        HD1-01.
     MOVE     DEN-A30             TO        HD1-02.
*
*ヘッダ２行目
***  ルート
     MOVE     "ﾙｰﾄ-"              TO        HD2-01.
***  Ｄ欄（中段）
     MOVE     DEN-A31             TO        HD2-02.
***  訂正フラグ＝１の時　訂正区分に"◯"を印刷
     IF       DEN-F53  =  1
              MOVE     NC"◯"     TO        HD2-03
     END-IF.
*
*ヘッダ３行目
***  Ｄ欄（下段）
     IF       DEN-A32  NOT =  SPACE
              MOVE     "ﾌﾛｱ-"     TO        HD3-01
              MOVE     DEN-A32    TO        HD3-02
              MOVE     "F"        TO        HD3-03
     END-IF.
***  Ｅ欄
     MOVE     "ﾃﾞﾝNO "            TO        HD3-04.
     MOVE     DEN-A03(1:6)        TO        HD3-05.
*
*ヘッダ４行目
***  社名
     MOVE     DEN-A11             TO        HD4-01.
*
*ヘッダ５行目
***  店名
     MOVE     DEN-A28             TO        HD5-01.
***  店コード
***  MOVE     DEN-A27             TO        HD5-02.
     MOVE     SPACE               TO        HD5-02.
     MOVE     DEN-A27             TO        HD5-02.
***  分類コード(大分類)
     MOVE     DEN-A06             TO        HD5-03.
***  伝票区分
     MOVE     DEN-A07             TO        HD5-04.
***  伝票_
     MOVE     DEN-A03             TO        HD5-05.
***  取引先コード
     MOVE     DEN-A01(3:6)        TO        HD5-06.
***  取引先名
     MOVE     DEN-A10             TO        HD5-07.
***  発注日
     MOVE     DEN-A12             TO        HD5-08.
***  納品日
     MOVE     DEN-A13             TO        HD5-09.
***  便
     MOVE     DEN-A082            TO        HD5-10.
*
*Ｆ欄退避
***  上段
     MOVE     DEN-A34             TO        WK-JYODAN.
*    下段（チラシ）
     MOVE     DEN-A33             TO        WK-TIRASI.
*
*訂正フラグチェック(TL-WRITE-SECで使用）
     IF       DEN-F53 = 1
              MOVE                1    TO   TS-FLG
     END-IF.
*
*ヘッダ部出力
     WRITE    PRT-REC          FROM   HD1   AFTER    1.
     WRITE    PRT-REC          FROM   HD2   AFTER    1.
     WRITE    PRT-REC          FROM   HD3   AFTER    1.
     WRITE    PRT-REC          FROM   HD4   AFTER    1.
     WRITE    PRT-REC          FROM   HD5   AFTER    1.
*    印字位置送り
     MOVE     SPACE               TO        PRT-REC.
     WRITE    PRT-REC          AFTER  1.
*    行カウンタ
     MOVE     7                   TO        L-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             テール部出力処理                        2.3.2    *
****************************************************************
 TAIL-WT-SEC            SECTION.
     MOVE     "TAIL-WT"           TO        S-NAME.
     MOVE     SPACE               TO        TL1  TL2  TL3.
*テール１行目
*****IF       TS-FLG =  1
     IF       GENKINKEI  NOT =  TSGENKEI
     OR       BAIKINKEI  NOT =  TSBAIKEI
***           訂正後原価金額計
              MOVE      TSGENKEI       TO   TL1-01
***           訂正後売価金額計
              MOVE      TSBAIKEI       TO   TL1-02
     END-IF.
*テール２行目
***  Ｆ欄上段
     MOVE     WK-JYODAN           TO        TL2-01.
***  原価金額合計
     MOVE   GENKINKEI             TO        TL2-02.
***  売価金額合計
     MOVE   BAIKINKEI             TO        TL2-03.
*テール３行目
***  チラシ開始日
     IF     WK-TIRASI  NOT =  SPACE
            IF  WK-TIRASI  NOT =  "0000"
                MOVE   "ﾁﾗｼｶｲｼﾋﾞ-"   TO   TL3-01
                MOVE   "."           TO   TL3-03
                MOVE WK-TIRASI(1:2)  TO   TL3-02
                MOVE WK-TIRASI(3:2)  TO   TL3-04
            END-IF
     END-IF.
*    ケース合計
     IF     KESU     NOT = ZERO
            MOVE   "ｹｲ"           TO   TL3-05
            MOVE   KESU           TO   TL3-06
            MOVE   "ｹｰｽ"          TO   TL3-07
     END-IF.
*
     COMPUTE  CNT-AFTER    =      26   -    L-CNT.
*
*テール部出力
***  テール１行目
     WRITE    PRT-REC        FROM      TL1  AFTER   CNT-AFTER.
***  テール２行目
     WRITE    PRT-REC        FROM      TL2  AFTER   1.
***  伝票訂正（二重線）
*****IF       TS-FLG =  1
     IF       GENKINKEI  NOT =  TSGENKEI
     OR       BAIKINKEI  NOT =  TSBAIKEI
              WRITE     PRT-REC   FROM      TL21      AFTER  0
     END-IF.
***  テール３行目
***  WRITE    PRT-REC        FROM      TL3  AFTER  1.
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
                                            KESU
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
     MOVE     SPACE               TO   HD1 HD2 HD3 HD4 HD5
                                       DT1 DT21 DT22
                                       TL2 TL3.
*テスト印字項目セット
     MOVE   ALL "9"              TO    HD1-02   HD4-01   HD5-01
                                       HD5-05   HD5-06   HD5-08
                                       HD5-09   DT21-04  DT21-05
                                       DT21-071 DT21-091
                                       DT21-072 DT21-092 DT21-10
                                       DT21-11  DT21-12  TL2-02
                                       TL2-03   TL3-06.
     MOVE   ALL "*"              TO    HD2-02   HD3-01   HD3-05
                                       HD5-02   HD5-03   HD5-04
                                       HD5-07   HD5-10   DT1-01
                                       DT1-02   DT21-01
                                       DT21-03  TL2-01   TL3-02
                                       TL3-04   HD3-02   TL3-01
                                       TL3-03   TL3-05   TL3-07
                                       DT21-06  HD3-03
                                       HD1-01   HD2-01   HD3-04.
*ヘッダ部
     WRITE    PRT-REC          FROM    HD1        AFTER    1.
     WRITE    PRT-REC          FROM    HD2        AFTER    1.
     WRITE    PRT-REC          FROM    HD3        AFTER    1.
     WRITE    PRT-REC          FROM    HD4        AFTER    1.
     WRITE    PRT-REC          FROM    HD5        AFTER    1.
     MOVE     SPACE            TO      PRT-REC.
     WRITE    PRT-REC          AFTER   1.
*明細部
     PERFORM   VARYING   L-CNT      FROM  1  BY  1
                                        UNTIL   L-CNT     >  9
              WRITE  PRT-REC   FROM    DT1       AFTER     1
              WRITE  PRT-REC   FROM    DT21       AFTER     1
     END-PERFORM.
*テール部
     WRITE    PRT-REC          FROM    TL2       AFTER     2.
     WRITE    PRT-REC          FROM    TL3       AFTER     1.
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
     MOVE     "SSY4501L"          TO        PGID.
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
     MOVE     "イオン殿向け"      TO        TORINM.
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
     CLOSE    IOSHIRED   PRTFILE    DSPFILE.
*
 END-EXIT.
     EXIT.
