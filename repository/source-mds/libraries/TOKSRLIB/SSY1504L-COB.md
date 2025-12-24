# SSY1504L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY1504L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　セキチューオンライン　　　        *
*    モジュール名　　　　：　セキチュー資材伝票発行（資材）　　*
*    作成日／更新日　　　：　2018/03/20                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    伝票フォーマット　　：　ＴＡ_型                          *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY1504L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          18/03/20.
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
*伝票発行用ワーク
     SELECT   BMSDENF   ASSIGN         DA-01-VS-BMSDENF
                        ORGANIZATION   SEQUENTIAL
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   DEN-ST.
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
******************************************************************
*伝票発行用ワーク
******************************************************************
 FD  BMSDENF
                        LABEL     RECORD   IS   STANDARD.
     COPY     BMSDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
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
     03  IX                       PIC  9(02)     VALUE  ZERO.
     03  IY                       PIC  9(02)     VALUE  ZERO.
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
     03  WK-BIKOU3                PIC  X(06)     VALUE  SPACE.
     03  WK-H-TOTAL               PIC  9(02)     VALUE  ZERO.
     03  WK-TOKUBAI               PIC  X(06)     VALUE  SPACE.
     03  WK-DENKU                 PIC  X(02)     VALUE  SPACE.
     03  WK-TORICD                PIC  9(08)     VALUE  ZERO.
     03  END-CHK                  PIC  X(03)     VALUE  SPACE.
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
         NC"開始が終了を超えています。".
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
 01  WK-GYO                       PIC  X(02).
 01  WK-GYO-R    REDEFINES  WK-GYO.
     03  WK-GYO-H                 PIC  9(02).
 01  WK-DENNO                     PIC  X(09).
 01  WK-DENNO-R  REDEFINES  WK-DENNO.
     03  WK-DENNO-H               PIC  9(09).
 01  WK-TOKCD                     PIC  X(06).
 01  WK-TOKCD-R  REDEFINES  WK-TOKCD.
     03  WK-TOKCD-H               PIC  9(06).
***  画面用日付取得
 01  WK-DATE8.
     03  WK-DATE8-YY1             PIC  9(02).
     03  WK-DATE8-YY2             PIC  9(06).
*
 01  PRT-WORK.
     03  HD-DENMSG                PIC  X(22).
     03  HD-TEISEI                PIC  X(01).
     03  HD-TORINM                PIC  X(20).
     03  HD-KAISYANM              PIC  X(20).
     03  HD-TENPONM               PIC  X(20).
     03  HD-TENCD                 PIC  X(05).
     03  HD-BUNRUI                PIC  X(04).
     03  HD-DENKU                 PIC  X(02).
     03  HD-DENNO                 PIC  X(09).
     03  HD-TORICD                PIC  X(06).
     03  HD-HATDATE               PIC  X(08).
     03  HD-NOUDATE               PIC  X(08).
     03  HD-SURYOKEI              PIC  9(07)V9(01).
     03  HD-GENKAKEI              PIC  9(09).
     03  HD-BAIKAKEI              PIC  9(09).
     03  HD-TSURYOKEI             PIC  9(07)V9(01).
     03  HD-TGENKAKEI             PIC  9(09).
     03  HD-TBAIKAKEI             PIC  9(09).
     03  HD-HASYU                 PIC  9(02).
     03  MEISAI-GYO               OCCURS  9.
         05  MS-SYONM             PIC X(20).
         05  MS-KIKAKUNM          PIC X(20).
         05  MS-JANCD             PIC X(13).
         05  MS-SURYO             PIC 9(06)V9(01).
         05  MS-TSURYO            PIC 9(06)V9(01).
         05  MS-GENKA             PIC 9(08)V9(02).
         05  MS-GENKAKIN          PIC 9(10).
         05  MS-BAIKA             PIC 9(08).
         05  MS-BAIKAKIN          PIC 9(10).
         05  MS-TGENKAKIN         PIC 9(10).
         05  MS-TBAIKAKIN         PIC 9(10).
         05  MS-TEISEI            PIC X(01).
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD0                          CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC  X(20)     VALUE SPACE.
     03  HD0-01                   PIC  X(22).
     03  FILLER                   PIC  X(40)     VALUE SPACE.
     03  HD0-02                   PIC  N(01).
     03  HD0-03                   PIC  N(01).
*
 01  HD1.
     03  FILLER                   PIC  X(26)     VALUE SPACE.
     03  HD1-01                   PIC  X(20).
     03  FILLER                   PIC  X(42)     VALUE SPACE.
     03  HD1-02                   PIC  X(01).
     03  HD1-03                   PIC  X(02).
     03  HD1-04                   PIC  X(01).
*
 01  HD2.
     03  FILLER                   PIC  X(09)     VALUE SPACE.
     03  HD2-01                   PIC  X(11).
     03  FILLER                   PIC  X(52)     VALUE SPACE.
     03  HD2-02                   PIC  X(20).
*
 01  HD3.
     03  FILLER                   PIC  X(10)     VALUE SPACE.
     03  HD3-01                   PIC  X(15).
     03  FILLER                   PIC  X(05)     VALUE SPACE.
     03  HD3-02                   PIC  X(05).
     03  FILLER                   PIC  X(07)     VALUE SPACE.
     03  HD3-03                   PIC  X(04).
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  HD3-04                   PIC  X(02).
     03  FILLER                   PIC  X(03)     VALUE SPACE.
     03  HD3-05                   PIC  X(09).
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  HD3-06                   PIC  X(06).
     03  FILLER                   PIC  X(04)     VALUE SPACE.
     03  HD3-07                   PIC  X(20).
     03  FILLER                   PIC  X(02)     VALUE SPACE.
     03  HD3-081                  PIC  XX.
     03  HD3-082                  PIC  XX.
     03  HD3-083                  PIC  XX.
     03  HD3-091                  PIC  XX.
     03  HD3-092                  PIC  XX.
     03  HD3-093                  PIC  XX.
*
*--------------------------------------------------------------*
*    明細                                                      *
*--------------------------------------------------------------*
*
 01  DT1.
     03  FILLER                   PIC  X(07)     VALUE SPACE.
     03  DT1-01                   PIC  X(20).
     03  FILLER                   PIC  X(03)     VALUE SPACE.
     03  DT1-02                   PIC  X(13).
     03  FILLER                   PIC  X(42)     VALUE SPACE.
     03  DT1-03                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06)     VALUE SPACE.
     03  DT1-04                   PIC  ZZZZZZZZ9.
*
 01  DT2.
     03  FILLER                   PIC  X(07)     VALUE SPACE.
     03  DT2-01                   PIC  X(15).
     03  DT2-02                   PIC  X(04).
     03  DT2-03                   PIC  X(01).
     03  FILLER                   PIC  X(02)     VALUE SPACE.
     03  DT2-04                   PIC  X(01).
     03  DT2-05                   PIC  X(13).
     03  DT2-06                   PIC  X(01).
     03  FILLER                   PIC  X(14)     VALUE SPACE.
     03  DT2-071                  PIC  ZZZZ9.
     03  DT2-072                  PIC  Z.
     03  FILLER                   PIC  X(02)     VALUE SPACE.
     03  DT2-081                  PIC  ZZZZZ9.
     03  DT2-082                  PIC  Z.
     03  FILLER                   PIC  X(04)     VALUE SPACE.
     03  DT2-091                  PIC  ZZZZZ9.
     03  DT2-092                  PIC  ZZ.
     03  DT2-10                   PIC  ZZZZZZZZ9.
     03  DT2-11                   PIC  ZZZZZ9.
     03  DT2-12                   PIC  ZZZZZZZZ9.
*伝票訂正用（ＤＴ・ＴＬ共通）
 01  DT21.
     03  FILLER                   PIC  X(64)     VALUE SPACE.
*    03  FILLER                   PIC  X(06)      VALUE
*                                                "======".
     03  FILLER                   PIC  X(21)     VALUE SPACE.
     03  FILLER                   PIC  X(09)     VALUE
                                                 "=========".
     03  FILLER                   PIC  X(06)     VALUE SPACE.
     03  FILLER                   PIC  X(09)     VALUE
                                                 "=========".
*
*--------------------------------------------------------------*
*    テール行                                                  *
*--------------------------------------------------------------*
*
 01  TL1.
     03  FILLER                   PIC  X(10)     VALUE SPACE.
     03  TL1-01                   PIC  X(06).
     03  FILLER                   PIC  X(22)     VALUE SPACE.
     03  TL1-011                  PIC  X(10).
     03  FILLER                   PIC  X(10)     VALUE SPACE.
     03  TL1-021                  PIC  ZZZZ9.
     03  TL1-022                  PIC  Z.
     03  TL1-023                  PIC  ZZ.
     03  TL1-031                  PIC  ZZZZZ9.
     03  TL1-032                  PIC  Z.
     03  FILLER                   PIC  X(12)     VALUE SPACE.
     03  TL1-04                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06)     VALUE SPACE.
     03  TL1-05                   PIC  ZZZZZZZZ9.
*
 01  TL3.
     03  FILLER                   PIC  X(38)     VALUE SPACE.
     03  TL3-01                   PIC  X(07).
*
*伝票訂正用（合計欄）
 01  TL2.
     03  FILLER                   PIC  X(73)     VALUE SPACE.
     03  TL2-01                   PIC  X(18).
     03  TL2-02                   PIC  X(18).
 LINKAGE                          SECTION.
 01  PARA-KBN                     PIC  X(01).
*
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-KBN.
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
 000-TRNFILE-ERR        SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      BMSDENF.
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
     OPEN     INPUT     BMSDENF.
     OPEN     I-O       DSPFILE.
     OPEN     OUTPUT    PRTFILE.
*対象取引先ＣＤセット
     EVALUATE  PARA-KBN
         WHEN  "1"
          MOVE 321717             TO   WK-TORICD
          DISPLAY NC"＃セキチュー資材ＴＡ伝票発行＃" UPON CONS
         WHEN  "2"
          MOVE 926061             TO   WK-TORICD
          DISPLAY NC"＃セキチュー植物ＴＡ伝票発行＃" UPON CONS
         WHEN  OTHER
              DISPLAY NC"＃取引先エラー！！＃" UPON CONS
              MOVE    "END"      TO   PROGRAM-END
              GO                 TO   INIT-EXIT
     END-EVALUATE.
*    初期値セット
     MOVE     ZERO                TO   DENNO1.
     MOVE     ZERO                TO   CNT-DENPYO.
     MOVE     SPACE               TO   END-FLG.
     MOVE     SPACE               TO   FSY11011.
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
     CLOSE    BMSDENF.
*
     OPEN     INPUT      BMSDENF.
 INIT-EXIT.
     EXIT.
****************************************************************
*             伝票データファイル読込み                2.0      *
****************************************************************
 DEN-CNT-SEC            SECTION.
*
     READ     BMSDENF AT  END
              MOVE     "END"     TO   END-FLG
              GO                 TO   DEN-CNT-EXIT
     END-READ.
*    セキチュー以外は読み飛ばし（取引先）
     MOVE     DEN-F323(1:6)      TO   WK-TOKCD.
     IF       WK-TOKCD-H  NOT =  WK-TORICD
              GO                 TO   DEN-CNT-SEC
     END-IF.
*    伝票枚数カウント
     MOVE    DEN-F302(1:9)       TO   WK-DENNO.
     IF      WK-DENNO-H  NOT =  WK-DEN-NO
              ADD    1           TO   WK-MAI
              MOVE   WK-DENNO-H  TO   WK-DEN-NO
     END-IF.
*
 DEN-CNT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
     MOVE          "MAIN-SEC"     TO   S-NAME.
*
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*
     MOVE          "MD04R"        TO   DSP-GRP.
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
*******************ＷＫ初期化
                   MOVE      SPACE     TO   PRT-WORK
                   INITIALIZE               PRT-WORK
                   MOVE      ZERO      TO   IX
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     END-FLG   =   "END"
                   CLOSE     BMSDENF  PRTFILE
                   OPEN      INPUT     BMSDENF
                   OPEN      OUTPUT    PRTFILE
                   MOVE      1         TO    ERR-FLG
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
                   MOVE      SPACE     TO   PRT-WORK
                   INITIALIZE               PRT-WORK
                   MOVE      ZERO      TO   IX
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     END-FLG   =   "END"
                   CLOSE     BMSDENF  PRTFILE
                   OPEN      INPUT     BMSDENF
                   OPEN      OUTPUT    PRTFILE
                   MOVE      1         TO    ERR-FLG
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
*伝票番号大小チェック
     IF           (MD02   >  MD03)
                   MOVE      1         TO   ERR-FLG
                   GO                  TO   KEY-IN-01
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
     READ     BMSDENF    AT      END
              MOVE       "END"    TO        END-FLG
              MOVE        1       TO        ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO        FL-READ-EXIT
     END-READ.
*MD04(印字パターン)=全件数印刷時
     IF       MD04    =    2  OR  ZEN-FLG = 1
              GO                  TO        READ-020
     END-IF.
*
     IF       KAI-FLG   =    1    GO  TO    READ-010.
*伝票番号チェック
     MOVE    DEN-F302(1:9)       TO   WK-DENNO.
     IF       WK-DENNO-H  =  MD02
              MOVE      1         TO        KAI-FLG
         ELSE
              GO        TO        FL-READ-SEC.
 READ-010.
     MOVE    DEN-F302(1:9)       TO   WK-DENNO.
     IF       ( SYU-FLG     = 1      )  AND
              ( WK-DENNO-H NOT = DEN-NO )
              MOVE      "END"     TO        END-FLG
              GO        TO        FL-READ-EXIT.
     IF       WK-DENNO-H =    MD03
              MOVE      WK-DENNO-H  TO      DEN-NO
              MOVE      1         TO        SYU-FLG.
 READ-020.
     MOVE    DEN-F302(1:9)       TO   WK-DENNO.
     IF       RD-SW     =    0
              MOVE      WK-DENNO-H TO  DENNO1
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
     MOVE     DEN-F302(1:9)       TO        WK-DENNO.
*****DISPLAY "DENNO = " WK-DENNO-H ":" DENNO1 UPON CONS.
     IF       WK-DENNO-H  NOT =             DENNO1
              PERFORM   HEAD-WT-SEC
              MOVE     SPACE      TO   END-CHK
              PERFORM  VARYING IY FROM 1 BY 1 UNTIL IY > 9
                                              OR END-CHK = "END"
                       PERFORM  MEISAI-WT-SEC
              END-PERFORM
              PERFORM   TAIL-WT-SEC
              MOVE      WK-DENNO-H   TO       DENNO1
              MOVE      SPACE        TO       PRT-WORK
              INITIALIZE                      PRT-WORK
              MOVE      ZERO         TO       IX
     END-IF.
*対象の場合、ワークへ退避
*１行目の時、ヘッダ項目作成
     ADD     1                   TO   IX.
     MOVE    DEN-F402(1:2)       TO   WK-GYO.
     IF   IX = 1
          MOVE "******** (TA) ********" TO  HD-DENMSG
          MOVE DEN-F213                 TO  HD-TORINM
          MOVE DEN-F326                 TO  HD-KAISYANM
          MOVE DEN-F311                 TO  HD-TENPONM
          MOVE DEN-F308                 TO  HD-TENCD
          MOVE DEN-F342                 TO  HD-BUNRUI
          MOVE DEN-F351                 TO  HD-DENKU
          MOVE DEN-F302                 TO  HD-DENNO
          MOVE DEN-F323                 TO  HD-TORICD
          MOVE DEN-F344                 TO  HD-HATDATE
          MOVE DEN-F346                 TO  HD-NOUDATE
     END-IF.
*明細行セット
     MOVE      DEN-F417                 TO  MS-SYONM(IX).
     MOVE      DEN-F419                 TO  MS-KIKAKUNM(IX).
     MOVE      DEN-F413                 TO  MS-JANCD(IX).
     MOVE      DEN-F453                 TO  MS-SURYO(IX).
     MOVE      DEN-F446                 TO  MS-GENKA(IX)
     COMPUTE MS-GENKAKIN(IX) = DEN-F453 *  DEN-F446.
     MOVE      DEN-F448                 TO  MS-BAIKA(IX)
     COMPUTE MS-BAIKAKIN(IX) = DEN-F453 *  DEN-F448
     ADD       MS-SURYO(IX)             TO  HD-SURYOKEI.
     ADD       MS-GENKAKIN(IX)          TO  HD-GENKAKEI.
     ADD       MS-BAIKAKIN(IX)          TO  HD-BAIKAKEI.
     IF        DEN-F604  NOT =  DEN-F453
               MOVE  "1"                TO  HD-TEISEI
               MOVE  "1"                TO  MS-TEISEI(IX)
               ADD   WK-GYO-H           TO  HD-HASYU
*      DISPLAY "DEN-F302 = " DEN-F302 UPON CONS
*      DISPLAY "DEN-F402 = " DEN-F402 UPON CONS
*      DISPLAY "HD-HASYU = " HD-HASYU UPON CONS
               MOVE  DEN-F604           TO  MS-TSURYO(IX)
               COMPUTE MS-TGENKAKIN(IX) = DEN-F604 * DEN-F446
               COMPUTE MS-TBAIKAKIN(IX) = DEN-F604 * DEN-F448
               ADD   DEN-F604           TO  HD-TSURYOKEI
               ADD   MS-TGENKAKIN(IX)   TO  HD-TGENKAKEI
               ADD   MS-TBAIKAKIN(IX)   TO  HD-TBAIKAKEI
     ELSE
               ADD   MS-GENKAKIN(IX)    TO  HD-TGENKAKEI
               ADD   MS-BAIKAKIN(IX)    TO  HD-TBAIKAKEI
     END-IF.
*
     PERFORM  FL-READ-SEC.
     IF       END-FLG   =         "END"
              PERFORM   HEAD-WT-SEC
              MOVE     SPACE      TO   END-CHK
              PERFORM  VARYING IY FROM 1 BY 1 UNTIL IY > 9
                                              OR END-CHK = "END"
                       PERFORM  MEISAI-WT-SEC
              END-PERFORM
              PERFORM   TAIL-WT-SEC
              MOVE      SPACE        TO       PRT-WORK
              INITIALIZE                      PRT-WORK
              MOVE      ZERO         TO       IX
     END-IF.
 DENP-WT-EXIT.
     EXIT.
****************************************************************
*             明細部出力処理　                        2.3.1    *
****************************************************************
 MEISAI-WT-SEC          SECTION.
     MOVE     "MEISAI-WT-SEC"     TO        S-NAME.
*
*****DISPLAY "IY = " IY.
     IF  MS-SYONM(IY)  =  SPACE
         MOVE  "END"              TO        END-CHK
         GO                       TO        MEISAI-WT-EXIT
     END-IF.
*項目初期化
     MOVE      SPACE              TO        DT1  DT2.
*明細１行目
***  商品名１
     MOVE     MS-SYONM(IY)        TO        DT1-01.
***  訂正伝票時処理
     IF       MS-TEISEI(IY)  =  "1"
***           訂正後原価金額
              MOVE MS-TGENKAKIN(IY)   TO    DT1-03
***           訂正後売価金額
              MOVE MS-TBAIKAKIN(IY)   TO    DT1-04
     END-IF.
*明細２行目
***  商品名２
     MOVE     MS-KIKAKUNM(IY)     TO        DT2-01.
***  ＪＡＮＣＤ
     MOVE     MS-JANCD(IY)        TO        DT2-05.
***  数量
     MOVE     MS-SURYO(IY)        TO        WK-SURYO.
***  数量（整数部）
     MOVE     WK-SURYO1           TO        DT2-071.
***  数量（端数部）
     IF       WK-SURYO2 =         ZERO
              MOVE     ZERO       TO        DT2-072
     ELSE
              MOVE     WK-SURYO2  TO        DT2-072
     END-IF.
*    訂正後数量
     IF       MS-TEISEI(IY)  =  "1"
              MOVE     MS-TSURYO(IY)        TO        WK-SURYO
***           訂正後数量（整数部）
              MOVE     WK-SURYO1            TO        DT2-081
***           訂正後数量（端数部）
              IF       WK-SURYO2  =         ZERO
                       MOVE       ZERO      TO        DT2-082
              ELSE
                       MOVE     WK-SURYO2   TO        DT2-082
              END-IF
     END-IF.
***  原単価
     MOVE     MS-GENKA(IY)        TO        WK-GENKA.
***  原単価（整数部）
     MOVE     WK-GENKA1           TO        DT2-091.
***  原単価（端数部）
     IF       WK-GENKA2   =       ZERO
              MOVE     ZERO       TO        DT2-092
     ELSE
              MOVE WK-GENKA2      TO        DT2-092
     END-IF.
***  原価金額
     MOVE     MS-GENKAKIN(IY)     TO        DT2-10.
***  売価単価
     MOVE     MS-BAIKA(IY)        TO        DT2-11.
***  売価金額
     MOVE     MS-BAIKAKIN(IY)     TO        DT2-12.
*
*備考（Ｆ欄）退避
     MOVE     SPACE               TO        WK-BIKOU3.
*特売開始日
     MOVE     SPACE               TO        WK-TOKUBAI.
*伝票区分
     MOVE     DEN-F351            TO        WK-DENKU.
*
*明細１行目ＷＲＩＴＥ
     WRITE    PRT-REC   FROM      DT1       AFTER     1.
*明細２行目ＷＲＩＴＥ
     WRITE    PRT-REC   FROM      DT2       AFTER     1.
*訂正伝票時処理（明細２行目二重線）
     IF       MS-TEISEI(IY)  =  "1"
              WRITE     PRT-REC   FROM   DT21  AFTER     0
     END-IF.
*
     ADD      2                   TO        L-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部出力処理                        2.3.1    *
****************************************************************
 HEAD-WT-SEC            SECTION.
     MOVE     "HEAD-WT"           TO        S-NAME.
     MOVE     SPACE               TO        HD1 HD2 HD3.
*
*ヘッダ０行目
***  Ｄ欄
     MOVE     HD-DENMSG            TO       HD0-01.
*ヘッダ２行目
***  社名
     MOVE     HD-TORINM           TO        HD2-01.
***  取引先名１
     MOVE     HD-KAISYANM         TO        HD2-02.
*ヘッダ３行目
***  発注店名１
     MOVE     HD-TENPONM          TO        HD3-01.
***  店コード
     MOVE     HD-TENCD            TO        HD3-02.
***  分類コード
     MOVE     HD-BUNRUI           TO        HD3-03.
***  伝票区分
     MOVE     HD-DENKU            TO        HD3-04.
***  伝票番号
     MOVE     HD-DENNO            TO        HD3-05.
***  取引先コード
     MOVE     HD-TORICD           TO        HD3-06.
***  取引先名２
     MOVE     SPACE               TO        HD3-07.
***  発注日（年）
     MOVE     HD-HATDATE(3:2)     TO        HD3-081.
***  発注日（月）
     MOVE     HD-HATDATE(5:2)     TO        HD3-082.
***  発注日（日）
     MOVE     HD-HATDATE(7:2)     TO        HD3-083.
***  納品日（年）
     MOVE     HD-NOUDATE(3:2)     TO        HD3-091.
***  納品日（月）
     MOVE     HD-HATDATE(5:2)     TO        HD3-092.
***  納品日（日）
     MOVE     HD-HATDATE(5:2)     TO        HD3-093.
*
*訂正フラグチェック(TL-WRITE-SECで使用）
     IF       HD-TEISEI  =  "1"
              MOVE      NC"○"    TO        HD0-02
              MOVE      NC"　"    TO        HD0-03
     ELSE
              MOVE      NC"　"    TO        HD0-02
              MOVE      NC"○"    TO        HD0-03
     END-IF.
*
*ヘッダ行出力
     WRITE    PRT-REC   FROM      HD0       AFTER  3.
     WRITE    PRT-REC   FROM      HD1       AFTER  1.
     WRITE    PRT-REC   FROM      HD2       AFTER  1.
     WRITE    PRT-REC   FROM      HD3       AFTER  1.
*
     MOVE     SPACE               TO        PRT-REC.
     WRITE    PRT-REC             AFTER  1.
*
     MOVE     7                   TO        L-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             テール部出力処理                        2.3.2    *
****************************************************************
 TAIL-WT-SEC            SECTION.
     MOVE     "TAIL-WT"           TO        S-NAME.
     MOVE   SPACE                 TO        TL1  TL2  TL3.
* テール１行目
***  ハッシュトータル
     MOVE     HD-HASYU            TO        TL1-023.
***  数量合計
     MOVE     HD-SURYOKEI         TO        WK-SURYO.
***  数量合計（整数部）
     MOVE     WK-SURYO1           TO        TL1-021.
***  数量合計（端数部）
     IF       WK-SURYO2   =       ZERO
              MOVE     ZERO       TO        TL1-022
     ELSE
              MOVE     WK-SURYO2  TO        TL1-022
     END-IF.
***  伝票区分印字
     EVALUATE  HD-DENKU
         WHEN  "01"  MOVE "ﾃｲﾊﾞﾝ     "  TO  TL1-011
         WHEN  "03"  MOVE "ﾄｸﾊﾞｲ     "  TO  TL1-011
         WHEN  OTHER MOVE "          "  TO  TL1-011
     END-EVALUATE.
*    訂正後数量合計
     IF  HD-TEISEI  =  "1"
              MOVE      HD-TSURYOKEI   TO   WK-SURYO
***           訂正後数量合計（整数部）
              MOVE     WK-SURYO1            TO        TL1-031
***           訂正後数量合計（端数部）
              IF       WK-SURYO2  =         ZERO
                       MOVE       ZERO      TO        TL1-032
              ELSE
                       MOVE     WK-SURYO2   TO        TL1-032
              END-IF
     END-IF.
*    原価金額合計
     MOVE   HD-GENKAKEI          TO         TL1-04.
*    売価金額合計
     MOVE   HD-BAIKAKEI          TO         TL1-05.
*テール２行目
***  訂正後合計欄が１マス２バイトである為の処理
***  訂正後原価金額合計
     IF  HD-TEISEI  =  "1"
              MOVE      ZERO      TO        ZERO-FLG
              MOVE      SPACE     TO        TL2-01
**************DISPLAY "HD-TGENKAKEI = " HD-TGENKAKEI UPON CONS
              IF        HD-TGENKAKEI  =  ZERO
                MOVE    ZERO      TO        TL2-01(17:1)
              ELSE
****************DISPLAY "BBB" UPON CONS
                PERFORM  VARYING  I FROM  1  BY  1  UNTIL I > 9
                   COMPUTE  J  =  I  *  2
                   IF  HD-TGENKAKEI(I:1)  NOT = ZERO
                       MOVE   1                TO    ZERO-FLG
                       MOVE HD-TGENKAKEI(I:1)  TO    TL2-01(J:1)
                   ELSE
                       IF      ZERO-FLG   =    1
                               MOVE     ZERO   TO    TL2-01(J:1)
                       END-IF
                   END-IF
***********DISPLAY "CCC = " I ":" J ":" TL2-01(J:1) UPON CONS
                END-PERFORM
              END-IF
***  訂正後売価金額合計
              MOVE      ZERO      TO        ZERO-FLG
              MOVE      SPACE     TO        TL2-02
              IF        HD-TBAIKAKEI  =  ZERO
              MOVE      ZERO      TO        TL2-02(17:1)
              ELSE
                PERFORM  VARYING  I FROM  1  BY  1  UNTIL I > 9
                   COMPUTE  J  =  I  *  2
                   IF  HD-TBAIKAKEI(I:1)     NOT = ZERO
                       MOVE   1                 TO   ZERO-FLG
                       MOVE HD-TBAIKAKEI(I:1)   TO   TL2-02(J:1)
                   ELSE
                       IF      ZERO-FLG   =    1
                               MOVE     ZERO   TO    TL2-02(J:1)
                       END-IF
                   END-IF
                END-PERFORM
              END-IF
     END-IF.
*テール行印字位置判定
     COMPUTE   CNT-AFTER   =     27  -  L-CNT.
*テール行印字
***  テール１行目
*****DISPLAY "TL1 = " TL1  UPON CONS.
     WRITE    PRT-REC          FROM   TL1     AFTER   CNT-AFTER.
***  伝票訂正時処理　
***  （訂正時はＬＩＮＥ＝３０行の為改頁は行わない）
*****IF       TS-FLG =  1
     IF       HD-TEISEI  =  "1"
***           二重線行
              WRITE     PRT-REC    FROM       DT21    AFTER  0
              WRITE     PRT-REC    FROM       TL3     AFTER  1
***           テール２行目
*******DISPLAY "TL2 = " TL2 UPON CONS
              WRITE     PRT-REC    FROM       TL2     AFTER  2
******DISPLAY "AAA" UPON CONS
     ELSE
              WRITE     PRT-REC    FROM       TL3     AFTER  1
***           改頁
              MOVE      SPACE      TO         PRT-REC
              WRITE     PRT-REC    AFTER      PAGE
     END-IF.
*
     MOVE     SPACE                TO         PRT-WORK.
     INITIALIZE                               PRT-WORK.
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
     MOVE     SPACE               TO   HD0 HD1 HD2 HD3
                                       DT1 DT2
                                       TL1 TL2 TL3.
*    テスト印字項目セット
     MOVE   ALL NC"＊"            TO   HD0-02  HD0-03.
     MOVE   ALL "9"               TO   HD3-081 HD3-082 HD3-083
                                       HD3-091 HD3-092 HD3-093
                                       DT2-071 DT2-091 DT2-11
                                       DT2-092 DT2-10  DT2-12
                                       DT2-072 DT2-092
                                       TL1-021 TL1-04  TL1-05
                                       TL1-022 TL1-023.
     MOVE   ALL "*"               TO   HD1-01  HD1-02  HD1-03
                                       HD1-04  HD2-01  HD2-02
                                       HD3-01  HD3-02  HD3-03
                                       HD3-05  HD3-06  HD3-07
                                       HD3-04  HD0-01  HD3-01
                                       DT1-01  DT1-02  DT2-01
                                       DT2-02  DT2-03  DT2-04
                                       DT2-05  DT2-06  TL3-01
                                       TL2-01  TL2-02.
*ヘッダ部
     WRITE    PRT-REC            FROM  HD0     AFTER  3.
     WRITE    PRT-REC            FROM  HD1     AFTER  1.
     WRITE    PRT-REC            FROM  HD2     AFTER  1.
     WRITE    PRT-REC            FROM  HD3     AFTER  1.
*    明細行印字の為、１行印字ポインタを送る。
     MOVE     SPACE              TO      PRT-REC.
     WRITE    PRT-REC            AFTER   1.
*ボディ部
     PERFORM   VARYING   L-CNT      FROM  1  BY  1
                                        UNTIL   L-CNT     >  9
              WRITE  PRT-REC   FROM   DT1      AFTER     1
              WRITE  PRT-REC   FROM   DT2      AFTER     1
     END-PERFORM.
*テール部
     WRITE    PRT-REC          FROM   TL1      AFTER     2.
     WRITE    PRT-REC          FROM   TL3      AFTER     1.
     WRITE    PRT-REC          FROM   TL2      AFTER     2.
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
     MOVE     "SSY1504L"          TO        PGID.
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
     MOVE     "セキチュー殿向け"  TO        TORINM.
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
     CLOSE    BMSDENF   PRTFILE    DSPFILE.
*
 END-EXIT.
     EXIT.

```
