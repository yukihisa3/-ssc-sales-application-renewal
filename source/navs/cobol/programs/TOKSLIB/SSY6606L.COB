****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ホーマックオンライン　　　        *
*    モジュール名　　　　：　ＴＣ納品（納品シール発行）        *
*    作成日／更新日　　　：　00/08/24                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    作成日／更新日　　　：　00/12/06                          *
*    作成者／更新者　　　：　ＮＡＶ金子                        *
*    作成日／更新日　　　：　00/12/07                          *
*    作成者／更新者　　　：　ＮＡＶ金子                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    伝票フォーマット　　：　専用用紙                          *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY6606L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/08/24.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YA             IS        YA
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
                        STATUS              DSP-ST.
*プリント定義ファイル　　
     SELECT   PRTFILE   ASSIGN    TO        LP-04-PRTF
                        STATUS              PRT-ST.
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
     COPY    FSY49021   OF   XMDLIB.
*
****************************************************************
*    FILE = ﾌﾟﾘﾝﾄ ﾌｱｲﾙ                                         *
****************************************************************
 FD  PRTFILE
                        LABEL RECORD   IS   OMITTED
                        LINAGE IS      15   LINES
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
     03  PAGE-CNT                 PIC  9(04)     VALUE  ZERO.
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
 01  WK-TOKCD                     PIC  9(08)   VALUE  883.
*01  WK-TOKCD                     PIC  9(08)   VALUE  999999.
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  GYO0.
     03  FILLER    OCCURS    2.
       05  FILLER                 PIC  X(40)     VALUE   SPACE.
       05  PAGE-NO                PIC  ZZZZ.
       05  FILLER                 PIC  X(05)     VALUE   SPACE.
 01  GYO1                         CHARACTER  TYPE  IS  YB.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HACDY1                   PIC  99.
     03  FILLER                   PIC  X(01)     VALUE   "･".
     03  HACDM1                   PIC  99.
     03  FILLER                   PIC  X(01)     VALUE   "･".
     03  HACDD1                   PIC  99.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  DEN1                     PIC  9999999.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  LINE1                    PIC  99.
     03  NOUDY1                   PIC  99.
     03  FILLER                   PIC  X(01)     VALUE   "･".
     03  NOUDM1                   PIC  99.
     03  FILLER                   PIC  X(01)     VALUE   "･".
     03  NOUDD1                   PIC  99.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  NOUNM11                  PIC  X(11).
     03  NOUNM12                  PIC  9999.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  HACDY2                   PIC  99.
     03  FILLER                   PIC  X(01)     VALUE   "･".
     03  HACDM2                   PIC  99.
     03  FILLER                   PIC  X(01)     VALUE   "･".
     03  HACDD2                   PIC  99.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  NOUDY2                   PIC  99.
     03  FILLER                   PIC  X(01)     VALUE   "･".
     03  NOUDM2                   PIC  99.
     03  FILLER                   PIC  X(01)     VALUE   "･".
     03  NOUDD2                   PIC  99.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HATYUK                   PIC  N(06).
     03  NOUNM21                  PIC  X(11)     VALUE   SPACE.
     03  NOUNM22                  PIC  9999.
*
 01  GYO2.
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  TOKCD1                   PIC  999999.
     03  TOKNM1                   PIC  X(33).
     03  FILLER                   PIC  X(11)     VALUE   SPACE.
     03  TOKCD2                   PIC  999999.
     03  TOKNM2                   PIC  X(33).
*
 01  GYO3.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  SYONM1-1                 PIC  X(20).
     03  FILLER                   PIC  X(30)     VALUE   SPACE.
     03  SYONM2-1                 PIC  X(20).
*
 01  GYO4.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  SYONM1-2                 PIC  X(20).
     03  FILLER                   PIC  X(30)     VALUE   SPACE.
     03  SYONM2-2                 PIC  X(20).
*
 01  GYO5.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  JAN1                     PIC  ZZZZZ99999999.
     03  HATYU1-1                 PIC  ZZZZZZZZ9.
     03  HATYU1-2                 PIC  Z.
     03  GENKA1                   PIC  ZZZZZZZ9.
     03  GENKA2                   PIC  ZZ.
     03  KIN                      PIC  ZZZZZZZZZZ9.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  DEN2                     PIC  9999999.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  LINE2                    PIC  99.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  JAN2                     PIC  ZZZZZ99999999.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  TEISEI                   PIC  ZZZ,ZZ9.
     03  HATYU2-1                 PIC  ZZZZZZZZ9.
     03  HATYU2-2                 PIC  Z.
*
 01  GYO5-TEISEI.
     03  FILLER                   PIC  X(84)     VALUE   SPACE.
     03  TEISEI-LINE              PIC  X(10)     VALUE   SPACE.
*
 01  GYO6                         CHARACTER  TYPE IS YA.
     03  FILLER                   PIC  X(11)     VALUE   SPACE.
     03  TEK-TEKNM1               PIC  N(02)     VALUE   SPACE.
     03  TEK-TEKNM2               PIC  X(01)     VALUE   SPACE.
     03  TEK-TEISEI               PIC  ZZZ,ZZ9.
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  TEK-TANNM1               PIC  N(02)     VALUE   SPACE.
     03  TEK-TANNM2               PIC  X(01)     VALUE   SPACE.
     03  TEK-TANABAN              PIC  X(06).
     03  FILLER                   PIC  X(15)     VALUE   SPACE.
     03  BUMON                    PIC  999.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  BAIKA                    PIC  ZZZZZZ9.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  TEK-TANNM11              PIC  N(02)     VALUE   SPACE.
     03  TEK-TANNM22              PIC  X(01)     VALUE   SPACE.
     03  TEK-TANABAN2             PIC  X(06).
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
     MOVE     SPACE               TO   FSY49021.
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
*****IF       DEN-A15  NOT =  1
*             GO                 TO   DEN-CNT-SEC
*****END-IF.
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
     MOVE          ZERO                TO   PAGE-CNT.
     MOVE          SPACE               TO   FSY49021.
     PERFORM       DSP-WRITE-SEC.
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
                   CLOSE     HMSHIRED  PRTFILE
                   OPEN      INPUT     HMSHIRED
                   OPEN      OUTPUT    PRTFILE
                   MOVE      1         TO   ERR-FLG
         WHEN      3
         WHEN      4
                   MOVE      SPACE     TO   END-FLG
                   IF   MD04 =    3
                        PERFORM   KEY-IN-SEC
                   ELSE
                        PERFORM   KEY-IN-SEC-2
                        MOVE 1    TO   ZEN-FLG
                   END-IF
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
*             範囲指定処理２                          2.1      *
****************************************************************
 KEY-IN-SEC-2           SECTION.
     MOVE          "KEI-IN-2"     TO   S-NAME.
*
 KEY-IN-01-2.
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*
     MOVE         "NE"            TO   DSP-PRO.
     MOVE         "KEY02"         TO   DSP-GRP.
     MOVE          ZERO           TO   PGST      PGED.
*
     PERFORM       DSP-READ-SEC.
*
     EVALUATE      DSP-FNC
         WHEN     "F005"
                   MOVE     "END"      TO   END-FLG
                   GO                  TO   KEY-IN-EXIT-2
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
                   GO                  TO   KEY-IN-01-2
     END-EVALUATE.
*
     IF           (PGST   =  ZERO)     AND
                  (PGED   =  ZERO)
                   MOVE      ZERO      TO   PGST
                   MOVE      ALL "9"   TO   PGED
                   MOVE      1         TO   ZEN-FLG
     END-IF.
*
     IF           (PGED   =  ZERO)
                   MOVE      ALL "9"   TO   PGED
     END-IF.
*
 KEY-IN-EXIT-2.
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
*****IF       DEN-A15  NOT =  1
*             GO                 TO   FL-READ-SEC
*****END-IF.
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
***  ５、６行目初期化
     MOVE     SPACE               TO   GYO5.
     MOVE     SPACE               TO   GYO6.
     MOVE     SPACE               TO   GYO5-TEISEI.
***  連番カウントアップ
     ADD      1              TO   PAGE-CNT.
***  ページ指定判定
     IF  MD04 =    4
         EVALUATE  TRUE
              WHEN PGST <=   PAGE-CNT  AND  PGED >=   PAGE-CNT
                   CONTINUE
              WHEN PGST >    PAGE-CNT
                   GO   TO   DENP-WT-SEC-READ
              WHEN PGED <    PAGE-CNT
                   MOVE      "END"    TO        END-FLG
                   GO   TO   DENP-WT-EXIT
         END-EVALUATE
     END-IF.
***  ページ番号セット
     MOVE     PAGE-CNT       TO   PAGE-NO(1).
***  ６行目漢字・テキスト項目セット
     MOVE     NC"訂正"            TO   TEK-TEKNM1.
     MOVE     NC"_番"            TO   TEK-TANNM1.
     MOVE     ":"                 TO   TEK-TEKNM2 TEK-TANNM2.
***  _番ｾｯﾄ 2008/12/12
     MOVE     NC"_番"            TO   TEK-TANNM11.
     MOVE     ":"                 TO   TEK-TANNM22.
***  発注日
     MOVE     DEN-A04(1:2)        TO   HACDY1 HACDY2.
     MOVE     DEN-A04(3:2)        TO   HACDM1 HACDM2.
     MOVE     DEN-A04(5:2)        TO   HACDD1 HACDD2.
***  伝票番号
     MOVE     DEN-A03             TO   DEN1   DEN2.
***  行番号
     MOVE     DEN-A19             TO   LINE1  LINE2.
***  納品日
     MOVE     DEN-A05(1:2)        TO   NOUDY1 NOUDY2.
     MOVE     DEN-A05(3:2)        TO   NOUDM1 NOUDM2.
     MOVE     DEN-A05(5:2)        TO   NOUDD1 NOUDD2.
***  納品先
     MOVE     DEN-A09             TO   NOUNM11 NOUNM21.
***  店舗ＣＤ
     MOVE     DEN-A081            TO   NOUNM12 NOUNM22.
***  取引先ＣＤ
     MOVE     DEN-A061            TO   TOKCD1 TOKCD2.
***  取引先名
     MOVE     DEN-A07             TO   TOKNM1 TOKNM2.
***  商品名１
     MOVE     DEN-A211            TO   SYONM1-1 SYONM2-1.
***  商品名２
     MOVE     DEN-A212            TO   SYONM1-2 SYONM2-2.
***  商品ＣＤ
     MOVE     DEN-A27             TO   WK-SYOCD.
     MOVE     WK-HEN-SYOCD        TO   JAN1   JAN2.
***  発注数量
     MOVE     DEN-A22             TO   WK-SURYO.
     MOVE     WK-SURYO1           TO   HATYU1-1 HATYU2-1.
     MOVE     WK-SURYO2           TO   HATYU1-2 HATYU2-2.
***  単価（原価単価）
     MOVE     DEN-A23             TO   WK-GENKA.
     MOVE     WK-GENKA1           TO   GENKA1.
     MOVE     WK-GENKA2           TO   GENKA2.
***  金額
     MOVE     DEN-A24             TO   KIN.
***  仕入単位
***  発注区分
     EVALUATE DEN-A14
         WHEN  0
         MOVE  NC"　定　期　　"   TO   HATYUK
         WHEN  1
         MOVE  NC"販促（特売）"   TO   HATYUK
         WHEN  2
         MOVE  NC"　新店改装　"   TO   HATYUK
         WHEN  3
         MOVE  NC"　投　入　　"   TO   HATYUK
         WHEN  4
         MOVE  NC"ダイレクト　"   TO   HATYUK
         WHEN  5
         MOVE  NC"客注ダイレク"   TO   HATYUK
         WHEN  OTHER
         MOVE  NC"＊＊＊＊＊＊"   TO   HATYUK
     END-EVALUATE.
***  訂正数
     IF       DEN-F15  NOT =  DEN-F50
              MOVE     DEN-F15    TO   TEISEI  TEK-TEISEI
              MOVE     ALL "="    TO   TEISEI-LINE
**** ELSE
****          MOVE     ZERO       TO   TEISEI  TEK-TEISEI
     END-IF.
***  部門
     MOVE     DEN-A101            TO   BUMON.
***  単価（売価単価）
     MOVE     DEN-A25             TO   BAIKA.
***  _番
     MOVE     DEN-F49             TO   TEK-TANABAN.
***  _番追加 2008/12/12
     MOVE     DEN-F49             TO   TEK-TANABAN2.
*連番印刷
*****WRITE    PRT-REC   FROM      GYO0      AFTER     3.
     WRITE    PRT-REC   FROM      GYO0      AFTER     1.
*明細１行目出力　　　
*****WRITE    PRT-REC   FROM      GYO1      AFTER     5.
     WRITE    PRT-REC   FROM      GYO1      AFTER     2.
*明細２行目出力
     WRITE    PRT-REC   FROM      GYO2      AFTER     2.
*明細３行目出力
     WRITE    PRT-REC   FROM      GYO3      AFTER     2.
*明細４行目出力
     WRITE    PRT-REC   FROM      GYO4      AFTER     1.
*明細５行目出力
     WRITE    PRT-REC   FROM      GYO5      AFTER     2.
     WRITE    PRT-REC   FROM      GYO5-TEISEI AFTER   0.
*明細６行目出力
     WRITE    PRT-REC   FROM      GYO6      AFTER     2.
*    改ページ
     MOVE     SPACE     TO        PRT-REC.
     WRITE    PRT-REC   AFTER     PAGE.
*
 DENP-WT-SEC-READ.
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
     MOVE   NC"Ｎ"               TO   HATYUK.
     MOVE   ALL "9"              TO   DEN1 DEN2 LINE1 LINE2 TOKCD1
                                      TOKCD2 JAN1 JAN2 HATYU1-1
                                      HATYU1-2 HATYU2-1 HATYU2-2
                                      GENKA1 GENKA2 KIN TEISEI
                                      BUMON HACDY1 HACDM1 HACDD1
                                      HACDY2 HACDM2 HACDD2 NOUDY1
                                      NOUDM1 NOUDD1 NOUDY2 NOUDM2
                                      NOUDD2 BUMON BAIKA
                                      NOUNM12 NOUNM22.
     MOVE   ALL "*"              TO   TOKNM1 TOKNM2 SYONM1-1
                                      SYONM1-2 SYONM2-1 SYONM2-2
                                      NOUNM11 NOUNM21.
*明細１行目出力　　　
*****WRITE    PRT-REC   FROM      GYO1      AFTER     5.
     WRITE    PRT-REC   FROM      GYO1      AFTER     3.
*明細２行目出力
     WRITE    PRT-REC   FROM      GYO2      AFTER     2.
*明細３行目出力
     WRITE    PRT-REC   FROM      GYO3      AFTER     2.
*明細４行目出力
     WRITE    PRT-REC   FROM      GYO4      AFTER     1.
*明細５行目出力
     WRITE    PRT-REC   FROM      GYO5      AFTER     2.
*明細６行目出力
     WRITE    PRT-REC   FROM      GYO6      AFTER     2.
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
     MOVE     "FSY49021"          TO        DSP-FMT.
*項目設定
***  伝票枚数
     MOVE     WK-MAI              TO        MD01.
***  プログラムＩＤ
     MOVE     "SSY6606L"          TO        PGID.
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
     MOVE     "ホーマック北海道"  TO        TORINM.
*画面出力
     WRITE    FSY49021.
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
