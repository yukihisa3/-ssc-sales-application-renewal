****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ドイトオンライン　　　　          *
*    モジュール名　　　　：　ドイトオンライン仕入伝票発行　　  *
*    作成日／更新日　　　：　13/03/25                          *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    伝票フォーマット　　：　専用（レーザー）ＦＯＲＭ未使用　　*
*    （Ａ４一括出力－訂正有）                                  *
*    2013/03/25 ドンキホーテ合併（発注レイアウト変更）         *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY1411L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          13/03/25.
******************************************************************
 ENVIRONMENT            DIVISION.
******************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         YA        IS   YA
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*伝票データ
     SELECT   DHSHIRED  ASSIGN    TO        DHSHIRED
                        ACCESS    MODE IS   SEQUENTIAL
                        STATUS              DEN-ST.
*店舗マスタ
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE      STATUS    TEN-ST.
*プリンター
     SELECT   PRTF      ASSIGN    TO        LP-04
                        STATUS              PRT-ST.
*画面ファイル
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
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
 FD  DHSHIRED
                 BLOCK    CONTAINS  1    RECORDS
                 LABEL    RECORD    IS   STANDARD.
                 COPY     DHSHIRED  OF   XFDLIB
                 JOINING  DEN       AS   PREFIX.
*店舗マスタ
 FD  HTENMS      BLOCK    CONTAINS  8    RECORDS
                 LABEL    RECORD    IS   STANDARD.
                 COPY     HTENMS    OF   XFDLIB
                 JOINING  TEN       AS   PREFIX.
*プリンター
 FD  PRTF
                        LABEL RECORD   IS   OMITTED
                        LINAGE IS      80   LINES
                        DATA  RECORD   IS   PRT-REC.
 01  PRT-REC.
     03  FILLER                   PIC  X(200).
*表示ファイル
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FSY11011  OF        XMDLIB.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*** ｽﾃｰﾀｽ
 01  ST-AREA.
     03  DEN-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
     03  PRT-ST                   PIC  X(02).
     03  IN-DATA                  PIC  X(01).
*%*表示パラメータ*%*
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
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)     VALUE   SPACE.
     03  ZEN-FLG                  PIC  9(01)     VALUE   ZERO.
     03  TAI-FLG                  PIC  9(01)     VALUE   ZERO.
     03  SYU-FLG                  PIC  9(01)     VALUE   ZERO.
     03  TAITL-FLG                PIC  9(01)     VALUE   ZERO.
     03  INJ-FLG                  PIC  X(03)     VALUE   SPACE.
     03  CNT-FLG                  PIC  X(03)     VALUE   SPACE.
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  MAI-CNT                  PIC  9(07)     VALUE   ZERO.
     03  IX                       PIC  9(01)     VALUE   ZERO.
     03  PAGE-CNT                 PIC  9(07)     VALUE   ZERO.
***  ｺﾞｳｹｲ ｴﾘｱ
 01  GK-AREA.
     03  GK-GENKA-HTY             PIC  9(09)     VALUE   ZERO.
     03  GK-GENKA-TEI             PIC  9(09)     VALUE   ZERO.
     03  GK-BAIKA-HTY             PIC  9(09)     VALUE   ZERO.
     03  GK-BAIKA-TEI             PIC  9(09)     VALUE   ZERO.
***  納品日編集１
 01  WK-NOU-DATE.
     03  WK-NOU-DT1               PIC  9(04).
     03  WK-NOU-DT2               PIC  9(02).
     03  WK-NOU-DT3               PIC  9(02).
 01  WK-NOU-DATE1                 PIC  9(08)  VALUE  ZERO.
***  納品日編集２
 01  WK-HEN-DATE.
     03  WK-HEN-DT1               PIC  9(04).
     03  WK-HEN-DT2               PIC  X(01).
     03  WK-HEN-DT3               PIC  9(02).
     03  WK-HEN-DT4               PIC  X(01).
     03  WK-HEN-DT5               PIC  9(02).
***  ＪＡＮＣＤ編集１
 01  WK-JANCD.
     03  WK-JANCD-00              PIC  X(01).
     03  WK-JANCD-01              PIC  9(02).
     03  WK-JANCD-02              PIC  9(05).
     03  WK-JANCD-03              PIC  9(07).
     03  WK-JANCD-99              PIC  X(01).
***  ＪＡＮＣＤ編集２
 01  WK-HEN-JANCD.
     03  WK-HEN-JANCD1            PIC  9(02).
     03  WK-HEN-JANCD2            PIC  X(01).
     03  WK-HEN-JANCD3            PIC  9(05).
     03  WK-HEN-JANCD4            PIC  X(01).
     03  WK-HEN-JANCD5            PIC  9(07).
 01  WK-KIKAKU                    PIC  X(03).
 01  WK-KIKAKU-R   REDEFINES      WK-KIKAKU.
     03  WK-KIKAKU-H              PIC  9(03).
*数量
 01  WK-DEN15                     PIC S9(09)V99.
 01  WK-DEN15R     REDEFINES      WK-DEN15.
     03  WK-DEN15A                PIC S9(09).
     03  WK-DEN15B                PIC  9(02).
*原価単価
 01  WK-DEN172                    PIC S9(09)V99.
 01  WK-DEN172R    REDEFINES      WK-DEN172.
     03  WK-DEN172A               PIC S9(09).
     03  WK-DEN172B               PIC  9(02).
*
 01  WK-SURYO                     PIC  9(05)V9.
 01  WK-SURYO-R  REDEFINES  WK-SURYO.
     03  WK-SURYO1                PIC  9(05).
     03  WK-SURYO2                PIC  9(01).
*
 01  WK-GENKA                     PIC  9(09)V99.
 01  WK-GENKA-R  REDEFINES  WK-GENKA.
     03  WK-GENKA1                PIC  9(09).
     03  WK-GENKA2                PIC  9(02).
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  WK-MAI                   PIC  9(06)     VALUE   ZERO.
     03  WRK-R040                 PIC  9(01)     VALUE   ZERO.
     03  RD-SW                    PIC  9(01)     VALUE   ZERO.
     03  I                        PIC  9(01)     VALUE   ZERO.
     03  DENPYO.
         05  FILLER               PIC  X(22)     VALUE
             "ｼｲﾚ ﾃﾞﾝﾋﾟｮｳ ﾊｯｺｳ ﾏｲｽｳ ".
         05  CNT-DENPYO           PIC  9(09)     VALUE   ZERO.
 01  WK-NOUHIN                    PIC  9(06)     VALUE   ZERO.
 01  WK-HNOUHIN.
     03  WK-HNOUHIN1              PIC  Z9.
     03  WK-HNOUHIN1              PIC  Z9.
     03  WK-HNOUHIN3              PIC  Z9.
 01  WK-TORIHIKI                  PIC  9(05)     VALUE   ZERO.
 01  WK-F02                       PIC  9(06)     VALUE   ZERO.
 01  WK-W014A                     PIC  9(02)     VALUE   ZERO.
 01  WK-DENNO                     PIC  9(09)     VALUE   ZERO.
*
*伝票明細退避
 01  WK-MEISAI.
     03  WK-MEISAI-REC            OCCURS   6.
         05  WK-MEISAI-GYO        PIC  X(1020).
*--- ﾃﾞﾝﾋﾟﾖｳ ｷ-
 01  WRK-DENNO.
     03  WK-DEN                   PIC  9(09)     VALUE   ZERO.
*    ﾒﾂｾ-ｼﾞ ｴﾘｱ
 01  MSG-AREA.
     03  MSG01                PIC  N(30)     VALUE
         NC"対象データありません".
     03  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください".
     03  MSG03                PIC  N(30)     VALUE
         NC"『ドイト伝票発行中』".
     03  MSG04                PIC  N(30)     VALUE
         NC"無効キーです".
     03  MSG05                PIC  N(30)     VALUE
         NC"開始が終了より大きいです".
     03  MSG06                PIC  N(30)     VALUE
         NC"Ａ４用紙のテストプリントはありません。".
*
 01  DEN-ERR                      PIC  N(11)     VALUE
         NC"伝票データ　異常！！".
 01  TEN-ERR                      PIC  N(11)     VALUE
         NC"店舗マスタ　異常！！".
 01  DSP-ERR                      PIC  N(11)     VALUE
         NC"画面ファイル　異常！！".
 01  PRT-ERR                      PIC  N(11)     VALUE
         NC"プリンター　異常！！".
*日付変換ワーク
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*画面退避用
     COPY   DHSHIRED OF XFDLIB  JOINING   DOT  AS   PREFIX.
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD01.
     03  FILLER                   PIC  X(08)     VALUE SPACE.
     03  HD1-01                   PIC  9(02).
     03  FILLER                   PIC  X(04)     VALUE SPACE.
     03  HD1-02                   PIC  X(06).
     03  FILLER                   PIC  X(05)     VALUE SPACE.
     03  HD1-03                   PIC  X(20).
     03  FILLER                   PIC  X(25)     VALUE SPACE.
     03  HD1-04                   PIC  9(03).
     03  FILLER                   PIC  X(05)     VALUE SPACE.
     03  HD1-05                   PIC  X(10).
*
 01  HD02.
     03  FILLER                   PIC  X(08)     VALUE SPACE.
     03  HD2-01                   PIC  9(03).
     03  FILLER                   PIC  X(07)     VALUE SPACE.
     03  HD2-02                   PIC  X(04).
*
 01  HD03.
     03  FILLER                   PIC  X(07)     VALUE SPACE.
     03  HD3-01                   PIC  X(16).
     03  FILLER                   PIC  X(14)     VALUE SPACE.
     03  HD3-02                   PIC  9(09).
*
*--------------------------------------------------------------*
*    明細                                                      *
*--------------------------------------------------------------*
*
 01  MS01.
     03  FILLER                   PIC  X(08)     VALUE SPACE.
     03  MS1-01                   PIC  X(20).
     03  FILLER                   PIC  X(18)     VALUE SPACE.
     03  MS1-02                   PIC  ZZ,ZZ9.
     03  MS1-021                  PIC  X(01).
     03  MS1-022                  PIC  Z.
*
 01  MS02.
     03  FILLER                   PIC  X(08)     VALUE SPACE.
     03  MS2-01                   PIC  X(20).
     03  FILLER                   PIC  X(02)     VALUE SPACE.
     03  MS2-02                   PIC  X(13).
     03  FILLER                   PIC  X(03)     VALUE SPACE.
     03  MS2-03                   PIC  ZZ,ZZ9.
     03  MS2-031                  PIC  X(01).
     03  MS2-032                  PIC  Z.
     03  FILLER                   PIC  X(10)     VALUE SPACE.
     03  MS2-04                   PIC  ZZZZZZZZ9.
     03  MS2-041                  PIC  99.
     03  FILLER                   PIC  X(04)     VALUE SPACE.
     03  MS2-05                   PIC  ZZZZZZZZ9.
*
*--------------------------------------------------------------*
*    テイル                                                    *
*--------------------------------------------------------------*
*
 01  TL01.
     03  FILLER                   PIC  X(64)     VALUE SPACE.
     03  TL1-01                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06)     VALUE SPACE.
     03  TL1-02                   PIC  ZZZZZZZZ9.
*
 01  TL02.
     03  FILLER                   PIC  X(64)     VALUE SPACE.
     03  TL2-01                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06)     VALUE SPACE.
     03  TL2-02                   PIC  ZZZZZZZZ9.
*
 01  TL03                         CHARACTER  TYPE  YA.
     03  FILLER                   PIC  X(08)     VALUE SPACE.
     03  TL3-01                   PIC  N(04).
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  TL3-02                   PIC  9999.
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  TL3-03                   PIC  N(01).
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  TL3-04                   PIC  Z9.
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  TL3-05                   PIC  N(01).
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  TL3-06                   PIC  Z9.
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  TL3-07                   PIC  N(01).
*--------------------------------------------------------------*
*    訂正行                                                    *
*--------------------------------------------------------------*
*
 01  TE01.
     03  FILLER                   PIC  X(45)     VALUE SPACE.
     03  TE1-01                   PIC  X(10)
                                  VALUE "==========".
*
 01  TE02.
     03  FILLER                   PIC  X(63)     VALUE SPACE.
     03  TE2-01                   PIC  X(12)
                                  VALUE "============".
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  TE2-01                   PIC  X(12)
                                  VALUE "============".
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
*伝票データ
 DEN-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DHSHIRED.
     DISPLAY  DEN-ERR          UPON    CONS.
     DISPLAY  DEN-ST           UPON    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
*店舗マスタ
 TEN-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HTENMS.
     DISPLAY  TEN-ERR          UPON    CONS.
     DISPLAY  TEN-ST           UPON    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
*プリンター
 PRT-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      PRTF.
     DISPLAY  PRT-ERR          UPON    CONS.
     DISPLAY  PRT-ST           UPON    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
*画面ファイル
 DSP-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DSPF.
     DISPLAY  DSP-ERR          UPON    CONS.
     DISPLAY  DSP-ST           UPON    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
******************************************************************
*            M  A  I  N          M  O  D  U  L  E                *
******************************************************************
 PROC-SEC                  SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-FLG = "END".
     PERFORM     END-SEC.
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
     OPEN        INPUT     DHSHIRED  HTENMS
                 I-O       DSPF.
*
     MOVE     ZERO               TO   WK-DEN WK-MAI WK-DENNO.
     MOVE     ZERO               TO   CNT-DENPYO.
*伝票枚数カウント
     PERFORM  DENPYO-CNT-SEC  UNTIL  CNT-FLG = "END".
*
     IF   WK-MAI  =  ZERO
          DISPLAY NC"＃＃対象データ無＃＃" UPON CONS
          STOP  RUN
     END-IF.
*
     CLOSE    DHSHIRED.
*画面初期化*
     MOVE        SPACE     TO        FSY11011.
 INIT-EXIT.
     EXIT.
**********************************************************
*    伝票枚数カウント
**********************************************************
 DENPYO-CNT-SEC            SECTION.
*伝票総枚数読込み
     READ     DHSHIRED  AT  END
              MOVE      "END"    TO   CNT-FLG
              GO                 TO   DENPYO-CNT-EXIT
     END-READ.
*
*取引先チェック
     IF       DEN-F01  NOT =  10545
              GO                 TO   DENPYO-CNT-SEC
     END-IF.
*伝票ブレイクチェック
     IF       DEN-F02  NOT =  WK-DENNO
              ADD      1         TO   WK-MAI
              MOVE     DEN-F02   TO   WK-DENNO
     END-IF.
*
 DENPYO-CNT-EXIT.
     EXIT.
***********************************************************
*                       画面処理                          *
***********************************************************
 MAIN-SEC                  SECTION.
     PERFORM     DSP-WRT-SEC.
*＃＃処理区分入力＃＃
 MAIN-010.
     MOVE         "MD04 "    TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    DSP-FNC
       WHEN  "F005"
*          『終了』
           MOVE  "END"       TO        END-FLG
           GO                TO        MAIN-EXIT
       WHEN  "E000"
*          『入力／実行』
           CONTINUE
       WHEN   OTHER
*          『別キー押下』
           GO                TO        MAIN-010
     END-EVALUATE.
*『処理区分判定』
*****MOVE        SPACE       TO        MD05.
     PERFORM     DSP-WRT-SEC.
     EVALUATE    MD04
         WHEN      "1"
*                  『テストプリント』
                   MOVE      MSG06     TO   MD05
         WHEN      "2"
*                  『全件出力』
                   OPEN  OUTPUT  PRTF
                   MOVE      1         TO   ZEN-FLG
                   MOVE      SPACE     TO   INJ-FLG
                   MOVE      ZERO      TO   MD02  MAI-CNT
                   MOVE      ALL "9"   TO   MD03
                   OPEN      INPUT DHSHIRED
                   PERFORM   DENPYO-CTL-SEC
                             UNTIL     INJ-FLG   =   "END"
                   CLOSE     DHSHIRED  PRTF
                   MOVE      SPACE     TO   MD05
         WHEN      "3"
*                  『範囲出力』
                   OPEN  OUTPUT  PRTF
                   MOVE      ZERO      TO   ZEN-FLG  MAI-CNT
                   MOVE      SPACE     TO   INJ-FLG
                   INITIALIZE               WK-MEISAI
                   PERFORM   KEY-IN-SEC
                   OPEN      INPUT DHSHIRED
                   PERFORM   DENPYO-CTL-SEC
                             UNTIL     INJ-FLG   =   "END"
                   CLOSE     DHSHIRED  PRTF
                   MOVE      SPACE     TO   MD05
         WHEN      OTHER
                   MOVE      MSG02     TO   MD05
     END-EVALUATE.
**** PERFORM       DSP-WRT-SEC.
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
*
     CLOSE       DSPF  HTENMS.
*
 END-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 DENPYO-CTL-SEC            SECTION.
*伝票読込み
     READ     DHSHIRED   AT        END
*             伝票発行
              PERFORM  DENPYO-SEIGYO-SEC
              MOVE    "END"       TO   INJ-FLG
              GO                  TO   DENPYO-CTL-EXIT
     END-READ.
*全件出力の場合
     IF       ZEN-FLG  =  1
              GO                  TO   DENPYO-020
     END-IF.
*範囲対象区分
 DENPYO-000.
     IF       TAI-FLG  =  1
              GO                  TO   DENPYO-010
     END-IF.
*範囲指定開始チェック
 DENPYO-005.
     IF       DEN-F02  =  MD02
              MOVE     1          TO   TAI-FLG
     ELSE
              IF   MD02 = ZERO
                   MOVE    1      TO   TAI-FLG
              ELSE
                   GO             TO   DENPYO-CTL-SEC
              END-IF
     END-IF.
*範囲指定終了チェック
 DENPYO-010.
     IF       SYU-FLG  =  1
     AND      DEN-F02  NOT =  WK-DENNO
*             最終伝票印字
              PERFORM  DENPYO-SEIGYO-SEC
              MOVE    "END"       TO   INJ-FLG
              GO                  TO   DENPYO-CTL-EXIT
     ELSE
              IF   DEN-F02  =  MD03
                   MOVE   1       TO   SYU-FLG
              END-IF
     END-IF.
 DENPYO-020.
*    伝票番号ﾌﾞﾚｲｸﾁｪｯｸ
     IF       DEN-F02  NOT =  WK-DENNO
              IF   MAI-CNT  >      ZERO
*                  伝票発行
                   PERFORM  DENPYO-SEIGYO-SEC
              END-IF
              ADD      1          TO   MAI-CNT
              MOVE     DEN-F02    TO   WK-DENNO
              INITIALIZE               WK-MEISAI
     END-IF.
*伝票をワークに退避
 DENPYO-030.
     MOVE     DEN-REC             TO   WK-MEISAI-GYO(DEN-F03).
*****DISPLAY "AAAAA" UPON CONS.
*
 DENPYO-CTL-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 DENPYO-SEIGYO-SEC         SECTION.
*伝票発行制御
*****『初期化』
*****『合計集計エリア初期化』
     MOVE     ZERO               TO   GK-AREA.
*****『上段印刷』
*****空白行を送る（１０行空白）
     MOVE      SPACE           TO         PRT-REC.
     WRITE     PRT-REC         AFTER      9.
*****『上段伝票ヘッダ』
     PERFORM  HEAD-WRT1-SEC.
*****『上段伝票明細』
*****DISPLAY "IX = " IX UPON CONS.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 6
             PERFORM MEIS-WRT1-SEC
     END-PERFORM.
*****『上段伝票テイル』
     PERFORM  TAIL-WRT1-SEC.
*****『合計集計エリア初期化』
     MOVE      ZERO        TO        GK-AREA.
*****『下段印刷』
*****空白行を送る（１０行空白）
     MOVE      SPACE           TO         PRT-REC.
     WRITE     PRT-REC         AFTER      18.
*****『下段伝票ヘッダ』
     PERFORM  HEAD-WRT1-SEC.
*****『下段伝票明細』
*****DISPLAY "IX = " IX UPON CONS.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 6
             PERFORM MEIS-WRT1-SEC
     END-PERFORM.
*****『下段伝票テイル』
     PERFORM  TAIL-WRT1-SEC.
*****頁カウンター加算
     ADD     1                 TO         PAGE-CNT.
*改頁を行なう。
     MOVE    SPACE             TO         PRT-REC.
     WRITE   PRT-REC           AFTER      PAGE.
*
 DENPYO-SEIGYO-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 HEAD-WRT1-SEC                 SECTION.
*伝票ワーク⇒ドイトフォーマットへセット
     MOVE       WK-MEISAI-GYO(1)  TO   DOT-REC.
*伝票区分
     MOVE       DOT-B07      TO      HD1-01.
*取引先ＣＤ
     MOVE       DOT-B10      TO      HD1-02.
*取引先名称
     MOVE       DOT-B14      TO      HD1-03.
*オーダー
     MOVE       DOT-B15      TO      HD1-04.
*納品日
*    システム日付８桁変換
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     DOT-B08   TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   WK-NOU-DATE1
     ELSE
         COMPUTE WK-NOU-DATE1 = 20000000 + DOT-B08
     END-IF.
     MOVE       WK-NOU-DATE1 TO      WK-NOU-DATE.
     MOVE       WK-NOU-DT1   TO      WK-HEN-DT1.
     MOVE       "/"          TO      WK-HEN-DT2.
     MOVE       WK-NOU-DT2   TO      WK-HEN-DT3.
     MOVE       "/"          TO      WK-HEN-DT4.
     MOVE       WK-NOU-DT3   TO      WK-HEN-DT5.
     MOVE       WK-HEN-DATE  TO      HD1-05.
*店舗ＣＤ
     MOVE       DOT-B05      TO      HD2-01.
*部門
     MOVE       SPACE        TO      HD2-02.
     MOVE       DOT-B06      TO      HD2-02.
*店舗名
     MOVE       DOT-B13      TO      HD3-01.
*伝票番号
     MOVE       DOT-B03      TO      HD3-02.
*ヘッダ１行目
     WRITE    PRT-REC   FROM      HD01      AFTER     1.
     WRITE    PRT-REC   FROM      HD02      AFTER     3.
     WRITE    PRT-REC   FROM      HD03      AFTER     2.
*****空白行を送る（１０行空白）
     MOVE      SPACE           TO         PRT-REC.
     WRITE    PRT-REC                       AFTER     3.
*
 HEAD-WRT1-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIS-WRT1-SEC                  SECTION.
*伝票データセット
     MOVE  WK-MEISAI-GYO(IX) TO      DOT-REC.
*明細行判定　空行の場合は、２行空白行を送る
     IF  DOT-F02  =  ZERO
         MOVE      SPACE     TO      PRT-REC
         WRITE    PRT-REC            AFTER     2
         GO                  TO      MEIS-WRT1-EXIT
     END-IF.
*明細行初期化
     MOVE        SPACE       TO      MS01  MS02.
*商品名称
     MOVE        DOT-C14     TO      MS1-01(1:8).
     MOVE        DOT-C16     TO      MS1-01(9:8).
     MOVE        DOT-C15     TO      MS2-01.
*商品ＣＤ
     MOVE        DOT-C04     TO      MS2-02.
*数量
     IF          DOT-F15  NOT =  DOT-F50
                 MOVE        DOT-F15     TO      WK-SURYO
                 MOVE        WK-SURYO1   TO      MS1-02
                 IF   WK-SURYO2 NOT = ZERO
                      MOVE  "."          TO      MS1-021
                      MOVE  WK-SURYO2    TO      MS1-022
                 END-IF
     END-IF.
     MOVE        DOT-F50     TO      WK-SURYO.
     MOVE        WK-SURYO1   TO      MS2-03.
     IF   WK-SURYO2 NOT = ZERO
          MOVE  "."          TO      MS2-031
          MOVE  WK-SURYO2    TO      MS2-032
     END-IF.
*原単価
     MOVE        DOT-F172    TO      WK-GENKA.
     MOVE        WK-GENKA1   TO      MS2-04.
     IF  WK-GENKA2  NOT =  ZERO
         MOVE    WK-GENKA2   TO      MS2-041
     END-IF.
*売単価
     MOVE        DOT-F173    TO      MS2-05.
*合計加算
     COMPUTE GK-GENKA-HTY = GK-GENKA-HTY + DOT-F50 * DOT-F172.
     COMPUTE GK-GENKA-TEI = GK-GENKA-TEI + DOT-F15 * DOT-F172.
     COMPUTE GK-BAIKA-HTY = GK-BAIKA-HTY + DOT-F50 * DOT-F173.
     COMPUTE GK-BAIKA-TEI = GK-BAIKA-TEI + DOT-F15 * DOT-F173.
*明細行印字
     WRITE    PRT-REC   FROM      MS01      AFTER     1.
     WRITE    PRT-REC   FROM      MS02      AFTER     1.
*数量訂正された場合は、訂正行を印字する。
     IF          DOT-F15  NOT =  DOT-F50
              WRITE PRT-REC FROM  TE01      AFTER     0
     END-IF.
*
 MEIS-WRT1-EXIT.
     EXIT.
**********************************************************
*                 合計データ編集書き出し                 *
**********************************************************
 TAIL-WRT1-SEC                  SECTION.
*伝票ワーク⇒ドイトフォーマットへセット
     MOVE       WK-MEISAI-GYO(1)  TO   DOT-REC.
*テイル行初期化
     MOVE       SPACE             TO   TL01  TL02  TL03.
*有効期限
*    システム日付８桁変換
     INITIALIZE                      LINK-AREA.
     MOVE      "3"           TO      LINK-IN-KBN.
     MOVE       DOT-B12      TO      LINK-IN-YMD6.
     CALL      "SKYDTCKB"    USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     MOVE LINK-OUT-YMD8(1:4) TO      TL3-02.
     MOVE LINK-OUT-YMD8(5:2) TO      TL3-04.
     MOVE LINK-OUT-YMD8(7:2) TO      TL3-06.
*****MOVE NC"有効期限"       TO      TL3-01.
     MOVE SPACE              TO      TL3-01.
     MOVE NC"年"             TO      TL3-03.
     MOVE NC"月"             TO      TL3-05.
     MOVE NC"日"             TO      TL3-07.
*原価金額合計
     IF   GK-GENKA-HTY  NOT =  GK-GENKA-TEI
          MOVE GK-GENKA-TEI  TO      TL1-01
     END-IF.
     MOVE      GK-GENKA-HTY  TO      TL2-01
*売価金額合計
     IF   GK-BAIKA-HTY  NOT =  GK-BAIKA-TEI
          MOVE GK-BAIKA-TEI  TO      TL1-02
     END-IF.
     MOVE      GK-BAIKA-HTY  TO      TL2-02
*テイル行印字
     WRITE    PRT-REC   FROM      TL01      AFTER     1.
     WRITE    PRT-REC   FROM      TL02      AFTER     1.
*数量訂正された場合は、訂正行を印字する。
     IF       GK-GENKA-HTY  NOT =  GK-GENKA-TEI
              WRITE PRT-REC FROM  TE02      AFTER     0
     END-IF.
     WRITE    PRT-REC   FROM      TL03      AFTER     1.
*
 TAIL-WRT1-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                           2.2       *
****************************************************************
 DSP-WRT-SEC          SECTION.
*
     MOVE     SPACE     TO        FORM-PARA.
     MOVE     NC"Ａ４用紙"  TO    DTYPE.
     MOVE     NC"ドイト（ドンキ合併）"  TO    TORINM.
     MOVE     WK-MAI    TO        MD01.
     MOVE    "SCREEN"   TO        DSP-GRP.
     MOVE    "FSY11011" TO        DSP-FMT.
     WRITE    FSY11011.
*
     MOVE     SPACE     TO        MD05.
 DSP-WRT-EXIT.
     EXIT.
****************************************************************
*             画面ＲＥＡＤ処理                      2.3        *
****************************************************************
 DSP-RD-SEC           SECTION.
*
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
     MOVE         "NE"       TO   DSP-PRO.
     MOVE         "KEY01"    TO   DSP-GRP.
     PERFORM       DSP-RD-SEC.
*
     EVALUATE      DSP-FNC
         WHEN     "F005"
                   MOVE      9         TO   END-FLG
                   GO                  TO   KEY-IN-EXIT
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      MSG04     TO   MD05
                   GO                  TO   KEY-IN-EXIT
     END-EVALUATE.
****************
*エラーチェック*
****************
*
     IF  (MD02  NOT  NUMERIC)
         MOVE    ZERO      TO        MD02.
*
     IF  (MD03  NOT  NUMERIC)
     OR  (MD03 = ZERO)
         MOVE  ALL "9"     TO        MD03.
*出力区分チェック
     IF  (MD04  =  "1"  OR  "2"  OR  "3")
         MOVE   "M"        TO        EDIT-OPTION  OF    MD04
         MOVE    SPACE     TO        EDIT-CURSOR  OF    MD04
       ELSE
         MOVE   "R"        TO        EDIT-OPTION  OF    MD04
         MOVE   "C"        TO        EDIT-CURSOR  OF    MD04
         MOVE    MSG02     TO        MD05
         PERFORM DSP-WRT-SEC
         GO                TO        KEY-IN-01.
*伝票■大小チェック
     IF  (MD02 > MD03)
         MOVE   "R"        TO        EDIT-OPTION  OF    MD02
         MOVE   "R"        TO        EDIT-OPTION  OF    MD03
         MOVE   "C"        TO        EDIT-CURSOR  OF    MD02
         MOVE    MSG05     TO        MD05
         PERFORM DSP-WRT-SEC
         GO                TO        KEY-IN-01
       ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF    MD02
         MOVE   "M"        TO        EDIT-OPTION  OF    MD03
         MOVE    SPACE     TO        EDIT-CURSOR  OF    MD02
     END-IF.
*項目表示
     PERFORM     DSP-WRT-SEC.
 KEY-IN-EXIT.
     EXIT.
******************************************************************
 END PROGRAM  SSY1411L.
******************************************************************
