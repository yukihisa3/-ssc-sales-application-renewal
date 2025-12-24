****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ドイトオンライン　　　　          *
*    モジュール名　　　　：　ドイトオンライン仕入伝票発行　　  *
*    作成日／更新日　　　：　03/12/12                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    伝票フォーマット　　：　専用（レーザープリンタ）　　　　　*
*    （Ａ４一括出力－訂正有）                                  *
*    2010/02/24 JANCD出力追加　高橋                            *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY1407L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          03/12/12.
******************************************************************
 ENVIRONMENT            DIVISION.
******************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*伝票データ
     SELECT   DTSHIRED  ASSIGN    TO        DTSHIRED
                        ACCESS    MODE IS   SEQUENTIAL
                        STATUS              DEN-ST.
*店舗マスタ
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE      STATUS    TEN-ST.
*プリンター
     SELECT   PRTF      ASSIGN    TO        GS-PRTF
                        DESTINATION        "PRT"
                        FORMAT              PRT-FORM
                        GROUP               PRT-GRP
                        PROCESSING          PRT-PROC
                        UNIT CONTROL        PRT-CTL
                        FILE      STATUS    PRT-ST.
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
 FD  DTSHIRED
                 BLOCK    CONTAINS  1    RECORDS
                 LABEL    RECORD    IS   STANDARD.
                 COPY     DTSHIRED  OF   XFDLIB
                 JOINING  DEN       AS   PREFIX.
*店舗マスタ
 FD  HTENMS      BLOCK    CONTAINS  8    RECORDS
                 LABEL    RECORD    IS   STANDARD.
                 COPY     HTENMS    OF   XFDLIB
                 JOINING  TEN       AS   PREFIX.
*プリンター
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FSY14071  OF        XMDLIB.
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
     COPY   DTSHIRED OF XFDLIB  JOINING   DOT  AS   PREFIX.
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
*伝票データ
 DEN-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DTSHIRED.
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
     OPEN        INPUT     DTSHIRED  HTENMS
                 I-O       DSPF.
*
     MOVE     ZERO               TO   WK-DEN WK-MAI WK-DENNO.
     MOVE     ZERO               TO   CNT-DENPYO.
*帳票出力エリア初期化
     MOVE     SPACE              TO   PRT-CTL.
     MOVE     "FSY14071"         TO   PRT-FORM.
     MOVE     SPACE              TO   FSY14071.
*伝票枚数カウント
     PERFORM  DENPYO-CNT-SEC  UNTIL  CNT-FLG = "END".
*
     IF   WK-MAI  =  ZERO
          DISPLAY NC"＃＃対象データ無＃＃" UPON CONS
          STOP  RUN
     END-IF.
*
     CLOSE    DTSHIRED.
*画面初期化*
     MOVE        SPACE     TO        FSY11011.
*帳票初期化*
     MOVE        SPACE     TO        FSY14071.
 INIT-EXIT.
     EXIT.
**********************************************************
*    伝票枚数カウント
**********************************************************
 DENPYO-CNT-SEC            SECTION.
*伝票総枚数読込み
     READ     DTSHIRED  AT  END
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
                   OPEN      INPUT DTSHIRED
                   PERFORM   DENPYO-CTL-SEC
                             UNTIL     INJ-FLG   =   "END"
                   CLOSE     DTSHIRED  PRTF
                   MOVE      SPACE     TO   MD05
         WHEN      "3"
*                  『範囲出力』
                   OPEN  OUTPUT  PRTF
                   MOVE      ZERO      TO   ZEN-FLG  MAI-CNT
                   MOVE      SPACE     TO   INJ-FLG
                   INITIALIZE               WK-MEISAI
                   PERFORM   KEY-IN-SEC
                   OPEN      INPUT DTSHIRED
                   PERFORM   DENPYO-CTL-SEC
                             UNTIL     INJ-FLG   =   "END"
                   CLOSE     DTSHIRED  PRTF
                   MOVE      SPACE     TO   MD05
         WHEN      OTHER
                   MOVE      MSG02     TO   MD05
     END-EVALUATE.
     PERFORM       DSP-WRT-SEC.
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
     READ     DTSHIRED   AT        END
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
*****『帳票初期化』
     MOVE     SPACE              TO   FSY14071.
     MOVE     ZERO               TO   GK-AREA.
*****『上段伝票ヘッダ』
     PERFORM  HEAD-WRT-SEC.
*****『上段伝票明細』
*****DISPLAY "IX = " IX UPON CONS.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 6
             PERFORM MEIS-WRT-SEC
     END-PERFORM.
*****『上段伝票テイル』
     PERFORM  TAIL-WRT-SEC.
*****『帳票出力』
*    印字制御項目セット
     MOVE      SPACE       TO        PRT-PROC.
     MOVE      SPACE       TO        PRT-CTL.
     MOVE      SPACE       TO        PRT-FORM.
     MOVE     "FSY14071"   TO        PRT-FORM.
     MOVE     "SCREEN"     TO        PRT-GRP.
     WRITE     FSY14071.
*****『帳票初期化』
     MOVE      SPACE       TO        FSY14071.
     MOVE      ZERO        TO        GK-AREA.
*
 DENPYO-SEIGYO-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 HEAD-WRT-SEC                 SECTION.
*伝票ワーク⇒ドイトフォーマットへセット
     MOVE       WK-MEISAI-GYO(1)  TO   DOT-REC.
*伝票区分
     MOVE       DOT-A03      TO      DENKU1 DENKU2.
*取引先ＣＤ
     MOVE       DOT-A06      TO      TORCD1 TORCD2.
*取引先名称
     MOVE       DOT-A18      TO      TORNM1 TORNM2.
*オーダー
     MOVE       DOT-A05      TO      ORDR1  ORDR2.
*納品日
     MOVE       DOT-F111     TO      WK-NOU-DATE1.
     MOVE       WK-NOU-DATE1 TO      WK-NOU-DATE.
*****DISPLAY "DOT-F111 = " DOT-F111 UPON CONS.
*****DISPLAY "WK-NOU-DATE = " WK-NOU-DATE UPON CONS.
     MOVE       WK-NOU-DT1   TO      WK-HEN-DT1.
     MOVE       "/"          TO      WK-HEN-DT2.
     MOVE       WK-NOU-DT2   TO      WK-HEN-DT3.
     MOVE       "/"          TO      WK-HEN-DT4.
     MOVE       WK-NOU-DT3   TO      WK-HEN-DT5.
     MOVE       WK-HEN-DATE  TO      HATYU1 HATYU2.
*店舗ＣＤ
     MOVE       DOT-A11      TO      TENCD1 TENCD2.
*部門
     MOVE       SPACE        TO      BUMON1(1:1) BUMON2(1:1).
     MOVE       DOT-A12(2:2) TO      BUMON1(2:2) BUMON2(2:2).
*店舗名
     MOVE       10545        TO      TEN-F52.
     MOVE       DOT-A11      TO      TEN-F011.
     READ       HTENMS
      INVALID     MOVE ALL NC"＊" TO TENNM3 TENNM4
      NOT INVALID MOVE TEN-F03    TO TENNM3 TENNM4
     END-READ.
*
*    MOVE       DOT-A53      TO      TENNM1 TENNM2.
*商品区分
     MOVE       DOT-A13      TO      SYOKU1 SYOKU2.
*企画番号
     MOVE       DOT-A07      TO      KIKAK1 KIKAK2.
*伝票番号
     MOVE       DOT-A14      TO      DENNO1 DENNO2.
*バーコード
     MOVE       DOT-A03      TO      WK-JANCD-01 WK-HEN-JANCD1.
     MOVE       DOT-A06      TO      WK-JANCD-02 WK-HEN-JANCD3.
     MOVE       DOT-A14      TO      WK-JANCD-03 WK-HEN-JANCD5.
     MOVE      X"C1"         TO      WK-JANCD-00 WK-JANCD-99.
     MOVE       WK-JANCD     TO      JANCD1 JANCD2.
     MOVE       "-"          TO      WK-HEN-JANCD2 WK-HEN-JANCD4.
     MOVE       WK-HEN-JANCD TO      JANNM1 JANNM2.
*
 HEAD-WRT-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIS-WRT-SEC                  SECTION.
*明細行判定
*****IF  WK-MEISAI-GYO(IX)  =  SPACE
*****    GO                 TO       MEIS-WRT-EXIT
*****END-IF.
*伝票データセット
     MOVE  WK-MEISAI-GYO(IX) TO      DOT-REC.
*明細行判定
     IF  DOT-F02  =  ZERO
         GO                 TO       MEIS-WRT-EXIT
     END-IF.
*商品名称
     MOVE        DOT-A29   TO        SYNM11(IX) SYNM21(IX).
     MOVE        DOT-A30   TO        SYNM12(IX) SYNM22(IX).
*商品ＣＤ
*2010/02/24 ｲﾝｽﾄCD=>JANCDに変更
*****MOVE        DOT-A31   TO        SYOCD1(IX) SYOCD2(IX).
     MOVE        DOT-A44   TO        SYOCD1(IX) SYOCD2(IX).
     MOVE        DOT-A31   TO        INSCD1(IX) INSCD2(IX).
*数量
*****DISPLAY "DOT-F15 = " DOT-F15 UPON CONS.
*****DISPLAY "DOT-F50 = " DOT-F50 UPON CONS.
     IF          DOT-F15  NOT =  DOT-F50
          MOVE        "ﾃ=="     TO      TESEI1(IX) TESEI3(IX)
          MOVE        "="       TO      TESEI2(IX) TESEI4(IX)
          MOVE        DOT-F15   TO      WK-SURYO
          MOVE        WK-SURYO1 TO      SURYT1(IX) SURYT2(IX)
          IF  WK-SURYO2  NOT =  ZERO
             MOVE    "."       TO       TTEN1(IX)   TTEN2(IX)
             MOVE    WK-SURYO2 TO       SYOTT1(IX) SYOTT2(IX)
          END-IF
          MOVE        DOT-F50   TO      WK-SURYO
          IF  WK-SURYO2  NOT =  ZERO
              CONTINUE
          ELSE
              MOVE    "="       TO      TEN1(IX)   TEN2(IX)
              MOVE    "="       TO      SYOTE1(IX) SYOTE2(IX)
          END-IF
     END-IF.
     MOVE        DOT-F50   TO        WK-SURYO.
     MOVE        WK-SURYO1 TO        SURYO1(IX) SURYO2(IX).
     IF  WK-SURYO2  NOT =  ZERO
         MOVE    "."       TO        TEN1(IX)   TEN2(IX)
         MOVE    WK-SURYO2 TO        SYOTE1(IX) SYOTE2(IX)
     END-IF.
*原単価
     MOVE        DOT-F172  TO        WK-DEN172.
     MOVE        WK-DEN172A TO       GENKA1(IX) GENKA2(IX).
     IF  WK-DEN172B = ZERO
         MOVE    SPACE     TO        GENOP1(IX) GENOP2(IX)
     ELSE
         MOVE  WK-DEN172B  TO        GENOP1(IX) GENOP2(IX)
     END-IF.
*原単価
     MOVE        DOT-F173  TO        BAIKA1(IX) BAIKA2(IX).
*合計加算
     COMPUTE GK-GENKA-HTY = GK-GENKA-HTY + DOT-F50 * DOT-F172.
     COMPUTE GK-GENKA-TEI = GK-GENKA-TEI + DOT-F15 * DOT-F172.
     COMPUTE GK-BAIKA-HTY = GK-BAIKA-HTY + DOT-F50 * DOT-F173.
     COMPUTE GK-BAIKA-TEI = GK-BAIKA-TEI + DOT-F15 * DOT-F173.
*
 MEIS-WRT-EXIT.
     EXIT.
**********************************************************
*                 合計データ編集書き出し                 *
**********************************************************
 TAIL-WRT-SEC                  SECTION.
*伝票ワーク⇒ドイトフォーマットへセット
     MOVE       WK-MEISAI-GYO(1)  TO   DOT-REC.
*有効期限
*    システム日付８桁変換
     INITIALIZE                      LINK-AREA.
     MOVE      "3"           TO      LINK-IN-KBN.
     MOVE       DOT-A10      TO      LINK-IN-YMD6.
     CALL      "SKYDTCKB"    USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     MOVE LINK-OUT-YMD8(1:4) TO      YUYY1 YUYY2.
     MOVE LINK-OUT-YMD8(5:2) TO      YUMM1 YUMM2.
     MOVE LINK-OUT-YMD8(7:2) TO      YUDD1 YUDD2.
*原価金額合計
     IF   GK-GENKA-HTY  NOT =  GK-GENKA-TEI
          MOVE GK-GENKA-TEI  TO      GKTGK1 GKTGK2
          MOVE "ﾃ="          TO      GTESE1 GTESE3
          MOVE "=="          TO      GTESE2 GTESE4
          MOVE "ﾃ="          TO      GTESE5 GTESE7
          MOVE "=="          TO      GTESE6 GTESE8
     END-IF.
     MOVE      DOT-A48       TO      GKGEN1 GKGEN2.
*売価金額合計
     IF   GK-BAIKA-HTY  NOT =  GK-BAIKA-TEI
          MOVE GK-BAIKA-TEI  TO      GKTBK1 GKTBK2
     END-IF.
     MOVE      DOT-A49       TO      GKBAI1 GKBAI2.
*
 TAIL-WRT-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                           2.2       *
****************************************************************
 DSP-WRT-SEC          SECTION.
*
     MOVE     SPACE     TO        FORM-PARA.
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
 END PROGRAM  SSY1407L.
******************************************************************
