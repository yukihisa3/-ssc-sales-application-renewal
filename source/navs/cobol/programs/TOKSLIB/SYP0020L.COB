****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　ユポラベル出力　　　　            *
*    作成日／更新日　　　：　2004/01/22                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　画面よりユポラベル発行の範囲を指定*
*                            しユポラベルを発行する。          *
*                            （消費税総額表示対応）            *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SYP0020L.
 AUTHOR.             NAV.
 DATE-WRITTEN.       04/01/22.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS     CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*----<< 画面　Ｆ >>----*
     SELECT  DSPF            ASSIGN    TO          GS-DSPF
                             SYMBOLIC  DESTINATION IS   "DSP"
                             PROCESSING  MODE      IS   DSP-TYP
                             GROUP                 IS   DSP-GRP
                             FORMAT                IS   DSP-FRM
                             SELECTED  FUNCTION    IS   DSP-FUNC
                             FILE      STATUS      IS   DSP-STA.
*----<< ユポラベルファイル  >>---*
     SELECT   NYUPOF         ASSIGN    TO         01-VI-NYUPOL1
                             ORGANIZATION         IS   INDEXED
                             ACCESS MODE          IS   SEQUENTIAL
                             RECORD KEY           IS   ZYU-F02
                                                       ZYU-F03
                                                       ZYU-F011
                                                       ZYU-F012
                             FILE STATUS          IS   ZYU-STA.
*----<< プリンタＦ >>-----*
     SELECT   PRTF           ASSIGN      TO  GS-PRTF
                             ORGANIZATION         IS   SEQUENTIAL
                             ACCESS MODE          IS   SEQUENTIAL
                             SYMBOLIC DESTINATION IS  "PRT"
                             PROCESSING MODE      IS   PRT-PRO
                             GROUP                IS   PRT-GRP
                             FORMAT               IS   PRT-FMT
                             SELECTED FUNCTION    IS   PRT-FNC
                             UNIT     CONTROL     IS   PRT-CTL
                             FILE STATUS          IS   PRT-STA
                             DESTINATION-1        IS   PRT-DES.
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*----<< 画面　Ｆ >>----*
 FD  DSPF.
     COPY    FYP00201  OF   XMDLIB.
*----<< ユポラベルファイル  >>---*
 FD  NYUPOF.
     COPY     NYUPOF  OF  XFDLIB
     JOINING  ZYU      AS  PREFIX.
*----<< プリンタＦ >>-----*
 FD  PRTF.
     COPY     FYP00202 OF  XMDLIB.
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*画面Ｆ制御用
 01  DSP-CNTL                    VALUE    SPACE.
     03  DSP-FRM                 PIC  X(8).
     03  DSP-GRP                 PIC  X(8).
     03  DSP-TYP                 PIC  X(2).
     03  DSP-FUNC                PIC  X(4).
     03  DSP-STA                 PIC  X(2).
*プリンタＦ制御用
 01  PRT-CONTROL.
     03  PRT-PRO           PIC  X(02).
     03  PRT-GRP           PIC  X(08).
     03  PRT-FMT           PIC  X(08).
     03  PRT-DES           PIC  X(08).
     03  PRT-CTL           PIC  X(06).
     03  PRT-FNC           PIC  X(04).
*ステータス
 01  STA-AREA.
     03  ZYU-STA             PIC  X(02).
     03  PRT-STA             PIC  X(02).
 01  WORK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  MAISU               PIC  9(04)  VALUE  ZERO.
     03  W-KINGK1             PIC  N(07)  VALUE  SPACE.
     03  W-KINGK2             PIC  N(06)  VALUE  SPACE.
     03  W-SU                PIC  9(01).
     03  W-RSU     REDEFINES   W-SU.
      05  R-SU               PIC  X(01).
     03  I                   PIC  9(01)  VALUE  ZERO.
     03  J                   PIC  9(01)  VALUE  ZERO.
     03  WK-KING             PIC  9(05)  VALUE  ZERO.
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
 01  MSG-GAIDO.
     03  GAID-1                  PIC  N(30)     VALUE
         NC"_終了".
     03  GAID-2                  PIC  N(30)     VALUE
         NC"_取消　_終了　_項目戻り".
 01  MSG-WORLD.
     03  MSG-1                   PIC  N(20)     VALUE
         NC"１、２、３を入力してください。".
     03  MSG-2                   PIC  N(20)     VALUE
         NC"誤ったＰＦキーが押下されました。".
     03  MSG-3                   PIC  N(20)     VALUE
         NC"開始が終了を超えています。".
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SYP0020L".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
 01  WK-SYOCD1.
     03  WK-SYOCD11          PIC  X(08)  VALUE  SPACE.
     03  WK-SYOCD12          PIC  X(05)  VALUE  SPACE.
     03  WK-SYOCD13          PIC  X(02)  VALUE  SPACE.
     03  WK-SYOCD14          PIC  X(01)  VALUE  SPACE.
 01  WK-SYOCD2.
     03  WK-SYOCD21          PIC  X(08)  VALUE  SPACE.
     03  WK-SYOCD22          PIC  X(05)  VALUE  SPACE.
     03  WK-SYOCD23          PIC  X(02)  VALUE  SPACE.
     03  WK-SYOCD24          PIC  X(01)  VALUE  SPACE.
 01  WK-SYOHIN.
     03  WK-SYOHIN1          PIC  X(08)  VALUE  SPACE.
     03  WK-SYOHIN2          PIC  X(08)  VALUE  SPACE.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
******************************************************************
 PROCEDURE               DIVISION.
******************************************************************
 DECLARATIVES.
*プリントＦ
 DSP-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       DSPF.
     MOVE    "DSPF"        TO    ERR-FL-ID.
     MOVE     DSP-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*プリントＦ
 PRT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       PRTF.
     MOVE    "PRTF"        TO    ERR-FL-ID.
     MOVE     PRT-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*ユポラベルＦ
 ZYU-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       NYUPOF.
     MOVE    "NYUPOF"     TO    ERR-FL-ID.
     MOVE     ZYU-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  END-FLG  =  "END"
     PERFORM  END-SEC.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
     DISPLAY  "**  SYP0020L   START  **"   UPON  CONS.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*ファイル ＯＰＥＮ
     OPEN     I-O         DSPF.
     OPEN     INPUT       NYUPOF.
     OPEN     OUTPUT      PRTF.
     PERFORM  DSP-SEC.
 INIT-EXIT.
     EXIT.
*=============================================================
*              処理区分入力処理                              *
*=============================================================
 DSP-SEC             SECTION.
*初期画面表示
 DSP-010.
     MOVE      LOW-VALUE         TO        FYP00201.
     MOVE      "FYP00201"        TO        DSP-FRM.
     MOVE      "CL"              TO        DSP-TYP.
     MOVE      HEN-DATE          TO        SDATE.
     MOVE      HEN-TIME          TO        STIME.
     MOVE      SPACE             TO        SCD11.
     MOVE      SPACE             TO        SCD12.
     MOVE      SPACE             TO        SCD13.
     MOVE      SPACE             TO        SCD14.
     MOVE      SPACE             TO        SCD21.
     MOVE      SPACE             TO        SCD22.
     MOVE      SPACE             TO        SCD23.
     MOVE      SPACE             TO        SCD24.
     MOVE      SPACE             TO        JANCD1.
     MOVE      SPACE             TO        JANCD2.
*処理区分入力処理
 DSP-020.
     MOVE      GAID-1            TO        GAID.
     PERFORM   DSP-WRITE-SEC.
     MOVE      "KUBUN"           TO        DSP-GRP.
     MOVE      "NE"              TO        DSP-TYP.
     PERFORM   DSP-READ-SEC.
     EVALUATE  DSP-FUNC
         WHEN  "F005"
                MOVE    "END"    TO        END-FLG
                GO               TO        DSP-EXIT
         WHEN  "E000"
                IF   (KBN IS NOT NUMERIC) OR
                     (KBN NOT = 1 AND 2  AND  3)
                      MOVE    MSG-1    TO    MSG
                      MOVE    "R"      TO    EDIT-OPTION OF KBN
                      MOVE    "C"      TO    EDIT-CURSOR OF KBN
                      GO               TO    DSP-020
                 ELSE
                      MOVE    SPACE    TO    MSG
                      MOVE    "D"      TO    EDIT-OPTION OF KBN
                      MOVE    " "      TO    EDIT-CURSOR OF KBN
                      IF  KBN  =  1  OR  2
                          MOVE  SPACE   TO WK-SYOCD11 SCD11
                          MOVE  SPACE   TO WK-SYOCD12 SCD12
                          MOVE  SPACE   TO WK-SYOCD13 SCD13
                          MOVE  SPACE   TO WK-SYOCD14 SCD14
                          MOVE  ALL "9" TO WK-SYOCD21 SCD21
                          MOVE  ALL "9" TO WK-SYOCD22 SCD22
                          MOVE  ALL "9" TO WK-SYOCD23 SCD23
                          MOVE  ALL "9" TO WK-SYOCD24 SCD24
                          MOVE  SPACE   TO JANCD1
                          MOVE  ALL "9" TO JANCD2
                          MOVE  ZERO    TO TCD1
                          MOVE  99999999 TO TCD2
                          GO            TO DSP-030
                      END-IF
                END-IF
         WHEN   OTHER
                MOVE    MSG-2    TO        MSG
                GO               TO        DSP-020
     END-EVALUATE.
*出力範囲入力
 DSP-025.
     MOVE      GAID-1            TO        GAID.
     PERFORM   DSP-WRITE-SEC.
     MOVE      "HANI "           TO        DSP-GRP.
     MOVE      "NE"              TO        DSP-TYP.
     PERFORM   DSP-READ-SEC.
     EVALUATE  DSP-FUNC
         WHEN  "F005"
                MOVE    "END"    TO        END-FLG
                GO               TO        DSP-EXIT
         WHEN  "E000"
                IF    TCD1   NOT   NUMERIC
                      MOVE ZERO     TO TCD1
                END-IF
                IF    TCD2   NOT   NUMERIC
                      MOVE 99999999 TO TCD2
                END-IF
                IF    TCD1  >  TCD2
                      MOVE    MSG-3    TO MSG
                      MOVE    "R"      TO EDIT-OPTION OF TCD1
                      MOVE    "R"      TO EDIT-OPTION OF TCD2
                      MOVE    "C"      TO EDIT-CURSOR OF TCD1
                      GO               TO DSP-025
                ELSE
                      MOVE    SPACE    TO MSG
                      MOVE    "D"      TO EDIT-OPTION OF TCD1
                      MOVE    "D"      TO EDIT-OPTION OF TCD2
                      MOVE    " "      TO EDIT-CURSOR OF TCD1
                END-IF
                IF    SCD21  =  SPACE
                      MOVE ALL "9" TO SCD21
                END-IF
                IF    SCD22  =  SPACE
                      MOVE ALL "9" TO SCD22
                END-IF
                IF    SCD23  =  SPACE
                      MOVE ALL "9" TO SCD23
                END-IF
                IF    SCD24  =  SPACE
                      MOVE ALL "9" TO SCD24
                END-IF
                MOVE  SCD11 TO WK-SYOCD11
                MOVE  SCD12 TO WK-SYOCD12
                MOVE  SCD13 TO WK-SYOCD13
                MOVE  SCD14 TO WK-SYOCD14
                MOVE  SCD21 TO WK-SYOCD21
                MOVE  SCD22 TO WK-SYOCD22
                MOVE  SCD23 TO WK-SYOCD23
                MOVE  SCD24 TO WK-SYOCD24
                IF    WK-SYOCD1  >  WK-SYOCD2
                      MOVE    MSG-3    TO MSG
                      MOVE    "R"      TO EDIT-OPTION OF SCD11
                      MOVE    "R"      TO EDIT-OPTION OF SCD12
                      MOVE    "R"      TO EDIT-OPTION OF SCD13
                      MOVE    "R"      TO EDIT-OPTION OF SCD14
                      MOVE    "R"      TO EDIT-OPTION OF SCD21
                      MOVE    "R"      TO EDIT-OPTION OF SCD22
                      MOVE    "R"      TO EDIT-OPTION OF SCD23
                      MOVE    "R"      TO EDIT-OPTION OF SCD24
                      MOVE    "C"      TO EDIT-CURSOR OF SCD11
                      GO               TO DSP-025
                ELSE
                      MOVE    SPACE    TO MSG
                      MOVE    "D"      TO EDIT-OPTION OF SCD11
                      MOVE    "D"      TO EDIT-OPTION OF SCD12
                      MOVE    "D"      TO EDIT-OPTION OF SCD13
                      MOVE    "D"      TO EDIT-OPTION OF SCD14
                      MOVE    "D"      TO EDIT-OPTION OF SCD21
                      MOVE    "D"      TO EDIT-OPTION OF SCD22
                      MOVE    "D"      TO EDIT-OPTION OF SCD23
                      MOVE    "D"      TO EDIT-OPTION OF SCD24
                      MOVE    " "      TO EDIT-CURSOR OF SCD11
                END-IF
                IF    JANCD2  =  SPACE
                      MOVE ALL "9" TO JANCD2
                END-IF
                IF    JANCD1  >  JANCD2
                      MOVE    MSG-3    TO MSG
                      MOVE    "R"      TO EDIT-OPTION OF JANCD1
                      MOVE    "R"      TO EDIT-OPTION OF JANCD2
                      MOVE    "C"      TO EDIT-CURSOR OF JANCD1
                      GO               TO DSP-025
                ELSE
                      MOVE    SPACE    TO MSG
                      MOVE    "D"      TO EDIT-OPTION OF JANCD1
                      MOVE    "D"      TO EDIT-OPTION OF JANCD2
                      MOVE    " "      TO EDIT-CURSOR OF JANCD1
                END-IF
         WHEN   OTHER
                MOVE    MSG-2    TO        MSG
                GO               TO        DSP-025
     END-EVALUATE.
*確認入力処理
 DSP-030.
     MOVE      GAID-2            TO        GAID.
     PERFORM   DSP-WRITE-SEC.
     MOVE      "TAIL"           TO        DSP-GRP.
     MOVE      "NE"              TO        DSP-TYP.
     PERFORM   DSP-READ-SEC.
     EVALUATE  DSP-FUNC
         WHEN  "F004"
                GO               TO        DSP-010
         WHEN  "F005"
                MOVE    "END"    TO        END-FLG
         WHEN  "F006"
                MOVE    SPACE    TO        MSG
                GO               TO        DSP-020
         WHEN  "E000"
                IF     KBN  =   1
                       PERFORM    TEST-SEC
                       GO        TO        DSP-010
                END-IF
         WHEN   OTHER
                MOVE    MSG-2    TO        MSG
                GO               TO        DSP-030
     END-EVALUATE.
 DSP-EXIT.
     EXIT.
*=============================================================
*                テスト印刷                                  *
*=============================================================
 TEST-SEC            SECTION.
     MOVE   NC"ＮＮＮＮＮＮＮＮＮＮＮＮＮ"  TO
                SMEI1L  SMEI2L SMEI3L  SMEI1R  SMEI2R SMEI3R.
     MOVE   NC"９９，９９９"     TO       KINGLN  KINGLK.
     MOVE   NC"９９，９９９"     TO       KINGRN  KINGRK.
     MOVE   "9999"               TO       BUMON1  BUMON2.
     MOVE "FYP00202"             TO  PRT-FMT.
     MOVE  SPACE                 TO  PRT-CTL.
     MOVE  SPACE                 TO  PRT-PRO.
     MOVE "SCREEN"               TO  PRT-GRP.
     WRITE     FYP00202.
     CLOSE     PRTF.
     OPEN   OUTPUT   PRTF.
 TEST-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
*ユポラベルＦ　ＲＥＡＤ
     PERFORM   ZYU-READ-SEC.
     IF    END-FLG  =  "END"
           GO       TO      MAIN-EXIT
     END-IF.
     MOVE   ZERO     TO      MAISU.
     PERFORM     UNTIL  MAISU  >= ZYU-F10
       MOVE   SPACE           TO       FYP00202
       MOVE   ZYU-F071        TO       SMEI1L
       MOVE   ZYU-F072(1:7)   TO       SMEI2L
       MOVE   ZYU-F072(8:7)   TO       SMEI3L
       MOVE   ZYU-F08         TO       BUMON1
       MOVE   ZYU-F06         TO       BCD1
*******税込価格（左）
       MOVE   ZYU-F12         TO       WK-KING
       PERFORM HENSYU-KOM-SEC
       MOVE     W-KINGK1       TO       KINGLN
*******税抜価格（左）
       MOVE   ZYU-F09         TO       WK-KING
       PERFORM HENSYU-NUK-SEC
       MOVE     W-KINGK2       TO       KINGLK
       IF       W-KINGK2  NOT =  SPACE
                MOVE "("      TO       KUGL21
                MOVE NC"税抜" TO       LK
                MOVE ")"      TO       KUGL22
       END-IF
       ADD      1           TO       MAISU
       IF     MAISU >= ZYU-F10
          MOVE "FYP00202"             TO  PRT-FMT
          MOVE  SPACE                 TO  PRT-CTL
          MOVE  SPACE                 TO  PRT-PRO
          MOVE "SCREEN"               TO  PRT-GRP
          WRITE     FYP00202
          GO                          TO  MAIN-SEC
       END-IF
       IF     END-FLG  = SPACE
            MOVE   ZYU-F071        TO       SMEI1R
            MOVE   ZYU-F072(1:7)   TO       SMEI2R
            MOVE   ZYU-F072(8:7)   TO       SMEI3R
            MOVE   ZYU-F08         TO       BUMON2
            MOVE   ZYU-F06         TO       BCD2
************税込価格（右）
            MOVE   ZYU-F12         TO       WK-KING
            PERFORM HENSYU-KOM-SEC
            MOVE     W-KINGK1       TO       KINGRN
************税抜価格（右）
            MOVE   ZYU-F09         TO       WK-KING
            PERFORM HENSYU-NUK-SEC
            MOVE     W-KINGK2       TO       KINGRK
            IF       W-KINGK2  NOT =  SPACE
                     MOVE "("      TO       KUGR21
                     MOVE NC"税抜" TO       RK
                     MOVE ")"      TO       KUGR22
            END-IF
            ADD     1           TO      MAISU
       END-IF
       MOVE "FYP00202"             TO  PRT-FMT
       MOVE  SPACE                 TO  PRT-CTL
       MOVE  SPACE                 TO  PRT-PRO
       MOVE "SCREEN"               TO  PRT-GRP
       WRITE     FYP00202
     END-PERFORM.
 MAIN-EXIT.
     EXIT.
*=============================================================
*                ユポラベルＦＲＥＡＤ処理                    *
*=============================================================
 ZYU-READ-SEC      SECTION.
     READ  NYUPOF
           AT   END
           MOVE   "END"    TO     END-FLG
           GO              TO     ZYU-READ-EXIT
        NOT AT END
           IF   ZYU-F11  = "9"
                GO         TO     ZYU-READ-SEC
           END-IF
           IF   ZYU-F10  = ZERO
                GO         TO     ZYU-READ-SEC
           END-IF
     END-READ.
*取引先ＣＤ
     IF    TCD1   <=   ZYU-F02
     AND   TCD2   >=   ZYU-F02
           CONTINUE
     ELSE
           GO              TO     ZYU-READ-SEC
     END-IF.
*商品ＣＤ
     MOVE  ZYU-F04         TO     WK-SYOHIN1.
     MOVE  ZYU-F05         TO     WK-SYOHIN2.
*
     IF    WK-SYOCD1   <=   WK-SYOHIN
     AND   WK-SYOCD2   >=   WK-SYOHIN
           CONTINUE
     ELSE
           GO              TO     ZYU-READ-SEC
     END-IF.
*ＪＡＮＣＤ
     IF    JANCD1   <=   ZYU-F06
     AND   JANCD2   >=   ZYU-F06
           CONTINUE
     ELSE
           GO              TO     ZYU-READ-SEC
     END-IF.
*
 ZYU-READ-EXIT.
     EXIT.
*=============================================================
*                画面ＦＲＥＡＤ処理                          *
*=============================================================
 DSP-READ-SEC      SECTION.
     READ      DSPF.
     MOVE    SPACE        TO      DSP-TYP.
 DSP-READ-EXIT.
     EXIT.
*=============================================================
*                画面ＦＷＲＩＴＥ処理                        *
*=============================================================
 DSP-WRITE-SEC     SECTION.
     MOVE      "SCREEN"          TO        DSP-GRP.
     WRITE     FYP00201.
     MOVE    SPACE        TO      DSP-TYP.
 DSP-WRITE-EXIT.
     EXIT.
*=============================================================
*                金額編集処理
*=============================================================
 HENSYU-KOM-SEC          SECTION.
*
     MOVE   SPACE           TO       W-KINGK1.
     MOVE   ZERO            TO       J.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I > 5
       ADD      1          TO       J
       IF       I     =     1
          IF    (WK-KING(1:1) = ZERO)
                 MOVE   SPACE   TO     W-KINGK1(J:1)
           ELSE
                 MOVE   NC"￥"  TO     W-KINGK1(J:1)
          END-IF
          ADD       1           TO     J
       END-IF
       IF       I     =     3
          IF    (WK-KING(1:1) = ZERO) AND (WK-KING(2:1) = ZERO)
                 MOVE   SPACE   TO     W-KINGK1(J:1)
           ELSE
                 MOVE   NC"，"  TO     W-KINGK1(J:1)
          END-IF
          ADD       1           TO     J
       END-IF
       MOVE   WK-KING(I:1)   TO     R-SU
       EVALUATE    R-SU
         WHEN      0
            MOVE   NC"０"   TO    W-KINGK1(J:1)
         WHEN      1
            MOVE   NC"１"   TO    W-KINGK1(J:1)
         WHEN      2
            MOVE   NC"２"   TO    W-KINGK1(J:1)
         WHEN      3
            MOVE   NC"３"   TO    W-KINGK1(J:1)
         WHEN      4
            MOVE   NC"４"   TO    W-KINGK1(J:1)
         WHEN      5
            MOVE   NC"５"   TO    W-KINGK1(J:1)
         WHEN      6
            MOVE   NC"６"   TO    W-KINGK1(J:1)
         WHEN      7
            MOVE   NC"７"   TO    W-KINGK1(J:1)
         WHEN      8
            MOVE   NC"８"   TO    W-KINGK1(J:1)
         WHEN      9
            MOVE   NC"９"   TO    W-KINGK1(J:1)
       END-EVALUATE
     END-PERFORM.
     MOVE     ZERO             TO    J
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I > 5
        MOVE     I       TO          J
        ADD      1       TO          J
        IF    WK-KING(I:1)   NOT =  ZERO
              IF ( J > 2 ) AND ( J < 7 )
                   IF  J =  2 OR 3
                       ADD  -1   TO  J
                   END-IF
                   MOVE  NC"￥"  TO  W-KINGK1(J:1)
              END-IF
              MOVE     5       TO    I
        ELSE
              IF      I   >=  3
                   ADD    1     TO      J
              END-IF
              MOVE    SPACE    TO    W-KINGK1(J:1)
        END-IF
     END-PERFORM.
*
 HENSYU-KOM-EXIT.
     EXIT.
*=============================================================
*                金額編集処理
*=============================================================
 HENSYU-NUK-SEC          SECTION.
*
     MOVE   SPACE           TO       W-KINGK2.
     MOVE   ZERO            TO       J.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I > 5
       ADD      1          TO       J
       IF       I     =     3
          IF    (WK-KING(1:1) = ZERO) AND (WK-KING(2:1) = ZERO)
                 MOVE   SPACE   TO     W-KINGK2(J:1)
           ELSE
               MOVE   NC"，"  TO     W-KINGK2(J:1)
          END-IF
          ADD       1           TO     J
       END-IF
       MOVE   WK-KING(I:1)   TO     R-SU
       EVALUATE    R-SU
         WHEN      0
            MOVE   NC"０"   TO    W-KINGK2(J:1)
         WHEN      1
            MOVE   NC"１"   TO    W-KINGK2(J:1)
         WHEN      2
            MOVE   NC"２"   TO    W-KINGK2(J:1)
         WHEN      3
            MOVE   NC"３"   TO    W-KINGK2(J:1)
         WHEN      4
            MOVE   NC"４"   TO    W-KINGK2(J:1)
         WHEN      5
            MOVE   NC"５"   TO    W-KINGK2(J:1)
         WHEN      6
            MOVE   NC"６"   TO    W-KINGK2(J:1)
         WHEN      7
            MOVE   NC"７"   TO    W-KINGK2(J:1)
         WHEN      8
            MOVE   NC"８"   TO    W-KINGK2(J:1)
         WHEN      9
            MOVE   NC"９"   TO    W-KINGK2(J:1)
       END-EVALUATE
     END-PERFORM.
     MOVE     ZERO             TO    J
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I > 5
        MOVE     I       TO          J
        IF    WK-KING(I:1)   NOT =  ZERO
              IF ( J > 1 ) AND ( J < 6 )
                   IF  J =  2
                       ADD  -1   TO  J
                   END-IF
                   MOVE  NC"￥"  TO  W-KINGK2(J:1)
              END-IF
              MOVE     5       TO    I
        ELSE
              IF      I   >=  3
                   ADD    1     TO      J
              END-IF
              MOVE    SPACE    TO    W-KINGK2(J:1)
        END-IF
     END-PERFORM.
*
 HENSYU-NUK-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      DSPF    PRTF    NYUPOF.
     DISPLAY  "**  SYP0020L     END  **"   UPON  CONS.
 END-EXIT.
     EXIT.
