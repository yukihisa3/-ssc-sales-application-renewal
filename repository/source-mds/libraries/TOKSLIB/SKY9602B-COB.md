# SKY9602B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKY9602B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    ユーザ　　　　名：　サカタのたね　　　殿                  *
*    システム　　　名：　在庫管理システム                      *
*    プログラム　　名：　バーコード印刷                        *
*    作成者　　　　　：　ＮＡＶ　　　　　                      *
*    作成日　　　　　：　1993.06.25      UPDATE: 1994.06.23    *
*                                                              *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SKY9602B.
 AUTHOR.             NAV.
 DATE-WRITTEN.       93.06.24.
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
*----<< バーコードファイル  >>---*
     SELECT   ZJANDT         ASSIGN        TO  01-VI-ZJANDT1
                             ORGANIZATION         IS   INDEXED
                             ACCESS MODE          IS   SEQUENTIAL
                             RECORD KEY           IS   JAN-F02
                                                       JAN-F03
                                                       JAN-F011
                                                       JAN-F012
                             FILE STATUS          IS   JAN-STA.
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
     COPY    FKY96021  OF   XMDLIB.
*----<< バーコードファイル  >>---*
 FD  ZJANDT.
     COPY     ZJANCDT  OF  XFDLIB
     JOINING  JAN      AS  PREFIX.
*----<< プリンタＦ >>-----*
 FD  PRTF.
     COPY     FKY96022 OF  XMDLIB.
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
     03  JAN-STA             PIC  X(02).
     03  PRT-STA             PIC  X(02).
 01  WORK-AREA.
     03  MAISU               PIC  9(04)  VALUE  ZERO.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  W-KINGKX.
         05  W-YEN           PIC  N(01)  VALUE  NC"￥".
         05  W-KINGK         PIC  N(06)  VALUE  SPACE.
     03  W-SU                PIC  9(01).
     03  W-RSU     REDEFINES   W-SU.
      05  R-SU               PIC  X(01).
     03  I                   PIC  9(01)  VALUE  ZERO.
     03  J                   PIC  9(01)  VALUE  ZERO.
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
         NC"１または２を入力してください。".
     03  MSG-2                   PIC  N(20)     VALUE
         NC"誤ったＰＦキーが押下されました。".
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SKY9602B".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
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
*バーコードＦ
 JAN-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZJANDT.
     MOVE    "ZJANDT"      TO    ERR-FL-ID.
     MOVE     JAN-STA      TO    ERR-STCD.
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
     DISPLAY  "**  SKY9602B   START  **"   UPON  CONS.
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
     OPEN     INPUT       ZJANDT.
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
     MOVE      LOW-VALUE         TO        FKY96021.
     MOVE      "FKY96021"        TO        DSP-FRM.
     MOVE      "CL"              TO        DSP-TYP.
     MOVE      HEN-DATE          TO        SDATE.
     MOVE      HEN-TIME          TO        STIME.
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
                               (KBN NOT = 1 AND 2)
                      MOVE    MSG-1    TO    MSG
                      MOVE    "R"      TO    EDIT-OPTION OF KBN
                      MOVE    "C"      TO    EDIT-CURSOR OF KBN
                      GO               TO    DSP-020
                 ELSE
                      MOVE    SPACE    TO    MSG
                      MOVE    "D"      TO    EDIT-OPTION OF KBN
                      MOVE    " "      TO    EDIT-CURSOR OF KBN
                END-IF
         WHEN   OTHER
                MOVE    MSG-2    TO        MSG
                GO               TO        DSP-020
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
*
     MOVE   NC"ＮＮＮＮＮＮＮＮＮＮ"  TO  MEI1A   MEI1B
            MEI2A MEI2B MEI3A MEI3B MEI4A MEI4B MEI5A MEI5B.
     MOVE   "9999"     TO       BUMON1  BUMON2  BUMON3 BUMON4
                                BUMON5.
*****MOVE    99999     TO       KINGK1  KINGK2  KINGK3 KINGK4
*****                           KINGK5.
     MOVE   NC"９９，９９９"    TO      W-KINGK.
     MOVE   W-KINGKX   TO       KINGK1  KINGK2  KINGK3 KINGK4
                                KINGK5.
*
     MOVE "FKY96022"             TO  PRT-FMT.
     MOVE  SPACE                 TO  PRT-CTL.
     MOVE  SPACE                 TO  PRT-PRO.
     MOVE "SCREEN"               TO  PRT-GRP.
     WRITE     FKY96022.
*
     CLOSE     PRTF.
     OPEN   OUTPUT   PRTF.
*
 TEST-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
*バーコードＦ　ＲＥＡＤ
     PERFORM   JAN-READ-SEC.
     IF    END-FLG  =  "END"
           GO       TO      MAIN-EXIT
     END-IF.
**
     MOVE      ZERO    TO      MAISU.
     PERFORM   UNTIL   MAISU >=  JAN-F10
        MOVE   SPACE           TO       FKY96022
        MOVE   JAN-F071        TO       MEI1A
        MOVE   JAN-F072        TO       MEI1B
        MOVE   JAN-F08         TO       BUMON1
********MOVE   JAN-F08         TO       KINGK1
        MOVE   JAN-F06         TO       BCD1
        PERFORM    PRICE-HENKAN
        MOVE   W-KINGKX        TO       KINGK1
        ADD      1             TO       MAISU
**
        IF    (MAISU >= JAN-F10) AND (END-FLG = SPACE)
             PERFORM   JAN-READ-SEC
             IF    END-FLG  =  "END"
                   MOVE    9999    TO   MAISU
              ELSE
                   MOVE    ZERO    TO   MAISU
             END-IF
        END-IF
        IF   END-FLG = SPACE
           MOVE   JAN-F071  TO       MEI2A
           MOVE   JAN-F072  TO       MEI2B
           MOVE   JAN-F08   TO       BUMON2
********   MOVE   JAN-F08   TO       KINGK2
           MOVE   JAN-F06   TO       BCD2
           PERFORM    PRICE-HENKAN
           MOVE   W-KINGKX  TO       KINGK2
           ADD     1        TO       MAISU
        END-IF
**
        IF    (MAISU >= JAN-F10) AND (END-FLG = SPACE)
             PERFORM   JAN-READ-SEC
             IF    END-FLG  =  "END"
                   MOVE    9999    TO   MAISU
              ELSE
                   MOVE    ZERO    TO   MAISU
             END-IF
        END-IF
        IF   END-FLG = SPACE
           MOVE   JAN-F071  TO       MEI3A
           MOVE   JAN-F072  TO       MEI3B
           MOVE   JAN-F08   TO       BUMON3
*******    MOVE   JAN-F08   TO       KINGK3
           MOVE   JAN-F06   TO       BCD3
           PERFORM    PRICE-HENKAN
           MOVE   W-KINGKX  TO       KINGK3
           ADD      1       TO       MAISU
        END-IF
**
        IF    (MAISU >= JAN-F10) AND (END-FLG = SPACE)
             PERFORM   JAN-READ-SEC
             IF    END-FLG  =  "END"
                   MOVE    9999    TO   MAISU
              ELSE
                   MOVE    ZERO    TO   MAISU
             END-IF
        END-IF
        IF   END-FLG = SPACE
           MOVE   JAN-F071  TO       MEI4A
           MOVE   JAN-F072  TO       MEI4B
           MOVE   JAN-F08   TO       BUMON4
********   MOVE   JAN-F08   TO       KINGK4
           MOVE   JAN-F06   TO       BCD4
           PERFORM    PRICE-HENKAN
           MOVE   W-KINGKX  TO       KINGK4
           ADD      1       TO       MAISU
        END-IF
**
        IF    (MAISU >= JAN-F10) AND (END-FLG = SPACE)
             PERFORM   JAN-READ-SEC
             IF    END-FLG  =  "END"
                   MOVE    9999    TO   MAISU
              ELSE
                   MOVE    ZERO    TO   MAISU
             END-IF
        END-IF
        IF   END-FLG = SPACE
           MOVE   JAN-F071  TO       MEI5A
           MOVE   JAN-F072  TO       MEI5B
           MOVE   JAN-F08   TO       BUMON5
********   MOVE   JAN-F08   TO       KINGK5
           MOVE   JAN-F06   TO       BCD5
           PERFORM    PRICE-HENKAN
           MOVE   W-KINGKX  TO       KINGK5
           ADD      1       TO       MAISU
        END-IF
**
        IF    (MAISU >= JAN-F10) AND (END-FLG = SPACE)
             PERFORM   JAN-READ-SEC
             IF    END-FLG  =  "END"
                   MOVE    9999    TO   MAISU
              ELSE
                   MOVE    ZERO    TO   MAISU
             END-IF
        END-IF
**出力
        MOVE "FKY96022"             TO  PRT-FMT
        MOVE  SPACE                 TO  PRT-CTL
        MOVE  SPACE                 TO  PRT-PRO
        MOVE "SCREEN"               TO  PRT-GRP
        WRITE     FKY96022
     END-PERFORM.
 MAIN-EXIT.
     EXIT.
*=============================================================
*                バーコードＦＲＥＡＤ処理                    *
*=============================================================
 JAN-READ-SEC      SECTION.
     READ  ZJANDT
           AT   END
           MOVE   "END"    TO     END-FLG
        NOT AT END
           IF   JAN-F11  =  "9"
                GO         TO     JAN-READ-SEC
           END-IF
           IF   JAN-F10  =  ZERO
                GO         TO     JAN-READ-SEC
           END-IF
     END-READ.
 JAN-READ-EXIT.
     EXIT.
*=============================================================
*                金額日本語変換処理
*=============================================================
 PRICE-HENKAN           SECTION.
*2004/05/31 ￥は出力しない START ##
     IF     JAN-F09  >  ZERO
            MOVE  NC"￥"   TO   W-YEN
     ELSE
            MOVE  SPACE    TO   W-YEN
     END-IF.
*2004/05/31 ￥は出力しない END   ##
*
     MOVE   SPACE       TO       W-KINGK.
     MOVE   ZERO        TO       J.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I > 5
        ADD      1          TO       J
        IF       I     =     3
           IF   (JAN-F09(1:1) = ZERO) AND (JAN-F09(2:1) = ZERO)
                 MOVE   SPACE     TO   W-KINGK(J:1)
           ELSE
                 MOVE   NC"，"    TO   W-KINGK(J:1)
           END-IF
           ADD       1            TO   J
        END-IF
        MOVE   JAN-F09(I:1)       TO     R-SU
        EVALUATE    R-SU
          WHEN      0
              MOVE   NC"０"   TO    W-KINGK(J:1)
          WHEN      1
              MOVE   NC"１"   TO    W-KINGK(J:1)
          WHEN      2
              MOVE   NC"２"   TO    W-KINGK(J:1)
          WHEN      3
              MOVE   NC"３"   TO    W-KINGK(J:1)
          WHEN      4
              MOVE   NC"４"   TO    W-KINGK(J:1)
          WHEN      5
              MOVE   NC"５"   TO    W-KINGK(J:1)
          WHEN      6
              MOVE   NC"６"   TO    W-KINGK(J:1)
          WHEN      7
              MOVE   NC"７"   TO    W-KINGK(J:1)
          WHEN      8
              MOVE   NC"８"   TO    W-KINGK(J:1)
          WHEN      9
              MOVE   NC"９"   TO    W-KINGK(J:1)
        END-EVALUATE
     END-PERFORM.
*
     MOVE     ZERO           TO    J
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I > 5
        MOVE     I           TO    J
        IF    JAN-F09(I:1)   NOT =  ZERO
              MOVE     5     TO    I
         ELSE
              IF      I   >=  3
                   ADD    1       TO   J
              END-IF
              MOVE    SPACE       TO   W-KINGK(J:1)
        END-IF
     END-PERFORM.
*
 PRICE-HENKAN-EXIT.
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
     WRITE     FKY96021.
     MOVE    SPACE        TO      DSP-TYP.
 DSP-WRITE-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      DSPF    PRTF    ZJANDT.
     DISPLAY  "**  SKY9602B     END  **"   UPON  CONS.
 END-EXIT.
     EXIT.

```
