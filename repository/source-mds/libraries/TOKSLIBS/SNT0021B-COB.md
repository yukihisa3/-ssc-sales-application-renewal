# SNT0021B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNT0021B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　入荷管理システム　　　　　　　　　*
*    モジュール名　　　　：　入荷実績抽出処理　　　　          *
*    作成日／更新日　　　：　12/06/29                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタを受取、実績を抽出する。　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SNT0021B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 入庫ファイル         >>******************************
     SELECT   NYKFILF   ASSIGN  TO   DA-01-VI-NYKFILL2
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   SEQUENTIAL
                        RECORD  KEY          IS   NYUK-F27
                                                  NYUK-F02
                                                  NYUK-F03
                                                  NYUK-F04
                                                  NYUK-F05
                        FILE    STATUS       IS   NYUK-STATUS.
****<< 商品名称マスタ       >>******************************
     SELECT   HMEIMS    ASSIGN  TO   DA-01-VI-MEIMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   MEI-F011
                                                  MEI-F012
                        FILE    STATUS       IS   MEI-STATUS.
****<< 仕入先マスタ         >>******************************
     SELECT   ZSHIMS    ASSIGN  TO   DA-01-VI-ZSHIMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   SHI-F01
                        FILE    STATUS       IS   SHI-STATUS.
****<< 取引先マスタ         >>******************************
     SELECT   HTOKMS    ASSIGN  TO   DA-01-VI-TOKMS2
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   TOK-F01
                        FILE    STATUS       IS   TOK-STATUS.
****<< 倉庫マスタ           >>******************************
     SELECT   ZSOKMS    ASSIGN  TO   DA-01-VI-ZSOKMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   SOK-F01
                        FILE    STATUS       IS   SOK-STATUS.
****<< 入荷実績抽出         >>******************************
     SELECT   NJISCSVF  ASSIGN  TO   NJISCSVF
                        FILE    STATUS       IS   JIS-STATUS.
 DATA                   DIVISION.
 FILE                   SECTION.
****<< 入庫ファイル         >>******************************
 FD    NYKFILF
       LABEL     RECORD    IS    STANDARD.
       COPY      NYKFILF   OF    XFDLIB
                 JOINING   NYUK  PREFIX.
****<< 商品名称マスタ       >>******************************
 FD    HMEIMS
       LABEL     RECORD    IS    STANDARD.
       COPY      HMEIMS    OF    XFDLIB
                 JOINING   MEI   PREFIX.
****<< 仕入先マスタ         >>******************************
 FD    ZSHIMS
       LABEL     RECORD    IS    STANDARD.
       COPY      ZSHIMS    OF    XFDLIB
                 JOINING   SHI   PREFIX.
****<< 取引先マスタ         >>******************************
 FD    HTOKMS
       LABEL     RECORD    IS    STANDARD.
       COPY      HTOKMS    OF    XFDLIB
                 JOINING   TOK   PREFIX.
****<< 倉庫マスタ           >>******************************
 FD    ZSOKMS
       LABEL     RECORD    IS    STANDARD.
       COPY      ZSOKMS    OF    XFDLIB
                 JOINING   SOK   PREFIX.
****<< 入荷実績抽出         >>******************************
 FD    NJISCSVF
       LABEL     RECORD    IS    STANDARD.
       COPY      NJISCSVF  OF    XFDLIB
                 JOINING   JIS   PREFIX.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 NYUK-STATUS          PIC  X(02).
     02 MEI-STATUS           PIC  X(02).
     02 SHI-STATUS           PIC  X(02).
     02 TOK-STATUS           PIC  X(02).
     02 SOK-STATUS           PIC  X(02).
     02 JIS-STATUS           PIC  X(02).
***  日付／時刻
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
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(06).
     02  SYSYMD-R            REDEFINES SYSYMD.
       03  SYS-YY            PIC  99.
       03  SYS-MM            PIC  99.
       03  SYS-DD            PIC  99.
****  カウンタ                ****
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)  VALUE  ZERO.
     02  WRITE-CNT           PIC  9(07)  VALUE  ZERO.
****  フラグエリア            ****
 01  FLG-AREA.
     02  HMEIMS-INV-FLG      PIC  X(03)  VALUE  SPACE.
     02  ZSHIMS-INV-FLG      PIC  X(03)  VALUE  SPACE.
     02  HTOKMS-INV-FLG      PIC  X(03)  VALUE  SPACE.
     02  ZSOKMS-INV-FLG      PIC  X(03)  VALUE  SPACE.
     02  END-FLG             PIC  X(03)  VALUE  SPACE.
***  日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SNY0020L".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
**** パラメータ               ****
 LINKAGE                SECTION.
 01  LINK-S-DATE        PIC  9(08).
 01  LINK-E-DATE        PIC  9(08).
 01  LINK-SOKCD         PIC  X(02).
 01  LINK-SIRCD1        PIC  9(08).
 01  LINK-SIRCD2        PIC  9(08).
 01  LINK-SIRCD3        PIC  9(08).
 01  LINK-SIRCD4        PIC  9(08).
 01  LINK-SIRCD5        PIC  9(08).
 01  LINK-SIRCD6        PIC  9(08).
 01  LINK-SIRCD7        PIC  9(08).
 01  LINK-SIRCD8        PIC  9(08).
 01  LINK-SIRCD9        PIC  9(08).
 01  LINK-SIRCD10       PIC  9(08).
 01  LINK-TORCD1        PIC  9(08).
 01  LINK-TORCD2        PIC  9(08).
 01  LINK-TORCD3        PIC  9(08).
 01  LINK-TORCD4        PIC  9(08).
 01  LINK-TORCD5        PIC  9(08).
 01  LINK-TORCD6        PIC  9(08).
 01  LINK-TORCD7        PIC  9(08).
 01  LINK-TORCD8        PIC  9(08).
 01  LINK-TORCD9        PIC  9(08).
 01  LINK-TORCD10       PIC  9(08).
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION      USING   LINK-S-DATE
                                              LINK-E-DATE
                                              LINK-SOKCD
                                              LINK-SIRCD1
                                              LINK-SIRCD2
                                              LINK-SIRCD3
                                              LINK-SIRCD4
                                              LINK-SIRCD5
                                              LINK-SIRCD6
                                              LINK-SIRCD7
                                              LINK-SIRCD8
                                              LINK-SIRCD9
                                              LINK-SIRCD10
                                              LINK-TORCD1
                                              LINK-TORCD2
                                              LINK-TORCD3
                                              LINK-TORCD4
                                              LINK-TORCD5
                                              LINK-TORCD6
                                              LINK-TORCD7
                                              LINK-TORCD8
                                              LINK-TORCD9
                                              LINK-TORCD10.
*
 DECLARATIVES.
**入庫ファイル
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NYKFILF.
     MOVE   "NYKFILF "        TO    ERR-FL-ID.
     MOVE    NYUK-STATUS      TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**商品名称マスタ
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HMEIMS.
     MOVE   "HMEIMS  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**仕入先マスタ
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSHIMS.
     MOVE   "ZSHIMS  "        TO    ERR-FL-ID.
     MOVE    SHI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**取引先マスタ
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HTOKMS.
     MOVE   "HTOKMS  "        TO    ERR-FL-ID.
     MOVE    TOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**倉庫マスタ
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS  "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**入荷実績抽出Ｆ
 FILEERR-SEC6           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NJISCSVF.
     MOVE   "NJISCSV1"        TO    ERR-FL-ID.
     MOVE    JIS-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
 FNY00201B-START         SECTION.
     DISPLAY  "**  SNT0021B   START  **"   UPON  CONS.
*
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC      UNTIL  END-FLG  =  "END".
     PERFORM       END-SEC.
*
     DISPLAY  "**  SNT0021B    END   **"   UPON  CONS.
     STOP     RUN.
 FNY00201B-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     NYKFILF.
     OPEN     INPUT     HMEIMS.
     OPEN     INPUT     ZSHIMS.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     ZSOKMS.
     OPEN     EXTEND    NJISCSVF.
*---- システム日付・時刻の取得
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
*入庫ファイル読み込み
     PERFORM   NYKFILF-READ-SEC.
 INIT-SEC-EXIT.
     EXIT.
************************************************************
*      1.3       入庫ファイル　ＲＥＡＤ処理                *
************************************************************
 NYKFILF-READ-SEC       SECTION.
*
     READ    NYKFILF
       AT  END
           MOVE     "END"        TO   END-FLG
           GO       TO   NYKFILF-READ-EXIT
     END-READ.
*
     ADD      1          TO   READ-CNT.
*
     IF    READ-CNT(5:3)  =  "000" OR "500"
           DISPLAY "READ-CNT = " READ-CNT  UPON CONS
     END-IF.
*倉庫ＣＤチェック
     IF   LINK-SOKCD  NOT =  SPACE
          IF   LINK-SOKCD  NOT =   NYUK-F27
               GO                    TO   NYKFILF-READ-SEC
          END-IF
     END-IF.
*仕入先ＣＤチェック
     IF  LINK-SIRCD1  = NYUK-F08
     OR  LINK-SIRCD2  = NYUK-F08
     OR  LINK-SIRCD3  = NYUK-F08
     OR  LINK-SIRCD4  = NYUK-F08
     OR  LINK-SIRCD5  = NYUK-F08
     OR  LINK-SIRCD6  = NYUK-F08
     OR  LINK-SIRCD7  = NYUK-F08
     OR  LINK-SIRCD8  = NYUK-F08
     OR  LINK-SIRCD9  = NYUK-F08
     OR  LINK-SIRCD10 = NYUK-F08
         CONTINUE
     ELSE
         GO             TO   NYKFILF-READ-SEC
     END-IF.
*取引先ＣＤチェック
     IF  NYUK-F09  NOT =  ZERO
         IF  LINK-TORCD1  = NYUK-F09
         OR  LINK-TORCD2  = NYUK-F09
         OR  LINK-TORCD3  = NYUK-F09
         OR  LINK-TORCD4  = NYUK-F09
         OR  LINK-TORCD5  = NYUK-F09
         OR  LINK-TORCD6  = NYUK-F09
         OR  LINK-TORCD7  = NYUK-F09
         OR  LINK-TORCD8  = NYUK-F09
         OR  LINK-TORCD9  = NYUK-F09
         OR  LINK-TORCD10 = NYUK-F09
             CONTINUE
         ELSE
             GO             TO   NYKFILF-READ-SEC
         END-IF
     ELSE
             GO             TO   NYKFILF-READ-SEC
     END-IF.
*日付範囲チェック
     IF  NYUK-F26  >=  LINK-S-DATE
     AND NYUK-F26  <=  LINK-E-DATE
         CONTINUE
     ELSE
         GO             TO   NYKFILF-READ-SEC
     END-IF.
*
 NYKFILF-READ-EXIT.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
*
     MOVE     SPACE          TO     JIS-REC.
     INITIALIZE                     JIS-REC.
*伝票区分
     MOVE     NYUK-F01       TO     JIS-F01.
*伝票番号
     MOVE     NYUK-F02       TO     JIS-F02.
*枝番
     MOVE     NYUK-F03       TO     JIS-F03.
*相殺区分
     MOVE     NYUK-F04       TO     JIS-F04.
*行番号
     MOVE     NYUK-F05       TO     JIS-F05.
*完了区分
     MOVE     NYUK-F06       TO     JIS-F06.
*量販店伝票番号
     MOVE     NYUK-F07       TO     JIS-F07.
*仕入先ＣＤ
     MOVE     NYUK-F08       TO     JIS-F08.
*仕入先名
     MOVE     NYUK-F08       TO     SHI-F01.
     PERFORM  ZSHIMS-READ-SEC.
     IF       ZSHIMS-INV-FLG = "INV"
              MOVE ALL NC"＊" TO    JIS-F09
     ELSE
              MOVE SHI-F02    TO    JIS-F09
     END-IF.
*取引先ＣＤ
     MOVE     NYUK-F09       TO     JIS-F10.
*取引先名
     MOVE     NYUK-F09       TO     TOK-F01.
     PERFORM  HTOKMS-READ-SEC.
     IF       HTOKMS-INV-FLG = "INV"
              MOVE ALL NC"＊" TO    JIS-F11
     ELSE
              MOVE TOK-F02    TO    JIS-F11
     END-IF.
*納入先ＣＤ
     MOVE     NYUK-F12       TO     JIS-F12.
*発注日
     MOVE     NYUK-F13       TO     JIS-F13.
*納品日
     MOVE     NYUK-F14       TO     JIS-F14.
*サカタ商品ＣＤ
     MOVE     NYUK-F15       TO     JIS-F15.
*サカタ品単ＣＤ
     MOVE     NYUK-F16       TO     JIS-F16.
*商品名１
*商品名２
     MOVE     NYUK-F15       TO     MEI-F011.
     MOVE     NYUK-F16       TO     MEI-F012.
     PERFORM  HMEIMS-READ-SEC.
     IF       HMEIMS-INV-FLG = "INV"
              MOVE ALL NC"＊" TO    JIS-F17
              MOVE SPACE      TO    JIS-F18
     ELSE
              MOVE MEI-F021   TO    JIS-F17(1:15)
              MOVE MEI-F022   TO    JIS-F17(16:15)
              MOVE SPACE      TO    JIS-F18
     END-IF.
*_番
     MOVE     NYUK-F17       TO     JIS-F19.
*入庫数
     MOVE     NYUK-F18       TO     JIS-F20.
     IF       NYUK-F01 = "51"
              COMPUTE JIS-F20 = JIS-F20 * -(1)
     END-IF.
*単価区分
     MOVE     NYUK-F19       TO     JIS-F21.
*仕入単価
     MOVE     NYUK-F20       TO     JIS-F22.
*単価区分２
     MOVE     NYUK-F21       TO     JIS-F23.
*原価単価
     MOVE     NYUK-F22       TO     JIS-F24.
*売価単価
     MOVE     NYUK-F23       TO     JIS-F25.
*実納品日
     MOVE     NYUK-F26       TO     JIS-F26.
*倉庫ＣＤ
     MOVE     NYUK-F27       TO     JIS-F27.
*倉庫名
     MOVE     NYUK-F27       TO     SOK-F01.
     PERFORM  ZSOKMS-READ-SEC.
     IF       ZSOKMS-INV-FLG = "INV"
              MOVE ALL NC"＊" TO    JIS-F28
     ELSE
              MOVE SOK-F02    TO    JIS-F28
     END-IF.
*計上フラグ
     MOVE     NYUK-F30       TO     JIS-F29.
*計上済フラグ
     MOVE     NYUK-F31       TO     JIS-F30.
*担当者
     MOVE     NYUK-F33       TO     JIS-F31.
*支払締日
     MOVE     NYUK-F36       TO     JIS-F32.
*
     WRITE  JIS-REC.
*
     ADD    1                TO     WRITE-CNT.
*
     PERFORM  NYKFILF-READ-SEC.
*
 MAIN-SEC-EXIT.
     EXIT.
************************************************************
*      商品名称マスタ索引
************************************************************
 HMEIMS-READ-SEC        SECTION.
*
     READ  HMEIMS
           INVALID      MOVE "INV"     TO   HMEIMS-INV-FLG
           NOT  INVALID MOVE SPACE     TO   HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
************************************************************
*      仕入先マスタ索引
************************************************************
 ZSHIMS-READ-SEC        SECTION.
*
     READ  ZSHIMS
           INVALID      MOVE "INV"     TO   ZSHIMS-INV-FLG
           NOT  INVALID MOVE SPACE     TO   ZSHIMS-INV-FLG
     END-READ.
*
 ZSHIMS-READ-EXIT.
     EXIT.
************************************************************
*      取引先マスタ索引
************************************************************
 HTOKMS-READ-SEC        SECTION.
*
     READ  HTOKMS
           INVALID      MOVE "INV"     TO   HTOKMS-INV-FLG
           NOT  INVALID MOVE SPACE     TO   HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
************************************************************
*      倉庫マスタ索引
************************************************************
 ZSOKMS-READ-SEC        SECTION.
*
     READ  ZSOKMS
           INVALID      MOVE "INV"     TO   ZSOKMS-INV-FLG
           NOT  INVALID MOVE SPACE     TO   ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
*
     CLOSE    NYKFILF  HMEIMS
              ZSHIMS   HTOKMS   ZSOKMS   NJISCSVF.
*
     DISPLAY "* NYKFILF (IN)=" READ-CNT   " *" UPON CONS.
     DISPLAY "* NJISCSVF(OT)=" WRITE-CNT  " *" UPON CONS.
 END-SEC-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
