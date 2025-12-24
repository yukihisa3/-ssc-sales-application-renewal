# SIT0140L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SIT0140L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＩＴ統制システム　                *
*    業務名　　　　　　　：　ルート条件マスタ                  *
*    モジュール名　　　　：　ルート条件マスタリスト（連携）    *
*    作成日／作成者　　　：　09/03/25-04/01 NAV                *
*    更新日／更新者　　　：                                    *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SIT0140L.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K.
 OBJECT-COMPUTER.       FACOM-K.
 SPECIAL-NAMES.
         STATION   IS   STAT
         YA        IS   NIHONGO
         YB        IS   YB
         YB-21     IS   YB-21
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<  マスタ更新履歴ファイル　>>**************************
     SELECT   MSTLOGF   ASSIGN  TO   DA-01-VI-MSTLOGL3
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  SEQUENTIAL
                        RECORD  KEY          IS  LOG-F01
                                                 LOG-F02
                                                 LOG-F03
                                                 LOG-F05
                                                 LOG-F06
                                                 LOG-F07
                        FILE STATUS          IS   LOG-STATUS.
****<< 担当者マスタファイル >>******************************
     SELECT   HTANMS    ASSIGN  TO   DA-01-VI-TANMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   TAN-F01
                                                  TAN-F02
                        FILE    STATUS       IS   TAN-STATUS.
****<<  取引先マスタファイル    >>*****************************
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-STATUS.
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  マスタ更新履歴ファイル　>>**************************
 FD    MSTLOGF
       LABEL     RECORD     IS   STANDARD.
       COPY      MSTLOGF    OF  XFDLIB
       JOINING   LOG        AS  PREFIX.
****<< 担当者マスタファイル >>******************************
 FD    HTANMS
       BLOCK     CONTAINS   2   RECORDS.
       COPY      HTANMS     OF  XFDLIB
       JOINING   TAN        AS  PREFIX.
*取引先マスタ
****<< 取引先マスタファイル >>******************************
 FD    HTOKMS.
       COPY      HTOKMS     OF  XFDLIB
       JOINING   TOK        AS  PREFIX.
****<<  プリントファイル  >>********************************
 FD    PRINTF    LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  画面制御項目            ****
*01  DSP-CONTROL.
*    02 DSP-PROC             PIC  X(2).
*    02 DSP-GROUP            PIC  X(8).
*    02 DSP-FORMAT           PIC  X(8).
*    02 DSP-STATUS           PIC  X(2).
*    02 DSP-FUNC             PIC  X(4).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 LOG-STATUS           PIC  X(2).
     02 TAN-STATUS           PIC  X(2).
     02 TOK-STATUS           PIC  X(2).
****  カウンタ                ****
 01  L-CNT                   PIC  9(2).
 01  P-CNT                   PIC  9(3).
 01  READ-CNT                PIC  9(07)  VALUE  0.
 01  IN-CNT                  PIC  9(07)  VALUE  0.
****  フラグ                  ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
***  ルート条件マスタ用履歴エリア
     COPY  JHMRUTL1  OF  XFDLIB
     JOINING  WKJRU  AS      PREFIX.
*----------------------------------------------*
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
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(34)  VALUE  SPACE.
     02  FILLER              PIC  N(19)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＊＊＊　ルート条件マスタリスト　＊＊＊".
     02  FILLER              PIC  X(15)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"処理日".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-YY                PIC  9(04).
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-MM                PIC  Z9.
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-DD                PIC  Z9.
     02  FILLER              PIC  X(3)   VALUE  SPACE.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"頁".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  PAGE-SUU            PIC  ZZ9.
****  見出し行１２　１４　１９***
 01  MIDASI-12.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(22)  VALUE
         "+------+------+------+".
****  見出し行１３           ****
 01  MIDASI-13.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"責任者".
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"担当者".
     02  FILLER              PIC  X(01)  VALUE  "!".
***  見出し行１５ １６ １７ １８
 01  MIDASI-15.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
*
****  見出し行２             ****
 01  MIDASI-2       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(11)  VALUE  SPACE.
     02  FILLER              PIC  N(12)  VALUE
         NC"取引先　取引先名（漢字）".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"分類".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"ルート".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"振分倉庫".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"ルート名称".
     02  FILLER              PIC  X(19)  VALUE  SPACE.
****  見出し行２１           ****
 01  MIDASI-21      CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(07)  VALUE
         NC"［処理：担当者".
     02  FILLER              PIC  X(14)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"：更新日".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"更新時刻］".
 01  SEN1.
     03  FILLER                  PIC  X(136)  VALUE
         ALL "-".
****  明細行１               ****
 01  MEISAI-1       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  JRU-01              PIC  9(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TORINM              PIC  N(15).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  JRU-02              PIC  X(04).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  JRU-03              PIC  X(08).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  JRU-05              PIC  X(02).
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  JRU-04              PIC  N(10).
****  明細行２               ****
 01  MEISAI-2       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-201             PIC  N(04).
     02  MEI-202             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-203             PIC  N(11).
     02  MEI-204             PIC  X(10).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-205             PIC  X(08).
     02  FILLER              PIC  N(01)  VALUE  NC"］".
**** エラーメッセージ         ****
*01  ERR-TAB.
*    02  MSG-ERR1            PIC  N(30)  VALUE
*           NC"無効ＰＦキーです。".
*    02  MSG-ERR2            PIC  N(30)  VALUE
*           NC"開始・終了コードの関係に誤りがあります。".
*    02  PMSG01              PIC N(20) VALUE
*           NC"_取消　_終了".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SIT0140L".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*帳票　更新履歴編集
 01  HEN-KUBUN.
     03  FILLER                   PIC  N(01)  VALUE NC"［".
     03  HEN-KUBUNNM              PIC  N(02).
     03  FILLER                   PIC  N(01)  VALUE NC"：".
 01  HEN-TAN.
     03  HEN-TANNM                PIC  N(10).
     03  FILLER                   PIC  N(01)  VALUE NC"：".
*担当者コード
 01  WK-TANCD.
     03  WK-TANCD1         PIC  X(02).
     03  WK-FILLER         PIC  X(06).
*--------------------------------------------------------*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*--------------------------------------------------------*
 LINKAGE          SECTION.
 01  PARA-BUMONCD          PIC X(04).
*01  PARA-TANCD            PIC X(02).
 01  PARA-TANCD            PIC X(08).
*01  PARA-MKUBUN           PIC 9(02).
 01  PARA-UPDDATES         PIC 9(08).
 01  PARA-UPDTIMES         PIC 9(06).
 01  PARA-UPDDATEE         PIC 9(08).
 01  PARA-UPDTIMEE         PIC 9(06).
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION  USING PARA-BUMONCD PARA-TANCD
                        PARA-UPDDATES  PARA-UPDTIMES
                        PARA-UPDDATEE  PARA-UPDTIMEE.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HTANMS.
     MOVE   "HTANMS  "        TO    ERR-FL-ID.
     MOVE    TAN-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  MSTLOGF.
     MOVE   "MSTLOGL3"        TO    ERR-FL-ID.
     MOVE    LOG-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 TOK-ERR                   SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HTOKMS.
     MOVE   "TOKMS2  "        TO    ERR-FL-ID.
     MOVE    TOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000      TO        PROGRAM-STATUS.
     STOP    RUN.
 END     DECLARATIVES.
************************************************************
 SIT0140L-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 SIT0140L-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
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
*
     OPEN     INPUT     MSTLOGF  HTANMS.
     OPEN     INPUT     HTOKMS.
     OPEN     OUTPUT    PRINTF.
     MOVE    "0"        TO     DSP-FLG.
     MOVE     62        TO     L-CNT.
     MOVE     1         TO     P-CNT.
*
*** マスタ更新履歴ＳＴＡＲＴ
     MOVE     "06"              TO    LOG-F01.
     MOVE     PARA-BUMONCD      TO    LOG-F02.
     MOVE     PARA-TANCD        TO    LOG-F03.
     MOVE     PARA-UPDDATES     TO    LOG-F05.
     MOVE     PARA-UPDTIMES     TO    LOG-F06.
     MOVE     ZERO              TO    LOG-F07.
*
     START    MSTLOGF   KEY  IS  >=  LOG-F01
                                     LOG-F02
                                     LOG-F03
                                     LOG-F05
                                     LOG-F06
                                     LOG-F07
              INVALID   MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END
     END-START.
     PERFORM       READ-LOG-SEC.
*
 INIT-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
     IF       L-CNT         >=      62
              PERFORM       MIDASI-SEC
     END-IF.
*更新履歴
     EVALUATE    LOG-F04
         WHEN    "1"
                 MOVE NC"登録"          TO   HEN-KUBUNNM
         WHEN    "2"
                 MOVE NC"修正"          TO   HEN-KUBUNNM
         WHEN    "3"
                 MOVE NC"削除"          TO   HEN-KUBUNNM
     END-EVALUATE.
     MOVE        HEN-KUBUN              TO   MEI-201.
*
     MOVE        LOG-F03                TO   WK-TANCD.
     MOVE        WK-TANCD1              TO   MEI-202.
*担当者名取得
     PERFORM     READ-TAN-SEC.
*
     MOVE        TAN-F03                TO  HEN-TANNM.
     MOVE        HEN-TAN                TO  MEI-203.
     MOVE        LOG-F05(1:4)           TO  HEN-DATE-YYYY.
     MOVE        LOG-F05(5:2)           TO  HEN-DATE-MM.
     MOVE        LOG-F05(7:2)           TO  HEN-DATE-DD.
     MOVE        HEN-DATE               TO  MEI-204.
*
     MOVE        LOG-F06(1:2)           TO  HEN-TIME-HH.
     MOVE        LOG-F06(3:2)           TO  HEN-TIME-MM.
     MOVE        LOG-F06(5:2)           TO  HEN-TIME-SS.
     MOVE        HEN-TIME               TO  MEI-205.
*
     MOVE   WKJRU-F01         TO      JRU-01.
     PERFORM  TOK-RD-SEC.
     MOVE   WKJRU-F02         TO      JRU-02.
     MOVE   WKJRU-F03         TO      JRU-03.
     MOVE   WKJRU-F04         TO      JRU-04.
     MOVE   WKJRU-F05         TO      JRU-05.
     WRITE    P-REC         FROM    MEISAI-1    AFTER  1.
     WRITE    P-REC         FROM    MEISAI-2    AFTER  1.
     ADD      2             TO      L-CNT.
*
     PERFORM  READ-LOG-SEC.
 MAIN-END.
     EXIT.
************************************************************
*      2.1       見出し処理                                *
************************************************************
 MIDASI-SEC             SECTION.
     MOVE     HEN-DATE-YYYY      TO      H-YY.
     MOVE     HEN-DATE-MM        TO      H-MM.
     MOVE     HEN-DATE-DD        TO      H-DD.
     MOVE     P-CNT              TO      PAGE-SUU.
     IF       P-CNT  = 1
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    MIDASI-12  AFTER  1
              WRITE      P-REC   FROM    MIDASI-13  AFTER  1
              WRITE      P-REC   FROM    MIDASI-12  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-12  AFTER  1
              WRITE      P-REC   FROM    MIDASI-2   AFTER  2
              WRITE      P-REC   FROM    MIDASI-21  AFTER  1
              WRITE      P-REC   FROM    SEN1       AFTER  1
        ELSE
              MOVE       SPACE   TO      P-REC
              WRITE      P-REC   AFTER   PAGE
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    MIDASI-12  AFTER  1
              WRITE      P-REC   FROM    MIDASI-13  AFTER  1
              WRITE      P-REC   FROM    MIDASI-12  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-12  AFTER  1
              WRITE      P-REC   FROM    MIDASI-2   AFTER  2
              WRITE      P-REC   FROM    MIDASI-21  AFTER  1
              WRITE      P-REC   FROM    SEN1       AFTER  1
     END-IF.
     ADD      1  TO      P-CNT.
     MOVE    14  TO      L-CNT.
 MIDASI-END.
     EXIT.
************************************************************
*      2.2.1     履歴ファイル読込み処理                    *
************************************************************
 READ-LOG-SEC              SECTION.
*
     READ     MSTLOGF   NEXT
              AT END    MOVE  "END"  TO  END-FLG
              GO             TO   READ-LOG-END
     END-READ.
**
     ADD     1    TO   READ-CNT.
***  マスタ区分のチェック
     IF       LOG-F01  NOT =  "06"
              MOVE  "END"    TO   END-FLG
              GO             TO   READ-LOG-END
     END-IF.
***  部門CD範囲のチェック
     IF       LOG-F02    >   PARA-BUMONCD
              MOVE  "END"    TO   END-FLG
              GO             TO   READ-LOG-END
     END-IF.
***  担当者CD範囲のチェック
*****IF       LOG-F03(1:2)  =   PARA-TANCD
     IF       LOG-F03       =   PARA-TANCD
              CONTINUE
     ELSE
              MOVE  "END"    TO   END-FLG
              GO             TO   READ-LOG-END
     END-IF.
***  更新日範囲のチェック
     IF   ( LOG-F05  <=   PARA-UPDDATEE )  AND
          ( LOG-F06  <=   PARA-UPDTIMEE )
              CONTINUE
     ELSE
              GO             TO   READ-LOG-SEC
     END-IF.
**
     ADD     1         TO  IN-CNT.
     MOVE    LOG-F08   TO  WKJRU-REC.
**
 READ-LOG-END.
     EXIT.
************************************************************
*      2.2.2     担当者マスタ読込み処理                    *
************************************************************
 READ-TAN-SEC              SECTION.
*
     MOVE    LOG-F02       TO  TAN-F01.
     MOVE    LOG-F03       TO  TAN-F02.
     READ     HTANMS
         INVALID
              MOVE   ALL NC"＊" TO    TAN-F03
              MOVE   ALL "*"    TO    TAN-F04
     END-READ.
**
 READ-TAN-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE  MSTLOGF  PRINTF   HTANMS    HTOKMS.
***
     DISPLAY  "SIT0140L READ ="  READ-CNT  UPON  CONS.
     DISPLAY  "SIT0140L IN   ="  IN-CNT    UPON  CONS.
 END-END.
     EXIT.
****************************************************************
*  取引先マスタより取引先名取得                                *
****************************************************************
 TOK-RD-SEC             SECTION.
*
     MOVE   WKJRU-F01        TO        TOK-F01.
     READ     HTOKMS         INVALID
              MOVE ALL NC"＊" TO       TORINM
     NOT  INVALID
              MOVE TOK-F02   TO        TORINM
     END-READ.
*
 TOK-RD-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
