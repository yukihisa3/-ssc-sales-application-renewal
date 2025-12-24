# SIT0160L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SIT0160L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　倉庫マスタリスト                  *
*    作成日／更新日　　　：　09/03/24-04/01                    *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　倉庫マスタリストの出力を行う      *
*    更新日　　　　　　　：　2011/04/14 T.TAKAHASHI            *
*    　　　　　　　　　　：　出荷送信区分追加                  *
*    更新日　　　　　　　：　2011/05/31 T.TAKAHASHI            *
*    　　　　　　　　　　：　出荷送信区分追加（手書）          *
*    更新日　　　　　　　：　2012/07/24 T.TAKAHASHI            *
*    　　　　　　　　　　：　小売連携区分追加　　　　          *
*    更新日　　　　　　　：　2012/10/09 T.TAKAHASHI            *
*    　　　　　　　　　　：　物流連携区分追加　　　　          *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SIT0160L.
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
****<<  マスタ更新履歴ファイル  >>**************************
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
****<<  プリントファイル  >>********************************
 FD    PRINTF    LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 LOG-STATUS           PIC  X(2).
     02 TAN-STATUS           PIC  X(2).
****  カウンタ                ****
 01  L-CNT                   PIC  9(2).
 01  P-CNT                   PIC  9(3).
 01  READ-CNT                PIC  9(07)  VALUE  0.
 01  IN-CNT                  PIC  9(07)  VALUE  0.
****  フラグ                  ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
***  倉庫マスタ用履歴エリア
     COPY  ZSOKMS   OF  XFDLIB
     JOINING  WKSOK     AS   PREFIX.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
****  日付保存                ****
*01  SYSTEM-HIZUKE.
*    02  SYSYMD              PIC  9(6).
*    02  SYSYMD-R            REDEFINES SYSYMD.
*      03  SYS-YY            PIC  99.
*      03  SYS-MM            PIC  99.
*      03  SYS-DD            PIC  99.
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
     02  FILLER              PIC  N(16)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＊＊＊　倉庫マスタリスト　＊＊＊".
     02  FILLER              PIC  X(18)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"処理日".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-YY                PIC  99.
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-MM                PIC  Z9.
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-DD                PIC  Z9.
     02  FILLER              PIC  X(3)   VALUE  SPACE.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"頁".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  PAGE-SUU            PIC  ZZ9.
     02  FILLER              PIC  X(8)   VALUE  SPACE.
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
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(10)  VALUE
         NC"倉庫　倉庫名（漢字）".
     02  FILLER              PIC  X(14)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE
         NC"_".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"住　所　１".
     02  FILLER              PIC  X(21)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"住　所　２".
     02  FILLER              PIC  X(21)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"ＴＥＬ".
***  97.03.11 ST  ***
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"運転".
***  97.03.11 ED  ***
****  見出し行３             ****
 01  MIDASI-3       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(18)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"（カナ）".
     02  FILLER              PIC  X(14)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"県コード".
     02  FILLER              PIC  X(64)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"ＦＡＸ".
***  97.03.11 ST  ***
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"手段".
***  97.03.11 ED  ***
****  見出し行４　           ****
 01  MIDASI-4       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(07)  VALUE
         NC"［処理：担当者".
     02  FILLER              PIC  X(14)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"：更新日".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"更新時刻］".
****  明細行１               ****
 01  MEISAI-1       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  SOK-01              PIC  X(02).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  SOK-02              PIC  N(18).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
*****02  SOK-04              PIC  X(05).
     02  SOK-04              PIC  X(08).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  SOK-05              PIC  N(18).
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  SOK-06              PIC  N(18).
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  SOK-07              PIC  X(15).
****  明細行２               ****
 01  MEISAI-2.
     02  FILLER              PIC  X(12)  VALUE  SPACE.
     02  SOK-03              PIC  X(25).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  SOK-09              PIC  9(02).
     02  FILLER              PIC  X(70)  VALUE  SPACE.
     02  SOK-08              PIC  X(15).
***  97.03.11 ST  ***
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  SOK-10              PIC  X(02).
***  97.03.11 ED  ***
****  明細行３               ****
 01  MEISAI-3       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-301             PIC  N(04).
     02  MEI-302             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-303             PIC  N(11).
     02  MEI-304             PIC  X(10).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-305             PIC  X(08).
     02  FILLER              PIC  N(01)  VALUE  NC"］".
*2011/04/14 ST T.TAKAHASHI
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-SYUSND          PIC  N(05).
     02  MEI-SYUSND-CD       PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-SYUSND-NM       PIC  N(01).
*2011/04/14 ED T.TAKAHASHI
*2011/05/31 ST T.TAKAHASHI
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-TEGSND          PIC  N(05).
     02  MEI-TEGSND-CD       PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-TEGSND-NM       PIC  N(01).
*2011/05/31 ED T.TAKAHASHI
*2012/07/24 ST T.TAKAHASHI
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-KOURI           PIC  N(05).
     02  MEI-KOURI-CD        PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-KOURI-NM        PIC  N(01).
*2012/07/24 ED T.TAKAHASHI
*2012/10/09 ST T.TAKAHASHI
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-BUTUR           PIC  N(05).
     02  MEI-BUTUR-CD        PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-BUTUR-NM        PIC  N(01).
*2012/10/09 ED T.TAKAHASHI
**** 線１
 01  SEN1.
     02  WK-SEN1             OCCURS  136.
         03  FILLER          PIC  X(01)  VALUE  "=".
**** 線２
 01  SEN2.
     02  WK-SEN2             OCCURS  136.
         03  FILLER          PIC  X(01)  VALUE  "-".
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SIT0160L".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
**
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
 LINKAGE          SECTION.
 01  PARA-BUMONCD          PIC X(04).
*01  PARA-TANCD            PIC X(02).
 01  PARA-TANCD            PIC X(08).
*01  PARA-MKUBUN           PIC 9(02).   **DELETE
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
 END     DECLARATIVES.
************************************************************
 SIT0160L-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 SIT0160L-END.
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
     OPEN     INPUT     MSTLOGF.
     OPEN     INPUT     HTANMS.
     OPEN     OUTPUT    PRINTF.
     MOVE    "0"        TO     DSP-FLG.
     MOVE     62        TO     L-CNT.
     MOVE     1         TO     P-CNT.
***@ DISPLAY  "PARA-BUMONCD="  PARA-BUMONCD  UPON  CONS.
***@ DISPLAY  "PARA-TANCD  ="  PARA-TANCD    UPON  CONS.
***@ DISPLAY  "PARA-UPDDATES="  PARA-UPDDATES UPON  CONS.
***@ DISPLAY  "PARA-UPDTIMES="  PARA-UPDTIMES UPON  CONS.
***@ DISPLAY  "PARA-UPDDATEE="  PARA-UPDDATEE  UPON  CONS.
***@ DISPLAY  "PARA-UPDTIMEE="  PARA-UPDTIMEE  UPON  CONS.
**
*** マスタ更新履歴ＳＴＡＲＴ
     MOVE     "07"      TO     LOG-F01.
     MOVE     PARA-BUMONCD     TO  LOG-F02.
     MOVE     PARA-TANCD       TO  LOG-F03.
     MOVE     PARA-UPDDATES    TO  LOG-F05.
     MOVE     PARA-UPDTIMES    TO  LOG-F06.
     MOVE     ZERO             TO  LOG-F07.
*
     START    MSTLOGF   KEY  IS  >=  LOG-F01
                                     LOG-F02
                                     LOG-F03
                                     LOG-F05
                                     LOG-F06
                                     LOG-F07
              INVALID   MOVE  "END"  TO  END-FLG
       DISPLAY  "SIT0160L START INVALID"  UPON  CONS
                        GO   TO   INIT-END
     END-START.
     PERFORM   READ-LOG-SEC.
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
     MOVE   WKSOK-F01         TO      SOK-01.
     MOVE   WKSOK-F02         TO      SOK-02.
     MOVE   WKSOK-F03         TO      SOK-03.
*****MOVE   WKSOK-F04         TO      SOK-04.
     MOVE   WKSOK-F111        TO      SOK-04(1:3).
     MOVE   "-"               TO      SOK-04(4:1).
     MOVE   WKSOK-F112        TO      SOK-04(5:4).
     MOVE   WKSOK-F05         TO      SOK-05.
     MOVE   WKSOK-F06         TO      SOK-06.
     MOVE   WKSOK-F07         TO      SOK-07.
     MOVE   WKSOK-F08         TO      SOK-08.
     MOVE   WKSOK-F09         TO      SOK-09.
     MOVE   WKSOK-F10         TO      SOK-10.
*更新履歴
     EVALUATE    LOG-F04
         WHEN    "1"
                 MOVE NC"登録"          TO   HEN-KUBUNNM
         WHEN    "2"
                 MOVE NC"修正"          TO   HEN-KUBUNNM
         WHEN    "3"
                 MOVE NC"削除"          TO   HEN-KUBUNNM
     END-EVALUATE.
     MOVE        HEN-KUBUN              TO   MEI-301.
*
     MOVE        LOG-F03                TO   WK-TANCD.
     MOVE        WK-TANCD1              TO   MEI-302.
*担当者名取得
     PERFORM     READ-TAN-SEC.
*
     MOVE        TAN-F03                TO  HEN-TANNM.
     MOVE        HEN-TAN                TO  MEI-303.
     MOVE        LOG-F05(1:4)           TO  HEN-DATE-YYYY.
     MOVE        LOG-F05(5:2)           TO  HEN-DATE-MM.
     MOVE        LOG-F05(7:2)           TO  HEN-DATE-DD.
     MOVE        HEN-DATE               TO  MEI-304.
*
     MOVE        LOG-F06(1:2)           TO  HEN-TIME-HH.
     MOVE        LOG-F06(3:2)           TO  HEN-TIME-MM.
     MOVE        LOG-F06(5:2)           TO  HEN-TIME-SS.
     MOVE        HEN-TIME               TO  MEI-305.
*2011/04/14 ST T.TAKAHASHI
     MOVE        NC"出荷オン："         TO  MEI-SYUSND.
*****2012/07/24 UOPDATE ST
*****MOVE        WKSOK-FIL1(19:1)       TO  MEI-SYUSND-CD.
*****IF          WKSOK-FIL1(19:1) = SPACE
     MOVE        WKSOK-F99              TO  MEI-SYUSND-CD.
     IF          WKSOK-F99        = SPACE
*****2012/07/24 UOPDATE ED
                 MOVE  NC"無"           TO  MEI-SYUSND-NM
     ELSE
                 MOVE  NC"有"           TO  MEI-SYUSND-NM
     END-IF.
*2011/04/14 ED T.TAKAHASHI
*2011/05/31 ST T.TAKAHASHI
     MOVE        NC"出荷手書："         TO  MEI-TEGSND.
*****2012/07/24 UOPDATE ST
*****MOVE        WKSOK-FIL1(18:1)       TO  MEI-TEGSND-CD.
*****IF          WKSOK-FIL1(18:1) = SPACE
     MOVE        WKSOK-F98              TO  MEI-TEGSND-CD.
     IF          WKSOK-F98        = SPACE
*****2012/07/24 UOPDATE ED
                 MOVE  NC"無"           TO  MEI-TEGSND-NM
     ELSE
                 MOVE  NC"有"           TO  MEI-TEGSND-NM
     END-IF.
*2011/05/31 ED T.TAKAHASHI
*2012/07/24 ST T.TAKAHASHI
     MOVE        NC"小売連携："         TO  MEI-KOURI.
     MOVE        WKSOK-F12              TO  MEI-KOURI-CD.
     IF          WKSOK-F12        = SPACE
                 MOVE  NC"無"           TO  MEI-KOURI-NM
     ELSE
                 MOVE  NC"有"           TO  MEI-KOURI-NM
     END-IF.
*2012/07/24 ED T.TAKAHASHI
*2012/10/09 ST T.TAKAHASHI
     MOVE        NC"物流連携："         TO  MEI-BUTUR.
     MOVE        WKSOK-F13              TO  MEI-BUTUR-CD.
     IF          WKSOK-F13        = SPACE
                 MOVE  NC"無"           TO  MEI-BUTUR-NM
     ELSE
                 MOVE  NC"有"           TO  MEI-BUTUR-NM
     END-IF.
*2012/10/09 ED T.TAKAHASHI
*
*****2012/07/24 UPDATE ST
*****WRITE    P-REC         FROM    MEISAI-1    AFTER  2.
     WRITE    P-REC         FROM    MEISAI-1    AFTER  1.
*****2012/07/24 UPDATE ED
     WRITE    P-REC         FROM    MEISAI-2    AFTER  1.
     WRITE    P-REC         FROM    MEISAI-3    AFTER  1.
*****2012/07/24 UPDATE ST
     WRITE    P-REC         FROM    SEN2        AFTER  1.
*****2012/07/24 UPDATE ED
     ADD      4             TO      L-CNT.
*
     PERFORM  READ-LOG-SEC.
*
 MAIN-END.
     EXIT.
************************************************************
*      2.1       見出し処理                                *
************************************************************
 MIDASI-SEC             SECTION.
     MOVE     WK-Y       TO      H-YY.
     MOVE     WK-M       TO      H-MM.
     MOVE     WK-D       TO      H-DD.
     MOVE     P-CNT      TO      PAGE-SUU.
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
**************2012/07/24 UPDATE ST
**************WRITE      P-REC   FROM    MIDASI-2   AFTER  2
              WRITE      P-REC   FROM    SEN1       AFTER  1
              WRITE      P-REC   FROM    MIDASI-2   AFTER  1
**************2012/07/24 UPDATE ED
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
              WRITE      P-REC   FROM    MIDASI-4   AFTER  1
**************2012/07/24 UPDATE ST
              WRITE      P-REC   FROM    SEN1       AFTER  1
**************2012/07/24 UPDATE ED
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
**************2012/07/24 UPDATE ST
**************WRITE      P-REC   FROM    MIDASI-2   AFTER  2
              WRITE      P-REC   FROM    SEN1       AFTER  1
              WRITE      P-REC   FROM    MIDASI-2   AFTER  2
**************2012/07/24 UPDATE ED
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
              WRITE      P-REC   FROM    MIDASI-4   AFTER  1
**************2012/07/24 UPDATE ST
              WRITE      P-REC   FROM    SEN1       AFTER  1
**************2012/07/24 UPDATE ED
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
     ADD      1   TO   READ-CNT.
***  マスタ区分のチェック
     IF       LOG-F01  NOT =  "07"
              MOVE  "END"    TO   END-FLG
              GO             TO   READ-LOG-END
     END-IF.
***  部門CD範囲のチェック
     IF       LOG-F02    >   PARA-BUMONCD
              MOVE  "END"    TO   END-FLG
              GO             TO   READ-LOG-END
     END-IF.
**
***  担当者CD範囲のチェック
*****IF       LOG-F03(1:2)   =   PARA-TANCD
     IF       LOG-F03        =   PARA-TANCD
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
     ADD    1     TO   IN-CNT.
     MOVE    LOG-F08   TO  WKSOK-REC.
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
     CLOSE   MSTLOGF PRINTF   HTANMS.
***
     DISPLAY  "SIT0160L READ ="  READ-CNT  UPON  CONS.
     DISPLAY  "SIT0160L IN   ="  IN-CNT    UPON  CONS.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
