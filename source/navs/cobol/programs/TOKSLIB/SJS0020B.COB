****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿
*    業務名　　　　　　　：　実績管理システム
*    モジュール名　　　　：　振替データ→実績累積Ｆ作成
*    処理概要　　　　　　：　振替データより、実績累積Ｆを作成す
*                            る。
*    作成日／更新日　　　：　2000/06/27
*    作成者／更新者　　　：　NAV
*    更新履歴            ：
*      2011/11/25 飯田/NAV 基幹サーバ統合、業務改善
*                          実績累積ファイルを項目追加。
*      2012/02/24 三浦/NAV 日次条件ファイルを日付ファイルへ
*                          変更。　　　　　　　　　　　　
*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJS0020B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU-GP6000.
 OBJECT-COMPUTER.       FUJITSU-GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*
*---<<  編集済振替Ｆ  >>---*
     SELECT   TOKFHO    ASSIGN    TO        DA-01-S-TOKFHO
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   TOK-STATUS.
*
*---<<  実績累積ファイル  >>---*
     SELECT   RUISEKF   ASSIGN    TO        DA-01-VI-RUISEKF
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                     *>
                     *> RUISEKL1をオーバーライドして使用。
                     *>
                        RECORD    KEY       IS   RUI-F01
                                                 RUI-F06
                                                 RUI-F03
                                                 RUI-F05
                                                 RUI-F02
                                                 RUI-F04
                        FILE      STATUS    IS   RUI-STATUS.
* 2011/11/25,S  S.I/NAV
*----<< 商品名称マスタ >>-*
     SELECT  HMEIMS   ASSIGN    TO        MEIMS1
                      ORGANIZATION        INDEXED
                      ACCESS    MODE      RANDOM
                      RECORD    KEY
                        MEI-F01 *> 自社商品ＣＤ
                      FILE      STATUS    MEI-STATUS.
*----<< 日次更新条件マスタ >>-*
     SELECT  HIDUKEF  ASSIGN    TO        HIDUKEL1
                      ORGANIZATION        INDEXED
                      ACCESS    MODE      RANDOM
                      RECORD    KEY
                        JHM-F01 *> 日時更新日付
                      FILE      STATUS    JHM-STATUS.
* 2011/11/25,E  S.I/NAV
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＴＯＫＵファイル  >>---*
 FD  TOKFHO   BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAF   OF        XFDLIB
              JOINING   TOK       PREFIX.
*---<<  部門取引先マスタ１  >>---*
 FD  RUISEKF.
     COPY     RUISEKF   OF        XFDLIB
              JOINING   RUI       PREFIX.

* 2011/11/25,S  S.I/NAV
*---<<  商品名称マスタ  >>---*
 FD  HMEIMS
     LABEL     RECORD     IS  STANDARD.
     COPY      HMEIMS    OF   XFDLIB
     JOINING   MEI  AS   PREFIX.
*---<<  日次更新条件マスタ  >>---*
 FD  HIDUKEF
     LABEL     RECORD     IS  STANDARD.
     COPY      HIDUKEF    OF   XFDLIB
     JOINING   JHM  AS   PREFIX.
* 2011/11/25,E  S.I/NAV

****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  TOK-STATUS          PIC  X(02).
     02  RUI-STATUS          PIC  X(02).
* 2011/11/25,S  S.I/NAV
     02  MEI-STATUS          PIC  X(02).
     02  JHM-STATUS          PIC  X(02).
* 2011/11/25,E  S.I/NAV
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  RUI-INV-FLG         PIC  X(03)  VALUE SPACE.
* 2011/11/25,S  S.I/NAV
     02  FG-HMEIMS-INV       PIC  9(01).
     02  FG-HIDUKEF-INV      PIC  9(01).
* 2011/11/25,E  S.I/NAV
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)  VALUE ZERO.
     02  CRT-CNT             PIC  9(07)  VALUE ZERO.
     02  SKIP-CNT            PIC  9(07)  VALUE ZERO.
****  ＷＲＫ領域  *** 1999/12/27 NAV START
 01  WRK-AREA.
     02  WRK-DATE1           PIC  9(06).
     02  WRK-DATE1R          REDEFINES   WRK-DATE1.
         04  WRK-DATE1R1     PIC  9(04).
         04  WRK-DATE1R2     PIC  9(02).
     02  WRK-DATE2           PIC  9(06).
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SJS0020B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    TOKFHO.
     MOVE     "TOKFHO"       TO   ERR-FL-ID.
     MOVE     TOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    RUISEKF.
     MOVE     "RUISEKF"      TO   ERR-FL-ID.
     MOVE     RUI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
* 2011/11/25,S  S.I/NAV
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HMEIMS.
     MOVE     "HMEIMS"       TO   ERR-FL-ID.
     MOVE     MEI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HIDUKEF.
     MOVE     "HIDUKEF"      TO   ERR-FL-ID.
     MOVE     JHM-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
* 2011/11/25,E  S.I/NAV
 END     DECLARATIVES.
****************************************************************
 SJS0020B-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM       END-SEC.
     STOP      RUN.
 SJS0020B-END.
     EXIT.
****************************************************************
*     1.0　　初期処理
****************************************************************
 INIT-SEC                    SECTION.
*ファイルのＯＰＥＮ
     OPEN  INPUT TOKFHO.
* 2011/11/25,S  S.I/NAV
     OPEN  INPUT HMEIMS.
     OPEN  INPUT HIDUKEF.
* 2011/11/25,E  S.I/NAV
     OPEN  I-O   RUISEKF.
*振替データ初期読込み
     PERFORM TOKU-READ-SEC.
*
 INIT-END.
     EXIT.
****************************************************************
*      電算室殿⇒振替データファイル読込み
****************************************************************
 TOKU-READ-SEC               SECTION.
*
     READ    TOKFHO
             AT  END
             MOVE   "END"      TO   END-FLG
             GO                TO   TOKU-READ-EXIT
     END-READ.
*読込み件数カウント
     ADD             1         TO   READ-CNT
*伝票区分＝”２０”の場合は読み飛ばし
     IF      TOK-F02  =  "20"
             ADD     1         TO   SKIP-CNT
             GO                TO   TOKU-READ-SEC
     END-IF.
*伝票区分＝”３５”で入出庫区分＝”１”以外は読み飛ばし
     IF      TOK-F02  =  "35"  AND  TOK-F16  NOT =  "1"
             ADD     1         TO   SKIP-CNT
             GO                TO   TOKU-READ-SEC
     END-IF.
*
 TOKU-READ-EXIT.
     EXIT.
****************************************************************
*      2.0　　メイン処理
****************************************************************
 MAIN-SEC                    SECTION.
*実績累積Ｆ存在チェック
     PERFORM  SONZAI-CHK-SEC.
*    存在確認
     IF    RUI-INV-FLG = "INV"
           PERFORM RUISEKF-WT-SEC
     ELSE
           ADD     1       TO     SKIP-CNT
     END-IF.
*売上計上Ｆ読込み
     PERFORM TOKU-READ-SEC.
*
 MAIN-END.
     EXIT.
****************************************************************
*    振替データ（発注）
****************************************************************
 RUISEKF-WT-SEC              SECTION.

* 2011/11/25,S  S.I/NAV
* 商品名称マスタ検索
     MOVE  TOK-F10          TO  MEI-F011. *> 自社商品ＣＤ
     MOVE  TOK-F11          TO  MEI-F012. *> 品単ＣＤ
     PERFORM  RD-HMEIMS-SEC.

* 日次更新条件マスタ検索
    *> データの出荷日で検索
     MOVE  TOK-F09          TO  JHM-F01. *> 日時更新日付
     PERFORM  RD-HIDUKEF-SEC.
* 2011/11/25,E  S.I/NAV

*実績累積Ｆ初期化
     MOVE   SPACE            TO   RUI-REC.
     INITIALIZE                   RUI-REC.
*レコード転送
     MOVE   TOK-REC          TO   RUI-REC.
*数量チェック
     IF     TOK-F13 <  ZERO
            COMPUTE  RUI-F13  =  TOK-F13  *  (-1)
     END-IF.
*金額チェック
     IF     TOK-F15 < ZERO
            COMPUTE  RUI-F15  =  TOK-F15  *  (-1)
     END-IF.
* 2011/11/25,S  S.I/
     MOVE  SPACE            TO  RUI-F23. *> 相手商品ＣＤ

     IF  FG-HMEIMS-INV = ZERO
         MOVE  MEI-F09      TO  RUI-F24 *> サカタ２０分類
         MOVE  MEI-F90      TO  RUI-F25 *> 商品分類ＣＤ
     ELSE
         MOVE  SPACE        TO  RUI-F24 *> サカタ２０分類
         MOVE  SPACE        TO  RUI-F25 *> 商品分類ＣＤ
     END-IF.

     IF  FG-HIDUKEF-INV = ZERO
         MOVE  JHM-FIL1(1:2) TO  RUI-F26 *> 週番号
     ELSE
         MOVE  ZERO         TO  RUI-F26 *> 週番号
     END-IF.

     MOVE  SPACE            TO  RUI-F27. *> 担当者ＣＤ
     MOVE  SPACE            TO  RUI-F28. *> 相手商品カナ名１
     MOVE  SPACE            TO  RUI-F29. *> 相手商品カナ名２
* 2011/11/25,E  S.I/NAV
*実績累積Ｆ出力
     WRITE  RUI-REC.
     ADD    1                TO   CRT-CNT.
*
 RUISEKF-WT-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ検索           2011/11/25 追加
****************************************************************
 RD-HMEIMS-SEC              SECTION.
     READ  HMEIMS
       INVALID
         MOVE  1            TO  FG-HMEIMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HMEIMS-INV
     END-READ.

 RD-HMEIMS-EXIT.
     EXIT.
****************************************************************
*    日次更新条件マスタ検索       2011/11/25 追加
****************************************************************
 RD-HIDUKEF-SEC             SECTION.
     READ  HIDUKEF
       INVALID
         MOVE  1            TO  FG-HIDUKEF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HIDUKEF-INV
     END-READ.

 RD-HIDUKEF-EXIT.
     EXIT.
****************************************************************
*    実績累積Ｆ存在チェック                                    *
****************************************************************
 SONZAI-CHK-SEC              SECTION.
*キーのセット
     MOVE   TOK-F01          TO    RUI-F01.
     MOVE   TOK-F06          TO    RUI-F06.
     MOVE   TOK-F03          TO    RUI-F03.
     MOVE   TOK-F05          TO    RUI-F05.
     MOVE   TOK-F02          TO    RUI-F02.
     MOVE   TOK-F04          TO    RUI-F04.
     READ   RUISEKF
            INVALID
            MOVE    "INV"    TO    RUI-INV-FLG
            NOT INVALID
            MOVE     SPACE   TO    RUI-INV-FLG
     END-READ.
*
 SONZAI-CHK-EXIT.
     EXIT.
****************************************************************
*      3.0      終了処理
****************************************************************
 END-SEC                SECTION.
     CLOSE              TOKFHO  RUISEKF.
* 2011/11/25,S  S.I/NAV
     CLOSE  HMEIMS.
     CLOSE  HIDUKEF.
* 2011/11/25,E  S.I/NAV
     DISPLAY  "TOKFHO     (IN) = "  READ-CNT      UPON   CONS.
     DISPLAY  "SKIP     (SKIP) = "  SKIP-CNT      UPON   CONS.
     DISPLAY  "CRT       (CRT) = "  CRT-CNT       UPON   CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
