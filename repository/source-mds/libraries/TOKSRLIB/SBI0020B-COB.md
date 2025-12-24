# SBI0020B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SBI0020B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＢＩツール連携　　　　　　　　　　*
*    モジュール名　　　　：　朝一マスタ抽出：商品名称マスタ抽出*
*    作成日／更新日　　　：　2018/12/12                        *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　商品名称ＴＢＬを読み、商品名称マス*
*                          タ連携データを出力する。　          *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBI0020B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 商品名称マスタ　 >>************************************
     SELECT   HMEIMS   ASSIGN    TO        DA-01-VI-MEIMS6
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   MEI-F99
                        FILE      STATUS    IS   MEI-STATUS.
****<< 連携商品名称マスタ  >>******************************
     SELECT   BIMSTMEI  ASSIGN  TO   DA-01-S-BIMSTMEI
                        ORGANIZATION         IS  SEQUENTIAL
                        ACCESS  MODE         IS  SEQUENTIAL
                        FILE STATUS          IS  MST-STATUS.
***************************************************************
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
****<< 商品名称マスタ　 >>*************************************
 FD  HMEIMS.
     COPY     HMEIMS   OF        XFDLIB
              JOINING   MEI       PREFIX.
****<< 連携商品名称マスタ　 >>******************************
 FD    BIMSTMEI BLOCK     CONTAINS  16   RECORDS
                LABEL     RECORD    IS   STANDARD.
       COPY     BIMSTMEI  OF        XFDLIB
                JOINING   MST       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 MEI-STATUS           PIC  X(02).
     02 MST-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBI0020B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  WT-CNT              PIC  9(07)   VALUE  0.
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME             PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS               PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y            PIC  9(02)  VALUE  ZERO.
         05  WK-M            PIC  9(02)  VALUE  ZERO.
         05  WK-D            PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE            PIC  9(08).
 01  KIJYUN-AREA.
     03  KIJYUN-DATE         PIC  9(08)  VALUE  ZERO.
*変換
*　単価変換
 01  WK-TANKA                PIC  9(07)V9(02).
 01  WK-TANKA-R              REDEFINES   WK-TANKA.
     03  WK-TANKA-1          PIC  9(07).
     03  WK-TANKA-2          PIC  9(02).
*　入数変換
 01  WK-IRISU                PIC  9(04)V9(02).
 01  WK-IRISU-R              REDEFINES   WK-IRISU.
     03  WK-IRISU-1          PIC  9(04).
     03  WK-IRISU-2          PIC  9(02).
*日付変換サブルーチン用ワーク
 01  SUB-AREA.
     03  LINK-IN-KBN         PIC X(01).
     03  LINK-IN-YMD6        PIC 9(06).
     03  LINK-IN-YMD8        PIC 9(08).
     03  LINK-OUT-RET        PIC X(01).
     03  LINK-OUT-YMD        PIC 9(08).
*
*LINKAGE                SECTION.
 01  PARA-OUTKENSU           PIC  9(06).
************************************************************
*PROCEDURE              DIVISION USING PARA-OUTKENSU.
 PROCEDURE              DIVISION.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HMEIMS.
     MOVE   "SHOTBL15"        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  BIMSTMEI.
     MOVE   "BIMSTMEI"        TO    ERR-FL-ID.
     MOVE    MST-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           100-INIT-SEC.
     PERFORM           200-MAIN-SEC
             UNTIL     END-FLG   =    "END".
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      １００   初期処理                                   *
************************************************************
 100-INIT-SEC           SECTION.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     INITIALIZE                        SUB-AREA.
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
*システム日付－１を算出
     INITIALIZE                        SUB-AREA.
     MOVE     "6"                 TO   LINK-IN-KBN.
     MOVE     1                   TO   LINK-IN-YMD6.
     MOVE     DATE-AREA           TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   KIJYUN-DATE.
*システム日付－１を表示
     DISPLAY NC"＃抽出基準日" " = "  KIJYUN-DATE " " NC"＃"
             UPON CONS.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*ファイルＯＰＥＮ
     OPEN      INPUT    HMEIMS.
     OPEN      EXTEND   BIMSTMEI.
*商品名称テーブルスタート
     MOVE     KIJYUN-DATE   TO   MEI-F99.
     START  HMEIMS  KEY  IS  >=  MEI-F99
            INVALID  MOVE "END"  TO  END-FLG
                     DISPLAY NC"＃抽出対象データ無１！＃"
                             UPON CONS
                     GO          TO  100-INIT-END
     END-START.
*
     PERFORM  HMEIMS-READ-SEC.
     IF  END-FLG  =  "END"
                     DISPLAY NC"＃抽出対象データ無２！＃"
                             UPON CONS
     END-IF.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*商品名称ＴＢＬ連携データ作成
     MOVE    SPACE                TO   MST-REC.
     INITIALIZE                        MST-REC.
*項目セット
*　サカタ商品ＣＤ
     MOVE    MEI-F01              TO   MST-F01.
*　サカタ品単
     MOVE    MEI-F0121            TO   MST-F02.
     MOVE    MEI-F0122            TO   MST-F03.
     MOVE    MEI-F0123            TO   MST-F04.
*　商品名漢字１，２
     MOVE    MEI-F021             TO   MST-F05.
     MOVE    MEI-F022             TO   MST-F06.
*　商品名カナ１，２
     MOVE    MEI-F031             TO   MST-F07.
     MOVE    MEI-F032             TO   MST-F08.
*　仕入単価
     MOVE    MEI-F041             TO   WK-TANKA.
     MOVE    WK-TANKA-1           TO   MST-F09(1:7).
     MOVE    "."                  TO   MST-F09(8:1).
     MOVE    WK-TANKA-2           TO   MST-F09(9:2).
*　原価単価
     MOVE    MEI-F042             TO   WK-TANKA.
     MOVE    WK-TANKA-1           TO   MST-F10(1:7).
     MOVE    "."                  TO   MST-F10(8:1).
     MOVE    WK-TANKA-2           TO   MST-F10(9:2).
*　売価単価
     MOVE    MEI-F043             TO   WK-TANKA.
     MOVE    WK-TANKA-1           TO   MST-F11(1:7).
     MOVE    "."                  TO   MST-F11(8:1).
     MOVE    WK-TANKA-2           TO   MST-F11(9:2).
*　仕入先ＣＤ
     MOVE    MEI-F05              TO   MST-F12.
*　ＪＡＮＣＤ
     MOVE    MEI-F06              TO   MST-F13.
*　入数
     MOVE    MEI-F07              TO   WK-IRISU.
     MOVE    WK-IRISU-1           TO   MST-F14(1:4).
     MOVE    "."                  TO   MST-F14(5:1).
     MOVE    WK-IRISU-2           TO   MST-F14(6:2).
*　廃盤区分
     MOVE    MEI-F08              TO   MST-F15.
*　サカタ２０分類
     MOVE    MEI-F09              TO   MST-F16.
*　振替区分
     MOVE    MEI-FIL1             TO   MST-F17.
*　物流束区分
     MOVE    MEI-F89              TO   MST-F18.
*　商品分類区分
     MOVE    MEI-F90              TO   MST-F19.
*　管理区分
     MOVE    MEI-F91              TO   MST-F20.
*　定番区分
     MOVE    MEI-F95              TO   MST-F21.
*連携：商品名称ＴＢＬ更新
     WRITE   MST-REC.
     ADD     1                    TO   WT-CNT.
*
     PERFORM  HMEIMS-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            イオンデータ読込処理
************************************************************
 HMEIMS-READ-SEC                   SECTION.
*
     READ    HMEIMS
        AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        HMEIMS-READ-EXT
     END-READ.
*
     ADD     1         TO        IN-CNT.
*
     IF      IN-CNT(5:3) = "000" OR "500"
             DISPLAY "ｼｮﾘｹﾝｽｳ = " IN-CNT  UPON CONS
     END-IF.
*更新日確認
     IF      MEI-F99  >  KIJYUN-DATE
             MOVE      "END"     TO   END-FLG
             GO        TO        HMEIMS-READ-EXT
     END-IF.
*
 HMEIMS-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
*
     CLOSE              HMEIMS.
     CLOSE              BIMSTMEI.
*
     DISPLAY "* ﾏｽﾀ" NC"抽出" ":" NC"商品名称" "ﾏｽﾀ *"
              UPON CONS.
     DISPLAY "* READ-CNT = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* WT-CNT   = " WT-CNT   " *"  UPON CONS.
*パラメタセット
     MOVE       WT-CNT              TO   PARA-OUTKENSU.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
