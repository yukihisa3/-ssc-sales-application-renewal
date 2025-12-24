# SBI0010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SBI0010B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＢＩツール連携　　　　　　　　　　*
*    モジュール名　　　　：　朝一マスタ抽出：商品変換ＴＢＬ抽出*
*    作成日／更新日　　　：　2018/12/12                        *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　商品変換ＴＢＬを読み、商品変換ＴＢ*
*                          Ｌ連携データを出力する。　          *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBI0010B.
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
****<< 商品変換テーブル >>************************************
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL15
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   TBL-F99
                        FILE      STATUS    IS   TBL-STATUS.
****<< ＳＵＢ商品変換テーブル >>******************************
     SELECT   SUBTBLF   ASSIGN    TO        DA-01-VI-SUBTBLL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SUB-F01  SUB-F02
                        FILE      STATUS    IS   SUB-STATUS.
****<< 連携商品変換ＴＢＬ  >>******************************
     SELECT   BIMSTTBL  ASSIGN  TO   DA-01-S-BIMSTTBL
                        ORGANIZATION         IS  SEQUENTIAL
                        ACCESS  MODE         IS  SEQUENTIAL
                        FILE STATUS          IS  MST-STATUS.
***************************************************************
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
****<< 商品変換テーブル >>*************************************
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
****<< ＳＵＢ商品変換テーブル >>*******************************
 FD  SUBTBLF.
     COPY     SUBTBLF   OF        XFDLIB
              JOINING   SUB       PREFIX.
****<< 連携商品変換ＴＢＬ　 >>******************************
 FD    BIMSTTBL BLOCK     CONTAINS  20   RECORDS
                LABEL     RECORD    IS   STANDARD.
       COPY     BIMSTTBL  OF        XFDLIB
                JOINING   MST       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TBL-STATUS           PIC  X(02).
     02 SUB-STATUS           PIC  X(02).
     02 MST-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBI0010B".
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
 01  SUBTBLF-INV-FLG         PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  WT-CNT              PIC  9(06)   VALUE  0.
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
                   PROCEDURE  HSHOTBL.
     MOVE   "SHOTBL15"        TO    ERR-FL-ID.
     MOVE    TBL-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBTBLF.
     MOVE   "SUBTBLL1"        TO    ERR-FL-ID.
     MOVE    SUB-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  BIMSTTBL.
     MOVE   "BIMSTTBL"        TO    ERR-FL-ID.
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
     OPEN      INPUT    HSHOTBL   SUBTBLF.
     OPEN      EXTEND   BIMSTTBL.
*商品変換テーブルスタート
     MOVE     KIJYUN-DATE   TO   TBL-F99.
     START  HSHOTBL  KEY  IS  >=  TBL-F99
            INVALID  MOVE "END"  TO  END-FLG
                     DISPLAY NC"＃抽出対象データ無１！＃"
                             UPON CONS
                     GO          TO  100-INIT-END
     END-START.
*
     PERFORM  HSHOTBL-READ-SEC.
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
*商品変換ＴＢＬ連携データ作成
     MOVE    SPACE                TO   MST-REC.
     INITIALIZE                        MST-REC.
*ＳＵＢ商品変換ＴＢＬ索引
     MOVE    TBL-F01              TO   SUB-F01.
     MOVE    TBL-F02              TO   SUB-F02.
     PERFORM  SUBTBLF-READ-SEC.
*項目セット
*　取引先ＣＤ
     MOVE    TBL-F01              TO   MST-F01.
*　相手商品ＣＤ
     MOVE    TBL-F02              TO   MST-F02.
*　サカタ商品ＣＤ
     MOVE    TBL-F031             TO   MST-F03.
*　サカタ品単
     MOVE    TBL-F0321            TO   MST-F04.
     MOVE    TBL-F0322            TO   MST-F05.
     MOVE    TBL-F0323            TO   MST-F06.
*　出荷場所
     MOVE    TBL-F04              TO   MST-F07.
*　仕入単価
     MOVE    TBL-F09              TO   WK-TANKA.
     MOVE    WK-TANKA-1           TO   MST-F08(1:7).
     MOVE    "."                  TO   MST-F08(8:1).
     MOVE    WK-TANKA-2           TO   MST-F08(9:2).
*　原価単価
     MOVE    TBL-F05              TO   WK-TANKA.
     MOVE    WK-TANKA-1           TO   MST-F09(1:7).
     MOVE    "."                  TO   MST-F09(8:1).
     MOVE    WK-TANKA-2           TO   MST-F09(9:2).
*　売価単価
     MOVE    TBL-F06              TO   WK-TANKA.
     MOVE    WK-TANKA-1           TO   MST-F10(1:7).
     MOVE    "."                  TO   MST-F10(8:1).
     MOVE    WK-TANKA-2           TO   MST-F10(9:2).
*　_番
     MOVE    TBL-F08              TO   MST-F11.
*　ＳＵＢ商品変換ＴＢＬより
     IF      SUBTBLF-INV-FLG  =  SPACE
             MOVE    SUB-F13      TO   MST-F12
             MOVE    SUB-F14      TO   MST-F13
             MOVE    SUB-F15      TO   WK-IRISU
             MOVE    WK-IRISU-1   TO   MST-F14(1:4)
             MOVE    "."          TO   MST-F14(5:1)
             MOVE    WK-IRISU-2   TO   MST-F14(6:2)
     END-IF.
*連携：商品変換ＴＢＬ更新
     WRITE   MST-REC.
     ADD     1                    TO   WT-CNT.
*
     PERFORM  HSHOTBL-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            イオンデータ読込処理
************************************************************
 HSHOTBL-READ-SEC                   SECTION.
*
     READ    HSHOTBL
        AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        HSHOTBL-READ-EXT
     END-READ.
*
     ADD     1         TO        IN-CNT.
*
     IF      IN-CNT(5:3) = "000" OR "500"
             DISPLAY "ｼｮﾘｹﾝｽｳ = " IN-CNT  UPON CONS
     END-IF.
*更新日確認
     IF      TBL-F99  >  KIJYUN-DATE
             MOVE      "END"     TO   END-FLG
             GO        TO        HSHOTBL-READ-EXT
     END-IF.
*
 HSHOTBL-READ-EXT.
     EXIT.
************************************************************
*    ＳＵＢ商品変換ＴＢＬマスタ読込
************************************************************
 SUBTBLF-READ-SEC                   SECTION.
*
     READ    SUBTBLF
             INVALID
             MOVE      "INV"        TO   SUBTBLF-INV-FLG
             NOT  INVALID
             MOVE      SPACE        TO   SUBTBLF-INV-FLG
     END-READ.
*
 SUBTBLF-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
*
     CLOSE             HSHOTBL  SUBTBLF  BIMSTTBL.
*
     DISPLAY "* ﾏｽﾀ" NC"抽出" ":" NC"商品変換" "TBL *"
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
