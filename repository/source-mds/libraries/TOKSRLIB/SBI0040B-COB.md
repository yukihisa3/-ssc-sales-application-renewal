# SBI0040B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SBI0040B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＢＩツール連携　　　　　　　　　　*
*    モジュール名　　　　：　朝一：社内振替データ抽出　　　　　*
*    作成日／更新日　　　：　2018/12/13                        *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　ＡＣＯＳ振替累積ＤＴを読み、ＢＩ用*
*                          連携用データを抽出する。　          *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBI0040B.
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
****<< ＡＣＯＳ振替データ累積ファイル  >>*********************
     SELECT   FRACOSF   ASSIGN    TO        DA-01-VI-FRACOSL4
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   FRA-F94
                        FILE      STATUS    IS   FRA-STATUS.
****<< 商品名称マスタ  >>*************************************
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
****<< 新日付管理マスタ  >>***********************************
     SELECT   DATECKF   ASSIGN    TO        DA-01-VI-DATECKL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   DAT-F01
                        FILE      STATUS    IS   DAT-STATUS.
****<< （振替）日次計上ＤＴ  >>****************************
     SELECT   BITRNSET  ASSIGN   TO         DA-01-S-BIFURSET
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS  MODE        IS   SEQUENTIAL
                        FILE STATUS         IS   FUR-STATUS.
***************************************************************
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
****<< ＡＣＯＳ振替データ累積ファイル  >>*********************
 FD  FRACOSF.
     COPY     FRACOSF   OF        XFDLIB
              JOINING   FRA       PREFIX.
****<< 商品名称マスタ  >>*************************************
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
****<< 新日付管理マスタ >>************************************
 FD  DATECKF.
     COPY     DATECKF   OF        XFDLIB
              JOINING   DAT       PREFIX.
****<< （振替）日次計上ＤＴ  >>*****************************
 FD    BITRNSET BLOCK     CONTAINS  8    RECORDS
                LABEL     RECORD    IS   STANDARD.
       COPY     BITRNSET  OF        XFDLIB
                JOINING   FUR       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 FRA-STATUS           PIC  X(02).
     02 MEI-STATUS           PIC  X(02).
     02 DAT-STATUS           PIC  X(02).
     02 FUR-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBI0040B".
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
 01  HMEIMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  DATECKF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  NYS-FLG                 PIC  X(01)  VALUE  SPACE.
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
*　日付変換
 01  WK-HIDUKE.
     03  WK-HIDUKE-1         PIC  9(04).
     03  WK-HIDUKE-2         PIC  9(02).
     03  WK-HIDUKE-3         PIC  9(02).
*日付変換サブルーチン用ワーク
 01  SUB-AREA.
     03  LINK-IN-KBN         PIC X(01).
     03  LINK-IN-YMD6        PIC 9(06).
     03  LINK-IN-YMD8        PIC 9(08).
     03  LINK-OUT-RET        PIC X(01).
     03  LINK-OUT-YMD        PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-OUTKENSU           PIC  9(07).
************************************************************
 PROCEDURE              DIVISION USING PARA-OUTKENSU.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  FRACOSF.
     MOVE   "FRACOSL1"        TO    ERR-FL-ID.
     MOVE    FRA-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HMEIMS.
     MOVE   "MEIMS1  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DATECKF.
     MOVE   "DATECKL1"        TO    ERR-FL-ID.
     MOVE    DAT-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  BITRNSET.
     MOVE   "BITRNSET"        TO    ERR-FL-ID.
     MOVE    FUR-STATUS       TO    ERR-STCD.
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
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*ファイルＯＰＥＮ
     OPEN      I-O      FRACOSF.
     OPEN      INPUT    HMEIMS  DATECKF.
     OPEN      EXTEND   BITRNSET.
*商品変換テーブルスタート
     MOVE      SPACE        TO   FRA-F94.
     START  FRACOSF  KEY  IS  >=  FRA-F94
            INVALID  MOVE "END"  TO  END-FLG
                     DISPLAY NC"＃抽出対象データ無１！＃"
                             UPON CONS
                     GO          TO  100-INIT-END
     END-START.
*
     PERFORM  FRACOSF-READ-SEC.
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
     MOVE    SPACE                TO   FUR-REC.
     INITIALIZE                        FUR-REC.
*商品名称マスタ索引
     MOVE    FRA-F10A             TO   MEI-F011.
     MOVE    FRA-F10B(1:5)        TO   MEI-F0121.
     MOVE    FRA-F10B(6:2)        TO   MEI-F0122.
     MOVE    FRA-F10B(8:1)        TO   MEI-F0123.
     PERFORM  HMEIMS-READ-SEC.
*新日付管理マスタ索引
     MOVE    FRA-F08              TO   DAT-F01.
     PERFORM  DATECKF-READ-SEC.
*項目セット
*　部門ＣＤ
     MOVE    FRA-F101             TO   FUR-F01.
*　伝票区分
     MOVE    FRA-F102             TO   FUR-F02.
*　伝票番号
     MOVE    FRA-F103             TO   FUR-F03.
*　行番号
     MOVE    FRA-F104             TO   FUR-F04.
*　赤黒区分
     MOVE    FRA-F105             TO   FUR-F05.
*　得意先／仕入先
     MOVE    FRA-F106             TO   FUR-F06.
*　納入先ＣＤ
     MOVE    FRA-F10S             TO   FUR-F07.
*　発注日
     MOVE    FRA-F107             TO   WK-HIDUKE.
     MOVE    WK-HIDUKE-1          TO   FUR-F08(1:4).
     MOVE    "/"                  TO   FUR-F08(5:1).
     MOVE    WK-HIDUKE-2          TO   FUR-F08(6:2).
     MOVE    "/"                  TO   FUR-F08(8:1).
     MOVE    WK-HIDUKE-3          TO   FUR-F08(9:2).
*　納品日
     MOVE    FRA-F108             TO   WK-HIDUKE.
     MOVE    WK-HIDUKE-1          TO   FUR-F09(1:4).
     MOVE    "/"                  TO   FUR-F09(5:1).
     MOVE    WK-HIDUKE-2          TO   FUR-F09(6:2).
     MOVE    "/"                  TO   FUR-F09(8:1).
     MOVE    WK-HIDUKE-3          TO   FUR-F09(9:2).
*　出荷日
     MOVE    FRA-F109             TO   WK-HIDUKE.
     MOVE    WK-HIDUKE-1          TO   FUR-F10(1:4).
     MOVE    "/"                  TO   FUR-F10(5:1).
     MOVE    WK-HIDUKE-2          TO   FUR-F10(6:2).
     MOVE    "/"                  TO   FUR-F10(8:1).
     MOVE    WK-HIDUKE-3          TO   FUR-F10(9:2).
*　サカタ商品ＣＤ
     MOVE    FRA-F10A             TO   FUR-F11.
*　サカタ品単ＣＤ
     MOVE    FRA-F10B(1:5)        TO   FUR-F12.
     MOVE    FRA-F10B(6:2)        TO   FUR-F13.
     MOVE    FRA-F10B(8:1)        TO   FUR-F14.
*　ストック番号
     MOVE    FRA-F10O             TO   FUR-F15.
*  符号判定
     EVALUATE  FRA-F102
         WHEN "40"
         IF  FRA-F105  =  1  OR  3  OR  5
             MOVE  "2"            TO   NYS-FLG
         ELSE
             MOVE  "1"            TO   NYS-FLG
         END-IF
         WHEN "41"
         IF  FRA-F105  =  1  OR  3  OR  5
             MOVE  "1"            TO   NYS-FLG
         ELSE
             MOVE  "2"            TO   NYS-FLG
         END-IF
         WHEN "50" WHEN "51"
         IF  FRA-F105  =  1
             MOVE  "2"            TO   NYS-FLG
         ELSE
             MOVE  "1"            TO   NYS-FLG
         END-IF
         WHEN  OTHER
         IF      FRA-F10G  =  "1"
             MOVE   "1"           TO   NYS-FLG
         ELSE
             MOVE   "2"           TO   NYS-FLG
         END-IF
     END-EVALUATE.
*　数量
     MOVE    FRA-F10D             TO   WK-TANKA.
     IF      NYS-FLG   =  "1"
             MOVE   "0"           TO   FUR-F16(1:1)
     ELSE
             MOVE   "-"           TO   FUR-F16(1:1)
     END-IF.
     MOVE    WK-TANKA-1           TO   FUR-F16(2:7).
     MOVE    "."                  TO   FUR-F16(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F16(10:2).
*　単価区分
     MOVE    FRA-F10C             TO   FUR-F17.
*　単価
     MOVE    FRA-F10E             TO   WK-TANKA.
     MOVE    "0"                  TO   FUR-F18(1:1).
     MOVE    WK-TANKA-1           TO   FUR-F18(2:7).
     MOVE    "."                  TO   FUR-F18(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F18(10:2).
*　金額
     MOVE    FRA-F10F             TO   WK-TANKA.
     IF      NYS-FLG  =  "1"
             MOVE   "0"           TO   FUR-F19(1:1)
     ELSE
             MOVE   "-"           TO   FUR-F19(1:1)
     END-IF.
     MOVE    WK-TANKA-1           TO   FUR-F19(2:7).
     MOVE    "."                  TO   FUR-F19(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F19(10:2).
*　場所ＣＤ
     MOVE    FRA-F10H             TO   FUR-F20.
*　入出庫区分
     MOVE    FRA-F10G             TO   FUR-F21.
*　処理日
     MOVE    DATE-AREA            TO   WK-HIDUKE.
     MOVE    WK-HIDUKE-1          TO   FUR-F29(1:4).
     MOVE    "/"                  TO   FUR-F29(5:1).
     MOVE    WK-HIDUKE-2          TO   FUR-F29(6:2).
     MOVE    "/"                  TO   FUR-F29(8:1).
     MOVE    WK-HIDUKE-3          TO   FUR-F29(9:2).
*　商品名称マスタより
     IF      HMEIMS-INV-FLG  =  SPACE
             MOVE    MEI-F06      TO   FUR-F36
     END-IF.
*　新日付管理マスタ
     IF      DATECKF-INV-FLG =  SPACE
             MOVE    DAT-F05(1:4) TO   FUR-F44(1:4)
             MOVE    "/"          TO   FUR-F44(5:1)
             MOVE    DAT-F05(5:2) TO   FUR-F44(6:2)
             MOVE    "/"          TO   FUR-F44(8:1)
             MOVE    "01"         TO   FUR-F44(9:2)
             MOVE    DAT-F04      TO   FUR-F45
     END-IF.
*    抽出区分更新
     MOVE    1                    TO   FRA-F94.
     MOVE    DATE-AREA            TO   FRA-F95.
     REWRITE FRA-REC.
*
     WRITE   FUR-REC.
     ADD     1                    TO   WT-CNT.
*
     PERFORM  FRACOSF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            ＡＣＯＳ振替ＤＴ累積Ｆ読込
************************************************************
 FRACOSF-READ-SEC                   SECTION.
*
     READ    FRACOSF
        AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        FRACOSF-READ-EXT
     END-READ.
*
     ADD     1         TO        IN-CNT.
*
     IF      IN-CNT(5:3) = "000" OR "500"
             DISPLAY "ｼｮﾘｹﾝｽｳ = " IN-CNT  UPON CONS
     END-IF.
*更新区分確認
     IF      FRA-F94  =  "1"
             MOVE      "END"     TO   END-FLG
             GO        TO        FRACOSF-READ-EXT
     END-IF.
*
 FRACOSF-READ-EXT.
     EXIT.
************************************************************
*    商品名称マスタ読込
************************************************************
 HMEIMS-READ-SEC                    SECTION.
*
     READ    HMEIMS
             INVALID
             MOVE      "INV"        TO   HMEIMS-INV-FLG
             NOT  INVALID
             MOVE      SPACE        TO   HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
************************************************************
*    新日付管理マスタ読込
************************************************************
 DATECKF-READ-SEC                   SECTION.
*
     READ    DATECKF
             INVALID
             MOVE      "INV"        TO   DATECKF-INV-FLG
             NOT  INVALID
             MOVE      SPACE        TO   DATECKF-INV-FLG
     END-READ.
*
 DATECKF-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
*
     CLOSE             FRACOSF  HMEIMS  DATECKF  BITRNSET.
*
     DISPLAY "* TRN" NC"抽出" ":" "ACOS" NC"振替" "    *"
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
