# SBI0120B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SBI0120B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＢＩツール連携　　　　　　　　　　*
*    モジュール名　　　　：　日次計上データ作成　　　　　　　　*
*    作成日／更新日　　　：　2018/12/14                        *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　ＢＩ編集用ファイルを読み、編集後、*
*                          ＢＩ連携用ファイルを作成出力する。　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBI0120B.
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
****<< ＢＩ編集用ファイル  >>*********************************
     SELECT   BITRNDTF   ASSIGN    TO        DA-01-VI-BITRNDT1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   TRN-F01
                                                 TRN-F02
                                                 TRN-F03
                        FILE      STATUS    IS   TRN-STATUS.
****<< ＢＩ連携用ファイル　  >>*******************************
     SELECT   BITRNSET  ASSIGN   TO         DA-01-S-BITRNSET
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS  MODE        IS   SEQUENTIAL
                        FILE STATUS         IS   FUR-STATUS.
***************************************************************
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
****<< ＢＩ編集用ファイル  >>*********************************
 FD  BITRNDTF.
     COPY     BITRNDTF  OF        XFDLIB
              JOINING   TRN       PREFIX.
****<< ＢＩ連携用ファイル　  >>*****************************
 FD    BITRNSET BLOCK     CONTAINS  8    RECORDS
                LABEL     RECORD    IS   STANDARD.
       COPY     BITRNSET  OF        XFDLIB
                JOINING   FUR       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TRN-STATUS           PIC  X(02).
     02 FUR-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBI0120B".
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
 01  PARA-OUTKENSU           PIC  9(06).
************************************************************
 PROCEDURE              DIVISION USING PARA-OUTKENSU.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  BITRNDTF.
     MOVE   "BITRNDT1"        TO    ERR-FL-ID.
     MOVE    TRN-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2           SECTION.
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
     OPEN      I-O      BITRNDTF.
     OPEN      EXTEND   BITRNSET.
*
     PERFORM  BITRNDTF-READ-SEC.
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
*ＢＩ連携用ファイル作成
     MOVE    SPACE                TO   FUR-REC.
     INITIALIZE                        FUR-REC.
*項目セット
*　部門ＣＤ
     MOVE    TRN-F101             TO   FUR-F01.
*　伝票区分
     MOVE    TRN-F102             TO   FUR-F02.
*　伝票番号
     MOVE    TRN-F103             TO   FUR-F03.
*　行番号
     MOVE    TRN-F104             TO   FUR-F04.
*　赤黒区分
     MOVE    TRN-F105             TO   FUR-F05.
*　得意先／仕入先
     MOVE    TRN-F106             TO   FUR-F06.
*　納入先ＣＤ
     MOVE    TRN-F107             TO   FUR-F07.
*　発注日
     MOVE    TRN-F108             TO   WK-HIDUKE.
     MOVE    WK-HIDUKE-1          TO   FUR-F08(1:4).
     MOVE    "/"                  TO   FUR-F08(5:1).
     MOVE    WK-HIDUKE-2          TO   FUR-F08(6:2).
     MOVE    "/"                  TO   FUR-F08(8:1).
     MOVE    WK-HIDUKE-3          TO   FUR-F08(9:2).
*　納品日
     MOVE    TRN-F109             TO   WK-HIDUKE.
     MOVE    WK-HIDUKE-1          TO   FUR-F09(1:4).
     MOVE    "/"                  TO   FUR-F09(5:1).
     MOVE    WK-HIDUKE-2          TO   FUR-F09(6:2).
     MOVE    "/"                  TO   FUR-F09(8:1).
     MOVE    WK-HIDUKE-3          TO   FUR-F09(9:2).
*　出荷日
     MOVE    TRN-F110             TO   WK-HIDUKE.
     MOVE    WK-HIDUKE-1          TO   FUR-F10(1:4).
     MOVE    "/"                  TO   FUR-F10(5:1).
     MOVE    WK-HIDUKE-2          TO   FUR-F10(6:2).
     MOVE    "/"                  TO   FUR-F10(8:1).
     MOVE    WK-HIDUKE-3          TO   FUR-F10(9:2).
*　サカタ商品ＣＤ
     MOVE    TRN-F111             TO   FUR-F11.
*　サカタ品単ＣＤ
     MOVE    TRN-F112(1:5)        TO   FUR-F12.
     MOVE    TRN-F112(6:2)        TO   FUR-F13.
     MOVE    TRN-F112(8:1)        TO   FUR-F14.
*　ストック番号
     MOVE    TRN-F113             TO   FUR-F15.
*　数量
     MOVE    TRN-F115             TO   WK-TANKA.
     IF      TRN-F115  >=   ZERO
             MOVE   "0"           TO   FUR-F16(1:1)
     ELSE
             MOVE   "-"           TO   FUR-F16(1:1)
     END-IF.
     MOVE    WK-TANKA-1           TO   FUR-F16(2:7).
     MOVE    "."                  TO   FUR-F16(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F16(10:2).
*　単価区分
     MOVE    TRN-F114             TO   FUR-F17.
*　単価
     MOVE    TRN-F116             TO   WK-TANKA.
     MOVE    "0"                  TO   FUR-F18(1:1).
     MOVE    WK-TANKA-1           TO   FUR-F18(2:7).
     MOVE    "."                  TO   FUR-F18(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F18(10:2).
*　金額
     MOVE    TRN-F117             TO   WK-TANKA.
     IF      TRN-F117  >=  ZERO
             MOVE   "0"           TO   FUR-F19(1:1)
     ELSE
             MOVE   "-"           TO   FUR-F19(1:1)
     END-IF.
     MOVE    WK-TANKA-1           TO   FUR-F19(2:7).
     MOVE    "."                  TO   FUR-F19(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F19(10:2).
*　場所ＣＤ
     MOVE    TRN-F118             TO   FUR-F20.
*　入出庫区分
     MOVE    TRN-F119             TO   FUR-F21.
*　税区分
     MOVE    TRN-F120             TO   FUR-F22.
*　作業明細区分
     MOVE    TRN-F121             TO   FUR-F23.
*　備考
     MOVE    TRN-F122             TO   FUR-F24.
*　注文番号
     MOVE    TRN-F123             TO   FUR-F25.
*　量販店商品ＣＤ
     MOVE    TRN-F124             TO   FUR-F26.
*　担当者ＣＤ
     MOVE    TRN-F125             TO   FUR-F27.
*　請求区分
     MOVE    TRN-F126             TO   FUR-F28.
*　処理日
     MOVE    TRN-F127             TO   WK-HIDUKE.
     MOVE    WK-HIDUKE-1          TO   FUR-F29(1:4).
     MOVE    "/"                  TO   FUR-F29(5:1).
     MOVE    WK-HIDUKE-2          TO   FUR-F29(6:2).
     MOVE    "/"                  TO   FUR-F29(8:1).
     MOVE    WK-HIDUKE-3          TO   FUR-F29(9:2).
*　量販店番号
     MOVE    TRN-F133             TO   FUR-F30.
*　支払締日（日付は１日で作成）
     IF  TRN-F134  NOT =  ZERO
         MOVE    TRN-F134(1:4)    TO   FUR-F31(1:4)
         MOVE    "/"              TO   FUR-F31(5:1)
         MOVE    TRN-F134(5:2)    TO   FUR-F31(6:2)
         MOVE    "/"              TO   FUR-F31(8:1)
         MOVE    "01"             TO   FUR-F31(9:2)
     ELSE
         MOVE    "0000/00/00"     TO   FUR-F31
     END-IF.
*　単価１（仕入単価）
     MOVE    TRN-F137             TO   WK-TANKA.
     MOVE    "0"                  TO   FUR-F32(1:1).
     MOVE    WK-TANKA-1           TO   FUR-F32(2:7).
     MOVE    "."                  TO   FUR-F32(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F32(10:2).
*　単価２（売価単価）
     MOVE    TRN-F138             TO   WK-TANKA.
     MOVE    "0"                  TO   FUR-F33(1:1).
     MOVE    WK-TANKA-1           TO   FUR-F33(2:7).
     MOVE    "."                  TO   FUR-F33(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F33(10:2).
*　営業第２部取引先ＣＤ
     MOVE    TRN-F139             TO   FUR-F34.
*　受注数量
     MOVE    TRN-F141             TO   WK-TANKA.
     IF      TRN-F141  >=  0
             MOVE   "0"           TO   FUR-F35(1:1)
     ELSE
             MOVE   "-"           TO   FUR-F35(1:1)
     END-IF.
     MOVE    WK-TANKA-1           TO   FUR-F35(2:7).
     MOVE    "."                  TO   FUR-F35(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F35(10:2).
*　ＪＡＮＣＤ
     MOVE    TRN-F201             TO   FUR-F36.
*　インストアＣＤ
     MOVE    TRN-F202             TO   FUR-F37.
*　訂正数
     MOVE    TRN-F203             TO   WK-TANKA.
     IF      TRN-F203  >=  "1"
             MOVE   "0"           TO   FUR-F38(1:1)
     ELSE
             MOVE   "-"           TO   FUR-F38(1:1)
     END-IF.
     MOVE    WK-TANKA-1           TO   FUR-F38(2:7).
     MOVE    "."                  TO   FUR-F38(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F38(10:2).
*　生産者
     MOVE    TRN-F204             TO   FUR-F39.
*　物流区分
     MOVE    TRN-F205             TO   FUR-F40.
*　商品形状区分
     MOVE    TRN-F206             TO   FUR-F41.
*　発注区分
     MOVE    TRN-F207             TO   FUR-F42.
*　小売連携区分
     MOVE    TRN-F208             TO   FUR-F43.
*　計上年月
     MOVE    TRN-F209(1:4)        TO   FUR-F44(1:4).
     MOVE    "/"                  TO   FUR-F44(5:1).
     MOVE    TRN-F209(5:2)        TO   FUR-F44(6:2).
     MOVE    "/"                  TO   FUR-F44(8:1).
     MOVE    "01"                 TO   FUR-F44(9:2).
*　計上週
     MOVE    TRN-F210             TO   FUR-F45.
*　未計上データ区分
     MOVE    TRN-F211             TO   FUR-F46.
*　手書オンライン区分
     MOVE    TRN-F212             TO   FUR-F47.
*連携：商品変換ＴＢＬ更新
     WRITE   FUR-REC.
     ADD     1                    TO   WT-CNT.
*
     PERFORM  BITRNDTF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            ＡＣＯＳ振替ＤＴ累積Ｆ読込
************************************************************
 BITRNDTF-READ-SEC                   SECTION.
*
     READ    BITRNDTF
        AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        BITRNDTF-READ-EXT
     END-READ.
*
     ADD     1         TO        IN-CNT.
*
     IF      IN-CNT(5:3) = "000" OR "500"
             DISPLAY "ｼｮﾘｹﾝｽｳ = " IN-CNT  UPON CONS
     END-IF.
*
 BITRNDTF-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
*
     CLOSE             BITRNDTF  BITRNSET.
*
     DISPLAY "* " NC"ＢＩ連携データ件数" " *" UPON CONS.
     DISPLAY "* READ-CNT = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* WT-CNT   = " WT-CNT   " *"  UPON CONS.
*パラメタセット
     MOVE       WT-CNT              TO   PARA-OUTKENSU.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
