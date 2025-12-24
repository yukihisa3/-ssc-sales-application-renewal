****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫改善／販売実績管理　　　　　　*
*    モジュール名　　　　：　商品名称マスタ区分初期セット　　　*
*    作成日／更新日　　　：　09/10/07                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　商品名称マスタを順読みして、管理　*
*    区分、商品分類ＣＤを初期セットする。　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SPD9999B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 商品名称マスタ >>************************************
     SELECT   HMEIMS   ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 商品名称マスタ >>*************************************
 FD  HMEIMS.
     COPY     HMEIMS   OF        XFDLIB
              JOINING   MEI       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 MEI-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SOS2510B".
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
 01  PARA-HIDUKE             PIC  9(08)  VALUE  ZERO.
 01  PARA-JIKAN              PIC  9(04)  VALUE  ZERO.
 01  PARA-SAKATACD           PIC  X(16)  VALUE  SPACE.
 01  PARA-SIIRETANKA         PIC  9(09)  VALUE  ZERO.
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
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  OUT-CNT             PIC  9(07)   VALUE  0.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
************************************************************
 PROCEDURE              DIVISION.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HMEIMS.
     MOVE   "HMEIMS  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
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
 PGM-CONTROL-EXT.
     EXIT    PROGRAM.
************************************************************
*      １００   初期処理                                   *
************************************************************
 100-INIT-SEC           SECTION.
*
     OPEN         I-O       HMEIMS.
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
     MOVE      LINK-OUT-YMD       TO   PARA-HIDUKE.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
     MOVE      WK-TIME(1:2)       TO   PARA-JIKAN(1:2).
     MOVE      WK-TIME(3:2)       TO   PARA-JIKAN(3:2).
*
     PERFORM  HMEIMS-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*更新判定
*初めに初期化する。
     MOVE     SPACE        TO     MEI-F90  MEI-F91.
*商品ＣＤの範囲を判定する。
     EVALUATE  MEI-F011
********1実咲　花
         WHEN  "00905000"  THRU  "00909999"
               MOVE  "1"   TO     MEI-F90
               MOVE  "1"   TO     MEI-F91
********2実咲　野菜
         WHEN  "00920000"  THRU  "00928999"
               MOVE  "2"   TO     MEI-F90
               MOVE  "1"   TO     MEI-F91
********2実咲　野菜
         WHEN  "00970400"  THRU  "00970999"
               MOVE  "2"   TO     MEI-F90
               MOVE  "1"   TO     MEI-F91
********3営利
         WHEN  "00000000"  THRU  "00099999"
               MOVE  "3"   TO     MEI-F90
               MOVE  "1"   TO     MEI-F91
********3営利
         WHEN  "00949000"  THRU  "00949999"
               MOVE  "3"   TO     MEI-F90
               MOVE  "1"   TO     MEI-F91
********4球根
         WHEN  "00300000"  THRU  "00399999"
               MOVE  "4"   TO     MEI-F90
               MOVE  " "   TO     MEI-F91
********5資材
         WHEN  "00500000"  THRU  "00899999"
               MOVE  "5"   TO     MEI-F90
               MOVE  " "   TO     MEI-F91
********6植物
         WHEN  "00950000"  THRU  "00959509"
               MOVE  "6"   TO     MEI-F90
               MOVE  " "   TO     MEI-F91
********6植物
         WHEN  "00959511"  THRU  "00959999"
               MOVE  "6"   TO     MEI-F90
               MOVE  " "   TO     MEI-F91
********6植物
         WHEN  "00959510"  THRU  "00959510"
               MOVE  "2"   TO     MEI-F90
               MOVE  "1"   TO     MEI-F91
********7芝種
         WHEN  "00155120"  THRU  "00155120"
               MOVE  "7"   TO     MEI-F90
               MOVE  "1"   TO     MEI-F91
         WHEN  "00155140"  THRU  "00155140"
               MOVE  "7"   TO     MEI-F90
               MOVE  "1"   TO     MEI-F91
********8球根　特販
         WHEN  "00959000"  THRU  "00959999"
               MOVE  "8"   TO     MEI-F90
               MOVE  " "   TO     MEI-F91
********9一寸そらまめ
         WHEN  "00100100"  THRU  "00100100"
               MOVE  "9"   TO     MEI-F90
               MOVE  "1"   TO     MEI-F91
********別部署
         WHEN  OTHER
               MOVE  "A"   TO     MEI-F90
               MOVE  " "   TO     MEI-F91
     END-EVALUATE.
*
     REWRITE  MEI-REC.
     ADD      1            TO     OUT-CNT.
*
     PERFORM  HMEIMS-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            商品名称マスタ読込処理
************************************************************
 HMEIMS-READ-SEC                   SECTION.
*
     READ    HMEIMS
        AT   END
             MOVE      "END"     TO   END-FLG
             GO                  TO   HMEIMS-READ-EXT
     END-READ.
*
     ADD     1         TO        IN-CNT.
*
     IF      IN-CNT(5:3)  =  "000" OR "500"
             DISPLAY "IN-CNT = " IN-CNT UPON CONS
     END-IF.
*
 HMEIMS-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
*
     CLOSE             HMEIMS.
*
     DISPLAY "IN-CNT  = " IN-CNT  UPON CONS.
     DISPLAY "OUT-CNT = " OUT-CNT UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
