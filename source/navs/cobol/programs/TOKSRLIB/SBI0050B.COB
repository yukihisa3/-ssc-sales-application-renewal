****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＢＩツール連携　　　　　　　　　　*
*    モジュール名　　　　：　朝一：売上未計上データ抽出　　　　*
*    作成日／更新日　　　：　2018/12/14                        *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　売上伝票ファイルより、未計上のＤＴ*
*                          を抽出する。（４０，４１，４２）　  *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBI0050B.
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
****<< 売上伝票ファイル  >>***********************************
     SELECT   SHTDENF  ASSIGN     TO        DA-01-VI-SHTDENL6
                       ORGANIZATION         INDEXED
                       ACCESS     MODE      SEQUENTIAL
                       RECORD     KEY       DEN-F277 DEN-F274
                                            DEN-F09  DEN-F02
                                            DEN-F04  DEN-F051
                                            DEN-F07  DEN-F112
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
****<< ＳＵＢ商品変換ＴＢＬ  >>*******************************
     SELECT   SUBTBLF   ASSIGN    TO        DA-01-VI-SUBTBLL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TBL-F01
                                                 TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
****<< 新日付管理マスタ  >>***********************************
     SELECT   DATECKF   ASSIGN    TO        DA-01-VI-DATECKL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   DAT-F01
                        FILE      STATUS    IS   DAT-STATUS.
****<< 取引先マスタ　　  >>***********************************
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
****<< （振替）日次計上ＤＴ  >>****************************
     SELECT   BITRNSET  ASSIGN   TO         DA-01-S-BIMKESET
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS  MODE        IS   SEQUENTIAL
                        FILE STATUS         IS   FUR-STATUS.
***************************************************************
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
****<< 売上伝票ファイル  >>***********************************
 FD  SHTDENF.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
****<< ＳＵＢ商品変換ＴＢＬ  >>*******************************
 FD  SUBTBLF.
     COPY     SUBTBLF    OF        XFDLIB
              JOINING   TBL       PREFIX.
****<< 新日付管理マスタ >>************************************
 FD  DATECKF.
     COPY     DATECKF   OF        XFDLIB
              JOINING   DAT       PREFIX.
****<< 取引先マスタ　　 >>************************************
 FD  HTOKMS.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
****<< （振替）日次計上ＤＴ  >>*****************************
 FD    BITRNSET BLOCK     CONTAINS  8    RECORDS
                LABEL     RECORD    IS   STANDARD.
       COPY     BITRNSET  OF        XFDLIB
                JOINING   FUR       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
*ホーマック
     COPY   HMSHIRED OF XFDLIB  JOINING   HOR  AS   PREFIX.
*カーマ
     COPY   KMSHIRED OF XFDLIB  JOINING   KAM  AS   PREFIX.
*ダイキ
     COPY   DISHIRED OF XFDLIB  JOINING   DAI  AS   PREFIX.
*ケーヨー
     COPY   DCMKEIYO OF XFDLIB  JOINING   KEI  AS   PREFIX.
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 DEN-STATUS           PIC  X(02).
     02 TBL-STATUS           PIC  X(02).
     02 DAT-STATUS           PIC  X(02).
     02 TOK-STATUS           PIC  X(02).
     02 FUR-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBI0050B".
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
 01  DATECKF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  HTOKMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
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
 01  WK-HEN-SU               PIC S9(07)V9(02).
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
 01  WK-CHG-HIDUKE           PIC  9(08)   VALUE  ZERO.
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
                   PROCEDURE  SHTDENF.
     MOVE   "SHTDENL1"        TO    ERR-FL-ID.
     MOVE    DEN-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBTBLF.
     MOVE   "SUBTBLL1"        TO    ERR-FL-ID.
     MOVE    TBL-STATUS       TO    ERR-STCD.
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
                   PROCEDURE  HTOKMS.
     MOVE   "TOKMS2  "        TO    ERR-FL-ID.
     MOVE    TOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC5           SECTION.
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
     OPEN      INPUT    SHTDENF.
     OPEN      INPUT    SUBTBLF  DATECKF  HTOKMS.
     OPEN      OUTPUT   BITRNSET.
*    売上伝票Ｆ読込
     PERFORM  SHTDENF-READ-SEC.
     IF  END-FLG  =  "END"
                     DISPLAY NC"＃抽出対象データ無！＃"
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
     MOVE    DEN-F01              TO   TBL-F01.
     MOVE    DEN-F25              TO   TBL-F02.
     PERFORM  SUBTBLF-READ-SEC.
*新日付管理マスタ索引
     IF   DEN-F113 = ZERO
          MOVE  DEN-F112          TO   DAT-F01
     ELSE
          MOVE  DEN-F113          TO   DAT-F01
     END-IF.
     PERFORM  DATECKF-READ-SEC.
*取引先マスタ索引
     MOVE    DEN-F01              TO   TOK-F01.
     PERFORM  HTOKMS-READ-SEC.
     IF       HTOKMS-INV-FLG  =  "INV"
              DISPLAY NC"＃取引先Ｍ未登録！！＃" TOK-F01
                      UPON CONS
              MOVE   4000         TO   PROGRAM-STATUS
              STOP  RUN
*****ELSE
*****DISPLAY "TOK-F75 = " TOK-F75 " " TOK-F76 " " TOK-F77 UPON
*****         CONS
     END-IF.
*項目セット
*　部門ＣＤ
     MOVE    TOK-F75              TO   FUR-F01.
     IF   DEN-F113 = ZERO
          MOVE  DEN-F112          TO   WK-CHG-HIDUKE
     ELSE
          MOVE  DEN-F113          TO   WK-CHG-HIDUKE
     END-IF.
     IF      TOK-F76  NOT =  ZERO
             IF      TOK-F76  <=  WK-CHG-HIDUKE
                     MOVE   TOK-F77       TO   FUR-F01
             END-IF
     END-IF.
*　伝票区分
     MOVE    DEN-F051             TO   FUR-F02.
*　伝票番号
     MOVE    DEN-F02              TO   FUR-F03.
*　行番号
     MOVE    DEN-F03              TO   FUR-F04.
*　赤黒区分
     MOVE    DEN-F04              TO   FUR-F05.
*　得意先／仕入先
     MOVE    TOK-F52              TO   FUR-F06.
*　納入先ＣＤ
     MOVE    DEN-F07              TO   FUR-F07.
*　発注日
     MOVE    DEN-F111             TO   WK-CHG-HIDUKE.
     MOVE    WK-CHG-HIDUKE        TO   WK-HIDUKE.
     MOVE    WK-HIDUKE-1          TO   FUR-F08(1:4).
     MOVE    "/"                  TO   FUR-F08(5:1).
     MOVE    WK-HIDUKE-2          TO   FUR-F08(6:2).
     MOVE    "/"                  TO   FUR-F08(8:1).
     MOVE    WK-HIDUKE-3          TO   FUR-F08(9:2).
*　納品日
     MOVE    DEN-F112             TO   WK-CHG-HIDUKE.
     MOVE    WK-CHG-HIDUKE        TO   WK-HIDUKE.
     MOVE    WK-HIDUKE-1          TO   FUR-F09(1:4).
     MOVE    "/"                  TO   FUR-F09(5:1).
     MOVE    WK-HIDUKE-2          TO   FUR-F09(6:2).
     MOVE    "/"                  TO   FUR-F09(8:1).
     MOVE    WK-HIDUKE-3          TO   FUR-F09(9:2).
*　出荷日
     MOVE    DEN-F113             TO   WK-CHG-HIDUKE.
     MOVE    WK-CHG-HIDUKE        TO   WK-HIDUKE.
     MOVE    WK-HIDUKE-1          TO   FUR-F10(1:4).
     MOVE    "/"                  TO   FUR-F10(5:1).
     MOVE    WK-HIDUKE-2          TO   FUR-F10(6:2).
     MOVE    "/"                  TO   FUR-F10(8:1).
     MOVE    WK-HIDUKE-3          TO   FUR-F10(9:2).
*　サカタ商品ＣＤ
     MOVE    DEN-F1411            TO   FUR-F11.
*　サカタ品単ＣＤ
     MOVE    DEN-F1412(1:5)       TO   FUR-F12.
     MOVE    DEN-F1412(6:2)       TO   FUR-F13.
     MOVE    DEN-F1412(8:1)       TO   FUR-F14.
*　ストック番号
*　数量
     MOVE    DEN-F15              TO   WK-HEN-SU.
*****判定
     IF    ( DEN-F051  =  40 )
     AND   ( DEN-F04   =  1  OR  3  OR  5 )
             COMPUTE  WK-HEN-SU  =  WK-HEN-SU  *  -1
     END-IF.
     IF    ( DEN-F051  =  41 OR  42 )
     AND   ( DEN-F04   =  0  OR  2  OR  4 )
             COMPUTE  WK-HEN-SU  =  WK-HEN-SU  *  -1
     END-IF.
     MOVE    WK-HEN-SU            TO   WK-TANKA.
     IF      WK-HEN-SU  >= ZERO
             MOVE   "0"           TO   FUR-F16(1:1)
     ELSE
             MOVE   "-"           TO   FUR-F16(1:1)
     END-IF.
     MOVE    WK-TANKA-1           TO   FUR-F16(2:7).
     MOVE    "."                  TO   FUR-F16(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F16(10:2).
*　単価区分
     MOVE    DEN-F16              TO   FUR-F17.
*　単価
     MOVE    DEN-F172             TO   WK-TANKA.
     MOVE    "0"                  TO   FUR-F18(1:1).
     MOVE    WK-TANKA-1           TO   FUR-F18(2:7).
     MOVE    "."                  TO   FUR-F18(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F18(10:2).
*　金額
     MOVE    DEN-F181             TO   WK-HEN-SU.
*****判定
     IF    ( DEN-F051  =  40 )
     AND   ( DEN-F04   =  1  OR  3  OR  5 )
             COMPUTE  WK-HEN-SU  =  WK-HEN-SU  *  -1
     END-IF.
     IF    ( DEN-F051  =  41 OR  42 )
     AND   ( DEN-F04   =  0  OR  2  OR  4 )
             COMPUTE  WK-HEN-SU  =  WK-HEN-SU  *  -1
     END-IF.
     MOVE    WK-HEN-SU            TO   WK-TANKA.
     IF      WK-HEN-SU  >= ZERO
             MOVE   "0"           TO   FUR-F19(1:1)
     ELSE
             MOVE   "-"           TO   FUR-F19(1:1)
     END-IF.
     MOVE    WK-TANKA-1           TO   FUR-F19(2:7).
     MOVE    "."                  TO   FUR-F19(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F19(10:2).
*　場所ＣＤ
     MOVE    DEN-F08              TO   FUR-F20.
*　入出庫区分
     IF    ( DEN-F051  =  40 )
     AND   ( DEN-F04   =  1  OR  3  OR  5 )
             MOVE   "1"           TO   FUR-F21
     ELSE
             MOVE   "2"           TO   FUR-F21
     END-IF.
     IF    ( DEN-F051  =  41 OR  42 )
     AND   ( DEN-F04   =  0  OR  2  OR  4 )
             MOVE   "1"           TO   FUR-F21
     ELSE
             MOVE   "2"           TO   FUR-F21
     END-IF.
*　税区分
     MOVE    DEN-F34              TO   FUR-F23.
*　作業明細区分
*　備考　　
     MOVE    DEN-F22              TO   FUR-F24.
*　注文番号
*　量販店商品ＣＤ
     MOVE    DEN-F25              TO   FUR-F26.
*　担当者ＣＤ
     MOVE    DEN-F06              TO   FUR-F27.
*　請求区分
*　処理日
     MOVE    DATE-AREA            TO   WK-HIDUKE.
     MOVE    WK-HIDUKE-1          TO   FUR-F29(1:4).
     MOVE    "/"                  TO   FUR-F29(5:1).
     MOVE    WK-HIDUKE-2          TO   FUR-F29(6:2).
     MOVE    "/"                  TO   FUR-F29(8:1).
     MOVE    WK-HIDUKE-3          TO   FUR-F29(9:2).
*　量販店番号(F30)
*　支払締日(F31)
*　単価１（仕入単価）
     MOVE    DEN-F171             TO   WK-TANKA.
     MOVE    "0"                  TO   FUR-F32(1:1).
     MOVE    WK-TANKA-1           TO   FUR-F32(2:7).
     MOVE    "."                  TO   FUR-F32(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F32(10:2).
*　単価２（売価単価）
     MOVE    DEN-F173             TO   WK-TANKA.
     MOVE    "0"                  TO   FUR-F33(1:1).
     MOVE    WK-TANKA-1           TO   FUR-F33(2:7).
     MOVE    "."                  TO   FUR-F33(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F33(10:2).
*　営業第２部取引先ＣＤ
     MOVE    DEN-F01              TO   FUR-F34.
*　受注数量
     MOVE    DEN-F50              TO   WK-HEN-SU.
*****判定
     IF    ( DEN-F051  =  40 )
     AND   ( DEN-F04   =  1  OR  3  OR  5 )
             COMPUTE  WK-HEN-SU  =  WK-HEN-SU  *  -1
     END-IF.
     IF    ( DEN-F051  =  41 OR  42 )
     AND   ( DEN-F04   =  0  OR  2  OR  4 )
             COMPUTE  WK-HEN-SU  =  WK-HEN-SU  *  -1
     END-IF.
     MOVE    WK-HEN-SU            TO   WK-TANKA.
     IF      WK-HEN-SU  >= ZERO
             MOVE   "0"           TO   FUR-F35(1:1)
     ELSE
             MOVE   "-"           TO   FUR-F35(1:1)
     END-IF.
     MOVE    WK-TANKA-1           TO   FUR-F35(2:7).
     MOVE    "."                  TO   FUR-F35(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F35(10:2).
*　ＪＡＮＣＤ／インストアＣＤ（サブ商品変換ＴＢＬより）
     IF      SUBTBLF-INV-FLG  =  SPACE
             MOVE    TBL-F13      TO   FUR-F36
             MOVE    TBL-F14      TO   FUR-F37
             IF   TBL-F13 = SPACE
                  IF  DEN-F25(13:1) = SPACE
                      MOVE SPACE       TO  FUR-F36
                      MOVE DEN-F25     TO  FUR-F37
                  ELSE
                      MOVE DEN-F25     TO  FUR-F36
                      MOVE SPACE       TO  FUR-F37
                  END-IF
             END-IF
     ELSE
             IF  DEN-F25(13:1) = SPACE
                 MOVE SPACE       TO  FUR-F36
                 MOVE DEN-F25     TO  FUR-F37
             ELSE
                 MOVE DEN-F25     TO  FUR-F36
                 MOVE SPACE       TO  FUR-F37
             END-IF
     END-IF.
*　訂正数（欠品数）
     COMPUTE  WK-HEN-SU  =  DEN-F50  -  DEN-F15.
     MOVE    WK-HEN-SU            TO   WK-TANKA.
     IF      WK-HEN-SU  >= ZERO
             MOVE   "0"           TO   FUR-F38(1:1)
     ELSE
             MOVE   "-"           TO   FUR-F38(1:1)
     END-IF.
     MOVE    WK-TANKA-1           TO   FUR-F38(2:7).
     MOVE    "."                  TO   FUR-F38(9:1).
     MOVE    WK-TANKA-2           TO   FUR-F38(10:2).
*　生産者（ナフコ）
     MOVE    DEN-F48              TO   FUR-F39.
*　物流区分
*　商品形状区分
     IF   DEN-F274  =  0
          GO                        TO   MAIN-010
     END-IF.
*  ホーマックの時
     IF   DEN-F01  =  880 OR 882 OR 883 OR 1427 OR 14272 OR 14273
          MOVE   DEN-REC            TO   HOR-REC
          MOVE   HOR-A163           TO   FUR-F41
          MOVE   HOR-A161           TO   FUR-F42
     END-IF.
*  カーマの時
     IF   DEN-F01  =  13938 OR 139381 OR 17137 OR 171371
          MOVE   DEN-REC            TO   KAM-REC
          MOVE   HOR-A163           TO   FUR-F41
          MOVE   HOR-A161           TO   FUR-F42
     END-IF.
*  ダイキの時
     IF   DEN-F01  =  100403 OR 100404 OR 100427 OR 100428 OR
                      100441 OR 100442
          MOVE   DEN-REC            TO   DAI-REC
          MOVE   DAI-A35            TO   FUR-F41
          MOVE   DAI-A182           TO   FUR-F42
     END-IF.
*  ケーヨーの時
     IF   DEN-F01  =  1731 OR 1732 OR 7601 OR 7602
          MOVE   DEN-REC            TO   KEI-REC
          MOVE   KEI-A37            TO   FUR-F41
          MOVE   KEI-A35            TO   FUR-F42
     END-IF.
 MAIN-010.
*　小売連携区分
     MOVE    DEN-F32              TO   FUR-F43.
*　新日付管理マスタ
     IF      DATECKF-INV-FLG =  SPACE
             MOVE    DAT-F05(1:4) TO   FUR-F44(1:4)
             MOVE    "/"          TO   FUR-F44(5:1)
             MOVE    DAT-F05(5:2) TO   FUR-F44(6:2)
             MOVE    "/"          TO   FUR-F44(8:1)
             MOVE    "01"         TO   FUR-F44(9:2)
             MOVE    DAT-F04      TO   FUR-F45
     END-IF.
*　未計上データ区分
     MOVE    "1"                  TO   FUR-F46.
*　手書オンライン区分
     MOVE    DEN-F274             TO   FUR-F47.
*連携：商品変換ＴＢＬ更新
     WRITE   FUR-REC.
     ADD     1                    TO   WT-CNT.
*
     PERFORM  SHTDENF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            売上伝票ファイル読込
************************************************************
 SHTDENF-READ-SEC                   SECTION.
*
     READ    SHTDENF
        AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        SHTDENF-READ-EXT
     END-READ.
*
     ADD     1                   TO   IN-CNT.
     IF      IN-CNT(4:4) = "0000" OR "5000"
             DISPLAY "ｼｮﾘｹﾝｽｳ = " IN-CNT  UPON CONS
     END-IF.
** 行Ｎ-が１２より大きい時読み飛ばし
     IF    DEN-F03     >     12
           GO                TO   SHTDENF-READ-SEC
     END-IF.
*計上済データは対象外とする
     IF    DEN-F277          =    9
           MOVE   "END"      TO   END-FLG
           GO                TO   SHTDENF-READ-EXT
     END-IF.
*
*引落済の時　処理対象外
     IF    DEN-F27C    NOT =      ZERO
           GO                TO   SHTDENF-READ-SEC
     END-IF.
*
*伝票区分が４０の伝票のみ対象とする
     IF     DEN-F051       =   40  OR  41  OR  42
             CONTINUE
     ELSE
           GO                TO   SHTDENF-READ-SEC
     END-IF.
*
 SHTDENF-READ-EXT.
     EXIT.
************************************************************
*    商品名称マスタ読込
************************************************************
 SUBTBLF-READ-SEC                    SECTION.
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
*    取引先マスタ読込
************************************************************
 HTOKMS-READ-SEC                    SECTION.
*
     READ    HTOKMS
             INVALID
             MOVE      "INV"        TO   HTOKMS-INV-FLG
             NOT  INVALID
             MOVE      SPACE        TO   HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
*
     CLOSE             SHTDENF  SUBTBLF  DATECKF  BITRNSET
                       HTOKMS.
*
     DISPLAY "* TRN" NC"抽出" ":" NC"未計上分" "   *"
              UPON CONS.
     DISPLAY "* READ-CNT = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* WT-CNT   = " WT-CNT   " *"  UPON CONS.
*パラメタセット
     MOVE       WT-CNT              TO   PARA-OUTKENSU.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
