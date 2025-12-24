****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＢＩツール連携　　　　　　　　　　*
*    モジュール名　　　　：　ＢＩ連携用：日次計上データセット　*
*    作成日／更新日　　　：　2018/12/19                        *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　日次計上ファイルより、ＢＩ編集用Ｆ*
*                          を抽出する。　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBI0100B.
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
****<< 日次計上ファイル  >>***********************************
     SELECT   TOKUJ     ASSIGN    TO        DA-01-S-TOKUJ
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   TOK-STATUS.
****<< 売上伝票ファイル  >>***********************************
     SELECT   SHTDENF  ASSIGN     TO        DA-01-VI-SHTDENL1
                       ORGANIZATION         IS   INDEXED
                       ACCESS     MODE      IS   RANDOM
                       RECORD     KEY       IS   DEN-F01  DEN-F02
                                                 DEN-F04  DEN-F051
                                                 DEN-F07  DEN-F112
                                                 DEN-F03
                        FILE      STATUS    IS   DEN-STATUS.
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
****<< 商品名称マスタ　  >>***********************************
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
****<< ＢＩ編集用ファイル　  >>****************************
     SELECT   BITRNDTF  ASSIGN   TO         DA-01-VI-BITRNDT1
                        ORGANIZATION        IS   INDEXED
                        ACCESS  MODE        IS   RANDOM
                        RECORD    KEY       IS   TRN-F01
                                                 TRN-F02
                                                 TRN-F03
                        FILE STATUS         IS   TRN-STATUS.
***************************************************************
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
****<< 日次計上ファイル  >>***********************************
 FD  TOKUJ    BLOCK  CONTAINS  4  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     TOKUJ     OF        XFDLIB
              JOINING   TOK       PREFIX.
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
****<< 商品名称マスタ　 >>************************************
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
****<< ＢＩ編集用ファイル　  >>*****************************
 FD  BITRNDTF.
     COPY     BITRNDTF  OF        XFDLIB
              JOINING   TRN       PREFIX.
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
     02 TOK-STATUS           PIC  X(02).
     02 DEN-STATUS           PIC  X(02).
     02 TBL-STATUS           PIC  X(02).
     02 DAT-STATUS           PIC  X(02).
     02 MEI-STATUS           PIC  X(02).
     02 TRN-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBI0100B".
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
 01  SHTDENF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  SUBTBLF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  DATECKF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  JYOTAI-FLG              PIC  X(01)  VALUE  SPACE.
 01  FUGO-FLG                PIC  X(01)  VALUE  SPACE.
 01  ERR-FLG                 PIC  X(01)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  WT-CNT              PIC  9(06)   VALUE  0.
     03  RENBAN-CNT          PIC  9(08)   VALUE  0.
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
*  文字列→数値変換
 01  WK-9-KETA               PIC  X(09).
 01  WK-9-KETA-R             REDEFINES   WK-9-KETA.
     03  WK-9-KETA-H         PIC  9(09).
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
                   PROCEDURE  TOKUJ.
     MOVE   "TOKUJ   "        TO    ERR-FL-ID.
     MOVE    TOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SHTDENF.
     MOVE   "SHTDENL1"        TO    ERR-FL-ID.
     MOVE    DEN-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBTBLF.
     MOVE   "SUBTBLL1"        TO    ERR-FL-ID.
     MOVE    TBL-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DATECKF.
     MOVE   "DATECKL1"        TO    ERR-FL-ID.
     MOVE    DAT-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HMEIMS.
     MOVE   "MEIMS1  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC6           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  BITRNDTF.
     MOVE   "BITRNDT1"        TO    ERR-FL-ID.
     MOVE    TRN-STATUS       TO    ERR-STCD.
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
*初期化
     MOVE      ZERO               TO   CNT-AREA.
*ファイルＯＰＥＮ
     OPEN      INPUT    TOKUJ    SHTDENF.
     OPEN      INPUT    SUBTBLF  DATECKF  HMEIMS.
     OPEN      I-O      BITRNDTF.
*    売上伝票Ｆ読込
     PERFORM  TOKUJ-READ-SEC.
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
*ＢＩ編集用ファイル作成
     MOVE    SPACE                TO   TRN-REC.
     INITIALIZE                        TRN-REC.
*    新日付管理マスタ索引
     MOVE    TOK-F10              TO   DAT-F01.
     IF      TOK-F10  =  ZERO
             MOVE     DEN-F09     TO   DAT-F01
     END-IF.
     PERFORM DATECKF-READ-SEC.
*    エラー区分初期化
     MOVE    SPACE                TO   ERR-FLG.
**マスタ／関連ファイル索引
     IF   TOK-F02  =  40 OR 41 OR 42 OR 44 OR 45  OR  70  OR  71
     OR   80  OR  81
*         ＳＵＢ商品変換ＴＢＬマスタ索引
          MOVE    TOK-F39              TO   TBL-F01
          MOVE    TOK-F24              TO   TBL-F02
          PERFORM  SUBTBLF-READ-SEC
*         商品名称マスタ索引
          MOVE    TOK-F11              TO   MEI-F011
          MOVE    TOK-F12(1:5)         TO   MEI-F0121
          MOVE    TOK-F12(6:2)         TO   MEI-F0122
          MOVE    TOK-F12(8:1)         TO   MEI-F0123
          PERFORM  HMEIMS-READ-SEC
*         売上伝票ファイル索引
          MOVE    TOK-F39              TO   DEN-F01
          MOVE    TOK-F03              TO   WK-9-KETA
          MOVE    WK-9-KETA-R          TO   DEN-F02
          MOVE    TOK-F05              TO   DEN-F04
          MOVE    TOK-F02              TO   DEN-F051
          MOVE    TOK-F07              TO   DEN-F07
          MOVE    TOK-F09              TO   DEN-F112
          MOVE    TOK-F04              TO   DEN-F03
          PERFORM  SHTDENF-READ-SEC
          MOVE    "1"                  TO   JYOTAI-FLG
     ELSE
          MOVE    "2"                  TO   JYOTAI-FLG
     END-IF.
*項目セット
*  作成日付
     MOVE         SYS-DATE        TO   TRN-F01.
*　作成時刻
     MOVE         WK-TIME(1:6)    TO   TRN-F02.
*　連番
     ADD          1               TO   RENBAN-CNT.
     MOVE         RENBAN-CNT      TO   TRN-F03.
*　得意先／仕入先ＣＤ
     MOVE         TOK-F06         TO   TRN-F04.
*  伝票番号
     MOVE         TOK-F03         TO   TRN-F05.
*  赤黒区分
     MOVE         TOK-F05         TO   TRN-F06.
*  伝票区分
     MOVE         TOK-F02         TO   TRN-F07.
*  納入先ＣＤ
     MOVE         TOK-F07         TO   TRN-F08.
*  出荷日
     MOVE         TOK-F10         TO   TRN-F09.
*　行番号
     MOVE         TOK-F04         TO   TRN-F10.
*　計上ＤＴレコ－ドセット
     MOVE         TOK-REC         TO   TRN-F100.
*  数量、単価、金額、単価１、単価２、受注数量数値以外０セット
     IF   TRN-F115  NOT  NUMERIC
          MOVE    ZERO            TO   TRN-F115
     END-IF.
     IF   TRN-F116  NOT  NUMERIC
          MOVE    ZERO            TO   TRN-F116
     END-IF.
     IF   TRN-F117  NOT  NUMERIC
          MOVE    ZERO            TO   TRN-F117
     END-IF.
     IF   TRN-F137  NOT  NUMERIC
          MOVE    ZERO            TO   TRN-F137
     END-IF.
     IF   TRN-F138  NOT  NUMERIC
          MOVE    ZERO            TO   TRN-F138
     END-IF.
     IF   TRN-F141  NOT  NUMERIC
          MOVE    ZERO            TO   TRN-F141
     END-IF.
*　符号判定
*****符号ＦＬＧ＝２のとき、数量、金額をマイナスする。
     EVALUATE  TOK-F02
         WHEN  40   WHEN  44  WHEN  70  WHEN  80
         IF    TOK-F05  =  0   OR  2   OR  4
               MOVE  "1"            TO   FUGO-FLG
         ELSE
               MOVE  "2"            TO   FUGO-FLG
         END-IF
         WHEN  41   WHEN  42  WHEN  45  WHEN  71  WHEN  81
         IF  TOK-F05  =  0   OR  2   OR  4
             MOVE  "2"            TO   FUGO-FLG
         ELSE
             MOVE  "1"            TO   FUGO-FLG
         END-IF
         WHEN  50   WHEN  60
             MOVE  "1"            TO   FUGO-FLG
         WHEN  51   WHEN  61
             MOVE  "2"            TO   FUGO-FLG
         WHEN  30   WHEN  31  WHEN  32
             IF   TOK-F19  =  "1"
                  MOVE   "1"      TO   FUGO-FLG
             ELSE
                  MOVE   "2"      TO   FUGO-FLG
             END-IF
         WHEN  OTHER
             DISPLAY NC"＃エラー伝区" " = " DEN-F02 " " NC"＃"
                     UPON CONS
             MOVE  "1"            TO   ERR-FLG
             MOVE  "1"            TO   TRN-F11
     END-EVALUATE.
*    数量符号セット
     IF      FUGO-FLG  =  "2"
        IF   TRN-F115  NUMERIC
             COMPUTE  TRN-F115  =  TRN-F115  *  -1
        END-IF
        IF   TRN-F117  NUMERIC
             COMPUTE  TRN-F117  =  TRN-F117  *  -1
        END-IF
        IF   TRN-F141  NUMERIC
             COMPUTE  TRN-F141  =  TRN-F141  *  -1
        END-IF
     END-IF.
*****売上系以外は読み飛ばす
     IF      JYOTAI-FLG  =  "2"
             GO                   TO   MAIN-010
     END-IF.
*　ＪＡＮＣＤ／インストアＣＤ（サブ商品変換ＴＢＬより）
     IF      SUBTBLF-INV-FLG  =  SPACE
             MOVE    TBL-F13      TO   TRN-F201
             MOVE    TBL-F14      TO   TRN-F202
             IF   TBL-F13 = SPACE
                  IF  TOK-F24(13:1) = SPACE
                      MOVE SPACE       TO  TRN-F201
                      MOVE TOK-F24     TO  TRN-F202
                  ELSE
                      MOVE TOK-F24     TO  TRN-F201
                      MOVE SPACE       TO  TRN-F202
                  END-IF
             END-IF
     ELSE
             IF  TOK-F24(13:1) = SPACE
                 MOVE SPACE       TO  TRN-F201
                 MOVE TOK-F24     TO  TRN-F202
             ELSE
                 MOVE TOK-F24     TO  TRN-F201
                 MOVE SPACE       TO  TRN-F202
             END-IF
     END-IF.
*　訂正数（欠品数）
     COMPUTE  WK-HEN-SU  =  TRN-F141 -  TRN-F115.
     MOVE    WK-HEN-SU            TO  TRN-F203
*　生産者（ナフコ）
     IF      SHTDENF-INV-FLG  =  SPACE
             MOVE    DEN-F48      TO  TRN-F204
             IF      DEN-F48  =  SPACE
                     MOVE DEN-F09 TO  TRN-F204
             END-IF
     END-IF.
*　物流区分
*　商品形状区分
*  ホーマックの時
     IF   SHTDENF-INV-FLG  =  SPACE
       IF   DEN-F01  =  880 OR 882 OR 883 OR 1427 OR 14272 OR 1427
          MOVE   DEN-REC            TO   HOR-REC
          MOVE   HOR-A163           TO   TRN-F206
          MOVE   HOR-A161           TO   TRN-F207
       END-IF
*  カーマの時
       IF   DEN-F01  =  13938 OR 139381 OR 17137 OR 171371
          MOVE   DEN-REC            TO   KAM-REC
          MOVE   HOR-A163           TO   TRN-F206
          MOVE   HOR-A161           TO   TRN-F207
       END-IF
*  ダイキの時
       IF   DEN-F01  =  100403 OR 100404 OR 100427 OR 100428 OR
                      100441 OR 100442
          MOVE   DEN-REC            TO   DAI-REC
          MOVE   DAI-A35            TO   TRN-F206
          MOVE   DAI-A182           TO   TRN-F207
       END-IF
*  ケーヨーの時
       IF   DEN-F01  =  1731 OR 1732 OR 7601 OR 7602
          MOVE   DEN-REC            TO   KEI-REC
          MOVE   KEI-A37            TO   TRN-F206
          MOVE   KEI-A35            TO   TRN-F207
       END-IF
*　小売連携区分
       MOVE    DEN-F32              TO   TRN-F208
*　手書オンライン区分
       MOVE    DEN-F274             TO   TRN-F212
     END-IF.
 MAIN-010.
*　新日付管理マスタ
     IF      DATECKF-INV-FLG =  SPACE
             MOVE    DAT-F05      TO   TRN-F209
             MOVE    DAT-F04      TO   TRN-F210
     END-IF.
*　未計上データ区分
     MOVE    SPACE                TO   TRN-F211.
*連携：商品変換ＴＢＬ更新
     WRITE   TRN-REC.
     ADD     1                    TO   WT-CNT.
*
     PERFORM  TOKUJ-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            売上伝票ファイル読込
************************************************************
 TOKUJ-READ-SEC                   SECTION.
*
     READ    TOKUJ
             AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        TOKUJ-READ-EXT
     END-READ.
*
     ADD     1                   TO   IN-CNT.
     IF      IN-CNT(4:4) = "0000" OR "5000"
             DISPLAY "ｼｮﾘｹﾝｽｳ = " IN-CNT  UPON CONS
     END-IF.
*
 TOKUJ-READ-EXT.
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
*    売上伝票ファイル読込
************************************************************
 SHTDENF-READ-SEC                    SECTION.
*
     READ    SHTDENF
             INVALID
             MOVE      "INV"        TO   SHTDENF-INV-FLG
             NOT  INVALID
             MOVE      SPACE        TO   SHTDENF-INV-FLG
     END-READ.
*
 SHTDENF-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
*
     CLOSE             SHTDENF  SUBTBLF  DATECKF  BITRNDTF
                       HMEIMS   TOKUJ.
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
