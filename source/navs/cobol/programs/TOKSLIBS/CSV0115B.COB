****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＸＣＥＬ連携                    *
*    モジュール名　　　　：　ＣＳＶデータ作成                  *
*    作成日／更新日　　　：　03/07/18                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　各編集Ｆを読み、ＣＳＶデータを作成*
*                          する。　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CSV0115B.
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
****<< 発注データ >>************************************
     SELECT   HACYUDT  ASSIGN    TO        DA-01-S-HACYUDT
                        FILE      STATUS    IS   HAC-STATUS.
****<< ＣＳＶデータ     >>************************************
     SELECT   CSVDT1   ASSIGN    TO        DA-01-S-CSVDT1
                        FILE      STATUS    IS   CSV-STATUS.
****<< 店舗マスタ  >>*****************************************
     SELECT   TENMS1    ASSIGN    TO        01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS MODE         IS   RANDOM
                        RECORD KEY          IS   TEN-F52  TEN-F011
                        FILE STATUS         IS   TEN-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 発注データ >>*********************************
 FD  HACYUDT           BLOCK CONTAINS 27   RECORDS
                       LABEL RECORD   IS   STANDARD.
     COPY     HACYUDT OF         XFDLIB
              JOINING  HAC        PREFIX.
****<< ＣＳＶデータ  >>****************************
 FD  CSVDT1             BLOCK CONTAINS 23   RECORDS
                       LABEL RECORD   IS   STANDARD.
     COPY     CSVDT1 OF         XFDLIB
              JOINING  CSV        PREFIX.
****<< 店舗マスタ  >>**************************************
 FD  TENMS1.
     COPY        HTENMS      OF  XFDLIB
     JOINING     TEN         AS  PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 HAC-STATUS           PIC  X(02).
     02 CSV-STATUS           PIC  X(02).
     02 TEN-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "CSV0115B".
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
 01  TENPO-END               PIC  X(03)  VALUE  SPACE.
 01  IX                      PIC  9(03)  VALUE  ZERO.
 01  IY                      PIC  9(03)  VALUE  ZERO.
 01  IZ                      PIC  9(04)  VALUE  ZERO.
 01  IA                      PIC  9(01)  VALUE  ZERO.
 01  BAIKA-KEI               PIC  9(09)  VALUE  ZERO.
****  カウント                ****
 01  CNT-AREA.
     03  SYO-CNT             PIC  9(07)  VALUE  ZERO.
     03  CSV-CNT             PIC  9(07)  VALUE  ZERO.
     03  PAGE-CNT            PIC  9(04)  VALUE  ZERO.
     03  MEISAI-CNT          PIC  9(01)  VALUE  ZERO.
     03  OUTPUT-CNT          PIC  9(07)  VALUE  ZERO.
     03  MEISAI-CNT-P        PIC  9(05)  VALUE  ZERO.
****  キー退避                ****
 01  KEY-BACKUP.
     03  WK-SYO-F02          PIC  X(02)   VALUE  SPACE.
     03  WK-SYO-F03          PIC  9(08)   VALUE  ZERO.
     03  WK-SYO-F06          PIC  9(01)   VALUE  ZERO.
     03  WK-SYO-F07          PIC  9(04)   VALUE  ZERO.
****  日付編集
 01  HENSYU-YMD.
     03  HIDUKE-Y            PIC  9(04)   VALUE  ZERO.
     03  FILLER              PIC  X(01)   VALUE  "/".
     03  HIDUKE-M            PIC  9(02)   VALUE  ZERO.
     03  FILLER              PIC  X(01)   VALUE  "/".
     03  HIDUKE-D            PIC  9(02)   VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
************************************************************
 PROCEDURE              DIVISION.
************************************************************
 DECLARATIVES.
***
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACYUDT.
     MOVE   "HACYUDT"        TO    ERR-FL-ID.
     MOVE    HAC-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  CSVDT1.
     MOVE   "CSVDT1 "        TO    ERR-FL-ID.
     MOVE    CSV-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  TENMS1.
     MOVE   "TENMS1  "        TO    ERR-FL-ID.
     MOVE    TEN-STATUS       TO    ERR-STCD.
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
     OPEN         INPUT     HACYUDT  TENMS1.
     OPEN         OUTPUT    CSVDT1.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
     PERFORM HACYUDT-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.

*    明細出力ｶｳﾝﾀｰｶｳﾝﾄｱｯﾌﾟ
     ADD      1              TO  MEISAI-CNT.
     PERFORM  MEISAI-WRT-SEC.
*
     PERFORM HACYUDT-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    発注データ読込み
************************************************************
 HACYUDT-READ-SEC                 SECTION.
*
     READ    HACYUDT
             AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        HACYUDT-READ-EXIT
     END-READ.
     ADD     1         TO        CSV-CNT.
*
 HACYUDT-READ-EXIT.
     EXIT.
************************************************************
*    明細出力
************************************************************
 MEISAI-WRT-SEC                    SECTION.
*    ﾚｺｰﾄﾞ初期化
     MOVE    SPACE               TO   CSV-REC.
     INITIALIZE                       CSV-REC.
*    カンマセット
     MOVE    ","                 TO   CSV-A01
                                      CSV-A02 CSV-A03 CSV-A04
                                      CSV-A05 CSV-A06 CSV-A07
                                      CSV-A08 CSV-A09 CSV-A10
                                      CSV-A11 CSV-A12 CSV-A13
                                      CSV-A14 CSV-A15.
*    通し番号ｶｳﾝﾄｱｯﾌﾟ
     ADD     1                   TO   OUTPUT-CNT.

*    日付(納品日)
     MOVE    HAC-F03             TO   CSV-F01.
*    店番
     MOVE    HAC-F06             TO   CSV-F02.
*    店舗名の設定
     MOVE  HAC-F01         TO  TEN-F52.
     MOVE  HAC-F06         TO  CSV-F02  TEN-F011.
     MOVE  X"28"           TO  CSV-F031.
     MOVE  X"29"           TO  CSV-F033.
           READ  TENMS1
              INVALID  KEY
                 MOVE  SPACE     TO  CSV-F032
              NOT INVALID  KEY
                 MOVE  TEN-F02   TO  CSV-F032
           END-READ.
*    自社商品コード
     MOVE    HAC-F05             TO   CSV-F04.
*    相手商品コード
     MOVE    HAC-F04             TO   CSV-F05.
*    商品名
     MOVE    HAC-F101            TO   CSV-F06.
*    規格
     MOVE    HAC-F102            TO   CSV-F07.
*    原価
     MOVE    HAC-F11             TO   CSV-F08.
*    売価
     MOVE    HAC-F12             TO   CSV-F09.
*    入数
     MOVE    ZERO                TO   CSV-F10.
*    数量
     MOVE    HAC-F15             TO   CSV-F11.
*    バッチ日付
     MOVE    HAC-F16             TO   CSV-F12.
*    バッチ時刻
     MOVE    HAC-F17             TO   CSV-F13.
*    バッチ取引先
     MOVE    HAC-F18             TO   CSV-F14.
*    _番
     MOVE    HAC-F19             TO   CSV-F15.
*    部門
     MOVE    HAC-YOBI            TO   CSV-F16.

*    出力ﾚｺｰﾄﾞ初期化
*****MOVE    SPACE               TO   CSV-REC.
*****INITIALIZE                       CSV-REC.
*    ﾚｺｰﾄﾞ出力
     WRITE     CSV-REC.
*
 MEISAI-WRT-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
*

*ﾌｧｲﾙCLOSE
     CLOSE             HACYUDT TENMS1 CSVDT1.
*ﾚｺｰﾄﾞｶｳﾝﾀｰ情報出力
     DISPLAY "READ-CNT   = " CSV-CNT     UPON CONS.
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
