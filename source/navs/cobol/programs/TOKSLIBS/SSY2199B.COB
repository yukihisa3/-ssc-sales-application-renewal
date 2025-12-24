****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　カタクラ　ＪＡＮＣＤ対応　　　　　*
*    モジュール名　　　　：　商品データコンバート　　　　　　　*
*    作成日／更新日　　　：　03/07/03                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　カタクラ商品データを順で読み、　　*
*　　　　　　　　　　　　　　商品変換テーブルに出力する。　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY2199B.
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
****<<　　　　　商品変換テーブル　　　　　　 >>*****************
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TBL-F01
                                                 TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
****<<　カタクラ商品データ　　　　　　　 >>*********************
     SELECT   KATASYO   ASSIGN    TO        DA-01-S-KATASYO
                        FILE      STATUS    IS   KTS-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 商品変換テーブル　　　　　　　　　>>********************
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
****<< カタクラ商品データ>>******************************
 FD  KATASYO
              BLOCK CONTAINS  53  RECORDS.
     COPY     KATASYO   OF        XFDLIB
              JOINING   KTS       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TBL-STATUS           PIC  X(02).
     02 KTS-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSY2199B".
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
 01  HSHOTBL-INV-FLG         PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)     VALUE  0.
     03  OUT-CNT1            PIC  9(07)     VALUE  0.

****システム日付　　　　      ****
 01  SYSDATE                 PIC  9(08)     VALUE  ZERO.
****日付変換サブルーチン用ワーク*****
 01  LINK-IN-KBN             PIC  X(01).
 01  LINK-IN-YMD6            PIC  9(06).
 01  LINK-IN-YMD8            PIC  9(08).
 01  LINK-OUT-RET            PIC  X(01).
 01  LINK-OUT-YMD            PIC  9(08).
****  日付取得                ****
 01  WK-DATE8                PIC  9(08).
*
************************************************************
 PROCEDURE              DIVISION.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HSHOTBL.
     MOVE   "HSHOTBL  "       TO    ERR-FL-ID.
     MOVE    TBL-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  KATASYO.
     MOVE   "KATASYO   "      TO    ERR-FL-ID.
     MOVE    KTS-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
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
*     100     初期処理
************************************************************
 100-INIT-SEC                     SECTION.
*
     ACCEPT   SYSDATE        FROM      DATE.
     MOVE     "3"            TO        LINK-IN-KBN.
     MOVE     SYSDATE        TO        LINK-IN-YMD6.
     MOVE     ZERO           TO        LINK-IN-YMD8.
     MOVE     ZERO           TO        LINK-OUT-RET.
     MOVE     ZERO           TO        LINK-OUT-YMD.
     CALL     "SKYDTCKB"     USING     LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD   TO        WK-DATE8.
     OPEN     I-O       HSHOTBL.
     OPEN     INPUT     KATASYO.
*カタクラ商品データ読み込み（初期）
     PERFORM  KATASYO-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*     200     主処理
************************************************************
 200-MAIN-SEC                     SECTION.
*取扱先ＣＤと量販店商品ＣＤを商品変換テーブル索引キーに設定
      MOVE   KTS-F01               TO   TBL-F01.
      MOVE   KTS-F02               TO   TBL-F02.
*商品変換テーブルの索引　　　　　
      READ    HSHOTBL
              INVALID        MOVE  "INV"    TO HSHOTBL-INV-FLG
              NOT INVALID    MOVE  SPACE    TO HSHOTBL-INV-FLG
      END-READ.
*データ項目のセット
*レコード初期化
      MOVE    SPACE                TO   TBL-REC.
      INITIALIZE                        TBL-REC.
*取引先ＣＤ
      MOVE    KTS-F01              TO   TBL-F01.
*相手商品ＣＤ
      MOVE    KTS-F02              TO   TBL-F02.
*商品ＣＤ
      MOVE    KTS-F03              TO   TBL-F031.
*単品１
      MOVE    KTS-F04              TO   TBL-F0321.
*単品２
      MOVE    KTS-F05              TO   TBL-F0322.
*単品３
      MOVE    KTS-F06              TO   TBL-F0323.
*出荷場所
      MOVE    KTS-F12              TO   TBL-F04.
*原価単価
      COMPUTE      TBL-F05  =     KTS-F10 / 100.
*売価単価
      COMPUTE      TBL-F06  =     KTS-F11 / 100.
*分類ＣＤ
      MOVE    KTS-F08              TO   TBL-F07.
*_番１
      MOVE    KTS-F07              TO   TBL-F08.
*仕入単価
      COMPUTE      TBL-F09  =     KTS-F09 / 100.
*最終更新担当者
      MOVE   "41"                  TO   TBL-F14.
*更新日
      MOVE    WK-DATE8             TO   TBL-F99.
*書き込みのときは、登録担当者と登録日もセットし書き込む。
      IF      HSHOTBL-INV-FLG   =  "INV"
              MOVE      "41"       TO    TBL-F13
              MOVE      WK-DATE8   TO    TBL-F98
*
              WRITE     TBL-REC
*同じ取引先コードが商品変換テーブルにあったときは更新処理
      ELSE
              REWRITE   TBL-REC

      END-IF.
*商品変換テーブルを更新した回数をカウント
     ADD      1         TO        OUT-CNT1.

*カタクラ商品データの読込処理
      PERFORM KATASYO-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*             カタクラ商品データの読込み処理
************************************************************
 KATASYO-READ-SEC                  SECTION.
*
      READ    KATASYO
              AT   END
              MOVE      "END"     TO   END-FLG
              GO        TO        KATASYO-READ-EXIT
      END-READ.
*
      ADD     1              TO        IN-CNT.
*
 KATASYO-READ-EXIT.
      EXIT.
***********************************************************
*      300    終了処理
***********************************************************
 300-END-SEC                     SECTION.
      CLOSE                      HSHOTBL   KATASYO.
*
      DISPLAY "* KATASYO  (INPUT) = " IN-CNT   " *"  UPON CONS.
      DISPLAY "* HSHOTBL (OUTPUT) = " OUT-CNT1 " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.

*****************<<  PROGRAM  END  >>***********************
