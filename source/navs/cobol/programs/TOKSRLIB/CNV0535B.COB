****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　マスタデータコンバート　　　　　　*
*    モジュール名　　　　：　顧客需要家ＷＫ⇒マスタ更新　　　　*
*    作成日／更新日　　　：　23/01/24                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＷＫ⇒顧客需要家マスタコピー　　　*
*                          　　　                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CNV0535B.
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
****<< 顧客需要家マスタ >>************************************
     SELECT   KYKJYKF   ASSIGN    TO        DA-01-VI-KYKJYKL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   KYK-F01
                                                 KYK-F02
                        FILE      STATUS    IS   KYK-STATUS.
****<< 顧客需要家ワーク >>************************************
     SELECT   KYKWK     ASSIGN    TO        DA-01-S-KYKWK
                        FILE      STATUS    IS   KWK-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 商品変換テーブル >>*************************************
 FD  KYKJYKF.
     COPY     KYKJYKF   OF        XFDLIB
              JOINING   KYK       PREFIX.
****<< 変換後商品変換TBL>>*********************************
 FD  KYKWK
              BLOCK CONTAINS  27  RECORDS.
     COPY     KYKJYKF   OF        XFDLIB
              JOINING   KWK       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 KYK-STATUS           PIC  X(02).
     02 KWK-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "CNV0535B".
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
 01  KYKJYKF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  KYKJYKF-END             PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  OUT-CNT1            PIC  9(07)   VALUE  0.
     03  OUT-CNT2            PIC  9(07)   VALUE  0.
     03  OUT-CNT3            PIC  9(07)   VALUE  0.
*
************************************************************
 PROCEDURE              DIVISION.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  KYKJYKF.
     MOVE   "KYKJYKF "        TO    ERR-FL-ID.
     MOVE    KYK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  KYKWK.
     MOVE   "KYKWK   "        TO    ERR-FL-ID.
     MOVE    KWK-STATUS       TO    ERR-STCD.
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
*      １００   初期処理                                   *
************************************************************
 100-INIT-SEC           SECTION.
*
     OPEN         INPUT     KYKWK.
     OPEN         I-O       KYKJYKF.
*
     PERFORM  KYKWK-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*
     MOVE  KWK-F01        TO   KYK-F01.
     MOVE  KWK-F02        TO   KYK-F02.
     READ  KYKJYKF
           INVALID      MOVE  "INV"   TO  KYKJYKF-INV-FLG
           NOT INVALID  MOVE  SPACE   TO  KYKJYKF-INV-FLG
     END-READ.
*
     IF    KYKJYKF-INV-FLG = "INV"
           MOVE   SPACE   TO   KYK-REC
           INITIALIZE          KYK-REC
           MOVE   KWK-REC TO   KYK-REC
           WRITE  KYK-REC
           ADD    1       TO   OUT-CNT1
     ELSE
           ADD    1       TO   OUT-CNT3
     END-IF.
*
     PERFORM KYKWK-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            商品変換テーブルの読込処理
************************************************************
 KYKWK-READ-SEC                   SECTION.
     READ   KYKWK
       AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        KYKWK-READ-EXIT
     END-READ.
     ADD     1         TO        IN-CNT.
*
 KYKWK-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             KYKJYKF  KYKWK.
*
     DISPLAY "* KYKWK   (INPUT)  = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* KYKJYKF (OUTPUT) = " OUT-CNT1 " *"  UPON CONS.
     DISPLAY "* KYKJYKF (REWRITE)= " OUT-CNT3 " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
