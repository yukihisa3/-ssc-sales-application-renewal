****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　マスタデータコンバート　　　　　　*
*    モジュール名　　　　：　顧客需要家Ｍ⇒ＷＫへの抽出　　　　*
*    作成日／更新日　　　：　03/07/03                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメータより受け取った取引先ＣＤ*
*                          の顧客需要家ＤＴをＷＫに抽出する　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CNV0530B.
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
****<< 顧客需要家Ｍ　　 >>************************************
     SELECT   KYKJYKF   ASSIGN    TO        DA-01-VI-KYKJYKL1
                        ORGANIZATION        IS   INDEXED
                        RECORD    KEY       IS   KYK-F01
                                                 KYK-F02
                        FILE      STATUS    IS   KYK-STATUS.
****<< 顧客需要家ＷＫ　　　　 >>******************************
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "CNV0530B".
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
 01  KYKJYKF-END             PIC  X(03)  VALUE  SPACE.
 01  WK-TENCD                PIC  9(05)  VALUE  ZERO.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  OUT-CNT1            PIC  9(07)   VALUE  0.
     03  OUT-CNT2            PIC  9(07)   VALUE  0.
*
************************************************************
 LINKAGE            SECTION.
 01  LINK-TOKCD1        PIC  9(08).
 01  LINK-TOKCD2        PIC  9(08).
************************************************************
 PROCEDURE              DIVISION       USING  LINK-TOKCD1
                                              LINK-TOKCD2.
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
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  KYKWK.
     MOVE   "KYKWK"        TO    ERR-FL-ID.
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
     OPEN         INPUT     KYKJYKF.
     OPEN         OUTPUT    KYKWK.
*
     MOVE         SPACE         TO   KYK-REC.
     INITIALIZE                      KYK-REC.
     MOVE         LINK-TOKCD1   TO   KYK-F01.
     START  KYKJYKF  KEY  IS  >=  KYK-F01 KYK-F02
            INVALID
            DISPLAY  NC"対象データ無し" UPON CONS
            MOVE     "END"      TO   END-FLG
            GO                  TO   100-INIT-END
     END-START.
*
     PERFORM  KYKJYKF-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*
     MOVE  SPACE          TO   KWK-REC.
     INITIALIZE                KWK-REC.
     MOVE  KYK-REC        TO   KWK-REC.
     MOVE  LINK-TOKCD2    TO   KWK-F01.
*店舗ＣＤ変更
*　ダイユーエイト
     IF    LINK-TOKCD2  =  751225
           MOVE  51       TO   KWK-F02(1:2)
     END-IF.
*　日敷
     IF    LINK-TOKCD2  =  781225
           MOVE  81       TO   KWK-F02(1:2)
     END-IF.
*　タイム
     IF    LINK-TOKCD2  =  761995
           MOVE  KWK-F02       TO   WK-TENCD
           MOVE  61            TO   KWK-F02(1:2)
           MOVE  WK-TENCD(2:1) TO   KWK-F02(3:1)
           MOVE  WK-TENCD(4:2) TO   KWK-F02(4:2)
     END-IF.
     WRITE KWK-REC.
     ADD   1              TO   OUT-CNT1.
*
     PERFORM KYKJYKF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            商品変換テーブルの読込処理
************************************************************
 KYKJYKF-READ-SEC                   SECTION.
     READ    KYKJYKF
        NEXT   AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        KYKJYKF-READ-EXT
     END-READ.
     ADD     1         TO        IN-CNT.
*
     IF      LINK-TOKCD1  <  KYK-F01
             MOVE      "END"     TO   END-FLG
     END-IF.
*
 KYKJYKF-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             KYKJYKF   KYKWK.
*
     DISPLAY "* KYKJYKF (INPUT) = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* KYKJYKF  (OUTPUT)= " OUT-CNT1 " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
