****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　自動発注システム　　　　　　　　　*
*    モジュール名　　　　：　　　　　　　　　　　　　　　　　　*
*    作成日／更新日　　　：　03/06/09                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　自動発注数量スライド処理          *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            TKY0103B.
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
*---<<  商品在庫マスタ　　      >>---*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-S-ZAMZAIF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   ZAI-STATUS.
*
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  商品在庫マスタ　　      >>---*
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  ZAI-STATUS          PIC  X(02).
****  フラグ  ***
 01  FLG-AREA.
     02  END-FLG             PIC  9(01)     VALUE ZERO.
     02  WK-KEISU-FLG        PIC  9(01)     VALUE ZERO.
****  カウンタ ***
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)     VALUE ZERO.
     02  REWRITE-CNT         PIC  9(07)     VALUE ZERO.
 LINKAGE                     SECTION.
 01  PARA-KBN                PIC  9(01).
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION USING PARA-KBN.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    ZAMZAIF.
     DISPLAY  "### ZAMZAIF ST = " ZAI-STATUS UPON CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 TKY0103B-START              SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    1.
     PERFORM       END-SEC.
     STOP      RUN.
 TKY0103B-END.
     EXIT.
****************************************************************
*      1.0 　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    I-O             ZAMZAIF.
     READ    ZAMZAIF
        AT   END
             MOVE      1    TO   END-FLG
        NOT AT END
             ADD       1    TO   READ-CNT
     END-READ.
 INIT-END.
     EXIT.
****************************************************************
*     2.0      メイン処理
****************************************************************
 MAIN-SEC                    SECTION.
*
     IF  PARA-KBN = "1"
*        発注数量２⇒１へセット
         MOVE    ZAI-F24        TO  ZAI-F23
*        発注数量３⇒２へセット
         MOVE    ZAI-F25        TO  ZAI-F24
     END-IF.
*発注数量３⇒初期値セット
     MOVE    ZERO           TO  ZAI-F25.
     REWRITE ZAI-REC.
     ADD     1              TO   REWRITE-CNT.
*
     READ    ZAMZAIF
        AT   END
             MOVE      1    TO   END-FLG
        NOT AT END
             ADD       1    TO   READ-CNT
     END-READ.
 MAIN-END.
     EXIT.
****************************************************************
*     3.0      終了処理
****************************************************************
 END-SEC                SECTION.
     CLOSE              ZAMZAIF.
     DISPLAY "* ZAMZAIF    (IN)= "  READ-CNT    " *" UPON CONS.
     DISPLAY "* ZAMZAIF(UPDATE)= "  REWRITE-CNT " *" UPON CONS.

 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
