# SZA0540B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SZA0540B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　　　　　　　　　　　　　　　　　　*
*    作成日／更新日　　　：　05/12/22                          *
*    作成者／更新者　　　：　ＮＡＶ松野　　　　　　　　　　　　*
*    処理概要　　　　　　：　未出庫数　引き当て済数　　　　　　*
*　　　　　　　　　　　　　　ゼロクリア　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0540B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
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
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
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
 KEI0100-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    1.
     PERFORM       END-SEC.
     STOP      RUN.
 KEI0100-END.
     EXIT.
****************************************************************
*      1.0 　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    I-O             ZAMZAIF.
***  DISPLAY "*** STA0110B  START ***" UPON CONS.
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
*未出庫数・引当済数のクリア
     MOVE             ZERO    TO  ZAI-F27.
     MOVE             ZERO    TO  ZAI-F28.
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
***  DISPLAY "*** STA0110B  END   ***" UPON CONS.

 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
