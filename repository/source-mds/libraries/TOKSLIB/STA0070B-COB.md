# STA0070B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/STA0070B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　　　　　　　　　　　　　　　　　　*
*    作成日／更新日　　　：　00/05/29                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　商品在庫マスタの現在数を　　　　　*
*                            ゼロにする　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STA0070B.
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
 ZTA0065-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    1.
     PERFORM       END-SEC.
     STOP      RUN.
 ZTA0065-END.
     EXIT.
****************************************************************
*              初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    I-O             ZAMZAIF.
**   DISPLAY "*** STA0070B START ***" UPON CONS.
 INIT010.
     READ    ZAMZAIF
        AT   END
             MOVE      1    TO   END-FLG
             GO             TO   INIT-END
        NOT AT END
             ADD       1    TO   READ-CNT
     END-READ.
*
*    IF      ZAI-F01  =  "86"  OR  "90"
*            CONTINUE
*    ELSE
*            GO             TO   INIT010
*    END-IF.
*
 INIT-END.
     EXIT.
*
****************************************************************
*              メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
*現在庫数・前月末在庫数のクリア
     MOVE    ZERO      TO        ZAI-F04.
     MOVE    ZERO      TO        ZAI-F05.
     REWRITE ZAI-REC.
     ADD     1         TO        REWRITE-CNT.
*
 MAIN010.
     READ    ZAMZAIF
        AT   END
             MOVE      1    TO   END-FLG
             GO             TO   MAIN-END
        NOT AT END
             ADD       1    TO   READ-CNT
     END-READ.
*
*    IF      ZAI-F01  =  "86"  OR  "90"
*            CONTINUE
*    ELSE
*            GO             TO   MAIN010
*    END-IF.
*
 MAIN-END.
     EXIT.
*
****************************************************************
*              終了処理　　                                    *
****************************************************************
 END-SEC                SECTION.
     CLOSE              ZAMZAIF.
     DISPLAY "* ZAMZAIF    (IN)= "  READ-CNT    " *" UPON CONS.
     DISPLAY "* ZAMZAIF(UPDATE)= "  REWRITE-CNT " *" UPON CONS.
**   DISPLAY "*** STA0070B END   ***" UPON CONS.

 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
