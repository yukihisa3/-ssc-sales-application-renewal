# SJZ0130B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJZ0130B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　在庫マスタ未出庫数／引当済数初期化*
*    作成日／更新日　　　：　2018/03/18                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　在庫マスタの未出庫数／引当済数を　*
*　　　　　　　　　　　　　　初期化する　　　　　　　　　　　　*
****************************************************************
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2018/03/13 高橋　　　新規作成（ＳＺＡ０１００Ｂ流用）
*　　　　　　　　（在庫マスタの未出庫数／引当済数を初期化する）
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJZ0130B.
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

 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
