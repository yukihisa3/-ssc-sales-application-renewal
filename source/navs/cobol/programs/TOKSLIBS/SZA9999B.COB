****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　発注残再計算処理_　　　　　　　　*
*    モジュール名　　　　：　　　　　　　　　　　　　　　　　　*
*    作成日／更新日　　　：　2001/02/19                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　在庫マスタ内の未入庫数量を初期化　*
*                            する                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA9999B.
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
*----<< 商品名称マスタ >>--*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F0121
                                            MEI-F0122
                                            MEI-F0123
                        FILE      STATUS    MEI-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  商品在庫マスタ　　      >>---*
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*---<<  商品名称マスタ　　      >>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  ZAI-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
****  フラグ  ***
 01  FLG-AREA.
     02  END-FLG             PIC  9(01)     VALUE ZERO.
     02  WK-KEISU-FLG        PIC  9(01)     VALUE ZERO.
     02  HMEIMS-INV-FLG      PIC  X(03)     VALUE SPACE.
     02  TAISYO-FLG          PIC  9(01)     VALUE ZERO.
****  カウンタ ***
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)     VALUE ZERO.
     02  REWRITE-CNT         PIC  9(07)     VALUE ZERO.
     02  SKIP-CNT            PIC  9(07)     VALUE ZERO.
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
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HMEIMS.
     DISPLAY  "### HMEIMS  ST = " MEI-STATUS UPON CONS.
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
     OPEN    INPUT           HMEIMS.
**   DISPLAY "*** SZA9999B START ***" UPON CONS.
     READ    ZAMZAIF
        AT   END
             MOVE      1    TO   END-FLG
        NOT AT END
             ADD       1    TO   READ-CNT
     END-READ.
 INIT-END.
     EXIT.
*
****************************************************************
*              メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
*商品名称マスタチェック
     PERFORM  HMEIMS-READ-SEC
     IF  TAISYO-FLG  =  1
         ADD     1     TO        SKIP-CNT
         GO            TO        MAIN-010
     END-IF.
*末入庫数のクリア
     MOVE    ZERO      TO        ZAI-F26.
     REWRITE ZAI-REC.
     ADD     1         TO        REWRITE-CNT.
*
 MAIN-010.
     READ    ZAMZAIF
        AT   END
             MOVE      1    TO   END-FLG
        NOT AT END
             ADD       1    TO   READ-CNT
     END-READ.
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
     DISPLAY "* ZAMZAIF  (SKIP)= "  SKIP-CNT    " *" UPON CONS.
**   DISPLAY "*** SZA9999B END   ***" UPON CONS.

 END-END.
     EXIT.
****************************************************************
*              商品名称マスタ読込                              *
****************************************************************
 HMEIMS-READ-SEC        SECTION.
*
     MOVE    ZERO          TO    TAISYO-FLG.
     MOVE    ZAI-F021      TO    MEI-F011.
     MOVE    ZAI-F022      TO    MEI-F012.
     READ    HMEIMS
             INVALID       MOVE "INV"    TO   HMEIMS-INV-FLG
                           MOVE ZERO     TO   TAISYO-FLG
                           GO            TO   HMEIMS-READ-EXIT
             NOT  INVALID  MOVE SPACE    TO   HMEIMS-INV-FLG
     END-READ.
*ＡＣＯＳ振替対象商品の場合
*****IF      MEI-FIL1  =  "1"
*            MOVE          1             TO   TAISYO-FLG
*    ELSE
*            MOVE          ZERO          TO   TAISYO-FLG
*****END-IF.
*
 HMEIMS-READ-EXIT.
     EXIT.
******************<<  PROGRAM  END  >>**************************
