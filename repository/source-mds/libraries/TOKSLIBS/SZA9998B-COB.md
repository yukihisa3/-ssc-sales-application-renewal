# SZA9998B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SZA9998B.COB`

## ソースコード

```cobol
**********************************************************
*                                                        *
*    顧客名　　　　　　　：　（株）サカタのタネ殿        *
*    業務名　　　　　　　：　在庫マスタ未入庫数再計算    *
*    モジュール名　　　　：　未入庫数再計算              *
*    作成日／更新日　　　：　2001/02/19                  *
*    作成者／更新者　　　：　NAV                         *
*                                                        *
**********************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SZA9998B.
 AUTHOR.                   NAV.
 DATE-WRITTEN.             01/02/19.
**********************************************************
 ENVIRONMENT               DIVISION.
**********************************************************
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     STATION     IS        STA
     CONSOLE     IS        CONS.
***********************************************************
*             INPUT-OUTPUT                                *
***********************************************************
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*発注ファイル（ヘッダ）
     SELECT      HACHEDF   ASSIGN    TO        DA-01-VI-HACHEDL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       HED-F02
                           FILE      STATUS    HED-ST.
*発注ファイル（明細）
     SELECT      HACMEIF   ASSIGN    TO        DA-01-VI-HACMEIL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       BDY-F02
                                               BDY-F03
                           FILE      STATUS    BDY-ST.
*在庫マスタ
     SELECT      ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       ZAI-F01   ZAI-F021
                                               ZAI-F022  ZAI-F031
                                               ZAI-F032  ZAI-F033
                           FILE      STATUS    ZAI-ST.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
 FD  HACHEDF.
     COPY        HACHEDF   OF        XFDLIB
                 JOINING   HED   AS  PREFIX.
 FD  HACMEIF.
     COPY        HACMEIF   OF        XFDLIB
                 JOINING   BDY   AS  PREFIX.
 FD  ZAMZAIF.
     COPY        ZAMZAIF   OF        XFDLIB
                 JOINING   ZAI   AS  PREFIX.
******************************************************************
 WORKING-STORAGE       SECTION.
******************************************************************
*
 01  FILE-STATUS.
     03  HED-ST            PIC X(02).
     03  BDY-ST            PIC X(02).
     03  ZAI-ST            PIC X(02).
 01  IN-DATA               PIC X(01).
 01  WK-BDY-KEY.
     03  WK-BDY-F02        PIC 9(07)  VALUE  ZERO.
 01  WORK-AREA.
     03  WK-ZAN            PIC S9(08)V99  VALUE  ZERO.
     03  HED-CNT           PIC  9(06) VALUE  ZERO.
     03  ZAI-CNT           PIC  9(06) VALUE  ZERO.
*フラグ
 01  FLG-AREA.
     03  END-FLG           PIC X(03)  VALUE  SPACE.
     03  ZAI-INV-FLG       PIC X(03)  VALUE  SPACE.
 01  FILE-ERR.
     03  HED-ERR           PIC N(10) VALUE
                        NC"発注（ヘッダ）エラー".
     03  BDY-ERR           PIC N(10) VALUE
                        NC"発注（明細）エラー".
     03  ZAI-ERR           PIC N(10) VALUE
                        NC"在庫マスタエラー".
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                           DIVISION.
 DECLARATIVES.
 HED-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HACHEDF.
     DISPLAY     HED-ERR   UPON      STA.
     DISPLAY     HED-ST    UPON      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 BDY-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HACMEIF.
     DISPLAY     BDY-ERR   UPON      STA.
     DISPLAY     BDY-ST    UPON      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 ZAI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZAMZAIF.
     DISPLAY     ZAI-ERR   UPON      STA.
     DISPLAY     ZAI-ST    UPON      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC                  SECTION.
     DISPLAY  "**  SZA9998B   START  **"   UPON  CONS.
*
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-FLG  =  "END".
     PERFORM     END-SEC.
*
     DISPLAY  "**  SZA9998B    END   **"   UPON  CONS.
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
*各ファイルのＯＰＥＮ
     OPEN    INPUT     HACHEDF.
     OPEN    INPUT     HACMEIF.
     OPEN    I-O       ZAMZAIF.
*発注ヘッダＦスタート
     MOVE      ZERO               TO   HED-F02.
     PERFORM   HED-ST-SEC.
*発注ヘッダ初期読込み
     IF  END-FLG  NOT =  "END"
         PERFORM HED-RD-SEC
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    MAIN-SEC                                                  *
****************************************************************
 MAIN-SEC          SECTION.
*発注明細Ｆ読込み
     MOVE    ZERO           TO     WK-BDY-F02.
*
     PERFORM BDY-ST-SEC.
*
     IF      WK-BDY-KEY  NOT =  HIGH-VALUE
             PERFORM  BDY-RD-SEC
     END-IF.
*
     PERFORM UNTIL HED-F02  NOT =  WK-BDY-F02
                OR WK-BDY-KEY   =  HIGH-VALUE
                PERFORM HATYU-ZAN-SEC
                PERFORM BDY-RD-SEC
     END-PERFORM.
*
     PERFORM  HED-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
     CLOSE       HACHEDF.
     CLOSE       HACMEIF.
     CLOSE       ZAMZAIF.
     DISPLAY "HED-CNT = " HED-CNT UPON CONS.
     DISPLAY "ZAI-CNT = " ZAI-CNT UPON CONS.
 END-EXIT.
     EXIT.
******************************************************************
*               発注データ　ＳＴＡＲＴ
******************************************************************
 HED-ST-SEC                SECTION.
     START   HACHEDF     KEY >=  HED-F02
       INVALID KEY
          MOVE   "END"           TO        END-FLG
     END-START.
 HED-ST-EXIT.
     EXIT.
******************************************************************
*               発注データ　順読み
******************************************************************
 HED-RD-SEC                SECTION.
     READ    HACHEDF   NEXT
       AT  END
             MOVE      "END"     TO        END-FLG
             GO                  TO        HED-RD-EXIT
     END-READ.
*伝票区分＝５０以外は，読み飛ばし
     IF  HED-F01   NOT =  50
         GO            TO        HED-RD-SEC
     END-IF.
*２００１年以下は，読み飛ばし
     IF  HED-F11   <   20130600
         GO            TO        HED-RD-SEC
     END-IF.
*発注ヘッダの取消ＦＬＧが”１”取消の場合は読み飛ばしを行なう。
     IF  HED-F24   =   "1"
         GO            TO        HED-RD-SEC
     END-IF.
*
*****DISPLAY "HED-F02 = " HED-F02.
     ADD     1           TO    HED-CNT.
*
 HED-RD-EXIT.
     EXIT.
******************************************************************
*               発注データ（明細）ＳＴＡＲＴ
******************************************************************
 BDY-ST-SEC                SECTION.
     MOVE    HED-F02             TO        BDY-F02.
     MOVE    ZERO                TO        BDY-F03.
*
     START   HACMEIF     KEY >=  BDY-F02   BDY-F03
       INVALID KEY
           MOVE    HIGH-VALUE     TO       WK-BDY-KEY
     END-START.
 BDY-ST-EXIT.
     EXIT.
******************************************************************
*               発注データ（明細）順読み
******************************************************************
 BDY-RD-SEC                SECTION.
     READ    HACMEIF   NEXT
       AT  END
             MOVE    HIGH-VALUE  TO        WK-BDY-KEY
             GO                  TO        BDY-RD-EXIT
     END-READ.
*
**** DISPLAY "BDY-F02 = " BDY-F02.
     MOVE    BDY-F02             TO        WK-BDY-F02.
 BDY-RD-EXIT.
     EXIT.
******************************************************************
*               在庫マスタ読込み
******************************************************************
 ZAI-RD-SEC                SECTION.
     READ    ZAMZAIF
             INVALID
             MOVE    "INV"       TO        ZAI-INV-FLG
             NOT  INVALID
             MOVE    SPACE       TO        ZAI-INV-FLG
     END-READ.
*
 ZAI-RD-EXIT.
     EXIT.
******************************************************************
*               発注残計算
******************************************************************
 HATYU-ZAN-SEC             SECTION.
*明細完了区分
     IF      BDY-F05  =  1
             GO         TO   HATYU-ZAN-EXIT
     END-IF.
*発注残計算
     COMPUTE  WK-ZAN  =  BDY-F09  -  BDY-F10.
     IF       WK-ZAN  <=  ZERO
              GO        TO   HATYU-ZAN-EXIT
     END-IF.
*在庫マスタ読込み
     MOVE    HED-F17    TO   ZAI-F01.
     MOVE    BDY-F06    TO   ZAI-F021.
     MOVE    BDY-F07    TO   ZAI-F022.
     MOVE    BDY-F08    TO   ZAI-F03.
     PERFORM ZAI-RD-SEC.
     IF      ZAI-INV-FLG  =  "INV"
             GO         TO   HATYU-ZAN-EXIT
     END-IF.
*発注残セット
*****MOVE    WK-ZAN     TO   ZAI-F26.
     ADD     WK-ZAN     TO   ZAI-F26.
*在庫マスタ更新
     REWRITE ZAI-REC.
*
     ADD     1          TO   ZAI-CNT.
*
 HATYU-ZAN-EXIT.
     EXIT.

```
