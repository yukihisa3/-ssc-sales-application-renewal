****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受配信管理システム　　　　　　　　*
*    モジュール名　　　　：　倉庫別_番マスタ作成              *
*    作成日／更新日　　　：　1999/10/18                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　倉庫別_番マスタを作成する。      *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKY1400B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          99/09/29.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*****<<    商品変換テーブル     >>****************************
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   TBL-F01
                                                 TBL-F02
                        FILE      STATUS    IS   TBL-ST.
*****<<    倉庫別_番マスタ     >>****************************
     SELECT   JHMTANF   ASSIGN    TO        DA-01-VI-JHMTANL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TAN-F01
                                                 TAN-F02
                        FILE      STATUS    IS   TAN-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
*
******************************************************************
*    倉庫別_番マスタ
******************************************************************
 FD  JHMTANF            LABEL RECORD   IS   STANDARD.
     COPY     JHMTANF   OF        XFDLIB
              JOINING   TAN       PREFIX.
*
****************************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
*フラグ
 01  END-FLG                      PIC       X(03)  VALUE SPACE.
 01  JHMTANF-INV-FLG              PIC       X(03)  VALUE SPACE.
 01  READ-CNT                     PIC       9(06)  VALUE ZERO.
 01  WRIT-CNT                     PIC       9(06)  VALUE ZERO.
 01  SKIP-CNT                     PIC       9(06)  VALUE ZERO.
*
 01  WK-DATE.
     03  SYS-DATE1                PIC       9(02)  VALUE ZERO.
     03  SYS-DATE2                PIC       9(06)  VALUE ZERO.
*ステイタス　エリア
 01  STATUS-AREA.
     03  TBL-ST                   PIC       X(02).
     03  TAN-ST                   PIC       X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  TBL-ERR           PIC N(15) VALUE
         NC"商品変換テーブルエラー".
     03  TAN-ERR           PIC N(15) VALUE
         NC"倉庫別_番マスタエラー".
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
**************************************************************
 PROCEDURE              DIVISION.
**************************************************************
*
 DECLARATIVES.
*
 TBL-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HSHOTBL.
     MOVE        TBL-ST    TO        E-ST.
     MOVE        "HSHOTBL " TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TBL-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMTANF.
     MOVE        TAN-ST    TO        E-ST.
     MOVE        "JHMTANF"  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TAN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*
 END          DECLARATIVES.
*
****************************************************************
*           プロセス                                0.0        *
****************************************************************
 PROCESS-START      SECTION.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL     END-FLG = "END".
     PERFORM  END-SEC.
*
     STOP     RUN.
*
 PROCESS-END.
     EXIT.
****************************************************************
*           初期処理                                1.0        *
****************************************************************
 INIT-SEC     SECTION.
*システム日付取得
     ACCEPT   SYS-DATE2   FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE2           TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WK-DATE.
*ファイルのＯＰＥＮ
     OPEN     INPUT     HSHOTBL.
     OPEN     I-O       JHMTANF.
*商品変換テーブル初期読込み
     PERFORM HSHOTBL-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*           メイン処理                              2.0        *
****************************************************************
 MAIN-SEC     SECTION.
*倉庫別_番マスタ読込み（存在チェック）
     MOVE     TBL-F04        TO      TAN-F01.
     MOVE     TBL-F03        TO      TAN-F02.
     PERFORM JHMTANF-READ-SEC.
*倉庫別_番マスタへ未登録の場合、レコード作成へ
     IF      JHMTANF-INV-FLG  =  "INV"
             MOVE  SPACE    TO      TAN-REC
             INITIALIZE           TAN-REC
             MOVE  TBL-F04  TO      TAN-F01
             MOVE  TBL-F03  TO      TAN-F02
             MOVE  TBL-F08  TO      TAN-F03
             MOVE  WK-DATE  TO      TAN-F98 TAN-F99
             WRITE TAN-REC
             ADD   1        TO      WRIT-CNT
     ELSE
             ADD   1        TO      SKIP-CNT
     END-IF.
*商品変換テーブル読込み
     PERFORM HSHOTBL-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*           終了処理                                3.0        *
****************************************************************
 END-SEC      SECTION.
*ファイルのＣＬＯＳＥ
     CLOSE    HSHOTBL  JHMTANF.
*件数メッセージ表示
     DISPLAY "## READ-CNT = " READ-CNT " ##" UPON CONS.
     DISPLAY "## WRIT-CNT = " WRIT-CNT " ##" UPON CONS.
     DISPLAY "## SKIP-CNT = " SKIP-CNT " ##" UPON CONS.
*
 END-EXIT.
     EXIT.
****************************************************************
*           商品変換テーブル読込み                       *
****************************************************************
 HSHOTBL-READ-SEC           SECTION.
*
     READ     HSHOTBL   AT        END
              MOVE     "END"      TO        END-FLG
              NOT  AT  END
              ADD       1         TO        READ-CNT
     END-READ.
*読込み件数途中表示
     IF       READ-CNT(4:3)  =  "000"
              DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*           倉庫別_番マスタ読込み                       *
****************************************************************
 JHMTANF-READ-SEC           SECTION.
*
     READ     JHMTANF   INVALID
              MOVE     "INV"      TO        JHMTANF-INV-FLG
              NOT  INVALID
              MOVE      SPACE     TO        JHMTANF-INV-FLG
     END-READ.
*
 JHMTANF-READ-EXIT.
     EXIT.
********************<<  PROGRAM  END  >>**************************
