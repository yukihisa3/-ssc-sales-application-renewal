****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　発注完了チェック                  *
*    作成日／更新日　　　：　02/01/07                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注明細ファイルを読み、完了ＦＬＧ*
*                            が”１”、発注ヘッダファイルの完了*
*                            区分に”１”をセットする。        *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0171B.
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
*---<<  発注ファイル（ヘッダ）>>---*
     SELECT   HACHEDF   ASSIGN    TO        DA-01-VI-HACHEDL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   HED-F02
                        FILE      STATUS    IS   HED-STATUS.
*
*---<<  発注ファイル（明細）>>---*
     SELECT   HACMEIF   ASSIGN    TO        DA-01-VI-HACMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   BDY-F02
                                                 BDY-F03
                        FILE      STATUS    IS   BDY-STATUS.
*
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  発注ファイル（ヘッダ）>>---*
 FD  HACHEDF.
     COPY     HACHEDF   OF        XFDLIB
              JOINING   HED       PREFIX.
*---<<  発注ファイル（明細）>>---*
 FD  HACMEIF.
     COPY     HACMEIF   OF        XFDLIB
              JOINING   BDY       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  HED-STATUS          PIC  X(02).
     02  BDY-STATUS          PIC  X(02).
****  フラグ  ***
 01  FLG-AREA.
     02  END-FLG             PIC  X(03)  VALUE  SPACE.
     02  INV-FLG             PIC  9(01)  VALUE  ZERO.
     02  NOTDEL-FLG          PIC  9(01)  VALUE  ZERO.
     02  WK-FLG-OK           PIC  9(01)  VALUE  ZERO.
     02  WK-FLG-NG           PIC  9(01)  VALUE  ZERO.
****  カウンタ ***
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)  VALUE  ZERO.
     02  SET-CNT             PIC  9(07)  VALUE  ZERO.
****  ＫＥＹ  ***
 01  WK-KEY-AREA.
     02  OLD-KEY.
       03  OLD-DENNO         PIC  9(07).
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SZA0171B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
****************************************************************
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HACHEDF.
     MOVE     "HACHEDF"           TO   ERR-FL-ID.
     MOVE     HED-STATUS          TO   ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON   CONS.
     DISPLAY  MSG-ABEND2          UPON   CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HACMEIF.
     MOVE     "HACMEIF"           TO   ERR-FL-ID.
     MOVE     BDY-STATUS          TO   ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON   CONS.
     DISPLAY  MSG-ABEND2          UPON   CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 SZA0171B-START              SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC       UNTIL   END-FLG  =  "END".
     PERFORM       END-SEC.
     STOP      RUN.
 SZA0171B-END.
     EXIT.
****************************************************************
*      ■０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    I-O             HACHEDF
             INPUT           HACMEIF.
*伝票番号初期値セット
     MOVE    ZERO            TO        OLD-DENNO.
*****　発注明細Ｆ　ＲＥＡＤ　****
     PERFORM    READ-HACMEIF-SEC.
 INIT-END.
     EXIT.
****************************************************************
*      ■０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
*発注ヘッダＦ　ＲＥＡＤ
     MOVE    OLD-DENNO            TO   HED-F02.
     PERFORM READ-HACHEDF-SEC.
     IF      INV-FLG  =  ZERO
             IF   WK-FLG-OK  =  1
             AND  WK-FLG-NG  =  1
                  MOVE       ZERO      TO   HED-F04
             END-IF
             IF   WK-FLG-OK  =  1
             AND  WK-FLG-NG  =  ZERO
                  MOVE       1         TO   HED-F04
******************DISPLAY "## OK ## HED-F02 = " HED-F02  UPON CONS
             END-IF
             IF   WK-FLG-OK  =  ZERO
             AND  WK-FLG-NG  =  1
                  MOVE       ZERO      TO   HED-F04
             END-IF
             IF   WK-FLG-OK  =  ZERO
             AND  WK-FLG-OK  =  ZERO
                  MOVE       ZERO      TO   HED-F04
             END-IF
             REWRITE HED-REC
     END-IF.
     MOVE    ZERO                      TO   WK-FLG-OK.
     MOVE    ZERO                      TO   WK-FLG-NG.
     MOVE    BDY-F02                   TO   OLD-DENNO.
     IF      BDY-F05  =  1
             MOVE       1              TO   WK-FLG-OK
     ELSE
             MOVE       1              TO   WK-FLG-NG
     END-IF.
*****　発注明細Ｆ　ＲＥＡＤ　****
     PERFORM    READ-HACMEIF-SEC.
*
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE              HACHEDF  HACMEIF.
*
     DISPLAY "* HACMEIF (READ   )= " READ-CNT " *" UPON CONS.
     DISPLAY "* HACHEDF (REWRITE)= " SET-CNT  " *" UPON CONS.
 END-END.
     EXIT.
****************************************************************
*              発注ファイル（ヘッダ）ＲＥＡＤ処理
****************************************************************
 READ-HACHEDF-SEC              SECTION.
     READ    HACHEDF
         INVALID
           MOVE    1              TO   INV-FLG
         NOT INVALID
           MOVE    0              TO   INV-FLG
     END-READ.
 READ-HACHEDF-END.
     EXIT.
****************************************************************
*              発注ファイル（明細）ＲＥＡＤ処理
****************************************************************
 READ-HACMEIF-SEC              SECTION.
     READ    HACMEIF
             AT  END
             MOVE   "END"         TO   END-FLG
             GO                   TO   READ-HACMEIF-END
             NOT  AT  END
             ADD     1            TO   READ-CNT
     END-READ.
*
     IF      READ-CNT(5:3)  =  "000"  OR  "500"
             DISPLAY "READ-CNT = " READ-CNT  UPON CONS
     END-IF.
*
     IF      READ-CNT  =  1
             MOVE     BDY-F02     TO   OLD-DENNO
             MOVE     ZERO        TO   WK-FLG-OK
             MOVE     ZERO        TO   WK-FLG-NG
     END-IF.
*
*    IF      BDY-F05  NOT =  1
*            GO                   TO   READ-HACMEIF-SEC
*    END-IF.
*
     IF      OLD-KEY  =  BDY-F02
*            伝票内完了チェック
             IF   BDY-F05  =  1
                  MOVE       1         TO   WK-FLG-OK
             ELSE
                  MOVE       1         TO   WK-FLG-NG
             END-IF
             GO                   TO   READ-HACMEIF-SEC
     ELSE
*************MOVE     BDY-F02     TO   OLD-DENNO
             ADD      1           TO   SET-CNT
     END-IF.
*
 READ-HACMEIF-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
