****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　発注ファイル消込み　　　　　　　　*
*    作成日／更新日　　　：　00/05/15                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　入庫ファイル及び発注ファイルを読み*
*                            対象データを入庫ファイル，発注ファ*
*                            イルから削除する                  *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0174B.
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
*
*---<<  発注ファイル（明細）>>---*
     SELECT   NYKFILF   ASSIGN    TO        DA-01-VI-NYKFILL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   BDY-F02
                                                 BDY-F03
                                                 BDY-F04
                                                 BDY-F05
                        FILE      STATUS    IS   BDY-STATUS.
*
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  発注ファイル（明細）>>---*
 FD  NYKFILF.
     COPY     NYKFILF   OF        XFDLIB
              JOINING   BDY       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  NYU-STATUS          PIC  X(02).
     02  HED-STATUS          PIC  X(02).
     02  BDY-STATUS          PIC  X(02).
     02  JYO-STATUS          PIC  X(02).
****  フラグ  ***
 01  FLG-AREA.
     02  END-FLG             PIC  X(03)  VALUE  SPACE.
     02  INV-FLG             PIC  9(01)  VALUE  ZERO.
     02  NOTDEL-FLG          PIC  9(01)  VALUE  ZERO.
****  カウンタ ***
 01  CNT-AREA.
     02  READ-NYU-CNT        PIC  9(07)  VALUE  ZERO.
     02  DEL-NYU-CNT         PIC  9(07)  VALUE  ZERO.
     02  DEL-HED-CNT         PIC  9(07)  VALUE  ZERO.
     02  DEL-BDY-CNT         PIC  9(07)  VALUE  ZERO.
****  条件日付け　****
 01  JYOUKEN-AREA.
     02  JYOUKEN-DATE        PIC  9(06)  VALUE  ZERO.
     02  WK-NOUNYUBI         PIC  9(08)  VALUE  ZERO.
     02  WK-NOUNYUBI-R       REDEFINES   WK-NOUNYUBI.
       03  WK-YYMM           PIC  9(06).
       03  WK-DD             PIC  9(02).
****  ＫＥＹ  ***
 01  WK-KEY-AREA.
     02  WK-N-KEY.
       03  WK-N-DENNO        PIC  9(07).
       03  WK-N-EDA          PIC  9(02).
       03  WK-N-SOSAI        PIC  9(01).
     02  BRK-KEY.
       03  BRK-DENNO         PIC  9(07).
       03  BRK-EDA           PIC  9(02).
       03  BRK-SOSAI         PIC  9(01).
     02  WK-H-KEY.
       03  WK-H-DENNO        PIC  9(07).
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SZA0174B".
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
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    NYKFILF.
     MOVE     "NYKFILF"           TO   ERR-FL-ID.
     MOVE     BDY-STATUS          TO   ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON   CONS.
     DISPLAY  MSG-ABEND2          UPON   CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 KEI0100-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC       UNTIL   END-FLG  =  "END".
     PERFORM       END-SEC.
     STOP      RUN.
 KEI0100-END.
     EXIT.
****************************************************************
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    I-O             NYKFILF.
*****　入庫ファイル　ＲＥＡＤ　****
     PERFORM    READ-NYKFILF-SEC.
 INIT-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
*
     DELETE  NYKFILF.
*
     ADD     1             TO               DEL-BDY-CNT.
*
     PERFORM    READ-NYKFILF-SEC.
*
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE              NYKFILF.
     DISPLAY "* NYKFILF (INPUT) = " READ-NYU-CNT " *" UPON CONS.
     DISPLAY "* NYKFILF (DELETE)= " DEL-NYU-CNT  " *" UPON CONS.
     DISPLAY "* NYKFILF (DELETE)= " DEL-HED-CNT  " *" UPON CONS.
     DISPLAY "* NYKFILF (DELETE)= " DEL-BDY-CNT  " *" UPON CONS.
 END-END.
     EXIT.
****************************************************************
*              発注ファイル（明細）ＲＥＡＤ処理
****************************************************************
 READ-NYKFILF-SEC              SECTION.
     READ    NYKFILF
         AT   END
           MOVE    "END"          TO   END-FLG
           GO                     TO   READ-NYKFILF-END
     END-READ.
*
     IF    BDY-F02  >   8000
           MOVE    "END"          TO   END-FLG
           GO                     TO   READ-NYKFILF-END
     END-IF.
*
 READ-NYKFILF-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
