# SZA0170B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SZA0170B.COB`

## ソースコード

```cobol
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
 PROGRAM-ID.            SZA0170B.
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
*---<<  入庫ファイル　>>---*
     SELECT  NYKFILF    ASSIGN    TO        DA-01-VI-NYKFILL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   NYU-F02
                                                 NYU-F03
                                                 NYU-F04
                                                 NYU-F05
                        FILE      STATUS    IS   NYU-STATUS.
*
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
*---<<  条件ファイル　>>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  入庫ファイル　>>---*
 FD  NYKFILF.
     COPY     NYKFILF   OF        XFDLIB
              JOINING   NYU       PREFIX.
*---<<  発注ファイル（ヘッダ）>>---*
 FD  HACHEDF.
     COPY     HACHEDF   OF        XFDLIB
              JOINING   HED       PREFIX.
*---<<  発注ファイル（明細）>>---*
 FD  HACMEIF.
     COPY     HACMEIF   OF        XFDLIB
              JOINING   BDY       PREFIX.
*---<<  条件ファイル　>>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SZA0170B".
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
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    NYKFILF.
     MOVE     "NYKFILF"           TO   ERR-FL-ID.
     MOVE     NYU-STATUS          TO   ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON   CONS.
     DISPLAY  MSG-ABEND2          UPON   CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HACHEDF.
     MOVE     "HACHEDF"           TO   ERR-FL-ID.
     MOVE     HED-STATUS          TO   ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON   CONS.
     DISPLAY  MSG-ABEND2          UPON   CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HACMEIF.
     MOVE     "HACMEIF"           TO   ERR-FL-ID.
     MOVE     BDY-STATUS          TO   ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON   CONS.
     DISPLAY  MSG-ABEND2          UPON   CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "HJYOKEN"           TO   ERR-FL-ID.
     MOVE     JYO-STATUS          TO   ERR-STCD.
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
     OPEN    I-O             NYKFILF
                             HACHEDF     HACMEIF
             INPUT           HJYOKEN.
*****　条件日付　加工　****
     MOVE    99                   TO   JYO-F01.
     MOVE    "ZAI"                TO   JYO-F02.
     READ    HJYOKEN.
     MOVE    JYO-F05              TO   JYOUKEN-DATE.
*****　入庫ファイル　ＲＥＡＤ　****
     PERFORM    READ-NYKFILF-SEC.
 INIT-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE    ZERO                 TO   NOTDEL-FLG.
     MOVE    ZERO                 TO   WK-NOUNYUBI.
     MOVE    WK-N-KEY             TO   BRK-KEY.
****　入庫ファイル調査　****
     PERFORM        UNTIL  WK-N-KEY       = HIGH-VALUE
                    OR     WK-N-DENNO NOT = BRK-DENNO
        IF   NYU-F26   >   WK-NOUNYUBI
             MOVE    NYU-F26      TO   WK-NOUNYUBI
        END-IF
*（計上済フラグ≠１なら削除対象外）
        IF   NOTDEL-FLG    =   ZERO   AND
             NYU-F31   NOT =   1
             MOVE    1            TO   NOTDEL-FLG
        END-IF
        PERFORM    READ-NYKFILF-SEC
        IF   WK-N-KEY   NOT =  HIGH-VALUE
             ADD   1              TO   READ-NYU-CNT
        END-IF
     END-PERFORM.
****　発注ファイル調査　****
     IF   NOTDEL-FLG   =   ZERO
          IF   WK-YYMM   <=  JYOUKEN-DATE
               PERFORM    CHECK-HACHEDF-SEC
          ELSE
*（納入日＞基準日なら削除対象外）
               MOVE    1          TO   NOTDEL-FLG
          END-IF
     END-IF.
****　入庫・発注ファイル削除　****
     IF   NOTDEL-FLG   =   ZERO
          PERFORM    DEL-NYKFILF-SEC
          PERFORM    DEL-HACHEDF-SEC
     END-IF.
 MAIN-END.
     EXIT.
****************************************************************
*      _１　　発注ファイル調査                                *
****************************************************************
 CHECK-HACHEDF-SEC           SECTION.
****　発注ファイル読み込み　****
     MOVE    BRK-DENNO            TO   HED-F02.
     PERFORM    READ-HACHEDF-SEC.
     IF      INV-FLG   NOT =  ZERO
             DISPLAY   "ﾊｯﾁｭｳﾌｧｲﾙ INVALID ﾃﾞﾝﾋﾟｮｳNO = "
                                  BRK-DENNO       UPON CONS
          MOVE    1               TO   NOTDEL-FLG
          GO                      TO   CHECK-HACHEDF-END
     END-IF.
*（完了区分＝０なら削除対象外）
     IF         HED-F04   =   ZERO
          MOVE    1               TO   NOTDEL-FLG
     END-IF.
 CHECK-HACHEDF-END.
     EXIT.
****************************************************************
*      _２　　入庫ファイル削除処理                            *
****************************************************************
 DEL-NYKFILF-SEC             SECTION.
****　入庫ファイル　検索　****
     MOVE    BRK-DENNO            TO   NYU-F02.
     MOVE    BRK-EDA              TO   NYU-F03.
     MOVE    BRK-SOSAI            TO   NYU-F04.
     MOVE    ZERO                 TO   NYU-F05.
     START    NYKFILF   KEY   >=  NYU-F02  NYU-F03
                                  NYU-F04  NYU-F05
          INVALID   KEY
             MOVE    "END"        TO   END-FLG
             GO                   TO   DEL-NYKFILF-END
          NOT   INVALID   KEY
             PERFORM    READ-NYKFILF-SEC
     END-START.
****　入庫ファイル　削除（実）　****
     PERFORM        UNTIL  WK-N-KEY       = HIGH-VALUE
                    OR     WK-N-DENNO NOT = BRK-DENNO
         DELETE     NYKFILF
         ADD   1                  TO   DEL-NYU-CNT
         PERFORM    READ-NYKFILF-SEC
     END-PERFORM.
 DEL-NYKFILF-END.
     EXIT.
****************************************************************
*      _３　　発注ファイル削除処理                            *
****************************************************************
 DEL-HACHEDF-SEC             SECTION.
****　発注ファイル（ヘッダ）検索・削除　***
     MOVE    BRK-DENNO            TO   HED-F02.
     PERFORM    READ-HACHEDF-SEC.
     IF      INV-FLG       =  ZERO
         DELETE     HACHEDF
         ADD   1                  TO   DEL-HED-CNT
     END-IF.
*
****　発注ファイル（明細）検索　****
     MOVE    BRK-DENNO            TO   BDY-F02.
     MOVE    ZERO                 TO   BDY-F03.
     START    HACMEIF   KEY   >=  BDY-F02  BDY-F03
          INVALID   KEY
             GO                   TO   DEL-HACHEDF-END
          NOT   INVALID   KEY
             PERFORM    READ-HACMEIF-SEC
     END-START.
****　発注ファイル（明細）削除（実）　****
     PERFORM        UNTIL  WK-H-KEY       = HIGH-VALUE
                    OR     WK-H-DENNO NOT = BRK-DENNO
         DELETE     HACMEIF
         ADD   1                  TO   DEL-BDY-CNT
         PERFORM    READ-HACMEIF-SEC
     END-PERFORM.
 DEL-HACHEDF-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE              NYKFILF
                        HACHEDF    HACMEIF
                        HJYOKEN.
     DISPLAY "* NYKFILF (INPUT) = " READ-NYU-CNT " *" UPON CONS.
     DISPLAY "* NYKFILF (DELETE)= " DEL-NYU-CNT  " *" UPON CONS.
     DISPLAY "* HACHEDF (DELETE)= " DEL-HED-CNT  " *" UPON CONS.
     DISPLAY "* HACMEIF (DELETE)= " DEL-BDY-CNT  " *" UPON CONS.
 END-END.
     EXIT.
****************************************************************
*              入庫ファイル　ＲＥＡＤ処理                      *
****************************************************************
 READ-NYKFILF-SEC              SECTION.
     READ    NYKFILF
         AT   END
           MOVE    HIGH-VALUE     TO   WK-N-KEY
           MOVE    "END"          TO   END-FLG
         NOT   AT   END
           MOVE    NYU-F02        TO   WK-N-DENNO
           MOVE    NYU-F03        TO   WK-N-EDA
           MOVE    NYU-F04        TO   WK-N-SOSAI
     END-READ.
 READ-NYKFILF-END.
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
         AT   END
           MOVE    HIGH-VALUE     TO   WK-H-KEY
         NOT   AT   END
           MOVE    BDY-F02        TO   WK-H-DENNO
     END-READ.
 READ-HACMEIF-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
