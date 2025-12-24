# ZMO0060B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/ZMO0060B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　発注ファイル消込み　　　　　　　　*
*    作成日／更新日　　　：　93/05/21                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　入庫ファイル及び発注ファイルを読み*
*                            対象データを入庫ファイル，発注ファ*
*                            イルから削除する                  *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZMO0060B.
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
     SELECT  ZNYUKDT    ASSIGN    TO        DA-01-VI-ZNYUKDT2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   NYU-F02
                                                 NYU-F03
                                                 NYU-F04
                                                 NYU-F05
                        FILE      STATUS    IS   NYU-STATUS.
*
*---<<  発注ファイル　>>---*
     SELECT   ZHACHDT   ASSIGN    TO        DA-01-VI-ZHACHDT2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   HAC-F02
                                                 HAC-F03
                                                 HAC-F04
                                                 HAC-F05
                        FILE      STATUS    IS   HAC-STATUS.
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
 FD  ZNYUKDT.
     COPY     ZNYUKDT   OF        XFDLIB
              JOINING   NYU       PREFIX.
*---<<  発注ファイル　>>---*
 FD  ZHACHDT.
     COPY     ZHACHDT.
*---<<  条件ファイル　>>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  NYU-STATUS          PIC  X(02).
     02  HAC-STATUS          PIC  X(02).
     02  JYO-STATUS          PIC  X(02).
****  フラグ  ***
 01  FLG-AREA.
     02  END-FLG1            PIC  9(01)  VALUE  ZERO.
     02  END-FLG2            PIC  9(01)  VALUE  ZERO.
     02  NOTDEL-FLG          PIC  9(01)  VALUE  ZERO.
****  カウンタ ***
 01  CNT-AREA.
     02  READ-NYU-CNT        PIC  9(07)  VALUE  ZERO.
*****02  READ-HAC-CNT        PIC  9(07)  VALUE  ZERO.
     02  DEL-NYU-CNT         PIC  9(07)  VALUE  ZERO.
     02  DEL-HAC-CNT         PIC  9(07)  VALUE  ZERO.
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
       03  WK-N-DENNO        PIC  9(07)  VALUE  ZERO.
       03  WK-N-EDANO        PIC  9(02)  VALUE  ZERO.
       03  WK-N-SOSAI        PIC  9(01)  VALUE  ZERO.
       03  WK-N-GYONO        PIC  9(02)  VALUE  ZERO.
     02  WK-H-KEY.
       03  WK-H-DENNO        PIC  9(07)  VALUE  ZERO.
       03  WK-H-EDANO        PIC  9(02)  VALUE  ZERO.
       03  WK-H-SOSAI        PIC  9(01)  VALUE  ZERO.
       03  WK-H-GYONO        PIC  9(02)  VALUE  ZERO.
     02  BRK-N-KEY.
       03  BRK-N-DENNO       PIC  9(07)  VALUE  ZERO.
       03  BRK-N-EDANO       PIC  9(02)  VALUE  ZERO.
       03  BRK-N-SOSAI       PIC  9(01)  VALUE  ZERO.
       03  BRK-N-GYONO       PIC  9(02)  VALUE  ZERO.
     02  BRK-H-KEY.
       03  BRK-H-DENNO       PIC  9(07)  VALUE  ZERO.
       03  BRK-H-EDANO       PIC  9(02)  VALUE  ZERO.
       03  BRK-H-SOSAI       PIC  9(01)  VALUE  ZERO.
       03  BRK-H-GYONO       PIC  9(02)  VALUE  ZERO.
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZMO0060B".
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
                     PROCEDURE    ZNYUKDT.
     MOVE     "ZNYUKDT"           TO   ERR-FL-ID.
     MOVE     NYU-STATUS          TO   ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON   CONS.
     DISPLAY  MSG-ABEND2          UPON   CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    ZHACHDT.
     MOVE     "ZHACHDT"           TO   ERR-FL-ID.
     MOVE     HAC-STATUS          TO   ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON   CONS.
     DISPLAY  MSG-ABEND2          UPON   CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "HJYOKEN"           TO   ERR-FL-ID.
     MOVE     HAC-STATUS          TO   ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON   CONS.
     DISPLAY  MSG-ABEND2          UPON   CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 KEI0100-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG1   >   1   OR
                             END-FLG2   >   1.
     PERFORM       END-SEC.
     STOP      RUN.
 KEI0100-END.
     EXIT.
****************************************************************
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    I-O             ZNYUKDT
                             ZHACHDT
             INPUT           HJYOKEN.
*****　条件日付　加工　****
     MOVE    99                   TO   JYO-F01.
     MOVE    "ZAI"                TO   JYO-F02.
     READ    HJYOKEN.
     MOVE    JYO-F05              TO   JYOUKEN-DATE.
*****IF   JYOUKEN-DATE(5:2)   =   12
*****     ADD   89                TO   JYOUKEN-DATE
*****ELSE
*****     ADD   1                 TO   JYOUKEN-DATE
*****END-IF.
*****　入庫ファイル　ＲＥＡＤ　****
     PERFORM    READ-ZNYUKDT-SEC.
     IF   END-FLG1   =   ZERO
          ADD   1                 TO   READ-NYU-CNT
*-------　発注ファイル　ＲＥＡＤ　-------*
          PERFORM    READ-ZHACHDT-SEC
          IF   END-FLG2   >   ZERO
               MOVE    9          TO   END-FLG1
               MOVE    9          TO   END-FLG2
          END-IF
     ELSE
          MOVE    9               TO   END-FLG1
          MOVE    9               TO   END-FLG2
     END-IF.
 INIT-END.
     EXIT.
****************************************************************
*      _１　　入庫ファイル　ＲＥＡＤ処理                      *
****************************************************************
 READ-ZNYUKDT-SEC              SECTION.
     READ    ZNYUKDT
         AT   END
           ADD   1                TO   END-FLG1
         NOT   AT   END
           MOVE    NYU-F02        TO   WK-N-DENNO
           MOVE    NYU-F03        TO   WK-N-EDANO
           MOVE    NYU-F04        TO   WK-N-SOSAI
           MOVE    NYU-F05        TO   WK-N-GYONO
     END-READ.
 READ-ZNYUKDT-END.
     EXIT.
****************************************************************
*      _２　　発注ファイル　ＲＥＡＤ処理                      *
****************************************************************
 READ-ZHACHDT-SEC              SECTION.
     READ    ZHACHDT
         AT   END
           ADD   1                TO   END-FLG2
         NOT   AT   END
           MOVE    HAC-F02        TO   WK-H-DENNO
           MOVE    HAC-F03        TO   WK-H-EDANO
           MOVE    HAC-F04        TO   WK-H-SOSAI
           MOVE    HAC-F05        TO   WK-H-GYONO
     END-READ.
 READ-ZHACHDT-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE    ZERO                 TO   NOTDEL-FLG.
     MOVE    ZERO                 TO   WK-NOUNYUBI.
****　入庫ファイル調査　****
     MOVE    WK-N-KEY             TO   BRK-N-KEY.
     PERFORM    UNTIL
                END-FLG1         >   ZERO          OR
                WK-N-DENNO   NOT =   BRK-N-DENNO
        IF   NYU-F23   >   WK-NOUNYUBI
             MOVE    NYU-F23      TO   WK-NOUNYUBI
        END-IF
        IF   NOTDEL-FLG   =   ZERO
             IF   NYU-F28   NOT =   1
                  MOVE    1       TO   NOTDEL-FLG
             END-IF
        END-IF
        PERFORM    READ-ZNYUKDT-SEC
        IF   END-FLG1   =   ZERO
             ADD   1              TO   READ-NYU-CNT
        END-IF
     END-PERFORM.
****　発注ファイル調査　****
     IF   NOTDEL-FLG   =   ZERO
          IF   WK-YYMM   <=  JYOUKEN-DATE
               PERFORM    CHECK-ZHACHDT-SEC
          ELSE
               MOVE    1          TO   NOTDEL-FLG
          END-IF
     END-IF.
****　入庫ファイル削除　****
     IF   NOTDEL-FLG   =   1
          IF   END-FLG1   >   ZERO
               MOVE    9          TO   END-FLG1
          END-IF
     ELSE
          PERFORM    DEL-ZNYUKDT-SEC
     END-IF.
****　発注ファイル削除　****
     IF   NOTDEL-FLG   =   1
          IF   END-FLG2   >   ZERO
               MOVE    9          TO   END-FLG2
          END-IF
     ELSE
          PERFORM    DEL-ZHACHDT-SEC
     END-IF.
 MAIN-END.
     EXIT.
****************************************************************
*      _１　　発注ファイル調査                                *
****************************************************************
 CHECK-ZHACHDT-SEC           SECTION.
****　発注ファイルＫＥＹセット　****
     MOVE    BRK-N-DENNO          TO   HAC-F02.
     MOVE    ZERO                 TO   HAC-F03.
     MOVE    ZERO                 TO   HAC-F04.
     MOVE    ZERO                 TO   HAC-F05.
     START    ZHACHDT   KEY   >=   HAC-F02 HAC-F03 HAC-F04 HAC-F05
          INVALID   KEY
             DISPLAY   "ﾊｯﾁｭｳﾌｧｲﾙ INVALID ﾃﾞﾝﾋﾟｮｳNO = "
                        BRK-N-DENNO                    UPON   CONS
             IF   END-FLG1   >   ZERO
                  MOVE    9       TO   END-FLG1
                  MOVE    9       TO   END-FLG2
             END-IF
             MOVE    1            TO   NOTDEL-FLG
             GO                   TO   CHECK-ZHACHDT-END
          NOT   INVALID   KEY
             PERFORM    READ-ZHACHDT-SEC
*************ADD   1              TO   READ-HAC-CNT
     END-START.
     IF   BRK-N-DENNO   NOT =   WK-H-DENNO
          DISPLAY   "ﾊｯﾁｭｳﾌｧｲﾙ INVALID ﾃﾞﾝﾋﾟｮｳNO = "
                     BRK-N-DENNO                    UPON   CONS
          IF   END-FLG1   >   ZERO
               MOVE    9          TO   END-FLG1
               MOVE    9          TO   END-FLG2
          END-IF
          MOVE    1               TO   NOTDEL-FLG
          GO                      TO   CHECK-ZHACHDT-END
     END-IF.
     MOVE    WK-H-KEY             TO   BRK-H-KEY.
     PERFORM    UNTIL
                END-FLG2         >   ZERO          OR
                WK-H-DENNO   NOT =   BRK-H-DENNO
        IF   NOTDEL-FLG   =   ZERO
             IF   HAC-F05   >=   1   AND
                  HAC-F05   <=   6
                  IF   HAC-F06    =   ZERO
                       MOVE    1  TO   NOTDEL-FLG
                  END-IF
             END-IF
        END-IF
        PERFORM    READ-ZHACHDT-SEC
********IF   END-FLG2   =   ZERO
*************ADD   1              TO   READ-HAC-CNT
********END-IF
     END-PERFORM.
 CHECK-ZHACHDT-END.
     EXIT.
****************************************************************
*      _２　　入庫ファイル削除処理                            *
****************************************************************
 DEL-ZNYUKDT-SEC             SECTION.
****　入庫ファイル　検索　****
     MOVE    BRK-N-DENNO          TO   NYU-F02.
     MOVE    BRK-N-EDANO          TO   NYU-F03.
     MOVE    BRK-N-SOSAI          TO   NYU-F04.
     MOVE    BRK-N-GYONO          TO   NYU-F05.
     START    ZNYUKDT   KEY   =   NYU-F02 NYU-F03 NYU-F04 NYU-F05
          INVALID   KEY
             DISPLAY   "ﾆｭｳｺF ｻｸｼﾞｮｴﾗ-  KEY = "
                        BRK-N-DENNO  " "
                        BRK-N-EDANO  " "
                        BRK-N-SOSAI  " "
                        BRK-N-GYONO             UPON   CONS
             MOVE    9            TO   END-FLG1
             MOVE    9            TO   END-FLG2
             MOVE    1            TO   NOTDEL-FLG
             GO                   TO   DEL-ZNYUKDT-END
          NOT   INVALID   KEY
             PERFORM    READ-ZNYUKDT-SEC
     END-START.
****　入庫ファイル　削除（実）　****
     PERFORM    UNTIL
                END-FLG1         >   1             OR
                WK-N-DENNO   NOT =   BRK-N-DENNO
         DELETE     ZNYUKDT
         ADD   1                  TO   DEL-NYU-CNT
         PERFORM    READ-ZNYUKDT-SEC
     END-PERFORM.
 DEL-ZNYUKDT-END.
     EXIT.
****************************************************************
*      _３　　発注ファイル削除処理                            *
****************************************************************
 DEL-ZHACHDT-SEC             SECTION.
****　発注ファイル　検索　****
     MOVE    BRK-H-DENNO          TO   HAC-F02.
     MOVE    BRK-H-EDANO          TO   HAC-F03.
     MOVE    BRK-H-SOSAI          TO   HAC-F04.
     MOVE    BRK-H-GYONO          TO   HAC-F05.
     START    ZHACHDT   KEY   =   HAC-F02 HAC-F03 HAC-F04 HAC-F05
          INVALID   KEY
             DISPLAY   "ﾊｯﾁｭｳFｻｸｼﾞｮｴﾗ-  KEY = "
                        BRK-H-DENNO  " "
                        BRK-H-EDANO  " "
                        BRK-H-SOSAI  " "
                        BRK-H-GYONO             UPON   CONS
             MOVE    9            TO   END-FLG1
             MOVE    9            TO   END-FLG2
             MOVE    1            TO   NOTDEL-FLG
             GO                   TO   DEL-ZHACHDT-END
          NOT   INVALID   KEY
             PERFORM    READ-ZHACHDT-SEC
     END-START.
****　発注ファイル　削除（実）　****
     PERFORM    UNTIL
                END-FLG2         >   1             OR
                WK-H-DENNO   NOT =   BRK-H-DENNO
         DELETE     ZHACHDT
         ADD   1                  TO   DEL-HAC-CNT
         PERFORM    READ-ZHACHDT-SEC
     END-PERFORM.
 DEL-ZHACHDT-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE              ZNYUKDT
                        ZHACHDT
                        HJYOKEN.
     DISPLAY "ﾆｭｳｺ  ﾌｧｲﾙ (IN) = "   READ-NYU-CNT   UPON   CONS.
*****DISPLAY "ﾊｯﾁｭｳ ﾌｧｲﾙ (IN) = "   READ-HAC-CNT   UPON   CONS.
     DISPLAY "ﾆｭｳｺ  ﾌｧｲﾙ(DEL) = "   DEL-NYU-CNT    UPON   CONS.
     DISPLAY "ﾊｯﾁｭｳ ﾌｧｲﾙ(DEL) = "   DEL-HAC-CNT    UPON   CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
