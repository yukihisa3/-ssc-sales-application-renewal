****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　_卸ファイル作成　　　　　　　　　*
*    作成日／更新日　　　：　00/05/29                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　条件ファイルの原票_を元に_卸ファ*
*                            イルの_卸_をセットし，出力する　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STA9998B.
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
*---<<  条件ファイル　  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*
*---<<  九州/北海道F    >>---*
     SELECT   TANADAT   ASSIGN    TO        DA-01-S-TANADAT
                        FILE      STATUS    IS   TAD-STATUS.
*
*---<<  九州/北海道F    >>---*
     SELECT   HIKAKUF   ASSIGN    TO        DA-01-S-HIKAKUF
                        FILE      STATUS    IS   HIK-STATUS.
*
*---<<  _卸ファイル　  >>---*
     SELECT   ZTANADT   ASSIGN    TO        DA-01-VI-ZTANADT1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   TAN-F01
                        FILE      STATUS    IS   TAN-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  条件ファイル　  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---<<  九州/北海道F    >>---*
 FD  TANADAT            BLOCK     CONTAINS   51  RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     TANADAT   OF   XFDLIB    JOINING   TAD  AS   PREFIX.
*---<<  九州/北海道F    >>---*
 FD  HIKAKUF            BLOCK     CONTAINS  255  RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HIKAKUF   OF   XFDLIB    JOINING   HIK  AS   PREFIX.
*---<<  _卸ファイル　  >>---*
 FD  ZTANADT.
     COPY     ZTANADT   OF        XFDLIB
              JOINING   TAN       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  JYO-STATUS          PIC  X(02)  VALUE "00".
     02  TAD-STATUS          PIC  X(02)  VALUE "00".
     02  HIK-STATUS          PIC  X(02)  VALUE "00".
     02  TAN-STATUS          PIC  X(02)  VALUE "00".
****  フラグ  ***
 01  FLG-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
****  カウンタ ***
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)  VALUE ZERO.
     02  SELECT-CNT          PIC  9(07)  VALUE ZERO.
     02  WRITE-CNT           PIC  9(07)  VALUE ZERO.
 01  OLD-TANABAN             PIC  X(08)  VALUE SPACE.
 01  NEW-TANABAN             PIC  X(08)  VALUE SPACE.
 01  WK-CHK-SOK              PIC  X(02)  VALUE SPACE.
 01  HJYOKEN-INV-FLG         PIC  X(03)  VALUE SPACE.
 01  TAI-FLG                 PIC  9(01)  VALUE ZERO.
****  ワ－ク  ***
 01  WK-AREA.
     02  WK-TANAOROSINO      PIC  9(10).
     02  WK-TANAOROSINO-R    REDEFINES   WK-TANAOROSINO.
       03  WK-GENPYONO       PIC  9(09).
       03  WK-GYONO          PIC  9(01).
     02  WK-TANAOROSINO1     PIC  9(10).
     02  WK-TANAOROSINO1-R   REDEFINES   WK-TANAOROSINO1.
       03  WK-GENPYONO1      PIC  9(09).
       03  WK-GYONO1         PIC  9(01).
     02  WK-TANAOROSINO2     PIC  9(10).
     02  WK-TANAOROSINO2-R   REDEFINES   WK-TANAOROSINO2.
       03  WK-GENPYONO2      PIC  9(09).
       03  WK-GYONO2         PIC  9(01).
     02  WK-SOUKOCD          PIC  X(02)  VALUE  SPACE.
     02  WK-BUMON                 PIC  9(09)V9(02).
     02  WK-BUMON-R               REDEFINES   WK-BUMON.
         03  WK-BUMON-CD1         PIC  9(05).
         03  WK-BUMON-CD2         PIC  9(03).
         03  WK-BUMON-CD3         PIC  9(01).
         03  WK-BUMON-CD4         PIC  9(02).
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "STA9998B".
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
                     PROCEDURE    HJYOKEN.
     MOVE     "HJYOKEN"      TO   ERR-FL-ID.
     MOVE     JYO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    TANADAT.
     MOVE     "TANADAT"      TO   ERR-FL-ID.
     MOVE     TAD-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    ZTANADT.
     MOVE     "ZTANADT"      TO   ERR-FL-ID.
     MOVE     TAN-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HIKAKUF.
     MOVE     "HIKAKUF"      TO   ERR-FL-ID.
     MOVE     HIK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 END     DECLARATIVES.
****************************************************************
 KEI0100-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 KEI0100-END.
     EXIT.
****************************************************************
*      1.0 　　初期処理
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    I-O             HJYOKEN
             INPUT           ZTANADT
             OUTPUT          TANADAT  HIKAKUF.
*
     MOVE    701404          TO    WK-GENPYONO.
     MOVE    ZERO            TO    WK-GYONO.
*
     PERFORM ZTANADT-READ-SEC.
     MOVE  TAN-F01           TO    WK-TANAOROSINO1.
*
 INIT-END.
     EXIT.
****************************************************************
*      1.2　　商品在庫ＲＥＡＤ処理
****************************************************************
 ZTANADT-READ-SEC                    SECTION.
     READ    ZTANADT
         AT  END
           MOVE   "END"      TO   END-FLG
           GO TO                  ZTANADT-READ-END
         NOT   AT   END
           MOVE  TAN-F01     TO   WK-TANAOROSINO2
           ADD   1           TO   READ-CNT
     END-READ.
*
 ZTANADT-READ-END.
     EXIT.
****************************************************************
*      2.0　　メイン処理
****************************************************************
 MAIN-SEC                    SECTION.
*
     IF     WK-GENPYONO1 NOT =   WK-GENPYONO2
            MOVE  TAN-F01    TO  WK-TANAOROSINO1
            ADD   1          TO  WK-GENPYONO
     END-IF.
*
     MOVE   SPACE            TO  TAD-REC.
     INITIALIZE                  TAD-REC.
     MOVE   TAN-REC          TO  TAD-REC.
     MOVE   WK-GENPYONO      TO  TAD-F011.
     MOVE   WK-GYONO2        TO  TAD-F012.
     MOVE   SPACE            TO  HIK-REC.
     INITIALIZE                  HIK-REC.
     MOVE   TAN-F01          TO  HIK-F01.
     MOVE   TAD-F01          TO  HIK-F02.
     WRITE  TAD-REC.
     WRITE  HIK-REC.
*
     PERFORM ZTANADT-READ-SEC.
*
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
 END010.
     CLOSE              HJYOKEN
                        TANADAT
                        ZTANADT.
*
     DISPLAY  "ｼﾖｳﾋﾝｻﾞｲｺM    (IN)= " READ-CNT      UPON CONS.
     DISPLAY  "ｼﾖｳﾋﾝｻﾞｲｺM(SELECT)= " SELECT-CNT    UPON CONS.
     DISPLAY  "ﾀﾅｵﾛｼF       (OUT)= " WRITE-CNT     UPON CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
