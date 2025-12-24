****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　_卸ファイル作成　　　　　　　　　*
*    作成日／更新日　　　：　93/04/27                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　条件ファイルの原票_を元に_卸ファ*
*                            イルの_卸_をセットし，出力する　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZTA0010B.
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
     SELECT   HJYOKEN    ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*
*---<<  商品在庫マスタ  >>---*
     SELECT   ZZAIMS    ASSIGN    TO        DA-01-VI-ZZAIMS4
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F031
                                                 ZAI-F032
                                                 ZAI-F033
                                                 ZAI-F021
                                                 ZAI-F022
                        FILE      STATUS    IS   ZAI-STATUS.
*
*---<<  _卸ファイル　  >>---*
     SELECT   ZTANADT   ASSIGN    TO        DA-01-VS-ZTANADT
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   TAN-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  条件ファイル　  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---<<  商品在庫マスタ  >>---*
 FD  ZZAIMS.
     COPY     ZZAIMS    OF        XFDLIB
              JOINING   ZAI       PREFIX.
*---<<  _卸ファイル　  >>---*
 FD  ZTANADT.
     COPY     ZTANADT   OF        XFDLIB
              JOINING   TAN       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  JYO-STATUS          PIC  X(02)  VALUE "00".
     02  ZAI-STATUS          PIC  X(02)  VALUE "00".
     02  TAN-STATUS          PIC  X(02)  VALUE "00".
****  フラグ  ***
 01  FLG-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
****  カウンタ ***
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)  VALUE ZERO.
     02  REWRITE-CNT         PIC  9(07)  VALUE ZERO.
     02  WRITE-CNT           PIC  9(07)  VALUE ZERO.
*93.11.12
 01  OLD-TANABAN             PIC  X(08)  VALUE SPACE.
 01  NEW-TANABAN             PIC  X(08)  VALUE SPACE.
****  ワ－ク  ***
 01  WK-AREA.
     02  WK-TANAOROSINO      PIC  9(10).
     02  WK-TANAOROSINO-R    REDEFINES   WK-TANAOROSINO.
       03  WK-GENPYONO       PIC  9(09).
       03  WK-GYONO          PIC  9(01).
     02  WK-SOUKOCD          PIC  9(02)  VALUE  ZERO.
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZTA0010B".
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
                     PROCEDURE    ZZAIMS.
     MOVE     "ZZAIMS"       TO   ERR-FL-ID.
     MOVE     ZAI-STATUS     TO   ERR-STCD.
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
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    I-O             HJYOKEN
             INPUT           ZZAIMS
             OUTPUT          ZTANADT.
     PERFORM    HJYOKEN-READ-SEC.
     IF   END-FLG   NOT =   "END"
       PERFORM    ZZAIMS-READ-SEC
       IF   END-FLG   NOT =   "END"
         MOVE   ZAI-F01      TO   WK-SOUKOCD
*93.11.12
         MOVE   ZAI-F03      TO   OLD-TANABAN
       END-IF
     END-IF.
 INIT-END.
     EXIT.
****************************************************************
*      _１　　条件Ｆ　ＲＥＡＤ処理                            *
****************************************************************
 HJYOKEN-READ-SEC              SECTION.
     MOVE   63               TO   JYO-F01.
     MOVE   SPACE            TO   JYO-F02.
     READ    HJYOKEN
         INVALID
           MOVE   "END"      TO   END-FLG
           DISPLAY  "ｼﾞｳｹﾝﾌｱｲﾙ INVALID ]]"   UPON   CONS
         NOT   INVALID
*93.11.12
***********MOVE   JYO-F04    TO   WK-GENPYONO
           MOVE   JYO-F05    TO   WK-GENPYONO
           MOVE   ZERO       TO   WK-GYONO
     END-READ.
 HJYOKEN-READ-END.
     EXIT.
****************************************************************
*      _２　　商品在庫ＲＥＡＤ処理                            *
****************************************************************
 ZZAIMS-READ-SEC                    SECTION.
     READ    ZZAIMS
         AT   END
           MOVE   "END"      TO   END-FLG
           GO TO                  ZZAIMS-READ-END
         NOT   AT   END
           ADD   1           TO   READ-CNT
     END-READ.
*
     IF      ZAI-F021        >=   "00300000"  AND
             ZAI-F021        <    "00400000"
           GO TO                  ZZAIMS-READ-SEC
     END-IF.
*
*
     IF      ZAI-F06         =    ZERO
           GO TO                  ZZAIMS-READ-SEC
     END-IF.
*
* 931118
     IF      ZAI-F03         =    SPACE
           GO TO                  ZZAIMS-READ-SEC
     END-IF.
*
*  931118
     IF      ZAI-F033        =    "00"
           GO TO                  ZZAIMS-READ-SEC
     END-IF.
*
 ZZAIMS-READ-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
     IF   ZAI-F01   NOT =   WK-SOUKOCD
       MOVE   ZAI-F01        TO   WK-SOUKOCD
       MOVE   ZAI-F03        TO   OLD-TANABAN
       IF   WK-GYONO   >   ZERO
         ADD   1             TO   WK-GENPYONO
         MOVE   ZERO         TO   WK-GYONO
       END-IF
*93.11.12
*****END-IF.
     ELSE
       IF   ZAI-F03(1:4)    NOT =   OLD-TANABAN(1:4)
            MOVE    ZAI-F03 TO  OLD-TANABAN
            IF   WK-GYONO   >   ZERO
                 ADD   1    TO   WK-GENPYONO
                 MOVE  ZERO TO   WK-GYONO
            END-IF
       END-IF
     END-IF.
*93.11.12
*
     MOVE   SPACE            TO   TAN-REC.
     INITIALIZE                   TAN-REC.
     MOVE   WK-TANAOROSINO   TO   TAN-F01.
     MOVE   ZAI-F01          TO   TAN-F04.
     MOVE   ZAI-F021         TO   TAN-F05.
     MOVE   ZAI-F022         TO   TAN-F06X.
     MOVE   ZAI-F03          TO   TAN-F15.
     MOVE   ZAI-F06          TO   TAN-F11.
     MOVE   ZAI-F06          TO   TAN-F16.
     WRITE    TAN-REC.
     ADD   1                 TO   WRITE-CNT.
     ADD   1                 TO   WK-TANAOROSINO.
*****MOVE   ZERO             TO   ZAI-F08.
*****REWRITE    ZAI-REC.
     ADD   1                 TO   REWRITE-CNT.
     PERFORM    ZZAIMS-READ-SEC.
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     IF   READ-CNT   >   ZERO
       IF   WK-GYONO   >   ZERO
         ADD   1             TO   WK-GENPYONO
       END-IF
*******MOVE   WK-GENPYONO    TO   JYO-F04
       MOVE   WK-GENPYONO    TO   JYO-F05
       REWRITE    JYO-REC
     END-IF.
     CLOSE              HJYOKEN
                        ZZAIMS
                        ZTANADT.
     DISPLAY  "ｼﾖｳﾋﾝｻﾞｲｺM(IN) = "  READ-CNT      UPON   CONS.
     DISPLAY  "ｼﾖｳﾋﾝｻﾞｲｺM(UP) = "  REWRITE-CNT   UPON   CONS.
     DISPLAY  "ﾀﾅｵﾛｼF   (OUT) = "  WRITE-CNT     UPON   CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
