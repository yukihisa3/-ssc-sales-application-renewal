****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　_卸ファイル作成　　　　　　　　　*
*    作成日／更新日　　　：　00/05/29                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　条件ファイルの原票■を元に_卸ファ*
*                            イルの_卸■をセットし，出力する　*
*    変更概要　　　　　　：　2006/11/21 NAV                   *
*                            _番下２桁＝”００”も出力する。　*
*                            ８３を６Ｍで出力する。            *
*    変更概要　　　　　　：　2013/05/17 NAV                   *
*                            条件Ｆ　項目Ｆ１２Ｃが１の場合、　*
*                            _番が空白でも_卸原票データを作成*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STA0010B.
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
*---<<  商品在庫マスタ  >>---*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL4
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
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "STA0010B".
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
                     PROCEDURE    ZAMZAIF.
     MOVE     "ZAMZAIF"      TO   ERR-FL-ID.
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
*      1.0 　　初期処理
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    I-O             HJYOKEN
             INPUT           ZAMZAIF
             OUTPUT          ZTANADT.
     PERFORM    HJYOKEN-READ-SEC.
     IF   END-FLG   NOT =   "END"
       PERFORM    ZAMZAIF-READ-SEC
       IF   END-FLG   NOT =   "END"
         MOVE   ZAI-F01      TO   WK-SOUKOCD
         MOVE   ZAI-F03      TO   OLD-TANABAN
       END-IF
     END-IF.
 INIT-END.
     EXIT.
****************************************************************
*      1.1　　条件Ｆ　ＲＥＡＤ処理
****************************************************************
 HJYOKEN-READ-SEC              SECTION.
*条件Ｆ索引（部門コード取得）
     MOVE    99                  TO   JYO-F01.
     MOVE    "BUMON"             TO   JYO-F02.
     READ    HJYOKEN
         INVALID
             MOVE   "END"      TO   END-FLG
             DISPLAY  "ｼﾞﾖｳｹﾝﾌｱｲﾙ ﾌﾞﾓﾝ INVALID ]]"  UPON CONS
         NOT INVALID
             MOVE    JYO-F05     TO   WK-BUMON
             IF      WK-BUMON = ZERO
                     MOVE   "END"      TO   END-FLG
                     DISPLAY  "ｼﾞﾖｳｹﾝF ﾌﾞﾓﾝ ZERO]]"  UPON CONS
             END-IF
     END-READ.
*原票ＮＯ取得
     MOVE   63               TO   JYO-F01.
     MOVE   SPACE            TO   JYO-F02.
     READ    HJYOKEN
         INVALID
           MOVE   "END"      TO   END-FLG
           DISPLAY  "ｼﾞｳｹﾝﾌｱｲﾙ INVALID ]]"   UPON   CONS
         NOT   INVALID
           MOVE   JYO-F05    TO   WK-GENPYONO
           MOVE   ZERO       TO   WK-GYONO
     END-READ.
 HJYOKEN-READ-END.
     EXIT.
****************************************************************
*      1.2　　商品在庫ＲＥＡＤ処理
****************************************************************
 ZAMZAIF-READ-SEC                    SECTION.
     READ    ZAMZAIF
         AT   END
           MOVE   "END"      TO   END-FLG
           GO TO                  ZAMZAIF-READ-END
         NOT   AT   END
           ADD   1           TO   READ-CNT
     END-READ.
*指定倉庫ＣＤブレイク
     IF      ZAI-F01   NOT =  WK-CHK-SOK
*****DISPLAY "ZAI-F01    = " ZAI-F01       UPON CONS
*****DISPLAY "WK-CHK-SOK = " WK-CHK-SOK    UPON CONS
             PERFORM   SOK-CHK-SEC
             IF  HJYOKEN-INV-FLG = "INV"
                 MOVE    ZERO   TO  TAI-FLG
             ELSE
                 MOVE JYO-F07   TO  WK-BUMON
                 IF  JYO-F06 = 1
                     MOVE  1    TO  TAI-FLG
                 ELSE
                     MOVE  ZERO TO  TAI-FLG
                 END-IF
             END-IF
             MOVE   ZAI-F01     TO  WK-CHK-SOK
     END-IF.
*対象ＦＬＧが０の時対象外
     IF      TAI-FLG          =   ZERO
           GO TO                  ZAMZAIF-READ-SEC
     END-IF.
*商品ＣＤ範囲
*2007/05/23 対象とする ST
*****IF      ZAI-F021        >=   "00300000"  AND
*************ZAI-F021        <    "00400000"
*************GO TO                  ZAMZAIF-READ-SEC
*****END-IF.
*2007/05/23 対象とする ED
*現在庫＝０は対象外
     IF      ZAI-F04         =    ZERO
           GO TO                  ZAMZAIF-READ-SEC
     END-IF.
*_番スペースは対象外
*2013/05/17 NAV ST Ｆ１２Ｃが０以外は_番のチェックは行わない。
*****IF      ZAI-F03         =    SPACE
*****      GO TO                  ZAMZAIF-READ-SEC
*****END-IF.
     IF      JYO-F12C  =  ZERO
             IF      ZAI-F03         =    SPACE
                     GO TO                  ZAMZAIF-READ-SEC
             END-IF
     END-IF.
*2013/05/17 NAV ED
*2006/11/21 NAV ST 停止します
*_番下２桁が”００”は対象外
*****IF      ZAI-F033        =    "00"
*****      GO TO                  ZAMZAIF-READ-SEC
*****END-IF.
*2006/11/21 NAV ED 停止します
*
     ADD   1                 TO   SELECT-CNT.
 ZAMZAIF-READ-END.
     EXIT.
****************************************************************
*      2.0　　メイン処理
****************************************************************
 MAIN-SEC                    SECTION.
     IF   ZAI-F01   NOT =   WK-SOUKOCD
       MOVE   ZAI-F01        TO   WK-SOUKOCD
       MOVE   ZAI-F03        TO   OLD-TANABAN
       IF   WK-GYONO   >   ZERO
         ADD   1             TO   WK-GENPYONO
         MOVE   ZERO         TO   WK-GYONO
       END-IF
     ELSE
       IF   ZAI-F03(1:4)    NOT =   OLD-TANABAN(1:4)
            MOVE    ZAI-F03 TO  OLD-TANABAN
            IF   WK-GYONO   >   ZERO
                 ADD   1    TO   WK-GENPYONO
                 MOVE  ZERO TO   WK-GYONO
            END-IF
       END-IF
     END-IF.
*
     MOVE   SPACE            TO   TAN-REC.
     INITIALIZE                   TAN-REC.
     MOVE   WK-TANAOROSINO   TO   TAN-F01.
     MOVE   WK-BUMON-CD2     TO   TAN-F02.
     MOVE   WK-BUMON-CD3     TO   TAN-F03.
     MOVE   ZAI-F01          TO   TAN-F04.
     IF     ZAI-F01  =  "83"
            MOVE  "6M"       TO   TAN-F04
     END-IF.
     MOVE   ZAI-F021         TO   TAN-F05.
     MOVE   ZAI-F022         TO   TAN-F06X.
     MOVE   ZAI-F03          TO   TAN-F15.
     MOVE   ZAI-F04          TO   TAN-F11.
     MOVE   ZAI-F04          TO   TAN-F16.
     MOVE   ZAI-F99(3:2)     TO   TAN-F13.
     WRITE    TAN-REC.
     ADD   1                 TO   WRITE-CNT.
     ADD   1                 TO   WK-TANAOROSINO.
*
     PERFORM    ZAMZAIF-READ-SEC.
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*原票ＮＯ取得（再読込み）
     MOVE   63               TO   JYO-F01.
     MOVE   SPACE            TO   JYO-F02.
     READ    HJYOKEN
         INVALID
           MOVE   "END"      TO   END-FLG
           DISPLAY  "ｼﾞｳｹﾝﾌｱｲﾙ INVALID ]]"   UPON   CONS
           GO                TO   END010
     END-READ.
*
     IF   READ-CNT   >   ZERO
       IF   WK-GYONO   >   ZERO
         ADD   1             TO   WK-GENPYONO
       END-IF
       MOVE   WK-GENPYONO    TO   JYO-F05
       REWRITE    JYO-REC
     END-IF.
 END010.
     CLOSE              HJYOKEN
                        ZAMZAIF
                        ZTANADT.
*
     DISPLAY  "ｼﾖｳﾋﾝｻﾞｲｺM    (IN)= " READ-CNT      UPON CONS.
     DISPLAY  "ｼﾖｳﾋﾝｻﾞｲｺM(SELECT)= " SELECT-CNT    UPON CONS.
     DISPLAY  "ﾀﾅｵﾛｼF       (OUT)= " WRITE-CNT     UPON CONS.
 END-END.
     EXIT.
****************************************************************
*      2.1        倉庫マスタ対象チェック                       *
****************************************************************
 SOK-CHK-SEC            SECTION.
*
     MOVE    20                  TO   JYO-F01.
     MOVE    ZAI-F01             TO   JYO-F02.
     READ    HJYOKEN
         INVALID
             MOVE   "INV"      TO   HJYOKEN-INV-FLG
         NOT INVALID
             MOVE   SPACE      TO   HJYOKEN-INV-FLG
     END-READ.
*
 SOK-CHK-EXIT.
     EXIT.
******************<<  PROGRAM  END  >>**************************
