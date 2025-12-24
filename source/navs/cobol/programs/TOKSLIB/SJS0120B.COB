****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　実績管理システム　　　　　　　　　*
*    モジュール名　　　　：　実績集計Ｆ　月次処理              *
*    作成日　　　　　　　：　2002/12/24                        *
*    作成者　　　　　　　：　NAV                               *
*    処理概要　　　　　　：　条件Ｆの在庫締年月の累積実績ＤＴを*
*                            抽出し、物理削除する。            *
*    更新日／更新者　　　：　2012/03/01  /  MIURA              *
*    処理概要　　　　　　：　基幹サーバ統合　保有期間変更　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJS0120B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU-GP6000.
 OBJECT-COMPUTER.       FUJITSU-GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  実績累積ファイル  >>---*
     SELECT   JISSYUF   ASSIGN    TO        DA-01-VI-JISSYUL4
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   JIS-F02
                        FILE      STATUS    IS   JIS-STATUS.
*
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  実績累積ファイル  >>---*
 FD  JISSYUF.
     COPY     JISSYUF   OF        XFDLIB
              JOINING   JIS       PREFIX.
*---<<  実績累積ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  JIS-STATUS          PIC  X(02).
     02  JYO-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  SKIP-FLG            PIC  X(01)  VALUE SPACE.
     02  JIS-INV-FLG         PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)  VALUE ZERO.
     02  CRT-CNT             PIC  9(07)  VALUE ZERO.
     02  DEL-CNT             PIC  9(07)  VALUE ZERO.
****  ＷＲＫ領域  *** 1999/12/27 NAV START
 01  WRK-AREA.
     02  WRK-DATE1           PIC  9(06).
     02  WRK-DATE1R          REDEFINES   WRK-DATE1.
         04  WRK-DATE1R1     PIC  9(04).
         04  WRK-DATE1R2     PIC  9(02).
     02  WRK-DATE2           PIC  9(06).
 01  WK-SIME                 PIC  9(06)  VALUE ZERO.
 01  WK-KIKAN                PIC  9(02)  VALUE ZERO.
 01  WK-TUKI                 PIC S9(02)  VALUE ZERO.
 01  WK-DEL-DATE.
     02  DEL-DATE1.
         03  DEL-DATE1-1     PIC  9(04)  VALUE ZERO.
         03  DEL-DATE1-2     PIC  9(02)  VALUE ZERO.
     02  DEL-DATE2           PIC  9(02)  VALUE ZERO.
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SJS0120B".
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
                     PROCEDURE    JISSYUF.
     MOVE     "JISSYUF"      TO   ERR-FL-ID.
     MOVE     JIS-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "HJYOKEN"      TO   ERR-FL-ID.
     MOVE     JYO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 KEI0100-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM       END-SEC.
     STOP      RUN.
 KEI0100-END.
     EXIT.
****************************************************************
*      ■０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
*ファイルのＯＰＥＮ
     OPEN    I-O             JISSYUF.
     OPEN    INPUT           HJYOKEN.
*実績累積データ保有期間
     MOVE    83          TO  JYO-F01.
     MOVE   "DATE"       TO  JYO-F02.
     READ    HJYOKEN  INVALID
             DISPLAY "## HJYOKEN INVALID KEY = " JYO-F01 JYO-F02
                      " ##"  UPON CONS
             STOP  RUN
     END-READ.
*    MOVE    7           TO  WK-KIKAN.
     MOVE    JYO-F04     TO  WK-KIKAN.
     COMPUTE WK-KIKAN = WK-KIKAN - 1.
     DISPLAY "DATA ｷｶﾝ = " WK-KIKAN UPON CONS.
*在庫締年月取得
     MOVE    99          TO  JYO-F01.
     MOVE   "ZAI"        TO  JYO-F02.
     READ    HJYOKEN  INVALID
             DISPLAY "## HJYOKEN INVALID KEY = " JYO-F01 JYO-F02
                      " ##"  UPON CONS
             STOP  RUN
     END-READ.
     MOVE    JYO-F05     TO  DEL-DATE1 WK-SIME.
     MOVE    WK-SIME(1:4) TO DEL-DATE1-1.
     MOVE    WK-SIME(5:2) TO DEL-DATE1-2.
**** DISPLAY "WK-SIME     = " WK-SIME     UPON CONS.
**** DISPLAY "JYO-F05     = " JYO-F05     UPON CONS.
**** DISPLAY "DEL-DATE1-1 = " DEL-DATE1-1 UPON CONS.
**** DISPLAY "DEL-DATE1-2 = " DEL-DATE1-2 UPON CONS.
     MOVE    DEL-DATE1-2 TO  WK-TUKI.
     MOVE    99          TO  DEL-DATE2.
     COMPUTE WK-TUKI = WK-TUKI - WK-KIKAN.
     IF      WK-TUKI <= ZERO
             ADD -1      TO  DEL-DATE1-1
             COMPUTE  DEL-DATE1-2 = 12 - WK-KIKAN
     ELSE
             MOVE WK-TUKI TO DEL-DATE1-2
     END-IF.
     DISPLAY "ｻｲｼｭｳ DEL DATE = " DEL-DATE1 UPON CONS.
*実績累積Ｆ初期読込み
     PERFORM JIS-READ-SEC.
*
 INIT-END.
     EXIT.
****************************************************************
*      累積実績Ｆ読込み                                        *
****************************************************************
 JIS-READ-SEC                SECTION.
*
     READ    JISSYUF
             AT  END
             MOVE   "END"      TO   END-FLG
             GO                TO   JIS-READ-EXIT
             NOT AT END
             ADD     1         TO   READ-CNT
     END-READ.
*経過件数表示
     IF      READ-CNT(5:3) = "000"  OR  "500"
             DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*終了年月日以上の場合、処理終了
     IF      JIS-F02  >  WK-DEL-DATE
             MOVE   "END"      TO   END-FLG
     END-IF.
*
 JIS-READ-EXIT.
     EXIT.
****************************************************************
*      ■０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
*累積実績Ｆ削除
     DELETE   JISSYUF.
     ADD      1            TO     DEL-CNT.
*売上計上Ｆ読込み
     PERFORM JIS-READ-SEC.
*
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE              JISSYUF  HJYOKEN.
     DISPLAY  "JISSYUF    (IN) = "  READ-CNT      UPON   CONS.
     DISPLAY  "GERUISKF  (CRT) = "  CRT-CNT       UPON   CONS.
     DISPLAY  "JISSYUF   (DEL) = "  DEL-CNT       UPON   CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
