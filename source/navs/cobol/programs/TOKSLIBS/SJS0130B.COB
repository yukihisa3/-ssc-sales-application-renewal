****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　実績管理システム　　　　　　　　　
*    モジュール名　　　　：　実績週集計Ｆ　月次処理
*    作成日／更新日　　　：　2011/01/14
*    作成者／更新者　　　：　NAV
*    処理概要　　　　　　：　
*      削除対象の実績週集計Ｆを抽出し、物理削除する。
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJS0130B.
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
*---<<  実績週累積ファイル  >>---*
     SELECT   JISSSYUF   ASSIGN    TO        DA-01-VI-JISSSYU4
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS
                                  JSS-F021 *> 年
                                  JSS-F022 *> 週番号
                                  JSS-F01  *> 売上仕入区分
                                  JSS-F03  *> 取引先
                                  JSS-F051 *> 商品ＣＤ
                                  JSS-F052 *> 品単ＣＤ
                                  JSS-F12  *> 相手商品ＣＤ
                        FILE      STATUS    IS   JSS-STATUS.
*
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*---<<  日付ファイル　　　  >>---*
     SELECT   HIDUKEF   ASSIGN    TO        DA-01-VI-HIDUKEL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYD-F01
                        FILE      STATUS    IS   JYD-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  実績週累積ファイル  >>---*
 FD  JISSSYUF.
     COPY     JISSSYUF   OF        XFDLIB
              JOINING   JSS       PREFIX.
*---<<  条件ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---<<  日次更新条件ファイル  >>---*
 FD  HIDUKEF.
     COPY     HIDUKEF   OF        XFDLIB
              JOINING   JYD       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  JSS-STATUS          PIC  X(02).
     02  JYO-STATUS          PIC  X(02).
     02  JYD-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  SKIP-FLG            PIC  X(01)  VALUE SPACE.
     02  JSS-INV-FLG         PIC  X(03)  VALUE SPACE.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SJS0130B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
* 月末日計算サブ用領域
     COPY  CGMATBIW.
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    JISSSYUF.
     MOVE     "JISSSYUF"     TO   ERR-FL-ID.
     MOVE     JSS-STATUS     TO   ERR-STCD.
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
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HIDUKEF.
     MOVE     "HIDUKEF"      TO   ERR-FL-ID.
     MOVE     JYD-STATUS     TO   ERR-STCD.
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
*    1.0  初期処理                                             *
****************************************************************
 INIT-SEC                    SECTION.
*ファイルのＯＰＥＮ
     OPEN  I-O   JISSSYUF.
     OPEN  INPUT HJYOKEN.
     OPEN  INPUT HIDUKEF.
* 実績累積データ保有期間
     MOVE    83          TO  JYO-F01.
     MOVE   "DATE"       TO  JYO-F02.
     READ    HJYOKEN  INVALID
             DISPLAY "## HJYOKEN INVALID KEY = " JYO-F01 JYO-F02
                      " ##"  UPON CONS
             STOP  RUN
     END-READ.
     MOVE    JYO-F04     TO  WK-KIKAN.
     COMPUTE WK-KIKAN = WK-KIKAN - 1.
     DISPLAY "DATA ｷｶﾝ = " WK-KIKAN UPON CONS.
* 在庫締年月取得
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
* 削除日を計算する。
     MOVE    DEL-DATE1-2 TO  WK-TUKI.

**** MOVE    99          TO  DEL-DATE2.
     COMPUTE WK-TUKI = WK-TUKI - WK-KIKAN.
     IF      WK-TUKI <= ZERO
             ADD -1      TO  DEL-DATE1-1
             COMPUTE  DEL-DATE1-2 = 12 + WK-TUKI
     ELSE
             MOVE WK-TUKI TO DEL-DATE1-2
     END-IF.

* 月末日計算サブ
     INITIALIZE  MATU-AREA.
     MOVE  DEL-DATE1        TO  MATU-YM.
     PERFORM  CGMATBIP.
     MOVE  MATU-D           TO  DEL-DATE2.

**** DISPLAY "ｻｲｼｭｳ DEL DATE = " DEL-DATE1 UPON CONS.
     DISPLAY "ｻｲｼｭｳ DEL DATE = " WK-DEL-DATE  UPON CONS.
* 週番号を取得する。
     MOVE  WK-DEL-DATE      TO  JYD-F01.

     READ  HIDUKEF
       INVALID
         DISPLAY "## JHMNITF INVALID KEY = " JYD-F01
                 " ##"  UPON CONS
         STOP  RUN
     END-READ.

     DISPLAY "ｻｲｼｭｳ DEL SYUNO = " JYD-FIL1(1:2) UPON CONS.

* 実績週累積ファイル読込み（降順）
     MOVE  DEL-DATE1-1      TO  JSS-F021. *> 年
     MOVE  JYD-FIL1(1:2)    TO  JSS-F022. *> 週番号
     MOVE  HIGH-VALUE       TO  JSS-F01.  *> 売上仕入区分
     MOVE  HIGH-VALUE       TO  JSS-F03.  *> 取引先
     MOVE  HIGH-VALUE       TO  JSS-F051. *> 商品ＣＤ
     MOVE  HIGH-VALUE       TO  JSS-F052. *> 品単ＣＤ
     MOVE  HIGH-VALUE       TO  JSS-F12.  *> 相手商品ＣＤ
     MOVE  LOW-VALUE        TO  END-FLG.
     PERFORM  JSS-READ-SEC.

 INIT-END.
     EXIT.
****************************************************************
*    月末日計算サブ                                            *
****************************************************************
     COPY  CGMATBIP.
****************************************************************
*      実績週累積ファイル読込み                                *
****************************************************************
 JSS-READ-SEC                SECTION.

     IF  END-FLG = LOW-VALUE
         MOVE  SPACE        TO  END-FLG

         START  JISSSYUF
                    KEY <= JSS-F021 *> 年
                           JSS-F022 *> 週番号
                           JSS-F01  *> 売上仕入区分
                           JSS-F03  *> 取引先
                           JSS-F051 *> 商品ＣＤ
                           JSS-F052 *> 品単ＣＤ
                           JSS-F12  *> 相手商品ＣＤ
                    WITH  REVERSED ORDER
           INVALID  KEY
             MOVE  "END"  TO  END-FLG
             GO TO  JSS-READ-EXIT
         END-START
     END-IF.

     READ  JISSSYUF
       AT END
         MOVE  "END"        TO  END-FLG
         GO TO  JSS-READ-EXIT
     END-READ.

     ADD 1  TO READ-CNT.
* 経過件数表示
     IF  READ-CNT(5:3) = "000"  OR  "500"
         DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.

 JSS-READ-EXIT.
     EXIT.
****************************************************************
*    2.0  メイン処理                                           *
****************************************************************
 MAIN-SEC                    SECTION.
* 実績週累積ファイル削除
     DELETE  JISSSYUF.
     ADD  1   TO  DEL-CNT.

* 実績週累積ファイル読込み（降順）
     PERFORM JSS-READ-SEC.

 MAIN-END.
     EXIT.
****************************************************************
*    3.0  終了処理                                             *
****************************************************************
 END-SEC                SECTION.
     CLOSE  JISSSYUF.
     CLOSE  HJYOKEN.
     CLOSE  HIDUKEF.
     DISPLAY  "JISSSYUF  (IN ) = "  READ-CNT  UPON CONS.
     DISPLAY  "JISSSYUF  (DEL) = "  DEL-CNT   UPON CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
