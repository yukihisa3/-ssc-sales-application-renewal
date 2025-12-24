****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿              *
*    業務名　　　　　　　：　本社－営業所間在庫管理　　　　　　*
*    モジュール名　　　　：　（本社）選択倉庫在庫データ抽出    *
*    作成日／作成者　　　：　2000/05/20 TAKAHASHI              *
*    再利用ＰＧ　　　　　：                                    *
*    更新日／更新者　　　：　                                  *
*                            （２００バイト　バージョン）      *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SZA0050B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*条件ファイル
     SELECT     HJYOKEN      ASSIGN    TO       DA-01-VI-JYOKEN1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      JYO-F01
                                                JYO-F02
                             FILE      STATUS   JYO-ST.
*商品在庫マスタ
     SELECT     ZAMZAIF       ASSIGN    TO       DA-01-VI-ZAMZAIL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      ZAI-F01
                                                ZAI-F02
                                                ZAI-F03
                             FILE      STATUS   ZAI-ST.
*倉庫（営業所）用在庫マスタ
     SELECT     EGYZAIKO     ASSIGN    TO       DA-01-S-EGYZAIKO
                             FILE      STATUS   EZA-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*条件ファイル
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*商品在庫マスタ
 FD  ZAMZAIF.
     COPY     ZAMZAIF    OF        XFDLIB
              JOINING   ZAI       PREFIX.
*倉庫（営業所）用在庫マスタ
 FD  EGYZAIKO.
     COPY     ZAMZAIF    OF        XFDLIB
              JOINING   ZAIKO     PREFIX.
*01  ZAIKO-REC.
*****03  ZAIKO-F01           PIC  X(101).
*****03  ZAIKO-F02           PIC  X(048).
*****03  ZAIKO-F03           PIC  X(051).
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  JYO-ST              PIC  X(02)  VALUE  SPACE.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
     03  EZA-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  9(01)  VALUE  ZERO.
     03  OUT-CNT             PIC  9(07)  VALUE  ZERO.
     03  READ-CNT            PIC  9(07)  VALUE  ZERO.
     03  READ-CNT2           PIC  9(07)  VALUE  ZERO.
     03  IX                  PIC  9(02)  VALUE  ZERO.
 01  WK-SYSTEM-DATE.
     03  SYS-DATE            PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE-8          PIC  9(08)  VALUE  ZERO.
 01  WK-SOKCD-WORK.
     03  WK-SOKCD-R          OCCURS      20.
         05  WK-SOKCD        PIC  9(02)  VALUE  ZERO.
*在庫マスタ退避用
 01  WK-ZAI-REC.
     03  WK-ZAI-01           PIC  X(101).
     03  WK-ZAI-02           PIC  X(432).
     03  WK-ZAI-03           PIC  X(048).
     03  WK-ZAI-04           PIC  X(019).
*
 01  FILE-ERR.
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件ファイル異常".
     03  ZAI-ERR             PIC  N(10)  VALUE
                   NC"在庫マスタ異常".
     03  EZA-ERR             PIC  N(10)  VALUE
                   NC"営業所在庫マスタ異常".
*
 01  SEC-NAME.
     03  FILLER              PIC  X(16)  VALUE "## ｴﾗｰSECTION = ".
     03  S-NAME              PIC  X(20).
     03  FILLER              PIC  X(03)  VALUE " ##".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
 LINKAGE                     SECTION.
 01  LINK-SOKCD              PIC  9(02).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-SOKCD.
****************************************************************
 DECLARATIVES.
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JYO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAMZAIF.
     DISPLAY     ZAI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 EZA-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE EGYZAIKO.
     DISPLAY     EZA-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     EZA-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC.
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN        INPUT       HJYOKEN   ZAMZAIF.
     OPEN        OUTPUT      EGYZAIKO.
*条件ファイル読込み
     MOVE        "80"          TO   JYO-F01.
     MOVE         LINK-SOKCD   TO   JYO-F02.
     READ  HJYOKEN  INVALID
        DISPLAY "## ﾁｭｳｼｭﾂ ﾀｲｼｮｳ ｿｳｺ ﾅｼ ##" UPON CONS
        DISPLAY "## SOKCD = " LINK-SOKCD "          ##" UPON CONS
        GO           TO   INIT-EXIT
     END-READ.
*データが存在した場合
     MOVE         ZERO         TO   WK-SOKCD-WORK.
     MOVE         JYO-F04      TO   WK-SOKCD(01).
     MOVE         JYO-F05      TO   WK-SOKCD(02).
     MOVE         JYO-F06      TO   WK-SOKCD(03).
     MOVE         JYO-F07      TO   WK-SOKCD(04).
     MOVE         JYO-F08      TO   WK-SOKCD(05).
     MOVE         JYO-F09      TO   WK-SOKCD(06).
     MOVE         JYO-F10      TO   WK-SOKCD(07).
     MOVE         JYO-F11      TO   WK-SOKCD(08).
     MOVE         JYO-F12      TO   WK-SOKCD(09).
     MOVE         JYO-F12A     TO   WK-SOKCD(10).
     MOVE         JYO-F12B     TO   WK-SOKCD(11).
     MOVE         JYO-F12C     TO   WK-SOKCD(12).

*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*抽出対象倉庫判定
     PERFORM  VARYING  IX  FROM  1  BY  1  UNTIL  IX  >  20
              IF  WK-SOKCD(IX) > ZERO
                  DISPLAY "##　ﾁｭｳｼｭﾂ　ｴｲｷﾞｮｳｼｮ CD = "
                               WK-SOKCD(IX)  " ##" UPON CONS
                  MOVE     ZERO         TO     READ-CNT2
                  PERFORM  ZAIKO-SOKCD-SEC
              END-IF
     END-PERFORM.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*              在庫マスタ読込
****************************************************************
 ZAIKO-SOKCD-SEC             SECTION.
     MOVE     "ZAIKO-SOKCD-SEC"   TO   S-NAME.
*
*在庫マスタスタート
     MOVE      WK-SOKCD(IX)    TO   ZAI-F01.
     MOVE      SPACE           TO   ZAI-F02.
     MOVE      SPACE           TO   ZAI-F03.
     MOVE      ZERO            TO   END-FLG.
     START ZAMZAIF KEY IS   >=   ZAI-F01 ZAI-F02 ZAI-F03
           INVALID
           GO                  TO   ZAIKO-SOKCD-EXIT
           NOT  INVALID
           PERFORM  ZAIKO-READ-SEC
     END-START.
*
 ZAIKO001.
     IF    END-FLG  =  9
           GO            TO     ZAIKO-SOKCD-EXIT
     END-IF.
     IF    WK-SOKCD(IX)  NOT =  ZAI-F01
           GO            TO     ZAIKO-SOKCD-EXIT
     END-IF.
*レコード更新
     MOVE  SPACE         TO     ZAIKO-REC.
     INITIALIZE                 ZAIKO-REC.
*レコードセット
     MOVE  ZAI-REC       TO     ZAIKO-REC.
*レコード追加
     WRITE ZAIKO-REC.
     ADD   1             TO     OUT-CNT.
*
     PERFORM ZAIKO-READ-SEC.
     GO                  TO     ZAIKO001.
*
 ZAIKO-SOKCD-EXIT.
     EXIT.
****************************************************************
*              在庫マスタ読込
****************************************************************
 ZAIKO-READ-SEC              SECTION.
     MOVE     "ZAIKO-READ-SEC"    TO   S-NAME.
*
     READ  ZAMZAIF  NEXT  AT  END
               MOVE      9        TO   END-FLG
           NOT  AT  END
               ADD       1        TO   READ-CNT
               ADD       1        TO   READ-CNT2
     END-READ.
*
     IF        READ-CNT2(6:2) = "00"
               DISPLAY "## READ-CNT2 = " READ-CNT2 " ##" UPON CONS
     END-IF.
*
 ZAIKO-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     CLOSE HJYOKEN ZAMZAIF EGYZAIKO.
*
     DISPLAY "## READ-CNT = " READ-CNT " ##"  UPON  CONS.
     DISPLAY "## OUT-CNT  = " OUT-CNT  " ##"  UPON  CONS.
*
 END-EXIT.
     EXIT.
